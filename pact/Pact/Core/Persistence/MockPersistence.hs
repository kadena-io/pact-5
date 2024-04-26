{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pact.Core.Persistence.MockPersistence (
  mockPactDb
  )where


import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (unless)
import Data.Maybe (isJust, fromMaybe)
import Data.List (find)
import Control.Lens ((^?), (^.), ix, view)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.IORef
import Control.Exception(throwIO)
import qualified Data.Map.Strict as M
import Data.ByteString (ByteString)
import Pact.Core.Guards
import Pact.Core.Namespace
import Pact.Core.Names
import Pact.Core.DefPacts.Types (DefPactExec)
import qualified Pact.Core.Persistence as Persistence
import Pact.Core.Persistence
import Pact.Core.Serialise
import Pact.Core.Gas (MilliGas)


type TxLogQueue = IORef (Map TxId [TxLog ByteString])

mockPactDb :: forall b i. PactSerialise b i -> IO (PactDb b i)
mockPactDb serial = do
  refMod <- newIORef M.empty
  refKs <- newIORef M.empty
  refUsrTbl <- newIORef M.empty
  refPacts <- newIORef M.empty
  refNS <- newIORef M.empty
  refRb <- newIORef Nothing
  refTxLog :: TxLogQueue <- newIORef mempty
  refTxId <- newIORef $ TxId 0
  pure $ PactDb
    { _pdbPurity = PImpure
    , _pdbRead = read' refKs refMod refNS refUsrTbl refPacts
    , _pdbWrite = write refKs refMod refNS refUsrTbl refTxId refTxLog refPacts
    , _pdbKeys = keys refKs refMod refNS refUsrTbl refPacts
    , _pdbCreateUserTable = createUsrTable refUsrTbl refTxId refTxLog
    , _pdbBeginTx = beginTx refRb refTxId refTxLog refMod refKs refUsrTbl
    , _pdbCommitTx = commitTx refRb refTxId refTxLog refMod refKs refUsrTbl
    , _pdbRollbackTx = rollbackTx refRb refTxLog refMod refKs refUsrTbl
    , _pdbTxIds = txIds refTxLog
    , _pdbGetTxLog = txLog refTxLog
    }
  where
  beginTx refRb refTxId refTxLog refMod refKs refUsrTbl em = do
    readIORef refRb >>= \case
      Just (_, _, _, _, _) -> pure Nothing
      Nothing -> do
        mods <- readIORef refMod
        ks <- readIORef refKs
        usrTbl <- readIORef refUsrTbl
        txl <- readIORef refTxLog
        writeIORef refRb (Just (em, txl, mods, ks, usrTbl))
        tid <- readIORef refTxId
        pure (Just tid)

  commitTx refRb refTxId refTxLog refMod refKs refUsrTbl = readIORef refRb >>= \case
    Just (em, txl, mods, ks, usr) -> case em of
      Transactional -> do
        writeIORef refRb Nothing
        txId <- atomicModifyIORef' refTxId (\(TxId i) -> (TxId (succ i), TxId i))
        txLogQueue <- readIORef refTxLog
        pure (Map.findWithDefault [] txId txLogQueue)
      Local -> do
        writeIORef refRb Nothing
        writeIORef refMod mods
        writeIORef refKs ks
        writeIORef refUsrTbl usr
        writeIORef refTxLog txl
        txId <- readIORef refTxId
        pure (Map.findWithDefault [] txId txl)
    Nothing ->
      throwIO Persistence.NoTxToCommit

  rollbackTx refRb refTxLog refMod refKs refUsrTbl = readIORef refRb >>= \case
    Just (_, txl, mods, ks, usr) -> do
      writeIORef refRb Nothing
      writeIORef refTxLog txl
      writeIORef refMod mods
      writeIORef refKs ks
      writeIORef refUsrTbl usr
    Nothing -> throwIO Persistence.NoTxToCommit

  txLog :: TxLogQueue -> TableName -> TxId -> IO [TxLog RowData]
  txLog refTxLog tn tid = do
    txl <- readIORef refTxLog
    case Map.lookup tid txl of
      Just txl' -> pure $ fromMaybe [] (traverse (traverse (fmap (view document) . _decodeRowData serial)) (filter (\(TxLog dom _ _) -> dom == toUserTable tn) txl'))
      Nothing -> pure []

  txIds :: TxLogQueue -> TableName -> TxId -> IO [TxId]
  txIds refTxLog tn (TxId txId) = do
    txl <- readIORef refTxLog
    let
      userTab = toUserTable tn
      subTxs = Map.filterWithKey (\(TxId i) txs -> i >= txId && isJust (find (\(TxLog dom _ _) -> dom == userTab) txs)) txl
    pure (Map.keys subTxs)

  keys
    :: forall k v
    .  IORef (Map KeySetName KeySet)
    -> IORef (Map ModuleName (ModuleData b i))
    -> IORef (Map NamespaceName Namespace)
    -> IORef (Map TableName (Map RowKey RowData))
    -> IORef (Map DefPactId (Maybe DefPactExec))
    -> Domain k v b i
    -> IO [k]
  keys refKs refMod refNS refUsrTbl refPacts d = case d of
    DKeySets -> do
      r <- readIORef refKs
      return (M.keys r)
    DModules -> do
      r <- readIORef refMod
      return (M.keys r)
    DUserTables tbl -> do
      r <- readIORef refUsrTbl
--      let tblName = toUserTable tbl
      case M.lookup tbl r of
        Just t -> return (M.keys t)
        Nothing -> throwIO (Persistence.NoSuchTable tbl)
    DDefPacts -> do
      r <- readIORef refPacts
      return (M.keys r)
    DNamespaces -> do
      r <- readIORef refNS
      pure (M.keys r)

  createUsrTable
    :: IORef (Map TableName (Map RowKey RowData))
    -> IORef TxId
    -> TxLogQueue
    -> TableName
    -> IO ()
  createUsrTable refUsrTbl _refTxId _refTxLog tbl = do
    ref <- readIORef refUsrTbl
    case M.lookup tbl ref of
      Nothing -> do
        -- TODO: Do we need a TxLog when a usertable is created?
        modifyIORef refUsrTbl (M.insert tbl mempty)
        pure ()
      Just _ -> throwIO (Persistence.TableAlreadyExists tbl)

  read'
    :: forall k v
    .  IORef (Map KeySetName KeySet)
    -> IORef (Map ModuleName (ModuleData b i))
    -> IORef (Map NamespaceName Namespace)
    -> IORef (Map TableName (Map RowKey RowData))
    -> IORef (Map DefPactId (Maybe DefPactExec))
    -> Domain k v b i
    -> k
    -> IO (Maybe v)
  read' refKs refMod refNS refUsrTbl refPacts domain k = case domain of
    DKeySets -> readKS refKs k
    DModules -> readMod refMod k
    DUserTables tbl ->
      readRowData refUsrTbl tbl k
    DDefPacts -> readPacts' refPacts k
    DNamespaces -> readNS refNS k

  checkTable :: forall m. MonadIO m => TableName -> IORef (Map TableName (Map RowKey RowData)) -> m ()
  checkTable tbl ref = liftIO $ do
    r <- readIORef ref
    unless (isJust (M.lookup tbl r)) $ throwIO (Persistence.NoSuchTable tbl)

  write
    :: forall k v m
    .  MonadIO m
    => IORef (Map KeySetName KeySet)
    -> IORef (Map ModuleName (ModuleData b i))
    -> IORef (Map NamespaceName Namespace)
    -> IORef (Map TableName (Map RowKey RowData))
    -> IORef TxId
    -> TxLogQueue
    -> IORef (Map DefPactId (Maybe DefPactExec))
    -> (MilliGas -> m ())
    -> WriteType
    -> Domain k v b i
    -> k
    -> v
    -> m ()
  write refKs refMod refNS refUsrTbl refTxId refTxLog refPacts gasHandler wt domain k v = case domain of
    DKeySets -> liftIO $ writeKS refKs refTxId refTxLog k v
    DModules -> liftIO $ writeMod refMod refTxId refTxLog v
    DUserTables tbl -> writeRowData refUsrTbl refTxId refTxLog tbl gasHandler wt k v
    DDefPacts -> liftIO $ writePacts' refPacts refTxId refTxLog k v
    DNamespaces -> liftIO $ writeNS refNS refTxId refTxLog k v

  readRowData ref tbl k = do
    -- let tblName = toUserTable tbl
    checkTable tbl ref
    r <- readIORef ref
    pure (r ^? ix tbl . ix k)

  writeRowData
    :: MonadIO m
    => IORef (Map TableName (Map RowKey RowData))
    -> IORef TxId
    -> TxLogQueue
    -> TableName
    -> (MilliGas -> m ())
    -> WriteType
    -> RowKey
    -> RowData
    -> m ()
  writeRowData ref refTxId refTxLog tbl handleGas wt k v = checkTable tbl ref *> case wt of
    Write -> do
      encodedData <- _encodeRowData serial handleGas v
      liftIO $ record refTxId refTxLog (TxLog (toUserTable tbl) (k ^. rowKey) encodedData)
      liftIO $ modifyIORef' ref (M.insertWith M.union tbl (M.singleton k v))
    Insert -> do
      r <- liftIO $ readIORef ref
      case M.lookup tbl r >>= M.lookup k of
        Just _ -> liftIO $ throwIO Persistence.WriteException
        Nothing -> do
          encodedData <- _encodeRowData serial handleGas v
          liftIO $ record refTxId refTxLog (TxLog (toUserTable tbl) (k ^. rowKey) encodedData)
          liftIO $ modifyIORef' ref (M.insertWith M.union tbl (M.singleton k v))
    Update -> do
      r <- liftIO $ readIORef ref
      case M.lookup tbl r >>= M.lookup k of
        Just (RowData m) -> do
          let (RowData v') = v
              nrd = RowData (M.union v' m)
          encodedData <- _encodeRowData serial handleGas nrd
          liftIO $ record refTxId refTxLog (TxLog (toUserTable tbl) (k ^. rowKey) encodedData)
          liftIO $ modifyIORef' ref (M.insertWith M.union tbl (M.singleton k nrd))
        Nothing -> liftIO $ throwIO Persistence.WriteException


  readKS ref ksn = do
    m <- readIORef ref
    pure (M.lookup ksn m)

  readNS ref ns = do
    m <- readIORef ref
    pure (M.lookup ns m)

  writeKS :: IORef (Map KeySetName KeySet) -> IORef TxId -> TxLogQueue -> KeySetName -> KeySet -> IO ()
  writeKS ref refTxId refTxLog ksn ks = do
    modifyIORef' ref (M.insert ksn ks)
    record refTxId refTxLog (TxLog "SYS:KEYSETS" (renderKeySetName ksn) (_encodeKeySet serial ks))

  writeNS :: IORef (Map NamespaceName Namespace) -> IORef TxId -> TxLogQueue -> NamespaceName  -> Namespace -> IO ()
  writeNS ref refTxId refTxLog nsn ns = do
    modifyIORef' ref (M.insert nsn ns)
    record refTxId refTxLog (TxLog "SYS:NAMESPACES" (_namespaceName nsn) (_encodeNamespace serial ns))

  readMod ref mn = do
    m <- readIORef ref
    pure (M.lookup mn m)

  writeMod :: IORef (Map ModuleName (ModuleData b i)) -> IORef TxId -> TxLogQueue -> ModuleData b i -> IO ()
  writeMod ref refTxId refTxLog md = let
    mname = view Persistence.mdModuleName md
    in do
         modifyIORef' ref (M.insert mname md)
         record refTxId refTxLog (TxLog "SYS:MODULES" (renderModuleName mname) (_encodeModuleData serial md))

  readPacts' ref pid = do
    m <- readIORef ref
    pure (M.lookup pid m)

  writePacts' :: IORef (Map DefPactId (Maybe DefPactExec)) -> IORef TxId -> TxLogQueue -> DefPactId -> Maybe DefPactExec -> IO ()
  writePacts' ref refTxId refTxLog pid pe = do
    modifyIORef' ref (M.insert pid pe)
    record refTxId refTxLog (TxLog "SYS:NAMESPACES" (renderDefPactId pid) (_encodeDefPactExec serial pe))

record :: IORef TxId -> TxLogQueue -> TxLog ByteString -> IO ()
record txId queue entry = do
  txIdNow <- readIORef txId
  modifyIORef queue $ \txMap -> Map.insertWith (<>) txIdNow [entry] txMap
