{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pact.Core.Persistence.MockPersistence (
  mockPactDb
  )where


import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (unless)
import Control.Exception.Safe
import Control.Lens ((^?), (^.), ix, view)
import Data.Maybe (isJust, fromMaybe, catMaybes)
import Data.List (find)
import Data.Map (Map)
import Data.IORef
import Data.Text(Text)
import qualified Data.Map.Strict as M
import Data.ByteString (ByteString)


import Pact.Core.Guards
import Pact.Core.Namespace
import Pact.Core.Names
import Pact.Core.DefPacts.Types (DefPactExec)
import Pact.Core.Persistence
import Pact.Core.Serialise
import Pact.Core.StableEncoding

import qualified Pact.Core.Errors as Errors



type TxLogQueue = IORef (Map TxId [TxLog ByteString])

-- | Small newtype to ensure we
--   turn the table names into Text keys
newtype Rendered v
  = Rendered { _unRender :: Text }
  deriving (Eq, Ord, Show)

renderTableName :: TableName -> Rendered TableName
renderTableName tn = Rendered (toUserTable tn)

newtype MockUserTable =
  MockUserTable (Map (Rendered TableName) (Map RowKey ByteString))
  deriving (Eq, Show, Ord)
  deriving (Semigroup, Monoid) via (Map (Rendered TableName) (Map RowKey ByteString))

newtype MockSysTable k v =
  MockSysTable (Map (Rendered k) ByteString)
  deriving (Eq, Show, Ord)
  deriving (Semigroup, Monoid) via (Map (Rendered k) ByteString)

data TableFromDomain k v b i where
  TFDUser :: IORef (MockUserTable) -> TableFromDomain RowKey RowData b i
  TFDSys :: IORef (MockSysTable k v) -> TableFromDomain k v b i

tableFromDomain :: Domain k v b i -> PactTables b i -> TableFromDomain k v b i
tableFromDomain d PactTables{..} = case d of
  DUserTables {} -> TFDUser ptUser
  DKeySets -> TFDSys ptKeysets
  DModules -> TFDSys ptModules
  DNamespaces -> TFDSys ptNamespaces
  DDefPacts -> TFDSys ptDefPact


-- | A record collection of all of the mutable
--   table references
data PactTables b i
  = PactTables
  { ptTxId :: !(IORef TxId)
  , ptUser :: !(IORef MockUserTable)
  , ptModules :: !(IORef (MockSysTable ModuleName (ModuleData b i)))
  , ptKeysets :: !(IORef (MockSysTable KeySetName KeySet))
  , ptNamespaces :: !(IORef (MockSysTable NamespaceName Namespace))
  , ptDefPact :: !(IORef (MockSysTable DefPactId (Maybe DefPactExec)))
  , ptTxLogQueue :: TxLogQueue
  , ptRollbackState :: IORef (Maybe (PactTablesState b i))
  }

-- | The state of the database at the beginning of a transaction
data PactTablesState b i
  = PactTablesState
  { _ptsExecMode :: ExecutionMode
  , _ptsUser :: !MockUserTable
  , _ptsModules :: !(MockSysTable ModuleName (ModuleData b i))
  , _ptsKeysets :: !(MockSysTable KeySetName KeySet)
  , _ptsNamespaces :: !(MockSysTable NamespaceName Namespace)
  , _ptsDefPact :: !(MockSysTable DefPactId (Maybe DefPactExec))
  , _ptsTxLogQueue :: Map TxId [TxLog ByteString]
  }


-- | Create an empty table
createPactTables :: IO (PactTables b i)
createPactTables = do
  refMod <- newIORef mempty
  refKs <- newIORef mempty
  refUsrTbl <- newIORef mempty
  refPacts <- newIORef mempty
  refNS <- newIORef mempty
  refRb <- newIORef Nothing
  refTxLog <- newIORef mempty
  refTxId <- newIORef $ TxId 0
  pure $ PactTables
    { ptTxId = refTxId
    , ptUser = refUsrTbl
    , ptModules = refMod
    , ptKeysets = refKs
    , ptNamespaces = refNS
    , ptDefPact = refPacts
    , ptTxLogQueue = refTxLog
    , ptRollbackState = refRb }

getRollbackState :: ExecutionMode -> PactTables b i -> IO (PactTablesState b i)
getRollbackState em PactTables{..} =
  PactTablesState em
    <$> readIORef ptUser
    <*> readIORef ptModules
    <*> readIORef ptKeysets
    <*> readIORef ptNamespaces
    <*> readIORef ptDefPact
    <*> readIORef ptTxLogQueue



mockPactDb :: forall b i. PactSerialise b i -> IO (PactDb b i)
mockPactDb serial = do
  pactTables <- createPactTables
  pure $ PactDb
    { _pdbPurity = PImpure
    , _pdbRead = read' pactTables
    , _pdbWrite = write pactTables
    , _pdbKeys = keys pactTables
    , _pdbCreateUserTable = createUsrTable pactTables
    , _pdbBeginTx = beginTx pactTables
    , _pdbCommitTx = commitTx pactTables
    , _pdbRollbackTx = rollbackTx pactTables
    , _pdbTxIds = txIds (ptTxLogQueue pactTables)
    , _pdbGetTxLog = txLog (ptTxLogQueue pactTables)
    }
  where
  beginTx pts@PactTables{..} em = do
    readIORef ptRollbackState >>= \case
      -- A tx is already in progress, so we fail to get a
      -- new tx id
      Just _ -> pure Nothing
      -- No tx in progress, get the state of the pure tables prior to rollback.
      Nothing -> do
        rbs <- getRollbackState em pts

        writeIORef ptRollbackState (Just rbs)
        tid <- readIORef ptTxId
        pure (Just tid)

  commitTx PactTables{..} = readIORef ptRollbackState >>= \case
    -- We are successfully in a transaction
    Just (PactTablesState em usr mods ks ns dp txl) -> case em of
      Transactional -> do
        -- Reset the rollback state,
        -- increment to the next tx id, and return the
        -- tx logs for the transaction
        writeIORef ptRollbackState Nothing
        txId <- atomicModifyIORef' ptTxId (\(TxId i) -> (TxId (succ i), TxId i))
        txLogQueue <- readIORef ptTxLogQueue
        pure (M.findWithDefault [] txId txLogQueue)
      Local -> do
        -- in local, we simply roll back all tables
        -- then and return the logs, then roll back the logs table
        writeIORef ptRollbackState Nothing
        writeIORef ptModules mods
        writeIORef ptKeysets ks
        writeIORef ptUser usr
        writeIORef ptTxLogQueue txl
        writeIORef ptNamespaces ns
        writeIORef ptDefPact dp

        txId <- readIORef ptTxId
        txl' <- readIORef ptTxLogQueue

        let logs = M.findWithDefault [] txId txl'
        writeIORef ptTxLogQueue txl
        pure logs
    Nothing ->
      throwIO Errors.NoTxToCommit

  rollbackTx PactTables{..} = readIORef ptRollbackState >>= \case
    Just (PactTablesState _ usr mods ks ns dp txl) -> do
      writeIORef ptRollbackState Nothing
      writeIORef ptModules mods
      writeIORef ptKeysets ks
      writeIORef ptUser usr
      writeIORef ptTxLogQueue txl
      writeIORef ptNamespaces ns
      writeIORef ptDefPact dp
    Nothing -> throwIO Errors.NoTxToCommit

  txLog :: TxLogQueue -> TableName -> TxId -> IO [TxLog RowData]
  txLog refTxLog tn tid = do
    txl <- readIORef refTxLog
    case M.lookup tid txl of
      Just txl' -> pure $ fromMaybe [] (traverse (traverse (fmap (view document) . _decodeRowData serial)) (filter (\(TxLog dom _ _) -> dom == toUserTable tn) txl'))
      Nothing -> pure []

  txIds :: TxLogQueue -> TableName -> TxId -> IO [TxId]
  txIds refTxLog tn (TxId txId) = do
    txl <- readIORef refTxLog
    let
      userTab = toUserTable tn
      subTxs = M.filterWithKey (\(TxId i) txs -> i >= txId && isJust (find (\(TxLog dom _ _) -> dom == userTab) txs)) txl
    pure (M.keys subTxs)

  keys
    :: PactTables b i
    -> Domain k v b i
    -> IO [k]
  keys PactTables{..} d = case d of
    DKeySets -> do
      MockSysTable r <- readIORef ptKeysets
      -- Note: the parser only fails on null input, so
      -- if this ever fails, then somehow the null key got into the keysets.
      -- this is benign.
      let getKeysetName = fromMaybe (KeySetName "" Nothing) . rightToMaybe . parseAnyKeysetName
      return $ getKeysetName . _unRender <$> M.keys r
    DModules -> do
      MockSysTable r <- readIORef ptModules
      let getModuleName = parseModuleName . _unRender
      return $ catMaybes $ getModuleName <$> M.keys r
    DUserTables tbl -> do
      MockUserTable r <- readIORef ptUser
      let tblName = renderTableName tbl
      case M.lookup tblName r of
        Just t -> return (M.keys t)
        Nothing -> throwIO (Errors.NoSuchTable tbl)
    DDefPacts -> do
      MockSysTable r <- readIORef ptDefPact
      return $ DefPactId . _unRender <$> M.keys r
    DNamespaces -> do
      MockSysTable r <- readIORef ptNamespaces
      pure $ NamespaceName . _unRender <$> M.keys r

  createUsrTable
    :: PactTables b i
    -> TableName
    -> GasM b i ()
  createUsrTable tbls@PactTables{..} tbl = do
    let uti = UserTableInfo (_tableModuleName tbl)
    MockUserTable ref <- liftIO $ readIORef ptUser
    let tblName = renderTableName tbl
    case M.lookup tblName ref of
      Nothing -> do
        liftIO $ record tbls (TxLog "SYS:usertables" (_tableName tbl) (encodeStable uti))
        liftIO $ modifyIORef ptUser (\(MockUserTable m) -> MockUserTable (M.insert tblName mempty m))
        pure ()
      Just _ -> liftIO $ throwIO (Errors.TableAlreadyExists tbl)


  read'
    :: forall k v
    .  PactTables b i
    -> Domain k v b i
    -> k
    -> IO (Maybe v)
  read' PactTables{..} domain k = case domain of
    DKeySets -> readSysTable ptKeysets k (Rendered . renderKeySetName) _decodeKeySet
    DModules -> readSysTable ptModules k (Rendered . renderModuleName) _decodeModuleData
    DUserTables tbl ->
      readRowData ptUser tbl k
    DDefPacts -> readSysTable ptDefPact k (Rendered . _defPactId) _decodeDefPactExec
    DNamespaces ->
      readSysTable ptNamespaces k (Rendered . _namespaceName) _decodeNamespace

  checkTable :: MonadIO m => Rendered TableName -> TableName -> MockUserTable -> m ()
  checkTable tbl tn (MockUserTable r) = liftIO $ do
    unless (isJust (M.lookup tbl r)) $ throwIO (Errors.NoSuchTable tn)

  write
    :: forall k v
    .  PactTables b i
    -> WriteType
    -> Domain k v b i
    -> k
    -> v
    -> GasM b i ()
  write pt wt domain k v = case domain of
    -- Todo : incrementally serialize other types
    DKeySets -> liftIO $ writeSysTable pt domain k v (Rendered . renderKeySetName) _encodeKeySet
    DModules -> liftIO $ writeSysTable pt domain k v (Rendered . renderModuleName) _encodeModuleData
    DUserTables tbl -> writeRowData pt tbl wt k v
    DDefPacts -> liftIO $ liftIO $ writeSysTable pt domain k v (Rendered . _defPactId) _encodeDefPactExec
    DNamespaces -> liftIO $ liftIO $ writeSysTable pt domain k v (Rendered . _namespaceName) _encodeNamespace

  readRowData ref tbl k = do
    let tblName = renderTableName tbl
    mt@(MockUserTable usrTables) <- readIORef ref
    checkTable tblName tbl mt
    case usrTables ^? ix tblName . ix k of
      Just bs -> case _decodeRowData serial bs of
        Just doc -> pure (Just (view document doc))
        Nothing -> throwM $ Errors.RowReadDecodeFailure (_rowKey k)
      Nothing -> pure Nothing

  writeRowData
    :: PactTables b i
    -> TableName
    -> WriteType
    -> RowKey
    -> RowData
    -> GasM b i ()
  writeRowData pts@PactTables{..} tbl wt k v = do
    let tblName = renderTableName tbl
    mt@(MockUserTable usrTables) <- liftIO $ readIORef ptUser
    checkTable tblName tbl mt
    case wt of
      Write -> do
        encodedData <- _encodeRowData serial v
        liftIO $ record pts (TxLog (toUserTable tbl) (k ^. rowKey) encodedData)
        liftIO $ modifyIORef' ptUser
          (\(MockUserTable m) -> (MockUserTable (M.adjust (M.insert k encodedData) tblName  m)))
      Insert -> do
        case M.lookup tblName usrTables >>= M.lookup k of
          Just _ -> liftIO $ throwIO (Errors.RowFoundException tbl k)
          Nothing -> do
            encodedData <- _encodeRowData serial v
            liftIO $ record pts (TxLog (toUserTable tbl) (k ^. rowKey) encodedData)
            liftIO $ modifyIORef' ptUser
              (\(MockUserTable m) -> (MockUserTable (M.adjust (M.insert k encodedData) tblName  m)))
      Update -> do
        case M.lookup tblName usrTables >>= M.lookup k of
          Just bs -> case view document <$> _decodeRowData serial bs of
            Just (RowData m) -> do
              let (RowData v') = v
                  nrd = RowData (M.union v' m)
              encodedData <- _encodeRowData serial nrd
              liftIO $ record pts (TxLog (toUserTable tbl) (k ^. rowKey) encodedData)
              liftIO $ modifyIORef' ptUser $ \(MockUserTable mut) ->
                MockUserTable (M.insertWith M.union tblName (M.singleton k encodedData) mut)
            Nothing ->
              liftIO $ throwIO (Errors.RowReadDecodeFailure (_rowKey k))
          Nothing ->
            liftIO $ throwIO (Errors.NoRowFound tbl k)

  readSysTable
    :: IORef (MockSysTable k v)
    -> k
    -> (k -> Rendered k)
    -> (PactSerialise b i -> ByteString -> Maybe (Document v))
    -> IO (Maybe v)
  readSysTable ref rowkey renderKey decode = do
    MockSysTable m <- readIORef ref
    case M.lookup (renderKey rowkey) m of
      Just bs -> case decode serial bs of
        Just rd -> pure (Just (view document rd))
        Nothing ->
          throwM (Errors.RowReadDecodeFailure (_unRender (renderKey rowkey)))
      Nothing -> pure Nothing
  {-# INLINE readSysTable #-}

  writeSysTable
    :: PactTables b i
    -> Domain k v b i
    -> k
    -> v
    -> (k -> Rendered k)
    -> (PactSerialise b i -> v -> ByteString)
    -> IO ()
  writeSysTable pts domain rowkey value renderKey encode = do
    case tableFromDomain domain pts of
      TFDSys ref -> do
        let encodedData = encode serial value
        record pts (TxLog (renderDomain domain) (_unRender (renderKey rowkey)) encodedData)
        modifyIORef' ref $ \(MockSysTable msys) ->
            MockSysTable (M.insert (renderKey rowkey) encodedData msys)
      TFDUser _ ->
        -- noop, should not be used for user tables
        error "Invariant violated: writeSysTable used for user table"

rightToMaybe :: Either e a -> Maybe a
rightToMaybe = \case
  Left{} -> Nothing
  Right a -> Just a

record :: PactTables b i -> TxLog ByteString -> IO ()
record PactTables{..} entry = do
  txIdNow <- readIORef ptTxId
  modifyIORef ptTxLogQueue $ \txMap -> M.insertWith (<>) txIdNow [entry] txMap
