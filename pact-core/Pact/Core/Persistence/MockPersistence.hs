{-# LANGUAGE TypeFamilies #-}

module Pact.Core.Persistence.MockPersistence (
  mockPactDb
  )where


import Control.Monad (unless)
import Data.Maybe (isJust)
import Control.Lens ((^?), (^.), ix, view)
import Data.Map (Map)
import Data.IORef (IORef, modifyIORef, modifyIORef', newIORef, readIORef, writeIORef)
import GHC.Word (Word64)
import Control.Exception(throwIO)
import qualified Data.Map.Strict as M

import Pact.Core.Guards (KeySetName)
import Pact.Core.Namespace
import Pact.Core.Names (ModuleName, RowKey, TableName, DefPactId, NamespaceName, rowKey, tableName)
import Pact.Core.DefPacts.Types (DefPactExec)
import qualified Pact.Core.Persistence as Persistence
import Pact.Core.Persistence (Domain(..),
                              ExecutionMode(Local, Transactional), FQKS, TxId(..), TxLog(..), PactDb(..),
                              RowData, ModuleData, WriteType(Insert, Update, Write), RowData(..),
                              Purity(PImpure)
                             )

mockPactDb :: forall b i. IO (PactDb b i)
mockPactDb = do
  refMod <- newIORef M.empty
  refKs <- newIORef M.empty
  refUsrTbl <- newIORef M.empty
  refPacts <- newIORef M.empty
  refNS <- newIORef M.empty
  refRb <- newIORef Nothing
  refTxLog <- newIORef mempty
  refTxId <- newIORef 0
  pure $ PactDb
    { _pdbPurity = PImpure
    , _pdbRead = read' refKs refMod refNS refUsrTbl refPacts
    , _pdbWrite = write refKs refMod refNS refUsrTbl refTxId refTxLog refPacts
    , _pdbKeys = keys refKs refMod refNS refUsrTbl refPacts
    , _pdbCreateUserTable = createUsrTable refUsrTbl refTxLog
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
        pure (Just (TxId tid))

  commitTx refRb refTxId refTxLog refMod refKs refUsrTbl = readIORef refRb >>= \case
    Just (em, txl, mods, ks, usr) -> case em of
      Transactional -> do
        writeIORef refRb Nothing
        modifyIORef' refTxId (+ 1)
      Local -> do
        writeIORef refRb Nothing
        writeIORef refMod mods
        writeIORef refKs ks
        writeIORef refUsrTbl usr
        writeIORef refTxLog txl
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

  txLog refTxLog tn tid = do
    m <- readIORef refTxLog
    case M.lookup tn m of
      Just txids -> case M.lookup tid txids of
        Just n -> pure n
        Nothing -> throwIO (Persistence.NoTxLog tn tid)
      Nothing -> throwIO (Persistence.NoTxLog tn tid)

  txIds refTxLog tn txId = do
    txl <- readIORef refTxLog
    case M.lookup tn txl of
      Just mtxl -> pure [ x | x <- M.keys mtxl, x >= txId ]
      Nothing -> throwIO (Persistence.NoSuchTable tn)

  keys
    :: forall k v
    .  IORef (Map KeySetName FQKS)
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
    -> IORef (Map TableName (Map TxId [TxLog RowData]))
    -> TableName
    -> ModuleName
    -> IO ()
  createUsrTable refUsrTbl refTxLog tbl _ = do
    ref <- readIORef refUsrTbl
    case M.lookup tbl ref of
      Nothing -> do
        modifyIORef refTxLog (M.insert tbl mempty)
        modifyIORef refUsrTbl (M.insert tbl mempty)
        pure ()
      Just _ -> throwIO (Persistence.TableAlreadyExists tbl)

  read'
    :: forall k v
    .  IORef (Map KeySetName FQKS)
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

  checkTable tbl ref = do
    r <- readIORef ref
    unless (isJust (M.lookup tbl r)) $ throwIO (Persistence.NoSuchTable tbl)

  write
    :: forall k v
    .  IORef (Map KeySetName FQKS)
    -> IORef (Map ModuleName (ModuleData b i))
    -> IORef (Map NamespaceName Namespace)
    -> IORef (Map TableName (Map RowKey RowData))
    -> IORef Word64
    -> IORef (Map TableName (Map TxId [TxLog RowData]))
    -> IORef (Map DefPactId (Maybe DefPactExec))
    -> WriteType
    -> Domain k v b i
    -> k
    -> v
    -> IO ()
  write refKs refMod refNS refUsrTbl refTxId refTxLog refPacts wt domain k v = case domain of
    DKeySets -> writeKS refKs k v
    DModules -> writeMod refMod v
    DUserTables tbl -> writeRowData refUsrTbl refTxId refTxLog tbl wt k v
    DDefPacts -> writePacts' refPacts k v
    DNamespaces -> writeNS refNS k v

  readRowData ref tbl k = do
    checkTable tbl ref
    r <- readIORef ref
    pure (r ^? ix tbl . ix k)

  writeToTxLog
    :: IORef Word64
    -> IORef (Map TableName (Map TxId [TxLog RowData]))
    -> TableName
    -> RowKey
    -> RowData
    -> IO ()
  writeToTxLog refTxId refTxLog tbl k rdata = do
    tid <- readIORef refTxId
    let entry = M.singleton (TxId tid) [TxLog (tbl ^. tableName) (k ^. rowKey) rdata]
    modifyIORef' refTxLog (M.insertWith (M.unionWith (<>)) tbl entry)

  writeRowData
    :: IORef (Map TableName (Map RowKey RowData))
    -> IORef Word64
    -> IORef (Map TableName (Map TxId [TxLog RowData]))
    -> TableName
    -> WriteType
    -> RowKey
    -> RowData
    -> IO ()
  writeRowData ref refTxId refTxLog tbl wt k v = checkTable tbl ref *> case wt of
    Write -> do
      writeToTxLog refTxId refTxLog tbl k v
      modifyIORef' ref (M.insertWith M.union tbl (M.singleton k v))
    Insert -> do
      r <- readIORef ref
      case M.lookup tbl r >>= M.lookup k of
        Just _ -> throwIO Persistence.WriteException
        Nothing -> do
          writeToTxLog refTxId refTxLog tbl k v
          modifyIORef' ref (M.insertWith M.union tbl (M.singleton k v))
    Update -> do
      r <- readIORef ref
      case M.lookup tbl r >>= M.lookup k of
        Just (RowData m) -> do
          let (RowData v') = v
              nrd = RowData (M.union v' m)
          writeToTxLog refTxId refTxLog tbl k nrd
          modifyIORef' ref (M.insertWith M.union tbl (M.singleton k nrd))
        Nothing -> throwIO Persistence.WriteException


  readKS ref ksn = do
    m <- readIORef ref
    pure (M.lookup ksn m)

  readNS ref ns = do
    m <- readIORef ref
    pure (M.lookup ns m)

  writeKS ref ksn ks = modifyIORef' ref (M.insert ksn ks)

  writeNS ref nsn ns = modifyIORef' ref (M.insert nsn ns)

  readMod ref mn = do
    m <- readIORef ref
    pure (M.lookup mn m)

  writeMod ref md = let
    mname = view Persistence.mdModuleName md
    in modifyIORef' ref (M.insert mname md)

  readPacts' ref pid = do
    m <- readIORef ref
    pure (M.lookup pid m)

  writePacts' ref pid pe = modifyIORef' ref (M.insert pid pe)
