-- | A SQLite implementation of the PactDb persistence abstraction.

-- TODO: Consider using neat-interpolation to clean up the string literals in this module.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Pact.Core.Persistence.SQLite (
  withSqlitePactDb,
  unsafeCreateSqlitePactDb,
  unsafeCloseSqlitePactDb,
  StmtCache(..)
) where

import Control.Monad
import Control.Exception.Safe
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef
import Data.Text (Text)
import Control.Lens
import qualified Database.SQLite3 as SQL
import qualified Database.SQLite3.Direct as Direct
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as M

import qualified Pact.Core.Errors as E
import Pact.Core.Persistence
import Pact.Core.Guards (renderKeySetName, parseAnyKeysetName)
import Pact.Core.Names
import Pact.Core.Serialise
import Pact.Core.StableEncoding (encodeStable)

-- | Acquire a SQLite-backed `PactDB`.
--
-- Pact entities (modules, keysets, namespaces, defpacts, and usertables) will be stored
-- in the database given by the connection string.
-- The `PactSerialise` paramater is used to determine which types of values are serializable,
-- and how they should be serialized. [`serializePact`] is a good default.
--
withSqlitePactDb
  :: (MonadMask m, MonadIO m)
  => PactSerialise b i
  -> Text
  -> (PactDb b i -> m a)
  -> m a
withSqlitePactDb serial connectionString act =
  bracket connect cleanup work
  where
    work (_,(pdb, _)) = act pdb
    connect = liftIO $ do
      con <- SQL.open connectionString
      forM_ fastNoJournalPragmas $ \p -> liftIO (SQL.exec con ("PRAGMA " <> p))

      (con,) <$> initializePactDb serial con

    cleanup (db, (_pdb, cache)) = liftIO $ do
      cache' <- readIORef cache
      _ <- finalizeUserTable (_stmtUserTbl cache')
      finalizeStmt (_stmtNamespace cache')
      finalizeStmt (_stmtKeyset cache')
      finalizeStmt (_stmtModules cache')
      finalizeStmt (_stmtDefPact cache')
      SQL.close db

    finalizeUserTable  = traverse finalizeStmt

finalizeStmt :: TblStatements -> IO ()
finalizeStmt (TblStatements i u rv rk) = do
    SQL.finalize i
    SQL.finalize u
    SQL.finalize rv
    SQL.finalize rk

-- | Acquire the sqlite pact db, but do not close the connection
-- NOTE: This functions is exposed for benchmarking, but should _not_ be used
-- anywhere else in the runtime unless otherwise needed
unsafeCreateSqlitePactDb
  :: (MonadIO m)
  => PactSerialise b i
  -> Text
  -> m (PactDb b i, SQL.Database, IORef StmtCache)
unsafeCreateSqlitePactDb serial connectionString = liftIO $ do
  con <- SQL.open connectionString
  (pdb, cache) <- initializePactDb serial con
  pure (pdb, con, cache)

unsafeCloseSqlitePactDb :: SQL.Database -> IORef StmtCache -> IO ()
unsafeCloseSqlitePactDb db cache = liftIO $ do
    cache' <- readIORef cache
    _ <- traverse finalizeStmt (_stmtUserTbl cache')
    finalizeStmt (_stmtNamespace cache')
    finalizeStmt (_stmtKeyset cache')
    finalizeStmt (_stmtModules cache')
    finalizeStmt (_stmtDefPact cache')
    SQL.close db

fastNoJournalPragmas :: [Text]
fastNoJournalPragmas = [
  "synchronous = OFF",
  "journal_mode = MEMORY",
  "locking_mode = EXCLUSIVE",
  "temp_store = MEMORY",
  "page_size = 1024"
  ]

createSysTables :: SQL.Database -> IO StmtCache
createSysTables db = do
  ks <- mkTbl (renderDomain DKeySets)
  mods <- mkTbl (renderDomain DModules)
  pacts <- mkTbl (renderDomain DDefPacts)
  ns <- mkTbl (renderDomain DNamespaces)
  pure $ StmtCache
    { _stmtNamespace = ns
    , _stmtKeyset = ks
    , _stmtModules = mods
    , _stmtDefPact = pacts
    , _stmtUserTbl = M.empty
    }

  where
    mkTbl tbl = do
      SQL.exec db (cStmt tbl)
      mkTblStatement db tbl
    cStmt tbl = "CREATE TABLE IF NOT EXISTS \"" <> tbl <> "\" \
                \ (txid UNSIGNED BIG INT, \
                \  rowkey TEXT, \
                \  rowdata BLOB, \
                \  UNIQUE (txid, rowkey))"

data TblStatements
  = TblStatements
  { _tblInsert :: SQL.Statement
  , _tblInsertOrUpdate :: SQL.Statement
  , _tblReadValue :: SQL.Statement
  , _tblReadKeys :: SQL.Statement
  }

mkTblStatement :: SQL.Database -> Text -> IO TblStatements
mkTblStatement db tbl = do
      insertStmt <- SQL.prepare db ("INSERT INTO \"" <> tbl <> "\" (txid, rowkey, rowdata) VALUES (?,?,?)")
      insertOrUpdateStmt <- SQL.prepare db ("INSERT OR REPLACE INTO \"" <> tbl <> "\" (txid, rowkey, rowdata) VALUES (?,?,?)")
      readValueStmt <- SQL.prepare db ("SELECT rowdata FROM \""<> tbl <> "\" WHERE rowkey = ? ORDER BY txid DESC LIMIT 1")
      readKeysStmt <-  SQL.prepare db ("SELECT rowkey FROM \""<> tbl <> "\" ORDER BY txid DESC")
      pure $ TblStatements insertStmt insertOrUpdateStmt readValueStmt readKeysStmt


data StmtCache
  = StmtCache
  { _stmtNamespace :: TblStatements
  , _stmtKeyset    :: TblStatements
  , _stmtModules   :: TblStatements
  , _stmtDefPact   :: TblStatements
  , _stmtUserTbl   :: M.Map TableName TblStatements
  }

-- | Create all tables that should exist in a fresh pact db,
--   or ensure that they are already created.
initializePactDb :: PactSerialise b i -> SQL.Database  -> IO (PactDb b i, IORef StmtCache)
initializePactDb serial db = do
  stmtsCache <- newIORef =<< createSysTables db
  txId <- newIORef (TxId 0)
  txLog <- newIORef []
  pure (PactDb
    { _pdbPurity = PImpure
    , _pdbRead = read' serial db stmtsCache
    , _pdbWrite = write' serial db txId txLog stmtsCache
    , _pdbKeys = readKeys db stmtsCache
    , _pdbCreateUserTable = createUserTable db txLog stmtsCache
    , _pdbBeginTx = beginTx txId db txLog
    , _pdbCommitTx = commitTx txId db txLog
    , _pdbRollbackTx = rollbackTx db txLog
    }, stmtsCache)

readKeys :: forall k v b i. SQL.Database -> IORef StmtCache -> Domain k v b i -> IO [k]
readKeys _db stmtCache = \case
  DKeySets -> withStmt (_tblReadKeys . _stmtKeyset <$> readIORef stmtCache) $ \stmt -> do
    parsedKS <- fmap parseAnyKeysetName <$> collect stmt []
    case sequence parsedKS of
      Left _ -> fail "unexpected decoding"
      Right v -> pure v

  DModules -> withStmt (_tblReadKeys . _stmtModules <$> readIORef stmtCache) $ \stmt ->
     fmap parseModuleName <$> collect stmt [] >>= \mns -> case sequence mns of
      Nothing -> fail "unexpected decoding"
      Just mns' -> pure mns'

  DDefPacts -> withStmt (_tblReadKeys . _stmtDefPact <$> readIORef stmtCache) $ \stmt ->
     fmap DefPactId <$> collect stmt []

  DNamespaces -> withStmt (_tblReadKeys . _stmtNamespace <$> readIORef stmtCache) $ \stmt ->
     fmap NamespaceName <$> collect stmt []


  DUserTables tbl -> do
     tblCache <- _stmtUserTbl <$> readIORef stmtCache
     case M.lookup tbl tblCache of
       Nothing -> fail "invariant failure: table unknown"
       Just stmt -> withStmt (pure $ _tblReadKeys stmt) $ \s -> fmap RowKey <$> collect s []
  where
    collect stmt acc = SQL.step stmt >>= \case
       SQL.Done -> SQL.reset stmt >> pure acc
       SQL.Row -> do
          [SQL.SQLText value] <- SQL.columns stmt
          collect stmt (value:acc)



commitTx :: IORef TxId -> SQL.Database -> IORef [TxLog ByteString] -> IO [TxLog ByteString]
commitTx txid db txLog = do
  _ <- atomicModifyIORef' txid (\old@(TxId n) -> (TxId (succ n), old))
  SQL.exec db "COMMIT TRANSACTION"
  readIORef txLog

beginTx :: IORef TxId -> SQL.Database -> IORef [TxLog ByteString] -> ExecutionMode -> IO (Maybe TxId)
beginTx txid db txLog em = do
    SQL.exec db "BEGIN TRANSACTION"
    writeIORef txLog []
    case em of
      Transactional -> Just <$> readIORef txid
      Local -> pure Nothing

rollbackTx :: SQL.Database -> IORef [TxLog ByteString] -> IO ()
rollbackTx db txLog = do
  SQL.exec db "ROLLBACK TRANSACTION"
  writeIORef txLog []

createUserTable
  :: SQL.Database
  -> IORef [TxLog ByteString]
  -> IORef StmtCache
  -> TableName
  -> GasM b i ()
createUserTable db txLog stmtCache tbl = do
  let uti = UserTableInfo (_tableModuleName tbl)
  liftIO $ do
    SQL.exec db stmt
    modifyIORef' txLog (TxLog "SYS:usertables" (_tableName tbl) (encodeStable uti) :)
    stmts <- mkTblStatement db tblName
    cache <- readIORef stmtCache
    let insert = modifyIORef' stmtCache (\c -> c{_stmtUserTbl = M.insert tbl stmts (_stmtUserTbl c)})
    case M.lookup tbl (_stmtUserTbl cache) of
      Nothing -> insert
      Just old -> do
        finalizeStmt old
        insert
  where
    stmt = "CREATE TABLE IF NOT EXISTS \"" <> tblName <> "\" \
           \ (txid UNSIGNED BIG INT, \
           \  rowkey TEXT, \
           \  rowdata BLOB, \
           \  UNIQUE (txid, rowkey))"
    tblName = toUserTable tbl



write'
  :: forall k v b i
  .  PactSerialise b i
  -> SQL.Database
  -> IORef TxId
  -> IORef [TxLog ByteString]
  -> IORef StmtCache
  -> WriteType
  -> Domain k v b i
  -> k
  -> v
  -> GasM b i ()
write' serial db txId txLog stmtCache wt domain k v =
  case domain of
    DUserTables tbl -> liftIO (checkInsertOk tbl k) >>= \case
      Nothing -> do
        encoded <- _encodeRowData serial v
        liftIO $ do
          tblCache <-_stmtUserTbl <$> readIORef stmtCache
          case M.lookup tbl tblCache of
            Nothing -> fail "invariant failure: table unknown"
            Just tblStmts -> withStmt (pure $ _tblInsert tblStmts) $ \stmt -> do
              let RowKey k' = k
              TxId i <- readIORef txId
              SQL.bind stmt [SQL.SQLInteger (fromIntegral i), SQL.SQLText k', SQL.SQLBlob encoded]
              doWrite stmt (TxLog (renderDomain domain) k' encoded:)

      Just old -> do
        let
          RowData old' = old
          RowData v' = v
          new = RowData (M.union v' old')
        encoded <- _encodeRowData serial new
        liftIO $ do
          tblCache <-_stmtUserTbl <$> readIORef stmtCache
          case M.lookup tbl tblCache of
            Nothing -> fail "invariant failure: table unknown"
            Just tblStmts -> withStmt (pure $ _tblInsertOrUpdate tblStmts) $ \stmt -> do
              let RowKey k' = k
              TxId i <- readIORef txId
              SQL.bind stmt [SQL.SQLInteger (fromIntegral i), SQL.SQLText k', SQL.SQLBlob encoded]
              doWrite stmt (TxLog (renderDomain domain) k' encoded:)

    DKeySets -> liftIO $ withStmt (_tblInsertOrUpdate . _stmtKeyset <$> readIORef stmtCache) $ \stmt -> do
      let encoded = _encodeKeySet serial v
      TxId i <- readIORef txId
      SQL.clearBindings stmt
      SQL.bind stmt [SQL.SQLInteger (fromIntegral i), SQL.SQLText (renderKeySetName k), SQL.SQLBlob encoded]
      doWrite stmt (TxLog (renderDomain domain) (renderKeySetName k) encoded:)

    DModules -> liftIO $ withStmt (_tblInsertOrUpdate . _stmtModules <$> readIORef stmtCache) $ \stmt -> do
      let encoded = _encodeModuleData serial v
      TxId i <- readIORef txId
      SQL.bind stmt [SQL.SQLInteger (fromIntegral i), SQL.SQLText (renderModuleName k), SQL.SQLBlob encoded]
      doWrite stmt (TxLog (renderDomain domain) (renderModuleName k) encoded:)

    DDefPacts -> liftIO $ withStmt (_tblInsertOrUpdate . _stmtDefPact <$> readIORef stmtCache) $ \stmt -> do
       let encoded = _encodeDefPactExec serial v
           DefPactId k' = k
       TxId i <- readIORef txId
       SQL.bind stmt [SQL.SQLInteger (fromIntegral i), SQL.SQLText k', SQL.SQLBlob encoded]
       doWrite stmt (TxLog (renderDomain domain) k' encoded:)

    DNamespaces ->
      liftIO $ withStmt (_tblInsertOrUpdate . _stmtNamespace <$> readIORef stmtCache) $ \stmt -> do
      let encoded = _encodeNamespace serial v
          NamespaceName k' = k
      TxId i <- readIORef txId
      SQL.bind stmt [SQL.SQLInteger (fromIntegral i), SQL.SQLText k', SQL.SQLBlob encoded]
      doWrite stmt  (TxLog (renderDomain domain) k' encoded:)
  where
    checkInsertOk ::  TableName -> RowKey -> IO (Maybe RowData)
    checkInsertOk tbl rk = do
      curr <- read' serial db stmtCache (DUserTables tbl) rk
      case (curr, wt) of
        (Nothing, Insert) -> return Nothing
        (Just _, Insert) -> throwIO (E.RowFoundException tbl rk)
        (Nothing, Write) -> return Nothing
        (Just old, Write) -> return $ Just old
        (Just old, Update) -> return $ Just old
        (Nothing, Update) -> throwIO (E.NoRowFound tbl rk)

    doWrite stmt txlog = Direct.stepNoCB stmt >>= \case
          Left _ ->  throwIO E.WriteException
          Right res
            | res == SQL.Done -> do
                SQL.reset stmt
                modifyIORef' txLog txlog
            | otherwise -> throwIO E.MultipleRowsReturnedFromSingleWrite

read' :: forall k v b i. PactSerialise b i -> SQL.Database -> IORef StmtCache -> Domain k v b i -> k -> IO (Maybe v)
read' serial _db stmtCache domain k = case domain of
  DKeySets -> withStmt (_tblReadValue . _stmtKeyset <$> readIORef stmtCache) $
    doRead (renderKeySetName k) (\v -> pure (view document <$> _decodeKeySet serial v))

  DModules -> withStmt (_tblReadValue . _stmtModules <$> readIORef stmtCache) $
    doRead (renderModuleName k) (\v -> pure (view document <$> _decodeModuleData serial v))

  DUserTables tbl -> do
    tblCache <- _stmtUserTbl <$> readIORef stmtCache
    case M.lookup tbl tblCache of
      Nothing -> fail "invariant failure: table unknown"
      Just stmt -> withStmt (pure $ _tblReadValue stmt) $ doRead (_rowKey k) (\v -> pure (view document <$> _decodeRowData serial v))

  DDefPacts -> do
    withStmt (_tblReadValue . _stmtDefPact <$> readIORef stmtCache) $
      doRead (renderDefPactId k) (\v -> pure (view document <$> _decodeDefPactExec serial v))

  DNamespaces ->
    withStmt (_tblReadValue . _stmtNamespace <$> readIORef stmtCache)
    (doRead (_namespaceName k) (\v -> pure (view document <$> _decodeNamespace serial v)))

  where
    doRead :: forall a. Text -> (ByteString -> IO (Maybe a)) -> SQL.Statement -> IO (Maybe a)
    doRead k' f stmt = do
       SQL.bind stmt [SQL.SQLText k']
       SQL.step stmt >>= \case
         SQL.Done -> do
           SQL.reset stmt
           pure Nothing
         SQL.Row -> do
           [SQL.SQLBlob value] <- SQL.columns stmt
           SQL.Done <- SQL.step stmt
           SQL.reset stmt
           f value

-- -- Utility functions
withStmt :: IO SQL.Statement -> (SQL.Statement -> IO a) -> IO a
withStmt stmt = bracket stmt SQL.clearBindings
