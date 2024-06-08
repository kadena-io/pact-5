-- | A SQLite implementation of the PactDb persistence abstraction.

-- TODO: Consider using neat-interpolation to clean up the string literals in this module.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Pact.Core.Persistence.SQLite (
  withSqlitePactDb,
  unsafeCreateSqlitePactDb
) where

-- import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef
import Data.Text (Text)
import Control.Lens (view)
import qualified Database.SQLite3 as SQL
import qualified Database.SQLite3.Direct as Direct
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map

import Pact.Core.Errors
import qualified Pact.Core.Errors as E
import Pact.Core.Persistence
import Pact.Core.Guards (renderKeySetName, parseAnyKeysetName)
import Pact.Core.Names
import Pact.Core.Gas
import Pact.Core.PactValue
import Pact.Core.Literal
import Control.Exception (throwIO)
import Pact.Core.Serialise

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
  bracket connect cleanup (\db -> liftIO (initializePactDb serial db) >>= act)
  where
    connect = liftIO $ SQL.open connectionString
    cleanup db = liftIO $ SQL.close db

-- | Acquire the sqlite pact db, but do not close the connection
-- NOTE: This functions is exposed for benchmarking, but should _not_ be used
-- anywhere else in the runtime unless otherwise needed
unsafeCreateSqlitePactDb
  :: (MonadIO m)
  => PactSerialise b i
  -> Text
  -> m (PactDb b i, SQL.Database)
unsafeCreateSqlitePactDb serial connectionString  = do
  db <- liftIO $ SQL.open connectionString
  _ <- forM_ fastNoJournalPragmas $ \p -> liftIO (SQL.exec db ("PRAGMA " <> p))
  (,db) <$> liftIO (initializePactDb serial db)

fastNoJournalPragmas :: [Text]
fastNoJournalPragmas = [
  "synchronous = OFF",
  "journal_mode = MEMORY",
  "locking_mode = EXCLUSIVE",
  "temp_store = MEMORY"
  ]

createSysTables :: SQL.Database -> IO ()
createSysTables db = do
  SQL.exec db (cStmt "SYS:KEYSETS")
  SQL.exec db (cStmt "SYS:MODULES")
  SQL.exec db (cStmt "SYS:PACTS")
  SQL.exec db (cStmt "SYS:NAMESPACES")
  where
    cStmt tbl = "CREATE TABLE IF NOT EXISTS \"" <> tbl <> "\" \
                \ (txid UNSIGNED BIG INT, \
                \  rowkey TEXT, \
                \  rowdata BLOB, \
                \  UNIQUE (txid, rowkey))"

-- | Create all tables that should exist in a fresh pact db,
--   or ensure that they are already created.
initializePactDb :: PactSerialise b i -> SQL.Database  -> IO (PactDb b i)
initializePactDb serial db = do
  createSysTables db
  txId <- newIORef (TxId 0)
  txLog <- newIORef []
  pure $ PactDb
    { _pdbPurity = PImpure
    , _pdbRead = read' serial db
    , _pdbWrite = write' serial db txId txLog
    , _pdbKeys = readKeys db
    , _pdbCreateUserTable = \tn -> createUserTable serial db txLog tn
    , _pdbBeginTx = beginTx txId db txLog
    , _pdbCommitTx = commitTx txId db txLog
    , _pdbRollbackTx = rollbackTx db txLog
    , _pdbTxIds = listTxIds db
    , _pdbGetTxLog = getTxLog serial db txId txLog
    }

getTxLog :: PactSerialise b i -> SQL.Database -> IORef TxId -> IORef [TxLog ByteString] -> TableName -> TxId -> IO [TxLog RowData]
getTxLog serial db currTxId txLog tab txId = do
  currTxId' <- readIORef currTxId
  if currTxId' == txId
    then do
    txLog' <- readIORef txLog
    let
      userTabLogs = filter (\tl -> _tableName tab == _txDomain tl) txLog'
      env :: Maybe [TxLog RowData] = traverse (traverse (fmap (view document) . _decodeRowData serial)) userTabLogs
    case env of
      Nothing -> fail "undexpected decoding error"
      Just xs -> pure xs
    else withStmt db ("SELECT rowkey,rowdata FROM \"" <> toUserTable tab <> "\" WHERE txid = ?") $ \stmt -> do
                         let TxId i = txId
                         SQL.bind stmt [SQL.SQLInteger $ fromIntegral i]
                         txLogBS <- collect stmt []
                         case traverse (traverse (fmap (view document) . _decodeRowData serial)) txLogBS of
                           Nothing -> fail "unexpected decoding error"
                           Just txl -> pure txl
  where
    collect stmt acc = SQL.step stmt >>= \case
        SQL.Done -> pure acc
        SQL.Row -> do
          [SQL.SQLText key, SQL.SQLBlob value] <- SQL.columns stmt
          collect stmt (TxLog (toUserTable tab) key value:acc)

readKeys :: forall k v b i. SQL.Database -> Domain k v b i -> IO [k]
readKeys db = \case
  DKeySets -> withStmt db "SELECT rowkey FROM \"SYS:KEYSETS\" ORDER BY txid DESC" $ \stmt -> do
    parsedKS <- fmap parseAnyKeysetName <$> collect stmt []
    case sequence parsedKS of
      Left _ -> fail "unexpected decoding"
      Right v -> pure v
  DModules -> withStmt db "SELECT rowkey FROM \"SYS:MODULES\" ORDER BY txid DESC" $ \stmt -> fmap parseModuleName <$> collect stmt [] >>= \mns -> case sequence mns of
    Nothing -> fail "unexpected decoding"
    Just mns' -> pure mns'
  DDefPacts -> withStmt db "SELECT rowkey FROM \"SYS:PACTS\" ORDER BY txid DESC" $ \stmt -> fmap DefPactId <$> collect stmt []
  DNamespaces -> withStmt db "SELECT rowkey FROM \"SYS:NAMESPACES\" ORDER BY txid DESC" $ \stmt -> fmap NamespaceName <$> collect stmt []
  DUserTables tbl -> withStmt db ("SELECT rowkey FROM \"" <> toUserTable tbl <> "\" ORDER BY txid DESC") $ \stmt -> fmap RowKey <$> collect stmt []
  where
    collect stmt acc = SQL.step stmt >>= \case
        SQL.Done -> pure acc
        SQL.Row -> do
          [SQL.SQLText value] <- SQL.columns stmt
          collect stmt (value:acc)


listTxIds :: SQL.Database -> TableName -> TxId -> IO [TxId]
listTxIds db tbl (TxId minTxId) = withStmt db ("SELECT txid FROM \"" <> toUserTable tbl <> "\" WHERE txid >= ? ORDER BY txid ASC") $ \stmt -> do
  SQL.bind stmt [SQL.SQLInteger $ fromIntegral minTxId]
  collect stmt []
  where
    collect stmt acc = SQL.step stmt >>= \case
        SQL.Done -> pure acc
        SQL.Row -> do
          [SQL.SQLInteger value] <- SQL.columns stmt
          -- Here we convert the Int64 received from SQLite into Word64
          -- using `fromIntegral`. It is assumed that recorded txids
          -- in the database will never be negative integers.
          collect stmt (TxId (fromIntegral value):acc)

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

createUserTable :: PactSerialise b i -> SQL.Database -> IORef [TxLog ByteString] -> TableName -> GasM (PactError i) b ()
createUserTable serial db txLog tbl = do
  let
    rd = RowData $ Map.singleton (Field "utModule")
         (PObject $ Map.fromList
          [ (Field "namespace", maybe (PLiteral LUnit) (PString . _namespaceName) (_mnNamespace (_tableModuleName tbl)))
          , (Field "name", PString (_tableName tbl))
          ])
  rdEnc <- _encodeRowData serial rd
  liftIO $ SQL.exec db stmt
  liftIO $ modifyIORef' txLog (TxLog "SYS:usertables" (_tableName tbl) rdEnc :)

  where
    stmt = "CREATE TABLE IF NOT EXISTS " <> tblName <> " \
           \ (txid UNSIGNED BIG INT, \
           \  rowkey TEXT, \
           \  rowdata BLOB, \
           \  UNIQUE (txid, rowkey))"
    tblName = "\"" <> toUserTable tbl <> "\""

write'
  :: forall k v b i
  .  PactSerialise b i
  -> SQL.Database
  -> IORef TxId
  -> IORef [TxLog ByteString]
  -> WriteType
  -> Domain k v b i
  -> k
  -> v
  -> GasM (PactError i) b ()
write' serial db txId txLog wt domain k v = do
  case domain of
    DUserTables tbl -> liftIO (checkInsertOk tbl k) >>= \case
      Nothing -> do
        encoded <- _encodeRowData serial v
        liftIO $ withStmt db ("INSERT INTO \"" <> toUserTable tbl <> "\" (txid, rowkey, rowdata) VALUES (?,?,?)") $ \stmt -> do
          let
            RowKey k' = k
          TxId i <- readIORef txId
          SQL.bind stmt [SQL.SQLInteger (fromIntegral i), SQL.SQLText k', SQL.SQLBlob encoded]
          doWrite stmt (TxLog (_tableName tbl) k' encoded:)

      Just old -> do
        let
          RowData old' = old
          RowData v' = v
          new = RowData (Map.union v' old')
        encoded <- _encodeRowData serial new
        liftIO $ withStmt db ("INSERT OR REPLACE INTO \"" <> toUserTable tbl <> "\" (txid, rowkey, rowdata) VALUES (?,?,?)") $ \stmt -> do
          let
            RowKey k' = k
          TxId i <- readIORef txId
          SQL.bind stmt [SQL.SQLInteger (fromIntegral i), SQL.SQLText k', SQL.SQLBlob encoded]
          doWrite stmt (TxLog (_tableName tbl) k' encoded:)

    DKeySets -> liftIO $ withStmt db "INSERT OR REPLACE INTO \"SYS:KEYSETS\" (txid, rowkey, rowdata) VALUES (?,?,?)" $ \stmt -> do
      let encoded = _encodeKeySet serial v
      TxId i <- readIORef txId
      SQL.bind stmt [SQL.SQLInteger (fromIntegral i), SQL.SQLText (renderKeySetName k), SQL.SQLBlob encoded]
      doWrite stmt (TxLog "SYS:KEYSETS" (renderKeySetName k) encoded:)

    DModules -> liftIO $ withStmt db "INSERT OR REPLACE INTO \"SYS:MODULES\" (txid, rowkey, rowdata) VALUES (?,?,?)" $ \stmt -> do
      let encoded = _encodeModuleData serial v
      TxId i <- readIORef txId
      SQL.bind stmt [SQL.SQLInteger (fromIntegral i), SQL.SQLText (renderModuleName k), SQL.SQLBlob encoded]
      doWrite stmt (TxLog "SYS:MODULES" (renderModuleName k) encoded:)

    DDefPacts -> liftIO $ withStmt db "INSERT OR REPLACE INTO \"SYS:PACTS\" (txid, rowkey, rowdata) VALUES (?,?,?)" $ \stmt -> do
      let
        encoded = _encodeDefPactExec serial v
        DefPactId k' = k
      TxId i <- readIORef txId
      SQL.bind stmt [SQL.SQLInteger (fromIntegral i), SQL.SQLText k', SQL.SQLBlob encoded]
      doWrite stmt (TxLog "SYS:PACTS" k' encoded:)

    DNamespaces -> liftIO $ withStmt db "INSERT OR REPLACE INTO \"SYS:NAMESPACES\" (txid, rowkey, rowdata) VALUES (?,?,?)" $ \stmt -> do
      let
        encoded = _encodeNamespace serial v
        NamespaceName k' = k
      TxId i <- readIORef txId
      SQL.bind stmt [SQL.SQLInteger (fromIntegral i), SQL.SQLText k', SQL.SQLBlob encoded]
      doWrite stmt  (TxLog "SYS:NAMESPACES" k' encoded:)
  where
    checkInsertOk ::  TableName -> RowKey -> IO (Maybe RowData)
    checkInsertOk tbl rk = do
      curr <- read' serial db (DUserTables tbl) rk
      case (curr, wt) of
        (Nothing, Insert) -> return Nothing
        (Just _, Insert) -> throwIO (E.RowFoundException tbl rk)
        (Nothing, Write) -> return Nothing
        (Just old, Write) -> return $ Just old
        (Just old, Update) -> return $ Just old
        (Nothing, Update) -> throwIO (E.NoRowFound tbl rk)

    doWrite stmt txlog = Direct.stepNoCB stmt >>= \case
          Left _ -> throwIO E.WriteException
          Right res
            | res == SQL.Done -> modifyIORef' txLog txlog
            | otherwise -> throwIO E.MultipleRowsReturnedFromSingleWrite

read' :: forall k v b i. PactSerialise b i -> SQL.Database -> Domain k v b i -> k -> IO (Maybe v)
read' serial db domain k = case domain of
  DKeySets -> withStmt db (selStmt "SYS:KEYSETS")
    (doRead (renderKeySetName k) (\v -> pure (view document <$> _decodeKeySet serial v)))

  DModules -> withStmt db (selStmt "SYS:MODULES")
    (doRead (renderModuleName k) (\v -> pure (view document <$> _decodeModuleData serial v)))

  DUserTables tbl ->  withStmt db (selStmt $ toUserTable tbl)
    (doRead (_rowKey k) (\v -> pure (view document <$> _decodeRowData serial v)))

  DDefPacts -> withStmt db (selStmt "SYS:PACTS")
    (doRead (renderDefPactId k) (\v -> pure (view document <$> _decodeDefPactExec serial v)))

  DNamespaces -> withStmt db (selStmt "SYS:NAMESPACES")
    (doRead (_namespaceName k) (\v -> pure (view document <$> _decodeNamespace serial v)))

  where
    selStmt tbl = "SELECT rowdata FROM \""<> tbl <> "\" WHERE rowkey = ? ORDER BY txid DESC LIMIT 1"
    doRead k' f stmt = do
      SQL.bind stmt [SQL.SQLText k']
      SQL.step stmt >>= \case
        SQL.Done -> pure Nothing
        SQL.Row -> do
          1 <- SQL.columnCount stmt
          [SQL.SQLBlob value] <- SQL.columns stmt
          SQL.Done <- SQL.step stmt
          f value


-- Utility functions
withStmt :: SQL.Database -> Text -> (SQL.Statement -> IO a) -> IO a
withStmt conn sql = bracket (SQL.prepare conn sql) SQL.finalize


