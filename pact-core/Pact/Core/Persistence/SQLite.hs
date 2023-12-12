-- |
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Pact.Core.Persistence.SQLite (
  withSqlitePactDb
                                    ) where

import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Exception.Lifted (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (newIORef, IORef, readIORef, atomicModifyIORef', writeIORef, modifyIORef')
import Data.Text (Text)
import Control.Lens (view)
import qualified Database.SQLite3 as SQL
import qualified Database.SQLite3.Direct as Direct
import Data.ByteString (ByteString)

import Pact.Core.Guards (renderKeySetName, parseAnyKeysetName)
import Pact.Core.Names (renderModuleName, DefPactId(..), NamespaceName(..), TableName(..), RowKey(..), parseRenderedModuleName)
import Pact.Core.Persistence (PactDb(..), Domain(..),
                              Purity(PImpure)
                             ,WriteType(..)
                             ,toUserTable
                             ,ExecutionMode(..), TxId(..)
                             , RowData, TxLog(..)
                             )
import qualified Pact.Core.Persistence as P
import Control.Exception (throwIO)
-- import Pact.Core.Repl.Utils (ReplEvalM)
import Pact.Core.Serialise
withSqlitePactDb
  :: (MonadIO m, MonadBaseControl IO m)
  => PactSerialise b i
  -> Text
  -> (PactDb b i -> m a)
  -> m a
withSqlitePactDb serial connectionString act =
  bracket connect cleanup (\db -> liftIO (initializePactDb serial db) >>= act)
  where
    connect = liftIO $ SQL.open connectionString
    cleanup db = liftIO $ SQL.close db

createSysTables :: SQL.Database -> IO ()
createSysTables db = do
  SQL.exec db "CREATE TABLE IF NOT EXISTS \"SYS:KEYSETS\"    (txid UNSIGNED BIG INT, rowkey TEXT, rowdata BLOB, UNIQUE (txid, rowkey))"
  SQL.exec db "CREATE TABLE IF NOT EXISTS \"SYS:MODULES\"    (txid UNSIGNED BIG INT, rowkey TEXT, rowdata BLOB, UNIQUE (txid, rowkey))"
  SQL.exec db "CREATE TABLE IF NOT EXISTS \"SYS:PACTS\"      (txid UNSIGNED BIG INT, rowkey TEXT, rowdata BLOB, UNIQUE (txid, rowkey))"
  SQL.exec db "CREATE TABLE IF NOT EXISTS \"SYS:NAMESPACES\" (txid UNSIGNED BIG INT, rowkey TEXT, rowdata BLOB, UNIQUE (txid, rowkey))"

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
    , _pdbCreateUserTable = createUserTable db txLog
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
      userTabLogs = filter (\tl -> toUserTable tab == _txDomain tl) txLog'
      env :: Maybe [TxLog RowData] = traverse (traverse (fmap (view document) . _decodeRowData serial)) userTabLogs
    case env of
      Nothing -> fail "undexpected decoding error"
      Just xs -> pure $ reverse xs
    else withStmt db ("SELECT rowkey,rowdata FROM \"" <> toUserTable tab <> "\" WHERE txid = ?") $ \stmt -> do
                         let TxId i = txId
                         SQL.bind stmt [SQL.SQLInteger $ fromIntegral i]
                         txLogBS <- collect stmt []
                         case traverse (traverse (fmap (view document) . _decodeRowData serial)) txLogBS of
                           Nothing -> fail "unexpected decoding error"
                           Just txl -> pure $ reverse txl
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
  DModules -> withStmt db "SELECT rowkey FROM \"SYS:MODULES\" ORDER BY txid DESC" $ \stmt -> fmap parseRenderedModuleName <$> collect stmt [] >>= \mns -> case sequence mns of
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

createUserTable :: SQL.Database -> IORef [TxLog ByteString] -> TableName -> IO ()
createUserTable db _txLog tbl = SQL.exec db ("CREATE TABLE IF NOT EXISTS " <> tblName <> " (txid UNSIGNED BIG INT, rowkey TEXT, rowdata BLOB, UNIQUE (txid, rowkey))")
  where
    tblName = "\"" <> toUserTable tbl <> "\""

write' :: forall k v b i. PactSerialise b i -> SQL.Database -> IORef TxId -> IORef [TxLog ByteString] -> WriteType -> Domain k v b i -> k -> v -> IO ()
write' serial db txId txLog _wt domain k v = case domain of
  DKeySets -> withStmt db "INSERT INTO \"SYS:kEYSETS\" (txid, rowkey, rowdata) VALUES (?,?,?)" $ \stmt -> do
      let encoded = _encodeKeySet serial v
      TxId i <- readIORef txId
      SQL.bind stmt [SQL.SQLInteger (fromIntegral i), SQL.SQLText (renderKeySetName k), SQL.SQLBlob encoded]
      SQL.stepNoCB stmt >>= \case
        SQL.Done -> modifyIORef' txLog (TxLog "SYS:KEYSETS" (renderKeySetName k) encoded:)
        SQL.Row -> fail "invariant violation"
  DModules -> withStmt db "INSERT INTO \"SYS:MODULES\" (txid, rowkey, rowdata) VALUES (?,?,?)" $ \stmt -> do
      let encoded = _encodeModuleData serial v
      TxId i <- readIORef txId
      SQL.bind stmt [SQL.SQLInteger (fromIntegral i), SQL.SQLText (renderModuleName k), SQL.SQLBlob encoded]
      Direct.stepNoCB stmt >>= \case
        Left _err -> throwIO P.WriteException
        Right res
          | res == SQL.Done -> modifyIORef' txLog (TxLog "SYS:Modules" (renderModuleName k) encoded:)
          | otherwise -> fail "invariant violation"
  DDefPacts -> withStmt db "INSERT INTO \"SYS:PACTS\" (txid, rowkey, rowdata) VALUES (?,?,?)" $ \stmt -> do
      let
        encoded = _encodeDefPactExec serial v
        DefPactId k' = k
      TxId i <- readIORef txId
      SQL.bind stmt [SQL.SQLInteger (fromIntegral i), SQL.SQLText k', SQL.SQLBlob encoded]
      SQL.stepNoCB stmt >>= \case
        SQL.Done -> modifyIORef' txLog (TxLog "SYS:DEFPACTS" k' encoded:)
        SQL.Row -> fail "invariant violation"
  DNamespaces -> withStmt db "INSERT INTO \"SYS:NAMESPACES\" (txid, rowkey, rowdata) VALUES (?,?,?)" $ \stmt -> do
      let
        encoded = _encodeNamespace serial v
        NamespaceName k' = k
      TxId i <- readIORef txId
      putStrLn ("DNamespaces: " <> show i <> " / " <> show k')
      SQL.bind stmt [SQL.SQLInteger (fromIntegral i), SQL.SQLText k', SQL.SQLBlob encoded]
      Direct.stepNoCB stmt >>= \case
        Left _err -> undefined
        Right res
          | res == SQL.Done -> modifyIORef' txLog (TxLog "SYS:NAMESPACES" k' encoded:)
          | otherwise -> fail "invariant viaolation"
  DUserTables tbl -> withStmt db ("INSERT INTO \"" <> toUserTable tbl <> "\" (txid, rowkey, rowdata) VALUES (?,?,?)") $ \stmt -> do
    let
      encoded = _encodeRowData serial v
      RowKey k' = k
    TxId i <- readIORef txId
    SQL.bind stmt [SQL.SQLInteger (fromIntegral i), SQL.SQLText k', SQL.SQLBlob encoded]
    SQL.stepNoCB stmt >>= \case
        SQL.Done -> modifyIORef' txLog (TxLog (toUserTable tbl) k' encoded:)
        SQL.Row -> fail "invariant viaolation"
  where
    insertWt :: IO ()
    insertWt = undefined
    updateWt :: ()
    updateWt = undefined
    writeWt ::  ()
    writeWt = undefined

read' :: forall k v b i. PactSerialise b i -> SQL.Database -> Domain k v b i -> k -> IO (Maybe v)
read' serial db domain k = case domain of
  DKeySets -> withStmt db "SELECT rowdata FROM \"SYS:KEYSETS\" WHERE rowkey = ? ORDER BY txid DESC LIMIT 1" $ \stmt -> do
      SQL.bind stmt [SQL.SQLText (renderKeySetName k)]
      SQL.step stmt >>= \case
        SQL.Done -> pure Nothing
        SQL.Row -> do
          1 <- SQL.columnCount stmt
          [SQL.SQLBlob value] <- SQL.columns stmt
          SQL.Done <- SQL.step stmt
          pure (view document <$> _decodeKeySet serial value)
  DModules ->  withStmt db "SELECT rowdata FROM \"SYS:MODULES\" WHERE rowkey = ? ORDER BY txid DESC LIMIT 1" $ \stmt -> do
      SQL.bind stmt [SQL.SQLText (renderModuleName k)]
      SQL.step stmt >>= \case
        SQL.Done -> pure Nothing
        SQL.Row -> do
          1 <- SQL.columnCount stmt
          [SQL.SQLBlob value] <- SQL.columns stmt
          SQL.Done <- SQL.step stmt
          pure (view document <$> _decodeModuleData serial value)
  DUserTables tbl -> do
    let tblName = "\"" <> toUserTable tbl <> "\""
    withStmt db ("SELECT rowdata FROM " <> tblName <> " WHERE rowkey = ? ORDER BY txid DESC LIMIT 1") $ \stmt -> do
      let RowKey rk = k
      SQL.bind stmt [SQL.SQLText rk]
      SQL.step stmt >>= \case
        SQL.Done -> pure Nothing
        SQL.Row -> do
          1 <- SQL.columnCount stmt
          [SQL.SQLBlob value] <- SQL.columns stmt
          SQL.Done <- SQL.step stmt
          pure (view document <$>  _decodeRowData serial value)
  DDefPacts -> withStmt db "SELECT rowdata FROM \"SYS:PACTS\" WHERE rowkey = ? ORDER BY txid DESC LIMIT 1" $ \stmt -> do
      let DefPactId pid = k
      SQL.bind stmt [SQL.SQLText pid]
      SQL.step stmt >>= \case
        SQL.Done -> pure Nothing
        SQL.Row -> do
          1 <- SQL.columnCount stmt
          [SQL.SQLBlob value] <- SQL.columns stmt
          SQL.Done <- SQL.step stmt
          pure (view document <$> _decodeDefPactExec serial value)
  DNamespaces -> withStmt db "SELECT rowdata FROM \"SYS:NAMESPACES\" WHERE rowkey = ? ORDER BY txid DESC LIMIT 1" $ \stmt -> do
      let NamespaceName ns = k
      SQL.bind stmt [SQL.SQLText ns]
      SQL.step stmt >>= \case
        SQL.Done -> pure Nothing
        SQL.Row -> do
          1 <- SQL.columnCount stmt
          [SQL.SQLBlob value] <- SQL.columns stmt
          SQL.Done <- SQL.step stmt
          pure (view document <$> _decodeNamespace serial value)

-- Utility functions
withStmt :: SQL.Database -> Text -> (SQL.Statement -> IO a) -> IO a
withStmt conn sql = bracket (SQL.prepare conn sql) SQL.finalize


