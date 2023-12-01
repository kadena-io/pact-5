-- |
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Pact.Core.Persistence.SQLite (
  withSqlitePactDb
                                    ) where

import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Exception.Lifted (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
-- import qualified Data.Text as T
import Data.IORef (newIORef, IORef, readIORef, atomicModifyIORef')
import Data.Text (Text)
import Control.Lens (view)
import qualified Database.SQLite3 as SQL

import Pact.Core.Guards (renderKeySetName, KeySetName(..))
import Pact.Core.Names (renderModuleName, DefPactId(..), NamespaceName(..), TableName(..), RowKey(..), parseRenderedModuleName)
import Pact.Core.Persistence (PactDb(..), Domain(..),
                              Purity(PImpure)
                             ,WriteType(..) --, RowData(..)
                             ,toUserTable
                             ,ExecutionMode(..), TxId(..)
                             )

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
  SQL.exec db "CREATE TABLE IF NOT EXISTS \"SYS:KEYSETS\"    (txid INTEGER PRIMARY KEY NOT NULL UNIQUE , rowkey TEXT NOT NULL, rowdata BLOB NOT NULL)"
  SQL.exec db "CREATE TABLE IF NOT EXISTS \"SYS:MODULES\"    (txid INTEGER PRIMARY KEY NOT NULL UNIQUE , rowkey TEXT NOT NULL, rowdata BLOB NOT NULL)"
  SQL.exec db "CREATE TABLE IF NOT EXISTS \"SYS:PACTS\"      (txid INTEGER PRIMARY KEY NOT NULL UNIQUE , rowkey TEXT NOT NULL, rowdata BLOB NOT NULL)"
  SQL.exec db "CREATE TABLE IF NOT EXISTS \"SYS:NAMESPACES\" (txid INTEGER PRIMARY KEY NOT NULL UNIQUE , rowkey TEXT NOT NULL, rowdata BLOB NOT NULL)"

-- | Create all tables that should exist in a fresh pact db,
--   or ensure that they are already created.
initializePactDb :: PactSerialise b i -> SQL.Database  -> IO (PactDb b i)
initializePactDb serial db = do
  createSysTables db
  txId <- newIORef (TxId 0)
  pure $ PactDb
    { _pdbPurity = PImpure
    , _pdbRead = read' serial db
    , _pdbWrite = write' serial db
    , _pdbKeys = readKeys db
    , _pdbCreateUserTable = createUserTable db
    , _pdbBeginTx = beginTx txId db
    , _pdbCommitTx = commitTx txId db
    , _pdbRollbackTx = rollbackTx db
    , _pdbTxIds = error "no txids"
    , _pdbGetTxLog = error "no txlog"
    , _pdbTxId = txId
    }

readKeys :: forall k v b i. SQL.Database -> Domain k v b i -> IO [k]
readKeys db = \case
  DKeySets -> withStmt db "SELECT rowkey FROM \"SYS:KEYSETS\" ORDER BY txid DESC" $ \stmt -> fmap KeySetName <$> collect stmt []
  DModules -> withStmt db "SELECT rowkey FROM \"SYS:MODULES\" ORDER BY txid DESC" $ \stmt -> fmap parseRenderedModuleName <$> collect stmt [] >>= \mns -> case sequence mns of
    Nothing -> error ""
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
      

commitTx :: IORef TxId -> SQL.Database -> IO ()
commitTx txid db = do
  _ <- atomicModifyIORef' txid (\old@(TxId n) -> (TxId (succ n), old))
  SQL.exec db "COMMIT TRANSACTION"

beginTx :: IORef TxId -> SQL.Database -> ExecutionMode -> IO (Maybe TxId)
beginTx txid db em = do
    SQL.exec db "BEGIN TRANSACTION"
    case em of
      Transactional -> Just <$> readIORef txid
      Local -> pure Nothing

rollbackTx :: SQL.Database -> IO ()
rollbackTx db = SQL.exec db "ROLLBACK TRANSACTION"

createUserTable :: SQL.Database -> TableName -> IO ()
createUserTable db tbl = SQL.exec db ("CREATE TABLE IF NOT EXISTS " <> tblName <> " (txid INTEGER PRIMARY KEY NOT NULL UNIQUE, rowkey TEXT NOT NULL, rowdata BLOB NOT NULL)")
  where
    tblName = "\"" <> toUserTable tbl <> "\""

write' :: forall k v b i. PactSerialise b i -> SQL.Database -> WriteType -> Domain k v b i -> k -> v -> IO ()
write' serial db _wt domain k v = case domain of
  DKeySets -> withStmt db "INSERT INTO \"SYS:kEYSETS\" (rowkey, rowdata) VALUES (?,?)" $ \stmt -> do
      let encoded = _encodeKeySet serial v
      SQL.bind stmt [SQL.SQLText (renderKeySetName k), SQL.SQLBlob encoded]
      SQL.stepNoCB stmt >>= \case
        SQL.Done -> pure ()
        SQL.Row -> fail "invariant violation"
  DModules -> withStmt db "INSERT INTO \"SYS:MODULES\" (rowkey, rowdata) VALUES (?,?)" $ \stmt -> do
      let encoded = _encodeModuleData serial v
      SQL.bind stmt [SQL.SQLText (renderModuleName k), SQL.SQLBlob encoded]
      SQL.stepNoCB stmt >>= \case
        SQL.Done -> pure ()
        SQL.Row -> fail "invariant violation"
  DDefPacts -> withStmt db "INSERT INTO \"SYS:PACTS\" (rowkey, rowdata) VALUES (?,?)" $ \stmt -> do
      let
        encoded = _encodeDefPactExec serial v
        DefPactId k' = k
      SQL.bind stmt [SQL.SQLText k', SQL.SQLBlob encoded]
      SQL.stepNoCB stmt >>= \case
        SQL.Done -> pure ()
        SQL.Row -> fail "invariant violation"
  DNamespaces -> withStmt db "INSERT INTO \"SYS:NAMESPACES\" (rowkey, rowdata) VALUES (?,?)" $ \stmt -> do
      let
        encoded = _encodeNamespace serial v
        NamespaceName k' = k
      SQL.bind stmt [SQL.SQLText k', SQL.SQLBlob encoded]
      SQL.stepNoCB stmt >>= \case
        SQL.Done -> pure ()
        SQL.Row -> fail "invariant viaolation"
  DUserTables tbl -> withStmt db ("INSERT INTO \"" <> toUserTable tbl <> "\" (rowkey, rowdata) VALUES (?,?)") $ \stmt -> do
    let
      encoded = _encodeRowData serial v
      RowKey k' = k
    SQL.bind stmt [SQL.SQLText k', SQL.SQLBlob encoded]
    SQL.stepNoCB stmt >>= \case
        SQL.Done -> pure ()
        SQL.Row -> fail "invariant viaolation"

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


