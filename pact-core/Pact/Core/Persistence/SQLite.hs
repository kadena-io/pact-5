-- |
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Pact.Core.Persistence.SQLite (
  withSqlitePactDb
                                    ) where

import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Exception.Lifted (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
-- import qualified Data.Text as T
import Data.Text (Text)
import qualified Database.SQLite3 as SQL

import Pact.Core.Guards (KeySetName(_keySetName))
import Pact.Core.Names (renderModuleName)
import Pact.Core.Persistence (PactDb(..), Domain(..),
                              Purity(PImpure)
                             ,WriteType(..)
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

createTables :: SQL.Database -> IO ()
createTables db = do
  SQL.exec db "CREATE TABLE IF NOT EXISTS SYS_KEYSETS (txid INTEGER PRIMARY KEY NOT NULL UNIQUE , rowkey TEXT NOT NULL, rowdata BLOB NOT NULL)"
  SQL.exec db "CREATE TABLE IF NOT EXISTS SYS_MODULES (txid INTEGER PRIMARY KEY NOT NULL UNIQUE , rowkey TEXT NOT NULL, rowdata BLOB NOT NULL)"

-- | Create all tables that should exist in a fresh pact db,
--   or ensure that they are already created.
initializePactDb :: PactSerialise b i -> SQL.Database  -> IO (PactDb b i)
initializePactDb serial db = do
  createTables db
  pure $ PactDb
    { _pdbPurity = PImpure
    , _pdbRead = read' serial db
    , _pdbWrite = write' serial db
    , _pdbKeys = undefined
    , _pdbCreateUserTable = undefined
    , _pdbBeginTx = undefined
    , _pdbCommitTx = undefined
    , _pdbRollbackTx = undefined
    , _pdbTxIds = undefined
    , _pdbGetTxLog = undefined
    }

write' :: forall k v b i. PactSerialise b i -> SQL.Database -> WriteType -> Domain k v b i -> k -> v -> IO ()
write' serial db _wt domain k v = case domain of
  DKeySets -> withStmt db "INSERT INTO SYS_kEYSETS (rowkey, rowdata) VALUES (?,?)" $ \stmt -> do
      let encoded = _encodeKeySet serial v
      SQL.bind stmt [SQL.SQLText (_keySetName k), SQL.SQLBlob encoded]
      SQL.stepNoCB stmt >>= \case
        SQL.Done -> pure ()
        SQL.Row -> fail "invariant viaolation"
  DModules -> withStmt db "INSERT INTO SYS_MODULES (rowkey, rowdata) VALUES (?,?)" $ \stmt -> do
      let encoded = _encodeModuleData serial v
      SQL.bind stmt [SQL.SQLText (renderModuleName k), SQL.SQLBlob encoded]
      SQL.stepNoCB stmt >>= \case
        SQL.Done -> pure ()
        SQL.Row -> fail "invariant viaolation"
  _ -> undefined

read' :: forall k v b i. PactSerialise b i -> SQL.Database -> Domain k v b i -> k -> IO (Maybe v)
read' serial db domain k = case domain of
  DKeySets -> withStmt db "SELECT rowdata FROM SYS_KEYSETS WHERE rowkey = ? ORDER BY txid DESC LIMIT 1" $ \stmt -> do
      SQL.bind stmt [SQL.SQLText (_keySetName k)]
      SQL.step stmt >>= \case
        SQL.Done -> pure Nothing
        SQL.Row -> do
          1 <- SQL.columnCount stmt
          [SQL.SQLBlob value] <- SQL.columns stmt
          SQL.Done <- SQL.step stmt
          case _decodeKeySet serial value of
            Left _ -> pure Nothing
            Right (Document _ _ c) -> pure (Just c)
  DModules ->  withStmt db "SELECT rowdata FROM SYS_MODULES WHERE rowkey = ? ORDER BY txid DESC LIMIT 1" $ \stmt -> do
      SQL.bind stmt [SQL.SQLText (renderModuleName k)]
      SQL.step stmt >>= \case
        SQL.Done -> pure Nothing
        SQL.Row -> do
          1 <- SQL.columnCount stmt
          [SQL.SQLBlob value] <- SQL.columns stmt
          SQL.Done <- SQL.step stmt
          case _decodeModuleData serial value of
            Left _ -> pure Nothing
            Right (Document _ _ c) -> pure (Just c)
  DUserTables _tbl -> pure Nothing
  DDefPacts -> pure Nothing
  DNamespaces -> pure Nothing

-- Utility functions
withStmt :: SQL.Database -> Text -> (SQL.Statement -> IO a) -> IO a
withStmt conn sql = bracket (SQL.prepare conn sql) SQL.finalize
