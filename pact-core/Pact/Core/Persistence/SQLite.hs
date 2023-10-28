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
import Pact.Core.Persistence (PactDb(..), Domain(DKeySets, DModules, DUserTables, DPacts),
                              Purity(PImpure)
                             , FQKS
                             )
-- import Pact.Core.Repl.Utils (ReplEvalM)


withSqlitePactDb
  :: (MonadIO m, MonadBaseControl IO m)
  => Text
  -> (PactDb b i -> m a)
  -> m a
withSqlitePactDb connectionString act =
  bracket connect cleanup (\db -> liftIO (initializePactDb db) >>= act)
  where
    connect = liftIO $ SQL.open connectionString
    cleanup db = liftIO $ SQL.close db



-- | Create all tables that should exist in a fresh pact db,
--   or ensure that they are already created.
initializePactDb :: SQL.Database  -> IO (PactDb b i)
initializePactDb db = do
  -- liftIO (createTables db)
  pure $ PactDb
    { _pdbPurity = PImpure
    , _pdbRead = read' db
    , _pdbWrite = undefined
    , _pdbKeys = undefined
    , _pdbCreateUserTable = undefined
    , _pdbBeginTx = undefined
    , _pdbCommitTx = undefined
    , _pdbRollbackTx = undefined
    , _pdbTxIds = undefined
    , _pdbGetTxLog = undefined
    }

read' :: forall k v b i. SQL.Database -> Domain k v b i -> k -> IO (Maybe v)
read' db domain k = case domain of
  DKeySets -> withStmt db "SELECT rowdata FROM SYS_keysets ORDER BY txid DESCENDING WHERE rowkey = ? LIMIT 1" $ \stmt -> do
      SQL.bind stmt [SQL.SQLText (_keySetName k)]
      SQL.step stmt >>= \case
        SQL.Done -> pure Nothing
        SQL.Row -> do
          1 <- SQL.columnCount stmt
          [SQL.SQLBlob value] <- SQL.columns stmt
          SQL.Done <- SQL.step stmt
          pure @IO (Just (_ value))
  DModules -> readModules
  DUserTables tbl -> readRowData tbl
  DPacts -> readDefPacts
  where
    readModules = pure @IO Nothing
    readRowData tbl = pure Nothing
    readDefPacts = pure @IO Nothing

-- Utility functions
withStmt :: SQL.Database -> Text -> (SQL.Statement -> IO a) -> IO a
withStmt conn sql = bracket (SQL.prepare conn sql) SQL.finalize
