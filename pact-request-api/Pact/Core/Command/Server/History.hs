{-# LANGUAGE ImportQualifiedPost #-}
-- |

module Pact.Core.Command.Server.History
  ( HistoryDb(..)
  , withSqliteAndHistoryDb
  , unsafeCreateHistoryDb
  , unsafeCloseHistoryDb
  )
where

import Data.Text qualified as T
import Control.Exception.Safe
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Database.SQLite3 as SQL
import qualified Database.SQLite3.Direct as Direct

import Pact.Core.Command.Types
import Pact.Core.Hash
import Pact.Core.Errors
import Pact.Core.Evaluate
import Pact.Core.Builtin
import Pact.Core.Persistence

import qualified Pact.JSON.Encode as J
import qualified Pact.JSON.Decode as J

import Pact.Core.Persistence.SQLite
import Pact.Core.Serialise

type Cmd = CommandResult Hash (PactOnChainError)

data HistoryDb
  = HistoryDb
  { _histDbInsert :: RequestKey -> Cmd -> IO (Either SomeException ())
  , _histDbRead   :: RequestKey -> IO (Maybe Cmd)
  }

withSqliteAndHistoryDb
  :: (MonadMask m, MonadIO m)
  => T.Text
  -> (PactDb CoreBuiltin Info -> HistoryDb -> m a)
  -> m a
withSqliteAndHistoryDb path act =
  bracket open close f
  where
  f (pdb, hdb, _, _) = act pdb hdb
  close (_, _, db, stmt) =
    liftIO $ unsafeCloseSqlitePactDb db stmt
  open = do
    (pdb, db, stmt) <- unsafeCreateSqlitePactDb serialisePact_lineinfo_pact51 path
    liftIO $ SQL.exec db createHistoryTblStmt
    pure (pdb, dbToHistDb db, db, stmt)

unsafeCreateHistoryDb :: T.Text -> IO (HistoryDb, Direct.Database)
unsafeCreateHistoryDb  conStr = do
  db <- SQL.open conStr
  SQL.exec db createHistoryTblStmt
  pure $ (dbToHistDb db, db)

unsafeCloseHistoryDb :: Direct.Database -> IO ()
unsafeCloseHistoryDb = SQL.close

dbToHistDb :: Direct.Database -> HistoryDb
dbToHistDb db = HistoryDb
  { _histDbInsert = \(RequestKey h) cmd -> try $! SQL.withStatement db "INSERT INTO \"SYS:PactServiceHistory\" (reqkey, cmddata) values (?,?)" $ \stmt -> do
      SQL.bind stmt [ SQL.SQLText $ hashToText h
                    , SQL.SQLBlob $ J.encodeStrict cmd]
      SQL.stepNoCB stmt >> pure ()
  , _histDbRead = \(RequestKey h) -> SQL.withStatement db "SELECT cmddata from \"SYS:PactServiceHistory\" where reqkey = ? LIMIT 1" $ \stmt -> do
      SQL.bind stmt [SQL.SQLText $ hashToText h]
      SQL.stepNoCB stmt >>= \case
        SQL.Done -> pure Nothing
        SQL.Row -> do
          [SQL.SQLBlob value] <- SQL.columns stmt
          pure $ J.decodeStrict value
  }


createHistoryTblStmt :: T.Text
createHistoryTblStmt
  = "CREATE TABLE IF NOT EXISTS \"SYS:PactServiceHistory\" \
    \ (reqkey TEXT PRIMARY KEY, \
    \  cmddata BLOB)"
