-- |

module Pact.Core.Persistence.SQLite (
  withSqlitePactDb
                                    ) where

import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Exception.Lifted (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
-- import qualified Data.Text as T
import Data.Text (Text)
import qualified Database.SQLite3 as SQL

import Pact.Core.Persistence (PactDb(..))
-- import Pact.Core.Repl.Utils (ReplEvalM)


withSqlitePactDb
  :: (MonadIO m, MonadBaseControl IO m)
  => Text
  -> (PactDb b i -> m a)
  -> m a
withSqlitePactDb connectionString act =
  bracket connect cleanup (\db -> liftIO (createPactDb db) >>= act)
  where
    connect = liftIO $ SQL.open connectionString
    cleanup db = liftIO $ SQL.close db



createPactDb :: SQL.Database  -> IO (PactDb b i)
createPactDb _db = do
  -- liftIO (createTables db)
  undefined
