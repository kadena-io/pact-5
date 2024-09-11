{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ExistentialQuantification #-}

module Pact.Core.Test.LegacyDBRegression
  ( tests )
  where

import Control.Exception.Safe
import Control.Lens
import Control.Applicative
import Control.Monad
import Data.Default
import Data.Text(Text)
import Test.Tasty
import Test.Tasty.HUnit
import System.FilePath
import System.Directory
import qualified Database.SQLite3 as SQL
import qualified Network.HTTP.Simple as Http
import qualified Data.ByteString as B
import qualified Data.Text as T

import Pact.Core.Persistence
import Pact.Core.Builtin
import Pact.Core.Info
import Pact.Core.Names
import Pact.Core.Persistence.SQLite
import Pact.Core.Serialise

import qualified Data.Char as Char
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import Data.IORef


dbFolder :: FilePath
dbFolder = "pact-tests" </> "legacy-db-regression"

dbFile :: FilePath
dbFile = "pact-v1-chain-9.sqlite"

dbFilePath :: FilePath
dbFilePath = dbFolder </> dbFile

data SomeDomain
  = forall k v. Show k => SomeDomain (Domain k v CoreBuiltin SpanInfo)

-- Copy pasted from Pact.Core.Names
-- exporting this causes a compliation error in Pact.Core.Principals
identParser :: Parser Text
identParser = do
  c1 <- MP.letterChar <|> MP.oneOf specials
  rest <- MP.takeWhileP Nothing (\c -> Char.isLetter c || Char.isDigit c || elem c specials)
  pure (T.cons c1 rest)
  where
  specials :: String
  specials = "%#+-_&$@<>=^?*!|/~"

-- Copy pasted from Pact.Core.Names
-- exporting this causes a compliation error in Pact.Core.Principals
moduleNameParser :: Parser ModuleName
moduleNameParser = do
  p <- identParser
  MP.try (go p <|> pure (ModuleName p Nothing))
  where
  go ns = do
    _ <- MP.char '.'
    p1 <- identParser
    pure (ModuleName p1 (Just (NamespaceName ns)))

type Parser = MP.Parsec () Text

-- | Hacky way of parsing a user table
parseUserTable :: Text -> Maybe TableName
parseUserTable s =
  case reverse (T.splitOn "_" s) of
    identRaw:tbl ->
      let tbl' = T.intercalate "_" (reverse tbl)
      in case (,) <$> MP.parseMaybe moduleNameParser tbl' <*> MP.parseMaybe identParser identRaw of
        Just (mn, ident) -> Just (TableName ident mn)
        _ -> Nothing
    _ -> Nothing

-- Note: It's an IO PactDb because `withResource` from tasty has a really
-- annoying signature
runTableDecodeRegression :: HasCallStack => IO (PactDb CoreBuiltin SpanInfo) -> SomeDomain -> TestTree
runTableDecodeRegression pdbIO (SomeDomain domain) = testCase testName $ do
  pdb <- pdbIO
  keys <- ignoreGas def $ _pdbKeys pdb domain
  forM_ keys $ \k -> do
    v <- ignoreGas def $ _pdbRead pdb domain k
    let msg = "Decode failed for table " <> T.unpack (renderDomain domain) <> " at key " <> show k
    assertBool msg $ has _Just v
  where
  testName = "Running regression for table: " <> T.unpack (renderDomain domain)

data DBHarness
  = DBHarness
  { _dbhDB :: SQL.Database
  , _dbhPactDb :: PactDb CoreBuiltin SpanInfo
  , _dbhStmtCache :: IORef StmtCache
  }

withStmt :: SQL.Database -> Text -> (SQL.Statement -> IO a) -> IO a
withStmt conn sql = bracket (SQL.prepare conn sql) SQL.finalize

withDb :: (SQL.Database -> IO c) -> IO c
withDb act =
  bracket (unsafeCreateSqlitePactDb serialisePact_raw_spaninfo (T.pack dbFilePath))
    (\(_, db, c) -> unsafeCloseSqlitePactDb db c)
    (\(_, db, _) -> act db)

tests :: IO TestTree
tests = do
  downloadRegressionDb
  userTables <- withDb getUserTables
  let allTables =
        [ SomeDomain DKeySets
        , SomeDomain DDefPacts
        , SomeDomain DModules
        , SomeDomain DNamespaces
        ] ++ [SomeDomain (DUserTables t) | u <- userTables, Just t <- [parseUserTable u]]
  pure $ withResource acquireDbHarness releaseDbHarness $ \pdbio ->
      testGroup "Legacy PactDb Regression" $
        runTableDecodeRegression (_dbhPactDb <$> pdbio) <$> allTables
  where
  acquireDbHarness :: IO DBHarness
  acquireDbHarness = do
    (pdb, db, cache) <- unsafeCreateSqlitePactDb serialisePact_raw_spaninfo (T.pack dbFilePath)
    pure (DBHarness db pdb cache )

  releaseDbHarness :: DBHarness -> IO ()
  releaseDbHarness harness =
    unsafeCloseSqlitePactDb (_dbhDB harness) (_dbhStmtCache harness)


getUserTables :: SQL.Database -> IO [Text]
getUserTables con = do
  withStmt con qry (go [])
  where
  qry = "select name from sqlite_master where type='table'"
  go acc stmt = SQL.step stmt >>= \case
    SQL.Done -> pure acc
    SQL.Row -> do
      [SQL.SQLText tbl] <- SQL.columns stmt
      go (tbl: acc) stmt


-- Function to download a file as a ByteString and save it to a file
downloadFile :: String -> FilePath -> IO ()
downloadFile url destination = do
    let request = Http.parseRequest_ url
    response <- Http.httpBS request
    let body = Http.getResponseBody response  -- Get the response as a ByteString
    B.writeFile destination body         -- Write the ByteString to a file

downloadRegressionDb :: IO ()
downloadRegressionDb = do
  fileExists <- doesFileExist dbFilePath
  unless fileExists $ do
    createDirectoryIfMissing True dbFolder
    downloadFile "https://chainweb-chain-db.s3.amazonaws.com/test-objects/pact-v1-chain-9.sqlite" dbFilePath


