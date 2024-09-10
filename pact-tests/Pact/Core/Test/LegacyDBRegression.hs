{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ExistentialQuantification #-}

module Pact.Core.Test.LegacyDBRegression
  ( tests
  , downloadRegressionDb )
  where

import Control.Lens
import Control.Applicative
import Control.Monad
import Data.Default
import Data.Text(Text)
import Test.Tasty
import Test.Tasty.HUnit
import System.FilePath
import System.Directory
import qualified Network.HTTP.Simple as Http
import qualified Data.ByteString as B
import qualified Data.Text as T

import Pact.Core.Persistence
import Pact.Core.Builtin
import Pact.Core.Info
import Pact.Core.Names
import Pact.Core.Persistence.SQLite
import Pact.Core.Serialise
import Data.String (IsString(..))

import qualified Data.Char as Char
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP


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

newtype UnsafeTableName
  = UnsafeTableName { _getUnsafeTable ::  TableName }
  deriving (Eq, Show)

-- | Hacky way of being able to provide a table name to our regression
instance IsString UnsafeTableName where
  fromString s =
    case reverse (T.splitOn "_" (T.pack s)) of
      identRaw:tbl ->
        let tbl' = T.intercalate "_" (reverse tbl)
        in case (,) <$> MP.parseMaybe moduleNameParser tbl' <*> MP.parseMaybe identParser identRaw of
          Just (mn, ident) -> UnsafeTableName (TableName ident mn)
          _ -> error "BOOM2"
      _ -> error "BOOM"


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

allTables :: [SomeDomain]
allTables =
  [ SomeDomain DKeySets
  , SomeDomain DDefPacts
  , SomeDomain DModules
  , SomeDomain DNamespaces
  ] ++ (SomeDomain <$> userTables)
  where
  userTables =
    DUserTables . _getUnsafeTable <$>
    [ "arkade.token_token-table"
    , "coin_allocation-table"
    , "coin_coin-table"
    , "free.KDAG_cumulative-kda-table"
    , "free.KDAG_last-id-table"
    , "free.KDAG_lock-kda-table"
    , "free.KDAG_multiplier-kda-table"
    , "free.KDAG_supply-table"
    , "free.KDAG_token-table"
    , "free.KDG_cumulative-kda-table"
    , "free.KDG_last-id-table"
    , "free.KDG_lock-kda-table"
    , "free.KDG_multiplier-kda-table"
    , "free.KDG_supply-table"
    , "free.KDG_token-table"
    , "free.KGOLD_cumulative-kda-table"
    , "free.KGOLD_last-id-table"
    , "free.KGOLD_lock-kda-table"
    , "free.KGOLD_multiplier-kda-table"
    , "free.KGOLD_supply-table"
    , "free.KGOLD_token-table"
    , "free.SHIB_token-table"
    , "free.anedak_token-table"
    , "free.babena_cumulative-babe-table"
    , "free.babena_cumulative-kda-table"
    , "free.babena_emergency-babe-table"
    , "free.babena_emergency-kda-table"
    , "free.babena_last-id-table"
    , "free.babena_lock-babe-table"
    , "free.babena_lock-kda-table"
    , "free.babena_multiplier-babe-table"
    , "free.babena_multiplier-kda-table"
    , "free.babena_supply-table"
    , "free.babena_token-table"
    , "free.backalley-token_allocation-table"
    , "free.backalley-token_token-table"
    , "free.backalley_allocation-table"
    , "free.backalley_token-table"
    , "free.bana_token-table"
    , "free.corona-inu_token-table"
    , "free.corona-token_token-table"
    , "free.crankk01_crankk01-token-table"
    , "free.dbc-token_token-table"
    , "free.docu_token-table"
    , "free.elon_token-table"
    , "free.fin-us_token-initialization-table"
    , "free.fin-us_token-table"
    , "free.hyperhub_token-table"
    , "free.inu-crew_counts"
    , "free.inu-crew_mint"
    , "free.inu-crew_nfts"
    , "free.inu-crew_price"
    , "free.inu-crew_values"
    , "free.jodie-inu_token-table"
    , "free.jodie-token_token-table"
    , "free.kadoge_token-table"
    , "free.kapepe-coin_token-table"
    , "free.kapybara-token_token-table"
    , "free.kimki_token-table"
    , "free.kishu-ken_token-table"
    , "free.kmp_token-table"
    , "free.kpepe_token-table"
    , "free.memory-wall_memories"
    , "free.phiga-inu_token-table"
    , "free.quality-ledger_lots-table"
    , "free.quality-ledger_products-table"
    , "free.real-kdoge_token-table"
    , "free.shatter_token-table"
    , "free.sway_token-table"
    , "free.timpi_token-table"
    , "free.util-random_state-table"
    , "free.wiza_base-multiplier-table"
    , "free.wiza_mined-wiza-table"
    , "free.wiza_staked-table"
    , "free.wiza_token-table"
    , "free.yeettoken_token-table"
    , "hypercent.prod-hype-coin_ledger"
    , "kaddex.kdx_contract-lock"
    , "kaddex.kdx_mint-cap-table"
    , "kaddex.kdx_privileges"
    , "kaddex.kdx_special-accounts"
    , "kaddex.kdx_supply-table"
    , "kaddex.kdx_token-table"
    , "kdlaunch.kdswap-token_token-table"
    , "kdlaunch.token_token-table"
    , "lago.USD2_token-table"
    , "lago.kwBTC_token-table"
    , "lago.kwUSDC_token-table"
    , "mok.token_token-table"
    , "n_5a7ccd559b245b7dcbd5259e1ee43d04fbf93eab.kapepe_token-table"
    , "n_7763cd0330f59f3c66e431dcd63a2c5c5e2e0b70.bubblegum_endtime-table"
    , "n_7763cd0330f59f3c66e431dcd63a2c5c5e2e0b70.bubblegum_ledger"
    , "n_7763cd0330f59f3c66e431dcd63a2c5c5e2e0b70.bubblegum_marketplace"
    , "n_7763cd0330f59f3c66e431dcd63a2c5c5e2e0b70.bubblegum_metadata-table"
    , "n_7763cd0330f59f3c66e431dcd63a2c5c5e2e0b70.bubblegum_mint-table"
    , "n_7763cd0330f59f3c66e431dcd63a2c5c5e2e0b70.bubblegum_nft-chains-table"
    , "n_7763cd0330f59f3c66e431dcd63a2c5c5e2e0b70.bubblegum_supplies"
    , "n_7763cd0330f59f3c66e431dcd63a2c5c5e2e0b70.bubblegum_vault-count-table"
    , "n_7763cd0330f59f3c66e431dcd63a2c5c5e2e0b70.bubblegum_vault-table"
    , "n_7763cd0330f59f3c66e431dcd63a2c5c5e2e0b70.bubblegum_whitelist-table"
    , "n_7763cd0330f59f3c66e431dcd63a2c5c5e2e0b70.dao-hive-factory_dao-accounts-count-table"
    , "n_7763cd0330f59f3c66e431dcd63a2c5c5e2e0b70.dao-hive-factory_dao-accounts-table"
    , "n_7763cd0330f59f3c66e431dcd63a2c5c5e2e0b70.dao-hive-factory_dao-actions-table"
    , "n_7763cd0330f59f3c66e431dcd63a2c5c5e2e0b70.dao-hive-factory_dao-charters-table"
    , "n_7763cd0330f59f3c66e431dcd63a2c5c5e2e0b70.dao-hive-factory_dao-links-table"
    , "n_7763cd0330f59f3c66e431dcd63a2c5c5e2e0b70.dao-hive-factory_dao-membership-ids-table"
    , "n_7763cd0330f59f3c66e431dcd63a2c5c5e2e0b70.dao-hive-factory_dao-messages-table"
    , "n_7763cd0330f59f3c66e431dcd63a2c5c5e2e0b70.dao-hive-factory_dao-pools-table"
    , "n_7763cd0330f59f3c66e431dcd63a2c5c5e2e0b70.dao-hive-factory_dao-proposals-table"
    , "n_7763cd0330f59f3c66e431dcd63a2c5c5e2e0b70.dao-hive-factory_dao-role-table"
    , "n_7763cd0330f59f3c66e431dcd63a2c5c5e2e0b70.dao-hive-factory_dao-thresholds-table"
    , "n_7763cd0330f59f3c66e431dcd63a2c5c5e2e0b70.dao-hive-factory_dao-total-count-table"
    , "n_7763cd0330f59f3c66e431dcd63a2c5c5e2e0b70.dao-hive-factory_dao-updates-table"
    , "n_7763cd0330f59f3c66e431dcd63a2c5c5e2e0b70.dao-hive-factory_dao-votes-table"
    , "n_7763cd0330f59f3c66e431dcd63a2c5c5e2e0b70.dao-hive-factory_daos-table"
    , "n_7763cd0330f59f3c66e431dcd63a2c5c5e2e0b70.dao-hive-factory_user-proposition-records"
    , "n_7763cd0330f59f3c66e431dcd63a2c5c5e2e0b70.dao-hive-factory_user-vote-records"
    , "n_a2fceb4ebd41f3bb808da95d1ca0af9b15cb068c.kadenai-donate_donate"
    , "n_a2fceb4ebd41f3bb808da95d1ca0af9b15cb068c.kadenai-donate_values"
    , "n_c5a4b8c52f0866d66bc55864998a37cc089db47c.KEKW_token-table"
    , "n_df83905bd42ed92e559616bb707f74979a4010e0.bana_token-table"
    , "runonflux.flux_ledger"
    , "runonflux.testflux_ledger" ]


tests :: TestTree
tests = withResource (unsafeCreateSqlitePactDb serialisePact_raw_spaninfo (T.pack dbFilePath))
  (\(_, db, cache) -> unsafeCloseSqlitePactDb db cache) $ \pdbio ->
    testGroup "Legacy PactDb Regression" $
      runTableDecodeRegression (view _1 <$> pdbio) <$> allTables



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


