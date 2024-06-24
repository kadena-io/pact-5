-- | Tests that values encoded by legacy pact deployments
--   decode into the expected pact-core values.
--   TODO: We need to write many more such tests during the legacy
--   pact compatibilty epic.

{-# LANGUAGE OverloadedStrings #-}

module Pact.Core.Test.LegacySerialiseTests where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Pact.Core.Serialise.LegacyPact

import Control.Monad (forM)
import System.Directory
import System.FilePath
import qualified Data.ByteString as BS
import qualified Data.Text.IO as T
import Pact.Core.Test.ReplTests
import Pact.Core.Persistence
import Pact.Core.Persistence.MockPersistence
import Pact.Core.Serialise
import Data.Foldable
import Control.Lens
import Pact.Core.Repl.Compile
import Data.Default

import Pact.Core.Gas

tests :: IO TestTree
tests = testGroup "Legacy Repl Tests" <$> legacyTests

legacyTestDir :: String
legacyTestDir = "pact-tests" </> "legacy-serial-tests"


-- Load a set of test cases from the file system.
-- Returns a list of tuples of:
--    - Test case name
--    - Test repl scripts with `expect` calls (*.repl)
--    - Legacy-encoded version of the same module (*.json)
replTestFiles :: IO [(String, [FilePath], [FilePath])]
replTestFiles = do
  base <- getDirectoryContents legacyTestDir
  forM base $ \p -> do
    let testPath = legacyTestDir </> p
    files <- getDirectoryContents testPath
    let replFiles = filter (isExtensionOf "repl") files
        modFiles =  filter (isExtensionOf "json") files
    pure (p, replFiles, modFiles)

legacyTests :: IO [TestTree]
legacyTests = do
  t <- replTestFiles
  forM t $ \(p, repl, lm) -> do
    -- Perform legacy decoding on all `.json` files for that test
    lm' <- sequence <$> traverse (toModuleData p) lm

    case lm' of
      Nothing -> error "Reading existing modules failed"
      Just ms -> do
        modTests <- fmap concat $ forM repl $ \r -> do
          sequence [runTest r interpretReplProgramBigStep "CEK", runTest r interpretReplProgramDirect "Direct"]
        pure (testGroup p modTests)
        where
        runTest r interpreter interpName = do
          pdb <- mockPactDb serialisePact_repl_spaninfo

          -- add default spaninfo
          let ms' = (fmap.fmap) (const def) ms
          let filePath = p </> r
          src <- T.readFile (legacyTestDir </> filePath)

          -- write modules into the pactdb
          _ <- ignoreGas def $ forM_ ms' $ \m ->
            _pdbWrite pdb Write DModules (view mdModuleName m) (liftReplBuiltin m)

          pure $ testCase (r <> " with interpreter (" <> interpName <> ")")
            $ runReplTest (ReplSourceDir legacyTestDir) pdb filePath src interpreter

  where
  toModuleData p fp =
    decodeModuleData <$> BS.readFile (legacyTestDir </> p </> fp)


