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
import Pact.Core.Test.ReplTests (runReplTest)
import Pact.Core.Persistence
import Pact.Core.Persistence.MockPersistence
import Pact.Core.Serialise
import Pact.Core.Builtin
import Data.Foldable (traverse_)
import Control.Lens
import Pact.Core.Repl.Compile
import Data.Default
import Pact.Core.IR.Term

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
    lm' <- sequence <$> traverse (toModuleData p) lm
    case lm' of
      Nothing -> error "Reading existing modules failed"
      Just ms -> do
        pdb <- mockPactDb serialisePact_repl_spaninfo

        -- add default spaninfo
        let ms' = (fmap.fmap) (const def) ms

        -- write modules into module cache
        traverse_ (\m -> writeModule pdb Write (view mdModuleName m) (liftReplBuiltin m)) ms'

        modTests <- forM repl $ \r -> do
          let filePath = legacyTestDir </> p </> r
          src <- T.readFile filePath
          pure $ testCase r (runReplTest pdb filePath src interpretReplProgram)
        pure (testGroup p modTests)
  where
  toModuleData p fp =
    decodeModuleData <$> BS.readFile (legacyTestDir </> p </> fp)

  -- Boilerplate for lifting `CoreBuiltin` into `ReplCoreBuiltin`
  -- Note: this is only used in this test.
  liftReplBuiltin :: ModuleData CoreBuiltin a -> ModuleData ReplCoreBuiltin a
  liftReplBuiltin = \case
    ModuleData em ed -> let
      defs' = over (traverseDefTerm . termBuiltin) RBuiltinWrap <$> _mDefs em
      ed' = over (traverseDefTerm . termBuiltin) RBuiltinWrap <$> ed
      in ModuleData (em{_mDefs = defs'}) ed'
    InterfaceData im ed -> let
      ifdefs = over (traverseIfDefTerm . termBuiltin) RBuiltinWrap <$> _ifDefns im
      ed' = over (traverseDefTerm . termBuiltin) RBuiltinWrap <$> ed
      in InterfaceData (im{_ifDefns = ifdefs}) ed'
      
