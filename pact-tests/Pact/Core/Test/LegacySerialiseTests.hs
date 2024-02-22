-- | Tests that values encoded by legacy pact deployments
--   decode into the expected pact-core values.
--   TODO: We need to write many more such tests during the legacy
--   pact compatibilty epic.

{-# LANGUAGE OverloadedStrings #-}

module Pact.Core.Test.LegacySerialiseTests where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import Data.Maybe
import Pact.Core.Serialise.LegacyPact
import Pact.Core.Guards

import Control.Monad (forM)
import System.Directory
import System.FilePath
import qualified Data.ByteString as BS
import qualified Data.Text.IO as T
import Pact.Core.Test.ReplTests (runReplTest)
import Pact.Core.Persistence
import Pact.Core.Persistence.MockPersistence
import Pact.Core.Serialise
import Data.Foldable (traverse_)
import Control.Lens
import Pact.Core.Repl.Compile
import Data.Default

tests :: TestTree
tests = testGroup "Legacy Serialisation"
  [ testGroup "KeySet"
    [ testCase "pred: keys-2"   $ assertBool "KeySet decoding failed" (isJust (decodeKeySet "{\"pred\":\"keys-2\",\"keys\":[\"ddd8\",\"ed0\"]}"))
    , testCase "pred: keys-all" $ assertBool "KeySet decoding failed" (isJust (decodeKeySet "{\"pred\":\"keys-all\",\"keys\":[\"ddd8\",\"ed0\"]}"))
    , testCase "pred: keys-any" $ assertBool "KeySet decoding failed" (isJust (decodeKeySet "{\"pred\":\"keys-any\",\"keys\":[\"ddd8\",\"ed0\"]}"))
    , testCase "pred defaults"  $ assertBool "KeySet decoding failed" (maybe False (\k -> KeysAll == _ksPredFun k) (decodeKeySet "{\"pred\":\"keys-all\",\"keys\":[\"ddd8\",\"ed0\"]}"))
    , testCase "pred invalid" $ assertBool "Accept invalid pred" (isNothing (decodeKeySet "{\"pred\":123,\"keys\":[\"ddd8\",\"ed0\"]}"))
    , testCase "pred empty" $ assertBool "Accept empty pred" (isNothing (decodeKeySet "{\"pred\":\"\",\"keys\":[\"ddd8\",\"ed0\"]}"))
    , testCase "pred custom" $ assertBool "Accept custom pred" (isJust (decodeKeySet "{\"pred\":\"b.ABC\",\"keys\":[\"ddd8\",\"ed0\"]}"))
    ]
    -- TODO: Add more test cases.
  ]



legacyTestDir :: String
legacyTestDir = "pact-tests" </> "legacy-serial-test"


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
    lm' <- sequence <$> traverse toModuleData lm
    case lm' of
      Nothing -> error "Reading existing modules failed"
      Just ms -> do
        pdb <- mockPactDb serialisePact_raw_spaninfo

        -- add default spaninfo
        let ms' = (fmap.fmap) (const def)  ms

        -- write modules into module cache
        traverse_ (\m -> writeModule pdb Write (view mdModuleName m) m) ms'

        modTests <- forM repl $ \r -> do
          let filePath = legacyTestDir </> p </> r
          src <- T.readFile filePath
          pure $ testCase r (runReplTest undefined filePath src interpretReplProgram)
        pure (testGroup p modTests)
  where
  toModuleData fp =
    decodeModuleData <$> BS.readFile fp
