{-# LANGUAGE PartialTypeSignatures #-}

module Pact.Core.Test.GasGolden
  (tests
  ) where

import Control.Monad
import Data.IORef
import Data.Maybe
import Data.Text (Text)
import Pact.Core.Builtin
import Pact.Core.Environment
import Pact.Core.Gas
import Pact.Core.Persistence.MockPersistence
import Pact.Core.Repl
import Pact.Core.Repl.Compile
import Pact.Core.Repl.Utils
import Pact.Core.Serialise
import Pact.Core.Evaluate(versionedNatives)
import System.Directory
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.List (sort)
import Control.Lens

tests :: IO TestTree
tests = do
  cases <- gasTestFiles
  pure $ testGroup "Gas Goldens"
    [ testCase "Capture all builtins" $ captureBuiltins (fst <$> cases)
    , goldenVsStringDiff "Gas Goldens, Pact 5.0: CEK" runDiff (gasGoldenOutputDir </> "builtinGas50.golden") (pact50Goldens cases interpretEvalBigStep)
    , goldenVsStringDiff "Gas Goldens, Pact 5.0: Direct" runDiff (gasGoldenOutputDir </> "builtinGas50.golden") (pact50Goldens cases interpretEvalDirect)
    , goldenVsStringDiff "Gas Goldens, Pact Latest: CEK" runDiff (gasGoldenOutputDir </> "builtinGas.golden") (gasGoldenTests cases interpretEvalBigStep)
    , goldenVsStringDiff "Gas Goldens, Pact Latest: Direct" runDiff (gasGoldenOutputDir </> "builtinGas.golden") (gasGoldenTests cases interpretEvalDirect)
    ]
  where
  pact50Goldens cases interp = gasGoldenTestsWithFlags (S.singleton FlagDisablePact51) cases interp
  runDiff = \ref new -> ["diff", "-u", ref, new]


gasTestDir :: [Char]
gasTestDir = "pact-tests" </> "gas-goldens"

gasGoldenOutputDir :: [Char]
gasGoldenOutputDir = "pact-tests" </> "gas-goldens" </> "goldens"


gasTestFiles :: IO [(Text, FilePath)]
gasTestFiles = do
  files <- filter (\f -> isExtensionOf "repl" f || isExtensionOf "pact" f) <$> getDirectoryContents gasTestDir
  pure $ (\f -> (T.pack $ dropExtension f, f)) <$> files


captureBuiltins :: [Text] -> Assertion
captureBuiltins b = let
  b' = S.fromList $ lookupOp <$> b
  cb = S.fromList coreBuiltinNames `S.difference` excludedBuiltins
  missingTests = S.difference cb b'
  in assertEqual "Missing builtins" S.empty (S.map lookupOp missingTests)
  where
    -- todo: exluded as we need to work on the builtin
    excludedBuiltins = S.fromList
      ["verify-spv"]

lookupOp :: Text -> Text
lookupOp n = fromMaybe n (M.lookup n fileNameToOp)

lookupFileNameOp :: Text -> Text
lookupFileNameOp n = fromMaybe n (M.lookup n opToFileName)

gasGoldenTests :: [(Text, FilePath)] -> ReplInterpreter -> IO BS.ByteString
gasGoldenTests = gasGoldenTestsWithFlags mempty

gasGoldenTestsWithFlags :: S.Set ExecutionFlag -> [(Text, FilePath)] -> ReplInterpreter -> IO BS.ByteString
gasGoldenTestsWithFlags flags natives interp = do
  let enabledNatives = S.fromList $ fmap lookupFileNameOp $ M.keys $ versionedNatives flags
  let filteredTestsToRun = filter ((`S.member` enabledNatives) . fst) natives
  gasOutputs <- forM filteredTestsToRun $ \(fn, fp) -> do
    mGas <- runGasTest (gasTestDir </> fp) interp
    case mGas of
      Nothing -> fail $ "Could not execute the gas tests for: " <> show fp
      Just (MilliGas consumed) -> pure $ BS.fromStrict $ T.encodeUtf8 (lookupOp fn <> ": " <> T.pack (show consumed))
  let sortedGasOutputs = sort gasOutputs
  pure (BS.intercalate "\n" sortedGasOutputs)



opToFileName :: M.Map Text Text
opToFileName = M.fromList
  [ ("=", "eq")
  , ("!=", "neq")
  , ("&", "and")
  , ("*", "mult")
  , ("+", "add")
  , ("-", "sub")
  , ("/", "div")
  , ("<", "lt")
  , ("<=", "leq")
  , (">", "gt")
  , (">=", "geq")
  , ("^", "pow")
  , ("and?", "and-p")
  , ("not?", "not-p")
  , ("or?", "or-p")
  , ("|", "binary-or")
  , ("~", "binary-inverse")
  ]

fileNameToOp :: M.Map Text Text
fileNameToOp = M.fromList [(v,k) | (k, v) <- M.toList opToFileName]

runGasTest :: FilePath -> ReplInterpreter -> IO (Maybe MilliGas)
runGasTest file interpret = do
  src <- T.readFile file
  pdb <- mockPactDb serialisePact_repl_fileLocSpanInfo
  ee <- defaultEvalEnv pdb replBuiltinMap
  let ee' = ee & eeGasEnv . geGasModel .~ replTableGasModel (Just (maxBound :: MilliGasLimit))
      gasRef = ee' ^. eeGasEnv . geGasRef
  let source = SourceCode file src
  let rstate = mkReplState ee' (const (const (pure ()))) (\f r -> void (loadFile interpret f r)) & replCurrSource .~ source
  stateRef <- newIORef rstate
  evalReplM stateRef (interpretReplProgram interpret source) >>= \case
    Left _ -> pure Nothing
    Right _ -> Just <$> readIORef gasRef
