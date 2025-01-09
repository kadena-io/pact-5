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
import Pact.Core.Repl.Compile
import Pact.Core.Repl.Utils
import Pact.Core.Serialise
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

type InterpretPact = SourceCode -> ReplM ReplCoreBuiltin [ReplCompileValue]

tests :: IO TestTree
tests = do
  cases <- gasTestFiles
  pure $ testGroup "Gas Goldens"
    [ testCase "Capture all builtins" $ captureBuiltins (fst <$> cases)
    , goldenVsStringDiff "Gas Goldens: CEK" runDiff (gasTestDir </> "builtinGas.golden") (gasGoldenTests cases interpretReplProgramBigStep)
    , goldenVsStringDiff "Gas Goldens: Direct" runDiff (gasTestDir </> "builtinGas.golden") (gasGoldenTests cases interpretReplProgramDirect)
    ]
  where
  runDiff = \ref new -> ["diff", "-u", ref, new]


gasTestDir :: [Char]
gasTestDir = "pact-tests" </> "gas-goldens"


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


gasGoldenTests :: [(Text, FilePath)] -> InterpretPact -> IO BS.ByteString
gasGoldenTests c interp = do
  gasOutputs <- forM c $ \(fn, fp) -> do
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

runGasTest :: FilePath -> InterpretPact -> IO (Maybe MilliGas)
runGasTest file interpret = do
  src <- T.readFile file
  pdb <- mockPactDb serialisePact_repl_spaninfo
  ee <- defaultEvalEnv pdb replBuiltinMap
  let ee' = ee & eeGasEnv . geGasModel .~ replTableGasModel (Just (maxBound :: MilliGasLimit))
      gasRef = ee' ^. eeGasEnv . geGasRef
  let source = SourceCode file src
  let rstate = mkReplState ee' (const (pure ())) & replCurrSource .~ source
  stateRef <- newIORef rstate
  evalReplM stateRef (interpret source) >>= \case
    Left _ -> pure Nothing
    Right _ -> Just <$> readIORef gasRef
