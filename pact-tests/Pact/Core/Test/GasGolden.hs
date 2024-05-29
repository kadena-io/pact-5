-- | 

module Pact.Core.Test.GasGolden
  (tests
  ) where

import Control.Monad
import Data.Default
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
import Pact.Core.Gas.TableGasModel
import Control.Lens

tests :: IO TestTree
tests = do
  cases <- gasTestFiles
  pure $ testGroup "Gas Goldens"
    [ testCase "Capture all builtins" $ captureBuiltins (fst <$> cases)
    , goldenVsString "Gas Goldens" (gasTestDir </> "builtinGas.golden") (gasGoldenTests cases)
    ]


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
      ["pairing-check"
      ,"verify-spv"
      ,"enforce-verifier"
      ]

lookupOp :: Text -> Text
lookupOp n = fromMaybe n (M.lookup n fileNameToOp)


gasGoldenTests :: [(Text, FilePath)] -> IO BS.ByteString
gasGoldenTests c = do
  gasOutputs <- forM c $ \(fn, fp) -> do
    mGas <- runGasTest (gasTestDir </> fp)
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

runGasTest :: FilePath -> IO (Maybe MilliGas)
runGasTest file = do
  src <- T.readFile file
  pdb <-  mockPactDb serialisePact_repl_spaninfo
  gasLog <- newIORef Nothing
  ee <- defaultEvalEnv pdb replCoreBuiltinMap
  let ee' = ee & eeGasModel .~ replTableGasModel (maxBound :: MilliGasLimit)
      gasRef = ee' ^. eeGasRef
  let source = SourceCode file src
  let rstate = ReplState
            { _replFlags = mempty
            , _replEvalState = def
            , _replPactDb = pdb
            , _replEvalLog = gasLog
            , _replCurrSource = source
            , _replEvalEnv = ee'
            , _replUserDocs = mempty
            , _replTLDefPos = mempty
            , _replTx = Nothing
            , _replNativesEnabled = False
            }
  stateRef <- newIORef rstate
  runReplT stateRef (interpretReplProgram source (const (pure ()))) >>= \case
    Left _ -> pure Nothing
    Right _ -> Just <$> readIORef gasRef
