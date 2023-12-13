module Pact.Core.Test.ReplTests where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad(when)
import Data.IORef
import Data.Default
import Data.Text(Text)
import Data.Foldable(traverse_)
import System.Directory
import System.FilePath

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Pact.Core.Gas
import Pact.Core.Literal
import Pact.Core.Persistence
import Pact.Core.Interpreter

import Pact.Core.Repl.Utils
import Pact.Core.Compile
import Pact.Core.Repl.Compile
import Pact.Core.PactValue
import Pact.Core.Environment
import Pact.Core.Builtin
import Pact.Core.Errors

tests :: IO TestTree
tests = do
  files <- replTestFiles
  pure $ testGroup "CoreReplTests" (runFileReplTest <$> files)


replTestDir :: [Char]
replTestDir = "pact-core-tests" </> "pact-tests"

replTestFiles :: IO [FilePath]
replTestFiles = do
  filter (\f -> isExtensionOf "repl" f || isExtensionOf "pact" f) <$> getDirectoryContents replTestDir

runFileReplTest :: TestName -> TestTree
runFileReplTest file = testCase file $ T.readFile (replTestDir </> file) >>= runReplTest file

runReplTest :: FilePath -> Text -> Assertion
runReplTest file src = do
  gasRef <- newIORef (Gas 0)
  gasLog <- newIORef Nothing
  pdb <- mockPactDb
  let ee = defaultEvalEnv pdb replRawBuiltinMap
      source = SourceCode (takeFileName file) src
  let rstate = ReplState
            { _replFlags = mempty
            , _replEvalState = def
            , _replPactDb = pdb
            , _replGas = gasRef
            , _replEvalLog = gasLog
            , _replCurrSource = source
            , _replEvalEnv = ee
            , _replTx = Nothing
            }
  stateRef <- newIORef rstate
  runReplT stateRef (interpretReplProgram source (const (pure ()))) >>= \case
    Left e -> let
      rendered = replError (ReplSource (T.pack file) src) e
      in assertFailure (T.unpack rendered)
    Right output -> traverse_ ensurePassing output
  where
  ensurePassing = \case
    RCompileValue (InterpretValue (IPV v i)) -> case v of
      PLiteral (LString msg) -> do
        let render = replError (ReplSource (T.pack file) src) (PEExecutionError (EvalError msg) i)
        when (T.isPrefixOf "FAILURE:" msg) $ assertFailure (T.unpack render)
      _ -> pure ()
    _ -> pure ()
