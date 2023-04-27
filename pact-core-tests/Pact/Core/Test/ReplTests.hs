module Pact.Core.Test.ReplTests where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad(when)
import Data.IORef
import Data.ByteString(ByteString)
import Data.Foldable(traverse_)
import Data.Text.Encoding(decodeUtf8)
import System.Directory
import System.FilePath

import qualified Data.Text as T
import qualified Data.ByteString as B

import Pact.Core.Gas
import Pact.Core.Literal
import Pact.Core.Persistence

import Pact.Core.Untyped.Eval.Runtime

import Pact.Core.Repl.Utils
import Pact.Core.Repl.Compile
import Pact.Core.Errors (PactError(..), ExecutionError (..))

tests :: IO TestTree
tests = do
  files <- replTestFiles
  pure $ testGroup "Core repl tests" (runFileReplTest <$> files)


replTestDir :: [Char]
replTestDir = "pact-core-tests" </> "pact-tests"

replTestFiles :: IO [FilePath]
replTestFiles = do
  filter (\f -> isExtensionOf "repl" f || isExtensionOf "pact" f) <$> getDirectoryContents replTestDir

runFileReplTest :: TestName -> TestTree
runFileReplTest file = testCase file $ B.readFile (replTestDir </> file) >>= runReplTest file

runReplTest :: FilePath -> ByteString -> Assertion
runReplTest file src = do
  gasRef <- newIORef (Gas 0)
  gasLog <- newIORef Nothing
  pdb <- mockPactDb
  let rstate = ReplState
            { _replFlags =  mempty
            , _replLoaded = mempty
            , _replPactDb = pdb
            , _replGas = gasRef
            , _replEvalLog = gasLog }
  stateRef <- newIORef rstate
  runReplT stateRef (interpretReplProgram src) >>= \case
    Left e -> let
      rendered = replError (ReplSource (T.pack file) (decodeUtf8 src)) e
      in assertFailure (T.unpack rendered)
    Right output -> traverse_ ensurePassing output
  where
  ensurePassing = \case
    InterpretLog _ ->  pure ()
    InterpretValue v i -> case v of
      VLiteral (LString msg) -> do
        let render = replError (ReplSource (T.pack file) (decodeUtf8 src)) (PEExecutionError (ExecutionError msg) i)
        when (T.isPrefixOf "FAILURE:" msg) $ assertFailure (T.unpack render)
      _ -> pure ()

