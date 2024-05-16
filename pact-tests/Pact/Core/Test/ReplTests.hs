{-# LANGUAGE GADTs #-}

module Pact.Core.Test.ReplTests where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad(when)
import Data.IORef
import Data.Default
import Data.Foldable(traverse_)
import System.Directory
import System.FilePath

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Pact.Core.Gas
import Pact.Core.Literal
import Pact.Core.Persistence.MockPersistence

import Pact.Core.Repl.Utils
import Pact.Core.Persistence (PactDb)
import Pact.Core.Persistence.SQLite (withSqlitePactDb)

import Pact.Core.Info (SpanInfo)
import Pact.Core.Compile
import Pact.Core.Repl.Compile
import Pact.Core.PactValue
import Pact.Core.Environment
import Pact.Core.Builtin
import Pact.Core.Errors
import Pact.Core.Serialise


type Interpreter = SourceCode -> (ReplCompileValue -> ReplM ReplCoreBuiltin ()) -> ReplM ReplCoreBuiltin [ReplCompileValue]

tests :: IO TestTree
tests = do
  files <- replTestFiles
  pure $ testGroup "ReplTests"
    [ testGroup "in-memory db:bigstep" (runFileReplTest interpretReplProgram <$> files)
    , testGroup "sqlite db:bigstep" (runFileReplTestSqlite interpretReplProgram <$> files)
    , testGroup "in-memory db:smallstep" (runFileReplTest interpretReplProgramSmallStep <$> files)
    , testGroup "sqlite db:smallstep" (runFileReplTestSqlite interpretReplProgramSmallStep <$> files)
    ]

replTestDir :: [Char]
replTestDir = "pact-tests" </> "pact-tests"

replTestFiles :: IO [FilePath]
replTestFiles = do
  filter (\f -> isExtensionOf "repl" f || isExtensionOf "pact" f) <$> getDirectoryContents replTestDir

runFileReplTest :: Interpreter -> TestName -> TestTree
runFileReplTest interp file = testCase file $ do
  pdb <- mockPactDb serialisePact_repl_spaninfo
  src <- T.readFile (replTestDir </> file)
  runReplTest pdb file src interp


runFileReplTestSqlite :: Interpreter -> TestName -> TestTree
runFileReplTestSqlite interp file = testCase file $ do
  ctnt <- T.readFile (replTestDir </> file)
  withSqlitePactDb serialisePact_repl_spaninfo ":memory:" $ \pdb -> do
    runReplTest pdb file ctnt interp

runReplTest :: PactDb ReplCoreBuiltin SpanInfo -> FilePath -> T.Text -> Interpreter -> Assertion
runReplTest pdb file src interp = do
  gasRef <- newIORef (Gas 0)
  gasLog <- newIORef Nothing
  ee <- defaultEvalEnv pdb replCoreBuiltinMap
  let source = SourceCode (replTestDir </> file) src
  let rstate = ReplState
            { _replFlags = mempty
            , _replEvalState = def
            , _replPactDb = pdb
            , _replGas = gasRef
            , _replEvalLog = gasLog
            , _replCurrSource = source
            , _replEvalEnv = ee
            , _replUserDocs = mempty
            , _replTLDefPos = mempty
            , _replTx = Nothing
            , _replNativesEnabled = False
            }
  stateRef <- newIORef rstate
  runReplT stateRef (interp source (const (pure ()))) >>= \case
    Left e -> let
      rendered = replError (SourceCode file src) e
      in assertFailure (T.unpack rendered)
    Right output -> traverse_ ensurePassing output
  where
  ensurePassing = \case
    RCompileValue (InterpretValue v i) -> case v of
      PLiteral (LString msg) -> do
        let render = replError (SourceCode file src) (PEExecutionError (EvalError msg) [] i)
        when (T.isPrefixOf "FAILURE:" msg) $ assertFailure (T.unpack render)
      _ -> pure ()
    _ -> pure ()
