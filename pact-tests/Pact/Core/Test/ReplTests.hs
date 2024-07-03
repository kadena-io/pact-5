{-# LANGUAGE GADTs #-}

module Pact.Core.Test.ReplTests
 ( tests
 , runReplTest
 , ReplSourceDir(..))where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad(when)
import Data.IORef
import Data.Foldable(traverse_)
import System.Directory
import System.FilePath

import qualified Data.Text as T
import qualified Data.Text.IO as T

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
    [ testGroup "in-memory db:bigstep" (runFileReplTest interpretReplProgramBigStep <$> files)
    , testGroup "sqlite db:bigstep" (runFileReplTestSqlite interpretReplProgramBigStep <$> files)
    , testGroup "in-memory db:smallstep" (runFileReplTest interpretReplProgramSmallStep <$> files)
    , testGroup "sqlite db:smallstep" (runFileReplTestSqlite interpretReplProgramSmallStep <$> files)
    , testGroup "in-memory db:direct" (runFileReplTest interpretReplProgramDirect <$> files)
    , testGroup "sqlite db:direct" (runFileReplTestSqlite interpretReplProgramDirect <$> files)
    ]

newtype ReplSourceDir
  = ReplSourceDir FilePath

defaultReplTestDir :: FilePath
defaultReplTestDir = "pact-tests" </> "pact-tests"


replTestFiles :: IO [FilePath]
replTestFiles = filter (\f -> isExtensionOf "repl" f || isExtensionOf "pact" f) <$> getDirectoryContents defaultReplTestDir

runFileReplTest :: Interpreter -> TestName -> TestTree
runFileReplTest interp file = testCase file $ do
  pdb <- mockPactDb serialisePact_repl_spaninfo
  src <- T.readFile (defaultReplTestDir </> file)
  runReplTest (ReplSourceDir defaultReplTestDir) pdb file src interp


runFileReplTestSqlite :: Interpreter -> TestName -> TestTree
runFileReplTestSqlite interp file = testCase file $ do
  ctnt <- T.readFile (defaultReplTestDir </> file)
  withSqlitePactDb serialisePact_repl_spaninfo ":memory:" $ \pdb -> do
    runReplTest (ReplSourceDir defaultReplTestDir) pdb file ctnt interp

runReplTest
  :: ReplSourceDir
  -> PactDb ReplCoreBuiltin SpanInfo
  -> FilePath
  -> T.Text
  -> Interpreter
  -> Assertion
runReplTest (ReplSourceDir path) pdb file src interp = do
  gasLog <- newIORef Nothing
  ee <- defaultEvalEnv pdb replBuiltinMap
  let source = SourceCode (path </> file) src
  let rstate = ReplState
            { _replFlags = mempty
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
