{-# LANGUAGE GADTs #-}

module Pact.Core.Test.ReplTests
 ( tests
 , runReplTest
 , ReplSourceDir(..))where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.IORef
import Data.Default
import Data.Foldable(traverse_)
import System.Directory
import System.FilePath

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Pact.Core.Persistence.MockPersistence

import Pact.Core.Repl.Utils
import Pact.Core.Persistence.SQLite (withSqlitePactDb)

import Pact.Core.Info (SpanInfo)
import Pact.Core.Repl.Compile
import Pact.Core.Environment
import Pact.Core.Builtin
import Pact.Core.Errors
import Pact.Core.Serialise
import Pact.Core.Persistence
import Pact.Core.IR.Term
import qualified Pact.Core.IR.ModuleHashing as MH


type Interpreter = SourceCode -> ReplM ReplCoreBuiltin [ReplCompileValue]

tests :: IO TestTree
tests = do
  files <- replTestFiles
  pure $ testGroup "ReplTests"
    [ testGroup "in-memory db:bigstep" (runFileReplTest interpretReplProgramBigStep <$> files)
    , testGroup "sqlite db:bigstep" (runFileReplTestSqlite interpretReplProgramBigStep <$> files)
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
  ee <- defaultEvalEnv pdb replBuiltinMap
  let source = SourceCode (path </> file) src
  let rstate = mkReplState ee (const (pure ())) & replCurrSource .~ source
  stateRef <- newIORef rstate
  evalReplM stateRef (interp source) >>= \case
    Left e -> let
      rendered = replError (SourceCode file src) e
      in assertFailure (T.unpack rendered)
    Right _ -> do
      traverse_ ensurePassing . _replTestResults =<< readIORef stateRef
      ensureModuleHashesMatch
  where
  moduleHashMatches mn = void $ runMaybeT $ do
    rawMod <- MaybeT $ ignoreGas def $ _pdbRead pdb DModules mn
    case rawMod of
      ModuleData rawModule _ -> do
        modNoReplBuiltins <- hoistMaybe $ (traverseModuleTerm . termBuiltin) (preview _RBuiltinWrap) rawModule
        liftIO $ assertEqual "Module hashes match if there's no repl builtins" (MH.hashModulePure modNoReplBuiltins) (MH.hashModulePure rawModule)
      -- Note: Interfaces cannot have builtins in them.
      _ -> mzero
  ensureModuleHashesMatch = do
    keys <- ignoreGas def $ _pdbKeys pdb DModules
    traverse_ moduleHashMatches keys
  ensurePassing (ReplTestResult _testName loc _ res) = case res of
    ReplTestPassed -> pure()
    ReplTestFailed msg -> do
      let render = replError (SourceCode file src) (PEExecutionError (EvalError msg) [] loc)
      assertFailure (T.unpack render)
