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

import Pact.Core.Info
import Pact.Core.Environment
import Pact.Core.Builtin
import Pact.Core.Errors
import Pact.Core.Serialise
import Pact.Core.Persistence
import Pact.Core.IR.Term
import Pact.Core.Repl
import Pact.Core.Repl.Compile
import qualified Pact.Core.IR.ModuleHashing as MH


tests :: IO TestTree
tests = do
  files <- replTestFiles
  pure $ testGroup "ReplTests"
    [ testGroup "in-memory db:bigstep" (runFileReplTest interpretEvalBigStep <$> files)
    , testGroup "sqlite db:bigstep" (runFileReplTestSqlite interpretEvalBigStep <$> files)
    , testGroup "in-memory db:direct" (runFileReplTest interpretEvalDirect <$> files)
    , testGroup "sqlite db:direct" (runFileReplTestSqlite interpretEvalDirect <$> files)
    ]

newtype ReplSourceDir
  = ReplSourceDir FilePath

defaultReplTestDir :: FilePath
defaultReplTestDir = "pact-tests" </> "pact-tests"


replTestFiles :: IO [FilePath]
replTestFiles = filter (\f -> isExtensionOf "repl" f || isExtensionOf "pact" f) <$> getDirectoryContents defaultReplTestDir

runFileReplTest :: ReplInterpreter -> TestName -> TestTree
runFileReplTest interp file = testCase file $ do
  pdb <- mockPactDb serialisePact_repl_fileLocSpanInfo
  src <- T.readFile (defaultReplTestDir </> file)
  runReplTest (ReplSourceDir defaultReplTestDir) pdb file src interp


runFileReplTestSqlite :: ReplInterpreter -> TestName -> TestTree
runFileReplTestSqlite interp file = testCase file $ do
  ctnt <- T.readFile (defaultReplTestDir </> file)
  withSqlitePactDb serialisePact_repl_fileLocSpanInfo ":memory:" $ \pdb -> do
    runReplTest (ReplSourceDir defaultReplTestDir) pdb file ctnt interp

runReplTest
  :: ReplSourceDir
  -> PactDb ReplCoreBuiltin FileLocSpanInfo
  -> FilePath
  -> T.Text
  -> ReplInterpreter
  -> Assertion
runReplTest (ReplSourceDir path) pdb file src interp = do
  ee <- defaultEvalEnv pdb replBuiltinMap
  let source = SourceCode (path </> file) src
  let rstate = mkReplState ee (const (const (pure ()))) (\f reset -> void (loadFile interp f reset)) & replCurrSource .~ source
  stateRef <- newIORef rstate
  evalReplM stateRef (interpretReplProgram interp source) >>= \case
    Left e -> do
      rstate' <- readIORef stateRef
      let rendered = renderLocatedPactErrorFromState rstate' e
      assertFailure (T.unpack rendered)
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
  ensurePassing (ReplTestResult _testName loc res) = case res of
    ReplTestPassed -> pure()
    ReplTestFailed msg -> do
      -- Todo: refine this with file locs
      let render = replError (SourceCode file src) (PEExecutionError (EvalError msg) [] loc)
      assertFailure (T.unpack render)
