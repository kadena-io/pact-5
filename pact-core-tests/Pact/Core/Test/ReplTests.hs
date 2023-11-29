{-# LANGUAGE GADTs #-}

module Pact.Core.Test.ReplTests where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad(when)
import Data.IORef
import Data.Default
import Data.ByteString(ByteString)
import Data.Foldable(traverse_)
import Data.Text.Encoding(decodeUtf8)
import Control.Lens
import Control.Lens.Plated
import System.Directory
import System.FilePath
import Data.Default

import qualified Data.Text as T
import qualified Data.ByteString as B

import Pact.Core.Gas
import Pact.Core.Literal
-- import Pact.Core.Persistence
import Pact.Core.Persistence.MockPersistence
-- import Pact.Core.Serialise
import Pact.Core.Interpreter

import Pact.Core.Repl.Utils
import Pact.Core.Persistence (PactDb(..), Domain(..), readKeySet, readModule, ModuleData(..), readNamespace, readDefPacts
                             ,writeKeySet, writeNamespace, writeDefPacts, writeModule
                             ,moduleDataInfo, moduleDataBuiltin)
import Pact.Core.Persistence.SQLite (withSqlitePactDb)
import Pact.Core.Serialise (serialisePact)

import Pact.Core.Info (SpanInfo)
import Pact.Core.Compile
import Pact.Core.Repl.Compile
import Pact.Core.PactValue
import Pact.Core.Environment
import Pact.Core.Builtin
import Pact.Core.Errors
import Pact.Core.IR.Term (termBuiltin)

tests :: IO TestTree
tests = do
  files <- replTestFiles
  pure $ testGroup "Repl Tests"
    [ testGroup "in-memory db" (runFileReplTest mockPactDb <$> files)
    , testGroup "sqlite db" (runFileReplTestSqlite <$> files)
    ]



replTestDir :: [Char]
replTestDir = "pact-core-tests" </> "pact-tests"

replTestFiles :: IO [FilePath]
replTestFiles = do
  filter (\f -> isExtensionOf "repl" f || isExtensionOf "pact" f) <$> getDirectoryContents replTestDir

runFileReplTest :: IO (PactDb (ReplBuiltin RawBuiltin) SpanInfo) -> TestName -> TestTree
runFileReplTest mkPactDb file = testCase file $ do
  pdb <- mkPactDb
  B.readFile (replTestDir </> file) >>= runReplTest pdb file

enhance :: PactDb RawBuiltin () -> PactDb ReplRawBuiltin SpanInfo
enhance pdb = PactDb
  { _pdbPurity = _pdbPurity pdb
  , _pdbRead  = \case
      (DUserTables tbl) -> _pdbRead pdb (DUserTables tbl)
      DKeySets -> readKeySet pdb
      DModules -> \k -> fmap enhanceModule <$> readModule pdb k
      DNamespaces -> readNamespace pdb
      DDefPacts -> readDefPacts pdb
  , _pdbWrite = \wt -> \case
      (DUserTables tbl) -> _pdbWrite pdb wt (DUserTables tbl)
      DKeySets -> writeKeySet pdb wt
      DModules -> \k v -> writeModule pdb wt k (stripModule v)
      DNamespaces -> writeNamespace pdb wt
      DDefPacts -> writeDefPacts pdb wt
  , _pdbKeys = undefined
  , _pdbCreateUserTable = _pdbCreateUserTable pdb
  , _pdbBeginTx = _pdbBeginTx pdb
  , _pdbCommitTx = _pdbCommitTx pdb
  , _pdbRollbackTx = _pdbRollbackTx pdb
  , _pdbTxIds = _pdbTxIds pdb
  , _pdbGetTxLog = _pdbGetTxLog pdb
  , _pdbTxId = _pdbTxId pdb
  }
  where
    enhanceModule :: ModuleData RawBuiltin () -> ModuleData ReplRawBuiltin SpanInfo
    enhanceModule m = m
      & moduleDataBuiltin %~ RBuiltinWrap
      & moduleDataInfo %~ const def
      
    stripModule :: ModuleData ReplRawBuiltin SpanInfo -> ModuleData RawBuiltin ()
    stripModule m = m
      & moduleDataInfo %~ const ()
      & moduleDataBuiltin %~ \(RBuiltinWrap b) -> b



runFileReplTestSqlite :: TestName -> TestTree
runFileReplTestSqlite file = testCase file $ do
  ctnt <- B.readFile (replTestDir </> file)
  withSqlitePactDb serialisePact ":memory:" $ \pdb -> do
    runReplTest (enhance pdb) file ctnt

  

runReplTest :: PactDb ReplRawBuiltin SpanInfo -> FilePath -> ByteString -> Assertion
runReplTest pdb file src = do
  gasRef <- newIORef (Gas 0)
  gasLog <- newIORef Nothing
  let ee = defaultEvalEnv pdb replRawBuiltinMap
      source = SourceCode (takeFileName file) src
  let rstate = ReplState
            { _replFlags =  mempty
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
      rendered = replError (ReplSource (T.pack file) (decodeUtf8 src)) e
      in assertFailure (T.unpack rendered)
    Right output -> traverse_ ensurePassing output
  where
  ensurePassing = \case
    RCompileValue (InterpretValue (IPV v i)) -> case v of
      PLiteral (LString msg) -> do
        let render = replError (ReplSource (T.pack file) (decodeUtf8 src)) (PEExecutionError (EvalError msg) i)
        when (T.isPrefixOf "FAILURE:" msg) $ assertFailure (T.unpack render)
      _ -> pure ()
    _ -> pure ()

