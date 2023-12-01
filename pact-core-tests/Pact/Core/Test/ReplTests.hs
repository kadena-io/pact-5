{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Pact.Core.Test.ReplTests where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad(when)
import Data.IORef
import Data.Default
import Data.ByteString(ByteString)
import Data.Foldable(traverse_)
import Data.Text.Encoding(decodeUtf8)
import System.Directory
import System.FilePath

import qualified Data.Text as T
import qualified Data.ByteString as B

import Pact.Core.Gas
import Pact.Core.Literal
import Pact.Core.Persistence.MockPersistence
import Pact.Core.Interpreter

import Pact.Core.Repl.Utils
import Pact.Core.Persistence (PactDb(..), ModuleData(..), builtinModuleData, infoModuleData)
import Pact.Core.Persistence.SQLite (withSqlitePactDb)

import Pact.Core.Info (SpanInfo)
import Pact.Core.Compile
import Pact.Core.IR.Term (Module(..), EvalModule)
import Pact.Core.Repl.Compile
import Pact.Core.PactValue
import Pact.Core.Environment
import Pact.Core.Builtin
import Pact.Core.Errors
import Pact.Core.Serialise
import Control.Lens

tests :: IO TestTree
tests = do
  files <- replTestFiles
  pure $ testGroup "Repl Tests"
    [ testGroup "in-memory db" (runFileReplTest <$> files)
    , testGroup "sqlite db" (runFileReplTestSqlite <$> files)
    ]


-- enhanceModuleData :: ModuleData RawBuiltin () -> ModuleData ReplRawBuiltin SpanInfo
-- enhanceModuleData = \case
--   ModuleData _em _defs -> undefined
--   InterfaceData _ifd _defs -> undefined

-- stripModuleData :: ModuleData ReplRawBuiltin SpanInfo -> ModuleData RawBuiltin ()
-- stripModuleData = \case
--   ModuleData _em _defs -> undefined
--   InterfaceData _ifd _defs -> undefined

-- enhanceEvalModule :: EvalModule RawBuiltin () -> EvalModule ReplRawBuiltin SpanInfo
-- enhanceEvalModule Module
--   { _mName
--   , _mGovernance
--   , _mDefs
--   , _mBlessed
--   , _mImports
--   , _mImplements
--   , _mHash
--   , _mInfo
--   } = Module
--       { _mName
--       , _mGovernance
--       , _mDefs = undefined _mDefs
--       , _mBlessed
--       , _mImports
--       , _mImplements
--       , _mHash
--       , _mInfo = def
--       }


replTestDir :: [Char]
replTestDir = "pact-core-tests" </> "pact-tests"

replTestFiles :: IO [FilePath]
replTestFiles = do
  filter (\f -> isExtensionOf "repl" f || isExtensionOf "pact" f) <$> getDirectoryContents replTestDir

runFileReplTest :: TestName -> TestTree
runFileReplTest file = testCase file $ do
  pdb <- mockPactDb
  B.readFile (replTestDir </> file) >>= runReplTest pdb file

runFileReplTestSqlite :: TestName -> TestTree
runFileReplTestSqlite file = testCase file $ do
  ctnt <- B.readFile (replTestDir </> file)
  withSqlitePactDb (enhance serialisePact) ":memory:" $ \pdb -> do
    runReplTest pdb file ctnt
  where
    enhance :: PactSerialise RawBuiltin () -> PactSerialise ReplRawBuiltin SpanInfo
    enhance s = s{ _encodeModuleData = \md ->
                     let encMod = md & builtinModuleData %~ (\(RBuiltinWrap r) -> r)
                                     & infoModuleData %~ const ()
                     in _encodeModuleData s encMod
                 , _decodeModuleData = \bs -> case _decodeModuleData s bs of
                     Just mdDoc -> Just $ LegacyDocument $ view document mdDoc
                       & builtinModuleData %~ RBuiltinWrap
                       & infoModuleData %~ const def
                     Nothing -> error "unexpected decoding error"
                 }
    
  

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

