module Pact.Core.Test.ReplTestUtils(runReplTest, ReplSourceDir(..)) where

import Control.Lens
import Control.Monad
import Data.IORef
import System.FilePath

import qualified Data.Text as T

import Pact.Core.Repl.Utils

import Pact.Core.Info
import Pact.Core.Environment
import Pact.Core.Builtin
import Pact.Core.Errors
import Pact.Core.Persistence
import Pact.Core.Repl
import Pact.Core.Repl.Compile


newtype ReplSourceDir
  = ReplSourceDir FilePath

runReplTest
  :: ReplSourceDir
  -> PactDb ReplCoreBuiltin FileLocSpanInfo
  -> FilePath
  -> T.Text
  -> ReplInterpreter
  -> IO (Either (PactError FileLocSpanInfo) [ReplCompileValue], ReplState ReplCoreBuiltin)
runReplTest (ReplSourceDir path) pdb file src interp = do
  ee <- defaultEvalEnv pdb replBuiltinMap
  let source = SourceCode (path </> file) src
  let rstate = mkReplState ee (const (const (pure ()))) (\f reset -> void (loadFile interp f reset)) & replCurrSource .~ source
  stateRef <- newIORef rstate
  (,) <$> evalReplM stateRef (interpretReplProgram interp source) <*> readIORef stateRef