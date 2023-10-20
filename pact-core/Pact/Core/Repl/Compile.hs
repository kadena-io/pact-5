{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}


module Pact.Core.Repl.Compile
 ( ReplCompileValue(..)
 , interpretReplProgram
 ) where

import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class(liftIO)
import Data.Text(Text)
import Data.Default
import System.FilePath(takeFileName)


import qualified Data.Map.Strict as M
import qualified Data.ByteString as B
import qualified Data.Text as T

import Pact.Core.Persistence
import Pact.Core.Builtin
import Pact.Core.Names
import Pact.Core.Repl.Utils
import Pact.Core.IR.Desugar
import Pact.Core.Errors
import Pact.Core.IR.Term
import Pact.Core.Compile
import Pact.Core.Interpreter
import Pact.Core.Environment


import Pact.Core.IR.Eval.Runtime
import Pact.Core.IR.Eval.CEK(eval)
import Pact.Core.IR.Eval.RawBuiltin
import Pact.Core.Repl.Runtime.ReplBuiltin
import Pact.Core.Hash
import Pact.Core.PactValue

import qualified Pact.Core.Syntax.ParseTree as Lisp
import qualified Pact.Core.Syntax.Lexer as Lisp
import qualified Pact.Core.Syntax.Parser as Lisp

-- Small internal debugging function for playing with file loading within
-- this module
data ReplCompileValue
  = RCompileValue (CompileValue ReplRawBuiltin)
  | RLoadedDefun Text
  | RLoadedDefConst Text
  deriving Show

loadFile
  :: FilePath
  -> (ReplCompileValue -> ReplM ReplRawBuiltin ())
  -> ReplM ReplRawBuiltin [ReplCompileValue]
loadFile loc display = do
  source <- SourceCode (takeFileName loc) <$> liftIO (B.readFile loc)
  replCurrSource .= source
  interpretReplProgram source display

defaultEvalEnv :: PactDb b i -> M.Map Text b -> EvalEnv b i
defaultEvalEnv pdb =
  EvalEnv mempty pdb (EnvData mempty) defaultPactHash def Nothing Transactional mempty

interpretReplProgram
  :: SourceCode
  -> (ReplCompileValue -> ReplM ReplRawBuiltin ())
  -> ReplM ReplRawBuiltin [ReplCompileValue]
interpretReplProgram (SourceCode _ source) display = do
  lexx <- liftEither (Lisp.lexer source)
  debugIfFlagSet ReplDebugLexer lexx
  parsed <- liftEither $ Lisp.parseReplProgram lexx
  concat <$> traverse pipe parsed
  where
  displayValue p = p <$ display p
  pipe = \case
    Lisp.RTL rtl ->
      pure <$> pipe' rtl
    Lisp.RTLReplSpecial rsf -> case rsf of
      Lisp.ReplLoad txt b _
        | b -> do
          oldSrc <- use replCurrSource
          evalState .= def
          pactdb <- liftIO mockPactDb
          replPactDb .= pactdb
          replEvalEnv .= defaultEvalEnv pactdb replRawBuiltinMap
          out <- loadFile (T.unpack txt) display
          replCurrSource .= oldSrc
          pure out
        | otherwise -> do
          oldSrc <- use replCurrSource
          oldEs <- use evalState
          oldEE <- use replEvalEnv
          when b $ evalState .= def
          out <- loadFile (T.unpack txt) display
          replEvalEnv .= oldEE
          evalState .= oldEs
          replCurrSource .= oldSrc
          pure out
  pipe' tl = case tl of
    Lisp.RTLTopLevel toplevel -> do
      v <- interpretTopLevel (Interpreter interpretExpr interpretGuard) toplevel
      displayValue (RCompileValue v)
    _ ->  do
      ds <- runDesugarReplTopLevel tl
      interpret ds
  interpretGuard i g = do
    pdb <- viewEvalEnv eePactDb
    ps <- viewEvalEnv eePactStep
    let env = CEKEnv mempty pdb replBuiltinEnv ps False
    ev <- coreEnforceGuard i (RBuiltinWrap RawEnforceGuard) Mt CEKNoHandler env [VGuard g]
    case ev of
      VError txt _ ->
        throwError (PEExecutionError (EvalError txt) i)
      EvalValue v -> do
        case v of
          VClosure{} -> do
            pure IPClosure
          VTable tv -> pure (IPTable (_tvName tv))
          VPactValue pv -> do
            pure (IPV pv i)

  interpretExpr term = do
    pdb <- use (replEvalEnv . eePactDb)
    let builtins = replBuiltinEnv
    ps <- viewEvalEnv eePactStep
    let cekEnv = CEKEnv mempty pdb builtins ps False
    eval cekEnv term >>= \case
      VError txt _ ->
        throwError (PEExecutionError (EvalError txt) (view termInfo term))
      EvalValue v -> do
        case v of
          VClosure{} -> do
            pure IPClosure
          VTable tv -> pure (IPTable (_tvName tv))
          VPactValue pv -> do
            pure (IPV pv (view termInfo term))
  interpret (DesugarOutput tl _deps) = do
    case tl of
      RTLDefun df -> do
        let fqn = FullyQualifiedName replModuleName (_dfunName df) replModuleHash
        loaded . loAllLoaded %= M.insert fqn (Dfun df)
        displayValue $ RLoadedDefun $ _dfunName df
      RTLDefConst dc -> do
        let fqn = FullyQualifiedName replModuleName (_dcName dc) replModuleHash
        loaded . loAllLoaded %= M.insert fqn (DConst dc)
        displayValue $ RLoadedDefConst $ _dcName dc
