{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}


module Pact.Core.Evaluate where

import Control.Lens
import Control.Monad.Except
import Data.Default
import Data.Text (Text)

import Pact.Core.Builtin
import Pact.Core.Compile
import Pact.Core.Environment
import Pact.Core.Errors
import Pact.Core.Info
import Pact.Core.Interpreter
import Pact.Core.IR.Eval.RawBuiltin
import Pact.Core.IR.Eval.Runtime
import Pact.Core.IR.Term
import Pact.Core.Persistence
import qualified Pact.Core.IR.Eval.CEK as Eval
import qualified Pact.Core.Syntax.Lexer as Lisp
import qualified Pact.Core.Syntax.Parser as Lisp

evaluate
  :: EvalEnv RawBuiltin SpanInfo
  -> Text
  -> IO (Either (PactError SpanInfo) [CompileValue RawBuiltin],
         EvalState RawBuiltin SpanInfo)
evaluate evalEnv source = runEvalM evalEnv def $ do
  lexx <- liftEither (Lisp.lexer source)
  parsed <- liftEither $ Lisp.parseProgram lexx
  traverse (interpretTopLevel $ Interpreter interpretExpr interpretGuard) parsed

  where
  interpretGuard i g = do
    let pdb = _eePactDb evalEnv
    let env = CEKEnv mempty pdb (rawBuiltinEnv @CEKBigStep) (_eeDefPactStep evalEnv) False
    ev <- coreEnforceGuard i RawEnforceGuard Mt CEKNoHandler env [VGuard g]
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

  interpretExpr purity term = do
    let builtins = rawBuiltinEnv @CEKBigStep
    let pdb = _eePactDb evalEnv
    let cekEnv = fromPurity $ CEKEnv mempty pdb builtins (_eeDefPactStep evalEnv) False
    Eval.eval cekEnv term >>= \case
      VError txt _ ->
        throwError (PEExecutionError (EvalError txt) (view termInfo term))
      EvalValue v -> do
        case v of
          VClosure{} -> do
            pure IPClosure
          VTable tv -> pure (IPTable (_tvName tv))
          VPactValue pv -> do
            pure (IPV pv (view termInfo term))
    where
    fromPurity env = case purity of
      PSysOnly -> sysOnlyEnv env
      PReadOnly -> readOnlyEnv env
      PImpure -> env
