{-# LANGUAGE BlockArguments #-}

module Pact.Core.Repl.Runtime.ReplBuiltin where

import Control.Monad.Except
import Control.Monad.IO.Class(liftIO)
import Data.Default
import Data.Text(Text)
import qualified Data.Text as T

import Pact.Core.Builtin
import Pact.Core.Literal
-- import Pact.Core.Gas
-- import Pact.Core.Errors

import Pact.Core.IR.Eval.Runtime
import Pact.Core.IR.Eval.CEK
import Pact.Core.Names
import Pact.Core.IR.Eval.RawBuiltin
import Pact.Core.Pretty


import Pact.Core.Repl.Runtime

type ReplBM b i = ReplEvalM (ReplBuiltin b) i
type ReplCont b i = Cont (ReplBuiltin b) i (ReplBM b i)
type ReplHandler b i = CEKErrorHandler (ReplBuiltin b) i (ReplBM b i)
type ReplCEKValue b i = CEKValue (ReplBuiltin b) i (ReplBM b i)
type ReplEvalResult b i = EvalResult (ReplBuiltin b) i (ReplBM b i)
type ReplBuiltinFn b i = NativeFn (ReplBuiltin b) i (ReplBM b i)

prettyShowValue :: CEKValue b i m -> Text
prettyShowValue = \case
  VPactValue p -> renderText p
  VTable (TableValue (TableName tn) _ _ _) -> "table{" <> tn <> "}"
  VClosure _ -> "<#closure>"

corePrint :: (IsBuiltin b, Default i) => NativeFunction b i (ReplEvalM b i)
corePrint = \info b cont handler _env -> \case
  [v] -> do
    liftIO $ putStrLn $ T.unpack (prettyShowValue v)
    returnCEKValue cont handler (VLiteral LUnit)
  args -> argsError info b args

rawExpect :: (IsBuiltin b, Default i) => NativeFunction b i (ReplEvalM b i)
rawExpect = \info b cont handler _env -> \case
  [VLiteral (LString msg), VPactValue v1, VClosure clo] ->
    unsafeApplyOne clo (VLiteral LUnit) >>= \case
       EvalValue (VPactValue v2) ->
        if v1 /= v2 then do
            let v1s = prettyShowValue (VPactValue v1)
                v2s = prettyShowValue (VPactValue v2)
            returnCEKValue cont handler $ VLiteral $ LString $ "FAILURE: " <> msg <> " expected: " <> v1s <> ", received: " <> v2s
        else returnCEKValue cont handler (VLiteral (LString ("Expect: success " <> msg)))
       v -> returnCEK cont handler v
  args -> argsError info b args

coreExpectThat :: (IsBuiltin b, Default i) => NativeFunction b i (ReplEvalM b i)
coreExpectThat = \info b cont handler _env -> \case
  [VLiteral (LString msg), VClosure vclo, v] -> do
    unsafeApplyOne vclo v >>= \case
      EvalValue (VLiteral (LBool c)) ->
        if c then returnCEKValue cont handler (VLiteral (LString ("Expect-that: success " <> msg)))
        else returnCEKValue cont handler  (VLiteral (LString ("FAILURE: Expect-that: Did not satisfy condition: " <> msg)))
      EvalValue _ -> return (VError "Expect-that: condition did not return a boolean")
      VError ve -> return (VError ve)
  args -> argsError info b args

coreExpectFailure :: (IsBuiltin b, Default i) => NativeFunction b i (ReplEvalM b i)
coreExpectFailure = \info b cont handler _env -> \case
  [VLiteral (LString toMatch), VClosure vclo] -> do
    tryError (unsafeApplyOne vclo (VLiteral LUnit)) >>= \case
      Right (VError _e) ->
        returnCEKValue cont handler $ VLiteral $ LString $ "Expect failure: Success: " <> toMatch
      Left _err -> do
        returnCEKValue cont handler $ VLiteral $ LString $ "Expect failure: Success: " <> toMatch
      Right _ ->
        returnCEKValue cont handler $ VLiteral $ LString $ "FAILURE: " <> toMatch <> ": expected failure, got result"
  args -> argsError info b args

coreEnvStackFrame :: (IsBuiltin b, Default i) => NativeFunction b i (ReplEvalM b i)
coreEnvStackFrame = \info b cont handler _env -> \case
  [_] -> do
    frames <- useEvalState esStack
    liftIO $ print frames
    returnCEKValue cont handler VUnit
  args -> argsError info b args

replBuiltinEnv
  :: Default i
  => BuiltinEnv (ReplBuiltin RawBuiltin) i (ReplEvalM (ReplBuiltin RawBuiltin) i)
replBuiltinEnv i b env =
  mkBuiltinFn i b env (replRawBuiltinRuntime b)

replRawBuiltinRuntime
  :: (Default i)
  => ReplBuiltin RawBuiltin
  -> NativeFunction (ReplBuiltin RawBuiltin) i (ReplEvalM (ReplBuiltin RawBuiltin) i)
replRawBuiltinRuntime = \case
  RBuiltinWrap cb ->
    rawBuiltinRuntime cb
  RBuiltinRepl br -> case br of
    RExpect -> rawExpect
    RExpectFailure -> coreExpectFailure
    RExpectThat -> coreExpectThat
    RPrint -> corePrint
    REnvStackFrame -> coreEnvStackFrame
