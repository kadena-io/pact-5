{-# LANGUAGE BlockArguments #-}

module Pact.Core.Repl.Runtime.ReplBuiltin where

import Control.Monad.Except
import Data.Default
import Data.Text(Text)
import qualified Data.Text as T

import Pact.Core.Builtin
import Pact.Core.Literal
import Pact.Core.Gas
-- import Pact.Core.Errors

import Pact.Core.Untyped.Eval.Runtime
import Pact.Core.Untyped.Eval.CEK
import Pact.Core.Untyped.Eval.Runtime.CoreBuiltin

import Pact.Core.Repl.Runtime

type ReplBM b i = ReplEvalM (ReplBuiltin b) i
type ReplCont b i = Cont (ReplBuiltin b) i (ReplBM b i)
type ReplHandler b i = CEKErrorHandler (ReplBuiltin b) i (ReplBM b i)
type ReplCEKValue b i = CEKValue (ReplBuiltin b) i (ReplBM b i)
type ReplEvalResult b i = EvalResult (ReplBuiltin b) i (ReplBM b i)
type ReplBuiltinFn b i = BuiltinFn (ReplBuiltin b) i (ReplBM b i)

asBool :: MonadCEK b i m => CEKValue b i m -> m Bool
asBool (VLiteral (LBool b)) = pure b
asBool _ = failInvariant "asBool"

asString :: MonadCEK b i m => CEKValue b i m -> m Text
asString (VLiteral (LString b)) = pure b
asString _ = failInvariant "asString"

-- Show functions injected by the compiler are
-- not a recoverable thing from an invariant standpoint
enforceValue
  :: MonadCEK b i m
  => EvalResult b i m
  -> m (CEKValue b i m)
enforceValue = \case
  EvalValue v -> pure v
  _ -> failInvariant "Error"

tryError :: MonadError a m => m b -> m (Either a b)
tryError ma = catchError (Right <$> ma) (pure . Left)

mkReplBuiltinFn
  :: (BuiltinArity b)
  => (ReplCont b i -> ReplHandler b i -> [ReplCEKValue b i] -> ReplBM b i (ReplEvalResult b i))
  -> ReplBuiltin b
  -> ReplBuiltinFn b i
mkReplBuiltinFn fn b =
  BuiltinFn b fn (builtinArity b) []
{-# INLINE mkReplBuiltinFn #-}

corePrint :: (BuiltinArity b, Default i) => ReplBuiltin b -> ReplBuiltinFn b i
corePrint = mkReplBuiltinFn \cont handler -> \case
  [showInst, v] -> do
    unsafeApplyOne showInst v >>= enforceValue >>= \case
     VLiteral (LString showed) -> do
        liftIO $ putStrLn $ T.unpack showed
        returnCEKValue cont handler (VLiteral LUnit)
     _ -> failInvariant "Print"
  _ -> failInvariant "Print"

coreExpect :: (BuiltinArity b, Default i) => ReplBuiltin b -> ReplBuiltinFn b i
coreExpect = mkReplBuiltinFn \cont handler -> \case
  [eqFn, showFn, VLiteral (LString msg), v1, clo@VClosure{}] -> do
    unsafeApplyOne clo (VLiteral LUnit) >>= \case
       EvalValue v2 -> do
        unsafeApplyTwo eqFn v1 v2 >>= enforceValue >>= asBool >>= \case
          False -> do
            v1s <- asString =<< enforceValue =<< unsafeApplyOne showFn v1
            v2s <- asString =<< enforceValue =<< unsafeApplyOne showFn v2
            returnCEKValue cont handler $ VLiteral $ LString $ "FAILURE: " <> msg <> " expected: " <> v1s <> ", received: " <> v2s
          True -> returnCEKValue cont handler (VLiteral (LString ("Expect: success " <> msg)))
       v -> returnCEK cont handler v
  _ -> failInvariant "Expect"

coreExpectThat :: (BuiltinArity b, Default i) => ReplBuiltin b -> ReplBuiltinFn b i
coreExpectThat = mkReplBuiltinFn \cont handler -> \case
  [VLiteral (LString msg), vclo, v] -> do
    unsafeApplyOne vclo v >>= \case
      EvalValue (VLiteral (LBool b)) ->
        if b then returnCEKValue cont handler (VLiteral (LString ("Expect-that: success " <> msg)))
        else returnCEKValue cont handler  (VLiteral (LString ("FAILURE: Expect-that: Did not satisfy condition: " <> msg)))
      EvalValue _ -> failInvariant "Expect"
      ve@VError{} -> returnCEK cont handler ve
  _ -> failInvariant "Expect"

coreExpectFailure :: (BuiltinArity b, Default i) => ReplBuiltin b -> ReplBuiltinFn b i
coreExpectFailure = mkReplBuiltinFn \cont handler -> \case
  [VLiteral (LString toMatch), vclo] -> do
    tryError (unsafeApplyOne vclo (VLiteral LUnit)) >>= \case
      Right (VError _e) ->
        returnCEKValue cont handler $ VLiteral $ LString $ "Expect failure: Success: " <> toMatch
      Left _err -> do
        returnCEKValue cont handler $ VLiteral $ LString $ "Expect failure: Success: " <> toMatch
      Right _ ->
        returnCEKValue cont handler $ VLiteral $ LString $ "FAILURE: " <> toMatch <> ": expected failure, got result"
  _ -> failInvariant "Expect-failure"



replCoreBuiltinRuntime
  :: (Default i)
  => ReplBuiltin CoreBuiltin
  -> ReplBuiltinFn CoreBuiltin i
replCoreBuiltinRuntime = \case
  RBuiltinWrap cb ->
    coreBuiltinLiftedRuntime RBuiltinWrap cb
  RExpect -> coreExpect RExpect
  RExpectFailure -> coreExpectFailure RExpectFailure
  RExpectThat -> coreExpectThat RExpectThat
  RPrint -> corePrint RPrint

defaultReplState :: Default i => ReplEvalState (ReplBuiltin CoreBuiltin) i
defaultReplState = ReplEvalState $
  CEKRuntimeEnv
  { _cekBuiltins = replCoreBuiltinRuntime
  , _cekLoaded = mempty
  , _cekGasModel = freeGasEnv }
