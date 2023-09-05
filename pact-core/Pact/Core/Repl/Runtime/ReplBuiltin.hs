{-# LANGUAGE BlockArguments #-}

module Pact.Core.Repl.Runtime.ReplBuiltin where

import Control.Monad.Except
import Control.Monad.IO.Class(liftIO)
import Data.Default
import Data.Text(Text)
import qualified Data.Text as T
import Control.Lens

import Pact.Core.Builtin
import Pact.Core.Literal
-- import Pact.Core.Gas
-- import Pact.Core.Errors

import Pact.Core.IR.Eval.Runtime
import Pact.Core.IR.Term
import Pact.Core.IR.Eval.CEK
import Pact.Core.IR.Eval.RawBuiltin(rawBuiltinLiftedRuntime, prettyShowValue)
import qualified Pact.Core.IR.Eval.RawBuiltin as RawBuiltin


import Pact.Core.Repl.Runtime

type ReplBM b i = ReplEvalM (ReplBuiltin b) i
type ReplCont b i = Cont (ReplBuiltin b) i (ReplBM b i)
type ReplHandler b i = CEKErrorHandler (ReplBuiltin b) i (ReplBM b i)
type ReplCEKValue b i = CEKValue (ReplBuiltin b) i (ReplBM b i)
type ReplEvalResult b i = EvalResult (ReplBuiltin b) i (ReplBM b i)
type ReplBuiltinFn b i = NativeFn (ReplBuiltin b) i (ReplBM b i)

asBool :: MonadEval b i m => CEKValue b i m -> m Bool
asBool (VLiteral (LBool b)) = pure b
asBool _ = failInvariant "asBool"

asString :: MonadEval b i m => CEKValue b i m -> m Text
asString (VLiteral (LString b)) = pure b
asString _ = failInvariant "asString"

-- Show functions injected by the compiler are
-- not a recoverable thing from an invariant standpoint
enforceValue
  :: MonadEval b i m
  => EvalResult b i m
  -> m (CEKValue b i m)
enforceValue = \case
  EvalValue v -> pure v
  _ -> failInvariant "Error"

-- tryError :: MonadError a m => m b -> m (Either a b)
-- tryError ma = catchError (Right <$> ma) (pure . Left)

mkReplBuiltinFn
  :: (BuiltinArity b)
  => i
  -> (ReplCont b i -> ReplHandler b i -> [ReplCEKValue b i] -> ReplBM b i (ReplEvalResult b i))
  -> ReplBuiltin b
  -> ReplBuiltinFn b i
mkReplBuiltinFn = mkBuiltinFn
{-# INLINE mkReplBuiltinFn #-}

corePrint :: (BuiltinArity b, Default i) => i -> ReplBuiltin b -> ReplBuiltinFn b i
corePrint info = mkReplBuiltinFn info \cont handler -> \case
  [v] -> do
    liftIO $ putStrLn $ T.unpack (prettyShowValue v)
    returnCEKValue cont handler (VLiteral LUnit)
  _ -> failInvariant "Print"

-- coreExpect :: (BuiltinArity b, Default i) => ReplBuiltin b -> i -> ReplBuiltinFn b i
-- coreExpect = mkReplBuiltinFn \cont handler -> \case
--   [VLiteral (LString msg), v1, clo@VClosure{}] -> do
--     unsafeApplyOne clo (VLiteral LUnit) >>= \case
--        EvalValue v2 -> do
--         unsafeApplyTwo eqFn v1 v2 >>= enforceValue >>= asBool >>= \case
--           False -> do
--             v1s <- asString =<< enforceValue =<< unsafeApplyOne showFn v1
--             v2s <- asString =<< enforceValue =<< unsafeApplyOne showFn v2
--             returnCEKValue cont handler $ VLiteral $ LString $ "FAILURE: " <> msg <> " expected: " <> v1s <> ", received: " <> v2s
--           True -> returnCEKValue cont handler (VLiteral (LString ("Expect: success " <> msg)))
--        v -> returnCEK cont handler v
--   e -> failInvariant $ "Expect" <> T.pack (show e)

rawExpect :: (BuiltinArity b, Default i) => i -> ReplBuiltin b -> ReplBuiltinFn b i
rawExpect info = mkReplBuiltinFn info \cont handler -> \case
  [VLiteral (LString msg), VPactValue v1, clo@VClosure{}] ->
    unsafeApplyOne clo (VLiteral LUnit) >>= \case
       EvalValue (VPactValue v2) ->
        if v1 /= v2 then do
            let v1s = RawBuiltin.prettyShowValue (VPactValue v1)
                v2s = RawBuiltin.prettyShowValue (VPactValue v2)
            returnCEKValue cont handler $ VLiteral $ LString $ "FAILURE: " <> msg <> " expected: " <> v1s <> ", received: " <> v2s
        else returnCEKValue cont handler (VLiteral (LString ("Expect: success " <> msg)))
       v -> returnCEK cont handler v
  _ -> failInvariant "Expect"

coreExpectThat :: (BuiltinArity b, Default i) => i -> ReplBuiltin b -> ReplBuiltinFn b i
coreExpectThat info = mkReplBuiltinFn info \cont handler -> \case
  [VLiteral (LString msg), vclo, v] -> do
    unsafeApplyOne vclo v >>= \case
      EvalValue (VLiteral (LBool b)) ->
        if b then returnCEKValue cont handler (VLiteral (LString ("Expect-that: success " <> msg)))
        else returnCEKValue cont handler  (VLiteral (LString ("FAILURE: Expect-that: Did not satisfy condition: " <> msg)))
      EvalValue _ -> failInvariant "Expect"
      ve@VError{} -> returnCEK cont handler ve
  _ -> failInvariant "Expect"

coreExpectFailure :: (BuiltinArity b, Default i) => i -> ReplBuiltin b -> ReplBuiltinFn b i
coreExpectFailure info = mkReplBuiltinFn info \cont handler -> \case
  [VLiteral (LString toMatch), vclo] -> do
    tryError (unsafeApplyOne vclo (VLiteral LUnit)) >>= \case
      Right (VError _e) ->
        returnCEKValue cont handler $ VLiteral $ LString $ "Expect failure: Success: " <> toMatch
      Left _err -> do
        returnCEKValue cont handler $ VLiteral $ LString $ "Expect failure: Success: " <> toMatch
      Right _ ->
        returnCEKValue cont handler $ VLiteral $ LString $ "FAILURE: " <> toMatch <> ": expected failure, got result"
  _ -> failInvariant "Expect-failure"

continuePact :: (BuiltinArity b, Default i) => i -> ReplBuiltin b -> ReplBuiltinFn b i
continuePact info = mkReplBuiltinFn info \cont handler -> \case
  [VLiteral (LInteger s)] -> do
    useEvalState esPactExec >>= \case
      Nothing -> pure (VError "No pact exec environment found!")
      Just pe -> lookupFqName (pe ^. peContinuation . pcDef) >>= \case
        Just (DPact d) ->
          let cont' = undefined
          in evalCEK cont' handler undefined undefined
        _ -> pure (VError "continuation is not a defpact")
  _ -> failInvariant "continue-pact"

replRawBuiltinRuntime
  :: (Default i)
  => i
  -> ReplBuiltin RawBuiltin
  -> ReplBuiltinFn RawBuiltin i
replRawBuiltinRuntime i = \case
  RBuiltinWrap cb ->
    rawBuiltinLiftedRuntime RBuiltinWrap i cb
  RExpect -> rawExpect i RExpect
  RExpectFailure -> coreExpectFailure i RExpectFailure
  RExpectThat -> coreExpectThat i RExpectThat
  RPrint -> corePrint i RPrint
  RContinuePact -> continuePact i RContinuePact

-- defaultReplState :: Default i => ReplEvalState (ReplBuiltin RawBuiltin) i
-- defaultReplState = ReplEvalState env (EvalState (CapState [] mempty) [] [] False)
--   where
--   env =
--     EvalEnv
--     { _cekBuiltins = replRawBuiltinRuntime
--     , _cekLoaded = mempty
--     , _cekGasModel = freeGasEnv
--     , _cekMHashes = mempty
--     , _cekMsgSigs = mempty }
