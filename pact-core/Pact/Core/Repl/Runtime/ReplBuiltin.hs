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
import Pact.Core.Errors

import Pact.Core.IR.Eval.Runtime
import Pact.Core.IR.Term
import Pact.Core.Type
import Pact.Core.Pacts.Types
import Pact.Core.IR.Eval.CEK
import Pact.Core.Names
import Pact.Core.IR.Eval.RawBuiltin
import Pact.Core.Pretty

import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (fromList)
import Pact.Core.PactValue


import Pact.Core.Repl.Runtime

type ReplBM b i = ReplEvalM (ReplBuiltin b) i
type ReplCont b i = Cont (ReplBuiltin b) i (ReplBM b i)
type ReplHandler b i = CEKErrorHandler (ReplBuiltin b) i (ReplBM b i)
type ReplCEKValue b i = CEKValue (ReplBuiltin b) i (ReplBM b i)
type ReplEvalResult b i = EvalResult (ReplBuiltin b) i (ReplBM b i)
type ReplBuiltinFn b i = NativeFn (ReplBuiltin b) i (ReplBM b i)

prettyShowValue :: CEKValue b i m -> Text
prettyShowValue = \case
  -- Todo: REMOVE THIS. THIS CANNOT MAKE IT INTO OUTPUTS.
  VPactValue p -> renderText p
  VTable (TableName tn) _ _ _ -> "table{" <> tn <> "}"
  VClosure _ -> "<#closure>"


-- mkReplBuiltinFn
--   :: (IsBuiltin b)
--   => i
--   -> ReplBuiltin b
--   -> (ReplCont b i -> ReplHandler b i -> [ReplCEKValue b i] -> ReplBM b i (ReplEvalResult b i))
--   -> ReplBuiltinFn b i
-- mkReplBuiltinFn = mkBuiltinFn
-- {-# INLINE mkReplBuiltinFn #-}

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



continuePact :: (IsBuiltin b, Default i) => NativeFunction b i (ReplEvalM b i)
continuePact = \info b cont handler env -> \case
  [VLiteral (LInteger s)] -> do
    useEvalState esPactExec >>= \case
      Nothing -> pure (VError "No pact exec environment found!")
      Just pe -> lookupFqName (pe ^. peContinuation . pcName) >>= \case
        Just (DPact dp)
          | s == toInteger (_peStep pe) + 1 &&
            s < toInteger (_peStepCount pe) -> do
              let
                step = _dpSteps dp NE.!! fromInteger s
                args' = VPactValue <$> pe ^. peContinuation . pcArgs
                toClosure = \case
                  Lam li args body i ->
                    applyLam (C (Closure li (_argType <$> args) (NE.length args) body Nothing env i)) args' Mt CEKNoHandler
                  _ -> error "invariant violation"
              v <- case step of
                Step s' _ -> toClosure s'
                StepWithRollback s' _rb _ -> toClosure s'
              setEvalState esPactExec (Just $ over peStep (+1) pe)
              returnCEK (PactStepC cont) handler v
          | otherwise ->
            throwExecutionError info (ContinuePactInvalidContext s (toInteger (_peStep pe)) (toInteger (_peStepCount pe)))
        _ -> pure (VError "continuation is not a defpact")
  args -> argsError info b args

pactState :: (IsBuiltin b, Default i) => NativeFunction b i (ReplEvalM b i)
pactState = \ info b _cont _handler _env -> \case
  [_] -> do
    mpe <- useEvalState esPactExec
    mRet <- case mpe of
      Just pe -> case _peYield pe of
        Nothing -> pure (Just $ PLiteral (LBool False))
        Just (Yield y) -> pure (Just $ PObject y)
      Nothing -> pure Nothing
    case mRet of
      Nothing -> pure (EvalValue $ VString "No Pact State available")
      Just yield ->
        let ps = [(Field "pactId", PLiteral (LString ""))
                 ,(Field "yield", yield)]
        in pure (EvalValue $ VObject (fromList ps))
  args -> argsError info b args

resetPactState :: (IsBuiltin b, Default i) => NativeFunction b i (ReplEvalM b i)
resetPactState = \info b _cont _handler _env -> \case
  [_] -> setEvalState esPactExec Nothing >> pure (EvalValue $ VString "Resetted Pact State")
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
    RContinuePact -> continuePact
    RPactState -> pactState
    RResetPactState -> resetPactState

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
