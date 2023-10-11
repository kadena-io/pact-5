{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pact.Core.Repl.Runtime.ReplBuiltin where

import Control.Lens
import Control.Monad.Except
import Control.Monad.IO.Class(liftIO)
import Data.Default
import Data.Text(Text)
import Data.ByteString.Short(toShort)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V


import Pact.Core.Builtin
import Pact.Core.Literal
import Pact.Core.Hash
import Pact.Core.IR.Eval.Runtime
import Pact.Core.Pacts.Types
import Pact.Core.IR.Eval.CEK
import Pact.Core.Names
import Pact.Core.IR.Eval.RawBuiltin
import Pact.Core.Pretty
import Pact.Core.Environment
import Pact.Core.PactValue
import Pact.Core.Gas
import Pact.Core.Guards
import Pact.Core.Capabilities
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



continuePact :: forall b i. (IsBuiltin b, Default i) => NativeFunction b i (ReplEvalM b i)
continuePact info b _cont _handler _env = \case
  [VLiteral (LInteger s)] -> go s False Nothing Nothing
    -- do
    -- useEvalState esPactExec >>= \case
    --   Nothing -> pure (VError "No pact exec environment found!")
    --   Just pe -> lookupFqName (pe ^. peContinuation . pcName) >>= \case
    --     Just (DPact dp)
    --       | s == toInteger (_peStep pe) + 1 &&
    --         s < toInteger (_peStepCount pe) -> do
    --           let
    --             step = _dpSteps dp NE.!! fromInteger s
    --             args' = VPactValue <$> pe ^. peContinuation . pcArgs
    --             toClosure = \case
    --               Lam _li args body i ->
    --                 applyLam (C (Closure undefined undefined (_argType <$> args) (NE.length args) body Nothing env i)) args' Mt CEKNoHandler
    --               _ -> error "invariant violation"
    --           v <- case step of
    --             Step s' _ -> toClosure s'
    --             StepWithRollback s' _rb _ -> toClosure s'
    --           setEvalState esPactExec (Just $ over peStep (+1) pe)
    --           returnCEK (PactStepC cont undefined) handler v
    --       | otherwise ->
    --         -- throwExecutionError info (ContinuePactInvalidContext s (toInteger (_peStep pe)) (toInteger (_peStepCount pe)))
    --         pure (VError "")
    --     _ -> pure (VError "continuation is not a defpact")
  args -> argsError info b args
  where
    go :: Integer -> Bool -> Maybe Text -> Maybe (M.Map Field PactValue) -> ReplEvalM b i (EvalResult b i (ReplEvalM b i))
    go step rollback mpid userResume = useEvalState esPactExec >>= \case
      -- If we try to execute `continue-pact`, we first check if we have a running
      -- `PactExec` in the `EvalState` environment.
      Nothing -> do
        case mpid of
          -- In case, there is no `PactExec` AND we have no user-specified `PactId`, we abort
          -- abort the execution.
          Nothing -> error "continue-pact: No pact id supplied and no pact exec in context"
          Just pid -> do
            -- If we do have a user-specified `PactId`, we can resume the
            -- execution of the `DefPact`.
            let
              pactId = PactId pid
              pactYield = Yield <$> userResume
              pactStep = PactStep (fromInteger step) rollback pactId pactYield

            setEvalState esPactExec Nothing
            (reEnv . eePactStep) .= Just pactStep
            pure (EvalValue (VObject (M.fromList [])))
      Just _ -> pure (EvalValue (VObject (M.fromList [])))


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
        in pure (EvalValue $ VObject (M.fromList ps))
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

envEvents :: (IsBuiltin b, Default i) => NativeFunction b i (ReplEvalM b i)
envEvents =  \info b cont handler _env -> \case
  [_] -> do
    events <- useEvalState esEvents
    liftIO $ print events
    returnCEKValue cont handler VUnit
  args -> argsError info b args

envHash :: (IsBuiltin b, Default i) => NativeFunction b i (ReplEvalM b i)
envHash =  \info b cont handler _env -> \case
  [VString s] -> do
    case decodeBase64UrlUnpadded (T.encodeUtf8 s) of
      Left e -> returnCEK cont handler (VError (T.pack e))
      Right hs -> do
        (reEnv . eeHash) .= Hash (toShort hs)
        returnCEKValue cont handler VUnit
  args -> argsError info b args

envData :: (IsBuiltin b, Default i) => NativeFunction b i (ReplEvalM b i)
envData = \info b cont handler _env -> \case
  [VObject o] -> do
    let ed = EnvData o
    (reEnv . eeMsgBody) .= ed
    returnCEKValue cont handler VUnit
  args -> argsError info b args

envChainData :: (IsBuiltin b, Default i) => NativeFunction b i (ReplEvalM b i)
envChainData = \info b cont handler _env -> \case
  [VObject cdataObj] -> do
    pd <- viewCEKEnv eePublicData
    go pd (M.toList cdataObj)
    where
    go pd [] = do
      reEnv . eePublicData .= pd
      returnCEKValue cont handler VUnit
    go pd ((k,v):rest) = case v of
      PInteger i
        | k == cdGasLimit ->
          go (set (pdPublicMeta . pmGasLimit) (Gas (fromIntegral i)) pd) rest
        | k == cdBlockHeight ->
          go (set pdBlockHeight (fromInteger i) pd) rest
      PDecimal i
        | k == cdGasPrice ->
          go (set (pdPublicMeta . pmGasPrice) (toRational i) pd) rest
      PString s
        | k == cdChainId ->
          go (set (pdPublicMeta . pmChainId) (ChainId s) pd) rest
        | k == cdSender ->
          go (set (pdPublicMeta . pmSender) s pd) rest
        | k == cdPrevBlockHash ->
          go (set pdPrevBlockHash s pd) rest
      _ -> returnCEK cont handler (VError $ "envChainData: bad public metadata value for key: " <> _field k)
  args -> argsError info b args

envKeys :: (IsBuiltin b, Default i) => NativeFunction b i (ReplEvalM b i)
envKeys = \info b cont handler _env -> \case
  [VList ks] -> do
    keys <- traverse (asString info b) ks
    reEnv . eeMsgSigs .= M.fromList ((,mempty) . PublicKeyText <$> V.toList keys)
    returnCEKValue cont handler VUnit
  args -> argsError info b args

envSigs :: (IsBuiltin b, Default i) => NativeFunction b i (ReplEvalM b i)
envSigs = \info b cont handler _env -> \case
  [VList ks] ->
    case traverse keyCapObj ks of
      Just sigs -> do
        (reEnv . eeMsgSigs) .= M.fromList (V.toList sigs)
        returnCEKValue cont handler VUnit
      Nothing -> returnCEK cont handler (VError "env-sigs format is wrong")
    where
    keyCapObj = \case
      PObject o -> do
        keyRaw<- M.lookup (Field "key") o
        kt <- preview (_PLiteral . _LString) keyRaw
        capsRaw <- M.lookup (Field "caps") o
        capsListPV <- preview _PList capsRaw
        caps <- traverse (preview _PCapToken) capsListPV
        let cts = over ctName fqnToQualName <$> caps
        pure (PublicKeyText kt, S.fromList (V.toList cts))
      _ -> Nothing
  args -> argsError info b args

-- loadExec :: (IsBuiltin b, Default i) => NativeFunction b i (ReplEvalM b i)
-- loadExec = \info b cont handler _env -> \case
--   [VString s, VBool reset] -> do
--     es <- use evalState
--     when reset $ evalState .= def

--   args -> argsError info b args




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
    REnvChainData -> envChainData
    REnvData -> envData
    REnvEvents -> envEvents
    REnvHash -> envHash
    REnvKeys -> envKeys
    REnvSigs -> envSigs
    RBeginTx -> undefined
    RCommitTx -> undefined
    RRollbackTx -> undefined
    -- RLoad -> loadExec
    -- RLoadWithEnv -> loadExec
