{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pact.Core.Repl.Runtime.ReplBuiltin where

import Control.Lens hiding ((%%=))
import Control.Monad(when)
import Control.Monad.Except
import Control.Monad.IO.Class(liftIO)
import Data.Default
import Data.Text(Text)
import Data.ByteString.Short(toShort)
import Data.Foldable(find)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.RAList as RAList
import qualified Data.List.NonEmpty as NE


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
import Pact.Core.Errors
import Pact.Core.Persistence
import Pact.Core.IR.Term
import Pact.Core.Type

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
    applyLam clo [] Mt CEKNoHandler >>= \case
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
    tryError (applyLam vclo [] Mt CEKNoHandler) >>= \case
      Right (VError _e) ->
        returnCEKValue cont handler $ VLiteral $ LString $ "Expect failure: Success: " <> toMatch
      Left _err -> do
        returnCEKValue cont handler $ VLiteral $ LString $ "Expect failure: Success: " <> toMatch
      Right _ ->
        returnCEKValue cont handler $ VLiteral $ LString $ "FAILURE: " <> toMatch <> ": expected failure, got result"
  [VString desc, VString toMatch, VClosure vclo] -> do
    tryError (applyLam vclo [] Mt CEKNoHandler) >>= \case
      Right (VError _e) ->
        returnCEKValue cont handler $ VLiteral $ LString $ "Expect failure: Success: " <> desc
      Left _err -> do
        returnCEKValue cont handler $ VLiteral $ LString $ "Expect failure: Success: " <> desc
      Right _ ->
        returnCEKValue cont handler $ VLiteral $ LString $ "FAILURE: " <> toMatch <> ": expected failure, got result"
  args -> argsError info b args



continuePact :: forall b i. (IsBuiltin b, Default i) => NativeFunction b i (ReplEvalM b i)
continuePact info b cont handler env = \case
  [VLiteral (LInteger s)] -> go s False Nothing Nothing
  args -> argsError info b args
  where
    go :: Integer -> Bool -> Maybe Text -> Maybe (M.Map Field PactValue) -> ReplEvalM b i (EvalResult b i (ReplEvalM b i))
    go step rollback mpid userResume = do
      mpe <- useEvalState esPactExec
      (pid, myield) <- case mpe of
        Nothing -> do
          pid <- maybe (error "") (pure . PactId) mpid
          pure (pid, Yield <$> userResume)
        Just pactExec ->
          let
            pid = maybe (_pePactId pactExec) PactId mpid
            yield = case userResume of
              Nothing -> _peYield pactExec
              Just o -> pure (Yield o)
          in pure (pid, yield)
      let pactStep = PactStep (fromInteger step) rollback pid myield
      setEvalState esPactExec Nothing

      (reEnv . eePactStep) .= Just pactStep
      resumePact info cont handler env Nothing

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
  [] -> do
    frames <- useEvalState esStack
    liftIO $ print frames
    returnCEKValue cont handler VUnit
  args -> argsError info b args

envEvents :: (IsBuiltin b, Default i) => NativeFunction b i (ReplEvalM b i)
envEvents =  \info b cont handler _env -> \case
  [] -> do
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

beginTx :: (IsBuiltin b, Default i) => NativeFunction b i (ReplEvalM b i)
beginTx = \info b cont handler _env -> \case
  [VString s] -> begin' info (Just s) >>= returnCEK cont handler . renderTx "Begin Tx"
  [] -> begin' info Nothing >>= returnCEK cont handler . renderTx "Begin Tx"
  args -> argsError info b args

renderTx :: Text -> Maybe (TxId, Maybe Text) -> EvalResult b i m
renderTx start (Just (TxId tid, mt)) =
  EvalValue $ VString $ start <> " " <> T.pack (show tid) <> maybe mempty ((<>) " ") mt
renderTx start Nothing = VError $ "tx-function failure " <> start

begin' :: (Default i) => i -> Maybe Text -> ReplEvalM b i (Maybe (TxId, Maybe Text))
begin' info mt = do
  pdb <- use (reEnv . eePactDb)
  mode <- viewCEKEnv eeMode
  mTxId <- liftDbFunction info (_pdbBeginTx pdb mode)
  reTx .= ((,mt) <$> mTxId)
  return ((,mt) <$> mTxId)

commitTx :: (IsBuiltin b, Default i) => NativeFunction b i (ReplEvalM b i)
commitTx = \info b cont handler _env -> \case
  [] -> do
    pdb <- use (reEnv . eePactDb)
    liftDbFunction info (_pdbCommitTx pdb)
    reState .= def
    use reTx >>= \case
      Just tx -> do
        reTx .= Nothing
        returnCEK cont handler (renderTx "Commit Tx" (Just tx))
      Nothing -> returnCEK cont handler (renderTx "Commit Tx" Nothing)
  args -> argsError info b args


rollbackTx :: (IsBuiltin b, Default i) => NativeFunction b i (ReplEvalM b i)
rollbackTx = \info b cont handler _env -> \case
  [] -> do
    pdb <- use (reEnv . eePactDb)
    liftDbFunction info (_pdbRollbackTx pdb)
    reState .= def
    use reTx >>= \case
      Just tx -> do
        reTx .= Nothing
        returnCEK cont handler (renderTx "Rollback Tx" (Just tx))
      Nothing -> returnCEK cont handler (renderTx "Rollback Tx" Nothing)
  args -> argsError info b args

sigKeyset :: (IsBuiltin b, Default i) => NativeFunction b i (ReplEvalM b i)
sigKeyset = \info b cont handler _env -> \case
  [] -> do
    sigs <- S.fromList . M.keys <$> viewCEKEnv eeMsgSigs
    returnCEKValue cont handler (VGuard (GKeyset (KeySet sigs KeysAll)))
  args -> argsError info b args


testCapability :: (IsBuiltin b, Default i) => NativeFunction b i (ReplEvalM b i)
testCapability = \info b currCont handler env -> \case
  [VCapToken origToken@(CapToken fqn args)] -> isCapInStack origToken >>= \case
    False -> do
      let qn = fqnToQualName fqn
      let ct = CapToken qn args
      lookupFqName fqn >>= \case
        Just (DCap d) -> do
          when (length args /= _dcapAppArity d) $ failInvariant info "Dcap argument length mismatch"
          (esCaps . csSlots) %%= (CapSlot ct []:)
          let env' = RAList.fromList $ fmap VPactValue (reverse args)
              capBody = _dcapTerm d
          -- Todo: clean up the staircase of doom.
          case _dcapMeta d of
            -- Managed capability, so we should look for it in the set of csmanaged
            Just (DefManaged mdm) -> do
              case mdm of
                -- | Not automanaged, so it must have a defmeta
                -- We are handling user-managed caps
                Just (DefManagedMeta cix _) -> do
                  let filteredCap = CapToken qn (filterIndex cix args)
                  -- Find the capability post-filtering
                  mgdCaps <- useEvalState (esCaps . csManaged)
                  case find ((==) filteredCap . _mcCap) mgdCaps of
                    Nothing -> do
                      msgCaps <- S.unions <$> viewCEKEnv eeMsgSigs
                      case find (findMsgSigCap cix filteredCap) msgCaps of
                        Just c -> do
                          let c' = set ctName fqn c
                          installCap info env c' >>= evalUserManagedCap currCont env' capBody
                        Nothing ->
                          throwExecutionError info (CapNotInstalled fqn)
                    Just managedCap -> evalUserManagedCap currCont env' capBody managedCap
                -- handle autonomous caps
                Nothing -> do
                  -- Find the capability post-filtering
                  mgdCaps <- useEvalState (esCaps . csManaged)
                  case find ((==) ct . _mcCap) mgdCaps of
                    Nothing -> do
                      msgCaps <- S.unions <$> viewCEKEnv eeMsgSigs
                      case find ((==) ct) msgCaps of
                        Just c -> do
                          let c' = set ctName fqn c
                          installCap info env c' >>= evalAutomanagedCap currCont env' capBody
                        Nothing ->
                          throwExecutionError info (CapNotInstalled fqn)
                    Just managedCap -> case _mcManaged managedCap of
                      AutoManaged bcond -> do
                        if bcond then
                          returnCEK currCont handler (VError "automanaged capability used more than once")
                        else do
                          let newManaged = AutoManaged True
                          esCaps . csManaged %%= S.union (S.singleton (set mcManaged newManaged managedCap))
                          evalWithStackFrame info currCont handler (set ceLocal env' env) capStackFrame Nothing capBody
                      _ -> failInvariant info "manager function mismatch"
            Just DefEvent ->
              failInvariant info "cannot evaluate the body of an event cap"
            Nothing -> do
              evalWithStackFrame info currCont handler (set ceLocal env' env) capStackFrame Nothing capBody
        Just {} ->
          failInvariant info "Captoken references invalid def"
        Nothing -> failInvariant info "No such def for evalCap"
    True ->
      returnCEKValue currCont handler (VString "Capability already acquired")
    where
    capStackFrame = StackFrame (_fqName fqn) (_fqModule fqn) SFDefcap
    evalUserManagedCap cont' env' capBody managedCap =  case _mcManaged managedCap of
      ManagedParam mpfqn pv managedIx -> do
        lookupFqName mpfqn >>= \case
          Just (Dfun dfun) -> do
            mparam <- maybe (failInvariant def "Managed param does not exist at index") pure (args ^? ix managedIx)
            evaluate mpfqn (_dfunTerm dfun) pv mparam >>= \case
              EvalValue res -> do
                result <- enforcePactValue res
                let mcM = ManagedParam mpfqn result managedIx
                esCaps . csManaged %%= S.union (S.singleton (set mcManaged mcM managedCap))
                let inCapEnv = set ceInCap True $ set ceLocal env' $ env
                evalWithStackFrame info cont' handler inCapEnv capStackFrame Nothing capBody
              VError v -> returnCEK currCont handler (VError v)
          _ -> failInvariant def "user managed cap is an invalid defn"
      _ -> failInvariant def "Invalid managed cap type"
    evalAutomanagedCap cont' env' capBody managedCap = case _mcManaged managedCap of
      AutoManaged bcond -> do
        if bcond then returnCEK currCont handler (VError "automanaged cap used once")
        else do
          let newManaged = AutoManaged True
          esCaps . csManaged %%= S.union (S.singleton (set mcManaged newManaged managedCap))
          let inCapEnv = set ceLocal env' $ set ceInCap True $ env
          evalWithStackFrame info cont' handler inCapEnv capStackFrame Nothing capBody
      _ -> failInvariant def "Invalid managed cap type"
    evaluate fqn' term managed value = case term of
      Lam _ lamargs body i -> do
        -- Todo: `applyLam` here gives suboptimal errors
        -- Todo: this completely violates our "step" semantics.
        -- This should be its own frame
        let inCapEnv = set ceInCap True env
            cloArgs = ArgClosure(_argType <$> lamargs)
            clo = Closure (_fqName fqn') (_fqModule fqn') cloArgs (NE.length lamargs) body Nothing inCapEnv i
        applyLam (C clo) [VPactValue managed, VPactValue value] Mt CEKNoHandler
      _t -> failInvariant (view termInfo _t) "Manager function was not a two-argument function"
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
    RExpectFailureMatch -> coreExpectFailure
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
    RBeginTx -> beginTx
    RBeginNamedTx -> beginTx
    RCommitTx -> commitTx
    RRollbackTx -> rollbackTx
    RSigKeyset -> sigKeyset
    RTestCapability -> testCapability
