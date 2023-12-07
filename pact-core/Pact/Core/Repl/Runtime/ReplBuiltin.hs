{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pact.Core.Repl.Runtime.ReplBuiltin where

import Control.Lens
import Control.Monad(when)
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
import Pact.Core.DefPacts.Types
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
import Pact.Core.Info
import Pact.Core.Namespace

import Pact.Core.Repl.Utils

prettyShowValue :: CEKValue step b i m -> Text
prettyShowValue = \case
  VPactValue p -> renderText p
  VTable (TableValue (TableName tn) _ _ _) -> "table{" <> tn <> "}"
  VClosure _ -> "<#closure>"

corePrint :: (IsBuiltin b, CEKEval step b SpanInfo (ReplM b)) => NativeFunction step b SpanInfo (ReplM b)
corePrint info b cont handler _env = \case
  [v] -> do
    liftIO $ putStrLn $ T.unpack (prettyShowValue v)
    returnCEKValue cont handler (VLiteral LUnit)
  args -> argsError info b args

rawExpect :: (IsBuiltin b, CEKEval step b SpanInfo (ReplM b)) => NativeFunction step b SpanInfo (ReplM b)
rawExpect info b cont handler _env = \case
  [VLiteral (LString msg), VPactValue v1, VClosure clo] ->
    applyLamUnsafe clo [] Mt CEKNoHandler >>= \case
       EvalValue (VPactValue v2) ->
        if v1 /= v2 then do
            let v1s = prettyShowValue (VPactValue v1)
                v2s = prettyShowValue (VPactValue v2)
            returnCEKValue cont handler $ VLiteral $ LString $ "FAILURE: " <> msg <> " expected: " <> v1s <> ", received: " <> v2s
        else returnCEKValue cont handler (VLiteral (LString ("Expect: success " <> msg)))
       _ -> returnCEK cont handler (VError "expect error: evaluation did not return a pact value to compare" info)
  args -> argsError info b args

coreExpectThat :: (IsBuiltin b, CEKEval step b SpanInfo (ReplM b)) => NativeFunction step b SpanInfo (ReplM b)
coreExpectThat info b cont handler _env = \case
  [VLiteral (LString msg), VClosure vclo, v] -> do
    applyLamUnsafe vclo [v] Mt CEKNoHandler >>= \case
      EvalValue (VLiteral (LBool c)) ->
        if c then returnCEKValue cont handler (VLiteral (LString ("Expect-that: success " <> msg)))
        else returnCEKValue cont handler  (VLiteral (LString ("FAILURE: Expect-that: Did not satisfy condition: " <> msg)))
      EvalValue _ -> returnCEK cont handler (VError "Expect-that: condition did not return a boolean" info)
      VError ve i -> returnCEK cont handler (VError ve i)
  args -> argsError info b args

coreExpectFailure :: (IsBuiltin b, CEKEval step b SpanInfo (ReplM b)) => NativeFunction step b SpanInfo (ReplM b)
coreExpectFailure info b cont handler _env = \case
  [VLiteral (LString toMatch), VClosure vclo] -> do
    es <- getEvalState
    tryError (applyLamUnsafe vclo [] Mt CEKNoHandler) >>= \case
      Right (VError _ _) -> do
        putEvalState es
        returnCEKValue cont handler $ VLiteral $ LString $ "Expect failure: Success: " <> toMatch
      Left _err -> do
        putEvalState es
        returnCEKValue cont handler $ VLiteral $ LString $ "Expect failure: Success: " <> toMatch
      Right _ ->
        returnCEKValue cont handler $ VLiteral $ LString $ "FAILURE: " <> toMatch <> ": expected failure, got result"
  [VString desc, VString toMatch, VClosure vclo] -> do
    es <- getEvalState
    tryError (applyLamUnsafe vclo [] Mt CEKNoHandler) >>= \case
      Right (VError _ _) -> do
        putEvalState es
        returnCEKValue cont handler $ VLiteral $ LString $ "Expect failure: Success: " <> desc
      Left _err -> do
        putEvalState es
        returnCEKValue cont handler $ VLiteral $ LString $ "Expect failure: Success: " <> desc
      Right _ ->
        returnCEKValue cont handler $ VLiteral $ LString $ "FAILURE: " <> toMatch <> ": expected failure, got result"
  args -> argsError info b args



continuePact :: (IsBuiltin b, CEKEval step b SpanInfo (ReplM b)) => NativeFunction step b SpanInfo (ReplM b)
continuePact info b cont handler env = \case
  [VInteger s] -> go s False Nothing Nothing
  [VInteger s, VBool r] -> go s r Nothing Nothing
  [VInteger s, VBool r, VString pid] -> go s r (Just pid) Nothing
  [VInteger s, VBool r, VString pid, VObject y] -> go s r (Just pid) (Just y)
  args -> argsError info b args
  where
    go step rollback mpid userResume = do
      mpe <- useEvalState esDefPactExec
      (pid, myield) <- case mpe of
        Nothing -> do
          pid <- maybe (throwExecutionError info NoDefPactIdAndExecEnvSupplied) (pure . DefPactId) mpid
          pure (pid, (\r -> Yield r Nothing Nothing) <$> userResume)
        Just pactExec ->
          let
            pid = maybe (_peDefPactId pactExec) DefPactId mpid
            yield = case userResume of
              Nothing -> _peYield pactExec
              Just o -> pure (Yield o Nothing Nothing)
          in pure (pid, yield)
      let pactStep = DefPactStep (fromInteger step) rollback pid myield
      setEvalState esDefPactExec Nothing
      replEvalEnv . eeDefPactStep .= Just pactStep
      s <- tryError $ resumePact info cont handler env Nothing
      replEvalEnv . eeDefPactStep .= Nothing
      liftEither s

pactState :: (IsBuiltin b, CEKEval step b SpanInfo (ReplM b)) => NativeFunction step b SpanInfo (ReplM b)
pactState info b cont handler _env = \case
  [] -> go False
  [VBool clear] -> go clear
  args -> argsError info b args
  where
  go clear = do
    mpe <- useEvalState esDefPactExec
    case mpe of
      Just pe -> do
        when clear $ esDefPactExec .== Nothing
        let yield' = case _peYield pe of
              Nothing ->  PLiteral (LBool False)
              Just (Yield y _ _) -> PObject y
            DefPactId pid = _peDefPactId pe
            ps = [(Field "pactId", PString pid)
                 ,(Field "yield", yield')
                 ,(Field "step", PInteger (fromIntegral (_peStep pe)))]
        returnCEKValue cont handler (VObject (M.fromList ps))
      Nothing -> returnCEK cont handler (VError "pact-state: no pact exec in context" info)

coreplEvalEnvStackFrame :: (IsBuiltin b, CEKEval step b SpanInfo (ReplM b)) => NativeFunction step b SpanInfo (ReplM b)
coreplEvalEnvStackFrame info b cont handler _env = \case
  [] -> do
    capSet <- getAllStackCaps
    returnCEKValue cont handler $ VString $ T.pack (show capSet)
  args -> argsError info b args

envEvents :: (IsBuiltin b, CEKEval step b SpanInfo (ReplM b)) => NativeFunction step b SpanInfo (ReplM b)
envEvents info b cont handler _env = \case
  [VBool clear] -> do
    events <- fmap envToObj <$> useEvalState esEvents
    when clear $ setEvalState esEvents []
    returnCEKValue cont handler (VList (V.fromList events))
    where
    envToObj (PactEvent name args mn mh) =
      PObject
      $ M.fromList
      $ over _1 Field
      <$> [ ("name", PString (renderQualName (QualifiedName name mn)))
        , ("params", PList (V.fromList args))
        , ("module-hash", PString (hashToText (_mhHash mh)))]
  args -> argsError info b args

envHash :: (IsBuiltin b, CEKEval step b SpanInfo (ReplM b)) => NativeFunction step b SpanInfo (ReplM b)
envHash info b cont handler _env = \case
  [VString s] -> do
    case decodeBase64UrlUnpadded (T.encodeUtf8 s) of
      Left e -> returnCEK cont handler (VError (T.pack e) info)
      Right hs -> do
        (replEvalEnv . eeHash) .= Hash (toShort hs)
        returnCEKValue cont handler VUnit
  args -> argsError info b args

envData :: (IsBuiltin b, CEKEval step b SpanInfo (ReplM b)) => NativeFunction step b SpanInfo (ReplM b)
envData info b cont handler _env = \case
  [VObject o] -> do
    let ed = ObjectData o
    (replEvalEnv . eeMsgBody) .= ed
    returnCEKValue cont handler VUnit
  args -> argsError info b args

envChainData :: (IsBuiltin b, CEKEval step b SpanInfo (ReplM b)) => NativeFunction step b SpanInfo (ReplM b)
envChainData info b cont handler _env = \case
  [VObject cdataObj] -> do
    pd <- viewEvalEnv eePublicData
    go pd (M.toList cdataObj)
    where
    go pd [] = do
      replEvalEnv . eePublicData .= pd
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
      _ -> returnCEK cont handler (VError ("envChainData: bad public metadata value for key: " <> _field k) info)
  args -> argsError info b args

envKeys :: (IsBuiltin b, CEKEval step b SpanInfo (ReplM b)) => NativeFunction step b SpanInfo (ReplM b)
envKeys info b cont handler _env = \case
  [VList ks] -> do
    keys <- traverse (asString info b) ks
    replEvalEnv . eeMsgSigs .= M.fromList ((,mempty) . PublicKeyText <$> V.toList keys)
    returnCEKValue cont handler VUnit
  args -> argsError info b args

envSigs :: (IsBuiltin b, CEKEval step b SpanInfo (ReplM b)) => NativeFunction step b SpanInfo (ReplM b)
envSigs info b cont handler _env = \case
  [VList ks] ->
    case traverse keyCapObj ks of
      Just sigs -> do
        (replEvalEnv . eeMsgSigs) .= M.fromList (V.toList sigs)
        returnCEKValue cont handler VUnit
      Nothing -> returnCEK cont handler (VError ("env-sigs format is wrong") info)
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

beginTx :: (IsBuiltin b, CEKEval step b SpanInfo (ReplM b)) => NativeFunction step b SpanInfo (ReplM b)
beginTx info b cont handler _env = \case
  [VString s] -> begin' info (Just s) >>= returnCEK cont handler . renderTx info "Begin Tx"
  [] -> begin' info Nothing >>= returnCEK cont handler . renderTx info "Begin Tx"
  args -> argsError info b args

renderTx :: i -> Text -> Maybe (TxId, Maybe Text) -> EvalResult step b i m
renderTx _info start (Just (TxId tid, mt)) =
  EvalValue $ VString $ start <> " " <> T.pack (show tid) <> maybe mempty (" " <>) mt
renderTx info start Nothing = VError ("tx-function failure " <> start) info

begin' :: SpanInfo -> Maybe Text -> ReplM b (Maybe (TxId, Maybe Text))
begin' info mt = do
  pdb <- use (replEvalEnv . eePactDb)
  mode <- viewEvalEnv eeMode
  mTxId <- liftDbFunction info (_pdbBeginTx pdb mode)
  replTx .= ((,mt) <$> mTxId)
  return ((,mt) <$> mTxId)

commitTx :: (IsBuiltin b, CEKEval step b SpanInfo (ReplM b)) => NativeFunction step b SpanInfo (ReplM b)
commitTx info b cont handler _env = \case
  [] -> do
    pdb <- use (replEvalEnv . eePactDb)
    liftDbFunction info (_pdbCommitTx pdb)
    fqdefs <- useEvalState (esLoaded . loAllLoaded)
    cs <- useEvalState esStack
    replEvalState .= set esStack cs (set (esLoaded . loAllLoaded) fqdefs def)
    use replTx >>= \case
      Just tx -> do
        replTx .= Nothing
        returnCEK cont handler (renderTx info "Commit Tx" (Just tx))
      Nothing -> returnCEK cont handler (renderTx info "Commit Tx" Nothing)
  args -> argsError info b args


rollbackTx :: (IsBuiltin b, CEKEval step b SpanInfo (ReplM b)) => NativeFunction step b SpanInfo (ReplM b)
rollbackTx info b cont handler _env = \case
  [] -> do
    pdb <- use (replEvalEnv . eePactDb)
    liftDbFunction info (_pdbRollbackTx pdb)
    fqdefs <- useEvalState (esLoaded . loAllLoaded)
    cs <- useEvalState esStack
    replEvalState .= set esStack cs (set (esLoaded . loAllLoaded) fqdefs def)
    use replTx >>= \case
      Just tx -> do
        replTx .= Nothing
        returnCEK cont handler (renderTx info "Rollback Tx" (Just tx))
      Nothing -> returnCEK cont handler (renderTx info "Rollback Tx" Nothing)
  args -> argsError info b args

sigKeyset :: (IsBuiltin b, CEKEval step b SpanInfo (ReplM b)) => NativeFunction step  b SpanInfo (ReplM b)
sigKeyset info b cont handler _env = \case
  [] -> do
    sigs <- S.fromList . M.keys <$> viewEvalEnv eeMsgSigs
    returnCEKValue cont handler (VGuard (GKeyset (KeySet sigs KeysAll)))
  args -> argsError info b args


testCapability :: (IsBuiltin b, CEKEval step b SpanInfo (ReplM b)) => NativeFunction step  b SpanInfo (ReplM b)
testCapability info b cont handler env = \case
  [VCapToken origToken] -> do
    lookupFqName (_ctName origToken) >>= \case
      Just (DCap d) -> do
        let cBody = Constant LUnit info
            ignoreContBody _env _mct _mev _cb c = c
            cont' = SeqC env cBody cont
        case _dcapMeta d of
          Unmanaged ->
            evalCap info cont' handler env origToken ignoreContBody cBody
          _ -> do
            -- Installed caps emit and event
            -- so we create a fake stack frame
            installCap info env origToken False *> evalCap info cont' handler env origToken ignoreContBody cBody
      _ -> returnCEK cont handler (VError "no such capability" info)
  args -> argsError info b args

envExecConfig :: (IsBuiltin b, CEKEval step b SpanInfo (ReplM b)) => NativeFunction step b SpanInfo (ReplM b)
envExecConfig info b cont handler _env = \case
  [VList s] -> do
    s' <- traverse go (V.toList s)
    replEvalEnv . eeFlags .= S.fromList s'
    let reps = PString . flagRep <$> s'
    returnCEKValue cont handler (VList (V.fromList reps))
    where
    go str = do
      str' <- asString info b str
      case M.lookup str' flagReps of
        Just f -> pure f
        Nothing -> failInvariant info $ "Invalid flag, allowed: " <> T.pack (show (M.keys flagReps))
  args -> argsError info b args

envNamespacePolicy :: (IsBuiltin b, CEKEval step b SpanInfo (ReplM b)) => NativeFunction step b SpanInfo (ReplM b)
envNamespacePolicy info b cont handler _env = \case
  [VBool allowRoot, VClosure (C clo)] -> do
    pdb <- viewEvalEnv eePactDb
    let qn = QualifiedName (_cloFnName clo) (_cloModName clo)
    when (_cloArity clo /= 2) $ failInvariant info "Namespace manager function has invalid argument length"
    getModuleMember info pdb qn >>= \case
      Dfun _ -> do
        let nsp = SmartNamespacePolicy allowRoot qn
        replEvalEnv . eeNamespacePolicy .= nsp
        returnCEKValue cont handler (VString "Installed namespace policy")
      _ -> returnCEK cont handler (VError "invalid namespace manager function type" info)
  args -> argsError info b args

replBuiltinEnv
  :: CEKEval step ReplRawBuiltin SpanInfo (ReplM ReplRawBuiltin)
  => BuiltinEnv step (ReplBuiltin RawBuiltin) SpanInfo (ReplM (ReplBuiltin RawBuiltin))
replBuiltinEnv i b env =
  mkBuiltinFn i b env (replRawBuiltinRuntime b)

replRawBuiltinRuntime
  :: CEKEval step ReplRawBuiltin SpanInfo (ReplM ReplRawBuiltin)
  => ReplBuiltin RawBuiltin
  -> NativeFunction step (ReplBuiltin RawBuiltin) SpanInfo (ReplM (ReplBuiltin RawBuiltin))
replRawBuiltinRuntime = \case
  RBuiltinWrap cb ->
    rawBuiltinRuntime cb
  RBuiltinRepl br -> case br of
    RExpect -> rawExpect
    RExpectFailure -> coreExpectFailure
    RExpectFailureMatch -> coreExpectFailure
    RExpectThat -> coreExpectThat
    RPrint -> corePrint
    REnvStackFrame -> coreplEvalEnvStackFrame
    RPactState -> pactState
    RResetPactState -> pactState
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
    RContinuePact -> continuePact
    RContinuePactRollback -> continuePact
    RContinuePactRollbackYield -> continuePact
    REnvExecConfig -> envExecConfig
    REnvNamespacePolicy -> envNamespacePolicy
