{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}

module Pact.Core.Repl.Runtime.ReplBuiltin where

import Control.Lens
import Control.Monad(when)
import Control.Monad.Except
import Control.Monad.IO.Class(liftIO)
import Data.Default
import Data.Text(Text)
import Data.Maybe(fromMaybe)
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
import Pact.Core.IR.Eval.CoreBuiltin
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
import qualified Pact.Core.Legacy.LegacyPactValue as Legacy

import qualified PackageInfo_pact_tng as PI
import qualified Data.Version as V
import qualified Data.Attoparsec.Text as A


import Pact.Core.Repl.Utils
import qualified Pact.Time as PactTime
import Pact.Core.Gas.TableGasModel

type ReplCEKEval step = CEKEval step ReplCoreBuiltin SpanInfo (ReplM ReplCoreBuiltin)

prettyShowValue :: CEKValue step b i m -> Text
prettyShowValue = \case
  VPactValue p -> renderText p
  VTable (TableValue (TableName tn mn) _ _) -> "table{" <> renderModuleName mn <> "_" <> tn <> "}"
  VClosure _ -> "<#closure>"

corePrint :: ReplCEKEval step => NativeFunction step ReplCoreBuiltin SpanInfo (ReplM ReplCoreBuiltin)
corePrint info b cont handler _env = \case
  [v] -> do
    liftIO $ putStrLn $ T.unpack (prettyShowValue v)
    returnCEKValue cont handler (VLiteral LUnit)
  args -> argsError info b args

coreExpect :: ReplCEKEval step => NativeFunction step ReplCoreBuiltin SpanInfo (ReplM ReplCoreBuiltin)
coreExpect info b cont handler _env = \case
  [VLiteral (LString msg), VClosure expected, VClosure provided] -> do
    es <- getEvalState
    tryError (applyLamUnsafe provided [] Mt CEKNoHandler) >>= \case
      Right (EvalValue (VPactValue v2)) -> do
        applyLamUnsafe expected [] Mt CEKNoHandler >>= \case
          EvalValue (VPactValue v1) -> do
            if v1 /= v2 then do
                let v1s = prettyShowValue (VPactValue v1)
                    v2s = prettyShowValue (VPactValue v2)
                returnCEKValue cont handler $ VLiteral $ LString $ "FAILURE: " <> msg <> " expected: " <> v1s <> ", received: " <> v2s
            else returnCEKValue cont handler (VLiteral (LString ("Expect: success " <> msg)))
          _ -> returnCEK cont handler (VError "evaluation within expect did not return a pact value" info)
      Right (VError errMsg _) -> do
        putEvalState es
        returnCEKValue cont handler $ VString $ "FAILURE: " <> msg <> " evaluation of actual failed with error message: " <> errMsg
      Right _v ->
        returnCEK cont handler $ VError "FAILURE: expect expression did not return a pact value for comparison" info
      Left err -> do
        putEvalState es
        currSource <- use replCurrSource
        returnCEKValue cont handler $ VString $ "FAILURE: " <> msg <> " evaluation of actual failed with error message:\n" <>
          replError currSource err
  args -> argsError info b args

coreExpectThat :: ReplCEKEval step => NativeFunction step ReplCoreBuiltin SpanInfo (ReplM ReplCoreBuiltin)
coreExpectThat info b cont handler _env = \case
  [VLiteral (LString msg), VClosure vclo, v] -> do
    applyLamUnsafe vclo [v] Mt CEKNoHandler >>= \case
      EvalValue (VLiteral (LBool c)) ->
        if c then returnCEKValue cont handler (VLiteral (LString ("Expect-that: success " <> msg)))
        else returnCEKValue cont handler  (VLiteral (LString ("FAILURE: Expect-that: Did not satisfy condition: " <> msg)))
      EvalValue _ -> returnCEK cont handler (VError "Expect-that: condition did not return a boolean" info)
      VError ve i -> returnCEK cont handler (VError ve i)
  args -> argsError info b args

coreExpectFailure :: ReplCEKEval step => NativeFunction step ReplCoreBuiltin SpanInfo (ReplM ReplCoreBuiltin)
coreExpectFailure info b cont handler _env = \case
  [VString doc, VClosure vclo] -> do
    es <- getEvalState
    tryError (applyLamUnsafe vclo [] Mt CEKNoHandler) >>= \case
      Right (VError _ _) -> do
        putEvalState es
        returnCEKValue cont handler $ VLiteral $ LString $ "Expect failure: Success: " <> doc
      Left _err -> do
        putEvalState es
        returnCEKValue cont handler $ VLiteral $ LString $ "Expect failure: Success: " <> doc
      Right _ ->
        returnCEKValue cont handler $ VLiteral $ LString $ "FAILURE: " <> doc <> ": expected failure, got result"
  [VString desc, VString toMatch, VClosure vclo] -> do
    es <- getEvalState
    tryError (applyLamUnsafe vclo [] Mt CEKNoHandler) >>= \case
      Right (VError err _) -> do
        putEvalState es
        if toMatch `T.isInfixOf` err
          then returnCEKValue cont handler $ VLiteral $ LString $ "Expect failure: Success: " <> desc
          else returnCEKValue cont handler $ VLiteral $ LString $
               "FAILURE: " <> desc <> ": expected error message '" <> toMatch <> "', got '" <> err <> "'"
      Left _err -> do
        putEvalState es
        returnCEKValue cont handler $ VLiteral $ LString $ "Expect failure: Success: " <> desc
      Right (EvalValue v) ->
        returnCEKValue cont handler $ VLiteral $ LString $ "FAILURE: " <> toMatch <> ": expected failure, got result: " <> prettyShowValue v
  args -> argsError info b args


continuePact :: forall step . ReplCEKEval step => NativeFunction step ReplCoreBuiltin SpanInfo (ReplM ReplCoreBuiltin)
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
              Just o -> case _peYield pactExec of
                Just (Yield _ p _) -> pure (Yield o p Nothing)
                Nothing ->
                  pure (Yield o Nothing Nothing)
          in pure (pid, yield)
      let pactStep = DefPactStep (fromInteger step) rollback pid myield
      setEvalState esDefPactExec Nothing
      replEvalEnv . eeDefPactStep .= Just pactStep
      merr <- tryError $ evalUnsafe @step =<< resumePact info Mt CEKNoHandler env Nothing
      replEvalEnv . eeDefPactStep .= Nothing
      v <- liftEither merr
      returnCEK cont handler v

pactState :: ReplCEKEval step => NativeFunction step ReplCoreBuiltin SpanInfo (ReplM ReplCoreBuiltin)
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

coreplEvalEnvStackFrame :: ReplCEKEval step => NativeFunction step ReplCoreBuiltin SpanInfo (ReplM ReplCoreBuiltin)
coreplEvalEnvStackFrame info b cont handler _env = \case
  [] -> do
    sfs <- fmap (PString . T.pack . show) <$> use (replEvalState . esStack)
    returnCEKValue cont handler $ VList (V.fromList sfs)
  args -> argsError info b args

envEvents :: ReplCEKEval step => NativeFunction step ReplCoreBuiltin SpanInfo (ReplM ReplCoreBuiltin)
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

envHash :: ReplCEKEval step => NativeFunction step ReplCoreBuiltin SpanInfo (ReplM ReplCoreBuiltin)
envHash info b cont handler _env = \case
  [VString s] -> do
    case decodeBase64UrlUnpadded (T.encodeUtf8 s) of
      Left e -> returnCEK cont handler (VError (T.pack e) info)
      Right hs -> do
        (replEvalEnv . eeHash) .= Hash (toShort hs)
        returnCEKValue cont handler $ VString $ "Set tx hash to " <> s
  args -> argsError info b args

envData :: ReplCEKEval step => NativeFunction step ReplCoreBuiltin SpanInfo (ReplM ReplCoreBuiltin)
envData info b cont handler _env = \case
  [VPactValue pv] -> do
    -- to mimic prod, we must roundtrip here
    -- if it fails silently, this is fine.
    let pv' = fromMaybe pv (Legacy.roundtripPactValue pv)
    (replEvalEnv . eeMsgBody) .= pv'
    returnCEKValue cont handler (VString "Setting transaction data")
  args -> argsError info b args

envChainData :: ReplCEKEval step => NativeFunction step ReplCoreBuiltin SpanInfo (ReplM ReplCoreBuiltin)
envChainData info b cont handler _env = \case
  [VObject cdataObj] -> do
    pd <- viewEvalEnv eePublicData
    go pd (M.toList cdataObj)
    where
    go pd [] = do
      replEvalEnv . eePublicData .= pd
      returnCEKValue cont handler (VString "Updated public metadata")
    go pd ((k,v):rest) = case v of
      PInteger i
        | k == cdGasLimit ->
          go (set (pdPublicMeta . pmGasLimit) (Gas (fromIntegral i)) pd) rest
        | k == cdBlockHeight ->
          go (set pdBlockHeight (fromInteger i) pd) rest
      PDecimal i
        | k == cdGasPrice ->
          go (set (pdPublicMeta . pmGasPrice) i pd) rest
      PString s
        | k == cdChainId ->
          go (set (pdPublicMeta . pmChainId) (ChainId s) pd) rest
        | k == cdSender ->
          go (set (pdPublicMeta . pmSender) s pd) rest
        | k == cdPrevBlockHash ->
          go (set pdPrevBlockHash s pd) rest
      PTime time
        | k == cdBlockTime -> go (set pdBlockTime (PactTime.toPosixTimestampMicros time) pd) rest
      _ -> returnCEK cont handler (VError ("envChainData: bad public metadata value for key: " <> _field k) info)
  args -> argsError info b args

envKeys :: ReplCEKEval step => NativeFunction step ReplCoreBuiltin SpanInfo (ReplM ReplCoreBuiltin)
envKeys info b cont handler _env = \case
  [VList ks] -> do
    keys <- traverse (asString info b) ks
    replEvalEnv . eeMsgSigs .= M.fromList ((,mempty) . PublicKeyText <$> V.toList keys)
    returnCEKValue cont handler (VString "Setting transaction keys")
  args -> argsError info b args

envSigs :: ReplCEKEval step => NativeFunction step ReplCoreBuiltin SpanInfo (ReplM ReplCoreBuiltin)
envSigs info b cont handler _env = \case
  [VList ks] ->
    case traverse keyCapObj ks of
      Just sigs -> do
        (replEvalEnv . eeMsgSigs) .= M.fromList (V.toList sigs)
        returnCEKValue cont handler $ VString "Setting transaction signatures/caps"
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

beginTx :: ReplCEKEval step => NativeFunction step ReplCoreBuiltin SpanInfo (ReplM ReplCoreBuiltin)
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

emptyTxState :: ReplM b ()
emptyTxState = do
  fqdefs <- useEvalState (esLoaded . loAllLoaded)
  cs <- useEvalState esStack
  esc <- useEvalState esCheckRecursion
  let newEvalState =
        set esStack cs
        $ set (esLoaded . loAllLoaded) fqdefs
        $ set esCheckRecursion esc def
  replEvalState .= newEvalState


commitTx :: ReplCEKEval step => NativeFunction step ReplCoreBuiltin SpanInfo (ReplM ReplCoreBuiltin)
commitTx info b cont handler _env = \case
  [] -> do
    pdb <- use (replEvalEnv . eePactDb)
    _txLog <- liftDbFunction info (_pdbCommitTx pdb)
    emptyTxState
    use replTx >>= \case
      Just tx -> do
        replTx .= Nothing
        returnCEK cont handler (renderTx info "Commit Tx" (Just tx))
      Nothing -> returnCEK cont handler (renderTx info "Commit Tx" Nothing)
  args -> argsError info b args


rollbackTx :: ReplCEKEval step => NativeFunction step ReplCoreBuiltin SpanInfo (ReplM ReplCoreBuiltin)
rollbackTx info b cont handler _env = \case
  [] -> do
    pdb <- use (replEvalEnv . eePactDb)
    liftDbFunction info (_pdbRollbackTx pdb)
    emptyTxState
    use replTx >>= \case
      Just tx -> do
        replTx .= Nothing
        returnCEK cont handler (renderTx info "Rollback Tx" (Just tx))
      Nothing -> returnCEK cont handler (renderTx info "Rollback Tx" Nothing)
  args -> argsError info b args

sigKeyset :: ReplCEKEval step => NativeFunction step ReplCoreBuiltin SpanInfo (ReplM ReplCoreBuiltin)
sigKeyset info b cont handler _env = \case
  [] -> do
    sigs <- S.fromList . M.keys <$> viewEvalEnv eeMsgSigs
    returnCEKValue cont handler (VGuard (GKeyset (KeySet sigs KeysAll)))
  args -> argsError info b args


testCapability :: ReplCEKEval step => NativeFunction step ReplCoreBuiltin SpanInfo (ReplM ReplCoreBuiltin)
testCapability info b cont handler env = \case
  [VCapToken origToken] -> do
    lookupFqName (_ctName origToken) >>= \case
      Just (DCap d) -> do
        let cBody = Constant LUnit info
            cont' = SeqC env cBody cont
        case _dcapMeta d of
          Unmanaged ->
            evalCap info cont' handler env origToken PopCapInvoke TestCapEval cBody
          _ -> do
            -- Installed caps emit and event
            -- so we create a fake stack frame
            installCap info env origToken False *> evalCap info cont' handler env origToken PopCapInvoke TestCapEval cBody
      _ -> returnCEK cont handler (VError "no such capability" info)
  args -> argsError info b args

envExecConfig :: ReplCEKEval step => NativeFunction step ReplCoreBuiltin SpanInfo (ReplM ReplCoreBuiltin)
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

envNamespacePolicy :: ReplCEKEval step => NativeFunction step ReplCoreBuiltin SpanInfo (ReplM ReplCoreBuiltin)
envNamespacePolicy info b cont handler _env = \case
  [VBool allowRoot, VClosure (C clo)] -> do
    pdb <- viewEvalEnv eePactDb
    let qn = fqnToQualName (_cloFqName clo)
    when (_cloArity clo /= 2) $ failInvariant info "Namespace manager function has invalid argument length"
    getModuleMember info pdb qn >>= \case
      Dfun _ -> do
        let nsp = SmartNamespacePolicy allowRoot qn
        replEvalEnv . eeNamespacePolicy .= nsp
        returnCEKValue cont handler (VString "Installed namespace policy")
      _ -> returnCEK cont handler (VError "invalid namespace manager function type" info)
  args -> argsError info b args

envGas :: ReplCEKEval step => NativeFunction step ReplCoreBuiltin SpanInfo (ReplM ReplCoreBuiltin)
envGas info b cont handler _env = \case
  [] -> do
    Gas gas <- milliGasToGas <$> getGas
    returnCEKValue cont handler (VInteger (fromIntegral gas))
  [VInteger g] -> do
    putGas $ gasToMilliGas (Gas (fromInteger g))
    returnCEKValue cont handler $ VString $ "Set gas to " <> T.pack (show g)
  args -> argsError info b args

envMilliGas :: ReplCEKEval step => NativeFunction step ReplCoreBuiltin SpanInfo (ReplM ReplCoreBuiltin)
envMilliGas info b cont handler _env = \case
  [] -> do
    MilliGas gas <- getGas
    returnCEKValue cont handler (VInteger (fromIntegral gas))
  [VInteger g] -> do
    putGas (MilliGas (fromIntegral g))
    returnCEKValue cont handler $ VString $ "Set milligas to" <> T.pack (show g)
  args -> argsError info b args

envGasLimit :: ReplCEKEval step => NativeFunction step ReplCoreBuiltin SpanInfo (ReplM ReplCoreBuiltin)
envGasLimit info b cont handler _env = \case
  [VInteger g] -> do
    (replEvalEnv . eeGasModel . gmGasLimit) .= MilliGasLimit (gasToMilliGas (Gas (fromInteger g)))
    returnCEKValue cont handler $ VString $ "Set gas limit to " <> T.pack (show g)
  args -> argsError info b args

envGasLog :: ReplCEKEval step => NativeFunction step ReplCoreBuiltin SpanInfo (ReplM ReplCoreBuiltin)
envGasLog info b cont handler _env = \case
  [] -> do
    gl <- useEvalState esGasLog
    setEvalState esGasLog $ Just []
    case gl of
      Nothing ->
        returnCEKValue cont handler (VString "Enabled gas log")
      Just logs -> let
        total = MilliGas $ sum [g | MilliGas g <- _gleThisUsed <$> logs]
        totalLine = PString . ("TOTAL: " <> ) $ renderCompactText total
        logLines = renderLine <$> reverse logs
        in
          returnCEKValue cont handler (VList $ V.fromList (totalLine:logLines))
  args -> argsError info b args
  where
  renderLine :: GasLogEntry (ReplBuiltin CoreBuiltin) -> PactValue
  renderLine (GasLogEntry entry (MilliGas millisUsed) entryGas) = PString $ renderCompactText' $ n <> ": " <> pretty entryGas
    where
      n = case entry of
            Left ga -> pretty ga <> ":currTotalGas=" <> pretty millisUsed
            Right nativeArg -> "Native" <> parens (pretty nativeArg) <> ":currTotalGas=" <> pretty millisUsed

envEnableReplNatives :: ReplCEKEval step => NativeFunction step ReplCoreBuiltin SpanInfo (ReplM ReplCoreBuiltin)
envEnableReplNatives info b cont handler _env = \case
  [VBool enabled] -> do
    let s = if enabled then "enabled" else "disabled"
    replNativesEnabled .= enabled
    returnCEKValue cont handler $ VString $ "repl natives " <> s
  args -> argsError info b args

envGasModel :: ReplCEKEval step => NativeFunction step ReplCoreBuiltin SpanInfo (ReplM ReplCoreBuiltin)
envGasModel info b cont handler _env = \case
  [] -> do
    gm <- viewEvalEnv eeGasModel
    let msg = "Current gas model is '" <> _gmName gm <> "': " <> _gmDesc gm
    returnCEKValue cont handler (VString msg)
  args@[VString model] -> do
    gm <- viewEvalEnv eeGasModel
    newmodel' <- case model of
      "table" -> pure $ replTableGasModel (_gmGasLimit gm)
      "fixed" -> pure (constantGasModel mempty (_gmGasLimit gm))
      _ -> argsError info b args
    replEvalEnv . eeGasModel .= newmodel'
    returnCEKValue cont handler $ VString $ "Set gas model to " <> _gmDesc newmodel'
  [VString "fixed", VInteger arg] -> do
    gm <- viewEvalEnv eeGasModel
    let newmodel' = constantGasModel (gasToMilliGas (Gas (fromIntegral arg))) (_gmGasLimit gm)
    replEvalEnv . eeGasModel .= newmodel'
    returnCEKValue cont handler $ VString $ "Set gas model to " <> _gmDesc newmodel'
  args -> argsError info b args


-----------------------------------
-- Pact Version
-----------------------------------

coreVersion :: (CEKEval step b i m, MonadEval b i m) => NativeFunction step b i m
coreVersion info b  cont handler _env = \case
  [] -> let
    v = T.pack (V.showVersion PI.version)
    in returnCEKValue cont handler (VString v)
  args -> argsError info b args


coreEnforceVersion :: (CEKEval step b i m, MonadEval b i m) => NativeFunction step b i m
coreEnforceVersion info b cont handler _env = \case
  [VString lowerBound] -> do
    lowerBound' <- mkVersion lowerBound
    if lowerBound' <= PI.version
      then returnCEKValue cont handler (VBool True)
      else throwExecutionError info (EnforcePactVersionFailure lowerBound' Nothing)
  [VString lowerBound, VString upperBound] -> do
    lowerBound' <- mkVersion lowerBound
    upperBound' <- mkVersion upperBound
    if lowerBound' <= PI.version && PI.version <= upperBound'
      then returnCEKValue cont handler (VBool True)
      else throwExecutionError info (EnforcePactVersionFailure lowerBound' (Just upperBound'))
  args -> argsError info b args
  where
    mkVersion s =
      case A.parseOnly ((A.decimal `A.sepBy` A.char '.') <* A.endOfInput) s of
        Left _msg -> throwExecutionError info (EnforcePactVersionParseFailure s)
        Right li -> pure (V.makeVersion li)



replBuiltinEnv
  :: CEKEval step ReplCoreBuiltin SpanInfo (ReplM ReplCoreBuiltin)
  => BuiltinEnv step (ReplBuiltin CoreBuiltin) SpanInfo (ReplM (ReplBuiltin CoreBuiltin))
replBuiltinEnv i b env =
  mkBuiltinFn i b env (replCoreBuiltinRuntime b)

replCoreBuiltinRuntime
  :: CEKEval step ReplCoreBuiltin SpanInfo (ReplM ReplCoreBuiltin)
  => ReplBuiltin CoreBuiltin
  -> NativeFunction step (ReplBuiltin CoreBuiltin) SpanInfo (ReplM (ReplBuiltin CoreBuiltin))
replCoreBuiltinRuntime = \case
  RBuiltinWrap cb ->
    coreBuiltinRuntime cb
  RBuiltinRepl br -> case br of
    RExpect -> coreExpect
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
    RContinuePactRollbackYieldObj -> continuePact
    REnvGas -> envGas
    REnvGasSet -> envGas
    REnvMilliGas -> envMilliGas
    REnvSetMilliGas -> envMilliGas
    REnvGasLimit -> envGasLimit
    REnvGasLog -> envGasLog
    REnvGasModel -> envGasModel
    REnvAskGasModel -> envGasModel
    REnvGasModelFixed -> envGasModel
    RPactVersion -> coreVersion
    REnforcePactVersionMin -> coreEnforceVersion
    REnforcePactVersionRange -> coreEnforceVersion
    REnvEnableReplNatives -> envEnableReplNatives
