{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Pact.Core.IR.Eval.Direct.ReplBuiltin
  (replBuiltinEnv) where


import Control.Lens
import Control.Monad(when)
import Control.Monad.State.Strict
import Control.Monad.Except
import Data.Either(partitionEithers)
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
import Pact.Core.IR.Eval.Direct.Evaluator
import Pact.Core.IR.Eval.Direct.CoreBuiltin
import Pact.Core.IR.Eval.Direct.Types
import Pact.Core.Names
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
import Pact.Core.ModRefs
import Pact.Core.StableEncoding

import qualified Pact.Core.Version as PI
import qualified Data.Version as V
import qualified Data.Attoparsec.Text as A


import Pact.Core.Repl.Utils
import qualified Pact.Time as PactTime
import Data.IORef


prettyShowValue :: EvalValue b i m -> Text
prettyShowValue = \case
  VPactValue p -> renderText p
  VClosure _ -> "<#closure>"

corePrint :: NativeFunction ReplRuntime ReplCoreBuiltin FileLocSpanInfo
corePrint info b _env = \case
  [v] -> do
    replPrintLn' info (prettyShowValue v)
    return (VLiteral LUnit)
  args -> argsError info b args

returnTestFailure
  :: FileLocSpanInfo
  -> Text
  -> Text
  -> EvalM ReplRuntime b FileLocSpanInfo (EvalValue ReplRuntime b FileLocSpanInfo)
returnTestFailure info testName msg = do
  recordTestFailure testName info msg
  return (VLiteral (LString msg))

returnTestSuccess
  :: FileLocSpanInfo
  -> Text
  -> Text
  -> EvalM ReplRuntime b FileLocSpanInfo (EvalValue ReplRuntime b FileLocSpanInfo)
returnTestSuccess info testName msg = do
  recordTestSuccess testName info
  return (VString msg)

coreExpect :: NativeFunction ReplRuntime ReplCoreBuiltin FileLocSpanInfo
coreExpect info b _env = \case
  [VLiteral (LString testName), VClosure expected, VClosure provided] -> do
    es <- get
    tryError (applyLamUnsafe info provided []) >>= \case
      Right (VPactValue v2) -> do
        applyLamUnsafe info expected [] >>= enforcePactValue info >>= \case
          v1 -> do
            if v1 /= v2 then do
                let v1s = prettyShowValue (VPactValue v1)
                    v2s = prettyShowValue (VPactValue v2)
                returnTestFailure info testName $ "FAILURE: " <> testName <> " expected: " <> v1s <> ", received: " <> v2s
            else returnTestSuccess info testName ("Expect: success " <> testName)
      Right _v -> do
        let failureMsg = "FAILURE: expect expression did not return a pact value for comparison"
        recordTestFailure testName info failureMsg
        throwUserRecoverableError info $ UserEnforceError failureMsg
      Left err -> do
        put es
        returnTestFailure info testName $ "FAILURE: " <> testName <> " evaluation of actual failed with error message:\n" <>
          renderCompactText err
  args -> argsError info b args

coreExpectThat :: NativeFunction ReplRuntime ReplCoreBuiltin FileLocSpanInfo
coreExpectThat info b _env = \case
  [VLiteral (LString testName), VClosure vclo, v] -> do
    applyLamUnsafe info vclo [v] >>= \case
      VLiteral (LBool c) ->
        if c then returnTestSuccess info testName ("Expect-that: success " <> testName)
        else returnTestFailure info testName ("FAILURE: Expect-that: Did not satisfy condition: " <> testName)
      _ -> do
        recordTestFailure testName info "FAILURE: expect-that expression did not return a boolean"
        throwNativeExecutionError info b "Expect-that: condition did not return a boolean"
  args -> argsError info b args

coreExpectFailure :: NativeFunction ReplRuntime ReplCoreBuiltin FileLocSpanInfo
coreExpectFailure info b _env = \case
  [VString testName, VClosure vclo] -> do
    es <- get
    tryError (applyLamUnsafe info vclo []) >>= \case
      Left (PEUserRecoverableError _ _ _) -> do
        put es
        returnTestSuccess info testName $ "Expect failure: Success: " <> testName
      Left _err -> do
        put es
        returnTestSuccess info testName $ "Expect failure: Success: " <> testName
      Right _ ->
        returnTestFailure info testName $ "FAILURE: " <> testName <> ": expected failure, got result"
  [VString testName, VString toMatch, VClosure vclo] -> do
    es <- get
    tryError (applyLamUnsafe info vclo []) >>= \case
      Left userErr -> do
        put es
        let err = renderCompactText userErr
        if toMatch `T.isInfixOf` err
          then returnTestSuccess info testName $ "Expect failure: Success: " <> testName
          else returnTestFailure info testName $
               "FAILURE: " <> testName <> ": expected error message '" <> toMatch <> "', got '" <> err <> "'"
      Right v ->
        returnTestFailure info testName $ "FAILURE: " <> toMatch <> ": expected failure, got result: " <> prettyShowValue v
  args -> argsError info b args


continuePact :: NativeFunction ReplRuntime ReplCoreBuiltin FileLocSpanInfo
continuePact info b env = \case
  [VInteger s] -> go s False Nothing Nothing
  [VInteger s, VBool r] -> go s r Nothing Nothing
  [VInteger s, VBool r, VString pid] -> go s r (Just pid) Nothing
  [VInteger s, VBool r, VString pid, VObject y] -> go s r (Just pid) (Just y)
  args -> argsError info b args
  where
    go step rollback mpid userResume = do
      mpe <- use esDefPactExec
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
      esDefPactExec .= Nothing
      replEvalEnv . eeDefPactStep .== Just pactStep
      merr <- tryError $ resumePact info env Nothing
      replEvalEnv . eeDefPactStep .== Nothing
      liftEither merr

pactState :: NativeFunction ReplRuntime ReplCoreBuiltin FileLocSpanInfo
pactState info b _env = \case
  [] -> go False
  [VBool clear] -> go clear
  args -> argsError info b args
  where
  go clear = do
    mpe <- use esDefPactExec
    case mpe of
      Just pe -> do
        when clear $ esDefPactExec .= Nothing
        let yield' = case _peYield pe of
              Nothing ->  PLiteral (LBool False)
              Just (Yield y _ _) -> PObject y
            DefPactId pid = _peDefPactId pe
            ps = [(Field "pactId", PString pid)
                 ,(Field "yield", yield')
                 ,(Field "step", PInteger (fromIntegral (_peStep pe)))]
        return (VObject (M.fromList ps))
      Nothing -> throwUserRecoverableError info $ UserEnforceError "pact-state: no pact exec in context"

coreplEvalEnvStackFrame :: NativeFunction ReplRuntime ReplCoreBuiltin FileLocSpanInfo
coreplEvalEnvStackFrame info b _env = \case
  [] -> do
    sfs <- fmap (PString . T.pack . show) <$> use esStack
    return $ VList (V.fromList sfs)
  args -> argsError info b args

envEvents :: NativeFunction ReplRuntime ReplCoreBuiltin FileLocSpanInfo
envEvents info b _env = \case
  [VBool clear] -> do
    events <- reverse . fmap envToObj <$> use esEvents
    when clear $ esEvents .= []
    return (VList (V.fromList events))
    where
    envToObj (PactEvent name args mn mh) =
      PObject
      $ M.fromList
      $ over _1 Field
      <$> [ ("name", PString (renderQualName (QualifiedName name mn)))
        , ("params", PList (V.fromList args))
        , ("module-hash", PString (hashToText (_mhHash mh)))]
  args -> argsError info b args

envHash :: NativeFunction ReplRuntime ReplCoreBuiltin FileLocSpanInfo
envHash info b _env = \case
  [VString s] -> do
    case decodeBase64UrlUnpadded (T.encodeUtf8 s) of
      Left e -> throwUserRecoverableError info $ UserEnforceError (T.pack e)
      Right hs -> do
        (replEvalEnv . eeHash) .== Hash (toShort hs)
        return $ VString $ "Set tx hash to " <> s
  args -> argsError info b args

envData :: NativeFunction ReplRuntime ReplCoreBuiltin FileLocSpanInfo
envData info b _env = \case
  [VPactValue pv] -> do
    -- to mimic prod, we must roundtrip here
    -- if it fails silently, this is fine.
    let pv' = fromMaybe pv (roundtripStable pv)
    (replEvalEnv . eeMsgBody) .== pv'
    return (VString "Setting transaction data")
  args -> argsError info b args

envChainData :: NativeFunction ReplRuntime ReplCoreBuiltin FileLocSpanInfo
envChainData info b _env = \case
  [VObject cdataObj] -> do
    pd <- viewEvalEnv eePublicData
    go pd (M.toList cdataObj)
    where
    go pd [] = do
      replEvalEnv . eePublicData .== pd
      return (VString "Updated public metadata")
    go pd ((k,v):rest) = case v of
      PInteger i
        | k == cdGasLimit ->
          go (set (pdPublicMeta . pmGasLimit) (GasLimit (Gas (fromIntegral i))) pd) rest
        | k == cdBlockHeight ->
          go (set pdBlockHeight (fromInteger i) pd) rest
      PDecimal i
        | k == cdGasPrice ->
          go (set (pdPublicMeta . pmGasPrice) (GasPrice i) pd) rest
      PString s
        | k == cdChainId ->
          go (set (pdPublicMeta . pmChainId) (ChainId s) pd) rest
        | k == cdSender ->
          go (set (pdPublicMeta . pmSender) s pd) rest
        | k == cdPrevBlockHash ->
          go (set pdPrevBlockHash s pd) rest
      PTime time
        | k == cdBlockTime -> go (set pdBlockTime (PactTime.toPosixTimestampMicros time) pd) rest
      _ ->
        throwUserRecoverableError info $ UserEnforceError $ "envChainData: bad public metadata value for key: " <> _field k
  args -> argsError info b args

envKeys :: NativeFunction ReplRuntime ReplCoreBuiltin FileLocSpanInfo
envKeys info b _env = \case
  [VList ks] -> do
    keys <- traverse (asString info b) ks
    replEvalEnv . eeMsgSigs .== M.fromList ((,mempty) . PublicKeyText <$> V.toList keys)
    return (VString "Setting transaction keys")
  args -> argsError info b args

envSigs :: NativeFunction ReplRuntime ReplCoreBuiltin FileLocSpanInfo
envSigs info b _env = \case
  [VList ks] ->
    case traverse keyCapObj ks of
      Just sigs -> do
        (replEvalEnv . eeMsgSigs) .== M.fromList (V.toList sigs)
        return $ VString "Setting transaction signatures/caps"
      Nothing -> throwUserRecoverableError info $
        UserEnforceError ("env-sigs: Expected object with 'key': string, 'caps': [capability]")
    where
    keyCapObj = \case
      PObject o -> do
        keyRaw <- M.lookup (Field "key") o
        kt <- preview (_PLiteral . _LString) keyRaw
        capsRaw <- M.lookup (Field "caps") o
        capsListPV <- preview _PList capsRaw
        caps <- traverse (preview _PCapToken) capsListPV
        let cts = over ctName fqnToQualName <$> caps
        pure (PublicKeyText kt, S.fromList (V.toList cts))
      _ -> Nothing
  args -> argsError info b args

beginTx :: NativeFunction ReplRuntime ReplCoreBuiltin FileLocSpanInfo
beginTx info b _env = \case
  [VString s] -> begin' info (Just s) >>= renderTx info "Begin Tx"
  [] -> begin' info Nothing >>= renderTx info "Begin Tx"
  args -> argsError info b args

renderTx :: i -> Text -> Maybe (TxId, Maybe Text) -> EvalM e b i (EvalValue e b i)
renderTx _info start (Just (TxId tid, mt)) =
  return $ VString $ start <> " " <> T.pack (show tid) <> maybe mempty (" " <>) mt
renderTx info start Nothing =
  throwUserRecoverableError info $ UserEnforceError ("tx-function failure " <> start)

begin' :: FileLocSpanInfo -> Maybe Text -> ReplM b (Maybe (TxId, Maybe Text))
begin' info mt = do
  pdb <- useReplState (replEvalEnv . eePactDb)
  mode <- viewEvalEnv eeMode
  mTxId <- liftGasM info (_pdbBeginTx pdb mode)
  replTx .== ((,mt) <$> mTxId)
  return ((,mt) <$> mTxId)


envSetDebug :: NativeFunction 'ReplRuntime ReplCoreBuiltin FileLocSpanInfo
envSetDebug info b _env = \case
  [VString flag] -> do
    -- Note: if these change, please change `env-set-debug-flags.md`
    flags <- case T.strip flag of
        "lexer" -> pure $ S.singleton ReplDebugLexer
        "parser" -> pure $ S.singleton ReplDebugParser
        "desugar" -> pure $ S.singleton ReplDebugDesugar
        "all" -> pure $ S.fromList [minBound .. maxBound]
        "none" -> pure $ mempty
        f -> throwExecutionError info $
          NativeExecutionError (builtinName b) $
          "unrecognized flag in env-set-debug: " <> f
    currFlags <- useReplState replFlags
    flagsSet <-
      if S.null flags then do
        replFlags .== mempty
        pure mempty
      else do
        let flagsToSet = S.difference (S.union currFlags flags) (S.intersection currFlags flags)
        replFlags .== flagsToSet
        pure flagsToSet
    replPrintLn' info $ renderCompactText' $ "set debug flags to " <> pretty (S.toList flagsSet)
    return VUnit
  args -> argsError info b args

commitTx :: NativeFunction ReplRuntime ReplCoreBuiltin FileLocSpanInfo
commitTx info b _env = \case
  [] -> do
    pdb <- useReplState (replEvalEnv . eePactDb)
    _txLog <- liftGasM info (_pdbCommitTx pdb)
    emptyTxState
    useReplState replTx >>= \case
      Just tx -> do
        replTx .== Nothing
        renderTx info "Commit Tx" (Just tx)
      Nothing -> renderTx info "Commit Tx" Nothing
  args -> argsError info b args


rollbackTx :: NativeFunction ReplRuntime ReplCoreBuiltin FileLocSpanInfo
rollbackTx info b _env = \case
  [] -> do
    pdb <- useReplState (replEvalEnv . eePactDb)
    liftGasM info (_pdbRollbackTx pdb)
    emptyTxState
    useReplState replTx >>= \case
      Just tx -> do
        replTx .== Nothing
        renderTx info "Rollback Tx" (Just tx)
      Nothing -> renderTx info "Rollback Tx" Nothing
  args -> argsError info b args

sigKeyset :: NativeFunction ReplRuntime ReplCoreBuiltin FileLocSpanInfo
sigKeyset info b _env = \case
  [] -> do
    sigs <- S.fromList . M.keys <$> viewEvalEnv eeMsgSigs
    return (VGuard (GKeyset (KeySet sigs KeysAll)))
  args -> argsError info b args


testCapability :: NativeFunction ReplRuntime ReplCoreBuiltin FileLocSpanInfo
testCapability info b env = \case
  [VCapToken origToken] -> do
    d <- getDefCap info (_ctName origToken)
    let cBody = Constant LUnit info
    case _dcapMeta d of
      Unmanaged -> do
        evalCap info env origToken PopCapInvoke TestCapEval cBody
      _ -> do
        -- Installed caps emit and event
        -- so we create a fake stack frame
        installCap info env origToken False *> evalCap info env origToken PopCapInvoke TestCapEval cBody
  args -> argsError info b args

envExecConfig :: NativeFunction ReplRuntime ReplCoreBuiltin FileLocSpanInfo
envExecConfig info b _env = \case
  [VList s] -> do
    s' <- traverse go (V.toList s)
    let (knownFlags, unknownFlags) = partitionEithers s'
    -- Emit warning for unknown flags
    emitPactWarning info $ UnknownReplFlags unknownFlags
    let flagSet = S.fromList knownFlags
    replEvalEnv . eeFlags .== flagSet
    replEvalEnv . eeNatives .== versionedReplNatives flagSet
    let reps = PString . flagRep <$> knownFlags
    return (VList (V.fromList reps))
    where
    go str = do
      str' <- asString info b str
      maybe (pure $ Right str') (pure . Left) (M.lookup str' flagReps)
      --failInvariant info $ "Invalid flag, allowed: " <> T.pack (show (M.keys flagReps))
  args -> argsError info b args

envNamespacePolicy :: NativeFunction ReplRuntime ReplCoreBuiltin FileLocSpanInfo
envNamespacePolicy info b _env = \case
  [VBool allowRoot, VClosure (C clo)] -> do
    let qn = fqnToQualName (_cloFqName clo)
    when (_cloArity clo /= 2) $
      throwNativeExecutionError info b "Namespace manager function has invalid argument length"
    getModuleMember info qn >>= \case
      Dfun _ -> do
        let nsp = SmartNamespacePolicy allowRoot qn
        replEvalEnv . eeNamespacePolicy .== nsp
        return (VString "Installed namespace policy")
      _ ->
        throwUserRecoverableError info $ UserEnforceError "invalid namespace manager function type"
  args -> argsError info b args

envGas :: NativeFunction ReplRuntime ReplCoreBuiltin FileLocSpanInfo
envGas info b _env = \case
  [] -> do
    Gas gas <- milliGasToGas <$> getGas
    return (VInteger (fromIntegral gas))
  [VInteger g] -> do
    putGas $ gasToMilliGas (Gas (fromInteger g))
    return $ VString $ "Set gas to " <> T.pack (show g)
  args -> argsError info b args

envMilliGas :: NativeFunction ReplRuntime ReplCoreBuiltin FileLocSpanInfo
envMilliGas info b _env = \case
  [] -> do
    MilliGas gas <- getGas
    return (VInteger (fromIntegral gas))
  [VInteger g] -> do
    putGas (MilliGas (fromIntegral g))
    return $ VString $ "Set milligas to" <> T.pack (show g)
  args -> argsError info b args

envGasLimit :: NativeFunction ReplRuntime ReplCoreBuiltin FileLocSpanInfo
envGasLimit info b _env = \case
  [VInteger g] -> do
    (replEvalEnv . eeGasEnv . geGasModel . gmGasLimit) .== Just (MilliGasLimit (gasToMilliGas (Gas (fromInteger g))))
    return $ VString $ "Set gas limit to " <> T.pack (show g)
  args -> argsError info b args

envGasLog :: NativeFunction 'ReplRuntime ReplCoreBuiltin FileLocSpanInfo
envGasLog info b _env = \case
  [] -> do
    (gasLogRef, logsJustEnabled) <- viewEvalEnv (eeGasEnv . geGasLog) >>= \case
      Just ref -> pure (ref, False)
      Nothing -> do
        ref' <- liftIO $ newIORef []
        replEvalEnv . eeGasEnv . geGasLog .== Just ref'
        pure (ref', True)
    logs <- liftIO $ readIORef gasLogRef
    liftIO $ writeIORef gasLogRef []
    if logsJustEnabled then return (VList (V.fromList [PString "Enabled gas log"]))
    else let
        total = MilliGas $ sum [g | MilliGas g <- _gleThisUsed <$> logs]
        totalLine = PString . ("TOTAL: " <> ) $ renderCompactText total
        logLines = gasLogEntrytoPactValue <$> reverse logs
        in
          return (VList $ V.fromList (totalLine:logLines))
  args -> argsError info b args

envEnableReplNatives :: NativeFunction ReplRuntime ReplCoreBuiltin FileLocSpanInfo
envEnableReplNatives info b _env = \case
  [VBool enabled] -> do
    let s = if enabled then "enabled" else "disabled"
    replNativesEnabled .== enabled
    return $ VString $ "repl natives " <> s
  args -> argsError info b args

envGasModel :: NativeFunction ReplRuntime ReplCoreBuiltin FileLocSpanInfo
envGasModel info b _env = \case
  [] -> do
    gm <- viewEvalEnv (eeGasEnv . geGasModel)
    let msg = "Current gas model is '" <> _gmName gm <> "': " <> _gmDesc gm
    return (VString msg)
  args@[VString model] -> do
    gm <- viewEvalEnv (eeGasEnv . geGasModel)
    newmodel' <- case model of
      "table" -> pure $ replTableGasModel (_gmGasLimit gm)
      _ -> argsError info b args
    replEvalEnv . eeGasEnv . geGasModel .== newmodel'
    return $ VString $ "Set gas model to " <> _gmDesc newmodel'
  args -> argsError info b args


-----------------------------------
-- Pact Version
-----------------------------------

coreVersion :: IsBuiltin b => NativeFunction e b i
coreVersion info b _env = \case
  [] -> let
    v = T.pack (V.showVersion PI.version)
    in return (VString v)
  args -> argsError info b args


coreEnforceVersion :: IsBuiltin b => NativeFunction e b i
coreEnforceVersion info b _env = \case
  [VString lowerBound] -> do
    lowerBound' <- mkVersion lowerBound
    if lowerBound' <= PI.version
      then return (VBool True)
      else throwExecutionError info (EnforcePactVersionFailure lowerBound' Nothing)
  [VString lowerBound, VString upperBound] -> do
    lowerBound' <- mkVersion lowerBound
    upperBound' <- mkVersion upperBound
    if lowerBound' <= PI.version && PI.version <= upperBound'
      then return (VBool True)
      else throwExecutionError info (EnforcePactVersionFailure lowerBound' (Just upperBound'))
  args -> argsError info b args
  where
    mkVersion s =
      case A.parseOnly ((A.decimal `A.sepBy` A.char '.') <* A.endOfInput) s of
        Left _msg -> throwExecutionError info (EnforcePactVersionParseFailure s)
        Right li -> pure (V.makeVersion li)

envModuleAdmin ::  NativeFunction ReplRuntime ReplCoreBuiltin FileLocSpanInfo
envModuleAdmin info b _env = \case
  [VModRef modRef] -> do
    let modName = _mrModule modRef
    (esCaps . csModuleAdmin) %= S.insert modName
    return $ VString $ "Acquired module admin for: " <> renderModuleName modName
  args -> argsError info b args

envVerifiers :: NativeFunction ReplRuntime ReplCoreBuiltin FileLocSpanInfo
envVerifiers info b _env = \case
  [VList ks] ->
    case traverse verifCapObj ks of
      Just sigs -> do
        (replEvalEnv . eeMsgVerifiers) .== M.fromList (V.toList sigs)
        return $ VString "Setting transaction verifiers/caps"
      Nothing ->
        throwNativeExecutionError info b ("Expected object with 'name': string, 'caps': [capability]")
    where
    verifCapObj = \case
      PObject o -> do
        keyRaw <- M.lookup (Field "name") o
        kt <- preview (_PLiteral . _LString) keyRaw
        capsRaw <- M.lookup (Field "caps") o
        capsListPV <- preview _PList capsRaw
        caps <- traverse (preview _PCapToken) capsListPV
        let cts = over ctName fqnToQualName <$> caps
        pure (VerifierName kt, S.fromList (V.toList cts))
      _ -> Nothing
  args -> argsError info b args

load :: NativeFunction 'ReplRuntime ReplCoreBuiltin FileLocSpanInfo
load info b _env = \case
  [VString s] -> load' s False
  [VString s, VBool reset] -> load' s reset
  args -> argsError info b args
  where
    load' sourceFile reset = do
      replPrintLn info $ PString $ "Loading " <> sourceFile <> "..."
      fload <- useReplState replLoad
      fload (T.unpack sourceFile) reset
      return VUnit

replBuiltinEnv
  :: BuiltinEnv ReplRuntime (ReplBuiltin CoreBuiltin) FileLocSpanInfo
replBuiltinEnv i b env =
  mkDirectBuiltinFn i b env (replCoreBuiltinRuntime b)

replCoreBuiltinRuntime
  :: ReplBuiltin CoreBuiltin
  -> NativeFunction ReplRuntime (ReplBuiltin CoreBuiltin) FileLocSpanInfo
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
    RPactVersion -> coreVersion
    REnforcePactVersionMin -> coreEnforceVersion
    REnforcePactVersionRange -> coreEnforceVersion
    REnvEnableReplNatives -> envEnableReplNatives
    REnvModuleAdmin -> envModuleAdmin
    REnvVerifiers -> envVerifiers
    REnvSetDebugFlag -> envSetDebug
    RLoad -> load
    RLoadWithEnv -> load
