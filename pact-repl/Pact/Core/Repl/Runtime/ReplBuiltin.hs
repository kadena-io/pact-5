{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}

module Pact.Core.Repl.Runtime.ReplBuiltin where

import Control.Lens
import Control.Monad(when)
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Default
import Data.Text(Text)
import Data.Maybe(fromMaybe)
import Data.Either (partitionEithers)
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
import Pact.Core.IR.Eval.CEK.CoreBuiltin
import Pact.Core.IR.Eval.CEK.Evaluator
import Pact.Core.Names
import Pact.Core.Pretty
import Pact.Core.Environment
import Pact.Core.PactValue
import Pact.Core.Gas
import Pact.Core.Guards
import Pact.Core.ModRefs
import Pact.Core.Capabilities
import Pact.Core.Errors
import Pact.Core.Persistence
import Pact.Core.IR.Term
import Pact.Core.Info
import Pact.Core.Namespace
import Pact.Core.StableEncoding

import qualified Pact.Core.Version as PI
import qualified Data.Version as V
import qualified Data.Attoparsec.Text as A


import Pact.Core.Repl.Utils
import qualified Pact.Time as PactTime
import Data.IORef

prettyShowValue :: CEKValue b i m -> Text
prettyShowValue = \case
  VPactValue p -> renderText p
  VClosure _ -> "<#closure>"

corePrint :: NativeFunction 'ReplRuntime ReplCoreBuiltin FileLocSpanInfo
corePrint info b cont handler _env = \case
  [v] -> do
    replPrintLn' info (prettyShowValue v)
    returnCEKValue cont handler (VLiteral LUnit)
  args -> argsError info b args

returnTestFailure
  :: IsBuiltin b
  => FileLocSpanInfo
  -> Text
  -> Cont ReplRuntime b FileLocSpanInfo
  -> CEKErrorHandler ReplRuntime b FileLocSpanInfo
  -> Text
  -> EvalM ReplRuntime b FileLocSpanInfo (EvalResult ReplRuntime b FileLocSpanInfo)
returnTestFailure info testName cont handler msg = do
  recordTestFailure testName info msg
  returnCEKValue cont handler (VLiteral (LString msg))

returnTestSuccess
  :: IsBuiltin b
  => FileLocSpanInfo
  -> Text
  -> Cont ReplRuntime b FileLocSpanInfo
  -> CEKErrorHandler ReplRuntime b FileLocSpanInfo
  -> Text
  -> EvalM ReplRuntime b FileLocSpanInfo (EvalResult ReplRuntime b FileLocSpanInfo)
returnTestSuccess info testName cont handler msg = do
  recordTestSuccess testName info
  returnCEKValue cont handler (VString msg)


coreExpect :: NativeFunction 'ReplRuntime ReplCoreBuiltin FileLocSpanInfo
coreExpect info b cont handler _env = \case
  [VLiteral (LString testName), VClosure expected, VClosure provided] -> do
    -- Get the state of execution before running the test
    es <- get
    tryError (applyLamUnsafe provided [] Mt CEKNoHandler) >>= \case
      Right (EvalValue (VPactValue v2)) -> do
        applyLamUnsafe expected [] Mt CEKNoHandler >>= \case
          EvalValue (VPactValue v1) -> do
            -- If v1 /= v2, the test has failed
            if v1 /= v2 then do
                let v1s = prettyShowValue (VPactValue v1)
                    v2s = prettyShowValue (VPactValue v2)
                let failureMsg = "FAILURE: " <> testName <> " expected: " <> v1s <> ", received: " <> v2s
                returnTestFailure info testName cont handler failureMsg
            else returnTestSuccess info testName cont handler ("Expect: success " <> testName)
          _ -> do
            recordTestFailure testName info "FAILURE: expect expression did not return a pact value for comparison"
            returnCEKError info cont handler $ UserEnforceError "evaluation within expect did not return a pact value"
      Right (VError _ errMsg _) -> do
        -- Restore state on error, which resets things like cap grants, etc.
        put es
        let failureMsg = "FAILURE: " <> testName <> " evaluation of actual failed with error message: " <> renderCompactText errMsg
        returnTestFailure info testName cont handler failureMsg
      Right _v -> do
        recordTestFailure testName info "FAILURE: expect expression did not return a pact value for comparison"
        returnCEKError info cont handler $ UserEnforceError "FAILURE: expect expression did not return a pact value for comparison"
      Left err -> do
        put es
        let failureMsg = "FAILURE: " <> testName <> " evaluation of actual failed with error message:\n" <> renderCompactText err
        returnTestFailure info testName cont handler failureMsg
  args -> argsError info b args

coreExpectThat :: NativeFunction 'ReplRuntime ReplCoreBuiltin FileLocSpanInfo
coreExpectThat info b cont handler _env = \case
  [VLiteral (LString testName), VClosure vclo, v] -> do
    applyLamUnsafe vclo [v] Mt CEKNoHandler >>= \case
      EvalValue (VBool c) ->
        if c then do
          let successMsg = "Expect-that: success " <> testName
          returnTestSuccess info testName cont handler successMsg
        else do
          let failureMsg = "FAILURE: Expect-that: Did not satisfy condition: " <> testName
          returnTestFailure info testName cont handler failureMsg
      EvalValue _ -> do
        recordTestFailure testName info "FAILURE: expect-that expression did not return a boolean"
        throwNativeExecutionError info b "Expect-that: condition did not return a boolean"
      ve@(VError _ errMsg _) -> do
        let failureMsg = "FAILURE: Expect-that: condition failed with error: " <> renderCompactText errMsg
        recordTestFailure testName info failureMsg
        returnCEK cont handler ve
  args -> argsError info b args

coreExpectFailure :: NativeFunction 'ReplRuntime ReplCoreBuiltin FileLocSpanInfo
coreExpectFailure info b cont handler _env = \case
  [VString testName, VClosure vclo] -> do
    es <- get
    tryError (applyLamUnsafe vclo [] Mt CEKNoHandler) >>= \case
      Right (VError _ _ _) -> do
        put es
        returnTestSuccess info testName cont handler $ "Expect failure: Success: " <> testName
      Left _err -> do
        put es
        returnTestSuccess info testName cont handler $ "Expect failure: Success: " <> testName
      Right _ ->
        returnTestFailure info testName cont handler $ "FAILURE: " <> testName <> ": expected failure, got result"
  [VString testName, VString toMatch, VClosure vclo] -> do
    es <- get
    tryError (applyLamUnsafe vclo [] Mt CEKNoHandler) >>= \case
      Right (VError _ errMsg _) -> do
        put es
        let err = renderCompactText errMsg
        if toMatch `T.isInfixOf` err
          then returnTestSuccess info testName cont handler $ "Expect failure: Success: " <> testName
          else returnTestFailure info testName cont handler $
               "FAILURE: " <> testName <> ": expected error message '" <> toMatch <> "', got '" <> err <> "'"
      Left errMsg -> do
        put es
        let err = renderCompactText errMsg
        if toMatch `T.isInfixOf` err
          then returnTestSuccess info testName cont handler $ "Expect failure: Success: " <> testName
          else returnTestFailure info testName cont handler $
               "FAILURE: " <> testName <> ": expected error message '" <> toMatch <> "', got '" <> err <> "'"
      Right (EvalValue v) ->
        returnTestFailure info testName cont handler $ "FAILURE: " <> toMatch <> ": expected failure, got result: " <> prettyShowValue v
  args -> argsError info b args


continuePact :: NativeFunction 'ReplRuntime ReplCoreBuiltin FileLocSpanInfo
continuePact info b cont handler env = \case
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
      merr <- tryError $ resumePact info Mt CEKNoHandler env Nothing
      replEvalEnv . eeDefPactStep .== Nothing
      v <- liftEither merr
      returnCEK cont handler v

pactState :: NativeFunction 'ReplRuntime ReplCoreBuiltin FileLocSpanInfo
pactState info b cont handler _env = \case
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
        returnCEKValue cont handler (VObject (M.fromList ps))
      Nothing -> returnCEKError info cont handler $ UserEnforceError "pact-state: no pact exec in context"

coreplEvalEnvStackFrame :: NativeFunction 'ReplRuntime ReplCoreBuiltin FileLocSpanInfo
coreplEvalEnvStackFrame info b cont handler _env = \case
  [] -> do
    sfs <- fmap (PString . T.pack . show) <$> use esStack
    returnCEKValue cont handler $ VList (V.fromList sfs)
  args -> argsError info b args

envEvents :: NativeFunction 'ReplRuntime ReplCoreBuiltin FileLocSpanInfo
envEvents info b cont handler _env = \case
  [VBool clear] -> do
    events <- reverse . fmap envToObj <$> use esEvents
    when clear $ esEvents .= []
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

envHash :: NativeFunction 'ReplRuntime ReplCoreBuiltin FileLocSpanInfo
envHash info b cont handler _env = \case
  [VString s] -> do
    case decodeBase64UrlUnpadded (T.encodeUtf8 s) of
      Left e -> returnCEKError info cont handler $ UserEnforceError (T.pack e)
      Right hs -> do
        (replEvalEnv . eeHash) .== Hash (toShort hs)
        returnCEKValue cont handler $ VString $ "Set tx hash to " <> s
  args -> argsError info b args

envData :: NativeFunction 'ReplRuntime ReplCoreBuiltin FileLocSpanInfo
envData info b cont handler _env = \case
  [VPactValue pv] -> do
    -- to mimic prod, we must roundtrip here
    -- if it fails silently, this is fine.
    let pv' = fromMaybe pv (roundtripStable pv)
    (replEvalEnv . eeMsgBody) .== pv'
    returnCEKValue cont handler (VString "Setting transaction data")
  args -> argsError info b args

envChainData :: NativeFunction 'ReplRuntime ReplCoreBuiltin FileLocSpanInfo
envChainData info b cont handler _env = \case
  [VObject cdataObj] -> do
    pd <- viewEvalEnv eePublicData
    go pd (M.toList cdataObj)
    where
    go pd [] = do
      replEvalEnv . eePublicData .== pd
      returnCEKValue cont handler (VString "Updated public metadata")
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
      _ -> returnCEKError info cont handler $ UserEnforceError $ "envChainData: bad public metadata value for key: " <> _field k
  args -> argsError info b args

envKeys :: NativeFunction 'ReplRuntime ReplCoreBuiltin FileLocSpanInfo
envKeys info b cont handler _env = \case
  [VList ks] -> do
    keys <- traverse (asString info b) ks
    replEvalEnv . eeMsgSigs .== M.fromList ((,mempty) . PublicKeyText <$> V.toList keys)
    returnCEKValue cont handler (VString "Setting transaction keys")
  args -> argsError info b args

envSigs :: NativeFunction 'ReplRuntime ReplCoreBuiltin FileLocSpanInfo
envSigs info b cont handler _env = \case
  [VList ks] ->
    case traverse keyCapObj ks of
      Just sigs -> do
        (replEvalEnv . eeMsgSigs) .== M.fromList (V.toList sigs)
        returnCEKValue cont handler $ VString "Setting transaction signatures/caps"
      Nothing -> returnCEKError info cont handler $ UserEnforceError ("env-sigs: Expected object with 'key': string, 'caps': [capability]")
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

envVerifiers :: NativeFunction 'ReplRuntime ReplCoreBuiltin FileLocSpanInfo
envVerifiers info b cont handler _env = \case
  [VList ks] ->
    case traverse verifCapObj ks of
      Just sigs -> do
        (replEvalEnv . eeMsgVerifiers) .== M.fromList (V.toList sigs)
        returnCEKValue cont handler $ VString "Setting transaction verifiers/caps"
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

beginTx :: NativeFunction 'ReplRuntime ReplCoreBuiltin FileLocSpanInfo
beginTx info b cont handler _env = \case
  [VString s] -> begin' info (Just s) >>= returnCEK cont handler . renderTx info "Begin Tx"
  [] -> begin' info Nothing >>= returnCEK cont handler . renderTx info "Begin Tx"
  args -> argsError info b args

renderTx :: i -> Text -> Maybe (TxId, Maybe Text) -> EvalResult e b i
renderTx _info start (Just (TxId tid, mt)) =
  EvalValue $ VString $ start <> " " <> T.pack (show tid) <> maybe mempty (" " <>) mt
renderTx info start Nothing = VError [] (UserEnforceError ("tx-function failure " <> start)) info

begin' :: FileLocSpanInfo -> Maybe Text -> ReplM b (Maybe (TxId, Maybe Text))
begin' info mt = do
  pdb <- useReplState (replEvalEnv . eePactDb)
  mode <- viewEvalEnv eeMode
  mTxId <- liftGasM info (_pdbBeginTx pdb mode)
  replTx .== ((,mt) <$> mTxId)
  return ((,mt) <$> mTxId)

emptyTxState :: ReplM b ()
emptyTxState = do
  fqdefs <- use (esLoaded . loAllLoaded)
  cs <- use esStack
  esc <- use esCheckRecursion
  let newEvalState =
        set esStack cs
        $ set (esLoaded . loAllLoaded) fqdefs
        $ set esCheckRecursion esc def
  put newEvalState


commitTx :: NativeFunction 'ReplRuntime ReplCoreBuiltin FileLocSpanInfo
commitTx info b cont handler _env = \case
  [] -> do
    pdb <- useReplState (replEvalEnv . eePactDb)
    _txLog <- liftGasM info (_pdbCommitTx pdb)
    emptyTxState
    useReplState replTx >>= \case
      Just tx -> do
        replTx .== Nothing
        returnCEK cont handler (renderTx info "Commit Tx" (Just tx))
      Nothing -> returnCEK cont handler (renderTx info "Commit Tx" Nothing)
  args -> argsError info b args


rollbackTx :: NativeFunction 'ReplRuntime ReplCoreBuiltin FileLocSpanInfo
rollbackTx info b cont handler _env = \case
  [] -> do
    pdb <- useReplState (replEvalEnv . eePactDb)
    liftGasM info (_pdbRollbackTx pdb)
    emptyTxState
    useReplState replTx >>= \case
      Just tx -> do
        replTx .== Nothing
        returnCEK cont handler (renderTx info "Rollback Tx" (Just tx))
      Nothing -> returnCEK cont handler (renderTx info "Rollback Tx" Nothing)
  args -> argsError info b args

sigKeyset :: NativeFunction 'ReplRuntime ReplCoreBuiltin FileLocSpanInfo
sigKeyset info b cont handler _env = \case
  [] -> do
    sigs <- S.fromList . M.keys <$> viewEvalEnv eeMsgSigs
    returnCEKValue cont handler (VGuard (GKeyset (KeySet sigs KeysAll)))
  args -> argsError info b args


testCapability :: NativeFunction 'ReplRuntime ReplCoreBuiltin FileLocSpanInfo
testCapability info b cont handler env = \case
  [VCapToken origToken] -> do
    d <- getDefCap info (_ctName origToken)
    let cBody = Constant LUnit info
        cont' = SeqC env info cBody cont
    case _dcapMeta d of
      Unmanaged ->
        evalCap info cont' handler env origToken PopCapInvoke TestCapEval cBody
      _ -> do
        -- Installed caps emit and event
        -- so we create a fake stack frame
        installCap info env origToken False *> evalCap info cont' handler env origToken PopCapInvoke TestCapEval cBody
  args -> argsError info b args

envExecConfig :: NativeFunction 'ReplRuntime ReplCoreBuiltin FileLocSpanInfo
envExecConfig info b cont handler _env = \case
  [VList s] -> do
    s' <- traverse go (V.toList s)
    let (knownFlags, unknownFlags) = partitionEithers s'
    -- Emit warning for unknown flags
    emitPactWarning info $ UnknownReplFlags unknownFlags

    let flagSet = S.fromList knownFlags
    replEvalEnv . eeFlags .== flagSet
    replEvalEnv . eeNatives .== versionedReplNatives flagSet
    let reps = PString . flagRep <$> knownFlags
    returnCEKValue cont handler (VList (V.fromList reps))
    where
    go str = do
      str' <- asString info b str
      maybe (pure $ Right str') (pure . Left) (M.lookup str' flagReps)

  args -> argsError info b args

envNamespacePolicy :: NativeFunction 'ReplRuntime ReplCoreBuiltin FileLocSpanInfo
envNamespacePolicy info b cont handler _env = \case
  [VBool allowRoot, VClosure (C clo)] -> do
    let qn = fqnToQualName (_cloFqName clo)
    when (_cloArity clo /= 2) $ throwNativeExecutionError info b "Namespace manager function has invalid argument length"
    getModuleMember info qn >>= \case
      Dfun _ -> do
        let nsp = SmartNamespacePolicy allowRoot qn
        replEvalEnv . eeNamespacePolicy .== nsp
        returnCEKValue cont handler (VString "Installed namespace policy")
      _ -> returnCEKError info cont handler $ UserEnforceError "invalid namespace manager function type"
  args -> argsError info b args

envGas :: NativeFunction 'ReplRuntime ReplCoreBuiltin FileLocSpanInfo
envGas info b cont handler _env = \case
  [] -> do
    Gas gas <- milliGasToGas <$> getGas
    returnCEKValue cont handler (VInteger (fromIntegral gas))
  [VInteger g] -> do
    putGas $ gasToMilliGas (Gas (fromInteger g))
    returnCEKValue cont handler $ VString $ "Set gas to " <> T.pack (show g)
  args -> argsError info b args

envMilliGas :: NativeFunction 'ReplRuntime ReplCoreBuiltin FileLocSpanInfo
envMilliGas info b cont handler _env = \case
  [] -> do
    MilliGas gas <- getGas
    returnCEKValue cont handler (VInteger (fromIntegral gas))
  [VInteger g] -> do
    putGas (MilliGas (fromIntegral g))
    returnCEKValue cont handler $ VString $ "Set milligas to " <> T.pack (show g)
  args -> argsError info b args

envGasLimit :: NativeFunction 'ReplRuntime ReplCoreBuiltin FileLocSpanInfo
envGasLimit info b cont handler _env = \case
  [VInteger g] -> do
    (replEvalEnv . eeGasEnv . geGasModel . gmGasLimit) .== Just (MilliGasLimit (gasToMilliGas (Gas (fromInteger g))))
    returnCEKValue cont handler $ VString $ "Set gas limit to " <> T.pack (show g)
  args -> argsError info b args

envGasLog :: NativeFunction 'ReplRuntime ReplCoreBuiltin FileLocSpanInfo
envGasLog info b cont handler _env = \case
  [] -> do
    (gasLogRef, logsJustEnabled) <- viewEvalEnv (eeGasEnv . geGasLog) >>= \case
      Just ref -> pure (ref, False)
      Nothing -> do
        ref' <- liftIO $ newIORef []
        replEvalEnv . eeGasEnv . geGasLog .== Just ref'
        pure (ref', True)
    logs <- liftIO $ readIORef gasLogRef
    liftIO $ writeIORef gasLogRef []
    if logsJustEnabled then returnCEKValue cont handler (VList (V.fromList [PString "Enabled gas log"]))
    else let
        total = MilliGas $ sum [g | MilliGas g <- _gleThisUsed <$> logs]
        totalLine = PString . ("TOTAL: " <> ) $ renderCompactText total
        logLines = gasLogEntrytoPactValue <$> reverse logs
        in
          returnCEKValue cont handler (VList $ V.fromList (totalLine:logLines))
  args -> argsError info b args

envEnableReplNatives :: NativeFunction 'ReplRuntime ReplCoreBuiltin FileLocSpanInfo
envEnableReplNatives info b cont handler _env = \case
  [VBool enabled] -> do
    let s = if enabled then "enabled" else "disabled"
    replNativesEnabled .== enabled
    returnCEKValue cont handler $ VString $ "repl natives " <> s
  args -> argsError info b args

envGasModel :: NativeFunction 'ReplRuntime ReplCoreBuiltin FileLocSpanInfo
envGasModel info b cont handler _env = \case
  [] -> do
    gm <- viewEvalEnv (eeGasEnv . geGasModel)
    let msg = "Current gas model is '" <> _gmName gm <> "': " <> _gmDesc gm
    returnCEKValue cont handler (VString msg)
  args@[VString model] -> do
    gm <- viewEvalEnv (eeGasEnv . geGasModel)
    newmodel' <- case model of
      "table" ->
        pure $ replTableGasModel (_gmGasLimit gm)
      _ -> argsError info b args
    replEvalEnv . eeGasEnv . geGasModel .== newmodel'
    returnCEKValue cont handler $ VString $ "Set gas model to " <> _gmDesc newmodel'
  args -> argsError info b args


envModuleAdmin :: NativeFunction 'ReplRuntime ReplCoreBuiltin FileLocSpanInfo
envModuleAdmin info b cont handler _env = \case
  [VModRef modRef] -> do
    let modName = _mrModule modRef
    (esCaps . csModuleAdmin) %= S.insert modName
    returnCEKValue cont handler $ VString $ "Acquired module admin for: " <> renderModuleName modName
  args -> argsError info b args


-----------------------------------
-- Pact Version
-----------------------------------

coreVersion :: (IsBuiltin b) => NativeFunction e b i
coreVersion info b  cont handler _env = \case
  [] -> let
    v = T.pack (V.showVersion PI.version)
    in returnCEKValue cont handler (VString v)
  args -> argsError info b args

envSetDebug :: NativeFunction 'ReplRuntime ReplCoreBuiltin FileLocSpanInfo
envSetDebug info b cont handler _env = \case
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
    returnCEKValue cont handler $ VUnit
  args -> argsError info b args


coreEnforceVersion :: (IsBuiltin b) => NativeFunction e b i
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

load :: NativeFunction 'ReplRuntime ReplCoreBuiltin FileLocSpanInfo
load info b cont handler _env = \case
  [VString s] -> load' s False
  [VString s, VBool reset] -> load' s reset
  args -> argsError info b args
  where
    load' sourceFile reset = do
      replPrintLn info $ PString $ "Loading " <> sourceFile <> "..."
      fload <- useReplState replLoad
      fload (T.unpack sourceFile) reset
      returnCEKValue cont handler VUnit


replBuiltinEnv
  :: BuiltinEnv 'ReplRuntime (ReplBuiltin CoreBuiltin) FileLocSpanInfo
replBuiltinEnv i b env =
  mkBuiltinFn i b env (replCoreBuiltinRuntime b)

replCoreBuiltinRuntime
  :: ReplBuiltin CoreBuiltin
  -> NativeFunction 'ReplRuntime (ReplBuiltin CoreBuiltin) FileLocSpanInfo
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
