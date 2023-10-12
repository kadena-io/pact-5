{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pact.Core.IR.Eval.CEK
  ( eval
  , returnCEKValue
  , returnCEK
  , applyLam
  , throwExecutionError
  , unsafeApplyOne
  , unsafeApplyTwo
  , mkDefPactClosure
  , resumePact
  , evalCap
  , nameToFQN
  , guardTable
  , enforceKeyset
  , enforceKeysetName
  , requireCap
  , installCap
  , composeCap
  , emitEvent
  , mkDefunClosure
  , enforceNotWithinDefcap
  , acquireModuleAdmin
  , isCapInStack
  , filterIndex
  , findMsgSigCap
  , evalWithStackFrame) where


import Control.Lens hiding ((%%=))
import Control.Monad(zipWithM, unless, when)
import Control.Monad.IO.Class
import Data.Default
import Data.List.NonEmpty(NonEmpty(..))
import Data.Foldable(find, foldl')
import qualified Data.RAList as RAList
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.List.NonEmpty as NE

import Pact.Core.Builtin
import Pact.Core.Names
import Pact.Core.Errors
import Pact.Core.Gas
import Pact.Core.Literal
import Pact.Core.PactValue
import Pact.Core.Capabilities
import Pact.Core.Type
import Pact.Core.Guards
import Pact.Core.ModRefs
import Pact.Core.Environment
import Pact.Core.Persistence
import Pact.Core.Hash

import Pact.Core.IR.Term hiding (PactStep)
import Pact.Core.IR.Eval.Runtime
import Pact.Core.Pacts.Types 
chargeNodeGas :: MonadEval b i m => NodeType -> m ()
chargeNodeGas _nt = pure ()
  -- gm <- view (eeGasModel . geGasModel . gmNodes) <$> readEnv
  -- chargeGas (gm nt)

eval
  :: forall b i m. (MonadEval b i m)
  => CEKEnv b i m
  -> EvalTerm b i
  -> m (EvalResult b i m)
eval = evalCEK Mt CEKNoHandler

evalCEK
  :: (MonadEval b i m)
  => Cont b i m
  -> CEKErrorHandler b i m
  -> CEKEnv b i m
  -> EvalTerm b i
  -> m (EvalResult b i m)
evalCEK cont handler env (Var n info)  = do
  chargeNodeGas VarNode
  case _nKind n of
    NBound i -> case RAList.lookup (view ceLocal env) i of
      -- Todo: module ref anns here
      Just v -> returnCEKValue cont handler v
      Nothing -> failInvariant info ("unbound identifier" <> T.pack (show n))
    -- Top level names are not closures, so we wipe the env
    NTopLevel mname mh -> do
      let fqn = FullyQualifiedName mname (_nName n) mh
      lookupFqName fqn >>= \case
        Just (Dfun d) -> do
          dfunClo <- VDefClosure <$> mkDefunClosure d mname env
          returnCEKValue cont handler dfunClo
        -- Todo: this should be GADT'd out
        -- and defconsts should already be evaluated
        Just (DConst d) ->
          evalCEK cont handler (set ceLocal mempty env) (_dcTerm d)
        Just (DPact d) -> do
          dpactClo <- mkDefPactClosure info fqn d env
          returnCEKValue cont handler dpactClo
        Just (DTable d) ->
          let (ResolvedTable sc) = _dtSchema d
              tn = TableName $ renderModuleName mname <> "_" <> _dtName d
              tbl = VTable (TableValue tn mname mh sc)
          in returnCEKValue cont handler tbl
        Just (DCap d) -> do
          let args = _argType <$> _dcapArgs d
              clo = CapTokenClosure fqn args (length args) info
          returnCEKValue cont handler (VClosure (CT clo))
        Just d ->
          throwExecutionError info (InvalidDefKind (defKind d) "in var position")
        Nothing ->
          throwExecutionError info (NameNotInScope (FullyQualifiedName mname (_nName n) mh))
    NModRef m ifs -> case ifs of
      [x] -> returnCEKValue cont handler (VModRef (ModRef m ifs (Just x)))
      [] -> throwExecutionError info (ModRefNotRefined (_nName n))
      _ -> returnCEKValue cont handler (VModRef (ModRef m ifs Nothing))
    NDynRef (DynamicRef dArg i) -> case RAList.lookup (view ceLocal env) i of
      Just (VModRef mr) -> do
        modRefHash <- _mHash <$> getModule info env (_mrModule mr)
        let nk = NTopLevel (_mrModule mr) modRefHash
        evalCEK cont handler env (Var (Name dArg nk) info)
      Just _ -> returnCEK cont handler (VError "dynamic name pointed to non-modref")
      Nothing -> failInvariant info ("unbound identifier" <> T.pack (show n))

evalCEK cont handler _env (Constant l _) = do
  chargeNodeGas ConstantNode
  returnCEKValue cont handler (VLiteral l)
evalCEK cont handler env (App fn args info) = do
  chargeNodeGas AppNode
  evalCEK (Args env info args cont) handler env fn
evalCEK cont handler env (Nullary body info) = do
  chargeNodeGas LamNode
  let clo = VLamClosure (LamClosure NullaryClosure 0 body Nothing env info)
  returnCEKValue cont handler clo
evalCEK cont handler env (Let _ e1 e2 _) =
  let cont' = LetC env e2 cont
  in evalCEK cont' handler env e1
evalCEK cont handler env (Lam _ args body info) = do
  chargeNodeGas LamNode
  let clo = VLamClosure (LamClosure (ArgClosure (_argType <$> args)) (NE.length args) body Nothing env info)
  returnCEKValue cont handler clo
evalCEK cont handler env (Builtin b i) = do
  chargeNodeGas BuiltinNode
  let builtins = view ceBuiltins env
  returnCEKValue cont handler (VNative (builtins i b env))
evalCEK cont handler env (Sequence e1 e2 _) = do
  chargeNodeGas SeqNode
  evalCEK (SeqC env e2 cont) handler env e1
evalCEK cont handler env (Conditional c info) = case c of
  CAnd te te' ->
    evalCEK (CondC env info (AndFrame te') cont) handler env te
  COr te te' ->
    evalCEK (CondC env info (OrFrame te') cont) handler env te
  CIf cond e1 e2 ->
    evalCEK (CondC env info (IfFrame e1 e2) cont) handler env cond
  CEnforce cond str ->
    evalCEK (CondC env info (EnforceFrame str) cont) handler env cond
  CEnforceOne str conds -> case conds of
    [] -> returnCEK cont handler (VError "enforce-one failure")
    x:xs -> do
      cs <- useEvalState (esCaps . csSlots)
      let handler' = CEKEnforceOne env info str xs cont cs handler
      let cont' = CondC env info (EnforceOneFrame str xs) Mt
          env' = readOnlyEnv env
      evalCEK cont' handler' env' x
evalCEK cont handler env (CapabilityForm cf info) = do
  fqn <- nameToFQN info env (view capFormName cf)
  case cf of
    -- Todo: duplication here in the x:xs case
    WithCapability _ args body -> case args of
      x:xs -> do
        let capFrame = WithCapFrame fqn body
        let cont' = CapInvokeC env info xs [] capFrame cont
        evalCEK cont' handler env x
      [] -> evalCap info cont handler env (CapToken fqn []) body
    CreateUserGuard _ args -> case args of
      [] -> createUserGuard cont handler fqn []
      x : xs -> let
        capFrame = CreateUserGuardFrame fqn
        cont' = CapInvokeC env info xs [] capFrame cont
        in evalCEK cont' handler env x
evalCEK cont handler env (ListLit ts _) = do
  chargeNodeGas ListNode
  case ts of
    [] -> returnCEKValue cont handler (VList mempty)
    x:xs -> evalCEK (ListC env xs [] cont) handler env x
evalCEK cont handler env (Try catchExpr rest _) = do
  caps <- useEvalState (esCaps . csSlots)
  let handler' = CEKHandler env catchExpr cont caps handler
  let env' = readOnlyEnv env
  evalCEK Mt handler' env' rest
evalCEK cont handler env (ObjectLit o _) =
  case o of
    (f, term):rest -> do
      let cont' = ObjC env f rest [] cont
      evalCEK cont' handler env term
    [] -> returnCEKValue cont handler (VObject mempty)
-- Error terms ignore the current cont
evalCEK _ handler _ (Error e _) =
  returnCEK Mt handler (VError e)

mkDefunClosure
  :: (MonadEval b i m)
  => Defun Name Type b i
  -> ModuleName
  -> CEKEnv b i m
  -> m (Closure b i m)
mkDefunClosure d mn e = case _dfunTerm d of
  Lam _ args body i ->
    pure (Closure (_dfunName d) mn (ArgClosure (_argType <$> args)) (NE.length args) body (_dfunRType d) e i)
  Nullary body i ->
    pure (Closure (_dfunName d) mn NullaryClosure 0 body (_dfunRType d) e i)
  _ ->
    throwExecutionError (_dfunInfo d) (DefIsNotClosure (_dfunName d))

mkDefPactClosure
  :: (MonadEval b i m)
  => i
  -> FullyQualifiedName
  -> DefPact Name Type b i
  -> CEKEnv b i m
  -> m (CEKValue b i m)
mkDefPactClosure info fqn dpact env = case _dpArgs dpact of
  [] ->
    let dpc = DefPactClosure fqn NullaryClosure 0 env info
    in pure (VDefPactClosure dpc)
  (x:xs) ->
    let dpc = DefPactClosure fqn (ArgClosure (fmap _argType (x :| xs))) (length (x:xs)) env info
    in pure (VDefPactClosure dpc)


initPact
  :: MonadEval b i m
  => i
  -> PactContinuation FullyQualifiedName PactValue
  -> Cont b i m
  -> CEKErrorHandler b i m
  -> CEKEnv b i m
  -> m (EvalResult b i m)
initPact i pc cont handler cenv = do
  case view cePactStep cenv of
    Nothing ->
      let
        pId = PactId ""-- (view eeHash env)
        pStep = PactStep 0 False pId Nothing
        cenv' = set cePactStep (Just pStep) cenv
      in applyPact i pc pStep cont handler cenv'
    Just _ -> pure (VError "not implemented")

applyPact
  :: MonadEval b i m
  => i
  -> PactContinuation FullyQualifiedName PactValue
  -> PactStep
  -> Cont b i m
  -> CEKErrorHandler b i m
  -> CEKEnv b i m
  -> m (EvalResult b i m)
applyPact i pc ps cont handler cenv = useEvalState esPactExec >>= \case
  Just _ ->  throwExecutionError i MultipleOrNestedPactExecFound
  Nothing -> lookupFqName (pc ^. pcName) >>= \case
    Just (DPact defPact) -> do
      let nSteps = NE.length (_dpSteps defPact)
      -- Check we try to apply the correct pact Step
      unless (ps ^. psStep < nSteps) $
        throwExecutionError i (PactStepNotFound (ps ^. psStep))

      step <- maybe (failInvariant i "Step not found") pure
        $ _dpSteps defPact ^? ix (ps ^. psStep)

      when (ps ^. psStep /= 0) $
        failInvariant i "applyPact with stepId /= 0"
      let pe = PactExec
               { _peYield = Nothing
               , _peStepHasRollback = hasRollback step
               , _peStepCount = nSteps
               , _peStep = _psStep ps
               , _pePactId = _psPactId ps
               , _peContinuation = pc
               }
      setEvalState esPactExec (Just pe)

      let cont' = PactStepC cenv cont

      case (ps ^. psRollback, step) of
        (False, _) ->
          evalWithStackFrame i cont' handler cenv sf Nothing  (ordinaryPactStepExec step)
          -- evalCEK cont' handler cenv
        (True, StepWithRollback _ rollbackExpr _) ->
          evalWithStackFrame i cont' handler cenv sf Nothing rollbackExpr
        (True, Step{}) -> throwExecutionError i PactStepHasNoRollback
    _otherwise -> failInvariant i "DefPact not found"
  where
  sf = StackFrame (view (pcName . fqName) pc) (view (pcName . fqModule) pc) SFDefPact

resumePact
  :: MonadEval b i m
  => i
  -> Maybe PactExec
  -> m (EvalResult b i m)
resumePact i crossChainContinuation = viewCEKEnv eePactStep >>= \case
  Nothing -> throwExecutionError i StepNotInEnvironment
  Just ps -> do
    pdb <- viewCEKEnv eePactDb
    dbState <- liftIO (readPacts pdb (_psPactId ps))
    case (dbState, crossChainContinuation) of
      (Just Nothing, _) -> error "resumePact: completed"
      (Nothing, Nothing) -> error "no prev exec found"
      (Nothing, Just ccExec) -> resumePactExec ccExec
      (Just (Just dbExec), Nothing) -> resumePactExec dbExec
      (Just (Just _dbExec), Just _ccExec) -> error "not implemented"
      where
        --resumePactExec :: MonadEval b i m => PactExec -> m (EvalResult b i m)
        resumePactExec pe = do
          when (_psPactId ps /= _pePactId pe) $
            error "resumePactExec: request and context pact IDs do not match"

          -- additional checks: https://github.com/kadena-io/pact/blob/e72d86749f5d65ac8d6e07a7652dd2ffb468607b/src/Pact/Eval.hs#L1590
          let
            env = undefined
          applyPact i undefined ps Mt CEKNoHandler env


enforceKeyset
  :: MonadEval b i m
  => KeySet FullyQualifiedName
  -> m Bool
enforceKeyset (KeySet kskeys ksPred) = do
  allSigs <- viewCEKEnv eeMsgSigs
  let matchedSigs = M.filterWithKey matchKey allSigs
  sigs <- checkSigCaps matchedSigs
  runPred (M.size sigs)
  where
  matchKey k _ = k `elem` kskeys
  atLeast t m = m >= t
  count = S.size kskeys
  runPred matched =
    case ksPred of
      KeysAll -> run atLeast
      KeysAny -> run (\_ m -> atLeast 1 m)
      Keys2 -> run (\_ m -> atLeast 2 m)
    where
    run p = pure (p count matched)

enforceKeysetName
  :: MonadEval b i m
  => i
  -> CEKEnv b i m
  -> KeySetName
  -> m Bool
enforceKeysetName info env ksn = do
  let pdb = view cePactDb env
  liftIO (readKeyset pdb ksn) >>= \case
    Just ks -> enforceKeyset ks
    Nothing ->
      throwExecutionError info (NoSuchKeySet ksn)

-- Todo: fail invariant
nameToFQN
  :: MonadEval b i m
  => i
  -> CEKEnv b i m
  -> Name
  -> m FullyQualifiedName
nameToFQN info env (Name n nk) = case nk of
  NTopLevel mn mh -> pure (FullyQualifiedName mn n mh)
  NDynRef (DynamicRef dArg i) -> case RAList.lookup (view ceLocal env) i of
    Just (VModRef mr) -> do
      md <- getModule info env (_mrModule mr)
      pure (FullyQualifiedName (_mrModule mr) dArg (_mHash md))
    Just _ -> throwExecutionError info (DynNameIsNotModRef dArg)
    Nothing -> failInvariant info ("unbound identifier" <> T.pack (show n))
  _ -> failInvariant info ("invalid name in fq position" <> T.pack (show n))

guardTable :: (MonadEval b i m) => i -> CEKEnv b i m -> TableValue -> m ()
guardTable i env (TableValue _ mn mh _) = do
  guardForModuleCall i env mn $ do
    mdl <- getModule i env mn
    enforceBlessedHashes i mdl mh

enforceBlessedHashes :: (MonadEval b i m) => i -> EvalModule b i -> ModuleHash -> m ()
enforceBlessedHashes info md mh
  | _mHash md == mh = return ()
  | mh `S.member` (_mBlessed md) = return ()
  | otherwise = throwExecutionError info (HashNotBlessed (_mName md) mh)

guardForModuleCall :: (MonadEval b i m) => i -> CEKEnv b i m -> ModuleName -> m () -> m ()
guardForModuleCall i env currMod onFound =
  findCallingModule >>= \case
    Just mn | mn == currMod -> onFound
    _ -> getModule i env currMod >>= acquireModuleAdmin i env

acquireModuleAdmin :: (MonadEval b i m) => i -> CEKEnv b i m -> EvalModule b i -> m ()
acquireModuleAdmin i env mdl = do
  mc <- useEvalState (esCaps . csModuleAdmin)
  unless (S.member (_mName mdl) mc) $ case _mGovernance mdl of
    KeyGov ksn -> do
      signed <- enforceKeysetName i env ksn
      unless signed $ throwExecutionError i (ModuleGovernanceFailure (_mName mdl))
      esCaps . csModuleAdmin %%= S.insert (_mName mdl)
    CapGov (ResolvedGov fqn) -> do
      let wcapBody = Constant LUnit i
      -- *special* use of `evalCap` here to evaluate module governance.
      evalCap i Mt CEKNoHandler (set ceLocal mempty env) (CapToken fqn []) wcapBody >>= \case
        VError _ ->
          throwExecutionError i (ModuleGovernanceFailure (_mName mdl))
        _ -> do
          esCaps . csModuleAdmin %%= S.insert (_mName mdl)

evalWithStackFrame
  :: (MonadEval b i m)
  => i
  -> Cont b i m
  -> CEKErrorHandler b i m
  -> CEKEnv b i m
  -> StackFrame
  -> Maybe Type
  -> EvalTerm b i
  -> m (EvalResult b i m)
evalWithStackFrame info cont handler env sf mty body = do
  esStack %%= (sf :)
  evalCEK (StackPopC info mty cont) handler env body

-- | Evaluate a capability in `(with-capability)`
-- the resulting
-- Todo: case: cap already installed
-- (coin.TRANSFER "bob" "alice" someguard 10.0)
evalCap
  :: MonadEval b i m
  => i
  -> Cont b i m
  -> CEKErrorHandler b i m
  -> CEKEnv b i m
  -> FQCapToken
  -> EvalTerm b i
  -> m (EvalResult b i m)
evalCap info currCont handler env origToken@(CapToken fqn args) contbody = isCapInStack origToken >>= \case
  False -> do
    let qn = fqnToQualName fqn
    let ct = CapToken qn args
    enforceNotWithinDefcap info env "with-capability"
    lookupFqName fqn >>= \case
      Just (DCap d) -> do
        when (length args /= _dcapAppArity d) $ failInvariant info "Dcap argument length mismatch"
        (esCaps . csSlots) %%= (CapSlot ct []:)
        let env' = RAList.fromList $ fmap VPactValue (reverse args)
            capBody = _dcapTerm d
            cont' = CapBodyC env contbody currCont
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
                        installCap info env c' >>= evalUserManagedCap cont' env' capBody
                      Nothing ->
                        throwExecutionError info (CapNotInstalled fqn)
                  Just managedCap -> evalUserManagedCap cont' env' capBody managedCap
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
                        installCap info env c' >>= evalAutomanagedCap cont' env' capBody
                      Nothing ->
                        throwExecutionError info (CapNotInstalled fqn)
                  Just managedCap -> case _mcManaged managedCap of
                    AutoManaged b -> do
                      if b then
                        returnCEK cont' handler (VError "automanaged capability used more than once")
                      else do
                        let newManaged = AutoManaged True
                        esCaps . csManaged %%= S.union (S.singleton (set mcManaged newManaged managedCap))
                        evalWithStackFrame info cont' handler (set ceLocal env' env) capStackFrame Nothing capBody
                    _ -> failInvariant info "manager function mismatch"
          Just DefEvent ->
            failInvariant info "cannot evaluate the body of an event cap"
          Nothing -> do
            evalWithStackFrame info cont' handler (set ceLocal env' env) capStackFrame Nothing capBody
      Just {} ->
        failInvariant info "Captoken references invalid def"
      Nothing -> failInvariant info "No such def for evalCap"
  True ->
    evalCEK currCont handler env contbody
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
    AutoManaged b -> do
      if b then returnCEK currCont handler (VError "automanaged cap used once")
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

enforceNotWithinDefcap
  :: (MonadEval b i m)
  => i
  -> CEKEnv b i m
  -> T.Text
  -> m ()
enforceNotWithinDefcap info env form =
  when (_ceInCap env) $ throwExecutionError info (FormIllegalWithinDefcap form)

requireCap
  :: MonadEval b i m
  => Cont b i m
  -> CEKErrorHandler b i m
  -> FQCapToken
  -> m (EvalResult b i m)
requireCap cont handler ct@(CapToken fqn _) = do
  capInStack <- isCapInStack  ct
  if capInStack then returnCEKValue cont handler (VBool True)
  else returnCEK cont handler $ VError $ "cap not in scope " <> renderQualName (fqnToQualName fqn)

isCapInStack
  :: (MonadEval b i m)
  => FQCapToken
  -> m Bool
isCapInStack (CapToken fqn args) = do
  let ct = CapToken (fqnToQualName fqn) args
  caps <- useEvalState (esCaps.csSlots)
  let csToSet cs = S.insert (_csCap cs) (S.fromList (_csComposed cs))
      capSet = foldMap csToSet caps
  pure $ S.member ct capSet

composeCap
  :: (MonadEval b i m)
  => i
  -> Cont b i m
  -> CEKErrorHandler b i m
  -> CEKEnv b i m
  -> FQCapToken
  -> m (EvalResult b i m)
composeCap info cont handler env (CapToken fqn args) = do
  let ct = CapToken (fqnToQualName fqn) args
  lookupFqName fqn >>= \case
    Just (DCap d) -> do
      (esCaps . csSlots) %%= (CapSlot ct []:)
      args' <- zipWithM (\pv arg -> maybeTCType info pv (_argType arg)) args (_dcapArgs d)
      let env' = RAList.fromList $ fmap VPactValue (reverse args')
          capBody = _dcapTerm d
      let cont' = CapPopC PopCapComposed cont
      evalCEK cont' handler (set ceLocal env' env) capBody
    -- todo: this error loc is _not_ good. Need to propagate `i` here, maybe in the stack
    Just d ->
      throwExecutionError (defInfo d) $ InvalidDefKind (defKind d) "in compose-capability"
    Nothing ->
      -- Todo: error loc here
      throwExecutionError' (NoSuchDef fqn)

filterIndex :: Int -> [a] -> [a]
filterIndex i xs = [x | (x, i') <- zip xs [0..], i /= i']

findMsgSigCap :: Int -> CapToken QualifiedName PactValue -> CapToken QualifiedName PactValue -> Bool
findMsgSigCap cix ct1 ct2 =
  _ctName ct1 == _ctName ct2 && (_ctArgs ct1 == filterIndex cix (_ctArgs ct2))

-- Todo:
-- `capAutonomous` are what we should use to match semantics accurately.
installCap :: (MonadEval b i m)
  => i
  -> CEKEnv b i m
  -> FQCapToken
  -> m (ManagedCap QualifiedName PactValue)
installCap info env (CapToken fqn args) = do
  enforceNotWithinDefcap info env "install-capability"
  let ct = CapToken (fqnToQualName fqn) args
  lookupFqName fqn >>= \case
    Just (DCap d) -> case _dcapMeta d of
      Just (DefManaged m) -> case m of
        Just (DefManagedMeta paramIx (FQName fqnMgr)) -> do
          managedParam <- maybe (throwExecutionError info (InvalidManagedCap fqn)) pure (args ^? ix paramIx)
          let mcapType = ManagedParam fqnMgr managedParam paramIx
              ctFiltered = CapToken (fqnToQualName fqn) (filterIndex paramIx args)
              mcap = ManagedCap ctFiltered ct mcapType
          (esCaps . csManaged) %%= S.insert mcap
          (esCaps . csAutonomous) %%= S.insert ct
          pure mcap
        Nothing -> do
          let mcapType = AutoManaged False
              mcap = ManagedCap ct ct mcapType
          (esCaps . csManaged) %%= S.insert mcap
          (esCaps . csAutonomous) %%= S.insert ct
          pure mcap
      Just DefEvent ->
        throwExecutionError info (InvalidManagedCap fqn)
      Nothing -> throwExecutionError info (InvalidManagedCap fqn)
    Just d ->
      -- todo: error loc here is not in install-cap
      throwExecutionError (defInfo d) (InvalidDefKind (defKind d) "install-capability")
    Nothing -> throwExecutionError' (NoSuchDef fqn)

-- Todo: should we typecheck / arity check here?
createUserGuard
  :: (MonadEval b i m)
  => Cont b i m
  -> CEKErrorHandler b i m
  -> FullyQualifiedName
  -> [PactValue]
  -> m (EvalResult b i m)
createUserGuard cont handler fqn args =
  lookupFqName fqn >>= \case
    Just (Dfun _) ->
      returnCEKValue cont handler (VGuard (GUserGuard (UserGuard fqn args)))
    Just _ ->
      returnCEK cont handler (VError "create-user-guard pointing to non-guard")
    Nothing ->
      failInvariant def "User guard pointing to no defn"


emitEvent
  :: MonadEval b i m
  => Cont b i m
  -> CEKErrorHandler b i m
  -> FQCapToken
  -> m (EvalResult b i m)
emitEvent cont handler ct@(CapToken fqn _) = do
  let pactEvent = PactEvent ct (_fqModule fqn) (_fqHash fqn)
  esEvents %%= (pactEvent:)
  returnCEKValue cont handler VUnit


returnCEK
  :: (MonadEval b i m)
  => Cont b i m
  -> CEKErrorHandler b i m
  -> EvalResult b i m
  -> m (EvalResult b i m)
returnCEK Mt handler v =
  case handler of
    CEKNoHandler -> return v
    CEKHandler env catchTerm cont' caps handler' -> case v of
      VError{} -> do
        setEvalState (esCaps . csSlots) caps
        evalCEK cont' handler' env catchTerm
      EvalValue v' ->
        returnCEKValue cont' handler' v'
    -- Enforce one is tricky. Not only do false results
    -- mean "continue to evaluate the list of expressions",
    -- but it also HANDLES ERRORS and continues to chug away!!!!
    -- Therefore, it has a custom handler which holds:
    --  - The last eval env
    --  - The "lazy" string expression
    --  - The remaining conditions, in case a falsy error needs to be handled
    --  - The remainder of the continuation and the old handler
    --
    -- This handler upon encountering an error has a choice to make:
    --  - Do we have unhandled expressions left? If so, resume evaluation with the head of
    --    the expression list
    --  - Are we done evaluating expressions? Then we have an enforce error: compute the
    --    error string and boom boom de boom return an unhandled error with it
    --
    --  How is the list of expressions kept up to date you may ask?
    --  EnforceOne is the only native that actualy has to _modify the handler_
    --  on successful expression evaluation in the case that it errors
    CEKEnforceOne env i str li cont cs h -> case v of
      VError{} -> case li of
        [] -> do
          setEvalState (esCaps . csSlots) cs
          let cont' = EnforceErrorC cont
          evalCEK cont' h env str
        x:xs -> do
          setEvalState (esCaps . csSlots) cs
          let handler' = CEKEnforceOne env i str xs cont cs h
              oldFrame = CondC env i (EnforceOneFrame str xs) Mt
          evalCEK oldFrame handler' env x
      EvalValue v' ->
        returnCEKValue cont h v'
returnCEK cont handler v = case v of
  VError{} -> returnCEK Mt handler v
  EvalValue v' -> returnCEKValue cont handler v'

-- | if true then 1 else 2
returnCEKValue
  :: (MonadEval b i m)
  => Cont b i m
  -> CEKErrorHandler b i m
  -> CEKValue b i m
  -> m (EvalResult b i m)
returnCEKValue Mt handler v =
  case handler of
    CEKNoHandler -> return (EvalValue v)
    -- Assuming no error, the caps will have been popped naturally
    CEKHandler _env _term cont' _ handler' ->
      returnCEKValue cont' handler' v
    CEKEnforceOne _ _ _ _ cont' _ handler' ->
      returnCEKValue cont' handler' v
-- Error terms that don't simply returnt the empty continuation
-- "Zero out" the continuation up to the latest handler
-- returnCEKValue _cont handler v@VError{} =
--   returnCEK Mt handler v
returnCEKValue (Args env i args cont) handler fn = do
  c <- canApply fn
  -- Argument evaluation
  case args of
    [] -> applyLam c [] cont handler
    (x:xs) -> do
      let cont' = Fn c env xs [] cont
      evalCEK cont' handler env x
  where
  canApply = \case
    -- Todo: restrict the type of closures applied to user functions
    VClosure (C clo) -> pure (C clo)
    VClosure (LC clo) -> pure (LC clo)
    VClosure (N clo) -> pure (N clo)
    VClosure (DPC clo) -> pure (DPC clo)
    VClosure (CT clo) -> pure (CT clo)
    VClosure _ ->
      throwExecutionError i CannotApplyPartialClosure
    _ -> failInvariant i "Cannot apply non-function to arguments"
  -- evalCEK (Fn fn cont) handler env arg
returnCEKValue (Fn fn env args vs cont) handler v = do
  case args of
    [] -> do
      applyLam fn (reverse (v:vs)) cont handler
    x:xs ->
      evalCEK (Fn fn env xs (v:vs) cont) handler env x
returnCEKValue (LetC env letbody cont) handler v = do
  evalCEK cont handler (over ceLocal (RAList.cons v) env) letbody
returnCEKValue (SeqC env e cont) handler _ =
  evalCEK cont handler env e
returnCEKValue (CondC env info frame cont) handler v = case v of
  (VLiteral (LBool b)) -> case frame of
    AndFrame te ->
      if b then evalCEK cont handler env te
      else returnCEKValue cont handler v
    OrFrame te ->
      if b then returnCEKValue cont handler v
      else evalCEK cont handler env te
    IfFrame ifExpr elseExpr ->
      if b then evalCEK cont handler env ifExpr
      else evalCEK cont handler env elseExpr
    EnforceFrame str ->
      if b then returnCEKValue cont handler v
      else do
        let cont' = EnforceErrorC cont
        evalCEK cont' handler env str
    EnforceOneFrame str li ->
      if b then returnCEKValue cont handler v
      else case li of
        x:xs -> do
          let cont' = CondC env info (EnforceOneFrame str xs) cont
              handler' = updateEnforceOneList xs handler
          evalCEK cont' handler' env x
        [] -> do
          let cont' = EnforceErrorC cont
          evalCEK cont' handler env str
  _ ->
    -- Todo: thread error loc here
    failInvariant info "Evaluation of conditional expression yielded non-boolean value"
  where
  updateEnforceOneList xs (CEKEnforceOne e i str _ c cs h) =
    CEKEnforceOne e i str xs c cs h
  updateEnforceOneList _ e = e
returnCEKValue (CapInvokeC env info terms pvs cf cont) handler v = do
  pv <- enforcePactValue v
  case terms of
    x:xs -> do
      let cont' = CapInvokeC env info xs (pv:pvs) cf cont
      evalCEK cont' handler env x
    [] -> case cf of
      WithCapFrame fqn wcbody -> do
        guardForModuleCall info env (_fqModule fqn) $ return ()
        evalCap info cont handler env (CapToken fqn (reverse (pv:pvs))) wcbody
      CreateUserGuardFrame fqn ->
        createUserGuard cont handler fqn (reverse (pv:pvs))
returnCEKValue (CapBodyC env capbody cont) handler _ = do
  let cont' = CapPopC PopCapInvoke cont
  evalCEK cont' handler env capbody
returnCEKValue (CapPopC st cont) handler v = case st of
  PopCapInvoke -> do
    -- todo: need safe tail here, but this should be fine given the invariant that `CapPopC`
    -- will never show up otherwise
    esCaps . csSlots %%= tail
    returnCEKValue cont handler v
  PopCapComposed -> do
    caps <- useEvalState (esCaps . csSlots)
    let cs = head caps
        csList = _csCap cs : _csComposed cs
        caps' = over (_head . csComposed) (++ csList) (tail caps)
    setEvalState (esCaps . csSlots) caps'
    returnCEKValue cont handler VUnit
returnCEKValue (ListC env args vals cont) handler v = do
  pv <- enforcePactValue v
  case args of
    [] ->
      returnCEKValue cont handler (VList (V.fromList (reverse (pv:vals))))
    e:es ->
      evalCEK (ListC env es (pv:vals) cont) handler env e
returnCEKValue (ObjC env currfield fs vs cont) handler v = do
  v' <- enforcePactValue v
  let fields = (currfield,v'):vs
  case fs of
    (f', term):fs' ->
      let cont' = ObjC env f' fs' fields cont
      in evalCEK cont' handler env term
    [] ->
      returnCEKValue cont handler (VObject (M.fromList (reverse fields)))
returnCEKValue (EnforceErrorC _) handler v = case v of
  VString err -> returnCEK Mt handler (VError err)
  _ -> failInvariant def "enforce function did not return a string"
returnCEKValue (StackPopC i mty cont) handler v = do
  v' <- (\pv -> maybeTCType i pv mty) =<< enforcePactValue v
  -- Todo: unsafe use of tail here. need `tailMay`
  (esStack %%= tail) *> returnCEKValue cont handler (VPactValue v')
returnCEKValue (PactStepC env cont) handler v =
  useEvalState esPactExec >>= \case
    Nothing -> failInvariant def "No PactExec found"
    Just pe -> case env ^. cePactStep of
      Nothing -> error "invariant violation"
      Just ps -> do
        let
          pdb = view cePactDb env
          isLastStep = _psStep ps == pred (_peStepCount pe)
          done = (not (_psRollback ps) && isLastStep) || _psRollback ps

        liftIO (writePacts pdb Write (_psPactId ps)
                 (if done then Nothing else Just pe))

        -- We need to reset the `yield` value of the current pact exection
        -- environment to match pact semantics. This `PactStepC` frame is
        -- only used as a continuation which resets the `yield`.
        setEvalState (esPactExec . _Just . peYield) Nothing
--        evalCEK cont handler env v
        liftIO $ print v
        returnCEKValue cont handler v


applyLam
  :: (MonadEval b i m)
  => CanApply b i m
  -> [CEKValue b i m]
  -> Cont b i m
  -> CEKErrorHandler b i m
  -> m (EvalResult b i m)
applyLam (C (Closure fn mn ca arity term mty env cloi)) args cont handler
  | arity == argLen = case ca of
    ArgClosure cloargs -> do
      args' <- traverse enforcePactValue args
      tcArgs <- zipWithM (\arg ty -> VPactValue <$> maybeTCType cloi arg ty) args' (NE.toList cloargs)
      esStack %%= (StackFrame fn mn SFDefun :)
      let cont' = StackPopC cloi mty cont
          varEnv = RAList.fromList (reverse tcArgs)
      evalCEK cont' handler (set ceLocal varEnv env) term
    NullaryClosure -> do
      esStack %%= (StackFrame fn mn SFDefun :)
      let cont' = StackPopC cloi mty cont
          varEnv = mempty
      evalCEK cont' handler (set ceLocal varEnv env) term
  | argLen > arity = throwExecutionError cloi ClosureAppliedToTooManyArgs
  | otherwise = case ca of
    NullaryClosure -> throwExecutionError cloi ClosureAppliedToTooManyArgs
    ArgClosure cloargs ->
      apply' mempty (NE.toList cloargs) args
  where
  argLen = length args
  -- Here we enforce an argument to a user fn is a
  apply' e (ty:tys) (x:xs) = do
    x' <- (\pv -> maybeTCType cloi pv ty) =<< enforcePactValue x
    apply' (RAList.cons (VPactValue x') e) tys xs
  apply' e (ty:tys) [] = do
    let env' = set ceLocal e env
        pclo = PartialClosure (Just (StackFrame fn mn SFDefun)) (ty :| tys) (length tys + 1) term mty env' cloi
    returnCEKValue cont handler (VPartialClosure pclo)
  apply' _ [] _ = throwExecutionError cloi ClosureAppliedToTooManyArgs

applyLam (LC (LamClosure ca arity term mty env cloi)) args cont handler
  | arity == argLen = case ca of
    ArgClosure _ -> do
      let locals = view ceLocal env
          locals' = foldl' (flip RAList.cons) locals args
      evalCEK cont handler (set ceLocal locals' env) term
    NullaryClosure -> do
      evalCEK cont handler env term
  | argLen > arity = throwExecutionError cloi ClosureAppliedToTooManyArgs
  | otherwise = case ca of
      NullaryClosure -> throwExecutionError cloi ClosureAppliedToTooManyArgs
      ArgClosure cloargs ->
        apply' (view ceLocal env) (NE.toList cloargs) args
  where
  argLen = length args
  -- Todo: runtime TC here
  apply' e (ty:tys) (x:xs) = do
    x' <- (\pv -> maybeTCType cloi pv ty) =<< enforcePactValue x
    apply' (RAList.cons (VPactValue x') e) tys xs
  apply' e [] [] = do
    evalCEK cont handler (set ceLocal e env) term
  apply' e (ty:tys) [] =
    returnCEKValue cont handler
    (VPartialClosure (PartialClosure Nothing (ty :| tys) (length tys + 1) term mty (set ceLocal e env) cloi))
  apply' _ [] _ = throwExecutionError cloi ClosureAppliedToTooManyArgs

applyLam (PC (PartialClosure li argtys _ term mty env i)) args cont handler =
  apply' (view ceLocal env) (NE.toList argtys) args
  where
  apply' e (ty:tys) (x:xs) = do
    x' <- (\pv -> maybeTCType i pv ty) =<< enforcePactValue x
    apply' (RAList.cons (VPactValue x') e) tys xs
  apply' e [] [] = do
    case li of
      Just sf -> do
        let cont' = StackPopC i mty cont
        esStack %%= (sf :)
        evalCEK cont' handler (set ceLocal e env) term
      Nothing -> evalCEK cont handler (set ceLocal e env) term
  apply' e (ty:tys) [] = do
    let pclo = PartialClosure li (ty :| tys) (length tys + 1) term mty (set ceLocal e env) i
    returnCEKValue cont handler (VPartialClosure pclo)
  apply' _ [] _ = throwExecutionError i ClosureAppliedToTooManyArgs

applyLam nclo@(N (NativeFn b env fn arity i)) args cont handler
  | arity == argLen = fn i b cont handler env args
  | argLen > arity = throwExecutionError i ClosureAppliedToTooManyArgs
  | null args = returnCEKValue cont handler (VClosure nclo)
  | otherwise = apply' arity [] args
  where
  argLen = length args
  apply' !a pa (x:xs) = apply' (a - 1) (x:pa) xs
  apply' !a pa [] =
    returnCEKValue cont handler (VPartialNative (PartialNativeFn b env fn a pa i))

applyLam (PN (PartialNativeFn b env fn arity pArgs i)) args cont handler
  | arity == argLen = fn i b cont handler env (reverse pArgs ++ args)
  | argLen > arity = throwExecutionError i ClosureAppliedToTooManyArgs
  | otherwise = apply' arity [] args
  where
  argLen = length args
  apply' !a pa (x:xs) = apply' (a - 1) (x:pa) xs
  apply' !a pa [] =
    returnCEKValue cont handler (VPartialNative (PartialNativeFn b env fn a pa i))

applyLam (DPC (DefPactClosure fqn argtys arity env i)) args cont handler
  | arity == argLen = case argtys of
    ArgClosure cloargs -> do
      args' <- traverse enforcePactValue args
      tcArgs <- zipWithM (\arg ty -> maybeTCType i arg ty) args' (NE.toList cloargs)
      let pc = PactContinuation fqn tcArgs
      let env' = set ceLocal (RAList.fromList (reverse (VPactValue <$> tcArgs))) env
      initPact i pc cont handler env'
    NullaryClosure -> do
      let pc = PactContinuation fqn []
      let env' = set ceLocal mempty env
      initPact i pc cont handler env'
  | otherwise = throwExecutionError i ClosureAppliedToTooManyArgs
  where
  argLen = length args
applyLam (CT (CapTokenClosure fqn argtys arity i)) args cont handler
  | arity == argLen = do
    args' <- traverse enforcePactValue args
    tcArgs <- zipWithM (\arg ty -> maybeTCType i arg ty) args' argtys
    returnCEKValue cont handler (VPactValue (PCapToken (CapToken fqn tcArgs)))
  | otherwise = throwExecutionError i ClosureAppliedToTooManyArgs
  where
  argLen = length args


-- | Apply one argument to a value
unsafeApplyOne
  :: MonadEval b i m
  => CanApply b i m
  -> CEKValue b i m
  -> m (EvalResult b i m)
unsafeApplyOne c arg =
  applyLam c [arg] Mt CEKNoHandler

unsafeApplyTwo
  :: MonadEval b i m
  => CanApply b i m
  -> CEKValue b i m
  -> CEKValue b i m
  -> m (EvalResult b i m)
unsafeApplyTwo c arg1 arg2 = applyLam c [arg1, arg2] Mt CEKNoHandler
