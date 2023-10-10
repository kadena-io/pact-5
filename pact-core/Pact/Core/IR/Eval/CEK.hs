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
  , evalCap
  , nameToFQN
  , guardTable
  , enforceKeyset
  , enforceKeysetName) where

--import Debug.Trace

import Control.Lens hiding ((%%=))
import Control.Monad(zipWithM, unless, when)
import Control.Monad.IO.Class
import Control.Monad.Except
import Data.Default
import Data.Text(Text)
import Data.List.NonEmpty(NonEmpty(..))
import Data.Foldable(find, foldl')
import qualified Data.RAList as RAList
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust)

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
import Pact.Core.Pacts.Types (PactStep(..), PactExec(..), PactId(..), PactContinuation(..), peYield
                             , peStepCount, psStep, psRollback)


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
      Nothing -> failInvariant' ("unbound identifier" <> T.pack (show n)) info
    -- Top level names are not closures, so we wipe the env
    NTopLevel mname mh -> do
      let fqn = FullyQualifiedName mname (_nName n) mh
      lookupFqName fqn >>= \case
        Just (Dfun d) -> do
          dfunClo <- mkDefunClosure d env
          returnCEKValue cont handler dfunClo
        Just (DConst d) ->
          evalCEK cont handler (set ceLocal mempty env) (_dcTerm d)
        Just (DPact d) -> do
          dpactClo <- mkDefPactClosure fqn d env
          returnCEKValue cont handler dpactClo
        Just (DTable d) ->
          let (ResolvedTable sc) = _dtSchema d
              tbl = VTable (TableValue (TableName (_dtName d)) mname mh sc)
          in returnCEKValue cont handler tbl
        Just d ->
          throwExecutionError info (InvalidDefKind (defKind d) "in var position")
        Nothing ->
          throwExecutionError info (NameNotInScope (FullyQualifiedName mname (_nName n) mh))
    NModRef m ifs -> case ifs of
      [x] -> returnCEKValue cont handler (VModRef (ModRef m ifs (Just x)))
      [] -> throwExecutionError info (ModRefNotRefined (_nName n))
      _ -> returnCEKValue cont handler (VModRef (ModRef m ifs Nothing))
evalCEK cont handler _env (Constant l _) = do
  chargeNodeGas ConstantNode
  returnCEKValue cont handler (VLiteral l)
evalCEK cont handler env (App fn args _) = do
  chargeNodeGas AppNode
  evalCEK (Args env args cont) handler env fn
evalCEK cont handler env (Let arg e1 e2 i) =
  let lam = Lam AnonLamInfo (pure arg) e2 i
  in evalCEK cont handler env (App lam (pure e1) i)
evalCEK cont handler env (Lam li args body info) = do
  chargeNodeGas LamNode
  let clo = VLamClosure (LamClosure li (_argType <$> args) (NE.length args) body Nothing env info)
  returnCEKValue cont handler clo
evalCEK cont handler env (Builtin b i) = do
  chargeNodeGas BuiltinNode
  let builtins = view ceBuiltins env
  returnCEKValue cont handler (VNative (builtins i b env))
evalCEK cont handler env (Sequence e1 e2 _) = do
  chargeNodeGas SeqNode
  evalCEK (SeqC env e2 cont) handler env e1
evalCEK cont handler env (Conditional c _) = case c of
  CAnd te te' ->
    evalCEK (CondC env (AndFrame te') cont) handler env te
  COr te te' ->
    evalCEK (CondC env (OrFrame te') cont) handler env te
  CIf cond e1 e2 ->
    evalCEK (CondC env (IfFrame e1 e2) cont) handler env cond
evalCEK cont handler env (CapabilityForm cf _) = do
  fqn <- nameToFQN (view capFormName cf)
  case cf of
    -- Todo: duplication here in the x:xs case
    WithCapability _ args body -> case args of
      x:xs -> let
        capFrame = WithCapFrame fqn body
        cont' = CapInvokeC env xs [] capFrame cont
        in evalCEK cont' handler env x
      [] -> evalCap cont handler env (CapToken fqn []) body
    RequireCapability _ args -> case args of
      [] ->
        requireCap cont handler (CapToken fqn [])
      x:xs -> let
        capFrame = RequireCapFrame fqn
        cont' = CapInvokeC env xs [] capFrame cont
        in evalCEK cont' handler env x
    ComposeCapability _ args -> case args of
      [] -> composeCap cont handler env (CapToken fqn [])
      x:xs -> let
        capFrame = ComposeCapFrame fqn
        cont' = CapInvokeC env xs [] capFrame cont
        in evalCEK cont' handler env x
    InstallCapability _ args -> case args of
      [] -> installCap cont handler (CapToken fqn [])
      x : xs -> let
        capFrame = InstallCapFrame fqn
        cont' = CapInvokeC env xs [] capFrame cont
        in evalCEK cont' handler env x
    EmitEvent _ args -> case args of
      [] -> emitEvent cont handler (CapToken fqn [])
      x : xs -> let
        capFrame = EmitEventFrame fqn
        cont' = CapInvokeC env xs [] capFrame cont
        in evalCEK cont' handler env x
    CreateUserGuard _ args -> case args of
      [] -> createUserGuard cont handler fqn []
      x : xs -> let
        capFrame = CreateUserGuardFrame fqn
        cont' = CapInvokeC env xs [] capFrame cont
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
evalCEK cont handler env (DynInvoke n fn _) =
  evalCEK (DynInvokeC env fn cont) handler env n
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
  -> CEKEnv b i m
  -> m (CEKValue b i m)
mkDefunClosure d e = case _dfunTerm d of
  Lam li args body i ->
    pure (VDefClosure (Closure li (_argType <$> args) (NE.length args) body (_dfunRType d) e i))
  _ ->
    throwExecutionError (_dfunInfo d) (DefIsNotClosure (_dfunName d))

mkDefPactClosure
  :: (MonadEval b i m)
  => FullyQualifiedName
  -> DefPact Name Type b i
  -> CEKEnv b i m
  -> m (CEKValue b i m)
mkDefPactClosure fqn (DefPact _ _ mrty (step :| steps) info) env = case step of
  Step step' _ -> toClosure False step'
  StepWithRollback step' _ _ -> toClosure True step'
  where
    nSteps = length steps + 1
    toClosure rb = \case
      Lam li args body i ->
        let dpc = DefPactClosure fqn li (_argType <$> args) (NE.length args) body rb nSteps mrty env i
        in pure (VDefPactClosure dpc)
      _ -> throwExecutionError info (InvariantFailure "Step is not lambda")


initPact
  :: MonadEval b i m
  => i
  -> PactExec
  -> Cont b i m
  -> CEKErrorHandler b i m
  -> CEKEnv b i m
  -> EvalTerm b i
  -> m (EvalResult b i m)
initPact i pe cont handler cenv term = do
  case view cePactStep cenv of
    Nothing ->
      let
        pId = PactId ""-- (view eeHash env)
        pStep = PactStep 0 False pId Nothing
        cenv' = set cePactStep (Just pStep) cenv
      in applyPact i pe pStep cont handler cenv' term
    Just _ -> pure (VError "not implemented")

applyPact
  :: MonadEval b i m
  => i
  -> PactExec
  -> PactStep
  -> Cont b i m
  -> CEKErrorHandler b i m
  -> CEKEnv b i m
  -> EvalTerm b i
  -> m (EvalResult b i m)
applyPact i pe ps cont handler cenv term = do
  mCurrExec <- useEvalState esPactExec
  when (isJust mCurrExec) $
    throwExecutionError i MultipleOrNestedPactExecFound

  -- TODO: Does this makes sense, `applyPact` should always start executing step 0

  -- If the PactStep we want to execute is outside of
  -- 0 <= peStepCount < ps ^. peStepCount
  -- we fail with `PactStepNotFound`
  when (ps ^. psStep < pe ^. peStepCount) $
    throwExecutionError i (PactStepNotFound (ps ^. psStep))

  -- TODO: another invariant should be that steps progress monoton linearly

  -- store the initial PactExec
  setEvalState esPactExec (Just pe)

  evalCEK cont handler cenv term

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
  -- elide pk
  --   | T.length pk < 8 = pk
  --   | otherwise = T.take 8 pk <> "..."
  count = S.size kskeys
  -- failed = "Keyset failure"
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
nameToFQN :: Applicative f => Name -> f FullyQualifiedName
nameToFQN (Name n nk) = case nk of
  NTopLevel mn mh -> pure (FullyQualifiedName mn n mh)
  NBound{} -> error "expected fully resolve FQ name"
  NModRef{} -> error "expected non-modref"

guardTable :: (MonadEval b i m) => i -> CEKEnv b i m -> TableValue -> m ()
guardTable i env (TableValue _ mn mh _) = do
  guardForModuleCall i env mn $ do
    mdl <- getModule mn env
    enforceBlessedHashes mdl mh

enforceBlessedHashes :: (MonadEval b i m) => EvalModule b i -> ModuleHash -> m ()
enforceBlessedHashes md mh
  | _mHash md == mh = return ()
  | mh `S.member` (_mBlessed md) = return ()
  | otherwise = error "Execution aborted: hash not blessed"

guardForModuleCall :: (MonadEval b i m) => i -> CEKEnv b i m -> ModuleName -> m () -> m ()
guardForModuleCall i env currMod onFound =
  findCallingModule >>= \case
    Just mn | mn == currMod -> onFound
    _ -> getModule currMod env >>= acquireModuleAdmin i env

acquireModuleAdmin :: (MonadEval b i m) => i -> CEKEnv b i m -> EvalModule b i -> m ()
acquireModuleAdmin i env mdl = case _mGovernance mdl of
  KeyGov ksn -> do
    signed <- enforceKeysetName i env ksn
    unless signed $ error "module admin key enforce failure"
  CapGov n -> do
    -- Todo: GADT fixes this.
    fqn <- nameToFQN n
    let wcapBody = Constant LUnit i
    -- *special* use of `evalCap` here to evaluate module governance.
    evalCap Mt CEKNoHandler env (CapToken fqn [])  wcapBody >>= \case
      VError e -> error (T.unpack e)
      _ -> pure ()

-- | Evaluate a capability in `(with-capability)`
-- the resulting
evalCap
  :: MonadEval b i m
  => Cont b i m
  -> CEKErrorHandler b i m
  -> CEKEnv b i m
  -> FQCapToken
  -> EvalTerm b i
  -> m (EvalResult b i m)
evalCap cont handler env ct@(CapToken fqn args) contbody = do
  lookupFqName fqn >>= \case
    Just (DCap d) -> do
      (esCaps . csSlots) %%= (CapSlot ct []:)
      let (env', capBody) = applyCapBody args (_dcapTerm d)
          cont' = CapBodyC env contbody cont
      -- Todo: horrible holy crap
      case _dcapMeta d of
        -- Managed capability, so we should look for it in the set of csmanaged
        Just (DefManaged mdm) -> do
          caps <- useEvalState (esCaps . csManaged)
          case mdm of
            -- | Not automanaged, so it must have a defmeta
            Just (DefManagedMeta cix _) -> do
              let cap = CapToken fqn (filterIndex cix args)
              -- Find the capability post-filtering
              case find ((==) cap . _mcCap) caps of
                Nothing ->
                  throwExecutionError (_dcapInfo d) (CapNotInstalled fqn)
                Just managedCap -> case _mcManaged managedCap of
                  ManagedParam mpfqn pv managedIx -> do
                    lookupFqName mpfqn >>= \case
                      Just (Dfun dfun) -> do
                        mparam <- maybe (error "fatal: no param") pure (args ^? ix managedIx)
                        result <- evaluate (_dfunTerm dfun) pv mparam
                        let mcM = ManagedParam mpfqn result managedIx
                        esCaps . csManaged %%= S.union (S.singleton (set mcManaged mcM managedCap))
                        evalCEK cont' handler (set ceLocal env' env) capBody
                      _ -> error "not a defun"
                  _ -> error "incorrect cap type"
            Nothing -> do
              -- Find the capability post-filtering
              case find ((==) ct . _mcCap) caps of
                Nothing -> error "cap not installed"
                Just managedCap -> case _mcManaged managedCap of
                  AutoManaged b -> do
                    if b then error "automanaged cap already used once"
                    else do
                      let newManaged = AutoManaged True
                      esCaps . csManaged %%= S.union (S.singleton (set mcManaged newManaged managedCap))
                      evalCEK cont' handler (set ceLocal env' env) capBody
                  _ -> error "incorrect cap type"
        Just DefEvent -> error "defEvent"
        Nothing -> evalCEK cont' handler (set ceLocal env' env) capBody
    Just {} -> error "was not defcap, invariant violated"
    Nothing -> error "No such def"
  where
  evaluate term managed value = case term of
    Lam li lamargs body i -> do
      -- Todo: `applyLam` here gives suboptimal errors
      -- Todo: this completely violates our "step" semantics.
      -- This should be its own frame
      let clo = Closure li (_argType <$> lamargs) (NE.length lamargs) body Nothing env i
      res <- applyLam (C clo) [VPactValue managed, VPactValue value] Mt CEKNoHandler
      case res of
        EvalValue out -> enforcePactValue out
        _ -> error "did not return a value"
    _t -> failInvariant "mgr function was not a lambda"
  -- Todo: typecheck arg here
  -- Todo: definitely a bug if a cap has a lambda as a body
  applyCapBody bArgs (Lam _ _lamArgs body _) = (RAList.fromList (fmap VPactValue (reverse bArgs)), body)
  applyCapBody [] b = (mempty, b)
  applyCapBody _ _ = error "invariant broken: cap does not take arguments but is a lambda"


requireCap
  :: MonadEval b i m
  => Cont b i m
  -> CEKErrorHandler b i m
  -> FQCapToken
  -> m (EvalResult b i m)
requireCap cont handler ct = do
  caps <- useEvalState (esCaps.csSlots)
  let csToSet cs = S.insert (_csCap cs) (S.fromList (_csComposed cs))
      capSet = foldMap csToSet caps
  if S.member ct capSet then returnCEKValue cont handler VUnit
  else throwExecutionError' (CapNotInScope "cap not in scope")

composeCap
  :: (MonadEval b i m)
  => Cont b i m
  -> CEKErrorHandler b i m
  -> CEKEnv b i m
  -> FQCapToken
  -> m (EvalResult b i m)
composeCap cont handler env ct@(CapToken fqn args) = do
  lookupFqName fqn >>= \case
    Just (DCap d) -> do
      (esCaps . csSlots) %%= (CapSlot ct []:)
      (env', capBody) <- applyCapBody (_dcapTerm d)
      let cont' = CapPopC PopCapComposed cont
      evalCEK cont' handler (set ceLocal env' env) capBody
    -- todo: this error loc is _not_ good. Need to propagate `i` here, maybe in the stack
    Just d ->
      throwExecutionError (defInfo d) $ InvalidDefKind (defKind d) "in compose-capability"
    Nothing ->
      -- Todo: error loc here
      throwExecutionError' (NoSuchDef fqn)
  where
  -- Todo: typecheck arg here
  -- Todo: definitely a bug if a cap has a lambda as a body
  applyCapBody (Lam _ lamArgs body _) = do
    args' <- zipWithM (\pv arg -> maybeTCType pv (_argType arg)) args (NE.toList lamArgs)
    pure (RAList.fromList (fmap VPactValue (reverse args')), body)
  applyCapBody b = pure (mempty, b)

filterIndex :: Int -> [a] -> [a]
filterIndex i xs = [x | (x, i') <- zip xs [0..], i /= i']

-- Todo:
-- `capAutonomous` are what we should use to match semantics accurately.
installCap :: (MonadEval b i m)
  => Cont b i m
  -> CEKErrorHandler b i m
  -> FQCapToken
  -> m (EvalResult b i m)
installCap cont handler ct@(CapToken fqn args) = do
  lookupFqName fqn >>= \case
    Just (DCap d) -> case _dcapMeta d of
      Just (DefManaged m) -> case m of
        Just (DefManagedMeta paramIx mgrfn) -> do
          fqnMgr <- nameToFQN mgrfn
          managedParam <- maybe (throwExecutionError (_dcapInfo d) (InvalidManagedCap fqn)) pure (args ^? ix paramIx)
          let mcapType = ManagedParam fqnMgr managedParam paramIx
              ctFiltered = CapToken fqn (filterIndex paramIx args)
              mcap = ManagedCap ctFiltered ct mcapType
          (esCaps . csManaged) %%= S.insert mcap
          returnCEKValue cont handler VUnit
        Nothing -> do
          let mcapType = AutoManaged False
              mcap = ManagedCap ct ct mcapType
          (esCaps . csManaged) %%= S.insert mcap
          returnCEKValue cont handler VUnit
      Just DefEvent ->
        throwExecutionError (_dcapInfo d) (InvalidManagedCap fqn)
      Nothing -> throwExecutionError (_dcapInfo d) (InvalidManagedCap fqn)
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
    Just _ -> error "user guard not a defun"
    Nothing -> error "boom"


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


returnCEK :: (MonadEval b i m)
  => Cont b i m
  -> CEKErrorHandler b i m
  -> EvalResult b i m
  -> m (EvalResult b i m)
returnCEK Mt handler v =
  case handler of
    CEKNoHandler -> return v
    CEKHandler env term cont' caps handler' -> case v of
      VError{} -> do
        setEvalState (esCaps . csSlots) caps
        evalCEK cont' handler' env term
      EvalValue v' ->
        returnCEKValue cont' handler' v'
returnCEK cont handler v = case v of
  VError{} -> returnCEK Mt handler v
  EvalValue v' -> returnCEKValue cont handler v'

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
    CEKHandler _env _term cont' _ handler' -> returnCEKValue cont' handler' v
-- Error terms that don't simply returnt the empty continuation
-- "Zero out" the continuation up to the latest handler
-- returnCEKValue _cont handler v@VError{} =
--   returnCEK Mt handler v
returnCEKValue (Args env (x :| xs) cont) handler fn = do
  c <- canApply fn
  let cont' = Fn c env xs [] cont
  evalCEK cont' handler env x
  where
  canApply = \case
    -- Todo: restrict the type of closures applied to user functions
    VClosure (C clo) -> pure (C clo)
    VClosure (LC clo) -> pure (LC clo)
    VClosure (N clo) -> pure (N clo)
    VClosure (DPC clo) -> pure (DPC clo)
    _ -> error "Cannot apply partial closure"
  -- evalCEK (Fn fn cont) handler env arg
returnCEKValue (Fn fn env args vs cont) handler v = do
  case args of
    [] -> do
      applyLam fn (reverse (v:vs)) cont handler
    x:xs ->
      evalCEK (Fn fn env xs (v:vs) cont) handler env x
returnCEKValue (SeqC env e cont) handler _ =
  evalCEK cont handler env e
returnCEKValue (CondC env frame cont) handler v = case v of
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
  _ ->
    -- Todo: thread error loc here
    failInvariant "Evaluation of conditional expression yielded non-boolean value"
returnCEKValue (CapInvokeC env terms pvs cf cont) handler v = do
  pv <- enforcePactValue v
  case terms of
    x:xs -> do
      let cont' = CapInvokeC env xs (pv:pvs) cf cont
      evalCEK cont' handler env x
    [] -> case cf of
      WithCapFrame fqn wcbody ->
        evalCap cont handler env (CapToken fqn (reverse (pv:pvs))) wcbody
      RequireCapFrame fqn  ->
        requireCap cont handler (CapToken fqn (reverse (pv:pvs)))
      ComposeCapFrame fqn ->
        composeCap cont handler env (CapToken fqn (reverse (pv:pvs)))
      InstallCapFrame fqn ->
        installCap cont handler (CapToken fqn (reverse (pv:pvs)))
      EmitEventFrame fqn ->
        emitEvent cont handler (CapToken fqn (reverse (pv:pvs)))
      CreateUserGuardFrame fqn ->
        createUserGuard cont handler fqn (reverse (pv:pvs))
returnCEKValue (CapBodyC env term cont) handler _ = do
  let cont' = CapPopC PopCapInvoke cont
  evalCEK cont' handler env term
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
-- Todo: note over here we might want to typecheck
-- Todo: inline the variable lookup instead of calling EvalCEK directly,
-- as we can provide a better error message this way.
returnCEKValue (DynInvokeC _env _fn _cont) _handler v = case v of
  VModRef _mn -> error "todo: stubbed while refactoring env"
    -- Todo: for when persistence is implemented
    -- here is where we would incur module loading
    -- readEnv >>= \e -> case view (eeMHashes . at (_mrModule mn)) e of
    --   Just mh ->
    --     evalCEK cont handler env (Var (Name fn (NTopLevel (_mrModule mn) mh)) def)
      -- Nothing -> failInvariant "No such module"
  _ -> failInvariant "Not a modref"
returnCEKValue (StackPopC mty cont) handler v = do
  v' <- (`maybeTCType` mty) =<< enforcePactValue v
  -- Todo: unsafe use of tail here. need `tailMay`
  (esStack %%= tail) *> returnCEKValue cont handler (VPactValue v')
returnCEKValue (PactStepC cont env) handler v =
  useEvalState esPactExec >>= \case
    Nothing -> error "invariant violation, pact exec missing"
    Just pe -> case env ^. cePactStep of
      Nothing -> error "invariant violation"
      Just ps -> do
        let isLastStep = ps ^. psStep == pred (pe ^. peStepCount)
            done = isLastStep || ps ^. psRollback

        -- If we want to rollback
        
        -- We need to reset the `yield` value of the current pact exection
        -- environment to match pact semantics. This `PactStepC` frame is
        -- only used as a continuation which resets the `yield`.
        setEvalState (esPactExec . _Just . peYield) Nothing
        returnCEKValue cont handler v



applyLam
  :: (MonadEval b i m)
  => CanApply b i m
  -> [CEKValue b i m]
  -> Cont b i m
  -> CEKErrorHandler b i m
  -> m (EvalResult b i m)
applyLam (C (Closure li cloargs arity term mty env cloi)) args cont handler
  | arity == argLen = do
    args' <- traverse enforcePactValue args
    tcArgs <- zipWithM (\arg ty -> VPactValue <$> maybeTCType arg ty) args' (NE.toList cloargs)
    esStack %%= (StackFrame li :)
    let cont' = StackPopC mty cont
        varEnv = RAList.fromList (reverse tcArgs)
    evalCEK cont' handler (set ceLocal varEnv env) term
  | argLen > arity = error "Closure applied to too many arguments"
  | otherwise = apply' mempty (NE.toList cloargs) args
  where
  argLen = length args
  -- Here we enforce an argument to a user fn is a
  apply' e (ty:tys) (x:xs) = do
    x' <- (`maybeTCType` ty) =<< enforcePactValue x
    apply' (RAList.cons (VPactValue x') e) tys xs
  apply' e [] [] = do
    esStack %%= (StackFrame li :)
    evalCEK cont handler (set ceLocal e env) term
  apply' e (ty:tys) [] = do
    let env' = set ceLocal e env
        pclo = PartialClosure li (ty :| tys) (length tys + 1) term mty env' cloi
    returnCEKValue cont handler (VPartialClosure pclo)
  apply' _ [] _ = error "Applying too many arguments to function"

applyLam (LC (LamClosure li cloargs arity term mty env cloi)) args cont handler
  | arity == argLen = do
    esStack %%= (StackFrame li :)
    let locals = view ceLocal env
    let cont' = StackPopC mty cont
        locals' = foldl' (flip RAList.cons) locals args
    evalCEK cont' handler (set ceLocal locals' env) term
  | argLen > arity = error "Closure applied to too many arguments"
  | otherwise = apply' (view ceLocal env) (NE.toList cloargs) args
  where
  argLen = length args
  -- Todo: runtime TC here
  apply' e (ty:tys) (x:xs) = do
    x' <- (`maybeTCType` ty) =<< enforcePactValue x
    apply' (RAList.cons (VPactValue x') e) tys xs
  apply' e [] [] = do
    esStack %%= (StackFrame li :)
    evalCEK cont handler (set ceLocal e env) term
  apply' e (ty:tys) [] =
    returnCEKValue cont handler
    (VPartialClosure (PartialClosure li (ty :| tys) (length tys + 1) term mty (set ceLocal e env) cloi))
  apply' _ [] _ = error "Applying too many arguments to function"

applyLam (PC (PartialClosure li argtys _ term mty env i)) args cont handler =
  apply' (view ceLocal env) (NE.toList argtys) args
  where
  apply' e (ty:tys) (x:xs) = do
    x' <- (`maybeTCType` ty) =<< enforcePactValue x
    apply' (RAList.cons (VPactValue x') e) tys xs
  apply' e [] [] = do
    let cont' = StackPopC mty cont
    esStack %%= (StackFrame li :)
    evalCEK cont' handler (set ceLocal e env) term
  apply' e (ty:tys) [] = do
    let pclo = PartialClosure li (ty :| tys) (length tys + 1) term mty (set ceLocal e env) i
    returnCEKValue cont handler (VPartialClosure pclo)
  apply' _ [] _ = error "Applying too many arguments to partial function"

applyLam (N (NativeFn b env fn arity i)) args cont handler
  | arity == argLen = fn i b cont handler env args
  | argLen > arity = error "Applying too many args to native"
  | otherwise = apply' arity [] args
  where
  argLen = length args
  apply' !a pa (x:xs) = apply' (a - 1) (x:pa) xs
  apply' !a pa [] =
    returnCEKValue cont handler (VPartialNative (PartialNativeFn b env fn a pa i))

applyLam (PN (PartialNativeFn b env fn arity pArgs i)) args cont handler
  | arity == argLen = fn i b cont handler env (reverse pArgs ++ args)
  | argLen > arity = error "Applying too many args to native partial"
  | otherwise = apply' arity [] args
  where
  argLen = length args
  apply' !a pa (x:xs) = apply' (a - 1) (x:pa) xs
  apply' !a pa [] =
    returnCEKValue cont handler (VPartialNative (PartialNativeFn b env fn a pa i))

applyLam (DPC (DefPactClosure fqn li cloargs arity term rb sc mty env i)) args cont handler
  | arity == argLen = do

    args' <- traverse enforcePactValue args
    tcArgs <- zipWithM (\arg ty -> VPactValue <$> maybeTCType arg ty) args' (NE.toList cloargs)

    let
      pe = PactExec
           { _peStepCount = sc
           , _peYield = Nothing
           , _peStep = 0
           , _peContinuation = PactContinuation fqn args'
           , _peStepHasRollback = rb
--           , _peNestedPactExec = mempty
           }
    esStack %%= (StackFrame li :)

    let env' = set ceLocal (RAList.fromList (reverse tcArgs)) env
        cont' = PactStepC (StackPopC mty cont) env'
    initPact i pe cont' handler env' term

  | argLen > arity = error "Closure applied to too many arguments"
  | otherwise = apply' mempty (NE.toList cloargs) args
  where
  argLen = length args
  -- Here we enforce an argument to a user fn is a
  apply' e (ty:tys) (x:xs) = do
    x' <- (`maybeTCType` ty) =<< enforcePactValue x
    apply' (RAList.cons (VPactValue x') e) tys xs
  apply' e [] [] = do
    esStack %%= (StackFrame li :)
    evalCEK cont handler (set ceLocal e env) term
  apply' e (ty:tys) [] =
    returnCEKValue cont handler (VPartialClosure (PartialClosure li (ty :| tys) (length tys + 1) term mty (set ceLocal e env) i))
  apply' _ [] _ = error "Applying too many arguments to function"

failInvariant :: MonadEval b i m => Text -> m a
failInvariant b =
  let e = PEExecutionError (InvariantFailure b) def
  in throwError e

failInvariant' :: MonadEval b i m => Text -> i -> m a
failInvariant' b i =
  let e = PEExecutionError (InvariantFailure b) i
  in throwError e

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
