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
  , acquireModuleAdmin) where

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

import Pact.Core.IR.Term
import Pact.Core.IR.Eval.Runtime


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
evalCEK cont handler env (Let _ e1 e2 _) =
  let cont' = LetC env e2 cont
  in evalCEK cont' handler env e1
evalCEK cont handler env (Lam _ args body info) = do
  chargeNodeGas LamNode
  let clo = VLamClosure (LamClosure (_argType <$> args) (NE.length args) body Nothing env info)
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
    pure (Closure (_dfunName d) mn (_argType <$> args) (NE.length args) body (_dfunRType d) e i)
  _ ->
    throwExecutionError (_dfunInfo d) (DefIsNotClosure (_dfunName d))

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
evalCap info cont handler env (CapToken fqn args) contbody = do
  let qn = fqnToQualName fqn
  let ct = CapToken qn args
  enforceNotWithinDefcap info env "with-capability"
  lookupFqName fqn >>= \case
    Just (DCap d) -> do
      (esCaps . csSlots) %%= (CapSlot ct []:)
      let (env', capBody) = applyCapBody args (_dcapTerm d)
          cont' = CapBodyC env contbody cont
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
                      evalCEK cont' handler (set ceLocal env' env) capBody
                  _ -> failInvariant info "manager function mismatch"
        Just DefEvent ->
          failInvariant info "cannot evaluate the body of an event cap"
        Nothing -> evalCEK cont' handler (set ceLocal env' env) capBody
    Just {} ->
      failInvariant (view termInfo contbody) "Captoken references invalid def"
    Nothing -> failInvariant (view termInfo contbody) "No such def for evalCap"
  where
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
              evalCEK cont' handler inCapEnv capBody
            VError v -> returnCEK cont handler (VError v)
        _ -> failInvariant def "user managed cap is an invalid defn"
    _ -> failInvariant def "Invalid managed cap type"
  evalAutomanagedCap cont' env' capBody managedCap = case _mcManaged managedCap of
    AutoManaged b -> do
      if b then returnCEK cont handler (VError "automanaged cap used once")
      else do
        let newManaged = AutoManaged True
        esCaps . csManaged %%= S.union (S.singleton (set mcManaged newManaged managedCap))
        let inCapEnv = set ceLocal env' $ set ceInCap True $ env
        evalCEK cont' handler inCapEnv capBody
    _ -> failInvariant def "Invalid managed cap type"
  evaluate fqn' term managed value = case term of
    Lam _ lamargs body i -> do
      -- Todo: `applyLam` here gives suboptimal errors
      -- Todo: this completely violates our "step" semantics.
      -- This should be its own frame
      let inCapEnv = set ceInCap True env
      let clo = Closure (_fqName fqn') (_fqModule fqn') (_argType <$> lamargs) (NE.length lamargs) body Nothing inCapEnv i
      applyLam (C clo) [VPactValue managed, VPactValue value] Mt CEKNoHandler
    _t -> failInvariant (view termInfo _t) "mgr function was not a lambda"
  -- Todo: typecheck arg here
  -- Todo: definitely a bug if a cap has a lambda as a body
  applyCapBody bArgs (Lam _ _lamArgs body _) = (RAList.fromList (fmap VPactValue (reverse bArgs)), body)
  applyCapBody [] b = (mempty, b)
  applyCapBody _ b = (mempty, b)

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
requireCap cont handler (CapToken fqn args) = do
  let ct = CapToken (fqnToQualName fqn) args
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
composeCap cont handler env (CapToken fqn args) = do
  let ct = CapToken (fqnToQualName fqn) args
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
    CEKHandler env term cont' caps handler' -> case v of
      VError{} -> do
        setEvalState (esCaps . csSlots) caps
        evalCEK cont' handler' env term
      EvalValue v' ->
        returnCEKValue cont' handler' v'
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
-- Error terms that don't simply returnt the empty continuation
-- "Zero out" the continuation up to the latest handler
-- returnCEKValue _cont handler v@VError{} =
--   returnCEK Mt handler v
returnCEKValue (Args env i (x :| xs) cont) handler fn = do
  c <- canApply fn
  let cont' = Fn c env xs [] cont
  evalCEK cont' handler env x
  where
  canApply = \case
    -- Todo: restrict the type of closures applied to user functions
    VClosure (C clo) -> pure (C clo)
    VClosure (LC clo) -> pure (LC clo)
    VClosure (N clo) -> pure (N clo)
    VClosure (CT clo) -> pure (CT clo)
    _ ->
      throwExecutionError i CannotApplyPartialClosure
  -- evalCEK (Fn fn cont) handler env arg
returnCEKValue (Fn fn env args vs cont) handler v = do
  case args of
    [] -> do
      applyLam fn (reverse (v:vs)) cont handler
    x:xs ->
      evalCEK (Fn fn env xs (v:vs) cont) handler env x
returnCEKValue (LetC env term cont) handler v = do
  evalCEK cont handler (over ceLocal (RAList.cons v) env) term
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
  _ ->
    -- Todo: thread error loc here
    failInvariant info "Evaluation of conditional expression yielded non-boolean value"
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
returnCEKValue (StackPopC mty cont) handler v = do
  v' <- (`maybeTCType` mty) =<< enforcePactValue v
  -- Todo: unsafe use of tail here. need `tailMay`
  (esStack %%= tail) *> returnCEKValue cont handler (VPactValue v')



applyLam
  :: (MonadEval b i m)
  => CanApply b i m
  -> [CEKValue b i m]
  -> Cont b i m
  -> CEKErrorHandler b i m
  -> m (EvalResult b i m)
applyLam (C (Closure fn mn cloargs arity term mty env cloi)) args cont handler
  | arity == argLen = do
    args' <- traverse enforcePactValue args
    tcArgs <- zipWithM (\arg ty -> VPactValue <$> maybeTCType arg ty) args' (NE.toList cloargs)
    esStack %%= (StackFrame fn mn :)
    let cont' = StackPopC mty cont
        varEnv = RAList.fromList (reverse tcArgs)
    evalCEK cont' handler (set ceLocal varEnv env) term
  | argLen > arity = throwExecutionError cloi ClosureAppliedToTooManyArgs
  | otherwise = apply' mempty (NE.toList cloargs) args
  where
  argLen = length args
  -- Here we enforce an argument to a user fn is a
  apply' e (ty:tys) (x:xs) = do
    x' <- (`maybeTCType` ty) =<< enforcePactValue x
    apply' (RAList.cons (VPactValue x') e) tys xs
  apply' e (ty:tys) [] = do
    let env' = set ceLocal e env
        pclo = PartialClosure (Just (StackFrame fn mn)) (ty :| tys) (length tys + 1) term mty env' cloi
    returnCEKValue cont handler (VPartialClosure pclo)
  apply' _ [] _ = throwExecutionError cloi ClosureAppliedToTooManyArgs

applyLam (LC (LamClosure cloargs arity term mty env cloi)) args cont handler
  | arity == argLen = do
    let locals = view ceLocal env
        locals' = foldl' (flip RAList.cons) locals args
    evalCEK cont handler (set ceLocal locals' env) term
  | argLen > arity = throwExecutionError cloi ClosureAppliedToTooManyArgs
  | otherwise = apply' (view ceLocal env) (NE.toList cloargs) args
  where
  argLen = length args
  -- Todo: runtime TC here
  apply' e (ty:tys) (x:xs) = do
    x' <- (`maybeTCType` ty) =<< enforcePactValue x
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
    x' <- (`maybeTCType` ty) =<< enforcePactValue x
    apply' (RAList.cons (VPactValue x') e) tys xs
  apply' e [] [] = do
    case li of
      Just sf -> do
        let cont' = StackPopC mty cont
        esStack %%= (sf :)
        evalCEK cont' handler (set ceLocal e env) term
      Nothing -> evalCEK cont handler (set ceLocal e env) term
  apply' e (ty:tys) [] = do
    let pclo = PartialClosure li (ty :| tys) (length tys + 1) term mty (set ceLocal e env) i
    returnCEKValue cont handler (VPartialClosure pclo)
  apply' _ [] _ = throwExecutionError i ClosureAppliedToTooManyArgs

applyLam (N (NativeFn b env fn arity i)) args cont handler
  | arity == argLen = fn i b cont handler env args
  | argLen > arity = throwExecutionError i ClosureAppliedToTooManyArgs
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

applyLam (CT (CapTokenClosure fqn argtys arity i)) args cont handler
  | arity == argLen = do
    args' <- traverse enforcePactValue args
    tcArgs <- zipWithM (\arg ty -> maybeTCType arg ty) args' argtys
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
