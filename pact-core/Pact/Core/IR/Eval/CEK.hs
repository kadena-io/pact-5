module Pact.Core.IR.Eval.CEK where

import Control.Lens
import Control.Monad.Except
import Data.Default
import Data.Text(Text)
import Data.List.NonEmpty(NonEmpty(..))
import qualified Data.Map.Strict as Map
import qualified Data.RAList as RAList
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Set as S
import qualified Data.List.NonEmpty as NE

import Pact.Core.Builtin
import Pact.Core.Names
import Pact.Core.Errors
import Pact.Core.Gas
import Pact.Core.Literal
import Pact.Core.PactValue
import Pact.Core.Capabilities
import Pact.Core.Type

import Pact.Core.IR.Term
import Pact.Core.IR.Eval.Runtime

chargeNodeGas :: MonadEval b i m => NodeType -> m ()
chargeNodeGas nt = do
  gm <- view (cekGasModel . geGasModel . gmNodes) <$> cekReadEnv
  cekChargeGas (gm nt)
  -- gm <- view (cekGasModel . geGasModel . gmNodes)
  -- chargeGas (gm nt)

chargeNative :: MonadEval b i m => b -> m ()
chargeNative native = do
  gm <- view (cekGasModel . geGasModel . gmNatives) <$> cekReadEnv
  cekChargeGas (gm native)
  -- gm <- view (cekGasModel . geGasModel . gmNatives)
  -- chargeGas (gm native)

-- Todo: exception handling? do we want labels
-- Todo: `traverse` usage should be perf tested.
-- It might be worth making `Arg` frames incremental, as opposed to a traverse call
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
    NBound i -> case RAList.lookup env i of
      -- Todo: module ref anns here
      Just v -> returnCEKValue cont handler v
      Nothing -> failInvariant' ("unbound identifier" <> T.pack (show n)) info
    -- Top level names are not closures, so we wipe the env
    NTopLevel mname mh -> do
      let fqn = FullyQualifiedName mname (_nName n) mh
      cekReadEnv >>= \renv -> case Map.lookup fqn (view cekLoaded renv) of
        Just (Dfun d) -> evalCEK cont handler RAList.Nil (_dfunTerm d)
        Just _ -> failInvariant' "invalid call" info
        Nothing -> failInvariant' ("top level name " <> T.pack (show fqn) <> " not in scope") info
    NModRef m ifs ->
      returnCEKValue cont handler (VModRef m ifs)
evalCEK cont handler _env (Constant l _) = do
  chargeNodeGas ConstantNode
  returnCEKValue cont handler (VLiteral l)
evalCEK cont handler env (App fn args _) = do
  chargeNodeGas AppNode
  evalCEK (Args env args cont) handler env fn
evalCEK cont handler env (Let n ty e1 e2 i) =
  let lam = Lam AnonLamInfo (pure (Arg n ty)) e2 i
  in evalCEK cont handler env (App lam (pure e1) i)
evalCEK cont handler env (Lam li args body _) = do
  chargeNodeGas LamNode
  let clo = VClosure (Closure li (_argType <$> args) body env)
  returnCEKValue cont handler clo
evalCEK cont handler _env (Builtin b _) = do
  chargeNodeGas BuiltinNode
  builtins <- view cekBuiltins <$> cekReadEnv
  returnCEKValue cont handler (VNative (builtins b))
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
    WithCapability _ args body -> case args of
      x:xs -> let
        capFrame = WithCapFrame fqn body
        cont' = CapInvokeC env xs [] capFrame cont
        in evalCEK cont' handler env x
      [] -> evalCap cont handler env (CapToken fqn []) body
    RequireCapability _ args -> case args of
      [] -> requireCap cont handler (CapToken fqn [])
      x:xs -> let
        capFrame = RequireCapFrame fqn
        cont' = CapInvokeC env xs [] capFrame cont
        in evalCEK cont' handler env x
    ComposeCapability _ args -> case args of
      [] -> composeCap cont handler (CapToken fqn [])
      x:xs -> let
        capFrame = ComposeCapFrame fqn
        cont' = CapInvokeC env xs [] capFrame cont
        in evalCEK cont' handler env x
    InstallCapability _ args -> case args of
      [] -> installCap cont handler env (CapToken fqn [])
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
    CreateUserGuard{} -> error "implement"
evalCEK cont handler env (ListLit ts _) = do
  chargeNodeGas ListNode
  case ts of
    [] -> returnCEKValue cont handler (VList mempty)
    x:xs -> evalCEK (ListC env xs [] cont) handler env x
evalCEK cont handler env (Try e1 rest _) = do
  caps <- useCekState (esCaps . csSlots)
  let handler' = CEKHandler env e1 cont caps handler
  evalCEK Mt handler' env rest
evalCEK cont handler env (DynInvoke n fn _) =
  evalCEK (DynInvokeC env fn cont) handler env n
-- Error terms ignore the current cont
evalCEK _ handler _ (Error e _) =
  returnCEK Mt handler (VError e)

-- Todo: fail invariant
nameToFQN :: Applicative f => Name -> f FullyQualifiedName
nameToFQN (Name n nk) = case nk of
  NTopLevel mn mh -> pure (FullyQualifiedName mn n mh)
  NBound{} -> error "expected fully resolve FQ name"
  NModRef{} -> error "expected non-modref"

-- Todo: fail invariants
cekToPactValue :: Applicative f => CEKValue b i m -> f PactValue
cekToPactValue = \case
  VLiteral lit -> pure (PLiteral lit)
  VList vec -> PList <$> traverse cekToPactValue vec
  VClosure{} -> error "closure is not a pact value"
  VNative{} -> error "Native is not a pact value"
  VModRef mn mns -> pure (PModRef mn mns)
  VGuard gu -> pure (PGuard gu)

-- Todo: managed
evalCap
  :: MonadEval b i m
  => Cont b i m
  -> CEKErrorHandler b i m
  -> CEKEnv b i m
  -> CapToken
  -> EvalTerm b i
  -> m (EvalResult b i m)
evalCap cont handler env ct@(CapToken fqn args) contbody = do
  cekReadEnv >>= \renv -> case Map.lookup fqn (view cekLoaded renv) of
    Just (DCap d) -> do
      modifyCEKState (esCaps . csSlots) (CapSlot ct []:)
      let (env', capBody) = applyCapBody args (_dcapTerm d)
          cont' = CapBodyC env contbody cont
      evalCEK cont' handler env' capBody
    Just {} -> error "was not defcap, invariant violated"
    Nothing -> error "No such def"
  where
  -- Todo: typecheck arg here
  -- Todo: definitely a bug if a cap has a lambda as a body
  applyCapBody bArgs (Lam _ _lamArgs body _) = (RAList.fromList (fmap pactToCEKValue (reverse bArgs)), body)
  applyCapBody [] b = (mempty, b)
  applyCapBody _ _ = error "invariant broken: cap does not take arguments but is a lambda"


requireCap
  :: MonadEval b i m
  => Cont b i m
  -> CEKErrorHandler b i m
  -> CapToken
  -> m (EvalResult b i m)
requireCap cont handler ct = do
  caps <- useCekState (esCaps.csSlots)
  let csToSet cs = S.insert (_csCap cs) (S.fromList (_csComposed cs))
      capSet = foldMap csToSet caps
  if S.member ct capSet then returnCEKValue cont handler VUnit
  else throwExecutionError' (CapNotInScope "ovuvue")

composeCap
  :: (MonadEval b i m)
  => Cont b i m
  -> CEKErrorHandler b i m
  -> CapToken
  -> m (EvalResult b i m)
composeCap cont handler ct@(CapToken fqn args) = do
  cekReadEnv >>= \renv -> case Map.lookup fqn (view cekLoaded renv) of
    Just (DCap d) -> do
      modifyCEKState (esCaps . csSlots) (CapSlot ct []:)
      let (env', capBody) = applyCapBody args (_dcapTerm d)
          cont' = CapPopC PopCapComposed cont
      evalCEK cont' handler env' capBody
    Just {} -> error "was not defcap, invariant violated"
    Nothing -> error "No such def"
  where
  -- Todo: typecheck arg here
  -- Todo: definitely a bug if a cap has a lambda as a body
  applyCapBody bArgs (Lam _ _lamArgs body _) = (RAList.fromList (fmap pactToCEKValue (reverse bArgs)), body)
  applyCapBody [] b = (mempty, b)
  applyCapBody _ _ = error "invariant broken: cap does not take arguments but is a lambda"

installCap :: a
installCap = undefined

emitEvent
  :: MonadEval b i m
  => Cont b i m
  -> CEKErrorHandler b i m
  -> CapToken
  -> m (EvalResult b i m)
emitEvent cont handler ct@(CapToken fqn _) = do
  let pactEvent = PactEvent ct (_fqModule fqn) (_fqHash fqn)
  modifyCEKState esEvents (pactEvent:)
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
        setCekState (esCaps . csSlots) caps
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
    VClosure clo -> pure (C clo)
    VNative bfn -> pure (N bfn)
    _ -> error "Cannot apply"
  -- evalCEK (Fn fn cont) handler env arg
returnCEKValue (Fn fn env args vs cont) handler v = case args of
  [] -> applyLam fn (reverse (v:vs)) cont handler
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
  _ -> failInvariant "Evaluation of conditional expression yielded non-boolean value"
returnCEKValue (CapInvokeC env terms pvs cf cont) handler v = case terms of
  x:xs -> do
    pv <- cekToPactValue v
    let cont' = CapInvokeC env xs (pv:pvs) cf cont
    evalCEK cont' handler env x
  [] -> case cf of
    WithCapFrame fqn wcbody ->
      evalCap cont handler env (CapToken fqn (reverse pvs)) wcbody
    RequireCapFrame fqn  ->
      requireCap cont handler (CapToken fqn (reverse pvs))
    ComposeCapFrame fqn ->
      composeCap cont handler (CapToken fqn (reverse pvs))
    InstallCapFrame{} -> error "todo"
    EmitEventFrame fqn ->
      emitEvent cont handler (CapToken fqn (reverse pvs))
returnCEKValue (CapBodyC env term cont) handler _ = do
  let cont' = CapPopC PopCapInvoke cont
  evalCEK cont' handler env term
returnCEKValue (CapPopC st cont) handler v = case st of
  PopCapInvoke -> do
    -- todo: need safe tail here, but this should be fine given the invariant that `CapPopC`
    -- will never show up otherwise
    modifyCEKState (esCaps . csSlots) tail
    returnCEKValue cont handler v
  PopCapComposed -> do
    caps <- useCekState (esCaps . csSlots)
    let cs = head caps
        csList = _csCap cs : _csComposed cs
        caps' = over (_head . csComposed) (++ csList) (tail caps)
    setCekState (esCaps . csSlots) caps'
    returnCEKValue cont handler VUnit
returnCEKValue (ListC env args vals cont) handler v = do
  case args of
    [] ->
      returnCEKValue cont handler (VList (V.fromList (reverse (v:vals))))
    e:es ->
      evalCEK (ListC env es (v:vals) cont) handler env e
-- Todo: note over here we might want to typecheck
-- Todo: inline the variable lookup instead of calling EvalCEK directly,
-- as we can provide a better error message this way.
returnCEKValue (DynInvokeC env fn cont) handler v = case v of
  VModRef mn _ -> do
    -- Todo: for when persistence is implemented
    -- here is where we would incur module loading
    cekReadEnv >>= \e -> case view (cekMHashes . at mn) e of
      Just mh ->
        evalCEK cont handler env (Var (Name fn (NTopLevel mn mh)) def)
      Nothing -> failInvariant "No such module"
  _ -> failInvariant "Not a modref"
returnCEKValue (StackPopC cont) handler v =
  modifyCEKState esStack tail *> returnCEKValue cont handler v

applyLam
  :: (MonadEval b i m)
  => CanApply b i m
  -> [CEKValue b i m]
  -> Cont b i m
  -> CEKErrorHandler b i m
  -> m (EvalResult b i m)
applyLam (C (Closure li cloargs term env)) args cont handler = apply' env (NE.toList cloargs) args
  where
  -- Todo: runtime TC here
  apply' e (_ty:tys) (x:xs) =
    apply' (RAList.cons x e) tys xs
  apply' e [] [] =
    -- Todo: stack frame here
    evalCEK cont handler e term
  apply' e (ty:tys) [] =
    returnCEKValue cont handler (VClosure (Closure li (ty :| tys) term e))
  apply' _ [] _ = error "Applying too many arguments to function"

applyLam (N (BuiltinFn b fn arity vs)) args cont handler = apply' arity vs args
  where
  apply' !a pa (x:xs)
    | a <= 0 = error "Applying too many args to native"
    | otherwise = apply' (a - 1) (x:pa) xs
  apply' !a pa []
    | a == 0 = fn cont handler (reverse pa)
    | otherwise = returnCEKValue cont handler (VNative (BuiltinFn b fn a pa))

failInvariant :: MonadEval b i m => Text -> m a
failInvariant b =
  let e = PEExecutionError (InvariantFailure b) def
  in throwError e

failInvariant' :: MonadEval b i m => Text -> i -> m a
failInvariant' b i =
  let e = PEExecutionError (InvariantFailure b) i
  in throwError e

throwExecutionError' :: (MonadEval b i m) => ExecutionError -> m a
throwExecutionError' e = throwError (PEExecutionError e def)

unsafeApplyOne
  :: MonadEval b i m
  => CEKValue b i m
  -> CEKValue b i m
  -> m (EvalResult b i m)
unsafeApplyOne (VClosure c) arg = do
  let cont = Fn (C c) mempty [] [] Mt
  returnCEKValue cont CEKNoHandler arg
unsafeApplyOne (VNative (BuiltinFn b fn arity args)) arg =
  if arity - 1 <= 0 then fn Mt CEKNoHandler (reverse (arg:args))
  else pure (EvalValue (VNative (BuiltinFn b fn (arity - 1) (arg:args))))
unsafeApplyOne _ _ = failInvariant "Applied argument to non-closure in native"

unsafeApplyTwo
  :: MonadEval b i m
  => CEKValue b i m
  -> CEKValue b i m
  -> CEKValue b i m
  -> m (EvalResult b i m)
unsafeApplyTwo (VClosure c) arg1 arg2 = do
  let cont = Fn (C c) mempty [] [arg1] Mt
  returnCEKValue cont CEKNoHandler arg2
unsafeApplyTwo (VNative (BuiltinFn b fn arity args)) arg1 arg2 =
  if arity - 2 <= 0 then fn Mt CEKNoHandler (reverse (arg1:arg2:args))
  else pure $ EvalValue $ VNative $ BuiltinFn b fn (arity - 2) (arg1:arg2:args)
unsafeApplyTwo _ _ _ = failInvariant "Applied argument to non-closure in native"
