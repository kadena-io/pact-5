{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pact.Core.IR.Eval.CEK
  ( eval
  , returnCEKValue
  , returnCEK
  , applyLam
  , unsafeApplyOne
  , unsafeApplyTwo
  , mkDefPactClosure
  , resumePact
  , evalCap
  , nameToFQN
  , guardTable
  , isKeysetInSigs
  , isKeysetNameInSigs
  , requireCap
  , installCap
  , composeCap
  , mkDefunClosure
  , enforceNotWithinDefcap
  , acquireModuleAdmin
  , isCapInStack
  , filterIndex
  , findMsgSigCap
  , evalWithStackFrame
  , emitEvent
  , emitCapability
  , guardForModuleCall
  ) where


import Control.Lens
import Control.Monad(zipWithM, unless, when)
import Data.Default
import Data.List.NonEmpty(NonEmpty(..))
import Data.Foldable(find, foldl')
import qualified Data.RAList as RAList
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
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
import Pact.Core.StableEncoding

import Pact.Core.IR.Term
import Pact.Core.IR.Eval.Runtime
import Pact.Core.DefPacts.Types

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
        Just (DConst d) -> case _dcTerm d of
          -- Todo: should this be an error?
          -- probably.
          TermConst term ->
            evalCEK cont handler (set ceLocal mempty env) term
          EvaledConst v ->
            returnCEKValue cont handler (VPactValue v)
        Just (DPact d) -> do
          dpactClo <- mkDefPactClosure info fqn d env
          returnCEKValue cont handler dpactClo
        Just (DTable d) ->
          let (ResolvedTable sc) = _dtSchema d
              tn = TableName (_dtName d) mname
              tbl = VTable (TableValue tn mh sc)
          in returnCEKValue cont handler tbl
        Just (DCap d) -> do
          let args = _argType <$> _dcapArgs d
              clo = CapTokenClosure fqn args (length args) info
          returnCEKValue cont handler (VClosure (CT clo))
        Just d ->
          throwExecutionError info (InvalidDefKind (defKind d) "in var position")
          -- TODO ^ shall this be an invariant failure?
          -- apparently this can never happen because `renameTerm` checks for variable top-level variable occurrences,
          -- erroring out with `InvalidDefInTermVariable` if it's violated.
          -- And, the execution can't change that invariant once it's established during desugar.
        Nothing ->
          throwExecutionError info (NameNotInScope (FullyQualifiedName mname (_nName n) mh))
          -- TODO ^ ditto, `renameTerm` apparently always fails in this case as well
    NModRef m ifs -> case ifs of
      [x] -> returnCEKValue cont handler (VModRef (ModRef m ifs (Just (S.singleton x))))
      [] -> throwExecutionError info (ModRefNotRefined (_nName n))
      _ -> returnCEKValue cont handler (VModRef (ModRef m ifs Nothing))
    NDynRef (DynamicRef dArg i) -> case RAList.lookup (view ceLocal env) i of
      Just (VModRef mr) -> do
        modRefHash <- _mHash <$> getModule info (view cePactDb env) (_mrModule mr)
        let nk = NTopLevel (_mrModule mr) modRefHash
        evalCEK cont handler env (Var (Name dArg nk) info)
      Just _ -> returnCEK cont handler (VError "dynamic name pointed to non-modref" info)
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
evalCEK cont handler env (Let _ e1 e2 _) = do
  let cont' = LetC env e2 cont
  evalCEK cont' handler env e1
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
  CEnforce cond str -> do
    let env' = sysOnlyEnv env
    evalCEK (CondC env' info (EnforceFrame str) cont) handler env' cond
  CEnforceOne str conds -> case conds of
    [] -> returnCEK cont handler (VError "enforce-one failure" info)
    x:xs -> do
      errState <- evalStateToErrorState <$> getEvalState
      let env' = readOnlyEnv env
      let handler' = CEKEnforceOne env' info str xs cont errState handler
      let cont' = CondC env' info (EnforceOneFrame str xs) Mt
      evalCEK cont' handler' env' x
evalCEK cont handler env (CapabilityForm cf info) = do
  fqn <- nameToFQN info env (view capFormName cf)
  case cf of
    -- Todo: duplication here in the x:xs case
    WithCapability _ args body -> do
      enforceNotWithinDefcap info env "with-capability"
      case args of
        x:xs -> do
          let capFrame = WithCapFrame fqn body
          let cont' = CapInvokeC env info xs [] capFrame cont
          evalCEK cont' handler env x
        [] -> evalCap info cont handler env (CapToken fqn []) (CapBodyC PopCapInvoke) body
    CreateUserGuard _ args -> case args of
      [] -> createUserGuard info cont handler fqn []
      x : xs -> let
        capFrame = CreateUserGuardFrame fqn
        cont' = CapInvokeC env info xs [] capFrame cont
        in evalCEK cont' handler env x
evalCEK cont handler env (ListLit ts info) = do
  chargeNodeGas ListNode
  case ts of
    [] -> returnCEKValue cont handler (VList mempty)
    x:xs -> evalCEK (ListC env info xs [] cont) handler env x
evalCEK cont handler env (Try catchExpr rest _) = do
  errState <- evalStateToErrorState <$> getEvalState
  let handler' = CEKHandler env catchExpr cont errState handler
  let env' = readOnlyEnv env
  evalCEK Mt handler' env' rest
evalCEK cont handler env (ObjectLit o info) =
  case o of
    (f, term):rest -> do
      let cont' = ObjC env info f rest [] cont
      evalCEK cont' handler env term
    [] -> returnCEKValue cont handler (VObject mempty)
-- Error terms ignore the current cont
evalCEK _ handler _ (Error e info) =
  returnCEK Mt handler (VError e info)

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
    -- TODO ^ apparently invariant failure, since the parser ensures defun has a form of `(defun ( args ) body)`,
    -- and desugar converts that into a Lam or Nullary.

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
  -> DefPactContinuation QualifiedName PactValue
  -> Cont b i m
  -> CEKErrorHandler b i m
  -> CEKEnv b i m
  -> m (EvalResult b i m)
initPact i pc cont handler cenv = do
  case view ceDefPactStep cenv of
    Nothing -> do
      pHash <- viewEvalEnv eeHash
      let
        pStep = DefPactStep 0 False (hashToDefPactId pHash) Nothing
        cenv' = set ceDefPactStep (Just pStep) cenv
      applyPact i pc pStep cont handler cenv' mempty
    Just ps ->
      let
        DefPactId p = _psDefPactId ps
        npId = hashToDefPactId (pactHash (T.encodeUtf8 p <> ":" <> encodeStable pc))
        pStep = DefPactStep (_psStep ps) (_psRollback ps) npId Nothing
      in applyNestedPact i pc pStep cont handler cenv
  where
    hashToDefPactId = DefPactId . hashToText

applyPact
  :: MonadEval b i m
  => i
  -> DefPactContinuation QualifiedName PactValue
  -> DefPactStep
  -> Cont b i m
  -> CEKErrorHandler b i m
  -> CEKEnv b i m
  -> M.Map DefPactId DefPactExec
  -> m (EvalResult b i m)
applyPact i pc ps cont handler cenv nested = useEvalState esDefPactExec >>= \case
  Just pe ->  throwExecutionError i (MultipleOrNestedDefPactExecFound pe)
  Nothing -> getModuleMember i (_cePactDb cenv) (pc ^. pcName) >>= \case
    DPact defPact -> do
      let nSteps = NE.length (_dpSteps defPact)

      -- Check we try to apply the correct pact Step
      unless (ps ^. psStep < nSteps) $
        throwExecutionError i (DefPactStepNotFound ps nSteps)

      step <- maybe (failInvariant i "Step not found") pure
        $ _dpSteps defPact ^? ix (ps ^. psStep)

      let pe = DefPactExec
               { _peYield = Nothing
               , _peStepHasRollback = hasRollback step
               , _peStepCount = nSteps
               , _peStep = _psStep ps
               , _peDefPactId = _psDefPactId ps
               , _peContinuation = pc
               , _peNestedDefPactExec = nested
               }

      setEvalState esDefPactExec (Just pe)
      let cont' = DefPactStepC cenv cont

      case (ps ^. psRollback, step) of
        (False, _) ->
          evalWithStackFrame i cont' handler cenv sf Nothing (ordinaryDefPactStepExec step)
        (True, StepWithRollback _ rollbackExpr) ->
          evalWithStackFrame i cont' handler cenv sf Nothing rollbackExpr
        (True, Step{}) -> throwExecutionError i (DefPactStepHasNoRollback ps)
    _otherwise -> failInvariant i "defpact continuation does not point to defun"
  where
  sf = StackFrame (view (pcName . qnName) pc) (view (pcName . qnModName) pc) SFDefPact

applyNestedPact
  :: MonadEval b i m
  => i
  -> DefPactContinuation QualifiedName PactValue
  -> DefPactStep
  -> Cont b i m
  -> CEKErrorHandler b i m
  -> CEKEnv b i m
  -> m (EvalResult b i m)
applyNestedPact i pc ps cont handler cenv = useEvalState esDefPactExec >>= \case
  Nothing -> failInvariant i $
    "applyNestedPact: Nested DefPact attempted but no pactExec found" <> T.pack (show pc)

  Just pe -> getModuleMember i (_cePactDb cenv) (pc ^. pcName) >>= \case
    DPact defPact -> do
      step <- maybe (failInvariant i "Step not found") pure
        $ _dpSteps defPact ^? ix (ps ^. psStep)

      let
        stepCount = NE.length (_dpSteps defPact)
        isRollback = hasRollback step

      when (stepCount /= _peStepCount pe) $
        throwExecutionError i (NestedDefPactParentStepCountMismatch (_peDefPactId pe) stepCount (_peStepCount pe))

      when (isRollback /= _peStepHasRollback pe) $
        throwExecutionError i (NestedDefPactParentRollbackMismatch (_peDefPactId pe) isRollback (_peStepHasRollback pe))

      exec <- case pe ^. peNestedDefPactExec . at (_psDefPactId ps) of
        Nothing
          | _psStep ps == 0 -> pure $ DefPactExec
                               { _peStepCount = stepCount
                               , _peYield = Nothing
                               , _peStep = _psStep ps
                               , _peDefPactId = _psDefPactId ps
                               , _peContinuation = pc
                               , _peStepHasRollback = isRollback
                               , _peNestedDefPactExec = mempty
                               }
          | otherwise ->
            throwExecutionError i (NestedDefPactDoubleExecution ps)
        Just npe
          | _psStep ps >= 0 && isRollback && _peStep npe == _psStep ps ->
            pure (set peStepHasRollback isRollback npe)
          | _psStep ps >  0 && _peStep npe + 1 == _psStep ps ->
            pure (over peStep (+1) $ set peStepHasRollback isRollback npe)
          | otherwise ->
            throwExecutionError i (NestedDefPactNeverStarted ps)

      setEvalState esDefPactExec (Just exec)
      let
        cenv' = set ceDefPactStep (Just ps) cenv
        cont' = NestedDefPactStepC cenv' cont pe

      case (ps ^. psRollback, step) of
        (False, _) ->
          evalWithStackFrame i cont' handler cenv' sf Nothing  (ordinaryDefPactStepExec step)
        (True, StepWithRollback _ rollbackExpr) ->
          evalWithStackFrame i cont' handler cenv' sf Nothing rollbackExpr
        (True, Step{}) -> throwExecutionError i (DefPactStepHasNoRollback ps)
    _otherwise -> failInvariant i "applyNestedPact: Expected a DefPact bot got something else"
  where
  sf = StackFrame (view (pcName . qnName) pc) (view (pcName . qnModName) pc) SFDefPact

resumePact
  :: MonadEval b i m
  => i
  -> Cont b i m
  -> CEKErrorHandler b i m
  -> CEKEnv b i m
  -> Maybe DefPactExec
  -> m (EvalResult b i m)
resumePact i cont handler env crossChainContinuation = viewEvalEnv eeDefPactStep >>= \case
  Nothing -> throwExecutionError i DefPactStepNotInEnvironment    -- <- TODO apparently can't happen, `continuePact` ensures that
  Just ps -> do
    pdb <- viewEvalEnv eePactDb
    dbState <- liftDbFunction i (readDefPacts pdb (_psDefPactId ps))
    case (dbState, crossChainContinuation) of
      (Just Nothing, _) -> throwExecutionError i (DefPactAlreadyCompleted ps)
      (Nothing, Nothing) -> throwExecutionError i (NoPreviousDefPactExecutionFound ps)
      (Nothing, Just ccExec) -> resumeDefPactExec ccExec
      (Just (Just dbExec), Nothing) -> resumeDefPactExec dbExec
      (Just (Just dbExec), Just ccExec) -> do

        -- Validate CC execution environment progressed far enough
        unless (_peStep ccExec > succ (_peStep dbExec)) $
          throwExecutionError i
            (CCDefPactContinuationError ps ccExec dbExec)

        -- Validate continuation db state
        when (_peContinuation dbExec /= _peContinuation ccExec) $
          throwExecutionError i (CCDefPactContinuationError ps ccExec dbExec)

        -- Validate step count against db state
        when (_peStepCount dbExec /= _peStepCount ccExec) $
          throwExecutionError i (CCDefPactContinuationError ps ccExec dbExec)

        resumeDefPactExec ccExec
      where
        --resumeDefPactExec :: MonadEval b i m => DefPactExec -> m (EvalResult b i m)
        resumeDefPactExec pe = do
          when (_psDefPactId ps /= _peDefPactId pe) $
            throwExecutionError i (DefPactIdMismatch (_psDefPactId ps) (_peDefPactId pe))

          when (_psStep ps < 0 || _psStep ps >= _peStepCount pe) $
            throwExecutionError i (InvalidDefPactStepSupplied ps pe)

          if _psRollback ps
            then when (_psStep ps /= _peStep pe) $
                 throwExecutionError i (DefPactRollbackMismatch ps pe)
            else when (_psStep ps /= succ (_peStep pe)) $
                 throwExecutionError i (DefPactStepMismatch ps pe)

          let pc = view peContinuation pe
              args = VPactValue <$> _pcArgs pc
              resume = case _psResume ps of
                         r@Just{} -> r
                         Nothing -> _peYield pe
              env' = set ceLocal (RAList.fromList (reverse args)) $ set ceDefPactStep (Just $ set psResume resume ps) env
          applyPact i pc ps cont handler env' (_peNestedDefPactExec pe)


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
      md <- getModule info (view cePactDb env) (_mrModule mr)
      pure (FullyQualifiedName (_mrModule mr) dArg (_mHash md))
    Just _ -> throwExecutionError info (DynNameIsNotModRef dArg)
    -- TODO this ^ is supposedly caught by the typechecker, so it's an invariant error now
    Nothing -> failInvariant info ("unbound identifier" <> T.pack (show n))
  _ -> failInvariant info ("invalid name in fq position" <> T.pack (show n))

guardTable :: (MonadEval b i m) => i -> CEKEnv b i m -> TableValue -> GuardTableOp -> m ()
guardTable i env (TableValue tn mh _) dbop = do
  checkLocalBypass $
    guardForModuleCall i env (_tableModuleName tn) $ do
      mdl <- getModule i (view cePactDb env) (_tableModuleName tn)
      enforceBlessedHashes i mdl mh
  where
  checkLocalBypass notBypassed = do
    enabled <- isExecutionFlagSet FlagAllowReadInLocal
    case dbop of
      GtWrite -> notBypassed
      GtCreateTable -> notBypassed
      _ | enabled -> pure ()
        | otherwise -> notBypassed


enforceBlessedHashes :: (MonadEval b i m) => i -> EvalModule b i -> ModuleHash -> m ()
enforceBlessedHashes info md mh
  | _mHash md == mh = return ()
  | mh `S.member` (_mBlessed md) = return ()
  | otherwise = throwExecutionError info (HashNotBlessed (_mName md) mh)

guardForModuleCall :: (MonadEval b i m) => i -> CEKEnv b i m -> ModuleName -> m () -> m ()
guardForModuleCall i env currMod onFound =
  findCallingModule >>= \case
    Just mn | mn == currMod -> onFound
    _ -> getModule i (view cePactDb env) currMod >>= acquireModuleAdmin i env

acquireModuleAdmin :: (MonadEval b i m) => i -> CEKEnv b i m -> EvalModule b i -> m ()
acquireModuleAdmin i env mdl = do
  mc <- useEvalState (esCaps . csModuleAdmin)
  unless (S.member (_mName mdl) mc) $ case _mGovernance mdl of
    KeyGov ksn -> do
      enforceKeysetNameAdmin i (_mName mdl) ksn
      esCaps . csModuleAdmin %== S.insert (_mName mdl)
    CapGov (ResolvedGov fqn) -> do
      let wcapBody = Constant LUnit i
      -- *special* use of `evalCap` here to evaluate module governance.
      evalCap i Mt CEKNoHandler env (CapToken fqn []) (CapBodyC PopCapInvoke) wcapBody >>= \case
        VError _ _ ->
          throwExecutionError i (ModuleGovernanceFailure (_mName mdl))
        EvalValue _ -> do
          esCaps . csModuleAdmin %== S.insert (_mName mdl)

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
  cont' <- pushStackFrame info cont mty sf
  evalCEK cont' handler env body

pushStackFrame
  :: (MonadEval b i m)
  => i
  -> Cont b i m
  -> Maybe Type
  -> StackFrame
  -> m (Cont b i m)
pushStackFrame info cont mty sf = do
  esStack %== (sf :)
  pure (StackPopC info mty cont)


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
  -> (CEKEnv b i m -> Maybe (CapToken QualifiedName PactValue) -> Maybe (PactEvent PactValue) -> EvalTerm b i -> Cont b i m -> Cont b i m)
  -> EvalTerm b i
  -> m (EvalResult b i m)
evalCap info currCont handler env origToken@(CapToken fqn args) modCont contbody = isCapInStack' origToken >>= \case
  False -> do
    lookupFqName fqn >>= \case
      Just (DCap d) -> do
        when (length args /= _dcapAppArity d) $ failInvariant info "Dcap argument length mismatch"
        let newLocals = RAList.fromList $ fmap VPactValue (reverse args)
            capBody = _dcapTerm d
        -- Todo: clean up the staircase of doom.
        case _dcapMeta d of
          -- Managed capability, so we should look for it in the set of csmanaged
          DefManaged mdm -> do
            case mdm of
              -- | Not automanaged, so it must have a defmeta
              -- We are handling user-managed caps
              DefManagedMeta (cix,_) _ -> do
                let filteredCap = CapToken qualCapName (filterIndex cix args)
                -- Find the capability post-filtering
                mgdCaps <- useEvalState (esCaps . csManaged)
                case find ((==) filteredCap . _mcCap) mgdCaps of
                  Nothing -> do
                    msgCaps <- S.unions <$> viewEvalEnv eeMsgSigs
                    case find (findMsgSigCap cix filteredCap) msgCaps of
                      Just c -> do
                        let c' = set ctName fqn c
                            cont' = modCont env (Just qualCapToken) (Just (fqctToPactEvent origToken)) contbody currCont
                        installCap info env c' False >>= evalUserManagedCap cont' newLocals capBody
                      Nothing ->
                        throwExecutionError info (CapNotInstalled fqn)
                  Just managedCap -> do
                    let cont' = modCont env (Just qualCapToken) (Just (fqctToPactEvent origToken)) contbody currCont
                    evalUserManagedCap cont' newLocals capBody managedCap
              -- handle autonomous caps
              AutoManagedMeta -> do
                -- Find the capability post-filtering
                let cont' = modCont env Nothing (Just (fqctToPactEvent origToken)) contbody currCont
                mgdCaps <- useEvalState (esCaps . csManaged)
                case find ((==) qualCapToken . _mcCap) mgdCaps of
                  Nothing -> do
                    msgCaps <- S.unions <$> viewEvalEnv eeMsgSigs
                    case find ((==) qualCapToken) msgCaps of
                      Just c -> do
                        let c' = set ctName fqn c
                        installCap info env c' False >>= evalAutomanagedCap cont' newLocals capBody
                      Nothing ->
                        throwExecutionError info (CapNotInstalled fqn)
                  Just managedCap ->
                    evalAutomanagedCap cont' newLocals capBody managedCap
                      -- if b then
                      --   returnCEK cont' handler (VError "Automanaged capability used more than once" info)
                      -- else do
                      --   let newManaged = AutoManaged True
                      --   esCaps . csManaged %== S.union (S.singleton (set mcManaged newManaged managedCap))
                      --   (esCaps . csSlots) %== (CapSlot qualCapToken []:)
                      --   sfCont <- pushStackFrame info cont' Nothing capStackFrame
                      --   emitCapability info origToken
                      --   evalCEK sfCont handler env capBody
                        -- evalWithStackFrame info cont' handler (set ceLocal env' env) capStackFrame Nothing capBody
                    -- _ -> failInvariant info "manager function mismatch"
          DefEvent -> do
            let cont' = modCont env Nothing (Just (fqctToPactEvent origToken)) contbody currCont
            let inCapEnv = set ceInCap True $ set ceLocal newLocals env
            (esCaps . csSlots) %== (CapSlot qualCapToken []:)
            sfCont <- pushStackFrame info cont' Nothing capStackFrame
            -- emitCapability info origToken
            evalCEK sfCont handler inCapEnv capBody
            -- evalWithStackFrame info cont' handler (set ceLocal newLocals env) capStackFrame Nothing capBody
          -- Not automanaged _nor_ user managed.
          -- Todo: a type that's basically `Maybe` here would save us a lot of grief.
          Unmanaged -> do
            let cont' = modCont env Nothing Nothing contbody currCont
            (esCaps . csSlots) %== (CapSlot qualCapToken []:)
            evalWithStackFrame info cont' handler (set ceLocal newLocals env) capStackFrame Nothing capBody
      Just {} ->
        failInvariant info "Captoken references invalid def"
      Nothing -> failInvariant info "No such def for evalCap"
  True ->
    evalCEK currCont handler env contbody
  where
  qualCapName = fqnToQualName fqn
  qualCapToken = CapToken qualCapName args
  capStackFrame = StackFrame (_fqName fqn) (_fqModule fqn) SFDefcap
  -- This function is handles both evaluating the manager function for the installed parameter
  -- and continuing evaluation for the actual capability body.
  -- Todo: currently, pact does this _after_ evaluation of the cap body. Should we do this?
  evalUserManagedCap cont' env' capBody managedCap =  case _mcManaged managedCap of
    ManagedParam mpfqn pv managedIx -> do
      lookupFqName mpfqn >>= \case
        -- We found the manager function, evaluate it and commit the argument.
        Just (Dfun dfun) -> do
          mparam <- maybe (failInvariant info "Managed param does not exist at index") pure (args ^? ix managedIx)
          evaluate mpfqn (_dfunTerm dfun) pv mparam >>= \case
            EvalValue res -> do
              result <- enforcePactValue info res
              let mcM = ManagedParam mpfqn result managedIx
              let inCapEnv = set ceInCap True $ set ceLocal env' $ env
              let inCapBodyToken = _mcOriginalCap managedCap
              -- BIG SEMANTICS NOTE HERE
              -- the cap slot here that we push should NOT be the qualified original token.
              -- Instead, it's the original token from the installed from the static cap. Otherwise, enforce checks
              -- within the cap body will fail (That is, keyset enforcement). Instead, once we are evaluating the body,
              -- we pop the current cap stack, then replace the head with the original intended token.
              -- this is done in `CapBodyC` and this is the only way to do this.
              esCaps . csManaged %== S.union (S.singleton (set mcManaged mcM managedCap))
              (esCaps . csSlots) %== (CapSlot inCapBodyToken []:)
              sfCont <- pushStackFrame info cont' Nothing capStackFrame
              -- emitCapability info origToken
              evalCEK sfCont handler inCapEnv capBody
              -- evalWithStackFrame info cont' handler inCapEnv capStackFrame Nothing capBody
            VError v i -> returnCEK currCont handler (VError v i)
        _ -> failInvariant info "user managed cap is an invalid defn"
    _ -> failInvariant info "Invalid managed cap type"
  evalAutomanagedCap cont' env' capBody managedCap = case _mcManaged managedCap of
    AutoManaged b -> do
      if b then returnCEK currCont handler (VError "Automanaged capability used more than once" info)
      else do
        let newManaged = AutoManaged True
        esCaps . csManaged %== S.union (S.singleton (set mcManaged newManaged managedCap))
        esCaps . csSlots %== (CapSlot qualCapToken []:)
        let inCapEnv = set ceLocal env' $ set ceInCap True $ env
        sfCont <- pushStackFrame info cont' Nothing capStackFrame
        -- emitCapability info origToken
        evalCEK sfCont handler inCapEnv capBody
        -- evalWithStackFrame info cont' handler inCapEnv capStackFrame Nothing capBody
    _ -> failInvariant info "Invalid managed cap type"
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

emitEvent
  :: (MonadEval b i m)
  => i
  -> PactEvent PactValue
  -> m ()
emitEvent info pe = findCallingModule >>= \case
    Just mn -> do
      let ctModule = _peModule pe
      if ctModule == mn then do
        esEvents %== (++ [pe])
      else throwExecutionError info (EventDoesNotMatchModule mn)
      -- let fqn = _ctName ct
      -- let ctModule = _fqModule fqn
    Nothing -> failInvariant info "emit-event called outside of module code"

emitCapability
  :: (MonadEval b i m)
  => i
  -> CapToken FullyQualifiedName PactValue
  -> m ()
emitCapability info tkn =
  emitEvent info (fqctToPactEvent tkn)

fqctToPactEvent :: CapToken FullyQualifiedName PactValue -> PactEvent PactValue
fqctToPactEvent (CapToken fqn args) = PactEvent (_fqName fqn) args (_fqModule fqn) (_fqHash fqn)

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
  => i
  -> Cont b i m
  -> CEKErrorHandler b i m
  -> FQCapToken
  -> m (EvalResult b i m)
requireCap info cont handler (CapToken fqn args) = do
  capInStack <- isCapInStack (CapToken (fqnToQualName fqn) args)
  if capInStack then returnCEKValue cont handler (VBool True)
  else returnCEK cont handler $ VError ("cap not in scope " <> renderQualName (fqnToQualName fqn)) info

isCapInStack
  :: (MonadEval b i m)
  => CapToken QualifiedName PactValue
  -> m Bool
isCapInStack ct = do
  capSet <- getAllStackCaps
  pure $ S.member ct capSet

isCapInStack'
  :: (MonadEval b i m)
  => CapToken FullyQualifiedName PactValue
  -> m Bool
isCapInStack' (CapToken fqn args) =
  isCapInStack (CapToken (fqnToQualName fqn) args)

composeCap
  :: (MonadEval b i m)
  => i
  -> Cont b i m
  -> CEKErrorHandler b i m
  -> CEKEnv b i m
  -> FQCapToken
  -> m (EvalResult b i m)
composeCap info cont handler env origToken =
  isCapInStack' origToken >>= \case
    False ->
      evalCap info cont handler env origToken (CapBodyC PopCapComposed) (Constant (LBool True) info)
      -- let ct = CapToken (fqnToQualName fqn) args
      -- lookupFqName fqn >>= \case
      --   Just (DCap d) -> do
      --     (esCaps . csSlots) %== (CapSlot ct []:)
      --     args' <- zipWithM (\pv arg -> maybeTCType info pv (_argType arg)) args (_dcapArgs d)
      --     let env' = RAList.fromList $ fmap VPactValue (reverse args')
      --         capBody = _dcapTerm d
      --     let cont' = UserGuardC (CapPopC PopCapComposed cont)
      --     evalCEK cont' handler (set ceLocal env' env) capBody
      --   -- todo: this error loc is _not_ good. Need to propagate `i` here, maybe in the stack
      --   Just d ->
      --     throwExecutionError (defInfo d) $ InvalidDefKind (defKind d) "in compose-capability"
      --   Nothing ->
      --     -- Todo: error loc here
      --     throwExecutionError' (NoSuchDef fqn)
    True -> returnCEKValue cont handler (VBool True)

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
  -> Bool
  -> m (ManagedCap QualifiedName PactValue)
installCap info _env (CapToken fqn args) autonomous = do
  let ct = CapToken (fqnToQualName fqn) args
  lookupFqName fqn >>= \case
    Just (DCap d) -> case _dcapMeta d of
      DefManaged m -> case m of
        DefManagedMeta (paramIx,_) (FQName fqnMgr) -> do
          managedParam <- maybe (throwExecutionError info (InvalidManagedCap fqn)) pure (args ^? ix paramIx)
          let mcapType = ManagedParam fqnMgr managedParam paramIx
              ctFiltered = CapToken (fqnToQualName fqn) (filterIndex paramIx args)
              mcap = ManagedCap ctFiltered ct mcapType
          (esCaps . csManaged) %== S.insert mcap
          when autonomous $
            (esCaps . csAutonomous) %== S.insert ct
          pure mcap
        AutoManagedMeta -> do
          let mcapType = AutoManaged False
              mcap = ManagedCap ct ct mcapType
          (esCaps . csManaged) %== S.insert mcap
          when autonomous $
            (esCaps . csAutonomous) %== S.insert ct
          pure mcap
      DefEvent ->
        throwExecutionError info (InvalidManagedCap fqn)
      Unmanaged -> throwExecutionError info (InvalidManagedCap fqn)
    Just d ->
      -- todo: error loc here is not in install-cap
      throwExecutionError (defInfo d) (InvalidDefKind (defKind d) "install-capability")
    Nothing -> throwExecutionError' (NoSuchDef fqn)

-- Todo: should we typecheck / arity check here?
createUserGuard
  :: (MonadEval b i m)
  => i
  -> Cont b i m
  -> CEKErrorHandler b i m
  -> FullyQualifiedName
  -> [PactValue]
  -> m (EvalResult b i m)
createUserGuard info cont handler fqn args =
  lookupFqName fqn >>= \case
    Just (Dfun _) ->
      returnCEKValue cont handler (VGuard (GUserGuard (UserGuard (fqnToQualName fqn) args)))
    Just _ ->
      returnCEK cont handler (VError "create-user-guard pointing to non-guard" info)
    Nothing ->
      failInvariant info "User guard pointing to no defn"


returnCEK
  :: (MonadEval b i m)
  => Cont b i m
  -> CEKErrorHandler b i m
  -> EvalResult b i m
  -> m (EvalResult b i m)
returnCEK Mt handler v =
  case handler of
    CEKNoHandler -> return v
    CEKHandler env catchTerm cont' errState handler' -> case v of
      VError{} -> do
        modifyEvalState (restoreFromErrorState errState)
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
    CEKEnforceOne env i str li cont errState h -> case v of
      VError{} -> case li of
        [] -> do
          modifyEvalState (restoreFromErrorState errState)
          let cont' = EnforceErrorC i cont
          evalCEK cont' h env str
        x:xs -> do
          modifyEvalState (restoreFromErrorState errState)
          let handler' = CEKEnforceOne env i str xs cont errState h
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
        let cont' = EnforceErrorC info cont
        evalCEK cont' handler env str
    EnforceOneFrame str li ->
      if b then returnCEKValue cont handler v
      else case li of
        x:xs -> do
          let cont' = CondC env info (EnforceOneFrame str xs) cont
              handler' = updateEnforceOneList xs handler
          evalCEK cont' handler' env x
        [] -> do
          let cont' = EnforceErrorC info cont
          evalCEK cont' handler env str
  _ ->
    failInvariant info "Evaluation of conditional expression yielded non-boolean value"
  where
  updateEnforceOneList xs (CEKEnforceOne e i str _ c cs h) =
    CEKEnforceOne e i str xs c cs h
  updateEnforceOneList _ e = e
returnCEKValue (CapInvokeC env info terms pvs cf cont) handler v = do
  pv <- enforcePactValue info v
  case terms of
    x:xs -> do
      let cont' = CapInvokeC env info xs (pv:pvs) cf cont
      evalCEK cont' handler env x
    [] -> case cf of
      WithCapFrame fqn wcbody -> do
        guardForModuleCall info env (_fqModule fqn) $ return ()
        evalCap info cont handler env (CapToken fqn (reverse (pv:pvs))) (CapBodyC PopCapInvoke) wcbody
      CreateUserGuardFrame fqn ->
        createUserGuard info cont handler fqn (reverse (pv:pvs))
returnCEKValue (CapBodyC cappop env mcap mevent capbody cont) handler _ = do
  maybe (pure ()) (emitEvent def) mevent
  case mcap of
    Nothing -> do
      let cont' = CapPopC cappop cont
      evalCEK cont' handler env capbody
    -- We're in a managed cap! We gotta do some quick stack manipulation.
    Just cap -> useEvalState (esCaps . csSlots) >>= \case
      (CapSlot _ tl:rest) -> do
        setEvalState (esCaps . csSlots)  (CapSlot cap tl:rest)
        let cont' = CapPopC PopCapInvoke cont
        evalCEK cont' handler env capbody
      [] -> failInvariant def "In CapBodyC but with no caps in stack"
returnCEKValue (CapPopC st cont) handler v = case st of
  PopCapInvoke -> do
    -- todo: need safe tail here, but this should be fine given the invariant that `CapPopC`
    -- will never show up otherwise
    esCaps . csSlots %== tail
    returnCEKValue cont handler v
  PopCapComposed -> do
    caps <- useEvalState (esCaps . csSlots)
    let cs = head caps
        csList = _csCap cs : _csComposed cs
        caps' = over (_head . csComposed) (++ csList) (tail caps)
    setEvalState (esCaps . csSlots) caps'
    returnCEKValue cont handler VUnit
returnCEKValue (ListC env info args vals cont) handler v = do
  pv <- enforcePactValue info v
  case args of
    [] ->
      returnCEKValue cont handler (VList (V.fromList (reverse (pv:vals))))
    e:es ->
      evalCEK (ListC env info es (pv:vals) cont) handler env e
returnCEKValue (ObjC env info currfield fs vs cont) handler v = do
  v' <- enforcePactValue info v
  let fields = (currfield,v'):vs
  case fs of
    (f', term):fs' ->
      let cont' = ObjC env info f' fs' fields cont
      in evalCEK cont' handler env term
    [] ->
      returnCEKValue cont handler (VObject (M.fromList (reverse fields)))
returnCEKValue (EnforceErrorC info _) handler v = case v of
  VString err -> returnCEK Mt handler (VError err info)
  _ -> failInvariant info "enforce function did not return a string"
-- Discard the value of running a user guard, no error occured, so
-- return true
returnCEKValue (UserGuardC cont) handler _v =
  returnCEKValue cont handler (VBool True)
returnCEKValue (StackPopC i mty cont) handler v = do
  v' <- (\pv -> maybeTCType i pv mty) =<< enforcePactValue i v
  -- Todo: unsafe use of tail here. need `tailMay`
  (esStack %== tail) *> returnCEKValue cont handler (VPactValue v')
returnCEKValue (DefPactStepC env cont) handler v =
  useEvalState esDefPactExec >>= \case
    Nothing -> failInvariant def "No PactExec found"
    Just pe -> case env ^. ceDefPactStep of
      Nothing -> failInvariant def "Expected a PactStep in the environment"
      Just ps -> do
        let
          pdb = view cePactDb env
          isLastStep = _psStep ps == pred (_peStepCount pe)
          done = (not (_psRollback ps) && isLastStep) || _psRollback ps
        when (nestedPactsNotAdvanced pe ps) $
          throwExecutionError def (NestedDefpactsNotAdvanced (_peDefPactId pe))
        liftDbFunction def
          (writeDefPacts pdb Write (_psDefPactId ps)
            (if done then Nothing else Just pe))

        returnCEKValue cont handler v

returnCEKValue (NestedDefPactStepC env cont parentDefPactExec) handler v =
  useEvalState esDefPactExec >>= \case
    Nothing -> failInvariant def "No DefPactExec found"
    Just pe ->  case env ^. ceDefPactStep of
      Nothing -> failInvariant def "Expected a DefPactStep in the environment"
      Just ps -> do
        when (nestedPactsNotAdvanced pe ps) $
          throwExecutionError def (NestedDefpactsNotAdvanced (_peDefPactId pe))

        let npe = parentDefPactExec & peNestedDefPactExec %~ M.insert (_psDefPactId ps) pe
        setEvalState esDefPactExec (Just npe)
        returnCEKValue cont handler v

-- | Important check for nested pacts:
--   Nested step must be equal to the parent step after execution.
nestedPactsNotAdvanced :: DefPactExec -> DefPactStep -> Bool
nestedPactsNotAdvanced resultState ps =
  any (\npe -> _peStep npe /= _psStep ps) (_peNestedDefPactExec resultState)
{-# INLINE nestedPactsNotAdvanced #-}

applyLam
  :: (MonadEval b i m)
  => CanApply b i m
  -> [CEKValue b i m]
  -> Cont b i m
  -> CEKErrorHandler b i m
  -> m (EvalResult b i m)
applyLam vc@(C (Closure fn mn ca arity term mty env cloi)) args cont handler
  | arity == argLen = case ca of
    ArgClosure cloargs -> do
      args' <- traverse (enforcePactValue cloi) args
      tcArgs <- zipWithM (\arg ty -> VPactValue <$> maybeTCType cloi arg ty) args' (NE.toList cloargs)
      esStack %== (StackFrame fn mn SFDefun :)
      let cont' = StackPopC cloi mty cont
          varEnv = RAList.fromList (reverse tcArgs)
      evalCEK cont' handler (set ceLocal varEnv env) term
    NullaryClosure -> do
      esStack %== (StackFrame fn mn SFDefun :)
      let cont' = StackPopC cloi mty cont
          varEnv = mempty
      evalCEK cont' handler (set ceLocal varEnv env) term
  | argLen > arity = throwExecutionError cloi ClosureAppliedToTooManyArgs
  | otherwise = case ca of
    NullaryClosure -> throwExecutionError cloi ClosureAppliedToTooManyArgs
    ArgClosure cloargs
      | null args ->
        returnCEKValue cont handler (VClosure vc)
      | otherwise ->
        apply' mempty (NE.toList cloargs) args
  where
  argLen = length args
  -- Here we enforce an argument to a user fn is a
  apply' e (ty:tys) (x:xs) = do
    x' <- (\pv -> maybeTCType cloi pv ty) =<< enforcePactValue cloi x
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
    x' <- (\pv -> maybeTCType cloi pv ty) =<< enforcePactValue cloi x
    apply' (RAList.cons (VPactValue x') e) tys xs
  apply' e [] [] = do
    evalCEK cont handler (set ceLocal e env) term
  apply' e (ty:tys) [] =
    returnCEKValue cont handler
    (VPartialClosure (PartialClosure Nothing (ty :| tys) (length tys + 1) term mty (set ceLocal e env) cloi))
  apply' _ [] _ = throwExecutionError cloi ClosureAppliedToTooManyArgs

applyLam (PC (PartialClosure li argtys _ term mty env cloi)) args cont handler =
  apply' (view ceLocal env) (NE.toList argtys) args
  where
  apply' e (ty:tys) (x:xs) = do
    x' <- (\pv -> maybeTCType cloi pv ty) =<< enforcePactValue cloi x
    apply' (RAList.cons (VPactValue x') e) tys xs
  apply' e [] [] = do
    case li of
      Just sf -> do
        let cont' = StackPopC cloi mty cont
        esStack %== (sf :)
        evalCEK cont' handler (set ceLocal e env) term
      Nothing -> evalCEK cont handler (set ceLocal e env) term
  apply' e (ty:tys) [] = do
    let pclo = PartialClosure li (ty :| tys) (length tys + 1) term mty (set ceLocal e env) cloi
    returnCEKValue cont handler (VPartialClosure pclo)
  apply' _ [] _ = throwExecutionError cloi ClosureAppliedToTooManyArgs

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
      args' <- traverse (enforcePactValue i) args
      tcArgs <- zipWithM (\arg ty -> maybeTCType i arg ty) args' (NE.toList cloargs)
      let pc = DefPactContinuation (fqnToQualName fqn) tcArgs
          env' = set ceLocal (RAList.fromList (reverse (VPactValue <$> tcArgs))) env
      initPact i pc cont handler env'
    NullaryClosure -> do
      let pc = DefPactContinuation (fqnToQualName fqn) []
          env' = set ceLocal mempty env
      initPact i pc cont handler env'
  | otherwise = throwExecutionError i ClosureAppliedToTooManyArgs
  where
  argLen = length args
applyLam (CT (CapTokenClosure fqn argtys arity i)) args cont handler
  | arity == argLen = do
    args' <- traverse (enforcePactValue i) args
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
