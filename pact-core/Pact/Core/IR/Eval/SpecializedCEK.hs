{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}

module Pact.Core.IR.Eval.SpecializedCEK
  ( eval
  , interpretGuard
  , safeEval
  -- , applyLam
  -- , mkDefPactClosure
  -- , resumePact
  -- , evalCap
  -- , nameToFQN
  -- , guardTable
  -- , isKeysetInSigs
  -- , isKeysetNameInSigs
  -- , requireCap
  -- , installCap
  -- , composeCap
  -- , mkDefunClosure
  -- , enforceNotWithinDefcap
  -- , acquireModuleAdmin
  -- , isCapInStack
  -- , filterIndex
  -- , findMsgSigCap
  -- , evalWithStackFrame
  -- , emitCapability
  -- , guardForModuleCall
  -- , enforceGuard
  , evalResumePact
  -- , applyContSmallStep
  -- , applyContToValueSmallStep
  -- , evaluateTermSmallStep
  ) where


-- import Control.Lens
import Control.Monad
import Data.Default
import Data.List.NonEmpty(NonEmpty(..))
import Data.Foldable(find, foldl', traverse_)
import Data.Maybe(isJust)
import qualified Data.RAList as RAList
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.List.NonEmpty as NE
import qualified Data.Kind as K

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
import Pact.Core.Namespace
import Pact.Core.DefPacts.Types

import Control.Lens hiding (from, to, op, parts)
import Control.Monad
import Control.Monad.IO.Class
import Data.Attoparsec.Text(parseOnly)
import Data.Containers.ListUtils(nubOrd)
import Data.Bits
import Data.Either(isLeft, isRight)
import Data.Foldable(foldl', traverse_, toList)
import Data.Decimal(roundTo', Decimal, DecimalRaw(..))
import Data.Vector(Vector)
import Data.Maybe(maybeToList)
import Numeric(showIntAtBase)
import qualified Control.Lens as Lens
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as V
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Char as Char
import qualified Data.ByteString as BS
import qualified GHC.Exts as Exts
import qualified Pact.Time as PactTime
import qualified Data.Poly as Poly

import Pact.Core.Builtin
import Pact.Core.Literal
import Pact.Core.Errors
import Pact.Core.Hash
import Pact.Core.Names
import Pact.Core.Guards
import Pact.Core.PactValue
import Pact.Core.Persistence
import Pact.Core.DefPacts.Types
import Pact.Core.Environment
import Pact.Core.Capabilities
import Pact.Core.Namespace
import Pact.Core.Gas
import Pact.Core.Crypto.Pairing
import Pact.Core.Type
import Pact.Core.Crypto.Hash.Poseidon

import Pact.Core.IR.Term
import Pact.Core.IR.Eval.Runtime
import Pact.Core.StableEncoding

import qualified Pact.Core.Pretty as Pretty
import qualified Pact.Core.Principal as Pr
import qualified Pact.Core.Trans.TOps as Musl


-- class CEKEval (step :: CEKStepKind) (b :: K.Type) (i :: K.Type) (m :: K.Type -> K.Type) | m -> b, m -> i where
  -- applyContToValue :: CoreCEKCont -> CoreCEKHandler -> CoreCEKValue -> Eval CoreEvalResult

  -- applyCont :: CoreCEKCont -> CoreCEKHandler -> EvalResult step b i m -> Eval CoreEvalResult

  -- evaluateTerm :: CoreCEKCont -> CoreCEKHandler -> CoreCEKEnv -> CoreTerm -> Eval CoreEvalResult

  -- returnFinal :: EvalResult step b i m -> Eval CoreEvalResult

  -- evalNormalForm :: CoreCEKEnv -> CoreTerm -> m (EvalResult step b i m)

  -- applyLamUnsafe :: CanApply step b i m -> [CoreCEKValue] -> CoreCEKCont -> CoreCEKHandler -> m (EvalResult step b i m)

  -- evalUnsafe :: CEKEvalResult step b i m -> m (EvalResult step b i m)


{-
  Our CEKH Machine's transitions when reducing terms.
  `evaluateTerm` reduces a term and either directly produces a value,
  or grows the continuation with the information about evaluation of its subterms

  Our machine's state is an n-tuple <C,E,K,H> where:
    - C: (C)ontrol, which either corresponds to:
        - A term to evaluate (CoreTerm) for some builtin set b, and tree annotation i
        - A reduced value (Closure, Table or PactValue)
    - E: (E)nvironment, which corresponds to our variable environment, the current pact db state,
      an optional defpact step (during defpact execution), our natives lookup environment, as well as
      a variable for whether we are within a defcap
    - K: (K)ontinuation, which corresponds to the current evaluation context. This may be enriched
    during term reduction
    - H: (H)andler, which holds the topmost installed error handler installed via `try`
    - The reader monad of `MonadEvalEnv` and the state within `MonadEvalState`
  Our machine corresponds to a function: <C, E, K, H> -> <C, E, K, H> that terminates when
  K=Mt and H=NoHandler and returns a semantic value, or an error
-}
evaluateTerm
  :: CoreCEKCont
  -> CoreCEKHandler
  -> CoreCEKEnv
  -> CoreTerm
  -> Eval CoreEvalResult
-- | ------ From ------ | ------ To ------ |
--   <Var n, E, K, H>      <E(n), E, K, H>
--
-- Handles free variable lookups as well as module reference dynamic invokes
evaluateTerm cont handler env (Var n info)  = do
  case _nKind n of
    NBound i -> do
      chargeGasArgs info (GAConstant unconsWorkNodeGas)
      case RAList.lookup (view ceLocal env) i of
        Just v -> applyContToValue cont handler v
        Nothing -> failInvariant info ("unbound identifier" <> T.pack (show n))
    -- Top level names are not closures, so we wipe the env
    NTopLevel mname mh -> do
      let fqn = FullyQualifiedName mname (_nName n) mh
      lookupFqName fqn >>= \case
        Just (Dfun d) -> do
          dfunClo <- VDefClosure <$> mkDefunClosure d mname env
          applyContToValue cont handler dfunClo
        -- Todo: this should be GADT'd out
        -- and defconsts should already be evaluated
        Just (DConst d) -> case _dcTerm d of
          -- Todo: should this be an error?
          -- probably.
          TermConst _term ->
            failInvariant info "Defconst not fully evaluated"
          EvaledConst v ->
            applyContToValue cont handler (VPactValue v)
        Just (DPact d) -> do
          let dpactClo = mkDefPactClosure info fqn d env
          applyContToValue cont handler dpactClo
        Just (DTable d) ->
          let (ResolvedTable sc) = _dtSchema d
              tn = TableName (_dtName d) mname
              tbl = VTable (TableValue tn mh sc)
          in applyContToValue cont handler tbl
        Just (DCap d) -> do
          let args = _argType <$> _dcapArgs d
              clo = CapTokenClosure fqn args (length args) info
          applyContToValue cont handler (VClosure (CT clo))
        Just d ->
          throwExecutionError info (InvalidDefKind (defKind d) "in var position")
        Nothing ->
          throwExecutionError info (NameNotInScope (FullyQualifiedName mname (_nName n) mh))
    NModRef m ifs -> case ifs of
      [x] -> applyContToValue cont handler (VModRef (ModRef m ifs (Just (S.singleton x))))
      [] -> throwExecutionError info (ModRefNotRefined (_nName n))
      _ -> applyContToValue cont handler (VModRef (ModRef m ifs Nothing))
    NDynRef (DynamicRef dArg i) -> case RAList.lookup (view ceLocal env) i of
      Just (VModRef mr) -> do
        modRefHash <- _mHash <$> getModule info (view cePactDb env) (_mrModule mr)
        let nk = NTopLevel (_mrModule mr) modRefHash
        evaluateTerm cont handler env (Var (Name dArg nk) info)
      Just _ -> applyCont cont handler (VError "dynamic name pointed to non-modref" info)
      Nothing -> failInvariant info ("unbound identifier" <> T.pack (show n))
-- | ------ From ------ | ------ To ------ |
--   <Const l, E, K, H>    <Value l, E, K, H>
--
evaluateTerm cont handler _env (Constant l info) = do
  applyContToValue cont handler (VLiteral l)
-- | ------ From ---------- | ------ To ------ |
--   <App fn args, E, K, H>    <fn, E, Args(E,args,K), H>
--
evaluateTerm cont handler env (App fn args info) = do
  evaluateTerm (Args env info args cont) handler env fn
-- | ------ From ---------- | ------ To ------ |
--   <Nullary body, E, K, H>    <VClosure(body, E), E, K, H>
--
evaluateTerm cont handler env (Nullary body info) = do
  chargeGasArgs info (GAConstant constantWorkNodeGas)
  let clo = VLamClosure (LamClosure NullaryClosure 0 body Nothing env info)
  applyContToValue cont handler clo
-- | ------ From ---------- | ------ To ------ |
--   <Let e1 e2, E, K, H>      <e1, E, LetC(E,e2,K), H>
--
evaluateTerm cont handler env (Let _ e1 e2 info) = do
  chargeGasArgs info (GAConstant constantWorkNodeGas)
  let cont' = LetC env e2 cont
  evaluateTerm cont' handler env e1
-- | ------ From ---------- | ------ To ------ |
--   <Lam args body, E, K, H>      <VLamClo(args, body, E), E, K, H>
--
evaluateTerm cont handler env (Lam args body info) = do
  chargeGasArgs info (GAConstant constantWorkNodeGas)
  let clo = VLamClosure (LamClosure (ArgClosure args) (NE.length args) body Nothing env info)
  applyContToValue cont handler clo
-- | ------ From ------ | ------ To ------ |
--   <Builtin b, E, K, H>    <E(b), E, K, H>
--
evaluateTerm cont handler env (Builtin b info) = do
  chargeGasArgs info (GAConstant constantWorkNodeGas)
  -- let builtins = view ceBuiltins env
  applyContToValue cont handler (VNative (rawBuiltinEnv info b env))
-- | ------ From ------ | ------ To ----------------- |
--   <Seq e1 e2, E, K, H>    <e1, E, SeqC(E, e2, K), H>
--
evaluateTerm cont handler env (Sequence e1 e2 info) = do
  chargeGasArgs info (GAConstant constantWorkNodeGas)
  evaluateTerm (SeqC env e2 cont) handler env e1
-- | ------ From --------------- | ------ To ------------------------ |
--   <CAnd e1 e2, E, K, H>         <e1, E, CondC(E, AndFrame(e2),K),H>
--   <COr e1 e2, E, K, H>          <e1, E, CondC(E, OrFrame(e2),K),H>
--   <CIf cond ifc elc, E, K, H>   <cond, E, CondC(E, IfFrame(ifc,elc),K), H>
--  Todo: enforce and enforce-one
evaluateTerm cont handler env (Conditional c info) = case c of
  CAnd te te' -> do
    chargeGasArgs info (GAConstant constantWorkNodeGas)
    evaluateTerm (CondC env info (AndC te') cont) handler env te
  COr te te' -> do

    evaluateTerm (CondC env info (OrC te') cont) handler env te
  CIf cond e1 e2 -> do
    chargeGasArgs info (GAConstant constantWorkNodeGas)
    evaluateTerm (CondC env info (IfC e1 e2) cont) handler env cond
  CEnforce cond str -> do
    let env' = sysOnlyEnv env
    chargeGasArgs info (GAConstant constantWorkNodeGas)
    evaluateTerm (CondC env' info (EnforceC str) cont) handler env' cond
  CEnforceOne str conds -> do
    chargeGasArgs info (GAConstant constantWorkNodeGas)
    case conds of
      [] ->
        applyCont cont handler (VError "enforce-one failure" info)
      x:xs -> do
        -- Todo: is this a bit too cheap??
        errState <- evalStateToErrorState <$> getEvalState
        let env' = readOnlyEnv env
        let handler' = CEKEnforceOne env' info str xs cont errState handler
        let cont' = CondC env' info (EnforceOneC str xs) Mt
        evaluateTerm cont' handler' env' x

evaluateTerm cont handler env (CapabilityForm cf info) =
  case cf of
    WithCapability rawCap body -> do
      chargeGasArgs info (GAConstant constantWorkNodeGas)
      enforceNotWithinDefcap info env "with-capability"
      let capFrame = WithCapC body
          cont' = CapInvokeC env info capFrame cont
      evaluateTerm cont' handler env rawCap
    CreateUserGuard name args -> do
      fqn <- nameToFQN info env name
      case args of
        [] -> createUserGuard info cont handler fqn []
        x : xs -> do
          let usrGuardFrame = CreateUserGuardC fqn xs []
          let cont' = CapInvokeC env info usrGuardFrame cont
          evaluateTerm cont' handler env x
evaluateTerm cont handler env (ListLit ts info) = do
  chargeGasArgs info (GAConstant unconsWorkNodeGas)
  case ts of
    [] -> applyContToValue cont handler (VList mempty)
    x:xs -> evaluateTerm (ListC env info xs [] cont) handler env x
evaluateTerm cont handler env (Try catchExpr rest info) = do
  chargeGasArgs info (GAConstant tryNodeGas)
  errState <- evalStateToErrorState <$> getEvalState
  let handler' = CEKHandler env catchExpr cont errState handler
  let env' = readOnlyEnv env
  evaluateTerm Mt handler' env' rest
evaluateTerm cont handler env (ObjectLit o info) = do
  chargeGasArgs info (GAConstant unconsWorkNodeGas)
  case o of
    (f, term):rest -> do
      let cont' = ObjC env info f rest [] cont
      evaluateTerm cont' handler env term
    [] -> applyContToValue cont handler (VObject mempty)
-- Error terms ignore the current cont
evaluateTerm _ handler _ (Error e info) = do
  chargeGasArgs info (GAConstant constantWorkNodeGas)
  applyCont Mt handler (VError e info)

mkDefunClosure
  :: EvalDefun RawBuiltin ()
  -> ModuleName
  -> CoreCEKEnv
  -> Eval (Closure CEKBigStep RawBuiltin () Eval)
mkDefunClosure d mn e = case _dfunTerm d of
  Lam args body i ->
    pure (Closure (_dfunName d) mn (ArgClosure args) (NE.length args) body (_dfunRType d) e i)
  Nullary body i ->
    pure (Closure (_dfunName d) mn NullaryClosure 0 body (_dfunRType d) e i)
  _ ->
    throwExecutionError (_dfunInfo d) (DefIsNotClosure (_dfunName d))

mkDefPactClosure
  :: ()
  -> FullyQualifiedName
  -> DefPact Name Type b i
  -> CoreCEKEnv
  -> CoreCEKValue
mkDefPactClosure info fqn dpact env = case _dpArgs dpact of
  [] ->
    let dpc = DefPactClosure fqn NullaryClosure 0 env info
    in VDefPactClosure dpc
  (x:xs) ->
    let dpc = DefPactClosure fqn (ArgClosure (x :| xs)) (length (x:xs)) env info
    in VDefPactClosure dpc

initPact
  :: ()
  -> DefPactContinuation QualifiedName PactValue
  -> CoreCEKCont
  -> CoreCEKHandler
  -> CoreCEKEnv
  -> Eval CoreEvalResult
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
  :: ()
  -> DefPactContinuation QualifiedName PactValue
  -> DefPactStep
  -> CoreCEKCont
  -> CoreCEKHandler
  -> CoreCEKEnv
  -> M.Map DefPactId DefPactExec
  -> Eval CoreEvalResult
applyPact i pc ps cont handler cenv nested = useEvalState esDefPactExec >>= \case
  Just pe -> throwExecutionError i (MultipleOrNestedDefPactExecFound pe)
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

emitXChainEvents
  :: Maybe Yield
  -- ^ from '_psResume', indicating a cross-chain resume.
  -> DefPactExec
   -- ^ tested for yield provenance to indicate a cross-chain yield.
  -> Eval ()
emitXChainEvents mResume dpe = do
  forM_ mResume $ \r -> case r of
    (Yield _ (Just (Provenance _ mh)) (Just sc)) ->
      emitXEvent "X_RESUME" sc mh
    _ -> return ()
  forM_ (_peYield dpe) $ \y -> case y of
    (Yield _ (Just (Provenance tc mh)) _) ->
      emitXEvent "X_YIELD" tc mh
    _ -> return ()
  where
    emitXEvent eName (ChainId cid) mh = emitReservedEvent eName
      [ PString cid
      , PString (renderQualName (view (peContinuation . pcName) dpe))
      , PList (V.fromList (view (peContinuation . pcArgs) dpe)) ]
      mh

applyNestedPact
  :: ()
  -> DefPactContinuation QualifiedName PactValue
  -> DefPactStep
  -> CoreCEKCont
  -> CoreCEKHandler
  -> CoreCEKEnv
  -> Eval CoreEvalResult
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
        throwExecutionError i (NestedDefPactParentStepCountMissmatch (_peDefPactId pe) stepCount (_peStepCount pe))

      when (isRollback /= _peStepHasRollback pe) $
        throwExecutionError i (NestedDefPactParentRollbackMissmatch (_peDefPactId pe) isRollback (_peStepHasRollback pe))

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
  :: ()
  -> CoreCEKCont
  -> CoreCEKHandler
  -> CoreCEKEnv
  -> Maybe DefPactExec
  -> Eval CoreEvalResult
resumePact i cont handler env crossChainContinuation = viewEvalEnv eeDefPactStep >>= \case
  Nothing -> throwExecutionError i DefPactStepNotInEnvironment
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
        --resumeDefPactExec ::  DefPactExec -> Eval CoreEvalResult
        resumeDefPactExec pe = do
          when (_psDefPactId ps /= _peDefPactId pe) $
            throwExecutionError i (DefPactIdMissmatch (_psDefPactId ps) (_peDefPactId pe))

          when (_psStep ps < 0 || _psStep ps >= _peStepCount pe) $
            throwExecutionError i (InvalidDefPactStepSupplied ps pe)

          if _psRollback ps
            then when (_psStep ps /= _peStep pe) $
                 throwExecutionError i (DefPactRollbackMissmatch ps pe)
            else when (_psStep ps /= succ (_peStep pe)) $
                 throwExecutionError i (DefPactStepMissmatch ps pe)

          let pc = view peContinuation pe
              args = VPactValue <$> _pcArgs pc
              resume = case _psResume ps of
                         r@Just{} -> r
                         Nothing -> _peYield pe
              env' = set ceLocal (RAList.fromList (reverse args)) $ set ceDefPactStep (Just $ set psResume resume ps) env
          applyPact i pc ps cont handler env' (_peNestedDefPactExec pe)

-- Todo: fail invariant
-- Todo: is this enough checks for ndynref?
nameToFQN
  :: ()
  -> CoreCEKEnv
  -> Name
  -> Eval FullyQualifiedName
nameToFQN info env (Name n nk) = case nk of
  NTopLevel mn mh -> pure (FullyQualifiedName mn n mh)
  NDynRef (DynamicRef dArg i) -> case RAList.lookup (view ceLocal env) i of
    Just (VModRef mr) -> do
      md <- getModule info (view cePactDb env) (_mrModule mr)
      pure (FullyQualifiedName (_mrModule mr) dArg (_mHash md))
    Just _ -> throwExecutionError info (DynNameIsNotModRef dArg)
    Nothing -> failInvariant info ("unbound identifier" <> T.pack (show n))
  _ -> failInvariant info ("invalid name in fq position" <> T.pack (show n))

guardTable
  :: ()
  -> CoreCEKCont
  -> CoreCEKHandler
  -> CoreCEKEnv
  -> TableValue
  -> GuardTableOp
  -> Eval CoreEvalResult
guardTable i cont handler env (TableValue tn mh _) dbop = do
  let mn = _tableModuleName tn
  checkLocalBypass $
    guardForModuleCall i cont handler env mn $ do
      mdl <- getModule i (view cePactDb env) mn
      enforceBlessedHashes i mdl mh
      applyContToValue cont handler VUnit
  where
  checkLocalBypass notBypassed = do
    enabled <- isExecutionFlagSet FlagAllowReadInLocal
    case dbop of
      GtWrite -> notBypassed
      GtCreateTable -> notBypassed
      _ | enabled -> applyContToValue cont handler VUnit
        | otherwise -> notBypassed


enforceBlessedHashes :: () -> EvalModule b i -> ModuleHash -> Eval ()
enforceBlessedHashes info md mh
  | _mHash md == mh = return ()
  | mh `S.member` _mBlessed md = return ()
  | otherwise = throwExecutionError info (HashNotBlessed (_mName md) mh)

guardForModuleCall
  :: ()
  -> CoreCEKCont
  -> CoreCEKHandler
  -> CoreCEKEnv
  -> ModuleName
  -> Eval CoreEvalResult
  -> Eval CoreEvalResult
guardForModuleCall i cont handler env currMod onFound =
  findCallingModule >>= \case
    Just mn | mn == currMod -> onFound
    _ -> do
      mc <- useEvalState (esCaps . csModuleAdmin)
      if S.member currMod mc then onFound
      else getModule i (view cePactDb env) currMod >>= acquireModuleAdmin i cont handler env

acquireModuleAdmin
  :: ()
  -> CoreCEKCont
  -> CoreCEKHandler
  -> CoreCEKEnv
  -> EvalModule b i
  -> Eval CoreEvalResult
acquireModuleAdmin i cont handler env mdl = do
  -- mc <- useEvalState (esCaps . csModuleAdmin)
  -- if S.member (_mName mdl) mc then applyContToValue cont handler VUnit
  -- else case _mGovernance mdl of
  case _mGovernance mdl of
    KeyGov ksn -> do
      enforceKeysetNameAdmin i (_mName mdl) ksn
      esCaps . csModuleAdmin %== S.insert (_mName mdl)
      applyContToValue cont handler VUnit
    CapGov (ResolvedGov fqn) -> do
      let wcapBody = Constant LUnit i
      let cont' = ModuleAdminC (_mName mdl) cont
      evalCap i cont' handler env (CapToken fqn []) (CapBodyC PopCapInvoke) wcapBody

evalWithStackFrame
  :: ()
  -> CoreCEKCont
  -> CoreCEKHandler
  -> CoreCEKEnv
  -> StackFrame
  -> Maybe Type
  -> CoreTerm
  -> Eval CoreEvalResult
evalWithStackFrame info cont handler env sf mty body = do
  cont' <- pushStackFrame info cont mty sf
  evaluateTerm cont' handler env body

pushStackFrame
  :: ()
  -> CoreCEKCont
  -> Maybe Type
  -> StackFrame
  -> Eval CoreCEKCont
pushStackFrame info cont mty sf = do
  esStack %== (sf :)
  pure (StackPopC info mty cont)

type ModCapCont step b i m
  = CoreCEKEnv
  -> Maybe (CapToken QualifiedName PactValue)
  -> Maybe (PactEvent PactValue)
  -> CoreTerm
  -> CoreCEKCont
  -> CoreCEKCont

-- | Our main workhorse for "Evaluate a capability, then do something else"
-- `evalCap` handles
--   - with-capability
--   - test-capability
--   - compose-capability
-- In all cases, evalCap checks if the capability is in scope
-- if it is, it is a no-op and does simply evaluates `contbody` with the current continuation
--
-- If it is not inscope, it pushes the `contbody` into a new continuation that will
-- evaluate it after cap aquisition, with the cap in scope.
-- Then:
--   - If the cap is unmanaged, simply evaluate the cap body with the new continuation
--   - If the cap is @event, set the event to emit after cap body evaluation, then eval the cap body
--   - If the cap is managed, install the cap (If possible) then evaluate the body, and if
--     the cap is user managed, ensure that the manager function run after the cap body
evalCap
  :: ()
  -> CoreCEKCont
  -> CoreCEKHandler
  -> CoreCEKEnv
  -> FQCapToken
  -> ModCapCont step b i m
  -> CoreTerm
  -> Eval CoreEvalResult
evalCap info currCont handler env origToken@(CapToken fqn args) modCont contbody = do
  capInStack <- isCapInStack' origToken
  if not capInStack then go else evaluateTerm currCont handler env contbody
  where
  go = do
    d <- getDefCap info fqn
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
                case find (== qualCapToken) msgCaps of
                  Just c -> do
                    let c' = set ctName fqn c
                    installCap info env c' False >>= evalAutomanagedCap cont' newLocals capBody
                  Nothing ->
                    throwExecutionError info (CapNotInstalled fqn)
              Just managedCap ->
                evalAutomanagedCap cont' newLocals capBody managedCap
      DefEvent -> do
        let cont' = modCont env Nothing (Just (fqctToPactEvent origToken)) contbody currCont
        let inCapEnv = set ceInCap True $ set ceLocal newLocals env
        (esCaps . csSlots) %== (CapSlot qualCapToken []:)
        sfCont <- pushStackFrame info cont' Nothing capStackFrame
        -- emitCapability info origToken
        evaluateTerm sfCont handler inCapEnv capBody
        -- evalWithStackFrame info cont' handler (set ceLocal newLocals env) capStackFrame Nothing capBody
      -- Not automanaged _nor_ user managed.
      -- Todo: a type that's basically `Maybe` here would save us a lot of grief.
      Unmanaged -> do
        let cont' = modCont env Nothing Nothing contbody currCont
            inCapEnv = set ceInCap True $ set ceLocal newLocals env
        (esCaps . csSlots) %== (CapSlot qualCapToken []:)
        evalWithStackFrame info cont' handler inCapEnv capStackFrame Nothing capBody
  qualCapName = fqnToQualName fqn
  qualCapToken = CapToken qualCapName args
  capStackFrame = StackFrame (_fqName fqn) (_fqModule fqn) SFDefcap
  -- This function is handles both evaluating the manager function for the installed parameter
  -- and continuing evaluation for the actual capability body.
  evalUserManagedCap cont' env' capBody managedCap =  case _mcManaged managedCap of
    ManagedParam mpfqn oldV managedIx -> do
      dfun <- getDefun info mpfqn
      dfunClo <- mkDefunClosure dfun (_fqModule mpfqn) env
      newV <- maybe (failInvariant info "Managed param does not exist at index") pure (args ^? ix managedIx)
      -- Set the mgr fun to evaluate after we apply the capability body
      let mgrFunCont = CapInvokeC env info (ApplyMgrFunC managedCap dfunClo oldV newV) cont'
      let inCapEnv = set ceInCap True $ set ceLocal env' $ env
      let inCapBodyToken = _mcOriginalCap managedCap
      -- BIG SEMANTICS NOTE HERE
      -- the cap slot here that we push should NOT be the qualified original token.
      -- Instead, it's the original token from the installed from the static cap. Otherwise, enforce checks
      -- within the cap body will fail (That is, keyset enforcement). Instead, once we are evaluating the body,
      -- we pop the current cap stack, then replace the head with the original intended token.
      -- this is done in `CapBodyC` and this is the only way to do this.
      (esCaps . csSlots) %== (CapSlot inCapBodyToken []:)
      sfCont <- pushStackFrame info mgrFunCont Nothing capStackFrame
      evaluateTerm sfCont handler inCapEnv capBody
    _ -> failInvariant info "Invalid managed cap type"
  evalAutomanagedCap cont' env' capBody managedCap = case _mcManaged managedCap of
    AutoManaged b -> do
      if b then applyCont currCont handler (VError "Automanaged capability used more than once" info)
      else do
        let newManaged = AutoManaged True
        esCaps . csManaged %== S.union (S.singleton (set mcManaged newManaged managedCap))
        esCaps . csSlots %== (CapSlot qualCapToken []:)
        let inCapEnv = set ceLocal env' $ set ceInCap True $ env
        sfCont <- pushStackFrame info cont' Nothing capStackFrame
        evaluateTerm sfCont handler inCapEnv capBody
    _ -> failInvariant info "Invalid managed cap type"


emitEvent
  :: ()
  -> PactEvent PactValue
  -> Eval ()
emitEvent info pe = findCallingModule >>= \case
    Just mn -> do
      let ctModule = _peModule pe
      if ctModule == mn then do
        esEvents %== (++ [pe])
      else throwExecutionError info (EventDoesNotMatchModule mn)
    Nothing -> failInvariant info "emit-event called outside of module code"

emitEventUnsafe
  :: PactEvent PactValue
  -> Eval ()
emitEventUnsafe pe = esEvents %== (++ [pe])

emitReservedEvent :: T.Text -> [PactValue] -> ModuleHash -> Eval ()
emitReservedEvent name params mhash = do
  let pactModule = ModuleName "pact" Nothing
  let pe = PactEvent name params pactModule mhash
  emitEventUnsafe pe

emitCapability
  :: ()
  -> CapToken FullyQualifiedName PactValue
  -> Eval ()
emitCapability info tkn =
  emitEvent info (fqctToPactEvent tkn)

fqctToPactEvent :: CapToken FullyQualifiedName PactValue -> PactEvent PactValue
fqctToPactEvent (CapToken fqn args) = PactEvent (_fqName fqn) args (_fqModule fqn) (_fqHash fqn)

enforceNotWithinDefcap
  :: ()
  -> CoreCEKEnv
  -> T.Text
  -> Eval ()
enforceNotWithinDefcap info env form =
  when (_ceInCap env) $ throwExecutionError info (FormIllegalWithinDefcap form)

requireCap
  :: ()
  -> CoreCEKCont
  -> CoreCEKHandler
  -> FQCapToken
  -> Eval CoreEvalResult
requireCap info cont handler (CapToken fqn args) = do
  capInStack <- isCapInStack (CapToken (fqnToQualName fqn) args)
  if capInStack then applyContToValue cont handler (VBool True)
  else applyCont cont handler $
    VError ("cap not in scope " <> renderQualName (fqnToQualName fqn)) info

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
{-# SPECIALIZE isCapInStack'
   :: FQCapToken
   -> Eval Bool
    #-}

composeCap
  :: ()
  -> CoreCEKCont
  -> CoreCEKHandler
  -> CoreCEKEnv
  -> FQCapToken
  -> Eval CoreEvalResult
composeCap info cont handler env origToken =
  isCapInStack' origToken >>= \case
    False ->
      evalCap info cont handler env origToken (CapBodyC PopCapComposed) (Constant (LBool True) info)
    True ->
      applyContToValue cont handler (VBool True)

filterIndex :: Int -> [a] -> [a]
filterIndex i xs = [x | (x, i') <- zip xs [0..], i /= i']

findMsgSigCap :: Int -> CapToken QualifiedName PactValue -> CapToken QualifiedName PactValue -> Bool
findMsgSigCap cix ct1 ct2 =
  _ctName ct1 == _ctName ct2 && (_ctArgs ct1 == filterIndex cix (_ctArgs ct2))

-- Todo:
-- `capAutonomous` are what we should use to match semantics accurately.
installCap :: ()
  -> CoreCEKEnv
  -> FQCapToken
  -> Bool
  -> Eval (ManagedCap QualifiedName PactValue)
installCap info _env (CapToken fqn args) autonomous = do
  let ct = CapToken (fqnToQualName fqn) args
  d <- getDefCap info fqn
  case _dcapMeta d of
    DefManaged m -> case m of
      DefManagedMeta (paramIx,_) (FQName fqnMgr) -> do
        managedParam <- maybe (throwExecutionError info (InvalidManagedCap fqn)) pure (args ^? ix paramIx)
        let mcapType = ManagedParam fqnMgr managedParam paramIx
            ctFiltered = CapToken (fqnToQualName fqn) (filterIndex paramIx args)
            mcap = ManagedCap ctFiltered ct mcapType
        capAlreadyInstalled <- S.member mcap <$> useEvalState (esCaps . csManaged)
        when capAlreadyInstalled $ throwExecutionError info (CapAlreadyInstalled fqn)
        (esCaps . csManaged) %== S.insert mcap
        when autonomous $
          (esCaps . csAutonomous) %== S.insert ct
        pure mcap
      AutoManagedMeta -> do
        let mcapType = AutoManaged False
            mcap = ManagedCap ct ct mcapType
        capAlreadyInstalled <- S.member mcap <$> useEvalState (esCaps . csManaged)
        when capAlreadyInstalled $ throwExecutionError info (CapAlreadyInstalled fqn)
        (esCaps . csManaged) %== S.insert mcap
        when autonomous $
          (esCaps . csAutonomous) %== S.insert ct
        pure mcap
    DefEvent ->
      throwExecutionError info (InvalidManagedCap fqn)
    Unmanaged -> throwExecutionError info (InvalidManagedCap fqn)


-- Todo: should we typecheck / arity check here?
createUserGuard
  :: ()
  -> CoreCEKCont
  -> CoreCEKHandler
  -> FullyQualifiedName
  -> [PactValue]
  -> Eval CoreEvalResult
createUserGuard info cont handler fqn args =
  lookupFqName fqn >>= \case
    Just (Dfun _) ->
      applyContToValue cont handler (VGuard (GUserGuard (UserGuard (fqnToQualName fqn) args)))
    Just _ ->
      applyCont cont handler (VError "create-user-guard pointing to non-guard" info)
    Nothing ->
      failInvariant info "User guard pointing to no defn"


applyCont
  :: CoreCEKCont
  -> CoreCEKHandler
  -> CoreEvalResult
  -> Eval CoreEvalResult
applyCont Mt handler v =
  case handler of
    CEKNoHandler -> return v
    CEKHandler env catchTerm cont' errState handler' -> case v of
      VError{} -> do
        modifyEvalState (restoreFromErrorState errState)
        evaluateTerm cont' handler' env catchTerm
      EvalValue v' ->
        applyContToValue cont' handler' v'
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
          evaluateTerm cont' h env str
        x:xs -> do
          modifyEvalState (restoreFromErrorState errState)
          let handler' = CEKEnforceOne env i str xs cont errState h
              oldFrame = CondC env i (EnforceOneC str xs) Mt
          evaluateTerm oldFrame handler' env x
      EvalValue v' ->
        applyContToValue cont h v'
applyCont cont handler v = case v of
  VError{} -> applyCont Mt handler v
  EvalValue v' -> applyContToValue cont handler v'

-- | if true then 1 else 2
applyContToValue
  :: CoreCEKCont
  -> CoreCEKHandler
  -> CoreCEKValue
  -> Eval CoreEvalResult
applyContToValue Mt handler v =
  case handler of
    CEKNoHandler -> return (EvalValue v)
    -- Assuming no error, the caps will have been popped naturally
    CEKHandler _env _term cont' _ handler' ->
      applyContToValue cont' handler' v
    CEKEnforceOne _ _ _ _ cont' _ handler' ->
      applyContToValue cont' handler' v
-- Error terms that don't simply returnt the empty continuation
-- "Zero out" the continuation up to the latest handler
-- applyContToValue _cont handler v@VError{} =
--   applyCont Mt handler v
-- | ------ From ------------------------- | ------------ To -------------- |
--   <VClosure c, Args(E, (x:xs), K), H>     <x, E, Fn(c, E, xs, K), H>
--
applyContToValue (Args env i args cont) handler fn = do
  c <- canApply fn
  -- Argument evaluation
  case args of
    [] -> applyLam c [] cont handler
    (x:xs) -> do
      let cont' = Fn c env xs [] cont
      evaluateTerm cont' handler env x
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
    -- Todo: this is _not_ an invariant failure. Requires a better error
    _ -> failInvariant i "Cannot apply non-function to arguments"
-- | ------ From ------------------------- | ------ To ----------------------- |
--   <v, _, Fn(clo, E, (x:xs), acc, K), H>   <x, E, Fn(c, E, xs, (v:acc), K), H>
--   <v, _, Fn(clo, E, [], K), H>            (apply clo (reverse (v:acc)) K H)
--
applyContToValue (Fn fn env args vs cont) handler v = do
  case args of
    [] -> do
      applyLam fn (reverse (v:vs)) cont handler
    x:xs ->
      evaluateTerm (Fn fn env xs (v:vs) cont) handler env x
-- | ------ From ------------ | ------ To ---------------- |
--   <v, LetC(E, body, K), H>   <body, (cons v E), K, H>
--
applyContToValue (LetC env letbody cont) handler v = do
  evaluateTerm cont handler (over ceLocal (RAList.cons v) env) letbody
-- | ------ From ------------ | ------ To ---------------- |
--   <_, SeqC(E, e2, K), H>     <e2, E, K, H>
--
applyContToValue (SeqC env e cont) handler _ =
  evaluateTerm cont handler env e
-- | ------ From ------------------------ | ------ To ---------------- |
--   <VBool b, CondC(E, AndC(e2), K), H>   if b then <e2, E, EnforceBool(K), H>
--                                         else <VBool b, K, H>
--   <VBool b, CondC(E, OrC(e2), K), H>    if b then <VBool b, K, H>
--                                         else <e2, E, EnforceBool(K), H>
--
-- | ------ From ------------------------------ | ------ To ---------------- |
--   <VBool b, CondC(E, IfC(ifE, elseE), K), H>   if b then <ifE, E, K, H>
--                                                else <VBool b, K, H>
--
applyContToValue (CondC env info frame cont) handler v = case v of
  (VLiteral (LBool b)) -> case frame of
    AndC te ->
      if b then evaluateTerm (EnforceBoolC info cont) handler env te
      else applyContToValue cont handler v
    OrC te ->
      if b then applyContToValue cont handler v
      else evaluateTerm (EnforceBoolC info cont) handler env te
    IfC ifExpr elseExpr ->
      if b then evaluateTerm cont handler env ifExpr
      else evaluateTerm cont handler env elseExpr
    EnforceC str ->
      if b then applyContToValue cont handler v
      else do
        let cont' = EnforceErrorC info cont
        evaluateTerm cont' handler env str
    FilterC clo elem' rest acc -> do
      let acc' = if b then elem':acc else acc
      case rest of
        x:xs -> do
          let cont' = CondC env info (FilterC clo x xs acc') cont
          applyLam clo [VPactValue x] cont' handler
        [] -> applyContToValue cont handler (VList (V.fromList (reverse acc')))
    EnforceOneC str li ->
      if b then applyContToValue cont handler v
      else case li of
        x:xs -> do
          let cont' = CondC env info (EnforceOneC str xs) cont
              handler' = updateEnforceOneList xs handler
          evaluateTerm cont' handler' env x
        [] -> do
          let cont' = EnforceErrorC info cont
          evaluateTerm cont' handler env str
    AndQC clo pv ->
      if b then applyLam clo [VPactValue pv] (EnforceBoolC info cont) handler
      else applyContToValue cont handler v
    OrQC clo pv ->
      if not b then applyLam clo [VPactValue pv] (EnforceBoolC info cont) handler
      else applyContToValue cont handler v
    NotQC -> applyContToValue cont handler (VBool (not b))
  _ ->
    applyCont cont handler (VError "Evaluation of conditional expression yielded non-boolean value" info)
  where
  updateEnforceOneList xs (CEKEnforceOne e i str _ c cs h) =
    CEKEnforceOne e i str xs c cs h
  updateEnforceOneList _ e = e
applyContToValue currCont@(CapInvokeC env info cf cont) handler v = case cf of
  WithCapC body -> case v of
    VCapToken ct@(CapToken fqn _) -> do
      -- Todo: CEK-style this
      let cont' = IgnoreValueC (PCapToken ct) currCont
      guardForModuleCall info cont' handler env (_fqModule fqn) $
        evalCap info cont handler env ct (CapBodyC PopCapInvoke) body
    -- Todo: this is actually more like "expected cap token"
    _ -> throwExecutionError info ExpectedPactValue
  CreateUserGuardC fqn terms pvs -> do
    pv <- enforcePactValue info v
    case terms of
      x:xs -> do
        let cf' = CreateUserGuardC fqn xs (pv:pvs)
            cont' = CapInvokeC env info cf' cont
        evaluateTerm cont' handler env x
      [] -> createUserGuard info cont handler fqn (reverse (pv:pvs))
  ApplyMgrFunC mgdCap clo old new -> do
    -- Set the manager fun to update the current managed cap.
    let cont' = CapInvokeC env info (UpdateMgrFunC mgdCap) cont
    applyLam (C clo) [VPactValue old, VPactValue new] cont' handler
  UpdateMgrFunC mcap -> case v of
    VPactValue v' -> do
      let mcap' = unsafeUpdateManagedParam v' mcap
      (esCaps . csManaged) %== S.insert mcap'
      applyContToValue cont handler v
    _ -> applyCont cont handler (VError "Manager function for managed cap did not return a value" info)
applyContToValue (BuiltinC env info frame cont) handler cv = do
  let pdb = _cePactDb env
  case cv of
    VPactValue v -> case frame of
      MapC closure rest acc -> case rest of
        x:xs ->
          let cont' = BuiltinC env info (MapC closure xs (v:acc)) cont
          in applyLam closure [VPactValue x] cont' handler
        [] ->
          applyContToValue cont handler (VList (V.fromList (reverse (v:acc))))
      FoldC clo rest -> case rest of
        x:xs ->
          let cont' = BuiltinC env info (FoldC clo xs) cont
          in applyLam clo [VPactValue v, VPactValue x] cont' handler
        [] -> applyContToValue cont handler cv
      ZipC clo (l, r) acc -> case (l, r) of
        (x:xs, y:ys) ->
          let cont' = BuiltinC env info (ZipC clo (xs, ys) (v:acc)) cont
          in applyLam clo [VPactValue x, VPactValue y] cont' handler
        (_, _) ->
          applyContToValue cont handler (VList (V.fromList (reverse (v:acc))))
      PreSelectC tv clo mf -> do
        keys <- liftDbFunction info (_pdbKeys pdb (tvToDomain tv))
        selectRead tv clo keys [] mf
      SelectC tv clo rdata remaining acc mf -> case v of
        PBool b -> do
          let acc' = if b then rdata:acc else acc
          selectRead tv clo remaining acc' mf
        _ -> applyCont cont handler (VError "select query did not return a boolean " info)
      ReadC tv rowkey -> do
        liftDbFunction info (_pdbRead pdb (tvToDomain tv) rowkey) >>= \case
          Just (RowData rdata) ->
            applyContToValue cont handler (VObject rdata)
          Nothing -> applyCont cont handler (VError "no such read object" info)
      WithReadC tv rowkey clo -> do
        liftDbFunction info (_pdbRead pdb (tvToDomain tv) rowkey) >>= \case
          Just (RowData rdata) ->
            applyLam clo [VObject rdata] cont handler
          Nothing -> applyCont cont handler (VError "no such read object" info)
      WithDefaultReadC tv rowkey (ObjectData defaultObj) clo -> do
        liftDbFunction info (_pdbRead pdb (tvToDomain tv) rowkey) >>= \case
          Just (RowData rdata) ->
            applyLam clo [VObject rdata] cont handler
          Nothing -> applyLam clo [VObject defaultObj] cont handler
      KeysC tv -> do
        ks <- liftDbFunction info (_pdbKeys pdb (tvToDomain tv))
        let li = V.fromList (PString . _rowKey <$> ks)
        applyContToValue cont handler (VList li)
      WriteC tv wt rk (ObjectData rv) -> do
        let check' = if wt == Update then checkPartialSchema else checkSchema
        if check' rv (_tvSchema tv) then do
          let rdata = RowData rv
          liftDbFunction info (_pdbWrite pdb wt (tvToDomain tv) rk rdata)
          applyContToValue cont handler (VString "Write succeeded")
        else applyCont cont handler (VError "object does not match schema" info)
      PreFoldDbC tv queryClo appClo -> do
        let tblDomain = DUserTables (_tvName tv)
        -- Todo: keys gas
        keys <- liftDbFunction info (_pdbKeys pdb tblDomain)
        foldDBRead tv queryClo appClo keys []
      TxIdsC tv tid -> do
        ks <- liftDbFunction info (_pdbTxIds pdb (_tvName tv) (TxId (fromIntegral tid)))
        let li = V.fromList (PInteger . fromIntegral . _txId <$> ks)
        applyContToValue cont handler (VList li)
      KeyLogC tv (RowKey key) tid -> do
        let txId = TxId (fromInteger tid)
        ids <- liftDbFunction info (_pdbTxIds pdb (_tvName tv) txId)
        ks <- concat <$> traverse (\t -> fmap (t,) <$> liftDbFunction info (_pdbGetTxLog pdb (_tvName tv) t)) ids
        let ks' = filter (\(_, txl) -> _txKey txl == key) ks
        let li = V.fromList (txLogToObj <$> ks')
        applyContToValue cont handler (VList li)
        where
        txLogToObj (TxId txid, TxLog _domain _key (RowData rdata)) = do
          PObject $ M.fromList
            [ (Field "txid", PInteger (fromIntegral txid))
            , (Field "value", PObject rdata)]
      FoldDbFilterC tv queryClo appClo (rk, ObjectData om) remaining accum -> case v of
        PBool b -> do
          let accum' = if b then (rk, PObject om):accum else accum
          foldDBRead tv queryClo appClo remaining accum'
        _ -> applyCont cont handler (VError "fold-db error: query returned non-boolean value" info)
      FoldDbMapC tv appClo remaining acc -> case remaining of
        (RowKey rk, pv):xs -> do
          let rdf = FoldDbMapC tv appClo xs (v:acc)
              cont' = BuiltinC env info rdf cont
          applyLam appClo [VString rk, VPactValue pv] cont' handler
        [] -> applyContToValue cont handler (VList (V.fromList (v:acc)))
      TxLogC tv tid -> do
        let txId = TxId (fromInteger tid)
        ks <- liftDbFunction info (_pdbGetTxLog pdb (_tvName tv) txId)
        let li = V.fromList (txLogToObj <$> ks)
        applyContToValue cont handler (VList li)
        where
        txLogToObj (TxLog domain key (RowData rdata)) = do
          PObject $ M.fromList
            [ (Field "table", PString domain)
            , (Field "key", PString key)
            , (Field "value", PObject rdata)]
      CreateTableC (TableValue tn _ _) -> do
        liftDbFunction info (_pdbCreateUserTable pdb tn)
        applyContToValue cont handler (VString "TableCreated")
      EmitEventC ct@(CapToken fqn _) ->
        lookupFqName (_ctName ct) >>= \case
          Just (DCap d) -> do
            enforceMeta (_dcapMeta d)
            emitCapability info ct
            applyContToValue cont handler (VBool True)
          Just _ ->
            failInvariant info "CapToken does not point to defcap"
          _ -> failInvariant info "No Capability found in emit-event"
        where
        enforceMeta Unmanaged = throwExecutionError info (InvalidEventCap fqn)
        enforceMeta _ = pure ()
      DefineKeysetC ksn newKs -> do
        liftDbFunction info (writeKeySet pdb Write ksn newKs)
        applyContToValue cont handler (VString "Keyset write success")
      DefineNamespaceC ns -> case v of
        PBool allow ->
          if allow then do
            let nsn = _nsName ns
            liftDbFunction info (_pdbWrite pdb Write DNamespaces nsn ns)
            applyContToValue cont handler $ VString $ "Namespace defined: " <> (_namespaceName nsn)
          else throwExecutionError info $ DefineNamespaceError "Namespace definition not permitted"
        _ ->
          throwExecutionError info $ DefineNamespaceError "Namespace manager function returned an invalid value"
      where
      foldDBRead tv queryClo appClo remaining acc =
        case remaining of
          rk@(RowKey raw):remaining' -> liftDbFunction info (_pdbRead pdb (tvToDomain tv) rk) >>= \case
            Just (RowData row) -> do
              let rdf = FoldDbFilterC tv queryClo appClo (rk, ObjectData row) remaining' acc
                  cont' = BuiltinC env info rdf cont
              applyLam queryClo [VString raw, VObject row] cont' handler
            Nothing ->
              failInvariant info "foldDB read a key that is not in the database"
          [] -> case acc of
            (RowKey rk, pv):xs -> do
              let rdf = FoldDbMapC tv appClo xs []
                  cont' = BuiltinC env info rdf cont
              applyLam appClo [VString rk, VPactValue pv] cont' handler
            [] -> applyContToValue cont handler (VList mempty)
      selectRead tv clo keys acc mf = case keys of
        k:ks -> liftDbFunction info (_pdbRead pdb (tvToDomain tv) k) >>= \case
          Just (RowData r) -> do
            let bf = SelectC tv clo (ObjectData r) ks acc mf
                cont' = BuiltinC env info bf cont
            applyLam clo [VObject r] cont' handler
          Nothing ->
            failInvariant info "Select keys returned a key that did not exist"
        [] -> case mf of
          Just fields ->
            let acc' = PObject . (`M.restrictKeys` S.fromList fields) . _objectData <$> reverse acc
            in applyContToValue cont handler (VList (V.fromList acc'))
          Nothing ->
            let acc' = PObject . _objectData <$> reverse acc
            in applyContToValue cont handler (VList (V.fromList acc'))
    _ -> applyCont cont handler (VError "higher order apply did not return a pactvalue" info)
applyContToValue (CapBodyC cappop env mcap mevent capbody cont) handler _ = do
  -- Todo: I think this requires some administrative check?
  maybe (pure ()) (emitEvent def) mevent
  case mcap of
    Nothing -> do
      let cont' = CapPopC cappop cont
      evaluateTerm cont' handler env capbody
    -- We're in a managed cap! We gotta do some quick stack manipulation.
    Just cap -> useEvalState (esCaps . csSlots) >>= \case
      (CapSlot _ tl:rest) -> do
        setEvalState (esCaps . csSlots)  (CapSlot cap tl:rest)
        let cont' = CapPopC PopCapInvoke cont
        evaluateTerm cont' handler env capbody
      [] -> failInvariant def "In CapBodyC but with no caps in stack"

applyContToValue (CapPopC st cont) handler v = case st of
  PopCapInvoke -> do
    esCaps . csSlots %== safeTail
    applyContToValue cont handler v
  PopCapComposed -> do
    useEvalState (esCaps . csSlots) >>= \case
      cap:cs -> do
        let csList = _csCap cap : _csComposed cap
            caps' = over (_head . csComposed) (++ csList) cs
        setEvalState (esCaps . csSlots) caps'
        applyContToValue cont handler VUnit
      [] -> failInvariant def "PopCapComposed present outside of cap eval"

applyContToValue (ListC env info args vals cont) handler v = do
  pv <- enforcePactValue def v
  case args of
    [] ->
      applyContToValue cont handler (VList (V.fromList (reverse (pv:vals))))
    e:es ->
      evaluateTerm (ListC env info es (pv:vals) cont) handler env e

applyContToValue (ObjC env info currfield fs vs cont) handler v = do
  v' <- enforcePactValue def v
  let fields = (currfield,v'):vs
  case fs of
    (f', term):fs' ->
      let cont' = ObjC env info f' fs' fields cont
      in evaluateTerm cont' handler env term
    [] ->
      applyContToValue cont handler (VObject (M.fromList (reverse fields)))

applyContToValue (EnforceErrorC info _) handler v = case v of
  VString err -> applyCont Mt handler (VError err info)
  _ -> failInvariant info "enforce function did not return a string"
-- Discard the value of running a user guard, no error occured, so
applyContToValue (IgnoreValueC v cont) handler _v =
  applyContToValue cont handler (VPactValue v)

applyContToValue (StackPopC i mty cont) handler v = do
  v' <- (\pv -> maybeTCType i pv mty) =<< enforcePactValue i v
  -- Todo: this seems like an invariant failure, so maybe safeTail is not what we want?
  -- Testing will determine whether this is observable.
  (esStack %== safeTail) *> applyContToValue cont handler (VPactValue v')
applyContToValue (DefPactStepC env cont) handler v =
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
        emitXChainEvents (_psResume ps) pe
        applyContToValue cont handler v

applyContToValue (NestedDefPactStepC env cont parentDefPactExec) handler v =
  useEvalState esDefPactExec >>= \case
    Nothing -> failInvariant def "No DefPactExec found"
    Just pe ->  case env ^. ceDefPactStep of
      Nothing -> failInvariant def "Expected a DefPactStep in the environment"
      Just ps -> do
        when (nestedPactsNotAdvanced pe ps) $
          throwExecutionError def (NestedDefpactsNotAdvanced (_peDefPactId pe))
        let npe = parentDefPactExec & peNestedDefPactExec %~ M.insert (_psDefPactId ps) pe
        setEvalState esDefPactExec (Just npe)
        applyContToValue cont handler v

applyContToValue (EnforcePactValueC info cont) handler v = case v of
  VPactValue{} -> applyContToValue cont handler v
  _ -> applyCont cont handler (VError "function expected to return pact value" info)

applyContToValue (EnforceBoolC info cont) handler v = case v of
  VBool{} -> applyContToValue cont handler v
  _ -> applyCont cont handler (VError "function expected to return boolean" info)

applyContToValue (ModuleAdminC mn cont) handler v = do
  (esCaps . csModuleAdmin) %== S.insert mn
  applyContToValue cont handler v

-- applyContToValue (EvalCapC env info captoken withCapBody cont) handler _ =
--   evalCap info cont handler env captoken (CapBodyC PopCapInvoke) withCapBody


-- | Important check for nested pacts:
--   Nested step must be equal to the parent step after execution.
nestedPactsNotAdvanced :: DefPactExec -> DefPactStep -> Bool
nestedPactsNotAdvanced resultState ps =
  any (\npe -> _peStep npe /= _psStep ps) (_peNestedDefPactExec resultState)
{-# INLINE nestedPactsNotAdvanced #-}

applyLam
  :: CanApply CEKBigStep RawBuiltin () Eval
  -> [CoreCEKValue]
  -> CoreCEKCont
  -> CoreCEKHandler
  -> Eval CoreEvalResult
applyLam vc@(C (Closure fn mn ca arity term mty env cloi)) args cont handler
  | arity == argLen = case ca of
    ArgClosure cloargs -> do
      args' <- traverse (enforcePactValue cloi) args
      tcArgs <- zipWithM (\arg (Arg _ ty) -> VPactValue <$> maybeTCType cloi arg ty) args' (NE.toList cloargs)
      esStack %== (StackFrame fn mn SFDefun :)
      let cont' = StackPopC cloi mty cont
          varEnv = RAList.fromList (reverse tcArgs)
      evaluateTerm cont' handler (set ceLocal varEnv env) term
    NullaryClosure -> do
      esStack %== (StackFrame fn mn SFDefun :)
      let cont' = StackPopC cloi mty cont
          varEnv = mempty
      evaluateTerm cont' handler (set ceLocal varEnv env) term
  | argLen > arity = throwExecutionError cloi ClosureAppliedToTooManyArgs
  | otherwise = case ca of
    NullaryClosure -> throwExecutionError cloi ClosureAppliedToTooManyArgs
    ArgClosure cloargs
      | null args ->
        applyContToValue cont handler (VClosure vc)
      | otherwise ->
        apply' mempty (NE.toList cloargs) args
  where
  argLen = length args
  -- Here we enforce an argument to a user fn is a
  apply' e (Arg _ ty:tys) (x:xs) = do
    x' <- (\pv -> maybeTCType cloi pv ty) =<< enforcePactValue cloi x
    apply' (RAList.cons (VPactValue x') e) tys xs
  apply' e (ty:tys) [] = do
    let env' = set ceLocal e env
        pclo = PartialClosure (Just (StackFrame fn mn SFDefun)) (ty :| tys) (length tys + 1) term mty env' cloi
    applyContToValue cont handler (VPartialClosure pclo)
  apply' _ [] _ = throwExecutionError cloi ClosureAppliedToTooManyArgs

applyLam (LC (LamClosure ca arity term mty env cloi)) args cont handler
  | arity == argLen = case ca of
    ArgClosure _ -> do
      let locals = view ceLocal env
          locals' = foldl' (flip RAList.cons) locals args
          cont' = EnforcePactValueC cloi cont
      evaluateTerm cont' handler (set ceLocal locals' env) term
    NullaryClosure -> do
      let cont' = EnforcePactValueC cloi cont
      evaluateTerm cont' handler env term
  | argLen > arity = throwExecutionError cloi ClosureAppliedToTooManyArgs
  | otherwise = case ca of
      NullaryClosure -> throwExecutionError cloi ClosureAppliedToTooManyArgs
      ArgClosure cloargs ->
        apply' (view ceLocal env) (NE.toList cloargs) args
  where
  argLen = length args
  -- Todo: runtime TC here
  apply' e (Arg _ ty:tys) (x:xs) = do
    x' <- (\pv -> maybeTCType cloi pv ty) =<< enforcePactValue cloi x
    apply' (RAList.cons (VPactValue x') e) tys xs
  apply' e [] [] = do
    evaluateTerm cont handler (set ceLocal e env) term
  apply' e (ty:tys) [] =
    applyContToValue cont handler
    (VPartialClosure (PartialClosure Nothing (ty :| tys) (length tys + 1) term mty (set ceLocal e env) cloi))
  apply' _ [] _ = throwExecutionError cloi ClosureAppliedToTooManyArgs

applyLam (PC (PartialClosure li argtys _ term mty env cloi)) args cont handler =
  apply' (view ceLocal env) (NE.toList argtys) args
  where
  apply' e (Arg _ ty:tys) (x:xs) = do
    x' <- (\pv -> maybeTCType cloi pv ty) =<< enforcePactValue cloi x
    apply' (RAList.cons (VPactValue x') e) tys xs
  apply' e [] [] = do
    case li of
      Just sf -> do
        let cont' = StackPopC cloi mty cont
        esStack %== (sf :)
        evaluateTerm cont' handler (set ceLocal e env) term
      Nothing -> do
        let cont' = EnforcePactValueC cloi cont
        evaluateTerm cont' handler (set ceLocal e env) term
  apply' e (ty:tys) [] = do
    let pclo = PartialClosure li (ty :| tys) (length tys + 1) term mty (set ceLocal e env) cloi
    applyContToValue cont handler (VPartialClosure pclo)
  apply' _ [] _ = throwExecutionError cloi ClosureAppliedToTooManyArgs

applyLam nclo@(N (NativeFn b env fn arity i)) args cont handler
  | arity == argLen = do
    chargeFlatNativeGas i b
    fn i b cont handler env args
  | argLen > arity = throwExecutionError i ClosureAppliedToTooManyArgs
  | null args = applyContToValue cont handler (VClosure nclo)
  | otherwise = apply' arity [] args
  where
  argLen = length args
  apply' !a pa (x:xs) = apply' (a - 1) (x:pa) xs
  apply' !a pa [] =
    applyContToValue cont handler (VPartialNative (PartialNativeFn b env fn a pa i))

applyLam (PN (PartialNativeFn b env fn arity pArgs i)) args cont handler
  | arity == argLen = do
    chargeFlatNativeGas i b
    fn i b cont handler env (reverse pArgs ++ args)
  | argLen > arity = throwExecutionError i ClosureAppliedToTooManyArgs
  | otherwise = apply' arity [] args
  where
  argLen = length args
  apply' !a pa (x:xs) = apply' (a - 1) (x:pa) xs
  apply' !a pa [] =
    applyContToValue cont handler (VPartialNative (PartialNativeFn b env fn a pa i))

applyLam (DPC (DefPactClosure fqn argtys arity env i)) args cont handler
  | arity == argLen = case argtys of
    ArgClosure cloargs -> do
      args' <- traverse (enforcePactValue i) args
      tcArgs <- zipWithM (\arg (Arg _ ty) -> maybeTCType i arg ty) args' (NE.toList cloargs)
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
    applyContToValue cont handler (VPactValue (PCapToken (CapToken fqn tcArgs)))
  | otherwise = throwExecutionError i ClosureAppliedToTooManyArgs
  where
  argLen = length args

checkSchema :: M.Map Field PactValue -> Schema -> Bool
checkSchema o (Schema sc) = isJust $ do
  let keys = M.keys o
  when (keys /= M.keys sc) Nothing
  traverse_ go (M.toList o)
  where
  go (k, v) = M.lookup k sc >>= (`checkPvType` v)

checkPartialSchema :: M.Map Field PactValue -> Schema -> Bool
checkPartialSchema o (Schema sc) =
  M.isSubmapOfBy (\obj ty -> isJust (checkPvType ty obj)) o sc

-- instance MonadEval b i m => CEKEval CEKSmallStep b i m where
--   applyContToValue cont handler v = pure (CEKReturn cont handler (EvalValue v))
--   applyCont cont handler v = pure (CEKReturn cont handler v)
--   evaluateTerm cont handler env term = pure (CEKEvaluateTerm cont handler env term)
--   returnFinal v = pure (CEKReturn Mt CEKNoHandler v)
--   applyLamUnsafe ca vs lc lh = applyLam ca vs lc lh >>= evalUnsafe

--   evalNormalForm initialEnv initialTerm = evalUnsafe (CEKEvaluateTerm Mt CEKNoHandler initialEnv initialTerm)
--   evalUnsafe (CEKReturn Mt CEKNoHandler result) =
--     return result
--   evalUnsafe (CEKReturn cont handler (EvalValue v)) =
--     applyContToValue cont handler v >>= evalUnsafe
--   evalUnsafe (CEKReturn cont handler result) =
--     applyCont cont handler result >>= evalUnsafe
--   evalUnsafe (CEKEvaluateTerm cont handler env term) =
--     evaluateTerm cont handler env term >>= evalUnsafe


-- instance MonadEval b i m => CEKEval CEKBigStep b i m where
--   applyContToValue = applyContToValue
--   {-# INLINE applyContToValue #-}
--   applyCont = applyCont
--   {-# INLINE applyCont #-}
--   evaluateTerm = evaluateTerm
--   {-# INLINE evaluateTerm #-}
--   returnFinal = return
--   {-# INLINE returnFinal #-}
--   applyLamUnsafe = applyLam
--   {-# INLINE applyLamUnsafe #-}

--   evalNormalForm = evaluateTerm Mt CEKNoHandler
--   {-# INLINE evalNormalForm #-}

--   evalUnsafe = pure

-- | The main logic of enforcing a guard.
--
-- The main difference to `coreEnforceGuard` is this function's type doesn't need to be a `NativeFunction step b i m`,
-- thus there's no need to wrap/unwrap the guard into a `VPactValue`,
-- and moreover it does not need to take a `b` which it does not use anyway.
enforceGuard
  :: ()
  -> CoreCEKCont
  -> CoreCEKHandler
  -> CoreCEKEnv
  -> Guard QualifiedName PactValue
  -> Eval CoreEvalResult
enforceGuard info cont handler env g = case g of
  GKeyset ks -> do
    cond <- isKeysetInSigs ks
    if cond then applyContToValue cont handler (VBool True)
    else applyCont cont handler (VError "enforce keyset failure" info)
  GKeySetRef ksn -> do
    cond <- isKeysetNameInSigs info (view cePactDb env) ksn
    if cond then applyContToValue cont handler (VBool True)
    else applyCont cont handler (VError "enforce keyset ref failure" info)
  GUserGuard ug -> runUserGuard info cont handler env ug
  GCapabilityGuard cg -> enforceCapGuard info cont handler cg
  GModuleGuard (ModuleGuard mn _) -> calledByModule mn >>= \case
    True -> applyContToValue cont handler (VBool True)
    False -> do
      md <- getModule info (view cePactDb env) mn
      let cont' = IgnoreValueC (PBool True) cont
      acquireModuleAdmin info cont' handler env md
      -- applyContToValue cont handler (VBool True)guard
  GDefPactGuard (DefPactGuard dpid _) -> do
    curDpid <- getDefPactId info
    if curDpid == dpid
       then applyContToValue cont handler (VBool True)
       else applyCont cont handler (VError "Capability pact guard failed: invalid pact id" info)


enforceCapGuard
  :: ()
  -> CoreCEKCont
  -> CoreCEKHandler
  -> CapabilityGuard QualifiedName PactValue
  -> Eval CoreEvalResult
enforceCapGuard info cont handler (CapabilityGuard qn args mpid) = case mpid of
  Nothing -> enforceCap
  Just pid -> do
    currPid <- getDefPactId info
    if currPid == pid then enforceCap
    else applyCont cont handler (VError "Capability pact guard failed: invalid pact id" info)
  where
  enforceCap = do
    cond <- isCapInStack (CapToken qn args)
    if cond then applyContToValue cont handler (VBool True)
    else do
      let errMsg = "Capability guard enforce failure cap not in scope: " <> renderQualName qn
      applyCont cont handler (VError errMsg info)


runUserGuard
  :: ()
  -> CoreCEKCont
  -> CoreCEKHandler
  -> CoreCEKEnv
  -> UserGuard QualifiedName PactValue
  -> Eval CoreEvalResult
runUserGuard info cont handler env (UserGuard qn args) =
  getModuleMember info (_cePactDb env) qn >>= \case
    Dfun d -> do
      when (length (_dfunArgs d) /= length args) $ throwExecutionError info CannotApplyPartialClosure
      let env' = sysOnlyEnv env
      clo <- mkDefunClosure d (_qnModName qn) env'
      -- Todo: sys only here
      applyLam (C clo) (VPactValue <$> args) (IgnoreValueC (PBool True) cont) handler
    d -> throwExecutionError info (InvalidDefKind (defKind d) "run-user-guard")

eval
  :: Purity
  -> CoreTerm
  -> Eval PactValue
eval purity term = do
  ee <- readEnv
  let cekEnv = envFromPurity purity (CEKEnv mempty (_eePactDb ee) rawBuiltinEnv (_eeDefPactStep ee) False)
  evaluateTerm Mt CEKNoHandler cekEnv term >>= \case
    VError txt i ->
      throwExecutionError i (EvalError txt)
    EvalValue v -> do
      case v of
        VPactValue pv -> pure pv
        _ ->
          throwExecutionError (view termInfo term) (EvalError "Evaluation did not reduce to a value")


safeEval :: PactDb RawBuiltin () -> Maybe DefPactStep -> CoreTerm -> Eval CoreEvalResult
safeEval pdb step term =
  let env = CEKEnv mempty pdb rawBuiltinEnv step False
  in evaluateTerm Mt CEKNoHandler env term


interpretGuard
  :: ()
  -> Guard QualifiedName PactValue
  -> Eval PactValue
interpretGuard info g = do
  ee <- readEnv
  let cekEnv = CEKEnv mempty (_eePactDb ee) rawBuiltinEnv (_eeDefPactStep ee) False
  enforceGuard info Mt CEKNoHandler cekEnv g >>= \case
    VError txt errInfo ->
      throwExecutionError errInfo (EvalError txt)
    EvalValue v -> do
      case v of
        VPactValue pv -> pure pv
        _ ->
          throwExecutionError info (EvalError "Evaluation did not reduce to a value")


evalResumePact
  :: ()
  -> Maybe DefPactExec
  -> Eval PactValue
evalResumePact info mdpe = do
  ee <- readEnv
  let pdb = _eePactDb ee
  let env = CEKEnv mempty pdb rawBuiltinEnv (_eeDefPactStep ee) False
  resumePact info Mt CEKNoHandler env mdpe >>= \case
    VError txt i ->
      throwExecutionError i (EvalError txt)
    EvalValue v -> do
      case v of
        VPactValue pv -> pure pv
        _ ->
          throwExecutionError info (EvalError "Evaluation did not reduce to a value")

-- evaluateTermSmallStep
--   :: Cont CEKSmallStep RawBuiltin () Eval
--   -> CEKErrorHandler CEKSmallStep RawBuiltin () Eval
--   -> CEKEnv CEKSmallStep RawBuiltin () Eval
--   -> CoreTerm
--   -> Eval (CEKReturn RawBuiltin () Eval)
-- evaluateTermSmallStep = evaluateTerm


-- applyContToValueSmallStep
--   :: Cont CEKSmallStep RawBuiltin () Eval
--   -> CEKErrorHandler CEKSmallStep RawBuiltin () Eval
--   -> CEKValue CEKSmallStep RawBuiltin () Eval
--   -> Eval (CEKReturn RawBuiltin () Eval)
-- applyContToValueSmallStep = applyContToValue


-- applyContSmallStep
--   :: Cont CEKSmallStep RawBuiltin () Eval
--   -> CEKErrorHandler CEKSmallStep RawBuiltin () Eval
--   -> EvalResult CEKSmallStep RawBuiltin () Eval
--   -> Eval (CEKReturn RawBuiltin () Eval)
-- applyContSmallStep = applyCont

--------------------------
-- Gas-related code
--------------------------
constantWorkNodeGas :: MilliGas
constantWorkNodeGas = (MilliGas 50)

unconsWorkNodeGas :: MilliGas
unconsWorkNodeGas = (MilliGas 100)

tryNodeGas :: MilliGas
tryNodeGas = (MilliGas 100)

-- nthAccessGas n
--  = GALinear n (LinearGasArg ())

-- nthAccessSlope
--   =

----------------------------------------------------------------------
-- Our builtin definitions start here
----------------------------------------------------------------------

-- -- Todo: runtime error
unaryIntFn :: (Integer -> Integer) -> CoreNativeFunction
unaryIntFn op info b cont handler _env = \case
  [VLiteral (LInteger i)] ->
    applyContToValue cont handler (VLiteral (LInteger (op i)))
  args -> argsError info b args
{-# INLINE unaryIntFn #-}

binaryIntFn
  :: (Integer -> Integer -> Integer)
  -> CoreNativeFunction
binaryIntFn op info b cont handler _env = \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> applyContToValue cont handler (VLiteral (LInteger (op i i')))
  args -> argsError info b args
{-# INLINE binaryIntFn #-}

roundingFn ::(Rational -> Integer) -> CoreNativeFunction
roundingFn op info b cont handler _env = \case
  [VLiteral (LDecimal d)] ->
    applyContToValue cont handler (VLiteral (LInteger (truncate (roundTo' op 0 d))))
  [VDecimal d, VInteger prec] ->
    applyContToValue cont handler (VLiteral (LDecimal (roundTo' op (fromIntegral prec) d)))
  args -> argsError info b args
{-# INLINE roundingFn #-}

---------------------------------
-- Arithmetic Ops
------------------------------
rawAdd :: CoreNativeFunction
rawAdd info b cont handler _env = \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> applyContToValue cont handler (VLiteral (LInteger (i + i')))
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> applyContToValue cont handler (VLiteral (LDecimal (i + i')))
  [VLiteral (LString i), VLiteral (LString i')] ->
    applyContToValue cont handler  (VLiteral (LString (i <> i')))
  [VObject l, VObject r] ->
    let o' = VObject (l `M.union` r)
    in applyContToValue cont handler o'
  [VList l, VList r] -> applyContToValue cont handler (VList (l <> r))
  args -> argsError info b args

rawSub :: CoreNativeFunction
rawSub info b cont handler _env = \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> applyContToValue cont handler (VLiteral (LInteger (i - i')))
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> applyContToValue cont handler (VLiteral (LDecimal (i - i')))
  args -> argsError info b args

rawMul :: CoreNativeFunction
rawMul info b cont handler _env = \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> applyContToValue cont handler (VLiteral (LInteger (i * i')))
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> applyContToValue cont handler (VLiteral (LDecimal (i * i')))
  args -> argsError info b args

rawPow :: CoreNativeFunction
rawPow info b cont handler _env = \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> do
    when (i' < 0) $ throwExecutionError info (ArithmeticException "negative exponent in integer power")
    -- Todo: move to iterated pow
    applyContToValue cont handler (VLiteral (LInteger (i ^ i')))
  [VLiteral (LDecimal l), VLiteral (LDecimal r)] -> do
    let result = Musl.trans_pow (dec2F l) (dec2F r)
    guardNanOrInf info result
    applyContToValue cont handler (VLiteral (LDecimal (f2Dec result)))
  args -> argsError info b args

rawLogBase :: CoreNativeFunction
rawLogBase info b cont handler _env = \case
  [VLiteral (LInteger base), VLiteral (LInteger n)] -> do
    when (base < 0 || n <= 0) $ throwExecutionError info (ArithmeticException "Illegal log base")
    let base' = fromIntegral base :: Double
        n' = fromIntegral n
        result = Musl.trans_logBase base' n'
    guardNanOrInf info result
    applyContToValue cont handler (VLiteral (LInteger (round result)))
    -- if i' == 0 then throwExecutionError' (ArithmeticException "div by zero")
    -- else applyContToValue cont handler (VLiteral (LInteger (div i i')))
  [VLiteral (LDecimal base), VLiteral (LDecimal arg)] -> do
    when (base < 0 || arg <= 0) $ throwExecutionError info (ArithmeticException "Invalid base or argument in log")
    let result = Musl.trans_logBase (dec2F base) (dec2F arg)
    guardNanOrInf info result
    applyContToValue cont handler (VLiteral (LDecimal (f2Dec result)))
  args -> argsError info b args

rawDiv :: CoreNativeFunction
rawDiv info b cont handler _env = \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] ->
    if i' == 0 then throwExecutionError info (ArithmeticException "div by zero")
    else applyContToValue cont handler (VLiteral (LInteger (div i i')))
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] ->
    if i' == 0 then throwExecutionError info (ArithmeticException "div by zero, decimal")
    else applyContToValue cont handler (VLiteral (LDecimal (i / i')))
  args -> argsError info b args

rawNegate :: CoreNativeFunction
rawNegate info b cont handler _env = \case
  [VLiteral (LInteger i)] ->
    applyContToValue cont handler (VLiteral (LInteger (negate i)))
  [VLiteral (LDecimal i)] ->
    applyContToValue cont handler (VLiteral (LDecimal (negate i)))
  args -> argsError info b args

rawEq :: CoreNativeFunction
rawEq info b cont handler _env = \case
  -- Todo: rawEqGas
  [VPactValue pv, VPactValue pv'] -> applyContToValue cont handler (VBool (pv == pv'))
  args -> argsError info b args

modInt :: CoreNativeFunction
modInt = binaryIntFn mod

rawNeq :: CoreNativeFunction
rawNeq info b cont handler _env = \case
  [VPactValue pv, VPactValue pv'] ->
    applyContToValue cont handler (VBool (pv /= pv'))
  args -> argsError info b args

rawGt :: CoreNativeFunction
rawGt info b cont handler _env = \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> applyContToValue cont handler (VLiteral (LBool (i > i')))
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> applyContToValue cont handler (VLiteral (LBool (i > i')))
  [VLiteral (LString i), VLiteral (LString i')] -> applyContToValue cont handler (VLiteral (LBool (i > i')))
  [VTime i, VTime i'] -> applyContToValue cont handler (VLiteral (LBool (i > i')))
  args -> argsError info b args

rawLt :: CoreNativeFunction
rawLt info b cont handler _env = \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> applyContToValue cont handler (VLiteral (LBool (i < i')))
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> applyContToValue cont handler (VLiteral (LBool (i < i')))
  [VLiteral (LString i), VLiteral (LString i')] -> applyContToValue cont handler (VLiteral (LBool (i < i')))
  [VTime i, VTime i'] -> applyContToValue cont handler (VLiteral (LBool (i < i')))
  args -> argsError info b args

rawGeq :: CoreNativeFunction
rawGeq info b cont handler _env = \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> applyContToValue cont handler (VLiteral (LBool (i >= i')))
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> applyContToValue cont handler (VLiteral (LBool (i >= i')))
  [VLiteral (LString i), VLiteral (LString i')] -> applyContToValue cont handler (VLiteral (LBool (i >= i')))
  [VTime i, VTime i'] -> applyContToValue cont handler (VLiteral (LBool (i >= i')))
  args -> argsError info b args

rawLeq :: CoreNativeFunction
rawLeq info b cont handler _env = \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> applyContToValue cont handler (VLiteral (LBool (i <= i')))
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> applyContToValue cont handler (VLiteral (LBool (i <= i')))
  [VLiteral (LString i), VLiteral (LString i')] -> applyContToValue cont handler (VLiteral (LBool (i <= i')))
  [VTime i, VTime i'] -> applyContToValue cont handler (VLiteral (LBool (i <= i')))
  args -> argsError info b args

bitAndInt :: CoreNativeFunction
bitAndInt = binaryIntFn (.&.)

bitOrInt :: CoreNativeFunction
bitOrInt = binaryIntFn (.|.)

bitComplementInt :: CoreNativeFunction
bitComplementInt = unaryIntFn complement

bitXorInt :: CoreNativeFunction
bitXorInt = binaryIntFn xor

bitShiftInt :: CoreNativeFunction
bitShiftInt =  binaryIntFn (\i s -> shift i (fromIntegral s))

rawAbs :: CoreNativeFunction
rawAbs info b cont handler _env = \case
  [VLiteral (LInteger i)] ->
    applyContToValue cont handler (VLiteral (LInteger (abs i)))
  [VLiteral (LDecimal e)] -> do
    applyContToValue cont handler (VLiteral (LDecimal (abs e)))
  args -> argsError info b args

rawExp :: CoreNativeFunction
rawExp info b cont handler _env = \case
  [VLiteral (LInteger i)] -> do
    let result = Musl.trans_exp (fromIntegral i)
    guardNanOrInf info result
    applyContToValue cont handler (VLiteral (LDecimal (f2Dec result)))
  [VLiteral (LDecimal e)] -> do
    let result = Musl.trans_exp (dec2F e)
    guardNanOrInf info result
    applyContToValue cont handler (VLiteral (LDecimal (f2Dec result)))
  args -> argsError info b args

rawLn :: CoreNativeFunction
rawLn info b cont handler _env = \case
  [VLiteral (LInteger i)] -> do
    let result = Musl.trans_ln (fromIntegral i)
    guardNanOrInf info result
    applyContToValue cont handler (VLiteral (LDecimal (f2Dec result)))
  [VLiteral (LDecimal e)] -> do
    let result = Musl.trans_ln (dec2F e)
    guardNanOrInf info result
    applyContToValue cont handler (VLiteral (LDecimal (f2Dec result)))
  args -> argsError info b args

rawSqrt :: CoreNativeFunction
rawSqrt info b cont handler _env = \case
  [VLiteral (LInteger i)] -> do
    when (i < 0) $ throwExecutionError info (ArithmeticException "Square root must be non-negative")
    let result = Musl.trans_sqrt (fromIntegral i)
    guardNanOrInf info result
    applyContToValue cont handler (VLiteral (LDecimal (f2Dec result)))
  [VLiteral (LDecimal e)] -> do
    when (e < 0) $ throwExecutionError info (ArithmeticException "Square root must be non-negative")
    let result = Musl.trans_sqrt (dec2F e)
    guardNanOrInf info result
    applyContToValue cont handler (VLiteral (LDecimal (f2Dec result)))
  args -> argsError info b args

-- Todo: fix all show instances
rawShow :: CoreNativeFunction
rawShow info b cont handler _env = \case
  [VLiteral (LInteger i)] ->
    applyContToValue cont handler (VLiteral (LString (T.pack (show i))))
  [VLiteral (LDecimal i)] ->
    applyContToValue cont handler (VLiteral (LString (T.pack (show i))))
  [VLiteral (LString i)] ->
    applyContToValue cont handler (VLiteral (LString (T.pack (show i))))
  [VLiteral (LBool i)] ->
    applyContToValue cont handler (VLiteral (LString (T.pack (show i))))
  [VLiteral LUnit] ->
    applyContToValue cont handler (VLiteral (LString "()"))
  args -> argsError info b args

-- Todo: Gas here is complicated, greg worked on this previously
rawContains :: CoreNativeFunction
rawContains info b cont handler _env = \case
  [VString f, VObject o] ->
    applyContToValue cont handler (VBool (M.member (Field f) o))
  [VString s, VString s'] ->
    applyContToValue cont handler (VBool (s `T.isInfixOf` s'))
  [VPactValue v, VList vli] ->
    applyContToValue cont handler (VBool (v `V.elem` vli))
  args -> argsError info b args

rawSort :: CoreNativeFunction
rawSort info b cont handler _env = \case
  [VList vli]
    | V.null vli -> applyContToValue cont handler (VList mempty)
    | otherwise -> do
    vli' <- liftIO $ do
      v' <- V.thaw vli
      V.sort v'
      V.freeze v'
    applyContToValue cont handler (VList vli')
  args -> argsError info b args

coreRemove :: CoreNativeFunction
coreRemove info b cont handler _env = \case
  [VString s, VObject o] -> applyContToValue cont handler (VObject (M.delete (Field s) o))
  args -> argsError info b args

asObject
  :: ()
  -> RawBuiltin
  -> PactValue
  -> Eval (M.Map Field PactValue)
asObject info b = \case
  PObject o -> pure o
  arg -> argsError info b [VPactValue arg]

rawSortObject :: CoreNativeFunction
rawSortObject info b cont handler _env = \case
  [VList fields, VList objs]
    | V.null fields -> applyContToValue cont handler (VList objs)
    | V.null objs -> applyContToValue cont handler (VList objs)
    | otherwise -> do
        objs' <- traverse (asObject info b) objs
        fields' <- traverse (fmap Field . asString info b) fields
        v' <- liftIO $ do
          mobjs <- V.thaw objs'
          V.sortBy (sort fields') mobjs
          V.freeze mobjs
        applyContToValue cont handler (VList (PObject <$> v'))
    where
    sort fs o o' =
      foldr go EQ fs
      where
      go field EQ = case (,) <$> M.lookup field o <*> M.lookup field o' of
        Just (PLiteral l1, PLiteral l2) -> l1 `compare` l2
        _ -> EQ
      go _ ne = ne
  args -> argsError info b args


-- -------------------------
-- double ops
-- -------------------------

guardNanOrInf :: () -> Double -> Eval ()
guardNanOrInf info a =
  when (isNaN a || isInfinite a) $ throwExecutionError info (FloatingPointError "Floating operation resulted in Infinity or NaN")

dec2F :: Decimal -> Double
dec2F = fromRational . toRational

f2Dec :: Double -> Decimal
f2Dec = fromRational . toRational

roundDec :: CoreNativeFunction
roundDec = roundingFn round

floorDec :: CoreNativeFunction
floorDec = roundingFn floor

ceilingDec :: CoreNativeFunction
ceilingDec = roundingFn ceiling

---------------------------
-- bool ops
---------------------------
notBool :: CoreNativeFunction
notBool info b cont handler _env = \case
  [VLiteral (LBool i)] -> applyContToValue cont handler  (VLiteral (LBool (not i)))
  args -> argsError info b args

---------------------------
-- string ops
---------------------------

rawTake :: CoreNativeFunction
rawTake info b cont handler _env = \case
  [VLiteral (LInteger i), VLiteral (LString t)]
    | i >= 0 -> do
      let clamp = min (fromIntegral i) (T.length t)
      applyContToValue cont handler  (VLiteral (LString (T.take clamp t)))
    | otherwise -> do
      let clamp = min (abs (T.length t + fromIntegral i)) (T.length t)
      applyContToValue cont handler  (VLiteral (LString (T.drop clamp t)))
  [VLiteral (LInteger i), VList li]
    | i >= 0 -> do
      let clamp = fromIntegral $ min i (fromIntegral (V.length li))
      applyContToValue cont handler  (VList (V.take clamp li))
    | otherwise -> do
      let clamp = fromIntegral $ max (fromIntegral (V.length li) + i) 0
      applyContToValue cont handler (VList (V.drop clamp li))
  args -> argsError info b args

rawDrop :: CoreNativeFunction
rawDrop info b cont handler _env = \case
  [VLiteral (LInteger i), VLiteral (LString t)]
    | i >= 0 -> do
      let clamp = min (fromIntegral i) (T.length t)
      applyContToValue cont handler  (VLiteral (LString (T.drop clamp t)))
    | otherwise -> do
      let clamp = min (abs (T.length t + fromIntegral i)) (T.length t)
      applyContToValue cont handler  (VLiteral (LString (T.take clamp t)))
  [VLiteral (LInteger i), VList li]
    | i >= 0 -> do
      let clamp = fromIntegral $ min i (fromIntegral (V.length li))
      applyContToValue cont handler  (VList (V.drop clamp li))
    | otherwise -> do
      let clamp = fromIntegral $ max (fromIntegral (V.length li) + i) 0
      applyContToValue cont handler (VList (V.take clamp li))
  args -> argsError info b args

rawLength :: CoreNativeFunction
rawLength info b cont handler _env = \case
  [VString t] -> do
    applyContToValue cont handler  (VLiteral (LInteger (fromIntegral (T.length t))))
  [VList li] -> applyContToValue cont handler (VLiteral (LInteger (fromIntegral (V.length li))))
  [VObject o] ->
    applyContToValue cont handler $ VInteger $ fromIntegral (M.size o)
  args -> argsError info b args

rawReverse :: CoreNativeFunction
rawReverse info b cont handler _env = \case
  [VList li] ->
    applyContToValue cont handler (VList (V.reverse li))
  [VLiteral (LString t)] -> do
    applyContToValue cont handler  (VLiteral (LString (T.reverse t)))
  args -> argsError info b args

coreConcat :: CoreNativeFunction
coreConcat info b cont handler _env = \case
  [VList li] -> do
    li' <- traverse (asString info b) li
    applyContToValue cont handler (VString (T.concat (V.toList li')))
  args -> argsError info b args

strToList :: CoreNativeFunction
strToList info b cont handler _env = \case
  [VLiteral (LString s)] -> do
    let v = VList (V.fromList (PLiteral . LString . T.singleton <$> T.unpack s))
    applyContToValue cont handler v
  args -> argsError info b args


zipList :: CoreNativeFunction
zipList info b cont handler _env = \case
  [VClosure clo, VList l, VList r] ->
    case (V.toList l, V.toList r) of
      (x:xs, y:ys) -> do
        let cont' = BuiltinC _env info (ZipC clo (xs, ys) []) cont
        applyLam clo [VPactValue x, VPactValue y] cont' handler
      (_, _) -> applyContToValue cont handler (VList mempty)
  args -> argsError info b args

coreMap :: CoreNativeFunction
coreMap info b cont handler env = \case
  [VClosure clo, VList li] -> case V.toList li of
    x:xs -> do
      let cont' = BuiltinC env info (MapC clo xs []) cont
      applyLam clo [VPactValue x] cont' handler
    [] -> applyContToValue cont handler (VList mempty)
  args -> argsError info b args

coreFilter :: CoreNativeFunction
coreFilter info b cont handler _env = \case
  [VClosure clo, VList li] -> case V.toList li of
    x:xs -> do
      let cont' = CondC _env info (FilterC clo x xs []) cont
      applyLam clo [VPactValue x] cont' handler
    [] -> applyContToValue cont handler (VList mempty)
  args -> argsError info b args

coreFold :: CoreNativeFunction
coreFold info b cont handler _env = \case
  [VClosure clo, VPactValue initElem, VList li] ->
    case V.toList li of
      x:xs -> do
        let cont' = BuiltinC _env info (FoldC clo xs) cont
        applyLam clo [VPactValue initElem, VPactValue x] cont' handler
      [] -> applyContToValue cont handler (VPactValue initElem)
  args -> argsError info b args

coreEnumerate :: CoreNativeFunction
coreEnumerate info b cont handler _env = \case
  [VLiteral (LInteger from), VLiteral (LInteger to)] -> do
    v <- createEnumerateList info from to (if from > to then -1 else 1)
    applyContToValue cont handler (VList (PLiteral . LInteger <$> v))
  args -> argsError info b args

createEnumerateList
  :: ()
  -> Integer
  -- ^ from
  -> Integer
  -- ^ to
  -> Integer
  -- ^ Step
  -> Eval (Vector Integer)
createEnumerateList info from to inc
  | from == to = pure (V.singleton from)
  | inc == 0 = pure mempty
  | from < to, from + inc < from =
    throwExecutionError info (EnumerationError "enumerate: increment diverges below from interval bounds.")
  | from > to, from + inc > from =
    throwExecutionError info (EnumerationError "enumerate: increment diverges above from interval bounds.")
  | otherwise = let
    step = succ (abs (from - to) `div` abs inc)
    in pure $ V.enumFromStepN from inc (fromIntegral step)

coreEnumerateStepN :: CoreNativeFunction
coreEnumerateStepN info b cont handler _env = \case
  [VLiteral (LInteger from), VLiteral (LInteger to), VLiteral (LInteger inc)] -> do
    v <- createEnumerateList info from to inc
    applyContToValue cont handler (VList (PLiteral . LInteger <$> v))
  args -> argsError info b args

makeList :: CoreNativeFunction
makeList info b cont handler _env = \case
  [VLiteral (LInteger i), VPactValue v] -> do
    applyContToValue cont handler (VList (V.fromList (replicate (fromIntegral i) v)))
  args -> argsError info b args

coreAccess :: CoreNativeFunction
coreAccess info b cont handler _env = \case
  [VLiteral (LInteger i), VList vec] ->
    case vec V.!? fromIntegral i of
      Just v -> applyContToValue cont handler (VPactValue v)
      _ -> throwExecutionError info (ArrayOutOfBoundsException (V.length vec) (fromIntegral i))
  [VString field, VObject o] ->
    case M.lookup (Field field) o of
      Just v -> applyContToValue cont handler (VPactValue v)
      Nothing ->
        let msg = "Object does not have field: " <> field
        in applyCont cont handler (VError msg info)
  args -> argsError info b args

coreIsCharset :: CoreNativeFunction
coreIsCharset info b cont handler _env = \case
  [VLiteral (LInteger i), VString s] ->
    case i of
      0 -> applyContToValue cont handler $ VBool $ T.all Char.isAscii s
      1 -> applyContToValue cont handler $ VBool $ T.all Char.isLatin1 s
      _ -> applyCont cont handler (VError "Unsupported character set" info)
  args -> argsError info b args

coreYield :: CoreNativeFunction
coreYield info b cont handler _env = \case
  [VObject o] -> go o Nothing
  [VObject o, VString cid] -> go o (Just (ChainId cid))
  args -> argsError info b args
  where
  go o mcid = do
    mpe <- useEvalState esDefPactExec
    case mpe of
      Nothing -> throwExecutionError info YieldOutsiteDefPact
      Just pe -> case mcid of
        Nothing -> do
          esDefPactExec . _Just . peYield .== Just (Yield o Nothing Nothing)
          applyContToValue cont handler (VObject o)
        Just cid -> do
          sourceChain <- viewEvalEnv (eePublicData . pdPublicMeta . pmChainId)
          p <- provenanceOf cid
          when (_peStepHasRollback pe) $ failInvariant info "Cross-chain yield not allowed in step with rollback"
          esDefPactExec . _Just . peYield .== Just (Yield o (Just p) (Just sourceChain))
          applyContToValue cont handler (VObject o)
  provenanceOf tid =
    Provenance tid . _mHash <$> getCallingModule info

corePactId :: CoreNativeFunction
corePactId info b cont handler _env = \case
  [] -> useEvalState esDefPactExec >>= \case
    Just dpe -> applyContToValue cont handler (VString (_defpactId (_peDefPactId dpe)))
    Nothing -> applyCont cont handler (VError "pact-id: not in pact execution" info)
  args -> argsError info b args

enforceYield
  :: ()
  -> Yield
  -> Eval ()
enforceYield info y = case _yProvenance y of
  Nothing -> pure ()
  Just p -> do
    m <- getCallingModule info
    cid <- viewEvalEnv $ eePublicData . pdPublicMeta . pmChainId
    let p' = Provenance cid (_mHash m):map (Provenance cid) (toList $ _mBlessed m)
    unless (p `elem` p') $ throwExecutionError info (YieldProvenanceDoesNotMatch p p')

coreResume :: CoreNativeFunction
coreResume info b cont handler _env = \case
  [VClosure clo] -> do
    mps <- viewEvalEnv eeDefPactStep
    case mps of
      Nothing -> throwExecutionError info NoActiveDefPactExec
      Just pactStep -> case _psResume pactStep of
        Nothing -> throwExecutionError info (NoYieldInDefPactStep pactStep)
        Just y@(Yield resumeObj _ _) -> do
          enforceYield info y
          applyLam clo [VObject resumeObj] cont handler
  args -> argsError info b args

-----------------------------------
-- try-related ops
-----------------------------------

enforceTopLevelOnly :: (IsBuiltin b, MonadEval b i m) => i -> b -> m ()
enforceTopLevelOnly info b = do
  s <- useEvalState esStack
  unless (null s) $ throwExecutionError info (NativeIsTopLevelOnly (builtinName b))

-----------------------------------
-- Guards and reads
-----------------------------------


-----------------------------------
-- Other Core forms
-----------------------------------

coreB64Encode :: CoreNativeFunction
coreB64Encode info b cont handler _env = \case
  [VLiteral (LString l)] ->
    applyContToValue cont handler $ VLiteral $ LString $ toB64UrlUnpaddedText $ T.encodeUtf8 l
  args -> argsError info b args


coreB64Decode :: CoreNativeFunction
coreB64Decode info b cont handler _env = \case
  [VLiteral (LString s)] -> case fromB64UrlUnpaddedText $ T.encodeUtf8 s of
    Left{} -> throwExecutionError info (DecodeError "invalid b64 encoding")
    Right txt -> applyContToValue cont handler (VLiteral (LString txt))
  args -> argsError info b args


-- | The implementation of `enforce-guard` native.
coreEnforceGuard :: CoreNativeFunction
coreEnforceGuard info b cont handler env = \case
  [VGuard g] -> enforceGuard info cont handler env g
  [VString s] -> case parseAnyKeysetName s of
      Left {} -> applyCont cont handler (VError "incorrect keyset name format" info)
      Right ksn -> do
        cond <- isKeysetNameInSigs info (view cePactDb env) ksn
        if cond
          then applyContToValue cont handler (VBool True)
          else applyCont cont handler (VError "enforce keyset ref failure" info)
  args -> argsError info b args

keysetRefGuard :: CoreNativeFunction
keysetRefGuard info b cont handler env = \case
  [VString g] -> case parseAnyKeysetName g of
    Left {} -> applyCont cont handler (VError "incorrect keyset name format" info)
    Right ksn -> do
      let pdb = view cePactDb env
      liftDbFunction info (readKeySet pdb ksn) >>= \case
        Nothing -> applyCont cont handler (VError ("no such keyset defined: " <> g) info)
        Just _ -> applyContToValue cont handler (VGuard (GKeySetRef ksn))
  args -> argsError info b args

coreTypeOf :: CoreNativeFunction
coreTypeOf info b cont handler _env = \case
  [v] -> case v of
    VPactValue pv ->
      applyContToValue cont handler $ VString $ renderType $ synthesizePvType pv
    VClosure _ -> applyContToValue cont handler $ VString "<<closure>>"
    VTable tv -> applyContToValue cont handler $ VString (renderType (TyTable (_tvSchema tv)))
  args -> argsError info b args

coreDec :: CoreNativeFunction
coreDec info b cont handler _env = \case
  [VInteger i] -> applyContToValue cont handler $ VDecimal $ Decimal 0 i
  args -> argsError info b args

coreReadInteger :: CoreNativeFunction
coreReadInteger info b cont handler _env = \case
  [VString s] -> do
    viewEvalEnv eeMsgBody >>= \case
      PObject envData ->
        case M.lookup (Field s) envData of
          Just (PInteger p) -> applyContToValue cont handler (VInteger p)
          _ -> applyCont cont handler (VError "read-integer failure" info)
      _ -> applyCont cont handler (VError "read-integer failure" info)
  args -> argsError info b args

coreReadMsg :: CoreNativeFunction
coreReadMsg info b cont handler _env = \case
  [VString s] -> do
    viewEvalEnv eeMsgBody >>= \case
      PObject envData ->
        case M.lookup (Field s) envData of
          Just pv -> applyContToValue cont handler (VPactValue pv)
          _ -> applyCont cont handler (VError "read-msg failure" info)
      _ -> applyCont cont handler (VError "read-msg failure: data is not an object" info)
  [] -> do
    envData <- viewEvalEnv eeMsgBody
    applyContToValue cont handler (VPactValue envData)
  args -> argsError info b args

coreReadDecimal :: CoreNativeFunction
coreReadDecimal info b cont handler _env = \case
  [VString s] -> do
    viewEvalEnv eeMsgBody >>= \case
      PObject envData ->
        case M.lookup (Field s) envData of
          Just (PDecimal p) -> applyContToValue cont handler (VDecimal p)
          _ -> applyCont cont handler (VError "read-decimal failure" info)
      _ -> applyCont cont handler (VError "read-decimal failure" info)
  args -> argsError info b args

coreReadString :: CoreNativeFunction
coreReadString info b cont handler _env = \case
  [VString s] -> do
    viewEvalEnv eeMsgBody >>= \case
      PObject envData ->
        case M.lookup (Field s) envData of
          Just (PString p) -> applyContToValue cont handler (VString p)
          _ -> applyCont cont handler (VError "read-string failure" info)
      _ -> applyCont cont handler (VError "read-string failure" info)
  args -> argsError info b args

readKeyset' :: (MonadEval b i m) => T.Text -> m (Maybe (KeySet QualifiedName))
readKeyset' ksn = do
  viewEvalEnv eeMsgBody >>= \case
    PObject envData ->
      case M.lookup (Field ksn) envData of
        Just (PObject dat) -> parseObj dat
          where
          parseObj d = pure $ do
            keys <- M.lookup (Field "keys") d
            keyText <- preview _PList keys >>= traverse (fmap PublicKeyText . preview (_PLiteral . _LString))
            predRaw <- M.lookup (Field "pred") d
            p <- preview (_PLiteral . _LString) predRaw
            pred' <- readPredicate p
            let ks = S.fromList (V.toList keyText)
            pure (KeySet ks pred')
          readPredicate = \case
            "keys-any" -> pure KeysAny
            "keys-2" -> pure Keys2
            "keys-all" -> pure KeysAll
            _ -> Nothing
        Just (PList li) ->
          case parseKeyList li of
            Just ks -> pure (Just (KeySet ks KeysAll))
            Nothing -> pure Nothing
          where
          parseKeyList d =
            S.fromList . V.toList . fmap PublicKeyText <$> traverse (preview (_PLiteral . _LString)) d
        _ -> pure Nothing
    _ -> pure Nothing


coreReadKeyset :: CoreNativeFunction
coreReadKeyset info b cont handler _env = \case
  [VString ksn] ->
    readKeyset' ksn >>= \case
      Just ks -> do
        shouldEnforce <- isExecutionFlagSet FlagEnforceKeyFormats
        if shouldEnforce && isLeft (enforceKeyFormats (const ()) ks)
           then applyCont cont handler (VError "Invalid keyset" info)
           else applyContToValue cont handler (VGuard (GKeyset ks))
      Nothing -> applyCont cont handler (VError "read-keyset failure" info)
  args -> argsError info b args


coreBind :: CoreNativeFunction
coreBind info b cont handler _env = \case
  [v@VObject{}, VClosure clo] ->
    applyLam clo [v] cont handler
  args -> argsError info b args


--------------------------------------------------
-- Db functions
--------------------------------------------------

createTable :: CoreNativeFunction
createTable info b cont handler env = \case
  [VTable tv] -> do
    enforceTopLevelOnly info b
    let cont' = BuiltinC env info (CreateTableC tv) cont
    guardTable info cont' handler env tv GtCreateTable
  args -> argsError info b args

dbSelect :: CoreNativeFunction
dbSelect info b cont handler env = \case
  [VTable tv, VClosure clo] -> do
    let cont' = BuiltinC env info (PreSelectC tv clo Nothing) cont
    guardTable info cont' handler env tv GtSelect
  [VTable tv, VList li, VClosure clo] -> do
    li' <- traverse (fmap Field . asString info b) (V.toList li)
    let cont' = BuiltinC env info (PreSelectC tv clo (Just li')) cont
    guardTable info cont' handler env tv GtSelect
  args -> argsError info b args

-- Todo: error handling
foldDb :: CoreNativeFunction
foldDb info b cont handler env = \case
  [VTable tv, VClosure queryClo, VClosure consumer] -> do
    let cont' = BuiltinC env info (PreFoldDbC tv queryClo consumer) cont
    guardTable info cont' handler env tv GtSelect
  args -> argsError info b args

dbRead :: CoreNativeFunction
dbRead info b cont handler env = \case
  [VTable tv, VString k] -> do
    let cont' = BuiltinC env info (ReadC tv (RowKey k)) cont
    guardTable info cont' handler env tv GtRead
  args -> argsError info b args

dbWithRead :: CoreNativeFunction
dbWithRead info b cont handler env = \case
  [VTable tv, VString k, VClosure clo] -> do
    let cont' = BuiltinC env info (WithReadC tv (RowKey k) clo) cont
    guardTable info cont' handler env tv GtWithRead
  args -> argsError info b args

dbWithDefaultRead :: CoreNativeFunction
dbWithDefaultRead info b cont handler env = \case
  [VTable tv, VString k, VObject defaultObj, VClosure clo] -> do
    let cont' = BuiltinC env info (WithDefaultReadC tv (RowKey k) (ObjectData defaultObj) clo) cont
    guardTable info cont' handler env tv GtWithDefaultRead
  args -> argsError info b args

-- | Todo: schema checking here? Or only on writes?
dbWrite :: CoreNativeFunction
dbWrite = write' Write

dbInsert :: CoreNativeFunction
dbInsert = write' Insert

write' :: WriteType -> CoreNativeFunction
write' wt info b cont handler env = \case
  [VTable tv, VString key, VObject o] -> do
    let cont' = BuiltinC env info (WriteC tv wt (RowKey key) (ObjectData o)) cont
    guardTable info cont' handler env tv GtWrite
  args -> argsError info b args

dbUpdate :: CoreNativeFunction
dbUpdate = write' Update

dbKeys :: CoreNativeFunction
dbKeys info b cont handler env = \case
  [VTable tv] -> do
    let cont' = BuiltinC env info (KeysC tv) cont
    guardTable info cont' handler env tv GtKeys
  args -> argsError info b args

dbTxIds :: CoreNativeFunction
dbTxIds info b cont handler env = \case
  [VTable tv, VInteger tid] -> do
    checkNonLocalAllowed info
    let cont' = BuiltinC env info (TxIdsC tv tid) cont
    guardTable info cont' handler env tv GtTxIds
  args -> argsError info b args


dbTxLog :: CoreNativeFunction
dbTxLog info b cont handler env = \case
  [VTable tv, VInteger tid] -> do
    checkNonLocalAllowed info
    let cont' = BuiltinC env info (TxLogC tv tid) cont
    guardTable info cont' handler env tv GtTxLog
  args -> argsError info b args

dbKeyLog :: CoreNativeFunction
dbKeyLog info b cont handler env = \case
  [VTable tv, VString key, VInteger tid] -> do
    checkNonLocalAllowed info
    let cont' = BuiltinC env info (KeyLogC tv (RowKey key) tid) cont
    guardTable info cont' handler env tv GtKeyLog
  args -> argsError info b args

defineKeySet'
  :: ()
  -> CoreCEKCont
  -> CoreCEKHandler
  -> CoreCEKEnv
  -> T.Text
  -> KeySet QualifiedName
  -> Eval CoreEvalResult
defineKeySet' info cont handler env ksname newKs  = do
  let pdb = view cePactDb env
  ignoreNamespaces <- not <$> isExecutionFlagSet FlagRequireKeysetNs
  case parseAnyKeysetName ksname of
    Left {} -> applyCont cont handler (VError "incorrect keyset name format" info)
    Right ksn -> do
      let writeKs = do
            liftDbFunction info (writeKeySet pdb Write ksn newKs)
            applyContToValue cont handler (VString "Keyset write success")
      liftDbFunction info (readKeySet pdb ksn) >>= \case
        Just oldKs -> do
          cond <- isKeysetInSigs oldKs
          if cond then writeKs
          else applyCont cont handler (VError "enforce keyset failure" info)
        Nothing | ignoreNamespaces -> writeKs
        Nothing | otherwise -> useEvalState (esLoaded . loNamespace) >>= \case
          Nothing -> applyCont cont handler (VError "Cannot define a keyset outside of a namespace" info)
          Just (Namespace ns uGuard _adminGuard) -> do
            when (Just ns /= _keysetNs ksn) $ throwExecutionError info (MismatchingKeysetNamespace ns)
            let cont' = BuiltinC env info (DefineKeysetC ksn newKs) cont
            enforceGuard info cont' handler env uGuard

defineKeySet :: CoreNativeFunction
defineKeySet info b cont handler env = \case
  [VString ksname, VGuard (GKeyset ks)] -> do
    enforceTopLevelOnly info b
    defineKeySet' info cont handler env ksname ks
  [VString ksname] -> do
    enforceTopLevelOnly info b
    readKeyset' ksname >>= \case
      Just newKs ->
        defineKeySet' info cont handler env ksname newKs
      Nothing -> applyCont cont handler (VError "read-keyset failure" info)
  args -> argsError info b args

--------------------------------------------------
-- Capabilities
--------------------------------------------------

requireCapability :: CoreNativeFunction
requireCapability info b cont handler _env = \case
  [VCapToken ct] -> requireCap info cont handler ct
  args -> argsError info b args

composeCapability :: CoreNativeFunction
composeCapability info b cont handler env = \case
  [VCapToken ct] ->
    useEvalState esStack >>= \case
      sf:_ -> do
        when (_sfFnType sf /= SFDefcap) $ failInvariant info "compose-cap"
        composeCap info cont handler env ct
      _ -> failInvariant info "compose-cap at the top level"
  args -> argsError info b args

installCapability :: CoreNativeFunction
installCapability info b cont handler env = \case
  [VCapToken ct] -> do
    enforceNotWithinDefcap info env "install-capability"
    _ <- installCap info env ct True
    applyContToValue cont handler (VString "Installed capability")
  args -> argsError info b args

coreEmitEvent :: CoreNativeFunction
coreEmitEvent info b cont handler env = \case
  [VCapToken ct@(CapToken fqn _)] -> do
    let cont' = BuiltinC env info (EmitEventC ct) cont
    guardForModuleCall info cont' handler env (_fqModule fqn) $
      -- Todo: this code is repeated in the EmitEventFrame code
      lookupFqName (_ctName ct) >>= \case
        Just (DCap d) -> do
          enforceMeta (_dcapMeta d)
          emitCapability info ct
          applyContToValue cont handler (VBool True)
        Just _ ->
          failInvariant info "CapToken does not point to defcap"
        _ -> failInvariant info "No Capability found in emit-event"
        where
        enforceMeta Unmanaged = throwExecutionError info (InvalidEventCap fqn)
        enforceMeta _ = pure ()
  args -> argsError info b args

createCapGuard :: CoreNativeFunction
createCapGuard info b cont handler _env = \case
  [VCapToken ct] -> do
    let qn = fqnToQualName (_ctName ct)
        cg = CapabilityGuard qn (_ctArgs ct) Nothing
    applyContToValue cont handler (VGuard (GCapabilityGuard cg))
  args -> argsError info b args

createCapabilityPactGuard :: CoreNativeFunction
createCapabilityPactGuard info b cont handler _env = \case
  [VCapToken ct] -> do
    pid <- getDefPactId info
    let qn = fqnToQualName (_ctName ct)
    let cg = CapabilityGuard qn (_ctArgs ct) (Just pid)
    applyContToValue cont handler (VGuard (GCapabilityGuard cg))
  args -> argsError info b args

createModuleGuard :: CoreNativeFunction
createModuleGuard info b cont handler _env = \case
  [VString n] ->
    findCallingModule >>= \case
      Just mn ->  do
        let cg = GModuleGuard (ModuleGuard mn n)
        applyContToValue cont handler (VGuard cg)
      Nothing ->
        applyCont cont handler (VError "not-in-module" info)
  args -> argsError info b args

createDefPactGuard :: CoreNativeFunction
createDefPactGuard info b cont handler _env = \case
  [VString name] -> do
    dpid <- getDefPactId info
    applyContToValue cont handler $ VGuard $ GDefPactGuard $ DefPactGuard dpid name
  args -> argsError info b args


coreIntToStr :: CoreNativeFunction
coreIntToStr info b cont handler _env = \case
  [VInteger base, VInteger v]
    | base >= 2 && base <= 16 -> do
      let v' = T.pack $ showIntAtBase base Char.intToDigit v ""
      applyContToValue cont handler (VString v')
    | base == 64 && v >= 0 -> do
      let v' = toB64UrlUnpaddedText $ integerToBS v
      applyContToValue cont handler (VString v')
    | base == 64 -> applyCont cont handler (VError "only positive values allowed for base64URL conversion" info)
    | otherwise -> applyCont cont handler (VError "invalid base for base64URL conversion" info)
  args -> argsError info b args

coreStrToInt :: CoreNativeFunction
coreStrToInt info b cont handler _env = \case
  [VString s] ->
    checkLen info s *> doBase info cont handler 10 s
  args -> argsError info b args

coreStrToIntBase :: CoreNativeFunction
coreStrToIntBase info b cont handler _env = \case
  [VInteger base, VString s]
    | base == 64 -> checkLen info s *> case decodeBase64UrlUnpadded $ T.encodeUtf8 s of
        Left{} -> throwExecutionError info (DecodeError "invalid b64 encoding")
        Right bs -> applyContToValue cont handler $ VInteger (bsToInteger bs)
    | base >= 2 && base <= 16 -> checkLen info s *> doBase info cont handler base s
    | otherwise -> applyCont cont handler (VError "Base value must be >= 2 and <= 16, or 64" info)
  args -> argsError info b args
  where
  -- Todo: DOS and gas analysis
  bsToInteger :: BS.ByteString -> Integer
  bsToInteger bs = fst $ foldl' go (0,(BS.length bs - 1) * 8) $ BS.unpack bs
  go (i,p) w = (i .|. (shift (fromIntegral w) p),p - 8)

coreDistinct  :: CoreNativeFunction
coreDistinct info b cont handler _env = \case
  [VList s] ->
    applyContToValue cont handler
      $ VList
      $ V.fromList
      $ nubOrd
      $ V.toList s
  args -> argsError info b args

coreFormat  :: CoreNativeFunction
coreFormat info b cont handler _env = \case
  [VString s, VList es] -> do
    let parts = T.splitOn "{}" s
        plen = length parts
    if | plen == 1 -> applyContToValue cont handler (VString s)
       | plen - length es > 1 -> applyCont cont handler $ VError "format: not enough arguments for template" info
       | otherwise -> do
          let args = formatArg <$> V.toList es
          applyContToValue cont handler $ VString $  T.concat $ alternate parts (take (plen - 1) args)
    where
    formatArg (PString ps) = ps
    formatArg a = renderPactValue a
    alternate (x:xs) ys = x : alternate ys xs
    alternate _ _ = []
  args -> argsError info b args

-- Todo: This _Really_ needs gas
-- moreover this is kinda hacky
-- BIG TODO: REMOVE PRETTY FROM SEMANTICS.
-- THIS CANNOT MAKE IT TO PROD
renderPactValue :: PactValue -> T.Text
renderPactValue = T.pack . show . Pretty.pretty

checkLen
  :: ()
  -> T.Text
  -> Eval ()
checkLen info txt =
  unless (T.length txt <= 512) $
      throwExecutionError info $ DecodeError "Invalid input, only up to 512 length supported"

doBase
  :: ()
  -> CoreCEKCont
  -> CoreCEKHandler
  -> Integer
  -> T.Text
  -> Eval CoreEvalResult
doBase info cont handler base txt = case baseStrToInt base txt of
  Left e -> throwExecutionError info (DecodeError e)
  Right n -> applyContToValue cont handler (VInteger n)

baseStrToInt :: Integer -> T.Text -> Either T.Text Integer
baseStrToInt base t
  | base <= 1 || base > 16 = Left $ "unsupported base: " `T.append` T.pack (show base)
  | T.null t = Left $ "empty text: " `T.append` t
  | otherwise = foldM go 0 $ T.unpack t
  where
      go :: Integer -> Char -> Either T.Text Integer
      go acc c'
        = let val = fromIntegral . Char.digitToInt $ c'
          in
            if val < base then
                pure $ base * acc + val
            else
                Left
                  $ "character '"
                      <>
                        T.singleton c'
                          <> "' is out of range for base " <> T.pack (show base) <> ": " <> t

_bsToInteger :: BS.ByteString -> Integer
_bsToInteger bs = fst $ foldl' go (0,(BS.length bs - 1) * 8) $ BS.unpack bs
  where
    go (i,p) w = (i .|. shift (fromIntegral w) p,p - 8)

integerToBS :: Integer -> BS.ByteString
integerToBS v = BS.pack $ reverse $ go v
  where
    go i | i <= 0xff = [fromIntegral i]
         | otherwise = fromIntegral (i .&. 0xff):go (shift i (-8))


coreAndQ :: CoreNativeFunction
coreAndQ info b cont handler env = \case
  [VClosure l, VClosure r, VPactValue v] -> do
    let cont' =  CondC env info (AndQC r v) cont
    applyLam l [VPactValue v] cont' handler
  args -> argsError info b args

coreOrQ :: CoreNativeFunction
coreOrQ info b cont handler env = \case
  [VClosure l, VClosure r, VPactValue v] -> do
    let cont' =  CondC env info (OrQC r v) cont
    applyLam l [VPactValue v] cont' handler
  args -> argsError info b args

coreNotQ :: CoreNativeFunction
coreNotQ info b cont handler env = \case
  [VClosure clo, VPactValue v] -> do
    let cont' = CondC env info NotQC cont
    applyLam clo [VPactValue v] cont' handler
  args -> argsError info b args

coreWhere :: CoreNativeFunction
coreWhere info b cont handler _env = \case
  [VString field, VClosure app, VObject o] -> do
    case M.lookup (Field field) o of
      Just v -> do
        let cont' = EnforceBoolC info cont
        applyLam app [VPactValue v] cont' handler
      Nothing -> applyCont cont handler (VError "no such field in object in where application" info)
  args -> argsError info b args

coreHash :: CoreNativeFunction
coreHash = \info b cont handler _env -> \case
  [VString s] ->
    applyContToValue cont handler (go (T.encodeUtf8 s))
  [VPactValue pv] -> do
    applyContToValue cont handler (go (encodeStable pv))
  args -> argsError info b args
  where
  go =  VString . hashToText . pactHash

txHash :: CoreNativeFunction
txHash info b cont handler _env = \case
  [] -> do
    h <- viewEvalEnv eeHash
    applyContToValue cont handler (VString (hashToText h))
  args -> argsError info b args

coreContinue :: CoreNativeFunction
coreContinue info b cont handler _env = \case
  [v] -> do
    applyContToValue cont handler v
  args -> argsError info b args

parseTime :: CoreNativeFunction
parseTime info b cont handler _env = \case
  [VString fmt, VString s] ->
    case PactTime.parseTime (T.unpack fmt) (T.unpack s) of
      Just t -> applyContToValue cont handler $ VPactValue (PTime t)
      Nothing ->
        applyCont cont handler (VError "parse-time parse failure" info)
  args -> argsError info b args

formatTime :: CoreNativeFunction
formatTime info b cont handler _env = \case
  [VString fmt, VPactValue (PTime t)] -> do
    let timeString = PactTime.formatTime (T.unpack fmt) t
    applyContToValue cont handler $ VString (T.pack timeString)
  args -> argsError info b args

time :: CoreNativeFunction
time info b cont handler _env = \case
  [VString s] -> do
    case PactTime.parseTime "%Y-%m-%dT%H:%M:%SZ" (T.unpack s) of
      Just t -> applyContToValue cont handler $ VPactValue (PTime t)
      Nothing ->
        applyCont cont handler (VError "time default format parse failure" info)
  args -> argsError info b args

addTime :: CoreNativeFunction
addTime info b cont handler _env = \case
  [VPactValue (PTime t), VPactValue (PDecimal seconds)] -> do
      let newTime = t PactTime..+^ PactTime.fromSeconds seconds
      applyContToValue cont handler $ VPactValue (PTime newTime)
  args -> argsError info b args

diffTime :: CoreNativeFunction
diffTime info b cont handler _env = \case
  [VPactValue (PTime x), VPactValue (PTime y)] -> do
    let secondsDifference = PactTime.toSeconds $ x PactTime..-. y
    applyContToValue cont handler $ VPactValue $ PDecimal secondsDifference
  args -> argsError info b args

minutes :: CoreNativeFunction
minutes info b cont handler _env = \case
  [VDecimal x] -> do
    let seconds = x * 60
    applyContToValue cont handler $ VDecimal seconds
  [VInteger x] -> do
    let seconds = fromIntegral x * 60
    applyContToValue cont handler $ VDecimal seconds
  args -> argsError info b args

hours :: CoreNativeFunction
hours info b cont handler _env = \case
  [VDecimal x] -> do
    let seconds = x * 60 * 60
    applyContToValue cont handler $ VDecimal seconds
  [VInteger x] -> do
    let seconds = fromIntegral x * 60 * 60
    applyContToValue cont handler $ VDecimal seconds
  args -> argsError info b args

days :: CoreNativeFunction
days info b cont handler _env = \case
  [VDecimal x] -> do
    let seconds = x * 60 * 60 * 24
    applyContToValue cont handler $ VDecimal seconds
  [VInteger x] -> do
    let seconds = fromIntegral x * 60 * 60 * 24
    applyContToValue cont handler $ VDecimal seconds
  args -> argsError info b args

describeModule :: CoreNativeFunction
describeModule info b cont handler env = \case
  [VString s] -> case parseModuleName s of
    Just mname -> do
      enforceTopLevelOnly info b
      checkNonLocalAllowed info
      getModuleData info (view cePactDb env) mname >>= \case
        ModuleData m _ -> applyContToValue cont handler $
          VObject $ M.fromList $ fmap (over _1 Field)
            [ ("name", PString (renderModuleName (_mName m)))
            , ("hash", PString (moduleHashToText (_mHash m)))
            , ("interfaces", PList (PString . renderModuleName <$> V.fromList (_mImplements m)))]
        InterfaceData iface _ -> applyContToValue cont handler $
          VObject $ M.fromList $ fmap (over _1 Field)
            [ ("name", PString (renderModuleName (_ifName iface)))
            , ("hash", PString (moduleHashToText (_ifHash iface)))
            ]
    Nothing -> applyCont cont handler (VError "invalid module name" info)
  args -> argsError info b args

dbDescribeTable :: CoreNativeFunction
dbDescribeTable info b cont handler _env = \case
  [VTable (TableValue name _ schema)] ->
    applyContToValue cont handler $ VObject $ M.fromList $ fmap (over _1 Field)
      [("name", PString (_tableName name))
      ,("module", PString (renderModuleName (_tableModuleName name)))
      ,("type", PString (renderType (TyTable schema)))]
  args -> argsError info b args

dbDescribeKeySet :: CoreNativeFunction
dbDescribeKeySet info b cont handler env = \case
  [VString s] -> do
    let pdb = _cePactDb env
    enforceTopLevelOnly info b
    case parseAnyKeysetName s of
      Right ksn -> do
        liftDbFunction info (_pdbRead pdb DKeySets ksn) >>= \case
          Just ks ->
            applyContToValue cont handler (VGuard (GKeyset ks))
          Nothing ->
            applyCont cont handler (VError ("keyset not found" <> s) info)
      Left{} ->
        applyCont cont handler (VError "invalid keyset name" info)
  args -> argsError info b args

coreCompose :: CoreNativeFunction
coreCompose info b cont handler env = \case
  [VClosure clo1, VClosure clo2, v] -> do
    let cont' = Fn clo2 env [] [] cont
    applyLam clo1 [v] cont' handler
  args -> argsError info b args

createPrincipalForGuard :: Guard QualifiedName PactValue -> Pr.Principal
createPrincipalForGuard = \case
  GKeyset (KeySet ks pf) -> case (toList ks, pf) of
    ([k], KeysAll)
      | ed25519HexFormat k -> Pr.K k
    (l, _) -> let h = mkHash $ map (T.encodeUtf8 . _pubKey) l
              in Pr.W (hashToText h) (predicateToString pf)
  GKeySetRef ksn -> Pr.R ksn
  GModuleGuard (ModuleGuard mn n) -> Pr.M mn n
  GUserGuard (UserGuard f args) ->
    let h = mkHash $ map encodeStable args
    in Pr.U (renderQualName f) (hashToText h)
    -- TODO orig pact gets here ^^^^ a Name
    -- which can be any of QualifiedName/BareName/DynamicName/FQN,
    -- and uses the rendered string here. Need to double-check equivalence.
  GCapabilityGuard (CapabilityGuard f args pid) ->
    let args' = map encodeStable args
        f' = T.encodeUtf8 $ renderQualName f
        pid' = T.encodeUtf8 . renderDefPactId <$> pid
        h = mkHash $ f' : args' ++ maybeToList pid'
    in Pr.C $ hashToText h
  GDefPactGuard (DefPactGuard dpid name) -> Pr.P dpid name
  where
    mkHash bss = pactHash $ mconcat bss

coreCreatePrincipal :: CoreNativeFunction
coreCreatePrincipal info b cont handler _env = \case
  [VGuard g] -> do
    let pr = createPrincipalForGuard g
    applyContToValue cont handler $ VString $ Pr.mkPrincipalIdent pr
  args -> argsError info b args

coreIsPrincipal :: CoreNativeFunction
coreIsPrincipal info b cont handler _env = \case
  [VString p] -> applyContToValue cont handler $ VBool $ isRight $ parseOnly Pr.principalParser p
  args -> argsError info b args

coreTypeOfPrincipal :: CoreNativeFunction
coreTypeOfPrincipal info b cont handler _env = \case
  [VString p] -> do
    let prty = case parseOnly Pr.principalParser p of
          Left _ -> ""
          Right pr -> Pr.showPrincipalType pr
    applyContToValue cont handler $ VString prty
  args -> argsError info b args

coreValidatePrincipal :: CoreNativeFunction
coreValidatePrincipal info b cont handler _env = \case
  [VGuard g, VString s] -> do
    let pr' = createPrincipalForGuard g
    applyContToValue cont handler $ VBool $ Pr.mkPrincipalIdent pr' == s
  args -> argsError info b args


--------------------------------------------------
-- Namespace functions
--------------------------------------------------
coreNamespace :: CoreNativeFunction
coreNamespace info b cont handler env = \case
  [VString n] -> do
    enforceTopLevelOnly info b
    let pdb = view cePactDb env
    if T.null n then do
      (esLoaded . loNamespace) .== Nothing
      applyContToValue cont handler (VString "Namespace reset to root")
    else
      liftDbFunction info (_pdbRead pdb DNamespaces (NamespaceName n)) >>= \case
        Just ns -> do
          (esLoaded . loNamespace) .== Just ns
          let msg = "Namespace set to " <> n
          applyContToValue cont handler (VString msg)
        Nothing ->
          applyCont cont handler $ VError ("Namespace " <> n <> " not defined") info
  args -> argsError info b args


coreDefineNamespace :: CoreNativeFunction
coreDefineNamespace info b cont handler env = \case
  [VString n, VGuard usrG, VGuard adminG] -> do
    enforceTopLevelOnly info b
    unless (isValidNsFormat n) $ throwExecutionError info (DefineNamespaceError "invalid namespace format")
    let pdb = view cePactDb env
    let nsn = NamespaceName n
        ns = Namespace nsn usrG adminG
    liftDbFunction info (_pdbRead pdb DNamespaces (NamespaceName n)) >>= \case
      -- G!
      -- https://static.wikia.nocookie.net/onepiece/images/5/52/Lao_G_Manga_Infobox.png/revision/latest?cb=20150405020446
      -- Enforce the old guard
      Just (Namespace _ _ laoG) -> do
        let cont' = BuiltinC env info (DefineNamespaceC ns) cont
        enforceGuard info cont' handler env laoG
      Nothing -> viewEvalEnv eeNamespacePolicy >>= \case
        SimpleNamespacePolicy -> do
          liftDbFunction info (_pdbWrite pdb Write DNamespaces nsn ns)
          applyContToValue cont handler $ VString $ "Namespace defined: " <> n
        SmartNamespacePolicy _ fun -> getModuleMember info pdb fun >>= \case
          Dfun d -> do
            clo <- mkDefunClosure d (_qnModName fun) env
            let cont' = BuiltinC env info (DefineNamespaceC ns) cont
            applyLam (C clo) [VString n, VGuard adminG] cont' handler
          _ -> failInvariant info "Fatal error: namespace manager function is not a defun"
  args -> argsError info b args
  where
  isValidNsFormat nsn = case T.uncons nsn of
    Just (h, tl) ->
      isValidNsHead h && T.all isValidNsChar tl
    Nothing -> False
  isValidNsHead c =
    Char.isLatin1 c && Char.isAlpha c
  isValidNsChar c =
    Char.isLatin1 c && (Char.isAlphaNum c || T.elem c validSpecialChars)
  validSpecialChars :: T.Text
  validSpecialChars =
    "%#+-_&$@<>=^?*!|/~"

coreDescribeNamespace :: CoreNativeFunction
coreDescribeNamespace info b cont handler _env = \case
  [VString n] -> do
    pdb <- viewEvalEnv eePactDb
    liftDbFunction info (_pdbRead pdb DNamespaces (NamespaceName n)) >>= \case
      Just (Namespace _ usrG laoG) -> do
        let obj = M.fromList
                  [ (Field "user-guard", PGuard usrG)
                  , (Field "admin-guard", PGuard laoG)
                  , (Field "namespace-name", PString n)]
        applyContToValue cont handler (VObject obj)
      Nothing ->
        applyCont cont handler (VError ("Namespace not defined " <> n) info)
  args -> argsError info b args


coreChainData :: CoreNativeFunction
coreChainData info b cont handler _env = \case
  [] -> do
    PublicData publicMeta blockHeight blockTime prevBh <- viewEvalEnv eePublicData
    let (PublicMeta cid sender (Gas gasLimit) gasPrice _ttl _creationTime) = publicMeta
    let fields = M.fromList [ (cdChainId, PString (_chainId cid))
                 , (cdBlockHeight, PInteger (fromIntegral blockHeight))
                 , (cdBlockTime, PTime (PactTime.fromPosixTimestampMicros blockTime))
                 , (cdPrevBlockHash, PString prevBh)
                 , (cdSender, PString sender)
                 , (cdGasLimit, PInteger (fromIntegral gasLimit))
                 , (cdGasPrice, PDecimal gasPrice)]
    applyContToValue cont handler (VObject fields)
  args -> argsError info b args


-- -------------------------
-- ZK defns
-- -------------------------

ensureOnCurve :: (Num p, Eq p, MonadEval b i m) => i -> CurvePoint p -> p -> m ()
ensureOnCurve info p bp = unless (isOnCurve p bp) $ throwExecutionError info PointNotOnCurve

toG1 :: ObjectData PactValue -> Maybe G1
toG1 (ObjectData obj) = do
  px <- fromIntegral <$> preview (ix (Field "x") . _PLiteral . _LInteger) obj
  py <- fromIntegral <$> preview (ix (Field "y") . _PLiteral . _LInteger) obj
  if px == 0 && py == 0 then pure CurveInf
  else pure (Point px py)

fromG1 :: G1 -> ObjectData PactValue
fromG1 CurveInf = ObjectData pts
  where
  pts = M.fromList
    [ (Field "x", PLiteral (LInteger 0))
    , (Field "y", PLiteral (LInteger 0))]
fromG1 (Point x y) = ObjectData pts
  where
  pts =
    M.fromList
    [ (Field "x", PLiteral (LInteger (fromIntegral x)))
    , (Field "y", PLiteral (LInteger (fromIntegral y)))]

toG2 :: ObjectData PactValue -> Maybe G2
toG2 (ObjectData om) = do
  pxl <- preview (ix (Field "x") . _PList) om
  px <- traverse (preview (_PLiteral . _LInteger . Lens.to fromIntegral)) pxl
  pyl <- preview (ix (Field "y") . _PList) om
  py <- traverse (preview (_PLiteral . _LInteger . Lens.to fromIntegral)) pyl
  let px' = Exts.fromList (V.toList px)
      py' = Exts.fromList (V.toList py)
  if px' == 0 && py' == 0 then pure CurveInf
  else pure (Point px' py')

fromG2 :: G2 -> ObjectData PactValue
fromG2 CurveInf = ObjectData pts
  where
  pts =
    M.fromList
    [ (Field "x", PList (V.fromList [PLiteral (LInteger 0)]))
    , (Field "y", PList (V.fromList [PLiteral (LInteger 0)]))]
fromG2 (Point x y) = ObjectData pts
  where
  toPactPt (Extension e) = let
    elems' = fmap (PInteger . fromIntegral) (Poly.unPoly e)
    in PList elems'
  x' = toPactPt x
  y' = toPactPt y
  pts =
    M.fromList
    [ (Field "x", x')
    , (Field "y", y')]


zkPairingCheck :: CoreNativeFunction
zkPairingCheck info b cont handler _env = \case
  args@[VList p1s, VList p2s] -> do
    g1s <- maybe (argsError info b args) pure (traverse (preview _PObject >=> (toG1 . ObjectData)) p1s)
    g2s <- maybe (argsError info b args) pure (traverse (preview _PObject >=> (toG2 . ObjectData)) p2s)
    traverse_ (\p -> ensureOnCurve info p b1) g1s
    traverse_ (\p -> ensureOnCurve info p b2) g2s
    let pairs = zip (V.toList g1s) (V.toList g2s)
    applyContToValue cont handler $ VBool $ pairingCheck pairs
  args -> argsError info b args

zkScalaMult :: CoreNativeFunction
zkScalaMult info b cont handler _env = \case
  args@[VString ptTy, VObject p1, VInteger scalar] -> do
    let scalar' = scalar `mod` curveOrder
    case T.toLower ptTy of
      "g1" -> do
        p1' <- maybe (argsError info b args) pure $ toG1 (ObjectData p1)
        ensureOnCurve info p1' b1
        let p2' = multiply p1' scalar'
            ObjectData o = fromG1 p2'
        applyContToValue cont handler (VObject o)
      "g2" -> do
        p1' <- maybe (argsError info b args) pure $ toG2 (ObjectData p1)
        ensureOnCurve info p1' b2
        let p2' = multiply p1' scalar'
            ObjectData o = fromG2 p2'
        applyContToValue cont handler (VObject o)
      _ -> argsError info b args
  args -> argsError info b args
  where
  curveOrder :: Integer
  curveOrder = 21888242871839275222246405745257275088548364400416034343698204186575808495617

zkPointAddition :: CoreNativeFunction
zkPointAddition info b cont handler _env = \case
  args@[VString ptTy, VObject p1, VObject p2] -> do
    case T.toLower ptTy of
      "g1" -> do
        p1' <- maybe (argsError info b args) pure $ toG1 (ObjectData p1)
        p2' <- maybe (argsError info b args) pure $ toG1 (ObjectData p2)
        ensureOnCurve info p1' b1
        ensureOnCurve info p2' b1
        let p3' = add p1' p2'
            ObjectData o = fromG1 p3'
        applyContToValue cont handler (VObject o)
      "g2" -> do
        p1' <- maybe (argsError info b args) pure $ toG2 (ObjectData p1)
        p2' <- maybe (argsError info b args) pure $ toG2 (ObjectData p2)
        ensureOnCurve info p1' b2
        ensureOnCurve info p2' b2
        let p3' = add p1' p2'
            ObjectData o = fromG2 p3'
        applyContToValue cont handler (VObject o)
      _ -> argsError info b args
  args -> argsError info b args

-----------------------------------
-- Poseidon
-----------------------------------

poseidonHash :: CoreNativeFunction
poseidonHash info b cont handler _env = \case
  [VList as]
    | not (V.null as) && length as <= 8,
    Just intArgs <- traverse (preview (_PLiteral . _LInteger)) as ->
      applyContToValue cont handler $ VInteger (poseidon (V.toList intArgs))
  args -> argsError info b args

-----------------------------------
-- Builtin exports
-----------------------------------


rawBuiltinEnv
  :: CoreBuiltinEnv
rawBuiltinEnv i b env = mkBuiltinFn i b env (rawBuiltinRuntime b)


rawBuiltinRuntime
  :: RawBuiltin
  -> CoreNativeFunction
rawBuiltinRuntime = \case
  RawAdd -> rawAdd
  RawSub -> rawSub
  RawMultiply -> rawMul
  RawDivide -> rawDiv
  RawNegate -> rawNegate
  RawAbs -> rawAbs
  RawPow -> rawPow
  RawNot -> notBool
  RawEq -> rawEq
  RawNeq -> rawNeq
  RawGT -> rawGt
  RawGEQ -> rawGeq
  RawLT -> rawLt
  RawLEQ -> rawLeq
  RawBitwiseAnd -> bitAndInt
  RawBitwiseOr -> bitOrInt
  RawBitwiseXor -> bitXorInt
  RawBitwiseFlip -> bitComplementInt
  RawBitShift -> bitShiftInt
  RawRound -> roundDec
  RawCeiling -> ceilingDec
  RawFloor -> floorDec
  RawRoundPrec -> roundDec
  RawCeilingPrec -> ceilingDec
  RawFloorPrec -> floorDec
  RawExp -> rawExp
  RawLn -> rawLn
  RawSqrt -> rawSqrt
  RawLogBase -> rawLogBase
  RawLength -> rawLength
  RawTake -> rawTake
  RawDrop -> rawDrop
  RawConcat -> coreConcat
  RawReverse -> rawReverse
  RawMod -> modInt
  RawMap -> coreMap
  RawFilter -> coreFilter
  RawZip -> zipList
  RawIntToStr -> coreIntToStr
  RawStrToInt -> coreStrToInt
  RawStrToIntBase -> coreStrToIntBase
  RawFold -> coreFold
  RawDistinct -> coreDistinct
  RawFormat -> coreFormat
  RawContains -> rawContains
  RawSort -> rawSort
  RawSortObject -> rawSortObject
  RawRemove -> coreRemove
  -- RawEnforce -> coreEnforce
  -- RawEnforceOne -> unimplemented
  RawEnumerate -> coreEnumerate
  RawEnumerateStepN -> coreEnumerateStepN
  RawShow -> rawShow
  RawReadMsg -> coreReadMsg
  RawReadMsgDefault -> coreReadMsg
  RawReadInteger -> coreReadInteger
  RawReadDecimal -> coreReadDecimal
  RawReadString -> coreReadString
  RawReadKeyset -> coreReadKeyset
  RawEnforceGuard -> coreEnforceGuard
  RawYield -> coreYield
  RawYieldToChain -> coreYield
  RawResume -> coreResume
  RawEnforceKeyset -> coreEnforceGuard
  RawKeysetRefGuard -> keysetRefGuard
  RawAt -> coreAccess
  RawMakeList -> makeList
  RawB64Encode -> coreB64Encode
  RawB64Decode -> coreB64Decode
  RawStrToList -> strToList
  RawBind -> coreBind
  RawRequireCapability -> requireCapability
  RawComposeCapability -> composeCapability
  RawInstallCapability -> installCapability
  RawCreateCapabilityGuard -> createCapGuard
  RawCreateCapabilityPactGuard -> createCapabilityPactGuard
  RawCreateModuleGuard -> createModuleGuard
  RawCreateDefPactGuard -> createDefPactGuard
  RawEmitEvent -> coreEmitEvent
  RawCreateTable -> createTable
  RawDescribeKeyset -> dbDescribeKeySet
  RawDescribeModule -> describeModule
  RawDescribeTable -> dbDescribeTable
  RawDefineKeySet -> defineKeySet
  RawDefineKeysetData -> defineKeySet
  RawFoldDb -> foldDb
  RawInsert -> dbInsert
  RawWrite -> dbWrite
  RawKeyLog -> dbKeyLog
  RawKeys -> dbKeys
  RawRead -> dbRead
  RawSelect -> dbSelect
  RawUpdate -> dbUpdate
  RawWithDefaultRead -> dbWithDefaultRead
  RawWithRead -> dbWithRead
  RawTxLog -> dbTxLog
  RawTxIds -> dbTxIds
  RawAndQ -> coreAndQ
  RawOrQ -> coreOrQ
  RawWhere -> coreWhere
  RawNotQ -> coreNotQ
  RawHash -> coreHash
  RawTxHash -> txHash
  RawContinue -> coreContinue
  RawParseTime -> parseTime
  RawFormatTime -> formatTime
  RawTime -> time
  RawAddTime -> addTime
  RawDiffTime -> diffTime
  RawHours -> hours
  RawMinutes -> minutes
  RawDays -> days
  RawCompose -> coreCompose
  RawSelectWithFields -> dbSelect
  RawCreatePrincipal -> coreCreatePrincipal
  RawIsPrincipal -> coreIsPrincipal
  RawTypeOfPrincipal -> coreTypeOfPrincipal
  RawValidatePrincipal -> coreValidatePrincipal
  RawNamespace -> coreNamespace
  RawDefineNamespace -> coreDefineNamespace
  RawDescribeNamespace -> coreDescribeNamespace
  RawZkPairingCheck -> zkPairingCheck
  RawZKScalarMult -> zkScalaMult
  RawZkPointAdd -> zkPointAddition
  RawPoseidonHashHackachain -> poseidonHash
  RawChainData -> coreChainData
  RawIsCharset -> coreIsCharset
  RawPactId -> corePactId
  RawTypeOf -> coreTypeOf
  RawDec -> coreDec
