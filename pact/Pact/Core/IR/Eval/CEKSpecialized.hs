{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE CPP #-}

module Pact.Core.IR.Eval.CEKSpecialized
  ( eval
  , interpretGuard
  , coreBuiltinEnv
  , evalResumePact
  , resumePact
  , module Pact.Core.IR.Eval.CEK.Types
  , module Pact.Core.IR.Eval.CEK.Utils
  ) where


#ifndef WITHOUT_CRYPTO
import qualified Control.Lens as Lens
#endif
import Control.Lens hiding (from, to, op, parts)
import Control.Monad
import Control.Monad.IO.Class
import Data.Attoparsec.Text(parseOnly)
import Data.Bits
import Data.Either(isLeft, isRight)
import Data.Foldable
import Data.Decimal(roundTo', Decimal, DecimalRaw(..))
import Data.Vector(Vector)
import Data.Maybe(maybeToList)
import Numeric(showIntAtBase)

import Data.List.NonEmpty(NonEmpty(..))
import qualified Data.RAList as RAList
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector.Algorithms.Intro as V
import qualified Data.Char as Char
import qualified Data.ByteString as BS
import qualified GHC.Exts as Exts
import qualified GHC.Integer.Logarithms as IntLog
import qualified Pact.Time as PactTime

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
import Pact.Core.Info

import Pact.Core.IR.Term
import Pact.Core.IR.Eval.Runtime
import Pact.Core.Namespace
import Pact.Core.DefPacts.Types
import Pact.Core.SizeOf
import Pact.Core.Verifiers
#ifndef WITHOUT_CRYPTO
import Pact.Core.Crypto.Pairing
import Pact.Core.Crypto.Hash.Poseidon
#endif
import Pact.Core.SPV

import Pact.Core.IR.Eval.CEK.Types hiding (Eval)
import Pact.Core.IR.Eval.CEK.Utils
import qualified Pact.Core.Pretty as Pretty
import qualified Pact.Core.Principal as Pr
import qualified Pact.Core.Trans.TOps as Musl

type Eval = EvalM CoreBuiltin SpanInfo
{-
  Our CEKH Machine's transitions when reducing terms.
  `evalCEK` reduces a term and either directly produces a value,
  or grows the continuation with the information about evaluation of its subterms

  Our machine's state is an n-tuple <C,E,K,H> where:
    - C: (C)ontrol, which either corresponds to:
        - A term to evaluate (EvalTerm b i) for some builtin set b, and tree annotation i
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
evalCEK
  :: Cont CEKBigStep CoreBuiltin SpanInfo Eval
  -> CEKErrorHandler CEKBigStep CoreBuiltin SpanInfo Eval
  -> CEKEnv CEKBigStep CoreBuiltin SpanInfo Eval
  -> EvalTerm CoreBuiltin SpanInfo
  -> Eval (EvalResult CEKBigStep CoreBuiltin SpanInfo Eval)
-- | ------ From ------ | ------ To ------ |
--   <Var n, E, K, H>      <E(n), E, K, H>
--
-- Handles free variable lookups as well as module reference dynamic invokes
-- Todo: it may not be worthwhile if accessing local variables is fast to charge
-- anything but a constant amount of gas, but it would be a worthwhile exercise.
evalCEK cont handler env (Var n info) = do
  case _nKind n of
    NBound i -> do
      case RAList.lookup (_ceLocal env) i of
        Just v -> returnCEKValue cont handler v
        Nothing -> failInvariant info (InvariantInvalidBoundVariable (_nName n))
    -- Top level names are not closures, so we wipe the env
    NTopLevel mname mh -> do
      let fqn = FullyQualifiedName mname (_nName n) mh
      lookupFqName fqn >>= \case
        Just (Dfun d) -> do
          dfunClo <- VDefClosure <$> mkDefunClosure d fqn env
          returnCEKValue cont handler dfunClo
        -- Todo: this should be GADT'd out
        -- and defconsts should already be evaluated
        Just (DConst d) -> case _dcTerm d of
          -- Note: `TermConst` cannot and should not be `evalCEK`'d. This is an error
          -- this can cause semantic divergences, due to things like provided data.
          -- moreover defcosts are always evaluated in `SysOnly` mode.
          TermConst _term ->
            failInvariant info (InvariantDefConstNotEvaluated fqn)
          EvaledConst v ->
            returnCEKValue cont handler (VPactValue v)
        Just (DPact d) -> do
          let dpactClo = mkDefPactClosure info fqn d env
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
          failInvariant info (InvariantInvalidDefKind (defKind mname d) "in var position")
        Nothing ->
          failInvariant info (InvariantInvalidBoundVariable (_nName n))
    NModRef m ifs -> case ifs of
      [] -> throwExecutionError info (ModRefImplementsNoInterfaces m)
      _ ->
        returnCEKValue cont handler (VModRef (ModRef m (S.fromList ifs)))
    NDynRef (DynamicRef dArg i) -> case RAList.lookup (view ceLocal env) i of
      Just (VModRef mr) -> do
        modRefHash <- _mHash <$> getModule info (view cePactDb env) (_mrModule mr)
        let nk = NTopLevel (_mrModule mr) modRefHash
        evalCEK cont handler env (Var (Name dArg nk) info)
      Just _ ->
        throwExecutionError info (DynNameIsNotModRef (_nName n))
      Nothing -> failInvariant info (InvariantInvalidBoundVariable (_nName n))
-- | ------ From ------ | ------ To ------ |
--   <Const l, E, K, H>    <Value l, E, K, H>
--
evalCEK cont handler _env (Constant l _info) = do
  -- chargeGasArgs _info (GAConstant constantWorkNodeGas)
  returnCEKValue cont handler (VLiteral l)
-- | ------ From ---------- | ------ To ------ |
--   <App fn args, E, K, H>    <fn, E, Args(E,args,K), H>
--
evalCEK cont handler env (App fn args info) = do
  -- chargeGasArgs info (GAConstant constantWorkNodeGas)
  evalCEK (Args env info args cont) handler env fn
-- | ------ From ---------- | ------ To ------ |
--   <Nullary body, E, K, H>    <VClosure(body, E), E, K, H>
--
evalCEK cont handler env (Nullary body info) = do
  -- chargeGasArgs info (GAConstant constantWorkNodeGas)
  let clo = VLamClosure (LamClosure NullaryClosure 0 body Nothing env info)
  returnCEKValue cont handler clo
-- | ------ From ---------- | ------ To ------ |
--   <Let e1 e2, E, K, H>      <e1, E, LetC(E,e2,K), H>
--
evalCEK cont handler env (Let _ e1 e2 _info) = do
  -- chargeGasArgs _info (GAConstant constantWorkNodeGas)
  let cont' = LetC env e2 cont
  evalCEK cont' handler env e1
-- | ------ From ---------- | ------ To ------ |
--   <Lam args body, E, K, H>      <VLamClo(args, body, E), E, K, H>
--
evalCEK cont handler env (Lam args body info) = do
  -- chargeGasArgs info (GAConstant constantWorkNodeGas)
  let clo = VLamClosure (LamClosure (ArgClosure args) (NE.length args) body Nothing env info)
  returnCEKValue cont handler clo
-- | ------ From ------ | ------ To ------ |
--   <Builtin b, E, K, H>    <E(b), E, K, H>
--
evalCEK cont handler env (Builtin b info) = do
  -- chargeGasArgs info (GAConstant constantWorkNodeGas)
  -- let builtins = view ceBuiltins env
  returnCEKValue cont handler (VNative (coreBuiltinEnv info b env))
-- | ------ From ------ | ------ To ----------------- |
--   <Seq e1 e2, E, K, H>    <e1, E, SeqC(E, e2, K), H>
--
evalCEK cont handler env (Sequence e1 e2 _info) = do
  -- chargeGasArgs info (GAConstant constantWorkNodeGas)
  evalCEK (SeqC env e2 cont) handler env e1
-- | ------ From --------------- | ------ To ------------------------ |
--   <CAnd e1 e2, E, K, H>         <e1, E, CondC(E, AndFrame(e2),K),H>
--   <COr e1 e2, E, K, H>          <e1, E, CondC(E, OrFrame(e2),K),H>
--   <CIf cond ifc elc, E, K, H>   <cond, E, CondC(E, IfFrame(ifc,elc),K), H>
--  Todo: enforce and enforce-one
evalCEK cont handler env (Conditional c info) = case c of
  CAnd te te' -> do
    -- chargeGasArgs info (GAConstant constantWorkNodeGas)
    evalCEK (CondC env info (AndC te') cont) handler env te
  COr te te' -> do
    -- chargeGasArgs info (GAConstant constantWorkNodeGas)
    evalCEK (CondC env info (OrC te') cont) handler env te
  CIf cond e1 e2 -> do
    -- chargeGasArgs info (GAConstant constantWorkNodeGas)
    evalCEK (CondC env info (IfC e1 e2) cont) handler env cond
  CEnforce cond str -> do
    let env' = sysOnlyEnv env
    -- chargeGasArgs info (GAConstant constantWorkNodeGas)
    evalCEK (CondC env' info (EnforceC str) cont) handler env' cond
  CEnforceOne str conds -> do
    case conds of
      [] ->
        -- Note: this will simply be re-thrown within EnforceErrorC, so we don't need anything fancy here
        returnCEK cont handler (VError [] (UserEnforceError "internal CEnforceOne error") info)
      x:xs -> do
        -- Todo: is this a bit too cheap??
        chargeGasArgs info (GAConstant unconsWorkNodeGas)
        errState <- evalStateToErrorState <$> getEvalState
        let env' = readOnlyEnv env
        let handler' = CEKEnforceOne env' info str xs cont errState handler
        let cont' = CondC env' info EnforceOneC Mt
        evalCEK cont' handler' env' x
-- | ------ From --------------- | ------ To ------------------------ |
--   <WithCap cap body, E, K, H>         <cap, E, CapInvokeC(E,WithCapC(body), K),H>
--   <CreateUG n [], E, K, H>            <UGuard n [], E, K,H>
--   <CreateUG n (x:xs), E, K,H>         <x, E, CapInvokeC(E,CrUGC(n, xs),K), H>
evalCEK cont handler env (CapabilityForm cf info) = do
  -- chargeGasArgs info (GAConstant constantWorkNodeGas)
  case cf of
    WithCapability rawCap body -> do
      enforceNotWithinDefcap info env "with-capability"
      let capFrame = WithCapC body
          cont' = CapInvokeC env info capFrame cont
      evalCEK cont' handler env rawCap
    CreateUserGuard name args -> do
      fqn <- nameToFQN info env name
      case args of
        [] -> createUserGuard info cont handler fqn []
        x : xs -> do
          let usrGuardFrame = CreateUserGuardC fqn xs []
          let cont' = CapInvokeC env info usrGuardFrame cont
          evalCEK cont' handler env x
-- | ------ From --------------- | ------ To ------------------------ |
--   <ListLit [], E, K, H>         <VList [], E, K, H>
---  <ListLit (x:xs), E, K, H>         <x, E, ListC(E,xs,K), H>
evalCEK cont handler env (ListLit ts info) = do
  chargeGasArgs info (GConcat (ListConcat (GasListLength (length ts))))
  case ts of
    [] -> returnCEKValue cont handler (VList mempty)
    x:xs -> evalCEK (ListC env info xs [] cont) handler env x
-- | ------ From --------------- | ------ To ------------------------ |
--   <Try c body, E, K, H>         <body, E, Mt, CEKHandler(E,c,K,_errState,H)>
--   _errState - callstack,granted caps,events,gas
evalCEK cont handler env (Try catchExpr rest info) = do
  chargeGasArgs info (GAConstant tryNodeGas)
  errState <- evalStateToErrorState <$> getEvalState
  let handler' = CEKHandler env catchExpr cont errState handler
  let env' = readOnlyEnv env
  evalCEK Mt handler' env' rest
-- | ------ From --------------- | ------ To ------------------------ |
--   <Try c body, E, K, H>         <body, E, Mt, CEKHandler(E,c,K,_errState,H)>
--   _errState - callstack,granted caps,events,gas
evalCEK cont handler env (ObjectLit o info) = do
  chargeGasArgs info (GConcat (ObjConcat (length o)))
  case o of
    (f, term):rest -> do
      let cont' = ObjC env info f rest [] cont
      evalCEK cont' handler env term
    [] -> returnCEKValue cont handler (VObject mempty)

evalCEK cont handler _env (InlineValue v _) =
  returnCEKValue cont handler (VPactValue v)


mkDefunClosure
  :: Defun Name Type CoreBuiltin SpanInfo
  -> FullyQualifiedName
  -> CEKEnv CEKBigStep CoreBuiltin SpanInfo Eval
  -> Eval (Closure CEKBigStep CoreBuiltin SpanInfo Eval)
mkDefunClosure d fqn e = case _dfunTerm d of
  Lam args body i ->
    pure (Closure fqn (ArgClosure args) (NE.length args) body (_dfunRType d) e i)
  Nullary body i ->
    pure (Closure fqn NullaryClosure 0 body (_dfunRType d) e i)
  _ ->
    failInvariant (_dfunInfo d) (InvariantMalformedDefun fqn)

mkDefPactClosure
  :: SpanInfo
  -> FullyQualifiedName
  -> DefPact Name Type CoreBuiltin SpanInfo
  -> CEKEnv CEKBigStep CoreBuiltin SpanInfo Eval
  -> CEKValue CEKBigStep CoreBuiltin SpanInfo Eval
mkDefPactClosure info fqn dpact env = case _dpArgs dpact of
  [] ->
    let dpc = DefPactClosure fqn NullaryClosure 0 env info
    in VDefPactClosure dpc
  (x:xs) ->
    let dpc = DefPactClosure fqn (ArgClosure (x :| xs)) (length (x:xs)) env info
    in VDefPactClosure dpc

initPact
  :: SpanInfo
  -> DefPactContinuation QualifiedName PactValue
  -> Cont CEKBigStep CoreBuiltin SpanInfo Eval
  -> CEKErrorHandler CEKBigStep CoreBuiltin SpanInfo Eval
  -> CEKEnv CEKBigStep CoreBuiltin SpanInfo Eval
  -> Eval (EvalResult CEKBigStep CoreBuiltin SpanInfo Eval)
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
  :: SpanInfo
  -> DefPactContinuation QualifiedName PactValue
  -> DefPactStep
  -> Cont CEKBigStep CoreBuiltin SpanInfo Eval
  -> CEKErrorHandler CEKBigStep CoreBuiltin SpanInfo Eval
  -> CEKEnv CEKBigStep CoreBuiltin SpanInfo Eval
  -> M.Map DefPactId DefPactExec
  -> Eval (EvalResult CEKBigStep CoreBuiltin SpanInfo Eval)
applyPact i pc ps cont handler cenv nested = useEvalState esDefPactExec >>= \case
  Just pe -> throwExecutionError i (MultipleOrNestedDefPactExecFound pe)
  Nothing -> getModuleMemberWithHash i (_cePactDb cenv) (pc ^. pcName) >>= \case
    (DPact defPact, mh) -> do
      let nSteps = NE.length (_dpSteps defPact)

      -- `applyPact` is only called from `initPact` or `resumePact`.
      -- `initPact` ensures that the step is 0,
      -- and there are guaranteed more than 0 steps due to how the parser is written.
      -- `resumePact` does a similar check before calling this function.
      step <- maybe (throwExecutionError i (InvalidDefPactStepSupplied ps nSteps)) pure
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
      let cont' = DefPactStepC cenv i cont
          contFqn = qualNameToFqn (pc ^. pcName) mh
          sf = StackFrame contFqn (pc ^. pcArgs) SFDefPact i
      case (ps ^. psRollback, step) of
        (False, _) ->
          evalWithStackFrame i cont' handler cenv Nothing sf (ordinaryDefPactStepExec step)
        (True, StepWithRollback _ rollbackExpr) ->
          evalWithStackFrame i cont' handler cenv Nothing sf rollbackExpr
        (True, Step{}) -> throwExecutionError i (DefPactStepHasNoRollback ps)
    (_, mh) -> failInvariant i (InvariantExpectedDefPact (qualNameToFqn (pc ^. pcName) mh))

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
  => SpanInfo
  -> DefPactContinuation QualifiedName PactValue
  -> DefPactStep
  -> Cont CEKBigStep CoreBuiltin SpanInfo Eval
  -> CEKErrorHandler CEKBigStep CoreBuiltin SpanInfo Eval
  -> CEKEnv CEKBigStep CoreBuiltin SpanInfo Eval
  -> Eval (EvalResult CEKBigStep CoreBuiltin SpanInfo Eval)
applyNestedPact i pc ps cont handler cenv = useEvalState esDefPactExec >>= \case
  Nothing -> failInvariant i $ InvariantPactExecNotInEnv (Just pc)

  Just pe -> getModuleMemberWithHash i (_cePactDb cenv) (pc ^. pcName) >>= \case
    (DPact defPact, mh) -> do
      step <- maybe (throwExecutionError i (InvalidDefPactStepSupplied ps (_peStepCount pe))) pure
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
        cont' = NestedDefPactStepC cenv' i cont pe
        contFqn = FullyQualifiedName (pc ^. pcName . qnModName) (pc ^. pcName . qnName) mh
        sf = StackFrame contFqn (pc ^. pcArgs) SFDefPact i

      case (ps ^. psRollback, step) of
        (False, _) ->
          evalWithStackFrame i cont' handler cenv' Nothing sf (ordinaryDefPactStepExec step)
        (True, StepWithRollback _ rollbackExpr) ->
          evalWithStackFrame i cont' handler cenv' Nothing sf rollbackExpr
        (True, Step{}) -> throwExecutionError i (DefPactStepHasNoRollback ps)
    (_, mh) -> failInvariant i (InvariantExpectedDefPact (qualNameToFqn (pc ^. pcName) mh))

resumePact
  :: ()
  => SpanInfo
  -> Cont CEKBigStep CoreBuiltin SpanInfo Eval
  -> CEKErrorHandler CEKBigStep CoreBuiltin SpanInfo Eval
  -> CEKEnv CEKBigStep CoreBuiltin SpanInfo Eval
  -> Maybe DefPactExec
  -> Eval (EvalResult CEKBigStep CoreBuiltin SpanInfo Eval)
resumePact i cont handler env crossChainContinuation = viewEvalEnv eeDefPactStep >>= \case
  Nothing -> throwExecutionError i DefPactStepNotInEnvironment -- TODO check with multichain
  Just ps -> do
    pdb <- viewEvalEnv eePactDb
    dbState <- liftDbFunction i (readDefPacts pdb (_psDefPactId ps))
    case (dbState, crossChainContinuation) of

      -- Terminate defpact in db: always fail
      (Just Nothing, _) -> throwExecutionError i (DefPactAlreadyCompleted ps)

      -- Nothing in db, Nothing in cross-chain continuation: fail
      (Nothing, Nothing) -> throwExecutionError i (NoPreviousDefPactExecutionFound ps)  -- TODO check with multichain

      -- Nothing in db, Just cross-chain continuation: proceed with cross-chain
      (Nothing, Just ccExec) -> resumeDefPactExec ccExec

      -- Active db record, Nothing chross-chain continuation: proceed with db
      (Just (Just dbExec), Nothing) -> resumeDefPactExec dbExec

      -- Active db record and cross-chain continuation:
      -- A valid possibility iff this is a flip-flop from another chain, e.g.
      --   0. This chain: start pact
      --   1. Other chain: continue pact
      --   2. This chain: continue pact
      -- Thus check at least one step skipped.
      (Just (Just dbExec), Just ccExec) -> do

        -- Validate CC execution environment progressed far enough
        unless (_peStep ccExec > succ (_peStep dbExec)) $
          throwExecutionError i
            (CCDefPactContinuationError ps ccExec dbExec)

        -- Validate continuation db state
        when (_peContinuation dbExec /= _peContinuation ccExec) $
          throwExecutionError i (CCDefPactContinuationError ps ccExec dbExec)    -- TODO check with multichain

        -- Validate step count against db state
        when (_peStepCount dbExec /= _peStepCount ccExec) $
          throwExecutionError i (CCDefPactContinuationError ps ccExec dbExec)    -- TODO check with multichain

        resumeDefPactExec ccExec
      where
        --resumeDefPactExec ::  => DefPactExec -> Eval (EvalResult step CoreBuiltin SpanInfo Eval)
        resumeDefPactExec pe = do
          when (_psDefPactId ps /= _peDefPactId pe) $
            throwExecutionError i (DefPactIdMismatch (_psDefPactId ps) (_peDefPactId pe))    -- TODO check with multichain

          when (_psStep ps < 0 || _psStep ps >= _peStepCount pe) $
            throwExecutionError i (InvalidDefPactStepSupplied ps (_peStepCount pe))

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
-- Todo: is this enough checks for ndynref?
nameToFQN
  :: SpanInfo
  -> CEKEnv CEKBigStep CoreBuiltin SpanInfo Eval
  -> Name
  -> Eval FullyQualifiedName
nameToFQN info env (Name n nk) = case nk of
  NTopLevel mn mh -> pure (FullyQualifiedName mn n mh)
  NDynRef (DynamicRef dArg i) -> case RAList.lookup (view ceLocal env) i of
    Just (VModRef mr) -> do
      md <- getModule info (view cePactDb env) (_mrModule mr)
      pure (FullyQualifiedName (_mrModule mr) dArg (_mHash md))
    Just _ -> throwExecutionError info (DynNameIsNotModRef n)
    Nothing -> failInvariant info (InvariantInvalidBoundVariable n)
  _ -> failInvariant info (InvariantInvalidBoundVariable n)


guardTable
  :: SpanInfo
  -> Cont CEKBigStep CoreBuiltin SpanInfo Eval
  -> CEKErrorHandler CEKBigStep CoreBuiltin SpanInfo Eval
  -> CEKEnv CEKBigStep CoreBuiltin SpanInfo Eval
  -> TableValue
  -> GuardTableOp
  -> Eval (EvalResult CEKBigStep CoreBuiltin SpanInfo Eval)
guardTable i cont handler env (TableValue tn mh _) dbop = do
  let mn = _tableModuleName tn
  checkLocalBypass $
    guardForModuleCall i cont handler env mn $ do
      mdl <- getModule i (view cePactDb env) mn
      enforceBlessedHashes i mdl mh
      returnCEKValue cont handler VUnit
  where
  checkLocalBypass notBypassed = do
    enabled <- isExecutionFlagSet FlagAllowReadInLocal
    case dbop of
      GtWrite -> notBypassed
      GtCreateTable -> notBypassed
      _ | enabled -> returnCEKValue cont handler VUnit
        | otherwise -> notBypassed


guardForModuleCall
  :: SpanInfo
  -> Cont CEKBigStep CoreBuiltin SpanInfo Eval
  -> CEKErrorHandler CEKBigStep CoreBuiltin SpanInfo Eval
  -> CEKEnv CEKBigStep CoreBuiltin SpanInfo Eval
  -> ModuleName
  -> Eval (EvalResult CEKBigStep CoreBuiltin SpanInfo Eval)
  -> Eval (EvalResult CEKBigStep CoreBuiltin SpanInfo Eval)
guardForModuleCall i cont handler env currMod onFound =
  findCallingModule >>= \case
    Just mn | mn == currMod -> onFound
    _ -> do
      mc <- useEvalState (esCaps . csModuleAdmin)
      if S.member currMod mc then onFound
      else getModule i (view cePactDb env) currMod >>= acquireModuleAdmin i cont handler env

-- | Acquires module admin for a known module
-- NOTE: This function should only be called _after_
-- checking whether `esCaps . csModuleAdmin` for the particular
-- module is in scope
acquireModuleAdmin
  :: SpanInfo
  -> Cont CEKBigStep CoreBuiltin SpanInfo Eval
  -> CEKErrorHandler CEKBigStep CoreBuiltin SpanInfo Eval
  -> CEKEnv CEKBigStep CoreBuiltin SpanInfo Eval
  -> EvalModule CoreBuiltin SpanInfo
  -> Eval (EvalResult CEKBigStep CoreBuiltin SpanInfo Eval)
acquireModuleAdmin i cont handler env mdl = do
  case _mGovernance mdl of
    KeyGov ksn -> do
      let cont' = ModuleAdminC (_mName mdl) cont
      isKeysetNameInSigs i cont' handler env ksn
    CapGov (FQName fqn) -> do
      let wcapBody = Constant LUnit i
      let cont' = ModuleAdminC (_mName mdl) cont
      evalCap i cont' handler env (CapToken fqn []) PopCapInvoke NormalCapEval wcapBody

-- | Evaluate a term with all the stack manipulation logic
evalWithStackFrame
  :: ()
  => SpanInfo
  -> Cont CEKBigStep CoreBuiltin SpanInfo Eval
  -> CEKErrorHandler CEKBigStep CoreBuiltin SpanInfo Eval
  -> CEKEnv CEKBigStep CoreBuiltin SpanInfo Eval
  -> Maybe Type
  -> StackFrame SpanInfo
  -> EvalTerm CoreBuiltin SpanInfo
  -> Eval (EvalResult CEKBigStep CoreBuiltin SpanInfo Eval)
evalWithStackFrame info cont handler env mty sf body = do
  cont' <- pushStackFrame info cont mty sf
  evalCEK cont' handler env body

-- | Push a stack frame into the stack, and check it for recursion
pushStackFrame
  :: SpanInfo
  -> Cont CEKBigStep CoreBuiltin SpanInfo Eval
  -> Maybe Type
  -> StackFrame SpanInfo
  -> Eval (Cont CEKBigStep CoreBuiltin SpanInfo Eval)
pushStackFrame info cont mty sf = do
  checkRecursion
  esStack %== (sf :)
  pure (StackPopC info mty cont)
  where
  checkRecursion = do
    RecursionCheck currentCalled <- usesEvalState esCheckRecursion NE.head
    let qn = fqnToQualName (_sfName sf)
    when (S.member qn currentCalled) $ throwExecutionError info (RuntimeRecursionDetected qn)
    esCheckRecursion %== NE.cons (RecursionCheck (S.insert qn currentCalled))



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
  => SpanInfo
  -> Cont CEKBigStep CoreBuiltin SpanInfo Eval
  -> CEKErrorHandler CEKBigStep CoreBuiltin SpanInfo Eval
  -> CEKEnv CEKBigStep CoreBuiltin SpanInfo Eval
  -> FQCapToken
  -> CapPopState
  -> EvalCapType
  -> EvalTerm CoreBuiltin SpanInfo
  -> Eval (EvalResult CEKBigStep CoreBuiltin SpanInfo Eval)
evalCap info currCont handler env origToken@(CapToken fqn args) popType ecType contbody = do
  capInStack <- isCapInStack' origToken
  if not capInStack then go else evalCEK currCont handler env contbody
  where
  go = do
    capsBeingEvaluated <- useEvalState (esCaps.csCapsBeingEvaluated)
    d <- getDefCap info fqn
    when (length args /= length (_dcapArgs d)) $ failInvariant info $
      (InvariantArgLengthMismatch fqn (length args) (length (_dcapArgs d)))
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
                        emittedEvent = fqctToPactEvent origToken <$ guard (ecType == NormalCapEval)
                        cbState = CapBodyState popType (Just qualCapToken) emittedEvent contbody
                        contWithCapBody = CapBodyC env info cbState currCont
                        contWithPop = CapPopC (PopCurrCapEval capsBeingEvaluated) info contWithCapBody
                    installCap info env c' False >>= evalUserManagedCap contWithPop newLocals capBody
                  Nothing ->
                    throwExecutionError info (CapNotInstalled qualCapToken)
              Just managedCap -> do
                let emittedEvent = fqctToPactEvent origToken <$ guard (ecType == NormalCapEval)
                let cbState = CapBodyState popType (Just qualCapToken) emittedEvent contbody
                let contWithCapBody = CapBodyC env info cbState currCont
                    contWithPop = CapPopC (PopCurrCapEval capsBeingEvaluated) info contWithCapBody
                evalUserManagedCap contWithPop newLocals capBody managedCap
          -- handle autonomous caps
          AutoManagedMeta -> do
            -- Find the capability post-filtering
            let emittedEvent = fqctToPactEvent origToken <$ guard (ecType == NormalCapEval)
            let cbState = CapBodyState popType Nothing emittedEvent contbody
            let contWithCapBody = CapBodyC env info cbState currCont
                contWithPop = CapPopC (PopCurrCapEval capsBeingEvaluated) info contWithCapBody
            mgdCaps <- useEvalState (esCaps . csManaged)
            case find ((==) qualCapToken . _mcCap) mgdCaps of
              Nothing -> do
                msgCaps <- S.unions <$> viewEvalEnv eeMsgSigs
                case find (== qualCapToken) msgCaps of
                  Just c -> do
                    let c' = set ctName fqn c
                    installCap info env c' False >>= evalAutomanagedCap contWithPop newLocals capBody
                  Nothing ->
                    throwExecutionError info (CapNotInstalled qualCapToken)
              Just managedCap ->
                evalAutomanagedCap contWithPop newLocals capBody managedCap
      DefEvent -> do
        let cbState = CapBodyState popType Nothing (Just (fqctToPactEvent origToken)) contbody
        let contWithCapBody = CapBodyC env info cbState currCont
            contWithPop = CapPopC (PopCurrCapEval capsBeingEvaluated) info contWithCapBody
        let inCapEnv = set ceInCap True $ set ceLocal newLocals env
        (esCaps . csSlots) %== (CapSlot qualCapToken []:)
        (esCaps . csCapsBeingEvaluated) %== S.insert qualCapToken
        sfCont <- pushStackFrame info contWithPop Nothing capStackFrame
        evalCEK sfCont handler inCapEnv capBody
      -- Not automanaged _nor_ user managed.
      -- Todo: a type that's basically `Maybe` here would save us a lot of grief.
      Unmanaged -> do
        let cbState = CapBodyState popType Nothing Nothing contbody
        let contWithBody = if ecType == NormalCapEval then CapBodyC env info cbState currCont
                    else currCont
            contWithPop = CapPopC (PopCurrCapEval capsBeingEvaluated) info contWithBody
            inCapEnv = set ceInCap True $ set ceLocal newLocals env
        (esCaps . csCapsBeingEvaluated) %== S.insert qualCapToken
        (esCaps . csSlots) %== (CapSlot qualCapToken []:)
        evalWithStackFrame info contWithPop handler inCapEnv Nothing capStackFrame capBody
  qualCapName = fqnToQualName fqn
  qualCapToken = CapToken qualCapName args
  capStackFrame = StackFrame fqn args SFDefcap info
  -- This function is handles both evaluating the manager function for the installed parameter
  -- and continuing evaluation for the actual capability body.
  evalUserManagedCap cont' env' capBody managedCap = case _mcManaged managedCap of
    ManagedParam mpfqn oldV managedIx -> do
      dfun <- getDefun info mpfqn
      dfunClo <- mkDefunClosure dfun mpfqn env
      newV <- maybe (failInvariant info (InvariantInvalidManagedCapIndex managedIx fqn)) pure (args ^? ix managedIx)
      -- Set the mgr fun to evaluate after we apply the capability body
      -- NOTE: test-capability doesn't actually run the manager function, it just runs the cap pop then
      -- pops it. It would be great to do without this, but a lot of our regressions rely on this.
      let mgrFunCont = if ecType == NormalCapEval then
                         CapInvokeC env info (ApplyMgrFunC managedCap dfunClo oldV newV) cont'
                       else cont'
      let inCapEnv = set ceInCap True $ set ceLocal env' $ env
      let inCapBodyToken = _mcOriginalCap managedCap
      -- BIG SEMANTICS NOTE HERE
      -- the cap slot here that we push should NOT be the qualified original token.
      -- Instead, it's the original token from the installed from the static cap. Otherwise, enforce checks
      -- within the cap body will fail (That is, keyset enforcement). Instead, once we are evaluating the body,
      -- we pop the current cap stack, then replace the head with the original intended token.
      -- this is done in `CapBodyC` and this is the only way to do this.
      (esCaps . csSlots) %== (CapSlot inCapBodyToken []:)
      (esCaps . csCapsBeingEvaluated) %== S.insert inCapBodyToken
      sfCont <- pushStackFrame info mgrFunCont Nothing capStackFrame
      evalCEK sfCont handler inCapEnv capBody
    _ -> failInvariant info (InvariantInvalidManagedCapKind "expected user managed, received automanaged")
  evalAutomanagedCap cont' env' capBody managedCap = case _mcManaged managedCap of
    AutoManaged b -> do
      if b then returnCEKError info currCont handler OneShotCapAlreadyUsed
      else do
        let newManaged = AutoManaged True
        esCaps . csManaged %== S.union (S.singleton (set mcManaged newManaged managedCap))
        esCaps . csSlots %== (CapSlot qualCapToken []:)
        (esCaps . csCapsBeingEvaluated) %== S.insert qualCapToken
        let inCapEnv = set ceLocal env' $ set ceInCap True $ env
        sfCont <- pushStackFrame info cont' Nothing capStackFrame
        evalCEK sfCont handler inCapEnv capBody
    _ -> failInvariant info (InvariantInvalidManagedCapKind "expected automanaged, received user managed")


returnCEKError
  :: SpanInfo
  -> Cont CEKBigStep CoreBuiltin SpanInfo Eval
  -> CEKErrorHandler CEKBigStep CoreBuiltin SpanInfo Eval
  -> UserRecoverableError
  -> Eval (EvalResult CEKBigStep CoreBuiltin SpanInfo Eval)
returnCEKError info cont handler err = do
  stack <- useEvalState esStack
  returnCEK cont handler (VError stack err info)

emitEvent
  :: SpanInfo
  -> PactEvent PactValue
  -> Eval ()
emitEvent info pe = findCallingModule >>= \case
    Just mn -> do
      -- Todo: ++ definitely feels suboptimal, especially for gas.
      -- That said: we can simply reverse the events in `env-events` as
      -- well as after final emission.
      let ctModule = _peModule pe
      if ctModule == mn then do
        esEvents %== (++ [pe])
      else throwExecutionError info (EventDoesNotMatchModule mn)
    Nothing -> throwExecutionError info (EventDoesNotMatchModule (_peModule pe))

emitEventUnsafe
  :: (MonadEval b i m)
  => PactEvent PactValue
  -> m ()
emitEventUnsafe pe = esEvents %== (++ [pe])

emitReservedEvent :: MonadEval b i m => T.Text -> [PactValue] -> ModuleHash -> m ()
emitReservedEvent name params mhash = do
  let pactModule = ModuleName "pact" Nothing
  let pe = PactEvent name params pactModule mhash
  emitEventUnsafe pe

emitCapability
  :: SpanInfo
  -> CapToken FullyQualifiedName PactValue
  -> Eval ()
emitCapability info tkn =
  emitEvent info (fqctToPactEvent tkn)
fqctToPactEvent :: CapToken FullyQualifiedName PactValue -> PactEvent PactValue
fqctToPactEvent (CapToken fqn args) = PactEvent (_fqName fqn) args (_fqModule fqn) (_fqHash fqn)

enforceNotWithinDefcap
  :: SpanInfo
  -> CEKEnv CEKBigStep CoreBuiltin SpanInfo Eval
  -> T.Text
  -> Eval ()
enforceNotWithinDefcap info env form =
  when (_ceInCap env) $ throwExecutionError info (FormIllegalWithinDefcap form)

requireCap
  :: SpanInfo
  -> Cont CEKBigStep CoreBuiltin SpanInfo Eval
  -> CEKErrorHandler CEKBigStep CoreBuiltin SpanInfo Eval
  -> FQCapToken
  -> Eval (EvalResult CEKBigStep CoreBuiltin SpanInfo Eval)
requireCap info cont handler (CapToken fqn args) = do
  let qualCapToken = CapToken (fqnToQualName fqn) args
  capInStack <- isCapInStack qualCapToken
  if capInStack then returnCEKValue cont handler (VBool True)
  else returnCEKError info cont handler (CapabilityNotGranted qualCapToken)


isCapInStack
  :: CapToken QualifiedName PactValue
  -> Eval Bool
isCapInStack ct = S.member ct <$> getAllStackCaps

isCapInStack'
  :: CapToken FullyQualifiedName PactValue
  -> Eval Bool
isCapInStack' (CapToken fqn args) =
  isCapInStack (CapToken (fqnToQualName fqn) args)

composeCap
  :: SpanInfo
  -> Cont CEKBigStep CoreBuiltin SpanInfo Eval
  -> CEKErrorHandler CEKBigStep CoreBuiltin SpanInfo Eval
  -> CEKEnv CEKBigStep CoreBuiltin SpanInfo Eval
  -> FQCapToken
  -> Eval (EvalResult CEKBigStep CoreBuiltin SpanInfo Eval)
composeCap info cont handler env origToken =
  isCapInStack' origToken >>= \case
    False ->
      evalCap info cont handler env origToken PopCapComposed NormalCapEval (Constant (LBool True) info)
    True ->
      returnCEKValue cont handler (VBool True)


filterIndex :: Int -> [a] -> [a]
filterIndex i xs = [x | (x, i') <- zip xs [0..], i /= i']

findMsgSigCap :: Int -> CapToken QualifiedName PactValue -> CapToken QualifiedName PactValue -> Bool
findMsgSigCap cix ct1 ct2 =
  _ctName ct1 == _ctName ct2 && (_ctArgs ct1 == filterIndex cix (_ctArgs ct2))

-- Todo:
-- `capAutonomous` are what we should use to match semantics accurately.
installCap
  :: SpanInfo
  -> CEKEnv CEKBigStep CoreBuiltin SpanInfo Eval
  -> FQCapToken
  -> Bool
  -> Eval (ManagedCap QualifiedName PactValue)
installCap info _env (CapToken fqn args) autonomous = do
  let ct = CapToken (fqnToQualName fqn) args
  d <- getDefCap info fqn
  case _dcapMeta d of
    DefManaged m -> case m of
      DefManagedMeta (paramIx,_) (FQName fqnMgr) -> do
        managedParam <- maybe (failInvariant info $ InvariantInvalidManagedCapIndex paramIx fqn) pure (args ^? ix paramIx)
        let mcapType = ManagedParam fqnMgr managedParam paramIx
            ctFiltered = CapToken (fqnToQualName fqn) (filterIndex paramIx args)
            mcap = ManagedCap ctFiltered ct mcapType
        capAlreadyInstalled <- S.member mcap <$> useEvalState (esCaps . csManaged)
        when capAlreadyInstalled $ throwExecutionError info (CapAlreadyInstalled ct)
        (esCaps . csManaged) %== S.insert mcap
        when autonomous $
          (esCaps . csAutonomous) %== S.insert ct
        pure mcap
      AutoManagedMeta -> do
        let mcapType = AutoManaged False
            mcap = ManagedCap ct ct mcapType
        capAlreadyInstalled <- S.member mcap <$> useEvalState (esCaps . csManaged)
        when capAlreadyInstalled $ throwExecutionError info (CapAlreadyInstalled ct)
        (esCaps . csManaged) %== S.insert mcap
        when autonomous $
          (esCaps . csAutonomous) %== S.insert ct
        pure mcap
    DefEvent ->
      throwExecutionError info (InvalidManagedCap fqn)
    Unmanaged -> throwExecutionError info (InvalidManagedCap fqn)


-- Todo: should we typecheck / arity check here?
createUserGuard
  :: SpanInfo
  -> Cont CEKBigStep CoreBuiltin SpanInfo Eval
  -> CEKErrorHandler CEKBigStep CoreBuiltin SpanInfo Eval
  -> FullyQualifiedName
  -> [PactValue]
  -> Eval (EvalResult CEKBigStep CoreBuiltin SpanInfo Eval)
createUserGuard info cont handler fqn args = do
  -- Note: we could use `getDefun` here, but this gives us a better error
  lookupFqName fqn >>= \case
    Just (Dfun _) ->
      returnCEKValue cont handler (VGuard (GUserGuard (UserGuard (fqnToQualName fqn) args)))
    Just d ->
      -- Note: this error is not recoverable in prod
      -- <interactive>:0:26:Error: User guard closure must be defun, found: defcap
      -- at <interactive>:0:7: (create-user-guard ((defcap m.g:<a> ())))
      -- at <interactive>:0:0: (try 1 (native `create-user-guard`  Defines a custom guar...)
      throwExecutionError info $ UserGuardMustBeADefun (fqnToQualName fqn) (defKind (_fqModule fqn) d)
    Nothing ->
      failInvariant info (InvariantUnboundFreeVariable fqn)


returnCEK
  :: Cont CEKBigStep CoreBuiltin SpanInfo Eval
  -> CEKErrorHandler CEKBigStep CoreBuiltin SpanInfo Eval
  -> EvalResult CEKBigStep CoreBuiltin SpanInfo Eval
  -> Eval (EvalResult CEKBigStep CoreBuiltin SpanInfo Eval)
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
              oldFrame = CondC env i EnforceOneC Mt
          evalCEK oldFrame handler' env x
      EvalValue v' ->
        returnCEKValue cont h v'
returnCEK cont handler v = case v of
  VError{} -> returnCEK Mt handler v
  EvalValue v' -> returnCEKValue cont handler v'

-- | if true then 1 else 2
returnCEKValue
  :: Cont CEKBigStep CoreBuiltin SpanInfo Eval
  -> CEKErrorHandler CEKBigStep CoreBuiltin SpanInfo Eval
  -> CEKValue CEKBigStep CoreBuiltin SpanInfo Eval
  -> Eval (EvalResult CEKBigStep CoreBuiltin SpanInfo Eval)
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
-- | ------ From ------------------------- | ------------ To -------------- |
--   <VClosure c, Args(E, (x:xs), K), H>     <x, E, Fn(c, E, xs, K), H>
--
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
    -- Todo: this is _not_ an invariant failure. Requires a better error
    _ -> throwExecutionError i CannotApplyValueToNonClosure
-- | ------ From ------------------------- | ------ To ----------------------- |
--   <v, _, Fn(clo, E, (x:xs), acc, K), H>   <x, E, Fn(c, E, xs, (v:acc), K), H>
--   <v, _, Fn(clo, E, [], K), H>            (apply clo (reverse (v:acc)) K H)
--
returnCEKValue (Fn fn env args vs cont) handler v = do
  case args of
    [] -> do
      applyLam fn (reverse (v:vs)) cont handler
    x:xs ->
      evalCEK (Fn fn env xs (v:vs) cont) handler env x
-- | ------ From ------------ | ------ To ---------------- |
--   <v, LetC(E, body, K), H>   <body, (cons v E), K, H>
--
returnCEKValue (LetC env letbody cont) handler v = do
  evalCEK cont handler (over ceLocal (RAList.cons v) env) letbody
-- | ------ From ------------ | ------ To ---------------- |
--   <_, SeqC(E, e2, K), H>     <e2, E, K, H>
--
returnCEKValue (SeqC env e cont) handler _ =
  evalCEK cont handler env e
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
-- Note: we charge gas for this reduction here, as these are essentially natives
-- that match and perform an uncons/match.
returnCEKValue (CondC env info frame cont) handler v = do
  case v of
    VBool b -> case frame of
      AndC te ->
        if b then evalCEK (EnforceBoolC info cont) handler env te
        else returnCEKValue cont handler v
      OrC te ->
        if b then returnCEKValue cont handler v
        else evalCEK (EnforceBoolC info cont) handler env te
      IfC ifExpr elseExpr ->
        if b then evalCEK cont handler env ifExpr
        else evalCEK cont handler env elseExpr
      EnforceC str ->
        if b then returnCEKValue cont handler v
        else do
          let cont' = EnforceErrorC info cont
          evalCEK cont' handler env str
      FilterC clo elem' rest acc -> do
        let acc' = if b then elem':acc else acc
        case rest of
          x:xs -> do
            chargeGasArgs info (GAConstant unconsWorkNodeGas)
            let cont' = CondC env info (FilterC clo x xs acc') cont
            applyLam clo [VPactValue x] cont' handler
          [] -> returnCEKValue cont handler (VList (V.fromList (reverse acc')))
      EnforceOneC ->
        if b then returnCEKValue cont handler v
        else
        -- Note: this will simply be re-thrown within EnforceErrorC, so we don't need anything fancy here
        returnCEK cont handler (VError [] (UserEnforceError "internal CEnforceOne error") info)
      AndQC clo pv ->
        if b then applyLam clo [VPactValue pv] (EnforceBoolC info cont) handler
        else returnCEKValue cont handler v
      OrQC clo pv ->
        if not b then applyLam clo [VPactValue pv] (EnforceBoolC info cont) handler
        else returnCEKValue cont handler v
      NotQC -> returnCEKValue cont handler (VBool (not b))
    VPactValue v' -> throwExecutionError info (ExpectedBoolValue v')
    _ ->
      -- Note: a non-boolean value in these functions is non recoverable
      throwExecutionError info ExpectedPactValue
returnCEKValue currCont@(CapInvokeC env info cf cont) handler v = case cf of
  WithCapC body -> case v of
    VCapToken ct@(CapToken fqn _) -> do
      -- Todo: CEK-style this
      let cont' = IgnoreValueC (PCapToken ct) currCont
      guardForModuleCall info cont' handler env (_fqModule fqn) $
        evalCap info cont handler env ct PopCapInvoke NormalCapEval body
    -- Todo: this is actually more like "expected cap token"
    VPactValue v' -> throwExecutionError info $ ExpectedCapToken v'
    _ -> throwExecutionError info $ ExpectedPactValue
  CreateUserGuardC fqn terms pvs -> do
    pv <- enforcePactValue info v
    case terms of
      x:xs -> do
        let cf' = CreateUserGuardC fqn xs (pv:pvs)
            cont' = CapInvokeC env info cf' cont
        evalCEK cont' handler env x
      [] -> createUserGuard info cont handler fqn (reverse (pv:pvs))
  ApplyMgrFunC mgdCap clo old new -> do
    -- Set the manager fun to update the current managed cap.
    let cont' = EnforcePactValueC info $ CapInvokeC env info (UpdateMgrFunC mgdCap) cont
    applyLam (C clo) [VPactValue old, VPactValue new] cont' handler
  -- note: typechecking should be handled by the manager function here.
  UpdateMgrFunC mcap -> case v of
    VPactValue v' -> do
      let mcap' = unsafeUpdateManagedParam v' mcap
      (esCaps . csManaged) %== S.insert mcap'
      returnCEKValue cont handler v
    _ -> throwExecutionError info ExpectedPactValue
returnCEKValue (BuiltinC env info frame cont) handler cv = do
  let pdb = _cePactDb env
  case cv of
    VPactValue v -> case frame of
      MapC closure rest acc -> do
        case rest of
          x:xs -> do
            let cont' = BuiltinC env info (MapC closure xs (v:acc)) cont
            chargeGasArgs info (GAConstant unconsWorkNodeGas)
            applyLam closure [VPactValue x] cont' handler
          [] ->
            returnCEKValue cont handler (VList (V.fromList (reverse (v:acc))))
      FoldC clo rest -> do
        case rest of
          x:xs -> do
            let cont' = BuiltinC env info (FoldC clo xs) cont
            chargeGasArgs info (GAConstant unconsWorkNodeGas)
            applyLam clo [VPactValue v, VPactValue x] cont' handler
          [] -> returnCEKValue cont handler cv
      ZipC clo (l, r) acc -> do
        case (l, r) of
          (x:xs, y:ys) -> do
            let cont' = BuiltinC env info (ZipC clo (xs, ys) (v:acc)) cont
            chargeGasArgs info (GAConstant unconsWorkNodeGas)
            applyLam clo [VPactValue x, VPactValue y] cont' handler
          (_, _) ->
            returnCEKValue cont handler (VList (V.fromList (reverse (v:acc))))
      ---------------------------------------------------------
      -- Db frames
      -- Todo: gas costs if post-read actions
      ---------------------------------------------------------
      PreSelectC tv clo mf -> do
        keys <- liftDbFunction info (_pdbKeys pdb (tvToDomain tv))
        selectRead tv clo keys [] mf
      SelectC tv clo rdata remaining acc mf -> case v of
        PBool b -> do
          let acc' = if b then rdata:acc else acc
          selectRead tv clo remaining acc' mf
        _ -> throwExecutionError info $ ExpectedBoolValue v
      ReadC tv rowkey -> do
        liftDbFunction info (_pdbRead pdb (tvToDomain tv) rowkey) >>= \case
          Just (RowData rdata) ->
            returnCEKValue cont handler (VObject rdata)
          Nothing ->
            returnCEKError info cont handler $
              NoSuchObjectInDb (_tvName tv) rowkey
      WithDefaultReadC tv rowkey (ObjectData defaultObj) clo -> do
        liftDbFunction info (_pdbRead pdb (tvToDomain tv) rowkey) >>= \case
          Just (RowData rdata) ->
            applyLam clo [VObject rdata] cont handler
          Nothing -> applyLam clo [VObject defaultObj] cont handler
      KeysC tv -> do
        ks <- liftDbFunction info (_pdbKeys pdb (tvToDomain tv))
        let li = V.fromList (PString . _rowKey <$> ks)
        returnCEKValue cont handler (VList li)
      WriteC tv wt rk (ObjectData rv) -> do
        let check' = if wt == Update then checkPartialSchema else checkSchema
        if check' rv (_tvSchema tv) then do
          let rdata = RowData rv
          rvSize <- sizeOf SizeOfV0 rv
          chargeGasArgs info (GWrite rvSize)
          _ <- liftGasM info $ _pdbWrite pdb wt (tvToDomain tv) rk rdata
          returnCEKValue cont handler (VString "Write succeeded")
        else
          throwExecutionError info (WriteValueDidNotMatchSchema (_tvSchema tv) (ObjectData rv))
      PreFoldDbC tv queryClo appClo -> do
        let tblDomain = DUserTables (_tvName tv)
        -- Todo: keys gas
        keys <- liftDbFunction info (_pdbKeys pdb tblDomain)
        foldDBRead tv queryClo appClo keys []
      TxIdsC tv tid -> do
        ks <- liftDbFunction info (_pdbTxIds pdb (_tvName tv) (TxId (fromIntegral tid)))
        let li = V.fromList (PInteger . fromIntegral . _txId <$> ks)
        returnCEKValue cont handler (VList li)
      KeyLogC tv (RowKey key) tid -> do
        let txId = TxId (fromInteger tid)
        ids <- liftDbFunction info (_pdbTxIds pdb (_tvName tv) txId)
        ks <- concat <$> traverse (\t -> fmap (t,) <$> liftDbFunction info (_pdbGetTxLog pdb (_tvName tv) t)) ids
        let ks' = filter (\(_, txl) -> _txKey txl == key) ks
        let li = V.fromList (txLogToObj <$> ks')
        returnCEKValue cont handler (VList li)
        where
        txLogToObj (TxId txid, TxLog _domain _key (RowData rdata)) = do
          PObject $ M.fromList
            [ (Field "txid", PInteger (fromIntegral txid))
            , (Field "value", PObject rdata)]
      FoldDbFilterC tv queryClo appClo (rk, ObjectData om) remaining accum -> case v of
        PBool b -> do
          let accum' = if b then (rk, PObject om):accum else accum
          foldDBRead tv queryClo appClo remaining accum'
        _ ->
          throwExecutionError info (ExpectedBoolValue v)
      FoldDbMapC tv appClo remaining acc -> case remaining of
        (RowKey rk, pv):xs -> do
          let rdf = FoldDbMapC tv appClo xs (v:acc)
              cont' = BuiltinC env info rdf cont
          applyLam appClo [VString rk, VPactValue pv] cont' handler
        [] -> returnCEKValue cont handler (VList (V.fromList (v:acc)))
      TxLogC tv tid -> do
        let txId = TxId (fromInteger tid)
        ks <- liftDbFunction info (_pdbGetTxLog pdb (_tvName tv) txId)
        let li = V.fromList (txLogToObj <$> ks)
        returnCEKValue cont handler (VList li)
        where
        txLogToObj (TxLog domain key (RowData rdata)) = do
          PObject $ M.fromList
            [ (Field "table", PString domain)
            , (Field "key", PString key)
            , (Field "value", PObject rdata)]
      CreateTableC (TableValue tn _ _) -> do
        liftGasM info (_pdbCreateUserTable pdb tn)
        returnCEKValue cont handler (VString "TableCreated")
      EmitEventC ct@(CapToken fqn _) -> do
        d <- getDefCap info fqn
        enforceMeta (_dcapMeta d)
        emitCapability info ct
        returnCEKValue cont handler (VBool True)
        where
        enforceMeta Unmanaged = throwExecutionError info (InvalidEventCap fqn)
        enforceMeta _ = pure ()
      DefineKeysetC ksn newKs -> do
        newKsSize <- sizeOf SizeOfV0 newKs
        chargeGasArgs info (GWrite newKsSize)
        _ <- writeKeySet info pdb Write ksn newKs
        returnCEKValue cont handler (VString "Keyset write success")
      DefineNamespaceC ns -> case v of
        PBool allow ->
          if allow then do
            let nsn = _nsName ns
            nsSize <- sizeOf SizeOfV0 ns
            chargeGasArgs info (GWrite nsSize)
            liftGasM info $ _pdbWrite pdb Write DNamespaces nsn ns
            returnCEKValue cont handler $ VString $ "Namespace defined: " <> (_namespaceName nsn)
          -- injecting the NativeName directly here as to not have to unnecessarily thread `b` around in the
          -- cont
          else throwExecutionError info $ NativeExecutionError (NativeName "define-namespace") $ "Namespace definition not permitted"
        _ ->
          throwExecutionError info $ NativeExecutionError (NativeName "define-namespace") $ "Namespace manager function returned an invalid value"
      RunKeysetPredC (KeySet ksKeys ksPred) -> case v of
        PBool allow ->
          if allow then returnCEKValue cont handler (VBool True)
          else returnCEKError info cont handler $
            KeysetPredicateFailure ksPred ksKeys
        _ ->
          throwExecutionError info (ExpectedBoolValue v)
      where
      foldDBRead tv queryClo appClo remaining acc =
        case remaining of
          rk@(RowKey raw):remaining' -> liftDbFunction info (_pdbRead pdb (tvToDomain tv) rk) >>= \case
            Just (RowData row) -> do
              let rdf = FoldDbFilterC tv queryClo appClo (rk, ObjectData row) remaining' acc
                  cont' = BuiltinC env info rdf cont
              applyLam queryClo [VString raw, VObject row] cont' handler
            Nothing ->
              failInvariant info (InvariantNoSuchKeyInTable (_tvName tv) rk)
          [] -> case acc of
            (RowKey rk, pv):xs -> do
              let rdf = FoldDbMapC tv appClo xs []
                  cont' = BuiltinC env info rdf cont
              applyLam appClo [VString rk, VPactValue pv] cont' handler
            [] -> returnCEKValue cont handler (VList mempty)
      selectRead tv clo keys acc mf = case keys of
        k:ks -> liftDbFunction info (_pdbRead pdb (tvToDomain tv) k) >>= \case
          Just (RowData r) -> do
            let bf = SelectC tv clo (ObjectData r) ks acc mf
                cont' = BuiltinC env info bf cont
            applyLam clo [VObject r] cont' handler
          Nothing ->
            failInvariant info (InvariantNoSuchKeyInTable (_tvName tv) k)
        [] -> case mf of
          Just fields ->
            let acc' = PObject . (`M.restrictKeys` S.fromList fields) . _objectData <$> reverse acc
            in returnCEKValue cont handler (VList (V.fromList acc'))
          Nothing ->
            let acc' = PObject . _objectData <$> reverse acc
            in returnCEKValue cont handler (VList (V.fromList acc'))
    _ ->
      throwExecutionError info ExpectedPactValue
returnCEKValue (CapBodyC env info (CapBodyState cappop mcap mevent capbody) cont) handler _ = do
  -- Todo: I think this requires some administrative check?
  maybe (pure ()) emitEventUnsafe mevent
  case mcap of
    Nothing -> do
      let cont' = CapPopC cappop info cont
      evalCEK cont' handler env capbody
    -- We're in a managed cap! We gotta do some quick stack manipulation.
    Just cap -> useEvalState (esCaps . csSlots) >>= \case
      (CapSlot _ tl:rest) -> do
        setEvalState (esCaps . csSlots)  (CapSlot cap tl:rest)
        let cont' = CapPopC cappop info cont
        evalCEK cont' handler env capbody
      [] -> failInvariant info InvariantEmptyCapStackFailure

returnCEKValue (CapPopC st info cont) handler v = case st of
  PopCurrCapEval oldSet -> do
    esCaps . csCapsBeingEvaluated .== oldSet
    returnCEKValue cont handler v
  PopCapInvoke -> do
    esCaps . csSlots %== safeTail
    returnCEKValue cont handler v
  PopCapComposed -> do
    useEvalState (esCaps . csSlots) >>= \case
      cap:cs -> do
        let csList = _csCap cap : _csComposed cap
            caps' = over (_head . csComposed) (++ csList) cs
        setEvalState (esCaps . csSlots) caps'
        returnCEKValue cont handler VUnit
      [] -> failInvariant info InvariantEmptyCapStackFailure

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
  VString err ->
    returnCEKError info Mt handler $ UserEnforceError err
  VPactValue v' -> throwExecutionError info $ ExpectedStringValue v'
  _ -> throwExecutionError info $ ExpectedPactValue
-- Discard the value of running a user guard, no error occured, so
returnCEKValue (IgnoreValueC v cont) handler _v =
  returnCEKValue cont handler (VPactValue v)

returnCEKValue (StackPopC i mty cont) handler v = do
  v' <- enforcePactValue i v
  maybeTCType i mty v'
  esStack %== safeTail
  esCheckRecursion %== getPrevRecCheck
  returnCEKValue cont handler (VPactValue v')
  where
  getPrevRecCheck (_ :| l) = case l of
    top : rest -> top :| rest
    [] -> (RecursionCheck mempty) :| []

returnCEKValue (DefPactStepC env info cont) handler v =
  useEvalState esDefPactExec >>= \case
    Nothing -> failInvariant info $ InvariantPactExecNotInEnv Nothing
    Just pe -> case env ^. ceDefPactStep of
      Nothing -> failInvariant info (InvariantPactStepNotInEnv Nothing)
      Just ps -> do
        let
          pdb = view cePactDb env
          isLastStep = _psStep ps == pred (_peStepCount pe)
          done = (not (_psRollback ps) && isLastStep) || _psRollback ps
        when (nestedPactsNotAdvanced pe ps) $
          throwExecutionError info (NestedDefpactsNotAdvanced (_peDefPactId pe))
        writeDefPacts info pdb Write (_psDefPactId ps)
            (if done then Nothing else Just pe)
        emitXChainEvents (_psResume ps) pe
        returnCEKValue cont handler v

returnCEKValue (NestedDefPactStepC env info cont parentDefPactExec) handler v =
  useEvalState esDefPactExec >>= \case
    Nothing -> failInvariant info $ InvariantPactExecNotInEnv Nothing
    Just pe ->  case env ^. ceDefPactStep of
      Nothing -> failInvariant info (InvariantPactStepNotInEnv Nothing)
      Just ps -> do
        when (nestedPactsNotAdvanced pe ps) $
          throwExecutionError info (NestedDefpactsNotAdvanced (_peDefPactId pe))
        let npe = parentDefPactExec & peNestedDefPactExec %~ M.insert (_psDefPactId ps) pe
        setEvalState esDefPactExec (Just npe)
        returnCEKValue cont handler v

returnCEKValue (EnforcePactValueC info cont) handler v = case v of
  VPactValue{} -> returnCEKValue cont handler v
  _ -> throwExecutionError info ExpectedPactValue

returnCEKValue (EnforceBoolC info cont) handler v = case v of
  VBool{} -> returnCEKValue cont handler v
  VPactValue v' -> throwExecutionError info (ExpectedBoolValue v')
  _ -> throwExecutionError info ExpectedPactValue

returnCEKValue (ModuleAdminC mn cont) handler v = do
  (esCaps . csModuleAdmin) %== S.insert mn
  returnCEKValue cont handler v

-- returnCEKValue (EvalCapC env info captoken withCapBody cont) handler _ =
--   evalCap info cont handler env captoken (CapBodyC PopCapInvoke) withCapBody


-- | Important check for nested pacts:
--   Nested step must be equal to the parent step after execution.
nestedPactsNotAdvanced :: DefPactExec -> DefPactStep -> Bool
nestedPactsNotAdvanced resultState ps =
  any (\npe -> _peStep npe /= _psStep ps) (_peNestedDefPactExec resultState)
{-# INLINE nestedPactsNotAdvanced #-}

-- | Apply a closure to its arguments,
--   dispatching based on closure type.
applyLam
  :: ()
  => CanApply CEKBigStep CoreBuiltin SpanInfo Eval
  -> [CEKValue CEKBigStep CoreBuiltin SpanInfo Eval]
  -> Cont CEKBigStep CoreBuiltin SpanInfo Eval
  -> CEKErrorHandler CEKBigStep CoreBuiltin SpanInfo Eval
  -> Eval (EvalResult CEKBigStep CoreBuiltin SpanInfo Eval)
applyLam vc@(C (Closure fqn ca arity term mty env cloi)) args cont handler
  -- Fully apply closure and evaluate
  | arity == argLen = case ca of
    ArgClosure cloargs -> do
      chargeGasArgs cloi (GAApplyLam (renderFullyQualName fqn) argLen)
      args' <- traverse (enforcePactValue cloi) args
      zipWithM_ (\arg (Arg _ ty _) -> maybeTCType cloi ty arg) args' (NE.toList cloargs)
      let varEnv = RAList.fromList (reverse args)
      evalWithStackFrame cloi cont handler (set ceLocal varEnv env) mty (StackFrame fqn args' SFDefun cloi) term
    NullaryClosure -> do
      let varEnv = mempty
      evalWithStackFrame cloi cont handler (set ceLocal varEnv env) mty (StackFrame fqn [] SFDefun cloi) term
  | argLen > arity = throwExecutionError cloi ClosureAppliedToTooManyArgs
  | otherwise = case ca of
    NullaryClosure -> throwExecutionError cloi ClosureAppliedToTooManyArgs
    ArgClosure cloargs
      | null args ->
        returnCEKValue cont handler (VClosure vc)
      | otherwise -> do
        chargeGasArgs cloi (GAApplyLam (renderFullyQualName fqn) argLen)
        apply' mempty (NE.toList cloargs) args
  where
  argLen = length args
  -- Here we enforce an argument to a user fn is a pact value
  apply' e (Arg _ ty _:tys) (x:xs) = do
    x' <- enforcePactValue cloi x
    maybeTCType cloi ty x'
    apply' (RAList.cons (VPactValue x') e) tys xs
  apply' e (ty:tys) [] = do
    let env' = set ceLocal e env
        -- Todo: fix partial SF args
        pclo = PartialClosure (Just (StackFrame fqn [] SFDefun cloi)) (ty :| tys) (length tys + 1) term mty env' cloi
    returnCEKValue cont handler (VPartialClosure pclo)
  apply' _ [] _ = throwExecutionError cloi ClosureAppliedToTooManyArgs

applyLam (LC (LamClosure ca arity term mty env cloi)) args cont handler
  | arity == argLen = case ca of
    ArgClosure _ -> do
      -- Todo: maybe lambda application should mangle some sort of name?
      chargeGasArgs cloi (GAApplyLam "#lambda" argLen)
      let locals = view ceLocal env
          locals' = foldl' (flip RAList.cons) locals args
          cont' = EnforcePactValueC cloi cont
      evalCEK cont' handler (set ceLocal locals' env) term
    NullaryClosure -> do
      let cont' = EnforcePactValueC cloi cont
      evalCEK cont' handler env term
  | argLen > arity = throwExecutionError cloi ClosureAppliedToTooManyArgs
  | otherwise = case ca of
      NullaryClosure -> throwExecutionError cloi ClosureAppliedToTooManyArgs
      ArgClosure cloargs -> do
        chargeGasArgs cloi (GAApplyLam "#lambda" argLen)
        apply' (view ceLocal env) (NE.toList cloargs) args
  where
  argLen = length args
  -- Todo: runtime TC here
  apply' e (Arg _ ty _:tys) (x:xs) = do
    x' <- enforcePactValue cloi x
    maybeTCType cloi ty x'
    apply' (RAList.cons (VPactValue x') e) tys xs
  apply' e [] [] = do
    evalCEK cont handler (set ceLocal e env) term
  apply' e (ty:tys) [] =
    returnCEKValue cont handler
    (VPartialClosure (PartialClosure Nothing (ty :| tys) (length tys + 1) term mty (set ceLocal e env) cloi))
  apply' _ [] _ = throwExecutionError cloi ClosureAppliedToTooManyArgs

applyLam (PC (PartialClosure li argtys _ term mty env cloi)) args cont handler = do
  chargeGasArgs cloi (GAApplyLam (getSfName li) (length args))
  apply' (view ceLocal env) (NE.toList argtys) args
  where
  apply' e (Arg _ ty _:tys) (x:xs) = do
    x' <- enforcePactValue cloi x
    maybeTCType cloi ty x'
    apply' (RAList.cons (VPactValue x') e) tys xs
  apply' e [] [] = do
    case li of
      Just sf -> do
        evalWithStackFrame cloi cont handler (set ceLocal e env) mty sf term
      Nothing -> do
        let cont' = EnforcePactValueC cloi cont
        evalCEK cont' handler (set ceLocal e env) term
  apply' e (ty:tys) [] = do
    let pclo = PartialClosure li (ty :| tys) (length tys + 1) term mty (set ceLocal e env) cloi
    returnCEKValue cont handler (VPartialClosure pclo)
  apply' _ [] _ = throwExecutionError cloi ClosureAppliedToTooManyArgs

applyLam nclo@(N (NativeFn b env fn arity i)) args cont handler
  | arity == argLen = do
    chargeFlatNativeGas i b
    fn i b cont handler env args
  | argLen > arity = throwExecutionError i ClosureAppliedToTooManyArgs
  | null args = returnCEKValue cont handler (VClosure nclo)
  | otherwise =
    apply' arity [] args
  where
  argLen = length args
  apply' !a pa (x:xs) = apply' (a - 1) (x:pa) xs
  apply' !a pa [] =
    returnCEKValue cont handler (VPartialNative (PartialNativeFn b env fn a pa i))

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
    returnCEKValue cont handler (VPartialNative (PartialNativeFn b env fn a pa i))

applyLam (DPC (DefPactClosure fqn argtys arity env i)) args cont handler
  | arity == argLen = case argtys of
    ArgClosure cloargs -> do
      -- Todo: defpact has much higher overhead, we must charge a bit more gas for this
      chargeGasArgs i (GAApplyLam (renderQualName (fqnToQualName fqn)) (fromIntegral argLen))
      args' <- traverse (enforcePactValue i) args
      zipWithM_ (\arg (Arg _ ty _) -> maybeTCType i ty arg) args' (NE.toList cloargs)
      let pc = DefPactContinuation (fqnToQualName fqn) args'
          env' = set ceLocal (RAList.fromList (reverse args)) env
      initPact i pc cont handler env'
    NullaryClosure -> do
      chargeGasArgs i (GAApplyLam (renderQualName (fqnToQualName fqn)) (fromIntegral argLen))
      let pc = DefPactContinuation (fqnToQualName fqn) []
          env' = set ceLocal mempty env
      -- Todo: defpact has much higher overhead, we must charge a bit more gas for this
      initPact i pc cont handler env'
  | otherwise = throwExecutionError i ClosureAppliedToTooManyArgs
  where
  argLen = length args
applyLam (CT (CapTokenClosure fqn argtys arity i)) args cont handler
  | arity == argLen = do
    chargeGasArgs i (GAApplyLam (renderQualName (fqnToQualName fqn)) (fromIntegral argLen))
    args' <- traverse (enforcePactValue i) args
    zipWithM_ (\arg ty -> maybeTCType i ty arg) args' argtys
    returnCEKValue cont handler (VPactValue (PCapToken (CapToken fqn args')))
  | otherwise = throwExecutionError i ClosureAppliedToTooManyArgs
  where
  argLen = length args

getSfName :: Maybe (StackFrame i) -> T.Text
getSfName = \case
  Just sf -> renderFullyQualName (_sfName sf)
  Nothing -> "#lambda"



-- | The main logic of enforcing a guard.
--
-- The main difference to `coreEnforceGuard` is this function's type doesn't need to be a `NativeFunction CEKBigStep b i m`,
-- thus there's no need to wrap/unwrap the guard into a `VPactValue`,
-- and moreover it does not need to take a `b` which it does not use anyway.
enforceGuard
  :: SpanInfo
  -> Cont CEKBigStep CoreBuiltin SpanInfo Eval
  -> CEKErrorHandler CEKBigStep CoreBuiltin SpanInfo Eval
  -> CEKEnv CEKBigStep CoreBuiltin SpanInfo Eval
  -> Guard QualifiedName PactValue
  -> Eval (EvalResult CEKBigStep CoreBuiltin SpanInfo Eval)
enforceGuard info cont handler env g = case g of
  GKeyset ks -> do
    isKeysetInSigs info cont handler env ks
  GKeySetRef ksn -> do
    isKeysetNameInSigs info cont handler env ksn
  GUserGuard ug -> runUserGuard info cont handler env ug
  GCapabilityGuard cg -> enforceCapGuard info cont handler cg
  GModuleGuard (ModuleGuard mn _) -> calledByModule mn >>= \case
    True -> returnCEKValue cont handler (VBool True)
    False -> do
      md <- getModule info (view cePactDb env) mn
      let cont' = IgnoreValueC (PBool True) cont
      acquireModuleAdmin info cont' handler env md
      -- returnCEKValue cont handler (VBool True)guard
  GDefPactGuard (DefPactGuard dpid _) -> do
    curDpid <- getDefPactId info
    if curDpid == dpid
       then returnCEKValue cont handler (VBool True)
       else returnCEKError info cont handler $
         CapabilityPactGuardInvalidPactId curDpid dpid


enforceCapGuard
  :: SpanInfo
  -> Cont CEKBigStep CoreBuiltin SpanInfo Eval
  -> CEKErrorHandler CEKBigStep CoreBuiltin SpanInfo Eval
  -> CapabilityGuard QualifiedName PactValue
  -> Eval (EvalResult CEKBigStep CoreBuiltin SpanInfo Eval)
enforceCapGuard info cont handler cg@(CapabilityGuard qn args mpid) = case mpid of
  Nothing -> enforceCap
  Just pid -> do
    currPid <- getDefPactId info
    if currPid == pid then enforceCap
    else returnCEKError info cont handler $
         CapabilityPactGuardInvalidPactId currPid pid
  where
  enforceCap = do
    cond <- isCapInStack (CapToken qn args)
    if cond then returnCEKValue cont handler (VBool True)
    else returnCEKError info cont handler $
      CapabilityGuardNotAcquired cg


runUserGuard
  :: SpanInfo
  -> Cont CEKBigStep CoreBuiltin SpanInfo Eval
  -> CEKErrorHandler CEKBigStep CoreBuiltin SpanInfo Eval
  -> CEKEnv CEKBigStep CoreBuiltin SpanInfo Eval
  -> UserGuard QualifiedName PactValue
  -> Eval (EvalResult CEKBigStep CoreBuiltin SpanInfo Eval)
runUserGuard info cont handler env (UserGuard qn args) =
  getModuleMemberWithHash info (_cePactDb env) qn >>= \case
    (Dfun d, mh) -> do
      when (length (_dfunArgs d) /= length args) $ throwExecutionError info CannotApplyPartialClosure
      let env' = sysOnlyEnv env
      clo <- mkDefunClosure d (qualNameToFqn qn mh) env'
      -- Todo: sys only here
      applyLam (C clo) (VPactValue <$> args) (IgnoreValueC (PBool True) cont) handler
    (d, _) -> throwExecutionError info (UserGuardMustBeADefun qn (defKind (_qnModName qn) d))


eval
  :: Purity
  -> EvalTerm CoreBuiltin SpanInfo
  -> Eval PactValue
eval purity term = do
  ee <- readEnv
  let cekEnv = envFromPurity purity (CEKEnv mempty (_eePactDb ee) coreBuiltinEnv (_eeDefPactStep ee) False)
  evalCEK Mt CEKNoHandler cekEnv term >>= \case
    VError stack err i ->
      throwUserRecoverableError' i stack err
    EvalValue v -> do
      case v of
        VPactValue pv -> pure pv
        _ ->
          throwExecutionError (view termInfo term) (EvalError "Evaluation did not reduce to a value")


interpretGuard
  :: SpanInfo
  -> Guard QualifiedName PactValue
  -> Eval PactValue
interpretGuard info g = do
  ee <- readEnv
  let cekEnv = CEKEnv mempty (_eePactDb ee) coreBuiltinEnv (_eeDefPactStep ee) False
  enforceGuard info Mt CEKNoHandler cekEnv g >>= \case
    VError stack err i ->
      throwUserRecoverableError' i stack err
    EvalValue v -> do
      case v of
        VPactValue pv -> pure pv
        _ ->
          throwExecutionError info (EvalError "Evaluation did not reduce to a value")


evalResumePact
  :: SpanInfo
  -> BuiltinEnv CEKBigStep CoreBuiltin SpanInfo Eval
  -> Maybe DefPactExec
  -> Eval PactValue
evalResumePact info bEnv mdpe = do
  ee <- readEnv
  let pdb = _eePactDb ee
  let env = CEKEnv mempty pdb bEnv (_eeDefPactStep ee) False
  resumePact info Mt CEKNoHandler env mdpe >>= \case
    VError stack err i ->
      throwUserRecoverableError' i stack err
    EvalValue v -> do
      case v of
        VPactValue pv -> pure pv
        _ ->
          throwExecutionError info (EvalError "Evaluation did not reduce to a value")



-- evalCEKSmallStep
--   :: Cont CEKSmallStep CoreBuiltin () Eval
--   -> CEKErrorHandler CEKSmallStep CoreBuiltin () Eval
--   -> CEKEnv CEKSmallStep CoreBuiltin () Eval
--   -> CoreTerm
--   -> Eval (CEKReturn CoreBuiltin () Eval)
-- evalCEKSmallStep = evalCEK


-- returnCEKValueSmallStep
--   :: Cont CEKSmallStep CoreBuiltin () Eval
--   -> CEKErrorHandler CEKSmallStep CoreBuiltin () Eval
--   -> CEKValue CEKSmallStep CoreBuiltin () Eval
--   -> Eval (CEKReturn CoreBuiltin () Eval)
-- returnCEKValueSmallStep = returnCEKValue


-- applyContSmallStep
--   :: Cont CEKSmallStep CoreBuiltin () Eval
--   -> CEKErrorHandler CEKSmallStep CoreBuiltin () Eval
--   -> EvalResult CEKSmallStep CoreBuiltin () Eval
--   -> Eval (CEKReturn CoreBuiltin () Eval)
-- applyContSmallStep = returnCEK

-- Keyset Code
isKeysetInSigs
  :: ()
  => SpanInfo
  -> Cont CEKBigStep CoreBuiltin SpanInfo Eval
  -> CEKErrorHandler CEKBigStep CoreBuiltin SpanInfo Eval
  -> CEKEnv CEKBigStep CoreBuiltin SpanInfo Eval
  -> KeySet
  -> Eval (EvalResult CEKBigStep CoreBuiltin SpanInfo Eval)
isKeysetInSigs info cont handler env ks@(KeySet kskeys ksPred) = do
  matchedSigs <- M.filterWithKey matchKey <$> viewEvalEnv eeMsgSigs
  sigs <- checkSigCaps matchedSigs
  runPred (M.size sigs)
  where
  matchKey k _ = k `elem` kskeys
  atLeast t m = m >= t
  count = S.size kskeys
  run p matched =
    if p count matched then returnCEKValue cont handler (VBool True)
    else returnCEKError info cont handler $
      KeysetPredicateFailure ksPred kskeys
  runPred matched =
    case ksPred of
      KeysAll -> run atLeast matched
      KeysAny -> run (\_ m -> atLeast 1 m) matched
      Keys2 -> run (\_ m -> atLeast 2 m) matched
      CustomPredicate n -> runCustomPred matched n
  runCustomPred matched = \case
    TQN qn -> do
      pdb <- viewEvalEnv eePactDb
      getModuleMemberWithHash info pdb qn >>= \case
        (Dfun d, mh) -> do
          clo <- mkDefunClosure d (qualNameToFqn qn mh) env
          let cont' = BuiltinC env info (RunKeysetPredC ks) cont
          applyLam (C clo) [VInteger (fromIntegral count), VInteger (fromIntegral matched)] cont' handler
        _ ->
          throwExecutionError info (InvalidCustomKeysetPredicate "expected defun")
    TBN (BareName bn) -> do
      m <- viewEvalEnv eeNatives
      case M.lookup bn m of
        Just b -> do
          -- let builtins = view ceBuiltins env
          let nativeclo = coreBuiltinEnv info b env
          let cont' = BuiltinC env info (RunKeysetPredC ks) cont
          applyLam (N nativeclo) [VInteger (fromIntegral count), VInteger (fromIntegral matched)] cont' handler
        Nothing ->
          throwExecutionError info (InvalidCustomKeysetPredicate "expected native")

isKeysetNameInSigs
  :: SpanInfo
  -> Cont CEKBigStep CoreBuiltin SpanInfo Eval
  -> CEKErrorHandler CEKBigStep CoreBuiltin SpanInfo Eval
  -> CEKEnv CEKBigStep CoreBuiltin SpanInfo Eval
  -> KeySetName
  -> Eval (EvalResult CEKBigStep CoreBuiltin SpanInfo Eval)
isKeysetNameInSigs info cont handler env ksn = do
  pdb <- viewEvalEnv eePactDb
  liftDbFunction info (readKeySet pdb ksn) >>= \case
    Just ks -> isKeysetInSigs info cont handler env ks
    Nothing ->
      throwExecutionError info (NoSuchKeySet ksn)

----------------------------------------------------------------------
-- Our builtin definitions start here
----------------------------------------------------------------------
unaryIntFn :: (Integer -> Integer) -> NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
unaryIntFn op info b cont handler _env = \case
  [VLiteral (LInteger i)] ->
    returnCEKValue cont handler (VLiteral (LInteger (op i)))
  args -> argsError info b args
{-# INLINE unaryIntFn #-}

binaryIntFn
  :: (Integer -> Integer -> Integer)
  -> NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
binaryIntFn op info b cont handler _env = \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> returnCEKValue cont handler (VLiteral (LInteger (op i i')))
  args -> argsError info b args
{-# INLINE binaryIntFn #-}

-- The majority of the asymptotic cost in here is this function:
-- ```
-- roundTo' :: (Integral i) => (Rational -> i) -> Word8 -> DecimalRaw i -> DecimalRaw i
-- roundTo' _ d (Decimal _  0) = Decimal d 0
-- roundTo' f d (Decimal e n) = Decimal d $ f n1
--    where
--       divisor = 10 ^ (e-d)
--       multiplier = 10 ^ (d-e)
--       n1 = case compare d e of
--          LT -> toRational n / divisor
--          EQ -> toRational n
--          GT -> toRational n * multiplier
-- `roundTo'` thus has the same asymptotic complexity as multiplication/division. Thus, worst case, we can upperbound it via
-- division
roundingFn :: (Rational -> Integer) -> NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
roundingFn op info b cont handler _env = \case
  [VLiteral (LDecimal d)] ->
    returnCEKValue cont handler (VLiteral (LInteger (truncate (roundTo' op 0 d))))
  [VDecimal d, VInteger prec] ->
    returnCEKValue cont handler (VLiteral (LDecimal (roundTo' op (fromIntegral prec) d)))
  args -> argsError info b args
{-# INLINE roundingFn #-}

---------------------------------
-- Arithmetic Ops
------------------------------

rawAdd :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
rawAdd info b cont handler _env = \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> do
    chargeGasArgs info (GIntegerOpCost PrimOpAdd i i')
    returnCEKValue cont handler (VLiteral (LInteger (i + i')))
  -- Overloaded decimal cases
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> do
    decimalAdd i i'
  [VLiteral (LInteger i), VLiteral (LDecimal i')] -> do
    decimalAdd (fromInteger i) i'
  [VLiteral (LDecimal i), VLiteral (LInteger i')] -> do
    decimalAdd i (Decimal 0 i')

  [VLiteral (LString i), VLiteral (LString i')] -> do
    chargeGasArgs info (GConcat (TextConcat (GasTextLength (T.length i + T.length i'))))
    returnCEKValue cont handler  (VLiteral (LString (i <> i')))
  [VObject l, VObject r] -> do
    chargeGasArgs info (GConcat (ObjConcat (M.size l + M.size r)))
    let o' = VObject (l `M.union` r)
    returnCEKValue cont handler o'
  [VList l, VList r] -> do
    chargeGasArgs info (GConcat (ListConcat (GasListLength (V.length l + V.length r))))
    returnCEKValue cont handler (VList (l <> r))
  args -> argsError info b args
  where
  decimalAdd i i' = do
    chargeGasArgs info (GIntegerOpCost PrimOpAdd (decimalMantissa i) (decimalMantissa i'))
    returnCEKValue cont handler (VLiteral (LDecimal (i + i')))

rawSub :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
rawSub info b cont handler _env = \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> do
    chargeGasArgs info (GIntegerOpCost PrimOpSub i i')
    returnCEKValue cont handler (VLiteral (LInteger (i - i')))
  -- Overloaded decimal cases
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] ->
    decimalSub i i'
  [VLiteral (LInteger i), VLiteral (LDecimal i')] ->
    decimalSub (Decimal 0 i) i'
  [VLiteral (LDecimal i), VLiteral (LInteger i')] ->
    decimalSub i (Decimal 0 i')
  args -> argsError info b args
  where
  decimalSub i i' = do
    chargeGasArgs info (GIntegerOpCost PrimOpSub (decimalMantissa i) (decimalMantissa i'))
    returnCEKValue cont handler (VLiteral (LDecimal (i - i')))



rawMul :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
rawMul info b cont handler _env = \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> do
    chargeGasArgs info (GIntegerOpCost PrimOpMul i i')
    returnCEKValue cont handler (VLiteral (LInteger (i * i')))
  -- overloads for decimal multiplication
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] ->
    decimalMul i i'
  [VLiteral (LInteger i), VLiteral (LDecimal i')] ->
    decimalMul (Decimal 0 i) i'
  [VLiteral (LDecimal i), VLiteral (LInteger i')] ->
    decimalMul i (Decimal 0 i')

  args -> argsError info b args
  where
  decimalMul i i' = do
    chargeGasArgs info (GIntegerOpCost PrimOpMul (decimalMantissa i) (decimalMantissa i'))
    returnCEKValue cont handler (VLiteral (LDecimal (i * i')))

rawPow :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
rawPow info b cont handler _env = \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> do
    chargeGasArgs info $ GIntegerOpCost PrimOpPow i i'
    when (i' < 0) $ throwExecutionError info (ArithmeticException "negative exponent in integer power")
    -- Todo: move to iterated pow
    returnCEKValue cont handler (VLiteral (LInteger (i ^ i')))
  [VLiteral (LDecimal l), VLiteral (LDecimal r)] -> do
    decPow l r
  [VLiteral (LInteger i), VLiteral (LDecimal i')] ->
    decPow (Decimal 0 i) i'
  [VLiteral (LDecimal i), VLiteral (LInteger i')] ->
    decPow i (Decimal 0 i')
  args -> argsError info b args
  where
  decPow l r = do
    let result = Musl.trans_pow (dec2F l) (dec2F r)
    guardNanOrInf info result
    returnCEKValue cont handler (VLiteral (LDecimal (f2Dec result)))

rawLogBase :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
rawLogBase info b cont handler _env = \case
  [VLiteral (LInteger base), VLiteral (LInteger n)] -> do
    checkArgs base n
    let base' = fromIntegral base :: Double
        n' = fromIntegral n
        result = Musl.trans_logBase base' n'
    guardNanOrInf info result
    returnCEKValue cont handler (VLiteral (LInteger (round result)))
  [VLiteral (LDecimal base), VLiteral (LDecimal arg)] -> do
     decLogBase base arg
  [VLiteral (LInteger base), VLiteral (LDecimal arg)] -> do
     decLogBase (Decimal 0 base) arg
  [VLiteral (LDecimal base), VLiteral (LInteger arg)] -> do
     decLogBase base (Decimal 0 arg)
  args -> argsError info b args
  where
  decLogBase base arg = do
    checkArgs base arg
    let result = Musl.trans_logBase (dec2F base) (dec2F arg)
    guardNanOrInf info result
    returnCEKValue cont handler (VLiteral (LDecimal (f2Dec result)))
  checkArgs :: (Num a, Ord a) => a -> a -> Eval ()
  checkArgs base arg = do
    when (base < 0) $ throwExecutionError info (ArithmeticException "Negative log base")
    when (arg <= 0) $ throwExecutionError info (ArithmeticException "Non-positive log argument")


rawDiv :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
rawDiv info b cont handler _env = \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> do
    when (i' == 0) $ throwExecutionError info (ArithmeticException "div by zero")
    chargeGasArgs info (GIntegerOpCost PrimOpDiv i i')
    returnCEKValue cont handler (VLiteral (LInteger (div i i')))

  [VLiteral (LInteger i), VLiteral (LDecimal i')] ->
    decimalDiv (Decimal 0 i) i'
  [VLiteral (LDecimal i), VLiteral (LInteger i')] ->
    decimalDiv i (Decimal 0 i')
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] ->
    decimalDiv i i'

  args -> argsError info b args
  where
  decimalDiv i i' = do
    when (i' == 0) $ throwExecutionError info (ArithmeticException "div by zero, decimal")
    chargeGasArgs info (GIntegerOpCost PrimOpDiv (decimalMantissa i) (decimalMantissa i'))
    returnCEKValue cont handler (VLiteral (LDecimal (i / i')))


rawNegate :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
rawNegate info b cont handler _env = \case
  [VLiteral (LInteger i)] ->
    returnCEKValue cont handler (VLiteral (LInteger (negate i)))
  [VLiteral (LDecimal i)] ->
    returnCEKValue cont handler (VLiteral (LDecimal (negate i)))
  args -> argsError info b args

rawEq :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
rawEq info b cont handler _env = \case
  [VPactValue pv, VPactValue pv'] -> do
    isEq <- valEqGassed info pv pv'
    returnCEKValue cont handler (VBool isEq)
  args -> argsError info b args

modInt :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
modInt = binaryIntFn mod

rawNeq :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
rawNeq info b cont handler _env = \case
  [VPactValue pv, VPactValue pv'] -> do
    isEq <- valEqGassed info pv pv'
    returnCEKValue cont handler (VBool $ not isEq)
  args -> argsError info b args

rawGt :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
rawGt = defCmp (== GT)

rawLt :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
rawLt = defCmp (== LT)

rawGeq :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
rawGeq = defCmp (`elem` [GT, EQ])

rawLeq :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
rawLeq = defCmp (`elem` [LT, EQ])

defCmp :: () => (Ordering -> Bool) -> NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
defCmp predicate info b cont handler _env = \case
  args@[VLiteral lit1, VLiteral lit2] -> litCmpGassed info lit1 lit2 >>= \case
    Just ordering -> returnCEKValue cont handler $ VBool $ predicate ordering
    Nothing -> argsError info b args
  -- Todo: time comparisons
  [VTime l, VTime r] -> returnCEKValue cont handler $ VBool $ predicate (compare l r)
  args -> argsError info b args
{-# INLINE defCmp #-}

bitAndInt :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
bitAndInt = binaryIntFn (.&.)

bitOrInt :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
bitOrInt = binaryIntFn (.|.)

bitComplementInt :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
bitComplementInt = unaryIntFn complement

bitXorInt :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
bitXorInt = binaryIntFn xor

bitShiftInt :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
bitShiftInt info b cont handler _env = \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> do
    chargeGasArgs info $ GIntegerOpCost PrimOpShift i i'
    returnCEKValue cont handler (VLiteral (LInteger (i `shift` fromIntegral i')))
  args -> argsError info b args

rawAbs :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
rawAbs info b cont handler _env = \case
  [VLiteral (LInteger i)] -> do
    returnCEKValue cont handler (VLiteral (LInteger (abs i)))
  [VLiteral (LDecimal e)] -> do
    returnCEKValue cont handler (VLiteral (LDecimal (abs e)))
  args -> argsError info b args

rawExp :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
rawExp info b cont handler _env = \case
  [VLiteral (LInteger i)] -> do
    let result = Musl.trans_exp (fromIntegral i)
    guardNanOrInf info result
    returnCEKValue cont handler (VLiteral (LDecimal (f2Dec result)))
  [VLiteral (LDecimal e)] -> do
    let result = Musl.trans_exp (dec2F e)
    guardNanOrInf info result
    returnCEKValue cont handler (VLiteral (LDecimal (f2Dec result)))
  args -> argsError info b args

rawLn :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
rawLn info b cont handler _env = \case
  [VLiteral (LInteger i)] -> do
    let result = Musl.trans_ln (fromIntegral i)
    guardNanOrInf info result
    returnCEKValue cont handler (VLiteral (LDecimal (f2Dec result)))
  [VLiteral (LDecimal e)] -> do
    let result = Musl.trans_ln (dec2F e)
    guardNanOrInf info result
    returnCEKValue cont handler (VLiteral (LDecimal (f2Dec result)))
  args -> argsError info b args

rawSqrt :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
rawSqrt info b cont handler _env = \case
  [VLiteral (LInteger i)] -> do
    when (i < 0) $ throwExecutionError info (ArithmeticException "Square root must be non-negative")
    let result = Musl.trans_sqrt (fromIntegral i)
    guardNanOrInf info result
    returnCEKValue cont handler (VLiteral (LDecimal (f2Dec result)))
  [VLiteral (LDecimal e)] -> do
    when (e < 0) $ throwExecutionError info (ArithmeticException "Square root must be non-negative")
    let result = Musl.trans_sqrt (dec2F e)
    guardNanOrInf info result
    returnCEKValue cont handler (VLiteral (LDecimal (f2Dec result)))
  args -> argsError info b args

-- Todo: fix all show instances
rawShow :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
rawShow info b cont handler _env = \case
  [VLiteral (LInteger i)] -> do
    let strLen = 1 + Exts.I# (IntLog.integerLog2# $ abs i)
    chargeGasArgs info $ GConcat $ TextConcat $ GasTextLength $ fromIntegral strLen
    returnCEKValue cont handler (VLiteral (LString (T.pack (show i))))
  [VLiteral (LDecimal i)] -> do
    let strLen = 1 + Exts.I# (IntLog.integerLog2# $ abs $ decimalMantissa i)
    chargeGasArgs info $ GConcat $ TextConcat $ GasTextLength $ fromIntegral strLen
    returnCEKValue cont handler (VLiteral (LString (T.pack (show i))))
  [VLiteral (LString i)] -> do
    chargeGasArgs info $ GConcat $ TextConcat $ GasTextLength $ T.length i
    returnCEKValue cont handler (VLiteral (LString (T.pack (show i))))
  [VLiteral (LBool i)] ->
    returnCEKValue cont handler (VLiteral (LString (T.pack (show i))))
  [VLiteral LUnit] ->
    returnCEKValue cont handler (VLiteral (LString "()"))
  args -> argsError info b args

-- Todo: Gas here is complicated, greg worked on this previously
rawContains :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
rawContains info b cont handler _env = \case
  [VString f, VObject o] -> do
    chargeGasArgs info $ GSearch $ FieldSearch (M.size o)
    returnCEKValue cont handler (VBool (M.member (Field f) o))
  [VString needle, VString hay] -> do
    chargeGasArgs info $ GSearch $ SubstringSearch needle hay
    returnCEKValue cont handler (VBool (needle `T.isInfixOf` hay))
  [VPactValue v, VList vli] -> do
    let search True _ = pure True
        search _ el = valEqGassed info v el
    res <- foldlM search False vli
    returnCEKValue cont handler (VBool res)
  args -> argsError info b args

rawSort :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
rawSort info b cont handler _env = \case
  [VList vli]
    | V.null vli -> returnCEKValue cont handler (VList mempty)
    | otherwise -> do
    vli' <- liftIO $ do
      v' <- V.thaw vli
      V.sort v'
      V.freeze v'
    returnCEKValue cont handler (VList vli')
  args -> argsError info b args

coreRemove :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
coreRemove info b cont handler _env = \case
  [VString s, VObject o] -> do
    chargeGasArgs info $ GObjOp $ ObjOpRemove s (M.size o)
    returnCEKValue cont handler (VObject (M.delete (Field s) o))
  args -> argsError info b args

asObject
  :: SpanInfo
  -> CoreBuiltin
  -> PactValue
  -> Eval (M.Map Field PactValue)
asObject info b = \case
  PObject o -> pure o
  arg -> argsError info b [VPactValue arg]

rawSortObject :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
rawSortObject info b cont handler _env = \case
  [VList fields, VList objs]
    | V.null fields -> returnCEKValue cont handler (VList objs)
    | V.null objs -> returnCEKValue cont handler (VList objs)
    | otherwise -> do
        objs' <- traverse (asObject info b) objs
        fields' <- traverse (fmap Field . asString info b) fields
        v' <- liftIO $ do
          mobjs <- V.thaw objs'
          V.sortBy (sort fields') mobjs
          V.freeze mobjs
        returnCEKValue cont handler (VList (PObject <$> v'))
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

guardNanOrInf :: MonadEval b i m => i -> Double -> m ()
guardNanOrInf info a =
  when (isNaN a || isInfinite a) $ throwExecutionError info (FloatingPointError "Floating operation resulted in Infinity or NaN")

dec2F :: Decimal -> Double
dec2F = fromRational . toRational

f2Dec :: Double -> Decimal
f2Dec = fromRational . toRational

roundDec :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
roundDec = roundingFn round

floorDec :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
floorDec = roundingFn floor

ceilingDec :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
ceilingDec = roundingFn ceiling

---------------------------
-- bool ops
---------------------------
notBool :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
notBool info b cont handler _env = \case
  [VLiteral (LBool i)] -> returnCEKValue cont handler  (VLiteral (LBool (not i)))
  args -> argsError info b args

---------------------------
-- string ops
---------------------------

-- Note: [Take/Drop Clamping]
-- Take an expression like one of the following:
--  When i >= 0:
--    let clamp = fromIntegral $ min i (fromIntegral (T.length t))
--  When i < 0:
--    let clamp = fromIntegral $ max (fromIntegral (T.length t) + i) 0
--
-- Note that it's `max (fromIntegral (T.length t) + i) 0` and not `max (T.length t + fromIntegral i) 0`.
-- That's because `i` may contain values larger than `Int`, which is the type `length` typically returns.
-- The sum `i + length t` may overflow `Int`, so it's converted to `Integer`, and the result of the `clamp` is always
-- below `maxBound :: Int`, so it can be safely casted back without overflow.
rawTake :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
rawTake info b cont handler _env = \case
  [VLiteral (LInteger i), VLiteral (LString t)]
    | i >= 0 -> do
      -- See Note: [Take/Drop Clamping]
      let clamp = fromIntegral $ min i (fromIntegral (T.length t))
      chargeGasArgs info $ GConcat $ TextConcat $ GasTextLength clamp
      returnCEKValue cont handler  (VLiteral (LString (T.take clamp t)))
    | otherwise -> do
      -- See Note: [Take/Drop Clamping]
      let clamp = fromIntegral $ max (fromIntegral (T.length t) + i) 0
      chargeGasArgs info $ GConcat $ TextConcat $ GasTextLength $ fromIntegral $ negate i
      returnCEKValue cont handler  (VLiteral (LString (T.drop clamp t)))
  [VLiteral (LInteger i), VList li]
    | i >= 0 -> do
      -- See Note: [Take/Drop Clamping]
      let clamp = fromIntegral $ min i (fromIntegral (V.length li))
      chargeGasArgs info $ GConcat $ ListConcat $ GasListLength clamp
      returnCEKValue cont handler  (VList (V.take clamp li))
    | otherwise -> do
      -- See Note: [Take/Drop Clamping]
      let clamp = fromIntegral $ max (fromIntegral (V.length li) + i) 0
      chargeGasArgs info $ GConcat $ ListConcat $ GasListLength $ fromIntegral $ negate i
      returnCEKValue cont handler (VList (V.drop clamp li))
  [VList li, VObject o] -> do
    strings <- traverse (fmap Field . asString info b) (V.toList li)
    chargeGasArgs info $ GConcat $ ObjConcat $ V.length li
    returnCEKValue cont handler $ VObject $ M.restrictKeys o (S.fromList strings)
  args -> argsError info b args

rawDrop :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
rawDrop info b cont handler _env = \case
  [VLiteral (LInteger i), VLiteral (LString t)]
    | i >= 0 -> do
      -- See Note: [Take/Drop Clamping]
      let clamp = fromIntegral $ min i (fromIntegral (T.length t))
      returnCEKValue cont handler  (VLiteral (LString (T.drop clamp t)))
    | otherwise -> do
      -- See Note: [Take/Drop Clamping]
      let clamp = fromIntegral $ max (fromIntegral (T.length t) + i) 0
      returnCEKValue cont handler  (VLiteral (LString (T.take clamp t)))
  [VLiteral (LInteger i), VList li]
    | i >= 0 -> do
      -- See Note: [Take/Drop Clamping]
      let clamp = fromIntegral $ min i (fromIntegral (V.length li))
      returnCEKValue cont handler  (VList (V.drop clamp li))
    | otherwise -> do
      -- See Note: [Take/Drop Clamping]
      let clamp = fromIntegral $ max (fromIntegral (V.length li) + i) 0
      returnCEKValue cont handler (VList (V.take clamp li))
  [VList li, VObject o] -> do
    strings <- traverse (fmap Field . asString info b) (V.toList li)
    returnCEKValue cont handler $ VObject $ M.withoutKeys o (S.fromList strings)
  args -> argsError info b args

rawLength :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
rawLength info b cont handler _env = \case
  [VString t] -> do
    chargeGasArgs info $ GStrOp $ StrOpLength $ T.length t
    returnCEKValue cont handler  (VLiteral (LInteger (fromIntegral (T.length t))))
  [VList li] -> returnCEKValue cont handler (VLiteral (LInteger (fromIntegral (V.length li))))
  [VObject o] ->
    returnCEKValue cont handler $ VInteger $ fromIntegral (M.size o)
  args -> argsError info b args

rawReverse :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
rawReverse info b cont handler _env = \case
  [VList li] -> do
    chargeGasArgs info (GConcat (ListConcat (GasListLength (V.length li))))
    returnCEKValue cont handler (VList (V.reverse li))
  [VLiteral (LString t)] -> do
    chargeGasArgs info (GConcat (TextConcat (GasTextLength (T.length t))))
    returnCEKValue cont handler  (VLiteral (LString (T.reverse t)))
  args -> argsError info b args

coreConcat :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
coreConcat info b cont handler _env = \case
  [VList li]
    | V.null li -> returnCEKValue cont handler (VString mempty)
    | otherwise -> do
    li' <- traverse (asString info b) li
    let totalLen = sum $ T.length <$> li'
    chargeGasArgs info (GConcat (TextListConcat (GasTextLength totalLen) (GasListLength (V.length li))))
    returnCEKValue cont handler (VString (T.concat (V.toList li')))
  args -> argsError info b args

strToList :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
strToList info b cont handler _env = \case
  [VLiteral (LString s)] -> do
    chargeGasArgs info $ GStrOp $ StrOpExplode $ T.length s
    let v = VList (V.fromList (PLiteral . LString . T.singleton <$> T.unpack s))
    returnCEKValue cont handler v
  args -> argsError info b args


zipList :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
zipList info b cont handler _env = \case
  [VClosure clo, VList l, VList r] ->
    case (V.toList l, V.toList r) of
      (x:xs, y:ys) -> do
        chargeGasArgs info (GAConstant unconsWorkNodeGas)
        let cont' = BuiltinC _env info (ZipC clo (xs, ys) []) cont
        applyLam clo [VPactValue x, VPactValue y] cont' handler
      (_, _) -> returnCEKValue cont handler (VList mempty)
  args -> argsError info b args

coreMap :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
coreMap info b cont handler env = \case
  [VClosure clo, VList li] -> case V.toList li of
    x:xs -> do
      let cont' = BuiltinC env info (MapC clo xs []) cont
      chargeGasArgs info (GAConstant unconsWorkNodeGas)
      applyLam clo [VPactValue x] cont' handler
    [] -> returnCEKValue cont handler (VList mempty)
  args -> argsError info b args

coreFilter :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
coreFilter info b cont handler _env = \case
  [VClosure clo, VList li] -> case V.toList li of
    x:xs -> do
      chargeGasArgs info (GAConstant unconsWorkNodeGas)
      let cont' = CondC _env info (FilterC clo x xs []) cont
      applyLam clo [VPactValue x] cont' handler
    [] -> returnCEKValue cont handler (VList mempty)
  args -> argsError info b args

coreFold :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
coreFold info b cont handler _env = \case
  [VClosure clo, VPactValue initElem, VList li] ->
    case V.toList li of
      x:xs -> do
        chargeGasArgs info (GAConstant unconsWorkNodeGas)
        let cont' = BuiltinC _env info (FoldC clo xs) cont
        applyLam clo [VPactValue initElem, VPactValue x] cont' handler
      [] -> returnCEKValue cont handler (VPactValue initElem)
  args -> argsError info b args

coreEnumerate :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
coreEnumerate info b cont handler _env = \case
  [VLiteral (LInteger from), VLiteral (LInteger to)] -> do
    v <- createEnumerateList info from to (if from > to then -1 else 1)
    returnCEKValue cont handler (VList (PLiteral . LInteger <$> v))
  args -> argsError info b args

createEnumerateList
  :: (MonadEval b i m)
  => i
  -> Integer
  -- ^ from
  -> Integer
  -- ^ to
  -> Integer
  -- ^ Step
  -> m (Vector Integer)
createEnumerateList info from to inc
  | from == to = do
    fromSize <- sizeOf SizeOfV0 from
    chargeGasArgs info (GMakeList 1 fromSize)
    pure (V.singleton from)
  | inc == 0 = pure mempty -- note: covered by the flat cost
  | from < to, from + inc < from =
    throwExecutionError info (EnumerationError "enumerate: increment diverges below from interval bounds.")
  | from > to, from + inc > from =
    throwExecutionError info (EnumerationError "enumerate: increment diverges above from interval bounds.")
  | otherwise = do
    let len = succ (abs (from - to) `div` abs inc)
    listSize <- sizeOf SizeOfV0 (max (abs from) (abs to))
    chargeGasArgs info (GMakeList len listSize)
    pure $ V.enumFromStepN from inc (fromIntegral len)

coreEnumerateStepN :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
coreEnumerateStepN info b cont handler _env = \case
  [VLiteral (LInteger from), VLiteral (LInteger to), VLiteral (LInteger inc)] -> do
    v <- createEnumerateList info from to inc
    returnCEKValue cont handler (VList (PLiteral . LInteger <$> v))
  args -> argsError info b args

makeList :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
makeList info b cont handler _env = \case
  [VLiteral (LInteger i), VPactValue v] -> do
    vSize <- sizeOf SizeOfV0 v
    chargeGasArgs info (GMakeList (fromIntegral i) vSize)
    returnCEKValue cont handler (VList (V.fromList (replicate (fromIntegral i) v)))
  args -> argsError info b args

coreAccess :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
coreAccess info b cont handler _env = \case
  [VLiteral (LInteger i), VList vec] ->
    case vec V.!? fromIntegral i of
      Just v -> returnCEKValue cont handler (VPactValue v)
      -- Note: this error is not recoverable in prod
      _ -> throwExecutionError info (ArrayOutOfBoundsException (V.length vec) (fromIntegral i))
  [VString field, VObject o] ->
    case M.lookup (Field field) o of
      Just v -> returnCEKValue cont handler (VPactValue v)
      Nothing ->
        -- Note: this error is not recoverable in prod
        throwExecutionError info (ObjectIsMissingField (Field field) (ObjectData o))
  args -> argsError info b args

coreIsCharset :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
coreIsCharset info b cont handler _env = \case
  [VLiteral (LInteger i), VString s] -> do
    chargeGasArgs info $ GStrOp $ StrOpParse $ T.length s
    case i of
      0 -> returnCEKValue cont handler $ VBool $ T.all Char.isAscii s
      1 -> returnCEKValue cont handler $ VBool $ T.all Char.isLatin1 s
      _ ->
        throwNativeExecutionError info b "Unsupported character set"
  args -> argsError info b args

coreYield :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
coreYield info b cont handler _env = \case
  [VObject o] -> go o Nothing
  [VObject o, VString cid] -> go o (Just (ChainId cid))
  args -> argsError info b args
  where
  go o mcid = do
    mpe <- useEvalState esDefPactExec
    case mpe of
      Nothing -> throwExecutionError info YieldOutsideDefPact
      Just pe -> case mcid of
        Nothing -> do
          esDefPactExec . _Just . peYield .== Just (Yield o Nothing Nothing)
          returnCEKValue cont handler (VObject o)
        Just cid -> do
          sourceChain <- viewEvalEnv (eePublicData . pdPublicMeta . pmChainId)
          p <- provenanceOf cid
          when (_peStepHasRollback pe) $ throwExecutionError info $ EvalError "Cross-chain yield not allowed in step with rollback"
          esDefPactExec . _Just . peYield .== Just (Yield o (Just p) (Just sourceChain))
          returnCEKValue cont handler (VObject o)
  provenanceOf tid =
    Provenance tid . _mHash <$> getCallingModule info

corePactId :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
corePactId info b cont handler _env = \case
  [] -> useEvalState esDefPactExec >>= \case
    Just dpe -> returnCEKValue cont handler (VString (_defpactId (_peDefPactId dpe)))
    Nothing ->
      throwExecutionError info NotInDefPactExecution
  args -> argsError info b args

enforceYield
  :: (MonadEval b i m)
  => i
  -> Yield
  -> m ()
enforceYield info y = case _yProvenance y of
  Nothing -> pure ()
  Just p -> do
    m <- getCallingModule info
    cid <- viewEvalEnv $ eePublicData . pdPublicMeta . pmChainId
    let p' = Provenance cid (_mHash m):map (Provenance cid) (toList $ _mBlessed m)
    unless (p `elem` p') $ throwExecutionError info (YieldProvenanceDoesNotMatch p p')

coreResume :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
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

enforceTopLevelOnly :: (MonadEval b i m) => i -> b -> m ()
enforceTopLevelOnly info b = do
  s <- useEvalState esStack
  unless (null s) $ throwExecutionError info (NativeIsTopLevelOnly (builtinName b))

-----------------------------------
-- Guards and reads
-----------------------------------


-----------------------------------
-- Other Core forms
-----------------------------------

coreB64Encode :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
coreB64Encode info b cont handler _env = \case
  [VLiteral (LString l)] -> do
    chargeGasArgs info $ GStrOp $ StrOpParse $ T.length l
    returnCEKValue cont handler $ VLiteral $ LString $ toB64UrlUnpaddedText $ T.encodeUtf8 l
  args -> argsError info b args


coreB64Decode :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
coreB64Decode info b cont handler _env = \case
  [VLiteral (LString s)] -> do
    chargeGasArgs info $ GStrOp $ StrOpParse $ T.length s
    case fromB64UrlUnpaddedText $ T.encodeUtf8 s of
      Left{} -> throwExecutionError info (DecodeError "invalid b64 encoding")
      Right txt -> returnCEKValue cont handler (VLiteral (LString txt))
  args -> argsError info b args


-- | The implementation of `enforce-guard` native.
coreEnforceGuard :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
coreEnforceGuard info b cont handler env = \case
  [VGuard g] -> enforceGuard info cont handler env g
  [VString s] -> do
    chargeGasArgs info $ GStrOp $ StrOpParse $ T.length s
    case parseAnyKeysetName s of
      Left {} ->
        throwNativeExecutionError info b "incorrect keyset name format"
      Right ksn -> isKeysetNameInSigs info cont handler env ksn
  args -> argsError info b args

keysetRefGuard :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
keysetRefGuard info b cont handler env = \case
  [VString g] -> do
    chargeGasArgs info $ GStrOp $ StrOpParse $ T.length g
    case parseAnyKeysetName g of
      Left {} -> throwNativeExecutionError info b "incorrect keyset name format"
      Right ksn -> do
        let pdb = view cePactDb env
        liftDbFunction info (readKeySet pdb ksn) >>= \case
          Nothing ->
            throwExecutionError info (NoSuchKeySet ksn)
          Just _ -> returnCEKValue cont handler (VGuard (GKeySetRef ksn))
  args -> argsError info b args

coreTypeOf :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
coreTypeOf info b cont handler _env = \case
  [v] -> case v of
    VPactValue pv ->
      returnCEKValue cont handler $ VString $ renderType $ synthesizePvType pv
    VClosure _ -> returnCEKValue cont handler $ VString "<<closure>>"
    VTable tv -> returnCEKValue cont handler $ VString (renderType (TyTable (_tvSchema tv)))
  args -> argsError info b args

coreDec :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
coreDec info b cont handler _env = \case
  [VInteger i] -> returnCEKValue cont handler $ VDecimal $ Decimal 0 i
  args -> argsError info b args

throwReadError
  :: ()
  => SpanInfo
  -> Cont CEKBigStep CoreBuiltin SpanInfo Eval
  -> CEKErrorHandler CEKBigStep CoreBuiltin SpanInfo Eval
  -> CoreBuiltin
  -> Eval (EvalResult CEKBigStep CoreBuiltin SpanInfo Eval)
throwReadError info cont handler b =
  returnCEKError info cont handler $ EnvReadFunctionFailure (builtinName b)

{-
[Note: Parsed Integer]
`read-integer` corresponds to prod's `ParsedInteger` newtype. That handles, in particular, the following codecs:

instance FromJSON Literal where
  parseJSON n@Number{} = LDecimal <$> decoder decimalCodec n
  parseJSON (String s) = pure $ LString s
  parseJSON (Bool b) = pure $ LBool b
  parseJSON o@Object {} =
    (LInteger <$> decoder integerCodec o) <|>
    (LTime <$> decoder timeCodec o) <|>
    (LDecimal <$> decoder decimalCodec o)
  parseJSON _t = fail "Literal parse failed"

instance A.FromJSON ParsedInteger where
  parseJSON (A.String s) =
    ParsedInteger <$> case pactAttoParseOnly (unPactParser number) s of
                        Right (LInteger i) -> return i
                        _ -> fail $ "Failure parsing integer string: " ++ show s
  parseJSON (A.Number n) = return $ ParsedInteger (round n)
  parseJSON v@A.Object{} = A.parseJSON v >>= \i -> case i of
    PLiteral (LInteger li) -> return $ ParsedInteger li
    _ -> fail $ "Failure parsing integer PactValue object: " ++ show i
  parseJSON v = fail $ "Failure parsing integer: " ++ show v

In prod, env data is just a json object. In core, we parse eagerly using the `PactValue` parser, so the following
can happen:
  - We may see a PString, we must run the number parser `parseNumLiteral`
  - We may see a PDecimal, in which case we round
  - We may see a PInteger, which we read as-is.
-}
coreReadInteger :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
coreReadInteger info b cont handler _env = \case
  [VString s] -> do
    viewEvalEnv eeMsgBody >>= \case
      PObject envData -> do
        chargeGasArgs info $ GObjOp $ ObjOpLookup s $ M.size envData
        case M.lookup (Field s) envData of
          -- See [Note: Parsed Integer]
          Just (PDecimal p) ->
            returnCEKValue cont handler (VInteger (round p))
          Just (PInteger p) ->
            returnCEKValue cont handler (VInteger p)
          -- See [Note: Parsed Integer]
          Just (PString raw) -> do
            chargeGasArgs info $ GStrOp $ StrOpConvToInt $ T.length raw
            case parseNumLiteral raw of
              Just (LInteger i) -> returnCEKValue cont handler (VInteger i)
              _ -> throwReadError info cont handler b
          _ -> throwReadError info cont handler b
      _ -> throwReadError info cont handler b
  args -> argsError info b args

coreReadMsg :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
coreReadMsg info b cont handler _env = \case
  [VString s] -> do
    viewEvalEnv eeMsgBody >>= \case
      PObject envData -> do
        chargeGasArgs info $ GObjOp $ ObjOpLookup s $ M.size envData
        case M.lookup (Field s) envData of
          Just pv -> returnCEKValue cont handler (VPactValue pv)
          _ -> throwReadError info cont handler b
      _ -> throwReadError info cont handler b
  [] -> do
    envData <- viewEvalEnv eeMsgBody
    returnCEKValue cont handler (VPactValue envData)
  args -> argsError info b args

{-
[Note: Parsed Decimal]

Simlar to [Note: Parsed Integer], except the decimal case handles:

instance A.FromJSON ParsedDecimal where
  parseJSON (A.String s) =
    ParsedDecimal <$> case pactAttoParseOnly (unPactParser number) s of
                        Right (LDecimal d) -> return d
                        Right (LInteger i) -> return (fromIntegral i)
                        _ -> fail $ "Failure parsing decimal string: " ++ show s
  parseJSON (A.Number n) = return $ ParsedDecimal (fromRational $ toRational n)
  parseJSON v = fail $ "Failure parsing decimal: " ++ show v

So the string parsing case accepts both the integer, and decimal output
-}
coreReadDecimal :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
coreReadDecimal info b cont handler _env = \case
  [VString s] -> do
    viewEvalEnv eeMsgBody >>= \case
      PObject envData -> do
        chargeGasArgs info $ GObjOp $ ObjOpLookup s $ M.size envData
        case M.lookup (Field s) envData of
          Just (PDecimal p) -> returnCEKValue cont handler (VDecimal p)
          -- See [Note: Parsed Decimal]
          Just (PInteger i) -> returnCEKValue cont handler (VDecimal (Decimal 0 i))
          Just (PString raw) -> do
            chargeGasArgs info $ GStrOp $ StrOpConvToInt $ T.length raw
            case parseNumLiteral raw of
              Just (LInteger i) -> returnCEKValue cont handler (VDecimal (Decimal 0 i))
              Just (LDecimal l) -> returnCEKValue cont handler (VDecimal l)
              _ -> throwReadError info cont handler b
          _ -> throwReadError info cont handler b
      _ -> throwReadError info cont handler b
  args -> argsError info b args


coreReadString :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
coreReadString info b cont handler _env = \case
  [VString s] -> do
    viewEvalEnv eeMsgBody >>= \case
      PObject envData -> do
        chargeGasArgs info $ GObjOp $ ObjOpLookup s $ M.size envData
        case M.lookup (Field s) envData of
          Just (PString p) -> returnCEKValue cont handler (VString p)
          _ -> throwReadError info cont handler b
      _ -> throwReadError info cont handler b
  args -> argsError info b args

readKeyset' :: (MonadEval b i m) => i -> T.Text -> m (Maybe KeySet)
readKeyset' info ksn = do
  viewEvalEnv eeMsgBody >>= \case
    PObject envData -> do
      chargeGasArgs info $ GObjOp $ ObjOpLookup ksn $ M.size envData
      case M.lookup (Field ksn) envData of
        Just (PGuard (GKeyset ks)) -> pure (Just ks)
        Just (PObject dat) -> do
          chargeGasArgs info $ GObjOp $ ObjOpLookup "keys" objSize
          chargeGasArgs info $ GObjOp $ ObjOpLookup "pred" objSize
          case parseObj dat of
            Nothing -> pure Nothing
            Just (ks, p) -> do
              chargeGasArgs info $ GStrOp $ StrOpParse $ T.length p
              pure $ KeySet ks <$> readPredicate p
          where
          objSize = M.size dat
          parseObj d = do
            keys <- M.lookup (Field "keys") d
            keyText <- preview _PList keys >>= traverse (fmap PublicKeyText . preview (_PLiteral . _LString))
            predRaw <- M.lookup (Field "pred") d
            p <- preview (_PLiteral . _LString) predRaw
            let ks = S.fromList (V.toList keyText)
            pure (ks, p)
          readPredicate = \case
            "keys-any" -> pure KeysAny
            "keys-2" -> pure Keys2
            "keys-all" -> pure KeysAll
            n | Just pn <- parseParsedTyName n -> pure (CustomPredicate pn)
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


coreReadKeyset :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
coreReadKeyset info b cont handler _env = \case
  [VString ksn] ->
    readKeyset' info ksn >>= \case
      Just ks -> do
        shouldEnforce <- isExecutionFlagSet FlagEnforceKeyFormats
        if shouldEnforce && isLeft (enforceKeyFormats (const ()) ks)
           then
            throwExecutionError info (InvalidKeysetFormat ks)
           else returnCEKValue cont handler (VGuard (GKeyset ks))
      Nothing -> throwReadError info cont handler b
  args -> argsError info b args


coreBind :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
coreBind info b cont handler _env = \case
  [v@VObject{}, VClosure clo] ->
    applyLam clo [v] cont handler
  args -> argsError info b args


--------------------------------------------------
-- Db functions
--------------------------------------------------

createTable :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
createTable info b cont handler env = \case
  [VTable tv] -> do
    enforceTopLevelOnly info b
    let cont' = BuiltinC env info (CreateTableC tv) cont
    guardTable info cont' handler env tv GtCreateTable
  args -> argsError info b args

dbSelect :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
dbSelect info b cont handler env = \case
  [VTable tv, VClosure clo] -> do
    let cont' = BuiltinC env info (PreSelectC tv clo Nothing) cont
    guardTable info cont' handler env tv GtSelect
  [VTable tv, VList li, VClosure clo] -> do
    li' <- traverse (fmap Field . asString info b) (V.toList li)
    let cont' = BuiltinC env info (PreSelectC tv clo (Just li')) cont
    guardTable info cont' handler env tv GtSelect
  args -> argsError info b args

foldDb :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
foldDb info b cont handler env = \case
  [VTable tv, VClosure queryClo, VClosure consumer] -> do
    let cont' = BuiltinC env info (PreFoldDbC tv queryClo consumer) cont
    guardTable info cont' handler env tv GtSelect
  args -> argsError info b args

dbRead :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
dbRead info b cont handler env = \case
  [VTable tv, VString k] -> do
    let cont' = BuiltinC env info (ReadC tv (RowKey k)) cont
    guardTable info cont' handler env tv GtRead
  args -> argsError info b args

dbWithRead :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
dbWithRead info b cont handler env = \case
  [VTable tv, VString k, VClosure clo] -> do
    let cont1 = Fn clo env [] [] cont
    let cont2 = BuiltinC env info (ReadC tv (RowKey k)) cont1
    guardTable info cont2 handler env tv GtWithRead
  args -> argsError info b args

dbWithDefaultRead :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
dbWithDefaultRead info b cont handler env = \case
  [VTable tv, VString k, VObject defaultObj, VClosure clo] -> do
    let cont' = BuiltinC env info (WithDefaultReadC tv (RowKey k) (ObjectData defaultObj) clo) cont
    guardTable info cont' handler env tv GtWithDefaultRead
  args -> argsError info b args

-- | Todo: schema checking here? Or only on writes?
dbWrite :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
dbWrite = write' Write

dbInsert :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
dbInsert = write' Insert

write' :: WriteType -> NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
write' wt info b cont handler env = \case
  [VTable tv, VString key, VObject o] -> do
    let cont' = BuiltinC env info (WriteC tv wt (RowKey key) (ObjectData o)) cont
    guardTable info cont' handler env tv GtWrite
  args -> argsError info b args

dbUpdate :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
dbUpdate = write' Update

dbKeys :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
dbKeys info b cont handler env = \case
  [VTable tv] -> do
    let cont' = BuiltinC env info (KeysC tv) cont
    guardTable info cont' handler env tv GtKeys
  args -> argsError info b args

dbTxIds :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
dbTxIds info b cont handler env = \case
  [VTable tv, VInteger tid] -> do
    checkNonLocalAllowed info b
    let cont' = BuiltinC env info (TxIdsC tv tid) cont
    guardTable info cont' handler env tv GtTxIds
  args -> argsError info b args


dbTxLog :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
dbTxLog info b cont handler env = \case
  [VTable tv, VInteger tid] -> do
    checkNonLocalAllowed info b
    let cont' = BuiltinC env info (TxLogC tv tid) cont
    guardTable info cont' handler env tv GtTxLog
  args -> argsError info b args

dbKeyLog :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
dbKeyLog info b cont handler env = \case
  [VTable tv, VString key, VInteger tid] -> do
    checkNonLocalAllowed info b
    let cont' = BuiltinC env info (KeyLogC tv (RowKey key) tid) cont
    guardTable info cont' handler env tv GtKeyLog
  args -> argsError info b args

defineKeySet'
  :: ()
  => SpanInfo
  -> Cont CEKBigStep CoreBuiltin SpanInfo Eval
  -> CEKErrorHandler CEKBigStep CoreBuiltin SpanInfo Eval
  -> CEKEnv CEKBigStep CoreBuiltin SpanInfo Eval
  -> T.Text
  -> KeySet
  -> Eval (EvalResult CEKBigStep CoreBuiltin SpanInfo Eval)
defineKeySet' info cont handler env ksname newKs  = do
  let pdb = view cePactDb env
  ignoreNamespaces <- not <$> isExecutionFlagSet FlagRequireKeysetNs
  case parseAnyKeysetName ksname of
    Left {} ->
      throwExecutionError info (InvalidKeysetNameFormat ksname)
    Right ksn -> do
      let writeKs = do
            newKsSize <- sizeOf SizeOfV0 newKs
            chargeGasArgs info (GWrite newKsSize)
            writeKeySet info pdb Write ksn newKs
            returnCEKValue cont handler (VString "Keyset write success")
      liftDbFunction info (readKeySet pdb ksn) >>= \case
        Just oldKs -> do
          let cont' = BuiltinC env info (DefineKeysetC ksn newKs) cont
          isKeysetInSigs info cont' handler env oldKs
        Nothing | ignoreNamespaces -> writeKs
        Nothing | otherwise -> useEvalState (esLoaded . loNamespace) >>= \case
          Nothing ->
            throwExecutionError info CannotDefineKeysetOutsideNamespace
          Just (Namespace ns uGuard _adminGuard) -> do
            when (Just ns /= _keysetNs ksn) $ throwExecutionError info (MismatchingKeysetNamespace ns)
            let cont' = BuiltinC env info (DefineKeysetC ksn newKs) cont
            enforceGuard info cont' handler env uGuard

defineKeySet :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
defineKeySet info b cont handler env = \case
  [VString ksname, VGuard (GKeyset ks)] -> do
    enforceTopLevelOnly info b
    defineKeySet' info cont handler env ksname ks
  [VString ksname] -> do
    enforceTopLevelOnly info b
    readKeyset' info ksname >>= \case
      Just newKs ->
        defineKeySet' info cont handler env ksname newKs
      Nothing -> returnCEKError info cont handler $ EnvReadFunctionFailure  (builtinName b)
  args -> argsError info b args

--------------------------------------------------
-- Capabilities
--------------------------------------------------

requireCapability :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
requireCapability info b cont handler _env = \case
  [VCapToken ct] -> do
    slots <- useEvalState $ esCaps . csSlots
    let cnt = sum [1 + length cs | CapSlot _ cs <- slots]
    chargeGasArgs info $ GCapOp $ CapOpRequire cnt
    requireCap info cont handler ct
  args -> argsError info b args


composeCapability :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
composeCapability info b cont handler env = \case
  [VCapToken ct] -> do
    enforceStackTopIsDefcap info b
    composeCap info cont handler env ct
  args -> argsError info b args

installCapability :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
installCapability info b cont handler env = \case
  [VCapToken ct] -> do
    enforceNotWithinDefcap info env "install-capability"
    _ <- installCap info env ct True
    returnCEKValue cont handler (VString "Installed capability")
  args -> argsError info b args

coreEmitEvent :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
coreEmitEvent info b cont handler env = \case
  [VCapToken ct@(CapToken fqn _)] -> do
    let cont' = BuiltinC env info (EmitEventC ct) cont
    guardForModuleCall info cont' handler env (_fqModule fqn) $ do
      -- Todo: this code is repeated in the EmitEventFrame code
      d <- getDefCap info fqn
      enforceMeta (_dcapMeta d)
      emitCapability info ct
      returnCEKValue cont handler (VBool True)
        where
        enforceMeta Unmanaged = throwExecutionError info (InvalidEventCap fqn)
        enforceMeta _ = pure ()
  args -> argsError info b args

createCapGuard :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
createCapGuard info b cont handler _env = \case
  [VCapToken ct] -> do
    let qn = fqnToQualName (_ctName ct)
        cg = CapabilityGuard qn (_ctArgs ct) Nothing
    returnCEKValue cont handler (VGuard (GCapabilityGuard cg))
  args -> argsError info b args

createCapabilityPactGuard :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
createCapabilityPactGuard info b cont handler _env = \case
  [VCapToken ct] -> do
    pid <- getDefPactId info
    let qn = fqnToQualName (_ctName ct)
    let cg = CapabilityGuard qn (_ctArgs ct) (Just pid)
    returnCEKValue cont handler (VGuard (GCapabilityGuard cg))
  args -> argsError info b args

createModuleGuard :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
createModuleGuard info b cont handler _env = \case
  [VString n] ->
    findCallingModule >>= \case
      Just mn ->  do
        let cg = GModuleGuard (ModuleGuard mn n)
        returnCEKValue cont handler (VGuard cg)
      Nothing ->
        throwNativeExecutionError info b "create-module-guard: must call within module"
  args -> argsError info b args

createDefPactGuard :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
createDefPactGuard info b cont handler _env = \case
  [VString name] -> do
    dpid <- getDefPactId info
    returnCEKValue cont handler $ VGuard $ GDefPactGuard $ DefPactGuard dpid name
  args -> argsError info b args


coreIntToStr :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
coreIntToStr info b cont handler _env = \case
  [VInteger base, VInteger v]
    | v < 0 ->
      throwNativeExecutionError info b "int-to-str error: cannot show negative integer"
    | base >= 2 && base <= 16 -> do
      let strLen = 1 + Exts.I# (IntLog.integerLogBase# base $ abs v)
      chargeGasArgs info $ GConcat $ TextConcat $ GasTextLength $ fromIntegral strLen
      let v' = T.pack $ showIntAtBase base Char.intToDigit v ""
      returnCEKValue cont handler (VString v')
    | base == 64 && v >= 0 -> do
      let bsLen = 1 + Exts.I# (IntLog.integerLogBase# 256 $ abs v)
          strLen = (bsLen * 4) `div` 3
      chargeGasArgs info $ GConcat $ TextConcat $ GasTextLength $ fromIntegral strLen
      let v' = toB64UrlUnpaddedText $ integerToBS v
      returnCEKValue cont handler (VString v')
    | base == 64 ->
      throwNativeExecutionError info b "only positive values allowed for base64URL conversion"
    | otherwise ->
      throwNativeExecutionError info b "invalid base for base64URL conversion"
  args -> argsError info b args

coreStrToInt :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
coreStrToInt info b cont handler _env = \case
  [VString s] -> do
    chargeGasArgs info $ GStrOp $ StrOpParse $ T.length s
    checkLen info s
    doBase info cont handler 10 s
  args -> argsError info b args

coreStrToIntBase :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
coreStrToIntBase info b cont handler _env = \case
  [VInteger base, VString s]
    | base == 64 -> do
        chargeGasArgs info $ GStrOp $ StrOpParse $ T.length s
        checkLen info s
        case decodeBase64UrlUnpadded $ T.encodeUtf8 s of
          Left{} -> throwExecutionError info (DecodeError "invalid b64 encoding")
          Right bs -> returnCEKValue cont handler $ VInteger (bsToInteger bs)
    | base >= 2 && base <= 16 -> do
        chargeGasArgs info $ GStrOp $ StrOpParse $ T.length s
        checkLen info s
        doBase info cont handler base s
    | otherwise -> throwNativeExecutionError info b $ "Base value must be >= 2 and <= 16, or 64"
  args -> argsError info b args
  where
  -- Todo: DOS and gas analysis
  bsToInteger :: BS.ByteString -> Integer
  bsToInteger bs = fst $ foldl' go (0,(BS.length bs - 1) * 8) $ BS.unpack bs
  go (i,p) w = (i .|. (shift (fromIntegral w) p), p - 8)

nubByM :: Monad m => (a -> a -> m Bool) -> [a] -> m [a]
nubByM eq = go
  where
  go [] = pure []
  go (x:xs) = do
    xs' <- filterM (fmap not . eq x) xs
    (x :) <$> go xs'

coreDistinct  :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
coreDistinct info b cont handler _env = \case
  [VList s] -> do
    uniques <- nubByM (valEqGassed info) $ V.toList s
    returnCEKValue cont handler
      $ VList
      $ V.fromList uniques
  args -> argsError info b args

coreFormat  :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
coreFormat info b cont handler _env = \case
  [VString s, VList es] -> do
    chargeGasArgs info $ GConcat $ TextConcat $ GasTextLength $ T.length s
    let parts = T.splitOn "{}" s
        plen = length parts
    if | plen == 1 -> returnCEKValue cont handler (VString s)
       | plen - length es > 1 ->
        throwNativeExecutionError info b $ "not enough arguments for template"
       | otherwise -> do
          args <- mapM formatArgM $ V.toList es
          returnCEKValue cont handler $ VString $  T.concat $ alternate parts (take (plen - 1) args)
    where
    formatArg (PString ps) = ps
    formatArg a = renderPactValue a

    formatArgM a = do
      let a' = formatArg a
      chargeGasArgs info $ GConcat $ TextConcat $ GasTextLength $ T.length a'
      pure a'

    alternate (x:xs) ys = x : alternate ys xs
    alternate _ _ = []

    -- Todo: this is kinda hacky
    -- BIG TODO: REMOVE PRETTY FROM SEMANTICS.
    -- THIS CANNOT MAKE IT TO PROD
    renderPactValue :: PactValue -> T.Text
    renderPactValue = T.pack . show . Pretty.pretty
  args -> argsError info b args

checkLen
  :: (MonadEval b i m)
  => i
  -> T.Text
  -> m ()
checkLen info txt =
  unless (T.length txt <= 512) $
      throwExecutionError info $ DecodeError "Invalid input, only up to 512 length supported"

doBase
  :: ()
  => SpanInfo
  -> Cont CEKBigStep CoreBuiltin SpanInfo Eval
  -> CEKErrorHandler CEKBigStep CoreBuiltin SpanInfo Eval
  -> Integer
  -> T.Text
  -> Eval (EvalResult CEKBigStep CoreBuiltin SpanInfo Eval)
doBase info cont handler base txt = case baseStrToInt base txt of
  Left e -> throwExecutionError info (DecodeError e)
  Right n -> returnCEKValue cont handler (VInteger n)

baseStrToInt :: Integer -> T.Text -> Either T.Text Integer
baseStrToInt base t
  | base <= 1 || base > 16 = Left $ "unsupported base: " `T.append` T.pack (show base)
  | T.null t = Left $ "empty text: " `T.append` t
  | T.any (not . Char.isHexDigit) t = Left "invalid digit: supported digits are 0-9, A-F"
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


coreAndQ :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
coreAndQ info b cont handler env = \case
  [VClosure l, VClosure r, VPactValue v] -> do
    let cont' =  CondC env info (AndQC r v) cont
    applyLam l [VPactValue v] cont' handler
  args -> argsError info b args

coreOrQ :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
coreOrQ info b cont handler env = \case
  [VClosure l, VClosure r, VPactValue v] -> do
    let cont' =  CondC env info (OrQC r v) cont
    applyLam l [VPactValue v] cont' handler
  args -> argsError info b args

coreNotQ :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
coreNotQ info b cont handler env = \case
  [VClosure clo, VPactValue v] -> do
    let cont' = CondC env info NotQC cont
    applyLam clo [VPactValue v] cont' handler
  args -> argsError info b args

coreWhere :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
coreWhere info b cont handler _env = \case
  [VString field, VClosure app, VObject o] -> do
    case M.lookup (Field field) o of
      Just v -> do
        let cont' = EnforceBoolC info cont
        applyLam app [VPactValue v] cont' handler
      Nothing ->
        throwExecutionError info (ObjectIsMissingField (Field field) (ObjectData o))
  args -> argsError info b args

coreHash :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
coreHash = \info b cont handler _env -> \case
  [VString s] ->
    returnCEKValue cont handler (go (T.encodeUtf8 s))
  [VPactValue pv] -> do
    returnCEKValue cont handler (go (encodeStable pv))
  args -> argsError info b args
  where
  go =  VString . hashToText . pactHash

txHash :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
txHash info b cont handler _env = \case
  [] -> do
    h <- viewEvalEnv eeHash
    returnCEKValue cont handler (VString (hashToText h))
  args -> argsError info b args

coreContinue :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
coreContinue info b cont handler _env = \case
  [v] -> do
    returnCEKValue cont handler v
  args -> argsError info b args

parseTime :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
parseTime info b cont handler _env = \case
  [VString fmt, VString s] -> do
    chargeGasArgs info $ GStrOp $ StrOpParseTime (T.length fmt) (T.length s)
    case PactTime.parseTime (T.unpack fmt) (T.unpack s) of
      Just t -> returnCEKValue cont handler $ VPactValue (PTime t)
      Nothing ->
        throwNativeExecutionError info b $ "parse-time parse failure"
  args -> argsError info b args

formatTime :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
formatTime info b cont handler _env = \case
  [VString fmt, VPactValue (PTime t)] -> do
    chargeGasArgs info $ GStrOp $ StrOpFormatTime $ T.length fmt
    let timeString = PactTime.formatTime (T.unpack fmt) t
    returnCEKValue cont handler $ VString (T.pack timeString)
  args -> argsError info b args

time :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
time info b cont handler _env = \case
  [VString s] -> do
    case PactTime.parseTime "%Y-%m-%dT%H:%M:%SZ" (T.unpack s) of
      Just t -> returnCEKValue cont handler $ VPactValue (PTime t)
      Nothing ->
        throwNativeExecutionError info b $ "time default format parse failure"
  args -> argsError info b args

addTime :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
addTime info b cont handler _env = \case
  [VPactValue (PTime t), VPactValue (PDecimal seconds)] -> do
      let newTime = t PactTime..+^ PactTime.fromSeconds seconds
      returnCEKValue cont handler $ VPactValue (PTime newTime)
  [VPactValue (PTime t), VPactValue (PInteger seconds)] -> do
      let newTime = t PactTime..+^ PactTime.fromSeconds (fromIntegral seconds)
      returnCEKValue cont handler $ VPactValue (PTime newTime)
  args -> argsError info b args

diffTime :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
diffTime info b cont handler _env = \case
  [VPactValue (PTime x), VPactValue (PTime y)] -> do
    let secondsDifference = PactTime.toSeconds $ x PactTime..-. y
    returnCEKValue cont handler $ VPactValue $ PDecimal secondsDifference
  args -> argsError info b args

minutes :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
minutes info b cont handler _env = \case
  [VDecimal x] -> do
    let seconds = x * 60
    returnCEKValue cont handler $ VDecimal seconds
  [VInteger x] -> do
    let seconds = fromIntegral x * 60
    returnCEKValue cont handler $ VDecimal seconds
  args -> argsError info b args

hours :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
hours info b cont handler _env = \case
  [VDecimal x] -> do
    let seconds = x * 60 * 60
    returnCEKValue cont handler $ VDecimal seconds
  [VInteger x] -> do
    let seconds = fromIntegral x * 60 * 60
    returnCEKValue cont handler $ VDecimal seconds
  args -> argsError info b args

days :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
days info b cont handler _env = \case
  [VDecimal x] -> do
    let seconds = x * 60 * 60 * 24
    returnCEKValue cont handler $ VDecimal seconds
  [VInteger x] -> do
    let seconds = fromIntegral x * 60 * 60 * 24
    returnCEKValue cont handler $ VDecimal seconds
  args -> argsError info b args

describeModule :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
describeModule info b cont handler env = \case
  [VString s] -> case parseModuleName s of
    Just mname -> do
      enforceTopLevelOnly info b
      checkNonLocalAllowed info b
      getModuleData info (view cePactDb env) mname >>= \case
        ModuleData m _ -> returnCEKValue cont handler $
          VObject $ M.fromList $ fmap (over _1 Field)
            [ ("name", PString (renderModuleName (_mName m)))
            , ("hash", PString (moduleHashToText (_mHash m)))
            , ("interfaces", PList (PString . renderModuleName <$> V.fromList (_mImplements m)))]
        InterfaceData iface _ -> returnCEKValue cont handler $
          VObject $ M.fromList $ fmap (over _1 Field)
            [ ("name", PString (renderModuleName (_ifName iface)))
            , ("hash", PString (moduleHashToText (_ifHash iface)))
            ]
    Nothing ->
      throwNativeExecutionError info b $ "invalid module name format"
  args -> argsError info b args

dbDescribeTable :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
dbDescribeTable info b cont handler _env = \case
  [VTable (TableValue name _ schema)] -> do
    enforceTopLevelOnly info b
    returnCEKValue cont handler $ VObject $ M.fromList $ fmap (over _1 Field)
      [("name", PString (_tableName name))
      ,("module", PString (renderModuleName (_tableModuleName name)))
      ,("type", PString (renderType (TyTable schema)))]
  args -> argsError info b args

dbDescribeKeySet :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
dbDescribeKeySet info b cont handler env = \case
  [VString s] -> do
    let pdb = _cePactDb env
    enforceTopLevelOnly info b
    case parseAnyKeysetName s of
      Right ksn -> do
        liftDbFunction info (_pdbRead pdb DKeySets ksn) >>= \case
          Just ks ->
            returnCEKValue cont handler (VGuard (GKeyset ks))
          Nothing ->
            throwExecutionError info (NoSuchKeySet ksn)
      Left{} ->
        throwNativeExecutionError info b  "incorrect keyset name format"
  args -> argsError info b args

coreCompose :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
coreCompose info b cont handler env = \case
  [VClosure clo1, VClosure clo2, v] -> do
    let cont' = Fn clo2 env [] [] cont
    applyLam clo1 [v] cont' handler
  args -> argsError info b args

createPrincipalForGuard :: (MonadEval b i m) => i -> Guard QualifiedName PactValue -> m Pr.Principal
createPrincipalForGuard info = \case
  GKeyset (KeySet ks pf) -> case (toList ks, pf) of
    ([k], KeysAll)
      | ed25519HexFormat k -> Pr.K k <$ chargeGas 1_000
    (l, _) -> do
      h <- mkHash $ map (T.encodeUtf8 . _pubKey) l
      case pf of
        CustomPredicate (TQN (QualifiedName n (ModuleName mn mns))) -> do
          let totalLength = T.length n + T.length mn + maybe 0 (T.length . _namespaceName) mns
          chargeGasArgs info $ GConcat $ TextConcat $ GasTextLength totalLength
        _ -> pure ()
      pure $ Pr.W (hashToText h) (predicateToText pf)
  GKeySetRef ksn ->
    Pr.R ksn <$ chargeGas 1_000
  GModuleGuard (ModuleGuard mn n) ->
    Pr.M mn n <$ chargeGas 1_000
  GUserGuard (UserGuard f args) -> do
    h <- mkHash $ map encodeStable args
    pure $ Pr.U (renderQualName f) (hashToText h)
    -- TODO orig pact gets here ^^^^ a Name
    -- which can be any of QualifiedName/BareName/DynamicName/FQN,
    -- and uses the rendered string here. Need to double-check equivalence.
  GCapabilityGuard (CapabilityGuard f args pid) -> do
    let args' = map encodeStable args
        f' = T.encodeUtf8 $ renderQualName f
        pid' = T.encodeUtf8 . renderDefPactId <$> pid
    h <- mkHash $ f' : args' ++ maybeToList pid'
    pure $ Pr.C $ hashToText h
  GDefPactGuard (DefPactGuard dpid name) -> Pr.P dpid name <$ chargeGas 1_000
  where
    chargeGas mg = chargeGasArgs info (GAConstant (MilliGas mg))
    mkHash bss = do
      let bs = mconcat bss
          gasChargeAmt = 1_000 + fromIntegral (BS.length bs `quot` 64) * 1_000
      chargeGas gasChargeAmt
      pure $ pactHash bs


coreCreatePrincipal :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
coreCreatePrincipal info b cont handler _env = \case
  [VGuard g] -> do
    pr <- createPrincipalForGuard info g
    returnCEKValue cont handler $ VString $ Pr.mkPrincipalIdent pr
  args -> argsError info b args

coreIsPrincipal :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
coreIsPrincipal info b cont handler _env = \case
  [VString p] -> do
    chargeGasArgs info $ GStrOp $ StrOpParse $ T.length p
    returnCEKValue cont handler $ VBool $ isRight $ parseOnly Pr.principalParser p
  args -> argsError info b args

coreTypeOfPrincipal :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
coreTypeOfPrincipal info b cont handler _env = \case
  [VString p] -> do
    chargeGasArgs info $ GStrOp $ StrOpParse $ T.length p
    let prty = case parseOnly Pr.principalParser p of
          Left _ -> ""
          Right pr -> Pr.showPrincipalType pr
    returnCEKValue cont handler $ VString prty
  args -> argsError info b args

coreValidatePrincipal :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
coreValidatePrincipal info b cont handler _env = \case
  [VGuard g, VString s] -> do
    pr' <- createPrincipalForGuard info g
    chargeGasArgs info $ GComparison $ TextComparison s
    returnCEKValue cont handler $ VBool $ Pr.mkPrincipalIdent pr' == s
  args -> argsError info b args


coreCond :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
coreCond info b cont handler _env = \case
  [VClosure clo] -> applyLam clo [] cont handler
  args -> argsError info b args


coreIdentity :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
coreIdentity info b cont handler _env = \case
  [VPactValue pv] -> returnCEKValue cont handler $ VPactValue pv
  args -> argsError info b args


--------------------------------------------------
-- Namespace functions
--------------------------------------------------
coreNamespace :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
coreNamespace info b cont handler env = \case
  [VString n] -> do
    enforceTopLevelOnly info b
    let pdb = view cePactDb env
    if T.null n then do
      (esLoaded . loNamespace) .== Nothing
      returnCEKValue cont handler (VString "Namespace reset to root")
    else do
      chargeGasArgs info $ GRead $ fromIntegral $ T.length n
      liftDbFunction info (_pdbRead pdb DNamespaces (NamespaceName n)) >>= \case
        Just ns -> do
          size <- sizeOf SizeOfV0 ns
          chargeGasArgs info $ GRead size
          (esLoaded . loNamespace) .== Just ns
          let msg = "Namespace set to " <> n
          returnCEKValue cont handler (VString msg)
        Nothing ->
          throwExecutionError info $ NamespaceNotFound (NamespaceName n)
  args -> argsError info b args


coreDefineNamespace :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
coreDefineNamespace info b cont handler env = \case
  [VString n, VGuard usrG, VGuard adminG] -> do
    enforceTopLevelOnly info b
    unless (isValidNsFormat n) $ throwNativeExecutionError info b "invalid namespace format"
    let pdb = view cePactDb env
    let nsn = NamespaceName n
        ns = Namespace nsn usrG adminG
    chargeGasArgs info $ GRead $ fromIntegral $ T.length n
    liftDbFunction info (_pdbRead pdb DNamespaces (NamespaceName n)) >>= \case
      -- G!
      -- https://static.wikia.nocookie.net/onepiece/images/5/52/Lao_G_Manga_Infobox.png/revision/latest?cb=20150405020446
      -- Enforce the old guard
      Just existing@(Namespace _ _ laoG) -> do
        size <- sizeOf SizeOfV0 existing
        chargeGasArgs info $ GRead size
        let cont' = BuiltinC env info (DefineNamespaceC ns) cont
        enforceGuard info cont' handler env laoG
      Nothing -> viewEvalEnv eeNamespacePolicy >>= \case
        SimpleNamespacePolicy -> do
          nsSize <- sizeOf SizeOfV0 ns
          chargeGasArgs info (GWrite nsSize)
          liftGasM info $ _pdbWrite pdb Write DNamespaces nsn ns
          returnCEKValue cont handler $ VString $ "Namespace defined: " <> n
        SmartNamespacePolicy _ fun -> getModuleMemberWithHash info pdb fun >>= \case
          (Dfun d, mh) -> do
            clo <- mkDefunClosure d (qualNameToFqn fun mh) env
            let cont' = BuiltinC env info (DefineNamespaceC ns) cont
            applyLam (C clo) [VString n, VGuard adminG] cont' handler
          _ -> throwNativeExecutionError info b $ "Fatal error: namespace manager function is not a defun"
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

coreDescribeNamespace :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
coreDescribeNamespace info b cont handler _env = \case
  [VString n] -> do
    pdb <- viewEvalEnv eePactDb
    chargeGasArgs info $ GRead $ fromIntegral $ T.length n
    liftDbFunction info (_pdbRead pdb DNamespaces (NamespaceName n)) >>= \case
      Just existing@(Namespace _ usrG laoG) -> do
        size <- sizeOf SizeOfV0 existing
        chargeGasArgs info $ GRead size
        let obj = M.fromList
                  [ (Field "user-guard", PGuard usrG)
                  , (Field "admin-guard", PGuard laoG)
                  , (Field "namespace-name", PString n)]
        returnCEKValue cont handler (VObject obj)
      Nothing ->
        throwExecutionError info $ NamespaceNotFound (NamespaceName n)
  args -> argsError info b args


coreChainData :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
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
    returnCEKValue cont handler (VObject fields)
  args -> argsError info b args


-- -------------------------
-- ZK defns
-- -------------------------

#ifndef WITHOUT_CRYPTO
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
  toPactPt ext = PList $ PInteger . fromIntegral <$> extElements ext
  x' = toPactPt x
  y' = toPactPt y
  pts =
    M.fromList
    [ (Field "x", x')
    , (Field "y", y')]


zkPairingCheck :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
zkPairingCheck info b cont handler _env = \case
  args@[VList p1s, VList p2s] -> do
    chargeGasArgs info (GAZKArgs (Pairing (max (V.length p1s) (V.length p2s))))
    g1s <- maybe (argsError info b args) pure (traverse (preview _PObject >=> (toG1 . ObjectData)) p1s)
    g2s <- maybe (argsError info b args) pure (traverse (preview _PObject >=> (toG2 . ObjectData)) p2s)
    traverse_ (\p -> ensureOnCurve info p b1) g1s
    traverse_ (\p -> ensureOnCurve info p b2) g2s
    let pairs = zip (V.toList g1s) (V.toList g2s)
    returnCEKValue cont handler $ VBool $ pairingCheck pairs
  args -> argsError info b args

zkScalarMult :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
zkScalarMult info b cont handler _env = \case
  args@[VString ptTy, VObject p1, VInteger scalar] -> do
    let scalar' = scalar `mod` curveOrder
    case T.toLower ptTy of
      "g1" -> do
        chargeGasArgs info (GAZKArgs (ScalarMult ZKG1))
        p1' <- maybe (argsError info b args) pure $ toG1 (ObjectData p1)
        ensureOnCurve info p1' b1
        let p2' = multiply p1' scalar'
            ObjectData o = fromG1 p2'
        returnCEKValue cont handler (VObject o)
      "g2" -> do
        chargeGasArgs info (GAZKArgs (ScalarMult ZKG2))
        p1' <- maybe (argsError info b args) pure $ toG2 (ObjectData p1)
        ensureOnCurve info p1' b2
        let p2' = multiply p1' scalar'
            ObjectData o = fromG2 p2'
        returnCEKValue cont handler (VObject o)
      _ -> argsError info b args
  args -> argsError info b args
  where
  curveOrder :: Integer
  curveOrder = 21888242871839275222246405745257275088548364400416034343698204186575808495617

zkPointAddition :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
zkPointAddition info b cont handler _env = \case
  args@[VString ptTy, VObject p1, VObject p2] -> do
    case T.toLower ptTy of
      "g1" -> do
        chargeGasArgs info (GAZKArgs (PointAdd ZKG1))
        p1' <- maybe (argsError info b args) pure $ toG1 (ObjectData p1)
        p2' <- maybe (argsError info b args) pure $ toG1 (ObjectData p2)
        ensureOnCurve info p1' b1
        ensureOnCurve info p2' b1
        let p3' = add p1' p2'
            ObjectData o = fromG1 p3'
        returnCEKValue cont handler (VObject o)
      "g2" -> do
        chargeGasArgs info (GAZKArgs (PointAdd ZKG2))
        p1' <- maybe (argsError info b args) pure $ toG2 (ObjectData p1)
        p2' <- maybe (argsError info b args) pure $ toG2 (ObjectData p2)
        ensureOnCurve info p1' b2
        ensureOnCurve info p2' b2
        let p3' = add p1' p2'
            ObjectData o = fromG2 p3'
        returnCEKValue cont handler (VObject o)
      _ -> argsError info b args
  args -> argsError info b args


-----------------------------------
-- Poseidon
-----------------------------------

poseidonHash :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
poseidonHash info b cont handler _env = \case
  [VList as]
    | not (V.null as) && length as <= 8,
    Just intArgs <- traverse (preview (_PLiteral . _LInteger)) as -> do
      chargeGasArgs info (GPoseidonHashHackAChain (length intArgs))
      returnCEKValue cont handler $ VInteger (poseidon (V.toList intArgs))
  args -> argsError info b args

#else

zkPairingCheck :: (MonadEval b i m) => NativeFunction CEKBigStep b i m
zkPairingCheck info _b _cont _handler _env _args = throwExecutionError info $ EvalError $ "crypto disabled"

zkScalarMult :: (MonadEval b i m) => NativeFunction CEKBigStep b i m
zkScalarMult info _b _cont _handler _env _args = throwExecutionError info $ EvalError $ "crypto disabled"

zkPointAddition :: (MonadEval b i m) => NativeFunction CEKBigStep b i m
zkPointAddition info _b _cont _handler _env _args = throwExecutionError info $ EvalError $ "crypto disabled"

poseidonHash :: (MonadEval b i m) => NativeFunction CEKBigStep b i m
poseidonHash info _b _cont _handler _env _args = throwExecutionError info $ EvalError $ "crypto disabled"

#endif

-----------------------------------
-- SPV
-----------------------------------

coreVerifySPV :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
coreVerifySPV info b cont handler _env = \case
  [VString proofType, VObject o] -> do
    SPVSupport f _ <- viewEvalEnv eeSPVSupport
    liftIO (f proofType (ObjectData o)) >>= \case
      Left err -> throwExecutionError info (SPVVerificationFailure err)
      Right (ObjectData o') -> returnCEKValue cont handler (VObject o')
  args -> argsError info b args

-----------------------------------
-- Verifiers
-----------------------------------
coreEnforceVerifier :: NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
coreEnforceVerifier info b cont handler _env = \case
  [VString verName] -> do
    enforceStackTopIsDefcap info b
    viewsEvalEnv eeMsgVerifiers (M.lookup (VerifierName verName)) >>= \case
      Just verCaps -> do
        verifierInScope <- anyCapabilityBeingEvaluated verCaps
        if verifierInScope then returnCEKValue cont handler (VBool True)
        else returnCEKError info cont handler $ verifError verName "not in scope"
      Nothing ->
        returnCEKError info cont handler (verifError verName "not in transaction")
  args -> argsError info b args
  where
    verifError verName msg = VerifierFailure (VerifierName verName) msg


-----------------------------------
-- Builtin exports
-----------------------------------


coreBuiltinEnv
  :: BuiltinEnv CEKBigStep CoreBuiltin SpanInfo Eval
coreBuiltinEnv i b env = mkBuiltinFn i b env (coreBuiltinRuntime b)
{-# INLINEABLE coreBuiltinEnv #-}

-- coreBuiltinRuntimeArray :: Vector (NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval)
-- coreBuiltinRuntimeArray =
--   V.fromList [
--   -- Addition/Concatenation
--   -- = CoreAdd
--   rawAdd
--   -- | CoreSub
--   , rawSub
--   -- | CoreMultiply
--   , rawMul
--   -- | CoreDivide
--   , rawDiv
--   -- | CoreNegate
--   , rawNegate
--   -- | CoreAbs
--   , rawAbs
--   -- | CorePow
--   , rawPow
--   -- | CoreNot
--   , notBool
--   -- | CoreEq
--   , rawEq
--   -- | CoreNeq
--   , rawNeq
--   -- | CoreGT
--   , rawGt
--   -- | CoreGEQ
--   , rawGeq
--   -- | CoreLT
--   , rawLt
--   -- | CoreLEQ
--   , rawLeq
--   -- | CoreBitwiseAnd
--   , bitAndInt
--   -- | CoreBitwiseOr
--   , bitOrInt
--   -- | CoreBitwiseXor
--   , bitXorInt
--   -- | CoreBitwiseFlip
--   , bitComplementInt
--   -- | CoreBitShift
--   , bitShiftInt
--   -- | CoreRound
--   , roundDec
--   -- | CoreCeiling
--   , ceilingDec
--   -- | CoreFloor
--   , floorDec
--   -- | CoreRoundPrec
--   , roundDec
--   -- | CoreCeilingPrec
--   ,  ceilingDec
--   -- | CoreFloorPrec
--   , floorDec
--   -- | CoreExp
--   , rawExp
--   -- | CoreLn
--   , rawLn
--   -- | CoreSqrt
--   , rawSqrt
--   -- | CoreLogBase
--   , rawLogBase
--   -- | CoreLength
--   ,  rawLength
--   -- | CoreTake
--   , rawTake
--   -- | CoreDrop
--   , rawDrop
--   -- | CoreConcat
--   , coreConcat
--   -- | CoreReverse
--   , rawReverse
--   -- | CoreContains
--   , rawContains
--   -- | CoreSort
--   , rawSort
--   -- | CoreSortObject
--   , rawSortObject
--   -- | CoreRemove
--   , coreRemove
--   -- | CoreMod
--   , modInt
--   -- | CoreMap
--   , coreMap
--   -- | CoreFilter
--   , coreFilter
--   -- | CoreZip
--   , zipList
--   -- | CoreIntToStr
--   , coreIntToStr
--   -- | CoreStrToInt
--   , coreStrToInt
--   -- | CoreStrToIntBase
--   , coreStrToIntBase
--   -- | CoreFold
--   , coreFold
--   -- | CoreDistinct
--   , coreDistinct
--   -- | CoreFormat
--   , coreFormat
--   -- | CoreEnumerate
--   , coreEnumerate
--   -- | CoreEnumerateStepN
--   , coreEnumerateStepN
--   -- -- Guards + read functions
--   -- | CoreShow
--   , rawShow
--   -- | CoreReadMsg
--   , coreReadMsg
--   -- | CoreReadMsgDefault
--   , coreReadMsg
--   -- | CoreReadInteger
--   , coreReadInteger
--   -- | CoreReadDecimal
--   , coreReadDecimal
--   -- | CoreReadString
--   , coreReadString
--   -- | CoreReadKeyset
--   , coreReadKeyset
--   -- | CoreEnforceGuard
--   , coreEnforceGuard
--   -- | CoreEnforceKeyset
--   , coreEnforceGuard
--   -- | CoreKeysetRefGuard
--   , keysetRefGuard
--   -- | CoreAt
--   , coreAccess
--   -- | CoreMakeList
--   , makeList
--   -- | CoreB64Encode
--   , coreB64Encode
--   -- | CoreB64Decode
--   , coreB64Decode
--   -- | CoreStrToList
--   , strToList
--   -- | CoreYield
--   , coreYield
--   -- | CoreYieldToChain
--   , coreYield
--   -- | CoreResume
--   , coreResume
--   -- | CoreBind
--   , coreBind
--   -- | CoreRequireCapability
--   , requireCapability
--   -- | CoreComposeCapability
--   , composeCapability
--   -- | CoreInstallCapability
--   , installCapability
--   -- | CoreEmitEvent
--   , coreEmitEvent
--   -- | CoreCreateCapabilityGuard
--   , createCapGuard
--   -- | CoreCreateCapabilityPactGuard
--   , createCapabilityPactGuard
--   -- | CoreCreateModuleGuard
--   , createModuleGuard
--   -- | CoreCreateDefPactGuard
--   , createDefPactGuard
--   -- | CoreCreateTable
--   , createTable
--   -- | CoreDescribeKeyset
--   , dbDescribeKeySet
--   -- | CoreDescribeModule
--   , describeModule
--   -- | CoreDescribeTable
--   , dbDescribeTable
--   -- | CoreDefineKeySet
--   , defineKeySet
--   -- | CoreDefineKeysetData
--   , defineKeySet
--   -- | CoreFoldDb
--   , foldDb
--   -- | CoreInsert
--   , dbInsert
--   -- | CoreKeyLog
--   , dbKeyLog
--   -- | CoreKeys
--   , dbKeys
--   -- | CoreRead
--   , dbRead
--   -- | CoreSelect
--   , dbSelect
--   -- | CoreSelectWithFields
--   , dbSelect
--   -- | CoreUpdate
--   , dbUpdate
--   -- | CoreWithDefaultRead
--   , dbWithDefaultRead
--   -- | CoreWithRead
--   , dbWithRead
--   -- | CoreWrite
--   , dbWrite
--   -- | CoreTxIds
--   , dbTxIds
--   -- | CoreTxLog
--   , dbTxLog
--   -- | CoreTxHash
--   , txHash
--   -- | CoreAndQ
--   , coreAndQ
--   -- | CoreOrQ
--   , coreOrQ
--   -- | CoreWhere
--   , coreWhere
--   -- | CoreNotQ
--   , coreNotQ
--   -- | CoreHash
--   , coreHash
--   -- | CoreContinue
--   , coreContinue
--   -- | CoreParseTime
--   , parseTime
--   -- | CoreFormatTime
--   , formatTime
--   -- | CoreTime
--   , time
--   -- | CoreAddTime
--   , addTime
--   -- | CoreDiffTime
--   , diffTime
--   -- | CoreHours
--   , hours
--   -- | CoreMinutes
--   , minutes
--   -- | CoreDays
--   , days
--   -- | CoreCompose
--   , coreCompose
--   -- | CoreCreatePrincipal
--   , coreCreatePrincipal
--   -- | CoreIsPrincipal
--   , coreIsPrincipal
--   -- | CoreTypeOfPrincipal
--   , coreTypeOfPrincipal
--   -- | CoreValidatePrincipal
--   , coreValidatePrincipal
--   -- | CoreNamespace
--   , coreNamespace
--   -- | CoreDefineNamespace
--   , coreDefineNamespace
--   -- | CoreDescribeNamespace
--   , coreDescribeNamespace
--   -- | CoreChainData
--   , coreChainData
--   -- | CoreIsCharset
--   , coreIsCharset
--   -- | CorePactId
--   , corePactId
--   -- | CoreZkPairingCheck
--   , zkPairingCheck
--   -- | CoreZKScalarMult
--   , zkScalarMult
--   -- | CoreZkPointAdd
--   , zkPointAddition
--   -- | CorePoseidonHashHackachain
--   , poseidonHash
--   -- | CoreTypeOf
--   , coreTypeOf
--   -- | CoreDec
--   , coreDec
--   -- | CoreCond
--   , coreCond
--   -- | CoreIdentity
--   , coreIdentity
--   -- | CoreVerifySPV
--   , coreVerifySPV
--   -- | CoreEnforceVerifier
--   , coreEnforceVerifier
--   ]

coreBuiltinRuntime
  :: CoreBuiltin
  -> NativeFunction CEKBigStep CoreBuiltin SpanInfo Eval
coreBuiltinRuntime = \case
  CoreAdd -> rawAdd
  CoreSub -> rawSub
  CoreMultiply -> rawMul
  CoreDivide -> rawDiv
  CoreNegate -> rawNegate
  CoreAbs -> rawAbs
  CorePow -> rawPow
  CoreNot -> notBool
  CoreEq -> rawEq
  CoreNeq -> rawNeq
  CoreGT -> rawGt
  CoreGEQ -> rawGeq
  CoreLT -> rawLt
  CoreLEQ -> rawLeq
  CoreBitwiseAnd -> bitAndInt
  CoreBitwiseOr -> bitOrInt
  CoreBitwiseXor -> bitXorInt
  CoreBitwiseFlip -> bitComplementInt
  CoreBitShift -> bitShiftInt
  CoreRound -> roundDec
  CoreCeiling -> ceilingDec
  CoreFloor -> floorDec
  CoreRoundPrec -> roundDec
  CoreCeilingPrec -> ceilingDec
  CoreFloorPrec -> floorDec
  CoreExp -> rawExp
  CoreLn -> rawLn
  CoreSqrt -> rawSqrt
  CoreLogBase -> rawLogBase
  CoreLength -> rawLength
  CoreTake -> rawTake
  CoreDrop -> rawDrop
  CoreConcat -> coreConcat
  CoreReverse -> rawReverse
  CoreMod -> modInt
  CoreMap -> coreMap
  CoreFilter -> coreFilter
  CoreZip -> zipList
  CoreIntToStr -> coreIntToStr
  CoreStrToInt -> coreStrToInt
  CoreStrToIntBase -> coreStrToIntBase
  CoreFold -> coreFold
  CoreDistinct -> coreDistinct
  CoreFormat -> coreFormat
  CoreContains -> rawContains
  CoreSort -> rawSort
  CoreSortObject -> rawSortObject
  CoreRemove -> coreRemove
  -- CoreEnforce -> coreEnforce
  -- CoreEnforceOne -> unimplemented
  CoreEnumerate -> coreEnumerate
  CoreEnumerateStepN -> coreEnumerateStepN
  CoreShow -> rawShow
  CoreReadMsg -> coreReadMsg
  CoreReadMsgDefault -> coreReadMsg
  CoreReadInteger -> coreReadInteger
  CoreReadDecimal -> coreReadDecimal
  CoreReadString -> coreReadString
  CoreReadKeyset -> coreReadKeyset
  CoreEnforceGuard -> coreEnforceGuard
  CoreYield -> coreYield
  CoreYieldToChain -> coreYield
  CoreResume -> coreResume
  CoreEnforceKeyset -> coreEnforceGuard
  CoreKeysetRefGuard -> keysetRefGuard
  CoreAt -> coreAccess
  CoreMakeList -> makeList
  CoreB64Encode -> coreB64Encode
  CoreB64Decode -> coreB64Decode
  CoreStrToList -> strToList
  CoreBind -> coreBind
  CoreRequireCapability -> requireCapability
  CoreComposeCapability -> composeCapability
  CoreInstallCapability -> installCapability
  CoreCreateCapabilityGuard -> createCapGuard
  CoreCreateCapabilityPactGuard -> createCapabilityPactGuard
  CoreCreateModuleGuard -> createModuleGuard
  CoreCreateDefPactGuard -> createDefPactGuard
  CoreEmitEvent -> coreEmitEvent
  CoreCreateTable -> createTable
  CoreDescribeKeyset -> dbDescribeKeySet
  CoreDescribeModule -> describeModule
  CoreDescribeTable -> dbDescribeTable
  CoreDefineKeySet -> defineKeySet
  CoreDefineKeysetData -> defineKeySet
  CoreFoldDb -> foldDb
  CoreInsert -> dbInsert
  CoreWrite -> dbWrite
  CoreKeyLog -> dbKeyLog
  CoreKeys -> dbKeys
  CoreRead -> dbRead
  CoreSelect -> dbSelect
  CoreUpdate -> dbUpdate
  CoreWithDefaultRead -> dbWithDefaultRead
  CoreWithRead -> dbWithRead
  CoreTxLog -> dbTxLog
  CoreTxIds -> dbTxIds
  CoreAndQ -> coreAndQ
  CoreOrQ -> coreOrQ
  CoreWhere -> coreWhere
  CoreNotQ -> coreNotQ
  CoreHash -> coreHash
  CoreTxHash -> txHash
  CoreContinue -> coreContinue
  CoreParseTime -> parseTime
  CoreFormatTime -> formatTime
  CoreTime -> time
  CoreAddTime -> addTime
  CoreDiffTime -> diffTime
  CoreHours -> hours
  CoreMinutes -> minutes
  CoreDays -> days
  CoreCompose -> coreCompose
  CoreSelectWithFields -> dbSelect
  CoreCreatePrincipal -> coreCreatePrincipal
  CoreIsPrincipal -> coreIsPrincipal
  CoreTypeOfPrincipal -> coreTypeOfPrincipal
  CoreValidatePrincipal -> coreValidatePrincipal
  CoreNamespace -> coreNamespace
  CoreDefineNamespace -> coreDefineNamespace
  CoreDescribeNamespace -> coreDescribeNamespace
  CoreZkPairingCheck -> zkPairingCheck
  CoreZKScalarMult -> zkScalarMult
  CoreZkPointAdd -> zkPointAddition
  CorePoseidonHashHackachain -> poseidonHash
  CoreChainData -> coreChainData
  CoreIsCharset -> coreIsCharset
  CorePactId -> corePactId
  CoreTypeOf -> coreTypeOf
  CoreDec -> coreDec
  CoreCond -> coreCond
  CoreIdentity -> coreIdentity
  CoreVerifySPV -> coreVerifySPV
  CoreEnforceVerifier -> coreEnforceVerifier
