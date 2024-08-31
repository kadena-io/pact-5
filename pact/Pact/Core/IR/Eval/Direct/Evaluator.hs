{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE CPP #-}

module Pact.Core.IR.Eval.Direct.Evaluator
 ( eval
 , evalResumePact
 , interpretGuard
 , coreBuiltinEnv
 , resumePact
 , applyPact
 , constantWorkNodeGas
 , applyLamUnsafe
 , evalCap
 , installCap
 , coreBuiltinRuntime
 , enforcePactValue) where

import Control.Lens hiding (op, from, to, parts)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Text(Text)
import Data.Foldable
import Data.Maybe(catMaybes)
import Data.List.NonEmpty(NonEmpty(..))
import Data.Bits
import Data.Either(isLeft, isRight)
import Data.Decimal(roundTo', Decimal, DecimalRaw(..))
import Data.Attoparsec.Text(parseOnly)
import Numeric(showIntAtBase)
import qualified Data.RAList as RAList
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.List.NonEmpty as NE
import qualified GHC.Exts as Exts
import qualified GHC.Integer.Logarithms as IntLog
import qualified Data.List as L

#ifdef WITH_TRACING
import System.Clock
#endif

#ifndef WITHOUT_CRYPTO
import qualified Control.Lens as Lens
#endif

import qualified Data.Vector.Algorithms.Intro as V
import qualified Data.Char as Char
import qualified Data.ByteString as BS
import qualified Pact.Time as PactTime


import Pact.Core.IR.Eval.Runtime.Utils
import Pact.Core.IR.Eval.Runtime.Types
import Pact.Core.IR.Term
import Pact.Core.Names
import Pact.Core.Environment
import Pact.Core.Type
import Pact.Core.Errors
import Pact.Core.PactValue
import Pact.Core.Hash
import Pact.Core.Persistence
import Pact.Core.Guards
import Pact.Core.Capabilities
import Pact.Core.DefPacts.Types
import Pact.Core.Literal
import Pact.Core.ModRefs
import Pact.Core.Builtin
import Pact.Core.IR.Eval.Direct.Types
import Pact.Core.Gas
import Pact.Core.StableEncoding
import Pact.Core.SPV
import Pact.Crypto.Hyperlane
import qualified Data.Binary.Get as Bin
import qualified Data.Binary.Put as Bin


import Pact.Core.Namespace
#ifndef WITHOUT_CRYPTO
import Pact.Core.Crypto.Pairing
import Pact.Core.Crypto.Hash.Poseidon
#endif
import Pact.Core.SizeOf


import qualified Pact.Core.Principal as Pr
import qualified Pact.Core.Trans.TOps as Musl


mkDefunClosure
  :: EvalDefun b i
  -> FullyQualifiedName
  -> DirectEnv e b i
  -> EvalM e b i (Closure e b i)
mkDefunClosure d fqn e = case _dfunTerm d of
  Lam args body i ->
    pure (Closure fqn (ArgClosure args) (NE.length args) body (_dfunRType d) e i)
  Nullary body i ->
    pure (Closure fqn NullaryClosure 0 body (_dfunRType d) e i)
  _ ->
    failInvariant (_dfunInfo d) (InvariantMalformedDefun fqn)


mkDefPactClosure
  :: i
  -> FullyQualifiedName
  -> DefPact Name Type b i
  -> DirectEnv e b i
  -> EvalValue e b i
mkDefPactClosure info fqn dpact env = case _dpArgs dpact of
  [] ->
    let dpc = DefPactClosure fqn NullaryClosure 0 env info
    in VDefPactClosure dpc
  (x:xs) ->
    let dpc = DefPactClosure fqn (ArgClosure (x :| xs)) (length (x:xs)) env info
    in VDefPactClosure dpc

envFromPurity :: Purity -> DirectEnv e b i -> DirectEnv e b i
envFromPurity PImpure = id
envFromPurity PReadOnly = readOnlyEnv
envFromPurity PSysOnly = sysOnlyEnv

eval
  :: (IsBuiltin b)
  => Purity
  -> BuiltinEnv e b i
  -> EvalTerm b i
  -> EvalM e b i PactValue
eval purity benv term = do
  ee <- viewEvalEnv id
  let directEnv = envFromPurity purity (DirectEnv mempty (_eePactDb ee) benv (_eeDefPactStep ee) False)
  evaluate directEnv term >>= \case
    VPactValue pv -> pure pv
    _ -> throwExecutionError (view termInfo term) (EvalError "Evaluation did not reduce to a value")

interpretGuard
  :: forall e b i
  .  (IsBuiltin b)
  => i
  -> BuiltinEnv e b i
  -> Guard QualifiedName PactValue
  -> EvalM e b i PactValue
interpretGuard info bEnv g = do
  ee <- viewEvalEnv id
  let eEnv = DirectEnv mempty (_eePactDb ee) bEnv (_eeDefPactStep ee) False
  PBool <$> enforceGuard info eEnv g

evalResumePact
  :: (IsBuiltin b)
  => i
  -> BuiltinEnv e b i
  -> Maybe DefPactExec
  -> EvalM e b i PactValue
evalResumePact info bEnv mdpe = do
  ee <- viewEvalEnv id
  let pdb = _eePactDb ee
  let env = DirectEnv mempty pdb bEnv (_eeDefPactStep ee) False
  resumePact info env mdpe >>= \case
    VPactValue pv -> pure pv
    _ ->
      throwExecutionError info (EvalError "Evaluation did not reduce to a value")

evaluate
  :: IsBuiltin b
  => DirectEnv e b i
  -> EvalTerm b i
  -> EvalM e b i (EvalValue e b i)
evaluate env = \case
  Var n info -> do
    case _nKind n of
      NBound i -> do
        case RAList.lookup (_ceLocal env) i of
          Just v -> return v
          Nothing -> failInvariant info (InvariantInvalidBoundVariable (_nName n))
      -- Top level names are not closures, so we wipe the env
      NTopLevel mname mh -> do
        let fqn = FullyQualifiedName mname (_nName n) mh
        lookupFqName fqn >>= \case
          Just (Dfun d) -> do
            dfunClo <- VDefClosure <$> mkDefunClosure d fqn env
            return dfunClo
          -- Todo: this should be GADT'd out
          -- and defconsts should already be evaluated
          Just (DConst d) -> case _dcTerm d of
            -- Note: `TermConst` cannot and should not be `evalCEK`'d. This is an error
            -- this can cause semantic divergences, due to things like provided data.
            -- moreover defcosts are always evaluated in `SysOnly` mode.
            TermConst _term ->
              failInvariant info (InvariantDefConstNotEvaluated fqn)
            EvaledConst v ->
              return (VPactValue v)
          Just (DPact d) -> do
            let dpactClo = mkDefPactClosure info fqn d env
            return dpactClo
          Just (DTable d) ->
            let (ResolvedTable sc) = _dtSchema d
                tn = TableName (_dtName d) mname
                tbl = VTable (TableValue tn mh sc)
            in return tbl
          Just (DCap d) -> do
            let args = _argType <$> _dcapArgs d
                clo = CapTokenClosure fqn args (length args) info
            return (VClosure (CT clo))
          Just d ->
            failInvariant info (InvariantInvalidDefKind (defKind mname d) "in var position")
          Nothing ->
            failInvariant info (InvariantUnboundFreeVariable (FullyQualifiedName mname (_nName n) mh))
      NModRef m ifs -> case ifs of
        [] -> throwExecutionError info (ModRefImplementsNoInterfaces m)
        _ -> return (VModRef (ModRef m (S.fromList ifs)))
      NDynRef (DynamicRef dArg i) -> case RAList.lookup (view ceLocal env) i of
        Just (VModRef mr) -> do
          modRefHash <- _mHash <$> getModule info (view cePactDb env) (_mrModule mr)
          let nk = NTopLevel (_mrModule mr) modRefHash
          evaluate env (Var (Name dArg nk) info)
        Just _ -> throwExecutionError info (DynNameIsNotModRef dArg)
        Nothing -> failInvariant info (InvariantInvalidBoundVariable (_nName n))
  Constant l _info -> do
    return (VLiteral l)
  App ufn uargs info -> do
    fn <- enforceUserAppClosure info =<< evaluate env ufn
    args <- traverse (evaluate env) uargs
    applyLam fn args
  Sequence e1 e2 _ -> do
    _ <- evaluate env e1
    evaluate env e2
  Builtin b info -> do
    let builtins = _ceBuiltins env
    return (VNative (builtins info b env))
  Nullary body info -> do
    let clo = VLamClosure (LamClosure NullaryClosure 0 body Nothing env info)
    pure clo
  Let _ e1 e2 _ -> do
    e1val <- evaluate env e1
    let newEnv = RAList.cons e1val (_ceLocal env)
    let env' = env {_ceLocal = newEnv }
    evaluate env' e2
  Lam args body info -> do
      let clo = VLamClosure (LamClosure (ArgClosure args) (NE.length args) body Nothing env info)
      return clo
  Conditional c info -> case c of
    CAnd te te' -> do
      b <- evaluate env te >>= enforceBool info
      -- chargeGasArgs info (GAConstant constantWorkNodeGas)
      if b then evaluate env te' >>= enforceBoolValue info
      else pure (VBool False)
    COr te te' -> do
      b <- evaluate env te >>= enforceBool info
      -- chargeGasArgs info (GAConstant constantWorkNodeGas)
      if b then pure (VBool True)
      else evaluate env te' >>= enforceBoolValue info
    CIf bExpr ifExpr elseExpr -> do
      b <- enforceBool info =<< evaluate env bExpr
      -- chargeGasArgs info (GAConstant constantWorkNodeGas)
      if b then evaluate env ifExpr
      else evaluate env elseExpr
    CEnforce cond str -> do
      let env' = sysOnlyEnv env
      b <- enforceBool info =<< evaluate env' cond
      -- chargeGasArgs info (GAConstant constantWorkNodeGas)
      if b then return (VBool True)
      else do
        msg <- enforceString info =<< evaluate env str
        throwUserRecoverableError info (UserEnforceError msg)
    CEnforceOne str conds ->
      go conds
      where
      go (x:xs) = do
        cond <- catchRecoverable (enforceBool info =<< evaluate env x) (\_ _ -> pure False)
        chargeGasArgs info (GAConstant unconsWorkNodeGas)
        if cond then return (VBool True)
        else go xs
      go [] = do
        msg <- enforceString info =<< evaluate env str
        throwUserRecoverableError info (UserEnforceError msg)

  CapabilityForm cf info -> case cf of
    WithCapability cap body -> do
      enforceNotWithinDefcap info env "with-capability"
      rawCap <- enforceCapToken info =<< evaluate env cap
      let capModule = view (ctName . fqModule) rawCap
      guardForModuleCall info env capModule $ pure ()
      evalCap info env rawCap PopCapInvoke NormalCapEval body
    CreateUserGuard n uargs -> do
      fqn <- nameToFQN info env n
      args <- traverse (evaluate env >=> enforcePactValue info) uargs
      createUserGuard info fqn args
  ListLit ts info -> do
    chargeGasArgs info (GConcat (ListConcat (GasListLength (length ts))))
    args <- traverse (evaluate env >=> enforcePactValue info) ts
    return (VList (V.fromList args))
  Try catchExpr tryExpr info -> do
    chargeGasArgs info (GAConstant tryNodeGas)
    let env' = readOnlyEnv env
    catchRecoverable (evaluate env' tryExpr) (\_ _ -> evaluate env catchExpr)
  ObjectLit o info -> do
    chargeGasArgs info (GConcat (ObjConcat (length o)))
    args <- traverse go o
    return (VObject (M.fromList args))
    where
    go (f, e) = do
      v <- evaluate env e
      (f,) <$> enforcePactValue info v
  InlineValue v _ ->
    return (VPactValue v)

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
  :: (IsBuiltin b)
  => i
  -> DirectEnv e b i
  -> FQCapToken
  -> CapPopState
  -> EvalCapType
  -> EvalTerm b i
  -> EvalM e b i (EvalValue e b i)
evalCap info env origToken@(CapToken fqn args) popType ecType contbody = do
  capInStack <- isCapInStack' origToken
  if not capInStack then go else evaluate env contbody
  where
  go = do
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
            mgdCaps <- use (esCaps . csManaged)
            case find ((==) filteredCap . _mcCap) mgdCaps of
              Nothing -> do
                msgCaps <- S.unions <$> viewEvalEnv eeMsgSigs
                case find (findMsgSigCap cix filteredCap) msgCaps of
                  Just c -> do
                    let c' = set ctName fqn c
                        emittedEvent = fqctToPactEvent origToken <$ guard (ecType == NormalCapEval)
                    installCap info env c' False >>= evalUserManagedCap newLocals capBody emittedEvent
                  Nothing ->
                    throwExecutionError info (CapNotInstalled qualCapToken)
              Just managedCap -> do
                let emittedEvent = fqctToPactEvent origToken <$ guard (ecType == NormalCapEval)
                evalUserManagedCap newLocals capBody emittedEvent managedCap
          -- handle autonomous caps
          AutoManagedMeta -> do
            -- Find the capability post-filtering
            let emittedEvent = fqctToPactEvent origToken <$ guard (ecType == NormalCapEval)
            mgdCaps <- use (esCaps . csManaged)
            case find ((==) qualCapToken . _mcCap) mgdCaps of
              Nothing -> do
                msgCaps <- S.unions <$> viewEvalEnv eeMsgSigs
                case find (== qualCapToken) msgCaps of
                  Just c -> do
                    let c' = set ctName fqn c
                    installCap info env c' False >>= evalAutomanagedCap emittedEvent newLocals capBody
                  Nothing ->
                    throwExecutionError info (CapNotInstalled qualCapToken)
              Just managedCap ->
                evalAutomanagedCap emittedEvent newLocals capBody managedCap
      DefEvent -> do
        oldCapsBeingEvaluated <- use (esCaps.csCapsBeingEvaluated)
        let event = Just (fqctToPactEvent origToken)
        let inCapEnv = set ceInCap True $ set ceLocal newLocals env
        (esCaps . csSlots) %= (CapSlot qualCapToken []:)
        (esCaps . csCapsBeingEvaluated) %= S.insert qualCapToken
        _ <- evalWithStackFrame info capStackFrame Nothing (evaluate inCapEnv capBody)
        (esCaps . csCapsBeingEvaluated) .= oldCapsBeingEvaluated
        evalWithCapBody info popType Nothing event env contbody
      -- Not automanaged _nor_ user managed.
      Unmanaged -> do
        let inCapEnv = set ceInCap True $ set ceLocal newLocals env
        oldCapsBeingEvaluated <- use (esCaps.csCapsBeingEvaluated)
        (esCaps . csSlots) %= (CapSlot qualCapToken []:)
        (esCaps . csCapsBeingEvaluated) %= S.insert qualCapToken
        -- we ignore the capbody here
        _ <- evalWithStackFrame info capStackFrame Nothing $ evaluate inCapEnv capBody
        (esCaps . csCapsBeingEvaluated) .= oldCapsBeingEvaluated
        case ecType of
          NormalCapEval -> do
            evalWithCapBody info popType Nothing Nothing env contbody
          TestCapEval ->
            -- todo: check with prod the return type of testCap
            return VUnit
  qualCapName = fqnToQualName fqn
  qualCapToken = CapToken qualCapName args
  capStackFrame = StackFrame fqn args SFDefcap info
  -- This function is handles both evaluating the manager function for the installed parameter
  -- and continuing evaluation for the actual capability body.
  evalUserManagedCap env' capBody emitted managedCap = case _mcManaged managedCap of
    ManagedParam mpfqn oldV managedIx -> do
      dfun <- getDefun info mpfqn
      dfunClo <- mkDefunClosure dfun mpfqn env
      newV <- maybe (failInvariant info (InvariantInvalidManagedCapIndex managedIx fqn)) pure (args ^? ix managedIx)
      -- Set the mgr fun to evaluate after we apply the capability body
      -- NOTE: test-capability doesn't actually run the manager function, it just runs the cap pop then
      -- pops it. It would be great to do without this, but a lot of our regressions rely on this.
      let inCapEnv = set ceInCap True $ set ceLocal env' $ env
      let inCapBodyToken = _mcOriginalCap managedCap
      oldCapsBeingEvaluated <- use (esCaps.csCapsBeingEvaluated)
      -- BIG SEMANTICS NOTE HERE
      -- the cap slot here that we push should NOT be the qualified original token.
      -- Instead, it's the original token from the installed from the static cap. Otherwise, enforce checks
      -- within the cap body will fail (That is, keyset enforcement). Instead, once we are evaluating the body,
      -- we pop the current cap stack, then replace the head with the original intended token.
      -- this is done in `CapBodyC` and this is the only way to do this.
      (esCaps . csSlots) %= (CapSlot inCapBodyToken []:)
      (esCaps . csCapsBeingEvaluated) %= S.insert inCapBodyToken
      _ <- evalWithStackFrame info capStackFrame Nothing (evaluate inCapEnv capBody)
      (esCaps . csCapsBeingEvaluated) .= oldCapsBeingEvaluated
      when (ecType == NormalCapEval) $ do
        updatedV <- enforcePactValue info =<< applyLam (C dfunClo) [VPactValue oldV, VPactValue newV]
        let mcap' = unsafeUpdateManagedParam updatedV managedCap
        (esCaps . csManaged) %= S.insert mcap'
      evalWithCapBody info popType (Just qualCapToken) emitted env contbody
    _ -> failInvariant info (InvariantInvalidManagedCapKind "expected user managed, received automanaged")
  evalAutomanagedCap emittedEvent env' capBody managedCap = case _mcManaged managedCap of
    AutoManaged b -> do
      if b then throwUserRecoverableError info OneShotCapAlreadyUsed
      else do
        let newManaged = AutoManaged True
        oldCapsBeingEvaluated <- use (esCaps.csCapsBeingEvaluated)
        esCaps . csManaged %= S.union (S.singleton (set mcManaged newManaged managedCap))
        esCaps . csSlots %= (CapSlot qualCapToken []:)
        (esCaps . csCapsBeingEvaluated) %= S.insert qualCapToken
        let inCapEnv = set ceLocal env' $ set ceInCap True $ env
        _ <- evalWithStackFrame info capStackFrame Nothing (evaluate inCapEnv capBody)
        (esCaps . csCapsBeingEvaluated) .= oldCapsBeingEvaluated

        evalWithCapBody info popType Nothing emittedEvent env contbody
    _ -> failInvariant info (InvariantInvalidManagedCapKind "expected automanaged, received user managed")

evalWithCapBody
  :: (IsBuiltin b)
  => i
  -> CapPopState
  -> Maybe (CapToken QualifiedName PactValue)
  -> Maybe (PactEvent PactValue)
  -> DirectEnv e b i
  -> EvalTerm b i
  -> EvalM e b i (EvalValue e b i)
evalWithCapBody info cappop mcap mevent env capbody = do
  maybe (pure ()) emitEventUnsafe mevent
  case mcap of
    Nothing -> do
      v <- evaluate env capbody
      popCap info cappop v
    Just cap -> use (esCaps . csSlots) >>= \case
      (CapSlot _ tl:rest) -> do
        (esCaps . csSlots) .= (CapSlot cap tl:rest)
        v <- evaluate env capbody
        popCap info cappop v
      [] -> failInvariant info InvariantEmptyCapStackFailure

popCap
  :: i
  -> CapPopState
  -> EvalValue e b i
  -> EvalM e b i (EvalValue e b i)
popCap info cappop v = case cappop of
  PopCapInvoke -> v <$ (esCaps . csSlots %= safeTail)
  PopCapComposed -> do
    use (esCaps . csSlots) >>= \case
      cap:cs -> do
        let csList = _csCap cap : _csComposed cap
            caps' = over (_head . csComposed) (++ csList) cs
        (esCaps . csSlots) .= caps'
        return v
      [] -> failInvariant info InvariantEmptyCapStackFailure



-- Todo: fail invariant
-- Todo: is this enough checks for ndynref?
nameToFQN
  :: i
  -> DirectEnv e b i
  -> Name
  -> EvalM e b i FullyQualifiedName
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
  :: (IsBuiltin b)
  => i
  -> DirectEnv e b i
  -> TableValue
  -> GuardTableOp
  -> EvalM e b i ()
guardTable i env (TableValue tn mh _) dbop = do
  let mn = _tableModuleName tn
  checkLocalBypass $
    guardForModuleCall i env mn $ do
      mdl <- getModule i (view cePactDb env) mn
      enforceBlessedHashes i mdl mh
  where
  checkLocalBypass notBypassed = do
    enabled <- isExecutionFlagSet FlagAllowReadInLocal
    case dbop of
      GtWrite -> notBypassed
      GtCreateTable -> notBypassed
      _ | enabled -> return ()
        | otherwise -> notBypassed


-- Todo: should we typecheck / arity check here?
createUserGuard
  :: i
  -> FullyQualifiedName
  -> [PactValue]
  -> EvalM e b i (EvalValue e b i)
createUserGuard info fqn args =
  lookupFqName fqn >>= \case
    Just (Dfun _) ->
      return (VGuard (GUserGuard (UserGuard (fqnToQualName fqn) args)))
    Just d ->
      -- Note: this error is not recoverable in prod
      -- <interactive>:0:26:Error: User guard closure must be defun, found: defcap
      -- at <interactive>:0:7: (create-user-guard ((defcap m.g:<a> ())))
      -- at <interactive>:0:0: (try 1 (native `create-user-guard`  Defines a custom guar...)
      throwExecutionError info $ UserGuardMustBeADefun (fqnToQualName fqn) (defKind (_fqModule fqn) d)
    Nothing ->
      failInvariant info (InvariantUnboundFreeVariable fqn)

enforceNotWithinDefcap
  :: i
  -> DirectEnv e b i
  -> T.Text
  -> EvalM e b i ()
enforceNotWithinDefcap info env form =
  when (_ceInCap env) $ throwExecutionError info (FormIllegalWithinDefcap form)

enforceBool :: i -> EvalValue e b i -> EvalM e b i Bool
enforceBool info = \case
  VBool b -> pure b
  VPactValue v' -> throwExecutionError info (ExpectedBoolValue v')
  _ -> throwExecutionError info ExpectedPactValue

enforceBool' :: i -> EvalValue e b i -> EvalM e b i (EvalValue e b i)
enforceBool' info = \case
  v@VBool{} -> pure v
  VPactValue v' -> throwExecutionError info (ExpectedBoolValue v')
  _ -> throwExecutionError info ExpectedPactValue

enforceString ::i -> EvalValue e b i -> EvalM e b i Text
enforceString info = \case
  VString b -> pure b
  VPactValue v' -> throwExecutionError info $ ExpectedStringValue v'
  _ -> throwExecutionError info $ ExpectedPactValue


enforceCapToken :: i -> EvalValue e b i -> EvalM e b i (CapToken FullyQualifiedName PactValue)
enforceCapToken info = \case
  VCapToken b -> pure b
  VPactValue v' -> throwExecutionError info $ ExpectedCapToken v'
  _ -> throwExecutionError info $ ExpectedPactValue

enforceBoolValue :: i -> EvalValue e b i -> EvalM e b i (EvalValue e b i)
enforceBoolValue info = \case
  VBool b -> pure (VBool b)
  VPactValue v' -> throwExecutionError info (ExpectedBoolValue v')
  _ -> throwExecutionError info ExpectedPactValue

enforceUserAppClosure :: i -> EvalValue e b i -> EvalM e b i (CanApply e b i)
enforceUserAppClosure info = \case
  VClosure c -> case c of
    C clo -> pure (C clo)
    LC clo -> pure (LC clo)
    N clo -> pure (N clo)
    DPC clo -> pure (DPC clo)
    CT clo -> pure (CT clo)
    _ -> throwExecutionError info CannotApplyPartialClosure
  _ -> throwExecutionError info CannotApplyValueToNonClosure


enforcePactValue :: i -> EvalValue e b i -> EvalM e b i PactValue
enforcePactValue info = \case
  VPactValue pv -> pure pv
  _ -> throwExecutionError info ExpectedPactValue

enforcePactValue' :: i -> EvalValue e b i -> EvalM e b i (EvalValue e b i)
enforcePactValue' info = \case
  VPactValue pv -> pure (VPactValue pv)
  _ -> throwExecutionError info ExpectedPactValue

catchRecoverable :: forall e b i a. EvalM e b i a -> (i -> UserRecoverableError -> EvalM e b i a) -> EvalM e b i a
catchRecoverable act catch = do
  eState <- evalStateToErrorState <$> get
  catchError act (handler eState)
  where
  handler :: ErrorState i -> PactError i -> EvalM e b i a
  handler eState (PEUserRecoverableError err _ i) = do
    modify' (restoreFromErrorState eState)
    catch i err
  handler _ e = throwError e

readOnlyEnv :: DirectEnv e b i -> DirectEnv e b i
readOnlyEnv e
  | view (cePactDb . pdbPurity) e == PSysOnly = e
  | otherwise =
      let pdb = view cePactDb e
          newPactdb =
              PactDb
             { _pdbPurity = PReadOnly
             , _pdbRead = _pdbRead pdb
             , _pdbWrite = \_ _ _ _ -> dbOpDisallowed
             , _pdbKeys = \_ -> dbOpDisallowed
             , _pdbCreateUserTable = \_ -> dbOpDisallowed
             , _pdbBeginTx = \_ -> dbOpDisallowed
             , _pdbCommitTx = dbOpDisallowed
             , _pdbRollbackTx = dbOpDisallowed
             }
      in set cePactDb newPactdb e

sysOnlyEnv :: forall e b i. DirectEnv e b i -> DirectEnv e b i
sysOnlyEnv e
  | view (cePactDb . pdbPurity) e == PSysOnly = e
  | otherwise =
  let newPactdb =
          PactDb
         { _pdbPurity = PSysOnly
         , _pdbRead = read'
         , _pdbWrite = \_ _ _ _ -> dbOpDisallowed
         , _pdbKeys = const dbOpDisallowed
         , _pdbCreateUserTable = \_ -> dbOpDisallowed
         , _pdbBeginTx = const dbOpDisallowed
         , _pdbCommitTx = dbOpDisallowed
         , _pdbRollbackTx = dbOpDisallowed
         }
  in set cePactDb newPactdb e
  where
  pdb = view cePactDb e
  read' :: Domain k v b i -> k -> IO (Maybe v)
  read' dom k = case dom of
    DUserTables _ -> dbOpDisallowed
    _ -> _pdbRead pdb dom k

evalWithStackFrame :: i -> StackFrame i -> Maybe Type -> EvalM e b i (EvalValue e b i) -> EvalM e b i (EvalValue e b i)
evalWithStackFrame info sf mty act = do
  esStack %= (sf:)
#ifdef WITH_FUNCALL_TRACING
  timeEnter <- liftIO $ getTime ProcessCPUTime
  esTraceOutput %= (TraceFunctionEnter timeEnter sf info:)
#endif
  v <- act
  esStack %= safeTail
  pv <- enforcePactValue info v
  rtcEnabled <- isExecutionFlagSet FlagDisableRuntimeRTC
  unless rtcEnabled $ maybeTCType info mty pv
#ifdef WITH_FUNCALL_TRACING
  timeExit <- liftIO $ getTime ProcessCPUTime
  esTraceOutput %= (TraceFunctionExit timeExit sf info:)
#endif
  return (VPactValue pv)
{-# INLINE evalWithStackFrame #-}

applyLamUnsafe
  :: (IsBuiltin b)
  => CanApply e b i
  -> [EvalValue e b i]
  -> EvalM e b i (EvalValue e b i)
applyLamUnsafe = applyLam

applyLam
  :: (IsBuiltin b)
  => CanApply e b i
  -> [EvalValue e b i]
  -> EvalM e b i (EvalValue e b i)
applyLam nclo@(N (NativeFn b env fn arity i)) args
  | arity == argLen = do
    chargeFlatNativeGas i b
    fn i b env args
  | argLen > arity = throwExecutionError i ClosureAppliedToTooManyArgs
  | null args = return (VClosure nclo)
  | otherwise =
    apply' arity [] args
  where
  argLen = length args
  apply' !a pa (x:xs) = apply' (a - 1) (x:pa) xs
  apply' !a pa [] =
    return (VPartialNative (PartialNativeFn b env fn a pa i))
applyLam (CT (CapTokenClosure fqn argtys arity i)) args
  | arity == argLen = do
    chargeGasArgs i (GAApplyLam (Just fqn) (fromIntegral argLen))
    args' <- traverse (enforcePactValue i) args
    zipWithM_ (\arg ty -> maybeTCType i ty arg) args' argtys
    return (VPactValue (PCapToken (CapToken fqn args')))
  | otherwise = throwExecutionError i ClosureAppliedToTooManyArgs
  where
  argLen = length args
applyLam vc@(C (Closure fqn ca arity term mty env cloi)) args
  | arity == argLen = case ca of
    ArgClosure cloargs -> do
      chargeGasArgs cloi (GAApplyLam (Just fqn) argLen)
      args' <- traverse (enforcePactValue cloi) args
      zipWithM_ (\arg (Arg _ ty _) -> maybeTCType cloi ty arg) args' (NE.toList cloargs)
      let sf = StackFrame fqn args' SFDefun cloi
          varEnv = RAList.fromList (reverse args)
      evalWithStackFrame cloi sf mty (evaluate (set ceLocal varEnv env) term)
    NullaryClosure -> do
      evalWithStackFrame cloi (StackFrame fqn [] SFDefun cloi) mty $ evaluate (set ceLocal mempty env) term
  | argLen > arity = throwExecutionError cloi ClosureAppliedToTooManyArgs
  | otherwise = case ca of
    NullaryClosure -> throwExecutionError cloi ClosureAppliedToTooManyArgs
    ArgClosure cloargs
      | null args ->
        return (VClosure vc)
      | otherwise -> do
        chargeGasArgs cloi (GAApplyLam (Just fqn) argLen)
        apply' mempty (NE.toList cloargs) args
  where
  argLen = length args
  -- Here we enforce an argument to a user fn is a
  apply' e (Arg _ ty _:tys) (x:xs) = do
    x' <- enforcePactValue cloi x
    maybeTCType cloi ty x'
    apply' (RAList.cons (VPactValue x') e) tys xs
  apply' e (ty:tys) [] = do
    let env' = set ceLocal e env
        -- Todo: fix partial SF args
        pclo = PartialClosure (Just (StackFrame fqn [] SFDefun cloi)) (ty :| tys) (length tys + 1) term mty env' cloi
    return (VPartialClosure pclo)
  apply' _ [] _ = throwExecutionError cloi ClosureAppliedToTooManyArgs

applyLam (LC (LamClosure ca arity term mty env cloi)) args
  | arity == argLen = case ca of
    ArgClosure _ -> do
      -- Todo: maybe lambda application should mangle some sort of name?
      chargeGasArgs cloi (GAApplyLam Nothing argLen)
      let locals = view ceLocal env
          locals' = foldl' (flip RAList.cons) locals args
      evaluate (set ceLocal locals' env) term
    NullaryClosure -> do
      evaluate env term >>= enforcePactValue' cloi
  | argLen > arity = throwExecutionError cloi ClosureAppliedToTooManyArgs
  | otherwise = case ca of
      NullaryClosure -> throwExecutionError cloi ClosureAppliedToTooManyArgs
      ArgClosure cloargs -> do
        chargeGasArgs cloi (GAApplyLam Nothing argLen)
        apply' (view ceLocal env) (NE.toList cloargs) args
  where
  argLen = length args
  -- Todo: runtime TC here
  apply' e (Arg _ ty _:tys) (x:xs) = do
    x' <- enforcePactValue cloi x
    maybeTCType cloi ty x'
    apply' (RAList.cons (VPactValue x') e) tys xs
  apply' e [] [] = do
    evaluate (set ceLocal e env) term
  apply' e (ty:tys) [] =
    return (VPartialClosure (PartialClosure Nothing (ty :| tys) (length tys + 1) term mty (set ceLocal e env) cloi))
  apply' _ [] _ = throwExecutionError cloi ClosureAppliedToTooManyArgs
applyLam (PC (PartialClosure li argtys _ term mty env cloi)) args = do
  chargeGasArgs cloi (GAApplyLam (_sfName <$> li) (length args))
  apply' (view ceLocal env) (NE.toList argtys) args
  where
  apply' e (Arg _ ty _:tys) (x:xs) = do
    x' <- enforcePactValue cloi x
    maybeTCType cloi ty x'
    apply' (RAList.cons (VPactValue x') e) tys xs
  apply' e [] [] = do
    case li of
      Just sf -> do
        evalWithStackFrame cloi sf mty $ evaluate (set ceLocal e env) term
      Nothing -> do
        evaluate (set ceLocal e env) term >>= enforcePactValue' cloi
  apply' e (ty:tys) [] = do
    let pclo = PartialClosure li (ty :| tys) (length tys + 1) term mty (set ceLocal e env) cloi
    return (VPartialClosure pclo)
  apply' _ [] _ = throwExecutionError cloi ClosureAppliedToTooManyArgs

applyLam (PN (PartialNativeFn b env fn arity pArgs i)) args
  | arity == argLen = do
    chargeFlatNativeGas i b
    fn i b env (reverse pArgs ++ args)
  | argLen > arity = throwExecutionError i ClosureAppliedToTooManyArgs
  | otherwise = apply' arity [] args
  where
  argLen = length args
  apply' !a pa (x:xs) = apply' (a - 1) (x:pa) xs
  apply' !a pa [] =
    return (VPartialNative (PartialNativeFn b env fn a pa i))
applyLam (DPC (DefPactClosure fqn argtys arity env i)) args
  | arity == argLen = case argtys of
    ArgClosure cloargs -> do
      -- Todo: defpact has much higher overhead, we must charge a bit more gas for this
      chargeGasArgs i (GAApplyLam (Just fqn) (fromIntegral argLen))
      args' <- traverse (enforcePactValue i) args
      zipWithM_ (\arg (Arg _ ty _) -> maybeTCType i ty arg) args' (NE.toList cloargs)
      let pc = DefPactContinuation (fqnToQualName fqn) args'
          env' = set ceLocal (RAList.fromList (reverse args)) env
      initPact i pc env'
    NullaryClosure -> do
      chargeGasArgs i (GAApplyLam (Just fqn) (fromIntegral argLen))
      let pc = DefPactContinuation (fqnToQualName fqn) []
          env' = set ceLocal mempty env
      -- Todo: defpact has much higher overhead, we must charge a bit more gas for this
      initPact i pc env'
  | otherwise = throwExecutionError i ClosureAppliedToTooManyArgs
  where
  argLen = length args



------------------------------------------------------
-- Guards
------------------------------------------------------

enforceGuard
  :: (IsBuiltin b)
  => i
  -> DirectEnv e b i
  -> Guard QualifiedName PactValue
  -> EvalM e b i Bool
enforceGuard info env g = case g of
  GKeyset ks -> do
    isKeysetInSigs info env ks
  GKeySetRef ksn -> do
    isKeysetNameInSigs info  env ksn
  GUserGuard ug -> runUserGuard info env ug
  GCapabilityGuard cg -> enforceCapGuard info cg
  GModuleGuard (ModuleGuard mn _) -> calledByModule mn >>= \case
    True -> return True
    False -> do
      acquireModuleAdminCapability info env mn
      return True
  GDefPactGuard (DefPactGuard dpid _) -> do
    curDpid <- getDefPactId info
    if curDpid == dpid
       then return True
       else throwUserRecoverableError info $
         CapabilityPactGuardInvalidPactId curDpid dpid

guardForModuleCall
  :: (IsBuiltin b)
  => i
  -> DirectEnv e b i
  -> ModuleName
  -> EvalM e b i ()
  -> EvalM e b i ()
guardForModuleCall i env currMod onFound =
  findCallingModule >>= \case
    Just mn | mn == currMod -> onFound
    _ -> do
      mc <- use (esCaps . csModuleAdmin)
      if S.member currMod mc then onFound
      else getModule i (view cePactDb env) currMod >>= acquireModuleAdmin i env

-- | Acquires module admin for a known module
-- NOTE: This function should only be called _after_
-- checking whether `esCaps . csModuleAdmin` for the particular
-- module is in scope
acquireModuleAdmin
  :: (IsBuiltin b)
  => i
  -> DirectEnv e b i
  -> EvalModule b i
  -> EvalM e b i ()
acquireModuleAdmin i env mdl = do
  case _mGovernance mdl of
    KeyGov ksn -> do
      () <$ isKeysetNameInSigs i env ksn
    CapGov (FQName fqn) -> do
      let wcapBody = Constant LUnit i
      () <$ evalCap i env (CapToken fqn []) PopCapInvoke NormalCapEval wcapBody
  (esCaps . csModuleAdmin) %= S.insert (_mName mdl)


acquireModuleAdminCapability
  :: (IsBuiltin b)
  => i
  -> DirectEnv e b i
  -> ModuleName
  -> EvalM e b i ()
acquireModuleAdminCapability i env mname = do
  sc <- S.member mname <$> use (esCaps . csModuleAdmin)
  unless sc $ getModule i (_cePactDb env) mname >>= acquireModuleAdmin i env


runUserGuard
  :: (IsBuiltin b)
  => i
  -> DirectEnv e b i
  -> UserGuard QualifiedName PactValue
  -> EvalM e b i Bool
runUserGuard info env (UserGuard qn args) =
  getModuleMemberWithHash info (_cePactDb env) qn >>= \case
    (Dfun d, mh) -> do
      when (length (_dfunArgs d) /= length args) $ throwExecutionError info CannotApplyPartialClosure
      let env' = sysOnlyEnv env
      clo <- mkDefunClosure d (qualNameToFqn qn mh) env'
      -- Todo: sys only here
      True <$ (applyLam (C clo) (VPactValue <$> args) >>= enforcePactValue info)
    (d, _) -> throwExecutionError info (UserGuardMustBeADefun qn (defKind (_qnModName qn) d))

enforceCapGuard
  :: i
  -> CapabilityGuard QualifiedName PactValue
  -> EvalM e b i Bool
enforceCapGuard info cg@(CapabilityGuard qn args mpid) = case mpid of
  Nothing -> enforceCap
  Just pid -> do
    currPid <- getDefPactId info
    if currPid == pid then enforceCap
    else throwUserRecoverableError info $
      CapabilityPactGuardInvalidPactId currPid pid
  where
  enforceCap = do
    cond <- isCapInStack (CapToken qn args)
    if cond then return True
    else do
      throwUserRecoverableError info $ CapabilityGuardNotAcquired cg

-- Keyset Code
isKeysetInSigs
  :: (IsBuiltin b)
  => i
  -> DirectEnv e b i
  -> KeySet
  -> EvalM e b i Bool
isKeysetInSigs info env (KeySet kskeys ksPred) = do
  matchedSigs <- M.filterWithKey matchKey <$> viewEvalEnv eeMsgSigs
  sigs <- checkSigCaps matchedSigs
  runPred (M.size sigs)
  where
  matchKey k _ = k `elem` kskeys
  atLeast t m = m >= t
  count = S.size kskeys
  run p matched =
    if p count matched then pure True
    else
      throwUserRecoverableError info $ KeysetPredicateFailure ksPred kskeys
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
          p <- enforceBool info =<< applyLam (C clo) [VInteger (fromIntegral count), VInteger (fromIntegral matched)]
          unless p $ throwUserRecoverableError info $ KeysetPredicateFailure ksPred kskeys
          pure p
        _ -> throwExecutionError info (InvalidCustomKeysetPredicate "expected defun")
    TBN (BareName bn) -> do
      m <- viewEvalEnv eeNatives
      case M.lookup bn m of
        Just b -> do
          let builtins = view ceBuiltins env
          let nativeclo = builtins info b env
          p <- enforceBool info =<< applyLam (N nativeclo) [VInteger (fromIntegral count), VInteger (fromIntegral matched)]
          unless p $ throwUserRecoverableError info $ KeysetPredicateFailure ksPred kskeys
          pure p
        Nothing ->
          throwExecutionError info (InvalidCustomKeysetPredicate "expected native")

isKeysetNameInSigs
  :: (IsBuiltin b)
  => i
  -> DirectEnv e b i
  -> KeySetName
  -> EvalM e b i Bool
isKeysetNameInSigs info env ksn = do
  pdb <- viewEvalEnv eePactDb
  liftDbFunction info (_pdbRead pdb DKeySets ksn) >>= \case
    Just ks -> isKeysetInSigs info env ks
    Nothing ->
      throwExecutionError info (NoSuchKeySet ksn)


------------------------------------------------------
-- Capabilities
------------------------------------------------------

requireCap
  :: i
  -> FQCapToken
  -> EvalM e b i (EvalValue e b i)
requireCap info (CapToken fqn args) = do
  let qualCapToken = CapToken (fqnToQualName fqn) args
  capInStack <- isCapInStack qualCapToken
  if capInStack then return (VBool True)
  else throwUserRecoverableError info (CapabilityNotGranted qualCapToken)

isCapInStack
  :: CapToken QualifiedName PactValue
  -> EvalM e b i Bool
isCapInStack ct = do
  capSet <- getAllStackCaps
  pure $ S.member ct capSet


isCapInStack'
  :: CapToken FullyQualifiedName PactValue
  -> EvalM e b i Bool
isCapInStack' (CapToken fqn args) =
  isCapInStack (CapToken (fqnToQualName fqn) args)

composeCap
  :: (IsBuiltin b)
  => i
  -> DirectEnv e b i
  -> FQCapToken
  -> EvalM e b i (EvalValue e b i)
composeCap info env origToken =
  isCapInStack' origToken >>= \case
    False ->
      evalCap info env origToken PopCapComposed NormalCapEval (Constant (LBool True) info)
    True ->
      return (VBool True)

filterIndex :: Int -> [a] -> [a]
filterIndex i xs = [x | (x, i') <- zip xs [0..], i /= i']

findMsgSigCap :: Int -> CapToken QualifiedName PactValue -> CapToken QualifiedName PactValue -> Bool
findMsgSigCap cix ct1 ct2 =
  _ctName ct1 == _ctName ct2 && (_ctArgs ct1 == filterIndex cix (_ctArgs ct2))

-- Todo:
-- `capAutonomous` are what we should use to match semantics accurately.
installCap
  :: i
  -> DirectEnv e b i
  -> FQCapToken
  -> Bool
  -> EvalM e b i (ManagedCap QualifiedName PactValue)
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
        capAlreadyInstalled <- S.member mcap <$> use (esCaps . csManaged)
        when capAlreadyInstalled $ throwExecutionError info (CapAlreadyInstalled ct)
        (esCaps . csManaged) %= S.insert mcap
        when autonomous $
          (esCaps . csAutonomous) %= S.insert ct
        pure mcap
      AutoManagedMeta -> do
        let mcapType = AutoManaged False
            mcap = ManagedCap ct ct mcapType
        capAlreadyInstalled <- S.member mcap <$> use (esCaps . csManaged)
        when capAlreadyInstalled $ throwExecutionError info (CapAlreadyInstalled ct)
        (esCaps . csManaged) %= S.insert mcap
        when autonomous $
          (esCaps . csAutonomous) %= S.insert ct
        pure mcap
    DefEvent ->
      throwExecutionError info (InvalidManagedCap fqn)
    Unmanaged -> throwExecutionError info (InvalidManagedCap fqn)




------------------------------------------------------
-- DefPacts
------------------------------------------------------
initPact
  :: (IsBuiltin b)
  => i
  -> DefPactContinuation QualifiedName PactValue
  -> DirectEnv e b i
  -> EvalM e b i (EvalValue e b i)
initPact i pc cenv = do
  case view ceDefPactStep cenv of
    Nothing -> do
      pHash <- viewEvalEnv eeHash
      let
        pStep = DefPactStep 0 False (hashToDefPactId pHash) Nothing
        cenv' = set ceDefPactStep (Just pStep) cenv
      applyPact i pc pStep cenv' mempty
    Just ps ->
      let
        DefPactId p = _psDefPactId ps
        npId = hashToDefPactId (pactHash (T.encodeUtf8 p <> ":" <> encodeStable pc))
        pStep = DefPactStep (_psStep ps) (_psRollback ps) npId Nothing
      in applyNestedPact i pc pStep cenv
  where
    hashToDefPactId = DefPactId . hashToText

-- | Important check for nested pacts:
--   Nested step must be equal to the parent step after execution.
nestedPactsNotAdvanced :: DefPactExec -> DefPactStep -> Bool
nestedPactsNotAdvanced resultState ps =
  any (\npe -> _peStep npe /= _psStep ps) (_peNestedDefPactExec resultState)
{-# INLINE nestedPactsNotAdvanced #-}

applyPact
  :: (IsBuiltin b)
  => i
  -> DefPactContinuation QualifiedName PactValue
  -> DefPactStep
  -> DirectEnv e b i
  -> M.Map DefPactId DefPactExec
  -> EvalM e b i (EvalValue e b i)
applyPact i pc ps cenv nested = use esDefPactExec >>= \case
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

      esDefPactExec .= (Just pe)
      let sf = StackFrame (qualNameToFqn (pc ^. pcName) mh) (pc ^. pcArgs) SFDefPact i

      result <- case (ps ^. psRollback, step) of
        (False, _) ->
          evalWithStackFrame i sf Nothing $ evaluate cenv (ordinaryDefPactStepExec step)
        (True, StepWithRollback _ rollbackExpr) ->
          evalWithStackFrame i sf Nothing $ evaluate cenv rollbackExpr
        (True, Step{}) ->
          throwExecutionError i (DefPactStepHasNoRollback ps)

      -- After evaluation, check the result state
      use esDefPactExec >>= \case
        Nothing -> failInvariant i $ InvariantPactExecNotInEnv Nothing
        Just resultExec -> case cenv ^. ceDefPactStep of
          Nothing -> failInvariant i (InvariantPactStepNotInEnv Nothing)
          Just ps' -> do
            let
              pdb = view cePactDb cenv
              isLastStep = _psStep ps' == pred (_peStepCount resultExec)
              done = (not (_psRollback ps') && isLastStep) || _psRollback ps'
            when (nestedPactsNotAdvanced resultExec ps') $
              throwExecutionError i (NestedDefpactsNotAdvanced (_peDefPactId resultExec))
            evalWrite i pdb Write DDefPacts (_psDefPactId ps') (if done then Nothing else Just resultExec)
            emitXChainEvents (_psResume ps') resultExec
            return result

    (_, mh) -> failInvariant i (InvariantExpectedDefPact (qualNameToFqn (pc ^. pcName) mh))

applyNestedPact
  :: (IsBuiltin b)
  => i
  -> DefPactContinuation QualifiedName PactValue
  -> DefPactStep
  -> DirectEnv e b i
  -> EvalM e b i (EvalValue e b i)
applyNestedPact i pc ps cenv = use esDefPactExec >>= \case
  Nothing -> failInvariant i $ InvariantPactExecNotInEnv Nothing

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

      esDefPactExec .= (Just exec)
      let
        cenv' = set ceDefPactStep (Just ps) cenv
      let contFqn = qualNameToFqn (pc ^. pcName) mh
          sf = StackFrame contFqn (pc ^. pcArgs) SFDefPact i
      result <- case (ps ^. psRollback, step) of
        (False, _) ->
          evalWithStackFrame i sf Nothing $ evaluate cenv' (ordinaryDefPactStepExec step)
        (True, StepWithRollback _ rollbackExpr) ->
          evalWithStackFrame i sf Nothing $ evaluate cenv' rollbackExpr
        (True, Step{}) -> throwExecutionError i (DefPactStepHasNoRollback ps)

      use esDefPactExec >>= \case
        Nothing -> failInvariant i $ InvariantPactExecNotInEnv Nothing
        Just resultExec -> do
          when (nestedPactsNotAdvanced resultExec ps) $
            throwExecutionError i (NestedDefpactsNotAdvanced (_peDefPactId resultExec))
          let npe = pe & peNestedDefPactExec %~ M.insert (_psDefPactId ps) resultExec
          esDefPactExec .= (Just npe)
          return result
    (_, mh) -> failInvariant i (InvariantExpectedDefPact (qualNameToFqn (pc ^. pcName) mh))

resumePact
  :: (IsBuiltin b)
  => i
  -> DirectEnv e b i
  -> Maybe DefPactExec
  -> EvalM e b i (EvalValue e b i)
resumePact i env crossChainContinuation = viewEvalEnv eeDefPactStep >>= \case
  Nothing -> throwExecutionError i DefPactStepNotInEnvironment
  Just ps -> do
    pdb <- viewEvalEnv eePactDb
    dbState <- liftDbFunction i (_pdbRead pdb DDefPacts (_psDefPactId ps))
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
        --resumeDefPactExec :: CEKEval step e b i, IsBuiltin b => DefPactExec -> m (CEKEvalResult step e b i)
        resumeDefPactExec pe = do
          when (_psDefPactId ps /= _peDefPactId pe) $
            throwExecutionError i (DefPactIdMismatch (_psDefPactId ps) (_peDefPactId pe))

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
          applyPact i pc ps env' (_peNestedDefPactExec pe)

emitXChainEvents
  :: Maybe Yield
  -- ^ from '_psResume', indicating a cross-chain resume.
  -> DefPactExec
   -- ^ tested for yield provenance to indicate a cross-chain yield.
  -> EvalM e b i ()
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




--------------------------
-- Gas-related code
--------------------------
constantWorkNodeGas :: MilliGas
constantWorkNodeGas = (MilliGas 50)

unconsWorkNodeGas :: MilliGas
unconsWorkNodeGas = (MilliGas 100)

tryNodeGas :: MilliGas
tryNodeGas = (MilliGas 100)

----------------------------------------------------------------------
-- Our builtin definitions start here
----------------------------------------------------------------------
unaryIntFn :: (IsBuiltin b) => (Integer -> Integer) -> NativeFunction e b i
unaryIntFn op info b _env = \case
  [VLiteral (LInteger i)] ->
    return (VLiteral (LInteger (op i)))
  args -> argsError info b args
{-# INLINE unaryIntFn #-}

binaryIntFn
  :: (IsBuiltin b)
  => (Integer -> Integer -> Integer)
  -> NativeFunction e b i
binaryIntFn op info b _env = \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> return (VLiteral (LInteger (op i i')))
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
roundingFn :: (IsBuiltin b) => (Rational -> Integer) -> NativeFunction e b i
roundingFn op info b _env = \case
  [VLiteral (LDecimal d)] ->
    return (VLiteral (LInteger (truncate (roundTo' op 0 d))))
  [VDecimal d, VInteger prec] ->
    return (VLiteral (LDecimal (roundTo' op (fromIntegral prec) d)))
  args -> argsError info b args
{-# INLINE roundingFn #-}

---------------------------------
-- Arithmetic Ops
------------------------------
rawAdd :: (IsBuiltin b) => NativeFunction e b i
rawAdd info b _env = \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> do
    chargeGasArgs info (GIntegerOpCost PrimOpAdd i i')
    return (VLiteral (LInteger (i + i')))
  -- Overloaded decimal cases
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> do
    decimalAdd i i'
  [VLiteral (LInteger i), VLiteral (LDecimal i')] -> do
    decimalAdd (fromInteger i) i'
  [VLiteral (LDecimal i), VLiteral (LInteger i')] -> do
    decimalAdd i (Decimal 0 i')

  [VLiteral (LString i), VLiteral (LString i')] -> do
    chargeGasArgs info (GConcat (TextConcat (GasTextLength (T.length i + T.length i'))))
    return  (VLiteral (LString (i <> i')))
  [VObject l, VObject r] -> do
    chargeGasArgs info (GConcat (ObjConcat (M.size l + M.size r)))
    let o' = VObject (l `M.union` r)
    return o'
  [VList l, VList r] -> do
    chargeGasArgs info (GConcat (ListConcat (GasListLength (V.length l + V.length r))))
    return (VList (l <> r))
  args -> argsError info b args
  where
  decimalAdd i i' = do
    chargeGasArgs info (GIntegerOpCost PrimOpAdd (decimalMantissa i) (decimalMantissa i'))
    return (VLiteral (LDecimal (i + i')))

rawSub :: (IsBuiltin b) => NativeFunction e b i
rawSub info b _env = \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> do
    chargeGasArgs info (GIntegerOpCost PrimOpSub i i')
    return (VLiteral (LInteger (i - i')))
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
    return (VLiteral (LDecimal (i - i')))



rawMul :: (IsBuiltin b) => NativeFunction e b i
rawMul info b _env = \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> do
    chargeGasArgs info (GIntegerOpCost PrimOpMul i i')
    return (VLiteral (LInteger (i * i')))
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
    return (VLiteral (LDecimal (i * i')))

rawPow :: (IsBuiltin b) => NativeFunction e b i
rawPow info b _env = \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> do
    chargeGasArgs info $ GIntegerOpCost PrimOpPow i i'
    when (i' < 0) $ throwExecutionError info (ArithmeticException "negative exponent in integer power")
    -- Todo: move to iterated pow
    return (VLiteral (LInteger (i ^ i')))
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
    return (VLiteral (LDecimal (f2Dec result)))

rawLogBase :: forall e b i. (IsBuiltin b) => NativeFunction e b i
rawLogBase info b _env = \case
  [VLiteral (LInteger base), VLiteral (LInteger n)] -> do
    checkArgs base n
    let base' = fromIntegral base :: Double
        n' = fromIntegral n
        result = Musl.trans_logBase base' n'
    guardNanOrInf info result
    return (VLiteral (LInteger (round result)))
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
    return (VLiteral (LDecimal (f2Dec result)))
  checkArgs :: (Num a, Ord a) => a -> a -> EvalM e b i ()
  checkArgs base arg = do
    when (base < 0) $ throwExecutionError info (ArithmeticException "Negative log base")
    when (arg <= 0) $ throwExecutionError info (ArithmeticException "Non-positive log argument")


rawDiv :: (IsBuiltin b) => NativeFunction e b i
rawDiv info b _env = \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> do
    when (i' == 0) $ throwExecutionError info (ArithmeticException "div by zero")
    chargeGasArgs info (GIntegerOpCost PrimOpDiv i i')
    return (VLiteral (LInteger (div i i')))

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
    return (VLiteral (LDecimal (i / i')))


rawNegate :: (IsBuiltin b) => NativeFunction e b i
rawNegate info b _env = \case
  [VLiteral (LInteger i)] ->
    return (VLiteral (LInteger (negate i)))
  [VLiteral (LDecimal i)] ->
    return (VLiteral (LDecimal (negate i)))
  args -> argsError info b args

rawEq :: (IsBuiltin b) => NativeFunction e b i
rawEq info b _env = \case
  [VPactValue pv, VPactValue pv'] -> do
    isEq <- valEqGassed info pv pv'
    return (VBool isEq)
  args -> argsError info b args

modInt :: (IsBuiltin b) => NativeFunction e b i
modInt = binaryIntFn mod

rawNeq :: (IsBuiltin b) => NativeFunction e b i
rawNeq info b _env = \case
  [VPactValue pv, VPactValue pv'] -> do
    isEq <- valEqGassed info pv pv'
    return (VBool $ not isEq)
  args -> argsError info b args

rawGt :: (IsBuiltin b) => NativeFunction e b i
rawGt = defCmp (== GT)

rawLt :: (IsBuiltin b) => NativeFunction e b i
rawLt = defCmp (== LT)

rawGeq :: (IsBuiltin b) => NativeFunction e b i
rawGeq = defCmp (`elem` [GT, EQ])

rawLeq :: (IsBuiltin b) => NativeFunction e b i
rawLeq = defCmp (`elem` [LT, EQ])

defCmp :: (IsBuiltin b) => (Ordering -> Bool) -> NativeFunction e b i
defCmp predicate info b _env = \case
  args@[VLiteral lit1, VLiteral lit2] -> litCmpGassed info lit1 lit2 >>= \case
    Just ordering -> return $ VBool $ predicate ordering
    Nothing -> argsError info b args
    -- cmp (LInteger l) (LInteger r) = do
    --   chargeGasArgs info (GComparison (IntComparison l r))
    --   return $ VBool $ predicate (compare l r)
    -- cmp (LBool l) (LBool r) = return $ VBool $ predicate (compare l r)
    -- cmp (LDecimal l) (LDecimal r) = do
    --   chargeGasArgs info (GComparison (DecimalComparison l r))
    --   return $ VBool $ predicate (compare l r)
    -- cmp (LString l) (LString r) = do
    --   chargeGasArgs info (GComparison (TextComparison l r))
    --   return $ VBool $ predicate (compare l r)
    -- cmp LUnit LUnit = return $ VBool (predicate EQ)
    -- cmp _ _ = argsError info b args
  -- Todo: time comparisons
  [VTime l, VTime r] -> return $ VBool $ predicate (compare l r)
  args -> argsError info b args
{-# INLINE defCmp #-}

bitAndInt :: (IsBuiltin b) => NativeFunction e b i
bitAndInt = binaryIntFn (.&.)

bitOrInt :: (IsBuiltin b) => NativeFunction e b i
bitOrInt = binaryIntFn (.|.)

bitComplementInt :: (IsBuiltin b) => NativeFunction e b i
bitComplementInt = unaryIntFn complement

bitXorInt :: (IsBuiltin b) => NativeFunction e b i
bitXorInt = binaryIntFn xor

bitShiftInt :: (IsBuiltin b) => NativeFunction e b i
bitShiftInt =  binaryIntFn (\i s -> shift i (fromIntegral s))

rawAbs :: (IsBuiltin b) => NativeFunction e b i
rawAbs info b _env = \case
  [VLiteral (LInteger i)] -> do
    return (VLiteral (LInteger (abs i)))
  [VLiteral (LDecimal e)] -> do
    return (VLiteral (LDecimal (abs e)))
  args -> argsError info b args

rawExp :: (IsBuiltin b) => NativeFunction e b i
rawExp info b _env = \case
  [VLiteral (LInteger i)] -> do
    let result = Musl.trans_exp (fromIntegral i)
    guardNanOrInf info result
    return (VLiteral (LDecimal (f2Dec result)))
  [VLiteral (LDecimal e)] -> do
    let result = Musl.trans_exp (dec2F e)
    guardNanOrInf info result
    return (VLiteral (LDecimal (f2Dec result)))
  args -> argsError info b args

rawLn :: (IsBuiltin b) => NativeFunction e b i
rawLn info b _env = \case
  [VLiteral (LInteger i)] -> do
    let result = Musl.trans_ln (fromIntegral i)
    guardNanOrInf info result
    return (VLiteral (LDecimal (f2Dec result)))
  [VLiteral (LDecimal e)] -> do
    let result = Musl.trans_ln (dec2F e)
    guardNanOrInf info result
    return (VLiteral (LDecimal (f2Dec result)))
  args -> argsError info b args

rawSqrt :: (IsBuiltin b) => NativeFunction e b i
rawSqrt info b _env = \case
  [VLiteral (LInteger i)] -> do
    when (i < 0) $ throwExecutionError info (ArithmeticException "Square root must be non-negative")
    let result = Musl.trans_sqrt (fromIntegral i)
    guardNanOrInf info result
    return (VLiteral (LDecimal (f2Dec result)))
  [VLiteral (LDecimal e)] -> do
    when (e < 0) $ throwExecutionError info (ArithmeticException "Square root must be non-negative")
    let result = Musl.trans_sqrt (dec2F e)
    guardNanOrInf info result
    return (VLiteral (LDecimal (f2Dec result)))
  args -> argsError info b args


-- Todo: fix all show instances
rawShow :: (IsBuiltin b) => NativeFunction e b i
rawShow info b _env = \case
  [VPactValue pv] -> VString <$> renderPactValue info pv
  args -> argsError info b args

-- Todo: Gas here is complicated, greg worked on this previously
rawContains :: (IsBuiltin b) => NativeFunction e b i
rawContains info b _env = \case
  [VString f, VObject o] -> do
    chargeGasArgs info $ GSearch $ FieldSearch (M.size o)
    return (VBool (M.member (Field f) o))
  [VString needle, VString hay] -> do
    chargeGasArgs info $ GSearch $ SubstringSearch needle hay
    return (VBool (needle `T.isInfixOf` hay))
  [VPactValue v, VList vli] -> do
    let search True _ = pure True
        search _ el = valEqGassed info v el
    res <- foldlM search False vli
    return (VBool res)
  args -> argsError info b args

rawSort :: (IsBuiltin b) => NativeFunction e b i
rawSort info b _env = \case
  [VList vli]
    | V.null vli -> return (VList mempty)
    | otherwise -> do
    vli' <- liftIO $ do
      v' <- V.thaw vli
      V.sort v'
      V.freeze v'
    return (VList vli')
  args -> argsError info b args

coreRemove :: (IsBuiltin b) => NativeFunction e b i
coreRemove info b _env = \case
  [VString s, VObject o] -> do
    chargeGasArgs info $ GObjOp $ ObjOpRemove s (M.size o)
    return (VObject (M.delete (Field s) o))
  args -> argsError info b args

asObject
  :: (IsBuiltin b)
  => i
  -> b
  -> PactValue
  -> EvalM e b i (M.Map Field PactValue)
asObject info b = \case
  PObject o -> pure o
  arg -> argsError info b [VPactValue arg]

rawSortObject :: (IsBuiltin b) => NativeFunction e b i
rawSortObject info b _env = \case
  [VList fields, VList objs]
    | V.null fields -> return (VList objs)
    | V.null objs -> return (VList objs)
    | otherwise -> do
        objs' <- traverse (asObject info b) objs
        fields' <- traverse (fmap Field . asString info b) fields
        v' <- liftIO $ do
          mobjs <- V.thaw objs'
          V.sortBy (sort fields') mobjs
          V.freeze mobjs
        return (VList (PObject <$> v'))
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

guardNanOrInf :: i -> Double -> EvalM e b i ()
guardNanOrInf info a =
  when (isNaN a || isInfinite a) $ throwExecutionError info (FloatingPointError "Floating operation resulted in Infinity or NaN")

dec2F :: Decimal -> Double
dec2F = fromRational . toRational

f2Dec :: Double -> Decimal
f2Dec = fromRational . toRational

roundDec :: (IsBuiltin b) => NativeFunction e b i
roundDec = roundingFn round

floorDec :: (IsBuiltin b) => NativeFunction e b i
floorDec = roundingFn floor

ceilingDec :: (IsBuiltin b) => NativeFunction e b i
ceilingDec = roundingFn ceiling

---------------------------
-- bool ops
---------------------------
notBool :: (IsBuiltin b) => NativeFunction e b i
notBool info b _env = \case
  [VLiteral (LBool i)] -> return  (VLiteral (LBool (not i)))
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
rawTake :: (IsBuiltin b) => NativeFunction e b i
rawTake info b _env = \case
  [VLiteral (LInteger i), VLiteral (LString t)]
    | i >= 0 -> do
      -- See Note: [Take/Drop Clamping]
      let clamp = fromIntegral $ min i (fromIntegral (T.length t))
      chargeGasArgs info $ GConcat $ TextConcat $ GasTextLength clamp
      return  (VLiteral (LString (T.take clamp t)))
    | otherwise -> do
      -- See Note: [Take/Drop Clamping]
      let clamp = fromIntegral $ max (fromIntegral (T.length t) + i) 0
      chargeGasArgs info $ GConcat $ TextConcat $ GasTextLength $ fromIntegral $ negate i
      return  (VLiteral (LString (T.drop clamp t)))
  [VLiteral (LInteger i), VList li]
    | i >= 0 -> do
      -- See Note: [Take/Drop Clamping]
      let clamp = fromIntegral $ min i (fromIntegral (V.length li))
      chargeGasArgs info $ GConcat $ ListConcat $ GasListLength clamp
      return  (VList (V.take clamp li))
    | otherwise -> do
      -- See Note: [Take/Drop Clamping]
      let clamp = fromIntegral $ max (fromIntegral (V.length li) + i) 0
      chargeGasArgs info $ GConcat $ ListConcat $ GasListLength $ fromIntegral $ negate i
      return (VList (V.drop clamp li))
  [VList li, VObject o] -> do
    strings <- traverse (fmap Field . asString info b) (V.toList li)
    chargeGasArgs info $ GConcat $ ObjConcat $ V.length li
    return $ VObject $ M.restrictKeys o (S.fromList strings)
  args -> argsError info b args

rawDrop :: (IsBuiltin b) => NativeFunction e b i
rawDrop info b _env = \case
  [VLiteral (LInteger i), VLiteral (LString t)]
    | i >= 0 -> do
      -- See Note: [Take/Drop Clamping]
      let clamp = fromIntegral $ min i (fromIntegral (T.length t))
      return  (VLiteral (LString (T.drop clamp t)))
    | otherwise -> do
      -- See Note: [Take/Drop Clamping]
      let clamp = fromIntegral $ max (fromIntegral (T.length t) + i) 0
      return  (VLiteral (LString (T.take clamp t)))
  [VLiteral (LInteger i), VList li]
    | i >= 0 -> do
      -- See Note: [Take/Drop Clamping]
      let clamp = fromIntegral $ min i (fromIntegral (V.length li))
      return  (VList (V.drop clamp li))
    | otherwise -> do
      -- See Note: [Take/Drop Clamping]
      let clamp = fromIntegral $ max (fromIntegral (V.length li) + i) 0
      return (VList (V.take clamp li))
  [VList li, VObject o] -> do
    strings <- traverse (fmap Field . asString info b) (V.toList li)
    return $ VObject $ M.withoutKeys o (S.fromList strings)
  args -> argsError info b args

rawLength :: (IsBuiltin b) => NativeFunction e b i
rawLength info b _env = \case
  [VString t] -> do
    chargeGasArgs info $ GStrOp $ StrOpLength $ T.length t
    return  (VLiteral (LInteger (fromIntegral (T.length t))))
  [VList li] -> return (VLiteral (LInteger (fromIntegral (V.length li))))
  [VObject o] ->
    return $ VInteger $ fromIntegral (M.size o)
  args -> argsError info b args

rawReverse :: (IsBuiltin b) => NativeFunction e b i
rawReverse info b _env = \case
  [VList li] -> do
    chargeGasArgs info (GConcat (ListConcat (GasListLength (V.length li))))
    return (VList (V.reverse li))
  [VLiteral (LString t)] -> do
    chargeGasArgs info (GConcat (TextConcat (GasTextLength (T.length t))))
    return  (VLiteral (LString (T.reverse t)))
  args -> argsError info b args

coreConcat :: (IsBuiltin b) => NativeFunction e b i
coreConcat info b _env = \case
  [VList li]
    | V.null li -> return (VString mempty)
    | otherwise -> do
    li' <- traverse (asString info b) li
    let totalLen = sum $ T.length <$> li'
    chargeGasArgs info (GConcat (TextListConcat (GasTextLength totalLen) (GasListLength (V.length li))))
    return (VString (T.concat (V.toList li')))
  args -> argsError info b args

strToList :: (IsBuiltin b) => NativeFunction e b i
strToList info b _env = \case
  [VLiteral (LString s)] -> do
    chargeGasArgs info $ GStrOp $ StrOpExplode $ T.length s
    let v = VList (V.fromList (PLiteral . LString . T.singleton <$> T.unpack s))
    return v
  args -> argsError info b args


zipList :: (IsBuiltin b) => NativeFunction e b i
zipList info b _env = \case
  [VClosure clo, VList l, VList r] ->
    VList <$> V.zipWithM go l r
    where
    go x y = do
      chargeGasArgs info (GAConstant unconsWorkNodeGas)
      enforcePactValue info =<< applyLam clo [VPactValue x, VPactValue y]
  args -> argsError info b args

coreMap :: (IsBuiltin b) => NativeFunction e b i
coreMap info b _env = \case
  [VClosure clo, VList li] ->
    VList <$> traverse go li
    where
    go x = do
      chargeGasArgs info (GAConstant unconsWorkNodeGas)
      applyLam clo [VPactValue x] >>= enforcePactValue info
  args -> argsError info b args

coreFilter :: (IsBuiltin b) => NativeFunction e b i
coreFilter info b _env = \case
  [VClosure clo, VList li] ->
    VList <$> V.filterM go li
    where
    go e = do
      chargeGasArgs info (GAConstant unconsWorkNodeGas)
      applyLam clo [VPactValue e] >>= enforceBool info
  args -> argsError info b args

coreFold :: (IsBuiltin b) => NativeFunction e b i
coreFold info b _env = \case
  [VClosure clo, VPactValue initElem, VList li] ->
    VPactValue <$> foldlM go initElem li
    where
    go e inc = do
      chargeGasArgs info (GAConstant unconsWorkNodeGas)
      applyLam clo [VPactValue e, VPactValue inc] >>= enforcePactValue info
  args -> argsError info b args

coreEnumerate :: (IsBuiltin b) => NativeFunction e b i
coreEnumerate info b _env = \case
  [VLiteral (LInteger from), VLiteral (LInteger to)] -> do
    v <- createEnumerateList info from to (if from > to then -1 else 1)
    return (VList (PLiteral . LInteger <$> v))
  args -> argsError info b args


coreEnumerateStepN :: (IsBuiltin b) => NativeFunction e b i
coreEnumerateStepN info b _env = \case
  [VLiteral (LInteger from), VLiteral (LInteger to), VLiteral (LInteger inc)] -> do
    v <- createEnumerateList info from to inc
    return (VList (PLiteral . LInteger <$> v))
  args -> argsError info b args

makeList :: (IsBuiltin b) => NativeFunction e b i
makeList info b _env = \case
  [VLiteral (LInteger i), VPactValue v] -> do
    vSize <- sizeOf info SizeOfV0 v
    chargeGasArgs info (GMakeList (fromIntegral i) vSize)
    return (VList (V.fromList (replicate (fromIntegral i) v)))
  args -> argsError info b args

coreAccess :: (IsBuiltin b) => NativeFunction e b i
coreAccess info b _env = \case
  [VLiteral (LInteger i), VList vec] ->
    case vec V.!? fromIntegral i of
      Just v -> return (VPactValue v)
      _ -> throwExecutionError info (ArrayOutOfBoundsException (V.length vec) (fromIntegral i))
  [VString field, VObject o] ->
    case M.lookup (Field field) o of
      Just v -> return (VPactValue v)
      Nothing ->
        throwExecutionError info (ObjectIsMissingField (Field field) (ObjectData o))
  args -> argsError info b args

coreIsCharset :: (IsBuiltin b) => NativeFunction e b i
coreIsCharset info b _env = \case
  [VLiteral (LInteger i), VString s] -> do
    chargeGasArgs info $ GStrOp $ StrOpParse $ T.length s
    case i of
      0 -> return $ VBool $ T.all Char.isAscii s
      1 -> return $ VBool $ T.all Char.isLatin1 s
      _ -> throwNativeExecutionError info b "Unsupported character set"
  args -> argsError info b args

coreYield :: (IsBuiltin b) => NativeFunction e b i
coreYield info b _env = \case
  [VObject o] -> go o Nothing
  [VObject o, VString cid] -> go o (Just (ChainId cid))
  args -> argsError info b args
  where
  go o mcid = do
    mpe <- use esDefPactExec
    case mpe of
      Nothing -> throwExecutionError info YieldOutsideDefPact
      Just pe -> case mcid of
        Nothing -> do
          esDefPactExec . _Just . peYield .= Just (Yield o Nothing Nothing)
          return (VObject o)
        Just cid -> do
          sourceChain <- viewEvalEnv (eePublicData . pdPublicMeta . pmChainId)
          p <- provenanceOf cid
          when (_peStepHasRollback pe) $ throwExecutionError info $ EvalError "Cross-chain yield not allowed in step with rollback"
          esDefPactExec . _Just . peYield .= Just (Yield o (Just p) (Just sourceChain))
          return (VObject o)
  provenanceOf tid =
    Provenance tid . _mHash <$> getCallingModule info

corePactId :: (IsBuiltin b) => NativeFunction e b i
corePactId info b _env = \case
  [] -> use esDefPactExec >>= \case
    Just dpe -> return (VString (_defPactId (_peDefPactId dpe)))
    Nothing -> throwExecutionError info NotInDefPactExecution
  args -> argsError info b args

enforceYield
  :: i
  -> Yield
  -> EvalM e b i ()
enforceYield info y = case _yProvenance y of
  Nothing -> pure ()
  Just p -> do
    m <- getCallingModule info
    cid <- viewEvalEnv $ eePublicData . pdPublicMeta . pmChainId
    let p' = Provenance cid (_mHash m):map (Provenance cid) (S.toList $ _mBlessed m)
    unless (p `elem` p') $ throwExecutionError info (YieldProvenanceDoesNotMatch p p')

coreResume :: (IsBuiltin b) => NativeFunction e b i
coreResume info b _env = \case
  [VClosure clo] -> do
    mps <- viewEvalEnv eeDefPactStep
    case mps of
      Nothing -> throwExecutionError info NoActiveDefPactExec
      Just pactStep -> case _psResume pactStep of
        Nothing -> throwExecutionError info (NoYieldInDefPactStep pactStep)
        Just y@(Yield resumeObj _ _) -> do
          enforceYield info y
          applyLam clo [VObject resumeObj]
  args -> argsError info b args

-----------------------------------
-- try-related ops
-----------------------------------

enforceTopLevelOnly :: (IsBuiltin b) => i -> b -> EvalM e b i ()
enforceTopLevelOnly info b = do
  s <- use esStack
  unless (null s) $ throwExecutionError info (NativeIsTopLevelOnly (builtinName b))

-----------------------------------
-- Guards and reads
-----------------------------------


-----------------------------------
-- Other Core forms
-----------------------------------

coreB64Encode :: (IsBuiltin b) => NativeFunction e b i
coreB64Encode info b _env = \case
  [VLiteral (LString l)] -> do
    chargeGasArgs info $ GStrOp $ StrOpParse $ T.length l
    return $ VLiteral $ LString $ toB64UrlUnpaddedText $ T.encodeUtf8 l
  args -> argsError info b args


coreB64Decode :: (IsBuiltin b) => NativeFunction e b i
coreB64Decode info b _env = \case
  [VLiteral (LString s)] -> do
    chargeGasArgs info $ GStrOp $ StrOpParse $ T.length s
    case fromB64UrlUnpaddedText $ T.encodeUtf8 s of
      Left{} -> throwExecutionError info (DecodeError "invalid b64 encoding")
      Right txt -> return (VLiteral (LString txt))
  args -> argsError info b args


-- | The implementation of `enforce-guard` native.
coreEnforceGuard :: (IsBuiltin b) => NativeFunction e b i
coreEnforceGuard info b env = \case
  [VGuard g] -> VBool <$> enforceGuard info env g
  [VString s] -> do
    chargeGasArgs info $ GStrOp $ StrOpParse $ T.length s
    case parseAnyKeysetName s of
      Left {} -> throwNativeExecutionError info b "incorrect keyset name format"
      Right ksn ->
        VBool <$> isKeysetNameInSigs info env ksn
  args -> argsError info b args

keysetRefGuard :: (IsBuiltin b) => NativeFunction e b i
keysetRefGuard info b env = \case
  [VString g] -> do
    chargeGasArgs info $ GStrOp $ StrOpParse $ T.length g
    case parseAnyKeysetName g of
      Left {} -> throwNativeExecutionError info b "incorrect keyset name format"
      Right ksn -> do
        let pdb = view cePactDb env
        liftDbFunction info (_pdbRead pdb DKeySets ksn) >>= \case
          Nothing -> throwExecutionError info (NoSuchKeySet ksn)
          Just _ -> return (VGuard (GKeySetRef ksn))
  args -> argsError info b args

coreTypeOf :: (IsBuiltin b) => NativeFunction e b i
coreTypeOf info b _env = \case
  [v] -> case v of
    VPactValue pv ->
      return $ VString $ renderType $ synthesizePvType pv
    VClosure _ -> return $ VString "<<closure>>"
    VTable tv -> return $ VString (renderType (TyTable (_tvSchema tv)))
  args -> argsError info b args

coreDec :: (IsBuiltin b) => NativeFunction e b i
coreDec info b _env = \case
  [VInteger i] -> return $ VDecimal $ Decimal 0 i
  args -> argsError info b args

--------------------------------------------------
-- Env-read* functions
--------------------------------------------------

-- | Throw a recoverable error to be used in the read-* family of functions
throwReadError
  :: (IsBuiltin b) => i -> b -> EvalM e b i a
throwReadError info b =
  throwUserRecoverableError info $ EnvReadFunctionFailure  (builtinName b)

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
coreReadInteger :: (IsBuiltin b) => NativeFunction e b i
coreReadInteger info b _env = \case
  [VString s] -> do
    viewEvalEnv eeMsgBody >>= \case
      PObject envData -> do
        chargeGasArgs info $ GObjOp $ ObjOpLookup s $ M.size envData
        case M.lookup (Field s) envData of
          -- See [Note: Parsed Integer]
          Just (PDecimal p) ->
            return (VInteger (round p))
          Just (PInteger p) ->
            return (VInteger p)
          -- See [Note: Parsed Integer]
          Just (PString raw) -> do
            chargeGasArgs info $ GStrOp $ StrOpConvToInt $ T.length raw
            case parseNumLiteral raw of
              Just (LInteger i) -> return (VInteger i)
              _ -> throwReadError info b
          _ -> throwReadError info b
      _ -> throwReadError info b
  args -> argsError info b args


coreReadMsg :: (IsBuiltin b) => NativeFunction e b i
coreReadMsg info b _env = \case
  [VString s] -> do
    viewEvalEnv eeMsgBody >>= \case
      PObject envData -> do
        chargeGasArgs info $ GObjOp $ ObjOpLookup s $ M.size envData
        case M.lookup (Field s) envData of
          Just pv -> return (VPactValue pv)
          _ -> throwReadError info b
      _ -> throwReadError info b
  [] -> do
    envData <- viewEvalEnv eeMsgBody
    return (VPactValue envData)
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
coreReadDecimal :: (IsBuiltin b) => NativeFunction e b i
coreReadDecimal info b _env = \case
  [VString s] -> do
    viewEvalEnv eeMsgBody >>= \case
      PObject envData -> do
        chargeGasArgs info $ GObjOp $ ObjOpLookup s $ M.size envData
        case M.lookup (Field s) envData of
          Just (PDecimal p) -> return (VDecimal p)
          -- See [Note: Parsed Decimal]
          Just (PInteger i) -> return (VDecimal (Decimal 0 i))
          Just (PString raw) ->  do
            chargeGasArgs info $ GStrOp $ StrOpConvToInt $ T.length raw
            case parseNumLiteral raw of
              Just (LInteger i) -> return (VDecimal (Decimal 0 i))
              Just (LDecimal l) -> return (VDecimal l)
              _ -> throwReadError info b
          _ -> throwReadError info b
      _ -> throwReadError info b
  args -> argsError info b args

coreReadString :: (IsBuiltin b) => NativeFunction e b i
coreReadString info b _env = \case
  [VString s] -> do
    viewEvalEnv eeMsgBody >>= \case
      PObject envData -> do
        chargeGasArgs info $ GObjOp $ ObjOpLookup s $ M.size envData
        case M.lookup (Field s) envData of
          Just (PString p) -> return (VString p)
          _ -> throwReadError info b
      _ -> throwReadError info b
  args -> argsError info b args



coreReadKeyset :: (IsBuiltin b) => NativeFunction e b i
coreReadKeyset info b _env = \case
  [VString ksn] ->
    readKeyset' info ksn >>= \case
      Just ks -> do
        shouldEnforce <- isExecutionFlagSet FlagEnforceKeyFormats
        if shouldEnforce && isLeft (enforceKeyFormats (const ()) ks)
           then throwExecutionError info (InvalidKeysetFormat ks)
           else return (VGuard (GKeyset ks))
      Nothing -> throwReadError info b
  args -> argsError info b args


coreBind :: (IsBuiltin b) => NativeFunction e b i
coreBind info b _env = \case
  [v@VObject{}, VClosure clo] ->
    applyLam clo [v] >>= enforcePactValue' info
  args -> argsError info b args


--------------------------------------------------
-- Db functions
--------------------------------------------------

createTable :: (IsBuiltin b) => NativeFunction e b i
createTable info b env = \case
  [VTable tv] -> do
    enforceTopLevelOnly info b
    let pdb = _cePactDb env
    guardTable info env tv GtCreateTable
    evalCreateUserTable info pdb (_tvName tv)
    return (VString "TableCreated")
  args -> argsError info b args

dbSelect :: (IsBuiltin b) => NativeFunction e b i
dbSelect info b env = \case
  [VTable tv, VClosure clo] ->
    selectRead tv clo Nothing
  [VTable tv, VList li, VClosure clo] -> do
    fields' <- traverse (fmap Field . asString info b) (V.toList li)
    selectRead tv clo (Just fields')
  args -> argsError info b args
  where
  pdb = _cePactDb env
  selectRead tv clo mf = do
    guardTable info env tv GtSelect
    ks <- liftDbFunction info (_pdbKeys pdb (tvToDomain tv))
    VList . V.fromList . fmap (PObject . mRestrictFields mf) . catMaybes <$> traverse go ks
    where
    mRestrictFields =
      maybe id (\fields -> flip M.restrictKeys (S.fromList fields))
    go k =
      liftDbFunction info (_pdbRead pdb (tvToDomain tv) k) >>= \case
        Just (RowData r) -> do
          cond <- enforceBool info =<< applyLam clo [VObject r]
          if cond then pure $ Just r
          else pure Nothing
        Nothing -> failInvariant info (InvariantNoSuchKeyInTable (_tvName tv) k)



foldDb :: (IsBuiltin b) => NativeFunction e b i
foldDb info b env = \case
  [VTable tv, VClosure queryClo, VClosure consumer] -> do
    -- let cont' = BuiltinC env info (PreFoldDbC tv queryClo consumer) cont
    -- guardTable info cont' handler env tv GtSelect
    guardTable info env tv GtSelect
    keys <- liftDbFunction info (_pdbKeys pdb (tvToDomain tv))
    VList . V.fromList . catMaybes <$> traverse go keys
    where
    go rk@(RowKey raw) = do
      liftDbFunction info (_pdbRead pdb (tvToDomain tv) rk) >>= \case
        Just (RowData row) -> do
          qryCond <- enforceBool info =<< applyLam queryClo [VString raw, VObject row]
          if qryCond then do
            v <- enforcePactValue info =<< applyLam consumer [VString raw, VObject row]
            pure (Just v)
          else pure Nothing
        Nothing ->
          failInvariant info (InvariantNoSuchKeyInTable (_tvName tv) rk)
    pdb = _cePactDb env

  args -> argsError info b args

readUserTable
  :: i
  -> DirectEnv e b i
  -> TableValue
  -> RowKey
  -> EvalM e b i RowData
readUserTable info env tv rk = do
  liftDbFunction info (_pdbRead (_cePactDb env) (tvToDomain tv) rk) >>= \case
    Just rd ->
      return rd
    Nothing -> throwUserRecoverableError info $ NoSuchObjectInDb (_tvName tv) rk

dbRead :: (IsBuiltin b) => NativeFunction e b i
dbRead info b env = \case
  [VTable tv, VString rk] -> do
    guardTable info env tv GtRead
    VObject . _unRowData <$> readUserTable info env tv (RowKey rk)
  args -> argsError info b args

dbWithRead :: (IsBuiltin b) => NativeFunction e b i
dbWithRead info b env = \case
  [VTable tv, VString rk, VClosure clo] -> do
    guardTable info env tv GtRead
    RowData o <- readUserTable info env tv (RowKey rk)
    applyLam clo [VObject o] >>= enforcePactValue' info
  args -> argsError info b args

dbWithDefaultRead :: (IsBuiltin b) => NativeFunction e b i
dbWithDefaultRead info b env = \case
  [VTable tv, VString rk, VObject defaultObj, VClosure clo] -> do
    guardTable info env tv GtWithDefaultRead
    liftDbFunction info (_pdbRead (_cePactDb env) (tvToDomain tv) (RowKey rk)) >>= \case
      Just (RowData o) ->
        applyLam clo [VObject o] >>= enforcePactValue' info
      Nothing ->
         applyLam clo [VObject defaultObj] >>= enforcePactValue' info
  args -> argsError info b args

-- | Todo: schema checking here? Or only on writes?
dbWrite :: (IsBuiltin b) => NativeFunction e b i
dbWrite = write' Write

dbInsert :: (IsBuiltin b) => NativeFunction e b i
dbInsert = write' Insert

write' :: (IsBuiltin b) => WriteType -> NativeFunction e b i
write' wt info b env = \case
  [VTable tv, VString key, VObject rv] -> do
    guardTable info env tv GtWrite
    let pdb = _cePactDb env
    let check' = if wt == Update then checkPartialSchema else checkSchema
    if check' rv (_tvSchema tv) then do
      let rdata = RowData rv
      rvSize <- sizeOf info SizeOfV0 rv
      chargeGasArgs info (GWrite rvSize)
      evalWrite info pdb wt (tvToDomain tv) (RowKey key) rdata
      return (VString "Write succeeded")
    else throwExecutionError info (WriteValueDidNotMatchSchema (_tvSchema tv) (ObjectData rv))
  args -> argsError info b args

dbUpdate :: (IsBuiltin b) => NativeFunction e b i
dbUpdate = write' Update

dbKeys :: (IsBuiltin b) => NativeFunction e b i
dbKeys info b env = \case
  [VTable tv] -> do
    guardTable info env tv GtKeys
    let pdb = _cePactDb env
    ks <- liftDbFunction info (_pdbKeys pdb (tvToDomain tv))
    let li = V.fromList (PString . _rowKey <$> ks)
    return (VList li)
    -- let cont' = BuiltinC env info (KeysC tv) cont
    -- guardTable info cont' handler env tv GtKeys
  args -> argsError info b args

defineKeySet'
  :: (IsBuiltin b)
  => i
  -> DirectEnv e b i
  -> T.Text
  -> KeySet
  -> EvalM e b i (EvalValue e b i)
defineKeySet' info env ksname newKs  = do
  let pdb = view cePactDb env
  ignoreNamespaces <- not <$> isExecutionFlagSet FlagRequireKeysetNs
  case parseAnyKeysetName ksname of
    Left {} -> throwExecutionError info (InvalidKeysetNameFormat ksname)
    Right ksn -> do
      let writeKs = do
            newKsSize <- sizeOf info SizeOfV0 newKs
            chargeGasArgs info (GWrite newKsSize)
            evalWrite info pdb Write DKeySets ksn newKs
            return (VString "Keyset write success")
      liftDbFunction info (_pdbRead pdb DKeySets ksn) >>= \case
        Just oldKs -> do
          _ <- isKeysetInSigs info env oldKs
          writeKs
        Nothing | ignoreNamespaces -> writeKs
        Nothing | otherwise -> use (esLoaded . loNamespace) >>= \case
          Nothing -> throwExecutionError info CannotDefineKeysetOutsideNamespace
          Just (Namespace ns uGuard _adminGuard) -> do
            when (Just ns /= _keysetNs ksn) $ throwExecutionError info (MismatchingKeysetNamespace ns)
            _ <- enforceGuard info env uGuard
            writeKs

defineKeySet :: (IsBuiltin b) => NativeFunction e b i
defineKeySet info b env = \case
  [VString ksname, VGuard (GKeyset ks)] -> do
    enforceTopLevelOnly info b
    defineKeySet' info env ksname ks
  [VString ksname] -> do
    enforceTopLevelOnly info b
    readKeyset' info ksname >>= \case
      Just newKs ->
        defineKeySet' info env ksname newKs
      Nothing -> throwUserRecoverableError info $ EnvReadFunctionFailure  (builtinName b)
  args -> argsError info b args

--------------------------------------------------
-- Capabilities
--------------------------------------------------

requireCapability :: (IsBuiltin b) => NativeFunction e b i
requireCapability info b _env = \case
  [VCapToken ct] -> do
    slots <- use $ esCaps . csSlots
    let cnt = sum [1 + length cs | CapSlot _ cs <- slots]
    chargeGasArgs info $ GCapOp $ CapOpRequire cnt
    requireCap info ct
  args -> argsError info b args

composeCapability :: (IsBuiltin b) => NativeFunction e b i
composeCapability info b env = \case
  [VCapToken ct] -> do
    enforceStackTopIsDefcap info b
    composeCap info env ct
  args -> argsError info b args

installCapability :: (IsBuiltin b) => NativeFunction e b i
installCapability info b env = \case
  [VCapToken ct] -> do
    enforceNotWithinDefcap info env "install-capability"
    _ <- installCap info env ct True
    return (VString "Installed capability")
  args -> argsError info b args

coreEmitEvent :: (IsBuiltin b) => NativeFunction e b i
coreEmitEvent info b env = \case
  [VCapToken ct@(CapToken fqn _)] -> do
    -- let cont' = BuiltinC env info (EmitEventC ct) cont
    guardForModuleCall info env (_fqModule fqn) $ return ()
    d <- getDefCap info fqn
    enforceMeta (_dcapMeta d)
    emitCapability info ct
    return (VBool True)
    where
    enforceMeta Unmanaged = throwExecutionError info (InvalidEventCap fqn)
    enforceMeta _ = pure ()
  args -> argsError info b args

createCapGuard :: (IsBuiltin b) => NativeFunction e b i
createCapGuard info b _env = \case
  [VCapToken ct] -> do
    let qn = fqnToQualName (_ctName ct)
        cg = CapabilityGuard qn (_ctArgs ct) Nothing
    return (VGuard (GCapabilityGuard cg))
  args -> argsError info b args

createCapabilityPactGuard :: (IsBuiltin b) => NativeFunction e b i
createCapabilityPactGuard info b _env = \case
  [VCapToken ct] -> do
    pid <- getDefPactId info
    let qn = fqnToQualName (_ctName ct)
    let cg = CapabilityGuard qn (_ctArgs ct) (Just pid)
    return (VGuard (GCapabilityGuard cg))
  args -> argsError info b args

createModuleGuard :: (IsBuiltin b) => NativeFunction e b i
createModuleGuard info b _env = \case
  [VString n] ->
    findCallingModule >>= \case
      Just mn ->  do
        let cg = GModuleGuard (ModuleGuard mn n)
        return (VGuard cg)
      Nothing ->
        throwNativeExecutionError info b "create-module-guard: must call within module"
  args -> argsError info b args

createDefPactGuard :: (IsBuiltin b) => NativeFunction e b i
createDefPactGuard info b _env = \case
  [VString name] -> do
    dpid <- getDefPactId info
    return $ VGuard $ GDefPactGuard $ DefPactGuard dpid name
  args -> argsError info b args


coreIntToStr :: (IsBuiltin b) => NativeFunction e b i
coreIntToStr info b _env = \case
  [VInteger base, VInteger v]
    | v < 0 ->
      throwNativeExecutionError info b "int-to-str error: cannot show negative integer"
    | base >= 2 && base <= 16 -> do
      let strLen = 1 + Exts.I# (IntLog.integerLogBase# base $ abs v)
      chargeGasArgs info $ GConcat $ TextConcat $ GasTextLength $ fromIntegral strLen
      let v' = T.pack $ showIntAtBase base Char.intToDigit v ""
      return (VString v')
    | base == 64 && v >= 0 -> do
      let bsLen = 1 + Exts.I# (IntLog.integerLogBase# 256 $ abs v)
          strLen = (bsLen * 4) `div` 3
      chargeGasArgs info $ GConcat $ TextConcat $ GasTextLength $ fromIntegral strLen
      let v' = toB64UrlUnpaddedText $ integerToBS v
      return (VString v')
    | base == 64 -> throwNativeExecutionError info b "only positive values allowed for base64URL conversion"
    | otherwise -> throwNativeExecutionError info b "invalid base for base64URL conversion"
  args -> argsError info b args

coreStrToInt :: (IsBuiltin b) => NativeFunction e b i
coreStrToInt info b _env = \case
  [VString s] -> do
    chargeGasArgs info $ GStrOp $ StrOpParse $ T.length s
    checkLen info s
    doBase info 10 s
  args -> argsError info b args

coreStrToIntBase :: (IsBuiltin b) => NativeFunction e b i
coreStrToIntBase info b _env = \case
  [VInteger base, VString s]
    | base == 64 -> do
      chargeGasArgs info $ GStrOp $ StrOpParse $ T.length s
      checkLen info s
      case decodeBase64UrlUnpadded $ T.encodeUtf8 s of
        Left{} -> throwExecutionError info (DecodeError "invalid b64 encoding")
        Right bs -> return $ VInteger (bsToInteger bs)
    | base >= 2 && base <= 16 -> do
        chargeGasArgs info $ GStrOp $ StrOpParse $ T.length s
        checkLen info s
        doBase info base s
    | otherwise -> throwNativeExecutionError info b $ "Base value must be >= 2 and <= 16, or 64"
  args -> argsError info b args
  where
  -- Todo: DOS and gas analysis
  bsToInteger :: BS.ByteString -> Integer
  bsToInteger bs = fst $ foldl' go (0,(BS.length bs - 1) * 8) $ BS.unpack bs
  go (i,p) w = (i .|. (shift (fromIntegral w) p),p - 8)

nubByM :: Monad m => (a -> a -> m Bool) -> [a] -> m [a]
nubByM eq = go
  where
  go [] = pure []
  go (x:xs) = do
    xs' <- filterM (fmap not . eq x) xs
    (x :) <$> go xs'

coreDistinct  :: (IsBuiltin b) => NativeFunction e b i
coreDistinct info b _env = \case
  [VList s] -> do
    uniques <- nubByM (valEqGassed info) $ V.toList s
    return
      $ VList
      $ V.fromList uniques
  args -> argsError info b args

coreFormat  :: (IsBuiltin b) => NativeFunction e b i
coreFormat info b _env = \case
  [VString s, VList es] -> do
    let parts = T.splitOn "{}" s
        plen = length parts
    if | plen == 1 -> do
          chargeGasArgs info $ GStrOp $ StrOpParse $ T.length s
          return $ VString s
       | plen - length es > 1 ->
        throwNativeExecutionError info b $ "not enough arguments for template"
       | otherwise -> do
          args <- mapM formatArgM $ V.toList $ V.take (plen - 1) es
          let totalLength = sum (T.length <$> parts) + sum (T.length <$> args)
          chargeGasArgs info $ GConcat $ TextConcat $ GasTextLength totalLength
          return $ VString $ T.concat $ alternate parts args
    where
    alternate (x:xs) ys = x : alternate ys xs
    alternate _ _ = []

    formatArgM (PString ps) = pure ps
    formatArgM a = renderPactValue info a

  args -> argsError info b args

checkLen
  :: i
  -> T.Text
  -> EvalM e b i ()
checkLen info txt =
  unless (T.length txt <= 512) $
      throwExecutionError info $ DecodeError "Invalid input, only up to 512 length supported"

doBase
  :: i
  -> Integer
  -> T.Text
  -> EvalM e b i (EvalValue e b i)
doBase info base txt = case baseStrToInt base txt of
  Left e -> throwExecutionError info (DecodeError e)
  Right n -> return (VInteger n)

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


coreAndQ :: (IsBuiltin b) => NativeFunction e b i
coreAndQ info b _env = \case
  [VClosure l, VClosure r, VPactValue v] -> do
    c1 <- enforceBool info =<< applyLam l [VPactValue v]
    if c1 then applyLam r [VPactValue v] >>= enforceBool' info
    else return (VBool False)
  args -> argsError info b args

coreOrQ :: (IsBuiltin b) => NativeFunction e b i
coreOrQ info b _env = \case
  [VClosure l, VClosure r, VPactValue v] -> do
    c1 <- enforceBool info =<< applyLam l [VPactValue v]
    if c1 then return (VBool True)
    else applyLam r [VPactValue v] >>= enforceBool' info
  args -> argsError info b args

coreNotQ :: (IsBuiltin b) => NativeFunction e b i
coreNotQ info b _env = \case
  [VClosure clo, VPactValue v] -> do
    c <- enforceBool info =<< applyLam clo [VPactValue v]
    return (VBool (not c))
  args -> argsError info b args

coreWhere :: (IsBuiltin b) => NativeFunction e b i
coreWhere info b _env = \case
  [VString field, VClosure app, VObject o] -> do
    case M.lookup (Field field) o of
      Just v -> do
        applyLam app [VPactValue v] >>= enforceBool' info
      Nothing ->
        throwExecutionError info (ObjectIsMissingField (Field field) (ObjectData o))
  args -> argsError info b args

coreHash :: (IsBuiltin b) => NativeFunction e b i
coreHash = \info b _env -> \case
  [VString s] ->
    return (go (T.encodeUtf8 s))
  [VPactValue pv] -> do
    return (go (encodeStable pv))
  args -> argsError info b args
  where
  go =  VString . hashToText . pactHash

txHash :: (IsBuiltin b) => NativeFunction e b i
txHash info b _env = \case
  [] -> do
    h <- viewEvalEnv eeHash
    return (VString (hashToText h))
  args -> argsError info b args

coreContinue :: (IsBuiltin b) => NativeFunction e b i
coreContinue info b _env = \case
  [v] -> do
    return v
  args -> argsError info b args

parseTime :: (IsBuiltin b) => NativeFunction e b i
parseTime info b _env = \case
  [VString fmt, VString s] -> do
    chargeGasArgs info $ GStrOp $ StrOpParseTime (T.length fmt) (T.length s)
    case PactTime.parseTime (T.unpack fmt) (T.unpack s) of
      Just t -> return $ VPactValue (PTime t)
      Nothing ->
        throwNativeExecutionError info b $ "parse-time parse failure"
  args -> argsError info b args

formatTime :: (IsBuiltin b) => NativeFunction e b i
formatTime info b _env = \case
  [VString fmt, VPactValue (PTime t)] -> do
    chargeGasArgs info $ GStrOp $ StrOpFormatTime $ T.length fmt
    let timeString = PactTime.formatTime (T.unpack fmt) t
    return $ VString (T.pack timeString)
  args -> argsError info b args

time :: (IsBuiltin b) => NativeFunction e b i
time info b _env = \case
  [VString s] -> do
    case PactTime.parseTime "%Y-%m-%dT%H:%M:%SZ" (T.unpack s) of
      Just t -> return $ VPactValue (PTime t)
      Nothing ->
        throwNativeExecutionError info b $ "time default format parse failure"
  args -> argsError info b args

addTime :: (IsBuiltin b) => NativeFunction e b i
addTime info b _env = \case
  [VPactValue (PTime t), VPactValue (PDecimal seconds)] -> do
      let newTime = t PactTime..+^ PactTime.fromSeconds seconds
      return $ VPactValue (PTime newTime)
  [VPactValue (PTime t), VPactValue (PInteger seconds)] -> do
      let newTime = t PactTime..+^ PactTime.fromSeconds (fromIntegral seconds)
      return $ VPactValue (PTime newTime)
  args -> argsError info b args

diffTime :: (IsBuiltin b) => NativeFunction e b i
diffTime info b _env = \case
  [VPactValue (PTime x), VPactValue (PTime y)] -> do
    let secondsDifference = PactTime.toSeconds $ x PactTime..-. y
    return $ VPactValue $ PDecimal secondsDifference
  args -> argsError info b args

minutes :: (IsBuiltin b) => NativeFunction e b i
minutes info b _env = \case
  [VDecimal x] -> do
    let seconds = x * 60
    return $ VDecimal seconds
  [VInteger x] -> do
    let seconds = fromIntegral x * 60
    return $ VDecimal seconds
  args -> argsError info b args

hours :: (IsBuiltin b) => NativeFunction e b i
hours info b _env = \case
  [VDecimal x] -> do
    let seconds = x * 60 * 60
    return $ VDecimal seconds
  [VInteger x] -> do
    let seconds = fromIntegral x * 60 * 60
    return $ VDecimal seconds
  args -> argsError info b args

days :: (IsBuiltin b) => NativeFunction e b i
days info b _env = \case
  [VDecimal x] -> do
    let seconds = x * 60 * 60 * 24
    return $ VDecimal seconds
  [VInteger x] -> do
    let seconds = fromIntegral x * 60 * 60 * 24
    return $ VDecimal seconds
  args -> argsError info b args

describeModule :: (IsBuiltin b) => NativeFunction e b i
describeModule info b env = \case
  [VString s] -> case parseModuleName s of
    Just mname -> do
      enforceTopLevelOnly info b
      checkNonLocalAllowed info b
      getModuleData info (view cePactDb env) mname >>= \case
        ModuleData m _ -> return $
          VObject $ M.fromList $ fmap (over _1 Field)
            [ ("name", PString (renderModuleName (_mName m)))
            , ("hash", PString (moduleHashToText (_mHash m)))
            , ("tx_hash", PString (hashToText (_mTxHash m)))
            , ("interfaces", PList (PString . renderModuleName <$> V.fromList (_mImplements m)))]
        InterfaceData iface _ -> return $
          VObject $ M.fromList $ fmap (over _1 Field)
            [ ("name", PString (renderModuleName (_ifName iface)))
            , ("hash", PString (moduleHashToText (_ifHash iface)))
            , ("tx_hash", PString (hashToText (_ifTxHash iface)))
            ]
    Nothing -> throwNativeExecutionError info b $ "invalid module name format"
  args -> argsError info b args

dbDescribeTable :: (IsBuiltin b) => NativeFunction e b i
dbDescribeTable info b _env = \case
  [VTable (TableValue name _ schema)] -> do
    enforceTopLevelOnly info b
    return $ VObject $ M.fromList $ fmap (over _1 Field)
      [("name", PString (_tableName name))
      ,("module", PString (renderModuleName (_tableModuleName name)))
      ,("type", PString (renderType (TyTable schema)))]
  args -> argsError info b args

dbDescribeKeySet :: (IsBuiltin b) => NativeFunction e b i
dbDescribeKeySet info b env = \case
  [VString s] -> do
    let pdb = _cePactDb env
    enforceTopLevelOnly info b
    case parseAnyKeysetName s of
      Right ksn -> do
        liftDbFunction info (_pdbRead pdb DKeySets ksn) >>= \case
          Just ks ->
            return (VGuard (GKeyset ks))
          Nothing ->
            throwExecutionError info (NoSuchKeySet ksn)
      Left{} ->
        throwNativeExecutionError info b  "incorrect keyset name format"
  args -> argsError info b args

coreCompose :: (IsBuiltin b) => NativeFunction e b i
coreCompose info b _env = \case
  [VClosure clo1, VClosure clo2, v] -> do
    v' <- enforcePactValue info =<< applyLam clo1 [v]
    applyLam clo2 [VPactValue v'] >>= enforcePactValue' info
    -- let cont' = Fn clo2 env [] [] cont
    -- applyLam clo1 [v] cont' handler
  args -> argsError info b args

coreCreatePrincipal :: (IsBuiltin b) => NativeFunction e b i
coreCreatePrincipal info b _env = \case
  [VGuard g] -> do
    pr <- createPrincipalForGuard info g
    return $ VString $ Pr.mkPrincipalIdent pr
  args -> argsError info b args

coreIsPrincipal :: (IsBuiltin b) => NativeFunction e b i
coreIsPrincipal info b _env = \case
  [VString p] -> do
    chargeGasArgs info $ GStrOp $ StrOpParse $ T.length p
    return $ VBool $ isRight $ parseOnly Pr.principalParser p
  args -> argsError info b args

coreTypeOfPrincipal :: (IsBuiltin b) => NativeFunction e b i
coreTypeOfPrincipal info b _env = \case
  [VString p] -> do
    chargeGasArgs info $ GStrOp $ StrOpParse $ T.length p
    let prty = case parseOnly Pr.principalParser p of
          Left _ -> ""
          Right pr -> Pr.showPrincipalType pr
    return $ VString prty
  args -> argsError info b args

coreValidatePrincipal :: (IsBuiltin b) => NativeFunction e b i
coreValidatePrincipal info b _env = \case
  [VGuard g, VString s] -> do
    pr' <- createPrincipalForGuard info g
    chargeGasArgs info $ GComparison $ TextComparison s
    return $ VBool $ Pr.mkPrincipalIdent pr' == s
  args -> argsError info b args


coreCond :: (IsBuiltin b) => NativeFunction e b i
coreCond info b _env = \case
  [VClosure clo] ->
    applyLam clo [] >>= enforcePactValue' info
  args -> argsError info b args

coreIdentity :: (IsBuiltin b) => NativeFunction e b i
coreIdentity info b _env = \case
  [VPactValue pv] -> return $ VPactValue pv
  args -> argsError info b args

--------------------------------------------------
-- Namespace functions
--------------------------------------------------
coreNamespace :: (IsBuiltin b) => NativeFunction e b i
coreNamespace info b env = \case
  [VString n] -> do
    enforceTopLevelOnly info b
    let pdb = view cePactDb env
    if T.null n then do
      (esLoaded . loNamespace) .= Nothing
      return (VString "Namespace reset to root")
    else do
      chargeGasArgs info $ GRead $ fromIntegral $ T.length n
      liftDbFunction info (_pdbRead pdb DNamespaces (NamespaceName n)) >>= \case
        Just ns -> do
          size <- sizeOf info SizeOfV0 ns
          chargeGasArgs info $ GRead size
          (esLoaded . loNamespace) .= Just ns
          let msg = "Namespace set to " <> n
          return (VString msg)
        Nothing ->
          throwExecutionError info $ NamespaceNotFound (NamespaceName n)
  args -> argsError info b args


coreDefineNamespace :: (IsBuiltin b) => NativeFunction e b i
coreDefineNamespace info b env = \case
  [VString n, VGuard usrG, VGuard adminG] -> do
    enforceTopLevelOnly info b
    unless (isValidNsFormat n) $ throwNativeExecutionError info b "invalid namespace format"
    let nsn = NamespaceName n
        ns = Namespace nsn usrG adminG
    chargeGasArgs info $ GRead $ fromIntegral $ T.length n
    liftDbFunction info (_pdbRead pdb DNamespaces (NamespaceName n)) >>= \case
      -- G!
      -- https://static.wikia.nocookie.net/onepiece/images/5/52/Lao_G_Manga_Infobox.png/revision/latest?cb=20150405020446
      -- Enforce the old guard
      Just existing@(Namespace _ _ laoG) -> do
        size <- sizeOf info SizeOfV0 existing
        chargeGasArgs info $ GRead size
        allow <- enforceGuard info env laoG
        writeNs allow nsn ns
      Nothing -> viewEvalEnv eeNamespacePolicy >>= \case
        SimpleNamespacePolicy -> do
          nsSize <- sizeOf info SizeOfV0 ns
          chargeGasArgs info (GWrite nsSize)
          evalWrite info pdb Write DNamespaces nsn ns
          return $ VString $ "Namespace defined: " <> n
        SmartNamespacePolicy _ fun -> getModuleMemberWithHash info pdb fun >>= \case
          (Dfun d, mh) -> do
            clo <- mkDefunClosure d (qualNameToFqn fun mh) env
            allow <- enforceBool info =<< applyLam (C clo) [VString n, VGuard adminG]
            writeNs allow nsn ns
          _ -> throwNativeExecutionError info b $ "Fatal error: namespace manager function is not a defun"
  args -> argsError info b args
  where
  pdb = _cePactDb env
  writeNs allow nsn ns = do
    unless allow $ throwNativeExecutionError info b $ "Namespace definition not permitted"
    nsSize <- sizeOf info SizeOfV0 ns
    chargeGasArgs info (GWrite nsSize)
    evalWrite info pdb Write DNamespaces nsn ns
    return $ VString $ "Namespace defined: " <> (_namespaceName nsn)
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

coreDescribeNamespace :: (IsBuiltin b) => NativeFunction e b i
coreDescribeNamespace info b _env = \case
  [VString n] -> do
    pdb <- viewEvalEnv eePactDb
    chargeGasArgs info $ GRead $ fromIntegral $ T.length n
    liftDbFunction info (_pdbRead pdb DNamespaces (NamespaceName n)) >>= \case
      Just existing@(Namespace _ usrG laoG) -> do
        size <- sizeOf info SizeOfV0 existing
        chargeGasArgs info $ GRead size
        let obj = M.fromList
                  [ (Field "user-guard", PGuard usrG)
                  , (Field "admin-guard", PGuard laoG)
                  , (Field "namespace-name", PString n)]
        return (VObject obj)
      Nothing ->
        throwExecutionError info $ NamespaceNotFound (NamespaceName n)
  args -> argsError info b args


coreChainData :: (IsBuiltin b) => NativeFunction e b i
coreChainData info b _env = \case
  [] -> do
    PublicData publicMeta blockHeight blockTime prevBh <- viewEvalEnv eePublicData
    let (PublicMeta cid sender (GasLimit (Gas gasLimit)) (GasPrice gasPrice) _ttl _creationTime) = publicMeta
    let fields = M.fromList [ (cdChainId, PString (_chainId cid))
                 , (cdBlockHeight, PInteger (fromIntegral blockHeight))
                 , (cdBlockTime, PTime (PactTime.fromPosixTimestampMicros blockTime))
                 , (cdPrevBlockHash, PString prevBh)
                 , (cdSender, PString sender)
                 , (cdGasLimit, PInteger (fromIntegral gasLimit))
                 , (cdGasPrice, PDecimal gasPrice)]
    return (VObject fields)
  args -> argsError info b args


-- -------------------------
-- ZK defns
-- -------------------------

#ifndef WITHOUT_CRYPTO
ensureOnCurve :: (Num p, Eq p) => i -> CurvePoint p -> p -> EvalM e b i ()
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


zkPairingCheck :: (IsBuiltin b) => NativeFunction e b i
zkPairingCheck info b _env = \case
  args@[VList p1s, VList p2s] -> do
    chargeGasArgs info (GAZKArgs (Pairing (max (V.length p1s) (V.length p2s))))
    g1s <- maybe (argsError info b args) pure (traverse (preview _PObject >=> (toG1 . ObjectData)) p1s)
    g2s <- maybe (argsError info b args) pure (traverse (preview _PObject >=> (toG2 . ObjectData)) p2s)
    traverse_ (\p -> ensureOnCurve info p b1) g1s
    traverse_ (\p -> ensureOnCurve info p b2) g2s
    let pairs = zip (V.toList g1s) (V.toList g2s)
    return $ VBool $ pairingCheck pairs
  args -> argsError info b args

zkScalarMult :: (IsBuiltin b) => NativeFunction e b i
zkScalarMult info b _env = \case
  args@[VString ptTy, VObject p1, VInteger scalar] -> do
    let scalar' = scalar `mod` curveOrder
    case T.toLower ptTy of
      "g1" -> do
        chargeGasArgs info (GAZKArgs (ScalarMult ZKG1))
        p1' <- maybe (argsError info b args) pure $ toG1 (ObjectData p1)
        ensureOnCurve info p1' b1
        let p2' = multiply p1' scalar'
            ObjectData o = fromG1 p2'
        return (VObject o)
      "g2" -> do
        chargeGasArgs info (GAZKArgs (ScalarMult ZKG2))
        p1' <- maybe (argsError info b args) pure $ toG2 (ObjectData p1)
        ensureOnCurve info p1' b2
        let p2' = multiply p1' scalar'
            ObjectData o = fromG2 p2'
        return (VObject o)
      _ -> argsError info b args
  args -> argsError info b args
  where
  curveOrder :: Integer
  curveOrder = 21888242871839275222246405745257275088548364400416034343698204186575808495617

zkPointAddition :: (IsBuiltin b) => NativeFunction e b i
zkPointAddition info b _env = \case
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
        return (VObject o)
      "g2" -> do
        chargeGasArgs info (GAZKArgs (PointAdd ZKG2))
        p1' <- maybe (argsError info b args) pure $ toG2 (ObjectData p1)
        p2' <- maybe (argsError info b args) pure $ toG2 (ObjectData p2)
        ensureOnCurve info p1' b2
        ensureOnCurve info p2' b2
        let p3' = add p1' p2'
            ObjectData o = fromG2 p3'
        return (VObject o)
      _ -> argsError info b args
  args -> argsError info b args


-----------------------------------
-- Poseidon
-----------------------------------

poseidonHash :: (IsBuiltin b) => NativeFunction e b i
poseidonHash info b _env = \case
  [VList as]
    | not (V.null as) && length as <= 8,
    Just intArgs <- traverse (preview (_PLiteral . _LInteger)) as -> do
      chargeGasArgs info (GPoseidonHashHackAChain (length intArgs))
      return $ VInteger (poseidon (V.toList intArgs))
  args -> argsError info b args

#else

zkPairingCheck :: (IsBuiltin b) => NativeFunction e b i
zkPairingCheck info _b _env _args = throwExecutionError info $ EvalError $ "crypto disabled"

zkScalarMult :: (IsBuiltin b) => NativeFunction e b i
zkScalarMult info _b _env _args = throwExecutionError info $ EvalError $ "crypto disabled"

zkPointAddition :: (IsBuiltin b) => NativeFunction e b i
zkPointAddition info _b _env _args = throwExecutionError info $ EvalError $ "crypto disabled"

poseidonHash :: (IsBuiltin b) => NativeFunction e b i
poseidonHash info _b _env _args = throwExecutionError info $ EvalError $ "crypto disabled"

#endif

-----------------------------------
-- SPV
-----------------------------------

coreVerifySPV :: (IsBuiltin b) => NativeFunction e b i
coreVerifySPV info b _env = \case
  [VString proofType, VObject o] -> do
    SPVSupport f _ <- viewEvalEnv eeSPVSupport
    liftIO (f proofType (ObjectData o)) >>= \case
      Left err -> throwExecutionError info (SPVVerificationFailure err)
      Right (ObjectData o') -> return (VObject o')
  args -> argsError info b args

-----------------------------------
-- Verifiers
-----------------------------------
coreEnforceVerifier :: (IsBuiltin b) => NativeFunction e b i
coreEnforceVerifier info b _env = \case
  [VString verName] -> do
    enforceStackTopIsDefcap info b
    viewsEvalEnv eeMsgVerifiers (M.lookup (VerifierName verName)) >>= \case
      Just verCaps -> do
        verifierInScope <- anyCapabilityBeingEvaluated verCaps
        if verifierInScope then return (VBool True)
        else throwUserRecoverableError info $ verifError verName "not in scope"
      Nothing ->
        throwUserRecoverableError info $ (verifError verName "not in transaction")
  args -> argsError info b args
  where
    verifError verName msg = VerifierFailure (VerifierName verName) msg

-----------------------------------
-- Hyperlane
-----------------------------------


coreHyperlaneDecodeTokenMessage :: (IsBuiltin b) => NativeFunction e b i
coreHyperlaneDecodeTokenMessage info b _env = \case
  [VString s] -> do
    chargeGasArgs info $ GHyperlaneEncodeDecodeTokenMessage (T.length s)
    case decodeBase64UrlUnpadded (T.encodeUtf8 s) of
      Left _e -> throwExecutionError info $ HyperlaneDecodeError HyperlaneDecodeErrorBase64
      Right bytes -> case Bin.runGetOrFail (unpackTokenMessageERC20 <* eof) (BS.fromStrict bytes) of
        Left (_, _, e) | "TokenMessage" `L.isPrefixOf` e -> do
          throwExecutionError info $ HyperlaneDecodeError $ HyperlaneDecodeErrorInternal e
        Left _ -> do
          throwExecutionError info $ HyperlaneDecodeError $ HyperlaneDecodeErrorBinary
        Right (_, _, tm) -> case tokenMessageToTerm tm of
          Left e -> throwExecutionError info $ HyperlaneDecodeError e
          Right pv -> return (VPactValue pv)
  args -> argsError info b args

coreHyperlaneMessageId :: (IsBuiltin b) => NativeFunction e b i
coreHyperlaneMessageId info b _env = \case
  [VObject o] -> case decodeHyperlaneMessageObject o of
    Left e -> throwExecutionError info $ HyperlaneError e
    Right r -> do
      let msgId =  getHyperlaneMessageId r
      chargeGasArgs info $ GHyperlaneMessageId (T.length msgId)
      return (VString msgId)
  args -> argsError info b args

coreHyperlaneEncodeTokenMessage :: (IsBuiltin b) => NativeFunction e b i
coreHyperlaneEncodeTokenMessage info b _env = \case
  [VObject o] ->  case decodeHyperlaneTokenMessageObject o of
    Left e -> throwExecutionError info $ HyperlaneError e
    Right r -> do
      let encoded = T.decodeUtf8 $ encodeBase64UrlUnpadded $ BS.toStrict $ Bin.runPut $ Bin.putBuilder $ packTokenMessageERC20 r
      chargeGasArgs info $ GHyperlaneEncodeDecodeTokenMessage (T.length encoded)
      return (VString encoded)
  args -> argsError info b args


-----------------------------------
-- Builtin exports
-----------------------------------


coreBuiltinEnv
  :: forall e i
  . BuiltinEnv e CoreBuiltin i
coreBuiltinEnv i b env = mkDirectBuiltinFn i b env (coreBuiltinRuntime b)

coreBuiltinRuntime
  :: (IsBuiltin b)
  => CoreBuiltin
  -> NativeFunction e b i
coreBuiltinRuntime =
#ifdef WITH_NATIVE_TRACING
  _traceNative . go
  where
  _traceNative
    :: NativeFunction e b i
    -> NativeFunction e b i
  _traceNative f info b env args = do
    timeEnter <- liftIO $ getTime ProcessCPUTime
    esTraceOutput %= (TraceNativeEnter timeEnter b info:)
    output <- f info b env args
    timeExit <- liftIO $ getTime ProcessCPUTime
    esTraceOutput %= (TraceNativeExit timeExit b info:)
    pure output
#else
  go
  where
#endif
  go = \case
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
    CoreKeys -> dbKeys
    CoreRead -> dbRead
    CoreSelect -> dbSelect
    CoreUpdate -> dbUpdate
    CoreWithDefaultRead -> dbWithDefaultRead
    CoreWithRead -> dbWithRead
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
    CoreHyperlaneMessageId -> coreHyperlaneMessageId
    CoreHyperlaneDecodeMessage -> coreHyperlaneDecodeTokenMessage
    CoreHyperlaneEncodeMessage -> coreHyperlaneEncodeTokenMessage
