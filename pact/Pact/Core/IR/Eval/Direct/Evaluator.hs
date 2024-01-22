{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}

module Pact.Core.IR.Eval.Direct.Evaluator where

import Control.Lens
import Control.Monad
import Control.Monad.Except
import GHC.Generics
import Control.DeepSeq
import Data.Text(Text)
import Data.Decimal
import Data.List.NonEmpty(NonEmpty(..))
import Data.RAList(RAList)
import Data.Map.Strict(Map)
import Data.Vector(Vector)
import Pact.Time(UTCTime)
import qualified Data.RAList as RAList
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.List.NonEmpty as NE
import qualified Data.Kind as K


import Pact.Core.IR.Eval.Runtime.Utils
  ( lookupFqName, chargeGasArgs, safeTail,maybeTCType
  , chargeFlatNativeGas, getDefPactId, getDefCap
  , calledByModule, findCallingModule)
import Pact.Core.IR.Eval.Runtime(Eval)
-- import Pact.Core.IR.Eval.Runtime.Utils hiding (sysOnlyEnv, readOnlyEnv)
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
import Data.Foldable (foldl')
import Pact.Core.StableEncoding
import Control.Monad.IO.Class
import Data.List (find)


mkDefunClosure
  :: (MonadEval b i m)
  => EvalDefun b i
  -> ModuleName
  -> DirectEnv b i m
  -> m (Closure b i m)
mkDefunClosure d mn e = case _dfunTerm d of
  Lam args body i ->
    pure (Closure (_dfunName d) mn (ArgClosure args) (NE.length args) body (_dfunRType d) e i)
  Nullary body i ->
    pure (Closure (_dfunName d) mn NullaryClosure 0 body (_dfunRType d) e i)
  _ ->
    throwExecutionError (_dfunInfo d) (DefIsNotClosure (_dfunName d))

mkDefPactClosure
  :: i
  -> FullyQualifiedName
  -> DefPact Name Type b i
  -> DirectEnv b i m
  -> EvalValue b i m
mkDefPactClosure info fqn dpact env = case _dpArgs dpact of
  [] ->
    let dpc = DefPactClosure fqn NullaryClosure 0 env info
    in VDefPactClosure dpc
  (x:xs) ->
    let dpc = DefPactClosure fqn (ArgClosure (x :| xs)) (length (x:xs)) env info
    in VDefPactClosure dpc

evaluate
  :: MonadEval b i m
  => DirectEnv b i m
  -> EvalTerm b i
  -> m (EvalValue b i m)
evaluate env = \case
  Var n info -> do
    case _nKind n of
      NBound i -> do
        case RAList.lookup (_ceLocal env) i of
          Just v -> return v
          Nothing -> failInvariant info ("unbound identifier" <> T.pack (show n))
      -- Top level names are not closures, so we wipe the env
      NTopLevel mname mh -> do
        let fqn = FullyQualifiedName mname (_nName n) mh
        lookupFqName fqn >>= \case
          Just (Dfun d) -> do
            dfunClo <- VDefClosure <$> mkDefunClosure d mname env
            return dfunClo
          -- Todo: this should be GADT'd out
          -- and defconsts should already be evaluated
          Just (DConst d) -> case _dcTerm d of
            -- Note: `TermConst` cannot and should not be `evalCEK`'d. This is an error
            -- this can cause semantic divergences, due to things like provided data.
            -- moreover defcosts are always evaluated in `SysOnly` mode.
            TermConst _term ->
              failInvariant info "Defconst not fully evaluated"
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
            throwExecutionError info (InvalidDefKind (defKind d) "in var position")
          Nothing ->
            throwExecutionError info (NameNotInScope (FullyQualifiedName mname (_nName n) mh))
      NModRef m ifs -> case ifs of
        [x] -> return (VModRef (ModRef m ifs (Just (S.singleton x))))
        [] -> throwExecutionError info (ModRefNotRefined (_nName n))
        _ -> return (VModRef (ModRef m ifs Nothing))
      NDynRef (DynamicRef dArg i) -> case RAList.lookup (view ceLocal env) i of
        Just (VModRef mr) -> do
          modRefHash <- _mHash <$> getModule info (view cePactDb env) (_mrModule mr)
          let nk = NTopLevel (_mrModule mr) modRefHash
          evaluate env (Var (Name dArg nk) info)
        Just _ -> throwRecoverableError info "dynamic name pointed to non-modref"
        Nothing -> failInvariant info ("unbound identifier" <> T.pack (show n))
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
      if b then evaluate env te' >>= enforceBoolValue info
      else pure (VBool False)
    COr te te' -> do
      b <- evaluate env te >>= enforceBool info
      if b then pure (VBool True)
      else evaluate env te' >>= enforceBoolValue info
    CIf bExpr ifExpr elseExpr -> do
      b <- enforceBool info =<< evaluate env bExpr
      if b then evaluate env ifExpr
      else evaluate env elseExpr
    CEnforce cond str -> do
      let env' = sysOnlyEnv env
      b <- enforceBool info =<< evaluate env' cond
      if b then return (VBool True)
      else do
        msg <- enforceString info =<< evaluate env str
        throwRecoverableError info msg
    CEnforceOne _ _ -> error "unsupported: todo"
  CapabilityForm cf info -> case cf of
    WithCapability cap body -> do
      enforceNotWithinDefcap info env "with-capability"
      rawCap <- enforceCapToken info =<< evaluate env cap
      evalCap info PopCapInvoke rawCap body
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
    -- case o of
    --   (f, term):rest -> do
    --     let cont' = ObjC env info f rest [] cont
    --     evalCEK cont' handler env term
    --   [] -> returnCEKValue cont handler (VObject mempty)


evalCap
  :: (MonadEval b i m)
  => i
  -> DirectEnv b i m
  -> FQCapToken
  -> CapPopState
  -> EvalCapType
  -> EvalTerm b i
  -> m (EvalValue b i m)
evalCap info env origToken@(CapToken fqn args) popType ecType contbody = do
  capInStack <- isCapInStack' origToken
  if not capInStack then go else evaluate env contbody
  where
  go = do
    d <- getDefCap info fqn
    when (length args /= length (_dcapArgs d)) $ failInvariant info "Dcap argument length mismatch"
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
                        -- cont' = CapBodyC popType env info (Just qualCapToken) emittedEvent contbody currCont
                    installCap info env c' False >>= evalUserManagedCap newLocals capBody emittedEvent
                  Nothing ->
                    throwExecutionError info (CapNotInstalled fqn)
              Just managedCap -> do
                let emittedEvent = fqctToPactEvent origToken <$ guard (ecType == NormalCapEval)
                let cont' = CapBodyC popType env info (Just qualCapToken) emittedEvent contbody currCont
                evalUserManagedCap cont' newLocals capBody managedCap
          -- handle autonomous caps
          AutoManagedMeta -> do
            -- Find the capability post-filtering
            let emittedEvent = fqctToPactEvent origToken <$ guard (ecType == NormalCapEval)
            -- let cont' = CapBodyC popType env info Nothing emittedEvent contbody currCont
            mgdCaps <- useEvalState (esCaps . csManaged)
            case find ((==) qualCapToken . _mcCap) mgdCaps of
              Nothing -> do
                msgCaps <- S.unions <$> viewEvalEnv eeMsgSigs
                case find (== qualCapToken) msgCaps of
                  Just c -> do
                    let c' = set ctName fqn c
                    installCap info env c' False >>= evalAutomanagedCap emittedEvent newLocals capBody
                  Nothing ->
                    throwExecutionError info (CapNotInstalled fqn)
              Just managedCap ->
                evalAutomanagedCap cont' newLocals capBody managedCap
      DefEvent -> do
        -- let cont' = CapBodyC popType env info Nothing () contbody currCont
        let event = Just (fqctToPactEvent origToken)
        let inCapEnv = set ceInCap True $ set ceLocal newLocals env
        (esCaps . csSlots) %== (CapSlot qualCapToken []:)
        _ <- evalWithStackFrame info capStackFrame Nothing (evaluate inCapEnv capBody)
        evalWithCapBody info popType Nothing event contbody
        -- sfCont <- pushStackFrame info cont' Nothing capStackFrame
        -- evalCEK sfCont handler inCapEnv capBody
      -- Not automanaged _nor_ user managed.
      -- Todo: a type that's basically `Maybe` here would save us a lot of grief.
      Unmanaged -> do
        let inCapEnv = set ceInCap True $ set ceLocal newLocals env
        -- let cont' = if ecType == NormalCapEval then CapBodyC popType env info Nothing Nothing contbody currCont
        --             else currCont
        (esCaps . csSlots) %== (CapSlot qualCapToken []:)
        -- we ignore the capbody here
        _ <- evalWithStackFrame info capStackFrame Nothing $ evaluate inCapEnv capBody
        case ecType of
          NormalCapEval -> do
            evalWithCapBody info popType Nothing Nothing env contbody
          TestCapEval ->
            -- todo: check with prod the return type of testCap
            return VUnit
        -- evalWithStackFrame info cont' handler inCapEnv capStackFrame Nothing capBody
  qualCapName = fqnToQualName fqn
  qualCapToken = CapToken qualCapName args
  capStackFrame = StackFrame (_fqName fqn) (_fqModule fqn) SFDefcap
  -- This function is handles both evaluating the manager function for the installed parameter
  -- and continuing evaluation for the actual capability body.
  evalUserManagedCap cont' env' capBody emitted managedCap = case _mcManaged managedCap of
    ManagedParam mpfqn oldV managedIx -> do
      dfun <- getDefun info mpfqn
      dfunClo <- mkDefunClosure dfun (_fqModule mpfqn) env
      newV <- maybe (failInvariant info "Managed param does not exist at index") pure (args ^? ix managedIx)
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
      sfCont <- pushStackFrame info mgrFunCont Nothing capStackFrame
      evalCEK sfCont handler inCapEnv capBody
    _ -> failInvariant info "Invalid managed cap type"
  evalAutomanagedCap emittedEvent env' capBody managedCap = case _mcManaged managedCap of
    AutoManaged b -> do
      if b then throwRecoverableError info "Automanaged capability used more than once"
      else do
        let newManaged = AutoManaged True
        esCaps . csManaged %== S.union (S.singleton (set mcManaged newManaged managedCap))
        esCaps . csSlots %== (CapSlot qualCapToken []:)
        let inCapEnv = set ceLocal env' $ set ceInCap True $ env
        -- sfCont <- pushStackFrame info cont' Nothing capStackFrame
        -- evalCEK sfCont handler inCapEnv capBody
        _ <- evalWithStackFrame info capStackFrame Nothing (evaluate inCapEnv capBody)
    _ -> failInvariant info "Invalid managed cap type"

evalWithCapBody info cappop mcap mevent env capbody = do
  maybe (pure ()) emitEventUnsafe mevent
  case mcap of
    Nothing -> do
      v <- evaluate env capbody
      popCap info cappop v
    Just cap -> useEvalState (esCaps . csSlots) >>= \case
      (CapSlot _ tl:rest) -> do
        setEvalState (esCaps . csSlots)  (CapSlot cap tl:rest)
        v <- evaluate env capbody
        popCap info cappop v
      [] -> failInvariant info "In CapBodyC but with no caps in stack"

popCap info cappop v = case cappop of
  PopCapInvoke -> v <$ (esCaps . csSlots %== safeTail)
  PopCapComposed -> do
    useEvalState (esCaps . csSlots) >>= \case
      cap:cs -> do
        let csList = _csCap cap : _csComposed cap
            caps' = over (_head . csComposed) (++ csList) cs
        setEvalState (esCaps . csSlots) caps'
        return v
      [] -> failInvariant info "Invariant failure: composed cap with empty cap stack"



-- Todo: fail invariant
-- Todo: is this enough checks for ndynref?
nameToFQN
  :: (MonadEval b i m)
  => i
  -> DirectEnv b i m
  -> Name
  -> m FullyQualifiedName
nameToFQN info env (Name n nk) = case nk of
  NTopLevel mn mh -> pure (FullyQualifiedName mn n mh)
  NDynRef (DynamicRef dArg i) -> case RAList.lookup (view ceLocal env) i of
    Just (VModRef mr) -> do
      md <- getModule info (view cePactDb env) (_mrModule mr)
      pure (FullyQualifiedName (_mrModule mr) dArg (_mHash md))
    Just _ -> throwExecutionError info (DynNameIsNotModRef dArg)
    Nothing -> failInvariant info ("unbound identifier " <> n)
  _ -> failInvariant info ("invalid name in fq position " <> n)

-- Todo: should we typecheck / arity check here?
createUserGuard
  :: (MonadEval b i m)
  => i
  -> FullyQualifiedName
  -> [PactValue]
  -> m (EvalValue b i m)
createUserGuard info fqn args =
  lookupFqName fqn >>= \case
    Just (Dfun _) ->
      return (VGuard (GUserGuard (UserGuard (fqnToQualName fqn) args)))
    Just _ ->
      throwRecoverableError  info "create-user-guard pointing to non-guard"
    Nothing ->
      failInvariant info "User guard pointing to no defn"

enforceNotWithinDefcap
  :: (MonadEval b i m)
  => i
  -> DirectEnv b i m
  -> T.Text
  -> m ()
enforceNotWithinDefcap info env form =
  when (_ceInCap env) $ throwExecutionError info (FormIllegalWithinDefcap form)

enforceBool :: (MonadEval b i m) => i -> EvalValue b i m -> m Bool
enforceBool info = \case
  VBool b -> pure b
  _ -> failInvariant info "Expected bool"

enforceString :: (MonadEval b i m) => i -> EvalValue b i m -> m Text
enforceString info = \case
  VString b -> pure b
  _ -> failInvariant info "Expected bool"


enforceCapToken :: (MonadEval b i m) => i -> EvalValue b i m -> m (CapToken FullyQualifiedName PactValue)
enforceCapToken info = \case
  VCapToken b -> pure b
  _ -> failInvariant info "Expected cap token"

enforceBoolValue :: (MonadEval b i m) => i -> EvalValue b i m -> m (EvalValue b i m)
enforceBoolValue info = \case
  VBool b -> pure (VBool b)
  _ -> failInvariant info "Expected bool"

enforceUserAppClosure :: (MonadEval b i m) => i -> EvalValue b i m -> m (CanApply b i m)
enforceUserAppClosure info = \case
  VClosure c -> case c of
    C clo -> pure (C clo)
    LC clo -> pure (LC clo)
    N clo -> pure (N clo)
    DPC clo -> pure (DPC clo)
    CT clo -> pure (CT clo)
    _ -> throwExecutionError info CannotApplyPartialClosure
  _ -> failInvariant info "Cannot apply non-function to arguments"


enforcePactValue :: (MonadEval b i m) => i -> EvalValue b i m -> m PactValue
enforcePactValue info = \case
  VPactValue pv -> pure pv
  _ -> throwExecutionError info ExpectedPactValue

enforcePactValue' :: (MonadEval b i m) => i -> EvalValue b i m -> m (EvalValue b i m)
enforcePactValue' info = \case
  VPactValue pv -> pure (VPactValue pv)
  _ -> throwExecutionError info ExpectedPactValue

catchRecoverable :: forall b i m a. (MonadEval b i m) => m a -> (i -> RecoverableError -> m a) -> m a
catchRecoverable act catch = catchError act handler
  where
  handler :: PactError i -> m a
  handler (PERecoverableError r i) = catch i r
  handler e = throwError e

readOnlyEnv :: DirectEnv b i m -> DirectEnv b i m
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
             , _pdbTxIds = \_ _ -> dbOpDisallowed
             , _pdbGetTxLog = \_ _ -> dbOpDisallowed
             }
      in set cePactDb newPactdb e

sysOnlyEnv :: forall b i m. DirectEnv b i m -> DirectEnv b i m
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
         , _pdbTxIds = \_ _ -> dbOpDisallowed
         , _pdbGetTxLog = \_ _ -> dbOpDisallowed
         }
  in set cePactDb newPactdb e
  where
  pdb = view cePactDb e
  read' :: Domain k v b i -> k -> IO (Maybe v)
  read' dom k = case dom of
    DUserTables _ -> dbOpDisallowed
    _ -> _pdbRead pdb dom k

evalWithStackFrame :: MonadEval b i m => i -> StackFrame -> Maybe Type ->  m (EvalValue b i m) -> m (EvalValue b i m)
evalWithStackFrame info sf mty act = do
  esStack %== (sf:)
  v <- act
  esStack %== safeTail
  pv <- enforcePactValue info v
  pv' <- maybeTCType info pv mty
  return (VPactValue pv')
{-# INLINE evalWithStackFrame #-}


applyLam
  :: (MonadEval b i m)
  => CanApply b i m
  -> [EvalValue b i m]
  -> m (EvalValue b i m)
applyLam vc@(C (Closure fn mn ca arity term mty env cloi)) args
  | arity == argLen = case ca of
    ArgClosure cloargs -> do
      let qn = QualifiedName fn mn
      chargeGasArgs cloi (GAApplyLam (renderQualName qn) argLen)
      args' <- traverse (enforcePactValue cloi) args
      tcArgs <- zipWithM (\arg (Arg _ ty) -> VPactValue <$> maybeTCType cloi arg ty) args' (NE.toList cloargs)
      let sf = StackFrame fn mn SFDefun
          varEnv = RAList.fromList (reverse tcArgs)
      evalWithStackFrame cloi sf mty (evaluate (set ceLocal varEnv env) term)
    NullaryClosure -> do
      evalWithStackFrame cloi (StackFrame fn mn SFDefun) mty $ evaluate (set ceLocal mempty env) term
  | argLen > arity = throwExecutionError cloi ClosureAppliedToTooManyArgs
  | otherwise = case ca of
    NullaryClosure -> throwExecutionError cloi ClosureAppliedToTooManyArgs
    ArgClosure cloargs
      | null args ->
        return (VClosure vc)
      | otherwise -> do
        chargeGasArgs cloi (GAApplyLam fn argLen)
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
    return (VPartialClosure pclo)
  apply' _ [] _ = throwExecutionError cloi ClosureAppliedToTooManyArgs

applyLam (LC (LamClosure ca arity term mty env cloi)) args
  | arity == argLen = case ca of
    ArgClosure _ -> do
      -- Todo: maybe lambda application should mangle some sort of name?
      chargeGasArgs cloi (GAApplyLam "#lambda" argLen)
      let locals = view ceLocal env
          locals' = foldl' (flip RAList.cons) locals args
      evaluate (set ceLocal locals' env) term
    NullaryClosure -> do
      evaluate env term >>= enforcePactValue' cloi
  | argLen > arity = throwExecutionError cloi ClosureAppliedToTooManyArgs
  | otherwise = case ca of
      NullaryClosure -> throwExecutionError cloi ClosureAppliedToTooManyArgs
      ArgClosure cloargs -> do
        chargeGasArgs cloi (GAApplyLam "#lambda" argLen)
        apply' (view ceLocal env) (NE.toList cloargs) args
  where
  argLen = length args
  -- Todo: runtime TC here
  apply' e (Arg _ ty:tys) (x:xs) = do
    x' <- (\pv -> maybeTCType cloi pv ty) =<< enforcePactValue cloi x
    apply' (RAList.cons (VPactValue x') e) tys xs
  apply' e [] [] = do
    evaluate (set ceLocal e env) term
  apply' e (ty:tys) [] =
    return (VPartialClosure (PartialClosure Nothing (ty :| tys) (length tys + 1) term mty (set ceLocal e env) cloi))
  apply' _ [] _ = throwExecutionError cloi ClosureAppliedToTooManyArgs
applyLam (PC (PartialClosure li argtys _ term mty env cloi)) args = do
  chargeGasArgs cloi (GAApplyLam (getSfName li) (length args))
  apply' (view ceLocal env) (NE.toList argtys) args
  where
  apply' e (Arg _ ty:tys) (x:xs) = do
    x' <- (\pv -> maybeTCType cloi pv ty) =<< enforcePactValue cloi x
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
applyLam (CT (CapTokenClosure fqn argtys arity i)) args
  | arity == argLen = do
    chargeGasArgs i (GAApplyLam (renderQualName (fqnToQualName fqn)) (fromIntegral argLen))
    args' <- traverse (enforcePactValue i) args
    tcArgs <- zipWithM (\arg ty -> maybeTCType i arg ty) args' argtys
    return (VPactValue (PCapToken (CapToken fqn tcArgs)))
  | otherwise = throwExecutionError i ClosureAppliedToTooManyArgs
  where
  argLen = length args
applyLam (DPC (DefPactClosure fqn argtys arity env i)) args
  | arity == argLen = case argtys of
    ArgClosure cloargs -> do
      -- Todo: defpact has much higher overhead, we must charge a bit more gas for this
      chargeGasArgs i (GAApplyLam (renderQualName (fqnToQualName fqn)) (fromIntegral argLen))
      args' <- traverse (enforcePactValue i) args
      tcArgs <- zipWithM (\arg (Arg _ ty) -> maybeTCType i arg ty) args' (NE.toList cloargs)
      let pc = DefPactContinuation (fqnToQualName fqn) tcArgs
          env' = set ceLocal (RAList.fromList (reverse (VPactValue <$> tcArgs))) env
      initPact i pc env'
    NullaryClosure -> do
      chargeGasArgs i (GAApplyLam (renderQualName (fqnToQualName fqn)) (fromIntegral argLen))
      let pc = DefPactContinuation (fqnToQualName fqn) []
          env' = set ceLocal mempty env
      -- Todo: defpact has much higher overhead, we must charge a bit more gas for this
      initPact i pc env'
  | otherwise = throwExecutionError i ClosureAppliedToTooManyArgs
  where
  argLen = length args

getSfName :: Maybe StackFrame -> T.Text
getSfName = \case
  Just sf -> renderQualName (QualifiedName (_sfFunction sf) (_sfModule sf))
  Nothing -> "#lambda"



------------------------------------------------------
-- Guards
------------------------------------------------------

enforceGuard
  :: (MonadEval b i m)
  => i
  -> DirectEnv b i m
  -> Guard QualifiedName PactValue
  -> m (EvalValue b i m)
enforceGuard info env g = case g of
  GKeyset ks -> do
    isKeysetInSigs info env ks
  GKeySetRef ksn -> do
    isKeysetNameInSigs info  env ksn
  GUserGuard ug -> runUserGuard info env ug
  GCapabilityGuard cg -> enforceCapGuard info cg
  GModuleGuard (ModuleGuard mn _) -> calledByModule mn >>= \case
    True -> return (VBool True)
    False -> do
      acquireModuleAdminCapability info env mn
      return (VBool True)
  GDefPactGuard (DefPactGuard dpid _) -> do
    curDpid <- getDefPactId info
    if curDpid == dpid
       then return (VBool True)
       else throwRecoverableError info "Capability pact guard failed: invalid pact id"

-- | Acquires module admin for a known module
-- NOTE: This function should only be called _after_
-- checking whether `esCaps . csModuleAdmin` for the particular
-- module is in scope
acquireModuleAdmin
  :: (MonadEval b i m)
  => i
  -> DirectEnv b i m
  -> EvalModule b i
  -> m ()
acquireModuleAdmin i env mdl = do
  case _mGovernance mdl of
    KeyGov ksn -> do
      () <$ isKeysetNameInSigs i env ksn
    CapGov (FQName fqn) -> do
      let wcapBody = Constant LUnit i
      () <$ evalCap i env (CapToken fqn []) PopCapInvoke NormalCapEval wcapBody
  (esCaps . csModuleAdmin) %== S.insert (_mName mdl)


acquireModuleAdminCapability
  :: (MonadEval b i m)
  => i
  -> DirectEnv b i m
  -> ModuleName
  -> m ()
acquireModuleAdminCapability i env mname = do
  sc <- S.member mname <$> useEvalState (esCaps . csModuleAdmin)
  unless sc $ getModule i (_cePactDb env) mname >>= acquireModuleAdmin i env


runUserGuard
  :: (MonadEval b i m)
  => i
  -> DirectEnv b i m
  -> UserGuard QualifiedName PactValue
  -> m (EvalValue b i m)
runUserGuard info env (UserGuard qn args) =
  getModuleMember info (_cePactDb env) qn >>= \case
    Dfun d -> do
      when (length (_dfunArgs d) /= length args) $ throwExecutionError info CannotApplyPartialClosure
      let env' = sysOnlyEnv env
      clo <- mkDefunClosure d (_qnModName qn) env'
      -- Todo: sys only here
      VBool True <$ applyLam (C clo) (VPactValue <$> args)
    d -> throwExecutionError info (InvalidDefKind (defKind d) "run-user-guard")

enforceCapGuard
  :: (MonadEval b i m)
  => i
  -> CapabilityGuard QualifiedName PactValue
  -> m (EvalValue b i m)
enforceCapGuard info (CapabilityGuard qn args mpid) = case mpid of
  Nothing -> enforceCap
  Just pid -> do
    currPid <- getDefPactId info
    if currPid == pid then enforceCap
    else throwRecoverableError info "Capability pact guard failed: invalid pact id"
  where
  enforceCap = do
    cond <- isCapInStack (CapToken qn args)
    if cond then return (VBool True)
    else do
      let errMsg = "Capability guard enforce failure cap not in scope: " <> renderQualName qn
      throwRecoverableError info errMsg

-- Keyset Code
isKeysetInSigs
  :: (MonadEval b i m)
  => i
  -> DirectEnv b i m
  -> KeySet
  -> m (EvalValue b i m)
isKeysetInSigs info env (KeySet kskeys ksPred) = do
  matchedSigs <- M.filterWithKey matchKey <$> viewEvalEnv eeMsgSigs
  sigs <- checkSigCaps matchedSigs
  runPred (M.size sigs)
  where
  matchKey k _ = k `elem` kskeys
  atLeast t m = m >= t
  count = S.size kskeys
  run p matched =
    if p count matched then pure $ VBool True
    else throwRecoverableError info "keyset enforce failure"
  runPred matched =
    case ksPred of
      KeysAll -> run atLeast matched
      KeysAny -> run (\_ m -> atLeast 1 m) matched
      Keys2 -> run (\_ m -> atLeast 2 m) matched
      CustomPredicate n -> runCustomPred matched n
  runCustomPred matched = \case
    TQN qn -> do
      pdb <- viewEvalEnv eePactDb
      getModuleMember info pdb qn >>= \case
        Dfun d -> do
          clo <- mkDefunClosure d (_qnModName qn) env
          p <- enforceBool info =<< applyLam (C clo) [VInteger (fromIntegral count), VInteger (fromIntegral matched)]
          if p then pure (VBool True)
          else throwRecoverableError info "keyset enforce failure"
        _ -> failInvariant info "invalid def type for custom keyset predicate"
    TBN (BareName bn) -> do
      m <- viewEvalEnv eeNatives
      case M.lookup bn m of
        Just b -> do
          let builtins = view ceBuiltins env
          let nativeclo = builtins info b env
          p <- enforceBool info =<< applyLam (N nativeclo) [VInteger (fromIntegral count), VInteger (fromIntegral matched)]
          if p then pure (VBool True)
          else throwRecoverableError info "keyset enforce failure"
        Nothing ->
          failInvariant info "could not find native definition for custom predicate"

isKeysetNameInSigs
  :: (MonadEval b i m)
  => i
  -> DirectEnv b i m
  -> KeySetName
  -> m (EvalValue b i m)
isKeysetNameInSigs info env ksn = do
  pdb <- viewEvalEnv eePactDb
  liftDbFunction info (readKeySet pdb ksn) >>= \case
    Just ks -> isKeysetInSigs info env ks
    Nothing ->
      throwExecutionError info (NoSuchKeySet ksn)


------------------------------------------------------
-- Capabilities
------------------------------------------------------

requireCap
  :: (MonadEval b i m)
  => i
  -> FQCapToken
  -> m (EvalValue b i m)
requireCap info (CapToken fqn args) = do
  capInStack <- isCapInStack (CapToken (fqnToQualName fqn) args)
  if capInStack then return (VBool True)
  else throwRecoverableError info
      ("cap not in scope " <> renderQualName (fqnToQualName fqn))

isCapInStack
  :: (MonadEval b i m)
  => CapToken QualifiedName PactValue
  -> m Bool
isCapInStack ct = do
  capSet <- getAllStackCaps
  pure $ S.member ct capSet
{-# SPECIALIZE isCapInStack
   :: CapToken QualifiedName PactValue
   -> Eval Bool
    #-}

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
  :: (MonadEval b i m)
  => i
  -> DirectEnv b i m
  -> FQCapToken
  -> m (EvalValue b i m)
composeCap info env origToken =
  isCapInStack' origToken >>= \case
    False ->
      evalCap info env origToken PopCapComposed NormalCapEval (Constant (LBool True) info)
    True ->
      return (VBool True)
{-# SPECIALIZE composeCap
   :: ()
   -> DirectEnv CoreBuiltin () Eval
   -> FQCapToken
   -> Eval (EvalValue CoreBuiltin () Eval)
    #-}

filterIndex :: Int -> [a] -> [a]
filterIndex i xs = [x | (x, i') <- zip xs [0..], i /= i']

findMsgSigCap :: Int -> CapToken QualifiedName PactValue -> CapToken QualifiedName PactValue -> Bool
findMsgSigCap cix ct1 ct2 =
  _ctName ct1 == _ctName ct2 && (_ctArgs ct1 == filterIndex cix (_ctArgs ct2))

-- Todo:
-- `capAutonomous` are what we should use to match semantics accurately.
installCap
  :: (MonadEval b i m)
  => i
  -> DirectEnv b i m
  -> FQCapToken
  -> Bool
  -> m (ManagedCap QualifiedName PactValue)
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




------------------------------------------------------
-- DefPacts
------------------------------------------------------
initPact
  :: (MonadEval b i m)
  => i
  -> DefPactContinuation QualifiedName PactValue
  -> DirectEnv b i m
  -> m (EvalValue b i m)
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
  :: (MonadEval b i m)
  => i
  -> DefPactContinuation QualifiedName PactValue
  -> DefPactStep
  -> DirectEnv b i m
  -> M.Map DefPactId DefPactExec
  -> m (EvalValue b i m)
applyPact i pc ps cenv nested = useEvalState esDefPactExec >>= \case
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

      result <- case (ps ^. psRollback, step) of
        (False, _) ->
          evalWithStackFrame i sf Nothing $ evaluate cenv (ordinaryDefPactStepExec step)
        (True, StepWithRollback _ rollbackExpr) ->
          evalWithStackFrame i sf Nothing $ evaluate cenv rollbackExpr
        (True, Step{}) ->
          throwExecutionError i (DefPactStepHasNoRollback ps)

      -- After evaluation, check the result state
      useEvalState esDefPactExec >>= \case
        Nothing -> failInvariant i "No PactExec found"
        Just resultExec -> case cenv ^. ceDefPactStep of
          Nothing -> failInvariant i "Expected a PactStep in the environment"
          Just ps' -> do
            let
              pdb = view cePactDb cenv
              isLastStep = _psStep ps' == pred (_peStepCount resultExec)
              done = (not (_psRollback ps') && isLastStep) || _psRollback ps'
            when (nestedPactsNotAdvanced resultExec ps') $
              throwExecutionError i (NestedDefpactsNotAdvanced (_peDefPactId resultExec))
            liftDbFunction i
              (writeDefPacts pdb Write (_psDefPactId ps')
                (if done then Nothing else Just resultExec))
            emitXChainEvents (_psResume ps') resultExec
            return result

    _otherwise -> failInvariant i "defpact continuation does not point to defun"
  where
  sf = StackFrame (view (pcName . qnName) pc) (view (pcName . qnModName) pc) SFDefPact
{-# SPECIALIZE applyPact
   :: ()
   -> DefPactContinuation QualifiedName PactValue
   -> DefPactStep
   -> DirectEnv CoreBuiltin () Eval
   -> M.Map DefPactId DefPactExec
   -> Eval (EvalValue CoreBuiltin () Eval)
    #-}

applyNestedPact
  :: (MonadEval b i m)
  => i
  -> DefPactContinuation QualifiedName PactValue
  -> DefPactStep
  -> DirectEnv b i m
  -> m (EvalValue b i m)
applyNestedPact i pc ps cenv = useEvalState esDefPactExec >>= \case
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

      result <- case (ps ^. psRollback, step) of
        (False, _) ->
          evalWithStackFrame i sf Nothing $ evaluate cenv' (ordinaryDefPactStepExec step)
        (True, StepWithRollback _ rollbackExpr) ->
          evalWithStackFrame i sf Nothing $ evaluate cenv' rollbackExpr
        (True, Step{}) -> throwExecutionError i (DefPactStepHasNoRollback ps)
      useEvalState esDefPactExec >>= \case
        Nothing -> failInvariant i "No DefPactExec found"
        Just resultExec -> do
          -- case cenv ^. ceDefPactStep of
          -- Nothing -> failInvariant i "Expected a DefPactStep in the environment"
          -- Just ps -> do
          when (nestedPactsNotAdvanced resultExec ps) $
            throwExecutionError i (NestedDefpactsNotAdvanced (_peDefPactId resultExec))
          let npe = pe & peNestedDefPactExec %~ M.insert (_psDefPactId ps) resultExec
          setEvalState esDefPactExec (Just npe)
          return result
    _otherwise -> failInvariant i "applyNestedPact: Expected a DefPact bot got something else"
  where
  sf = StackFrame (view (pcName . qnName) pc) (view (pcName . qnModName) pc) SFDefPact
{-# SPECIALIZE applyNestedPact
   :: ()
   -> DefPactContinuation QualifiedName PactValue
   -> DefPactStep
   -> DirectEnv CoreBuiltin () Eval
   -> Eval (EvalValue CoreBuiltin () Eval)
    #-}

resumePact
  :: (MonadEval b i m)
  => i
  -> DirectEnv b i m
  -> Maybe DefPactExec
  -> m (EvalValue b i m)
resumePact i env crossChainContinuation = viewEvalEnv eeDefPactStep >>= \case
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
        --resumeDefPactExec :: CEKEval step b i m, MonadEval b i m => DefPactExec -> m (CEKEvalResult step b i m)
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
          applyPact i pc ps env' (_peNestedDefPactExec pe)
{-# SPECIALIZE resumePact
   :: ()
   -> DirectEnv CoreBuiltin () Eval
   -> Maybe DefPactExec
   -> Eval (EvalValue CoreBuiltin () Eval)
    #-}

emitXChainEvents
  :: (MonadEval b i m)
  => Maybe Yield
  -- ^ from '_psResume', indicating a cross-chain resume.
  -> DefPactExec
   -- ^ tested for yield provenance to indicate a cross-chain yield.
  -> m ()
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

emitReservedEvent :: MonadEval b i m => T.Text -> [PactValue] -> ModuleHash -> m ()
emitReservedEvent name params mhash = do
  let pactModule = ModuleName "pact" Nothing
  let pe = PactEvent name params pactModule mhash
  emitEventUnsafe pe

emitEventUnsafe
  :: (MonadEval b i m)
  => PactEvent PactValue
  -> m ()
emitEventUnsafe pe = esEvents %== (++ [pe])

emitCapability
  :: (MonadEval b i m)
  => i
  -> CapToken FullyQualifiedName PactValue
  -> m ()
emitCapability info tkn =
  emitEvent info (fqctToPactEvent tkn)
{-# SPECIALIZE emitCapability
   :: ()
   -> CapToken FullyQualifiedName PactValue
   -> Eval ()
    #-}

emitEvent
  :: (MonadEval b i m)
  => i
  -> PactEvent PactValue
  -> m ()
emitEvent info pe = findCallingModule >>= \case
    Just mn -> do
      -- Todo: ++ definitely feels suboptimal, especially for gas.
      -- That said: we can simply reverse the events in `env-events` as
      -- well as after final emission.
      let ctModule = _peModule pe
      if ctModule == mn then do
        esEvents %== (++ [pe])
      else throwExecutionError info (EventDoesNotMatchModule mn)
    Nothing -> failInvariant info "emit-event called outside of module code"

fqctToPactEvent :: CapToken FullyQualifiedName PactValue -> PactEvent PactValue
fqctToPactEvent (CapToken fqn args) = PactEvent (_fqName fqn) args (_fqModule fqn) (_fqHash fqn)

------------------------------------------------------
-- Utilities
------------------------------------------------------

failInvariant :: MonadEval b i m => i -> Text -> m a
failInvariant i b =
  let e = PEExecutionError (InvariantFailure b) i
  in throwError e

tryNodeGas :: MilliGas
tryNodeGas = (MilliGas 100)
