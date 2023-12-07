{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}


module Pact.Core.IR.Desugar
 ( runDesugarTerm
 , runDesugarTopLevel
 , runDesugarReplTopLevel
 , DesugarOutput(..)
 , DesugarBuiltin(..)
 ) where

import Control.Applicative((<|>))
import Control.Monad ( when, forM, (>=>), unless)
import Control.Monad.Reader
import Control.Monad.State.Strict ( StateT(..), MonadState )
import Control.Monad.Trans.Maybe(MaybeT(..), runMaybeT, hoistMaybe)
import Control.Monad.Except
import Control.Lens hiding (List)
import Data.Text(Text)
import Data.Map.Strict(Map)
import Data.Maybe(mapMaybe, isJust)
import Data.List(findIndex)
import Data.List.NonEmpty(NonEmpty(..))
import Data.Set(Set)
import Data.Graph(stronglyConnComp, SCC(..))
import Data.Foldable(find, traverse_, foldrM, foldl', foldlM)
import qualified Data.Map.Strict as M
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S

import Pact.Core.Builtin
import Pact.Core.Names
import Pact.Core.Type
import Pact.Core.Literal
import Pact.Core.Hash
import Pact.Core.Persistence hiding (loaded)
import Pact.Core.Capabilities
import Pact.Core.Errors
import Pact.Core.IR.Term
import Pact.Core.Guards
import Pact.Core.Imports
import Pact.Core.Environment
import Pact.Core.Gas
import Pact.Core.Namespace

import qualified Pact.Core.Syntax.ParseTree as Lisp

{- Note on Desugaring + Renaming:

  [Desugaring]
  In core, we have partially closures as semantic values, auto-curried application
  for certain natives, and native names with arity multiple arities. That is,
  a native like `select` has a 2-argument and 3-argument list version.
  Thus, in pact-core, our solution to this is to pick a "Default" arity to resolve for natives,
  and let fully saturated application of certain natives determine the overload. To do this,
  We desugar saturated arity applications for certain natives and make them resolve to the intended
  overload.

  [Renaming]
  In core, we use a locally nameless representation for bound variable representation via basic debruijn indices.
-}

type DesugarType = Lisp.Type

data RenamerEnv b i
  = RenamerEnv
  { _reBinds :: Map Text (NameKind, Maybe DefKind)
  , _reVarDepth :: DeBruijn
  , _reCurrModule :: Maybe (ModuleName, [ModuleName])
  , _reCurrDef :: Maybe DefKind
  }
makeLenses ''RenamerEnv

-- Our type to keep track of
newtype RenamerState
  = RenamerState { _rsDependencies :: Set ModuleName }

makeLenses ''RenamerState

newtype RenamerT b i m a =
  RenamerT (StateT RenamerState (ReaderT (RenamerEnv b i) m) a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (RenamerEnv b i)
    , MonadState RenamerState
    , MonadIO)
  via (StateT RenamerState (ReaderT (RenamerEnv b i) m))

instance MonadTrans (RenamerT b i) where
  lift = RenamerT . lift . lift

instance MonadGas m => MonadGas (RenamerT b i m) where
  logGas logText g = lift (logGas logText g)
  chargeGas g = lift (chargeGas g)


instance (MonadEvalEnv b i m) => MonadEvalEnv b i (RenamerT b i m) where
  readEnv = RenamerT (lift (lift readEnv))

instance (MonadEvalState b i m) => MonadEvalState b i (RenamerT b i m) where
  getEvalState = RenamerT (lift (lift getEvalState))
  putEvalState e = RenamerT (lift (lift (putEvalState e)))
  modifyEvalState f = RenamerT (lift (lift (modifyEvalState f)))

-- Todo: DesugarBuiltin
-- probably should just be a `data` definition we pass in.
-- This class is causing us to use `Proxy`
class DesugarBuiltin b where
  liftRaw :: RawBuiltin -> b
  desugarOperator :: i -> Lisp.Operator -> Term ParsedName DesugarType b i
  desugarAppArity :: i -> b -> [Term ParsedName DesugarType b i] -> Term ParsedName DesugarType b i

instance DesugarBuiltin RawBuiltin where
  liftRaw = id
  desugarOperator info = \case
    -- Manual eta expansion for and as well as Or
    Lisp.AndOp -> let
      arg1Name = "#andArg1"
      arg1 = Arg arg1Name (Just (Lisp.TyPrim PrimBool))
      arg2Name = "#andArg2"
      arg2 = Arg arg2Name (Just (Lisp.TyPrim PrimBool))
      in Lam AnonLamInfo (arg1 :| [arg2]) (Conditional (CAnd (Var (BN (BareName arg1Name)) info) (Var (BN (BareName arg2Name)) info)) info) info
    Lisp.OrOp -> let
      arg1Name = "#orArg1"
      arg1 = Arg arg1Name (Just (Lisp.TyPrim PrimBool))
      arg2Name = "#orArg2"
      arg2 = Arg arg2Name (Just (Lisp.TyPrim PrimBool))
      in Lam AnonLamInfo (arg1 :| [arg2]) (Conditional (COr (Var (BN (BareName arg1Name)) info) (Var (BN (BareName arg2Name)) info)) info) info
    Lisp.EnforceOp -> let
      arg1Name = "#enforceArg1"
      arg1 = Arg arg1Name (Just (Lisp.TyPrim PrimBool))
      arg2Name = "#enforceArg2"
      arg2 = Arg arg2Name (Just (Lisp.TyPrim PrimString))
      in Lam AnonLamInfo (arg1 :| [arg2]) (Conditional (CEnforce (Var (BN (BareName arg1Name)) info) (Var (BN (BareName arg2Name)) info)) info) info
    Lisp.EnforceOneOp -> let
      arg1Name = "#enforceOneArg1"
      arg1 = Arg arg1Name (Just (Lisp.TyPrim PrimString))
      arg2Name = "#enforceOneArg2"
      arg2 = Arg arg2Name (Just (Lisp.TyList (Lisp.TyPrim PrimBool)))
      in Lam AnonLamInfo (arg1 :| [arg2]) (Conditional (CEnforceOne (Var (BN (BareName arg1Name)) info) [Var (BN (BareName arg2Name)) info]) info) info
  desugarAppArity = desugarAppArityRaw id

desugarAppArityRaw
  :: (RawBuiltin -> builtin)
  -> info
  -> RawBuiltin
  -> [Term name Lisp.Type builtin info]
  -> Term name Lisp.Type builtin info
-- Todo: this presents a really, _really_ annoying case for the map overload :(
-- Jose: I am unsure how to fix this so far, but it does not break any tests.
-- that is:
-- prod:
--   pact> (map (- 1) [1, 2, 3])
--   [0 -1 -2]
-- core:
--   pact>(map (- 1) [1 2 3])
--   (interactive):1:0: Native evaluation error for native map, received incorrect argument(s) of type(s) [integer] , [list]
--   1 | (map (- 1) [1 2 3])
--     | ^^^^^^^^^^^^^^^^^^^

--   pact>(map (lambda (x) (- 1 x)) [1 2 3])
--   [0, -1, -2]
-- this is because prod simply suspends the static term without figuring out the arity which is being used
-- to apply, vs core which does not attempt to do this, and picks an overload eagerly and statically.
-- in 99% of cases this is fine, but we overloaded `-` to be completely different functions.
desugarAppArityRaw f i RawSub [e1] =
    App (Builtin (f RawNegate) i) ([e1]) i
desugarAppArityRaw f i RawEnumerate [e1, e2, e3] =
    App (Builtin (f RawEnumerateStepN) i) ([e1, e2, e3]) i
desugarAppArityRaw f i RawSelect [e1, e2, e3] =
    App (Builtin (f RawSelectWithFields) i) ([e1, e2, e3]) i
desugarAppArityRaw f i RawSort [e1, e2] =
  App (Builtin (f RawSortObject) i) [e1, e2] i
-- Rounding functions
desugarAppArityRaw f i RawRound [e1, e2] =
  App (Builtin (f RawRoundPrec) i) [e1, e2] i
desugarAppArityRaw f i RawCeiling [e1, e2] =
  App (Builtin (f RawCeilingPrec) i) [e1, e2] i
desugarAppArityRaw f i RawFloor [e1, e2] =
  App (Builtin (f RawFloorPrec) i) [e1, e2] i


desugarAppArityRaw f i RawStrToInt [e1, e2] =
  App (Builtin (f RawStrToIntBase) i) [e1, e2] i
desugarAppArityRaw f i RawReadMsg [] =
  App (Builtin (f RawReadMsgDefault) i) [] i
desugarAppArityRaw f i RawDefineKeySet [e1] =
  App (Builtin (f RawDefineKeysetData) i) [e1] i
desugarAppArityRaw f i RawPoseidonHashHackachain li =
  App (Builtin (f RawPoseidonHashHackachain) i )[(ListLit li i)] i
desugarAppArityRaw f i RawYield [e1, e2] =
  App (Builtin (f RawYieldToChain) i) [e1, e2] i
desugarAppArityRaw f i b args =
    App (Builtin (f b) i) args i

instance DesugarBuiltin (ReplBuiltin RawBuiltin) where
  liftRaw :: RawBuiltin -> ReplBuiltin RawBuiltin
  liftRaw = RBuiltinWrap
  desugarOperator i dsg =
    over termBuiltin RBuiltinWrap $ desugarOperator i dsg
  desugarAppArity i (RBuiltinWrap b) ne =
    desugarAppArityRaw RBuiltinWrap i b ne
  -- (expect <description> <expected> <expression-to-eval>)
  desugarAppArity i (RBuiltinRepl RExpect) ([e1, e2, e3]) | isn't _Nullary e3 =
    App (Builtin (RBuiltinRepl RExpect) i) ([e1, e2, suspendTerm e3]) i
  -- (expect-failure <arg1> <term>)
  desugarAppArity i (RBuiltinRepl RExpectFailure) [e1, e2] | isn't _Nullary e2 =
    App (Builtin (RBuiltinRepl RExpectFailure) i) [e1, suspendTerm e2] i
  -- (expect-failure <arg1> <expected-msg> <term>)
  desugarAppArity i (RBuiltinRepl RExpectFailure) [e1, e2, e3] | isn't _Nullary e2 =
    App (Builtin (RBuiltinRepl RExpectFailureMatch) i) [e1, e2, suspendTerm e3] i
  -- (pact-state <arg>)
  desugarAppArity i (RBuiltinRepl RPactState) [e1] =
    App (Builtin (RBuiltinRepl RResetPactState) i) [e1] i
  -- (continue-pact <arg1> <arg2>)
  desugarAppArity i (RBuiltinRepl RContinuePact) [e1, e2] =
    App (Builtin (RBuiltinRepl RContinuePactRollback) i) [e1, e2] i
  -- (continue-pact <arg1> <arg2> <arg3>)
  desugarAppArity i (RBuiltinRepl RContinuePact) [e1, e2, e3] =
      App (Builtin (RBuiltinRepl RContinuePactRollbackYield) i) [e1, e2, e3] i
  -- (continue-pact <arg1> <arg2> <arg3> <arg4>)
  desugarAppArity i (RBuiltinRepl RContinuePact) [e1, e2, e3, e4] =
      App (Builtin (RBuiltinRepl RContinuePactRollbackYieldObj) i) [e1, e2, e3, e4] i
  desugarAppArity i b ne =
    App (Builtin b i) ne i


data DesugarOutput a
  = DesugarOutput
  { _dsOut :: a
  , _dsDeps :: Set ModuleName
  } deriving (Show, Functor)

dsOut :: Lens (DesugarOutput a) (DesugarOutput a') a a'
dsOut f (DesugarOutput a d) =
  f a <&> \a' -> DesugarOutput a' d

throwDesugarError :: MonadError (PactError i) m => DesugarError -> i -> RenamerT b i m a
throwDesugarError de = RenamerT . lift . throwError . PEDesugarError de

desugarLispTerm
  :: (MonadEval b i m, DesugarBuiltin b)
  => Lisp.Expr i
  -> RenamerT b i m (Term ParsedName DesugarType b i)
desugarLispTerm = \case
  Lisp.Var (BN n) i  -> do
    reservedNatives <- viewEvalEnv eeNatives
    case M.lookup (_bnName n) reservedNatives of
      Just b -> pure (Builtin b i)
      Nothing
        | n == BareName "constantly" -> do
          let c1 = Arg cvar1 Nothing
              c2 = Arg cvar2 Nothing
          pure $ Lam AnonLamInfo (c1 :| [c2]) (Var (BN (BareName cvar1)) i) i
        | n == BareName "identity" -> do
          let c1 = Arg ivar1 Nothing
          pure $ Lam AnonLamInfo (c1 :| []) (Var (BN (BareName ivar1)) i) i
        | n == BareName "CHARSET_ASCII" -> pure (Constant (LInteger 0) i)
        | n == BareName "CHARSET_LATIN1" -> pure (Constant (LInteger 1) i)
        | otherwise ->
          pure (Var (BN n) i)
    where
    cvar1 = "#constantlyA1"
    cvar2 = "#constantlyA2"
    ivar1 = "#identityA1"
  Lisp.Var n i -> pure (Var n i)
  Lisp.Block nel i -> do
    nel' <- traverse desugarLispTerm nel
    pure $ foldr (\a b -> Sequence a b i) (NE.last nel') (NE.init nel')
  Lisp.LetIn binders expr i -> do
    expr' <- desugarLispTerm expr
    foldrM (binderToLet i) expr' binders
  Lisp.Lam [] body i ->
    Nullary <$> desugarLispTerm body <*> pure i
  Lisp.Lam (x:xs) body i -> do
    let nsts = x :| xs
        args = (\(Lisp.MArg n t) -> Arg n t) <$> nsts
    body' <- desugarLispTerm body
    pure (Lam AnonLamInfo args body' i)
  Lisp.Suspend body i -> desugarLispTerm (Lisp.Lam [] body i)
  Lisp.Binding fs hs i -> do
    hs' <- traverse desugarLispTerm hs
    body <- bindingBody hs'
    let bodyLam b = Lam AnonLamInfo (pure (Arg objFreshText Nothing)) b i
    pure $ bodyLam $ foldr bindToLet body fs
      where
      bindingBody hs' = case reverse hs' of
        [] -> throwDesugarError EmptyBindingBody i
        x:xs -> pure $ foldl' (\acc e -> Sequence e acc i) x xs
      objFreshText = "#bindObject"
      objFreshVar = Var (BN (BareName objFreshText)) i
      bindToLet (Field field, marg) body =
        let arg = toArg marg
            fieldLit = Constant (LString field) i
            access = App (Builtin (liftRaw RawAt) i) [fieldLit, objFreshVar] i
        in Let arg access body i
  Lisp.If e1 e2 e3 i -> Conditional <$>
     (CIf <$> desugarLispTerm e1 <*> desugarLispTerm e2 <*> desugarLispTerm e3) <*> pure i
  Lisp.App (Lisp.Operator o _oi) [e1, e2] i -> case o of
    Lisp.AndOp ->
      Conditional <$> (CAnd <$> desugarLispTerm e1 <*> desugarLispTerm e2) <*> pure i
    Lisp.OrOp ->
      Conditional <$> (COr <$> desugarLispTerm e1 <*> desugarLispTerm e2) <*> pure i
    Lisp.EnforceOp ->
      Conditional <$> (CEnforce <$> desugarLispTerm e1 <*> desugarLispTerm e2) <*> pure i
    Lisp.EnforceOneOp -> case e2 of
      Lisp.List e _ ->
        Conditional <$> (CEnforceOne <$> desugarLispTerm e1 <*> traverse desugarLispTerm e) <*> pure i
      _ ->
        throwDesugarError (InvalidSyntax "enforce-one: expected argument list") i
  Lisp.App e hs i -> do
    e' <- desugarLispTerm e
    hs' <- traverse desugarLispTerm hs
    case e' of
      Builtin b _ -> pure (desugarAppArity i b hs')
      _ -> pure (App e' hs' i)
  Lisp.Operator bop i -> pure (desugarOperator i bop)
  -- Lisp.DynAccess e fn i ->
  --   DynInvoke <$> desugarLispTerm e <*> pure fn  <*> pure i
  Lisp.List e1 i ->
    ListLit <$> traverse desugarLispTerm e1 <*> pure i
  Lisp.Constant l i ->
    pure (Constant l i)
  Lisp.Try e1 e2 i ->
    Try <$> desugarLispTerm e1 <*> desugarLispTerm e2 <*> pure i
  Lisp.Error e i ->
    pure (Error e i)
  Lisp.Object fields i ->
    ObjectLit <$> (traverse._2) desugarLispTerm fields <*> pure i
  Lisp.CapabilityForm cf i -> (`CapabilityForm` i) <$> case cf of
    Lisp.WithCapability pn exs ex ->
      WithCapability pn <$> traverse desugarLispTerm exs <*> desugarLispTerm ex
    Lisp.CreateUserGuard pn exs ->
      CreateUserGuard pn <$> traverse desugarLispTerm exs
  where
  binderToLet i (Lisp.Binder n mty expr) term = do
    expr' <- desugarLispTerm expr
    pure $ Let (Arg n mty) expr' term i

suspendTerm
  :: Term ParsedName DesugarType builtin info
  -> Term ParsedName DesugarType builtin info
suspendTerm e' =
  Nullary e' (view termInfo e')

toArg
  :: Lisp.MArg
  -> Arg DesugarType
toArg (Lisp.MArg n mty) = Arg n mty

desugarDefun
  :: (MonadEval b i m, DesugarBuiltin b)
  => Lisp.Defun i
  -> RenamerT b i m (Defun ParsedName DesugarType b i)
desugarDefun (Lisp.Defun defname [] mrt body _ _ i) = do
  body' <- desugarLispTerm body
  view reCurrModule >>= \case
    Just _ -> do
      let bodyLam = Nullary body' i
      pure $ Defun defname [] mrt bodyLam i
    Nothing ->
      throwDesugarError (NotAllowedOutsideModule "defun") i
desugarDefun (Lisp.Defun defname (arg:args) mrt body _ _ i) = do
  let args' = toArg <$> (arg :| args)
  body' <- desugarLispTerm body
  view reCurrModule >>= \case
    Just (mn,_) -> do
      let bodyLam = Lam (TLDefun mn defname) args' body' i
      pure $ Defun defname (NE.toList args') mrt bodyLam i
    Nothing -> throwDesugarError (NotAllowedOutsideModule "defun") i

desugarDefPact
  :: (MonadEval b i m, DesugarBuiltin b)
  => Lisp.DefPact i
  -> RenamerT b i m (DefPact ParsedName DesugarType b i)
desugarDefPact (Lisp.DefPact dpname _ _ [] _ _ i) =
  throwDesugarError (EmptyDefPact dpname) i
desugarDefPact (Lisp.DefPact dpname margs rt (step:steps) _ _ i) =
  view reCurrModule >>= \case
    Just (mn,_) -> do
      let args' = toArg <$> margs
      steps' <- forM (step :| steps) \case
        Lisp.Step s _ ->
          Step <$> desugarLispTerm s
        Lisp.StepWithRollback s rb _ ->
          StepWithRollback
          <$> desugarLispTerm s
          <*> desugarLispTerm rb

      -- In DefPacts, last step is not allowed to rollback.
      when (hasRollback $ NE.last steps') $
        throwDesugarError (LastStepWithRollback (QualifiedName dpname mn)) i

      pure $ DefPact dpname args' rt steps' i
    Nothing -> throwDesugarError (NotAllowedOutsideModule "defpact") i

desugarDefConst
  :: (MonadEval b i m, DesugarBuiltin b)
  => Lisp.DefConst i
  -> RenamerT b i m (DefConst ParsedName DesugarType b i)
desugarDefConst (Lisp.DefConst n mty e _ i) = do
  e' <- desugarLispTerm e
  pure $ DefConst n mty (TermConst e') i

desugarDefMeta
  :: (MonadEval b i m)
  => i
  -> [Arg t]
  -> Lisp.DCapMeta
  -> RenamerT b i m (DefCapMeta ParsedName)
desugarDefMeta info args = \case
  Lisp.DefEvent -> pure DefEvent
  Lisp.DefManaged marg -> case marg of
    Just (arg, name) ->
        case findIndex ((==) arg . view argName) args of
          Just index' ->
            let dmanaged = DefManagedMeta index' (FQParsed name)
            in pure (DefManaged dmanaged)
          Nothing ->
            throwDesugarError (InvalidManagedArg arg) info
    Nothing -> pure (DefManaged AutoManagedMeta)

desugarDefCap
  :: (MonadEval b i m, DesugarBuiltin b)
  => Lisp.DefCap i
  -> RenamerT b i m (DefCap ParsedName DesugarType b i)
desugarDefCap (Lisp.DefCap dcn arglist rtype term _docs _model meta i) =
  view reCurrModule >>= \case
    Just _ -> do
      let arglist' = toArg <$> arglist
      term' <- desugarLispTerm term
      meta' <- maybe (pure Unmanaged) (desugarDefMeta i arglist') meta
      pure (DefCap dcn (length arglist) arglist' rtype term' meta' i)
    Nothing ->
      throwDesugarError (NotAllowedOutsideModule "defcap") i

desugarDefSchema
  :: (MonadEval b i m)
  => Lisp.DefSchema i
  -> RenamerT b i m (DefSchema DesugarType i)
desugarDefSchema (Lisp.DefSchema dsn args _docs _model i) = do
  let args' = (\(Lisp.Arg n ty) -> (Field n, ty)) <$> args
      scd = M.fromList args'
  pure $ DefSchema dsn scd i

desugarDefTable
  :: (MonadEval b i m)
  => Lisp.DefTable i
  -> RenamerT b i m  (DefTable ParsedName i)
desugarDefTable (Lisp.DefTable dtn dts _ i) =
  pure (DefTable dtn (DesugaredTable dts) i)

desugarIfDef
  :: (MonadEval b i m, DesugarBuiltin b)
  => Lisp.IfDef i
  -> RenamerT b i m  (IfDef ParsedName DesugarType b i)
desugarIfDef = \case
  Lisp.IfDfun (Lisp.IfDefun n margs rty _ _ i) -> IfDfun <$> case margs of
    [] -> do
      pure $ IfDefun n [] rty i
    _ -> do
      let args = toArg <$> margs
      rty' <- maybe (throwDesugarError (UnannotatedReturnType n) i) pure rty
      pure $ IfDefun n args (Just rty') i
  -- Todo: check managed impl
  Lisp.IfDCap (Lisp.IfDefCap n margs rty _ _ _meta i) -> IfDCap <$> do
    let args = toArg <$> margs
    pure $ IfDefCap n args rty i
  Lisp.IfDConst dc -> IfDConst <$> desugarDefConst dc
  Lisp.IfDPact (Lisp.IfDefPact n margs rty _ _ i) -> IfDPact <$> case margs of
    [] -> do
      pure $ IfDefPact n [] rty i
    _ -> do
      let args = toArg <$> margs
      rty' <- maybe (throwDesugarError (UnannotatedReturnType n) i) pure rty
      pure $ IfDefPact n args (Just rty') i
  Lisp.IfDSchema ds -> IfDSchema <$> desugarDefSchema ds

desugarDef
  :: (MonadEval b i m, DesugarBuiltin b)
  => Lisp.Def i
  -> RenamerT b i m (Def ParsedName DesugarType b i)
desugarDef = \case
  Lisp.Dfun d -> Dfun <$> desugarDefun d
  Lisp.DConst d -> DConst <$> desugarDefConst d
  Lisp.DCap dc -> DCap <$> desugarDefCap dc
  Lisp.DSchema d -> DSchema <$> desugarDefSchema d
  Lisp.DTable d -> DTable <$> desugarDefTable d
  Lisp.DPact d -> DPact <$> desugarDefPact d

desugarModule
  :: (MonadEval b i m, DesugarBuiltin b)
  => Lisp.Module i
  -> RenamerT b i m (Module ParsedName DesugarType b i)
desugarModule (Lisp.Module mname mgov extdecls defs _ _ i) = do
  (imports, blessed, implemented) <- splitExts extdecls
  defs' <- locally reCurrModule (const (Just (mname,[]))) $ traverse desugarDef (NE.toList defs)
  pure $ Module mname mgov defs' blessed imports implemented placeholderHash i
  where
  splitExts = split ([], S.empty, [])
  split (accI, accB, accImp) (h:hs) = case h of
    Lisp.ExtBless b -> case parseModuleHash b of
      Nothing -> throwDesugarError (InvalidBlessedHash b) i
      Just mh -> split (accI, S.insert mh accB, accImp) hs
    Lisp.ExtImport imp -> do
      imp' <- desugarImport i imp
      split (imp':accI, accB, accImp) hs
    Lisp.ExtImplements mn -> split (accI, accB, mn:accImp) hs
  split (a, b, c) [] = pure (reverse a, b, reverse c)

desugarImport :: MonadEval b i m => i -> Lisp.Import -> RenamerT b i m Import
desugarImport info (Lisp.Import mn (Just blessed) imported) = case parseModuleHash blessed of
  Just mbh' -> pure (Import mn (Just mbh') imported)
  Nothing -> throwDesugarError (InvalidBlessedHash blessed) info
desugarImport _ (Lisp.Import mn Nothing imported) = pure (Import mn Nothing imported)

-- Todo: Interface hashing, either on source or
-- the contents
desugarInterface
  :: (MonadEval b i m, DesugarBuiltin b)
  => Lisp.Interface i
  -> RenamerT b i m (Interface ParsedName DesugarType b i)
desugarInterface (Lisp.Interface ifn ifdefns imps _ _ info) = do
  defs' <- traverse desugarIfDef ifdefns
  let mhash = ModuleHash (Hash "placeholder")
  imps' <- traverse (desugarImport info) imps
  pure $ Interface ifn defs' imps' mhash info

desugarUse
  :: (MonadEval b i m )
  => i
  -> Lisp.Import
  -> RenamerT b i m Import
desugarUse i imp = do
  imp' <- desugarImport i imp
  imp' <$ handleImport i mempty imp'

-----------------------------------------------------------
-- Renaming
-----------------------------------------------------------

-- Strongly connected components in term
termSCC
  :: ModuleName
  -- ^ The current module
  -> Set Text
  -- ^ The current set of definitions, we thread this through
  -- because lambdas may shadow.
  -- Note: this is a decent place to emit shadowing warnings.
  -> Term ParsedName DesugarType b1 i1
  -> Set Text
termSCC currM currDefns = \case
  -- todo: factor out this patmat on `ParsedName`,
  -- we use it multiple times
  Var n _ -> case n of
    BN bn | S.member (_bnName bn) currDefns -> S.singleton (_bnName bn)
          | otherwise -> mempty
    QN (QualifiedName n' mn')
      | S.member n' currDefns && mn' == currM -> S.singleton n'
      | otherwise -> mempty
    DN _ -> mempty
  Lam _ args e _ ->
    let currDefns' = foldl' (\s t -> S.delete (_argName t) s) currDefns args
        tySCC = foldMap (argSCC currM currDefns) args
    in tySCC <> termSCC currM currDefns' e
  Let arg e1 e2 _ ->
    let currDefns' = S.delete (_argName arg) currDefns
        tySCC = argSCC currM currDefns arg
    in tySCC <> termSCC currM currDefns e1 <> termSCC currM currDefns' e2
  App fn apps _ ->
    S.union (termSCC currM currDefns fn) (foldMap (termSCC currM currDefns) apps)
  Sequence e1 e2 _ -> S.union (termSCC currM currDefns e1) (termSCC currM currDefns e2)
  Conditional c _ ->
    foldMap (termSCC currM currDefns) c
  Builtin{} -> S.empty
  Constant{} -> S.empty
  ListLit v _ -> foldMap (termSCC currM currDefns) v
  Try e1 e2 _ -> S.union (termSCC currM currDefns e1) (termSCC currM currDefns e2)
  Nullary e _ -> termSCC currM currDefns e
  CapabilityForm cf _ -> foldMap (termSCC currM currDefns) cf <> case view capFormName cf of
    BN n | S.member (_bnName n) currDefns -> S.singleton (_bnName n)
          | otherwise -> mempty
    QN (QualifiedName n' mn')
      | S.member n' currDefns && mn' == currM -> S.singleton n'
      | otherwise -> S.singleton n'
    DN _ -> mempty
  -- DynInvoke m _ _ -> termSCC currM currDefns m
  ObjectLit m _ -> foldMap (termSCC currM currDefns . view _2) m
  Error {} -> S.empty

parsedNameSCC :: ModuleName -> Set Text -> ParsedName -> Set Text
parsedNameSCC currM currDefns n = case n of
  BN bn | S.member (_bnName bn) currDefns -> S.singleton (_bnName bn)
        | otherwise -> mempty
  QN (QualifiedName n' mn')
    | S.member n' currDefns && mn' == currM -> S.singleton n'
    | otherwise -> mempty
  DN _ -> mempty

typeSCC
  :: ModuleName
  -> Set Text
  -> DesugarType
  -> Set Text
typeSCC currM currDefs = \case
  Lisp.TyPrim _ -> mempty
  Lisp.TyList l -> typeSCC currM currDefs l
  Lisp.TyModRef _ -> mempty
  Lisp.TyObject pn -> case pn of
    -- Todo: factor out, repeated in termSCC
    TBN bn | S.member (_bnName bn) currDefs -> S.singleton (_bnName bn)
          | otherwise -> mempty
    TQN (QualifiedName n' mn')
      | S.member n' currDefs && mn' == currM -> S.singleton n'
      | otherwise -> mempty
  Lisp.TyKeyset -> mempty
  Lisp.TyPolyList -> mempty
  Lisp.TyPolyObject -> mempty
  Lisp.TyTable pn ->  case pn of
    -- Todo: factor out, repeated in termSCC
    TBN bn | S.member (_bnName bn) currDefs -> S.singleton (_bnName bn)
          | otherwise -> mempty
    TQN (QualifiedName n' mn')
      | S.member n' currDefs && mn' == currM -> S.singleton n'
      | otherwise -> mempty

argSCC :: ModuleName -> Set Text -> Arg DesugarType -> Set Text
argSCC currM currDefs (Arg _ ty) = case ty of
  Just t -> typeSCC currM currDefs t
  Nothing -> mempty

defunSCC
  :: ModuleName
  -> Set Text
  -> Defun ParsedName DesugarType  b i
  -> Set Text
defunSCC mn cd df =
  let tscc = termSCC mn cd (_dfunTerm df)
      argScc = foldMap (argSCC mn cd) (_dfunArgs df)
  in tscc <> argScc <> maybe mempty (typeSCC mn cd) (_dfunRType df)

defConstSCC
  :: ModuleName
  -> Set Text
  -> DefConst ParsedName DesugarType  b i
  -> Set Text
defConstSCC mn cd dc =
  let tscc = foldMap (termSCC mn cd) (_dcTerm dc)
      tyscc =  maybe mempty (typeSCC mn cd) (_dcType dc)
  in tscc <> tyscc

defTableSCC
  :: ModuleName
  -> Set Text
  -> DefTable ParsedName info
  -> Set Text
defTableSCC mn cd dt =
  let (DesugaredTable t) =  (_dtSchema dt)
  in parsedNameSCC mn cd t

defCapSCC
  :: ModuleName
  -> Set Text
  -> DefCap ParsedName DesugarType b i1
  -> Set Text
defCapSCC mn cd dc =
  let argsScc = foldMap (argSCC mn cd) (_dcapArgs dc)
      rtypeScc = maybe mempty (typeSCC mn cd) (_dcapRType dc)
  in argsScc <> rtypeScc <> case _dcapMeta dc of
    DefManaged (DefManagedMeta _ (FQParsed pn)) ->
      termSCC mn cd (_dcapTerm dc) <> parsedNameSCC mn cd pn
    _ -> termSCC mn cd (_dcapTerm dc)


defPactSCC
  :: ModuleName
  -> Set Text
  -> DefPact ParsedName DesugarType b i
  -> Set Text
defPactSCC mn cd dp =
  let argsScc = foldMap (argSCC mn cd) (_dpArgs dp)
      rtScc = maybe mempty (typeSCC mn cd) (_dpRetType dp)
      stepsScc = foldMap (defPactStepSCC mn cd) (_dpSteps dp)
  in argsScc <> rtScc <> stepsScc

defPactStepSCC
  :: ModuleName
  -> Set Text
  -> Step ParsedName DesugarType b i
  -> Set Text
defPactStepSCC mn cd = \case
  Step step -> termSCC mn cd step
  StepWithRollback step rollback ->
    S.unions $ [termSCC mn cd step, termSCC mn cd rollback]

defSCC
  :: ModuleName
  -> Set Text
  -> Def ParsedName DesugarType b i1
  -> Set Text
defSCC mn cd = \case
  Dfun d -> defunSCC mn cd d
  DConst d -> defConstSCC mn cd d
  DCap dc -> defCapSCC mn cd dc
  DSchema ds -> foldMap (typeSCC mn cd) ( _dsSchema ds)
  DPact dp -> defPactSCC mn cd dp
  DTable dt -> defTableSCC mn cd dt

ifDefSCC
  :: ModuleName
  -> Set Text
  -> IfDef ParsedName DesugarType b i1
  -> Set Text
ifDefSCC mn currDefs = \case
  IfDfun _ -> mempty
  IfDCap _ -> mempty
  IfDConst d -> defConstSCC mn currDefs d
  IfDPact _ -> mempty
  IfDSchema ds -> foldMap (typeSCC mn currDefs) ( _dsSchema ds)

-- Todo: this handles imports, rename?
loadTopLevelMembers
  :: (MonadEval b i m)
  => i
  -> Maybe (Set Text)
  -> ModuleData b i
  -> Map Text (NameKind, Maybe DefKind)
  -> RenamerT b i m (Map Text (NameKind, Maybe DefKind))
loadTopLevelMembers i mimports mdata binds = case mdata of
  ModuleData md _ -> do
    let modName = _mName md
        mhash = _mHash md
    let depMap = M.fromList $ toLocalDepMap modName mhash <$> _mDefs md
        loadedDeps = M.fromList $ toLoadedDepMap modName mhash <$> _mDefs md
    loadWithImports depMap loadedDeps
  InterfaceData iface _ -> do
    let ifname = _ifName iface
    let ifhash = _ifHash iface
        dcDeps = mapMaybe ifDefToDef (_ifDefns iface)
        depMap = M.fromList $ toLocalDepMap ifname ifhash <$> dcDeps
        loadedDeps = M.fromList $ toLoadedDepMap ifname ifhash <$> dcDeps
    loadWithImports depMap loadedDeps
  where
  toLocalDepMap modName mhash defn = (defName defn, (NTopLevel modName mhash, Just (defKind defn)))
  toLoadedDepMap modName mhash defn = (defName defn, (FullyQualifiedName modName (defName defn) mhash, defKind defn))
  loadWithImports depMap loadedDeps = case mimports of
      Just st -> do
        let depsKeys = M.keysSet depMap
        unless (S.isSubsetOf st depsKeys) $ throwDesugarError (InvalidImports (S.toList (S.difference st depsKeys))) i
        (esLoaded . loToplevel) %== (`M.union` (M.restrictKeys loadedDeps st))
        pure (M.union (M.restrictKeys depMap st) binds)
      Nothing -> do
        (esLoaded . loToplevel) %== (`M.union` loadedDeps)
        pure (M.union depMap binds)

-- | Resolve a module name, return the implemented members as well if any
-- including all current
resolveModuleName
  :: (MonadEval b i m)
  => i
  -> ModuleName
  -> RenamerT b i m (ModuleName, [ModuleName])
resolveModuleName i mn =
  view reCurrModule >>= \case
    Just (currMod, imps) | currMod == mn -> pure (currMod, imps)
    _ -> resolveModuleData mn i >>= \case
      ModuleData md _ -> do
        let implementeds = view mImplements md
        pure (mn, implementeds)
      -- todo: error type here
      InterfaceData iface _ ->
        throwDesugarError (InvalidModuleReference (_ifName iface)) i

-- | Resolve a module name, return the implemented members as well if any
-- including all current
resolveInterfaceName :: (MonadEval b i m) => i -> ModuleName -> RenamerT b i m (ModuleName)
resolveInterfaceName i mn =
  view reCurrModule >>= \case
    Just (currMod, _imps) | currMod == mn -> pure currMod
    _ -> resolveModuleData mn i >>= \case
      ModuleData _ _ ->
        throwDesugarError (InvalidModuleReference mn) i
      -- todo: error type here
      InterfaceData _ _ ->
        pure mn


-- | Resolve module data, fail if not found
resolveModuleData
  :: (MonadEval b i m)
  => ModuleName
  -> i
  -> RenamerT b i m (ModuleData b i)
resolveModuleData mn@(ModuleName name mNs) i = do
  pdb <- viewEvalEnv eePactDb
  lift (lookupModuleData i pdb mn) >>= \case
    Just md -> pure md
    Nothing -> case mNs of
      Just _ -> throwDesugarError (NoSuchModule mn) i
      Nothing -> useEvalState (esLoaded . loNamespace) >>= \case
        Nothing -> throwDesugarError (NoSuchModule mn) i
        Just (Namespace ns _ _) ->
          lift (getModuleData i pdb (ModuleName name (Just ns)))

renameType
  :: (MonadEval b i m, DesugarBuiltin b)
  => i
  -> DesugarType
  -> RenamerT b i m Type
renameType i = \case
  Lisp.TyPrim p -> pure (TyPrim p)
  Lisp.TyList ty ->
    TyList <$> renameType i ty
  Lisp.TyModRef tmr ->
    TyModRef (S.fromList tmr) <$ traverse (resolveInterfaceName i) tmr
  Lisp.TyKeyset -> pure TyGuard
  Lisp.TyObject pn ->
    TyObject <$> resolveSchema pn
  Lisp.TyTable pn ->
    TyTable <$> resolveSchema pn
  Lisp.TyPolyList -> pure TyAnyList
  Lisp.TyPolyObject -> pure TyAnyObject
  where
  resolveSchema = \case
    TBN bn -> do
      (_, dkty) <- resolveBare bn i
      case dkty of
        Just (DKDefSchema sc) -> pure sc
        Just _ -> throwDesugarError (InvalidDefInSchemaPosition (_bnName bn)) i
        Nothing ->
          throwDesugarError (UnboundTypeVariable (_bnName bn)) i
    TQN qn -> do
      (_, dt) <- resolveQualified qn i
      case dt of
        Just (DKDefSchema sc) -> pure sc
        _ -> throwDesugarError (InvalidDefInSchemaPosition (_qnName qn)) i


-- Rename a term (that is part of a module)
-- emitting the list of dependent calls
renameTerm
  :: (MonadEval b i m, DesugarBuiltin b)
  => Term ParsedName DesugarType b i
  -> RenamerT b i m (Term Name Type b i)
renameTerm (Var n i) = resolveName i n >>= \case
  (n', Just dk)
    | dk `elem` legalVarDefs  -> pure (Var n' i)
    | otherwise ->
      throwDesugarError (InvalidDefInTermVariable (rawParsedName n)) i
    where
    legalVarDefs = [DKDefun, DKDefConst, DKDefTable, DKDefCap, DKDefPact]
  (n', _) -> pure (Var n' i)
-- Todo: what happens when an argument is shadowed?
renameTerm (Lam li nsts body i) = do
  depth <- view reVarDepth
  let len = fromIntegral (NE.length nsts)
      newDepth = depth + len
      ixs = NE.fromList [depth .. newDepth - 1]
  let m = M.fromList $ NE.toList $ NE.zip (_argName <$> nsts) ((,Nothing). NBound <$> ixs)
  term' <- local (inEnv m newDepth) (renameTerm body)
  nsts' <- (traversed . argType . _Just) (renameType i) nsts
  pure (Lam li nsts' term' i)
  where
  inEnv m newDepth =
    over reBinds (M.union m) .
    set reVarDepth newDepth
renameTerm (Let arg e1 e2 i) = do
  depth <- view reVarDepth
  arg' <- traverse (renameType i) arg
  let inEnv = over reVarDepth succ .
              over reBinds (M.insert (_argName arg) (NBound depth, Nothing))
  e1' <- renameTerm e1
  e2' <- local inEnv (renameTerm e2)
  pure (Let arg' e1' e2' i)
renameTerm (App fn apps i) = do
  fn' <- renameTerm fn
  apps' <- traverse renameTerm apps
  pure (App fn' apps' i)
renameTerm (Nullary term i) =
  Nullary <$> renameTerm term <*> pure i
renameTerm (Sequence e1 e2 i) = do
  Sequence <$> renameTerm e1 <*> renameTerm e2 <*> pure i
renameTerm (Conditional c i) =
  Conditional <$> traverse renameTerm c <*> pure i
renameTerm (Builtin b i) =
  pure (Builtin b i)
renameTerm (Constant l i) =
  pure (Constant l i)
renameTerm (ListLit v i) = do
  ListLit <$> traverse renameTerm v <*> pure i
-- renameTerm (DynInvoke te t i) =
--   DynInvoke <$> renameTerm te <*> pure t <*> pure i
renameTerm (Try e1 e2 i) = do
  Try <$> renameTerm e1 <*> renameTerm e2 <*> pure i
renameTerm (CapabilityForm cf i) =
  view reCurrModule >>= \case
    Just _ -> case view capFormName cf of
      QN qn -> do
          (n', dk) <- resolveQualified qn i
          when ((isCapForm cf && dk /= Just DKDefCap) || (not (isCapForm cf) && dk == Just DKDefun))
            $ throwDesugarError (InvalidCapabilityReference (_qnName qn)) i
          let cf' = set capFormName n' cf
          checkCapForm cf'
          CapabilityForm <$> traverse renameTerm cf' <*> pure i
          -- throwDesugarError (CapabilityOutOfScope (_qnName qn) (_qnModName qn)) i
      BN bn -> do
        (n', dk) <- resolveBare bn i
        when (isJust dk && not (dk == Just DKDefCap) && isCapForm cf)
          $ throwDesugarError (InvalidCapabilityReference (_bnName bn)) i
        let cf' = set capFormName n' cf
        checkCapForm cf'
        CapabilityForm <$> traverse renameTerm cf' <*> pure i
      DN dn -> do
        n' <- resolveDynamic i dn
        let cf' = set capFormName n' cf
        CapabilityForm <$> traverse renameTerm cf' <*> pure i
    Nothing -> do
      checkCapFormNonModule cf
      let n = view capFormName cf
      (n', _) <- resolveName i n
      let cf' = set capFormName n' cf
      CapabilityForm <$> traverse renameTerm cf' <*> pure i
    where
    isCapForm = \case
      CreateUserGuard{} -> False
      _ -> True

    checkCapFormNonModule = const (pure ())
      -- WithCapability{} ->
      --   throwDesugarError (NotAllowedOutsideModule "with-capability") i
      -- CreateUserGuard{} -> pure ()

    checkCapForm = \case
      WithCapability{} -> enforceNotWithinDefcap i "with-capability"
      _ -> pure ()
renameTerm (Error e i) = pure (Error e i)
renameTerm (ObjectLit o i) =
  ObjectLit <$> (traverse._2) renameTerm o <*> pure i

enforceNotWithinDefcap
  :: (MonadEval b i m)
  => i
  -> Text
  -> RenamerT b i m ()
enforceNotWithinDefcap i form = do
  withinDefCap <- (== Just DKDefCap) <$> view reCurrDef
  when withinDefCap $ throwDesugarError (NotAllowedWithinDefcap form) i

renameDefun
  :: (MonadEval b i m, DesugarBuiltin b)
  => Defun ParsedName DesugarType b i
  -> RenamerT b i m (Defun Name Type b i)
renameDefun (Defun n args ret term i) = do
  -- Todo: put type variables in scope here, if we want to support polymorphism
  args' <- (traverse.traverse) (renameType i) args
  ret' <- traverse (renameType i) ret
  term' <- local (set reCurrDef (Just DKDefun)) $ renameTerm term
  pure (Defun n args' ret' term' i)

renamePactStep
  :: (MonadEval b i m, DesugarBuiltin b)
  => Step ParsedName DesugarType b i
  -> RenamerT b i m (Step Name Type b i)
renamePactStep = \case
  Step step ->
    Step <$> renameTerm step
  StepWithRollback step rollback ->
    StepWithRollback <$> renameTerm step <*> renameTerm rollback

renameDefPact
  :: (MonadEval b i m, DesugarBuiltin b)
  => DefPact ParsedName DesugarType b i
  -> RenamerT b i m (DefPact Name Type b i)
renameDefPact (DefPact n argtys mret steps i) = do
  args' <- (traverse.traverse) (renameType i) argtys
  mret' <- traverse (renameType i) mret
  steps' <- local (set reCurrDef (Just DKDefPact) . bindArgs) $
    traverse renamePactStep steps
  pure (DefPact n args' mret' steps' i)
  where
  -- Todo: duplication, factor out
  bindArgs rEnv
      | null argtys = rEnv
      | otherwise = let
        depth = view reVarDepth rEnv
        len = fromIntegral (length argtys)
        newDepth = depth + len
        ixs = [depth .. newDepth - 1]
        m = M.fromList $ zip (_argName <$> argtys) ((, Nothing) . NBound <$> ixs)
        in over reBinds (M.union m) $ set reVarDepth newDepth rEnv


renameDefSchema
  :: (MonadEval b i m, DesugarBuiltin b)
  => DefSchema DesugarType i
  -> RenamerT b i m (DefSchema Type i)
renameDefSchema (DefSchema dsn dsc i) = do
  dsc' <- traverse (renameType i) dsc
  pure (DefSchema dsn dsc' i)

renameDefTable
  :: (MonadEval b i m)
  => DefTable ParsedName i
  -> RenamerT b i m (DefTable Name i)
renameDefTable (DefTable dtn sc i) = do
  case sc of
    DesugaredTable dn -> resolveName i dn >>= \case
      (_, Just (DKDefSchema rsc)) -> pure (DefTable dtn (ResolvedTable rsc) i)
      (n, Just _) ->
        throwDesugarError (InvalidDefInSchemaPosition (_nName n)) i
      (_n, Nothing) -> throwDesugarError (UnboundTypeVariable (rawParsedName dn)) i

renameReplDefun
  :: (MonadEval b i m, DesugarBuiltin b)
  => Defun ParsedName DesugarType b i
  -> RenamerT b i m (Defun Name Type b i)
renameReplDefun (Defun n args ret term i) = do
  let fqn = FullyQualifiedName replModuleName n replModuleHash
  args' <- (traverse.traverse) (renameType i) args
  ret' <- traverse (renameType i) ret
  esLoaded . loToplevel %== M.insert n (fqn, DKDefun)
  term' <- local (set reCurrDef (Just DKDefun)) $ renameTerm term
  pure (Defun n args' ret' term' i)

renameReplDefConst
  :: (MonadEval b i m, DesugarBuiltin b)
  => DefConst ParsedName DesugarType b i
  -> RenamerT b i m (DefConst Name Type b i)
renameReplDefConst (DefConst n mty term i) = do
  let fqn = FullyQualifiedName replModuleName n replModuleHash
  esLoaded . loToplevel %== M.insert n (fqn, DKDefConst)
  mty' <- traverse (renameType i) mty
  term' <- local (set reCurrDef (Just DKDefConst)) $ traverse renameTerm term
  pure (DefConst n mty' term' i)

renameDefConst
  :: (MonadEval b i m, DesugarBuiltin b)
  => DefConst ParsedName DesugarType b i
  -> RenamerT b i m (DefConst Name Type b i)
renameDefConst (DefConst n mty term i) = do
  mty' <- traverse (renameType i) mty
  term' <- local (set reCurrDef (Just DKDefConst)) $ traverse renameTerm term
  pure (DefConst n mty' term' i)

renameDefCap
  :: (MonadEval b i m, DesugarBuiltin b)
  => DefCap ParsedName DesugarType b i
  -> RenamerT b i m (DefCap Name Type b i)
renameDefCap (DefCap name arity argtys rtype term meta info) = do
  meta' <- resolveMeta info meta
  argtys' <- (traverse.traverse) (renameType info) argtys
  rtype' <- traverse (renameType info) rtype
  term' <- local (set reCurrDef (Just DKDefCap) .  bindArgs) $ renameTerm term
  pure (DefCap name arity argtys' rtype' term' meta' info)
  where
  -- Todo: debruijn code should be isolated
  bindArgs rEnv
    | null argtys = rEnv
    | otherwise = let
      depth = view reVarDepth rEnv
      len = fromIntegral (length argtys)
      newDepth = depth + len
      ixs = [depth .. newDepth - 1]
      m = M.fromList $ zip (_argName <$> argtys) ((, Nothing) . NBound <$> ixs)
      in over reBinds (M.union m) $ set reVarDepth newDepth rEnv

resolveMeta
  :: MonadEval b i m
  => i
  -> DefCapMeta ParsedName
  -> RenamerT b i m (DefCapMeta Name)
resolveMeta _ DefEvent = pure DefEvent
resolveMeta _ Unmanaged = pure Unmanaged
resolveMeta _ (DefManaged AutoManagedMeta) = pure (DefManaged AutoManagedMeta)
resolveMeta info (DefManaged (DefManagedMeta i (FQParsed pn))) = do
  (name', _) <- resolveName info pn
  fqn <- expectedFree info name'
  pure (DefManaged (DefManagedMeta i (FQName fqn)))

expectedFree
  :: (MonadEval b i m)
  => i
  -> Name
  -> RenamerT b i m FullyQualifiedName
expectedFree i (Name n nk) = case nk of
  NTopLevel mname mh ->
    pure (FullyQualifiedName mname n mh)
  _ -> throwDesugarError (ExpectedFreeVariable n) i


renameDef
  :: (MonadEval b i m, DesugarBuiltin b)
  => Def ParsedName DesugarType b i
  -> RenamerT b i m (Def Name Type b i)
renameDef = \case
  Dfun d -> Dfun <$> renameDefun d
  DConst d -> DConst <$> renameDefConst d
  DCap d -> DCap <$> renameDefCap d
  DSchema d -> DSchema <$> renameDefSchema d
  DTable d -> DTable <$> renameDefTable d
  DPact d -> DPact <$> renameDefPact d

renameIfDef
  :: (MonadEval b i m, DesugarBuiltin b)
  => IfDef ParsedName DesugarType b i
  -> RenamerT b i m (IfDef Name Type b i)
renameIfDef = \case
  -- Todo: export and use lenses lol
  IfDfun d -> do
    let i = _ifdInfo d
    args' <- (traverse.traverse) (renameType i) (_ifdArgs d)
    rtype' <- traverse (renameType i) (_ifdRType d)
    pure (IfDfun (d{_ifdArgs = args', _ifdRType = rtype'}))
  IfDConst d -> IfDConst <$> renameDefConst d
  IfDSchema d -> IfDSchema <$> renameDefSchema d
  IfDCap d -> do
    let i = _ifdcInfo d
    args' <- (traverse.traverse) (renameType i) (_ifdcArgs d)
    rtype' <- traverse (renameType i) (_ifdcRType d)
    pure (IfDCap (d{_ifdcArgs = args', _ifdcRType = rtype'}))
  IfDPact d -> do
    let i = _ifdpInfo d
    args' <- (traverse.traverse) (renameType i) (_ifdpArgs d)
    rtype' <- traverse (renameType i) (_ifdpRType d)
    pure (IfDPact (d{_ifdpArgs = args', _ifdpRType = rtype'}))

resolveName
  :: (MonadEval b i m)
  => i
  -> ParsedName
  -> RenamerT b i m (Name, Maybe DefKind)
resolveName i = \case
  BN b -> resolveBare b i
  QN q -> resolveQualified q i
  DN dn -> (, Nothing) <$> resolveDynamic i dn

resolveDynamic
  :: (MonadEval b i m)
  => i
  -> DynamicName
  -> RenamerT b i m (Name)
resolveDynamic i (DynamicName dn dArg) = views reBinds (M.lookup dn) >>= \case
  Just tnk -> case tnk of
    (NBound d, _) -> do
      depth <- view reVarDepth
      let dbjIx = depth - d - 1
          dr = NDynRef (DynamicRef dArg dbjIx)
      pure (Name dn dr)
    _ ->
      throwDesugarError (InvalidDynamicInvoke dn) i
  Nothing ->
    throwDesugarError (UnboundTermVariable dn) i

-- | Resolve bare name atoms
-- which either correspond to:
--  - A top-level name in scope (e.g a definition)
--  - A module reference (we query bare module names first)
--  - A module reference with
resolveBare
  :: (MonadEval b i m)
  => BareName
  -> i
  -> RenamerT b i m (Name, Maybe DefKind)
resolveBare (BareName bn) i = views reBinds (M.lookup bn) >>= \case
  Just tnk -> case tnk of
    (NBound d, _) -> do
      depth <- view reVarDepth
      pure (Name bn (NBound (depth - d - 1)), Nothing)
    (nk, dk) -> pure (Name bn nk, dk)
  Nothing -> usesEvalState (esLoaded . loToplevel) (M.lookup bn) >>= \case
    Just (fqn, dk) -> pure (Name bn (NTopLevel (_fqModule fqn) (_fqHash fqn)), Just dk)
    Nothing -> do
      let mn = ModuleName bn Nothing
      view reCurrModule >>= \case
        Just (currMod, imps) | currMod == mn ->
          pure (Name bn (NModRef mn imps), Nothing)
        _ -> do
          (mn', imps) <- resolveModuleName i mn
          pure (Name bn (NModRef mn' imps), Nothing)

-- | Resolve a qualified name `<qual>.<name>` with the following
-- procedure:
--  - <qual> has the form `<namespace>.<module>`:
--    - if we find no identifier loaded or in the database, then it must not exist.
--      it definitely refers to a fully qualified member
--  - <qual> is of the form `<module>` (so, no namespace):
--    - The identifier can mean one of three things: a root-namespaced identifier (e.g `coin.transfer`),
--      a namespaced identifier when a namespace is in scope (so m.f when `(namespace 'free)` has been called),
--      or a module reference where `<name>` is actually the module name, and `<module>` was actually the namespace.
--
resolveQualified
  :: (MonadEval b i m)
  => QualifiedName
  -> i
  -> RenamerT b i m (Name, Maybe DefKind)
resolveQualified (QualifiedName qn qmn@(ModuleName modName mns)) i = do
  pdb <- viewEvalEnv eePactDb
  runMaybeT (baseLookup pdb qn qmn <|> modRefLookup pdb <|> namespacedLookup pdb) >>= \case
    Just p -> pure p
    Nothing -> throwDesugarError (NoSuchModuleMember qmn qn) i
  where
  baseLookup pdb defnName moduleName = do
    MaybeT (lift (lookupModuleData i pdb moduleName)) >>= \case
      ModuleData module' _ -> do
        d <- hoistMaybe (findDefInModule defnName module' )
        lift $ rsDependencies %= S.insert moduleName
        pure (Name qn (NTopLevel moduleName (_mHash module')), Just (defKind d))
      InterfaceData iface _ -> do
        d <- hoistMaybe (findDefInInterface defnName iface)
        lift $ rsDependencies %= S.insert moduleName
        pure (Name qn (NTopLevel moduleName (_ifHash iface)), Just (defKind d))
  modRefLookup pdb = case mns of
    -- Fail eagerly: the previous lookup was fully qualified
    Just _ -> MaybeT (throwDesugarError (NoSuchModuleMember qmn qn) i)
    Nothing -> do
      let mn' = ModuleName qn (Just (NamespaceName modName))
      m <- MaybeT $ lift $ lookupModule i pdb mn'
      let nk = NModRef mn' (_mImplements m)
      pure (Name qn nk, Nothing)
  namespacedLookup pdb = do
    Namespace ns _ _ <- MaybeT (useEvalState (esLoaded . loNamespace))
    let mn' = ModuleName modName (Just ns)
    baseLookup pdb qn mn'

-- | Handle all name resolution for modules
renameModule
  :: (MonadEval b i m, DesugarBuiltin b)
  => Module ParsedName DesugarType b i
  -> RenamerT b i m (Module Name Type b i)
renameModule (Module unmangled mgov defs blessed imports implements mhash i) = do
  rsDependencies .= mempty
  mname <- mangleNamespace unmangled
  mgov' <- resolveGov mname mgov
  let defNames = S.fromList $ fmap defName defs
  let scc = mkScc mname defNames <$> defs
  defs' <- forM (stronglyConnComp scc) \case
    AcyclicSCC d -> pure d
    CyclicSCC d ->
      -- todo: just in case, match on `d` because it makes no sense for there to be an empty cycle
      -- but all uses of `head` are still scary
      throwDesugarError (RecursionDetected mname (defName <$> d)) (defInfo (head d))
  binds <- view reBinds
  bindsWithImports <- foldlM (handleImport i) binds imports
  (defs'', _, _) <- over _1 reverse <$> foldlM (go mname) ([], S.empty, bindsWithImports) defs'
  traverse_ (checkImplements i defs'' mname) implements
  pure (Module mname mgov' defs'' blessed imports implements mhash i)
  where
  -- Our deps are acyclic, so we resolve all names
  go mname (defns, s, m) defn = do
    when (S.member (defName defn) s) $ throwDesugarError (DuplicateDefinition (defName defn)) i
    let dn = defName defn
    defn' <- local (set reCurrModule (Just (mname, implements)))
             $ local (set reBinds m) $ renameDef defn
    let dk = defKind defn'
    let depPair = (NTopLevel mname mhash, dk)
    let m' = M.insert dn (over _2 Just depPair) m
    pure (defn':defns, S.insert (defName defn) s, m')

  resolveGov mname = \case
    KeyGov rawKsn -> case parseAnyKeysetName (_keysetName rawKsn) of
      Left {} -> lift $ throwExecutionError i (ModuleGovernanceFailure mname)
      Right ksn -> do
        lift $ enforceKeysetNameAdmin i mname ksn
        pure (KeyGov ksn)
    CapGov (UnresolvedGov govName) ->
      case find (\d -> BN (BareName (defName d)) == govName) defs of
        Just (DCap d) -> do
          let fqn = FullyQualifiedName mname (_dcapName d) mhash
          pure (CapGov (ResolvedGov fqn))
        Just d -> throwDesugarError (InvalidGovernanceRef (QualifiedName (defName d) mname)) i
        Nothing -> throwDesugarError (InvalidGovernanceRef (QualifiedName (rawParsedName govName) mname)) i
  mkScc mname dns def = (def, defName def, S.toList (defSCC mname dns def))


handleImport
  :: (MonadEval b i m)
  => i
  -> Map Text (NameKind, Maybe DefKind)
  -> Import
  -> RenamerT b i m (Map Text (NameKind, Maybe DefKind))
handleImport info binds (Import mn mh imported) = do
  mdata <- resolveModuleData mn info
  let imported' = S.fromList <$> imported
      mdhash = view mdModuleHash mdata
  case mh of
    Just modHash -> when (modHash /= mdhash) $ throwDesugarError (InvalidImportModuleHash mn modHash) info
    Nothing -> pure ()
  loadTopLevelMembers info imported' mdata binds

checkImplements
  :: (MonadEval b i m)
  => i
  -> [Def Name Type b i]
  -> ModuleName
  -> ModuleName
  -> RenamerT b i m ()
checkImplements i defs moduleName ifaceName = do
  resolveModuleData ifaceName i >>= \case
    InterfaceData iface _deps ->
      traverse_ checkImplementedMember (_ifDefns iface)
    _ -> throwDesugarError (NoSuchInterface ifaceName) i
  where
  checkImplementedMember = \case
    IfDConst{} -> pure ()
    IfDSchema{} -> pure ()
    IfDfun ifd ->
      case find (\df -> _ifdName ifd == defName df) defs of
        Just (Dfun v) ->
          when (_dfunArgs v /= _ifdArgs ifd || _dfunRType v /= _ifdRType ifd) $
            throwDesugarError (ImplementationError moduleName ifaceName (_dfunName v)) i
        Just d ->
          throwDesugarError (ImplementationError moduleName ifaceName  (defName d)) i
        Nothing ->
          throwDesugarError (NotImplemented moduleName ifaceName (_ifdName ifd)) i
    IfDCap ifd ->
      case find (\df -> _ifdcName ifd == defName df) defs of
        Just (DCap v) ->
          when (_dcapArgs v /= _ifdcArgs ifd || _dcapRType v /= _ifdcRType ifd) $
            throwDesugarError (ImplementationError moduleName ifaceName (_dcapName v)) i
        Just d -> throwDesugarError (ImplementationError moduleName ifaceName  (defName d)) i
        Nothing ->
          throwDesugarError (NotImplemented moduleName ifaceName (_ifdcName ifd)) i
    IfDPact ifd ->
      case find (\df -> _ifdpName ifd == defName df) defs of
        Just (DPact v) ->
          when (_dpArgs v /= _ifdpArgs ifd || _dpRetType v /= _ifdpRType ifd) $
          throwDesugarError (ImplementationError moduleName ifaceName (_ifdpName ifd)) i
        Just _ ->  throwDesugarError (ImplementationError moduleName ifaceName (_ifdpName ifd)) i
        Nothing -> throwDesugarError (NotImplemented moduleName ifaceName (_ifdpName ifd)) i


-- | Todo: support imports
--   Todo: support
renameInterface
  :: (MonadEval b i m, DesugarBuiltin b)
  => Interface ParsedName DesugarType b i
  -> RenamerT b i m (Interface Name Type b i)
renameInterface (Interface unmangled defs imports ih info) = do
  ifn <- mangleNamespace unmangled
  let defNames = ifDefName <$> defs
  let scc = mkScc ifn (S.fromList defNames) <$> defs
  defs' <- forM (stronglyConnComp scc) \case
    AcyclicSCC d -> pure d
    CyclicSCC d ->
      -- todo: just in case, match on `d` because it makes no sense for there to be an empty cycle
      -- but all uses of `head` are still scary
      throwDesugarError (RecursionDetected ifn (ifDefName <$> d)) (ifDefInfo (head d))
  binds <- view reBinds
  bindsWithImports <- foldlM (handleImport info) binds imports
  (defs'', _, _) <- over _1 reverse <$> foldlM (go ifn) ([], S.empty, bindsWithImports) defs'
  pure (Interface ifn defs'' imports ih info)
  where
  mkScc ifn dns def = (def, ifDefName def, S.toList (ifDefSCC ifn dns def))
  go ifn (ds, s, m) d = do
    let dn = ifDefName d
    when (S.member dn s) $
      throwDesugarError (DuplicateDefinition dn) info
    d' <- local (set reBinds m) $
          local (set reCurrModule (Just (ifn, []))) $ renameIfDef d
    let m' = case ifDefToDef d' of
              Just defn ->
                let dk = defKind defn
                in M.insert dn (NTopLevel ifn ih, Just dk) m
              Nothing -> m
    pure (d':ds, S.insert dn s, m')

runRenamerT
  :: (MonadEval b i m)
  => RenamerT b i m a
  -> m (a, RenamerState)
runRenamerT (RenamerT act) = do
  tlBinds <- usesEvalState loToplevel (fmap (\(fqn, dk) -> (fqnToNameKind fqn, Just dk)))
  let renamerEnv = RenamerEnv tlBinds 0 Nothing Nothing
      renamerState = RenamerState mempty
  runReaderT (runStateT act renamerState) renamerEnv
  where
  fqnToNameKind fqn = NTopLevel (_fqModule fqn) (_fqHash fqn)

runDesugar
  :: (MonadEval b i m)
  => RenamerT b i m a
  -> m (DesugarOutput a)
runDesugar act = do
  (renamed, RenamerState deps) <- runRenamerT act
  pure (DesugarOutput renamed deps)

runDesugarTerm
  :: (MonadEval b i m, DesugarBuiltin b)
  => Lisp.Expr i
  -> m (DesugarOutput (Term Name Type b i))
runDesugarTerm = runDesugar . (desugarLispTerm >=> renameTerm)

runDesugarModule
  :: (MonadEval b i m, DesugarBuiltin b)
  => Lisp.Module i
  -> m (DesugarOutput (Module Name Type b i))
runDesugarModule  = runDesugar . (desugarModule >=> renameModule)

runDesugarInterface
  :: (MonadEval b i m, DesugarBuiltin b)
  => Lisp.Interface i
  -> m (DesugarOutput (Interface Name Type b i))
runDesugarInterface  = runDesugar . (desugarInterface >=> renameInterface)

runDesugarReplDefun
  :: (MonadEval b i m, DesugarBuiltin b)
  => Lisp.Defun i
  -> m (DesugarOutput (Defun Name Type b i))
runDesugarReplDefun =
  runDesugar
  . local (set reCurrModule (Just (replModuleName, [])))
  . (desugarDefun >=> renameReplDefun)

runDesugarReplDefConst
  :: (MonadEval b i m, DesugarBuiltin b)
  =>  Lisp.DefConst i
  -> m (DesugarOutput (DefConst Name Type b i))
runDesugarReplDefConst  =
  runDesugar
  . local (set reCurrModule (Just (replModuleName,[])))
  . (desugarDefConst >=> renameReplDefConst)

runDesugarTopLevel
  :: (MonadEval b i m, DesugarBuiltin b)
  => Lisp.TopLevel i
  -> m (DesugarOutput (TopLevel Name Type b i))
runDesugarTopLevel = \case
  Lisp.TLModule m -> over dsOut TLModule <$> runDesugarModule m
  Lisp.TLTerm e -> over dsOut TLTerm <$> runDesugarTerm e
  Lisp.TLInterface i -> over dsOut TLInterface <$> runDesugarInterface i
  Lisp.TLUse imp info -> runDesugar $ (`TLUse` info) <$> desugarUse info imp



runDesugarReplTopLevel
  :: (MonadEval b i m, DesugarBuiltin b)
  => Lisp.ReplTopLevel i
  -> m (DesugarOutput (ReplTopLevel Name Type b i))
runDesugarReplTopLevel = \case
  -- We do not run desugar here for the repl.
  -- We pattern match before we ever hit this case, therefore this should not be reachable
  -- This is fine to stay in `error`. The repl special functions and forms do not show up on chain
  -- and we want this to be a clear haskell error. The current repl implementation
  -- makes sure to not ever hit this.
  Lisp.RTLTopLevel _ ->
    error "Fatal: do not use desugarReplTopLevel on toplevel forms from the parser. Use runDesugarTopLevel directly"
  Lisp.RTLDefun de ->
    over dsOut RTLDefun <$> runDesugarReplDefun de
  Lisp.RTLDefConst dc ->
    over dsOut RTLDefConst <$> runDesugarReplDefConst dc
