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
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict ( StateT(..), MonadState )
import Control.Monad.Trans.Maybe(MaybeT(..), runMaybeT, hoistMaybe)
import Control.Monad.Except
import Control.Lens hiding (List)
import Data.Text(Text)
import Data.Map.Strict(Map)
import Data.Maybe(mapMaybe)
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

data ModuleType = MTModule | MTInterface

data CurrModule
  = CurrModule
  { _cmName :: ModuleName
  , _cmImplements :: [ModuleName]
  , _cmType :: ModuleType
  }
makeLenses ''CurrModule

data RenamerEnv b i
  = RenamerEnv
  { _reBinds :: Map Text (NameKind, Maybe DefKind)
  , _reCurrModuleTmpBinds ::  Map Text (NameKind, DefKind)
  , _reVarDepth :: DeBruijn
  , _reCurrModule :: Maybe CurrModule
  , _reCurrDef :: Maybe DefKind
  }
makeLenses ''RenamerEnv

currModuleName :: MonadReader (RenamerEnv b i) m => m (Maybe ModuleName)
currModuleName = preview $ reCurrModule . folded . cmName

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

instance (MonadEvalEnv b i m) => MonadEvalEnv b i (RenamerT b i m) where
  readEnv = RenamerT (lift (lift readEnv))

instance (MonadEvalState b i m) => MonadEvalState b i (RenamerT b i m) where
  getEvalState = RenamerT (lift (lift getEvalState))
  putEvalState e = RenamerT (lift (lift (putEvalState e)))
  modifyEvalState f = RenamerT (lift (lift (modifyEvalState f)))

-- Todo: DesugarBuiltin
-- probably should just be a `data` definition we pass in.
class IsBuiltin b => DesugarBuiltin b where
  liftCoreBuiltin :: CoreBuiltin -> b
  desugarOperator :: i -> Lisp.Operator -> Term ParsedName DesugarType b i
  desugarAppArity :: i -> b -> [Term n dt b i] -> Term n dt b i

instance DesugarBuiltin CoreBuiltin where
  liftCoreBuiltin = id
  desugarOperator info = \case
    -- Manual eta expansion for and as well as Or
    Lisp.AndOp -> let
      arg1Name = "#andArg1"
      arg1 = Arg arg1Name (Just (Lisp.TyPrim PrimBool)) info
      arg2Name = "#andArg2"
      arg2 = Arg arg2Name (Just (Lisp.TyPrim PrimBool)) info
      in Lam (arg1 :| [arg2]) (Conditional (CAnd (Var (BN (BareName arg1Name)) info) (Var (BN (BareName arg2Name)) info)) info) info
    Lisp.OrOp -> let
      arg1Name = "#orArg1"
      arg1 = Arg arg1Name (Just (Lisp.TyPrim PrimBool)) info
      arg2Name = "#orArg2"
      arg2 = Arg arg2Name (Just (Lisp.TyPrim PrimBool)) info
      in Lam (arg1 :| [arg2]) (Conditional (COr (Var (BN (BareName arg1Name)) info) (Var (BN (BareName arg2Name)) info)) info) info
    Lisp.EnforceOp -> let
      arg1Name = "#enforceArg1"
      arg1 = Arg arg1Name (Just (Lisp.TyPrim PrimBool)) info
      arg2Name = "#enforceArg2"
      arg2 = Arg arg2Name (Just (Lisp.TyPrim PrimString)) info
      in Lam (arg1 :| [arg2]) (Conditional (CEnforce (Var (BN (BareName arg1Name)) info) (Var (BN (BareName arg2Name)) info)) info) info
    Lisp.EnforceOneOp -> let
      arg1Name = "#enforceOneArg1"
      arg1 = Arg arg1Name (Just (Lisp.TyPrim PrimString)) info
      arg2Name = "#enforceOneArg2"
      arg2 = Arg arg2Name (Just (Lisp.TyList (Lisp.TyPrim PrimBool))) info
      in Lam (arg1 :| [arg2]) (Conditional (CEnforceOne (Var (BN (BareName arg1Name)) info) [Var (BN (BareName arg2Name)) info]) info) info
  desugarAppArity = desugarAppArityRaw id

desugarAppArityRaw
  :: (CoreBuiltin -> builtin)
  -> info
  -> CoreBuiltin
  -> [Term name t builtin info]
  -> Term name t builtin info
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
desugarAppArityRaw f i CoreSub [e1] =
    App (Builtin (f CoreNegate) i) ([e1]) i
desugarAppArityRaw f i CoreEnumerate [e1, e2, e3] =
    App (Builtin (f CoreEnumerateStepN) i) ([e1, e2, e3]) i
desugarAppArityRaw f i CoreSelect [e1, e2, e3] =
    App (Builtin (f CoreSelectWithFields) i) ([e1, e2, e3]) i
desugarAppArityRaw f i CoreSort [e1, e2] =
  App (Builtin (f CoreSortObject) i) [e1, e2] i
-- Rounding functions
desugarAppArityRaw f i CoreRound [e1, e2] =
  App (Builtin (f CoreRoundPrec) i) [e1, e2] i
desugarAppArityRaw f i CoreCeiling [e1, e2] =
  App (Builtin (f CoreCeilingPrec) i) [e1, e2] i
desugarAppArityRaw f i CoreFloor [e1, e2] =
  App (Builtin (f CoreFloorPrec) i) [e1, e2] i


desugarAppArityRaw f i CoreStrToInt [e1, e2] =
  App (Builtin (f CoreStrToIntBase) i) [e1, e2] i
desugarAppArityRaw f i CoreReadMsg [] =
  App (Builtin (f CoreReadMsgDefault) i) [] i
desugarAppArityRaw f i CoreDefineKeySet [e1] =
  App (Builtin (f CoreDefineKeysetData) i) [e1] i
desugarAppArityRaw f i CorePoseidonHashHackachain li =
  App (Builtin (f CorePoseidonHashHackachain) i )[(ListLit li i)] i
desugarAppArityRaw f i CoreYield [e1, e2] =
  App (Builtin (f CoreYieldToChain) i) [e1, e2] i
desugarAppArityRaw f i b args =
    App (Builtin (f b) i) args i

instance DesugarBuiltin (ReplBuiltin CoreBuiltin) where
  liftCoreBuiltin = RBuiltinWrap
  desugarOperator i dsg =
    over termBuiltin RBuiltinWrap $ desugarOperator i dsg
  desugarAppArity i (RBuiltinWrap b) ne =
    desugarAppArityRaw RBuiltinWrap i b ne
  -- (expect <description> <expected> <expression-to-eval>)
  desugarAppArity i (RBuiltinRepl RExpect) ([e1, e2, e3]) | isn't _Nullary e3 =
    App (Builtin (RBuiltinRepl RExpect) i) ([e1, suspendTerm e2, suspendTerm e3]) i
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
  desugarAppArity i (RBuiltinRepl REnvGas) [e1] =
      App (Builtin (RBuiltinRepl REnvGasSet) i) [e1] i
  desugarAppArity i (RBuiltinRepl REnvGasModel) [] =
      App (Builtin (RBuiltinRepl REnvAskGasModel) i) [] i
  desugarAppArity i (RBuiltinRepl REnvGasModel) [e1, e2] =
      App (Builtin (RBuiltinRepl REnvGasModelFixed) i) [e1, e2] i
  desugarAppArity i (RBuiltinRepl RBeginTx) [e1] =
      App (Builtin (RBuiltinRepl RBeginNamedTx) i) [e1] i
  desugarAppArity i (RBuiltinRepl REnforcePactVersionMin) [e1, e2] =
      App (Builtin (RBuiltinRepl REnforcePactVersionRange) i) [e1, e2] i
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
          let c1 = Arg cvar1 Nothing i
              c2 = Arg cvar2 Nothing i
          pure $ Lam (c1 :| [c2]) (Var (BN (BareName cvar1)) i) i
        | n == BareName "CHARSET_ASCII" -> pure (Constant (LInteger 0) i)
        | n == BareName "CHARSET_LATIN1" -> pure (Constant (LInteger 1) i)
        | otherwise ->
          pure (Var (BN n) i)
    where
    cvar1 = "#constantlyA1"
    cvar2 = "#constantlyA2"
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
        args = (\(Lisp.MArg n t ai) -> Arg n t ai) <$> nsts
    body' <- desugarLispTerm body
    pure (Lam args body' i)
  Lisp.Suspend body i -> desugarLispTerm (Lisp.Lam [] body i)
  Lisp.Binding fs hs i -> do
    hs' <- traverse desugarLispTerm hs
    body <- bindingBody hs'
    let bodyLam b = Lam (pure (Arg objFreshText Nothing i)) b i
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
            access = App (Builtin (liftCoreBuiltin CoreAt) i) [fieldLit, objFreshVar] i
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
    case (e, hs) of
      (MapV mapI, Lisp.App operand args appI:xs) -> do
        let v = Lisp.Var injectedArg1Name i
            newArg = Lisp.Lam [Lisp.MArg injectedArg1 Nothing i] (Lisp.App operand (args ++ [v]) appI) appI
        commonDesugar (MapV mapI) (newArg:xs)
      (FilterV filterI, Lisp.App operand args appI:xs) -> do
        let v = Lisp.Var injectedArg1Name i
            newArg = Lisp.Lam [Lisp.MArg injectedArg1 Nothing i] (Lisp.App operand (args ++ [v]) appI) appI
        commonDesugar (FilterV filterI) (newArg:xs)
      (FoldV foldI, Lisp.App operand args appI:xs) -> do
        let v1 = Lisp.Var injectedArg1Name i
            v2 = Lisp.Var injectedArg2Name i
            newArg = Lisp.Lam [Lisp.MArg injectedArg1 Nothing i, Lisp.MArg injectedArg2 Nothing i] (Lisp.App operand (args ++ [v1, v2]) appI) appI
        commonDesugar (FoldV foldI) (newArg:xs)
      (ZipV zipI, Lisp.App operand args appI:xs) -> do
        let v1 = Lisp.Var injectedArg1Name i
            v2 = Lisp.Var injectedArg2Name i
            newArg = Lisp.Lam [Lisp.MArg injectedArg1 Nothing i, Lisp.MArg injectedArg2 Nothing i] (Lisp.App operand (args ++ [v1, v2]) appI) appI
        commonDesugar (ZipV zipI) (newArg:xs)
      (CondV condI, l) -> case reverse l of
        defCase:xs -> do
          defCase' <- desugarLispTerm defCase
          body <- foldlM toNestedIf defCase' xs
          pure $ App (Builtin (liftCoreBuiltin CoreCond) i) [Nullary body condI] condI
        _ -> throwDesugarError (InvalidSyntax "cond: expected list of conditions with a default case") i
        where
        toNestedIf b (Lisp.App cond [body] i') = do
          cond' <- desugarLispTerm cond
          body' <- desugarLispTerm body
          pure $ Conditional (CIf cond' body' b) i'
        toNestedIf _ _ =
          throwDesugarError (InvalidSyntax "cond: expected application of conditions") i
      _ -> commonDesugar e hs
    where
    commonDesugar operator operands = do
      e' <- desugarLispTerm operator
      hs' <- traverse desugarLispTerm operands
      case e' of
        Builtin b _ -> pure (desugarAppArity i b hs')
        _ -> pure (App e' hs' i)
    --  stands for "injected Higher order 1". The name is unimportant,
    --  injected names are not meant to be very readable
    injectedArg1 = ":ijHO1"
    injectedArg1Name = BN (BareName injectedArg1)
    injectedArg2 = ":ijHO2"
    injectedArg2Name =  BN (BareName injectedArg2)
  Lisp.Operator bop i -> pure (desugarOperator i bop)
  Lisp.List e1 i ->
    ListLit <$> traverse desugarLispTerm e1 <*> pure i
  Lisp.Constant l i ->
    pure (Constant l i)
  Lisp.Try e1 e2 i ->
    Try <$> desugarLispTerm e1 <*> desugarLispTerm e2 <*> pure i
  Lisp.Object fields i ->
    ObjectLit <$> (traverse._2) desugarLispTerm fields <*> pure i
  Lisp.CapabilityForm cf i -> (`CapabilityForm` i) <$> case cf of
    Lisp.WithCapability cap body ->
      WithCapability <$> desugarLispTerm cap <*> desugarLispTerm body
    Lisp.CreateUserGuard pn exs ->
      CreateUserGuard pn <$> traverse desugarLispTerm exs
  where
  binderToLet i (Lisp.Binder n mty expr) term = do
    expr' <- desugarLispTerm expr
    pure $ Let (Arg n mty i) expr' term i

pattern MapV :: i -> Lisp.Expr i
pattern MapV info = Lisp.Var (BN (BareName "map")) info
pattern FilterV :: i -> Lisp.Expr i
pattern FilterV info = Lisp.Var (BN (BareName "map")) info
pattern FoldV :: i -> Lisp.Expr i
pattern FoldV info = Lisp.Var (BN (BareName "map")) info
pattern ZipV :: i -> Lisp.Expr i
pattern ZipV info = Lisp.Var (BN (BareName "map")) info

pattern CondV :: i -> Lisp.Expr i
pattern CondV info = Lisp.Var (BN (BareName "cond")) info

suspendTerm
  :: Term n dt builtin info
  -> Term n dt builtin info
suspendTerm e' =
  Nullary e' (view termInfo e')

toArg
  :: Lisp.MArg i
  -> Arg DesugarType i
toArg (Lisp.MArg n mty i) = Arg n mty i

desugarDefun
  :: (MonadEval b i m, DesugarBuiltin b)
  => ModuleName             -- ^ proves this function is called within Pact module scope
  -> Lisp.Defun i
  -> RenamerT b i m (Defun ParsedName DesugarType b i)
desugarDefun _modWitness (Lisp.Defun spec [] body _ _ i) = do
  body' <- desugarLispTerm body
  let bodyLam = Nullary body' i
      spec' = toArg spec
  pure $ Defun spec' [] bodyLam i
desugarDefun _modWitness (Lisp.Defun spec (arg:args) body _ _ i) = do
  let args' = toArg <$> (arg :| args)
  body' <- desugarLispTerm body
  let bodyLam = Lam args' body' i
      spec' = toArg spec
  pure $ Defun spec' (NE.toList args') bodyLam i

desugarDefPact
  :: (MonadEval b i m, DesugarBuiltin b)
  => ModuleName
  -> Lisp.DefPact i
  -> RenamerT b i m (DefPact ParsedName DesugarType b i)
desugarDefPact _mn (Lisp.DefPact (Lisp.MArg dpname _ _) _ [] _ _ i) =
  throwDesugarError (EmptyDefPact dpname) i
desugarDefPact mn (Lisp.DefPact spec@(Lisp.MArg dpname _ _) margs (step:steps) _ _ i) = do
  let args' = toArg <$> margs
      spec' = toArg spec
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

  pure $ DefPact spec' args' steps' i

desugarDefConst
  :: (MonadEval b i m, DesugarBuiltin b)
  => ModuleName             -- ^ proves this function is called within Pact module scope
  -> Lisp.DefConst i
  -> RenamerT b i m (DefConst ParsedName DesugarType b i)
desugarDefConst _modWitness (Lisp.DefConst spec e _ i) = do
  e' <- desugarLispTerm e
  let spec' = toArg spec
  pure $ DefConst spec' (TermConst e') i

desugarDefMeta
  :: (MonadEval b i m)
  => i
  -> [Arg t i]
  -> Lisp.DCapMeta
  -> RenamerT b i m (DefCapMeta ParsedName)
desugarDefMeta info args = \case
  Lisp.DefEvent -> pure DefEvent
  Lisp.DefManaged marg -> case marg of
    Just (arg, name) ->
        case findIndex ((==) arg . view argName) args of
          Just index' ->
            let dmanaged = DefManagedMeta (index', arg) name
            in pure (DefManaged dmanaged)
          Nothing ->
            throwDesugarError (InvalidManagedArg arg) info
    Nothing -> pure (DefManaged AutoManagedMeta)

desugarDefCap
  :: (MonadEval b i m, DesugarBuiltin b)
  => ModuleName             -- ^ proves this function is called within Pact module scope
  -> Lisp.DefCap i
  -> RenamerT b i m (DefCap ParsedName DesugarType b i)
desugarDefCap _modWitness (Lisp.DefCap spec arglist term _docs _model meta i) = do
  let arglist' = toArg <$> arglist
      spec' = toArg spec
  term' <- desugarLispTerm term
  meta' <- fmap FQParsed <$> maybe (pure Unmanaged) (desugarDefMeta i arglist') meta
  pure (DefCap spec' arglist' term' meta' i)

desugarDefSchema
  :: (MonadEval b i m)
  => ModuleName             -- ^ proves this function is called within Pact module scope
  -> Lisp.DefSchema i
  -> RenamerT b i m (DefSchema DesugarType i)
desugarDefSchema _modWitness (Lisp.DefSchema dsn args _docs _model i) = do
  let args' = (\(Lisp.Arg n ty _) -> (Field n, ty)) <$> args
      scd = M.fromList args'
  pure $ DefSchema dsn scd i

desugarDefTable
  :: (MonadEval b i m)
  => ModuleName             -- ^ proves this function is called within Pact module scope
  -> Lisp.DefTable i
  -> RenamerT b i m  (DefTable ParsedName i)
desugarDefTable _modWitness (Lisp.DefTable dtn dts _ i) =
  pure (DefTable dtn (DesugaredTable dts) i)

desugarIfDef
  :: (MonadEval b i m, DesugarBuiltin b)
  => ModuleName
  -> Lisp.IfDef i
  -> RenamerT b i m  (IfDef ParsedName DesugarType b i)
desugarIfDef ifn = \case
  Lisp.IfDfun (Lisp.IfDefun spec margs _ _ i) -> pure $ IfDfun $ IfDefun (toArg spec) (toArg <$> margs) i
  -- Todo: check managed impl
  Lisp.IfDCap (Lisp.IfDefCap spec margs _ _ meta i) -> IfDCap <$> do
    let args = toArg <$> margs
        spec' = toArg spec
    meta' <- fmap (BareName . rawParsedName) <$> maybe (pure Unmanaged) (desugarDefMeta i args) meta
    pure $ IfDefCap spec' args meta' i
  Lisp.IfDConst dc -> IfDConst <$> desugarDefConst ifn dc
  Lisp.IfDPact (Lisp.IfDefPact spec margs _ _ i) -> pure $ IfDPact $ IfDefPact (toArg spec) (toArg <$> margs) i
  Lisp.IfDSchema ds -> IfDSchema <$> desugarDefSchema ifn ds

desugarDef
  :: (MonadEval b i m, DesugarBuiltin b)
  => ModuleName
  -> Lisp.Def i
  -> RenamerT b i m (Def ParsedName DesugarType b i)
desugarDef mname = \case
  Lisp.Dfun d -> Dfun <$> desugarDefun mname d
  Lisp.DConst d -> DConst <$> desugarDefConst mname d
  Lisp.DCap dc -> DCap <$> desugarDefCap mname dc
  Lisp.DSchema d -> DSchema <$> desugarDefSchema mname d
  Lisp.DTable d -> DTable <$> desugarDefTable mname d
  Lisp.DPact d -> DPact <$> desugarDefPact mname d

desugarModule
  :: (MonadEval b i m, DesugarBuiltin b)
  => Lisp.Module i
  -> RenamerT b i m (Module ParsedName DesugarType b i)
desugarModule (Lisp.Module mname mgov extdecls defs _ _ i) = do
  (imports, blessed, implemented) <- splitExts extdecls
  defs' <- locally reCurrModule (const (Just $ CurrModule mname [] MTModule))
          $ traverse (desugarDef mname) (NE.toList defs)
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
  defs' <- traverse (desugarIfDef ifn) ifdefns
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
  Var n _ -> parsedNameSCC currM currDefns n
  -- Note: Lambda Args contain types, which may contain names that
  -- show up in the dependency graph
  Lam args e _ ->
    let currDefns' = foldl' (\s t -> S.delete (_argName t) s) currDefns args
        tySCC = foldMap (argSCC currM currDefns) args
    in tySCC <> termSCC currM currDefns' e
  -- Note: Let args contain types, which may contain names that
  -- show up in the dependency graph
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
  CapabilityForm cf _ -> foldMap (termSCC currM currDefns) cf <> case cf of
    CreateUserGuard nameParam _ -> parsedNameSCC currM currDefns nameParam
    WithCapability _ _ -> mempty
  ObjectLit m _ ->
    foldMap (termSCC currM currDefns . view _2) m
  InlineValue{} -> mempty

parsedNameSCC :: ModuleName -> Set Text -> ParsedName -> Set Text
parsedNameSCC currM currDefns n = case n of
  BN bn | S.member (_bnName bn) currDefns -> S.singleton (_bnName bn)
        | otherwise -> mempty
  QN (QualifiedName n' mn')
    | S.member n' currDefns && mn' == currM -> S.singleton n'
    | otherwise -> mempty
  DN _ -> mempty

tyNameSCC :: ModuleName -> Set Text -> ParsedTyName -> Set Text
tyNameSCC currM currDefs = \case
  TBN bn | S.member (_bnName bn) currDefs -> S.singleton (_bnName bn)
        | otherwise -> mempty
  TQN (QualifiedName n' mn')
    | S.member n' currDefs && mn' == currM -> S.singleton n'
    | otherwise -> mempty

typeSCC
  :: ModuleName
  -> Set Text
  -> DesugarType
  -> Set Text
typeSCC currM currDefs = \case
  Lisp.TyPrim _ -> mempty
  Lisp.TyList l -> typeSCC currM currDefs l
  Lisp.TyModRef _ -> mempty
  Lisp.TyObject pn -> tyNameSCC currM currDefs pn
  Lisp.TyKeyset -> mempty
  Lisp.TyPolyList -> mempty
  Lisp.TyPolyObject -> mempty
  Lisp.TyTable pn -> tyNameSCC currM currDefs pn
  Lisp.TyAny -> mempty

-- | Get the dependent components from an `Arg`
-- Args mean local bindings to something, therefore
-- the actual arg name _cannot_ refer to a dependency,
-- but the type annotation may contain a reference to a schema ann
argSCC :: ModuleName -> Set Text -> Arg DesugarType i -> Set Text
argSCC currM currDefs (Arg _ ty _) = case ty of
  Just t -> typeSCC currM currDefs t
  Nothing -> mempty

-- | Get the set of dependencies from a defun
-- Note: names will show up in:
--   - Defun terms
--   - Defun args
--   - Defun return type
defunSCC
  :: ModuleName
  -> Set Text
  -> Defun ParsedName DesugarType  b i
  -> Set Text
defunSCC mn cd df =
  let tscc = termSCC mn cd (_dfunTerm df)
      argScc = foldMap (argSCC mn cd) (_dfunArgs df)
  in tscc <> argScc <> maybe mempty (typeSCC mn cd) (_argType $ _dfunSpec df)

-- | Get the set of dependencies from a defconst
-- Note: names will show up in:
--   - Defconst terms
--   - Defconst return type
defConstSCC
  :: ModuleName
  -> Set Text
  -> DefConst ParsedName DesugarType  b i
  -> Set Text
defConstSCC mn cd dc =
  let tscc = foldMap (termSCC mn cd) (_dcTerm dc)
      tyscc = maybe mempty (typeSCC mn cd) (_argType $ _dcSpec dc)
  in tscc <> tyscc

-- | Get the set of dependencies from a deftable
-- Note: names will show up in:
--   - The schema reference of the table
defTableSCC
  :: ModuleName
  -> Set Text
  -> DefTable ParsedName info
  -> Set Text
defTableSCC mn cd dt =
  let (DesugaredTable t) = (_dtSchema dt)
  in parsedNameSCC mn cd t

-- | Get the set of dependencies from a defcap
-- Note: names will show up in:
--   - Defcap terms
--   - Defcap args
--   - Defcap return type
--   - Defcap managed meta
defCapSCC
  :: ModuleName
  -> Set Text
  -> DefCap ParsedName DesugarType b i1
  -> Set Text
defCapSCC mn cd dc =
  let argsScc = foldMap (argSCC mn cd) (_dcapArgs dc)
      rtypeScc = maybe mempty (typeSCC mn cd) (_argType $ _dcapSpec dc)
      termScc = termSCC mn cd (_dcapTerm dc)
  in argsScc <> rtypeScc <> termScc <> case _dcapMeta dc of
    DefManaged (DefManagedMeta _ (FQParsed pn)) -> parsedNameSCC mn cd pn
    _ -> mempty

-- | Get the set of dependencies from a defpact
-- Note: names will show up in:
--   - Defpact Steps
--   - Defpact args
--   - Defpact return type
defPactSCC
  :: ModuleName
  -> Set Text
  -> DefPact ParsedName DesugarType b i
  -> Set Text
defPactSCC mn cd dp =
  let argsScc = foldMap (argSCC mn cd) (_dpArgs dp)
      rtScc = maybe mempty (typeSCC mn cd) (_argType $ _dpSpec dp)
      stepsScc = foldMap (defPactStepSCC mn cd) (_dpSteps dp)
  in argsScc <> rtScc <> stepsScc

-- | Calculate the functions the particular defpact step depends on
defPactStepSCC
  :: ModuleName
  -> Set Text
  -> Step ParsedName DesugarType b i
  -> Set Text
defPactStepSCC mn cd = \case
  Step step -> termSCC mn cd step
  StepWithRollback step rollback ->
    S.unions $ [termSCC mn cd step, termSCC mn cd rollback]

-- | Get the set of dependencies from a defun signature defn
-- Note: names will show up in:
--   - Defun signature arguments
--   - Defun signature return type
ifDefunSCC
  :: ModuleName
  -> Set Text
  -> IfDefun DesugarType i
  -> Set Text
ifDefunSCC mn currDefs (IfDefun (Arg _name ty _) args _info) =
   foldMap (argSCC mn currDefs) args <> maybe mempty (typeSCC mn currDefs) ty

-- | Get the set of dependencies from a defcap signature
-- Note: names will show up in:
--   - Defcap signature args
--   - Defcap signature return type
--   - Defcap signature meta
ifDefCapSCC
  :: ModuleName
  -> Set Text
  -> IfDefCap ParsedName DesugarType i
  -> Set Text
ifDefCapSCC mn currDefs (IfDefCap (Arg _name rty _) args meta _info) =
   foldMap (argSCC mn currDefs) args <> maybe mempty (typeSCC mn currDefs) rty <> metaSCC meta
   where
  metaSCC (DefManaged (DefManagedMeta _ bn)) = parsedNameSCC mn currDefs (BN bn)
  metaSCC _ = mempty

-- | Get the set of dependencies from a defpact signature
-- Note: names will show up in:
--   - Defpact signature args
--   - Defpact signature return type
ifDefPactSCC
  :: ModuleName
  -> Set Text
  -> IfDefPact DesugarType i
  -> Set Text
ifDefPactSCC mn currDefs (IfDefPact (Arg _name rty _) args _info) =
   foldMap (argSCC mn currDefs) args <> maybe mempty (typeSCC mn currDefs) rty

-- | Calculate the dependency set for any type of def
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

-- | Calculate the dependency set for any type of interface def
ifDefSCC
  :: ModuleName
  -> Set Text
  -> IfDef ParsedName DesugarType b i1
  -> Set Text
ifDefSCC mn currDefs = \case
  IfDfun ifd -> ifDefunSCC mn currDefs ifd
  IfDCap d -> ifDefCapSCC mn currDefs d
  IfDConst d -> defConstSCC mn currDefs d
  IfDPact d -> ifDefPactSCC mn currDefs d
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
  toLocalDepMap modName mhash defn = (defName defn, (NTopLevel modName mhash, Just (defKind modName defn)))
  toLoadedDepMap modName mhash defn = (defName defn, (FullyQualifiedName modName (defName defn) mhash, defKind modName defn))
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
resolveModuleName i mn@(ModuleName name mNs) =
  view reCurrModule >>= \case
    -- TODO better error message if it's not MTMOdule
    -- If we are in a Module eval, we will need to check two conditions:
    -- is the current module name exactly equivalent? if so, return it.
    -- if not, why not? Is it because the module we're searching for is unmangled, or
    -- because it lives in the root namespace?
    -- We therefore check the root namespace first, and if nothing was found, then
    -- we mangle and check again.
    Just (CurrModule currMod imps MTModule)
      | currMod == mn -> pure (currMod, imps)
      | otherwise -> do
        pdb <- viewEvalEnv eePactDb
        lift (lookupModuleData i pdb mn) >>= \case
          Just md -> getModName md
          Nothing -> case mNs of
            Just _ -> throwDesugarError (NoSuchModule mn) i
            -- Over here, it means we have not found it in the root namespace
            -- and the currModule's name may be mangled
            Nothing -> useEvalState (esLoaded . loNamespace) >>= \case
              Nothing -> throwDesugarError (NoSuchModule mn) i
              Just (Namespace ns _ _)
                | ModuleName name (Just ns) == currMod -> pure (currMod, imps)
                | otherwise ->
                  lift (getModuleData i pdb (ModuleName name (Just ns))) >>= getModName
    _ -> resolveModuleData mn i >>= getModName
    where
    getModName = \case
      ModuleData module_ _ -> pure (_mName module_, _mImplements module_)
      InterfaceData _ _ ->
        throwDesugarError (InvalidModuleReference mn) i

-- | Resolve a module name, return the implemented members as well if any
-- including all current
resolveInterfaceName :: (MonadEval b i m) => i -> ModuleName -> RenamerT b i m (ModuleName)
resolveInterfaceName i mn@(ModuleName name mNs) =
  view reCurrModule >>= \case
    -- TODO better error message if it's not MTInterface
    -- If we are in an interface eval, we will need to check two conditions:
    -- is the current module name exactly equivalent? if so, return it.
    -- if not, why not? Is it because the module we're searching for is unmangled, or
    -- because it lives in the root namespace?
    -- We therefore check the root namespace first, and if nothing was found, then
    -- we mangle and check again.
    Just (CurrModule currMod _ MTInterface)
      | currMod == mn -> pure mn
      | otherwise -> do
          pdb <- viewEvalEnv eePactDb
          lift (lookupModuleData i pdb mn) >>= \case
            Just (InterfaceData _ _) -> pure mn
            Just _ -> throwDesugarError (InvalidModuleReference mn) i
            Nothing -> case mNs of
              Just _ -> throwDesugarError (NoSuchModule mn) i
              -- Over here, it means we have not found it in the root namespace
              -- and the currModule's name may be mangled
              Nothing -> useEvalState (esLoaded . loNamespace) >>= \case
                Nothing -> throwDesugarError (NoSuchModule mn) i
                Just (Namespace ns _ _)
                  | ModuleName name (Just ns) == currMod -> pure currMod
                  | otherwise ->
                    lift (getModuleData i pdb (ModuleName name (Just ns))) >>= getModName
    _ -> resolveModuleData mn i >>= getModName
    where
    getModName = \case
      ModuleData _ _ ->
        throwDesugarError (InvalidModuleReference mn) i
      InterfaceData _ _ -> pure mn


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
  Lisp.TyAny -> pure TyAny
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

argType' :: Traversal (Arg ty i) (Arg ty' i) ty ty'
argType' f (Arg n ty i) = Arg n <$> traverse f ty <*> pure i

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
renameTerm (Lam nsts body i) = do
  depth <- view reVarDepth
  let len = fromIntegral (NE.length nsts)
      newDepth = depth + len
      ixs = NE.fromList [depth .. newDepth - 1]
  let m = M.fromList $ NE.toList $ NE.zip (_argName <$> nsts) ((,Nothing). NBound <$> ixs)
  term' <- local (inEnv m newDepth) (renameTerm body)
  nsts' <- (traversed . argType . _Just) (renameType i) nsts
  pure (Lam nsts' term' i)
  where
  inEnv m newDepth =
    over reBinds (M.union m) .
    set reVarDepth newDepth
renameTerm (Let arg e1 e2 i) = do
  depth <- view reVarDepth
  arg' <- argType' (renameType i) arg
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
renameTerm (Try e1 e2 i) = do
  Try <$> renameTerm e1 <*> renameTerm e2 <*> pure i
renameTerm (CapabilityForm cf i) = case cf of
  CreateUserGuard parsedName args -> do
      (name', _dkind) <- resolveName i parsedName
      -- Ensure user guards have a valid reference
      -- Todo: we currently cover this in caps.repl, do we want
      -- to make this a static error?
      -- when (dkind /= Just DKDefun) $ throwDesugarError (InvalidUserGuard (rawParsedName parsedName)) i
      CapabilityForm <$> (CreateUserGuard name' <$> traverse renameTerm args) <*> pure i
  WithCapability cap body -> do
    enforceNotWithinDefcap i "with-capability"
    CapabilityForm <$> (WithCapability <$> renameTerm cap <*> renameTerm body) <*> pure i
renameTerm (ObjectLit o i) =
  ObjectLit <$> (traverse._2) renameTerm o <*> pure i
renameTerm (InlineValue pb i) = pure (InlineValue pb i)

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
renameDefun (Defun (Arg n ret ni) args term i) = do
  -- Todo: put type variables in scope here, if we want to support polymorphism
  args' <- (traverse.argType') (renameType i) args
  ret' <- traverse (renameType i) ret
  term' <- local (set reCurrDef (Just DKDefun)) $ renameTerm term
  pure (Defun (Arg n ret' ni) args' term' i)

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
renameDefPact (DefPact (Arg n mret ni) argtys steps i) = do
  args' <- (traverse.argType') (renameType i) argtys
  mret' <- traverse (renameType i) mret
  steps' <- local (set reCurrDef (Just DKDefPact) . bindArgs) $
    traverse renamePactStep steps
  pure (DefPact (Arg n mret' ni) args' steps' i)
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
renameReplDefun (Defun (Arg n ret ni) args term i) = do
  let fqn = FullyQualifiedName replModuleName n replModuleHash
  args' <- (traverse.argType') (renameType i) args
  ret' <- traverse (renameType i) ret
  esLoaded . loToplevel %== M.insert n (fqn, DKDefun)
  term' <- local (set reCurrDef (Just DKDefun)) $ renameTerm term
  pure (Defun (Arg n ret' ni) args' term' i)

renameReplDefConst
  :: (MonadEval b i m, DesugarBuiltin b)
  => DefConst ParsedName DesugarType b i
  -> RenamerT b i m (DefConst Name Type b i)
renameReplDefConst (DefConst (Arg n mty ni) term i) = do
  let fqn = FullyQualifiedName replModuleName n replModuleHash
  esLoaded . loToplevel %== M.insert n (fqn, DKDefConst)
  mty' <- traverse (renameType i) mty
  term' <- local (set reCurrDef (Just DKDefConst)) $ traverse renameTerm term
  pure (DefConst (Arg n mty' ni) term' i)

renameDefConst
  :: (MonadEval b i m, DesugarBuiltin b)
  => DefConst ParsedName DesugarType b i
  -> RenamerT b i m (DefConst Name Type b i)
renameDefConst (DefConst (Arg n mty ni) term i) = do
  mty' <- traverse (renameType i) mty
  term' <- local (set reCurrDef (Just DKDefConst)) $ traverse renameTerm term
  pure (DefConst (Arg n mty' ni) term' i)

renameDefCap
  :: (MonadEval b i m, DesugarBuiltin b)
  => DefCap ParsedName DesugarType b i
  -> RenamerT b i m (DefCap Name Type b i)
renameDefCap (DefCap (Arg name rtype ni) argtys term meta info) = do
  meta' <- resolveMeta info meta
  argtys' <- (traverse.argType') (renameType info) argtys
  rtype' <- traverse (renameType info) rtype
  term' <- local (set reCurrDef (Just DKDefCap) .  bindArgs) $ renameTerm term
  pure (DefCap (Arg name rtype' ni) argtys' term' meta' info)
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
  -> DefCapMeta (FQNameRef ParsedName)
  -> RenamerT b i m (DefCapMeta (FQNameRef Name))
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
  => ModuleName
  -> Set Text
  -> IfDef ParsedName DesugarType b i
  -> RenamerT b i m (IfDef Name Type b i)
renameIfDef mn ifDefuns = \case
  IfDfun (IfDefun spec args i) -> do
    args' <- (traverse.argType') (renameType i) args
    rtype' <- traverse (renameType i) (_argType spec)
    let spec' = spec & argType .~ rtype'
    pure (IfDfun $ IfDefun spec' args' i)

  IfDConst d -> IfDConst <$> renameDefConst d
  IfDSchema d -> IfDSchema <$> renameDefSchema d
  IfDCap (IfDefCap spec args meta i) -> do
    args' <- (traverse.argType') (renameType i) args
    rtype' <- traverse (renameType i) (_argType spec)
    traverse_ (ensureInLocals i) meta

    let spec' = spec & argType .~ rtype'
    pure (IfDCap $ IfDefCap spec' args' meta i)
    where
    ensureInLocals info (BareName n) = do
      unless (S.member n ifDefuns) $ throwDesugarError (NoSuchModuleMember mn n) info

  IfDPact (IfDefPact spec args i) -> do
    args' <- (traverse.argType') (renameType i) args
    rtype' <- traverse (renameType i) (_argType spec)
    let spec' = spec & argType .~ rtype'
    pure (IfDPact $ IfDefPact spec' args' i)

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
  -> RenamerT b i m Name
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
      let unmangled = ModuleName bn Nothing
      (mn, imps) <- resolveModuleName i unmangled
      pure (Name bn (NModRef mn imps), Nothing)

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
  lookupLocalQual defnName moduleName = do
    currMod <- MaybeT currModuleName
    guard (currMod == moduleName)
    (nk, dk) <- MaybeT $ view (reCurrModuleTmpBinds . at defnName)
    pure (Name defnName nk, Just dk)
  baseLookup pdb defnName moduleName = lookupLocalQual defnName moduleName <|> do
    MaybeT (lift (lookupModuleData i pdb moduleName)) >>= \case
      ModuleData module' _ -> do
        d <- hoistMaybe (findDefInModule defnName module' )
        lift $ rsDependencies %= S.insert moduleName
        pure (Name qn (NTopLevel moduleName (_mHash module')), Just (defKind (_mName module') d))
      InterfaceData iface _ -> do
        let ifn = _ifName iface
        d <- hoistMaybe (findDefInInterface defnName iface)
        lift $ rsDependencies %= S.insert moduleName
        pure (Name qn (NTopLevel moduleName (_ifHash iface)), Just (defKind ifn d))
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
  (defs'', _, _, _) <- over _1 reverse <$> foldlM (go mname) ([], S.empty, bindsWithImports, M.empty) defs'
  traverse_ (checkImplements i defs'' mname) implements
  pure (Module mname mgov' defs'' blessed imports implements mhash i)
  where
  -- Our deps are acyclic, so we resolve all names
  go mname (!defns, !s, !m, !mlocals) defn = do
    when (S.member (defName defn) s) $ throwDesugarError (DuplicateDefinition (defName defn)) i
    let dn = defName defn
    defn' <- local (set reCurrModule (Just $ CurrModule mname implements MTModule) . set reCurrModuleTmpBinds mlocals)
             $ local (set reBinds m) $ renameDef defn
    let dk = defKind mname defn'
    let depPair = (NTopLevel mname mhash, dk)
    let m' = M.insert dn (over _2 Just depPair) m
        mlocals' = M.insert dn depPair mlocals
    pure (defn':defns, S.insert (defName defn) s, m', mlocals')

  resolveGov mname = \case
    KeyGov rawKsn -> case parseAnyKeysetName (_keysetName rawKsn) of
      Left {} -> lift $ throwExecutionError i (ModuleGovernanceFailure mname)
      Right ksn ->
        pure (KeyGov ksn)
    CapGov (FQParsed govName) ->
      case find (\d -> BN (BareName (defName d)) == govName) defs of
        Just (DCap d) -> do
          let fqn = FullyQualifiedName mname (_argName $ _dcapSpec d) mhash
          pure (CapGov (FQName fqn))
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
    IfDfun (IfDefun (Arg ifdName ifRty _) ifArgs _) ->
      case find (\df -> ifdName == defName df) defs of
        Just (Dfun (Defun (Arg funName funRty _) funArgs _ _)) ->
          when (weakArgsNeq funArgs ifArgs || funRty /= ifRty) $
            throwDesugarError (ImplementationError moduleName ifaceName funName) i
        Just d ->
          throwDesugarError (ImplementationError moduleName ifaceName  (defName d)) i
        Nothing ->
          throwDesugarError (NotImplemented moduleName ifaceName ifdName) i
    IfDCap (IfDefCap (Arg ifdcName ifdcRty _) ifdcArgs' ifdcMeta' _) ->
      case find (\df -> ifdcName == defName df) defs of
        Just (DCap (DefCap (Arg dcapName dcapRty _) dcapArgs' _ dcapMeta' _)) -> do
          unless (checkMetaMatches dcapMeta' ifdcMeta') $
             throwDesugarError (ImplementationError moduleName ifaceName $ ": defcap mismatch for " <> dcapName) i
          when (weakArgsNeq dcapArgs' ifdcArgs' || dcapRty /= ifdcRty) $
            throwDesugarError (ImplementationError moduleName ifaceName dcapName) i
        Just d -> throwDesugarError (ImplementationError moduleName ifaceName  (defName d)) i
        Nothing ->
          throwDesugarError (NotImplemented moduleName ifaceName ifdcName) i
    IfDPact (IfDefPact (Arg ifdpName ifdpRty _) ifdpArgs' _) ->
      case find (\df -> ifdpName == defName df) defs of
        Just (DPact (DefPact (Arg _ dpRty _) dpArgs' _ _)) ->
          when (weakArgsNeq dpArgs' ifdpArgs' || dpRty /= ifdpRty) $
          throwDesugarError (ImplementationError moduleName ifaceName ifdpName) i
        Just _ ->  throwDesugarError (ImplementationError moduleName ifaceName ifdpName) i
        Nothing -> throwDesugarError (NotImplemented moduleName ifaceName ifdpName) i

  -- Weak comparison of 'Arg' by ignoring '_argInfo' via mapping it to unit.
  weakArgsNeq a b = fmap void a /= fmap void b

  checkMetaMatches :: DefCapMeta (FQNameRef Name) -> DefCapMeta BareName -> Bool
  checkMetaMatches Unmanaged Unmanaged = True
  checkMetaMatches DefEvent DefEvent = True
  checkMetaMatches (DefManaged l) (DefManaged r) = checkManagedMatches l r
  checkMetaMatches _ _ = False

  checkManagedMatches :: DefManagedMeta (FQNameRef Name) -> DefManagedMeta BareName -> Bool
  checkManagedMatches _ AutoManagedMeta = True
  checkManagedMatches (DefManagedMeta lhs (FQName lName)) (DefManagedMeta rhs (BareName rName)) =
    lhs == rhs &&
    _fqName lName == rName &&
    _fqModule lName == moduleName
  checkManagedMatches _ _ = False


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
  (defs'', _, _, _) <- over _1 reverse <$> foldlM (go ifn) ([], S.empty, bindsWithImports, S.empty) defs'
  pure (Interface ifn defs'' imports ih info)
  where
  mkScc ifn dns def = (def, ifDefName def, S.toList (ifDefSCC ifn dns def))
  go ifn (ds, s, m, dfnSet) d = do
    let dn = ifDefName d
    when (S.member dn s) $
      throwDesugarError (DuplicateDefinition dn) info
    d' <- local (set reBinds m) $
          local (set reCurrModule (Just $ CurrModule ifn [] MTInterface)) $ renameIfDef ifn dfnSet d
    let m' = case ifDefToDef d' of
              Just defn ->
                let dk = defKind ifn defn
                in M.insert dn (NTopLevel ifn ih, Just dk) m
              Nothing -> m
        dfnSet' = case d of
          IfDfun{} -> S.insert dn dfnSet
          _ -> dfnSet
    pure (d':ds, S.insert dn s, m', dfnSet')

runRenamerT
  :: (MonadEval b i m)
  => RenamerT b i m a
  -> m (a, RenamerState)
runRenamerT (RenamerT act) = do
  tlBinds <- usesEvalState loToplevel (fmap (\(fqn, dk) -> (fqnToNameKind fqn, Just dk)))
  let renamerEnv = RenamerEnv tlBinds mempty 0 Nothing Nothing
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
  . local (set reCurrModule (Just $ CurrModule replModuleName [] MTModule))
  . (desugarDefun replModuleName >=> renameReplDefun)

runDesugarReplDefConst
  :: (MonadEval b i m, DesugarBuiltin b)
  =>  Lisp.DefConst i
  -> m (DesugarOutput (DefConst Name Type b i))
runDesugarReplDefConst  =
  runDesugar
  . local (set reCurrModule (Just $ CurrModule replModuleName [] MTModule))
  . (desugarDefConst replModuleName >=> renameReplDefConst)

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


-- Some types don't get all their lenses used, hence GHC warns about unused bindings.
-- This is one way to controllably silence these warnings.
data Unused where Unused :: a -> Unused

_unused :: [Unused]
_unused = [Unused $ set cmImplements, Unused $ set cmType]
