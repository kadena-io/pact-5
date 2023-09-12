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

import Debug.Trace
import Control.Monad ( when, forM, (>=>))
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Lens hiding (List)
import Data.Text(Text)
import Data.Map.Strict(Map)
import Data.Maybe(mapMaybe)
import Data.List(findIndex)
import Data.List.NonEmpty(NonEmpty(..))
import Data.Set(Set)
import Data.Graph(stronglyConnComp, SCC(..))
import Data.Foldable(foldl', foldlM)
import Data.Proxy
import Data.Foldable(find, traverse_, foldrM)
import qualified Data.Map.Strict as M
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set

import Pact.Core.Builtin
import Pact.Core.Names
import Pact.Core.Type
import Pact.Core.Literal
import Pact.Core.Hash
import Pact.Core.Persistence hiding (loaded)
import Pact.Core.Capabilities
import Pact.Core.Errors
import Pact.Core.IR.Term

import qualified Pact.Core.Syntax.ParseTree as Lisp

{- Note on Desugaring + Renaming:

  [Desugaring]
  In surface new pact core (and the lisp as well) we have "blocks",
  which are more like a sequence term `a; b`. We desugar blocks such as:
    let x1 = e1;
    let x2 = e2;
    defun f(a, b) = e3
    f1(f(x1, x2));
    f2();
  into:
    let x1 = e1 in
    let x2 = e2 in
    let f = fn (a, b) => e3 in
    {f1(f(x1, x2)), f2()}

  Moreover, `if a then b else c` gets desugared into
  `if a then () => b else () => c`

  A special note: The "empty application" is currently being desugared into unit application for
  all but builtins. That is:
  (f) is desugared as `(f ())` and

  [Renaming]
  In core, we use a locally nameless representation, and prior to the final pass,
  we use unique names for bound variables, for simplicity in the typechecker and in other passes.
  In the process of generating unique names and renaming bound locals, we also perform two other
  tasks:
    - We resolve imported names
    - We ensure the call graph in the functions declared in the module is acyclic
    If perf of stronglyConnCompR is every measured to be suboptimal, it might be
    worth writing our own.
-}

-- data RNameKind
--   = RNBound DeBruijn
--   | RNTopLevel ModuleName ModuleHash DefKind
--   | RNModRef ModuleName [ModuleName]
--   deriving Show

type DesugarType = Lisp.Type

data RenamerEnv b i
  = RenamerEnv
  { _reBinds :: Map Text (NameKind, Maybe DefKind)
  , _reVarDepth :: DeBruijn
  , _rePactDb :: PactDb b i
  , _reCurrModule :: Maybe ModuleName
  , _reCurrDef :: Maybe DefKind
  }
makeLenses ''RenamerEnv

data RenamerState b i
  = RenamerState
  { _rsModuleBinds :: Map ModuleName (Map Text (NameKind, DefKind))
  , _rsLoaded :: Loaded b i
  , _rsDependencies :: Set ModuleName }

makeLenses ''RenamerState

newtype RenamerT m b i a =
  RenamerT (StateT (RenamerState b i) (ReaderT (RenamerEnv b i) m) a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (RenamerEnv b i)
    , MonadState (RenamerState b i)
    , MonadIO)
  via (StateT (RenamerState b i) (ReaderT (RenamerEnv b i) m))

data DesugarOutput b i a
  = DesugarOutput
  { _dsOut :: a
  , _dsLoaded :: Loaded b i
  , _dsDeps :: Set ModuleName
  } deriving (Show, Functor)

type MonadDesugar raw reso i m =
  ( DesugarBuiltin raw
  , MonadError (PactError i) m
  , MonadState (RenamerState reso i) m
  , MonadReader (RenamerEnv reso i) m
  , MonadIO m)

type MonadRenamer reso i m =
  ( MonadError (PactError i) m
  , MonadState (RenamerState reso i) m
  , MonadReader (RenamerEnv reso i) m
  , MonadIO m)

dsOut :: Lens (DesugarOutput b i a) (DesugarOutput b i a') a a'
dsOut f (DesugarOutput a l d) =
  f a <&> \a' -> DesugarOutput a' l d

-- Todo: DesugarBuiltin
-- probably should just be a `data` definition we pass in.
-- This class is causing us to use `Proxy`
class DesugarBuiltin b where
  reservedNatives :: Map Text b
  liftRaw :: RawBuiltin -> b
  desugarOperator :: i -> Lisp.Operator -> Term ParsedName DesugarType b i
  desugarAppArity :: i -> b -> NonEmpty (Term ParsedName DesugarType b i) -> Term ParsedName DesugarType b i

instance DesugarBuiltin RawBuiltin where
  liftRaw = id
  reservedNatives = rawBuiltinMap
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
  -- Todo:
  -- Builtins of known arity differences we are yet to support:
  --  str-to-int
  --  read (db, later milestone)
  --  select (db, later milestone)
  --  floor
  --  log
  desugarAppArity = desugarAppArityRaw id

desugarAppArityRaw
  :: (RawBuiltin -> builtin)
  -> info
  -> RawBuiltin
  -> NonEmpty (Term name DesugarType builtin info)
  -> Term name DesugarType builtin info
desugarAppArityRaw f i RawEnumerate (e1 :| [e2, e3]) =
    App (Builtin (f RawEnumerateStepN) i) (e1 :| [e2, e3]) i
desugarAppArityRaw f i RawSort (e1 :| [e2]) =
  App (Builtin (f RawSortObject) i) (e1 :| [e2]) i
desugarAppArityRaw f i b args =
    App (Builtin (f b) i) args i

instance DesugarBuiltin (ReplBuiltin RawBuiltin) where
  liftRaw = RBuiltinWrap
  reservedNatives = replRawBuiltinMap
  desugarOperator i dsg =
    over termBuiltin RBuiltinWrap $ desugarOperator i dsg
  desugarAppArity i (RBuiltinWrap b) ne =
    desugarAppArityRaw RBuiltinWrap i b ne
  desugarAppArity i (RBuiltinRepl RExpect) (e1 :| [e2, e3]) | isn't _Lam e3 =
    App (Builtin (RBuiltinRepl RExpect) i) (e1 :| [e2, suspendTerm e3]) i
  desugarAppArity i (RBuiltinRepl RExpectFailure) (e1 :| [e2]) | isn't _Lam e2 =
    App (Builtin (RBuiltinRepl RExpectFailure) i) (e1 :| [suspendTerm e2]) i
  desugarAppArity i (RBuiltinRepl RContinuePact) (e1 :| e2)  =
    App (Builtin (RBuiltinRepl RContinuePact) i) (e1 :| e2) i
  desugarAppArity i b ne =
    App (Builtin b i) ne i

throwDesugarError :: MonadError (PactError i) m => DesugarError -> i -> m a
throwDesugarError de = throwError . PEDesugarError de

-- pattern Bind i = Lisp.Var (BN (BareName "bind")) i

-- pattern WithRead i = Lisp.Var (BN (BareName "with-read")) i

-- pattern WithDefaultRead i = Lisp.Var (BN (BareName "with-read")) i


desugarLispTerm
  :: forall raw reso i m
  . (MonadDesugar raw reso i m)
  => Lisp.Expr i
  -> m (Term ParsedName DesugarType raw i)
desugarLispTerm = \case
  Lisp.Var (BN n) i  ->
    case M.lookup (_bnName n) reservedNatives' of
      Just b -> pure (Builtin b i)
      Nothing ->
        pure (Var (BN n) i)
  Lisp.Var n i -> pure (Var n i)
  Lisp.Block nel i -> do
    nel' <- traverse desugarLispTerm nel
    pure $ foldr (\a b -> Sequence a b i) (NE.last nel') (NE.init nel')
  Lisp.LetIn binders expr i -> do
    expr' <- desugarLispTerm expr
    foldrM (binderToLet i) expr' binders
  Lisp.Lam [] body i -> let
    n = "#unitLamArg"
    nty = Just (Lisp.TyPrim PrimUnit)
    in Lam AnonLamInfo (pure (Arg n nty)) <$> desugarLispTerm body <*> pure i
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
            access = App (Builtin (liftRaw RawAt) i) (fieldLit :| [objFreshVar]) i
        in Let arg access body i
  Lisp.If e1 e2 e3 i -> Conditional <$>
     (CIf <$> desugarLispTerm e1 <*> desugarLispTerm e2 <*> desugarLispTerm e3) <*> pure i
  -- Note: this is our "unit arg application" desugaring
  -- This _may not_ stay long term
  Lisp.App e [] i -> desugarLispTerm e <&> \case
    v@Var{} ->
      let arg = Constant LUnit i :| []
      in App v arg i
    e' -> e'
  Lisp.App (Lisp.Operator o _oi) [e1, e2] i -> case o of
    Lisp.AndOp ->
      Conditional <$> (CAnd <$> desugarLispTerm e1 <*> desugarLispTerm e2) <*> pure i
    Lisp.OrOp ->
      Conditional <$> (COr <$> desugarLispTerm e1 <*> desugarLispTerm e2) <*> pure i
  Lisp.App e (h:hs) i -> do
    e' <- desugarLispTerm e
    h' <- desugarLispTerm h
    hs' <- traverse desugarLispTerm hs
    case e' of
      Builtin b _ -> pure (desugarAppArity i b (h' :| hs'))
      _ -> pure (App e' (h' :| hs') i)
  Lisp.Operator bop i -> pure (desugarOperator i bop)
  Lisp.DynAccess e fn i ->
    DynInvoke <$> desugarLispTerm e <*> pure fn  <*> pure i
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
    Lisp.RequireCapability pn exs ->
      RequireCapability pn <$> traverse desugarLispTerm exs
    Lisp.ComposeCapability pn exs ->
      ComposeCapability pn <$> traverse desugarLispTerm exs
    Lisp.InstallCapability pn exs ->
      InstallCapability pn <$> traverse desugarLispTerm exs
    Lisp.EmitEvent pn exs ->
      EmitEvent pn <$> traverse desugarLispTerm exs
  where
  binderToLet i (Lisp.Binder n mty expr) term = do
    expr' <- desugarLispTerm expr
    pure $ Let (Arg n mty) expr' term i
  reservedNatives' :: Map Text raw
  reservedNatives' = reservedNatives

suspendTerm
  :: Term ParsedName DesugarType builtin info
  -> Term ParsedName DesugarType builtin info
suspendTerm e' =
  Lam AnonLamInfo (pure (Arg "#suspendArg" (Just (Lisp.TyPrim PrimUnit)))) e' (view termInfo e')

toArg
  :: Lisp.MArg
  -> Arg DesugarType
toArg (Lisp.MArg n mty) = Arg n mty

-- Arg for turning nullary into unary functions
-- Todo: Remove this, support nullary apps.
unitFnArg :: Arg DesugarType
unitFnArg = Arg "#unitFnArg" (Just (Lisp.TyPrim PrimUnit))

desugarDefun
  :: (MonadDesugar raw reso i m)
  => Lisp.Defun i
  -> m (Defun ParsedName DesugarType raw i)
desugarDefun (Lisp.Defun defname [] mrt body _ _ i) = do
  body' <- desugarLispTerm body
  view reCurrModule >>= \case
    Just mn -> do
      let bodyLam = Lam (TLDefun mn defname) (pure unitFnArg) body' i
      pure $ Defun defname [unitFnArg] mrt bodyLam i
    Nothing -> error "Defun is module-less"
desugarDefun (Lisp.Defun defname (arg:args) mrt body _ _ i) = do
  let args' = toArg <$> (arg :| args)
  body' <- desugarLispTerm body
  view reCurrModule >>= \case
    Just mn -> do
      let bodyLam = Lam (TLDefun mn defname) args' body' i
      pure $ Defun defname (NE.toList args') mrt bodyLam i
    Nothing -> error "Defun is module-less"

desugarDefPact
  :: forall i m raw reso. MonadDesugar raw reso i m
  => Lisp.DefPact i
  -> m (DefPact ParsedName DesugarType raw i)
desugarDefPact (Lisp.DefPact dpname _ _ [] _ _ i) = throwDesugarError (EmptyDefPact dpname) i
desugarDefPact (Lisp.DefPact dpname margs rt (step:steps) _ _ i) =
  view reCurrModule >>= \case
    Just mn -> do
      let
        args' = case margs of
                  [] -> pure unitFnArg
                  arg:args -> toArg <$> (arg :| args)
        desugarStep b = do
          tm <- desugarLispTerm b
          pure (Lam (TLDefPact mn dpname) args' tm i) -- TODO: add TLPactStep
        desugarMSteps = maybe (pure Nothing) (fmap Just . traverse desugarStep)
      steps' <- forM (step :| steps) \case
        Lisp.Step s ms ->
          Step <$> desugarStep s <*> desugarMSteps ms
        Lisp.StepWithRollback s rb ms ->
          StepWithRollback
          <$> desugarStep s
          <*> desugarStep rb
          <*> desugarMSteps ms
      pure $ DefPact dpname args' rt (NE.reverse steps') i
    Nothing -> error "Defpact is module-less"

desugarDefConst
  :: (MonadDesugar raw reso i m)
  => Lisp.DefConst i
  -> m (DefConst ParsedName DesugarType raw i)
desugarDefConst (Lisp.DefConst n mty e _ i) = do
  e' <- desugarLispTerm e
  pure $ DefConst n mty e' i

desugarDefMeta
  :: Applicative f
  => Term n DesugarType b i
  -> Lisp.DCapMeta
  -> f (DefCapMeta ParsedName)
desugarDefMeta body = \case
  Lisp.DefEvent -> pure DefEvent
  Lisp.DefManaged marg -> case marg of
    Just (arg, name) -> case body of
      Lam _ args _ _ ->
        case findIndex ((==) arg . view argName) (NE.toList args) of
          Just index' ->
            let dmanaged = DefManagedMeta index' name
            in pure (DefManaged (Just dmanaged))
          Nothing -> error "no such managed arg"
      _ -> error "invalid body: not a lambda form, cannot find app arg"
    Nothing -> pure (DefManaged Nothing)

desugarDefCap
  :: (MonadDesugar raw reso info m)
  => Lisp.DefCap info
  -> m (DefCap ParsedName DesugarType raw info)
desugarDefCap (Lisp.DefCap dcn [] rtype term _docs _model meta i) = do
  term' <- desugarLispTerm term
  meta' <- traverse (desugarDefMeta term') meta
  pure (DefCap dcn 0 [] rtype term' meta' i)
desugarDefCap (Lisp.DefCap dcn (x:xs) rtype term _docs _model meta i) = do
  let args = toArg <$> (x :| xs)
  let appArity = NE.length args
  let termBody = Lisp.Lam (x:xs) term i
  term' <- desugarLispTerm termBody
  -- todo: pass module name into this.
  -- same with renameDef
  view reCurrModule >>= \case
    Just mname -> do
      let bodyLam = Lam (TLDefCap mname dcn) args term' i
      meta' <- traverse (desugarDefMeta term') meta
      pure (DefCap dcn appArity (NE.toList args) rtype bodyLam meta' i)
    Nothing -> error "defcap outside of module"

desugarDefSchema
  :: (MonadRenamer reso info m)
  => Lisp.DefSchema info
  -> m (DefSchema DesugarType info)
desugarDefSchema (Lisp.DefSchema dsn args _docs _model i) = do
  let args' = (\(Lisp.Arg n ty) -> (Field n, ty)) <$> args
      scd = M.fromList args'
  pure $ DefSchema dsn scd i

desugarDefTable
  :: (MonadRenamer reso info m)
  => Lisp.DefTable info
  -> m (DefTable ParsedName info)
desugarDefTable (Lisp.DefTable dtn dts _ i) =
  pure (DefTable dtn (DesugaredTable dts) i)

desugarIfDef
  :: (MonadDesugar raw reso info m)
  => Lisp.IfDef info
  -> m (IfDef ParsedName DesugarType raw info)
desugarIfDef = \case
  Lisp.IfDfun (Lisp.IfDefun n margs rty _ _ i) -> IfDfun <$> case margs of
    [] -> do
      pure $ IfDefun n [unitFnArg] rty i
    _ -> do
      let args = toArg <$> margs
      rty' <- maybe (throwDesugarError (UnannotatedReturnType n) i) pure rty
      pure $ IfDefun n args (Just rty') i
  -- Todo: check managed impl
  Lisp.IfDCap (Lisp.IfDefCap n margs rty _ _ _meta i) -> IfDCap <$> do
    let args = toArg <$> margs
    pure $ IfDefCap n args rty i
  Lisp.IfDConst dc -> IfDConst <$> desugarDefConst dc
  _ -> error "unimplemented: special interface decl forms in desugar"

desugarDef
  :: (MonadDesugar raw reso i m)
  => Lisp.Def i
  -> m (Def ParsedName DesugarType raw i)
desugarDef = \case
  Lisp.Dfun d -> Dfun <$> desugarDefun d
  Lisp.DConst d -> DConst <$> desugarDefConst d
  Lisp.DCap dc -> DCap <$> desugarDefCap dc
  Lisp.DSchema d -> DSchema <$> desugarDefSchema d
  Lisp.DTable d -> DTable <$> desugarDefTable d
  Lisp.DPact d -> DPact <$> desugarDefPact d

-- Todo: Module hashing, either on source or
-- the contents
-- Todo: governance
desugarModule
  :: (MonadDesugar raw reso i m)
  => Lisp.Module i
  -> m (Module ParsedName DesugarType raw i)
desugarModule (Lisp.Module mname mgov extdecls defs _ _ i) = do
  let (imports, blessed, implemented) = splitExts extdecls
  defs' <- locally reCurrModule (const (Just mname)) $ traverse desugarDef (NE.toList defs)
  let mhash = ModuleHash (Hash "placeholder")
      mgov' = BN . BareName <$> mgov
  pure $ Module mname mgov' defs' blessed imports implemented mhash i
  where
  splitExts = split ([], Set.empty, [])
  split (accI, accB, accImp) (h:hs) = case h of
    -- todo: implement bless hashes
    Lisp.ExtBless _ -> split (accI, accB, accImp) hs
    Lisp.ExtImport imp -> split (imp:accI, accB, accImp) hs
    Lisp.ExtImplements mn -> split (accI, accB, mn:accImp) hs
  split (a, b, c) [] = (reverse a, b, reverse c)

-- Todo: Interface hashing, either on source or
-- the contents
desugarInterface
  :: (MonadDesugar raw reso i m)
  => Lisp.Interface i
  -> m (Interface ParsedName DesugarType raw i)
desugarInterface (Lisp.Interface ifn ifdefns _ _ info) = do
  defs' <- traverse desugarIfDef ifdefns
  let mhash = ModuleHash (Hash "placeholder")
  pure $ Interface ifn defs' mhash info

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
    BN bn | Set.member (_bnName bn) currDefns -> Set.singleton (_bnName bn)
          | otherwise -> mempty
    QN (QualifiedName n' mn')
      | Set.member n' currDefns && mn' == currM -> Set.singleton n'
      | otherwise -> mempty
  Lam _ args e _ ->
    let currDefns' = foldl' (\s t -> Set.delete (_argName t) s) currDefns args
    in termSCC currM currDefns' e
  Let arg e1 e2 _ ->
    let currDefns' = Set.delete (_argName arg) currDefns
    in Set.union (termSCC currM currDefns e1) (termSCC currM currDefns' e2)
  App fn apps _ ->
    Set.union (termSCC currM currDefns fn) (foldMap (termSCC currM currDefns) apps)
  Sequence e1 e2 _ -> Set.union (termSCC currM currDefns e1) (termSCC currM currDefns e2)
  Conditional c _ ->
    foldMap (termSCC currM currDefns) c
  Builtin{} -> Set.empty
  Constant{} -> Set.empty
  ListLit v _ -> foldMap (termSCC currM currDefns) v
  Try e1 e2 _ -> Set.union (termSCC currM currDefns e1) (termSCC currM currDefns e2)
  CapabilityForm cf _ -> foldMap (termSCC currM currDefns) cf <> case view capFormName cf of
    BN n | Set.member (_bnName n) currDefns -> Set.singleton (_bnName n)
          | otherwise -> mempty
    QN (QualifiedName n' mn')
      | Set.member n' currDefns && mn' == currM -> Set.singleton n'
      | otherwise -> Set.singleton n'
  DynInvoke m _ _ -> termSCC currM currDefns m
  ObjectLit m _ -> foldMap (termSCC currM currDefns . view _2) m
  Error {} -> Set.empty

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
    BN bn | Set.member (_bnName bn) currDefs -> Set.singleton (_bnName bn)
          | otherwise -> mempty
    QN (QualifiedName n' mn')
      | Set.member n' currDefs && mn' == currM -> Set.singleton n'
      | otherwise -> mempty
  Lisp.TyKeyset -> mempty
  Lisp.TyTime -> mempty
  Lisp.TyPolyList -> mempty
  Lisp.TyPolyObject -> mempty
  Lisp.TyTable pn ->  case pn of
    -- Todo: factor out, repeated in termSCC
    BN bn | Set.member (_bnName bn) currDefs -> Set.singleton (_bnName bn)
          | otherwise -> mempty
    QN (QualifiedName n' mn')
      | Set.member n' currDefs && mn' == currM -> Set.singleton n'
      | otherwise -> mempty

defunSCC
  :: ModuleName
  -> Set Text
  -> Defun ParsedName DesugarType  b i
  -> Set Text
defunSCC mn cd = termSCC mn cd . _dfunTerm

defConstSCC
  :: ModuleName
  -> Set Text
  -> DefConst ParsedName DesugarType  b i
  -> Set Text
defConstSCC mn cd = termSCC mn cd . _dcTerm

-- defCapSCC :: ModuleName -> DefCap Name b i -> Set Text
-- defCapSCC mn = termSCC mn . _dcapTerm

defPactSCC
  :: ModuleName
  -> Set Text
  -> PactStep ParsedName DesugarType b i
  -> Set Text
defPactSCC mn cd = \case
  Step step mSteps -> Set.union (termSCC mn cd step) (stepsSCC mSteps)
  StepWithRollback step rollback mSteps ->
    Set.unions $ stepsSCC mSteps : [termSCC mn cd step, termSCC mn cd rollback]
  where
    stepsSCC :: Maybe [Term ParsedName DesugarType b i] -> Set Text
    stepsSCC = maybe Set.empty (foldMap $ termSCC mn cd)

defSCC
  :: ModuleName
  -> Set Text
  -> Def ParsedName DesugarType b i1
  -> Set Text
defSCC mn cd = \case
  Dfun d -> defunSCC mn cd d
  DConst d -> defConstSCC mn cd d
  DCap dc -> termSCC mn cd (_dcapTerm dc)
  DSchema ds -> foldMap (typeSCC mn cd) ( _dsSchema ds)
  DTable _ -> mempty
  DPact dp -> foldMap (defPactSCC mn cd) (_dpSteps dp)

ifDefSCC
  :: ModuleName
  -> Set Text
  -> IfDef ParsedName DesugarType b i1
  -> Set Text
ifDefSCC mn currDefs = \case
  IfDfun _ -> mempty
  IfDCap _ -> mempty
  IfDConst d -> defConstSCC mn currDefs d

resolveModuleName
  :: (MonadRenamer b i m)
  => ModuleName
  -> i
  -> m (ModuleData b i)
resolveModuleName mn i =
  use (rsLoaded . loModules . at mn) >>= \case
    Just md -> pure md
    Nothing -> trace "@@@@@@" $
      view rePactDb >>= liftIO . (`readModule` mn) >>= \case
      Nothing -> throwDesugarError (NoSuchModule mn) i
      Just md -> case md of
        ModuleData module_ depmap ->
          md <$ loadModule' module_ depmap
        InterfaceData in' depmap ->
          md <$ loadInterface' in' depmap

toFqDep
  :: ModuleName
  -> ModuleHash
  -> Def name ty builtin i
  -> (FullyQualifiedName, Def name ty builtin i)
toFqDep modName mhash def = let
  fqn = FullyQualifiedName modName (defName def) mhash
  in (fqn, def)


-- | Load a module into the environment
loadModule'
  :: MonadState (RenamerState b i) m
  => Module Name Type b i
  -> Map FullyQualifiedName (Def Name Type b i)
  -> m ()
loadModule' module_ deps = do
  let modName = _mName module_
      mhash = _mHash module_
      toDepMap def = (defName def, (NTopLevel modName mhash, defKind def))
      depMap = M.fromList $ toDepMap <$> _mDefs module_
  loadModule module_ deps depMap


-- load and interface into the environment
loadInterface'
  :: MonadState (RenamerState b i) m
  => Interface Name Type b i
  -> Map FullyQualifiedName (Def Name Type b i)
  -> m ()
loadInterface' iface deps = do
  let modName = _ifName iface
      mhash = _ifHash iface
      toDepMap def = (defName def, (NTopLevel modName mhash, defKind def))
      dcDeps = mapMaybe (fmap DConst . preview _IfDConst) (_ifDefns iface)
      dconstDeps = M.fromList $ toDepMap <$> dcDeps
  loadInterface iface deps dconstDeps dcDeps

-- | Load a module and it's constituents into the `Loaded` environment.
-- including the types of the members
loadModule
  :: MonadState (RenamerState b i) m
  => Module Name Type b i
  -> Map FullyQualifiedName (Def Name Type b i)
  -> Map Text (NameKind, DefKind)
  -> m ()
loadModule module_ deps depMap = do
  let modName = _mName module_
  let mhash = _mHash module_
  let memberTerms = M.fromList (toFqDep modName mhash <$> _mDefs module_)
      allDeps = M.union memberTerms deps
  rsLoaded %= over loModules (M.insert modName (ModuleData module_ deps)) . over loAllLoaded (M.union allDeps)
  rsModuleBinds %= M.insert modName depMap
  rsDependencies %= Set.insert modName

-- Load an interface into the `Loaded` environment
-- noting that the only interface names that are "legal" in terms
-- are (For now, while we implement more features) the declared constants.

loadInterface
  :: MonadState (RenamerState b i) m
  => Interface Name Type b i
  -> Map FullyQualifiedName (Def Name Type b i)
  -> Map Text (NameKind, DefKind)
  -> [Def Name Type b i]
  -> m ()
loadInterface iface deps depMap dcDeps = do
  let ifaceName = _ifName iface
      ifhash = _ifHash iface
  let memberTerms = M.fromList (toFqDep ifaceName ifhash <$> dcDeps)
      allDeps = M.union memberTerms deps
  rsLoaded %= over loModules (M.insert ifaceName (InterfaceData iface deps)) . over loAllLoaded (M.union allDeps)
  rsModuleBinds %= M.insert ifaceName depMap
  rsDependencies %= Set.insert ifaceName

-- | Look up a qualified name in the pact db
-- if it's there, great! We will load the module into the scope of
-- `Loaded`, as well as include it in the renamer map
-- Todo: Bare namespace lookup first, then
-- current namespace.
-- Namespace definitions are yet to be supported in core

lookupModuleMember
  :: (MonadRenamer b i m)
 => ModuleName
 -> Text
 -> i
 -> m (Name, DefKind)
lookupModuleMember modName name i = do
  view rePactDb >>= liftIO . (`readModule` modName) >>= \case
    Just m -> case m of
      ModuleData module_ deps ->
        let mhash = _mHash module_
            depMap = M.fromList $ toDepMap mhash <$> _mDefs module_
        in case M.lookup name depMap of
          -- Great! The name exists
          -- This, we must include the module in `Loaded`, as well as propagate its deps and
          -- all loaded members in `loAllLoaded`
          Just (nk, dk) -> do
            loadModule module_ deps depMap
            pure (Name name nk, dk)
          -- Module exists, but it has no such member
          -- Todo: check whether the module name includes a namespace
          -- if it does not, we retry the lookup under the current namespace
          Nothing ->
            throwDesugarError (NoSuchModuleMember modName name) i
      InterfaceData iface deps -> do
        let mhash = _ifHash iface
            dcDeps = mapMaybe (fmap DConst . preview _IfDConst) (_ifDefns iface)
            dconstDeps = M.fromList $ toDepMap mhash <$> dcDeps
        case M.lookup name dconstDeps of
          Just (nk, dk) -> do
            loadInterface iface deps dconstDeps dcDeps
            pure (Name name nk, dk)
          Nothing -> throwDesugarError (NoSuchModuleMember modName name) i
    Nothing -> trace "!!!!!!!" $ throwDesugarError (NoSuchModule modName) i
  where
  toDepMap mhash def = (defName def, (NTopLevel modName mhash, defKind def))


renameType
  :: (MonadRenamer reso i m)
  => i
  -> DesugarType
  -> m Type
renameType i = \case
  Lisp.TyPrim p -> pure (TyPrim p)
  Lisp.TyList ty ->
    TyList <$> renameType i ty
  Lisp.TyModRef tmr ->
    TyModRef tmr <$ resolveModuleName tmr i
  Lisp.TyKeyset -> pure TyGuard
  Lisp.TyObject pn ->
    TyObject <$> resolveSchema pn
  Lisp.TyTable pn ->
    TyTable <$> resolveSchema pn
  Lisp.TyPolyList ->
    throwDesugarError (UnsupportedType "[any]") i
  Lisp.TyPolyObject ->
    throwDesugarError (UnsupportedType "object{any}") i
  Lisp.TyTime ->
    throwDesugarError (UnsupportedType "time") i
  where
  resolveSchema = \case
    BN bn ->
      view reCurrModule >>= \case
        Just currM -> do
          rs <- use rsModuleBinds
          case rs ^? ix currM . ix (_bnName bn) of
            Just (_, DKDefSchema sc) -> pure sc
            _ -> error "no schema with required name - bare"
        _ -> error "schema lives outside a module"
    QN qn -> do
      let currM = _qnModName qn
      rs <- use rsModuleBinds
      case rs ^? ix currM . ix (_qnName qn)  of
        Just (_, DKDefSchema sc) ->
          pure sc
        _ -> error "no schema with required name - qualified"


-- Rename a term (that is part of a module)
-- emitting the list of dependent calls
renameTerm
  :: (MonadDesugar raw reso i m)
  => Term ParsedName DesugarType raw i
  -> m (Term Name Type raw i)
renameTerm (Var n i) = resolveName i n >>= \case
  (n', Just dk)
    | dk `elem` legalVarDefs  -> pure (Var n' i)
    | otherwise ->
      throwDesugarError (InvalidDefInTermVariable (rawParsedName n)) i
    where
    legalVarDefs = [DKDefun, DKDefConst, DKDefTable, DKDefPact]
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
  inEnv m depth =
    over reBinds (M.union m) .
    set reVarDepth depth
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
renameTerm (DynInvoke te t i) =
  DynInvoke <$> renameTerm te <*> pure t <*> pure i
renameTerm (Try e1 e2 i) = do
  Try <$> renameTerm e1 <*> renameTerm e2 <*> pure i
renameTerm (CapabilityForm cf i) =
  view reCurrModule >>= \case
    Just mn -> case view capFormName cf of
      QN qn
        | _qnModName qn == mn -> do
          (n', dk) <- resolveQualified qn i
          when (dk /= DKDefCap) $ throwDesugarError (InvalidCapabilityReference (_qnName qn)) i
          let cf' = set capFormName n' cf
          checkCapForm cf'
          CapabilityForm <$> traverse renameTerm cf' <*> pure i
        | otherwise -> throwDesugarError (CapabilityOutOfScope (_qnName qn) (_qnModName qn)) i
      BN bn -> do
        (n', dk) <- resolveQualified (QualifiedName (_bnName bn) mn) i
        when (dk /= DKDefCap) $ throwDesugarError (InvalidCapabilityReference (_bnName bn)) i
        let cf' = set capFormName n' cf
        checkCapForm cf'
        CapabilityForm <$> traverse renameTerm cf' <*> pure i
    Nothing -> do
      checkCapFormNonModule cf
      let n = view capFormName cf
      (n', declty) <- resolveName i n
      case declty of
        Just DKDefCap -> do
          let cf' = set capFormName n' cf
          CapabilityForm <$> traverse renameTerm cf' <*> pure i
        _ -> throwDesugarError (InvalidCapabilityReference (_nName n')) i
    where
    checkCapFormNonModule = \case
      InstallCapability{} -> pure ()
      WithCapability{} -> throwDesugarError (NotAllowedOutsideModule "with-capability") i
      RequireCapability{} -> throwDesugarError (NotAllowedOutsideModule "require-capability") i
      ComposeCapability{} -> throwDesugarError (NotAllowedOutsideModule "compose-capability") i
      EmitEvent{} -> throwDesugarError (NotAllowedOutsideModule "emit-event") i
      CreateUserGuard{} -> pure ()
    checkCapForm = \case
      WithCapability{} -> enforceNotWithinDefcap i "with-capability"
      InstallCapability{} -> enforceNotWithinDefcap i "install-capability"
      _ -> pure ()
renameTerm (Error e i) = pure (Error e i)
renameTerm (ObjectLit o i) =
  ObjectLit <$> (traverse._2) renameTerm o <*> pure i
-- renameTerm (ObjectOp o i) =
--   ObjectOp <$> traverse renameTerm o <*> pure i

enforceNotWithinDefcap
  :: (MonadRenamer b i m)
  => i
  -> Text
  -> m ()
enforceNotWithinDefcap i form = do
  withinDefCap <- (== Just DKDefCap) <$> view reCurrDef
  when withinDefCap $ throwDesugarError (NotAllowedWithinDefcap form) i

renameDefun
  :: (MonadDesugar raw reso i m)
  => Defun ParsedName DesugarType raw i
  -> m (Defun Name Type raw i)
renameDefun (Defun n args ret term i) = do
  -- Todo: put type variables in scope here, if we want to support polymorphism
  args' <- (traverse.traverse) (renameType i) args
  ret' <- traverse (renameType i) ret
  term' <- local (set reCurrDef (Just DKDefun)) $ renameTerm term
  pure (Defun n args' ret' term' i)

renamePactStep
  :: forall raw reso i m. MonadDesugar raw reso i m
  => PactStep ParsedName DesugarType raw i
  -> m (PactStep Name Type raw i)
renamePactStep = \case
  Step step mSteps ->
    Step <$> renameTerm step <*> mListRename mSteps
  StepWithRollback step rollback mSteps ->
    StepWithRollback <$> renameTerm step <*> renameTerm rollback <*> mListRename mSteps
  where
    mListRename :: Maybe [Term ParsedName DesugarType raw i] -> m (Maybe [Term Name Type raw i])
    mListRename = maybe (pure Nothing) (fmap Just . traverse renameTerm)

renameDefPact
  :: MonadDesugar raw reso i m
  => DefPact ParsedName DesugarType raw i
  -> m (DefPact Name Type raw i)
renameDefPact (DefPact n args mret steps i) = do
  args' <- (traverse.traverse) (renameType i) args
  mret' <- traverse (renameType i) mret
  steps' <- local (set reCurrDef (Just DKDefPact)) $
    traverse renamePactStep steps
  pure (DefPact n args' mret' steps' i)

renameDefSchema
  :: (MonadRenamer reso i m)
  => DefSchema DesugarType i
  -> m (DefSchema Type i)
renameDefSchema (DefSchema dsn dsc i) = do
  dsc' <- traverse (renameType i) dsc
  pure (DefSchema dsn dsc' i)

renameDefTable
  :: (MonadRenamer reso i m)
  => DefTable ParsedName i
  -> m (DefTable Name i)
renameDefTable (DefTable dtn sc i) =
  case sc of
    DesugaredTable dn -> resolveName i dn >>= \case
      (_, Just (DKDefSchema rsc)) -> pure (DefTable dtn (ResolvedTable rsc) i)
      _ -> error "invalid schema"

renameReplDefun
  :: (MonadDesugar raw reso i m)
  => Defun ParsedName DesugarType raw i
  -> m (Defun Name Type raw i)
renameReplDefun (Defun n args ret term i) = do
  -- Todo: put type variables in scope here, if we want to support polymorphism
  let fqn = FullyQualifiedName replModuleName n replModuleHash
  args' <- (traverse.traverse) (renameType i) args
  ret' <- traverse (renameType i) ret
  rsModuleBinds %= M.insertWith (<>) replModuleName (M.singleton n (NTopLevel replModuleName replModuleHash, DKDefun))
  rsLoaded . loToplevel %= M.insert n (fqn, DKDefun)
  term' <- local (set reCurrDef (Just DKDefun)) $ renameTerm term
  pure (Defun n args' ret' term' i)

renameReplDefConst
  :: (MonadDesugar raw reso i m)
  => DefConst ParsedName DesugarType raw i
  -> m (DefConst Name Type raw i)
renameReplDefConst (DefConst n mty term i) = do
  -- Todo: put type variables in scoperhere, if we want to support polymorphism
  let fqn = FullyQualifiedName replModuleName n replModuleHash
  rsModuleBinds %= M.insertWith (<>) replModuleName (M.singleton n (NTopLevel replModuleName replModuleHash, DKDefConst))
  rsLoaded . loToplevel %= M.insert n (fqn, DKDefConst)
  mty' <- traverse (renameType i) mty
  term' <- renameTerm term
  pure (DefConst n mty' term' i)

renameDefConst
  :: (MonadDesugar raw reso i m)
  => DefConst ParsedName DesugarType raw i
  -> m (DefConst Name Type raw i)
renameDefConst (DefConst n mty term i) = do
  -- Todo: put type variables in scope here, if we want to support polymorphism
  mty' <- traverse (renameType i) mty
  term' <- local (set reCurrDef (Just DKDefConst)) $ renameTerm term
  pure (DefConst n mty' term' i)

renameDefCap
  :: (MonadDesugar raw reso i m)
  => DefCap ParsedName DesugarType raw i
  -> m (DefCap Name Type raw i)
renameDefCap (DefCap name arity argtys rtype term meta info) = do
  meta' <- (traverse . traverse) resolveName' meta
  argtys' <- (traverse.traverse) (renameType info) argtys
  rtype' <- traverse (renameType info) rtype
  term' <- local (set reCurrDef (Just DKDefCap)) $ renameTerm term
  pure (DefCap name arity argtys' rtype' term' meta' info)
  where
  resolveName' dn = resolveName info dn >>= \case
    (n, Just DKDefun) -> pure n
    _ -> error "defcap manager function does not refer to a defun"

renameDef
  :: MonadDesugar raw reso i m
  => Def ParsedName DesugarType raw i
  -> m (Def Name Type raw i)
renameDef = \case
  Dfun d -> Dfun <$> renameDefun d
  DConst d -> DConst <$> renameDefConst d
  DCap d -> DCap <$> renameDefCap d
  DSchema d -> DSchema <$> renameDefSchema d
  DTable d -> DTable <$> renameDefTable d
  DPact d -> DPact <$> renameDefPact d

renameIfDef
  :: (MonadDesugar raw reso i m)
  => IfDef ParsedName DesugarType raw i
  -> m (IfDef Name Type raw i)
renameIfDef = \case
  -- Todo: export and use lenses lol
  IfDfun d -> do
    let i = _ifdInfo d
    args' <- (traverse.traverse) (renameType i) (_ifdArgs d)
    rtype' <- traverse (renameType i) (_ifdRType d)
    pure (IfDfun (d{_ifdArgs = args', _ifdRType = rtype'}))
  IfDConst d -> IfDConst <$> renameDefConst d
  IfDCap d -> do
    let i = _ifdcInfo d
    args' <- (traverse.traverse) (renameType i) (_ifdcArgs d)
    rtype' <- traverse (renameType i) (_ifdcRType d)
    pure (IfDCap (d{_ifdcArgs = args', _ifdcRType = rtype'}))

resolveName
  :: (MonadRenamer b i m)
  => i
  -> ParsedName
  -> m (Name, Maybe DefKind)
resolveName i = \case
  BN b -> resolveBare b i
  QN q -> over _2 Just <$> resolveQualified q i

-- not in immediate binds, so it must be in the module
-- Todo: resolve module ref within this model
-- Todo: hierarchical namespace search
resolveBare
  :: (MonadRenamer b i m)
  => BareName
  -> i
  -> m (Name, Maybe DefKind)
resolveBare (BareName bn) i = views reBinds (M.lookup bn) >>= \case
  Just tnk -> case tnk of
    (NBound d, _) -> do
      depth <- view reVarDepth
      pure (Name bn (NBound (depth - d - 1)), Nothing)
    (nk, dk) -> pure (Name bn nk, dk)
  Nothing -> uses (rsLoaded . loToplevel) (M.lookup bn) >>= \case
    Just (fqn, dk) -> pure (Name bn (NTopLevel (_fqModule fqn) (_fqHash fqn)), Just dk)
    Nothing -> trace (show bn)  $ do
      let mn = ModuleName bn Nothing
      resolveModuleName mn i >>= \case
        ModuleData md _ -> do
          let implementeds = view mImplements md
          pure (Name bn (NModRef mn implementeds), Nothing)
        -- todo: error type here
        InterfaceData iface _ ->
          throwDesugarError (InvalidModuleReference (_ifName iface)) i

resolveQualified
  :: (MonadRenamer b i m)
  => QualifiedName
  -> i
  -> m (Name, DefKind)
resolveQualified (QualifiedName qn qmn) i = do
  uses rsModuleBinds (M.lookup qmn) >>= \case
    Just binds -> case M.lookup qn binds of
      Just (nk, dk) -> pure (Name qn nk, dk)
      Nothing ->
        throwDesugarError (NoSuchModuleMember qmn qn) i
    Nothing -> lookupModuleMember qmn qn i

-- | Todo: support imports
-- Todo:
renameModule
  :: (MonadDesugar raw reso i m)
  => Module ParsedName DesugarType raw i
  -> m (Module Name Type raw i)
renameModule (Module mname mgov defs blessed imp implements mhash i) = local (set reCurrModule (Just mname)) $ do
  -- let defMap = M.fromList $ (\d -> (defName d, (NTopLevel mname mhash, defKind d))) <$> defs
  -- let fqns = M.fromList $ (\d -> (defName d, (FullyQualifiedName mname (defName d) mhash, defKind d))) <$> defs
  -- `maybe all of this next section should be in a block laid out by the
  -- `locally reBinds`
  mgov' <- resolveGov mgov
  let defNames = Set.fromList $ fmap defName defs
  let scc = mkScc defNames <$> defs
  defs' <- forM (stronglyConnComp scc) \case
    AcyclicSCC d -> pure d
    CyclicSCC d ->
      -- todo: just in case, match on `d` because it makes no sense for there to be an empty cycle
      -- but all uses of `head` are still scary
      throwDesugarError (RecursionDetected mname (defName <$> d)) (defInfo (head d))
  binds <- view reBinds
  rsModuleBinds %= M.insert mname mempty
  (defs'', _, _) <- over _1 reverse <$> foldlM go ([], Set.empty, binds) defs'
  let fqns = M.fromList $ (\d -> (defName d, (FullyQualifiedName mname (defName d) mhash, defKind d))) <$> defs''
  rsLoaded . loToplevel %= M.union fqns
  traverse_ (checkImplements i mname defs'') implements
  pure (Module mname mgov' defs'' blessed imp implements mhash i)
  where
  -- Our deps are acyclic, so we resolve all names
  go (defns, s, m) defn = do
    when (Set.member (defName defn) s) $ error "duplicate defn name"
    let dn = defName defn
    defn' <- local (set reBinds m) $ renameDef defn
    let depPair = (NTopLevel mname mhash, defKind defn')
    let m' = M.insert dn (over _2 Just depPair) m
    rsModuleBinds . ix mname %= M.insert dn depPair
    pure (defn':defns, Set.insert (defName defn) s, m')

  resolveGov = traverse $ \govName -> case find (\d -> BN (BareName (defName d)) == govName) defs of
    Just (DCap d) -> pure (Name (_dcapName d) (NTopLevel mname mhash))
    Just d -> throwDesugarError (InvalidGovernanceRef (QualifiedName (defName d) mname)) i
    Nothing ->
      -- Todo: could be better error? In this case the governance ref does not exist.
      throwDesugarError (InvalidGovernanceRef (QualifiedName (rawParsedName govName) mname)) i
  mkScc dns def = (def, defName def, Set.toList (defSCC mname dns def))

checkImplements
  :: (MonadRenamer reso i m)
  => i
  -> ModuleName
  -> [Def raw Type b i]
  -> ModuleName
  -> m ()
checkImplements i mn defs ifaceName =
  use (rsLoaded . loModules . at ifaceName) >>= \case
    Just (InterfaceData in' _depmap) ->
      traverse_ checkImplementedMember (_ifDefns in')
    -- Todo: lift into DesugarError (technically name resolution error but this is fine)
    Just _ -> throwDesugarError (NoSuchInterface mn) i
    Nothing -> view rePactDb >>= liftIO . (`readModule` mn) >>= \case
      Just (InterfaceData in' depmap) -> do
        loadInterface' in' depmap
        traverse_ checkImplementedMember (_ifDefns in')
      -- Todo: improve this error, could be "found module, expected interface"
      Just _ -> throwDesugarError (NoSuchInterface mn) i
      Nothing -> throwDesugarError (NoSuchInterface mn) i
  where
  checkImplementedMember = \case
    IfDConst{} -> pure ()
    IfDfun ifd ->
      case find (\df -> _ifdName ifd == defName df) defs of
        Just (Dfun v) ->
          when (_dfunArgs v /= _ifdArgs ifd || _dfunRType v /= _ifdRType ifd) $ error "function args dont match"
        Just _ -> error "not implemented"
        Nothing -> error "not implemented"
    IfDCap ifd ->
      case find (\df -> _ifdcName ifd == defName df) defs of
        Just (DCap v) ->
          when (_dcapArgs v /= _ifdcArgs ifd || _dcapRType v /= _ifdcRType ifd) $ error "function args dont match"
        Just _ -> error "not implemented"
        Nothing -> error "not implemented"

-- | Todo: support imports
--   Todo: support
renameInterface
  :: (MonadDesugar raw reso i m)
  => Interface ParsedName DesugarType raw i
  -> m (Interface Name Type raw i)
renameInterface (Interface ifn defs ih info) = local (set reCurrModule (Just ifn)) $ do
      -- defMap = M.fromList $ (, (NTopLevel ifn ih, DKDefConst)) <$> rawDefNames
      -- fqns = M.fromList $ (\n -> (n, (FullyQualifiedName ifn n ih, DKDefConst))) <$> rawDefNames
  -- `maybe all of this next section should be in a block laid out by the
  -- `locally reBinds`
  -- rsModuleBinds %= M.insert ifn defMap
  -- rsLoaded . loToplevel %= M.union fqns
  let defNames = ifDefName <$> defs
  let scc = mkScc (Set.fromList defNames) <$> defs
  defs' <- forM (stronglyConnComp scc) \case
    AcyclicSCC d -> pure d
    CyclicSCC d ->
      -- todo: just in case, match on `d` because it makes no sense for there to be an empty cycle
      -- but all uses of `head` are still scary
      throwDesugarError (RecursionDetected ifn (ifDefName <$> d)) (ifDefInfo (head d))
  -- defs' <- locally reBinds (M.union (over _2 Just <$> defMap)) $ traverse renameIfDef defs
  binds <- view reBinds
  (defs'', _, _) <- over _1 reverse <$> foldlM go ([], Set.empty, binds) defs'

  pure (Interface ifn defs'' ih info)
  where
  mkScc dns def = (def, ifDefName def, Set.toList (ifDefSCC ifn dns def))
  go (ds, s, m) d = do
    when (Set.member (ifDefName d) s) $ error "duplicate defn name in interface"
    let dn = ifDefName d
    d' <- local (set reBinds m) $ renameIfDef d
    let m' = maybe m (\dk -> M.insert dn (NTopLevel ifn ih, Just dk) m) (ifDefKind d')
    rsModuleBinds . ix ifn %= maybe id (M.insert dn . (NTopLevel ifn ih,)) (ifDefKind d')
    pure (d':ds, Set.insert dn s, m')

runRenamerM
  :: RenamerState b i
  -> RenamerEnv b i
  -> RenamerT m b i a
  -> m (a, RenamerState b i)
runRenamerM st env (RenamerT act) = runReaderT (runStateT act st) env

reStateFromLoaded :: Loaded b i -> RenamerState b i
reStateFromLoaded loaded = RenamerState mbinds loaded Set.empty
  where
  mbind = \case
    ModuleData m _ ->
      let depNames = (\def -> (defName def, (NTopLevel (_mName m) (_mHash m), defKind def))) <$> _mDefs m
      in M.fromList depNames
    InterfaceData iface _ ->
      let depNames = _dcName <$> mapMaybe (preview _IfDConst)  (_ifDefns iface)
      in M.fromList $ (,(NTopLevel (_ifName iface) (_ifHash iface), DKDefConst)) <$> depNames
  mbinds = fmap mbind (_loModules loaded)

loadedBinds :: Loaded b i -> Map Text (NameKind, DefKind)
loadedBinds loaded =
  let f fqn = NTopLevel (_fqModule fqn) (_fqHash fqn)
  in over _1 f <$> _loToplevel loaded

runDesugar'
  :: MonadError (PactError i) m
  => PactDb b i
  -> Loaded b i
  -> RenamerT m b i a
  -> m (DesugarOutput b i a)
runDesugar' pdb loaded act = do
  let reState = reStateFromLoaded loaded
      rTLBinds = loadedBinds loaded
      rEnv = RenamerEnv (over _2 Just <$> rTLBinds) 0 pdb Nothing Nothing
  (renamed, RenamerState _ loaded' deps) <- runRenamerM reState rEnv act
  pure (DesugarOutput renamed loaded' deps)

runDesugarTerm
  :: (MonadError (PactError i) m, MonadIO m, DesugarBuiltin raw)
  => Proxy raw
  -> PactDb reso i
  -> Loaded reso i
  -> Lisp.Expr i
  -> m (DesugarOutput reso i (Term Name Type raw i))
runDesugarTerm _ pdb loaded = runDesugar' pdb loaded  . RenamerT . (desugarLispTerm >=> renameTerm)

runDesugarModule'
  :: (MonadError (PactError i) m, MonadIO m, DesugarBuiltin raw)
  => Proxy raw
  -> PactDb reso i
  -> Loaded reso i
  -> Lisp.Module i
  -> m (DesugarOutput reso i (Module Name Type raw i))
runDesugarModule' _ pdb loaded = runDesugar' pdb loaded . RenamerT . (desugarModule >=> renameModule)

runDesugarInterface
  :: (MonadError (PactError i) m, MonadIO m, DesugarBuiltin raw)
  => Proxy raw
  -> PactDb reso i
  -> Loaded reso i
  -> Lisp.Interface i
  -> m (DesugarOutput reso i (Interface Name Type raw i))
runDesugarInterface _ pdb loaded  = runDesugar' pdb loaded . RenamerT . (desugarInterface >=> renameInterface)

runDesugarReplDefun
  :: (MonadError (PactError i) m, MonadIO m, DesugarBuiltin raw)
  => Proxy raw
  -> PactDb reso i
  -> Loaded reso i
  -> Lisp.Defun i
  -> m (DesugarOutput reso i (Defun Name Type raw i))
runDesugarReplDefun _ pdb loaded =
  runDesugar' pdb loaded
  . local (set reCurrModule (Just replModuleName))
  . RenamerT
  . (desugarDefun >=> renameReplDefun)

runDesugarReplDefConst
  :: (MonadError (PactError i) m, MonadIO m, DesugarBuiltin raw)
  => Proxy raw
  -> PactDb reso i
  -> Loaded reso i
  -> Lisp.DefConst i
  -> m (DesugarOutput reso i (DefConst Name Type raw i))
runDesugarReplDefConst _ pdb loaded =
  runDesugar' pdb loaded
  . local (set reCurrModule (Just replModuleName))
  . RenamerT
  . (desugarDefConst >=> renameReplDefConst)

-- runDesugarModule
--   :: (DesugarTerm term raw i)
--   => Loaded b i
--   -> Lisp.Module term i
--   -> IO (DesugarOutput b i (Module Name TypeVar raw i))
-- runDesugarModule loaded = runDesugarModule' loaded 0

runDesugarTopLevel
  :: (MonadError (PactError i) m, MonadIO m, DesugarBuiltin raw)
  => Proxy raw
  -> PactDb reso i
  -> Loaded reso i
  -> Lisp.TopLevel i
  -> m (DesugarOutput reso i (TopLevel Name Type raw i))
runDesugarTopLevel proxy pdb loaded = \case
  Lisp.TLModule m -> over dsOut TLModule <$> runDesugarModule' proxy pdb loaded m
  Lisp.TLTerm e -> over dsOut TLTerm <$> runDesugarTerm proxy pdb loaded e
  Lisp.TLInterface i -> over dsOut TLInterface <$> runDesugarInterface proxy pdb loaded i


runDesugarReplTopLevel
  :: (MonadError (PactError i) m, MonadIO m, DesugarBuiltin raw)
  => Proxy raw
  -> PactDb reso i
  -> Loaded reso i
  -> Lisp.ReplTopLevel i
  -> m (DesugarOutput reso i (ReplTopLevel Name Type raw i))
runDesugarReplTopLevel proxy pdb loaded = \case
  Lisp.RTLTopLevel m ->
    over dsOut RTLTopLevel <$> runDesugarTopLevel proxy pdb loaded m
  Lisp.RTLDefun de ->
    over dsOut RTLDefun <$> runDesugarReplDefun proxy pdb loaded de
  Lisp.RTLDefConst dc ->
    over dsOut RTLDefConst <$> runDesugarReplDefConst proxy pdb loaded dc
