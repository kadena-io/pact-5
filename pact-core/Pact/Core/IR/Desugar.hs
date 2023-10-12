{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}


module Pact.Core.IR.Desugar
 ( runDesugarTerm
 , runDesugarTopLevel
 , runDesugarReplTopLevel
 , DesugarOutput(..)
 , DesugarBuiltin(..)
 ) where

import Control.Monad ( when, forM, (>=>), unless)
import Control.Monad.Reader
import Control.Monad.State.Strict ( StateT(..), MonadState )
import Control.Monad.Except
import Control.Lens hiding (List)
import Data.Text(Text)
import Data.Map.Strict(Map)
import Data.Maybe(mapMaybe, isJust)
import Data.List(findIndex)
import Data.List.NonEmpty(NonEmpty(..))
import Data.Set(Set)
import Data.Graph(stronglyConnComp, SCC(..))
import Data.Foldable(foldl', foldlM)
import Data.Proxy
import Data.Foldable(find, traverse_, foldrM)
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

instance (MonadError e m) => MonadError e (RenamerT m b i) where
  throwError e = RenamerT (lift (throwError e))
  catchError ma f = RenamerT $ StateT $ \rs ->
      ReaderT $ \env ->
        catchError (runRenamerM rs env ma) (\e -> runRenamerM rs env (f e))


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
  desugarAppArity :: i -> b -> [Term ParsedName DesugarType b i] -> Term ParsedName DesugarType b i

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
  -> [Term name DesugarType builtin info]
  -> Term name DesugarType builtin info
desugarAppArityRaw f i RawEnumerate [e1, e2, e3] =
    App (Builtin (f RawEnumerateStepN) i) ([e1, e2, e3]) i
desugarAppArityRaw f i RawSort [e1, e2] =
  App (Builtin (f RawSortObject) i) [e1, e2] i
desugarAppArityRaw f i RawReadMsg [] =
  App (Builtin (f RawReadMsgDefault) i) [] i
desugarAppArityRaw f i b args =
    App (Builtin (f b) i) args i

instance DesugarBuiltin (ReplBuiltin RawBuiltin) where
  liftRaw :: RawBuiltin -> ReplBuiltin RawBuiltin
  liftRaw = RBuiltinWrap
  reservedNatives = replRawBuiltinMap
  desugarOperator i dsg =
    over termBuiltin RBuiltinWrap $ desugarOperator i dsg
  desugarAppArity i (RBuiltinWrap b) ne =
    desugarAppArityRaw RBuiltinWrap i b ne
  desugarAppArity i (RBuiltinRepl RExpect) ([e1, e2, e3]) | isn't _Lam e3 =
    App (Builtin (RBuiltinRepl RExpect) i) ([e1, e2, suspendTerm e3]) i
  desugarAppArity i (RBuiltinRepl RExpectFailure) [e1, e2] | isn't _Lam e2 =
    App (Builtin (RBuiltinRepl RExpectFailure) i) [e1, suspendTerm e2] i
  desugarAppArity i (RBuiltinRepl RExpectFailure) [e1, e2, e3] | isn't _Lam e2 =
    App (Builtin (RBuiltinRepl RExpectFailureMatch) i) [e1, e2, suspendTerm e3] i
  desugarAppArity i b ne =
    App (Builtin b i) ne i

throwDesugarError :: MonadError (PactError i) m => DesugarError -> i -> m a
throwDesugarError de = throwError . PEDesugarError de

-- Really ugly hack because
-- of inconsistent old prod pact syntax :)))))))
-- pattern HigherOrderApp :: Text -> i -> Text -> i -> [Lisp.Expr i] -> i -> i -> Lisp.Expr i
-- pattern HigherOrderApp fnCaller ci fnCallee fi xs ai unused =
--   Lisp.App (Lisp.Var (BN (BareName fnCaller)) ci)
--     (Lisp.App (Lisp.Var (BN (BareName fnCallee)) unused) [] fi :   xs)
--     ai

desugarLispTerm
  :: forall raw reso i m
  . (MonadDesugar raw reso i m)
  => Lisp.Expr i
  -> m (Term ParsedName DesugarType raw i)
desugarLispTerm = \case
  -- HigherOrderApp fnCaller ci fnCallee fi xs ai _
  --   | fnCaller `elem` specialCallsiteFns -> do
  --     caller <- desugarLispTerm (Lisp.Var (BN (BareName fnCaller)) ci)
  --     callee <- desugarLispTerm $ Lisp.Var (BN (BareName fnCallee)) fi
  --     xs' <- traverse desugarLispTerm xs
  --     pure (App caller (callee:xs') ai)
  --   where
  --   specialCallsiteFns = ["map", "fold", "zip"]
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
  -- Note: this is our "unit arg application" desugaring
  -- This _may not_ stay long term
  -- Lisp.App e [] i ->
  --   App <$> desugarLispTerm e <&> [] <*> pure i
    -- v@Var{} ->
    --   let arg = Constant LUnit i :| []
    --   in App v arg i
    -- v@Builtin{} ->
    --   let arg = Constant LUnit i :| []
    --   in App v arg i
    -- e' -> e'
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
      _ -> error "enforce-one incorrect form"
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
    -- Lisp.RequireCapability pn exs ->
    --   RequireCapability pn <$> traverse desugarLispTerm exs
    -- Lisp.ComposeCapability pn exs ->
    --   ComposeCapability pn <$> traverse desugarLispTerm exs
    -- Lisp.InstallCapability pn exs ->
    --   InstallCapability pn <$> traverse desugarLispTerm exs
    -- Lisp.EmitEvent pn exs ->
    --   EmitEvent pn <$> traverse desugarLispTerm exs
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
  Nullary e' (view termInfo e')

toArg
  :: Lisp.MArg
  -> Arg DesugarType
toArg (Lisp.MArg n mty) = Arg n mty

desugarDefun
  :: (MonadDesugar raw reso i m)
  => Lisp.Defun i
  -> m (Defun ParsedName DesugarType raw i)
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
    Just mn -> do
      let bodyLam = Lam (TLDefun mn defname) args' body' i
      pure $ Defun defname (NE.toList args') mrt bodyLam i
    Nothing -> throwDesugarError (NotAllowedOutsideModule "defun") i



desugarDefConst
  :: (MonadDesugar raw reso i m)
  => Lisp.DefConst i
  -> m (DefConst ParsedName DesugarType raw i)
desugarDefConst (Lisp.DefConst n mty e _ i) = do
  e' <- desugarLispTerm e
  pure $ DefConst n mty e' i

desugarDefMeta
  :: (MonadRenamer b i m)
  => i
  -> [Arg t]
  -> Lisp.DCapMeta
  -> m (DefCapMeta ParsedName)
desugarDefMeta info args = \case
  Lisp.DefEvent -> pure DefEvent
  Lisp.DefManaged marg -> case marg of
    Just (arg, name) ->
        case findIndex ((==) arg . view argName) args of
          Just index' ->
            let dmanaged = DefManagedMeta index' (FQParsed name)
            in pure (DefManaged (Just dmanaged))
          Nothing ->
            throwDesugarError (InvalidManagedArg arg) info
    Nothing -> pure (DefManaged Nothing)

desugarDefCap
  :: (MonadDesugar raw reso info m)
  => Lisp.DefCap info
  -> m (DefCap ParsedName DesugarType raw info)
desugarDefCap (Lisp.DefCap dcn arglist rtype term _docs _model meta i) =
  view reCurrModule >>= \case
    Just _ -> do
      let arglist' = toArg <$> arglist
      term' <- desugarLispTerm term
      meta' <- traverse (desugarDefMeta i arglist') meta
      pure (DefCap dcn (length arglist) arglist' rtype term' meta' i)
    Nothing ->
      throwDesugarError (NotAllowedOutsideModule "defcap") i

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
  _ -> error "unimplemented"

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
  pure $ Module mname mgov defs' blessed imports implemented mhash i
  where
  splitExts = split ([], S.empty, [])
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

desugarUse
  :: (MonadRenamer b i m)
  => i
  -> Import
  -> m Import
desugarUse i imp = imp <$ handleImport i mempty imp

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
    in termSCC currM currDefns' e
  Let arg e1 e2 _ ->
    let currDefns' = S.delete (_argName arg) currDefns
    in S.union (termSCC currM currDefns e1) (termSCC currM currDefns' e2)
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
  Lisp.TyTime -> mempty
  Lisp.TyPolyList -> mempty
  Lisp.TyPolyObject -> mempty
  Lisp.TyTable pn ->  case pn of
    -- Todo: factor out, repeated in termSCC
    TBN bn | S.member (_bnName bn) currDefs -> S.singleton (_bnName bn)
          | otherwise -> mempty
    TQN (QualifiedName n' mn')
      | S.member n' currDefs && mn' == currM -> S.singleton n'
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

defTableSCC
  :: ModuleName
  -> Set Text
  -> DefTable ParsedName info
  -> Set Text
defTableSCC mn cd dt =
  let (DesugaredTable t) = (_dtSchema dt)
  in parsedNameSCC mn cd t

-- defCapSCC :: ModuleName -> DefCap Name b i -> Set Text
-- defCapSCC mn = termSCC mn . _dcapTerm

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

-- Todo: this handles imports, rename?
loadTopLevelMembers
  :: (MonadRenamer b i m)
  => i
  -> Maybe (Set Text)
  -> ModuleData b i
  -> Map Text (NameKind, Maybe DefKind)
  -> m (Map Text (NameKind, Maybe DefKind))
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
        dcDeps = mapMaybe (fmap DConst . preview _IfDConst) (_ifDefns iface)
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
        (rsLoaded . loToplevel) %= (`M.union` (M.restrictKeys loadedDeps st))
        pure (M.union (M.restrictKeys depMap st) binds)
      Nothing -> do
        (rsLoaded . loToplevel) %= (`M.union` loadedDeps)
        pure (M.union depMap binds)


resolveModuleName
  :: (MonadRenamer b i m)
  => ModuleName
  -> i
  -> m (ModuleData b i)
resolveModuleName mn i =
  use (rsLoaded . loModules . at mn) >>= \case
    Just md -> pure md
    Nothing ->
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
  rsDependencies %= S.insert modName

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
  rsDependencies %= S.insert ifaceName

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
    Nothing -> throwDesugarError (NoSuchModule modName) i
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
    TBN bn ->
      view reCurrModule >>= \case
        Just currM -> do
          rs <- use rsModuleBinds
          case rs ^? ix currM . ix (_bnName bn) of
            Just (_, DKDefSchema sc) -> pure sc
            _ -> error "no schema with required name - bare"
        _ -> error "schema lives outside a module"
    TQN qn -> do
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
    legalVarDefs = [DKDefun, DKDefConst, DKDefTable, DKDefCap]
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
          when ((isCapForm cf && dk /= DKDefCap) || (not (isCapForm cf) && dk == DKDefun))
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

    checkCapFormNonModule = \case
      WithCapability{} ->
        throwDesugarError (NotAllowedOutsideModule "with-capability") i
      CreateUserGuard{} -> pure ()

    checkCapForm = \case
      WithCapability{} -> enforceNotWithinDefcap i "with-capability"
      _ -> pure ()
renameTerm (Error e i) = pure (Error e i)
renameTerm (ObjectLit o i) =
  ObjectLit <$> (traverse._2) renameTerm o <*> pure i

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
renameDefTable (DefTable dtn sc i) = do
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
  meta' <- traverse resolveMeta meta
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
  resolveMeta DefEvent = pure DefEvent
  resolveMeta (DefManaged Nothing) = pure (DefManaged Nothing)
  resolveMeta (DefManaged (Just (DefManagedMeta i (FQParsed pn)))) = do
    (name', _) <- resolveName info pn
    fqn <- expectedFree info name'
    pure (DefManaged (Just (DefManagedMeta i (FQName fqn))))

expectedFree
  :: MonadRenamer reso i m
  => i
  -> Name
  -> m FullyQualifiedName
expectedFree i (Name n nk) = case nk of
  NTopLevel mname mh ->
    pure (FullyQualifiedName mname n mh)
  _ -> throwDesugarError (ExpectedFreeVariable n) i


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
  DN dn -> (, Nothing) <$> resolveDynamic i dn

resolveDynamic
  :: (MonadRenamer b i m)
  => i
  -> DynamicName
  -> m (Name)
resolveDynamic i (DynamicName dn dArg) = views reBinds (M.lookup dn) >>= \case
  Just tnk -> case tnk of
    (NBound d, _) -> do
      depth <- view reVarDepth
      let dbjIx = depth - d - 1
          dr = NDynRef (DynamicRef dArg dbjIx)
      pure (Name dn dr)
    _ -> error "dynamic names cannot be unbound"
  Nothing ->
    throwDesugarError (UnboundTermVariable dn) i

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
    Nothing -> do
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
  let defNames = S.fromList $ fmap defName defs
  let scc = mkScc defNames <$> defs
  defs' <- forM (stronglyConnComp scc) \case
    AcyclicSCC d -> pure d
    CyclicSCC d ->
      -- todo: just in case, match on `d` because it makes no sense for there to be an empty cycle
      -- but all uses of `head` are still scary
      throwDesugarError (RecursionDetected mname (defName <$> d)) (defInfo (head d))
  binds <- view reBinds
  bindsWithImports <- handleImports binds imp
  rsModuleBinds %= M.insert mname mempty
  (defs'', _, _) <- over _1 reverse <$> foldlM go ([], S.empty, bindsWithImports) defs'
  let fqns = M.fromList $ (\d -> (defName d, (FullyQualifiedName mname (defName d) mhash, defKind d))) <$> defs''
  rsLoaded . loToplevel %= M.union fqns
  traverse_ (checkImplements i mname defs'') implements
  pure (Module mname mgov' defs'' blessed imp implements mhash i)
  where
  handleImports binds [] = pure binds
  handleImports binds (imp':xs) = do
    binds' <- handleImport i binds imp'
    handleImports binds' xs

  -- Our deps are acyclic, so we resolve all names
  go (defns, s, m) defn = do
    when (S.member (defName defn) s) $ error "duplicate defn name"
    let dn = defName defn
    defn' <- local (set reBinds m) $ renameDef defn
    let dk = defKind defn'
    let depPair = (NTopLevel mname mhash, dk)
    let m' = M.insert dn (over _2 Just depPair) m
        fqn = FullyQualifiedName mname dn mhash
    rsModuleBinds . ix mname %= M.insert dn depPair
    rsLoaded . loToplevel . ix dn .= (fqn, dk)
    pure (defn':defns, S.insert (defName defn) s, m')

  resolveGov = \case
    KeyGov ksn -> pure (KeyGov ksn)
    CapGov (UnresolvedGov govName) ->
      case find (\d -> BN (BareName (defName d)) == govName) defs of
        Just (DCap d) -> do
          let fqn = FullyQualifiedName mname (_dcapName d) mhash
          pure (CapGov (ResolvedGov fqn))
        Just d -> throwDesugarError (InvalidGovernanceRef (QualifiedName (defName d) mname)) i
        Nothing -> throwDesugarError (InvalidGovernanceRef (QualifiedName (rawParsedName govName) mname)) i
  mkScc dns def = (def, defName def, S.toList (defSCC mname dns def))


handleImport
  :: (MonadRenamer b i m)
  => i
  -> Map Text (NameKind, Maybe DefKind)
  -> Import
  -> m (Map Text (NameKind, Maybe DefKind))
handleImport info binds (Import mn mh imported) = do
  mdata <- resolveModuleName mn info
  let imported' = S.fromList <$> imported
      mdhash = view mdModuleHash mdata
  case mh of
    Just modHash -> when (modHash /= mdhash) $ throwDesugarError (InvalidImportModuleHash mn modHash) info
    Nothing -> pure ()
  loadTopLevelMembers info imported' mdata binds

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
  let scc = mkScc (S.fromList defNames) <$> defs
  defs' <- forM (stronglyConnComp scc) \case
    AcyclicSCC d -> pure d
    CyclicSCC d ->
      -- todo: just in case, match on `d` because it makes no sense for there to be an empty cycle
      -- but all uses of `head` are still scary
      throwDesugarError (RecursionDetected ifn (ifDefName <$> d)) (ifDefInfo (head d))
  -- defs' <- locally reBinds (M.union (over _2 Just <$> defMap)) $ traverse renameIfDef defs
  binds <- view reBinds
  (defs'', _, _) <- over _1 reverse <$> foldlM go ([], S.empty, binds) defs'

  pure (Interface ifn defs'' ih info)
  where
  mkScc dns def = (def, ifDefName def, S.toList (ifDefSCC ifn dns def))
  go (ds, s, m) d = do
    when (S.member (ifDefName d) s) $ error "duplicate defn name in interface"
    let dn = ifDefName d
    d' <- local (set reBinds m) $ renameIfDef d
    let m' = maybe m (\dk -> M.insert dn (NTopLevel ifn ih, Just dk) m) (ifDefKind d')
    rsModuleBinds . ix ifn %= maybe id (M.insert dn . (NTopLevel ifn ih,)) (ifDefKind d')
    pure (d':ds, S.insert dn s, m')

runRenamerM
  :: RenamerState b i
  -> RenamerEnv b i
  -> RenamerT m b i a
  -> m (a, RenamerState b i)
runRenamerM st env (RenamerT act) = runReaderT (runStateT act st) env

reStateFromLoaded :: Loaded b i -> RenamerState b i
reStateFromLoaded loaded = RenamerState mbinds loaded S.empty
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
  Lisp.TLUse imp info -> runDesugar' pdb loaded $ (`TLUse` info) <$> desugarUse info imp



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
