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


module Pact.Core.IR.Desugar
 ( runDesugarTermLisp
 , runDesugarTopLevelLisp
 , runDesugarReplTopLevel
 , DesugarOutput(..)
 , DesugarBuiltin(..)
 ) where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Lens hiding (List,ix)
import Data.Text(Text)
import Data.Map.Strict(Map)
import Data.Maybe(mapMaybe)
import Data.List.NonEmpty(NonEmpty(..))
import Data.Set(Set)
import Data.Graph(stronglyConnComp, SCC(..))
import Data.Proxy
import Data.Foldable(find, traverse_, foldrM)
import qualified Data.Map.Strict as Map
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set

import Pact.Core.Builtin
import Pact.Core.Names
import Pact.Core.Type
import Pact.Core.Literal
import Pact.Core.Hash
import Pact.Core.Persistence
import Pact.Core.Capabilities
import Pact.Core.Errors
import Pact.Core.IR.Term

import qualified Pact.Core.Syntax.Lisp.ParseTree as Lisp
import qualified Pact.Core.Untyped.Term as Term

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

data RenamerEnv m b i
  = RenamerEnv
  { _reBinds :: Map Text NameKind
  , _reVarDepth :: DeBruijn
  , _rePactDb :: PactDb m b i
  , _reCurrModule :: Maybe ModuleName
  }
makeLenses ''RenamerEnv

data RenamerState b i
  = RenamerState
  { _rsModuleBinds :: Map ModuleName (Map Text NameKind)
  , _rsLoaded :: Loaded b i
  , _rsDependencies :: Set ModuleName }

makeLenses ''RenamerState

newtype RenamerT m b i a =
  RenamerT (StateT (RenamerState b i) (ReaderT (RenamerEnv m b i) m) a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (RenamerEnv m b i)
    , MonadState (RenamerState b i))
  via (StateT (RenamerState b i) (ReaderT (RenamerEnv m b i) m))

data DesugarOutput b i a
  = DesugarOutput
  { _dsOut :: a
  , _dsLoaded :: Loaded b i
  , _dsDeps :: Set ModuleName
  } deriving (Show, Functor)

dsOut :: Lens (DesugarOutput b i a) (DesugarOutput b i a') a a'
dsOut f (DesugarOutput a l d) =
  f a <&> \a' -> DesugarOutput a' l d

class DesugarBuiltin b where
  reservedNatives :: Map Text b
  desugarOperator :: i -> Lisp.Operator -> Term ParsedName b i
  desugarAppArity :: i -> b -> NonEmpty (Term ParsedName b i) -> Term ParsedName b i

instance DesugarBuiltin RawBuiltin where
  reservedNatives = rawBuiltinMap
  desugarOperator info = \case
    Lisp.AddOp ->
      Builtin RawAdd info
    Lisp.SubOp ->
      Builtin RawSub info
    Lisp.MultOp ->
      Builtin RawMultiply info
    Lisp.DivOp ->
      Builtin RawDivide info
    Lisp.GTOp ->
      Builtin RawGT info
    Lisp.GEQOp ->
      Builtin RawGEQ info
    Lisp.LTOp ->
      Builtin RawLT info
    Lisp.LEQOp ->
      Builtin RawLEQ info
    Lisp.EQOp ->
      Builtin RawEq info
    Lisp.NEQOp ->
      Builtin RawNeq info
    Lisp.BitAndOp ->
      Builtin RawBitwiseAnd info
    Lisp.BitOrOp ->
      Builtin RawBitwiseOr info
    Lisp.BitComplementOp ->
      Builtin RawBitwiseFlip info
    -- Manual eta expansion for and as well as Or
    Lisp.AndOp -> let
      arg1 = "#andArg1"
      arg2 = "#andArg2"
      in Lam ((arg1, Just TyBool) :| [(arg2, Just TyBool)]) (Conditional (CAnd (Var (BN (BareName arg1)) info) (Var (BN (BareName arg2)) info)) info) info
    Lisp.OrOp -> let
      arg1 = "#orArg1"
      arg2 = "#orArg2"
      in Lam ((arg1, Just TyBool) :| [(arg2, Just TyBool)]) (Conditional (COr (Var (BN (BareName arg1)) info) (Var (BN (BareName arg2)) info)) info) info
    Lisp.PowOp -> Builtin RawPow info
  -- Todo:
  -- Builtins of known arity differences we are yet to support:
  --  str-to-int
  --  read (db, later milestone)
  --  select (db, later milestone)
  --  floor
  --  log
  desugarAppArity i raw ne = desugarAppArityRaw id i raw ne

desugarAppArityRaw
  :: (RawBuiltin -> builtin)
  -> info
  -> RawBuiltin
  -> NonEmpty (Term name builtin info)
  -> Term name builtin info
desugarAppArityRaw f i RawEnumerate (e1 :| [e2, e3]) =
    App (Builtin (f RawEnumerateStepN) i) (e1 :| [e2, e3]) i
desugarAppArityRaw f i b args =
    App (Builtin (f b) i) args i

instance DesugarBuiltin (ReplBuiltin RawBuiltin) where
  reservedNatives = replRawBuiltinMap
  desugarOperator i dsg =
    over termBuiltin RBuiltinWrap $ desugarOperator i dsg
  desugarAppArity i (RBuiltinWrap b) ne =
    desugarAppArityRaw RBuiltinWrap i b ne
  desugarAppArity i RExpect (e1 :| [e2, e3]) | isn't _Lam e3 =
    App (Builtin RExpect i) (e1 :| [e2, suspendTerm e3]) i
  desugarAppArity i RExpectFailure (e1 :| [e2]) | isn't _Lam e2 =
    App (Builtin RExpectFailure i) (e1 :| [suspendTerm e2]) i
  desugarAppArity i b ne =
    App (Builtin b i) ne i

throwDesugarError :: MonadError (PactError i) m => DesugarError -> i -> RenamerT m b i a
throwDesugarError de = liftRenamerT . throwError . PEDesugarError de

-- type DesugarTerm term b i = (?desugarTerm :: term -> Term ParsedName Text b i)
desugarLispTerm
  :: forall raw reso i m
  . (DesugarBuiltin raw, MonadError (PactError i) m)
  => Lisp.Expr i
  -> RenamerT m reso i (Term ParsedName raw i)
desugarLispTerm = \case
  Lisp.Var (BN n) i | isReservedNative (_bnName n) ->
    pure (Builtin (reservedNatives Map.! _bnName n) i)
  Lisp.Var n i -> pure (Var n i)
  Lisp.Block nel i -> do
    nel' <- traverse desugarLispTerm nel
    pure $ foldr (\a b -> Sequence a b i) (NE.last nel') (NE.init nel')
  Lisp.LetIn binders expr i -> do
    expr' <- desugarLispTerm expr
    foldrM (binderToLet i) expr' binders
  Lisp.Lam [] body i -> let
    n = "#unitLamArg"
    nty = Just TyUnit
    in Lam (pure (n, nty)) <$> desugarLispTerm body <*> pure i
  Lisp.Lam (x:xs) body i -> do
    let nsts = x :| xs
        (ns, ts) = NE.unzip nsts
    ts' <- (traverse.traverse) (desugarType i) ts
    body' <- desugarLispTerm body
    pure (Lam (NE.zip ns ts') body' i)
  Lisp.Suspend body i -> do
    let n = "#unitLamArg"
        nty = Just TyUnit
    body' <- desugarLispTerm body
    pure (Lam (pure (n, nty)) body' i)
  Lisp.If e1 e2 e3 i -> Conditional <$>
     (CIf <$> desugarLispTerm e1 <*> desugarLispTerm e2 <*> desugarLispTerm e3) <*> pure i
  -- Note: this is our "unit arg application" desugaring
  -- This _may not_ stay long term
  Lisp.App e [] i -> desugarLispTerm e <&> \case
    v@Var{} ->
      let arg = Constant LUnit i :| []
      in App v arg i
    e' -> e'
  Lisp.App (Lisp.Operator o oi) [e1, e2] i -> case o of
    Lisp.AndOp ->
      Conditional <$> (CAnd <$> desugarLispTerm e1 <*> desugarLispTerm e2) <*> pure i
    Lisp.OrOp ->
      Conditional <$> (COr <$> desugarLispTerm e1 <*> desugarLispTerm e2) <*> pure i
    _ -> do
      let o' = desugarOperator oi o
      e1' <- desugarLispTerm e1
      e2' <- desugarLispTerm e2
      pure (App o' (e1' :| [e2']) i)
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
  _ -> error "implement rest (parser covering whole syntax"
  where
  binderToLet i (Lisp.Binder n mty expr) term = do
    expr' <- desugarLispTerm expr
    -- term' <- desugarLispTerm expr
    mty' <- traverse (desugarType i) mty
    pure $ Let n mty' expr' term i
  isReservedNative n =
    Map.member n (reservedNatives @raw)

suspendTerm
  :: Term ParsedName builtin info
  -> Term ParsedName builtin info
suspendTerm e' =
  Lam (("#suspendarg", Just TyUnit) :| []) e' (view termInfo e')

-- Todo: remove this monkey patch
enforceArg
  :: MonadError (PactError i) m
  => i
  -> Lisp.MArg
  -> RenamerT m b i Lisp.Arg
enforceArg i (Lisp.MArg n mty) = case mty of
  Just ty ->
    pure (Lisp.Arg n ty)
  Nothing -> throwDesugarError (UnannotatedType n) i


-- desugarDefun :: (DesugarBuiltin b) => Lisp.Defun i -> Defun ParsedName b i
desugarDefun
  :: (DesugarBuiltin builtin, MonadError (PactError info) m)
  => Lisp.Defun info
  -> RenamerT m b info (Defun ParsedName builtin info)
desugarDefun (Lisp.Defun defname [] (Just rt) body _ _ i) = do
  rt' <- desugarType i rt
  let dfnType = TyFun TyUnit rt'
  body' <- desugarLispTerm body
  let bodyLam = Lam ((defname, Just TyUnit) :| []) body' i
  pure $ Defun defname dfnType bodyLam i
desugarDefun (Lisp.Defun defname (marg:margs) (Just rt) body _ _ i) = do
  arg <- enforceArg i marg
  args <- traverse (enforceArg i) margs
  let neArgs = arg :| args
  rt' <- desugarType i rt
  neArgs' <- traverse (desugarType i . Lisp._argType) neArgs
  let dfnType = foldr TyFun rt' neArgs'
  let lamArgs = NE.zipWith (\(Lisp.Arg n _) ty -> (n, Just ty)) neArgs neArgs'
  body' <- desugarLispTerm body
  let bodyLam = Lam lamArgs body' i
  pure $ Defun defname dfnType bodyLam i
desugarDefun _ = error "functions require annotations"

desugarDefConst :: (DesugarBuiltin builtin, MonadError (PactError info) m) => Lisp.DefConst info -> RenamerT m b info (DefConst ParsedName builtin info)
desugarDefConst (Lisp.DefConst n mty e _ i) = do
  mty' <- traverse (desugarType i) mty
  e' <- desugarLispTerm e
  pure $ DefConst n mty' e' i

desugarDefMeta :: Lisp.DCapMeta -> DefCapMeta ParsedName
desugarDefMeta = \case
  Lisp.DefEvent -> DefEvent
  Lisp.DefManaged marg ->
    DefManaged marg

desugarDefCap
  :: (MonadError (PactError info) m, DesugarBuiltin builtin)
  => Lisp.DefCap info
  -> RenamerT m b info (DefCap ParsedName builtin info)
desugarDefCap (Lisp.DefCap dcn margs mrtype term _docs _model meta i) = do
  args <- traverse (enforceArg i) margs
  rtype <- maybe (error "boom") (desugarType i) mrtype
  argTys <- traverse (desugarType i . Lisp._argType) args
  term' <- desugarLispTerm term
  let meta' = fmap desugarDefMeta meta
      dct = foldr TyFun rtype argTys
  pure (DefCap dcn dct term' meta' i)


desugarIfDef
  :: (MonadError (PactError info) m, DesugarBuiltin builtin)
  => Lisp.IfDef info
  -> RenamerT m b info (IfDef ParsedName builtin info)
desugarIfDef = \case
  Lisp.IfDfun (Lisp.IfDefun n margs (Just rty) _ _ i) -> IfDfun <$> case margs of
    [] -> do
      rty' <- desugarType i rty
      let dty = TyUnit :~> rty'
      pure $ IfDefun n dty i
    _ -> do
      args <- traverse (enforceArg i) margs
      argsTys <- traverse (desugarType i . Lisp._argType) args
      rty' <- desugarType i rty
      let dty = foldr TyFun rty' argsTys
      pure $ IfDefun n dty i
  Lisp.IfDConst dc -> IfDConst <$> desugarDefConst dc
  _ -> error "unimplemented"

desugarDef :: (DesugarBuiltin builtin, MonadError (PactError info) m) => Lisp.Def info -> RenamerT m b info (Def ParsedName builtin info)
desugarDef = \case
  Lisp.Dfun d -> Dfun <$> desugarDefun d
  Lisp.DConst d -> DConst <$> desugarDefConst d
  Lisp.DCap dc -> DCap <$> desugarDefCap dc
  _ -> error "unimplemented"

-- Todo: Module hashing, either on
-- Todo: governance
desugarModule
  :: (DesugarBuiltin builtin, MonadError (PactError info) m)
  => Lisp.Module info
  -> RenamerT m b info (Module ParsedName builtin info)
desugarModule (Lisp.Module mname mgov extdecls defs _ _) = do
  let (imports, blessed, implemented) = splitExts extdecls
  defs' <- traverse desugarDef (NE.toList defs)
  let mhash = ModuleHash (Hash "placeholder")
      mgov' = BN . BareName <$> mgov
  pure $ Module mname mgov' defs' blessed imports implemented mhash
  where
  splitExts = split ([], Set.empty, [])
  split (accI, accB, accImp) (h:hs) = case h of
    -- todo: implement bless hashes
    Lisp.ExtBless _ -> split (accI, accB, accImp) hs
    Lisp.ExtImport i -> split (i:accI, accB, accImp) hs
    Lisp.ExtImplements mn -> split (accI, accB, mn:accImp) hs
  split (a, b, c) [] = (reverse a, b, reverse c)

desugarInterface
  :: (MonadError (PactError info) m, DesugarBuiltin builtin)
  => Lisp.Interface info
  -> RenamerT m b info (Interface ParsedName builtin info)
desugarInterface (Lisp.Interface ifn ifdefns _ _) = do
  defs' <- traverse desugarIfDef ifdefns
  let mhash = ModuleHash (Hash "placeholder")
  pure $ Interface ifn defs' mhash

desugarType
  :: MonadError (PactError i) m
  => i
  -> Lisp.Type
  -> RenamerT m b i (Type n)
desugarType i = \case
  Lisp.TyPrim p -> pure (TyPrim p)
  Lisp.TyGuard -> pure TyGuard
  Lisp.TyList t ->
    TyList <$> desugarType i t
  Lisp.TyModRef mr ->
    pure (TyModRef mr)
  Lisp.TyKeyset -> pure TyGuard
  Lisp.TyPolyList ->
    throwDesugarError (UnsupportedType "[any]") i
  Lisp.TyObject _ ->
    throwDesugarError (UnsupportedType "object{schema}") i
  Lisp.TyPolyObject ->
    throwDesugarError (UnsupportedType "object{any}") i
  Lisp.TyTime ->
    throwDesugarError (UnsupportedType "time") i


-----------------------------------------------------------
-- Renaming
-----------------------------------------------------------

-- Strongly connected components in term
termSCC
  :: ModuleName
  -> Term Name b1 i1
  -> Set Text
termSCC currM = conn
  where
  conn = \case
    Var n _ -> case _nKind n of
      NTopLevel m _ | m == currM ->
        Set.singleton (_nName n)
      _ -> Set.empty
    Lam _ e _ -> conn e
    Let _ _ e1 e2 _ -> Set.union (conn e1) (conn e2)
    App fn apps _ ->
      Set.union (conn fn) (foldMap conn apps)
    Sequence e1 e2 _ -> Set.union (conn e1) (conn e2)
    Conditional c _ ->
      foldMap conn c
    Builtin{} -> Set.empty
    Constant{} -> Set.empty
    ListLit v _ -> foldMap conn v
    Try e1 e2 _ -> Set.union (conn e1) (conn e2)
    CapabilityForm cf _ ->  foldMap conn cf <> case _nKind (view capFormName cf) of
      NTopLevel mn _ | mn == currM ->
        Set.singleton (_nName (view capFormName cf))
      _ -> mempty
    DynInvoke m _ _ -> conn m
    Error {} -> Set.empty


defunSCC :: ModuleName -> Defun Name b i -> Set Text
defunSCC mn = termSCC mn . _dfunTerm

defConstSCC :: ModuleName -> DefConst Name b i -> Set Text
defConstSCC mn = termSCC mn . _dcTerm

-- defCapSCC :: ModuleName -> DefCap Name b i -> Set Text
-- defCapSCC mn = termSCC mn . _dcapTerm

defSCC :: ModuleName -> Def Name b i1 -> Set Text
defSCC mn = \case
  Dfun d -> defunSCC mn d
  DConst d -> defConstSCC mn d
  DCap dc -> termSCC mn (_dcapTerm dc)

ifDefSCC :: ModuleName -> IfDef Name b i1 -> Set Text
ifDefSCC mn = \case
  IfDfun _ -> mempty
  IfDConst d -> defConstSCC mn d

liftRenamerT :: Monad m => m a -> RenamerT m cb ci a
liftRenamerT ma = RenamerT (lift (lift ma))

resolveModuleName
  :: MonadError (PactError i) m
  => ModuleName
  -> i
  -> RenamerT m b i (ModuleData b i)
resolveModuleName mn i =
  use (rsLoaded . loModules . at mn) >>= \case
    Just md -> pure md
    Nothing ->
      view rePactDb >>= liftRenamerT . (`_readModule` mn) >>= \case
      Nothing -> throwDesugarError (NoSuchModule mn) i
      Just md -> case md of
        ModuleData module_ depmap ->
          md <$ loadModule' module_ depmap
        InterfaceData in' depmap ->
          md <$ loadInterface' in' depmap

-- lookupModule
--   :: MonadError (PactError i) m
--   => ModuleName
--   -> i
--   -> RenamerT m b i (ModuleData b i)
-- lookupModule mn i = view rePactDb >>= liftRenamerT . (`_readModule` mn) >>= \case
--    Nothing -> throwDesugarError (NoSuchModule mn) i
--    Just md -> case md of
--      ModuleData module_ depmap ->
--       md <$ loadModule' module_ depmap
--      InterfaceData in' depmap ->
--       md <$ loadInterface' in' depmap

toFqDep
  :: ModuleName
  -> ModuleHash
  -> Term.Def name builtin i
  -> (FullyQualifiedName, Term.Def name builtin i)
toFqDep modName mhash def = let
  fqn = FullyQualifiedName modName (Term.defName def) mhash
  in (fqn, def)

loadModule'
  :: Monad m
  => Term.EvalModule builtin info
  -> Map FullyQualifiedName (Term.EvalDef builtin info)
  -> RenamerT m builtin info ()
loadModule' module_ deps = do
  let modName = Term._mName module_
      mhash = Term._mHash module_
      toDepMap def = (Term.defName def, NTopLevel modName mhash)
      depMap = Map.fromList $ toDepMap <$> Term._mDefs module_
  loadModule module_ deps depMap

loadInterface'
  :: Monad m
  => Term.EvalInterface builtin info
  -> Map FullyQualifiedName (Term.EvalDef builtin info)
  -> RenamerT m builtin info ()
loadInterface' iface deps = do
  let modName = Term._ifName iface
      mhash = Term._ifHash iface
      toDepMap def = (Term.defName def, NTopLevel modName mhash)
      dcDeps = mapMaybe (fmap Term.DConst . preview Term._IfDConst) (Term._ifDefns iface)
      dconstDeps = Map.fromList $ toDepMap <$> dcDeps
  loadInterface iface deps dconstDeps dcDeps


-- | Load a module and it's constituents into the `Loaded` environment.
-- including the types of the members
loadModule
  :: Monad m
  => Term.EvalModule builtin info
  -> Map FullyQualifiedName (Term.EvalDef builtin info)
  -> Map Text NameKind
  -> RenamerT m builtin info ()
loadModule module_ deps depMap = do
  let modName = Term._mName module_
  let mhash = Term._mHash module_
  let memberTerms = Map.fromList (toFqDep modName mhash <$> Term._mDefs module_)
      allDeps = Map.union memberTerms deps
      allTyped = Term.defType <$> memberTerms
  rsLoaded %= over loModules (Map.insert modName (ModuleData module_ deps)) . over loAllLoaded (Map.union allDeps)
  rsLoaded . loAllTyped <>= allTyped
  rsModuleBinds %= Map.insert modName depMap
  rsDependencies %= Set.insert modName

-- Load an interface into the `Loaded` environment
-- noting that the only interface names that are "legal" in terms
-- are (For now, while we implement more features) the declared constants.
loadInterface
  :: Monad m
  => Term.EvalInterface builtin info
  -> Map FullyQualifiedName (Term.EvalDef builtin info)
  -> Map Text NameKind
  -> [Term.Def Name builtin info]
  -> RenamerT m builtin info ()
loadInterface iface deps depMap dcDeps = do
  let ifName = Term._ifName iface
      ifhash = Term._ifHash iface
  let memberTerms = Map.fromList (toFqDep ifName ifhash <$> dcDeps)
      allDeps = Map.union memberTerms deps
      allTyped = Term.defType <$> memberTerms
  rsLoaded %= over loModules (Map.insert ifName (InterfaceData iface deps)) . over loAllLoaded (Map.union allDeps)
  rsLoaded . loAllTyped <>= allTyped
  rsModuleBinds %= Map.insert ifName depMap
  rsDependencies %= Set.insert ifName

-- | Look up a qualified name in the pact db
-- if it's there, great! We will load the module into the scope of
-- `Loaded`, as well as include it in the renamer map
-- Todo: Bare namespace lookup first, then
-- current namespace.
-- Namespace definitions are yet to be supported in core
lookupModuleMember
  :: (MonadError (PactError i) m)
  => ModuleName
  -> Text
  -> i
  -> RenamerT m b i Name
lookupModuleMember modName name i = do
  view rePactDb >>= liftRenamerT . (`_readModule` modName) >>= \case
    Just m -> case m of
      ModuleData module_ deps ->
        let mhash = Term._mHash module_
            depMap = Map.fromList $ toDepMap mhash <$> Term._mDefs module_
        in case Map.lookup name depMap of
          -- Great! The name exists
          -- This, we must include the module in `Loaded`, as well as propagate its deps and
          -- all loaded members in `loAllLoaded`
          Just irtl -> do
            loadModule module_ deps depMap
            pure (Name name irtl)
          -- Module exists, but it has no such member
          -- Todo: check whether the module name includes a namespace
          -- if it does not, we retry the lookup under the current namespace
          Nothing ->
            throwDesugarError (NoSuchModuleMember modName name) i
      InterfaceData iface deps -> do
        let mhash = Term._ifHash iface
            dcDeps = mapMaybe (fmap Term.DConst . preview Term._IfDConst) (Term._ifDefns iface)
            dconstDeps = Map.fromList $ toDepMap mhash <$> dcDeps
        case Map.lookup name dconstDeps of
          Just nk -> do
            loadInterface iface deps dconstDeps dcDeps
            pure (Name name nk)
          Nothing -> throwDesugarError (NoSuchModuleMember modName name) i
    Nothing -> throwDesugarError (NoSuchModule modName) i
  where
  toDepMap mhash def = (Term.defName def, NTopLevel modName mhash)

resolveTyModRef
  :: MonadError (PactError i) m
  => i
  -> Type n
  -> RenamerT m b i (Type n)
resolveTyModRef i = transformM \case
  TyModRef tmr ->
     TyModRef tmr <$ resolveModuleName tmr i
  a -> pure a

-- Rename a term (that is part of a module)
-- emitting the list of dependent calls
renameTerm
  :: (MonadError (PactError i) m)
  => Term ParsedName b' i
  -> RenamerT m b i (Term Name b' i)
renameTerm (Var n i) = (`Var` i) <$> resolveName i n
-- Todo: what happens when an argument is shadowed?
renameTerm (Lam nsts body i) = do
  depth <- view reVarDepth
  let (pns, ts) = NE.unzip nsts
      len = fromIntegral (NE.length nsts)
      newDepth = depth + len
      ixs = NE.fromList [depth .. newDepth - 1]
      -- ns = rawParsedName <$> pns
  let m = Map.fromList $ NE.toList $ NE.zip pns (NBound <$> ixs)
      -- ns' = NE.zipWith (\ix n -> Name n (NBound ix)) ixs ns
  term' <- local (inEnv m newDepth) (renameTerm body)
  _ <- (traversed . _Just) (resolveTyModRef i) ts
  pure (Lam (NE.zip pns ts) term' i)
  where
  inEnv m depth =
    over reBinds (Map.union m) .
    set reVarDepth depth
renameTerm (Let name mt e1 e2 i) = do
  depth <- view reVarDepth
  let inEnv = over reVarDepth succ .
              over reBinds (Map.insert name (NBound depth))
  e1' <- renameTerm e1
  _ <- _Just (resolveTyModRef i) mt
  e2' <- local inEnv (renameTerm e2)
  pure (Let name mt e1' e2' i)
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
          n' <- resolveQualified qn i
          let cf' = set capFormName n' cf
          CapabilityForm <$> traverse renameTerm cf' <*> pure i
        | otherwise -> error "invariant borken"
      BN bn -> do
        n' <- resolveQualified (QualifiedName (_bnName bn) mn) i
        let cf' = set capFormName n' cf
        CapabilityForm <$> traverse renameTerm cf' <*> pure i
    Nothing -> error "capability used outside module"
renameTerm (Error e i) = pure (Error e i)
-- renameTerm (ObjectLit o i) =
--   ObjectLit <$> traverse renameTerm o <*> pure i
-- renameTerm (ObjectOp o i) =
--   ObjectOp <$> traverse renameTerm o <*> pure i

renameDefun
  :: (MonadError (PactError i) m)
  => Defun ParsedName b' i
  -> RenamerT m b i (Defun Name b' i)
renameDefun (Defun n dty term i) = do
  -- Todo: put type variables in scope here, if we want to support polymorphism
  term' <- renameTerm term
  pure (Defun n dty term' i)

renameReplDefun
  :: (MonadError (PactError i) m)
  => Defun ParsedName b' i
  -> RenamerT m b i (Defun Name b' i)
renameReplDefun (Defun n dty term i) = do
  -- Todo: put type variables in scope here, if we want to support polymorphism
  let fqn = FullyQualifiedName replModuleName n replModuleHash
  rsModuleBinds %= Map.insertWith (<>) replModuleName (Map.singleton n (NTopLevel replModuleName replModuleHash))
  rsLoaded . loToplevel %= Map.insert n fqn
  term' <- renameTerm term
  pure (Defun n dty term' i)

renameReplDefConst
  :: (MonadError (PactError i) m)
  => DefConst ParsedName b' i
  -> RenamerT m b i (DefConst Name b' i)
renameReplDefConst (DefConst n mty term i) = do
  -- Todo: put type variables in scoperhere, if we want to support polymorphism
  let fqn = FullyQualifiedName replModuleName n replModuleHash
  rsModuleBinds %= Map.insertWith (<>) replModuleName (Map.singleton n (NTopLevel replModuleName replModuleHash))
  rsLoaded . loToplevel %= Map.insert n fqn
  term' <- renameTerm term
  pure (DefConst n mty term' i)

renameDefConst
  :: (MonadError (PactError i) m)
  => DefConst ParsedName b' i
  -> RenamerT m b i (DefConst Name b' i)
renameDefConst (DefConst n mty term i) = do
  -- Todo: put type variables in scope here, if we want to support polymorphism
  term' <- renameTerm term
  pure (DefConst n mty term' i)

renameDefCap
  :: (MonadError (PactError info) m)
  => DefCap ParsedName raw info
  -> RenamerT m reso info (DefCap Name raw info)
renameDefCap (DefCap name ty term meta i) = do
  meta' <- (traverse . traverse) (resolveName i) meta
  term' <- renameTerm term
  pure (DefCap name ty term' meta' i)

renameDef
  :: (MonadError (PactError i) m)
  => Def ParsedName b' i
  -> RenamerT m b i (Def Name b' i)
renameDef = \case
  Dfun d -> Dfun <$> renameDefun d
  DConst d -> DConst <$> renameDefConst d
  DCap d -> DCap <$> renameDefCap d

renameIfDef
  :: (MonadError (PactError i) m)
  => IfDef ParsedName b' i
  -> RenamerT m b i (IfDef Name b' i)
renameIfDef = \case
  IfDfun d -> pure (IfDfun d)
  IfDConst d -> IfDConst <$> renameDefConst d

resolveName
  :: (MonadError (PactError i) m)
  => i
  -> ParsedName
  -> RenamerT m b i Name
resolveName i = \case
  BN b -> resolveBare b i
  QN q -> resolveQualified q i

-- not in immediate binds, so it must be in the module
-- Todo: resolve module ref within this model
-- Todo: hierarchical namespace search
resolveBare
  :: (MonadError (PactError i) m)
  => BareName
  -> i
  -> RenamerT m b i Name
resolveBare (BareName bn) i = views reBinds (Map.lookup bn) >>= \case
  Just nk -> case nk of
    NBound d -> do
      depth <- view reVarDepth
      pure (Name bn (NBound (depth - d - 1)))
    _ -> pure (Name bn nk)
  Nothing -> uses (rsLoaded . loToplevel) (Map.lookup bn) >>= \case
    Just fqn -> pure (Name bn (NTopLevel (_fqModule fqn) (_fqHash fqn)))
    Nothing -> do
      let mn = ModuleName bn Nothing
      resolveModuleName mn i >>= \case
        ModuleData md _ -> do
          let implementeds = view Term.mImplemented md
          pure (Name bn (NModRef mn implementeds))
        -- todo: error type here
        InterfaceData{} -> error "CANNOT USE INTERFACE AS MODULE REF"

resolveQualified
  :: (MonadError (PactError i) m)
  => QualifiedName
  -> i
  -> RenamerT m b i Name
resolveQualified (QualifiedName qn qmn) i = do
  uses rsModuleBinds (Map.lookup qmn) >>= \case
    Just binds -> case Map.lookup qn binds of
      Just irnk -> pure (Name qn irnk)
      Nothing ->
        throwDesugarError (NoSuchModuleMember qmn qn) i
    Nothing -> lookupModuleMember qmn qn i

-- | Todo: support imports
-- Todo:
renameModule
  :: (MonadError (PactError i) m)
  => Module ParsedName b' i
  -> RenamerT m b i (Module Name b' i)
renameModule (Module mname mgov defs blessed imp implements mhash) = do
  let rawDefNames = defName <$> defs
      defMap = Map.fromList $ (, NTopLevel mname mhash) <$> rawDefNames
      fqns = Map.fromList $ (\n -> (n, FullyQualifiedName mname n mhash)) <$> rawDefNames
  -- `maybe all of this next section should be in a block laid out by the
  -- `locally reBinds`
  mgov' <- resolveGov mgov
  rsModuleBinds %= Map.insert mname defMap
  rsLoaded . loToplevel %= Map.union fqns
  defs' <- locally reBinds (Map.union defMap) . local (set reCurrModule (Just mname)) $ traverse renameDef defs
  let scc = mkScc <$> defs'
  defs'' <- forM (stronglyConnComp scc) \case
    AcyclicSCC d -> pure d
    CyclicSCC d ->
      -- todo: just in case, match on `d` because it makes no sense for there to be an empty cycle
      -- but all uses of `head` are still scary
      throwDesugarError (RecursionDetected mname (defName <$> d)) (defInfo (head d))
  traverse_ (checkImplements mname defs) implements
  pure (Module mname mgov' defs'' blessed imp implements mhash)
  where
  resolveGov = traverse $ \govName -> case find (\d -> BN (BareName (defName d)) == govName) defs of
    Just (DCap d) -> pure (Name (_dcapName d) (NTopLevel mname mhash))
    _ -> error "no such ovuvue"

  mkScc def = (def, defName def, Set.toList (defSCC mname def))

checkImplements
  :: (Monad m, Foldable t)
  => ModuleName
  -> t (Def name builtin info) -> ModuleName -> RenamerT m b i ()
checkImplements mn defs ifName =
  use (rsLoaded . loModules . at ifName) >>= \case
    Just (InterfaceData in' _depmap) ->
      traverse_ checkImplementedMember (Term._ifDefns in')
    -- Todo: lift into DesugarError (technically name resolution error but this is fine)
    Just _ -> error "found module, expected to implement interface"
    Nothing -> view rePactDb >>= liftRenamerT . (`_readModule` mn) >>= \case
      Just (InterfaceData in' depmap) -> do
        loadInterface' in' depmap
        traverse_ checkImplementedMember (Term._ifDefns in')
      Just _ -> error "found module, expected to implement interface"
      Nothing -> error "no such interface"
  where
  checkImplementedMember = \case
    Term.IfDConst{} -> pure ()
    Term.IfDfun ifd ->
      case find (\df -> Term._ifdName ifd == defName df) defs of
        Just (Dfun v) ->
          when (_dfunType v /= Term._ifdType ifd) $ error "BOOM"
        Just _ -> error "not implemented"
        Nothing -> error "not implemented"

-- | Todo: support imports
-- Todo:
renameInterface
  :: (MonadError (PactError i) m)
  => Interface ParsedName b' i
  -> RenamerT m b i (Interface Name b' i)
renameInterface (Interface ifn defs ih) = do
  let dcs = mapMaybe (preview _IfDConst) defs
      rawDefNames = _dcName <$> dcs
      defMap = Map.fromList $ (, NTopLevel ifn ih) <$> rawDefNames
      fqns = Map.fromList $ (\n -> (n, FullyQualifiedName ifn n ih)) <$> rawDefNames
  -- `maybe all of this next section should be in a block laid out by the
  -- `locally reBinds`
  rsModuleBinds %= Map.insert ifn defMap
  rsLoaded . loToplevel %= Map.union fqns
  defs' <- locally reBinds (Map.union defMap) $ traverse renameIfDef defs
  let scc = mkScc <$> defs'
  defs'' <- forM (stronglyConnComp scc) \case
    AcyclicSCC d -> pure d
    CyclicSCC d ->
      -- todo: just in case, match on `d` because it makes no sense for there to be an empty cycle
      -- but all uses of `head` are still scary
      throwDesugarError (RecursionDetected ifn (ifDefName <$> d)) (ifDefInfo (head d))
  -- mgov' <- locally reBinds (Map.union defMap) $ traverse (resolveBareName' . rawParsedName) mgov
  pure (Interface ifn defs'' ih)
  where
  mkScc def = (def, ifDefName def, Set.toList (ifDefSCC ifn def))

runRenamerM
  :: RenamerState b i
  -> RenamerEnv m b i
  -> RenamerT m b i a
  -> m (a, RenamerState b i)
runRenamerM st env (RenamerT act) = runReaderT (runStateT act st) env

reStateFromLoaded :: Loaded b i -> RenamerState b i
reStateFromLoaded loaded = RenamerState mbinds loaded Set.empty
  where
  mbind = \case
    ModuleData m _ ->
      let depNames = Term.defName <$> Term._mDefs m
      in Map.fromList $ (,NTopLevel (Term._mName m) (Term._mHash m)) <$> depNames
    InterfaceData iface _ ->
      let depNames = Term._dcName <$> mapMaybe (preview Term._IfDConst)  (Term._ifDefns iface)
      in Map.fromList $ (,NTopLevel (Term._ifName iface) (Term._ifHash iface)) <$> depNames
  mbinds = fmap mbind (_loModules loaded)

loadedBinds :: Loaded b i -> Map Text NameKind
loadedBinds loaded =
  let f fqn = NTopLevel (_fqModule fqn) (_fqHash fqn)
  in f <$> _loToplevel loaded

runDesugar'
  :: MonadError (PactError i) m
  => PactDb m b i
  -> Loaded b i
  -> RenamerT m b i a
  -> m (DesugarOutput b i a)
runDesugar' pdb loaded act = do
  let reState = reStateFromLoaded loaded
      rTLBinds = loadedBinds loaded
      rEnv = RenamerEnv rTLBinds 0 pdb Nothing
  (renamed, RenamerState _ loaded' deps) <- runRenamerM reState rEnv act
  pure (DesugarOutput renamed loaded' deps)

runDesugarTerm
  :: (MonadError (PactError i) m, DesugarBuiltin raw)
  => Proxy raw
  -> PactDb m reso i
  -> Loaded reso i
  -> Lisp.Expr i
  -> m (DesugarOutput reso i (Term Name raw i))
runDesugarTerm _ pdb loaded = runDesugar' pdb loaded  . (desugarLispTerm >=> renameTerm)

runDesugarModule'
  :: (MonadError (PactError i) m, DesugarBuiltin raw)
  => Proxy raw
  -> PactDb m reso i
  -> Loaded reso i
  -> Lisp.Module i
  -> m (DesugarOutput reso i (Module Name raw i))
runDesugarModule' _ pdb loaded = runDesugar' pdb loaded . (desugarModule >=> renameModule)

runDesugarInterface
  :: (MonadError (PactError i) m, DesugarBuiltin raw)
  => Proxy raw
  -> PactDb m reso i
  -> Loaded reso i
  -> Lisp.Interface i
  -> m (DesugarOutput reso i (Interface Name raw i))
runDesugarInterface _ pdb loaded  = runDesugar' pdb loaded . (desugarInterface >=> renameInterface)

runDesugarReplDefun
  :: (MonadError (PactError i) m, DesugarBuiltin raw)
  => Proxy raw
  -> PactDb m reso i
  -> Loaded reso i
  -> Lisp.Defun i
  -> m (DesugarOutput reso i (Defun Name raw i))
runDesugarReplDefun _ pdb loaded = runDesugar' pdb loaded . (desugarDefun >=> renameReplDefun)

runDesugarReplDefConst
  :: (MonadError (PactError i) m, DesugarBuiltin raw)
  => Proxy raw
  -> PactDb m reso i
  -> Loaded reso i
  -> Lisp.DefConst i
  -> m (DesugarOutput reso i (DefConst Name raw i))
runDesugarReplDefConst _ pdb loaded = runDesugar' pdb loaded . (desugarDefConst >=> renameReplDefConst)

-- runDesugarModule
--   :: (DesugarTerm term b' i)
--   => Loaded b i
--   -> Lisp.Module term i
--   -> IO (DesugarOutput b i (Module Name TypeVar b' i))
-- runDesugarModule loaded = runDesugarModule' loaded 0

runDesugarTopLevel
  :: (MonadError (PactError i) m, DesugarBuiltin raw)
  => Proxy raw
  -> PactDb m reso i
  -> Loaded reso i
  -> Lisp.TopLevel i
  -> m (DesugarOutput reso i (TopLevel Name raw i))
runDesugarTopLevel proxy pdb loaded = \case
  Lisp.TLModule m -> over dsOut TLModule <$> runDesugarModule' proxy pdb loaded m
  Lisp.TLTerm e -> over dsOut TLTerm <$> runDesugarTerm proxy pdb loaded e
  Lisp.TLInterface i -> over dsOut TLInterface <$> runDesugarInterface proxy pdb loaded i


runDesugarReplTopLevel
  :: (DesugarBuiltin raw, MonadError (PactError i) m)
  => Proxy raw
  -> PactDb m reso i
  -> Loaded reso i
  -> Lisp.ReplTopLevel i
  -> m (DesugarOutput reso i (ReplTopLevel Name raw i))
runDesugarReplTopLevel proxy pdb loaded = \case
  Lisp.RTLModule m ->
    over dsOut RTLModule <$> runDesugarModule' proxy pdb loaded m
  Lisp.RTLDefun de ->
    over dsOut RTLDefun <$> runDesugarReplDefun proxy pdb loaded de
  Lisp.RTLDefConst dc ->
    over dsOut RTLDefConst <$> runDesugarReplDefConst proxy pdb loaded dc
  Lisp.RTLTerm ex ->
    over dsOut RTLTerm <$> runDesugarTerm proxy pdb loaded ex
  Lisp.RTLInterface iface ->
    over dsOut RTLInterface <$> runDesugarInterface proxy pdb loaded iface

runDesugarTermLisp
  :: forall raw reso i m
  .  (DesugarBuiltin raw, MonadError (PactError i) m)
  => Proxy raw
  -> PactDb m reso i
  -> Loaded reso i
  -> Lisp.Expr i
  -> m (DesugarOutput reso i (Term Name raw i))
runDesugarTermLisp = runDesugarTerm

runDesugarTopLevelLisp
  :: forall raw reso i m
  . (DesugarBuiltin raw, MonadError (PactError i) m)
  => Proxy raw
  -> PactDb m reso i
  -> Loaded reso i
  -> Lisp.TopLevel i
  -> m (DesugarOutput reso i (TopLevel Name raw i))
runDesugarTopLevelLisp = runDesugarTopLevel
