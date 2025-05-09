{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Pact.Core.Lint where

-- import Control.Lens
-- import Pact.Core.Capabilities qualified as Core

import Control.Monad.Trans.RWS
import Data.Map (Map)
import Pact.Core.Builtin
import Pact.Core.IR.Term
import Pact.Core.Literal
import Pact.Core.Names
import Pact.Core.Type

-- instance Ord ParsedName where
--   QN ln <= QN rn = ln < rn
--   QN _ <= BN _ = True
--   QN _ <= DN _ = True
--   BN _ <= QN _ = False
--   BN ln <= BN rn = ln < rn
--   BN _ <= DN _ = True
--   DN _ <= QN _ = False
--   DN _ <= BN _ = False
--   DN ln <= DN rn = ln < rn

type CoreTerm = Term Name Type CoreBuiltin

loeb :: (Functor f) => f (f a -> a) -> f a
loeb x = go where go = fmap ($ go) x
{-# INLINE loeb #-}

-- loebM :: (MonadFix m, Traversable t) => t (t a -> m a) -> m (t a)
-- loebM f = mfix $ \a -> (`traverse` f) ($ a)
-- {-# INLINE loebM #-}

data LintingState = LintingState

data LintMessage = LintMessage

data Grant = Grant
  deriving (Eq, Show)

newtype Scope a = Scope
  { -- | A list of all capabilities that have been granted within the lexical
    --   scope. This does _not_ include capabilities that may have been
    --   granted by another function which then called the function defined
    --   within this scope. These would be "dynamic grants" that can only be
    --   fully computed at run-time using symbolic evaluation.
    capGrants :: [Grant]
  }
  -- \| A list of bindings, corresponding to the de Bruijn indices that refer
  --   to them within the lexical scope.
  -- , localBindings :: [a]

  deriving
    ( Show,
      Functor,
      Foldable,
      Traversable
    )

instance Eq (Scope a) where
  Scope lgs == Scope rgs = lgs == rgs

instance Semigroup (Scope a) where
  Scope lgs <> Scope rgs = Scope (lgs <> rgs)

instance Monoid (Scope a) where
  mempty = Scope []

-- | Pact has two kinds of scoping:
--
--   - Lexical scoping is where a term defined by either an enclosing let or
--     an adjacent defun. Thus, a reference to "foo" represents a value that
--     we can examine during analysis to determine the implications of that
--     value in a given context.
--
--   - Dynamic scoping is where a term is defined only at runtime, such as
--     when binding a name to a value from a database. We can analyze such
--     scenarios by assuming a set of the worst possible values during
--     symbolic evaluation.
data Scopes m a
  = Scopes
  { lexicalScopes :: [Scope a],
    dynamicScopes :: [m (Scope a)]
  }

instance Show (Scopes m a) where
  show (Scopes m a) =
    "Scopes: " <> show m <> ", and " <> show (length a) <> " with-scopes"

instance Semigroup (Scopes m a) where
  Scopes ls lw <> Scopes rs rw = Scopes (ls <> rs) (lw <> rw)

instance Monoid (Scopes m a) where
  mempty = emptyScopes

emptyScopes :: Scopes m a
emptyScopes = Scopes mempty mempty

class Scoped a m | m -> a where
  askScopes :: m (Scopes m a)
  clearScopes :: m r -> m r
  pushScopes :: Scopes m a -> m r -> m r
  hasGrant :: Grant -> m Bool
  lookupVar :: Int -> m (Maybe a)

withScopes ::
  (Scoped a m) =>
  Scopes m a ->
  m r ->
  m r
withScopes scopes = clearScopes . pushScopes scopes

pushScope ::
  (Scoped a m) =>
  Scope a ->
  m r ->
  m r
pushScope scope = pushScopes $ Scopes [scope] mempty

type Bindings = Map ParsedName

data SourcePos
  = SourcePos
  { -- | Name of source file
    getSourceName :: FilePath,
    -- | Line number
    getSourceLine :: !Int,
    -- | Column number
    getSourceColumn :: !Int
  }
  deriving
    ( Eq,
      Ord,
      Read,
      Show
    )

type PositionSet = Bindings SourcePos

-- analyze ::
--   forall m a.
--   (Monad m, Scoped a m) =>
--   Scopes m a ->
--   [([ParsedName], SourcePos, m a)] ->
--   m (Bindings (m a), Bindings SourcePos)
-- analyze scopes bnds = do
--   (scope :: Bindings (m a), p) <- foldM insert mempty bnds
--   res :: Bindings (m a) <-
--     -- t :: Bindings
--     -- m :: m
--     -- a :: m a
--     loebM (fmap encapsulate scope :: Bindings (Bindings (m a) -> m (m a))) ::
--       m (Bindings (m a))
--   pure (res, p)
--   where
--     insert ::
--       (Bindings (m a), PositionSet) ->
--       ([ParsedName], SourcePos, m a) ->
--       m (Bindings (m a), PositionSet)
--     insert (_m, _p) (_path, _pos, _value) = undefined

--     encapsulate :: m a -> (Scope a -> m a)
--     encapsulate f attrs = withScopes scopes $ pushScope attrs f

-- jww (2025-05-09): Need to check the type of the variable
lintTerm ::
  (Monad m) =>
  CoreTerm i ->
  RWST (Scopes m (CoreTerm i)) [LintMessage] () m ()
lintTerm = \case
  Var (Name _name kind) _ ->
    case kind of
      NBound _idx -> undefined -- local
      NTopLevel _modName _modHash -> undefined -- global
      NModRef _modName _modNames -> undefined -- global
      NDynRef _dyn -> undefined -- global
  Nullary body _ -> lintTerm body
  Lam _ body _ -> lintTerm body
  Let _ value body _ -> do
    lintTerm value -- jww (2025-05-07): implement lexical scoping
    lintTerm body
  App func args _ -> do
    mapM_ lintTerm args
    lintTerm func
  BuiltinForm _bf _ ->
    undefined
  Builtin b _ ->
    case b of
      CoreInsert ->
        tell [LintMessage]
      CoreUpdate ->
        tell [LintMessage]
      CoreWrite ->
        tell [LintMessage]
      _ ->
        pure ()
  Constant lit _ -> case lit of
    LString _str -> undefined
    LInteger _int -> undefined
    LDecimal _dec -> undefined
    LUnit -> undefined
    LBool _bool -> undefined
  Sequence t1 t2 _ -> do
    lintTerm t1
    lintTerm t2
  ListLit xs _ ->
    mapM_ lintTerm xs
  ObjectLit _obj _ ->
    undefined -- mapM lintTerm obj
  InlineValue _v _ ->
    undefined

-- Try t1 t2 _ ->
--   lintTerm e t1 $ \t1' ->
--     lintTerm e t2 $ \t2' ->
--       withEqTypes (reflectTerm t1') (reflectTerm t2') $
--         k $
--           Try t1' t2'
-- Conditional bf _ -> case bf of
--   CAnd o1 o2 ->
--     lintTerm e o1 $ \(o1' :: LTerm e1 t1) ->
--       lintTerm e o2 $ \(o2' :: LTerm e2 t2) ->
--         withEqTypes (reflectTerm o1') SSTBool $
--           withEqTypes (reflectTerm o2') SSTBool $
--             k $
--               Conditional (LCAnd o1' o2')
--   COr o1 o2 ->
--     lintTerm e o1 $ \(o1' :: LTerm e1 t1) ->
--       lintTerm e o2 $ \(o2' :: LTerm e2 t2) ->
--         withEqTypes (reflectTerm o1') SSTBool $
--           withEqTypes (reflectTerm o2') SSTBool $
--             k $
--               Conditional (LCOr o1' o2')
--   CIf obool otrue ofalse ->
--     lintTerm e obool $ \(obool' :: LTerm e1 t1) ->
--       withEqTypes SSTBool (reflectTerm obool') $
--         lintTerm e otrue $ \(otrue' :: LTerm e2 t2) ->
--           lintTerm e ofalse $ \(ofalse' :: LTerm e3 t3) ->
--             withEqTypes (reflectTerm otrue') (reflectTerm ofalse') $
--               k $
--                 Conditional (LCIf obool' otrue' ofalse')
--   CEnforceOne msg bs ->
--     lintTerm e msg $ \(msg' :: LTerm e1 t1) ->
--       withEqTypes (reflectTerm msg') SSTStr $ do
--         bs' <-
--           mapM
--             ( \x -> lintTerm e x $ \(x' :: LTerm e2 t2) ->
--                 withEqTypes (reflectTerm x') SSTBool $
--                   pure x'
--             )
--             bs
--         k $ Conditional (LCEnforceOne msg' bs')
--   CEnforce b msg ->
--     lintTerm e b $ \(b' :: LTerm e1 t1) ->
--       lintTerm e msg $ \(msg' :: LTerm e2 t2) ->
--         withEqTypes (reflectTerm b') SSTBool $
--           withEqTypes (reflectTerm msg') SSTStr $
--             k $
--               Conditional (LCEnforce b' msg')
-- CapabilityForm cf _ ->
--   case cf of
--     WithCapability c body ->
--       lintTerm e c $ \c' ->
--         case reflectTerm c' of
--           ct@(SSTCapToken _nm) ->
--             lintTerm e body $ \body' ->
--               withEqTypes ct (reflectTerm body') $
--                 k $
--                   CapabilityForm $
--                     WithCapability c' body'
--           _ -> fail "WithCapability passed a non-capability"
--     CreateUserGuard n _args -> case n of
--       Right _name ->
--         fail "lintTerm: NYI CreateUserGuard"
--       _ -> fail "CreateUserGuard given index or non-bare name"
