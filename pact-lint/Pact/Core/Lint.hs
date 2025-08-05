{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Pact.Core.Lint
  ( lintModule,
    lintTerm,
    LintMessage(..),
    Scoped(..),
    Scope(..),
    Scopes(..),
    emptyScopes,
    withScopes,
    pushScope,
  )
where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.RWS
import Data.Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Pact.Core.Builtin
import Pact.Core.IR.Term
import Pact.Core.IR.Eval.CEK.Types
import Pact.Core.Literal
import Pact.Core.Names
import Pact.Core.Pretty
import Pact.Core.SpanInfo

data LintMessage
  = LintMissingCapability SpanInfo Text
  | LintInsertCall SpanInfo
  | LintUpdateCall SpanInfo
  | LintWriteCall SpanInfo
  | LintDeleteCall SpanInfo
  deriving (Eq, Show)

data Grant
  = Grant Text
  deriving (Eq, Show)

-- | A scope represents the set of variables that are in scope at a given
-- point in the program. This includes both lexical and dynamic scoping.
data Scope a
  = Scope [a]
  -- ^ The set of variables that are in the lexical scope.
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

-- jww (2025-05-09): Need to check the type of the variable
lintTerm ::
  (Monad m, HasSpanInfo i) =>
  Bindings (CoreDef i) ->
  CoreTerm i ->
  RWST (Scopes m (CoreTerm i)) [LintMessage] () m ()
lintTerm _globals = go
  where
    go = \case
      Var (Name _name kind) _ ->
        case kind of
          NBound _idx -> undefined -- local
          NTopLevel _modName _modHash -> undefined -- global
          NModRef _modName _modNames -> undefined -- global
          NDynRef _dyn -> undefined -- global
      Nullary body _ -> go body
      Lam _ body _ -> go body
      Let _ value body _ -> do
        go value -- jww (2025-05-07): implement lexical scoping
        go body
      App func args _ -> do
        mapM_ go args
        go func
      BuiltinForm bf _ -> do
        -- Check if this is a database modification operation
        case bf of
          -- Look for specific database modification operations
          DBWrite -> do
            -- Check if we have an active capability granting write access
            hasWriteGrant <- hasGrant $ Grant "write"
            unless hasWriteGrant $ do
              let pos = i ^. spanInfo
              tell [LintMissingCapability pos "write"]
          DBInsert -> do
            -- Check if we have an active capability granting insert access
            hasInsertGrant <- hasGrant $ Grant "insert"
            unless hasInsertGrant $ do
              let pos = i ^. spanInfo
              tell [LintMissingCapability pos "insert"]
          DBUpdate -> do
            -- Check if we have an active capability granting update access
            hasUpdateGrant <- hasGrant $ Grant "update"
            unless hasUpdateGrant $ do
              let pos = i ^. spanInfo
              tell [LintMissingCapability pos "update"]
          DBDelete -> do
            -- Check if we have an active capability granting delete access
            hasDeleteGrant <- hasGrant $ Grant "delete"
            unless hasDeleteGrant $ do
              let pos = i ^. spanInfo
              tell [LintMissingCapability pos "delete"]
          _ -> pure ()
      Builtin b i ->
        case b of
          CoreInsert -> do
            -- Check if we have an active capability granting insert access
            hasInsertGrant <- hasGrant $ Grant "insert"
            unless hasInsertGrant $ do
              let pos = i ^. spanInfo
              tell [LintMissingCapability pos "insert"]
            tell [LintInsertCall pos]
          CoreUpdate -> do
            -- Check if we have an active capability granting update access
            hasUpdateGrant <- hasGrant $ Grant "update"
            unless hasUpdateGrant $ do
              let pos = i ^. spanInfo
              tell [LintMissingCapability pos "update"]
            tell [LintUpdateCall pos]
          CoreWrite -> do
            -- Check if we have an active capability granting write access
            hasWriteGrant <- hasGrant $ Grant "write"
            unless hasWriteGrant $ do
              let pos = i ^. spanInfo
              tell [LintMissingCapability pos "write"]
            tell [LintWriteCall pos]
          CoreDelete -> do
            -- Check if we have an active capability granting delete access
            hasDeleteGrant <- hasGrant $ Grant "delete"
            unless hasDeleteGrant $ do
              let pos = i ^. spanInfo
              tell [LintMissingCapability pos "delete"]
            tell [LintDeleteCall pos]
          _ ->
            pure ()
        where
          pos = i ^. spanInfo
      Constant lit _ -> case lit of
        LString _str -> undefined
        LInteger _int -> undefined
        LDecimal _dec -> undefined
        LUnit -> undefined
        LBool _bool -> undefined
      Sequence t1 t2 _ -> do
        go t1
        go t2
      ListLit xs _ ->
        mapM_ go xs
      ObjectLit _obj _ ->
        undefined -- mapM go obj
      InlineValue _v _ ->
        undefined

lintModule ::
  (Monad m, HasSpanInfo i) =>
  CoreModule i ->
  RWST (Scopes m (CoreTerm i)) [LintMessage] () m ()
lintModule m = mapM_ (go (mapDefs (_mDefs m))) (_mDefs m)
  where
    go defs (Dfun d) = lintTerm defs (_dfunTerm d)
    go defs (DCap c) = lintTerm defs (_dcapTerm c)
    go defs (DPact d) = void $ traverse step (_dpSteps d)
      where
        step (Step t) = lintTerm defs t
        step (StepWithRollback t r) = do
          lintTerm defs t
          lintTerm defs r
        step _ = pure ()
    go _ _ = pure ()

    mapDefs :: [CoreDef i] -> Bindings (CoreDef i)
    mapDefs _ = undefined

-- Try t1 t2 _ ->
--   go e t1 $ \t1' ->
--     go e t2 $ \t2' ->
--       withEqTypes (reflectTerm t1') (reflectTerm t2') $
--         k $
--           Try t1' t2'
-- Conditional bf _ -> case bf of
--   CAnd o1 o2 ->
--     go e o1 $ \(o1' :: LTerm e1 t1) ->
--       go e o2 $ \(o2' :: LTerm e2 t2) ->
--         withEqTypes (reflectTerm o1') SSTBool $
--           withEqTypes (reflectTerm o2') SSTBool $
--             k $
--               Conditional (LCAnd o1' o2')
--   COr o1 o2 ->
--     go e o1 $ \(o1' :: LTerm e1 t1) ->
--       go e o2 $ \(o2' :: LTerm e2 t2) ->
--         withEqTypes (reflectTerm o1') SSTBool $
--           withEqTypes (reflectTerm o2') SSTBool $
--             k $
--               Conditional (LCOr o1' o2')
--   CIf obool otrue ofalse ->
--     go e obool $ \(obool' :: LTerm e1 t1) ->
--       withEqTypes SSTBool (reflectTerm obool') $
--         go e otrue $ \(otrue' :: LTerm e2 t2) ->
--           go e ofalse $ \(ofalse' :: LTerm e3 t3) ->
--             withEqTypes (reflectTerm otrue') (reflectTerm ofalse') $
--               k $
--                 Conditional (LCIf obool' otrue' ofalse')
--   CEnforceOne msg bs ->
--     go e msg $ \(msg' :: LTerm e1 t1) ->
--       withEqTypes (reflectTerm msg') SSTStr $ do
--         bs' <-
--           mapM
--             ( \x -> go e x $ \(x' :: LTerm e2 t2) ->
--                 withEqTypes (reflectTerm x') SSTBool $
--                   pure x'
--             )
--             bs
--         k $ Conditional (LCEnforceOne msg' bs')
--   CEnforce b msg ->
--     go e b $ \(b' :: LTerm e1 t1) ->
--       go e msg $ \(msg' :: LTerm e2 t2) ->
--         withEqTypes (reflectTerm b') SSTBool $
--           withEqTypes (reflectTerm msg') SSTStr $
--             k $
--               Conditional (LCEnforce b' msg')
-- CapabilityForm cf _ ->
--   case cf of
--     WithCapability c body ->
--       go e c $ \c' ->
--         case reflectTerm c' of
--           ct@(SSTCapToken _nm) ->
--             go e body $ \body' ->
--               withEqTypes ct (reflectTerm body') $
--                 k $
--                   CapabilityForm $
--                     WithCapability c' body'
--           _ -> fail "WithCapability passed a non-capability"
--     CreateUserGuard n _args -> case n of
--       Right _name ->
--         fail "lintTerm: NYI CreateUserGuard"
--       _ -> fail "CreateUserGuard given index or non-bare name"
