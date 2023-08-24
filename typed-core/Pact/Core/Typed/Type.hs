{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveTraversable #-}


module Pact.Core.Typed.Type where

import Data.List.NonEmpty(NonEmpty)
import Data.Map.Strict(Map)

import Pact.Core.Literal
import Pact.Core.Type(PrimType(..), BuiltinTC)
import Pact.Core.Names
import Pact.Core.Pretty

import qualified Pact.Core.Type as CoreType

data Type n
  = TyVar n
  -- ^ type variables
  | TyPrim PrimType
  -- ^ Built-in types
  | TyFun (Type n) (Type n)
  -- ^ Type n
  | TyList (Type n)
  -- ^ List aka [a]
  | TyModRef ModuleName
  -- ^ Module references
  -- | TyObject Schema
  -- ^ Objects
  | TyForall (NonEmpty n) (Type n)
  -- | TyTable Schema
  -- ^ Tables
  deriving (Eq, Show, Functor, Foldable, Traversable)

liftCoreType :: CoreType.Type -> Type n
liftCoreType = \case
  CoreType.TyPrim p -> TyPrim p
  CoreType.TyList t -> TyList (liftCoreType t)
  CoreType.TyModRef m -> TyModRef m
  CoreType.TyTable{} -> error "tytable"
  CoreType.TyObject{} -> error "tyobject"

pattern TyInt :: Type n
pattern TyInt = TyPrim PrimInt

pattern TyDecimal :: Type n
pattern TyDecimal = TyPrim PrimDecimal

-- pattern TyTime :: Type n
-- pattern TyTime = TyPrim PrimTime

pattern TyBool :: Type n
pattern TyBool = TyPrim PrimBool

pattern TyString :: Type n
pattern TyString = TyPrim PrimString

pattern TyUnit :: Type n
pattern TyUnit = TyPrim PrimUnit

pattern TyGuard :: Type n
pattern TyGuard = TyPrim PrimGuard

pattern (:~>) :: Type n -> Type n -> Type n
pattern (:~>) l r = TyFun l r

typeOfLit :: Literal -> Type n
typeOfLit = \case
  LString{} -> TyString
  LInteger{} -> TyDecimal
  LUnit -> TyUnit
  LDecimal{} -> TyDecimal
  LBool{} -> TyBool

instance Pretty (Type n) where
  pretty _ty = error "todo"

-- Note, no superclasses, for now
data Pred tv
  = Pred BuiltinTC (Type tv)
  deriving (Show, Eq, Functor, Foldable, Traversable)

data TypeScheme tv =
  TypeScheme [tv] [Pred tv]  (Type tv)
  deriving Show


newtype Schema tv
  = Schema { _schema :: Map Field (Type tv) }
  deriving (Eq, Show)

tyFunToArgList :: Type n -> ([Type n], Type n)
tyFunToArgList (TyFun l r) =
  unFun [l] r
  where
  unFun args (TyFun l' r') = unFun (l':args) r'
  unFun args ret = (reverse args, ret)
tyFunToArgList r = ([], r)
