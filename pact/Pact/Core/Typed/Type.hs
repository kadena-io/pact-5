{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}

module Pact.Core.Typed.Type where

import Control.Lens
import Control.DeepSeq
import Data.Void
import Data.Set(Set)
import Data.Text(Text)
import Data.Map.Strict(Map)
import GHC.Generics

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Pact.Core.Literal
import Pact.Core.Names
import Pact.Core.Pretty(Pretty(..), (<+>))
import qualified Pact.Core.Type as CoreType

import qualified Pact.Core.Pretty as Pretty

data PrimType =
  PrimInt |
  PrimDecimal |
  PrimBool |
  PrimString |
  PrimGuard |
  PrimTime |
  PrimUnit
  deriving (Eq,Ord,Show, Enum, Bounded, Generic)

renderPrimType :: PrimType -> Text
renderPrimType = \case
  PrimInt -> "integer"
  PrimDecimal -> "decimal"
  PrimBool -> "bool"
  PrimString -> "string"
  PrimGuard -> "guard"
  PrimTime -> "time"
  PrimUnit -> "unit"

fromCorePrimType :: CoreType.PrimType -> PrimType
fromCorePrimType = \case
  CoreType.PrimInt -> PrimInt
  CoreType.PrimDecimal -> PrimDecimal
  CoreType.PrimBool -> PrimBool
  CoreType.PrimString -> PrimString
  CoreType.PrimGuard -> PrimGuard
  CoreType.PrimTime -> PrimTime
  CoreType.PrimUnit -> PrimUnit

liftCoreType :: CoreType.Type -> Type a
liftCoreType = \case
  CoreType.TyPrim p -> TyPrim (fromCorePrimType p)
  CoreType.TyList t ->
    TyList $ liftCoreType t
  CoreType.TyModRef mns -> TyModRef mns
  CoreType.TyObject (CoreType.Schema _qn m) ->
    TyObject $ RowConcrete $ liftCoreType <$> m
  CoreType.TyTable (CoreType.Schema _qn m) ->
    TyObject $ RowConcrete $ liftCoreType <$> m
  _ -> error "unsupported type for typechecking"

liftType :: Type Void -> Type a
liftType = fmap absurd


instance Pretty PrimType where
  pretty = \case
    PrimInt -> "integer"
    PrimDecimal -> "decimal"
    PrimBool -> "bool"
    PrimString -> "string"
    PrimGuard -> "guard"
    PrimTime -> "time"
    PrimUnit -> "unit"

data Type n
  = TyPrim PrimType
  | TyVar n
  | TyFun (Type n) (Type n)
  | TyNullary (Type n)
  | TyList (Type n)
  | TyObject (RowCtor n)
  | TyTable (RowCtor n)
  | TyModRef (Set ModuleName)
  | TyCapToken (CapRef n)
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic)

data CapRef n
  = CapVar n
  | CapConcrete QualifiedName
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic)

data RowCtor n
  = RowVar n
  | RowConcrete (Map Field (Type n))
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic)

data PactKind
  = TyKind
  | RowKind
  | UserDefKind
  deriving (Show, Eq)

data Schema
  = Schema QualifiedName (Map Field (Type Void))
  deriving (Eq, Show, Generic)

instance NFData Schema

data TypeVar n
  = TypeVar n PactKind
  deriving (Show, Eq)


tyVarKind :: TypeVar n -> PactKind
tyVarKind (TypeVar _ k) = k

tyFunToArgList :: Type n -> ([Type n], Type n)
tyFunToArgList (TyFun l r) =
  let (args, ret) = tyFunToArgList r
  in (l:args, ret)
tyFunToArgList ret = ([], ret)

type DebruijnTypeVar = TypeVar NamedDeBruijn

pattern RowVariable :: DeBruijn -> Text -> TypeVar NamedDeBruijn
pattern RowVariable ix a = TypeVar (NamedDeBruijn ix a) RowKind
pattern TypeVariable :: DeBruijn -> Text -> TypeVar NamedDeBruijn
pattern TypeVariable ix a = TypeVar (NamedDeBruijn ix a) TyKind
pattern UserDefVariable :: DeBruijn -> Text -> TypeVar NamedDeBruijn
pattern UserDefVariable ix a = TypeVar (NamedDeBruijn ix a) UserDefKind

instance Pretty n => Pretty (CapRef n) where
  pretty = \case
    CapVar n -> pretty n
    CapConcrete qn -> pretty qn

instance Pretty n => Pretty (RowCtor n) where
  pretty = \case
    RowVar n -> pretty n
    RowConcrete m ->
      Pretty.braces $ Pretty.hsep (prettyObj <$> M.toList m)
    where
    prettyObj (k, v) = pretty k <> ":" <> pretty v

instance Pretty n => Pretty (Type n) where
  pretty = \case
    TyPrim p -> pretty p
    TyVar n -> "typeVar" <> Pretty.brackets (pretty n)
    TyList n ->
      Pretty.brackets (pretty n)
    TyFun arg ret ->
      Pretty.parens ("->" <+> pretty arg <+> pretty ret)
    TyNullary ret ->
      Pretty.parens ("=>" <+> pretty ret)
    TyObject m ->
      pretty m
      -- Pretty.braces (Pretty.hsep (prettyObj <$> (M.toList m)))
    TyTable m ->
      pretty m
      -- Pretty.braces (Pretty.hsep (prettyObj <$> (M.toList m)))
    TyModRef mn ->
      let mns = Pretty.hsep (Pretty.punctuate Pretty.comma (pretty <$> S.toList mn))
      in "module" <> Pretty.braces mns
    TyCapToken n -> "captoken" <> Pretty.angles (pretty n)


-- Built in typeclasses
data BuiltinTC ty
  = Eq ty
  | Ord ty
  | Show ty
  | Add ty
  | Num ty
  | ListLike ty
  | Fractional ty
  | EnforceRead ty
  | IsValue ty
  | EqRow ty
  | RoseSubRow (RoseRow ty) (RoseRow ty)
  | RoseRowEq (RoseRow ty) (RoseRow ty)
  deriving (Show, Eq, Functor, Foldable, Traversable)

data RoseRow ty
  = RoseRowTy ty
  | RoseRowCat (RoseRow ty) (RoseRow ty)
  deriving (Show, Eq, Functor, Foldable, Traversable)

instance Pretty ty => Pretty (RoseRow ty) where
  pretty = \case
    RoseRowTy ty -> pretty ty
    RoseRowCat l r ->
      Pretty.parens (pretty l <+> "⊙" <+> pretty r)

pattern RoseConcrete :: Map Field (Type n) -> RoseRow (Type n)
pattern RoseConcrete o = RoseRowTy (TyObject (RowConcrete o))

pattern RoseVar :: n -> RoseRow (Type n)
pattern RoseVar v = (RoseRowTy (TyObject (RowVar v)))

data Arg ty
  = Arg
  { _argName :: !Text
  , _argType :: Type ty
  } deriving (Show, Eq, Functor, Foldable, Traversable, Generic)

instance Pretty ty => Pretty (Arg ty) where
  pretty (Arg n ty) =
    pretty n <> ":" <> pretty ty

pattern TyInt :: Type n
pattern TyInt = TyPrim PrimInt

pattern TyDecimal :: Type n
pattern TyDecimal = TyPrim PrimDecimal

pattern TyTime :: Type n
pattern TyTime = TyPrim PrimTime

pattern TyBool :: Type n
pattern TyBool = TyPrim PrimBool

pattern TyString :: Type n
pattern TyString = TyPrim PrimString

pattern TyUnit :: Type n
pattern TyUnit = TyPrim PrimUnit

pattern TyGuard :: Type n
pattern TyGuard = TyPrim PrimGuard

pattern (:~>) :: Type n -> Type n -> Type n
pattern l :~> r = TyFun l r

infixr 5 :~>

instance (Pretty ty) => Pretty (BuiltinTC ty) where
  pretty = \case
    Eq t -> "Eq" <> Pretty.braces (pretty t)
    Ord t -> "Ord" <> Pretty.braces (pretty t)
    Show t -> "Show" <> Pretty.braces (pretty t)
    Add t -> "Add" <> Pretty.braces (pretty t)
    Num t -> "Num" <> Pretty.braces (pretty t)
    ListLike t -> "ListLike" <> Pretty.braces (pretty t)
    Fractional t -> "Fractional" <> Pretty.braces (pretty t)
    EnforceRead t -> "EnforceRead" <> Pretty.braces (pretty t)
    EqRow t -> "EqRow" <> Pretty.braces (pretty t)
    IsValue t -> "IsValue" <> Pretty.braces (pretty t)
    RoseSubRow l r ->
      pretty l <+> "≼" <+> pretty r
    RoseRowEq l r ->
      pretty l <+> "~" <+> pretty r

-- Note, no superclasses, for now
newtype Pred tv
  = Pred (BuiltinTC tv)
  deriving (Show, Eq, Functor, Foldable, Traversable)

instance Pretty ty => Pretty (Pred ty) where
  pretty (Pred ty) = pretty ty


data TypeScheme tv =
  TypeScheme [tv] [Pred (Type tv)]  (Type tv)
  deriving Show

instance Pretty ty => Pretty (TypeScheme ty) where
  pretty (TypeScheme tvs preds ty) =
    case tvs of
      [] -> pretty ty
      _ ->
        "forall"
          <+> Pretty.parens (Pretty.commaSep tvs)
          <> "."
          <> if null preds then mempty else (" " <> Pretty.parens (Pretty.commaSep preds))
          <+> pretty ty

pattern NonGeneric :: Type tyname -> TypeScheme tyname
pattern NonGeneric ty = TypeScheme [] [] ty

typeOfLit :: Literal -> Type n
typeOfLit = TyPrim . literalPrim

literalPrim :: Literal -> PrimType
literalPrim = \case
  LString{} -> PrimString
  LInteger{} -> PrimInt
  LDecimal{} -> PrimDecimal
  LBool{} -> PrimBool
  LUnit -> PrimUnit

instance NFData PrimType
instance NFData ty => NFData (RowCtor ty)
instance NFData ty => NFData (CapRef ty)
instance NFData ty => NFData (Type ty)
instance NFData ty => NFData (Arg ty)

makeLenses ''Arg
