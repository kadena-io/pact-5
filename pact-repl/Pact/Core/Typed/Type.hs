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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}

module Pact.Core.Typed.Type where

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
import Control.Lens

data PrimType
  = PrimInt
  | PrimDecimal
  | PrimBool
  | PrimString
  | PrimGuard
  | PrimTime
  | PrimUnit
  -- Todo: Captokens could be a prim type from the POV of pact the language,
  -- but in John's formal verification work, he needs the capability to reference
  -- the type in the cap token's name
  -- | PrimCapToken
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
  -- PrimCapToken -> "cap-token"

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
    -- PrimCapToken -> "cap-token"

-- Todo: type family-ize Typed.Type to turn
-- row instantiations into type errors
type family TypeVariable v
type family RowVariable v

-- let ((x:module{k, y, z} some-module)) -- some_module : module {k, y, z, v, x, q}

data Type n
  = TyPrim PrimType
  | TyVar n
  | TyFun (Type n) (Type n)
  | TyNullary (Type n)
  | TyList (Type n)
  | TyObject (RowTy n)
  | TyTable (RowTy n)
  | TyModRef (Set ModuleName)
  | TyCapToken -- (CapRef n) -- todo: capref is useful for FV but for now exclude it
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data CapRef n
  = CapVar n
  | CapConcrete QualifiedName
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data RowTy n
  = RowVar n
  | RowConcrete (Map Field (Type n))
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

instance Plated (Type n) where
  plate f = \case
    t@TyVar{} -> pure t
    t@TyPrim{} -> pure t
    TyFun l r ->
      TyFun <$> f l <*> f r
    TyNullary t -> TyNullary <$> f t
    TyList n -> TyList <$> f n
    TyObject o -> TyObject <$> plateRow o
    TyTable o -> TyTable <$> plateRow o
    TyModRef mr -> pure (TyModRef mr)
    TyCapToken -> pure TyCapToken
    where
    plateRow = \case
      RowVar n -> pure (RowVar n)
      RowConcrete m ->
        RowConcrete <$> traverse f m


data PactKind
  = TyKind
  | RowKind
  deriving (Show, Eq, Ord)

data Schema
  = Schema QualifiedName (Map Field (Type Void))
  deriving (Eq, Show, Generic)

instance Pretty PactKind where
  pretty = \case
    TyKind -> "TYPE"
    RowKind -> "ROW"

data TypeVar n
  = TypeVar
    { _tpVar :: n
    , _tpKind :: PactKind }
  deriving (Show, Eq, Ord)

instance Pretty n => Pretty (TypeVar n) where
  pretty (TypeVar n k) =
    pretty n <+> ":" <+> pretty k


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
-- pattern UserDefVariable :: DeBruijn -> Text -> TypeVar NamedDeBruijn
-- pattern UserDefVariable ix a = TypeVar (NamedDeBruijn ix a) UserDefKind

instance Pretty n => Pretty (CapRef n) where
  pretty = \case
    CapVar n -> pretty n
    CapConcrete qn -> pretty qn

instance Pretty n => Pretty (RowTy n) where
  pretty = \case
    RowVar n -> "'" <> pretty n
    RowConcrete m ->
      Pretty.braces $ Pretty.hsep (prettyObj <$> M.toList m)
    where
    prettyObj (k, v) = pretty k <> ":" <> pretty v

instance Pretty n => Pretty (Type n) where
  pretty = \case
    TyPrim p -> pretty p
    TyVar n -> "'" <> pretty n
    TyList n ->
      Pretty.brackets (pretty n)
    TyFun arg ret ->
      Pretty.parens ("->" <+> pretty arg <+> pretty ret)
    TyNullary ret ->
      Pretty.parens ("=>" <+> pretty ret)
    TyObject m ->
      "object" <> Pretty.braces (pretty m)
      -- Pretty.braces (Pretty.hsep (prettyObj <$> (M.toList m)))
    TyTable m ->
      "table" <> Pretty.braces (pretty m)
      -- Pretty.braces (Pretty.hsep (prettyObj <$> (M.toList m)))
    TyModRef mn ->
      let mns = Pretty.hsep (Pretty.punctuate Pretty.comma (pretty <$> S.toList mn))
      in "module" <> Pretty.braces mns
    TyCapToken -> "cap-token"


-- Built in typeclasses
data BuiltinTC n
  = Eq (Type n)
  | Ord (Type n)
  | Show (Type n)
  | Add (Type n)
  | Num (Type n)
  | ListLike (Type n)
  | Fractional (Type n)
  | EnforceRead (Type n)
  | IsValue (Type n)
  | EqRow (RowTy n)
  | RoseSubRow (RoseRow n) (RoseRow n)
  | RoseRowEq (RoseRow n) (RoseRow n)
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic)


data RoseRow n
  = RoseRowTy (RowTy n)
  | RoseRowCat (RoseRow n) (RoseRow n)
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic)

instance Pretty ty => Pretty (RoseRow ty) where
  pretty = \case
    RoseRowTy ty -> pretty ty
    RoseRowCat l r ->
      Pretty.parens (pretty l <+> "⊙" <+> pretty r)

pattern RoseConcrete :: Map Field (Type n) -> RoseRow n
pattern RoseConcrete o = RoseRowTy (RowConcrete o)

pattern RoseVar :: n -> RoseRow n
pattern RoseVar v = (RoseRowTy (RowVar v))

{-# COMPLETE RoseVar, RoseConcrete, RoseRowCat #-}

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
  = Pred { _typeclassPredicate :: BuiltinTC tv }
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic)

makeLenses ''Pred

instance Pretty ty => Pretty (Pred ty) where
  pretty (Pred ty) = pretty ty


data TypeScheme tv =
  TypeScheme [tv] [Pred tv]  (Type tv)
  deriving (Show, Eq, Generic)

instance Pretty Schema where
  pretty (Schema _qn tys) =
    -- todo: fix
    pretty (M.toList tys)

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

traverseTCType :: Traversal (BuiltinTC ty) (BuiltinTC ty) (Type ty) (Type ty)
traverseTCType f = \case
  Eq t -> Eq <$> f t
  Ord t -> Ord <$> f t
  Show t -> Show <$> f t
  Add t -> Add <$> f t
  Num t -> Num <$> f t
  ListLike t -> ListLike <$> f t
  Fractional t -> Fractional <$> f t
  EnforceRead t -> EnforceRead <$> f t
  IsValue t -> IsValue <$> f t
  EqRow t -> EqRow <$> traverseRowTy f t
  RoseSubRow l r -> RoseSubRow <$> traverseRoseRowType f l <*> traverseRoseRowType f r
  RoseRowEq l r -> RoseRowEq <$> traverseRoseRowType f l <*> traverseRoseRowType f r

traverseRoseRowType :: Traversal (RoseRow n) (RoseRow n) (Type n) (Type n)
traverseRoseRowType f = \case
  RoseRowTy ty -> RoseRowTy <$> traverseRowTy f ty
  RoseRowCat l r -> RoseRowCat <$> traverseRoseRowType f l <*> traverseRoseRowType f r

traverseRowTy :: Traversal (RowTy n) (RowTy n) (Type n) (Type n)
traverseRowTy f = \case
  RowVar n -> pure (RowVar n)
  RowConcrete m -> RowConcrete <$> traverse f m

typeOfLit :: Literal -> Type n
typeOfLit = TyPrim . literalPrim

literalPrim :: Literal -> PrimType
literalPrim = \case
  LString{} -> PrimString
  LInteger{} -> PrimInt
  LDecimal{} -> PrimDecimal
  LBool{} -> PrimBool
  LUnit -> PrimUnit

returnType :: Lens' (Type n) (Type n)
returnType f = \case
  TyFun l r ->
    TyFun l <$> returnType f r
  TyNullary r ->
    TyNullary <$> returnType f r
  a -> f a

instance NFData PrimType
instance NFData ty => NFData (RowTy ty)
instance NFData ty => NFData (CapRef ty)
instance NFData ty => NFData (Type ty)
instance NFData Schema
deriving newtype instance NFData ty => NFData (Pred ty)
instance NFData name => NFData (RoseRow name)
instance NFData name => NFData (BuiltinTC name)
instance NFData tyname => NFData (TypeScheme tyname)
-- instance NFData ty => NFData (Arg ty)

-- makeLenses ''Arg
