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
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE StandaloneDeriving #-}

module Pact.Core.Typed.Type
 ( Type(..)
 , RowTy(..)
 , MRef(..)
 , PactKind(..)
 , Schema(..)
 , TypeVar(..)
 , RoseRow(..)
 , DebruijnTypeVar
 , pattern RowVariable
 , pattern TypeVariable
 , pattern RoseConcrete
 , pattern RoseVar
 , pattern TyInt
 , pattern TyDecimal
 , pattern TyTime
 , pattern TyBool
 , pattern TyString
 , pattern TyGuard
 , pattern (:~>)
 , pattern NonGeneric
 , pattern TyUnit
 , pattern MRefVariable
 , BuiltinTC(..)
 , TypeScheme(..)
 , PrimType(..)
 , Pred
 , DefnType(..)
 , tyVarKind
 , tyFunToArgList
 , traverseTCType
 , traverseRoseRowType
 , traverseRowTy
 , typeOfLit
 , literalPrim
 , returnType
 , liftType
 , fromCorePrimType
 , renderPrimType
 , typeSchemesAlphaEquivalent
 )where

import Control.DeepSeq
import Data.Void
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
import Pact.Core.Info
import Data.IntMap (IntMap)

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

-- Todo: I want to remove how rows, type variables and modref variables share one type variable type,
-- because it leads to a lot of "this is impossible" type scenarios that I have to satisfy to make
-- the compiler happy, but type families make many other things much less ergonomic. The following comment is
-- just a sketch of what this might look like if I do pull the trigger on type families for this.
--
--
-- data InferenceMode s

-- type InferenceLevel = Int

-- data InferenceVar
--   = Unbound Text Unique !InferenceLevel
--   | Bound Text Unique
--   deriving (Eq, Show)

-- data TypeRef n
--   = TV InferenceVar
--   | LinkTy (Type n)
--   deriving (Eq, Show)

-- data RowRef n
--   = RV InferenceVar
--   | LinkRow (RowTy n)
--   deriving (Show)

-- data ModRefRef n
--   = MV InferenceVar
--   | LinkModRef (S.Set ModuleName)
--   deriving (Show)

-- data TypeVariable v where
--   TVAbsurd :: Void -> TypeVariable Void
--   TVInference :: STRef s (TypeRef (InferenceMode s)) -> TypeVariable (InferenceMode s)
--   TVDebruijn :: TypeVar NamedDeBruijn -> TypeVariable NamedDeBruijn

-- instance Show (TypeVariable v) where
--   show = \case
--     TVAbsurd r -> show r
--     TVInference r -> show $ unsafeReadSTRef r
--     TVDebruijn dbj -> show dbj

-- instance Eq (TypeVariable v) where
--   (TVAbsurd l) == (TVAbsurd r) = l == r
--   (TVInference l) == (TVInference r) = l == r
--   (TVDebruijn l) == (TVDebruijn r) = l == r

-- data RowVariable v where
--   RVAbsurd :: Void -> RowVariable Void
--   RVInference :: STRef s (RowRef (InferenceMode s)) -> RowVariable (InferenceMode s)
--   RVDebruijn :: TypeVar NamedDeBruijn -> RowVariable NamedDeBruijn

-- instance Eq (RowVariable v) where
--   (RVAbsurd l) == (RVAbsurd r) = l == r
--   (RVInference l) == (RVInference r) = l == r
--   (RVDebruijn l) == (RVDebruijn r) = l == r

-- data ModRefVariable v where
--   MVAbsurd :: Void -> ModRefVariable Void
--   MVInference :: STRef s (RowRef (InferenceMode s)) -> ModRefVariable (InferenceMode s)
--   MVDebruijn :: TypeVar NamedDeBruijn -> ModRefVariable NamedDeBruijn

-- instance Eq (ModRefVariable v) where
--   (MVAbsurd l) == (MVAbsurd r) = l == r
--   (MVInference l) == (MVInference r) = l == r
--   (MVDebruijn l) == (MVDebruijn r) = l == r

data Type n
  = TyPrim PrimType
  | TyVar n
  | TyFun (Type n) (Type n)
  | TyNullary (Type n)
  | TyList (Type n)
  | TyObject (RowTy n)
  | TyTable (RowTy n)
  | TyModRef (MRef n)
  | TyCapToken
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

data MRef n
  = MRefVar n
  | MConcrete (S.Set ModuleName)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

instance Pretty n => Pretty (MRef n) where
  pretty = \case
    MRefVar n -> pretty n
    MConcrete mns ->
      Pretty.hsep (Pretty.punctuate Pretty.comma (pretty <$> S.toList mns))

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
  | ModRefKind
  deriving (Show, Eq, Ord)

data Schema
  = Schema QualifiedName (Map Field (Type Void))
  deriving (Eq, Show, Generic)

instance Pretty PactKind where
  pretty = \case
    TyKind -> "TYPE"
    RowKind -> "ROW"
    ModRefKind -> "REFERENCE"

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
pattern MRefVariable :: DeBruijn -> Text -> TypeVar NamedDeBruijn
pattern MRefVariable ix a = TypeVar (NamedDeBruijn ix a) ModRefKind

instance Pretty n => Pretty (RowTy n) where
  pretty = \case
    RowVar n -> "'" <> pretty n
    RowConcrete m ->
      Pretty.hsep (prettyObj <$> M.toList m)
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
    TyTable m ->
      "table" <> Pretty.braces (pretty m)
    TyModRef mn ->
      "module" <> Pretty.braces (pretty mn)
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
  | RoseSubRow (RoseRow n) (RoseRow n)
  | RoseRowEq (RoseRow n) (RoseRow n)
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

data RoseRow n
  = RoseRowTy (RowTy n)
  | RoseRowCat (RoseRow n) (RoseRow n)
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

instance Pretty ty => Pretty (RoseRow ty) where
  pretty = \case
    RoseRowTy ty -> pretty ty
    RoseRowCat l r ->
      Pretty.parens (pretty l <+> "⊙" <+> pretty r)

pattern RoseConcrete :: Map Field (Type n) -> RoseRow n
pattern RoseConcrete o = RoseRowTy (RowConcrete o)

pattern RoseVar :: n -> RoseRow n
pattern RoseVar v = RoseRowTy (RowVar v)

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
    IsValue t -> "IsValue" <> Pretty.braces (pretty t)
    RoseSubRow l r ->
      pretty l <+> "≼" <+> pretty r
    RoseRowEq l r ->
      pretty l <+> "~" <+> pretty r

-- Note, no superclasses, for now
type Pred i tv = Located i (BuiltinTC tv)

data TypeScheme tv =
  TypeScheme [tv] [BuiltinTC tv]  (Type tv)
  deriving (Show, Eq, Generic)

data DefnType tv =
  IndexedDefpactStepType (IntMap (Type Void))
  | NotIndexed (TypeScheme tv)
  deriving Show

instance Ord tv => Eq (DefnType tv) where
  (IndexedDefpactStepType l) == (IndexedDefpactStepType r) = l == r
  (NotIndexed t) == (NotIndexed t') =
    typeSchemesAlphaEquivalent t t'
  _ == _ = False

typeSchemesAlphaEquivalent :: (Ord tv, Eq tv') => TypeScheme tv -> TypeScheme tv' -> Bool
typeSchemesAlphaEquivalent (TypeScheme tvs preds ty) (TypeScheme tvs' preds' ty')
  | length tvs /= length tvs' = False
  | otherwise =
    let m = M.fromList (zip tvs tvs')
        replace n = m M.! n
    in (fmap replace ty == ty') && ((fmap.fmap) replace preds == preds')

instance Pretty Schema where
  pretty (Schema _qn tys) =
    Pretty.hsep (prettyObj <$> M.toList tys)
    where
    prettyObj (k, v) = pretty k <> ":" <> pretty v

instance Pretty ty => Pretty (TypeScheme ty) where
  pretty (TypeScheme tvs preds ty) =
    case tvs of
      [] -> pretty ty
      _ ->
        "forall"
          <+> Pretty.parens (Pretty.commaSep tvs)
          <> "."
          <> if null preds then mempty else (Pretty.space <> Pretty.parens (Pretty.commaSep preds) <+> "=>")
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
instance NFData n => NFData (MRef n)
instance NFData ty => NFData (RowTy ty)
instance NFData ty => NFData (Type ty)
instance NFData Schema
instance NFData name => NFData (RoseRow name)
instance NFData name => NFData (BuiltinTC name)
instance (NFData tyname) => NFData (TypeScheme tyname)
