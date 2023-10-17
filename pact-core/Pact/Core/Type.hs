{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}


module Pact.Core.Type
 ( PrimType(..)
 , Type(..)
--  , TypeScheme(..)
 , pattern TyInt
 , pattern TyDecimal
 , pattern TyTime
 , pattern TyBool
 , pattern TyString
 , pattern TyUnit
 , pattern TyGuard
 , typeOfLit
 , literalPrim
--  , BuiltinTC(..)
--  , Pred(..)
--  , renderType
--  , renderPred
--  , TypeOfDef(..)
 , Arg(..)
 , argName
 , argType
 , TypedArg(..)
 , targName
 , targType
 , Schema(..)
 , DefKind(..)
 ) where

import Control.Lens
import Data.Text(Text)
import Data.Map.Strict(Map)

import Pact.Core.Literal
import Pact.Core.Names
import Pact.Core.Pretty(Pretty(..), (<+>))

import qualified Pact.Core.Pretty as Pretty

data PrimType =
  PrimInt |
  PrimDecimal |
  -- PrimTime |
  PrimBool |
  PrimString |
  PrimGuard |
  PrimTime |
  PrimUnit
  deriving (Eq,Ord,Show, Enum, Bounded)

instance Pretty PrimType where
  pretty = \case
    PrimInt -> "integer"
    PrimDecimal -> "decimal"
    PrimBool -> "bool"
    PrimString -> "string"
    PrimGuard -> "guard"
    PrimTime -> "time"
    PrimUnit -> "unit"

-- Todo: caps are a bit strange here
-- same with defpacts. Not entirely sure how to type those yet.
-- | Our internal core type language
--   Tables, rows and and interfaces are quite similar,
--    t ::= B
--      |   v
--      |   t -> t
--      |   row
--      |   list<t>
--      |   interface name row
--
--    row  ::= {name:t, row*}
--    row* ::= name:t | ϵ
data Type
  = TyPrim PrimType
  -- ^ Built-in types
  | TyList Type
  -- ^ List aka [a]
  | TyModRef ModuleName
  -- ^ Module references
  | TyObject Schema
  -- ^ Objects
  | TyTable Schema
  -- ^ Tables
  deriving (Eq, Show, Ord)

newtype Schema
  = Schema { _schema :: Map Field Type }
  deriving (Eq, Show, Ord)

pattern TyInt :: Type
pattern TyInt = TyPrim PrimInt

pattern TyDecimal :: Type
pattern TyDecimal = TyPrim PrimDecimal

pattern TyTime :: Type
pattern TyTime = TyPrim PrimTime

pattern TyBool :: Type
pattern TyBool = TyPrim PrimBool

pattern TyString :: Type
pattern TyString = TyPrim PrimString

pattern TyUnit :: Type
pattern TyUnit = TyPrim PrimUnit

pattern TyGuard :: Type
pattern TyGuard = TyPrim PrimGuard


-- Built in typeclasses
-- data BuiltinTC
--   = Eq
--   | Ord
--   | Show
--   | Add
--   | Num
--   | ListLike
--   | Fractional
--   deriving (Show, Eq, Ord)

-- instance Pretty BuiltinTC where
--   pretty = \case
--     Eq -> "Eq"
--     Ord -> "Ord"
--     Show -> "Show"
--     Add -> "Add"
--     Num -> "Num"
--     ListLike -> "ListLike"
--     Fractional -> "Fractional"

-- -- Note, no superclasses, for now
-- data Pred tv
--   = Pred BuiltinTC (Type tv)
--   deriving (Show, Eq, Functor, Foldable, Traversable)

-- data TypeScheme tv =
--   TypeScheme [tv] [Pred tv]  (Type tv)
--   deriving Show

-- data TypeOfDef tv
--   = DefunType (Type tv)
--   | DefcapType [Type tv] (Type tv)
--   deriving (Show, Functor, Foldable, Traversable)

typeOfLit :: Literal -> Type
typeOfLit = TyPrim . literalPrim

literalPrim :: Literal -> PrimType
literalPrim = \case
  LString{} -> PrimString
  LInteger{} -> PrimInt
  LDecimal{} -> PrimDecimal
  LBool{} -> PrimBool
  LUnit -> PrimUnit

-- renderType :: Type -> Text
-- renderType = T.pack . show . pretty

-- renderPred :: (Pretty n) => Pred n -> Text
-- renderPred = T.pack . show . pretty

data Arg ty
  = Arg
  { _argName :: !Text
  , _argType :: Maybe ty
  } deriving (Show, Eq, Functor, Foldable, Traversable)

instance Pretty ty => Pretty (Arg ty) where
  pretty (Arg n ty) =
    pretty n <> maybe mempty ((":" <>) . pretty) ty

data TypedArg ty
  = TypedArg
  { _targName :: !Text
  , _targType :: ty
  } deriving (Show, Eq, Functor, Foldable, Traversable)

data DefKind
  = DKDefun
  | DKDefConst
  | DKDefCap
  | DKDefSchema Schema
  | DKDefTable
  deriving (Show, Eq)

-- instance Pretty n => Pretty (Pred n) where
--   pretty (Pred tc ty) = pretty tc <>  Pretty.angles (pretty ty)

instance Pretty Type where
  pretty = \case
    TyPrim p -> pretty p
    TyGuard -> "guard"
    TyList l -> "list" <+> liParens l
      where
      liParens t@TyPrim{} = pretty t
      liParens t = Pretty.parens (pretty t)
    TyModRef mr ->
      "module" <> Pretty.braces (pretty mr)
    TyObject _o -> "todo: <object>"
    TyTable _o -> "todo: table"

-- instance Pretty tv => Pretty (TypeScheme tv) where
--   pretty (TypeScheme tvs preds ty) =
--     quant tvs <> qual preds <> pretty ty
--     where
--     renderTvs xs suffix =
--       Pretty.hsep $ fmap (\n -> Pretty.parens (pretty n <> ":" <+> suffix)) xs
--     quant [] = mempty
--     quant as =
--       "∀" <> renderTvs as "*" <> ". "
--     qual [] = mempty
--     qual as =
--       Pretty.parens (Pretty.commaSep as) <+> "=> "

makeLenses ''Arg
makeLenses ''TypedArg
