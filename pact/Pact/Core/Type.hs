{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StrictData #-}


module Pact.Core.Type
 ( PrimType(..)
 , Type(..)
 , pattern TyInt
 , pattern TyDecimal
 , pattern TyTime
 , pattern TyBool
 , pattern TyString
 , pattern TyUnit
 , pattern TyGuard
 , typeOfLit
 , literalPrim
 , Arg(..)
 , argName
 , argType
 , TypedArg(..)
 , targName
 , targType
 , Schema(..)
 , DefKind(..)
 , renderType
 ) where

import Control.Lens
import Control.DeepSeq
import Data.List
import Data.Set(Set)
import Data.Text(Text)
import Data.Map.Strict(Map)
import GHC.Generics

import qualified Data.Text as T
import qualified Data.Set as S

import Pact.Core.Literal
import Pact.Core.Names
import Pact.Core.Pretty(Pretty(..), (<+>))

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

instance NFData PrimType

renderPrimType :: PrimType -> Text
renderPrimType = \case
  PrimInt -> "integer"
  PrimDecimal -> "decimal"
  PrimBool -> "bool"
  PrimString -> "string"
  PrimGuard -> "guard"
  PrimTime -> "time"
  PrimUnit -> "unit"

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
  | TyAnyList
  -- ^ Any list
  | TyModRef (Set ModuleName)
  -- ^ Module references
  | TyObject Schema
  -- ^ Objects
  | TyAnyObject
  -- ^ Object with any schema
  | TyTable Schema
  -- ^ Tables
  | TyCapToken
  -- ^ type of cap tokens
  | TyAny
  deriving (Eq, Show, Ord, Generic)

instance NFData Type

data Schema
  = Schema QualifiedName (Map Field Type)
  deriving (Eq, Show, Ord, Generic)

instance NFData Schema

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
  } deriving (Show, Eq, Functor, Foldable, Traversable, Generic)

instance NFData ty => NFData (Arg ty)

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
  | DKDefPact
  | DKDefSchema Schema
  | DKDefTable
  deriving (Show, Eq, Generic)

instance NFData DefKind

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
    TyModRef mrs ->
      "module" <> Pretty.braces (Pretty.hsep (Pretty.punctuate Pretty.comma (pretty <$> S.toList mrs))  )
    TyObject (Schema n _sc) ->
      "object" <> Pretty.braces (pretty n)
    TyTable (Schema n _sc) ->
      "table" <> Pretty.braces (pretty n)
    TyCapToken -> "CAPTOKEN"
    TyAnyList -> "list"
    TyAnyObject -> "object"
    TyAny -> "*"

renderType :: Type -> Text
renderType = \case
  TyPrim p -> renderPrimType p
  TyList t -> "[" <> renderType t <> "]"
  TyGuard -> "guard"
  TyModRef s ->
    let s' = T.concat (intersperse ", " (renderModuleName <$> S.toList s))
    in "module" <> "{" <> s' <> "}"
  TyObject (Schema n _sc) ->
    "object{" <> renderQualName n <> "}"
  TyTable (Schema n _sc) ->
    "table{" <> renderQualName n <> "}"
  TyCapToken -> "CAPTOKEN"
  TyAnyObject -> "object"
  TyAnyList -> "list"
  TyAny -> "*"

makeLenses ''Arg
makeLenses ''TypedArg
