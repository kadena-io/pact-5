{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DerivingStrategies #-}


module Pact.Core.PactValue
 ( PactValue(..)
 , _PLiteral
 , _PList
 , _PGuard
 , _PCapToken
 , _PObject
 , checkPvType
 , ObjectData(..)
 , envMap
 , FQCapToken
 , pattern PInteger
 , _PInteger
 , pattern PDecimal
 , _PDecimal
 , pattern PString
 , _PString
 , pattern PBool
 , _PBool
 , pattern PUnit
 , _PUnit
 , synthesizePvType
 ) where

import Control.Lens
import Data.Vector(Vector)
import Data.Map.Strict(Map)
import Data.Text(Text)
import Data.Decimal(Decimal)

import Control.DeepSeq
import GHC.Generics

import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import qualified Pact.Time as PactTime
import qualified Data.Set as S

import Pact.Core.Type
import Pact.Core.Names
import Pact.Core.Guards
import Pact.Core.Literal
import Pact.Core.Pretty
import Pact.Core.ModRefs
import Pact.Core.Capabilities

import qualified Pact.Core.Pretty as Pretty

data PactValue
  = PLiteral !Literal
  | PList !(Vector PactValue)
  | PGuard !(Guard QualifiedName PactValue)
  | PObject !(Map Field PactValue)
  | PModRef !ModRef
  | PCapToken !(CapToken FullyQualifiedName PactValue)
  | PTime !PactTime.UTCTime
  -- BIG TODO:
  -- This ord instance is dangerous. Consider removing in favor of newtyping over it.
  deriving (Eq, Show, Ord, Generic)

instance NFData PactValue

makePrisms ''PactValue

pattern PInteger :: Integer -> PactValue
pattern PInteger i = PLiteral (LInteger i)
_PInteger :: Prism' PactValue Integer
_PInteger = _PLiteral . _LInteger

pattern PDecimal :: Decimal -> PactValue
pattern PDecimal d = PLiteral (LDecimal d)
_PDecimal :: Prism' PactValue Decimal
_PDecimal = _PLiteral . _LDecimal

pattern PString :: Text -> PactValue
pattern PString s = PLiteral (LString s)
_PString :: Prism' PactValue Text
_PString = _PLiteral . _LString

pattern PBool :: Bool -> PactValue
pattern PBool b = PLiteral (LBool b)
_PBool :: Prism' PactValue Bool
_PBool = _PLiteral . _LBool

pattern PUnit :: PactValue
pattern PUnit = PLiteral LUnit
_PUnit :: Prism' PactValue ()
_PUnit = _PLiteral . _LUnit

type FQCapToken = CapToken FullyQualifiedName PactValue

instance Pretty PactValue where
  pretty = \case
    PLiteral lit -> pretty lit
    PList p -> Pretty.list (V.toList (pretty <$> p))
    PGuard g -> pretty g
    PObject o -> pretty (ObjectData o)
    PModRef md -> pretty md
    PCapToken (CapToken fqn args) ->
      "CapToken" <> pretty (CapToken (fqnToQualName fqn) args)
    PTime t -> pretty (PactTime.formatTime "%Y-%m-%d %H:%M:%S%Q %Z" t)

synthesizePvType :: PactValue -> Type
synthesizePvType = \case
  PLiteral l -> typeOfLit l
  PList _ -> TyAnyList
  PGuard _ -> TyGuard
  PModRef mr -> TyModRef (_mrImplemented mr)
  PObject _ -> TyAnyObject
  PCapToken {} -> TyCapToken
  PTime _ -> TyTime

-- | Check that a `PactValue` has the provided `Type`, returning
-- `Just ty` if so and `Nothing` otherwise.
checkPvType :: Type -> PactValue -> Bool
checkPvType TyAny = const True
checkPvType ty = \case
  PLiteral l -> typeOfLit l == ty
  PGuard{} -> ty == TyGuard
  PObject o -> case ty of
    -- Todo: gas
    TyObject (Schema _ sc) ->
      let tyList = M.toList sc
          oList = M.toList o
      in tcObj oList tyList
      where
      tcObj l1 l2
        | length l1 == length l2 = and $ zipWith mcheck l1 l2
        | otherwise = False
      mcheck (f1, pv) (f2, t)
        | f1 == f2 = checkPvType t pv
        | otherwise = False
    TyAnyObject -> True
    _ -> False
  -- Todo: gas
  PList l -> case ty of
    TyList t'  -> all (checkPvType t') l
    TyAnyList -> True
    _ -> False
  PModRef (ModRef _orig ifs) -> case ty of
    TyModRef mns -> mns `S.isSubsetOf` ifs
    _ -> False
  PCapToken _ -> False
  PTime _ -> ty == TyTime


newtype ObjectData term
  = ObjectData { _objectData :: Map Field term }
  deriving (Eq, Show, NFData)

envMap
  :: Lens (ObjectData term)
          (ObjectData term')
          (Map Field term)
          (Map Field term')
envMap f (ObjectData m) = fmap ObjectData (f m)

instance Pretty term => Pretty (ObjectData term) where
  pretty (ObjectData o) =
    braces $ mconcat $ punctuate comma (objPair <$> M.toList o)
      where
      -- SEMANTIC NOTE: this specific formatting matters, since it's what we use
      -- to pretty print and thus makes it into hashes
      objPair (f, t) = dquotes (pretty f) <> ":" <+> pretty t
