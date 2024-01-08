{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}


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
 , pattern PDecimal
 , pattern PString
 , pattern PBool
 , pattern PUnit
 , synthesizePvType
 ) where

import Control.Lens
import Control.Monad(zipWithM)
import Data.Vector(Vector)
import Data.Map.Strict(Map)
import Data.Text(Text)
import Data.Maybe(isJust)
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

pattern PDecimal :: Decimal -> PactValue
pattern PDecimal d = PLiteral (LDecimal d)

pattern PString :: Text -> PactValue
pattern PString s = PLiteral (LString s)

pattern PBool :: Bool -> PactValue
pattern PBool b = PLiteral (LBool b)

pattern PUnit :: PactValue
pattern PUnit = PLiteral LUnit

type FQCapToken = CapToken FullyQualifiedName PactValue

instance Pretty PactValue where
  pretty = \case
    PLiteral lit -> pretty lit
    PList p -> Pretty.list (V.toList (pretty <$> p))
    PGuard g -> pretty g
    PObject o ->
      braces $ hsep $ punctuate comma (objPair <$> M.toList o)
      where
      objPair (f, t) = pretty f <> ":" <> pretty t
    PModRef md -> pretty md
    PCapToken (CapToken fqn args) ->
      parens (pretty fqn) <> if null args then mempty else hsep (pretty <$> args)
    PTime t -> pretty (PactTime.formatTime "%Y-%m-%d %H:%M:%S%Q %Z" t)

synthesizePvType :: PactValue -> Type
synthesizePvType = \case
  PLiteral l -> typeOfLit l
  PList _ -> TyList TyUnit
  PGuard _ -> TyGuard
  PModRef mr -> TyModRef (S.fromList (_mrImplemented mr))
  PObject f ->
    let tys = synthesizePvType <$> f
    in TyObject (Schema tys)
  PCapToken {} -> TyCapToken
  PTime _ -> TyTime

-- | Check that a `PactValue` has the provided `Type`, returning
-- `Just ty` if so and `Nothing` otherwise.
checkPvType :: Type -> PactValue -> Maybe Type
checkPvType ty = \case
  PLiteral l
    | typeOfLit l == ty -> Just ty
    | otherwise -> Nothing
  PGuard{}
    | ty == TyGuard -> Just TyGuard
    | otherwise -> Nothing
  -- PTable _ sc1
  --   | ty == TyTable sc1 -> Just (TyTable sc1)
  --   | otherwise -> Nothing
  -- todo: types of objects
  PObject o -> case ty of
    TyObject (Schema sc) ->
      let tyList = M.toList sc
          oList = M.toList o
      in tcObj oList tyList
      where
      tcObj l1 l2
        | length l1 == length l2 = TyObject . Schema . M.fromList <$> zipWithM mcheck l1 l2
        | otherwise = Nothing
      mcheck (f1, pv) (f2, t)
        | f1 == f2 = (f1,) <$> checkPvType t pv
        | otherwise = Nothing
    TyAnyObject -> Just TyAnyObject
    _ -> Nothing
  PList l -> case ty of
    TyList t' | all (isJust . checkPvType t') l -> Just (TyList t')
    TyAnyList -> Just TyAnyList
    _ -> Nothing
  PModRef (ModRef _orig ifs refinedSet) -> case ty of
    TyModRef mns
      | Just rf <- refinedSet, mns `S.isSubsetOf` rf -> Just (TyModRef mns)
      | isJust refinedSet -> Nothing
      | mns `S.isSubsetOf` (S.fromList ifs) && refinedSet == Nothing -> Just (TyModRef mns)
      | otherwise -> Nothing
    _ -> Nothing
  PCapToken _ -> Nothing
  PTime _ -> case ty of
    TyTime -> Just TyTime
    _ -> Nothing



newtype ObjectData term
  = ObjectData { _objectData :: Map Field term }
  deriving (Eq, Show, NFData)

envMap
  :: Lens (ObjectData term)
          (ObjectData term')
          (Map Field term)
          (Map Field term')
envMap f (ObjectData m) = fmap ObjectData (f m)
