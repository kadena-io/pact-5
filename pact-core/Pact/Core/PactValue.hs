{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}

module Pact.Core.PactValue
 ( PactValue(..)
 , _PLiteral
 , _PList
 , _PGuard
 , _PCapToken
 , checkPvType
 , EnvData(..)
 , envMap
 , FQCapToken
 , pattern PInteger
 , pattern PDecimal
 , pattern PString) where

import Control.Lens
import Control.Monad(zipWithM)
import Data.Vector(Vector)
import Data.Map.Strict(Map)
import Data.Text(Text)
import Data.Maybe(isJust)
import Data.Decimal(Decimal)
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import qualified Pact.Time as PactTime

import Pact.Core.Type
import Pact.Core.Names
import Pact.Core.Guards
import Pact.Core.Literal
import Pact.Core.Pretty
import Pact.Core.ModRefs
import Pact.Core.Capabilities

import qualified Pact.Core.Pretty as Pretty

data PactValue
  = PLiteral Literal
  | PList (Vector PactValue)
  | PGuard (Guard FullyQualifiedName PactValue)
  | PObject (Map Field PactValue)
  | PModRef ModRef
  | PCapToken (CapToken FullyQualifiedName PactValue)
  | PTime !PactTime.UTCTime
  deriving (Eq, Show, Ord)

makePrisms ''PactValue

pattern PInteger :: Integer -> PactValue
pattern PInteger i = PLiteral (LInteger i)

pattern PDecimal :: Decimal -> PactValue
pattern PDecimal d = PLiteral (LDecimal d)

pattern PString :: Text -> PactValue
pattern PString s = PLiteral (LString s)

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
      parens (pretty (fqnToQualName fqn) <> if null args then mempty else hsep (pretty <$> args))
    PTime t -> pretty (PactTime.formatTime "%Y-%m-%d %H:%M:%S%Q %Z" t)


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
    _ -> Nothing
  PList l -> case ty of
    TyList t' | all (isJust . checkPvType t') l -> Just (TyList t')
    _ -> Nothing
  PModRef (ModRef _orig ifs refined) -> case ty of
    TyModRef mn
      | refined == Just mn -> Just (TyModRef mn)
      | isJust refined -> Nothing
      | mn `elem` ifs && refined == Nothing -> Just (TyModRef mn)
      | otherwise -> Nothing
    _ -> Nothing
  PCapToken _ -> Nothing
  PTime _ -> case ty of
    TyPrim PrimTime -> Just $ TyPrim PrimTime
    _ -> Nothing



newtype EnvData term
  = EnvData { _envMap :: Map Field term }
  deriving (Eq, Show)

envMap
  :: Lens (EnvData term)
          (EnvData term')
          (Map Field term)
          (Map Field term')
envMap f (EnvData m) = fmap EnvData (f m)
