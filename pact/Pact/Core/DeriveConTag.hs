{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

-- |
-- Module      :  Pact.Core.DeriveConTag
-- Copyright   :  (C) 2022 Kadena
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jose Cardona <jose@kadena.io>
--
-- A small module for a a template haskell class which derives constructor
-- names and declaration ordering.
--

module Pact.Core.DeriveConTag
  ( HasConstrInfo(..)
  , deriveConstrInfo
  , ConstrInfo(..)
  ) where

#if !MIN_VERSION_base(4,20,0)
import Data.List(foldl')
#endif

import Data.Word(Word8)
import Data.Text(Text)
import Control.Monad
import Language.Haskell.TH
import qualified Data.Text.Read as T
import qualified Data.Text as T

import Pact.JSON.Encode(Encode)
import Pact.JSON.Decode(FromJSON)
import qualified Pact.JSON.Encode as J
import qualified Pact.JSON.Decode as JD
import Numeric (showHex)

-- | Simple constructor info: Constructor name and constructor
--   index in order of data type declaration
data ConstrInfo
  = ConstrInfo
  { _ciName :: Text
  , _ciIndex :: Word8
  } deriving Show

instance Encode ConstrInfo where
  build (ConstrInfo n i) = J.object
    [ "conName" J..= n
    , "conIndex" J..= T.pack (showHex i "")]

instance FromJSON ConstrInfo where
  parseJSON = JD.withObject "ConstrInfo" $ \o -> do
    n <- o JD..: "conName"
    t <- o JD..: "conIndex"
    case T.hexadecimal t of
      Right (a,_) -> pure (ConstrInfo n a)
      _ -> fail "cannot parse Constr Info index"

-- | A simple metadata info typeclass for getting the constructor tag and info for a haskell
--   algebraic data type with simple constructors
class HasConstrInfo a where
  getConstrInfo :: a -> ConstrInfo

  allConstrInfos :: proxy a -> [ConstrInfo]

  constrIndex :: a -> Word8
  constrIndex = _ciIndex . getConstrInfo
  {-# INLINABLE constrIndex #-}

  constrName :: a -> Text
  constrName = _ciName . getConstrInfo
  {-# INLINABLE constrName #-}

-- | Derive a `HasConstrInfo` instance
deriveConstrInfo :: Name -> Q [Dec]
deriveConstrInfo ty = reify ty >>= \case
  TyConI (DataD _ctx _n _tvs _k cons _) -> do
    when (length cons >= fromIntegral (maxBound :: Word8)) $
      fail "deriveConstrInfo: too many constructors"
    -- Create our `HasConstrInfo (f a_1 ... a_n)` signature
    tyVars <- traverse (const (newName "x")) _tvs
    let declTy = mkTyApp (ConT ty) (VarT <$> tyVars)
    let instTy = mkTyApp (ConT ''HasConstrInfo) [declTy]

    cInfos <- zipWithM mkConstrEntry [0..] cons
    fClauses <- zipWithM mkConstrFun [0..] cons
    let fDec = FunD (mkName "getConstrInfo") fClauses
        allClause = Clause [WildP] (NormalB (ListE cInfos)) []
        allDec = FunD (mkName "allConstrInfos") [allClause]
    pure $ [InstanceD Nothing [] instTy [fDec, allDec]]
  _ -> fail "Can only derive HasConstrInfo for a Type"
  where
  mkConstrEntry :: Integer -> Con -> Q Exp
  mkConstrEntry ix (NormalC n _) =
    let nraw = nameBase n
    in pure $ mkConstrInfoExp nraw ix
  mkConstrEntry ix (RecC n _) =
    let nraw = nameBase n
    in pure $ mkConstrInfoExp nraw ix
  mkConstrEntry _ _ = fail "Cannot create constrInfo from non-simple constructor"

  mkConstrFun :: Integer -> Con -> Q Clause
  mkConstrFun ix (NormalC n bangTy) =
    let nraw = nameBase n
        body = NormalB (mkConstrInfoExp nraw ix)
    in pure $ Clause [ConP n [] (const WildP <$> bangTy)] body []
  mkConstrFun ix (RecC n bangTy) =
    let nraw = nameBase n
        body = NormalB (mkConstrInfoExp nraw ix)
    in pure $ Clause [ConP n [] (const WildP <$> bangTy)] body []
  mkConstrFun _ _ = fail "Cannot create constrInfo from non-simple constructor"

  mkConstrInfoExp :: String -> Integer -> Exp
  mkConstrInfoExp cn ix = do
    AppE (AppE (ConE 'ConstrInfo) (LitE (StringL cn))) (LitE (IntegerL ix))

  mkTyApp :: Type -> [Type] -> Type
  mkTyApp =
    foldl' AppT

