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

module Pact.Core.SizeOf.Deriving
  ( makeSizeOf
  ) where

#if !MIN_VERSION_base(4,20,0)
import Data.List(foldl')
#endif

import Data.Word(Word8)
import Control.Monad
import Language.Haskell.TH

-- | Derive our `SizeOf` instances
makeSizeOf :: Name -> Q [Dec]
makeSizeOf ty = reify ty >>= \case
  TyConI (DataD _ctx _n _tvs _k cons _) -> do
    when (length cons >= fromIntegral (maxBound :: Word8)) $
      fail "deriveConstrInfo: too many constructors"
    -- Create our `SizeOf (f a_1 ... a_n)` signature
    let sizeOfName = mkName "SizeOf"
    tyVars <- traverse (const (newName "a")) _tvs
    let tyCtx = AppT (ConT sizeOfName) . VarT <$> tyVars
    let declTy = mkTyApp (ConT ty) (VarT <$> tyVars)
    let instTy = mkTyApp (ConT (mkName "SizeOf")) [declTy]
    let szClause = FunD (mkName "estimateSize")
    let countBytes = mkName "countBytes"
    decl <- case cons of
      [] -> fail "makeSizeOf not supported for empty data decls"
      -- This is for a single constructor, our by-far most common case
      [NormalC n bt] -> case bt of
        -- No fields, so assume it's just 1 byte
        [] -> do
          let body = AppE (VarE countBytes) (LitE (IntegerL 1))
          pure $ Clause [WildP] (NormalB body) []
        _ -> do
          -- We generate the overhead for
          let overhead = VarE (mkName "addSmallTagOverhead")
          let vars = fieldNames bt
          -- (Ctor f_a f_b ... f_n)
          let clausePats = ConP n [] (VarP <$> vars)
          -- do
          -- addSmallTagOverhead
          -- estimateSize f_a
          -- estimateSize f_b
          -- ...
          let body = DoE Nothing (NoBindS overhead : (NoBindS . callEstimateSize . VarE <$> vars))
          pure $ Clause [clausePats] (NormalB body) []
      -- Record ctor, which we support the same as the other case
      [RecC n bt] -> case bt of
        -- No fields, so assume it's just 1 byte
        [] -> do
          let body = AppE (VarE countBytes) (LitE (IntegerL 1))
          pure $ Clause [WildP] (NormalB body) []
        _ -> do
          -- We generate the overhead for
          let overhead = VarE (mkName "addSmallTagOverhead")
          let vars = fieldNames bt
          -- (Ctor f_a f_b ... f_n)
          let clausePats = ConP n [] (VarP <$> vars)
          -- do
          -- addSmallTagOverhead
          -- estimateSize f_a
          -- estimateSize f_b
          -- ...
          let body = DoE Nothing (NoBindS overhead : (NoBindS . callEstimateSize . VarE <$> vars))
          pure $ Clause [clausePats] (NormalB body) []
      -- Case is an adt variant
      _ -> do
        normalCInfos <- traverse getNormalCInfo cons
        variantVar <- newName "x"
        let overhead = VarE (mkName "adtTagOverhead")
        variants <- traverse (uncurry mkSingleVariantMatch) normalCInfos
        let bodyExp = DoE Nothing [NoBindS overhead , NoBindS (CaseE (VarE variantVar) variants)]
        pure $ Clause [VarP variantVar] (NormalB bodyExp) []
    pure [InstanceD Nothing tyCtx instTy [szClause [decl]]]

  _ -> fail "Can only derive HasConstrInfo for a Type"
  where
  callEstimateSize = AppE (VarE (mkName "estimateSize"))
  fieldNames = zipWith (\a _ -> mkName ("f_" <> [a])) ['a' .. 'z']
  getNormalCInfo (NormalC n bt) = pure (n, fieldNames bt)
  getNormalCInfo _ = fail "makeSizeOf is not supported for infix data constructors, record variants or GADTs"
  mkSingleVariantMatch n fields = do
    let clausePat = ConP n [] (VarP <$> fields)
    body <- case fields of
      [] -> pure $ NormalB $ AppE (VarE (mkName "pure")) (TupE [])
      _ -> pure $ NormalB $ DoE Nothing (NoBindS . callEstimateSize . VarE <$> fields)
    pure $ Match clausePat body []


  mkTyApp :: Type -> [Type] -> Type
  mkTyApp =
    foldl' AppT

