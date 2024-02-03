{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Pact.Core.Literal
 ( _LString
 , _LInteger
 , _LDecimal
 , _LUnit
 , _LBool
 , Literal(..)
 ) where

import Control.Lens(makePrisms)
import Data.Data(Data)
import Data.Text(Text)
import Data.Decimal

import Control.DeepSeq
import GHC.Generics

import Pact.Core.Pretty

deriving instance Data Decimal

data Literal
  = LString !Text
  | LInteger !Integer
  | LDecimal !Decimal
  | LUnit
  | LBool !Bool
  deriving (Show, Eq, Ord, Generic, Data)

instance NFData Literal

makePrisms ''Literal

instance Pretty Literal where
  pretty = \case
    LString t -> dquotes (pretty t)
    LInteger i -> pretty i
    LDecimal d ->
      if roundTo 0 d == d then
        pretty (show (roundTo 0 d)) <> ".0"
      else pretty (show d)
    LUnit -> "()"
    LBool b -> if b then "true" else "false"
