{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Pact.Core.Literal
 ( _LString
 , _LInteger
 , _LDecimal
 , _LUnit
 , _LBool
 , Literal(..)) where

import Control.Lens(makePrisms)
import Data.Text(Text)
import Data.Decimal

import Pact.Core.Pretty

data Literal
  = LString !Text
  | LInteger !Integer
  | LDecimal !Decimal
  | LUnit
  | LBool !Bool
  deriving (Show, Eq)

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
