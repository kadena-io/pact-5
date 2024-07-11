{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE CPP #-}

module Pact.Core.Literal
 ( _LString
 , _LInteger
 , _LDecimal
 , _LUnit
 , _LBool
 , Literal(..)
 , parseNumLiteral) where

import Control.Applicative
import Control.Lens(makePrisms)
import Data.Text(Text)
import Data.Void(Void)
import Data.Decimal

import Control.DeepSeq
import GHC.Generics

import Pact.Core.Pretty
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import Data.Char (digitToInt)

#if !MIN_VERSION_base(4,20,0)
import Data.List(foldl')
#endif

data Literal
  = LString !Text
  | LInteger !Integer
  | LDecimal !Decimal
  | LUnit
  | LBool !Bool
  deriving (Show, Eq, Ord, Generic)

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

numberParser :: MP.Parsec Void Text Literal
numberParser = do
  neg <- maybe id (const negate) <$> optional (MP.char '-')
  num <- some MP.digitChar
  dec <- optional (MP.char '.' *> some MP.digitChar)
  MP.eof
  let strToNum = foldl' (\x d -> 10*x + toInteger (digitToInt d))
  case dec of
    Nothing -> return $ LInteger (neg (strToNum 0 num))
    Just d ->
      let precision = length d
      in if precision > 255
         then fail $ "decimal precision overflow (255 max): " ++ show num ++ "." ++ show d
         else return $ LDecimal $ Decimal
           (fromIntegral precision)
           (neg (strToNum (strToNum 0 num) d))

parseNumLiteral :: Text -> Maybe Literal
parseNumLiteral = MP.parseMaybe numberParser
