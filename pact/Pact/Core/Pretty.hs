{-# LANGUAGE OverloadedStrings #-}

module Pact.Core.Pretty
( module Pretty
, renderCompactString
, renderCompactString'
, renderCompactText
, renderCompactText'
, renderText
, renderText'
, commaSep
, commaSepNE
, commaBraces
, commaBrackets
, bracketsSep
, parensSep
, bracesSep
, PrettyLispApp(..)
, AbbrevPretty(..)
, prettyAbbrevText
, prettyAbbrevText'
) where

import Data.Text(Text)
import Prettyprinter
import qualified Prettyprinter as Pretty
import qualified Data.Text as T
import Prettyprinter.Render.String
import Prettyprinter.Render.Text
import Data.List.NonEmpty(NonEmpty)

import qualified Data.List.NonEmpty as NE

renderCompactString :: Pretty a => a -> String
renderCompactString = renderString . layoutPretty defaultLayoutOptions . pretty

renderCompactString' :: Doc ann -> String
renderCompactString' = renderString . layoutPretty defaultLayoutOptions

renderCompactText :: Pretty a => a -> Text
renderCompactText = renderStrict . layoutPretty defaultLayoutOptions . pretty

renderCompactText' :: Doc ann -> Text
renderCompactText' = renderStrict . layoutPretty defaultLayoutOptions

renderText :: Pretty a => a -> Text
renderText = renderStrict . layoutPretty defaultLayoutOptions . pretty

renderText' :: Doc ann -> Text
renderText' = renderStrict . layoutPretty defaultLayoutOptions

commaSepNE :: Pretty a => NonEmpty a -> Doc ann
commaSepNE = commaSep . NE.toList

commaSep :: Pretty a => [a] -> Doc ann
commaSep = Pretty.hsep . Pretty.punctuate "," . fmap pretty

commaBraces, commaBrackets, bracketsSep, parensSep, bracesSep :: [Doc ann] -> Doc ann
commaBraces   = encloseSep "{" "}" ","
commaBrackets = encloseSep "[" "]" ","
bracketsSep   = brackets . sep
parensSep     = parens   . sep
bracesSep     = braces   . sep

-- A useful type for "abbreviated"
-- values
newtype AbbrevPretty a
  = AbbrevPretty a
  deriving (Show)

-- | Very inefficient, but useful for making pretty printed, abbreviated strings, to display
-- things like long lists as [1, 2, ...]
-- Invariant: `lim` should be at least > 3, otherwise you'll get an ugly string
prettyAbbrevText :: Pretty a => Int -> a -> Doc ann
prettyAbbrevText lim v =
  let k = renderCompactText v
  in if T.length k <= (lim - 3) then pretty k
  else pretty (T.take (lim - 3) k <> "...")

-- | Very inefficient, but useful for making pretty printed, abbreviated strings, to display
-- things like long lists as [1, 2, ...]
-- Invariant: `lim` should be at least > 3, otherwise you'll get an ugly string
prettyAbbrevText' :: Int -> Doc ann -> Doc ann
prettyAbbrevText' lim v =
  let k = renderCompactText' v
  in if T.length k <= (lim - 3) then pretty k
  else pretty (T.take (lim - 3) k <> "...")

data PrettyLispApp n arg
  = PrettyLispApp
  { _plOperator :: n
  , _plOperands :: [arg]
  } deriving (Show)

instance (Pretty n, Pretty arg) => Pretty (PrettyLispApp n arg) where
  pretty (PrettyLispApp n args) =
    parens (pretty n <> if null args then mempty else space <> hsep (pretty <$> args))
