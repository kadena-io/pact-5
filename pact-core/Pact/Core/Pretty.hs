{-# LANGUAGE OverloadedStrings #-}

module Pact.Core.Pretty
( module Pretty
, renderCompactString
, renderCompactString'
, renderText
, renderText'
, prettyCommaSep
, prettyCommaSepNE
) where

import Data.Text(Text)
import Prettyprinter
import Prettyprinter as Pretty
import Prettyprinter.Render.String
import Prettyprinter.Render.Text
import Data.List(intersperse)
import Data.List.NonEmpty(NonEmpty)
import Data.Foldable(fold)

import qualified Data.List.NonEmpty as NE

renderCompactString :: Pretty a => a -> String
renderCompactString = renderString . layoutPretty defaultLayoutOptions . pretty

renderCompactString' :: Doc ann -> String
renderCompactString' = renderString . layoutPretty defaultLayoutOptions

renderText :: Pretty a => a -> Text
renderText = renderStrict . layoutPretty defaultLayoutOptions . pretty

renderText' :: Doc ann -> Text
renderText' = renderStrict . layoutPretty defaultLayoutOptions

prettyCommaSepNE :: Pretty a => NonEmpty a -> Doc ann
prettyCommaSepNE = fold . NE.intersperse ", " . fmap pretty

prettyCommaSep :: Pretty a => [a] -> Doc ann
prettyCommaSep = fold . intersperse ", " . fmap pretty
