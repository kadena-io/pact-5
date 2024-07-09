{-# LANGUAGE TemplateHaskell #-}

module Pact.Core.Repl.BuiltinDocs
  ( topLevelHasDocs
  , builtinDocs
  ) where

import Data.Text (Text)
import Pact.Core.Syntax.ParseTree
import Pact.Core.Names

import qualified Data.Map.Strict as M
import Pact.Core.Repl.BuiltinDocs.Internal

-- Only used for the REPL output.
topLevelHasDocs :: TopLevel i -> Maybe Text
topLevelHasDocs (TLTerm term) = case term of
  Var (BN (BareName bn)) _ -> _markdownDoc <$> M.lookup bn builtinDocs
  Operator op _ -> _markdownDoc <$> M.lookup (renderOp op) builtinDocs
  _ -> mempty
topLevelHasDocs _ = Nothing

builtinDocs :: M.Map Text MarkdownDoc
builtinDocs = $$mkBuiltinDocs
