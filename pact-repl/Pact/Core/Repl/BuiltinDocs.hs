{-# LANGUAGE TemplateHaskell #-}

module Pact.Core.Repl.BuiltinDocs
  ( topLevelHasDocs
  , builtinDocs
  , renderBuiltinDoc
  ) where

import Data.Text (Text)
import Control.Monad
import Pact.Core.Syntax.ParseTree
import Pact.Core.Names

import qualified Data.Map.Strict as M
import Pact.Core.Repl.BuiltinDocs.Internal

import Text.Pandoc

-- Only used for the REPL output.
topLevelHasDocs :: TopLevel i -> Maybe Text
topLevelHasDocs (TLTerm term) = case term of
  Var (BN (BareName bn)) _ -> _markdownDoc <$> M.lookup bn builtinDocs
  _ -> mempty
topLevelHasDocs _ = Nothing

builtinDocs :: M.Map Text MarkdownDoc
builtinDocs = $$mkBuiltinDocs

renderBuiltinDoc :: Text -> IO (Either PandocError Text)
renderBuiltinDoc = runIO . (readMarkdown readerOpts >=> writeANSI writerOpts)

readerOpts :: ReaderOptions
readerOpts = def
  { readerExtensions = pandocExtensions
  }

writerOpts :: WriterOptions
writerOpts = def
  { writerExtensions = pandocExtensions
  }
