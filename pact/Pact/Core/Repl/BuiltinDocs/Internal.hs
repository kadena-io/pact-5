-- |
{-# LANGUAGE DeriveLift #-}

module Pact.Core.Repl.BuiltinDocs.Internal where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import System.Directory
import System.FilePath

import Control.Monad
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Data.Functor

listBuiltinDocs :: IO [FilePath]
listBuiltinDocs = do
  let baseDir = "docs/builtins"
  cats <- listDirectory baseDir
  files <- forM cats $ \cat -> do
    let catPath = baseDir </> cat
    docs <- listDirectory catPath
      <&> filter (\f -> takeExtension f == ".md")
    pure $ (catPath </>) <$> docs
  pure $ concat files


embedIO :: Lift a => IO a -> Code Q a
embedIO action = runIO action `bindCode` liftTyped

newtype MarkdownDoc
  = MarkdownDoc { _markdownDoc :: T.Text }
  deriving (Lift, Eq, Show)

mkBuiltinDocs :: Code Q (M.Map T.Text MarkdownDoc)
mkBuiltinDocs = embedIO action
  where
  action = do
    files <- listBuiltinDocs
    cnt <- forM files $ \f -> do
      let bname = takeBaseName f
      content <- T.readFile f
      pure (normalizedNameToBuiltin $ T.pack bname, MarkdownDoc content)
    pure $ M.fromList cnt


builtinToNormalizedName :: T.Text -> T.Text
builtinToNormalizedName = \case
  "!=" -> "neq"
  "&" -> "and"
  "*" -> "mult"
  "+" -> "add"
  "-" -> "sub"
  "/" -> "div"
  "<" -> "lt"
  "<=" -> "leq"
  "=" -> "eq"
  ">" -> "gt"
  ">=" -> "geq"
  "^" -> "pow"
  "and?" -> "and-q"
  "not?" -> "not-q"
  "or?" -> "or-q"
  "|" -> "bitwise-or"
  "~" -> "bitwise-reverse"
  "begin-named-tx" -> "begin-tx"
  "continue-pact-rollback-yield" -> "continue-pact"
  "continue-pact-rollback-yield-object" -> "continue-pact"
  "continue-pact-with-rollback" -> "continue-pact"
  "enforce-pact-version-range" -> "enforce-pact-version"
  "env-set-gas" -> "env-gas"
  "expect-failure-match" -> "expect-failure"
  other -> other

normalizedNameToBuiltin :: T.Text -> T.Text
normalizedNameToBuiltin = \case
  "neq" -> "!="
  "and" -> "&"
  "mult" -> "*"
  "add" -> "+"
  "sub" -> "-"
  "div" -> "/"
  "lt" -> "<"
  "leq" -> "<="
  "eq" -> "="
  "gt" -> ">"
  "geq" -> ">="
  "pow" -> "^"
  "and-q" -> "and?"
  "not-q" -> "not?"
  "or-q" -> "or?"
  "bitwise-or" -> "|"
  "bitwise-reverse" -> "~"
  other -> other
