{-# LANGUAGE LambdaCase #-}
module Main where

import Pact.Core.Repl
import Pact.Core.LanguageServer
import System.Environment


main :: IO ()
main = getArgs >>= \case
  ["--lsp"] -> startServer
  _ -> runRepl
