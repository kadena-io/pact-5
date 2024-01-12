{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Pact.Core.Repl
import Pact.Core.Builtin
import Pact.Core.LanguageServer

import qualified Options.Applicative as O
import Control.Applicative ((<|>))

import qualified PackageInfo_pact_core as PI
import Data.Version (showVersion)
import qualified Data.Text.IO as T
import Data.Foldable

data ReplOpts
  = OVersion
  | OBuiltins
  | OLanguageServer

replOpts :: O.Parser (Maybe ReplOpts)
replOpts = O.optional $
  O.flag' OVersion (O.short 'v' <> O.long "version" <> O.help "Display version")
  <|> O.flag' OBuiltins (O.short 'b' <> O.long "builtins" <> O.help "List builtins")
  <|> O.flag' OLanguageServer (O.long "lsp" <> O.help "Start Language Server")

argParser :: O.ParserInfo (Maybe ReplOpts)
argParser = O.info (O.helper <*> replOpts)
            (O.fullDesc <> O.header "The Pact Smart Contract Language Interpreter")


main :: IO ()
main = O.execParser argParser >>= \case
  Just OVersion -> printVersion
  Just OBuiltins -> printBuiltins
  Just OLanguageServer -> startLSP
  Nothing -> runRepl
  where
    printVersion = putStrLn ("pact version " <> showVersion PI.version)
    printBuiltins = traverse_ (\bi -> T.putStrLn $ "\"" <> bi <> "\"") replcoreBuiltinNames
