{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
-- |
-- Module       : Pact.Core.Repl.Commands
-- Copyright    : (c) 2023, Kadena LLC
-- License      : BSD-style
--
-- Maintainer   : Jose Cardona <jose@kadena.io>
--
-- This module contains the datatypes and utilities for the
-- core Pact repl commands, as well as their semantics and
-- specification.
--
module Pact.Core.Repl.Command
( -- * commands
  debugCommand
, helpCommand
 -- * utils
, replCompletion
, replError
) where

import Control.Lens
import Control.Monad.Reader

import qualified Data.List as List
import Data.Text(Text)
import Data.List(isPrefixOf)
import Data.Maybe(mapMaybe)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import Pact.Core.Info
import Pact.Core.Names
import Pact.Core.Persistence
import Pact.Core.Pretty

import Pact.Core.Errors
import qualified Pact.Core.IR.Term as Term
import Pact.Core.Repl.Types
import System.Console.Haskeline



-- -------------------------------------------------------------------- --
-- Debug command

-- | The individual action of the debug command
debugCommand :: DebugFlagUpdate -> InputT (ReplM b) ()
debugCommand = \case
  None -> do
    lift (replFlags .= Set.empty)
    outputStrLn $ unwords ["Disabling all debug flags"]
  All -> do
    lift (replFlags .= Set.fromList [minBound .. maxBound])
    outputStrLn $ unwords ["Enabling all debug flags"]
  Some flag -> do
    lift (replFlags %= Set.insert flag)
    outputStrLn $ unwords ["set debug flag for", prettyReplDebugFlag flag]

-- -------------------------------------------------------------------- --
-- Help command

-- | The individual action of the help command
--
helpCommand :: InputT (ReplM b) ()
helpCommand = outputStrLn $ List.intercalate "\n"
  [ "The following commands are supported by the Pact repl:"
  , ""
  , "   <expression>      evaluate and run a pact expression"
  , "   :help             display all available commands"
  , "   :load  <file> ... load .pact or .repl files and their dependents"
  , "   :debug <flag> ... set a debug flag in the repl"
  , ""
  , "   options for debug flags: "
  , ""
  , "     lexer           show lexer phase information"
  , "     parser          show parse phase information"
  , "     desugar         show desugaring phase information"
  , "     tc-term         show typecheck phase information"
  , "     tc-type         show inferred type information"
  , "     specializer     show specializer phase information"
  , "     untyped-core    show untyped core phase information"
  , ""
  ]


-- -------------------------------------------------------------------- --
-- Completion and Error reporting

-- | Repl name and command completion
--
replCompletion
  :: [Text]
  -- ^ natives
  -> CompletionFunc (ReplM b)
replCompletion natives =
  completeQuotedWord (Just '\\') "\"" listFiles $
  completeWord (Just '\\') filenameWordBreakChars $ \str -> do
    tlns <- uses (replLoaded . loToplevel) Map.keys
    moduleNames <- uses (replLoaded . loModules) (fmap renderModuleName . Map.keys)
    prefixedNames <- uses (replLoaded . loModules) toPrefixed
    let
      cmds = [":load", ":type", ":debug"]
      allNames = Set.fromList $ T.unpack <$> concat
        [tlns, moduleNames, prefixedNames, natives, cmds]
    pure $ simpleCompletion <$> Set.toList (Set.filter (str `isPrefixOf`) allNames)
  where
    defNames = \case
      ModuleData md _ ->
        Term.defName <$> Term._mDefs md
      InterfaceData iface _ ->
        fmap Term._dcName $ mapMaybe (preview Term._IfDConst) $ Term._ifDefns iface
      -- fmap Term.defName . Term._mDefs . _mdModule
    toPrefixed m =
      concat $ prefixF <$> Map.toList m
    prefixF (mn, ems) = let
      dns = defNames ems
      in fmap ((renderModuleName mn <> ".") <>) dns

-- | Repl error reporting
--
replError
  :: ReplSource
  -> PactErrorI
  -> Text
replError (ReplSource file src) pe =
  let srcLines = T.lines src
      pei = view peInfo pe
      slice = withLine (_liStartLine pei) $ take (max 1 (_liEndLine pei)) $ drop (_liStartLine pei) srcLines
      colMarker = "  | " <> T.replicate (_liStartColumn pei) " " <> T.replicate (max 1 (_liEndColumn pei - _liStartColumn pei)) "^"
      errRender = renderText pe
      fileErr = file <> ":" <> T.pack (show (_liStartLine pei)) <> ":" <> T.pack (show (_liStartColumn pei)) <> ": "
  in T.unlines ([fileErr <> errRender] ++ slice ++ [colMarker])
  where
    withLine st lns = zipWith (\i e -> T.pack (show i) <> " | " <> e) [st ..] lns
