{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}


-- |
-- Module      :  Pact.Core.IR.Typecheck
-- Copyright   :  (C) 2022 Kadena
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jose Cardona <jose@kadena.io>
--
-- Pact core minimal repl
--


module Main where

import Control.Lens
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Trans(lift)
import System.Console.Haskeline
import Data.Default
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Set as Set

--import Pact.Core.Persistence
import Pact.Core.Persistence.MockPersistence
import Pact.Core.Pretty
import Pact.Core.Builtin
import Pact.Core.Names
import Pact.Core.Interpreter

import Pact.Core.Compile
import Pact.Core.Repl.Compile
import Pact.Core.Repl.Utils
import Pact.Core.Environment
import Pact.Core.Imports

main :: IO ()
main = do
  pdb <- mockPactDb
  g <- newIORef mempty
  evalLog <- newIORef Nothing
  let ee = defaultEvalEnv pdb replRawBuiltinMap
  ref <- newIORef (ReplState mempty pdb def ee g evalLog defaultSrc Nothing)
  runReplT ref (runInputT replSettings loop) >>= \case
    Left err -> do
      putStrLn "Exited repl session with error:"
      putStrLn $ T.unpack $ replError (ReplSource "(interactive)" "") err
    _ -> pure ()
  where
  replSettings = Settings (replCompletion rawBuiltinNames) (Just ".pc-history") True
  displayOutput = \case
    RCompileValue cv -> case cv of
      LoadedModule mn -> outputStrLn $ show $
        "loaded module" <+> pretty mn
      LoadedInterface mn -> outputStrLn $ show $
        "Loaded interface" <+> pretty mn
      InterpretValue iv -> case iv of
        IPV v _ -> outputStrLn (show (pretty v))
        IPTable (TableName tn) -> outputStrLn $ "table{" <> T.unpack tn <> "}"
        IPClosure -> outputStrLn "<<closure>>"
      LoadedImports i ->
        outputStrLn $ "loaded imports from" <> show (pretty (_impModuleName i))
    RLoadedDefun mn ->
      outputStrLn $ show $
        "loaded defun" <+> pretty mn
    RLoadedDefConst mn ->
      outputStrLn $ show $
        "loaded defconst" <+> pretty mn
  catch' ma = catchAll ma (\e -> outputStrLn (show e) *> loop)
  defaultSrc = SourceCode "(interactive)" mempty
  loop = do
    minput <- fmap T.pack <$> getInputLine "pact>"
    case minput of
      Nothing -> outputStrLn "goodbye"
      Just input | T.null input -> loop
      Just input -> case parseReplAction (T.strip input) of
        Nothing -> do
          outputStrLn "Error: Expected command [:load, :type, :syntax, :debug] or expression"
          loop
        Just ra -> case ra of
          RASetFlag flag -> do
            lift (replFlags %= Set.insert flag)
            outputStrLn $ unwords ["set debug flag for", prettyReplFlag flag]
            loop
          RADebugAll -> do
            lift (replFlags .= Set.fromList [minBound .. maxBound])
            outputStrLn $ unwords ["set all debug flags"]
            loop
          RADebugNone -> do
            lift (replFlags .= Set.empty)
            outputStrLn $ unwords ["Remove all debug flags"]
            loop
          RAExecuteExpr src -> catch' $ do
            let display' rcv = runInputT replSettings (displayOutput rcv)
            let sourceBs = T.encodeUtf8 src
            lift (replCurrSource .= defaultSrc{_scPayload=sourceBs})
            eout <- lift (tryError (interpretReplProgram (SourceCode "(interactive)" sourceBs) display'))
            case eout of
              Right _ -> pure ()
              Left err -> do
                SourceCode srcFile currSrc <- lift (use replCurrSource)
                let srcText = T.decodeUtf8 currSrc
                let rs = ReplSource (T.pack srcFile) srcText
                lift (replCurrSource .= defaultSrc)
                outputStrLn (T.unpack (replError rs err))
            loop
