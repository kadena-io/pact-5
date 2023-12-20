{-# LANGUAGE OverloadedStrings #-}
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
import qualified Data.Set as Set

import Pact.Core.Builtin
import Pact.Core.Compile
import Pact.Core.Environment
import Pact.Core.Hash
import Pact.Core.Imports
import Pact.Core.Persistence.MockPersistence
import Pact.Core.Pretty
import Pact.Core.Repl.Compile
import Pact.Core.Repl.Utils
import Pact.Core.Serialise

main :: IO ()
main = do
  pdb <- mockPactDb serialisePact_repl_spaninfo
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
      LoadedModule mn mh -> outputStrLn $ show $
        "loaded module" <+> pretty mn <> ", hash" <+> pretty (moduleHashToText mh)
      LoadedInterface mn mh -> outputStrLn $ show $
        "loaded interface" <+> pretty mn <> ", hash" <+> pretty (moduleHashToText mh)
      InterpretValue v _ -> outputStrLn (show (pretty v))
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
            lift (replCurrSource .= defaultSrc{_scPayload=src})
            eout <- lift (tryError (interpretReplProgramSmallStep (SourceCode "(interactive)" src) display'))
            case eout of
              Right _ -> pure ()
              Left err -> do
                SourceCode srcFile currSrcText <- lift (use replCurrSource)
                let rs = ReplSource (T.pack srcFile) currSrcText
                lift (replCurrSource .= defaultSrc)
                outputStrLn (T.unpack (replError rs err))
            loop
