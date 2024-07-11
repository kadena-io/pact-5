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


module Pact.Core.Repl(runRepl, execScript) where

import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Trans(lift)
import System.Console.Haskeline
import Data.IORef
import qualified Data.Text as T
import qualified Data.Set as Set

import Pact.Core.Builtin
import Pact.Core.Environment
import Pact.Core.Persistence.MockPersistence
import Pact.Core.Pretty
import Pact.Core.Repl.Compile
import Pact.Core.Repl.Utils
import Pact.Core.Serialise
import Pact.Core.Info
import Pact.Core.Errors

execScript :: Bool -> FilePath -> IO (Either (PactError SpanInfo) [ReplCompileValue])
execScript dolog f = do
  pdb <- mockPactDb serialisePact_repl_spaninfo
  evalLog <- newIORef Nothing
  ee <- defaultEvalEnv pdb replBuiltinMap
  ref <- newIORef (ReplState mempty ee evalLog defaultSrc mempty mempty Nothing False)
  runReplT ref $ loadFile f interpretEvalDirect logger
  where
  defaultSrc = SourceCode "(interactive)" mempty
  logger
    | dolog = liftIO . print . pretty
    | otherwise = const (pure ())

runRepl :: IO ()
runRepl = do
  pdb <- mockPactDb serialisePact_repl_spaninfo
  evalLog <- newIORef Nothing
  ee <- defaultEvalEnv pdb replBuiltinMap
  ref <- newIORef (ReplState mempty ee evalLog defaultSrc mempty mempty Nothing False)
  runReplT ref (runInputT replSettings loop) >>= \case
    Left err -> do
      putStrLn "Exited repl session with error:"
      putStrLn $ T.unpack $ replError (SourceCode "(interactive)" "") err
    _ -> pure ()
  where

  replSettings = Settings (replCompletion replCoreBuiltinNames) (Just ".pc-history") True
  displayOutput = outputStrLn . show . pretty
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
            lift (replFlags %== Set.insert flag)
            outputStrLn $ unwords ["set debug flag for", prettyReplFlag flag]
            loop
          RADebugAll -> do
            lift (replFlags .== Set.fromList [minBound .. maxBound])
            outputStrLn $ unwords ["set all debug flags"]
            loop
          RADebugNone -> do
            lift (replFlags .== Set.empty)
            outputStrLn $ unwords ["Remove all debug flags"]
            loop
          RAExecuteExpr src -> catch' $ do
            let display' rcv = runInputT replSettings (displayOutput rcv)
            lift (replCurrSource .== defaultSrc{_scPayload=src})
            eout <- lift (tryError (interpretReplProgramDirect (SourceCode "(interactive)" src) display'))
            case eout of
              Right _ -> pure ()
              Left err -> do
                rs <- lift (useReplState replCurrSource)
                lift (replCurrSource .== defaultSrc)
                outputStrLn (T.unpack (replError rs err))
            loop
