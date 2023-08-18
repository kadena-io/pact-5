{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      :  Pact.Core.Repl
-- Copyright   :  (C) 2023 Kadena
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jose Cardona <jose@kadena.io>
--
-- Pact core minimal repl
--
module Main
( main
) where

import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Trans(lift)
import Control.Monad.IO.Class(liftIO)
import System.Console.Haskeline
import Data.IORef
import Data.Foldable(traverse_)

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Pact.Core.Persistence
import Pact.Core.Pretty
import Pact.Core.Builtin

import Pact.Core.Compile
import Pact.Core.Repl.Compile
import Pact.Core.Repl.Command
import Pact.Core.Repl.Types


main :: IO ()
main = do
  pactDb <- mockPactDb
  g <- newIORef mempty
  evalLog <- newIORef Nothing
  ref <- newIORef (ReplState mempty mempty pactDb g evalLog)
  runReplM ref (runInputT replSettings loop) >>= \case
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
        IPClosure -> outputStrLn "<<closure>>"
    RLoadedDefun mn ->
      outputStrLn $ show $
        "loaded defun" <+> pretty mn
    RLoadedDefConst mn ->
      outputStrLn $ show $
        "loaded defconst" <+> pretty mn
    -- InterpretValue v _ -> outputStrLn (show (pretty v))
    -- InterpretLog t -> outputStrLn (T.unpack t)
  catch' ma = catchAll ma (\e -> outputStrLn (show e) *> loop)
  loop = do
    minput <- fmap T.pack <$> getInputLine "pact>"
    case minput of
      Nothing -> outputStrLn "goodbye"
      Just input | T.null input -> loop
      Just input -> case parseReplActionText (T.strip input) of
        Nothing -> do
          outputStrLn "Error: Expected supported command or expression. See :help for more information."
          outputStrLn $ show $ parseReplActionText input
          loop
        Just ra -> case ra of
          RALoad txt -> let
            file = T.unpack txt
            in catch' $ do
              source <- liftIO (B.readFile file)
              eout <- lift $ tryError $ interpretReplProgram source
              case eout of
                Right vs -> traverse_ displayOutput vs
                Left err -> let
                  rs = ReplSource (T.pack file) (T.decodeUtf8 source)
                  in outputStrLn (T.unpack (replError rs err))
              loop
          RAShowHelp -> do
            helpCommand
            loop
          RASetDebugFlag flag -> do
            debugCommand flag
            loop
          RAExecuteExpr src -> catch' $ do
            eout <- lift (tryError (interpretReplProgram (T.encodeUtf8 src)))
            case eout of
              Right out -> traverse_ displayOutput out
              Left err -> let
                rs = ReplSource "(interactive)" input
                in outputStrLn (T.unpack (replError rs err))
            loop
