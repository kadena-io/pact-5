{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}


-- |
-- Module      :  Pact.Core.IR.Typecheck
-- Copyright   :  (C) 2022 Kadena
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jose Cardona <jose@kadena.io>
--
-- Pact core minimal repl
--


module Pact.Core.Repl(runRepl, execScript, mkReplState) where

import Control.Monad.IO.Class
import Control.Exception.Safe
import Control.Monad.Except
import Control.Monad.Trans(lift)
import System.Console.Haskeline
import Data.IORef
import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Pact.Core.Builtin
import Pact.Core.Environment
import Pact.Core.Persistence.MockPersistence
import Pact.Core.Pretty
import Pact.Core.Repl.Compile
import Pact.Core.Repl.Utils
import Pact.Core.Serialise
import Pact.Core.Info
import Pact.Core.Errors

execScript :: Bool -> FilePath -> IO (Either (PactError SpanInfo) [ReplCompileValue], ReplState ReplCoreBuiltin)
execScript dolog f = do
  pdb <- mockPactDb serialisePact_repl_spaninfo
  ee <- defaultEvalEnv pdb replBuiltinMap
  ref <- newIORef (mkReplState ee logger)
  v <- evalReplM ref $ loadFile f interpretEvalDirect
  state <- readIORef ref
  pure (v, state)
  where
  logger :: Text -> EvalM e b i ()
  logger
    | dolog = liftIO . T.putStrLn
    | otherwise = const (pure ())

runRepl :: IO ()
runRepl = do
  pdb <- mockPactDb serialisePact_repl_spaninfo
  ee <- defaultEvalEnv pdb replBuiltinMap
  let display' rcv = runInputT replSettings (displayOutput rcv)
  ref <- newIORef (mkReplState ee display')
  evalReplM ref (runInputT replSettings loop) >>= \case
    Left err -> do
      putStrLn "Exited repl session with error:"
      putStrLn $ T.unpack $ replError (SourceCode "(interactive)" "") err
    _ -> pure ()
  where
  replSettings = Settings (replCompletion replCoreBuiltinNames) (Just ".pc-history") True
  displayOutput :: (Pretty a, MonadIO m) => a -> InputT m ()
  displayOutput = outputStrLn . show . pretty
  catch' ma = catchAny ma (\e -> outputStrLn (show e) *> loop)
  defaultSrc = SourceCode "(interactive)" mempty
  loop = do
    minput <- fmap T.pack <$> getInputLine "pact>"
    case minput of
      Nothing -> outputStrLn "goodbye"
      Just input | T.null input -> loop
      Just src -> catch' $ do
        lift (replCurrSource .== defaultSrc{_scPayload=src})
        eout <- lift (tryError (interpretReplProgramDirect (SourceCode "(interactive)" src)))
        case eout of
          Right _ -> pure ()
          Left err -> do
            rs <- lift (useReplState replCurrSource)
            lift (replCurrSource .== defaultSrc)
            outputStrLn (T.unpack (replError rs err))
        loop
