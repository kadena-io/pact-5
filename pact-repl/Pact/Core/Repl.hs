{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}


-- |
-- Module      :  Pact.Core.IR.Typecheck
-- Copyright   :  (C) 2022 Kadena
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jose Cardona <jose@kadena.io>
--
-- Pact core minimal repl
--


module Pact.Core.Repl(runRepl, execScript, defaultReplState) where

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
import Control.Lens
import qualified Data.Map.Strict as M

execScript :: Bool -> FilePath -> IO (Either (PactError FileLocSpanInfo) [ReplCompileValue])
execScript dolog f = do
  ref <- newIORef =<< defaultReplState logger
  runReplT ref $ loadFile interpretEvalDirect f True
  where
  logger :: Text -> EvalM e b i ()
  logger
    | dolog = liftIO . T.putStrLn
    | otherwise = const (pure ())


defaultReplState :: (forall b. Text -> EvalM 'ReplRuntime b FileLocSpanInfo ()) -> IO (ReplState ReplCoreBuiltin)
defaultReplState dolog = do
  pdb <- mockPactDb serialisePact_repl_flspaninfo
  ee <- defaultEvalEnv pdb replBuiltinMap
  let rstate = ReplState
          { _replLogType = ReplStdOut
          , _replUserDocs= mempty
          , _replTx = Nothing
          , _replTLDefPos = mempty
          , _replOutputLine = dolog
          , _replNativesEnabled = False
          , _replLoadedFiles = mempty
          , _replLoad = defaultLoadFile
          , _replFlags = mempty
          , _replEvalEnv = ee
          , _replCurrSource = defaultSrc}
  pure rstate
  where
  defaultSrc = SourceCode "(interactive)" mempty


runRepl :: IO ()
runRepl = do
  let display' rcv = runInputT replSettings (displayOutput rcv)
  ref <- newIORef =<< defaultReplState display'
  runReplT ref (runInputT replSettings loop) >>= \case
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
            let replInfo = view peInfo err
            rs <- lift (usesReplState replLoadedFiles (M.lookup (_flsiFile replInfo))) >>= \case
              Just sc -> pure sc
              Nothing -> lift (useReplState replCurrSource)
            lift (replCurrSource .== defaultSrc)
            outputStrLn (T.unpack (replError rs err))
        loop
