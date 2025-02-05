{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
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


module Pact.Core.Repl
  ( runRepl
  , execScript
  , mkReplState
  , renderLocatedPactErrorFromState)
  where

import Control.Lens
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

execScript :: Bool -> FilePath -> IO (Either (PactError FileLocSpanInfo) [ReplCompileValue], ReplState ReplCoreBuiltin)
execScript dolog f = do
  pdb <- mockPactDb serialisePact_repl_fileLocSpanInfo
  ee <- defaultEvalEnv pdb replBuiltinMap
  ref <- newIORef (mkReplState' ee logger)
  v <- evalReplM ref $ loadFile interpretEvalDirect f True
  state <- readIORef ref
  pure (v, state)
  where
  logger :: FileLocSpanInfo -> Text -> EvalM e b i ()
  logger (FileLocSpanInfo file info) v
    | dolog = liftIO $ T.putStrLn $ T.pack file <> ":" <> renderCompactText info <> ": " <> v
    | otherwise = pure ()

-- | Render a nice error
renderLocatedPactErrorFromState :: ReplState b -> PactError FileLocSpanInfo -> Text
renderLocatedPactErrorFromState rstate err = rendered
  where
  replInfo = view peInfo err
  originFile = case rstate ^. replLoadedFiles . at (_flsiFile replInfo) of
            Just sc -> sc
            Nothing -> rstate ^. replCurrSource
  rendered = replError originFile err

runRepl :: IO ()
runRepl = do
  pdb <- mockPactDb serialisePact_repl_fileLocSpanInfo
  ee <- defaultEvalEnv pdb replBuiltinMap
  let display' info rcv = runInputT replSettings (displayOutput info rcv)
  ref <- newIORef (mkReplState' ee display')
  evalReplM ref (runInputT replSettings loop) >>= \case
    Left err -> do
      putStrLn "Exited repl session with error:"
      putStrLn $ T.unpack $ replError (SourceCode "(interactive)" "") err
    _ -> pure ()
  where
  replSettings = Settings (replCompletion replCoreBuiltinNames) (Just ".pc-history") True
  displayOutput :: (Pretty a, MonadIO m) => FileLocSpanInfo -> a -> InputT m ()
  displayOutput _ = outputStrLn . show . pretty
  catch' ma = catchAny ma (\e -> outputStrLn (show e) *> loop)
  defaultSrc = SourceCode "(interactive)" mempty
  inputNeedsToBeConsumed err _input = case err of
    PEParseError (ParsingError e) _info | e == "[')']" -> True
    _ -> False
  inputLoop accumulator = catch' $ do
    minput <- fmap T.pack <$> getInputLine "....> "
    case minput of
      Nothing -> outputStrLn "goodbye"
      Just input | T.null input -> inputLoop accumulator
      Just rest -> catch' $ do
        let src = accumulator <> "\n" <> rest
        lift (replCurrSource .== defaultSrc{_scPayload=src})
        eout <- lift (tryError (interpretReplProgramDirect (SourceCode "(interactive)" src)))
        case eout of
          Right _ -> pure ()
          -- Expected a final `)` in our input, thus, continue chugging
          Left err | inputNeedsToBeConsumed err src -> inputLoop src
          Left err -> do
            rstate <- lift getReplState
            let renderedError = renderLocatedPactErrorFromState rstate err
            lift (replCurrSource .== defaultSrc)
            outputStrLn (T.unpack renderedError)
  loop = do
    minput <- fmap T.pack <$> getInputLine "pact> "
    case minput of
      Nothing -> outputStrLn "goodbye"
      Just input | T.null input -> loop
      Just src -> catch' $ do
        lift (replCurrSource .== defaultSrc{_scPayload=src})
        eout <- lift (tryError (interpretReplProgramDirect (SourceCode "(interactive)" src)))
        case eout of
          Right _ -> pure ()
          -- Expected a final `)` in our input, thus, continue chugging
          Left err | inputNeedsToBeConsumed err src -> inputLoop src
          Left err -> do
            rstate <- lift getReplState
            let renderedError = renderLocatedPactErrorFromState rstate err
            lift (replCurrSource .== defaultSrc)
            outputStrLn (T.unpack renderedError)
        loop
