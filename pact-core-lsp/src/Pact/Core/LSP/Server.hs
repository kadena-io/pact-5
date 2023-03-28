-- |
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Pact.Core.LSP.Server where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Aeson as A
import qualified Data.Text as T
import           Language.LSP.Server
import qualified Language.LSP.Types as J

import           Pact.Core.LSP.Types
import           Pact.Core.LSP.Handlers

import Data.Map.Strict (empty)
import System.IO (Handle, stdin, stdout, withFile)
import Colog.Core
import qualified Colog.Core as L
import Data.Text.Prettyprint.Doc (viaShow)
import Pact.Core.Pretty (pretty)
import Language.LSP.Logging (defaultClientLogger)
import Language.LSP.Types (TextDocumentSyncOptions(..)
                          ,TextDocumentSyncKind (..))
import System.Environment (getArgs)
import GHC.IO.IOMode (IOMode(WriteMode))
import Data.Text (Text)

run :: IO ()
run = getArgs >>= \case
  ["--debug"] -> withFile "pact-lsp-debug.log" WriteMode
    (runWith stdin stdout . L.logStringHandle)
  _otherwise ->  runWith stdin stdout L.logStringStderr

runWith :: Handle -> Handle -> LogAction IO String -> IO ()
runWith i o l = do
  -- _ssPactDb <- mockPactDb
  let
    _ssLoaded = empty
    _ssCache = empty
  state <- newMVar (ServerState mempty)

  let
    defaultConfig = ServerConfig "pact"
    onConfigurationChange :: ServerConfig -> A.Value -> Either Text ServerConfig
    onConfigurationChange _ new = case A.fromJSON new of
                                      A.Success cfg -> Right cfg
                                      A.Error msg   -> Left (T.pack msg)
    doInitialize env _ = pure (Right env)
    staticHandlers = mconcat
      [ initializeHandler
      , documentOpenNotificationHandler
      --, completionRequestHandler
      , hoverRequestHandler
      , documentSaveNotificationHandler
      , documentCloseNotificationHandler
      , documentChangeNotificationHandler
      , workspaceChangeNotificationHandler
      ]

    forward :: LanguageContextEnv ServerConfig -> HandlerM a -> IO a
    forward env handler =
      modifyMVar state \old -> do
        runLspT env $ do
          (res, st) <- runHandlerM handler old
          result <- case res of
            Left errMsg -> do
              -- send user notification for failure
              sendNotification J.SWindowLogMessage
                J.LogMessageParams{_xtype = J.MtError
                                  ,_message = T.pack (show errMsg)}
              liftIO (fail (show errMsg))

            Right a -> pure a
          pure (st, result)

    interpretHandler e = Iso (forward e) liftIO
    options = defaultOptions{textDocumentSync = Just syncOpt}

  void (runServerWithHandles ioLogger lspLogger i o ServerDefinition{..})
  where
    prettyMsg m = "[" <> viaShow (L.getSeverity m) <> "] "
      <> pretty (L.getMsg m)
    ioLogger :: LogAction IO (WithSeverity LspServerLog)
    ioLogger = L.cmap (show . prettyMsg) l
    lspLogger :: LogAction (LspM config) (WithSeverity LspServerLog)
    lspLogger =
      let clientLogger = L.cmap (fmap (T.pack . show . pretty))
            defaultClientLogger
      in clientLogger <> L.hoistLogAction liftIO ioLogger

    syncOpt :: TextDocumentSyncOptions
    syncOpt = TextDocumentSyncOptions
      { _openClose = Just True
      , _change = Just TdSyncIncremental
      , _willSave = Just False
      , _willSaveWaitUntil = Just False
      , _save = Just (J.InR (J.SaveOptions (Just True)))
      }
