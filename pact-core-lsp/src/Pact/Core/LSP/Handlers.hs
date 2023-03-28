{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Pact.Core.LSP.Handlers where

import Language.LSP.Server (Handlers, notificationHandler, getVirtualFile,
                            LspT, sendNotification, requestHandler)
import Language.LSP.Types
import Pact.Core.LSP.Types
import Control.Lens ((^.), use, modifying)
import Language.LSP.Types.Lens
import Control.Monad.Trans (MonadTrans (..), liftIO)
import qualified Language.LSP.VFS as VFS
import Data.Text.Utf16.Rope (toText)
import Data.Text.Encoding (encodeUtf8)


import qualified Pact.Core.Errors as P
import Control.Lens.Getter (view)
import qualified Pact.Core.Info as P
import Pact.Core.Builtin (RawBuiltin, CoreBuiltin)
import qualified Data.Map.Strict as M
import Control.Monad.Except (MonadError(..), runExceptT)
import qualified Data.Text as T
import Pact.Core.Typed.Term (TopLevel(..))
import qualified Pact.Core.Syntax.Lisp.Lexer as Lisp
import Pact.Core.LSP.AnalyzeSource (analyzeSource, AnalyzeError (..), runAnalyze, AnalyzeState (..), AnalyzeResult (..))
import Control.Lens.Operators

liftLsp :: LspT ServerConfig IO a -> HandlerM a
liftLsp = HandlerM . lift . lift

initializeHandler :: Handlers HandlerM
initializeHandler = notificationHandler SInitialized $ const (pure ())

documentChangeNotificationHandler :: Handlers HandlerM
documentChangeNotificationHandler = notificationHandler STextDocumentDidChange $ const (pure ())

workspaceChangeNotificationHandler :: Handlers HandlerM
workspaceChangeNotificationHandler = notificationHandler SWorkspaceDidChangeConfiguration $ const (pure ())

documentOpenNotificationHandler :: Handlers HandlerM
documentOpenNotificationHandler = notificationHandler STextDocumentDidOpen $ \msg -> do
  let _uri = msg^.params.textDocument.uri
  documentDiagnostics _uri

documentCloseNotificationHandler :: Handlers HandlerM
documentCloseNotificationHandler = notificationHandler STextDocumentDidClose $ \msg -> do
  let _uri = msg^.params.textDocument.uri
  pure ()
  -- modifying ssCache (M.delete _uri)
  -- modifying ssLoaded (M.delete _uri)

documentSaveNotificationHandler :: Handlers HandlerM
documentSaveNotificationHandler = notificationHandler STextDocumentDidSave $ \msg -> do
  let _uri = msg^.params.textDocument.uri
  documentDiagnostics _uri

handleError :: (HandlerError -> HandlerM a) -> HandlerM a -> HandlerM a
handleError = flip catchError

hoverRequestHandler :: Handlers HandlerM
hoverRequestHandler = requestHandler STextDocumentHover $ \req resp -> handleError (const (pure ())) $ do
  let
    _uri = req^.params.textDocument.uri
    pos  = req^.params.position

  -- cache <- use ssCache
  -- case M.lookup _uri cache of
  --   Nothing -> throwError (NotCached _uri)
  --   Just _ -> pure ()
  pure ()

documentDiagnostics ::  Uri -> HandlerM ()
documentDiagnostics _uri = do
  let nuri = toNormalizedUri _uri

  mFile <- liftLsp $ getVirtualFile nuri
  aRes <- case mFile of
    Nothing ->
      throwError (NoVirtualFile _uri)

    Just (VFS.VirtualFile _ _ rope) -> do
      let src = encodeUtf8 (toText rope)
      liftIO $ runAnalyze (analyzeSource src)

  _diagnostics <- case aRes of
    (Left (AnalyzeError pe), _) -> pure (List [pactErrorToDiagnostic pe])
    (Right (AnalyzeResult aCom), AnalyzeState aState) -> do
      ssCache %= M.insert _uri (aCom, aState)

      pure (List []) -- no diagnostics
  let
    _version = Nothing
  liftLsp (sendNotification STextDocumentPublishDiagnostics PublishDiagnosticsParams{..})

pactErrorToDiagnostic :: P.PactErrorI -> Diagnostic
pactErrorToDiagnostic pe = Diagnostic{..}
  where
    (P.SpanInfo sl sc el ec) = view P.peInfo pe
    _range = Range
      (Position (fromIntegral sl) (fromIntegral sc))
      (Position (fromIntegral el) (fromIntegral ec))
    _severity = Just DsError
    _code = Nothing
    _source = case pe of
      P.PELexerError _ _ -> Just "Lexer"
      P.PEParseError _ _ -> Just "Parser"
      P.PEDesugarError _ _ -> Just "Desugar"
      P.PETypecheckError _ _ -> Just "Typechecker"
      P.PEOverloadError _ _ -> Just "Overloader"
      _ -> Nothing
    _message = P.renderPactError pe
    _tags = Nothing
    _relatedInformation = Nothing
