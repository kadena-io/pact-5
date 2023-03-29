{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

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
import Pact.Core.Builtin (ReplRawBuiltin, BuiltinForm (..))
import qualified Data.Map.Strict as M
import Control.Monad.Except (MonadError(..))
import Pact.Core.Typed.Term (OverloadedReplTopLevel, ReplTopLevel (..),
                             Term (..), DefConst (..), Defun (..), Module (..), Def (..), termInfo)
import Pact.Core.LSP.AnalyzeSource (analyzeSource, AnalyzeError (..), runAnalyze,
                                    AnalyzeState (..), AnalyzeResult (..))
import Control.Lens.Operators
import Pact.Core.Names (NamedDeBruijn)
import Control.Applicative ((<|>))
import Data.Monoid (Alt (..))
import qualified Data.Text as T

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
hoverRequestHandler = requestHandler STextDocumentHover $ \req resp ->
  handleError (const (pure ())) $ do
  let
    _uri = req^.params.textDocument.uri
    pos  = req^.params.position

  cache <- use ssCache
  term <- case M.lookup _uri cache of
    Nothing -> throwError (NotCached _uri)
    Just (m, _) -> pure $ getAlt (foldMap (Alt . termAt pos) m)
  let _contents = HoverContents (MarkupContent MkPlainText (T.pack (show (term,pos))))
      _range = Nothing
  resp (Right (Just Hover{..}))

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


termAt
  :: Position
  -> OverloadedReplTopLevel NamedDeBruijn ReplRawBuiltin P.SpanInfo
  -> Maybe (OverloadedReplTopLevel NamedDeBruijn ReplRawBuiltin P.SpanInfo)
termAt p = \case
  RTLModule m -> goModule m
  RTLInterface _inf ->  Nothing
  RTLDefun df -> goDefun df
  RTLDefConst dc -> goDefConst dc
  RTLTerm term -> RTLTerm <$> goTerm term
  where
    goTerm term = if p `inside` view termInfo term
                  then case term of
                         t@(Lam _ b _) -> goTerm b <|> Just t
                         t@(App tm1 tm2 _) ->
                           goTerm tm1 <|> getAlt (foldMap (Alt . goTerm) tm2) <|> Just t
                         t@(Let _ tm1 tm2 _) -> goTerm tm1 <|> goTerm tm2 <|> Just t
                         t@(TyApp tm _ _) -> goTerm tm <|> Just t
                         t@(TyAbs _ tm _) -> goTerm tm <|> Just t
                         t@(Sequence tm1 tm2 _) -> goTerm tm1 <|> goTerm tm2 <|> Just t
                         t@(Conditional op _) ->
                           (case op of
                               CAnd a b  -> goTerm a <|> goTerm b
                               COr a b   -> goTerm a <|> goTerm b
                               CIf a b c -> goTerm a <|> goTerm b <|> goTerm c) <|> Just t
                         t@(ListLit _ tms _) -> getAlt (foldMap (Alt . goTerm) tms) <|> Just t
                         t@(DynInvoke tm _ _) -> goTerm tm <|> Just t
                         t@(Try tm1 tm2 _) -> goTerm tm1 <|> goTerm tm2 <|> Just t
                         t -> Just t
                  else Nothing
    goDefun = \case
      t@(Defun _ _ tm i)
        | p `inside` i -> (RTLTerm <$> goTerm tm) <|> Just (RTLDefun t)
      _otherwise -> Nothing
    goDefConst = \case
      t@(DefConst _ _ tm i)
        | p `inside` i -> (RTLTerm <$> goTerm tm) <|> Just (RTLDefConst t)
      _otherwise -> Nothing
    goModule (Module _ defs _ _ _ _) = let
      goDef = \case
        Dfun t -> goDefun t
        DConst t -> goDefConst t
      in getAlt (foldMap (Alt . goDef) defs)

-- | Check if a `Position` is contained within a `Span`
inside :: Position -> P.SpanInfo -> Bool
inside pos (P.SpanInfo sl sc el ec) = sPos <= pos && pos < ePos
  where
    sPos = Position (fromIntegral sl) (fromIntegral sc)
    ePos = Position (fromIntegral el) (fromIntegral ec)
