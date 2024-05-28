-- |
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Pact.Core.LanguageServer
  ( startLSP
  ) where

import Control.Lens hiding (Iso)
import Control.Monad.Except
import Language.LSP.Server
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Lens (uri, textDocument, params, text, position, newName)
import Language.LSP.Protocol.Types
import Language.LSP.VFS
import Language.LSP.Diagnostics
import Data.Monoid (Alt(..))
import Control.Monad.IO.Class
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.IORef
import Data.Default
import System.FilePath

import Pact.Core.Persistence.MockPersistence

import Pact.Core.Repl.Utils
import Pact.Core.Info
import Pact.Core.Compile
import Pact.Core.Environment
import Pact.Core.Builtin
import Pact.Core.Errors
import Pact.Core.Serialise
import Pact.Core.Pretty (renderText)
import System.IO (stderr)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import System.Exit

import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans (lift)
import Control.Concurrent.MVar
import qualified Data.Map.Strict as M
import Pact.Core.IR.Eval.Runtime.Types
import qualified Pact.Core.Syntax.ParseTree as Lisp
import qualified Pact.Core.Syntax.Lexer as Lisp
import qualified Pact.Core.Syntax.Parser as Lisp
import Pact.Core.IR.Term
import Pact.Core.LanguageServer.Utils
import Pact.Core.LanguageServer.Renaming
import Pact.Core.Repl.Runtime.ReplBuiltin
import Pact.Core.Repl.BuiltinDocs
import Pact.Core.Repl.UserDocs
import Pact.Core.Names
import qualified Pact.Core.IR.ModuleHashing as MHash
import qualified Pact.Core.IR.ConstEval as ConstEval

data LSState =
  LSState
  { _lsReplState :: M.Map NormalizedUri (ReplState ReplCoreBuiltin)
  -- ^ Post-Compilation State for each opened file
  , _lsTopLevel :: M.Map NormalizedUri [EvalTopLevel ReplCoreBuiltin SpanInfo]
  -- ^ Top-level terms for each opened file. Used to find the match of a
  --   particular (cursor) position inside the file.
  }

makeLenses ''LSState

newLSState :: IO (MVar LSState)
newLSState = newMVar (LSState mempty mempty)

type LSM = LspT () (ReaderT (MVar LSState) IO)

getState :: LSM LSState
getState = lift ask >>= liftIO . readMVar

modifyState :: (LSState -> LSState) -> LSM ()
modifyState f = do
  st <- lift ask
  liftIO (modifyMVar_ st $ \s -> pure $ f s)

startLSP :: IO ()
startLSP = do
  lsState <- newLSState
  runServer (server lsState) >>= \case
    0 -> exitSuccess
    rc -> exitWith (ExitFailure rc)
  where
    server state = ServerDefinition
      { defaultConfig = ()
      , configSection = "pact"
      -- In general, we can configure the pact LSP server via
      -- a server specific section. This is currently unused.
      , parseConfig = const $ const $ pure ()
      , onConfigChange = const $ pure ()
      , doInitialize = \env _ -> pure (Right env)
      , staticHandlers = const handlers
      , interpretHandler = \env -> Iso (\lsm -> runLSM lsm state env) liftIO
      , options = defaultOptions { optTextDocumentSync = Just syncOptions }
      }

    -- Note: Syncing options are related to problems highlighted in #83.
    -- Some IDEs differ in what kind of notification they send, also the
    -- `TextDocumentSyncOptions` control the frequency (incremental, full)
    -- the server is being informed.
    syncOptions = TextDocumentSyncOptions
                  (Just True) -- send open/close notification
                  (Just TextDocumentSyncKind_Incremental)
                  -- Documents are synce by sending the full content once
                  -- a file is being opened. Later updates are send
                  -- incrementally to the server.
                  (Just False) -- dont send `willSave` notification.
                  (Just False) -- dont send `willSaveWaitUntil` request.
                  (Just $ InR $ SaveOptions $ Just True) -- Include content on save.

    runLSM lsm state cfg = runReaderT (runLspT cfg lsm) state

    handlers :: Handlers LSM
    handlers = mconcat
      [ initializedHandler
      , documentDidOpenHandler
      , documentDidChangeHandler
      , documentDidCloseHandler
      , documentDidSaveHandler
      , workspaceDidChangeConfigurationHandler
      -- request handler
      , documentHoverRequestHandler
      , documentDefinitionRequestHandler
      , documentRenameRequestHandler
      ]

debug :: MonadIO m => Text -> m ()
debug msg = liftIO $ T.hPutStrLn stderr $ "[pact-lsp] " <> msg

-- Handler executed after the LSP client initiates a connection to our server.
initializedHandler :: Handlers LSM
initializedHandler = notificationHandler SMethod_Initialized $ \_ -> pure ()

workspaceDidChangeConfigurationHandler :: Handlers LSM
workspaceDidChangeConfigurationHandler
  = notificationHandler SMethod_WorkspaceDidChangeConfiguration $ \_ -> pure ()

documentDidOpenHandler :: Handlers LSM
documentDidOpenHandler = notificationHandler SMethod_TextDocumentDidOpen $ \msg -> do
  let nuri = msg ^. params . textDocument . uri . to toNormalizedUri
      content = msg ^. params . textDocument . text

  debug $ "open file " <> renderText nuri
  sendDiagnostics nuri (Just 0) content

documentDidChangeHandler :: Handlers LSM
documentDidChangeHandler = notificationHandler SMethod_TextDocumentDidChange $ \msg -> do
  let nuri = msg ^. params . textDocument . uri . to toNormalizedUri

  debug $ "didChangeHandler notification: " <> renderText nuri

  mdoc <- getVirtualFile nuri
  case mdoc of
    Nothing -> debug $ "No virtual file found for: " <> renderText nuri
    Just vf -> sendDiagnostics nuri (Just $ virtualFileVersion vf) (virtualFileText vf)


documentDidCloseHandler :: Handlers LSM
documentDidCloseHandler = notificationHandler SMethod_TextDocumentDidClose $ \msg -> do
  let nuri = msg ^. params . textDocument . uri . to toNormalizedUri

  debug $ "didCloseHandler notification: " <> renderText nuri
  modifyState ((lsReplState %~ M.delete nuri) . (lsTopLevel %~ M.delete nuri))

documentDidSaveHandler :: Handlers LSM
documentDidSaveHandler = notificationHandler SMethod_TextDocumentDidSave $ \msg -> do
  let nuri = msg ^. params . textDocument . uri . to toNormalizedUri
      content =  msg ^. params . text
  debug $ "didSaveHandler notification: " <> renderText nuri

  case content of
    Nothing -> do
      debug "didSaveHandler: read content from VFS"
      mdoc <- getVirtualFile nuri
      case mdoc of
        Nothing -> debug $ "No virtual file found for: " <> renderText nuri
        Just vf -> sendDiagnostics nuri (Just $ virtualFileVersion vf) (virtualFileText vf)
    Just t -> do
      debug "didSaveHandler: content from request"
      sendDiagnostics nuri Nothing t

-- Working horse for producing document diagnostics.
sendDiagnostics :: NormalizedUri -> Maybe Int32 -> Text -> LSM ()
sendDiagnostics nuri mv content = liftIO runPact >>= \case
  Left err -> do
    -- We only publish a single diagnostic
    publishDiagnostics 1  nuri mv $ partitionBySource [pactErrorToDiagnostic err]
  Right (stRef, r) -> do
    st <- liftIO (readIORef stRef)
    modifyState ((lsReplState %~ M.insert nuri st) . (lsTopLevel %~ M.insert nuri r))

    -- We emit an empty set of diagnostics
    publishDiagnostics 0  nuri mv $ partitionBySource []
  where
    runPact = do
      let file = fromMaybe "<local>" $ uriToFilePath (fromNormalizedUri nuri)
      pdb <- mockPactDb serialisePact_repl_spaninfo
      gasLog <- newIORef Nothing
      let
        builtinMap = if isReplScript nuri
                     then replCoreBuiltinMap
                     else RBuiltinWrap <$> coreBuiltinMap

      ee <- defaultEvalEnv pdb builtinMap
      let
        src = SourceCode (takeFileName file) content
        rstate = ReplState
          { _replFlags = mempty
          , _replEvalState = def
          , _replPactDb = pdb
          , _replEvalLog = gasLog
          , _replCurrSource = src
          , _replEvalEnv = ee
          , _replTx = Nothing
          , _replUserDocs = mempty
          , _replTLDefPos = mempty
          -- Note: for the lsp, we don't want it to fail on repl natives not enabled,
          -- since there may be no way for us to set it for the LSP from pact directly.
          -- Once this is possible, we can set it to `False` as is the default
          , _replNativesEnabled = True
          }
      stateRef <- newIORef rstate
      res <- runReplT stateRef (processFile (replBuiltinEnv @CEKSmallStep) src)
      pure ((stateRef,) <$> res)

    pactErrorToDiagnostic :: PactError SpanInfo -> Diagnostic
    pactErrorToDiagnostic err = Diagnostic
      { _range = err ^. peInfo .to spanInfoToRange
      , _severity = Just DiagnosticSeverity_Error
      , _code = Nothing -- We do not have any error code right now
      , _codeDescription = Nothing
      , _source = Just (pactErrorSource err)
      , _message = renderText err
      , _tags = Nothing
      , _relatedInformation = Nothing
      , _data_ = Nothing
      }

    isReplScript :: NormalizedUri -> Bool
    isReplScript = maybe False ((==) ".repl" . takeExtension) . uriToFilePath . fromNormalizedUri

    pactErrorSource :: PactError i -> Text
    pactErrorSource = \case
      PELexerError{} -> "Lexer"
      PEParseError{} -> "Parse"
      PEDesugarError{} -> "Desugar"
      PEExecutionError{} -> "Execution"

spanInfoToRange :: SpanInfo -> Range
spanInfoToRange (SpanInfo sl sc el ec) = mkRange
  (fromIntegral sl)  (fromIntegral sc)
  (fromIntegral el)  (fromIntegral ec)


getMatch
  :: Position
  -> [EvalTopLevel ReplCoreBuiltin SpanInfo]
  -> Maybe (PositionMatch ReplCoreBuiltin SpanInfo)
getMatch pos tl = getAlt (foldMap (Alt . topLevelTermAt pos) tl)

documentDefinitionRequestHandler :: Handlers LSM
documentDefinitionRequestHandler = requestHandler SMethod_TextDocumentDefinition $ \req resp ->
  getState >>= \st -> do
    let uri' = req ^. params . textDocument . uri
        nuri = toNormalizedUri uri'
        pos = req ^. params . position

    tlDefSpan <- case getMatch pos =<< view (lsTopLevel . at nuri) st of
      Just tlm -> case tlm of
        TermMatch (Var (Name n (NTopLevel mn _)) _) -> do
          let qn = QualifiedName n mn
          pure $ preview (lsReplState . at nuri . _Just . replTLDefPos . ix qn) st
        o -> do
          debug $ "defi match " <> renderText (show o)
          pure Nothing
      _ -> pure Nothing
    debug $ "documentDefinition request: " <> renderText nuri
    let loc = Location uri' . spanInfoToRange
    case loc <$> tlDefSpan of
      Just x -> resp (Right $ InL $ Definition (InL x))
      Nothing -> resp (Right $ InR $ InR Null)


documentHoverRequestHandler :: Handlers LSM
documentHoverRequestHandler = requestHandler SMethod_TextDocumentHover $ \req resp ->
  getState >>= \st -> do
    let nuri = req ^. params . textDocument . uri . to toNormalizedUri
        pos = req ^. params . position

    case getMatch pos =<< view (lsTopLevel . at nuri) st of
      Just tlm -> case tlm of
        TermMatch (Builtin builtin i) -> let
                docs = fromMaybe "No docs available"
                  (M.lookup (replCoreBuiltinToUserText builtin) builtinDocs)

                mc = MarkupContent MarkupKind_PlainText docs
                range = spanInfoToRange i
                hover = Hover (InL mc) (Just range)
                in resp (Right (InL hover))

        TermMatch (Var (Name n (NTopLevel mn _)) _) ->
          -- Access user-annotated documentation using the @doc command.
          let qn = QualifiedName n mn
              toHover d = Hover (InL $ MarkupContent MarkupKind_PlainText d) Nothing
              doc = preview (lsReplState . at nuri . _Just . replUserDocs . ix qn) st
          in resp (Right (maybeToNull (toHover <$> doc)))
        _ ->  resp (Right (InR Null))
      Nothing -> do
        debug "documentHover: could not find term on position"
        resp (Right (InR Null))

documentRenameRequestHandler :: Handlers LSM
documentRenameRequestHandler = requestHandler SMethod_TextDocumentRename $ \req resp ->
  getState >>= \st -> do
    let
        uri' = req ^. params . textDocument . uri
        nuri = req ^. params . textDocument . uri . to toNormalizedUri
        pos = req ^. params . position
        nName = req ^. params . newName
        tls = fromMaybe [] $ view (lsTopLevel . at nuri) st
        toTextEdit r = TextEdit r nName

    case getRenameSpanInfo tls <$> getMatch pos tls of
      Nothing -> do
        debug "documentRenameRequestHandler: could not find term at position"
        resp (Right (InR Null))
      Just changePos -> do
        debug $ "documentRenameRequestHandler: got " <> sshow (length changePos) <> " changes"
        let changes = InL . toTextEdit . spanInfoToRange <$> changePos
            te = TextDocumentEdit
                 (OptionalVersionedTextDocumentIdentifier uri' $ InR Null)
                 changes
            we = WorkspaceEdit Nothing (Just [InL te]) Nothing
        resp (Right (InL we))

processFile
  :: BuiltinEnv CEKSmallStep ReplCoreBuiltin SpanInfo (ReplM ReplCoreBuiltin)
  -> SourceCode
  -> ReplM ReplCoreBuiltin [EvalTopLevel ReplCoreBuiltin SpanInfo]
processFile replEnv (SourceCode _ source) = do
  lexx <- liftEither (Lisp.lexer source)
  parsed <- liftEither $ Lisp.parseReplProgram lexx
  concat <$> traverse pipe parsed
  where
  pipe = \case
    Lisp.RTL (Lisp.RTLTopLevel tl) -> do
      functionDocs tl
      (ds, deps) <- compileDesugarOnly replEnv tl
      constEvaled <- ConstEval.evalTLConsts replEnv ds
      let tlFinal = MHash.hashTopLevel constEvaled
      let act = [ds] <$ evalTopLevel replEnv tlFinal deps
      catchError act (const (pure []))
    _ -> pure []

sshow :: Show a => a -> Text
sshow = T.pack . show
