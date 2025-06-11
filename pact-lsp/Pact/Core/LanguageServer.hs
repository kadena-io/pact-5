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
import System.FilePath

import Pact.Core.Persistence.MockPersistence

import Pact.Core.Repl.Utils
import Pact.Core.Info
import Pact.Core.Compile
import Pact.Core.Environment
import Pact.Core.Builtin
import Pact.Core.Errors
import Pact.Core.Serialise
import Pact.Core.Coverage.Types(LcovReport(..))
import Pact.Core.Pretty (renderText)
import System.IO (stderr)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import System.Exit

import Control.Monad
import Control.Monad.State.Strict(put)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans (lift)
import Control.Concurrent.MVar
import qualified Data.Map.Strict as M
import qualified Control.Exception as E
import qualified Pact.Core.Syntax.ParseTree as Lisp
import qualified Pact.Core.Syntax.Lexer as Lisp
import qualified Pact.Core.Syntax.Parser as Lisp
import Pact.Core.IR.Term
import Pact.Core.Persistence
import Pact.Core.LanguageServer.Utils
import Pact.Core.LanguageServer.Renaming
import Pact.Core.Repl.BuiltinDocs
import Pact.Core.Repl.BuiltinDocs.Internal
import Pact.Core.Repl.UserDocs
import Pact.Core.Names
import qualified Pact.Core.IR.ModuleHashing as MHash
import qualified Pact.Core.IR.ConstEval as ConstEval
import qualified Pact.Core.Repl.Compile as Repl
import Pact.Core.Interpreter
import Data.Default

data LSState =
  LSState
  { _lsReplState :: M.Map NormalizedUri (ReplState ReplCoreBuiltin)
  -- ^ Post-Compilation State for each opened file
  , _lsTopLevel :: M.Map NormalizedUri [EvalTopLevel ReplCoreBuiltin FileLocSpanInfo]
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
sendDiagnostics nuri mv content = liftIO (setupAndProcessFile nuri content) >>= \case
  Left err -> do
    -- We only publish a single diagnostic
    publishDiagnostics 1  nuri mv $ partitionBySource [pactErrorToDiagnostic err]
  Right (st, tl) -> do
    modifyState ((lsReplState %~ M.insert nuri st) . (lsTopLevel %~ M.unionWith (<>) tl))

    -- We emit an empty set of diagnostics
    publishDiagnostics 0  nuri mv $ partitionBySource []
  where
    pactErrorToDiagnostic :: PactError FileLocSpanInfo -> Diagnostic
    pactErrorToDiagnostic err = Diagnostic
      { _range = err ^. peInfo . spanInfo . to spanInfoToRange
      , _severity = Just DiagnosticSeverity_Error
      , _code = Nothing -- We do not have any error code right now
      , _codeDescription = Nothing
      , _source = Just (pactErrorSource err)
      , _message = renderText err
      , _tags = Nothing
      , _relatedInformation = Nothing
      , _data_ = Nothing
      }

    pactErrorSource :: PactError i -> Text
    pactErrorSource = \case
      PELexerError{} -> "Lexer"
      PEParseError{} -> "Parse"
      PEDesugarError{} -> "Desugar"
      PEExecutionError{} -> "Execution"
      PEUserRecoverableError{} -> "Execution"
      PEVerifierError{} -> "Verifier"

setupAndProcessFile
  :: NormalizedUri
  -> Text
  -> IO (Either (PactError FileLocSpanInfo)
          (ReplState ReplCoreBuiltin
          ,M.Map NormalizedUri  [EvalTopLevel ReplCoreBuiltin FileLocSpanInfo]))
setupAndProcessFile nuri content = do
  pdb <- mockPactDb serialisePact_repl_fileLocSpanInfo
  let
    builtinMap = if isReplScript fp
                 then replBuiltinMap
                 else RBuiltinWrap <$> coreBuiltinMap

  ee <- defaultEvalEnv pdb builtinMap
  let
      src = SourceCode (takeFileName fp) content
      rstate = ReplState
          { _replFlags = mempty
          , _replCurrSource = src
          , _replEvalEnv = ee
          , _replTx = Nothing
          , _replUserDocs = mempty
          , _replTLDefPos = mempty
          -- Note: for the lsp, we don't want it to fail on repl natives not enabled,
          -- since there may be no way for us to set it for the LSP from pact directly.
          -- Once this is possible, we can set it to `False` as is the default
          , _replNativesEnabled = True
          , _replLoad = doLoad
          , _replLogType = ReplStdOut
          , _replLoadedFiles = mempty
          , _replTraceLine = const $ const $ pure ()
          , _replPrintLine = const $ const $ pure ()
          , _replTestResults = []
          , _replCoverage = ReplCoverage False (LcovReport mempty)
          }
  stateRef <- newIORef rstate
  res <- evalReplM stateRef (processFile Repl.interpretEvalBigStep nuri src)
  st <- readIORef stateRef
  pure $ (st,) <$> res
  where
    fp = fromMaybe "<local>" $ uriToFilePath (fromNormalizedUri nuri)
    isReplScript :: FilePath -> Bool
    isReplScript = (==) ".repl" . takeExtension

spanInfoToRange :: SpanInfo -> Range
spanInfoToRange (SpanInfo sl sc el ec) = mkRange
  (fromIntegral sl)  (fromIntegral sc)
  (fromIntegral el)  (fromIntegral ec)


getMatch
  :: HasSpanInfo i
  => Position
  -> [EvalTopLevel ReplCoreBuiltin i]
  -> Maybe (PositionMatch ReplCoreBuiltin i)
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
    let loc = Location uri' . spanInfoToRange . view spanInfo
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
                docs = fromMaybe (MarkdownDoc "*No docs available*")
                  (M.lookup (replCoreBuiltinToUserText builtin) builtinDocs)

                mc = MarkupContent MarkupKind_Markdown (_markdownDoc docs)
                range = spanInfoToRange (view spanInfo i)
                hover = Hover (InL mc) (Just range)
                in resp (Right (InL hover))

        TermMatch (Var (Name n (NTopLevel mn _)) _) ->
          -- Access user-annotated documentation using the @doc command.
          let qn = QualifiedName n mn
              toHover d = Hover (InL $ MarkupContent MarkupKind_Markdown (_markdownDoc d)) Nothing
              doc = MarkdownDoc <$> preview (lsReplState . at nuri . _Just . replUserDocs . ix qn) st
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

doLoad :: FilePath -> Bool -> EvalM ReplRuntime ReplCoreBuiltin FileLocSpanInfo ()
doLoad fp reset = do
  oldSrc <- useReplState replCurrSource
  fp' <- mangleFilePath fp
  res <- liftIO $ E.try (T.readFile fp')
  pactdb <- liftIO (mockPactDb serialisePact_repl_fileLocSpanInfo)
  oldEE <- useReplState replEvalEnv
  when reset $ do
    ee <- liftIO (defaultEvalEnv pactdb replBuiltinMap)
    put def
    replEvalEnv .== ee
  when (Repl.isPactFile fp) $ esLoaded . loToplevel .= mempty
  _ <- case res of
    Left (_e:: E.IOException) ->
      throwExecutionError def $ EvalError $ "File not found: " <> T.pack fp
    Right txt -> do
      let source = SourceCode fp txt
      replCurrSource .== source
      let nfp = normalizedFilePathToUri (toNormalizedFilePath fp')
      processFile Repl.interpretEvalBigStep nfp source
  replCurrSource .== oldSrc
  unless reset $ do
    replEvalEnv .== oldEE
  pure ()


mangleFilePath :: FilePath -> EvalM ReplRuntime b FileLocSpanInfo FilePath
mangleFilePath fp = do
  (SourceCode currFile _) <- useReplState replCurrSource
  case currFile of
    "<local>" -> pure fp
    _ | isAbsolute fp -> pure fp
      | takeFileName currFile == currFile -> pure fp
      | otherwise -> pure $ combine (takeDirectory currFile) fp

processFile
  :: Interpreter ReplRuntime ReplCoreBuiltin FileLocSpanInfo
  -> NormalizedUri
  -> SourceCode
  -> ReplM ReplCoreBuiltin (M.Map NormalizedUri [EvalTopLevel ReplCoreBuiltin FileLocSpanInfo])
processFile replEnv nuri (SourceCode f source) = do
  lexx <- liftEither $ over _Left (fmap toFileLoc) (Lisp.lexer source)
  parsed <- liftEither $ bimap (fmap toFileLoc) ((fmap.fmap) toFileLoc) $ Lisp.parseReplProgram lexx
  M.unionsWith (<>) <$> traverse pipe parsed
  where
    toFileLoc = FileLocSpanInfo f
    pipe (Lisp.RTLTopLevel tl) = do
      functionDocs tl
      (ds, deps) <- compileDesugarOnly replEnv tl
      constEvaled <- ConstEval.evalTLConsts replEnv ds
      tlFinal <- MHash.hashTopLevel constEvaled
      let act = M.singleton nuri [ds] <$ evalTopLevel replEnv (RawCode mempty) tlFinal deps
      catchError act (const (pure mempty))
    pipe _ = pure mempty


sshow :: Show a => a -> Text
sshow = T.pack . show
