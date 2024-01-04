-- | 
{-# LANGUAGE DuplicateRecordFields #-}

module Pact.Core.LanguageServer
  ( startServer
  ) where

import Control.Lens hiding (Iso)

import Language.LSP.Server
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Lens (uri, textDocument, params, text)
import Language.LSP.Protocol.Types
import Language.LSP.VFS
import Language.LSP.Diagnostics

import Control.Monad.IO.Class
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.IORef
import Data.Default
import System.FilePath

import Pact.Core.Gas
import Pact.Core.Persistence.MockPersistence

import Pact.Core.Repl.Utils

import Pact.Core.Info
import Pact.Core.Repl.Compile
import Pact.Core.Environment
import Pact.Core.Builtin
import Pact.Core.Errors
import Pact.Core.Serialise
import Pact.Core.Pretty

import System.IO (stderr)
import qualified Data.Text.IO as T
import System.Exit

import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans (lift)
import Control.Concurrent.MVar
import qualified Data.Map.Strict as M

data LocationInfo
  = LocationInfo
  { _locNormalizedUri :: NormalizedUri
  , _locSpanInfo :: SpanInfo
  } deriving Eq

newtype LSState =
  LSState { _unLSState :: M.Map NormalizedUri (ReplState ReplCoreBuiltin) }

newLSState :: IO (MVar LSState)
newLSState = newMVar (LSState mempty)

type LSM = LspT () (ReaderT (MVar LSState) IO)

-- getState :: LSM LSState
-- getState = lift ask >>= liftIO . readMVar

-- setState :: LSState -> LSM ()
-- setState s = do
--   st <- lift ask
--   liftIO (putMVar st s)

modifyState :: (LSState -> LSState) -> LSM ()
modifyState f = do
  st <- lift ask
  liftIO (modifyMVar_ st $ \s -> pure $ f s)


startServer :: IO ()
startServer = do
  lsState <- newLSState
  runServer (server lsState) >>= \case
    0 -> exitSuccess
    rc -> exitWith (ExitFailure rc)
  where
    server state = ServerDefinition
      { defaultConfig = ()
      , configSection = "pact"
      , parseConfig = const $ const $ pure ()
      , onConfigChange = const $ pure ()
      , doInitialize = \env _ -> pure (Right env)
      , staticHandlers = const handlers
      , interpretHandler = \env -> Iso (\lsm -> runLSM lsm state env) liftIO
      , options = defaultOptions
      }

    runLSM lsm state cfg = runReaderT (runLspT cfg lsm) state

    handlers :: Handlers LSM
    handlers = mconcat
      [ initializedHandler
      , documentDidOpenHandler
      , documentDidChangeHandler
      , documentDidCloseHandler
      , documentDidSaveHandler
      -- request handler
--      , documentHoverRequestHandler
      ]

debug :: MonadIO m => Text -> m ()
debug msg = liftIO $ T.hPutStrLn stderr $ "[pact-lsp] " <> msg

initializedHandler :: Handlers LSM
initializedHandler = notificationHandler SMethod_Initialized $ \_ -> pure ()

documentDidOpenHandler :: Handlers LSM
documentDidOpenHandler = notificationHandler SMethod_TextDocumentDidOpen $ \msg -> do
  let nuri = msg ^. params . textDocument . uri . to toNormalizedUri
      content = msg ^. params . textDocument . text

  debug $ "open file" <> renderText nuri
  sendDiagnostics nuri 0 content

documentDidChangeHandler :: Handlers LSM
documentDidChangeHandler = notificationHandler SMethod_TextDocumentDidChange $ \msg -> do
  let nuri = msg ^. params . textDocument . uri . to toNormalizedUri

  mdoc <- getVirtualFile nuri
  case mdoc of
    Nothing -> debug $ "No virtual file found for: " <> renderText nuri
    Just vf -> sendDiagnostics nuri (virtualFileVersion vf) (virtualFileText vf)


documentDidCloseHandler :: Handlers LSM
documentDidCloseHandler = notificationHandler SMethod_TextDocumentDidClose $ \msg -> do
  let nuri = msg ^. params . textDocument . uri . to toNormalizedUri
  debug $ "Remove from cache: " <> renderText nuri
  modifyState (\(LSState old) -> LSState $ M.delete nuri old)

documentDidSaveHandler :: Handlers LSM
documentDidSaveHandler = notificationHandler SMethod_TextDocumentDidSave $ \msg -> do
  let nuri = msg ^. params . textDocument . uri . to toNormalizedUri
  debug $ "Document saved: " <> renderText nuri
  sendDiagnostics nuri 0 ""


sendDiagnostics :: NormalizedUri -> Int32 -> Text -> LSM ()
sendDiagnostics nuri v content = liftIO runPact >>= \case
  Left err -> do
    -- We only publish a single diagnostic
    publishDiagnostics 1  nuri (Just v) $ partitionBySource [pactErrorToDiagnostic err]
  Right (st, _r) -> do
    st' <- liftIO (readIORef st)
    modifyState (\(LSState old) -> LSState $ M.insert nuri st' old)

    -- We emit an empty set of diagnostics
    publishDiagnostics 0  nuri (Just v) $ partitionBySource []
  where
    runPact = do
      let file = fromMaybe "<local>" $ uriToFilePath (fromNormalizedUri nuri)
      pdb <- mockPactDb serialisePact_repl_spaninfo
      gasRef <- newIORef (Gas 0)
      gasLog <- newIORef Nothing
      let
        builtinMap = if isReplScript nuri
                     then replcoreBuiltinMap
                     else RBuiltinWrap <$> coreBuiltinMap
      
      ee <-defaultEvalEnv pdb builtinMap
      let
        src = SourceCode (takeFileName file) content
        rstate = ReplState
          { _replFlags = mempty
          , _replEvalState = def
          , _replPactDb = pdb
          , _replGas = gasRef
          , _replEvalLog = gasLog
          , _replCurrSource = src
          , _replEvalEnv = ee
          , _replTx = Nothing
          }
      stateRef <- newIORef rstate
      res <- runReplT stateRef (interpretReplProgram src (const (pure ())))
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

    spanInfoToRange :: SpanInfo -> Range
    spanInfoToRange (SpanInfo sl sc el ec) = mkRange
      (fromIntegral sl)  (fromIntegral sc)
      (fromIntegral el)  (fromIntegral ec)


    isReplScript :: NormalizedUri -> Bool
    isReplScript = maybe False ((==) ".repl" . takeExtension) . uriToFilePath . fromNormalizedUri

    pactErrorSource :: PactError i -> Text
    pactErrorSource = \case
      PELexerError{} -> "Lexer"
      PEParseError{} -> "Parse"
      PEDesugarError{} -> "Desugar"
      PEExecutionError{} -> "Execution"

-- documentHoverRequestHandler :: Handlers LSM
-- documentHoverRequestHandler = requestHandler SMethod_TextDocumentHover $ \req resp ->
--   getState >>= \(LSState cache) -> do
--     let nuri = req ^. params . textDocument . uri . to toNormalizedUri
-- --        pos = req ^. params . position

--     case M.lookup nuri cache of
--       Nothing -> do
--         let msg = "cache does not contain file: " <> renderText nuri
--             err = ResponseError (InR ErrorCodes_InternalError) msg Nothing
--         debug msg
--         resp (Left err)
--       Just st -> do
--         let _lo = view (replEvalState . esLoaded) st
--             mc = MarkupContent MarkupKind_PlainText ""
--             range = undefined
--             hover = Hover (InL mc) (Just range)
--         resp (Right (InL hover))
