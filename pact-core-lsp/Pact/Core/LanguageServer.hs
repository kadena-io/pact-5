-- | 
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Pact.Core.LanguageServer
  ( startServer
  ) where

import Control.Lens hiding (Iso)

import Language.LSP.Server
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Lens (uri, textDocument, params, text, position)
import Language.LSP.Protocol.Types
import Language.LSP.VFS
import Language.LSP.Diagnostics

import Control.Monad.IO.Class
import Data.Monoid (First(..), getFirst)
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
import Data.List (find)

import Pact.Core.IR.Eval.Runtime
import Pact.Core.IR.Eval.CEK(CEKEval)
import Pact.Core.Repl.Runtime.ReplBuiltin

import qualified Pact.Core.Syntax.ParseTree as Lisp
import qualified Pact.Core.Syntax.Lexer as Lisp
import qualified Pact.Core.Syntax.Parser as Lisp
import Pact.Core.IR.Desugar
import Pact.Core.IR.Term
import Control.Monad.Except
import Pact.Core.LanguageServer.Utils (termAt)

-- data LocationInfo
--   = LocationInfo
--   { _locNormalizedUri :: NormalizedUri
--   , _locSpanInfo :: SpanInfo
--   } deriving Eq

data LSState =
  LSState
  { _lsReplState :: M.Map NormalizedUri (ReplState ReplCoreBuiltin)
  , _lsLexed :: M.Map NormalizedUri [Lisp.ReplSpecialTL SpanInfo]
  }

makeLenses ''LSState

newLSState :: IO (MVar LSState)
newLSState = newMVar (LSState mempty mempty)

type LSM = LspT () (ReaderT (MVar LSState) IO)

getState :: LSM LSState
getState = lift ask >>= liftIO . readMVar

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
  modifyState ((lsReplState %~ M.delete nuri) . (lsLexed %~ M.delete nuri))

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
  Right (stRef, r) -> do
    st <- liftIO (readIORef stRef)
    modifyState ((lsReplState %~ M.insert nuri st) . (lsLexed %~ M.insert nuri r))

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
      
      ee <- defaultEvalEnv pdb builtinMap
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
      res <- runReplT stateRef (processFile src)
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

documentHoverRequestHandler :: Handlers LSM
documentHoverRequestHandler = requestHandler SMethod_TextDocumentHover $ \req resp ->
  getState >>= \st -> do
    let nuri = req ^. params . textDocument . uri . to toNormalizedUri
        pos = req ^. params . position

    case st ^. lsLexed . at nuri of
      Nothing -> do
        let msg = "cache does not contain file: " <> renderText nuri
            err = ResponseError (InR ErrorCodes_InternalError) msg Nothing
        debug msg
        resp (Left err)
      Just rtl -> case getFirst (mconcat $ First . termAt pos <$> rtl) of
        Nothing -> undefined
        Just p
          | isNative p -> do
              let
                mc = MarkupContent MarkupKind_PlainText ""
                range = undefined
                hover = Hover (InL mc) (Just range)
              resp (Right (InL hover))
  where
  isNative :: Lisp.ReplSpecialTL SpanInfo -> Bool
  isNative (Lisp.RTL (Lisp.RTLTopLevel t)) = topLevelHasDocs t 

processFile
  :: SourceCode
  -> ReplM ReplCoreBuiltin [Lisp.ReplSpecialTL SpanInfo]
processFile (SourceCode _ source) = do
  lexx <- liftEither (Lisp.lexer source)
  liftEither $ Lisp.parseReplProgram lexx
  -- concat <$> traverse pipe parsed
  -- where
  -- pipe = \case
  --   Lisp.RTL rtl ->
  --     pure <$> pipe' rtl
  --   Lisp.RTLReplSpecial rsf -> case rsf of
  --     Lisp.ReplLoad txt resetState _
  --       | resetState -> do
  --         oldSrc <- use replCurrSource
  --         evalState .= def
  --         pactdb <- liftIO (mockPactDb serialisePact_repl_spaninfo)
  --         replPactDb .= pactdb
  --         ee <- liftIO (defaultEvalEnv pactdb replcoreBuiltinMap)
  --         replEvalEnv .= ee
  --         out <- loadFile (T.unpack txt) replEnv display
  --         replCurrSource .= oldSrc
  --         pure out
  --       | otherwise -> do
  --         oldSrc <- use replCurrSource
  --         oldEs <- use evalState
  --         oldEE <- use replEvalEnv
  --         out <- loadFile (T.unpack txt) replEnv display
  --         replEvalEnv .= oldEE
  --         evalState .= oldEs
  --         replCurrSource .= oldSrc
  --         pure out
  -- pipe' tl = case tl of
  --   Lisp.RTLTopLevel toplevel -> do
  --     v <- interpretTopLevel replEnv toplevel
  --     displayValue (RCompileValue v)
  --   _ ->  do
  --     ds <- runDesugarReplTopLevel tl
  --     interpret ds
  -- interpret (DesugarOutput tl _deps) = do
  --   case tl of
  --     RTLDefun df -> do
  --       let fqn = FullyQualifiedName replModuleName (_dfunName df) replModuleHash
  --       loaded . loAllLoaded %= M.insert fqn (Dfun df)
  --       displayValue $ RLoadedDefun $ _dfunName df
  --     RTLDefConst dc -> do
  --       let fqn = FullyQualifiedName replModuleName (_dcName dc) replModuleHash
  --       loaded . loAllLoaded %= M.insert fqn (DConst dc)
  --       displayValue $ RLoadedDefConst $ _dcName dc
