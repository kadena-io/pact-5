{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveTraversable #-}

module Pact.Core.Command.Server
  ( API
  , PollRequest(..)
  , PollResponse(..)
  , ListenRequest(..)
  , ListenResponse(..)
  , SendRequest(..)
  , SendResponse(..)
  , LocalRequest(..)
  , LocalResponse(..)
  , Log
  , ServerRuntime(..)
  , runServer
  , server ) where

import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Default
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import Data.IORef
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.LruCache as LRU
import Data.LruCache.IO
import qualified Data.LruCache.IO as LRU
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Version
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Network.Wai.Logger
import System.Log.FastLogger.Date
import Pact.Core.Builtin
import Pact.Core.ChainData
import Pact.Core.Command.Client
import Pact.Core.Command.RPC
import Pact.Core.Command.Server.Config
import Pact.Core.Command.Server.Servant
import Pact.Core.Command.Types
import Pact.Core.Hash
import Pact.Core.Compile
import Pact.Core.DefPacts.Types
import Pact.Core.Errors
import Pact.Core.Evaluate
import Pact.Core.Gas
import Pact.Core.Namespace
import Pact.Core.Persistence.SQLite
import Pact.Core.Persistence.Types
import Pact.Core.SPV
import Pact.Core.Serialise
import Pact.Core.StableEncoding
import qualified Pact.Core.Version as PI
import qualified Pact.JSON.Decode as JD
import qualified Pact.JSON.Encode as JE
import qualified Pact.JSON.Legacy.Utils as JL
import Servant.API
import Servant.Server
import System.Directory
import Control.Exception.Safe hiding (Handler)


-- | Temporarily pretend our Log type in CommandResult is unit.
type Log = ()

-- | Runtime environment for a Pact server.
data ServerRuntime
  = ServerRuntime
  { _srDbEnv :: PactDb CoreBuiltin Info
  , _srRequestCache :: LruHandle RequestKey (CommandResult Hash (PactErrorCompat Info))
  , _srSPVSupport :: SPVSupport
  }

newtype PollRequest
  = PollRequest (NE.NonEmpty RequestKey)

instance JD.FromJSON PollRequest where
  parseJSON = JD.withObject "Poll" $ \o -> PollRequest <$> o JD..: "requestKeys"

instance JE.Encode PollRequest where
  build (PollRequest rks) = JE.object [ "requestKeys" JE..= JE.Array rks ]

newtype PollResponse
  = PollResponse (HM.HashMap RequestKey (CommandResult Hash (PactErrorCompat Info)))

instance JE.Encode PollResponse where
  build (PollResponse pr) = JE.build $ JL.legacyHashMap requestKeyToB64Text (commandToStableEncoding <$> pr)

instance JD.FromJSON PollResponse where
  parseJSON v = do
    o <- JD.parseJSON v
    pure $ PollResponse $ commandFromStableEncoding <$> o

newtype ListenRequest
  = ListenRequest RequestKey

instance JE.Encode ListenRequest where
  build (ListenRequest rk) = JE.object [ "listen" JE..= rk ]

instance JD.FromJSON ListenRequest where
  parseJSON = JD.withObject "ListenRequest" $ \o ->
    ListenRequest <$> o JD..: "listen"

newtype ListenResponse
  = ListenResponse (CommandResult Hash (PactErrorCompat Info))

instance JD.FromJSON ListenResponse where
  parseJSON v = ListenResponse . commandFromStableEncoding <$> JD.parseJSON v

commandToStableEncoding
  :: CommandResult Hash (PactErrorCompat Info)
  -> CommandResult Hash (PactErrorCompat (StableEncoding Info))
commandToStableEncoding m = CommandResult
      { _crReqKey = _crReqKey m
      , _crTxId = _crTxId m
      , _crResult = (fmap.fmap) StableEncoding (_crResult m)
      , _crGas = _crGas m
      , _crLogs = _crLogs m
      , _crContinuation = _crContinuation m
      , _crMetaData = _crMetaData m
      , _crEvents = _crEvents m
      }

commandFromStableEncoding
  :: CommandResult Hash (PactErrorCompat (StableEncoding Info))
  -> CommandResult Hash (PactErrorCompat Info)
commandFromStableEncoding m = CommandResult
      { _crReqKey = _crReqKey m
      , _crTxId = _crTxId m
      , _crResult = (fmap.fmap) _stableEncoding (_crResult m)
      , _crGas = _crGas m
      , _crLogs = _crLogs m
      , _crContinuation = _crContinuation m
      , _crMetaData = _crMetaData m
      , _crEvents = _crEvents m
      }

instance JE.Encode ListenResponse where
  build (ListenResponse m) = JE.build $ commandToStableEncoding m

newtype LocalRequest
  = LocalRequest { _localRequest :: Command Text }

instance JE.Encode LocalRequest where
  build (LocalRequest cmd) = JE.build cmd

newtype LocalResponse
  = LocalResponse { _localResponse :: CommandResult Hash (PactErrorCompat Info) }

instance JD.FromJSON LocalResponse where
  parseJSON v = LocalResponse . commandFromStableEncoding <$> JD.parseJSON v

instance JD.FromJSON LocalRequest where
  parseJSON v = LocalRequest <$> JD.parseJSON v

instance JE.Encode LocalResponse where
  build (LocalResponse cmdr) = JE.build $ commandToStableEncoding cmdr

newtype SendRequest
  = SendRequest { _sendRequest :: SubmitBatch }

instance JE.Encode SendRequest where
  build (SendRequest sr) = JE.build sr

instance JD.FromJSON SendRequest where
  parseJSON v = SendRequest <$> JD.parseJSON v

newtype SendResponse
  = SendResponse RequestKeys
  deriving (Eq, Show)

instance JE.Encode SendResponse where
  build (SendResponse sr) = JE.build sr

instance JD.FromJSON SendResponse where
  parseJSON v = SendResponse <$> JD.parseJSON v

type API = ("api" :> "v1" :>
           (("send" :> ReqBody '[PactJson] SendRequest :> Post '[PactJson] SendResponse)
       :<|> ("poll" :> ReqBody '[PactJson] PollRequest :> Post '[PactJson] PollResponse)
       :<|> ("listen" :> ReqBody '[PactJson] ListenRequest :> Post '[PactJson] ListenResponse)
       :<|> ("local" :> ReqBody '[PactJson] LocalRequest :> Post '[PactJson] LocalResponse)))
           :<|> "version" :> Get '[PlainText] Text

runServer :: Config -> SPVSupport -> IO ()
runServer (Config port persistDir logDir _verbose _gl) spv = do
  (traverse_.traverse_) (createDirectoryIfMissing True) [persistDir, logDir]
  emptyCache <- newLruHandle 100
  case persistDir of
    Nothing -> withSqlitePactDb serialisePact_raw_spaninfo ":memory:" $ \pdb -> do
      runServer_ (ServerRuntime pdb emptyCache spv) port logDir
    Just pdir -> withSqlitePactDb serialisePact_raw_spaninfo (T.pack pdir <> "pactdb.sqlite") $ \pdb -> do
      runServer_ (ServerRuntime pdb emptyCache spv) port logDir

runServer_ :: ServerRuntime -> Port -> Maybe FilePath -> IO ()
runServer_ env port logDir = bracket setupLogger teardownLogger runServer'

  where
  runServer' (logger, _) =
    runSettings (settings logger) $ cors (const corsPolicy) app
  teardownLogger (_, remover) = void remover
  setupLogger = do
    lt <- case logDir of
      Just ld -> pure (LogFileNoRotate ld 4096)
      Nothing -> pure (LogStdout 4096)
    apf <- initLogger FromFallback lt =<< newTimeCache simpleTimeFormat
    let remover = logRemover apf
    pure (apacheLogger apf, remover)
  app = serve (Proxy @API) (server env)
  settings logger = defaultSettings
    & setPort port
    & setHost "127.0.0.1"
    & setLogger logger
  corsPolicy = Just CorsResourcePolicy
    { corsOrigins = Nothing
    , corsMethods = ["GET", "POST"]
    , corsRequestHeaders = ["authorization", "content-type"]
    , corsExposedHeaders = Nothing
    , corsMaxAge = Just $ 60*60*24 -- one day
    , corsVaryOrigin = False
    , corsRequireOrigin = False
    , corsIgnoreFailures = False
    }

server :: ServerRuntime -> Server API
server env =
  (sendHandler env
  :<|> pollHandler env
  :<|> listenHandler env
  :<|> localHandler env)
  :<|> versionHandler

versionHandler :: Handler Text
versionHandler = pure $ T.pack $ "pact version " <> showVersion PI.version

pollHandler :: ServerRuntime -> PollRequest -> Handler PollResponse
pollHandler cenv (PollRequest rks) = do
  h <- traverse (listenHandler cenv . ListenRequest) rks
  let hr = NE.map (\(ListenResponse r) -> r) h
      hres = HM.fromList $ NE.toList (NE.zip rks hr)
  pure $ PollResponse hres

sendHandler :: ServerRuntime -> SendRequest -> Handler SendResponse
sendHandler runtime (SendRequest submitBatch) = do
    requestKeys <- forM (_sbCmds submitBatch) $ \cmd -> do
      let requestKey = cmdToRequestKey cmd
      _ <- liftIO $ cached (_srRequestCache runtime) requestKey (computeResultAndUpdateState requestKey cmd)
      pure requestKey
    pure $ SendResponse $ RequestKeys requestKeys
   where
        computeResultAndUpdateState :: RequestKey -> Command Text -> IO (CommandResult Hash (PactErrorCompat Info))
        computeResultAndUpdateState requestKey cmd =
          case verifyCommand @(StableEncoding PublicMeta) (fmap E.encodeUtf8 cmd) of
            ProcFail errStr -> do
              let pe = PEExecutionError (EvalError (T.pack errStr)) [] def
              pure $ pactErrorToCommandResult requestKey pe (Gas 0)

            ProcSucc (Command (Payload (Exec (ExecMsg code d)) _ _ signer mverif _) _ h) -> do
              let parsedCode = _pcExps code
                  msgData = MsgData
                    { mdData = d
                    , mdHash = h
                    , mdStep = Nothing
                    , mdSigners = signer
                    , mdVerifiers = maybe [] (fmap void) mverif
                    }
              evalExec Transactional (_srDbEnv runtime) (_srSPVSupport runtime) freeGasModel mempty SimpleNamespacePolicy
                def msgData def parsedCode >>= \case
                Left pe ->
                  pure $ pactErrorToCommandResult requestKey pe (Gas 0)
                Right evalResult ->
                  pure $ evalResultToCommandResult requestKey evalResult

            ProcSucc (Command (Payload (Continuation contMsg) _ _ signer mverif _) _ h) -> do
                  let msgData = MsgData
                        { mdData = _cmData contMsg
                        , mdHash = h
                        , mdStep = Just $ DefPactStep (_cmStep contMsg) (_cmRollback contMsg) (_cmPactId contMsg) Nothing
                        , mdSigners = signer
                        , mdVerifiers = maybe [] (fmap void) mverif
                        }
                      cont = Cont
                        { _cPactId = _cmPactId contMsg
                        , _cStep = _cmStep contMsg
                        , _cRollback = _cmRollback contMsg
                        , _cProof = _cmProof contMsg
                        }
                  evalContinuation Transactional  (_srDbEnv runtime) (_srSPVSupport runtime) freeGasModel mempty
                    SimpleNamespacePolicy def msgData def cont >>= \case
                    Left pe ->
                      pure $ pactErrorToCommandResult requestKey pe (Gas 0)
                    Right evalResult -> pure $ evalResultToCommandResult requestKey evalResult

        evalResultToCommandResult :: RequestKey -> EvalResult -> CommandResult Hash (PactErrorCompat Info)
        evalResultToCommandResult requestKey (EvalResult out logs exec gas _lm txid _lgas ev) =
          CommandResult
          { _crReqKey = requestKey
          , _crTxId = txid
          , _crResult = evalOutputToCommandResult out
          , _crGas = gas
          , _crLogs = Just (hashTxLogs logs)
          , _crEvents = ev
          , _crContinuation = exec
          , _crMetaData = Nothing
          }
        pactErrorToCommandResult :: RequestKey -> PactError Info -> Gas -> CommandResult Hash (PactErrorCompat Info)
        pactErrorToCommandResult rk pe gas = CommandResult
          { _crReqKey = rk
          , _crTxId = Nothing
          , _crResult = PactResultErr $ PEPact5Error $ pactErrorToErrorCode pe
          , _crGas = gas
          , _crLogs = Nothing
          , _crEvents = [] -- todo
          , _crContinuation = Nothing
          , _crMetaData = Nothing
          }

  -- TODO: once base-4.19 switch to L.unsnoc
        evalOutputToCommandResult :: [CompileValue Info] -> PactResult (PactErrorCompat Info)
        evalOutputToCommandResult li = case L.uncons $ L.reverse li of
            Just (v, _) -> PactResultOk (compileValueToPactValue v)
            Nothing -> PactResultErr $ PEPact5Error $ pactErrorToErrorCode $ PEExecutionError (EvalError "empty input") [] def

localHandler :: ServerRuntime -> LocalRequest -> Handler LocalResponse
localHandler env (LocalRequest cmd) = do
  (SendResponse (RequestKeys rks))  <- sendHandler env (SendRequest (SubmitBatch $ cmd NE.:| []))
  PollResponse pr <- pollHandler env (PollRequest rks)
  case HM.toList pr of
    (_, cmdResult): _ -> pure (LocalResponse cmdResult)
    [] -> throwError err404

listenHandler :: ServerRuntime -> ListenRequest -> Handler ListenResponse
listenHandler env (ListenRequest key) = do
    let (LRU.LruHandle cacheRef) = _srRequestCache env
    cache <- liftIO $ readIORef cacheRef

    -- Since the response is calculated synchronously in the send handler,
    -- we can immediately look up the key. If the key is not found,
    -- it indicates that the request key is invalid.
    case LRU.lookup key cache of
        Just (result, _) -> pure (ListenResponse result)
        Nothing -> throwError err404
