{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE InstanceSigs #-}

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
  , server
  , requestKeyError
  , ProcessResult(..)
  , ProcessMsg(..)
  , processMsg
  ) where

import Control.Concurrent
import Control.Exception.Safe hiding (Handler)
import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as LBS
import Data.Default
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Version
import Network.Wai.Handler.Warp
import Network.Wai.Logger
import Network.Wai.Middleware.Cors
import Pact.Core.Builtin
import Pact.Core.ChainData
import Pact.Core.Command.Client
import Pact.Core.Command.RPC
import Pact.Core.Command.Server.Config
import Pact.Core.Command.Server.History
import Pact.Core.Command.Server.Servant
import Pact.Core.Command.Types
import Pact.Core.Compile
import Pact.Core.Errors
import Pact.Core.Evaluate
import Pact.Core.Gas
import Pact.Core.Hash
import Pact.Core.Namespace
import Pact.Core.Persistence.Types
    ( hashTxLogs, ExecutionMode(..), PactDb )
import Pact.Core.SPV
import Pact.Core.StableEncoding
import qualified PackageInfo_pact_tng as PI
import qualified Pact.JSON.Decode as JD
import qualified Pact.JSON.Encode as JE
import qualified Pact.JSON.Legacy.Utils as JL
import Servant.API
import Servant.Server
import System.Directory
import System.FilePath
import System.Log.FastLogger.Date

-- | Temporarily pretend our Log type in CommandResult is unit.
type Log = ()

data ProcessResult
  = PESuccess
  | PEExistingRequestKey
  | PEUnknownException SomeException

instance Show ProcessResult where
  show = \case
    PESuccess -> "Success"
    PEExistingRequestKey -> "ExistingRequestKey"
    PEUnknownException _ -> "UnkownException"

data ProcessMsg
  = StoreMsg RequestKey (CommandResult Hash PactOnChainError) (MVar ProcessResult)

instance Show ProcessMsg where
  show = \case
    StoreMsg rk _ _ -> show rk

-- | Runtime environment for a Pact server.
data ServerRuntime
  = ServerRuntime
  { _srDbEnv :: PactDb CoreBuiltin Info
  , _srHistoryDb :: HistoryDb
  , _srSPVSupport :: SPVSupport
  , _srvChan :: Chan ProcessMsg
  }

newtype PollRequest
  = PollRequest (NE.NonEmpty RequestKey)
  deriving newtype (Eq, Show)

instance JD.FromJSON PollRequest where
  parseJSON = JD.withObject "Poll" $ \o -> PollRequest <$> o JD..: "requestKeys"

instance JE.Encode PollRequest where
  build (PollRequest rks) = JE.object [ "requestKeys" JE..= JE.Array rks ]

newtype PollResponse
  = PollResponse (HM.HashMap RequestKey (CommandResult Hash (PactOnChainError)))
  deriving newtype (Eq, Show)

instance JE.Encode PollResponse where
  build (PollResponse pr) = JE.build $ JL.legacyHashMap requestKeyToB64Text pr

instance JD.FromJSON PollResponse where
  parseJSON v = do
    o <- JD.parseJSON v
    pure $ PollResponse o

newtype ListenRequest
  = ListenRequest RequestKey
  deriving newtype (Eq, Show)

instance JE.Encode ListenRequest where
  build (ListenRequest rk) = JE.object [ "listen" JE..= rk ]

instance JD.FromJSON ListenRequest where
  parseJSON = JD.withObject "ListenRequest" $ \o ->
    ListenRequest <$> o JD..: "listen"

newtype ListenResponse
  = ListenResponse (CommandResult Hash (PactOnChainError))
  deriving newtype (Eq, Show)

instance JD.FromJSON ListenResponse where
  parseJSON v = ListenResponse <$> JD.parseJSON v

instance JE.Encode ListenResponse where
  build (ListenResponse m) = JE.build m

newtype LocalRequest
  = LocalRequest { _localRequest :: Command Text }

instance JE.Encode LocalRequest where
  build (LocalRequest cmd) = JE.build cmd

newtype LocalResponse
  = LocalResponse { _localResponse :: CommandResult Hash (PactOnChainError) }

instance JD.FromJSON LocalResponse where
  parseJSON v = LocalResponse <$> JD.parseJSON v

instance JD.FromJSON LocalRequest where
  parseJSON v = LocalRequest <$> JD.parseJSON v

instance JE.Encode LocalResponse where
  build (LocalResponse cmdr) = JE.build cmdr

newtype SendRequest
  = SendRequest { _sendRequest :: SubmitBatch }
  deriving newtype (Eq, Show)


instance JE.Encode SendRequest where
  build (SendRequest sr) = JE.build sr

instance JD.FromJSON SendRequest where
  parseJSON v = SendRequest <$> JD.parseJSON v

newtype SendResponse
  = SendResponse RequestKeys
  deriving newtype (Eq, Show)

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

  chan <- newChan

  case persistDir of
    Nothing -> withSqliteAndHistoryDb ":memory:" $
      \pdb histDb ->
        runServer_ (ServerRuntime pdb histDb spv chan) port logDir
    Just pdir -> do
      let pdir' = T.pack $ pdir </> "pactdb.sqlite"
      withSqliteAndHistoryDb pdir' $ \pdb histDb ->
        runServer_ (ServerRuntime pdb histDb spv chan) port logDir

processMsg :: ServerRuntime -> IO ()
processMsg env = do
  el <- readChan (_srvChan env)
  case el of
    StoreMsg rk cmd result -> _histDbRead (_srHistoryDb env) rk >>= \case
      Nothing -> _histDbInsert (_srHistoryDb env) rk cmd >>= \case
        Right _ -> putMVar result PESuccess
        Left e -> putMVar result (PEUnknownException e)
      Just _ -> putMVar result PEExistingRequestKey


runServer_ :: ServerRuntime -> Port -> Maybe FilePath -> IO ()
runServer_ env port logDir = bracket setupLogger teardownLogger runServer'
  where
  runServer' (logger, _,_) = do
    runSettings (settings logger) $ cors (const corsPolicy) app
  teardownLogger (_, remover, tid) = do
    killThread tid
    void remover
  setupLogger = do
    tid <- forkIO $ forever (processMsg env)
    lt <- case logDir of
      Just ld -> do
        let ld' = ld </> "pact-server.log"
        pure (LogFileNoRotate ld' 4096)
      Nothing -> pure (LogStdout 4096)
    apf <- initLogger FromFallback lt =<< newTimeCache simpleTimeFormat
    let remover = logRemover apf
    pure (apacheLogger apf, remover, tid)
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
      res <- liftIO $ try $! do
        result <- computeResultAndUpdateState runtime Transactional requestKey cmd
        storeResult <- newEmptyMVar
        writeChan (_srvChan runtime) (StoreMsg requestKey result storeResult)
        readMVar storeResult
      case res of
        Right PESuccess -> pure requestKey
        Right PEExistingRequestKey -> throwError err400{errBody = requestKeyError requestKey}
        Right (PEUnknownException _) -> throwError err500
        Left (_::SomeException)-> throwError err500
    pure $ SendResponse $ RequestKeys requestKeys

computeResultAndUpdateState :: ServerRuntime -> ExecutionMode -> RequestKey -> Command Text -> IO (CommandResult Hash (PactOnChainError))
computeResultAndUpdateState runtime execMode requestKey cmd =
  case verifyCommand @(StableEncoding PublicMeta) (fmap E.encodeUtf8 cmd) of
    ProcFail errStr -> do
      let pe = PEExecutionError (EvalError (T.pack errStr)) [] def
      pure $ pactErrorToCommandResult requestKey pe (Gas 0)

    ProcSucc (Command (Payload (Exec (ExecMsg code d)) _ _ signer mverif _) _ h) -> do
      let parsedCode = _pcExps code
          msgData = MsgData
            { mdData = d
            , mdHash = h
            , mdSigners = signer
            , mdVerifiers = maybe [] (fmap void) mverif
            }
      ge <- mkFreeGasEnv GasLogsDisabled
      evalExec (RawCode (_pcCode code)) execMode (_srDbEnv runtime) (_srSPVSupport runtime) ge mempty SimpleNamespacePolicy
        def msgData def parsedCode >>= \case
        Left pe ->
          pure $ pactErrorToCommandResult requestKey pe (Gas 0)
        Right evalResult ->
          pure $ evalResultToCommandResult requestKey evalResult

    ProcSucc (Command (Payload (Continuation contMsg) _ _ signer mverif _) _ h) -> do
      let msgData = MsgData
            { mdData = _cmData contMsg
            , mdHash = h
            , mdSigners = signer
            , mdVerifiers = maybe [] (fmap void) mverif
            }
          cont = Cont
            { _cPactId = _cmPactId contMsg
            , _cStep = _cmStep contMsg
            , _cRollback = _cmRollback contMsg
            , _cProof = _cmProof contMsg
            }
      ge <- mkFreeGasEnv GasLogsDisabled
      evalContinuation execMode (_srDbEnv runtime) (_srSPVSupport runtime) ge mempty
        SimpleNamespacePolicy def msgData def cont >>= \case
          Left pe ->
            pure $ pactErrorToCommandResult requestKey pe (Gas 0)
          Right evalResult -> pure $ evalResultToCommandResult requestKey evalResult

evalResultToCommandResult :: RequestKey -> EvalResult -> CommandResult Hash (PactOnChainError)
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

pactErrorToCommandResult :: RequestKey -> PactError Info -> Gas -> CommandResult Hash (PactOnChainError)
pactErrorToCommandResult rk pe gas = CommandResult
  { _crReqKey = rk
  , _crTxId = Nothing
  , _crResult = PactResultErr $ pactErrorToOnChainError pe
  , _crGas = gas
  , _crLogs = Nothing
  , _crEvents = [] -- todo
  , _crContinuation = Nothing
  , _crMetaData = Nothing
  }

 -- TODO: once base-4.19 switch to L.unsnoc
evalOutputToCommandResult :: [CompileValue Info] -> PactResult (PactOnChainError)
evalOutputToCommandResult li = case L.uncons $ L.reverse li of
  Just (v, _) -> PactResultOk (compileValueToPactValue v)
  Nothing -> PactResultErr $ pactErrorToOnChainError $ PEExecutionError (EvalError "empty input") [] def

localHandler :: ServerRuntime -> LocalRequest -> Handler LocalResponse
localHandler env (LocalRequest cmd) = do
  let requestKey = cmdToRequestKey cmd
  res <- liftIO $ try $! do
    result <- computeResultAndUpdateState env Local requestKey cmd
    storeResult <- newEmptyMVar
    writeChan (_srvChan env) (StoreMsg requestKey result storeResult)
    (result,) <$> readMVar storeResult
  case res of
    Right (result, PESuccess) -> pure $ LocalResponse result
    Right (_,PEExistingRequestKey) -> throwError err400{errBody = requestKeyError requestKey}
    Right (_, PEUnknownException _) -> throwError err500
    Left (_::SomeException)-> throwError err500

listenHandler :: ServerRuntime -> ListenRequest -> Handler ListenResponse
listenHandler env (ListenRequest key) = do
  mResult <- liftIO $ _histDbRead (_srHistoryDb env) key
  case mResult of
    Just result -> pure (ListenResponse result)
    Nothing -> throwError err404


requestKeyError :: RequestKey -> LBS.ByteString
requestKeyError (RequestKey h) = "Request key: "
                                 <> LBS.fromStrict (E.encodeUtf8 (hashToText h))
                                 <> " has already been submitted."
                                 <> " If you are resubmitting a valid transaction, try changing the nonce in the payload"
