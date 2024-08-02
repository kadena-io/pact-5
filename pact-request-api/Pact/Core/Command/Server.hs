{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Pact.Core.Command.Server
  ( CommandEnv(..)
  , runServer ) where

import Control.Concurrent.MVar
import Control.Lens
import Control.Monad.Except
import Control.Monad.IO.Class
import qualified Data.HashMap.Strict as HM
import Data.IORef
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.LruCache as LRU
import Data.LruCache.IO as LRU
import Data.Proxy
import Data.Set (Set)
import Data.Text
import qualified Data.Text.Encoding as E
import Data.Traversable
import Data.Word
import GHC.Generics
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Pact.Core.Builtin
import Pact.Core.ChainData
import Pact.Core.Command.Client
import Pact.Core.Command.RPC
import Pact.Core.Command.Server.Servant
import Pact.Core.Command.Types
import Pact.Core.Compile
import Pact.Core.Environment.Types
import Pact.Core.Errors
import Pact.Core.Evaluate
import Pact.Core.Gas
import Pact.Core.Hash
import Pact.Core.Persistence.Types
import Pact.Core.SPV
import Pact.Core.Info
import Pact.Core.StableEncoding
import qualified Pact.JSON.Decode as JD
import qualified Pact.JSON.Encode as JE
import qualified Pact.JSON.Legacy.Utils as JL
import Servant.API
import Servant.Server


-- | Commandline configuration for running a Pact server.
data Config = Config {
  _port :: Word16,
  _persistDir :: Maybe FilePath,
  _logDir :: FilePath,
  _pragmas :: [Pragma],
  _verbose :: Bool,
  _gasLimit :: Maybe Int,
  _gasRate :: Maybe Int
  } deriving (Eq,Show,Generic)

-- | Pragma for configuring a SQLite database.
newtype Pragma = Pragma Text
  deriving (Eq, Show, Generic)

-- | Temporarily pretend our Log type in CommandResult is unit.
type Log = ()

-- | Runtime environment for a Pact server.
data CommandEnv
  = CommandEnv
  { _ceMode :: ExecutionMode
  , _ceDbEnv :: PactDb CoreBuiltin Info
  , _ceGasEnv :: GasEnv CoreBuiltin Info
  , _cePublicData :: PublicData
  , _ceSPVSupport :: SPVSupport
  , _ceNetworkId :: Maybe NetworkId
  , _ceExecutionConfig :: Set ExecutionFlag
  , _ceEvalEnv :: EvalEnv CoreBuiltin Info
  , _ceEvalState :: MVar (EvalState CoreBuiltin Info)
  , _ceRequestCache :: LruHandle RequestKey (CommandResult Log (PactErrorCode Info))
  }


newtype PollRequest
  = PollRequest (NE.NonEmpty RequestKey)

instance JD.FromJSON PollRequest where
  parseJSON = JD.withObject "Poll" $ \o -> PollRequest <$> o JD..: "requestKeys"

newtype PollResponses
  = PollResponses (HM.HashMap RequestKey (CommandResult Log (PactErrorCode Info)))

instance JE.Encode PollResponses where
  build (PollResponses m) = JE.build $ JL.legacyHashMap requestKeyToB16Text m

newtype ListenRequest
  = ListenRequest RequestKey

-- instance JD.Encode ListenRequest where
--   build (ListenRequest rk) = JD.build rk

instance JD.FromJSON ListenRequest where
  parseJSON = JD.withObject "ListenRequest" $ \o ->
    ListenRequest <$> o JD..: "listen"

newtype ListenResponse
  = ListenResponse (CommandResult Log (PactErrorCode Info))

instance JE.Encode SpanInfo where
  build (SpanInfo ls cs le ce) = JE.object
    [ "line_start" JE..= JE.Aeson ls
    , "column_start" JE..= JE.Aeson cs
    , "line_end" JE..= JE.Aeson le
    , "column_end" JE..= JE.Aeson ce
    ]

instance JE.Encode ListenResponse where
  build (ListenResponse r) = JE.build r

instance JE.Encode Log where
  build _ = JE.null

type API = "api" :> "v1" :>
           (("send" :> ReqBody '[JSON] SubmitBatch :> Post '[PactJson] RequestKeys)
       :<|> ("poll" :> ReqBody '[JSON] PollRequest :> Post '[PactJson] PollResponses)
       :<|> ("listen" :> ReqBody '[JSON] ListenRequest :> Post '[PactJson] ListenResponse)
       :<|> ("local" :> ReqBody '[JSON] (Command Text) :> Post '[PactJson] (CommandResult Log (PactErrorCode Info))))

runServer :: CommandEnv -> Port -> IO ()
runServer env port = runSettings settings $ cors (const corsPolicy) app
  where
  app = serve (Proxy @API) (server env)
  settings = defaultSettings
    & setPort port
    & setHost "127.0.0.1"
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

server :: CommandEnv -> Server API
server env =
  sendHandler env
  :<|> pollHandler env
  :<|> listenHandler env
  :<|> localHandler env

pollHandler :: CommandEnv -> PollRequest -> Handler PollResponses
pollHandler cenv (PollRequest rks) = do
  h <- traverse (listenHandler cenv . ListenRequest) rks
  let hr = NE.map (\(ListenResponse r) -> r) h
      hres = HM.fromList $ NE.toList (NE.zip rks hr)
  pure $ PollResponses hres

sendHandler :: CommandEnv -> SubmitBatch -> Handler RequestKeys
sendHandler env submitBatch = do
    requestKeys <- forM (_sbCmds submitBatch) $ \cmd -> do
      let requestKey = cmdToRequestKey cmd
      _ <- liftIO $ cached (_ceRequestCache env) requestKey (computeResultAndUpdateState requestKey cmd)
      pure requestKey
    pure $ RequestKeys requestKeys

    where
        computeResultAndUpdateState :: RequestKey -> Command Text -> IO (CommandResult Log (PactErrorCode Info))
        computeResultAndUpdateState requestKey cmd = do
            modifyMVar (_ceEvalState env) $ \evalState -> do
                case verifyCommand @(StableEncoding PublicMeta) (fmap E.encodeUtf8 cmd) of
                    ProcFail _ -> error "TODO"
                    ProcSucc Command { _cmdPayload = Payload { _pPayload = Exec execMsg }} -> do
                        let parsedCode = Right $ _pcExps (_pmCode execMsg)
                        (evalState', result) <- interpretReturningState (_ceEvalEnv env) evalState parsedCode
                        case result of
                            Right goodRes -> pure (evalState', evalResultToCommandResult requestKey goodRes)
                            Left _ -> error "TODO"
                    ProcSucc Command { _cmdPayload = Payload { _pPayload = Continuation contMsg }} -> do
                        let evalInput = contMsgToEvalInput contMsg
                        (evalState', result) <- interpretReturningState (_ceEvalEnv env) evalState evalInput
                        case result of
                            Right goodRes -> pure (evalState', evalResultToCommandResult requestKey goodRes)
                            Left _ -> error "TODO"

        evalResultToCommandResult :: RequestKey -> EvalResult -> CommandResult Log (PactErrorCode Info)
        evalResultToCommandResult requestKey EvalResult {_erOutput, _erLogs, _erExec, _erGas, _erTxId, _erEvents} =
            CommandResult {
                _crReqKey = requestKey,
                _crTxId = _erTxId,
                _crResult = evalOutputToCommandResult _erOutput,
                _crGas = _erGas,
                _crLogs = Nothing, -- TODO
                _crEvents = _erEvents,
                _crContinuation = Nothing,
                _crMetaData = Nothing -- TODO
            }
  -- TODO: once base-4.19 switch to L.unsnoc
        evalOutputToCommandResult :: [CompileValue Info] -> PactResult (PactErrorCode i)
        evalOutputToCommandResult li = case L.uncons $ L.reverse li of
            Just (v, _) -> PactResultOk (compileValueToPactValue v)
            Nothing -> PactResultErr undefined --PactError (PEExecutionError (EvalError "empty input") [] def)

        contMsgToEvalInput :: ContMsg -> EvalInput
        contMsgToEvalInput = undefined

localHandler :: CommandEnv -> Command Text -> Handler (CommandResult Log (PactErrorCode Info))
localHandler = undefined

listenHandler :: CommandEnv -> ListenRequest -> Handler ListenResponse
listenHandler env (ListenRequest key) = do
    let (LRU.LruHandle cacheRef) = _ceRequestCache env
    cache <- liftIO $ readIORef cacheRef
    case LRU.lookup key cache of
        Just (result, _) -> pure (ListenResponse result)
        Nothing -> throwError err404
