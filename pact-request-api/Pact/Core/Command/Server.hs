{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Pact.Core.Command.Server
  ( CommandEnv(..)
  , API(..)
  , PollRequest(..)
  , PollResponses(..)
  , ListenRequest(..)
  , ListenResponse(..)
  , Log
  , runServer
  , server
  , defaultEnv) where

import Control.Concurrent.MVar
import Control.Lens
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Default
import qualified Data.HashMap.Strict as HM
import Data.IORef
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.LruCache as LRU
import Data.LruCache.IO
import qualified Data.LruCache.IO as LRU
import Data.Proxy
import Data.Set (Set)
import Data.Text
import qualified Data.Text as T
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
import Pact.Core.Info
import Pact.Core.Persistence.MockPersistence
import Pact.Core.Persistence.Types
import Pact.Core.SPV
import Pact.Core.Serialise
import Pact.Core.StableEncoding
import qualified Pact.JSON.Decode as JD
import qualified Pact.JSON.Encode as JE
import qualified Pact.JSON.Legacy.Utils as JL
import Servant.API
import Servant.Server

-- | Commandline configuration for running a Pact server.
data Config = Config
  { _port :: Word16
  , _persistDir :: Maybe FilePath
  , _logDir :: FilePath
  , _pragmas :: [Pragma]
  , _verbose :: Bool
  , _gasLimit :: Maybe Int
  , _gasRate :: Maybe Int
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

defaultEnv :: IO CommandEnv
defaultEnv = do
  pdb <- mockPactDb serialisePact_raw_spaninfo
  ee <- defaultEvalEnv pdb coreBuiltinMap
  es <- newMVar def
  emptyCache <- newLruHandle 100
  gasRef <- newIORef mempty
  let gasEnv = GasEnv
        { _geGasRef = gasRef
        , _geGasLog = Nothing
        , _geGasModel = tableGasModel $ MilliGasLimit $ MilliGas 10000000000
        }
  pure $ CommandEnv Transactional pdb gasEnv def noSPVSupport Nothing mempty ee es emptyCache

newtype PollRequest
  = PollRequest (NE.NonEmpty RequestKey)

instance JD.FromJSON PollRequest where
  parseJSON = JD.withObject "Poll" $ \o -> PollRequest <$> o JD..: "requestKeys"

instance JE.Encode PollRequest where
  build (PollRequest rks) = JE.object [ "requestKeys" JE..= JE.Array rks ]

newtype PollResponses
  = PollResponses (HM.HashMap RequestKey (CommandResult Log (PactErrorCode Info)))

instance JE.Encode PollResponses where
  build (PollResponses m) = JE.build $ JL.legacyHashMap requestKeyToB16Text m

instance JD.FromJSON PollResponses where
  parseJSON v = PollResponses <$> JD.withObject "PollResponses" (\_ -> JD.parseJSON v) v

newtype ListenRequest
  = ListenRequest RequestKey

instance JE.Encode ListenRequest where
  build (ListenRequest rk) = JE.object [ "listen" JE..= rk ]

instance JD.FromJSON ListenRequest where
  parseJSON = JD.withObject "ListenRequest" $ \o ->
    ListenRequest <$> o JD..: "listen"

newtype ListenResponse
  = ListenResponse (CommandResult Log (PactErrorCode Info))

instance JD.FromJSON ListenResponse where
  parseJSON v = ListenResponse <$> JD.parseJSON v

instance JE.Encode SpanInfo where
  build (SpanInfo ls cs le ce) = JE.object
    [ "line_start" JE..= JE.Aeson ls
    , "column_start" JE..= JE.Aeson cs
    , "line_end" JE..= JE.Aeson le
    , "column_end" JE..= JE.Aeson ce
    ]
instance JD.FromJSON SpanInfo where
  parseJSON = JD.withObject "SpanInfo" $ \o ->
    SpanInfo
      <$> o JD..: "line_start" <*> o JD..: "column_start"
      <*> o JD..: "line_end" <*> o JD..: "column_end"

instance JE.Encode ListenResponse where
  build (ListenResponse r) = JE.build r

instance JE.Encode Log where
  build _ = JE.null

type API = "api" :> "v1" :>
           (("send" :> ReqBody '[PactJson] SubmitBatch :> Post '[PactJson] RequestKeys)
       :<|> ("poll" :> ReqBody '[PactJson] PollRequest :> Post '[PactJson] PollResponses)
       :<|> ("listen" :> ReqBody '[PactJson] ListenRequest :> Post '[PactJson] ListenResponse)
       :<|> ("local" :> ReqBody '[PactJson] (Command Text) :> Post '[PactJson] (CommandResult Log (PactErrorCode Info))))


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
                    ProcFail errStr -> pure (evalState
                                            , pactErrorToCommandResult requestKey $ PEExecutionError (EvalError (T.pack errStr)) [] def)
                    ProcSucc (Command (Payload (Exec execMsg) _ _ _ _ _) _ _) -> do
                      let parsedCode = Right $ _pcExps (_pmCode execMsg)
                      (evalState', result) <- interpretReturningState (_ceEvalEnv env) evalState parsedCode
                      case result of
                        Right goodRes -> pure (evalState', evalResultToCommandResult requestKey goodRes)
                        Left err -> pure (evalState', pactErrorToCommandResult requestKey err)
                    ProcSucc (Command (Payload (Continuation contMsg) _ _ _ _ _) _ _) -> do
                      let evalInput = contMsgToEvalInput contMsg
                      (evalState', result) <- interpretReturningState (_ceEvalEnv env) evalState evalInput
                      case result of
                        Right goodRes -> pure (evalState', evalResultToCommandResult requestKey goodRes)
                        Left err -> pure (evalState', pactErrorToCommandResult requestKey err)

        evalResultToCommandResult :: RequestKey -> EvalResult -> CommandResult Log (PactErrorCode Info)
        evalResultToCommandResult requestKey (EvalResult out _log _exec gas _lm txid _lgas ev) =
          CommandResult
          { _crReqKey = requestKey
          , _crTxId = txid
          , _crResult = evalOutputToCommandResult out
          , _crGas = gas
          , _crLogs = Nothing
          , _crEvents = ev
          , _crContinuation = Nothing
          , _crMetaData = Nothing
          }
        pactErrorToCommandResult :: RequestKey -> PactError Info -> CommandResult Log (PactErrorCode Info)
        pactErrorToCommandResult rk pe = CommandResult
          { _crReqKey = rk
          , _crTxId = Nothing
          , _crResult =  PactResultErr $ pactErrorToErrorCode pe
          , _crGas = Gas 0
          , _crLogs = Nothing
          , _crEvents = [] -- todo
          , _crContinuation = Nothing
          , _crMetaData = Nothing
          }

  -- TODO: once base-4.19 switch to L.unsnoc
        evalOutputToCommandResult :: [CompileValue Info] -> PactResult (PactErrorCode Info)
        evalOutputToCommandResult li = case L.uncons $ L.reverse li of
            Just (v, _) -> PactResultOk (compileValueToPactValue v)
            Nothing -> PactResultErr $ pactErrorToErrorCode $ PEExecutionError (EvalError "empty input") [] def

        contMsgToEvalInput :: ContMsg -> EvalInput
        contMsgToEvalInput = undefined

localHandler :: CommandEnv -> Command Text -> Handler (CommandResult Log (PactErrorCode Info))
localHandler = undefined

listenHandler :: CommandEnv -> ListenRequest -> Handler ListenResponse
listenHandler env (ListenRequest key) = do
    let (LRU.LruHandle cacheRef) = _ceRequestCache env
    cache <- liftIO $ readIORef cacheRef

    -- Since the response is calculated synchronously in the send handler,
    -- we can immediately look up the key. If the key is not found,
    -- it indicates that the request key is invalid.
    case LRU.lookup key cache of
        Just (result, _) -> pure (ListenResponse result)
        Nothing -> throwError err404
