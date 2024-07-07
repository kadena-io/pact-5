{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

import Control.Monad.IO.Class
import Control.Monad.Except
import Control.Concurrent.MVar
-- import Data.ByteString
import qualified Data.LruCache as LRU
import Data.IORef
import Data.LruCache.IO as LRU
import Data.Text
import qualified Data.Text.Encoding as E
import Data.Traversable
import Data.Word
import GHC.Generics
import Servant.API
import Servant.Server
-- import System.FilePath

import Pact.Core.Builtin
import Pact.Core.Compile
import Pact.Core.Command.RPC
import Pact.Core.Command.Types
import Pact.Core.ChainData
import Pact.Core.Errors
import Pact.Core.Environment.Types
import Pact.Core.Evaluate
-- import Pact.Core.Gas.Types
-- import Pact.Core.Info
import Pact.Core.Persistence.Types
import Pact.Core.StableEncoding
import Pact.Core.Syntax.ParseTree
import Pact.Core.Command.Client

-- | Commandline configuration for running a Pact server.
data Config = Config {
  _port :: Word16,
  _persistDir :: Maybe FilePath,
  _logDir :: FilePath,
  _pragmas :: [Pragma],
  _verbose :: Bool,
  -- _entity :: Maybe EntityName,
  _gasLimit :: Maybe Int,
  _gasRate :: Maybe Int
  -- _execConfig :: Maybe ExecutionConfig
  } deriving (Eq,Show,Generic)

-- | Pragma for configuring a SQLite database.
newtype Pragma = Pragma Text
  deriving (Eq, Show, Generic)

-- | Temporarily pretend our Log type in CommandResult is unit.
type Log = ()

-- | Runtime environment for a Pact server.
data CommandEnv = CommandEnv {
    --   _ceEntity :: Maybe EntityName
      _ceMode :: ExecutionMode
    , _ceDbEnv :: PactDb CoreBuiltin ()
    -- , _ceLogger :: Logger
    -- , _ceGasEnv :: GasEnv
    , _cePublicData :: PublicData
    -- , _ceSPVSupport :: SPVSupport
    , _ceNetworkId :: Maybe NetworkId
    -- , _ceExecutionConfig :: ExecutionConfig
    , _ceEvalEnv :: EvalEnv CoreBuiltin ()
    , _ceEvalState :: MVar (EvalState CoreBuiltin ())
    , _ceRequestCache :: LruHandle RequestKey (CommandResult Log PactErrorI)
    }

type API =
       "v1" :> "send" :> ReqBody '[JSON] SubmitBatch :> Post '[JSON] RequestKeys
  :<|> "v1" :> "listen" :> Capture "requestKey" RequestKey :> Get '[JSON] (CommandResult Log PactErrorI)

server :: CommandEnv -> Server API
server env =
    sendHandler env
    :<|> listenHandler env

sendHandler :: CommandEnv -> SubmitBatch -> Handler RequestKeys
sendHandler env submitBatch = do
    requestKeys <- forM (_sbCmds submitBatch) (\cmd -> liftIO $ do
        -- let evalEnv = _ceEvalEnv env
        let requestKey = cmdToRequestKey cmd
        _ <- cached (_ceRequestCache env) requestKey (computeResultAndUpdateState requestKey cmd)
        pure requestKey)
            -- TODO, stick result into the cache.
        -- TODO, stick result into the cache.
    pure $ RequestKeys requestKeys

    where
        computeResultAndUpdateState :: RequestKey -> Command Text -> IO (CommandResult Log PactErrorI)
        computeResultAndUpdateState requestKey cmd = do
            modifyMVar (_ceEvalState env) $ \evalState -> do
                case verifyCommand @(StableEncoding PublicMeta) (fmap E.encodeUtf8 cmd) of
                    ProcFail _ -> error "TODO"
                    ProcSucc Command{ _cmdPayload = Payload { _pPayload = Exec execMsg }} -> do
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
        
        evalResultToCommandResult :: RequestKey -> EvalResult [TopLevel ()] -> CommandResult Log PactErrorI
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
        
        evalOutputToCommandResult :: [CompileValue ()] -> PactResult PactErrorI
        evalOutputToCommandResult = \case
            [InterpretValue v _info] -> PactResultOk v
            other -> error ("Wanted single InterpretValue. Got\n" ++ show other)

        contMsgToEvalInput :: ContMsg -> EvalInput
        contMsgToEvalInput = undefined

listenHandler :: CommandEnv -> RequestKey -> Handler (CommandResult Log PactErrorI)
listenHandler env key = do
    let (LRU.LruHandle cacheRef) = _ceRequestCache env
    cache <- liftIO $ readIORef cacheRef
    case LRU.lookup key cache of
        Just (result, _) -> pure result
        Nothing -> throwError err404