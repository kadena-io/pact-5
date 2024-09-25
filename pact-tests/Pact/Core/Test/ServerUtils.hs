{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

module Pact.Core.Test.ServerUtils where

import Control.Exception
import Control.Concurrent
import Data.IORef
import qualified Data.Text as T
import Data.Text(Text)
import qualified Network.HTTP.Client as HTTP
import Network.Wai.Handler.Warp
import Pact.Core.Command.Server
import Pact.Core.Command.Server.Config
import Pact.Core.Environment
import Pact.Core.Persistence.SQLite
import Pact.Core.SPV
import Pact.Core.Serialise
import qualified Pact.JSON.Encode as J
import Servant
import Servant.Client
import System.IO.Temp
import System.IO.Unsafe
import Test.Tasty.HUnit
-- -------------------------------------------------------------------------- --
-- Global Test Manager

-- | Use a single global Manager throughout all tests
--
testMgr :: HTTP.Manager
testMgr = unsafePerformIO $ HTTP.newManager HTTP.defaultManagerSettings
{-# NOINLINE testMgr #-}

-- -------------------------------------------------------------------------- --
-- Constants

testDir :: FilePath
testDir = "pact-tests/"

serverRoot :: Port -> String
serverRoot port = "http://localhost:" ++ show port ++ "/"

sendClient :: SendRequest -> ClientM SendResponse
pollClient :: PollRequest -> ClientM PollResponse
listenClient :: ListenRequest -> ClientM ListenResponse
localClient :: LocalRequest -> ClientM LocalResponse
versionClient :: ClientM Text
(sendClient :<|> pollClient :<|> listenClient :<|> localClient) :<|> versionClient = client (Proxy @API)

withTestServe :: FilePath -> SPVSupport -> (Port -> IO a) -> IO a
withTestServe configFile spv app = do
  Config{..} <- validateConfigFile configFile
  store <- newIORef mempty
  case _persistDir of
    Nothing -> withSqlitePactDb serialisePact_raw_spaninfo ":memory:" $ \pdb -> do
      withTestApiServer (ServerRuntime pdb store spv) app
    Just pdir -> withSqlitePactDb serialisePact_raw_spaninfo (T.pack pdir <> "pactdb.sqlite") $ \pdb -> do
      withTestApiServer (ServerRuntime pdb store spv) app

-- | Runs an API server for testing.
--
-- A free port is randomly chosen by the operating system and given to the inner
-- computation.
--
-- The server only binds to localhost. This is more secure when only local
-- access is needed. It also prevents the firewall configuration window from
-- popping up on MacOSX.
--
-- Any exception in the server application is rethrown on the calling thread.
--
withTestApiServer
  :: ServerRuntime
  -> (Port -> IO a)
  -> IO a
withTestApiServer runtime =
  testWithApplicationSettings settings $
    pure (serve (Proxy @API) (server runtime))
 where
  settings = setHost "127.0.0.1" defaultSettings


-- -------------------------------------------------------------------------- --
-- Internal Tools

waitUntilStarted :: Port -> Int -> IO ()
waitUntilStarted _ i | i > 10 = throwIO $ userError "waitUntilStarted: failing after 10 attempts"
waitUntilStarted port i = do
  mgr <- HTTP.newManager HTTP.defaultManagerSettings
  baseUrl' <- parseBaseUrl $ serverRoot port
  let clientEnv = mkClientEnv mgr baseUrl'
  r <- runClientM versionClient clientEnv
  case r of
    Right _ -> pure ()
    Left _ -> do
      threadDelay 100
      waitUntilStarted port (succ i)

withTestPactServerWithSpv_ :: SPVSupport -> FilePath -> (Port -> IO a) -> IO a
withTestPactServerWithSpv_ spv configFile app =
  withTestServe configFile spv $ \port -> do
    waitUntilStarted port 0
    app port

withTestConfig :: String -> [ExecutionFlag] -> (FilePath -> IO a) -> IO a
withTestConfig label _flags inner =
  withSystemTempDirectory ("pact-tests." <> label) $ \dir -> do
    let confFilePath = dir <> "/" <> "test-config.yaml"
    J.encodeFile confFilePath $ J.object
      [ "port" J..= J.Aeson @Int 0 -- ignored by test server
      , "logDir" J..= J.string dir
      , "persistDir" J..= J.string dir
      , "pragmas" J..= J.Array @[T.Text] []
      , "verbose" J..= False
      -- , "execConfig" J..= J.Array [flags]
      ]
    inner confFilePath

-- -------------------------------------------------------------------------- --
-- Test Pact Server

withTestPactServer :: String -> (ClientEnv -> IO a) -> IO a
withTestPactServer label = withTestPactServerWithSpv label [] noSPVSupport

withTestPactServerWithSpv
  :: String
  -> [ExecutionFlag]
  -> SPVSupport
  -> (ClientEnv -> IO a)
  -> IO a
withTestPactServerWithSpv label flags spv action =
  withTestConfig label flags $ \fp ->
    withTestPactServerWithSpv_ spv fp $ \port -> do
      clientEnv <- mkClientEnv testMgr <$> parseBaseUrl (serverRoot port)
      action clientEnv

shouldSatisfy :: HasCallStack => a -> (a -> Bool) -> Assertion
shouldSatisfy actual p = assertBool "pred failed" (p actual)

shouldBe :: HasCallStack => (Eq a, Show a) => a -> a -> Assertion
shouldBe = flip (assertEqual "should be equal")
