{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- |

module Pact.Core.Test.PactServerTests where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import Data.Default
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Data.Proxy
import Data.Text
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import NeatInterpolation (text)
import qualified Network.HTTP.Types   as HTTP
import Control.Concurrent
import Network.Wai.Test.Internal
import Pact.Core.ChainData
import Pact.Core.Command.Client
import Pact.Core.Command.Crypto
import Pact.Core.Command.RPC
import Pact.Core.Command.Server
import Pact.Core.Command.Types
import Pact.Core.Command.Util
import Pact.Core.Command.Server.History
import Pact.Core.Hash
import Pact.Core.Names
import Pact.Core.PactValue
import Pact.Core.Persistence.SQLite
import Pact.Core.SPV (noSPVSupport)
import Pact.Core.Serialise
import Pact.Core.StableEncoding
import qualified Pact.JSON.Encode as J
import Servant.API
import Servant.Client
import Servant.Server
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Wai


sendClient :: SendRequest -> ClientM SendResponse
pollClient :: PollRequest -> ClientM PollResponse
listenClient :: ListenRequest -> ClientM ListenResponse
localClient :: LocalRequest -> ClientM LocalResponse
versionClient :: ClientM Text
(sendClient :<|> pollClient :<|> listenClient :<|> localClient) :<|> versionClient = client (Proxy @API)

tests :: TestTree
tests =  withResource
  mkEnv
  rmEnv
  $ \getEnv -> do
  testGroup "PactServer"
    [ t404 getEnv
    , sendTests getEnv
    , listenTests getEnv
    , versionTest getEnv
    , integrationTests getEnv
    , localTests getEnv
    , testPactContinuation getEnv
    ]
  where
  mkEnv = do
    (pdb,db,stmt) <- unsafeCreateSqlitePactDb serialisePact_raw_spaninfo ":memory:"
    (histDb, db') <- unsafeCreateHistoryDb ":memory:"
    chan <- newChan
    let
      runtime = ServerRuntime pdb histDb noSPVSupport chan
      app = serve (Proxy @API) (server runtime)
    tid <- forkIO $ forever (processMsg runtime)
    pure (app, db, db', stmt, tid)
  rmEnv (_, db, db', stmt, tid) = do
    killThread tid
    unsafeCloseSqlitePactDb db stmt
    unsafeCloseHistoryDb db'

  mkTestCase env name session = testCase name $ do
    (app, _,_, _, _) <- env
    void (runSessionWith initState session app)

  t404 env = mkTestCase env "non-existing endpoint gives 404" $ do
    r404 <- get "/does/not/exists/"
    assertStatus 404 r404
  sendTests env = testGroup "send endpoint"
    [ mkTestCase env "unsupported media type (no header set)" $ do
        res <- post "/api/v1/send" mempty
        assertStatus 415 res
    , mkTestCase env "unsupported media type (wrong media type)" $ do
        res <- postWithHeaders "/api/v1/send" mempty [(HTTP.hContentType, "text/html; charset=utf-8")]
        assertStatus 415 res
    , mkTestCase mkEnv "accept valid request" $ do
        serializedCmd <- liftIO mkSubmitBatch
        res <- postWithHeaders "/api/v1/send" serializedCmd [(HTTP.hContentType, "application/json")]
        assertStatus 200 res
    , mkTestCase mkEnv "reject already known request key" $ do
        serializedCmd <- liftIO mkSubmitBatch

        res@(SResponse _ _ reqResp) <- postWithHeaders "/api/v1/send" serializedCmd [(HTTP.hContentType, "application/json")]
        assertStatus 200 res

        let (Just (SendResponse (RequestKeys (rks NE.:| _)))) :: Maybe SendResponse = A.decodeStrict $ LBS.toStrict reqResp
        res'@(SResponse _ _ rb') <- postWithHeaders "/api/v1/send" serializedCmd [(HTTP.hContentType, "application/json")]
        assertStatus 400 res'
        liftIO $ assertEqual "Should inform about existing requestKey" (requestKeyError rks) rb'
    ]
  listenTests env = testGroup "listen endpoint"
    [ mkTestCase env "non existing request key results in 404" $ do
        -- hash with pactHashLength (32) size
        let h = Hash "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
            req = J.encode $ J.build $ ListenRequest (RequestKey h)
        res <- postWithHeaders "/api/v1/listen" req  [(HTTP.hContentType, "application/json")]
        assertStatus 404 res

    , mkTestCase env "request with invalid request key results in 400" $ do
        let h = Hash ""
            req = J.encode $ J.build $ ListenRequest (RequestKey h)
        res <- postWithHeaders "/api/v1/listen" req  [(HTTP.hContentType, "application/json")]
        assertStatus 400 res
    ]
  localTests env = testGroup "local endpoint"
    [ mkTestCase env "return correct result" $ do
        cmd <- liftIO  mkLocalRequest
        res@(SResponse _ _ reqResp) <- postWithHeaders "/api/v1/local" cmd [(HTTP.hContentType, "application/json")]
        assertStatus 200 res
        let (Just (LocalResponse cmdResult)) :: Maybe LocalResponse = A.decodeStrict $ LBS.toStrict reqResp
        liftIO $ assertEqual "Result match expected output" (PactResultOk $ PInteger 3) (_crResult cmdResult)
    ]
  integrationTests env = testGroup "integration test (combined send and listen)"
    [ mkTestCase env "send and listen request" $ do
        cmd <- liftIO mkSubmitBatch
        res@(SResponse _ _ reqResp) <- postWithHeaders "/api/v1/send" cmd [(HTTP.hContentType, "application/json")]
        assertStatus 200 res

        let (Just (SendResponse (RequestKeys rks))) :: Maybe SendResponse = A.decodeStrict $ LBS.toStrict reqResp
        liftIO $ assertBool "Response contains one request key" (NE.length rks == 1)

        let req = J.encode $ J.build $ ListenRequest (NE.head rks)

        res'@(SResponse _ _ reqResp') <- postWithHeaders "/api/v1/listen" req  [(HTTP.hContentType, "application/json")]
        assertStatus 200 res'

        let (Just (ListenResponse cmdResult)) :: Maybe ListenResponse = A.decodeStrict $ LBS.toStrict reqResp'
        liftIO $ assertEqual "Result match expected output" (PactResultOk $ PInteger 3) (_crResult cmdResult)
    ]
  versionTest env = mkTestCase env "version endpoint" $ do
    res@(SResponse _ _ cnt) <- get "/version"
    assertStatus 200 res
    let cnt' = T.decodeUtf8 $ LBS.toStrict cnt
    liftIO $ assertBool "should contain 'pact version' string" (T.isPrefixOf "pact version" cnt')

  testPactContinuation env = testGroup "pact continuation" [
    mkTestCase env "executes the next step and updates pact's state" $ do
        ks <- liftIO generateEd25519KeyPair
        modCmd <- liftIO $ mkCmd ks (threeStepPactCode "testCorrectNextStep")
        testCmd <- liftIO $ mkCmd ks "(testCorrectNextStep.tester)"
        contCmd <- liftIO $
                   mkCont (DefPactId $ hashToText (_cmdHash testCmd))
                     1 False PUnit (def :: PublicMeta) [(DynEd25519KeyPair ks, [])] [] (Just "nonce") Nothing Nothing
        let cmds = J.encode $ J.build $ SubmitBatch $ modCmd NE.:| [testCmd, contCmd]
        resp <- postWithHeaders "/api/v1/send" cmds [(HTTP.hContentType, "application/json; charset=utf-8")]
        assertStatus 200 resp

--        let (Just (SendResponse rks@(RequestKeys h))) :: Maybe SendResponse = A.decodeStrict $ LBS.toStrict reqResp

        -- check module deployment
        let req = J.encode $ J.build $ ListenRequest (cmdToRequestKey modCmd)
        SResponse _ _ reqResp <- postWithHeaders "/api/v1/listen" req  [(HTTP.hContentType, "application/json")]
        let (Just (ListenResponse cres)) :: Maybe ListenResponse = A.decodeStrict $ LBS.toStrict reqResp
        liftIO $ assertBool "Should return the correct content"
          $ isJust $ preview (crResult . _PactResultOk . _PString . prefixedBy "Loaded module testCorrectNextStep") cres


        -- check exeuction of first step
        let req' = J.encode $ J.build $ ListenRequest (cmdToRequestKey testCmd)
        SResponse _ _ reqResp' <- postWithHeaders "/api/v1/listen" req'  [(HTTP.hContentType, "application/json")]
        let (Just (ListenResponse cres')) :: Maybe ListenResponse = A.decodeStrict $ LBS.toStrict reqResp'
        liftIO $ assertEqual "Should match step 0" "step 0" (view (crResult . _PactResultOk . _PString) cres')
        -- check first cont msg
        let req'' = J.encode $ J.build $ ListenRequest (cmdToRequestKey contCmd)
        SResponse _ _ reqResp'' <- postWithHeaders "/api/v1/listen" req''  [(HTTP.hContentType, "application/json")]
        let (Just (ListenResponse cres'')) :: Maybe ListenResponse = A.decodeStrict $ LBS.toStrict reqResp''
        liftIO $ assertEqual "Should match step 1" "step 1" (view (crResult . _PactResultOk . _PString) cres'')

    ]

mkSubmitBatch :: IO LBS.ByteString
mkSubmitBatch = do
  ks <- generateEd25519KeyPair
  let rpc :: PactRPC Text = Exec (ExecMsg "(+ 1 2)" PUnit)
      metaData = J.build $ StableEncoding (def :: PublicMeta)
  cmd <- mkCommand [(ks, [])] [] metaData "nonce" Nothing rpc
  pure $ J.encode $ J.build $ SubmitBatch $ fmap decodeUtf8 cmd NE.:| []

mkLocalRequest :: IO LBS.ByteString
mkLocalRequest = do
  ks <- generateEd25519KeyPair
  let rpc :: PactRPC Text = Exec (ExecMsg "(+ 1 2)" PUnit)
      metaData = J.build $ StableEncoding (def :: PublicMeta)
  cmd <- mkCommand [(ks, [])] [] metaData "nonce" Nothing rpc
  pure $ J.encode $ J.build $ LocalRequest $ fmap decodeUtf8 cmd

mkCmd :: Ed25519KeyPair -> Text -> IO (Command Text)
mkCmd ks code = do
  let mguard = toB16Text $ getPublic ks
      md = PObject $ M.fromList [("admin-keyset", PList (V.fromList [PString mguard]))]
      rpc :: PactRPC Text = Exec (ExecMsg code md)
      metaData = J.build $ StableEncoding (def :: PublicMeta)
  cmd <- mkCommand [(ks, [])] [] metaData "nonce" Nothing rpc
  pure $ fmap decodeUtf8 cmd

threeStepPactCode :: T.Text -> T.Text
threeStepPactCode moduleName =
  [text|
    (define-keyset 'k (read-keyset "admin-keyset"))
      (module $moduleName 'k
        (defpact tester ()
          (step "step 0")
          (step "step 1")
          (step "step 2"))) |]

prefixedBy :: Text -> Prism' Text Text
prefixedBy prefix = prism' id (\s -> if prefix `T.isPrefixOf` s then Just s else Nothing)
