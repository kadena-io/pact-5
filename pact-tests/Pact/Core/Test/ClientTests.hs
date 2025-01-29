{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Pact.Core.Test.ClientTests where

import Data.Default (def)
import Test.Tasty
import Test.Tasty.HUnit

import Data.Text (Text)
import Servant.Client
import Pact.Core.PactValue
import Pact.Core.Command.Crypto (genKeyPair)
import Pact.Core.Command.Types
import Pact.Core.Command.Server
import Pact.Core.Command.Client

import Pact.Core.Test.ServerUtils
import Pact.Core.Errors

simpleServerCmd :: IO (Command Text)
simpleServerCmd = do
  simpleKeys <- DynEd25519KeyPair <$> genKeyPair
  mkExec  "(+ 1 2)" PUnit def [(simpleKeys,[])] [] Nothing (Just "test1")


simpleServerCmdWithPactErr :: IO (Command Text)
simpleServerCmdWithPactErr = do
  simpleKeys <- DynEd25519KeyPair <$> genKeyPair
  mkExec  "(+ 1 2 3)" PUnit def [(simpleKeys,[])] [] Nothing (Just "test1")

tests :: TestTree
tests = testGroup "Servant API client tests" [
    testCase "correctly runs a simple command locally" $ do
      cmd <- simpleServerCmd
      res <- withTestPactServer "clientspec" $ \clientEnv -> do
        runClientM (localClient (LocalRequest cmd)) clientEnv
      let cmdPactResult = (PactResultOk . PInteger) 3
      (_crResult . _localResponse  <$> res) `shouldBe` Right cmdPactResult

    , testCase "correctly runs a simple command with pact error locally" $ do
      cmd <- simpleServerCmdWithPactErr
      res <- withTestPactServer "clientspec" $ \clientEnv -> do
        runClientM (localClient (LocalRequest cmd)) clientEnv
      (_crResult . _localResponse <$> res) `shouldSatisfy` failWith (ErrorType "ExecutionError")

    , testCase "correctly runs a simple command publicly and listens to the result" $ do
      cmd <- simpleServerCmd
      let rk = cmdToRequestKey cmd
      (res,res') <- withTestPactServer "clientspec" $ \clientEnv -> do
        !res <- runClientM (sendClient (SendRequest (SubmitBatch [cmd]))) clientEnv
        !res' <- runClientM (listenClient (ListenRequest rk)) clientEnv
        return (res,res')
      res `shouldBe` Right (SendResponse (RequestKeys [rk]))
      let cmdData = (PactResultOk . PInteger) 3
      case res' of
        Left _ -> assertFailure "client request failed"
        Right r -> case r of
          ListenResponse lr -> _crResult lr `shouldBe` cmdData

    , testCase "correctly runs a simple command with pact error publicly and listens to the result" $ do
      cmd <- simpleServerCmdWithPactErr
      let rk = cmdToRequestKey cmd
      (res,res') <- withTestPactServer "clientspec" $ \clientEnv -> do
        !res <- runClientM (sendClient (SendRequest (SubmitBatch [cmd]))) clientEnv
        !res' <- runClientM (listenClient (ListenRequest rk)) clientEnv
        return (res,res')
      res `shouldBe` Right (SendResponse (RequestKeys [rk]))
      case res' of
        Left _ -> assertFailure "client request failed"
        Right r -> case r of
          -- ListenTimeout _ -> assertFailure "timeout"
          ListenResponse lr -> Right (_crResult lr) `shouldSatisfy` failWith (ErrorType "ExecutionError")
  ]

failWith :: ErrorType -> Either ClientError (PactResult (PactOnChainError)) -> Bool
failWith errType res = case res of
  Left _ -> False
  Right r -> case r of
    PactResultOk _ -> False
    PactResultErr pe -> _peType pe == errType

