{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Pact.Core.Test.CommandTests
  ( tests
  ) where

import qualified Data.Aeson as A
import Data.Foldable (forM_)
import Data.ByteString
import Data.Text
import Test.Tasty
import Test.Tasty.HUnit

import Pact.Core.PactValue

import Pact.Core.Command.Client
import Pact.Core.Command.Crypto (generateEd25519KeyPair, generateWebAuthnEd25519KeyPair)
import Pact.Core.Command.RPC
import Pact.Core.Command.Types
import Pact.Core.StableEncoding

exampleCommand :: IO (Command ByteString)
exampleCommand = do
  testKeyPair <- generateEd25519KeyPair
  let rpc :: PactRPC Text = Exec $ ExecMsg { _pmCode = "(+ 1 2)", _pmData = PUnit}
  let metaData = A.Number 1 :: A.Value
  mkCommand [(testKeyPair, [])] [] metaData "nonce" Nothing rpc

tests :: IO TestTree
tests = do
  pure $ testGroup "CommandTests"
    [ testCase "verifyCommand" $ do
      cmd <- exampleCommand
      let cmdResult = verifyCommand @(StableEncoding PactValue) cmd
      case cmdResult of
        ProcFail f -> do
          print f
          assertFailure "Command should be valid"
        ProcSucc _ ->
          assertBool "Command should be valid" True

    , testCase "verifyBatch" $ do
        webAuthnKeys <- generateWebAuthnEd25519KeyPair
        let
          metaData = StableEncoding (PUnit)
          mkRpc :: Text -> PactRPC Text
          mkRpc pactCode = Exec $ ExecMsg { _pmCode = pactCode, _pmData = PUnit }
        cmds <- mkCommandsWithBatchSignatures (webAuthnKeys, [])
          [([], metaData, "nonce-1", Nothing, mkRpc "(+ 1 1)")
          ,([], metaData, "nonce-2", Nothing, mkRpc "(+ 1 2)")
          ,([], metaData, "nonce-3", Nothing, mkRpc "(+ 1 3)")
          ,([], metaData, "nonce-4", Nothing, mkRpc "(+ 1 4)")
          ]
        forM_ cmds $ \cmd -> case verifyCommand @(StableEncoding PactValue) cmd of
          ProcFail f -> assertFailure $ "Command should be valid: " <> show f
          ProcSucc _ -> pure ()

    ]
