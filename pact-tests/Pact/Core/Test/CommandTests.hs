{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Pact.Core.Test.CommandTests
  ( tests
  ) where

import qualified Data.Aeson as A
import Data.ByteString
import Data.Text
import Test.Tasty
import Test.Tasty.HUnit

import Pact.JSON.Legacy.Value

import Pact.Core.Command.Client
import Pact.Core.Command.Crypto (generateEd25519KeyPair)
import Pact.Core.Command.RPC
import Pact.Core.Command.Types

exampleCommand :: IO (Command ByteString)
exampleCommand = do
  testKeyPair <- generateEd25519KeyPair
  let rpc :: PactRPC Text = Exec $ ExecMsg { _pmCode = "(+ 1 2)", _pmData = LegacyValue A.Null}
  let metaData = A.Number 1 :: A.Value
  mkCommand [(testKeyPair, [])] [] metaData "nonce" Nothing rpc

tests :: IO TestTree
tests = do
  pure $ testGroup "CommandTests"
    [ testCase "verifyCommand" $ do
      cmd <- exampleCommand
      let cmdResult = verifyCommand @Int cmd
      case cmdResult of
        ProcFail f -> do
          print f
          assertFailure "Command should be valid"
        ProcSucc _ -> 
          assertBool "Command should be valid" True
    ]
