{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Pact.Core.Test.CommandTests
  ( tests
  ) where

import qualified Data.Aeson as A
import Data.List.Unsafe
import Data.Foldable (forM_)
import Data.ByteString
import Data.Text
import Test.Tasty
import Test.Tasty.HUnit

import Pact.Core.PactValue

import Pact.Core.Command.Client
import Pact.Core.Command.Crypto
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
        cmdsA <- mkCommandsWithBatchSignatures (webAuthnKeys, [])
          [([], metaData, "nonce-1", Nothing, mkRpc "(+ 1 1)")
          ,([], metaData, "nonce-2", Nothing, mkRpc "(+ 1 2)")
          ,([], metaData, "nonce-3", Nothing, mkRpc "(+ 1 3)")
          ,([], metaData, "nonce-4", Nothing, mkRpc "(+ 1 4)")
          ]

        -- Happy Path: All commands in the batch should verify.
        forM_ cmdsA $ \cmd -> case verifyCommand @(StableEncoding PactValue) cmd of
          ProcFail f -> assertFailure $ "Command should be valid: " <> show f
          ProcSucc _ -> pure ()

        cmdsB <- mkCommandsWithBatchSignatures (webAuthnKeys, [])
          [([], metaData, "nonce-5", Nothing, mkRpc "(+ 1 2)")]

        -- Sanity Check: Swapping an unrelated signature into a command should cause
        -- verification to fail.
        let (cmdA', _cmdB') = swapSignatures (unsafeHead cmdsA) (unsafeHead cmdsB)
        case verifyCommand @(StableEncoding PactValue) cmdA' of
          ProcSucc _ -> assertFailure $ "Command verification should fail"
          ProcFail _ -> pure ()

        -- Merkle Test: We will make a valid Merkle Root and Merkle Proof for a
        -- batch signature, but recombine parts of the Batch Tokens, so that
        -- the WebAuthn signature of the root is correct for the command, but
        -- the MerkleProof isn't valid.
        let (cmdA'', _cmdB'') = swapBatchTokenProofs (unsafeHead cmdsA) (unsafeHead cmdsB)
        case verifyCommand @(StableEncoding PactValue) cmdA'' of
          ProcSucc _ -> assertFailure $ "Command verification should fail"
          ProcFail _ -> pure ()
    ]

swapSignatures :: Command a -> Command b -> (Command a, Command b)
swapSignatures cmdA cmdB =
  (cmdA { _cmdSigs = _cmdSigs cmdB },
   cmdB { _cmdSigs = _cmdSigs cmdA }
  )

swapBatchTokenProofs :: Command a -> Command b -> (Command a, Command b)
swapBatchTokenProofs cmdA cmdB =
  case (_cmdSigs cmdA, _cmdSigs cmdB) of
    ([WebAuthnBatchToken tokenA], [WebAuthnBatchToken tokenB]) ->
      let (sigA', sigB') = swapProofs tokenA tokenB
      in (cmdA { _cmdSigs = [WebAuthnBatchToken sigA'] },
          cmdB { _cmdSigs = [WebAuthnBatchToken sigB'] }
         )
    _ -> error "swapBatchTokenProofs is only meant to be called for commands with a single WebAuthnBatchToken signature"
  where
    swapProofs :: BatchToken  -> BatchToken -> (BatchToken, BatchToken)
    swapProofs tokenA tokenB =
      (tokenA { _btMerkleProofObject = _btMerkleProofObject tokenB },
       tokenB { _btMerkleProofObject = _btMerkleProofObject tokenA }
      )
