-- | 
{-# LANGUAGE OverloadedStrings #-}

module Pact.Core.Test.LegacySerialiseTests where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import Data.Maybe
import Pact.Core.Serialise.LegacyPact
import Pact.Core.Guards

tests :: TestTree
tests = testGroup "Legacy Serialisation"
  [ testGroup "KeySet"
    [ testCase "pred: keys-2"   $ assertBool "KeySet decoding failed" (isJust (decodeKeySet "{\"pred\":\"keys-2\",\"keys\":[\"ddd8\",\"ed0\"]}"))
    , testCase "pred: keys-all" $ assertBool "KeySet decoding failed" (isJust (decodeKeySet "{\"pred\":\"keys-all\",\"keys\":[\"ddd8\",\"ed0\"]}"))
    , testCase "pred: keys-any" $ assertBool "KeySet decoding failed" (isJust (decodeKeySet "{\"pred\":\"keys-any\",\"keys\":[\"ddd8\",\"ed0\"]}"))
    , testCase "pred defaults"  $ assertBool "KeySet decoding failed" (maybe False (\k -> KeysAll == _ksPredFun k) (decodeKeySet "{\"pred\":\"keys-all\",\"keys\":[\"ddd8\",\"ed0\"]}"))
    , testCase "pred invalid" $ assertBool "Accept invalid pred" (isNothing (decodeKeySet "{\"pred\":\"ABC\",\"keys\":[\"ddd8\",\"ed0\"]}"))
    ]
  ]
