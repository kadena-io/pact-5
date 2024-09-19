{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

module Pact.Core.Test.JSONRoundtripTests(tests) where

import qualified Pact.JSON.Encode as J

import Pact.Core.StableEncoding
import Pact.Core.Gen

import Data.Typeable
import Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog
import qualified Data.Aeson as JD

data StableEncodingCase
  = forall a. (JD.FromJSON (StableEncoding a), J.Encode (StableEncoding a), Typeable a, Eq a, Show a) => StableEncodingCase (Gen a)

data EncodingCase
  = forall a. (JD.FromJSON a, J.Encode a, Typeable a, Eq a, Show a) => EncodingCase (Gen a)


testJSONRoundtrip :: EncodingCase -> TestTree
testJSONRoundtrip (EncodingCase gen) =
  testProperty testName $ property $ do
    v <- forAll gen
    JD.decodeStrict (J.encodeStrict v) === Just v
  where
  testName = "JSON roundtrips for: " <> show (typeNameOfGen gen)

typeNameOfGen :: Typeable a => Gen a -> String
typeNameOfGen a = show (typeRep (proxyFromGen a))
  where
  proxyFromGen :: Gen a -> Proxy a
  proxyFromGen _ = Proxy

testStableEncodingRoundtrip :: StableEncodingCase -> TestTree
testStableEncodingRoundtrip (StableEncodingCase gen) = do
  let testName = "JSON roundtrips for StableEncoding: " <> show (typeNameOfGen gen)
  testProperty testName $ property $ do
    v <- forAll gen
    JD.decodeStrict (J.encodeStrict (StableEncoding v)) === Just (StableEncoding v)


tests :: TestTree
tests = testGroup "JSON Roundtrips" $ stableEncodings ++ jsonRoundtrips
  where
  stableEncodings = fmap testStableEncodingRoundtrip $
    [ StableEncodingCase namespaceNameGen
    , StableEncodingCase moduleNameGen
    , StableEncodingCase qualifiedNameGen
    , StableEncodingCase pactValueGen
    , StableEncodingCase (guardGen qualifiedNameGen)
    , StableEncodingCase keySetNameGen
    , StableEncodingCase keySetGen
    , StableEncodingCase fullyQualifiedNameGen
    , StableEncodingCase (capTokenGen qualifiedNameGen pactValueGen)
    , StableEncodingCase publicMetaGen
    , StableEncodingCase publicDataGen
    , StableEncodingCase rowDataGen
    , StableEncodingCase defPactExecGen
    , StableEncodingCase namespaceGen
    , StableEncodingCase pactEventGen
    , StableEncodingCase spanInfoGen
    ]
  jsonRoundtrips = fmap testJSONRoundtrip $
    [ EncodingCase signerGen
    , EncodingCase lineInfoGen
    ]
