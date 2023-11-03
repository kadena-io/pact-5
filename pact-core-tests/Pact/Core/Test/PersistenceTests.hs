-- |
{-# LANGUAGE TypeApplications #-}

module Pact.Core.Test.PersistenceTests where

import Control.Monad.IO.Class (liftIO)
import Hedgehog (Gen, Property, (===), forAll, property)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog

import Pact.Core.Names (FullyQualifiedName)
import Pact.Core.Info (SpanInfo)
import Pact.Core.Guards (KeySet)
import Pact.Core.Gen.Serialise (keySetGen, keySetNameGen)
import Pact.Core.Serialise (serialiseCBOR)
import Pact.Core.Builtin (RawBuiltin)
import Pact.Core.Persistence.SQLite
import Pact.Core.Persistence (WriteType(Insert), readKeySet, writeKeySet)

tests :: TestTree
tests = testGroup "Persistence Roundtrip"
 [ testProperty "KeySet" $ keysetPersistRoundtrip (keySetGen undefined) ]

-- TODO: Choose a different type parameter for KeySet when Custom predicates
-- are reintroduced.
keysetPersistRoundtrip :: Gen (KeySet FullyQualifiedName) -> Property
keysetPersistRoundtrip keysetGen =
  property $ do
    keysetName <- forAll keySetNameGen
    keyset <- forAll keysetGen
    writtenKeySet <-  liftIO $ withSqlitePactDb (serialiseCBOR @RawBuiltin @SpanInfo) ":memory:" $ \db -> do
      () <- writeKeySet db Insert keysetName keyset
      readKeySet db keysetName
    Just keyset === writtenKeySet
