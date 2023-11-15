-- |

module Pact.Core.Test.PersistenceTests where

import Control.Monad.IO.Class (liftIO)
import Hedgehog (Gen, Property, (===), forAll, property)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog
import qualified Hedgehog.Gen as Gen

import Pact.Core.Names (FullyQualifiedName)
import Pact.Core.Info (SpanInfo)
import Pact.Core.Guards (KeySet)
import Pact.Core.Gen.Serialise (keySetGen, keySetNameGen, moduleNameGen, moduleDataGen, builtinGen, infoGen
                               ,defPactIdGen, defPactExecGen, namespaceNameGen, namespaceGen)
import Pact.Core.Serialise (PactSerialise, serialiseCBOR)
import Pact.Core.Builtin (RawBuiltin)
import Pact.Core.Persistence.SQLite
import Pact.Core.Persistence (WriteType(Insert), readKeySet, writeKeySet, writeModule, readModule
                             ,writeDefPacts, readDefPacts, readNamespace, writeNamespace)

testsWithSerial :: (Show b, Show i, Eq b, Eq i) => PactSerialise b i -> Gen b -> Gen i -> [TestTree]
testsWithSerial serial b i =
 [ testProperty "KeySet" $ keysetPersistRoundtrip serial (keySetGen undefined)
 , testProperty "ModuleData" $ moduleDataRoundtrip serial b i
 , testProperty "DefPactExec" $ defPactExecRoundtrip serial b i
 , testProperty "Namespace" $ namespaceRoundtrip serial
 ]

tests :: TestTree
tests = testGroup "Persistence Roundtrip"
  [ testGroup "CBOR encoding/decoding" $ testsWithSerial serialiseCBOR builtinGen (pure ())
  ]

keysetPersistRoundtrip :: PactSerialise b i -> Gen (KeySet FullyQualifiedName) -> Property
keysetPersistRoundtrip serial keysetGen =
  property $ do
    keysetName <- forAll keySetNameGen
    keyset <- forAll keysetGen
    writtenKeySet <-  liftIO $ withSqlitePactDb serial ":memory:" $ \db -> do
      () <- writeKeySet db Insert keysetName keyset
      readKeySet db keysetName
    Just keyset === writtenKeySet

moduleDataRoundtrip :: (Show b,Show i, Eq b, Eq i) => PactSerialise b i -> Gen b -> Gen i -> Property
moduleDataRoundtrip serial b i = property $ do
  moduleData <- forAll (moduleDataGen b i)
  moduleName <- forAll moduleNameGen
  writtenModuleData <- liftIO $ withSqlitePactDb serial ":memory:" $ \db -> do
    () <- writeModule db Insert moduleName moduleData
    readModule db moduleName
  Just moduleData === writtenModuleData

defPactExecRoundtrip :: (Show b,Show i, Eq b, Eq i) => PactSerialise b i -> Gen b -> Gen i -> Property
defPactExecRoundtrip serial b i = property $ do
  defPactId <- forAll defPactIdGen
  defPactExec <- forAll (Gen.maybe defPactExecGen)
  writtenDefPactExec <- liftIO $ withSqlitePactDb serial ":memory:" $ \db -> do
    () <- writeDefPacts db Insert defPactId defPactExec
    readDefPacts db defPactId
  Just defPactExec === writtenDefPactExec

namespaceRoundtrip :: PactSerialise b i -> Property
namespaceRoundtrip serial = property $ do
  ns <- forAll namespaceNameGen
  namespace <- forAll namespaceGen
  writtenNamespace <- liftIO $ withSqlitePactDb serial ":memory:" $ \db -> do
    () <- writeNamespace db Insert ns namespace
    readNamespace db ns
  Just namespace === writtenNamespace
