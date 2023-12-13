-- |

module Pact.Core.Test.PersistenceTests where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Hedgehog (Gen, Property, (===), forAll, property)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog
import Test.Tasty.HUnit (assertEqual, testCase)
import qualified Hedgehog.Gen as Gen

import Pact.Core.Names (Field(..), FullyQualifiedName, RowKey(..), TableName(..), ModuleName(..), NamespaceName(..))
import Pact.Core.Guards (KeySet(KeySet), KeySetName(..), PublicKeyText(..), KSPredicate(KeysAll))
import Pact.Core.Gen.Serialise (keySetGen, keySetNameGen, moduleNameGen, moduleDataGen, builtinGen
                               ,defPactIdGen, defPactExecGen, namespaceNameGen, namespaceGen)
import Pact.Core.Serialise (PactSerialise, serialisePact)
import qualified Pact.Core.PactValue as PactValue
import Pact.Core.Persistence.SQLite
import Pact.Core.Persistence (WriteType(Insert), readKeySet, writeKeySet, writeModule, readModule
                             ,writeDefPacts, readDefPacts, readNamespace, writeNamespace
                             , Domain(..), PactDb(_pdbKeys, _pdbTxIds, _pdbCreateUserTable, _pdbRead, _pdbWrite, _pdbBeginTx, _pdbCommitTx), TxId(..)
                             , ExecutionMode(Transactional)
                             , TxLog(TxLog, _txDomain, _txKey, _txValue)
                             , RowData(..)
                             , WriteType(Insert, Update, Write)
                             )
import Data.Foldable (forM_)
import qualified Data.Text as T

testsWithSerial :: (Show b, Show i, Eq b, Eq i) => PactSerialise b i -> Gen b -> Gen i -> [TestTree]
testsWithSerial serial b i =
 [ testProperty "KeySet" $ keysetPersistRoundtrip serial (keySetGen undefined)
 , testProperty "ModuleData" $ moduleDataRoundtrip serial b i
 , testProperty "DefPactExec" $ defPactExecRoundtrip serial b i
 , testProperty "Namespace" $ namespaceRoundtrip serial
 ]

tests :: TestTree
tests = testGroup "Persistence Roundtrip"
  [ testGroup "CBOR encoding/decoding" $ testsWithSerial serialisePact builtinGen (pure ())
  ]

readExistingDb :: FilePath -> IO ()
readExistingDb fp = withSqlitePactDb serialisePact (T.pack fp) $ \pdb -> do

  txIds <- _pdbTxIds pdb (TableName "token-table" (ModuleName "yeettoken" (Just (NamespaceName "free")))) (TxId 0)
  print txIds

  -- keys <- _pdbKeys pdb DKeySets
  -- forM_ keys $ \k -> do
  --   print k
  --   Just _ <- readKeySet pdb k
  --   pure ()
  
  -- TODO: fails
  -- keys' <- _pdbKeys pdb DNamespaces
  -- forM_ keys' $ \k -> do
  --   Just n <- readNamespace pdb k
  --   print n
  --   pure ()
 
  -- keys' <- _pdbKeys pdb DDefPacts
  -- forM_ keys' $ \k -> do
  --   print k
  --   Just _n <- readDefPacts pdb k
  --   pure ()

  -- TODO: fails
  -- keys <- _pdbKeys pdb DModules
  -- forM_ keys $ \mn -> do
  --   print mn
  --   Just _ <- readModule pdb mn
  --   pure ()


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

defPactExecRoundtrip :: PactSerialise b i -> Gen b -> Gen i -> Property
defPactExecRoundtrip serial _b _i = property $ do
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

sqliteRegression :: TestTree
sqliteRegression =
  testCase "sqlite persistence backend produces expected values/txlogs" $
  withSqlitePactDb serialisePact "tmp.sqlite" $ \pdb -> do
    let
      user1 = "user1"
      usert = TableName user1 (ModuleName "someModule" Nothing)
    txId1 <- _pdbBeginTx pdb Transactional
    _pdbCreateUserTable pdb usert

    txs1 <- _pdbCommitTx pdb
    assertEqual "output of commit" txs1 [ TxLog "SYS:usertables" "user1" "TODO" ]

    _ <- _pdbBeginTx pdb Transactional
    let row = RowData $ Map.fromList [(Field "gah", PactValue.PDecimal 123.454345)]
    _pdbWrite pdb Insert (DUserTables usert) (RowKey "key1") row
    Just row' <- _pdbRead pdb (DUserTables usert) (RowKey "key1")
    assertEqual "row should be identical to its saved/recalled value" row row'

    let row2 = RowData $ Map.fromList
                 [(Field "gah", PactValue.PBool False)
                 ,(Field "fh", PactValue.PInteger 1)
                 ]
    _pdbWrite pdb Update (DUserTables usert) (RowKey "key1") row2
    Just row2' <- _pdbRead pdb (DUserTables usert) (RowKey "key1")
    assertEqual "user update should overwrite with new value" row2 row2'

    let ks = KeySet (Set.fromList [PublicKeyText "skdjhfskj"]) KeysAll
    _ <- _pdbWrite pdb Write DKeySets (KeySetName "ks1" Nothing) ks
    Just ks' <- _pdbRead pdb DKeySets (KeySetName "ks1" Nothing)
    assertEqual "keyset should be equal after storage/retrieval" ks ks'


    return ()
