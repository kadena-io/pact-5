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
import Pact.Core.Serialise (PactSerialise(..), serialisePact)
import qualified Pact.Core.PactValue as PactValue
import Pact.Core.Persistence.SQLite
import Pact.Core.Persistence (WriteType(Insert), readKeySet, writeKeySet, writeModule, readModule
                             ,writeDefPacts, readDefPacts, readNamespace, writeNamespace
                             , Domain(..), PactDb(..), TxId(..)
                             , ExecutionMode(Transactional)
                             , TxLog(..)
                             , RowData(..)
                             , WriteType(Insert, Update, Write)
                             )
import qualified Data.Text as T
import Pact.Core.Repl.Compile
import Pact.Core.Repl.Utils
import Pact.Core.Environment
import Pact.Core.Persistence.MockPersistence
import Data.Default
import Data.IORef
import Pact.Core.Builtin


testsWithSerial :: (Show b, Show i, Eq b, Eq i) => PactSerialise b i -> Gen b -> Gen i -> [TestTree]
testsWithSerial serial b i =
 [ testProperty "KeySet" $ keysetPersistRoundtrip serial (keySetGen undefined)
 , testProperty "ModuleData" $ moduleDataRoundtrip serial b i
 , testProperty "DefPactExec" $ defPactExecRoundtrip serial b i
 , testProperty "Namespace" $ namespaceRoundtrip serial
 ]

tests :: TestTree
tests = testGroup "Persistence"
  [ testGroup "CBOR encoding/decoding roundtrip" $ testsWithSerial serialisePact builtinGen (pure ())
  , sqliteRegression
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
  withSqlitePactDb serialiseRepl ":memory:" $ \pdb -> do
    let
      user1 = "user1"
      usert = TableName user1 (ModuleName "someModule" Nothing)
    _txId1 <- _pdbBeginTx pdb Transactional
    _pdbCreateUserTable pdb usert

    txs1 <- _pdbCommitTx pdb
    -- TODO: https://github.com/kadena-io/chainweb-node/blob/28764eb2dae323608ee2d4f17e984948455f04a1/test/Chainweb/Test/Pact/Checkpointer.hs#L492C13-L497C17
    assertEqual "output of commit" txs1 [ TxLog "SYS:usertables" "user1" mempty ]


    --  Begin tx
    Just t1 <- _pdbBeginTx pdb Transactional
    let
      row = RowData $ Map.fromList [(Field "gah", PactValue.PDecimal 123.454345)]
      rowEnc = _encodeRowData serialiseRepl row
    _pdbWrite pdb Insert (DUserTables usert) (RowKey "key1") row
    Just row' <- _pdbRead pdb (DUserTables usert) (RowKey "key1")
    assertEqual "row should be identical to its saved/recalled value" row row'

    let
      row2 = RowData $ Map.fromList
             [ (Field "gah", PactValue.PBool False)
             , (Field "fh", PactValue.PInteger 1)
             ]
      row2Enc = _encodeRowData serialiseRepl row2
                 
    _pdbWrite pdb Update (DUserTables usert) (RowKey "key1") row2
    Just row2' <- _pdbRead pdb (DUserTables usert) (RowKey "key1")
    assertEqual "user update should overwrite with new value" row2 row2'

    let
      ks = KeySet (Set.fromList [PublicKeyText "skdjhfskj"]) KeysAll
      ksEnc = _encodeKeySet serialiseRepl ks
    _ <- _pdbWrite pdb Write DKeySets (KeySetName "ks1" Nothing) ks
    Just ks' <- _pdbRead pdb DKeySets (KeySetName "ks1" Nothing)
    assertEqual "keyset should be equal after storage/retrieval" ks ks'


    -- module
    let mn = ModuleName "test" Nothing
    md <- loadModule
    let mdEnc = _encodeModuleData serialiseRepl md
    _pdbWrite pdb Write DModules mn md

    Just md' <- _pdbRead pdb DModules mn
    assertEqual "module should be identical to its saved/recalled value" md md'

    txs2 <- _pdbCommitTx pdb
    assertEqual "output of commit" txs2
      [ TxLog "SYS:Modules" "test" mdEnc
      , TxLog "SYS:KeySets" "ks1" ksEnc
      , TxLog "user1" "key1" row2Enc
      , TxLog "user1" "key1" rowEnc
      ]

  -- begin tx
    _ <- _pdbBeginTx pdb Transactional
    tids <- _pdbTxIds pdb usert t1
    assertEqual "user txids" [TxId 1] tids

    txlog <- _pdbGetTxLog pdb usert (head tids)
    assertEqual "user txlog" txlog
      [ TxLog "user1" "key1" (RowData $ Map.union (_unRowData row2) (_unRowData row))
      ]

    _pdbWrite pdb Insert (DUserTables usert) (RowKey "key2") row
    Just r1 <- _pdbRead pdb (DUserTables usert) (RowKey "key2")
    assertEqual "user insert key2 pre-rollback" row r1

    rkeys <- _pdbKeys pdb (DUserTables usert)
    assertEqual "keys pre-rollback [key1, key2]" [RowKey "key1", RowKey "key2"] rkeys

    _pdbRollbackTx pdb
    r2 <- _pdbRead pdb (DUserTables usert) (RowKey "key2")
    assertEqual "rollback erases key2" Nothing r2

    rkeys2 <- _pdbKeys pdb (DUserTables usert)
    assertEqual "keys post-rollback [key1]" [RowKey "key1"] rkeys2
    
    where
      loadModule = do
        let src = "(module test G (defcap G () true) (defun f (a: integer) 1))"
        pdb <- mockPactDb serialiseRepl
        g <- newIORef mempty
        evalLog <- newIORef Nothing
        let ee = defaultEvalEnv pdb replRawBuiltinMap
        ref <- newIORef (ReplState mempty pdb def ee g evalLog (SourceCode "" "") Nothing)
        Right _ <- runReplT ref (interpretReplProgram (SourceCode "test" src) (const (pure ())))
        Just md <- readModule pdb (ModuleName "test" Nothing)
        pure md
        
