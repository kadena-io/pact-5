-- | Tests of the SQLite persistence backend.

module Pact.Core.Test.PersistenceTests where

import Control.Monad.IO.Class (liftIO)
import Data.Default (def)
import Data.IORef (newIORef)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Hedgehog (Gen, Property, (===), forAll, property)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog
import Test.Tasty.HUnit (assertEqual, testCase)
import qualified Hedgehog.Gen as Gen

import Pact.Core.Builtin (replcoreBuiltinMap)
import Pact.Core.Environment (defaultEvalEnv)
import Pact.Core.Guards (KeySet(KeySet), KeySetName(..), PublicKeyText(..), KSPredicate(KeysAll))
import Pact.Core.Gen.Serialise (keySetGen, keySetNameGen, moduleNameGen, moduleDataGen, builtinGen
                               ,defPactIdGen, defPactExecGen, namespaceNameGen, namespaceGen)
import Pact.Core.Literal (Literal(LUnit))
import Pact.Core.Names
import Pact.Core.PactValue
import qualified Pact.Core.PactValue as PactValue
import Pact.Core.Persistence.SQLite
import Pact.Core.Persistence
import Pact.Core.Persistence.MockPersistence (mockPactDb)
import Pact.Core.Repl.Compile (interpretReplProgram)
import Pact.Core.Repl.Utils (ReplState(..), SourceCode(..), runReplT)
import Pact.Core.Serialise (PactSerialise(..), serialisePact, serialisePact_repl_spaninfo)

-- | Top-level TestTree for Persistence Tests.
tests :: TestTree
tests = testGroup "Persistence"
  [ testGroup "CBOR encoding/decoding roundtrip" $
      testsWithSerial serialisePact builtinGen (pure ())
  , sqliteRegression
  ]

-- | Generate the test tree for any given `PactSerialise` serialization scheme,
-- given also a means of creating builtins and infos for that scheme.
testsWithSerial :: (Show b, Show i, Eq b, Eq i)
  => PactSerialise b i
  -> Gen b
  -> Gen i
  -> [TestTree]
testsWithSerial serial b i =
 [ testProperty "KeySet" $ keysetPersistRoundtrip serial (keySetGen undefined)
   -- ^ keySetGen does not use its first argument now. We will pass a real argument
   --   once custom keyset predicate functions are supported.
 , testProperty "ModuleData" $ moduleDataRoundtrip serial b i
 , testProperty "DefPactExec" $ defPactExecRoundtrip serial b i
 , testProperty "Namespace" $ namespaceRoundtrip serial
 ]

keysetPersistRoundtrip :: PactSerialise b i -> Gen KeySet -> Property
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

-- | This regression test carries out a number of core operations on the
--   PactDb, including reading and writing Namespaces, KeySets, Modules, and
--   user data, within transactions. It ensures that TxLogs for transactions
--   contain the expected metadata.
sqliteRegression :: TestTree
sqliteRegression =
  testCase "sqlite persistence backend produces expected values/txlogs" $
  withSqlitePactDb serialisePact_repl_spaninfo ":memory:" $ \pdb -> do
    let
      user1 = "user1"
      usert = TableName user1 (ModuleName "someModule" Nothing)
    _txId1 <- _pdbBeginTx pdb Transactional
    _pdbCreateUserTable pdb usert

    txs1 <- _pdbCommitTx pdb
    let
      rd = RowData $ Map.singleton (Field "utModule")
        (PObject $ Map.fromList
          [ (Field "namespace", PLiteral LUnit)
          , (Field "name", PString user1)
          ])
      rdEnc = _encodeRowData serialisePact_repl_spaninfo rd
    assertEqual "output of commit" txs1 [ TxLog "SYS:usertables" "user1" rdEnc ]


    --  Begin tx
    Just t1 <- _pdbBeginTx pdb Transactional
    let
      row = RowData $ Map.fromList [(Field "gah", PactValue.PDecimal 123.454345)]
      rowEnc = _encodeRowData serialisePact_repl_spaninfo row
    _pdbWrite pdb Insert (DUserTables usert) (RowKey "key1") row
    Just row' <- _pdbRead pdb (DUserTables usert) (RowKey "key1")
    assertEqual "row should be identical to its saved/recalled value" row row'

    let
      row2 = RowData $ Map.fromList
             [ (Field "gah", PactValue.PBool False)
             , (Field "fh", PactValue.PInteger 1)
             ]
      row2Enc = _encodeRowData serialisePact_repl_spaninfo row2

    _pdbWrite pdb Update (DUserTables usert) (RowKey "key1") row2
    Just row2' <- _pdbRead pdb (DUserTables usert) (RowKey "key1")
    assertEqual "user update should overwrite with new value" row2 row2'

    let
      ks = KeySet (Set.fromList [PublicKeyText "skdjhfskj"]) KeysAll
      ksEnc = _encodeKeySet serialisePact_repl_spaninfo ks
    _ <- _pdbWrite pdb Write DKeySets (KeySetName "ks1" Nothing) ks
    Just ks' <- _pdbRead pdb DKeySets (KeySetName "ks1" Nothing)
    assertEqual "keyset should be equal after storage/retrieval" ks ks'


    -- module
    let mn = ModuleName "test" Nothing
    md <- loadModule
    let mdEnc = _encodeModuleData serialisePact_repl_spaninfo md
    _pdbWrite pdb Write DModules mn md

    Just md' <- _pdbRead pdb DModules mn
    assertEqual "module should be identical to its saved/recalled value" md md'

    txs2 <- _pdbCommitTx pdb
    assertEqual "output of commit" txs2
      [ TxLog "SYS:MODULES" "test" mdEnc
      , TxLog "SYS:KEYSETS" "ks1" ksEnc
      , TxLog "user1" "key1" row2Enc
      , TxLog "user1" "key1" rowEnc
      ]

    -- begin tx
    _ <- _pdbBeginTx pdb Transactional
    tids <- _pdbTxIds pdb usert t1
    assertEqual "user txids" [TxId 1] tids

    txlog <- _pdbGetTxLog pdb usert (head tids)
    assertEqual "user txlog" txlog
      [ TxLog "USER_someModule_user1" "key1" (RowData $ Map.union (_unRowData row2) (_unRowData row))
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
        pdb <- mockPactDb serialisePact_repl_spaninfo
        g <- newIORef mempty
        evalLog <- newIORef Nothing
        ee <- defaultEvalEnv pdb replcoreBuiltinMap
        ref <- newIORef (ReplState mempty pdb def ee g evalLog (SourceCode "" "") mempty mempty Nothing)
        Right _ <- runReplT ref (interpretReplProgram (SourceCode "test" src) (const (pure ())))
        Just md <- readModule pdb (ModuleName "test" Nothing)
        pure md

