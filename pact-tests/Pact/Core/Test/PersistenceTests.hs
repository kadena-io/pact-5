-- | Tests of the SQLite persistence backend.

module Pact.Core.Test.PersistenceTests where

import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Data.Default (Default, def)
import Data.Text (Text)
import qualified Data.Map as Map
import Hedgehog (Gen, Property, (===), forAll, property)
import Test.Tasty
import Test.Tasty.Hedgehog
import qualified Hedgehog.Gen as Gen

import Pact.Core.Builtin
import Pact.Core.Environment
import Pact.Core.Guards
import Pact.Core.Gen
import Pact.Core.Persistence.SQLite
import Pact.Core.Persistence
import Pact.Core.Persistence.MockPersistence
import Pact.Core.Serialise
import Pact.Core.PactDbRegression
import Test.Tasty.HUnit (testCase)

-- | Top-level TestTree for Persistence Tests.
tests :: TestTree
tests = testGroup "Persistence"
  [ testGroup "CBOR encoding/decoding roundtrip" $
      testsWithSerial serialisePact coreBuiltinMap builtinGen (pure ())
  , sqliteRegression
  , pureDbRegression
  ]

getEvalEnv :: PactSerialise b i -> Map.Map Text b -> IO (EvalEnv b i)
getEvalEnv serial builtins = do
  pdb <- mockPactDb serial
  defaultEvalEnv pdb builtins

-- | Generate the test tree for any given `PactSerialise` serialization scheme,
-- given also a means of creating builtins and infos for that scheme.
testsWithSerial :: (Show i, Eq b, Eq i, Default i, IsBuiltin b)
  => PactSerialise b i
  -> Map.Map Text b
  -> Gen b
  -> Gen i
  -> [TestTree]
testsWithSerial serial builtins b i =
 [ testProperty "KeySet" $ keysetPersistRoundtrip serial builtins keySetGen
   -- ^ keySetGen does not use its first argument now. We will pass a real argument
   --   once custom keyset predicate functions are supported.
 , testProperty "ModuleData" $ moduleDataRoundtrip serial builtins b i
 , testProperty "DefPactExec" $ defPactExecRoundtrip serial builtins b i
 , testProperty "Namespace" $ namespaceRoundtrip serial builtins
 ]

keysetPersistRoundtrip :: (Default i) => PactSerialise b i -> Map.Map Text b -> Gen KeySet -> Property
keysetPersistRoundtrip serial builtins keysetGen =
  property $ do
    keysetName <- forAll keySetNameGen
    keyset <- forAll keysetGen
    evalEnv <- liftIO $ getEvalEnv serial builtins
    writtenKeySetResult <- liftIO $ runEvalM (ExecEnv evalEnv) def  $ withSqlitePactDb serial ":memory:" $ \db -> do
      evalWrite def db Insert DKeySets keysetName keyset
      liftGasM def $ _pdbRead db DKeySets keysetName
    case writtenKeySetResult of
      (Left _, _) -> fail "Unexpected EvalM error"
      (Right writtenKeySet, _) -> Just keyset === writtenKeySet

moduleDataRoundtrip :: (Show i, Eq b, Eq i, Default i, IsBuiltin b) => PactSerialise b i -> Map.Map Text b -> Gen b -> Gen i -> Property
moduleDataRoundtrip serial builtins b i = property $ do
  moduleData <- forAll (moduleDataGen b i)
  moduleName <- forAll moduleNameGen
  evalEnv <- liftIO $ getEvalEnv serial builtins
  readResult <- liftIO $ runEvalM (ExecEnv evalEnv) def $ withSqlitePactDb serial ":memory:" $ \db -> do
    () <- evalWrite def db Insert DModules moduleName moduleData
    liftGasM def $ _pdbRead db DModules moduleName
  case readResult of
    (Left _, _) -> fail "Unexpected EvalM error"
    (Right writtenModuleData, _) ->
      Just moduleData === writtenModuleData

defPactExecRoundtrip :: (Default i) => PactSerialise b i -> Map.Map Text b -> Gen b -> Gen i -> Property
defPactExecRoundtrip serial builtins _b _i = property $ do
  defPactId <- forAll defPactIdGen
  defPactExec <- forAll (Gen.maybe defPactExecGen)
  evalEnv <- liftIO $ getEvalEnv serial builtins
  writeResult <- liftIO $ runEvalM (ExecEnv evalEnv) def $ withSqlitePactDb serial ":memory:" $ \db -> do
    () <- evalWrite def db Insert DDefPacts defPactId defPactExec
    liftGasM def $ _pdbRead db DDefPacts defPactId
  case writeResult of
    (Left _, _) -> fail "Unexpected EvalM error"
    (Right writtenDefPactExec, _) ->
      Just defPactExec === writtenDefPactExec

namespaceRoundtrip :: (Default i) => PactSerialise b i -> Map.Map Text b -> Property
namespaceRoundtrip serial builtins = property $ do
  ns <- forAll namespaceNameGen
  namespace <- forAll namespaceGen
  evalEnv <- liftIO $ getEvalEnv serial builtins
  writeResult <- liftIO $ runEvalM (ExecEnv evalEnv) def $ withSqlitePactDb serial ":memory:" $ \db -> do
    () <- evalWrite def db Insert DNamespaces ns namespace
    liftGasM def $ _pdbRead db DNamespaces ns
  case writeResult of
    (Left _, _) -> fail "Unexpected EvalM error"
    (Right writtenNamespace, _) ->
      Just namespace === writtenNamespace

-- | This regression test carries out a number of core operations on the
--   PactDb, including reading and writing Namespaces, KeySets, Modules, and
--   user data, within transactions. It ensures that TxLogs for transactions
--   contain the expected metadata.
sqliteRegression :: TestTree
sqliteRegression = withResource
  (unsafeCreateSqlitePactDb serialisePact_lineinfo_pact51 ":memory:")
  (\(_, db, stmtcache) -> unsafeCloseSqlitePactDb db stmtcache)
  $ \db ->
  testCase "Sqlite Db regression"
    (runPactDbRegression =<< fmap (view _1) db)

pureDbRegression :: TestTree
pureDbRegression = testCase "PureDb regression"
  (runPactDbRegression =<< mockPactDb serialisePact_lineinfo_pact51)
