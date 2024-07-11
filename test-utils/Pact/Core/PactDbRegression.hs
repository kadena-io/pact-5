module Pact.Core.PactDbRegression(runPactDbRegression) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except
import Data.Default
import qualified Data.Map as M
import qualified Data.Set as S
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)

import Pact.Core.Builtin
import Pact.Core.Environment
import Pact.Core.Guards
import Pact.Core.Names
import Pact.Core.PactValue
import Pact.Core.Persistence
import Pact.Core.Persistence.MockPersistence
import Pact.Core.Serialise
import Pact.Core.Info
import Pact.Core.Evaluate
import Pact.Core.Compile
import Pact.Core.StableEncoding

-- | Run our pact db regression suite
--   It takes an `IO (PactDb ..)` due to tasty's weird signature in
--   withResource.
runPactDbRegression :: IO (PactDb CoreBuiltin SpanInfo) -> TestTree
runPactDbRegression pdbAction = testCase "PactDb persistence backend produces expected values/txlogs" $ do
  pdb <- pdbAction
  let
    user1 = "user1"
    usert = TableName user1 (ModuleName "someModule" Nothing)
  _txId1 <- liftIO $ _pdbBeginTx pdb Transactional
  ignoreGas def $ _pdbCreateUserTable pdb usert

  txs1 <- liftIO $ _pdbCommitTx pdb
  let rdEnc = encodeStable (UserTableInfo (_tableModuleName usert))
  liftIO $ assertEqual "output of commit" txs1 [ TxLog "SYS:usertables" "user1" rdEnc ]


  --  Begin tx
  t1 <- liftIO $ _pdbBeginTx pdb Transactional >>= \case
    Nothing -> error "expected txid"
    Just t -> pure t
  let
    row = RowData $ M.fromList [(Field "gah", PDecimal 123.454345)]
  rowEnc <- ignoreGas def $ _encodeRowData serialisePact_raw_spaninfo row
  ignoreGas def $ _pdbWrite pdb Insert (DUserTables usert) (RowKey "key1") row
  row' <- liftIO $ do
      _pdbRead pdb (DUserTables usert) (RowKey "key1") >>= \case
        Nothing -> error "expected row"
        Just r -> pure r
  liftIO $ assertEqual "row should be identical to its saved/recalled value" row row'

  let
    row2 = RowData $ M.fromList
          [ (Field "gah", PBool False)
          , (Field "fh", PInteger 1)
          ]
  row2Enc <- ignoreGas def $ _encodeRowData serialisePact_raw_spaninfo row2

  ignoreGas def $ _pdbWrite pdb Update (DUserTables usert) (RowKey "key1") row2
  row2' <- liftIO $ _pdbRead pdb (DUserTables usert) (RowKey "key1") >>= \case
    Nothing -> error "expected row"
    Just r -> pure r
  liftIO $ assertEqual "user update should overwrite with new value" row2 row2'

  let
    ks = KeySet (S.fromList [PublicKeyText "skdjhfskj"]) KeysAll
    ksEnc = _encodeKeySet serialisePact_raw_spaninfo ks
  _ <- ignoreGas def $ _pdbWrite pdb Write DKeySets (KeySetName "ks1" Nothing) ks
  ks' <- liftIO $ _pdbRead pdb DKeySets (KeySetName "ks1" Nothing) >>= \case
    Nothing -> error "expected keyset"
    Just r -> pure r
  liftIO $ assertEqual "keyset should be equal after storage/retrieval" ks ks'


  -- module
  let mn = ModuleName "test" Nothing
  md <- liftIO loadModule
  let mdEnc = _encodeModuleData serialisePact_raw_spaninfo md
  ignoreGas def $ _pdbWrite pdb Write DModules mn md

  md' <- liftIO $ _pdbRead pdb DModules mn >>= \case
    Nothing -> error "Expected module"
    Just r -> pure r
  liftIO $ assertEqual "module should be identical to its saved/recalled value" md md'

  txs2 <- liftIO $ _pdbCommitTx pdb
  liftIO $ flip (assertEqual "output of commit") txs2
    [ TxLog "SYS:MODULES" "test" mdEnc
    , TxLog "SYS:KEYSETS" "ks1" ksEnc
    , TxLog "USER_someModule_user1" "key1" row2Enc
    , TxLog "USER_someModule_user1" "key1" rowEnc
    ]

  -- begin tx
  _ <- liftIO $ do
    _ <- _pdbBeginTx pdb Transactional
    tids <- _pdbTxIds pdb usert t1
    assertEqual "user txids" [TxId 1] tids

    -- Note on _pdbGetTxLog
    -- This test is commented on purpose, because it used to be part of the spec,
    -- but in the presence of compaction, this test is a bit strange.
    -- We are currently unsure whether to keep this as part of the spec.

    -- txlog <- _pdbGetTxLog pdb usert (head tids)
    -- flip (assertEqual "user txlog") txlog
    --   [ TxLog "USER_someModule_user1" "key1" row2
    --   , TxLog "USER_someModule_user1" "key1" row
    --   ]

  ignoreGas def $ _pdbWrite pdb Insert (DUserTables usert) (RowKey "key2") row
  r1 <- liftIO $ _pdbRead pdb (DUserTables usert) (RowKey "key2") >>= \case
    Nothing -> error "expected row"
    Just r -> pure r
  liftIO $ assertEqual "user insert key2 pre-rollback" row r1

  liftIO $ do
    rkeys <- _pdbKeys pdb (DUserTables usert)
    liftIO $ assertEqual "keys pre-rollback [key1, key2]" [RowKey "key1", RowKey "key2"] rkeys

    _pdbRollbackTx pdb
    r2 <- _pdbRead pdb (DUserTables usert) (RowKey "key2")
    assertEqual "rollback erases key2" Nothing r2

    rkeys2 <- _pdbKeys pdb (DUserTables usert)
    assertEqual "keys post-rollback [key1]" [RowKey "key1"] rkeys2

  where
    loadModule = do
      let src = "(module test G (defcap G () true) (defun f (a: integer) 1))"
      pdb <- mockPactDb serialisePact_raw_spaninfo
      ee <- defaultEvalEnv pdb coreBuiltinMap
      Right _ <- runEvalMResult (ExecEnv ee) def $ do
        p <- liftEither (parseOnlyProgram src)
        traverse (interpretTopLevel evalInterpreter) p
      Just md <- _pdbRead pdb DModules (ModuleName "test" Nothing)
      pure md
