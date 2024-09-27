module Pact.Core.PactDbRegression(runPactDbRegression) where

import Control.Monad.Except
import Data.Default
import qualified Data.Map as M
import qualified Data.Set as S
-- import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertEqual)

import Pact.Core.Builtin
import Pact.Core.Environment
import Pact.Core.Guards
import Pact.Core.Names
import Pact.Core.PactValue
import Pact.Core.Persistence
import Pact.Core.Persistence.MockPersistence
import Pact.Core.Serialise
import Pact.Core.Evaluate
import Pact.Core.Compile
import Pact.Core.StableEncoding

-- | Run our pact db regression suite
--   It takes an `IO (PactDb ..)` due to tasty's weird signature in
--   withResource.
runPactDbRegression :: PactDb CoreBuiltin Info -> IO ()
runPactDbRegression pdb = do
  let
    user1 = "user1"
    usert = TableName user1 (ModuleName "someModule" Nothing)
  _txId1 <- ignoreGas def $ _pdbBeginTx pdb Transactional
  ignoreGas def $ _pdbCreateUserTable pdb usert

  txs1 <- ignoreGas def $ _pdbCommitTx pdb
  let rdEnc = encodeStable (UserTableInfo (_tableModuleName usert))
  assertEqual "output of commit" txs1 [ TxLog "SYS:usertables" "user1" rdEnc ]


  --  Begin tx
  _ <- ignoreGas def $ _pdbBeginTx pdb Transactional >>= \case
    Nothing -> error "expected txid"
    Just t -> pure t
  let
    row = RowData $ M.fromList [(Field "gah", PDecimal 123.454345)]
  rowEnc <- ignoreGas def $ _encodeRowData serialisePact_lineinfo row
  ignoreGas def $ _pdbWrite pdb Insert (DUserTables usert) (RowKey "key1") row
  row' <- do
      ignoreGas def (_pdbRead pdb (DUserTables usert) (RowKey "key1")) >>= \case
        Nothing -> error "expected row"
        Just r -> pure r
  assertEqual "row should be identical to its saved/recalled value" row row'

  let
    row2 = RowData $ M.fromList
          [ (Field "gah", PBool False)
          , (Field "fh", PInteger 1)
          ]
  row2Enc <- ignoreGas def $ _encodeRowData serialisePact_lineinfo row2

  ignoreGas def $ _pdbWrite pdb Update (DUserTables usert) (RowKey "key1") row2
  row2' <- ignoreGas def $ _pdbRead pdb (DUserTables usert) (RowKey "key1") >>= \case
    Nothing -> error "expected row"
    Just r -> pure r
  assertEqual "user update should overwrite with new value" row2 row2'

  let
    ks = KeySet (S.fromList [PublicKeyText "skdjhfskj"]) KeysAll
    ksEnc = _encodeKeySet serialisePact_lineinfo ks
  _ <- ignoreGas def $ _pdbWrite pdb Write DKeySets (KeySetName "ks1" Nothing) ks
  ks' <- ignoreGas def $ _pdbRead pdb DKeySets (KeySetName "ks1" Nothing) >>= \case
    Nothing -> error "expected keyset"
    Just r -> pure r
  assertEqual "keyset should be equal after storage/retrieval" ks ks'


  -- module
  let mn = ModuleName "test" Nothing
  md <- loadModule
  let mdEnc = _encodeModuleData serialisePact_lineinfo md
  ignoreGas def $ _pdbWrite pdb Write DModules mn md

  md' <- ignoreGas def $ _pdbRead pdb DModules mn >>= \case
    Nothing -> error "Expected module"
    Just r -> pure r
  assertEqual "module should be identical to its saved/recalled value" md md'

  txs2 <- ignoreGas def $ _pdbCommitTx pdb
  -- Tx logs should be emitted in order
  flip (assertEqual "output of commit") txs2
    [ TxLog "someModule_user1" "key1" rowEnc
    , TxLog "someModule_user1" "key1" row2Enc
    , TxLog "SYS:KeySets" "ks1" ksEnc
    , TxLog "SYS:Modules" "test" mdEnc
    ]

  -- begin tx
  _ <- ignoreGas def $ _pdbBeginTx pdb Transactional

  ignoreGas def $ _pdbWrite pdb Insert (DUserTables usert) (RowKey "key2") row
  r1 <- ignoreGas def $ _pdbRead pdb (DUserTables usert) (RowKey "key2") >>= \case
    Nothing -> error "expected row"
    Just r -> pure r
  assertEqual "user insert key2 pre-rollback" row r1

  rkeys <- ignoreGas def $ _pdbKeys pdb (DUserTables usert)
  assertEqual "keys pre-rollback [key1, key2]" [RowKey "key1", RowKey "key2"] rkeys

  -- Roll back tx2
  ignoreGas def $ _pdbRollbackTx pdb

  _ <- ignoreGas def $ _pdbBeginTx pdb Transactional
  r2 <- ignoreGas def $ _pdbRead pdb (DUserTables usert) (RowKey "key2")
  assertEqual "rollback erases key2" Nothing r2

  rkeys2 <- ignoreGas def $ _pdbKeys pdb (DUserTables usert)
  assertEqual "keys post-rollback [key1]" [RowKey "key1"] rkeys2
  _ <- ignoreGas def $ _pdbCommitTx pdb

  return ()


loadModule :: IO (ModuleData CoreBuiltin Info)
loadModule = do
  let src = RawCode "(module test G (defcap G () true) (defun f (a: integer) 1))"
  pdb <- mockPactDb serialisePact_lineinfo
  ee <- defaultEvalEnv pdb coreBuiltinMap
  Right _ <- runEvalMResult (ExecEnv ee) def $ do
    p <- liftEither (compileOnlyLineInfo src)
    traverse (interpretTopLevel evalInterpreter) p
  Just md <- ignoreGas def $ _pdbRead pdb DModules (ModuleName "test" Nothing)
  pure md
