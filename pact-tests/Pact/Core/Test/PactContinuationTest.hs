{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Pact.Core.Test.PactContinuationTest(tests) where

import Control.Lens hiding ((.=))
import Control.Monad (forM_)
import Control.Monad.Reader
import Servant.Client
import Data.Decimal

import Test.Tasty
import Test.Tasty.HUnit
import Data.Default (def)
import Data.List (isInfixOf)
import Data.Text (Text)
import NeatInterpolation (text)
import System.Timeout

import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Short as SBS
import qualified Data.HashMap.Strict as HM
import qualified Control.Exception as Exception

import Pact.Core.Hash
import Pact.Core.Command.Client
import Pact.Core.Command.Server
import Pact.Core.Command.Types
import Pact.Core.Environment.Types
import Pact.Core.Errors
import Pact.Core.Evaluate
import Pact.Core.SPV
import Pact.Core.StableEncoding
import Pact.Core.Signer
import Pact.Core.PactValue
import Pact.Core.DefPacts.Types
import Pact.Core.Command.Util
import Pact.Core.Verifiers
import Pact.Core.Command.Crypto
import Pact.Core.Names
import Pact.Core.Capabilities

import qualified Pact.JSON.Encode as JE

import Pact.Core.Test.ServerUtils
-- ---- TESTS -----

tests :: TestTree
tests = testGroup "PactContinuationTests"
  [ testGroup "testPactContinuation" [testPactContinuation]
  , testGroup "testPactRollback" [testPactRollback]
  , testGroup "testPactYield" [testPactYield]
  , testGroup "testTwoPartyEscrow" $ [testTwoPartyEscrow]
  , testGroup "testOldNestedPacts" [testOldNestedPacts]
  , testGroup "testManagedCaps" [testManagedCaps]
  , testGroup "testElideModRefEvents" [testElideModRefEvents]
  , testGroup "testNestedPactContinuation" [testNestedPactContinuation]
  , testGroup "testNestedPactYield" [testNestedPactYield]
  , testGroup "testVerifiers" [testVerifiers]
  ]

testElideModRefEvents :: TestTree
testElideModRefEvents = testGroup "test module elide" $ [
  testCase "elides modref infos" $ do
    cmd <- mkExec code PUnit def [] [] Nothing Nothing
    results <- runAll' [cmd] noSPVSupport []
    runResults results $ do
      shouldMatch cmd $ ExpectResult $ \cr ->
        JE.encodeStrict (JE.Array (StableEncoding <$> (_crEvents cr))) `shouldSatisfy`
          (not . ("refInfo" `isInfixOf`) . B.unpack)
  ]
  where
    code =
      [text|

           (interface iface
             (defun f:bool ()))

           (module evmodule G

             (defcap G () true)

             (implements iface)

             (defun f:bool () true)

             (defcap EVENT (mod:module{iface})
               @event true)

             (defun emit(mod:module{iface})
               (emit-event (EVENT mod))))

           (evmodule.emit evmodule)
           |]


mkModuleHash :: Text -> IO ModuleHash
mkModuleHash =
  either (fail . show) (return . ModuleHash . Hash . SBS.toShort) . parseB64UrlUnpaddedText'

testManagedCaps :: TestTree
testManagedCaps = do
  testCase "exercises managed PAY cap" $ do
    let setupPath = testDir ++ "cont-scripts/setup-"
        testPath = testDir ++ "cont-scripts/managed-"

    (_, sysModuleCmd)  <- mkApiReq (setupPath ++ "01-system.yaml")
    (_, acctModuleCmd) <- mkApiReq (setupPath ++ "02-accounts.yaml")
    (_, createAcctCmd) <- mkApiReq (setupPath ++ "04-create.yaml")
    (_, managedPay) <- mkApiReq (testPath ++ "01-pay.yaml")
    (_, managedPayFails) <- mkApiReq (testPath ++ "02-pay-fails.yaml")
    let allCmds = [sysModuleCmd,acctModuleCmd,createAcctCmd,managedPay,managedPayFails]
    allResults <- runAll allCmds

    mhash <- mkModuleHash "xkYy5KYtEkV1WFZ791iCnZjRBx6um5FEe1aU-rPCOv8"

    runResults allResults $ do
      sysModuleCmd `succeedsWith` (`shouldBe` textVal "system module loaded")
      acctModuleCmd `succeedsWith` (`shouldBe` textVal "TableCreated")
      succeeds createAcctCmd
      managedPay `succeedsWith'`
        (`shouldBe` (textVal "Transfer succeeded",
         [PactEvent "PAY"
          [textVal "Alice",textVal "Bob",decValue 0.9]
          (ModuleName "accounts" Nothing)
          mhash]))
      -- PEUserRecoverableError (UserEnforceError "insufficient balance")
      -- Encountered failure in: PEUserRecoverableError, caused by: UserEnforceError
      managedPayFails `failsWithCode` (ErrorCode 0x0004000000000000)

testOldNestedPacts :: TestTree
testOldNestedPacts = do
  testCase "throws error when multiple defpact executions occur in same transaction" $ do
    adminKeys <- DynEd25519KeyPair <$> generateEd25519KeyPair
    let makeExecCmdWith = makeExecCmd adminKeys

    moduleCmd <- makeExecCmdWith (threeStepPactCode "nestedPact")
    nestedExecPactCmd <- makeExecCmdWith ("(nestedPact.tester)" <> " (nestedPact.tester)")
    allResults <- runAll [moduleCmd, nestedExecPactCmd]

    runResults allResults $ do
      succeeds moduleCmd
      -- pact-5 --explain-error-code 0x00031a0000000000
      -- Encountered failure in: PEExecutionError, caused by: MultipleOrNestedDefPactExecFound
      nestedExecPactCmd `failsWithCode` (ErrorCode 0x00031a0000000000)


-- CONTINUATIONS TESTS

testPactContinuation :: TestTree
testPactContinuation = testGroup "test pact continuation" $ [
  testCase "sends (+ 1 2) command to locally running dev server" $ do
    let cmdData = (PactResultOk . PInteger) 3
        --expRes = Just $ CommandResult _ ((Just . TxId) 0) cmdData (Gas 0)
    cr <- testSimpleServerCmd
    (_crResult <$> cr) `shouldBe` Just cmdData

  ,testCase "when provided with correct next step executes the next step and updates pact's state" $ do
      let mname1 = "testCorrectNextStep"
      testCorrectNextStep (threeStepPactCode mname1) ("(" <> mname1 <> ".tester)") testFlags

  ,testCase "when provided with incorrect next step throws error and does not update pact's state" $ do
      let mname2 = "testIncorrectNextStep"
      testIncorrectNextStep (threeStepPactCode mname2) ("(" <> mname2 <> ".tester)") testFlags

  ,testCase "when last step of a pact executed deletes pact from the state" $ do
      let mname3 = "testLastStep"
      testLastStep (threeStepPactCode mname3) ("(" <> mname3 <> ".tester)") testFlags

  ,testCase "when error occurs when executing pact step throws error and does not update pact's state" $ do
      let mname4 = "testErrStep"
      testErrStep (errorStepPactCode mname4) ("(" <> mname4 <> ".tester)") testFlags
      ]

testNestedPactContinuation :: TestTree
testNestedPactContinuation = testGroup "test nested pact continuation" $ [
  testCase "sends (+ 1 2) command to locally running dev server" $ do
    let cmdData = (PactResultOk . PInteger) 3
    cr <- testSimpleServerCmd
    (_crResult <$> cr) `shouldBe` Just cmdData

  ,testCase "when provided with correct next step executes the next step and updates nested pact's state" $ do
    let mname1 = "testCorrectNextNestedStep"
    testCorrectNextStep (threeStepNestedPactCode mname1) ("(" <> mname1 <> "-nested.nestedTester " <> mname1 <> "-2)") nestedDefPactFlags

  ,testCase "when provided with incorrect next step throws error and does not update nested pact's state" $ do
      let mname2 = "testIncorrectNextNestedStep"
      testIncorrectNextStep (threeStepNestedPactCode mname2) ("(" <> mname2 <> "-nested.nestedTester " <> mname2 <> "-2)") nestedDefPactFlags
  ,testCase "when last step of a pact executed deletes pact from the state" $ do
      let mname3 = "testNestedLastStep"
      testLastStep (threeStepNestedPactCode mname3) ("(" <> mname3 <> "-nested.nestedTester " <> mname3 <> "-2)") nestedDefPactFlags

  , testCase "when error occurs when executing pact step throws error and does not update pact's state" $ do
      let mname4 = "testNestedErrStep"
      testErrStep (errorStepNestedPactCode mname4) ("(" <> mname4 <> "-nested.nestedTester)") nestedDefPactFlags
  ]

testSimpleServerCmd :: IO (Maybe (CommandResult Hash (PactErrorCompat (LocatedErrorInfo Info))))
testSimpleServerCmd = do
  simpleKeys <- DynEd25519KeyPair <$> generateEd25519KeyPair
  cmd <- mkExec  "(+ 1 2)" PUnit def [(simpleKeys,[])] [] Nothing (Just "test1")
  allResults <- runAll [cmd]
  return $ M.lookup (cmdToRequestKey cmd) allResults


testCorrectNextStep :: Text -> Text -> [ExecutionFlag] -> Assertion
testCorrectNextStep code command flags = do
  adminKeys <- DynEd25519KeyPair <$> generateEd25519KeyPair
  let makeExecCmdWith = makeExecCmd adminKeys
  moduleCmd       <- makeExecCmdWith code
  executePactCmd  <- makeExecCmdWith command

  let makeContCmdWith = makeContCmd adminKeys False PUnit executePactCmd
  contNextStepCmd <- makeContCmdWith 1 "test3"
  checkStateCmd   <- makeContCmdWith 1 "test4"
  allResults      <- runAll' [moduleCmd, executePactCmd, contNextStepCmd, checkStateCmd] noSPVSupport flags

  runResults allResults $ do
    succeeds moduleCmd
    executePactCmd `succeedsWith` (`shouldBe` textVal "step 0")
    contNextStepCmd `succeedsWith` (`shouldBe` textVal "step 1")
    -- pact-5 --explain-error-code 0x00031f0000000000
    -- Encountered failure in: PEExecutionError, caused by: DefPactStepMismatch
    -- Fails with a `DefpactStepMismatch`, which is what we want.
    checkStateCmd `failsWithCode` (ErrorCode 0x00031f0000000000)


threeStepPactCode :: T.Text -> T.Text
threeStepPactCode moduleName =
  [text|
    (define-keyset 'k (read-keyset "admin-keyset"))
      (module $moduleName 'k
        (defpact tester ()
          (step "step 0")
          (step "step 1")
          (step "step 2"))) |]

threeStepNestedPactCode :: T.Text -> T.Text
threeStepNestedPactCode moduleName =
  [text|
    (interface iface

     (defpact ndp:string ())
    )
    (module $moduleName g
     (defcap g () true)
     (defpact tester ()
       (step "step 0")
       (step "step 1")
       (step "step 2")))
     (module $moduleName-2 g
     (defcap g () true)
     (implements iface)
     (defpact ndp:string ()
       (step
         (let
           ((unused 1))
           ($moduleName.tester)
           "step 0")
       )
       (step
         (let
           ((unused 1))
           (continue ($moduleName.tester))
           "step 1")
       )
       (step
         (let
           ((unused 1))
           (continue ($moduleName.tester))
           "step 2")
       ))
       )

     (module $moduleName-nested g
       (defcap g () true)
       (defpact nestedTester (m:module{iface})
         (step
         (let
           ((unused 1))
           (m::ndp)
           "step 0")
       )
       (step
         (let
           ((unused 1))
           (continue (m::ndp))
           "step 1")
       )
       (step
         (let
           ((unused 1))
           (continue (m::ndp))
           "step 2")
       )
       ))
       |]


testIncorrectNextStep :: Text -> Text -> [ExecutionFlag] -> Assertion
testIncorrectNextStep code command flags = do
  adminKeys <- DynEd25519KeyPair <$> generateEd25519KeyPair

  let makeExecCmdWith = makeExecCmd adminKeys
  moduleCmd         <- makeExecCmdWith code
  executePactCmd    <- makeExecCmdWith command

  let makeContCmdWith = makeContCmd adminKeys False PUnit executePactCmd
  incorrectStepCmd  <- makeContCmdWith 2 "test3"
  checkStateCmd     <- makeContCmdWith 1 "test4"
  allResults        <- runAll' [moduleCmd, executePactCmd, incorrectStepCmd, checkStateCmd] noSPVSupport flags

  runResults allResults $ do
    succeeds moduleCmd
    executePactCmd `succeedsWith` (`shouldBe` textVal "step 0")
    -- We expect a step mismatch
    -- pact-5 --explain-error-code 0x00031f0000000000
    -- Encountered failure in: PEExecutionError, caused by: DefPactStepMismatch
    incorrectStepCmd `failsWithCode` (ErrorCode 0x00031f0000000000)
    checkStateCmd `succeedsWith` (`shouldBe` textVal "step 1")


testLastStep :: Text -> Text -> [ExecutionFlag] -> Assertion
testLastStep code command flags = do
  adminKeys <- DynEd25519KeyPair <$> generateEd25519KeyPair

  let makeExecCmdWith = makeExecCmd adminKeys
  moduleCmd        <- makeExecCmdWith code
  executePactCmd   <- makeExecCmdWith command

  let makeContCmdWith = makeContCmd adminKeys False PUnit executePactCmd
  contNextStep1Cmd <- makeContCmdWith 1 "test3"
  contNextStep2Cmd <- makeContCmdWith 2 "test4"
  checkStateCmd    <- makeContCmdWith 3 "test5"
  allResults       <- runAll' [moduleCmd, executePactCmd, contNextStep1Cmd,
                              contNextStep2Cmd, checkStateCmd] noSPVSupport flags

  runResults allResults $ do
    succeeds moduleCmd
    executePactCmd `succeedsWith` (`shouldBe` textVal "step 0")
    contNextStep1Cmd `succeedsWith` (`shouldBe` textVal "step 1")
    contNextStep2Cmd `succeedsWith` (`shouldBe` textVal "step 2")
    -- We are expecting the pact was already completed here.
    -- pact-5 --explain-error-code 0x0003150000000000
    -- Encountered failure in: PEExecutionError, caused by: DefPactAlreadyCompleted
    checkStateCmd `failsWithCode` (ErrorCode 0x0003150000000000)



testErrStep :: Text -> Text -> [ExecutionFlag] -> Assertion
testErrStep code command flags = do
  adminKeys <- DynEd25519KeyPair <$> generateEd25519KeyPair

  let makeExecCmdWith = makeExecCmd adminKeys
  moduleCmd        <- makeExecCmdWith code
  executePactCmd   <- makeExecCmdWith command

  let makeContCmdWith = makeContCmd adminKeys False PUnit executePactCmd
  contErrStepCmd   <- makeContCmdWith 1 "test3"
  checkStateCmd    <- makeContCmdWith 2 "test4"
  allResults       <- runAll' [moduleCmd, executePactCmd, contErrStepCmd, checkStateCmd] noSPVSupport flags

  runResults allResults $ do
    succeeds moduleCmd
    executePactCmd `succeedsWith` (`shouldBe` textVal "step 0")
    fails contErrStepCmd
    -- We expect a step mismatch
    -- pact-5 --explain-error-code 0x00031f0000000000
    -- Encountered failure in: PEExecutionError, caused by: DefPactStepMismatch
    checkStateCmd `failsWithCode` (ErrorCode 0x00031f0000000000)


errorStepPactCode :: T.Text -> T.Text
errorStepPactCode moduleName =
  [text|
    (define-keyset 'k (read-keyset "admin-keyset"))
    (module $moduleName 'k
      (defpact tester ()
        (step "step 0")
        (step (+ "will throw error in step 1"))
    (step "step 2")))|]

errorStepNestedPactCode :: T.Text -> T.Text
errorStepNestedPactCode moduleName =
  [text|
      (module $moduleName g
       (defcap g () true)
       (defpact tester ()
         (step "step 0")
         (step (+ "will throw error in step 1"))
         (step "step 2")))
       (module $moduleName-nested g
         (defcap g () true)
         (defpact nestedTester ()
           (step
           (let
             ((unused 1))
             ($moduleName.tester)
             "step 0")
         )
         (step
           (let
             ((unused 1))
             (continue ($moduleName.tester))
             "step 1")
         )
         (step
           (let
             ((unused 1))
             (continue ($moduleName.tester))
             "step 2")
         )
         ))
                |]


-- ROLLBACK TESTS

testPactRollback :: TestTree
testPactRollback = testGroup "pact rollback" $ [
  testCase "when provided with correct rollback stepexecutes the rollback function and deletes pact from state"
      testCorrectRollbackStep

  ,testCase "when provided with incorrect rollback step throws error and does not delete pact from state"
      testIncorrectRollbackStep

  ,testCase "when error occurs when executing rollback function throws error and does not delete pact from state"
      testRollbackErr

  , testCase "when trying to rollback a step without a rollback function outputs a rollback failure and doesn't change the pact's state"
      testNoRollbackFunc
  ]


testCorrectRollbackStep :: Assertion
testCorrectRollbackStep = do
  let moduleName = "testCorrectRollbackStep"
  adminKeys <- DynEd25519KeyPair <$> generateEd25519KeyPair

  let makeExecCmdWith = makeExecCmd adminKeys
  moduleCmd       <- makeExecCmdWith (pactWithRollbackCode moduleName)
  executePactCmd  <- makeExecCmdWith ("(" <> moduleName <> ".tester)")

  let makeContCmdWith = makeContCmd adminKeys False PUnit executePactCmd
      makeContCmdWithRollback = makeContCmd adminKeys True PUnit executePactCmd
  contNextStepCmd <- makeContCmdWith 1 "test3"
  rollbackStepCmd <- makeContCmdWithRollback 1 "test4" -- rollback = True
  checkStateCmd   <- makeContCmdWith 2 "test5"
  allResults      <- runAll [moduleCmd, executePactCmd, contNextStepCmd,
                             rollbackStepCmd, checkStateCmd]

  runResults allResults $ do
    succeeds moduleCmd
    executePactCmd `succeedsWith` (`shouldBe` textVal "step 0")
    contNextStepCmd `succeedsWith` (`shouldBe` textVal "step 1")
    rollbackStepCmd `succeedsWith` (`shouldBe` textVal "rollback 1")
    -- We are expecting the pact was already completed here.
    -- pact-5 --explain-error-code 0x0003150000000000
    -- Encountered failure in: PEExecutionError, caused by: DefPactAlreadyCompleted
    checkStateCmd `failsWithCode` (ErrorCode 0x0003150000000000)



pactWithRollbackCode :: T.Text -> T.Text
pactWithRollbackCode moduleName =
  [text|
    (define-keyset 'k (read-keyset "admin-keyset"))
    (module $moduleName 'k
      (defpact tester ()
        (step-with-rollback "step 0" "rollback 0")
        (step-with-rollback "step 1" "rollback 1")
        (step               "step 2")))
        |]


testIncorrectRollbackStep :: Assertion
testIncorrectRollbackStep = do
  let moduleName = "testIncorrectRollbackStep"
  adminKeys <- DynEd25519KeyPair <$> generateEd25519KeyPair

  let makeExecCmdWith = makeExecCmd adminKeys
  moduleCmd       <- makeExecCmdWith (pactWithRollbackCode moduleName)
  executePactCmd  <- makeExecCmdWith ("(" <> moduleName <> ".tester)")

  let makeContCmdWith = makeContCmd adminKeys False PUnit executePactCmd
      makeContCmdWithRollback = makeContCmd adminKeys True PUnit executePactCmd
  contNextStepCmd <- makeContCmdWith 1 "test3"
  incorrectRbCmd  <- makeContCmdWithRollback 2 "test4"
  checkStateCmd   <- makeContCmdWith 2 "test5"
  allResults      <- runAll [moduleCmd, executePactCmd, contNextStepCmd,
                             incorrectRbCmd, checkStateCmd]

  runResults allResults $ do
    succeeds moduleCmd
    executePactCmd `succeedsWith` (`shouldBe` textVal "step 0")
    contNextStepCmd `succeedsWith` (`shouldBe` textVal "step 1")
    -- pact-5 --explain-error-code 0x00031e0000000000
    -- Encountered failure in: PEExecutionError, caused by: DefPactRollbackMismatch
    incorrectRbCmd `failsWithCode` (ErrorCode 0x00031e0000000000)
    checkStateCmd `succeedsWith` (`shouldBe` textVal "step 2")


testRollbackErr :: Assertion
testRollbackErr = do
  let moduleName = "testRollbackErr"
  adminKeys <- DynEd25519KeyPair <$> generateEd25519KeyPair

  let makeExecCmdWith = makeExecCmd adminKeys
  moduleCmd        <- makeExecCmdWith (pactWithRollbackErrCode moduleName)
  executePactCmd   <- makeExecCmdWith ("(" <> moduleName <> ".tester)")

  let makeContCmdWith = makeContCmd adminKeys False PUnit executePactCmd
      makeContCmdWithRollback = makeContCmd adminKeys True PUnit executePactCmd
  contNextStepCmd  <- makeContCmdWith 1 "test3"
  rollbackErrCmd   <- makeContCmdWithRollback 1 "test4"
  checkStateCmd    <- makeContCmdWith 2 "test5"
  allResults       <- runAll [moduleCmd, executePactCmd, contNextStepCmd,
                              rollbackErrCmd, checkStateCmd]

  runResults allResults $ do
    succeeds moduleCmd
    executePactCmd `succeedsWith` (`shouldBe` textVal "step 0")
    contNextStepCmd `succeedsWith` (`shouldBe` textVal "step 1")
    fails rollbackErrCmd
    checkStateCmd `succeedsWith` (`shouldBe` textVal "step 2")


pactWithRollbackErrCode :: T.Text -> T.Text
pactWithRollbackErrCode moduleName =
  [text|
    (define-keyset 'k (read-keyset "admin-keyset"))
    (module $moduleName 'k
      (defpact tester ()
        (step-with-rollback "step 0" "rollback 0")
        (step-with-rollback "step 1" (+ "will throw error in rollback 1"))
        (step               "step 2")))
        |]


testNoRollbackFunc :: Assertion
testNoRollbackFunc = do
  let moduleName = "testNoRollbackFunc"
  adminKeys <- DynEd25519KeyPair <$> generateEd25519KeyPair

  let makeExecCmdWith = makeExecCmd adminKeys
  moduleCmd        <- makeExecCmdWith (threeStepPactCode moduleName)
  executePactCmd   <- makeExecCmdWith ("(" <> moduleName <> ".tester)")

  let makeContCmdWith = makeContCmd adminKeys False PUnit executePactCmd
      makeContCmdWithRollback = makeContCmd adminKeys True PUnit executePactCmd
  contNextStepCmd  <- makeContCmdWith 1 "test3"
  noRollbackCmd    <- makeContCmdWithRollback 1 "test4"
  checkStateCmd    <- makeContCmdWith 2 "test5"
  allResults       <- runAll [moduleCmd, executePactCmd, contNextStepCmd,
                              noRollbackCmd, checkStateCmd]

  runResults allResults $ do
    succeeds moduleCmd
    executePactCmd `succeedsWith` (`shouldBe` textVal "step 0")
    contNextStepCmd `succeedsWith` (`shouldBe` textVal "step 1")
    -- ✗ pact-5 --explain-error-code 0x00031b0000000000
    -- Encountered failure in: PEExecutionError, caused by: DefPactStepHasNoRollback
    noRollbackCmd `failsWithCode` (ErrorCode 0x00031b0000000000)
    checkStateCmd `succeedsWith` (`shouldBe` textVal "step 2")



-- YIELD / RESUME TESTS

testPactYield :: TestTree
testPactYield = testGroup "pact yield"$ [
  testCase "when previous step yields value resumes value" $ do
      let mname1 = "testValidYield"
      testValidYield mname1 pactWithYield testFlags

  ,testCase "when previous step does not yield value throws error when trying to resume, and does not delete pact from state" $ do
      let mname2 = "testNoYield"
      testNoYield mname2 pactWithYieldErr testFlags

  , testCase "resets yielded values after each step" $ do
    let mname3 = "testResetYield"
    testResetYield mname3 pactWithSameNameYield testFlags

  , testCase "testCrossChainYield:succeeds with same module" $
      testCrossChainYield "" Nothing mkFakeSPV testFlags
  -- pact-5 --explain-error-code 0x0003320000000000
  -- Encountered failure in: PEExecutionError, caused by: YieldProvenanceDoesNotMatch
  -- Note: when porting over from prod, this used to be ;;1 for the bless code, but
  -- simply changing a comment on an identical module does _not_ change the hash.
  -- the hash depends on the cbor encoding, so we bless some dummy hash
  ,testCase "testCrossChainYield:fails with different module" $
      testCrossChainYield "(bless \"_9xPxvYomOU0iEqXpcrChvoA-E9qoaE1TqU460xN1AA\")"
        (Just (`shouldBeErrorCode` (ErrorCode 0x0003320000000000)))
        mkFakeSPV testFlags

  ,testCase "testCrossChainYield:succeeds with blessed module" $
    testCrossChainYield "(bless \"kuBrddl82uCHbhV1ECaH7fMf00Pq9lc2mPShU4Us_Jg\")" Nothing mkFakeSPV testFlags

  ,testCase "testCrossChainYield:fails with a ContinuationError post-fork" $
      testCrossChainYield "(bless \"kuBrddl82uCHbhV1ECaH7fMf00Pq9lc2mPShU4Us_Jg\")"
        -- pact-5 --explain-error-code 0x0003390000000000
        -- Encountered failure in: PEExecutionError, caused by: ContinuationError
        (Just $ (`shouldBeErrorCode` ErrorCode 0x0003380000000000))
        (const noSPVSupport) testFlags
  ]
testNestedPactYield :: TestTree
testNestedPactYield = testGroup "nested pact yield" $ [
  testCase "when previous step yields value resumes value" $ do
      let mname1 = "testNestedValidYield"
      testValidYield mname1 nestedPactWithYield nestedDefPactFlags

  ,testCase "when previous step does not yield value throws error when trying to resume, and does not delete pact from state" $ do
      let mname2 = "testNestedNoYield"
      testNoYield mname2 nestedPactWithYieldErr nestedDefPactFlags

  ,testCase "resets yielded values after each step" $ do
    let mname3 = "testNestedResetYield"
    testResetYield mname3 nestedPactWithSameNameYield nestedDefPactFlags

  ,testCase "testCrossChainYield:succeeds with same module"
      testNestedCrossChainYield
  ]
  where
  testNestedCrossChainYield = step0
    where
    -- STEP 0: runs on server for "chain0results"
    -- note we're not changing server ID, just starting with
    -- a fresh server to prove that a new pact coming through
    -- SPV can start from step 1.
    step0 = do
      adminKeys <- DynEd25519KeyPair <$> generateEd25519KeyPair

      let makeExecCmdWith = makeExecCmd' (Just "xchain") adminKeys
      moduleCmd        <- makeExecCmdWith nestedPactCrossChainYield
      executePactCmd   <- makeExecCmdWith "(cross-chain-tester.cross-chain \"jose\")"

      chain0Results <-
        runAll' [moduleCmd,executePactCmd] noSPVSupport nestedDefPactFlags

      mhash <- mkModuleHash "J7Ua1U7GmzmmoMpEz3mASyG5aYEPGT6zZAkDZn7bl9I"

      runResults chain0Results $ do
        succeeds moduleCmd
        executePactCmd `succeedsWith'`
            (`shouldBe` (textVal "jose->A",
                    [PactEvent
                     "X_YIELD"
                     [ textVal ""
                     , textVal "cross-chain-tester.cross-chain"
                     , PList $ V.fromList [ textVal "jose" ]]
                     (ModuleName "pact" Nothing)
                     mhash]))
        shouldMatch executePactCmd $ ExpectResult $ \cr ->
          preview (crContinuation . _Just . peYield . _Just . ySourceChain . _Just) cr
          `shouldBe`
          (Just (ChainId ""))

      let rk = cmdToRequestKey executePactCmd

      case M.lookup rk chain0Results of
        Nothing -> assertFailure $
          "Could not find result " ++ show rk ++ ": " ++ show chain0Results
        Just cr -> case _crContinuation cr of
          Nothing -> assertFailure $
            "No continuation in result: " ++ show rk
          Just pe -> do
            step1 adminKeys executePactCmd moduleCmd pe mhash

    -- STEP 1: found the pact exec from step 0; return this
    -- from the SPV operation. Run a fresh server, reload
    -- the module, and run the step.
    step1 adminKeys executePactCmd moduleCmd pe mhash = do

      let proof = (ContProof "hi there")
          makeContCmdWith = makeContCmd' (Just proof) adminKeys False PUnit executePactCmd
          spv = noSPVSupport {
            _spvVerifyContinuation = \cp ->
                if cp == proof then
                  return $ Right pe
                else
                  return $ Left "Invalid proof"
            }

      chain1Cont <- makeContCmdWith 1 "chain1Cont"
      chain1ContDupe <- makeContCmdWith 1 "chain1ContDupe"

      chain1Results <-
        runAll' [moduleCmd, chain1Cont,chain1ContDupe] spv nestedDefPactFlags

      runResults chain1Results $ do
        succeeds moduleCmd
        chain1Cont `succeedsWith'`
          (`shouldBe` (textVal "jose->A->B",
                  [PactEvent
                    "X_RESUME"
                    [ textVal ""
                    , textVal "cross-chain-tester.cross-chain"
                    , PList $ V.fromList [ textVal "jose" ]]
                    (ModuleName "pact" Nothing)
                    mhash]))
        -- we expect the defpact to already be completed here
        -- pact-5 --explain-error-code 0x0003150000000000
        -- Encountered failure in: PEExecutionError, caused by: DefPactAlreadyCompleted
        chain1ContDupe `failsWithCode` (ErrorCode 0x0003150000000000)


testValidYield :: Text -> (Text -> Text) -> [ExecutionFlag] -> Assertion
testValidYield moduleName mkCode flags = do
  adminKeys <- DynEd25519KeyPair <$> generateEd25519KeyPair

  let makeExecCmdWith = makeExecCmd adminKeys
  moduleCmd          <- makeExecCmdWith (mkCode moduleName)
  executePactCmd     <- makeExecCmdWith ("(" <> moduleName <> ".tester \"testing\")")

  let makeContCmdWith = makeContCmd adminKeys False PUnit executePactCmd
                        -- pact takes an input
  resumeAndYieldCmd  <- makeContCmdWith 1 "test3"
  resumeOnlyCmd      <- makeContCmdWith 2 "test4"
  checkStateCmd      <- makeContCmdWith 3 "test5"
  allResults         <- runAll' [moduleCmd, executePactCmd, resumeAndYieldCmd,
                                resumeOnlyCmd, checkStateCmd] noSPVSupport flags

  runResults allResults $ do
    succeeds moduleCmd
    executePactCmd `succeedsWith` (`shouldBe` textVal "testing->Step0")
    resumeAndYieldCmd `succeedsWith` (`shouldBe` textVal "testing->Step0->Step1")
    resumeOnlyCmd `succeedsWith` (`shouldBe` textVal "testing->Step0->Step1->Step2")
    checkStateCmd `failsWithCode` (ErrorCode 0x0003150000000000)


pactWithYield :: T.Text -> T.Text
pactWithYield moduleName =
  [text|
    (define-keyset 'k (read-keyset "admin-keyset"))
    (module $moduleName 'k
      (defpact tester (name)
        (step
          (let ((result0 (+ name "->Step0")))
            (yield { "step0-result": result0})
            result0))
        (step
          (resume {"step0-result" := res0 }
          (let ((result1 (+ res0 "->Step1")))
            (yield {"step1-result": result1})
            result1)))
        (step
          (resume { "step1-result" := res1 }
             (+ res1 "->Step2")))))|]

nestedPactWithYield :: T.Text -> T.Text
nestedPactWithYield moduleName =
  [text|
    (module nested-$moduleName g
      (defcap g () true)
      (defpact tester (name)
        (step
          (let ((result0 (+ name "->Step0")))
            (yield { "step0-result": result0})
            result0))
        (step
          (resume {"step0-result" := res0 }
          (let ((result1 (+ res0 "->Step1")))
            (yield {"step1-result": result1})
            result1)))
        (step
          (resume { "step1-result" := res1 }
             (+ res1 "->Step2")))))
     (module $moduleName g
      (defcap g () true)
      (defpact tester (name)
        (step
          (let ((result0 (+ name "->Step0")))
            (nested-$moduleName.tester name)
            (yield { "step0-result": result0})
            result0))
        (step
          (resume {"step0-result" := res0 }
          (let ((result1 (+ res0 "->Step1")))
            (continue (nested-$moduleName.tester name))
            (yield {"step1-result": result1})
            result1)))
        (step
          (resume { "step1-result" := res1 }
             (continue (nested-$moduleName.tester name))
             (+ res1 "->Step2")))))
             |]



testNoYield :: Text -> (Text -> Text) -> [ExecutionFlag] -> Assertion
testNoYield moduleName mkCode flags = do
  -- let moduleName = "testNoYield"
  adminKeys <- DynEd25519KeyPair <$> generateEd25519KeyPair

  let makeExecCmdWith = makeExecCmd adminKeys
  moduleCmd      <- makeExecCmdWith (mkCode moduleName)
  executePactCmd <- makeExecCmdWith ("(" <> moduleName <> ".tester \"testing\")") -- pact takes an input

  let makeContCmdWith = makeContCmd adminKeys False PUnit executePactCmd
  noYieldStepCmd <- makeContCmdWith 1 "test3"
  resumeErrCmd   <- makeContCmdWith 2 "test3"
  checkStateCmd  <- makeContCmdWith 1 "test5"
  allResults     <- runAll' [moduleCmd, executePactCmd, noYieldStepCmd,
                           resumeErrCmd, checkStateCmd] noSPVSupport flags

  runResults allResults $ do
    succeeds moduleCmd
    executePactCmd `succeedsWith` (`shouldBe` textVal "testing->Step0")
    noYieldStepCmd `succeedsWith` (`shouldBe` textVal "step 1 has no yield")
    fails resumeErrCmd
    checkStateCmd `failsWithCode` (ErrorCode 0x00031f0000000000)


pactWithYieldErr :: T.Text -> T.Text
pactWithYieldErr moduleName =
  [text| (define-keyset 'k (read-keyset "admin-keyset"))
             (module $moduleName 'k
               (defpact tester (name)
                 (step
                   (let ((result0 (+ name "->Step0")))
                    (yield { "step0-result": result0 })
                    result0))
                 (step "step 1 has no yield")
                 (step
                   (resume { "step0-result" := res0 }
                      (+ res0 "->Step2")))))|]

nestedPactWithYieldErr :: T.Text -> T.Text
nestedPactWithYieldErr moduleName =
  [text|
    (module nested-$moduleName g
      (defcap g () true)
      (defpact tester (name)
       (step
         (let ((result0 (+ name "->Step0")))
          (yield { "step0-result": result0 })
          result0))
       (step "step 1 has no yield")
       (step
         (resume { "step0-result" := res0 }
            (+ res0 "->Step2")))))
    (module $moduleName g
     (defcap g () true)
     (defpact tester (name)
       (step
         (let ((result0 (+ name "->Step0")))
          (nested-$moduleName.tester name)
          (yield { "step0-result": result0 })
            result0))
       (step
       (let ((unused 1))
       (continue (nested-$moduleName.tester name))
       "step 1 has no yield"
       ))
       (step
         (resume { "step0-result" := res0 }
            (continue (nested-$moduleName.tester name))
            (+ res0 "->Step2")))))
            |]


testResetYield :: Text -> (Text -> Text) -> [ExecutionFlag] -> Assertion
testResetYield moduleName mkCode flags = do
  adminKeys <- DynEd25519KeyPair <$> generateEd25519KeyPair

  let makeExecCmdWith = makeExecCmd adminKeys
  moduleCmd        <- makeExecCmdWith (mkCode moduleName)
  executePactCmd   <- makeExecCmdWith ("(" <> moduleName <> ".tester)")

  let makeContCmdWith = makeContCmd adminKeys False PUnit executePactCmd
  yieldSameKeyCmd  <- makeContCmdWith 1 "test3"
  resumeStepCmd    <- makeContCmdWith 2 "test4"
  checkStateCmd    <- makeContCmdWith 3 "test5"
  allResults       <- runAll' [moduleCmd, executePactCmd, yieldSameKeyCmd,
                              resumeStepCmd, checkStateCmd] noSPVSupport flags

  runResults allResults $ do
    succeeds moduleCmd
    executePactCmd `succeedsWith` (`shouldBe` textVal "step 0")
    yieldSameKeyCmd `succeedsWith` (`shouldBe` textVal "step 1")
    resumeStepCmd `succeedsWith` (`shouldBe` textVal "step 1")
    checkStateCmd `failsWithCode` (ErrorCode 0x0003150000000000)



pactWithSameNameYield :: T.Text -> T.Text
pactWithSameNameYield moduleName =
  [text|
    (define-keyset 'k (read-keyset "admin-keyset"))
      (module $moduleName 'k
        (defpact tester ()
          (step
            (let ((result0 "step 0"))
             (yield { "result": result0 })
             result0))
          (step
            (let ((result1 "step 1"))
             (yield { "result": result1 })
             result1))
          (step
            (resume { "result" := res }
              res))))|]


nestedPactWithSameNameYield :: T.Text -> T.Text
nestedPactWithSameNameYield moduleName =
  [text|
    (module nested-$moduleName g
      (defcap g () true)
      (defpact tester ()
        (step
          (let ((result0 "step 0"))
           (yield { "result": result0 })
           result0))
        (step
          (let ((result1 "step 1"))
           (yield { "result": result1 })
           result1))
        (step
          (resume { "result" := res }
            res))))
     (module $moduleName g
      (defcap g () true)
      (defpact tester ()
        (step
          (let ((result0 "step 0"))
           (nested-$moduleName.tester)
           (yield { "result": result0 })
           result0))
        (step
          (let ((result1 "step 1"))
           (continue (nested-$moduleName.tester))
           (yield { "result": result1 })
           result1))
        (step
          (resume { "result" := res }
            (enforce (= (continue (nested-$moduleName.tester)) "step 1") "failure")
            res))))
            |]

fakeProof :: ContProof
fakeProof = ContProof "hi there"

mkFakeSPV :: DefPactExec -> SPVSupport
mkFakeSPV pe =
  noSPVSupport {
    _spvVerifyContinuation = \cp ->
      if cp == fakeProof then
        return $ Right pe
      else
        return $ Left "Invalid proof"
  }

testCrossChainYield :: T.Text -> Maybe (PactErrorCode (LocatedErrorInfo Info) -> Assertion) -> (DefPactExec -> SPVSupport) -> [ExecutionFlag] -> Assertion
testCrossChainYield blessCode expectFailure mkSpvSupport spvFlags = step0
  where

    -- STEP 0: runs on server for "chain0results"
    -- note we're not changing server ID, just starting with
    -- a fresh server to prove that a new pact coming through
    -- SPV can start from step 1.
    step0 = do
      adminKeys <- DynEd25519KeyPair <$> generateEd25519KeyPair

      let makeExecCmdWith = makeExecCmd' (Just "xchain") adminKeys
      moduleCmd        <- makeExecCmdWith (pactCrossChainYield "")
      moduleCmd'       <- makeExecCmdWith (pactCrossChainYield blessCode)
      executePactCmd   <- makeExecCmdWith "(cross-chain-tester.cross-chain \"emily\")"

      chain0Results <-
        runAll' [moduleCmd,executePactCmd] noSPVSupport []

      mhash <- mkModuleHash "kuBrddl82uCHbhV1ECaH7fMf00Pq9lc2mPShU4Us_Jg"

      runResults chain0Results $ do
        succeeds moduleCmd
        executePactCmd `succeedsWith'`
            (`shouldBe` (textVal "emily->A",
                    [PactEvent
                     "X_YIELD"
                     [ textVal ""
                     , textVal "cross-chain-tester.cross-chain"
                     , PList $ V.fromList [ textVal "emily" ]]
                     (ModuleName "pact" Nothing)
                     mhash]))
        shouldMatch executePactCmd $ ExpectResult $ \cr ->
          preview (crContinuation . _Just . peYield . _Just . ySourceChain . _Just) cr
          `shouldBe`
          (Just (ChainId ""))

      let rk = cmdToRequestKey executePactCmd

      case M.lookup rk chain0Results of
        Nothing -> assertFailure $
          "Could not find result " ++ show rk ++ ": " ++ show chain0Results
        Just cr -> case _crContinuation cr of
          Nothing -> assertFailure $
            "No continuation in result: " ++ show rk
          Just pe -> do
            step1 adminKeys executePactCmd moduleCmd' pe mhash
            -- step1fail adminKeys executePactCmd moduleCmd pe

    -- STEP 1: found the pact exec from step 0; return this
    -- from the SPV operation. Run a fresh server, reload
    -- the module, and run the step.
    step1 adminKeys executePactCmd moduleCmd pe mhash = do

      let
          makeContCmdWith = makeContCmd' (Just fakeProof) adminKeys False PUnit executePactCmd

      chain1Cont <- makeContCmdWith 1 "chain1Cont"
      chain1ContDupe <- makeContCmdWith 1 "chain1ContDupe"

      chain1Results <-
        runAll' [moduleCmd,chain1Cont,chain1ContDupe] (mkSpvSupport pe) spvFlags

      runResults chain1Results $ do
        succeeds moduleCmd
        case expectFailure of
          Nothing -> do
            -- chain1Cont `succeedsWith` textVal "emily->A->B"
            chain1Cont `succeedsWith'`
              (`shouldBe` (textVal "emily->A->B",
                   [PactEvent
                   "X_RESUME"
                   [ textVal ""
                   , textVal "cross-chain-tester.cross-chain"
                   , PList $ V.fromList [ textVal "emily" ]]
                   (ModuleName "pact" Nothing)
                   mhash]))
            chain1ContDupe `failsWithCode` (ErrorCode 0x0003150000000000)
          Just expected ->
            chain1ContDupe `failsWith'` expected


pactCrossChainYield :: T.Text -> T.Text
pactCrossChainYield blessExpr =
  [text|
    (module cross-chain-tester GOV
      (defcap GOV () true)
      (defschema schema-a a-result:string)
      $blessExpr
      (defpact cross-chain (name)
        (step
          (let*
            ((nameA (+ name "->A"))
             (r:object{schema-a} { "a-result": nameA }))

            (yield r "")
            nameA))

        (step
          (resume { "a-result" := ar }
                  (+ ar "->B")))
        ))
  |]

nestedPactCrossChainYield :: T.Text
nestedPactCrossChainYield =
  [text|
    (module nested-tester GOV
      (defcap GOV () true)
      (defschema schema-a a-result:string)
      (defpact cross-chain (name)
        (step
          (let*
            ((nameA (+ name "->A"))
             (r:object{schema-a} { "a-result": nameA }))

            (yield r "")
            nameA))

        (step
          (resume { "a-result" := ar }
                  (+ ar "->B")))
        ))
    (module cross-chain-tester GOV
      (defcap GOV () true)
      (defschema schema-a a-result:string)
      (defpact cross-chain (name)
        (step
          (let*
            ((nameA (+ name "->A"))
             (r:object{schema-a} { "a-result": nameA }))
            (nested-tester.cross-chain name)
            (yield r "")
            nameA))

        (step
          (resume { "a-result" := ar }
          (continue (nested-tester.cross-chain name))
                  (+ ar "->B")))
        ))
  |]


-- TWO PARTY ESCROW TESTS

testTwoPartyEscrow :: TestTree
testTwoPartyEscrow = testGroup "two party escrow" $ [
  testCase "when debtor tries to cancel pre-timeout throws error and money still escrowed"
      testDebtorPreTimeoutCancel

  , testCase "when debtor tries to cancel after timeout cancels escrow and deposits escrowed amount back to debtor"
      testDebtorPostTimeoutCancel

  , testCase "cancels escrow immediately if creditor cancels"
    testCreditorCancel

  ,testCase "throws error when creditor or debtor try to finish alone"
    testFinishAlone

  ,testCase "throws error when final price negotiated up"
    testPriceNegUp

  ,testGroup "when both debtor and creditor finish together" $ [
    testCase "finishes escrow if final price stays the same or negotiated down"
      testValidEscrowFinish
    ,testCase "with valid price, still fails if bad cap is on a signature"
      testPriceNegDownBadCaps
    ]
  ]

twoPartyEscrow
  :: [Command Text]
  -> (Hash -> ReaderT (M.Map RequestKey (CommandResult Hash (PactErrorCompat (LocatedErrorInfo Info)))) IO ())
  -> Assertion
twoPartyEscrow testCmds act = do
  let setupPath = testDir ++ "cont-scripts/setup-"

  (_, sysModuleCmd)  <- mkApiReq (setupPath ++ "01-system.yaml")
  (_, acctModuleCmd) <- mkApiReq (setupPath ++ "02-accounts.yaml")
  (_, testModuleCmd) <- mkApiReq (setupPath ++ "03-test.yaml")
  (_, createAcctCmd) <- mkApiReq (setupPath ++ "04-create.yaml")
  (_, resetTimeCmd)  <- mkApiReq (setupPath ++ "05-reset.yaml")
  (_, runEscrowCmd)  <- mkApiReq (setupPath ++ "06-escrow.yaml")  -- When escrow pact executed
  (_, balanceCmd)    <- mkApiReq (setupPath ++ "07-balance.yaml")
  let allCmds = sysModuleCmd : acctModuleCmd : testModuleCmd : createAcctCmd
                : resetTimeCmd : runEscrowCmd : balanceCmd : testCmds
  allResults <- runAll allCmds

  runResults allResults $ do
    sysModuleCmd `succeedsWith` (`shouldBe` textVal "system module loaded")
    acctModuleCmd `succeedsWith` (`shouldBe` textVal "TableCreated")
    testModuleCmd `succeedsWith` (`shouldBe` textVal "test module loaded")
    succeeds createAcctCmd -- Alice should be funded with $100
    succeeds resetTimeCmd
    succeeds runEscrowCmd
    balanceCmd `succeedsWith` (`shouldBe` decValue 98.00)
    act (_cmdHash runEscrowCmd)

decValue :: Decimal -> PactValue
decValue = PDecimal

checkContHash
  :: HasCallStack
  => [ApiReqParts]
  -> ReaderT (M.Map RequestKey (CommandResult Hash (PactErrorCompat (LocatedErrorInfo Info)))) IO ()
  -> Hash
  -> ReaderT (M.Map RequestKey (CommandResult Hash (PactErrorCompat (LocatedErrorInfo Info)))) IO ()
checkContHash reqs act hsh = forM_ reqs $ \req ->
  let desc = show $ view (_1 . to _ylNonce) req
  in case preview (_1 . to _ylPactTxHash . _Just) req of
    Nothing -> liftIO $ assertFailure $ "Expected pact hash in request: " ++ desc
    Just ph | ph == hsh -> act
            | otherwise -> liftIO $ toAssertionFailure' ("checkContHash for req " ++ desc ++ ": ") ph hsh


testDebtorPreTimeoutCancel :: Assertion
testDebtorPreTimeoutCancel = do
  let testPath = testDir ++ "cont-scripts/fail-deb-cancel-"

  (req, tryCancelCmd)        <- mkApiReq (testPath ++ "01-rollback.yaml")
  (_, checkStillEscrowCmd) <- mkApiReq (testPath ++ "02-balance.yaml")

  let allCmds = [tryCancelCmd, checkStillEscrowCmd]

  twoPartyEscrow allCmds $ checkContHash [req] $ do
    -- pact-5 --explain-error-code 0x0004000000000000
    -- Encountered failure in: PEUserRecoverableError, caused by: UserEnforceError
    -- Printed, it's PEUserRecoverableError (UserEnforceError "Cancel can only be effected by creditor, or debitor after timeout")
    tryCancelCmd `failsWithCode` (ErrorCode 0x0004000000000000)
    checkStillEscrowCmd `succeedsWith` (`shouldBe` decValue 98.00)


testDebtorPostTimeoutCancel :: Assertion
testDebtorPostTimeoutCancel = do
  let testPath = testDir ++ "cont-scripts/pass-deb-cancel-"

  (_, setTimeCmd)          <- mkApiReq (testPath ++ "01-set-time.yaml")
  (req, tryCancelCmd)        <- mkApiReq (testPath ++ "02-rollback.yaml")
  (_, checkStillEscrowCmd) <- mkApiReq (testPath ++ "03-balance.yaml")
  let allCmds = [setTimeCmd, tryCancelCmd, checkStillEscrowCmd]

  twoPartyEscrow allCmds $ checkContHash [req] $ do
    succeeds setTimeCmd
    succeeds tryCancelCmd
    checkStillEscrowCmd `succeedsWith` (`shouldBe` decValue 100.00)


testCreditorCancel :: Assertion
testCreditorCancel = do
  let testPath = testDir ++ "cont-scripts/pass-cred-cancel-"

  (_, resetTimeCmd)        <- mkApiReq (testPath ++ "01-reset.yaml")
  (req, credCancelCmd)       <- mkApiReq (testPath ++ "02-rollback.yaml")
  (_, checkStillEscrowCmd) <- mkApiReq (testPath ++ "03-balance.yaml")
  let allCmds = [resetTimeCmd, credCancelCmd, checkStillEscrowCmd]

  twoPartyEscrow allCmds $ checkContHash [req] $ do
    succeeds resetTimeCmd
    succeeds credCancelCmd
    checkStillEscrowCmd `succeedsWith` (`shouldBe` decValue 100.00)


testFinishAlone :: Assertion
testFinishAlone = do
  let testPathCred  = testDir ++ "cont-scripts/fail-cred-finish-"
      testPathDeb   = testDir ++ "cont-scripts/fail-deb-finish-"

  (r1, tryCredAloneCmd) <- mkApiReq (testPathCred ++ "01-cont.yaml")
  (r2, tryDebAloneCmd)  <- mkApiReq (testPathDeb ++ "01-cont.yaml")
  let allCmds = [tryCredAloneCmd, tryDebAloneCmd]

  -- Both of these tests expect keyset failures
  -- pact-5 --explain-error-code 0x0004040000000000
  -- Encountered failure in: PEUserRecoverableError, caused by: KeysetPredicateFailure
  twoPartyEscrow allCmds $ checkContHash [r1, r2] $ do
    -- PEUserRecoverableError (KeysetPredicateFailure KeysAll (fromList [PublicKeyText {_pubKey = "7d0c9ba189927df85c8c54f8b5c8acd76c1d27e923abbf25a957afdf25550804"}]))
    -- [StackFrame {_sfName = FullyQualifiedName {_fqModule = ModuleName {_mnName = "accounts", _mnNamespace = Nothing}, _fqName = "USER_GUARD", _fqHash = ModuleHash {_mhHash = "bgU2grm5I7_Jyx6Hb93izSWhNDNWcxYlLYOPhNCdIDU"}},
    tryCredAloneCmd `failsWithCode` (ErrorCode 0x0004040000000000)
    -- PEUserRecoverableError (KeysetPredicateFailure KeysAll (fromList [PublicKeyText {_pubKey = "ac69d9856821f11b8e6ca5cdd84a98ec3086493fd6407e74ea9038407ec9eba9"}]))
    tryDebAloneCmd `failsWithCode` (ErrorCode 0x0004040000000000)


testPriceNegUp :: Assertion
testPriceNegUp = do
  let testPath = testDir ++ "cont-scripts/fail-both-price-up-"

  (req, tryNegUpCmd) <- mkApiReq (testPath ++ "01-cont.yaml")
  -- pact-5 --explain-error-code 0x0004000000000000
  -- Encountered failure in: PEUserRecoverableError, caused by: UserEnforceError
  -- PEUserRecoverableError (UserEnforceError "Price cannot negotiate up")
  twoPartyEscrow [tryNegUpCmd] $ checkContHash [req] $ do
    tryNegUpCmd `failsWithCode` (ErrorCode 0x0004000000000000)


testValidEscrowFinish :: Assertion
testValidEscrowFinish = do
  let testPath = testDir ++ "cont-scripts/pass-both-price-down-"

  (req, tryNegDownCmd)  <- mkApiReq (testPath ++ "01-cont.yaml")
  (_, credBalanceCmd) <- mkApiReq (testPath ++ "02-cred-balance.yaml")
  (_, debBalanceCmd)  <- mkApiReq (testPath ++ "03-deb-balance.yaml")
  let allCmds = [tryNegDownCmd, credBalanceCmd, debBalanceCmd]

  twoPartyEscrow allCmds $ checkContHash [req] $ do
    tryNegDownCmd `succeedsWith`
                         (`shouldBe` textVal "Escrow completed with 1.75 paid and 0.25 refunded")
    credBalanceCmd `succeedsWith` (`shouldBe` decValue 1.75)
    debBalanceCmd `succeedsWith` (`shouldBe` decValue 98.25)

testPriceNegDownBadCaps :: Assertion
testPriceNegDownBadCaps = do
  let testPath = testDir ++ "cont-scripts/fail-both-price-down-"

  (req, tryNegUpCmd) <- mkApiReq (testPath ++ "01-cont-badcaps.yaml")
  -- PEUserRecoverableError (KeysetPredicateFailure KeysAll (fromList
  -- [PublicKeyText {_pubKey = "7d0c9ba189927df85c8c54f8b5c8acd76c1d27e923abbf25a957afdf25550804"}]))
  twoPartyEscrow [tryNegUpCmd] $ checkContHash [req] $ do
    tryNegUpCmd `failsWithCode` (ErrorCode 0x0004040000000000)

testVerifiers :: TestTree
testVerifiers = testGroup "using a verifier" $ [
  testCase "should parse and run" $ do
    simpleKeys <- DynEd25519KeyPair <$> generateEd25519KeyPair
    cmd <- mkExec  "(+ 1 2)" PUnit def
      [(simpleKeys,[])]
      [Verifier
        (VerifierName "TESTING-VERIFIER")
        (ParsedVerifierProof $ PDecimal 3)
        [SigCapability (CapToken (QualifiedName "TRANSFER" (ModuleName "coin" Nothing)) [PString "jeff", PDecimal 10])]]
      Nothing (Just "test1")
    allResults <- runAll [cmd]
    runResults allResults $
      succeeds cmd
  ]
--type SigCapability = CapToken QualifiedName PactValue



--- UTILS ---

shouldMatch
    :: HasCallStack
    => Command Text
    -> ExpectResult
    -> ReaderT (M.Map RequestKey (CommandResult Hash (PactErrorCompat (LocatedErrorInfo Info)))) IO ()
shouldMatch cmd er = ask >>= liftIO . shouldMatch' (makeCheck cmd er)

shouldMatch' :: HasCallStack => CommandResultCheck -> M.Map RequestKey (CommandResult Hash (PactErrorCompat (LocatedErrorInfo Info))) -> Assertion
shouldMatch' CommandResultCheck{..} results = checkResult _crcExpect apiRes
  where
    apiRes = M.lookup _crcReqKey results
    checkResult (ExpectResult crTest) result = case result of
      Nothing -> assertFailure $ "Failed to find ApiResult for " ++ show _crcReqKey
      Just cr -> crTest cr

succeeds :: HasCallStack => Command Text ->
                ReaderT (M.Map RequestKey (CommandResult Hash (PactErrorCompat (LocatedErrorInfo Info)))) IO ()
succeeds cmd = cmd `succeedsWith` (\_ -> pure ())

succeedsWith :: HasCallStack => Command Text -> (PactValue -> Assertion) ->
                ReaderT (M.Map RequestKey (CommandResult Hash (PactErrorCompat (LocatedErrorInfo Info)))) IO ()
succeedsWith cmd r = succeedsWith' cmd (\(pv,es) -> (es `shouldBe` []) *> r pv)

succeedsWith' :: HasCallStack => Command Text -> ((PactValue,[PactEvent PactValue]) -> Assertion) ->
                ReaderT (M.Map RequestKey (CommandResult Hash (PactErrorCompat (LocatedErrorInfo Info)))) IO ()
succeedsWith' cmd r = shouldMatch cmd (resultShouldBe $ Right r)

fails :: HasCallStack => Command Text ->
         ReaderT (M.Map RequestKey (CommandResult Hash (PactErrorCompat (LocatedErrorInfo Info)))) IO ()
fails cmd = cmd `failsWith` (\_ -> pure ())

failsWith :: HasCallStack => Command Text -> (PactErrorCode (LocatedErrorInfo Info) -> Assertion) ->
             ReaderT (M.Map RequestKey (CommandResult Hash (PactErrorCompat (LocatedErrorInfo Info)))) IO ()
failsWith cmd r = failsWith' cmd (\e -> r e)

failsWithCode :: HasCallStack => Command Text -> ErrorCode ->
             ReaderT (M.Map RequestKey (CommandResult Hash (PactErrorCompat (LocatedErrorInfo Info)))) IO ()
failsWithCode cmd r = failsWith' cmd ((`shouldBe` r) . _peCode)

shouldBeErrorCode :: HasCallStack => PactErrorCode info -> ErrorCode -> Assertion
shouldBeErrorCode pe code = _peCode pe `shouldBe` code

failsWith' :: HasCallStack => Command Text -> (PactErrorCode (LocatedErrorInfo Info) -> Assertion) ->
             ReaderT (M.Map RequestKey (CommandResult Hash (PactErrorCompat (LocatedErrorInfo Info)))) IO ()
failsWith' cmd r = shouldMatch cmd (resultShouldBe $ Left r)


runResults :: r -> ReaderT r m a -> m a
runResults rs act = runReaderT act rs

makeExecCmd :: DynKeyPair -> Text -> IO (Command Text)
makeExecCmd keyPairs code = makeExecCmd' Nothing keyPairs code

makeExecCmd' :: Maybe Text -> DynKeyPair -> Text -> IO (Command Text)
makeExecCmd' nonce keyPairs code = mkExec code obj def [(keyPairs,[])] [] Nothing nonce
  where
    obj = PObject $ M.fromList [("admin-keyset", PList (V.fromList [formatPubKeyForCmd keyPairs]))]
--  (object ["admin-keyset" .= [formatPubKeyForCmd keyPairs]]) def [(keyPairs,[])] [] Nothing nonce


formatPubKeyForCmd :: DynKeyPair -> PactValue
formatPubKeyForCmd (DynEd25519KeyPair kp) = PString $ toB16Text $ getPublic kp
formatPubKeyForCmd (DynWebAuthnKeyPair _ pub _priv) = PString $ toB16Text $ exportWebAuthnPublicKey pub



makeContCmd
  :: DynKeyPair     -- signing pair
  -> Bool           -- isRollback
  -> PactValue          -- data
  -> Command Text   -- cmd to get pact Id from
  -> Int            -- step
  -> Text           -- nonce
  -> IO (Command Text)
makeContCmd = makeContCmd' Nothing


makeContCmd'
  :: Maybe ContProof
  -> DynKeyPair
      -- ^ signing pair
  -> Bool
      -- ^ isRollback
  -> PactValue
      -- ^ data
  -> Command Text
      -- ^ cmd to get pact Id from
  -> Int
      -- ^ step
  -> Text
      -- ^ nonce
  -> IO (Command Text)
makeContCmd' contProofM keyPairs isRollback cmdData pactExecCmd step nonce =
  mkCont (getDefPactId pactExecCmd) step isRollback cmdData def [(keyPairs,[])] [] (Just nonce) contProofM Nothing

textVal :: Text -> PactValue
textVal = PString

getDefPactId :: Command Text -> DefPactId
getDefPactId cmd = DefPactId $ hashToText hsh
  where hsh = _cmdHash cmd

newtype ExpectResult = ExpectResult (CommandResult Hash (PactErrorCompat (LocatedErrorInfo Info)) -> Assertion)
    deriving (Semigroup)

data CommandResultCheck = CommandResultCheck
  { _crcReqKey :: RequestKey
  , _crcExpect :: ExpectResult
  }


makeCheck :: Command T.Text -> ExpectResult -> CommandResultCheck
makeCheck c@Command{} expect = CommandResultCheck (cmdToRequestKey c) expect

runAll :: [Command T.Text] -> IO (M.Map RequestKey (CommandResult Hash (PactErrorCompat (LocatedErrorInfo Info))))
runAll cmds = runAll' cmds noSPVSupport []

runAll'
  :: [Command T.Text]
  -> SPVSupport
  -> [ExecutionFlag]
  -> IO (M.Map RequestKey (CommandResult Hash (PactErrorCompat (LocatedErrorInfo Info))))
runAll' cmds spv flags =
  withTestPactServerWithSpv "continuationspec" flags spv $ \clientEnv ->
    run clientEnv cmds



run :: ClientEnv -> [Command T.Text] -> IO (M.Map RequestKey (CommandResult Hash (PactErrorCompat (LocatedErrorInfo Info))))
run clientEnv cmds = do
  sendResp <- doSend clientEnv . SendRequest . SubmitBatch $ NEL.fromList cmds
  case sendResp of
    Left servantErr -> Exception.evaluate (error $ show servantErr)
    Right (SendResponse RequestKeys{..}) -> do
      results <- timeout 3000000 (helper _rkRequestKeys)
      case results of
        Nothing -> Exception.evaluate (error "Received empty poll. Timeout in retrying.")
        Just res -> return $ M.fromList (HM.toList res)

  where helper reqKeys = do
          pollResp <- doPoll clientEnv $ PollRequest reqKeys
          case pollResp of
            Left servantErr -> Exception.evaluate (error $ show servantErr)
            Right (PollResponse apiResults) ->
              if null apiResults then helper reqKeys
              else return apiResults


doSend :: ClientEnv -> SendRequest -> IO (Either ClientError SendResponse)
doSend clientEnv req = runClientM (sendClient req) clientEnv

doPoll :: ClientEnv -> PollRequest -> IO (Either ClientError PollResponse)
doPoll clientEnv req = runClientM (pollClient req) clientEnv

resultShouldBe
    :: HasCallStack
    => Either (PactErrorCode (LocatedErrorInfo Info) -> Assertion) ((PactValue,[PactEvent PactValue]) -> Assertion)
    -> ExpectResult
resultShouldBe expect = ExpectResult $ \cr ->
  case (expect, _crResult cr) of
    (Left expErr, PactResultErr e) -> case e of
      PEPact5Error err -> expErr err
      _ -> error "case impossible: no compat errors are emitted by pact-5"
    (Right expVal, PactResultOk pv) -> expVal (pv, _crEvents cr)
    _ -> assertFailure $ unwords
      [ "Expected", either (\_ -> "PactError") (\_ -> "PactValue") expect, ", found", show cr ]

toAssertionFailure' :: (HasCallStack, Show e, Show a) => String -> e -> a -> Assertion
toAssertionFailure' msg expect actual =
  assertFailure $ msg ++ "Expected " ++ show expect ++ ", found " ++ show actual


testFlags :: [ExecutionFlag]
testFlags = []

nestedDefPactFlags :: [ExecutionFlag]
nestedDefPactFlags = []
