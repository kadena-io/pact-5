module Pact.Core.Test.TypecheckerTests where


import Test.Tasty
import Test.Tasty.HUnit

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.IORef
import Data.Default
import Data.Foldable(traverse_)
import System.Directory
import System.FilePath

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S
import qualified Data.Text.IO as T

import Pact.Core.Persistence.MockPersistence

import Pact.Core.Repl.Utils
import Pact.Core.Persistence.SQLite (withSqlitePactDb)

import Pact.Core.Info
import Pact.Core.Environment
import Pact.Core.Builtin
import Pact.Core.Errors
import Pact.Core.Serialise
import Pact.Core.Persistence
import Pact.Core.IR.Term
import Pact.Core.Repl
import Pact.Core.Repl.Compile
import Pact.Core.Test.ReplTestUtils
import qualified Pact.Core.IR.ModuleHashing as MH
import Pact.Core.Names
import Pact.Core.Repl.BuiltinDocs.Internal(builtinToNormalizedName)
import Pact.Core.Typed.Infer
import Pact.Core.Typed.Type
import Data.Text (Text)


tests :: TestTree
tests = do
  testGroup "Static Typechecker" [
    testGroup "Native tc tests" (testNativeTypechecks interpretEvalDirect <$> [CoreAdd .. CoreResume])]
    -- [ testGroup "in-memory db:bigstep" (runFileReplTest interpretEvalBigStep <$> files)
    -- , testGroup "sqlite db:bigstep" (runFileReplTestSqlite interpretEvalBigStep <$> files)
    -- , testGroup "in-memory db:direct" (runFileReplTest interpretEvalDirect <$> files)
    -- , testGroup "sqlite db:direct" (runFileReplTestSqlite interpretEvalDirect <$> files)
    -- ]


tcTestsDir :: FilePath
tcTestsDir = "pact-tests" </> "typechecker"

nativesTestDir :: FilePath
nativesTestDir = tcTestsDir </> "natives"

unitTestsDir :: FilePath
unitTestsDir = tcTestsDir </> "unit-tests"

badTestsDir :: FilePath
badTestsDir = tcTestsDir </> "bad"

defaultReplTestDir :: FilePath
defaultReplTestDir = "pact-tests" </> "pact-tests"


replTestFiles :: IO [FilePath]
replTestFiles = filter (\f -> isExtensionOf "repl" f || isExtensionOf "pact" f) <$> getDirectoryContents defaultReplTestDir

expectedNativeTypes :: CoreBuiltin -> M.Map Text (DefnType (TypeVar NamedDeBruijn))
expectedNativeTypes = M.fromList . go
  where
  go = \case
    CoreAdd ->
      [ ("add-integers", NotIndexed (NonGeneric (TyInt :~> TyInt :~> TyInt)))
      , ("add-decimals", NotIndexed $ NonGeneric $ TyDecimal :~> TyDecimal :~> TyDecimal)
      , ("add-strings", NotIndexed $ NonGeneric $ TyString :~> TyString :~> TyString)
      , ("add-lists", NotIndexed $ NonGeneric $ TyList TyInt :~> TyList TyInt :~> TyList TyInt)
      , ("add-objs", NotIndexed $ NonGeneric $ TyObject (RowConcrete sc1) :~> TyObject (RowConcrete sc2) :~> TyObject (RowConcrete (sc1 <> sc2)))
      ]
      where
      sc1 = M.singleton (Field "a") TyInt
      sc2 = M.singleton (Field "b") TyString
    CoreSub ->
      [ ("sub-integers", NotIndexed $ NonGeneric $ TyInt :~> TyInt :~> TyInt)
      , ("sub-decimals", NotIndexed $ NonGeneric $ TyDecimal :~> TyDecimal :~> TyDecimal)
      ]
    CoreMultiply ->
      [ ("mult-integers", NotIndexed $ NonGeneric $ TyInt :~> TyInt :~> TyInt)
      , ("mult-decimals", NotIndexed $ NonGeneric $ TyDecimal :~> TyDecimal :~> TyDecimal)
      ]
    CoreDivide ->
      [ ("div-integers", NotIndexed $ NonGeneric $ TyInt :~> TyInt :~> TyInt)
      , ("div-decimals", NotIndexed $ NonGeneric $ TyDecimal :~> TyDecimal :~> TyDecimal)
      ]
    CoreNegate ->
      [ ("negate-integers", NotIndexed $ NonGeneric $ TyInt :~> TyInt)
      , ("negate-decimals", NotIndexed $ NonGeneric $ TyDecimal :~> TyDecimal)
      , ("negate-generic", NotIndexed $ genSig)
      , ("use-negate-generic", NotIndexed $ NonGeneric $ TyNullary TyDecimal)
      ]
      where
      genSig = TypeScheme [tv] [Num (TyVar tv)] (TyVar tv :~> TyVar tv)
      tv = TypeVariable 0 "a"
    CoreAbs ->
      [ ("abs-integers", NotIndexed $ NonGeneric $ TyInt :~> TyInt)
      , ("abs-decimals", NotIndexed $ NonGeneric $ TyDecimal :~> TyDecimal)
      , ("abs-generic", NotIndexed $ genSig)
      , ("use-abs", NotIndexed $ NonGeneric $ TyNullary TyDecimal)
      ]
      where
      genSig = TypeScheme [tv] [Num (TyVar tv)] (TyVar tv :~> TyVar tv)
      tv = TypeVariable 0 "a"
    CorePow ->
      [ ("pow-integers", NotIndexed $ NonGeneric $ TyInt :~> TyInt :~> TyInt)
      , ("pow-decimals", NotIndexed $ NonGeneric $ TyDecimal :~> TyDecimal :~> TyDecimal)
      ]
    CoreNot ->
      [ ("tc-not", NotIndexed $ NonGeneric $ TyBool :~> TyBool)
      ]
    CoreEq ->
      [ ("use-eq", NotIndexed $ NonGeneric $ TyNullary TyBool)]
    CoreNeq ->
      [ ("use-neq", NotIndexed $ NonGeneric $ TyNullary TyBool)]
    CoreGT ->
      [ ("use-gt", NotIndexed $ NonGeneric $ TyNullary TyBool)]
    CoreGEQ ->
      [ ("use-geq", NotIndexed $ NonGeneric $ TyNullary TyBool)]
    CoreLT ->
      [ ("use-lt", NotIndexed $ NonGeneric $ TyNullary TyBool)]
    CoreLEQ ->
      [ ("use-leq", NotIndexed $ NonGeneric $ TyNullary TyBool)]
    CoreBitwiseAnd ->
      [ ("use-bw-and", NotIndexed $ NonGeneric $ TyInt :~> TyInt)]
    CoreBitwiseOr ->
      [ ("use-bw-or", NotIndexed $ NonGeneric $ TyInt :~> TyInt)]
    CoreBitwiseXor ->
      [ ("use-bw-xor", NotIndexed $ NonGeneric $ TyInt :~> TyInt)]
    CoreBitwiseFlip ->
      [ ("use-bw-flip", NotIndexed $ NonGeneric $ TyInt :~> TyInt)]
    CoreBitShift ->
      [ ("use-shift", NotIndexed $ NonGeneric $ TyInt :~> TyInt)]
    CoreRound ->
      [ ("use-round", NotIndexed $ NonGeneric $ TyDecimal :~> TyInt)]
    CoreCeiling ->
      [ ("use-ceiling", NotIndexed $ NonGeneric $ TyDecimal :~> TyInt)]
    CoreFloor ->
      [ ("use-floor", NotIndexed $ NonGeneric $ TyDecimal :~> TyInt)]
    CoreRoundPrec ->
      [ ("use-round-prec", NotIndexed $ NonGeneric $ TyDecimal :~> TyDecimal)]
    CoreCeilingPrec ->
      [ ("use-ceiling-prec", NotIndexed $ NonGeneric $ TyDecimal :~> TyDecimal)]
    CoreFloorPrec ->
      [ ("use-floor-prec", NotIndexed $ NonGeneric $ TyDecimal :~> TyDecimal)]
    CoreExp ->
      [ ("exp-integer", NotIndexed $ NonGeneric $ TyInt :~> TyDecimal)
      , ("exp-decimal", NotIndexed $ NonGeneric $ TyDecimal :~> TyDecimal)
      ]
    CoreLn ->
      [ ("ln-integer", NotIndexed $ NonGeneric $ TyInt :~> TyDecimal)
      , ("ln-decimal", NotIndexed $ NonGeneric $ TyDecimal :~> TyDecimal)
      ]
    CoreSqrt ->
      [ ("sqrt-integer", NotIndexed $ NonGeneric $ TyInt :~> TyDecimal)
      , ("sqrt-decimal", NotIndexed $ NonGeneric $ TyDecimal :~> TyDecimal)
      ]
    CoreLogBase ->
      [ ("log-integer", NotIndexed $ NonGeneric $ TyInt :~> TyInt)
      , ("log-decimal", NotIndexed $ NonGeneric $ TyDecimal :~> TyDecimal)
      ]
    CoreLength ->
      [ ("length-string", NotIndexed $ NonGeneric $ TyString :~> TyInt)
      , ("length-list", NotIndexed $ NonGeneric $ TyList TyInt :~> TyInt)
      ]
    CoreTake ->
      [ ("take-string", NotIndexed $ NonGeneric $ TyString :~> TyString)
      , ("take-list", NotIndexed $ NonGeneric $ TyList TyInt :~> TyList TyInt)
      ]
    CoreDrop ->
      [ ("drop-string", NotIndexed $ NonGeneric $ TyString :~> TyString)
      , ("drop-list", NotIndexed $ NonGeneric $ TyList TyInt :~> TyList TyInt)
      ]
    CoreConcat ->
      [ ("concat-string", NotIndexed $ NonGeneric $ TyList TyString :~> TyString)
      ]
    CoreReverse ->
      [ ("reverse-string", NotIndexed $ NonGeneric $ TyString :~> TyString)
      , ("reverse-list", NotIndexed $ NonGeneric $ TyList TyString :~> TyList TyString)
      ]
    CoreContains ->
      [ ("contains-string", NotIndexed $ NonGeneric $ TyString :~> TyBool)
      , ("contains-list", NotIndexed $ NonGeneric $ TyString :~> TyList TyString :~> TyBool)
      ]
    CoreSort ->
      [ ("use-sort", NotIndexed $ NonGeneric $ TyNullary (TyList TyBool))
      ]
    CoreSortObject ->
      [ ("use-sort-obj", NotIndexed $ NonGeneric $ TyNullary (TyList (TyObject (RowConcrete scm))))
      ]
      where
      scm = M.fromList
        [ (Field "foo", TyInt)
        , (Field "bar", TyInt)]
    CoreRemove ->
      [ ("use-remove", NotIndexed $ NonGeneric $ TyObject (RowConcrete scm) :~> TyObject (RowConcrete removed))
      ]
      where
      removed = M.delete (Field "foo") scm
      scm = M.fromList
        [ (Field "foo", TyInt)
        , (Field "bar", TyString)]
    CoreMod ->
      [ ("use-mod", NotIndexed $ NonGeneric $ TyInt :~> TyInt :~> TyInt)
      ]
    CoreMap ->
      [ ("use-map", NotIndexed $ NonGeneric $ TyNullary (TyList TyInt))
      , ("FOO", NotIndexed $ NonGeneric $ TyInt)
      ]
    CoreFilter ->
      [ ("use-filter", NotIndexed $ NonGeneric $ TyNullary (TyList TyInt))
      ]
    CoreZip ->
      [ ("use-zip", NotIndexed $ NonGeneric $ TyNullary (TyList TyDecimal))
      ]
    CoreIntToStr ->
      [ ("use-int-to-str", NotIndexed $ NonGeneric $ TyInt :~> TyInt :~> TyString)
      ]
    CoreStrToInt ->
      [ ("use-str-to-int", NotIndexed $ NonGeneric $ TyString :~> TyInt)
      ]
    CoreStrToIntBase ->
      [ ("use-str-to-int-base", NotIndexed $ NonGeneric $ TyInt :~> TyString :~> TyInt)
      ]
    CoreFold ->
      [ ("use-fold", NotIndexed $ NonGeneric $ TyInt :~> TyInt)
      ]
    CoreDistinct ->
      [ ("use-distinct", NotIndexed $ NonGeneric $ TyNullary (TyList (TyList TyInt)))
      , ("foo-tbl", NotIndexed $ NonGeneric $ TyTable (RowConcrete scm))
      ]
      where
      scm = M.singleton (Field "m") (TyModRef (MConcrete (S.singleton (ModuleName "foobar" Nothing))))
    CoreFormat ->
      [ ("use-format", NotIndexed $ NonGeneric $ TyNullary TyString)
      , ("foo-tbl", NotIndexed $ NonGeneric $ TyTable (RowConcrete scm))
      ]
      where
      scm = M.singleton (Field "m") (TyModRef (MConcrete (S.singleton (ModuleName "foobar" Nothing))))
    CoreEnumerate ->
      [ ("use-enumerate", NotIndexed $ NonGeneric $ TyInt :~> TyInt :~> TyList TyInt)
      ]
    CoreEnumerateStepN ->
      [ ("use-enumerate-step", NotIndexed $ NonGeneric $ TyInt :~> TyInt :~> TyInt :~> TyList TyInt)
      ]
    CoreShow ->
      [ ("show-integer", NotIndexed $ NonGeneric $ TyInt :~> TyString)
      , ("show-decimal", NotIndexed $ NonGeneric $ TyDecimal :~> TyString)
      , ("show-string", NotIndexed $ NonGeneric $ TyString :~> TyString)
      , ("show-list", NotIndexed $ NonGeneric $ TyList TyInt :~> TyString)
      , ("show-obj", NotIndexed $ NonGeneric $ TyObject (RowConcrete obj1scm) :~> TyString)
      , ("show-modref", NotIndexed $ NonGeneric $ TyModRef (MConcrete (S.singleton (ModuleName "foobar" Nothing))) :~> TyString)
      , ("show-table", NotIndexed $ NonGeneric $ TyTable (RowConcrete obj1scm) :~> TyString)
      , ("show-guard", NotIndexed $ NonGeneric $ TyGuard :~> TyString)
      , ("show-unit", NotIndexed $ NonGeneric $ TyUnit :~> TyString)
      ]
      where
      obj1scm = M.singleton (Field "a") TyInt
    CoreReadMsg ->
      [ ("use-read-msg", NotIndexed $ NonGeneric $ TyNullary TyString)
      ]
    CoreReadMsgDefault ->
      [ ("use-read-msg-default", NotIndexed $ NonGeneric $ TyNullary TyString)
      ]
    CoreReadInteger ->
      [ ("use-read-integer", NotIndexed $ NonGeneric $ TyNullary TyInt)
      ]
    CoreReadDecimal ->
      [ ("use-read-decimal", NotIndexed $ NonGeneric $ TyNullary TyDecimal)
      ]
    CoreReadString ->
      [ ("use-read-string", NotIndexed $ NonGeneric $ TyNullary TyString)
      ]
    CoreReadKeyset ->
      [ ("use-read-keyset", NotIndexed $ NonGeneric $ TyNullary TyGuard)
      ]
    CoreEnforceGuard ->
      [ ("use-enforce-guard", NotIndexed $ NonGeneric $ TyNullary TyBool)
      ]
    CoreEnforceKeyset ->
      [ ("use-enforce-keyset", NotIndexed $ NonGeneric $ TyNullary TyBool)
      ]
    CoreKeysetRefGuard ->
      [ ("use-keyset-ref-guard", NotIndexed $ NonGeneric $ TyString :~> TyGuard)
      ]
    CoreAt ->
      [ ("use-at-obj", NotIndexed $ NonGeneric $ TyObject (RowConcrete scm) :~> TyInt)
      , ("use-at-list", NotIndexed $ NonGeneric $ TyList (TyObject (RowConcrete scm)) :~> TyInt)
      ]
      where
      scm = M.fromList
        [ (Field "foo", TyInt)
        , (Field "bar", TyInt)
        ]
    CoreMakeList ->
      let a = TypeVariable 0 "a"
      in [ ("use-make-list", NotIndexed $ TypeScheme [a] [] (TyInt :~> TyVar a :~> TyList (TyVar a)))
      ]
    CoreB64Encode ->
      [ ("use-base64-encode", NotIndexed $ NonGeneric $ TyString :~> TyString)
      ]
    CoreB64Decode ->
      [ ("use-base64-decode", NotIndexed $ NonGeneric $ TyString :~> TyString)
      ]
    CoreStrToList ->
      [ ("use-str-to-list", NotIndexed $ NonGeneric $ TyString :~> TyList TyString)
      ]
    CoreYield ->
      [ ("use-yield", IndexedDefpactStepType fns)]
      where
      fns = IM.fromList
        [ (0, TyString :~> TyObject (RowConcrete scm))
        , (1, TyString :~> TyObject (RowConcrete scm2))
        , (2, TyString :~> TyInt)
        ]
      scm = M.singleton (Field "a") TyString
      scm2 = M.singleton (Field "b") TyString
    CoreYieldToChain ->
      [ ("use-yield-to-chain", IndexedDefpactStepType fns)]
      where
      fns = IM.fromList
        [ (0, TyString :~> TyObject (RowConcrete scm))
        , (1, TyString :~> TyObject (RowConcrete scm2))
        , (2, TyString :~> TyInt)
        ]
      scm = M.singleton (Field "a") TyString
      scm2 = M.singleton (Field "b") TyString
    CoreResume ->
      [ ("use-resume", IndexedDefpactStepType fns)]
      where
      fns = IM.fromList
        [ (0, TyString :~> TyObject (RowConcrete scm))
        , (1, TyString :~> TyObject (RowConcrete scm2))
        , (2, TyString :~> TyInt)
        ]
      scm = M.singleton (Field "a") TyString
      scm2 = M.singleton (Field "b") TyString
    CoreBind -> []
    CoreRequireCapability -> []
    CoreComposeCapability -> []
    CoreInstallCapability -> []
    CoreEmitEvent -> []
    CoreCreateCapabilityGuard -> []
    CoreCreateCapabilityPactGuard -> []
    CoreCreateModuleGuard -> []
    CoreCreateDefPactGuard -> []
    CoreCreateTable -> []
    CoreDescribeKeyset -> []
    CoreDescribeModule -> []
    CoreDescribeTable -> []
    CoreDefineKeySet -> []
    CoreDefineKeysetData -> []
    CoreFoldDb -> []
    CoreInsert -> []
    CoreKeys -> []
    CoreRead -> []
    CoreSelect -> []
    CoreSelectWithFields -> []
    CoreUpdate -> []
    CoreWithDefaultRead -> []
    CoreWithRead -> []
    CoreWrite -> []
    CoreTxHash -> []
    CoreAndQ -> []
    CoreOrQ -> []
    CoreWhere -> []
    CoreNotQ -> []
    CoreHash -> []
    CoreContinue -> []
    CoreParseTime -> []
    CoreFormatTime -> []
    CoreTime -> []
    CoreAddTime -> []
    CoreDiffTime -> []
    CoreHours -> []
    CoreMinutes -> []
    CoreDays -> []
    CoreCompose -> []
    CoreCreatePrincipal -> []
    CoreIsPrincipal -> []
    CoreTypeOfPrincipal -> []
    CoreValidatePrincipal -> []
    CoreNamespace -> []
    CoreDefineNamespace -> []
    CoreDescribeNamespace -> []
    CoreChainData -> []
    CoreIsCharset -> []
    CorePactId -> []
    CoreZkPairingCheck -> []
    CoreZKScalarMult -> []
    CoreZkPointAdd -> []
    CorePoseidonHashHackachain -> []
    CoreTypeOf -> []
    CoreDec -> []
    CoreCond -> []
    CoreIdentity -> []
    CoreVerifySPV -> []
    CoreEnforceVerifier -> []
    CoreAcquireModuleAdmin -> []
    CoreHyperlaneMessageId -> []
    CoreHyperlaneDecodeMessage -> []
    CoreHyperlaneEncodeMessage -> []
    CoreReadWithFields -> []
    CoreListModules -> []
    CoreStaticRedeploy -> []
    CoreHashKeccak256 -> []
    CoreHashPoseidon -> []

testNativeTypechecks :: ReplInterpreter -> CoreBuiltin -> TestTree
testNativeTypechecks interp b = testCase ("Typechecker unit test for " <> T.unpack nn) $ do
  pdb <- mockPactDb serialisePact_repl_fileLocSpanInfo
  ee <- defaultEvalEnv pdb replBuiltinMap
  let file = nativesTestDir </> T.unpack (builtinToNormalizedName nn) <> ".repl"
  fileSrc <- T.readFile file
  let source = SourceCode file fileSrc
  let rstate = mkReplState ee (const (const (pure ()))) (\f reset -> void (loadFile interp f reset)) & replCurrSource .~ source
  stateRef <- newIORef rstate
  evalReplM stateRef (goTC source) >>= \case
    Left e -> do
      rstate' <- readIORef stateRef
      let rendered = renderLocatedPactErrorFromState rstate' e
      assertFailure (T.unpack rendered)
    Right ks -> do
      -- Remove the gov cap, always named g
      let actualInferred = M.delete "g" ks
      let expectedTys = expectedNativeTypes b
          expectedKeys = M.keysSet expectedTys
          actualKeys = M.keysSet actualInferred
      when (expectedKeys /= actualKeys) $ do
        assertFailure $ "Typechecking did not produce the expected definition(s):" <> show (S.difference expectedKeys actualKeys)
      zipWithM_ expectSameType (M.toList expectedTys) (M.elems actualInferred)
  where
  expectSameType (l1, texpected) tactual =
    assertEqual ("Type inferred for " <> T.unpack l1 <> "is not what was expected") texpected tactual
  (NativeName nn) = builtinName b
  goTC src = do
    _ <- interpretReplProgram interp src
    typecheckModule def (ModuleName "tc" Nothing) >>= \case
      Left tcErr -> renderTypecheckError tcErr >>= (throwExecutionError def . TypecheckingFailure (ModuleName "tc" Nothing))
      Right (_, r) -> pure $ M.mapKeys _fqName r



-- runFileReplTest :: ReplInterpreter -> TestName -> TestTree
-- runFileReplTest interp file = testCase file $ do
--   pdb <- mockPactDb serialisePact_repl_fileLocSpanInfo
--   src <- T.readFile (defaultReplTestDir </> file)
--   runReplTest (ReplSourceDir defaultReplTestDir) pdb file src interp


-- runFileReplTestSqlite :: ReplInterpreter -> TestName -> TestTree
-- runFileReplTestSqlite interp file = testCase file $ do
--   ctnt <- T.readFile (defaultReplTestDir </> file)
--   withSqlitePactDb serialisePact_repl_fileLocSpanInfo ":memory:" $ \pdb -> do
--     runReplTest (ReplSourceDir defaultReplTestDir) pdb file ctnt interp