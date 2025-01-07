module Pact.Core.Test.TypecheckerTests where


import Test.Tasty
import Test.Tasty.HUnit

import Control.Lens
import Control.Monad
import Data.IORef
import Data.Default
import System.Directory
import System.FilePath

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S
import qualified Data.Text.IO as T

import Pact.Core.Persistence.MockPersistence

import Pact.Core.Repl.Utils
import Pact.Core.Environment
import Pact.Core.Builtin
import Pact.Core.Errors
import Pact.Core.Serialise
import Pact.Core.Repl
import Pact.Core.Repl.Compile
import Pact.Core.Names
import Pact.Core.Repl.BuiltinDocs.Internal(builtinToNormalizedName)
import Pact.Core.Typed.Infer
import Pact.Core.Typed.Type
import Data.Text (Text)


tests :: IO TestTree
tests = do
  bad <- tcBadTests
  pure $ testGroup "Static Typechecker"
    [ testGroup "Native tc tests - direct" (testNativeTypechecks interpretEvalDirect <$> [minBound .. maxBound])
    , testGroup "Native tc tests - CEK" (testNativeTypechecks interpretEvalBigStep <$> [minBound .. maxBound])
    , testGroup "TC failures - direct" (bad interpretEvalDirect)
    , testGroup "TC failures - cek" (bad interpretEvalBigStep)
    ]

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
    CoreBind ->
      [ ("use-bind", NotIndexed $ NonGeneric $ TyString :~> TyInt)
      ]
    -- Note: the require-capability,
    -- compose-capability, install-capability, emit-event
    CoreRequireCapability ->
      [ ("FOO", NotIndexed $ NonGeneric $ TyInt :~> TyCapToken)
      , ("BAR", NotIndexed $ NonGeneric $ TyNullary TyCapToken)
      , ("EMITTED", NotIndexed $ NonGeneric $ TyString :~> TyCapToken)
      , ("foo-mgr", NotIndexed $ NonGeneric $ TyInt :~> TyInt :~> TyInt)
      , ("needs-foo", NotIndexed $ NonGeneric $ TyInt :~> TyInt)
      , ("use-foo", NotIndexed $ NonGeneric $ TyInt :~> TyInt)
      ]
    CoreComposeCapability ->
      [ ("FOO", NotIndexed $ NonGeneric $ TyInt :~> TyCapToken)
      , ("BAR", NotIndexed $ NonGeneric $ TyNullary TyCapToken)
      , ("EMITTED", NotIndexed $ NonGeneric $ TyString :~> TyCapToken)
      , ("foo-mgr", NotIndexed $ NonGeneric $ TyInt :~> TyInt :~> TyInt)
      , ("needs-foo", NotIndexed $ NonGeneric $ TyInt :~> TyInt)
      , ("use-foo", NotIndexed $ NonGeneric $ TyInt :~> TyInt)
      ]
    CoreInstallCapability ->
      [ ("FOO", NotIndexed $ NonGeneric $ TyInt :~> TyCapToken)
      , ("BAR", NotIndexed $ NonGeneric $ TyNullary TyCapToken)
      , ("EMITTED", NotIndexed $ NonGeneric $ TyString :~> TyCapToken)
      , ("foo-mgr", NotIndexed $ NonGeneric $ TyInt :~> TyInt :~> TyInt)
      , ("needs-foo", NotIndexed $ NonGeneric $ TyInt :~> TyInt)
      , ("use-foo", NotIndexed $ NonGeneric $ TyInt :~> TyInt)
      ]
    CoreEmitEvent ->
      [ ("FOO", NotIndexed $ NonGeneric $ TyInt :~> TyCapToken)
      , ("BAR", NotIndexed $ NonGeneric $ TyNullary TyCapToken)
      , ("EMITTED", NotIndexed $ NonGeneric $ TyString :~> TyCapToken)
      , ("foo-mgr", NotIndexed $ NonGeneric $ TyInt :~> TyInt :~> TyInt)
      , ("needs-foo", NotIndexed $ NonGeneric $ TyInt :~> TyInt)
      , ("use-foo", NotIndexed $ NonGeneric $ TyInt :~> TyInt)
      ]
    CoreCreateCapabilityGuard ->
      [ ("FOO", NotIndexed $ NonGeneric $ TyString :~> TyCapToken)
      , ("use-create-cap-guard", NotIndexed $ NonGeneric $ TyString :~> TyGuard)]
    CoreCreateCapabilityPactGuard ->
      [ ("FOO", NotIndexed $ NonGeneric $ TyString :~> TyCapToken)
      , ("use-create-cap-pact-guard", NotIndexed $ NonGeneric $ TyString :~> TyGuard)]
    CoreCreateModuleGuard ->
      [ ("use-create-module-guard", NotIndexed $ NonGeneric $ TyString :~> TyGuard)]
    CoreCreateDefPactGuard ->
      [ ("use-create-pact-guard", NotIndexed $ NonGeneric $ TyString :~> TyGuard)]
    CoreCreateTable ->
      [ ("use-create-table", NotIndexed $ NonGeneric $ TyNullary TyString)
      , ("foo-tbl", NotIndexed $ NonGeneric $ TyTable (RowConcrete scm))]
      where
      scm = M.fromList
        [ (Field "a", TyInt)
        , (Field "b", TyInt)]
    CoreDescribeKeyset ->
      [ ("use-describe-keyset", NotIndexed $ NonGeneric $ TyString :~> TyGuard)]
    CoreDescribeModule ->
      [ ("use-describe-module", NotIndexed $ NonGeneric $ TyString :~> TyObject (RowConcrete scm))]
      where
      scm =  M.fromList
        [(Field "hash", TyString)
        ,(Field "interfaces", TyList TyString)
        ,(Field "name", TyString)
        ,(Field "code", TyString)
        ,(Field "tx-hash", TyString)
        ]
    CoreDescribeTable ->
      let tv = RowVariable 0 "a"
      in [ ("use-describe-table", NotIndexed $ TypeScheme [tv] [] (TyTable (RowVar tv) :~> TyObject (RowConcrete scm))) ]
      where
      scm =  M.fromList
        [(Field "module", TyString)
        ,(Field "name", TyString)
        ,(Field "type", TyString)
        ]
    CoreDefineKeySet ->
      [ ("use-define-keyset", NotIndexed $ NonGeneric $ TyString :~> TyString)]
    CoreDefineKeysetData ->
      [ ("use-define-keyset-env-data", NotIndexed $ NonGeneric $ TyString :~> TyString)]
    CoreFoldDb ->
      [ ("use-fold-db", NotIndexed $ TypeScheme [rv] [] (TyTable (RowVar rv) :~> TyList (TyObject (RowConcrete scm))))]
      where
      scm = M.fromList
        [ (Field "key", TyString)
        , (Field "object", TyObject (RowVar rv))]
      rv = RowVariable 0 "a"
    CoreInsert ->
      [ ("use-insert", NotIndexed $ TypeScheme [rv] [] (TyTable (RowVar rv) :~> TyObject (RowVar rv) :~> TyString))]
      where
      rv = RowVariable 0 "a"
    CoreKeys ->
      [ ("use-keys", NotIndexed $ TypeScheme [rv] [] (TyTable (RowVar rv) :~> TyList TyString))]
      where
      rv = RowVariable 0 "a"
    CoreRead ->
      [ ("use-read", NotIndexed $ TypeScheme [rv] [] (TyTable (RowVar rv) :~> TyObject (RowVar rv)))]
      where
      rv = RowVariable 0 "a"
    CoreSelect ->
      [ ("use-select", NotIndexed $ TypeScheme [rv] [constr] (TyTable (RowVar rv) :~> TyList (TyObject (RowVar rv))))
      , ("tbl1", NotIndexed $ NonGeneric $ TyTable (RowConcrete sc1))
      , ("tbl2", NotIndexed $ NonGeneric $ TyTable (RowConcrete sc2))
      , ("invoke-select", NotIndexed $ NonGeneric $ TyNullary TyUnit)
      ]
      where
      constr = RoseSubRow (RoseConcrete subRowScm) (RoseVar rv)
      subRowScm = M.fromList
        [ (Field "foo", TyInt)
        ]
      sc1 = subRowScm
      sc2 = M.insert (Field "bar") TyString sc1
      rv = RowVariable 0 "a"
    CoreSelectWithFields ->
      [ ("use-select", NotIndexed $ TypeScheme [rv, tv] [constr1, constr2] (TyTable (RowVar rv) :~> TyList (TyObject (RowConcrete outScm))))
      , ("tbl1", NotIndexed $ NonGeneric $ TyTable (RowConcrete sc1))
      , ("tbl2", NotIndexed $ NonGeneric $ TyTable (RowConcrete sc2))
      , ("invoke-select", NotIndexed $ NonGeneric $ TyNullary TyUnit)
      ]
      where
      constr1 = RoseSubRow (RoseConcrete subRowScm) (RoseVar rv)
      constr2 = RoseSubRow (RoseConcrete (M.singleton (Field "bar") (TyVar tv))) (RoseVar rv)
      outScm = M.singleton (Field "bar") (TyVar tv)
      subRowScm = M.fromList
        [ (Field "foo", TyInt)
        ]
      sc1 = M.fromList
        [ (Field "foo", TyInt)
        , (Field "bar", TyBool)]
      sc2 = M.fromList
        [ (Field "foo", TyInt)
        , (Field "bar", TyString)]
      rv = RowVariable 1 "row"
      tv = TypeVariable 0 "a"
    CoreUpdate ->
      [ ("use-update", NotIndexed $ TypeScheme [rv] [constr1] (TyTable (RowVar rv) :~> TyString))
      , ("tbl1", NotIndexed $ NonGeneric $ TyTable (RowConcrete sc1))
      , ("tbl2", NotIndexed $ NonGeneric $ TyTable (RowConcrete sc2))
      , ("invoke-update", NotIndexed $ NonGeneric $ TyNullary TyUnit)
      ]
      where
      constr1 = RoseSubRow (RoseConcrete subRowScm) (RoseVar rv)
      subRowScm = M.fromList
        [ (Field "foo", TyInt)
        ]
      sc1 = M.fromList
        [ (Field "foo", TyInt)
        , (Field "bar", TyBool)]
      sc2 = M.fromList
        [ (Field "foo", TyInt)
        , (Field "bar", TyString)]
      rv = RowVariable 0 "row"
    CoreWithDefaultRead ->
      [ ("use-with-default-read", NotIndexed $ TypeScheme [rv] [constr1] (TyTable (RowVar rv) :~> TyObject (RowVar rv) :~> TyInt))
      , ("tbl1", NotIndexed $ NonGeneric $ TyTable (RowConcrete sc1))
      , ("tbl2", NotIndexed $ NonGeneric $ TyTable (RowConcrete sc2))
      , ("invoke-with-default-read", NotIndexed $ NonGeneric $ TyNullary TyInt) ]
      where
      constr1 = RoseSubRow (RoseConcrete subRowScm) (RoseVar rv)
      subRowScm = M.fromList
        [ (Field "foo", TyInt)
        ]
      sc1 = M.fromList
        [ (Field "foo", TyInt)
        , (Field "bar", TyBool)]
      sc2 = M.fromList
        [ (Field "foo", TyInt)
        , (Field "bar", TyString)]
      rv = RowVariable 0 "row"
    CoreWithRead ->
      [ ("use-with-read", NotIndexed $ TypeScheme [rv] [constr1] (TyTable (RowVar rv) :~> TyInt))
      , ("tbl1", NotIndexed $ NonGeneric $ TyTable (RowConcrete sc1))
      , ("tbl2", NotIndexed $ NonGeneric $ TyTable (RowConcrete sc2))
      , ("invoke-with-read", NotIndexed $ NonGeneric $ TyNullary TyInt) ]
      where
      constr1 = RoseSubRow (RoseConcrete subRowScm) (RoseVar rv)
      subRowScm = M.fromList
        [ (Field "foo", TyInt)
        ]
      sc1 = M.fromList
        [ (Field "foo", TyInt)
        , (Field "bar", TyBool)]
      sc2 = M.fromList
        [ (Field "foo", TyInt)
        , (Field "bar", TyString)]
      rv = RowVariable 0 "row"
    CoreWrite ->
      [ ("use-write", NotIndexed $ TypeScheme [] [] (TyTable (RowConcrete sc1) :~> TyString))
      , ("tbl1", NotIndexed $ NonGeneric $ TyTable (RowConcrete sc1))
      , ("tbl2", NotIndexed $ NonGeneric $ TyTable (RowConcrete sc1))
      , ("invoke-write", NotIndexed $ NonGeneric $ TyNullary TyString) ]
      where
      sc1 = M.fromList
        [ (Field "foo", TyInt)
        , (Field "bar", TyBool)]
    CoreTxHash ->
      [ ("use-tx-hash", NotIndexed $ NonGeneric $ TyString :~> TyBool) ]
    CoreAndQ ->
      [ ("use-andq", NotIndexed $ NonGeneric $ TyInt :~> TyBool)
      ]
    CoreOrQ ->
      [ ("use-orq", NotIndexed $ NonGeneric $ TyInt :~> TyBool)
      ]
    CoreWhere ->
      [ ("use-where", NotIndexed $ TypeScheme [rv] [constr] (TyObject (RowVar rv) :~> TyBool) ) ]
      where
      rv = RowVariable 0 "a"
      constr = RoseSubRow (RoseConcrete (M.singleton (Field "foo") TyString)) (RoseVar rv)
    CoreNotQ ->
      [ ("use-notq", NotIndexed $ NonGeneric $ TyInt :~> TyBool)
      ]
    CoreHash ->
      [ ("use-hash", NotIndexed $ NonGeneric $ TyNullary TyString)]
    CoreContinue ->
      [ ("use-continue", NotIndexed $ TypeScheme [tv] [] (TyVar tv :~> TyVar tv))]
      where
      tv = TypeVariable 0 "a"
    CoreParseTime ->
      [ ("use-parse-time", NotIndexed $ NonGeneric $ TyString :~> TyString :~> TyTime) ]
    CoreFormatTime ->
      [ ("use-format-time", NotIndexed $ NonGeneric $ TyString :~> TyTime :~> TyString) ]
    CoreTime ->
      [ ("use-time", NotIndexed $ NonGeneric $ TyString :~> TyTime) ]
    CoreAddTime ->
      [ ("use-add-time", NotIndexed $ NonGeneric $ TyNullary TyTime) ]
    CoreDiffTime ->
      [ ("use-diff-time", NotIndexed $ NonGeneric $ TyNullary TyDecimal) ]
    CoreHours ->
      [ ("use-hours", NotIndexed $ NonGeneric $ TyNullary TyDecimal) ]
    CoreMinutes ->
      [ ("use-minutes", NotIndexed $ NonGeneric $ TyNullary TyDecimal) ]
    CoreDays ->
      [ ("use-days", NotIndexed $ NonGeneric $ TyNullary TyDecimal) ]
    CoreCompose ->
      [ ("use-compose", NotIndexed $ NonGeneric $ TyNullary TyInt) ]
    CoreCreatePrincipal ->
      [ ("use-create-principal", NotIndexed $ NonGeneric $ TyNullary TyString) ]
    CoreIsPrincipal ->
      [ ("use-is-principal", NotIndexed $ NonGeneric $ TyNullary TyBool) ]
    CoreTypeOfPrincipal ->
      [ ("use-typeof-principal", NotIndexed $ NonGeneric $ TyNullary TyString) ]
    CoreValidatePrincipal ->
      [ ("use-validate-principal", NotIndexed $ NonGeneric $ TyString :~> TyBool) ]
    CoreNamespace ->
      [ ("use-namespace", NotIndexed $ NonGeneric $ TyString :~> TyString) ]
    CoreDefineNamespace ->
      [ ("use-define-namespace", NotIndexed $ NonGeneric $ TyString :~> TyString) ]
    CoreDescribeNamespace ->
      [ ("use-describe-namespace", NotIndexed $ NonGeneric $ TyString :~> TyObject (RowConcrete schema)) ]
      where
      schema = M.fromList
        [ (Field "admin-guard", TyGuard)
        , (Field "namespace-name", TyString)
        , (Field "user-guard", TyGuard)]
    CoreChainData ->
      [ ("use-chain-data", NotIndexed $ NonGeneric $ TyNullary $ TyObject (RowConcrete schema)) ]
      where
      schema = M.fromList
        [ (Field "chain-id", TyString)
        , (Field "block-height", TyInt)
        , (Field "block-time", TyTime)
        , (Field "prev-block-hash", TyString)
        , (Field "sender", TyString)
        , (Field "gas-limit", TyInt)
        , (Field "gas-price", TyDecimal)]
    CoreIsCharset ->
      [ ("use-is-charset", NotIndexed $ NonGeneric $ TyNullary TyBool) ]
    CorePactId ->
      [ ("use-pact-id", NotIndexed $ NonGeneric $ TyNullary TyString) ]
    CoreZkPairingCheck ->
      [ ("use-pairing-check", NotIndexed $ NonGeneric $ TyNullary TyBool) ]
    CoreZKScalarMult ->
      [ ("use-scalar-mult", NotIndexed $ NonGeneric $ TyNullary TyBool) ]
    CoreZkPointAdd ->
      [ ("use-point-add", NotIndexed $ NonGeneric $ TyNullary TyBool) ]
    CorePoseidonHashHackachain ->
      [ ("use-poseidon", NotIndexed $ NonGeneric $ TyNullary TyInt) ]
    CoreTypeOf ->
      [ ("use-typeof", NotIndexed $ NonGeneric $ TyNullary TyString) ]
    CoreDec ->
       [ ("use-dec", NotIndexed $ NonGeneric $ TyNullary TyDecimal) ]
    CoreCond ->
       [ ("use-cond", NotIndexed $ NonGeneric $ TyNullary TyString) ]
    CoreIdentity ->
      [ ("use-identity", NotIndexed $ TypeScheme [tv] [] (TyVar tv :~> TyVar tv))]
      where
      tv = TypeVariable 0 "a"
    -- Unsure how to properly test this function...
    CoreVerifySPV -> []
    CoreEnforceVerifier ->
       [ ("MOCK", NotIndexed $ NonGeneric $ TyNullary TyCapToken) ]
    CoreAcquireModuleAdmin ->
       [ ("use-acquire", NotIndexed $ NonGeneric $ TyNullary TyString) ]
    CoreHyperlaneMessageId ->
      [ ("use-hyperlane-message-id", NotIndexed $ NonGeneric $ TyNullary TyString) ]
    CoreHyperlaneDecodeMessage ->
      [ ("use-hdtm", NotIndexed $ NonGeneric $ TyNullary (TyObject (RowConcrete schema))) ]
      where
      schema = M.fromList
        [ (Field "recipient", TyGuard)
        , (Field "amount", TyDecimal)
        , (Field "chaindId", TyString)
        ]
    CoreHyperlaneEncodeMessage ->
      [ ("use-hetm", NotIndexed $ NonGeneric $ TyNullary TyString) ]
    CoreReadWithFields ->
      [ ("use-read", NotIndexed $ TypeScheme [rv, tv2, tv1] [constr] (TyTable (RowVar rv) :~> TyObject (RowConcrete outSchema)))]
      where
      constr = RoseSubRow (RoseConcrete outSchema) (RoseVar rv)
      outSchema = M.fromList
        [ (Field "foo", TyVar tv1)
        , (Field "bar", TyVar tv2)]
      tv2 = TypeVariable 1 "b"
      tv1 = TypeVariable 0 "a"
      rv = RowVariable 2 "row"
    CoreListModules ->
      [ ("use-list-modules", NotIndexed $ NonGeneric $ TyNullary $ TyList TyString) ]
    CoreStaticRedeploy ->
      [ ("use-static-redeploy", NotIndexed $ NonGeneric $ TyNullary TyUnit) ]
    CoreHashKeccak256 ->
      [ ("use-keccak256", NotIndexed $ NonGeneric $ TyNullary TyString) ]
    CoreHashPoseidon ->
      [ ("use-poseidon", NotIndexed $ NonGeneric $ TyNullary TyInt) ]

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
    assertEqual ("Type inference mismatch for " <> T.unpack l1) texpected tactual
  (NativeName nn) = builtinName b
  goTC src = do
    _ <- interpretReplProgram interp src
    typecheckModule def (ModuleName "tc" Nothing) >>= \case
      Left tcErr -> renderTypecheckError tcErr >>= (throwExecutionError def . TypecheckingFailure (ModuleName "tc" Nothing))
      Right (_, r) -> pure $ M.mapKeys _fqName r

tcBadTests :: IO (ReplInterpreter -> [TestTree])
tcBadTests = do
  files <- listDirectory badTestsDir
  pure (\interp -> testTCBad interp <$> files)

testTCBad :: ReplInterpreter -> FilePath -> TestTree
testTCBad interp fp = testCase ("Typecheck bad test for " <> fp) $ do
  pdb <- mockPactDb serialisePact_repl_fileLocSpanInfo
  ee <- defaultEvalEnv pdb replBuiltinMap
  let file = badTestsDir </> fp
  fileSrc <- T.readFile file
  let source = SourceCode file fileSrc
  let rstate = mkReplState ee (const (const (pure ()))) (\f reset -> void (loadFile interp f reset)) & replCurrSource .~ source
  stateRef <- newIORef rstate
  evalReplM stateRef (goTC source) >>= \case
    Left (PEExecutionError TypecheckingFailure{} _ _) -> pure ()
    Left e -> do
      rstate' <- readIORef stateRef
      let rendered = renderLocatedPactErrorFromState rstate' e
      assertFailure (T.unpack rendered)
    Right r -> do
        assertFailure $ "Typechecking succeeded where it was expected to fail " <> show r
  where
  goTC src = do
    _ <- interpretReplProgram interp src
    typecheckModule def (ModuleName "tc" Nothing) >>= \case
      Left tcErr -> renderTypecheckError tcErr >>= (throwExecutionError def . TypecheckingFailure (ModuleName "tc" Nothing))
      Right (_, r) -> pure $ M.mapKeys _fqName r