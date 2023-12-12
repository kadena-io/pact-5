-- |

module Pact.Core.Test.SerialiseTests where

import Pact.Core.Serialise
import Pact.Core.Gen.Serialise
import Pact.Core.Serialise.CBOR_V1 ()
import qualified Codec.Serialise as S
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog
import Hedgehog (Gen, Property, (===), forAll, property)
import qualified Hedgehog.Gen as Gen
import Codec.CBOR.Read (deserialiseFromBytes)
import Data.ByteString (fromStrict)
import Codec.CBOR.Write (toStrictByteString)

serialiseRoundtrip :: forall a. (S.Serialise a, Show a, Eq a) => Gen a -> Property
serialiseRoundtrip g = property $ do
  expr <- forAll g
  S.deserialise (S.serialise expr) === expr

documentVersionGen :: Gen DocumentVersion
documentVersionGen = Gen.element [minBound .. maxBound]

documentGen :: Gen a -> Gen (Document a)
documentGen g = Gen.choice
  [ Document <$> documentVersionGen <*> g
  , LegacyDocument <$> g
  ]

serialiseModule :: Property
serialiseModule = property $ do
  m <- forAll (moduleDataGen builtinGen (pure ()))
  let
    encoded = _encodeModuleData serialisePact m
  case _decodeModuleData serialisePact encoded of
    Just (Document v c) -> do
      v === V1_CBOR
      m === c
    _ -> fail "fail"


serialiseKeySet :: Property
serialiseKeySet = property $ do
  ks <- forAll (keySetGen fullyQualifiedNameGen)
  let
    encoded = _encodeKeySet serialisePact ks
  case _decodeKeySet serialisePact encoded of
    Just (Document v c) -> do
      v === V1_CBOR
      ks === c
    _ -> fail "fail"

serialiseDefPactExec :: Property
serialiseDefPactExec = property $ do
  dpe <- forAll (Gen.maybe defPactExecGen)
  let
    encoded = _encodeDefPactExec serialisePact dpe
  case _decodeDefPactExec serialisePact encoded of
    Just (Document v c) -> do
      v === V1_CBOR
      dpe === c
    _ -> fail "fail"


serialiseRoundtripVersion :: Property
serialiseRoundtripVersion = property $ do
  v <- forAll documentVersionGen
  let
    encoded = toStrictByteString (encodeVersion v)
  case deserialiseFromBytes decodeVersion (fromStrict encoded) of
    Left _ -> fail "fail"
    Right (_, v') -> v === v'


tests :: TestTree
tests = testGroup "Serialise Roundtrip"
  [ testGroup "Document"
    [ testProperty "DocumentVersion" serialiseRoundtripVersion
    ]
  , testGroup "CBOR"
    [ testProperty "NamespaceName" $ serialiseRoundtrip namespaceNameGen
    , testProperty "ModuleName" $ serialiseRoundtrip moduleNameGen
    , testProperty "KeySetName" $ serialiseRoundtrip keySetNameGen
    , testProperty "QualifiedName" $ serialiseRoundtrip qualifiedNameGen
    , testProperty "BareName" $ serialiseRoundtrip bareNameGen
    , testProperty "DynamicName" $ serialiseRoundtrip dynamicNameGen
    , testProperty "ParsedName" $ serialiseRoundtrip parsedNameGen
    , testProperty "Hash" $ serialiseRoundtrip hashGen
    , testProperty "ModuleHash" $ serialiseRoundtrip moduleHashGen
    , testProperty "FullyQualifiedName" $ serialiseRoundtrip fullyQualifiedNameGen
    , testProperty "DynamicRef" $ serialiseRoundtrip dynamicRefGen
    , testProperty "NameKind" $ serialiseRoundtrip nameKindGen
    , testProperty "Name" $ serialiseRoundtrip nameGen
    , testProperty "resolvedGov" $ serialiseRoundtrip resolvedGovGen
    , testProperty "Governance" $ serialiseRoundtrip governanceGen
    , testProperty "PrimType" $ serialiseRoundtrip tyPrimGen
    , testProperty "Field" $ serialiseRoundtrip fieldGen
    , testProperty "Schema" $ serialiseRoundtrip schemaGen
    , testProperty "Types" $ serialiseRoundtrip typeGen
    , testProperty "Arg" $ serialiseRoundtrip argGen
    , testProperty "Import" $ serialiseRoundtrip importGen
    , testProperty "SpanInfo" $ serialiseRoundtrip infoGen
    , testProperty "Builtin" $ serialiseRoundtrip builtinGen
    , testProperty "Literal" $ serialiseRoundtrip literalGen
    , testProperty "LamInfo" $ serialiseRoundtrip lamInfoGen
    , testProperty "BuiltinForm" $ serialiseRoundtrip (builtinFormGen builtinGen infoGen)
    , testProperty "Term" $ serialiseRoundtrip (termGen builtinGen infoGen)
    , testProperty "Defun" $ serialiseRoundtrip (defunGen builtinGen infoGen)
    , testProperty "DefConst" $ serialiseRoundtrip (defConstGen builtinGen infoGen)
    , testProperty "FQNameRef" $ serialiseRoundtrip fqNameRefGen
    , testProperty "DefManagedMeta" $ serialiseRoundtrip defManagedMetaGen
    , testProperty "DefCapMeta" $ serialiseRoundtrip defCapMetaGen
    , testProperty "DefCap" $ serialiseRoundtrip (defCapGen builtinGen infoGen)
    , testProperty "Def" $ serialiseRoundtrip (defGen builtinGen infoGen)
    , testProperty "Module" $ serialiseRoundtrip (evalModuleGen builtinGen infoGen)
    , testProperty "DefSchema" $ serialiseRoundtrip (defSchemaGen infoGen)
    , testProperty "DefTable" $ serialiseRoundtrip (defTableGen infoGen)
    , testProperty "Step" $ serialiseRoundtrip (stepGen builtinGen infoGen)
    , testProperty "DefPact" $ serialiseRoundtrip (defPactGen builtinGen infoGen)
    -- , testProperty "ReplBuiltins" $ serialiseRoundtrip replBuiltinsGen
    -- , testProperty "ReplRawBuiltin" $ serialiseRoundtrip replRawBuiltinGen
    ],
    testGroup "CBOR Serialise"
      [ testProperty "KeySet roundtrip" serialiseKeySet
      , testProperty "Module roundtrip" serialiseModule
      , testProperty "DefPactExec roundtrip" serialiseDefPactExec
      ]
  ]