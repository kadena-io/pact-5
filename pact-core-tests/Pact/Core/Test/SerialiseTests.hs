{-# LANGUAGE TypeApplications #-}
-- | 

module Pact.Core.Test.SerialiseTests where

import Pact.Core.Serialise
import Pact.Core.Gen.Serialise
import Pact.Core.Serialise.CBOR_V1 ()
import qualified Codec.Serialise as S

import Pact.Core.Builtin
import Pact.Core.Info

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog
import Hedgehog (Gen, Property, (===), forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

serialiseRoundtrip :: forall a. (S.Serialise a, Show a, Eq a) => Gen a -> Property
serialiseRoundtrip g = property $ do
  expr <- forAll g
  S.deserialise (S.serialise expr) === expr

documentFormatGen :: Gen DocumentFormat
documentFormatGen = Gen.element [minBound .. maxBound]

documentVersionGen :: Gen DocumentVersion
documentVersionGen = DocumentVersion <$> Gen.word32 (Range.linear 0 100)

documentGen :: Gen a -> Gen (Document a)
documentGen g = Document <$> documentVersionGen <*> documentFormatGen <*> g

serialiseModule :: Property
serialiseModule = property $ do
  m <- forAll (moduleDataGen builtinGen (pure ()))
  let
    encoded = _encodeModuleData serialiseCBOR m
  case _decodeModuleData serialiseCBOR encoded of
    Left _ -> fail "asas"
    Right (Document v f c) -> do
      v === DocumentVersion 0
      f === DocumentCBOR
      m === c


serialiseKeySet :: Property
serialiseKeySet = property $ do
  ks <- forAll (keySetGen fullyQualifiedNameGen)
  let
    encoded = _encodeKeySet serialiseCBOR ks
  case _decodeKeySet serialiseCBOR encoded of
    Left _ -> fail "asas"
    Right (Document v f c) -> do
      v === DocumentVersion 0
      f === DocumentCBOR
      ks === c

serialiseDefPactExec :: Property
serialiseDefPactExec = property $ do
  dpe <- forAll (Gen.maybe defPactExecGen)
  let
    encoded = _encodeDefPactExec serialiseCBOR dpe
  case _decodeDefPactExec serialiseCBOR encoded of
    Left _ -> fail "asas"
    Right (Document v f c) -> do
      v === DocumentVersion 0
      f === DocumentCBOR
      dpe === c

tests :: TestTree
tests = testGroup "Serialise Roundtrip"
  [ testGroup "Document"
    [ testProperty "DocumentFormat" $ serialiseRoundtrip documentFormatGen
    , testProperty "DocumentVersion" $ serialiseRoundtrip documentVersionGen
--    , testProperty "Document" $ serialiseRoundtrip (documentGen (Gen.constant ()))
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
