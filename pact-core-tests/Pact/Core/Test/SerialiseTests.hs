-- | 

module Pact.Core.Test.SerialiseTests where

import Pact.Core.Gen.Serialise
import Pact.Core.Serialise.CBOR ()
import Codec.Serialise

import Test.Tasty
import Test.Tasty.Hedgehog
import Hedgehog

serialiseRoundtrip :: forall a. (Serialise a, Show a, Eq a) => Gen a -> Property
serialiseRoundtrip g = property $ do
  expr <- forAll g
  deserialise (serialise expr) === expr


tests :: TestTree
tests = testGroup "Serialise Roundtrip"
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
  -- , testProperty "unresolvedGov" $ serialiseRoundtrip unresolvedGovGen
  -- , testProperty "resolvedGov" $ serialiseRoundtrip resolvedGovGen
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
  , testProperty "BuiltinForm" $ serialiseRoundtrip builtinFormGen
  , testProperty "Term" $ serialiseRoundtrip termGen
  , testProperty "Defun" $ serialiseRoundtrip defunGen
  ]
