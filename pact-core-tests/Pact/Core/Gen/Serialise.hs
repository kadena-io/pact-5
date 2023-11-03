-- | 
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pact.Core.Gen.Serialise where

import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Map.Strict (fromList)
import qualified Data.Set as Set
import Data.Default

import Pact.Core.Names
import Pact.Core.Guards
import Pact.Core.Hash
import Pact.Core.Type
import Pact.Core.Imports
import Pact.Core.IR.Term
import Pact.Core.Info
import Pact.Core.Builtin
import Pact.Core.Literal
import Pact.Core.Capabilities

import qualified Data.ByteString.Short as BSS
import Pact.Core.Test.LexerParserTests (identGen)
import Hedgehog hiding (Var)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.Decimal

namespaceNameGen :: Gen NamespaceName
namespaceNameGen = NamespaceName <$> identGen

moduleNameGen :: Gen ModuleName
moduleNameGen =  do
  name <- identGen
  ModuleName name <$> Gen.maybe namespaceNameGen

publicKeyTextGen :: Gen PublicKeyText
publicKeyTextGen = PublicKeyText <$> identGen

-- ksPredicateGen :: Gen (KSPredicate n)
-- ksPredicateGen = Gen.element [minBound .. maxBound]

keySetNameGen :: Gen KeySetName
keySetNameGen = KeySetName <$> identGen

qualifiedNameGen :: Gen QualifiedName
qualifiedNameGen = do
  name <- identGen
  QualifiedName name <$> moduleNameGen

bareNameGen :: Gen BareName
bareNameGen = BareName <$> identGen

dynamicNameGen :: Gen DynamicName
dynamicNameGen = do
  name <- identGen
  DynamicName name <$> identGen

parsedNameGen :: Gen ParsedName
parsedNameGen = Gen.choice
  [ QN <$> qualifiedNameGen
  , BN <$> bareNameGen
  , DN <$> dynamicNameGen
  ]

hashGen :: Gen Hash
hashGen = Hash . BSS.toShort . encodeUtf8 <$> identGen

moduleHashGen :: Gen ModuleHash
moduleHashGen = ModuleHash <$> hashGen

fullyQualifiedNameGen :: Gen FullyQualifiedName
fullyQualifiedNameGen = do
  modName <- moduleNameGen
  name <- identGen
  FullyQualifiedName modName name <$> moduleHashGen

dynamicRefGen :: Gen DynamicRef
dynamicRefGen = do
  name <- identGen
  DynamicRef name <$> Gen.word64 Range.constantBounded

nameKindGen :: Gen NameKind
nameKindGen = Gen.choice
  [ NBound <$> Gen.word64 Range.constantBounded
  , NTopLevel <$> moduleNameGen <*> moduleHashGen
  , NModRef <$> moduleNameGen <*> Gen.list (Range.linear 0 100) moduleNameGen
  , NDynRef <$> dynamicRefGen
  ]

nameGen :: Gen Name
nameGen = do
  name <- identGen
  Name name <$> nameKindGen

-- TODO
unresolvedGovGen :: Gen (CapGovRef ParsedName)
unresolvedGovGen = UnresolvedGov <$> parsedNameGen

resolvedGovGen :: Gen (CapGovRef Name)
resolvedGovGen = ResolvedGov <$> fullyQualifiedNameGen

governanceGen :: Gen (Governance Name)
governanceGen = Gen.choice
  [ KeyGov <$> keySetNameGen
  , CapGov <$> resolvedGovGen
  ]

tyPrimGen :: Gen PrimType
tyPrimGen = Gen.choice
  [ pure PrimInt
  , pure PrimDecimal
  , pure PrimBool
  , pure PrimString
  , pure PrimGuard
  , pure PrimTime
  , pure PrimUnit
  ]

fieldGen :: Gen Field
fieldGen = Field <$> identGen

schemaGen :: Gen Schema
schemaGen = do
  elems <- Gen.list (Range.linear 0 10) $ (,) <$> fieldGen <*> typeGen
  pure (Schema (fromList elems))

typeGen :: Gen Type
typeGen = Gen.recursive Gen.choice
 [ TyPrim <$> tyPrimGen
 , TyModRef <$> moduleNameGen
 ]
 [ TyList <$> typeGen
 , TyObject <$> schemaGen
 , TyTable <$> schemaGen
 ]

argGen :: Gen (Arg Type)
argGen = do
  name <- identGen
  Arg name <$> Gen.maybe typeGen

importGen :: Gen Import
importGen = do
  mn <- moduleNameGen
  mh <- Gen.maybe moduleHashGen
  imp <- Gen.maybe (Gen.list (Range.linear 0 10) identGen)
  pure (Import mn mh imp)

infoGen :: Gen SpanInfo
infoGen = pure def

builtinGen :: Gen RawBuiltin
builtinGen = Gen.element [minBound .. maxBound]

textGen :: Gen Text
textGen = Gen.text (Range.linear 0 100) Gen.unicode

integerGen :: Gen Integer
integerGen = Gen.integral (Range.linear (-1000) 1000)

decimalGen :: Gen Decimal
decimalGen = do
  places <- Gen.word8 Range.linearBounded
  Decimal places <$> integerGen

literalGen :: Gen Literal
literalGen = Gen.choice
  [ LString <$> textGen
  , LInteger <$> integerGen
  , LDecimal <$> decimalGen
  , pure LUnit
  , LBool <$> Gen.bool_ -- no shrinking
  ]

lamInfoGen :: Gen LamInfo
lamInfoGen = Gen.choice
  [ TLDefun <$> moduleNameGen <*> textGen
  , TLDefCap <$> moduleNameGen <*> textGen
  , TLDefPact <$> moduleNameGen <*> textGen
  , pure AnonLamInfo
  ]

builtinFormGen :: Gen (BuiltinForm (Term Name Type RawBuiltin SpanInfo))
builtinFormGen = Gen.choice
  [ CAnd <$> termGen <*> termGen
  , COr <$> termGen <*> termGen
  , CIf <$> termGen <*> termGen <*> termGen
  , CEnforceOne <$> termGen <*> Gen.list (Range.linear 0 16) termGen
  , CEnforce <$> termGen <*> termGen
  ]

termGen :: Gen (Term Name Type RawBuiltin SpanInfo)
termGen = Gen.recursive Gen.choice
  [ Var <$> nameGen <*> infoGen
  , Builtin <$> builtinGen <*> infoGen
  , Constant <$> literalGen <*> infoGen
  , Error <$> identGen <*> infoGen
  ]
  [ Lam <$> lamInfoGen <*> Gen.nonEmpty (Range.linear 1 16) argGen <*> termGen <*> infoGen
  , Let <$> argGen <*> termGen <*> termGen <*> infoGen
  , App <$> termGen <*> Gen.list (Range.linear 0 16) termGen <*> infoGen
  , Sequence <$> termGen <*> termGen <*> infoGen
  , Nullary <$> termGen <*> infoGen
  , Conditional <$> builtinFormGen <*> infoGen
  , ListLit <$> Gen.list (Range.linear 0 16) termGen <*> infoGen
  , Try <$> termGen <*> termGen <*> infoGen
  , ObjectLit <$> Gen.list (Range.linear 1 16) ((,) <$> fieldGen <*> termGen) <*> infoGen
  ]

defunGen :: Gen (Defun Name Type RawBuiltin SpanInfo)
defunGen = do
  name <- identGen
  args <- Gen.list (Range.linear 0 100) argGen
  ret <- Gen.maybe typeGen
  term <- termGen
  Defun name args ret term <$> infoGen

defConstGen :: Gen (DefConst Name Type RawBuiltin SpanInfo)
defConstGen = do
  name <- identGen
  ty <- Gen.maybe typeGen
  term <- termGen
  DefConst name ty term <$> infoGen

fqNameRefGen :: Gen (FQNameRef Name)
fqNameRefGen = FQName <$> fullyQualifiedNameGen

defManagedMetaGen :: Gen (DefManagedMeta Name)
defManagedMetaGen = Gen.choice
  [ DefManagedMeta <$> Gen.int (Range.linear 0 100) <*> fqNameRefGen
  , pure AutoManagedMeta
  ]

defCapMetaGen :: Gen (DefCapMeta Name)
defCapMetaGen = Gen.choice
  [ pure DefEvent
  , DefManaged <$> defManagedMetaGen
  , pure Unmanaged
  ]

defCapGen :: Gen (DefCap Name Type RawBuiltin SpanInfo)
defCapGen = do
  name <- identGen
  arity <- Gen.int (Range.linear 0 16)
  args <- Gen.list (Range.singleton arity) argGen
  ret <- Gen.maybe typeGen
  term <- termGen
  meta <- defCapMetaGen
  DefCap name arity args ret term meta <$> infoGen

defSchemaGen :: Gen (DefSchema Type SpanInfo)
defSchemaGen = do
  name <- identGen
  schema <- _schema <$> schemaGen
  DefSchema name schema <$> infoGen

defTableGen :: Gen (DefTable Name SpanInfo)
defTableGen = do
  name <- identGen
  schema <- ResolvedTable <$> schemaGen
  DefTable name schema <$> infoGen

stepGen :: Gen (Step Name Type RawBuiltin SpanInfo)
stepGen = Gen.choice
  [ Step <$> termGen <*> mt
  , StepWithRollback <$> termGen <*> termGen <*> mt
  ]
  where
    mt = Gen.maybe (Gen.list (Range.linear 0 16) termGen)

defPactGen :: Gen (DefPact Name Type RawBuiltin SpanInfo)
defPactGen = do
  name <- identGen
  args <- Gen.list (Range.linear 0 16) argGen
  ret <- Gen.maybe typeGen
  steps <- Gen.nonEmpty (Range.linear 0 16) stepGen
  DefPact name args ret steps <$> infoGen

defGen :: Gen (Def Name Type RawBuiltin SpanInfo)
defGen = Gen.choice
  [ Dfun <$> defunGen
  , DConst <$> defConstGen
  , DCap <$> defCapGen
  , DSchema <$> defSchemaGen
  , DTable <$> defTableGen
  , DPact <$> defPactGen
  ]


evalModuleGen :: Gen (EvalModule RawBuiltin SpanInfo)
evalModuleGen = do
  name <- moduleNameGen
  gov <- governanceGen
  defs <- Gen.list (Range.linear 0 100) defGen
  blessed <- Set.fromList <$> Gen.list (Range.linear 0 100) moduleHashGen
  imps <- Gen.list (Range.linear 0 100) importGen
  impl <- Gen.list (Range.linear 0 100) moduleNameGen
  h <- moduleHashGen
  Module name gov defs blessed imps impl h <$> infoGen
