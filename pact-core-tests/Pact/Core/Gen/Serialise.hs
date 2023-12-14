-- | 
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NamedFieldPuns #-}

module Pact.Core.Gen.Serialise where

import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Map.Strict (fromList)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Vector as Vec
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
import Pact.Core.Persistence
import Pact.Core.PactValue
import Pact.Core.DefPacts.Types
import Pact.Core.ChainData
import Pact.Core.Namespace

import qualified Data.ByteString.Short as BSS
import Pact.Core.Test.LexerParserTests (identGen)
import Hedgehog hiding (Var)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.Decimal

namespaceNameGen :: Gen NamespaceName
namespaceNameGen = NamespaceName <$> identGen

namespaceGen :: Gen Namespace
namespaceGen = do
  name <- namespaceNameGen
  user <- guardGen 3 fullyQualifiedNameGen
  Namespace name user <$> guardGen 3 fullyQualifiedNameGen

moduleNameGen :: Gen ModuleName
moduleNameGen =  do
  name <- identGen
  ModuleName name <$> Gen.maybe namespaceNameGen

publicKeyTextGen :: Gen PublicKeyText
publicKeyTextGen = PublicKeyText <$> identGen

-- ksPredicateGen :: Gen (KSPredicate n)
-- ksPredicateGen = Gen.element [minBound .. maxBound]

keySetNameGen :: Gen KeySetName
keySetNameGen = KeySetName <$> identGen <*> Gen.maybe namespaceNameGen

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

keySetGen :: Gen a -> Gen (KeySet a)
keySetGen _genA = do
  ksKeysList <- Gen.list (Range.linear 1 10) publicKeyTextGen
  let _ksKeys = Set.fromList ksKeysList
  -- customPredicate <- CustomPredicate <$> genA
  _ksPredFun <- Gen.choice
    [ pure KeysAll
    , pure Keys2
    , pure KeysAny
    -- , customPredicate -- TODO: Reinstantiate this when CustomPredicate is brought back into Guard.hs.
    ]
  pure $ KeySet { _ksKeys, _ksPredFun }

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
 , TyModRef <$> Gen.set (Range.linear 0 10) moduleNameGen
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

builtinFormGen :: Gen b -> Gen i -> Gen (BuiltinForm (Term Name Type b i))
builtinFormGen b i = Gen.choice
  [ CAnd <$> termGen b i <*> termGen b i
  , COr <$> termGen b i <*> termGen b i
  , CIf <$> termGen b i <*> termGen b i <*> termGen b i
  , CEnforceOne <$> termGen b i <*> Gen.list (Range.linear 0 16) (termGen b i)
  , CEnforce <$> termGen b i <*> termGen b i
  ]

termGen :: Gen b -> Gen i -> Gen (Term Name Type b i)
termGen b i = Gen.recursive Gen.choice
  [ Var <$> nameGen <*> i
  , Builtin <$> b <*> i
  , Constant <$> literalGen <*> i
  , Error <$> identGen <*> i
  ]
  [ Lam <$> lamInfoGen <*> Gen.nonEmpty (Range.linear 1 16) argGen <*> termGen b i <*> i
  , Let <$> argGen <*> termGen b i <*> termGen b i <*> i
  , App <$> termGen b i <*> Gen.list (Range.linear 0 16) (termGen b i) <*> i
  , Sequence <$> termGen b i <*> termGen b i <*> i
  , Nullary <$> termGen b i <*> i
  , Conditional <$> builtinFormGen b i <*> i
  , ListLit <$> Gen.list (Range.linear 0 16) (termGen b i)<*> i
  , Try <$> termGen b i <*> termGen b i <*> i
  , ObjectLit <$> Gen.list (Range.linear 1 16) ((,) <$> fieldGen <*> termGen b i) <*> i
  ]

defunGen :: Gen b -> Gen i -> Gen (Defun Name Type b i)
defunGen b i = do
  name <- identGen
  args <- Gen.list (Range.linear 0 100) argGen
  ret <- Gen.maybe typeGen
  term <- termGen b i
  Defun name args ret term <$> i

ifDefunGen ::Gen i -> Gen (IfDefun Type i)
ifDefunGen i = do
  name <- identGen
  args <- Gen.list (Range.linear 0 100) argGen
  ret <- Gen.maybe typeGen
  IfDefun name args ret <$> i

defConstGen :: Gen b -> Gen i -> Gen (DefConst Name Type b i)
defConstGen b i = do
  name <- identGen
  ty <- Gen.maybe typeGen
  cval <- constValGen (termGen b i)
  DefConst name ty cval <$> i

constValGen :: Gen (Term name ty b i) -> Gen (ConstVal (Term name ty b i))
constValGen t = Gen.choice
  [ TermConst <$> t
  , EvaledConst <$> pactValueGen
  ]

fqNameRefGen :: Gen (FQNameRef Name)
fqNameRefGen = FQName <$> fullyQualifiedNameGen

-- defManagedMetaGen :: Gen name -> Gen (DefManagedMeta name)
-- defManagedMetaGen genName = Gen.choice
--   [ DefManagedMeta <$> liftA2 (,) (Gen.int (Range.linear 0 100)) genText <*> genName
--   , pure AutoManagedMeta
--   ]

defManagedMetaGen :: Gen name -> Gen (DefManagedMeta name)
defManagedMetaGen genName = Gen.choice
  [ DefManagedMeta
    <$> liftA2
          (,)
          (Gen.int (Range.linear 0 100))
          (Gen.text (Range.linear 0 100) Gen.latin1)
    <*> genName
  , pure AutoManagedMeta
  ]

defCapMetaGen :: Gen name -> Gen (DefCapMeta name)
defCapMetaGen genName = Gen.choice
  [ pure DefEvent
  , DefManaged <$> defManagedMetaGen genName
  , pure Unmanaged
  ]

defCapGen :: Gen b -> Gen i -> Gen (DefCap Name Type b i)
defCapGen b i = do
  name <- identGen
  arity <- Gen.int (Range.linear 0 16)
  args <- Gen.list (Range.singleton arity) argGen
  ret <- Gen.maybe typeGen
  term <- termGen b i
  meta <- defCapMetaGen fqNameRefGen
  DefCap name arity args ret term meta <$> i

ifDefCapGen :: Gen i -> Gen (IfDefCap name Type i)
ifDefCapGen i = do
  name <- identGen
  args <- Gen.list (Range.linear 1 8) argGen
  ret <- Gen.maybe typeGen
  meta <- defCapMetaGen bareNameGen
  IfDefCap name args ret meta <$> i

defSchemaGen :: Gen i -> Gen (DefSchema Type i)
defSchemaGen i = do
  name <- identGen
  schema <- _schema <$> schemaGen
  DefSchema name schema <$> i

defTableGen :: Gen i -> Gen (DefTable Name i)
defTableGen i = do
  name <- identGen
  schema <- ResolvedTable <$> schemaGen
  DefTable name schema <$> i

stepGen :: Gen b -> Gen i -> Gen (Step Name Type b i)
stepGen b i = Gen.choice
  [ Step <$> termGen b i
  , StepWithRollback <$> termGen b i <*> termGen b i
  ]

defPactGen :: Gen b -> Gen i -> Gen (DefPact Name Type b i)
defPactGen b i = do
  name <- identGen
  args <- Gen.list (Range.linear 0 16) argGen
  ret <- Gen.maybe typeGen
  steps <- Gen.nonEmpty (Range.linear 0 16) (stepGen b i)
  DefPact name args ret steps <$> i


ifDefPactGen :: Gen i -> Gen (IfDefPact Type i)
ifDefPactGen i = do
  name <- identGen
  args <- Gen.list (Range.linear 0 16) argGen
  ret <- Gen.maybe typeGen
  IfDefPact name args ret <$> i


defGen :: Gen b -> Gen i -> Gen (Def Name Type b i)
defGen b i = Gen.choice
  [ Dfun <$> defunGen b i
  , DConst <$> defConstGen b i
  , DCap <$> defCapGen b i
  , DSchema <$> defSchemaGen i
  , DTable <$> defTableGen i
  , DPact <$> defPactGen b i
  ]


evalModuleGen :: Gen b -> Gen i -> Gen (EvalModule b i)
evalModuleGen b i= do
  name <- moduleNameGen
  gov <- governanceGen
  defs <- Gen.list (Range.linear 0 100) (defGen b i)
  blessed <- Set.fromList <$> Gen.list (Range.linear 0 100) moduleHashGen
  imps <- Gen.list (Range.linear 0 100) importGen
  impl <- Gen.list (Range.linear 0 100) moduleNameGen
  h <- moduleHashGen
  Module name gov defs blessed imps impl h <$> i

ifDefGen :: Gen b -> Gen i -> Gen (IfDef Name Type b i)
ifDefGen b i = Gen.choice
  [ IfDfun <$> ifDefunGen i
  , IfDConst <$> defConstGen b i
  , IfDCap <$> ifDefCapGen i
  , IfDSchema <$> defSchemaGen i
  , IfDPact <$> ifDefPactGen i
  ]

evalInterfaceGen :: Gen b -> Gen i -> Gen (EvalInterface b i)
evalInterfaceGen b i = do
  name <- moduleNameGen
  defs <- Gen.list (Range.linear 0 100) (ifDefGen b i)
  imports <- Gen.list (Range.linear 0 100) (importGen )
  h <- moduleHashGen
  Interface name defs imports h <$> i

moduleDataGen :: Gen b -> Gen i -> Gen (ModuleData b i)
moduleDataGen b i = Gen.choice
  [ ModuleData <$> evalModuleGen b i<*> m
  , InterfaceData <$> evalInterfaceGen b i<*> m
  ]
  where
    m = Gen.map (Range.linear 0 8) $ (,) <$> fullyQualifiedNameGen <*> defGen b i


defPactIdGen :: Gen DefPactId
defPactIdGen = DefPactId <$> identGen

userGuardGen :: Int -> Gen (UserGuard FullyQualifiedName PactValue)
userGuardGen depth = do
  ident <- fullyQualifiedNameGen
  UserGuard ident <$> Gen.list (Range.linear 0 depth) pactValueGen

guardGen :: Int -> Gen n -> Gen (Guard n PactValue)
guardGen depth n
  | depth <= 0 = Gen.choice [gKeySetGen, gKeySetRefGen]
  | otherwise  = Gen.choice [gKeySetGen, gKeySetRefGen]
  where
    gKeySetGen = GKeyset <$> keySetGen n
    gKeySetRefGen = GKeySetRef <$> keySetNameGen
--    gUserGuardGen = GUserGuard <$> userGuardGen (depth - 1)

pactValueGen :: Gen PactValue
pactValueGen = do
  i <- Gen.int (Range.linear 0 8)
  pactValueGen' i

pactValueGen' :: Int ->Gen PactValue
pactValueGen' depth = Gen.choice
  [ PLiteral <$> literalGen
  , PList . Vec.fromList <$> Gen.list (Range.linear 0 depth) (pactValueGen' (depth - 1))
  , PGuard <$> guardGen (depth - 1) fullyQualifiedNameGen
  ]

chainIdGen :: Gen ChainId
chainIdGen = ChainId <$> identGen

provenanceGen :: Gen Provenance
provenanceGen = do
  chainId <- chainIdGen
  Provenance chainId <$> moduleHashGen

yieldGen :: Gen Yield
yieldGen = do
  d <- Gen.map (Range.linear 0 16) ((,) <$> fieldGen <*> pactValueGen)
  p <- Gen.maybe provenanceGen
  Yield d p <$> Gen.maybe chainIdGen

defPactContinuationGen :: Gen (DefPactContinuation FullyQualifiedName PactValue)
defPactContinuationGen = do
  ident <- fullyQualifiedNameGen
  DefPactContinuation ident <$> Gen.list (Range.linear 0 8) pactValueGen

defPactExecGen :: Gen DefPactExec
defPactExecGen = do
  i <- Gen.int (Range.linear 0 3)
  defPactExecGen' i

defPactExecGen' :: Int -> Gen DefPactExec
defPactExecGen' depth = do
  sc <- Gen.int (Range.linear 1 16)
  yield <- Gen.maybe yieldGen
  step <- Gen.int (Range.linear 1 sc)
  dpid <- defPactIdGen
  cont <- defPactContinuationGen
  nested <- if depth <= 0
            then pure Map.empty
            else Gen.map (Range.linear 0 3) genNested
  rb <- Gen.bool
  pure (DefPactExec sc yield step dpid cont rb nested)
  where
    genNested = do
      dpid <- defPactIdGen
      pexec <- defPactExecGen' (depth - 1)
      pure (dpid, pexec)
