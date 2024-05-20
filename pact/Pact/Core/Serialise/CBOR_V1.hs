-- |
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pact.Core.Serialise.CBOR_V1
  ( encodeModuleData, decodeModuleData
  , encodeModuleData_repl_spaninfo, decodeModuleData_repl_spaninfo
  , encodeModuleData_raw_spaninfo, decodeModuleData_raw_spaninfo
  , encodeKeySet, decodeKeySet
  , encodeDefPactExec, decodeDefPactExec
  , encodeNamespace, decodeNamespace
  , encodeRowData, decodeRowData
  ) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except
import Codec.Serialise.Class
import Codec.CBOR.Encoding
import Codec.CBOR.Decoding
import Data.Decimal
import Data.Foldable
import Data.IORef
import qualified Data.Text as Text

import Pact.Core.Names
import Pact.Core.Persistence
import Pact.Core.IR.Term
import Pact.Core.Gas
import Pact.Core.Guards
import Pact.Core.Hash
import Pact.Core.Type
import Pact.Core.Literal
import Pact.Core.Capabilities
import Pact.Core.Builtin
import Pact.Core.Imports
import Pact.Core.Info
import Pact.Core.DefPacts.Types
import Pact.Core.PactValue
import Pact.Core.ModRefs
import Pact.Core.ChainData
import Pact.Core.Namespace
import Pact.Time.Internal (UTCTime(..), NominalDiffTime(..))
import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Write (toStrictByteString)
import Data.ByteString (ByteString, fromStrict)
import qualified Data.Map as Map
import Pact.Core.Errors

encodeModuleData :: ModuleData CoreBuiltin () -> ByteString
encodeModuleData = toStrictByteString . encode

encodeModuleData_repl_spaninfo :: ModuleData ReplCoreBuiltin SpanInfo -> ByteString
encodeModuleData_repl_spaninfo = toStrictByteString . encode

encodeModuleData_raw_spaninfo :: ModuleData CoreBuiltin SpanInfo -> ByteString
encodeModuleData_raw_spaninfo = toStrictByteString . encode

decodeModuleData :: ByteString -> Maybe (ModuleData CoreBuiltin ())
decodeModuleData bs = either (const Nothing) (Just . snd) (deserialiseFromBytes decode (fromStrict bs))

decodeModuleData_repl_spaninfo :: ByteString -> Maybe (ModuleData ReplCoreBuiltin SpanInfo)
decodeModuleData_repl_spaninfo bs = either (const Nothing) (Just . snd) (deserialiseFromBytes decode (fromStrict bs))

decodeModuleData_raw_spaninfo :: ByteString -> Maybe (ModuleData CoreBuiltin SpanInfo)
decodeModuleData_raw_spaninfo bs = either (const Nothing) (Just . snd) (deserialiseFromBytes decode (fromStrict bs))

encodeKeySet :: KeySet -> ByteString
encodeKeySet = toStrictByteString . encode

decodeKeySet :: ByteString -> Maybe KeySet
decodeKeySet bs = either (const Nothing) (Just . snd) (deserialiseFromBytes decode (fromStrict bs))


encodeDefPactExec :: Maybe DefPactExec -> ByteString
encodeDefPactExec =  toStrictByteString . encode

decodeDefPactExec :: ByteString -> Maybe (Maybe DefPactExec)
decodeDefPactExec bs = either (const Nothing) (Just . snd) (deserialiseFromBytes decode (fromStrict bs))

encodeNamespace :: Namespace -> ByteString
encodeNamespace = toStrictByteString . encode

decodeNamespace :: ByteString -> Maybe Namespace
decodeNamespace bs =either (const Nothing) (Just . snd) (deserialiseFromBytes decode (fromStrict bs))

-- TODO: Do we want a gas log entry here???
-- most likely not, maybe just pre/post?
chargeGasMSerialize :: i -> MilliGas -> GasM (PactError i) ()
chargeGasMSerialize info amount = do
  let fakeStackFrame = [] -- TODO: Greg: use real stack frame?
  (GasMEnv gasRef mgl@(MilliGasLimit gasLimit)) <- ask
  !currGas <- liftIO $ readIORef gasRef
  let !used = currGas <> amount
  liftIO (writeIORef gasRef used)
  when (used > gasLimit) $ throwError (PEExecutionError (GasExceeded mgl used) fakeStackFrame info)

encodeRowData :: i -> RowData -> GasM (PactError i) ByteString
encodeRowData info rd = do
  gasSerializeRowData info rd
  pure . toStrictByteString $ encode rd

gasSerializeRowData :: forall i. i -> RowData -> GasM (PactError i) ()
gasSerializeRowData info (RowData fields) = do
  -- Charge for keys
  chargeGasMSerialize info $ MilliGas $ 1000 * fromIntegral (sum $ Text.length . _field <$> Map.keys fields)
  -- Charge for values
  traverse_ gasSerializePactValue fields

  where

    gasSerializePactValue :: PactValue -> GasM (PactError i) ()
    gasSerializePactValue = \case
      PLiteral l -> gasSerializeLiteral l
      PList vs -> do
        chargeGasMSerialize info $ MilliGas 1000
        traverse_ gasSerializePactValue vs
      PGuard _ -> chargeGasMSerialize info $ MilliGas 1000
      PModRef _ -> chargeGasMSerialize info $ MilliGas 1000
      PObject o -> do
        chargeGasMSerialize info $ MilliGas $ (1000 *) $ sum $ fromIntegral . Text.length . _field <$> Map.keys o
        traverse_ gasSerializePactValue o
      PCapToken {} -> chargeGasMSerialize info $ MilliGas 1000
      PTime _ -> chargeGasMSerialize info $ MilliGas 1000

    gasSerializeLiteral = \case
      LString s -> chargeGasMSerialize info $ MilliGas $ 1000 * fromIntegral (Text.length s)
      LInteger i -> chargeGasMSerialize info $ MilliGas $ 1000 * fromIntegral (length (show i))
      LDecimal d -> chargeGasMSerialize info $ MilliGas $ 1000 * fromIntegral (length (show d))
      LBool _ -> chargeGasMSerialize info $ MilliGas 1000
      LUnit -> chargeGasMSerialize info $ MilliGas 1000


decodeRowData :: ByteString -> Maybe RowData
decodeRowData bs = either (const Nothing) (Just . snd) (deserialiseFromBytes decode (fromStrict bs))

instance Serialise Namespace where
  encode (Namespace n user admin) = encode n <> encode user <> encode admin
  decode = Namespace <$> decode <*> decode <*> decode

instance Serialise RowData where
  encode (RowData m) = encode m
  decode = RowData <$> decode

instance Serialise NamespaceName where
  encode (NamespaceName ns) = encode ns
  decode = NamespaceName <$> decode

instance Serialise ModuleName where
  encode (ModuleName mn mns) = encode mn <> encode mns
  decode = ModuleName <$> decode <*> decode

instance Serialise KeySetName where
  encode (KeySetName ksn ns) = encode ksn <> encode ns
  decode = KeySetName <$> decode <*> decode

instance Serialise PublicKeyText where
  encode (PublicKeyText t) = encode t
  decode = PublicKeyText <$> decode

instance Serialise KeySet where
  encode (KeySet ks p) = encode ks <> encode p
  decode = KeySet <$> decode <*> decode

instance Serialise KSPredicate where
  encode = \case
    KeysAll -> encodeWord 0
    Keys2 -> encodeWord 1
    KeysAny -> encodeWord 2
    CustomPredicate pn -> encodeWord 3 <> encode pn

  decode = decodeWord >>= \case
    0 -> pure KeysAll
    1 -> pure Keys2
    2 -> pure KeysAny
    3 -> CustomPredicate <$> decode
    _ -> fail "unexpected decoding"

instance Serialise QualifiedName where
  encode (QualifiedName qn mn) = encode qn <> encode mn
  decode = QualifiedName <$> decode <*> decode

instance Serialise BareName where
  encode (BareName bn) = encode bn
  decode = BareName <$> decode

instance Serialise DynamicName where
  encode (DynamicName dn dcall) = encode dn <> encode dcall
  decode = DynamicName <$> decode <*> decode

instance Serialise ParsedName where
  encode (QN qn) = encodeWord 0 <> encode qn
  encode (BN bn) = encodeWord 1 <> encode bn
  encode (DN dn) = encodeWord 2 <> encode dn

  decode = decodeWord >>= \case
    0 -> QN <$> decode
    1 -> BN <$> decode
    2 -> DN <$> decode
    _ -> fail "unexpected decoding"

instance Serialise ParsedTyName where
  encode (TQN qn) = encodeWord 0 <> encode qn
  encode (TBN bn) = encodeWord 1 <> encode bn

  decode = decodeWord >>= \case
    0 -> TQN <$> decode
    1 -> TBN <$> decode
    _ -> fail "unexpected decoding"


instance Serialise Hash where
  encode (Hash h) = encode h
  decode = Hash <$> decode

instance Serialise ModuleHash where
  encode (ModuleHash mh) = encode mh
  decode = ModuleHash <$> decode

instance Serialise FullyQualifiedName where
  encode (FullyQualifiedName mn fqn h) = encode mn <> encode fqn <> encode h
  decode = FullyQualifiedName <$> decode <*> decode <*> decode

instance Serialise (Governance Name) where
  encode = \case
    KeyGov ksn -> encodeWord 0 <> encode ksn
    CapGov cgn -> encodeWord 1 <> encode cgn

  decode = decodeWord >>= \case
    0 -> KeyGov <$> decode
    1 -> CapGov <$> decode
    _ -> fail "unexpected decoding"

instance (Serialise ty, Serialise i) => Serialise (Arg ty i) where
  encode (Arg n ty i) = encode n <> encode ty <> encode i
  decode = Arg <$> decode <*> decode <*> decode


instance Serialise Decimal where
  encode (Decimal places mantissa) = encode places <> encode mantissa
  decode = Decimal <$> decode <*> decode

instance Serialise Literal where
  encode (LString s) = encodeWord 0 <> encode s
  encode (LInteger i) = encodeWord 1 <> encode i
  encode (LDecimal d) = encodeWord 2 <> encode d
  encode LUnit = encodeWord 3
  encode (LBool b) = encodeWord 4 <> encode b

  decode = decodeWord >>= \case
    0 -> LString <$> decode
    1 -> LInteger <$> decode
    2 -> LDecimal <$> decode
    3 -> pure LUnit
    4 -> LBool <$> decode
    _ -> fail "unexpected decoding"

instance Serialise Field where
  encode (Field f) = encode f
  decode = Field <$> decode

instance (Serialise name, Serialise e) => Serialise (CapForm name e) where
  encode (WithCapability e1 e2) = encodeWord 0 <> encode e1 <> encode e2
  encode (CreateUserGuard name es) = encodeWord 1 <> encode name <> encode es

  decode = decodeWord >>= \case
    0 -> WithCapability <$> decode <*> decode
    1 -> CreateUserGuard <$> decode <*> decode
    _ -> fail "unexpected decoding"

instance
  (Serialise b, Serialise i)
  => Serialise (BuiltinForm (Term Name Type b i)) where
  encode (CAnd t1 t2) = encodeWord 0 <> encode t1 <> encode t2
  encode (COr t1 t2) = encodeWord 1 <> encode t1 <> encode t2
  encode (CIf t1 t2 t3) = encodeWord 2 <> encode t1 <> encode t2 <> encode t3
  encode (CEnforceOne t1 t2) = encodeWord 3 <> encode t1 <> encode t2
  encode (CEnforce t1 t2) = encodeWord 4 <> encode t1 <> encode t2

  decode = decodeWord >>= \case
    0 -> CAnd <$> decode <*> decode
    1 -> COr <$> decode <*> decode
    2 -> CIf <$> decode <*> decode <*> decode
    3 -> CEnforceOne <$> decode <*> decode
    4 -> CEnforce <$> decode <*> decode
    _ -> fail "unexpected decoding"

instance
  (Serialise b, Serialise i)
  => Serialise (Term Name Type b i) where
  encode (Var n i) = encodeWord 0 <> encode n <> encode i
  encode (Lam args term i) = encodeWord 1 <> encode args <> encode term <> encode i
  encode (Let arg t1 t2 i) = encodeWord 2 <> encode arg <> encode t1 <> encode t2 <> encode i
  encode (App t1 t2 i) = encodeWord 3 <> encode t1 <> encode t2 <> encode i
  encode (Sequence t1 t2 i) = encodeWord 4 <> encode t1 <> encode t2 <> encode i
  encode (Nullary t i) = encodeWord 5 <> encode t <> encode i
  encode (Conditional bi i) = encodeWord 6 <> encode bi <> encode i
  encode (Builtin bi i) = encodeWord 7 <> encode bi <> encode i
  encode (Constant lit i) = encodeWord 8 <> encode lit <> encode i
  encode (ListLit t i) = encodeWord 9 <> encode t <> encode i
  encode (Try t1 t2 i) = encodeWord 10 <> encode t1 <> encode t2 <> encode i
  encode (ObjectLit o i) = encodeWord 11 <> encode o <> encode i
  encode (CapabilityForm cf i) = encodeWord 12 <> encode cf <> encode i

  decode = decodeWord >>= \case
    0 -> Var <$> decode <*> decode
    1 -> Lam <$> decode <*> decode <*> decode
    2 -> Let <$> decode <*> decode <*> decode <*> decode
    3 -> App <$> decode <*> decode <*> decode
    4 -> Sequence <$> decode <*> decode <*> decode
    5 -> Nullary <$> decode <*> decode
    6 -> Conditional <$> decode <*> decode
    7 -> Builtin <$> decode <*> decode
    8 -> Constant <$> decode <*> decode
    9 -> ListLit <$> decode <*> decode
    10 -> Try <$> decode <*> decode <*> decode
    11 -> ObjectLit <$> decode <*> decode
    12 -> CapabilityForm <$> decode <*> decode
    _ -> fail "unexpected decoding"

instance
  (Serialise b, Serialise i)
  =>Serialise (Defun Name Type b i) where
  encode (Defun spec args term i) = encode spec <> encode args <> encode term <> encode i

  decode = Defun <$> decode <*> decode <*> decode <*> decode

instance
  (Serialise b, Serialise i)
  => Serialise (DefConst Name Type b i) where
  encode (DefConst spec term i) = encode spec <> encode term <> encode i

  decode = DefConst <$> decode <*> decode <*> decode

instance (Serialise b, Serialise i) => Serialise (ConstVal (Term Name Type b i)) where
  encode = \case
    TermConst t -> encodeWord 0 <> encode t
    EvaledConst pv -> encodeWord 1 <> encode pv

  decode = decodeWord >>= \case
    0 -> TermConst <$> decode
    1 -> EvaledConst <$> decode
    _ -> fail "unexpected decoding"

instance Serialise (FQNameRef Name) where
  encode (FQName fqn) = encode fqn
  decode = FQName <$> decode

instance Serialise (DefManagedMeta Name) where
  encode (DefManagedMeta i ref) = encodeWord 0 <> encode i <> encode ref
  encode AutoManagedMeta = encodeWord 1

  decode = decodeWord >>= \case
    0 -> DefManagedMeta <$> decode <*> decode
    1 -> pure AutoManagedMeta
    _ -> fail "unexpected decoding"

instance Serialise (DefManagedMeta BareName) where
  encode (DefManagedMeta i ref) = encodeWord 0 <> encode i <> encode ref
  encode AutoManagedMeta = encodeWord 1

  decode = decodeWord >>= \case
    0 -> DefManagedMeta <$> decode <*> decode
    1 -> pure AutoManagedMeta
    _ -> fail "unexpected decoding"

instance Serialise (DefManagedMeta (FQNameRef Name)) where
  encode (DefManagedMeta i ref) = encodeWord 0 <> encode i <> encode ref
  encode AutoManagedMeta = encodeWord 1

  decode = decodeWord >>= \case
    0 -> DefManagedMeta <$> decode <*> decode
    1 -> pure AutoManagedMeta
    _ -> fail "unexpected decoding"

instance Serialise (DefCapMeta BareName) where
  encode DefEvent = encodeWord 0
  encode (DefManaged meta) = encodeWord 1 <> encode meta
  encode Unmanaged = encodeWord 2

  decode = decodeWord >>= \case
    0 -> pure DefEvent
    1 -> DefManaged <$> decode
    2 -> pure Unmanaged
    _ -> fail "unexpected decoding"

instance Serialise (DefCapMeta (FQNameRef Name)) where
  encode DefEvent = encodeWord 0
  encode (DefManaged meta) = encodeWord 1 <> encode meta
  encode Unmanaged = encodeWord 2

  decode = decodeWord >>= \case
    0 -> pure DefEvent
    1 -> DefManaged <$> decode
    2 -> pure Unmanaged
    _ -> fail "unexpected decoding"

instance (Serialise b, Serialise i) => Serialise (DefCap Name Type b i) where
  encode (DefCap spec args term meta i) =
    encode spec
    <> encode args
    <> encode term
    <> encode meta
    <> encode i

  decode = DefCap <$> decode <*> decode
           <*> decode <*> decode <*> decode


instance Serialise i => Serialise (DefSchema Type i) where
  encode (DefSchema n schema i) = encode n <> encode schema <> encode i
  decode = DefSchema <$> decode <*> decode <*> decode

instance Serialise (TableSchema Name) where
  encode (ResolvedTable n) = encode n
  decode = ResolvedTable <$> decode

instance Serialise i => Serialise (DefTable Name i) where
  encode (DefTable n schema i) = encode n <> encode schema <> encode i
  decode = DefTable <$> decode <*> decode <*> decode

instance
  (Serialise b, Serialise i)
  => Serialise (Step Name Type b i) where
  encode (Step t) = encodeWord 0 <> encode t
  encode (StepWithRollback t rb) = encodeWord 1 <> encode t <> encode rb

  decode = decodeWord >>= \case
    0 -> Step <$> decode
    1 -> StepWithRollback <$> decode <*> decode
    _ -> fail "unexpected decoding"

instance
  (Serialise b, Serialise i)
  => Serialise (DefPact Name Type b i) where
  encode (DefPact spec args steps i) = encode spec <> encode args
    <> encode steps <> encode i

  decode = DefPact <$> decode <*> decode <*> decode <*> decode

instance
  (Serialise b, Serialise i)
  => Serialise (Def Name Type b i) where
  encode (Dfun df) = encodeWord 0 <> encode df
  encode (DConst dc) = encodeWord 1 <> encode dc
  encode (DCap cap) = encodeWord 2 <> encode cap
  encode (DSchema schema) = encodeWord 3 <> encode schema
  encode (DTable table) = encodeWord 4 <> encode table
  encode (DPact dp) = encodeWord 5 <> encode dp

  decode = decodeWord >>= \case
    0 -> Dfun <$> decode
    1 -> DConst <$> decode
    2 -> DCap <$> decode
    3 -> DSchema <$> decode
    4 -> DTable <$> decode
    5 -> DPact <$> decode
    _ -> fail "unexpected decoding"

instance Serialise DynamicRef where
  encode (DynamicRef n b) = encode n <> encode b
  decode = DynamicRef <$> decode <*> decode

instance Serialise NameKind where
  encode (NBound d) = encodeWord 0 <> encode d
  encode (NTopLevel mn mh) = encodeWord 1 <> encode mn <> encode mh
  encode (NModRef mn ms) = encodeWord 2 <> encode mn <> encode ms
  encode (NDynRef dref) = encodeWord 3 <> encode dref

  decode = decodeWord >>= \case
    0 -> NBound <$> decode
    1 -> NTopLevel <$> decode <*> decode
    2 -> NModRef <$> decode <*> decode
    3 -> NDynRef <$> decode
    _ -> fail "unexpected decoding"

instance Serialise Name where
  encode (Name n k) = encode n <> encode k
  decode = Name <$> decode <*> decode

instance Serialise PrimType where
  encode = \case
    PrimInt -> encodeWord 0
    PrimDecimal -> encodeWord 1
    PrimBool -> encodeWord 2
    PrimString -> encodeWord 3
    PrimGuard -> encodeWord 4
    PrimTime -> encodeWord 5
    PrimUnit -> encodeWord 6

  decode = decodeWord >>= \case
    0 -> pure PrimInt
    1 -> pure PrimDecimal
    2 -> pure PrimBool
    3 -> pure PrimString
    4 -> pure PrimGuard
    5 -> pure PrimTime
    6 -> pure PrimUnit
    _ -> fail "unexpected decoding"

instance Serialise Schema where
  encode (Schema sc m) =
    encode sc <> encode m
  decode = Schema <$> decode <*> decode

instance Serialise Type where
  encode (TyPrim pt) = encodeWord 0 <> encode pt
  encode (TyList ty) = encodeWord 1 <> encode ty
  encode TyAnyList = encodeWord 2
  encode (TyModRef mr) = encodeWord 3 <> encode mr
  encode (TyObject s) = encodeWord 4 <> encode s
  encode TyAnyObject = encodeWord 5
  encode (TyTable s) = encodeWord 6 <> encode s
  encode TyCapToken = encodeWord 7
  encode TyAny = encodeWord 8

  decode = decodeWord >>= \case
    0 -> TyPrim <$> decode
    1 -> TyList <$> decode
    2 -> pure TyAnyList
    3 -> TyModRef <$> decode
    4 -> TyObject <$> decode
    5 -> pure TyAnyObject
    6 -> TyTable <$> decode
    7 -> pure TyCapToken
    _ -> fail "unexpected decoding"

instance Serialise Import where
  encode (Import mn mh mimp) = encode mn <> encode mh <> encode mimp
  decode = Import <$> decode <*> decode <*> decode

instance
  (Serialise b, Serialise i)
  => Serialise (EvalModule b i) where
  encode (Module mn mg mdef mbless mimports mimpl mhash minfo) =
    encode mn <> encode mg <> encode mdef
    <> encode mbless <> encode mimports <> encode mimpl
    <> encode mhash <> encode minfo

  decode = Module <$> decode <*> decode <*> decode <*> decode <*> decode
           <*> decode <*> decode <*> decode


instance
  Serialise i
  => Serialise (IfDefun Type i) where
  encode (IfDefun spec args i) = encode spec <> encode args <> encode i

  decode = IfDefun <$> decode <*> decode <*> decode

instance
  Serialise i
  => Serialise (IfDefCap name Type i) where
  encode (IfDefCap spec args meta i) = encode spec <> encode args
                                   <> encode meta <> encode i

  decode = IfDefCap <$> decode <*> decode <*> decode <*> decode

instance
  Serialise i
  => Serialise (IfDefPact Type i) where
  encode (IfDefPact spec args i) = encode spec <> encode args <> encode i

  decode = IfDefPact <$> decode <*> decode <*> decode

instance
  (Serialise b, Serialise i)
  => Serialise (IfDef Name Type b i) where
  encode (IfDfun df) = encodeWord 0 <> encode df
  encode (IfDConst dc) = encodeWord 1 <> encode dc
  encode (IfDCap cap) = encodeWord 2 <> encode cap
  encode (IfDSchema schema) = encodeWord 3 <> encode schema
  encode (IfDPact dp) =  encodeWord 4 <> encode dp

  decode = decodeWord >>= \case
    0 -> IfDfun <$> decode
    1 -> IfDConst <$> decode
    2 -> IfDCap <$> decode
    3 -> IfDSchema <$> decode
    4 -> IfDPact <$> decode
    _ -> fail "unexpected decoding"

instance
  (Serialise b, Serialise i)
  => Serialise (EvalInterface b i) where
  encode (Interface n defs imp h i) = encode n <> encode defs <> encode imp <> encode h <> encode i

  decode = Interface <$> decode <*> decode <*> decode <*> decode <*> decode

instance
  (Serialise b, Serialise i)
  => Serialise (ModuleData b i) where
  encode = \case
    ModuleData em m -> encodeWord 0 <> encode em <> encode m
    InterfaceData ei m -> encodeWord 1 <> encode ei <> encode m

  decode = decodeWord >>= \case
    0 -> ModuleData <$> decode <*> decode
    1 -> InterfaceData <$> decode <*> decode
    _ -> fail "unexpected decoding"

instance Serialise SpanInfo where
  encode (SpanInfo sl sc el ec) = encode sl <> encode sc <> encode el <> encode ec
  decode = SpanInfo <$> decode <*> decode <*> decode <*> decode

instance Serialise CoreBuiltin where
  encode = \case
    CoreAdd -> encodeWord 0
    CoreSub-> encodeWord 1
    CoreMultiply -> encodeWord 2
    CoreDivide -> encodeWord 3
    CoreNegate -> encodeWord 4
    CoreAbs -> encodeWord 5
    CorePow -> encodeWord 6
    CoreNot -> encodeWord 7
    CoreEq -> encodeWord 8
    CoreNeq -> encodeWord 9
    CoreGT -> encodeWord 10
    CoreGEQ -> encodeWord 11
    CoreLT -> encodeWord 12
    CoreLEQ -> encodeWord 13
    CoreBitwiseAnd -> encodeWord 14
    CoreBitwiseOr -> encodeWord 15
    CoreBitwiseXor -> encodeWord 16
    CoreBitwiseFlip -> encodeWord 17
    CoreBitShift -> encodeWord 18
    CoreRound -> encodeWord 19
    CoreCeiling -> encodeWord 20
    CoreFloor -> encodeWord 21
    CoreExp -> encodeWord 22
    CoreLn -> encodeWord 23
    CoreSqrt -> encodeWord 24
    CoreLogBase -> encodeWord 25
    CoreLength -> encodeWord 26
    CoreTake -> encodeWord 27
    CoreDrop -> encodeWord 28
    CoreConcat -> encodeWord 29
    CoreReverse -> encodeWord 30
    CoreContains -> encodeWord 31
    CoreSort -> encodeWord 32
    CoreSortObject -> encodeWord 33
    CoreRemove -> encodeWord 34
    CoreMod -> encodeWord 35
    CoreMap -> encodeWord 36
    CoreFilter -> encodeWord 37
    CoreZip -> encodeWord 38
    CoreIntToStr -> encodeWord 39
    CoreStrToInt -> encodeWord 40
    CoreStrToIntBase -> encodeWord 41
    CoreFold -> encodeWord 42
    CoreDistinct -> encodeWord 43
    CoreFormat -> encodeWord 44
    CoreEnumerate -> encodeWord 45
    CoreEnumerateStepN -> encodeWord 46
    CoreShow -> encodeWord 47
    CoreReadMsg -> encodeWord 48
    CoreReadMsgDefault -> encodeWord 49
    CoreReadInteger -> encodeWord 50
    CoreReadDecimal -> encodeWord 51
    CoreReadString -> encodeWord 52
    CoreReadKeyset -> encodeWord 53
    CoreEnforceGuard -> encodeWord 54
    CoreEnforceKeyset -> encodeWord 55
    CoreKeysetRefGuard -> encodeWord 56
    CoreAt -> encodeWord 57
    CoreMakeList -> encodeWord 58
    CoreB64Encode -> encodeWord 59
    CoreB64Decode -> encodeWord 60
    CoreStrToList -> encodeWord 61
    CoreYield -> encodeWord 62
    CoreResume -> encodeWord 63
    CoreBind -> encodeWord 64
    CoreRequireCapability -> encodeWord 65
    CoreComposeCapability -> encodeWord 66
    CoreInstallCapability -> encodeWord 67
    CoreEmitEvent -> encodeWord 68
    CoreCreateCapabilityGuard -> encodeWord 69
    CoreCreateCapabilityPactGuard -> encodeWord 70
    CoreCreateModuleGuard -> encodeWord 71
    CoreCreateTable -> encodeWord 72
    CoreDescribeKeyset -> encodeWord 73
    CoreDescribeModule -> encodeWord 74
    CoreDescribeTable -> encodeWord 75
    CoreDefineKeySet -> encodeWord 76
    CoreDefineKeysetData -> encodeWord 77
    CoreFoldDb -> encodeWord 78
    CoreInsert -> encodeWord 79
    CoreKeyLog -> encodeWord 80
    CoreKeys -> encodeWord 81
    CoreRead -> encodeWord 82
    CoreSelect -> encodeWord 83
    CoreSelectWithFields -> encodeWord 84
    CoreUpdate -> encodeWord 85
    CoreWithDefaultRead -> encodeWord 86
    CoreWithRead -> encodeWord 87
    CoreWrite -> encodeWord 88
    CoreTxIds -> encodeWord 89
    CoreTxLog -> encodeWord 90
    CoreTxHash -> encodeWord 91
    CoreAndQ -> encodeWord 92
    CoreOrQ -> encodeWord 93
    CoreWhere -> encodeWord 94
    CoreNotQ -> encodeWord 95
    CoreHash -> encodeWord 96
    CoreContinue -> encodeWord 97
    CoreParseTime -> encodeWord 98
    CoreFormatTime -> encodeWord 99
    CoreTime -> encodeWord 100
    CoreAddTime -> encodeWord 101
    CoreDiffTime -> encodeWord 102
    CoreHours -> encodeWord 103
    CoreMinutes -> encodeWord 104
    CoreDays -> encodeWord 105
    CoreCompose -> encodeWord 106
    CoreNamespace -> encodeWord 107
    CoreDefineNamespace -> encodeWord 108
    CoreDescribeNamespace -> encodeWord 109
    CoreCreatePrincipal -> encodeWord 110
    CoreIsPrincipal -> encodeWord 111
    CoreTypeOfPrincipal -> encodeWord 112
    CoreValidatePrincipal -> encodeWord 113
    CoreCreateDefPactGuard -> encodeWord 114
    CoreYieldToChain -> encodeWord 115
    CoreChainData -> encodeWord 116
    CoreIsCharset -> encodeWord 117
    CorePactId -> encodeWord 118
    CoreZkPairingCheck -> encodeWord 119
    CoreZKScalarMult -> encodeWord 120
    CoreZkPointAdd -> encodeWord 121
    CorePoseidonHashHackachain -> encodeWord 122
    CoreTypeOf -> encodeWord 123
    CoreDec -> encodeWord 124
    CoreRoundPrec -> encodeWord 125
    CoreCeilingPrec -> encodeWord 126
    CoreFloorPrec -> encodeWord 127
    CoreCond -> encodeWord 128
    CoreIdentity -> encodeWord 129
    CoreVerifySPV -> encodeWord 130

  decode = decodeWord >>= \case
    0 -> pure CoreAdd
    1 -> pure CoreSub
    2 -> pure CoreMultiply
    3 -> pure CoreDivide
    4 -> pure CoreNegate
    5 -> pure CoreAbs
    6 -> pure CorePow
    7 -> pure CoreNot
    8 -> pure CoreEq
    9 -> pure CoreNeq
    10 -> pure CoreGT
    11 -> pure CoreGEQ
    12 -> pure CoreLT
    13 -> pure CoreLEQ
    14 -> pure CoreBitwiseAnd
    15 -> pure CoreBitwiseOr
    16 -> pure CoreBitwiseXor
    17 -> pure CoreBitwiseFlip
    18 -> pure CoreBitShift
    19 -> pure CoreRound
    20 -> pure CoreCeiling
    21 -> pure CoreFloor
    22 -> pure CoreExp
    23 -> pure CoreLn
    24 -> pure CoreSqrt
    25 -> pure CoreLogBase
    26 -> pure CoreLength
    27 -> pure CoreTake
    28 -> pure CoreDrop
    29 -> pure CoreConcat
    30 -> pure CoreReverse
    31 -> pure CoreContains
    32 -> pure CoreSort
    33 -> pure CoreSortObject
    34 -> pure CoreRemove
    35 -> pure CoreMod
    36 -> pure CoreMap
    37 -> pure CoreFilter
    38 -> pure CoreZip
    39 -> pure CoreIntToStr
    40 -> pure CoreStrToInt
    41 -> pure CoreStrToIntBase
    42 -> pure CoreFold
    43 -> pure CoreDistinct
    44 -> pure CoreFormat
    45 -> pure CoreEnumerate
    46 -> pure CoreEnumerateStepN
    47 -> pure CoreShow
    48 -> pure CoreReadMsg
    49 -> pure CoreReadMsgDefault
    50 -> pure CoreReadInteger
    51 -> pure CoreReadDecimal
    52 -> pure CoreReadString
    53 -> pure CoreReadKeyset
    54 -> pure CoreEnforceGuard
    55 -> pure CoreEnforceKeyset
    56 -> pure CoreKeysetRefGuard
    57 -> pure CoreAt
    58 -> pure CoreMakeList
    59 -> pure CoreB64Encode
    60 -> pure CoreB64Decode
    61 -> pure CoreStrToList
    62 -> pure CoreYield
    63 -> pure CoreResume
    64 -> pure CoreBind
    65 -> pure CoreRequireCapability
    66 -> pure CoreComposeCapability
    67 -> pure CoreInstallCapability
    68 -> pure CoreEmitEvent
    69 -> pure CoreCreateCapabilityGuard
    70 -> pure CoreCreateCapabilityPactGuard
    71 -> pure CoreCreateModuleGuard
    72 -> pure CoreCreateTable
    73 -> pure CoreDescribeKeyset
    74 -> pure CoreDescribeModule
    75 -> pure CoreDescribeTable
    76 -> pure CoreDefineKeySet
    77 -> pure CoreDefineKeysetData
    78 -> pure CoreFoldDb
    79 -> pure CoreInsert
    80 -> pure CoreKeyLog
    81 -> pure CoreKeys
    82 -> pure CoreRead
    83 -> pure CoreSelect
    84 -> pure CoreSelectWithFields
    85 -> pure CoreUpdate
    86 -> pure CoreWithDefaultRead
    87 -> pure CoreWithRead
    88 -> pure CoreWrite
    89 -> pure CoreTxIds
    90 -> pure CoreTxLog
    91 -> pure CoreTxHash
    92 -> pure CoreAndQ
    93 -> pure CoreOrQ
    94 -> pure CoreWhere
    95 -> pure CoreNotQ
    96 -> pure CoreHash
    97 -> pure CoreContinue
    98 -> pure CoreParseTime
    99 -> pure CoreFormatTime
    100 -> pure CoreTime
    101 -> pure CoreAddTime
    102 -> pure CoreDiffTime
    103 -> pure CoreHours
    104 -> pure CoreMinutes
    105 -> pure CoreDays
    106 -> pure CoreCompose
    107 -> pure CoreNamespace
    108 -> pure CoreDefineNamespace
    109 -> pure CoreDescribeNamespace
    110 -> pure CoreCreatePrincipal
    111 -> pure CoreIsPrincipal
    112 -> pure CoreTypeOfPrincipal
    113 -> pure CoreValidatePrincipal
    114 -> pure CoreCreateDefPactGuard

    115 -> pure CoreYieldToChain
    116 -> pure CoreChainData
    117 -> pure CoreIsCharset
    118 -> pure CorePactId
    119 -> pure CoreZkPairingCheck
    120 -> pure CoreZKScalarMult
    121 -> pure CoreZkPointAdd
    122 -> pure CorePoseidonHashHackachain
    123 -> pure CoreTypeOf
    124 -> pure CoreDec

    125 -> pure CoreRoundPrec
    126 -> pure CoreCeilingPrec
    127 -> pure CoreFloorPrec
    128 -> pure CoreCond
    129 -> pure CoreIdentity
    130 -> pure CoreVerifySPV
    _ -> fail "unexpected decoding"


instance Serialise ReplBuiltins where
  encode = encodeWord . fromIntegral . fromEnum
  decode = do
    vInd <- toEnum . fromIntegral <$> decodeWord
    if vInd >= minBound && vInd <= maxBound
      then pure $ toEnum vInd
      else fail "unexpected encoding"

instance Serialise ReplCoreBuiltin where
  encode (RBuiltinWrap b) = encodeWord 0 <> encode b
  encode (RBuiltinRepl r) = encodeWord 1 <> encode r

  decode = decodeWord >>= \case
    0 -> RBuiltinWrap <$> decode
    1 -> RBuiltinRepl <$> decode
    _ -> fail "unexpected decoding"



-- DefPacts

instance Serialise (UserGuard QualifiedName PactValue) where
  encode (UserGuard f a) = encode f <> encode a
  decode = UserGuard <$> decode <*> decode

instance Serialise DefPactId where
  encode (DefPactId pid) = encode pid
  decode = DefPactId <$> decode

instance Serialise (CapabilityGuard QualifiedName PactValue) where
  encode (CapabilityGuard n a pid) = encode n <> encode a <> encode pid
  decode = CapabilityGuard <$> decode <*> decode <*> decode

instance Serialise ModuleGuard where
  encode (ModuleGuard mn n) = encode mn <> encode n
  decode = ModuleGuard <$> decode <*> decode

instance Serialise (Guard QualifiedName PactValue) where
  encode = \case
    GKeyset ks -> encodeWord 0 <> encode ks
    GKeySetRef ksn -> encodeWord 1 <> encode ksn
    GUserGuard ug -> encodeWord 2 <> encode ug
    GCapabilityGuard cg -> encodeWord 3 <> encode cg
    GModuleGuard mg -> encodeWord 4 <> encode mg
    GDefPactGuard dpg -> encodeWord 5 <> encode dpg
  decode = decodeWord >>= \case
    0 -> GKeyset <$> decode
    1 -> GKeySetRef <$> decode
    2 -> GUserGuard <$> decode
    3 -> GCapabilityGuard <$> decode
    4 -> GModuleGuard <$> decode
    5 -> GDefPactGuard <$> decode
    _ -> fail "unexpected decoding"

instance Serialise DefPactGuard where
  encode (DefPactGuard i name )= encode i <> encode name
  decode = DefPactGuard <$> decode <*> decode

instance Serialise ModRef where
  encode (ModRef mn imp ref) = encode mn <> encode imp <> encode ref
  decode = ModRef <$> decode <*> decode <*> decode

instance Serialise (CapToken FullyQualifiedName PactValue) where
  encode (CapToken n a) = encode n <> encode a
  decode = CapToken <$> decode <*> decode

instance Serialise PactValue where
  encode = \case
    PLiteral l -> encodeWord 0 <> encode l
    PList l -> encodeWord 1 <> encode l
    PGuard g -> encodeWord 2 <> encode g
    PObject o -> encodeWord 3 <> encode o
    PModRef mr -> encodeWord 4 <> encode mr
    PCapToken ct -> encodeWord 5 <> encode ct
    PTime (UTCTime (NominalDiffTime pt)) -> encodeWord 6 <> encode pt
  decode = decodeWord >>= \case
    0 -> PLiteral <$> decode
    1 -> PList <$> decode
    2 -> PGuard <$> decode
    3 -> PObject <$> decode
    4 -> PModRef <$> decode
    5 -> PCapToken <$> decode
    6 -> PTime . UTCTime . NominalDiffTime <$> decode
    _ -> fail "unexpected decoding"

instance Serialise ChainId where
  encode (ChainId cid) = encode cid
  decode = ChainId <$> decode

instance Serialise Provenance where
  encode (Provenance tcid mh) = encode tcid <> encode mh
  decode = Provenance <$> decode <*> decode

instance Serialise Yield where
  encode (Yield d p s) = encode d <> encode p <> encode s
  decode = Yield <$> decode <*> decode <*> decode

instance Serialise (DefPactContinuation QualifiedName PactValue) where
  encode (DefPactContinuation n a) = encode n <> encode a
  decode = DefPactContinuation <$> decode <*> decode

instance Serialise DefPactExec where
  encode (DefPactExec sc y s dpid cont rb ndp)
    = encode sc <> encode y <> encode s <> encode dpid <> encode cont <> encode rb <> encode ndp
  decode = DefPactExec <$> decode <*> decode <*> decode <*> decode <*> decode <*> decode <*> decode
