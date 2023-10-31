-- | 
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pact.Core.Serialise.CBOR where

import Codec.Serialise.Class
import Codec.CBOR.Encoding
import Codec.CBOR.Decoding
import Data.Decimal

import Pact.Core.Names
import Pact.Core.Persistence
import Pact.Core.IR.Term
import Pact.Core.Guards
import Pact.Core.Hash
import Pact.Core.Type
import Pact.Core.Literal
import Pact.Core.Capabilities
import Pact.Core.Builtin
import Pact.Core.Imports
import Pact.Core.Info

instance Serialise NamespaceName where
  encode (NamespaceName ns) = encode ns
  decode = NamespaceName <$> decode

instance Serialise ModuleName where
  encode (ModuleName mn mns) = encodeListLen 2 <> encode mn <> encode mns
  decode = do
    2 <- decodeListLen
    ModuleName <$> decode <*> decode

instance Serialise KeySetName where
  encode (KeySetName ksn) = encode ksn
  decode = KeySetName <$> decode

instance Serialise QualifiedName where
  encode (QualifiedName qn mn) = encodeListLen 2 <> encode qn <> encode mn
  decode = do
    2 <- decodeListLen
    QualifiedName <$> decode <*> decode

instance Serialise BareName where
  encode (BareName bn) = encode bn
  decode = BareName <$> decode

instance Serialise DynamicName where
  encode (DynamicName dn dcall) = encodeListLen 2 <> encode dn <> encode dcall
  decode = do
    2 <- decodeListLen
    DynamicName <$> decode <*> decode

instance Serialise ParsedName where
  encode (QN qn) = encodeListLen 2 <> encodeWord 0 <> encode qn
  encode (BN bn) = encodeListLen 2 <> encodeWord 1 <> encode bn
  encode (DN dn) = encodeListLen 2 <> encodeWord 2 <> encode dn

  decode = do
    2 <- decodeListLen
    decodeWord >>= \case
      0 -> QN <$> decode
      1 -> BN <$> decode
      2 -> DN <$> decode
      _ -> fail "unexpected decoding"

instance Serialise Hash where
  encode (Hash h) = encode h
  decode = Hash <$> decode

instance Serialise ModuleHash where
  encode (ModuleHash mh) = encode mh
  decode = ModuleHash <$> decode

instance Serialise FullyQualifiedName where
  encode (FullyQualifiedName mn fqn h) = encodeListLen 3 <> encode mn <> encode fqn <> encode h
  decode = do
    3 <- decodeListLen
    FullyQualifiedName <$> decode <*> decode <*> decode

instance Serialise (CapGovRef Name) where
  encode (ResolvedGov fqn) = encode fqn
  decode = ResolvedGov <$> decode

instance Serialise (Governance Name) where
  encode = \case
    KeyGov ksn -> encodeListLen 2 <> encodeWord 0 <> encode ksn
    CapGov cgn -> encodeListLen 2 <> encodeWord 1 <> encode cgn

  decode = do
    2 <- decodeListLen
    decodeWord >>= \case
      0 -> KeyGov <$> decode
      1 -> CapGov <$> decode
      _ -> fail "unexpected decoding"

instance Serialise ty => Serialise (Arg ty) where
  encode (Arg n ty) = encodeListLen 2 <> encode n <> encode ty
  decode = do
    2 <- decodeListLen
    Arg <$> decode <*> decode

instance Serialise LamInfo where
  encode (TLDefun mn t) = encodeWord 0 <> encodeListLen 2 <> encode mn <> encode t
  encode (TLDefCap mn t) = encodeWord 1 <> encodeListLen 2 <> encode mn <> encode t
  encode (TLDefPact mn t) = encodeWord 2 <> encodeListLen 2 <> encode mn <> encode t
  encode AnonLamInfo = encodeWord 3

  decode = decodeWord >>= \case
    0 -> do
      2 <- decodeListLen
      TLDefun <$> decode <*> decode
    1 -> do
      2 <- decodeListLen
      TLDefCap <$> decode <*> decode
    2 -> do
      2 <- decodeListLen
      TLDefPact <$> decode <*> decode
    3 -> pure AnonLamInfo
    _ -> fail "unexpected decoding"


instance Serialise Decimal where
  encode (Decimal places mantissa) = encodeListLen 2 <> encode places <> encode mantissa
  decode = do
    2 <- decodeListLen
    Decimal <$> decode <*> decode

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
    _ -> fail "unexpeced decoding"

instance Serialise Field where
  encode (Field f) = encode f
  decode = Field <$> decode

instance (Serialise name, Serialise e) => Serialise (CapForm name e) where
  encode (WithCapability name es e) = encodeWord 0 <> encodeListLen 3 <> encode name <> encode es <> encode e
  encode (CreateUserGuard name es) = encodeWord 1 <> encodeListLen 2 <> encode name <> encode es

  decode = decodeWord >>= \case
    0 -> do
      3 <- decodeListLen
      WithCapability <$> decode <*> decode <*> decode
    1 -> do
      2 <- decodeListLen
      CreateUserGuard <$> decode <*> decode
    _ -> fail "unexpected decoding"

instance Serialise (BuiltinForm (Term Name Type RawBuiltin SpanInfo)) where
  encode (CAnd t1 t2) = encodeWord 0 <> encodeListLen 2 <> encode t1 <> encode t2
  encode (COr t1 t2) = encodeWord 1 <> encodeListLen 2 <> encode t1 <> encode t2
  encode (CIf t1 t2 t3) = encodeWord 2 <> encodeListLen 3 <> encode t1 <> encode t2 <> encode t3
  encode (CEnforceOne t1 t2) = encodeWord 3 <> encodeListLen 2 <> encode t1 <> encode t2
  encode (CEnforce t1 t2) = encodeWord 4 <> encodeListLen 2 <> encode t1 <> encode t2

  decode = decodeWord >>= \case
    0 -> do
      2 <- decodeListLen
      CAnd <$> decode <*> decode
    1 -> do
      2 <- decodeListLen
      COr <$> decode <*> decode
    2 -> do
      3 <- decodeListLen
      CIf <$> decode <*> decode <*> decode
    3 -> do
      2 <- decodeListLen
      CEnforceOne <$> decode <*> decode
    4 -> do
      2 <- decodeListLen
      CEnforce <$> decode <*> decode
    _ -> fail "unexpected decoding"

instance Serialise (Term Name Type RawBuiltin SpanInfo) where
  encode (Var n i) = encodeWord 0 <> encodeListLen 2 <> encode n <> encode i
  encode (Lam li args term i) = encodeWord 1 <> encodeListLen 4 <> encode li <> encode args <> encode term <> encode i
  encode (Let arg t1 t2 i) = encodeWord 2 <> encodeListLen 4 <> encode arg <> encode t1 <> encode t2 <> encode i
  encode (App t1 t2 i) = encodeWord 3 <> encodeListLen 3 <> encode t1 <> encode t2 <> encode i
  encode (Sequence t1 t2 i) = encodeWord 4 <> encodeListLen 3 <> encode t1 <> encode t2 <> encode i
  encode (Nullary t i) = encodeWord 5 <> encodeListLen 2 <> encode t <> encode i
  encode (Conditional bi i) = encodeWord 6 <> encodeListLen 2 <> encode bi <> encode i
  encode (Builtin bi i) = encodeWord 7 <> encodeListLen 2 <> encode bi <> encode i
  encode (Constant lit i) = encodeWord 8 <> encodeListLen 2 <> encode lit <> encode i
  encode (ListLit t i) = encodeWord 9 <> encodeListLen 2 <> encode t <> encode i
  encode (Try t1 t2 i) = encodeWord 10 <> encodeListLen 3 <> encode t1 <> encode t2 <> encode i
  encode (ObjectLit o i) = encodeWord 11 <> encodeListLen 2 <> encode o <> encode i
  encode (CapabilityForm cf i) = encodeWord 12 <> encodeListLen 2 <> encode cf <> encode i
  encode (Error t i) = encodeWord 13 <> encodeListLen 2 <> encode t <> encode i

  decode = decodeWord >>= \case
    0 -> do
      2 <- decodeListLen
      Var <$> decode <*> decode
    1 -> do
      4 <- decodeListLen
      Lam <$> decode <*> decode <*> decode <*> decode
    2 -> do
      4 <- decodeListLen
      Let <$> decode <*> decode <*> decode <*> decode
    3 -> do
      3 <- decodeListLen
      App <$> decode <*> decode <*> decode
    4 -> do
      3 <- decodeListLen
      Sequence <$> decode <*> decode <*> decode
    5 -> do
      2 <- decodeListLen
      Nullary <$> decode <*> decode
    6 -> do
      2 <- decodeListLen
      Conditional <$> decode <*> decode
    7 -> do
      2 <- decodeListLen
      Builtin <$> decode <*> decode
    8 -> do
      2 <- decodeListLen
      Constant <$> decode <*> decode
    9 -> do
      2 <- decodeListLen
      ListLit <$> decode <*> decode
    10 -> do
      3 <- decodeListLen
      Try <$> decode <*> decode <*> decode
    11 -> do
      2 <- decodeListLen
      ObjectLit <$> decode <*> decode
    12 -> do
      2 <- decodeListLen
      CapabilityForm <$> decode <*> decode
    13 -> do
      2 <- decodeListLen
      Error <$> decode <*> decode
    _ -> fail "unexpected decoding"

instance Serialise (Defun Name Type RawBuiltin SpanInfo) where
  encode (Defun n args ret term i) = encodeListLen 5
    <> encode n <> encode args <> encode ret
    <> encode term <> encode i

  decode = do
    5 <- decodeListLen
    Defun <$> decode <*> decode <*> decode
      <*> decode <*> decode

instance Serialise (DefConst Name Type RawBuiltin SpanInfo) where
  encode (DefConst n ret term i) = encodeListLen 4
    <> encode n <> encode ret
    <> encode term <> encode i

  decode = do
    4 <- decodeListLen
    DefConst <$> decode <*> decode <*> decode <*> decode

instance Serialise (FQNameRef Name) where
  encode (FQName fqn) = encode fqn
  decode = FQName <$> decode

instance Serialise (DefManagedMeta Name) where
  encode (DefManagedMeta i ref) = encodeWord 0 <> encodeListLen 2 <> encode i <> encode ref
  encode AutoManagedMeta = encodeWord 1

  decode = decodeWord >>= \case
    0 -> do
      2 <- decodeListLen
      DefManagedMeta <$> decode <*> decode
    1 -> pure AutoManagedMeta
    _ -> fail "unexpected decoding"

instance Serialise (DefCapMeta Name) where
  encode DefEvent = encodeWord 0
  encode (DefManaged meta) = encodeWord 1 <> encode meta
  encode Unmanaged = encodeWord 2

  decode = decodeWord >>= \case
    0 -> pure DefEvent
    1 -> DefManaged <$> decode
    2 -> pure Unmanaged
    _ -> fail "unexpected dcecoding"

instance Serialise (DefCap Name Type RawBuiltin SpanInfo) where
  encode (DefCap n arity args ret term meta i) = encodeListLen 7
    <> encode n <> encode arity <> encode args
    <> encode ret <> encode term <> encode meta
    <> encode i

  decode = do
    7 <- decodeListLen
    DefCap <$> decode <*> decode <*> decode
                      <*> decode <*> decode
                      <*> decode <*> decode


instance Serialise (DefSchema Type SpanInfo) where
  encode (DefSchema n schema i) = encodeListLen 3 <> encode n <> encode schema <> encode i

  decode = do
    3 <- decodeListLen
    DefSchema <$> decode <*> decode <*> decode

instance Serialise (TableSchema Name) where
  encode (ResolvedTable n) = encode n
  decode = ResolvedTable <$> decode

instance Serialise (DefTable Name SpanInfo) where
  encode (DefTable n schema i) = encodeListLen 3 <> encode n <> encode schema <> encode i

  decode = do
    3 <- decodeListLen
    DefTable <$> decode <*> decode <*> decode

instance Serialise (Step Name Type RawBuiltin SpanInfo) where
  encode (Step t mt) = encodeWord 0 <> encodeListLen 2 <> encode t <> encode mt
  encode (StepWithRollback t rb mt) = encodeWord 1 <> encodeListLen 3
    <> encode t <> encode rb <> encode mt

  decode = decodeWord >>= \case
    0 -> do
      2 <- decodeListLen
      Step <$> decode <*> decode
    1 -> do
      3 <- decodeListLen
      StepWithRollback <$> decode <*> decode <*> decode
    _ -> fail "unexpected decoding"

instance Serialise (DefPact Name Type RawBuiltin SpanInfo) where
  encode (DefPact n args ret steps i) = encodeListLen 5 <> encode n <> encode args
    <> encode ret <> encode steps <> encode i

  decode = do
    5 <- decodeListLen
    DefPact <$> decode <*> decode <*> decode <*> decode <*> decode

instance
  Serialise (Def Name Type RawBuiltin SpanInfo) where
  encode (Dfun df) = encodeListLen 2 <> encodeWord 0 <> encode df
  encode (DConst dc) = encodeListLen 2 <> encodeWord 1 <> encode dc
  encode (DCap cap) = encodeListLen 2 <> encodeWord 2 <> encode cap
  encode (DSchema schema) = encodeListLen 2 <> encodeWord 3 <> encode schema
  encode (DTable table) = encodeListLen 2 <> encodeWord 4 <> encode table
  encode (DPact dp) = encodeListLen 2 <> encodeWord 5 <> encode dp

  decode = do
    2 <- decodeListLen
    decodeWord >>= \case
      0 -> Dfun <$> decode
      1 -> DConst <$> decode
      2 -> DCap <$> decode
      3 -> DSchema <$> decode
      4 -> DTable <$> decode
      5 -> DPact <$> decode
      _ -> fail "unexpected decoding"

instance Serialise DynamicRef where
  encode (DynamicRef n b) = encodeListLen 2 <> encode n <> encode b
  decode = do
    2 <- decodeListLen
    DynamicRef <$> decode <*> decode

instance Serialise NameKind where
  encode (NBound d) = encodeWord 0 <> encode d
  encode (NTopLevel mn mh) = encodeWord 1 <> encodeListLen 2 <> encode mn <> encode mh
  encode (NModRef mn ms) = encodeWord 2 <> encodeListLen 2 <> encode mn <> encode ms
  encode (NDynRef dref) = encodeWord 3 <> encode dref

  decode = decodeWord >>= \case
    0 -> NBound <$> decode
    1 -> do
      2 <- decodeListLen
      NTopLevel <$> decode <*> decode
    2 -> do
      2 <- decodeListLen
      NModRef <$> decode <*> decode
    3 -> NDynRef <$> decode
    _ -> fail "unexpected decoding"

instance Serialise Name where
  encode (Name n k) = encodeListLen 2 <> encode n <> encode k
  decode = do
    2 <- decodeListLen
    Name <$> decode <*> decode

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
  encode (Schema m) = encode m
  decode = Schema <$> decode

instance Serialise Type where
  encode (TyPrim pt) = encodeWord 0 <> encode pt
  encode (TyList ty) = encodeWord 1 <> encode ty
  encode (TyModRef mr) = encodeWord 2 <> encode mr
  encode (TyObject s) = encodeWord 3 <> encode s
  encode (TyTable s) = encodeWord 4 <> encode s

  decode = decodeWord >>= \case
    0 -> TyPrim <$> decode
    1 -> TyList <$> decode
    2 -> TyModRef <$> decode
    3 -> TyObject <$> decode
    4 -> TyTable <$> decode
    _ -> fail "unexpected decoding"

instance Serialise Import where
  encode (Import mn mh mimp) = encodeListLen 3 <> encode mn <> encode mh <> encode mimp
  decode = do
    3 <- decodeListLen
    Import <$> decode <*> decode <*> decode

instance Serialise (EvalModule RawBuiltin SpanInfo) where
  encode (Module mn mg mdef mbless mimports mimpl mhash minfo) =
    encodeListLen 8 <> encode mn <> encode mg <> encode mdef
    <> encode mbless <> encode mimports <> encode mimpl
    <> encode mhash <> encode minfo

  decode = do
    8 <- decodeListLen
    Module <$> decode <*> decode <*> decode <*> decode <*> decode <*>
     decode <*> decode <*> decode



instance Serialise (IfDefun Type SpanInfo) where
  encode (IfDefun n args ret i) = encodeListLen 4
    <> encode n <> encode args <> encode ret
    <> encode i

  decode = do
    4 <- decodeListLen
    IfDefun <$> decode <*> decode
      <*> decode <*> decode

instance Serialise (IfDefCap Type SpanInfo) where
  encode (IfDefCap n args ret i) = encodeListLen 4
    <> encode n <> encode args
    <> encode ret <> encode i

  decode = do
    4 <- decodeListLen
    IfDefCap <$> decode <*> decode <*> decode <*> decode

instance Serialise (IfDefPact Type SpanInfo) where
  encode (IfDefPact n args ret i) = encodeListLen 4
    <> encode n <> encode args
    <> encode ret <> encode i

  decode = do
    4 <- decodeListLen
    IfDefPact <$> decode <*> decode <*> decode <*> decode

instance
  Serialise (IfDef Name Type RawBuiltin SpanInfo) where
  encode (IfDfun df) = encodeListLen 2 <> encodeWord 0 <> encode df
  encode (IfDConst dc) = encodeListLen 2 <> encodeWord 1 <> encode dc
  encode (IfDCap cap) = encodeListLen 2 <> encodeWord 2 <> encode cap
  encode (IfDSchema schema) = encodeListLen 2 <> encodeWord 3 <> encode schema
  encode (IfDPact dp) = encodeListLen 2 <> encodeWord 4 <> encode dp

  decode = do
    2 <- decodeListLen
    decodeWord >>= \case
      0 -> IfDfun <$> decode
      1 -> IfDConst <$> decode
      2 -> IfDCap <$> decode
      3 -> IfDSchema <$> decode
      4 -> IfDPact <$> decode
      _ -> fail "unexpected decoding"

instance Serialise (EvalInterface RawBuiltin SpanInfo) where
  encode (Interface n defs h i) = encodeListLen 4 <> encode n <> encode defs <> encode h <> encode i
  decode = do
    4 <- decodeListLen
    Interface <$> decode <*> decode <*> decode <*> decode

instance Serialise (ModuleData RawBuiltin SpanInfo) where
  encode = \case
    ModuleData em m -> encodeWord 0 <> encodeListLen 2 <> encode em <> encode m
    InterfaceData ei m -> encodeWord 1 <> encodeListLen 2 <> encode ei <> encode m

  decode = decodeWord >>= \case
    0 -> do
      2 <- decodeListLen
      ModuleData <$> decode <*> decode
    1 -> do
      2 <- decodeListLen
      InterfaceData <$> decode <*> decode
    _ -> fail "unexpected decoding"

instance Serialise SpanInfo where
  encode (SpanInfo sl sc el ec) = encodeListLen 4 <> encode sl <> encode sc <> encode el <> encode ec
  decode = do
    4 <- decodeListLen
    SpanInfo <$> decode <*> decode <*> decode <*> decode

instance Serialise RawBuiltin where
  encode = \case
    RawAdd -> encodeWord 0
    RawSub-> encodeWord 1
    RawMultiply -> encodeWord 2
    RawDivide -> encodeWord 3
    RawNegate -> encodeWord 4
    RawAbs -> encodeWord 5
    RawPow -> encodeWord 6
    RawNot -> encodeWord 7
    RawEq -> encodeWord 8
    RawNeq -> encodeWord 9
    RawGT -> encodeWord 10
    RawGEQ -> encodeWord 11
    RawLT -> encodeWord 12
    RawLEQ -> encodeWord 13
    RawBitwiseAnd -> encodeWord 14
    RawBitwiseOr -> encodeWord 15
    RawBitwiseXor -> encodeWord 16
    RawBitwiseFlip -> encodeWord 17
    RawBitShift -> encodeWord 18
    RawRound -> encodeWord 19
    RawCeiling -> encodeWord 20
    RawFloor -> encodeWord 21
    RawExp -> encodeWord 22
    RawLn -> encodeWord 23
    RawSqrt -> encodeWord 24
    RawLogBase -> encodeWord 25
    RawLength -> encodeWord 26
    RawTake -> encodeWord 27
    RawDrop -> encodeWord 28
    RawConcat -> encodeWord 29
    RawReverse -> encodeWord 30
    RawContains -> encodeWord 31
    RawSort -> encodeWord 32
    RawSortObject -> encodeWord 33
    RawRemove -> encodeWord 34
    RawMod -> encodeWord 35
    RawMap -> encodeWord 36
    RawFilter -> encodeWord 37
    RawZip -> encodeWord 38
    RawIntToStr -> encodeWord 39
    RawStrToInt -> encodeWord 40
    RawStrToIntBase -> encodeWord 41
    RawFold -> encodeWord 42
    RawDistinct -> encodeWord 43
    RawFormat -> encodeWord 44
    RawEnumerate -> encodeWord 45
    RawEnumerateStepN -> encodeWord 46
    RawShow -> encodeWord 47
    RawReadMsg -> encodeWord 48
    RawReadMsgDefault -> encodeWord 49
    RawReadInteger -> encodeWord 50
    RawReadDecimal -> encodeWord 51
    RawReadString -> encodeWord 52
    RawReadKeyset -> encodeWord 53
    RawEnforceGuard -> encodeWord 54
    RawEnforceKeyset -> encodeWord 55
    RawKeysetRefGuard -> encodeWord 56
    RawAt -> encodeWord 57
    RawMakeList -> encodeWord 58
    RawB64Encode -> encodeWord 59
    RawB64Decode -> encodeWord 60
    RawStrToList -> encodeWord 61
    RawYield -> encodeWord 62
    RawResume -> encodeWord 63
    RawBind -> encodeWord 64
    RawRequireCapability -> encodeWord 65
    RawComposeCapability -> encodeWord 66
    RawInstallCapability -> encodeWord 67
    RawEmitEvent -> encodeWord 68
    RawCreateCapabilityGuard -> encodeWord 69
    RawCreateCapabilityPactGuard -> encodeWord 70
    RawCreateModuleGuard -> encodeWord 71
    RawCreateTable -> encodeWord 72
    RawDescribeKeyset -> encodeWord 73
    RawDescribeModule -> encodeWord 74
    RawDescribeTable -> encodeWord 75
    RawDefineKeySet -> encodeWord 76
    RawDefineKeysetData -> encodeWord 77
    RawFoldDb -> encodeWord 78
    RawInsert -> encodeWord 79
    RawKeyLog -> encodeWord 80
    RawKeys -> encodeWord 81
    RawRead -> encodeWord 82
    RawSelect -> encodeWord 83
    RawSelectWithFields -> encodeWord 84
    RawUpdate -> encodeWord 85
    RawWithDefaultRead -> encodeWord 86
    RawWithRead -> encodeWord 87
    RawWrite -> encodeWord 88
    RawTxIds -> encodeWord 89
    RawTxLog -> encodeWord 90
    RawTxHash -> encodeWord 91
    RawAndQ -> encodeWord 92
    RawOrQ -> encodeWord 93
    RawWhere -> encodeWord 94
    RawNotQ -> encodeWord 95
    RawHash -> encodeWord 96
    RawContinue -> encodeWord 97
    RawParseTime -> encodeWord 98
    RawFormatTime -> encodeWord 99
    RawTime -> encodeWord 100
    RawAddTime -> encodeWord 101
    RawDiffTime -> encodeWord 102
    RawHours -> encodeWord 103
    RawMinutes -> encodeWord 104
    RawDays -> encodeWord 105
    RawCompose -> encodeWord 106
    RawNamespace -> encodeWord 107
    RawDefineNamespace -> encodeWord 108
    RawDescribeNamespace -> encodeWord 109

  decode = decodeWord >>= \case
    0 -> pure RawAdd
    1 -> pure RawSub
    2 -> pure RawMultiply
    3 -> pure RawDivide
    4 -> pure RawNegate
    5 -> pure RawAbs
    6 -> pure RawPow
    7 -> pure RawNot
    8 -> pure RawEq
    9 -> pure RawNeq
    10 -> pure RawGT
    11 -> pure RawGEQ
    12 -> pure RawLT
    13 -> pure RawLEQ
    14 -> pure RawBitwiseAnd
    15 -> pure RawBitwiseOr
    16 -> pure RawBitwiseXor
    17 -> pure RawBitwiseFlip
    18 -> pure RawBitShift
    19 -> pure RawRound
    20 -> pure RawCeiling
    21 -> pure RawFloor
    22 -> pure RawExp
    23 -> pure RawLn
    24 -> pure RawSqrt
    25 -> pure RawLogBase
    26 -> pure RawLength
    27 -> pure RawTake
    28 -> pure RawDrop
    29 -> pure RawConcat
    30 -> pure RawReverse
    31 -> pure RawContains
    32 -> pure RawSort
    33 -> pure RawSortObject
    34 -> pure RawRemove
    35 -> pure RawMod
    36 -> pure RawMap
    37 -> pure RawFilter
    38 -> pure RawZip
    39 -> pure RawIntToStr
    40 -> pure RawStrToInt
    41 -> pure RawStrToIntBase
    42 -> pure RawFold
    43 -> pure RawDistinct
    44 -> pure RawFormat
    45 -> pure RawEnumerate
    46 -> pure RawEnumerateStepN
    47 -> pure RawShow
    48 -> pure RawReadMsg
    49 -> pure RawReadMsgDefault
    50 -> pure RawReadInteger
    51 -> pure RawReadDecimal
    52 -> pure RawReadString
    53 -> pure RawReadKeyset
    54 -> pure RawEnforceGuard
    55 -> pure RawEnforceKeyset
    56 -> pure RawKeysetRefGuard
    57 -> pure RawAt
    58 -> pure RawMakeList
    59 -> pure RawB64Encode
    60 -> pure RawB64Decode
    61 -> pure RawStrToList
    62 -> pure RawYield
    63 -> pure RawResume
    64 -> pure RawBind
    65 -> pure RawRequireCapability
    66 -> pure RawComposeCapability
    67 -> pure RawInstallCapability
    68 -> pure RawEmitEvent
    69 -> pure RawCreateCapabilityGuard
    70 -> pure RawCreateCapabilityPactGuard
    71 -> pure RawCreateModuleGuard
    72 -> pure RawCreateTable
    73 -> pure RawDescribeKeyset
    74 -> pure RawDescribeModule
    75 -> pure RawDescribeTable
    76 -> pure RawDefineKeySet
    77 -> pure RawDefineKeysetData
    78 -> pure RawFoldDb
    79 -> pure RawInsert
    80 -> pure RawKeyLog
    81 -> pure RawKeys
    82 -> pure RawRead
    83 -> pure RawSelect
    84 -> pure RawSelectWithFields
    85 -> pure RawUpdate
    86 -> pure RawWithDefaultRead
    87 -> pure RawWithRead
    88 -> pure RawWrite
    89 -> pure RawTxIds
    90 -> pure RawTxLog
    91 -> pure RawTxHash
    92 -> pure RawAndQ
    93 -> pure RawOrQ
    94 -> pure RawWhere
    95 -> pure RawNotQ
    96 -> pure RawHash
    97 -> pure RawContinue
    98 -> pure RawParseTime
    99 -> pure RawFormatTime
    100 -> pure RawTime
    101 -> pure RawAddTime
    102 -> pure RawDiffTime
    103 -> pure RawHours
    104 -> pure RawMinutes
    105 -> pure RawDays
    106 -> pure RawCompose
    107 -> pure RawNamespace
    108 -> pure RawDefineNamespace
    109 -> pure RawDescribeNamespace
    _ -> fail "unexpeced decoding"
