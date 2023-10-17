{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}

module Pact.Core.Builtin
 ( RawBuiltin(..)
 , rawBuiltinToText
 , rawBuiltinMap
 , rawBuiltinNames
 , ReplBuiltin(..)
 , replRawBuiltinNames
 , replRawBuiltinMap
 , IsBuiltin(..)
--  , CapabilityOp(..)
--  , CapType(..)
--  , DefType(..)
 , CoreBuiltin(..)
 , ReplRawBuiltin
 , ReplCoreBuiltin
 , BuiltinForm(..)
 , ReplBuiltins(..)
 , HasObjectOps(..)
 )where

import Data.Text(Text)
import Data.Map.Strict(Map)

import qualified Data.Map.Strict as M

import Pact.Core.Names(NativeName(..))
import Pact.Core.Pretty

type ReplRawBuiltin = ReplBuiltin RawBuiltin
type ReplCoreBuiltin = ReplBuiltin CoreBuiltin

data BuiltinForm o
  = CAnd o o
  | COr o o
  | CIf o o o
  | CEnforceOne o [o]
  | CEnforce o o
  deriving (Show, Eq, Functor, Foldable, Traversable)

instance Pretty o => Pretty (BuiltinForm o) where
  pretty = \case
    CAnd o o' ->
      parens ("and" <+> pretty o <+> pretty o')
    COr o o' ->
      parens ("or" <+> pretty o <+> pretty o')
    CIf o o' o3 ->
      parens ("if" <+> pretty o <+> pretty o' <+> pretty o3)
    CEnforceOne o li ->
      parens ("enforce-one" <+> pretty o <+> brackets (hsep (punctuate comma (pretty <$> li))))
    CEnforce o o' ->
      parens ("enforce" <+> pretty o <+> pretty o')
    -- CFold e1 e2 e3 ->
    --   parens ("fold" <+> pretty e1 <+> pretty e2 <+> pretty e3)
    -- CMap e1 e2 ->
    --   parens ("map" <+> pretty e1 <+> pretty e2)
    -- CZip e1 e2 e3 ->
    --   parens ("zip" <+> pretty e1 <+> pretty e2 <+> pretty e3)

class HasObjectOps b where
  objectAt :: b

data DefType
  = DTDefun
  | DTDefcap
  | DTDefConst
  deriving Show

{-
  [Typeclasses and Instances]
  Builtin operator overloads, grouped by the current type class hierarchy:

  class Add a where
    (+) :: a -> a -> a

  instance Add integer
  instance Add decimal
  instance Add string
  instance Add (list a)

  class Eq a where
    (==) :: a -> a -> bool
    (/=) :: a -> a -> bool

  instance Eq integer
  instance Eq decimal
  instance Eq string
  instance Eq time
  instance Eq unit
  instance Eq bool
  instance (Eq a) => Eq (list a)
  -- todo: rows

  class Ord a where
    (>=) :: a -> a -> bool
    (>) :: a -> a -> bool
    (<) :: a -> a -> bool
    (<=) :: a -> a -> bool

  instance Ord integer
  instance Ord decimal
  instance Ord string
  instance Ord time
  instance Ord unit
  instance Ord a => Ord (list a)

  class Show a where
    show :: a -> string

  instance Show integer
  instance Show decimal
  instance Show string
  instance Show time
  instance Show unit
  instance Show bool
  instance (Show a) => Show (list a)

  class Num a where
    (-) :: a -> a -> a
    (*) :: a -> a -> a
    (/) :: a -> a -> a
    abs :: a -> a
    negate :: a -> a

  instance Num integer
  instance Num decimal

  class Fractional a where
    ln :: a -> decimal
    exp :: a -> decimal
    sqrt :: a -> decimal
    log-base :: a -> a -> a

  instance Fractional integer
  instance Fractional decimal

  class ListLike a where
    take :: integer -> a -> a
    drop :: integer -> a -> a
    concat :: [a] -> a
    reverse :: a -> a
    length :: a -> integer

  instance ListList string
  instance ListLike (list a)
-}
data RawBuiltin
  -- Operators
  -- Addition/Concatenation
  = RawAdd
  -- Num
  | RawSub
  | RawMultiply
  | RawDivide
  | RawNegate
  | RawAbs
  | RawPow
  -- Boolean Ops
  -- | RawAnd
  -- | RawOr
  | RawNot
  -- Equality and Comparisons
  | RawEq
  | RawNeq
  -- Ord
  | RawGT
  | RawGEQ
  | RawLT
  | RawLEQ
  -- Bitwise Ops
  | RawBitwiseAnd
  | RawBitwiseOr
  | RawBitwiseXor
  | RawBitwiseFlip
  | RawBitShift
  --  Rounding
  | RawRound
  | RawCeiling
  | RawFloor
  -- Fractional
  | RawExp
  | RawLn
  | RawSqrt
  | RawLogBase
  -- List like
  | RawLength
  | RawTake
  | RawDrop
  | RawConcat
  | RawReverse
  | RawContains
  | RawSort
  | RawSortObject
  | RawRemove
  -- General
  | RawMod
  | RawMap
  | RawFilter
  | RawZip
  | RawIntToStr
  | RawStrToInt
  | RawStrToIntBase
  | RawFold
  | RawDistinct
  | RawFormat
  -- | RawEnforce
  -- | RawEnforceOne
  | RawEnumerate
  | RawEnumerateStepN
  -- Guards + read functions
  | RawShow
  | RawReadMsg
  | RawReadMsgDefault
  | RawReadInteger
  | RawReadDecimal
  | RawReadString
  | RawReadKeyset
  | RawEnforceGuard
  | RawEnforceKeyset
  | RawKeysetRefGuard
  | RawAt
  | RawMakeList
  | RawB64Encode
  | RawB64Decode
  | RawStrToList
  | RawBind
  | RawRequireCapability
  | RawComposeCapability
  | RawInstallCapability
  | RawEmitEvent
  | RawCreateCapabilityGuard
  | RawCreateModuleGuard
  -- Database functions
  | RawCreateTable
  | RawDescribeKeyset
  | RawDescribeModule
  | RawDescribeTable
  | RawDefineKeySet
  | RawDefineKeysetData
  | RawFoldDb
  | RawInsert
  | RawKeyLog
  | RawKeys
  | RawRead
  | RawSelect
  | RawSelectWithFields
  | RawUpdate
  | RawWithDefaultRead
  | RawWithRead
  | RawWrite
  | RawTxIds
  | RawTxLog
  | RawTxHash
  -- Db QueryFunctions
  | RawAndQ
  | RawOrQ
  | RawWhere
  | RawNotQ
  | RawHash
  -- Time functions
  | RawParseTime
  | RawFormatTime
  | RawTime
  | RawAddTime
  | RawDiffTime
  | RawHours
  | RawMinutes
  | RawDays
  | RawCompose
  deriving (Eq, Show, Ord, Bounded, Enum)

instance HasObjectOps RawBuiltin where
  objectAt = RawAt

rawBuiltinToText :: RawBuiltin -> Text
rawBuiltinToText = \case
  -- Addition
  RawAdd -> "+"
  -- Num
  RawSub -> "-"
  RawMultiply -> "*"
  RawDivide -> "/"
  RawNegate -> "negate"
  RawAbs -> "abs"
  RawPow -> "^"
  -- Bolean ops
  -- RawAnd -> "and"
  -- RawOr -> "or"
  RawNot -> "not"
  -- Eq
  RawEq -> "="
  RawNeq -> "!="
  -- Ord
  RawGT -> ">"
  RawGEQ -> ">="
  RawLT -> "<"
  RawLEQ -> "<="
  -- Int ops
  RawBitwiseAnd -> "&"
  RawBitwiseOr -> "|"
  RawBitwiseXor -> "xor"
  RawBitwiseFlip -> "~"
  RawBitShift -> "shift"
  RawMod -> "mod"
  -- roundings
  RawRound -> "round"
  RawCeiling -> "ceiling"
  RawFloor -> "floor"
  -- Fractional
  RawExp -> "exp"
  RawLn -> "ln"
  RawSqrt -> "sqrt"
  RawLogBase -> "log"
  -- ListLike
  RawLength -> "length"
  RawTake -> "take"
  RawDrop -> "drop"
  RawConcat -> "concat"
  RawReverse -> "reverse"
  -- general
  RawMap -> "map"
  RawFilter -> "filter"
  RawContains -> "contains"
  RawSort -> "sort"
  RawSortObject -> "sort-object"
  RawRemove -> "remove"
  -- RawIf -> "if"
  RawIntToStr -> "int-to-str"
  RawStrToInt -> "str-to-int"
  RawStrToIntBase -> "str-to-int-base"
  RawFold -> "fold"
  RawZip -> "zip"
  RawDistinct -> "distinct"
  RawFormat -> "format"
  -- RawEnforce -> "enforce"
  -- RawEnforceOne -> "enforce-one"
  RawEnumerate -> "enumerate"
  RawEnumerateStepN -> "enumerate-step"
  RawShow -> "show"
  RawReadMsg -> "read-msg"
  RawReadMsgDefault -> "read-msg-default"
  RawReadInteger -> "read-integer"
  RawReadDecimal -> "read-decimal"
  RawReadString -> "read-string"
  RawReadKeyset -> "read-keyset"
  RawEnforceGuard -> "enforce-guard"
  RawEnforceKeyset -> "enforce-keyset"
  RawKeysetRefGuard -> "keyset-ref-guard"
  RawCreateCapabilityGuard -> "create-capability-guard"
  RawCreateModuleGuard -> "create-module-guard"
  RawAt -> "at"
  RawMakeList -> "make-list"
  RawB64Encode -> "base64-encode"
  RawB64Decode -> "base64-decode"
  RawStrToList -> "str-to-list"
  RawBind -> "bind"
  RawRequireCapability -> "require-capability"
  RawComposeCapability -> "compose-capability"
  RawInstallCapability -> "install-capability"
  RawEmitEvent -> "emit-event"
  RawCreateTable -> "create-table"
  RawDescribeKeyset -> "describe-keyset"
  RawDescribeModule -> "describe-module"
  RawDescribeTable -> "describe-table"
  RawDefineKeySet -> "define-keyset"
  RawDefineKeysetData -> "define-read-keyset"
  RawFoldDb -> "fold-db"
  RawInsert -> "insert"
  RawKeyLog -> "keylog"
  RawKeys -> "keys"
  RawRead -> "read"
  RawSelect -> "select"
  RawSelectWithFields -> ""
  RawUpdate -> "update"
  RawWithDefaultRead -> "with-default-read"
  RawWithRead -> "with-read"
  RawWrite -> "write"
  RawTxIds -> "txids"
  RawTxLog -> "txlog"
  RawTxHash -> "tx-hash"
  RawAndQ -> "and?"
  RawOrQ -> "or?"
  RawWhere -> "where"
  RawNotQ -> "not?"
  RawHash -> "hash"
  RawParseTime -> "parse-time"
  RawFormatTime -> "format-time"
  RawTime -> "time"
  RawAddTime -> "add-time"
  RawDiffTime -> "diff-time"
  RawHours -> "hours"
  RawMinutes -> "minutes"
  RawDays -> "days"
  RawCompose -> "compose"

instance IsBuiltin RawBuiltin where
  builtinName = NativeName . rawBuiltinToText
  builtinArity = \case
    RawAdd -> 2
    -- Num ->
    RawSub -> 2
    RawMultiply -> 2
    RawDivide -> 2
    RawNegate -> 1
    RawAbs -> 1
    RawPow -> 2
    -- Boolean Ops ->
    -- RawAnd -> 2
    -- RawOr -> 2
    RawNot -> 1
    -- Equality and Comparisons ->
    RawEq -> 2
    RawNeq -> 2
    -- Ord ->
    RawGT -> 2
    RawGEQ -> 2
    RawLT -> 2
    RawLEQ -> 2
    -- Bitwise Ops ->
    RawBitwiseAnd -> 2
    RawBitwiseOr -> 2
    RawBitwiseXor -> 2
    RawBitwiseFlip -> 1
    RawBitShift -> 2
    --  Rounding ->
    RawRound -> 1
    RawCeiling -> 1
    RawFloor -> 1
    -- Fractional ->
    RawExp -> 1
    RawLn -> 1
    RawSqrt -> 1
    RawLogBase -> 2
    -- List like ->
    RawLength -> 1
    RawTake -> 2
    RawDrop -> 2
    RawConcat -> 1
    RawReverse -> 1
    RawContains -> 2
    RawSort -> 1
    RawSortObject -> 2
    RawRemove -> 2
    -- General ->
    RawMod -> 2
    RawMap -> 2
    RawFilter -> 2
    RawZip -> 3
    -- RawIf -> 3
    RawIntToStr -> 2
    RawStrToInt -> 1
    RawStrToIntBase -> 2
    RawFold -> 3
    RawDistinct -> 1
    RawFormat -> 2
    -- RawEnforce -> 2
    -- RawEnforceOne -> 2
    RawEnumerate -> 2
    RawEnumerateStepN -> 3
    -- Show ->
    RawShow -> 1
    RawReadMsg -> 1
    RawReadMsgDefault -> 0
    RawReadInteger -> 1
    RawReadDecimal -> 1
    RawReadString -> 1
    RawReadKeyset -> 1
    RawEnforceKeyset -> 1
    RawEnforceGuard -> 1
    RawKeysetRefGuard -> 1
    RawCreateCapabilityGuard -> 1
    RawCreateModuleGuard -> 1
    RawAt -> 2
    RawMakeList -> 2
    RawB64Encode -> 1
    RawB64Decode -> 1
    RawStrToList -> 1
    RawBind -> 2
    RawRequireCapability -> 1
    RawComposeCapability -> 1
    RawInstallCapability -> 1
    RawEmitEvent -> 1
    RawCreateTable -> 1
    RawDescribeKeyset -> 1
    RawDescribeModule -> 1
    RawDescribeTable -> 1
    RawDefineKeySet -> 2
    RawDefineKeysetData -> 1
    RawFoldDb -> 3
    RawInsert -> 3
    RawKeyLog -> 3
    RawKeys -> 1
    RawRead -> 2
    RawSelect -> 2
    RawSelectWithFields -> 3
    RawUpdate -> 3
    RawWithDefaultRead -> 4
    RawWithRead -> 3
    RawWrite -> 3
    RawTxIds -> 2
    RawTxLog -> 2
    RawTxHash -> 0
    RawAndQ -> 3
    RawOrQ -> 3
    RawWhere -> 3
    RawNotQ -> 2
    RawHash -> 1
    RawParseTime -> 2
    RawFormatTime -> 2
    RawTime -> 1
    RawAddTime -> 2
    RawDiffTime -> 2
    RawHours -> 1
    RawMinutes -> 1
    RawDays -> 1
    RawCompose -> 3


rawBuiltinNames :: [Text]
rawBuiltinNames = fmap rawBuiltinToText [minBound .. maxBound]

rawBuiltinMap :: Map Text RawBuiltin
rawBuiltinMap = M.fromList $ (\b -> (rawBuiltinToText b, b)) <$> [minBound .. maxBound]

-- Todo: rename
-- | Our repl builtins.
data ReplBuiltins
  = RExpect
  | RExpectFailure
  | RExpectFailureMatch
  | RExpectThat
  | RPrint
  | REnvStackFrame
  | REnvChainData
  | REnvData
  | REnvEvents
  | REnvHash
  | REnvKeys
  | REnvSigs
  | RBeginTx
  | RBeginNamedTx
  | RCommitTx
  | RRollbackTx
  | RSigKeyset
  | RTestCapability
  | REnvExecConfig
  -- | RLoad
  -- | RLoadWithEnv
  -- | RExpect
  -- | RExpectFailure
  -- | RExpectThat
  -- | RPactState
  -- | RRollbackTx
  -- | RSigKeyset
  -- | RTestCapability
  -- | RVerify
  -- | RWithAppliedEnv
  -- | REnvEnableReplNatives
  -- | RBeginTx
  -- | RBench
  -- | RCommitTx
  -- | RContinuePact
  -- | REnvExecConfig
  -- | REnvGas
  -- | REnvGasLimit
  -- | REnvGasLog
  -- | REnvGasModel
  -- | REnvGasPrice
  -- | REnvGasRate
  -- | REnvNamespacePolicy
  deriving (Show, Enum, Bounded, Eq)


instance IsBuiltin ReplBuiltins where
  builtinName = NativeName . replBuiltinsToText
  builtinArity = \case
    RExpect -> 3
    RExpectFailure -> 2
    RExpectFailureMatch -> 3
    RExpectThat -> 3
    RPrint -> 1
    REnvStackFrame -> 0
    REnvChainData -> 1
    REnvData -> 1
    REnvEvents -> 1
    REnvHash -> 1
    REnvKeys -> 1
    REnvSigs -> 1
    RBeginTx -> 0
    RBeginNamedTx -> 1
    RCommitTx -> 0
    RRollbackTx -> 0
    RSigKeyset -> 1
    RTestCapability -> 1
    REnvExecConfig -> 1
    -- RLoad -> 1
    -- RLoadWithEnv -> 2
-- Note: commented out natives are
-- to be implemented later
data ReplBuiltin b
  = RBuiltinWrap b
  | RBuiltinRepl ReplBuiltins
  deriving (Eq, Show)

instance HasObjectOps b => HasObjectOps (ReplBuiltin b) where
  objectAt = RBuiltinWrap objectAt

-- NOTE: Maybe `ReplBuiltin` is not a great abstraction, given
-- expect arity changes based on whether it's corebuiltin or rawbuiltin
instance IsBuiltin b => IsBuiltin (ReplBuiltin b) where
  builtinName = NativeName . replBuiltinToText (_natName . builtinName)
  builtinArity = \case
    RBuiltinWrap b -> builtinArity b
    RBuiltinRepl b -> builtinArity b

    -- RLoad -> 1

instance Bounded b => Bounded (ReplBuiltin b) where
  minBound = RBuiltinWrap minBound
  maxBound = RBuiltinRepl maxBound

instance (Enum b, Bounded b) => Enum (ReplBuiltin b) where
  toEnum  i =
    if i <= mbound then RBuiltinWrap (toEnum i)
    else RBuiltinRepl (toEnum (i - mbound - 1))
    where
    mbound = fromEnum (maxBound :: b)
  {-# SPECIALISE toEnum :: Int -> ReplBuiltin RawBuiltin #-}

  fromEnum e =
    let maxContained = fromEnum (maxBound :: b) + 1
    in case e of
      RBuiltinWrap b -> fromEnum b
      RBuiltinRepl rb -> maxContained + fromEnum rb
  {-# SPECIALISE fromEnum :: ReplBuiltin RawBuiltin -> Int #-}

replBuiltinsToText :: ReplBuiltins -> Text
replBuiltinsToText = \case
  RExpect -> "expect"
  RExpectFailure -> "expect-failure"
  RExpectFailureMatch -> "expect-failure-match"
  RExpectThat -> "expect-that"
  RPrint -> "print"
  REnvStackFrame -> "env-stackframe"
  REnvChainData -> "env-chain-data"
  REnvData -> "env-data"
  REnvEvents -> "env-events"
  REnvHash -> "env-hash"
  REnvKeys -> "env-keys"
  REnvSigs -> "env-sigs"
  RBeginTx -> "begin-tx"
  RBeginNamedTx -> "begin-named-tx"
  RCommitTx -> "commit-tx"
  RRollbackTx -> "rollback-tx"
  RSigKeyset -> "sig-keyset"
  RTestCapability -> "test-capability"
  REnvExecConfig -> "env-exec-config"
  -- RLoad -> "load"
  -- RLoadWithEnv -> "load-with-env"

replBuiltinToText :: (t -> Text) -> ReplBuiltin t -> Text
replBuiltinToText f = \case
  RBuiltinWrap b -> f b
  RBuiltinRepl rb -> replBuiltinsToText rb

replRawBuiltinNames :: [Text]
replRawBuiltinNames = fmap (replBuiltinToText rawBuiltinToText) [minBound .. maxBound]

replRawBuiltinMap :: Map Text (ReplBuiltin RawBuiltin)
replRawBuiltinMap =
  M.fromList $ (\b -> (replBuiltinToText rawBuiltinToText b, b)) <$> [minBound .. maxBound]

-- Todo: is not a great abstraction.
-- In particular: the arity could be gathered from the type.
class Show b => IsBuiltin b where
  builtinArity :: b -> Int
  builtinName :: b -> NativeName


instance Pretty RawBuiltin where
  pretty b = pretty (rawBuiltinToText b)

instance (Pretty b) => Pretty (ReplBuiltin b) where
  pretty = \case
    RBuiltinWrap b -> pretty b
    t -> pretty (replBuiltinToText (const "") t)

-- monomorphised builtin operations
data CoreBuiltin
  -- IntOps
  -- Integer Add
  = AddInt
  -- Int Num functions
  | SubInt
  | DivInt
  | MulInt
  | NegateInt
  | AbsInt
  | PowInt
  -- Int fractional
  | ExpInt
  | LnInt
  | SqrtInt
  | LogBaseInt
  -- General int ops
  | ModInt
  | BitAndInt
  | BitOrInt
  | BitXorInt
  | BitShiftInt
  | BitComplementInt
  -- Int show instance
  | ShowInt
  -- Int Equality
  | EqInt
  | NeqInt
  | GTInt
  | GEQInt
  | LTInt
  | LEQInt
  -- If
  -- | IfElse
  -- Decimal ops
  -- Decimal add
  | AddDec
  -- Decimal num
  | SubDec
  | DivDec
  | MulDec
  | NegateDec
  | AbsDec
  | PowDec
  -- Decimal rounding ops
  | RoundDec
  | CeilingDec
  | FloorDec
  -- Decimal rounding ops
  | ExpDec
  | LnDec
  | LogBaseDec
  | SqrtDec
  -- Decimal Show
  | ShowDec
  -- Decimal Equality
  | EqDec
  | NeqDec
  -- Decimal ord
  | GTDec
  | GEQDec
  | LTDec
  | LEQDec
  -- Bool Comparisons
  -- | AndBool
  -- | OrBool
  | NotBool
  -- other bool ops
  | EqBool
  | NeqBool
  | ShowBool
  -- String Equality
  | EqStr
  | NeqStr
  -- String Ord
  | GTStr
  | GEQStr
  | LTStr
  | LEQStr
   -- String Add
  | AddStr
  -- String ListLike
  | ConcatStr
  | DropStr
  | TakeStr
  | LengthStr
  | ReverseStr
  -- String Show
  | ShowStr
  -- Object equality
  -- | EqObj
  -- | NeqObj
  -- List Equality
  | EqList
  | NeqList
  -- List Ord
  | GTList
  | GEQList
  | LTList
  | LEQList
  -- List Show
  | ShowList
  -- List Add
  | AddList
  -- ListLike List
  | TakeList
  | DropList
  | LengthList
  | ConcatList
  | ReverseList
  -- Misc list ops
  | FilterList
  | DistinctList
  | MapList
  | ZipList
  | FoldList
  -- Unit ops
  | EqUnit
  | NeqUnit
  | ShowUnit
  -- Module references
  | EqModRef
  | NeqModRef
  -- Others
  | Enforce
  | EnforceOne
  | Enumerate
  | EnumerateStepN
  | ReadInteger
  | ReadDecimal
  | ReadString
  | ReadKeyset
  | EnforceGuard
  | KeysetRefGuard
  -- List ops
  | ListAccess
  | MakeList
  | B64Encode
  | B64Decode
  | StrToList
  deriving (Eq, Show, Ord, Bounded, Enum)


instance Pretty CoreBuiltin where
  pretty = pretty . coreBuiltinToText

instance IsBuiltin CoreBuiltin where
  builtinName = NativeName . coreBuiltinToText
  builtinArity = \case
    AddInt -> 2
    SubInt -> 2
    DivInt -> 2
    MulInt -> 2
    PowInt -> 2
    NegateInt -> 1
    AbsInt -> 1
    ExpInt -> 1
    LnInt -> 1
    SqrtInt -> 1
    LogBaseInt -> 2
    ModInt -> 2
    BitAndInt -> 2
    BitOrInt -> 2
    BitXorInt -> 2
    BitShiftInt -> 2
    BitComplementInt -> 1
    ShowInt -> 1
    EqInt -> 2
    NeqInt -> 2
    GTInt -> 2
    GEQInt -> 2
    LTInt -> 2
    LEQInt -> 2
    -- IfElse -> 3
    AddDec -> 2
    SubDec -> 2
    DivDec -> 2
    MulDec -> 2
    PowDec -> 2
    NegateDec -> 1
    AbsDec -> 1
    RoundDec -> 1
    CeilingDec -> 1
    FloorDec -> 1
    ExpDec -> 1
    LnDec -> 1
    LogBaseDec -> 2
    SqrtDec -> 1
    ShowDec -> 1
    EqDec -> 2
    NeqDec -> 2
    GTDec -> 2
    GEQDec -> 2
    LTDec -> 2
    LEQDec -> 2
    -- AndBool -> 2
    -- OrBool -> 2
    NotBool -> 1
    EqBool -> 2
    NeqBool -> 2
    ShowBool -> 1
    EqStr -> 2
    NeqStr -> 2
    GTStr -> 2
    GEQStr -> 2
    LTStr -> 2
    LEQStr -> 2
    AddStr -> 2
    ConcatStr -> 1
    DropStr -> 2
    TakeStr -> 2
    LengthStr -> 1
    ReverseStr -> 1
    ShowStr -> 1
    -- EqObj -> 2
    -- NeqObj -> 2
    EqList -> 3
    NeqList -> 3
    GTList -> 2
    GEQList -> 2
    LTList -> 2
    LEQList -> 2
    ShowList -> 2
    AddList -> 2
    TakeList -> 2
    DropList -> 2
    LengthList -> 1
    ConcatList -> 1
    ReverseList -> 1
    FilterList -> 2
    DistinctList -> 1
    MapList -> 2
    ZipList -> 3
    FoldList -> 3
    EqUnit -> 2
    NeqUnit -> 2
    ShowUnit -> 1
    -- Module references
    EqModRef -> 2
    NeqModRef -> 2
    Enforce -> 2
    EnforceOne -> 2
    Enumerate -> 2
    EnumerateStepN -> 3
    ReadInteger -> 1
    ReadDecimal -> 1
    ReadString -> 1
    ReadKeyset -> 1
    EnforceGuard -> 1
    KeysetRefGuard -> 1
    -- CreateUserGuard -> 1
    ListAccess -> 2
    MakeList -> 2
    B64Encode -> 1
    B64Decode -> 1
    StrToList -> 1

coreBuiltinToText :: CoreBuiltin -> Text
coreBuiltinToText = \case
-- IntOps
  AddInt -> "addInt"
  -- Int Num functions
  SubInt -> "subInt"
  DivInt -> "divInt"
  MulInt -> "mulInt"
  NegateInt -> "negateInt"
  AbsInt -> "absInt"
  PowInt -> "powInt"
  -- Int fractional
  ExpInt -> "expInt"
  LnInt -> "lnInt"
  SqrtInt -> "sqrtInt"
  LogBaseInt -> "logBaseInt"
  -- General int ops
  ModInt -> "modInt"
  BitAndInt -> "bitAndInt"
  BitOrInt -> "bitOrInt"
  BitXorInt -> "bitXorInt"
  BitShiftInt -> "bitShiftInt"
  BitComplementInt -> "bitComplementInt"
  -- Int show instance
  ShowInt -> "showInt"
  -- Int Equality
  EqInt -> "eqInt"
  NeqInt -> "neqInt"
  GTInt -> "gtInt"
  GEQInt -> "geqInt"
  LTInt -> "ltInt"
  LEQInt -> "leqInt"
  -- If
  -- IfElse -> "ifElse"
  -- Decimal ops
  -- Decimal add
  AddDec -> "addDec"
  -- Decimal num
  SubDec -> "subDec"
  DivDec -> "divDec"
  MulDec -> "mulDec"
  NegateDec -> "negateDec"
  AbsDec -> "absDec"
  PowDec -> "powDec"
  -- Decimal rounding ops
  RoundDec -> "roundDec"
  CeilingDec -> "ceilingDec"
  FloorDec -> "floorDec"
  -- Decimal rounding ops
  ExpDec -> "expDec"
  LnDec -> "lnDec"
  LogBaseDec -> "logBaseDec"
  SqrtDec -> "sqrtDec"
  -- Decimal Show
  ShowDec -> "showDec"
  -- Decimal Equality
  EqDec -> "eqDec"
  NeqDec -> "neqDec"
  -- Decimal ord
  GTDec -> "gtDec"
  GEQDec -> "geqDec"
  LTDec -> "ltDec"
  LEQDec -> "leqDec"
  -- Bool Comparisons
  -- AndBool -> "andBool"
  -- OrBool -> "orBool"
  NotBool -> "notBool"
  -- other bool ops
  EqBool -> "eqBool"
  NeqBool -> "neqBool"
  ShowBool -> "showBool"
  -- String Equality
  EqStr -> "eqStr"
  NeqStr -> "neqStr"
  -- String Ord
  GTStr -> "gtStr"
  GEQStr -> "gtStr"
  LTStr -> "gtStr"
  LEQStr -> "gtStr"
   -- String Add
  AddStr -> "addStr"
  -- String ListLike
  ConcatStr -> "concatStr"
  DropStr -> "dropStr"
  TakeStr -> "takeStr"
  LengthStr -> "lengthStr"
  ReverseStr -> "reverseStr"
  -- String Show
  ShowStr -> "showStr"
  -- Object equality
  -- EqObj -> "eqObj"
  -- NeqObj -> "neqObj"
  -- List Equality
  EqList -> "eqList"
  NeqList -> "neqList"
  -- List Ord
  GTList -> "gtList"
  GEQList -> "geqList"
  LTList -> "ltList"
  LEQList -> "leqList"
  -- List Show
  ShowList -> "showList"
  -- List Add
  AddList -> "addList"
  -- ListLike List
  TakeList -> "takeList"
  DropList -> "dropList"
  LengthList -> "lengthList"
  ConcatList -> "concatList"
  ReverseList -> "reverseList"
  -- Misc list ops
  FilterList -> "filterList"
  DistinctList -> "distinctList"
  MapList -> "mapList"
  ZipList -> "zipList"
  FoldList -> "foldList"
  -- Unit ops
  EqUnit -> "eqUnit"
  NeqUnit -> "neqUnit"
  ShowUnit -> "showUnit"
  -- Module references
  EqModRef -> "eqModRef"
  NeqModRef -> "neqModRef"
  -- Others
  Enforce -> "enforce"
  EnforceOne -> "enforceOn"
  Enumerate -> "enumerate"
  EnumerateStepN -> "enumerateStep"
  ReadInteger -> "read-integer"
  ReadDecimal -> "read-decimal"
  ReadString -> "read-string"
  ReadKeyset -> "read-keyset"
  EnforceGuard -> "enforce-guard"
  KeysetRefGuard -> "keyset-ref-guard"
  -- CreateUserGuard -> "create-user-guard"
  ListAccess -> "at"
  MakeList -> "make-list"
  B64Encode -> "base64-encode"
  B64Decode -> "base64-decode"
  StrToList -> "str-to-list"
