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
 , ReplRawBuiltin
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

class HasObjectOps b where
  objectAt :: b

data DefType
  = DTDefun
  | DTDefcap
  | DTDefConst
  deriving Show

{-

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
  | RawRoundPrec
  | RawCeilingPrec
  | RawFloorPrec
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
  | RawYield
  | RawYieldToChain
  | RawResume
  | RawBind
  | RawRequireCapability
  | RawComposeCapability
  | RawInstallCapability
  | RawEmitEvent
  | RawCreateCapabilityGuard
  | RawCreateCapabilityPactGuard
  | RawCreateModuleGuard
  | RawCreateDefPactGuard
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
  | RawContinue
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
  | RawCreatePrincipal
  | RawIsPrincipal
  | RawTypeOfPrincipal
  | RawValidatePrincipal
  -- Namespaces
  | RawNamespace
  | RawDefineNamespace
  | RawDescribeNamespace
  | RawChainData
  | RawIsCharset
  | RawPactId
  -- ZK
  | RawZkPairingCheck
  | RawZKScalarMult
  | RawZkPointAdd
  -- Poseidon Hackachain
  | RawPoseidonHashHackachain
  -- Misc
  | RawTypeOf
  | RawDec
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
  -- Boolean ops
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
  RawRoundPrec -> "round-prec"
  RawCeilingPrec -> "ceiling-prec"
  RawFloorPrec -> "floor-prec"
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
  RawCreateCapabilityPactGuard -> "create-capability-pact-guard"
  RawCreateModuleGuard -> "create-module-guard"
  RawCreateDefPactGuard -> "create-pact-guard"
  RawAt -> "at"
  RawMakeList -> "make-list"
  RawB64Encode -> "base64-encode"
  RawB64Decode -> "base64-decode"
  RawStrToList -> "str-to-list"
  RawYield -> "yield"
  RawYieldToChain -> "yield-to-chain"
  RawResume -> "resume"
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
  RawContinue -> "continue"
  RawParseTime -> "parse-time"
  RawFormatTime -> "format-time"
  RawTime -> "time"
  RawAddTime -> "add-time"
  RawDiffTime -> "diff-time"
  RawHours -> "hours"
  RawMinutes -> "minutes"
  RawDays -> "days"
  RawCompose -> "compose"
  RawCreatePrincipal -> "create-principal"
  RawIsPrincipal -> "is-principal"
  RawTypeOfPrincipal -> "typeof-principal"
  RawValidatePrincipal -> "validate-principal"
  RawNamespace -> "namespace"
  RawDefineNamespace -> "define-namespace"
  RawDescribeNamespace -> "describe-namespace"
  RawZkPairingCheck -> "pairing-check"
  RawZKScalarMult -> "scalar-mult"
  RawZkPointAdd -> "point-add"
  RawPoseidonHashHackachain -> "poseidon-hash-hack-a-chain"
  RawChainData -> "chain-data"
  RawIsCharset -> "is-charset"
  RawPactId -> "pact-id"
  RawTypeOf -> "typeof"
  RawDec -> "dec"

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
    RawRoundPrec -> 2
    RawCeilingPrec -> 2
    RawFloorPrec -> 2
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
    RawCreateCapabilityPactGuard -> 1
    RawCreateModuleGuard -> 1
    RawCreateDefPactGuard -> 1
    RawAt -> 2
    RawMakeList -> 2
    RawB64Encode -> 1
    RawB64Decode -> 1
    RawStrToList -> 1
    RawYield -> 1
    RawYieldToChain -> 2
    RawResume -> 1
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
    RawContinue -> 1
    RawParseTime -> 2
    RawFormatTime -> 2
    RawTime -> 1
    RawAddTime -> 2
    RawDiffTime -> 2
    RawHours -> 1
    RawMinutes -> 1
    RawDays -> 1
    RawCompose -> 3
    RawCreatePrincipal -> 1
    RawIsPrincipal -> 1
    RawTypeOfPrincipal -> 1
    RawValidatePrincipal -> 2
    RawNamespace -> 1
    RawDefineNamespace -> 3
    RawDescribeNamespace -> 1
    RawZkPairingCheck -> 2
    RawZKScalarMult -> 3
    RawZkPointAdd -> 3
    RawPoseidonHashHackachain -> 1
    RawChainData -> 0
    RawIsCharset -> 2
    RawPactId -> 0
    RawTypeOf -> 1
    RawDec -> 1



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
  | REnvNamespacePolicy
  -- | REnvGas
  -- | REnvGasLimit
  -- | REnvGasLog
  -- | REnvGasModel
  -- | REnvGasPrice
  -- | REnvGasRate
  -- | REnvNamespacePolicy
  -- Defpact
  | RContinuePact
  | RContinuePactRollback
  | RContinuePactRollbackYield
  | RContinuePactRollbackYieldObj
  | RPactState
  | RResetPactState
  deriving (Show, Enum, Bounded, Eq)


instance IsBuiltin ReplBuiltins where
  builtinName = NativeName . replBuiltinsToText
  builtinArity = \case
    RExpect -> 3
    RExpectFailure -> 2
    RExpectFailureMatch -> 3
    RExpectThat -> 3
    RPrint -> 1
    RPactState -> 0
    RResetPactState -> 1
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
    RSigKeyset -> 0
    RTestCapability -> 1
    RContinuePact -> 1
    RContinuePactRollback -> 2
    RContinuePactRollbackYield -> 3
    RContinuePactRollbackYieldObj -> 4
    REnvExecConfig -> 1
    REnvNamespacePolicy -> 2
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
  RPactState -> "pact-state"
  RResetPactState -> "reset-pact-state"
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
  RContinuePact -> "continue-pact"
  RContinuePactRollback -> "continue-pact-with-rollback"
  RContinuePactRollbackYield -> "continue-pact-rollback-yield"
  RContinuePactRollbackYieldObj -> "continue-pact-rollback-yield-object"
  REnvExecConfig -> "env-exec-config"
  REnvNamespacePolicy -> "env-namespace-policy"

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
