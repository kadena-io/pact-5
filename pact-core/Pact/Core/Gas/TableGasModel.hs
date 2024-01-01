module Pact.Core.Gas.TableGasModel
 ( tableGasModel
 , replTableGasModel )
 where

import Pact.Core.Builtin
import Pact.Core.Gas

tableGasModel :: MilliGasLimit -> GasModel RawBuiltin
tableGasModel gl =
  GasModel
  { _gmRunModel = runTableModel
  , _gmNatives = nativeGasTable
  , _gmName = "table"
  , _gmGasLimit = gl
  , _gmDesc = "table-based cost model"}

replTableGasModel :: MilliGasLimit -> GasModel ReplRawBuiltin
replTableGasModel gl =
  (tableGasModel gl) { _gmNatives = replNativeGasTable }


runTableModel :: GasArgs -> MilliGas
runTableModel = \case
  GAConstant c -> c
  GALinear (MilliGas x) (LinearGasArg (mnum, mdiv) intercept) ->
    MilliGas $ ((x * mnum) `div` mdiv) + intercept

nativeGasTable :: RawBuiltin -> MilliGas
nativeGasTable = MilliGas . \case
  RawAdd -> 1
  RawSub -> 1
  RawMultiply -> 1
  RawDivide -> 1
  RawNegate -> 1
  RawAbs -> 1
  RawPow -> 1
  RawNot -> 1
  RawEq -> 1
  RawNeq -> 1
  RawGT -> 1
  RawGEQ -> 1
  RawLT -> 1
  RawLEQ -> 1
  RawBitwiseAnd -> 1
  RawBitwiseOr -> 1
  RawBitwiseXor -> 1
  RawBitwiseFlip -> 1
  RawBitShift -> 1
  RawRound -> 1
  RawCeiling -> 1
  RawFloor -> 1
  RawRoundPrec -> 1
  RawCeilingPrec -> 1
  RawFloorPrec -> 1
  RawExp -> 1
  RawLn -> 1
  RawSqrt -> 1
  RawLogBase -> 1
  RawLength -> 1
  RawTake -> 1
  RawDrop -> 1
  RawConcat -> 1
  RawReverse -> 1
  RawContains -> 1
  RawSort -> 1
  RawSortObject -> 1
  RawRemove -> 1
  RawMod -> 1
  RawMap -> 1
  RawFilter -> 1
  RawZip -> 1
  RawIntToStr -> 1
  RawStrToInt -> 1
  RawStrToIntBase -> 1
  RawFold -> 1
  RawDistinct -> 1
  RawFormat -> 1
  RawEnumerate -> 1
  RawEnumerateStepN -> 1
  RawShow -> 1
  RawReadMsg -> 1
  RawReadMsgDefault -> 1
  RawReadInteger -> 1
  RawReadDecimal -> 1
  RawReadString -> 1
  RawReadKeyset -> 1
  RawEnforceGuard -> 1
  RawEnforceKeyset -> 1
  RawKeysetRefGuard -> 1
  RawAt -> 1
  RawMakeList -> 1
  RawB64Encode -> 1
  RawB64Decode -> 1
  RawStrToList -> 1
  RawYield -> 1
  RawYieldToChain -> 1
  RawResume -> 1
  RawBind -> 1
  RawRequireCapability -> 1
  RawComposeCapability -> 1
  RawInstallCapability -> 1
  RawEmitEvent -> 1
  RawCreateCapabilityGuard -> 1
  RawCreateCapabilityPactGuard -> 1
  RawCreateModuleGuard -> 1
  RawCreateDefPactGuard -> 1
  RawCreateTable -> 1
  RawDescribeKeyset -> 1
  RawDescribeModule -> 1
  RawDescribeTable -> 1
  RawDefineKeySet -> 1
  RawDefineKeysetData -> 1
  RawFoldDb -> 1
  RawInsert -> 1
  RawKeyLog -> 1
  RawKeys -> 1
  RawRead -> 1
  RawSelect -> 1
  RawSelectWithFields -> 1
  RawUpdate -> 1
  RawWithDefaultRead -> 1
  RawWithRead -> 1
  RawWrite -> 1
  RawTxIds -> 1
  RawTxLog -> 1
  RawTxHash -> 1
  RawAndQ -> 1
  RawOrQ -> 1
  RawWhere -> 1
  RawNotQ -> 1
  RawHash -> 1
  RawContinue -> 1
  RawParseTime -> 1
  RawFormatTime -> 1
  RawTime -> 1
  RawAddTime -> 1
  RawDiffTime -> 1
  RawHours -> 1
  RawMinutes -> 1
  RawDays -> 1
  RawCompose -> 1
  RawCreatePrincipal -> 1
  RawIsPrincipal -> 1
  RawTypeOfPrincipal -> 1
  RawValidatePrincipal -> 1
  RawNamespace -> 1
  RawDefineNamespace -> 1
  RawDescribeNamespace -> 1
  RawChainData -> 1
  RawIsCharset -> 1
  RawPactId -> 1
  RawZkPairingCheck -> 1
  RawZKScalarMult -> 1
  RawZkPointAdd -> 1
  RawPoseidonHashHackachain -> 1
  RawTypeOf -> 1
  RawDec -> 1

replNativeGasTable :: ReplBuiltin RawBuiltin -> MilliGas
replNativeGasTable = \case
  RBuiltinWrap bwrap -> nativeGasTable bwrap
  _ -> mempty

