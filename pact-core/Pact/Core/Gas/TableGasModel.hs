module Pact.Core.Gas.TableGasModel
 ( tableGasModel
 , replTableGasModel )
 where

import Pact.Core.Builtin
import Pact.Core.Gas

tableGasModel :: MilliGasLimit -> GasModel CoreBuiltin
tableGasModel gl =
  GasModel
  { _gmRunModel = runTableModel
  , _gmNatives = nativeGasTable
  , _gmName = "table"
  , _gmGasLimit = gl
  , _gmDesc = "table-based cost model"}

replTableGasModel :: MilliGasLimit -> GasModel ReplCoreBuiltin
replTableGasModel gl =
  (tableGasModel gl) { _gmNatives = replNativeGasTable }


runTableModel :: GasArgs -> MilliGas
runTableModel = \case
  GAConstant c -> c
  GALinear (MilliGas x) (LinearGasArg (mnum, mdiv) intercept) ->
    MilliGas $ ((x * mnum) `div` mdiv) + intercept

nativeGasTable :: CoreBuiltin -> MilliGas
nativeGasTable = MilliGas . \case
  CoreAdd -> 1
  CoreSub -> 1
  CoreMultiply -> 1
  CoreDivide -> 1
  CoreNegate -> 1
  CoreAbs -> 1
  CorePow -> 1
  CoreNot -> 1
  CoreEq -> 1
  CoreNeq -> 1
  CoreGT -> 1
  CoreGEQ -> 1
  CoreLT -> 1
  CoreLEQ -> 1
  CoreBitwiseAnd -> 1
  CoreBitwiseOr -> 1
  CoreBitwiseXor -> 1
  CoreBitwiseFlip -> 1
  CoreBitShift -> 1
  CoreRound -> 1
  CoreCeiling -> 1
  CoreFloor -> 1
  CoreRoundPrec -> 1
  CoreCeilingPrec -> 1
  CoreFloorPrec -> 1
  CoreExp -> 1
  CoreLn -> 1
  CoreSqrt -> 1
  CoreLogBase -> 1
  CoreLength -> 1
  CoreTake -> 1
  CoreDrop -> 1
  CoreConcat -> 1
  CoreReverse -> 1
  CoreContains -> 1
  CoreSort -> 1
  CoreSortObject -> 1
  CoreRemove -> 1
  CoreMod -> 1
  CoreMap -> 1
  CoreFilter -> 1
  CoreZip -> 1
  CoreIntToStr -> 1
  CoreStrToInt -> 1
  CoreStrToIntBase -> 1
  CoreFold -> 1
  CoreDistinct -> 1
  CoreFormat -> 1
  CoreEnumerate -> 1
  CoreEnumerateStepN -> 1
  CoreShow -> 1
  CoreReadMsg -> 1
  CoreReadMsgDefault -> 1
  CoreReadInteger -> 1
  CoreReadDecimal -> 1
  CoreReadString -> 1
  CoreReadKeyset -> 1
  CoreEnforceGuard -> 1
  CoreEnforceKeyset -> 1
  CoreKeysetRefGuard -> 1
  CoreAt -> 1
  CoreMakeList -> 1
  CoreB64Encode -> 1
  CoreB64Decode -> 1
  CoreStrToList -> 1
  CoreYield -> 1
  CoreYieldToChain -> 1
  CoreResume -> 1
  CoreBind -> 1
  CoreRequireCapability -> 1
  CoreComposeCapability -> 1
  CoreInstallCapability -> 1
  CoreEmitEvent -> 1
  CoreCreateCapabilityGuard -> 1
  CoreCreateCapabilityPactGuard -> 1
  CoreCreateModuleGuard -> 1
  CoreCreateDefPactGuard -> 1
  CoreCreateTable -> 1
  CoreDescribeKeyset -> 1
  CoreDescribeModule -> 1
  CoreDescribeTable -> 1
  CoreDefineKeySet -> 1
  CoreDefineKeysetData -> 1
  CoreFoldDb -> 1
  CoreInsert -> 1
  CoreKeyLog -> 1
  CoreKeys -> 1
  CoreRead -> 1
  CoreSelect -> 1
  CoreSelectWithFields -> 1
  CoreUpdate -> 1
  CoreWithDefaultRead -> 1
  CoreWithRead -> 1
  CoreWrite -> 1
  CoreTxIds -> 1
  CoreTxLog -> 1
  CoreTxHash -> 1
  CoreAndQ -> 1
  CoreOrQ -> 1
  CoreWhere -> 1
  CoreNotQ -> 1
  CoreHash -> 1
  CoreContinue -> 1
  CoreParseTime -> 1
  CoreFormatTime -> 1
  CoreTime -> 1
  CoreAddTime -> 1
  CoreDiffTime -> 1
  CoreHours -> 1
  CoreMinutes -> 1
  CoreDays -> 1
  CoreCompose -> 1
  CoreCreatePrincipal -> 1
  CoreIsPrincipal -> 1
  CoreTypeOfPrincipal -> 1
  CoreValidatePrincipal -> 1
  CoreNamespace -> 1
  CoreDefineNamespace -> 1
  CoreDescribeNamespace -> 1
  CoreChainData -> 1
  CoreIsCharset -> 1
  CorePactId -> 1
  CoreZkPairingCheck -> 1
  CoreZKScalarMult -> 1
  CoreZkPointAdd -> 1
  CorePoseidonHashHackachain -> 1
  CoreTypeOf -> 1
  CoreDec -> 1

replNativeGasTable :: ReplBuiltin CoreBuiltin -> MilliGas
replNativeGasTable = \case
  RBuiltinWrap bwrap -> nativeGasTable bwrap
  _ -> mempty

