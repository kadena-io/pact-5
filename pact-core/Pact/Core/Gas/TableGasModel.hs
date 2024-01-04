{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DataKinds #-}

module Pact.Core.Gas.TableGasModel
 ( tableGasModel
 , replTableGasModel )
 where

import qualified GHC.Integer.Logarithms as IntLog
import Data.Word(Word64)
import GHC.Int(Int(..))
import Pact.Core.Builtin
    ( CoreBuiltin(..), ReplCoreBuiltin, ReplBuiltin(RBuiltinWrap) )
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

------------------------------------------------
-- ZK Costs
------------------------------------------------
-- note: the following is from prod, we simply adapt it to use milligas
-- pointAddGas :: ZKGroup -> Gas
-- pointAddGas = \case
--   ZKG1 -> 5
--   ZKG2 -> 30

-- scalarMulGas :: ZKGroup -> Gas
-- scalarMulGas = \case
--   ZKG1 -> 360
--   ZKG2 -> 1450

-- pairingGas :: Int -> Gas
-- pairingGas npairs
--   | npairs > 0 = fromIntegral (npairs * slope + intercept)
--   | otherwise = 100
--   where
--   slope = 3760
--   intercept = 11600
-- Todo: costs are from prod. We need to re-bench
pointAddGas :: ZKGroup -> MilliGas
pointAddGas = \case
  ZKG1 -> MilliGas 5_000
  ZKG2 -> MilliGas 30_000

scalarMulGas :: ZKGroup -> MilliGas
scalarMulGas = \case
  ZKG1 -> MilliGas 360_000
  ZKG2 -> MilliGas 1_450_000

pairingGas :: Int -> MilliGas
pairingGas npairs
  | npairs > 0 = MilliGas $ fromIntegral (npairs * slope + intercept)
  -- Note: this is just a penalty for calling the function without the right number of pairs
  | otherwise = MilliGas 100_000
  where
  slope = 3_760
  intercept = 11_600

-- [Complexity of mathematical operations]
-- see: https://en.wikipedia.org/wiki/Computational_complexity_of_mathematical_operations
--      https://gmplib.org/manual/Algorithms
-- The ones that matter to us:
--  addition: O(n) where n is a n-bit number
--  subtraction: O(n) (same as addition)
--  multiplication: O(n^(1.465)) ~ O(n*sqrt(n)) (3-way toom is a good approximation in general).
--                  libgmp uses Toom-cook in general.
--  division: O(M(n)log n) in general, meaning O(n*sqrt(n)*log n)

-- ghci> I# (IntLog.integerLog2# (10 ^ 80))
-- 265
-- ghci> I# (IntLog.integerLog2# (10 ^ 200))
-- 664
intCostUpperBound :: Int
intCostUpperBound = 664
-- TODO Note: 10^80 seems a bit restrictive given benchmarks show this is just hilariously fast.
-- up to 10^300, so 10^200 seems like a decent upper bound
-- intAdditionUpperBound =  265


-- | Costing function for binary integer ops
-- intCost :: IntOpThreshold -> Integer -> Gas
intAdditionCost :: Integer -> Integer ->  MilliGas
intAdditionCost !lop !rop
  | lop > rop = go lop
  | otherwise = go rop
  where
  go !a =
    let !nbits = (I# (IntLog.integerLog2# (abs a)) + 1)
    -- Note: benchmarks tell us the integer cost model for addition is roughly
    -- addGasCost(x) = 20 + 0.008*(Log2(x)) in ns, so we can set it to ~25 or so milligas while
    -- we calibrate further, but this seems like a good upper bound on the gas,
    -- since after 10^(intAdditionUpperBound), we essentially gas bomb it
    in if nbits <= intCostUpperBound then MilliGas 30
       else MilliGas $ fromIntegral (nbits * nbits `quot` 6400)

intMultCost :: Integer -> Integer ->  MilliGas
intMultCost !lop !rop
  | lop > rop = go lop
  | otherwise = go rop
  where
  go !a =
    -- Note: some simple benchmarks up to 10^300 has the function of integer multiplication
    -- have the cost intMul(n) = 0.1523*n + 26.2 ~ 1.5n+26 ~ (3*n)/20+26
    -- after the bound,
    let !nbits = (I# (IntLog.integerLog2# (abs a)) + 1)
    in if nbits <= intCostUpperBound then MilliGas $ fromIntegral $ ((3*nbits) `quot` 20 + 26)
       else MilliGas $ fromIntegral (nbits * nbits `quot` 6400)

intDivCost :: Integer -> Integer ->  MilliGas
intDivCost !lop !rop
  | lop > rop = go lop
  | otherwise = go rop
  where
  go !a =
    -- Note: The division algorithm basically has complexity O(M(n)log(n))
    -- With a bit of squinting (okay maybe a lot), we can simply charge as much as multiplication
    -- below our threshold, benchmarks find integer and rational division to be quite fast
    let !nbits = (I# (IntLog.integerLog2# (abs a)) + 1)
    in if nbits <= intCostUpperBound then MilliGas $ fromIntegral $ ((3*nbits) `quot` 20 + 26)
       else MilliGas $ fromIntegral (nbits * nbits `quot` 6400)

runTableModel :: GasArgs -> MilliGas
runTableModel = \case
  GAConstant c -> c
  GIntegerOpCost !primOp lop rop -> case primOp of
    PrimOpAdd -> intAdditionCost lop rop
    PrimOpSub -> intAdditionCost lop rop
    PrimOpMul -> intMultCost lop rop
    PrimOpDiv -> intDivCost lop rop
  GALinear (MilliGas x) (LinearGasArg mnum mdiv intercept) ->
    MilliGas $ ((x * mnum) `div` mdiv) + intercept

-- Prod gas table, for reference.
-- defaultGasTable :: Map Text Gas
-- defaultGasTable =
--   Map.fromList
--   [("!=", 2)
--   ,("&", 1)
--   ,("*", 3)
--   ,("+", 1)
--   ,("-", 1)
--   ,("/", 3)
--   ,("<", 2)
--   ,("<=", 2)
--   ,("=", 2)
--   ,(">", 2)
--   ,(">=", 2)
--   ,("^", 4)
--   ,("abs", 1)
--   ,("add-time", 3)
--   ,("and", 1)
--   ,("and?", 1)
--   ,("at", 2)
--   ,("base64-decode", 1)
--   ,("base64-encode", 1)
--   ,("bind", 4)
--   ,("ceiling", 1)
--   ,("chain-data", 1)
--   ,("compose", 1)
--   ,("compose-capability", 2)
--   ,("concat", 1)
--   ,("constantly", 1)
--   ,("contains", 2)
--   ,("create-module-guard", 1)
--   ,("create-pact-guard", 1)
--   ,("create-principal", 1)
--   ,("create-user-guard", 1)
--   ,("create-capability-guard", 1)
--   ,("create-capability-pact-guard", 1)
--   ,("days", 4)
--   ,("dec", 1)
--   ,("decrypt-cc20p1305", 33)
--   ,("diff-time", 8)
--   ,("drop", 3)
--   ,("emit-event",1)
--   ,("enforce", 1)
--   ,("enforce-guard", 8)
--   ,("enforce-keyset", 8)
--   ,("enforce-one", 6)
--   ,("enforce-pact-version", 1)
--   ,("enforce-verifier", 10)
--   ,("enumerate", 1)
--   ,("exp", 5)
--   ,("filter", 3)
--   ,("floor", 1)
--   ,("fold", 3)
--   ,("format", 4)
--   ,("format-time", 4)
--   ,("hash", 5)
--   ,("hours", 4)
--   ,("identity", 2)
--   ,("if", 1)
--   ,("install-capability", 3)
--   ,("int-to-str", 1)
--   ,("is-charset", 1)
--   ,("is-principal",1)
--   ,("keys-2", 1)
--   ,("keys-all", 1)
--   ,("keys-any", 1)
--   ,("keyset-ref-guard", 7)
--   ,("length", 1)
--   ,("ln", 6)
--   ,("log", 3)
--   ,("make-list",1)
--   ,("map", 4)
--   ,("zip", 4)
--   ,("minutes", 4)
--   ,("mod", 1)
--   ,("namespace", 12)
--   ,("not", 1)
--   ,("not?", 1)
--   ,("or", 1)
--   ,("or?", 1)
--   ,("pact-id", 1)
--   ,("pact-version", 1)
--   ,("parse-time", 2)
--   ,("read", 10)
--   ,("read-decimal", 1)
--   ,("read-integer", 1)
--   ,("read-keyset", 1)
--   ,("read-msg", 10)
--   ,("read-string", 1)
--   ,("remove", 2)
--   ,("require-capability", 1)
--   ,("resume", 2)
--   ,("reverse", 2)
--   ,("round", 1)
--   ,("shift", 1)
--   ,("sort", 2)
--   ,("sqrt", 6)
--   ,("str-to-int", 1)
--   ,("str-to-list", 1)
--   ,("take", 3)
--   ,("time", 2)
--   ,("try", 1)
--   ,("tx-hash", 1)
--   ,("typeof", 2)
--   ,("typeof-principal",1)
--   ,("distinct", 2)
--   ,("validate-keypair", 29)
--   ,("validate-principal", 1)
--   ,("verify-spv", 100) -- deprecated
--   ,("where", 2)
--   ,("with-capability", 2)
--   ,("with-default-read", 14)
--   ,("with-read", 13)
--   ,("xor", 1)
--   ,("yield", 2)
--   ,("|", 1)
--   ,("~", 1)
--   -- Nested defpacts
--   ,("continue",1)
--   -- IO
--   -- DDL
--   ,("create-table", 250)
--   -- Registries
--   ,("define-keyset", 25)
--   ,("define-namespace", 25)
--   -- Single row
--   ,("insert", 100)
--   ,("update", 100)
--   ,("write", 100)
--   -- Multi row read, tx penalty
--   ,("keys", 200)
--   ,("select", 200)
--   ,("fold-db", 200)

--   -- Metadata, tx penalty
--   ,("describe-keyset", 100)
--   ,("describe-module", 100)
--   ,("describe-table", 100)
--   ,("list-modules", 100)
--   ,("describe-namespace",100)

--   -- History, massive tx penalty
--   ,("keylog", 100000)
--   ,("txids", 100000)
--   ,("txlog", 100000)

--   -- Zk entries
--   -- TODO: adjust gas, this is purely for testing purposes
--   ,("scalar-mult", 1)
--   ,("point-add", 1)
--   ,("pairing-check", 1)

--   ,("poseidon-hash-hack-a-chain", 124)
--   ]
basicWorkGas :: Word64
basicWorkGas = 25

nativeGasTable :: CoreBuiltin -> MilliGas
nativeGasTable = MilliGas . \case
  -- Basic arithmetic
  -- note: add, sub, mul, div are special and are covered by
  -- special functions
  CoreAdd -> 1
  CoreSub -> 1
  CoreMultiply -> 1
  CoreDivide -> 1
  --
  CoreNegate -> 50
  --
  CoreAbs -> 50
  -- Pow is also a special case of recursive multiplication, gas table is not enough.
  CorePow -> 1
  --
  CoreNot -> basicWorkGas
  -- Todo: ORD functions are special.
  -- They require more in-depth analysis than currently is being done
  CoreEq -> 1
  CoreNeq -> 1
  CoreGT -> 1
  CoreGEQ -> 1
  CoreLT -> 1
  CoreLEQ -> 1
  -- All of the bitwise functions are quite fast, regardless of the size of the integer
  -- constant time gas is fine here
  CoreBitwiseAnd -> basicWorkGas
  CoreBitwiseOr -> basicWorkGas
  CoreBitwiseXor -> basicWorkGas
  CoreBitwiseFlip -> basicWorkGas
  -- Shift requires special handling
  -- given it can actually grow the number
  CoreBitShift -> 1
  -- Todo: rounding likely needs benchmarks, but
  CoreRound -> 1
  CoreCeiling -> 1
  CoreFloor -> 1
  CoreRoundPrec -> 1
  CoreCeilingPrec -> 1
  CoreFloorPrec -> 1
  --
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

