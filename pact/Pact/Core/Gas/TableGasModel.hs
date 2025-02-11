{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Pact.Core.Gas.TableGasModel
 ( tableGasModel
 , replTableGasModel
 , runTableModel
 , pointAddGas
 , scalarMulGas
 , pairingGas
 , mkTableGasEnv
 )
 where

import qualified Data.Text as T
import GHC.Num.Integer
import qualified GHC.Integer.Logarithms as IntLog
import Pact.Core.Builtin
import Pact.Core.Gas.Types
import Data.Decimal
import qualified Data.Vector as V
import GHC.Base


tableGasCostConfig :: GasCostConfig
tableGasCostConfig = GasCostConfig
  -- The basic cost of entering the native table +
  -- performing some work
  { _gcNativeBasicWork = 100
  , _gcFunctionArgumentCost = 25
  , _gcMachineTickCost = 25
  , _gcUnconsWork = 100
  , _gcReadPenalty = 2_500
  , _gcWritePenalty = 25_000
  , _gcMetadataTxPenalty = 100_000
  , _gcSelectPenalty = 40_000_000
  , _gcConcatFactor = 100
  -- Note: on a _really_ slow hard disk, it writes about
  -- 80 bytes per microsecond, or 1 byte = 5 milligas after conversions
  -- (80 bytes / 1 microsecond) * (2.5 micros / 1000 milligas) = 0.2 milligas per byte or 1 byte = 5 milligas
  -- We add some extra overhead here, because we need to serialize and perform other checks.
  , _gcPerByteWriteCost = 200
  -- Reads also tend to be about twice as fast as writes, so we charge a bit less
  , _gcPerByteReadCost = 100
  , _gcSortBytePenaltyReduction = 1000
  , _gcPoseidonQuadraticGasFactor = 50_000
  , _gcPoseidonLinearGasFactor = 38_000
  , _gcModuleLoadSlope = 200
  , _gcModuleLoadIntercept = 10
  , _gcDesugarBytePenalty = 400
  , _gcMHashBytePenalty = 100
  , _gcSizeOfBytePenalty = 5
  , _gc_keccak256GasPerOneHundredBytes = 146
  , _gc_keccak256GasPerChunk = 2_120
  }


tableGasModel :: MilliGasLimit -> GasModel CoreBuiltin
tableGasModel gl =
  GasModel
  { _gmName = "table"
  , _gmGasLimit = Just gl
  , _gmDesc = "table-based cost model"
  , _gmNativeTable = coreBuiltinGasCost tableGasCostConfig
  , _gmGasCostConfig = tableGasCostConfig
  }

mkTableGasEnv :: MilliGasLimit -> EnableGasLogs -> IO (GasEnv CoreBuiltin i)
mkTableGasEnv mgl enabled = mkGasEnv enabled (tableGasModel mgl)

replTableGasModel :: Maybe MilliGasLimit -> GasModel ReplCoreBuiltin
replTableGasModel gl =
  GasModel
  { _gmName = "table"
  , _gmGasLimit = gl
  , _gmDesc = "table-based cost model"
  , _gmNativeTable = replNativeGasTable
  , _gmGasCostConfig = tableGasCostConfig
  }

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

integerBits :: Integer -> Int
integerBits = \case
  IS _ -> 64 -- note: Small ints are machine word sized
  i -> I# (IntLog.integerLog2# (abs i) +# 1#)
{-# INLINE integerBits #-}

numberOfBits :: Integer -> Int
numberOfBits i = I# (IntLog.integerLog2# (abs i) +# 1#)
{-# INLINE numberOfBits #-}


-- | Costing function for binary integer ops
-- intCost :: IntOpThreshold -> Integer -> Gas
intAdditionCost :: Integer -> Integer ->  MilliGas
intAdditionCost !lop !rop
  | lop > rop = go lop
  | otherwise = go rop
  where
  go !a =
    let !nbits = numberOfBits a
    -- Note: benchmarks tell us the integer cost model for addition is roughly
    -- addGasCost(x) = 20 + 0.008*(Log2(x)) in ns, so we can set it to ~25 or so milligas while
    -- we calibrate further, but this seems like a good upper bound on the gas,
    -- since after 10^(intAdditionUpperBound), we essentially gas bomb it
    in if nbits <= intCostUpperBound then MilliGas 30
       else MilliGas $ fromIntegral (nbits * nbits `quot` 6400)
{-# INLINE intAdditionCost #-}


intMultCost :: Integer -> Integer ->  MilliGas
intMultCost !lop !rop
  | lop > rop = go lop
  | otherwise = go rop
  where
  go !a =
    -- Note: some simple benchmarks up to 10^300 has the function of integer multiplication
    -- have the cost intMul(n) = 0.1523*n + 26.2 ~ 1.5n+26 ~ (3*n)/20+26
    -- after the bound,
    let !nbits = numberOfBits a
    in if nbits <= intCostUpperBound then MilliGas $ fromIntegral $ (3*nbits) `quot` 20 + 26
       else MilliGas $ fromIntegral (nbits * nbits `quot` 6400)
{-# INLINE intMultCost #-}

intDivCost :: Integer -> Integer -> MilliGas
intDivCost !lop !rop
  | lop > rop = go lop
  | otherwise = go rop
  where
  go !a =
    -- Note: The division algorithm basically has complexity O(M(n)log(n))
    -- With a bit of squinting (okay maybe a lot), we can simply charge as much as multiplication
    -- below our threshold, benchmarks find integer and rational division to be quite fast
    let !nbits = numberOfBits a
    in if nbits <= intCostUpperBound then MilliGas $ fromIntegral $ (3*nbits) `quot` 20 + 26
       else MilliGas $ fromIntegral (nbits * nbits `quot` 6400)
{-# INLINE intDivCost #-}

transExpCost :: Integer -> MilliGas
transExpCost !power = MilliGas total
  where
  nDigitsBase, nDigitsPower, totalMults, k_const, operandSizeAverage :: SatWord
  -- totalMults: Total number of multiplications (worst-case scenario)
  -- For exponentiation by squaring, total multiplications T_m = 2L - 2
  !totalMults = 2 * nDigitsPower - 2
  -- n0: Number of bits in the base k
  !nDigitsBase = fromIntegral (numberOfBits 3) -- (numberOfBits 2718281828459045090795598298427648842334747314453125)
  !nDigitsPower = fromIntegral (numberOfBits power)
  !k_const = 1 -- Our constant for karasuba mult per mul in terms of milligas
  -- Constant for karasuba algorithm
  alpha :: Double
  alpha = 2.5
  -- operandSizeAvg: Average operand size in bits (geometric mean)
  -- operandSizeAvg = n0 * 2^((L - 1) / 2)
  --
  -- This calculation accounts for the exponential growth of operand sizes due to squaring.
  -- The exponent (L - 1) / 2 represents the average number of squarings,
  -- since operand size doubles with each squaring.
  !operandSizeAverage =
    nDigitsBase * (ceiling ((2 :: Double) ** (fromIntegral (nDigitsPower - 1) / 2)))
  -- Note:
  -- The exponential growth factor p^(alpha / 2) is already included in (operandSizeAvg ** alpha)
  -- due to the properties of exponents:
  --
  --   (operandSizeAvg) ** alpha
  -- = [n0 * 2^((L - 1) / 2)] ** alpha
  -- = n0^alpha * 2^((L - 1) * alpha / 2)
  --
  -- Since 2^((L - 1) * alpha / 2) = [2^(L - 1)]^(alpha / 2)
  -- and 2^(L - 1) ≈ p (when p is a power of 2),
  -- we have:
  --
  --   2^((L - 1) * alpha / 2) = p^(alpha / 2)
  --
  -- Therefore, (operandSizeAvg ** alpha) includes the p^(alpha / 2) term,
  -- and we do not need to multiply by it separately.
  !total =
    totalMults * k_const * ceiling (fromIntegral operandSizeAverage ** alpha)


-- | Int shifting needs a bit of an adjustment.
-- It's hilariously fast, but it can also create numbers of hilariously large sizes
--
-- Shifting is essentially k * 2^p, so we can simply charge 2*p and the multiplication by `k` separately
intShiftCost :: Integer -> Integer -> MilliGas
intShiftCost lop !rop
  | rop > 0 =
    intPowCost 2 rop <> MilliGas ((3* (max (fromIntegral (numberOfBits lop)) (fromIntegral rop))) `quot` 20 + 26)
  | otherwise = MilliGas 0

-- | Estimates the total computational cost of exponentiation by squaring
-- using the average operand size (geometric mean).
--
-- Parameters:
-- k     : Base integer
-- p     : Exponent integer
-- K     : Scaling constant (cost per bit operation), in our case 1 milligas
-- alpha : Exponent from multiplication algorithm (e.g., 1.585 for Karatsuba)
--
-- The formula for total cost is:
--
--   C_total = T_m * K * (n_0 ^ alpha) * p^alpha
--   After simplification
--   C_total = T_m * K * (n_avg)^alpha
--
-- Where:
--   - T_m    : Total number of multiplications (squarings and multiplications by k)
--              For the worst-case scenario (exponent with all bits '1'):
--              T_m = 2L - 2
--   - n_avg  : Average operand size in bits (geometric mean of operand sizes)
--              n_avg = n0 * 2^((L - 1) / 2)
--   - n0     : Number of bits in the base k
--   - L      : Number of bits in the exponent p
--   - alpha  : Exponent from the multiplication algorithm complexity
--              (alpha = log_2 3 ≈ 1.585 for Karatsuba algorithm)
--
-- Explanation:
-- The operand size doubles with each squaring operation.
-- The geometric mean is used to estimate the average operand size over all steps.
-- Exponential growth due to squaring is captured in n_avg.
-- Multiplying n_avg by K and raising to the power of alpha estimates the cost per multiplication.
-- Total cost is then the cost per multiplication times the total number of multiplications.
intPowCost :: Integer -> Integer -> MilliGas
intPowCost !base !power = MilliGas total
  where
  nDigitsBase, nDigitsPower, totalMults, k_const, operandSizeAverage :: SatWord
  -- totalMults: Total number of multiplications (worst-case scenario)
  -- For exponentiation by squaring, total multiplications T_m = 2L - 2
  !totalMults = 2 * nDigitsPower - 2
  -- n0: Number of bits in the base k
  !nDigitsBase = fromIntegral (numberOfBits base)
  !nDigitsPower = fromIntegral (numberOfBits power)
  !k_const = 1 -- Our constant for karasuba mult per mul in terms of milligas
  -- Constant for multiplication in general
  alpha :: Double
  alpha = 2
  -- operandSizeAvg: Average operand size in bits (geometric mean)
  -- operandSizeAvg = n0 * 2^((L - 1) / 2)
  --
  -- This calculation accounts for the exponential growth of operand sizes due to squaring.
  -- The exponent (L - 1) / 2 represents the average number of squarings,
  -- since operand size doubles with each squaring.
  !operandSizeAverage =
    nDigitsBase * (ceiling ((2 :: Double) ** (fromIntegral (nDigitsPower - 1) / 2)))
  -- Note:
  -- The exponential growth factor p^(alpha / 2) is already included in (operandSizeAvg ** alpha)
  -- due to the properties of exponents:
  --
  --   (operandSizeAvg) ** alpha
  -- = [n0 * 2^((L - 1) / 2)] ** alpha
  -- = n0^alpha * 2^((L - 1) * alpha / 2)
  --
  -- Since 2^((L - 1) * alpha / 2) = [2^(L - 1)]^(alpha / 2)
  -- and 2^(L - 1) ≈ p (when p is a power of 2),
  -- we have:
  --
  --   2^((L - 1) * alpha / 2) = p^(alpha / 2)
  --
  -- Therefore, (operandSizeAvg ** alpha) includes the p^(alpha / 2) term,
  -- and we do not need to multiply by it separately.
  !total =
    totalMults * k_const * ceiling (fromIntegral operandSizeAverage ** alpha)

runTableModel :: (b -> MilliGas) -> GasCostConfig -> GasArgs b -> MilliGas
runTableModel nativeTable GasCostConfig{..} = \case
  GNative b -> nativeTable b
  GAConstant !c -> c
  GIntegerOpCost !primOp lop rop -> case primOp of
    PrimOpAdd -> intAdditionCost lop rop
    PrimOpSub -> intAdditionCost lop rop
    PrimOpMul -> intMultCost lop rop
    PrimOpDiv -> intDivCost lop rop
    PrimOpShift -> intShiftCost lop rop
    PrimOpPow -> intPowCost lop rop
  -- Note: concat values are currently just constant
  -- Todo: get actual metrics on list cat / text cat
  GConcat c -> case c of
    TextConcat (GasTextLength totalLen) ->
      MilliGas (fromIntegral totalLen * _gcConcatFactor)
    TextListConcat (GasTextLength totalCharSize) (GasListLength nElems) -> MilliGas $
      fromIntegral totalCharSize * _gcConcatFactor + fromIntegral nElems * listLenCost
      where
      listLenCost :: SatWord
      listLenCost = 40
    ListConcat (GasListLength totalLen) ->
      MilliGas (fromIntegral totalLen * _gcConcatFactor)
    ObjConcat totalLen ->
      MilliGas (fromIntegral totalLen * _gcConcatFactor)
  GAApplyLam _ !i -> MilliGas $ fromIntegral i * _gcFunctionArgumentCost + 50
  GAZKArgs zka -> case zka of
    PointAdd g -> pointAddGas g
    ScalarMult g -> scalarMulGas g
    Pairing np -> pairingGas np
  -- Writes on average are about 50% slower than reads,
  -- So we add a bit of a higher penalty to this, since this
  -- costs us gas as well
  GWrite bytes ->
    MilliGas $ bytes * _gcPerByteWriteCost
  GRead bytes ->
    MilliGas $ bytes * _gcPerByteReadCost
    -- a string of 10⁶ chars (which is 2×10⁶ sizeof bytes) takes a little less than 2×10⁶ to write
  GMakeList len sz ->
    MilliGas $ fromIntegral len * fromIntegral sz
  GComparison cmpty -> case cmpty of
    TextComparison str ->
      MilliGas $ textCompareCost str + _gcNativeBasicWork
    IntComparison l r ->
      MilliGas $ fromIntegral (max (integerBits l) (integerBits r)) + _gcNativeBasicWork
    -- See [Decimal comparisons]
    DecimalComparison l r ->
      let !lmantissa = decimalMantissa l
          !rmantissa = decimalMantissa r
      in
      intDivCost lmantissa rmantissa <> MilliGas (fromIntegral (max (integerBits lmantissa) (integerBits rmantissa)) + _gcNativeBasicWork)
    ListComparison maxSz ->
      MilliGas $ fromIntegral maxSz * _gcNativeBasicWork
    ObjComparison i ->
      MilliGas $ fromIntegral i * _gcNativeBasicWork
    -- For sorting, what we do is essentially take the `sizeOf` number of bytes that we are comparing.
    -- Take that, have a cost of comparison proportional to the number of bytes,
    -- and charge for the _worst case_ O(n^2) number of comparisons.
    SortComparisons size len ->
      let !lenW = fromIntegral len
      -- Comparisons is 1 mg per byte, so we simply take the length^2 * size
      in MilliGas $ (fromIntegral size * lenW * lenW) `div` _gcSortBytePenaltyReduction

  GSearch sty -> case sty of
    SubstringSearch needle hay -> MilliGas $ fromIntegral (T.length needle + T.length hay) + _gcNativeBasicWork
    FieldSearch cnt -> MilliGas $ fromIntegral cnt + _gcNativeBasicWork
  GModuleOp op -> case op of
    MOpLoadModule byteSize  ->
      -- After some benchmarking, we can essentially say that the byte size of linear in
      -- the size of the module.
      -- We can cost module loads at approximately
      -- y=0.005x+10
      let !szCost = (fromIntegral byteSize `div` _gcModuleLoadSlope) + _gcModuleLoadIntercept
      in MilliGas szCost
    MOpMergeDeps i i' ->
      -- We can cost this quadratically, at 10mg per element merged
      MilliGas $ fromIntegral (i * i') * 10
    MOpDesugarModule sz ->
      -- This is a pretty expensive traversal, so we will charge a bit more of a hefty price for it
      MilliGas (sz * _gcDesugarBytePenalty)
  GStrOp op -> case op of
    StrOpLength len ->
      let charsPerMg = 100
      in MilliGas $ fromIntegral $ len `quot` charsPerMg + 1
    StrOpConvToInt len ->
      let mgPerChar = 20
      in MilliGas $ fromIntegral $ len * mgPerChar + 1
    StrOpParse len ->
      let mgPerChar = 5
      in MilliGas $ fromIntegral $ len * mgPerChar + 1
    StrOpExplode len ->
      let mgPerChar = 100
      in MilliGas $ fromIntegral $ len * mgPerChar + 1
    StrOpParseTime fmtLen strLen ->
      -- a test string of 1000 %Y's (hence 2000 chars) is processed in about 10³ [g] = 10⁶ [mg]
      -- and there's also a cost of unpacking the strings
      let fmtCharSqPerMg = 4
          unpackMgPerChar = 10
      in MilliGas $ fromIntegral $ (fmtLen * fmtLen) `div` fmtCharSqPerMg + unpackMgPerChar * strLen + 1
    StrOpFormatTime fmtLen ->
      let mgPerChar = 20
      in MilliGas $ fromIntegral $ fmtLen * mgPerChar + 1
  GObjOp op -> case op of
    ObjOpLookup key objSize ->
      let objSzLog = fromIntegral $ numberOfBits (toInteger objSize)
      in MilliGas $ objSzLog * textCompareCost key
    ObjOpRemove key objSize ->
      -- an object with 10⁷ keys takes about 200 ms (≈10⁸ milligas) to remove a key of length 1,
      -- and the execution time grows linearly, hence it's about 10 milligas per key/value pair in the object
      let objSizeFactor = 10
      in MilliGas $ fromIntegral $ objSize * textCompareCost key * objSizeFactor
  GTranscendental top -> case top of
    TransExp p -> transExpCost p
    -- The estimated cost of computing log n is:
    -- for some number with `n` bits, `log(2^n) = n (log 2)`
    -- So computing `ln k` has a cost proportional to `n`.
    -- Assuming the multiplication cost is `n log n * (log (log n))
    -- for large
    -- Note: p is nonzero
    TransLn p -> MilliGas (cost_ln p)
    TransLogBase base num ->
      MilliGas (cost_ln base + cost_ln num)
    -- For square root, we use the formula, for n bits:
    -- n * log n * (log (log n))
    TransSqrt p
      | p > 0 ->
      let !n = numberOfBits p
          n_flt = (fromIntegral n :: Double)
      in MilliGas $ fromIntegral n * ceiling (log n_flt) * ceiling (log (log n_flt))
      | otherwise -> MilliGas 0
    where
    cost_ln :: Integer -> SatWord
    cost_ln p =
      let !n = numberOfBits p
          !n_flt = (fromIntegral n :: Double)
      in fromIntegral n * ceiling ((log n_flt) ** 2) * ceiling (log (log n_flt))
  GCapOp op -> case op of
    CapOpRequire cnt ->
      let mgPerCap = 100
      in MilliGas $ fromIntegral $ cnt * mgPerCap
  GHyperlaneMessageId m -> MilliGas $ fromIntegral m
  GHyperlaneEncodeDecodeTokenMessage m -> MilliGas $ fromIntegral m
  GHashOp hashOp -> case hashOp of
    GHashBlake w -> MilliGas $ w * _gcMHashBytePenalty
    GHashPoseidon len ->
      MilliGas $ fromIntegral (len * len) * _gcPoseidonQuadraticGasFactor + fromIntegral len * _gcPoseidonLinearGasFactor
    GHashKeccak chunkBytes ->
      let costPerOneHundredBytes = _gc_keccak256GasPerOneHundredBytes
          costPerChunk = _gc_keccak256GasPerChunk

              -- we need to use ceiling here, otherwise someone could cheat by
              -- having as many bytes as they want, but in chunks of 99 bytes.
          gasOne numBytesInChunk = costPerChunk + costPerOneHundredBytes * ceiling (fromIntegral @_ @Double numBytesInChunk / 100.0)

      in MilliGas (V.sum (V.map gasOne chunkBytes))
  where
  textCompareCost str = fromIntegral $ T.length str
  -- Running CountBytes costs 0.9 MilliGas, according to the analysis in bench/Bench.hs

-- This is the minimum amount of gas we mean to charge to simply use the gas table.
-- 25 milliGas = 62.5 nanoseconds, which is a negligible amount
-- _gcNativeBasicWork :: SatWord
-- _gcNativeBasicWork = 200

-- | Our internal gas table for constant costs on natives
coreBuiltinGasCost :: GasCostConfig -> CoreBuiltin -> MilliGas
coreBuiltinGasCost GasCostConfig{..} = MilliGas . \case
  -- Basic arithmetic
  -- note: add, sub, mul, div are special and are covered by
  -- special functions
  CoreAdd -> _gcNativeBasicWork
  CoreSub -> _gcNativeBasicWork
  CoreMultiply -> _gcNativeBasicWork
  CoreDivide -> _gcNativeBasicWork
  --
  CoreNegate -> _gcNativeBasicWork
  --
  CoreAbs -> _gcNativeBasicWork
  -- Pow is also a special case of recursive multiplication, gas table is not enough.
  CorePow -> _gcNativeBasicWork
  --
  CoreNot -> _gcNativeBasicWork
  -- ValEqGassed handles EQ and NEQ
  CoreEq -> _gcNativeBasicWork
  CoreNeq -> _gcNativeBasicWork
  -- Note: `litCmpGassed`
  CoreGT -> _gcNativeBasicWork
  CoreGEQ -> _gcNativeBasicWork
  CoreLT -> _gcNativeBasicWork
  CoreLEQ -> _gcNativeBasicWork
  -- All of the bitwise functions are quite fast, regardless of the size of the integer
  -- constant time gas is fine here
  CoreBitwiseAnd -> 250
  CoreBitwiseOr -> 250
  CoreBitwiseXor -> 250
  CoreBitwiseFlip -> 250
  -- Shift requires special handling
  -- given it can actually grow the number
  CoreBitShift -> _gcNativeBasicWork
  -- Todo: rounding likely needs benchmarks, but
  CoreRound -> _gcNativeBasicWork
  CoreCeiling -> _gcNativeBasicWork
  CoreFloor -> _gcNativeBasicWork
  CoreRoundPrec -> _gcNativeBasicWork
  CoreCeilingPrec -> _gcNativeBasicWork
  CoreFloorPrec -> _gcNativeBasicWork
  -- Todo: transcendental functions are definitely over_gassed
  CoreExp -> 2_000
  CoreLn -> 1_000
  CoreSqrt -> 1_000
  CoreLogBase -> 1_000
  -- note: length, take and drop are constant time
  -- for vector and string, but variable for maps
  CoreLength -> _gcNativeBasicWork
  -- Note: take and drop are gassed at their callsites
  CoreTake -> _gcNativeBasicWork
  CoreDrop -> _gcNativeBasicWork
  -- concat is gassed at its callsite
  CoreConcat -> _gcNativeBasicWork
  -- Note: reverse is gassed based on the number of elements
  CoreReverse -> _gcNativeBasicWork
  -- note: contains needs to be gassed based on the
  -- specific data structure, so flat gas won't do
  CoreContains -> _gcNativeBasicWork
  -- Note: Sorting gas is handling in sort
  CoreSort -> _gcNativeBasicWork
  CoreSortObject -> _gcNativeBasicWork
  -- Note: remove is gassed as its callsite
  CoreRemove -> _gcNativeBasicWork
  -- Modulo has the time time complexity as division
  CoreMod -> _gcNativeBasicWork
  -- Map, filter, zip complexity only requires a list reverse, so the only cost
  -- we will charge is the cost of reverse
  CoreMap ->
    _gcNativeBasicWork
  CoreFilter -> _gcNativeBasicWork
  CoreZip -> _gcNativeBasicWork
  -- The time complexity of fold is the time complexity of the uncons operation
  CoreFold -> _gcNativeBasicWork
  -- Note: these following functions are gassed at their callsite
  CoreIntToStr -> _gcNativeBasicWork
  CoreStrToInt -> _gcNativeBasicWork
  CoreStrToIntBase -> _gcNativeBasicWork
  -- Note: Distinct has special gas handling
  -- at its callsite
  CoreDistinct -> _gcNativeBasicWork
  CoreFormat -> _gcNativeBasicWork
  -- EnumerateN functions require more special gas handling as well
  CoreEnumerate -> _gcNativeBasicWork
  CoreEnumerateStepN -> _gcNativeBasicWork
  -- Show also requires stringification
  CoreShow -> _gcNativeBasicWork
  -- read-* functions no longer parse their input
  CoreReadMsg -> _gcNativeBasicWork * 2
  CoreReadMsgDefault -> _gcNativeBasicWork * 2
  CoreReadInteger -> _gcNativeBasicWork * 2
  CoreReadDecimal -> _gcNativeBasicWork * 2
  CoreReadString -> _gcNativeBasicWork * 2
  CoreReadKeyset -> _gcNativeBasicWork * 2
  -- Todo: Enforce functions should have variable gas
  -- based on the guard type
  CoreEnforceGuard -> 2_000
  CoreEnforceKeyset -> 2_000
  CoreKeysetRefGuard -> 2_000

  CoreAt -> _gcNativeBasicWork
  -- Make-list is essentially replicate, so we just
  -- need the gas penalty
  CoreMakeList -> _gcNativeBasicWork
  -- Note: this is gassed via `StrOpParse`
  CoreB64Encode -> 250
  CoreB64Decode -> 250
  -- Todo: str-to-list variable
  -- Str-to-list should be fast (it's essentially just a call to `unpack`)
  -- but should vary based on the length of the string
  CoreStrToList -> 250
  -- Yield is essentially all constant-time ops
  CoreYield -> _gcNativeBasicWork
  CoreYieldToChain -> _gcNativeBasicWork
  -- Resume already will use applyLam gas
  -- and the rest of the operations are constant time
  CoreResume -> 500
  -- Bind only applies a lambda
  CoreBind ->
    _gcNativeBasicWork
  -- Todo: cap function gas should depend on the work of eval-cap
  -- and the cap state
  CoreRequireCapability -> 500
  CoreComposeCapability -> 500
  CoreInstallCapability -> 500
  -- emit-event is constant work
  CoreEmitEvent -> 1_000
  -- Create-capability-guard is constant time and fast, in core we are cheaper here
  CoreCreateCapabilityGuard ->
    _gcNativeBasicWork
  CoreCreateCapabilityPactGuard ->
    _gcNativeBasicWork
  -- create-module-guard is a simple uncons
  CoreCreateModuleGuard -> 2 * _gcNativeBasicWork
  -- create-pact-guard is a simple uncons
  CoreCreateDefPactGuard -> 2 * _gcNativeBasicWork
  -- Create-table depends heavily on the implementation, but
  -- should return within a reasonable time frame. We
  -- charge a penalty for using within a tx
  CoreCreateTable -> 250_000
  -- The following functions also incur a gas penalty
  CoreDescribeKeyset -> _gcMetadataTxPenalty
  CoreDescribeModule -> _gcMetadataTxPenalty
  CoreDescribeTable -> _gcMetadataTxPenalty
  -- Registry functions
  -- both are constant-time work, but incur a db tx penalty
  CoreDefineKeySet ->
    5_000
  CoreDefineKeysetData ->
    5_000
  -- fold-db incurs currently a penalty on mainnet
  CoreFoldDb ->
    _gcSelectPenalty
  -- Insert db overhead
  CoreInsert ->
    _gcWritePenalty
  -- Todo: keys gas needs to be revisited. We leave in the current penalty
  CoreKeys ->
    _gcSelectPenalty
  -- read depends on bytes as well, 10 gas is a tx penalty
  CoreRead ->
    _gcReadPenalty
  CoreSelect ->
    _gcSelectPenalty
  CoreSelectWithFields ->
    _gcSelectPenalty
  -- Update same gas penalty as write and insert
  CoreUpdate -> _gcWritePenalty
  -- note: with-default read and read
  -- should cost the same.
  CoreWithDefaultRead ->
    _gcReadPenalty
  CoreWithRead -> _gcReadPenalty
  -- Write penalty as well
  CoreWrite -> _gcWritePenalty
  -- Tx-hash should be constant-time
  CoreTxHash ->
    _gcNativeBasicWork
  -- and? and co. should have essentially no penalty but whatever applyLam costs
  CoreAndQ -> _gcNativeBasicWork
  CoreOrQ -> _gcNativeBasicWork
  CoreWhere -> _gcNativeBasicWork
  CoreNotQ -> _gcNativeBasicWork
  -- Todo: hashGas depends on input
  CoreHash -> _gcNativeBasicWork
  -- Continue in pact-core currently amounts to `id`
  CoreContinue -> _gcNativeBasicWork
  -- Time functions complexity
  -- is handled in the function itself
  CoreParseTime -> 500
  CoreFormatTime -> 500
  -- Time parsing runs EXTREMELY quick
  --
  CoreTime -> 500

  -- Add-time and diff-time are essentially constant time
  -- time is a Word64
  CoreAddTime -> 250
  CoreDiffTime -> 250

  -- These 3 functions cost a bit more
  CoreHours -> 250
  CoreMinutes -> 250
  CoreDays -> 250

  -- Compose is constant time, and just evaluated in pact-core to
  -- some continuation manipulation
  CoreCompose ->
    _gcNativeBasicWork
  -- Note: create-principal is gassed via the principal creation functions
  CoreCreatePrincipal -> _gcNativeBasicWork
  CoreIsPrincipal -> _gcNativeBasicWork
  CoreTypeOfPrincipal -> _gcNativeBasicWork
  -- note: validate-principal is essentially a constant time comparison on fixed-length strings.
  -- The actual gassing of constructing the principal is done inside of it
  CoreValidatePrincipal -> 250
  -- Namespace function is constant work
  CoreNamespace -> _gcNativeBasicWork
  -- define-namespace tx penalty
  CoreDefineNamespace -> 25_000
  CoreDescribeNamespace -> _gcMetadataTxPenalty
  CoreChainData -> 500
  CoreIsCharset -> 500
  CorePactId -> _gcNativeBasicWork
  -- Note: pairing functions have custom gas
  CoreZkPairingCheck -> _gcNativeBasicWork
  CoreZKScalarMult -> _gcNativeBasicWork
  CoreZkPointAdd -> _gcNativeBasicWork
  CorePoseidonHashHackachain -> 124_000
  -- Note: type synthesis is constant time and very fast
  CoreTypeOf -> _gcNativeBasicWork
  -- note: Dec requires less gas overall
  CoreDec ->
    _gcNativeBasicWork
  CoreCond -> _gcNativeBasicWork
  CoreIdentity -> _gcNativeBasicWork
  CoreVerifySPV -> 100_000
  CoreEnforceVerifier -> 10_000
  CoreHyperlaneMessageId -> 2_000
  CoreHyperlaneDecodeMessage -> 2_000
  CoreHyperlaneEncodeMessage -> 2_000
  CoreAcquireModuleAdmin -> 20_000
  CoreReadWithFields -> _gcReadPenalty
  CoreListModules -> _gcMetadataTxPenalty
  CoreStaticRedeploy -> _gcNativeBasicWork
  CoreHashKeccak256 -> 1_000
  CoreHashPoseidon -> 124_000
{-# INLINABLE runTableModel #-}


replNativeGasTable :: ReplBuiltin CoreBuiltin -> MilliGas
replNativeGasTable = \case
  RBuiltinWrap bwrap -> coreBuiltinGasCost tableGasCostConfig bwrap
  _ -> mempty


-- [Decimal Comparisons]
-- The `Ord` instance, and by that measure and comparison on two decimals is done via
-- -- Round the two DecimalRaw values to the largest exponent.
--
-- roundMax :: (Integral i) => DecimalRaw i -> DecimalRaw i -> (Word8, i, i)
-- roundMax (Decimal _  0)   (Decimal _  0)  = (0,0,0)
-- roundMax (Decimal e1 n1)  (Decimal _  0)  = (e1,n1,0)
-- roundMax (Decimal _  0)   (Decimal e2 n2) = (e2,0,n2)
-- roundMax d1@(Decimal e1 n1) d2@(Decimal e2 n2)
--   | e1 == e2  = (e1, n1, n2)
--   | otherwise = (e, n1', n2')
--     where
--       e = max e1 e2
--       (Decimal _ n1') = roundTo e d1
--       (Decimal _ n2') = roundTo e d2
--
-- roundTo :: (Integral i) => Word8 -> DecimalRaw i -> DecimalRaw i
-- roundTo d (Decimal _ 0) = Decimal d 0
-- roundTo d (Decimal e n) = Decimal d $ fromIntegral n1
--     where
--       n1 = case compare d e of
--              LT -> n `divRound` divisor
--              EQ -> n
--              GT -> n * multiplier
--       divisor = 10 ^ (e-d)
--       multiplier = 10 ^ (d-e)
-- so the cost of two decimal comparisons involves a multiplication, and then an integer comparison.
-- thus, we must charge it as such
