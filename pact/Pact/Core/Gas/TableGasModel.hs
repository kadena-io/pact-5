{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DataKinds #-}

module Pact.Core.Gas.TableGasModel
 ( tableGasModel
 , replTableGasModel
 , runTableModel
  --
 , pointAddGas
 , scalarMulGas
 , pairingGas
 , serializationCosts)
 where

import Data.Word(Word64)
import Data.Ratio((%))
import GHC.Int(Int(..))
import qualified Data.Text as T
import GHC.Num.Integer
import qualified GHC.Integer.Logarithms as IntLog
import Pact.Core.Builtin
import Pact.Core.Gas.Types
import Data.Decimal

tableGasModel :: MilliGasLimit -> GasModel CoreBuiltin
tableGasModel gl =
  GasModel
  { _gmName = "table"
  , _gmGasLimit = Just gl
  , _gmDesc = "table-based cost model"
  , _gmNativeTable = coreBuiltinGasCost
  , _gmSerialize = serializationCosts
  }

replTableGasModel :: Maybe MilliGasLimit -> GasModel ReplCoreBuiltin
replTableGasModel gl =
  GasModel
  { _gmName = "table"
  , _gmGasLimit = gl
  , _gmDesc = "table-based cost model"
  , _gmNativeTable = replNativeGasTable
  , _gmSerialize = serializationCosts
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
-- TODO Note: 10^80 seems a bit restrictive given benchmarks show this is just hilariously fast.
-- up to 10^300, so 10^200 seems like a decent upper bound
-- intAdditionUpperBound =  265

integerBits :: Integer -> Int
integerBits = \case
  IS _ -> 64 -- note: Small ints are machine word sized
  i -> I# (IntLog.integerLog2# (abs i))
{-# INLINE integerBits #-}


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
    let !nbits = (I# (IntLog.integerLog2# (abs a)) + 1)
    in if nbits <= intCostUpperBound then MilliGas $ fromIntegral $ (3*nbits) `quot` 20 + 26
       else MilliGas $ fromIntegral (nbits * nbits `quot` 6400)
{-# INLINE intMultCost #-}

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
    in if nbits <= intCostUpperBound then MilliGas $ fromIntegral $ (3*nbits) `quot` 20 + 26
       else MilliGas $ fromIntegral (nbits * nbits `quot` 6400)
{-# INLINE intDivCost #-}

intShiftCost :: Integer -> Integer -> MilliGas
intShiftCost _ !rop
  | rop > 0 = MilliGas $ fromIntegral $ rop `quot` 1000
  | otherwise = MilliGas 0

intPowCost :: Integer -> Integer -> MilliGas
intPowCost !base !power = MilliGas $ g (I# (IntLog.integerLogBase# 10 (abs base))) (I# (IntLog.integerLogBase# 10 (abs power)))
  where
  g x y = f ((x - 3) `div` 2) ((y - 3) `div` 2)
  f x y = f00 * 2 ^ max x 0 * 300 ^ max y 0
  f00 = 1000
{- The benchmarks show the run time t, for x = log₁₀ base, y = log₁₀ power:
           |  y = 3  | y = 5  | y = 7
    x = 3  |  3 μs   | 1.2 ms | 302 ms
    x = 5  |  7 μs   | 2.1 ms | 591 ms
    x = 7  |  12 μs  | 4.3 ms | 988 ms

    Let `g(x, y)` the function describing this dependency.
    Note that `g(x + 2, y) = 2 g(x, y)`  and `g(x, y + 2) = 300 g(x, y)` with a good precision ε.
    For simplicity, let also `f` be such that `g(x, y) = f((x - 3) / 2, (y - 3) / 2)`.
    With a good precision ε, we thus need f such that
    ```
    f(0, 0) = 3 [μs]
    f(x + 1, y) = 2 f(x, y)
    f(x, y + 1) = 300 f(x, y)
    ```
    This means `f(x, y) = a^x • b^y • f₀₀`.
    The first equation above means `f₀₀ = 3 [μs] ≈ 1000 [milligas]`, the second one means `a = 2`, and the third one means `b = 300`.
 -}

runTableModel :: (b -> MilliGas) -> GasArgs b -> MilliGas
runTableModel nativeTable = \case
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
      MilliGas (fromIntegral totalLen * 100)
    TextListConcat (GasTextLength totalCharSize) (GasListLength nElems) -> MilliGas $
      fromIntegral totalCharSize * stringLenCost + fromIntegral nElems * listLenCost
      where
      stringLenCost,listLenCost :: Word64
      stringLenCost = 100
      listLenCost = 40
    ListConcat (GasListLength totalLen) ->
      MilliGas (fromIntegral totalLen * 100)
    ObjConcat totalLen ->
      MilliGas (fromIntegral totalLen * 100)
  GAApplyLam _ !i -> MilliGas $ fromIntegral i * applyLamCostPerArg + 50
  GAZKArgs zka -> case zka of
    PointAdd g -> pointAddGas g
    ScalarMult g -> scalarMulGas g
    Pairing np -> pairingGas np
  -- Todo: Gwrite needs benchmarking
  GWrite bytes -> memoryCost bytes
  GRead bytes ->
    let mgPerByte = 1
    in MilliGas $ fromIntegral $ bytes * mgPerByte
    -- a string of 10⁶ chars (which is 2×10⁶ sizeof bytes) takes a little less than 2×10⁶ to write
  GMakeList len sz ->
    MilliGas $ fromIntegral len * sz
  GComparison cmpty -> case cmpty of
    TextComparison str ->
      MilliGas $ textCompareCost str + basicWorkGas
    IntComparison l r ->
      MilliGas $ fromIntegral (max (integerBits l) (integerBits r)) + basicWorkGas
    -- See [Decimal comparisons]
    DecimalComparison l r ->
      let !lmantissa = decimalMantissa l
          !rmantissa = decimalMantissa r
      in
      intDivCost lmantissa rmantissa <> MilliGas (fromIntegral (max (integerBits lmantissa) (integerBits rmantissa)) + basicWorkGas)
    ListComparison maxSz ->
      MilliGas $ fromIntegral maxSz * basicWorkGas
    ObjComparison i ->
      MilliGas $ fromIntegral i * basicWorkGas
  GSearch sty -> case sty of
    SubstringSearch needle hay -> MilliGas $ fromIntegral (T.length needle + T.length hay) + basicWorkGas
    FieldSearch cnt -> MilliGas $ fromIntegral cnt + basicWorkGas
  GPoseidonHashHackAChain len ->
    MilliGas $ fromIntegral (len * len) * quadraticGasFactor + fromIntegral len * linearGasFactor
     where
     quadraticGasFactor, linearGasFactor :: Word64
     quadraticGasFactor = 50_000
     linearGasFactor = 38_000
  GModuleMemory bytes -> moduleMemoryCost bytes
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
      let objSzLog = fromIntegral $ I# (IntLog.integerLog2# $ fromIntegral objSize) + 1
      in MilliGas $ objSzLog * textCompareCost key
    ObjOpRemove key objSize ->
      -- an object with 10⁷ keys takes about 200 ms (≈10⁸ milligas) to remove a key of length 1,
      -- and the execution time grows linearly, hence it's about 10 milligas per key/value pair in the object
      let objSizeFactor = 10
      in MilliGas $ fromIntegral $ objSize * textCompareCost key * objSizeFactor
  GCapOp op -> case op of
    CapOpRequire cnt ->
      let mgPerCap = 100
      in MilliGas $ fromIntegral $ cnt * mgPerCap
  GCountBytes -> MilliGas 1
  GHyperlaneMessageId m -> MilliGas $ fromIntegral m
  GHyperlaneEncodeDecodeTokenMessage m -> MilliGas $ fromIntegral m
  where
  textCompareCost str = fromIntegral $ T.length str
  -- Running CountBytes costs 0.9 MilliGas, according to the analysis in bench/Bench.hs

basicWorkGas :: Word64
basicWorkGas = 25

-- Slope to costing function,
-- sets a 10mb practical limit on module sizes.
moduleMemFeePerByte :: Rational
moduleMemFeePerByte = 0.006

-- 0.01x+50000 linear costing funciton
moduleMemoryCost :: Word64 -> MilliGas
moduleMemoryCost sz = MilliGas $ ceiling (moduleMemFeePerByte * fromIntegral sz) + 60_000_000
{-# INLINE moduleMemoryCost #-}

perByteFactor :: Rational
perByteFactor = 1%10


applyLamCostPerArg :: Word64
applyLamCostPerArg = 25

memoryCost :: Word64 -> MilliGas
memoryCost !bytes = gasToMilliGas (Gas totalCost)
  where
  !sizeFrac = realToFrac bytes
  !totalCost = ceiling (perByteFactor * sizeFrac)
{-# INLINE memoryCost #-}


-- | Our internal gas table for constant costs on natives
coreBuiltinGasCost :: CoreBuiltin -> MilliGas
coreBuiltinGasCost = MilliGas . \case
  -- Basic arithmetic
  -- note: add, sub, mul, div are special and are covered by
  -- special functions
  CoreAdd -> basicWorkGas
  CoreSub -> basicWorkGas
  CoreMultiply -> basicWorkGas
  CoreDivide -> basicWorkGas
  --
  CoreNegate -> 50
  --
  CoreAbs -> 50
  -- Pow is also a special case of recursive multiplication, gas table is not enough.
  CorePow -> 4_000
  --
  CoreNot -> basicWorkGas
  -- Todo: ORD functions are special.
  -- They require more in-depth analysis than currently is being done
  CoreEq -> 1_000
  CoreNeq -> 1_000
  CoreGT -> basicWorkGas
  CoreGEQ -> basicWorkGas
  CoreLT -> basicWorkGas
  CoreLEQ -> basicWorkGas
  -- All of the bitwise functions are quite fast, regardless of the size of the integer
  -- constant time gas is fine here
  CoreBitwiseAnd -> 1_000
  CoreBitwiseOr -> 1_000
  CoreBitwiseXor -> 1_000
  CoreBitwiseFlip -> 1_000
  -- Shift requires special handling
  -- given it can actually grow the number
  CoreBitShift -> 1
  -- Todo: rounding likely needs benchmarks, but
  CoreRound -> basicWorkGas
  CoreCeiling -> basicWorkGas
  CoreFloor -> basicWorkGas
  CoreRoundPrec -> basicWorkGas
  CoreCeilingPrec -> basicWorkGas
  CoreFloorPrec -> basicWorkGas
  -- Todo: transcendental functions are definitely over_gassed
  CoreExp -> 5_000
  CoreLn -> 6_000
  CoreSqrt -> 6_000
  CoreLogBase -> 3_000
  -- note: length, take and drop are constant time
  -- for vector and string, but variable for maps
  -- Todo: gas take/drop on objects
  CoreLength -> basicWorkGas
  CoreTake -> basicWorkGas
  CoreDrop -> basicWorkGas
  -- concat
  -- Todo: concat gas based on total length of text
  CoreConcat -> 1
  -- Todo: reverse gas based on `n` elements
  CoreReverse -> 1
  -- note: contains needs to be gassed based on the
  -- specific data structure, so flat gas won't do
  CoreContains -> 1
  -- Todo: sorting gas needs to be revisited
  CoreSort -> 1
  CoreSortObject -> 1
  -- Todo: Delete is O(log n)
  CoreRemove -> 1
  -- Modulo has the time time complexity as division
  CoreMod -> 1
  -- Map, filter, zip complexity only requires a list reverse, so the only cost
  -- we will charge is the cost of reverse
  CoreMap ->
    basicWorkGas
  CoreFilter -> 1
  CoreZip -> 1
  -- The time complexity of fold is the time complexity of the uncons operation
  CoreFold -> basicWorkGas
  -- Todo: complexity of these
  CoreIntToStr -> 1
  CoreStrToInt -> 1
  CoreStrToIntBase -> 1
  -- Todo: Distinct and format require
  -- special gas handling
  CoreDistinct -> 1
  CoreFormat -> 1
  -- EnumerateN functions require more special gas handling as well
  CoreEnumerate -> 1
  CoreEnumerateStepN -> 1
  -- Show also requires stringification
  CoreShow -> 1
  -- read-* functions no longer parse their input
  -- TODO: is this fine?
  CoreReadMsg -> basicWorkGas
  CoreReadMsgDefault -> basicWorkGas
  CoreReadInteger -> basicWorkGas
  CoreReadDecimal -> basicWorkGas
  CoreReadString -> basicWorkGas
  CoreReadKeyset -> basicWorkGas
  -- Todo: Enforce functions should have variable gas
  -- based on the guard type
  CoreEnforceGuard -> 8_000
  CoreEnforceKeyset -> 8_000
  CoreKeysetRefGuard -> 7_000
  -- Todo: At-costs
  -- depend on impl
  CoreAt -> 1_000
  -- Make-list is essentially replicate, so we just
  -- need the gas penalty
  CoreMakeList -> 1
  -- Todo: complexity of b64 encode/decode
  CoreB64Encode -> 1_000
  CoreB64Decode -> 1_000
  -- Todo: str-to-list variable
  -- Str-to-list should be fast (it's essentially just a call to `unpack`)
  -- but should vary based on the length of the string
  CoreStrToList -> 1_000
  -- Yield is essentially all constant-time ops
  CoreYield -> 2_000
  CoreYieldToChain -> 2_000
  -- Resume already will use applyLam gas
  -- and the rest of the operations are constant time
  CoreResume -> 2_000
  -- Bind only applies a lambda
  CoreBind ->
    basicWorkGas
  -- Todo: cap function gas should depend on the work of eval-cap
  -- and the cap state
  CoreRequireCapability -> 1_000
  CoreComposeCapability -> 1_000
  CoreInstallCapability -> 3_000
  -- emit-event is constant work
  CoreEmitEvent -> 1_000
  -- Create-capability-guard is constant time and fast, in core we are cheaper here
  CoreCreateCapabilityGuard ->
    basicWorkGas
  CoreCreateCapabilityPactGuard ->
    basicWorkGas
  -- create-module-guard is a simple uncons
  CoreCreateModuleGuard -> 2 * basicWorkGas
  -- create-pact-guard is a simple uncons
  CoreCreateDefPactGuard -> 2 * basicWorkGas
  -- Create-table depends heavily on the implementation, but
  -- should return within a reasonable time frame. We
  -- charge a penalty for using within a tx
  CoreCreateTable -> 250_000
  -- The following functions also incur a gas penalty
  CoreDescribeKeyset -> dbMetadataTxPenalty
  CoreDescribeModule -> dbMetadataTxPenalty
  CoreDescribeTable -> dbMetadataTxPenalty
  -- Registry functions
  -- both are constant-time work, but incur a db tx penalty
  CoreDefineKeySet ->
    25_000
  CoreDefineKeysetData ->
    25_000
  -- fold-db incurs currently a penalty on mainnet
  CoreFoldDb ->
    dbSelectPenalty
  -- Insert db overhead
  CoreInsert ->
    100_000
  -- Todo: keys gas needs to be revisited. We leave in the current penalty
  CoreKeys ->
    dbSelectPenalty
  -- read depends on bytes as well, 10 gas is a tx penalty
  CoreRead ->
    10_000
  CoreSelect ->
    40_000_000
  CoreSelectWithFields ->
    40_000_000
  -- Update same gas penalty as write and insert
  CoreUpdate -> 100_000
  -- note: with-default read and read
  -- should cost the same.
  CoreWithDefaultRead ->
    dbReadPenalty
  CoreWithRead -> dbReadPenalty
  -- Write penalty as well
  CoreWrite -> dbWritePenalty
  -- Tx-hash should be constant-time
  -- Todo: benchmark. b64 url conversion is constant time since tx hashes
  -- are of fixed size
  CoreTxHash ->
    basicWorkGas
  -- and? and co. should have essentially no penalty but whatever applyLam costs
  CoreAndQ -> basicWorkGas
  CoreOrQ -> basicWorkGas
  CoreWhere -> basicWorkGas
  CoreNotQ -> basicWorkGas
  -- Todo: hashGas depends on input
  CoreHash -> basicWorkGas
  -- Continue in pact-core currently amounts to `id`
  CoreContinue -> basicWorkGas
  -- Todo: Time functions complexity
  CoreParseTime -> 2_000
  CoreFormatTime -> 4_000
  CoreTime ->
    2_000
  CoreAddTime -> 3_000
  CoreDiffTime -> 8_000
  CoreHours -> 4_000
  CoreMinutes -> 4_000
  CoreDays -> 4_000
  -- Compose is constant time, and just evaluated in pact-core to
  -- some continuation manipulation
  CoreCompose ->
    basicWorkGas
  -- Note: create-principal is gassed via the principal creation functions
  CoreCreatePrincipal -> basicWorkGas
  CoreIsPrincipal -> basicWorkGas
  CoreTypeOfPrincipal -> basicWorkGas
  -- note: validate-principal is essentially a constant time comparison on fixed-length strings.
  -- Todo: benchmark validate-principal
  CoreValidatePrincipal -> 1_000
  -- Namespace function is constant work
  CoreNamespace -> basicWorkGas
  -- define-namespace tx penalty
  CoreDefineNamespace -> 25_000
  CoreDescribeNamespace -> dbMetadataTxPenalty
  CoreChainData -> 500
  CoreIsCharset -> 500
  CorePactId -> basicWorkGas
  -- Note: pairing functions have custom gas
  CoreZkPairingCheck -> basicWorkGas
  CoreZKScalarMult -> basicWorkGas
  CoreZkPointAdd -> basicWorkGas
  CorePoseidonHashHackachain -> 124_000
  -- Note: type synthesis is constant time and very fast
  CoreTypeOf -> basicWorkGas
  -- note: Dec requires less gas overall
  CoreDec ->
    basicWorkGas
  CoreCond -> basicWorkGas
  CoreIdentity -> basicWorkGas
  CoreVerifySPV -> 100_000
  CoreEnforceVerifier -> 10_000
  CoreHyperlaneMessageId -> 2_000
  CoreHyperlaneDecodeMessage -> 2_000
  CoreHyperlaneEncodeMessage -> 2_000
  CoreAcquireModuleAdmin -> 20_000
  CoreReadWithFields -> 10_000
  CoreListModules -> dbMetadataTxPenalty
{-# INLINABLE runTableModel #-}


replNativeGasTable :: ReplBuiltin CoreBuiltin -> MilliGas
replNativeGasTable = \case
  RBuiltinWrap bwrap -> coreBuiltinGasCost bwrap
  _ -> mempty



dbSelectPenalty :: Word64
dbSelectPenalty = 40_000_000

dbWritePenalty :: Word64
dbWritePenalty = 100_000

dbReadPenalty :: Word64
dbReadPenalty = 10_000

dbMetadataTxPenalty :: Word64
dbMetadataTxPenalty = 100_000

-- PactValue serialization costs
serializationCosts :: SerializationCosts
serializationCosts = SerializationCosts
  { objectKeyCostMilliGasOffset = 1
  , objectKeyCostMilliGasPer1000Chars = 69
  , boolMilliGasCost = 52
  , unitMilliGasCost = 51
  , integerCostMilliGasPerDigit = 2
  , decimalCostMilliGasOffset = 59
  , decimalCostMilliGasPerDigit = 2
  , timeCostMilliGas = 184
  }



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
