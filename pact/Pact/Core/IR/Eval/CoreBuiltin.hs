{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE CPP #-}

module Pact.Core.IR.Eval.CoreBuiltin
 ( coreBuiltinRuntime
 , coreBuiltinEnv
 , coreEnforceGuard) where

-- |
-- Module      :  Pact.Core.Eval.CoreBuiltin
-- Copyright   :  (C) 2022 Kadena
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jose Cardona <jose@kadena.io>
--
-- CEK runtime for our IR term
--

import Control.Lens hiding (from, to, op, parts)
import Control.Monad
import Control.Monad.IO.Class
import Data.Attoparsec.Text(parseOnly)
import Data.Bits
import Data.Either(isLeft, isRight)
import Data.Foldable
import Data.Decimal(roundTo', Decimal, DecimalRaw(..))
import Numeric(showIntAtBase)
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as V
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Char as Char
import qualified Data.ByteString as BS
import qualified GHC.Exts as Exts
import qualified GHC.Integer.Logarithms as IntLog
import qualified Pact.Time as PactTime
#ifndef WITHOUT_CRYPTO
import qualified Control.Lens as Lens
#endif

import Pact.Core.Builtin
import Pact.Core.Literal
import Pact.Core.Errors
import Pact.Core.Hash
import Pact.Core.Names
import Pact.Core.Guards
import Pact.Core.PactValue
import Pact.Core.Persistence
import Pact.Core.DefPacts.Types
import Pact.Core.Environment
import Pact.Core.Capabilities
import Pact.Core.Namespace
import Pact.Core.Gas
import Pact.Core.Type
#ifndef WITHOUT_CRYPTO
import Pact.Core.Crypto.Pairing
import Pact.Core.Crypto.Hash.Poseidon
#endif
import Pact.Crypto.Hyperlane

import Pact.Core.IR.Term
import Pact.Core.IR.Eval.Runtime
import Pact.Core.StableEncoding
import Pact.Core.IR.Eval.CEK
import Pact.Core.SizeOf
import Pact.Core.SPV

import qualified Pact.Core.Principal as Pr
import qualified Pact.Core.Trans.TOps as Musl

import qualified Data.Binary.Get as Bin
import qualified Data.Binary.Put as Bin

----------------------------------------------------------------------
-- Our builtin definitions start here
----------------------------------------------------------------------
unaryIntFn :: (CEKEval e step b i, IsBuiltin b) => (Integer -> Integer) -> NativeFunction e step b i
unaryIntFn op info b cont handler _env = \case
  [VLiteral (LInteger i)] ->
    returnCEKValue cont handler (VLiteral (LInteger (op i)))
  args -> argsError info b args
{-# INLINE unaryIntFn #-}

binaryIntFn
  :: (CEKEval e step b i, IsBuiltin b)
  => (Integer -> Integer -> Integer)
  -> NativeFunction e step b i
binaryIntFn op info b cont handler _env = \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> returnCEKValue cont handler (VLiteral (LInteger (op i i')))
  args -> argsError info b args
{-# INLINE binaryIntFn #-}

-- The majority of the asymptotic cost in here is this function:
-- ```
-- roundTo' :: (Integral i) => (Rational -> i) -> Word8 -> DecimalRaw i -> DecimalRaw i
-- roundTo' _ d (Decimal _  0) = Decimal d 0
-- roundTo' f d (Decimal e n) = Decimal d $ f n1
--    where
--       divisor = 10 ^ (e-d)
--       multiplier = 10 ^ (d-e)
--       n1 = case compare d e of
--          LT -> toRational n / divisor
--          EQ -> toRational n
--          GT -> toRational n * multiplier
-- `roundTo'` thus has the same asymptotic complexity as multiplication/division. Thus, worst case, we can upperbound it via
-- division
roundingFn :: (CEKEval e step b i, IsBuiltin b) => (Rational -> Integer) -> NativeFunction e step b i
roundingFn op info b cont handler _env = \case
  [VLiteral (LDecimal d)] ->
    returnCEKValue cont handler (VLiteral (LInteger (truncate (roundTo' op 0 d))))
  [VDecimal d, VInteger prec] ->
    returnCEKValue cont handler (VLiteral (LDecimal (roundTo' op (fromIntegral prec) d)))
  args -> argsError info b args
{-# INLINE roundingFn #-}

---------------------------------
-- Arithmetic Ops
------------------------------
{-# SPECIALIZE rawAdd
   :: NativeFunction ExecRuntime CEKBigStep CoreBuiltin i
    #-}
{-# SPECIALIZE rawAdd
   :: NativeFunction ExecRuntime CEKSmallStep CoreBuiltin i
    #-}
rawAdd :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
rawAdd info b cont handler _env = \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> do
    chargeGasArgs info (GIntegerOpCost PrimOpAdd i i')
    returnCEKValue cont handler (VLiteral (LInteger (i + i')))
  -- Overloaded decimal cases
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> do
    decimalAdd i i'
  [VLiteral (LInteger i), VLiteral (LDecimal i')] -> do
    decimalAdd (fromInteger i) i'
  [VLiteral (LDecimal i), VLiteral (LInteger i')] -> do
    decimalAdd i (Decimal 0 i')

  [VLiteral (LString i), VLiteral (LString i')] -> do
    chargeGasArgs info (GConcat (TextConcat (GasTextLength (T.length i + T.length i'))))
    returnCEKValue cont handler  (VLiteral (LString (i <> i')))
  [VObject l, VObject r] -> do
    chargeGasArgs info (GConcat (ObjConcat (M.size l + M.size r)))
    let o' = VObject (l `M.union` r)
    returnCEKValue cont handler o'
  [VList l, VList r] -> do
    chargeGasArgs info (GConcat (ListConcat (GasListLength (V.length l + V.length r))))
    returnCEKValue cont handler (VList (l <> r))
  args -> argsError info b args
  where
  decimalAdd i i' = do
    chargeGasArgs info (GIntegerOpCost PrimOpAdd (decimalMantissa i) (decimalMantissa i'))
    returnCEKValue cont handler (VLiteral (LDecimal (i + i')))

rawSub :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
rawSub info b cont handler _env = \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> do
    chargeGasArgs info (GIntegerOpCost PrimOpSub i i')
    returnCEKValue cont handler (VLiteral (LInteger (i - i')))
  -- Overloaded decimal cases
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] ->
    decimalSub i i'
  [VLiteral (LInteger i), VLiteral (LDecimal i')] ->
    decimalSub (Decimal 0 i) i'
  [VLiteral (LDecimal i), VLiteral (LInteger i')] ->
    decimalSub i (Decimal 0 i')
  args -> argsError info b args
  where
  decimalSub i i' = do
    chargeGasArgs info (GIntegerOpCost PrimOpSub (decimalMantissa i) (decimalMantissa i'))
    returnCEKValue cont handler (VLiteral (LDecimal (i - i')))



rawMul :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
rawMul info b cont handler _env = \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> do
    chargeGasArgs info (GIntegerOpCost PrimOpMul i i')
    returnCEKValue cont handler (VLiteral (LInteger (i * i')))
  -- overloads for decimal multiplication
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] ->
    decimalMul i i'
  [VLiteral (LInteger i), VLiteral (LDecimal i')] ->
    decimalMul (Decimal 0 i) i'
  [VLiteral (LDecimal i), VLiteral (LInteger i')] ->
    decimalMul i (Decimal 0 i')

  args -> argsError info b args
  where
  decimalMul i i' = do
    chargeGasArgs info (GIntegerOpCost PrimOpMul (decimalMantissa i) (decimalMantissa i'))
    returnCEKValue cont handler (VLiteral (LDecimal (i * i')))

rawPow :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
rawPow info b cont handler _env = \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> do
    chargeGasArgs info $ GIntegerOpCost PrimOpPow i i'
    when (i' < 0) $ throwExecutionError info (ArithmeticException "negative exponent in integer power")
    -- Todo: move to iterated pow
    returnCEKValue cont handler (VLiteral (LInteger (i ^ i')))
  [VLiteral (LDecimal l), VLiteral (LDecimal r)] -> do
    decPow l r
  [VLiteral (LInteger i), VLiteral (LDecimal i')] ->
    decPow (Decimal 0 i) i'
  [VLiteral (LDecimal i), VLiteral (LInteger i')] ->
    decPow i (Decimal 0 i')
  args -> argsError info b args
  where
  decPow l r = do
    let result = Musl.trans_pow (dec2F l) (dec2F r)
    guardNanOrInf info result
    returnCEKValue cont handler (VLiteral (LDecimal (f2Dec result)))

rawLogBase :: forall e step b i. (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
rawLogBase info b cont handler _env = \case
  [VLiteral (LInteger base), VLiteral (LInteger n)] -> do
    checkArgs base n
    let base' = fromIntegral base :: Double
        n' = fromIntegral n
        result = Musl.trans_logBase base' n'
    guardNanOrInf info result
    returnCEKValue cont handler (VLiteral (LInteger (round result)))
  [VLiteral (LDecimal base), VLiteral (LDecimal arg)] -> do
     decLogBase base arg
  [VLiteral (LInteger base), VLiteral (LDecimal arg)] -> do
     decLogBase (Decimal 0 base) arg
  [VLiteral (LDecimal base), VLiteral (LInteger arg)] -> do
     decLogBase base (Decimal 0 arg)
  args -> argsError info b args
  where
  decLogBase base arg = do
    checkArgs base arg
    let result = Musl.trans_logBase (dec2F base) (dec2F arg)
    guardNanOrInf info result
    returnCEKValue cont handler (VLiteral (LDecimal (f2Dec result)))
  checkArgs :: (Num a, Ord a) => a -> a -> EvalM e b i ()
  checkArgs base arg = do
    when (base < 0) $ throwExecutionError info (ArithmeticException "Negative log base")
    when (arg <= 0) $ throwExecutionError info (ArithmeticException "Non-positive log argument")


rawDiv :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
rawDiv info b cont handler _env = \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> do
    when (i' == 0) $ throwExecutionError info (ArithmeticException "div by zero")
    chargeGasArgs info (GIntegerOpCost PrimOpDiv i i')
    returnCEKValue cont handler (VLiteral (LInteger (div i i')))

  [VLiteral (LInteger i), VLiteral (LDecimal i')] ->
    decimalDiv (Decimal 0 i) i'
  [VLiteral (LDecimal i), VLiteral (LInteger i')] ->
    decimalDiv i (Decimal 0 i')
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] ->
    decimalDiv i i'

  args -> argsError info b args
  where
  decimalDiv i i' = do
    when (i' == 0) $ throwExecutionError info (ArithmeticException "div by zero, decimal")
    chargeGasArgs info (GIntegerOpCost PrimOpDiv (decimalMantissa i) (decimalMantissa i'))
    returnCEKValue cont handler (VLiteral (LDecimal (i / i')))


rawNegate :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
rawNegate info b cont handler _env = \case
  [VLiteral (LInteger i)] ->
    returnCEKValue cont handler (VLiteral (LInteger (negate i)))
  [VLiteral (LDecimal i)] ->
    returnCEKValue cont handler (VLiteral (LDecimal (negate i)))
  args -> argsError info b args

rawEq :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
rawEq info b cont handler _env = \case
  [VPactValue pv, VPactValue pv'] -> do
    isEq <- valEqGassed info pv pv'
    returnCEKValue cont handler (VBool isEq)
  args -> argsError info b args

modInt :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
modInt = binaryIntFn mod

rawNeq :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
rawNeq info b cont handler _env = \case
  [VPactValue pv, VPactValue pv'] -> do
    isEq <- valEqGassed info pv pv'
    returnCEKValue cont handler (VBool $ not isEq)
  args -> argsError info b args

rawGt :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
rawGt = defCmp (== GT)

rawLt :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
rawLt = defCmp (== LT)

rawGeq :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
rawGeq = defCmp (`elem` [GT, EQ])

rawLeq :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
rawLeq = defCmp (`elem` [LT, EQ])

defCmp :: (CEKEval e step b i, IsBuiltin b) => (Ordering -> Bool) -> NativeFunction e step b i
defCmp predicate info b cont handler _env = \case
  args@[VLiteral lit1, VLiteral lit2] -> litCmpGassed info lit1 lit2 >>= \case
    Just ordering -> returnCEKValue cont handler $ VBool $ predicate ordering
    Nothing -> argsError info b args
  -- Todo: time comparisons
  [VTime l, VTime r] -> returnCEKValue cont handler $ VBool $ predicate (compare l r)
  args -> argsError info b args
{-# INLINE defCmp #-}

bitAndInt :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
bitAndInt = binaryIntFn (.&.)

bitOrInt :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
bitOrInt = binaryIntFn (.|.)

bitComplementInt :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
bitComplementInt = unaryIntFn complement

bitXorInt :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
bitXorInt = binaryIntFn xor

bitShiftInt :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
bitShiftInt info b cont handler _env = \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> do
    chargeGasArgs info $ GIntegerOpCost PrimOpShift i i'
    returnCEKValue cont handler (VLiteral (LInteger (i `shift` fromIntegral i')))
  args -> argsError info b args

rawAbs :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
rawAbs info b cont handler _env = \case
  [VLiteral (LInteger i)] -> do
    returnCEKValue cont handler (VLiteral (LInteger (abs i)))
  [VLiteral (LDecimal e)] -> do
    returnCEKValue cont handler (VLiteral (LDecimal (abs e)))
  args -> argsError info b args

rawExp :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
rawExp info b cont handler _env = \case
  [VLiteral (LInteger i)] -> do
    let result = Musl.trans_exp (fromIntegral i)
    guardNanOrInf info result
    returnCEKValue cont handler (VLiteral (LDecimal (f2Dec result)))
  [VLiteral (LDecimal e)] -> do
    let result = Musl.trans_exp (dec2F e)
    guardNanOrInf info result
    returnCEKValue cont handler (VLiteral (LDecimal (f2Dec result)))
  args -> argsError info b args

rawLn :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
rawLn info b cont handler _env = \case
  [VLiteral (LInteger i)] -> do
    let result = Musl.trans_ln (fromIntegral i)
    guardNanOrInf info result
    returnCEKValue cont handler (VLiteral (LDecimal (f2Dec result)))
  [VLiteral (LDecimal e)] -> do
    let result = Musl.trans_ln (dec2F e)
    guardNanOrInf info result
    returnCEKValue cont handler (VLiteral (LDecimal (f2Dec result)))
  args -> argsError info b args

rawSqrt :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
rawSqrt info b cont handler _env = \case
  [VLiteral (LInteger i)] -> do
    when (i < 0) $ throwExecutionError info (ArithmeticException "Square root must be non-negative")
    let result = Musl.trans_sqrt (fromIntegral i)
    guardNanOrInf info result
    returnCEKValue cont handler (VLiteral (LDecimal (f2Dec result)))
  [VLiteral (LDecimal e)] -> do
    when (e < 0) $ throwExecutionError info (ArithmeticException "Square root must be non-negative")
    let result = Musl.trans_sqrt (dec2F e)
    guardNanOrInf info result
    returnCEKValue cont handler (VLiteral (LDecimal (f2Dec result)))
  args -> argsError info b args



rawShow :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
rawShow info b cont handler _env = \case
  [VPactValue pv] -> do
    str <- renderPactValue info pv
    returnCEKValue cont handler $ VString str
  args -> argsError info b args

-- Todo: Gas here is complicated, greg worked on this previously
rawContains :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
rawContains info b cont handler _env = \case
  [VString f, VObject o] -> do
    chargeGasArgs info $ GSearch $ FieldSearch (M.size o)
    returnCEKValue cont handler (VBool (M.member (Field f) o))
  [VString needle, VString hay] -> do
    chargeGasArgs info $ GSearch $ SubstringSearch needle hay
    returnCEKValue cont handler (VBool (needle `T.isInfixOf` hay))
  [VPactValue v, VList vli] -> do
    let search True _ = pure True
        search _ el = valEqGassed info v el
    res <- foldlM search False vli
    returnCEKValue cont handler (VBool res)
  args -> argsError info b args

rawSort :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
rawSort info b cont handler _env = \case
  [VList vli]
    | V.null vli -> returnCEKValue cont handler (VList mempty)
    | otherwise -> do
    vli' <- liftIO $ do
      v' <- V.thaw vli
      V.sort v'
      V.freeze v'
    returnCEKValue cont handler (VList vli')
  args -> argsError info b args

coreRemove :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
coreRemove info b cont handler _env = \case
  [VString s, VObject o] -> do
    chargeGasArgs info $ GObjOp $ ObjOpRemove s (M.size o)
    returnCEKValue cont handler (VObject (M.delete (Field s) o))
  args -> argsError info b args

asObject
  :: IsBuiltin b
  => i
  -> b
  -> PactValue
  -> EvalM e b i (M.Map Field PactValue)
asObject info b = \case
  PObject o -> pure o
  arg -> argsError info b [VPactValue arg]

rawSortObject :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
rawSortObject info b cont handler _env = \case
  [VList fields, VList objs]
    | V.null fields -> returnCEKValue cont handler (VList objs)
    | V.null objs -> returnCEKValue cont handler (VList objs)
    | otherwise -> do
        objs' <- traverse (asObject info b) objs
        fields' <- traverse (fmap Field . asString info b) fields
        v' <- liftIO $ do
          mobjs <- V.thaw objs'
          V.sortBy (sort fields') mobjs
          V.freeze mobjs
        returnCEKValue cont handler (VList (PObject <$> v'))
    where
    sort fs o o' =
      foldr go EQ fs
      where
      go field EQ = case (,) <$> M.lookup field o <*> M.lookup field o' of
        Just (PLiteral l1, PLiteral l2) -> l1 `compare` l2
        _ -> EQ
      go _ ne = ne
  args -> argsError info b args


-- -------------------------
-- double ops
-- -------------------------

guardNanOrInf :: i -> Double -> EvalM e b i ()
guardNanOrInf info a =
  when (isNaN a || isInfinite a) $ throwExecutionError info (FloatingPointError "Floating operation resulted in Infinity or NaN")

dec2F :: Decimal -> Double
dec2F = fromRational . toRational

f2Dec :: Double -> Decimal
f2Dec = fromRational . toRational

roundDec :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
roundDec = roundingFn round

floorDec :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
floorDec = roundingFn floor

ceilingDec :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
ceilingDec = roundingFn ceiling

---------------------------
-- bool ops
---------------------------
notBool :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
notBool info b cont handler _env = \case
  [VLiteral (LBool i)] -> returnCEKValue cont handler  (VLiteral (LBool (not i)))
  args -> argsError info b args

---------------------------
-- string ops
---------------------------

-- Note: [Take/Drop Clamping]
-- Take an expression like one of the following:
--  When i >= 0:
--    let clamp = fromIntegral $ min i (fromIntegral (T.length t))
--  When i < 0:
--    let clamp = fromIntegral $ max (fromIntegral (T.length t) + i) 0
--
-- Note that it's `max (fromIntegral (T.length t) + i) 0` and not `max (T.length t + fromIntegral i) 0`.
-- That's because `i` may contain values larger than `Int`, which is the type `length` typically returns.
-- The sum `i + length t` may overflow `Int`, so it's converted to `Integer`, and the result of the `clamp` is always
-- below `maxBound :: Int`, so it can be safely casted back without overflow.
rawTake :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
rawTake info b cont handler _env = \case
  [VLiteral (LInteger i), VLiteral (LString t)]
    | i >= 0 -> do
      -- See Note: [Take/Drop Clamping]
      let clamp = fromIntegral $ min i (fromIntegral (T.length t))
      chargeGasArgs info $ GConcat $ TextConcat $ GasTextLength clamp
      returnCEKValue cont handler  (VLiteral (LString (T.take clamp t)))
    | otherwise -> do
      -- See Note: [Take/Drop Clamping]
      let clamp = fromIntegral $ max (fromIntegral (T.length t) + i) 0
      chargeGasArgs info $ GConcat $ TextConcat $ GasTextLength $ fromIntegral $ negate i
      returnCEKValue cont handler  (VLiteral (LString (T.drop clamp t)))
  [VLiteral (LInteger i), VList li]
    | i >= 0 -> do
      -- See Note: [Take/Drop Clamping]
      let clamp = fromIntegral $ min i (fromIntegral (V.length li))
      chargeGasArgs info $ GConcat $ ListConcat $ GasListLength clamp
      returnCEKValue cont handler  (VList (V.take clamp li))
    | otherwise -> do
      -- See Note: [Take/Drop Clamping]
      let clamp = fromIntegral $ max (fromIntegral (V.length li) + i) 0
      chargeGasArgs info $ GConcat $ ListConcat $ GasListLength $ fromIntegral $ negate i
      returnCEKValue cont handler (VList (V.drop clamp li))
  [VList li, VObject o] -> do
    strings <- traverse (fmap Field . asString info b) (V.toList li)
    chargeGasArgs info $ GConcat $ ObjConcat $ V.length li
    returnCEKValue cont handler $ VObject $ M.restrictKeys o (S.fromList strings)
  args -> argsError info b args

rawDrop :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
rawDrop info b cont handler _env = \case
  [VLiteral (LInteger i), VLiteral (LString t)]
    | i >= 0 -> do
      -- See Note: [Take/Drop Clamping]
      let clamp = fromIntegral $ min i (fromIntegral (T.length t))
      returnCEKValue cont handler  (VLiteral (LString (T.drop clamp t)))
    | otherwise -> do
      -- See Note: [Take/Drop Clamping]
      let clamp = fromIntegral $ max (fromIntegral (T.length t) + i) 0
      returnCEKValue cont handler  (VLiteral (LString (T.take clamp t)))
  [VLiteral (LInteger i), VList li]
    | i >= 0 -> do
      -- See Note: [Take/Drop Clamping]
      let clamp = fromIntegral $ min i (fromIntegral (V.length li))
      returnCEKValue cont handler  (VList (V.drop clamp li))
    | otherwise -> do
      -- See Note: [Take/Drop Clamping]
      let clamp = fromIntegral $ max (fromIntegral (V.length li) + i) 0
      returnCEKValue cont handler (VList (V.take clamp li))
  [VList li, VObject o] -> do
    strings <- traverse (fmap Field . asString info b) (V.toList li)
    returnCEKValue cont handler $ VObject $ M.withoutKeys o (S.fromList strings)
  args -> argsError info b args

rawLength :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
rawLength info b cont handler _env = \case
  [VString t] -> do
    chargeGasArgs info $ GStrOp $ StrOpLength $ T.length t
    returnCEKValue cont handler  (VLiteral (LInteger (fromIntegral (T.length t))))
  [VList li] -> returnCEKValue cont handler (VLiteral (LInteger (fromIntegral (V.length li))))
  [VObject o] ->
    returnCEKValue cont handler $ VInteger $ fromIntegral (M.size o)
  args -> argsError info b args

rawReverse :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
rawReverse info b cont handler _env = \case
  [VList li] -> do
    chargeGasArgs info (GConcat (ListConcat (GasListLength (V.length li))))
    returnCEKValue cont handler (VList (V.reverse li))
  [VLiteral (LString t)] -> do
    chargeGasArgs info (GConcat (TextConcat (GasTextLength (T.length t))))
    returnCEKValue cont handler  (VLiteral (LString (T.reverse t)))
  args -> argsError info b args

coreConcat :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
coreConcat info b cont handler _env = \case
  [VList li]
    | V.null li -> returnCEKValue cont handler (VString mempty)
    | otherwise -> do
    li' <- traverse (asString info b) li
    let totalLen = sum $ T.length <$> li'
    chargeGasArgs info (GConcat (TextListConcat (GasTextLength totalLen) (GasListLength (V.length li))))
    returnCEKValue cont handler (VString (T.concat (V.toList li')))
  args -> argsError info b args

strToList :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
strToList info b cont handler _env = \case
  [VLiteral (LString s)] -> do
    chargeGasArgs info $ GStrOp $ StrOpExplode $ T.length s
    let v = VList (V.fromList (PLiteral . LString . T.singleton <$> T.unpack s))
    returnCEKValue cont handler v
  args -> argsError info b args


zipList :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
zipList info b cont handler _env = \case
  [VClosure clo, VList l, VList r] ->
    case (V.toList l, V.toList r) of
      (x:xs, y:ys) -> do
        chargeGasArgs info (GAConstant unconsWorkNodeGas)
        let cont' = BuiltinC _env info (ZipC clo (xs, ys) []) cont
        applyLam clo [VPactValue x, VPactValue y] cont' handler
      (_, _) -> returnCEKValue cont handler (VList mempty)
  args -> argsError info b args

coreMap :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
coreMap info b cont handler env = \case
  [VClosure clo, VList li] -> case V.toList li of
    x:xs -> do
      let cont' = BuiltinC env info (MapC clo xs []) cont
      chargeGasArgs info (GAConstant unconsWorkNodeGas)
      applyLam clo [VPactValue x] cont' handler
    [] -> returnCEKValue cont handler (VList mempty)
  args -> argsError info b args

coreFilter :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
coreFilter info b cont handler _env = \case
  [VClosure clo, VList li] -> case V.toList li of
    x:xs -> do
      chargeGasArgs info (GAConstant unconsWorkNodeGas)
      let cont' = CondC _env info (FilterC clo x xs []) cont
      applyLam clo [VPactValue x] cont' handler
    [] -> returnCEKValue cont handler (VList mempty)
  args -> argsError info b args

coreFold :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
coreFold info b cont handler _env = \case
  [VClosure clo, VPactValue initElem, VList li] ->
    case V.toList li of
      x:xs -> do
        chargeGasArgs info (GAConstant unconsWorkNodeGas)
        let cont' = BuiltinC _env info (FoldC clo xs) cont
        applyLam clo [VPactValue initElem, VPactValue x] cont' handler
      [] -> returnCEKValue cont handler (VPactValue initElem)
  args -> argsError info b args

coreEnumerate :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
coreEnumerate info b cont handler _env = \case
  [VLiteral (LInteger from), VLiteral (LInteger to)] -> do
    v <- createEnumerateList info from to (if from > to then -1 else 1)
    returnCEKValue cont handler (VList (PLiteral . LInteger <$> v))
  args -> argsError info b args


coreEnumerateStepN :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
coreEnumerateStepN info b cont handler _env = \case
  [VLiteral (LInteger from), VLiteral (LInteger to), VLiteral (LInteger inc)] -> do
    v <- createEnumerateList info from to inc
    returnCEKValue cont handler (VList (PLiteral . LInteger <$> v))
  args -> argsError info b args

makeList :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
makeList info b cont handler _env = \case
  [VLiteral (LInteger i), VPactValue v] -> do
    vSize <- sizeOf info SizeOfV0 v
    chargeGasArgs info (GMakeList (fromIntegral i) vSize)
    returnCEKValue cont handler (VList (V.fromList (replicate (fromIntegral i) v)))
  args -> argsError info b args

coreAccess :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
coreAccess info b cont handler _env = \case
  [VLiteral (LInteger i), VList vec] ->
    case vec V.!? fromIntegral i of
      Just v -> returnCEKValue cont handler (VPactValue v)
      -- Note: this error is not recoverable in prod
      _ -> throwExecutionError info (ArrayOutOfBoundsException (V.length vec) (fromIntegral i))
  [VString field, VObject o] ->
    case M.lookup (Field field) o of
      Just v -> returnCEKValue cont handler (VPactValue v)
      Nothing ->
        -- Note: this error is not recoverable in prod
        throwExecutionError info (ObjectIsMissingField (Field field) (ObjectData o))
  args -> argsError info b args

coreIsCharset :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
coreIsCharset info b cont handler _env = \case
  [VLiteral (LInteger i), VString s] -> do
    chargeGasArgs info $ GStrOp $ StrOpParse $ T.length s
    case i of
      0 -> returnCEKValue cont handler $ VBool $ T.all Char.isAscii s
      1 -> returnCEKValue cont handler $ VBool $ T.all Char.isLatin1 s
      _ ->
        throwNativeExecutionError info b "Unsupported character set"
  args -> argsError info b args

coreYield :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
coreYield info b cont handler _env = \case
  [VObject o] -> go o Nothing
  [VObject o, VString cid] -> go o (Just (ChainId cid))
  args -> argsError info b args
  where
  go o mcid = do
    mpe <- use esDefPactExec
    case mpe of
      Nothing -> throwExecutionError info YieldOutsideDefPact
      Just pe -> case mcid of
        Nothing -> do
          esDefPactExec . _Just . peYield .= Just (Yield o Nothing Nothing)
          returnCEKValue cont handler (VObject o)
        Just cid -> do
          sourceChain <- viewEvalEnv (eePublicData . pdPublicMeta . pmChainId)
          p <- provenanceOf cid
          when (_peStepHasRollback pe) $ throwExecutionError info $ EvalError "Cross-chain yield not allowed in step with rollback"
          esDefPactExec . _Just . peYield .= Just (Yield o (Just p) (Just sourceChain))
          returnCEKValue cont handler (VObject o)
  provenanceOf tid =
    Provenance tid . _mHash <$> getCallingModule info

corePactId :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
corePactId info b cont handler _env = \case
  [] -> use esDefPactExec >>= \case
    Just dpe -> returnCEKValue cont handler (VString (_defPactId (_peDefPactId dpe)))
    Nothing ->
      throwExecutionError info NotInDefPactExecution
  args -> argsError info b args

enforceYield
  :: i
  -> Yield
  -> EvalM e b i ()
enforceYield info y = case _yProvenance y of
  Nothing -> pure ()
  Just p -> do
    m <- getCallingModule info
    cid <- viewEvalEnv $ eePublicData . pdPublicMeta . pmChainId
    let p' = Provenance cid (_mHash m):map (Provenance cid) (toList $ _mBlessed m)
    unless (p `elem` p') $ throwExecutionError info (YieldProvenanceDoesNotMatch p p')

coreResume :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
coreResume info b cont handler _env = \case
  [VClosure clo] -> do
    mps <- viewEvalEnv eeDefPactStep
    case mps of
      Nothing -> throwExecutionError info NoActiveDefPactExec
      Just pactStep -> case _psResume pactStep of
        Nothing -> throwExecutionError info (NoYieldInDefPactStep pactStep)
        Just y@(Yield resumeObj _ _) -> do
          enforceYield info y
          applyLam clo [VObject resumeObj] cont handler
  args -> argsError info b args

-----------------------------------
-- try-related ops
-----------------------------------

enforceTopLevelOnly :: (IsBuiltin b) => i -> b -> EvalM e b i ()
enforceTopLevelOnly info b = do
  s <- use esStack
  unless (null s) $ throwExecutionError info (NativeIsTopLevelOnly (builtinName b))

-----------------------------------
-- Guards and reads
-----------------------------------


-----------------------------------
-- Other Core forms
-----------------------------------

coreB64Encode :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
coreB64Encode info b cont handler _env = \case
  [VLiteral (LString l)] -> do
    chargeGasArgs info $ GStrOp $ StrOpParse $ T.length l
    returnCEKValue cont handler $ VLiteral $ LString $ toB64UrlUnpaddedText $ T.encodeUtf8 l
  args -> argsError info b args


coreB64Decode :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
coreB64Decode info b cont handler _env = \case
  [VLiteral (LString s)] -> do
    chargeGasArgs info $ GStrOp $ StrOpParse $ T.length s
    case fromB64UrlUnpaddedText $ T.encodeUtf8 s of
      Left{} -> throwExecutionError info (DecodeError "invalid b64 encoding")
      Right txt -> returnCEKValue cont handler (VLiteral (LString txt))
  args -> argsError info b args


-- | The implementation of `enforce-guard` native.
coreEnforceGuard :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
coreEnforceGuard info b cont handler env = \case
  [VGuard g] -> enforceGuard info cont handler env g
  [VString s] -> do
    chargeGasArgs info $ GStrOp $ StrOpParse $ T.length s
    case parseAnyKeysetName s of
      Left {} ->
        throwNativeExecutionError info b "incorrect keyset name format"
      Right ksn -> isKeysetNameInSigs info cont handler env ksn
  args -> argsError info b args

keysetRefGuard :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
keysetRefGuard info b cont handler env = \case
  [VString g] -> do
    chargeGasArgs info $ GStrOp $ StrOpParse $ T.length g
    case parseAnyKeysetName g of
      Left {} -> throwNativeExecutionError info b "incorrect keyset name format"
      Right ksn -> do
        let pdb = view cePactDb env
        liftDbFunction info (_pdbRead pdb DKeySets ksn) >>= \case
          Nothing ->
            throwExecutionError info (NoSuchKeySet ksn)
          Just _ -> returnCEKValue cont handler (VGuard (GKeySetRef ksn))
  args -> argsError info b args

coreTypeOf :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
coreTypeOf info b cont handler _env = \case
  [v] -> case v of
    VPactValue pv ->
      returnCEKValue cont handler $ VString $ renderType $ synthesizePvType pv
    VClosure _ -> returnCEKValue cont handler $ VString "<<closure>>"
    VTable tv -> returnCEKValue cont handler $ VString (renderType (TyTable (_tvSchema tv)))
  args -> argsError info b args

coreDec :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
coreDec info b cont handler _env = \case
  [VInteger i] -> returnCEKValue cont handler $ VDecimal $ Decimal 0 i
  args -> argsError info b args

throwReadError
  :: (CEKEval e step b i, IsBuiltin b)
  => i
  -> Cont e step b i
  -> CEKErrorHandler e step b i
  -> b
  -> EvalM e b i (CEKEvalResult e step b i)
throwReadError info cont handler b =
  returnCEKError info cont handler $ EnvReadFunctionFailure (builtinName b)

{-
[Note: Parsed Integer]
`read-integer` corresponds to prod's `ParsedInteger` newtype. That handles, in particular, the following codecs:

instance FromJSON Literal where
  parseJSON n@Number{} = LDecimal <$> decoder decimalCodec n
  parseJSON (String s) = pure $ LString s
  parseJSON (Bool b) = pure $ LBool b
  parseJSON o@Object {} =
    (LInteger <$> decoder integerCodec o) <|>
    (LTime <$> decoder timeCodec o) <|>
    (LDecimal <$> decoder decimalCodec o)
  parseJSON _t = fail "Literal parse failed"

instance A.FromJSON ParsedInteger where
  parseJSON (A.String s) =
    ParsedInteger <$> case pactAttoParseOnly (unPactParser number) s of
                        Right (LInteger i) -> return i
                        _ -> fail $ "Failure parsing integer string: " ++ show s
  parseJSON (A.Number n) = return $ ParsedInteger (round n)
  parseJSON v@A.Object{} = A.parseJSON v >>= \i -> case i of
    PLiteral (LInteger li) -> return $ ParsedInteger li
    _ -> fail $ "Failure parsing integer PactValue object: " ++ show i
  parseJSON v = fail $ "Failure parsing integer: " ++ show v

In prod, env data is just a json object. In core, we parse eagerly using the `PactValue` parser, so the following
can happen:
  - We may see a PString, we must run the number parser `parseNumLiteral`
  - We may see a PDecimal, in which case we round
  - We may see a PInteger, which we read as-is.
-}
coreReadInteger :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
coreReadInteger info b cont handler _env = \case
  [VString s] -> do
    viewEvalEnv eeMsgBody >>= \case
      PObject envData -> do
        chargeGasArgs info $ GObjOp $ ObjOpLookup s $ M.size envData
        case M.lookup (Field s) envData of
          -- See [Note: Parsed Integer]
          Just (PDecimal p) ->
            returnCEKValue cont handler (VInteger (round p))
          Just (PInteger p) ->
            returnCEKValue cont handler (VInteger p)
          -- See [Note: Parsed Integer]
          Just (PString raw) -> do
            chargeGasArgs info $ GStrOp $ StrOpConvToInt $ T.length raw
            case parseNumLiteral raw of
              Just (LInteger i) -> returnCEKValue cont handler (VInteger i)
              _ -> throwReadError info cont handler b
          _ -> throwReadError info cont handler b
      _ -> throwReadError info cont handler b
  args -> argsError info b args

coreReadMsg :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
coreReadMsg info b cont handler _env = \case
  [VString s] -> do
    viewEvalEnv eeMsgBody >>= \case
      PObject envData -> do
        chargeGasArgs info $ GObjOp $ ObjOpLookup s $ M.size envData
        case M.lookup (Field s) envData of
          Just pv -> returnCEKValue cont handler (VPactValue pv)
          _ -> throwReadError info cont handler b
      _ -> throwReadError info cont handler b
  [] -> do
    envData <- viewEvalEnv eeMsgBody
    returnCEKValue cont handler (VPactValue envData)
  args -> argsError info b args

{-
[Note: Parsed Decimal]

Simlar to [Note: Parsed Integer], except the decimal case handles:

instance A.FromJSON ParsedDecimal where
  parseJSON (A.String s) =
    ParsedDecimal <$> case pactAttoParseOnly (unPactParser number) s of
                        Right (LDecimal d) -> return d
                        Right (LInteger i) -> return (fromIntegral i)
                        _ -> fail $ "Failure parsing decimal string: " ++ show s
  parseJSON (A.Number n) = return $ ParsedDecimal (fromRational $ toRational n)
  parseJSON v = fail $ "Failure parsing decimal: " ++ show v

So the string parsing case accepts both the integer, and decimal output
-}
coreReadDecimal :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
coreReadDecimal info b cont handler _env = \case
  [VString s] -> do
    viewEvalEnv eeMsgBody >>= \case
      PObject envData -> do
        chargeGasArgs info $ GObjOp $ ObjOpLookup s $ M.size envData
        case M.lookup (Field s) envData of
          Just (PDecimal p) -> returnCEKValue cont handler (VDecimal p)
          -- See [Note: Parsed Decimal]
          Just (PInteger i) -> returnCEKValue cont handler (VDecimal (Decimal 0 i))
          Just (PString raw) -> do
            chargeGasArgs info $ GStrOp $ StrOpConvToInt $ T.length raw
            case parseNumLiteral raw of
              Just (LInteger i) -> returnCEKValue cont handler (VDecimal (Decimal 0 i))
              Just (LDecimal l) -> returnCEKValue cont handler (VDecimal l)
              _ -> throwReadError info cont handler b
          _ -> throwReadError info cont handler b
      _ -> throwReadError info cont handler b
  args -> argsError info b args


coreReadString :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
coreReadString info b cont handler _env = \case
  [VString s] -> do
    viewEvalEnv eeMsgBody >>= \case
      PObject envData -> do
        chargeGasArgs info $ GObjOp $ ObjOpLookup s $ M.size envData
        case M.lookup (Field s) envData of
          Just (PString p) -> returnCEKValue cont handler (VString p)
          _ -> throwReadError info cont handler b
      _ -> throwReadError info cont handler b
  args -> argsError info b args




coreReadKeyset :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
coreReadKeyset info b cont handler _env = \case
  [VString ksn] ->
    readKeyset' info ksn >>= \case
      Just ks -> do
        shouldEnforce <- isExecutionFlagSet FlagEnforceKeyFormats
        if shouldEnforce && isLeft (enforceKeyFormats (const ()) ks)
           then
            throwExecutionError info (InvalidKeysetFormat ks)
           else returnCEKValue cont handler (VGuard (GKeyset ks))
      Nothing -> throwReadError info cont handler b
  args -> argsError info b args


coreBind :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
coreBind info b cont handler _env = \case
  [v@VObject{}, VClosure clo] ->
    applyLam clo [v] cont handler
  args -> argsError info b args


--------------------------------------------------
-- Db functions
--------------------------------------------------

createTable :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
createTable info b cont handler env = \case
  [VTable tv] -> do
    enforceTopLevelOnly info b
    let cont' = BuiltinC env info (CreateTableC tv) cont
    guardTable info cont' handler env tv GtCreateTable
  args -> argsError info b args

dbSelect :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
dbSelect info b cont handler env = \case
  [VTable tv, VClosure clo] -> do
    let cont' = BuiltinC env info (PreSelectC tv clo Nothing) cont
    guardTable info cont' handler env tv GtSelect
  [VTable tv, VList li, VClosure clo] -> do
    li' <- traverse (fmap Field . asString info b) (V.toList li)
    let cont' = BuiltinC env info (PreSelectC tv clo (Just li')) cont
    guardTable info cont' handler env tv GtSelect
  args -> argsError info b args

foldDb :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
foldDb info b cont handler env = \case
  [VTable tv, VClosure queryClo, VClosure consumer] -> do
    let cont' = BuiltinC env info (PreFoldDbC tv queryClo consumer) cont
    guardTable info cont' handler env tv GtSelect
  args -> argsError info b args

dbRead :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
dbRead info b cont handler env = \case
  [VTable tv, VString k] -> do
    let cont' = BuiltinC env info (ReadC tv (RowKey k)) cont
    guardTable info cont' handler env tv GtRead
  args -> argsError info b args

dbWithRead :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
dbWithRead info b cont handler env = \case
  [VTable tv, VString k, VClosure clo] -> do
    let cont1 = Fn clo env [] [] cont
    let cont2 = BuiltinC env info (ReadC tv (RowKey k)) cont1
    guardTable info cont2 handler env tv GtWithRead
  args -> argsError info b args

dbWithDefaultRead :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
dbWithDefaultRead info b cont handler env = \case
  [VTable tv, VString k, VObject defaultObj, VClosure clo] -> do
    let cont' = BuiltinC env info (WithDefaultReadC tv (RowKey k) (ObjectData defaultObj) clo) cont
    guardTable info cont' handler env tv GtWithDefaultRead
  args -> argsError info b args

-- | Todo: schema checking here? Or only on writes?
dbWrite :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
dbWrite = write' Write

dbInsert :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
dbInsert = write' Insert

write' :: (CEKEval e step b i, IsBuiltin b) => WriteType -> NativeFunction e step b i
write' wt info b cont handler env = \case
  [VTable tv, VString key, VObject o] -> do
    let cont' = BuiltinC env info (WriteC tv wt (RowKey key) (ObjectData o)) cont
    guardTable info cont' handler env tv GtWrite
  args -> argsError info b args

dbUpdate :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
dbUpdate = write' Update

dbKeys :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
dbKeys info b cont handler env = \case
  [VTable tv] -> do
    let cont' = BuiltinC env info (KeysC tv) cont
    guardTable info cont' handler env tv GtKeys
  args -> argsError info b args

defineKeySet'
  :: (CEKEval e step b i)
  => i
  -> Cont e step b i
  -> CEKErrorHandler e step b i
  -> CEKEnv e step b i
  -> T.Text
  -> KeySet
  -> EvalM e b i (CEKEvalResult e step b i)
defineKeySet' info cont handler env ksname newKs  = do
  let pdb = view cePactDb env
  ignoreNamespaces <- not <$> isExecutionFlagSet FlagRequireKeysetNs
  case parseAnyKeysetName ksname of
    Left {} ->
      throwExecutionError info (InvalidKeysetNameFormat ksname)
    Right ksn -> do
      let writeKs = do
            newKsSize <- sizeOf info SizeOfV0 newKs
            chargeGasArgs info (GWrite newKsSize)
            evalWrite info pdb Write DKeySets ksn newKs
            returnCEKValue cont handler (VString "Keyset write success")
      liftDbFunction info (_pdbRead pdb DKeySets ksn) >>= \case
        Just oldKs -> do
          let cont' = BuiltinC env info (DefineKeysetC ksn newKs) cont
          isKeysetInSigs info cont' handler env oldKs
        Nothing | ignoreNamespaces -> writeKs
        Nothing | otherwise -> use (esLoaded . loNamespace) >>= \case
          Nothing ->
            throwExecutionError info CannotDefineKeysetOutsideNamespace
          Just (Namespace ns uGuard _adminGuard) -> do
            when (Just ns /= _keysetNs ksn) $ throwExecutionError info (MismatchingKeysetNamespace ns)
            let cont' = BuiltinC env info (DefineKeysetC ksn newKs) cont
            enforceGuard info cont' handler env uGuard

defineKeySet :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
defineKeySet info b cont handler env = \case
  [VString ksname, VGuard (GKeyset ks)] -> do
    enforceTopLevelOnly info b
    defineKeySet' info cont handler env ksname ks
  [VString ksname] -> do
    enforceTopLevelOnly info b
    readKeyset' info ksname >>= \case
      Just newKs ->
        defineKeySet' info cont handler env ksname newKs
      Nothing -> returnCEKError info cont handler $ EnvReadFunctionFailure  (builtinName b)
  args -> argsError info b args

--------------------------------------------------
-- Capabilities
--------------------------------------------------

requireCapability :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
requireCapability info b cont handler _env = \case
  [VCapToken ct] -> do
    slots <- use $ esCaps . csSlots
    let cnt = sum [1 + length cs | CapSlot _ cs <- slots]
    chargeGasArgs info $ GCapOp $ CapOpRequire cnt
    requireCap info cont handler ct
  args -> argsError info b args


composeCapability :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
composeCapability info b cont handler env = \case
  [VCapToken ct] -> do
    enforceStackTopIsDefcap info b
    composeCap info cont handler env ct
  args -> argsError info b args

installCapability :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
installCapability info b cont handler env = \case
  [VCapToken ct] -> do
    enforceNotWithinDefcap info env "install-capability"
    _ <- installCap info env ct True
    returnCEKValue cont handler (VString "Installed capability")
  args -> argsError info b args

coreEmitEvent :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
coreEmitEvent info b cont handler env = \case
  [VCapToken ct@(CapToken fqn _)] -> do
    let cont' = BuiltinC env info (EmitEventC ct) cont
    guardForModuleCall info cont' handler env (_fqModule fqn) $ do
      -- Todo: this code is repeated in the EmitEventFrame code
      d <- getDefCap info fqn
      enforceMeta (_dcapMeta d)
      emitCapability info ct
      returnCEKValue cont handler (VBool True)
        where
        enforceMeta Unmanaged = throwExecutionError info (InvalidEventCap fqn)
        enforceMeta _ = pure ()
  args -> argsError info b args

createCapGuard :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
createCapGuard info b cont handler _env = \case
  [VCapToken ct] -> do
    let qn = fqnToQualName (_ctName ct)
        cg = CapabilityGuard qn (_ctArgs ct) Nothing
    returnCEKValue cont handler (VGuard (GCapabilityGuard cg))
  args -> argsError info b args

createCapabilityPactGuard :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
createCapabilityPactGuard info b cont handler _env = \case
  [VCapToken ct] -> do
    pid <- getDefPactId info
    let qn = fqnToQualName (_ctName ct)
    let cg = CapabilityGuard qn (_ctArgs ct) (Just pid)
    returnCEKValue cont handler (VGuard (GCapabilityGuard cg))
  args -> argsError info b args

createModuleGuard :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
createModuleGuard info b cont handler _env = \case
  [VString n] ->
    findCallingModule >>= \case
      Just mn ->  do
        let cg = GModuleGuard (ModuleGuard mn n)
        returnCEKValue cont handler (VGuard cg)
      Nothing ->
        throwNativeExecutionError info b "create-module-guard: must call within module"
  args -> argsError info b args

createDefPactGuard :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
createDefPactGuard info b cont handler _env = \case
  [VString name] -> do
    dpid <- getDefPactId info
    returnCEKValue cont handler $ VGuard $ GDefPactGuard $ DefPactGuard dpid name
  args -> argsError info b args


coreIntToStr :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
coreIntToStr info b cont handler _env = \case
  [VInteger base, VInteger v]
    | v < 0 ->
      throwNativeExecutionError info b "int-to-str error: cannot show negative integer"
    | base >= 2 && base <= 16 -> do
      let strLen = 1 + Exts.I# (IntLog.integerLogBase# base $ abs v)
      chargeGasArgs info $ GConcat $ TextConcat $ GasTextLength $ fromIntegral strLen
      let v' = T.pack $ showIntAtBase base Char.intToDigit v ""
      returnCEKValue cont handler (VString v')
    | base == 64 && v >= 0 -> do
      let bsLen = 1 + Exts.I# (IntLog.integerLogBase# 256 $ abs v)
          strLen = (bsLen * 4) `div` 3
      chargeGasArgs info $ GConcat $ TextConcat $ GasTextLength $ fromIntegral strLen
      let v' = toB64UrlUnpaddedText $ integerToBS v
      returnCEKValue cont handler (VString v')
    | base == 64 ->
      throwNativeExecutionError info b "only positive values allowed for base64URL conversion"
    | otherwise ->
      throwNativeExecutionError info b "invalid base for base64URL conversion"
  args -> argsError info b args

coreStrToInt :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
coreStrToInt info b cont handler _env = \case
  [VString s] -> do
    chargeGasArgs info $ GStrOp $ StrOpParse $ T.length s
    checkLen info s
    doBase info cont handler 10 s
  args -> argsError info b args

coreStrToIntBase :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
coreStrToIntBase info b cont handler _env = \case
  [VInteger base, VString s]
    | base == 64 -> do
        chargeGasArgs info $ GStrOp $ StrOpParse $ T.length s
        checkLen info s
        case decodeBase64UrlUnpadded $ T.encodeUtf8 s of
          Left{} -> throwExecutionError info (DecodeError "invalid b64 encoding")
          Right bs -> returnCEKValue cont handler $ VInteger (bsToInteger bs)
    | base >= 2 && base <= 16 -> do
        chargeGasArgs info $ GStrOp $ StrOpParse $ T.length s
        checkLen info s
        doBase info cont handler base s
    | otherwise -> throwNativeExecutionError info b $ "Base value must be >= 2 and <= 16, or 64"
  args -> argsError info b args
  where
  -- Todo: DOS and gas analysis
  bsToInteger :: BS.ByteString -> Integer
  bsToInteger bs = fst $ foldl' go (0,(BS.length bs - 1) * 8) $ BS.unpack bs
  go (i,p) w = (i .|. (shift (fromIntegral w) p), p - 8)

nubByM :: Monad m => (a -> a -> m Bool) -> [a] -> m [a]
nubByM eq = go
  where
  go [] = pure []
  go (x:xs) = do
    xs' <- filterM (fmap not . eq x) xs
    (x :) <$> go xs'

coreDistinct  :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
coreDistinct info b cont handler _env = \case
  [VList s] -> do
    uniques <- nubByM (valEqGassed info) $ V.toList s
    returnCEKValue cont handler
      $ VList
      $ V.fromList uniques
  args -> argsError info b args

coreFormat  :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
coreFormat info b cont handler _env = \case
  [VString s, VList es] -> do
    let parts = T.splitOn "{}" s
        plen = length parts
    if | plen == 1 -> do
          chargeGasArgs info $ GStrOp $ StrOpParse $ T.length s
          returnCEKValue cont handler (VString s)
       | plen - length es > 1 ->
        throwNativeExecutionError info b $ "not enough arguments for template"
       | otherwise -> do
          args <- mapM formatArgM $ V.toList $ V.take (plen - 1) es
          let totalLength = sum (T.length <$> parts) + sum (T.length <$> args)
          chargeGasArgs info $ GConcat $ TextConcat $ GasTextLength totalLength
          returnCEKValue cont handler $ VString $ T.concat $ alternate parts args
    where
    alternate (x:xs) ys = x : alternate ys xs
    alternate _ _ = []

    formatArgM (PString ps) = pure ps
    formatArgM a = renderPactValue info a

  args -> argsError info b args

checkLen
  :: i
  -> T.Text
  -> EvalM e b i ()
checkLen info txt =
  unless (T.length txt <= 512) $
      throwExecutionError info $ DecodeError "Invalid input, only up to 512 length supported"

doBase
  :: (CEKEval e step b i)
  => i
  -> Cont e step b i
  -> CEKErrorHandler e step b i
  -> Integer
  -> T.Text
  -> EvalM e b i (CEKEvalResult e step b i)
doBase info cont handler base txt = case baseStrToInt base txt of
  Left e -> throwExecutionError info (DecodeError e)
  Right n -> returnCEKValue cont handler (VInteger n)

baseStrToInt :: Integer -> T.Text -> Either T.Text Integer
baseStrToInt base t
  | base <= 1 || base > 16 = Left $ "unsupported base: " `T.append` T.pack (show base)
  | T.null t = Left $ "empty text: " `T.append` t
  | T.any (not . Char.isHexDigit) t = Left "invalid digit: supported digits are 0-9, A-F"
  | otherwise = foldM go 0 $ T.unpack t
  where
      go :: Integer -> Char -> Either T.Text Integer
      go acc c'
        = let val = fromIntegral . Char.digitToInt $ c'
          in
            if val < base then
                pure $ base * acc + val
            else
                Left
                  $ "character '"
                      <>
                        T.singleton c'
                          <> "' is out of range for base " <> T.pack (show base) <> ": " <> t

_bsToInteger :: BS.ByteString -> Integer
_bsToInteger bs = fst $ foldl' go (0,(BS.length bs - 1) * 8) $ BS.unpack bs
  where
    go (i,p) w = (i .|. shift (fromIntegral w) p,p - 8)

integerToBS :: Integer -> BS.ByteString
integerToBS v = BS.pack $ reverse $ go v
  where
    go i | i <= 0xff = [fromIntegral i]
         | otherwise = fromIntegral (i .&. 0xff):go (shift i (-8))


coreAndQ :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
coreAndQ info b cont handler env = \case
  [VClosure l, VClosure r, VPactValue v] -> do
    let cont' =  CondC env info (AndQC r v) cont
    applyLam l [VPactValue v] cont' handler
  args -> argsError info b args

coreOrQ :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
coreOrQ info b cont handler env = \case
  [VClosure l, VClosure r, VPactValue v] -> do
    let cont' =  CondC env info (OrQC r v) cont
    applyLam l [VPactValue v] cont' handler
  args -> argsError info b args

coreNotQ :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
coreNotQ info b cont handler env = \case
  [VClosure clo, VPactValue v] -> do
    let cont' = CondC env info NotQC cont
    applyLam clo [VPactValue v] cont' handler
  args -> argsError info b args

coreWhere :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
coreWhere info b cont handler _env = \case
  [VString field, VClosure app, VObject o] -> do
    case M.lookup (Field field) o of
      Just v -> do
        let cont' = EnforceBoolC info cont
        applyLam app [VPactValue v] cont' handler
      Nothing ->
        throwExecutionError info (ObjectIsMissingField (Field field) (ObjectData o))
  args -> argsError info b args

coreHash :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
coreHash = \info b cont handler _env -> \case
  [VString s] ->
    returnCEKValue cont handler (go (T.encodeUtf8 s))
  [VPactValue pv] -> do
    returnCEKValue cont handler (go (encodeStable pv))
  args -> argsError info b args
  where
  go =  VString . hashToText . pactHash

txHash :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
txHash info b cont handler _env = \case
  [] -> do
    h <- viewEvalEnv eeHash
    returnCEKValue cont handler (VString (hashToText h))
  args -> argsError info b args

coreContinue :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
coreContinue info b cont handler _env = \case
  [v] -> do
    returnCEKValue cont handler v
  args -> argsError info b args

parseTime :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
parseTime info b cont handler _env = \case
  [VString fmt, VString s] -> do
    chargeGasArgs info $ GStrOp $ StrOpParseTime (T.length fmt) (T.length s)
    case PactTime.parseTime (T.unpack fmt) (T.unpack s) of
      Just t -> returnCEKValue cont handler $ VPactValue (PTime t)
      Nothing ->
        throwNativeExecutionError info b $ "parse-time parse failure"
  args -> argsError info b args

formatTime :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
formatTime info b cont handler _env = \case
  [VString fmt, VPactValue (PTime t)] -> do
    chargeGasArgs info $ GStrOp $ StrOpFormatTime $ T.length fmt
    let timeString = PactTime.formatTime (T.unpack fmt) t
    returnCEKValue cont handler $ VString (T.pack timeString)
  args -> argsError info b args

time :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
time info b cont handler _env = \case
  [VString s] -> do
    case PactTime.parseTime "%Y-%m-%dT%H:%M:%SZ" (T.unpack s) of
      Just t -> returnCEKValue cont handler $ VPactValue (PTime t)
      Nothing ->
        throwNativeExecutionError info b $ "time default format parse failure"
  args -> argsError info b args

addTime :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
addTime info b cont handler _env = \case
  [VPactValue (PTime t), VPactValue (PDecimal seconds)] -> do
      let newTime = t PactTime..+^ PactTime.fromSeconds seconds
      returnCEKValue cont handler $ VPactValue (PTime newTime)
  [VPactValue (PTime t), VPactValue (PInteger seconds)] -> do
      let newTime = t PactTime..+^ PactTime.fromSeconds (fromIntegral seconds)
      returnCEKValue cont handler $ VPactValue (PTime newTime)
  args -> argsError info b args

diffTime :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
diffTime info b cont handler _env = \case
  [VPactValue (PTime x), VPactValue (PTime y)] -> do
    let secondsDifference = PactTime.toSeconds $ x PactTime..-. y
    returnCEKValue cont handler $ VPactValue $ PDecimal secondsDifference
  args -> argsError info b args

minutes :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
minutes info b cont handler _env = \case
  [VDecimal x] -> do
    let seconds = x * 60
    returnCEKValue cont handler $ VDecimal seconds
  [VInteger x] -> do
    let seconds = fromIntegral x * 60
    returnCEKValue cont handler $ VDecimal seconds
  args -> argsError info b args

hours :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
hours info b cont handler _env = \case
  [VDecimal x] -> do
    let seconds = x * 60 * 60
    returnCEKValue cont handler $ VDecimal seconds
  [VInteger x] -> do
    let seconds = fromIntegral x * 60 * 60
    returnCEKValue cont handler $ VDecimal seconds
  args -> argsError info b args

days :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
days info b cont handler _env = \case
  [VDecimal x] -> do
    let seconds = x * 60 * 60 * 24
    returnCEKValue cont handler $ VDecimal seconds
  [VInteger x] -> do
    let seconds = fromIntegral x * 60 * 60 * 24
    returnCEKValue cont handler $ VDecimal seconds
  args -> argsError info b args

describeModule :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
describeModule info b cont handler env = \case
  [VString s] -> case parseModuleName s of
    Just mname -> do
      enforceTopLevelOnly info b
      checkNonLocalAllowed info b
      getModuleData info (view cePactDb env) mname >>= \case
        ModuleData m _ -> returnCEKValue cont handler $
          VObject $ M.fromList $ fmap (over _1 Field)
            [ ("name", PString (renderModuleName (_mName m)))
            , ("hash", PString (moduleHashToText (_mHash m)))
            , ("tx_hash", PString (hashToText (_mTxHash m)))
            , ("interfaces", PList (PString . renderModuleName <$> V.fromList (_mImplements m)))]
        InterfaceData iface _ -> returnCEKValue cont handler $
          VObject $ M.fromList $ fmap (over _1 Field)
            [ ("name", PString (renderModuleName (_ifName iface)))
            , ("hash", PString (moduleHashToText (_ifHash iface)))
            , ("tx_hash", PString (hashToText (_ifTxHash iface)))
            ]
    Nothing ->
      throwNativeExecutionError info b $ "invalid module name format"
  args -> argsError info b args

dbDescribeTable :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
dbDescribeTable info b cont handler _env = \case
  [VTable (TableValue name _ schema)] -> do
    enforceTopLevelOnly info b
    returnCEKValue cont handler $ VObject $ M.fromList $ fmap (over _1 Field)
      [("name", PString (_tableName name))
      ,("module", PString (renderModuleName (_tableModuleName name)))
      ,("type", PString (renderType (TyTable schema)))]
  args -> argsError info b args

dbDescribeKeySet :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
dbDescribeKeySet info b cont handler env = \case
  [VString s] -> do
    let pdb = _cePactDb env
    enforceTopLevelOnly info b
    case parseAnyKeysetName s of
      Right ksn -> do
        liftDbFunction info (_pdbRead pdb DKeySets ksn) >>= \case
          Just ks ->
            returnCEKValue cont handler (VGuard (GKeyset ks))
          Nothing ->
            throwExecutionError info (NoSuchKeySet ksn)
      Left{} ->
        throwNativeExecutionError info b  "incorrect keyset name format"
  args -> argsError info b args

coreCompose :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
coreCompose info b cont handler env = \case
  [VClosure clo1, VClosure clo2, v] -> do
    let cont' = Fn clo2 env [] [] cont
    applyLam clo1 [v] cont' handler
  args -> argsError info b args

coreCreatePrincipal :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
coreCreatePrincipal info b cont handler _env = \case
  [VGuard g] -> do
    pr <- createPrincipalForGuard info g
    returnCEKValue cont handler $ VString $ Pr.mkPrincipalIdent pr
  args -> argsError info b args

coreIsPrincipal :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
coreIsPrincipal info b cont handler _env = \case
  [VString p] -> do
    chargeGasArgs info $ GStrOp $ StrOpParse $ T.length p
    returnCEKValue cont handler $ VBool $ isRight $ parseOnly Pr.principalParser p
  args -> argsError info b args

coreTypeOfPrincipal :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
coreTypeOfPrincipal info b cont handler _env = \case
  [VString p] -> do
    chargeGasArgs info $ GStrOp $ StrOpParse $ T.length p
    let prty = case parseOnly Pr.principalParser p of
          Left _ -> ""
          Right pr -> Pr.showPrincipalType pr
    returnCEKValue cont handler $ VString prty
  args -> argsError info b args

coreValidatePrincipal :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
coreValidatePrincipal info b cont handler _env = \case
  [VGuard g, VString s] -> do
    pr' <- createPrincipalForGuard info g
    chargeGasArgs info $ GComparison $ TextComparison s
    returnCEKValue cont handler $ VBool $ Pr.mkPrincipalIdent pr' == s
  args -> argsError info b args


coreCond :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
coreCond info b cont handler _env = \case
  [VClosure clo] -> applyLam clo [] cont handler
  args -> argsError info b args


coreIdentity :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
coreIdentity info b cont handler _env = \case
  [VPactValue pv] -> returnCEKValue cont handler $ VPactValue pv
  args -> argsError info b args


--------------------------------------------------
-- Namespace functions
--------------------------------------------------
coreNamespace :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
coreNamespace info b cont handler env = \case
  [VString n] -> do
    enforceTopLevelOnly info b
    let pdb = view cePactDb env
    if T.null n then do
      (esLoaded . loNamespace) .= Nothing
      returnCEKValue cont handler (VString "Namespace reset to root")
    else do
      chargeGasArgs info $ GRead $ fromIntegral $ T.length n
      liftDbFunction info (_pdbRead pdb DNamespaces (NamespaceName n)) >>= \case
        Just ns -> do
          size <- sizeOf info SizeOfV0 ns
          chargeGasArgs info $ GRead size
          (esLoaded . loNamespace) .= Just ns
          let msg = "Namespace set to " <> n
          returnCEKValue cont handler (VString msg)
        Nothing ->
          throwExecutionError info $ NamespaceNotFound (NamespaceName n)
  args -> argsError info b args


coreDefineNamespace :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
coreDefineNamespace info b cont handler env = \case
  [VString n, VGuard usrG, VGuard adminG] -> do
    enforceTopLevelOnly info b
    unless (isValidNsFormat n) $ throwNativeExecutionError info b "invalid namespace format"
    let pdb = view cePactDb env
    let nsn = NamespaceName n
        ns = Namespace nsn usrG adminG
    chargeGasArgs info $ GRead $ fromIntegral $ T.length n
    liftDbFunction info (_pdbRead pdb DNamespaces (NamespaceName n)) >>= \case
      -- G!
      -- https://static.wikia.nocookie.net/onepiece/images/5/52/Lao_G_Manga_Infobox.png/revision/latest?cb=20150405020446
      -- Enforce the old guard
      Just existing@(Namespace _ _ laoG) -> do
        size <- sizeOf info SizeOfV0 existing
        chargeGasArgs info $ GRead size
        let cont' = BuiltinC env info (DefineNamespaceC ns) cont
        enforceGuard info cont' handler env laoG
      Nothing -> viewEvalEnv eeNamespacePolicy >>= \case
        SimpleNamespacePolicy -> do
          nsSize <- sizeOf info SizeOfV0 ns
          chargeGasArgs info (GWrite nsSize)
          evalWrite info pdb Write DNamespaces nsn ns
          returnCEKValue cont handler $ VString $ "Namespace defined: " <> n
        SmartNamespacePolicy _ fun -> getModuleMemberWithHash info pdb fun >>= \case
          (Dfun d, mh) -> do
            clo <- mkDefunClosure d (qualNameToFqn fun mh) env
            let cont' = BuiltinC env info (DefineNamespaceC ns) cont
            applyLam (C clo) [VString n, VGuard adminG] cont' handler
          _ -> throwNativeExecutionError info b $ "Fatal error: namespace manager function is not a defun"
  args -> argsError info b args
  where
  isValidNsFormat nsn = case T.uncons nsn of
    Just (h, tl) ->
      isValidNsHead h && T.all isValidNsChar tl
    Nothing -> False
  isValidNsHead c =
    Char.isLatin1 c && Char.isAlpha c
  isValidNsChar c =
    Char.isLatin1 c && (Char.isAlphaNum c || T.elem c validSpecialChars)
  validSpecialChars :: T.Text
  validSpecialChars =
    "%#+-_&$@<>=^?*!|/~"

coreDescribeNamespace :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
coreDescribeNamespace info b cont handler _env = \case
  [VString n] -> do
    pdb <- viewEvalEnv eePactDb
    chargeGasArgs info $ GRead $ fromIntegral $ T.length n
    liftDbFunction info (_pdbRead pdb DNamespaces (NamespaceName n)) >>= \case
      Just existing@(Namespace _ usrG laoG) -> do
        size <- sizeOf info SizeOfV0 existing
        chargeGasArgs info $ GRead size
        let obj = M.fromList
                  [ (Field "user-guard", PGuard usrG)
                  , (Field "admin-guard", PGuard laoG)
                  , (Field "namespace-name", PString n)]
        returnCEKValue cont handler (VObject obj)
      Nothing ->
        throwExecutionError info $ NamespaceNotFound (NamespaceName n)
  args -> argsError info b args


coreChainData :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
coreChainData info b cont handler _env = \case
  [] -> do
    PublicData publicMeta blockHeight blockTime prevBh <- viewEvalEnv eePublicData
    let (PublicMeta cid sender (GasLimit (Gas gasLimit)) (GasPrice gasPrice) _ttl _creationTime) = publicMeta
    let fields = M.fromList [ (cdChainId, PString (_chainId cid))
                 , (cdBlockHeight, PInteger (fromIntegral blockHeight))
                 , (cdBlockTime, PTime (PactTime.fromPosixTimestampMicros blockTime))
                 , (cdPrevBlockHash, PString prevBh)
                 , (cdSender, PString sender)
                 , (cdGasLimit, PInteger (fromIntegral gasLimit))
                 , (cdGasPrice, PDecimal gasPrice)]
    returnCEKValue cont handler (VObject fields)
  args -> argsError info b args


-- -------------------------
-- ZK defns
-- -------------------------

#ifndef WITHOUT_CRYPTO
ensureOnCurve :: (Num p, Eq p) => i -> CurvePoint p -> p -> EvalM e b i ()
ensureOnCurve info p bp = unless (isOnCurve p bp) $ throwExecutionError info PointNotOnCurve

toG1 :: ObjectData PactValue -> Maybe G1
toG1 (ObjectData obj) = do
  px <- fromIntegral <$> preview (ix (Field "x") . _PLiteral . _LInteger) obj
  py <- fromIntegral <$> preview (ix (Field "y") . _PLiteral . _LInteger) obj
  if px == 0 && py == 0 then pure CurveInf
  else pure (Point px py)

fromG1 :: G1 -> ObjectData PactValue
fromG1 CurveInf = ObjectData pts
  where
  pts = M.fromList
    [ (Field "x", PLiteral (LInteger 0))
    , (Field "y", PLiteral (LInteger 0))]
fromG1 (Point x y) = ObjectData pts
  where
  pts =
    M.fromList
    [ (Field "x", PLiteral (LInteger (fromIntegral x)))
    , (Field "y", PLiteral (LInteger (fromIntegral y)))]

toG2 :: ObjectData PactValue -> Maybe G2
toG2 (ObjectData om) = do
  pxl <- preview (ix (Field "x") . _PList) om
  px <- traverse (preview (_PLiteral . _LInteger . Lens.to fromIntegral)) pxl
  pyl <- preview (ix (Field "y") . _PList) om
  py <- traverse (preview (_PLiteral . _LInteger . Lens.to fromIntegral)) pyl
  let px' = Exts.fromList (V.toList px)
      py' = Exts.fromList (V.toList py)
  if px' == 0 && py' == 0 then pure CurveInf
  else pure (Point px' py')

fromG2 :: G2 -> ObjectData PactValue
fromG2 CurveInf = ObjectData pts
  where
  pts =
    M.fromList
    [ (Field "x", PList (V.fromList [PLiteral (LInteger 0)]))
    , (Field "y", PList (V.fromList [PLiteral (LInteger 0)]))]
fromG2 (Point x y) = ObjectData pts
  where
  toPactPt ext = PList $ PInteger . fromIntegral <$> extElements ext
  x' = toPactPt x
  y' = toPactPt y
  pts =
    M.fromList
    [ (Field "x", x')
    , (Field "y", y')]


zkPairingCheck :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
-- zkPairingCheck :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
zkPairingCheck info b cont handler _env = \case
  args@[VList p1s, VList p2s] -> do
    chargeGasArgs info (GAZKArgs (Pairing (max (V.length p1s) (V.length p2s))))
    g1s <- maybe (argsError info b args) pure (traverse (preview _PObject >=> (toG1 . ObjectData)) p1s)
    g2s <- maybe (argsError info b args) pure (traverse (preview _PObject >=> (toG2 . ObjectData)) p2s)
    traverse_ (\p -> ensureOnCurve info p b1) g1s
    traverse_ (\p -> ensureOnCurve info p b2) g2s
    let pairs = zip (V.toList g1s) (V.toList g2s)
    returnCEKValue cont handler $ VBool $ pairingCheck pairs
  args -> argsError info b args

zkScalarMult :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
zkScalarMult info b cont handler _env = \case
  args@[VString ptTy, VObject p1, VInteger scalar] -> do
    let scalar' = scalar `mod` curveOrder
    case T.toLower ptTy of
      "g1" -> do
        chargeGasArgs info (GAZKArgs (ScalarMult ZKG1))
        p1' <- maybe (argsError info b args) pure $ toG1 (ObjectData p1)
        ensureOnCurve info p1' b1
        let p2' = multiply p1' scalar'
            ObjectData o = fromG1 p2'
        returnCEKValue cont handler (VObject o)
      "g2" -> do
        chargeGasArgs info (GAZKArgs (ScalarMult ZKG2))
        p1' <- maybe (argsError info b args) pure $ toG2 (ObjectData p1)
        ensureOnCurve info p1' b2
        let p2' = multiply p1' scalar'
            ObjectData o = fromG2 p2'
        returnCEKValue cont handler (VObject o)
      _ -> argsError info b args
  args -> argsError info b args
  where
  curveOrder :: Integer
  curveOrder = 21888242871839275222246405745257275088548364400416034343698204186575808495617

zkPointAddition :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
zkPointAddition info b cont handler _env = \case
  args@[VString ptTy, VObject p1, VObject p2] -> do
    case T.toLower ptTy of
      "g1" -> do
        chargeGasArgs info (GAZKArgs (PointAdd ZKG1))
        p1' <- maybe (argsError info b args) pure $ toG1 (ObjectData p1)
        p2' <- maybe (argsError info b args) pure $ toG1 (ObjectData p2)
        ensureOnCurve info p1' b1
        ensureOnCurve info p2' b1
        let p3' = add p1' p2'
            ObjectData o = fromG1 p3'
        returnCEKValue cont handler (VObject o)
      "g2" -> do
        chargeGasArgs info (GAZKArgs (PointAdd ZKG2))
        p1' <- maybe (argsError info b args) pure $ toG2 (ObjectData p1)
        p2' <- maybe (argsError info b args) pure $ toG2 (ObjectData p2)
        ensureOnCurve info p1' b2
        ensureOnCurve info p2' b2
        let p3' = add p1' p2'
            ObjectData o = fromG2 p3'
        returnCEKValue cont handler (VObject o)
      _ -> argsError info b args
  args -> argsError info b args


-----------------------------------
-- Poseidon
-----------------------------------

poseidonHash :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
poseidonHash info b cont handler _env = \case
  [VList as]
    | not (V.null as) && length as <= 8,
    Just intArgs <- traverse (preview (_PLiteral . _LInteger)) as -> do
      chargeGasArgs info (GPoseidonHashHackAChain (length intArgs))
      returnCEKValue cont handler $ VInteger (poseidon (V.toList intArgs))
  args -> argsError info b args

#else

zkPairingCheck :: NativeFunction e step b i
zkPairingCheck info _b _cont _handler _env _args = throwExecutionError info $ EvalError $ "crypto disabled"

zkScalarMult :: NativeFunction e step b i
zkScalarMult info _b _cont _handler _env _args = throwExecutionError info $ EvalError $ "crypto disabled"

zkPointAddition :: NativeFunction e step b i
zkPointAddition info _b _cont _handler _env _args = throwExecutionError info $ EvalError $ "crypto disabled"

poseidonHash :: NativeFunction e step b i
poseidonHash info _b _cont _handler _env _args = throwExecutionError info $ EvalError $ "crypto disabled"

#endif

-----------------------------------
-- SPV
-----------------------------------

coreVerifySPV :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
coreVerifySPV info b cont handler _env = \case
  [VString proofType, VObject o] -> do
    SPVSupport f _ <- viewEvalEnv eeSPVSupport
    liftIO (f proofType (ObjectData o)) >>= \case
      Left err -> throwExecutionError info (SPVVerificationFailure err)
      Right (ObjectData o') -> returnCEKValue cont handler (VObject o')
  args -> argsError info b args

-----------------------------------
-- Verifiers
-----------------------------------
coreEnforceVerifier :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
coreEnforceVerifier info b cont handler _env = \case
  [VString verName] -> do
    enforceStackTopIsDefcap info b
    viewsEvalEnv eeMsgVerifiers (M.lookup (VerifierName verName)) >>= \case
      Just verCaps -> do
        verifierInScope <- anyCapabilityBeingEvaluated verCaps
        if verifierInScope then returnCEKValue cont handler (VBool True)
        else returnCEKError info cont handler $ verifError verName "not in scope"
      Nothing ->
        returnCEKError info cont handler (verifError verName "not in transaction")
  args -> argsError info b args
  where
    verifError verName msg = VerifierFailure (VerifierName verName) msg



coreHyperlaneDecodeTokenMessage :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
coreHyperlaneDecodeTokenMessage info b cont handler _env = \case
  [VString s] -> do
    chargeGasArgs info $ GHyperlaneEncodeDecodeTokenMessage (T.length s)
    case decodeBase64UrlUnpadded (T.encodeUtf8 s) of
      Left _e -> throwExecutionError info $ HyperlaneDecodeError HyperlaneDecodeErrorBase64
      Right bytes -> case Bin.runGetOrFail (unpackTokenMessageERC20 <* eof) (BS.fromStrict bytes) of
        Left (_, _, e) | "TokenMessage" `L.isPrefixOf` e -> do
                           throwExecutionError info $ HyperlaneDecodeError $ HyperlaneDecodeErrorInternal e
        Left _ -> do
          throwExecutionError info $ HyperlaneDecodeError $ HyperlaneDecodeErrorBinary
        Right (_, _, tm) -> case tokenMessageToTerm tm of
          Left e -> throwExecutionError info $ HyperlaneDecodeError e
          Right pv -> returnCEKValue cont handler (VPactValue pv)
  args -> argsError info b args

coreHyperlaneMessageId :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
coreHyperlaneMessageId info b cont handler _env = \case
  [VObject o] -> case decodeHyperlaneMessageObject o of
      Left e -> throwExecutionError info $ HyperlaneError e
      Right r -> do
        let msgId =  getHyperlaneMessageId r
        chargeGasArgs info $ GHyperlaneMessageId (T.length msgId)
        returnCEKValue cont handler (VString msgId)
  args -> argsError info b args

coreHyperlaneEncodeTokenMessage :: (CEKEval e step b i, IsBuiltin b) => NativeFunction e step b i
coreHyperlaneEncodeTokenMessage info b cont handler _env = \case
  [VObject o] -> case decodeHyperlaneTokenMessageObject o of
      Left e -> throwExecutionError info $ HyperlaneError e
      Right r -> do
        let encoded = T.decodeUtf8 $ encodeBase64UrlUnpadded $ BS.toStrict $ Bin.runPut $ Bin.putBuilder $ packTokenMessageERC20 r
        chargeGasArgs info $ GHyperlaneEncodeDecodeTokenMessage (T.length encoded)
        returnCEKValue cont handler (VString encoded)
  args -> argsError info b args

-----------------------------------
-- Builtin exports
-----------------------------------


{-# SPECIALIZE coreBuiltinEnv :: BuiltinEnv ExecRuntime CEKBigStep CoreBuiltin i #-}
{-# SPECIALIZE coreBuiltinEnv :: BuiltinEnv ExecRuntime CEKSmallStep CoreBuiltin i #-}
coreBuiltinEnv
  :: forall e step i. (CEKEval e step CoreBuiltin i)
  => BuiltinEnv e step CoreBuiltin i
coreBuiltinEnv i b env = mkBuiltinFn i b env (coreBuiltinRuntime b)
{-# INLINEABLE coreBuiltinEnv #-}


{-# SPECIALIZE coreBuiltinRuntime
   :: CoreBuiltin
   -> NativeFunction ExecRuntime CEKBigStep CoreBuiltin ()
    #-}
{-# SPECIALIZE coreBuiltinRuntime
   :: CoreBuiltin
   -> NativeFunction ExecRuntime CEKSmallStep CoreBuiltin i
    #-}
coreBuiltinRuntime
  :: (CEKEval e step b i, IsBuiltin b)
  => CoreBuiltin
  -> NativeFunction e step b i
coreBuiltinRuntime = \case
  CoreAdd -> rawAdd
  CoreSub -> rawSub
  CoreMultiply -> rawMul
  CoreDivide -> rawDiv
  CoreNegate -> rawNegate
  CoreAbs -> rawAbs
  CorePow -> rawPow
  CoreNot -> notBool
  CoreEq -> rawEq
  CoreNeq -> rawNeq
  CoreGT -> rawGt
  CoreGEQ -> rawGeq
  CoreLT -> rawLt
  CoreLEQ -> rawLeq
  CoreBitwiseAnd -> bitAndInt
  CoreBitwiseOr -> bitOrInt
  CoreBitwiseXor -> bitXorInt
  CoreBitwiseFlip -> bitComplementInt
  CoreBitShift -> bitShiftInt
  CoreRound -> roundDec
  CoreCeiling -> ceilingDec
  CoreFloor -> floorDec
  CoreRoundPrec -> roundDec
  CoreCeilingPrec -> ceilingDec
  CoreFloorPrec -> floorDec
  CoreExp -> rawExp
  CoreLn -> rawLn
  CoreSqrt -> rawSqrt
  CoreLogBase -> rawLogBase
  CoreLength -> rawLength
  CoreTake -> rawTake
  CoreDrop -> rawDrop
  CoreConcat -> coreConcat
  CoreReverse -> rawReverse
  CoreMod -> modInt
  CoreMap -> coreMap
  CoreFilter -> coreFilter
  CoreZip -> zipList
  CoreIntToStr -> coreIntToStr
  CoreStrToInt -> coreStrToInt
  CoreStrToIntBase -> coreStrToIntBase
  CoreFold -> coreFold
  CoreDistinct -> coreDistinct
  CoreFormat -> coreFormat
  CoreContains -> rawContains
  CoreSort -> rawSort
  CoreSortObject -> rawSortObject
  CoreRemove -> coreRemove
  -- CoreEnforce -> coreEnforce
  -- CoreEnforceOne -> unimplemented
  CoreEnumerate -> coreEnumerate
  CoreEnumerateStepN -> coreEnumerateStepN
  CoreShow -> rawShow
  CoreReadMsg -> coreReadMsg
  CoreReadMsgDefault -> coreReadMsg
  CoreReadInteger -> coreReadInteger
  CoreReadDecimal -> coreReadDecimal
  CoreReadString -> coreReadString
  CoreReadKeyset -> coreReadKeyset
  CoreEnforceGuard -> coreEnforceGuard
  CoreYield -> coreYield
  CoreYieldToChain -> coreYield
  CoreResume -> coreResume
  CoreEnforceKeyset -> coreEnforceGuard
  CoreKeysetRefGuard -> keysetRefGuard
  CoreAt -> coreAccess
  CoreMakeList -> makeList
  CoreB64Encode -> coreB64Encode
  CoreB64Decode -> coreB64Decode
  CoreStrToList -> strToList
  CoreBind -> coreBind
  CoreRequireCapability -> requireCapability
  CoreComposeCapability -> composeCapability
  CoreInstallCapability -> installCapability
  CoreCreateCapabilityGuard -> createCapGuard
  CoreCreateCapabilityPactGuard -> createCapabilityPactGuard
  CoreCreateModuleGuard -> createModuleGuard
  CoreCreateDefPactGuard -> createDefPactGuard
  CoreEmitEvent -> coreEmitEvent
  CoreCreateTable -> createTable
  CoreDescribeKeyset -> dbDescribeKeySet
  CoreDescribeModule -> describeModule
  CoreDescribeTable -> dbDescribeTable
  CoreDefineKeySet -> defineKeySet
  CoreDefineKeysetData -> defineKeySet
  CoreFoldDb -> foldDb
  CoreInsert -> dbInsert
  CoreWrite -> dbWrite
  CoreKeys -> dbKeys
  CoreRead -> dbRead
  CoreSelect -> dbSelect
  CoreUpdate -> dbUpdate
  CoreWithDefaultRead -> dbWithDefaultRead
  CoreWithRead -> dbWithRead
  CoreAndQ -> coreAndQ
  CoreOrQ -> coreOrQ
  CoreWhere -> coreWhere
  CoreNotQ -> coreNotQ
  CoreHash -> coreHash
  CoreTxHash -> txHash
  CoreContinue -> coreContinue
  CoreParseTime -> parseTime
  CoreFormatTime -> formatTime
  CoreTime -> time
  CoreAddTime -> addTime
  CoreDiffTime -> diffTime
  CoreHours -> hours
  CoreMinutes -> minutes
  CoreDays -> days
  CoreCompose -> coreCompose
  CoreSelectWithFields -> dbSelect
  CoreCreatePrincipal -> coreCreatePrincipal
  CoreIsPrincipal -> coreIsPrincipal
  CoreTypeOfPrincipal -> coreTypeOfPrincipal
  CoreValidatePrincipal -> coreValidatePrincipal
  CoreNamespace -> coreNamespace
  CoreDefineNamespace -> coreDefineNamespace
  CoreDescribeNamespace -> coreDescribeNamespace
  CoreZkPairingCheck -> zkPairingCheck
  CoreZKScalarMult -> zkScalarMult
  CoreZkPointAdd -> zkPointAddition
  CorePoseidonHashHackachain -> poseidonHash
  CoreChainData -> coreChainData
  CoreIsCharset -> coreIsCharset
  CorePactId -> corePactId
  CoreTypeOf -> coreTypeOf
  CoreDec -> coreDec
  CoreCond -> coreCond
  CoreIdentity -> coreIdentity
  CoreVerifySPV -> coreVerifySPV
  CoreEnforceVerifier -> coreEnforceVerifier
  CoreHyperlaneMessageId -> coreHyperlaneMessageId
  CoreHyperlaneDecodeMessage -> coreHyperlaneDecodeTokenMessage
  CoreHyperlaneEncodeMessage -> coreHyperlaneEncodeTokenMessage
