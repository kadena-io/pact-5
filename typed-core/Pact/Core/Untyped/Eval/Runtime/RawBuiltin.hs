{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ConstraintKinds #-}

-- |
-- Module      :  Pact.Core.IR.Typecheck
-- Copyright   :  (C) 2022 Kadena
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jose Cardona <jose@kadena.io>
--
-- CEK Evaluator for untyped core using our RawBuiltins (aka untyped, no typechecking)
--

module Pact.Core.Untyped.Eval.Runtime.RawBuiltin where

import Control.Monad(when)

import Data.Bits
import Data.Decimal(roundTo', Decimal)
import Data.Text(Text)
import Data.Vector(Vector)
import Data.List(intersperse)
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Pact.Core.Builtin
import Pact.Core.Literal
import Pact.Core.Errors
import Pact.Core.Hash
import Pact.Core.Names
import Pact.Core.Pretty(pretty)

import Pact.Core.Untyped.Eval.Runtime
import Pact.Core.Untyped.Eval.CEK


----------------------------------------------------------------------
-- Our builtin definitions start here
----------------------------------------------------------------------

-- -- Todo: runtime error
unaryIntFn :: (BuiltinArity b, MonadEval b i m) => (Integer -> Integer) -> b -> NativeFn b i m
unaryIntFn op = mkBuiltinFn \cont handler -> \case
  [VLiteral (LInteger i)] -> returnCEKValue cont handler (VLiteral (LInteger (op i)))
  _ -> failInvariant "unary int function"
{-# INLINE unaryIntFn #-}

unaryDecFn :: (BuiltinArity b, MonadEval b i m) => (Decimal -> Decimal) -> b -> NativeFn b i m
unaryDecFn op = mkBuiltinFn \cont handler -> \case
  [VLiteral (LDecimal i)] -> returnCEKValue cont handler (VLiteral (LDecimal (op i)))
  _ -> failInvariant "unary decimal function"
{-# INLINE unaryDecFn #-}

binaryIntFn
  :: (BuiltinArity b, MonadEval b i m)
  => (Integer -> Integer -> Integer)
  -> b
  -> NativeFn b i m
binaryIntFn op = mkBuiltinFn \cont handler -> \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> returnCEKValue cont handler (VLiteral (LInteger (op i i')))
  _ -> failInvariant "binary int function"
{-# INLINE binaryIntFn #-}

binaryDecFn :: (BuiltinArity b, MonadEval b i m) => (Decimal -> Decimal -> Decimal) -> b -> NativeFn b i m
binaryDecFn op = mkBuiltinFn \cont handler -> \case
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> returnCEKValue cont handler (VLiteral (LDecimal (op i i')))
  _ -> failInvariant "binary decimal function"
{-# INLINE binaryDecFn #-}

binaryBoolFn :: (BuiltinArity b, MonadEval b i m) => (Bool -> Bool -> Bool) -> b -> NativeFn b i m
binaryBoolFn op = mkBuiltinFn \cont handler -> \case
  [VLiteral (LBool l), VLiteral (LBool r)] -> returnCEKValue cont handler (VLiteral (LBool (op l r)))
  _ -> failInvariant "binary bool function"
{-# INLINE binaryBoolFn #-}

compareIntFn :: (BuiltinArity b, MonadEval b i m) => (Integer -> Integer -> Bool) -> b -> NativeFn b i m
compareIntFn op = mkBuiltinFn \cont handler -> \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> returnCEKValue cont handler (VLiteral (LBool (op i i')))
  _ -> failInvariant "int cmp function"
{-# INLINE compareIntFn #-}

compareDecFn :: (BuiltinArity b, MonadEval b i m) => (Decimal -> Decimal -> Bool) -> b -> NativeFn b i m
compareDecFn op = mkBuiltinFn \cont handler -> \case
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> returnCEKValue cont handler (VLiteral (LBool (op i i')))
  _ -> failInvariant "dec cmp function"
{-# INLINE compareDecFn #-}

compareStrFn :: (BuiltinArity b, MonadEval b i m) => (Text -> Text -> Bool) -> b -> NativeFn b i m
compareStrFn op = mkBuiltinFn \cont handler -> \case
  [VLiteral (LString i), VLiteral (LString i')] -> returnCEKValue cont handler (VLiteral (LBool (op i i')))
  _ -> failInvariant "str cmp function"
{-# INLINE compareStrFn #-}

roundingFn :: (BuiltinArity b, MonadEval b i m) => (Rational -> Integer) -> b -> NativeFn b i m
roundingFn op = mkBuiltinFn \cont handler -> \case
  [VLiteral (LDecimal i)] -> returnCEKValue cont handler (VLiteral (LInteger (truncate (roundTo' op 0 i))))
  _ -> failInvariant "rounding function"
{-# INLINE roundingFn #-}

---------------------------------
-- Arithmetic Ops
------------------------------
rawAdd :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
rawAdd = mkBuiltinFn \cont handler -> \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> returnCEKValue cont handler (VLiteral (LInteger (i + i')))
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> returnCEKValue cont handler (VLiteral (LDecimal (i + i')))
  [VLiteral (LString i), VLiteral (LString i')] ->
    returnCEKValue cont handler  (VLiteral (LString (i <> i')))
  [VList l, VList r] -> returnCEKValue cont handler (VList (l <> r))
  _ -> failInvariant "add"

rawSub :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
rawSub = mkBuiltinFn \cont handler -> \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> returnCEKValue cont handler (VLiteral (LInteger (i - i')))
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> returnCEKValue cont handler (VLiteral (LDecimal (i - i')))
  _ -> failInvariant "subtract"

rawMul :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
rawMul =  mkBuiltinFn \cont handler -> \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> returnCEKValue cont handler (VLiteral (LInteger (i * i')))
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> returnCEKValue cont handler (VLiteral (LDecimal (i * i')))
  _ -> failInvariant "multiply"

rawPow :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
rawPow = mkBuiltinFn \cont handler -> \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> do
    when (i' < 0) $ throwExecutionError' (ArithmeticException "negative exponent in integer power")
    returnCEKValue cont handler (VLiteral (LInteger (i ^ i')))
  [VLiteral (LDecimal a), VLiteral (LDecimal b)] -> do
    let result = dec2F a ** dec2F b
    guardNanOrInf result
    returnCEKValue cont handler (VLiteral (LDecimal (f2Dec result)))
  _ -> failInvariant "pow"

rawLogBase :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
rawLogBase = mkBuiltinFn \cont handler -> \case
  [VLiteral (LInteger base), VLiteral (LInteger n)] -> do
    when (base < 0 || n <= 0) $ throwExecutionError' (ArithmeticException "Illegal log base")
    let base' = fromIntegral base :: Double
        n' = fromIntegral n
        out = round (logBase base' n')
    returnCEKValue cont handler (VLiteral (LInteger out))
    -- if i' == 0 then throwExecutionError' (ArithmeticException "div by zero")
    -- else returnCEKValue cont handler (VLiteral (LInteger (div i i')))
  [VLiteral (LDecimal base), VLiteral (LDecimal arg)] -> do
    when (base < 0 || arg <= 0) $ throwExecutionError' (ArithmeticException "Invalid base or argument in log")
    let result = logBase (dec2F base) (dec2F arg)
    guardNanOrInf result
    returnCEKValue cont handler (VLiteral (LDecimal (f2Dec result)))
  _ -> failInvariant "logBase"

rawDiv :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
rawDiv = mkBuiltinFn \cont handler -> \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] ->
    if i' == 0 then throwExecutionError' (ArithmeticException "div by zero")
    else returnCEKValue cont handler (VLiteral (LInteger (div i i')))
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] ->
    if i' == 0 then throwExecutionError' (ArithmeticException "div by zero, decimal")
    else returnCEKValue cont handler (VLiteral (LDecimal (i / i')))
  _ -> failInvariant "div"

rawNegate :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
rawNegate = mkBuiltinFn \cont handler -> \case
  [VLiteral (LInteger i)] ->
    returnCEKValue cont handler (VLiteral (LInteger (negate i)))
  [VLiteral (LDecimal i)] ->
    returnCEKValue cont handler (VLiteral (LDecimal (negate i)))
  _ -> failInvariant "negate"

rawMod :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
rawMod = binaryIntFn mod

rawEq :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
rawEq = mkBuiltinFn \cont handler -> \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> returnCEKValue cont handler (VLiteral (LBool (i == i')))
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> returnCEKValue cont handler (VLiteral (LBool (i == i')))
  [VLiteral (LString i), VLiteral (LString i')] -> returnCEKValue cont handler (VLiteral (LBool (i == i')))
  [VLiteral (LBool i), VLiteral (LBool i')] -> returnCEKValue cont handler (VLiteral (LBool (i == i')))
  [VLiteral LUnit, VLiteral LUnit] -> returnCEKValue cont handler (VLiteral (LBool True))
  [VList l, VList r] ->
    if V.length l /= V.length r then
      returnCEKValue cont handler (VLiteral (LBool False))
    else returnCEKValue cont handler (VBool (valueEq (VList l) (VList r)))
  _ -> failInvariant "eq"

modInt :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
modInt = binaryIntFn mod

valueEq :: CEKValue b i m -> CEKValue b i m -> Bool
valueEq (VInteger i) (VInteger r) = i == r
valueEq (VDecimal l) (VDecimal r) = l == r
valueEq (VString l) (VString r) = l == r
valueEq VUnit VUnit = True
valueEq (VBool l) (VBool r) = l == r
valueEq (VList l) (VList r) =
  V.length l == V.length r &&  all (uncurry valueEq) (V.zip l r)
valueEq _ _ = False

prettyShowValue :: CEKValue b i m -> Text
prettyShowValue = \case
  VLiteral lit -> T.pack (show (pretty lit))
  VList vec ->
    "[" <> T.concat (intersperse ", " (prettyShowValue <$> V.toList vec)) <> "]"
  VClosure _ _ -> "<#closure>"
  VNative _ -> "<#nativefn>"
  VGuard _ -> "<#guard>"
  VModRef mn _ -> "modRef{" <> (_mnName mn) <> "}"

rawNeq :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
rawNeq = mkBuiltinFn \cont handler -> \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> returnCEKValue cont handler (VLiteral (LBool (i /= i')))
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> returnCEKValue cont handler (VLiteral (LBool (i /= i')))
  [VLiteral (LString i), VLiteral (LString i')] -> returnCEKValue cont handler (VLiteral (LBool (i /= i')))
  [VLiteral (LBool i), VLiteral (LBool i')] -> returnCEKValue cont handler (VLiteral (LBool (i /= i')))
  [VLiteral LUnit, VLiteral LUnit] -> returnCEKValue cont handler (VLiteral (LBool False))
  [VList l, VList r] ->
    if V.length l /= V.length r then
      returnCEKValue cont handler (VLiteral (LBool True))
    else returnCEKValue cont handler (VBool (not (valueEq (VList l) (VList r))))
  _ -> failInvariant "neq"

rawGt :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
rawGt = mkBuiltinFn \cont handler -> \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> returnCEKValue cont handler (VLiteral (LBool (i > i')))
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> returnCEKValue cont handler (VLiteral (LBool (i > i')))
  [VLiteral (LString i), VLiteral (LString i')] -> returnCEKValue cont handler (VLiteral (LBool (i > i')))
  _ -> failInvariant "int cmp function"

rawLt :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
rawLt = mkBuiltinFn \cont handler -> \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> returnCEKValue cont handler (VLiteral (LBool (i < i')))
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> returnCEKValue cont handler (VLiteral (LBool (i < i')))
  [VLiteral (LString i), VLiteral (LString i')] -> returnCEKValue cont handler (VLiteral (LBool (i < i')))
  _ -> failInvariant "int cmp function"

rawGeq :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
rawGeq = mkBuiltinFn \cont handler -> \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> returnCEKValue cont handler (VLiteral (LBool (i >= i')))
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> returnCEKValue cont handler (VLiteral (LBool (i >= i')))
  [VLiteral (LString i), VLiteral (LString i')] -> returnCEKValue cont handler (VLiteral (LBool (i >= i')))
  _ -> failInvariant "int cmp function"

rawLeq :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
rawLeq = mkBuiltinFn \cont handler -> \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> returnCEKValue cont handler (VLiteral (LBool (i <= i')))
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> returnCEKValue cont handler (VLiteral (LBool (i <= i')))
  [VLiteral (LString i), VLiteral (LString i')] -> returnCEKValue cont handler (VLiteral (LBool (i <= i')))
  _ -> failInvariant "int cmp function"

bitAndInt :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
bitAndInt = binaryIntFn (.&.)

bitOrInt :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
bitOrInt = binaryIntFn (.|.)

bitComplementInt :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
bitComplementInt = unaryIntFn complement

bitXorInt :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
bitXorInt = binaryIntFn xor

bitShiftInt :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
bitShiftInt =  binaryIntFn (\i s -> shift i (fromIntegral s))

rawAbs :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
rawAbs = mkBuiltinFn \cont handler -> \case
  [VLiteral (LInteger i)] ->
    returnCEKValue cont handler (VLiteral (LInteger (abs i)))
  [VLiteral (LDecimal e)] -> do
    returnCEKValue cont handler (VLiteral (LDecimal (abs e)))
  _ -> failInvariant "abs"

rawExp :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
rawExp = mkBuiltinFn \cont handler -> \case
  [VLiteral (LInteger i)] -> do
    let result = exp (fromIntegral i)
    guardNanOrInf result
    returnCEKValue cont handler (VLiteral (LDecimal (f2Dec result)))
  [VLiteral (LDecimal e)] -> do
    let result = exp (dec2F e)
    guardNanOrInf result
    returnCEKValue cont handler (VLiteral (LDecimal (f2Dec result)))
  _ -> failInvariant "exe"

rawLn :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
rawLn = mkBuiltinFn \cont handler -> \case
  [VLiteral (LInteger i)] -> do
    let result = log (fromIntegral i)
    guardNanOrInf result
    returnCEKValue cont handler (VLiteral (LDecimal (f2Dec result)))
  [VLiteral (LDecimal e)] -> do
    let result = log (dec2F e)
    guardNanOrInf result
    returnCEKValue cont handler (VLiteral (LDecimal (f2Dec result)))
  _ -> failInvariant "lnInt"

rawSqrt :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
rawSqrt = mkBuiltinFn \cont handler -> \case
  [VLiteral (LInteger i)] -> do
    when (i < 0) $ throwExecutionError' (ArithmeticException "Square root must be non-negative")
    let result = sqrt (fromIntegral i)
    guardNanOrInf result
    returnCEKValue cont handler (VLiteral (LDecimal (f2Dec result)))
  [VLiteral (LDecimal e)] -> do
    when (e < 0) $ throwExecutionError' (ArithmeticException "Square root must be non-negative")
    let result = sqrt (dec2F e)
    guardNanOrInf result
    returnCEKValue cont handler (VLiteral (LDecimal (f2Dec result)))
  _ -> failInvariant "sqrtInt"

-- Todo: fix all show instances
rawShow :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
rawShow = mkBuiltinFn \cont handler -> \case
  [VLiteral (LInteger i)] ->
    returnCEKValue cont handler (VLiteral (LString (T.pack (show i))))
  [VLiteral (LDecimal i)] ->
    returnCEKValue cont handler (VLiteral (LString (T.pack (show i))))
  [VLiteral (LString i)] ->
    returnCEKValue cont handler (VLiteral (LString (T.pack (show i))))
  [VLiteral (LBool i)] ->
    returnCEKValue cont handler (VLiteral (LString (T.pack (show i))))
  [VLiteral LUnit] ->
    returnCEKValue cont handler (VLiteral (LString "()"))
  _ -> failInvariant "showInt"

-- -------------------------
-- double ops
-- -------------------------

guardNanOrInf :: MonadEval b i m => Double -> m ()
guardNanOrInf a =
  when (isNaN a || isInfinite a) $ throwExecutionError' (FloatingPointError "Floating operation resulted in Infinity or NaN")

dec2F :: Decimal -> Double
dec2F = fromRational . toRational

f2Dec :: Double -> Decimal
f2Dec = fromRational . toRational

roundDec :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
roundDec = roundingFn round

floorDec :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
floorDec = roundingFn floor

ceilingDec :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
ceilingDec = roundingFn ceiling

-- Todo: exp and ln, sqrt have similar failure conditions
-- expDec :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
-- expDec = mkBuiltinFn \cont handler -> \case
--   [VLiteral (LDecimal e)] -> do
--     let result = exp (dec2F e)
--     guardNanOrInf result
--     returnCEKValue cont handler (VLiteral (LDecimal (f2Dec result)))
--   _ -> failInvariant "binary decimal function"
--   -- unaryDecFn (f2Dec . exp . dec2F)

-- lnDec :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
-- lnDec = mkBuiltinFn \cont handler -> \case
--   [VLiteral (LDecimal e)] -> do
--     let result = log (dec2F e)
--     guardNanOrInf result
--     returnCEKValue cont handler (VLiteral (LDecimal (f2Dec result)))
--   _ -> failInvariant "binary decimal function"

-- logBaseDec :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
-- logBaseDec = mkBuiltinFn \cont handler -> \case
--   [VLiteral (LDecimal base), VLiteral (LDecimal arg)] -> do
--     when (base < 0 || arg <= 0) $ throwExecutionError' (ArithmeticException "Invalid base or argument in log")
--     let result = logBase (dec2F base) (dec2F arg)
--     guardNanOrInf result
--     returnCEKValue cont handler (VLiteral (LDecimal (f2Dec result)))
--   _ -> failInvariant "binary decimal function"


-- sqrtDec :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
-- sqrtDec = mkBuiltinFn \cont handler -> \case
--   [VLiteral (LDecimal e)] -> do
--     when (e < 0) $ throwExecutionError' (ArithmeticException "Square root must be non-negative")
--     let result = sqrt (dec2F e)
--     guardNanOrInf result
--     returnCEKValue cont handler (VLiteral (LDecimal (f2Dec result)))
--   _ -> failInvariant "binary decimal function"


---------------------------
-- bool ops
---------------------------
andBool :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
andBool = binaryBoolFn (&&)

orBool :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
orBool = binaryBoolFn (||)

notBool :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
notBool = mkBuiltinFn \cont handler -> \case
  [VLiteral (LBool i)] -> returnCEKValue cont handler  (VLiteral (LBool (not i)))
  _ -> failInvariant "notBool"

-- eqBool :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
-- eqBool = binaryBoolFn (==)

-- neqBool :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
-- neqBool = binaryBoolFn (/=)

-- showBool :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
-- showBool = mkBuiltinFn \cont handler -> \case
--   [VLiteral (LBool i)] -> do
--     let out = if i then "true" else "false"
--     returnCEKValue cont handler (VLiteral (LString out))
--   _ -> failInvariant "showBool"

---------------------------
-- string ops
---------------------------
-- eqStr :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
-- eqStr = compareStrFn (==)

-- neqStr :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
-- neqStr = compareStrFn (/=)

-- gtStr :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
-- gtStr = compareStrFn (>)

-- geqStr :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
-- geqStr = compareStrFn (>=)

-- ltStr :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
-- ltStr = compareStrFn (<)

-- leqStr :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
-- leqStr = compareStrFn (<=)

-- addStr :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
-- addStr =  mkBuiltinFn \cont handler -> \case
--   [VLiteral (LString i), VLiteral (LString i')] ->
--     returnCEKValue cont handler  (VLiteral (LString (i <> i')))
--   _ -> failInvariant "addStr"

rawTake :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
rawTake = mkBuiltinFn \cont handler -> \case
  [VLiteral (LInteger i), VLiteral (LString t)]
    | i >= 0 -> do
      let clamp = min (fromIntegral i) (T.length t)
      returnCEKValue cont handler  (VLiteral (LString (T.take clamp t)))
    | otherwise -> do
      let clamp = min (abs (T.length t + fromIntegral i)) (T.length t)
      returnCEKValue cont handler  (VLiteral (LString (T.drop clamp t)))
  [VLiteral (LInteger i), VList li]
    | i >= 0 -> do
      let clamp = fromIntegral $ min i (fromIntegral (V.length li))
      returnCEKValue cont handler  (VList (V.take clamp li))
    | otherwise -> do
      let clamp = fromIntegral $ max (fromIntegral (V.length li) + i) 0
      returnCEKValue cont handler (VList (V.drop clamp li))
  _ -> failInvariant "takeStr"

rawDrop :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
rawDrop = mkBuiltinFn \cont handler -> \case
  [VLiteral (LInteger i), VLiteral (LString t)]
    | i >= 0 -> do
      let clamp = min (fromIntegral i) (T.length t)
      returnCEKValue cont handler  (VLiteral (LString (T.drop clamp t)))
    | otherwise -> do
      let clamp = min (abs (T.length t + fromIntegral i)) (T.length t)
      returnCEKValue cont handler  (VLiteral (LString (T.take clamp t)))
  [VLiteral (LInteger i), VList li]
    | i >= 0 -> do
      let clamp = fromIntegral $ min i (fromIntegral (V.length li))
      returnCEKValue cont handler  (VList (V.drop clamp li))
    | otherwise -> do
      let clamp = fromIntegral $ max (fromIntegral (V.length li) + i) 0
      returnCEKValue cont handler (VList (V.take clamp li))
  _ -> failInvariant "dropStr"

rawLength :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
rawLength = mkBuiltinFn \cont handler -> \case
  [VLiteral (LString t)] -> do
    returnCEKValue cont handler  (VLiteral (LInteger (fromIntegral (T.length t))))
  [VList li] -> returnCEKValue cont handler (VLiteral (LInteger (fromIntegral (V.length li))))
  _ -> failInvariant "lengthStr"

rawReverse :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
rawReverse = mkBuiltinFn \cont handler -> \case
  [VList li] ->
    returnCEKValue cont handler (VList (V.reverse li))
  [VLiteral (LString t)] -> do
    returnCEKValue cont handler  (VLiteral (LString (T.reverse t)))
  _ -> failInvariant "reverseStr"

-- showStr :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
-- showStr = mkBuiltinFn \cont handler -> \case
--   [VLiteral (LString t)] -> do
--     let out = "\"" <> t <> "\""
--     returnCEKValue cont handler  (VLiteral (LString out))
--   _ -> failInvariant "showStr"

concatStr :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
concatStr = mkBuiltinFn \cont handler -> \case
  [VList li] -> do
    li' <- traverse asString li
    returnCEKValue cont handler (VLiteral (LString (T.concat (V.toList li'))))
  _ -> failInvariant "concatStr"

strToList :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
strToList = mkBuiltinFn \cont handler -> \case
  [VLiteral (LString s)] -> do
    let v = VList (V.fromList (VLiteral . LString . T.singleton <$> T.unpack s))
    returnCEKValue cont handler v
  _ -> failInvariant "concatStr"

---------------------------
-- Unit ops
---------------------------

-- eqUnit :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
-- eqUnit = mkBuiltinFn \cont handler -> \case
--   [VLiteral LUnit, VLiteral LUnit] -> returnCEKValue cont handler (VLiteral (LBool True))
--   _ -> failInvariant "eqUnit"

-- neqUnit :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
-- neqUnit = mkBuiltinFn \cont handler -> \case
--   [VLiteral LUnit, VLiteral LUnit] -> returnCEKValue cont handler (VLiteral (LBool False))
--   _ -> failInvariant "neqUnit"

-- showUnit :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
-- showUnit = mkBuiltinFn \cont handler -> \case
--   [VLiteral LUnit] -> returnCEKValue cont handler (VLiteral (LString "()"))
--   _ -> failInvariant "showUnit"

---------------------------
-- Object ops
---------------------------

-- eqObj :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
-- eqObj = mkBuiltinFn \case
--   [l@VObject{}, r@VObject{}] -> pure (VLiteral (LBool (unsafeEqCEKValue l r)))
--   _ -> failInvariant "eqObj"

-- neqObj :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
-- neqObj = mkBuiltinFn \case
--   [l@VObject{}, r@VObject{}] -> pure (VLiteral (LBool (unsafeNeqCEKValue l r)))
--   _ -> failInvariant "neqObj"


------------------------------
--- conversions + unsafe ops
------------------------------
-- asBool :: MonadEval b i m => CEKValue b i m -> m Bool
-- asBool (VLiteral (LBool b)) = pure b
-- asBool _ = failInvariant "asBool"

asString :: MonadEval b i m => CEKValue b i m -> m Text
asString (VLiteral (LString b)) = pure b
asString _ = failInvariant "asString"

asList :: MonadEval b i m => CEKValue b i m -> m (Vector (CEKValue b i m))
asList (VList l) = pure l
asList _ = failInvariant "asList"

-- unsafeEqLiteral :: Literal -> Literal -> Bool
-- unsafeEqLiteral (LString i) (LString i') = i == i'
-- unsafeEqLiteral (LInteger i) (LInteger i') = i == i'
-- unsafeEqLiteral (LDecimal i) (LDecimal i') = i == i'
-- unsafeEqLiteral LUnit LUnit = True
-- unsafeEqLiteral (LBool i) (LBool i') = i == i'
-- unsafeEqLiteral (LTime i) (LTime i') = i == i'
-- unsafeEqLiteral _ _ =
--   throw (InvariantFailure "invariant failed in literal EQ")

-- unsafeNeqLiteral :: Literal -> Literal -> Bool
-- unsafeNeqLiteral a b = not (unsafeEqLiteral a b)

-- unsafeEqCEKValue :: CEKValue b i m -> CEKValue b i m -> Bool
-- unsafeEqCEKValue (VLiteral l) (VLiteral l') = unsafeEqLiteral l l'
-- unsafeEqCEKValue (VObject o) (VObject o') = and (Map.intersectionWith unsafeEqCEKValue o o')
-- unsafeEqCEKValue (VList l) (VList l') =  V.length l == V.length l' &&  and (V.zipWith unsafeEqCEKValue l l')
-- unsafeEqCEKValue _ _ = throw (InvariantFailure "invariant failed in value Eq")

-- unsafeNeqCEKValue :: CEKValue b i m -> CEKValue b i m -> Bool
-- unsafeNeqCEKValue a b = not (unsafeEqCEKValue a b)

---------------------------
-- list ops
---------------------------


-- neqList :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
-- neqList = mkBuiltinFn \cont handler -> \case
--   [neqClo, VList l, VList r] ->
--     if V.length l /= V.length r then
--       returnCEKValue cont handler (VLiteral (LBool True))
--     else zip' (V.toList l) (V.toList r) []
--     where
--     zip' (x:xs) (y:ys) acc = unsafeApplyTwo neqClo x y >>= \case
--        EvalValue (VLiteral (LBool b)) -> zip' xs ys (b:acc)
--        v@VError{} -> returnCEK cont handler v
--        _ -> failInvariant "applying closure in list eq yielded incorrect type"
--     zip' _ _ acc = returnCEKValue cont handler (VLiteral (LBool (or acc)))
--   _ -> failInvariant "neqList"

zipList :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
zipList = mkBuiltinFn \cont handler -> \case
  [clo, VList l, VList r] -> zip' (V.toList l) (V.toList r) []
    where
    zip' (x:xs) (y:ys) acc = unsafeApplyTwo clo x y >>= \case
       EvalValue v -> zip' xs ys (v:acc)
       v@VError{} -> returnCEK cont handler v
    zip' _ _ acc = returnCEKValue cont handler (VList (V.fromList (reverse acc)))
  _ -> failInvariant "zipList"

-- addList :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
-- addList = mkBuiltinFn \cont handler -> \case
--   [VList l, VList r] -> returnCEKValue cont handler (VList (l <> r))
--   _ -> failInvariant "addList"

-- pcShowList :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
-- pcShowList = mkBuiltinFn \cont handler -> \case
--   [showFn, VList l1] -> show' (V.toList l1) []
--     where
--     show' (x:xs) acc = unsafeApplyOne showFn x >>= \case
--        EvalValue (VLiteral (LString b)) -> show' xs (b:acc)
--        v@VError{} -> returnCEK cont handler v
--        _ -> failInvariant "applying closure in list eq yielded incorrect type"
--     show' _ acc = do
--       let out = "[" <> T.intercalate ", " (reverse acc) <> "]"
--       returnCEKValue cont handler (VLiteral (LString out))
--   _ -> failInvariant "showList"

coreMap :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
coreMap = mkBuiltinFn \cont handler -> \case
  [fn, VList li] -> map' (V.toList li) []
    where
    map' (x:xs) acc = unsafeApplyOne fn x >>= \case
       EvalValue cv -> map' xs (cv:acc)
       v -> returnCEK cont handler v
    map' _ acc = returnCEKValue cont handler (VList (V.fromList (reverse acc)))
  _ -> failInvariant "map"

coreFilter :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
coreFilter = mkBuiltinFn \cont handler -> \case
  [fn, VList li] -> filter' (V.toList li) []
    where
    filter' (x:xs) acc = unsafeApplyOne fn x >>= \case
      EvalValue (VLiteral (LBool b)) ->
        if b then filter' xs (x:acc) else filter' xs acc
      v@VError{} ->
        returnCEK cont handler v
      _ -> failInvariant "filter"
    filter' [] acc = returnCEKValue cont handler (VList (V.fromList (reverse acc)))
  _ -> failInvariant "filter"

coreFold :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
coreFold = mkBuiltinFn \cont handler -> \case
  [fn, initElem, VList li] ->
    fold' initElem (V.toList li)
    where
    fold' e (x:xs) = unsafeApplyTwo fn e x >>= \case
      EvalValue v -> fold' v xs
      v -> returnCEK cont handler v
    fold' e [] = returnCEKValue cont handler e
  _ -> failInvariant "fold"

lengthList :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
lengthList = mkBuiltinFn \cont handler -> \case
  [VList li] -> returnCEKValue cont handler (VLiteral (LInteger (fromIntegral (V.length li))))
  _ -> failInvariant "lengthList"

-- takeList :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
-- takeList = mkBuiltinFn \cont handler -> \case
--   [VLiteral (LInteger i), VList li]
--     | i >= 0 -> do
--       let clamp = fromIntegral $ min i (fromIntegral (V.length li))
--       returnCEKValue cont handler  (VList (V.take clamp li))
--     | otherwise -> do
--       let clamp = fromIntegral $ max (fromIntegral (V.length li) + i) 0
--       returnCEKValue cont handler (VList (V.drop clamp li))
--   _ -> failInvariant "takeList"

-- dropList :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
-- dropList = mkBuiltinFn \cont handler -> \case
--   [VLiteral (LInteger i), VList li]
--     | i >= 0 -> do
--       let clamp = fromIntegral $ min i (fromIntegral (V.length li))
--       returnCEKValue cont handler  (VList (V.drop clamp li))
--     | otherwise -> do
--       let clamp = fromIntegral $ max (fromIntegral (V.length li) + i) 0
--       returnCEKValue cont handler (VList (V.take clamp li))
--   _ -> failInvariant "dropList"

-- reverseList :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
-- reverseList = mkBuiltinFn \cont handler -> \case
--   [VList li] ->
--     returnCEKValue cont handler (VList (V.reverse li))
--   _ -> failInvariant "takeList"

coreEnumerate :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
coreEnumerate = mkBuiltinFn \cont handler -> \case
  [VLiteral (LInteger from), VLiteral (LInteger to)] -> do
    v <- createEnumerateList from to (if from > to then -1 else 1)
    returnCEKValue cont handler (VList (VLiteral . LInteger <$> v))
  _ -> failInvariant "enumerate"

createEnumerateList
  :: (MonadEval b i m)
  => Integer
  -- ^ from
  -> Integer
  -- ^ to
  -> Integer
  -- ^ Step
  -> m (Vector Integer)
createEnumerateList from to inc
  | from == to = pure (V.singleton from)
  | inc == 0 = pure mempty
  | from < to, from + inc < from =
    throwExecutionError' (EnumerationError "enumerate: increment diverges below from interval bounds.")
  | from > to, from + inc > from =
    throwExecutionError' (EnumerationError "enumerate: increment diverges above from interval bounds.")
  | otherwise = let
    step = succ (abs (from - to) `div` abs inc)
    in pure $ V.enumFromStepN from inc (fromIntegral step)

coreEnumerateStepN :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
coreEnumerateStepN = mkBuiltinFn \cont handler -> \case
  [VLiteral (LInteger from), VLiteral (LInteger to), VLiteral (LInteger inc)] -> do
    v <- createEnumerateList from to inc
    returnCEKValue cont handler (VList (VLiteral . LInteger <$> v))
  _ -> failInvariant "enumerate-step"

-- concatList :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
-- concatList = mkBuiltinFn \cont handler -> \case
--   [VList li] -> do
--     li' <- traverse asList li
--     returnCEKValue cont handler (VList (V.concat (V.toList li')))
--   _ -> failInvariant "takeList"

makeList :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
makeList = mkBuiltinFn \cont handler -> \case
  [VLiteral (LInteger i), v] -> do
    returnCEKValue cont handler (VList (V.fromList (replicate (fromIntegral i) v)))
  _ -> failInvariant "makeList"

listAccess :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
listAccess = mkBuiltinFn \cont handler -> \case
  [VLiteral (LInteger i), VList vec] ->
    case vec V.!? fromIntegral i of
      Just v -> returnCEKValue cont handler v
      _ -> throwExecutionError' (ArrayOutOfBoundsException (V.length vec) (fromIntegral i))
  _ -> failInvariant "list-access"

-----------------------------------
-- try-related ops
-----------------------------------

coreEnforce :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
coreEnforce = mkBuiltinFn \cont handler -> \case
  [VLiteral (LBool b), VLiteral (LString s)] ->
    if b then returnCEKValue cont handler (VLiteral LUnit)
    else returnCEK cont handler (VError s)
  _ -> failInvariant "enforce"

-- coreEnforceOne :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
-- coreEnforceOne = mkBuiltinFn \case
--   [VList v, VLiteral (LString msg)] ->
--     enforceFail msg (V.toList v)
--   _ -> failInvariant "coreEnforceOne"
--   where
--   handler msg rest = \case
--     EnforceException _ -> enforceFail msg rest
--     e -> throwM e
--   enforceClo _ [] = pure (VLiteral LUnit)
--   enforceClo msg (x:xs) = catch (unsafeApplyOne x (VLiteral LUnit)) (handler msg xs)
--   enforceFail msg [] = throwM (EnforceException msg)
--   enforceFail msg as = enforceClo msg as
-----------------------------------
-- Guards and reads
-----------------------------------

-- readError :: Text -> Text -> Text
-- readError field expected =
--   "invalid value at field " <> field <> " expected: " <> expected

-- coreReadInteger :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
-- coreReadInteger = mkBuiltinFn \case
--   [VLiteral (LString s)] ->
--     case view (ckeData . envMap . at (Field s)) ?cekRuntimeEnv of
--       Just pv -> case pv of
--         PLiteral l@LInteger{} -> pure (VLiteral l)
--         _ -> throwM (ReadException (readError s "integer"))
--       _ -> throwM (ReadException ("no field at key " <> s))
--   _ -> failInvariant "read-integer"

-- coreReadString :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
-- coreReadString = mkBuiltinFn \case
--   [VLiteral (LString s)] ->
--     case view (ckeData . envMap . at (Field s)) ?cekRuntimeEnv of
--       Just pv-> case pv of
--         PLiteral l@LString{} -> pure (VLiteral l)
--         _ -> throwM (ReadException (readError s "string"))
--       _ -> throwM (ReadException ("no field at key " <> s))
--   _ -> failInvariant "read-string"

-- coreReadDecimal :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
-- coreReadDecimal = mkBuiltinFn \case
--   [VLiteral (LString s)] ->
--     case view (ckeData . envMap . at (Field s)) ?cekRuntimeEnv of
--       Just pv -> case pv of
--         PLiteral l@LDecimal{} -> pure (VLiteral l)
--         _ -> throwM (ReadException (readError s "decimal"))
--       _ -> throwM (ReadException ("no field at key " <> s))
--   _ -> failInvariant "read-decimal"

-- coreReadObject :: CEKRuntime b i => Row Void -> CEKValue b i m  -> EvalT b i (CEKValue b i m)
-- coreReadObject ty = \case
--   VLiteral (LString s) ->
--     case view (ckeData . envMap . at (Field s)) ?cekRuntimeEnv of
--       Just pv -> case pv of
--         t@PObject{} | checkPactValueType (TyRow ty) t -> pure (fromPactValue t)
--         _ -> throwM (ReadException (readError s "object"))
--       _ -> throwM (ReadException ("no field at key " <> s))
--   _ -> failInvariant "readObject"

-- coreReadKeyset :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
-- coreReadKeyset = mkBuiltinFn \case
--   [VLiteral (LString s)] ->
--     case view (ckeData . envMap . at (Field s)) ?cekRuntimeEnv of
--       Just pv -> case pv of
--         PObject m -> case lookupKs m of
--           Just ks -> pure (VGuard (GKeyset ks))
--           _ -> throwM (ReadException "Invalid keyset format")
--         _ -> throwM (ReadException (readError s "decimal"))
--       _ -> throwM (ReadException ("no field at key " <> s))
--   _ -> failInvariant "read-keyset"
--   where
--   -- Todo: public key parsing.
--   -- This is most certainly wrong, it needs more checks.
--   lookupKs m = do
--     ks <- Map.lookup (Field "keys") m >>= \case
--       PList v -> do
--         o <- traverse (preview (_PLiteral . _LString)) v
--         guard (all (T.all isHexDigit) o)
--         pure $ Set.fromList $ V.toList (PublicKey . T.encodeUtf8 <$> o)
--       _ -> Nothing
--     kspred <- case Map.lookup (Field "pred") m of
--       (Just (PLiteral LString{})) -> pure KeysAll
--       Just _ -> Nothing
--       Nothing -> pure KeysAll
--     pure (KeySet ks kspred)


-- coreKeysetRefGuard :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
-- coreKeysetRefGuard = mkBuiltinFn \case
--   [VLiteral (LString s)] -> pure (VGuard (GKeySetRef (KeySetName s)))
--   _ -> failInvariant "keyset-ref-guard"

-- coreEnforceGuard :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
-- coreEnforceGuard = mkBuiltinFn \case
--   [VGuard v] -> case v of
--     GKeyset ks -> enforceKeySet ks
--     GKeySetRef ksr -> enforceKeySetRef ksr
--     GUserGuard ug -> enforceUserGuard ug
--   _ -> failInvariant "enforceGuard"

-- enforceKeySet :: CEKRuntime b i => KeySet name -> EvalT b i (CEKValue b i m)
-- enforceKeySet (KeySet keys p) = do
--   let sigs = _ckeSigs ?cekRuntimeEnv
--       matched = Set.size $ Set.filter (`Set.member` keys) sigs
--       count = Set.size keys
--   case p of
--     KeysAll | matched == count -> pure (VLiteral LUnit)
--     Keys2 | matched >= 2 -> pure (VLiteral LUnit)
--     KeysAny | matched > 0 -> pure (VLiteral LUnit)
--     _ -> throwM (EnforceException "cannot match keyset predicate")

-- enforceKeySetRef :: CEKRuntime b i => KeySetName -> EvalT b i (CEKValue b i m)
-- enforceKeySetRef ksr = do
--   let pactDb = _ckePactDb ?cekRuntimeEnv
--   liftIO (_readKeyset pactDb ksr) >>= \case
--     Just ks -> enforceKeySet ks
--     Nothing -> throwM (EnforceException "no such keyset")

-- enforceUserGuard :: CEKRuntime b i => CEKValue b i m -> EvalT b i (CEKValue b i m)
-- enforceUserGuard = \case
--   v@VClosure{} -> unsafeApplyOne v (VLiteral LUnit) >>= \case
--     VLiteral LUnit -> pure (VLiteral LUnit)
--     _ -> failInvariant "expected a function returning unit"
--   _ -> failInvariant "invalid type for user closure"

-- createUserGuard :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
-- createUserGuard = mkBuiltinFn \case
--   [v@VClosure{}] -> pure (VGuard (GUserGuard v))
--   _ -> failInvariant "create-user-guard"

-----------------------------------
-- Other Core forms
-----------------------------------

-- coreIf :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
-- coreIf = mkBuiltinFn \case
--   [VLiteral (LBool b), VClosure tbody tenv, VClosure fbody fenv] ->
--     if b then eval tenv tbody else  eval fenv fbody
--   _ -> failInvariant "if"

coreB64Encode :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
coreB64Encode = mkBuiltinFn \cont handler -> \case
  [VLiteral (LString l)] ->
    returnCEKValue cont handler $ VLiteral $ LString $ toB64UrlUnpaddedText $ T.encodeUtf8 l
  _ -> failInvariant "base64-encode"


coreB64Decode :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
coreB64Decode = mkBuiltinFn \cont handler -> \case
  [VLiteral (LString s)] -> case fromB64UrlUnpaddedText $ T.encodeUtf8 s of
    Left{} -> throwExecutionError' (DecodeError "invalid b64 encoding")
    Right txt -> returnCEKValue cont handler (VLiteral (LString txt))
  _ -> failInvariant "base64-encode"



-----------------------------------
-- Core definitions
-----------------------------------

unimplemented :: NativeFn b i m
unimplemented = error "unimplemented"

rawBuiltinRuntime
  :: (MonadEval RawBuiltin i m)
  => RawBuiltin
  -> NativeFn RawBuiltin i m
rawBuiltinRuntime = rawBuiltinLiftedRuntime id

rawBuiltinLiftedRuntime
  :: (MonadEval b i m, BuiltinArity b)
  => (RawBuiltin -> b)
  -> RawBuiltin
  -> NativeFn b i m
rawBuiltinLiftedRuntime f = \case
  RawAdd -> rawAdd (f RawAdd)
  RawSub -> rawSub (f RawSub)
  RawMultiply -> rawMul (f RawMultiply)
  RawDivide -> rawDiv (f RawDivide)
  RawNegate -> rawNegate (f RawNegate)
  RawAbs -> rawAbs (f RawAbs)
  RawPow -> rawPow (f RawPow)
  RawNot -> notBool (f RawNot)
  RawEq -> rawEq (f RawEq)
  RawNeq -> rawNeq (f RawNeq)
  RawGT -> rawGt (f RawGT)
  RawGEQ -> rawGeq (f RawGEQ)
  RawLT -> rawLt (f RawLT)
  RawLEQ -> rawLeq (f RawLEQ)
  RawBitwiseAnd -> bitAndInt (f RawBitwiseAnd)
  RawBitwiseOr -> bitOrInt (f RawBitwiseOr)
  RawBitwiseXor -> bitXorInt (f RawBitwiseXor)
  RawBitwiseFlip -> bitComplementInt (f RawBitwiseFlip)
  RawBitShift -> bitShiftInt (f RawBitShift)
  RawRound -> roundDec (f RawRound)
  RawCeiling -> ceilingDec (f RawCeiling)
  RawFloor -> floorDec (f RawFloor)
  RawExp -> rawExp (f RawExp)
  RawLn -> rawLn (f RawLn)
  RawSqrt -> rawSqrt (f RawSqrt)
  RawLogBase -> rawLogBase (f RawLogBase)
  RawLength -> rawLength (f RawLength)
  RawTake -> rawTake (f RawTake)
  RawDrop -> rawDrop (f RawDrop)
  RawConcat -> concatStr (f RawConcat)
  RawReverse -> rawReverse (f RawReverse)
  RawMod -> modInt (f RawMod)
  RawMap -> coreMap (f RawMap)
  RawFilter -> coreFilter (f RawFilter)
  RawZip -> zipList (f RawZip)
  RawIntToStr -> unimplemented
  RawStrToInt -> unimplemented
  RawFold -> coreFold (f RawFold)
  RawDistinct -> unimplemented
  RawEnforce -> coreEnforce (f RawEnforce)
  RawEnforceOne -> unimplemented
  RawEnumerate -> coreEnumerate (f RawEnumerate)
  RawEnumerateStepN -> coreEnumerateStepN (f RawEnumerateStepN)
  RawShow -> rawShow (f RawShow)
  RawReadInteger -> unimplemented
  RawReadDecimal -> unimplemented
  RawReadString -> unimplemented
  RawReadKeyset -> unimplemented
  RawEnforceGuard -> unimplemented
  RawKeysetRefGuard -> unimplemented
  RawListAccess -> listAccess (f RawListAccess)
  RawMakeList -> makeList (f RawMakeList)
  RawB64Encode -> coreB64Encode (f RawB64Encode)
  RawB64Decode -> coreB64Decode (f RawB64Decode)
  RawStrToList -> strToList (f RawStrToList)
