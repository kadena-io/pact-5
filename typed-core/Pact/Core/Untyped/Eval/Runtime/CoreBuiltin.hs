{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ConstraintKinds #-}

-- |
-- Module      :  Pact.Core.IR.Typecheck
-- Copyright   :  (C) 2022 Kadena
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jose Cardona <jose@kadena.io>
--
-- CEK Evaluator for untyped core using our RawBuiltins (aka untyped, no typechecking)
--

module Pact.Core.Untyped.Eval.Runtime.CoreBuiltin
  ( coreBuiltinRuntime
  , coreBuiltinLiftedRuntime ) where

import Control.Monad(when)

import Data.Bits
import Data.Decimal(roundTo', Decimal)
import Data.Text(Text)
import Data.Vector(Vector)
import qualified Data.Vector as V
-- import qualified Data.Primitive.Array as Array
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Pact.Core.Builtin
import Pact.Core.Literal
import Pact.Core.Errors
import Pact.Core.Hash

import Pact.Core.Untyped.Eval.Runtime
import Pact.Core.Untyped.Eval.CEK

-- | Run our CEK interpreter
--   for only our core builtins
--   monomorphized version
-- runCoreCEK
  -- :: EvalEnv CoreBuiltin i
  -- ^ Runtime environment
  -- -> EvalTerm CoreBuiltin i
  -- ^ Term to evaluate
--   -> IO (CEKValue CoreBuiltin i)
-- runCoreCEK = runCEK
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
-- integer ops
------------------------------
addInt :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
addInt = binaryIntFn (+)

subInt :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
subInt = binaryIntFn (-)

mulInt :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
mulInt = binaryIntFn (*)

powInt :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
powInt = mkBuiltinFn \cont handler -> \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> do
    when (i' < 0) $ throwExecutionError' (ArithmeticException "negative exponent in integer power")
    returnCEKValue cont handler (VLiteral (LInteger (i ^ i')))
  _ -> failInvariant "binary int function"

logBaseInt :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
logBaseInt = mkBuiltinFn \cont handler -> \case
  [VLiteral (LInteger base), VLiteral (LInteger n)] -> do
    when (base < 0 || n <= 0) $ throwExecutionError' (ArithmeticException "Illegal log base")
    let base' = fromIntegral base :: Double
        n' = fromIntegral n
        out = round (logBase base' n')
    returnCEKValue cont handler (VLiteral (LInteger out))
    -- if i' == 0 then throwExecutionError' (ArithmeticException "div by zero")
    -- else returnCEKValue cont handler (VLiteral (LInteger (div i i')))
  _ -> failInvariant "binary int function"

divInt :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
divInt = mkBuiltinFn \cont handler -> \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] ->
    if i' == 0 then throwExecutionError' (ArithmeticException "div by zero")
    else returnCEKValue cont handler (VLiteral (LInteger (div i i')))
  _ -> failInvariant "binary int function"

negateInt :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
negateInt = unaryIntFn negate

modInt :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
modInt = binaryIntFn mod

eqInt :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
eqInt = compareIntFn (==)

neqInt :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
neqInt = compareIntFn (/=)

gtInt :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
gtInt = compareIntFn (>)

ltInt :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
ltInt = compareIntFn (<)

geqInt :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
geqInt = compareIntFn (>=)

leqInt :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
leqInt = compareIntFn (<=)

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

absInt :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
absInt = unaryIntFn abs

expInt :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
expInt = mkBuiltinFn \cont handler -> \case
  [VLiteral (LInteger i)] -> do
    let result = exp (fromIntegral i)
    guardNanOrInf result
    returnCEKValue cont handler (VLiteral (LDecimal (f2Dec result)))
  _ -> failInvariant "expInt"

lnInt :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
lnInt = mkBuiltinFn \cont handler -> \case
  [VLiteral (LInteger i)] -> do
    let result = log (fromIntegral i)
    guardNanOrInf result
    returnCEKValue cont handler (VLiteral (LDecimal (f2Dec result)))
  _ -> failInvariant "lnInt"

sqrtInt :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
sqrtInt = mkBuiltinFn \cont handler -> \case
  [VLiteral (LInteger i)] -> do
    when (i < 0) $ throwExecutionError' (ArithmeticException "Square root must be non-negative")
    let result = sqrt (fromIntegral i)
    guardNanOrInf result
    returnCEKValue cont handler (VLiteral (LDecimal (f2Dec result)))
  _ -> failInvariant "sqrtInt"

showInt :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
showInt = mkBuiltinFn \cont handler -> \case
  [VLiteral (LInteger i)] ->
    returnCEKValue cont handler (VLiteral (LString (T.pack (show i))))
  _ -> failInvariant "showInt"

-- -------------------------
-- double ops
-- -------------------------

addDec :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
addDec = binaryDecFn (+)

subDec :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
subDec = binaryDecFn (-)

mulDec :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
mulDec = binaryDecFn (*)

guardNanOrInf :: MonadEval b i m => Double -> m ()
guardNanOrInf a =
  when (isNaN a || isInfinite a) $ throwExecutionError' (FloatingPointError "Floating operation resulted in Infinity or NaN")

powDec :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
powDec = mkBuiltinFn \cont handler -> \case
  [VLiteral (LDecimal a), VLiteral (LDecimal b)] -> do
    let result = dec2F a ** dec2F b
    guardNanOrInf result
    returnCEKValue cont handler (VLiteral (LDecimal (f2Dec result)))
  _ -> failInvariant "binary decimal function"

divDec :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
divDec =  mkBuiltinFn \cont handler -> \case
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] ->
    if i' == 0 then throwExecutionError' (ArithmeticException "div by zero, decimal")
    else returnCEKValue cont handler (VLiteral (LDecimal (i / i')))
  _ -> failInvariant "binary decimal function"

negateDec :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
negateDec = unaryDecFn negate

absDec :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
absDec = unaryDecFn abs

eqDec :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
eqDec = compareDecFn (==)

neqDec :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
neqDec = compareDecFn (/=)

gtDec :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
gtDec = compareDecFn (>)

geqDec :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
geqDec = compareDecFn (>=)

ltDec :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
ltDec = compareDecFn (<)

leqDec :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
leqDec = compareDecFn (<=)

showDec :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
showDec = mkBuiltinFn \cont handler -> \case
  [VLiteral (LDecimal i)] ->
    returnCEKValue cont handler  (VLiteral (LString (T.pack (show i))))
  _ -> failInvariant "showDec"

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
expDec :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
expDec = mkBuiltinFn \cont handler -> \case
  [VLiteral (LDecimal e)] -> do
    let result = exp (dec2F e)
    guardNanOrInf result
    returnCEKValue cont handler (VLiteral (LDecimal (f2Dec result)))
  _ -> failInvariant "binary decimal function"
  -- unaryDecFn (f2Dec . exp . dec2F)

lnDec :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
lnDec = mkBuiltinFn \cont handler -> \case
  [VLiteral (LDecimal e)] -> do
    let result = log (dec2F e)
    guardNanOrInf result
    returnCEKValue cont handler (VLiteral (LDecimal (f2Dec result)))
  _ -> failInvariant "binary decimal function"

logBaseDec :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
logBaseDec = mkBuiltinFn \cont handler -> \case
  [VLiteral (LDecimal base), VLiteral (LDecimal arg)] -> do
    when (base < 0 || arg <= 0) $ throwExecutionError' (ArithmeticException "Invalid base or argument in log")
    let result = logBase (dec2F base) (dec2F arg)
    guardNanOrInf result
    returnCEKValue cont handler (VLiteral (LDecimal (f2Dec result)))
  _ -> failInvariant "binary decimal function"


sqrtDec :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
sqrtDec = mkBuiltinFn \cont handler -> \case
  [VLiteral (LDecimal e)] -> do
    when (e < 0) $ throwExecutionError' (ArithmeticException "Square root must be non-negative")
    let result = sqrt (dec2F e)
    guardNanOrInf result
    returnCEKValue cont handler (VLiteral (LDecimal (f2Dec result)))
  _ -> failInvariant "binary decimal function"


---------------------------
-- bool ops
---------------------------
-- andBool :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
-- andBool = binaryBoolFn (&&)

-- orBool :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
-- orBool = binaryBoolFn (||)

notBool :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
notBool = mkBuiltinFn \cont handler -> \case
  [VLiteral (LBool i)] -> returnCEKValue cont handler  (VLiteral (LBool (not i)))
  _ -> failInvariant "notBool"

eqBool :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
eqBool = binaryBoolFn (==)

neqBool :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
neqBool = binaryBoolFn (/=)

showBool :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
showBool = mkBuiltinFn \cont handler -> \case
  [VLiteral (LBool i)] -> do
    let out = if i then "true" else "false"
    returnCEKValue cont handler (VLiteral (LString out))
  _ -> failInvariant "showBool"

---------------------------
-- string ops
---------------------------
eqStr :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
eqStr = compareStrFn (==)

neqStr :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
neqStr = compareStrFn (/=)

gtStr :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
gtStr = compareStrFn (>)

geqStr :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
geqStr = compareStrFn (>=)

ltStr :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
ltStr = compareStrFn (<)

leqStr :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
leqStr = compareStrFn (<=)

addStr :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
addStr =  mkBuiltinFn \cont handler -> \case
  [VLiteral (LString i), VLiteral (LString i')] ->
    returnCEKValue cont handler  (VLiteral (LString (i <> i')))
  _ -> failInvariant "addStr"

takeStr :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
takeStr = mkBuiltinFn \cont handler -> \case
  [VLiteral (LInteger i), VLiteral (LString t)]
    | i >= 0 -> do
      let clamp = min (fromIntegral i) (T.length t)
      returnCEKValue cont handler  (VLiteral (LString (T.take clamp t)))
    | otherwise -> do
      let clamp = min (abs (T.length t + fromIntegral i)) (T.length t)
      returnCEKValue cont handler  (VLiteral (LString (T.drop clamp t)))
  _ -> failInvariant "takeStr"

dropStr :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
dropStr = mkBuiltinFn \cont handler -> \case
  [VLiteral (LInteger i), VLiteral (LString t)]
    | i >= 0 -> do
      let clamp = min (fromIntegral i) (T.length t)
      returnCEKValue cont handler  (VLiteral (LString (T.drop clamp t)))
    | otherwise -> do
      let clamp = min (abs (T.length t + fromIntegral i)) (T.length t)
      returnCEKValue cont handler  (VLiteral (LString (T.take clamp t)))
  _ -> failInvariant "dropStr"

lengthStr :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
lengthStr = mkBuiltinFn \cont handler -> \case
  [VLiteral (LString t)] -> do
    returnCEKValue cont handler  (VLiteral (LInteger (fromIntegral (T.length t))))
  _ -> failInvariant "lengthStr"

reverseStr :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
reverseStr = mkBuiltinFn \cont handler -> \case
  [VLiteral (LString t)] -> do
    returnCEKValue cont handler  (VLiteral (LString (T.reverse t)))
  _ -> failInvariant "reverseStr"

showStr :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
showStr = mkBuiltinFn \cont handler -> \case
  [VLiteral (LString t)] -> do
    let out = "\"" <> t <> "\""
    returnCEKValue cont handler  (VLiteral (LString out))
  _ -> failInvariant "showStr"

concatStr :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
concatStr = mkBuiltinFn \cont handler -> \case
  [VList li] -> do
    li' <- traverse asString li
    returnCEKValue cont handler (VLiteral (LString (T.concat (V.toList li'))))
  _ -> failInvariant "concatStr"

strToList :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
strToList = mkBuiltinFn \cont handler -> \case
  [VLiteral (LString s)] -> do
    let v = (VList (V.fromList ((VLiteral . LString . T.singleton <$> T.unpack s))))
    returnCEKValue cont handler v
  _ -> failInvariant "concatStr"

---------------------------
-- Unit ops
---------------------------

eqUnit :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
eqUnit = mkBuiltinFn \cont handler -> \case
  [VLiteral LUnit, VLiteral LUnit] -> returnCEKValue cont handler (VLiteral (LBool True))
  _ -> failInvariant "eqUnit"

neqUnit :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
neqUnit = mkBuiltinFn \cont handler -> \case
  [VLiteral LUnit, VLiteral LUnit] -> returnCEKValue cont handler (VLiteral (LBool False))
  _ -> failInvariant "neqUnit"

showUnit :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
showUnit = mkBuiltinFn \cont handler -> \case
  [VLiteral LUnit] -> returnCEKValue cont handler (VLiteral (LString "()"))
  _ -> failInvariant "showUnit"

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
eqList :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
eqList = mkBuiltinFn \cont handler -> \case
  [eqClo, VList l, VList r] ->
    if V.length l /= V.length r then
      returnCEKValue cont handler (VLiteral (LBool False))
    else zip' (V.toList l) (V.toList r) []
    where
    zip' [] _ acc = returnCEKValue cont handler (VLiteral (LBool (and acc)))
    zip' _ [] acc = returnCEKValue cont handler (VLiteral (LBool (and acc)))
    zip' (x:xs) (y:ys) acc = unsafeApplyTwo eqClo x y >>= \case
       EvalValue (VLiteral (LBool b)) -> zip' xs ys (b:acc)
       v@VError{} -> returnCEK cont handler v
       _ -> failInvariant "applying closure in list eq yielded incorrect type"
  _ -> failInvariant "eqList"

neqList :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
neqList = mkBuiltinFn \cont handler -> \case
  [neqClo, VList l, VList r] ->
    if V.length l /= V.length r then
      returnCEKValue cont handler (VLiteral (LBool True))
    else zip' (V.toList l) (V.toList r) []
    where
    zip' (x:xs) (y:ys) acc = unsafeApplyTwo neqClo x y >>= \case
       EvalValue (VLiteral (LBool b)) -> zip' xs ys (b:acc)
       v@VError{} -> returnCEK cont handler v
       _ -> failInvariant "applying closure in list eq yielded incorrect type"
    zip' _ _ acc = returnCEKValue cont handler (VLiteral (LBool (or acc)))
  _ -> failInvariant "neqList"

zipList :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
zipList = mkBuiltinFn \cont handler -> \case
  [clo, VList l, VList r] -> zip' (V.toList l) (V.toList r) []
    where
    zip' (x:xs) (y:ys) acc = unsafeApplyTwo clo x y >>= \case
       EvalValue v -> zip' xs ys (v:acc)
       v@VError{} -> returnCEK cont handler v
    zip' _ _ acc = returnCEKValue cont handler (VList (V.fromList (reverse acc)))
  _ -> failInvariant "zipList"

addList :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
addList = mkBuiltinFn \cont handler -> \case
  [VList l, VList r] -> returnCEKValue cont handler (VList (l <> r))
  _ -> failInvariant "addList"

pcShowList :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
pcShowList = mkBuiltinFn \cont handler -> \case
  [showFn, VList l1] -> show' (V.toList l1) []
    where
    show' (x:xs) acc = unsafeApplyOne showFn x >>= \case
       EvalValue (VLiteral (LString b)) -> show' xs (b:acc)
       v@VError{} -> returnCEK cont handler v
       _ -> failInvariant "applying closure in list eq yielded incorrect type"
    show' _ acc = do
      let out = "[" <> T.intercalate ", " (reverse acc) <> "]"
      returnCEKValue cont handler (VLiteral (LString out))
  _ -> failInvariant "showList"

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

takeList :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
takeList = mkBuiltinFn \cont handler -> \case
  [VLiteral (LInteger i), VList li]
    | i >= 0 -> do
      let clamp = fromIntegral $ min i (fromIntegral (V.length li))
      returnCEKValue cont handler  (VList (V.take clamp li))
    | otherwise -> do
      let clamp = fromIntegral $ max (fromIntegral (V.length li) + i) 0
      returnCEKValue cont handler (VList (V.drop clamp li))
  _ -> failInvariant "takeList"

dropList :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
dropList = mkBuiltinFn \cont handler -> \case
  [VLiteral (LInteger i), VList li]
    | i >= 0 -> do
      let clamp = fromIntegral $ min i (fromIntegral (V.length li))
      returnCEKValue cont handler  (VList (V.drop clamp li))
    | otherwise -> do
      let clamp = fromIntegral $ max (fromIntegral (V.length li) + i) 0
      returnCEKValue cont handler (VList (V.take clamp li))
  _ -> failInvariant "dropList"

reverseList :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
reverseList = mkBuiltinFn \cont handler -> \case
  [VList li] ->
    returnCEKValue cont handler (VList (V.reverse li))
  _ -> failInvariant "takeList"

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

concatList :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
concatList = mkBuiltinFn \cont handler -> \case
  [VList li] -> do
    li' <- traverse asList li
    returnCEKValue cont handler (VList (V.concat (V.toList li')))
  _ -> failInvariant "takeList"

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
-- Module references
-----------------------------------
eqModRef :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
eqModRef = mkBuiltinFn \cont handler -> \case
  [VModRef m1 _,  VModRef m2 _] ->
    returnCEKValue cont handler $ VBool (m1 == m2)
  vals -> failInvariant $ "base64-encode" <> T.pack (show vals)

neqModRef :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
neqModRef = mkBuiltinFn \cont handler -> \case
  [VModRef m1 _,  VModRef m2 _] ->
    returnCEKValue cont handler $ VBool (m1 /= m2)
  _ -> failInvariant "base64-encode"


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

coreBuiltinRuntime :: MonadEval CoreBuiltin i m => CoreBuiltin -> NativeFn CoreBuiltin i m
coreBuiltinRuntime = \case
  -- Int Add + num ops
  AddInt -> addInt AddInt
  SubInt -> subInt SubInt
  DivInt -> divInt DivInt
  MulInt -> mulInt MulInt
  NegateInt -> negateInt NegateInt
  AbsInt -> absInt AbsInt
  PowInt -> powInt PowInt
  -- Int fractional
  ExpInt -> expInt ExpInt
  LnInt -> lnInt LnInt
  SqrtInt -> sqrtInt SqrtInt
  LogBaseInt -> logBaseInt LogBaseInt
  -- Geenral int ops
  ModInt -> modInt ModInt
  BitAndInt -> bitAndInt BitAndInt
  BitOrInt -> bitOrInt BitOrInt
  BitXorInt ->  bitXorInt BitXorInt
  BitShiftInt -> bitShiftInt BitShiftInt
  BitComplementInt -> bitComplementInt BitComplementInt
  -- Int Equality + Ord
  EqInt -> eqInt EqInt
  NeqInt -> neqInt NeqInt
  GTInt -> gtInt GTInt
  GEQInt -> geqInt GEQInt
  LTInt -> ltInt LTInt
  LEQInt -> leqInt LEQInt
  -- IntShow inst
  ShowInt -> showInt ShowInt
  -- If
  -- IfElse -> coreIf IfElse
  -- Decimal ops
  -- Add + Num
  AddDec -> addDec AddDec
  SubDec -> subDec SubDec
  DivDec -> divDec DivDec
  MulDec -> mulDec MulDec
  PowDec -> powDec PowDec
  NegateDec -> negateDec NegateDec
  AbsDec -> absDec AbsDec
  -- Decimal rounding ops
  RoundDec -> roundDec RoundDec
  CeilingDec -> ceilingDec CeilingDec
  FloorDec -> floorDec FloorDec
  -- Decimal fractional
  ExpDec -> expDec ExpDec
  LnDec -> lnDec LnDec
  LogBaseDec -> logBaseDec LogBaseDec
  SqrtDec -> sqrtDec SqrtDec
  -- Decimal show
  ShowDec -> showDec ShowDec
  -- Decimal Equality + Ord
  EqDec -> eqDec EqDec
  NeqDec -> neqDec NeqDec
  GTDec -> gtDec GTDec
  GEQDec -> geqDec GEQDec
  LTDec -> ltDec LTDec
  LEQDec -> leqDec LEQDec
  -- Bool Ops
  -- AndBool -> andBool AndBool
  -- OrBool -> orBool OrBool
  NotBool -> notBool NotBool
  -- Bool Equality
  EqBool -> eqBool EqBool
  NeqBool -> neqBool NeqBool
  ShowBool -> showBool ShowBool
  -- String Equality + Ord
  EqStr -> eqStr EqStr
  NeqStr -> neqStr NeqStr
  GTStr -> gtStr GTStr
  GEQStr -> geqStr GEQStr
  LTStr -> ltStr LTStr
  LEQStr -> leqStr LEQStr
  -- String Ops
  AddStr -> addStr AddStr
  -- String listlike
  ConcatStr -> concatStr ConcatStr
  DropStr -> dropStr DropStr
  TakeStr -> takeStr TakeStr
  LengthStr -> lengthStr LengthStr
  ReverseStr -> reverseStr ReverseStr
  -- String show
  ShowStr -> showStr ShowStr
  -- Object equality
  -- EqObj -> eqObj EqObj
  -- NeqObj -> neqObj NeqObj
  -- List Equality + Ord
  EqList -> eqList EqList
  NeqList -> neqList NeqList
  GTList -> unimplemented
  GEQList -> unimplemented
  LTList -> unimplemented
  LEQList -> unimplemented
  -- List Show
  ShowList -> pcShowList ShowList
  -- ListAdd
  AddList -> addList AddList
  -- List ListlLike
  TakeList -> takeList TakeList
  DropList -> dropList DropList
  LengthList -> lengthList LengthList
  ConcatList -> concatList ConcatList
  ReverseList -> reverseList ReverseList
  -- misc list ops
  FilterList -> coreFilter FilterList
  DistinctList -> unimplemented
  ZipList -> zipList ZipList
  MapList -> coreMap MapList
  FoldList -> coreFold FoldList
  -- Unit ops
  EqUnit -> eqUnit EqUnit
  NeqUnit -> neqUnit NeqUnit
  ShowUnit -> showUnit ShowUnit
  EqModRef -> eqModRef EqModRef
  NeqModRef -> neqModRef NeqModRef
  Enforce -> coreEnforce Enforce
  EnforceOne -> unimplemented
    -- coreEnforceOne EnforceOne
  Enumerate -> coreEnumerate Enumerate
  EnumerateStepN -> coreEnumerateStepN EnumerateStepN
  ReadInteger -> unimplemented
  ReadDecimal -> unimplemented
  ReadString -> unimplemented
  -- ReadInteger -> coreReadInteger ReadInteger
  -- ReadDecimal -> coreReadDecimal ReadDecimal
  -- ReadString -> coreReadString ReadString
  -- ReadKeyset -> coreReadKeyset ReadKeyset
  -- EnforceGuard -> coreEnforceGuard EnforceGuard
  -- KeysetRefGuard -> coreKeysetRefGuard KeysetRefGuard
  ReadKeyset -> unimplemented
  EnforceGuard -> unimplemented
  KeysetRefGuard -> unimplemented
  -- CreateUserGuard -> createUserGuard CreateUserGuard
  ListAccess -> listAccess ListAccess
  MakeList -> makeList MakeList
  B64Encode -> coreB64Encode B64Encode
  B64Decode -> coreB64Decode B64Decode
  StrToList -> strToList StrToList

coreBuiltinLiftedRuntime
  :: (MonadEval b i m, BuiltinArity b)
  => (CoreBuiltin -> b)
  -> CoreBuiltin
  -> NativeFn b i m
coreBuiltinLiftedRuntime f = \case
  -- Int Add + num ops
  AddInt -> addInt (f AddInt)
  SubInt -> subInt (f SubInt)
  DivInt -> divInt (f DivInt)
  MulInt -> mulInt (f MulInt)
  PowInt -> powInt (f PowInt)
  NegateInt -> negateInt (f NegateInt)
  AbsInt -> absInt (f AbsInt)
  -- Int fractional
  ExpInt -> expInt (f ExpInt)
  LnInt -> lnInt (f LnInt)
  SqrtInt -> sqrtInt (f SqrtInt)
  LogBaseInt -> logBaseInt (f LogBaseInt)
  -- Geenral int ops
  ModInt -> modInt (f ModInt)
  BitAndInt -> bitAndInt (f BitAndInt)
  BitOrInt -> bitOrInt (f BitOrInt)
  BitXorInt ->  bitXorInt (f BitXorInt)
  BitShiftInt -> bitShiftInt (f BitShiftInt)
  BitComplementInt -> bitComplementInt (f BitComplementInt)
  -- Int Equality + Ord
  EqInt -> eqInt (f EqInt)
  NeqInt -> neqInt (f NeqInt)
  GTInt -> gtInt (f GTInt)
  GEQInt -> geqInt (f GEQInt)
  LTInt -> ltInt (f LTInt)
  LEQInt -> leqInt (f LEQInt)
  -- IntShow inst
  ShowInt -> showInt (f ShowInt)
  -- If
  -- IfElse -> coreIf (f IfElse)
  -- Decimal ops
  -- Add + Num
  AddDec -> addDec (f AddDec)
  SubDec -> subDec (f SubDec)
  DivDec -> divDec (f DivDec)
  MulDec -> mulDec (f MulDec)
  PowDec -> powDec (f PowDec)
  NegateDec -> negateDec (f NegateDec)
  AbsDec -> absDec (f AbsDec)
  -- Decimal rounding ops
  RoundDec -> roundDec (f RoundDec)
  CeilingDec -> ceilingDec (f CeilingDec)
  FloorDec -> floorDec (f FloorDec)
  -- Decimal fractional
  ExpDec -> expDec (f ExpDec)
  LnDec -> lnDec (f LnDec)
  LogBaseDec -> logBaseDec (f LogBaseDec)
  SqrtDec -> sqrtDec (f SqrtDec)
  -- Decimal show
  ShowDec -> showDec (f ShowDec)
  -- Decimal Equality + Ord
  EqDec -> eqDec (f EqDec)
  NeqDec -> neqDec (f NeqDec)
  GTDec -> gtDec (f GTDec)
  GEQDec -> geqDec (f GEQDec)
  LTDec -> ltDec (f LTDec)
  LEQDec -> leqDec (f LEQDec)
  -- Bool Ops
  -- AndBool -> andBool (f AndBool)
  -- OrBool -> orBool (f OrBool)
  NotBool -> notBool (f NotBool)
  -- Bool Equality
  EqBool -> eqBool (f EqBool)
  NeqBool -> neqBool (f NeqBool)
  ShowBool -> showBool (f ShowBool)
  -- String Equality + Ord
  EqStr -> eqStr (f EqStr)
  NeqStr -> neqStr (f NeqStr)
  GTStr -> gtStr (f GTStr)
  GEQStr -> geqStr (f GEQStr)
  LTStr -> ltStr (f LTStr)
  LEQStr -> leqStr (f LEQStr)
  -- String Ops
  AddStr -> addStr (f AddStr)
  -- String listlike
  ConcatStr -> concatStr (f ConcatStr)
  DropStr -> dropStr (f DropStr)
  TakeStr -> takeStr (f TakeStr)
  LengthStr -> lengthStr (f LengthStr)
  ReverseStr -> reverseStr (f ReverseStr)
  -- String show
  ShowStr -> showStr (f ShowStr)
  -- Object equality
  -- EqObj -> eqObj EqObj
  -- NeqObj -> neqObj NeqObj
  -- List Equality + Ord
  EqList -> eqList (f EqList)
  NeqList -> neqList (f NeqList)
  GTList -> unimplemented
  GEQList -> unimplemented
  LTList -> unimplemented
  LEQList -> unimplemented
  -- List Show
  ShowList -> pcShowList (f ShowList)
  -- ListAdd
  AddList -> addList (f AddList)
  -- List ListlLike
  TakeList -> takeList (f TakeList)
  DropList -> dropList (f DropList)
  LengthList -> lengthList (f LengthList)
  ConcatList -> concatList (f ConcatList)
  ReverseList -> reverseList (f ReverseList)
  -- misc list ops
  FilterList -> coreFilter (f FilterList)
  DistinctList -> unimplemented
  ZipList -> zipList (f ZipList)
  MapList -> coreMap (f MapList)
  FoldList -> coreFold (f FoldList)
  -- Unit ops
  EqUnit -> eqUnit (f EqUnit)
  NeqUnit -> neqUnit (f NeqUnit)
  ShowUnit -> showUnit (f ShowUnit)
  EqModRef -> eqModRef (f EqModRef)
  NeqModRef -> neqModRef (f NeqModRef)
  Enforce -> coreEnforce (f Enforce)
  EnforceOne -> unimplemented
    -- coreEnforceOne EnforceOne
  Enumerate -> coreEnumerate (f Enumerate)
  EnumerateStepN -> coreEnumerateStepN (f EnumerateStepN)
  ReadInteger -> unimplemented
  ReadDecimal -> unimplemented
  ReadString -> unimplemented
  -- ReadInteger -> coreReadInteger ReadInteger
  -- ReadDecimal -> coreReadDecimal ReadDecimal
  -- ReadString -> coreReadString ReadString
  -- ReadKeyset -> coreReadKeyset ReadKeyset
  -- EnforceGuard -> coreEnforceGuard EnforceGuard
  -- KeysetRefGuard -> coreKeysetRefGuard KeysetRefGuard
  ReadKeyset -> unimplemented
  EnforceGuard -> unimplemented
  KeysetRefGuard -> unimplemented
  ListAccess -> listAccess (f ListAccess)
  MakeList -> makeList (f MakeList)
  B64Encode -> coreB64Encode (f B64Encode)
  B64Decode -> coreB64Decode (f B64Decode)
  StrToList -> strToList (f StrToList)
