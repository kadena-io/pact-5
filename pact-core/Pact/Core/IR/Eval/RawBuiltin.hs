{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ConstraintKinds #-}

module Pact.Core.IR.Eval.RawBuiltin
 ( rawBuiltinLiftedRuntime
 , rawBuiltinRuntime
 , prettyShowValue ) where

-- |
-- Module      :  Pact.Core.Eval.RawBuiltin
-- Copyright   :  (C) 2022 Kadena
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jose Cardona <jose@kadena.io>
--
-- CEK runtime for our IR term
--

import Control.Lens hiding (from, to, op, (%%=))
import Control.Monad(when)
import Data.Bits
import Data.Decimal(roundTo', Decimal)
import Data.Text(Text)
import Data.Vector(Vector)
import Control.Monad.IO.Class
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as V
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
-- import qualified Data.RAList as RAList

import Pact.Core.Builtin
import Pact.Core.Literal
import Pact.Core.Errors
import Pact.Core.Hash
import Pact.Core.Names
import Pact.Core.Pretty(renderText)
import Pact.Core.Guards
import Pact.Core.Type(Arg(..))
import Pact.Core.PactValue
import Pact.Core.Persistence

import Pact.Core.IR.Term
import Pact.Core.IR.Eval.Runtime
import Pact.Core.IR.Eval.CEK


----------------------------------------------------------------------
-- Our builtin definitions start here
----------------------------------------------------------------------

-- -- Todo: runtime error
unaryIntFn :: (BuiltinArity b, MonadEval b i m) => (Integer -> Integer) -> i -> b -> NativeFn b i m
unaryIntFn op info = mkBuiltinFn info \cont handler -> \case
  [VLiteral (LInteger i)] -> returnCEKValue cont handler (VLiteral (LInteger (op i)))
  _ -> failInvariant "unary int function"
{-# INLINE unaryIntFn #-}

binaryIntFn
  :: (BuiltinArity b, MonadEval b i m)
  => (Integer -> Integer -> Integer)
  -> i
  -> b
  -> NativeFn b i m
binaryIntFn op info = mkBuiltinFn info \cont handler -> \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> returnCEKValue cont handler (VLiteral (LInteger (op i i')))
  _ -> failInvariant "binary int function"
{-# INLINE binaryIntFn #-}

roundingFn :: (BuiltinArity b, MonadEval b i m) => (Rational -> Integer) -> i -> b -> NativeFn b i m
roundingFn op info = mkBuiltinFn info \cont handler -> \case
  [VLiteral (LDecimal i)] -> returnCEKValue cont handler (VLiteral (LInteger (truncate (roundTo' op 0 i))))
  _ -> failInvariant "rounding function"
{-# INLINE roundingFn #-}

---------------------------------
-- Arithmetic Ops
------------------------------
rawAdd :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
rawAdd info = mkBuiltinFn info \cont handler -> \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> returnCEKValue cont handler (VLiteral (LInteger (i + i')))
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> returnCEKValue cont handler (VLiteral (LDecimal (i + i')))
  [VLiteral (LString i), VLiteral (LString i')] ->
    returnCEKValue cont handler  (VLiteral (LString (i <> i')))
  [VObject l, VObject r] ->
    let o' = VObject (l `Map.union` r)
    in returnCEKValue cont handler o'
  [VList l, VList r] -> returnCEKValue cont handler (VList (l <> r))
  _ -> failInvariant "add"

rawSub :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
rawSub info = mkBuiltinFn info \cont handler -> \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> returnCEKValue cont handler (VLiteral (LInteger (i - i')))
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> returnCEKValue cont handler (VLiteral (LDecimal (i - i')))
  _ -> failInvariant "subtract"

rawMul :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
rawMul info = mkBuiltinFn info \cont handler -> \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> returnCEKValue cont handler (VLiteral (LInteger (i * i')))
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> returnCEKValue cont handler (VLiteral (LDecimal (i * i')))
  _ -> failInvariant "multiply"

rawPow :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
rawPow info = mkBuiltinFn info \cont handler -> \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> do
    when (i' < 0) $ throwExecutionError' (ArithmeticException "negative exponent in integer power")
    returnCEKValue cont handler (VLiteral (LInteger (i ^ i')))
  [VLiteral (LDecimal a), VLiteral (LDecimal b)] -> do
    let result = dec2F a ** dec2F b
    guardNanOrInf result
    returnCEKValue cont handler (VLiteral (LDecimal (f2Dec result)))
  _ -> failInvariant "pow"

rawLogBase :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
rawLogBase info = mkBuiltinFn info \cont handler -> \case
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

rawDiv :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
rawDiv info = mkBuiltinFn info \cont handler -> \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] ->
    if i' == 0 then throwExecutionError' (ArithmeticException "div by zero")
    else returnCEKValue cont handler (VLiteral (LInteger (div i i')))
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] ->
    if i' == 0 then throwExecutionError' (ArithmeticException "div by zero, decimal")
    else returnCEKValue cont handler (VLiteral (LDecimal (i / i')))
  _ -> failInvariant "div"

rawNegate :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
rawNegate info = mkBuiltinFn info \cont handler -> \case
  [VLiteral (LInteger i)] ->
    returnCEKValue cont handler (VLiteral (LInteger (negate i)))
  [VLiteral (LDecimal i)] ->
    returnCEKValue cont handler (VLiteral (LDecimal (negate i)))
  _ -> failInvariant "negate"

rawEq :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
rawEq info = mkBuiltinFn info \cont handler -> \case
  [VPactValue pv, VPactValue pv'] -> returnCEKValue cont handler (VBool (pv == pv'))
  _ -> failInvariant "eq"

modInt :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
modInt = binaryIntFn mod

prettyShowValue :: CEKValue b i m -> Text
prettyShowValue = \case
  -- Todo: REMOVE THIS. THIS CANNOT MAKE IT INTO OUTPUTS.
  VPactValue p -> renderText p
  VClosure _ -> "<#closure>"

rawNeq :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
rawNeq info = mkBuiltinFn info \cont handler -> \case
  [VPactValue pv, VPactValue pv'] ->
    returnCEKValue cont handler (VBool (pv /= pv'))
  _ -> failInvariant "neq"

rawGt :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
rawGt info = mkBuiltinFn info \cont handler -> \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> returnCEKValue cont handler (VLiteral (LBool (i > i')))
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> returnCEKValue cont handler (VLiteral (LBool (i > i')))
  [VLiteral (LString i), VLiteral (LString i')] -> returnCEKValue cont handler (VLiteral (LBool (i > i')))
  _ -> failInvariant "int cmp function"

rawLt :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
rawLt info = mkBuiltinFn info \cont handler -> \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> returnCEKValue cont handler (VLiteral (LBool (i < i')))
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> returnCEKValue cont handler (VLiteral (LBool (i < i')))
  [VLiteral (LString i), VLiteral (LString i')] -> returnCEKValue cont handler (VLiteral (LBool (i < i')))
  _ -> failInvariant "int cmp function"

rawGeq :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
rawGeq info = mkBuiltinFn info \cont handler -> \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> returnCEKValue cont handler (VLiteral (LBool (i >= i')))
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> returnCEKValue cont handler (VLiteral (LBool (i >= i')))
  [VLiteral (LString i), VLiteral (LString i')] -> returnCEKValue cont handler (VLiteral (LBool (i >= i')))
  _ -> failInvariant "int cmp function"

rawLeq :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
rawLeq info = mkBuiltinFn info \cont handler -> \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> returnCEKValue cont handler (VLiteral (LBool (i <= i')))
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> returnCEKValue cont handler (VLiteral (LBool (i <= i')))
  [VLiteral (LString i), VLiteral (LString i')] -> returnCEKValue cont handler (VLiteral (LBool (i <= i')))
  _ -> failInvariant "int cmp function"

bitAndInt :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
bitAndInt = binaryIntFn (.&.)

bitOrInt :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
bitOrInt = binaryIntFn (.|.)

bitComplementInt :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
bitComplementInt = unaryIntFn complement

bitXorInt :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
bitXorInt = binaryIntFn xor

bitShiftInt :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
bitShiftInt =  binaryIntFn (\i s -> shift i (fromIntegral s))

rawAbs :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
rawAbs info = mkBuiltinFn info \cont handler -> \case
  [VLiteral (LInteger i)] ->
    returnCEKValue cont handler (VLiteral (LInteger (abs i)))
  [VLiteral (LDecimal e)] -> do
    returnCEKValue cont handler (VLiteral (LDecimal (abs e)))
  _ -> failInvariant "abs"

rawExp :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
rawExp info = mkBuiltinFn info \cont handler -> \case
  [VLiteral (LInteger i)] -> do
    let result = exp (fromIntegral i)
    guardNanOrInf result
    returnCEKValue cont handler (VLiteral (LDecimal (f2Dec result)))
  [VLiteral (LDecimal e)] -> do
    let result = exp (dec2F e)
    guardNanOrInf result
    returnCEKValue cont handler (VLiteral (LDecimal (f2Dec result)))
  _ -> failInvariant "exe"

rawLn :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
rawLn info = mkBuiltinFn info \cont handler -> \case
  [VLiteral (LInteger i)] -> do
    let result = log (fromIntegral i)
    guardNanOrInf result
    returnCEKValue cont handler (VLiteral (LDecimal (f2Dec result)))
  [VLiteral (LDecimal e)] -> do
    let result = log (dec2F e)
    guardNanOrInf result
    returnCEKValue cont handler (VLiteral (LDecimal (f2Dec result)))
  _ -> failInvariant "lnInt"

rawSqrt :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
rawSqrt info = mkBuiltinFn info \cont handler -> \case
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
rawShow :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
rawShow info = mkBuiltinFn info \cont handler -> \case
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

rawContains :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
rawContains info = mkBuiltinFn info \cont handler -> \case
  [VString f, VObject o] ->
    returnCEKValue cont handler (VBool (Map.member (Field f) o))
  [VString s, VString s'] ->
    returnCEKValue cont handler (VBool (s `T.isInfixOf` s'))
  [VPactValue v, VList vli] ->
    returnCEKValue cont handler (VBool (v `V.elem` vli))
  _ -> failInvariant "contains"

rawSort :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
rawSort info = mkBuiltinFn info \cont handler -> \case
  [VList vli]
    | V.null vli -> returnCEKValue cont handler (VList mempty)
    | otherwise -> do
    vli' <- liftIO $ do
      v' <- V.thaw vli
      V.sort v'
      V.freeze v'
    returnCEKValue cont handler (VList vli')
  _ -> failInvariant "contains"

rawRemove :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
rawRemove info = mkBuiltinFn info \cont handler -> \case
  [VString s, VObject o] -> returnCEKValue cont handler (VObject (Map.delete (Field s) o))
  _ -> failInvariant "contains"

asObject :: PactValue -> Map.Map Field PactValue
asObject = \case
  PObject o -> o
  _ -> error "todo: as-object-failure"

rawSortObject :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
rawSortObject info = mkBuiltinFn info \cont handler -> \case
  [VList fields, VList objs]
    | V.null fields -> returnCEKValue cont handler (VList objs)
    | V.null objs -> returnCEKValue cont handler (VList objs)
    | otherwise -> do
        let objs' = asObject <$> objs
        fields' <- traverse (fmap Field . asString) fields
        v' <- liftIO $ do
          mobjs <- V.thaw objs'
          V.sortBy (sort fields') mobjs
          V.freeze mobjs
        returnCEKValue cont handler (VList (PObject <$> v'))
    where
    sort fs o o' =
      foldr go EQ fs
      where
      go field EQ = case (,) <$> Map.lookup field o <*> Map.lookup field o' of
        Just (PLiteral l1, PLiteral l2) -> l1 `compare` l2
        _ -> EQ
      go _ ne = ne
  _ -> failInvariant "contains"


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

roundDec :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
roundDec = roundingFn round

floorDec :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
floorDec = roundingFn floor

ceilingDec :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
ceilingDec = roundingFn ceiling

---------------------------
-- bool ops
---------------------------
notBool :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
notBool info = mkBuiltinFn info \cont handler -> \case
  [VLiteral (LBool i)] -> returnCEKValue cont handler  (VLiteral (LBool (not i)))
  _ -> failInvariant "notBool"

---------------------------
-- string ops
---------------------------

rawTake :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
rawTake info = mkBuiltinFn info \cont handler -> \case
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

rawDrop :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
rawDrop info = mkBuiltinFn info \cont handler -> \case
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

rawLength :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
rawLength info = mkBuiltinFn info \cont handler -> \case
  [VLiteral (LString t)] -> do
    returnCEKValue cont handler  (VLiteral (LInteger (fromIntegral (T.length t))))
  [VList li] -> returnCEKValue cont handler (VLiteral (LInteger (fromIntegral (V.length li))))
  _ -> failInvariant "lengthStr"

rawReverse :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
rawReverse info = mkBuiltinFn info \cont handler -> \case
  [VList li] ->
    returnCEKValue cont handler (VList (V.reverse li))
  [VLiteral (LString t)] -> do
    returnCEKValue cont handler  (VLiteral (LString (T.reverse t)))
  _ -> failInvariant "reverseStr"

-- showStr :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
-- showStr info = mkBuiltinFn info \cont handler -> \case
--   [VLiteral (LString t)] -> do
--     let out = "\"" <> t <> "\""
--     returnCEKValue cont handler  (VLiteral (LString out))
--   _ -> failInvariant "showStr"

coreConcat :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
coreConcat info = mkBuiltinFn info \cont handler -> \case
  [VList li] -> do
    li' <- traverse asString li
    returnCEKValue cont handler (VString (T.concat (V.toList li')))
  _ -> failInvariant "concatStr"

strToList :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
strToList info = mkBuiltinFn info \cont handler -> \case
  [VLiteral (LString s)] -> do
    let v = VList (V.fromList (PLiteral . LString . T.singleton <$> T.unpack s))
    returnCEKValue cont handler v
  _ -> failInvariant "concatStr"

---------------------------
-- Unit ops
---------------------------

-- eqUnit :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
-- eqUnit info = mkBuiltinFn info \cont handler -> \case
--   [VLiteral LUnit, VLiteral LUnit] -> returnCEKValue cont handler (VLiteral (LBool True))
--   _ -> failInvariant "eqUnit"

-- neqUnit :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
-- neqUnit info = mkBuiltinFn info \cont handler -> \case
--   [VLiteral LUnit, VLiteral LUnit] -> returnCEKValue cont handler (VLiteral (LBool False))
--   _ -> failInvariant "neqUnit"

-- showUnit :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
-- showUnit info = mkBuiltinFn info \cont handler -> \case
--   [VLiteral LUnit] -> returnCEKValue cont handler (VLiteral (LString "()"))
--   _ -> failInvariant "showUnit"

---------------------------
-- Object ops
---------------------------

-- eqObj :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
-- eqObj info = mkBuiltinFn info \case
--   [l@VObject{}, r@VObject{}] -> pure (VLiteral (LBool (unsafeEqCEKValue l r)))
--   _ -> failInvariant "eqObj"

-- neqObj :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
-- neqObj info = mkBuiltinFn info \case
--   [l@VObject{}, r@VObject{}] -> pure (VLiteral (LBool (unsafeNeqCEKValue l r)))
--   _ -> failInvariant "neqObj"


------------------------------
--- conversions + unsafe ops
------------------------------
-- asBool :: MonadEval b i m => CEKValue b i m -> m Bool
-- asBool (VLiteral (LBool b)) = pure b
-- asBool _ = failInvariant "asBool"

asString :: MonadEval b i m => PactValue -> m Text
asString (PLiteral (LString b)) = pure b
asString _ = failInvariant "asString"

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
-- neqList info = mkBuiltinFn info \cont handler -> \case
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

zipList :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
zipList info = mkBuiltinFn info \cont handler -> \case
  [clo, VList l, VList r] -> zip' (V.toList l) (V.toList r) []
    where
    zip' (x:xs) (y:ys) acc = unsafeApplyTwo clo (VPactValue x) (VPactValue y) >>= \case
       EvalValue v -> enforcePactValue v >>= zip' xs ys . (:acc)
       v@VError{} -> returnCEK cont handler v
    zip' _ _ acc = returnCEKValue cont handler (VList (V.fromList (reverse acc)))
  _ -> failInvariant "zipList"

-- addList :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
-- addList info = mkBuiltinFn info \cont handler -> \case
--   [VList l, VList r] -> returnCEKValue cont handler (VList (l <> r))
--   _ -> failInvariant "addList"

-- pcShowList :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
-- pcShowList info = mkBuiltinFn info \cont handler -> \case
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

coreMap :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
coreMap info = mkBuiltinFn info \cont handler -> \case
  [fn, VList li] -> map' (V.toList li) []
    where
    map' (x:xs) acc = unsafeApplyOne fn (VPactValue x) >>= \case
       EvalValue cv -> enforcePactValue cv >>= map' xs . (:acc)
       v -> returnCEK cont handler v
    map' _ acc = returnCEKValue cont handler (VList (V.fromList (reverse acc)))
  _ -> failInvariant "map"

coreFilter :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
coreFilter info = mkBuiltinFn info \cont handler -> \case
  [fn, VList li] -> filter' (V.toList li) []
    where
    filter' (x:xs) acc = unsafeApplyOne fn (VPactValue x) >>= \case
      EvalValue (VLiteral (LBool b)) ->
        if b then filter' xs (x:acc) else filter' xs acc
      v@VError{} ->
        returnCEK cont handler v
      _ -> failInvariant "filter"
    filter' [] acc = returnCEKValue cont handler (VList (V.fromList (reverse acc)))
  _ -> failInvariant "filter"

coreFold :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
coreFold info = mkBuiltinFn info \cont handler -> \case
  [fn, initElem, VList li] ->
    fold' initElem (V.toList li)
    where
    fold' e (x:xs) = unsafeApplyTwo fn e (VPactValue x) >>= \case
      EvalValue v -> fold' v xs
      v -> returnCEK cont handler v
    fold' e [] = returnCEKValue cont handler e
  _ -> failInvariant "fold"

coreEnumerate :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
coreEnumerate info = mkBuiltinFn info \cont handler -> \case
  [VLiteral (LInteger from), VLiteral (LInteger to)] -> do
    v <- createEnumerateList from to (if from > to then -1 else 1)
    returnCEKValue cont handler (VList (PLiteral . LInteger <$> v))
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

coreEnumerateStepN :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
coreEnumerateStepN info = mkBuiltinFn info \cont handler -> \case
  [VLiteral (LInteger from), VLiteral (LInteger to), VLiteral (LInteger inc)] -> do
    v <- createEnumerateList from to inc
    returnCEKValue cont handler (VList (PLiteral . LInteger <$> v))
  _ -> failInvariant "enumerate-step"

-- concatList :: (BuiltinArity b, MonadEval b i m) => b -> NativeFn b i m
-- concatList info = mkBuiltinFn info \cont handler -> \case
--   [VList li] -> do
--     li' <- traverse asList li
--     returnCEKValue cont handler (VList (V.concat (V.toList li')))
--   _ -> failInvariant "takeList"

makeList :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
makeList info = mkBuiltinFn info \cont handler -> \case
  [VLiteral (LInteger i), VPactValue v] -> do
    returnCEKValue cont handler (VList (V.fromList (replicate (fromIntegral i) v)))
  _ -> failInvariant "makeList"

coreAccess :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
coreAccess info = mkBuiltinFn info \cont handler -> \case
  [VLiteral (LInteger i), VList vec] ->
    case vec V.!? fromIntegral i of
      Just v -> returnCEKValue cont handler (VPactValue v)
      _ -> throwExecutionError' (ArrayOutOfBoundsException (V.length vec) (fromIntegral i))
  [VString field, VObject o] ->
    case Map.lookup (Field field) o of
      Just v -> returnCEKValue cont handler (VPactValue v)
      Nothing -> error "todo: better error here"
  _ -> failInvariant "list-access"


coreYield :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
coreYield info = mkBuiltinFn info \cont handler -> \case
  [VObject o] -> do
    esPactExec . _Just . peYield %%= const (Just $ Yield o)
    returnCEKValue cont handler VUnit
  _ -> failInvariant "yield expects object"


-----------------------------------
-- try-related ops
-----------------------------------

coreEnforce :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
coreEnforce info = mkBuiltinFn info \cont handler -> \case
  [VLiteral (LBool b), VLiteral (LString s)] ->
    if b then returnCEKValue cont handler (VLiteral LUnit)
    else returnCEK cont handler (VError s)
  _ -> failInvariant "enforce"

-- coreEnforceOne :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
-- coreEnforceOne info = mkBuiltinFn info \case
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

-- coreReadInteger :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
-- coreReadInteger info = mkBuiltinFn info \case
--   [VLiteral (LString s)] ->
--     case view (ckeData . envMap . at (Field s)) ?cekRuntimeEnv of
--       Just pv -> case pv of
--         PLiteral l@LInteger{} -> pure (VLiteral l)
--         _ -> throwM (ReadException (readError s "integer"))
--       _ -> throwM (ReadException ("no field at key " <> s))
--   _ -> failInvariant "read-integer"

-- coreReadString :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
-- coreReadString info = mkBuiltinFn info \case
--   [VLiteral (LString s)] ->
--     case view (ckeData . envMap . at (Field s)) ?cekRuntimeEnv of
--       Just pv-> case pv of
--         PLiteral l@LString{} -> pure (VLiteral l)
--         _ -> throwM (ReadException (readError s "string"))
--       _ -> throwM (ReadException ("no field at key " <> s))
--   _ -> failInvariant "read-string"

-- coreReadDecimal :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
-- coreReadDecimal info = mkBuiltinFn info \case
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

-- coreReadKeyset :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
-- coreReadKeyset info = mkBuiltinFn info \case
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


-- coreKeysetRefGuard :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
-- coreKeysetRefGuard info = mkBuiltinFn info \case
--   [VLiteral (LString s)] -> pure (VGuard (GKeySetRef (KeySetName s)))
--   _ -> failInvariant "keyset-ref-guard"

-- coreEnforceGuard :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
-- coreEnforceGuard info = mkBuiltinFn info \case
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

-- createUserGuard :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
-- createUserGuard info = mkBuiltinFn info \case
--   [v@VClosure{}] -> pure (VGuard (GUserGuard v))
--   _ -> failInvariant "create-user-guard"

-----------------------------------
-- Other Core forms
-----------------------------------

-- coreIf :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
-- coreIf info = mkBuiltinFn info \case
--   [VLiteral (LBool b), VClosure tbody tenv, VClosure fbody fenv] ->
--     if b then eval tenv tbody else  eval fenv fbody
--   _ -> failInvariant "if"

coreB64Encode :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
coreB64Encode info = mkBuiltinFn info \cont handler -> \case
  [VLiteral (LString l)] ->
    returnCEKValue cont handler $ VLiteral $ LString $ toB64UrlUnpaddedText $ T.encodeUtf8 l
  _ -> failInvariant "base64-encode"


coreB64Decode :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
coreB64Decode info = mkBuiltinFn info \cont handler -> \case
  [VLiteral (LString s)] -> case fromB64UrlUnpaddedText $ T.encodeUtf8 s of
    Left{} -> throwExecutionError' (DecodeError "invalid b64 encoding")
    Right txt -> returnCEKValue cont handler (VLiteral (LString txt))
  _ -> failInvariant "base64-encode"

coreEnforceGuard :: (BuiltinArity b, MonadEval b i m) => i -> b -> NativeFn b i m
coreEnforceGuard info = mkBuiltinFn info \cont handler -> \case
  [VGuard g] -> case g of
      GKeyset ks -> do
        cond <- enforceKeyset ks
        if cond then returnCEKValue cont handler VUnit
        else returnCEK cont handler (VError "enforce keyset failure")
      GKeySetRef ksn -> do
        cond <- enforceKeysetName ksn
        if cond then returnCEKValue cont handler VUnit
        else returnCEK cont handler (VError "enforce keyset failure")
      GUserGuard ug -> runUserGuard cont handler ug
  _ -> failInvariant "enforce-guard"


enforceKeyset :: MonadEval b i m => KeySet FullyQualifiedName -> m Bool
enforceKeyset (KeySet kskeys ksPred) = do
  sigs <- Map.filterWithKey matchKey . view eeMsgSigs <$> readEnv
  runPred (Map.size sigs)
  where
  matchKey k _ = k `elem` kskeys
  atLeast t m = m >= t
  -- elide pk
  --   | T.length pk < 8 = pk
  --   | otherwise = T.take 8 pk <> "..."
  count = Set.size kskeys
  -- failed = "Keyset failure"
  runPred matched =
    case ksPred of
      KeysAll -> run atLeast
      KeysAny -> run (\_ m -> atLeast 1 m)
      Keys2 -> run (\_ m -> atLeast 2 m)
    where
    run p = pure (p count matched)

enforceKeysetName
  :: MonadEval b i m
  => KeySetName
  -> m Bool
enforceKeysetName ksn = do
  pactDb <- view eePactDb <$> readEnv
  liftIO (readKeyset pactDb ksn) >>= \case
    Just ks -> enforceKeyset ks
    Nothing -> failInvariant "No such keyset"

runUserGuard
  :: MonadEval b i m
  => Cont b i m
  -> CEKErrorHandler b i m
  -> UserGuard FullyQualifiedName PactValue
  -> m (EvalResult b i m)
runUserGuard cont handler (UserGuard fqn args) =
  lookupFqName fqn >>= \case
    Just (Dfun d) -> do
      when (length (_dfunArgs d) /= length args) $ error "user guard not saturated"
      -- Todo: this is probably needs to be factored out
      let li = TLDefun (_fqModule fqn) (_fqName fqn)
          cloargs = NE.fromList (_argType <$> _dfunArgs d)
          clo = Closure li cloargs (NE.length cloargs) (_dfunTerm d) (_dfunRType d) (_dfunInfo d)
      applyLam (C clo) (VPactValue <$> args) cont handler
    _ -> failInvariant "enforce-user-guard"

-----------------------------------
-- Core definitions
-----------------------------------

unimplemented :: NativeFn b i m
unimplemented = error "unimplemented"

rawBuiltinRuntime
  :: (MonadEval RawBuiltin i m)
  => i
  -> RawBuiltin
  -> NativeFn RawBuiltin i m
rawBuiltinRuntime = rawBuiltinLiftedRuntime id

rawBuiltinLiftedRuntime
  :: (MonadEval b i m, BuiltinArity b)
  => (RawBuiltin -> b)
  -> i
  -> RawBuiltin
  -> NativeFn b i m
rawBuiltinLiftedRuntime f i = \case
  RawAdd -> rawAdd i (f RawAdd)
  RawSub -> rawSub i (f RawSub)
  RawMultiply -> rawMul i (f RawMultiply)
  RawDivide -> rawDiv i (f RawDivide)
  RawNegate -> rawNegate i (f RawNegate)
  RawAbs -> rawAbs i (f RawAbs)
  RawPow -> rawPow i (f RawPow)
  RawNot -> notBool i (f RawNot)
  RawEq -> rawEq i (f RawEq)
  RawNeq -> rawNeq i (f RawNeq)
  RawGT -> rawGt i (f RawGT)
  RawGEQ -> rawGeq i (f RawGEQ)
  RawLT -> rawLt i (f RawLT)
  RawLEQ -> rawLeq i (f RawLEQ)
  RawBitwiseAnd -> bitAndInt i (f RawBitwiseAnd)
  RawBitwiseOr -> bitOrInt i (f RawBitwiseOr)
  RawBitwiseXor -> bitXorInt i (f RawBitwiseXor)
  RawBitwiseFlip -> bitComplementInt i (f RawBitwiseFlip)
  RawBitShift -> bitShiftInt i (f RawBitShift)
  RawRound -> roundDec i (f RawRound)
  RawCeiling -> ceilingDec i (f RawCeiling)
  RawFloor -> floorDec i (f RawFloor)
  RawExp -> rawExp i (f RawExp)
  RawLn -> rawLn i (f RawLn)
  RawSqrt -> rawSqrt i (f RawSqrt)
  RawLogBase -> rawLogBase i (f RawLogBase)
  RawLength -> rawLength i (f RawLength)
  RawTake -> rawTake i (f RawTake)
  RawDrop -> rawDrop i (f RawDrop)
  RawConcat -> coreConcat i (f RawConcat)
  RawReverse -> rawReverse i (f RawReverse)
  RawMod -> modInt i (f RawMod)
  RawMap -> coreMap i (f RawMap)
  RawFilter -> coreFilter i (f RawFilter)
  RawZip -> zipList i (f RawZip)
  RawIntToStr -> unimplemented
  RawStrToInt -> unimplemented
  RawFold -> coreFold i (f RawFold)
  RawDistinct -> unimplemented
  RawContains -> rawContains i (f RawContains)
  RawSort -> rawSort i (f RawSort)
  RawSortObject -> rawSortObject i (f RawSortObject)
  RawRemove -> rawRemove i (f RawRemove)
  RawEnforce -> coreEnforce i (f RawEnforce)
  RawEnforceOne -> unimplemented
  RawEnumerate -> coreEnumerate i (f RawEnumerate)
  RawEnumerateStepN -> coreEnumerateStepN i (f RawEnumerateStepN)
  RawShow -> rawShow i (f RawShow)
  RawReadInteger -> unimplemented
  RawReadDecimal -> unimplemented
  RawReadString -> unimplemented
  RawReadKeyset -> unimplemented
  RawEnforceGuard -> coreEnforceGuard i (f RawEnforceGuard)
  RawKeysetRefGuard -> unimplemented
  RawAt -> coreAccess i (f RawAt)
  RawMakeList -> makeList i (f RawMakeList)
  RawB64Encode -> coreB64Encode i (f RawB64Encode)
  RawB64Decode -> coreB64Decode i (f RawB64Decode)
  RawStrToList -> strToList i (f RawStrToList)
  RawYield -> coreYield i (f RawYield)
