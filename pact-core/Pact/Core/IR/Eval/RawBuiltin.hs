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
 ( rawBuiltinRuntime
 , rawBuiltinEnv ) where

-- |
-- Module      :  Pact.Core.Eval.RawBuiltin
-- Copyright   :  (C) 2022 Kadena
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jose Cardona <jose@kadena.io>
--
-- CEK runtime for our IR term
--

import Control.Lens hiding (from, to, op)
import Control.Monad(when)
import Data.Bits
import Data.Decimal(roundTo', Decimal)
import Data.Vector(Vector)
import Control.Monad.IO.Class
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as V
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
-- import qualified Data.RAList as RAList

import Pact.Core.Builtin
import Pact.Core.Literal
import Pact.Core.Errors
import Pact.Core.Hash
import Pact.Core.Names
import Pact.Core.Guards
import Pact.Core.Type(Arg(..))
import Pact.Core.PactValue
import Pact.Core.Persistence
import Pact.Core.Environment

import Pact.Core.IR.Term
import Pact.Core.IR.Eval.Runtime
import Pact.Core.IR.Eval.CEK


----------------------------------------------------------------------
-- Our builtin definitions start here
----------------------------------------------------------------------

-- -- Todo: runtime error
unaryIntFn :: (IsBuiltin b, MonadEval b i m) => (Integer -> Integer) -> NativeFunction b i m
unaryIntFn op = \info b cont handler _env -> \case
  [VLiteral (LInteger i)] -> returnCEKValue cont handler (VLiteral (LInteger (op i)))
  args -> argsError info b args
{-# INLINE unaryIntFn #-}

binaryIntFn
  :: (IsBuiltin b, MonadEval b i m)
  => (Integer -> Integer -> Integer)
  -> NativeFunction b i m
binaryIntFn op = \info b cont handler _env -> \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> returnCEKValue cont handler (VLiteral (LInteger (op i i')))
  args -> argsError info b args
{-# INLINE binaryIntFn #-}

roundingFn :: (IsBuiltin b, MonadEval b i m) => (Rational -> Integer) -> NativeFunction b i m
roundingFn op = \info b cont handler _env -> \case
  [VLiteral (LDecimal i)] -> returnCEKValue cont handler (VLiteral (LInteger (truncate (roundTo' op 0 i))))
  args -> argsError info b args
{-# INLINE roundingFn #-}

---------------------------------
-- Arithmetic Ops
------------------------------
rawAdd :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
rawAdd = \info b cont handler _env -> \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> returnCEKValue cont handler (VLiteral (LInteger (i + i')))
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> returnCEKValue cont handler (VLiteral (LDecimal (i + i')))
  [VLiteral (LString i), VLiteral (LString i')] ->
    returnCEKValue cont handler  (VLiteral (LString (i <> i')))
  [VObject l, VObject r] ->
    let o' = VObject (l `M.union` r)
    in returnCEKValue cont handler o'
  [VList l, VList r] -> returnCEKValue cont handler (VList (l <> r))
  args -> argsError info b args

rawSub :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
rawSub = \info b cont handler _env -> \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> returnCEKValue cont handler (VLiteral (LInteger (i - i')))
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> returnCEKValue cont handler (VLiteral (LDecimal (i - i')))
  args -> argsError info b args

rawMul :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
rawMul = \info b cont handler _env -> \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> returnCEKValue cont handler (VLiteral (LInteger (i * i')))
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> returnCEKValue cont handler (VLiteral (LDecimal (i * i')))
  args -> argsError info b args

rawPow :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
rawPow = \info b cont handler _env -> \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> do
    when (i' < 0) $ throwExecutionError info (ArithmeticException "negative exponent in integer power")
    returnCEKValue cont handler (VLiteral (LInteger (i ^ i')))
  [VLiteral (LDecimal l), VLiteral (LDecimal r)] -> do
    let result = dec2F l ** dec2F r
    guardNanOrInf info result
    returnCEKValue cont handler (VLiteral (LDecimal (f2Dec result)))
  args -> argsError info b args

rawLogBase :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
rawLogBase = \info b cont handler _env -> \case
  [VLiteral (LInteger base), VLiteral (LInteger n)] -> do
    when (base < 0 || n <= 0) $ throwExecutionError info (ArithmeticException "Illegal log base")
    let base' = fromIntegral base :: Double
        n' = fromIntegral n
        out = round (logBase base' n')
    returnCEKValue cont handler (VLiteral (LInteger out))
    -- if i' == 0 then throwExecutionError' (ArithmeticException "div by zero")
    -- else returnCEKValue cont handler (VLiteral (LInteger (div i i')))
  [VLiteral (LDecimal base), VLiteral (LDecimal arg)] -> do
    when (base < 0 || arg <= 0) $ throwExecutionError info (ArithmeticException "Invalid base or argument in log")
    let result = logBase (dec2F base) (dec2F arg)
    guardNanOrInf info result
    returnCEKValue cont handler (VLiteral (LDecimal (f2Dec result)))
  args -> argsError info b args

rawDiv :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
rawDiv = \info b cont handler _env -> \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] ->
    if i' == 0 then throwExecutionError info (ArithmeticException "div by zero")
    else returnCEKValue cont handler (VLiteral (LInteger (div i i')))
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] ->
    if i' == 0 then throwExecutionError info (ArithmeticException "div by zero, decimal")
    else returnCEKValue cont handler (VLiteral (LDecimal (i / i')))
  args -> argsError info b args

rawNegate :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
rawNegate = \info b cont handler _env -> \case
  [VLiteral (LInteger i)] ->
    returnCEKValue cont handler (VLiteral (LInteger (negate i)))
  [VLiteral (LDecimal i)] ->
    returnCEKValue cont handler (VLiteral (LDecimal (negate i)))
  args -> argsError info b args

rawEq :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
rawEq = \info b cont handler _env -> \case
  [VPactValue pv, VPactValue pv'] -> returnCEKValue cont handler (VBool (pv == pv'))
  args -> argsError info b args

modInt :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
modInt = binaryIntFn mod

rawNeq :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
rawNeq = \info b cont handler _env -> \case
  [VPactValue pv, VPactValue pv'] ->
    returnCEKValue cont handler (VBool (pv /= pv'))
  args -> argsError info b args

rawGt :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
rawGt = \info b cont handler _dcNameenv -> \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> returnCEKValue cont handler (VLiteral (LBool (i > i')))
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> returnCEKValue cont handler (VLiteral (LBool (i > i')))
  [VLiteral (LString i), VLiteral (LString i')] -> returnCEKValue cont handler (VLiteral (LBool (i > i')))
  args -> argsError info b args

rawLt :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
rawLt = \info b cont handler _env -> \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> returnCEKValue cont handler (VLiteral (LBool (i < i')))
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> returnCEKValue cont handler (VLiteral (LBool (i < i')))
  [VLiteral (LString i), VLiteral (LString i')] -> returnCEKValue cont handler (VLiteral (LBool (i < i')))
  args -> argsError info b args

rawGeq :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
rawGeq = \info b cont handler _env -> \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> returnCEKValue cont handler (VLiteral (LBool (i >= i')))
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> returnCEKValue cont handler (VLiteral (LBool (i >= i')))
  [VLiteral (LString i), VLiteral (LString i')] -> returnCEKValue cont handler (VLiteral (LBool (i >= i')))
  args -> argsError info b args

rawLeq :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
rawLeq = \info b cont handler _env -> \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> returnCEKValue cont handler (VLiteral (LBool (i <= i')))
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> returnCEKValue cont handler (VLiteral (LBool (i <= i')))
  [VLiteral (LString i), VLiteral (LString i')] -> returnCEKValue cont handler (VLiteral (LBool (i <= i')))
  args -> argsError info b args

bitAndInt :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
bitAndInt = binaryIntFn (.&.)

bitOrInt :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
bitOrInt = binaryIntFn (.|.)

bitComplementInt :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
bitComplementInt = unaryIntFn complement

bitXorInt :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
bitXorInt = binaryIntFn xor

bitShiftInt :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
bitShiftInt =  binaryIntFn (\i s -> shift i (fromIntegral s))

rawAbs :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
rawAbs = \info b cont handler env -> \case
  [VLiteral (LInteger i)] ->
    returnCEKValue cont handler (VLiteral (LInteger (abs i)))
  [VLiteral (LDecimal e)] -> do
    returnCEKValue cont handler (VLiteral (LDecimal (abs e)))
  args -> argsError info b args

rawExp :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
rawExp = \info b cont handler _env -> \case
  [VLiteral (LInteger i)] -> do
    let result = exp (fromIntegral i)
    guardNanOrInf info result
    returnCEKValue cont handler (VLiteral (LDecimal (f2Dec result)))
  [VLiteral (LDecimal e)] -> do
    let result = exp (dec2F e)
    guardNanOrInf info result
    returnCEKValue cont handler (VLiteral (LDecimal (f2Dec result)))
  args -> argsError info b args

rawLn :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
rawLn = \info b cont handler _env -> \case
  [VLiteral (LInteger i)] -> do
    let result = log (fromIntegral i)
    guardNanOrInf info result
    returnCEKValue cont handler (VLiteral (LDecimal (f2Dec result)))
  [VLiteral (LDecimal e)] -> do
    let result = log (dec2F e)
    guardNanOrInf info result
    returnCEKValue cont handler (VLiteral (LDecimal (f2Dec result)))
  args -> argsError info b args

rawSqrt :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
rawSqrt = \info b cont handler _env -> \case
  [VLiteral (LInteger i)] -> do
    when (i < 0) $ throwExecutionError info (ArithmeticException "Square root must be non-negative")
    let result = sqrt (fromIntegral i)
    guardNanOrInf info result
    returnCEKValue cont handler (VLiteral (LDecimal (f2Dec result)))
  [VLiteral (LDecimal e)] -> do
    when (e < 0) $ throwExecutionError info (ArithmeticException "Square root must be non-negative")
    let result = sqrt (dec2F e)
    guardNanOrInf info result
    returnCEKValue cont handler (VLiteral (LDecimal (f2Dec result)))
  args -> argsError info b args

-- Todo: fix all show instances
rawShow :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
rawShow = \info b cont handler _env -> \case
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
  args -> argsError info b args

rawContains :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
rawContains = \info b cont handler _env -> \case
  [VString f, VObject o] ->
    returnCEKValue cont handler (VBool (M.member (Field f) o))
  [VString s, VString s'] ->
    returnCEKValue cont handler (VBool (s `T.isInfixOf` s'))
  [VPactValue v, VList vli] ->
    returnCEKValue cont handler (VBool (v `V.elem` vli))
  args -> argsError info b args

rawSort :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
rawSort = \info b cont handler _env -> \case
  [VList vli]
    | V.null vli -> returnCEKValue cont handler (VList mempty)
    | otherwise -> do
    vli' <- liftIO $ do
      v' <- V.thaw vli
      V.sort v'
      V.freeze v'
    returnCEKValue cont handler (VList vli')
  args -> argsError info b args

rawRemove :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
rawRemove = \info b cont handler _env -> \case
  [VString s, VObject o] -> returnCEKValue cont handler (VObject (M.delete (Field s) o))
  args -> argsError info b args

asObject
  :: (MonadEval b i m, IsBuiltin b)
  => i
  -> b
  -> PactValue
  -> m (M.Map Field PactValue)
asObject info b = \case
  PObject o -> pure o
  arg -> argsError info b [VPactValue arg]

rawSortObject :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
rawSortObject = \info b cont handler _env -> \case
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

guardNanOrInf :: MonadEval b i m => i -> Double -> m ()
guardNanOrInf info a =
  when (isNaN a || isInfinite a) $ throwExecutionError info (FloatingPointError "Floating operation resulted in Infinity or NaN")

dec2F :: Decimal -> Double
dec2F = fromRational . toRational

f2Dec :: Double -> Decimal
f2Dec = fromRational . toRational

roundDec :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
roundDec = roundingFn round

floorDec :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
floorDec = roundingFn floor

ceilingDec :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
ceilingDec = roundingFn ceiling

---------------------------
-- bool ops
---------------------------
notBool :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
notBool = \info b cont handler _env -> \case
  [VLiteral (LBool i)] -> returnCEKValue cont handler  (VLiteral (LBool (not i)))
  args -> argsError info b args

---------------------------
-- string ops
---------------------------

rawTake :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
rawTake = \info b cont handler _env -> \case
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
  args -> argsError info b args

rawDrop :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
rawDrop = \info b cont handler _env -> \case
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
  args -> argsError info b args

rawLength :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
rawLength = \info b cont handler _env -> \case
  [VLiteral (LString t)] -> do
    returnCEKValue cont handler  (VLiteral (LInteger (fromIntegral (T.length t))))
  [VList li] -> returnCEKValue cont handler (VLiteral (LInteger (fromIntegral (V.length li))))
  args -> argsError info b args

rawReverse :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
rawReverse = \info b cont handler _env -> \case
  [VList li] ->
    returnCEKValue cont handler (VList (V.reverse li))
  [VLiteral (LString t)] -> do
    returnCEKValue cont handler  (VLiteral (LString (T.reverse t)))
  args -> argsError info b args

-- showStr :: (IsBuiltin b, MonadEval b i m) => b -> NativeFn b i m
-- showStr = \info b cont handler env -> \case
--   [VLiteral (LString t)] -> do
--     let out = "\"" <> t <> "\""
--     returnCEKValue cont handler  (VLiteral (LString out))
--   _ -> failInvariant "showStr"

coreConcat :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
coreConcat = \info b cont handler _env -> \case
  [VList li] -> do
    li' <- traverse (asString info b) li
    returnCEKValue cont handler (VString (T.concat (V.toList li')))
  args -> argsError info b args

strToList :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
strToList = \info b cont handler _env -> \case
  [VLiteral (LString s)] -> do
    let v = VList (V.fromList (PLiteral . LString . T.singleton <$> T.unpack s))
    returnCEKValue cont handler v
  args -> argsError info b args

---------------------------
-- Unit ops
---------------------------

-- eqUnit :: (IsBuiltin b, MonadEval b i m) => b -> NativeFn b i m
-- eqUnit = \info b cont handler env -> \case
--   [VLiteral LUnit, VLiteral LUnit] -> returnCEKValue cont handler (VLiteral (LBool True))
--   _ -> failInvariant "eqUnit"

-- neqUnit :: (IsBuiltin b, MonadEval b i m) => b -> NativeFn b i m
-- neqUnit = \info b cont handler env -> \case
--   [VLiteral LUnit, VLiteral LUnit] -> returnCEKValue cont handler (VLiteral (LBool False))
--   _ -> failInvariant "neqUnit"

-- showUnit :: (IsBuiltin b, MonadEval b i m) => b -> NativeFn b i m
-- showUnit = \info b cont handler env -> \case
--   [VLiteral LUnit] -> returnCEKValue cont handler (VLiteral (LString "()"))
--   _ -> failInvariant "showUnit"

---------------------------
-- Object ops
---------------------------

-- eqObj :: (IsBuiltin b, MonadEval b i m) => b -> NativeFn b i m
-- eqObj info b = mkBuiltinFn info b \case
--   [l@VObject{}, r@VObject{}] -> pure (VLiteral (LBool (unsafeEqCEKValue l r)))
--   _ -> failInvariant "eqObj"

-- neqObj :: (IsBuiltin b, MonadEval b i m) => b -> NativeFn b i m
-- neqObj info b = mkBuiltinFn info b \case
--   [l@VObject{}, r@VObject{}] -> pure (VLiteral (LBool (unsafeNeqCEKValue l r)))
--   _ -> failInvariant "neqObj"


------------------------------
--- conversions + unsafe ops
------------------------------
-- asBool :: MonadEval b i m => CEKValue b i m -> m Bool
-- asBool (VLiteral (LBool b)) = pure b
-- asBool _ = failInvariant "asBool"

zipList :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
zipList = \info b cont handler _env -> \case
  [VClosure clo, VList l, VList r] -> zip' (V.toList l) (V.toList r) []
    where
    zip' (x:xs) (y:ys) acc = unsafeApplyTwo clo (VPactValue x) (VPactValue y) >>= \case
       EvalValue v -> enforcePactValue v >>= zip' xs ys . (:acc)
       v@VError{} -> returnCEK cont   handler v
    zip' _ _ acc = returnCEKValue cont handler (VList (V.fromList (reverse acc)))
  args -> argsError info b args

coreMap :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
coreMap = \info b cont handler _env -> \case
  [VClosure fn, VList li] -> map' (V.toList li) []
    where
    map' (x:xs) acc = unsafeApplyOne fn (VPactValue x) >>= \case
       EvalValue cv -> enforcePactValue cv >>= map' xs . (:acc)
       v -> returnCEK cont handler v
    map' _ acc = returnCEKValue cont handler (VList (V.fromList (reverse acc)))
  args -> argsError info b args

coreFilter :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
coreFilter = \info b cont handler _env -> \case
  [VClosure fn, VList li] -> filter' (V.toList li) []
    where
    filter' (x:xs) acc = unsafeApplyOne fn (VPactValue x) >>= \case
      EvalValue (VLiteral (LBool b')) ->
        if b' then filter' xs (x:acc) else filter' xs acc
      EvalValue v -> argsError info b [v]
      v@VError{} ->
        returnCEK cont handler v
    filter' [] acc = returnCEKValue cont handler (VList (V.fromList (reverse acc)))
  args -> argsError info b args

coreFold :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
coreFold = \info b cont handler _env -> \case
  [VClosure fn, initElem, VList li] ->
    fold' initElem (V.toList li)
    where
    fold' e (x:xs) = unsafeApplyTwo fn e (VPactValue x) >>= \case
      EvalValue v -> fold' v xs
      v -> returnCEK cont handler v
    fold' e [] = returnCEKValue cont handler e
  args -> argsError info b args

coreEnumerate :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
coreEnumerate = \info b cont handler _env -> \case
  [VLiteral (LInteger from), VLiteral (LInteger to)] -> do
    v <- createEnumerateList info from to (if from > to then -1 else 1)
    returnCEKValue cont handler (VList (PLiteral . LInteger <$> v))
  args -> argsError info b args

createEnumerateList
  :: (MonadEval b i m)
  => i
  -> Integer
  -- ^ from
  -> Integer
  -- ^ to
  -> Integer
  -- ^ Step
  -> m (Vector Integer)
createEnumerateList info from to inc
  | from == to = pure (V.singleton from)
  | inc == 0 = pure mempty
  | from < to, from + inc < from =
    throwExecutionError info (EnumerationError "enumerate: increment diverges below from interval bounds.")
  | from > to, from + inc > from =
    throwExecutionError info (EnumerationError "enumerate: increment diverges above from interval bounds.")
  | otherwise = let
    step = succ (abs (from - to) `div` abs inc)
    in pure $ V.enumFromStepN from inc (fromIntegral step)

coreEnumerateStepN :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
coreEnumerateStepN = \info b cont handler _env -> \case
  [VLiteral (LInteger from), VLiteral (LInteger to), VLiteral (LInteger inc)] -> do
    v <- createEnumerateList info from to inc
    returnCEKValue cont handler (VList (PLiteral . LInteger <$> v))
  args -> argsError info b args

-- concatList :: (IsBuiltin b, MonadEval b i m) => b -> NativeFn b i m
-- concatList = \info b cont handler env -> \case
--   [VList li] -> do
--     li' <- traverse asList li
--     returnCEKValue cont handler (VList (V.concat (V.toList li')))
--   _ -> failInvariant "takeList"

makeList :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
makeList = \info b cont handler _env -> \case
  [VLiteral (LInteger i), VPactValue v] -> do
    returnCEKValue cont handler (VList (V.fromList (replicate (fromIntegral i) v)))
  args -> argsError info b args

coreAccess :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
coreAccess = \info b cont handler _env -> \case
  [VLiteral (LInteger i), VList vec] ->
    case vec V.!? fromIntegral i of
      Just v -> returnCEKValue cont handler (VPactValue v)
      _ -> throwExecutionError info (ArrayOutOfBoundsException (V.length vec) (fromIntegral i))
  [VString field, VObject o] ->
    case M.lookup (Field field) o of
      Just v -> returnCEKValue cont handler (VPactValue v)
      Nothing ->
        let msg = "Object does not have field: " <> field
        in returnCEK cont handler (VError msg)
  args -> argsError info b args

-----------------------------------
-- try-related ops
-----------------------------------

coreEnforce :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
coreEnforce = \info b cont handler _env -> \case
  [VLiteral (LBool b'), VLiteral (LString s)] ->
    if b' then returnCEKValue cont handler (VLiteral LUnit)
    else returnCEK cont handler (VError s)
  args -> argsError info b args

-- coreEnforceOne :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
-- coreEnforceOne info b = mkBuiltinFn info b \case
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

-- coreReadInteger :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
-- coreReadInteger info b = mkBuiltinFn info b \case
--   [VLiteral (LString s)] ->
--     case view (ckeData . envMap . at (Field s)) ?cekRuntimeEnv of
--       Just pv -> case pv of
--         PLiteral l@LInteger{} -> pure (VLiteral l)
--         _ -> throwM (ReadException (readError s "integer"))
--       _ -> throwM (ReadException ("no field at key " <> s))
--   _ -> failInvariant "read-integer"

-- coreReadString :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
-- coreReadString info b = mkBuiltinFn info b \case
--   [VLiteral (LString s)] ->
--     case view (ckeData . envMap . at (Field s)) ?cekRuntimeEnv of
--       Just pv-> case pv of
--         PLiteral l@LString{} -> pure (VLiteral l)
--         _ -> throwM (ReadException (readError s "string"))
--       _ -> throwM (ReadException ("no field at key " <> s))
--   _ -> failInvariant "read-string"

-- coreReadDecimal :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
-- coreReadDecimal info b = mkBuiltinFn info b \case
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

-- coreReadKeyset :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
-- coreReadKeyset info b = mkBuiltinFn info b \case
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
--     ks <- M.lookup (Field "keys") m >>= \case
--       PList v -> do
--         o <- traverse (preview (_PLiteral . _LString)) v
--         guard (all (T.all isHexDigit) o)
--         pure $ Set.fromList $ V.toList (PublicKey . T.encodeUtf8 <$> o)
--       _ -> Nothing
--     kspred <- case M.lookup (Field "pred") m of
--       (Just (PLiteral LString{})) -> pure KeysAll
--       Just _ -> Nothing
--       Nothing -> pure KeysAll
--     pure (KeySet ks kspred)


-- coreKeysetRefGuard :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
-- coreKeysetRefGuard info b = mkBuiltinFn info b \case
--   [VLiteral (LString s)] -> pure (VGuard (GKeySetRef (KeySetName s)))
--   _ -> failInvariant "keyset-ref-guard"

-- coreEnforceGuard :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
-- coreEnforceGuard info b = mkBuiltinFn info b \case
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

-- createUserGuard :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
-- createUserGuard info b = mkBuiltinFn info b \case
--   [v@VClosure{}] -> pure (VGuard (GUserGuard v))
--   _ -> failInvariant "create-user-guard"

-----------------------------------
-- Other Core forms
-----------------------------------

-- coreIf :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
-- coreIf info b = mkBuiltinFn info b \case
--   [VLiteral (LBool b), VClosure tbody tenv, VClosure fbody fenv] ->
--     if b then eval tenv tbody else  eval fenv fbody
--   _ -> failInvariant "if"

coreB64Encode :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
coreB64Encode = \info b cont handler _env -> \case
  [VLiteral (LString l)] ->
    returnCEKValue cont handler $ VLiteral $ LString $ toB64UrlUnpaddedText $ T.encodeUtf8 l
  args -> argsError info b args


coreB64Decode :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
coreB64Decode = \info b cont handler _env -> \case
  [VLiteral (LString s)] -> case fromB64UrlUnpaddedText $ T.encodeUtf8 s of
    Left{} -> throwExecutionError info (DecodeError "invalid b64 encoding")
    Right txt -> returnCEKValue cont handler (VLiteral (LString txt))
  args -> argsError info b args

coreEnforceGuard :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
coreEnforceGuard = \info b cont handler env -> \case
  [VGuard g] -> case g of
      GKeyset ks -> do
        cond <- enforceKeyset ks env
        if cond then returnCEKValue cont handler VUnit
        else returnCEK cont handler (VError "enforce keyset failure")
      GKeySetRef ksn -> do
        cond <- enforceKeysetName info ksn env
        if cond then returnCEKValue cont handler VUnit
        else returnCEK cont handler (VError "enforce keyset failure")
      GUserGuard ug -> runUserGuard info cont handler env ug
  args -> argsError info b args


enforceKeyset
  :: MonadEval b i m
  => KeySet FullyQualifiedName
  -> CEKEnv b i m
  -> m Bool
enforceKeyset (KeySet kskeys ksPred) env = do
  let sigs = views (ceEnv.eeMsgSigs) (M.filterWithKey matchKey) env
  runPred (M.size sigs)
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
  => i
  -> KeySetName
  -> CEKEnv b i m
  -> m Bool
enforceKeysetName info ksn env = do
  let pactDb = view (ceEnv.eePactDb) env
  liftIO (readKeyset pactDb ksn) >>= \case
    Just ks -> enforceKeyset ks env
    Nothing ->
      throwExecutionError info (NoSuchKeySet ksn)

runUserGuard
  :: MonadEval b i m
  => i
  -> Cont b i m
  -> CEKErrorHandler b i m
  -> CEKEnv b i m
  -> UserGuard FullyQualifiedName PactValue
  -> m (EvalResult b i m)
runUserGuard info cont handler env (UserGuard fqn args) =
  lookupFqName fqn >>= \case
    Just (Dfun d) -> do
      when (length (_dfunArgs d) /= length args) $ error "user guard not saturated"
      -- Todo: this is probably needs to be factored out
      let li = TLDefun (_fqModule fqn) (_fqName fqn)
          cloargs = NE.fromList (_argType <$> _dfunArgs d)
          clo = Closure li cloargs (NE.length cloargs) (_dfunTerm d) (_dfunRType d) env (_dfunInfo d)
      -- Todo: sys only here
      applyLam (C clo) (VPactValue <$> args) cont handler
    Just d -> throwExecutionError info (InvalidDefKind (defKind d) "run-user-guard")
    Nothing -> throwExecutionError info (NameNotInScope fqn)

coreBind :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
coreBind = \info b cont handler _env -> \case
  [v@VObject{}, VClosure clo] ->
    applyLam clo [v] cont handler
  args -> argsError info b args


createTable :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
createTable = \info b cont handler env -> \case
  [VTable tn mn _ _] -> do
    let pdb = view (ceEnv . eePactDb) env
    -- Todo: error handling here
    -- Todo: guard table
    liftIO (_pdbCreateUserTable pdb tn mn)
    returnCEKValue cont handler VUnit
  args -> argsError info b args

-----------------------------------
-- Core definitions
-----------------------------------

unimplemented :: NativeFunction b i m
unimplemented = error "unimplemented"

rawBuiltinEnv
  :: (MonadEval RawBuiltin i m)
  => BuiltinEnv RawBuiltin i m
rawBuiltinEnv i b env = mkBuiltinFn i b env (rawBuiltinRuntime b)

rawBuiltinRuntime
  :: (MonadEval b i m, IsBuiltin b)
  => RawBuiltin
  -> NativeFunction b i m
rawBuiltinRuntime = \case
  RawAdd -> rawAdd
  RawSub -> rawSub
  RawMultiply -> rawMul
  RawDivide -> rawDiv
  RawNegate -> rawNegate
  RawAbs -> rawAbs
  RawPow -> rawPow
  RawNot -> notBool
  RawEq -> rawEq
  RawNeq -> rawNeq
  RawGT -> rawGt
  RawGEQ -> rawGeq
  RawLT -> rawLt
  RawLEQ -> rawLeq
  RawBitwiseAnd -> bitAndInt
  RawBitwiseOr -> bitOrInt
  RawBitwiseXor -> bitXorInt
  RawBitwiseFlip -> bitComplementInt
  RawBitShift -> bitShiftInt
  RawRound -> roundDec
  RawCeiling -> ceilingDec
  RawFloor -> floorDec
  RawExp -> rawExp
  RawLn -> rawLn
  RawSqrt -> rawSqrt
  RawLogBase -> rawLogBase
  RawLength -> rawLength
  RawTake -> rawTake
  RawDrop -> rawDrop
  RawConcat -> coreConcat
  RawReverse -> rawReverse
  RawMod -> modInt
  RawMap -> coreMap
  RawFilter -> coreFilter
  RawZip -> zipList
  RawIntToStr -> unimplemented
  RawStrToInt -> unimplemented
  RawFold -> coreFold
  RawDistinct -> unimplemented
  RawContains -> rawContains
  RawSort -> rawSort
  RawSortObject -> rawSortObject
  RawRemove -> rawRemove
  RawEnforce -> coreEnforce
  RawEnforceOne -> unimplemented
  RawEnumerate -> coreEnumerate
  RawEnumerateStepN -> coreEnumerateStepN
  RawShow -> rawShow
  RawReadInteger -> unimplemented
  RawReadDecimal -> unimplemented
  RawReadString -> unimplemented
  RawReadKeyset -> unimplemented
  RawEnforceGuard -> coreEnforceGuard
  RawKeysetRefGuard -> unimplemented
  RawAt -> coreAccess
  RawMakeList -> makeList
  RawB64Encode -> coreB64Encode
  RawB64Decode -> coreB64Decode
  RawStrToList -> strToList
  RawBind -> coreBind
  RawCreateTable -> createTable
  RawDescribeKeyset -> unimplemented
  RawDescribeModule -> unimplemented
  RawDescribeTable -> unimplemented
  RawFoldDb -> unimplemented
  RawInsert -> unimplemented
  RawKeyLog -> unimplemented
  RawKeys -> unimplemented
  RawRead -> unimplemented
  RawSelect -> unimplemented
  RawUpdate -> unimplemented
  RawWithDefaultRead -> unimplemented
  RawWithRead -> unimplemented
  RawWrite -> unimplemented
