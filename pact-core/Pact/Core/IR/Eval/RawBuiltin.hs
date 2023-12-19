{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiWayIf #-}

module Pact.Core.IR.Eval.RawBuiltin
 ( rawBuiltinRuntime
 , rawBuiltinEnv
 , coreEnforceGuard) where

-- |
-- Module      :  Pact.Core.Eval.RawBuiltin
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
import Data.Containers.ListUtils(nubOrd)
import Data.Bits
import Data.Either(isLeft, isRight)
import Data.Foldable(foldl', traverse_, toList)
import Data.Decimal(roundTo', Decimal, DecimalRaw(..))
import Data.Vector(Vector)
import Data.Maybe(isJust, maybeToList)
import Numeric(showIntAtBase)
import qualified Control.Lens as Lens
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as V
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Char as Char
import qualified Data.ByteString as BS
import qualified GHC.Exts as Exts
import qualified Pact.Time as PactTime
import qualified Data.Poly as Poly

import Pact.Core.Builtin
import Pact.Core.Literal
import Pact.Core.Errors
import Pact.Core.Hash
import Pact.Core.Names
import Pact.Core.Guards
import Pact.Core.Type
import Pact.Core.PactValue
import Pact.Core.Persistence
import Pact.Core.DefPacts.Types
import Pact.Core.Environment
import Pact.Core.Capabilities
import Pact.Core.Namespace
import Pact.Core.Gas
import Pact.Core.Crypto.Pairing
import Pact.Core.Crypto.Hash.Poseidon

import Pact.Core.IR.Term
import Pact.Core.IR.Eval.Runtime
import Pact.Core.StableEncoding
import Pact.Core.IR.Eval.CEK

import qualified Pact.Core.Pretty as Pretty
import qualified Pact.Core.Principal as Pr
import qualified Pact.Core.Trans.TOps as Musl


----------------------------------------------------------------------
-- Our builtin definitions start here
----------------------------------------------------------------------

unaryIntFn :: (IsBuiltin b, MonadEval b i m) => (Integer -> Integer) -> NativeFunction b i m
unaryIntFn op info b cont handler _env = \case
  [VLiteral (LInteger i)] -> returnCEKValue cont handler (VLiteral (LInteger (op i)))
  args -> argsError info b args
{-# INLINE unaryIntFn #-}

binaryIntFn
  :: (IsBuiltin b, MonadEval b i m)
  => (Integer -> Integer -> Integer)
  -> NativeFunction b i m
binaryIntFn op info b cont handler _env = \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> returnCEKValue cont handler (VLiteral (LInteger (op i i')))
  args -> argsError info b args
{-# INLINE binaryIntFn #-}

roundingFn :: (IsBuiltin b, MonadEval b i m) => (Rational -> Integer) -> NativeFunction b i m
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
rawAdd :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
rawAdd info b cont handler _env = \case
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
rawSub info b cont handler _env = \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> returnCEKValue cont handler (VLiteral (LInteger (i - i')))
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> returnCEKValue cont handler (VLiteral (LDecimal (i - i')))
  args -> argsError info b args

rawMul :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
rawMul info b cont handler _env = \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> returnCEKValue cont handler (VLiteral (LInteger (i * i')))
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> returnCEKValue cont handler (VLiteral (LDecimal (i * i')))
  args -> argsError info b args

rawPow :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
rawPow info b cont handler _env = \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> do
    when (i' < 0) $ throwExecutionError info (ArithmeticException "negative exponent in integer power")
    -- Todo: move to iterated pow
    returnCEKValue cont handler (VLiteral (LInteger (i ^ i')))
  [VLiteral (LDecimal l), VLiteral (LDecimal r)] -> do
    let result = Musl.trans_pow (dec2F l) (dec2F r)
    guardNanOrInf info result
    returnCEKValue cont handler (VLiteral (LDecimal (f2Dec result)))
  args -> argsError info b args

rawLogBase :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
rawLogBase info b cont handler _env = \case
  [VLiteral (LInteger base), VLiteral (LInteger n)] -> do
    when (base < 0 || n <= 0) $ throwExecutionError info (ArithmeticException "Illegal log base")
    let base' = fromIntegral base :: Double
        n' = fromIntegral n
        result = Musl.trans_logBase base' n'
    guardNanOrInf info result
    returnCEKValue cont handler (VLiteral (LInteger (round result)))
    -- if i' == 0 then throwExecutionError' (ArithmeticException "div by zero")
    -- else returnCEKValue cont handler (VLiteral (LInteger (div i i')))
  [VLiteral (LDecimal base), VLiteral (LDecimal arg)] -> do
    when (base < 0 || arg <= 0) $ throwExecutionError info (ArithmeticException "Invalid base or argument in log")
    let result = Musl.trans_logBase (dec2F base) (dec2F arg)
    guardNanOrInf info result
    returnCEKValue cont handler (VLiteral (LDecimal (f2Dec result)))
  args -> argsError info b args

rawDiv :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
rawDiv info b cont handler _env = \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] ->
    if i' == 0 then throwExecutionError info (ArithmeticException "div by zero")
    else returnCEKValue cont handler (VLiteral (LInteger (div i i')))
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] ->
    if i' == 0 then throwExecutionError info (ArithmeticException "div by zero, decimal")
    else returnCEKValue cont handler (VLiteral (LDecimal (i / i')))
  args -> argsError info b args

rawNegate :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
rawNegate info b cont handler _env = \case
  [VLiteral (LInteger i)] ->
    returnCEKValue cont handler (VLiteral (LInteger (negate i)))
  [VLiteral (LDecimal i)] ->
    returnCEKValue cont handler (VLiteral (LDecimal (negate i)))
  args -> argsError info b args

rawEq :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
rawEq info b cont handler _env = \case
  -- Todo: rawEqGas
  [VPactValue pv, VPactValue pv'] -> returnCEKValue cont handler (VBool (pv == pv'))
  args -> argsError info b args

modInt :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
modInt = binaryIntFn mod

rawNeq :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
rawNeq info b cont handler _env = \case
  [VPactValue pv, VPactValue pv'] ->
    returnCEKValue cont handler (VBool (pv /= pv'))
  args -> argsError info b args

rawGt :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
rawGt info b cont handler _env = \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> returnCEKValue cont handler (VLiteral (LBool (i > i')))
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> returnCEKValue cont handler (VLiteral (LBool (i > i')))
  [VLiteral (LString i), VLiteral (LString i')] -> returnCEKValue cont handler (VLiteral (LBool (i > i')))
  [VTime i, VTime i'] -> returnCEKValue cont handler (VLiteral (LBool (i > i')))
  args -> argsError info b args

rawLt :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
rawLt info b cont handler _env = \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> returnCEKValue cont handler (VLiteral (LBool (i < i')))
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> returnCEKValue cont handler (VLiteral (LBool (i < i')))
  [VLiteral (LString i), VLiteral (LString i')] -> returnCEKValue cont handler (VLiteral (LBool (i < i')))
  [VTime i, VTime i'] -> returnCEKValue cont handler (VLiteral (LBool (i < i')))
  args -> argsError info b args

rawGeq :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
rawGeq info b cont handler _env = \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> returnCEKValue cont handler (VLiteral (LBool (i >= i')))
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> returnCEKValue cont handler (VLiteral (LBool (i >= i')))
  [VLiteral (LString i), VLiteral (LString i')] -> returnCEKValue cont handler (VLiteral (LBool (i >= i')))
  [VTime i, VTime i'] -> returnCEKValue cont handler (VLiteral (LBool (i >= i')))
  args -> argsError info b args

rawLeq :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
rawLeq info b cont handler _env = \case
  [VLiteral (LInteger i), VLiteral (LInteger i')] -> returnCEKValue cont handler (VLiteral (LBool (i <= i')))
  [VLiteral (LDecimal i), VLiteral (LDecimal i')] -> returnCEKValue cont handler (VLiteral (LBool (i <= i')))
  [VLiteral (LString i), VLiteral (LString i')] -> returnCEKValue cont handler (VLiteral (LBool (i <= i')))
  [VTime i, VTime i'] -> returnCEKValue cont handler (VLiteral (LBool (i <= i')))
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
rawAbs info b cont handler _env = \case
  [VLiteral (LInteger i)] ->
    returnCEKValue cont handler (VLiteral (LInteger (abs i)))
  [VLiteral (LDecimal e)] -> do
    returnCEKValue cont handler (VLiteral (LDecimal (abs e)))
  args -> argsError info b args

rawExp :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
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

rawLn :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
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

rawSqrt :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
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

-- Todo: fix all show instances
rawShow :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
rawShow info b cont handler _env = \case
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

-- Todo: Gas here is complicated, greg worked on this previously
rawContains :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
rawContains info b cont handler _env = \case
  [VString f, VObject o] ->
    returnCEKValue cont handler (VBool (M.member (Field f) o))
  [VString s, VString s'] ->
    returnCEKValue cont handler (VBool (s `T.isInfixOf` s'))
  [VPactValue v, VList vli] ->
    returnCEKValue cont handler (VBool (v `V.elem` vli))
  args -> argsError info b args

rawSort :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
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

coreRemove :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
coreRemove info b cont handler _env = \case
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
notBool info b cont handler _env = \case
  [VLiteral (LBool i)] -> returnCEKValue cont handler  (VLiteral (LBool (not i)))
  args -> argsError info b args

---------------------------
-- string ops
---------------------------

rawTake :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
rawTake info b cont handler _env = \case
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
rawDrop info b cont handler _env = \case
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
rawLength info b cont handler _env = \case
  [VString t] -> do
    returnCEKValue cont handler  (VLiteral (LInteger (fromIntegral (T.length t))))
  [VList li] -> returnCEKValue cont handler (VLiteral (LInteger (fromIntegral (V.length li))))
  [VObject o] ->
    returnCEKValue cont handler $ VInteger $ fromIntegral (M.size o)
  args -> argsError info b args

rawReverse :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
rawReverse info b cont handler _env = \case
  [VList li] ->
    returnCEKValue cont handler (VList (V.reverse li))
  [VLiteral (LString t)] -> do
    returnCEKValue cont handler  (VLiteral (LString (T.reverse t)))
  args -> argsError info b args

coreConcat :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
coreConcat info b cont handler _env = \case
  [VList li] -> do
    li' <- traverse (asString info b) li
    returnCEKValue cont handler (VString (T.concat (V.toList li')))
  args -> argsError info b args

strToList :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
strToList info b cont handler _env = \case
  [VLiteral (LString s)] -> do
    let v = VList (V.fromList (PLiteral . LString . T.singleton <$> T.unpack s))
    returnCEKValue cont handler v
  args -> argsError info b args


zipList :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
zipList info b cont handler _env = \case
  [VClosure clo, VList l, VList r] -> zip' (V.toList l) (V.toList r) []
    where
    zip' (x:xs) (y:ys) acc = unsafeApplyTwo clo (VPactValue x) (VPactValue y) >>= \case
       EvalValue v -> enforcePactValue info v >>= zip' xs ys . (:acc)
       v@VError{} -> returnCEK cont handler v
    zip' _ _ acc = returnCEKValue cont handler (VList (V.fromList (reverse acc)))
  args -> argsError info b args

-- (try [1] (map (+ 1) [1 2 (enforce false "greg")])
coreMap :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
coreMap info b cont handler _env = \case
  [VClosure fn, VList li] -> do
    map' (V.toList li) []
    where
    map' (x:xs) acc = applyLam fn [VPactValue x] Mt CEKNoHandler >>= \case
       EvalValue cv -> enforcePactValue info cv >>= map' xs . (:acc)
       v@VError{} -> returnCEK cont handler v
    map' _ acc = returnCEKValue cont handler (VList (V.fromList (reverse acc)))
  args -> argsError info b args

coreFilter :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
coreFilter info b cont handler _env = \case
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
coreFold info b cont handler _env = \case
  [VClosure fn, initElem, VList li] ->
    fold' initElem (V.toList li)
    where
    fold' e (x:xs) = unsafeApplyTwo fn e (VPactValue x) >>= \case
      EvalValue v -> fold' v xs
      v -> returnCEK cont handler v
    fold' e [] = returnCEKValue cont handler e
  args -> argsError info b args

coreEnumerate :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
coreEnumerate info b cont handler _env = \case
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
coreEnumerateStepN info b cont handler _env = \case
  [VLiteral (LInteger from), VLiteral (LInteger to), VLiteral (LInteger inc)] -> do
    v <- createEnumerateList info from to inc
    returnCEKValue cont handler (VList (PLiteral . LInteger <$> v))
  args -> argsError info b args

makeList :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
makeList info b cont handler _env = \case
  [VLiteral (LInteger i), VPactValue v] -> do
    returnCEKValue cont handler (VList (V.fromList (replicate (fromIntegral i) v)))
  args -> argsError info b args

coreAccess :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
coreAccess info b cont handler _env = \case
  [VLiteral (LInteger i), VList vec] ->
    case vec V.!? fromIntegral i of
      Just v -> returnCEKValue cont handler (VPactValue v)
      _ -> throwExecutionError info (ArrayOutOfBoundsException (V.length vec) (fromIntegral i))
  [VString field, VObject o] ->
    case M.lookup (Field field) o of
      Just v -> returnCEKValue cont handler (VPactValue v)
      Nothing ->
        let msg = "Object does not have field: " <> field
        in returnCEK cont handler (VError msg info)
  args -> argsError info b args

coreIsCharset :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
coreIsCharset info b cont handler _env = \case
  [VLiteral (LInteger i), VString s] ->
    case i of
      0 -> returnCEKValue cont handler $ VBool $ T.all Char.isAscii s
      1 -> returnCEKValue cont handler $ VBool $ T.all Char.isLatin1 s
      _ -> returnCEK cont handler (VError "Unsupported character set" info)
  args -> argsError info b args

coreYield :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
coreYield info b cont handler _env = \case
  [VObject o] -> go o Nothing
  [VObject o, VString cid] -> go o (Just (ChainId cid))
  args -> argsError info b args
  where
  go o mcid = do
    mpe <- useEvalState esDefPactExec
    case mpe of
      Nothing -> throwExecutionError info YieldOutsiteDefPact
      Just pe -> case mcid of
        Nothing -> do
          esDefPactExec . _Just . peYield .== Just (Yield o Nothing Nothing)
          returnCEKValue cont handler (VObject o)
        Just cid -> do
          sourceChain <- viewEvalEnv (eePublicData . pdPublicMeta . pmChainId)
          p <- provenanceOf cid
          when (_peStepHasRollback pe) $ failInvariant info "Cross-chain yield not allowed in step with rollback"
          esDefPactExec . _Just . peYield .== Just (Yield o (Just p) (Just sourceChain))
          returnCEKValue cont handler (VObject o)
  provenanceOf tid =
    Provenance tid . _mHash <$> getCallingModule info

corePactId :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
corePactId info b cont handler _env = \case
  [] -> useEvalState esDefPactExec >>= \case
    Just dpe -> returnCEKValue cont handler (VString (_defpactId (_peDefPactId dpe)))
    Nothing -> returnCEK cont handler (VError "pact-id: not in pact execution" info)
  args -> argsError info b args

coreResume :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
coreResume info b cont handler _env = \case
  [VClosure clo] -> do
    mps <- viewEvalEnv eeDefPactStep
    case mps of
      Nothing -> throwExecutionError info NoActiveDefPactExec
      Just pactStep -> case _psResume pactStep of
        Nothing -> throwExecutionError info (NoYieldInDefPactStep pactStep)
        Just (Yield resumeObj _ _) -> applyLam clo [VObject resumeObj] cont handler
  args -> argsError info b args

-----------------------------------
-- try-related ops
-----------------------------------

enforceTopLevelOnly :: (IsBuiltin b, MonadEval b i m) => i -> b -> m ()
enforceTopLevelOnly info b = do
  s <- useEvalState esStack
  unless (null s) $ throwExecutionError info (NativeIsTopLevelOnly (builtinName b))

-----------------------------------
-- Guards and reads
-----------------------------------


-----------------------------------
-- Other Core forms
-----------------------------------

coreB64Encode :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
coreB64Encode info b cont handler _env = \case
  [VLiteral (LString l)] ->
    returnCEKValue cont handler $ VLiteral $ LString $ toB64UrlUnpaddedText $ T.encodeUtf8 l
  args -> argsError info b args


coreB64Decode :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
coreB64Decode info b cont handler _env = \case
  [VLiteral (LString s)] -> case fromB64UrlUnpaddedText $ T.encodeUtf8 s of
    Left{} -> throwExecutionError info (DecodeError "invalid b64 encoding")
    Right txt -> returnCEKValue cont handler (VLiteral (LString txt))
  args -> argsError info b args

-- | The main logic of enforcing a guard.
--
-- The main difference to `coreEnforceGuard` is this function's type doesn't need to be a `NativeFunction b i m`,
-- thus there's no need to wrap/unwrap the guard into a `VPactValue`,
-- and moreover it does not need to take a `b` which it does not use anyway.
enforceGuard
  :: (MonadEval b i m)
  => i
  -> Cont b i m
  -> CEKErrorHandler b i m
  -> CEKEnv b i m
  -> Guard QualifiedName PactValue
  -> m (EvalResult b i m)
enforceGuard info cont handler env g = case g of
  GKeyset ks -> do
    cond <- isKeysetInSigs ks
    if cond then returnCEKValue cont handler (VBool True)
    else returnCEK cont handler (VError "enforce keyset failure" info)
  GKeySetRef ksn -> do
    cond <- isKeysetNameInSigs info (view cePactDb env) ksn
    if cond then returnCEKValue cont handler (VBool True)
    else returnCEK cont handler (VError "enforce keyset ref failure" info)
  GUserGuard ug -> runUserGuard info cont handler env ug
  GCapabilityGuard cg -> enforceCapGuard info cont handler cg
  GModuleGuard (ModuleGuard mn _) -> calledByModule mn >>= \case
    True -> returnCEKValue cont handler (VBool True)
    False -> do
      md <- getModule info (view cePactDb env) mn
      acquireModuleAdmin info env md
      returnCEKValue cont handler (VBool True)
  GDefPactGuard (DefPactGuard dpid _) -> do
    curDpid <- getDefPactId info
    if curDpid == dpid
       then returnCEKValue cont handler (VBool True)
       else returnCEK cont handler (VError "Capability pact guard failed: invalid pact id" info)

-- | A version of `enforceGuard` that also accepts a continuation
-- that gets called if the guard is enforced successfully.
enforceGuardCont
  :: (MonadEval b i m)
  => i
  -> Cont b i m
  -> CEKErrorHandler b i m
  -> CEKEnv b i m
  -> Guard QualifiedName PactValue
  -> m (EvalResult b i m)
  -> m (EvalResult b i m)
enforceGuardCont info cekCont cekHandler env g successCont =
  enforceGuard info Mt CEKNoHandler env g >>= \case
    EvalValue {} -> successCont
    VError e i -> returnCEK cekCont cekHandler (VError e i)

-- | The implementation of `enforce-guard` native.
coreEnforceGuard :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
coreEnforceGuard info b cont handler env = \case
  [VGuard g] -> enforceGuard info cont handler env g
  [VString s] -> case parseAnyKeysetName s of
      Left {} -> returnCEK cont handler (VError "incorrect keyset name format" info)
      Right ksn -> do
        cond <- isKeysetNameInSigs info (view cePactDb env) ksn
        if cond
          then returnCEKValue cont handler (VBool True)
          else returnCEK cont handler (VError "enforce keyset ref failure" info)
  args -> argsError info b args

keysetRefGuard :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
keysetRefGuard info b cont handler env = \case
  [VString g] -> case parseAnyKeysetName g of
    Left {} -> returnCEK cont handler (VError "incorrect keyset name format" info)
    Right ksn -> do
      let pdb = view cePactDb env
      liftDbFunction info (readKeySet pdb ksn) >>= \case
        Nothing -> returnCEK cont handler (VError ("no such keyset defined: " <> g) info)
        Just _ -> returnCEKValue cont handler (VGuard (GKeySetRef ksn))
  args -> argsError info b args

coreTypeOf :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
coreTypeOf info b cont handler _env = \case
  [v] -> case v of
    VPactValue pv ->
      returnCEKValue cont handler $ VString $ renderType $ synthesizePvType pv
    VClosure _ -> returnCEKValue cont handler $ VString "<<closure>>"
    VTable tv -> returnCEKValue cont handler $ VString (renderType (TyTable (_tvSchema tv)))
  args -> argsError info b args

coreDec :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
coreDec info b cont handler _env = \case
  [VInteger i] -> returnCEKValue cont handler $ VDecimal $ Decimal 0 i
  args -> argsError info b args

coreReadInteger :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
coreReadInteger info b cont handler _env = \case
  [VString s] -> do
    ObjectData envData <- viewEvalEnv eeMsgBody
    case M.lookup (Field s) envData of
      Just (PInteger p) -> returnCEKValue cont handler (VInteger p)
      _ -> returnCEK cont handler (VError "read-integer failure" info)
  args -> argsError info b args

coreReadMsg :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
coreReadMsg info b cont handler _env = \case
  [VString s] -> do
    ObjectData envData <- viewEvalEnv eeMsgBody
    case M.lookup (Field s) envData of
      Just pv -> returnCEKValue cont handler (VPactValue pv)
      _ -> returnCEK cont handler (VError "read-integer failure" info)
  [] -> do
    ObjectData envData <- viewEvalEnv eeMsgBody
    returnCEKValue cont handler (VObject envData)
  args -> argsError info b args

coreReadDecimal :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
coreReadDecimal info b cont handler _env = \case
  [VString s] -> do
    ObjectData envData <- viewEvalEnv eeMsgBody
    case M.lookup (Field s) envData of
      Just (PDecimal p) -> returnCEKValue cont handler (VDecimal p)
      _ -> returnCEK cont handler (VError "read-integer failure" info)
  args -> argsError info b args

coreReadString :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
coreReadString info b cont handler _env = \case
  [VString s] -> do
    ObjectData envData <- viewEvalEnv eeMsgBody
    case M.lookup (Field s) envData of
      Just (PString p) -> returnCEKValue cont handler (VString p)
      _ -> returnCEK cont handler (VError "read-integer failure" info)
  args -> argsError info b args

readKeyset' :: (MonadEval b i m) => T.Text -> m (Maybe (KeySet QualifiedName))
readKeyset' ksn = do
    ObjectData envData <- viewEvalEnv eeMsgBody
    case M.lookup (Field ksn) envData of
      Just (PObject dat) -> parseObj dat
        where
        parseObj d = pure $ do
          keys <- M.lookup (Field "keys") d
          keyText <- preview _PList keys >>= traverse (fmap PublicKeyText . preview (_PLiteral . _LString))
          predRaw <- M.lookup (Field "pred") d
          p <- preview (_PLiteral . _LString) predRaw
          pred' <- readPredicate p
          let ks = S.fromList (V.toList keyText)
          pure (KeySet ks pred')
        readPredicate = \case
          "keys-any" -> pure KeysAny
          "keys-2" -> pure Keys2
          "keys-all" -> pure KeysAll
          _ -> Nothing
      Just (PList li) ->
        case parseKeyList li of
          Just ks -> pure (Just (KeySet ks KeysAll))
          Nothing -> pure Nothing
        where
        parseKeyList d =
          S.fromList . V.toList . fmap PublicKeyText <$> traverse (preview (_PLiteral . _LString)) d
      _ -> pure Nothing


coreReadKeyset :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
coreReadKeyset info b cont handler _env = \case
  [VString ksn] ->
    readKeyset' ksn >>= \case
      Just ks -> do
        shouldEnforce <- isExecutionFlagSet FlagEnforceKeyFormats
        if shouldEnforce && isLeft (enforceKeyFormats (const ()) ks)
           then returnCEK cont handler (VError "Invalid keyset" info)
           else returnCEKValue cont handler (VGuard (GKeyset ks))
      Nothing -> returnCEK cont handler (VError "read-keyset failure" info)
  args -> argsError info b args

enforceCapGuard
  :: MonadEval b i m
  => i
  -> Cont b i m
  -> CEKErrorHandler b i m
  -> CapabilityGuard QualifiedName PactValue
  -> m (EvalResult b i m)
enforceCapGuard info cont handler (CapabilityGuard qn args mpid) = case mpid of
  Nothing -> enforceCap
  Just pid -> do
    currPid <- getDefPactId info
    if currPid == pid then enforceCap
    else returnCEK cont handler (VError "Capability pact guard failed: invalid pact id" info)
  where
  enforceCap = do
    cond <- isCapInStack (CapToken qn args)
    if cond then returnCEKValue cont handler (VBool True)
    else do
      let errMsg = "Capability guard enforce failure cap not in scope: " <> renderQualName qn
      returnCEK cont handler (VError errMsg info)

runUserGuard
  :: MonadEval b i m
  => i
  -> Cont b i m
  -> CEKErrorHandler b i m
  -> CEKEnv b i m
  -> UserGuard QualifiedName PactValue
  -> m (EvalResult b i m)
runUserGuard info cont handler env (UserGuard qn args) =
  getModuleMember info (_cePactDb env) qn >>= \case
    Dfun d -> do
      when (length (_dfunArgs d) /= length args) $ throwExecutionError info CannotApplyPartialClosure
      let env' = sysOnlyEnv env
      clo <- mkDefunClosure d (_qnModName qn) env'
      -- Todo: sys only here
      applyLam (C clo) (VPactValue <$> args) (UserGuardC cont) handler
    d -> throwExecutionError info (InvalidDefKind (defKind d) "run-user-guard")

coreBind :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
coreBind info b cont handler _env = \case
  [v@VObject{}, VClosure clo] ->
    applyLam clo [v] cont handler
  args -> argsError info b args


--------------------------------------------------
-- Db functions
--------------------------------------------------

createTable :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
createTable info b cont handler env = \case
  [VTable tv@(TableValue tn _ _)] -> do
    enforceTopLevelOnly info b
    guardTable info env tv GtCreateTable
    let pdb = view cePactDb env
    -- Todo: error handling here
    liftDbFunction info (_pdbCreateUserTable pdb tn)
    returnCEKValue cont handler VUnit
  args -> argsError info b args

dbSelect :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
dbSelect info b cont handler env = \case
  [VTable tv, VClosure clo] -> do
    let pdb = view cePactDb env
    guardTable info env tv GtSelect
    ks <- liftDbFunction info (_pdbKeys pdb (tvToDomain tv))
    go Nothing clo tv pdb ks []
  [VTable tv, VList li, VClosure clo] -> do
    let pdb = view cePactDb env
    guardTable info env tv GtSelect
    li' <- traverse (fmap Field . asString info b) (V.toList li)
    ks <- liftDbFunction info (_pdbKeys pdb (tvToDomain tv))
    go (Just li') clo tv pdb ks []
  args -> argsError info b args
  where
    go mf _clo _tv _ [] acc = case mf of
      Just fields -> do
        let acc' = PObject . (`M.restrictKeys` S.fromList fields) <$> reverse acc
        returnCEKValue cont handler (VList (V.fromList acc'))
      Nothing ->
        returnCEKValue cont handler (VList (V.fromList (fmap PObject (reverse acc))))
    go mf clo tv pdb (k:ks) acc = liftDbFunction info (_pdbRead pdb (tvToDomain tv) k) >>= \case
      Just (RowData rdata) -> applyLam clo [VObject rdata] Mt CEKNoHandler >>= \case
        EvalValue (VBool cond) ->
          if cond then go mf clo tv pdb ks (rdata:acc) else go mf clo tv pdb ks acc
        EvalValue _ -> returnCEK cont handler (VError "select query error" info)
        VError e i -> returnCEK cont handler (VError e i)
      Nothing -> returnCEK cont handler (VError "select is not enabled" info)

-- Todo: error handling
foldDb :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
foldDb info b cont handler env = \case
  [VTable tv, VClosure queryClo, VClosure consumer] -> do
    let pdb = view cePactDb env
    guardTable info env tv GtSelect
    let tblDomain = DUserTables (_tvName tv)
    keys <- liftDbFunction info (_pdbKeys pdb tblDomain)
    go pdb [] keys
    where
      -- todo: weird key invariant
      go pdb acc (rk@(RowKey k):ks) = do
        liftDbFunction info (_pdbRead pdb (DUserTables (_tvName tv)) rk) >>= \case
          Just (RowData row) -> do
            applyLam queryClo [VString k, VObject row] Mt CEKNoHandler >>= \case
              EvalValue (VBool qry) -> if qry then do
                applyLam consumer [VString k, VObject row] Mt CEKNoHandler >>= \case
                  EvalValue (VPactValue v) -> go pdb (v:acc) ks
                  EvalValue _ ->
                    returnCEK cont handler (VError "Fold db did not return a pact value" info)
                  v -> returnCEK cont handler v
                else go pdb acc ks
              EvalValue _ ->
                returnCEK cont handler (VError "Fold db did not return a pact value" info)
              v@VError{} -> returnCEK cont handler v
          Nothing -> error "no key despite keys"
      go _ acc [] =
        returnCEKValue cont handler (VList (V.fromList (reverse acc)))
  args -> argsError info b args

dbRead :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
dbRead info b cont handler env = \case
  [VTable tv, VString k] -> do
    let pdb = view cePactDb env
    guardTable info env tv GtRead
    liftDbFunction info (_pdbRead pdb (DUserTables (_tvName tv)) (RowKey k)) >>= \case
      Just (RowData v) -> returnCEKValue cont handler (VObject v)
      Nothing -> returnCEK cont handler (VError "no such read object" info)
  args -> argsError info b args

dbWithRead :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
dbWithRead info b cont handler env = \case
  [VTable tv, VString k, VClosure clo] -> do
    let pdb = view cePactDb env
    guardTable info env tv GtWithRead
    liftDbFunction info (_pdbRead pdb (DUserTables (_tvName tv)) (RowKey k)) >>= \case
      Just (RowData v) -> applyLam clo [VObject v] cont handler
      Nothing -> returnCEK cont handler (VError "no such read object" info)
  args -> argsError info b args

dbWithDefaultRead :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
dbWithDefaultRead info b cont handler env = \case
  [VTable tv, VString k, VObject defaultObj, VClosure clo] -> do
    let pdb = view cePactDb env
    guardTable info env tv GtWithDefaultRead
    liftDbFunction info (_pdbRead pdb (DUserTables (_tvName tv)) (RowKey k)) >>= \case
      Just (RowData v) -> applyLam clo [VObject v] cont handler
      Nothing -> applyLam clo [VObject defaultObj] cont handler
  args -> argsError info b args

-- | Todo: schema checking here? Or only on writes?
dbWrite :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
dbWrite = write' Write

dbInsert :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
dbInsert = write' Insert

write' :: (IsBuiltin b, MonadEval b i m) => WriteType -> NativeFunction b i m
write' wt info b cont handler env = \case
  [VTable tv, VString key, VObject o] -> do
    guardTable info env tv GtWrite
    if checkSchema o (_tvSchema tv) then do
        let pdb = view cePactDb env
        let rowData = RowData o
        liftDbFunction info (_pdbWrite pdb wt (tvToDomain tv) (RowKey key) rowData)
        returnCEKValue cont handler (VString "Write succeeded")
    else
        returnCEK cont handler (VError "object does not match schema" info)
  args -> argsError info b args

dbUpdate :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
dbUpdate info b cont handler env = \case
  [VTable tv, VString key, VObject o] -> do
    guardTable info env tv GtWrite
    if checkPartialSchema o (_tvSchema tv) then do
        let pdb = view cePactDb env
        let rowData = RowData o
        liftDbFunction info (_pdbWrite pdb Update (tvToDomain tv) (RowKey key) rowData)
        returnCEKValue cont handler (VString "Write succeeded")
    else returnCEK cont handler (VError "object does not match schema" info)
  args -> argsError info b args

dbKeys :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
dbKeys info b cont handler env = \case
  [VTable tv] -> do
    guardTable info env tv GtKeys
    let pdb = view cePactDb env
    ks <- liftDbFunction info (_pdbKeys pdb (tvToDomain tv))
    let li = V.fromList (PString . _rowKey <$> ks)
    returnCEKValue cont handler (VList li)
  args -> argsError info b args

dbTxIds :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
dbTxIds info b cont handler env = \case
  [VTable tv, VInteger tid] -> do
    checkNonLocalAllowed info
    guardTable info env tv GtTxIds
    let pdb = view cePactDb env
    ks <- liftDbFunction info (_pdbTxIds pdb (_tvName tv) (TxId (fromIntegral tid)))
    let li = V.fromList (PInteger . fromIntegral . _txId <$> ks)
    returnCEKValue cont handler (VList li)
  args -> argsError info b args


dbTxLog :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
dbTxLog info b cont handler env = \case
  [VTable tv, VInteger tid] -> do
    checkNonLocalAllowed info
    guardTable info env tv GtTxLog
    let pdb = view cePactDb env
        txId = TxId (fromInteger tid)
    ks <- liftDbFunction info (_pdbGetTxLog pdb (_tvName tv) txId)
    let li = V.fromList (txLogToObj <$> ks)
    returnCEKValue cont handler (VList li)
    where
    txLogToObj (TxLog domain key (RowData v)) = do
      PObject $ M.fromList
        [ (Field "table", PString domain)
        , (Field "key", PString key)
        , (Field "value", PObject v)]
  args -> argsError info b args

dbKeyLog :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
dbKeyLog info b cont handler env = \case
  [VTable tv, VString key, VInteger tid] -> do
    checkNonLocalAllowed info
    guardTable info env tv GtKeyLog
    let pdb = view cePactDb env
        txId = TxId (fromInteger tid)
    ids <- liftDbFunction info (_pdbTxIds pdb (_tvName tv) txId)
    ks <- concat <$> traverse (\t -> fmap (t,) <$> liftDbFunction info (_pdbGetTxLog pdb (_tvName tv) t)) ids
    let ks' = filter (\(_, txl) -> _txKey txl == key) ks
    let li = V.fromList (txLogToObj <$> ks')
    returnCEKValue cont handler (VList li)
    where
    txLogToObj (TxId txid, TxLog _domain _key (RowData v)) = do
      PObject $ M.fromList
        [ (Field "txid", PInteger (fromIntegral txid))
        , (Field "value", PObject v)]
  args -> argsError info b args

tvToDomain :: TableValue -> Domain RowKey RowData b i
tvToDomain tv =
  DUserTables (_tvName tv)

-- | Todo: isProperSubmapOf
checkSchema :: M.Map Field PactValue -> Schema -> Bool
checkSchema o (Schema sc) = isJust $ do
  let keys = M.keys o
  when (keys /= M.keys sc) Nothing
  traverse_ go (M.toList o)
  where
  go (k, v) = M.lookup k sc >>= (`checkPvType` v)

checkPartialSchema :: M.Map Field PactValue -> Schema -> Bool
checkPartialSchema o (Schema sc) =
  M.isSubmapOfBy (\obj ty -> isJust (checkPvType ty obj)) o sc

defineKeySet'
  :: (MonadEval b i m)
  => i
  -> Cont b i m
  -> CEKErrorHandler b i m
  -> CEKEnv b i m
  -> T.Text
  -> KeySet QualifiedName
  -> m (EvalResult b i m)
defineKeySet' info cont handler env ksname newKs  = do
  let pdb = view cePactDb env
  ignoreNamespaces <- not <$> isExecutionFlagSet FlagRequireKeysetNs
  case parseAnyKeysetName ksname of
    Left {} -> returnCEK cont handler (VError "incorrect keyset name format" info)
    Right ksn -> do
      let writeKs = do
            liftDbFunction info (writeKeySet pdb Write ksn newKs)
            returnCEKValue cont handler (VString "Keyset write success")
      liftDbFunction info (readKeySet pdb ksn) >>= \case
        Just oldKs -> do
          cond <- isKeysetInSigs oldKs
          if cond then writeKs
          else returnCEK cont handler (VError "enforce keyset failure" info)
        Nothing | ignoreNamespaces -> writeKs
        Nothing | otherwise -> useEvalState (esLoaded . loNamespace) >>= \case
          Nothing -> returnCEK cont handler (VError "Cannot define a keyset outside of a namespace" info)
          Just (Namespace ns uGuard _adminGuard) -> do
            enforceGuardCont info cont handler env uGuard $
              if Just ns == _keysetNs ksn
                then writeKs
                else returnCEK cont handler (VError "Mismatching keyset namespace" info)

defineKeySet :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
defineKeySet info b cont handler env = \case
  [VString ksname, VGuard (GKeyset ks)] -> do
    enforceTopLevelOnly info b
    defineKeySet' info cont handler env ksname ks
  [VString ksname] -> do
    enforceTopLevelOnly info b
    readKeyset' ksname >>= \case
      Just newKs ->
        defineKeySet' info cont handler env ksname newKs
      Nothing -> returnCEK cont handler (VError "read-keyset failure" info)
  args -> argsError info b args

--------------------------------------------------
-- Capabilities
--------------------------------------------------

requireCapability :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
requireCapability info b cont handler _env = \case
  [VCapToken ct] -> requireCap info cont handler ct
  args -> argsError info b args

composeCapability :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
composeCapability info b cont handler env = \case
  [VCapToken ct] ->
    useEvalState esStack >>= \case
      sf:_ -> do
        when (_sfFnType sf /= SFDefcap) $ failInvariant info "compose-cap"
        composeCap info cont handler env ct
      _ -> failInvariant info "compose-cap at the top level"
  args -> argsError info b args

installCapability :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
installCapability info b cont handler env = \case
  [VCapToken ct] -> do
    enforceNotWithinDefcap info env "install-capability"
    _ <- installCap info env ct True
    returnCEKValue cont handler (VString "Installed capability")
  args -> argsError info b args

coreEmitEvent :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
coreEmitEvent info b cont handler env = \case
  [VCapToken ct@(CapToken fqn _)] -> do
    guardForModuleCall info env (_fqModule fqn) $ return ()
    lookupFqName (_ctName ct) >>= \case
      Just (DCap d) -> do
        enforceMeta (_dcapMeta d)
        emitCapability info ct
        returnCEKValue cont handler (VBool True)
      Just _ ->
        failInvariant info "CapToken does not point to defcap"
      _ -> failInvariant info "No Capability found in emit-event"
      where
      enforceMeta Unmanaged = throwExecutionError info (InvalidEventCap fqn)
      enforceMeta _ = pure ()
  args -> argsError info b args

createCapGuard :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
createCapGuard info b cont handler _env = \case
  [VCapToken ct] -> do
    let qn = fqnToQualName (_ctName ct)
        cg = CapabilityGuard qn (_ctArgs ct) Nothing
    returnCEKValue cont handler (VGuard (GCapabilityGuard cg))
  args -> argsError info b args

createCapabilityPactGuard :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
createCapabilityPactGuard info b cont handler _env = \case
  [VCapToken ct] -> do
    pid <- getDefPactId info
    let qn = fqnToQualName (_ctName ct)
    let cg = CapabilityGuard qn (_ctArgs ct) (Just pid)
    returnCEKValue cont handler (VGuard (GCapabilityGuard cg))
  args -> argsError info b args

createModuleGuard :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
createModuleGuard info b cont handler _env = \case
  [VString n] ->
    findCallingModule >>= \case
      Just mn ->  do
        let cg = GModuleGuard (ModuleGuard mn n)
        returnCEKValue cont handler (VGuard cg)
      Nothing ->
        returnCEK cont handler (VError "not-in-module" info)
  args -> argsError info b args

createDefPactGuard :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
createDefPactGuard info b cont handler _env = \case
  [VString name] -> do
    dpid <- getDefPactId info
    returnCEKValue cont handler $ VGuard $ GDefPactGuard $ DefPactGuard dpid name
  args -> argsError info b args


coreIntToStr :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
coreIntToStr info b cont handler _env = \case
  [VInteger base, VInteger v]
    | base >= 2 && base <= 16 -> do
      let v' = T.pack $ showIntAtBase base Char.intToDigit v ""
      returnCEKValue cont handler (VString v')
    | base == 64 && v >= 0 -> do
      let v' = toB64UrlUnpaddedText $ integerToBS v
      returnCEKValue cont handler (VString v')
    | base == 64 -> returnCEK cont handler (VError "only positive values allowed for base64URL conversion" info)
    | otherwise -> returnCEK cont handler (VError "invalid base for base64URL conversion" info)
  args -> argsError info b args

coreStrToInt :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
coreStrToInt info b cont handler _env = \case
  [VString s] ->
    checkLen info s *> doBase info cont handler 10 s
  args -> argsError info b args

coreStrToIntBase :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
coreStrToIntBase info b cont handler _env = \case
  [VInteger base, VString s]
    | base == 64 -> checkLen info s *> case decodeBase64UrlUnpadded $ T.encodeUtf8 s of
        Left{} -> throwExecutionError info (DecodeError "invalid b64 encoding")
        Right bs -> returnCEKValue cont handler $ VInteger (bsToInteger bs)
    | base >= 2 && base <= 16 -> checkLen info s *> doBase info cont handler base s
    | otherwise -> returnCEK cont handler (VError "Base value must be >= 2 and <= 16, or 64" info)
  args -> argsError info b args
  where
  -- Todo: DOS and gas analysis
  bsToInteger :: BS.ByteString -> Integer
  bsToInteger bs = fst $ foldl' go (0,(BS.length bs - 1) * 8) $ BS.unpack bs
  go (i,p) w = (i .|. (shift (fromIntegral w) p),p - 8)

coreDistinct  :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
coreDistinct info b cont handler _env = \case
  [VList s] ->
    returnCEKValue cont handler
      $ VList
      $ V.fromList
      $ nubOrd
      $ V.toList s
  args -> argsError info b args

coreFormat  :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
coreFormat info b cont handler _env = \case
  [VString s, VList es] -> do
    let parts = T.splitOn "{}" s
        plen = length parts
    if | plen == 1 -> returnCEKValue cont handler (VString s)
       | plen - length es > 1 -> returnCEK cont handler $ VError "format: not enough arguments for template" info
       | otherwise -> do
          let args = formatArg <$> V.toList es
          returnCEKValue cont handler $ VString $  T.concat $ alternate parts (take (plen - 1) args)
    where
    formatArg (PString ps) = ps
    formatArg a = renderPactValue a
    alternate (x:xs) ys = x : alternate ys xs
    alternate _ _ = []
  args -> argsError info b args

-- Todo: This _Really_ needs gas
-- moreover this is kinda hacky
-- BIG TODO: REMOVE PRETTY FROM SEMANTICS.
-- THIS CANNOT MAKE IT TO PROD
renderPactValue :: PactValue -> T.Text
renderPactValue = T.pack . show . Pretty.pretty

checkLen
  :: (MonadEval b i m)
  => i
  -> T.Text
  -> m ()
checkLen info txt =
  unless (T.length txt <= 512) $
      throwExecutionError info $ DecodeError "Invalid input, only up to 512 length supported"

doBase
  :: (MonadEval b i m)
  => i
  -> Cont b i m
  -> CEKErrorHandler b i m
  -> Integer
  -> T.Text
  -> m (EvalResult b i m)
doBase info cont handler base txt = case baseStrToInt base txt of
  Left e -> throwExecutionError info (DecodeError e)
  Right n -> returnCEKValue cont handler (VInteger n)

baseStrToInt :: Integer -> T.Text -> Either T.Text Integer
baseStrToInt base t
  | base <= 1 || base > 16 = Left $ "unsupported base: " `T.append` T.pack (show base)
  | T.null t = Left $ "empty text: " `T.append` t
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


coreAndQ :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
coreAndQ info b cont handler _env = \case
  [VClosure l, VClosure r, e] -> do
    applyLam l [e] Mt CEKNoHandler >>= \case
      EvalValue (VBool out)
        | out -> applyLam r [e] Mt CEKNoHandler >>= \case
            EvalValue (VBool out') -> returnCEKValue cont handler (VBool out')
            VError err i -> returnCEK cont handler (VError err i)
            _ -> returnCEK cont handler invalidCloValue
        | otherwise -> returnCEKValue cont handler (VBool out)
      EvalValue _ -> returnCEK cont handler invalidCloValue
      VError err i -> returnCEK cont handler (VError err i)
    where
    invalidCloValue = VError "invalid return application for and? closure" info
  args -> argsError info b args

coreOrQ :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
coreOrQ info b cont handler _env = \case
  [VClosure l, VClosure r, e] -> do
    applyLam l [e] Mt CEKNoHandler >>= \case
      EvalValue (VBool out)
        | out -> returnCEKValue cont handler (VBool out)
        | otherwise -> applyLam r [e] Mt CEKNoHandler >>= \case
            EvalValue (VBool out') -> returnCEKValue cont handler (VBool out')
            VError err i -> returnCEK cont handler (VError err i)
            _ -> returnCEK cont handler invalidCloValue
      EvalValue _ -> returnCEK cont handler invalidCloValue
      VError err i -> returnCEK cont handler (VError err i)
    where
    invalidCloValue = VError "invalid return application for and? closure" info
  args -> argsError info b args

coreNotQ :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
coreNotQ info b cont handler _env = \case
  [VClosure l, e] -> do
    applyLam l [e] Mt CEKNoHandler >>= \case
      EvalValue (VBool out) -> returnCEKValue cont handler (VBool (not out))
      EvalValue _ -> returnCEK cont handler invalidCloValue
      VError err i -> returnCEK cont handler (VError err i)
    where
    invalidCloValue = VError "invalid return application for and? closure" info
  args -> argsError info b args

coreWhere :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
coreWhere info b cont handler _env = \case
  [VString field, VClosure app, VObject o] -> do
    case M.lookup (Field field) o of
      Just v -> applyLam app [VPactValue v] Mt CEKNoHandler >>= \case
        EvalValue (VBool cond) -> returnCEKValue cont handler (VBool cond)
        EvalValue _ -> returnCEK cont handler (VError "where application did not result in a boolean" info)
        VError err i -> returnCEK cont handler (VError err i)
      Nothing -> returnCEK cont handler (VError "no such field in object in where application" info)
  args -> argsError info b args

coreHash :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
coreHash = \info b cont handler _env -> \case
  [VString s] ->
    returnCEKValue cont handler (go (T.encodeUtf8 s))
  [VPactValue pv] -> do
    returnCEKValue cont handler (go (encodeStable pv))
  args -> argsError info b args
  where
  go =  VString . hashToText . pactHash

txHash :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
txHash info b cont handler _env = \case
  [] -> do
    h <- viewEvalEnv eeHash
    returnCEKValue cont handler (VString (hashToText h))
  args -> argsError info b args

coreContinue :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
coreContinue info b cont handler _env = \case
  [v] -> do
    returnCEKValue cont handler v
  args -> argsError info b args

parseTime :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
parseTime info b cont handler _env = \case
  [VString fmt, VString s] ->
    case PactTime.parseTime (T.unpack fmt) (T.unpack s) of
      Just t -> returnCEKValue cont handler $ VPactValue (PTime t)
      Nothing ->
        returnCEK cont handler (VError "parse-time parse failure" info)
  args -> argsError info b args

formatTime :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
formatTime info b cont handler _env = \case
  [VString fmt, VPactValue (PTime t)] -> do
    let timeString = PactTime.formatTime (T.unpack fmt) t
    returnCEKValue cont handler $ VString (T.pack timeString)
  args -> argsError info b args

time :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
time info b cont handler _env = \case
  [VString s] -> do
    case PactTime.parseTime "%Y-%m-%dT%H:%M:%SZ" (T.unpack s) of
      Just t -> returnCEKValue cont handler $ VPactValue (PTime t)
      Nothing ->
        returnCEK cont handler (VError "time default format parse failure" info)
  args -> argsError info b args

addTime :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
addTime info b cont handler _env = \case
  [VPactValue (PTime t), VPactValue (PDecimal seconds)] -> do
      let newTime = t PactTime..+^ PactTime.fromSeconds seconds
      returnCEKValue cont handler $ VPactValue (PTime newTime)
  args -> argsError info b args

diffTime :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
diffTime info b cont handler _env = \case
  [VPactValue (PTime x), VPactValue (PTime y)] -> do
    let secondsDifference = PactTime.toSeconds $ x PactTime..-. y
    returnCEKValue cont handler $ VPactValue $ PDecimal secondsDifference
  args -> argsError info b args

minutes :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
minutes info b cont handler _env = \case
  [VDecimal x] -> do
    let seconds = x * 60
    returnCEKValue cont handler $ VDecimal seconds
  [VInteger x] -> do
    let seconds = fromIntegral x * 60
    returnCEKValue cont handler $ VDecimal seconds
  args -> argsError info b args

hours :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
hours info b cont handler _env = \case
  [VDecimal x] -> do
    let seconds = x * 60 * 60
    returnCEKValue cont handler $ VDecimal seconds
  [VInteger x] -> do
    let seconds = fromIntegral x * 60 * 60
    returnCEKValue cont handler $ VDecimal seconds
  args -> argsError info b args

days :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
days info b cont handler _env = \case
  [VDecimal x] -> do
    let seconds = x * 60 * 60 * 24
    returnCEKValue cont handler $ VDecimal seconds
  [VInteger x] -> do
    let seconds = fromIntegral x * 60 * 60 * 24
    returnCEKValue cont handler $ VDecimal seconds
  args -> argsError info b args

describeModule :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
describeModule info b cont handler env = \case
  [VString s] -> case parseModuleName s of
    Just mname -> do
      enforceTopLevelOnly info b
      checkNonLocalAllowed info
      getModuleData info (view cePactDb env) mname >>= \case
        ModuleData m _ -> returnCEKValue cont handler $
          VObject $ M.fromList $ fmap (over _1 Field)
            [ ("name", PString (renderModuleName (_mName m)))
            , ("hash", PString (moduleHashToText (_mHash m)))
            , ("interfaces", PList (PString . renderModuleName <$> V.fromList (_mImplements m)))]
        InterfaceData iface _ -> returnCEKValue cont handler $
          VObject $ M.fromList $ fmap (over _1 Field)
            [ ("name", PString (renderModuleName (_ifName iface)))
            , ("hash", PString (moduleHashToText (_ifHash iface)))
            ]
    Nothing -> returnCEK cont handler (VError "invalid module name" info)
  args -> argsError info b args

dbDescribeTable :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
dbDescribeTable info b cont handler _env = \case
  [VTable (TableValue name _ schema)] ->
    returnCEKValue cont handler $ VObject $ M.fromList $ fmap (over _1 Field)
      [("name", PString (_tableName name))
      ,("module", PString (renderModuleName (_tableModuleName name)))
      ,("type", PString (renderType (TyTable schema)))]
  args -> argsError info b args

dbDescribeKeySet :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
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
            returnCEK cont handler (VError ("keyset not found" <> s) info)
      Left{} ->
        returnCEK cont handler (VError "invalid keyset name" info)
  args -> argsError info b args

coreCompose :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
coreCompose info b cont handler env = \case
  [VClosure clo1, VClosure clo2, v] -> do
    let cont' = Fn clo2 env [] [] cont
    applyLam clo1 [v] cont' handler
  args -> argsError info b args

createPrincipalForGuard :: Guard QualifiedName PactValue -> Pr.Principal
createPrincipalForGuard = \case
  GKeyset (KeySet ks pf) -> case (toList ks, pf) of
    ([k], KeysAll) -> Pr.K k
    (l, _) -> let h = mkHash $ map (T.encodeUtf8 . _pubKey) l
              in Pr.W (hashToText h) (predicateToString pf)
  GKeySetRef ksn -> Pr.R ksn
  GModuleGuard (ModuleGuard mn n) -> Pr.M mn n
  GUserGuard (UserGuard f args) ->
    let h = mkHash $ map encodeStable args
    in Pr.U (renderQualName f) (hashToText h)
    -- TODO orig pact gets here ^^^^ a Name
    -- which can be any of QualifiedName/BareName/DynamicName/FQN,
    -- and uses the rendered string here. Need to double-check equivalence.
  GCapabilityGuard (CapabilityGuard f args pid) ->
    let args' = map encodeStable args
        f' = T.encodeUtf8 $ renderQualName f
        pid' = T.encodeUtf8 . renderDefPactId <$> pid
        h = mkHash $ f' : args' ++ maybeToList pid'
    in Pr.C $ hashToText h
  GDefPactGuard (DefPactGuard dpid name) -> Pr.P dpid name
  where
    mkHash bss = pactHash $ mconcat bss

coreCreatePrincipal :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
coreCreatePrincipal info b cont handler _env = \case
  [VGuard g] -> do
    let pr = createPrincipalForGuard g
    returnCEKValue cont handler $ VString $ Pr.mkPrincipalIdent pr
  args -> argsError info b args

coreIsPrincipal :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
coreIsPrincipal info b cont handler _env = \case
  [VString p] -> returnCEKValue cont handler $ VBool $ isRight $ parseOnly Pr.principalParser p
  args -> argsError info b args

coreTypeOfPrincipal :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
coreTypeOfPrincipal info b cont handler _env = \case
  [VString p] -> do
    let prty = case parseOnly Pr.principalParser p of
          Left _ -> ""
          Right pr -> Pr.showPrincipalType pr
    returnCEKValue cont handler $ VString prty
  args -> argsError info b args

coreValidatePrincipal :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
coreValidatePrincipal info b cont handler _env = \case
  [VGuard g, VString s] -> do
    let pr' = createPrincipalForGuard g
    returnCEKValue cont handler $ VBool $ Pr.mkPrincipalIdent pr' == s
  args -> argsError info b args


--------------------------------------------------
-- Namespace functions
--------------------------------------------------
coreNamespace :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
coreNamespace info b cont handler env = \case
  [VString n] -> do
    enforceTopLevelOnly info b
    let pdb = view cePactDb env
    if T.null n then do
      (esLoaded . loNamespace) .== Nothing
      returnCEKValue cont handler (VString "Namespace reset to root")
    else
      liftDbFunction info (_pdbRead pdb DNamespaces (NamespaceName n)) >>= \case
        Just ns -> do
          (esLoaded . loNamespace) .== Just ns
          let msg = "Namespace set to " <> n
          returnCEKValue cont handler (VString msg)
        Nothing ->
          returnCEK cont handler $ VError ("Namespace " <> n <> " not defined") info
  args -> argsError info b args


coreDefineNamespace :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
coreDefineNamespace info b cont handler env = \case
  [VString n, VGuard usrG, VGuard adminG] -> do
    enforceTopLevelOnly info b
    unless (isValidNsFormat n) $ throwExecutionError info (DefineNamespaceError "invalid namespace format")
    let pdb = view cePactDb env
    liftDbFunction info (_pdbRead pdb DNamespaces (NamespaceName n)) >>= \case
      -- G!
      -- https://static.wikia.nocookie.net/onepiece/images/5/52/Lao_G_Manga_Infobox.png/revision/latest?cb=20150405020446
      -- Enforce the old guard
      Just (Namespace _ _ laoG) ->
        enforceGuardCont info cont handler env laoG $ do
          let nsn = NamespaceName n
              ns = Namespace nsn usrG adminG
          liftDbFunction info (_pdbWrite pdb Write DNamespaces nsn ns)
          returnCEKValue cont handler $ VString $ "Namespace defined: " <> n
      Nothing -> do
        enforcePolicy pdb n adminG
        let nsn = NamespaceName n
            ns = Namespace nsn usrG adminG
        liftDbFunction info (_pdbWrite pdb Write DNamespaces nsn ns)
        returnCEKValue cont handler $ VString $ "Namespace defined: " <> n
  args -> argsError info b args
  where
  enforcePolicy pdb nsn adminG = viewEvalEnv eeNamespacePolicy >>= \case
    SimpleNamespacePolicy -> pure ()
    SmartNamespacePolicy _ fun -> getModuleMember info pdb fun >>= \case
      Dfun d -> do
        clo <- mkDefunClosure d (_qnModName fun) env
        -- Todo: nested exec?
        applyLam (C clo) [VString nsn, VGuard adminG] Mt CEKNoHandler >>= \case
          EvalValue (VBool allow) ->
            unless allow $ throwExecutionError info $ DefineNamespaceError "Namespace definition not permitted"
          EvalValue _ ->
            throwExecutionError info $ DefineNamespaceError "Namespace manager function returned an invalid value"
          VError e _ ->
            throwExecutionError info $ DefineNamespaceError e
      _ -> failInvariant info "Namespace manager function is not a defun"
  isValidNsFormat nsn = case T.uncons nsn of
    Just (h, tl) ->
      isValidNsHead h && T.all isValidNsChar tl
    Nothing -> False
    -- not (T.null nsn) && isValidNsHead (T.head nsn) && T.all isValidNsChar (T.tail )
  isValidNsHead c =
    Char.isLatin1 c && Char.isAlpha c
  isValidNsChar c =
    Char.isLatin1 c && (Char.isAlphaNum c || T.elem c validSpecialChars)
  validSpecialChars :: T.Text
  validSpecialChars =
    "%#+-_&$@<>=^?*!|/~"

coreDescribeNamespace :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
coreDescribeNamespace info b cont handler _env = \case
  [VString n] -> do
    pdb <- viewEvalEnv eePactDb
    liftDbFunction info (_pdbRead pdb DNamespaces (NamespaceName n)) >>= \case
      Just (Namespace _ usrG laoG) -> do
        let obj = M.fromList
                  [ (Field "user-guard", PGuard usrG)
                  , (Field "admin-guard", PGuard laoG)
                  , (Field "namespace-name", PString n)]
        returnCEKValue cont handler (VObject obj)
      Nothing ->
        returnCEK cont handler (VError ("Namespace not defined " <> n) info)
  args -> argsError info b args


-- chainDataDef :: NativeDef
-- chainDataDef = defRNative "chain-data" chainData
--     (funType (tTyObject pcTy) [])
--     ["(chain-data)"]
--     "Get transaction public metadata. Returns an object with 'chain-id', 'block-height', \
--     \'block-time', 'prev-block-hash', 'sender', 'gas-limit', 'gas-price', and 'gas-fee' fields."
--   where
--     pcTy = TyUser (snd chainDataSchema)
--     chainData :: RNativeFun e
--     chainData _ [] = do
--       PublicData{..} <- view eePublicData

--       let PublicMeta{..} = _pdPublicMeta
--           toTime = toTerm . fromPosixTimestampMicros

--       pure $ toTObject TyAny def
--         [ (cdChainId, toTerm _pmChainId)
--         , (cdBlockHeight, toTerm _pdBlockHeight)
--         , (cdBlockTime, toTime _pdBlockTime)
--         , (cdPrevBlockHash, toTerm _pdPrevBlockHash)
--         , (cdSender, toTerm _pmSender)
--         , (cdGasLimit, toTerm _pmGasLimit)
--         , (cdGasPrice, toTerm _pmGasPrice)
--         ]
--     chainData i as = argsError i as

coreChainData :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
coreChainData info b cont handler _env = \case
  [] -> do
    PublicData publicMeta blockHeight blockTime prevBh <- viewEvalEnv eePublicData
    let (PublicMeta cid sender (Gas gasLimit) gasPrice _ttl _creationTime) = publicMeta
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

ensureOnCurve :: (Num p, Eq p, MonadEval b i m) => i -> CurvePoint p -> p -> m ()
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
  toPactPt (Extension e) = let
    elems' = fmap (PInteger . fromIntegral) (Poly.unPoly e)
    in PList elems'
  x' = toPactPt x
  y' = toPactPt y
  pts =
    M.fromList
    [ (Field "x", x')
    , (Field "y", y')]


zkPairingCheck :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
zkPairingCheck info b cont handler _env = \case
  args@[VList p1s, VList p2s] -> do
    g1s <- maybe (argsError info b args) pure (traverse (preview _PObject >=> (toG1 . ObjectData)) p1s)
    g2s <- maybe (argsError info b args) pure (traverse (preview _PObject >=> (toG2 . ObjectData)) p2s)
    traverse_ (\p -> ensureOnCurve info p b1) g1s
    traverse_ (\p -> ensureOnCurve info p b2) g2s
    let pairs = zip (V.toList g1s) (V.toList g2s)
    returnCEKValue cont handler $ VBool $ pairingCheck pairs
  args -> argsError info b args

zkScalaMult :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
zkScalaMult info b cont handler _env = \case
  args@[VString ptTy, VObject p1, VInteger scalar] -> do
    let scalar' = scalar `mod` curveOrder
    case T.toLower ptTy of
      "g1" -> do
        p1' <- maybe (argsError info b args) pure $ toG1 (ObjectData p1)
        ensureOnCurve info p1' b1
        let p2' = multiply p1' scalar'
            ObjectData o = fromG1 p2'
        returnCEKValue cont handler (VObject o)
      "g2" -> do
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

zkPointAddition :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
zkPointAddition info b cont handler _env = \case
  args@[VString ptTy, VObject p1, VObject p2] -> do
    case T.toLower ptTy of
      "g1" -> do
        p1' <- maybe (argsError info b args) pure $ toG1 (ObjectData p1)
        p2' <- maybe (argsError info b args) pure $ toG1 (ObjectData p2)
        ensureOnCurve info p1' b1
        ensureOnCurve info p2' b1
        let p3' = add p1' p2'
            ObjectData o = fromG1 p3'
        returnCEKValue cont handler (VObject o)
      "g2" -> do
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

poseidonHash :: (IsBuiltin b, MonadEval b i m) => NativeFunction b i m
poseidonHash info b cont handler _env = \case
  [VList as]
    | not (V.null as) && length as <= 8,
    Just intArgs <- traverse (preview (_PLiteral . _LInteger)) as ->
      returnCEKValue cont handler $ VInteger (poseidon (V.toList intArgs))
  args -> argsError info b args

-----------------------------------
-- Builtin exports
-----------------------------------

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
  RawRoundPrec -> roundDec
  RawCeilingPrec -> ceilingDec
  RawFloorPrec -> floorDec
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
  RawIntToStr -> coreIntToStr
  RawStrToInt -> coreStrToInt
  RawStrToIntBase -> coreStrToIntBase
  RawFold -> coreFold
  RawDistinct -> coreDistinct
  RawFormat -> coreFormat
  RawContains -> rawContains
  RawSort -> rawSort
  RawSortObject -> rawSortObject
  RawRemove -> coreRemove
  -- RawEnforce -> coreEnforce
  -- RawEnforceOne -> unimplemented
  RawEnumerate -> coreEnumerate
  RawEnumerateStepN -> coreEnumerateStepN
  RawShow -> rawShow
  RawReadMsg -> coreReadMsg
  RawReadMsgDefault -> coreReadMsg
  RawReadInteger -> coreReadInteger
  RawReadDecimal -> coreReadDecimal
  RawReadString -> coreReadString
  RawReadKeyset -> coreReadKeyset
  RawEnforceGuard -> coreEnforceGuard
  RawYield -> coreYield
  RawYieldToChain -> coreYield
  RawResume -> coreResume
  RawEnforceKeyset -> coreEnforceGuard
  RawKeysetRefGuard -> keysetRefGuard
  RawAt -> coreAccess
  RawMakeList -> makeList
  RawB64Encode -> coreB64Encode
  RawB64Decode -> coreB64Decode
  RawStrToList -> strToList
  RawBind -> coreBind
  RawRequireCapability -> requireCapability
  RawComposeCapability -> composeCapability
  RawInstallCapability -> installCapability
  RawCreateCapabilityGuard -> createCapGuard
  RawCreateCapabilityPactGuard -> createCapabilityPactGuard
  RawCreateModuleGuard -> createModuleGuard
  RawCreateDefPactGuard -> createDefPactGuard
  RawEmitEvent -> coreEmitEvent
  RawCreateTable -> createTable
  RawDescribeKeyset -> dbDescribeKeySet
  RawDescribeModule -> describeModule
  RawDescribeTable -> dbDescribeTable
  RawDefineKeySet -> defineKeySet
  RawDefineKeysetData -> defineKeySet
  RawFoldDb -> foldDb
  RawInsert -> dbInsert
  RawWrite -> dbWrite
  RawKeyLog -> dbKeyLog
  RawKeys -> dbKeys
  RawRead -> dbRead
  RawSelect -> dbSelect
  RawUpdate -> dbUpdate
  RawWithDefaultRead -> dbWithDefaultRead
  RawWithRead -> dbWithRead
  RawTxLog -> dbTxLog
  RawTxIds -> dbTxIds
  RawAndQ -> coreAndQ
  RawOrQ -> coreOrQ
  RawWhere -> coreWhere
  RawNotQ -> coreNotQ
  RawHash -> coreHash
  RawTxHash -> txHash
  RawContinue -> coreContinue
  RawParseTime -> parseTime
  RawFormatTime -> formatTime
  RawTime -> time
  RawAddTime -> addTime
  RawDiffTime -> diffTime
  RawHours -> hours
  RawMinutes -> minutes
  RawDays -> days
  RawCompose -> coreCompose
  RawSelectWithFields -> dbSelect
  RawCreatePrincipal -> coreCreatePrincipal
  RawIsPrincipal -> coreIsPrincipal
  RawTypeOfPrincipal -> coreTypeOfPrincipal
  RawValidatePrincipal -> coreValidatePrincipal
  RawNamespace -> coreNamespace
  RawDefineNamespace -> coreDefineNamespace
  RawDescribeNamespace -> coreDescribeNamespace
  RawZkPairingCheck -> zkPairingCheck
  RawZKScalarMult -> zkScalaMult
  RawZkPointAdd -> zkPointAddition
  RawPoseidonHashHackachain -> poseidonHash
  RawChainData -> coreChainData
  RawIsCharset -> coreIsCharset
  RawPactId -> corePactId
  RawTypeOf -> coreTypeOf
  RawDec -> coreDec
