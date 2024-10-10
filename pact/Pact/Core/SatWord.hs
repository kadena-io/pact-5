{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MagicHash      #-}
{-# LANGUAGE UnboxedTuples  #-}
{-# LANGUAGE DerivingVia  #-}
{-# LANGUAGE GeneralisedNewtypeDeriving  #-}

{- |
Adapted from `Data.SatWord`, which is adapated from 'Data.SafeInt' to perform saturating arithmetic
(i.e. returning max or min bounds) instead of throwing on overflow.

This is not quite as fast as using 'Int' or 'Int64' directly, but we need the safety.
-}

module Pact.Core.SatWord(
   SatWord (unSatWord)
  , unsafeToSatWord
  , fromSatWord
  ) where

import Data.Aeson(FromJSON, ToJSON)
import Control.DeepSeq (NFData)
import Data.Bits
import Data.Primitive (Prim)
import GHC.Base
import GHC.Generics
import GHC.Real
import qualified Pact.Core.Pretty as P

-- Note: real and integral instances cannot overflow
newtype SatWord = SW { unSatWord :: Word }
    deriving newtype (Show, Read, Eq, Ord, Enum, Bounded, NFData, Bits, FiniteBits, Prim, P.Pretty, Real, Integral)
    deriving (FromJSON, ToJSON) via Word
    deriving stock (Generic)

{-# INLINE unsafeToSatWord #-}
-- | Wrap an 'Word' as a 'SatWord'. This is unsafe because the 'Int' can be a result of an arbitrary
-- potentially underflowing/overflowing operation.
unsafeToSatWord :: Word -> SatWord
unsafeToSatWord = SW

{-# INLINE fromSatWord #-}
-- | An optimized version of @fromIntegral . unSatInt@.
fromSatWord :: forall a. Num a => SatWord -> a
fromSatWord = coerce (fromIntegral :: Word -> a)

-- | In the `Num' instance, we plug in our own addition, multiplication
-- and subtraction function that perform overflow-checking.
instance Num SatWord where
  {-# INLINE (+) #-}
  (+) = plusSW

  {-# INLINE (*) #-}
  (*) = timesSW

  {-# INLINE (-) #-}
  (-) = minusSW

  {-# INLINE negate #-}
  negate (SW y)
    | y == minBound = maxBound
    | otherwise     = SW (negate y)

  {-# INLINE abs #-}
  abs x
    | x >= 0    = x
    | otherwise = negate x

  {-# INLINE signum #-}
  signum = coerce (signum :: Word -> Word)

  {-# INLINE fromInteger #-}
  fromInteger x
    | x > maxBoundInteger  = maxBound
    | x < minBoundInteger  = minBound
    | otherwise            = SW (fromInteger x)

{-# INLINABLE maxBoundInteger #-}
maxBoundInteger :: Integer
maxBoundInteger = toInteger (maxBound :: Word)

{-# INLINABLE minBoundInteger #-}
minBoundInteger :: Integer
minBoundInteger = toInteger (minBound :: Word)

{-
'addWordC#', 'subWordC#', and 'timesWord2#' have tricky returns:
all of them return non-zero (*not* necessarily 1) in the case of an overflow,
so we can't use 'isTrue#'; and the first two return a truncated value in
case of overflow, but this is *not* the same as the saturating result,
but rather a bitwise truncation that is typically not what we want.

So we have to case on the result, and then do some logic to work out what
kind of overflow we're facing, and pick the correct result accordingly.
-}

{-# INLINE plusSW #-}
plusSW :: SatWord -> SatWord -> SatWord
plusSW (SW (W# x#)) (SW (W# y#)) =
  case addWordC# x# y#  of
    (# r#, 0# #) -> SW (W# r#)
    -- Overflow
    _ ->
      if      isTrue# ((x# `gtWord#` 0##) `andI#` (y# `gtWord#` 0##)) then maxBound
      else if isTrue# ((x# `ltWord#` 0##) `andI#` (y# `ltWord#` 0##)) then minBound
      -- x and y have opposite signs, and yet we've overflowed, should
      -- be impossible
      else overflowError

{-# INLINE minusSW #-}
minusSW :: SatWord -> SatWord -> SatWord
minusSW (SW (W# x#)) (SW (W# y#)) =
  case subWordC# x# y# of
    (# r#, 0# #) -> SW (W# r#)
    -- Overflow
    _ ->
      if      isTrue# ((x# `geWord#` 0##) `andI#` (y# `ltWord#` 0##)) then maxBound
      else if isTrue# ((x# `leWord#` 0##) `andI#` (y# `gtWord#` 0##)) then minBound
      -- x and y have the same sign, and yet we've overflowed, should
      -- be impossible
      else overflowError

{-# INLINE timesSW #-}
timesSW :: SatWord -> SatWord -> SatWord
timesSW (SW (W# x#)) (SW (W# y#)) =
  case timesWord2# x# y# of
      (# 0##, r# #) -> SW (W# r#)
      -- Overflow
      _ ->
          if      isTrue# ((x# `gtWord#` 0##) `andI#` (y# `gtWord#` 0##)) then maxBound
          else if isTrue# ((x# `gtWord#` 0##) `andI#` (y# `ltWord#` 0##)) then minBound
          else if isTrue# ((x# `ltWord#` 0##) `andI#` (y# `gtWord#` 0##)) then minBound
          else if isTrue# ((x# `ltWord#` 0##) `andI#` (y# `ltWord#` 0##)) then maxBound
          -- Logically unreachable unless x or y is 0, in which case
          -- it should be impossible to overflow
          else overflowError

-- Specialized versions of several functions. They're specialized for
-- Int in the GHC base libraries. We try to get the same effect by
-- including specialized code and adding a rewrite rule.

sumSW :: [SatWord] -> SatWord
sumSW     l       = sum' l 0
  where
    sum' []     a = a
    sum' (x:xs) a = sum' xs $! a + x

productSW :: [SatWord] -> SatWord
productSW l       = prod l 1
  where
    prod []     a = a
    prod (x:xs) a = prod xs $! a * x

{-# RULES
  "sum/SatWord"          sum = sumSW;
  "product/SatWord"      product = productSW
  #-}
