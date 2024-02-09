{-# LANGUAGE MagicHash #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingVia #-}

-- |
-- Module      :  Pact.Types.SizeOf
-- Copyright   :  (C) 2019 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--

module Pact.Core.SizeOf
  ( SizeOf(..)
  , SizeOf1(..)
  , constructorCost
  , Bytes
  , wordSize
  , SizeOfVersion(..)

  -- * SizeOf 
  , countBytes
  ) where

import Control.Monad
import Data.Decimal
import Data.Default (def)
import Data.Int (Int64)
import Data.Set (Set)
import Data.Text (Text)
import Pact.Time
import Data.Vector (Vector)
import Data.Word (Word8, Word64)
import GHC.Generics
import GHC.Int(Int(..))
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Short as SBS
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Set as S
import qualified GHC.Integer.Logarithms as IntLog

import Pact.Core.Names
import Pact.Core.Pretty
import Pact.Core.Hash
import Pact.Core.IR.Term
import Pact.Core.Capabilities
import Pact.Core.Type
import Pact.Core.Environment.Types
import Pact.Core.Builtin
import Pact.Core.Literal
import Pact.Core.PactValue
import Pact.Core.Guards
import Pact.Core.DefPacts.Types
import Pact.Core.Imports
import Pact.Core.Info
import Pact.Core.ModRefs
import Pact.Core.Namespace (Namespace)
import Pact.Core.IR.Eval.Runtime.Utils (chargeGasArgs)
import Pact.Core.Gas (GasArgs(GCountBytes))


-- |  Estimate of number of bytes needed to represent data type
--
-- Assumptions: GHC, 64-bit machine
-- General approach:
--   Memory Consumption = Constructor Header Size + Cost of Constructor Field(s)
--   Cost of Constructor Field(s)* = 1 word per field + cost of each field's value
-- (*) See Resource 2 for exceptions to these rules (i.e. newtypes are free)
-- Resources:
-- 1. http://wiki.haskell.org/GHC/Memory_Footprint
-- 2. https://stackoverflow.com/questions/3254758/memory-footprint-of-haskell-data-types

-- | Size estimates determine the gas cost of various operations,
--   (e.g., writing data to a user table or loading a module), so
--   we must version the estimation process in order to allow us to
--   update our estimates without breaking compatibility.
--
--   In other words, a particular combination of (SizeOfVersion, Type)
--   must always produce the same answer, across all releases of pact.
--   An exception is made when we are certain that a particular pact
--   release will never encounter a given (SizeOfVersion, Type) combination;
--   this combination may be ignored.
--
--   Although SizeOfVersion is the mechanism used to link a pact version
--   to a size estimate, it is not always necessary to use your pact version's
--   SizeOf version when computing sizes of things. In other words, different
--   contexts have different reasons for computing the size of something,
--   and are free to use different SizeOfVersion when calling `sizeOf`.
--   For example, we have found the SizeOfV0 measurements to be sufficient
--   when computing the size of a module during module-loading, so the
--   code that charges gas for module load hardcodes SizeOfVersion to
--   SizeOfV0. Size estimates for writing to user tables change more
--   frequenly, so the gas computation for that refers to pact version
--   flags to get the appropriate SizeOfVersion.
data SizeOfVersion
  = SizeOfV2
  | SizeOfV1
  | SizeOfV0
  deriving (Show, Eq)

instance Pretty SizeOfVersion where
  pretty = viaShow

countBytes :: MonadEval b i m => SizeOfVersion -> Bytes -> m Bytes
countBytes szver bytes = do
  when (szver == SizeOfV2) (chargeGasArgs def GCountBytes)
  pure bytes

class SizeOf t where
  sizeOf :: forall m b i. MonadEval b i m => SizeOfVersion -> t -> m Bytes
  default sizeOf :: forall b i m.(Generic t, GSizeOf (Rep t), MonadEval b i m) => SizeOfVersion -> t -> m Bytes
  sizeOf v a = gsizeOf v (from a)

-- | "word" is 8 bytes on 64-bit
wordSize64, wordSize :: Bytes
wordSize64 = 8
wordSize = wordSize64

-- | Constructor header is 1 word
headerCost :: Bytes
headerCost = 1 * wordSize

-- | In general, each constructor field costs 1 word
constructorFieldCost :: Word64 -> Bytes
constructorFieldCost numFields = numFields * wordSize

-- | Total cost for constructor
constructorCost :: Word64 -> Bytes
constructorCost numFields = headerCost + (constructorFieldCost numFields)


instance (SizeOf v) => SizeOf (Vector v) where
  sizeOf ver v = do
    let rawVecSize = (7 + vectorLength) * wordSize
    spineSize <- countBytes ver rawVecSize
    elementSizes <- traverse (sizeOf ver) v
    pure $ spineSize + sum elementSizes
    where
      vectorLength = fromIntegral (V.length v)

instance (SizeOf a) => SizeOf (Set a) where
  sizeOf ver s = do
    let !setSizeOverhead = (1 + 3 * setLength) * wordSize
    spineSize <- countBytes ver setSizeOverhead
    elementSizes <- traverse (sizeOf ver) (S.toList s)
    pure $ spineSize + sum elementSizes
    where
      setLength = fromIntegral (S.size s)

instance (SizeOf k, SizeOf v) => SizeOf (M.Map k v) where
  sizeOf ver m = do
    let !mapSizeOverhead = 6 * mapLength * wordSize
    spineSize <- countBytes ver mapSizeOverhead
    elementSizes <- traverse (\(k,v) -> liftA2 (+) (sizeOf ver k) (sizeOf ver v)) (M.toList m)
    pure $ spineSize + sum elementSizes
    where
      mapLength = fromIntegral (M.size m)

instance (SizeOf a, SizeOf b) => SizeOf (a,b) where
  sizeOf ver (a,b) = do
    headBytes <- countBytes ver (constructorCost 3)
    aBytes <- sizeOf ver a
    bBytes <- sizeOf ver b
    pure $ headBytes + aBytes + bBytes

instance (SizeOf a) => SizeOf (Maybe a) where
  sizeOf ver (Just e) =
    liftA2 (+) (countBytes ver (constructorCost 1)) (sizeOf ver e)
  sizeOf ver Nothing =
    countBytes ver (constructorCost 0)

instance (SizeOf a) => SizeOf [a] where
  sizeOf ver arr = do
    let listSzOverhead = (1 + (3 * listLength)) * wordSize
    spineSize <- countBytes ver listSzOverhead
    elementSizes <- traverse (sizeOf ver) arr
    pure $ spineSize + sum elementSizes
    where
      listLength = fromIntegral (L.length arr)

instance SizeOf BS.ByteString where
  sizeOf ver bs =
    countBytes ver byteStringSize
    where
      byteStringSize = (9 * wordSize) + byteStringLength

      -- 'UTF8.length' returns different values than 'BS.length'. Moreoever,
      -- 'BS.length' is \(O(1)\) but 'UTF8.length' is \(O(n)\), with a pretty
      -- bad constant factor.
      --
      byteStringLength = fromIntegral (UTF8.length bs)

instance SizeOf SBS.ShortByteString where
  sizeOf ver = sizeOf ver . SBS.fromShort

instance SizeOf Text where
  sizeOf ver t =
    countBytes ver $ (6 * wordSize) + (2 * (fromIntegral (T.length t)))

instance SizeOf Integer where
  sizeOf ver i = countBytes ver $ case ver of
    SizeOfV0 ->
      if i < 0 then 0 else ceiling ((logBase 100000 (realToFrac i)) :: Double)
    _ ->
      fromIntegral (max 64 (I# (IntLog.integerLog2# (abs i)) + 1)) `quot` 8

instance SizeOf Int where
  sizeOf ver _ = countBytes ver $ 2 * wordSize

instance SizeOf Word8 where
  sizeOf ver _ = countBytes ver $ 2 * wordSize

instance (SizeOf i) => SizeOf (DecimalRaw i) where
  sizeOf ver (Decimal p m) = do
    constructorSize <- countBytes ver (constructorCost 2)
    pSize <- sizeOf ver p
    mSize <- sizeOf ver m
    pure $ constructorSize + pSize + mSize

instance SizeOf Int64 where
  -- Assumes 64-bit machine
  sizeOf ver _ = countBytes ver $ 2 * wordSize


instance SizeOf Word64 where
  -- Assumes 64-bit machine
  sizeOf ver _ = countBytes ver $ 2 * wordSize


instance SizeOf UTCTime where
  -- newtype is free
  -- Internally 'UTCTime' is just a 64-bit count of 'microseconds'
  sizeOf ver ti =
    liftA2 (+) (countBytes ver (constructorCost 1)) (sizeOf ver (toPosixTimestampMicros ti))

instance SizeOf Bool where
  sizeOf ver _ = countBytes ver wordSize

instance SizeOf () where
  sizeOf _ _ = pure 0

-- See ghc memory note above
-- as well as https://blog.johantibell.com/2011/06/memory-footprints-of-some-common-data.html
-- for both hash sets and hashmaps.
instance (SizeOf k, SizeOf v) => SizeOf (HM.HashMap k v) where
  sizeOf ver m = do
    spineSize <- countBytes ver hmOverhead
    elementSizes <- traverse (\(k,v) -> liftA2 (+) (sizeOf ver k) (sizeOf ver v)) (HM.toList m)
    pure $ spineSize + sum elementSizes
    where
      !hmOverhead = (5 * hmLength + 4 * (hmLength - 1)) * wordSize
      hmLength = fromIntegral (HM.size m)

-- Note: Atm hashset is only a newtype wrapper over hashmap with unit as the element
-- member of every entry. This means you don't pay at all for the cost of (),
-- but you do pay for the extra constructor field of holding it, hence the `hsSize` bit
-- stays roughly the same.
instance (SizeOf k) => SizeOf (HS.HashSet k) where
  sizeOf ver hs = do
    spineSize <- countBytes ver hsSizeOverhead
    elementSizes <- traverse (sizeOf ver) (HS.toList hs)
    pure $ spineSize + sum elementSizes
    where
      hsSizeOverhead = (5 + hsLength + 4 * (hsLength - 1)) * wordSize
      hsLength = fromIntegral (HS.size hs)

instance (SizeOf a, SizeOf b) => SizeOf (Either a b)

instance  (SizeOf a) => SizeOf (NE.NonEmpty a) where
  sizeOf ver (a NE.:| rest) = do
    constructorSize <- countBytes ver (constructorCost 2)
    aSize <- sizeOf ver a
    restSize <- sizeOf ver rest
    pure $ constructorSize + aSize + restSize


class SizeOf1 f where
  sizeOf1 :: SizeOf a => SizeOfVersion -> f a -> Bytes

-- Generic deriving
class GSizeOf f where
  gsizeOf :: forall b i m a. MonadEval b i m => SizeOfVersion -> f a -> m Bytes

-- For sizes of products, we'll calculate the size at the leaves,
-- and simply add 1 extra word for every leaf.
instance (GSizeOf f, GSizeOf g) => GSizeOf (f :*: g) where
  gsizeOf ver (a :*: b) = liftA2 (+) (gsizeOf ver a) (gsizeOf ver b)

-- Sums we can just branch recursively as usual
-- Ctor information is one level lower.
instance (GSizeOf a, GSizeOf b) => GSizeOf (a :+: b) where
  gsizeOf ver = \case
    L1 a -> gsizeOf ver a
    R1 b -> gsizeOf ver b


-- No fields ctors are shared.
-- We are ok charging a bit extra here.
instance {-# OVERLAPS #-} GSizeOf (C1 c U1) where
  gsizeOf ver (M1 _) = countBytes ver wordSize

-- Regular constructors pay the header cost
-- and 1 word for each field, which is added @ the leaves.
instance (GSizeOf f) => GSizeOf (C1 c f) where
  gsizeOf ver (M1 p) = liftA2 (+) (countBytes ver headerCost) (gsizeOf ver p)

-- Metainfo about selectors
instance (GSizeOf f) => GSizeOf (S1 c f) where
  gsizeOf ver (M1 p) = gsizeOf ver p

-- Metainfo about the whole data type.
instance (GSizeOf f) => GSizeOf (D1 c f) where
  gsizeOf ver (M1 p) = gsizeOf ver p

-- Single field, means size of field + 1 word.
instance (SizeOf c) => GSizeOf (K1 i c) where
  gsizeOf ver (K1 c) = liftA2 (+) (sizeOf ver c) (countBytes ver wordSize)

-- No-argument constructors are always shared by ghc
-- so they don't really allocate.
-- However, this instance is used whenever we hit a leaf
-- that _does_ in fact allocate 1 word for the field itself.
-- 0-cost constructor `SizeOf` is caught by the `GSizeOf (C1 c U1)`
-- instance
instance GSizeOf U1 where
  gsizeOf ver U1 = countBytes ver wordSize

--- Pact-core instances
-- Putting some of the more annoying GADTs here
instance SizeOf (FQNameRef name) where
  sizeOf ver c = do
    headBytes <- countBytes ver (headerCost + wordSize)
    tailBytes <- case c of
      FQParsed n -> sizeOf ver n
      FQName fqn -> sizeOf ver fqn
    pure $ headBytes + tailBytes

instance SizeOf (TableSchema name) where
  sizeOf ver c = do
    headBytes <- countBytes ver (headerCost + wordSize)
    tailBytes <- case c of
      DesugaredTable n -> sizeOf ver n
      ResolvedTable fqn -> sizeOf ver fqn
    pure $ headBytes + tailBytes

instance SizeOf Literal where
  sizeOf ver literal = fmap (constructorCost 1 +) $ case literal of
    LString s -> sizeOf ver s
    LInteger i -> sizeOf ver i
    LDecimal d -> sizeOf ver d
    LBool _b -> pure 0
    LUnit -> pure 0

deriving newtype instance SizeOf Hash
deriving newtype instance SizeOf Field
deriving newtype instance SizeOf NamespaceName
deriving newtype instance SizeOf BareName
instance SizeOf ModuleHash
instance SizeOf ModuleName
instance SizeOf DynamicRef
instance SizeOf NameKind
instance SizeOf Name
instance SizeOf QualifiedName
instance SizeOf DynamicName
instance SizeOf ParsedName
instance SizeOf ParsedTyName
instance SizeOf FullyQualifiedName

-- Type
instance SizeOf PrimType
instance SizeOf Schema
instance SizeOf Type

-- defpacts
deriving newtype instance SizeOf DefPactId
instance (SizeOf n, SizeOf v) => SizeOf (DefPactContinuation n v)
deriving newtype instance SizeOf ChainId
instance SizeOf Provenance
instance SizeOf Yield
instance SizeOf DefPactExec

-- spaninfo
instance SizeOf SpanInfo

-- builtins
instance SizeOf CoreBuiltin
instance SizeOf ReplBuiltins
instance SizeOf b => SizeOf (ReplBuiltin b)


-- Import
instance SizeOf Import

-- guards
deriving newtype instance SizeOf PublicKeyText
instance SizeOf KeySetName
instance (SizeOf name, SizeOf v) => SizeOf (UserGuard name v)
instance (SizeOf name, SizeOf v) => SizeOf (CapabilityGuard name v)
instance SizeOf KSPredicate
instance SizeOf KeySet
instance SizeOf ModuleGuard
instance SizeOf DefPactGuard
instance (SizeOf name, SizeOf v) => SizeOf (Guard name v)

-- Caps
instance (SizeOf name, SizeOf v) => SizeOf (CapToken name v)
instance SizeOf n => SizeOf (DefManagedMeta n)
instance SizeOf n => SizeOf (DefCapMeta n)
instance SizeOf n => SizeOf (Governance n)

instance SizeOf ModRef

instance SizeOf PactValue where
  sizeOf ver pactValue = fmap (constructorCost 1 +) $ case pactValue of
    PLiteral l -> sizeOf ver l
    PObject obj -> sizeOf ver obj
    PList l -> sizeOf ver l
    PGuard g -> sizeOf ver g
    PModRef m -> sizeOf ver m
    PCapToken t -> sizeOf ver t
    PTime t -> sizeOf ver t

-- Modules and interfaces
instance SizeOf ty => SizeOf (Arg ty)
instance (SizeOf name, SizeOf e) => SizeOf (CapForm name e)
instance (SizeOf e) => SizeOf (BuiltinForm e)
instance (SizeOf n, SizeOf t, SizeOf b, SizeOf i) => SizeOf (Term n t b i)
instance (SizeOf n, SizeOf t, SizeOf b, SizeOf i) => SizeOf (Defun n t b i)
instance (SizeOf term) => SizeOf (ConstVal term)
instance (SizeOf n, SizeOf t, SizeOf b, SizeOf i) => SizeOf (DefConst n t b i)
instance (SizeOf n, SizeOf t, SizeOf b, SizeOf i) => SizeOf (DefCap n t b i)
instance (SizeOf n, SizeOf t, SizeOf b, SizeOf i) => SizeOf (Step n t b i)
instance (SizeOf n, SizeOf t, SizeOf b, SizeOf i) => SizeOf (DefPact n t b i)
instance (SizeOf t, SizeOf i) => SizeOf (DefSchema t i)
instance (SizeOf n, SizeOf i) => SizeOf (DefTable n i)
instance (SizeOf n, SizeOf t, SizeOf b, SizeOf i) => SizeOf (Def n t b i)

instance (SizeOf n, SizeOf t, SizeOf b, SizeOf i) => SizeOf (Module n t b i)

instance (SizeOf t, SizeOf i) => SizeOf (IfDefun t i)
instance (SizeOf t, SizeOf i) => SizeOf (IfDefPact t i)
instance (SizeOf n, SizeOf t, SizeOf i) => SizeOf (IfDefCap n t i)
instance (SizeOf n, SizeOf t, SizeOf b, SizeOf i) => SizeOf (IfDef n t b i)
instance (SizeOf n, SizeOf t, SizeOf b, SizeOf i) => SizeOf (Interface n t b i)

instance SizeOf Namespace
