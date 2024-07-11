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

import Data.Decimal
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
import Pact.Core.Gas


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
data SizeOfVersion
  = SizeOfV0
  deriving (Show, Eq)

instance Pretty SizeOfVersion where
  pretty = viaShow

type Bytes = Word64

countBytes :: i -> Bytes -> EvalM e b i Bytes
countBytes i bytes = do
  chargeGasArgs i GCountBytes
  pure bytes

class SizeOf t where
  sizeOf :: forall e b i. i -> SizeOfVersion -> t -> EvalM e b i Bytes
  default sizeOf :: forall e b i.(Generic t, GSizeOf (Rep t)) => i -> SizeOfVersion -> t -> EvalM e b i Bytes
  sizeOf i v a = gsizeOf i v (from a)

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
  sizeOf i ver v = do
    let rawVecSize = (7 + vectorLength) * wordSize
    spineSize <- countBytes i rawVecSize
    elementSizes <- traverse (sizeOf i ver) v
    pure $ spineSize + sum elementSizes
    where
      vectorLength = fromIntegral (V.length v)

instance (SizeOf a) => SizeOf (Set a) where
  sizeOf i ver s = do
    let !setSizeOverhead = (1 + 3 * setLength) * wordSize
    spineSize <- countBytes i setSizeOverhead
    elementSizes <- traverse (sizeOf i ver) (S.toList s)
    pure $ spineSize + sum elementSizes
    where
      setLength = fromIntegral (S.size s)

instance (SizeOf k, SizeOf v) => SizeOf (M.Map k v) where
  sizeOf i ver m = do
    let !mapSizeOverhead = 6 * mapLength * wordSize
    spineSize <- countBytes i mapSizeOverhead
    elementSizes <- traverse (\(k,v) -> liftA2 (+) (sizeOf i ver k) (sizeOf i ver v)) (M.toList m)
    pure $ spineSize + sum elementSizes
    where
      mapLength = fromIntegral (M.size m)

instance (SizeOf a, SizeOf b) => SizeOf (a,b) where
  sizeOf i ver (a,b) = do
    headBytes <- countBytes i (constructorCost 3)
    aBytes <- sizeOf i ver a
    bBytes <- sizeOf i ver b
    pure $ headBytes + aBytes + bBytes

instance (SizeOf a) => SizeOf (Maybe a) where
  sizeOf i ver (Just e) =
    liftA2 (+) (countBytes i (constructorCost 1)) (sizeOf i ver e)
  sizeOf i _ver Nothing =
    countBytes i (constructorCost 0)

instance (SizeOf a) => SizeOf [a] where
  sizeOf i ver arr = do
    let listSzOverhead = (1 + (3 * listLength)) * wordSize
    spineSize <- countBytes i listSzOverhead
    elementSizes <- traverse (sizeOf i ver) arr
    pure $ spineSize + sum elementSizes
    where
      listLength = fromIntegral (L.length arr)

instance SizeOf BS.ByteString where
  sizeOf i _ver bs =
    countBytes i byteStringSize
    where
      byteStringSize = (9 * wordSize) + byteStringLength

      -- 'UTF8.length' returns different values than 'BS.length'. Moreoever,
      -- 'BS.length' is \(O(1)\) but 'UTF8.length' is \(O(n)\), with a pretty
      -- bad constant factor.
      --
      byteStringLength = fromIntegral (UTF8.length bs)

instance SizeOf SBS.ShortByteString where
  sizeOf i ver = sizeOf i ver . SBS.fromShort

instance SizeOf Text where
  sizeOf i _ver t =
    countBytes i $ (6 * wordSize) + (2 * (fromIntegral (T.length t)))

instance SizeOf Integer where
  sizeOf i ver e = countBytes i $ case ver of
    SizeOfV0 ->
      fromIntegral (max 64 (I# (IntLog.integerLog2# (abs e)) + 1)) `quot` 8

instance SizeOf Int where
  sizeOf i _ver _ = countBytes i $ 2 * wordSize

instance SizeOf Word8 where
  sizeOf i _ver _ = countBytes i $ 2 * wordSize

instance (SizeOf i) => SizeOf (DecimalRaw i) where
  sizeOf i ver (Decimal p m) = do
    constructorSize <- countBytes i (constructorCost 2)
    pSize <- sizeOf i ver p
    mSize <- sizeOf i ver m
    pure $ constructorSize + pSize + mSize

instance SizeOf Int64 where
  -- Assumes 64-bit machine
  sizeOf i _ver _ = countBytes i $ 2 * wordSize


instance SizeOf Word64 where
  -- Assumes 64-bit machine
  sizeOf i _ver _ = countBytes i $ 2 * wordSize


instance SizeOf UTCTime where
  -- newtype is free
  -- Internally 'UTCTime' is just a 64-bit count of 'microseconds'
  sizeOf i ver ti =
    liftA2 (+) (countBytes i (constructorCost 1)) (sizeOf i ver (toPosixTimestampMicros ti))

instance SizeOf Bool where
  sizeOf i _ver _ = countBytes i wordSize

instance SizeOf () where
  sizeOf _ _ _ = pure 0

-- See ghc memory note above
-- as well as https://blog.johantibell.com/2011/06/memory-footprints-of-some-common-data.html
-- for both hash sets and hashmaps.
instance (SizeOf k, SizeOf v) => SizeOf (HM.HashMap k v) where
  sizeOf i ver m = do
    spineSize <- countBytes i hmOverhead
    elementSizes <- traverse (\(k,v) -> liftA2 (+) (sizeOf i ver k) (sizeOf i ver v)) (HM.toList m)
    pure $ spineSize + sum elementSizes
    where
      !hmOverhead = (5 * hmLength + 4 * (hmLength - 1)) * wordSize
      hmLength = fromIntegral (HM.size m)

-- Note: Atm hashset is only a newtype wrapper over hashmap with unit as the element
-- member of every entry. This means you don't pay at all for the cost of (),
-- but you do pay for the extra constructor field of holding it, hence the `hsSize` bit
-- stays roughly the same.
instance (SizeOf k) => SizeOf (HS.HashSet k) where
  sizeOf i ver hs = do
    spineSize <- countBytes i hsSizeOverhead
    elementSizes <- traverse (sizeOf i ver) (HS.toList hs)
    pure $ spineSize + sum elementSizes
    where
      hsSizeOverhead = (5 + hsLength + 4 * (hsLength - 1)) * wordSize
      hsLength = fromIntegral (HS.size hs)

instance (SizeOf a, SizeOf b) => SizeOf (Either a b)

instance  (SizeOf a) => SizeOf (NE.NonEmpty a) where
  sizeOf i ver (a NE.:| rest) = do
    constructorSize <- countBytes i (constructorCost 2)
    aSize <- sizeOf i ver a
    restSize <- sizeOf i ver rest
    pure $ constructorSize + aSize + restSize


class SizeOf1 f where
  sizeOf1 :: SizeOf a => SizeOfVersion -> f a -> Bytes

-- Generic deriving
class GSizeOf f where
  gsizeOf :: forall e b i a. i -> SizeOfVersion -> f a -> EvalM e b i Bytes

-- For sizes of products, we'll calculate the size at the leaves,
-- and simply add 1 extra word for every leaf.
instance (GSizeOf f, GSizeOf g) => GSizeOf (f :*: g) where
  gsizeOf i ver (a :*: b) = liftA2 (+) (gsizeOf i ver a) (gsizeOf i ver b)

-- Sums we can just branch recursively as usual
-- Ctor information is one level lower.
instance (GSizeOf a, GSizeOf b) => GSizeOf (a :+: b) where
  gsizeOf i ver = \case
    L1 a -> gsizeOf i ver a
    R1 b -> gsizeOf i ver b


-- No fields ctors are shared.
-- We are ok charging a bit extra here.
instance {-# OVERLAPS #-} GSizeOf (C1 c U1) where
  gsizeOf i _ver (M1 _) = countBytes i wordSize

-- Regular constructors pay the header cost
-- and 1 word for each field, which is added @ the leaves.
instance (GSizeOf f) => GSizeOf (C1 c f) where
  gsizeOf i ver (M1 p) = liftA2 (+) (countBytes i headerCost) (gsizeOf i ver p)

-- Metainfo about selectors
instance (GSizeOf f) => GSizeOf (S1 c f) where
  gsizeOf i ver (M1 p) = gsizeOf i ver p

-- Metainfo about the whole data type.
instance (GSizeOf f) => GSizeOf (D1 c f) where
  gsizeOf i ver (M1 p) = gsizeOf i ver p

-- Single field, means size of field + 1 word.
instance (SizeOf c) => GSizeOf (K1 i c) where
  gsizeOf i ver (K1 c) = liftA2 (+) (sizeOf i ver c) (countBytes i wordSize)

-- No-argument constructors are always shared by ghc
-- so they don't really allocate.
-- However, this instance is used whenever we hit a leaf
-- that _does_ in fact allocate 1 word for the field itself.
-- 0-cost constructor `SizeOf` is caught by the `GSizeOf (C1 c U1)`
-- instance
instance GSizeOf U1 where
  gsizeOf i _ver U1 = countBytes i wordSize

--- Pact-core instances
-- Putting some of the more annoying GADTs here
instance SizeOf (FQNameRef name) where
  sizeOf i ver c = do
    headBytes <- countBytes i (headerCost + wordSize)
    tailBytes <- case c of
      FQParsed n -> sizeOf i ver n
      FQName fqn -> sizeOf i ver fqn
    pure $ headBytes + tailBytes

instance SizeOf (TableSchema name) where
  sizeOf i ver c = do
    headBytes <- countBytes i (headerCost + wordSize)
    tailBytes <- case c of
      DesugaredTable n -> sizeOf i ver n
      ResolvedTable fqn -> sizeOf i ver fqn
    pure $ headBytes + tailBytes

instance SizeOf Literal where
  sizeOf i ver literal = fmap (constructorCost 1 +) $ case literal of
    LString s -> sizeOf i ver s
    LInteger i' -> sizeOf i ver i'
    LDecimal d -> sizeOf i ver d
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
instance SizeOf ReplOnlyBuiltin
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
  sizeOf i ver pactValue = fmap (constructorCost 1 +) $ case pactValue of
    PLiteral l -> sizeOf i ver l
    PObject obj -> sizeOf i ver obj
    PList l -> sizeOf i ver l
    PGuard g -> sizeOf i ver g
    PModRef m -> sizeOf i ver m
    PCapToken t -> sizeOf i ver t
    PTime t -> sizeOf i ver t

-- Modules and interfaces
instance (SizeOf ty, SizeOf i) => SizeOf (Arg ty i)
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

