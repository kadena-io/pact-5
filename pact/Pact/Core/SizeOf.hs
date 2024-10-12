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
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}

-- |
-- Module      :  Pact.Types.SizeOf
-- Copyright   :  (C) 2019 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--

module Pact.Core.SizeOf
  ( SizeOf(..)
  , SizeOf1(..)
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
import GHC.Int(Int(..))
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import qualified Data.Text as T
import qualified Data.Text.Unsafe as TU
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
import Pact.Core.Persistence.Types
import Pact.Core.Imports
import Pact.Core.Info
import Pact.Core.ModRefs
import Pact.Core.Namespace
import Pact.Core.Gas
import Control.Monad


-- |  Estimate of number of bytes needed to represent data type
--
-- NOTE:
-- We do not charge 1 word per field for estimating the in-memory size anymore. This is because this ends up
-- Costing the user a factor of 10 larger than the actual bytes needed to represent the structure in
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

-- | "word" is 8 bytes on 64-bit
wordSize64, wordSize :: Bytes
wordSize64 = 8
wordSize = wordSize64

-- A cbor tag is 1 byte
tagOverhead :: Bytes
tagOverhead = 1


cborArraySize :: (Foldable f, SizeOf a) => i -> SizeOfVersion -> f a -> EvalM e b i Bytes
cborArraySize i ver v = do
  -- CBOR size overhead for arrays:
  -- 1 byte for type tag (3 bits major type array, 5 bits for variable length)
  -- 4 bytes for length (this is actually a MAJOR overshoot, but this is fine)
  let arrayOverhead = 5
  !elementSizes <- foldM (\count e -> (+ count) <$> sizeOf i ver e) 0 v
  countBytes i $ arrayOverhead + elementSizes
{-# INLINE cborArraySize #-}

instance (SizeOf v) => SizeOf (Vector v) where
  sizeOf i ver v = cborArraySize i ver v

instance (SizeOf a) => SizeOf (Set a) where
  sizeOf i ver s = cborArraySize i ver s

instance (SizeOf k, SizeOf v) => SizeOf (M.Map k v) where
  sizeOf i ver m = cborArraySize i ver (M.toList m)

instance (SizeOf a, SizeOf b) => SizeOf (a,b) where
  sizeOf i ver (a,b) = do
    -- A tuple is essentially an array but with a fixed length of 2
    aBytes <- sizeOf i ver a
    bBytes <- sizeOf i ver b
    pure $ tagOverhead + aBytes + bBytes

instance (SizeOf a) => SizeOf (Maybe a) where
  sizeOf i ver e = cborArraySize i ver e

instance (SizeOf a) => SizeOf [a] where
  sizeOf i ver arr = cborArraySize i ver arr

instance SizeOf BS.ByteString where
  sizeOf i _ver bs =
    countBytes i (fromIntegral (BS.length bs) + 4) -- We're going to use an array size overhead of 4 here

instance SizeOf SBS.ShortByteString where
  sizeOf i ver = sizeOf i ver . SBS.fromShort

instance SizeOf Text where
  sizeOf i _ver t =
    -- We will
    countBytes i $ fromIntegral (TU.lengthWord8 t + 4)

instance SizeOf Integer where
  sizeOf i _ e = countBytes i $
      fromIntegral (max 64 (I# (IntLog.integerLog2# (abs e)) + 1)) `quot` 8

-- And int fits in 4 bytes
instance SizeOf Int where
  sizeOf i _ver _ = countBytes i (tagOverhead + 4)

-- Word 8 = 1 byte, so the tag overhead is enough
instance SizeOf Word8 where
  sizeOf i _ver _ = countBytes i tagOverhead

instance (SizeOf i) => SizeOf (DecimalRaw i) where
  sizeOf i ver (Decimal p m) = do
    pSize <- sizeOf i ver p
    mSize <- sizeOf i ver m
    countBytes i $ tagOverhead + pSize + mSize

instance SizeOf Int64 where
  sizeOf i _ver _ = countBytes i (tagOverhead + wordSize)


instance SizeOf Word64 where
  sizeOf i _ver _ = countBytes i (tagOverhead + wordSize)


instance SizeOf UTCTime where
  -- newtype is free
  -- Internally 'UTCTime' is just a 64-bit integer
  sizeOf i _ver _ =
    countBytes i wordSize

-- Note: a bool takes up 1 byte of space, so the tagoverhead is enough
instance SizeOf Bool where
  -- Note: this probably overestimates
  sizeOf i _ver _ = countBytes i tagOverhead

instance SizeOf () where
  sizeOf :: i -> SizeOfVersion -> () -> EvalM e b i Bytes
  sizeOf _ _ _ = pure 0

-- We can assume the amount it takes to represent this in memory
-- is something along the lines of
--  - 1 word per the number of elements
--  -
instance (SizeOf k, SizeOf v) => SizeOf (HM.HashMap k v) where
  sizeOf i ver m = cborArraySize i ver (HM.toList m)

-- Note: Atm hashset is only a newtype wrapper over hashmap with unit as the element
-- member of every entry. This means you don't pay at all for the cost of (),
-- but you do pay for the extra constructor field of holding it, hence the `hsSize` bit
-- stays roughly the same.
instance (SizeOf k) => SizeOf (HS.HashSet k) where
  sizeOf i ver hs = cborArraySize i ver (HS.toList hs)

instance (SizeOf a, SizeOf b) => SizeOf (Either a b) where
  sizeOf i ver = fmap (+ tagOverhead) . \case
    Left e -> sizeOf i ver e
    Right r -> sizeOf i ver r
  {-# INLINE sizeOf #-}

instance  (SizeOf a) => SizeOf (NE.NonEmpty a) where
  sizeOf i ver e = cborArraySize i ver e


class SizeOf1 f where
  sizeOf1 :: SizeOf a => SizeOfVersion -> f a -> Bytes

--- Pact-core instances
-- Putting some of the more annoying GADTs here
instance SizeOf (FQNameRef name) where
  sizeOf i ver c = do
    tailBytes <- case c of
      FQParsed n -> sizeOf i ver n
      FQName fqn -> sizeOf i ver fqn
    pure $ tagOverhead + tailBytes

instance SizeOf (TableSchema name) where
  sizeOf i ver c = do
    tailBytes <- case c of
      DesugaredTable n -> sizeOf i ver n
      ResolvedTable fqn -> sizeOf i ver fqn
    pure $ tagOverhead + tailBytes

instance SizeOf Literal where
  sizeOf i ver literal = do
    -- Overhead of a tag + word
    (tagOverhead +) <$> case literal of
      LString s -> sizeOf i ver s
      LInteger i' -> sizeOf i ver i'
      LDecimal d -> sizeOf i ver d
      LBool b -> sizeOf i ver b
      LUnit -> countBytes i tagOverhead

instance SizeOf LineInfo where
  sizeOf i ver (LineInfo li) = sizeOf i ver li
  {-# INLINE sizeOf #-}

-- | Note: we will _not_ charge for the size of the module
instance SizeOf ModuleCode where
  sizeOf i ver (ModuleCode m)
    | T.null m = pure 0
    | otherwise = sizeOf i ver m

deriving newtype instance SizeOf Hash
deriving newtype instance SizeOf Field
deriving newtype instance SizeOf NamespaceName
deriving newtype instance SizeOf BareName
deriving newtype instance SizeOf ModuleHash

instance SizeOf ModuleName where
  sizeOf i ver (ModuleName mn nsn) = do
    szm <- sizeOf i ver mn
    szn <- sizeOf i ver nsn
    pure (tagOverhead + szm + szn)

instance SizeOf DynamicRef where
  sizeOf i ver (DynamicRef a b) = do
    szm <- sizeOf i ver a
    szn <- sizeOf i ver b
    pure (tagOverhead + szm + szn)

instance SizeOf NameKind where
  sizeOf i ver nk = do
    namesz <- case nk of
      NBound b -> sizeOf i ver b
      NTopLevel mn mh -> (+) <$> sizeOf i ver mn <*> sizeOf i ver mh
      NModRef mn mns -> (+) <$> sizeOf i ver mn <*> sizeOf i ver mns
      NDynRef dr -> sizeOf i ver dr
    pure $ tagOverhead + namesz


instance SizeOf Name where
  sizeOf i ver (Name a b) = do
    szm <- sizeOf i ver a
    szn <- sizeOf i ver b
    pure (tagOverhead + szm + szn)


instance SizeOf QualifiedName where
  sizeOf i ver (QualifiedName a b) = do
    szm <- sizeOf i ver a
    szn <- sizeOf i ver b
    pure (tagOverhead + szm + szn)

instance SizeOf DynamicName where
  sizeOf i ver (DynamicName a b) = do
    szm <- sizeOf i ver a
    szn <- sizeOf i ver b
    pure (tagOverhead + szm + szn)

instance SizeOf ParsedName where
  sizeOf i ver pn = do
    namesz <- case pn of
      BN b -> sizeOf i ver b
      QN n -> sizeOf i ver n
      DN n -> sizeOf i ver n
    pure $ tagOverhead + namesz

instance SizeOf ParsedTyName where
  sizeOf i ver pn = do
    namesz <- case pn of
      TBN b -> sizeOf i ver b
      TQN n -> sizeOf i ver n
    pure $ tagOverhead + namesz


instance SizeOf FullyQualifiedName where
  sizeOf i ver (FullyQualifiedName a b c) = do
    szm <- sizeOf i ver a
    szn <- sizeOf i ver b
    szc <- sizeOf i ver c
    pure (tagOverhead + szm + szn + szc)

-- Prim types are at most 2 bytes
instance SizeOf PrimType where
  sizeOf i _ver _ = countBytes i tagOverhead

instance SizeOf Schema where
  sizeOf i ver (Schema a b) = do
    szm <- sizeOf i ver a
    szn <- sizeOf i ver b
    pure (tagOverhead + szm + szn)

instance SizeOf Type where
  sizeOf i ver ty = do
    namesz <- case ty of
      TyPrim p -> sizeOf i ver p
      TyList t -> sizeOf i ver t
      TyAnyList -> pure tagOverhead
      TyModRef mrs -> sizeOf i ver mrs
      TyObject sc -> sizeOf i ver sc
      TyAnyObject -> pure tagOverhead
      TyTable sc -> sizeOf i ver sc
      TyCapToken -> pure tagOverhead
      TyAny -> pure tagOverhead
    pure $ tagOverhead + namesz

-- defpacts
deriving newtype instance SizeOf DefPactId

instance (SizeOf n, SizeOf v) => SizeOf (DefPactContinuation n v) where
  sizeOf i ver (DefPactContinuation a b) = do
    szm <- sizeOf i ver a
    szn <- sizeOf i ver b
    pure (tagOverhead + szm + szn)

deriving newtype instance SizeOf ChainId

instance SizeOf Provenance where
  sizeOf i ver (Provenance a b) = do
    szm <- sizeOf i ver a
    szn <- sizeOf i ver b
    pure (tagOverhead + szm + szn)
instance SizeOf Yield where
  sizeOf i ver (Yield a b c) = do
    szm <- sizeOf i ver a
    szn <- sizeOf i ver b
    szc <- sizeOf i ver c
    pure (tagOverhead + szm + szn + szc)

instance SizeOf DefPactExec where
  sizeOf i ver (DefPactExec a b c d e f g) = do
      sza <- sizeOf i ver a
      szb <- sizeOf i ver b
      szc <- sizeOf i ver c
      szd <- sizeOf i ver d
      sze <- sizeOf i ver e
      szf <- sizeOf i ver f
      szg <- sizeOf i ver g
      pure (tagOverhead + sza + szb + szc + szd + sze + szf + szg)

-- spaninfo
instance SizeOf SpanInfo where
  sizeOf i ver (SpanInfo a b c d) = do
    szm <- sizeOf i ver a
    szn <- sizeOf i ver b
    szc <- sizeOf i ver c
    szd <- sizeOf i ver d
    pure (tagOverhead + szm + szn + szc + szd)

-- builtins
instance SizeOf CoreBuiltin where
  sizeOf _i _ver _ = pure (tagOverhead + 1)
  {-# INLINE sizeOf #-}

instance SizeOf ReplOnlyBuiltin where
  sizeOf _i _ver _ = pure (tagOverhead + 1)

instance SizeOf b => SizeOf (ReplBuiltin b) where

  sizeOf i ver = fmap (+ tagOverhead) . \case
    RBuiltinWrap b -> sizeOf i ver b
    RBuiltinRepl r -> sizeOf i ver r


-- Import
instance SizeOf Import where
  sizeOf i ver (Import a b c) = do
    szm <- sizeOf i ver a
    szn <- sizeOf i ver b
    szc <- sizeOf i ver c
    pure (tagOverhead + szm + szn + szc)

-- guards
deriving newtype instance SizeOf PublicKeyText

instance SizeOf KeySetName where
  sizeOf i ver (KeySetName a b) = do
    szm <- sizeOf i ver a
    szn <- sizeOf i ver b
    pure (tagOverhead + szm + szn)

instance (SizeOf name, SizeOf v) => SizeOf (UserGuard name v) where
  sizeOf i ver (UserGuard a b) = do
    szm <- sizeOf i ver a
    szn <- sizeOf i ver b
    pure (tagOverhead + szm + szn)

instance (SizeOf name, SizeOf v) => SizeOf (CapabilityGuard name v) where
  sizeOf i ver (CapabilityGuard a b c) = do
    szm <- sizeOf i ver a
    szn <- sizeOf i ver b
    szc <- sizeOf i ver c
    pure (tagOverhead + szm + szn + szc)

instance SizeOf KSPredicate where
  sizeOf i ver = \case
    CustomPredicate pn -> sizeOf i ver pn
    _ -> pure (tagOverhead + 1)

instance SizeOf KeySet where
  sizeOf i ver (KeySet a b) = do
    szm <- sizeOf i ver a
    szn <- sizeOf i ver b
    pure (tagOverhead + szm + szn)

instance SizeOf ModuleGuard where
  sizeOf i ver (ModuleGuard a b) = do
    szm <- sizeOf i ver a
    szn <- sizeOf i ver b
    pure (tagOverhead + szm + szn)

instance SizeOf DefPactGuard where
  sizeOf i ver (DefPactGuard a b) = do
    szm <- sizeOf i ver a
    szn <- sizeOf i ver b
    pure (tagOverhead + szm + szn)

instance (SizeOf name, SizeOf v) => SizeOf (Guard name v) where
  sizeOf i ver = fmap (+ tagOverhead) . \case
    GKeyset d -> sizeOf i ver d
    GKeySetRef d -> sizeOf i ver d
    GUserGuard d -> sizeOf i ver d
    GCapabilityGuard d -> sizeOf i ver d
    GDefPactGuard d -> sizeOf i ver d
    GModuleGuard d -> sizeOf i ver d

-- Caps
instance (SizeOf name, SizeOf v) => SizeOf (CapToken name v) where
  sizeOf i ver (CapToken a b) = do
    szm <- sizeOf i ver a
    szn <- sizeOf i ver b
    pure (tagOverhead + szm + szn)

instance SizeOf n => SizeOf (DefManagedMeta n) where
  sizeOf i ver = fmap (+ tagOverhead) . \case
    DefManagedMeta a b -> do
      szm <- sizeOf i ver a
      szn <- sizeOf i ver b
      pure (tagOverhead + szm + szn)
    AutoManagedMeta -> pure tagOverhead

instance SizeOf n => SizeOf (DefCapMeta n) where
  sizeOf i ver = fmap (+ tagOverhead) . \case
    DefEvent -> pure tagOverhead
    DefManaged dm -> sizeOf i ver dm
    Unmanaged -> pure tagOverhead

instance SizeOf (Governance n) where
  sizeOf i ver = fmap (+ tagOverhead) . \case
    KeyGov kg -> sizeOf i ver kg
    CapGov cg -> sizeOf i ver cg

instance SizeOf ModRef where
  sizeOf i ver (ModRef a b) = do
    szm <- sizeOf i ver a
    szn <- sizeOf i ver b
    pure (tagOverhead + szm + szn)


instance SizeOf PactValue where
  sizeOf i ver pactValue = fmap (+ tagOverhead) $ case pactValue of
    PLiteral l -> sizeOf i ver l
    PObject obj -> sizeOf i ver obj
    PList l -> sizeOf i ver l
    PGuard g -> sizeOf i ver g
    PModRef m -> sizeOf i ver m
    PCapToken t -> sizeOf i ver t
    PTime t -> sizeOf i ver t

-- Modules and interfaces
instance (SizeOf ty, SizeOf i) => SizeOf (Arg ty i) where
  sizeOf i ver (Arg a b c) = do
    szm <- sizeOf i ver a
    szn <- sizeOf i ver b
    szc <- sizeOf i ver c
    pure (tagOverhead + szm + szn + szc)

instance (SizeOf e) => SizeOf (BuiltinForm e) where
  sizeOf i ver = \case
    CAnd a b -> do
      sza <- sizeOf i ver a
      szb <- sizeOf i ver b
      pure (tagOverhead + sza + szb)

    COr a b -> do
      sza <- sizeOf i ver a
      szb <- sizeOf i ver b
      pure (tagOverhead + sza + szb)

    CIf a b c -> do
      sza <- sizeOf i ver a
      szb <- sizeOf i ver b
      szc <- sizeOf i ver c
      pure (tagOverhead + sza + szb + szc)

    CEnforce a b -> do
      sza <- sizeOf i ver a
      szb <- sizeOf i ver b
      pure (tagOverhead + sza + szb)

    CWithCapability a b -> do
      sza <- sizeOf i ver a
      szb <- sizeOf i ver b
      pure (tagOverhead + sza + szb)

    CCreateUserGuard a -> sizeOf i ver a

    CEnforceOne a b -> do
      sza <- sizeOf i ver a
      szb <- sizeOf i ver b
      pure (tagOverhead + sza + szb)

    CTry a b -> do
      sza <- sizeOf i ver a
      szb <- sizeOf i ver b
      pure (tagOverhead + sza + szb)

instance (SizeOf n, SizeOf t, SizeOf b, SizeOf i) => SizeOf (Term n t b i) where
  sizeOf i ver = \case
    Var a b -> do
      sza <- sizeOf i ver a
      szb <- sizeOf i ver b
      pure (tagOverhead + sza + szb)
    Lam a b c -> do
      sza <- sizeOf i ver a
      szb <- sizeOf i ver b
      szc <- sizeOf i ver c
      pure (tagOverhead + sza + szb + szc)
    Let a b c d -> do
      sza <- sizeOf i ver a
      szb <- sizeOf i ver b
      szc <- sizeOf i ver c
      szd <- sizeOf i ver d
      pure (tagOverhead + sza + szb + szc + szd)
    App a b c -> do
      sza <- sizeOf i ver a
      szb <- sizeOf i ver b
      szc <- sizeOf i ver c
      pure (tagOverhead + sza + szb + szc)
    BuiltinForm a b -> do
      sza <- sizeOf i ver a
      szb <- sizeOf i ver b
      pure (tagOverhead + sza + szb)
    Constant a b -> do
      sza <- sizeOf i ver a
      szb <- sizeOf i ver b
      pure (tagOverhead + sza + szb)
    Builtin a b -> do
      sza <- sizeOf i ver a
      szb <- sizeOf i ver b
      pure (tagOverhead + sza + szb)
    Sequence a b c -> do
      sza <- sizeOf i ver a
      szb <- sizeOf i ver b
      szc <- sizeOf i ver c
      pure (tagOverhead + sza + szb + szc)
    Nullary a b -> do
      sza <- sizeOf i ver a
      szb <- sizeOf i ver b
      pure (tagOverhead + sza + szb)
    ListLit a b -> do
      sza <- sizeOf i ver a
      szb <- sizeOf i ver b
      pure (tagOverhead + sza + szb)
    ObjectLit a b -> do
      sza <- sizeOf i ver a
      szb <- sizeOf i ver b
      pure (tagOverhead + sza + szb)
    InlineValue a b -> do
      sza <- sizeOf i ver a
      szb <- sizeOf i ver b
      pure (tagOverhead + sza + szb)

instance (SizeOf n, SizeOf t, SizeOf b, SizeOf i) => SizeOf (Defun n t b i) where
  sizeOf i ver (Defun a b c d) = do
      sza <- sizeOf i ver a
      szb <- sizeOf i ver b
      szc <- sizeOf i ver c
      szd <- sizeOf i ver d
      pure (tagOverhead + sza + szb + szc + szd)

instance (SizeOf term) => SizeOf (ConstVal term) where
  sizeOf i ver = fmap (+ tagOverhead) . \case
    EvaledConst cv -> sizeOf i ver cv
    TermConst cv -> sizeOf i ver cv

instance (SizeOf n, SizeOf t, SizeOf b, SizeOf i) => SizeOf (DefConst n t b i) where
  sizeOf i ver (DefConst a b c) = do
      sza <- sizeOf i ver a
      szb <- sizeOf i ver b
      szc <- sizeOf i ver c
      pure (tagOverhead + sza + szb + szc)

instance (SizeOf n, SizeOf t, SizeOf b, SizeOf i) => SizeOf (DefCap n t b i) where
  sizeOf i ver (DefCap a b c d e) = do
      sza <- sizeOf i ver a
      szb <- sizeOf i ver b
      szc <- sizeOf i ver c
      szd <- sizeOf i ver d
      sze <- sizeOf i ver e
      pure (tagOverhead + sza + szb + szc + szd + sze)

instance (SizeOf n, SizeOf t, SizeOf b, SizeOf i) => SizeOf (Step n t b i) where
  sizeOf i ver = fmap (+ tagOverhead) . \case
    Step term -> sizeOf i ver term
    StepWithRollback a b -> do
      sza <- sizeOf i ver a
      szb <- sizeOf i ver b
      pure (sza + szb)

instance (SizeOf n, SizeOf t, SizeOf b, SizeOf i) => SizeOf (DefPact n t b i) where
  sizeOf i ver (DefPact a b c d) = do
      sza <- sizeOf i ver a
      szb <- sizeOf i ver b
      szc <- sizeOf i ver c
      szd <- sizeOf i ver d
      pure (tagOverhead + sza + szb + szc + szd)

instance (SizeOf t, SizeOf i) => SizeOf (DefSchema t i) where
  sizeOf i ver (DefSchema a b c) = do
      sza <- sizeOf i ver a
      szb <- sizeOf i ver b
      szc <- sizeOf i ver c
      pure (tagOverhead + sza + szb + szc)

instance (SizeOf i) => SizeOf (DefTable n i) where
  sizeOf i ver (DefTable a b c) = do
      sza <- sizeOf i ver a
      szb <- sizeOf i ver b
      szc <- sizeOf i ver c
      pure (tagOverhead + sza + szb + szc)

instance (SizeOf n, SizeOf t, SizeOf b, SizeOf i) => SizeOf (Def n t b i) where
  sizeOf i ver = fmap (+ tagOverhead) . \case
   Dfun d -> sizeOf i ver d
   DConst d -> sizeOf i ver d
   DCap d -> sizeOf i ver d
   DPact d -> sizeOf i ver d
   DSchema d -> sizeOf i ver d
   DTable d -> sizeOf i ver d

instance (SizeOf n, SizeOf t, SizeOf b, SizeOf i) => SizeOf (Module n t b i) where
  sizeOf i ver (Module a b c d e f g h i' j) = do
      sza <- sizeOf i ver a
      szb <- sizeOf i ver b
      szc <- sizeOf i ver c
      szd <- sizeOf i ver d
      sze <- sizeOf i ver e
      szf <- sizeOf i ver f
      szg <- sizeOf i ver g
      szh <- sizeOf i ver h
      szi' <- sizeOf i ver i'
      szj <- sizeOf i ver j
      pure (tagOverhead + sza + szb + szc + szd + sze + szf + szg + szh + szi' + szj)

instance (SizeOf t, SizeOf i) => SizeOf (IfDefun t i) where
  sizeOf i ver (IfDefun a b c) = do
      sza <- sizeOf i ver a
      szb <- sizeOf i ver b
      szc <- sizeOf i ver c
      pure (tagOverhead + sza + szb + szc)

instance (SizeOf t, SizeOf i) => SizeOf (IfDefPact t i) where
  sizeOf i ver (IfDefPact a b c) = do
      sza <- sizeOf i ver a
      szb <- sizeOf i ver b
      szc <- sizeOf i ver c
      pure (tagOverhead + sza + szb + szc)

instance (SizeOf t, SizeOf i) => SizeOf (IfDefCap n t i) where
  sizeOf i ver (IfDefCap a b c d) = do
      sza <- sizeOf i ver a
      szb <- sizeOf i ver b
      szc <- sizeOf i ver c
      szd <- sizeOf i ver d
      pure (tagOverhead + sza + szb + szc + szd)

instance (SizeOf n, SizeOf t, SizeOf b, SizeOf i) => SizeOf (IfDef n t b i) where
  sizeOf i ver = fmap (+ tagOverhead) . \case
    IfDfun d -> sizeOf i ver d
    IfDCap d -> sizeOf i ver d
    IfDConst d -> sizeOf i ver d
    IfDSchema d -> sizeOf i ver d
    IfDPact d -> sizeOf i ver d

instance (SizeOf n, SizeOf t, SizeOf b, SizeOf i) => SizeOf (Interface n t b i) where
  sizeOf i ver (Interface a b c d e f g) = do
      sza <- sizeOf i ver a
      szb <- sizeOf i ver b
      szc <- sizeOf i ver c
      szd <- sizeOf i ver d
      sze <- sizeOf i ver e
      szf <- sizeOf i ver f
      szg <- sizeOf i ver g
      pure (tagOverhead + sza + szb + szc + szd + sze + szf + szg)

instance SizeOf Namespace where
  sizeOf i ver (Namespace a b c) = do
      sza <- sizeOf i ver a
      szb <- sizeOf i ver b
      szc <- sizeOf i ver c
      pure (tagOverhead + sza + szb + szc)

instance (SizeOf b, SizeOf i) => SizeOf (ModuleData b i) where
  sizeOf i ver = \case
    ModuleData a b -> do
      sza <- sizeOf i ver a
      szb <- sizeOf i ver b
      pure (tagOverhead + sza + szb)
    InterfaceData a b -> do
      sza <- sizeOf i ver a
      szb <- sizeOf i ver b
      pure (tagOverhead + sza + szb)
