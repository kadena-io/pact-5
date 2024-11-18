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
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- |
-- Module      :  Pact.Types.SizeOf
-- Copyright   :  (C) 2019 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--

module Pact.Core.SizeOf
  ( sizeOf
  , Bytes
  , wordSize
  , SizeOfVersion(..)
  , SizeOf(..)
  ) where

import Data.Decimal
import Data.Int (Int64)
import Data.Set (Set)
import Data.Text (Text)
import Pact.Time
import Data.Vector (Vector)
import Data.Word (Word8, Word64)
import GHC.Int(Int(..))
import Control.Monad.ST
import Control.Monad.Reader hiding (lift)
import Control.Monad.Except
import Data.Foldable
import Control.Monad.Trans(lift)
import Data.STRef
import Data.IORef
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
import Pact.Core.Environment
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
import Pact.Core.SizeOf.Deriving
import Pact.Core.Gas
import Control.Monad


-- |  Estimate of number of bytes needed to represent data type
--
-- NOTE:
-- We do not charge 1 word per field for estimating the in-memory size anymore. This is because this ends up
-- Costing the user a factor of 10 larger than the actual bytes needed to represent the structure in
--
--   Size estimates determine the gas cost of various operations,
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

type Bytes = SatWord

{-# INLINABLE sizeOf #-}

data SizeOfEnv s
  = SizeOfEnv
  { _szLimit :: !SatWord
  , _szVer :: !SizeOfVersion
  , _szCountRef :: !(STRef s SatWord)
  } deriving (Eq)

newtype ByteLimitExceeded
  = ByteLimitExceeded SatWord
  deriving (Eq, Show)

newtype SizeOfM s a
  = SizeOfM (ReaderT (SizeOfEnv s) (ExceptT ByteLimitExceeded (ST s)) a)
  deriving (Functor, Applicative, Monad, MonadReader (SizeOfEnv s), MonadError ByteLimitExceeded)

sizeOf :: SizeOf a => i -> SizeOfVersion -> a -> EvalM e b i Bytes
sizeOf info ver v = do
  genv <- viewEvalEnv eeGasEnv
  let milliGasPerByte = _gcSizeOfBytePenalty $ _gmGasCostConfig (_geGasModel genv)
  MilliGas currGas <- liftIO (readIORef (_geGasRef genv))
  let byteLimit = case _gmGasLimit (_geGasModel genv) of
        Nothing -> maxBound
        Just (MilliGasLimit (MilliGas limit)) ->
          (limit - currGas) `div` milliGasPerByte
  let byteAmt = runST $ do
        r <- newSTRef 0
        let env = SizeOfEnv byteLimit ver r
        let (SizeOfM e) = estimateSize v
        runExceptT (runReaderT e env) >>= \case
          Left ble -> pure (Left ble)
          _ -> Right <$> readSTRef r
  case byteAmt of
    Right b -> pure b
    Left (ByteLimitExceeded blim) -> do
      -- Note: this will raise a gas limit exceeded
      chargeGasArgs info (GAConstant (MilliGas (blim * milliGasPerByte)))
      pure blim


-- | Count bytes up to a limit.
--   NOTE: do not change the name of this function without fixing `SizeOf.Deriving.hs`
countBytes :: SatWord -> SizeOfM s ()
countBytes bytes = do
  SizeOfEnv lim _ ref <- ask
  !currCount <- SizeOfM ((lift . lift) (readSTRef ref))
  let !newTotal = bytes + currCount
  SizeOfM ((lift . lift) (writeSTRef ref newTotal))
  when (newTotal > lim) $ throwError (ByteLimitExceeded newTotal)

-- | For a small data type (that is, < 24 fields), there's only a need for a 1-byte
--   Overhead, since it is represented as a list of items, with `n < 24` elements.
--
--  _most data types_ qualify as this.
--   NOTE: do not change the name of this function without fixing `SizeOf.Deriving.hs`
addSmallTagOverhead :: SizeOfM s ()
addSmallTagOverhead = countBytes tagOverhead

-- | Most algebraic data types take up 1 small tag tag + 1 byte for their word tag
--   NOTE: do not change the name of this function without fixing `SizeOf.Deriving.hs`
adtTagOverhead :: SizeOfM s ()
adtTagOverhead = countBytes (tagOverhead + 1)

class SizeOf t where
  estimateSize :: t -> SizeOfM s ()

-- | "word" is 8 bytes on 64-bit
wordSize64, wordSize :: Bytes
wordSize64 = 8
wordSize = wordSize64

-- A cbor tag is 1 byte
tagOverhead :: Bytes
tagOverhead = 1


cborArraySize :: (Foldable f, SizeOf a) => f a -> SizeOfM s ()
cborArraySize v = do
  -- CBOR size overhead for arrays:
  -- 1 byte for type tag (3 bits major type array, 5 bits for variable length)
  -- 4 bytes for length (this is actually a MAJOR overshoot, but this is fine)
  let arrayOverhead = 5
  countBytes arrayOverhead
  traverse_ estimateSize v
{-# INLINE cborArraySize #-}

instance (SizeOf v) => SizeOf (Vector v) where
  estimateSize v = cborArraySize v

instance (SizeOf a) => SizeOf (Set a) where
  estimateSize v = cborArraySize v

instance (SizeOf k, SizeOf v) => SizeOf (M.Map k v) where
  estimateSize m = cborArraySize (M.toList m)

instance (SizeOf a, SizeOf b) => SizeOf (a,b) where
  estimateSize (a, b) = do
    addSmallTagOverhead
    estimateSize a
    estimateSize b

instance (SizeOf a) => SizeOf [a] where
  estimateSize arr = cborArraySize arr

instance SizeOf BS.ByteString where
  estimateSize bs =
    countBytes (fromIntegral (BS.length bs) + 4) -- We're going to use an array size overhead of 4 here

instance SizeOf SBS.ShortByteString where
  estimateSize = estimateSize . SBS.fromShort

instance SizeOf Text where
  estimateSize t =
    -- We will rock you
    countBytes $ fromIntegral (TU.lengthWord8 t + 4)

instance SizeOf Integer where
  estimateSize e = countBytes $
      fromIntegral (max 64 (I# (IntLog.integerLog2# (abs e)) + 1)) `quot` 8

-- And int fits in 4 bytes
instance SizeOf Int where
  estimateSize _ = countBytes (tagOverhead + 4)

-- Word 8 = 1 byte, so the tag overhead is enough
instance SizeOf Word8 where
  estimateSize _ = countBytes tagOverhead

instance SizeOf Int64 where
  estimateSize _ = countBytes (tagOverhead + wordSize)


instance SizeOf Word64 where
  estimateSize _ = countBytes (tagOverhead + wordSize)


instance SizeOf UTCTime where
  -- newtype is free
  -- Internally 'UTCTime' is just a 64-bit integer
  estimateSize _ =
    countBytes wordSize

-- Note: a bool takes up 1 byte of space, so the tagoverhead is enough
instance SizeOf Bool where
  -- Note: this probably overestimates
  estimateSize _ = countBytes tagOverhead

instance SizeOf () where
  estimateSize _ = pure ()

-- We can assume the amount it takes to represent this in memory
-- is something along the lines of
--  - 1 word per the number of elements
--  -
instance (SizeOf k, SizeOf v) => SizeOf (HM.HashMap k v) where
  estimateSize m = cborArraySize (HM.toList m)

-- Note: Atm hashset is only a newtype wrapper over hashmap with unit as the element
-- member of every entry. This means you don't pay at all for the cost of (),
-- but you do pay for the extra constructor field of holding it, hence the `hsSize` bit
-- stays roughly the same.
instance (SizeOf k) => SizeOf (HS.HashSet k) where
  estimateSize hs = cborArraySize (HS.toList hs)


instance  (SizeOf a) => SizeOf (NE.NonEmpty a) where
  estimateSize e = cborArraySize e

------------------------------------------------------------------------------
-- SizeOf instances
------------------------------------------------------------------------------
makeSizeOf ''Either
makeSizeOf ''Maybe
makeSizeOf ''DecimalRaw
makeSizeOf ''Literal

instance SizeOf LineInfo where
  estimateSize (LineInfo li) = estimateSize li
  {-# INLINE estimateSize #-}

-- | Note: we will _not_ charge for the size of the module
instance SizeOf ModuleCode where
  estimateSize (ModuleCode m)
    | T.null m = pure ()
    | otherwise = estimateSize m

deriving newtype instance SizeOf Hash
deriving newtype instance SizeOf Field
deriving newtype instance SizeOf NamespaceName
deriving newtype instance SizeOf BareName
deriving newtype instance SizeOf ModuleHash

makeSizeOf ''ModuleName
makeSizeOf ''QualifiedName
makeSizeOf ''DynamicName
makeSizeOf ''DynamicRef
makeSizeOf ''ParsedName
makeSizeOf ''ParsedTyName
makeSizeOf ''FullyQualifiedName
makeSizeOf ''NameKind
makeSizeOf ''Name

-- Prim types are at most 2 bytes
instance SizeOf PrimType where
  estimateSize _ = countBytes tagOverhead

-- These are mutually recursive
$(concat <$> traverse makeSizeOf [''Schema, ''Type])

deriving newtype instance SizeOf DefPactId
deriving newtype instance SizeOf ChainId

--- Note: SizeOf deriving does not support GADTs
instance SizeOf (FQNameRef name) where
  estimateSize c = do
    adtTagOverhead
    case c of
      FQParsed n -> estimateSize n
      FQName fqn -> estimateSize fqn


--- Note: SizeOf deriving does not support GADTs
instance SizeOf (TableSchema name) where
  estimateSize c = do
    adtTagOverhead
    case c of
      DesugaredTable n -> estimateSize n
      ResolvedTable fqn -> estimateSize fqn


-- defpacts


makeSizeOf ''SpanInfo

-- Note: this is a pass through instance, since this is repl-only
instance SizeOf FileLocSpanInfo where
  estimateSize (FileLocSpanInfo _f s) =
    estimateSize s

-- builtins
instance SizeOf CoreBuiltin where
  estimateSize _ = countBytes (tagOverhead + 1)
  {-# INLINE estimateSize #-}

instance SizeOf ReplOnlyBuiltin where
  estimateSize _ = countBytes (tagOverhead + 1)

makeSizeOf ''ReplBuiltin
makeSizeOf ''Import

-- guards
deriving newtype instance SizeOf PublicKeyText

makeSizeOf ''KeySetName
makeSizeOf ''UserGuard
makeSizeOf ''CapabilityGuard
makeSizeOf ''KSPredicate
makeSizeOf ''KeySet
makeSizeOf ''ModuleGuard
makeSizeOf ''DefPactGuard
makeSizeOf ''Guard
makeSizeOf ''CapToken
makeSizeOf ''DefManagedMeta
makeSizeOf ''DefCapMeta
makeSizeOf ''Governance
makeSizeOf ''ModRef
makeSizeOf ''TableName
makeSizeOf ''TableValue
makeSizeOf ''PactValue
makeSizeOf ''DefPactContinuation
makeSizeOf ''Provenance
makeSizeOf ''Yield
makeSizeOf ''Arg
makeSizeOf ''BuiltinForm
makeSizeOf ''Term
makeSizeOf ''Defun
makeSizeOf ''ConstVal
makeSizeOf ''DefConst
makeSizeOf ''DefCap
makeSizeOf ''Step
makeSizeOf ''NestedDefPactExec
makeSizeOf ''DefPactExec
makeSizeOf ''DefPact
makeSizeOf ''DefSchema
makeSizeOf ''DefTable
makeSizeOf ''Def
makeSizeOf ''Module
makeSizeOf ''IfDefun
makeSizeOf ''IfDefPact
makeSizeOf ''IfDefCap
makeSizeOf ''IfDef
makeSizeOf ''Interface
makeSizeOf ''Namespace
makeSizeOf ''ModuleData
