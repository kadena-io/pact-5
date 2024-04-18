{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Pact.Core.Gas.Types (
    MilliGas(..),
    MilliGasLimit(..),
    Gas(..),
    GasLimit,
    GasPrice,
    gasToMilliGas,
    milliGasToGas,
    millisPerGas
  ) where


import Control.DeepSeq
import Data.Decimal(Decimal)
import Data.Monoid
import Data.Word (Word64)
import Data.Semiring(Semiring)

import Pact.Core.Pretty

-- | Gas in pact-core, represented as an unsigned
-- integer, units will go in terms of 1e3 = 2ns
newtype MilliGas
  = MilliGas Word64
  deriving (Eq, Ord, Show)
  deriving newtype NFData
  deriving (Semigroup, Monoid) via (Sum Word64)
  deriving (Semiring, Enum) via Word64

instance Pretty MilliGas where
  pretty (MilliGas g) = pretty g <> "mG"

newtype MilliGasLimit
  = MilliGasLimit MilliGas
  deriving (Eq, Ord, Show)
  deriving newtype NFData

-- | Gas in pact-core, represented as an unsigned
-- integer, units will go in terms of 1e3 = 2ns
newtype Gas
  = Gas Word64
  deriving (Eq, Ord, Show)
  deriving (Semigroup, Monoid) via (Sum Word64)
  deriving (Semiring, Enum) via Word64
  deriving newtype NFData

type GasLimit = Gas
type GasPrice = Decimal

millisPerGas :: Word64
millisPerGas = 1000

gasToMilliGas :: Gas -> MilliGas
gasToMilliGas (Gas n) = MilliGas (n * millisPerGas)
{-# INLINE gasToMilliGas #-}

milliGasToGas :: MilliGas -> Gas
milliGasToGas (MilliGas n) = Gas (n `quot` millisPerGas)
{-# INLINE milliGasToGas #-}