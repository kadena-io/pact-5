{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Pact.Core.Gas
 ( MilliGas(..)
 , Gas(..)
 , GasModel(..)
 , GasArgs(..)
 , NodeType(..)
 , GasLimit
 , GasPrice
 , MilliGasLimit(..)
 , gmName
 , gmDesc
 , gmNatives
 , gmGasLimit
 , gmRunModel
 , constantGasModel
 , freeGasModel
 , gasToMilliGas
 , milliGasToGas
 , millisPerGas
 , LinearGasArg(..)
 ) where

import Control.Lens
import Control.DeepSeq
import Data.Decimal(Decimal)
import Data.Word(Word64)
import Data.Monoid(Sum(..))
import Data.Text(Text)
import Data.Semiring(Semiring)
import GHC.Generics

import qualified Data.Text as T

-- | Gas in pact-core, represented as an unsigned
-- integer, units will go in terms of 1e3 = 2ns
newtype MilliGas
  = MilliGas Word64
  deriving (Eq, Ord, Show, NFData)
  deriving (Semigroup, Monoid) via (Sum Word64)
  deriving (Semiring, Enum) via Word64

newtype MilliGasLimit
  = MilliGasLimit MilliGas
  deriving (Eq, Ord, Show, NFData)

-- | Gas in pact-core, represented as an unsigned
-- integer, units will go in terms of 1e3 = 2ns
newtype Gas
  = Gas Word64
  deriving (Eq, Ord, Show, NFData)
  deriving (Semigroup, Monoid) via (Sum Word64)
  deriving (Semiring, Enum) via Word64

type GasLimit = Gas
type GasPrice = Decimal

-- | Flat structure of all types of nodes used in evaluation that have an evaluator
-- type case
data NodeType
  = VarNode
  | LamNode
  | LetNode
  | AppNode
  | SeqNode
  | NullaryNode
  -- conditional nodes
  | CondCAndNode
  | CondCOrNode
  | CondIfNode
  | CondEnforceOneNode
  | CondEnforceNode
  ---
  | BuiltinNode
  | ConstantNode
  | ListNode
  | TryNode
  | ObjectLitNode
  | CapFormWithCapNode
  | CapFormCreateUGNode
  | ErrorNode
  deriving (Eq, Show, Enum, Bounded)

data LinearGasArg
  = LinearGasArg
  { _loaSlope :: !(Word64, Word64)
  , _loaIntercept :: !Word64
  } deriving (Eq, Show)

data GasArgs
  = GAConstant !MilliGas
  | GALinear !MilliGas {-# UNPACK #-} !LinearGasArg
  deriving (Show)

data GasModel b
  = GasModel
  { _gmName :: !Text
  , _gmDesc :: !Text
  , _gmNatives :: !(b -> MilliGas)
  , _gmRunModel :: !(GasArgs -> MilliGas)
  , _gmGasLimit :: !MilliGasLimit
  } deriving Generic
makeLenses ''GasModel

instance NFData (GasModel b)

constantGasModel :: MilliGas -> MilliGasLimit -> GasModel b
constantGasModel unitPrice gl
  = GasModel
  { _gmName = "unitGasModel"
  , _gmDesc = "GasModel with constant cost " <> T.pack (show unitPrice)
  , _gmNatives = const unitPrice
  , _gmRunModel = const unitPrice
  , _gmGasLimit = gl
  }

freeGasModel :: GasModel b
freeGasModel = constantGasModel mempty (MilliGasLimit mempty)

millisPerGas :: Word64
millisPerGas = 1000

gasToMilliGas :: Gas -> MilliGas
gasToMilliGas (Gas n) = MilliGas (n * millisPerGas)

milliGasToGas :: MilliGas -> Gas
milliGasToGas (MilliGas n) = Gas (n `quot` millisPerGas)
