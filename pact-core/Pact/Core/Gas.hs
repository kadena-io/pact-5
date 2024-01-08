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
 , ZKGroup(..)
 , ZKArg(..)
 , IntegerPrimOp(..)
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
  deriving (Eq, Show, Enum, Bounded)

-- | Data type representing generally linear computations of the form
-- f(x) = (slopeNum*x)/slopeDenom + intercept
-- Todo: `Ratio`? Unfortunately ratio is not strict though
data LinearGasArg
  = LinearGasArg
  { _loaSlopeNum :: !Word64
  , _loaSlopeDenom :: !Word64
  , _loaIntercept :: !Word64
  } deriving (Eq, Show)

-- | The elliptic curve pairing group we are
-- handling
data ZKGroup
  = ZKG1
  -- ^ Group one, that is Fq in Pairing
  | ZKG2
  -- ^ Group two, that is, Fq2 Pairing
  deriving Show

data ZKArg
  = PointAdd !ZKGroup
  -- ^ Point addition Gas arguments, where the gas is dependent on the group.
  | ScalarMult !ZKGroup
  -- ^ Scalar multiplication gas, group dependent
  | Pairing !Int
  -- ^ Pairing function gas, dependent on number of pairs
  deriving Show

data IntegerPrimOp
  = PrimOpAdd
  | PrimOpSub
  | PrimOpMul
  | PrimOpDiv
  deriving (Eq, Show, Enum, Ord)

data GasArgs
  = GAConstant !MilliGas
  -- Todo: integerOpCost seems like a case of `GALinear`
  -- Maybe we can investigate generalizing the operational costs in terms of a more general structure
  -- instead of the current `GasArgs` model?
  | GALinear !MilliGas {-# UNPACK #-} !LinearGasArg
  | GIntegerOpCost !IntegerPrimOp !Integer !Integer
  | GAApplyLam !Integer
  | GAZKArgs !ZKArg
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
