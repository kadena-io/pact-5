{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Pact.Core.Gas
 ( Gas(..)
 , GasModel(..)
 , GasEnv(..)
 , NodeType(..)
 , GasLimit
 , GasPrice
 , gmName
 , gmDesc
 , gmNatives
 , gmNodes
 , geGasLimit
 , geGasPrice
 , geGasModel
 , constantGasModel
 , freeGasModel
 , freeGasEnv
 , MonadGas(..)
 ) where

import Control.Lens
import Control.DeepSeq
import Data.Decimal(Decimal)
import Data.Word(Word64)
import Data.Monoid(Sum(..))
import Data.Text(Text)
import qualified Data.Text as T
import Data.Semiring(Semiring)

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

nodeGas :: NodeType -> Gas
nodeGas _ = Gas 1

data GasModel b
  = GasModel
  { _gmName :: Text
  , _gmDesc :: Text
  , _gmNatives :: b -> Gas
  , _gmNodes :: NodeType -> Gas
  }
makeLenses ''GasModel


data GasEnv b
  = GasEnv
  { _geGasLimit :: GasLimit
  , _geGasPrice :: GasPrice
  , _geGasModel :: GasModel b
  }
makeLenses ''GasEnv

constantGasModel :: Gas -> GasModel b
constantGasModel unitPrice =
  GasModel
  { _gmName = "unitGasModel"
  , _gmDesc = "GasModel with constant cost " <> T.pack (show unitPrice)
  , _gmNatives = const unitPrice
  , _gmNodes = if unitPrice > Gas 0 then nodeGas else const (Gas 0)
  }

freeGasModel :: GasModel b
freeGasModel = constantGasModel (Gas 0)

freeGasEnv :: GasEnv b
freeGasEnv =
  GasEnv (Gas 1) 1 freeGasModel

class Monad m => MonadGas m where
  logGas :: Text -> Gas -> m ()
  chargeGas :: Gas -> m ()
