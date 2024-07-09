{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}

module Pact.Core.Gas.Types
  ( MilliGas(..)
  , MilliGasLimit(..)
  , Gas(..)
  , GasLimit(..)
  , GasPrice(..)
  , gasToMilliGas
  , milliGasToGas
  , milliGasPerGas

  , GasLogEntry(..)
  , GasEnv(..)
  , geGasRef
  , geGasLogRef
  , geGasModel

  , GasModel(..)
  , GasArgs(..)
  , SerializationCosts(..)

  , NodeType(..)
  , LinearGasArg(..)
  , ZKGroup(..)
  , ZKArg(..)
  , IntegerPrimOp(..)
  , StrOp(..)
  , ObjOp(..)
  , CapOp(..)
  , ConcatType(..)
  , GasTextLength(..)
  , GasListLength(..)
  , GasObjectSize(..)
  , ComparisonType(..)
  , SearchType(..)

  , gmRunModel
  , gmGasLimit
  , gmDesc
  , gmName
  , gmSerialize

  , constantGasModel
  , freeGasModel

  ) where


import Control.DeepSeq
import Control.Lens
import Data.Decimal(Decimal)
import Data.Monoid
import Data.Word (Word64)
import Data.Semiring(Semiring)
import qualified Data.Text as T
import Data.Text (Text)
import GHC.Generics

import Pact.Core.Pretty
import Pact.Core.Names (FullyQualifiedName)
import Data.IORef

-- | Gas in pact-core, represented as an unsigned
-- integer, units will go in terms of 1e3 = 2ns
newtype MilliGas
  = MilliGas Word64
  deriving (Eq, Ord, Show)
  deriving newtype NFData
  deriving (Semigroup, Monoid) via (Sum Word64)
  deriving (Bounded, Semiring, Enum) via Word64

instance Pretty MilliGas where
  pretty (MilliGas g) = pretty g <> "mG"

newtype MilliGasLimit
  = MilliGasLimit MilliGas
  deriving (Bounded, Eq, Ord, Show)
  deriving newtype NFData

-- | Gas in pact-core, represented as an unsigned
-- integer, units will go in terms of 1e3 = 2ns
newtype Gas
  = Gas { _gas :: Word64 }
  deriving (Eq, Ord, Show)
  deriving (Semigroup, Monoid) via (Sum Word64)
  deriving (Semiring, Enum) via Word64
  deriving newtype NFData

newtype GasLimit =
  GasLimit Gas
  deriving (Eq, Show, Ord)
  deriving newtype NFData

newtype GasPrice
  = GasPrice Decimal
  deriving (Eq, Show, Ord)
  deriving newtype NFData

milliGasPerGas :: Word64
milliGasPerGas = 1000

gasToMilliGas :: Gas -> MilliGas
gasToMilliGas (Gas n) = MilliGas (n * milliGasPerGas)
{-# INLINE gasToMilliGas #-}

milliGasToGas :: MilliGas -> Gas
milliGasToGas (MilliGas n) = Gas (n `quot` milliGasPerGas)
{-# INLINE milliGasToGas #-}


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
  deriving (Show, Generic)

instance NFData ZKGroup

data ZKArg
  = PointAdd !ZKGroup
  -- ^ Point addition Gas arguments, where the gas is dependent on the group.
  | ScalarMult !ZKGroup
  -- ^ Scalar multiplication gas, group dependent
  | Pairing !Int
  -- ^ Pairing function gas, dependent on number of pairs
  deriving (Show, Generic, NFData)

data IntegerPrimOp
  = PrimOpAdd
  | PrimOpSub
  | PrimOpMul
  | PrimOpDiv
  | PrimOpShift
  | PrimOpPow
  deriving (Eq, Show, Enum, Ord, Generic, NFData)

data StrOp
  = StrOpLength !Int
  -- ^ The cost of computing the length. In a sense, it's charged post-factum.
  | StrOpConvToInt !Int
  -- ^ The cost of converting a string of a given length to an integer.
  | StrOpParse !Int
  -- ^ The cost of a general scanning parse of a string of a given length.
  | StrOpExplode !Int
  -- ^ The cost of splitting a string into a list of chars.
  | StrOpParseTime !Int !Int
  -- ^ The cost of parsing time with the given format string and time string lengths.
  | StrOpFormatTime !Int
  -- ^ The cost of formatting time with the given format string length.
  deriving (Eq, Show, Ord, Generic, NFData)

data ObjOp
  = ObjOpLookup !T.Text !Int
  -- ^ The cost of looking up a key in an object with the given fields count.
  | ObjOpRemove !T.Text !Int
  -- ^ The cost of removing a key from an object with the given fields count.
  deriving (Eq, Show, Ord, Generic, NFData)

data CapOp
  = CapOpRequire !Int
  deriving (Eq, Show, Ord, Generic, NFData)

data GasArgs b
  = GAConstant !MilliGas
  | GNative b
  -- Todo: integerOpCost seems like a case of `GALinear`
  -- Maybe we can investigate generalizing the operational costs in terms of a more general structure
  -- instead of the current `GasArgs` model?
  | GConcat !ConcatType
  -- ^ The cost of concatenating two elements
  -- TODO: We actually reuse this cost for construction as well for objects/lists. Should we
  -- instead consider renaming the objcat and listcat constructors to be ListCatOrConstruction
  -- | GALinear !Word64 {-# UNPACK #-} !LinearGasArg
  -- ^ Cost of linear-based gas
  | GIntegerOpCost !IntegerPrimOp !Integer !Integer
  -- ^ Cost of integer operations
  | GMakeList !Integer !Word64
  -- ^ Cost of creating a list of `n` elements + some memory overhead per elem
  | GAApplyLam (Maybe FullyQualifiedName) !Int
  -- ^ Cost of function application
  | GAZKArgs !ZKArg
  -- ^ Cost of ZK function
  | GWrite !Word64
  -- ^ Cost of writes, per bytes, roughly based on in-memory cost.
  | GRead !Word64
  -- ^ Cost of reads, per bytes, roughly based on in-memory cost.
  | GComparison !ComparisonType
  -- ^ Gas costs for comparisons
  | GSearch !SearchType
  -- ^ Gas costs for searches
  | GPoseidonHashHackAChain !Int
  -- ^ poseidon-hash-hack-a-chain costs.
  | GHyperlaneMessageId !Int
  -- ^ ^ Cost of the hyperlane-message-id on this size (in bytes) of the
  --   hyperlane Message Body, which is the only variable-length
  --   part of a HyperlaneMessage
  | GHyperlaneEncodeDecodeTokenMessage !Int
  -- ^ Cost of hyperlane-encode-token-message and hyperlane-decode-token-message
  --   on this size (in bytes) of the hyperlane TokenMessage base64-encoded string.
  | GModuleMemory !Word64
  | GStrOp !StrOp
  | GObjOp !ObjOp
  | GCapOp !CapOp
  | GCountBytes
  -- ^ Cost of computing SizeOf for N bytes.
  deriving (Show, Generic, NFData)

instance Show b => Pretty (GasArgs b) where
  pretty = pretty . show

newtype GasTextLength
  = GasTextLength Int
  deriving Show
  deriving newtype NFData

newtype GasListLength
  = GasListLength Int
  deriving Show
  deriving newtype NFData

newtype GasObjectSize
  = GasObjectSize Int
  deriving Show
  deriving newtype NFData

data SearchType
  = SubstringSearch !Text !Text
  -- ^ searching `needle` in `hay`
  | FieldSearch !Int
  -- ^ checking if an object has a field
  deriving (Show, Generic, NFData)

data ComparisonType
  = TextComparison !Text
  -- ^ comparing with a string of length `n`
  -- Note: comparing two strings of different lengths always returns early
  -- and thus is independent of the length of the strings,
  -- hence we only care about one string for the case when their lengths are equal.
  | IntComparison !Integer !Integer
  -- ^ compare two integers, of at most `n` bits
  -- Note: decimal comparison overhead should be the same as
  | DecimalComparison !Decimal !Decimal
  -- ^ compare decimals with similar mantissas, of at most `n` bits
  -- | TimeCmp
  -- ^ TODO: Comparisons gas for time.
  | ListComparison !Int
  -- ^ N comparisons constant time overhead
  | ObjComparison !Int
  -- ^ Compare objects of at most size `N`
  deriving (Show, Generic, NFData)

data ConcatType
  = TextConcat !GasTextLength
  -- ^ Total final string length
  | TextListConcat !GasTextLength !GasListLength
  -- ^ Total final string length, number of strings
  | ListConcat !GasListLength
  -- ^ Final list length
  | ObjConcat !Int
  -- ^ Upper bound on max object size
  deriving (Show, Generic, NFData)

data SerializationCosts = SerializationCosts
  { objectKeyCostMilliGasOffset :: Word64
  , objectKeyCostMilliGasPer1000Chars :: Word64
  , boolMilliGasCost :: Word64
  , unitMilliGasCost :: Word64
  , integerCostMilliGasPerDigit :: Word64
  , decimalCostMilliGasOffset :: Word64
  , decimalCostMilliGasPerDigit :: Word64
  , timeCostMilliGas :: Word64
  }
  deriving (Show, Generic, NFData)

freeSerializationCosts :: SerializationCosts
freeSerializationCosts = SerializationCosts
  { objectKeyCostMilliGasOffset = 0
  , objectKeyCostMilliGasPer1000Chars = 0
  , boolMilliGasCost = 0
  , unitMilliGasCost = 0
  , integerCostMilliGasPerDigit = 0
  , decimalCostMilliGasOffset = 0
  , decimalCostMilliGasPerDigit = 0
  , timeCostMilliGas = 0
  }

data GasModel b
  = GasModel
  { _gmName :: !Text
  , _gmDesc :: !Text
  , _gmRunModel :: !(GasArgs b -> MilliGas)
  , _gmGasLimit :: !(Maybe MilliGasLimit)
  , _gmSerialize :: !SerializationCosts
  } deriving (Generic, NFData)
makeLenses ''GasModel

constantGasModel :: MilliGas -> MilliGasLimit -> GasModel b
constantGasModel unitPrice gl
  = GasModel
  { _gmName = "unitGasModel"
  , _gmDesc = "GasModel with constant cost " <> T.pack (show unitPrice)
  , _gmRunModel = const unitPrice
  , _gmGasLimit = Just gl
  , _gmSerialize = freeSerializationCosts
  }

freeGasModel :: GasModel b
freeGasModel = GasModel
  { _gmName = "freeGasModel"
  , _gmDesc = "free gas model"
  , _gmRunModel = const mempty
  , _gmGasLimit = Nothing
  , _gmSerialize = freeSerializationCosts
  }

data GasLogEntry b i = GasLogEntry
  { _gleArgs :: GasArgs b
  , _gleInfo :: i
  , _gleInfoStack :: [i]
  , _gleThisUsed :: !MilliGas
  } deriving (Show, Generic, NFData)

data GasEnv b i
  = GasEnv
  { _geGasRef :: !(IORef MilliGas)
  , _geGasLogRef :: !(IORef (Maybe [GasLogEntry b i]))
  , _geGasModel :: !(GasModel b)
  } deriving (Generic, NFData)
makeLenses ''GasEnv
