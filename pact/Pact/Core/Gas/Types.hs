{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE InstanceSigs #-}

module Pact.Core.Gas.Types
  ( MilliGas(..)
  , MilliGasLimit(..)
  , Gas(..)
  , GasLimit(..)
  , _GasLimit
  , GasPrice(..)
  , _GasPrice
  , gasToMilliGas
  , milliGasToGas
  , milliGasPerGas
  , milliGasToGasLimit

  , GasLogEntry(..)
  , GasEnv(..)
  , geGasRef
  , geGasLog
  , geGasModel

  , GasModel(..)
  , GasArgs(..)

  , NodeType(..)
  , ZKGroup(..)
  , ZKArg(..)
  , IntegerPrimOp(..)
  , StrOp(..)
  , ObjOp(..)
  , CapOp(..)
  , HashOp(..)
  , ConcatType(..)
  , GasTextLength(..)
  , GasListLength(..)
  , GasObjectSize(..)
  , ComparisonType(..)
  , SearchType(..)
  , ModuleOp(..)

  , gmGasLimit
  , gmDesc
  , gmName
  , gmGasCostConfig
  , gmNativeTable

  , freeGasModel
  , mkGasEnv
  , mkFreeGasEnv
  , GasCostConfig(..)
  , TranscendentalCost(..)
  , EnableGasLogs(..)
  , module Pact.Core.SatWord
  , pattern GHash
  ) where


import Control.DeepSeq
import Control.Lens
import Data.Decimal(Decimal)
import Data.Monoid
import Data.Vector(Vector)
import Data.Primitive hiding (sizeOf)
import qualified Data.Text as T
import Data.Text (Text)
import GHC.Generics

import Pact.Core.Pretty
import Pact.Core.Names (FullyQualifiedName)
import Data.IORef
import Pact.Core.SatWord

-- | Gas in pact-core, represented as an unsigned
-- integer, units will go in terms of 1e3 = 2ns
newtype MilliGas
  = MilliGas SatWord
  deriving Show
  deriving newtype (Eq, Ord, NFData, Prim, Bounded, Enum)
  deriving (Semigroup, Monoid) via (Sum SatWord)

instance Pretty MilliGas where
  pretty (MilliGas g) = pretty g <> "mG"

newtype MilliGasLimit
  = MilliGasLimit MilliGas
  deriving (Bounded, Eq, Ord, Show)
  deriving newtype NFData

-- | Gas in pact-core, represented as an unsigned
-- integer, units will go in terms of 1e3 = 2ns
newtype Gas
  = Gas { _gas :: SatWord }
  deriving (Eq, Ord, Show)
  deriving (Semigroup, Monoid) via (Sum SatWord)
  deriving (Enum) via SatWord
  deriving newtype NFData

instance Pretty Gas where
  pretty (Gas g) = pretty g

newtype GasLimit =
  GasLimit Gas
  deriving (Eq, Show, Ord)
  deriving newtype NFData

instance Pretty GasLimit where
  pretty (GasLimit g) = pretty g

makePrisms ''GasLimit

newtype GasPrice
  = GasPrice Decimal
  deriving (Eq, Show, Ord)
  deriving newtype NFData

makePrisms ''GasPrice

-- | Our gas costs tuned for the pact language's gas model
--   Note: All values are `SatWord`s, which are essentially
--   costs in MilliGas
data GasCostConfig
  = GasCostConfig
  { _gcNativeBasicWork :: !SatWord
  -- ^ The minimal work our machine charges for
  -- calling any native
  , _gcFunctionArgumentCost :: !SatWord
  -- ^ The flat cost per argument for
  -- function calls. Note: Typechecking is costed separately
  , _gcMachineTickCost :: !SatWord
  -- ^ The flat cost for a state transition in our machine
  , _gcUnconsWork :: !SatWord
  -- ^ Flat cost for list unconsing, particularly for maps and
  -- folds in natives
  , _gcReadPenalty :: !SatWord
  -- ^ Our flat penalty for reading from the database
  , _gcWritePenalty :: !SatWord
  -- ^ Our flat penalty for writing to the database
  , _gcMetadataTxPenalty :: !SatWord
  -- ^ Penalty for db metadata functions
  , _gcSelectPenalty :: !SatWord
  -- ^ Penalty for calling select
  , _gcConcatFactor :: !SatWord
  -- ^ Cost of element concatenation. Particularly for our
  -- concatenation functions (+, concat)
  , _gcPerByteWriteCost :: !SatWord
  -- ^ The cost per byte on write for
  -- entries to the database. This applies to all tables
  , _gcPerByteReadCost :: !SatWord
  -- ^ The cost per byte of reading from the database
  , _gcSortBytePenaltyReduction :: !SatWord
  -- ^ The sort penalty reduction
  , _gcPoseidonQuadraticGasFactor :: !SatWord
  -- ^ Poseidon hashing quadratic gas factor
  , _gcPoseidonLinearGasFactor :: !SatWord
  -- ^ Poseidon hashing linear gas factor
  , _gcModuleLoadSlope :: !SatWord
  -- ^ Module load cost function slope
  , _gcModuleLoadIntercept :: !SatWord
  -- ^ Module load cost function Intercept
  , _gcDesugarBytePenalty :: !SatWord
  -- ^ Module load desugaring byte penalty
  , _gcMHashBytePenalty :: !SatWord
  -- ^ Module load hashing byte penalty
  , _gcSizeOfBytePenalty :: !SatWord
  -- ^ Our `SizeOf` limit penalty
  , _gc_keccak256GasPerOneHundredBytes :: !SatWord
  -- ^ Cost of keccak gas per 100 bytes
  , _gc_keccak256GasPerChunk :: !SatWord
  -- ^ Cost of keccak gas per chunk
  } deriving (Eq, Show, Generic)

instance NFData GasCostConfig

milliGasPerGas :: SatWord
milliGasPerGas = 1000

gasToMilliGas :: Gas -> MilliGas
gasToMilliGas (Gas n) = MilliGas (n * milliGasPerGas)
{-# INLINE gasToMilliGas #-}

milliGasToGas :: MilliGas -> Gas
milliGasToGas (MilliGas n) =
  let (n', r) = n `quotRem` milliGasPerGas
      gas = if r == 0 then n' else n' + 1
  in (Gas (fromIntegral gas))
{-# INLINE milliGasToGas #-}

milliGasToGasLimit :: MilliGasLimit -> GasLimit
milliGasToGasLimit (MilliGasLimit mg) =
  GasLimit (milliGasToGas mg)

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


-- | The elliptic curve pairing group we are
-- handling
data ZKGroup
  = ZKG1
  -- ^ Group one, that is Fq in Pairing
  | ZKG2
  -- ^ Group two, that is, Fq2 Pairing
  deriving (Show, Eq, Generic)

instance NFData ZKGroup

data ZKArg
  = PointAdd !ZKGroup
  -- ^ Point addition Gas arguments, where the gas is dependent on the group.
  | ScalarMult !ZKGroup
  -- ^ Scalar multiplication gas, group dependent
  | Pairing !Int
  -- ^ Pairing function gas, dependent on number of pairs
  deriving (Show, Eq, Generic, NFData)

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

data HashOp
  = GHashBlake !SatWord
  | GHashPoseidon !Int
  | GHashKeccak (Vector Int)
  deriving (Eq, Show, Ord, Generic, NFData)

data GasArgs b
  = GAConstant !MilliGas
  -- ^ Constant gas costs
  | GNative !b
  -- ^ Indexing into our native flat gas cost table
  | GIntegerOpCost !IntegerPrimOp !Integer !Integer
  -- ^ Cost of integer operations
  | GAApplyLam (Maybe FullyQualifiedName) !Int
  -- ^ Cost of function application
  | GConcat !ConcatType
  -- ^ The cost of concatenating two elements
  -- TODO: We actually reuse this cost for construction as well for objects/lists. Should we
  -- instead consider renaming the objcat and listcat constructors to be ListCatOrConstruction
  | GMakeList !Integer !SatWord
  -- ^ Cost of creating a list of `n` elements + some memory overhead per elem
  | GAZKArgs !ZKArg
  -- ^ Cost of ZK function
  | GWrite !SatWord
  -- ^ Cost of writes, per bytes, roughly based on in-memory cost.
  | GRead !SatWord
  -- ^ Cost of reads, per bytes, roughly based on in-memory cost.
  | GComparison !ComparisonType
  -- ^ Gas costs for comparisons
  | GSearch !SearchType
  -- ^ Gas costs for searches
  | GHyperlaneMessageId !Int
  -- ^ ^ Cost of the hyperlane-message-id on this size (in bytes) of the
  --   hyperlane Message Body, which is the only variable-length
  --   part of a HyperlaneMessage
  | GHyperlaneEncodeDecodeTokenMessage !Int
  -- ^ Cost of hyperlane-encode-token-message and hyperlane-decode-token-message
  --   on this size (in bytes) of the hyperlane TokenMessage base64-encoded string.
  | GModuleOp ModuleOp
  -- ^ The cost of integrating module deps, which is essentially a map union
  -- Map union is O(m*log(n/m+1)) where 0 < m <= n
  | GTranscendental !TranscendentalCost
  | GStrOp !StrOp
  | GObjOp !ObjOp
  | GCapOp !CapOp
  | GHashOp !HashOp
  -- ^ The cost of Blake2b hashing a particular value in bytes
  deriving (Show, Eq, Generic, NFData)

data TranscendentalCost
  = TransExp !Integer -- Exponent integral part will dominate the work
  | TransSqrt !Integer -- Integer part of
  | TransLn !Integer -- Integer part of ln
  | TransLogBase !Integer !Integer -- We will compute this as Ln(num) / Ln(base)
  deriving (Eq, Show, Generic, NFData)

data ModuleOp
  = MOpLoadModule !Int
  -- ^ Cost of loading module, the first element is the size of the module, the second and third
-- arguments are:
  | MOpMergeDeps Int Int
  -- ^ Cost of adding deps to the symbol table
  | MOpDesugarModule !SatWord -- Size of the tree
  -- ^ the cost of module desugar
  | MOpFindTransitiveDep !SatWord
  deriving (Show, Eq, Generic, NFData)

instance Show b => Pretty (GasArgs b) where
  pretty = pretty . show

newtype GasTextLength
  = GasTextLength Int
  deriving Show
  deriving newtype (Eq, NFData)

newtype GasListLength
  = GasListLength Int
  deriving Show
  deriving newtype (Eq, NFData)

newtype GasObjectSize
  = GasObjectSize Int
  deriving Show
  deriving newtype (Eq, NFData)

data SearchType
  = SubstringSearch !Text !Text
  -- ^ searching `needle` in `hay`
  | FieldSearch !Int
  -- ^ checking if an object has a field
  deriving (Show, Eq, Generic, NFData)

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
  | ListComparison !Int
  -- ^ N comparisons constant time overhead
  | ObjComparison !Int
  -- ^ Compare objects of at most size `N`
  | SortComparisons !SatWord !Int
  deriving (Show, Eq, Generic, NFData)

data ConcatType
  = TextConcat !GasTextLength
  -- ^ Total final string length
  | TextListConcat !GasTextLength !GasListLength
  -- ^ Total final string length, number of strings
  | ListConcat !GasListLength
  -- ^ Final list length
  | ObjConcat !Int
  -- ^ Upper bound on max object size
  deriving (Show, Eq, Generic, NFData)

freeGasCostConfig :: GasCostConfig
freeGasCostConfig = GasCostConfig
  { _gcNativeBasicWork = 1
  -- ^ The minimal work our machine charges for
  -- calling any native
  , _gcFunctionArgumentCost = 1
  -- ^ The flat cost per argument for
  -- function calls. Note: Typechecking is costed separately
  , _gcMHashBytePenalty = 1
  , _gcMachineTickCost = 1
  -- ^ The flat cost for a state transition in our machine
  , _gcUnconsWork = 1
  -- ^ Flat cost for list unconsing, particularly for maps and
  -- folds in natives
  , _gcReadPenalty = 1
  -- ^ Our flat penalty for reading from the database
  , _gcWritePenalty = 1
  -- ^ Our flat penalty for writing to the database
  , _gcMetadataTxPenalty = 1
  -- ^ Penalty for db metadata functions
  , _gcSelectPenalty = 1
  -- ^ Penalty for calling select
  , _gcConcatFactor = 1
  -- ^ Cost of element concatenation. Particularly for our
  -- concatenation functions (+, concat)
  , _gcPerByteWriteCost = 1
  -- ^ The cost per byte on write for
  -- entries to the database. This applies to all tables
  , _gcPerByteReadCost = 1
  -- ^ The cost per byte of reading from the database
  , _gcSortBytePenaltyReduction = 1
  -- ^ The sort penalty reduction
  , _gcPoseidonQuadraticGasFactor = 1
  -- ^ Poseidon hashing quadratic gas factor
  , _gcPoseidonLinearGasFactor = 1
  -- ^ Poseidon hashing linear gas factor
  , _gcModuleLoadSlope = 1
  -- ^ Module load cost function slope
  , _gcModuleLoadIntercept = 1
  -- ^ Module load cost function Intercept
  , _gcDesugarBytePenalty = 1
  -- ^ Module load desugaring byte penalty
  , _gcSizeOfBytePenalty = 1

  , _gc_keccak256GasPerOneHundredBytes = 1
  -- ^ Cost of keccak gas per 100 bytes
  , _gc_keccak256GasPerChunk = 1
  -- ^ Cost of keccak gas per chunk
  }

data EnableGasLogs
  = GasLogsEnabled
  | GasLogsDisabled
  deriving (Eq, Show, Ord)


data GasModel b
  = GasModel
  { _gmName :: !Text
  , _gmDesc :: !Text
  , _gmNativeTable :: !(b -> MilliGas)
  , _gmGasLimit :: !(Maybe MilliGasLimit)
  , _gmGasCostConfig :: !GasCostConfig
  } deriving (Generic, NFData)
makeLenses ''GasModel


freeGasModel :: GasModel b
freeGasModel = GasModel
  { _gmName = "freeGasModel"
  , _gmDesc = "free gas model"
  , _gmNativeTable = \_ -> mempty
  , _gmGasLimit = Nothing
  , _gmGasCostConfig = freeGasCostConfig
  }

data GasLogEntry b i = GasLogEntry
  { _gleArgs :: GasArgs b
  , _gleInfo :: i
  , _gleInfoStack :: [i]
  , _gleThisUsed :: !MilliGas
  } deriving (Show, Eq, Generic, NFData)


data GasEnv b i
  = GasEnv
  { _geGasRef :: !(IORef MilliGas)
  , _geGasLog :: !(Maybe (IORef [GasLogEntry b i]))
  , _geGasModel :: !(GasModel b)
  } deriving (Generic, NFData)
makeLenses ''GasEnv

mkGasEnv :: EnableGasLogs -> GasModel b -> IO (GasEnv b i)
mkGasEnv enabled model = do
  gasRef <- newIORef mempty
  glref <- if enabled == GasLogsEnabled then
    Just <$> newIORef [] else
    pure Nothing
  pure (GasEnv gasRef glref model)
{-# INLINE mkGasEnv #-}


mkFreeGasEnv :: EnableGasLogs -> IO (GasEnv b i)
mkFreeGasEnv enabled = mkGasEnv enabled freeGasModel

pattern GHash :: SatWord -> GasArgs b
pattern GHash w = GHashOp (GHashBlake w)
