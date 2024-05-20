{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

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
 , ConcatType(..)
 , GasTextLength(..)
 , GasListLength(..)
 , GasObjectSize(..)
 , ComparisonType(..)
 , SearchType(..)
 , GasMEnv(..)
 , GasM(..)
 , runGasM
 , ignoreGas
 ) where

import Control.Lens
import Control.DeepSeq
import Control.Monad.Reader
import Control.Monad.Except
import Data.IORef
import Data.Decimal(Decimal)
import Data.Word(Word64)
import Data.Text(Text)
import GHC.Generics

import qualified Data.Text as T

import Pact.Core.Pretty
import Pact.Core.Gas.Types
import Pact.Core.Errors
import Control.Exception


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
  deriving (Eq, Show, Enum, Ord, Generic, NFData)

data GasArgs
  = GAConstant !MilliGas
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
  | GAApplyLam Text !Int
  -- ^ Cost of function application
  | GAZKArgs !ZKArg
  -- ^ Cost of ZK function
  | GWrite !Word64
  -- ^ Cost of writes, per bytes, roughly based on in-memory cost.
  | GComparison !ComparisonType
  -- ^ Gas costs for comparisons
  | GSearch !SearchType
  -- ^ Gas costs for searches
  | GPoseidonHashHackAChain !Int
  -- ^ poseidon-hash-hack-a-chain costs.
  | GModuleMemory !Word64
  | GCountBytes
  -- ^ Cost of computing SizeOf for N bytes.
  deriving (Show, Generic, NFData)

instance Pretty GasArgs where
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

data GasModel b
  = GasModel
  { _gmName :: !Text
  , _gmDesc :: !Text
  , _gmNatives :: !(b -> MilliGas)
  , _gmRunModel :: !(GasArgs -> MilliGas)
  , _gmGasLimit :: !MilliGasLimit
  } deriving (Generic, NFData)
makeLenses ''GasModel

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
freeGasModel = constantGasModel mempty (MilliGasLimit (MilliGas maxBound)) -- TODO: some tests seem to charge gas even with freeGasModel.

data GasMEnv
  = GasMEnv
  { _gasMRef :: IORef MilliGas
  , _gasMLimit :: MilliGasLimit
  }

newtype GasM e a
  = GasM (ReaderT GasMEnv (ExceptT e IO) a)
  deriving
  ( Functor
  , Applicative
  , Monad
  , MonadReader GasMEnv
  , MonadError e
  , MonadIO) via (ReaderT GasMEnv (ExceptT e IO))

runGasM
  :: i
  -> GasMEnv
  -> GasM (PactError i) a
  -> IO (Either (PactError i) a)
runGasM info env (GasM m) = do
  res <- try $ runExceptT $ runReaderT m env
  case res of
    Left (e :: DbOpException) -> pure $ Left $ PEExecutionError (DbOpFailure e) info
    Right a -> pure a

ignoreGas
  :: i
  -> GasM (PactError i) a
  -> IO a
ignoreGas info m = do
  gasRef <- newIORef (MilliGas 0)
  let maxLimit = MilliGasLimit (MilliGas maxBound)
  runGasM info (GasMEnv gasRef maxLimit) m >>=
    \case
      Left _ -> error "impossible case: ran out of gas with an infinite limit"
      Right a -> pure a