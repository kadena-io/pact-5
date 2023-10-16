{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE InstanceSigs #-}


module Pact.Core.Environment
 ( EvalEnv(..)
 , eeMsgSigs, eePactDb
 , eeHash, eeMsgBody
 , eePublicData, eeMode
 , PactState(..)
 , psLoaded
 , TxCreationTime(..)
 , PublicData(..)
 , pdPublicMeta, pdBlockHeight
 , pdBlockTime, pdPrevBlockHash
 , PublicMeta(..)
 , pmChainId, pmSender, pmGasLimit
 , pmGasPrice, pmTTL, pmCreationTime
 , TTLSeconds(..)
 , ChainId(..)
 , cdChainId, cdBlockHeight
 , cdBlockTime, cdPrevBlockHash
 , cdSender, cdGasLimit, cdGasPrice
 , EvalState(..)
 , HasEvalState(..)
--  , esCaps, esEvents, esInCap
--  , esStack, esLoaded
 , StackFrame(..)
 , StackFunctionType(..)
 ) where

import Data.Int(Int64)
import Data.Word(Word64)
import Control.Lens
import Data.Set(Set)
import Data.Text(Text)
import Data.Map.Strict(Map)
import Data.Default

import Pact.Core.Gas
import Pact.Core.Persistence
import Pact.Core.Capabilities
import Pact.Core.Guards
import Pact.Core.PactValue ( PactValue, EnvData )
import Pact.Core.Hash
import Pact.Core.Names

-- | Wrapper for 'PublicMeta' ttl field in seconds since offset
--
newtype TTLSeconds
  = TTLSeconds Integer
  deriving (Eq, Show)

-- | Wrapper for 'PublicMeta' creation time field in seconds since POSIX epoch
--
newtype TxCreationTime
  = TxCreationTime Integer
  deriving (Eq, Show)

newtype ChainId
  = ChainId { _chainId :: Text }
  deriving (Eq, Show)

-- | Allows user to specify execution parameters specific to public-chain
-- execution, namely gas parameters, TTL, creation time, chain identifier.
data PublicMeta
  = PublicMeta
  { _pmChainId :: !ChainId
    -- ^ platform-specific chain identifier, e.g. "0"
  , _pmSender :: !Text
    -- ^ sender gas account key
  , _pmGasLimit :: !GasLimit
    -- ^ gas limit (maximum acceptable gas units for tx)
  , _pmGasPrice :: !GasPrice
    -- ^ per-unit gas price
  , _pmTTL :: !TTLSeconds
    -- ^ TTL in seconds
  , _pmCreationTime :: !TxCreationTime
    -- ^ Creation time in seconds since UNIX epoch
  } deriving (Eq, Show)
makeLenses ''PublicMeta

instance Default PublicMeta where
  def =
    PublicMeta
    { _pmChainId = ChainId ""
    , _pmSender = ""
    , _pmGasLimit = Gas 0
    , _pmGasPrice = 0
    , _pmTTL = TTLSeconds 0
    , _pmCreationTime = TxCreationTime 0
    }

-- | "Public chain" data with immutable block data
-- height, hash, creation time
data PublicData = PublicData
  { _pdPublicMeta :: !PublicMeta
    -- ^ 'PublicMeta' data from request
  , _pdBlockHeight :: !Word64
    -- ^ block height as specified by platform.
  , _pdBlockTime :: !Int64
    -- ^ block creation time, micros since UNIX epoch
  , _pdPrevBlockHash :: !Text
    -- ^ block hash of preceding block
  }
  deriving (Show)
makeLenses ''PublicData

instance Default PublicData where
  def =
    PublicData
    { _pdPublicMeta = def
    , _pdBlockHeight = 0
    , _pdBlockTime = 0
    , _pdPrevBlockHash = ""}

-- From pact
-- | All of the types included in our evaluation environment.
data EvalEnv b i
  = EvalEnv
  { _eeMsgSigs :: Map PublicKeyText (Set (CapToken QualifiedName PactValue))
  , _eePactDb :: PactDb b i
  , _eeMsgBody :: EnvData PactValue
  , _eeHash :: Hash
  , _eePublicData :: PublicData
  -- ^
  , _eeMode :: ExecutionMode
  -- ^ The pact execution mode: local or transactional
  }

makeLenses ''EvalEnv

newtype PactState b i
  = PactState
  { _psLoaded :: Loaded b i
  }

makeLenses ''PactState

data StackFunctionType
  = SFDefun
  | SFDefcap
  deriving (Eq, Show, Enum, Bounded)

data StackFrame
  = StackFrame
  { _sfFunction :: Text
  , _sfModule :: ModuleName
  , _sfFnType :: StackFunctionType }
  deriving Show

data EvalState b i
  = EvalState
  { _esCaps :: CapState QualifiedName PactValue
  , _esStack :: [StackFrame]
  , _esEvents :: [PactEvent PactValue]
  , _esLoaded :: Loaded b i
  } deriving Show

instance Default (EvalState b i) where
  def = EvalState def [] [] mempty

makeClassy ''EvalState

instance HasLoaded (EvalState b i) b i where
  loaded = esLoaded

cdChainId :: Field
cdChainId = Field "chain-id"
cdBlockHeight :: Field
cdBlockHeight = Field "block-height"
cdBlockTime :: Field
cdBlockTime = Field "block-time"
cdPrevBlockHash :: Field
cdPrevBlockHash = Field "prev-block-hash"
cdSender :: Field
cdSender = Field "sender"
cdGasLimit :: Field
cdGasLimit = Field "gas-limit"
cdGasPrice :: Field
cdGasPrice = Field "gas-price"
