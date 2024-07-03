{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE InstanceSigs #-}

module Pact.Core.ChainData
  ( TxCreationTime(..)
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
  , NetworkId(..)
  ) where

import Data.Int(Int64)
import Data.Word(Word64)
import Control.DeepSeq
import Control.Lens
import GHC.Generics
import Data.Text(Text)
import Data.Default

import qualified Pact.JSON.Encode as J
import qualified Pact.JSON.Decode as JD


import Pact.Core.Gas.Types
import Pact.Core.Names

-- | Wrapper for 'PublicMeta' ttl field in seconds since offset
--
newtype TTLSeconds
  = TTLSeconds Integer
  deriving (Eq, Show, NFData)

-- | Wrapper for 'PublicMeta' creation time field in seconds since POSIX epoch
--
newtype TxCreationTime
  = TxCreationTime Integer
  deriving (Eq, Show, NFData)

newtype ChainId
  = ChainId { _chainId :: Text }
  deriving (Eq, Show, NFData)

newtype NetworkId
  = NetworkId { _networkId :: Text }
  deriving (Eq, Show, NFData)
  deriving newtype (J.Encode, JD.FromJSON)

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
  } deriving (Eq, Show, Generic)
makeLenses ''PublicMeta

instance Default PublicMeta where
  def =
    PublicMeta
    { _pmChainId = ChainId ""
    , _pmSender = ""
    , _pmGasLimit = GasLimit (Gas 0)
    , _pmGasPrice = GasPrice 0
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
  deriving (Show, Generic)
makeLenses ''PublicData

instance Default PublicData where
  def =
    PublicData
    { _pdPublicMeta = def
    , _pdBlockHeight = 0
    , _pdBlockTime = 0
    , _pdPrevBlockHash = ""}

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

instance NFData PublicMeta
instance NFData PublicData
