{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Pact.Core.Pacts.Types
 ( PactId(..)
 , PactContinuation(..)
 , pcName, pcArgs
 , PactStep(..)
 , psStep, psRollback, psPactId, psResume
 , PactExec(..)
 , peStepCount, peYield, peStep, peContinuation, peStepHasRollback, pePactId
 , peNestedPactExec
 , Yield(..)
 , Provenance(..)
 ) where

-- Todo: yield
import Data.Text(Text)
import Control.Lens
import Data.Map.Strict (Map)

import Pact.Core.PactValue
import Pact.Core.Names
import Pact.Core.Hash
import Pact.Core.Pretty
import Pact.Core.ChainData

newtype PactId
  = PactId Text
  deriving (Eq,Ord,Show,Pretty)

data PactContinuation name v
  = PactContinuation
  { _pcName :: name
  , _pcArgs :: [v]
  } deriving (Eq, Show)

makeLenses ''PactContinuation

-- | Provenance datatype contains all of the necessary
-- data to 'endorse' a yield object.
--
data Provenance = Provenance
  { _pTargetChainId :: !ChainId
    -- ^ the target chain id for the endorsement
  , _pModuleHash :: ModuleHash
    -- ^ a hash of current containing module
  } deriving (Eq, Show)


-- | `Yield` representing an object
data Yield
  = Yield
  { _yData :: Map Field PactValue
  , _yProvenance :: Maybe Provenance
  , _ySourceChain :: Maybe ChainId }
  deriving (Show)

-- | Internal representation of pacts
data PactExec
  = PactExec
  { _peStepCount :: Int
  , _peYield :: Maybe Yield
  , _peStep :: Int
  , _pePactId :: PactId
  , _peContinuation :: PactContinuation FullyQualifiedName PactValue
  , _peStepHasRollback :: Bool
  , _peNestedPactExec :: Map PactId PactExec
  } deriving Show

makeLenses ''PactExec

data PactStep = PactStep
  { _psStep :: !Int
  , _psRollback :: !Bool
  , _psPactId :: !PactId
  , _psResume :: !(Maybe Yield)
  } deriving Show

makeLenses ''PactStep
