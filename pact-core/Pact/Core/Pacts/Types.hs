{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Pact.Core.Pacts.Types
 ( PactId(..)
 , PactContinuation(..)
 , ChainId(..)
 , pcName, pcArgs
 , PactStep(..)
 , psStep, psRollback, psPactId, psResume
 , PactExec(..)
 , peStepCount, peYield, peStep, peContinuation, peStepHasRollback
 , Yield(..)
 ) where

-- Todo: yield
import Data.Text(Text)
import Pact.Core.Pretty
import Control.Lens
import Data.Map.Strict (Map)
import Pact.Core.PactValue
import Pact.Core.Names

newtype ChainId
  = ChainId { _chainId :: Text }
  deriving (Eq,Ord,Show,Pretty)

newtype PactId
  = PactId Text
  deriving (Eq,Ord,Show,Pretty)

data PactContinuation name v
  = PactContinuation
  { _pcName :: name
  , _pcArgs :: [v]
  } deriving (Eq, Show)

makeLenses ''PactContinuation

-- | `Yield` representing an object
newtype Yield
  = Yield {unYield :: Map Field PactValue}
  deriving (Show)

-- | Internal representation of pacts
data PactExec
  = PactExec
  { _peStepCount :: Int
  , _peYield :: Maybe Yield
  , _peStep :: Int
  -- , _pePactId :: PactId
  , _peContinuation :: PactContinuation FullyQualifiedName PactValue
  , _peStepHasRollback :: Bool
--  , _peNestedPactExec :: Map PactId NestedPactExec
  } deriving Show

makeLenses ''PactExec

data PactStep = PactStep
  { _psStep :: !Int
  , _psRollback :: !Bool
  , _psPactId :: !PactId
  , _psResume :: !(Maybe Yield)
  } deriving Show

makeLenses ''PactStep
