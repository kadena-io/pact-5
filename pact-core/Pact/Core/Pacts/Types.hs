{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Pact.Core.Pacts.Types
 ( PactId(..)
 , PactContinuation(..)
 , ChainId(..)
 , pcName, pcArgs
 ) where

-- Todo: yield
import Data.Text(Text)
import Pact.Core.Pretty
import Control.Lens

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
