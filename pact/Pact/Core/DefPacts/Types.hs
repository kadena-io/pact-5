{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Pact.Core.DefPacts.Types
 ( DefPactContinuation(..)
 , pcName, pcArgs
 , DefPactStep(..)
 , psStep, psRollback, psDefPactId, psResume
 , DefPactExec(..)
 , NestedDefPactExec(..)
 , peStepCount, peYield, peStep, peContinuation, peStepHasRollback, peDefPactId
 , peNestedDefPactExec
 , Yield(..)
 , yData, yProvenance, ySourceChain
 , Provenance(..)
 , toNestedPactExec
 , fromNestedPactExec
 ) where

-- Todo: yield
import Control.Lens
import Data.Map.Strict (Map)
import Control.DeepSeq
import GHC.Generics

import Pact.Core.PactValue
import Pact.Core.Names
import Pact.Core.Hash
import Pact.Core.ChainData
import Pact.Core.Pretty

data DefPactContinuation name v
  = DefPactContinuation
  { _pcName :: name
  , _pcArgs :: [v]
  } deriving (Eq, Show, Generic)

makeLenses ''DefPactContinuation

-- | Provenance datatype contains all of the necessary
-- data to 'endorse' a yield object.
--
data Provenance = Provenance
  { _pTargetChainId :: !ChainId
    -- ^ the target chain id for the endorsement
  , _pModuleHash :: ModuleHash
    -- ^ a hash of current containing module
  } deriving (Eq, Show, Generic)

instance Pretty Provenance where
  pretty (Provenance (ChainId cid) mh) =
    parens $
      "Provenance" <+> pretty cid <+> pretty mh

-- | `Yield` representing an object
data Yield
  = Yield
  { _yData :: Map Field PactValue
  , _yProvenance :: Maybe Provenance
  , _ySourceChain :: Maybe ChainId
  } deriving (Show, Eq, Generic)

makeLenses ''Yield

data NestedDefPactExec
  = NestedDefPactExec
  { _npeStepCount :: Int
  , _npeYield :: Maybe Yield
  , _npeStep :: Int
  , _npeDefPactId :: DefPactId
  , _npeContinuation :: DefPactContinuation QualifiedName PactValue
  , _npeNestedDefPactExec :: Map DefPactId NestedDefPactExec
  } deriving (Show, Eq, Generic)


-- | Internal representation of pacts
data DefPactExec
  = DefPactExec
  { _peStepCount :: Int
  , _peYield :: Maybe Yield
  , _peStep :: Int
  , _peDefPactId :: DefPactId
  , _peContinuation :: DefPactContinuation QualifiedName PactValue
  , _peStepHasRollback :: Bool
  , _peNestedDefPactExec :: Map DefPactId NestedDefPactExec
  } deriving (Show, Eq, Generic)

makeLenses ''DefPactExec

toNestedPactExec :: DefPactExec -> NestedDefPactExec
toNestedPactExec (DefPactExec stepCount yield step pid cont _ nested) =
  NestedDefPactExec stepCount yield step pid cont nested

fromNestedPactExec :: Bool -> NestedDefPactExec -> DefPactExec
fromNestedPactExec rollback (NestedDefPactExec stepCount yield step pid cont nested) =
  DefPactExec stepCount yield step pid cont rollback nested

data DefPactStep = DefPactStep
  { _psStep :: !Int
  , _psRollback :: !Bool
  , _psDefPactId :: !DefPactId
  , _psResume :: !(Maybe Yield)
  } deriving (Eq, Show, Generic)

makeLenses ''DefPactStep

instance NFData Provenance
instance NFData Yield
instance NFData DefPactStep
instance (NFData name, NFData v) => NFData (DefPactContinuation name v)
instance NFData NestedDefPactExec
instance NFData DefPactExec

instance (Pretty name, Pretty v) => Pretty (DefPactContinuation name v) where
  pretty (DefPactContinuation n v) =
    pretty $ PrettyLispApp n v
