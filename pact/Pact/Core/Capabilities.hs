{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StrictData #-}


module Pact.Core.Capabilities
 ( DefCapMeta(..)
 , DefManagedMeta(..)
 , CapToken(..)
 , ctName, ctArgs
 , CapSlot(..)
 , csCap, csComposed
 , CapState(..)
 , csSlots, csManaged
 , csModuleAdmin, csAutonomous
 , csCapsBeingEvaluated
 , ManagedCap(..)
 , mcCap, mcManaged, mcOriginalCap
 , ManagedCapType(..)
 , PactEvent(..)
 , dcMetaFqName
 , Signer(..)
 , getManagedParam
 ) where

import Control.Lens
import Control.DeepSeq
import Data.Text(Text)
import Data.Set(Set)
import Data.Default
import GHC.Generics


import Pact.Core.Pretty
import Pact.Core.Names
import Pact.Core.Hash ( ModuleHash )
import Pact.Core.Scheme

data DefManagedMeta name
  = DefManagedMeta (Int, Text) name
  | AutoManagedMeta
  deriving (Show, Functor, Foldable, Traversable, Eq, Generic)

data DefCapMeta name
  = DefEvent
  | DefManaged (DefManagedMeta name)
  | Unmanaged
  deriving (Show, Functor, Foldable, Traversable, Eq, Generic)

dcMetaFqName :: Traversal' (DefCapMeta (FQNameRef Name)) FullyQualifiedName
dcMetaFqName f = \case
  DefManaged (DefManagedMeta i (FQName fqn)) ->
    DefManaged . DefManagedMeta i . FQName <$> f fqn
  p -> pure p

-- | An acquired capability token
-- with the reference
data CapToken name v
  = CapToken
  { _ctName :: name
  , _ctArgs :: [v]
  } deriving (Show, Eq, Ord, Functor, Generic)


--
data CapSlot name v
 = CapSlot
 { _csCap :: CapToken name v
 , _csComposed :: [CapToken name v]
 } deriving (Show, Eq, Generic)


-- | The overall capability state
data CapState name v
  = CapState
  { _csSlots :: [CapSlot name v]
  , _csManaged :: Set (ManagedCap name v)
  , _csModuleAdmin :: Set ModuleName
  , _csAutonomous :: Set (CapToken name v)
  , _csCapsBeingEvaluated :: Set (CapToken name v)
  }
  deriving (Show, Generic)


instance (Ord name, Ord v) => Default (CapState name v) where
  def = CapState mempty mempty mempty mempty mempty

-- | Our pact event type.
--   Note: the name + module are isomorphic to a
--   QualifiedName, but it is kept in this format currently for
--   ease of legacy integration
data PactEvent v
  = PactEvent
  { _peName :: Text
  , _peArgs :: [v]
  , _peModule :: ModuleName
  , _peModuleHash :: ModuleHash
  } deriving (Show, Eq, Generic)


data ManagedCapType v
  = AutoManaged Bool
  | ManagedParam FullyQualifiedName v Int
  -- ^ managed cap, with manager function, managed value
  deriving (Show, Generic)


data ManagedCap name v
  = ManagedCap
  { _mcCap :: CapToken name v
  -- ^ The token without the managed param
  , _mcOriginalCap :: CapToken name v
  -- ^ The original, installed token
  , _mcManaged :: ManagedCapType v
  -- ^ Managed capability type
  } deriving (Show, Generic)

getManagedParam :: ManagedCap name v -> Maybe v
getManagedParam (ManagedCap _mc _orig (ManagedParam _ v _)) = Just v
getManagedParam _ = Nothing

instance (Eq name, Eq v) => Eq (ManagedCap name v) where
  l == r = _mcCap l == _mcCap r

instance (Ord name, Ord v) => Ord (ManagedCap name v) where
  l `compare` r = _mcCap l `compare` _mcCap r

makeLenses ''CapState
makeLenses ''CapToken
makeLenses ''CapSlot
makeLenses ''ManagedCap

-- | Signer combines PPKScheme, PublicKey, and the Address (aka the
--   formatted PublicKey).
data Signer name v = Signer
 { _siScheme :: !(Maybe PPKScheme)
 -- ^ PPKScheme, which is defaulted to 'defPPKScheme' if not present
 , _siPubKey :: !Text
 -- ^ pub key value
 , _siAddress :: !(Maybe Text)
 -- ^ optional "address", for different pub key formats like ETH
 , _siCapList :: [CapToken name v]
 -- ^ clist for designating signature to specific caps
 } deriving (Eq, Ord, Show, Generic)

instance (Pretty name, Pretty v) => Pretty (CapToken name v) where
  pretty (CapToken qn args) =
    pretty $ PrettyLispApp qn args

instance (NFData name, NFData v) => NFData (Signer name v)
instance (NFData name, NFData v) => NFData (ManagedCap name v)
instance NFData v => NFData (ManagedCapType v)
instance NFData v => NFData (PactEvent v)
instance (NFData name, NFData v) => NFData (CapState name v)
instance (NFData name, NFData v) => NFData (CapSlot name v)
instance (NFData name, NFData v) => NFData (CapToken name v)
instance (NFData name) => NFData (DefManagedMeta name)
instance (NFData name) => NFData (DefCapMeta name)
