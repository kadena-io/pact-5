module Pact.Core.ModRefs
 ( ModRef(..)
 ) where

import Pact.Core.Names
import Pact.Core.Pretty


-- | Original module reference
data ModRef
  = ModRef
  { _mrModule :: ModuleName
  -- ^ Original module
  , _mrImplemented :: [ModuleName]
  -- ^ All implemented interfaces
  , _mrRefined :: Maybe ModuleName
  -- ^ The "Selected" interface from a type refinement
  }
  deriving (Show)

instance Pretty ModRef where
  pretty (ModRef _mn _imp mref) = case mref of
    Just ref -> "module" <> braces (pretty ref)
    Nothing -> "module<not refined>"

instance Eq ModRef where
  m1 == m2 = _mrModule m1 == _mrModule m2

instance Ord ModRef where
  m1 `compare` m2 = _mrModule m1 `compare` _mrModule m2
