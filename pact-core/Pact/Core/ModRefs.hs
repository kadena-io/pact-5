{-# LANGUAGE TemplateHaskell #-}


module Pact.Core.ModRefs
 ( ModRef(..)
 , mrModule
 , mrImplemented
 , mrRefined
 ) where

import Control.Lens

import Data.Set(Set)
import Pact.Core.Names
import Pact.Core.Pretty

import qualified Data.Set as S

-- | Original module reference
data ModRef
  = ModRef
  { _mrModule :: ModuleName
  -- ^ Original module
  , _mrImplemented :: [ModuleName]
  -- ^ All implemented interfaces
  , _mrRefined :: Maybe (Set ModuleName)
-- ^ The "Selected" interface from a type refinement
  }
  deriving (Show)

instance Pretty ModRef where
  pretty (ModRef _mn _imp mref) = case mref of
    Just ref -> "module" <> braces (pretty (S.toList ref))
    Nothing -> "module<not refined>"

instance Eq ModRef where
  m1 == m2 = _mrModule m1 == _mrModule m2

instance Ord ModRef where
  m1 `compare` m2 = _mrModule m1 `compare` _mrModule m2

makeLenses ''ModRef
