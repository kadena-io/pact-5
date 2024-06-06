{-# LANGUAGE TemplateHaskell #-}


module Pact.Core.ModRefs
 ( ModRef(..)
 , mrModule
 , mrImplemented
 ) where

import Control.Lens
import Control.DeepSeq
import Data.Set(Set)
import GHC.Generics
import qualified Data.Set as S

import Pact.Core.Names
import Pact.Core.Pretty


-- | Original module reference
data ModRef
  = ModRef
  { _mrModule :: ModuleName
  -- ^ Original module
  , _mrImplemented :: Set ModuleName
  -- ^ All implemented interfaces
  }
  deriving (Show, Generic)

instance NFData ModRef

instance Pretty ModRef where
  pretty (ModRef _mn imps) =
    "module" <> braces (pretty (S.toList imps))

instance Eq ModRef where
  m1 == m2 = _mrModule m1 == _mrModule m2

instance Ord ModRef where
  m1 `compare` m2 = _mrModule m1 `compare` _mrModule m2

makeLenses ''ModRef
