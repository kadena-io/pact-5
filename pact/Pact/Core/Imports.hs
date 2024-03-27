{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}

module Pact.Core.Imports
( Import(..)
)
where

import Control.DeepSeq
import Data.Text(Text)
import GHC.Generics
import Pact.Core.Names
import Pact.Core.Hash


data Import
  = Import
  { _impModuleName  :: ModuleName
  , _impModuleHash :: Maybe ModuleHash
  , _impImported :: Maybe [Text]
  } deriving (Show, Eq, Generic)

instance NFData Import
