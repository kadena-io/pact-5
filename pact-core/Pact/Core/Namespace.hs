{-# LANGUAGE TemplateHaskell #-}

module Pact.Core.Namespace where

import Control.Lens

import Pact.Core.Names
import Pact.Core.Guards
import Pact.Core.PactValue

data Namespace = Namespace
  { _nsName :: !NamespaceName
  , _nsUser :: !(Guard FullyQualifiedName PactValue)
  , _nsAdmin :: !(Guard FullyQualifiedName PactValue)
  } deriving (Eq, Show)

makeLenses ''Namespace
