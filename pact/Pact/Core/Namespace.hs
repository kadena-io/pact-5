{-# LANGUAGE TemplateHaskell #-}

module Pact.Core.Namespace
 ( Namespace(..)
 , nsName, nsUser, nsAdmin
 , NamespacePolicy(..)
 ) where

import Control.Lens
import Control.DeepSeq
import GHC.Generics

import Pact.Core.Names
import Pact.Core.Guards
import Pact.Core.PactValue

data Namespace = Namespace
  { _nsName :: !NamespaceName
  , _nsUser :: !(Guard QualifiedName PactValue)
  , _nsAdmin :: !(Guard QualifiedName PactValue)
  } deriving (Eq, Show, Generic)

instance NFData Namespace

makeLenses ''Namespace

-- | Governance of namespace use. Policy dictates:
-- 1. Whether a namespace can be created.
-- 2. Whether the default namespace can be used.
data NamespacePolicy
  = SimpleNamespacePolicy
  -- ^ if namespace is Nothing/root, govern usage; otherwise govern creation.
  | SmartNamespacePolicy !Bool !QualifiedName
  -- ^ Bool governs root usage, Name governs ns creation.
  -- Def is (defun xxx:bool (ns:string ns-admin:guard))
  deriving (Show, Generic)

instance NFData NamespacePolicy
