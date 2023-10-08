{-# LANGUAGE DeriveTraversable #-}


module Pact.Core.Guards
( PublicKeyText(..)
, KeySetName(..)
, Governance(..)
, KeySet(..)
, Guard(..)
, UserGuard(..)
, CapabilityGuard(..)
, KSPredicate(..)
)
where

import Data.Text(Text)
import qualified Data.Set as S

import Pact.Core.Names

newtype PublicKeyText = PublicKeyText { _pubKey :: Text }
  deriving (Eq,Ord,Show)

newtype KeySetName = KeySetName Text
    deriving (Eq,Ord,Show)

data Governance name
  = KeyGov KeySetName
  | CapGov name
  deriving (Eq, Show, Functor, Foldable, Traversable)

data KSPredicate name
  = KeysAll
  | Keys2
  | KeysAny
  -- | CustomPredicate name
  deriving (Eq, Show, Ord)

data KeySet name
  = KeySet
  { _ksKeys :: !(S.Set PublicKeyText)
  , _ksPredFun :: KSPredicate name
  } deriving (Eq, Show, Ord)

data UserGuard name term
  = UserGuard
  { _ugFunction :: name
  , _ugArgs :: [term] }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data CapabilityGuard name term
  = CapabilityGuard
  { _cgName :: !name
  , _cgArgs :: ![term] }
    -- , _cgPactId :: !(Maybe PactId)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Guard name term
  = GKeyset (KeySet name)
  | GKeySetRef KeySetName
  | GUserGuard (UserGuard name term)
  | GCapabilityGuard (CapabilityGuard name term)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Namespace name term
  = Namespace
  { _nsName :: !NamespaceName
  , _nsUser :: !(Guard name term)
  , _nsAdmin :: !(Guard name term)
  } deriving (Eq, Show)
