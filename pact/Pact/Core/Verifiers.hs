{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}

module Pact.Core.Verifiers
  ( VerifierName(..)
  , Verifier(..)
  , verifierName
  , verifierProof
  , verifierCaps
  , ParsedVerifierProof(..)
  ) where

import Control.DeepSeq
import Control.Lens
import Data.Aeson
import Data.Text
import GHC.Generics

import qualified Pact.JSON.Encode as J

import Pact.Core.Names
import Pact.Core.Capabilities
import Pact.Core.PactValue

newtype VerifierName = VerifierName Text
  deriving newtype (J.Encode, NFData, Eq, Show, Ord, FromJSON)
  deriving stock Generic

data Verifier prf = Verifier
  { _verifierName :: VerifierName
  , _verifierProof :: prf
  , _verifierCaps :: [CapToken QualifiedName PactValue]
  }
  deriving (Eq, Show, Generic, Ord, Functor, Foldable, Traversable)

makeLenses ''Verifier

instance NFData a => NFData (Verifier a)

newtype ParsedVerifierProof = ParsedVerifierProof PactValue
  deriving newtype (NFData, Eq, Show, Ord)
  deriving stock Generic
