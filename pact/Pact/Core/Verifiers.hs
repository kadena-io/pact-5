{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Pact.Core.Verifiers
  ( Verifier(..)
  , verifierName
  , verifierProof
  , verifierCaps
  , ParsedVerifierProof(..)
  ) where

import Control.DeepSeq
import Control.Lens
import Data.Aeson
import GHC.Generics

import qualified Pact.JSON.Decode as JD
import qualified Pact.JSON.Encode as J

import Pact.Core.Legacy.LegacyPactValue

import Pact.Core.Names
import Pact.Core.Capabilities
import Pact.Core.PactValue
import Pact.Core.StableEncoding

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

instance J.Encode a => J.Encode (Verifier a) where
  build va = J.object
    [ "name" J..= _verifierName va
    , "proof" J..= _verifierProof va
    , "clist" J..= J.build (J.Array  (StableEncoding <$>  _verifierCaps va))
    ]

instance FromJSON a => FromJSON (Verifier a) where
  parseJSON = withObject "Verifier" $ \o -> do
    name <- o .: "name"
    proof <- o .: "proof"
    legacyCaps <- o .: "clist"
    return $ Verifier name proof (_unLegacy <$> legacyCaps)


instance J.Encode ParsedVerifierProof where
  build (ParsedVerifierProof as) = J.build (StableEncoding as)

instance JD.FromJSON ParsedVerifierProof where
  parseJSON =
    fmap (ParsedVerifierProof . _stableEncoding) . (parseJSON @(StableEncoding PactValue))
