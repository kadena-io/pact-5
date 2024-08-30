{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Pact.Core.Signer
 ( SigCapability(..)
 , Signer(..) )
 where

import Data.Text(Text)
import Control.DeepSeq
import GHC.Generics
import Data.Maybe(fromMaybe)

import Pact.Core.Capabilities
import Pact.Core.PactValue
import Pact.Core.Names
import Pact.Core.Scheme
import Pact.Core.StableEncoding

import qualified Pact.JSON.Encode as J
import qualified Pact.JSON.Decode as JD


-- | Type corresponding to user-facing capabilities
--
newtype SigCapability
  = SigCapability { _sigCapability :: CapToken QualifiedName PactValue }
  deriving newtype (Eq, Ord, Show, NFData)

-- | Signer combines PPKScheme, PublicKey, and the Address (aka the
--   formatted PublicKey).
data Signer = Signer
 { _siScheme :: !(Maybe PPKScheme)
 -- ^ PPKScheme, which is defaulted to 'defPPKScheme' if not present
 , _siPubKey :: !Text
 -- ^ pub key value
 , _siAddress :: !(Maybe Text)
 -- ^ optional "address", for different pub key formats like ETH
 , _siCapList :: [SigCapability]
 -- ^ clist for designating signature to specific caps
 } deriving (Eq, Ord, Show, Generic)

instance NFData Signer

instance J.Encode SigCapability where
  build (SigCapability (CapToken name args)) = J.object
    [ "name" J..= J.build (StableEncoding name)
    , "args" J..= J.build (J.Array (StableEncoding <$> args))
    ]

instance JD.FromJSON SigCapability where
  parseJSON = JD.withObject "SigCapability" $ \o -> do
    name <- o JD..: "name"
    args <- o JD..: "args"
    pure $ SigCapability $ CapToken (_stableEncoding name) (_stableEncoding <$> args)


instance J.Encode Signer where
  build o = J.object
    [ "addr" J..?= _siAddress o
    , "scheme" J..?= _siScheme o
    , "pubKey" J..= _siPubKey o
    , "clist" J..??= J.Array (_siCapList o)
    ]

instance JD.FromJSON Signer where
  parseJSON = JD.withObject "Signer" $ \o -> do
    scheme <- o JD..:? "scheme"
    pubKey <- o JD..: "pubKey"
    addr <- o JD..:? "addr"
    clist <- fromMaybe [] <$> o JD..:? "clist"
    pure $ Signer scheme pubKey addr clist
