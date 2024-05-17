module Pact.Core.SPV
 ( ContProof(..)
 , SPVSupport(..)
 , noSPVSupport)where

import Data.Text(Text)
import Data.ByteString(ByteString)
import GHC.Generics
import Control.DeepSeq

import Pact.Core.DefPacts.Types
import Pact.Core.PactValue

newtype ContProof = ContProof { _contProof :: ByteString }
  deriving (Eq, Ord, Show, Generic)


instance NFData ContProof

-- | Backend for SPV support
data SPVSupport = SPVSupport
  { _spvSupport :: !(Text -> (ObjectData PactValue) -> IO (Either Text (ObjectData PactValue)))
    -- ^ Attempt to verify an SPV proof of a given type,
    -- given a payload object. On success, returns the
    -- specific data represented by the proof.
  , _spvVerifyContinuation :: !(ContProof -> IO (Either Text DefPactExec))
    -- ^ Attempt to verify an SPV proof of a continuation given
    -- a continuation payload object bytestring. On success, returns
    -- the 'PactExec' associated with the proof.
  } deriving (Generic)

instance NFData SPVSupport

noSPVSupport :: SPVSupport
noSPVSupport = SPVSupport spv vcon
  where
    spv = \_ _ -> return $ Left "SPV verification not supported"
    vcon = \_ -> return $ Left "Cross-chain continuations not supported"
