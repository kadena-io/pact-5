module Pact.Core.Command.Client (

  -- * Command construction
  mkCommand,
  mkCommand',
  mkUnsignedCommand,

  -- * Command construction with dynamic keys (Ed25519 and WebAuthn)
  mkCommandWithDynKeys,
  mkCommandWithDynKeys',
) where

import Control.Monad.Except
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Pact.JSON.Encode as J

import Pact.Core.Capabilities
import Pact.Core.ChainData
import Pact.Core.Command.RPC
import Pact.Core.Command.Types
import Pact.Core.Command.Util
import Pact.Core.Command.Crypto
import Pact.Core.Guards
import qualified Pact.Core.Hash as PactHash
import Pact.Core.PactValue
import Pact.Core.Names
import Pact.Core.Verifiers


-- | Construct a `Command` from a `PactRPC` request, a nonce, and a set of credentials.
-- This is the main entry point for constructing commands.
mkCommand
  :: J.Encode c
  => J.Encode m
  => [(Ed25519KeyPair, [UserCapability])]
  -> [Verifier ParsedVerifierProof]
  -> m
  -> Text
  -> Maybe NetworkId
  -> PactRPC c
  -> IO (Command ByteString)
mkCommand creds vers meta nonce nid rpc = mkCommand' creds encodedPayload
  where
    payload = Payload rpc nonce meta (keyPairsToSigners creds) (nonemptyVerifiers vers) nid
    encodedPayload = J.encodeStrict payload


keyPairToSigner :: Ed25519KeyPair -> [UserCapability] -> Signer QualifiedName PactValue
keyPairToSigner cred caps = Signer scheme pub addr caps
      where
        scheme = Nothing
        pub = toB16Text $ exportEd25519PubKey $ fst cred
        addr = Nothing

keyPairsToSigners :: [Ed25519KeyPairCaps] -> [Signer QualifiedName PactValue]
keyPairsToSigners creds = map (uncurry keyPairToSigner) creds

signHash :: PactHash.Hash -> Ed25519KeyPair -> Text
signHash hsh (pub,priv) =
  toB16Text $ exportEd25519Signature $ signEd25519 pub priv hsh

-- | Make a Command without signing it. This is used in Chainweb's Rosetta Utils.
mkUnsignedCommand
  :: J.Encode m
  => J.Encode c
  => [Signer QualifiedName PactValue]
  -> [Verifier ParsedVerifierProof]
  -> m
  -> Text
  -> Maybe NetworkId
  -> PactRPC c
  -> IO (Command ByteString)
mkUnsignedCommand signers vers meta nonce nid rpc = mkCommand' [] encodedPayload
  where encodedPayload = J.encodeStrict payload
        payload = Payload rpc nonce meta signers (nonemptyVerifiers vers) nid

-- | Given an already-serialized payload, construct a `Command` by signing it with the
-- provided credentials.
mkCommand' :: [(Ed25519KeyPair ,a)] -> ByteString -> IO (Command ByteString)
mkCommand' creds env = do
  let hsh = PactHash.hash env    -- hash associated with a Command, aka a Command's Request Key
      toUserSig (cred,_) = ED25519Sig $ signHash hsh cred
  let sigs = toUserSig <$> creds
  return $ Command env sigs hsh


-- | A utility function used for testing.
-- It generalizes `mkCommand` by taking a `DynKeyPair`, which could contain mock
-- WebAuthn keys. If WebAuthn keys are encountered, this function does mock WebAuthn
-- signature generation when constructing the `Command`.
mkCommandWithDynKeys' :: [(DynKeyPair, a)] -> ByteString -> IO (Command ByteString)
mkCommandWithDynKeys' creds env = do
  let hsh = PactHash.hash env    -- hash associated with a Command, aka a Command's Request Key
  sigs <- traverse (toUserSig hsh) creds
  return $ Command env sigs hsh
  where
    toUserSig :: PactHash.Hash -> (DynKeyPair, a) -> IO UserSig
    toUserSig hsh = \case
      (DynEd25519KeyPair (pub, priv), _) ->
        pure $ ED25519Sig $ signHash hsh (pub, priv)
      (DynWebAuthnKeyPair _ pubWebAuthn privWebAuthn, _) -> do
        signResult <- runExceptT $ signWebauthn pubWebAuthn privWebAuthn "" hsh
        case signResult of
          Left e -> error $ "Failed to sign with mock WebAuthn keypair: " ++ e
          Right sig -> return $ WebAuthnSig sig


-- | Construct a `Command` from a `PactRPC` request, a nonce, and a set of credentials.
-- This function is mainy useful for testing, because DynKeyPairs are either
-- Ed25519 or WebAuthn keys. If you have Ed25519 keys, use `mkCommand`. You will
-- only have access to WebAuthn keys in test (because they are normally managed on
-- the client's and never exposed to the client or the server).
--
-- During testing, you can create fake WebAuthn credentials.
mkCommandWithDynKeys
  :: J.Encode c
  => J.Encode m
  => [(DynKeyPair, [UserCapability])]
  -> [Verifier ParsedVerifierProof]
  -> m
  -> Text
  -> Maybe NetworkId
  -> PactRPC c
  -> IO (Command ByteString)
mkCommandWithDynKeys creds vers meta nonce nid rpc = mkCommandWithDynKeys' creds encodedPayload
  where
    encodedPayload = J.encodeStrict payload
    payload = Payload rpc nonce meta (map credToSigner creds) (nonemptyVerifiers vers) nid
    credToSigner cred =
      case cred of
        (DynEd25519KeyPair (pubEd25519, _), caps) ->
          Signer
            { _siScheme = Nothing
            , _siPubKey = toB16Text (exportEd25519PubKey pubEd25519)
            , _siAddress = Nothing
            , _siCapList = caps
            }
        (DynWebAuthnKeyPair isPrefixed pubWebAuthn _, caps) ->
          let
            prefix = case isPrefixed of
              WebAuthnPubKeyBare -> ""
              WebAuthnPubKeyPrefixed -> webAuthnPrefix
          in Signer
            { _siScheme = Just WebAuthn
            , _siPubKey = prefix <> toB16Text (exportWebAuthnPublicKey pubWebAuthn)
            , _siAddress = Nothing
            , _siCapList = caps
            }

type UserCapability = CapToken QualifiedName PactValue

-- | A utility function for handling the common case of commands
-- with no verifiers. `None` is distinguished from `Just []` in
-- our JSON encodings, which is important for maintaining forward
-- compatibility - old version of the interpreter did non include
-- a `verifiers` field.
nonemptyVerifiers :: [Verifier ParsedVerifierProof] -> Maybe [Verifier ParsedVerifierProof]
nonemptyVerifiers [] = Nothing
nonemptyVerifiers vs = Just vs