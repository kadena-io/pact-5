{-# LANGUAGE OverloadedStrings #-}

module Pact.Core.Principal where

import Data.Text(Text)

import Pact.Core.Guards
import Pact.Core.Names

data Principal
  = K !PublicKeyText
    -- ^ format: `k:public key`, where hex public key
    -- is the text public key of the underlying keyset
  | W !Text !Text
    -- ^ format: `w:b64url-encoded hash:pred` where
    -- the hash is a b64url-encoding of the hash of
    -- the list of public keys of the multisig keyset
  | R !KeySetName
    -- ^ format: `r:keyset-name` where keyset name is
    -- any definable keyset name
  | U !Text !Text
    -- ^ format: `u:fqn of user guard function:b64url-encoded
    -- hash of args
  | M !ModuleName !Text
    -- ^ format: `m:fq module name:fqn of module guard function
  | P !PactId !Text
    -- ^ format: `p:pactid:fqn of pact function
  | C !Text
    -- ^ format: `c:hash of cap name + cap params + pactId if any
  deriving (Eq, Ord, Show)

-- | Given a principal type, construct its textual representation
--
-- Invariant: should roundtrip with parser.
--
mkPrincipalIdent :: Principal -> Text
mkPrincipalIdent = \case
  P pid n -> "p:" <> renderPactId pid <> ":" <> n
  K pk -> "k:" <> renderPublicKeyText pk
  W ph n -> "w:" <> ph <> ":" <> n
  R n -> "r:" <> renderKeySetName n
  U n ph -> "u:" <> n <> ":" <> ph
  M mn n -> "m:" <> renderModuleName mn <> ":" <> n
  C c -> "c:" <> c
