{-# LANGUAGE OverloadedStrings #-}

module Pact.Core.Principal
( Principal(..)
, mkPrincipalIdent
, showPrincipalType
, principalParser
) where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text
import Data.Char(isHexDigit)
import Data.Text(Text)
import Text.Parser.Combinators(eof)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T

import Pact.Core.Guards
import Pact.Core.Names
import Pact.Core.Parser

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
  | P !DefPactId !Text
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
  P pid n -> "p:" <> renderDefPactId pid <> ":" <> n
  K pk -> "k:" <> renderPublicKeyText pk
  W ph n -> "w:" <> ph <> ":" <> n
  R n -> "r:" <> renderKeySetName n
  U n ph -> "u:" <> n <> ":" <> ph
  M mn n -> "m:" <> renderModuleName mn <> ":" <> n
  C c -> "c:" <> c

showPrincipalType :: Principal -> Text
showPrincipalType = \case
  K{} -> "k:"
  W{} -> "w:"
  R{} -> "r:"
  U{} -> "u:"
  M{} -> "m:"
  P{} -> "p:"
  C{} -> "c:"

principalParser :: Parser Principal
principalParser = alts <* void eof
  where
    alts = kParser
       <|> wParser
       <|> rParser
       <|> uParser
       <|> mParser
       <|> pParser
       <|> cParser

    kParser = do
      prefix 'k'
      K <$> hexKeyFormat

    wParser = do
      prefix 'w'
      binCtor W base64UrlHashParser nameMatcher

    rParser = do
      prefix 'r'
      R <$> keysetNameParser

    uParser = do
      prefix 'u'
      binCtor U nameMatcher base64UrlHashParser

    mParser = do
      prefix 'm'
      binCtor M moduleNameParser nameMatcher

    pParser = do
      prefix 'p'
      binCtor P (DefPactId <$> base64UrlHashParser) nameMatcher

    cParser = do
      prefix 'c'
      C <$> base64UrlHashParser

    binCtor :: (a -> b -> Principal) -> Parser a -> Parser b -> Parser Principal
    binCtor ctor p1 p2 = ctor <$> p1 <*> (char ':' *> p2)

    hexKeyFormat = PublicKeyText . T.pack <$> count 64 (satisfy isHexDigit)

    base64UrlUnpaddedAlphabet :: BS.ByteString
    base64UrlUnpaddedAlphabet =
      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"

    base64UrlHashParser = T.pack <$> count 43 (satisfy (`BS.elem` base64UrlUnpaddedAlphabet))

    char' = void . char
    prefix ch = char ch >> char' ':'
