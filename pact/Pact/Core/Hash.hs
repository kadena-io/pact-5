{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}


module Pact.Core.Hash
( Hash(..)
, ModuleHash(..)
, hash
, hashToText
, verifyHash
, initialHash
, pactHash
, pactInitialHash
, pactHashLength
, encodeBase64UrlUnpadded
, decodeBase64UrlUnpadded
, toB64UrlUnpaddedText
, fromB64UrlUnpaddedText
, defaultPactHash
, placeholderHash
, moduleHashToText
, parseHash
, parseModuleHash
, hashToByteString
) where

import Control.DeepSeq
import Data.Aeson
import Data.ByteString.Short (ShortByteString, fromShort, toShort)
import Data.ByteString (ByteString)
import Data.Hashable (Hashable)
import Data.Serialize (Serialize)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Word
import GHC.Generics

import qualified Data.ByteString as B
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Pact.JSON.Encode as J
import qualified Pact.JSON.Decode as JD

import qualified Data.ByteArray as ByteArray
import qualified Crypto.Hash as Crypto

import Pact.Core.Pretty ( renderCompactString, Pretty(pretty) )

-- | Untyped hash value, encoded with unpadded base64url.
-- Within Pact these are blake2b_256 but unvalidated as such,
-- so other hash values are kosher (such as an ETH sha256, etc).
newtype Hash = Hash { unHash :: ShortByteString }
  deriving (Eq, Ord, NFData, Generic, Serialize, Hashable)

instance Show Hash where
  show (Hash h) = show $ encodeBase64UrlUnpadded $ fromShort h

instance FromJSONKey Hash where
  fromJSONKey = FromJSONKeyTextParser $ \t -> case decodeBase64UrlUnpadded (T.encodeUtf8 t) of
    Left e -> fail e
    Right bs | B.length bs == pactHashLength -> pure (unsafeBsToPactHash bs)
             | otherwise -> fail ("Invalid hash length " <> show (T.length t))

instance Pretty Hash where
  pretty (Hash h) =
    pretty $ decodeUtf8 (encodeBase64UrlUnpadded (fromShort h))

hashToText :: Hash -> Text
hashToText (Hash h) = toB64UrlUnpaddedText (fromShort h)

instance FromJSON Hash where
  parseJSON = withText "Hash" $ \t -> case decodeBase64UrlUnpadded (T.encodeUtf8 t) of
    Left e -> fail e
    Right bs | B.length bs == pactHashLength -> pure (unsafeBsToPactHash bs)
             | otherwise -> fail ("Invalid hash length " <> show (T.length t))

instance J.Encode Hash where
  build = J.build . hashToText


moduleHashToText :: ModuleHash -> Text
moduleHashToText (ModuleHash h) = hashToText h

pactHash :: ByteString -> Hash
pactHash = hash

pactInitialHash :: Hash
pactInitialHash = initialHash

pactHashLength :: Int
pactHashLength = 32

-- | Creates a 'Hash' value directly from a 'ByteString' without using the hashing functions.
-- This function is unsafe because it bypasses the hashing process and assumes the provided 'ByteString'
-- is a valid hash.
unsafeBsToPactHash :: ByteString -> Hash
unsafeBsToPactHash = Hash . toShort

hashToByteString :: Hash -> ByteString
hashToByteString = fromShort . unHash


parseHash :: Text -> Maybe Hash
parseHash t = case decodeBase64UrlUnpadded (T.encodeUtf8 t) of
  Left{} -> Nothing
  Right bs | B.length bs == pactHashLength -> Just (unsafeBsToPactHash bs)
           | otherwise -> Nothing

parseModuleHash :: Text -> Maybe ModuleHash
parseModuleHash = fmap ModuleHash . parseHash

hash :: ByteString -> Hash
hash = unsafeBsToPactHash . ByteArray.convert . Crypto.hashWith Crypto.Blake2b_256

verifyHash :: Hash -> ByteString -> Either String Hash
verifyHash h b = if hashed == h
  then Right h
  else Left $ "Hash Mismatch, received " ++ renderCompactString h ++
              " but our hashing resulted in " ++ renderCompactString hashed
  where hashed = hash b

-- NOTE: This instance is unsafe, but _really_ useful for debugging pasted
-- gas logs.
-- Uncomment and import Data.String(IsString(..)) to enable.
-- instance IsString Hash where
--   fromString s = case (decodeBase64UrlUnpadded $ T.encodeUtf8 (T.pack s)) of
--     Right h -> Hash $ toShort h
--     Left _ -> error $ "Invalid hash string: " ++ s

initialHash :: Hash
initialHash = hash mempty

equalWord8 :: Word8
equalWord8 = toEnum $ fromEnum '='

toB64UrlUnpaddedText :: ByteString -> Text
toB64UrlUnpaddedText  = decodeUtf8 . encodeBase64UrlUnpadded

encodeBase64UrlUnpadded :: ByteString -> ByteString
encodeBase64UrlUnpadded = fst . B.spanEnd (== equalWord8) . B64URL.encode

decodeBase64UrlUnpadded :: ByteString -> Either String ByteString
decodeBase64UrlUnpadded = B64URL.decode . pad
  where pad t = let s = B.length t `mod` 4 in t <> B.replicate ((4 - s) `mod` 4) equalWord8

fromB64UrlUnpaddedText :: ByteString -> Either String Text
fromB64UrlUnpaddedText bs = case decodeBase64UrlUnpadded bs of
  Right bs' -> case T.decodeUtf8' bs' of
    Left _ -> Left "Base64URL decode failed: invalid unicode"
    Right t -> Right t
  Left _ -> Left $ "Base64URL decode failed"


newtype ModuleHash = ModuleHash { _mhHash :: Hash }
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (NFData, J.Encode, JD.FromJSON)

instance Pretty ModuleHash where
  pretty (ModuleHash h) = pretty h

placeholderHash :: ModuleHash
placeholderHash = ModuleHash (Hash "#placeholder")

defaultPactHash :: Hash
defaultPactHash = pactHash ""
