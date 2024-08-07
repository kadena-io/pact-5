{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Pact.Types.Util
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Utility types and functions.
--
module Pact.Core.Command.Util
  (
  -- | JSON helpers
    outputJSON
  , fromJSON'
  -- | Base 16 helpers
  , parseB16JSON, parseB16Text, parseB16TextOnly
  , toB16Text
  , B16JsonBytes(..)
  -- | Base64Url helpers
  , encodeBase64UrlUnpadded, decodeBase64UrlUnpadded
  , parseB64UrlUnpaddedText, parseB64UrlUnpaddedText'
  , toB64UrlUnpaddedText, fromB64UrlUnpaddedText
  , B64JsonBytes(..)
  ) where

import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Hashable (Hashable)
import Data.Text (Text)
import Data.Text.Encoding

import Pact.Core.Hash
import qualified Pact.JSON.Encode as J

resultToEither :: Result a -> Either String a
resultToEither (Success s) = Right s
resultToEither (Error s) = Left s

fromJSON' :: FromJSON a => Value -> Either String a
fromJSON' = resultToEither . fromJSON


parseB64UrlUnpaddedText :: Text -> Parser ByteString
parseB64UrlUnpaddedText t = case decodeBase64UrlUnpadded (encodeUtf8 t) of
  Right s -> return s
  Left e -> fail $ "Base64URL decode failed: " ++ e
{-# INLINE parseB64UrlUnpaddedText #-}

parseB64UrlUnpaddedText' :: Text -> Either String ByteString
parseB64UrlUnpaddedText' = resultToEither . parse parseB64UrlUnpaddedText



parseB16JSON :: Value -> Parser ByteString
parseB16JSON = withText "Base16" parseB16Text
{-# INLINE parseB16JSON #-}

parseB16Text :: Text -> Parser ByteString
parseB16Text t = case B16.decode (encodeUtf8 t) of
  Right bs -> return bs
  Left _ -> fail $ "Base16 decode failed: " ++ show t
{-# INLINE parseB16Text #-}

parseB16TextOnly :: Text -> Either String ByteString
parseB16TextOnly t = resultToEither $ parse parseB16Text t

toB16Text :: ByteString -> Text
toB16Text s = decodeUtf8 $ B16.encode s

-- | Utility for GHCI output of JSON
outputJSON :: J.Encode a => a -> IO ()
outputJSON a = BSL8.putStrLn $ J.encode a

-- | Tagging ByteStrings (and isomorphic types) that are JSON encoded as
-- Hex strings
newtype B16JsonBytes = B16JsonBytes { _b16JsonBytes :: B.ByteString }
    deriving (Show, Eq, Ord, Hashable, Generic)

instance FromJSON B16JsonBytes where
    parseJSON = fmap B16JsonBytes . parseB16JSON
    {-# INLINE parseJSON #-}
instance FromJSONKey B16JsonBytes where
    fromJSONKey = FromJSONKeyTextParser (fmap B16JsonBytes . parseB16Text)
    {-# INLINE fromJSONKey #-}

-- | Tagging ByteStrings (and isomorphic types) that are JSON encoded as
-- Base64Url (without padding) strings
newtype B64JsonBytes = B64JsonBytes { _b64JsonBytes :: B.ByteString }
    deriving (Show, Eq, Ord, Hashable, Generic)

instance FromJSON B64JsonBytes where
    parseJSON = fmap B64JsonBytes . withText "Base64" parseB64UrlUnpaddedText
    {-# INLINE parseJSON #-}
instance FromJSONKey B64JsonBytes where
    fromJSONKey = FromJSONKeyTextParser (fmap B64JsonBytes . parseB64UrlUnpaddedText)
    {-# INLINE fromJSONKey #-}
