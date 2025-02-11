module Pact.Core.Crypto.Base64
  ( encodeBase64UrlUnpadded
  , decodeBase64UrlUnpadded
  , fromB64UrlUnpaddedText
  , toB64UrlUnpaddedText
  ) where

import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Word
import Data.ByteString (ByteString)

import qualified Data.ByteString as B
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

equalWord8 :: Word8
equalWord8 = toEnum $ fromEnum '='

decodeBase64UrlUnpadded :: ByteString -> Either String ByteString
decodeBase64UrlUnpadded = B64URL.decode

fromB64UrlUnpaddedText :: ByteString -> Either String Text
fromB64UrlUnpaddedText bs = case decodeBase64UrlUnpadded bs of
  Right bs' -> case T.decodeUtf8' bs' of
    Left _ -> Left "Base64URL decode failed: invalid unicode"
    Right t -> Right t
  Left _ -> Left $ "Base64URL decode failed"

encodeBase64UrlUnpadded :: ByteString -> ByteString
encodeBase64UrlUnpadded = fst . B.spanEnd (== equalWord8) . B64URL.encode

toB64UrlUnpaddedText :: ByteString -> Text
toB64UrlUnpaddedText  = decodeUtf8 . encodeBase64UrlUnpadded
