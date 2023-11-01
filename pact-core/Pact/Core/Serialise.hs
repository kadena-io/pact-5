-- | 

module Pact.Core.Serialise where

import Data.ByteString (ByteString, fromStrict)
import Data.Word (Word32)

import Pact.Core.Info
import Pact.Core.Builtin
import Pact.Core.IR.Term
import qualified Codec.Serialise as S
import qualified Codec.CBOR.Encoding as S
import qualified Codec.CBOR.Decoding as S
import Codec.CBOR.Write (toStrictByteString)

import Pact.Core.Serialise.CBOR ()
import Data.Bifunctor

import Data.Int (Int64)

-- | Document version
newtype DocumentVersion
  = DocumentVersion { unDocumentVersion :: Word32 }
  deriving (Show, Eq, Ord)


-- | Supported Document Formats
data DocumentFormat
  = DocumentCBOR
  deriving (Show, Eq, Enum, Bounded)


data DecodeError
  = DecodeFailure Int64 String
  deriving (Show, Eq)


data Document a
  = Document
  { _documentVersion :: DocumentVersion
  , _documentFormat :: DocumentFormat
  , _documentContent :: a
  } deriving (Show, Eq)

data Serialise
  = Serialise
  { _encodeModule :: EvalModule RawBuiltin SpanInfo -> ByteString
  , _decodeModule :: ByteString -> Either DecodeError (Document (EvalModule RawBuiltin SpanInfo))
  }


serialiseCBOR :: Serialise
serialiseCBOR = Serialise
  { _encodeModule = toStrictByteString . S.encode . Document version format
  , _decodeModule = first toErr . S.deserialiseOrFail . fromStrict
  }
  where
    version = DocumentVersion 0
    format = DocumentCBOR
    toErr (S.DeserialiseFailure offset msg) = DecodeFailure offset msg

instance S.Serialise a => S.Serialise (Document a) where
  encode (Document v f c) = S.encode v <> S.encode f <> S.encode c
  decode = Document <$> S.decode <*> S.decode <*> S.decode

instance S.Serialise DocumentVersion where
  encode (DocumentVersion v) = S.encode v
  decode = DocumentVersion <$> S.decode

instance S.Serialise DocumentFormat where
  encode = \case
    DocumentCBOR -> S.encodeWord 0
  decode = S.decodeWord >>= \case
    0 -> pure DocumentCBOR
    _ -> fail "unexpected decoding"
