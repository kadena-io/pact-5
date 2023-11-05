-- | The canonical way of encoding and decoding Pact entities into bytestrings.
--   There are two places where in Pact where serialization is needed:
--     - Computing module hashes
--     - Reading and writing the Pact Database
--
--  Normal usage of this module involes the `serializeModuleForHash` function,
--  and `defaultSerializeForDatabase`.

module Pact.Core.Serialise where

import Data.ByteString (ByteString, fromStrict)
import Data.Word (Word32)

-- import Pact.Core.Info
-- import Pact.Core.Builtin
--import Pact.Core.IR.Term
import Pact.Core.Persistence
import Pact.Core.Guards
import Pact.Core.Names

import qualified Codec.Serialise as S
import qualified Codec.CBOR.Encoding as S
import qualified Codec.CBOR.Decoding as S
import Codec.CBOR.Write (toStrictByteString)

import Pact.Core.Serialise.CBOR ()
import Data.Bifunctor

import Data.Int (Int64)

-- | A Document in the sense of a Document-oriented database.
--   Documents contain an abstract value (represented
--   by the type parameter `a`), the version number and the
--   encoding scheme for the value.
--
--   Documents are produced from bytestrings through one of the
--   decoding methods in a `Serialise` record.
data Document a
  = Document
  { _documentVersion :: DocumentVersion
  , _documentFormat :: DocumentFormat
  , _documentContent :: a
  } deriving (Show, Eq)

-- | Document version
newtype DocumentVersion
  = DocumentVersion { unDocumentVersion :: Word32 }
  deriving (Show, Eq, Ord)


-- | Supported Document Formats
data DocumentFormat
  = DocumentCBOR
  | DocumentCanonicalJSON
  -- ^ A JSON encoding with all forms of nondeterminism removed:
  --     Lexographic keys, stripped whitespace.
  deriving (Show, Eq, Enum, Bounded)


data DecodeError
  = DecodeFailure Int64 String
  deriving (Show, Eq)

-- | A Serializer that encodes in CBOR at the latest version, and attempts
--   to decode at each possible version, starting from the most recent.
defaultSerializeForDatabase :: PactSerialise b i
defaultSerializeForDatabase = undefined


-- | The main serialization API for Pact entities.
data PactSerialise b i
  = PactSerialise
  { _encodeModuleData :: ModuleData b i -> ByteString
  , _decodeModuleData :: ByteString -> Either DecodeError (Document (ModuleData b i))
  , _encodeKeySet :: KeySet FullyQualifiedName -> ByteString
  , _decodeKeySet :: ByteString -> Either DecodeError (Document (KeySet FullyQualifiedName))
  }


serialiseCBOR :: (S.Serialise b, S.Serialise i) => PactSerialise b i
serialiseCBOR = PactSerialise
  { _encodeModuleData = toStrictByteString . S.encode . Document version format
  , _decodeModuleData = first toErr . S.deserialiseOrFail . fromStrict
  , _encodeKeySet = toStrictByteString . S.encode . Document version format
  , _decodeKeySet = first toErr . S.deserialiseOrFail . fromStrict
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
    DocumentCanonicalJSON -> S.encodeWord 1
  decode = S.decodeWord >>= \case
    0 -> pure DocumentCBOR
    1 -> pure DocumentCanonicalJSON
    _ -> fail "unexpected decoding"
