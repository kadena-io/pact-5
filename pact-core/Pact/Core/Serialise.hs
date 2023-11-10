-- | The canonical way of encoding and decoding Pact entities into bytestrings.
--   There are two places where in Pact where serialization is needed:
--     - Computing module hashes
--     - Reading and writing the Pact Database
--
--  Normal usage of this module involes the `serializeModuleForHash` function,
--  and `defaultSerializeForDatabase`.

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Pact.Core.Serialise where


import Data.ByteString (ByteString, fromStrict)
import Data.Word (Word32)

-- import Pact.Core.Info
import Pact.Core.Builtin
--import Pact.Core.IR.Term
import Pact.Core.Persistence
import Pact.Core.Guards
import Pact.Core.Names
import Pact.Core.Namespace
import Pact.Core.DefPacts.Types

import qualified Codec.Serialise as S
import qualified Codec.CBOR.Encoding as S
import qualified Codec.CBOR.Decoding as S

--import Codec.CBOR.Write (toStrictByteString)
import Codec.CBOR.Read (deserialiseFromBytes, DeserialiseFailure)

import Pact.Core.Serialise.CBOR_V1 ()
-- import Data.Bifunctor

import Data.Int (Int64)


data SerialVersion
  = V0
  | V1


encodeVersion :: SerialVersion -> S.Encoding
encodeVersion = \case
  V0 -> S.encodeWord 0
  V1 -> S.encodeWord 1

decodeVersion :: S.Decoder s SerialVersion
decodeVersion = S.decodeWord >>= \case
  0 -> pure V0
  1 -> pure V1
  _ -> fail "unexpected decoding"



pactEncodeKeySet :: KeySet FullyQualifiedName -> ByteString
pactEncodeKeySet = undefined

pactDecodeKeySet
  :: ByteString
  -> Either DeserialiseFailure (SerialVersion, KeySet FullyQualifiedName)
pactDecodeKeySet bs = snd <$> deserialiseFromBytes dec (fromStrict bs)
  where
    dec = decodeVersion >>= \case
      V0 -> (V0,) <$> S.decode
      V1 -> (V1,) <$> S.decode






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

encodeDocumentVersion :: DocumentVersion -> S.Encoding
encodeDocumentVersion (DocumentVersion v) = S.encodeWord32 v

decodeDocumentVersion :: S.Decoder s DocumentVersion
decodeDocumentVersion = DocumentVersion <$> S.decodeWord32

-- | Supported Document Formats
data DocumentFormat
  = DocumentCBOR
  | DocumentCanonicalJSON
  -- ^ A JSON encoding with all forms of nondeterminism removed:
  --     Lexographic keys, stripped whitespace.
  deriving (Show, Eq, Enum, Bounded)

encodeDocumentFormat :: DocumentFormat -> S.Encoding
encodeDocumentFormat = \case
  DocumentCBOR -> S.encodeWord8 0
  DocumentCanonicalJSON -> S.encodeWord8 1

decodeDocumentFormat :: S.Decoder s DocumentFormat
decodeDocumentFormat = S.decodeWord8 >>= \case
  0 -> pure DocumentCBOR
  1 -> pure DocumentCanonicalJSON
  _ -> fail "unexpected decoding"


encodeDocument :: (c -> S.Encoding) -> Document c -> S.Encoding
encodeDocument encode (Document v f c) = mconcat
  [ encodeDocumentVersion v
  , encodeDocumentFormat f
  , encode c
  ]

decodeDocument :: S.Decoder ByteString s -> S.Decoder ByteString (Document s)
decodeDocument decode = do
  v <- decodeDocumentVersion
  f <- decodeDocumentFormat
  Document v f <$> decode


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
  , _encodeDefPactExec :: Maybe DefPactExec -> ByteString
  , _decodeDefPactExec :: ByteString -> Either DecodeError (Document (Maybe DefPactExec))
  , _encodeNamespace :: Namespace -> ByteString
  , _decodeNamespace :: ByteString -> Either DecodeError (Document Namespace)
  , _encodeRowData :: RowData -> ByteString
  , _decodeRowData :: ByteString -> Either DecodeError (Document RowData)
  }

serialiseCBOR :: PactSerialise RawBuiltin ()
serialiseCBOR = undefined

-- serialiseCBOR :: (S.Serialise b, S.Serialise i) => PactSerialise b i
-- serialiseCBOR = PactSerialise
--   { _encodeModuleData = toStrictByteString . S.encode . Document version format
--   , _decodeModuleData = first toErr . S.deserialiseOrFail . fromStrict
--   , _encodeKeySet = toStrictByteString . S.encode . Document version format
--   , _decodeKeySet = first toErr . S.deserialiseOrFail . fromStrict
--   , _encodeDefPactExec = toStrictByteString . S.encode . Document version format
--   , _decodeDefPactExec = first toErr . S.deserialiseOrFail . fromStrict
--   }
--   where
--     version = DocumentVersion 0
--     format = DocumentCBOR
--     toErr (S.DeserialiseFailur
--           e offset msg) = DecodeFailure offset msg

-- instance S.Serialise (V1Serial a) => S.Serialise (Document a) where
--   encode (Document v f c) = S.encode v <> S.encode f <> S.encode c
--   decode = do
--     _documentVersion <- S.decode
--     _documentFormat <- S.decode
--     _documentContent <- case (_documentVersion, _documentFormat) of
--       (DocumentVersion 1, DocumentCBOR) -> unV1Serial <$> S.decode @(V1Serial a)
--     pure $ Document {_documentVersion, _documentFormat, _documentContent }



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
