{-# LANGUAGE RankNTypes #-}

-- | The canonical way of encoding and decoding Pact entities into bytestrings.
--   There are two places where in Pact where serialization is needed:
--     - Computing module hashes
--     - Reading and writing the Pact Database
--
--  Normal usage of this module involes the `serializeModuleForHash` function,
--  and `defaultSerializeForDatabase`.

module Pact.Core.Serialise where

import Data.ByteString (ByteString, fromStrict)

import Control.Applicative ((<|>))
import Pact.Core.Builtin
import Pact.Core.Persistence
import Pact.Core.Guards
import Pact.Core.Namespace
import Pact.Core.DefPacts.Types
import Control.Lens

import qualified Codec.CBOR.Encoding as S
import qualified Codec.CBOR.Decoding as S

import Codec.CBOR.Write (toStrictByteString)
import Codec.CBOR.Read (deserialiseFromBytes)

import qualified Pact.Core.Serialise.LegacyPact as LegacyPact
import qualified Pact.Core.Serialise.CBOR_V1 as V1
import Pact.Core.Gas (MilliGas)
import Pact.Core.Environment.Types (MonadEval)
import Pact.Core.Info (SpanInfo)

data DocumentVersion
  = V1_CBOR
  deriving (Show,Eq, Enum, Bounded)

data Document a
  = Document DocumentVersion a
  | LegacyDocument a
   deriving (Show, Eq)

document :: Lens' (Document a) a
document = lens getDoc setDoc
  where
    getDoc (Document _ d) = d
    getDoc (LegacyDocument d) = d
    setDoc (Document v _) = Document v
    setDoc (LegacyDocument _) = LegacyDocument


decodeVersion :: S.Decoder s DocumentVersion
decodeVersion = S.decodeWord >>= \case
  0 -> pure V1_CBOR
  _ -> fail "unexpected version decoding"

encodeVersion :: DocumentVersion -> S.Encoding
encodeVersion = \case
  V1_CBOR -> S.encodeWord 0


-- | The main serialization API for Pact entities.
data PactSerialise b i
  = PactSerialise
  { _encodeModuleData :: ModuleData b i -> ByteString
  , _decodeModuleData :: ByteString -> Maybe (Document (ModuleData b i))
  , _encodeKeySet :: KeySet -> ByteString
  , _decodeKeySet :: ByteString -> Maybe (Document KeySet)
  , _encodeDefPactExec :: Maybe DefPactExec -> ByteString
  , _decodeDefPactExec :: ByteString -> Maybe (Document (Maybe DefPactExec))
  , _encodeNamespace :: Namespace -> ByteString
  , _decodeNamespace :: ByteString -> Maybe (Document Namespace)
  , _encodeRowData :: forall m. Monad m => (MilliGas -> m ()) -> RowData -> m ByteString
  , _decodeRowData :: ByteString -> Maybe (Document RowData)
  }

serialisePact :: PactSerialise CoreBuiltin ()
serialisePact = PactSerialise
  { _encodeModuleData = docEncode V1.encodeModuleData
  , _decodeModuleData = \bs ->
      LegacyDocument <$> LegacyPact.decodeModuleData bs
      <|> docDecode bs (\case
                           V1_CBOR -> V1.decodeModuleData
                       )

  , _encodeKeySet = docEncode V1.encodeKeySet
  , _decodeKeySet = \bs ->
      LegacyDocument <$> LegacyPact.decodeKeySet bs
      <|> docDecode bs (\case
                           V1_CBOR -> V1.decodeKeySet
                       )

  , _encodeDefPactExec = docEncode V1.encodeDefPactExec
  , _decodeDefPactExec = \bs ->
      LegacyDocument <$> LegacyPact.decodeDefPactExec bs
      <|> docDecode bs (\case
                           V1_CBOR -> V1.decodeDefPactExec
                       )

  , _encodeNamespace = docEncode V1.encodeNamespace
  , _decodeNamespace = \bs ->
      LegacyDocument <$> LegacyPact.decodeNamespace bs
      <|> docDecode bs (\case
                           V1_CBOR -> V1.decodeNamespace
                       )

  , _encodeRowData = \_chargeGas rd -> pure $ docEncode V1.encodeRowData rd
  , _decodeRowData = \bs ->
      LegacyDocument <$> LegacyPact.decodeRowData bs
      <|> docDecode bs (\case
                           V1_CBOR -> V1.decodeRowData
                       )
  }
  where
    docEncode :: (a -> ByteString) -> a -> ByteString
    docEncode enc o = toStrictByteString (encodeVersion V1_CBOR <> S.encodeBytes (enc o))

    docDecode :: ByteString -> (DocumentVersion -> ByteString -> Maybe a) -> Maybe (Document a)
    docDecode bs dec = case deserialiseFromBytes (liftA2 (,) decodeVersion S.decodeBytes) (fromStrict bs) of
      Left _ -> Nothing
      Right (_, (v,c)) ->  Document v <$> dec v c

serialisePact_repl_spaninfo :: PactSerialise ReplCoreBuiltin SpanInfo
serialisePact_repl_spaninfo = serialisePact
  { _encodeModuleData = V1.encodeModuleData_repl_spaninfo
  , _decodeModuleData = fmap LegacyDocument . V1.decodeModuleData_repl_spaninfo
  }

serialisePact_raw_spaninfo :: PactSerialise CoreBuiltin SpanInfo
serialisePact_raw_spaninfo = serialisePact
  { _encodeModuleData = V1.encodeModuleData_raw_spaninfo
  , _decodeModuleData = fmap LegacyDocument . V1.decodeModuleData_raw_spaninfo
  }
