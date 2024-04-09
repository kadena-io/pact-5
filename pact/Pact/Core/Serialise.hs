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
import qualified Pact.Core.Environment.Types as Core
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
data PactSerialise m b i
  = PactSerialise
  { _encodeModuleData :: ModuleData b i -> m ByteString
  , _decodeModuleData :: ByteString -> m (Maybe (Document (ModuleData b i)))
  , _encodeKeySet :: KeySet -> m ByteString
  , _decodeKeySet :: ByteString -> m (Maybe (Document KeySet))
  , _encodeDefPactExec :: Maybe DefPactExec -> m ByteString
  , _decodeDefPactExec :: ByteString -> m (Maybe (Document (Maybe DefPactExec)))
  , _encodeNamespace :: Namespace -> m ByteString
  , _decodeNamespace :: ByteString -> m (Maybe (Document Namespace))
  , _encodeRowData :: RowData -> m ByteString
  , _decodeRowData :: ByteString -> m (Maybe (Document RowData))
  }

serialisePact :: forall b i m. Core.MonadEval b i m => PactSerialise m CoreBuiltin ()
serialisePact = PactSerialise
  { _encodeModuleData = pure . docEncode V1.encodeModuleData
  , _decodeModuleData = \bs -> do
      case LegacyPact.decodeModuleData bs of
        Just result -> pure result
        Nothing -> docDecode bs >>= \case
          V1_CBOR -> V1.decodeModuleData bs

      -- (pure $ LegacyDocument <$> LegacyPact.decodeModuleData bs)
      -- <|> (pure $ docDecode bs (\case
      --                      V1_CBOR -> V1.decodeModuleData
      --                  ))

  , _encodeKeySet = pure . docEncode V1.encodeKeySet
  , _decodeKeySet = \bs -> undefined
      -- (pure $ LegacyDocument <$> LegacyPact.decodeKeySet bs)
      -- <|> (pure $ docDecode bs (\case
      --                      V1_CBOR -> V1.decodeKeySet
      --                  ))

  , _encodeDefPactExec = pure . docEncode V1.encodeDefPactExec
  , _decodeDefPactExec = \bs -> undefined
      -- (pure $ LegacyDocument <$> LegacyPact.decodeDefPactExec bs)
      -- <|> (pure $ docDecode bs (\case
      --                      V1_CBOR -> V1.decodeDefPactExec
      --                  ))

  , _encodeNamespace = pure . docEncode V1.encodeNamespace
  , _decodeNamespace = \bs -> undefined
      -- pure (fmap LegacyDocument $ LegacyPact.decodeNamespace bs)
      -- <|> (pure $ docDecode bs (\case
      --                      V1_CBOR -> V1.decodeNamespace
      --                  ))

  , _encodeRowData = pure . docEncode V1.encodeRowData
  , _decodeRowData = \bs -> undefined
      -- pure (fmap LegacyDocument $ LegacyPact.decodeRowData bs)
      -- <|> pure (docDecode bs (\case
      --                      V1_CBOR -> V1.decodeRowData
      --                  ))
  }
  where
    docEncode :: (a -> ByteString) -> a -> ByteString
    docEncode enc o = toStrictByteString (encodeVersion V1_CBOR <> S.encodeBytes (enc o))

    docDecode :: ByteString -> (DocumentVersion -> ByteString -> m (Maybe a)) -> m (Maybe (Document a))
    docDecode bs dec = do
      -- TODO: Charge some gas for decoding the envelope.
      case deserialiseFromBytes (liftA2 (,) decodeVersion S.decodeBytes) (fromStrict bs) of
        Left _ -> pure Nothing
        Right (_, (v,c)) -> fmap (Document v) <$> dec v c

serialisePact_repl_spaninfo :: forall b i m. Core.MonadEval b i m => PactSerialise m ReplCoreBuiltin SpanInfo
serialisePact_repl_spaninfo = serialisePact
  { _encodeModuleData = pure . V1.encodeModuleData_repl_spaninfo
  , _decodeModuleData = pure . fmap LegacyDocument . V1.decodeModuleData_repl_spaninfo
  }


serialisePact_raw_spaninfo :: forall b i m. Core.MonadEval b i m => PactSerialise m CoreBuiltin SpanInfo
serialisePact_raw_spaninfo = serialisePact
  { _encodeModuleData = pure . V1.encodeModuleData_raw_spaninfo
  , _decodeModuleData = pure . fmap LegacyDocument . V1.decodeModuleData_raw_spaninfo
  }
