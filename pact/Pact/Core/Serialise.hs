{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}

-- | The canonical way of encoding and decoding Pact entities into bytestrings.
--   There are two places where in Pact where serialization is needed:
--     - Computing module hashes
--     - Reading and writing the Pact Database
--
--  Normal usage of this module involes the `serializeModuleForHash` function,
--  and `defaultSerializeForDatabase`.

module Pact.Core.Serialise
  ( DocumentVersion(..)
  , Document(..)
  , PactSerialise(..)
  , document
  , serialisePact
  , serialisePact_raw_spaninfo
  , serialisePact_repl_spaninfo
  , decodeVersion
  , encodeVersion
  , liftReplBuiltin
  , ) where

import Data.ByteString (ByteString, fromStrict)

import Control.Applicative ((<|>))
import Pact.Core.Builtin
import Pact.Core.Persistence
import Pact.Core.Guards
import Pact.Core.Namespace
import Pact.Core.DefPacts.Types
import Pact.Core.IR.Term
import Control.Lens

import qualified Codec.CBOR.Encoding as S
import qualified Codec.CBOR.Decoding as S

import Codec.CBOR.Write (toStrictByteString)
import Codec.CBOR.Read (deserialiseFromBytes)

import qualified Pact.Core.Serialise.LegacyPact as LegacyPact
import qualified Pact.Core.Serialise.CBOR_V1 as V1
import qualified Pact.Core.Legacy.LegacyPactValue as LegacyPact
import Pact.Core.Info (SpanInfo)
import Data.Default

data DocumentVersion
  = V1_CBOR
  deriving (Show,Eq, Enum, Bounded)

data Document a
  = Document DocumentVersion a
  | LegacyDocument a
   deriving (Show, Eq, Functor)

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
  , _encodeRowData :: RowData -> GasM b i ByteString
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

  , _encodeRowData = gEncodeRowData
  , _decodeRowData = \bs ->
      LegacyDocument <$> LegacyPact.decodeLegacy bs
      <|> docDecode bs (\case
                           V1_CBOR -> V1.decodeRowData
                       )
  }

gEncodeRowData :: RowData -> GasM b i ByteString
gEncodeRowData rd = do
  let encodedRow = V1.encodeRowDataNoGas rd
  pure $ toStrictByteString $ encodeVersion V1_CBOR <> S.encodeBytes encodedRow

liftReplBuiltin :: ModuleData CoreBuiltin a -> ModuleData ReplCoreBuiltin a
liftReplBuiltin = \case
  ModuleData em ed -> let
    defs' = over (traverseDefTerm . termBuiltin) RBuiltinWrap <$> _mDefs em
    ed' = over (traverseDefTerm . termBuiltin) RBuiltinWrap <$> ed
    in ModuleData (em{_mDefs = defs'}) ed'
  InterfaceData im ed -> let
    ifdefs = over (traverseIfDefTerm . termBuiltin) RBuiltinWrap <$> _ifDefns im
    ed' = over (traverseDefTerm . termBuiltin) RBuiltinWrap <$> ed
    in InterfaceData (im{_ifDefns = ifdefs}) ed'


serialisePact_repl_spaninfo :: PactSerialise ReplCoreBuiltin SpanInfo
serialisePact_repl_spaninfo = serialisePact
  { _encodeModuleData = docEncode V1.encodeModuleData_repl_spaninfo
  , _decodeModuleData =
      \bs ->
        (LegacyDocument . fmap def . liftReplBuiltin <$> LegacyPact.decodeModuleData bs)
        <|> docDecode bs (\case
                            V1_CBOR -> V1.decodeModuleData_repl_spaninfo
                        )
  , _encodeRowData = gEncodeRowData
  }

docEncode :: (a -> ByteString) -> a -> ByteString
docEncode enc o = toStrictByteString (encodeVersion V1_CBOR <> S.encodeBytes (enc o))
{-# INLINE docEncode #-}

docDecode :: ByteString -> (DocumentVersion -> ByteString -> Maybe a) -> Maybe (Document a)
docDecode bs dec = case deserialiseFromBytes (liftA2 (,) decodeVersion S.decodeBytes) (fromStrict bs) of
  Right (_, (v,c)) ->  Document v <$> dec v c
  Left _ -> Nothing
{-# INLINE docDecode #-}

serialisePact_raw_spaninfo :: PactSerialise CoreBuiltin SpanInfo
serialisePact_raw_spaninfo = serialisePact
  { _encodeModuleData = docEncode V1.encodeModuleData_raw_spaninfo
  , _decodeModuleData =
      \bs ->
        (LegacyDocument . fmap def <$> LegacyPact.decodeModuleData bs)
        <|> docDecode bs (\case
                            V1_CBOR -> V1.decodeModuleData_raw_spaninfo
                        )
  , _encodeRowData = gEncodeRowData
  }
