-- | 

module Pact.Core.Serialise.LegacyPact
  ( encodeModuleData, decodeModuleData
  , encodeKeySet, decodeKeySet
  , encodeDefPactExec, decodeDefPactExec
  , encodeNamespace, decodeNamespace
  , encodeRowData, decodeRowData
  ) where

import Pact.Core.Names
import Pact.Core.Persistence
import Pact.Core.Guards
import Pact.Core.Builtin
import Pact.Core.DefPacts.Types
import Pact.Core.Namespace
import Pact.Core.PactValue
import Data.ByteString (ByteString)

import qualified Pact.JSON.Encode as JE
import qualified Pact.JSON.Decode as JD

encodeModuleData :: ModuleData RawBuiltin () -> ByteString
encodeModuleData = undefined

decodeModuleData :: ByteString -> Maybe (ModuleData RawBuiltin ())
decodeModuleData = undefined


encodeKeySet :: KeySet FullyQualifiedName -> ByteString
encodeKeySet = JE.encodeStrict

decodeKeySet :: ByteString -> Maybe (KeySet FullyQualifiedName)
decodeKeySet = undefined


encodeDefPactExec :: Maybe DefPactExec -> ByteString
encodeDefPactExec =  undefined

decodeDefPactExec :: ByteString -> Maybe (Maybe DefPactExec)
decodeDefPactExec = undefined

encodeNamespace :: Namespace -> ByteString
encodeNamespace = JE.encodeStrict

decodeNamespace :: ByteString -> Maybe Namespace
decodeNamespace = undefined

encodeRowData :: RowData -> ByteString
encodeRowData = undefined

decodeRowData :: ByteString -> Maybe RowData
decodeRowData = undefined



instance JE.Encode (KeySet FullyQualifiedName) where
  build (KeySet ks pred) = JE.object
--    [ "pred" JE..= undefined
    [
     "keys" JE..= JE.Array ks
    ]

instance JE.Encode PublicKeyText where
  build (PublicKeyText k) = JE.build k

instance JE.Encode Namespace where
  build (Namespace n user admin) = JE.object
    [ "admin" JE..= admin
    , "user" JE..= user
    , "name" JE..= n
    ]

instance JE.Encode NamespaceName where
  build = undefined

instance JE.Encode (Guard FullyQualifiedName PactValue) where
  build = undefined
