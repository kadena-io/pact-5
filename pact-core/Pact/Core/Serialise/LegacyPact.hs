-- | 

module Pact.Core.Serialise.LegacyPact
  ( decodeModuleData
  , decodeKeySet
  , decodeDefPactExec
  , decodeNamespace
  , decodeRowData
  ) where

import Pact.Core.Names
import Pact.Core.Persistence
import Pact.Core.Guards
import Pact.Core.Builtin
import Pact.Core.DefPacts.Types
import Pact.Core.Namespace
import Pact.Core.PactValue
import Data.ByteString (ByteString)

import qualified Pact.JSON.Decode as JD

decodeModuleData :: ByteString -> Maybe (ModuleData RawBuiltin ())
decodeModuleData = undefined


decodeKeySet :: ByteString -> Maybe (KeySet FullyQualifiedName)
decodeKeySet = undefined

decodeDefPactExec :: ByteString -> Maybe (Maybe DefPactExec)
decodeDefPactExec = undefined

decodeNamespace :: ByteString -> Maybe Namespace
decodeNamespace = undefined

decodeRowData :: ByteString -> Maybe RowData
decodeRowData = undefined
