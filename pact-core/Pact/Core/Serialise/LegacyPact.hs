-- | 
{-# OPTIONS_GHC -fno-warn-orphans #-}
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
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)

import qualified Pact.JSON.Decode as JD

decodeModuleData :: ByteString -> Maybe (ModuleData RawBuiltin ())
decodeModuleData = JD.decodeStrict'

decodeKeySet :: ByteString -> Maybe (KeySet FullyQualifiedName)
decodeKeySet = JD.decodeStrict'

decodeDefPactExec :: ByteString -> Maybe (Maybe DefPactExec)
decodeDefPactExec = JD.decodeStrict'

decodeNamespace :: ByteString -> Maybe Namespace
decodeNamespace = JD.decodeStrict'

decodeRowData :: ByteString -> Maybe RowData
decodeRowData = JD.decodeStrict'

instance JD.FromJSON NamespaceName where
  parseJSON = JD.withText "NamespaceName" (pure . NamespaceName)


instance JD.FromJSON (KeySet FullyQualifiedName) where
  parseJSON v = JD.withObject "KeySet" keyListPred v <|> keyListOnly
      where
        defPred = KeysAll

        keyListPred o = KeySet
          <$> o JD..: "keys"
          <*> (fromMaybe defPred <$> o JD..:? "pred")

        keyListOnly = KeySet <$> JD.parseJSON v <*> pure defPred

instance JD.FromJSON (KSPredicate FullyQualifiedName) where
  parseJSON = JD.withText "KSPredicate" $ \case
    "keys-all" -> pure KeysAll
    "keys2" -> pure Keys2
    "KeysAny" -> pure KeysAny
    _ -> fail "unexpected parsing"

instance JD.FromJSON PublicKeyText where
  parseJSON = JD.withText "PublicKeyText" (pure . PublicKeyText)


instance JD.FromJSON Namespace where
  parseJSON = JD.withObject "Namespace" $ \v -> Namespace
    <$> v JD..: "name"
    <*> v JD..: "user"
    <*> v JD..: "admin"

instance JD.FromJSON (Guard FullyQualifiedName PactValue) where
  parseJSON = undefined

instance JD.FromJSON (ModuleData RawBuiltin ()) where
  parseJSON = undefined

instance JD.FromJSON DefPactExec where
  parseJSON = undefined

instance JD.FromJSON RowData where
  parseJSON = undefined
