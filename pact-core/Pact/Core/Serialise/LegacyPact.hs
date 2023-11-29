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
import qualified Data.ByteString.Short as SB
import qualified Data.Text.Encoding as T
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Pact.Core.ChainData
import Pact.Core.Hash
import Pact.Core.Parser
import Pact.Core.ModRefs
-- import Pact.Core.Guards
import Pact.Core.Literal 
import Data.Decimal
import Pact.Core.Info
import Pact.Time
import qualified Data.Attoparsec.Text as AP
import qualified Pact.JSON.Decode as JD
import qualified Data.Text as T
import Data.Vector(Vector)
import Data.Map.Strict(Map)
import Text.Read (readMaybe)

decodeModuleData :: ByteString -> Maybe (ModuleData RawBuiltin ())
decodeModuleData = JD.decodeStrict'

decodeModuleData_Repl_Info :: ByteString -> Maybe (ModuleData ReplRawBuiltin SpanInfo)
decodeModuleData_Repl_Info = JD.decodeStrict'

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
    "keys-2" -> pure Keys2
    "keys-any" -> pure KeysAny
    _ -> fail "unexpected parsing"

instance JD.FromJSON PublicKeyText where
  parseJSON = JD.withText "PublicKeyText" (pure . PublicKeyText)


instance JD.FromJSON Namespace where
  parseJSON = JD.withObject "Namespace" $ \v -> Namespace
    <$> v JD..: "name"
    <*> v JD..: "user"
    <*> v JD..: "admin"

instance JD.FromJSON (ModuleData RawBuiltin ()) where
  parseJSON = undefined

instance JD.FromJSON DefPactExec where
  parseJSON = JD.withObject "PactExec" $ \o ->
    DefPactExec
      <$> o JD..: "stepCount"
      <*> o JD..: "yield"
      <*> o JD..: "step"
      <*> o JD..: "pactId"
      <*> o JD..: "continuation"
      <*> o JD..: "stepHasRollback"
      <*> (fromMaybe mempty <$> o JD..:? "nested")

instance JD.FromJSONKey DefPactId where
  fromJSONKey = JD.FromJSONKeyTextParser $ JD.parseJSON . JD.String

instance JD.FromJSON Yield where
  parseJSON = JD.withObject "Yield" $ \o ->
    Yield
      <$> o JD..: "data"
      <*> o JD..: "provenance"
      <*> o JD..:? "source"

instance JD.FromJSON ChainId where
  parseJSON = JD.withText "ChainId" (pure . ChainId)

instance JD.FromJSON Provenance where
  parseJSON = JD.withObject "Provenance" $ \o ->
    Provenance
      <$> o JD..: "targetChainId"
      <*> o JD..: "moduleHash"

instance JD.FromJSON ModuleHash where
  parseJSON v = ModuleHash <$> JD.parseJSON v

instance JD.FromJSON Hash where
  parseJSON = JD.withText "Hash" $ \h ->
    case decodeBase64UrlUnpadded (T.encodeUtf8 h) of
      Left err -> fail ("Base64URL decode failed: " <> err)
      Right r -> pure (Hash (SB.toShort r))

instance JD.FromJSON DefPactId where
  parseJSON = JD.withText "DefPactId" (pure . DefPactId)

-- https://github.com/kadena-io/pact/blob/09f3b43fc10fbcdd798b01af45e4ddb6cecb91e7/src/Pact/Types/RowData.hs#L179C7-L181C24
-- We currently ignore the version field
instance JD.FromJSON RowData where
  parseJSON = JD.withObject "RowData" $ \o ->
    RowData <$> o JD..: "$d"

instance JD.FromJSON (DefPactContinuation FullyQualifiedName PactValue) where
  parseJSON = JD.withObject "DefPactContinuation" $ \o ->
    DefPactContinuation
      <$> o JD..: "def"
      <*> o JD..: "args"

instance JD.FromJSON FullyQualifiedName where
  parseJSON = JD.withText "FullyQualifiedName" $ \f ->
    case AP.parseOnly (fullyQualNameParser <* AP.endOfInput) f of
      Left s  -> fail s
      Right n -> return n

instance JD.FromJSON PactValue where
  parseJSON v = fromLegacyPactValue <$> JD.parseJSON v


instance JD.FromJSONKey Field where
  fromJSONKey = JD.FromJSONKeyTextParser $ JD.parseJSON . JD.String

instance JD.FromJSON ModuleName where
  parseJSON = JD.withObject "ModuleName" $ \o ->
    ModuleName
      <$> o JD..: "name"
      <*> o JD..:? "namespace"

instance JD.FromJSON ModRef where
  parseJSON = JD.withObject "ModRef" $ \o ->
    ModRef
      <$> o JD..: "refName"
      <*> o JD..: "refSpec"
      <*> pure Nothing

-- instance JD.FromJSON Literal where
-- --  parseJSON n@Number{} = LDecimal <$> decoder decimalCodec n
--   parseJSON (LString s) = pure $ LString s
--   parseJSON (LBool b) = pure $ LBool b
--   parseJSON (LInteger i) = undefined --decoder integerCodec i
--     (LDecimal <$> decoder decimalCodec o)
--   parseJSON _t = fail "Literal parse failed"

instance JD.FromJSON Field where
  parseJSON = JD.withText "Field" (pure . Field)


-- | LegacyLiteral and `LegacyPactValue` are used to represent the old
--   structure, used in legacy pact.

data LegacyLiteral
  = Legacy_LString T.Text
  | Legacy_LInteger Integer
  | Legacy_LDecimal Decimal
  | Legacy_LBool Bool
  | Legacy_LTime UTCTime

data LegacyPactValue
  = Legacy_PLiteral LegacyLiteral
  | Legacy_PList (Vector LegacyPactValue)
  | Legacy_PObject (Map Field LegacyPactValue)
  | Legacy_PGuard (Guard FullyQualifiedName LegacyPactValue)
  | Legacy_PModRef ModRef

instance JD.FromJSON LegacyPactValue where
  parseJSON v =
    (Legacy_PLiteral <$> JD.parseJSON v) <|>
    (Legacy_PList <$> JD.parseJSON v) <|>
    (Legacy_PGuard <$> JD.parseJSON v) <|>
    (Legacy_PModRef <$> (parseNoInfo v <|> JD.parseJSON v)) <|>
    (Legacy_PObject <$> JD.parseJSON v)
    where
      parseNoInfo = JD.withObject "ModRef" $ \o -> ModRef
        <$> o JD..: "refName"
        <*> o JD..: "refSpec"
        <*> pure Nothing

instance JD.FromJSON LegacyLiteral where
  parseJSON = \case
    n@JD.Number{} -> Legacy_LDecimal <$> decodeDecimal n
    JD.String s -> pure $ Legacy_LString s
    JD.Bool b -> pure $ Legacy_LBool b
    o@JD.Object {} ->
      (Legacy_LInteger <$> decodeInteger o) <|>
      (Legacy_LTime <$> decodeTime o) <|>
      (Legacy_LDecimal <$> decodeDecimal o)
    _t -> fail "Literal parse failed"
    where
      decodeInteger = JD.withObject "Integer" $ \o -> do
        s <- o JD..: "int"
        case s of
          JD.Number n -> return (round n)
          JD.String n -> case readMaybe (T.unpack n) of
            Just i -> return i
            Nothing -> fail $ "Invalid integer value: " ++ show s
          _ -> fail $ "Invalid integer value: " ++ show s

      decodeDecimal (JD.Number n) = return $ fromRational $ toRational n
      decodeDecimal (JD.Object o) = o JD..: "decimal" >>= \s -> case readMaybe (T.unpack s) of
        Just d -> return d
        Nothing -> fail $ "Invalid decimal value: " ++ show s
      decodeDecimal v = fail $ "Invalid decimal value: " ++ show v

      decodeTime = JD.withObject "time" $ \o ->
        (o JD..: "time" >>= mkTime pactISO8601Format) <|>
        (o JD..: "timep" >>= mkTime highPrecFormat)

      mkTime fmt v = case parseTime fmt v of
              Just t -> return t
              Nothing -> fail $ "Invalid time value, expected " ++ fmt

      pactISO8601Format :: String
      pactISO8601Format = "%Y-%m-%dT%H:%M:%SZ"

      highPrecFormat :: String
      highPrecFormat = "%Y-%m-%dT%H:%M:%S.%vZ"


instance JD.FromJSON (Guard FullyQualifiedName PactValue) where
  parseJSON = undefined -- fromLegacyPactValue <$> JD.parseJSON v


-- https://github.com/kadena-io/pact/blob/ba15517b56eba4fdaf6b2fbd3e5245eeedd0fc9f/src/Pact/Types/Term/Internal.hs#L802
instance JD.FromJSON (Guard FullyQualifiedName LegacyPactValue) where
  parseJSON v = GKeyset <$> JD.parseJSON v
    <|> GKeySetRef <$> parseRef v
    <|> GUserGuard <$> JD.parseJSON v
    <|> GCapabilityGuard <$> JD.parseJSON v
    <|> GModuleGuard <$> JD.parseJSON v
    where
    parseRef = JD.withObject "KeySetRef" $ \o -> do
      ref <- o JD..: "keysetref"
      pure (KeySetName ref)


instance JD.FromJSON (UserGuard FullyQualifiedName LegacyPactValue) where
  parseJSON = JD.withObject "UserGuard" $ \o ->
    UserGuard
      <$> o JD..: "fun"
      <*> o JD..: "args"

instance JD.FromJSON (CapabilityGuard FullyQualifiedName LegacyPactValue) where
  parseJSON = JD.withObject "CapabilityGuard" $ \o ->
    CapabilityGuard
      <$> o JD..: "cgName"
      <*> o JD..: "cgArgs"
      <*> o JD..:? "cgPactId"

instance JD.FromJSON ModuleGuard where
  parseJSON = JD.withObject "ModuleGuard" $ \o ->
    ModuleGuard
      <$> o JD..: "moduleName"
      <*> o JD..: "name"

fromLegacyPactValue :: LegacyPactValue -> PactValue
fromLegacyPactValue = \case
  Legacy_PLiteral ll -> case ll of
    Legacy_LString t -> PLiteral (LString t)
    Legacy_LInteger i -> PLiteral (LInteger i)
    Legacy_LDecimal d -> PLiteral (LDecimal d)
    Legacy_LBool b -> PLiteral (LBool b)
    Legacy_LTime utc -> PTime utc
  Legacy_PList v -> PList (fromLegacyPactValue <$> v)
  Legacy_PObject o -> PObject (fromLegacyPactValue <$> o)
  Legacy_PGuard _g -> undefined
  Legacy_PModRef mref -> PModRef mref
