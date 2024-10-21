{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

-- |
--
-- Stable encoding which matches Pacts StableEncoding.
--

module Pact.Core.StableEncoding
  ( encodeStable
  , decodeStable
  , eitherDecodeStable
  , roundtripStable
  , StableEncoding(..))
where

import Control.Applicative
import Data.Bifunctor
import Data.Text(Text)
import Data.ByteString (ByteString)
import Data.Coerce(coerce)
import Data.Decimal (DecimalRaw(..))
import Data.Scientific (Scientific)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Pact.JSON.Decode as JD
import qualified Pact.JSON.Encode as J
import Unsafe.Coerce (unsafeCoerce)

import Pact.Core.Capabilities
import Pact.Core.ChainData
import Pact.Core.Gas.Types
import Pact.Core.Legacy.LegacyCodec
import Pact.Core.Info
import Pact.Core.Literal
import Pact.Core.Guards
import Pact.Core.Names
import Pact.Core.ModRefs
import Pact.Core.Persistence.Types
import Pact.Core.Hash
import Pact.Core.DefPacts.Types
import Pact.Core.PactValue
import Pact.Time
import Data.Maybe (fromMaybe)
import Pact.Core.Namespace

-- | JSON serialization for 'readInteger' and public meta info;
-- accepts both a String version (parsed as a Pact integer),
-- a Number, or a PactValue { "int": ... } integer
newtype ParsedInteger = ParsedInteger Integer
  deriving (Eq,Show,Ord)

encodeStable :: J.Encode (StableEncoding a) => a -> ByteString
encodeStable = J.encodeStrict . StableEncoding
{-# INLINE encodeStable #-}

decodeStable :: JD.FromJSON (StableEncoding a) => ByteString -> Maybe a
decodeStable = fmap _stableEncoding . JD.decodeStrict'
{-# INLINE decodeStable #-}

eitherDecodeStable :: JD.FromJSON (StableEncoding a) => ByteString -> Either String a
eitherDecodeStable = fmap _stableEncoding . JD.eitherDecodeStrict'
{-# INLINE eitherDecodeStable #-}

roundtripStable :: (JD.FromJSON (StableEncoding a), J.Encode (StableEncoding a)) => a -> Maybe a
roundtripStable a = _stableEncoding <$> JD.decodeStrict (J.encodeStrict (StableEncoding a))

instance J.Encode ParsedInteger where
  build (ParsedInteger i) = J.build $ J.Aeson i
  {-# INLINE build #-}

instance JD.FromJSON ParsedInteger where
  parseJSON (JD.String s) =
    ParsedInteger <$> case parseNumLiteral s of
                        Just (LInteger i) -> return i
                        _ -> fail $ "Failure parsing integer string: " ++ show s
  parseJSON (JD.Number n) = return $ ParsedInteger (round n)
  parseJSON v@JD.Object{} = JD.parseJSON v >>= \i -> case i of
    StableEncoding (PLiteral (LInteger li)) -> return $ ParsedInteger li
    StableEncoding pv -> fail $ "Failure parsing integer PactValue object: " ++ show pv
  parseJSON v = fail $ "Failure parsing integer: " ++ show v

newtype StableEncoding a = StableEncoding { _stableEncoding :: a }
  deriving (Ord, Eq, Show)

instance J.Encode (StableEncoding ()) where
  build (StableEncoding _) = J.null

instance JD.FromJSON (StableEncoding ()) where
  parseJSON = fmap StableEncoding . JD.parseJSON

instance J.Encode (StableEncoding DefPactId) where
  build (StableEncoding (DefPactId pid)) =
    J.build pid

-- | Stable encoding of `Literal`
--
-- `isSafeInteger` checks for the Javascript maximum/minimum numbers.
-- Details can be found here: https://github.com/kadena-io/pact/blob/e72d86749f5d65ac8d6e07a7652dd2ffb468607b/src/Pact/Types/Codec.hs#L44
instance J.Encode (StableEncoding Literal) where
  build (StableEncoding lit) = case lit of
    LString t -> J.build t
    LInteger i -> encodeInteger i
    LDecimal d -> encodeDecimal d
    LUnit -> J.null
    LBool b -> J.build b
    where
      encodeInteger i
        | isSafeInteger i = J.object [ "int" J..= J.Aeson i ]
        | otherwise = J.object [ "int" J..= T.pack (show i) ]
      encodeDecimal d@(Decimal _ mantissa)
        | isSafeInteger mantissa = J.build $ J.Aeson @Scientific $ fromRational $ toRational d
        | otherwise = J.object [ "decimal" J..= T.pack (show d) ]
      isSafeInteger i = i >= -9007199254740991 && i <= 9007199254740991
  {-# INLINE build #-}

-- | Stable encoding of `Guard FullyQualifiedName PactValue`
instance (J.Encode (StableEncoding name), J.Encode (StableEncoding v))
  => J.Encode (StableEncoding (Guard name v)) where
  build (StableEncoding g) = case g of
    GKeyset ks -> J.build (StableEncoding ks)
    GKeySetRef ksn -> J.object ["keysetref" J..= StableEncoding ksn]
    GUserGuard ug -> J.build (StableEncoding ug)
    GCapabilityGuard cg -> J.build (StableEncoding cg)
    GModuleGuard mg -> J.build (StableEncoding mg)
    GDefPactGuard dpg -> J.build (StableEncoding dpg)
  {-# INLINE build #-}

instance (JD.FromJSON (StableEncoding name), JD.FromJSON (StableEncoding v))
  => JD.FromJSON (StableEncoding (Guard name v)) where
  parseJSON v =
    fmap StableEncoding (
    (GKeyset . _stableEncoding) <$> JD.parseJSON v <|>
    (GKeySetRef . _stableEncoding) <$> ksr v <|>
    (GUserGuard . _stableEncoding) <$> JD.parseJSON v <|>
    (GCapabilityGuard . _stableEncoding) <$> JD.parseJSON v <|>
    (GModuleGuard . _stableEncoding) <$> JD.parseJSON v <|>
    (GDefPactGuard . _stableEncoding) <$> JD.parseJSON v)
    where
    ksr = JD.withObject "KeySetRef" $ \o -> o JD..: "keysetref"

instance JD.FromJSON (StableEncoding KeySet) where
  parseJSON v = objKs v <|> keyListOnly v
    where
    keyListOnly v' = fmap StableEncoding $
      KeySet
        <$> fmap (S.mapMonotonic _stableEncoding) (JD.parseJSON v')
        <*> pure KeysAll
    objKs =
      JD.withObject "KeySet" $ \o -> do
        keys <- o JD..: "keys"
        pred' <- fromMaybe (StableEncoding KeysAll) <$> o JD..:? "pred"
        pure $ StableEncoding (KeySet (S.fromList (fmap PublicKeyText keys)) (_stableEncoding pred'))

instance JD.FromJSON (StableEncoding KeySetName) where
  parseJSON v = oldKs v <|> newKs v
    where
    oldKs = JD.withText "KeySetName" (pure . StableEncoding . (`KeySetName` Nothing))
    newKs =
      JD.withObject "KeySetName" $ \o -> do
        ns <- o JD..:? "ns"
        ksn <- o JD..: "ksn"
        pure $ StableEncoding (KeySetName ksn (NamespaceName <$> ns))


-- | Stable encoding of `CapabilityGuard FullyQualifiedName PactValue`
instance (J.Encode (StableEncoding name), J.Encode (StableEncoding v))
  => J.Encode (StableEncoding (CapabilityGuard name v)) where
  build (StableEncoding (CapabilityGuard name args mpid)) = J.object
    [ "cgPactId" J..= fmap StableEncoding mpid
    , "cgArgs" J..= J.Array (StableEncoding <$> args)
    , "cgName" J..= StableEncoding name
    ]
  {-# INLINABLE build #-}

instance (JD.FromJSON (StableEncoding name), JD.FromJSON (StableEncoding v))
  => JD.FromJSON (StableEncoding (CapabilityGuard name v)) where
  parseJSON = JD.withObject "CapabilityGuard" $ \o -> do
    name <- o JD..: "cgName"
    args <- o JD..: "cgArgs"
    mpid <- o JD..:? "cgPactId"
    pure $ StableEncoding
        (CapabilityGuard (_stableEncoding name) (fmap _stableEncoding args) (fmap _stableEncoding mpid))
  {-# INLINE parseJSON #-}

instance J.Encode (StableEncoding QualifiedName) where
  build (StableEncoding qn) = J.build (renderQualName qn)
  {-# INLINABLE build #-}

instance JD.FromJSON (StableEncoding QualifiedName) where
  parseJSON = JD.withText "QualifiedName" $ \t -> case parseQualifiedName t of
    Just qn -> pure (StableEncoding qn)
    _ -> fail "could not parse qualified name"

-- | Stable encoding of `FullyQualifiedName`
instance J.Encode (StableEncoding FullyQualifiedName) where
  build (StableEncoding (FullyQualifiedName (ModuleName mn mns) n (ModuleHash mh))) = J.build t
    where
      t = maybe "" ((<> ".") . _namespaceName) mns <> mn <> "." <> n <> ".{" <> hashToText mh <> "}"
  {-# INLINABLE build #-}

instance JD.FromJSON (StableEncoding FullyQualifiedName) where
  parseJSON = JD.withText "FullyQualifiedName" $ \t ->
    case parseFullyQualifiedName t of
      Just fqn -> pure (StableEncoding fqn)
      Nothing -> fail "could not parse FullyQualifiedName"

-- | Stable encoding of `ModuleGuard`
instance J.Encode (StableEncoding ModuleGuard) where
  build (StableEncoding (ModuleGuard m name)) = J.object
    [ "moduleName" J..= StableEncoding m
    , "name" J..= name
    ]
  {-# INLINABLE build #-}

instance JD.FromJSON (StableEncoding ModuleGuard) where
  parseJSON = JD.withObject "ModuleGuard" $ \o -> do
    m <- o JD..: "moduleName"
    name <- o JD..: "name"
    pure $ StableEncoding (ModuleGuard (_stableEncoding m) name)

-- | Stalbe encoding of `DefPactGuard`
instance J.Encode (StableEncoding DefPactGuard) where
  build (StableEncoding (DefPactGuard dpid name)) = J.object
    [ "pactId" J..= StableEncoding dpid
    , "name" J..= name
    ]
  {-# INLINABLE build #-}

instance JD.FromJSON (StableEncoding DefPactGuard) where
  parseJSON = JD.withObject "DefPactGuard" $ \o -> do
    dpid <- o JD..: "pactId"
    name <- o JD..: "name"
    pure $ StableEncoding (DefPactGuard (_stableEncoding dpid) name)

instance J.Encode (StableEncoding NestedDefPactExec) where
  build (StableEncoding (NestedDefPactExec sc yield step defPactId continuation nestedDefPactExec)) = J.object
    [ "nested" J..?= if M.null nestedDefPactExec then Nothing else Just (J.Object (convertMap nestedDefPactExec))
    , "executed" J..= (Nothing :: Maybe Bool) -- compat field for prod
    , "pactId" J..= StableEncoding defPactId
    , "step" J..= J.Aeson step
    , "yield" J..= fmap StableEncoding yield
    , "continuation" J..= StableEncoding continuation
    , "stepCount" J..= J.Aeson sc
    ]
    where convertMap :: Map DefPactId NestedDefPactExec -> Map T.Text (StableEncoding NestedDefPactExec)
          convertMap = unsafeCoerce

instance JD.FromJSON (StableEncoding NestedDefPactExec) where
  parseJSON = JD.withObject "NestedDefPactExec" $ \o -> do
    stepCount <- o JD..: "stepCount"
    (_ :: Maybe Bool) <- o JD..: "executed"
    yield <- o JD..:? "yield"
    step <- o JD..: "step"
    defPactId <- o JD..: "pactId"
    continuation <- o JD..: "continuation"
    nestedDefPactExec <- fromMaybe mempty <$> (o JD..:? "nested")
    pure $ StableEncoding
      (NestedDefPactExec
        stepCount
        (fmap _stableEncoding yield)
        step
        (_stableEncoding defPactId)
        (_stableEncoding continuation)
        (convertKeys nestedDefPactExec))
      where
        convertKeys :: Map T.Text (StableEncoding NestedDefPactExec) -> Map DefPactId NestedDefPactExec
        convertKeys = Map.fromList . fmap (bimap DefPactId _stableEncoding) . Map.toList

instance J.Encode (StableEncoding DefPactExec) where
  build (StableEncoding (DefPactExec sc yield step defPactId continuation stepHasRollback nestedDefPactExec)) = J.object
    [ "nested" J..?= if M.null nestedDefPactExec then Nothing else Just (J.Object (convertMap nestedDefPactExec))
    , "executed" J..= (Nothing :: Maybe Bool) -- compat field for prod
    , "pactId" J..= StableEncoding defPactId
    , "stepHasRollback" J..= stepHasRollback
    , "step" J..= J.Aeson step
    , "yield" J..= fmap StableEncoding yield
    , "continuation" J..= StableEncoding continuation
    , "stepCount" J..= J.Aeson sc
    ]
    where convertMap :: Map DefPactId NestedDefPactExec -> Map T.Text (StableEncoding NestedDefPactExec)
          convertMap = unsafeCoerce

instance JD.FromJSON (StableEncoding DefPactExec) where
  parseJSON = JD.withObject "DefPactExec" $ \o -> do
    stepCount <- o JD..: "stepCount"
    (_ :: Maybe Bool) <- o JD..: "executed"
    yield <- o JD..:? "yield"
    step <- o JD..: "step"
    defPactId <- o JD..: "pactId"
    continuation <- o JD..: "continuation"
    stepHasRollback <- o JD..: "stepHasRollback"
    nestedDefPactExec <- fromMaybe mempty <$> (o JD..:? "nested")
    pure $ StableEncoding
      (DefPactExec
        stepCount
        (fmap _stableEncoding yield)
        step
        (_stableEncoding defPactId)
        (_stableEncoding continuation)
        stepHasRollback
        (convertKeys nestedDefPactExec))
      where
        convertKeys :: Map T.Text (StableEncoding NestedDefPactExec) -> Map DefPactId NestedDefPactExec
        convertKeys = Map.fromList . fmap (bimap DefPactId _stableEncoding) . Map.toList

instance JD.FromJSON (StableEncoding (DefPactContinuation QualifiedName PactValue)) where
  parseJSON = JD.withObject "DefPactContinuation" $ \o -> do
    name <- o JD..: "def"
    args <- o JD..: "args"
    pure $ StableEncoding (DefPactContinuation (_stableEncoding name) (_stableEncoding <$> args))


instance JD.FromJSON (StableEncoding DefPactId) where
  parseJSON = JD.withText "DefPactId" $ \t -> pure $ StableEncoding (DefPactId t)

instance J.Encode (StableEncoding Yield) where
  build (StableEncoding (Yield data' provenance sourceChain)) = J.object
    [ "data" J..= (StableEncoding data')
    , "provenance" J..= fmap StableEncoding provenance
    , "source" J..?= fmap StableEncoding sourceChain
    ]
  {-# INLINABLE build #-}

instance JD.FromJSON (StableEncoding Yield) where
  parseJSON = JD.withObject "Yield" $ \o -> do
    data' <- fmap _stableEncoding <$> o JD..: "data"
    provenance <- o JD..: "provenance"
    sourceChain <- o JD..:? "source"
    pure $ StableEncoding (Yield data' (fmap _stableEncoding provenance) (_stableEncoding <$> sourceChain))

instance J.Encode (StableEncoding Provenance) where
  build (StableEncoding (Provenance chainId moduleHash)) = J.object
    [ "targetChainId" J..= StableEncoding chainId
    , "moduleHash" J..= StableEncoding moduleHash
    ]
  {-# INLINABLE build #-}

instance JD.FromJSON (StableEncoding Provenance) where
  parseJSON = JD.withObject "Provenance" $ \o -> do
    targetChainId <- o JD..: "targetChainId"
    moduleHash <- o JD..: "moduleHash"
    pure $ StableEncoding (Provenance (_stableEncoding targetChainId) (_stableEncoding moduleHash))

instance J.Encode (StableEncoding ChainId) where
  build (StableEncoding (ChainId cid)) = J.build cid
  {-# INLINABLE build #-}

instance JD.FromJSON (StableEncoding ChainId) where
  parseJSON = JD.withText "ChainId" $ \t -> pure $ StableEncoding (ChainId t)

-- | Stable encoding of `UserGuard FullyQualifiedName PactValue`
instance (J.Encode (StableEncoding name), J.Encode (StableEncoding v))
  => J.Encode (StableEncoding (UserGuard name v)) where
  build (StableEncoding (UserGuard fun args)) = J.object
    [ "args" J..= J.array (StableEncoding <$> args)
    , "fun" J..= StableEncoding fun
    ]
  {-# INLINABLE build #-}

instance (JD.FromJSON (StableEncoding name), JD.FromJSON (StableEncoding v))
  => JD.FromJSON (StableEncoding (UserGuard name v)) where
  parseJSON = JD.withObject "UserGuard" $ \o -> do
    fun <- o JD..: "fun"
    args <- o JD..: "args"
    pure $ StableEncoding (UserGuard (_stableEncoding fun) (fmap _stableEncoding args))

-- TODO: KeySetName is namespaced (maybe)
-- | Stable encoding of `KeySetName`
instance J.Encode (StableEncoding KeySetName) where
  build (StableEncoding (KeySetName ksn mns)) =
    case mns of
         Nothing -> J.build ksn
         Just ns -> J.object [ "ns" J..= StableEncoding ns, "ksn" J..= ksn ]
  {-# INLINABLE build #-}

-- | Stable encoding of `KeySet FullyQualifiedName`
instance J.Encode (StableEncoding KeySet) where
  build (StableEncoding (KeySet keys predFun)) =J.object
    [ "pred" J..= StableEncoding predFun
    , "keys" J..= J.Array (S.mapMonotonic StableEncoding keys) -- TODO: is this valid?
    ]
  {-# INLINABLE build #-}

-- | Stable encoding of `Map Field PactValue`
instance J.Encode (StableEncoding v) => J.Encode (StableEncoding (Map Field v)) where
  build (StableEncoding o) = J.build $ J.Object $ c (M.toList o)
    where
    c :: [(Field, v)] -> [(T.Text, StableEncoding v)]
    c = coerce
  {-# INLINABLE build #-}

-- | Stable encoding of `KSPredicate FullyQualifiedName`
instance J.Encode (StableEncoding KSPredicate) where
  build (StableEncoding ksp) = case ksp of
    KeysAll -> J.build ("keys-all" :: T.Text)
    Keys2 -> J.build ("keys-2" :: T.Text)
    KeysAny -> J.build ("keys-any" :: T.Text)
    CustomPredicate pn -> J.build (renderParsedTyName pn)
  {-# INLINABLE build #-}

instance JD.FromJSON (StableEncoding KSPredicate) where
  parseJSON = JD.withText "KSPredicate" parsePredName
    where
      parsePredName txt = case txt of
        "keys-all" -> pure $ StableEncoding KeysAll
        "keys-any" -> pure $ StableEncoding KeysAny
        "keys-2" -> pure $ StableEncoding Keys2
        _ -> case parseParsedTyName txt of
          Just parsedName -> pure $ StableEncoding (CustomPredicate parsedName)
          Nothing -> fail "invalid keyset predicate"

-- | Stable encoding of `PublicKeyText`
instance J.Encode (StableEncoding PublicKeyText) where
  build (StableEncoding (PublicKeyText pkt)) = J.build pkt
  {-# INLINABLE build #-}

-- | Stable encoding of `PublicKeyText`
instance JD.FromJSON (StableEncoding PublicKeyText) where
  parseJSON = JD.withText "PublicKey" (pure . StableEncoding . PublicKeyText)
  {-# INLINABLE parseJSON #-}

-- | Stable encoding of `NamespaceName`
instance J.Encode (StableEncoding NamespaceName) where
  build (StableEncoding (NamespaceName ns)) = J.build ns
  {-# INLINABLE build #-}

instance JD.FromJSON (StableEncoding NamespaceName) where
  parseJSON = JD.withText "NamespaceName" $ \t -> pure $ StableEncoding (NamespaceName t)

instance J.Encode (StableEncoding Namespace) where
  build (StableEncoding (Namespace nsn user admin)) = J.object
    [ "admin" J..= StableEncoding admin
    , "user" J..= StableEncoding user
    , "name" J..= StableEncoding nsn
    ]

instance JD.FromJSON (StableEncoding Namespace) where
  parseJSON = JD.withObject "Namespace" $ \o -> do
    StableEncoding admin <- o JD..: "admin"
    StableEncoding user <- o JD..: "user"
    StableEncoding nsn <- o JD..: "name"
    pure (StableEncoding (Namespace nsn user admin))

-- | Stable encoding of `ModuleName`
instance J.Encode (StableEncoding ModuleName) where
  build (StableEncoding (ModuleName mn ns)) = J.object
    [ "namespace" J..= (StableEncoding <$> ns)
    , "name" J..= mn
    ]
  {-# INLINABLE build #-}

instance J.Encode (StableEncoding UserTableInfo) where
  build (StableEncoding (UserTableInfo mn)) =
    J.object [ "utModule" J..= StableEncoding mn]

instance JD.FromJSON (StableEncoding ModuleName) where
  parseJSON = JD.withObject "ModuleName" $ \o -> do
    ns <- o JD..:? "namespace"
    mn <- o JD..: "name"
    case parseModuleName mn of
      Nothing -> fail "Invalid module name"
      Just _ -> pure $ StableEncoding (ModuleName mn (fmap _stableEncoding ns))

-- | Stable encoding of `ModRef`
instance J.Encode (StableEncoding ModRef) where
  build (StableEncoding (ModRef mn imp)) = J.object
    [ "refSpec" J..= Just (J.Array (StableEncoding <$> S.toList imp))
    , "refName" J..= StableEncoding mn
    ]
  {-# INLINABLE build #-}

instance JD.FromJSON (StableEncoding ModRef) where
  parseJSON = JD.withObject "ModRef" $ \o -> do
    refName <- o JD..: "refName"
    refSpec :: Maybe [StableEncoding ModuleName] <- o JD..:? "refSpec"
    pure $ StableEncoding (ModRef (_stableEncoding refName) (maybe Set.empty (S.fromList . fmap _stableEncoding) refSpec))

instance J.Encode (StableEncoding UTCTime) where
  build (StableEncoding utc) = encoder timeCodec utc
  {-# INLINABLE build #-}

-- | Stable encoding of `PactValue`
instance J.Encode (StableEncoding PactValue) where
  build (StableEncoding pv) = case pv of
    PLiteral lit  -> J.build (StableEncoding lit)
    PList l -> J.build (J.Array (StableEncoding <$> l))
    PGuard g -> J.build (StableEncoding g)
    PObject o -> J.build (StableEncoding o)
    PModRef mr -> J.build (StableEncoding mr)
    PCapToken ct -> J.build (StableEncoding ct)
    PTime pt -> J.build (StableEncoding pt)
  {-# INLINABLE build #-}

instance JD.FromJSON (StableEncoding PactValue) where
  parseJSON  v = fmap StableEncoding $
    (PLiteral . _stableEncoding <$> JD.parseJSON v) <|>
    (PList . fmap _stableEncoding <$> JD.parseJSON v) <|>
    (PGuard . _stableEncoding <$> JD.parseJSON v) <|>
    (PModRef . _stableEncoding <$> JD.parseJSON v) <|>
    (PTime <$> decoder timeCodec v) <|>
    (PCapToken . _stableEncoding <$> JD.parseJSON v) <|>
    (PObject . fmap _stableEncoding <$> JD.parseJSON v)
  {-# INLINABLE parseJSON #-}

instance JD.FromJSON (StableEncoding Literal) where
  parseJSON n@JD.Number{} = StableEncoding . LDecimal <$> decoder decimalCodec n
  parseJSON (JD.String s) = pure $ StableEncoding $ LString s
  parseJSON (JD.Bool b) = pure $ StableEncoding $ LBool b
  parseJSON o@(JD.Object _) =
    (StableEncoding . LInteger <$> decoder integerCodec o) <|>
    (StableEncoding . LDecimal <$> decoder decimalCodec o)
  parseJSON JD.Null = pure (StableEncoding LUnit)
  parseJSON _t = fail "Literal parse failed"

instance (J.Encode (StableEncoding name), J.Encode (StableEncoding v))
  => J.Encode (StableEncoding (CapToken name v)) where
  build (StableEncoding (CapToken name args)) = J.object
    [ "#ctName" J..= J.build (StableEncoding name)
    , "#ctArgs" J..= J.build (J.Array (StableEncoding <$> args))
    ]
  {-# INLINE build #-}

instance (JD.FromJSON (StableEncoding name), JD.FromJSON (StableEncoding v))
  => JD.FromJSON (StableEncoding (CapToken name v)) where
  parseJSON = JD.withObject "CapToken" $ \o -> do
    name <- o JD..: "#ctName"
    args <- o JD..: "#ctArgs"
    pure $ StableEncoding (CapToken (_stableEncoding name) (_stableEncoding <$> args))
  {-# INLINE parseJSON #-}


-- | Stable encoding of `DefPactContinuation FullyQualifiedName PactValue`
instance J.Encode (StableEncoding (DefPactContinuation QualifiedName PactValue)) where
  build (StableEncoding (DefPactContinuation name args))= J.object
    [ "args" J..= J.Array (StableEncoding <$> args)
    , "def" J..= J.build (StableEncoding name)
    ]
  {-# INLINABLE build #-}

instance J.Encode (StableEncoding (PactEvent PactValue)) where
  build (StableEncoding (PactEvent name args modName (ModuleHash modHash))) = J.object
    [ "params" J..= J.Array (StableEncoding <$> args)
    , "name" J..= name
    , "module" J..= StableEncoding modName
    , "moduleHash" J..= modHash
    ]
  {-# INLINABLE build #-}

instance J.Encode (StableEncoding ModuleHash) where
  build (StableEncoding (ModuleHash h)) = J.build (hashToText h)
  {-# INLINABLE build #-}

instance JD.FromJSON (StableEncoding ModuleHash) where
  parseJSON = JD.withText "ModuleHash" $ \t -> case parseModuleHash t of
    Just mh -> pure $ StableEncoding mh
    _ -> fail "could not parse module hash"

instance JD.FromJSON (StableEncoding (PactEvent PactValue)) where
  parseJSON = JD.withObject "PactEvent" $ \o -> do
    name <- o JD..: "name"
    args <- o JD..: "params"
    modName <- o JD..: "module"
    modHash <- o JD..: "moduleHash"
    pure $ StableEncoding (PactEvent name (fmap _stableEncoding args) (_stableEncoding modName) (_stableEncoding modHash))

instance J.Encode (StableEncoding SpanInfo) where
  build (StableEncoding si) = J.object
    [ "startLine" J..= J.Aeson (_liStartLine si)
    , "endLine" J..= J.Aeson (_liEndLine si)
    , "startColumn" J..= J.Aeson (_liStartColumn si)
    , "endColumn" J..= J.Aeson (_liEndColumn si)
    ]

instance JD.FromJSON (StableEncoding SpanInfo) where
  parseJSON = JD.withObject "SpanInfo" $ \o -> do
    startLine <- o JD..: "startLine"
    startColumn <- o JD..: "startColumn"
    endLine <- o JD..: "endLine"
    endColumn <- o JD..: "endColumn"
    pure $ StableEncoding (SpanInfo startLine startColumn endLine endColumn)



instance J.Encode (StableEncoding GasPrice) where
  build (StableEncoding (GasPrice d)) = J.build $ J.Aeson @Scientific $ fromRational $ toRational d

instance JD.FromJSON (StableEncoding GasPrice) where
  parseJSON (JD.String s) =
    fmap StableEncoding $ case parseNumLiteral s of
      Just (LDecimal d) -> return $ GasPrice d
      Just (LInteger r) -> return $ GasPrice $ fromIntegral r
      _ -> fail $ "Failure parsing decimal string: " ++ show s
  parseJSON (JD.Number n) =
    return $ StableEncoding $ GasPrice (fromRational $ toRational n)
  parseJSON v = fail $ "Failure parsing decimal: " ++ show v

instance J.Encode (StableEncoding GasLimit) where
  build (StableEncoding (GasLimit (Gas l))) = J.build (ParsedInteger (fromIntegral l))

instance JD.FromJSON (StableEncoding GasLimit) where
  parseJSON v = do
    ParsedInteger s <- JD.parseJSON v
    pure (StableEncoding (GasLimit (Gas (fromIntegral s))))

instance J.Encode (StableEncoding TTLSeconds) where
  build (StableEncoding (TTLSeconds b)) = J.build (ParsedInteger b)

instance J.Encode (StableEncoding TxCreationTime) where
  build (StableEncoding (TxCreationTime b)) = J.build (ParsedInteger b)


instance JD.FromJSON (StableEncoding TTLSeconds) where
  parseJSON v = do
    ParsedInteger ttl <- JD.parseJSON v
    pure $ StableEncoding $ TTLSeconds ttl

instance JD.FromJSON (StableEncoding TxCreationTime) where
  parseJSON v = do
    ParsedInteger ttl <- JD.parseJSON v
    pure $ StableEncoding $ TxCreationTime ttl

instance J.Encode (StableEncoding PublicMeta) where
  build (StableEncoding o) = J.object
    [ "creationTime" J..= StableEncoding (_pmCreationTime o)
    , "ttl" J..= StableEncoding (_pmTTL o)
    , "gasLimit" J..= StableEncoding (_pmGasLimit o)
    , "chainId" J..= StableEncoding (_pmChainId o)
    , "gasPrice" J..= StableEncoding (_pmGasPrice o)
    , "sender" J..= _pmSender o
    ]
  {-# INLINABLE build #-}

instance JD.FromJSON (StableEncoding PublicMeta) where
  parseJSON = JD.withObject "PublicMeta" $ \o -> do
    chainId <- o JD..: "chainId"
    sender <- o JD..: "sender"
    StableEncoding gasLimit <- o JD..: "gasLimit"
    StableEncoding gasPrice <- o JD..: "gasPrice"
    StableEncoding ttl <- o JD..: "ttl"
    StableEncoding creationTime <- o JD..: "creationTime"
    pure $ StableEncoding $ PublicMeta (ChainId chainId) sender gasLimit gasPrice ttl creationTime

instance J.Encode (StableEncoding PublicData) where
  build (StableEncoding pd) = J.object
    [ "publicMeta" J..= StableEncoding (_pdPublicMeta pd)
    , "blockTime" J..= J.Aeson (_pdBlockTime pd)
    , "prevBlockHash" J..= _pdPrevBlockHash pd
    , "blockHeight" J..= J.Aeson (_pdBlockHeight pd)
    ]

instance JD.FromJSON (StableEncoding PublicData) where
  parseJSON = JD.withObject "PublicData" $ \o -> do
    StableEncoding pm <- o JD..: "publicMeta"
    bh <- o JD..: "blockHeight"
    bt <- o JD..: "blockTime"
    pbh <- o JD..: "prevBlockHash"
    pure (StableEncoding (PublicData pm bh bt pbh))



instance J.Encode (StableEncoding a) => J.Encode (StableEncoding (Maybe a)) where
  build (StableEncoding a) = J.build (StableEncoding <$> a)

instance JD.FromJSON (StableEncoding a) => JD.FromJSON (StableEncoding (Maybe a)) where
    parseJSON v = StableEncoding . fmap _stableEncoding <$> JD.parseJSON v

instance J.Encode (StableEncoding RowData) where
  build (StableEncoding (RowData o)) = J.object
    [ "$d" J..= (StableEncoding (RowDataValue <$> o)) ]

-- Legacy instance
instance JD.FromJSON (StableEncoding RowData) where
  parseJSON v =
    parseVersioned v <|>
    StableEncoding . RowData . fmap _stableEncoding <$> JD.parseJSON v
    where
      -- Note: This is called `parsedVersioned` as a legacy port, but
      -- there isn't really any need to parse the row data version here.
      -- We are moving to CBOR, so there won't be any versions other than row data
      -- v1
      parseVersioned = JD.withObject "RowData" $ \o -> StableEncoding . RowData
          <$> (fmap (_unRowDataValue._stableEncoding) <$> o JD..: "$d")
  {-# INLINE parseJSON #-}

instance J.Encode (StableEncoding RowDataValue) where
  build (StableEncoding (RowDataValue v)) = case v of
    PLiteral t -> J.build (StableEncoding t)
    PTime t -> encoder timeCodec t
    PList v' -> J.build $ J.Array (StableEncoding . RowDataValue <$> v')
    PGuard g -> buildTagged "g" $ J.build (StableEncoding (RowDataValue <$> g))
    PModRef mr -> buildTagged "m" $ J.build (StableEncoding mr)
    PCapToken ct -> buildTagged "ct" $ J.build (StableEncoding (RowDataValue <$> ct))
    PObject o -> buildTagged "o" $ J.build (StableEncoding (RowDataValue <$> o))
    where
    buildTagged :: Text -> J.Builder -> J.Builder
    buildTagged tag o = J.object
      [ "$t" J..= tag
      , "$v" J..= o ]

newtype RowDataValue
    = RowDataValue { _unRowDataValue :: PactValue }
    deriving newtype (Show, Eq)

instance JD.FromJSON (StableEncoding RowDataValue) where
  parseJSON v1 =
    (StableEncoding . RowDataValue . PLiteral . _stableEncoding <$> JD.parseJSON v1) <|>
    (StableEncoding . RowDataValue . PTime <$> decoder timeCodec v1) <|>
    (StableEncoding . RowDataValue . PList . fmap (_unRowDataValue . _stableEncoding) <$> JD.parseJSON v1) <|>
    parseTagged v1
    where
      parseTagged = JD.withObject "tagged RowData" $ \o -> do
        (t :: Text) <- o JD..: "$t"
        val <- o JD..: "$v"
        case t of
          "o" -> StableEncoding . RowDataValue . PObject . fmap (_unRowDataValue . _stableEncoding) <$> JD.parseJSON val
          "g" -> StableEncoding . RowDataValue . PGuard . fmap (_unRowDataValue) . _stableEncoding <$> JD.parseJSON val
          "ct" -> StableEncoding . RowDataValue . PCapToken . fmap (_unRowDataValue) . _stableEncoding <$> JD.parseJSON val
          "m" -> StableEncoding . RowDataValue . PModRef <$> parseMR val
          _ -> fail "tagged RowData"
      parseMR = JD.withObject "tagged ModRef" $ \o -> ModRef
          <$> (fmap _stableEncoding $ o JD..: "refName")
          <*> (maybe mempty (S.fromList . fmap _stableEncoding) <$> o JD..: "refSpec")
  {-# INLINE parseJSON #-}
