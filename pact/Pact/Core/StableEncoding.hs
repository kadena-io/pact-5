{-# LANGUAGE TypeApplications #-}

-- |
--
-- Stable encoding which matches Pacts StableEncoding.
--

module Pact.Core.StableEncoding
  (encodeStable
  ,StableEncoding(..))
where

import Data.Decimal (DecimalRaw(..))
import Data.Scientific (Scientific)
import Data.Map.Strict (Map)
import Data.Ratio ((%), denominator)
import Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Pact.JSON.Decode as JD
import qualified Pact.JSON.Encode as J
import Pact.JSON.Legacy.Utils

import Pact.Core.Capabilities
import Pact.Core.Literal
import Pact.Core.Guards
import Pact.Core.Names
import Pact.Core.ModRefs
import Pact.Core.Hash
import Pact.Core.DefPacts.Types
import Pact.Core.PactValue
import Pact.Time
import Data.Coerce(coerce)


encodeStable :: J.Encode (StableEncoding a) => a -> ByteString
encodeStable = J.encodeStrict . StableEncoding


newtype StableEncoding a = StableEncoding { _stableEncoding :: a }
  deriving (Ord, Eq)

instance J.Encode (StableEncoding DefPactId) where
  build (StableEncoding (DefPactId pid)) =
    J.build pid

instance JD.FromJSON (StableEncoding DefPactId) where
  parseJSON = JD.withText "DefPactId" $ \s ->
    pure $ StableEncoding (DefPactId s)

-- | Stable encoding of `Literal`
--
-- `isSafeInteger` checks for the Javascript maximum/minimum numbers.
-- Details can be found here: https://github.com/kadena-io/pact/blob/e72d86749f5d65ac8d6e07a7652dd2ffb468607b/src/Pact/Types/Codec.hs#L44
instance J.Encode (StableEncoding Literal) where
  build (StableEncoding lit) = case lit of
    LString t -> J.build t
    LInteger i -> encodeInteger i
    LDecimal d -> encodeDecimal d
    LUnit -> encodeUnit
    LBool b -> J.build b
    where
      encodeInteger i
        | isSafeInteger i = J.object [ "int" J..= J.Aeson i ]
        | otherwise = J.object [ "int" J..= T.pack (show i) ]
      encodeDecimal d@(Decimal _ mantissa)
        | isSafeInteger mantissa = J.build $ J.Aeson @Scientific $ fromRational $ toRational d
        | otherwise = J.object [ "decimal" J..= T.pack (show d) ]
      encodeUnit = J.object ["unit" J..= T.empty] -- TODO: Discuss?
      isSafeInteger i = i >= -9007199254740991 && i <= 9007199254740991
  {-# INLINABLE build #-}

instance J.Encode (StableEncoding RowData) where
  build (StableEncoding (RowData o)) = J.object
    [ "$d" J..= (StableEncoding o) ]

-- | Stable encoding of `Guard FullyQualifiedName PactValue`
instance J.Encode (StableEncoding (Guard QualifiedName PactValue)) where
  build (StableEncoding g) = case g of
    GKeyset ks -> J.build (StableEncoding ks)
    GKeySetRef ksn -> J.object ["keysetref" J..= StableEncoding ksn]
    GUserGuard ug -> J.build (StableEncoding ug)
    GCapabilityGuard cg -> J.build (StableEncoding cg)
    GModuleGuard mg -> J.build (StableEncoding mg)
    GDefPactGuard dpg -> J.build (StableEncoding dpg)
  {-# INLINABLE build #-}

-- | Stable encoding of `CapabilityGuard FullyQualifiedName PactValue`
instance J.Encode (StableEncoding (CapabilityGuard QualifiedName PactValue)) where
  build (StableEncoding (CapabilityGuard name args mpid)) = J.object
    [ "cgPactId" J..= fmap StableEncoding mpid
    , "cgArgs" J..= J.Array (StableEncoding <$> args)
    , "cgName" J..= StableEncoding name
    ]
  {-# INLINABLE build #-}

instance J.Encode (StableEncoding QualifiedName) where
  build (StableEncoding qn) = J.build (renderQualName qn)
  {-# INLINABLE build #-}

-- | Stable encoding of `FullyQualifiedName`
instance J.Encode (StableEncoding FullyQualifiedName) where
  build (StableEncoding (FullyQualifiedName (ModuleName mn mns) n (ModuleHash mh))) = J.build t
    where
      t = maybe "" ((<> ".") . _namespaceName) mns <> mn <> "." <> n <> ".{" <> hashToText mh <> "}"
  {-# INLINABLE build #-}

-- | Stable encoding of `ModuleGuard`
instance J.Encode (StableEncoding ModuleGuard) where
  build (StableEncoding (ModuleGuard m name)) = J.object
    [ "moduleName" J..= _mnName m
    , "name" J..= name
    ]
  {-# INLINABLE build #-}

-- | Stalbe encoding of `DefPactGuard`
instance J.Encode (StableEncoding DefPactGuard) where
  build (StableEncoding (DefPactGuard dpid name)) = J.object
    [ "pactId" J..= StableEncoding dpid
    , "name" J..= name
    ]
  {-# INLINABLE build #-}

-- | Stable encoding of `UserGuard FullyQualifiedName PactValue`
instance J.Encode (StableEncoding (UserGuard QualifiedName PactValue)) where
  build (StableEncoding (UserGuard fun args)) = J.object
    [ "args" J..= J.array (StableEncoding <$> args)
    , "fun" J..= StableEncoding fun
    ]
  {-# INLINABLE build #-}

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
    , "keys" J..= J.Array (S.map StableEncoding keys) -- TODO: is this valid?
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

-- | Stable encoding of `PublicKeyText`
instance J.Encode (StableEncoding PublicKeyText) where
  build (StableEncoding (PublicKeyText pkt)) = J.build pkt
  {-# INLINABLE build #-}

-- | Stable encoding of `NamespaceName`
instance J.Encode (StableEncoding NamespaceName) where
  build (StableEncoding (NamespaceName ns)) = J.build ns
  {-# INLINABLE build #-}

-- | Stable encoding of `ModuleName`
instance J.Encode (StableEncoding ModuleName) where
  build (StableEncoding (ModuleName mn ns)) = J.object
    [ "namespace" J..= (StableEncoding <$> ns)
    , "name" J..= mn
    ]
  {-# INLINABLE build #-}

-- | Stable encoding of `ModRef`
instance J.Encode (StableEncoding ModRef) where
  build (StableEncoding (ModRef mn imp)) = J.object
    [ "refSpec" J..= Just (J.Array (StableEncoding <$> S.toList imp))
    , "refName" J..= StableEncoding mn
    ]
  {-# INLINABLE build #-}

-- | Stable encoding of `UTCTime`
--
-- See https://github.com/kadena-io/pact/blob/e72d86749f5d65ac8d6e07a7652dd2ffb468607b/src/Pact/Types/Codec.hs#L150
-- for further details
instance J.Encode (StableEncoding UTCTime) where
  build (StableEncoding utc)
    | denom utc == 1 = J.object [ "time" J..= T.pack (formatTime "%Y-%m-%dT%H:%M:%SZ" utc) ]
    | otherwise = J.object [ "timep" J..= T.pack (formatTime "%Y-%m-%dT%H:%M:%S.%vZ" utc) ]
    where
      denom :: UTCTime -> Integer
      denom = denominator . (% 1000) . fromIntegral . toPosixTimestampMicros
  {-# INLINABLE build #-}

-- | Stable encoding of `PactValue`
instance J.Encode (StableEncoding PactValue) where
  build (StableEncoding pv) = case pv of
    PLiteral lit  -> J.build (StableEncoding lit)
    PList l -> J.build (J.Array (StableEncoding <$> l))
    PGuard g -> J.build (StableEncoding g)
    PObject o -> J.build (StableEncoding o)
    PModRef mr -> J.build (StableEncoding mr)
    -- TODO: implement/figure this out
    PCapToken _ct -> error "not implemented"
    PTime pt -> J.build (StableEncoding pt)
  {-# INLINABLE build #-}

instance J.Encode (StableEncoding name) => J.Encode (StableEncoding (CapToken name PactValue)) where
  build (StableEncoding (CapToken name args)) = J.object
    [ "name" J..= J.build (StableEncoding name)
    , "args" J..= J.build (J.Array (StableEncoding <$> args))
    ]


-- | Stable encoding of `DefPactContinuation FullyQualifiedName PactValue`
instance J.Encode (StableEncoding (DefPactContinuation QualifiedName PactValue)) where
  build (StableEncoding (DefPactContinuation name args))= J.object
    [ "args" J..= J.Array (StableEncoding <$> args)
    , "def" J..= J.build (StableEncoding name)
    ]
  {-# INLINABLE build #-}
