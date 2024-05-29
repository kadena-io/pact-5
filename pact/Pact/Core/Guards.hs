{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StrictData #-}


module Pact.Core.Guards
( PublicKeyText(..)
, renderPublicKeyText
, KeySetName(..)
, renderKeySetName
, keysetNameParser
, parseAnyKeysetName
, Governance(..)
, KeySet(..)
, Guard(..)
, UserGuard(..)
, CapabilityGuard(..)
, DefPactGuard(..)
, KSPredicate(..)
, predicateToText
, ModuleGuard(..)
-- , CapGovRef(..)

-- * Key Format Validation
, allKeyFormats
, ed25519HexFormat
, webAuthnFormat
, isValidKeyFormat
, enforceKeyFormats
)
where

import Control.Applicative
import Control.Lens (_Left, over)
import Control.Monad
import Control.DeepSeq
import Data.Attoparsec.Text
import Data.ByteString (ByteString)
import Data.Foldable
import Data.Maybe (isJust)
import Data.Text(Text)
import GHC.Generics
import Text.Parser.Token as P

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Base16 as B16
import qualified Data.Char as Char
import qualified Codec.Serialise as Serialise

import Pact.Core.Pretty
import Pact.Core.Names
import Pact.Core.RuntimeParsers
import qualified Pact.Crypto.WebAuthn.Cose.PublicKeyWithSignAlg as WA
import qualified Pact.Crypto.WebAuthn.Cose.SignAlg as WA

newtype PublicKeyText = PublicKeyText { _pubKey :: Text }
  deriving (Eq,Ord,Show, NFData)

instance Pretty PublicKeyText where
  pretty (PublicKeyText t) = pretty t

renderPublicKeyText :: PublicKeyText -> Text
renderPublicKeyText = _pubKey

data KeySetName = KeySetName
  { _keysetName :: Text
  , _keysetNs :: Maybe NamespaceName
  } deriving (Eq, Ord, Show, Generic)

instance NFData KeySetName

instance Pretty KeySetName where
  pretty (KeySetName ks Nothing) = "'" <> pretty ks
  pretty (KeySetName ks (Just ns)) = "'" <> pretty ns <> "." <> pretty ks

renderKeySetName :: KeySetName -> Text
renderKeySetName (KeySetName n Nothing) = n
renderKeySetName (KeySetName n (Just ns)) = _namespaceName ns <> "." <> n

keysetNameParser :: Parser KeySetName
keysetNameParser = qualified <|> withoutNs
  where
    qualified = do
      ns <- NamespaceName <$> ident style
      kn <- P.dot *> ident style
      pure $ KeySetName kn (Just ns)
    withoutNs = do
      t <- takeText
      guard $ not $ T.null t
      pure $ KeySetName t Nothing

parseAnyKeysetName :: Text -> Either String KeySetName
parseAnyKeysetName = parseOnly keysetNameParser

data Governance name
  = KeyGov KeySetName
  | CapGov (FQNameRef name)
  deriving (Eq, Show, Generic)

instance NFData name => NFData (Governance name)

data KSPredicate
  = KeysAll
  | Keys2
  | KeysAny
  | CustomPredicate ParsedTyName
  deriving (Eq, Show, Ord, Generic)

instance NFData KSPredicate

predicateToText ::  KSPredicate -> Text
predicateToText = \case
    KeysAll -> "keys-all"
    Keys2 -> "keys2"
    KeysAny -> "keys-any"
    CustomPredicate pn -> renderParsedTyName pn

instance Pretty KSPredicate where
  pretty = pretty . predicateToText

data KeySet
  = KeySet
  { _ksKeys :: !(S.Set PublicKeyText)
  , _ksPredFun :: KSPredicate
  } deriving (Eq, Show, Ord, Generic)

instance NFData KeySet

instance Pretty KeySet where
  pretty (KeySet ks f) = "KeySet" <+> commaBraces
    [ "keys: " <> prettyList (S.toList ks)
    , "pred: " <> pretty f
    ]

ed25519HexFormat :: PublicKeyText -> Bool
ed25519HexFormat (PublicKeyText k) = T.length k == 64 && T.all isHexDigitLower k

webAuthnFormat :: PublicKeyText -> Bool
webAuthnFormat = isJust . parseWebAuthnPublicKeyText

parseWebAuthnPublicKeyText :: PublicKeyText -> Maybe WA.CosePublicKey
parseWebAuthnPublicKeyText (PublicKeyText k)
  | Just pkText <- T.stripPrefix webAuthnPrefix k
  , T.all isHexDigitLower pkText
  , Right kbs <- B16.decode (T.encodeUtf8 pkText)
  , Right pk <- parseWebAuthnPublicKey kbs
  = Just pk
  | otherwise = Nothing
  where
  parseWebAuthnPublicKey :: ByteString -> Either String WA.CosePublicKey
  parseWebAuthnPublicKey rawPk = do
    pk <- over _Left (\e -> "WebAuthn public key parsing error: " <> show e) $
      Serialise.deserialiseOrFail @WA.CosePublicKey (BSL.fromStrict rawPk)
    webAuthnPubKeyHasValidAlg pk
    return pk

  webAuthnPubKeyHasValidAlg :: WA.CosePublicKey -> Either String ()
  webAuthnPubKeyHasValidAlg (WA.PublicKeyWithSignAlg _ signAlg) =
    unless (WA.fromCoseSignAlg signAlg `elem` validCoseSignAlgorithms)
      $ Left "Signing algorithm must be EdDSA or P256"

validCoseSignAlgorithms :: [Int]
validCoseSignAlgorithms =
  [ -7 -- ECDSA with SHA-256, the most common WebAuthn signing algorithm.
  , -8 -- EdDSA, which is also supported by YubiKey.
  ]

-- | Lower-case hex numbers.
isHexDigitLower :: Char -> Bool
isHexDigitLower c =
  -- adapted from GHC.Unicode#isHexDigit
  Char.isDigit c || (fromIntegral (Char.ord c - Char.ord 'a')::Word) <= 5


-- | Prefix for any webauthn keys.
-- This prefix is required for recognizing public keys originating from webauthn
-- attestastion ceremonies later in a pact runtime context.
webAuthnPrefix :: Text
webAuthnPrefix = "WEBAUTHN-"

allKeyFormats :: [PublicKeyText -> Bool]
allKeyFormats = [ed25519HexFormat, webAuthnFormat]

isValidKeyFormat :: PublicKeyText -> Bool
isValidKeyFormat k = any ($ k) allKeyFormats

enforceKeyFormats :: (PublicKeyText -> err) -> KeySet -> Either err ()
enforceKeyFormats onErr (KeySet keys _pred) = traverse_ validateKey keys
  where
  validateKey k = if isValidKeyFormat k then pure () else Left $ onErr k


data UserGuard name term
  = UserGuard
  { _ugFunction :: name
  , _ugArgs :: [term] }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance (NFData name, NFData term) => NFData (UserGuard name term)

instance (Pretty name, Pretty term) => Pretty (UserGuard name term) where
  pretty (UserGuard fn args) = "UserGuard" <+> commaBraces
    [ "fun: " <> pretty fn
    , "args: " <> prettyList args
    ]

data ModuleGuard
  = ModuleGuard
  { _mgModule :: ModuleName
  , _mgName :: Text
  } deriving (Show, Eq, Ord, Generic)

instance NFData ModuleGuard

-- Todo: module guards are compared on equality based on name
-- Why????
-- instance Eq ModuleGuard where
--   mg == mg' = _mgModule mg == _mgModule mg'

-- instance Ord ModuleGuard where
--   mg `compare` mg' = _mgModule mg `compare` _mgModule mg'

instance Pretty ModuleGuard where
  pretty (ModuleGuard mg name) = "ModuleGuard" <+> commaBraces
    [ "module: " <> pretty mg
    , "name: " <> pretty name
    ]

data CapabilityGuard name term
  = CapabilityGuard
  { _cgName :: !name
  , _cgArgs :: ![term]
  , _cgPactId :: !(Maybe DefPactId) }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance (NFData name, NFData term) => NFData (CapabilityGuard name term)

data DefPactGuard
  = DefPactGuard
  { _dpgDefPactId :: !DefPactId
  , _dpgName :: !Text
  } deriving (Eq, Ord, Show, Generic)

instance NFData DefPactGuard

data Guard name term
  = GKeyset KeySet
  | GKeySetRef KeySetName
  | GUserGuard (UserGuard name term)
  | GCapabilityGuard (CapabilityGuard name term)
  | GModuleGuard ModuleGuard
  | GDefPactGuard DefPactGuard
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance (NFData name, NFData term) => NFData (Guard name term)

instance (Pretty name, Pretty term) => Pretty (Guard name term) where
  pretty = \case
    GKeyset ks -> pretty ks
    GKeySetRef ks -> pretty ks
    GUserGuard ug -> pretty ug
    GCapabilityGuard cg -> pretty cg
    GModuleGuard g -> pretty g
    GDefPactGuard dpg -> pretty dpg


instance (Pretty name, Pretty term) => Pretty (CapabilityGuard name term) where
  pretty (CapabilityGuard cg args pid) = "CapabilityGuard" <+> commaBraces
    [ "name: " <> pretty cg
    , "args: " <> pretty args
    , "pactId: " <> pretty pid
    ]

instance Pretty DefPactGuard where
  pretty (DefPactGuard dpid name) = "PactGuard" <+> commaBraces
    [ "pactId: " <> pretty dpid
    , "name: "   <> pretty name
    ]
