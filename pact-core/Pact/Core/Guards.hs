{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}


module Pact.Core.Guards
( PublicKeyText(..)
, renderPublicKeyText
, KeySetName(..)
, renderKeySetName
, Governance(..)
, KeySet(..)
, enforceKeyFormats
, Guard(..)
, UserGuard(..)
, CapabilityGuard(..)
, DefPactGuard(..)
, KSPredicate(..)
, predicateToString
, ModuleGuard(..)
, CapGovRef(..)
)
where

import qualified Data.Char as Char
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Foldable
import Data.String
import Data.Text(Text)
import Pact.Core.Pretty

import Pact.Core.Names

newtype PublicKeyText = PublicKeyText { _pubKey :: Text }
  deriving (Eq,Ord,Show)

instance Pretty PublicKeyText where
  pretty (PublicKeyText t) = pretty t

renderPublicKeyText :: PublicKeyText -> Text
renderPublicKeyText = _pubKey

newtype KeySetName = KeySetName { _keysetName :: Text }
    deriving (Eq,Ord,Show)

instance Pretty KeySetName where
  pretty (KeySetName ks) = "'" <> pretty ks

renderKeySetName :: KeySetName -> Text
renderKeySetName = _keysetName

data Governance name
  = KeyGov KeySetName
  | CapGov (CapGovRef name)
  deriving (Eq, Show)

data CapGovRef name where
  UnresolvedGov :: ParsedName -> CapGovRef ParsedName
  ResolvedGov :: FullyQualifiedName -> CapGovRef Name

instance Eq (CapGovRef name) where
  (UnresolvedGov g1) == (UnresolvedGov g2) = g1 == g2
  (ResolvedGov g1) == (ResolvedGov g2) = g1 == g2

instance Show (CapGovRef name) where
  show (UnresolvedGov ksn) = "(UnresolvedGov " <> show ksn <> ")"
  show (ResolvedGov g) = "(ResolvedGov" <> show g <> ")"

data KSPredicate name
  = KeysAll
  | Keys2
  | KeysAny
  -- | CustomPredicate name
  deriving (Eq, Show, Ord)

predicateToString :: IsString s => KSPredicate name -> s
predicateToString = \case
    KeysAll -> "keys-all"
    Keys2 -> "keys2"
    KeysAny -> "keys-any"

instance Pretty (KSPredicate name) where
  pretty = predicateToString

data KeySet name
  = KeySet
  { _ksKeys :: !(S.Set PublicKeyText)
  , _ksPredFun :: KSPredicate name
  } deriving (Eq, Show, Ord)

instance Pretty name => Pretty (KeySet name) where
  pretty (KeySet ks f) = "KeySet" <+> commaBraces
    [ "keys: " <> prettyList (S.toList ks)
    , "pred: " <> pretty f
    ]

type KeyFormatValidator = PublicKeyText -> Bool

ed22519Hex :: KeyFormatValidator
ed22519Hex (PublicKeyText k) = T.length k == 64 && T.all isHexDigitLower k
  where
  isHexDigitLower c = Char.isDigit c || (fromIntegral (Char.ord c - Char.ord 'a') :: Word) <= 5

keyFormats :: [KeyFormatValidator]
keyFormats = [ed22519Hex]

isValidKeyFormat :: PublicKeyText -> Bool
isValidKeyFormat k = any ($ k) keyFormats

enforceKeyFormats :: (PublicKeyText -> err) -> KeySet a -> Either err ()
enforceKeyFormats onErr (KeySet keys _pred) = traverse_ validateKey keys
  where
  validateKey k = if isValidKeyFormat k then pure () else Left $ onErr k


data UserGuard name term
  = UserGuard
  { _ugFunction :: name
  , _ugArgs :: [term] }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Pretty name, Pretty term) => Pretty (UserGuard name term) where
  pretty (UserGuard fn args) = "UserGuard" <+> commaBraces
    [ "fun: " <> pretty fn
    , "args: " <> prettyList args
    ]

data ModuleGuard
  = ModuleGuard
  { _mgModule :: ModuleName
  , _mgName :: Text
  } deriving (Show, Eq, Ord)

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
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data DefPactGuard
  = DefPactGuard
  { _dpgDefPactId :: !DefPactId
  , _dpgName :: !Text
  } deriving (Eq, Ord, Show)

data Guard name term
  = GKeyset (KeySet name)
  | GKeySetRef KeySetName
  | GUserGuard (UserGuard name term)
  | GCapabilityGuard (CapabilityGuard name term)
  | GModuleGuard ModuleGuard
  | GDefPactGuard DefPactGuard
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Pretty name, Pretty term) => Pretty (Guard name term) where
  pretty = \case
    GKeyset ks -> pretty ks
    GKeySetRef ks -> pretty ks
    GUserGuard ug -> pretty ug
    GCapabilityGuard cg -> pretty cg
    GModuleGuard g -> pretty g
    GDefPactGuard dpg -> pretty dpg


data Namespace name term
  = Namespace
  { _nsName :: !NamespaceName
  , _nsUser :: !(Guard name term)
  , _nsAdmin :: !(Guard name term)
  } deriving (Eq, Show)

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
