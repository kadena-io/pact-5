{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}

module Pact.Core.Names
 ( ModuleName(..)
 , NamespaceName(..)
 , Field(..)
 , ParsedName(..)
 , ParsedTyName(..)
 , DynamicName(..)
 , DynamicRef(..)
 , Name(..)
 , NameKind(..)
 , BareName(..)
 , QualifiedName(..)
 , renderQualName
 , renderModuleName
 , TypeVar(..)
 , Unique
 , tyVarName
 , tyVarUnique
 , tyname
 , tynameUnique
 , Supply
 , NamedDeBruijn(..)
 , ndIndex
 , ndName
 , DeBruijn
 , TypeName(..)
 , rawParsedName
 , ONameKind(..)
 , OverloadedName(..)
 , FullyQualifiedName(..)
 , TableName(..)
 , replRawModuleName
 , replModuleName
 , replModuleHash
 , fqnToName
 , fqnToQualName
 , NativeName(..)
 , RowKey(..)
 , renderFullyQualName
 , FQNameRef(..)
 , fqName
 , fqModule
 , fqHash
 , userTable
 , DefPactId(..)
 , parseModuleName
 , renderDefPactId
 ) where

import Control.Lens
import Data.Text(Text)
import Data.Word(Word64)
import Control.Applicative((<|>))
import Control.DeepSeq
import GHC.Generics
import qualified Data.Text as T
import qualified Data.Char as Char
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP

import Pact.Core.Hash
import Pact.Core.Pretty(Pretty(..))

-- | Newtype wrapper over bare namespaces
newtype NamespaceName = NamespaceName { _namespaceName :: Text }
  deriving (Eq, Ord, Show, Generic)

instance NFData NamespaceName

instance Pretty NamespaceName where
  pretty (NamespaceName n) = pretty n

-- Module names, which consist of a raw module name
-- and maybe a namespace qualifier
data ModuleName = ModuleName
  { _mnName      :: Text
  , _mnNamespace :: Maybe NamespaceName
  } deriving (Eq, Ord, Show, Generic)

instance NFData ModuleName

instance Pretty ModuleName where
  pretty (ModuleName m mn) =
    maybe mempty (\b -> pretty b <> ".") mn <> pretty m

newtype BareName
  = BareName
  { _bnName :: Text }
  deriving (Show, Eq, Ord)

instance Pretty BareName where
  pretty (BareName b) = pretty b

-- | Qualified module members.
data QualifiedName =
  QualifiedName
  { _qnName :: Text
  , _qnModName :: ModuleName
  } deriving (Show, Eq, Generic)

instance NFData QualifiedName

instance Ord QualifiedName where
  compare (QualifiedName qn1 m1) (QualifiedName qn2 m2) =
    case compare m1 m2 of
      EQ -> compare qn1 qn2
      t -> t

renderQualName :: QualifiedName -> Text
renderQualName (QualifiedName n (ModuleName m ns)) =
  maybe "" ((<> ".") . _namespaceName) ns <> m <> "." <> n

renderModuleName :: ModuleName -> Text
renderModuleName (ModuleName m ns) =
  maybe "" ((<> ".") . _namespaceName) ns <> m

instance Pretty QualifiedName where
  pretty (QualifiedName n m) =
    pretty m <> "." <> pretty n

data DynamicName
  = DynamicName
  { _dnName :: Text
  , _dnCall :: Text
  } deriving (Show, Eq)

data ParsedTyName
  = TQN QualifiedName
  | TBN BareName
  deriving (Show, Eq)

instance Pretty ParsedTyName where
  pretty = \case
    TQN qn -> pretty qn
    TBN n -> pretty n

data ParsedName
  = QN QualifiedName
  | BN BareName
  | DN DynamicName
  deriving (Show, Eq)

-- | The member name of the ParsedName
-- that is either an atom "f"
-- or "f" in <qualifier>."f"
rawParsedName :: ParsedName -> Text
rawParsedName (BN (BareName n)) = n
rawParsedName (QN qn) = _qnName qn
rawParsedName (DN dn) = _dnName dn

instance Pretty ParsedName where
  pretty = \case
    QN qn -> pretty qn
    BN n -> pretty n
    DN dn -> pretty (_dnName dn) <> "::" <> pretty (_dnCall dn)

-- | Object and Schema row labels.
-- So in Field "a" in {"a":v},
newtype Field = Field { _field :: Text }
  deriving (Eq, Ord, Show, Generic)
  deriving newtype NFData

instance Pretty Field where
  pretty (Field f) = pretty f

-- Uniques
type Unique = Int
type Supply = Int

-- A name paired with a debruijn index
data NamedDeBruijn
  = NamedDeBruijn
  { _ndIndex :: !DeBruijn
  , _ndName :: Text }
  deriving (Show, Eq)

type DeBruijn = Word64

-- | Names used in dictionary overloading that handle
-- injected typeclass dictionaries. For use in pact-core-typed
data ONameKind b
  = OBound Unique
  | OTopLevel ModuleName ModuleHash
  | OBuiltinDict b
  deriving (Show, Eq)

-- | Name with overloaded dictionary definitions
-- For use in pact-core-typed
data OverloadedName b
  = OverloadedName
  { _olName :: !Text
  , _olNameKind :: ONameKind b }
  deriving (Show, Eq)

-- | Name type representing all local and free
-- variable binders and dynamic invokes
data Name
  = Name
  { _nName :: !Text
  , _nKind :: NameKind }
  deriving (Show, Eq, Ord)

-- Dynamic references.
data DynamicRef
  = DynamicRef
  { _drNameArg :: !Text
  , _drBindType :: DeBruijn
  } deriving (Show, Eq, Ord)

-- | NameKind distinguishes the identifier
-- from the binding type, whether it is a free or bound variable,
-- and whether the free variable is simply a module reference,
-- a top-level function, or a dynamic reference
data NameKind
  = NBound DeBruijn
  -- ^ Locally bound names, via defuns or lambdas
  | NTopLevel ModuleName ModuleHash
  -- ^ top level names, referring to only
  -- defuns, defconsts, deftables and defcaps
  | NModRef ModuleName [ModuleName]
  -- ^ module reference, pointing to the module name +
  -- the implemented interfaces
  | NDynRef DynamicRef
  deriving (Show, Eq, Ord)

data FullyQualifiedName
  = FullyQualifiedName
  { _fqModule :: ModuleName
  , _fqName :: !Text
  , _fqHash :: ModuleHash
  } deriving (Eq, Show, Ord, Generic)

instance NFData FullyQualifiedName

fqnToName :: FullyQualifiedName -> Name
fqnToName (FullyQualifiedName mn name mh) =
  Name name (NTopLevel mn mh)

fqnToQualName :: FullyQualifiedName -> QualifiedName
fqnToQualName (FullyQualifiedName mn name _) =
  QualifiedName name mn

instance Pretty FullyQualifiedName where
  pretty fq = pretty $ fqnToQualName fq

data TypeVar
  = TypeVar
  { _tyVarName :: !Text
  , _tyVarUnique :: !Unique }
  | UnificationVar
  { _tyVarName :: !Text
  , _tyVarUnique :: !Unique }
  deriving (Show)

instance Eq TypeVar where
  l == r = _tyVarUnique l == _tyVarUnique r

instance Ord TypeVar where
  l <= r = _tyVarUnique l <= _tyVarUnique r

instance Pretty TypeVar where
  pretty t = pretty (_tyVarName t)

data TypeName
  = TypeName
  { _tyname :: !Text
  , _tynameUnique :: !Unique }
  deriving (Show, Eq)

newtype NativeName
  = NativeName
  { _natName :: Text }
  deriving (Show, Eq)

makeLenses ''TypeVar
makeLenses ''TypeName
makeLenses ''NamedDeBruijn
makeClassy ''NativeName

instance (Pretty b) => Pretty (OverloadedName b) where
  pretty (OverloadedName n nk) = case nk of
    OBound _ -> pretty n
    OBuiltinDict b -> "DICT<" <> pretty b <> ">"
    OTopLevel mn _ -> pretty mn <> "." <> pretty n

instance Pretty Name where
  pretty (Name n nk) = case nk of
    NBound dix -> pretty n <> "<" <> pretty dix <> ">"
    NTopLevel mn _mh -> pretty mn <> "." <> pretty n
    NModRef m _ -> pretty m
    NDynRef dr -> pretty n <> "::" <> pretty (_drNameArg dr)

instance Pretty NamedDeBruijn where
  pretty (NamedDeBruijn _i _n) =
    pretty _n

newtype TableName = TableName { _tableName :: Text }
  deriving (Eq, Ord, Show)

instance Pretty TableName where
  pretty (TableName tn) = pretty tn

-- | Constants for resolving repl things
replRawModuleName :: Text
replRawModuleName = "#repl"

-- | Repl module
replModuleName :: ModuleName
replModuleName = ModuleName replRawModuleName Nothing

replModuleHash :: ModuleHash
replModuleHash = ModuleHash (Hash "#repl")

renderFullyQualName :: FullyQualifiedName -> Text
renderFullyQualName (FullyQualifiedName mn n mh) =
  renderQualName (QualifiedName n mn) <> ".{" <> hashToText (_mhHash mh) <> "}"

-- | Newtype over text user keys
newtype RowKey
  = RowKey { _rowKey :: Text }
  deriving (Eq, Ord, Show)

-- | A Name reference which
-- is always fully qualified after name resolution
data FQNameRef name where
  FQParsed :: ParsedName -> FQNameRef ParsedName
  FQName :: FullyQualifiedName -> FQNameRef Name

instance Show (FQNameRef name) where
  show = \case
    FQParsed pn -> show pn
    FQName fqn -> show fqn

instance Eq (FQNameRef name) where
  (FQParsed pn) == (FQParsed pn') = pn == pn'
  (FQName fqn) == (FQName fqn') = fqn == fqn'

makeLenses ''FullyQualifiedName

userTable :: TableName -> TableName
userTable (TableName tn) = TableName ("USER_" <> tn)

-- | The identifier that indexes defpacts in the db,
--   generally computed from the continuation, or
--   in the case of nested defpacts, the hash of the
--   parent + the nested continuation
newtype DefPactId
  = DefPactId { _defpactId :: Text }
  deriving (Eq,Ord,Show)

instance Pretty DefPactId where
  pretty (DefPactId p) = pretty p

type Parser = MP.Parsec () Text

identParser :: Parser Text
identParser = do
  c1 <- MP.letterChar <|> MP.oneOf specials
  rest <- MP.takeWhileP Nothing (\c -> Char.isLetter c || Char.isDigit c || elem c specials)
  pure (T.cons c1 rest)
  where
  specials :: String
  specials = "%#+-_&$@<>=^?*!|/~"

moduleNameParser :: Parser ModuleName
moduleNameParser = do
  p <- identParser
  MP.try (go p <|> pure (ModuleName p Nothing))
  where
  go ns = do
    _ <- MP.single '.'
    p1 <- identParser
    pure (ModuleName p1 (Just (NamespaceName ns)))

parseModuleName :: Text -> Maybe ModuleName
parseModuleName = MP.parseMaybe moduleNameParser

renderDefPactId :: DefPactId -> Text
renderDefPactId (DefPactId t) = t
