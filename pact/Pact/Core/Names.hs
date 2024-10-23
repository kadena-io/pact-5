{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StrictData #-}

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
 , qnName
 , qnModName
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
 , tableName
 , tableModuleName
 , replRawModuleName
 , replModuleName
 , replModuleHash
 , fqnToName
 , fqnToQualName
 , qualNameToFqn
 , NativeName(..)
 , RowKey(..)
 , rowKey
 , renderFullyQualName
 , FQNameRef(..)
 , fqName
 , fqModule
 , fqHash
 , DefPactId(..)
 , parseModuleName
 , renderDefPactId
 , renderParsedTyName
 , parseParsedTyName
 , parseQualifiedName
 , parseFullyQualifiedName
 , VerifierName(..)
 , renderTableName
 , jsonSafeRenderTableName
 , parseJsonSafeTableName
 , HashedModuleName(..)
 , renderHashedModuleName
 , parseHashedModuleName
 ) where

import Control.Lens
import Data.Aeson
import Data.Text(Text)
import qualified Data.Text as T
import Data.Word(Word64)
import Control.Applicative((<|>))
import Control.DeepSeq
import GHC.Generics
import Codec.Serialise.Class(Serialise)
import qualified Data.Char as Char
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP

import qualified Pact.JSON.Encode as J

import Pact.Core.Hash
import Pact.Core.Pretty hiding (dot)
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Short as SB
import Data.String (IsString)

-- | Newtype wrapper over bare namespaces
newtype NamespaceName = NamespaceName { _namespaceName :: Text }
  deriving (Generic)
  deriving newtype (Eq, Ord, Show, NFData, IsString)

instance Pretty NamespaceName where
  pretty (NamespaceName n) = pretty n

-- Module names, which consist of a raw module name
-- and maybe a namespace qualifier
data ModuleName = ModuleName
  { _mnName      :: Text
  , _mnNamespace :: Maybe NamespaceName
  } deriving (Eq, Ord, Show, Generic)

instance NFData ModuleName

data HashedModuleName
  = HashedModuleName
  { _hmName :: ModuleName
  , _hmHash :: ModuleHash
  } deriving (Eq, Ord, Show, Generic)

instance Pretty ModuleName where
  pretty (ModuleName m mn) =
    maybe mempty (\b -> pretty b <> ".") mn <> pretty m

newtype BareName
  = BareName
  { _bnName :: Text }
  deriving newtype (Show, Eq, Ord, NFData)

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
  } deriving (Show, Eq, Generic)

instance NFData DynamicName

instance Pretty DynamicName where
  pretty (DynamicName dn call) =
    pretty dn <> "::" <> pretty call

data ParsedTyName
  = TQN QualifiedName
  | TBN BareName
  deriving (Show, Eq, Ord, Generic)

instance NFData ParsedTyName

instance Pretty ParsedTyName where
  pretty = \case
    TQN qn -> pretty qn
    TBN n -> pretty n

data ParsedName
  = QN QualifiedName
  | BN BareName
  | DN DynamicName
  deriving (Show, Eq, Generic)

instance NFData ParsedName

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
  deriving (Eq, Ord, Show, Generic, FromJSONKey)
  deriving newtype (IsString, NFData, Serialise)

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
  deriving (Show, Eq, Ord, Generic)

instance NFData Name

-- Dynamic references.
data DynamicRef
  = DynamicRef
  { _drNameArg :: !Text
  , _drBindType :: DeBruijn
  } deriving (Show, Eq, Ord, Generic)

instance NFData DynamicRef

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
  deriving (Show, Eq, Ord, Generic)

instance NFData NameKind

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

qualNameToFqn :: QualifiedName -> ModuleHash -> FullyQualifiedName
qualNameToFqn (QualifiedName name mn) mh =
  FullyQualifiedName mn name mh

instance Pretty FullyQualifiedName where
  pretty (FullyQualifiedName mn n mh) =
    pretty (QualifiedName n mn) <> ".{" <> pretty (_mhHash mh) <> "}"

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
  deriving newtype (Show, Eq, Ord, NFData)

makeLenses ''TypeVar
makeLenses ''TypeName
makeLenses ''NamedDeBruijn
makeClassy ''NativeName

instance Pretty NativeName where
  pretty (NativeName n) = pretty n

instance (Pretty b) => Pretty (OverloadedName b) where
  pretty (OverloadedName n nk) = case nk of
    OBound _ -> pretty n
    OBuiltinDict b -> "DICT<" <> pretty b <> ">"
    OTopLevel mn _ -> pretty mn <> "." <> pretty n

instance Pretty Name where
  pretty (Name n nk) = case nk of
    NBound dix -> pretty n <> "<" <> pretty dix <> ">"
    NTopLevel mn _mh -> pretty mn <> "." <> pretty n <> "." <> braces (pretty _mh)
    NModRef m _ -> pretty m
    NDynRef dr -> pretty n <> "::" <> pretty (_drNameArg dr)

instance Pretty NamedDeBruijn where
  pretty (NamedDeBruijn _i _n) =
    pretty _n

data TableName
  = TableName
  { _tableName :: Text
  , _tableModuleName :: ModuleName
  } deriving (Eq, Ord, Show, Generic)

instance NFData TableName

makeLenses ''TableName

instance Pretty TableName where
  pretty (TableName tn ns) = pretty ns <> pretty '_' <> pretty tn

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
  deriving newtype (Eq, Ord, Show, NFData)

makeLenses ''RowKey

instance Pretty RowKey where
  pretty (RowKey rk) = pretty rk

-- | A Name reference which
-- is always fully qualified after name resolution
data FQNameRef name where
  FQParsed :: ParsedName -> FQNameRef ParsedName
  FQName :: FullyQualifiedName -> FQNameRef Name

instance NFData (FQNameRef name) where
  rnf (FQParsed pn) = rnf pn
  rnf (FQName fqn) = rnf fqn

instance Show (FQNameRef name) where
  show = \case
    FQParsed pn -> show pn
    FQName fqn -> show fqn

instance Eq (FQNameRef name) where
  (FQParsed pn) == (FQParsed pn') = pn == pn'
  (FQName fqn) == (FQName fqn') = fqn == fqn'

instance Pretty (FQNameRef name) where
  pretty = \case
    FQParsed pn -> pretty pn
    FQName fqn -> pretty (_fqName fqn)



makeLenses ''FullyQualifiedName
makeLenses ''QualifiedName

-- | The identifier that indexes defpacts in the db,
--   generally computed from the continuation, or
--   in the case of nested defpacts, the hash of the
--   parent + the nested continuation
newtype DefPactId
  = DefPactId { _defPactId :: Text }
  deriving newtype (Eq,Ord,Show, NFData)

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
    _ <- dot
    p1 <- identParser
    pure (ModuleName p1 (Just (NamespaceName ns)))

jsonSafeTableNameParser :: Parser TableName
jsonSafeTableNameParser = do
  p <- moduleNameParser
  _ <- MP.char ':'
  ident <- identParser
  pure (TableName ident p)

hashedModuleNameParser :: Parser HashedModuleName
hashedModuleNameParser = do
  mn <- moduleNameParser
  h <- MP.between (MP.char '{') (MP.char '}') $
      MP.takeWhile1P Nothing (\s -> Char.isAlphaNum s || s `elem` ['-', '_'])
  hash' <- case decodeBase64UrlUnpadded (T.encodeUtf8 h) of
    Right hash' -> pure $ ModuleHash $ Hash $ SB.toShort hash'
    Left _ -> fail "invalid hash encoding"
  pure (HashedModuleName mn hash')

qualNameParser :: Parser QualifiedName
qualNameParser = do
  ModuleName n ns <- moduleNameParser
  case ns of
    Just nsn@(NamespaceName nsRaw) ->
      go n nsn <|> pure (QualifiedName n (ModuleName nsRaw Nothing))
    Nothing ->
      fail "invalid qualified name"
  where
  go n nsn = do
    _ <- dot
    p1 <- identParser
    let qual = QualifiedName p1 (ModuleName n (Just nsn))
    pure qual

dot :: Parser Char
dot = MP.char '.'

fullyQualNameParser :: Parser FullyQualifiedName
fullyQualNameParser = do
  qualifier <- identParser
  mname <- dot *> identParser
  dot *> (withIdent qualifier mname <|> withHash qualifier mname Nothing)
  where
  withIdent qualifier mname = do
    i <- MP.try identParser
    dot *> withHash qualifier mname (Just i)
  withHash qualifier mname oname = do
    h <- MP.between (MP.char '{') (MP.char '}') $
      MP.takeWhile1P Nothing (\s -> Char.isAlphaNum s || s `elem` ['-', '_'])
    hash' <- case decodeBase64UrlUnpadded (T.encodeUtf8 h) of
      Right hash' -> pure $ ModuleHash $ Hash $ SB.toShort hash'
      Left _ -> fail "invalid hash encoding"
    case oname of
      Just nn ->
        pure (FullyQualifiedName (ModuleName mname (Just (NamespaceName qualifier))) nn hash')
      Nothing ->
        pure (FullyQualifiedName (ModuleName qualifier Nothing) mname hash')

-- Here we are parsing either a qualified name, or a bare name
-- bare names are just the atom `n`, and qualified names are of the form
-- <n>.<n>(.<n>)?
-- so therefore, if we've parsed a module name without a namespace, then we actually have
-- a barename. Otherwise, we either have a qualified name ready, or we need to parse one more
-- dot identifier to make it work
parsedTyNameParser :: Parser ParsedTyName
parsedTyNameParser = do
  ModuleName n ns <- moduleNameParser
  case ns of
    Just nsn@(NamespaceName nsRaw) ->
      go n nsn <|> pure (TQN (QualifiedName n (ModuleName nsRaw Nothing)))
    Nothing -> pure (TBN (BareName n))
  where
  go n nsn = do
    _ <- dot
    p1 <- identParser
    let qual = QualifiedName p1 (ModuleName n (Just nsn))
    pure (TQN qual)

parseModuleName :: Text -> Maybe ModuleName
parseModuleName = MP.parseMaybe (moduleNameParser <* MP.eof)

parseHashedModuleName :: Text -> Maybe HashedModuleName
parseHashedModuleName =  MP.parseMaybe (hashedModuleNameParser <* MP.eof)

parseParsedTyName :: Text -> Maybe ParsedTyName
parseParsedTyName = MP.parseMaybe (parsedTyNameParser <* MP.eof)

parseQualifiedName :: Text -> Maybe QualifiedName
parseQualifiedName = MP.parseMaybe (qualNameParser <* MP.eof)

parseFullyQualifiedName :: Text -> Maybe FullyQualifiedName
parseFullyQualifiedName = MP.parseMaybe (fullyQualNameParser <* MP.eof)

renderDefPactId :: DefPactId -> Text
renderDefPactId (DefPactId t) = t

renderParsedTyName :: ParsedTyName -> Text
renderParsedTyName (TBN (BareName n)) = n
renderParsedTyName (TQN qn) = renderQualName qn

newtype VerifierName = VerifierName Text
  deriving newtype (J.Encode, NFData, Eq, Show, Ord, FromJSON)
  deriving stock Generic

-- | Map the user's table name into a set of names suitable for
--   storage in the persistence backend.
renderTableName :: TableName -> Text
renderTableName (TableName tbl mn) = renderModuleName mn <> "_" <> tbl

renderHashedModuleName :: HashedModuleName -> Text
renderHashedModuleName (HashedModuleName mn mh) =
  renderModuleName mn <> "{" <> moduleHashToText mh <> "}"

-- | Map the user's table name into a set of names suitable for
--   storage in the persistence backend.
jsonSafeRenderTableName :: TableName -> Text
jsonSafeRenderTableName (TableName tbl mn) = renderModuleName mn <> ":" <> tbl

parseJsonSafeTableName :: Text -> Maybe TableName
parseJsonSafeTableName = MP.parseMaybe (jsonSafeTableNameParser <* MP.eof)
