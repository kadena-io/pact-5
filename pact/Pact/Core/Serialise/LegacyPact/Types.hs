{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}


{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pact.Core.Serialise.LegacyPact.Types where

import Data.Text (Text)
import Text.Read (readMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.Vector (Vector)
import GHC.Generics
import qualified Data.List.NonEmpty as NE

import qualified Pact.JSON.Decode as JD
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Set (Set)
import qualified Data.Map.Strict as M
import Data.ByteString.Short (ShortByteString, toShort)
import Bound.Scope
import Bound.Var
import Bound
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V

import qualified Data.Attoparsec.Text as AP
import Data.Decimal (Decimal)
import Pact.Time
import qualified Pact.Core.Hash as PC

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as A
import qualified Data.Aeson.Types as A
import qualified Data.Aeson.KeyMap as A

import Pact.JSON.Legacy.Hashable
import Data.String (IsString, fromString)
import Data.List (sort)
import Text.Trifecta (ident,TokenParsing,(<?>),dot, alphaNum, between, char, CharParsing, IdentifierStyle(..), letter,  digit, oneOf)
import Pact.Core.Hash (decodeBase64UrlUnpadded)
import Text.Parser.Token.Highlight
import Data.Hashable
import Control.Applicative
import Control.Monad (ap)
import Data.Functor.Classes (Eq1(..), Show1(..))
import Text.Show.Deriving

data ModuleData r = ModuleData
  { _mdModule :: !(ModuleDef (Def r))
  , _mdRefMap :: !(HM.HashMap Text r)
  , _mdDependencies :: !(HM.HashMap FullyQualifiedName r)
  }
  deriving (Generic, Functor, Foldable, Traversable)

instance (JD.FromJSON r) => JD.FromJSON (ModuleData r) where
  parseJSON =
    JD.withObject "ModuleData" $ \o ->
      ModuleData
      <$> o JD..: "module"
      <*> o JD..: "refMap"
      <*> (HM.fromList <$> (fromMaybe mempty <$> o JD..:? "dependencies"))

instance Hashable FullyQualifiedName where
  hashWithSalt s FullyQualifiedName{..} =
    s `hashWithSalt` _fqName `hashWithSalt` _fqModule `hashWithSalt` _fqModuleHash

data ModuleDef g
  = MDModule !(Module g)
  | MDInterface !Interface
 deriving (Eq,Functor,Foldable,Traversable,Show,Generic)


instance JD.FromJSON g => JD.FromJSON (ModuleDef g) where
  parseJSON v
    = MDModule <$> JD.parseJSON v
    <|> MDInterface <$> JD.parseJSON v

data Interface = Interface
  { _interfaceName :: !ModuleName
  , _interfaceImports :: ![Use]
  }
  deriving (Eq,Show,Generic)

instance JD.FromJSON Interface where
  parseJSON = JD.withObject "Interface" $ \o ->
    Interface
      <$> o JD..: "name"
      <*> o JD..: "imports"

data Module g = Module
  { _mName :: !ModuleName
  , _mGovernance :: !(Governance g)
  , _mHash :: !ModuleHash
  , _mBlessed :: !(HS.HashSet ModuleHash)
  , _mInterfaces :: ![ModuleName]
  , _mImports :: ![Use]
  }
  deriving (Eq,Functor,Foldable,Traversable,Show,Generic)

instance JD.FromJSON g => JD.FromJSON (Module g) where
  parseJSON = JD.withObject "Module" $ \o ->
    Module
      <$> o JD..: "name"
      <*> o JD..: "governance"
      <*> o JD..: "hash"
      <*> o JD..: "blessed"
      <*> o JD..: "interfaces"
      <*> o JD..: "imports"

data ModuleName = ModuleName
  { _mnName      :: Text
  , _mnNamespace :: Maybe NamespaceName
  } deriving (Eq, Ord, Generic, Show)

instance Hashable ModuleName where
  hashWithSalt s (ModuleName n Nothing)   =
    s `hashWithSalt` (0::Int) `hashWithSalt` n
  hashWithSalt s (ModuleName n (Just ns)) =
    s `hashWithSalt` (1::Int) `hashWithSalt` n `hashWithSalt` ns


instance JD.FromJSON ModuleName where
  parseJSON = JD.withObject "ModuleName" $ \o ->
    ModuleName
      <$> o JD..: "name"
      <*> o JD..: "namespace"

newtype NamespaceName = NamespaceName { _namespaceName :: Text }
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (JD.FromJSON, Hashable)

newtype Governance g = Governance { _gGovernance :: Either KeySetName g }
  deriving (Eq,Ord,Functor,Foldable,Traversable,Show,Generic)

instance JD.FromJSON g => JD.FromJSON (Governance g) where
  parseJSON = JD.withObject "Governance" $ \o ->
    Governance <$> (Left <$> o JD..: "keyset" <|>
                    Right <$> o JD..: "capability")

data KeySetName
  = KeySetName
  { _ksnName :: Text
  , _ksnNamespace :: Maybe NamespaceName
  } deriving (Eq, Ord, Show, Generic)

instance JD.FromJSON KeySetName where
  parseJSON v =
    newKs v <|> oldKs v
    where
    oldKs = JD.withText "KeySetName" (pure . (`KeySetName` Nothing))
    newKs =
      JD.withObject "KeySetName" $ \o -> KeySetName
        <$> o JD..: "ksn"
        <*> (fromMaybe Nothing <$> o JD..:? "ns")

data Literal =
    LString { _lString :: !Text } |
    LInteger { _lInteger :: !Integer } |
    LDecimal { _lDecimal :: !Decimal } |
    LBool { _lBool :: !Bool } |
    LTime { _lTime :: !UTCTime }
        deriving (Eq,Generic,Ord,Show)

instance JD.FromJSON Literal where
  parseJSON = \case
    n@JD.Number{} -> LDecimal <$> decodeDecimal n
    JD.String s -> pure $ LString s
    JD.Bool b -> pure $ LBool b
    o@JD.Object {} ->
      (LInteger <$> decodeInteger o) <|>
      (LTime <$> decodeTime o) <|>
      (LDecimal <$> decodeDecimal o)
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

newtype ModuleHash = ModuleHash { _mhHash :: Hash }
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (Hashable, JD.FromJSON)

newtype Hash = Hash { unHash :: ShortByteString }
  deriving (Eq, Ord, Generic, Hashable,LegacyHashable, Show)

instance JD.FromJSON Hash where
  parseJSON = JD.withText "Hash" $ \t -> case PC.decodeBase64UrlUnpadded (T.encodeUtf8 t) of
    Left _ -> fail "cant decode"
    Right t' -> pure (Hash $ toShort t')

instance JD.FromJSONKey Hash where
    fromJSONKey = JD.FromJSONKeyTextParser $ \t ->
      case PC.decodeBase64UrlUnpadded (T.encodeUtf8 t) of
        Left _ -> fail "cant decode"
        Right t' -> pure (Hash $ toShort t')

data Use = Use
  { _uModuleName :: !ModuleName
  , _uModuleHash :: !(Maybe ModuleHash)
  , _uImports :: !(Maybe (Vector Text))
  }
  deriving (Show, Eq, Generic)

instance JD.FromJSON Use where
  parseJSON = JD.withObject "Use" $ \o ->
    Use <$> o JD..: "module"
        <*> o JD..:? "hash"
        <*> o JD..:? "imports"

data Def n = Def
  { _dDefName :: !DefName
  , _dModule :: !ModuleName
  , _dDefType :: !DefType
  , _dFunType :: !(FunType (Term n))
  , _dDefBody :: !(Scope Int Term n)
  , _dDefMeta :: !(Maybe (DefMeta (Term n)))
  } deriving (Functor,Foldable,Traversable,Generic)

deriving instance (Show1 Term, Show n) => Show (Def n)

data DefcapMeta n =
  DefcapManaged
  { _dcManaged :: !(Maybe (Text,n))
    -- ^ "Auto" managed or user-managed by (param,function)
  } |
  DefcapEvent
    -- ^ Eventing defcap.
  deriving (Functor,Foldable,Traversable,Generic,Eq,Show,Ord)

instance JD.FromJSON n => JD.FromJSON (Def n) where
  parseJSON = JD.withObject "Def" $ \o ->
    Def
      <$> o JD..: "defName"
      <*> o JD..: "module"
      <*> o JD..: "defType"
      <*> o JD..: "funType"
      <*> o JD..: "defBody"
      <*> o JD..: "defMeta"

instance (JD.FromJSON b, Traversable f, JD.FromJSON (f JD.Value), JD.FromJSON (f a)) =>
  JD.FromJSON (Scope b f a) where
  parseJSON = JD.withObject "Scope" $ \o -> do
    f <- o JD..: "scope"
    Scope <$> traverse JD.parseJSON f

instance (A.FromJSON a, A.FromJSON b) =>
  JD.FromJSON (Var b a) where
  parseJSON = JD.withObject "Var" $ \v ->
    (B <$> v A..: "b") <|> (F <$> v A..: "f")

instance JD.FromJSON n => JD.FromJSON (DefcapMeta n) where
  parseJSON v = parseUser v <|> parseAuto v <|> parseEvent v
    where
      parseUser = JD.withObject "DefcapMeta" $ \o -> DefcapManaged . Just <$>
        ((,) <$> o JD..: "managedParam" <*> o JD..: "managerFun")
      parseAuto = JD.withObject "DefcapMeta" $ \o -> do
        b <- o JD..: "managerAuto"
        if b then pure (DefcapManaged Nothing)
        else fail "Expected True"
      parseEvent = JD.withText "DefcapMeta" $ \t ->
        if t == "event" then pure DefcapEvent
        else fail "Expected 'event'"

newtype DefName = DefName { _unDefName :: Text }
  deriving (Show,Eq,Ord)
  deriving newtype (JD.FromJSON)

newtype DefMeta n = DMDefcap (DefcapMeta n)
  deriving (Functor,Foldable,Traversable,Generic,Eq,Show,Ord)


instance JD.FromJSON n => JD.FromJSON (DefMeta n) where
  parseJSON = fmap DMDefcap . JD.parseJSON


data DefType
  = Defun
  | Defpact
  | Defcap
  deriving (Eq,Show,Generic, Bounded, Enum)

instance JD.FromJSON DefType

-- | Function type
data FunType o = FunType {
  _ftArgs :: ![Arg o],
  _ftReturn :: !(Type o)
  }
  deriving (Eq,Ord,Functor,Foldable,Traversable,Generic,Show)

instance JD.FromJSON o => JD.FromJSON (FunType o) where
  parseJSON = JD.withObject "FunType" $ \o -> FunType
    <$> o JD..: "args"
    <*> o JD..: "return"

data Arg o = Arg {
  _aName :: Text,
  _aType :: Type o
  }
  deriving (Eq,Ord,Functor,Foldable,Traversable,Generic,Show)

instance JD.FromJSON o => JD.FromJSON (Arg o) where
  parseJSON = JD.withObject "Arg" $ \o -> Arg
    <$> o JD..: "name"
    <*> o JD..: "type"

-- | Pact types.
data Type v =
  TyAny |
  TyVar { _tyVar :: !(TypeVar v) } |
  TyPrim PrimType |
  TyList { _tyListType :: !(Type v) } |
  TySchema
  { _tySchema :: !SchemaType
  , _tySchemaType :: !(Type v)
  , _tySchemaPartial :: !SchemaPartial } |
  TyFun { _tyFunType :: !(FunType v) } |
  TyUser { _tyUser :: !v } |
  TyModule
  { _tyModuleSpec :: !(Maybe [v])
    -- ^ Nothing for interfaces, implemented ifaces for modules
  }
  deriving (Eq,Show,Ord,Functor,Foldable,Traversable,Generic)

withThisText :: [Char] -> Text -> A.Value -> A.Parser a -> A.Parser a
withThisText s t v p = JD.withText s go v
  where
    go tv | tv == t = p
          | otherwise = fail $ s ++ ": Expected " ++ show t

instance JD.FromJSON v => JD.FromJSON (Type v) where
  parseJSON v =
    withThisText "TyAny" "*" v (pure TyAny) <|>
    (TyVar <$> JD.parseJSON v) <|>
    (TyPrim <$> JD.parseJSON v) <|>
    (TyFun <$> JD.parseJSON v) <|>
    JD.withObject "TyList" (\o -> TyList <$> o JD..: "list") v <|>
    JD.withObject "TySchema"
      (\o -> TySchema
        <$> o JD..: "schema"
        <*> o JD..: "type"
        <*> o JD..: "partial")
      v <|>
    JD.withObject "TyModule" (\o -> TyModule <$> o JD..: "modspec") v <|>
    (TyUser <$> JD.parseJSON v)

-- | Type variables are namespaced for value types and schema types.
data TypeVar v =
  TypeVar { _tvName :: !TypeVarName, _tvConstraint :: ![Type v] } |
  SchemaVar { _tvName :: !TypeVarName }
  deriving (Eq,Ord,Show, Functor,Foldable,Traversable,Generic)

instance JD.FromJSON v => JD.FromJSON (TypeVar v) where
  parseJSON = JD.withObject "TypeVar" $ \o -> (o JD..: "tag") >>= \case
    ("TypeVar" :: Text) -> TypeVar <$> o JD..: "name" <*> o JD..: "constraint"
    "SchemaVar" -> SchemaVar <$> o JD..: "name"
    _t -> fail "unexpected decoding"

newtype TypeVarName = TypeVarName { _typeVarName :: Text }
  deriving (Eq,Ord,Generic, Show)
  deriving newtype (JD.FromJSON)

data PrimType =
  TyInteger |
  TyDecimal |
  TyTime |
  TyBool |
  TyString |
  TyGuard !(Maybe GuardType)
  deriving (Eq,Ord,Generic,Show)

instance JD.FromJSON PrimType where
  parseJSON v = JD.withText "PrimType" doStr v <|> JD.withObject "PrimType" doObj v
    where
      doStr s
        | s == tyInteger = pure TyInteger
        | s == tyDecimal = pure TyDecimal
        | s == tyTime = pure TyTime
        | s == tyBool = pure TyBool
        | s == tyString = pure TyString
        | otherwise = fail "Bad PrimType Value"
      doObj o = TyGuard <$> o JD..: "guard"

tyInteger,tyDecimal,tyTime,tyBool,tyString,tyList,tyObject,
  tyKeySet,tyTable,tyGuard :: Text
tyInteger = "integer"
tyDecimal = "decimal"
tyTime = "time"
tyBool = "bool"
tyString = "string"
tyList = "list"
tyObject = "object"
tyKeySet = "keyset"
tyGuard = "guard"
tyTable = "table"

data GuardType
  = GTyKeySet
  | GTyKeySetName
  | GTyPact
  | GTyUser
  | GTyModule
  | GTyCapability
  deriving (Eq,Ord,Generic,Show)

instance JD.FromJSON GuardType where
  parseJSON = JD.withText "GuardType" $ \case
    "keyset" -> pure GTyKeySet
    "keysetref" -> pure GTyKeySetName
    "pact" -> pure GTyPact
    "user" -> pure GTyUser
    "module" -> pure GTyModule
    "capability" -> pure GTyCapability
    _ -> fail "Unrecognized guard type"

data SchemaType =
  TyTable |
  TyObject |
  TyBinding
  deriving (Eq,Ord,Generic,Show)

instance JD.FromJSON SchemaType where
  parseJSON = JD.withText "SchemaType" $ \case
    "table" -> pure TyTable
    "object" -> pure TyObject
    "binding" -> pure TyBinding
    _ -> fail "Bad SchemaType value"

data SchemaPartial = FullSchema | PartialSchema !(Set Text) | AnySubschema
  deriving (Eq,Ord,Show,Generic)

instance JD.FromJSON SchemaPartial where
  parseJSON v =
    withThisText "FullSchema" "full" v (pure FullSchema) <|>
    withThisText "AnySubschema" "any" v (pure AnySubschema) <|>
    (PartialSchema <$> JD.parseJSON v)



newtype NativeDefName = NativeDefName Text
  deriving (Show,Eq,Ord)
  deriving newtype (JD.FromJSON)

data FunApp = FunApp
  { _faName :: !Text
  , _faModule :: !(Maybe ModuleName)
  , _faDefType :: !DefType
  , _faTypes :: !(FunTypes (Term Name))
  , _faDocs :: !(Maybe Text)
  } deriving (Generic)

instance JD.FromJSON FunApp where
  parseJSON = JD.withObject "FunApp" $ \o ->
    FunApp
      <$> o JD..: "name"
      <*> o JD..: "module"
      <*> o JD..: "defType"
      <*> o JD..: "type"
      <*> o JD..: "docs"

data NativeDFun = NativeDFun
  { _nativeName :: !NativeDefName
  , _nativeFun :: !(forall m . Monad m => FunApp -> [Term Ref] -> m (Term Name))
  }

instance Show NativeDFun where
  showsPrec p (NativeDFun name _) = showParen (p > 10) $
    showString "NativeDFun " . showsPrec 11 name . showString " _"

type Ref = Ref' (Term Name)
-- | Variable type for an evaluable 'Term'.
data Ref' d =
  -- | "Reduced" (evaluated) or native (irreducible) term.
  Direct !d |
  -- | Unevaulated/un-reduced term, never a native.
  Ref !(Term (Ref' d))
--  deriving (Functor,Foldable,Traversable,Generic)
  deriving (Generic)

deriving instance (Show1 Term, Show d) => Show (Ref' d)
type FunTypes o = NE.NonEmpty (FunType o)

data Example
  = ExecExample !Text
  -- ^ An example shown as a good execution
  | ExecErrExample !Text
  -- ^ An example shown as a failing execution
  | LitExample !Text
  -- ^ An example shown as a literal
  deriving (Eq, Show, Generic)


data ConstVal n =
  CVRaw { _cvRaw :: !n } |
  CVEval { _cvRaw :: !n
         , _cvEval :: !n }
  deriving (Eq,Functor,Foldable,Traversable,Generic,Show)

instance JD.FromJSON n => JD.FromJSON (ConstVal n) where
  parseJSON v =
    JD.withObject "CVEval"
     (\o -> CVEval <$> o JD..: "raw" <*> o JD..: "eval") v <|>
    JD.withObject "CVRaw"
     (\o -> CVRaw <$> o JD..: "raw") v

data App t = App
  { _appFun :: !t
  , _appArgs :: ![t]
  } deriving (Functor,Foldable,Traversable,Eq,Show,Generic)

instance JD.FromJSON t => JD.FromJSON (App t) where
  parseJSON = JD.withObject "App" $ \o -> App
    <$> o JD..: "fun"
    <*> o JD..: "args"

data BindPair n = BindPair
  { _bpArg :: !(Arg n)
  , _bpVal :: !n }
  deriving (Eq,Show,Functor,Traversable,Foldable,Generic)

instance JD.FromJSON n => JD.FromJSON (BindPair n) where
  parseJSON = JD.withObject "BindPair" $ \o ->
    BindPair
      <$> o JD..: "arg"
      <*> o JD..: "val"

data BindType n =
  -- | Normal "let" bind
  BindLet |
  -- | Schema-style binding, with string value for key
  BindSchema { _bType :: !n }
  deriving (Eq,Functor,Foldable,Traversable,Ord,Show,Generic)

instance JD.FromJSON n => JD.FromJSON (BindType n) where
  parseJSON v =
    withThisText "BindLet" "let" v (pure BindLet) <|>
    JD.withObject "BindSchema" (\o -> BindSchema <$> o JD..: "bind") v


data Lam n
  = Lam
  { _lamArg :: !Text
  , _lamTy  :: !(FunType (Term n))
  , _lamBindBody :: !(Scope Int Term n)
  } deriving (Functor,Foldable,Traversable,Generic)

deriving instance (Show1 Term, Show n) => Show (Lam n)
deriving instance (Eq1 Term, Eq n) => Eq (Lam n)

instance JD.FromJSON n => JD.FromJSON (Lam n) where
  parseJSON = JD.withObject "Lam" $ \o ->
    Lam
      <$> o JD..: "amArg"
      <*> o JD..: "amTy"
      <*> o JD..: "amBindBody"

newtype TypeName = TypeName Text
  deriving (Show,Eq,Ord,Generic)
  deriving newtype (JD.FromJSON)

data Term n =
    TModule {
      _tModuleDef :: !(ModuleDef (Term n))
    , _tModuleBody :: !(Scope () Term n)
    } |
    TList {
      _tList :: !(Vector (Term n))
    , _tListType :: !(Type (Term n))
    } |
    TDef {
      _tDef :: !(Def n)
    } |
    TConst {
      _tConstArg :: !(Arg (Term n))
    , _tModule :: !(Maybe ModuleName)
    , _tConstVal :: !(ConstVal (Term n))
    } |
    TApp {
      _tApp :: !(App (Term n))
    } |
    TVar {
      _tVar :: !n
    } |
    TBinding {
      _tBindPairs :: ![BindPair (Term n)]
    , _tBindBody :: !(Scope Int Term n)
    , _tBindType :: !(BindType (Type (Term n)))
    } |
    TLam {
      _tLam :: Lam n
    } |
    TObject {
      _tObject :: !(Object n)
    } |
    TSchema {
      _tSchemaName :: !TypeName
    , _tModule :: !(Maybe ModuleName)
    , _tFields :: ![Arg (Term n)]
    } |
    TLiteral {
      _tLiteral :: !Literal
    } |
    TGuard {
      _tGuard :: !(Guard (Term n))
    } |
    TUse {
      _tUse :: !Use
    } |
    TStep {
      _tStep :: !(Step (Term n))
    } |
    TModRef {
      _tModRef :: !ModRef
    } |
    TTable {
      _tTableName :: !TableName
    , _tModuleName :: !ModuleName
    , _tHash :: !ModuleHash
    , _tTableType :: !(Type (Term n))
    } |
    TDynamic {
      _tDynModRef :: !(Term n)
    , _tDynMember :: !(Term n)
    }
    deriving (Functor,Foldable,Traversable,Generic)

deriving instance (Show1 Term, Show n) => Show (Term n)



newtype FieldKey = FieldKey Text
  deriving (Show,Eq,Ord,Generic)
  deriving newtype (JD.FromJSON)

newtype ObjectMap v = ObjectMap { _objectMap :: M.Map FieldKey v }
  deriving (Eq,Ord,Show,Functor,Foldable,Traversable,Generic)

instance JD.FromJSON v => JD.FromJSON (ObjectMap v) where
  parseJSON v = flip (JD.withObject "ObjectMap") v $ \_ ->
    ObjectMap . M.mapKeys FieldKey <$> JD.parseJSON v

data Object n = Object
  { _oObject :: !(ObjectMap (Term n))
  , _oObjectType :: !(Type (Term n))
  , _oKeyOrder :: !(Maybe [FieldKey])
  } deriving (Functor,Foldable,Traversable,Generic)

deriving instance (Show1 Term, Show n) => Show (Object n)

instance JD.FromJSON n => JD.FromJSON (Object n) where
  parseJSON = JD.withObject "Object" $ \o ->
    Object <$> o JD..: "obj" <*> o JD..: "type" <*> o JD..:? "keyorder" -- <*> o JD..: "i"

data Step n = Step
  { _sEntity :: !(Maybe n)
  , _sExec :: !n
  , _sRollback :: !(Maybe n)
  } deriving (Eq,Show,Generic,Functor,Foldable,Traversable)

instance JD.FromJSON n => JD.FromJSON (Step n) where
  parseJSON = JD.withObject "Step" $ \o -> Step
    <$> o JD..: "entity"
    <*> o JD..: "exec"
    <*> o JD..: "rollback"

data ModRef = ModRef
    { _modRefName :: !ModuleName
      -- ^ Fully-qualified module name.
    , _modRefSpec :: !(Maybe [ModuleName])
      -- ^ Specification: for modules, 'Just' implemented interfaces;
      -- for interfaces, 'Nothing'.
    } deriving (Eq,Show,Generic, Ord)

instance JD.FromJSON ModRef where
  parseJSON = JD.withObject "ModRef" $ \o ->
    ModRef
      <$> o JD..: "refName"
      <*> o JD..: "refSpec"

newtype TableName = TableName Text
  deriving (Show,Eq,Ord)
  deriving newtype (JD.FromJSON)


prop :: IsString a => Semigroup a => TermProperties -> a
prop TermArgs = "args"
prop TermBody = "body"
prop TermConstArg = "arg"
prop TermConstVal = "val"
prop TermDefBody = "defBody"
prop TermDefMeta = "defMeta"
prop TermDefName = "defName"
prop TermDefType = "defType"
prop TermDynMem = "dmem"
prop TermDynRef = "dref"
prop TermFields = "fields"
prop TermFun = "fun"
prop TermFunType = "funType"
prop TermGuard = "guard"
prop TermHash = "hash"
prop TermI = "i"
prop TermInfo = "info"
prop TermLamArg = "amArg"
prop TermLamBindBody = "amBindBody"
prop TermLamInfo = "amInfo"
prop TermLamTy = "amTy"
prop TermList = "list"
prop TermLiteral = "lit"
prop TermMeta = "meta"
prop TermModName = "modname"
prop TermModRefInfo = "refInfo"
prop TermModRefName = "refName"
prop TermModRefSpec = "refSpec"
prop TermModule = "module"
prop TermName = "name"
prop TermNatDocs = "docs"
prop TermNatExamples = "examples"
prop TermNatFunTypes = "types"
prop TermNatTopLevel = "tl"
prop TermObjectKeyorder = "keyorder"
prop TermObjectObj = "obj"
prop TermPairs = "pairs"
prop TermType = "type"
prop (TermUnknown t) = "UNKNOWN_TERM[" <> fromString t <> "]"
prop TermUseImports = "imports"
prop TermVar = "var"

unprop :: IsString a => Eq a => Show a => a -> TermProperties
unprop "args" = TermArgs
unprop "body" = TermBody
unprop "arg" = TermConstArg
unprop "val" = TermConstVal
unprop "defBody" = TermDefBody
unprop "defMeta" = TermDefMeta
unprop "defName" = TermDefName
unprop "defType" = TermDefType
unprop "dmem" = TermDynMem
unprop "dref" = TermDynRef
unprop "fields" = TermFields
unprop "fun" = TermFun
unprop "funType" = TermFunType
unprop "guard" = TermGuard
unprop "hash" = TermHash
unprop "i" = TermI
unprop "info" = TermInfo
unprop "amArg" = TermLamArg
unprop "amBindBody" = TermLamBindBody
unprop "amInfo" = TermLamInfo
unprop "amTy" = TermLamTy
unprop "list" = TermList
unprop "lit" = TermLiteral
unprop "meta" = TermMeta
unprop "modname" = TermModName
unprop "refInfo" = TermModRefInfo
unprop "refName" = TermModRefName
unprop "refSpec" = TermModRefSpec
unprop "module" = TermModule
unprop "name" = TermName
unprop "docs" = TermNatDocs
unprop "examples" = TermNatExamples
unprop "types" = TermNatFunTypes
unprop "tl" = TermNatTopLevel
unprop "keyorder" = TermObjectKeyorder
unprop "obj" = TermObjectObj
unprop "pairs" = TermPairs
unprop "type" = TermType
unprop "imports" = TermUseImports
unprop "var" = TermVar
unprop t = TermUnknown (show t)
{-# INLINE unprop #-}

data TermProperties
  = TermArgs
  | TermBody
  | TermConstArg
  | TermConstVal
  | TermDefBody
  | TermDefMeta
  | TermDefName
  | TermDefType
  | TermDynMem
  | TermDynRef
  | TermFields
  | TermFun
  | TermFunType
  | TermGuard
  | TermHash
  | TermI
  | TermInfo
  | TermLamArg
  | TermLamBindBody
  | TermLamInfo
  | TermLamTy
  | TermList
  | TermLiteral
  | TermMeta
  | TermModName
  | TermModRefInfo
  | TermModRefName
  | TermModRefSpec
  | TermModule
  | TermName
  | TermNatDocs
  | TermNatExamples
  | TermNatFunTypes
  | TermNatTopLevel
  | TermObjectKeyorder
  | TermObjectObj
  | TermPairs
  | TermType
  | TermUnknown !String
  | TermUseImports
  | TermVar

  deriving (Show, Eq, Ord)

data Guard a
  = GPact !PactGuard
  | GKeySet !KeySet
  | GKeySetRef !KeySetName
  | GModule !ModuleGuard
  | GUser !(UserGuard a)
  | GCapability !(CapabilityGuard a)
  deriving (Eq,Show,Generic,Functor,Foldable,Traversable,Ord)


instance JD.FromJSON a => JD.FromJSON (Guard a) where
  parseJSON v = case props v of
    [GuardKeys, GuardPred] -> GKeySet <$> JD.parseJSON v
    [GuardKeysetref] -> flip (JD.withObject "KeySetRef") v $ \o ->
        GKeySetRef <$> o JD..: "keysetref" -- keyNamef
    [GuardName, GuardPactId] -> GPact <$> JD.parseJSON v
    [GuardModuleName, GuardName] -> GModule <$> JD.parseJSON v
    [GuardArgs, GuardFun] -> GUser <$> JD.parseJSON v
    [GuardCgArgs, GuardCgName, GuardCgPactId] -> GCapability <$> JD.parseJSON v
    _ -> fail "unexpected properties for Guard"
   where
    props (JD.Object o) = sort $ ungprop <$> A.keys o
    props _ = []

ungprop :: (IsString a, Eq a) => Show a => a -> GuardProperty
ungprop "args" = GuardArgs
ungprop "cgArgs" = GuardCgArgs
ungprop "cgName" = GuardCgName
ungprop "cgPactId" = GuardCgPactId
ungprop "fun" = GuardFun
ungprop "keys" = GuardKeys
ungprop "keysetref" = GuardKeysetref
ungprop "ksn" = GuardKsn
ungprop "moduleName" = GuardModuleName
ungprop "name" = GuardName
ungprop "ns" = GuardNs
ungprop "pactId" = GuardPactId
ungprop "pred" = GuardPred
ungprop t = GuardUnknown (show t)

data GuardProperty
  = GuardArgs
  | GuardCgArgs
  | GuardCgName
  | GuardCgPactId
  | GuardFun
  | GuardKeys
  | GuardKeysetref
  | GuardKsn
  | GuardModuleName
  | GuardName
  | GuardNs
  | GuardPactId
  | GuardPred
  | GuardUnknown !String
  deriving (Show, Eq, Ord)

data PactGuard = PactGuard
  { _pgPactId :: !PactId
  , _pgName :: !Text
  } deriving (Eq,Generic,Show,Ord)

instance JD.FromJSON PactGuard where
  parseJSON = JD.withObject "PactGuard" $ \o ->
    PactGuard
      <$> o JD..: "PactId"
      <*> o JD..: "Name"

newtype PactId = PactId Text
  deriving (Eq,Ord,Show,Generic)
  deriving newtype (JD.FromJSON,JD.FromJSONKey)

-- | KeySet pairs keys with a predicate function name.
data KeySet = KeySet
  { _ksKeys :: !(Set PublicKeyText)
  , _ksPredFun :: !Name
  } deriving (Eq,Generic,Show,Ord)

instance JD.FromJSON KeySet where

    parseJSON v = JD.withObject "KeySet" keyListPred v <|> keyListOnly
      where
        defPred = Name (BareName "keys-all")

        keyListPred o = KeySet
          <$> o JD..: "keys"
          <*> (fromMaybe defPred <$> o JD..:? "pred")

        keyListOnly = KeySet
          <$> JD.parseJSON v
          <*> pure defPred

newtype PublicKeyText = PublicKeyText { _pubKey :: Text }
  deriving (Eq,Ord,Generic, Show)

instance JD.FromJSON PublicKeyText where
  parseJSON = JD.withText "PublicKey" (return . PublicKeyText)

data Name
  = QName QualifiedName
  | Name BareName
  | DName DynamicName
  | FQName FullyQualifiedName
  deriving (Generic, Show)

instance JD.FromJSON Name where
  parseJSON = JD.withText "Name" $ \t -> case parseName t of
    Left s  -> fail s
    Right n -> return n

parseName :: Text -> Either String Name
parseName = AP.parseOnly (nameParser <* AP.endOfInput)

fullyQualNameParser :: AP.Parser FullyQualifiedName
fullyQualNameParser = do
  qualifier <- ident style
  mname <- dot *> ident style
  oname <- optional (dot *> ident style)
  h <- dot *> (between (char '{') (char '}') $ some (alphaNum <|> char '-' <|> char '_'))
  hash' <- case decodeBase64UrlUnpadded (T.encodeUtf8 $ T.pack h) of
    Right hash' -> pure $ toShort hash'
    Left _ -> fail "invalid hash encoding"
  case oname of
    Just nn ->
      pure (FullyQualifiedName nn (ModuleName mname (Just $ NamespaceName qualifier)) (Hash hash'))
    Nothing ->
      pure (FullyQualifiedName mname (ModuleName qualifier Nothing) (Hash hash'))

qualifiedNameParser :: (TokenParsing m, Monad m) => m QualifiedName
qualifiedNameParser = do
  a <- ident style
  b <- dot *> ident style
  c <- optional (dot *> ident style)
  case c of
    Nothing -> return (QualifiedName (ModuleName a Nothing) b) <?> "qualified name"
    Just c' -> return (QualifiedName (ModuleName b (Just . NamespaceName $ a)) c') <?> "namespaced qualified name"

symbols :: CharParsing m => m Char
symbols = oneOf "%#+-_&$@<>=^?*!|/~"

style :: CharParsing m => IdentifierStyle m
style = IdentifierStyle "atom"
        (letter <|> symbols)
        (letter <|> digit <|> symbols)
        (HS.fromList ["true","false"])
        Symbol
        ReservedIdentifier

nameParser :: (TokenParsing m, Monad m) => m Name
nameParser = (QName <$> qualifiedNameParser <?> "qualifiedName") <|>
               (Name <$> bareNameParser <?> "bareName")
  where
    bareNameParser = BareName <$> ident style

data QualifiedName = QualifiedName
  { _qnQual :: ModuleName
  , _qnName :: Text
  } deriving (Generic,Show)

instance Eq QualifiedName where
  (QualifiedName a b) == (QualifiedName d e) =
    a == d && b == e
instance Ord QualifiedName where
  (QualifiedName a b) `compare` (QualifiedName d e) =
    (a,b) `compare` (d,e)

instance JD.FromJSON QualifiedName where
  parseJSON = JD.withText "QualifiedName" $ \t ->
    case parseQualifiedName t of
      Left s  -> fail s
      Right n -> return n

parseQualifiedName :: Text -> Either String QualifiedName
parseQualifiedName = AP.parseOnly (qualifiedNameParser <* AP.endOfInput)

data BareName = BareName
  { _bnName :: Text
  } deriving (Generic,Eq,Show)


data DynamicName = DynamicName
    { _dynMember :: !Text
    , _dynRefArg :: !Text
    , _dynInterfaces :: !(Set ModuleName)
    } deriving (Generic,Eq,Show)

instance JD.FromJSON DynamicName where
  parseJSON = JD.withObject "DynamicName" $ \o ->
    DynamicName
      <$> o JD..: "Member"
      <*> o JD..: "RefArg"
      <*> o JD..: "Interfaces"

data FullyQualifiedName
  = FullyQualifiedName
  { _fqName :: !Text
  , _fqModule :: !ModuleName
  , _fqModuleHash :: !Hash
  } deriving (Generic, Eq, Show)

instance JD.FromJSON FullyQualifiedName where
  parseJSON = JD.withText "FullyQualifiedName" $ \f ->
    case AP.parseOnly (fullyQualNameParser <* AP.endOfInput) f of
      Left s  -> fail s
      Right n -> return n

data ModuleGuard = ModuleGuard
  { _mgModuleName :: !ModuleName
  , _mgName :: !Text
  } deriving (Eq,Generic,Show,Ord)

instance JD.FromJSON ModuleGuard where
  parseJSON = JD.withObject "ModuleGuard" $ \o ->
    ModuleGuard
    <$> o JD..: "ModuleName"
    <*> o JD..: "Name"

data UserGuard a = UserGuard
  { _ugFun :: !Name
  , _ugArgs :: ![a]
  } deriving (Eq,Generic,Show,Functor,Foldable,Traversable, Ord)

instance Eq Name where
  (QName (QualifiedName a b)) == (QName (QualifiedName c d)) =
    (a,b) == (c,d)
  (Name (BareName a)) == (Name (BareName b)) =
    a == b
  (DName (DynamicName a b c)) == (DName (DynamicName d e f)) =
    (a,b,c) == (d,e,f)
  _ == _ = False
instance Ord Name where
  (QName (QualifiedName a b)) `compare` (QName (QualifiedName c d)) =
    (a,b) `compare` (c,d)
  (Name (BareName a)) `compare` (Name (BareName b)) =
    a `compare` b
  (DName (DynamicName a b c)) `compare` (DName (DynamicName d e f)) =
    (a,b,c) `compare` (d,e,f)
  (FQName f) `compare` (FQName f') =
    f `compare` f'
  Name {} `compare` _ = LT
  QName {} `compare` DName {} = LT
  QName {} `compare` FQName {} = LT
  QName {} `compare` Name {} = GT
  FQName {} `compare` Name {} = GT
  FQName {} `compare` QName {} = GT
  FQName {} `compare` DName {} = LT
  DName {} `compare` _ = GT

instance Ord FullyQualifiedName where
  (FullyQualifiedName fq fm fh) `compare` (FullyQualifiedName fq' fm' fh') =
    (fq, fm, fh) `compare` (fq', fm', fh')

instance JD.FromJSON a => JD.FromJSON (UserGuard a) where
  parseJSON = JD.withObject "UserGuard" $ \o ->
    UserGuard
    <$> o JD..: "fun"
    <*> o JD..: "args"

data CapabilityGuard n = CapabilityGuard
    { _cgName :: !QualifiedName
    , _cgArgs :: ![n]
    , _cgPactId :: !(Maybe PactId)
    }
  deriving (Eq,Show,Generic,Functor,Foldable,Traversable, Ord)

instance JD.FromJSON a => JD.FromJSON (CapabilityGuard a) where
  parseJSON = JD.withObject "CapabilityGuard" $ \o ->
    CapabilityGuard
    <$> o JD..: "cgName"
    <*> o JD..: "cgArgs"
    <*> o JD..:? "cgPactId"



instance Applicative Term where
    pure a = TVar a
    (<*>) = ap

instance Monad Term where
    return = pure
    TModule m b >>= f = TModule (fmap (>>= f) m) (b >>>= f)
    TList bs t >>= f = TList (V.map (>>= f) bs) (fmap (>>= f) t)
    TDef (Def n m dt ft b dm) >>= f =
      TDef (Def n m dt (fmap (>>= f) ft) (b >>>= f) (fmap (fmap (>>= f)) dm))
    TConst d m c >>= f = TConst (fmap (>>= f) d) m (fmap (>>= f) c)
    TApp a >>= f = TApp (fmap (>>= f) a)
    TVar n >>= f = (f n)
    TBinding bs b c >>= f =
      TBinding (map (fmap (>>= f)) bs) (b >>>= f) (fmap (fmap (>>= f)) c)
    TLam (Lam arg ty b) >>= f =
      TLam (Lam arg (fmap (>>= f) ty) (b >>>= f))
    TObject (Object bs t kf) >>= f =
      TObject (Object (fmap (>>= f) bs) (fmap (>>= f) t) kf )
    TLiteral l >>= _ = TLiteral l
    TGuard g >>= f = TGuard (fmap (>>= f) g)
    TUse u >>= _ = TUse u
    TStep (Step ent e r) >>= f =
      TStep (Step (fmap (>>= f) ent) (e >>= f) (fmap (>>= f) r))
    TSchema {..} >>= f =
      TSchema _tSchemaName _tModule (fmap (fmap (>>= f)) _tFields)
    TTable {..} >>= f =
      TTable _tTableName _tModuleName _tHash (fmap (>>= f) _tTableType)
    TDynamic r m>>= f = TDynamic (r >>= f) (m >>= f)
    TModRef mr >>= _ = TModRef mr

-- Persistence

data PactValue
  = PLiteral Literal
  | PList (Vector PactValue)
  | PObject (ObjectMap PactValue)
  | PGuard (Guard PactValue)
  | PModRef ModRef
  deriving (Eq,Show,Generic,Ord)


instance JD.FromJSON PactValue where
  parseJSON v =
    (PLiteral <$> JD.parseJSON v) <|>
    (PList <$> JD.parseJSON v) <|>
    (PGuard <$> JD.parseJSON v) <|>
    (PModRef <$> (parseNoInfo v <|> JD.parseJSON v)) <|>
    (PObject <$> JD.parseJSON v)
    where
      parseNoInfo = JD.withObject "ModRef" $ \o -> ModRef
        <$> o JD..: "refName"
        <*> o JD..: "refSpec"

type PersistModuleData = ModuleData (Ref' PersistDirect)

data PersistDirect =
    PDValue !PactValue
  | PDNative !NativeDefName
  | PDFreeVar !FullyQualifiedName
  deriving (Eq,Show,Generic)


instance JD.FromJSON PersistDirect where
  parseJSON v =
    JD.withObject "PDFreeVar" (\o -> PDFreeVar <$> o JD..: "pdfv") v <|>
    JD.withObject "PDValue" (\o -> PDValue <$> o JD..: "pdval") v <|>
    JD.withObject "PDNative" (\o -> PDNative <$> o JD..: "pdnat") v


instance JD.FromJSON (Ref' PersistDirect) where
  parseJSON v =
    JD.withObject "Direct" (\o -> Direct <$> o JD..: "direct") v <|>
        JD.withObject "Ref" (\o -> Ref <$> o JD..: "ref") v
  {-# INLINE parseJSON #-}

fromPactValue :: PactValue -> Term Name
fromPactValue (PLiteral l) = TLiteral l
fromPactValue (PObject o) = TObject (Object (fmap fromPactValue o) TyAny Nothing)
fromPactValue (PList l) = TList (fmap fromPactValue l) TyAny
fromPactValue (PGuard x) = TGuard (fmap fromPactValue x)
fromPactValue (PModRef r) = TModRef r

fromPersistDirect :: (NativeDefName -> Maybe (Term Name)) -> PersistDirect -> Either Text (Term Name)
fromPersistDirect _ (PDValue v) = return $! fromPactValue v
fromPersistDirect _ (PDFreeVar f) = return $ TVar (FQName f)
fromPersistDirect natLookup (PDNative nn) = case natLookup nn of
  Just t -> return t
  Nothing -> Left  "Native lookup failed"

allModuleExports :: ModuleData Ref -> HM.HashMap FullyQualifiedName Ref
allModuleExports md = case _mdModule md of
  MDModule m ->
    let toFQ k = FullyQualifiedName k (_mName m) (_mhHash (_mHash m))
    in HM.mapKeys toFQ (_mdRefMap md) `HM.union` (_mdDependencies md)
  _ -> HM.empty

data Namespace a = Namespace
  { _nsName :: !NamespaceName
  , _nsUser :: !(Guard a)
  , _nsAdmin :: !(Guard a)
  } deriving (Eq, Show, Generic)

instance JD.FromJSON a => JD.FromJSON (Namespace a) where
  parseJSON = JD.withObject "Namespace" $ \o ->
    Namespace
    <$> o JD..: "name"
    <*> o JD..: "user"
    <*> o JD..: "admin"


newtype ChainId = ChainId { _chainId :: Text }
  deriving stock (Eq, Generic)
  deriving newtype
    ( Show, JD.FromJSON )

data Provenance = Provenance
  { _pTargetChainId :: !ChainId
    -- ^ the target chain id for the endorsement
  , _pModuleHash :: ModuleHash
    -- ^ a hash of current containing module
  } deriving (Eq, Show, Generic)

instance JD.FromJSON Provenance where
  parseJSON = JD.withObject "Provenance" $ \o ->
    Provenance <$> o JD..: "targetChainId" <*> o JD..: "moduleHash"

data Yield = Yield
  { _yData :: !(ObjectMap PactValue)
    -- ^ Yield data from the pact continuation
  , _yProvenance :: !(Maybe Provenance)
    -- ^ Provenance data
  , _ySourceChain :: !(Maybe ChainId)
  } deriving (Eq, Show, Generic)


instance JD.FromJSON Yield where
  parseJSON = JD.withObject "Yield" $ \o ->
    Yield <$> o JD..: "data" <*> o JD..: "provenance" <*> o JD..:? "source"

data PactContinuation = PactContinuation
  { _pcDef :: Name
  , _pcArgs :: [PactValue]
  } deriving (Eq, Show, Generic)

instance JD.FromJSON PactContinuation where
  parseJSON = JD.withObject "PactContinuation" $ \o ->
    PactContinuation <$> o JD..: "def" <*> o JD..: "args"

data PactStep = PactStep
  { _psStep :: !Int
    -- ^ intended step to execute
  , _psRollback :: !Bool
    -- ^ rollback
  , _psPactId :: !PactId
    -- ^ pact id
  , _psResume :: !(Maybe Yield)
    -- ^ resume value. Note that this is only set in Repl tests and in private use cases;
    -- in all other cases resume value comes out of PactExec.
} deriving (Eq,Show)

data PactExec = PactExec
  { _peStepCount :: Int
    -- ^ Count of steps in pact (discovered when code is executed)
  , _peYield :: !(Maybe Yield)
    -- ^ Yield value if invoked
  , _peExecuted :: Maybe Bool
    -- ^ Only populated for private pacts, indicates if step was executed or skipped.
  , _peStep :: Int
    -- ^ Step that was executed or skipped
  , _pePactId :: PactId
    -- ^ Pact id. On a new pact invocation, is copied from tx id.
  , _peContinuation :: PactContinuation
    -- ^ Strict (in arguments) application of pact, for future step invocations.
  , _peStepHasRollback :: !Bool
    -- ^ Track whether a current step has a rollback
  , _peNested :: M.Map PactId NestedPactExec
    -- ^ Track whether a current step has nested defpact evaluation results
  } deriving (Eq, Show, Generic)

data NestedPactExec = NestedPactExec
  { _npeStepCount :: Int
    -- ^ Count of steps in pact (discovered when code is executed)
  , _npeYield :: !(Maybe Yield)
    -- ^ Yield value if invoked
  , _npeExecuted :: Maybe Bool
    -- ^ Only populated for private pacts, indicates if step was executed or skipped.
  , _npeStep :: Int
    -- ^ Step that was executed or skipped
  , _npePactId :: PactId
    -- ^ Pact id. On a new pact invocation, is copied from tx id.
  , _npeContinuation :: PactContinuation
    -- ^ Strict (in arguments) application of pact, for future step invocations.
  , _npeNested :: M.Map PactId NestedPactExec
    -- ^ Track whether a current step has nested defpact evaluation results
  } deriving (Eq, Show, Generic)

instance JD.FromJSON NestedPactExec where
  parseJSON = JD.withObject "NestedPactExec" $ \o ->
    NestedPactExec
      <$> o JD..: "stepCount"
      <*> o JD..: "yield"
      <*> o JD..: "executed"
      <*> o JD..: "step"
      <*> o JD..: "pactId"
      <*> o JD..: "continuation"
      <*> o JD..: "nested"

instance JD.FromJSON PactExec where
  parseJSON = JD.withObject "PactExec" $ \o ->
    PactExec
      <$> o JD..: "stepCount"
      <*> o JD..: "yield"
      <*> o JD..: "executed"
      <*> o JD..: "step"
      <*> o JD..: "pactId"
      <*> o JD..: "continuation"
      <*> o JD..: "stepHasRollback"
      <*> (fromMaybe mempty <$> o JD..:? "nested")


-- RowData
data RowData = RowData
    { _rdVersion :: !RowDataVersion
    , _rdData :: !(ObjectMap RowDataValue)
    }
  deriving (Eq,Show,Generic,Ord)

instance JD.FromJSON RowData where
  parseJSON v =
    parseVersioned v <|>
    -- note: Parsing into `OldPactValue` here defaults to the code used in
    -- the old FromJSON instance for PactValue, prior to the fix of moving
    -- the `PModRef` parsing before PObject
    RowData RDV0 . fmap oldPactValueToRowData <$> JD.parseJSON v
    where
      oldPactValueToRowData = \case
        OldPLiteral l -> RDLiteral l
        OldPList l -> RDList $ recur l
        OldPObject o -> RDObject $ recur o
        OldPGuard g -> RDGuard $ recur g
        OldPModRef m -> RDModRef m
      recur :: Functor f => f OldPactValue -> f RowDataValue
      recur = fmap oldPactValueToRowData
      parseVersioned = JD.withObject "RowData" $ \o -> RowData
          <$> o JD..: "$v"
          <*> o JD..: "$d"

data OldPactValue
  = OldPLiteral !Literal
  | OldPList !(Vector OldPactValue)
  | OldPObject !(ObjectMap OldPactValue)
  | OldPGuard !(Guard OldPactValue)
  | OldPModRef !ModRef


instance JD.FromJSON OldPactValue where
  parseJSON v =
    (OldPLiteral <$> JD.parseJSON v) <|>
    (OldPList <$> JD.parseJSON v) <|>
    (OldPGuard <$> JD.parseJSON v) <|>
    (OldPObject <$> JD.parseJSON v) <|>
    (OldPModRef <$> (parseNoInfo v <|> JD.parseJSON v))
    where
      parseNoInfo = JD.withObject "ModRef" $ \o -> ModRef
        <$> o JD..: "refName"
        <*> o JD..: "refSpec"

data RowDataVersion = RDV0 | RDV1
  deriving (Eq,Show,Generic,Ord,Enum,Bounded)

instance JD.FromJSON RowDataVersion where
  parseJSON = JD.withScientific "RowDataVersion" $ \case
    0 -> pure RDV0
    1 -> pure RDV1
    _ -> fail "RowDataVersion"

data RowDataValue
    = RDLiteral !Literal
    | RDList !(Vector RowDataValue)
    | RDObject !(ObjectMap RowDataValue)
    | RDGuard !(Guard RowDataValue)
    | RDModRef !ModRef
    deriving (Eq,Show,Generic,Ord)

instance JD.FromJSON RowDataValue where
  parseJSON v1 =
    (RDLiteral <$> JD.parseJSON v1) <|>
    (RDList <$> JD.parseJSON v1) <|>
    parseTagged v1
    where
      parseTagged = JD.withObject "tagged RowData" $ \o -> do
        (t :: Text) <- o JD..: "$t"
        val <- o JD..: "$v"
        case t of
          "o" -> RDObject <$> JD.parseJSON val
          "g" -> RDGuard <$> JD.parseJSON val
          "m" -> RDModRef <$> parseMR val
          _ -> fail "tagged RowData"
      parseMR = JD.withObject "tagged ModRef" $ \o -> ModRef
          <$> o JD..: "refName"
          <*> o JD..: "refSpec"

instance JD.FromJSON n => JD.FromJSON (Term n) where
  parseJSON v = flip (A.<?>) (A.Key "Term") $ case propsWithoutOptionals of
    [TermBody, TermModule] ->  wo "Module" $ \o -> TModule
      <$> o JD..: p TermModule
      <*> o JD..: p TermBody
    [TermList, TermType] -> wo "List" $ \o -> TList
      <$> o JD..: p TermList
      <*> o JD..: p TermType
    [TermDefBody, TermDefMeta, TermDefName, TermDefType, TermFunType, TermMeta, TermModule] -> parseWithInfo TDef
    [TermConstArg, TermConstVal, TermMeta, TermModName] -> wo "Const" $ \o -> TConst
      <$> o JD..: p TermConstArg
      <*> o JD..: p TermModName
      <*> o JD..: p TermConstVal
    [TermArgs, TermFun] -> parseWithInfo TApp
    [TermVar] -> wo "Var" $ \o -> TVar
        <$>  o JD..: p TermVar
    [TermBody, TermPairs, TermType] -> wo "Binding" $ \o -> TBinding
      <$> o JD..: p TermPairs
      <*> o JD..: p TermBody
      <*> o JD..: p TermType
    [TermObjectObj, TermType] -> parseWithInfo TObject -- FIXME keyorder is optional
    [TermLiteral] -> wo "Literal" $ \o -> TLiteral
      <$> o JD..: p TermLiteral
    [TermGuard] -> wo "Guard" $ \o -> TGuard
      <$> o JD..: p TermGuard
    [TermHash, TermModule, TermUseImports] -> parseWithInfo TUse
    [TermLamArg, TermLamBindBody, TermLamInfo, TermLamTy] -> parseWithInfo TLam
    [TermBody, TermMeta] -> wo "Step" $ \o -> TStep
      <$> o JD..: p TermBody
     --  parseWithInfo TStep
    [TermFields, TermMeta, TermModName, TermName] -> wo "Schema" $ \o -> TSchema
      <$>  o JD..: p TermName
      <*> o JD..: p TermModName
      <*> o JD..: p TermFields
    [TermHash, TermMeta, TermModName, TermName, TermType] -> wo "Table" $ \o -> TTable
      <$>  o JD..: p TermName
      <*> o JD..: p TermModName
      <*> o JD..: p TermHash
      <*> o JD..: p TermType
    [TermDynMem, TermDynRef] -> wo "Dynamic" $ \o -> TDynamic
      <$> o JD..: p TermDynRef
      <*> o JD..: p TermDynMem
    [TermModRefInfo, TermModRefName, TermModRefSpec] -> parseWithInfo TModRef
    _ -> fail $ "unexpected properties for Term: "
      <> "[" <> T.unpack (T.intercalate "," (props v)) <> "]"
      <> ", " <> show propsWithoutOptionals
   where
    p = prop
    wo n f = JD.withObject n f v
    props (A.Object m) = A.toText <$> A.keys m
    props _ = []

    propsWithoutOptionals = sort $ unprop
        <$> filter (\x -> x `notElem` ["i", "info", "keyorder"]) (props v)

    parseWithInfo :: JD.FromJSON a => (a -> Term n) -> A.Parser (Term n)
    parseWithInfo f = f <$> JD.parseJSON v

-- Necessary to make this compile
-- Otherwise we get the following error
-- pact/Pact/Core/Serialise/LegacyPact/Types.hs:1861:20: error: [GHC-79890]
--     • ‘Step’ is not in the type environment at a reify
--     • In the untyped splice: $(makeLiftShowsPrec ''Step)
--      |
-- 1861 |   liftShowsPrec = $(makeLiftShowsPrec ''Step)
-- see: https://gitlab.haskell.org/ghc/ghc/-/issues/9813
$( return [] )

instance Show1 Def where
  liftShowsPrec = $(makeLiftShowsPrec ''Def)
instance Show1 Lam where
  liftShowsPrec = $(makeLiftShowsPrec ''Lam)
instance Show1 Object where
  liftShowsPrec = $(makeLiftShowsPrec ''Object)
instance Show1 Term where
  liftShowsPrec = $(makeLiftShowsPrec ''Term)

instance Show1 Type where
  liftShowsPrec = $(makeLiftShowsPrec ''Type)
instance Show1 TypeVar where
  liftShowsPrec = $(makeLiftShowsPrec ''TypeVar)
instance Show1 FunType where
  liftShowsPrec = $(makeLiftShowsPrec ''FunType)
instance Show1 Arg where
  liftShowsPrec = $(makeLiftShowsPrec ''Arg)

instance Show1 Guard where
  liftShowsPrec = $(makeLiftShowsPrec ''Guard)
instance Show1 CapabilityGuard where
  liftShowsPrec = $(makeLiftShowsPrec ''CapabilityGuard)
instance Show1 UserGuard where
  liftShowsPrec = $(makeLiftShowsPrec ''UserGuard)
instance Show1 BindPair where
  liftShowsPrec = $(makeLiftShowsPrec ''BindPair)
instance Show1 App where
  liftShowsPrec = $(makeLiftShowsPrec ''App)
instance Show1 ObjectMap where
  liftShowsPrec = $(makeLiftShowsPrec ''ObjectMap)
instance Show1 BindType where
  liftShowsPrec = $(makeLiftShowsPrec ''BindType)
instance Show1 ConstVal where
  liftShowsPrec = $(makeLiftShowsPrec ''ConstVal)
instance Show1 DefcapMeta where
  liftShowsPrec = $(makeLiftShowsPrec ''DefcapMeta)
instance Show1 DefMeta where
  liftShowsPrec = $(makeLiftShowsPrec ''DefMeta)
instance Show1 ModuleDef where
  liftShowsPrec = $(makeLiftShowsPrec ''ModuleDef)
instance Show1 Module where
  liftShowsPrec = $(makeLiftShowsPrec ''Module)
instance Show1 Governance where
  liftShowsPrec = $(makeLiftShowsPrec ''Governance)
instance Show1 Step where
  liftShowsPrec = $(makeLiftShowsPrec ''Step)
