{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies #-}


module Pact.Core.Persistence
 ( ModuleData(..)
 , PactDb(..)
 , Loaded(..)
 , HasLoaded(..)
 , HasPactDb(..)
 , Domain(..)
 , WriteType(..)
 , Purity(..)
 , RowData(..)
 , ExecutionMode(..)
 , mdModuleName
 , mdModuleHash
 , readModule, writeModule
 , readKeySet, writeKeySet
 , readDefPacts, writeDefPacts
 , readNamespace, writeNamespace
 , GuardTableOp(..)
 , DbOpException(..)
 , TxId(..)
 , TxLog(..)
 , dbOpDisallowed
 , toUserTable
 , FQKS
 ) where

import Control.Lens
import Control.Exception(throwIO, Exception)
import Control.Applicative((<|>))
import Data.Default
import Data.Map.Strict(Map)
import Data.Text(Text)
import Data.Word(Word64)

import Pact.Core.Type
import Pact.Core.Names
import Pact.Core.IR.Term
import Pact.Core.Guards
import Pact.Core.Hash
import Pact.Core.PactValue
import Pact.Core.DefPacts.Types
import Pact.Core.Namespace

import Data.Dynamic (Typeable)

-- | Modules as they are stored
-- in our backend.
-- That is: All module definitions, as well as their dependencies
-- Todo: bikeshed this name? This contains interface data
data ModuleData b i
  = ModuleData (EvalModule b i) (Map FullyQualifiedName (EvalDef b i))
  | InterfaceData (EvalInterface b i) (Map FullyQualifiedName (EvalDef b i))
  deriving (Show, Eq, Functor)

mdModuleName :: Lens' (ModuleData b i) ModuleName
mdModuleName f = \case
  ModuleData ev deps ->
    mName f ev <&> \ev' -> ModuleData ev' deps
  InterfaceData iface deps ->
    ifName f iface <&> \ev' -> InterfaceData ev' deps

mdModuleHash :: Lens' (ModuleData b i) ModuleHash
mdModuleHash f = \case
  ModuleData ev deps ->
    mHash f ev <&> \ev' -> ModuleData ev' deps
  InterfaceData iface deps ->
    ifHash f iface <&> \ev' -> InterfaceData ev' deps

type FQKS = KeySet FullyQualifiedName

newtype RowData
  = RowData (Map Field PactValue)
  deriving (Eq, Show)

-- -------------------------------------------------------------------------- --
-- ExecutionMode

data ExecutionMode
  = Transactional
  | Local
  deriving (Eq,Show)

newtype TxId = TxId { _txId :: Word64 }
    deriving (Eq,Ord, Show)

-- | Transaction record.
data TxLog v
  = TxLog
  { _txDomain :: !Text
  , _txKey :: !Text
  , _txValue :: !v
  }
  deriving (Eq,Show,Functor, Foldable, Traversable)
-- makeLenses ''TxLog

-- -------------------------------------------------------------------------- --
-- WriteType

-- | Instruction for '_writeRow'.
data WriteType =
  -- | Insert a new row, fail if key already found.
  --   Requires complete row value, enforced by pact runtime.
  Insert |
  -- | Update an existing row, fail if key not found.
  --   Allows incomplete row values.
  Update |
  -- | Update an existing row, or insert a new row if not found.
  --   Requires complete row value, enforced by pact runtime.
  Write
  deriving (Eq,Ord,Show,Enum,Bounded)

-- | Specify key and value types for database domains.
data Domain k v b i where
  -- | User tables accept a TableName and map to an 'ObjectMap PactValue'
  DUserTables :: !TableName -> Domain RowKey RowData b i
  -- | Keysets
  DKeySets :: Domain KeySetName (KeySet FullyQualifiedName) b i
  -- | Modules
  DModules :: Domain ModuleName (ModuleData b i) b i
  -- | Namespaces
  DNamespaces :: Domain NamespaceName Namespace b i
  -- | Pacts map to 'Maybe PactExec' where Nothing indicates
  -- a terminated pact.

  -- | DefPact state, `Nothing` implies DefPact with `DefPactId` is completed.
  DDefPacts :: Domain DefPactId (Maybe DefPactExec) b i

data Purity
  -- | Read-only access to systables.
  = PSysOnly
  -- | Read-only access to systables and module tables.
  | PReadOnly
  -- | All database access allowed (normal).
  | PImpure
  deriving (Eq,Show,Ord,Bounded,Enum)

-- | Fun-record type for Pact back-ends.
data PactDb b i
  = PactDb
  { _pdbPurity :: !Purity
  , _pdbRead :: forall k v. Domain k v b i -> k -> IO (Maybe v)
  , _pdbWrite :: forall k v. WriteType -> Domain k v b i -> k -> v -> IO ()
  , _pdbKeys :: forall k v. Domain k v b i -> IO [k]
  , _pdbCreateUserTable :: TableName -> IO ()
  , _pdbBeginTx :: ExecutionMode -> IO (Maybe TxId)
  , _pdbCommitTx :: IO ()
  , _pdbRollbackTx :: IO ()
  , _pdbTxIds :: TableName -> TxId -> IO [TxId]
  , _pdbGetTxLog :: TableName -> TxId -> IO [TxLog RowData]
  }

makeClassy ''PactDb

-- Potentially new Pactdb abstraction
-- That said: changes in `Purity` that restrict read/write
-- have to be done for all read functions.
readModule :: PactDb b i -> ModuleName -> IO (Maybe (ModuleData b i))
readModule pdb = _pdbRead pdb DModules

writeModule :: PactDb b i -> WriteType -> ModuleName -> ModuleData b i -> IO ()
writeModule pdb wt = _pdbWrite pdb wt DModules

readKeySet :: PactDb b i -> KeySetName -> IO (Maybe FQKS)
readKeySet pdb = _pdbRead pdb DKeySets

writeKeySet :: PactDb b i -> WriteType -> KeySetName -> FQKS -> IO ()
writeKeySet pdb wt = _pdbWrite pdb wt DKeySets

readDefPacts :: PactDb b i -> DefPactId -> IO (Maybe (Maybe DefPactExec))
readDefPacts pdb = _pdbRead pdb DDefPacts

writeDefPacts :: PactDb b i -> WriteType -> DefPactId -> Maybe DefPactExec -> IO ()
writeDefPacts pdb wt = _pdbWrite pdb wt DDefPacts

readNamespace :: PactDb b i -> NamespaceName -> IO (Maybe Namespace)
readNamespace pdb = _pdbRead pdb DNamespaces

writeNamespace :: PactDb b i -> WriteType -> NamespaceName -> Namespace -> IO ()
writeNamespace pdb wt = _pdbWrite pdb wt DNamespaces

data DbOpException
  = WriteException
  | NoSuchTable TableName
  | TableAlreadyExists TableName
  | TxAlreadyBegun TxId
  | NoTxToCommit
  | NoTxLog TableName TxId
  | OpDisallowed
  deriving (Show, Eq, Typeable)

dbOpDisallowed :: IO a
dbOpDisallowed = throwIO OpDisallowed

instance Exception DbOpException

data GuardTableOp
  = GtRead
  | GtSelect
  | GtWithRead
  | GtWithDefaultRead
  | GtKeys
  | GtTxIds
  | GtTxLog
  | GtKeyLog
  | GtWrite
  | GtCreateTable
  deriving Show

-- | Our loaded modules, names in top-level scope and fully qualified dependencies.
data Loaded b i
  = Loaded
  { _loModules :: Map ModuleName (ModuleData b i)
  -- ^ All loaded modules and interfaces
  , _loToplevel :: Map Text (FullyQualifiedName, DefKind)
  -- ^ All names bound @ the top level scope, that is, by `(use)` statements
  -- or module loads
  , _loNamespace :: Maybe Namespace
  -- ^ The potentially loaded current namespace
  , _loAllLoaded :: Map FullyQualifiedName (Def Name Type b i)
  -- ^ All of our fully qualified dependencies
  } deriving Show

makeClassy ''Loaded

instance Semigroup (Loaded b i) where
  (Loaded ms tl ns al) <> (Loaded ms' tl' ns' al') =
    Loaded (ms <> ms') (tl <> tl') (ns <|> ns') (al <> al')

instance Monoid (Loaded b i) where
  mempty = Loaded mempty mempty Nothing mempty

instance Default (Loaded b i) where
  def = Loaded mempty mempty Nothing mempty


toUserTable :: TableName -> Text
toUserTable (TableName tbl mn) = "USER_" <> renderModuleName mn <> "_" <> tbl
