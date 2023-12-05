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
 , TxId(..)
 , TxLog(..)
 , toUserTable
 , FQKS
 ) where

import Control.Lens
import Control.Monad.IO.Class (MonadIO)
import Control.Exception(throwIO, Exception)
import Control.Applicative((<|>))
import Data.Default
import Data.IORef (IORef)
import Data.Map.Strict(Map)
import Data.Text(Text)
import Data.Word(Word64)
import Pact.Core.Errors (dbOpDisallowed)

import Pact.Core.Environment.State (TxId(..), Loaded(..), esLoaded, HasLoaded(..), ModuleData(..), MonadEvalState)
import Pact.Core.Type
import Pact.Core.Names
import Pact.Core.IR.Term
import Pact.Core.Guards
import Pact.Core.Hash
import Pact.Core.PactValue
import Pact.Core.DefPacts.Types
import Pact.Core.Namespace

import Data.Dynamic (Typeable)


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
data PactDb m b i
  = PactDb
  { _pdbPurity :: !Purity
  , _pdbRead :: forall k v. Domain k v b i -> k -> m (Maybe v)
  , _pdbWrite :: forall k v. WriteType -> Domain k v b i -> k -> v -> m ()
  , _pdbKeys :: forall k v. Domain k v b i -> m [k]
  , _pdbCreateUserTable :: TableName -> m ()
  , _pdbBeginTx :: ExecutionMode -> m (Maybe TxId)
  , _pdbCommitTx :: m ()
  , _pdbRollbackTx :: m ()
  , _pdbTxIds :: TableName -> TxId -> m [TxId]
  , _pdbGetTxLog :: TableName -> TxId -> m [TxLog RowData]
  }


makeClassy ''PactDb

-- Potentially new Pactdb abstraction
-- That said: changes in `Purity` that restrict read/write
-- have to be done for all read functions.
readModule :: PactDb m b i -> ModuleName -> m (Maybe (ModuleData b i))
readModule pdb = _pdbRead pdb DModules

writeModule :: PactDb m b i -> WriteType -> ModuleName -> ModuleData b i -> m ()
writeModule pdb wt = _pdbWrite pdb wt DModules

readKeySet :: PactDb m b i -> KeySetName -> m (Maybe FQKS)
readKeySet pdb = _pdbRead pdb DKeySets

writeKeySet :: PactDb m b i -> WriteType -> KeySetName -> FQKS -> m ()
writeKeySet pdb wt = _pdbWrite pdb wt DKeySets

readDefPacts :: PactDb m b i -> DefPactId -> m (Maybe (Maybe DefPactExec))
readDefPacts pdb = _pdbRead pdb DDefPacts

writeDefPacts :: PactDb m b i -> WriteType -> DefPactId -> Maybe DefPactExec -> m ()
writeDefPacts pdb wt = _pdbWrite pdb wt DDefPacts

readNamespace :: PactDb m b i -> NamespaceName -> m (Maybe Namespace)
readNamespace pdb = _pdbRead pdb DNamespaces

writeNamespace :: PactDb m b i -> WriteType -> NamespaceName -> Namespace -> m ()
writeNamespace pdb wt = _pdbWrite pdb wt DNamespaces

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



toUserTable :: TableName -> Text
toUserTable (TableName tbl mn) = "USER_" <> renderModuleName mn <> "_" <> tbl
