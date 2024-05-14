{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StrictData #-}


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
 , objectDataToRowData
 , rowDataToObjectData
 ) where

import Control.Lens
import Control.Exception(throwIO, Exception)
import Control.Applicative((<|>))
import Data.Default
import Data.Map.Strict(Map)
import Control.DeepSeq
import GHC.Generics
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
import Data.ByteString (ByteString)

import Data.Dynamic (Typeable)

-- | Modules as they are stored in our backend.
data ModuleData b i
  = ModuleData (EvalModule b i) (Map FullyQualifiedName (EvalDef b i))
  | InterfaceData (EvalInterface b i) (Map FullyQualifiedName (EvalDef b i))
  deriving (Show, Eq, Functor, Generic)

instance (NFData b, NFData i) => NFData (ModuleData b i)

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

-- | Data reflecting Key/Value storage in user-tables.
newtype RowData
  = RowData { _unRowData :: Map Field PactValue }
  deriving (Eq, Show)

objectDataToRowData :: ObjectData PactValue -> RowData
objectDataToRowData (ObjectData obj) = RowData obj
{-# INLINE objectDataToRowData #-}

rowDataToObjectData :: RowData -> ObjectData PactValue
rowDataToObjectData (RowData o) = ObjectData o
{-# INLINE rowDataToObjectData #-}

-- -------------------------------------------------------------------------- --
-- ExecutionMode

-- | Semantics for running transactions.
data ExecutionMode
  = Transactional
    -- ^ `beginTx` and `commitTx` atomically commit actions to the database.
  | Local
    -- ^ `beginTx` and `commitTx` have no effect to the database.
  deriving (Eq,Show, Generic)

instance NFData ExecutionMode

-- | Identifier for transactions
newtype TxId = TxId { _txId :: Word64 }
    deriving (Eq,Ord, Show, NFData)

-- | Transaction record.
--
data TxLog v
  = TxLog
  { _txDomain :: !Text
  , _txKey :: !Text
  , _txValue :: !v
  }
  deriving (Eq,Show,Functor, Foldable, Traversable)

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
  deriving (Eq,Ord,Show,Enum,Bounded, Generic)

instance NFData WriteType

-- | Specify key and value types for database domains.
data Domain k v b i where
  -- | User tables accept a TableName and map to an 'ObjectMap PactValue'
  DUserTables :: !TableName -> Domain RowKey RowData b i
  -- | Keysets
  DKeySets :: Domain KeySetName KeySet b i
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
  deriving (Eq,Show,Ord,Bounded,Enum, Generic)

instance NFData Purity

-- | Fun-record type for Pact back-ends.
data PactDb b i
  = PactDb
  { _pdbPurity :: !Purity
  , _pdbRead :: forall k v. Domain k v b i -> k -> IO (Maybe v)
  , _pdbWrite :: forall k v. WriteType -> Domain k v b i -> k -> v -> IO ()
  , _pdbKeys :: forall k v. Domain k v b i -> IO [k]
  , _pdbCreateUserTable :: TableName -> IO ()
  , _pdbBeginTx :: ExecutionMode -> IO (Maybe TxId)
  , _pdbCommitTx :: IO [TxLog ByteString]
  , _pdbRollbackTx :: IO ()
  , _pdbTxIds :: TableName -> TxId -> IO [TxId]
  , _pdbGetTxLog :: TableName -> TxId -> IO [TxLog RowData]
  }

instance NFData (PactDb b i) where
  -- Note: CommitTX and RollbackTx cannot be rnf'd
  rnf (PactDb purity r w k cut btx ctx rtx tids txl) =
    rnf purity `seq` rnf r `seq` rnf w `seq` rnf k `seq` rnf cut
       `seq` rnf btx `seq` ctx `seq` rtx `seq` rnf tids `seq` rnf txl

makeClassy ''PactDb

-- Potentially new Pactdb abstraction
-- That said: changes in `Purity` that restrict read/write
-- have to be done for all read functions.
readModule :: PactDb b i -> ModuleName -> IO (Maybe (ModuleData b i))
readModule pdb = _pdbRead pdb DModules

writeModule :: PactDb b i -> WriteType -> ModuleName -> ModuleData b i -> IO ()
writeModule pdb wt = _pdbWrite pdb wt DModules

readKeySet :: PactDb b i -> KeySetName -> IO (Maybe KeySet)
readKeySet pdb = _pdbRead pdb DKeySets

writeKeySet :: PactDb b i -> WriteType -> KeySetName -> KeySet -> IO ()
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
  | RowFoundException TableName RowKey
  | NoRowFound TableName RowKey
  | NoSuchTable TableName
  | TableAlreadyExists TableName
  | TxAlreadyBegun TxId
  | NoTxToCommit
  | NoTxLog TableName TxId
  | OpDisallowed
  | MultipleRowsReturnedFromSingleWrite
  deriving (Show, Eq, Typeable, Generic)

instance NFData DbOpException

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
  } deriving (Show, Generic)

instance (NFData b, NFData i) => NFData (Loaded b i)

makeClassy ''Loaded

instance Semigroup (Loaded b i) where
  (Loaded ms tl ns al) <> (Loaded ms' tl' ns' al') =
    Loaded (ms <> ms') (tl <> tl') (ns <|> ns') (al <> al')

instance Monoid (Loaded b i) where
  mempty = Loaded mempty mempty Nothing mempty

instance Default (Loaded b i) where
  def = Loaded mempty mempty Nothing mempty

-- | Map the user's table name into a set of names suitable for
--   storage in the persistence backend (prefix USER_ and the module name
--   to avoid conflicts with any system tables).
toUserTable :: TableName -> Text
toUserTable (TableName tbl mn) = "USER_" <> renderModuleName mn <> "_" <> tbl
