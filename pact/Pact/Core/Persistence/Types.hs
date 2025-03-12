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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}


module Pact.Core.Persistence.Types
 ( ModuleData(..)
 , GasM(..)
 , PactDb(..)
 , Loaded(..)
 , HasLoaded(..)
 , HasPactDb(..)
 , Domain(..)
 , WriteType(..)
 , Purity(..)
 , RowData(..)
 , _RowData
 , ExecutionMode(..)
 , mdModuleName
 , mdModuleHash
 , mdDependencies
 , GuardTableOp(..)
 , TxId(..)
 , TxLog(..)
 , toUserTable
 , objectDataToRowData
 , rowDataToObjectData
 , renderDomain
 , UserTableInfo(..)
 , hashTxLogs
 ) where

import Control.Applicative((<|>))
import Control.Lens
import Data.Default
import Data.Map.Strict(Map)
import Control.DeepSeq
import Control.Exception.Safe
import GHC.Generics
import Data.Text(Text)
import Data.Word(Word64)

import Pact.Core.Gas.Types
import Pact.Core.Type
import Pact.Core.Names
import Pact.Core.IR.Term
import Pact.Core.Guards
import Pact.Core.Hash
import Pact.Core.PactValue
import Pact.Core.DefPacts.Types
import Pact.Core.Namespace
import Data.ByteString (ByteString)

import Pact.Core.Errors
import Pact.Core.StackFrame
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Text.Encoding as T

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

mdDependencies :: Lens' (ModuleData b i) (Map FullyQualifiedName (EvalDef b i))
mdDependencies f = \case
  ModuleData ev deps ->
    ModuleData ev <$> f deps
  InterfaceData iface deps ->
    InterfaceData iface <$> f deps

-- | Data reflecting Key/Value storage in user-tables.
newtype RowData
  = RowData { _unRowData :: Map Field PactValue }
  deriving stock (Eq, Show)
  deriving newtype NFData

makePrisms ''RowData

type instance Index RowData = Field
type instance IxValue RowData = PactValue
instance Ixed RowData where
  ix k = _RowData . ix k
instance At RowData where
  at k = _RowData . at k

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
    deriving stock (Eq,Ord,Show,Generic)
    deriving newtype NFData

-- | Transaction record.
--
data TxLog v
  = TxLog
  { _txDomain :: !Text
  , _txKey :: !Text
  , _txValue :: !v
  }
  deriving stock (Eq, Generic, Show, Functor, Foldable, Traversable)
  deriving anyclass NFData


hashTxLogs :: [TxLog ByteString] -> Hash
hashTxLogs logs = pactHash $ mconcat
  [ "D" <> T.encodeUtf8 _txDomain <> "K" <> T.encodeUtf8 _txKey <> "V" <> _txValue
  | TxLog{..} <- logs
  ]

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
  -- | Module source
  DModuleSource :: Domain HashedModuleName ModuleCode b i

deriving stock instance Show (Domain k v b i)

data Purity
  -- | Read-only access to systables.
  = PSysOnly
  -- | Read-only access to systables and module tables.
  | PReadOnly
  -- | All database access allowed (normal).
  | PImpure
  deriving (Eq,Show,Ord,Bounded,Enum, Generic)

instance NFData Purity

newtype GasM b i a
  = GasM { runGasM :: ReaderT (GasEnv b i, i, [StackFrame i]) (ExceptT (PactError i) IO) a }
  deriving newtype
  ( Functor
  , Applicative
  , Monad
  , MonadReader (GasEnv b i, i, [StackFrame i])
  , MonadError (PactError i)
  , MonadThrow
  , MonadCatch
  , MonadMask
  , MonadIO
  , MonadFail
  )



-- | Fun-record type for Pact back-ends.
-- b: The type of builtin functions.
-- i: The type of Info (usually SpanInfo or ()).
data PactDb b i
  = PactDb
  { _pdbPurity :: !Purity
  , _pdbRead :: forall k v. Domain k v b i -> k -> GasM b i (Maybe v)
  , _pdbWrite :: forall k v. WriteType -> Domain k v b i -> k -> v -> GasM b i ()
  , _pdbKeys :: forall k v. Domain k v b i -> GasM b i [k]
  , _pdbCreateUserTable :: TableName -> GasM b i ()
  , _pdbBeginTx :: ExecutionMode -> GasM b i (Maybe TxId)
  , _pdbCommitTx :: GasM b i [TxLog ByteString]
  , _pdbRollbackTx :: GasM b i ()
  }

instance NFData (PactDb b i) where
  -- Note: CommitTX and RollbackTx cannot be rnf'd
  rnf (PactDb purity r _ k cut btx ctx rtx) =
    rnf purity `seq` rnf r `seq` rnf k `seq` rnf cut
       `seq` rnf btx `seq` ctx `seq` rtx `seq` ()

makeClassy ''PactDb




data GuardTableOp
  = GtRead
  | GtSelect
  | GtKeys
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

newtype UserTableInfo
  = UserTableInfo ModuleName
  deriving (Eq, Show)



-- | Map the user's table name into a set of names suitable for
--   storage in the persistence backend.
--   Note: we don't use the USER_ prefix from production pact
--   since this is how chainweb table names are made.
toUserTable :: TableName -> Text
toUserTable (TableName tbl mn) = renderModuleName mn <> "_" <> tbl

renderDomain :: Domain k v b i -> Text
renderDomain = \case
  DUserTables tbl -> toUserTable tbl
  DKeySets -> "SYS:KeySets"
  DModules -> "SYS:Modules"
  DNamespaces -> "SYS:Namespaces"
  DDefPacts -> "SYS:Pacts"
  DModuleSource -> "SYS:ModuleSources"
