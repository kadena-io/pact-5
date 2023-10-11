{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
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
 , mockPactDb
 , mdModuleName
 , mdModuleHash
 , readModule, writeModule
 , readKeyset, writeKeySet
 , GuardTableOp(..)
 , DbOpException(..)
 ) where

import Control.Lens
-- import Control.Monad.State.Class
-- import Control.Monad.IO.Class
-- import Control.Monad.Except
import Control.Monad(unless)
import Control.Exception(throwIO, Exception)
import Data.Maybe(isJust)
import Data.Text(Text)
import Data.IORef
import Data.Map.Strict(Map)

import Pact.Core.Type
import Pact.Core.Names
import Pact.Core.IR.Term
import Pact.Core.Guards
import Pact.Core.Hash
import Pact.Core.PactValue
-- import Pact.Core.Errors

import qualified Data.Map.Strict as M
import Data.Dynamic (Typeable)

-- | Modules as they are stored
-- in our backend.
-- That is: All module definitions, as well as their dependencies
-- Todo: bikeshed this name? This contains interface data
data ModuleData b i
  = ModuleData (EvalModule b i) (Map FullyQualifiedName (EvalDef b i))
  -- { _mdModule :: EvalModule b i
  -- , _mdDependencies :: Map FullyQualifiedName (EvalDef b i)
  -- }
  | InterfaceData (EvalInterface b i) (Map FullyQualifiedName (EvalDef b i))
  deriving Show
  -- { _ifInterface :: EvalInterface b i
  -- , _ifDependencies :: Map FullyQualifiedName (EvalDefConst b i)
  -- } deriving Show

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

data ExecutionMode =
    Transactional |
    Local
    deriving (Eq,Show)

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
  -- Namespaces :: Domain NamespaceName (Namespace PactValue)
  -- | Pacts map to 'Maybe PactExec' where Nothing indicates
  -- a terminated pact.
  -- Pacts :: Domain PactId (Maybe PactExec)

data Purity
  -- | Read-only access to systables.
  = PSysOnly
  -- | Read-only access to systables and module tables.
  | PReadOnly
  -- | All database access allowed (normal).
  | PImpure
  deriving (Eq,Show,Ord,Bounded,Enum)

-- | Fun-record type for Pact back-ends.
-- Todo: `Domain` requires sometimes some really annoying type anns,
-- Do we want to keep this abstraction or go to the monomorphized one?
data PactDb b i
  = PactDb
  { _pdbPurity :: !Purity
  , _pdbRead :: forall k v. Domain k v b i -> k -> IO (Maybe v)
  , _pdbWrite :: forall k v. WriteType -> Domain k v b i -> k -> v -> IO ()
  , _pdbKeys :: forall k v. Domain k v b i -> IO [k]
  , _pdbCreateUserTable :: TableName -> ModuleName -> IO ()
--  , _pdbBeginTx ::
  }

makeClassy ''PactDb

-- Potentially new Pactdb abstraction
-- That said: changes in `Purity` that restrict read/write
-- have to be done for all read functions.
readModule :: PactDb b i -> ModuleName -> IO (Maybe (ModuleData b i))
readModule pdb = _pdbRead pdb DModules

writeModule :: PactDb b i -> WriteType -> ModuleName -> ModuleData b i -> IO ()
writeModule pdb wt = _pdbWrite pdb wt DModules

readKeyset :: PactDb b i -> KeySetName -> IO (Maybe FQKS)
readKeyset pdb = _pdbRead pdb DKeySets

writeKeySet :: PactDb b i -> WriteType -> KeySetName -> FQKS -> IO ()
writeKeySet pdb wt = _pdbWrite pdb wt DKeySets

data DbOpException
  = WriteException
  | NoSuchTable TableName
  | TableAlreadyExists TableName
  deriving (Show, Eq, Typeable)

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

data Loaded b i
  = Loaded
  { _loModules :: Map ModuleName (ModuleData b i)
  , _loToplevel :: Map Text (FullyQualifiedName, DefKind)
  , _loAllLoaded :: Map FullyQualifiedName (Def Name Type b i)
  } deriving Show

makeClassy ''Loaded

instance Semigroup (Loaded b i) where
  (Loaded ms tl al) <> (Loaded ms' tl' al') =
    Loaded (ms <> ms') (tl <> tl') (al <> al')

instance Monoid (Loaded b i) where
  mempty = Loaded mempty mempty mempty

mockPactDb :: forall b i. IO (PactDb b i)
mockPactDb = do
  refMod <- newIORef M.empty
  refKs <- newIORef M.empty
  refUsrTbl <- newIORef M.empty
--  ref <- newIORef
  pure $ PactDb
    { _pdbPurity = PImpure
    , _pdbRead = read' refKs refMod refUsrTbl
    , _pdbWrite = write refKs refMod refUsrTbl
    , _pdbKeys = keys refKs refMod refUsrTbl
    , _pdbCreateUserTable = createUsrTable refUsrTbl
    }
  where
  keys
    :: forall k v
    .  IORef (Map KeySetName FQKS)
    -> IORef (Map ModuleName (ModuleData b i))
    -> IORef (Map TableName (Map RowKey RowData))
    -> Domain k v b i
    -> IO [k]
  keys refKs refMod refUsrTbl d = case d of
    DKeySets -> do
      r <- readIORef refKs
      return (M.keys r)
    DModules -> do
      r <- readIORef refMod
      return (M.keys r)
    DUserTables tbl -> do
      r <- readIORef refUsrTbl
      case M.lookup tbl r of
        Just t -> return (M.keys t)
        Nothing -> throwIO (NoSuchTable tbl)

  createUsrTable
    :: IORef (Map TableName (Map RowKey RowData))
    -> TableName
    -> ModuleName
    -> IO ()
  createUsrTable refUsrTbl tbl _ = do
    ref <- readIORef refUsrTbl
    case M.lookup tbl ref of
      Nothing -> do
        modifyIORef refUsrTbl (M.insert tbl mempty)
        pure ()
      Just _ -> throwIO (TableAlreadyExists tbl)

  read'
    :: forall k v
    .  IORef (Map KeySetName FQKS)
    -> IORef (Map ModuleName (ModuleData b i))
    -> IORef (Map TableName (Map RowKey RowData))
    -> Domain k v b i
    -> k
    -> IO (Maybe v)
  read' refKs refMod refUsrTbl domain k = case domain of
    DKeySets -> readKS refKs k
    DModules -> readMod refMod k
    DUserTables tbl ->
      readRowData refUsrTbl tbl k

  checkTable tbl ref = do
    r <- readIORef ref
    unless (isJust (M.lookup tbl r)) $ throwIO (NoSuchTable tbl)

  write
    :: forall k v
    .  IORef (Map KeySetName FQKS)
    -> IORef (Map ModuleName (ModuleData b i))
    -> IORef (Map TableName (Map RowKey RowData))
    -> WriteType
    -> Domain k v b i
    -> k
    -> v
    -> IO ()
  write refKs refMod refUsrTbl wt domain k v = case domain of
    DKeySets -> writeKS refKs k v
    DModules -> writeMod refMod v
    DUserTables tbl -> writeRowData refUsrTbl tbl wt k v

  readRowData ref tbl k = do
    checkTable tbl ref
    r <- readIORef ref
    pure (r ^? ix tbl . ix k)

  writeRowData ref tbl wt k v = checkTable tbl ref *> case wt of
    Write ->
      modifyIORef' ref (M.insertWith M.union tbl (M.singleton k v))
    Insert -> do
      r <- readIORef ref
      case M.lookup tbl r >>= M.lookup k of
        Just _ -> throwIO WriteException
        Nothing ->
          modifyIORef' ref (M.insertWith M.union tbl (M.singleton k v))
    Update -> do
      r <- readIORef ref
      case M.lookup tbl r >>= M.lookup k of
        Just (RowData m) -> do
          let (RowData v') = v
              nrd = RowData (M.union v' m)
          modifyIORef' ref (M.insertWith M.union tbl (M.singleton k nrd))
        Nothing -> throwIO WriteException


  readKS ref ksn = do
    m <- readIORef ref
    pure (M.lookup ksn m)

  writeKS ref ksn ks = modifyIORef' ref (M.insert ksn ks)

  readMod ref mn = do
    m <- readIORef ref
    pure (M.lookup mn m)

  writeMod ref md = let
    mname = view mdModuleName md
    in modifyIORef' ref (M.insert mname md)
