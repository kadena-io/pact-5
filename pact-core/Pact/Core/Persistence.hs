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
 , mockPactDb
 , mdModuleName
 , mdModuleHash
 , readModule, writeModule
 , readKeyset, writeKeySet
 , readPacts, writePacts
 , GuardTableOp(..)
 , DbOpException(..)
 , TxId(..)
 , TxLog(..)
 , dbOpDisallowed
 ) where

import Control.Lens
import Control.Monad(unless)
import Control.Exception(throwIO, Exception)
import Control.Applicative((<|>))
import Data.Maybe(isJust)
import Data.Text(Text)
import Data.Word(Word64)
import Data.IORef
import Data.Default
import Data.Map.Strict(Map)
import Control.DeepSeq
import GHC.Generics

import Pact.Core.Type
import Pact.Core.Names
import Pact.Core.IR.Term
import Pact.Core.Guards
import Pact.Core.Hash
import Pact.Core.PactValue
import Pact.Core.DefPacts.Types
import Pact.Core.Namespace

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
  deriving (Eq,Show,Ord,Bounded,Enum, Generic)

instance NFData Purity

-- | Fun-record type for Pact back-ends.
data PactDb b i
  = PactDb
  { _pdbPurity :: !Purity
  , _pdbRead :: forall k v. Domain k v b i -> k -> IO (Maybe v)
  , _pdbWrite :: forall k v. WriteType -> Domain k v b i -> k -> v -> IO ()
  , _pdbKeys :: forall k v. Domain k v b i -> IO [k]
  , _pdbCreateUserTable :: TableName -> ModuleName -> IO ()
  , _pdbBeginTx :: ExecutionMode -> IO (Maybe TxId)
  , _pdbCommitTx :: IO ()
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

readKeyset :: PactDb b i -> KeySetName -> IO (Maybe FQKS)
readKeyset pdb = _pdbRead pdb DKeySets

writeKeySet :: PactDb b i -> WriteType -> KeySetName -> FQKS -> IO ()
writeKeySet pdb wt = _pdbWrite pdb wt DKeySets

readPacts :: PactDb b i -> DefPactId -> IO (Maybe (Maybe DefPactExec))
readPacts pdb = _pdbRead pdb DDefPacts

writePacts :: PactDb b i -> WriteType -> DefPactId -> Maybe DefPactExec -> IO ()
writePacts pdb wt = _pdbWrite pdb wt DDefPacts

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

mockPactDb :: forall b i. IO (PactDb b i)
mockPactDb = do
  refMod <- newIORef M.empty
  refKs <- newIORef M.empty
  refUsrTbl <- newIORef M.empty
  refPacts <- newIORef M.empty
  refRb <- newIORef Nothing
  refTxLog <- newIORef mempty
  refTxId <- newIORef 0
  refNs <- newIORef mempty
  pure $ PactDb
    { _pdbPurity = PImpure
    , _pdbRead = read' refKs refMod refUsrTbl refPacts refNs
    , _pdbWrite = write refKs refMod refUsrTbl refTxId refTxLog refPacts refNs
    , _pdbKeys = keys refKs refMod refUsrTbl refPacts refNs
    , _pdbCreateUserTable = createUsrTable refUsrTbl refTxLog
    , _pdbBeginTx = beginTx refRb refTxId refTxLog refMod refKs refUsrTbl refNs
    , _pdbCommitTx = commitTx refRb refTxId refTxLog refMod refKs refUsrTbl refNs
    , _pdbRollbackTx = rollbackTx refRb refTxLog refMod refKs refUsrTbl refNs
    , _pdbTxIds = txIds refTxLog
    , _pdbGetTxLog = txLog refTxLog
    }
  where
  beginTx refRb refTxId refTxLog refMod refKs refUsrTbl refNs em = do
    readIORef refRb >>= \case
      Just (_, _, _, _, _, _) -> pure Nothing
      Nothing -> do
        mods <- readIORef refMod
        ks <- readIORef refKs
        usrTbl <- readIORef refUsrTbl
        txl <- readIORef refTxLog
        ns <- readIORef refNs
        writeIORef refRb (Just (em, txl, mods, ks, usrTbl, ns))
        tid <- readIORef refTxId
        pure (Just (TxId tid))

  commitTx refRb refTxId refTxLog refMod refKs refUsrTbl refNs = readIORef refRb >>= \case
    Just (em, txl, mods, ks, usr, ns) -> case em of
      Transactional -> do
        writeIORef refRb Nothing
        modifyIORef' refTxId (+ 1)
      Local -> do
        writeIORef refRb Nothing
        writeIORef refMod mods
        writeIORef refKs ks
        writeIORef refUsrTbl usr
        writeIORef refTxLog txl
        writeIORef refNs ns
    Nothing ->
      throwIO NoTxToCommit

  rollbackTx refRb refTxLog refMod refKs refUsrTbl refNs = readIORef refRb >>= \case
    Just (_, txl, mods, ks, usr, ns) -> do
      writeIORef refRb Nothing
      writeIORef refTxLog txl
      writeIORef refMod mods
      writeIORef refKs ks
      writeIORef refUsrTbl usr
      writeIORef refNs ns
    Nothing -> throwIO NoTxToCommit

  txLog refTxLog tn tid = do
    m <- readIORef refTxLog
    case M.lookup tn m of
      Just txids -> case M.lookup tid txids of
        Just n -> pure n
        Nothing -> throwIO (NoTxLog tn tid)
      Nothing -> throwIO (NoTxLog tn tid)

  txIds refTxLog tn txId = do
    txl <- readIORef refTxLog
    case M.lookup tn txl of
      Just mtxl -> pure [ x | x <- M.keys mtxl, x >= txId ]
      Nothing -> throwIO (NoSuchTable tn)

  keys
    :: forall k v
    .  IORef (Map KeySetName FQKS)
    -> IORef (Map ModuleName (ModuleData b i))
    -> IORef (Map TableName (Map RowKey RowData))
    -> IORef (Map DefPactId (Maybe DefPactExec))
    -> IORef (Map NamespaceName Namespace)
    -> Domain k v b i
    -> IO [k]
  keys refKs refMod refUsrTbl refPacts refNs d = case d of
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
    DDefPacts -> do
      r <- readIORef refPacts
      return (M.keys r)
    DNamespaces -> do
      r <- readIORef refNs
      return (M.keys r)

  createUsrTable
    :: IORef (Map TableName (Map RowKey RowData))
    -> IORef (Map TableName (Map TxId [TxLog RowData]))
    -> TableName
    -> ModuleName
    -> IO ()
  createUsrTable refUsrTbl refTxLog tbl _ = do
    ref <- readIORef refUsrTbl
    case M.lookup tbl ref of
      Nothing -> do
        modifyIORef refTxLog (M.insert tbl mempty)
        modifyIORef refUsrTbl (M.insert tbl mempty)
        pure ()
      Just _ -> throwIO (TableAlreadyExists tbl)

  read'
    :: forall k v
    .  IORef (Map KeySetName FQKS)
    -> IORef (Map ModuleName (ModuleData b i))
    -> IORef (Map TableName (Map RowKey RowData))
    -> IORef (Map DefPactId (Maybe DefPactExec))
    -> IORef (Map NamespaceName Namespace)
    -> Domain k v b i
    -> k
    -> IO (Maybe v)
  read' refKs refMod refUsrTbl refPacts refNS domain k = case domain of
    DKeySets -> readKS refKs k
    DModules -> readMod refMod k
    DUserTables tbl ->
      readRowData refUsrTbl tbl k
    DDefPacts -> readPacts' refPacts k
    DNamespaces ->
      M.lookup k <$> readIORef refNS

  checkTable tbl ref = do
    r <- readIORef ref
    unless (isJust (M.lookup tbl r)) $ throwIO (NoSuchTable tbl)

  write
    :: forall k v
    .  IORef (Map KeySetName FQKS)
    -> IORef (Map ModuleName (ModuleData b i))
    -> IORef (Map TableName (Map RowKey RowData))
    -> IORef Word64
    -> IORef (Map TableName (Map TxId [TxLog RowData]))
    -> IORef (Map DefPactId (Maybe DefPactExec))
    -> IORef (Map NamespaceName Namespace)
    -> WriteType
    -> Domain k v b i
    -> k
    -> v
    -> IO ()
  write refKs refMod refUsrTbl refTxId refTxLog refPacts refNs wt domain k v = case domain of
    DKeySets -> writeKS refKs k v
    DModules -> writeMod refMod v
    DUserTables tbl -> writeRowData refUsrTbl refTxId refTxLog tbl wt k v
    DDefPacts -> writePacts' refPacts k v
    DNamespaces ->
      modifyIORef' refNs (M.insert k v)

  readRowData ref tbl k = do
    checkTable tbl ref
    r <- readIORef ref
    pure (r ^? ix tbl . ix k)

  writeToTxLog
    :: IORef Word64
    -> IORef (Map TableName (Map TxId [TxLog RowData]))
    -> TableName
    -> RowKey
    -> RowData
    -> IO ()
  writeToTxLog refTxId refTxLog tbl k rdata = do
    tid <- readIORef refTxId
    let entry = M.singleton (TxId tid) [TxLog (_tableName tbl) (_rowKey k) rdata]
    modifyIORef' refTxLog (M.insertWith (M.unionWith (<>)) tbl entry)

  writeRowData
    :: IORef (Map TableName (Map RowKey RowData))
    -> IORef Word64
    -> IORef (Map TableName (Map TxId [TxLog RowData]))
    -> TableName
    -> WriteType
    -> RowKey
    -> RowData
    -> IO ()
  writeRowData ref refTxId refTxLog tbl wt k v = checkTable tbl ref *> case wt of
    Write -> do
      writeToTxLog refTxId refTxLog tbl k v
      modifyIORef' ref (M.insertWith M.union tbl (M.singleton k v))
    Insert -> do
      r <- readIORef ref
      case M.lookup tbl r >>= M.lookup k of
        Just _ -> throwIO WriteException
        Nothing -> do
          writeToTxLog refTxId refTxLog tbl k v
          modifyIORef' ref (M.insertWith M.union tbl (M.singleton k v))
    Update -> do
      r <- readIORef ref
      case M.lookup tbl r >>= M.lookup k of
        Just (RowData m) -> do
          let (RowData v') = v
              nrd = RowData (M.union v' m)
          writeToTxLog refTxId refTxLog tbl k nrd
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

  readPacts' ref pid = do
    m <- readIORef ref
    pure (M.lookup pid m)

  writePacts' ref pid pe = modifyIORef' ref (M.insert pid pe)
