{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}


module Pact.Core.Persistence
 ( ModuleData(..)
 , PactDb(..)
 , Loaded(..)
 , loModules
 , loToplevel
 , loAllLoaded
 , mockPactDb
 , mdModuleName
 , mdModuleHash
 , readModule, writeModule
 , readKeyset, writeKeySet
 ) where

import Control.Lens
import Data.Text(Text)
import Data.IORef
import Data.Map.Strict(Map)

import Pact.Core.Names
import Pact.Core.IR.Term
import Pact.Core.Guards
import Pact.Core.Hash

import qualified Data.Map.Strict as Map

-- | Modules as they are stored
-- in our backend.
-- That is: All module definitions, as well as
data ModuleData b i
  = ModuleData (Module Name b i) (Map FullyQualifiedName (Def Name b i))
  -- { _mdModule :: EvalModule b i
  -- , _mdDependencies :: Map FullyQualifiedName (EvalDef b i)
  -- }
  | InterfaceData (Interface Name b i) (Map FullyQualifiedName (Def Name b i))
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

-- | Specify key and value types for database domains.
data Domain k v b i where
  -- | User tables accept a TableName and map to an 'ObjectMap PactValue'
  -- UserTables :: !TableName -> Domain RowKey RowData
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
  , _pdbWrite :: forall k v. Domain k v b i -> k -> v -> IO ()
  }

-- Potentially new Pactdb abstraction
-- That said: changes in `Purity` that restrict read/write
-- have to be done for all read functions.
-- data PactDb b i
--   = PactDb
--   { _pdbPrity :: !Purity
--   , _pdbReadModule :: ModuleName -> IO (Maybe (ModuleData b i))
--   -- ^ Look up module by module name
--   , _pdbWriteModule :: ModuleData b i -> IO ()
--   -- ^ Save a module
--   , _pdbReadKeyset :: KeySetName -> IO (Maybe FQKS)
--   -- ^ Read in a fully resolve keyset
--   , _pdbWriteKeyset :: KeySetName -> FQKS -> IO ()
--   -- ^ write in a keyset
--   }

readModule :: PactDb b i -> ModuleName -> IO (Maybe (ModuleData b i))
readModule pdb = _pdbRead pdb DModules

writeModule :: PactDb b i -> ModuleName -> ModuleData b i -> IO ()
writeModule pdb = _pdbWrite pdb DModules

readKeyset :: PactDb b i -> KeySetName -> IO (Maybe FQKS)
readKeyset pdb = _pdbRead pdb DKeySets

writeKeySet :: PactDb b i -> KeySetName -> FQKS -> IO ()
writeKeySet pdb = _pdbWrite pdb DKeySets


data Loaded b i
  = Loaded
  { _loModules :: Map ModuleName (ModuleData b i)
  , _loToplevel :: Map Text (FullyQualifiedName, DefKind)
  , _loAllLoaded :: Map FullyQualifiedName (Def Name b i)
  } deriving Show

makeLenses ''Loaded

instance Semigroup (Loaded b i) where
  (Loaded ms tl al) <> (Loaded ms' tl' al') =
    Loaded (ms <> ms') (tl <> tl') (al <> al')

instance Monoid (Loaded b i) where
  mempty = Loaded mempty mempty mempty

mockPactDb :: forall b i. IO (PactDb b i)
mockPactDb = do
  refMod <- newIORef Map.empty
  refKs <- newIORef Map.empty
  pure $ PactDb
    { _pdbPurity = PImpure
    , _pdbRead = read' refKs refMod
    , _pdbWrite = write refKs refMod
    -- , _readModule = liftIO . readMod refMod
    -- , _writeModule = liftIO . writeMod refMod
    -- , _readKeyset = liftIO . readKS refKs
    -- , _writeKeyset = \ksn  -> liftIO . writeKS refKs ksn
    }
  where
  read'
    :: forall k v
    .  IORef (Map KeySetName FQKS)
    -> IORef (Map ModuleName (ModuleData b i))
    -> Domain k v b i
    -> k
    -> IO (Maybe v)
  read' refKs refMod domain k = case domain of
    DKeySets -> readKS refKs k
    DModules -> readMod refMod k

  write
    :: forall k v
    .  IORef (Map KeySetName FQKS)
    -> IORef (Map ModuleName (ModuleData b i))
    -> Domain k v b i
    -> k
    -> v
    -> IO ()
  write refKs refMod domain k v = case domain of
    DKeySets -> writeKS refKs k v
    DModules -> writeMod refMod v

  readKS ref ksn = do
    m <- readIORef ref
    pure (Map.lookup ksn m)

  writeKS ref ksn ks = modifyIORef' ref (Map.insert ksn ks)

  readMod :: IORef (Map ModuleName (ModuleData b i)) -> ModuleName -> IO (Maybe (ModuleData b i))
  readMod ref mn = do
    m <- readIORef ref
    pure (Map.lookup mn m)

  writeMod ref md = let
    mname = view mdModuleName md
    in modifyIORef' ref (Map.insert mname md)

