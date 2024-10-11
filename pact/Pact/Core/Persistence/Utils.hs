{-# LANGUAGE TypeApplications #-}

module Pact.Core.Persistence.Utils
  ( evalWrite
  , evalCreateUserTable
  , dbOpDisallowed
  , liftGasM
  , ignoreGas
  , chargeGasM
  , lookupModule
  , lookupModuleData
  , getModuleData
  , getModule
  , getModuleMember
  , getModuleMemberWithHash
  , throwDbOpErrorGasM
  ) where

import Control.Lens
import Control.Monad.Reader
import Data.IORef
import qualified Data.Map.Strict as M

import Pact.Core.Environment
import Pact.Core.Errors
import Pact.Core.Names
import Pact.Core.Persistence.Types
import Pact.Core.Gas
import Control.Monad.Except
import Pact.Core.IR.Term
import Pact.Core.Hash
import Data.Maybe (mapMaybe)


evalWrite :: i -> PactDb b i -> WriteType -> Domain k v b i -> k -> v -> EvalM e b i ()
evalWrite info pdb wt d k v = liftGasM info $ _pdbWrite pdb wt d k v

evalCreateUserTable :: i -> PactDb b i -> TableName -> EvalM e b i ()
evalCreateUserTable info pdb tn = liftGasM info $ _pdbCreateUserTable pdb tn

dbOpDisallowed :: GasM b i a
dbOpDisallowed = do
  (_, info, stack) <- ask
  throwError $ PEExecutionError (DbOpFailure OpDisallowed) stack info

throwDbOpErrorGasM :: DbOpError -> GasM b i a
throwDbOpErrorGasM opex = do
  (_, i, stack) <- ask
  throwError (PEExecutionError (DbOpFailure opex) stack i)

chargeGasM :: GasArgs b -> GasM b i ()
chargeGasM gasArgs = do
  (gasEnv, info, stack) <- ask
  either throwError return =<< liftIO (chargeGasArgsM gasEnv info stack gasArgs)

-- | lookupModuleData for only modules
lookupModule :: i -> ModuleName -> EvalM e b i (Maybe (EvalModule b i))
lookupModule info mn = do
 pdb <- viewEvalEnv eePactDb
 use (esLoaded . loModules . at mn) >>= \case
   Just (ModuleData md _) -> pure (Just md)
   Just (InterfaceData _ _) ->
    throwExecutionError info (ExpectedModule mn)
   Nothing -> do
    liftGasM info (_pdbRead pdb DModules mn) >>= \case
      Just mdata@(ModuleData md deps) -> do
        let newLoaded = M.fromList $ toFqDep mn (_mHash md) <$> _mDefs md
        (esLoaded . loAllLoaded) %= M.union newLoaded . M.union deps
        (esLoaded . loModules) %= M.insert mn mdata
        pure (Just md)
      Just (InterfaceData _ _) ->
        throwExecutionError info (ExpectedModule mn)
      Nothing -> pure Nothing

-- | lookupModuleData modules and interfaces
lookupModuleData :: i -> ModuleName -> EvalM e b i (Maybe (ModuleData b i))
lookupModuleData info mn = do
 pdb <- viewEvalEnv eePactDb
 use (esLoaded . loModules . at mn) >>= \case
   Just md -> pure (Just md)
   Nothing -> do
    liftGasM info (_pdbRead pdb DModules mn) >>= \case
      Just mdata@(ModuleData md deps) -> do
        let newLoaded = M.fromList $ toFqDep mn (_mHash md) <$> _mDefs md
        loadedSize <- uses loAllLoaded M.size
        chargeGasArgs info (GModuleOp (MOpMergeDeps loadedSize (M.size newLoaded + M.size deps)))
        (esLoaded . loAllLoaded) %= M.union newLoaded . M.union deps
        (esLoaded . loModules) %= M.insert mn mdata
        pure (Just mdata)
      Just mdata@(InterfaceData iface deps) -> do
        let ifDefs = mapMaybe ifDefToDef (_ifDefns iface)
        let newLoaded = M.fromList $ toFqDep mn (_ifHash iface) <$> ifDefs
        loadedSize <- uses loAllLoaded M.size
        chargeGasArgs info (GModuleOp (MOpMergeDeps loadedSize (M.size newLoaded + M.size deps)))
        (esLoaded . loAllLoaded) %= M.union newLoaded . M.union deps
        (esLoaded . loModules) %= M.insert mn mdata
        pure (Just mdata)
      Nothing -> pure Nothing

-- | getModuleData, but only for modules, no interfaces
getModule :: i -> ModuleName -> EvalM e b i (EvalModule b i)
getModule info mn = lookupModule info mn >>= \case
  Just md -> pure md
  Nothing -> throwExecutionError info (ModuleDoesNotExist mn)

-- | Get or load a module or interface based on the module name
getModuleData :: i -> ModuleName -> EvalM e b i (ModuleData b i)
getModuleData info mn = lookupModuleData info mn >>= \case
  Just md -> pure md
  Nothing -> throwExecutionError info (ModuleDoesNotExist mn)

-- | Returns a module member, but only for modules, no interfaces
getModuleMember :: i -> QualifiedName -> EvalM e b i (EvalDef b i)
getModuleMember info (QualifiedName qn mn) = do
  md <- getModule info mn
  case findDefInModule qn md of
    Just d -> pure d
    Nothing -> do
      let fqn = FullyQualifiedName mn qn (_mHash md)
      throwExecutionError info (ModuleMemberDoesNotExist fqn)

getModuleMemberWithHash :: i -> QualifiedName -> EvalM e b i (EvalDef b i, ModuleHash)
getModuleMemberWithHash info (QualifiedName qn mn) = do
  md <- getModule info mn
  case findDefInModule qn md of
    Just d -> pure (d, _mHash md)
    Nothing -> do
      let fqn = FullyQualifiedName mn qn (_mHash md)
      throwExecutionError info (ModuleMemberDoesNotExist fqn)

-- | A utility function that lifts a `GasM` action into a `MonadEval` action.
liftGasM :: i -> GasM b i a -> EvalM e b i a
liftGasM info action = do
  gasEnv <- viewEvalEnv eeGasEnv
  stack <- use esStack
  caught <- liftIO $ runExceptT (runReaderT (runGasM action) (gasEnv, info, stack))
  case caught of
    Right r -> pure r
    Left gasErr -> throwError gasErr

-- | Run a 'GasM' computation with an infinite gas limit.
ignoreGas
  :: i
  -> GasM b i a
  -> IO a
ignoreGas info m = do
  gasRef <- newIORef (MilliGas 0)
  r <- runExceptT $ runReaderT (runGasM m)
    ( GasEnv
      { _geGasRef = gasRef
      , _geGasLog = Nothing
      , _geGasModel = freeGasModel
      }
    , info
    , []
    )
  case r of
    Left _ -> error "impossible case: ran out of gas with an infinite limit"
    Right a -> pure a
