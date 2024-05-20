module Pact.Core.Persistence.Utils where

import Control.Exception(throwIO)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Default

import Pact.Core.Names
import Pact.Core.Guards
import Pact.Core.DefPacts.Types
import Pact.Core.Gas
import Pact.Core.Namespace
import Pact.Core.Errors
import Pact.Core.Persistence.Types
import Pact.Core.Environment


-- Potentially new Pactdb abstraction
-- That said: changes in `Purity` that restrict read/write
-- have to be done for all read functions.
readModule :: PactDb b i -> ModuleName -> IO (Maybe (ModuleData b i))
readModule pdb = _pdbRead pdb DModules

writeModule :: (MonadEval b i m) => i -> PactDb b i -> WriteType -> ModuleName -> ModuleData b i -> m ()
writeModule info pdb wt mn md =
  liftGasM info $ _pdbWrite pdb info wt DModules mn md

readKeySet :: PactDb b i -> KeySetName -> IO (Maybe KeySet)
readKeySet pdb = _pdbRead pdb DKeySets

writeKeySet :: (MonadEval b i m) => i -> PactDb b i -> WriteType -> KeySetName -> KeySet -> m ()
writeKeySet info pdb wt ksn ks = do
  liftGasM info $ _pdbWrite pdb info wt DKeySets ksn ks

readDefPacts :: PactDb b i -> DefPactId -> IO (Maybe (Maybe DefPactExec))
readDefPacts pdb = _pdbRead pdb DDefPacts

writeDefPacts :: (MonadEval b i m) => i -> PactDb b i -> WriteType -> DefPactId -> Maybe DefPactExec -> m ()
writeDefPacts info pdb wt defpactId defpactExec =
  liftGasM info $ _pdbWrite pdb info wt DDefPacts defpactId defpactExec

readNamespace :: PactDb b i -> NamespaceName -> IO (Maybe Namespace)
readNamespace pdb = _pdbRead pdb DNamespaces

writeNamespace :: (MonadEval b i m) => i -> PactDb b i -> WriteType -> NamespaceName -> Namespace -> m ()
writeNamespace info pdb wt namespaceName namespace =
  liftGasM info $ _pdbWrite pdb info wt DNamespaces namespaceName namespace

-- | For several db operations, we expect not to use gas. This
--   function tests that assumption by failing if it is violated.
-- failIfUsesGas :: MilliGas -> m ()
-- failIfUsesGas _ = error "Expected no gas use (even charges of 0 gas)"


dbOpDisallowed :: MonadIO m => m a
dbOpDisallowed = liftIO $ putStrLn "OpDisallowed" >> throwIO OpDisallowed

dbOpDisallowed2 :: forall i m a. (MonadError (PactError i) m, MonadIO m, Default i) => m a
dbOpDisallowed2 = do
  liftIO (putStrLn "OpDisallowed")
  throwError (PEExecutionError (DbOpFailure OpDisallowed) [] def)

-- | A utility function that lifts a `GasM` action into a `MonadEval` action.
liftGasM :: MonadEval b i m => i -> GasM (PactError i) a -> m a
liftGasM info action = do
  gasRef <- viewEvalEnv eeGasRef
  gasLimit <- viewEvalEnv (eeGasModel . gmGasLimit)
  either throwError pure =<<
    liftIO (runGasM info (GasMEnv gasRef gasLimit) action)
