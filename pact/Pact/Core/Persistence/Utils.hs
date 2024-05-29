module Pact.Core.Persistence.Utils where

import Control.Exception(throwIO)
import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Pact.Core.Environment
import Pact.Core.Errors
import Pact.Core.Guards
import Pact.Core.DefPacts.Types
import Pact.Core.Gas
import Pact.Core.Names
import Pact.Core.Namespace
import Pact.Core.Persistence.Types


-- Potentially new Pactdb abstraction
-- That said: changes in `Purity` that restrict read/write
-- have to be done for all read functions.
readModule :: PactDb b i -> ModuleName -> IO (Maybe (ModuleData b i))
readModule pdb = _pdbRead pdb DModules

writeModule :: (MonadEval b i m) => [StackFrame i] -> i -> PactDb b i -> WriteType -> ModuleName -> ModuleData b i -> m ()
writeModule stackFrame info pdb wt mn md =
  liftGasM stackFrame info $ _pdbWrite pdb stackFrame info wt DModules mn md

readKeySet :: PactDb b i -> KeySetName -> IO (Maybe KeySet)
readKeySet pdb = _pdbRead pdb DKeySets

writeKeySet :: (MonadEval b i m) => [StackFrame i] -> i -> PactDb b i -> WriteType -> KeySetName -> KeySet -> m ()
writeKeySet stackFrame info pdb wt ksn ks = do
  liftGasM stackFrame info $ _pdbWrite pdb stackFrame info wt DKeySets ksn ks

readDefPacts :: PactDb b i -> DefPactId -> IO (Maybe (Maybe DefPactExec))
readDefPacts pdb = _pdbRead pdb DDefPacts

writeDefPacts :: (MonadEval b i m) => [StackFrame i] -> i -> PactDb b i -> WriteType -> DefPactId -> Maybe DefPactExec -> m ()
writeDefPacts stackFrame info pdb wt defpactId defpactExec =
  liftGasM stackFrame info $ _pdbWrite pdb stackFrame info wt DDefPacts defpactId defpactExec

readNamespace :: PactDb b i -> NamespaceName -> IO (Maybe Namespace)
readNamespace pdb = _pdbRead pdb DNamespaces

writeNamespace :: (MonadEval b i m) => [StackFrame i] -> i -> PactDb b i -> WriteType -> NamespaceName -> Namespace -> m ()
writeNamespace stackFrame info pdb wt namespaceName namespace =
  liftGasM stackFrame info $ _pdbWrite pdb stackFrame info wt DNamespaces namespaceName namespace

-- | For several db operations, we expect not to use gas. This
--   function tests that assumption by failing if it is violated.
-- failIfUsesGas :: MilliGas -> m ()
-- failIfUsesGas _ = error "Expected no gas use (even charges of 0 gas)"


dbOpDisallowed :: MonadIO m => m a
dbOpDisallowed = liftIO $ throwIO OpDisallowed

-- | A utility function that lifts a `GasM` action into a `MonadEval` action.
liftGasM :: MonadEval b i m => [StackFrame i] -> i -> GasM (PactError i) b a -> m a
liftGasM stack info action = do
  gasRef <- viewEvalEnv eeGasRef
  gasModel <- viewEvalEnv eeGasModel
  either throwError pure =<<
    liftIO (runGasM stack info (GasMEnv gasRef gasModel) action)
