module Pact.Core.Persistence.Utils where

import Control.Lens
import Control.Exception.Safe
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Error.Class (throwError)
import Data.IORef

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

writeModule :: i -> PactDb b i -> WriteType -> ModuleName -> ModuleData b i -> EvalM e b i ()
writeModule info pdb wt mn md = do
  liftGasM info $ _pdbWrite pdb wt DModules mn md

readKeySet :: PactDb b i -> KeySetName -> IO (Maybe KeySet)
readKeySet pdb = _pdbRead pdb DKeySets

writeKeySet :: i -> PactDb b i -> WriteType -> KeySetName -> KeySet -> EvalM e b i ()
writeKeySet info pdb wt ksn ks = do
  liftGasM info $ _pdbWrite pdb wt DKeySets ksn ks

readDefPacts :: PactDb b i -> DefPactId -> IO (Maybe (Maybe DefPactExec))
readDefPacts pdb = _pdbRead pdb DDefPacts

writeDefPacts :: i -> PactDb b i -> WriteType -> DefPactId -> Maybe DefPactExec -> EvalM e b i ()
writeDefPacts info pdb wt defpactId defpactExec =
  liftGasM info $ _pdbWrite pdb wt DDefPacts defpactId defpactExec

readNamespace :: PactDb b i -> NamespaceName -> IO (Maybe Namespace)
readNamespace pdb = _pdbRead pdb DNamespaces

writeNamespace :: i -> PactDb b i -> WriteType -> NamespaceName -> Namespace -> EvalM e b i ()
writeNamespace info pdb wt namespaceName namespace =
  liftGasM info $ _pdbWrite pdb wt DNamespaces namespaceName namespace


dbOpDisallowed :: MonadIO m => m a
dbOpDisallowed = liftIO $ throwIO OpDisallowed

-- | A utility function that lifts a `GasM` action into a `MonadEval` action.
liftGasM :: i -> GasM (PactError i) b a -> EvalM e b i a
liftGasM info action = do
  stack <- use esStack
  gasRef <- viewEvalEnv eeGasRef
  let chargeGas = gasMChargeGas' stack info gasRef
  gasModel <- viewEvalEnv eeGasModel
  caught <- try $ liftIO (runGasM stack info (GasMEnv chargeGas gasModel) action)
  case caught of
    Right e -> either throwError pure e
    Left err ->
      throwExecutionError info (DbOpFailure err)





gasMChargeGas' :: [StackFrame i] -> i -> IORef MilliGas -> MilliGas -> GasM (PactError i) b ()
gasMChargeGas' stackFrame info gasRef amount = do
  GasMEnv _chargeGas gasModel <- ask
  let mgl@(MilliGasLimit gasLimit) = _gmGasLimit gasModel
  !currGas <- liftIO $ readIORef gasRef
  let !used = currGas <> _gmRunModel gasModel (GAConstant amount)
  liftIO (writeIORef gasRef used)
  when (used > gasLimit) $ throwError (PEExecutionError (GasExceeded mgl used) stackFrame info)
