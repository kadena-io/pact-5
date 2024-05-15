module Pact.Core.Persistence.Utils where

import Control.Applicative((<|>))
import Control.Lens
import Control.Exception(throwIO)
import qualified Control.Monad.Catch as Exceptions
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Default
import Data.Map.Strict(Map)
import Control.DeepSeq
import GHC.Generics
import Data.Text(Text)
import Data.Word(Word64)
import Data.ByteString (ByteString)

import Pact.Core.Type
import Pact.Core.Names
import Pact.Core.IR.Term
import Pact.Core.Guards
import Pact.Core.Hash
import Pact.Core.PactValue
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
writeModule info pdb wt mn md = do
  gasRef <- viewEvalEnv eeGasRef
  gasLimit <- viewEvalEnv (eeGasModel . gmGasLimit)
  either throwError pure =<< liftIO (runGasM (GasMEnv gasRef gasLimit) (_pdbWrite pdb info wt DModules mn md))

readKeySet :: PactDb b i -> KeySetName -> IO (Maybe KeySet)
readKeySet pdb = _pdbRead pdb DKeySets

writeKeySet :: (MonadIO m, MonadError (PactError i) m, Default i, Exceptions.MonadCatch m) => PactDb b i -> WriteType -> KeySetName -> KeySet -> m ()
writeKeySet pdb wt = undefined
  -- _pdbWrite pdb failIfUsesGas wt DKeySets

readDefPacts :: PactDb b i -> DefPactId -> IO (Maybe (Maybe DefPactExec))
readDefPacts pdb = _pdbRead pdb DDefPacts

writeDefPacts :: (MonadIO m, MonadError (PactError i) m, Default i, Exceptions.MonadCatch m) => PactDb b i -> WriteType -> DefPactId -> Maybe DefPactExec -> m ()
writeDefPacts pdb wt = undefined
  -- _pdbWrite pdb failIfUsesGas wt DDefPacts

readNamespace :: PactDb b i -> NamespaceName -> IO (Maybe Namespace)
readNamespace pdb = _pdbRead pdb DNamespaces

writeNamespace :: (MonadIO m, MonadError (PactError i) m, Default i, Exceptions.MonadCatch m) => PactDb b i -> WriteType -> NamespaceName -> Namespace -> m ()
writeNamespace pdb wt = undefined
  -- _pdbWrite pdb failIfUsesGas wt DNamespaces

-- | For several db operations, we expect not to use gas. This
--   function tests that assumption by failing if it is violated.
-- failIfUsesGas :: MilliGas -> m ()
-- failIfUsesGas _ = error "Expected no gas use (even charges of 0 gas)"


dbOpDisallowed :: MonadIO m => m a
dbOpDisallowed = liftIO $ putStrLn "OpDisallowed" >> throwIO OpDisallowed

dbOpDisallowed2 :: forall i m a. (MonadError (PactError i) m, MonadIO m, Default i) => m a
dbOpDisallowed2 = liftIO (putStrLn "OpDisallowed") >> throwError (PEExecutionError (DbOpFailure OpDisallowed) def)
