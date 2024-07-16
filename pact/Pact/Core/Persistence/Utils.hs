{-# LANGUAGE TypeApplications #-}

module Pact.Core.Persistence.Utils
  ( evalWrite
  , evalCreateUserTable
  , dbOpDisallowed
  , liftGasM
  , ignoreGas
  , chargeGasM
  ) where

import Control.Lens
import Control.Exception.Safe
import Control.Monad.Reader
import Data.IORef

import Pact.Core.Environment
import Pact.Core.Errors
import Pact.Core.Names
import Pact.Core.Persistence.Types
import Pact.Core.Gas
import Control.Monad.Except


evalWrite :: i -> PactDb b i -> WriteType -> Domain k v b i -> k -> v -> EvalM e b i ()
evalWrite info pdb wt d k v = liftGasM info $ _pdbWrite pdb wt d k v

evalCreateUserTable :: i -> PactDb b i -> TableName -> EvalM e b i ()
evalCreateUserTable info pdb tn = liftGasM info $ _pdbCreateUserTable pdb tn

dbOpDisallowed :: MonadIO m => m a
dbOpDisallowed = liftIO $ throwIO OpDisallowed

chargeGasM :: GasArgs b -> GasM b i ()
chargeGasM gasArgs = do
  (gasEnv, info, stack) <- ask
  either throwError return =<< liftIO (chargeGasArgsM gasEnv info stack gasArgs)

-- | A utility function that lifts a `GasM` action into a `MonadEval` action.
liftGasM :: i -> GasM b i a -> EvalM e b i a
liftGasM info action = do
  gasEnv <- viewEvalEnv eeGasEnv
  stack <- use esStack
  caught <- liftIO $ try @IO @DbOpException (runExceptT (runReaderT (runGasM action) (gasEnv, info, stack)))
  case caught of
    Right (Right r) -> pure r
    Right (Left gasErr) -> throwError gasErr
    Left err -> throwExecutionError info (DbOpFailure err)

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
