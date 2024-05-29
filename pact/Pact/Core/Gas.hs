{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}

module Pact.Core.Gas
 ( module Pact.Core.Gas.Types
 , runGasM
 , ignoreGas
 ) where

import Control.Exception
import Control.Monad.Reader
import Control.Monad.Except
import Data.IORef

import Pact.Core.Errors
import Pact.Core.Gas.Types
import Pact.Core.StackFrame

-- | Run a 'GasM' computation with the given environment.
runGasM
  :: [StackFrame i]
  -> i
  -> GasMEnv b
  -> GasM (PactError i) b a
  -> IO (Either (PactError i) a)
runGasM stack info env (GasM m) = do
  res <- try $ runExceptT $ runReaderT m env
  case res of
    Left (e :: DbOpException) -> pure $ Left $ PEExecutionError (DbOpFailure e) stack info
    Right a -> pure a

-- | Run a 'GasM' computation with an infinite gas limit.
ignoreGas
  :: i
  -> GasM (PactError i) b a
  -> IO a
ignoreGas info m = do
  gasRef <- newIORef (MilliGas 0)
  runGasM [] info (GasMEnv gasRef freeGasModel) m >>=
    \case
      Left _ -> error "impossible case: ran out of gas with an infinite limit"
      Right a -> pure a