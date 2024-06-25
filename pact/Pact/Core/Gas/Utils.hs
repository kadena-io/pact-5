{-# language RecordWildCards #-}

module Pact.Core.Gas.Utils (chargeGasArgsM) where

import Data.Foldable
import Data.IORef
import Pact.Core.StackFrame
import Pact.Core.Errors
import Pact.Core.Gas.Types

chargeGasArgsM
  :: GasEnv b i -> i -> [StackFrame i] -> GasArgs b -> IO (Either (PactError i) ())
chargeGasArgsM GasEnv{..} info stack gasArgs = do
  let !milliGasCost = _gmRunModel _geGasModel gasArgs
  newGasTotal <- do
    !currGasTotal <- readIORef _geGasRef
    -- calculate new gas total
    let !newGasTotal = currGasTotal <> milliGasCost
    -- add a gas log entry if the gas log is enabled
    maybeGasLog <- readIORef _geGasLogRef
    forM_ maybeGasLog $ \gasLog -> do
      -- TODO: make info stack stricter
      writeIORef _geGasLogRef $ Just
        (GasLogEntry { _gleArgs = gasArgs, _gleInfo = info, _gleInfoStack = _sfInfo <$> stack, _gleThisUsed = milliGasCost } : gasLog)

    writeIORef _geGasRef newGasTotal
    return newGasTotal
  case _gmGasLimit _geGasModel of
    Just (MilliGasLimit milliGasLimit) | newGasTotal > milliGasLimit ->
      return $ Left $ PEExecutionError
        GasExceeded
        stack info
    _ -> return $ Right ()
