{-# LANGUAGE RecordWildCards #-}

module Pact.Core.Gas.Utils
  ( chargeGasArgsM
  , chargeGasArgs
  , chargeFlatNativeGas
  , scalarMulMilliGas
  ) where

import Control.Lens
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Foldable
import Data.IORef
import Pact.Core.StackFrame
import Pact.Core.Errors
import Pact.Core.Gas.Types
import Pact.Core.Gas.TableGasModel
import Pact.Core.Environment

-- | Multiply Milligas by a scalar
scalarMulMilliGas :: Integral a => MilliGas -> a -> MilliGas
scalarMulMilliGas (MilliGas mg) i =
  MilliGas (mg * fromIntegral i)
{-# INLINE scalarMulMilliGas #-}

chargeGasArgsM
  :: GasEnv b i -> i -> [StackFrame i] -> GasArgs b -> IO (Either (PactError i) ())
chargeGasArgsM GasEnv{..} info stack gasArgs = do
  let !milliGasCost = runTableModel (_gmNativeTable _geGasModel) (_gmGasCostConfig _geGasModel) gasArgs
  case _gmGasLimit _geGasModel of
    Just mgl@(MilliGasLimit milliGasLimit) -> do
      newGasTotal <- do
        !currGasTotal <- readIORef _geGasRef
        -- calculate new gas total
        let !newGasTotal = currGasTotal <> milliGasCost
        -- add a gas log entry if the gas log is enabled
        forM_ _geGasLog $ \ref -> do
          -- TODO: make info stack stricter
          modifyIORef' ref $
            (GasLogEntry { _gleArgs = gasArgs, _gleInfo = info, _gleInfoStack = _sfInfo <$> stack, _gleThisUsed = milliGasCost } :)

        writeIORef _geGasRef newGasTotal
        return newGasTotal
      if milliGasLimit >= newGasTotal then pure (Right ())
      else return $ Left $ PEExecutionError (GasExceeded (milliGasToGasLimit mgl) (milliGasToGas newGasTotal)) stack info
    _ -> return $ Right ()
{-# INLINE chargeGasArgsM #-}


chargeGasArgs :: i -> GasArgs b -> EvalM e b i ()
chargeGasArgs info gasArgs = do
  stack <- use esStack
  gasEnv <- viewEvalEnv eeGasEnv
  either throwError return =<<
    liftIO (chargeGasArgsM gasEnv info stack gasArgs)
{-# INLINABLE chargeGasArgs #-}

chargeFlatNativeGas :: i -> b -> EvalM e b i ()
chargeFlatNativeGas info nativeArg =
  chargeGasArgs info (GNative nativeArg)
{-# INLINABLE chargeFlatNativeGas #-}
