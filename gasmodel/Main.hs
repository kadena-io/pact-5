{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Main where

import qualified Criterion.Main as C

import Pact.Core.GasModel.InterpreterGas as InterpreterGas

main :: IO ()
main = do
  C.defaultMain [InterpreterGas.benchmarks]



