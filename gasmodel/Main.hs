{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Main where

import qualified Criterion.Main as C

import Pact.Core.GasModel.InterpreterGas as InterpreterGas
import Pact.Core.GasModel.BuiltinsGas as BuiltinsGas
import Pact.Core.GasModel.ContractBench as ContractBench
import Pact.Core.GasModel.Serialization as Serialization

main :: IO ()
main = do
  C.defaultMain
    [ ContractBench.allBenchmarks
    , InterpreterGas.benchmarks
    , BuiltinsGas.benchmarks
    , Serialization.benchmarks
    ]



