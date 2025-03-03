{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Main where

import qualified Criterion.Main as C

import Pact.Core.GasModel.BuiltinsGas as BuiltinsGas
import Pact.Core.GasModel.ContractBench as ContractBench
import Pact.Core.GasModel.Serialization as Serialization
import Pact.Core.GasModel.ModuleLoadBench as ModuleLoad
import Pact.Core.GasModel.ModuleDepsBench as ModuleDepsBench

main :: IO ()
main = do
  depsBenches <- ModuleDepsBench.benchmarks
  C.defaultMain
    [ ContractBench.allBenchmarks
    , BuiltinsGas.benchmarks
    , Serialization.benchmarks
    , depsBenches
    , ModuleLoad.benchmarks
    ]



