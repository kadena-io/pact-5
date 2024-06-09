{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Main where

import qualified Criterion.Main as C

import Pact.Core.GasModel.InterpreterGas as InterpreterGas
import Pact.Core.GasModel.BuiltinsGas as BuiltinsGas
import Pact.Core.GasModel.ContractBench as ContractBench
import qualified System.Environment as Env

main :: IO ()
main = do
  v <- Env.lookupEnv "RESET_COIN_BENCH_DB"
  C.defaultMain
    [ ContractBench.allBenchmarks (v == Just "1")
    -- , InterpreterGas.benchmarks
    -- , BuiltinsGas.benchmarks
    ]



