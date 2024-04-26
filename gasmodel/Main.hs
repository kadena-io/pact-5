{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Main where

import qualified Criterion.Main as C

import Pact.Core.GasModel.InterpreterGas as InterpreterGas
import Pact.Core.GasModel.BuiltinsGas as BuiltinsGas

main :: IO ()
main = do
  C.defaultMain
    [ InterpreterGas.benchmarks
    , BuiltinsGas.benchmarks
    ]



