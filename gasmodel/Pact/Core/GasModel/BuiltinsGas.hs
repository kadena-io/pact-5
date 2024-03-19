{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Pact.Core.GasModel.BuiltinsGas where

import qualified Criterion as C
import qualified Data.Text as T
import qualified Database.SQLite3 as SQL
import NeatInterpolation (text)

import Pact.Core.Builtin
import Pact.Core.Persistence
import Pact.Core.Persistence.SQLite
import Pact.Core.Serialise (serialisePact)

import Pact.Core.GasModel.Utils

enumExp :: Int -> Int -> [(String, T.Text)]
enumExp base mult = [ (show val, T.pack $ show val) | val <- iterate (* mult) base ]

benchArithOp :: T.Text -> PactDb CoreBuiltin () -> [C.Benchmark]
benchArithOp op pdb =
  [ C.bgroup "integer"
    [ runNativeBenchmark pdb title [text|($op $x $x)|] | (title, x) <- vals ]
  , C.bgroup "float"
    [ runNativeBenchmark pdb title [text|($op $x.0 $x.0)|] | (title, x) <- vals ]
  , C.bgroup "mixed"
    [ runNativeBenchmark pdb title [text|($op $x $x.0)|] | (title, x) <- vals ]
  ]
  where
  vals = take 3 $ enumExp 1000 1000000

benchDistinct :: PactDb CoreBuiltin () -> [C.Benchmark]
benchDistinct pdb = [C.bgroup "flat" flats]
  where
  flats = [ runNativeBenchmark pdb title [text|(distinct (enumerate 0 $cnt))|]
          | (title, cnt) <- take 3 $ enumExp 1000 2
          ]

benchEnumerate :: PactDb CoreBuiltin () -> [C.Benchmark]
benchEnumerate pdb = [ runNativeBenchmark pdb title [text|(enumerate 0 $cnt)|]
                     | (title, cnt) <- take 3 $ enumExp 1000 10
                     ]

benchesForFun :: PactDb CoreBuiltin () -> CoreBuiltin -> [C.Benchmark]
benchesForFun pdb bn = case bn of
  CoreAdd -> benchArithOp "+" pdb
  CoreSub -> benchArithOp "-" pdb
  CoreMultiply -> benchArithOp "-" pdb
  CoreDivide -> benchArithOp "-" pdb
  CoreDistinct -> benchDistinct pdb
  CoreEnumerate -> benchEnumerate pdb
  _ -> []

benchmarks :: C.Benchmark
benchmarks = C.envWithCleanup mkPactDb cleanupPactDb $ \ ~(pdb, _db) -> do
  C.bgroup "pact-core-builtin-gas"
    [ C.bgroup (T.unpack $ coreBuiltinToText coreBuiltin) benches
    | coreBuiltin <- [minBound .. maxBound]
    , let benches = benchesForFun pdb coreBuiltin
    , not $ null benches
    ]
  where
  mkPactDb = do
    tup@(pdb, _) <- unsafeCreateSqlitePactDb serialisePact ":memory:"
    _ <- _pdbBeginTx pdb Transactional
    pure tup

  cleanupPactDb (_, db) = SQL.close db
