{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Pact.Core.GasModel.BuiltinsGas where

import qualified Criterion as C
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Database.SQLite3 as SQL
import Data.Bifunctor
import NeatInterpolation (text)

import Pact.Core.Builtin
import Pact.Core.Names
import Pact.Core.PactValue
import Pact.Core.Persistence
import Pact.Core.Persistence.SQLite
import Pact.Core.Serialise (serialisePact)

import Pact.Core.GasModel.Utils

enumExpNum :: Integer -> Integer -> [(String, Integer)]
enumExpNum base mult = [ (show val, val) | val <- iterate (* mult) base ]

enumExpText :: Integer -> Integer -> [(String, T.Text)]
enumExpText base mult = second (T.pack . show) <$> enumExpNum base mult

mkList :: Integer -> PactValue
mkList cnt = PList $ V.fromList $ PInteger <$> [0 .. cnt]

enumExpList :: Integer -> Integer -> [(String, PactValue)]
enumExpList base mult = [ (title, mkList cnt) | (title, cnt) <- enumExpNum base mult ]

enumExpListDeep :: Integer -> Integer -> Integer -> [(String, PactValue)]
enumExpListDeep depth base mult =
  [ (title <> " depth " <> show depth, go depth cnt)
  | (title, cnt) <- enumExpNum base mult
  ]
  where
  go 0 cnt = mkList cnt
  go d cnt = PList $ V.fromList $ replicate (fromIntegral cnt) (go (d - 1) cnt)

mkMap :: (Integer -> PactValue) -> Integer -> M.Map Field PactValue
mkMap mkVal cnt = M.fromList [ (Field $ T.pack $ show n, mkVal n) | n <- [0..cnt] ]

enumExpObject :: Integer -> Integer -> [(String, PactValue)]
enumExpObject base mult =
  [ (title, PObject $ mkMap PInteger cnt)
  | (title, cnt) <- enumExpNum base mult
  ]

enumExpObjectComplex :: Integer -> Integer -> [(String, PactValue)]
enumExpObjectComplex base mult =
  [ (title, PObject $ mkMap mkList cnt)
  | (title, cnt) <- enumExpNum base mult
  ]

type BuiltinBenches = PactDb CoreBuiltin () -> [C.Benchmark]

benchArithOp' :: Integer -> T.Text -> BuiltinBenches
benchArithOp' growthFactor op pdb =
  [ C.bgroup "integer"
    [ runNativeBenchmark pdb title [text|($op $x $x)|] | (title, x) <- vals ]
  , C.bgroup "float"
    [ runNativeBenchmark pdb title [text|($op $x.0 $x.0)|] | (title, x) <- vals ]
  , C.bgroup "mixed"
    [ runNativeBenchmark pdb title [text|($op $x $x.0)|] | (title, x) <- vals ]
  ]
  where
  vals = take 3 $ enumExpText 1000 growthFactor

benchArithOp :: T.Text -> BuiltinBenches
benchArithOp = benchArithOp' 1000000

benchAbs :: BuiltinBenches
benchAbs pdb =
  [ C.bgroup "integer"
    [ runNativeBenchmark pdb title [text|(abs -$x)|] | (title, x) <- vals ]
  , C.bgroup "float"
    [ runNativeBenchmark pdb title [text|(abs -$x.0)|] | (title, x) <- vals ]
  ]
  where
  vals = take 3 $ enumExpText 1000 1000000

benchNegate :: BuiltinBenches
benchNegate pdb =
  [ C.bgroup "integer"
    [ runNativeBenchmark pdb title [text|(negate $x)|] | (title, x) <- vals ]
  , C.bgroup "float"
    [ runNativeBenchmark pdb title [text|(negate $x.0)|] | (title, x) <- vals ]
  ]
  where
  vals = take 3 $ enumExpText 1000 1000000

benchEqOp :: T.Text -> BuiltinBenches
benchEqOp op pdb =
  [ C.bgroup "lists-eq" listsEq
  , C.bgroup "lists-neq" listsNeq
  , C.bgroup "lists-deep" listsDeep
  , C.bgroup "objects-simple" objsSimple
  , C.bgroup "objects-complex" objsComplex
  ]
  where
  listsEq =
    [ runNativeBenchmarkPrepared [("x", list)] pdb title [text|($op x x)|]
    | (title, list) <- take 3 $ enumExpList 1000 100
    ]
  listsNeq =
    [ runNativeBenchmarkPrepared [("x", list)] pdb title [text|($op x [])|]
    | (title, list) <- take 3 $ enumExpList 1000 100
    ]
  listsDeep =
    [ runNativeBenchmarkPrepared [("x", list)] pdb title [text|($op x x)|]
    | (title, list) <- take 3 $ enumExpListDeep 3 5 3
    ]
  objsSimple =
    [ runNativeBenchmarkPrepared [("x", obj)] pdb title [text|($op x x)|]
    | (title, obj) <- take 3 $ enumExpObject 1000 10
    ]
  objsComplex =
    [ runNativeBenchmarkPrepared [("x", obj)] pdb title [text|($op x x)|]
    | (title, obj) <- take 3 $ enumExpObjectComplex 1000 2
    ]

benchDistinct :: BuiltinBenches
benchDistinct pdb =
  [ C.bgroup "flat" flats
  , C.bgroup "nested" nesteds
  ]
  where
  flats =
    [ runNativeBenchmarkPrepared [("x", list)] pdb title [text|(distinct x)|]
    | (title, list) <- take 3 $ enumExpList 1000 2
    ]
  nesteds =
    [ runNativeBenchmarkPrepared [("x", list)] pdb title [text|(distinct)|]
    | (title, list) <- take 3 $ enumExpListDeep 3 5 4
    ]

benchEnumerate :: BuiltinBenches
benchEnumerate pdb = [ runNativeBenchmark pdb title [text|(enumerate 0 $cnt)|]
                     | (title, cnt) <- take 3 $ enumExpText 1000 10
                     ]

benchesForFun :: CoreBuiltin -> BuiltinBenches
benchesForFun bn pdb = case bn of
  CoreAdd -> benchArithOp "+" pdb
  CoreSub -> benchArithOp "-" pdb
  CoreMultiply -> benchArithOp "-" pdb
  CoreDivide -> benchArithOp "-" pdb
  CoreNegate -> benchNegate pdb
  CoreAbs -> benchAbs pdb
  CorePow -> benchArithOp' 100 "^" pdb
  CoreNot -> omittedDeliberately
  CoreEq -> benchEqOp "=" pdb
  CoreNeq -> benchEqOp "!=" pdb
  CoreDistinct -> benchDistinct pdb
  CoreEnumerate -> benchEnumerate pdb
  _ -> []
  where
  omittedDeliberately = []

benchmarks :: C.Benchmark
benchmarks = C.envWithCleanup mkPactDb cleanupPactDb $ \ ~(pdb, _db) -> do
  C.bgroup "pact-core-builtin-gas"
    [ C.bgroup (T.unpack $ coreBuiltinToText coreBuiltin) benches
    | coreBuiltin <- [minBound .. maxBound]
    , let benches = benchesForFun coreBuiltin pdb
    , not $ null benches
    ]
  where
  mkPactDb = do
    tup@(pdb, _) <- unsafeCreateSqlitePactDb serialisePact ":memory:"
    _ <- _pdbBeginTx pdb Transactional
    pure tup

  cleanupPactDb (_, db) = SQL.close db
