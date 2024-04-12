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
import Pact.Core.Literal
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

enumExpString :: T.Text -> Integer -> Integer -> [(String, PactValue)]
enumExpString rep base mult =
  [ (title, PString $ T.replicate (fromIntegral cnt) rep)
  | (title, cnt) <- enumExpNum base mult
  ]

type BuiltinBenches = PactDb CoreBuiltin () -> [C.Benchmark]

benchArithBinOp' :: Integer -> T.Text -> BuiltinBenches
benchArithBinOp' growthFactor op pdb =
  [ C.bgroup "integer"
    [ runNativeBenchmark pdb title [text|($op $x $x)|] | (title, x) <- vals ]
  , C.bgroup "float"
    [ runNativeBenchmark pdb title [text|($op $x.0 $x.0)|] | (title, x) <- vals ]
  , C.bgroup "float_int"
    [ runNativeBenchmark pdb title [text|($op $x.0 $x)|] | (title, x) <- vals ]
  , C.bgroup "int_float"
    [ runNativeBenchmark pdb title [text|($op $x $x.0)|] | (title, x) <- vals ]
  ]
  where
  vals = take 3 $ enumExpText 1000 growthFactor

benchArithBinOp :: T.Text -> BuiltinBenches
benchArithBinOp = benchArithBinOp' 1000000

benchArithUnOp :: T.Text -> BuiltinBenches
benchArithUnOp op pdb =
  [ C.bgroup "integer"
    [ runNativeBenchmark pdb title [text|($op $x)|] | (title, x) <- vals ]
  , C.bgroup "float"
    [ runNativeBenchmark pdb title [text|($op $x.0)|] | (title, x) <- vals ]
  ]
  where
  vals = take 3 $ enumExpText 1000 1000000

benchAddNonArithOverloads :: BuiltinBenches
benchAddNonArithOverloads pdb =
  [ C.bgroup "list-shallow"
    [ runNativeBenchmarkPrepared [("x", list)] pdb title [text|(+ x x)|]
    | (title, list) <- take 3 $ enumExpList 1000 100
    ]
  , C.bgroup "list-deep"
    [ runNativeBenchmarkPrepared [("x", list)] pdb title [text|(+ x x)|]
    | (title, list) <- take 3 $ enumExpListDeep 3 5 3
    ]
  , C.bgroup "object"
    [ runNativeBenchmarkPrepared [("x", obj)] pdb title [text|(+ x x)|]
    | (title, obj) <- take 3 $ enumExpObject 1000 100
    ]
  , C.bgroup "string"
    [ runNativeBenchmarkPrepared [("x", str)] pdb title [text|(+ x x)|]
    | (title, str) <- take 3 $ enumExpString "a" 1000 1000
    ]
  ]

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
  [ C.bgroup "lists-same" listsEq
  , C.bgroup "lists-diff" listsNeq
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

benchBitwiseBinOp :: Integer -> T.Text -> BuiltinBenches
benchBitwiseBinOp growth op pdb =
  [ runNativeBenchmark pdb title [text|($op $x $x)|]
  | (title, x) <- take 3 $ enumExpText 1000 growth
  ]

benchBitwiseFlip :: BuiltinBenches
benchBitwiseFlip pdb =
  [ runNativeBenchmark pdb title [text|(~ $x $x)|]
  | (title, x) <- take 3 $ enumExpText 1000 1000000
  ]

benchFloatingUnOp :: T.Text -> BuiltinBenches
benchFloatingUnOp op pdb =
  [ runNativeBenchmark pdb title [text|($op $x.0)|]
  | (title, x) <- take 3 $ enumExpText 1000 1000
  ]

benchFloatingMixedOp :: T.Text -> BuiltinBenches
benchFloatingMixedOp op pdb =
  [ runNativeBenchmark pdb title [text|($op $x.0 $x)|]
  | (title, x) <- take 3 $ enumExpText 1000 1000
  ]

benchLength :: BuiltinBenches
benchLength pdb =
  [ C.bgroup "list"
    [ runNativeBenchmarkPrepared [("x", list)] pdb title [text|(length x)|]
    | (title, list) <- take 3 $ enumExpList 1000 100
    ]
  , C.bgroup "string"
    [ runNativeBenchmarkPrepared [("x", str)] pdb title [text|(length x)|]
    | (title, str) <- take 3 $ enumExpString "a" 1000 1000
    ]
  , C.bgroup "object"
    [ runNativeBenchmarkPrepared [("x", obj)] pdb title [text|(length x)|]
    | (title, obj) <- take 3 $ enumExpObject 1000 10
    ]
  , C.bgroup "object-complex"
    [ runNativeBenchmarkPrepared [("x", obj)] pdb title [text|(length x)|]
    | (title, obj) <- take 3 $ enumExpObjectComplex 1000 2
    ]
  ]

benchTakeDrop :: T.Text -> BuiltinBenches
benchTakeDrop op pdb =
  [ C.bgroup "list"
    [ runNativeBenchmarkPrepared [("x", list), ("len", PInteger len)] pdb title [text|($op len x)|]
    | (listTitle, list@(PList vec)) <- take 3 $ enumExpList 1000 100
    , (takeTitle, len) <- take 3 $ enumExpNum 1000 100
    , fromIntegral len <= V.length vec
    , let title = listTitle <> "_" <> takeTitle
    ]
  , C.bgroup "string"
    [ runNativeBenchmarkPrepared [("x", str), ("len", PInteger len)] pdb title [text|($op len x)|]
    | (strTitle, str@(PString vec)) <- take 3 $ enumExpString "a" 1000 100
    , (takeTitle, len) <- take 3 $ enumExpNum 1000 100
    , fromIntegral len <= T.length vec
    , let title = strTitle <> "_" <> takeTitle
    ]
  , C.bgroup "object"
    [ runNativeBenchmarkPrepared [("x", obj), ("keys", PList keys)] pdb title [text|($op keys x)|]
    | (strTitle, obj@(PObject m)) <- take 3 $ enumExpObject 1000 100
    , (takeTitle, len) <- take 3 $ enumExpNum 1000 100
    , fromIntegral len <= M.size m
    , let title = strTitle <> "_" <> takeTitle
    , let keys = V.fromList $ fmap (PString . _field) $ take (fromIntegral len) $ M.keys m
    ]
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
    [ runNativeBenchmarkPrepared [("x", list)] pdb title [text|(distinct x)|]
    | (title, list) <- take 3 $ enumExpListDeep 3 5 4
    ]

benchEnumerate :: BuiltinBenches
benchEnumerate pdb =
  [ runNativeBenchmark pdb title [text|(enumerate 0 $cnt)|]
  | (title, cnt) <- take 3 $ enumExpText 1000 10
  ]

benchesForBuiltin :: CoreBuiltin -> BuiltinBenches
benchesForBuiltin bn pdb = case bn of
  CoreAdd -> benchArithBinOp "+" pdb <> benchAddNonArithOverloads pdb
  CoreSub -> benchArithBinOp "-" pdb
  CoreMultiply -> benchArithBinOp "*" pdb
  CoreDivide -> benchArithBinOp "/" pdb
  CoreNegate -> benchNegate pdb
  CoreAbs -> benchArithUnOp "abs" pdb
  CorePow -> benchArithBinOp' 100 "^" pdb
  CoreNot -> omittedDeliberately
  CoreEq -> benchEqOp "=" pdb
  CoreNeq -> benchEqOp "!=" pdb
  CoreGT -> benchEqOp ">" pdb
  CoreGEQ -> benchEqOp ">=" pdb
  CoreLT -> benchEqOp "<" pdb
  CoreLEQ -> benchEqOp "<=" pdb
  CoreBitwiseAnd -> benchBitwiseBinOp 1000000 "&" pdb
  CoreBitwiseOr -> benchBitwiseBinOp 1000000 "|" pdb
  CoreBitwiseXor -> benchBitwiseBinOp 1000000 "xor" pdb
  CoreBitwiseFlip -> benchBitwiseFlip pdb
  CoreBitShift -> benchBitwiseBinOp 1000 "shift" pdb
  CoreRound -> benchFloatingUnOp "round" pdb
  CoreCeiling -> benchFloatingUnOp "ceiling" pdb
  CoreFloor -> benchFloatingUnOp "floor" pdb
  CoreRoundPrec -> benchFloatingMixedOp "round" pdb
  CoreCeilingPrec -> benchFloatingMixedOp "ceiling" pdb
  CoreFloorPrec -> benchFloatingMixedOp "floor" pdb
  CoreExp -> benchArithUnOp "exp" pdb
  CoreLn -> benchArithUnOp "ln" pdb
  CoreSqrt -> benchArithUnOp "sqrt" pdb
  CoreLogBase -> benchArithBinOp "log" pdb
  CoreLength -> benchLength pdb
  CoreTake -> benchTakeDrop "take" pdb
  CoreDrop -> benchTakeDrop "drop" pdb
  CoreDistinct -> benchDistinct pdb
  CoreEnumerate -> benchEnumerate pdb
  _ -> []
  where
  omittedDeliberately = []

benchmarkName :: CoreBuiltin -> String
benchmarkName = \case
  CoreAdd -> "add"
  CoreSub -> "sub"
  CoreMultiply -> "mul"
  CoreDivide -> "div"
  CoreNegate -> "neg"
  CorePow -> "pow"
  CoreEq -> "eq"
  CoreNeq -> "neq"
  CoreGT -> "gt"
  CoreGEQ -> "geq"
  CoreLT -> "lt"
  CoreLEQ -> "leq"
  CoreBitwiseAnd -> "and"
  CoreBitwiseOr -> "or"
  CoreBitwiseFlip -> "flip"
  b -> T.unpack $ coreBuiltinToText b

benchmarks :: C.Benchmark
benchmarks = C.envWithCleanup mkPactDb cleanupPactDb $ \ ~(pdb, _db) -> do
  C.bgroup "pact-core-builtin-gas"
    [ C.bgroup (benchmarkName coreBuiltin) benches
    | coreBuiltin <- [minBound .. maxBound]
    , let benches = benchesForBuiltin coreBuiltin pdb
    , not $ null benches
    ]
  where
  mkPactDb = do
    tup@(pdb, _) <- unsafeCreateSqlitePactDb serialisePact ":memory:"
    _ <- _pdbBeginTx pdb Transactional
    pure tup

  cleanupPactDb (_, db) = SQL.close db
