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

fieldToValue :: Field -> PactValue
fieldToValue = PString . _field

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
    | (strTitle, str@(PString s)) <- take 3 $ enumExpString "a" 1000 100
    , (takeTitle, len) <- take 3 $ enumExpNum 1000 100
    , fromIntegral len <= T.length s
    , let title = strTitle <> "_" <> takeTitle
    ]
  , C.bgroup "object"
    [ runNativeBenchmarkPrepared [("x", obj), ("keys", PList keys)] pdb title [text|($op keys x)|]
    | (strTitle, obj@(PObject m)) <- take 3 $ enumExpObject 1000 100
    , (takeTitle, len) <- take 3 $ enumExpNum 1000 100
    , fromIntegral len <= M.size m
    , let title = strTitle <> "_" <> takeTitle
    , let keys = V.fromList $ fmap fieldToValue $ take (fromIntegral len) $ M.keys m
    ]
  ]

benchConcat :: BuiltinBenches
benchConcat pdb =
  [ runNativeBenchmarkPrepared [("x", strs)] pdb title [text|(concat x)|]
  | (strTitle, str) <- take 4 $ enumExpString "a" 1 10
  , (repTitle, reps) <- take 3 $ enumExpNum 100 10
  , let title = strTitle <> "_" <> repTitle
  , let strs = PList $ V.replicate (fromIntegral reps) str
  ]

benchReverse :: BuiltinBenches
benchReverse pdb =
  [ C.bgroup "list"
    [ runNativeBenchmarkPrepared [("x", list)] pdb title "(reverse x)"
    | (title, list) <- take 3 $ enumExpList 1000 100
    ]
  , C.bgroup "string"
    [ runNativeBenchmarkPrepared [("x", str)] pdb title "(reverse x)"
    | (title, str) <- take 3 $ enumExpString "a" 1000 100
    ]
  ]

benchContains :: BuiltinBenches
benchContains pdb =
  [ C.bgroup "object-first"
    [ runNativeBenchmarkPrepared [("x", obj), ("key", key)] pdb title "(contains key x)"
    | (title, obj@(PObject m)) <- take 3 $ enumExpObject 1000 100
    , let key = fieldToValue $ head $ M.keys m
    ]
  , C.bgroup "object-last"
    [ runNativeBenchmarkPrepared [("x", obj), ("key", key)] pdb title "(contains key x)"
    | (title, obj@(PObject m)) <- take 3 $ enumExpObject 1000 100
    , let key = fieldToValue $ last $ M.keys m
    ]
  , C.bgroup "object-nonexistent"
    [ runNativeBenchmarkPrepared [("x", obj), ("key", key)] pdb title "(contains key x)"
    | (title, obj@(PObject m)) <- take 3 $ enumExpObject 1000 100
    , let key = PString $ (<> "surelydoesntexist") $ _field $ last $ M.keys m
    ]
  , C.bgroup "string-short"
    [ runNativeBenchmarkPrepared [("x", str), ("needle", PString "b")] pdb title "(contains needle x)"
    | (title, str) <- take 3 $ enumExpString "a" 1000 100
    ]
  , C.bgroup "string-long-failfast"
    [ runNativeBenchmarkPrepared [("x", str), ("needle", needle)] pdb title "(contains needle x)"
    | let needle = PString $ T.replicate 1000 "b"
    , (title, str) <- take 3 $ enumExpString "a" 1000 100
    ]
  , C.bgroup "string-long-failfar"
    [ runNativeBenchmarkPrepared [("x", str), ("needle", needle)] pdb title "(contains needle x)"
    | let needle = PString $ T.replicate 999 "a" <> "b"
    , (title, str) <- take 3 $ enumExpString "a" 1000 100
    ]
  , C.bgroup "list"
    [ runNativeBenchmarkPrepared [("x", list), ("needle", PString "b")] pdb title "(contains needle x)"
    | (title, list) <- take 3 $ enumExpList 1000 100
    ]
  ]

benchSort :: BuiltinBenches
benchSort pdb =
  [ C.bgroup "flat"
    [ runNativeBenchmarkPrepared [("x", list)] pdb title "(sort x)"
    | (title, list) <- take 3 $ enumExpList 1000 100
    ]
  , C.bgroup "nested"
    [ runNativeBenchmarkPrepared [("x", list)] pdb title "(sort x)"
    | (title, list) <- take 3 $ enumExpListDeep 3 5 4
    ]
  , C.bgroup "object-simple"
    [ runNativeBenchmarkPrepared [("x", objs), ("keys", keys)] pdb title "(sort keys x)"
    | (_, obj@(PObject m)) <- [head $ enumExpObject 1000 1]
    , (repTitle, reps) <- take 3 $ enumExpNum 1000 3
    , (keysTitle, keysLen) <- take 3 $ enumExpNum 10 2
    , let title = repTitle <> "_" <> keysTitle
    , let objs = PList $ V.replicate (fromIntegral reps) obj
    , let keys = PList $ V.fromList $ fmap fieldToValue $ take (fromIntegral keysLen) $ M.keys m
    ]
  , C.bgroup "object-complex"
    [ runNativeBenchmarkPrepared [("x", objs), ("keys", keys)] pdb title "(sort keys x)"
    | (_, obj@(PObject m)) <- [head $ enumExpObjectComplex 1000 1]
    , (repTitle, reps) <- take 3 $ enumExpNum 1000 3
    , (keysTitle, keysLen) <- take 3 $ enumExpNum 10 2
    , let title = repTitle <> "_" <> keysTitle
    , let objs = PList $ V.replicate (fromIntegral reps) obj
    , let keys = PList $ V.fromList $ fmap fieldToValue $ take (fromIntegral keysLen) $ M.keys m
    ]
  ]

benchRemove :: BuiltinBenches
benchRemove pdb =
  [ runNativeBenchmarkPrepared [("x", obj), ("key", key)] pdb title "(remove key x)"
  | (title, obj@(PObject m)) <- take 3 $ enumExpObject 1000 100
  , let key = fieldToValue $ last $ M.keys m
  ]

benchDistinct :: BuiltinBenches
benchDistinct pdb =
  [ C.bgroup "flat"
    [ runNativeBenchmarkPrepared [("x", list)] pdb title [text|(distinct x)|]
    | (title, list) <- take 3 $ enumExpList 1000 2
    ]
  , C.bgroup "nested"
    [ runNativeBenchmarkPrepared [("x", list)] pdb title [text|(distinct x)|]
    | (title, list) <- take 3 $ enumExpListDeep 3 5 4
    ]
  ]

benchEnumerate :: BuiltinBenches
benchEnumerate pdb =
  [ runNativeBenchmark pdb title [text|(enumerate 0 $cnt)|]
  | (title, cnt) <- take 3 $ enumExpText 1000 10
  ]

benchesForBuiltin :: CoreBuiltin -> BuiltinBenches
benchesForBuiltin bn = case bn of
  CoreAdd -> benchArithBinOp "+" <> benchAddNonArithOverloads
  CoreSub -> benchArithBinOp "-"
  CoreMultiply -> benchArithBinOp "*"
  CoreDivide -> benchArithBinOp "/"
  CoreNegate -> benchNegate
  CoreAbs -> benchArithUnOp "abs"
  CorePow -> benchArithBinOp' 100 "^"
  CoreNot -> omittedDeliberately
  CoreEq -> benchEqOp "="
  CoreNeq -> benchEqOp "!="
  CoreGT -> benchEqOp ">"
  CoreGEQ -> benchEqOp ">="
  CoreLT -> benchEqOp "<"
  CoreLEQ -> benchEqOp "<="
  CoreBitwiseAnd -> benchBitwiseBinOp 1000000 "&"
  CoreBitwiseOr -> benchBitwiseBinOp 1000000 "|"
  CoreBitwiseXor -> benchBitwiseBinOp 1000000 "xor"
  CoreBitwiseFlip -> benchBitwiseFlip
  CoreBitShift -> benchBitwiseBinOp 1000 "shift"
  CoreRound -> benchFloatingUnOp "round"
  CoreCeiling -> benchFloatingUnOp "ceiling"
  CoreFloor -> benchFloatingUnOp "floor"
  CoreRoundPrec -> benchFloatingMixedOp "round"
  CoreCeilingPrec -> benchFloatingMixedOp "ceiling"
  CoreFloorPrec -> benchFloatingMixedOp "floor"
  CoreExp -> benchArithUnOp "exp"
  CoreLn -> benchArithUnOp "ln"
  CoreSqrt -> benchArithUnOp "sqrt"
  CoreLogBase -> benchArithBinOp "log"
  CoreLength -> benchLength
  CoreTake -> benchTakeDrop "take"
  CoreDrop -> benchTakeDrop "drop"
  CoreConcat -> benchConcat
  CoreReverse -> benchReverse
  CoreContains -> benchContains
  CoreSort -> benchSort
  CoreRemove -> benchRemove
  CoreDistinct -> benchDistinct
  CoreEnumerate -> benchEnumerate
  _ -> const []
  where
  omittedDeliberately = const []

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
