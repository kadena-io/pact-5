{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Pact.Core.GasModel.BuiltinsGas where

import qualified Criterion as C
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Database.SQLite3 as SQL
import Control.Monad
import Data.Bifunctor
import Data.Default
import NeatInterpolation (text)

import Pact.Core.IR.Term
import Pact.Core.Builtin
import Pact.Core.Capabilities
import Pact.Core.Environment
import Pact.Core.Gas
import Pact.Core.Guards
import Pact.Core.Literal
import Pact.Core.Names
import Pact.Core.Namespace
import Pact.Core.PactValue
import Pact.Core.Persistence
import Pact.Core.Persistence.SQLite
import Pact.Core.Serialise (serialisePact)
import Pact.Core.Type
import Pact.Time

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

enumExpObjectWithStrings :: Int -> Integer -> Integer -> [(String, PactValue)]
enumExpObjectWithStrings repCount base mult =
  [ (title, PObject $ mkMap (PString . T.replicate repCount . T.pack . show) cnt)
  | (title, cnt) <- enumExpNum base mult
  ]

enumExpString :: T.Text -> Integer -> Integer -> [(String, PactValue)]
enumExpString rep base mult =
  [ (title, PString $ T.replicate (fromIntegral cnt) rep)
  | (title, cnt) <- enumExpNum base mult
  ]

enumExpScopedIdent :: Integer -> Integer -> [(String, PactValue)]
enumExpScopedIdent base mult =
  [ (title, PString $ str <> "." <> str)
  | (title, PString str) <- enumExpString "a" base mult
  ]

type BuiltinBenches = PactDb CoreBuiltin () -> [C.Benchmark]

benchArithBinOp :: T.Text -> BuiltinBenches
benchArithBinOp op pdb =
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
  vals = take 3 $ enumExpText 1_000 1_000_000

benchPow :: BuiltinBenches
benchPow pdb =
  [ C.bgroup "integer"
    [ runNativeBenchmark pdb (xTitle <> "_" <> yTitle) [text|(^ $x $y)|]
    | (xTitle, x) <- take 3 $ enumExpText 1_000 100
    , (yTitle, y) <- take 3 $ enumExpText 1_000 100
    ]
  , C.bgroup "float"
    [ runNativeBenchmark pdb title [text|(^ $x.0 $x.0)|] | (title, x) <- floatVals ]
  , C.bgroup "float_int"
    [ runNativeBenchmark pdb title [text|(^ $x.0 $x)|] | (title, x) <- floatVals ]
  , C.bgroup "int_float"
    [ runNativeBenchmark pdb title [text|(^ $x $x.0)|] | (title, x) <- floatVals ]
  ]
  where
  floatVals = take 3 $ enumExpText 10 3

benchArithUnOp :: T.Text -> BuiltinBenches
benchArithUnOp op pdb =
  [ C.bgroup "integer"
    [ runNativeBenchmark pdb title [text|($op $x)|] | (title, x) <- vals ]
  , C.bgroup "float"
    [ runNativeBenchmark pdb title [text|($op $x.0)|] | (title, x) <- vals ]
  ]
  where
  vals = take 3 $ enumExpText 1_000 1_000_000

benchAddNonArithOverloads :: BuiltinBenches
benchAddNonArithOverloads pdb =
  [ C.bgroup "list-shallow"
    [ runNativeBenchmarkPrepared [("x", list)] pdb title [text|(+ x x)|]
    | (title, list) <- take 3 $ enumExpList 1_000 100
    ]
  , C.bgroup "list-deep"
    [ runNativeBenchmarkPrepared [("x", list)] pdb title [text|(+ x x)|]
    | (title, list) <- take 3 $ enumExpListDeep 3 5 3
    ]
  , C.bgroup "object"
    [ runNativeBenchmarkPrepared [("x", obj)] pdb title [text|(+ x x)|]
    | (title, obj) <- take 3 $ enumExpObject 1_000 100
    ]
  , C.bgroup "string"
    [ runNativeBenchmarkPrepared [("x", str)] pdb title [text|(+ x x)|]
    | (title, str) <- take 3 $ enumExpString "a" 1_000 1_000
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
  vals = take 3 $ enumExpText 1_000 1_000_000

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
    | (title, list) <- take 3 $ enumExpList 1_000 100
    ]
  listsNeq =
    [ runNativeBenchmarkPrepared [("x", list)] pdb title [text|($op x [])|]
    | (title, list) <- take 3 $ enumExpList 1_000 100
    ]
  listsDeep =
    [ runNativeBenchmarkPrepared [("x", list)] pdb title [text|($op x x)|]
    | (title, list) <- take 3 $ enumExpListDeep 3 5 3
    ]
  objsSimple =
    [ runNativeBenchmarkPrepared [("x", obj)] pdb title [text|($op x x)|]
    | (title, obj) <- take 3 $ enumExpObject 1_000 10
    ]
  objsComplex =
    [ runNativeBenchmarkPrepared [("x", obj)] pdb title [text|($op x x)|]
    | (title, obj) <- take 3 $ enumExpObjectComplex 1_000 2
    ]

benchIntegerBinOp :: Integer -> T.Text -> BuiltinBenches
benchIntegerBinOp growth op pdb =
  [ runNativeBenchmark pdb title [text|($op $x $x)|]
  | (title, x) <- take 3 $ enumExpText 1_000 growth
  ]

benchBitwiseFlip :: BuiltinBenches
benchBitwiseFlip pdb =
  [ runNativeBenchmark pdb title [text|(~ $x $x)|]
  | (title, x) <- take 3 $ enumExpText 1_000 1_000_000
  ]

benchFloatingUnOp :: T.Text -> BuiltinBenches
benchFloatingUnOp op pdb =
  [ runNativeBenchmark pdb title [text|($op $x.0)|]
  | (title, x) <- take 3 $ enumExpText 1_000 1_000
  ]

benchFloatingMixedOp :: T.Text -> BuiltinBenches
benchFloatingMixedOp op pdb =
  [ runNativeBenchmark pdb title [text|($op $x.0 $x)|]
  | (title, x) <- take 3 $ enumExpText 1_000 1_000
  ]

benchLength :: BuiltinBenches
benchLength pdb =
  [ C.bgroup "list"
    [ runNativeBenchmarkPrepared [("x", list)] pdb title [text|(length x)|]
    | (title, list) <- take 3 $ enumExpList 1_000 100
    ]
  , C.bgroup "string"
    [ runNativeBenchmarkPrepared [("x", str)] pdb title [text|(length x)|]
    | (title, str) <- take 3 $ enumExpString "a" 1_000 1_000
    ]
  , C.bgroup "object"
    [ runNativeBenchmarkPrepared [("x", obj)] pdb title [text|(length x)|]
    | (title, obj) <- take 3 $ enumExpObject 1_000 10
    ]
  , C.bgroup "object-complex"
    [ runNativeBenchmarkPrepared [("x", obj)] pdb title [text|(length x)|]
    | (title, obj) <- take 3 $ enumExpObjectComplex 1_000 2
    ]
  ]

fieldToValue :: Field -> PactValue
fieldToValue = PString . _field

benchTakeDrop :: T.Text -> BuiltinBenches
benchTakeDrop op pdb =
  [ C.bgroup "list"
    [ runNativeBenchmarkPrepared [("x", list), ("len", PInteger len)] pdb title [text|($op len x)|]
    | (listTitle, list@(PList vec)) <- take 3 $ enumExpList 1_000 100
    , (takeTitle, len) <- take 3 $ enumExpNum 1_000 100
    , fromIntegral len <= V.length vec
    , let title = listTitle <> "_" <> takeTitle
    ]
  , C.bgroup "string"
    [ runNativeBenchmarkPrepared [("x", str), ("len", PInteger len)] pdb title [text|($op len x)|]
    | (strTitle, str@(PString s)) <- take 3 $ enumExpString "a" 1_000 100
    , (takeTitle, len) <- take 3 $ enumExpNum 1_000 100
    , fromIntegral len <= T.length s
    , let title = strTitle <> "_" <> takeTitle
    ]
  , C.bgroup "object"
    [ runNativeBenchmarkPrepared [("x", obj), ("keys", PList keys)] pdb title [text|($op keys x)|]
    | (strTitle, obj@(PObject m)) <- take 3 $ enumExpObject 1_000 100
    , (takeTitle, len) <- take 3 $ enumExpNum 1_000 100
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
    | (title, list) <- take 3 $ enumExpList 1_000 100
    ]
  , C.bgroup "string"
    [ runNativeBenchmarkPrepared [("x", str)] pdb title "(reverse x)"
    | (title, str) <- take 3 $ enumExpString "a" 1_000 100
    ]
  ]

benchContains :: BuiltinBenches
benchContains pdb =
  [ C.bgroup "object-first"
    [ runNativeBenchmarkPrepared [("x", obj), ("key", key)] pdb title "(contains key x)"
    | (title, obj@(PObject m)) <- take 3 $ enumExpObject 1_000 100
    , let key = fieldToValue $ head $ M.keys m
    ]
  , C.bgroup "object-last"
    [ runNativeBenchmarkPrepared [("x", obj), ("key", key)] pdb title "(contains key x)"
    | (title, obj@(PObject m)) <- take 3 $ enumExpObject 1_000 100
    , let key = fieldToValue $ last $ M.keys m
    ]
  , C.bgroup "object-nonexistent"
    [ runNativeBenchmarkPrepared [("x", obj), ("key", key)] pdb title "(contains key x)"
    | (title, obj@(PObject m)) <- take 3 $ enumExpObject 1_000 100
    , let key = PString $ (<> "surelydoesntexist") $ _field $ last $ M.keys m
    ]
  , C.bgroup "string-short"
    [ runNativeBenchmarkPrepared [("x", str), ("needle", PString "b")] pdb title "(contains needle x)"
    | (title, str) <- take 3 $ enumExpString "a" 1_000 100
    ]
  , C.bgroup "string-long-failfast"
    [ runNativeBenchmarkPrepared [("x", str), ("needle", needle)] pdb title "(contains needle x)"
    | let needle = PString $ T.replicate 1_000 "b"
    , (title, str) <- take 3 $ enumExpString "a" 1_000 100
    ]
  , C.bgroup "string-long-failfar"
    [ runNativeBenchmarkPrepared [("x", str), ("needle", needle)] pdb title "(contains needle x)"
    | let needle = PString $ T.replicate 999 "a" <> "b"
    , (title, str) <- take 3 $ enumExpString "a" 1_000 100
    ]
  , C.bgroup "list"
    [ runNativeBenchmarkPrepared [("x", list), ("needle", PString "b")] pdb title "(contains needle x)"
    | (title, list) <- take 3 $ enumExpList 1_000 100
    ]
  ]

benchSort :: BuiltinBenches
benchSort pdb =
  [ C.bgroup "flat"
    [ runNativeBenchmarkPrepared [("x", list)] pdb title "(sort x)"
    | (title, list) <- take 3 $ enumExpList 1_000 100
    ]
  , C.bgroup "nested"
    [ runNativeBenchmarkPrepared [("x", list)] pdb title "(sort x)"
    | (title, list) <- take 3 $ enumExpListDeep 3 5 4
    ]
  , C.bgroup "object-simple"
    [ runNativeBenchmarkPrepared [("x", objs), ("ks", keys)] pdb title "(sort ks x)"
    | (_, obj@(PObject m)) <- [head $ enumExpObject 1_000 1]
    , (repTitle, reps) <- take 3 $ enumExpNum 1_000 4
    , (keysTitle, keysLen) <- take 3 $ enumExpNum 10 2
    , let title = repTitle <> "_" <> keysTitle
    , let objs = PList $ V.replicate (fromIntegral reps) obj
    , let keys = PList $ V.fromList $ fmap fieldToValue $ take (fromIntegral keysLen) $ M.keys m
    ]
  , C.bgroup "object-complex"
    [ runNativeBenchmarkPrepared [("x", objs), ("ks", keys)] pdb title "(sort ks x)"
    | (_, obj@(PObject m)) <- [head $ enumExpObjectComplex 1_000 1]
    , (repTitle, reps) <- take 3 $ enumExpNum 100 4
    , (keysTitle, keysLen) <- take 3 $ enumExpNum 10 2
    , let title = repTitle <> "_" <> keysTitle
    , let objs = PList $ V.replicate (fromIntegral reps) obj
    , let keys = PList $ V.fromList $ fmap fieldToValue $ take (fromIntegral keysLen) $ M.keys m
    ]
  ]

benchRemove :: BuiltinBenches
benchRemove pdb =
  [ runNativeBenchmarkPrepared [("x", obj), ("key", key)] pdb title "(remove key x)"
  | (title, obj@(PObject m)) <- take 3 $ enumExpObject 1_000 100
  , let key = fieldToValue $ last $ M.keys m
  ]

benchMap :: BuiltinBenches
benchMap pdb =
  [ runNativeBenchmarkPrepared [("x", list)] pdb title "(map (lambda (e) e) x)"
  | (title, list) <- take 3 $ enumExpList 10_000 10
  ]

benchFilter :: BuiltinBenches
benchFilter pdb =
  [ C.bgroup "all"
    [ runNativeBenchmarkPrepared [("x", list)] pdb title "(filter (lambda (e) true) x)"
    | (title, list) <- take 3 $ enumExpList 10_000 10
    ]
  , C.bgroup "none"
    [ runNativeBenchmarkPrepared [("x", list)] pdb title "(filter (lambda (e) false) x)"
    | (title, list) <- take 3 $ enumExpList 10_000 10
    ]
  ]

benchZip :: BuiltinBenches
benchZip pdb =
  [ runNativeBenchmarkPrepared [("x", list)] pdb title "(zip (lambda (a b) a) x x)"
  | (title, list) <- take 3 $ enumExpList 10_000 10
  ]

benchIntToStr :: BuiltinBenches
benchIntToStr pdb =
  [ C.bgroup "basic"
    [ runNativeBenchmark pdb (T.unpack base <> "_" <> title) [text|(int-to-str $base $x)|]
    | base <- ["2", "10", "16"]
    , (title, x) <- take 3 $ enumExpText 1_000 10_000
    ]
  , C.bgroup "base64"
    [ runNativeBenchmark pdb title [text|(int-to-str 64 $x)|]
    | (title, x) <- take 3 $ enumExpText 1_000 10_000
    ]
  ]

benchStrToInt :: BuiltinBenches
benchStrToInt pdb =
  [ C.bgroup "single-arg"
    [ runNativeBenchmarkPrepared [("x", str)] pdb title "(str-to-int x)"
    | (title, str) <- take 3 $ enumExpString "1" 100 2
    ]
  , C.bgroup "with-base"
    [ runNativeBenchmarkPrepared [("x", str)] pdb (T.unpack base <> "_" <> title) [text|(str-to-int $base x)|]
    | base <- ["2", "10", "16"]
    , (title, str) <- take 3 $ enumExpString "1" 100 2
    ]
  ]

benchFold :: BuiltinBenches
benchFold pdb =
  [ runNativeBenchmarkPrepared [("x", list)] pdb title "(fold (lambda (a b) a) 0 x)"
  | (title, list) <- take 3 $ enumExpList 10_000 10
  ]

benchFormat :: BuiltinBenches
benchFormat pdb =
  [ C.bgroup "simple"
    [ runNativeBenchmarkPrepared [("str", str), ("list", list)] pdb title "(format str list)"
    | (title, list@(PList vec)) <- take 3 $ enumExpList 10_000 10
    , let len = V.length vec
    , let str = PString $ T.replicate len "{} "
    ]
  , C.bgroup "complex"
    [ runNativeBenchmarkPrepared [("str", str), ("list", list)] pdb title "(format str list)"
    | (title, list@(PList vec)) <- take 3 $ enumExpListDeep 3 10 2
    , let len = V.length vec
    , let str = PString $ T.replicate len "{} "
    ]
  ]

benchEnumerate :: BuiltinBenches
benchEnumerate pdb =
  [ C.bgroup "no-step"
    [ runNativeBenchmark pdb title [text|(enumerate 0 $cnt)|]
    | (title, cnt) <- take 3 $ enumExpText 1_000 10
    ]
  , C.bgroup "with-step"
    [ runNativeBenchmark pdb title [text|(enumerate 0 $cnt 1)|]
    | (title, cnt) <- take 3 $ enumExpText 1_000 10
    ]
  ]

benchShow :: BuiltinBenches
benchShow pdb =
  [ C.bgroup "integer"
    [ runNativeBenchmarkPrepared [("x", PInteger num)] pdb title "(show x)"
    | (title, num) <- take 3 $ enumExpNum 1_000 1_000
    ]
  , C.bgroup "decimal"
    [ runNativeBenchmark pdb title [text|(show $num.0)|]
    | (title, num) <- take 3 $ enumExpText 1_000 1_000
    ]
  , C.bgroup "string"
    [ runNativeBenchmarkPrepared [("x", str)] pdb title "(show x)"
    | (title, str) <- take 3 $ enumExpString "a" 1_000 100
    ]
  , C.bgroup "list/flat"
    [ runNativeBenchmarkPrepared [("x", lst)] pdb title "(show x)"
    | (title, lst) <- take 3 $ enumExpList 1_000 10
    ]
  , C.bgroup "list/deep"
    [ runNativeBenchmarkPrepared [("x", lst)] pdb title "(show x)"
    | (title, lst) <- take 3 $ enumExpListDeep 3 6 2
    ]
  ]

benchDistinct :: BuiltinBenches
benchDistinct pdb =
  [ C.bgroup "flat"
    [ runNativeBenchmarkPrepared [("x", list)] pdb title "(distinct x)"
    | (title, list) <- take 3 $ enumExpList 1_000 2
    ]
  , C.bgroup "nested"
    [ runNativeBenchmarkPrepared [("x", list)] pdb title "(distinct x)"
    | (title, list) <- take 3 $ enumExpListDeep 3 5 4
    ]
  ]

benchReadOp :: T.Text -> BuiltinBenches
benchReadOp readOp pdb =
  [ runNativeBenchmarkPreparedEnvMod (msgBody obj) [("k", key)] pdb title [text|($readOp k)|]
  | (title, PObject obj) <- take 3 $ enumExpObjectWithStrings 100 1_000 100
  , let key = fieldToValue $ last $ M.keys obj
  ]

benchReadString :: BuiltinBenches
benchReadString pdb =
  [ runNativeBenchmarkPreparedEnvMod (msgBody obj) [("k", key)] pdb title "(read-string k)"
  | (title, PObject obj) <- take 3 $ enumExpObjectWithStrings 10 1_000 100
  , let key = fieldToValue $ last $ M.keys obj
  ]

benchReadKeyset :: BuiltinBenches
benchReadKeyset pdb =
  [ runNativeBenchmarkPreparedEnvMod (msgBody obj) [] pdb title "(read-keyset 'thekeyset)"
  | (_, PObject objBase) <- [head $ enumExpObjectWithStrings 10 1_000 1]
  , (title, str) <- take 3 $ enumExpScopedIdent 1_000 100
  , let ksn = PObject [(Field "keys", PList [PString "publickeytext"]), (Field "pred", str)]
  , let obj = M.insert (Field "thekeyset") ksn objBase
  ]

benchKeysetGuardOp :: T.Text -> BuiltinBenches
benchKeysetGuardOp op pdb = dummyTx pdb initDb
  [ runNativeBenchmarkPreparedEnvMod (msgSigsNoCap [pkt]) [("x", str)] pdb title [text|($op x)|]
  | (title, str) <- keys
  ]
  where
  pkt = PublicKeyText "somepubkey"
  keys = take 3 $ enumExpScopedIdent 10_000 10
  initDb = forM_ keys $ \(_title, ident) -> case ident of
    PString s
      | Right ksn <- parseAnyKeysetName s -> ignoreGas def $
          _pdbWrite pdb Write DKeySets ksn $ KeySet [pkt] KeysAny
    _ -> error "not a string"

benchAt :: BuiltinBenches
benchAt pdb =
  [ C.bgroup "list"
    [ runNativeBenchmarkPrepared [("x", list), ("pos", pos)] pdb title "(at pos x)"
    | (title, list@(PList v)) <- take 3 $ enumExpList 1_000 100
    , let pos = PInteger $ fromIntegral $ V.length v `div` 2
    ]
  , C.bgroup "object"
    [ runNativeBenchmarkPrepared [("x", obj)] pdb title "(at \"999\" x)"
    | (title, obj) <- take 3 $ enumExpObject 1_000 100
    ]
  ]

benchMakeList :: BuiltinBenches
benchMakeList pdb =
  [ C.bgroup "flat"
    [ runNativeBenchmarkPrepared [("cnt", PInteger cnt)] pdb title "(make-list cnt 0)"
    | (title, cnt) <- take 3 $ enumExpNum 1_000 100
    ]
  , C.bgroup "nested"
    [ runNativeBenchmarkPrepared [("cnt", PInteger cnt), ("elt", elt)] pdb title "(make-list cnt elt)"
    | (repTitle, cnt) <- take 3 $ enumExpNum 1_000 10
    , (lstTitle, elt) <- take 3 $ enumExpList 100 2
    , let title = repTitle <> "_" <> lstTitle
    ]
  ]

benchB64Op :: T.Text -> BuiltinBenches
benchB64Op op pdb =
  [ runNativeBenchmarkPrepared [("s", str)] pdb title [text|($op s)|]
  | (title, str) <- take 3 $ enumExpString "YWEK" 250 100
  ]

benchStrToList :: BuiltinBenches
benchStrToList pdb =
  [ runNativeBenchmarkPrepared [("s", str)] pdb title "(str-to-list s)"
  | (title, str) <- take 3 $ enumExpString "a" 1_000 10
  ]

benchRequireCapability :: BuiltinBenches
benchRequireCapability pdb =
  [ C.bgroup "flat"
    [ runNativeBenchmarkPreparedStMod (stCaps capToks) [("c", capTok)] pdb title "(require-capability c)"
    | let capTok = PCapToken $ CapToken (mkGasModelFqn "0") []
    , (title, cnt) <- take 3 $ enumExpNum 1_000 10
    , let capToks = [ CapToken (fqnToQualName $ mkGasModelFqn $ T.pack $ show n) [] | n <- [0..cnt] ]
    ]
  , C.bgroup "nested"
    [ runNativeBenchmarkPreparedStMod (stCaps capToks) [("c", capTok)] pdb (cntTitle <> "_" <> argTitle) "(require-capability c)"
    | (cntTitle, cnt) <- take 3 $ enumExpNum 10 10
    , (argTitle, pv) <- take 3 $ enumExpListDeep 2 5 3
    , let capTok = PCapToken $ CapToken (mkGasModelFqn "0") [pv, PInteger cnt]
    , let capToks = [ CapToken (fqnToQualName $ mkGasModelFqn "0") [pv, PInteger n] | n <- [0..cnt] ]
    ]
  ]

benchComposeCapability :: BuiltinBenches
benchComposeCapability pdb =
  [ C.bgroup "flat"
    [ runNativeBenchmarkPreparedStMod stMod [("c", theCapTok)] pdb title "(compose-capability c)"
    | (title, cnt) <- take 3 $ enumExpNum 1_000 10
    , let sf = StackFrame (mkGasModelFqn "") [] SFDefcap ()
          theCapName = T.pack $ "theCap" <> show cnt
          theCapTok = PCapToken $ CapToken (mkGasModelFqn theCapName) []
          theCapDef = DCap $ DefCap (Arg theCapName Nothing ()) [] (Constant (LBool True) ()) (DefManaged AutoManagedMeta) ()
          capToksStack = [ CapToken (fqnToQualName $ mkGasModelFqn $ T.pack $ show n) [] | n <- [0..cnt] ]
          manageds = [ ManagedCap ct ct (AutoManaged False)
                     | n <- [0..cnt]
                     , let ct = CapToken (fqnToQualName $ mkGasModelFqn $ T.pack $ "theCap" <> show n) []
                     ]
    , let stMod = stStack [sf] <> stCaps capToksStack <> stManaged manageds <> stAddDef theCapName theCapDef
    ]
  , C.bgroup "nested"
    [ runNativeBenchmarkPreparedStMod stMod [("c", theCapTok)] pdb title "(compose-capability c)"
    | (cntTitle, cnt) <- take 3 $ enumExpNum 10 10
    , (argTitle, pv) <- take 3 $ enumExpListDeep 2 5 3
    , let title = cntTitle <> "_" <> argTitle
          sf = StackFrame (mkGasModelFqn "") [] SFDefcap ()
          theCapName = T.pack $ "theCap" <> show cnt
          theCapTok = PCapToken $ CapToken (mkGasModelFqn theCapName) [pv, PInteger (-1)]
          theCapDef = DCap $ DefCap (Arg theCapName Nothing ()) [Arg "a" Nothing (), Arg "b" Nothing ()] (Constant (LBool True) ()) (DefManaged AutoManagedMeta) ()
          capToksStack = [ CapToken (fqnToQualName $ mkGasModelFqn theCapName) [pv, PInteger n] | n <- [0..cnt] ]
          manageds = [ ManagedCap ct ct (AutoManaged False)
                     | n <- [0..cnt]
                     , let ct = CapToken (fqnToQualName $ mkGasModelFqn $ T.pack $ "theCap" <> show n) [pv, PInteger (-1)]
                     ]
    , let stMod = stStack [sf] <> stCaps capToksStack <> stManaged manageds <> stAddDef theCapName theCapDef
    ]
  ]

benchInstallCapability :: BuiltinBenches
benchInstallCapability pdb =
  [ C.bgroup "flat"
    [ runNativeBenchmarkPreparedStMod (stManaged manageds <> stAddDef "theCap" theCapDef) [("c", capTok)] pdb title "(install-capability c)"
    | let capTok = PCapToken $ CapToken (mkGasModelFqn "theCap") []
          theCapDef = DCap $ DefCap (Arg "theCap" Nothing ()) [] (Constant (LBool True) ()) (DefManaged AutoManagedMeta) ()
    , (title, cnt) <- take 3 $ enumExpNum 1_000 100
    , let manageds = [ ManagedCap ct ct (AutoManaged False)
                     | n <- [0..cnt]
                     , let ct = CapToken (fqnToQualName $ mkGasModelFqn $ T.pack $ show n) []
                     ]
    ]
  , C.bgroup "nested"
    [ runNativeBenchmarkPreparedStMod (stManaged manageds <> stAddDef "theCap" theCapDef) [("c", capTok)] pdb title "(install-capability c)"
    | let theCapDef = DCap $ DefCap (Arg "theCap" Nothing ()) [Arg "a1" Nothing (), Arg "a2" Nothing ()] (Constant (LBool True) ()) (DefManaged AutoManagedMeta) ()
          capFqn = mkGasModelFqn "theCap"
    , (cntTitle, cnt) <- take 3 $ enumExpNum 10 10
    , (argTitle, pv) <- take 3 $ enumExpListDeep 3 5 3
    , let title = cntTitle <> "_" <> argTitle
          capTok = PCapToken $ CapToken capFqn [pv, PInteger (-1)]
          manageds = [ ManagedCap ct ct (AutoManaged False)
                     | n <- [0..cnt]
                     , let ct = CapToken (fqnToQualName capFqn) [pv, PInteger n]
                     ]
    ]
  ]

benchParseTime :: BuiltinBenches
benchParseTime pdb =
  [ C.bgroup "matching length"
    [ runNativeBenchmarkPrepared [("f", fmtStr), ("t", timeStr)] pdb title "(parse-time f t)"
    | (title, reps) <- take 3 $ enumExpNum 100 10
    , let fmtStr = PString $ T.replicate (fromIntegral reps) "%Y"
          timeStr = PString $ T.replicate (fromIntegral reps) "2020"
    ]
  , C.bgroup "fmt shorter"
    [ runNativeBenchmarkPrepared [("f", PString "%Y"), ("t", timeStr)] pdb title "(parse-time f t)"
    | (title, reps) <- take 3 $ enumExpNum 100 10
    , let timeStr = PString $ T.replicate (fromIntegral reps) "2020"
    ]
  ]

benchFormatTime :: BuiltinBenches
benchFormatTime pdb =
  [ runNativeBenchmarkPrepared [("f", fmtStr), ("t", time)] pdb title "(format-time f t)"
  | (title, reps) <- take 3 $ enumExpNum 100 10
  , let fmtStr = PString $ T.replicate (fromIntegral reps) "%Y"
        time = PTime posixEpoch
  ]

benchEmitEvent :: BuiltinBenches
benchEmitEvent pdb = [ runNativeBenchmarkPreparedStMod stMod [("c", capTok)] pdb "novar" "(emit-event c)" ]
  where
  capTok = PCapToken $ CapToken (mkGasModelFqn "theCap") []
  theCapDef = DCap $ DefCap (Arg "theCap" Nothing ()) [] (Constant (LBool True) ()) (DefManaged AutoManagedMeta) ()
  sf = StackFrame (mkGasModelFqn "") [] SFDefcap ()
  stMod = stAddDef "theCap" theCapDef <> stStack [sf]

benchCreatePrincipal :: BuiltinBenches
benchCreatePrincipal pdb =
  [ C.bgroup "keyset/pred"
    [ runNativeBenchmarkPrepared [("ks", keyset)] pdb title "(create-principal ks)"
    | (title, reps) <- take 3 $ enumExpNum 1_000 10
    , let predName = BareName $ T.replicate (fromIntegral reps) "a"
          keys = S.fromList [ PublicKeyText "k1", PublicKeyText "k2" ]
          keyset = PGuard $ GKeyset $ KeySet keys (CustomPredicate $ TBN predName)
    ]
  , C.bgroup "keyset/keys"
    [ runNativeBenchmarkPrepared [("ks", keyset)] pdb title "(create-principal ks)"
    | (title, reps) <- take 3 $ enumExpNum 100 10
    , let keys = S.fromList [ PublicKeyText $ T.pack $ "key" ++ show n | n <- [1..reps] ]
          keyset = PGuard $ GKeyset $ KeySet keys (CustomPredicate $ TBN $ BareName "somepred")
    ]
  , C.bgroup "userguard"
    [ runNativeBenchmarkPrepared [("ug", ug)] pdb (repsTitle <> "_" <> strTitle) "(create-principal ug)"
    | (repsTitle, reps) <- take 3 $ enumExpNum 100 10
    , (strTitle, str) <- take 3 $ enumExpString "a" 100 10
    , let ug = PGuard $ GUserGuard $ UserGuard (fqnToQualName $ mkGasModelFqn "somepred") (replicate (fromIntegral reps) str)
    ]
  , C.bgroup "capguard"
    [ runNativeBenchmarkPrepared [("cg", cg)] pdb (repsTitle <> "_" <> strTitle) "(create-principal cg)"
    | (repsTitle, reps) <- take 3 $ enumExpNum 100 10
    , (strTitle, str) <- take 3 $ enumExpString "a" 100 10
    , let cg = PGuard $ GCapabilityGuard $ CapabilityGuard (fqnToQualName $ mkGasModelFqn "somepred") (replicate (fromIntegral reps) str) Nothing
    ]
  ]

benchIsPrincipal :: BuiltinBenches
benchIsPrincipal pdb =
  [ runNativeBenchmarkPrepared [("s", PString $ "r:" <> str)] pdb title "(is-principal s)"
  | (title, PString str) <- take 3 $ enumExpString "a" 1_000 100
  ]

benchTypeOfPrincipal :: BuiltinBenches
benchTypeOfPrincipal pdb =
  [ runNativeBenchmarkPrepared [("s", PString $ "r:" <> str)] pdb title "(typeof-principal s)"
  | (title, PString str) <- take 3 $ enumExpString "a" 1_000 100
  ]

benchValidatePrincipal :: BuiltinBenches
benchValidatePrincipal pdb =
  [ runNativeBenchmarkPrepared [("s", PString $ "r:" <> str), ("g", g)] pdb title "(validate-principal g s)"
  | (title, PString str) <- take 3 $ enumExpString "a" 1_000 100
  , let g = PGuard $ GKeySetRef $ KeySetName str Nothing
  ]

benchNamespace :: BuiltinBenches
benchNamespace pdb = dummyTx pdb initDb
  [ runNativeBenchmarkPrepared [("s", str)] pdb title "(namespace s)"
  | (title, str) <- names
  ]
  where
  names = take 3 $ enumExpString "a" 1_000 100
  g = GKeySetRef $ KeySetName "ks" Nothing
  initDb = forM_ names $ \(_title, name) -> case name of
    PString n -> let nsn = NamespaceName n
                 in ignoreGas def $
                      _pdbWrite pdb Write DNamespaces nsn (Namespace nsn g g)
    _ -> error "not a string"

benchDefineNamespace :: BuiltinBenches
benchDefineNamespace pdb =
  [ runNativeBenchmarkPrepared [("s", str), ("g", g)] (ignoreWrites pdb) title "(define-namespace s g g)"
  | (title, str) <- take 3 $ enumExpString "a" 1_000 100
  ]
  where
  g = PGuard $ GKeySetRef $ KeySetName "ks" Nothing

benchDescribeNamespace :: BuiltinBenches
benchDescribeNamespace pdb = dummyTx pdb initDb
  [ runNativeBenchmarkPrepared [("s", str)] pdb title "(describe-namespace s)"
  | (title, str) <- names
  ]
  where
  names = take 3 $ enumExpString "a" 1_000 100
  g = GKeySetRef $ KeySetName "ks" Nothing
  initDb = forM_ names $ \(_title, name) -> case name of
    PString n -> let nsn = NamespaceName n
                 in ignoreGas def $
                      _pdbWrite pdb Write DNamespaces nsn (Namespace nsn g g)
    _ -> error "not a string"

benchChainData :: BuiltinBenches
benchChainData pdb = [runNativeBenchmark pdb "simple" "(chain-data)"]

benchIsCharset :: BuiltinBenches
benchIsCharset pdb =
  [ C.bgroup "ascii"
    [ runNativeBenchmarkPrepared [("s", str)] pdb title "(is-charset 0 s)"
    | (title, str) <- take 3 $ enumExpString "a" 1_000 100
    ]
  , C.bgroup "latin1"
    [ runNativeBenchmarkPrepared [("s", str)] pdb title "(is-charset 1 s)"
    | (title, str) <- take 3 $ enumExpString "a" 1_000 100
    ]
  ]

benchPoseidon :: BuiltinBenches
benchPoseidon pdb =
  [ runNativeBenchmark pdb (show cnt) [text|(poseidon-hash-hack-a-chain $args)|]
  | cnt <- [1..8] :: [Int]
  , let args = T.unwords $ T.pack . show <$> [1..cnt]
  ]

benchesForBuiltin :: CoreBuiltin -> BuiltinBenches
benchesForBuiltin bn = case bn of
  CoreAdd -> benchArithBinOp "+" <> benchAddNonArithOverloads
  CoreSub -> benchArithBinOp "-"
  CoreMultiply -> benchArithBinOp "*"
  CoreDivide -> benchArithBinOp "/"
  CoreNegate -> benchNegate
  CoreAbs -> benchArithUnOp "abs"
  CorePow -> benchPow
  CoreNot -> omittedDeliberately
  CoreEq -> benchEqOp "="
  CoreNeq -> benchEqOp "!="
  CoreGT -> benchEqOp ">"
  CoreGEQ -> benchEqOp ">="
  CoreLT -> benchEqOp "<"
  CoreLEQ -> benchEqOp "<="
  CoreBitwiseAnd -> benchIntegerBinOp 1_000_000 "&"
  CoreBitwiseOr -> benchIntegerBinOp 1_000_000 "|"
  CoreBitwiseXor -> benchIntegerBinOp 1_000_000 "xor"
  CoreBitwiseFlip -> benchBitwiseFlip
  CoreBitShift -> benchIntegerBinOp 1_000 "shift"
  CoreMod -> benchIntegerBinOp 1_000 "mod"
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
  CoreSortObject -> alreadyCovered
  CoreRemove -> benchRemove
  CoreMap -> benchMap
  CoreZip -> benchZip
  CoreFilter -> benchFilter
  CoreIntToStr -> benchIntToStr
  CoreStrToInt -> benchStrToInt
  CoreStrToIntBase -> alreadyCovered
  CoreFold -> benchFold
  CoreDistinct -> benchDistinct
  CoreFormat -> benchFormat
  CoreEnumerate -> benchEnumerate
  CoreEnumerateStepN -> alreadyCovered
  CoreShow -> benchShow
  CoreReadMsg -> benchReadOp "read-msg"
  CoreReadMsgDefault -> omittedDeliberately
  CoreReadInteger -> benchReadOp "read-integer"
  CoreReadDecimal -> benchReadOp "read-decimal"
  CoreReadString -> benchReadString
  CoreReadKeyset -> benchReadKeyset
  CoreEnforceGuard -> benchKeysetGuardOp "enforce-guard"
  CoreEnforceKeyset -> alreadyCovered
  CoreKeysetRefGuard -> benchKeysetGuardOp "keyset-ref-guard"
  CoreAt -> benchAt
  CoreMakeList -> benchMakeList
  CoreB64Encode -> benchB64Op "base64-encode"
  CoreB64Decode -> benchB64Op "base64-decode"
  CoreStrToList -> benchStrToList
  CoreYield -> todo
  CoreYieldToChain -> todo
  CoreResume -> todo
  CoreBind -> omittedDeliberately
  CoreRequireCapability -> benchRequireCapability
  CoreComposeCapability -> benchComposeCapability
  CoreInstallCapability -> benchInstallCapability
  CoreEmitEvent -> benchEmitEvent
  CoreCreateCapabilityGuard -> omittedDeliberately
  CoreCreateCapabilityPactGuard -> omittedDeliberately
  CoreCreateModuleGuard -> omittedDeliberately
  CoreCreateDefPactGuard -> omittedDeliberately
  CoreParseTime -> benchParseTime
  CoreFormatTime -> benchFormatTime
  CoreTime -> omittedDeliberately
  CoreAddTime -> omittedDeliberately
  CoreDiffTime -> omittedDeliberately
  CoreHours -> omittedDeliberately
  CoreMinutes -> omittedDeliberately
  CoreDays -> omittedDeliberately
  CoreCompose -> omittedDeliberately
  CoreCreatePrincipal -> benchCreatePrincipal
  CoreIsPrincipal -> benchIsPrincipal
  CoreTypeOfPrincipal -> benchTypeOfPrincipal
  CoreValidatePrincipal -> benchValidatePrincipal
  CoreNamespace -> benchNamespace
  CoreDefineNamespace -> benchDefineNamespace
  CoreDescribeNamespace -> benchDescribeNamespace
  CoreChainData -> benchChainData
  CoreIsCharset -> benchIsCharset
  CorePactId -> omittedDeliberately
  CoreZkPairingCheck -> omittedDeliberately
  CoreZKScalarMult -> omittedDeliberately
  CoreZkPointAdd -> omittedDeliberately
  CorePoseidonHashHackachain -> benchPoseidon
  CoreTypeOf -> omittedDeliberately
  CoreDec -> omittedDeliberately
  CoreCond -> omittedDeliberately
  _ -> const []
  where
  omittedDeliberately = const []
  alreadyCovered = const []
  todo = const []

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
  mkPactDb = unsafeCreateSqlitePactDb serialisePact ":memory:"

  cleanupPactDb (_, db) = SQL.close db
