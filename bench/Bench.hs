{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Control.DeepSeq
import Criterion.Main hiding (env)
import Data.Decimal
import Data.Default (Default, def)
import Data.Int (Int64)
import Data.List qualified as List
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as Text
import Data.Word (Word64)
import Pact.Core.Builtin
import Pact.Core.Environment.Types
import Pact.Core.Guards
import Pact.Core.IR.Desugar
import Pact.Core.IR.Term
import Pact.Core.Info
import Pact.Core.Literal
import Pact.Core.Names
import Pact.Core.PactValue
import Pact.Core.Persistence.MockPersistence
import Pact.Core.Persistence.Types
import Pact.Core.Persistence.Utils hiding (getModule)
import Pact.Core.Serialise
import Pact.Core.Serialise.CBOR_V1
import Pact.Core.SizeOf
import Pact.Core.Syntax.Lexer qualified as Lisp
import Pact.Core.Syntax.ParseTree qualified as Lisp
import Pact.Core.Syntax.Parser qualified as Lisp
import Pact.Core.Type
import Pact.Time qualified as PactTime

main :: IO ()
main = do

  pdb <- mockPactDb serialisePact_repl_spaninfo
  ee <- defaultEvalEnv pdb replBuiltinMap
  let es = def
  !module1 <- expectEval ee es $ getModule exampleModule1

  let rd1 = rowData 1 1
      -- deepseq rd1
      _ = rnf rd1
      rd2 = rowData 1000 1
      _ = rnf rd2
      rd3 = rowData 10_000 1
      _ = rnf rd3
      rd4 = rowData 100_000 1
      _ = rnf rd4
      rd5 = rowData 100_000_000 1
      _ = rnf rd5

      rd1b = rowData 10 10
      rd2b = rowData 1000 10
      rd3b = rowData 1_000_000 1000
      _ = rnf (rd1b, rd2b, rd3b)

      int1 = rowInteger 1000 1
      int2 = rowInteger 1000 1_000_000_000_000
      int3 = rowInteger 2000 1_000_000_000_000

      str1 = rowString 1000 1
      str2 = rowString 1000 1000
      str3 = rowString 2000 1000

      decimal1 = rowDecimal 1 1
      decimal2 = rowDecimal 1 111_111_111
      decimal3 = rowDecimal 1 0.111_111_111
      decimal4 = rowDecimal 1000 1
      decimal5 = rowDecimal 2000 1

      bool1 = rowBool 1
      bool2 = rowBool 1000

      unit1 = rowUnit 1
      unit2 = rowUnit 1000

      time1 = rowTime 2024
      time2 = rowTime 20_000_000_000_000_000

      -- Keyset Guards
      rdKeySet1 = RowData $ M.fromList [(Field "foo", PGuard $ GKeyset $ fakeKeySet 1 64 KeysAll)]
      rdKeySet2 = RowData $ M.fromList [(Field "foo", PGuard $ GKeyset $ fakeKeySet 10 64 KeysAll)]
      rdKeySet3 = RowData $ M.fromList [(Field "foo", PGuard $ GKeyset $ fakeKeySet 1 64 (CustomPredicate (TBN (BareName (Text.replicate 1000 "0")))))]
      rdKeySet4 = RowData $ M.fromList [(Field "foo", PGuard $ GKeyset $ fakeKeySet 10 128 KeysAll)]
      _ = rnf (rdKeySet1, rdKeySet2, rdKeySet3, rdKeySet4)

      -- This data is used to test lazyness of the benchmarks. I've confirmed that
      -- this slowness does not appear in benchmark results.
      slowString =
        let fib n = if n < 2 then n else fib (n-1) + fib (n-2)
        in Text.pack $ show (fib (40:: Integer))
      rdSlow = RowData $ M.fromList [(Field "foo", PLiteral (LString slowString))]

  -- The comments list the time that each action takes, and the
  -- number of calls made to the countBytes function.
  -- In a previous version of the code, these calls were counted in
  -- the EvalState monad. We do not keep that code in the `master` branch
  -- because it is messy, but if you need it, it is the last commit of
  -- the `greg/track-count-bytes` branch.
  --
  -- 1 milligas = 2.5 nanoseconds

  let encode = ignoreGas () . encodeRowData

  print ("Running..." :: String)
  defaultMain
    [
      -- 0.003 ms / 0 calls
      bench "nil" $ nfIO $ expectEval ee es (pure ())

      -- 0.9 ms / 1 call
    , bench "int" $ nfIO $ getSize ee es SizeOfV0 (1 :: Int)

      -- 0.9 ms / 1 call
    , bench "long-string" $ nfIO $ getSize ee es SizeOfV0 longString

      -- 152 ms (60800 milligas) / 100000 calls => 0.6 milligas per call
    , bench "long-bool-list" $ nfIO $ getSize ee es SizeOfV0 longBoolList

      -- 165 ms (66000 gas) / 100000 calls => 0.6 milligas per call
    , bench "long-int-list" $ nfIO $ getSize ee es SizeOfV0 longIntList

      -- 218 ms (87200 milligas) / 100100 calls => 0.9 milligas per call
    , bench "long-nested-bool-list" $ nfIO $ getSize ee es SizeOfV0 longNestedBoolList

      -- 214 ms (85600 milligas) / 101000 calls => 0.9 milligas per call
    , bench "long-nested-bool-list-2" $ nfIO $ getSize ee es SizeOfV0 longNestedBoolList2

      -- 0.2 ms (80 miligas) / 1646 bytes / 125 calls => 0.6 milligas per call
    , bench "module-1" $ nfIO $ getSize ee es SizeOfV0 module1

      -- 36 ms (14400 milligas) / 253338 bytes / 18637 calls to countBytes => 0.8 milligas per call
      -- We remove this benchmark but save the results, because
      -- the code was unwieldy.
      -- , bench "module-coin" $ nfIO $ getSize ee es SizeOfV0 coinModule

      -- 453 ns (183 milligas)
    , bench "row-data 1" $ nfIO $ encode rd1

      -- 600 ns (60 milligas)
    , bench "row-data 2" $ nfIO $ encode rd2

      -- 5987 ns (1090 milligas)
    , bench "row-data 3" $ nfIO $ encode rd3

    , bench "row-data 4" $ nfIO $ encode rd4

      -- 5.9 micros (8500000 milligas)
    , bench "row-data 5" $ nfIO $ encode rd5

    -- 1700 nanos ()/ 10 chars
    , bench "row-data 1b" $ nfIO $ encode rd1b

    -- 1954 nanos () / 1000 characters
    , bench "row-data 2b" $ nfIO $ (ignoreGas () . encodeRowData) rd2b

    -- 337 micros () / 1_000_000 characters
    , bench "row-data 3b" $ nfIO $ (ignoreGas () . encodeRowData) rd3b

    -- 144 micros (57600 milligas) : 1000 small integers
    , bench "pact-integer-1" $ nfIO $ (ignoreGas () . encodeRowData) int1

    -- 190 micros (76000 milligas) : 1000 large integers (1e12).
    -- (this - pact-integer-2) / 1000 = 46 nanos per integer, when we move
    -- from 1 digit to 12 digits. Each digit adds 4 nanos, or 2 milligas.
    , bench "pact-integer-2" $ nfIO $ (ignoreGas () . encodeRowData) int2

    -- 381 micros (152400 milligas) : 2000 large integers (1e12).
    -- This bench differs from pact-integer-2 by having 2000 integers instead of 1000.
    -- It takes about twice as long as pact-integer-2, validating the linear
    -- effect of integer count on encoding time.
    , bench "pact-integer-3" $ nfIO $ (ignoreGas () . encodeRowData) int3

    -- 154 micros (61600 milligas) : 1000 strings of length 1
    , bench "pact-string-1" $ nfIO $ (ignoreGas () . encodeRowData) str1

    -- 327 micros (130800 milligas) : 1000 strings of length 1000.
    -- This bench differs from pact-string-1 by using strings of length 1000
    -- instead of strings of length 1.
    -- It takes an extra 173 micros, 173 nanos (69 milligas) per element.
    -- So the gas cost of a string is 69 milligas per 1000 characters.
    , bench "pact-string-2" $ nfIO $ (ignoreGas () . encodeRowData) str2

    -- 652 micros. This bench differs from pact-string-2 by having twice
    -- as many elements, and it takes twice as long, as expected.
    , bench "pact-string-3" $ nfIO $ encode str3

    -- 599 nanos (240 milligas) : 1 decimal with 1 digit
    , bench "pact-decimal-1" $ nfIO $ encode decimal1

    -- 670 nanos (268 milligas) : 1 decimal with 9 digits
    , bench "pact-decimal-2" $ nfIO $ encode decimal2

    -- 641 nanos (256 milligas) : 1 decimal with 9 digits and a different exponent
    -- Each digit adds 7 nanos, or 2.8 milligas.
    , bench "pact-decimal-3" $ nfIO $ encode decimal3

    -- 145 micros  (58000 milligas) : 1000 decimals with 1 digit
    -- Scaling up to 1000 Decimals (each with 1 digit), serialization
    -- takes 145 nanos per Decimal, or 58 milligas. We use this as the
    -- per-Decimal offset gas cost.
    , bench "pact-decimal-4" $ nfIO $ encode decimal4

    -- 294 micros (117600 milligas) : 2000 decimals with 1 digit
    , bench "pact-decimal-5" $ nfIO $ encode decimal5

    -- 537 nanos (214 milligas) : 1 boolean
    , bench "pact-bool-1" $ nfIO $ encode bool1

    -- 131 micros (52400 milligas) : 1000 booleans.
    -- Serializing each boolean costs 131 nanos (52 milligas).
    , bench "pact-bool-2" $ nfIO $ encode bool2

    -- 541 nanos (216 milligas) : 1 unit
    , bench "pact-unit-1" $ nfIO $ encode unit1

    -- 128 micros (51200 milligas) : 1000 units.
    -- Serializing each unit costs 128 nanos (51 milligas).
    , bench "pact-unit-2" $ nfIO $ encode unit2

    -- 460 nanos (184 milligas) : 1 time
    , bench "pact-time-1" $ nfIO $ encode time1

    -- 460 nanos (184 milligas) : 1 time much further in the future.
    -- No matter the time, serializing a time costs 184 milligas.
    , bench "pact-time-2" $ nfIO $ encode time2

    -- 1 micro (400 milligas) : 1 keyset guard with 1 key
    , bench "pact-guard-keyset-1" $ nfIO $ encode rdKeySet1

    -- 4.2 micros (1680 milligas) : 1 keyset guard with 10 keys
    , bench "pact-guard-keyset-2" $ nfIO $ encode rdKeySet2

    -- 3.2 micros (1280 milligas) : 1 keyset guard with 1 key and a long custom predicate
    , bench "pact-guard-keyset-3" $ nfIO $ encode rdKeySet3

    -- 5.8 micros (2320 milligas) : 1 keyset guard with 10 long keys
    , bench "pact-guard-keyset-4" $ nfIO $ encode rdKeySet4

    , bench "pact-lazyness-test" $ nfIO $ encode rdSlow
    ]
  where
    !longString = Text.replicate 100_000 "a"
    !longBoolList = List.replicate 100_000 True
    !longIntList = List.replicate 100_000 (1 :: Integer)
    !longNestedBoolList = List.replicate 100 (List.replicate 1000 True)
    !longNestedBoolList2 = List.replicate 10_000 (List.replicate 10 True)

-- | For a given target length for all the keys, and a given number of elements,
--   Create a RowData with desired size, with  keys of length that add up to
--   the target.
rowData :: Int -> Int -> RowData
rowData nChars nElems =
  RowData (M.fromList $ map (\i -> (fieldName i, PLiteral (LInteger (fromIntegral i)))) [0..nElems] )
  where
    fieldNameLength = nChars `div` nElems
    fieldName i = padName $ Field $ Text.pack (show i)
    padName (Field f) = Field $ Text.replicate (fieldNameLength - Text.length f) "0" <> f

rowInteger :: Int -> Int -> RowData
rowInteger nElems intValue =
  RowData (M.fromList $ map (\i -> (fieldName i, PLiteral (LInteger (fromIntegral intValue)))) [0..nElems] )
  where
    fieldName i = padName $ Field $ Text.pack (show i)
    padName (Field f) = Field $ Text.replicate (20 - Text.length f) "0" <> f

rowString :: Int -> Int -> RowData
rowString nElems elemLength =
  RowData (M.fromList $ map (\i -> (fieldName i, PString (Text.replicate elemLength "."))) [0..nElems] )
  where
    fieldName i = padName $ Field $ Text.pack (show i)
    padName (Field f) = Field $ Text.replicate (20 - Text.length f) "0" <> f

rowDecimal :: Int -> Decimal -> RowData
rowDecimal nElems decimalValue =
  RowData (M.fromList $ map (\i -> (fieldName i, PDecimal decimalValue)) [0..nElems] )
  where
    fieldName i = padName $ Field $ Text.pack (show i)
    padName (Field f) = Field $ Text.replicate (20 - Text.length f) "0" <> f

rowBool :: Int -> RowData
rowBool nElems =
  RowData (M.fromList $ map (\i -> (fieldName i, PBool False)) [0..nElems] )
  where
    fieldName i = padName $ Field $ Text.pack (show i)
    padName (Field f) = Field $ Text.replicate (20 - Text.length f) "0" <> f

rowUnit :: Int -> RowData
rowUnit nElems =
  RowData (M.fromList $ map (\i -> (fieldName i, PUnit)) [0..nElems] )
  where
    fieldName i = padName $ Field $ Text.pack (show i)
    padName (Field f) = Field $ Text.replicate (4 - Text.length f) "0" <> f

rowTime :: Int64 -> RowData
rowTime extraMicros =
  RowData (M.fromList [(Field "0", PTime (PactTime.fromPosixTimestampMicros extraMicros))] )

-- | Make a fake keyset with nKeys keys, each key of length keyLength, with the given
-- predicate. Each key will be the string representation of its index, padded to the
-- desired length.
fakeKeySet :: Int -> Int -> KSPredicate -> KeySet
fakeKeySet nKeys keyLength pred' =
  let key i = PublicKeyText $ Text.replicate (keyLength - length (show i)) "-" <>  Text.pack (show i)
  in KeySet (S.fromList $ map key [0..nKeys]) pred'

getModule :: String -> EvalM 'ExecRuntime ReplCoreBuiltin SpanInfo (Module Name Type ReplCoreBuiltin SpanInfo)
getModule code = do
  let moduleSyntax = parseMod code
  desugarOutput <- runDesugarModule @ReplCoreBuiltin @SpanInfo moduleSyntax
  pure $ _dsOut desugarOutput

parseMod :: String -> Lisp.Module SpanInfo
parseMod code =
    let parseResult = Lisp.lexer (Text.pack code) >>= Lisp.parseModule
    in either (error . show) id parseResult

exampleModule1 :: String
exampleModule1 = "\
 \(module m G \
 \  (defcap G () true) \
 \  (defun foo()       \
 \   \"return 10\"     \
 \   10)               \
 \)"


getSize :: (Default i, SizeOf a, IsBuiltin b, Show i) => EvalEnv b i -> EvalState b i -> SizeOfVersion -> a -> IO Word64
getSize env state version value = do
  (Right v, _state) <- runEvalM (ExecEnv env) state $ sizeOf def version value
  return v

expectEval :: Show i => EvalEnv b i -> EvalState b i -> EvalM 'ExecRuntime b i a -> IO a
expectEval env state action = do
    (result, _state) <- runEvalM (ExecEnv env) state action
    case result of
        Left e -> error $ "Unexpected failure: " ++ show e
        Right a -> pure a
