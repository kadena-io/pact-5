{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad.State
import Criterion.Main hiding (env)
import Data.Default (Default, def)
import Data.Decimal
import Data.List qualified as List
import Data.Either (fromRight)
import Data.Text qualified as Text
import Data.Int (Int64)
import Data.Word (Word64)
import System.FilePath
import qualified Data.Map as M

import Pact.Core.Builtin
import Pact.Core.Environment.Types
import Pact.Core.Environment.Utils ((.==), useEvalState, usesEvalState)
import Pact.Core.IR.Eval.Runtime.Types
import Pact.Core.Persistence.MockPersistence
import Pact.Core.Names (Name)
import Pact.Core.Serialise
import Pact.Core.Info
import Pact.Core.Persistence.Types
import Pact.Core.Type
import Pact.Core.IR.Desugar
import Pact.Core.IR.Term
import Pact.Core.Gas
import Pact.Core.Names
import Pact.Core.Literal
import Pact.Core.PactValue
import Pact.Core.Serialise.CBOR_V1
import Pact.Core.Syntax.LexUtils qualified as Lisp
import Pact.Core.Syntax.ParseTree qualified as Lisp
import Pact.Core.Syntax.Parser
import Pact.Core.SizeOf
import qualified Pact.Time as PactTime
import Pact.Core.Syntax.Parser qualified as Lisp
import qualified Pact.Core.Syntax.Lexer as Lisp
import Pact.Core.Errors (PactErrorI)
import Control.DeepSeq
import qualified Data.Type.Bool as Bool

getSize :: (Default i, SizeOf a, IsBuiltin b, Show i) => EvalEnv b i -> EvalState b i -> SizeOfVersion -> a -> IO Word64
getSize env state version value = do
  (Right v, _state) <- runEvalM env state $ sizeOf version value
  return v

expectEval :: Show i => EvalEnv b i -> EvalState b i -> EvalM b i a -> IO a
expectEval env state action = do
    (result, _state) <- runEvalM env state action
    case result of
        Left e -> error $ "Unexpected failure: " ++ show e
        Right a -> pure a


main :: IO ()
main = do

  pdb <- mockPactDb serialisePact_repl_spaninfo
  ee <- defaultEvalEnv pdb replCoreBuiltinMap
  let es = def
  !module1 <- expectEval ee es $ getModule exampleModule1
  coinModule <- expectEval ee es $ getModule coinModuleCode


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

  -- The comments list the time that each action takes, and the
  -- number of calls made to the countBytes function.
  -- In a previous version of the code, these calls were counted in
  -- the EvalState monad. We do not keep that code in the `master` branch
  -- because it is messy, but if you need it, it is the last commit of
  -- the `greg/track-count-bytes` branch.
  --
  -- 1 milligas = 2.5 nanoseconds
  defaultMain
    [
      -- 0.003 ms / 0 calls
      bench "nil" $ nfIO $ expectEval ee es (pure ())

      -- 0.9 ms / 1 call
    , bench "int" $ nfIO $ getSize ee es SizeOfV2 (1 :: Int)

      -- 0.9 ms / 1 call
    , bench "long-string" $ nfIO $ getSize ee es SizeOfV2 longString

      -- 152 ms (60800 milligas) / 100000 calls => 0.6 milligas per call
    , bench "long-bool-list" $ nfIO $ getSize ee es SizeOfV2 longBoolList

      -- 165 ms (66000 gas) / 100000 calls => 0.6 milligas per call
    , bench "long-int-list" $ nfIO $ getSize ee es SizeOfV2 longIntList

      -- 218 ms (87200 milligas) / 100100 calls => 0.9 milligas per call
    , bench "long-nested-bool-list" $ nfIO $ getSize ee es SizeOfV2 longNestedBoolList

      -- 214 ms (85600 milligas) / 101000 calls => 0.9 milligas per call
    , bench "long-nested-bool-list-2" $ nfIO $ getSize ee es SizeOfV2 longNestedBoolList

      -- 0.2 ms (80 miligas) / 1646 bytes / 125 calls => 0.6 milligas per call
    , bench "module-1" $ nfIO $ getSize ee es SizeOfV2 module1

      -- 36 ms (14400 milligas) / 253338 bytes / 18637 calls to countBytes => 0.8 milligas per call
    , bench "module-coin" $ nfIO $ getSize ee es SizeOfV2 coinModule

    --   -- 453 ns (183 milligas)
    -- , bench "row-data 1" $ nfIO $ (ignoreGas () . encodeRowData ()) rd1

    --   -- 600 ns (60 milligas)
    -- , bench "row-data 2" $ nfIO $ (ignoreGas () . encodeRowData ()) rd2

    --   -- 5987 ns (1090 milligas)
    -- , bench "row-data 3" $ nfIO $ (ignoreGas () . encodeRowData ()) rd3

    --   -- 5.9 micros (8500000 milligas)
    -- , bench "row-data 5" $ nfIO $ (ignoreGas () . encodeRowData ()) rd5

    -- 1700 nanos ()/ 10 chars
    , bench "row-data 1b" $ nfIO $ (ignoreGas () . encodeRowData ()) rd1b
    
    -- 1954 nanos () / 1000 characters
    , bench "row-data 2b" $ nfIO $ (ignoreGas () . encodeRowData ()) rd2b

    -- 337 micros () / 1_000_000 characters
    , bench "row-data 3b" $ nfIO $ (ignoreGas () . encodeRowData ()) rd3b

    -- 144 micros (57600 milligas) : 1000 small integers
    , bench "pact-integer-1" $ nfIO $ (ignoreGas () . encodeRowData ()) int1

    -- 190 micros (76000 milligas) : 1000 large integers (1e12).
    -- (this - pact-integer-2) / 1000 = 46 nanos per integer, when we move
    -- from 1 digit to 12 digits. Each digit adds 4 nanos, or 2 milligas.
    , bench "pact-integer-2" $ nfIO $ (ignoreGas () . encodeRowData ()) int2

    -- 381 micros (152400 milligas) : 2000 large integers (1e12).
    -- This bench differs from pact-integer-2 by having 2000 integers instead of 1000.
    -- It takes about twice as long as pact-integer-2, validating the linear
    -- effect of integer count on encoding time.
    , bench "pact-integer-3" $ nfIO $ (ignoreGas () . encodeRowData ()) int3

    -- 154 micros (61600 milligas) : 1000 strings of length 1
    , bench "pact-string-1" $ nfIO $ (ignoreGas () . encodeRowData ()) str1

    -- 327 micros (130800 milligas) : 1000 strings of length 1000.
    -- This bench differs from pact-string-1 by using strings of length 1000
    -- instead of strings of length 1.
    -- It takes an extra 173 micros, 173 nanos (69 milligas) per element.
    -- So the gas cost of a string is 69 milligas per 1000 characters.
    , bench "pact-string-2" $ nfIO $ (ignoreGas () . encodeRowData ()) str2
    
    -- 652 micros. This bench differs from pact-string-2 by having twice
    -- as many elements, and it takes twice as long, as expected.
    , bench "pact-string-3" $ nfIO $ (ignoreGas () . encodeRowData ()) str3

    -- 599 nanos (240 milligas) : 1 decimal with 1 digit
    , bench "pact-decimal-1" $ nfIO $ (ignoreGas () . encodeRowData ()) decimal1

    -- 670 nanos (268 milligas) : 1 decimal with 9 digits
    , bench "pact-decimal-2" $ nfIO $ (ignoreGas () . encodeRowData ()) decimal2

    -- 641 nanos (256 milligas) : 1 decimal with 9 digits and a different exponent
    -- Each digit adds 7 nanos, or 2.8 milligas.
    , bench "pact-decimal-3" $ nfIO $ (ignoreGas () . encodeRowData ()) decimal3

    -- 145 micros  (58000 milligas) : 1000 decimals with 1 digit
    -- Scaling up to 1000 Decimals (each with 1 digit), serialization
    -- takes 145 nanos per Decimal, or 58 milligas. We use this as the
    -- per-Decimal offset gas cost.
    , bench "pact-decimal-4" $ nfIO $ (ignoreGas () . encodeRowData ()) decimal4

    -- 294 micros (117600 milligas) : 2000 decimals with 1 digit
    , bench "pact-decimal-5" $ nfIO $ (ignoreGas () . encodeRowData ()) decimal5

    -- 537 nanos (214 milligas) : 1 boolean
    , bench "pact-bool-1" $ nfIO $ (ignoreGas () . encodeRowData ()) bool1

    -- 131 micros (52400 milligas) : 1000 booleans.
    -- Serializing each boolean costs 131 nanos (52 milligas).
    , bench "pact-bool-2" $ nfIO $ (ignoreGas () . encodeRowData ()) bool2

    -- 541 nanos (216 milligas) : 1 unit
    , bench "pact-unit-1" $ nfIO $ (ignoreGas () . encodeRowData ()) unit1

    -- 128 micros (51200 milligas) : 1000 units.
    -- Serializing each unit costs 128 nanos (51 milligas).
    , bench "pact-unit-2" $ nfIO $ (ignoreGas () . encodeRowData ()) unit2

    -- 460 nanos (184 milligas) : 1 time
    , bench "pact-time-1" $ nfIO $ (ignoreGas () . encodeRowData ()) time1

    -- 460 nanos (184 milligas) : 1 time much further in the future.
    -- No matter the time, serializing a time costs 184 milligas.
    , bench "pact-time-2" $ nfIO $ (ignoreGas () . encodeRowData ()) time2
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
    fieldNameLength = 10
    fieldName i = padName $ Field $ Text.pack (show i)
    padName (Field f) = Field $ Text.replicate (20 - Text.length f) "0" <> f

rowString :: Int -> Int -> RowData
rowString nElems elemLength =
  RowData (M.fromList $ map (\i -> (fieldName i, PString (Text.replicate elemLength "."))) [0..nElems] )
  where
    fieldNameLength = 10
    fieldName i = padName $ Field $ Text.pack (show i)
    padName (Field f) = Field $ Text.replicate (20 - Text.length f) "0" <> f

rowDecimal :: Int -> Decimal -> RowData
rowDecimal nElems decimalValue =
  RowData (M.fromList $ map (\i -> (fieldName i, PDecimal decimalValue)) [0..nElems] )
  where
    fieldNameLength = 10
    fieldName i = padName $ Field $ Text.pack (show i)
    padName (Field f) = Field $ Text.replicate (20 - Text.length f) "0" <> f

rowBool :: Int -> RowData
rowBool nElems =
  RowData (M.fromList $ map (\i -> (fieldName i, PBool False)) [0..nElems] )
  where
    fieldNameLength = 10
    fieldName i = padName $ Field $ Text.pack (show i)
    padName (Field f) = Field $ Text.replicate (20 - Text.length f) "0" <> f

rowUnit :: Int -> RowData
rowUnit nElems =
  RowData (M.fromList $ map (\i -> (fieldName i, PUnit)) [0..nElems] )
  where
    fieldNameLength = 10
    fieldName i = padName $ Field $ Text.pack (show i)
    padName (Field f) = Field $ Text.replicate (4 - Text.length f) "0" <> f

rowTime :: Int64 -> RowData
rowTime extraMicros =
  RowData (M.fromList [(Field "0", PTime (PactTime.fromPosixTimestampMicros extraMicros))] )


getModule :: String -> EvalM ReplCoreBuiltin SpanInfo (Module Name Type ReplCoreBuiltin SpanInfo)
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



coinModuleCode :: String
coinModuleCode = unlines [
  "(module coin GOVERNANCE"
 ,""
 ,"  @doc \"docs\""
 ,""
 ,"  @model"
 ,"    [ (defproperty conserves-mass"
 ,"        (= (column-delta coin-table 'balance) 0.0))"
 ,""
 ,"      (defproperty valid-account (account:string)"
 ,"        (and"
 ,"          (>= (length account) 3)"
 ,"          (<= (length account) 256)))"
 ,"    ]"
 ,""
 ,"  ;(implements fungible-v2)"
 ,"  ;(implements fungible-xchain-v1)"
 ,""
 ,"  ;; coin-v2"
 ,"  (bless \"ut_J_ZNkoyaPUEJhiwVeWnkSQn9JT9sQCWKdjjVVrWo\")"
 ,""
 ,"  ;; coin v3"
 ,"  (bless \"1os_sLAUYvBzspn5jjawtRpJWiH1WPfhyNraeVvSIwU\")"
 ,""
 ,"  ;; coin v4"
 ,"  (bless \"BjZW0T2ac6qE_I5X8GE4fal6tTqjhLTC7my0ytQSxLU\")"
 ,""
 ,"  ; --------------------------------------------------------------------------"
 ,"  ; Schemas and Tables"
 ,""
 ,"  (defschema coin-schema"
 ,"    @doc \"The coin contract token schema\""
 ,"    @model [ (invariant (>= balance 0.0)) ]"
 ,""
 ,"    balance:decimal"
 ,"    guard:guard)"
 ,""
 ,"  (deftable coin-table:{coin-schema})"
 ,""
 ,"  ; --------------------------------------------------------------------------"
 ,"  ; Capabilities"
 ,""
 ,"  (defcap GOVERNANCE ()"
 ,"    (enforce false \"Enforce non-upgradeability\"))"
 ,""
 ,"  (defcap GAS ()"
 ,"    \"Magic capability to protect gas buy and redeem\""
 ,"    true)"
 ,""
 ,"  (defcap COINBASE ()"
 ,"    \"Magic capability to protect miner reward\""
 ,"    true)"
 ,""
 ,"  (defcap GENESIS ()"
 ,"    \"Magic capability constraining genesis transactions\""
 ,"    true)"
 ,""
 ,"  (defcap REMEDIATE ()"
 ,"    \"Magic capability for remediation transactions\""
 ,"    true)"
 ,""
 ,"  (defcap DEBIT (sender:string)"
 ,"    \"Capability for managing debiting operations\""
 ,"    (enforce-guard (at 'guard (read coin-table sender)))"
 ,"    (enforce (!= sender \"\") \"valid sender\"))"
 ,""
 ,"  (defcap CREDIT (receiver:string)"
 ,"    \"Capability for managing crediting operations\""
 ,"    (enforce (!= receiver \"\") \"valid receiver\"))"
 ,""
 ,"  (defcap ROTATE (account:string)"
 ,"    @doc \"Autonomously managed capability for guard rotation\""
 ,"    @managed"
 ,"    true)"
 ,""
 ,"  (defcap TRANSFER:bool"
 ,"    ( sender:string"
 ,"      receiver:string"
 ,"      amount:decimal"
 ,"    )"
 ,"    @managed amount TRANSFER-mgr"
 ,"    (enforce (!= sender receiver) \"same sender and receiver\")"
 ,"    (enforce-unit amount)"
 ,"    (enforce (> amount 0.0) \"Positive amount\")"
 ,"    (compose-capability (DEBIT sender))"
 ,"    (compose-capability (CREDIT receiver))"
 ,"  )"
 ,""
 ,"  (defun TRANSFER-mgr:decimal"
 ,"    ( managed:decimal"
 ,"      requested:decimal"
 ,"    )"
 ,""
 ,"    (let ((newbal (- managed requested)))"
 ,"      (enforce (>= newbal 0.0)"
 ,"        (format \"TRANSFER exceeded for balance {}\" [managed]))"
 ,"      newbal)"
 ,"  )"
 ,""
 ,"  (defcap TRANSFER_XCHAIN:bool"
 ,"    ( sender:string"
 ,"      receiver:string"
 ,"      amount:decimal"
 ,"      target-chain:string"
 ,"    )"
 ,""
 ,"    @managed amount TRANSFER_XCHAIN-mgr"
 ,"    (enforce-unit amount)"
 ,"    (enforce (> amount 0.0) \"Cross-chain transfers require a positive amount\")"
 ,"    (compose-capability (DEBIT sender))"
 ,"  )"
 ,""
 ,"  (defun TRANSFER_XCHAIN-mgr:decimal"
 ,"    ( managed:decimal"
 ,"      requested:decimal"
 ,"    )"
 ,""
 ,"    (enforce (>= managed requested)"
 ,"      (format \"TRANSFER_XCHAIN exceeded for balance {}\" [managed]))"
 ,"    0.0"
 ,"  )"
 ,""
 ,"  (defcap TRANSFER_XCHAIN_RECD:bool"
 ,"    ( sender:string"
 ,"      receiver:string"
 ,"      amount:decimal"
 ,"      source-chain:string"
 ,"    )"
 ,"    @event true"
 ,"  )"
 ,""
 ,"  ; v3 capabilities"
 ,"  (defcap RELEASE_ALLOCATION"
 ,"    ( account:string"
 ,"      amount:decimal"
 ,"    )"
 ,"    @doc \"Event for allocation release, can be used for sig scoping.\""
 ,"    @event true"
 ,"  )"
 ,""
 ,"  ; --------------------------------------------------------------------------"
 ,"  ; Constants"
 ,""
 ,"  (defconst COIN_CHARSET CHARSET_LATIN1"
 ,"    \"The default coin contract character set\")"
 ,""
 ,"  (defconst MINIMUM_PRECISION 12"
 ,"    \"Minimum allowed precision for coin transactions\")"
 ,""
 ,"  (defconst MINIMUM_ACCOUNT_LENGTH 3"
 ,"    \"Minimum account length admissible for coin accounts\")"
 ,""
 ,"  (defconst MAXIMUM_ACCOUNT_LENGTH 256"
 ,"    \"Maximum account name length admissible for coin accounts\")"
 ,""
 ,"  (defconst VALID_CHAIN_IDS (map (int-to-str 10) (enumerate 0 19))"
 ,"    \"List of all valid Chainweb chain ids\")"
 ,""
 ,"  ; --------------------------------------------------------------------------"
 ,"  ; Utilities"
 ,""
 ,"  (defun enforce-unit:bool (amount:decimal)"
 ,"    @doc \"Enforce minimum precision allowed for coin transactions\""
 ,""
 ,"    (enforce"
 ,"      (= (floor amount MINIMUM_PRECISION)"
 ,"         amount)"
 ,"      (format \"Amount violates minimum precision: {}\" [amount]))"
 ,"    )"
 ,""
 ,"  (defun validate-account (account:string)"
 ,"    @doc \"Docs\""
 ,""
 ,"    (enforce"
 ,"      (is-charset COIN_CHARSET account)"
 ,"      (format"
 ,"        \"Account does not conform to the coin contract charset: {}\""
 ,"        [account]))"
 ,""
 ,"    (let ((account-length (length account)))"
 ,""
 ,"      (enforce"
 ,"        (>= account-length MINIMUM_ACCOUNT_LENGTH)"
 ,"        (format"
 ,"          \"Account name does not conform to the min length requirement: {}\""
 ,"          [account]))"
 ,""
 ,"      (enforce"
 ,"        (<= account-length MAXIMUM_ACCOUNT_LENGTH)"
 ,"        (format"
 ,"          \"Account name does not conform to the max length requirement: {}\""
 ,"          [account]))"
 ,"      )"
 ,"  )"
 ,""
 ,"  ; --------------------------------------------------------------------------"
 ,"  ; Coin Contract"
 ,""
 ,"  (defun gas-only ()"
 ,"    \"Predicate for gas-only user guards.\""
 ,"    (require-capability (GAS)))"
 ,""
 ,"  (defun gas-guard (guard:guard)"
 ,"    \"Predicate for gas + single key user guards\""
 ,"    (enforce-one"
 ,"      \"Enforce either the presence of a GAS cap or keyset\""
 ,"      [ (gas-only)"
 ,"        (enforce-guard guard)"
 ,"      ]))"
 ,""
 ,"  (defun buy-gas:string (sender:string total:decimal)"
 ,"    @doc \"Docs\""
 ,""
 ,"    @model [ (property (> total 0.0))"
 ,"             (property (valid-account sender))"
 ,"           ]"
 ,""
 ,"    (validate-account sender)"
 ,""
 ,"    (enforce-unit total)"
 ,"    (enforce (> total 0.0) \"gas supply must be a positive quantity\")"
 ,""
 ,"    (require-capability (GAS))"
 ,"    (with-capability (DEBIT sender)"
 ,"      (debit sender total))"
 ,"    )"
 ,""
 ,"  (defun redeem-gas:string (miner:string miner-guard:guard sender:string total:decimal)"
 ,"    @doc \"Docs\""
 ,""
 ,"    @model [ (property (> total 0.0))"
 ,"             (property (valid-account sender))"
 ,"             (property (valid-account miner))"
 ,"           ]"
 ,""
 ,"    (validate-account sender)"
 ,"    (validate-account miner)"
 ,"    (enforce-unit total)"
 ,""
 ,"    (require-capability (GAS))"
 ,"    (let*"
 ,"      ((fee (read-decimal \"fee\"))"
 ,"       (refund (- total fee)))"
 ,""
 ,"      (enforce-unit fee)"
 ,"      (enforce (>= fee 0.0)"
 ,"        \"fee must be a non-negative quantity\")"
 ,""
 ,"      (enforce (>= refund 0.0)"
 ,"        \"refund must be a non-negative quantity\")"
 ,""
 ,"      (emit-event (TRANSFER sender miner fee)) ;v3"
 ,""
 ,"        ; directly update instead of credit"
 ,"      (with-capability (CREDIT sender)"
 ,"        (if (> refund 0.0)"
 ,"          (with-read coin-table sender"
 ,"            { \"balance\" := balance }"
 ,"            (update coin-table sender"
 ,"              { \"balance\": (+ balance refund) }))"
 ,""
 ,"          \"noop\"))"
 ,""
 ,"      (with-capability (CREDIT miner)"
 ,"        (if (> fee 0.0)"
 ,"          (credit miner miner-guard fee)"
 ,"          \"noop\"))"
 ,"      )"
 ,""
 ,"    )"
 ,""
 ,"  (defun create-account:string (account:string guard:guard)"
 ,"    @model [ (property (valid-account account)) ]"
 ,""
 ,"    (validate-account account)"
 ,"    (enforce-reserved account guard)"
 ,""
 ,"    (insert coin-table account"
 ,"      { \"balance\" : 0.0"
 ,"      , \"guard\"   : guard"
 ,"      })"
 ,"    )"
 ,""
 ,"  (defun get-balance:decimal (account:string)"
 ,"    (with-read coin-table account"
 ,"      { \"balance\" := balance }"
 ,"      balance"
 ,"      )"
 ,"    )"
 ,""
 ," ; (defun details:object{fungible-v2.account-details}"
 ," ;   ( account:string )"
 ," ;   (with-read coin-table account"
 ," ;     { \"balance\" := bal"
 ," ;     , \"guard\" := g }"
 ," ;     { \"account\" : account"
 ," ;     , \"balance\" : bal"
 ," ;     , \"guard\": g })"
 ," ;   )"
 ,""
 ,"  (defun rotate:string (account:string new-guard:guard)"
 ,"    (with-capability (ROTATE account)"
 ,"      (with-read coin-table account"
 ,"        { \"guard\" := old-guard }"
 ,""
 ,"        (enforce-guard old-guard)"
 ,""
 ,"        (update coin-table account"
 ,"          { \"guard\" : new-guard }"
 ,"          )))"
 ,"    )"
 ,""
 ,""
 ,"  (defun precision:integer"
 ,"    ()"
 ,"    MINIMUM_PRECISION)"
 ,""
 ,"  (defun transfer:string (sender:string receiver:string amount:decimal)"
 ,"    @model [ (property conserves-mass)"
 ,"             (property (> amount 0.0))"
 ,"             (property (valid-account sender))"
 ,"             (property (valid-account receiver))"
 ,"             (property (!= sender receiver)) ]"
 ,""
 ,"    (enforce (!= sender receiver)"
 ,"      \"sender cannot be the receiver of a transfer\")"
 ,""
 ,"    (validate-account sender)"
 ,"    (validate-account receiver)"
 ,""
 ,"    (enforce (> amount 0.0)"
 ,"      \"transfer amount must be positive\")"
 ,""
 ,"    (enforce-unit amount)"
 ,""
 ,"    (with-capability (TRANSFER sender receiver amount)"
 ,"      (debit sender amount)"
 ,"      (with-read coin-table receiver"
 ,"        { \"guard\" := g }"
 ,""
 ,"        (credit receiver g amount))"
 ,"      )"
 ,"    )"
 ,""
 ,"  (defun transfer-create:string"
 ,"    ( sender:string"
 ,"      receiver:string"
 ,"      receiver-guard:guard"
 ,"      amount:decimal )"
 ,""
 ,"    @model [ (property conserves-mass) ]"
 ,""
 ,"    (enforce (!= sender receiver)"
 ,"      \"sender cannot be the receiver of a transfer\")"
 ,""
 ,"    (validate-account sender)"
 ,"    (validate-account receiver)"
 ,""
 ,"    (enforce (> amount 0.0)"
 ,"      \"transfer amount must be positive\")"
 ,""
 ,"    (enforce-unit amount)"
 ,""
 ,"    (with-capability (TRANSFER sender receiver amount)"
 ,"      (debit sender amount)"
 ,"      (credit receiver receiver-guard amount))"
 ,"    )"
 ,""
 ,"  (defun coinbase:string (account:string account-guard:guard amount:decimal)"
 ,"    @doc \"Docs\""
 ,""
 ,"    @model [ (property (valid-account account))"
 ,"             (property (> amount 0.0))"
 ,"           ]"
 ,""
 ,"    (validate-account account)"
 ,"    (enforce-unit amount)"
 ,""
 ,"    (require-capability (COINBASE))"
 ,"    (emit-event (TRANSFER \"\" account amount)) ;v3"
 ,"    (with-capability (CREDIT account)"
 ,"      (credit account account-guard amount))"
 ,"    )"
 ,""
 ,"  (defun remediate:string (account:string amount:decimal)"
 ,"    @doc \"Docs\""
 ,"    @model [ (property (valid-account account))"
 ,"             (property (> amount 0.0))"
 ,"           ]"
 ,""
 ,"    (validate-account account)"
 ,""
 ,"    (enforce (> amount 0.0)"
 ,"      \"Remediation amount must be positive\")"
 ,""
 ,"    (enforce-unit amount)"
 ,""
 ,"    (require-capability (REMEDIATE))"
 ,"    (emit-event (TRANSFER \"\" account amount)) ;v3"
 ,"    (with-read coin-table account"
 ,"      { \"balance\" := balance }"
 ,""
 ,"      (enforce (<= amount balance) \"Insufficient funds\")"
 ,""
 ,"      (update coin-table account"
 ,"        { \"balance\" : (- balance amount) }"
 ,"        ))"
 ,"    )"
 ,""
 ,"  (defpact fund-tx (sender:string miner:string miner-guard:guard total:decimal)"
 ,"    @doc \"Docs\""
 ,""
 ,"    @model [ (property (> total 0.0))"
 ,"             (property (valid-account sender))"
 ,"             (property (valid-account miner))"
 ,"             ;(property conserves-mass) not supported yet"
 ,"           ]"
 ,""
 ,"    (step (buy-gas sender total))"
 ,"    (step (redeem-gas miner miner-guard sender total))"
 ,"    )"
 ,""
 ,"  (defun debit:string (account:string amount:decimal)"
 ,"    @doc \"Debit AMOUNT from ACCOUNT balance\""
 ,""
 ,"    @model [ (property (> amount 0.0))"
 ,"             (property (valid-account account))"
 ,"           ]"
 ,""
 ,"    (validate-account account)"
 ,""
 ,"    (enforce (> amount 0.0)"
 ,"      \"debit amount must be positive\")"
 ,""
 ,"    (enforce-unit amount)"
 ,""
 ,"    (require-capability (DEBIT account))"
 ,"    (with-read coin-table account"
 ,"      { \"balance\" := balance }"
 ,""
 ,"      (enforce (<= amount balance) \"Insufficient funds\")"
 ,""
 ,"      (update coin-table account"
 ,"        { \"balance\" : (- balance amount) }"
 ,"        ))"
 ,"    )"
 ,""
 ,""
 ,"  (defun credit:string (account:string guard:guard amount:decimal)"
 ,"    @doc \"Credit AMOUNT to ACCOUNT balance\""
 ,""
 ,"    @model [ (property (> amount 0.0))"
 ,"             (property (valid-account account))"
 ,"           ]"
 ,""
 ,"    (validate-account account)"
 ,""
 ,"    (enforce (> amount 0.0) \"credit amount must be positive\")"
 ,"    (enforce-unit amount)"
 ,""
 ,"    (require-capability (CREDIT account))"
 ,"    (with-default-read coin-table account"
 ,"      { \"balance\" : -1.0, \"guard\" : guard }"
 ,"      { \"balance\" := balance, \"guard\" := retg }"
 ,"      ; we don't want to overwrite an existing guard with the user-supplied one"
 ,"      (enforce (= retg guard)"
 ,"        \"account guards do not match\")"
 ,""
 ,"      (let ((is-new"
 ,"             (if (= balance -1.0)"
 ,"                 (enforce-reserved account guard)"
 ,"               false)))"
 ,""
 ,"        (write coin-table account"
 ,"          { \"balance\" : (if is-new amount (+ balance amount))"
 ,"          , \"guard\"   : retg"
 ,"          }))"
 ,"      ))"
 ,""
 ,"  (defun check-reserved:string (account:string)"
 ,"    \"Docs\""
 ,"    (let ((pfx (take 2 account)))"
 ,"      (if (= \":\" (take -1 pfx)) (take 1 pfx) \"\")))"
 ,""
 ,"  (defun enforce-reserved:bool (account:string guard:guard)"
 ,"    @doc \"Enforce reserved account name protocols.\""
 ,"    (if (validate-principal guard account)"
 ,"      true"
 ,"      (let ((r (check-reserved account)))"
 ,"        (if (= r \"\")"
 ,"          true"
 ,"          (if (= r \"k\")"
 ,"            (enforce false \"Single-key account protocol violation\")"
 ,"            (enforce false"
 ,"              (format \"Reserved protocol guard violation: {}\" [r]))"
 ,"            )))))"
 ,""
 ,""
 ,"  (defschema crosschain-schema"
 ,"    @doc \"Schema for yielded value in cross-chain transfers\""
 ,"    receiver:string"
 ,"    receiver-guard:guard"
 ,"    amount:decimal"
 ,"    source-chain:string)"
 ,""
 ,"  (defpact transfer-crosschain:string"
 ,"    ( sender:string"
 ,"      receiver:string"
 ,"      receiver-guard:guard"
 ,"      target-chain:string"
 ,"      amount:decimal )"
 ,""
 ,"    @model [ (property (> amount 0.0))"
 ,"             (property (valid-account sender))"
 ,"             (property (valid-account receiver))"
 ,"           ]"
 ,""
 ,"    (step"
 ,"      (with-capability"
 ,"        (TRANSFER_XCHAIN sender receiver amount target-chain)"
 ,""
 ,"        (validate-account sender)"
 ,"        (validate-account receiver)"
 ,""
 ,"        (enforce (!= \"\" target-chain) \"empty target-chain\")"
 ,"        (enforce (!= (at 'chain-id (chain-data)) target-chain)"
 ,"          \"cannot run cross-chain transfers to the same chain\")"
 ,""
 ,"        (enforce (> amount 0.0)"
 ,"          \"transfer quantity must be positive\")"
 ,""
 ,"        (enforce-unit amount)"
 ,""
 ,"        (enforce (contains target-chain VALID_CHAIN_IDS)"
 ,"          \"target chain is not a valid chainweb chain id\")"
 ,""
 ,"        ;; step 1 - debit delete-account on current chain"
 ,"        (debit sender amount)"
 ,"        (emit-event (TRANSFER sender \"\" amount))"
 ,""
 ,"        (let"
 ,"          ((crosschain-details:object{crosschain-schema}"
 ,"            { \"receiver\" : receiver"
 ,"            , \"receiver-guard\" : receiver-guard"
 ,"            , \"amount\" : amount"
 ,"            , \"source-chain\" : (at 'chain-id (chain-data))"
 ,"            }))"
 ,"          (yield crosschain-details target-chain)"
 ,"          )))"
 ,""
 ,"    (step"
 ,"      (resume"
 ,"        { \"receiver\" := receiver"
 ,"        , \"receiver-guard\" := receiver-guard"
 ,"        , \"amount\" := amount"
 ,"        , \"source-chain\" := source-chain"
 ,"        }"
 ,""
 ,"        (emit-event (TRANSFER \"\" receiver amount))"
 ,"        (emit-event (TRANSFER_XCHAIN_RECD \"\" receiver amount source-chain))"
 ,""
 ,"        ;; step 2 - credit create account on target chain"
 ,"        (with-capability (CREDIT receiver)"
 ,"          (credit receiver receiver-guard amount))"
 ,"        ))"
 ,"    )"
 ,""
 ,""
 ,"  ; --------------------------------------------------------------------------"
 ,"  ; Coin allocations"
 ,""
 ,"  (defschema allocation-schema"
 ,"    @doc \"Genesis allocation registry\""
 ,"    ;@model [ (invariant (>= balance 0.0)) ]"
 ,""
 ,"    balance:decimal"
 ,"    date:time"
 ,"    guard:guard"
 ,"    redeemed:bool)"
 ,""
 ,"  (deftable allocation-table:{allocation-schema})"
 ,""
 ,"  (defun create-allocation-account"
 ,"    ( account:string"
 ,"      date:time"
 ,"      keyset-ref:string"
 ,"      amount:decimal"
 ,"    )"
 ,""
 ,"    @doc \"Docs\""
 ,""
 ,"    @model [ (property (valid-account account)) ]"
 ,""
 ,"    (require-capability (GENESIS))"
 ,""
 ,"    (validate-account account)"
 ,"    (enforce (>= amount 0.0)"
 ,"      \"allocation amount must be non-negative\")"
 ,""
 ,"    (enforce-unit amount)"
 ,""
 ,"    (let"
 ,"      ((guard:guard (keyset-ref-guard keyset-ref)))"
 ,""
 ,"      (create-account account guard)"
 ,""
 ,"      (insert allocation-table account"
 ,"        { \"balance\" : amount"
 ,"        , \"date\" : date"
 ,"        , \"guard\" : guard"
 ,"        , \"redeemed\" : false"
 ,"        })))"
 ,""
 ,"  (defun release-allocation"
 ,"    ( account:string )"
 ,""
 ,"    @doc \"Docs\""
 ,"    @model [ (property (valid-account account)) ]"
 ,""
 ,"    (validate-account account)"
 ,""
 ,"    (with-read allocation-table account"
 ,"      { \"balance\" := balance"
 ,"      , \"date\" := release-time"
 ,"      , \"redeemed\" := redeemed"
 ,"      , \"guard\" := guard"
 ,"      }"
 ,""
 ,"      (let ((curr-time:time (at 'block-time (chain-data))))"
 ,""
 ,"        (enforce (not redeemed)"
 ,"          \"allocation funds have already been redeemed\")"
 ,""
 ,"        (enforce"
 ,"          (>= curr-time release-time)"
 ,"          (format \"funds locked until {}. current time: {}\" [release-time curr-time]))"
 ,""
 ,"        (with-capability (RELEASE_ALLOCATION account balance)"
 ,""
 ,"        (enforce-guard guard)"
 ,""
 ,"        (with-capability (CREDIT account)"
 ,"          (emit-event (TRANSFER \"\" account balance))"
 ,"          (credit account guard balance)"
 ,""
 ,"          (update allocation-table account"
 ,"            { \"redeemed\" : true"
 ,"            , \"balance\" : 0.0"
 ,"            })"
 ,""
 ,"          \"Allocation successfully released to main ledger\"))"
 ,"    )))"
 ,""
 ,")"

  ]
