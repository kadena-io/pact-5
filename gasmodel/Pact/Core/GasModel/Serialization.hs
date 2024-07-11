{-# LANGUAGE TypeApplications #-}
module Pact.Core.GasModel.Serialization
  (benchmarks) where

import Criterion(Benchmark)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Criterion as C

import Pact.Core.Names
import Pact.Core.PactValue
import Pact.Core.Guards
import Pact.Core.StableEncoding

import Pact.Core.Persistence.Types
import Pact.Core.Persistence.Utils (ignoreGas)
import Pact.Core.Serialise.CBOR_V1 (encodeRowDataNoGas, encodeRowData)

benchmarks :: Benchmark
benchmarks = C.bgroup "Encode/Decode benchmarks"
  [ benchJSONEncodeRowData
  , benchCBOREncodeRowData
  , benchCBOREncodeRowDataGas ]

accountBalancePV :: RowData
accountBalancePV =
  RowData $
  M.fromList
  [ (Field "balance", PDecimal 100)
  , (Field "guard", PGuard (GKeyset (KeySet (S.singleton (PublicKeyText n)) KeysAll)))]
  where
  n = T.replicate 64 "a"

benchJSONEncodeRowData :: Benchmark
benchJSONEncodeRowData =
  C.bench "encode row data json" (C.nf encodeStable accountBalancePV)

benchCBOREncodeRowData :: Benchmark
benchCBOREncodeRowData =
  C.bench "encode row data cbor" (C.nf encodeRowDataNoGas accountBalancePV)

benchCBOREncodeRowDataGas :: Benchmark
benchCBOREncodeRowDataGas =
  C.bench "encode row data cbor - gassed" (C.nfIO (ignoreGas () $ encodeRowData accountBalancePV))
