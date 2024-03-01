{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Pact.Core.GasModel.BuiltinsGas where

import Control.Lens
-- import Control.Monad
import Control.Monad.IO.Class
import Data.Default
-- import Data.Functor(void)
-- import Data.Bifunctor(bimap)
-- import Criterion.Types(Report)
import qualified Data.RAList as RA
import qualified Data.List.NonEmpty as NE
import qualified Criterion as C
-- import qualified Criterion.Report as C
-- import qualified Criterion.Analysis as C
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Database.SQLite3 as SQL
import NeatInterpolation (text)

import Pact.Core.Builtin
import Pact.Core.Environment
import Pact.Core.Names
import Pact.Core.Gas
import Pact.Core.Literal
import Pact.Core.Type
import Pact.Core.Capabilities
import Pact.Core.IR.Eval.Runtime
import Pact.Core.PactValue
import Pact.Core.IR.Term
import Pact.Core.Persistence
import Pact.Core.Hash
import Pact.Core.Persistence.SQLite
import Pact.Core.Serialise (serialisePact)
import qualified Pact.Core.IR.Eval.CEK as Eval

import Pact.Core.GasModel.Utils

showText :: Show a => a -> T.Text
showText = T.pack . show

benchEnumerate :: PactDb CoreBuiltin () -> C.Benchmark
benchEnumerate pdb = C.bgroup "enumerate" [ bench cnt | cnt <- [1000 :: Int, 10000, 100000] ]
  where
  bench cnt = let cnt' = showText cnt in runNativeBenchmark pdb (show cnt) [text|(enumerate 0 $cnt')|]

benchmarks :: C.Benchmark
benchmarks = C.envWithCleanup mkPactDb cleanupPactDb $ \ ~(pdb, _db) -> do
  C.bgroup "pact-core-builtin-gas" [ benchEnumerate pdb ]
  where
  mkPactDb = do
    tup@(pdb, _) <- unsafeCreateSqlitePactDb serialisePact ":memory:"
    _ <- _pdbBeginTx pdb Transactional
    pure tup

  cleanupPactDb (_, db) = SQL.close db
