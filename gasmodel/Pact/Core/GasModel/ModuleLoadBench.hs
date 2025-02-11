{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}


module Pact.Core.GasModel.ModuleLoadBench where

import Criterion as C
import Data.Word
import Data.Default
import NeatInterpolation (text)
import Data.IORef
import Data.Map.Strict(Map)
import Data.Set(Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.ByteString as B

import Pact.Core.Environment
import Pact.Core.Names
import Pact.Core.Capabilities
import Pact.Core.PactValue
import Pact.Core.Guards
import Pact.Core.Evaluate
import Pact.Core.Hash
import Pact.Core.Persistence
import Pact.Core.Builtin
import Pact.Core.SPV
import Pact.Core.Gas.TableGasModel
import Pact.Core.Gas
import Pact.Core.Namespace
import Pact.Core.IR.Term
import Pact.Core.Serialise
import Pact.Core.Persistence.MockPersistence
import Pact.Core.Gen

import Hedgehog
-- Internal API use of hedgehog. Unfortunately needed here, since we need to sample stuff.

import qualified Hedgehog.Internal.Seed as Seed
import qualified Hedgehog.Internal.Tree as Tree
import Hedgehog.Internal.Gen(evalGen)

import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R
import Control.Monad (forM)
import Pact.Core.SizeOf

genModule :: Word64 -> ModuleData CoreBuiltin Info
genModule w =
  sampleWithSeed (Seed.from w)
    (moduleDataOnlyGen builtinGen lineInfoGen)

genModules :: Word64 -> [ModuleData CoreBuiltin Info]
genModules w =
  sampleWithSeed (Seed.from w)
    (G.list (R.constant 100 100) (moduleDataOnlyGen builtinGen lineInfoGen))

sizeOfVsSize :: IO [(Int, Int)]
sizeOfVsSize = do
  let modules = genModules 42020
  pdb <- mockPactDb serialisePact_lineinfo_pact51
  ee <- setupBenchEvalEnv pdb mempty PUnit
  out <- runEvalMResult (ExecEnv ee) def $ forM modules $ \m -> do
    sz <- sizeOfInternal m
    let bs = _encodeModuleData serialisePact_lineinfo_pact51 m
    pure (fromIntegral sz, B.length bs)
  either (error . show) pure out
  where
  sizeOfInternal = \case
    ModuleData m deps ->
      -- sizeOf def SizeOfV0 m
      (+) <$> sizeOf def SizeOfV0 m <*> sizeOf def SizeOfV0 deps
    InterfaceData m deps ->
      -- sizeOf def SizeOfV0 m
      (+) <$> sizeOf def SizeOfV0 m <*> sizeOf def SizeOfV0 deps



mixSeed :: Seed -> Seed
mixSeed (Seed v g) =
  Seed (Seed.mix64 v) (Seed.mixGamma (g + Seed.goldenGamma))

-- Sample a generator with a fixed seed, and mix the seed
-- if we can't generate something
sampleWithSeed :: Seed -> Gen a -> a
sampleWithSeed seed gen =
    let
      loop n s =
        if n <= 0 then
          error "SampleWithSeed: too many discards, could not generate a sample"
        else
          case evalGen 30 s gen of
            Nothing ->
              loop (n - 1) (mixSeed s)
            Just x ->
              Tree.treeValue x
    in loop (100 :: Int) seed

runModuleLoadBench :: PactDb CoreBuiltin Info -> Int -> Benchmark
runModuleLoadBench pdb i =
  envWithCleanup mkModule doRollback $ \ ~(ee, mn, bs) ->
    bench (title bs) $ nfIO $ runEvalMResult (ExecEnv ee) def $ do
      () <$ getModule def mn
  where
  doRollback _ =
    ignoreGas def $ _pdbRollbackTx pdb
  title bs =
    let bs' = T.pack (show bs)
    in T.unpack [text| Benching module of size (in bytes), ${bs'} |]
  bytesize mdata = B.length $ _encodeModuleData serialisePact_lineinfo_pact51 mdata
  mkModule = do
    let mdata = genModule (42020 + fromIntegral i)
    _ <- ignoreGas def $ _pdbBeginTx pdb Transactional
    ee <- setupBenchEvalEnv pdb mempty PUnit
    ignoreGas def $ _pdbWrite pdb Write DModules (moduleDataName mdata) mdata
    pure (ee, moduleDataName mdata, bytesize mdata)

moduleDataName :: ModuleData b i -> ModuleName
moduleDataName = \case
  ModuleData m _ -> _mName m
  InterfaceData iface _ -> _ifName iface

benchmarks :: Benchmark
benchmarks = C.env mkPdb $ \ ~(pdb) ->
    C.bgroup "Module load benches" (runModuleLoadBench pdb <$> [1..1])
  where
  mkPdb = do
    pdb <- mockPactDb serialisePact_lineinfo_pact51
    _ <- ignoreGas def $ _pdbBeginTx pdb Transactional
    _ <- ignoreGas def $ _pdbCommitTx pdb
    pure pdb


setupBenchEvalEnv
  :: PactDb CoreBuiltin i
  -> Map PublicKeyText (Set (CapToken QualifiedName PactValue))
  -> PactValue -> IO (EvalEnv CoreBuiltin i)
setupBenchEvalEnv pdb signers mBody = do
  gasRef <- newIORef mempty
  let
    gasEnv = GasEnv
      { _geGasRef = gasRef
      , _geGasLog = Nothing
      , _geGasModel = tableGasModel (MilliGasLimit (MilliGas 200_000_000))
      }
  pure $ EvalEnv
    { _eeMsgSigs = signers
    , _eeMsgVerifiers = mempty
    , _eePactDb = pdb
    , _eeMsgBody = mBody
    , _eeHash = defaultPactHash
    , _eePublicData = def
    , _eeDefPactStep = Nothing
    , _eeMode = Transactional
    , _eeFlags = S.fromList [FlagEnforceKeyFormats, FlagRequireKeysetNs]
    , _eeNatives = coreBuiltinMap
    , _eeNamespacePolicy = SimpleNamespacePolicy
    , _eeGasEnv = gasEnv
    , _eeSPVSupport = noSPVSupport
    , _eeWarnings = Nothing
    }
