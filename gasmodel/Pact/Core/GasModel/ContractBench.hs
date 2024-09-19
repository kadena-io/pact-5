{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Pact.Core.GasModel.ContractBench where


import Control.Lens
import Control.Monad
import Criterion
import Data.Text(Text)
import Data.Default
import System.FilePath
import NeatInterpolation (text)
import Control.Monad.Except
import Data.IORef
import Data.Map.Strict(Map)
import Data.Set(Set)
import System.ProgressBar
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Pact.Core.Environment
import Pact.Core.Names
import Pact.Core.Capabilities
import Pact.Core.PactValue
import Pact.Core.Guards
import Pact.Core.IR.Desugar
import Pact.Core.Compile
import Pact.Core.Evaluate
import Pact.Core.Hash
import Pact.Core.Persistence
import Pact.Core.Builtin
import Pact.Core.SPV
import Pact.Core.Gas.TableGasModel
import Pact.Core.Gas
import Pact.Core.Namespace
import Pact.Core.Serialise
import Pact.Core.Persistence.MockPersistence

import Pact.Core.Errors
import Pact.Core.Interpreter
import Data.Decimal
import Pact.Core.Pretty
import qualified Data.Text.Encoding as T

contractsPath :: FilePath
contractsPath = "gasmodel" </> "contracts"

-- | Create a single-key keyset
mkKs :: PublicKeyText -> PactValue
mkKs a = PGuard $ GKeyset $ KeySet (S.singleton a) KeysAll

interpretBigStep :: Interpreter ExecRuntime CoreBuiltin Info
interpretBigStep = evalInterpreter

interpretDirect :: Interpreter ExecRuntime CoreBuiltin Info
interpretDirect = evalDirectInterpreter


data CoinBenchSenders
  = CoinBenchSenderA
  | CoinBenchSenderB
  | CoinBenchSenderC
  | CoinBenchSenderD
  deriving Show

getSender :: CoinBenchSenders -> String
getSender = drop 9 . show

pubKeyFromSenderRaw :: CoinBenchSenders -> Text
pubKeyFromSenderRaw = \case
  CoinBenchSenderA -> senderKeyA
  CoinBenchSenderB -> senderKeyB
  CoinBenchSenderC -> senderKeyC
  CoinBenchSenderD -> senderKeyD

kColonFromSender :: CoinBenchSenders -> Text
kColonFromSender = ("k:" <>) . pubKeyFromSenderRaw

pubKeyFromSender :: CoinBenchSenders -> PublicKeyText
pubKeyFromSender = PublicKeyText . pubKeyFromSenderRaw

coinTableName :: TableName
coinTableName = TableName "coin-table" (ModuleName "coin" Nothing)

prePopulateCoinEntries :: Default i => PactDb CoreBuiltin i -> IO ()
prePopulateCoinEntries pdb = do
  let style = defStyle {stylePrefix = msg "Pre-filling the coin table"}
  putStrLn "Setting up the coin table"
  pbar <- newProgressBar style 10 (Progress 0 1_000 ())
  forM_ [1 :: Integer .. 1_000] $ \i -> do
    let n = renderCompactText $ pactHash $ T.encodeUtf8 $ T.pack (show i)
    let obj = M.fromList [(Field "balance", PDecimal 100), (Field "guard", PGuard (GKeyset (KeySet (S.singleton (PublicKeyText n)) KeysAll)))]
    ignoreGas def $ _pdbWrite pdb Write (DUserTables coinTableName) (RowKey n) (RowData obj)
    incProgress pbar 1


senderKeyA :: Text
senderKeyA = T.replicate 64 "a"
senderKeyB :: Text
senderKeyB = T.replicate 63 "a" <> "b"
senderKeyC :: Text
senderKeyC = T.replicate 63 "a" <> "c"
senderKeyD :: Text
senderKeyD = T.replicate 63 "a" <> "d"

coinInitData :: PactValue
coinInitData = PObject $ M.fromList $
  [ (Field "a", mkKs (pubKeyFromSender CoinBenchSenderA))
  , (Field "b", mkKs (pubKeyFromSender CoinBenchSenderB))
  , (Field "c", mkKs (pubKeyFromSender CoinBenchSenderC))
  , (Field "d", mkKs (pubKeyFromSender CoinBenchSenderD))
  ]

coinInitSigners :: M.Map PublicKeyText (Set (CapToken QualifiedName PactValue))
coinInitSigners = M.fromList $ fmap (over _1 PublicKeyText) $
  [ (senderKeyA, mempty)
  , (senderKeyB, mempty)
  , (senderKeyC, mempty)
  , (senderKeyD, mempty)
  ]

benchmarkSqliteFile :: String
benchmarkSqliteFile = "core-benches.sqlite"

transferCapFromSender :: CoinBenchSenders -> CoinBenchSenders -> Decimal -> CapToken QualifiedName PactValue
transferCapFromSender sender receiver amount =
  CapToken (QualifiedName "TRANSFER" (ModuleName "coin" Nothing))
    [ PString (kColonFromSender sender)
    , PString (kColonFromSender receiver)
    , PDecimal amount]

runPactTxFromSource
  :: EvalEnv CoreBuiltin Info
  -> Text
  -> Interpreter ExecRuntime CoreBuiltin Info
  -> IO (Either (PactError Info) [CompileValue Info],EvalState CoreBuiltin Info)
runPactTxFromSource ee source interpreter = runEvalM (ExecEnv ee) def $ do
  program <- liftEither $ compileOnlyLineInfo (RawCode source)
  traverse (interpretTopLevel interpreter) program

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


setupCoinTxs :: PactDb CoreBuiltin Info -> IO ()
setupCoinTxs pdb = do
  putStrLn "Setting up the coin contract and the default funds"
  source <- T.readFile (contractsPath </> "coin-v5-create.pact")
  ee <- setupBenchEvalEnv pdb coinInitSigners coinInitData
  () <$ runPactTxFromSource ee source evalDirectInterpreter


_run :: IO ()
_run = do
  pdb <- mockPactDb serialisePact_lineinfo
  setupCoinTxs pdb >>= print

coinTransferTxRaw :: Text -> Text -> Text
coinTransferTxRaw sender receiver =
  [text| (coin.transfer "$sender" "$receiver" 200.0) |]

coinTransferCreateTxRaw :: Text -> Text -> Text -> Text
coinTransferCreateTxRaw sender receiver receiverKs =
  [text| (coin.transfer-create "$sender" "$receiver" (read-keyset "$receiverKs") 200.0) |]

factorialNTXRaw :: Int -> Text
factorialNTXRaw n =
  [text| (fold * 1 (enumerate 1 ${n'})) |]
  where
  n' = T.pack (show n)

deepLetTXRaw :: Int -> Text
deepLetTXRaw n =
  [text| (let* ($nestedLets) $lastVar) |]
  where
  initial = "(x1 1)"
  nestedLets = T.concat $ initial :
    [ [text| (x$ncurr (* $ncurr x${nprev})) |] | (prev, curr) <- zip [1..n] [2..n]
    , let nprev = T.pack (show prev)
    , let ncurr = T.pack (show curr)]
  lastVar = "x" <> T.pack (show n)

getRightIO :: Show e => Either e a -> IO a
getRightIO = either (error . show) pure

resetEEGas :: EvalEnv b i -> IO ()
resetEEGas ee =
  writeIORef (_geGasRef $ _eeGasEnv ee) mempty


runEvalTx
  :: String
  -> Text
  -> PactDb CoreBuiltin Info
  -> Map PublicKeyText (Set (CapToken QualifiedName PactValue))
  -> PactValue
  -> Interpreter ExecRuntime CoreBuiltin Info -> Benchmark
runEvalTx title termToCompile pdb signers envdata interp =
  bench title $ perRunEnvWithCleanup mkTerm doRollback $ \ ~(term, ee, es) ->
    (getRightIO =<<) $ runEvalMResult (ExecEnv ee) es (eval interp PImpure term)
    where
    mkTerm = do
      _ <- ignoreGas def $ _pdbBeginTx pdb Transactional
      ee <- setupBenchEvalEnv pdb signers envdata
      -- note, we will return this eval state, as it definitely contains the loaded coin contract here.
      (eterm, es) <- runEvalM (ExecEnv ee) def $ do
        t <- liftEither $ compileOnlyTermLineInfo (RawCode termToCompile)
        _dsOut <$> runDesugarTerm t
      term <- getRightIO eterm
      pure (term, ee, def {_esLoaded = _esLoaded es})
    doRollback _ = do
      ignoreGas def $ _pdbRollbackTx pdb

runEvalDesugarTx
  :: String
  -> Text
  -> PactDb CoreBuiltin Info
  -> Map PublicKeyText (Set (CapToken QualifiedName PactValue))
  -> PactValue
  -> Interpreter ExecRuntime CoreBuiltin Info
  -> Benchmark
runEvalDesugarTx title termToCompile pdb signers envdata interp =
  bench title $ perRunEnvWithCleanup mkTerm doRollback $ \ ~(pterm, ee) ->
    (getRightIO =<<) $ runEvalMResult (ExecEnv ee) def $ do
      DesugarOutput term _ <- runDesugarTerm pterm
      eval interp PImpure term
    where
    mkTerm = do
      _ <- ignoreGas def $ _pdbBeginTx pdb Transactional
      ee <- setupBenchEvalEnv pdb signers envdata
      -- note, we will return this eval state, as it definitely contains the loaded coin contract here.
      pterm <- getRightIO $ liftEither $ compileOnlyTermLineInfo (RawCode termToCompile)
      pure (pterm, ee)
    doRollback _ = do
      ignoreGas def $ _pdbRollbackTx pdb

-- | Run A benchmark of pure code only, no db interactions.
--   Generally good for
runPureBench
  :: String
  -> Text
  -> PactDb CoreBuiltin Info
  -> Interpreter ExecRuntime CoreBuiltin Info
  -> Benchmark
runPureBench title termToCompile pdb interp =
  bench title $ perRunEnv mkTerm $ \ ~(term, ee) ->
    runEvalMResult (ExecEnv ee) def (eval interp PImpure term)
    where
    mkTerm = do
      ee <- setupBenchEvalEnv pdb mempty (PObject mempty)
      -- note, we will return this eval state, as it definitely contains the loaded coin contract here.
      eterm <- runEvalMResult (ExecEnv ee) def $ do
        t <- liftEither $ compileOnlyTermLineInfo (RawCode termToCompile)
        _dsOut <$> runDesugarTerm t
      term <- getRightIO eterm
      pure (term, ee)

runCoinTransferTx
  :: PactDb CoreBuiltin Info
  -> CoinBenchSenders
  -> CoinBenchSenders
  -> Interpreter ExecRuntime CoreBuiltin Info
  -> String
  -> Benchmark
runCoinTransferTx pdb sender receiver interp interpName =
  runEvalTx title termRaw pdb (transferSigners sender receiver) (PObject mempty) interp
    where
    termRaw = coinTransferTxRaw (kColonFromSender sender) (kColonFromSender receiver)
    title =
      "InterpretOnly(transfer) from "
      <> getSender sender
      <> " to "
      <> getSender receiver
      <> " using: "
      <> interpName

runCoinTransferCreateTx
  :: PactDb CoreBuiltin Info
  -> CoinBenchSenders
  -> CoinBenchSenders
  -> Interpreter ExecRuntime CoreBuiltin Info
  -> String
  -> Benchmark
runCoinTransferCreateTx pdb sender receiver interp interpName =
  runEvalTx title termRaw pdb signers envData interp
    where
    title =
      "InterpretOnly(transfer-create) from "
      <> getSender sender
      <> " to "
      <> getSender receiver
      <> " using: "
      <> interpName
    termRaw = coinTransferCreateTxRaw (kColonFromSender sender) (kColonFromSender receiver) "ks"
    signers = transferSigners sender receiver
    envData = PObject $ M.fromList [(Field "ks", mkKs (pubKeyFromSender receiver))]

runCoinTransferCreateTxDesugar
  :: PactDb CoreBuiltin Info
  -> CoinBenchSenders
  -> CoinBenchSenders
  -> Interpreter ExecRuntime CoreBuiltin Info
  -> String
  -> Benchmark
runCoinTransferCreateTxDesugar pdb sender receiver interp interpName =
  bench title $ perRunEnvWithCleanup mkTerm doRollback $ \ ~(term, ee) ->
    runEvalMResult (ExecEnv ee) def $ do
      t <- runDesugarTerm term
      eval interp PImpure (_dsOut t)
    where
    title =
      "Load+Link+Interpret(transfer-create) from "
      <> getSender sender
      <> " to "
      <> getSender receiver
      <> " using: "
      <> interpName
    mkTerm = do
      _ <- ignoreGas def $ _pdbBeginTx pdb Transactional
      let ks = M.fromList [(Field "ks", mkKs (pubKeyFromSender receiver))]
      ee <- setupBenchEvalEnv pdb (transferSigners sender receiver) (PObject ks)
      let termText = coinTransferCreateTxRaw (kColonFromSender sender) (kColonFromSender receiver) "ks"
      -- note, we will return this eval state, as it definitely contains the loaded coin contract here.
      eterm <- getRightIO $ compileOnlyTermLineInfo (RawCode termText)
      pure (eterm, ee)
    doRollback _ = do
      ignoreGas def $ _pdbRollbackTx pdb

runCoinTransferTxDesugar
  :: PactDb CoreBuiltin Info
  -> CoinBenchSenders
  -> CoinBenchSenders
  -> Interpreter ExecRuntime CoreBuiltin Info
  -> String
  -> Benchmark
runCoinTransferTxDesugar pdb sender receiver interp interpName =
  runEvalDesugarTx title termRaw pdb signers envData interp
    where
    title =
      "Load+Link+Interpret(transfer) from "
      <> getSender sender
      <> " to "
      <> getSender receiver
      <> " using: "
      <> interpName
    signers = transferSigners sender receiver
    envData = PObject mempty
    termRaw = coinTransferTxRaw (kColonFromSender sender) (kColonFromSender receiver)

transferSigners :: CoinBenchSenders -> CoinBenchSenders -> Map PublicKeyText (Set (CapToken QualifiedName PactValue))
transferSigners sender receiver =
  M.singleton (pubKeyFromSender sender) (S.singleton (transferCapFromSender sender receiver 200.0))

allBenchmarks :: Benchmark
allBenchmarks = do
  env mkPactDb $ \ ~(pdb) ->
    bgroup "Pact Core Benchmarks"
      [ pureBenchmarks pdb
      , coinTransferBenches pdb]
  where
  coinTransferBenches pdb =
    bgroup "Coin Transfer"
    [ runCoinTransferTx pdb CoinBenchSenderA CoinBenchSenderB interpretBigStep "CEK"
    , runCoinTransferTx pdb CoinBenchSenderA CoinBenchSenderB interpretDirect "Direct"
    -- Transfer with desugar
    , runCoinTransferTxDesugar pdb CoinBenchSenderA CoinBenchSenderB interpretBigStep "CEK"
    , runCoinTransferTxDesugar pdb CoinBenchSenderA CoinBenchSenderB interpretDirect "Direct"
    -- transfer-create sender A to B
    , runCoinTransferCreateTx pdb CoinBenchSenderA CoinBenchSenderB interpretBigStep "CEK"
    , runCoinTransferCreateTx pdb CoinBenchSenderA CoinBenchSenderB interpretDirect "Direct"
    -- transfer-create sender A to B with desugar
    , runCoinTransferCreateTxDesugar pdb CoinBenchSenderA CoinBenchSenderB interpretBigStep "CEK"
    , runCoinTransferCreateTxDesugar pdb CoinBenchSenderA CoinBenchSenderB interpretDirect "Direct"
    -- transfer-create sender A to C
    , runCoinTransferCreateTx pdb CoinBenchSenderA CoinBenchSenderC interpretBigStep "CEK"
    , runCoinTransferCreateTx pdb CoinBenchSenderA CoinBenchSenderC interpretDirect "Direct"
  -- transfer-create sender A to C with desugar
    , runCoinTransferCreateTxDesugar pdb CoinBenchSenderA CoinBenchSenderC interpretBigStep "CEK"
    , runCoinTransferCreateTxDesugar pdb CoinBenchSenderA CoinBenchSenderC interpretDirect "Direct"
    ]
  pureBenchmarks pdb = bgroup "Pure Code"
    [ runPureBench "Factorial 1000" (factorialNTXRaw 1000) pdb interpretBigStep
    , runPureBench "Let 100" (deepLetTXRaw 100) pdb interpretBigStep
    , runPureBench "Let 1000" (deepLetTXRaw 1000) pdb interpretBigStep
    -- , runPureBench "Let 10000" (deepLetTXRaw 10000) pdb interpretBigStep
    ]
  mkPactDb = do
    pdb <- mockPactDb serialisePact_lineinfo
    _ <- ignoreGas def $ _pdbBeginTx pdb Transactional
    _ <- setupCoinTxs pdb
    _ <- ignoreGas def $ _pdbCommitTx pdb
    _ <- ignoreGas def $ _pdbBeginTx pdb Transactional
    _ <- ignoreGas def $ _pdbCommitTx pdb
    pure pdb

unsafeModuleHash :: Text -> Hash
unsafeModuleHash e =
  let (Just (ModuleHash e')) = parseModuleHash e
  in e'


mkCoinIdent :: Text -> Name
mkCoinIdent n = Name n (NTopLevel (ModuleName "coin" Nothing) (ModuleHash {_mhHash = unsafeModuleHash "DFsR46Z3vJzwyd68i0MuxIF0JxZ_OJfIaMyFFgAyI4w"}))
