{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Pact.Core.GasModel.ContractBench where


import Control.Lens
import Control.Monad
import Criterion
import Control.Exception
import Data.Text(Text)
import Data.Default
import System.Directory
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
import qualified Database.SQLite3.Direct as SQL

import Pact.Core.Environment
import Pact.Core.Names
import Pact.Core.Capabilities
import Pact.Core.PactValue
import Pact.Core.Guards
import Pact.Core.IR.Desugar
import Pact.Core.IR.Eval.Runtime
import Pact.Core.Compile
import Pact.Core.Evaluate
import Pact.Core.Hash
import Pact.Core.Persistence
import Pact.Core.Builtin
import Pact.Core.SPV
import qualified Pact.Core.Syntax.Parser as Lisp
import qualified Pact.Core.Syntax.Lexer as Lisp
import qualified Pact.Core.Syntax.LexUtils as Lisp
import qualified Pact.Core.IR.Eval.CEK as CEK
import qualified Pact.Core.IR.Eval.CoreBuiltin as CEK
import qualified Pact.Core.IR.Eval.CEKSpecialized as CEKSpec
import qualified Pact.Core.IR.Eval.Direct.Evaluator as Direct
import Pact.Core.Gas.TableGasModel
import Pact.Core.Gas
import Pact.Core.Namespace
import Pact.Core.Serialise
import Pact.Core.Persistence.MockPersistence

import Pact.Core.Info
import Pact.Core.Errors
import Pact.Core.Interpreter
import Pact.Core.Persistence.SQLite
import Pact.Core.GasModel.Utils
import Data.Decimal
import Criterion.Main
import Pact.Core.Pretty
import qualified Data.Text.Encoding as T

parseOnlyExpr :: Text -> Either PactErrorI Lisp.ParsedExpr
parseOnlyExpr =
  Lisp.lexer >=> Lisp.parseExpr

contractsPath :: FilePath
contractsPath = "gasmodel" </> "contracts"

-- | Create a single-key keyset
mkKs :: PublicKeyText -> PactValue
mkKs a = PGuard $ GKeyset $ KeySet (S.singleton a) KeysAll

interpretBigStep :: Interpreter CoreBuiltin SpanInfo (EvalM CoreBuiltin SpanInfo)
interpretBigStep =
  Interpreter runGuard runTerm
  where
  runTerm purity term = CEK.eval purity eEnv term
  runGuard info g = CEK.interpretGuard info eEnv g
  eEnv = CEK.coreBuiltinEnv @CEK.CEKBigStep

interpretSpecialized :: Interpreter CoreBuiltin SpanInfo (EvalM CoreBuiltin SpanInfo)
interpretSpecialized =
  Interpreter runGuard runTerm
  where
  runTerm purity term = CEKSpec.eval purity term
  runGuard info g = CEKSpec.interpretGuard info g

interpretDirect :: Interpreter CoreBuiltin SpanInfo (EvalM CoreBuiltin SpanInfo)
interpretDirect =
  Interpreter runGuard runTerm
  where
  runTerm purity term = Direct.eval purity eEnv term
  runGuard info g = Direct.interpretGuard info eEnv g
  eEnv = Direct.coreBuiltinEnv


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

prePopulateCoinEntries :: Default i => PactDb b i -> IO ()
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
  :: EvalEnv CoreBuiltin SpanInfo
  -> Text
  -> Interpreter CoreBuiltin SpanInfo (EvalM CoreBuiltin SpanInfo)
  -> IO (Either (PactError SpanInfo) [CompileValue SpanInfo],EvalState CoreBuiltin SpanInfo)
runPactTxFromSource ee source interpreter = runEvalM ee def $ do
  program <- liftEither $ parseOnlyProgram source
  traverse (interpretTopLevel interpreter) program

setupBenchEvalEnv
  :: PactDb CoreBuiltin i
  -> Map PublicKeyText (Set (CapToken QualifiedName PactValue))
  -> PactValue -> IO (EvalEnv CoreBuiltin i)
setupBenchEvalEnv pdb signers mBody = do
  gasRef <- newIORef mempty
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
    , _eeGasRef = gasRef
    , _eeGasModel = tableGasModel (MilliGasLimit (MilliGas 200_000_000))
    , _eeSPVSupport = noSPVSupport
    }


setupCoinTxs :: PactDb CoreBuiltin SpanInfo -> IO ()
setupCoinTxs pdb = do
  putStrLn "Setting up the coin contract and the default funds"
  source <- T.readFile (contractsPath </> "coin-v5-create.pact")
  ee <- setupBenchEvalEnv pdb coinInitSigners coinInitData
  () <$ runPactTxFromSource ee source evalDirectInterpreter


_run :: IO ()
_run = do
  pdb <- mockPactDb serialisePact_raw_spaninfo
  setupCoinTxs pdb >>= print

coinTransferTxRaw :: Text -> Text -> Text
coinTransferTxRaw sender receiver =
  [text| (coin.transfer "$sender" "$receiver" 200.0) |]

coinTransferCreateTxRaw :: Text -> Text -> Text -> Text
coinTransferCreateTxRaw sender receiver receiverKs =
  [text| (coin.transfer-create "$sender" "$receiver" (read-keyset "$receiverKs") 200.0) |]

getRightIO :: Exception e => Either e a -> IO a
getRightIO = either throwIO pure

resetEEGas :: EvalEnv b i -> IO ()
resetEEGas ee =
  writeIORef (_eeGasRef ee) mempty


runEvalTx
  :: String
  -> Text
  -> PactDb CoreBuiltin SpanInfo
  -> Map PublicKeyText (Set (CapToken QualifiedName PactValue))
  -> PactValue
  -> Interpreter CoreBuiltin SpanInfo (EvalM CoreBuiltin SpanInfo) -> Benchmark
runEvalTx title termToCompile pdb signers envdata interp =
  bench title $ perRunEnvWithCleanup mkTerm doRollback $ \ ~(term, ee, es) ->
    fst <$> runEvalM ee es (eval interp PImpure term)
    where
    mkTerm = do
      () <$ _pdbBeginTx pdb Transactional
      ee <- setupBenchEvalEnv pdb signers envdata
      -- note, we will return this eval state, as it definitely contains the loaded coin contract here.
      (eterm, es) <- runEvalM ee def $ do
        t <- liftEither $ parseOnlyExpr termToCompile
        _dsOut <$> runDesugarTerm t
      term <- getRightIO eterm
      pure (term, ee, def {_esLoaded = _esLoaded es})
    doRollback _ = do
      _pdbRollbackTx pdb

runCoinTransferTx
  :: PactDb CoreBuiltin SpanInfo
  -> CoinBenchSenders
  -> CoinBenchSenders
  -> Interpreter CoreBuiltin SpanInfo (EvalM CoreBuiltin SpanInfo)
  -> String
  -> Benchmark
runCoinTransferTx pdb sender receiver interp interpName =
  runEvalTx title termRaw pdb (transferSigners sender receiver) (PObject mempty) interp
  -- bench title $ perRunEnvWithCleanup mkTerm doRollback $ \ ~(term, ee, es) ->
  --   fst <$> runEvalM ee es (eval interp PImpure term)
    where
    termRaw = coinTransferTxRaw (kColonFromSender sender) (kColonFromSender receiver)
    title =
      "Coin transfer from "
      <> getSender sender
      <> " to "
      <> getSender receiver
      <> " using: "
      <> interpName

runCoinTransferCreateTx
  :: PactDb CoreBuiltin SpanInfo
  -> CoinBenchSenders
  -> CoinBenchSenders
  -> Interpreter CoreBuiltin SpanInfo (EvalM CoreBuiltin SpanInfo)
  -> String
  -> Benchmark
runCoinTransferCreateTx pdb sender receiver interp interpName =
  bench title $ perRunEnvWithCleanup mkTerm doRollback $ \ ~(term, ee, es) ->
    fst <$> runEvalM ee es (eval interp PImpure term)
    where
    title =
      "Coin transfer-create from "
      <> getSender sender
      <> " to "
      <> getSender receiver
      <> " using: "
      <> interpName
    mkTerm = do
      () <$ _pdbBeginTx pdb Transactional
      let ks = M.fromList [(Field "ks", mkKs (pubKeyFromSender receiver))]
      ee <- setupBenchEvalEnv pdb (transferSigners sender receiver) (PObject ks)
      let termText = coinTransferCreateTxRaw (kColonFromSender sender) (kColonFromSender receiver) "ks"
      -- note, we will return this eval state, as it definitely contains the loaded coin contract here.
      (eterm, es) <- runEvalM ee def $ do
        t <- liftEither $ parseOnlyExpr termText
        _dsOut <$> runDesugarTerm t
      term <- getRightIO eterm
      pure (term, ee, es)
    doRollback _ = do
      _pdbRollbackTx pdb

runCoinTransferCreateTxDesugar
  :: PactDb CoreBuiltin SpanInfo
  -> CoinBenchSenders
  -> CoinBenchSenders
  -> Interpreter CoreBuiltin SpanInfo (EvalM CoreBuiltin SpanInfo)
  -> String
  -> Benchmark
runCoinTransferCreateTxDesugar pdb sender receiver interp interpName =
  bench title $ perRunEnvWithCleanup mkTerm doRollback $ \ ~(term, ee) ->
    fmap fst $ runEvalM ee def $ do
      t <- runDesugarTerm term
      eval interp PImpure (_dsOut t)
    where
    title =
      "Coin transfer-create from "
      <> getSender sender
      <> " to "
      <> getSender receiver
      <> " including name resolution: "
      <> interpName
    mkTerm = do
      () <$ _pdbBeginTx pdb Transactional
      let ks = M.fromList [(Field "ks", mkKs (pubKeyFromSender receiver))]
      ee <- setupBenchEvalEnv pdb (transferSigners sender receiver) (PObject ks)
      let termText = coinTransferCreateTxRaw (kColonFromSender sender) (kColonFromSender receiver) "ks"
      -- note, we will return this eval state, as it definitely contains the loaded coin contract here.
      eterm <- getRightIO $ parseOnlyExpr termText
      pure (eterm, ee)
    doRollback _ = do
      _pdbRollbackTx pdb

runCoinTransferTxDesugar
  :: PactDb CoreBuiltin SpanInfo
  -> CoinBenchSenders
  -> CoinBenchSenders
  -> Interpreter CoreBuiltin SpanInfo (EvalM CoreBuiltin SpanInfo)
  -> String
  -> Benchmark
runCoinTransferTxDesugar pdb sender receiver interp interpName =
  bench title $ perRunEnvWithCleanup mkTerm doRollback $ \ ~(term, ee) ->
    fmap fst $ runEvalM ee def $ do
      t <- runDesugarTerm term
      eval interp PImpure (_dsOut t)
    where
    title =
      "Coin transfer from "
      <> getSender sender
      <> " to "
      <> getSender receiver
      <> " including name resolution: "
      <> interpName
    mkTerm = do
      () <$ _pdbBeginTx pdb Transactional
      ee <- setupBenchEvalEnv pdb (transferSigners sender receiver) (PObject mempty)
      let termText = coinTransferTxRaw (kColonFromSender sender) (kColonFromSender receiver)
      -- note, we will return this eval state, as it definitely contains the loaded coin contract here.
      parsedTerm <- getRightIO $ parseOnlyExpr termText
      pure (parsedTerm, ee)
    doRollback _ = do
      _pdbRollbackTx pdb

transferSigners :: CoinBenchSenders -> CoinBenchSenders -> Map PublicKeyText (Set (CapToken QualifiedName PactValue))
transferSigners sender receiver =
  M.singleton (pubKeyFromSender sender) (S.singleton (transferCapFromSender sender receiver 200.0))

allBenchmarks :: Bool -> Benchmark
allBenchmarks resetDb = do
  envWithCleanup mkPactDb cleanupPactDb $ \ ~(pdb, _db) ->
    bgroup "Coin benches"
      [coinTransferBenches pdb]
  where
  coinTransferBenches pdb =
    bgroup "transfer benchmarks" [
    -- [ runCoinTransferTx pdb CoinBenchSenderA CoinBenchSenderB interpretBigStep "CEK"
    -- , runCoinTransferTx pdb CoinBenchSenderA CoinBenchSenderB interpretDirect "Direct"
    runCoinTransferTx pdb CoinBenchSenderA CoinBenchSenderB interpretSpecialized "CEKSpec"
    -- , runCoinTransferTxDesugar pdb CoinBenchSenderA CoinBenchSenderB interpretBigStep "CEK"
    -- , runCoinTransferTxDesugar pdb CoinBenchSenderA CoinBenchSenderB interpretDirect "Direct"
    -- , runCoinTransferTxDesugar pdb CoinBenchSenderA CoinBenchSenderB interpretSpecialized "CEKSpec"
    -- transfer-create sender A to B
    -- , runCoinTransferCreateTx pdb CoinBenchSenderA CoinBenchSenderB interpretBigStep "CEK"
    -- , runCoinTransferCreateTx pdb CoinBenchSenderA CoinBenchSenderB interpretDirect "Direct"
    -- , runCoinTransferCreateTxDesugar pdb CoinBenchSenderA CoinBenchSenderB interpretBigStep "CEK"
    -- , runCoinTransferCreateTxDesugar pdb CoinBenchSenderA CoinBenchSenderB interpretDirect "Direct"
    -- transfer-create sender A to C
    -- , runCoinTransferCreateTx pdb CoinBenchSenderA CoinBenchSenderC interpretBigStep "CEK"
    -- , runCoinTransferCreateTx pdb CoinBenchSenderA CoinBenchSenderC interpretDirect "Direct"
    -- , runCoinTransferCreateTxDesugar pdb CoinBenchSenderA CoinBenchSenderC interpretBigStep "CEK"
    -- , runCoinTransferCreateTxDesugar pdb CoinBenchSenderA CoinBenchSenderC interpretDirect "Direct"
    ]
  mkPactDb = do
    c <- doesFileExist benchmarkSqliteFile
    when (c && resetDb) $ removeFile benchmarkSqliteFile
    (pdb, db) <- unsafeCreateSqlitePactDb serialisePact_raw_spaninfo (T.pack benchmarkSqliteFile)
    _ <- _pdbBeginTx pdb Transactional
    _ <- setupCoinTxs pdb
    _ <- _pdbCommitTx pdb
    _ <- _pdbBeginTx pdb Transactional
    when resetDb $ prePopulateCoinEntries pdb
    _ <- _pdbCommitTx pdb
    pure (pdb, SqliteDbNF db)
  cleanupPactDb (_, SqliteDbNF db) = do
    SQL.close db

_testRunBench :: IO ()
_testRunBench = do
  defaultMain [allBenchmarks True]

_testCoinTransfer :: IO ()
_testCoinTransfer = withSqlitePactDb serialisePact_raw_spaninfo (T.pack benchmarkSqliteFile) $ \pdb -> do
  _ <- _pdbBeginTx pdb Transactional
  p <- setupCoinTxs pdb
  print p
  _ <- _pdbCommitTx pdb *> _pdbBeginTx pdb Transactional
  ee <- setupBenchEvalEnv pdb (transferSigners CoinBenchSenderA CoinBenchSenderB) (PObject mempty)
  let termText = coinTransferTxRaw (kColonFromSender CoinBenchSenderA) (kColonFromSender CoinBenchSenderB)
  print termText
  -- note, we will return this eval state, as it definitely contains the loaded coin contract here.
  (eterm, es) <- runEvalM ee def $ do
    t <- liftEither $ parseOnlyExpr termText
    _dsOut <$> runDesugarTerm t
  term <- getRightIO eterm
  (out, _) <- runEvalM ee es (eval interpretDirect PImpure term)
  print out
