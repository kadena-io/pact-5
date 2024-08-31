
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import Control.Lens
import Control.Monad
import Control.Exception
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
import qualified Pact.Core.Syntax.Parser as Lisp
import qualified Pact.Core.Syntax.Lexer as Lisp
import qualified Pact.Core.Syntax.LexUtils as Lisp
import qualified Pact.Core.IR.Eval.CEK as CEK
import qualified Pact.Core.IR.Eval.CoreBuiltin as CEK
import qualified Pact.Core.IR.Eval.Direct.Evaluator as Direct
import Pact.Core.Gas.TableGasModel
import Pact.Core.Gas
import Pact.Core.Namespace
import Pact.Core.IR.Term
import Pact.Core.Serialise
import Pact.Core.Persistence.MockPersistence

import Pact.Core.Info
import Pact.Core.Errors
import Pact.Core.Interpreter
import Pact.Core.Persistence.SQLite
import Data.Decimal
import Pact.Core.Pretty
import Pact.Core.Literal
import qualified Data.Text.Encoding as T

parseOnlyExpr :: Text -> Either PactErrorI Lisp.ParsedExpr
parseOnlyExpr =
  Lisp.lexer >=> Lisp.parseExpr

contractsPath :: FilePath
contractsPath = "gasmodel" </> "contracts"

-- | Create a single-key keyset
mkKs :: PublicKeyText -> PactValue
mkKs a = PGuard $ GKeyset $ KeySet (S.singleton a) KeysAll

interpretBigStep :: Interpreter ExecRuntime CoreBuiltin SpanInfo
interpretBigStep =
  Interpreter runGuard runTerm evalResumePact
  where
  runTerm purity term = CEK.eval purity eEnv term
  runGuard info g = CEK.interpretGuard info eEnv g
  eEnv = CEK.coreBuiltinEnv @ExecRuntime
  evalResumePact info pactExec = CEK.evalResumePact info eEnv pactExec


interpretDirect :: Interpreter ExecRuntime CoreBuiltin SpanInfo
interpretDirect =
  Interpreter runGuard runTerm evalResumePact
  where
  runTerm purity term = Direct.eval purity eEnv term
  runGuard info g = Direct.interpretGuard info eEnv g
  eEnv = Direct.coreBuiltinEnv
  evalResumePact info pactExec = Direct.evalResumePact info eEnv pactExec


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
benchmarkSqliteFile = "profile-tx.sqlite"

transferCapFromSender :: CoinBenchSenders -> CoinBenchSenders -> Decimal -> CapToken QualifiedName PactValue
transferCapFromSender sender receiver amount =
  CapToken (QualifiedName "TRANSFER" (ModuleName "coin" Nothing))
    [ PString (kColonFromSender sender)
    , PString (kColonFromSender receiver)
    , PDecimal amount]

runPactTxFromSource
  :: EvalEnv CoreBuiltin SpanInfo
  -> Text
  -> Interpreter ExecRuntime CoreBuiltin SpanInfo
  -> IO (Either (PactError SpanInfo) [CompileValue SpanInfo],EvalState CoreBuiltin SpanInfo)
runPactTxFromSource ee source interpreter = runEvalM (ExecEnv ee) def $ do
  program <- liftEither $ parseOnlyProgram source
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

factorialNTXRaw :: Int -> Text
factorialNTXRaw n =
  [text| (fold (*) 1 (enumerate 1 ${n'})) |]
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

getRightIO :: Exception e => Either e a -> IO a
getRightIO = either throwIO pure

resetEEGas :: EvalEnv b i -> IO ()
resetEEGas ee =
  writeIORef (_geGasRef $ _eeGasEnv ee) mempty


transferSigners :: CoinBenchSenders -> CoinBenchSenders -> Map PublicKeyText (Set (CapToken QualifiedName PactValue))
transferSigners sender receiver =
  M.singleton (pubKeyFromSender sender) (S.singleton (transferCapFromSender sender receiver 200.0))

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
  (eterm, es) <- runEvalM (ExecEnv ee) def $ do
    t <- liftEither $ parseOnlyExpr termText
    _dsOut <$> runDesugarTerm t
  term <- getRightIO eterm
  (out, _) <- runEvalM (ExecEnv ee) es (eval interpretDirect PImpure term)
  print out

unsafeModuleHash :: Text -> Hash
unsafeModuleHash e =
  let (Just (ModuleHash e')) = parseModuleHash e
  in e'

withTx :: PactDb b i -> IO a -> IO a
withTx pdb act = do
  () <$ _pdbBeginTx pdb Transactional
  v <- act
  _ <- _pdbCommitTx pdb
  pure v


runCoinXferDirect :: PactDb CoreBuiltin SpanInfo -> IO ()
runCoinXferDirect pdb =  do
  ee <- setupBenchEvalEnv pdb (transferSigners CoinBenchSenderA CoinBenchSenderB) (PObject mempty)
  (m, es) <- runEvalM (ExecEnv ee) def $ getModule def pdb (ModuleName "coin" Nothing)
  _ <- getRightIO m
  let es' = def {_esLoaded=_esLoaded es}
  forM_ [1 :: Integer .. 1000] $ \_ -> withTx pdb $ do
    (out, _) <- runEvalM (ExecEnv ee) es' $ eval interpretBigStep PImpure term
    writeIORef (_geGasRef $ _eeGasEnv ee) mempty
    either throw print out
  pure ()
  where
  term = App (Var (mkCoinIdent "transfer") def)
    [ Constant (LString (kColonFromSender CoinBenchSenderA)) def
    , Constant (LString (kColonFromSender CoinBenchSenderB)) def
    , Constant (LDecimal 200) def] def


mkCoinIdent :: Text -> Name
mkCoinIdent n = Name n (NTopLevel (ModuleName "coin" Nothing) (ModuleHash {_mhHash = unsafeModuleHash "DFsR46Z3vJzwyd68i0MuxIF0JxZ_OJfIaMyFFgAyI4w"}))

main :: IO ()
main = withSqlitePactDb serialisePact_raw_spaninfo (T.pack benchmarkSqliteFile) $ \pdb -> do
  withTx pdb $ setupCoinTxs pdb
  withTx pdb $ prePopulateCoinEntries pdb
  runCoinXferDirect pdb
