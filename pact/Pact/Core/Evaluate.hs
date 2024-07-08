{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Pact.Core.Evaluate
  ( MsgData(..)
  , RawCode(..)
  , EvalResult(..)
  , ContMsg(..)
  , Info
  , initMsgData
  , evalExec
  , evalExecDefaultState
  , evalContinuation
  , setupEvalEnv
  , interpret
  , compileOnly
  , compileOnlyTerm
  , evaluateDefaultState
  , Eval
  , EvalBuiltinEnv
  , evalTermExec
  , allModuleExports
  , evalDirectInterpreter
  , evalInterpreter
  ) where

import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Exception.Safe
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Default
import Data.Text (Text)
import Data.Map.Strict(Map)
import Data.IORef

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Pact.Core.Builtin
import Pact.Core.Compile
import Pact.Core.Environment
import Pact.Core.Errors
import Pact.Core.Hash (Hash)
import Pact.Core.IR.Eval.CoreBuiltin
import Pact.Core.Persistence
import Pact.Core.DefPacts.Types
import Pact.Core.Capabilities
import Pact.Core.PactValue
import Pact.Core.Gas
import Pact.Core.Names
import Pact.Core.Guards
import Pact.Core.SPV
import Pact.Core.Namespace
import Pact.Core.IR.Desugar
import Pact.Core.Verifiers
import Pact.Core.Interpreter
import qualified Pact.Core.IR.Eval.CEK as Eval
import qualified Pact.Core.IR.Eval.Direct.Evaluator as Direct
import qualified Pact.Core.Syntax.Lexer as Lisp
import qualified Pact.Core.Syntax.Parser as Lisp
import qualified Pact.Core.Syntax.ParseTree as Lisp
import Pact.Core.Info

type Eval = EvalM ExecRuntime CoreBuiltin Info

-- Our Builtin environment for evaluation in Chainweb prod
type EvalBuiltinEnv = Eval.CoreBuiltinEnv Info

evalInterpreter :: Interpreter ExecRuntime CoreBuiltin i
evalInterpreter =
  Interpreter runGuard runTerm resume
  where
  runTerm purity term = Eval.eval purity env term
  runGuard info g = Eval.interpretGuard info env g
  resume info defPact = Eval.evalResumePact info env defPact
  env = coreBuiltinEnv @ExecRuntime @Eval.CEKBigStep

evalDirectInterpreter :: Interpreter ExecRuntime CoreBuiltin i
evalDirectInterpreter =
  Interpreter runGuard runTerm resume
  where
  runTerm purity term = Direct.eval purity env term
  runGuard info g = Direct.interpretGuard info env g
  resume info defPact = Direct.evalResumePact info env defPact
  env = Direct.coreBuiltinEnv

-- | Transaction-payload related environment data.
data MsgData = MsgData
  { mdData :: !PactValue
  , mdStep :: !(Maybe DefPactStep)
  , mdHash :: !Hash
  , mdSigners :: [Signer QualifiedName PactValue]
  , mdVerifiers :: [Verifier ()]
  }

initMsgData :: Hash -> MsgData
initMsgData h = MsgData (PObject mempty) def h mempty mempty

type EvalInput = Either (Maybe DefPactExec) [Lisp.TopLevel Info]

newtype RawCode = RawCode { _rawCode :: Text }
  deriving (Eq, Show)

data ContMsg = ContMsg
  { _cmPactId :: !DefPactId
  , _cmStep :: !Int
  , _cmRollback :: !Bool
  , _cmData :: !PactValue
  , _cmProof :: !(Maybe ContProof)
  } deriving (Eq,Show)

-- | Results of evaluation.
data EvalResult tv = EvalResult
  { _erInput :: !(Either (Maybe DefPactExec) tv)
    -- ^ compiled user input
  , _erOutput :: ![CompileValue Info]
    -- ^ Output values
  , _erLogs :: ![TxLog ByteString]
    -- ^ Transaction logs
  , _erExec :: !(Maybe DefPactExec)
    -- ^ Result of defpact execution if any
  , _erGas :: Gas
    -- ^ Gas consumed/charged
  , _erLoadedModules :: Map ModuleName (ModuleData CoreBuiltin Info)
    -- ^ Modules loaded, with flag indicating "newly loaded"
  , _erTxId :: !(Maybe TxId)
    -- ^ Transaction id, if executed transactionally
  , _erLogGas :: Maybe [(Text, Gas)]
    -- ^ Details on each gas consumed/charged
  , _erEvents :: [PactEvent PactValue]
    -- ^ emitted events
  -- , _erWarnings :: S.Set PactWarning
    -- ^ emitted warning
  } deriving (Show)

type Info = SpanInfo

setupEvalEnv
  :: PactDb CoreBuiltin a
  -> ExecutionMode -- <- we have this
  -> MsgData -- <- create at type for this
  -> GasModel CoreBuiltin
  -> NamespacePolicy
  -> SPVSupport
  -> PublicData
  -> S.Set ExecutionFlag
  -> IO (EvalEnv CoreBuiltin a)
setupEvalEnv pdb mode msgData gasModel' np spv pd efs = do
  gasRef <- newIORef mempty
  gasLogRef <- newIORef Nothing
  let gasEnv = GasEnv
        { _geGasRef = gasRef
        , _geGasLogRef = gasLogRef
        , _geGasModel = gasModel'
        }
  pure $ EvalEnv
    { _eeMsgSigs = mkMsgSigs $ mdSigners msgData
    , _eeMsgVerifiers = mkMsgVerifiers $ mdVerifiers msgData
    , _eePactDb = pdb
    , _eeMsgBody = mdData msgData
    , _eeHash = mdHash msgData
    , _eePublicData = pd
    , _eeDefPactStep = mdStep msgData
    , _eeMode = mode
    , _eeFlags = efs
    , _eeNatives = coreBuiltinMap
    , _eeNamespacePolicy = np
    , _eeGasEnv = gasEnv
    , _eeSPVSupport = spv
    }
  where
  mkMsgSigs ss = M.fromList $ map toPair ss
    where
    toPair (Signer _scheme pubK addr capList) =
      (PublicKeyText (fromMaybe pubK addr),S.fromList capList)
  mkMsgVerifiers vs = M.fromListWith S.union $ map toPair vs
    where
    toPair (Verifier vfn _ caps) = (vfn, S.fromList caps)

evalExec :: EvalEnv CoreBuiltin Info -> EvalState CoreBuiltin Info -> RawCode -> IO (Either (PactError Info) (EvalResult [Lisp.TopLevel Info]))
evalExec evalEnv evalSt rc = do
  terms <- either throwM return $ compileOnly rc
  either throwError return <$> interpret evalEnv evalSt (Right terms)

evalTermExec
  :: EvalEnv CoreBuiltin Info
  -> EvalState CoreBuiltin Info
  -> Lisp.Expr Info
  -> IO (Either (PactError Info) (EvalResult (Lisp.Expr Info)))
evalTermExec evalEnv evalSt term =
  either throwError return <$> interpretOnlyTerm evalEnv evalSt term

evalContinuation :: EvalEnv CoreBuiltin Info -> EvalState CoreBuiltin Info -> ContMsg -> IO (Either (PactError Info) (EvalResult [Lisp.TopLevel Info]))
evalContinuation evalEnv evalSt cm = case _cmProof cm of
  Nothing ->
    interpret (setStep Nothing) evalSt (Left Nothing)
  Just p -> do
    etpe <- (_spvVerifyContinuation . _eeSPVSupport $ evalEnv) p
    pe <- either contError return etpe
    interpret (setStep (_peYield pe)) evalSt (Left $ Just pe)
  where
    contError spvErr = throw $ PEExecutionError (ContinuationError spvErr) [] ()
    setStep y = set eeDefPactStep (Just $ DefPactStep (_cmStep cm) (_cmRollback cm) (_cmPactId cm) y) evalEnv

evalExecDefaultState :: EvalEnv CoreBuiltin Info -> RawCode -> IO (Either (PactError Info) (EvalResult [Lisp.TopLevel Info]))
evalExecDefaultState evalEnv rc = evalExec evalEnv def rc

interpret
  :: EvalEnv CoreBuiltin Info
  -> EvalState CoreBuiltin Info
  -> EvalInput
  -> IO (Either (PactError Info) (EvalResult [Lisp.TopLevel Info]))
interpret evalEnv evalSt evalInput = do
  (result, state) <- runEvalM (ExecEnv evalEnv) evalSt $ evalWithinTx evalInput
  gas <- readIORef (_geGasRef $ _eeGasEnv evalEnv)
  case result of
    Left err -> return $ Left err
    Right (rs, logs, txid) ->
      return $! Right $! EvalResult
        { _erInput = evalInput
        , _erOutput = rs
        , _erLogs = logs
        , _erExec = _esDefPactExec state
        -- Todo: quotrem
        , _erGas = milliGasToGas gas
        , _erLoadedModules = _loModules $ _esLoaded state
        , _erTxId = txid
        , _erLogGas = Nothing
        , _erEvents = _esEvents state
        }

interpretOnlyTerm
  :: EvalEnv CoreBuiltin Info
  -> EvalState CoreBuiltin Info
  -> Lisp.Expr Info
  -> IO (Either (PactError Info) (EvalResult (Lisp.Expr Info)))
interpretOnlyTerm evalEnv evalSt term = do
  (result, state) <- runEvalM (ExecEnv evalEnv) evalSt $ evalCompiledTermWithinTx term
  gas <- readIORef (_geGasRef $ _eeGasEnv evalEnv)
  case result of
    Left err -> return $ Left err
    Right (rs, logs, txid) ->
      return $! Right $! EvalResult
        { _erInput = Right term
        , _erOutput = [InterpretValue rs (view Lisp.termInfo term)]
        , _erLogs = logs
        , _erExec = _esDefPactExec state
        -- todo: quotrem
        , _erGas = milliGasToGas gas
        , _erLoadedModules = _loModules $ _esLoaded state
        , _erTxId = txid
        , _erLogGas = Nothing
        , _erEvents = _esEvents state
        }

-- Used to be `evalTerms`
evalWithinTx
  :: EvalInput
  -> Eval ([CompileValue Info], [TxLog ByteString], Maybe TxId)
evalWithinTx input = withRollback (start runInput >>= end)

  where

    withRollback act =
      act `onException` safeRollback

    safeRollback =
      void (tryAny evalRollbackTx)

    start act = do
      pdb <- viewEvalEnv eePactDb
      mode <- viewEvalEnv eeMode
      txid <- liftDbFunction def (_pdbBeginTx pdb mode)
      (,txid) <$> act

    end (rs,txid) = do
      pdb <- viewEvalEnv eePactDb
      logs <- liftDbFunction def (_pdbCommitTx pdb)
      -- maybe might want to decode using serialisepact
      return (rs, logs, txid)

    runInput = case input of
      Right ts -> evaluateTerms ts
      Left pe -> (:[]) <$> evalResumePact pe

    evalRollbackTx = do
      esCaps .= def
      pdb <- viewEvalEnv eePactDb
      liftDbFunction def (_pdbRollbackTx pdb)

evalCompiledTermWithinTx
  :: Lisp.Expr Info
  -> Eval (PactValue, [TxLog ByteString], Maybe TxId)
evalCompiledTermWithinTx input = withRollback (start runInput >>= end)

  where

    withRollback act =
      act `onException` safeRollback

    safeRollback =
      void (tryAny evalRollbackTx)

    start act = do
      pdb <- viewEvalEnv eePactDb
      mode <- viewEvalEnv eeMode
      txid <- liftDbFunction def (_pdbBeginTx pdb mode)
      (,txid) <$> act

    end (rs,txid) = do
      pdb <- viewEvalEnv eePactDb
      logs <- liftDbFunction def (_pdbCommitTx pdb)
      -- maybe might want to decode using serialisepact
      return (rs, logs, txid)

    runInput = do
      DesugarOutput term' _ <- runDesugarTerm input
      eval evalInterpreter PImpure term'

    evalRollbackTx = do
      esCaps .= def
      pdb <- viewEvalEnv eePactDb
      liftDbFunction def (_pdbRollbackTx pdb)

-- | Runs only compilation pipeline
compileOnly :: RawCode -> Either (PactError Info) [Lisp.TopLevel Info]
compileOnly = (Lisp.lexer >=> Lisp.parseProgram) . _rawCode

-- | Runs only compilation pipeline for a single term
compileOnlyTerm :: RawCode -> Either (PactError Info) (Lisp.Expr Info)
compileOnlyTerm =
  (Lisp.lexer >=> Lisp.parseExpr) . _rawCode


evalResumePact
  :: Maybe DefPactExec
  -> Eval (CompileValue Info)
evalResumePact mdp =
  (`InterpretValue` def) <$> resumePact evalInterpreter def mdp

-- | Compiles and evaluates the code
evaluateDefaultState :: RawCode -> Eval [CompileValue Info]
evaluateDefaultState = either throwError evaluateTerms . compileOnly

evaluateTerms
  :: [Lisp.TopLevel Info]
  -> Eval [CompileValue Info]
evaluateTerms tls = do
  traverse (interpretTopLevel evalInterpreter) tls
