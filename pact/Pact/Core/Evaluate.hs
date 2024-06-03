{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Pact.Core.Evaluate
  ( MsgData(..)
  , RawCode(..)
  , EvalResult(..)
  , ContMsg(..)
  , initMsgData
  , evalExec
  , evalExecDefaultState
  , evalContinuation
  , setupEvalEnv
  , interpret
  , compileOnly
  , compileOnlyTerm
  , evaluateDefaultState
  , builtinEnv
  , Eval
  , EvalBuiltinEnv
  , evalTermExec
  , allModuleExports
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
import Pact.Core.IR.Eval.Runtime hiding (EvalResult)
import Pact.Core.Persistence
import Pact.Core.DefPacts.Types
import Pact.Core.Capabilities
import Pact.Core.PactValue
import Pact.Core.Gas
import Pact.Core.Info
import Pact.Core.Names
import Pact.Core.Guards
import Pact.Core.SPV
import Pact.Core.Namespace
import Pact.Core.IR.Desugar
import Pact.Core.Verifiers
import qualified Pact.Core.IR.Eval.CEK as Eval
import qualified Pact.Core.Syntax.Lexer as Lisp
import qualified Pact.Core.Syntax.Parser as Lisp
import qualified Pact.Core.Syntax.ParseTree as Lisp
import qualified Pact.Core.IR.Eval.Runtime.Types as Eval

-- Our Builtin environment for evaluation in Chainweb prod
type EvalBuiltinEnv = BuiltinEnv CEKBigStep CoreBuiltin SpanInfo (EvalM CoreBuiltin SpanInfo)

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

builtinEnv :: EvalBuiltinEnv
builtinEnv = coreBuiltinEnv @Eval.CEKBigStep @SpanInfo

type EvalInput = Either (Maybe DefPactExec) [Lisp.TopLevel SpanInfo]

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
  , _erOutput :: ![CompileValue SpanInfo]
    -- ^ Output values
  , _erLogs :: ![TxLog ByteString]
    -- ^ Transaction logs
  , _erExec :: !(Maybe DefPactExec)
    -- ^ Result of defpact execution if any
  , _erGas :: Gas
    -- ^ Gas consumed/charged
  , _erLoadedModules :: Map ModuleName (ModuleData CoreBuiltin SpanInfo)
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

setupEvalEnv
  :: PactDb CoreBuiltin SpanInfo
  -> ExecutionMode -- <- we have this
  -> MsgData -- <- create at type for this
  -> GasModel CoreBuiltin
  -> NamespacePolicy
  -> SPVSupport
  -> PublicData
  -> S.Set ExecutionFlag
  -> IO (EvalEnv CoreBuiltin SpanInfo)
setupEvalEnv pdb mode msgData gasModel np spv pd efs = do
  gasRef <- newIORef mempty
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
    , _eeGasRef = gasRef
    , _eeGasModel = gasModel
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

evalExec :: EvalEnv CoreBuiltin SpanInfo -> EvalState CoreBuiltin SpanInfo -> RawCode -> IO (Either (PactError SpanInfo) (EvalResult [Lisp.TopLevel SpanInfo]))
evalExec evalEnv evalSt rc = do
  terms <- either throwM return $ compileOnly rc
  either throwError return <$> interpret evalEnv evalSt (Right terms)

evalTermExec
  :: EvalEnv CoreBuiltin SpanInfo
  -> EvalState CoreBuiltin SpanInfo
  -> Lisp.Expr SpanInfo
  -> IO (Either (PactError SpanInfo) (EvalResult (Lisp.Expr SpanInfo)))
evalTermExec evalEnv evalSt term =
  either throwError return <$> interpretOnlyTerm evalEnv evalSt term

evalContinuation :: EvalEnv CoreBuiltin SpanInfo -> EvalState CoreBuiltin SpanInfo -> ContMsg -> IO (Either (PactError SpanInfo) (EvalResult [Lisp.TopLevel SpanInfo]))
evalContinuation evalEnv evalSt cm = case _cmProof cm of
  Nothing ->
    interpret (setStep Nothing) evalSt (Left Nothing)
  Just p -> do
    etpe <- (_spvVerifyContinuation . _eeSPVSupport $ evalEnv) p
    pe <- either contError return etpe
    interpret (setStep (_peYield pe)) evalSt (Left $ Just pe)
  where
    contError spvErr = throw $ PEExecutionError (ContinuationError spvErr) [] (def :: SpanInfo)
    setStep y = set eeDefPactStep (Just $ DefPactStep (_cmStep cm) (_cmRollback cm) (_cmPactId cm) y) evalEnv

evalExecDefaultState :: EvalEnv CoreBuiltin SpanInfo -> RawCode -> IO (Either (PactError SpanInfo) (EvalResult [Lisp.TopLevel SpanInfo]))
evalExecDefaultState evalEnv rc = evalExec evalEnv def rc

interpret :: EvalEnv CoreBuiltin SpanInfo -> EvalState CoreBuiltin SpanInfo -> EvalInput -> IO (Either (PactError SpanInfo) (EvalResult [Lisp.TopLevel SpanInfo]))
interpret evalEnv evalSt evalInput = do
  (result, state) <- runEvalM evalEnv evalSt $ evalWithinTx evalInput
  gas <- readIORef (_eeGasRef evalEnv)
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
  :: EvalEnv CoreBuiltin SpanInfo
  -> EvalState CoreBuiltin SpanInfo
  -> Lisp.Expr SpanInfo
  -> IO (Either (PactError SpanInfo) (EvalResult (Lisp.Expr SpanInfo)))
interpretOnlyTerm evalEnv evalSt term = do
  (result, state) <- runEvalM evalEnv evalSt $ evalCompiledTermWithinTx term
  gas <- readIORef (_eeGasRef evalEnv)
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
  -> EvalM CoreBuiltin SpanInfo ([CompileValue SpanInfo], [TxLog ByteString], Maybe TxId)
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
      Left pe -> (:[]) <$> resumePact pe

    evalRollbackTx = do
      esCaps .== def
      pdb <- viewEvalEnv eePactDb
      liftDbFunction def (_pdbRollbackTx pdb)

evalCompiledTermWithinTx
  :: Lisp.Expr SpanInfo
  -> EvalM CoreBuiltin SpanInfo (PactValue, [TxLog ByteString], Maybe TxId)
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
      Eval.eval PImpure builtinEnv term'

    evalRollbackTx = do
      esCaps .== def
      pdb <- viewEvalEnv eePactDb
      liftDbFunction def (_pdbRollbackTx pdb)

-- | Runs only compilation pipeline
compileOnly :: RawCode -> Either (PactError SpanInfo) [Lisp.TopLevel SpanInfo]
compileOnly = -- bimap void (fmap void) .
  (Lisp.lexer >=> Lisp.parseProgram) . _rawCode

-- | Runs only compilation pipeline for a single term
compileOnlyTerm :: RawCode -> Either (PactError SpanInfo) (Lisp.Expr SpanInfo)
compileOnlyTerm =
  -- bimap void void .
  (Lisp.lexer >=> Lisp.parseExpr) . _rawCode


resumePact
  :: Maybe DefPactExec
  -> EvalM CoreBuiltin SpanInfo (CompileValue SpanInfo)
resumePact mdp =
  (`InterpretValue` def) <$> Eval.evalResumePact def builtinEnv mdp

-- | Compiles and evaluates the code
evaluateDefaultState :: RawCode -> EvalM CoreBuiltin SpanInfo [CompileValue SpanInfo]
evaluateDefaultState = either throwError evaluateTerms . compileOnly

evaluateTerms
  :: [Lisp.TopLevel SpanInfo]
  -> EvalM CoreBuiltin SpanInfo [CompileValue SpanInfo]
evaluateTerms tls = do
  traverse (interpretTopLevel builtinEnv) tls
