{-# LANGUAGE PartialTypeSignatures #-}

module Pact.Core.Evaluate
  ( MsgData(..)
  , EvalResult(..)
  , initMsgData
  , evalExec
  , setupEvalEnv
  , interpret
  , compileOnly
  ) where

import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Exception.Safe
import Data.ByteString (ByteString)
import Data.Default
import Data.Text (Text)
import Data.Map.Strict(Map)

import qualified Data.Set as S

import Pact.Core.Builtin
import Pact.Core.Compile
import Pact.Core.Environment
import Pact.Core.Errors
import Pact.Core.Hash (Hash)
import Pact.Core.Interpreter
import Pact.Core.IR.Eval.RawBuiltin
import Pact.Core.IR.Eval.Runtime hiding (EvalResult)
import Pact.Core.IR.Term
import Pact.Core.Persistence
import Pact.Core.DefPacts.Types
import Pact.Core.Capabilities
import Pact.Core.PactValue
import Pact.Core.Gas
import Pact.Core.Names
import Pact.Core.Namespace
import qualified Pact.Core.IR.Eval.CEK as Eval
import qualified Pact.Core.Syntax.Lexer as Lisp
import qualified Pact.Core.Syntax.Parser as Lisp
import qualified Pact.Core.Syntax.ParseTree as Lisp

-- | Transaction-payload related environment data.
data MsgData = MsgData
  { mdData :: !(ObjectData PactValue)
  , mdStep :: !(Maybe DefPactStep)
  , mdHash :: !Hash
  -- , mdSigners :: [Signer]
  }

initMsgData :: Hash -> MsgData
initMsgData h = MsgData (ObjectData mempty) def h

type EvalInput = Either (Maybe DefPactExec) [Lisp.TopLevel ()]

newtype RawCode = RawCode { _rawCode :: Text }
  deriving (Eq, Show)

-- | Results of evaluation.
data EvalResult = EvalResult
  { _erInput :: !EvalInput
    -- ^ compiled user input
  , _erOutput :: ![CompileValue ()] -- TODO: it was PactValue previously which was more informative
    -- ^ Output values
  , _erLogs :: ![TxLog ByteString]
    -- ^ Transaction logs
  , _erExec :: !(Maybe DefPactExec)
    -- ^ Result of defpact execution if any
  , _erGas :: Gas
    -- ^ Gas consumed/charged
  , _erLoadedModules :: Map ModuleName (ModuleData RawBuiltin ())
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
  :: PactDb RawBuiltin ()
  -> ExecutionMode -- <- we have this
  -> MsgData -- <- create at type for this
  -- -> GasEnv -- <- also have this, use constant gas model
  -> NamespacePolicy -- <- also have this, as-is
  -- -> SPVSupport -- <- WIP: Ignore for now
  -> PublicData -- <- we have this
  -> S.Set ExecutionFlag
  -> IO (EvalEnv RawBuiltin ())
setupEvalEnv pdb mode msgData np pd efs = do
  pure EvalEnv
    { _eeMsgSigs = mempty
    , _eePactDb = pdb
    , _eeMsgBody = mdData msgData
    , _eeHash = mdHash msgData
    , _eePublicData = pd
    , _eeDefPactStep = mdStep msgData
    , _eeMode = mode
    , _eeFlags = efs
    , _eeNatives = rawBuiltinMap
    , _eeNamespacePolicy = np
    }

evalExec :: EvalEnv RawBuiltin () -> RawCode -> IO EvalResult
evalExec evalEnv rc = do
  terms <- either throwM return $ compileOnly rc
  interpret evalEnv (Right terms)

interpret :: EvalEnv RawBuiltin () -> EvalInput -> IO EvalResult
interpret evalEnv evalInput = do
  (result, state) <- runEvalM evalEnv def $ evalWithinTx evalInput
  (rs, logs, txid) <- either throwM return result
  return $! EvalResult
    { _erInput = evalInput
    , _erOutput = rs
    , _erLogs = logs
    , _erExec = _esDefPactExec state
    , _erGas = Gas 0 -- TODO: return gas
    , _erLoadedModules = _loModules $ _esLoaded state
    , _erTxId = txid
    , _erLogGas = Nothing
    , _erEvents = _esEvents state
    }

-- Used to be `evalTerms`
evalWithinTx
  :: EvalInput
  -> EvalM RawBuiltin () ([CompileValue ()], [TxLog ByteString], Maybe TxId)
evalWithinTx input = withRollback (start runInput >>= end)

  where

    withRollback act =
      act `onException` safeRollback

    safeRollback =
      void (tryAny evalRollbackTx)

    start act = do
      pdb <- viewEvalEnv eePactDb
      mode <- viewEvalEnv eeMode
      txid <- liftDbFunction () (_pdbBeginTx pdb mode)
      (,txid) <$> act

    end (rs,txid) = do
      pdb <- viewEvalEnv eePactDb
      logs <- liftDbFunction () (_pdbCommitTx pdb)
      -- maybe might want to decode using serialisepact
      return (rs, logs, txid)

    runInput = case input of
      Right ts -> evaluateTerms ts
      Left pe -> (:[]) <$> resumePact pe

    evalRollbackTx = do
      esCaps .== def
      pdb <- viewEvalEnv eePactDb
      liftDbFunction () (_pdbRollbackTx pdb)

-- | Runs only compilation pipeline
compileOnly :: RawCode -> Either (PactError ()) [Lisp.TopLevel ()]
compileOnly = bimap void (fmap void) . (Lisp.lexer >=> Lisp.parseProgram) . _rawCode

resumePact
  :: Maybe DefPactExec
  -> EvalM RawBuiltin () (CompileValue ())
resumePact mdp = do
  evalEnv <- viewEvalEnv id
  let pdb = _eePactDb evalEnv
  let env = CEKEnv mempty pdb rawBuiltinEnv (_eeDefPactStep evalEnv) False
  ev <- Eval.resumePact () Mt CEKNoHandler env mdp
  InterpretValue <$> case ev of
    VError txt i ->
      throwError (PEExecutionError (EvalError txt) i)
    EvalValue v -> do
      case v of
        VClosure{} -> do
          pure IPClosure
        VTable tv -> pure (IPTable (_tvName tv))
        VPactValue pv -> do
          pure (IPV pv ())


evaluateTerms
  :: [Lisp.TopLevel ()]
  -> EvalM RawBuiltin () [CompileValue ()]
evaluateTerms tls = do
  traverse (interpretTopLevel $ Interpreter interpretExpr interpretGuard) tls

  where
  interpretGuard i g = do
    evalEnv <- viewEvalEnv id
    let pdb = _eePactDb evalEnv
    let env = CEKEnv mempty pdb rawBuiltinEnv (_eeDefPactStep evalEnv) False
    ev <- coreEnforceGuard i RawEnforceGuard Mt CEKNoHandler env [VGuard g]
    case ev of
      VError txt _ ->
        throwError (PEExecutionError (EvalError txt) i)
      EvalValue v -> do
        case v of
          VClosure{} -> do
            pure IPClosure
          VTable tv -> pure (IPTable (_tvName tv))
          VPactValue pv -> do
            pure (IPV pv i)

  interpretExpr purity term = do
    evalEnv <- viewEvalEnv id
    let builtins = rawBuiltinEnv
    let pdb = _eePactDb evalEnv
    let cekEnv = fromPurity $ CEKEnv mempty pdb builtins (_eeDefPactStep evalEnv) False
    Eval.eval cekEnv term >>= \case
      VError txt _ ->
        throwError (PEExecutionError (EvalError txt) (view termInfo term))
      EvalValue v -> do
        case v of
          VClosure{} -> do
            pure IPClosure
          VTable tv -> pure (IPTable (_tvName tv))
          VPactValue pv -> do
            pure (IPV pv (view termInfo term))
    where
    fromPurity env = case purity of
      PSysOnly -> sysOnlyEnv env
      PReadOnly -> readOnlyEnv env
      PImpure -> env
