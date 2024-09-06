{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}

module Pact.Core.Evaluate
  ( MsgData(..)
  , RawCode(..)
  , EvalResult(..)
  , Cont(..)
  , Info
  , evalExec
  , evalExecTerm
  , evalContinuation
  , evalGasPayerCap
  , setupEvalEnv
  , interpret
  , compileOnly
  , compileOnlyTerm
  , evaluateDefaultState
  , Eval
  , EvalBuiltinEnv
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
import Data.Set (Set)

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
import Pact.Core.Info
import Pact.Core.Signer
import qualified Pact.Core.IR.Eval.CEK as Eval
import qualified Pact.Core.IR.Eval.Direct.Evaluator as Direct
import qualified Pact.Core.Syntax.Lexer as Lisp
import qualified Pact.Core.Syntax.Parser as Lisp
import qualified Pact.Core.Syntax.ParseTree as Lisp
import Control.Monad.IO.Class

type Eval = EvalM ExecRuntime CoreBuiltin Info

-- Our Builtin environment for evaluation in Chainweb prod
type EvalBuiltinEnv = Eval.CoreBuiltinEnv Info
type PactTxResult a =
  (Either (PactError Info) (a, [TxLog ByteString], Maybe TxId), EvalState CoreBuiltin Info)

evalInterpreter :: Interpreter ExecRuntime CoreBuiltin i
evalInterpreter =
  Interpreter runGuard runTerm resume evalWithCap
  where
  runTerm purity term = Eval.eval purity cekEnv term
  runGuard info g = Eval.interpretGuard info cekEnv g
  resume info defPact = Eval.evalResumePact info cekEnv defPact
  evalWithCap info purity ct term =
    Eval.evalWithinCap info purity cekEnv ct term

cekEnv :: Eval.BuiltinEnv ExecRuntime CoreBuiltin i
cekEnv = coreBuiltinEnv @ExecRuntime

evalDirectInterpreter :: Interpreter ExecRuntime CoreBuiltin i
evalDirectInterpreter =
  Interpreter runGuard runTerm resume evalWithCap
  where
  runTerm purity term = Direct.eval purity env term
  runGuard info g = Direct.interpretGuard info env g
  resume info defPact = Direct.evalResumePact info env defPact
  evalWithCap info purity ct term =
    Direct.evalWithinCap info purity env ct term
  env = Direct.coreBuiltinEnv

-- | Transaction-payload related environment data.
data MsgData = MsgData
  { mdData :: !PactValue
  , mdHash :: !Hash
  , mdSigners :: [Signer]
  , mdVerifiers :: [Verifier ()]
  }

type EvalInput = Either (Maybe DefPactExec) [Lisp.TopLevel Info]

newtype RawCode = RawCode { _rawCode :: Text }
  deriving (Eq, Show)

data Cont = Cont
  { _cPactId :: !DefPactId
  , _cStep :: !Int
  , _cRollback :: !Bool
  , _cProof :: !(Maybe ContProof)
  } deriving (Eq,Show)

-- | Results of evaluation.
data EvalResult = EvalResult
  { _erOutput :: ![CompileValue Info]
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
  } deriving stock (Eq, Show)

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
  let gasEnv = GasEnv
        { _geGasRef = gasRef
        , _geGasLog = Nothing
        , _geGasModel = gasModel'
        }
  pure $ EvalEnv
    { _eeMsgSigs = mkMsgSigs $ mdSigners msgData
    , _eeMsgVerifiers = mkMsgVerifiers $ mdVerifiers msgData
    , _eePactDb = pdb
    , _eeMsgBody = mdData msgData
    , _eeHash = mdHash msgData
    , _eePublicData = pd
    , _eeDefPactStep = Nothing
    , _eeMode = mode
    , _eeFlags = efs
    , _eeNatives = coreBuiltinMap
    , _eeNamespacePolicy = np
    , _eeGasEnv = gasEnv
    , _eeSPVSupport = spv
    , _eeWarnings = Nothing
    }
  where
  mkMsgSigs ss = M.fromList $ map toPair ss
    where
    toPair (Signer _scheme pubK addr capList) =
      (PublicKeyText (fromMaybe pubK addr),S.fromList (_sigCapability <$> capList))
  mkMsgVerifiers vs = M.fromListWith S.union $ map toPair vs
    where
    toPair (Verifier vfn _ caps) = (vfn, S.fromList (_sigCapability <$> caps))

evalExec
  :: ExecutionMode -> PactDb CoreBuiltin Info -> SPVSupport -> GasModel CoreBuiltin -> Set ExecutionFlag -> NamespacePolicy
  -> PublicData -> MsgData
  -> CapState QualifiedName PactValue
  -> [Lisp.TopLevel Info] -> IO (Either (PactError Info) EvalResult)
evalExec execMode db spv gasModel flags nsp publicData msgData capState terms = do
  evalEnv <- setupEvalEnv db execMode msgData gasModel nsp spv publicData flags
  let evalState = def & esCaps .~ capState
  interpret evalEnv evalState (Right terms)

evalExecTerm
  :: ExecutionMode -> PactDb CoreBuiltin Info -> SPVSupport -> GasModel CoreBuiltin -> Set ExecutionFlag -> NamespacePolicy
  -> PublicData -> MsgData
  -> CapState QualifiedName PactValue
  -> Lisp.Expr Info -> IO (Either (PactError Info) EvalResult)
evalExecTerm execMode db spv gasModel flags nsp publicData msgData capState term = do
  evalEnv <- setupEvalEnv db execMode msgData gasModel nsp spv publicData flags
  let evalState = def & esCaps .~ capState
  interpret evalEnv evalState (Right [Lisp.TLTerm term])

evalContinuation
  :: ExecutionMode -> PactDb CoreBuiltin Info -> SPVSupport -> GasModel CoreBuiltin -> Set ExecutionFlag -> NamespacePolicy
  -> PublicData -> MsgData
  -> CapState QualifiedName PactValue
  -> Cont -> IO (Either (PactError Info) EvalResult)
evalContinuation execMode db spv gasModel flags nsp publicData msgData capState cont = do
  evalEnv <- setupEvalEnv db execMode msgData gasModel nsp spv publicData flags
  let evalState = def & esCaps .~ capState
  case _cProof cont of
    Nothing ->
      interpret
        (evalEnv & eeDefPactStep .~ step Nothing)
        evalState
        (Left Nothing)
    Just p -> _spvVerifyContinuation spv p >>= \case
      Left spvErr -> pure $ Left $ PEExecutionError (ContinuationError spvErr) [] def
      Right pe -> do
        interpret
          (evalEnv & eeDefPactStep .~ step (_peYield pe))
          evalState
          (Left $ Just pe)
    where
    step y = Just $ DefPactStep (_cStep cont) (_cRollback cont) (_cPactId cont) y

evalGasPayerCap
  :: CapToken QualifiedName PactValue
  -> PactDb CoreBuiltin Info -> SPVSupport -> GasModel CoreBuiltin -> Set ExecutionFlag -> NamespacePolicy
  -> PublicData -> MsgData
  -> CapState QualifiedName PactValue
  -> Lisp.Expr Info -> IO (Either (PactError Info) EvalResult)
evalGasPayerCap capToken db spv gasModel flags nsp publicData msgData capState body = do
  evalEnv <- setupEvalEnv db Transactional msgData gasModel nsp spv publicData flags
  let evalState = def & esCaps .~ capState
  interpretGasPayerTerm evalEnv evalState capToken body


interpret
  :: EvalEnv CoreBuiltin Info
  -> EvalState CoreBuiltin Info
  -> EvalInput
  -> IO (Either (PactError Info) EvalResult)
interpret evalEnv evalSt evalInput = do
  (result, state) <- evalWithinTx evalInput evalEnv evalSt
  gas <- readIORef (_geGasRef $ _eeGasEnv evalEnv)
  case result of
    Left err -> return $ Left err
    Right (rs, logs, txid) ->
      return $! Right $! EvalResult
        { _erOutput = rs
        , _erLogs = logs
        , _erExec = _esDefPactExec state
        -- Todo: quotrem
        , _erGas = milliGasToGas gas
        , _erLoadedModules = _loModules $ _esLoaded state
        , _erTxId = txid
        , _erLogGas = Nothing
        , _erEvents = reverse $ _esEvents state
        }

interpretGasPayerTerm
  :: EvalEnv CoreBuiltin Info
  -> EvalState CoreBuiltin Info
  -> CapToken QualifiedName PactValue
  -> Lisp.Expr Info
  -> IO (Either (PactError Info) EvalResult)
interpretGasPayerTerm evalEnv evalSt ct term = do
  (result, state) <- evalWithinCap ct term evalEnv evalSt
  gas <- readIORef (_geGasRef $ _eeGasEnv evalEnv)
  case result of
    Left err -> return $ Left err
    Right (_, logs, txid) ->
      return $! Right $! EvalResult
        { _erOutput = []
        , _erLogs = logs
        , _erExec = _esDefPactExec state
        -- todo: quotrem
        , _erGas = milliGasToGas gas
        , _erLoadedModules = _loModules $ _esLoaded state
        , _erTxId = txid
        , _erLogGas = Nothing
        , _erEvents = reverse $ _esEvents state
        }

-- Used to be `evalTerms`
evalWithinTx
  :: EvalInput
  -> EvalEnv CoreBuiltin Info
  -> EvalState CoreBuiltin Info
  -> IO (PactTxResult [CompileValue Info])
evalWithinTx input ee es = evalWithinTx' ee es runInput
  where
  runInput = case input of
    Right ts -> evaluateTerms ts
    Left pe -> (:[]) <$> evalResumePact pe

evalWithinCap
  :: CapToken QualifiedName PactValue
  -> Lisp.Expr Info
  -> EvalEnv CoreBuiltin Info
  -> EvalState CoreBuiltin Info
  -> IO (PactTxResult ())
evalWithinCap ct body ee es =
  evalWithinTx' ee es runInput
  where
  runInput = do
    let info = view Lisp.termInfo body
    (DesugarOutput term' _) <- runDesugarTerm body
    () <$ Eval.evalWithinCap info PImpure cekEnv ct term'


-- | Evaluate some input action within a tx context
evalWithinTx'
  :: EvalEnv CoreBuiltin Info
  -> EvalState CoreBuiltin Info
  -> Eval a
  -> IO (Either (PactError Info) (a, [TxLog ByteString], Maybe TxId), EvalState CoreBuiltin Info)
evalWithinTx' ee es action = do
      txid <-_pdbBeginTx pdb mode
      (result, newState) <- runEvalM (ExecEnv ee) es runAction
      -- maybe might want to decode using serialisepact
      return ((\(res, logs) -> (res, logs, txid)) <$> result, newState)
    where
    pdb = view eePactDb ee
    mode = view eeMode ee
    -- Note: runAction catches all synchronous exceptions as well as
    -- thrown PactErrors, and runs rollback in the case of an exception
    -- or error.
    runAction =
      let act = tryAny action >>= \case
            Left _ -> throwError $ PEExecutionError UnknownException [] def
            Right v -> pure v
      in tryError act >>= \case
        Left err -> liftIO (_pdbRollbackTx pdb) *> throwError err
        Right v -> (v,) <$> liftIO (_pdbCommitTx pdb)

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
