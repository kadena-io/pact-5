{-# LANGUAGE PartialTypeSignatures #-}

module Pact.Core.Evaluate where

import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Exception.Safe
import Data.Bifunctor(bimap)
import Data.Default
import Data.Text (Text)
import Data.Map.Strict(Map)

import qualified Data.Set as S

import Pact.Core.Builtin
import Pact.Core.Compile
import Pact.Core.Environment
import Pact.Core.Errors
import Pact.Core.Interpreter
import Pact.Core.IR.Eval.RawBuiltin
import Pact.Core.IR.Eval.Runtime
import Pact.Core.IR.Term
import Pact.Core.Persistence
import Pact.Core.DefPacts.Types
import Pact.Core.Capabilities
import Pact.Core.PactValue
import Pact.Core.Gas
import Pact.Core.Names
import qualified Pact.Core.IR.Eval.CEK as Eval
import qualified Pact.Core.Syntax.Lexer as Lisp
import qualified Pact.Core.Syntax.Parser as Lisp
import qualified Pact.Core.Syntax.ParseTree as Lisp

type EvalInput = Either (Maybe DefPactExec) RawCode

newtype RawCode = RawCode { _rawCode :: Text }
  deriving (Eq, Show)

-- | Results of evaluation.
data EvalResult = EvalResult
  { _erInput :: !EvalInput
    -- ^ compiled user input
  , _erOutput :: ![PactValue]
    -- ^ Output values
  -- , _erLogs :: ![TxLogJson]
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

-- setupEvalEnv
--   :: PactDbEnv e <- here pass pactdb
--   -> Maybe EntityName <- core does not have this. ignore
--   -> ExecutionMode <- we have this
--   -> MsgData <- create at type for this
--   -> RefStore <-  this is called `Loaded` in pact core. See in Pact.Core.Persistence
--   -> GasEnv <- also have this, use constant gas model
--   -> NamespacePolicy <- also have this, as-is
--   -> SPVSupport <- WIP: Ignore for now
--   -> PublicData <- we have this
--   -> ExecutionConfig <- have this as well, except we just pass in the set directly
--   -> IO (EvalEnv e)

-- evalExec :: Interpreter e -> EvalEnv e -> ParsedCode -> IO EvalResult
-- evalExec runner evalEnv ParsedCode {..} = do
--   terms <- throwEither $ compileExps (ParseEnv isNarrowTry) (mkTextInfo _pcCode) _pcExps
--   interpret runner evalEnv (Right terms)
--   where
--     isNarrowTry = not $ S.member FlagDisablePact44 $ _ecFlags $ _eeExecutionConfig evalEnv

-- interpret :: Interpreter e -> EvalEnv e -> EvalInput -> IO EvalResult
-- interpret runner evalEnv terms = do
--   ((rs,logs,txid),state) <-
--     runEval def evalEnv $ evalTerms runner terms
--   milliGas <- readIORef (_eeGas evalEnv)
--   warnings <- readIORef (_eeWarnings evalEnv)
--   let pact48Disabled = views (eeExecutionConfig . ecFlags) (S.member FlagDisablePact48) evalEnv
--       gasLogs = _evalLogGas state
--       pactExec = _evalPactExec state
--       modules = _rsLoadedModules $ _evalRefs state
--       gasUsed = if pact48Disabled then milliGasToGas milliGas else gasRem milliGas
--   -- output uses lenient conversion
--   return $! EvalResult
--     terms
--     (map (elideModRefInfo . toPactValueLenient) rs)
--     logs pactExec gasUsed modules txid gasLogs (_evalEvents state) warnings
--   where
--     -- Round up by 1 if the `MilliGas` amount is in any way fractional.
--     gasRem (MilliGas milliGas) =
--       let (d, r) = milliGas `quotRem` millisPerGas
--       in Gas (if r == 0 then d else d+1)

-- Used to be `evalTerms`
evalWithinTx
  :: Either (Maybe DefPactExec) [Lisp.TopLevel ()]
  -> EvalM RawBuiltin () ([CompileValue ()], [a], Maybe TxId)
  -- ^ TODO: txLog, that `[a]` should be type narrowed but it is blocked on
  -- SQLite merge
evalWithinTx input = withRollback (start runInput >>= end)

  where

    withRollback act =
      act `onException` safeRollback

    safeRollback =
      void (tryAny evalRollbackTx)

    start act = do
      pdb <- viewEvalEnv eePactDb
      mode <- viewEvalEnv eeMode
      -- Todo: commitTx will eventually return these logs
      txid <- liftDbFunction () (_pdbBeginTx pdb mode)
      (,txid) <$> act

    end (rs,txid) = do
      pdb <- viewEvalEnv eePactDb
      -- Todo: commitTx will eventually return these logs
      let logs = []
      _ <- liftDbFunction () (_pdbCommitTx pdb)
      return (rs, logs,txid)

    runInput = case input of
      Right ts -> evaluateTerms ts
      Left pe -> (:[]) <$> resumePact pe

    evalRollbackTx = do
      esCaps .== def
      pdb <- viewEvalEnv eePactDb
      liftDbFunction () (_pdbRollbackTx pdb)

-- | Runs only compilation pipeline
compileOnly :: RawCode -> Either (PactError ()) [Lisp.TopLevel ()]
compileOnly =  bimap void (fmap void) . (Lisp.lexer >=> Lisp.parseProgram) . _rawCode

-- evaluateDefaultState
--   :: EvalEnv RawBuiltin ()
--   -> [Lisp.TopLevel ()]
--   -> IO (Either (PactError ()) [CompileValue ()],
--          EvalState RawBuiltin ())
-- evaluateDefaultState evalEnv source = evaluateTerms evalEnv def source

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
