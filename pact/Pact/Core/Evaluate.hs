{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Pact.Core.Evaluate
  ( MsgData(..)
  , RawCode(..)
  , EvalResult(..)
  , initMsgData
  , evalExec
  , setupEvalEnv
  , interpret
  , compileOnly
  , compileOnlyTerm
  , evaluateDefaultState
  , builtinEnv
  , Eval
  , EvalBuiltinEnv
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
import Pact.Core.Names
import Pact.Core.Guards
import Pact.Core.Namespace
import qualified Pact.Core.IR.Eval.CEK as Eval
import qualified Pact.Core.Syntax.Lexer as Lisp
import qualified Pact.Core.Syntax.Parser as Lisp
import qualified Pact.Core.Syntax.ParseTree as Lisp
import qualified Pact.Core.IR.Eval.Runtime.Types as Eval

-- Our Builtin environment for evaluation in Chainweb prod
type EvalBuiltinEnv = CoreBuiltinEnv

-- | Transaction-payload related environment data.
data MsgData = MsgData
  { mdData :: !PactValue
  , mdStep :: !(Maybe DefPactStep)
  , mdHash :: !Hash
  , mdSigners :: [Signer QualifiedName PactValue]
  }

initMsgData :: Hash -> MsgData
initMsgData h = MsgData (PObject mempty) def h mempty

builtinEnv :: EvalBuiltinEnv
builtinEnv = coreBuiltinEnv @Eval.CEKBigStep

type EvalInput = Either (Maybe DefPactExec) [Lisp.TopLevel ()]

newtype RawCode = RawCode { _rawCode :: Text }
  deriving (Eq, Show)

-- | Results of evaluation.
data EvalResult = EvalResult
  { _erInput :: !EvalInput
    -- ^ compiled user input
  , _erOutput :: ![CompileValue ()]
    -- ^ Output values
  , _erLogs :: ![TxLog ByteString]
    -- ^ Transaction logs
  , _erExec :: !(Maybe DefPactExec)
    -- ^ Result of defpact execution if any
  , _erGas :: Gas
    -- ^ Gas consumed/charged
  , _erLoadedModules :: Map ModuleName (ModuleData CoreBuiltin ())
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
  :: PactDb CoreBuiltin ()
  -> ExecutionMode -- <- we have this
  -> MsgData -- <- create at type for this
  -- -> GasEnv -- <- also have this, use constant gas model
  -> NamespacePolicy -- <- also have this, as-is
  -- -> SPVSupport -- <- WIP: Ignore for now
  -> PublicData -- <- we have this
  -> S.Set ExecutionFlag
  -> IO (EvalEnv CoreBuiltin ())
setupEvalEnv pdb mode msgData np pd efs = do
  gasRef <- newIORef mempty
  pure $ EvalEnv
    { _eeMsgSigs = mkMsgSigs $ mdSigners msgData
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
    , _eeGasModel = freeGasModel
    }
  where
  mkMsgSigs ss = M.fromList $ map toPair ss
  toPair (Signer _scheme pubK addr capList) = (pk,S.fromList capList)
    where
    pk = PublicKeyText $ fromMaybe pubK addr

evalExec :: EvalEnv CoreBuiltin () -> RawCode -> IO (Either (PactError ()) EvalResult)
evalExec evalEnv rc = do
  terms <- either throwM return $ compileOnly rc
  either throwError return <$> interpret evalEnv (Right terms)

interpret :: EvalEnv CoreBuiltin () -> EvalInput -> IO (Either (PactError ()) EvalResult)
interpret evalEnv evalInput = do
  (result, state) <- runEvalM evalEnv def $ evalWithinTx evalInput
  case result of
    Left err -> return $ Left err
    Right (rs, logs, txid) ->
      return $! Right $! EvalResult
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
  -> EvalM CoreBuiltin () ([CompileValue ()], [TxLog ByteString], Maybe TxId)
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

-- | Runs only compilation pipeline for a single term
compileOnlyTerm :: RawCode -> Either (PactError ()) (Lisp.Expr ())
compileOnlyTerm =
  bimap void void . (Lisp.lexer >=> Lisp.parseExpr) . _rawCode


resumePact
  :: Maybe DefPactExec
  -> Eval (CompileValue ())
resumePact mdp =
  (`InterpretValue` ()) <$> Eval.evalResumePact () builtinEnv mdp

-- | Compiles and evaluates the code
evaluateDefaultState :: RawCode -> Eval [CompileValue ()]
evaluateDefaultState = either throwError evaluateTerms . compileOnly

evaluateTerms
  :: [Lisp.TopLevel ()]
  -> Eval [CompileValue ()]
evaluateTerms tls = do
  traverse (interpretTopLevel builtinEnv) tls
