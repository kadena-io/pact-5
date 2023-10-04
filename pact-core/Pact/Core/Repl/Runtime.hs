{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}


module Pact.Core.Repl.Runtime where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Catch
import Data.Default
import Data.IORef
import Data.Text(Text)

import Pact.Core.Gas
import Pact.Core.Errors

import Pact.Core.IR.Term
import Pact.Core.IR.Eval.Runtime
import Pact.Core.IR.Eval.CEK
import Pact.Core.Repl.Utils
import Pact.Core.Environment

data ReplEvalEnv b i
  = ReplEvalEnv
  { _reGas :: IORef Gas
  , _reGasLog :: IORef (Maybe [(Text, Gas)])
  , _reBuiltins :: BuiltinEnv b i (ReplEvalM b i)
  }

data ReplEvalState b i
  = ReplEvalState
  { _reEnv :: EvalEnv b i
  , _reState :: EvalState b i
  , _reSource :: SourceCode
  }

-- Todo: are we going to inject state as the reader monad here?
newtype ReplEvalM b i a =
  ReplEvalM (ExceptT (PactError i) (StateT (ReplEvalState b i) (ReaderT (ReplEvalEnv b i) IO)) a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (ReplEvalEnv b i)
    , MonadState (ReplEvalState b i)
    , MonadError (PactError i)
    , MonadIO
    , MonadThrow
    , MonadCatch)
  via (ExceptT (PactError i) (StateT (ReplEvalState b i) (ReaderT (ReplEvalEnv b i) IO)))

makeLenses ''ReplEvalEnv
makeLenses ''ReplEvalState

instance MonadEvalEnv b i (ReplEvalM b i) where
  readEnv = use reEnv

instance MonadGas (ReplEvalM b i) where
  logGas msg g = do
    r <- view reGasLog
    liftIO $ modifyIORef' r (fmap ((msg, g):))
  chargeGas g = do
    r <- view reGas
    liftIO (modifyIORef' r (<> g))

instance MonadEvalState b i (ReplEvalM b i) where
  getEvalState = use reState
  modifyEvalState f =
    reState %= f
  putEvalState s =
    reState .= s



runReplEvalM
  :: ReplEvalEnv b i
  -> ReplEvalState b i
  -> ReplEvalM b i a
  -> IO (Either (PactError i) a, ReplEvalState b i)
runReplEvalM env st (ReplEvalM action) = runReaderT (runStateT (runExceptT action) st) env

runReplCEK
  :: (Default i)
  => ReplEvalEnv b i
  -> ReplEvalState b i
  -> EvalTerm b i
  -> IO (Either (PactError i) (EvalResult b i (ReplEvalM b i)), ReplEvalState b i)
runReplCEK env st term =
  runReplEvalM env st (eval (CEKEnv mempty (view (reEnv . eePactDb) st) (_reBuiltins env)) term)
