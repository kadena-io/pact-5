{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE InstanceSigs #-}

module Pact.Core.IR.Eval.Runtime.Types
 ( EvalM(..)
 , runEvalM
 , EvalState(..)
 , esStack
 , esCaps, esEvents
 , csModuleAdmin
 , esLoaded
 , CapState(..)
 , csSlots, csManaged, csCapsBeingEvaluated
 , ManagedCap(..)
 , mcCap, mcManaged, mcOriginalCap
 , ManagedCapType(..)
 , StackFrame(..)
 , TableValue(..)
 , ErrorState(..)
 , Eval
 ) where

import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Except
import Data.List.NonEmpty(NonEmpty)
import GHC.Generics
import Control.DeepSeq


import Pact.Core.Names

import Pact.Core.PactValue
import Pact.Core.Hash
import Pact.Core.Type
import Pact.Core.Capabilities
import Pact.Core.Environment
import Pact.Core.Debug
import Pact.Core.Errors
import Pact.Core.Builtin



type Eval = EvalM CoreBuiltin ()

data TableValue
  = TableValue
  { _tvName :: !TableName
  , _tvHash :: !ModuleHash
  , _tvSchema :: !Schema
  } deriving (Show, Generic)

instance NFData TableValue

-- | State to preserve in the error handler
data ErrorState i
  = ErrorState (CapState QualifiedName PactValue) [StackFrame i] (NonEmpty RecursionCheck)
  deriving (Show, Generic)

instance NFData i => NFData (ErrorState i)

-- Todo: are we going to inject state as the reader monad here?
newtype EvalM b i a =
  EvalT (ReaderT (EvalEnv b i) (ExceptT (PactError i) (StateT (EvalState b i) IO)) a)
  deriving
    ( Functor, Applicative, Monad
    , MonadIO
    , MonadThrow
    , MonadCatch
    , MonadMask
    , MonadReader (EvalEnv b i)
    , MonadError (PactError i))
  via (ReaderT (EvalEnv b i) (ExceptT (PactError i) (StateT (EvalState b i) IO)))

instance PhaseDebug b i (EvalM b i) where
  debugPrint _ _ = pure ()

instance MonadEvalEnv b i (EvalM b i) where
  readEnv = EvalT ask

instance MonadEvalState b i (EvalM b i) where
  getEvalState = EvalT get
  putEvalState p = EvalT (put p)
  modifyEvalState f = EvalT (modify' f)


runEvalM
  :: EvalEnv b i
  -> EvalState b i
  -> EvalM b i a
  -> IO (Either (PactError i) a, EvalState b i)
runEvalM env st (EvalT action) =
  runStateT (runExceptT (runReaderT action env)) st
