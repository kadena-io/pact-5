{-# LANGUAGE TemplateHaskell #-}
module Pact.Core.Interpreter
  ( Interpreter(..)
  , InterpretValue(..)
  , enforceKeyset
  )where

import Control.Lens
import Control.Monad.Except
import Data.Map.Strict(Map)
import Data.Set(Set)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Pact.Core.Type
import Pact.Core.IR.Term
import Pact.Core.Info
import Pact.Core.Names
import Pact.Core.PactValue
import Pact.Core.Guards
import Pact.Core.Capabilities
import Pact.Core.Environment
import Pact.Core.Errors

newtype Interpreter b m
  = Interpreter
  { _interpret :: Term Name Type b SpanInfo -> m InterpretValue
  }

data InterpretValue
  = IPV PactValue SpanInfo
  | IPClosure
  | IPTable TableName
  deriving Show

enforceKeyset
  :: (MonadError (PactError i) m)
  => EvalEnv b i
  -> KeySet FullyQualifiedName
  -> m Bool
enforceKeyset ee (KeySet kskeys ksPred) = do
  let sigs = M.filterWithKey matchKey . view eeMsgSigs $ ee
  runPred (M.size sigs)
  where
  matchKey k _ = k `elem` kskeys
  atLeast t m = m >= t
  -- elide pk
  --   | T.length pk < 8 = pk
  --   | otherwise = T.take 8 pk <> "..."
  count = S.size kskeys
  -- failed = "Keyset failure"
  runPred matched =
    case ksPred of
      KeysAll -> run atLeast
      KeysAny -> run (\_ m -> atLeast 1 m)
      Keys2 -> run (\_ m -> atLeast 2 m)
    where
    run p = pure (p count matched)
