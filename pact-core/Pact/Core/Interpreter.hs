{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Pact.Core.Interpreter
  ( Interpreter(..)
  , InterpretValue(..)
  )where

import Pact.Core.Type
import Pact.Core.IR.Term
import Pact.Core.Info
import Pact.Core.Names
import Pact.Core.Guards
import Pact.Core.PactValue
import Pact.Core.Persistence(Purity)


-- | Our general interpreter abstraction. It allows us to
-- decouple evaluation from
data Interpreter b i m
  = Interpreter
  { _interpret :: !(Purity -> Term Name Type b i -> m InterpretValue)
  , _interpretGuard :: !(i -> Guard FullyQualifiedName PactValue -> m InterpretValue)
  }

data InterpretValue
  = IPV PactValue SpanInfo
  | IPClosure
  | IPTable TableName
  deriving Show
