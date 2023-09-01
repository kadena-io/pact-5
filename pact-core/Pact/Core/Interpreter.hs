module Pact.Core.Interpreter
  ( Interpreter(..)
  , InterpretValue(..)
  )where

import Pact.Core.Type
import Pact.Core.IR.Term
import Pact.Core.Info
import Pact.Core.Names
import Pact.Core.PactValue


newtype Interpreter b s m
  = Interpreter {
    _interpret :: Term Name Type b SpanInfo -> m InterpretValue
  }

data InterpretValue
  = IPV PactValue SpanInfo
  | IPClosure
  | IPTable TableName
  deriving Show
