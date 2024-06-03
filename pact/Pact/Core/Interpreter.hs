module Pact.Core.Interpreter
 ( Interpreter(..)
 ) where

import Pact.Core.Names
import Pact.Core.Guards
import Pact.Core.Persistence
import Pact.Core.IR.Term
import Pact.Core.PactValue

-- | Our Interpreter abstraction for
--   working with different pact interpreters.
data Interpreter b i m
  = Interpreter
  { interpretGuard :: !(i -> Guard QualifiedName PactValue -> m PactValue)
  , eval :: !(Purity -> EvalTerm b i -> m PactValue)
  }
