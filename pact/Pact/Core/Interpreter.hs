module Pact.Core.Interpreter
 ( Interpreter(..)
 ) where

import Pact.Core.Names
import Pact.Core.Guards
import Pact.Core.Persistence
import Pact.Core.IR.Term
import Pact.Core.PactValue
import Pact.Core.Environment
import Pact.Core.DefPacts.Types
import Pact.Core.Capabilities

-- | Our Interpreter abstraction for
--   working with different pact interpreters.
data Interpreter e b i
  = Interpreter
  { interpretGuard :: !(i -> Guard QualifiedName PactValue -> EvalM e b i PactValue)
  , eval :: !(Purity -> EvalTerm b i -> EvalM e b i PactValue)
  , resumePact :: !(i -> Maybe DefPactExec -> EvalM e b i PactValue)
  , evalWithCapability :: !(i -> Purity -> CapToken QualifiedName PactValue -> EvalTerm b i -> EvalM e b i PactValue)
  }
