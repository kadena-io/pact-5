{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Pact.Core.Debug
 ( DebugFlag(..)
 , PhaseDebug(..)
 , DebugPrint(..)
 ) where

import Pact.Core.Type
import Pact.Core.Names
import Pact.Core.Syntax.LexUtils(PosToken)
import qualified Pact.Core.Syntax.ParseTree as Syntax
import qualified Pact.Core.IR.Term as Term

data DebugPrint b i term where
  DPLexer :: DebugPrint b i [PosToken]
  DPParser :: DebugPrint b i (Syntax.TopLevel i)
  DPDesugar :: DebugPrint b i (Term.TopLevel Name Type b i)

data DebugFlag
  = DFLexer
  | DFParser
  | DFDesugar
  deriving (Show, Eq, Ord, Enum, Bounded)

class Monad m => PhaseDebug b i m where
  debugPrint :: DebugPrint b i term -> term -> m ()
