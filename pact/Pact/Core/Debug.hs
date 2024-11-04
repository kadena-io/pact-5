{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}

module Pact.Core.Debug
 ( DebugFlag(..)
 , DebugPrint(..)
 , DebugPrintable(..)
 ) where

import Pact.Core.Type
import Pact.Core.Names
import Pact.Core.Syntax.LexUtils(PosToken)
import Pact.Core.Environment
import qualified Pact.Core.Syntax.ParseTree as Syntax
import qualified Pact.Core.IR.Term as Term
import Pact.Core.Builtin (CoreBuiltin)

data DebugPrint b i term where
  DPLexer :: DebugPrint b i [PosToken]
  DPParser :: DebugPrint b i (Syntax.TopLevel i)
  DPDesugar :: DebugPrint b i (Term.TopLevel Name Type b i)

data DebugFlag
  = DFLexer
  | DFParser
  | DFDesugar
  deriving (Show, Eq, Ord, Enum, Bounded)

class DebugPrintable (e :: RuntimeMode) b | e -> b where
  debugPrint :: DebugPrint b i term -> term -> EvalM e b i ()

instance DebugPrintable 'ExecRuntime CoreBuiltin where
  debugPrint _ _ = pure ()
