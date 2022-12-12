module Pact.Core.Debug
 ( DebugFlag(..)
 , PhaseDebug(..)
 ) where

import Pact.Core.Pretty

data DebugFlag
  = DebugLexer
  | DebugParser
  | DebugDesugar
  | DebugTypechecker
  | DebugTypecheckerType
  | DebugSpecializer
  | DebugUntyped
  deriving (Show, Eq, Ord, Enum, Bounded)

class Monad m => PhaseDebug m where
  debugPrint :: Pretty a => DebugFlag -> a -> m ()
