{-# LANGUAGE TemplateHaskell #-}
module Pact.Core.Interpreter
  ( Interpreter(..)
  , InterpretValue(..)
  -- , enforceKeyset
  )where

-- import Control.Lens
-- import Control.Monad.Except
-- import qualified Data.Map.Strict as M
-- import qualified Data.Set as S

import Pact.Core.Type
import Pact.Core.IR.Term
import Pact.Core.Info
import Pact.Core.Names
import Pact.Core.PactValue

-- | Our general interpreter abstraction. It allows us to
-- decouple evaluation from
newtype Interpreter b i m
  = Interpreter
  { _interpret :: Term Name Type b i -> m InterpretValue
  }

data InterpretValue
  = IPV PactValue SpanInfo
  | IPClosure
  | IPTable TableName
  deriving Show

