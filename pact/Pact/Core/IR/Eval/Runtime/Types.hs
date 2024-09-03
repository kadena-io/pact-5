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
  ( TableValue(..)
  , ErrorState(..)
  , EvalCapType(..)) where



import Data.List.NonEmpty(NonEmpty)
import GHC.Generics
import Control.DeepSeq


import Pact.Core.Names

import Pact.Core.PactValue
import Pact.Core.Hash
import Pact.Core.Type
import Pact.Core.Capabilities
import Pact.Core.Environment


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

data EvalCapType
  = NormalCapEval
  | TestCapEval
  deriving (Show, Eq, Enum, Bounded)
