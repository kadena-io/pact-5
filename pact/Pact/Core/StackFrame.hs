{-# LANGUAGE TypeSynonymInstances #-}
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
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveTraversable #-}


module Pact.Core.StackFrame
 ( StackFrame(..)
 , StackFunctionType(..))
 where

import Control.DeepSeq(NFData)
import GHC.Generics

import Pact.Core.Names
import Pact.Core.PactValue
import Pact.Core.Pretty


data StackFunctionType
  = SFDefun
  | SFDefcap
  | SFDefPact
  deriving (Eq, Show, Enum, Bounded, Generic)

instance NFData StackFunctionType

data StackFrame i
  = StackFrame
  { _sfName :: !FullyQualifiedName
  , _sfArgs :: ![PactValue]
  , _sfFnType :: !StackFunctionType
  , _sfInfo :: !i }
  deriving (Show, Generic, Functor, Foldable, Traversable)

instance NFData i => NFData (StackFrame i)

instance Pretty (StackFrame i) where
  pretty (StackFrame sfn args _ _) =
    parens (pretty sfn <> if null args then mempty else space <> hsep (pretty <$> args))
