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
{-# LANGUAGE DeriveGeneric #-}

module Pact.Core.IR.Eval.Direct.Types where

import Control.Lens
import GHC.Generics
import Control.DeepSeq
import Data.Text(Text)
import Data.Decimal
import Data.List.NonEmpty(NonEmpty(..))
import Data.RAList(RAList)
import Data.Map.Strict(Map)
import Data.Vector(Vector)
import Pact.Time(UTCTime)
import qualified Data.RAList as RAList
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.List.NonEmpty as NE
import qualified Data.Kind as K


import Pact.Core.IR.Eval.Runtime.Utils
import Pact.Core.IR.Term
import Pact.Core.Names
import Pact.Core.Environment
import Pact.Core.Type
import Pact.Core.Errors
import Pact.Core.PactValue
import Pact.Core.Hash
import Pact.Core.Persistence
import Pact.Core.Guards
import Pact.Core.Capabilities
import Pact.Core.DefPacts.Types
import Pact.Core.Literal
import Pact.Core.ModRefs
import Pact.Core.Builtin

data ClosureType
  = NullaryClosure
  | ArgClosure !(NonEmpty (Arg Type))
  deriving (Show, Generic)

instance NFData ClosureType

data Closure b i m
  = Closure
  { _cloFnName :: !Text
  , _cloModName :: !ModuleName
  , _cloTypes :: !ClosureType
  , _cloArity :: !Int
  , _cloTerm :: !(EvalTerm b i)
  , _cloRType :: !(Maybe Type)
  , _cloEnv :: !(DirectEnv b i m)
  , _cloInfo :: i
  } deriving (Show, Generic)

instance (NFData b, NFData i) => NFData (Closure b i m)

-- | A closure coming from a lambda application with its accompanying environment capturing args,
-- but is not partially applied
data LamClosure (b :: K.Type) (i :: K.Type) (m :: K.Type -> K.Type)
  = LamClosure
  { _lcloTypes :: !ClosureType
  , _lcloArity :: !Int
  , _lcloTerm :: !(EvalTerm b i)
  , _lcloRType :: !(Maybe Type)
  , _lcloEnv :: !(DirectEnv b i m)
  , _lcloInfo :: i
  } deriving (Show, Generic)

instance (NFData b, NFData i) => NFData (LamClosure b i m)

-- | A partially applied function because we don't allow
-- them to be applied at the lhs of an app since pact historically hasn't had partial closures.
-- This is a bit annoying to deal with but helps preserve semantics
data PartialClosure (b :: K.Type) (i :: K.Type) (m :: K.Type -> K.Type)
  = PartialClosure
  { _pcloFrame :: !(Maybe StackFrame)
  , _pcloTypes :: !(NonEmpty (Arg Type))
  , _pcloArity :: !Int
  , _pcloTerm :: !(EvalTerm b i)
  , _pcloRType :: !(Maybe Type)
  , _pcloEnv :: !(DirectEnv b i m)
  , _pcloInfo :: i
  } deriving (Show, Generic)

instance (NFData b, NFData i) => NFData (PartialClosure b i m)

data DefPactClosure (b :: K.Type) (i :: K.Type) (m :: K.Type -> K.Type)
  = DefPactClosure
  { _pactcloFQN :: !FullyQualifiedName
  , _pactcloTypes :: !ClosureType
  , _pactcloArity :: !Int
  , _pactEnv :: !(DirectEnv b i m)
  , _pactcloInfo :: i
  } deriving (Show, Generic)

instance (NFData b, NFData i) => NFData (DefPactClosure b i m)

data CapTokenClosure i
  = CapTokenClosure
  { _ctcCapName :: !FullyQualifiedName
  , _ctcTypes :: [Maybe Type]
  , _ctcArity :: Int
  , _ctcInfo :: i
  } deriving (Eq, Show, Generic)

instance NFData i => NFData (CapTokenClosure i)

-- | A partially applied native because we don't allow
-- them to be applied at the lhs of an app since pact historically hasn't had partial closures.
-- This is a bit annoying to deal with but helps preserve semantics
data PartialNativeFn b i m
  = PartialNativeFn
  { _pNative :: !b
  , _pNativeEnv :: !(DirectEnv b i m)
  , _pNativeFn :: !(NativeFunction b i m)
  , _pNativeArity :: {-# UNPACK #-} !Int
  , _pNativeAppliedArgs :: ![EvalValue b i m]
  , _pNativeLoc :: i
  } deriving (Generic)

data CanApply (b :: K.Type) (i :: K.Type) (m :: K.Type -> K.Type)
  = C {-# UNPACK #-} !(Closure b i m)
  | LC {-# UNPACK #-} !(LamClosure b i m)
  | PC {-# UNPACK #-} !(PartialClosure b i m)
  | N {-# UNPACK #-} !(NativeFn b i m)
  | PN {-# UNPACK #-} !(PartialNativeFn b i m)
  | DPC {-# UNPACK #-} !(DefPactClosure b i m)
  | CT {-# UNPACK #-} !(CapTokenClosure i)
  deriving (Show, Generic)


instance (Show i, Show b) => Show (NativeFn b i m) where
  show (NativeFn b _ _ arity _) = unwords
    ["(NativeFn"
    , show b
    , "#fn"
    , show arity
    , ")"
    ]

instance (Show i, Show b) => Show (PartialNativeFn b i m) where
  show (PartialNativeFn b _ _ arity _ _) = unwords
    ["(NativeFn"
    , show b
    , "#fn"
    , show arity
    , ")"
    ]

data TableValue
  = TableValue
  { _tvName :: !TableName
  , _tvHash :: !ModuleHash
  , _tvSchema :: !Schema
  } deriving (Show, Generic)


instance NFData TableValue

-- | The type of our semantic runtime values
data EvalValue b i m
  = VPactValue !PactValue
  -- ^ PactValue(s), which contain no terms
  | VTable !TableValue
  -- ^ Table references, which despite being a syntactic
  -- value with
  | VClosure  !(CanApply b i m)
  -- ^ Closures, which may contain terms
  deriving (Generic)

instance (NFData b, NFData i) => NFData (EvalValue b i m)

instance Show (EvalValue b i m) where
  show = \case
    VPactValue pv -> show pv
    VTable vt -> "table" <> show (_tvName vt)
    VClosure _ -> "closure<>"

-- | Locally bound variables
-- type DirectEnv b i m = RAList (EvalValue b i m)

data DirectEnv b i m
  = DirectEnv
  { _ceLocal :: RAList (EvalValue b i m)
  , _cePactDb :: PactDb b i
  , _ceBuiltins :: BuiltinEnv b i m
  , _ceDefPactStep :: Maybe DefPactStep
  , _ceInCap :: Bool }
  deriving (Generic)

instance (NFData b, NFData i) => NFData (DirectEnv b i m)


instance (Show i, Show b) => Show (DirectEnv b i m) where
  show (DirectEnv e _ _ _ _) = show e

type NativeFunction (b :: K.Type) (i :: K.Type) (m :: K.Type -> K.Type)
  = i -> b -> DirectEnv b i m -> [EvalValue b i m] -> m (EvalValue b i m)

-- | List of builtins
type BuiltinEnv b i m
  = i -> b -> DirectEnv b i m -> NativeFn b i m

data NativeFn (b :: K.Type) (i :: K.Type) (m :: K.Type -> K.Type)
  = NativeFn
  { _native :: !b
  , _nativeEnv :: !(DirectEnv b i m)
  , _nativeFn :: !(NativeFunction b i m)
  , _nativeArity :: {-# UNPACK #-} !Int
  , _nativeLoc :: i
  } deriving (Generic)

pattern VLiteral :: Literal -> EvalValue b i m
pattern VLiteral lit = VPactValue (PLiteral lit)

pattern VString :: Text -> EvalValue b i m
pattern VString txt = VLiteral (LString txt)

pattern VInteger :: Integer -> EvalValue b i m
pattern VInteger txt = VLiteral (LInteger txt)

pattern VUnit :: EvalValue b i m
pattern VUnit = VLiteral LUnit

pattern VBool :: Bool -> EvalValue b i m
pattern VBool b = VLiteral (LBool b)

pattern VDecimal :: Decimal -> EvalValue b i m
pattern VDecimal d = VLiteral (LDecimal d)

pattern VGuard :: Guard QualifiedName PactValue -> EvalValue b i m
pattern VGuard g = VPactValue (PGuard g)

pattern VList :: Vector PactValue -> EvalValue b i m
pattern VList p = VPactValue (PList p)

pattern VTime :: UTCTime -> EvalValue b i m
pattern VTime p = VPactValue (PTime p)

pattern VObject :: Map Field PactValue -> EvalValue b i m
pattern VObject o = VPactValue (PObject o)

pattern VModRef :: ModRef -> EvalValue b i m
pattern VModRef mn = VPactValue (PModRef mn)

pattern VCapToken :: CapToken FullyQualifiedName PactValue -> EvalValue b i m
pattern VCapToken ct = VPactValue (PCapToken ct)

pattern VNative :: NativeFn b i m -> EvalValue b i m
pattern VNative clo = VClosure (N clo)

pattern VPartialNative :: PartialNativeFn b i m -> EvalValue b i m
pattern VPartialNative clo = VClosure (PN clo)

pattern VDefClosure :: Closure b i m -> EvalValue b i m
pattern VDefClosure clo = VClosure (C clo)

pattern VLamClosure :: LamClosure b i m -> EvalValue b i m
pattern VLamClosure clo = VClosure (LC clo)

pattern VPartialClosure :: PartialClosure b i m -> EvalValue b i m
pattern VPartialClosure clo = VClosure (PC clo)

pattern VDefPactClosure :: DefPactClosure b i m -> EvalValue b i m
pattern VDefPactClosure clo = VClosure (DPC clo)

-- | What to do post-cap evaluation: do we pop the cap from the stack,
-- or compose it within the capset
data CapPopState
  = PopCapComposed
  | PopCapInvoke
  deriving (Eq, Show, Generic)

data EvalCapType
  = NormalCapEval
  | TestCapEval
  deriving (Show, Eq, Enum, Bounded)

instance (NFData b, NFData i) => NFData (CanApply b i m)
instance (NFData b, NFData i) => NFData (NativeFn b i m)
instance (NFData b, NFData i) => NFData (PartialNativeFn b i m)

makeLenses ''DirectEnv
