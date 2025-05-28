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
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveTraversable #-}

module Pact.Core.IR.Eval.CEK.Types
 ( CEKTLEnv
 , CEKEnv(..)
 , ceLocal
 , cePactDb
 , ceBuiltins
 , ceDefPactStep
 , ceInCap
 , ceReentrant
 , EvalEnv(..)
 , NativeFunction
 , BuiltinEnv
 , NativeFn(..)
 , EvalM(..)
 , runEvalM
 , CEKValue(..)
 , Cont(..)
 , CEKErrorHandler(..)
 , CondCont(..)
 , Closure(..)
 , EvalResult(..)
--  , EvalTEnv(..)
--  , emGas, emGasLog, emRuntimeEnv
 , EvalState(..)
 , esStack
 , esCaps, esEvents
 , csModuleAdmin
 , esLoaded
 , pattern VLiteral
 , pattern VGuard
 , pattern VList
 , pattern VModRef
 , pattern VString
 , pattern VInteger
 , pattern VDecimal
 , pattern VUnit
 , pattern VBool
 , pattern VObject
 , pattern VDefClosure
 , pattern VLamClosure
 , pattern VPartialClosure
 , pattern VDefPactClosure
 , pattern VNative
 , pattern VPartialNative
 , pattern VCapToken
 , pattern VTime
 , pattern VTable
 , CapCont(..)
 , CapState(..)
 , csSlots, csManaged
 , ManagedCap(..)
 , mcCap, mcManaged, mcOriginalCap
 , ManagedCapType(..)
 , CapPopState(..)
 , LamClosure(..)
 , PartialNativeFn(..)
 , PartialClosure(..)
 , CapTokenClosure(..)
 , CanApply(..)
 , StackFrame(..)
 -- defpact
 , DefPactClosure(..)
 , TableValue(..)
 , ClosureType(..)
 , ErrorState(..)
 , BuiltinCont(..)
 , ContType(..)
 , Eval
 , CoreTerm
 , CoreCEKCont
 , CoreCEKHandler
 , CoreCEKEnv
 , CoreBuiltinEnv
 , CoreCEKValue
 , CoreEvalResult
 , CapBodyState(..)
 ) where

import Control.Lens
import Data.List.NonEmpty(NonEmpty)
import Data.Text(Text)
import Data.Map.Strict(Map)
import Data.Decimal(Decimal)
import Data.Vector(Vector)
import Data.RAList(RAList)
import Data.Set(Set)
import Pact.Time(UTCTime)
import GHC.Generics
import Control.DeepSeq

import qualified Data.Kind as K

import Pact.Core.Names
import Pact.Core.Guards
import Pact.Core.Pretty(Pretty(..))
import Pact.Core.PactValue
import Pact.Core.IR.Term
import Pact.Core.Literal
import Pact.Core.Type
import Pact.Core.Persistence
import Pact.Core.ModRefs
import Pact.Core.Capabilities
import Pact.Core.Environment
import Pact.Core.DefPacts.Types
import Pact.Core.Namespace
import Pact.Core.Builtin
import Pact.Core.Errors
import Pact.Core.IR.Eval.Runtime.Types

import qualified Pact.Core.Pretty as P


-- data CEKReturn e b i
--   = CEKEvaluateTerm (Cont e CEKSmallStep b i) (CEKErrorHandler e CEKSmallStep b i) (CEKEnv e CEKSmallStep b i) (EvalTerm b i)
--   | CEKReturn (Cont e CEKSmallStep b i) (CEKErrorHandler e CEKSmallStep b i) (EvalResult e CEKSmallStep b i)
--   deriving (Show, Generic)

-- instance (NFData b, NFData i) => NFData (CEKReturn e b i)

-- | The top level env map
type CEKTLEnv b i = Map FullyQualifiedName (EvalDef b i)

-- | Locally bound variables
data CEKEnv (e :: RuntimeMode) (b :: K.Type) (i :: K.Type)
  = CEKEnv
  { _ceLocal :: RAList (CEKValue e b i)
  , _cePactDb :: PactDb b i
  , _ceBuiltins :: BuiltinEnv e b i
  , _ceDefPactStep :: Maybe DefPactStep
  , _ceReentrant :: Set ModuleName
  , _ceInCap :: Bool }
  deriving (Generic)

instance (NFData b, NFData i) => NFData (CEKEnv e b i)

instance (Show i, Show b) => Show (CEKEnv e b i) where
  show (CEKEnv e _ _ _ _ _) = show e

-- | List of builtins
type BuiltinEnv (e :: RuntimeMode) (b :: K.Type) (i :: K.Type)
  = i -> b -> CEKEnv e b i -> NativeFn e b i


data ClosureType i
  = NullaryClosure
  | ArgClosure !(NonEmpty (Arg Type i))
  deriving (Show, Functor, Generic)

instance NFData i => NFData (ClosureType i)

data Closure (e :: RuntimeMode) (b :: K.Type) (i :: K.Type)
  = Closure
  { _cloFqName :: !FullyQualifiedName
  , _cloTypes :: !(ClosureType i)
  , _cloArity :: !Int
  , _cloTerm :: !(EvalTerm b i)
  , _cloRType :: !(Maybe Type)
  , _cloEnv :: !(CEKEnv e b i)
  , _cloInfo :: i
  } deriving (Show, Generic)

instance (NFData b, NFData i) => NFData (Closure e b i)

-- | A closure coming from a lambda application with its accompanying environment capturing args,
-- but is not partially applied
data LamClosure (e :: RuntimeMode) (b :: K.Type) (i :: K.Type)
  = LamClosure
  { _lcloTypes :: !(ClosureType i)
  , _lcloArity :: !Int
  , _lcloTerm :: !(EvalTerm b i)
  , _lcloRType :: !(Maybe Type)
  , _lcloEnv :: !(CEKEnv e b i)
  , _lcloInfo :: i
  } deriving (Show, Generic)

instance (NFData b, NFData i) => NFData (LamClosure e b i)

-- | A partially applied function because we don't allow
-- them to be applied at the lhs of an app since pact historically hasn't had partial closures.
-- This is a bit annoying to deal with but helps preserve semantics
data PartialClosure (e :: RuntimeMode) (b :: K.Type) (i :: K.Type)
  = PartialClosure
  { _pcloFrame :: !(Maybe (StackFrame i))
  , _pcloTypes :: !(NonEmpty (Arg Type i))
  , _pcloNArgs :: !Int
  , _pcloArity :: !Int
  , _pcloTerm :: !(EvalTerm b i)
  , _pcloRType :: !(Maybe Type)
  , _pcloEnv :: !(CEKEnv e b i)
  , _pcloInfo :: i
  } deriving (Show, Generic)

instance (NFData b, NFData i) => NFData (PartialClosure e b i)

data DefPactClosure (e :: RuntimeMode) (b :: K.Type) (i :: K.Type)
  = DefPactClosure
  { _pactcloFQN :: !FullyQualifiedName
  , _pactcloTypes :: !(ClosureType i)
  , _pactcloArity :: !Int
  , _pactEnv :: !(CEKEnv e b i)
  , _pactcloInfo :: i
  } deriving (Show, Generic)

instance (NFData b, NFData i) => NFData (DefPactClosure e b i)

data CapTokenClosure i
  = CapTokenClosure
  { _ctcCapName :: !FullyQualifiedName
  , _ctcTypes :: [Maybe Type]
  , _ctcArity :: Int
  , _ctcInfo :: i
  } deriving (Eq, Show, Generic)

instance NFData i => NFData (CapTokenClosure i)

data CanApply (e :: RuntimeMode) (b :: K.Type) (i :: K.Type)
  = C !(Closure e b i)
  | N !(NativeFn e b i)
  | CT !(CapTokenClosure i)
  | LC !(LamClosure e b i)
  | PC !(PartialClosure e b i)
  | PN !(PartialNativeFn e b i)
  | DPC !(DefPactClosure e b i)
  deriving (Show, Generic)

instance (NFData b, NFData i) => NFData (CanApply e b i)


-- | The type of our semantic runtime values
data CEKValue (e :: RuntimeMode) (b :: K.Type) (i :: K.Type)
  = VPactValue !PactValue
  -- ^ PactValue(s), which contain no terms
  -- value with
  | VClosure  !(CanApply e b i)
  -- ^ Closures, which may contain terms
  deriving (Generic)

instance (NFData b, NFData i) => NFData (CEKValue e b i)

instance Show (CEKValue e b i) where
  show = \case
    VPactValue pv -> show pv
    VClosure _ -> "closure<>"

pattern VLiteral :: Literal -> CEKValue e b i
pattern VLiteral lit = VPactValue (PLiteral lit)

pattern VTable :: TableValue -> CEKValue e b i
pattern VTable tv = VPactValue (PTable tv)

pattern VString :: Text -> CEKValue e b i
pattern VString txt = VLiteral (LString txt)

pattern VInteger :: Integer -> CEKValue e b i
pattern VInteger txt = VLiteral (LInteger txt)

pattern VUnit :: CEKValue e b i
pattern VUnit = VLiteral LUnit

pattern VBool :: Bool -> CEKValue e b i
pattern VBool b = VLiteral (LBool b)

pattern VDecimal :: Decimal -> CEKValue e b i
pattern VDecimal d = VLiteral (LDecimal d)

pattern VGuard :: Guard QualifiedName PactValue -> CEKValue e b i
pattern VGuard g = VPactValue (PGuard g)

pattern VList :: Vector PactValue -> CEKValue e b i
pattern VList p = VPactValue (PList p)

pattern VTime :: UTCTime -> CEKValue e b i
pattern VTime p = VPactValue (PTime p)

pattern VObject :: Map Field PactValue -> CEKValue e b i
pattern VObject o = VPactValue (PObject o)

pattern VModRef :: ModRef -> CEKValue e b i
pattern VModRef mn = VPactValue (PModRef mn)

pattern VCapToken :: CapToken FullyQualifiedName PactValue -> CEKValue e b i
pattern VCapToken ct = VPactValue (PCapToken ct)

pattern VNative :: NativeFn e b i -> CEKValue e b i
pattern VNative clo = VClosure (N clo)

pattern VPartialNative :: PartialNativeFn e b i -> CEKValue e b i
pattern VPartialNative clo = VClosure (PN clo)

pattern VDefClosure :: Closure e b i -> CEKValue e b i
pattern VDefClosure clo = VClosure (C clo)

pattern VLamClosure :: LamClosure e b i -> CEKValue e b i
pattern VLamClosure clo = VClosure (LC clo)

pattern VPartialClosure :: PartialClosure e b i -> CEKValue e b i
pattern VPartialClosure clo = VClosure (PC clo)

pattern VDefPactClosure :: DefPactClosure e b i -> CEKValue e b i
pattern VDefPactClosure clo = VClosure (DPC clo)

-- | Result of an evaluation step, either a CEK value or an error.
data EvalResult (e :: RuntimeMode) (b :: K.Type) (i :: K.Type)
  = EvalValue (CEKValue e b i)
  | VError [StackFrame i] UserRecoverableError i
  deriving (Show, Generic)

instance (NFData b, NFData i) => NFData (EvalResult e b i)




type NativeFunction (e :: RuntimeMode) (b :: K.Type) (i :: K.Type)
  = i -> b -> Cont e b i -> CEKErrorHandler e b i -> CEKEnv e b i -> [CEKValue e b i] -> EvalM e b i (EvalResult e b i)

data NativeFn (e :: RuntimeMode) (b :: K.Type) (i :: K.Type)
  = NativeFn
  { _native :: !b
  , _nativeEnv :: !(CEKEnv e b i)
  , _nativeFn :: !(NativeFunction e b i)
  , _nativeArity :: !Int
  , _nativeLoc :: !i
  } deriving (Generic)

instance (NFData b, NFData i) => NFData (NativeFn e b i)

-- | A partially applied native because we don't allow
-- them to be applied at the lhs of an app since pact historically hasn't had partial closures.
-- This is a bit annoying to deal with but helps preserve semantics
data PartialNativeFn (e :: RuntimeMode) (b :: K.Type) (i :: K.Type)
  = PartialNativeFn
  { _pNative :: !b
  , _pNativeEnv :: !(CEKEnv e b i)
  , _pNativeFn :: !(NativeFunction e b i)
  , _pNativeArity :: !Int
  , _pNativeAppliedArgs :: ![CEKValue e b i]
  , _pNativeLoc :: !i
  } deriving (Generic)

instance (NFData b, NFData i) => NFData (PartialNativeFn e b i)

-- | Continuation Frames that handle conditional argument returns in a lazy manner.
data CondCont (e :: RuntimeMode) (b :: K.Type) (i :: K.Type)
  = AndC (EvalTerm b i)
  -- ^ <term to evaluate in the True case>
  | OrC (EvalTerm b i)
  -- ^ <term to evaluate in the False case>
  | IfC (EvalTerm b i) (EvalTerm b i)
  -- ^ <true case term> <false case term>
  | EnforceC (EvalTerm b i)
  -- ^ <error string term>
  | EnforceOneC
  -- ^ <error string term> [<enforceable term>]
  | FilterC (CanApply e b i) PactValue [PactValue] [PactValue]
  -- ^ {filtering closure} <current focused value> <remaining> <accumulator>
  | AndQC (CanApply e b i) PactValue
  -- ^ {bool comparison closure} <original value>
  | OrQC (CanApply e b i) PactValue
  -- ^ {bool comparison closure} <original value>
  | NotQC
  -- ^ Nada
  deriving (Show, Generic)

data BuiltinCont (e :: RuntimeMode) (b :: K.Type) (i :: K.Type)
  = MapC (CanApply e b i) [PactValue] [PactValue]
  -- ^ {closure} {remaining} {accum}
  | FoldC (CanApply e b i) [PactValue]
  -- ^ {closure} {accum} {rest}
  | ZipC (CanApply e b i) ([PactValue],[PactValue]) [PactValue]
  -- ^ <zip closure> <lists to zip> <accumulator>
  | SelectC TableValue (CanApply e b i) (ObjectData PactValue) [RowKey] [ObjectData PactValue] (Maybe [Field])
  -- ^ <table> <filter closure> <current value> <remaining keys> <accumulator> <fields>
  | FoldDbFilterC TableValue (CanApply e b i) (CanApply e b i) (RowKey, ObjectData PactValue) [RowKey] [(RowKey, PactValue)]
  -- ^ <table> <filter closure> <accum closure> <current k/v pair in focus> <remaining keys> <accumulator>
  | FoldDbMapC TableValue (CanApply e b i) [(RowKey, PactValue)] [PactValue]
  -- ^ <table> <accum closure> <remaining pairs> <accumulator>
  | DefineKeysetC KeySetName KeySet
  -- ^ <keyset to push to the db>
  | DefineNamespaceC Namespace
  -- ^ namespace to write to the db
  | RunKeysetPredC KeySet
  -- ^ check the keyset predicate
  deriving (Show, Generic)


-- | Control flow around Capability special forms, in particular cap token forms
data CapCont (e :: RuntimeMode) (b :: K.Type) (i :: K.Type)
  = WithCapC (EvalTerm b i)
  | ApplyMgrFunC (ManagedCap QualifiedName PactValue) (Closure e b i) PactValue PactValue
  -- ^ <cap token of the corresponding function> ^mgr closure ^ old value ^ new value
  | UpdateMgrFunC (ManagedCap QualifiedName PactValue)
  | CreateUserGuardC FullyQualifiedName [EvalTerm b i] [PactValue]
  deriving (Show, Generic)

-- | What to do post-cap evaluation: do we pop the cap from the stack,
-- or compose it within the capset
data CapPopState
  = PopCapComposed
  | PopCapInvoke
  | PopCurrCapEval (Set (CapToken QualifiedName PactValue))
  deriving (Eq, Show, Generic)

instance NFData CapPopState

data CapBodyState b i
  = CapBodyState
  { _cbPopState :: !CapPopState
  , _cbBodyCap :: !(Maybe (CapToken QualifiedName PactValue))
  , _cbEmittedEvent :: !(Maybe (PactEvent PactValue))
  , _cbEvalBody :: !(EvalTerm b i)
  } deriving (Show, Generic)

instance (NFData b, NFData i) => NFData (CapBodyState b i)

data Cont (e :: RuntimeMode) (b :: K.Type) (i :: K.Type)
  = Mt
  -- ^ Empty Continuation
  | Fn !(CanApply e b i) !(CEKEnv e b i) ![EvalTerm b i] ![CEKValue e b i] !(Cont e b i)
  -- ^ Continuation which evaluates arguments for a function to apply
  | Args !(CEKEnv e b i) i ![EvalTerm b i] !(Cont e b i)
  -- ^ Continuation holding the arguments to evaluate in a function application
  | LetC !(CEKEnv e b i) i (Arg Type i) !(EvalTerm b i) !(Cont e b i)
  -- ^ Let single-variable pushing
  -- Optimization frame: Bypasses closure creation and thus less alloc
  -- Known as a single argument it will not construct a needless closure
  | SeqC (CEKEnv e b i) i (EvalTerm b i) (Cont e b i)
  -- ^ Sequencing expression, holding the next term to evaluate
  | ListC (CEKEnv e b i) i [EvalTerm b i] [PactValue] (Cont e b i)
  -- ^ Continuation for list elements
  | CondC (CEKEnv e b i) i (CondCont e b i) (Cont e b i)
  -- ^ Continuation for conditionals with lazy semantics
  | BuiltinC (CEKEnv e b i) i (BuiltinCont e b i) (Cont e b i)
  -- ^ Continuation for higher-order function builtins
  | ObjC (CEKEnv e b i) i Field [(Field, EvalTerm b i)] [(Field, PactValue)] (Cont e b i)
  -- Todo: merge all cap constructors
  -- ^ Continuation for the current object field being evaluated, and the already evaluated pairs
  | CapInvokeC (CEKEnv e b i) i (CapCont e b i) (Cont e b i)
  -- ^ Frame for control flow around argument reduction to with-capability and create-user-guard
  | CapBodyC (CEKEnv e b i) i !(CapBodyState b i) (Cont e b i)
  -- ^ CapBodyC includes
  --  - what to do after the cap body (pop it, or compose it)
  --  - Is it a user managed cap? If so, include the body token
  --  - the capability "user body" to evaluate, generally carrying a series of expressions
  --    or a simple return value in the case of `compose-capability`
  --  - The rest of the continuation
  | CapPopC CapPopState i (Cont e b i)
  -- ^ What to do after returning from a defcap: do we compose the returned cap, or do we simply pop it from the stack
  | DefPactStepC (CEKEnv e b i) i (Cont e b i)
  -- ^ Cont frame after a defpact, ensuring we save the defpact to the database and whatnot
  | NestedDefPactStepC (CEKEnv e b i) i (Cont e b i) DefPactExec
  -- ^ Frame for control flow around nested defpact execution
  | IgnoreValueC PactValue (Cont e b i)
  -- ^ Frame to ignore value after user guard execution
  | EnforceBoolC i (Cont e b i)
  -- ^ Enforce boolean
  | EnforcePactValueC i (Cont e b i)
  -- ^ Enforce pact value
  | ModuleAdminC ModuleName (Cont e b i)
  -- ^ Add module admin on successful cap eval
  | StackPopC i (Maybe Type) (Cont e b i)
  -- ^ Pop the current stack frame and check the return value for the declared type
  | EnforceErrorC i (Cont e b i)
  -- ^ Continuation for "enforced" errors.
  deriving (Show, Generic)

instance (NFData b, NFData i) => NFData (BuiltinCont e b i)
instance (NFData b, NFData i) => NFData (CapCont e b i)
instance (NFData b, NFData i) => NFData (CondCont e b i)
instance (NFData b, NFData i) => NFData (Cont e b i)

-- | An enumerable set of frame types, for our gas model
data ContType
  = CTFn
  | CTArgs
  | CTLetC
  | CTSeqC
  | CTListC
  -- | CTCondC
  -- Conditionals
  | CTAndC
  | CTOrC
  | CTIfC
  | CTEnforceC
  | CTEnforceOneC
  | CTFilterC
  | CTAndQC
  | CTOrQC
  | CTNotQC
  -- Builtin forms
  | CTMapC
  | CTFoldC
  | CTZipC
  | CTPreSelectC
  | CTPreFoldDbC
  | CTSelectC
  | CTFoldDbFilterC
  | CTFoldDbMapC
  | CTReadC
  | CTWriteC
  | CTWithDefaultReadC
  | CTKeysC
  | CTCreateTableC
  | CTEmitEventC
  | CTDefineNamespaceC
  | CTDefineKeysetC
  --
  | CTObjC
  -- Cap control flow
  | CTCapInvokeC
  --
  | CTCapBodyC
  | CTCapPopC
  | CTDefPactStepC
  | CTNestedDefPactStepC
  | CTIgnoreValueC
  | CTEnforceBoolC
  | CTEnforcePactValueC
  | CTModuleAdminC
  | CTStackPopC
  | CTEnforceErrorC
  | CTMt
  deriving (Show, Eq, Enum, Bounded)


data CEKErrorHandler (e :: RuntimeMode) (b :: K.Type) (i :: K.Type)
  = CEKNoHandler
  | CEKHandler (CEKEnv e b i) (EvalTerm b i) (Cont e b i) (ErrorState i) (CEKErrorHandler e b i)
  | CEKEnforceOne (CEKEnv e b i) i (EvalTerm b i) [EvalTerm b i] (Cont e b i) (ErrorState i) (CEKErrorHandler e b i)
  deriving (Show, Generic)

instance (NFData b, NFData i) => NFData (CEKErrorHandler e b i)

-- data CEKStepKind
--   = CEKSmallStep
--   | CEKBigStep
--   deriving (Eq, Show)

-- type family CEKEvalResult (e :: RuntimeMode) (b :: K.Type) (i :: K.Type) where
--   CEKEvalResult e CEKBigStep b i = EvalResult e CEKBigStep b i
--   CEKEvalResult e CEKSmallStep b i = CEKReturn e b i

instance (Show i, Show b) => Show (NativeFn e b i) where
  show (NativeFn b _ _ arity _) = unwords
    ["(NativeFn"
    , show b
    , "#fn"
    , show arity
    , ")"
    ]

instance (Show i, Show b) => Show (PartialNativeFn e b i) where
  show (PartialNativeFn b _ _ arity _ _) = unwords
    ["(NativeFn"
    , show b
    , "#fn"
    , show arity
    , ")"
    ]

instance (Pretty b, Show i, Show b) => Pretty (NativeFn e b i) where
  pretty = pretty . show

instance (Show i, Show b, Pretty b) => Pretty (CEKValue e b i) where
  pretty = \case
    VPactValue pv -> pretty pv
    VClosure{} ->
      P.angles "closure#"

makeLenses ''CEKEnv

type Eval = EvalM ExecRuntime CoreBuiltin
type CoreTerm a = EvalTerm CoreBuiltin a
type CoreCEKCont = Cont ExecRuntime CoreBuiltin
type CoreCEKHandler = CEKErrorHandler ExecRuntime CoreBuiltin
type CoreCEKEnv = CEKEnv ExecRuntime CoreBuiltin
type CoreBuiltinEnv a = BuiltinEnv ExecRuntime CoreBuiltin a
type CoreCEKValue = CEKValue ExecRuntime CoreBuiltin
type CoreEvalResult = EvalResult ExecRuntime CoreBuiltin
