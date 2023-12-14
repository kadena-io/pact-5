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
 ( CEKTLEnv
 , CEKEnv(..)
 , ceLocal
 , cePactDb
 , ceBuiltins
 , ceDefPactStep
 , ceInCap
 , EvalEnv(..)
 , NativeFunction
 , BuiltinEnv
 , NativeFn(..)
 , EvalM(..)
 , runEvalM
 , CEKValue(..)
 , Cont(..)
 , CEKErrorHandler(..)
 , CondFrame(..)
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
 , CapFrame(..)
 , CapState(..)
 , csSlots, csManaged
 , ManagedCap(..)
 , mcCap, mcManaged, mcOriginalCap
 , ManagedCapType(..)
 , PactEvent(..)
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
 , BuiltinFrame(..)
--  , CEKEval(..)
 , CEKReturn(..)
 , CEKEvalResult
 , CEKStepKind(..)
 ) where

import Control.Lens
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Except
import Data.List.NonEmpty(NonEmpty)
import Data.Text(Text)
import Data.Map.Strict(Map)
import Data.Decimal(Decimal)
import Data.Vector(Vector)
import Data.RAList(RAList)
import Pact.Time(UTCTime)

import qualified Data.Kind as K

import Pact.Core.Names
import Pact.Core.Guards
import Pact.Core.Pretty(Pretty(..))
import Pact.Core.Gas
import Pact.Core.PactValue
import Pact.Core.Hash
import Pact.Core.IR.Term
import Pact.Core.Literal
import Pact.Core.Type
import Pact.Core.Persistence
import Pact.Core.ModRefs
import Pact.Core.Capabilities
import Pact.Core.Environment
import Pact.Core.DefPacts.Types
import Pact.Core.Debug
import Pact.Core.Errors

import qualified Pact.Core.Pretty as P

data CEKReturn b i m
  = CEKEvaluateTerm (Cont CEKSmallStep b i m) (CEKErrorHandler CEKSmallStep b i m) (CEKEnv CEKSmallStep b i m) (EvalTerm b i)
  | CEKReturn (Cont CEKSmallStep b i m) (CEKErrorHandler CEKSmallStep b i m) (EvalResult CEKSmallStep b i m)
  deriving Show

-- | The top level env map
type CEKTLEnv b i = Map FullyQualifiedName (EvalDef b i)

-- | Locally bound variables
-- type CEKEnv step b i m = RAList (CEKValue b i m)

data CEKEnv (step :: CEKStepKind) (b :: K.Type) (i :: K.Type) (m :: K.Type -> K.Type)
  = CEKEnv
  { _ceLocal :: RAList (CEKValue step b i m)
  , _cePactDb :: PactDb b i
  , _ceBuiltins :: BuiltinEnv step b i m
  , _ceDefPactStep :: Maybe DefPactStep
  , _ceInCap :: Bool }

instance (Show i, Show b) => Show (CEKEnv step b i m) where
  show (CEKEnv e _ _ _ _) = show e

-- | List of builtins
type BuiltinEnv (step :: CEKStepKind) (b :: K.Type) (i :: K.Type) (m :: K.Type -> K.Type)
  = i -> b -> CEKEnv step b i m -> NativeFn step b i m


data ClosureType
  = NullaryClosure
  | ArgClosure !(NonEmpty (Maybe Type))
  deriving Show

data Closure (step :: CEKStepKind) (b :: K.Type) (i :: K.Type) (m :: K.Type -> K.Type)
  = Closure
  { _cloFnName :: !Text
  , _cloModName :: !ModuleName
  , _cloTypes :: ClosureType
  , _cloArity :: !Int
  , _cloTerm :: !(EvalTerm b i)
  , _cloRType :: !(Maybe Type)
  , _cloEnv :: !(CEKEnv step b i m)
  , _cloInfo :: i
  } deriving Show

-- | A closure coming from a lambda application with its accompanying environment capturing args,
-- but is not partially applied
data LamClosure (step :: CEKStepKind) (b :: K.Type) (i :: K.Type) (m :: K.Type -> K.Type)
  = LamClosure
  { _lcloTypes :: ClosureType
  , _lcloArity :: Int
  , _lcloTerm :: !(EvalTerm b i)
  , _lcloRType :: !(Maybe Type)
  , _lcloEnv :: !(CEKEnv step b i m)
  , _lcloInfo :: i
  } deriving Show

-- | A partially applied function because we don't allow
-- them to be applied at the lhs of an app since pact historically hasn't had partial closures.
-- This is a bit annoying to deal with but helps preserve semantics
data PartialClosure (step :: CEKStepKind) (b :: K.Type) (i :: K.Type) (m :: K.Type -> K.Type)
  = PartialClosure
  { _pcloFrame :: Maybe StackFrame
  , _pcloTypes :: !(NonEmpty (Maybe Type))
  , _pcloArity :: Int
  , _pcloTerm :: !(EvalTerm b i)
  , _pcloRType :: !(Maybe Type)
  , _pcloEnv :: !(CEKEnv step b i m)
  , _pcloInfo :: i
  } deriving Show

data DefPactClosure (step :: CEKStepKind) (b :: K.Type) (i :: K.Type) (m :: K.Type -> K.Type)
  = DefPactClosure
  { _pactcloFQN :: FullyQualifiedName
  , _pactcloTypes :: !ClosureType
  , _pactcloArity :: Int
  , _pactEnv :: !(CEKEnv step b i m)
  , _pactcloInfo :: i
  } deriving Show

data CapTokenClosure i
  = CapTokenClosure
  { _ctcCapName :: FullyQualifiedName
  , _ctcTypes :: [Maybe Type]
  , _ctcArity :: Int
  , _ctcInfo :: i
  } deriving (Eq, Show)

data CanApply (step :: CEKStepKind) (b :: K.Type) (i :: K.Type) (m :: K.Type -> K.Type)
  = C {-# UNPACK #-} !(Closure step b i m)
  | LC {-# UNPACK #-} !(LamClosure step b i m)
  | PC {-# UNPACK #-} !(PartialClosure step b i m)
  | N {-# UNPACK #-} !(NativeFn step b i m)
  | PN {-# UNPACK #-} !(PartialNativeFn step b i m)
  | DPC {-# UNPACK #-} !(DefPactClosure step b i m)
  | CT {-# UNPACK #-} !(CapTokenClosure i)
  deriving Show

data TableValue
  = TableValue
  { _tvName :: !TableName
  , _tvModule :: !ModuleName
  , _tvHash :: !ModuleHash
  , _tvSchema :: !Schema
  } deriving Show

-- | The type of our semantic runtime values
data CEKValue (step :: CEKStepKind) (b :: K.Type) (i :: K.Type) (m :: K.Type -> K.Type)
  = VPactValue PactValue
  -- ^ PactValue(s), which contain no terms
  | VTable !TableValue
  -- ^ Table references, which despite being a syntactic
  -- value with
  | VClosure  !(CanApply step b i m)
  -- ^ Closures, which may contain terms

instance Show (CEKValue step b i m) where
  show = \case
    VPactValue pv -> show pv
    VTable vt -> "table" <> show (_tvName vt)
    VClosure _ -> "closure<>"

pattern VLiteral :: Literal -> CEKValue step b i m
pattern VLiteral lit = VPactValue (PLiteral lit)

pattern VString :: Text -> CEKValue step b i m
pattern VString txt = VLiteral (LString txt)

pattern VInteger :: Integer -> CEKValue step b i m
pattern VInteger txt = VLiteral (LInteger txt)

pattern VUnit :: CEKValue step b i m
pattern VUnit = VLiteral LUnit

pattern VBool :: Bool -> CEKValue step b i m
pattern VBool b = VLiteral (LBool b)

pattern VDecimal :: Decimal -> CEKValue step b i m
pattern VDecimal d = VLiteral (LDecimal d)

pattern VGuard :: Guard FullyQualifiedName PactValue -> CEKValue step b i m
pattern VGuard g = VPactValue (PGuard g)

pattern VList :: Vector PactValue -> CEKValue step b i m
pattern VList p = VPactValue (PList p)

pattern VTime :: UTCTime -> CEKValue step b i m
pattern VTime p = VPactValue (PTime p)

pattern VObject :: Map Field PactValue -> CEKValue step b i m
pattern VObject o = VPactValue (PObject o)

pattern VModRef :: ModRef -> CEKValue step b i m
pattern VModRef mn = VPactValue (PModRef mn)

pattern VCapToken :: CapToken FullyQualifiedName PactValue -> CEKValue step b i m
pattern VCapToken ct = VPactValue (PCapToken ct)

pattern VNative :: NativeFn step b i m -> CEKValue step b i m
pattern VNative clo = VClosure (N clo)

pattern VPartialNative :: PartialNativeFn step b i m -> CEKValue step b i m
pattern VPartialNative clo = VClosure (PN clo)

pattern VDefClosure :: Closure step b i m -> CEKValue step b i m
pattern VDefClosure clo = VClosure (C clo)

pattern VLamClosure :: LamClosure step b i m -> CEKValue step b i m
pattern VLamClosure clo = VClosure (LC clo)

pattern VPartialClosure :: PartialClosure step b i m -> CEKValue step b i m
pattern VPartialClosure clo = VClosure (PC clo)

pattern VDefPactClosure :: DefPactClosure step b i m -> CEKValue step b i m
pattern VDefPactClosure clo = VClosure (DPC clo)

-- | Result of an evaluation step, either a CEK value or an error.
data EvalResult (step :: CEKStepKind) (b :: K.Type) (i :: K.Type) (m :: K.Type -> K.Type)
  = EvalValue (CEKValue step b i m)
  | VError Text i
  deriving Show


-- Todo: are we going to inject state as the reader monad here?
newtype EvalM b i a =
  EvalT (ReaderT (EvalEnv b i) (ExceptT (PactError i) (StateT (EvalState b i) IO)) a)
  deriving
    ( Functor, Applicative, Monad
    , MonadIO
    , MonadThrow
    , MonadCatch
    , MonadError (PactError i))
  via (ReaderT (EvalEnv b i) (ExceptT (PactError i) (StateT (EvalState b i) IO)))

instance PhaseDebug b i (EvalM b i) where
  debugPrint _ _ = pure ()

runEvalM
  :: EvalEnv b i
  -> EvalState b i
  -> EvalM b i a
  -> IO (Either (PactError i) a, EvalState b i)
runEvalM env st (EvalT action) =
  runStateT (runExceptT (runReaderT action env)) st

type NativeFunction (step :: CEKStepKind) (b :: K.Type) (i :: K.Type) (m :: K.Type -> K.Type)
  = i -> b -> Cont step b i m -> CEKErrorHandler step b i m -> CEKEnv step b i m -> [CEKValue step b i m] -> m (CEKEvalResult step b i m)

data NativeFn (step :: CEKStepKind) (b :: K.Type) (i :: K.Type) (m :: K.Type -> K.Type)
  = NativeFn
  { _native :: b
  , _nativeEnv :: CEKEnv step b i m
  , _nativeFn :: NativeFunction step b i m
  , _nativeArity :: {-# UNPACK #-} !Int
  , _nativeLoc :: i
  }

-- | A partially applied native because we don't allow
-- them to be applied at the lhs of an app since pact historically hasn't had partial closures.
-- This is a bit annoying to deal with but helps preserve semantics
data PartialNativeFn (step :: CEKStepKind) (b :: K.Type) (i :: K.Type) (m :: K.Type -> K.Type)
  = PartialNativeFn
  { _pNative :: b
  , _pNativeEnv :: CEKEnv step b i m
  , _pNativeFn :: NativeFunction step b i m
  , _pNativeArity :: {-# UNPACK #-} !Int
  , _pNativeAppliedArgs :: [CEKValue step b i m]
  , _pNativeLoc :: i
  }


data CondFrame (step :: CEKStepKind) (b :: K.Type) (i :: K.Type) (m :: K.Type -> K.Type)
  = AndFrame (EvalTerm b i)
  | OrFrame (EvalTerm b i)
  | IfFrame (EvalTerm b i) (EvalTerm b i)
  | EnforceFrame (EvalTerm b i)
  | EnforceOneFrame (EvalTerm b i) [EvalTerm b i]
  | FilterFrame (CanApply step b i m) PactValue [PactValue] [PactValue]
  | AndQFrame (CanApply step b i m) PactValue
  | OrQFrame (CanApply step b i m) PactValue
  | NotQFrame
  deriving Show

data BuiltinFrame (step :: CEKStepKind) (b :: K.Type) (i :: K.Type) (m :: K.Type -> K.Type)
  = MapFrame (CanApply step b i m) [PactValue] [PactValue]
  -- ^ {closure} {remaining} {accum}
  | FoldFrame (CanApply step b i m) [PactValue]
  -- ^ {closure} {accum} {rest}
  | ZipFrame (CanApply step b i m) ([PactValue],[PactValue]) [PactValue]
  -- ^ <zip closure> <lists to zip> <accumulator>
  | PreSelectFrame TableValue (CanApply step b i m) (Maybe [Field])
  -- ^ <table> <select filter closure> <filter fields>*
  | PreFoldDbFrame TableValue (CanApply step b i m) (CanApply step b i m)
  -- ^ <table> <select filter closure> <accumulator closure>
  | SelectFrame TableValue (CanApply step b i m) (ObjectData PactValue) [RowKey] [ObjectData PactValue] (Maybe [Field])
  -- ^ <table> <filter closure> <current value> <remaining keys> <accumulator> <fields>
  | FoldDbFilterFrame TableValue (CanApply step b i m) (CanApply step b i m) (RowKey, ObjectData PactValue) [RowKey] [(RowKey, PactValue)]
  -- ^ <table> <filter closure> <accum closure> <current k/v pair in focus> <remaining keys> <accumulator>
  | FoldDbMapFrame TableValue (CanApply step b i m) [(RowKey, PactValue)] [PactValue]
  -- ^ <table> <accum closure> <remaining pairs> <accumulator>
  | ReadFrame TableValue RowKey
  -- ^ <table> <key to read>
  | WriteFrame TableValue WriteType RowKey (ObjectData PactValue)
  -- ^ <table> <write type> <key to write> <value to write>
  | WithReadFrame TableValue RowKey (CanApply step b i m)
   -- ^ <table> <key to read> <closure to apply afterwards>
  | WithDefaultReadFrame TableValue RowKey (ObjectData PactValue) (CanApply step b i m)
  -- ^ <table> <key to read> <default value> <closure to apply afterwards>
  | KeysFrame TableValue
  -- ^ Table to apply `keys` to
  | TxIdsFrame TableValue Integer
  -- ^ <table> <key to read> <default value> <closure to apply afterwards>
  | TxLogFrame TableValue Integer
  -- ^ <table> <txid>
  | KeyLogFrame TableValue RowKey Integer
  -- <table> <key> <txid>
  | CreateTableFrame TableValue
  -- <create-table>
  | EmitEventFrame (CapToken FullyQualifiedName PactValue)
  deriving Show


data CapFrame b i
  = WithCapFrame FullyQualifiedName (EvalTerm b i)
  | CreateUserGuardFrame FullyQualifiedName
  deriving Show


data CapPopState
  = PopCapComposed
  | PopCapInvoke
  deriving (Eq, Show)

data Cont (step :: CEKStepKind) (b :: K.Type) (i :: K.Type) (m :: K.Type -> K.Type)
  = Fn (CanApply step b i m) (CEKEnv step b i m) [EvalTerm b i] [CEKValue step b i m] (Cont step b i m)
  -- ^ Continuation which evaluates arguments for a function to apply
  | Args (CEKEnv step b i m) i [EvalTerm b i] (Cont step b i m)
  -- ^ Continuation holding the arguments to evaluate in a function application
  | LetC (CEKEnv step b i m) (EvalTerm b i) (Cont step b i m)
  -- ^ Let single-variable pushing
  -- Optimization frame: Bypasses closure creation and thus less alloc
  -- Known as a single argument it will not construct a needless closure
  | SeqC (CEKEnv step b i m) (EvalTerm b i) (Cont step b i m)
  -- ^ Sequencing expression, holding the next term to evaluate
  | ListC (CEKEnv step b i m) i [EvalTerm b i] [PactValue] (Cont step b i m)
  -- ^ Continuation for list elements
  | CondC (CEKEnv step b i m) i (CondFrame step b i m) (Cont step b i m)
  -- ^ Continuation for conditionals with lazy semantics
  | BuiltinC (CEKEnv step b i m) i (BuiltinFrame step b i m) (Cont step b i m)
  -- ^ Continuation for higher-order function builtins
  | ObjC (CEKEnv step b i m) i Field [(Field, EvalTerm b i)] [(Field, PactValue)] (Cont step b i m)
  -- Todo: merge all cap constructors
  -- ^ Continuation for the current object field being evaluated, and the already evaluated pairs
  | CapInvokeC (CEKEnv step b i m) i [EvalTerm b i] [PactValue] (CapFrame b i) (Cont step b i m)
  -- ^ Frame for control flow around argument reduction to with-capability and create-user-guard
  | EvalCapC (CEKEnv step b i m) i FQCapToken (EvalTerm b i) (Cont step b i m)
  -- ^ Capability special form frams that eva
  | CapBodyC CapPopState (CEKEnv step b i m) (Maybe (CapToken QualifiedName PactValue)) (Maybe (PactEvent PactValue)) (EvalTerm b i) (Cont step b i m)
  -- ^ CapBodyC includes
  --  - what to do after the cap body (pop it, or compose it)
  --  - Is it a user managed cap? If so, include the body token
  --  - the capability "user body" to evaluate, generally carrying a series of expressions
  --    or a simple return value in the case of `compose-capability`
  --  - The rest of the continuation
  | CapPopC CapPopState (Cont step b i m)
  -- ^ What to do after returning from a defcap: do we compose the returned cap, or do we simply pop it from the stack
  | DefPactStepC (CEKEnv step b i m) (Cont step b i m)
  -- ^ Cont frame after a defpact, ensuring we save the defpact to the database and whatnot
  | NestedDefPactStepC (CEKEnv step b i m) (Cont step b i m) DefPactExec
  -- ^ Frame for control flow around nested defpact execution
  | IgnoreValueC PactValue (Cont step b i m)
  -- ^ Frame to ignore value after user guard execution
  | EnforceBoolC i (Cont step b i m)
  -- ^ Enforce boolean
  | EnforcePactValueC i (Cont step b i m)
  -- ^ Enforce pact value
  | ModuleAdminC ModuleName (Cont step b i m)
  -- ^ Add module admin on successful cap eval
  | StackPopC i (Maybe Type) (Cont step b i m)
  -- ^ Pop the current stack frame and check the return value for the declared type
  | EnforceErrorC i (Cont step b i m)
  -- ^ Continuation for "enforced" errors.
  | Mt
  -- ^ Empty Continuation
  deriving Show

-- | State to preserve in the error handler
data ErrorState
  = ErrorState (CapState QualifiedName PactValue) [StackFrame]
  deriving Show

data CEKErrorHandler (step :: CEKStepKind) (b :: K.Type) (i :: K.Type) (m :: K.Type -> K.Type)
  = CEKNoHandler
  | CEKHandler (CEKEnv step b i m) (EvalTerm b i) (Cont step b i m) ErrorState (CEKErrorHandler step b i m)
  | CEKEnforceOne (CEKEnv step b i m) i (EvalTerm b i) [EvalTerm b i] (Cont step b i m) ErrorState (CEKErrorHandler step b i m)
  deriving Show

data CEKStepKind
  = CEKSmallStep
  | CEKBigStep
  deriving (Eq, Show)

type family CEKEvalResult (step :: CEKStepKind) (b :: K.Type) (i :: K.Type) (m :: K.Type -> K.Type) where
  CEKEvalResult CEKBigStep b i m = EvalResult CEKBigStep b i m
  CEKEvalResult CEKSmallStep b i m = CEKReturn b i m

instance (Show i, Show b) => Show (NativeFn step b i m) where
  show (NativeFn b _ _ arity _) = unwords
    ["(NativeFn"
    , show b
    , "#fn"
    , show arity
    , ")"
    ]

instance (Show i, Show b) => Show (PartialNativeFn step b i m) where
  show (PartialNativeFn b _ _ arity _ _) = unwords
    ["(NativeFn"
    , show b
    , "#fn"
    , show arity
    , ")"
    ]

instance (Pretty b, Show i, Show b) => Pretty (NativeFn step b i m) where
  pretty = pretty . show

instance (Show i, Show b, Pretty b) => Pretty (CEKValue step b i m) where
  pretty = \case
    VPactValue pv -> pretty pv
    VTable tv -> "table" <> P.braces (pretty (_tvName tv))
    VClosure{} ->
      P.angles "closure#"

makeLenses ''CEKEnv

instance MonadGas (EvalM b i) where
  logGas _msg _g = pure ()

  chargeGas _g = pure ()

instance MonadEvalEnv b i (EvalM b i) where
  readEnv = EvalT ask

instance MonadEvalState b i (EvalM b i) where
  getEvalState = EvalT get
  putEvalState p = EvalT (put p)
  modifyEvalState f = EvalT (modify' f)

