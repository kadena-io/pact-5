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
-- import Data.IORef

import Pact.Core.Names
import Pact.Core.Guards
import Pact.Core.Pretty(Pretty(..))
import Pact.Core.Gas
import Pact.Core.PactValue
import Pact.Core.Hash
import Pact.Core.IR.Term
import Pact.Core.Literal
import Pact.Core.Type
import Pact.Core.Persistence ( PactDb )
import Pact.Core.ModRefs
import Pact.Core.Capabilities
import Pact.Core.Environment
import Pact.Core.DefPacts.Types (DefPactExec)
import Pact.Core.Errors

import qualified Pact.Core.Pretty as P
import qualified Pact.Core.DefPacts.Types as P


-- | The top level env map
type CEKTLEnv b i = Map FullyQualifiedName (EvalDef b i)

-- | Locally bound variables
-- type CEKEnv b i m = RAList (CEKValue b i m)

data CEKEnv b i m
  = CEKEnv
  { _ceLocal :: RAList (CEKValue b i m)
  , _cePactDb :: PactDb b i
  , _ceBuiltins :: BuiltinEnv b i m
  , _ceDefPactStep :: Maybe P.DefPactStep
  , _ceInCap :: Bool }

instance (Show i, Show b) => Show (CEKEnv b i m) where
  show (CEKEnv e _ _ _ _) = show e

-- | List of builtins
type BuiltinEnv b i m = i -> b -> CEKEnv b i m -> NativeFn b i m

data ClosureType
  = NullaryClosure
  | ArgClosure !(NonEmpty (Maybe Type))
  deriving Show

data Closure b i m
  = Closure
  { _cloFnName :: !Text
  , _cloModName :: !ModuleName
  , _cloTypes :: ClosureType
  , _cloArity :: !Int
  , _cloTerm :: !(EvalTerm b i)
  , _cloRType :: !(Maybe Type)
  , _cloEnv :: !(CEKEnv b i m)
  , _cloInfo :: i
  } deriving Show

-- | A closure coming from a lambda application with its accompanying environment capturing args,
-- but is not partially applied
data LamClosure b i m
  = LamClosure
  { _lcloTypes :: ClosureType
  , _lcloArity :: Int
  , _lcloTerm :: !(EvalTerm b i)
  , _lcloRType :: !(Maybe Type)
  , _lcloEnv :: !(CEKEnv b i m)
  , _lcloInfo :: i
  } deriving Show

-- | A partially applied function because we don't allow
-- them to be applied at the lhs of an app since pact historically hasn't had partial closures.
-- This is a bit annoying to deal with but helps preserve semantics
data PartialClosure b i m
  = PartialClosure
  { _pcloFrame :: Maybe StackFrame
  , _pcloTypes :: !(NonEmpty (Maybe Type))
  , _pcloArity :: Int
  , _pcloTerm :: !(EvalTerm b i)
  , _pcloRType :: !(Maybe Type)
  , _pcloEnv :: !(CEKEnv b i m)
  , _pcloInfo :: i
  } deriving Show

data DefPactClosure b i m
  = DefPactClosure
  { _pactcloFQN :: FullyQualifiedName
  , _pactcloTypes :: !ClosureType
  , _pactcloArity :: Int
  , _pactEnv :: !(CEKEnv b i m)
  , _pactcloInfo :: i
  } deriving Show

data CapTokenClosure i
  = CapTokenClosure
  { _ctcCapName :: FullyQualifiedName
  , _ctcTypes :: [Maybe Type]
  , _ctcArity :: Int
  , _ctcInfo :: i
  } deriving (Eq, Show)

data CanApply b i m
  = C {-# UNPACK #-} !(Closure b i m)
  | LC {-# UNPACK #-} !(LamClosure b i m)
  | PC {-# UNPACK #-} !(PartialClosure b i m)
  | N {-# UNPACK #-} !(NativeFn b i m)
  | PN {-# UNPACK #-} !(PartialNativeFn b i m)
  | DPC {-# UNPACK #-} !(DefPactClosure b i m)
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
data CEKValue b i m
  = VPactValue PactValue
  | VTable !TableValue
  -- = VLiteral !Literal
  -- | VList !(Vector (CEKValue b i m))
  | VClosure {-# UNPACK #-} !(CanApply b i m)
  -- | VModRef ModuleName [ModuleName]
  -- | VGuard !(Guard FullyQualifiedName PactValue)

instance Show (CEKValue b i m) where
  show = \case
    VPactValue pv -> show pv
    VTable vt -> "table" <> show (_tvName vt)
    VClosure _ -> "closure<>"

pattern VLiteral :: Literal -> CEKValue b i m
pattern VLiteral lit = VPactValue (PLiteral lit)

pattern VString :: Text -> CEKValue b i m
pattern VString txt = VLiteral (LString txt)

pattern VInteger :: Integer -> CEKValue b i m
pattern VInteger txt = VLiteral (LInteger txt)

pattern VUnit :: CEKValue b i m
pattern VUnit = VLiteral LUnit

pattern VBool :: Bool -> CEKValue b i m
pattern VBool b = VLiteral (LBool b)

pattern VDecimal :: Decimal -> CEKValue b i m
pattern VDecimal d = VLiteral (LDecimal d)

pattern VGuard :: Guard FullyQualifiedName PactValue -> CEKValue b i m
pattern VGuard g = VPactValue (PGuard g)

pattern VList :: Vector PactValue -> CEKValue b i m
pattern VList p = VPactValue (PList p)

pattern VObject :: Map Field PactValue -> CEKValue b i m
pattern VObject o = VPactValue (PObject o)

pattern VModRef :: ModRef -> CEKValue b i m
pattern VModRef mn = VPactValue (PModRef mn)

pattern VCapToken :: CapToken FullyQualifiedName PactValue -> CEKValue b i m
pattern VCapToken ct = VPactValue (PCapToken ct)

pattern VNative :: NativeFn b i m -> CEKValue b i m
pattern VNative clo = VClosure (N clo)

pattern VPartialNative :: PartialNativeFn b i m -> CEKValue b i m
pattern VPartialNative clo = VClosure (PN clo)

pattern VDefClosure :: Closure b i m -> CEKValue b i m
pattern VDefClosure clo = VClosure (C clo)

pattern VLamClosure :: LamClosure b i m -> CEKValue b i m
pattern VLamClosure clo = VClosure (LC clo)

pattern VPartialClosure :: PartialClosure b i m -> CEKValue b i m
pattern VPartialClosure clo = VClosure (PC clo)

pattern VDefPactClosure :: DefPactClosure b i m -> CEKValue b i m
pattern VDefPactClosure clo = VClosure (DPC clo)

-- | Result of an evaluation step, either a CEK value or an error.
data EvalResult b i m
  = EvalValue (CEKValue b i m)
  | VError Text i
  deriving Show


-- data EvalTEnv b i
--   = EvalTEnv
--   { _emRuntimeEnv :: EvalEnv b i
--   , _emGas :: IORef Gas
--   , _emGasLog :: IORef (Maybe [(Text, Gas)])
--   }

-- Todo: are we going to inject state as the reader monad here?
newtype EvalM b i a =
  EvalT (ReaderT (EvalEnv b i) (ExceptT (PactError i) (StateT (EvalState b i) IO)) a)
  -- EvalT (ReaderT (EvalEnv b i) (StateT (EvalState b i) (ExceptT (PactError i) IO)) a)
  deriving
    ( Functor, Applicative, Monad
    , MonadIO
    , MonadThrow
    , MonadCatch)
  via (ReaderT (EvalEnv b i) (ExceptT (PactError i) (StateT (EvalState b i) IO)))

runEvalM
  :: EvalEnv b i
  -> EvalState b i
  -> EvalM b i a
  -> IO (Either (PactError i) a, EvalState b i)
runEvalM env st (EvalT action) =
  runStateT (runExceptT (runReaderT action env)) st

type NativeFunction b i m
  = i -> b -> Cont b i m -> CEKErrorHandler b i m -> CEKEnv b i m -> [CEKValue b i m] -> m (EvalResult b i m)

data NativeFn b i m
  = NativeFn
  { _native :: b
  , _nativeEnv :: CEKEnv b i m
  , _nativeFn :: NativeFunction b i m
  , _nativeArity :: {-# UNPACK #-} !Int
  , _nativeLoc :: i
  }

-- | A partially applied native because we don't allow
-- them to be applied at the lhs of an app since pact historically hasn't had partial closures.
-- This is a bit annoying to deal with but helps preserve semantics
data PartialNativeFn b i m
  = PartialNativeFn
  { _pNative :: b
  , _pNativeEnv :: CEKEnv b i m
  , _pNativeFn :: NativeFunction b i m
  , _pNativeArity :: {-# UNPACK #-} !Int
  , _pNativeAppliedArgs :: [CEKValue b i m]
  , _pNativeLoc :: i
  }


data ExecutionMode
  = Transactional
  | Local
  deriving (Eq, Show, Bounded, Enum)

data CondFrame b i
  = AndFrame (EvalTerm b i)
  | OrFrame (EvalTerm b i)
  | IfFrame (EvalTerm b i) (EvalTerm b i)
  | EnforceFrame (EvalTerm b i)
  | EnforceOneFrame (EvalTerm b i) [EvalTerm b i]
  deriving Show

data CapFrame b i
  = WithCapFrame FullyQualifiedName (EvalTerm b i)
  | CreateUserGuardFrame FullyQualifiedName
  -- | RequireCapFrame FullyQualifiedName
  -- | ComposeCapFrame FullyQualifiedName
  -- | InstallCapFrame FullyQualifiedName
  -- | EmitEventFrame FullyQualifiedName
  deriving Show


data CapPopState
  = PopCapComposed
  | PopCapInvoke
  deriving (Eq, Show)

data Cont b i m
  = Fn (CanApply b i m) (CEKEnv b i m) [EvalTerm b i] [CEKValue b i m] (Cont b i m)
  -- ^ Continuation which evaluates arguments for a function to apply
  | Args (CEKEnv b i m) i [EvalTerm b i] (Cont b i m)
  -- ^ Continuation holding the arguments to evaluate in a function application
  | LetC (CEKEnv b i m) (EvalTerm b i) (Cont b i m)
  -- ^ Let single-variable pushing
  -- Known as a single argument it will not construct a needless closure
  | SeqC (CEKEnv b i m) (EvalTerm b i) (Cont b i m)
  -- ^ Sequencing expression, holding the next term to evaluate
  | ListC (CEKEnv b i m) [EvalTerm b i] [PactValue] (Cont b i m)
  -- ^ Continuation for list elements
  | CondC (CEKEnv b i m) i (CondFrame b i) (Cont b i m)
  -- ^ Continuation for conditionals with lazy semantics
  | ObjC (CEKEnv b i m) Field [(Field, EvalTerm b i)] [(Field, PactValue)] (Cont b i m)
  -- ^ Continuation for the current object field being evaluated, and the already evaluated pairs
  | CapInvokeC (CEKEnv b i m) i [EvalTerm b i] [PactValue] (CapFrame b i) (Cont b i m)
  -- ^ Capability special form frams that eva
  | CapBodyC CapPopState (CEKEnv b i m) (Maybe (CapToken QualifiedName PactValue)) (Maybe (PactEvent PactValue)) (EvalTerm b i) (Cont b i m)
  -- ^ CapBodyC includes
  --  - what to do after the cap body (pop it, or compose it)
  --  - Is it a user managed cap? If so, include the body token
  --  - the capability "user body" to evaluate, generally carrying a series of expressions
  --    or a simple return value in the case of `compose-capability`
  --  - The rest of the continuation
  | CapPopC CapPopState (Cont b i m)
  -- ^ What to do after returning from a defcap: do we compose the returned cap, or do we simply pop it from the stack
  | DefPactStepC (CEKEnv b i m) (Cont b i m)
  -- ^ Cont frame after a defpact, ensuring we save the defpact to the database and whatnot
  | NestedDefPactStepC (CEKEnv b i m) (Cont b i m) DefPactExec
  -- ^ Frame for control flow around nested defpact execution
  | UserGuardC (Cont b i m)
  -- ^ Frame to ignore value after user guard execution
  | StackPopC i (Maybe Type) (Cont b i m)
  -- ^ Pop the current stack frame and check the return value for the declared type
  | EnforceErrorC i (Cont b i m)
  -- ^ Continuation for "enforced" errors.
  | Mt
  -- ^ Empty Continuation
  deriving Show

-- | State to preserve in the error handler
data ErrorState
  = ErrorState (CapState QualifiedName PactValue) [StackFrame]
  deriving Show

data CEKErrorHandler b i m
  = CEKNoHandler
  | CEKHandler (CEKEnv b i m) (EvalTerm b i) (Cont b i m) ErrorState (CEKErrorHandler b i m)
  | CEKEnforceOne (CEKEnv b i m) i (EvalTerm b i) [EvalTerm b i] (Cont b i m) ErrorState (CEKErrorHandler b i m)
  deriving Show

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

instance (Pretty b, Show i, Show b) => Pretty (NativeFn b i m) where
  pretty = pretty . show

instance (Show i, Show b, Pretty b) => Pretty (CEKValue b i m) where
  pretty = \case
    VPactValue pv -> pretty pv
    VTable tv -> "table" <> P.braces (pretty (_tvName tv))
    VClosure{} ->
      P.angles "closure#"

makeLenses ''CEKEnv
-- makeLenses ''EvalEnv

instance MonadGas (EvalM b i) where
  logGas _msg _g = pure ()
    -- r <- EvalT $ view emGasLog
    -- liftIO $ modifyIORef' r (fmap ((msg, g):))

  chargeGas _g = pure ()
    -- r <- EvalT $ view emGas
    -- liftIO (modifyIORef' r (<> g))

instance MonadEvalEnv b i (EvalM b i) where
  readEnv = EvalT ask

instance MonadEvalState b i (EvalM b i) where
  getEvalState = EvalT get
  putEvalState p = EvalT (put p)
  modifyEvalState f = EvalT (modify' f)

