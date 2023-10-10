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
 , EvalEnv(..)
 , NativeFunction
 , BuiltinEnv
 , NativeFn(..)
 , EvalT(..)
 , runEvalT
 , CEKValue(..)
 , Cont(..)
 , CEKErrorHandler(..)
 , MonadEvalEnv(..)
 , MonadEvalState(..)
 , MonadGas(..)
 , CondFrame(..)
 , MonadEval
 , Closure(..)
 , EvalResult(..)
 , EvalTEnv(..)
 , emGas, emGasLog, emRuntimeEnv
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
 , pattern VNative
 , pattern VPartialNative
 , pattern VCapToken
 , CapFrame(..)
 , CapPopState(..)
 , LamClosure(..)
 , PartialNativeFn(..)
 , PartialClosure(..)
 , CapTokenClosure(..)
 , CanApply(..)
 , TableValue(..)
 ) where

import Control.Lens hiding ((%%=))
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.List.NonEmpty(NonEmpty)
import Data.Text(Text)
import Data.Map.Strict(Map)
import Data.Default
import Data.Decimal(Decimal)
import Data.Vector(Vector)
import Data.RAList(RAList)
import Data.IORef

import Pact.Core.Names
import Pact.Core.Guards
import Pact.Core.Pretty(Pretty(..))
import Pact.Core.Gas
import Pact.Core.PactValue
import Pact.Core.Errors
import Pact.Core.Hash
import Pact.Core.IR.Term
import Pact.Core.Literal
import Pact.Core.Type
import Pact.Core.Persistence
import Pact.Core.ModRefs
import Pact.Core.Capabilities
import Pact.Core.Environment
import qualified Pact.Core.Pretty as P


-- | The top level env map
type CEKTLEnv b i = Map FullyQualifiedName (EvalDef b i)

-- | Locally bound variables
-- type CEKEnv b i m = RAList (CEKValue b i m)

data CEKEnv b i m
  = CEKEnv
  { _ceLocal :: RAList (CEKValue b i m)
  , _cePactDb :: PactDb b i
  , _ceBuiltins :: BuiltinEnv b i m }

instance (Show i, Show b) => Show (CEKEnv b i m) where
  show (CEKEnv e _ _) = show e

-- | List of builtins
type BuiltinEnv b i m = i -> b -> CEKEnv b i m -> NativeFn b i m

data Closure b i m
  = Closure
  { _cloFnName :: !Text
  , _cloModName :: !ModuleName
  , _cloTypes :: !(NonEmpty (Maybe Type))
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
  { _lcloTypes :: !(NonEmpty (Maybe Type))
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

-- | Result of an evaluation step, either a CEK value or an error.
data EvalResult b i m
  = EvalValue (CEKValue b i m)
  | VError Text
  deriving Show


type MonadEval b i m = (MonadEvalEnv b i m, MonadEvalState b i m, MonadGas m, MonadError (PactError i) m, MonadIO m, Default i)

class Monad m => MonadGas m where
  logGas :: Text -> Gas -> m ()
  chargeGas :: Gas -> m ()

class (Monad m) => MonadEvalEnv b i m | m -> b, m -> i where
  readEnv :: m (EvalEnv b i)

-- | Our monad mirroring `EvalState` for our evaluation state
class Monad m => MonadEvalState b i m | m -> b, m -> i where
  getEvalState :: m (EvalState b i)
  putEvalState :: EvalState b i -> m ()
  modifyEvalState :: (EvalState b i -> EvalState b i) -> m ()


data EvalTEnv b i m
  = EvalTEnv
  { _emRuntimeEnv :: CEKEnv b i (EvalT b i m)
  , _emGas :: IORef Gas
  , _emGasLog :: IORef (Maybe [(Text, Gas)])
  }

-- Todo: are we going to inject state as the reader monad here?
newtype EvalT b i m a =
  EvalT (ReaderT (EvalTEnv b i m) (StateT (EvalState b i) m) a)
  deriving
    ( Functor, Applicative, Monad
    , MonadIO
    , MonadThrow
    , MonadCatch)
  via (ReaderT (EvalTEnv b i m) (StateT (EvalState b i) m))

runEvalT
  :: EvalTEnv b i m
  -> EvalState b i
  -> EvalT b i m a
  -> m (a, EvalState b i)
runEvalT env st (EvalT action) = runStateT (runReaderT action env) st

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
  | Args (CEKEnv b i m) (NonEmpty (EvalTerm b i)) (Cont b i m)
  -- ^ Continuation holding the arguments to evaluate in a function application
  | LetC (CEKEnv b i m) (EvalTerm b i) (Cont b i m)
  -- ^ Let single-variable pushing
  -- Known as a single argument it will not construct a needless closure
  | SeqC (CEKEnv b i m) (EvalTerm b i) (Cont b i m)
  -- ^ Sequencing expression, holding the next term to evaluate
  | ListC (CEKEnv b i m) [EvalTerm b i] [PactValue] (Cont b i m)
  -- ^ Continuation for list elements
  | CondC (CEKEnv b i m) (CondFrame b i) (Cont b i m)
  -- ^ Continuation for conditionals with lazy semantics
  | ObjC (CEKEnv b i m) Field [(Field, EvalTerm b i)] [(Field, PactValue)] (Cont b i m)
  -- ^ Continuation for the current object field being evaluated, and the already evaluated pairs
  | DynInvokeC (CEKEnv b i m) Text (Cont b i m)
  -- ^ Continuation for dynamic invocation of `m::f`
  | CapInvokeC (CEKEnv b i m) [EvalTerm b i] [PactValue] (CapFrame b i) (Cont b i m)
  -- ^ Capability special form frams that eva
  | CapBodyC (CEKEnv b i m) (EvalTerm b i) (Cont b i m)
  | CapPopC CapPopState (Cont b i m)
  | StackPopC (Maybe Type) (Cont b i m)
  | Mt
  deriving Show


data CEKErrorHandler b i m
  = CEKNoHandler
  | CEKHandler (CEKEnv b i m) (EvalTerm b i) (Cont b i m) [CapSlot QualifiedName PactValue] (CEKErrorHandler b i m)
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
makeLenses ''EvalTEnv

instance (MonadIO m) => MonadGas (EvalT b i m) where
  logGas msg g = do
    r <- EvalT $ view emGasLog
    liftIO $ modifyIORef' r (fmap ((msg, g):))

  chargeGas g = do
    r <- EvalT $ view emGas
    liftIO (modifyIORef' r (<> g))

-- instance (MonadIO m) => MonadEvalEnv b i (EvalT b i m) where
--   readEnv = EvalT $ view emRuntimeEnv

instance Monad m => MonadEvalState b i (EvalT b i m) where
  getEvalState = EvalT get
  putEvalState p = EvalT (put p)
  modifyEvalState f = EvalT (modify' f)

