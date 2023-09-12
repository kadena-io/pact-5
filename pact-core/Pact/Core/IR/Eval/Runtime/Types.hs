{-# LANGUAGE LambdaCase #-}
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

module Pact.Core.IR.Eval.Runtime.Types
 ( CEKTLEnv
 , CEKEnv
 , EvalEnv(..)
 , NativeFn(..)
 , EvalT(..)
 , runEvalT
 , CEKValue(..)
 , Cont(..)
 , eeBuiltins
 , eeLoaded
 , eeGasModel
 , eeMHashes, eeMsgSigs
 , eePactDb
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
 , EvalState(..)
 , esCaps, esEvents, esInCap, esPactExec
 , peStepCount, peYield, peStep, peStepHasRollback, peContinuation -- pePactId
 , esStack
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
 -- Capabilities
 , CapToken(..)
 , ctName, ctArgs
 , CapSlot(..)
 , csCap, csComposed
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
 , CanApply(..)
 , StackFrame(..)
 -- defpact
 , PactExec(..)
 , Yield(..)
 , DefPactClosure(..)
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
import Data.Set(Set)
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
import Pact.Core.Pacts.Types
import qualified Pact.Core.Pretty as P


-- | The top level env map
type CEKTLEnv b i = Map FullyQualifiedName (EvalDef b i)

-- | Locally bound variables
type CEKEnv b i m = RAList (CEKValue b i m)

-- | List of builtins
type BuiltinEnv b i m = i -> b -> NativeFn b i m

newtype StackFrame
  = StackFrame
  { _sfLamInfo :: LamInfo }
  deriving Show

data Closure b i
  = Closure
  { _cloLamInfo :: !LamInfo
  , _cloTypes :: !(NonEmpty (Maybe Type))
  , _cloArity :: !Int
  , _cloTerm :: !(EvalTerm b i)
  , _cloRType :: !(Maybe Type)
  , _cloInfo :: i
  } deriving Show

-- | A closure coming from a lambda application with its accompanying environment capturing args,
-- but is not partially applied
data LamClosure b i m
  = LamClosure
  { _lcloLamInfo :: !LamInfo
  , _lcloTypes :: !(NonEmpty (Maybe Type))
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
  { _pcloLamInfo :: !LamInfo
  , _pcloTypes :: !(NonEmpty (Maybe Type))
  , _pcloArity :: Int
  , _pcloTerm :: !(EvalTerm b i)
  , _pcloRType :: !(Maybe Type)
  , _pcloEnv :: !(CEKEnv b i m)
  , _pcloInfo :: i
  } deriving Show

data DefPactClosure b i
  = DefPactClosure
  { _pactcloFQN :: FullyQualifiedName
  , _pactcloLamInfo :: !LamInfo
  , _pactcloTypes :: !(NonEmpty (Maybe Type))
  , _pactcloArity :: Int
  , _pactcloTerm :: !(EvalTerm b i)
  , _pactcloHasRollback :: Bool
  , _pactcloStepCount :: Int
  , _pactcloRType :: !(Maybe Type)
  , _pactcloInfo :: i
  } deriving Show

data CanApply b i m
  = C {-# UNPACK #-} !(Closure b i)
  | LC {-# UNPACK #-} !(LamClosure b i m)
  | PC {-# UNPACK #-} !(PartialClosure b i m)
  | N {-# UNPACK #-} !(NativeFn b i m)
  | PN {-# UNPACK #-} !(PartialNativeFn b i m)
  | DPC {-# UNPACK #-} !(DefPactClosure b i)
  deriving Show

-- | The type of our semantic runtime values
data CEKValue b i m
  = VPactValue PactValue
  | VTable TableName ModuleName ModuleHash Schema
  -- = VLiteral !Literal
  -- | VList !(Vector (CEKValue b i m))
  | VClosure {-# UNPACK #-} !(CanApply b i m)
  -- | VModRef ModuleName [ModuleName]
  -- | VGuard !(Guard FullyQualifiedName PactValue)

instance Show (CEKValue b i m) where
  show = \case
    VPactValue pv -> show pv
    VTable tn _ _ _sc -> "table" <> show tn
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

pattern VNative :: NativeFn b i m -> CEKValue b i m
pattern VNative clo = VClosure (N clo)

pattern VPartialNative :: PartialNativeFn b i m -> CEKValue b i m
pattern VPartialNative clo = VClosure (PN clo)

pattern VDefClosure :: Closure b i -> CEKValue b i m
pattern VDefClosure clo = VClosure (C clo)

pattern VLamClosure :: LamClosure b i m -> CEKValue b i m
pattern VLamClosure clo = VClosure (LC clo)

pattern VPartialClosure :: PartialClosure b i m -> CEKValue b i m
pattern VPartialClosure clo = VClosure (PC clo)

pattern VDefPactClosure :: DefPactClosure b i -> CEKValue b i m
pattern VDefPactClosure clo = VClosure (DPC clo)

-- | Result of an evaluation step, either a CEK value or an error.
data EvalResult b i m
  = EvalValue (CEKValue b i m)
  | VError Text
  deriving Show

-- | `PactId` representing pact identifiers
-- newtype PactId
--   = PactId {unPactId :: Text}
--   deriving (Eq, Show)

-- | `Yield` representing an object
newtype Yield
  = Yield {unYield :: Map Field PactValue}
  deriving (Show)

-- | Internal representation of pacts
data PactExec
  = PactExec
  { _peStepCount :: Int
  , _peYield :: Maybe Yield
  , _peStep :: Int
  -- , _pePactId :: PactId
  , _peContinuation :: PactContinuation FullyQualifiedName PactValue
  , _peStepHasRollback :: Bool
--  , _peNestedPactExec :: Map PactId NestedPactExec
  } deriving Show

newtype NestedPactExec
  = NestedPactExec PactExec
  deriving Show

data EvalState b i
  = EvalState
  { _esCaps :: CapState
  , _esStack :: [StackFrame]
  , _esEvents :: [PactEvent b i]
  , _esInCap :: Bool
  , _esPactExec :: Maybe PactExec
  } deriving Show


type MonadEval b i m = (MonadEvalEnv b i m, MonadEvalState b i m, MonadGas m, MonadError (PactError i) m, MonadIO m, Default i)

class Monad m => MonadGas m where
  logGas :: Text -> Gas -> m ()
  chargeGas :: Gas -> m ()


class (Monad m) => MonadEvalEnv b i m | m -> b, m -> i where
  readEnv :: m (EvalEnv b i m)

-- | Our monad mirroring `EvalState` for our evaluation state
class Monad m => MonadEvalState b i m | m -> b, m -> i where
  getEvalState :: m (EvalState b i)
  putEvalState :: EvalState b i -> m ()
  modifyEvalState :: (EvalState b i -> EvalState b i) -> m ()


data EvalTEnv b i m
  = EvalTEnv
  { _emRuntimeEnv :: EvalEnv b i (EvalT b i m)
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

data NativeFn b i m
  = NativeFn
  { _native :: b
  , _nativeFn :: Cont b i m -> CEKErrorHandler b i m -> [CEKValue b i m] -> m (EvalResult b i m)
  , _nativeArity :: {-# UNPACK #-} !Int
  , _nativeLoc :: i
  }

-- | A partially applied native because we don't allow
-- them to be applied at the lhs of an app since pact historically hasn't had partial closures.
-- This is a bit annoying to deal with but helps preserve semantics
data PartialNativeFn b i m
  = PartialNativeFn
  { _pNative :: b
  , _pNativeFn :: Cont b i m -> CEKErrorHandler b i m -> [CEKValue b i m] -> m (EvalResult b i m)
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

data CapToken
  = CapToken
  { _ctName :: FullyQualifiedName
  , _ctArgs :: [PactValue]
  } deriving (Show, Eq, Ord)

data CapSlot
 = CapSlot
 { _csCap :: CapToken
 , _csComposed :: [CapToken]
 } deriving (Show, Eq)

data PactEvent b i
  = PactEvent
  { _peToken :: CapToken
  , _peModule :: ModuleName
  , _peModuleHash :: ModuleHash
  } deriving (Show, Eq)

data ManagedCapType
  = AutoManaged Bool
  | ManagedParam FullyQualifiedName PactValue Int
  -- ^ managed cap, with manager function, managed value
  deriving Show

data ManagedCap
  = ManagedCap
  { _mcCap :: CapToken
  -- ^ The token without the managed param
  , _mcOriginalCap :: CapToken
  -- ^ The original, installed token
  , _mcManaged :: ManagedCapType
  -- ^ Managed capability type
  } deriving (Show)

instance Eq ManagedCap where
  l == r = _mcCap l == _mcCap r

instance Ord ManagedCap where
  l `compare` r = _mcCap l `compare` _mcCap r

-- | The overall capability state
data CapState
  = CapState
  { _csSlots :: [CapSlot]
  , _csManaged :: Set ManagedCap
  }
  deriving Show

data CapFrame b i
  = WithCapFrame FullyQualifiedName (EvalTerm b i)
  | RequireCapFrame FullyQualifiedName
  | ComposeCapFrame FullyQualifiedName
  | InstallCapFrame FullyQualifiedName
  | EmitEventFrame FullyQualifiedName
  | CreateUserGuardFrame FullyQualifiedName
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
  | PactStepC (Cont b i m)
  | Mt
  deriving Show


data CEKErrorHandler b i m
  = CEKNoHandler
  | CEKHandler (CEKEnv b i m) (EvalTerm b i) (Cont b i m) [CapSlot] (CEKErrorHandler b i m)
  deriving Show

data EvalEnv b i m
  = EvalEnv
  { _eeBuiltins :: BuiltinEnv b i m
  , _eeGasModel :: GasEnv b
  , _eeLoaded :: CEKTLEnv b i
  , _eeMHashes :: Map ModuleName ModuleHash
  , _eeMsgSigs :: Map PublicKeyText (Set CapToken)
  , _eePactDb :: PactDb b i
  -- , _eePactStep :: Maybe (PactStep Name Type b i) -- TODO: Is this actually needed?
  --   _cekGas :: IORef Gas
  -- , _cekEvalLog :: IORef (Maybe [(Text, Gas)])
  -- , _ckeData :: EnvData PactValue
  -- , _ckeTxHash :: Hash
  -- , _ckeResolveName :: QualifiedName -> Maybe FullyQualifiedName
  -- , _ckeSigs :: Set PublicKey
  -- , _ckePactDb :: PactDb b i
  }

instance (Show i, Show b) => Show (NativeFn b i m) where
  show (NativeFn b _ arity _) = unwords
    ["(NativeFn"
    , show b
    , "#fn"
    , show arity
    , ")"
    ]

instance (Show i, Show b) => Show (PartialNativeFn b i m) where
  show (PartialNativeFn b _ arity _ _) = unwords
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
    VTable tn _ _ _sc -> "table" <> P.braces (pretty tn)
    VClosure{} ->
      P.angles "closure#"

makeLenses ''PactExec
makeLenses ''EvalEnv
makeLenses ''EvalTEnv
makeLenses ''EvalState
makeLenses ''CapState
makeLenses ''CapToken
makeLenses ''CapSlot
makeLenses ''ManagedCap

instance (MonadIO m) => MonadGas (EvalT b i m) where
  logGas msg g = do
    r <- EvalT $ view emGasLog
    liftIO $ modifyIORef' r (fmap ((msg, g):))

  chargeGas g = do
    r <- EvalT $ view emGas
    liftIO (modifyIORef' r (<> g))

instance (MonadIO m) => MonadEvalEnv b i (EvalT b i m) where
  readEnv = EvalT $ view emRuntimeEnv

instance Monad m => MonadEvalState b i (EvalT b i m) where
  getEvalState = EvalT get
  putEvalState p = EvalT (put p)
  modifyEvalState f = EvalT (modify' f)

