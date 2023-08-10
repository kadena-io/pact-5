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

module Pact.Core.IR.Eval.Runtime
 ( CEKTLEnv
 , CEKEnv
 , CEKRuntimeEnv(..)
 , BuiltinFn(..)
 , EvalT(..)
 , runEvalT
 , CEKValue(..)
 , Cont(..)
 , mkBuiltinFn
 , cekBuiltins
 , cekLoaded
 , cekGasModel
 , cekMHashes, cekMsgSigs
 , fromPactValue
 , checkPactValueType
 , pactToCEKValue
 , cfFQN
 , CEKErrorHandler(..)
 , MonadEvalEnv(..)
 , MonadEvalState(..)
 , CondFrame(..)
 , MonadEval
 , Closure(..)
 , EvalResult(..)
 , EvalEnv(..)
 , EvalState(..)
 , esCaps, esEvents, esInCap
 , esStack
 , pattern VString
 , pattern VInteger
 , pattern VDecimal
 , pattern VUnit
 , pattern VBool
 -- Capabilities
 , CapToken(..)
 , ctName, ctArgs
 , CapSlot(..)
 , csCap, csComposed
 , CapFrame(..)
 , CapState(..)
 , csSlots, csManaged
 , ManagedCap(..)
 , mcCap, mcManaged
 , ManagedCapType(..)
 , PactEvent(..)
 , CapPopState(..)
 , CanApply(..)
 ) where

import Control.Lens
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Void
import Data.List.NonEmpty(NonEmpty)
import Data.Text(Text)
import Data.Map.Strict(Map)
import Data.Default
import Data.Decimal(Decimal)
-- import Data.Set(Set)
import Data.Vector(Vector)
import Data.RAList(RAList)
import Data.Set(Set)
import Data.IORef
import qualified Data.Vector as V

import Pact.Core.Names
import Pact.Core.Guards
import Pact.Core.Pretty(Pretty(..), (<+>))
import Pact.Core.Gas
import Pact.Core.PactValue
import Pact.Core.Errors
import Pact.Core.Builtin
import Pact.Core.Hash
import Pact.Core.IR.Term
import Pact.Core.Literal
import Pact.Core.Type
import qualified Pact.Core.Pretty as P


-- | The top level env map
type CEKTLEnv b i = Map FullyQualifiedName (EvalDef b i)

-- | Locally bound variables
type CEKEnv b i m = RAList (CEKValue b i m)

-- | List of builtins
type BuiltinEnv b i m = b -> BuiltinFn b i m

data StackFrame
  = StackFrame
  { _sfModule :: ModuleName
  , _sfLamInfo :: LamInfo
  , _sfApp :: [PactValue]
  } deriving Show

data Closure b i m
  = Closure
  { _cloLamInfo :: !LamInfo
  , _cloTypes :: !(NonEmpty (Maybe (Type Void)))
  , _cloTerm :: !(EvalTerm b i)
  , _cloEnv :: !(CEKEnv b i m)
  } deriving Show

data CanApply b i m
  = C {-# UNPACK #-} !(Closure b i m)
  | N {-# UNPACK #-} !(BuiltinFn b i m)
  deriving Show

-- | The type of our semantic runtime values
data CEKValue b i m
  = VLiteral !Literal
  | VList !(Vector (CEKValue b i m))
  | VClosure {-# UNPACK #-} !(Closure b i m)
  | VNative {-# UNPACK #-} !(BuiltinFn b i m)
  | VModRef ModuleName [ModuleName]
  | VGuard !(Guard FullyQualifiedName PactValue)

instance Show (CEKValue b i m) where
  show = \case
    VLiteral lit -> show lit
    VList vec -> show vec
    VClosure _ -> "closure<>"
    VNative _ -> "native<>"
    VModRef mn mns -> "modRef" <> show mn <> show mns
    VGuard _ -> "guard<>"

pactToCEKValue :: PactValue -> CEKValue b i m
pactToCEKValue = \case
  PLiteral lit -> VLiteral lit
  PList vec -> VList (pactToCEKValue <$> vec)
  PGuard gu -> VGuard gu
  PModRef mn ifs -> VModRef mn ifs

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

-- | Result of an evaluation step, either a CEK value or an error.
data EvalResult b i m
  = EvalValue (CEKValue b i m)
  | VError Text
  deriving Show

data EvalState b i
  = EvalState
  { _esCaps :: CapState
  , _esStack :: [StackFrame]
  , _esEvents :: [PactEvent b i]
  , _esInCap :: Bool
  } deriving Show

type MonadEval b i m = (MonadEvalEnv b i m, MonadEvalState b i m, MonadError (PactError i) m, Default i)

class (Monad m) => MonadEvalEnv b i m | m -> b, m -> i where
  cekReadEnv :: m (CEKRuntimeEnv b i m)
  cekLogGas :: Text -> Gas -> m ()
  cekChargeGas :: Gas -> m ()

class Monad m => (MonadEvalState b i m) | m -> b, m -> i where
  setCekState :: Lens' (EvalState b i) s -> s -> m ()
  modifyCEKState :: Lens' (EvalState b i) s -> (s -> s) -> m ()
  useCekState :: Lens' (EvalState b i) s -> m s
  usesCekState :: Lens' (EvalState b i) s -> (s -> s') -> m s'

data EvalEnv b i m
  = EvalEnv
  { _emRuntimeEnv :: CEKRuntimeEnv b i (EvalT b i m)
  , _emGas :: IORef Gas
  , _emGasLog :: IORef (Maybe [(Text, Gas)])
  }

-- Todo: are we going to inject state as the reader monad here?
newtype EvalT b i m a =
  EvalT (ReaderT (EvalEnv b i m) (StateT (EvalState b i) m) a)
  deriving
    ( Functor, Applicative, Monad
    , MonadIO
    , MonadThrow
    , MonadCatch)
  via (ReaderT (EvalEnv b i m) (StateT (EvalState b i) m))

runEvalT
  :: EvalEnv b i m
  -> EvalState b i
  -> EvalT b i m a
  -> m (a, EvalState b i)
runEvalT env st (EvalT action) = runStateT (runReaderT action env) st

data BuiltinFn b i m
  = BuiltinFn
  { _native :: b
  , _nativeFn :: Cont b i m -> CEKErrorHandler b i m -> [CEKValue b i m] -> m (EvalResult b i m)
  , _nativeArity :: {-# UNPACK #-} !Int
  , _nativeAppliedArgs :: [CEKValue b i m]
  }

mkBuiltinFn
  :: (BuiltinArity b)
  => (Cont b i m -> CEKErrorHandler b i m -> [CEKValue b i m] -> m (EvalResult b i m))
  -> b
  -> BuiltinFn b i m
mkBuiltinFn fn b =
  BuiltinFn b fn (builtinArity b) []
{-# INLINE mkBuiltinFn #-}

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
  , _mcManaged :: ManagedCapType
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
  deriving Show

cfFQN :: Lens' (CapFrame b i) FullyQualifiedName
cfFQN f = \case
  WithCapFrame fqn b -> (`WithCapFrame` b) <$> f fqn
  RequireCapFrame fqn -> RequireCapFrame <$> f fqn
  ComposeCapFrame fqn -> ComposeCapFrame <$> f fqn
  InstallCapFrame fqn -> InstallCapFrame <$> f fqn
  EmitEventFrame fqn -> EmitEventFrame <$> f fqn

data CapPopState
  = PopCapComposed
  | PopCapInvoke
  deriving (Eq, Show)

data Cont b i m
  = Fn (CanApply b i m) (CEKEnv b i m) [EvalTerm b i] [CEKValue b i m] (Cont b i m)
  | Args (CEKEnv b i m) (NonEmpty (EvalTerm b i)) (Cont b i m)
  | SeqC (CEKEnv b i m) (EvalTerm b i) (Cont b i m)
  | ListC (CEKEnv b i m) [EvalTerm b i] [CEKValue b i m] (Cont b i m)
  | CondC (CEKEnv b i m) (CondFrame b i) (Cont b i m)
  | DynInvokeC (CEKEnv b i m) Text (Cont b i m)
  | CapInvokeC (CEKEnv b i m) [EvalTerm b i] [PactValue] (CapFrame b i) (Cont b i m)
  | CapBodyC (CEKEnv b i m) (EvalTerm b i) (Cont b i m)
  | CapPopC CapPopState (Cont b i m)
  | StackPopC (Cont b i m)
  | Mt
  deriving Show


data CEKErrorHandler b i m
  = CEKNoHandler
  | CEKHandler (CEKEnv b i m) (EvalTerm b i) (Cont b i m) [CapSlot] (CEKErrorHandler b i m)
  deriving Show

data CEKRuntimeEnv b i m
  = CEKRuntimeEnv
  { _cekBuiltins :: BuiltinEnv b i m
  , _cekGasModel :: GasEnv b
  , _cekLoaded :: CEKTLEnv b i
  , _cekMHashes :: Map ModuleName ModuleHash
  , _cekMsgSigs :: Map PublicKeyText (Set CapToken)
  --   _cekGas :: IORef Gas
  -- , _cekEvalLog :: IORef (Maybe [(Text, Gas)])
  -- , _ckeData :: EnvData PactValue
  -- , _ckeTxHash :: Hash
  -- , _ckeResolveName :: QualifiedName -> Maybe FullyQualifiedName
  -- , _ckeSigs :: Set PublicKey
  -- , _ckePactDb :: PactDb b i
  }

instance (Show i, Show b) => Show (BuiltinFn b i m) where
  show (BuiltinFn b _ arity _) = unwords
    ["(BuiltinFn"
    , show b
    , "#fn"
    , show arity
    , ")"
    ]

instance (Pretty b, Show i, Show b) => Pretty (BuiltinFn b i m) where
  pretty = pretty . show

instance (Show i, Show b, Pretty b) => Pretty (CEKValue b i m) where
  pretty = \case
    VLiteral i ->
      pretty i
    VList v ->
      P.brackets $ P.hsep (P.punctuate P.comma (V.toList (pretty <$> v)))
    VClosure{} ->
      P.angles "closure#"
    VNative b ->
      P.angles $ "native" <+> pretty b
    VGuard _ -> P.angles "guard#"
    VModRef mn _ ->
      "modref" <> P.braces (pretty mn)

makeLenses ''CEKRuntimeEnv

fromPactValue :: PactValue -> CEKValue b i m
fromPactValue = \case
  PLiteral lit -> VLiteral lit
  PList vec -> VList (fromPactValue <$> vec)
  PGuard gu ->
    VGuard gu
  PModRef mn ifs -> VModRef mn ifs

checkPactValueType :: Type Void -> PactValue -> Bool
checkPactValueType ty = \case
  PLiteral lit -> typeOfLit lit == ty
  PList vec -> case ty of
    TyList t -> V.null vec || all (checkPactValueType t) vec
    _ -> False
  PGuard _ -> ty == TyGuard
  PModRef _ ifs -> case ty of
    TyModRef m -> m `elem` ifs
    _ -> False

makeLenses ''EvalEnv
makeLenses ''EvalState
makeLenses ''CapState
makeLenses ''CapToken
makeLenses ''CapSlot
makeLenses ''ManagedCap

instance (MonadIO m) => MonadEvalEnv b i (EvalT b i m) where
  cekReadEnv = EvalT $ view emRuntimeEnv
  cekLogGas msg g = do
    r <- EvalT $ view emGasLog
    liftIO $ modifyIORef' r (fmap ((msg, g):))
  cekChargeGas g = do
    r <- EvalT $ view emGas
    liftIO (modifyIORef' r (<> g))

instance Monad m => MonadEvalState b i (EvalT b i m) where
  setCekState l s = EvalT $ l .= s
  modifyCEKState l f = EvalT (l %= f)
  useCekState l = EvalT (use l)
  usesCekState l f = EvalT (uses l f)
