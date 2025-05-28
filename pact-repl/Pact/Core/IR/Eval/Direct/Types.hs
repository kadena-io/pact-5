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

module Pact.Core.IR.Eval.Direct.Types
 ( ClosureType(..)
 , Closure(..)
 , LamClosure(..)
 , PartialClosure(..)
 , DefPactClosure(..)
 , CapTokenClosure(..)
 , PartialNativeFn(..)
 , NativeFn(..)
 , CanApply(..)
 , EvalValue(..)
 , DirectEnv(..)
 , ceLocal, ceDefPactStep
 , ceBuiltins, cePactDb
 , ceInCap, ceReentrant
 , pattern VLiteral
 , pattern VString
 , pattern VInteger
 , pattern VUnit
 , pattern VBool
 , pattern VDecimal
 , pattern VGuard
 , pattern VList
 , pattern VTime
 , pattern VObject
 , pattern VModRef
 , pattern VCapToken
 , pattern VNative
 , pattern VPartialNative
 , pattern VDefClosure
 , pattern VLamClosure
 , pattern VPartialClosure
 , pattern VDefPactClosure
 , pattern VTable
 , CapPopState(..)
 , NativeFunction
 , BuiltinEnv
 , toArgTypeError
 , argsError
 , mkDirectBuiltinFn
 , enforceSaturatedApp
 ) where

import Control.Lens
import GHC.Generics
import Control.DeepSeq
import Data.Text(Text)
import Data.Set(Set)
import Data.Decimal
import Data.List.NonEmpty(NonEmpty(..))
import Data.RAList(RAList)
import Data.Map.Strict(Map)
import Data.Vector(Vector)
import Pact.Time(UTCTime)
import qualified Data.Kind as K


import Pact.Core.IR.Eval.Runtime.Utils
import Pact.Core.IR.Term
import Pact.Core.Names
import Pact.Core.Environment
import Pact.Core.Type
import Pact.Core.Errors
import Pact.Core.PactValue
import Pact.Core.Persistence
import Pact.Core.Guards
import Pact.Core.Capabilities
import Pact.Core.DefPacts.Types
import Pact.Core.Literal
import Pact.Core.ModRefs
import Pact.Core.Builtin

data ClosureType i
  = NullaryClosure
  | ArgClosure !(NonEmpty (Arg Type i))
  deriving (Show, Generic)

instance NFData i => NFData (ClosureType i)

data Closure e b i
  = Closure
  { _cloFqName :: !FullyQualifiedName
  , _cloTypes :: !(ClosureType i)
  , _cloArity :: !Int
  , _cloTerm :: !(EvalTerm b i)
  , _cloRType :: !(Maybe Type)
  , _cloEnv :: !(DirectEnv e b i)
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
  , _lcloEnv :: !(DirectEnv e b i)
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
  , _pcloEnv :: !(DirectEnv e b i)
  , _pcloInfo :: i
  } deriving (Show, Generic)

instance (NFData b, NFData i) => NFData (PartialClosure e b i)

data DefPactClosure (e :: RuntimeMode) (b :: K.Type) (i :: K.Type)
  = DefPactClosure
  { _pactcloFQN :: !FullyQualifiedName
  , _pactcloTypes :: !(ClosureType i)
  , _pactcloArity :: !Int
  , _pactEnv :: !(DirectEnv e b i)
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

-- | A partially applied native because we don't allow
-- them to be applied at the lhs of an app since pact historically hasn't had partial closures.
-- This is a bit annoying to deal with but helps preserve semantics
data PartialNativeFn e b i
  = PartialNativeFn
  { _pNative :: !b
  , _pNativeEnv :: !(DirectEnv e b i)
  , _pNativeFn :: !(NativeFunction e b i)
  , _pNativeArity :: !Int
  , _pNativeAppliedArgs :: ![EvalValue e b i]
  , _pNativeLoc :: i
  } deriving (Generic)

data CanApply (e :: RuntimeMode) (b :: K.Type) (i :: K.Type)
  = C !(Closure e b i)
  | N !(NativeFn e b i)
  | CT !(CapTokenClosure i)
  | LC !(LamClosure e b i)
  | PC !(PartialClosure e b i)
  | PN !(PartialNativeFn e b i)
  | DPC !(DefPactClosure e b i)
  deriving (Show, Generic)


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

-- | The type of our semantic runtime values
data EvalValue e b i
  = VPactValue !PactValue
  -- ^ PactValue(s), which contain no terms
  -- value with
  | VClosure  !(CanApply e b i)
  -- ^ Closures, which may contain terms
  deriving (Generic)

instance (NFData b, NFData i) => NFData (EvalValue e b i)

instance Show (EvalValue e b i) where
  show = \case
    VPactValue pv -> show pv
    VClosure _ -> "closure<>"

-- | Locally bound variables and
--   our local read-only environment.
data DirectEnv e b i
  = DirectEnv
  { _ceLocal :: RAList (EvalValue e b i)
  , _cePactDb :: PactDb b i
  , _ceBuiltins :: BuiltinEnv e b i
  , _ceDefPactStep :: Maybe DefPactStep
  , _ceReentrant :: Set ModuleName
  , _ceInCap :: Bool }
  deriving (Generic)

instance (NFData b, NFData i) => NFData (DirectEnv e b i)


instance (Show i, Show b) => Show (DirectEnv e b i) where
  show (DirectEnv e _ _ _ _ _) = show e

type NativeFunction (e :: RuntimeMode) (b :: K.Type) (i :: K.Type)
  = i -> b -> DirectEnv e b i -> [EvalValue e b i] -> EvalM e b i (EvalValue e b i)

-- | List of builtins
type BuiltinEnv e b i
  = i -> b -> DirectEnv e b i -> NativeFn e b i

data NativeFn (e :: RuntimeMode) (b :: K.Type) (i :: K.Type)
  = NativeFn
  { _native :: !b
  , _nativeEnv :: !(DirectEnv e b i)
  , _nativeFn :: !(NativeFunction e b i)
  , _nativeArity :: !Int
  , _nativeLoc :: i
  } deriving (Generic)

pattern VLiteral :: Literal -> EvalValue e b i
pattern VLiteral lit = VPactValue (PLiteral lit)

pattern VString :: Text -> EvalValue e b i
pattern VString txt = VLiteral (LString txt)

pattern VInteger :: Integer -> EvalValue e b i
pattern VInteger txt = VLiteral (LInteger txt)

pattern VUnit :: EvalValue e b i
pattern VUnit = VLiteral LUnit

pattern VBool :: Bool -> EvalValue e b i
pattern VBool b = VLiteral (LBool b)

pattern VDecimal :: Decimal -> EvalValue e b i
pattern VDecimal d = VLiteral (LDecimal d)

pattern VGuard :: Guard QualifiedName PactValue -> EvalValue e b i
pattern VGuard g = VPactValue (PGuard g)

pattern VList :: Vector PactValue -> EvalValue e b i
pattern VList p = VPactValue (PList p)

pattern VTime :: UTCTime -> EvalValue e b i
pattern VTime p = VPactValue (PTime p)

pattern VObject :: Map Field PactValue -> EvalValue e b i
pattern VObject o = VPactValue (PObject o)

pattern VModRef :: ModRef -> EvalValue e b i
pattern VModRef mn = VPactValue (PModRef mn)

pattern VCapToken :: CapToken FullyQualifiedName PactValue -> EvalValue e b i
pattern VCapToken ct = VPactValue (PCapToken ct)

pattern VNative :: NativeFn e b i -> EvalValue e b i
pattern VNative clo = VClosure (N clo)

pattern VPartialNative :: PartialNativeFn e b i -> EvalValue e b i
pattern VPartialNative clo = VClosure (PN clo)

pattern VDefClosure :: Closure e b i -> EvalValue e b i
pattern VDefClosure clo = VClosure (C clo)

pattern VLamClosure :: LamClosure e b i -> EvalValue e b i
pattern VLamClosure clo = VClosure (LC clo)

pattern VPartialClosure :: PartialClosure e b i -> EvalValue e b i
pattern VPartialClosure clo = VClosure (PC clo)

pattern VDefPactClosure :: DefPactClosure e b i -> EvalValue e b i
pattern VDefPactClosure clo = VClosure (DPC clo)

pattern VTable :: TableValue -> EvalValue e b i
pattern VTable tv = VPactValue (PTable tv)

-- | What to do post-cap evaluation: do we pop the cap from the stack,
-- or compose it within the capset
data CapPopState
  = PopCapComposed
  | PopCapInvoke
  deriving (Eq, Show, Generic)


instance (NFData b, NFData i) => NFData (CanApply e b i)
instance (NFData b, NFData i) => NFData (NativeFn e b i)
instance (NFData b, NFData i) => NFData (PartialNativeFn e b i)

makeLenses ''DirectEnv

toArgTypeError :: EvalValue e b i -> ArgTypeError
toArgTypeError = \case
  VPactValue pv -> case pv of
    PLiteral l -> ATEPrim (literalPrim l)
    PTime _ -> ATEPrim PrimTime
    PList _ -> ATEList
    PObject _ -> ATEObject
    PGuard _ -> ATEPrim PrimGuard
    PModRef _ -> ATEModRef
    PCapToken _ -> ATEClosure
    PTable _ -> ATETable
  VClosure{} -> ATEClosure

argsError
  :: IsBuiltin b
  => i
  -> b
  -> [EvalValue e b i]
  -> EvalM e b i a
argsError info b args =
  throwExecutionError info (NativeArgumentsError (builtinName b) (toArgTypeError <$> args))

mkDirectBuiltinFn
  :: (IsBuiltin b)
  => i
  -> b
  -> DirectEnv e b i
  -> NativeFunction e b i
  -> NativeFn e b i
mkDirectBuiltinFn i b env fn =
  NativeFn b env fn (builtinArity b) i
{-# INLINE mkDirectBuiltinFn #-}

invalidArgs
  :: i
  -> ErrorClosureType
  -> Int
  -> Int
  -> EvalM e b i a
invalidArgs info mn expected actual =
  throwExecutionError info $ InvalidNumArgs mn expected actual

enforceSaturatedApp :: IsBuiltin b => i -> EvalValue e b i -> EvalM e b i ()
enforceSaturatedApp info = \case
  VPactValue _ -> pure ()
  VClosure clo -> case clo of
    PC pc ->
      invalidArgs info (maybe ErrClosureLambda ErrClosureUserFun (_sfName <$> _pcloFrame pc)) (_pcloArity pc + _pcloNArgs pc) (_pcloNArgs pc)
    PN pn ->
      let nargs = length (_pNativeAppliedArgs pn)
      in invalidArgs info (ErrClosureNativeFun (builtinName (_pNative pn))) (_pNativeArity pn + nargs) nargs
    _ -> pure ()
{-# INLINE enforceSaturatedApp #-}
