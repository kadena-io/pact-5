{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE InstanceSigs #-}


-- |
-- Module      :  Pact.Core.IR.Typecheck
-- Copyright   :  (C) 2022 Kadena
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jose Cardona <jose@kadena.io>
--
-- HM type inference for core IR.
--
module Pact.Core.Typed.Infer
 ( runInferTerm
 , runInferTopLevel
 , runInferModule
 , runInferReplTopLevel
 , TypeOfBuiltin(..)
 , TCState(..)
 ) where

import Control.Lens hiding (Level)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.ST
-- import Control.Monad.ST.Unsafe(unsafeIOToST, unsafeSTToIO)
import Control.Monad.State.Strict
import Control.Monad.Except
-- import Control.Exception(throwIO, catch)
import Data.Void
-- import Data.Dynamic (Typeable)
import Data.RAList(RAList)
import Data.Foldable(traverse_, foldlM)
import Data.Default
import Data.STRef
import Data.Map(Map)
import Data.Text(Text)

import qualified Data.Map.Strict as M
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.RAList as RAList
import qualified Data.Set as Set

import Pact.Core.Builtin
import Pact.Core.Names
import Pact.Core.Persistence
import Pact.Core.Capabilities
import Pact.Core.ModRefs
import Pact.Core.Typed.Term
import Pact.Core.Typed.Type
import Pact.Core.PactValue
import Pact.Core.Hash

import qualified Pact.Core.IR.Term as IR
import qualified Pact.Core.Type as IR

-- inference based on https://okmij.org/ftp/ML/generalization.html
-- Note: Type inference levels in the types
-- a-la sound-lazy might be worth implementing later on.
-- The eager implementation is simpler to maintain and extend to typeclasses.

-- note, we will debruijnize, so this is purely for
-- Display purposes
type UniqueSupply s = STRef s Unique
type Level = Int
type TCTypeVar s = TypeVar (TvRef s)
type TCType s = Type (TCTypeVar s)
type TCPred s = Pred (Type (TCTypeVar s))
type TCRowCtor s = RowCtor (TCTypeVar s)

data Tv s
  = Unbound !Text !Unique !Level
  | Bound !Text !Unique
  | LinkTy !(TCType s)
  | LinkRow !(RowCtor (TCTypeVar s))
  | LinkCap !(CapRef (TCTypeVar s))
  deriving (Eq)

-- Note: TyVar equality
-- is reference equality
newtype TvRef s =
  TvRef (STRef s (Tv s))
  deriving (Eq)


data TCEnv s b i
  = TCEnv
  { _tcSupply :: UniqueSupply s
  -- ^ Supply for fresh variables.
  , _tcVarEnv :: RAList (TCType s)
  -- ^ Builtins map, that uses the enum instance
  , _tcLevel :: STRef s Level
  -- ^ Type Variable "Region"
  , _tcLoaded :: Loaded b i
  }

makeLenses ''TCEnv

data TCState b i
  = TCState
  { _tcFree :: Map FullyQualifiedName (Type Void)
  , _tcInterfaces :: Map ModuleName (Interface Name Void b i)
  }
  deriving Show
  -- ^ Free variables

instance Default (TCState b i) where
  def = TCState mempty mempty

makeLenses ''TCState

-- | Term emitted by desugar
type IRTerm b i = IR.Term Name IR.Type b i
type IRTopLevel b i = IR.TopLevel Name IR.Type b i
type IRModule b i = IR.Module Name IR.Type b i
type IRInterface b i = IR.Interface Name b i

-- | Term emitted by the typechecker prior to final generalization/unification.
type TCTerm s b i = Term Name (TCTypeVar s) (b, [TCType s], [TCPred s]) i

-- Term/defun outputs post typechecking
-- with ST monad existential removed
type TypedTerm b i = OverloadedTerm NamedDeBruijn b i

type TypedGenTerm b i = OverloadedTerm NamedDeBruijn b i

type TypedDefun b i = OverloadedDefun NamedDeBruijn b i

type TypedDefCap b i = OverloadedDefCap NamedDeBruijn b i

type TypedDefConst b i = OverloadedDefConst NamedDeBruijn b i

type TypedDef b i = OverloadedDef NamedDeBruijn b i

type TypedIfDef b i = OverloadedIfDef NamedDeBruijn b i

type TypedTopLevel b i = OverloadedTopLevel NamedDeBruijn b i

type TypedReplTopLevel b i = OverloadedReplTopLevel NamedDeBruijn b i

type TypedModule b i = OverloadedModule NamedDeBruijn b i

type TypedInterface b i = OverloadedInterface NamedDeBruijn b i

data TypecheckFailure i
  = TypecheckFailure Text i
  deriving Show

-- | Our inference monad, where we can plumb through generalization "regions",
-- our variable environment and our "supply" of unique names
newtype InferM s b i a =
  InferM (ExceptT (TypecheckFailure i) (ReaderT (TCEnv s b i) (StateT (TCState b i) (ST s))) a)
  deriving
    ( Functor, Applicative, Monad
    , MonadReader (TCEnv s b i)
    , MonadState (TCState b i)
    , MonadError (TypecheckFailure i))
  via (ExceptT (TypecheckFailure i) (ReaderT (TCEnv s b i) (StateT (TCState b i) (ST s))))

class TypeOfBuiltin b where
  typeOfBuiltin :: b -> TypeScheme DebruijnTypeVar

instance TypeOfBuiltin CoreBuiltin where
  typeOfBuiltin = \case
    -- Add
    -- (+) : forall a. Add a => a -> a -> a
    CoreAdd ->
      addBinopType
    -- Num
    -- (-) : forall a. Num a => a -> a -> a
    -- (*) : forall a. Num a => a -> a -> a
    -- (/) : forall a. Num a => a -> a -> a
    -- negate : forall a. Num a => a -> a
    -- abs : forall a. Num a => a -> a
    -- (^) : forall a. Num a => a -> a -> a
    CoreSub ->
      numBinopType
    CoreMultiply ->
      numBinopType
    CoreDivide ->
      numBinopType
    CoreNegate ->
      unaryNumType
    CoreAbs ->
      unaryNumType
    CorePow ->
      numBinopType
    -- Boolean ops
    -- not : bool -> bool
    CoreNot ->
      NonGeneric (TyBool :~> TyBool)
    -- Equality
    -- (=) : forall a. Eq a => a -> a -> bool
    -- (!=) : forall a. Eq a => a -> a -> bool
    CoreEq ->
      eqTyp
    CoreNeq ->
      eqTyp
    -- Ord
    -- (>) : forall a. Ord a => a -> a -> bool
    -- (>=) : forall a. Ord a => a -> a -> bool
    -- (<) : forall a. Ord a => a -> a -> bool
    -- (<=) : forall a. Ord a => a -> a -> bool
    CoreGT ->
      ordTyp
    CoreGEQ ->
      ordTyp
    CoreLT ->
      ordTyp
    CoreLEQ ->
      ordTyp
    -- Integer ops
    -- (&) : integer -> integer -> integer
    -- (|) : integer -> integer -> integer
    -- (xor) : integer -> integer -> integer
    -- (~) : integer -> integer -> integer
    -- shift : integer -> integer -> integer
    -- mod : integer -> integer -> integer
    CoreBitwiseAnd ->
      binaryInt
    CoreBitwiseOr ->
      binaryInt
    CoreBitwiseXor ->
      binaryInt
    CoreBitwiseFlip ->
      unaryInt
    CoreBitShift ->
      binaryInt
    CoreMod ->
      binaryInt
    -- Rounding functions
    -- round : decimal -> integer
    -- ceiling : decimal -> integer
    -- floor : decimal -> integer
    --
    -- round-prec : decimal -> integer -> decimal
    -- ceiling-prec : decimal -> integer -> decimal
    -- floor-prec : decimal -> integer -> decimal
    CoreRound -> roundingFn
    CoreCeiling -> roundingFn
    CoreFloor -> roundingFn
    CoreRoundPrec -> roundingPrecFn
    CoreCeilingPrec -> roundingPrecFn
    CoreFloorPrec -> roundingPrecFn
    -- Fractional
    -- exp : forall a. Fractional a -> a -> decimal
    -- ln : forall a. Fractional a -> a -> decimal
    -- sqrt : forall a. Fractional a -> a -> decimal
    -- log-base : forall a. Fractional a -> a -> a -> decimal
    CoreExp ->
      unaryFractional
    CoreLn ->
      unaryFractional
    CoreSqrt ->
      unaryFractional
    CoreLogBase ->
      let aVar = nd "a" 0
          a = TyVar aVar
      in TypeScheme [aVar] [Pred (Fractional a)] (a :~> a :~> a)
    -- ListLike
    CoreLength ->
      let aVar = nd "a" 0
          a = TyVar aVar
      in TypeScheme [aVar] [Pred (ListLike a)] (a :~> TyInt)
    CoreTake -> takeDropTy
    CoreDrop -> takeDropTy
    CoreConcat ->
      NonGeneric (TyList TyString :~> TyString)
    CoreReverse ->
      let aVar = nd "a" 0
          a = TyVar aVar
      in TypeScheme [aVar] [] (TyList a :~> TyList a)
    -- general
    CoreMap ->
      let aVar = nd "a" 1
          bVar = nd "b" 0
          a = TyVar aVar
          b = TyVar bVar
      in TypeScheme [aVar, bVar] [] ((a :~> b) :~> TyList a :~> TyList b)
    CoreFilter ->
      let aVar = nd "a" 0
          a = TyVar aVar
      in TypeScheme [aVar] [] ((a :~> TyBool) :~> TyList a :~> TyList a)
    CoreContains ->
      let aVar = nd "a" 0
          a = TyVar aVar
      in TypeScheme [aVar] [Pred (Eq a)] (a :~> TyList a :~> TyBool)
    CoreSort ->
      let aVar = nd "a" 0
          a = TyVar aVar
      in TypeScheme [aVar] [Pred (Ord a)] (TyList a :~> TyList a)
    CoreSortObject ->
      error "todo: sort-object not supported by the typechecker"
    CoreRemove ->
      error "todo: remove not supported by the typechecker"
    -- CoreIf -> "if"
    CoreIntToStr ->
      NonGeneric (TyInt :~> TyInt :~> TyString)
    CoreStrToInt ->
      NonGeneric (TyString :~> TyInt)
    CoreStrToIntBase ->
      NonGeneric (TyInt :~> TyString :~> TyInt)
    CoreFold ->
      let aVar = nd "a" 1
          bVar = nd "b" 0
          a = TyVar aVar
          b = TyVar bVar
      in TypeScheme [aVar, bVar] [] ((a :~> b :~> a) :~> a :~> TyList b :~> a)
    CoreZip ->
      let aVar = nd "a" 2
          a = TyVar aVar
          bVar = nd "b" 1
          b = TyVar bVar
          cVar = nd "c" 0
          c = TyVar cVar
      in TypeScheme [aVar, bVar, cVar] [] ((a :~> b :~> c) :~> TyList a :~> TyList b :~> TyList c)
    CoreDistinct ->
      let aVar = nd "a" 0
          a = TyVar aVar
      in TypeScheme [aVar] [Pred (Ord a)] (TyList a :~> TyList a)
    CoreFormat ->
      error "todo: format not supported"
    CoreEnumerate ->
      NonGeneric (TyInt :~> TyInt :~> TyList TyInt)
    CoreEnumerateStepN ->
      NonGeneric (TyInt :~> TyInt :~> TyList TyInt)
    CoreShow ->
      let aVar = nd "a" 0
          a = TyVar aVar
      in TypeScheme [aVar] [Pred (Ord a)] (TyList a :~> TyList a)
    CoreReadMsg ->
      let aVar = nd "a" 0
          a = TyVar aVar
      in TypeScheme [aVar] [] (TyString :~> a)
    CoreReadMsgDefault ->
      let aVar = nd "a" 0
          a = TyVar aVar
      in TypeScheme [aVar] [] (TyNullary a)
    CoreReadInteger ->
      NonGeneric (TyString :~> TyInt)
    CoreReadDecimal ->
      NonGeneric (TyString :~> TyDecimal)
    CoreReadString ->
      NonGeneric (TyString :~> TyString)
    CoreReadKeyset ->
      NonGeneric (TyString :~> TyGuard)
    CoreEnforceGuard ->
      let aVar = nd "a" 0
          a = TyVar aVar
      in TypeScheme [aVar] [Pred (EnforceRead a)] (a :~> TyBool)
    CoreEnforceKeyset ->
      let aVar = nd "a" 0
          a = TyVar aVar
      in TypeScheme [aVar] [Pred (EnforceRead a)] (a :~> TyBool)
    CoreKeysetRefGuard ->
      NonGeneric (TyString :~> TyGuard)
    CoreCreateCapabilityGuard ->
      let var = TypeVar (NamedDeBruijn 0 "a") UserDefKind
      in TypeScheme [var] [] (TyCapToken (CapVar var) :~> TyGuard)
    CoreCreateCapabilityPactGuard ->
      let var = TypeVar (NamedDeBruijn 0 "a") UserDefKind
      in TypeScheme [var] [] (TyCapToken (CapVar var) :~> TyGuard)
    CoreCreateModuleGuard ->
      NonGeneric (TyString :~> TyGuard)
    CoreCreateDefPactGuard ->
      NonGeneric (TyString :~> TyGuard)
    -- todo: object access within `At`
    CoreAt ->
      let aVar = nd "a" 0
          a = TyVar aVar
      in TypeScheme [aVar] [] (TyInt :~> TyList a :~> a)
    CoreMakeList ->
      let aVar = nd "a" 0
          a = TyVar aVar
      in TypeScheme [aVar] [] (TyInt :~> a :~> TyList a)
    CoreB64Encode ->
      NonGeneric (TyString :~> TyString)
    CoreB64Decode ->
      NonGeneric (TyString :~> TyString)
    CoreStrToList ->
      NonGeneric (TyString :~> TyList TyString)
    CoreYield ->
      let aVar = RowVariable 0 "a"
      in TypeScheme [aVar] [] (TyObject (RowVar aVar) :~> TyObject (RowVar aVar))
    CoreYieldToChain ->
      let aVar = RowVariable 0 "a"
      in TypeScheme [aVar] [] (TyObject (RowVar aVar) :~> TyString :~> TyObject (RowVar aVar))
    CoreResume ->
      let aVar = RowVariable 1 "a"
          bodyVar = TypeVariable 0 "b"
      in TypeScheme [aVar, bodyVar] [] ((TyObject (RowVar aVar) :~> TyVar bodyVar) :~> TyVar bodyVar)
    CoreBind ->
      let aVar = RowVariable 1 "a"
          bodyVar = TypeVariable 0 "b"
      in TypeScheme [aVar, bodyVar] [] (TyObject (RowVar aVar) :~> (TyObject (RowVar aVar) :~> TyVar bodyVar) :~> TyVar bodyVar)
    CoreRequireCapability ->
      let aVar = UserDefVariable 0 "a"
      in TypeScheme [aVar] [] (TyCapToken (CapVar aVar) :~> TyBool)
    CoreComposeCapability ->
      let aVar = UserDefVariable 0 "a"
      in TypeScheme [aVar] [] (TyCapToken (CapVar aVar) :~> TyBool)
    CoreInstallCapability ->
            let aVar = UserDefVariable 0 "a"
      in TypeScheme [aVar] [] (TyCapToken (CapVar aVar) :~> TyString)
    CoreEmitEvent ->
      let aVar = UserDefVariable 0 "a"
      in TypeScheme [aVar] [] (TyCapToken (CapVar aVar) :~> TyString)
    CoreCreateTable ->
      let aVar = UserDefVariable 0 "a"
      in TypeScheme [aVar] [] (TyCapToken (CapVar aVar) :~> TyString)
    CoreDescribeKeyset ->
      error "todo: unsupported"
    CoreDescribeModule ->
      error "todo: unsupported"
    CoreDescribeTable ->
      error "todo: unsupported"
    CoreDefineKeySet ->
      NonGeneric (TyString :~> TyGuard :~> TyString)
    CoreDefineKeysetData ->
      NonGeneric (TyString :~> TyString)
    -- fold-db : forall (r: ROW) (out : TYPE)
    --           .  table<r>
    --           -> (string -> object<r> -> bool)
    --           -> (string -> object<r> -> <out>)
    --           -> [<out>]
    CoreFoldDb -> let
      rowVar = RowVariable 1 "row"
      outVar = TypeVariable 0 "a"
      queryLam = TyString :~> TyObject (RowVar rowVar) :~> TyBool
      appLam = TyString :~> TyObject (RowVar rowVar) :~> TyVar outVar
      fnTy = TyTable (RowVar rowVar) :~> queryLam :~> appLam :~> TyList (TyVar outVar)
      in TypeScheme [rowVar, outVar] [] fnTy
    -- insert : forall (r: ROW) . table<r> -> string -> object<r> -> string
    CoreInsert -> let
      rowVar = RowVariable 0 "row"
      fnTy = TyTable (RowVar rowVar) :~> TyString :~> TyObject (RowVar rowVar) :~> TyString
      in TypeScheme [rowVar] [] fnTy
    CoreKeyLog ->
       error "todo: unsupported"
    CoreKeys -> let
      rowVar = RowVariable 0 "row"
      fnTy = TyTable (RowVar rowVar) :~> TyList TyString
      in TypeScheme [rowVar] [] fnTy
    CoreRead -> let
      rowVar = RowVariable 0 "row"
      fnTy = TyTable (RowVar rowVar) :~> TyString :~> TyObject (RowVar rowVar)
      in TypeScheme [rowVar] [] fnTy
    CoreSelect ->let
      rowVar = RowVariable 0 "row"
      fnTy = TyTable (RowVar rowVar) :~> (TyObject (RowVar rowVar) :~> TyBool) :~> TyList (TyObject (RowVar rowVar))
      in TypeScheme [rowVar] [] fnTy
    CoreSelectWithFields ->
      error "todo: not supported"
    CoreUpdate -> let
      rowVar = RowVariable 0 "row"
      fnTy = TyTable (RowVar rowVar) :~> (TyObject (RowVar rowVar) :~> TyBool) :~> TyList (TyObject (RowVar rowVar))
      in TypeScheme [rowVar] [] fnTy
    CoreWithDefaultRead ->
      error "todo: support function"
    CoreWithRead ->
      error "todo: support function"
    CoreWrite ->
      error "todo: support function"
    CoreTxIds ->
      error "todo: support function"
    CoreTxLog ->
      error "todo: support function"
    CoreTxHash ->
      error "todo: support function"
    CoreAndQ ->
      error "todo: support function"
    CoreOrQ ->
      error "todo: support function"
    CoreWhere ->
      error "todo: support function"
    CoreNotQ ->
      error "todo: support function"
    CoreHash ->
      error "todo: support function"
    CoreContinue ->
      error "todo: support function"
    CoreParseTime ->
      error "todo: support function"
    CoreFormatTime ->
      error "todo: support function"
    CoreTime ->
      error "todo: support function"
    CoreAddTime ->
      error "todo: support function"
    CoreDiffTime ->
      error "todo: support function"
    CoreHours ->
      error "todo: support function"
    CoreMinutes ->
      error "todo: support function"
    CoreDays ->
      error "todo: support function"
    CoreCompose ->
      error "todo: support function"
    CoreCreatePrincipal ->
      error "todo: support function"
    CoreIsPrincipal ->
      error "todo: support function"
    CoreTypeOfPrincipal ->
      error "todo: support function"
    CoreValidatePrincipal ->
      error "todo: support function"
    CoreNamespace ->
      error "todo: support function"
    CoreDefineNamespace ->
      error "todo: support function"
    CoreDescribeNamespace ->
      error "todo: support function"
    CoreZkPairingCheck ->
      error "todo: support function"
    CoreZKScalarMult ->
      error "todo: support function"
    CoreZkPointAdd ->
      error "todo: support function"
    CorePoseidonHashHackachain ->
      error "todo: support function"
    CoreChainData ->
      error "todo: support function"
    CoreIsCharset ->
      error "todo: support function"
    CorePactId ->
      error "todo: support function"
    CoreTypeOf ->
      error "todo: support function"
    CoreDec ->
      error "todo: support function"
    CoreCond -> error "todo: nothing"
    where
    nd = flip TypeVariable
    unaryNumType =
      let aVar = nd "a" 0
          a = TyVar aVar
      in TypeScheme [aVar] [Pred (Num a)] (a :~> a)
    unaryFractional =
      let aVar = nd "a" 0
          a = TyVar aVar
      in TypeScheme [aVar] [Pred (Fractional a)] (a :~> TyDecimal)
    addBinopType =
      let aVar = nd "a" 0
          a = TyVar aVar
      in TypeScheme [aVar] [Pred (Add a)] (a :~> a :~> a)
    numBinopType =
      let aVar = nd "a" 0
          a = TyVar aVar
      in TypeScheme [aVar] [Pred (Num a)] (a :~> a :~> a)
    eqTyp =
      let aVar = nd "a" 0
          a = TyVar aVar
      in TypeScheme [aVar] [Pred (Eq a)] (a :~> a :~> TyBool)
    ordTyp =
      let aVar = nd "a" 0
          a = TyVar aVar
      in TypeScheme [aVar] [Pred (Ord a)] (a :~> a :~> TyBool)
    unaryInt = NonGeneric (TyInt :~> TyInt)
    -- integer -> integer -> integer
    binaryInt = NonGeneric (TyInt :~> TyInt :~> TyInt)
    -- decimal -> integer
    roundingFn = NonGeneric (TyDecimal :~> TyInt)
    roundingPrecFn = NonGeneric (TyDecimal :~> TyInt :~> TyDecimal)
    -- bool -> bool -> bool
    -- binaryBool = TypeScheme [] [] (TyBool :~> TyBool :~> TyBool)
    -- forall a. ListLike a => int -> a -> a
    takeDropTy =
      let aVar = nd "a" 0
          a = TyVar aVar
      in TypeScheme [aVar] [Pred (ListLike a)] (TyInt :~> a :~> a)

instance TypeOfBuiltin b => TypeOfBuiltin (ReplBuiltin b) where
  typeOfBuiltin = \case
    RBuiltinWrap b -> typeOfBuiltin b
    _ -> error "todo: type repl builtins"
    -- RExpect -> let
    --   aVar = nd "a" 0
    --   aTv = TyVar aVar
    --   in TypeScheme [aVar] [Pred Eq aTv, Pred Show aTv] (TyString :~> aTv :~> (TyUnit :~> aTv) :~> TyString)
    -- RExpectFailure -> let
    --   aVar = nd "a" 0
    --   aTv = TyVar aVar
    --   in TypeScheme [aVar] [] (TyString :~> (TyUnit :~> aTv) :~> TyString)
    -- RExpectThat -> let
    --   aVar = nd "a" 0
    --   aTv = TyVar aVar
    --   in TypeScheme [aVar] [] (TyString :~> (aTv :~> TyBool) :~> aTv :~> TyString)
    -- RPrint -> let
    --   aVar = nd "a" 0
    --   aTv = TyVar aVar
    --   in TypeScheme [aVar] [Pred Show aTv] (aTv :~> TyUnit)

liftST :: ST s a -> InferM s b i a
liftST action = InferM $ lift $ lift $ lift action

throwTypecheckError :: i -> Text -> InferM s b i a
throwTypecheckError i msg = throwError (TypecheckFailure msg i)

-- _dbgTypedTerm
--   :: TCTerm s b i
--   -> InferM s b i (Term Text Text (b, [Type Text], [Pred Text]) i)
-- _dbgTypedTerm = \case
--   Var n i -> pure (Var (_nName n) i)
--   Lam nel body i -> do
--     nel' <- (traversed._2) _dbgType nel
--     body' <- _dbgTypedTerm body
--     pure (Lam nel' body' i)
--   App fn body i ->
--     App <$> _dbgTypedTerm fn <*> traverse _dbgTypedTerm body <*> pure i
--   Let n e1 e2 i ->
--     Let n <$> _dbgTypedTerm e1 <*> _dbgTypedTerm e2 <*> pure i
--   Builtin (b, tys, preds) i -> do
--     tys' <- traverse _dbgType tys
--     preds' <- traverse _dbgPred preds
--     pure (Builtin (b, tys', preds') i)
--   Constant l i -> pure (Constant l i)
--   TyApp t nelty i ->
--     TyApp <$> _dbgTypedTerm t <*> traverse _dbgType nelty <*> pure i
--   TyAbs tys term i -> do
--     tys' <- traverse _dbgTvRef tys
--     term' <- _dbgTypedTerm term
--     pure (TyAbs tys' term' i)
--   Sequence e1 e2 i ->
--     Sequence <$> _dbgTypedTerm e1 <*> _dbgTypedTerm e2 <*> pure i
--   Conditional c i ->
--     Conditional <$> traverse _dbgTypedTerm c <*> pure i
--   Try e1 e2 i ->
--     Try <$> _dbgTypedTerm e1 <*> _dbgTypedTerm e2 <*> pure i
--   Error t e i ->
--     Error <$> _dbgType t <*> pure e <*> pure i
--   DynInvoke n t i ->
--     DynInvoke <$> _dbgTypedTerm n <*> pure t <*> pure i
--   CapabilityForm cf i ->
--     let cf' = over capFormName _nName cf
--     in CapabilityForm <$> traverse _dbgTypedTerm cf' <*> pure i
--   ListLit ty li i ->
--     ListLit <$> _dbgType ty <*> traverse _dbgTypedTerm li <*> pure i
--   -- ObjectLit obj i ->
--   --   ObjectLit <$> traverse _dbgTypedTerm obj <*> pure i
--   -- ObjectOp oop i ->
--   --   ObjectOp <$> traverse _dbgTypedTerm oop <*> pure i

-- _dbgTypeScheme :: TypeScheme (TvRef s) -> InferM s b i (TypeScheme Text)
-- _dbgTypeScheme (TypeScheme tvs preds ty) = do
--   tvs' <- traverse rv tvs
--   preds' <- traverse _dbgPred preds
--   ty' <- _dbgType ty
--   pure (TypeScheme tvs' preds' ty')
--   where
--   rv n = readTvRef n >>= \case
--     Unbound u l _ -> pure ("unbound" <> T.pack (show (u, l)))
--     Bound u l -> pure ("bound" <> T.pack (show (u, l)))
--     Link _ -> pure "linktv"

-- _dbgTvRef :: TvRef s -> InferM s b i Text
-- _dbgTvRef tv = readTvRef tv >>= \case
--     Unbound u l _ -> pure ("unbound" <> T.pack (show (u, l)))
--     Bound u l -> pure ("bound" <> T.pack (show (u, l)))
--     Link ty -> do
--       ty' <- _dbgType ty
--       pure $ "linked type<" <> T.pack (show ty') <> ">"

-- _dbgPred :: TCPred s -> InferM s b i (Pred Text)
-- _dbgPred (Pred i t) = Pred i <$> _dbgType t

-- _dbgType :: TCType s -> InferM s b i (Type Text)
-- _dbgType = \case
--   TyVar tv -> readTvRef tv >>= \case
--     Unbound u l _ -> pure (TyVar ("unbound" <> T.pack (show (u, l))))
--     Bound u l -> pure (TyVar ("bound" <> T.pack (show (u, l))))
--     Link ty -> _dbgType ty
--   TyFun l r -> TyFun <$> _dbgType l <*> _dbgType r
--   TyList t -> TyList <$> _dbgType t
--   TyPrim p -> pure (TyPrim p)
--   TyModRef mr -> pure (TyModRef mr)
--   TyForall {} -> error "impredicative"


enterLevel :: InferM s b i ()
enterLevel = do
  lref <- asks _tcLevel
  liftST (modifySTRef' lref succ)

leaveLevel :: InferM s b i ()
leaveLevel = do
  lref <- asks _tcLevel
  liftST (modifySTRef' lref pred)

currentLevel :: InferM s b i Level
currentLevel =
  asks _tcLevel >>= liftST . readSTRef

readTvRef :: TCTypeVar s -> InferM s b i (Tv s)
readTvRef (TypeVar (TvRef tv) _) = liftST (readSTRef tv)

writeTvRef :: TCTypeVar s -> Tv s -> InferM s b i ()
writeTvRef (TypeVar (TvRef tv) _) t = liftST (writeSTRef tv t)

newCapTokenRef :: QualifiedName -> InferM s b i (TCTypeVar s)
newCapTokenRef qn = do
  uref <- asks _tcSupply
  liftST (modifySTRef' uref (+ 1))
  (`TypeVar` UserDefKind) . TvRef <$> liftST (newSTRef (LinkCap (CapConcrete qn)))

newTvRef :: InferM s b i (TvRef s)
newTvRef = do
  uref <- asks _tcSupply
  u <- liftST (readSTRef uref)
  let tvName = "'a_" <> T.pack (show u)
  l <- currentLevel
  liftST (modifySTRef' uref (+ 1))
  TvRef <$> liftST (newSTRef (Unbound tvName u l))

newSupplyIx :: InferM s b i Unique
newSupplyIx = do
  uref <- asks _tcSupply
  u <- liftST (readSTRef uref)
  liftST (modifySTRef' uref (+ 1))
  pure u
---------------------------------------------------------------
-- Type class instances,
-- entailment, context reduction.
-- ---------------------------------------------------------------

-- -- | For some typeclass C,
-- -- is there an instance of C t?
-- -- If so, return the qualifiers of the instance.
-- -- that is, for (C a_1, .., C a_n) => C t
-- -- byInst (C t) returns Just [C a_1, .., C a_n].
-- -- Note: if these were user defined, if we decide to extend to this
-- -- byInst would have to match the type of C (K t) to an instantiated version
-- -- of the qualified type (C a_1, .., C a_n) => C (K t_1) for type constructors
byInst :: TCPred s -> InferM s b i (Maybe [TCPred s])
byInst (Pred p) = case p of
  Eq ty -> eqInst ty
  Add ty -> addInst ty
  Num ty -> numInst ty
  Ord ty -> ordInst ty
  Show ty -> showInst ty
  ListLike ty -> listLikeInst ty
  Fractional ty -> fractionalInst ty
  EnforceRead{} -> error "todo: implement"
  EqRow{} -> error "todo: eqrow"
  RoseSubRow{} -> error "todo: rosesubrow"

-- | Instances of Eq:
--
--  instance Eq integer
--  instance Eq decimal
--  instance Eq string
--  instance Eq unit
--  instance Eq bool
--  instance Eq time <- todo
--
--  instance (Eq 'a) => Eq (list 'a)
--  For rows:
--  instance (Eq t_1,..,Eq t_n) => Eq (RowConcrete {l_1:t1,..,l_n)
--    where t_1..t_n are monotypes without type variables.
--  instance (EqRow r) => Eq (Object {r})
--
eqInst :: TCType s -> InferM s b i (Maybe [TCPred s])
eqInst = \case
  TyVar tv -> readTvRef tv >>= \case
    LinkTy ty -> eqInst ty
    _ -> pure Nothing
  -- All prims have an EQ instance
  TyPrim _ -> pure (Just [])
  TyModRef _ -> pure (Just [])
  TyList t -> pure (Just [Pred (Eq t)])
  TyCapToken cr -> case cr of
    CapConcrete{} -> pure $ Just []
    CapVar tv -> readTvRef tv >>= \case
      LinkCap cvar' -> eqInst (TyCapToken cvar')
      _ -> pure Nothing
  TyObject r -> case r of
    RowVar rv -> readTvRef rv >>= \case
      LinkRow row' -> eqInst (TyObject row')
      _ -> pure Nothing
    RowConcrete row -> do
      preds <- M.elems <$> traverse eqInst row
      pure $ concat <$> sequence preds
  _ -> pure Nothing

-- | Instances of Ord:
--
--  instance Ord integer
--  instance Ord decimal
--  instance Ord string
--  instance Ord unit
--  instance Ord time <- todo
--
--  instance (Ord 'a) => Ord (list 'a)
--  For rows:
--
ordInst :: TCType s -> InferM s b i (Maybe [TCPred s])
ordInst = \case
  TyVar tv -> readTvRef tv >>= \case
    LinkTy ty -> ordInst ty
    _ -> pure Nothing
  -- All prims have an Ord instance
  TyPrim p -> case p of
    PrimInt -> pure (Just [])
    PrimDecimal -> pure (Just [])
    PrimString -> pure (Just [])
    -- PrimTime -> pure (Just [])
    PrimUnit -> pure (Just [])
    _ -> pure Nothing
  TyList t -> pure (Just [Pred (Ord t)])
  _ -> pure Nothing


-- -- | Instances of Add:
-- --
-- --  instance Add integer
-- --  instance Add decimal
-- --  instance Add string
-- --  instance Add (list 'a)
-- --
-- --
addInst :: TCType s -> InferM s b i (Maybe [TCPred s])
addInst = \case
  TyVar tv -> readTvRef tv >>= \case
    LinkTy ty -> addInst ty
    _ -> pure Nothing
  -- All prims have an EQ instance
  TyPrim p -> pure $ case p of
    PrimString -> Just []
    PrimInt -> Just []
    PrimDecimal -> Just []
    _ -> Nothing
  TyList _ -> pure (Just [])
  _ -> pure Nothing

-- | Instances of num:
-- instance Num integer
-- instance Num decimal
numInst :: TCType s -> InferM s b i (Maybe [TCPred s])
numInst = \case
  TyVar tv -> readTvRef tv >>= \case
    LinkTy ty -> numInst ty
    _ -> pure Nothing
  -- All prims have an EQ instance
  TyPrim p -> pure $ case p of
    PrimInt -> Just []
    PrimDecimal -> Just []
    _ -> Nothing
  _ -> pure Nothing

-- | Instances of fractional:
-- instance Fractional integer
-- instance Fractional decimal
fractionalInst :: TCType s -> InferM s b i (Maybe [TCPred s])
fractionalInst = \case
  TyVar tv -> readTvRef tv >>= \case
    LinkTy ty -> fractionalInst ty
    _ -> pure Nothing
  TyPrim p -> pure $ case p of
    PrimInt -> Just []
    PrimDecimal -> Just []
    _ -> Nothing
  _ -> pure Nothing

listLikeInst :: TCType s -> InferM s b i (Maybe [TCPred s])
listLikeInst = \case
  TyVar tv -> readTvRef tv >>= \case
    LinkTy ty -> listLikeInst ty
    _ -> pure Nothing
  TyPrim p -> pure $ case p of
    PrimString -> Just []
    _ -> Nothing
  TyList _ -> pure $ Just []
  _ -> pure Nothing

-- | Instances of Show:
--
--  instance Show integer
--  instance Show decimal
--  instance Show string
--  instance Show unit
--  instance Show bool
--  instance Show time <- todo
--
--  instance (Show 'a) => Show (list 'a)
--  instance (Show )
--  For rows:
--  instance (Eq {l1:t1, .., ln:tn}) where t1..tn are monotypes without type variables.
--
showInst :: TCType s -> InferM s b i (Maybe [TCPred s])
showInst = \case
  TyVar tv -> readTvRef tv >>= \case
    LinkTy ty -> showInst ty
    _ -> pure Nothing
  -- All prims have an EQ instance
  TyPrim _p -> pure (Just [])
  TyList t -> pure (Just [Pred (Show t)])
  _ -> pure Nothing

entail :: [TCPred s] -> TCPred s -> InferM s b i Bool
entail ps p = byInst p >>= \case
  Nothing -> pure False
  Just qs -> and <$> traverse (entail ps) qs

isHnf :: TCPred s -> InferM s b i Bool
isHnf (Pred t) = do
  unbounds <- traverse tyHnf t
  pure $ and unbounds

tyHnf :: TCType s -> InferM s b i Bool
tyHnf = \case
  TyVar tv -> readTvRef tv >>= \case
    LinkTy ty -> tyHnf ty
    Bound{} -> pure True
    _ -> pure False
  TyObject c -> rowCtorHnf c
  TyTable c -> rowCtorHnf c
  TyCapToken cr -> case cr of
    CapVar tv -> readTvRef tv >>= \case
      Bound{} -> pure True
      LinkCap cvar -> tyHnf (TyCapToken cvar)
      _ -> pure False
    CapConcrete{} -> pure False
  _ -> pure False
  where
  rowCtorHnf = \case
    RowVar v -> readTvRef v >>= \case
      LinkRow r -> rowCtorHnf r
      Bound{} -> pure True
      _ -> pure False
    RowConcrete rows ->
      and <$> traverse tyHnf rows

toHnf :: TCPred s -> i -> InferM s b i [TCPred s]
toHnf p i = isHnf p >>= \case
  True -> pure [p]
  False -> byInst p >>= \case
    Nothing -> error "context reduction error"
      -- p' <- _dbgPred p
      -- throwTypecheckError (ContextReductionError p') i
    Just ps -> toHnfs ps i

toHnfs :: [TCPred s] -> i -> InferM s b i [TCPred s]
toHnfs ps i = do
  pss <- traverse (`toHnf` i) ps
  pure (concat pss)

simplify :: [TCPred s] -> InferM s b i [TCPred s]
simplify = loop []
  where
  loop rs [] = pure rs
  loop rs (p:ps) = entail (rs ++ ps) p >>= \cond ->
    if cond then loop rs ps else loop (p:rs) ps

reduce :: [TCPred s]-> i -> InferM s b i [TCPred s]
reduce ps i = toHnfs ps i >>= simplify

split
  :: [TCPred s]
  -> i
  -> InferM s b i ([TCPred s], [TCPred s])
split ps i = do
  ps' <- reduce ps i
  partition' ([], []) ps'
  where
  partition' (ds, rs) (p@(Pred ty) : xs) = do
    cond <- and <$> traverse hasUnbound ty
    if cond then partition' (p:ds, rs) xs
    else partition' (ds, p:rs) xs
  partition' (ds, rs) [] =
    pure (reverse ds, reverse rs)
  varUnbound ref = readTvRef ref >>= \case
    Unbound{} -> pure True
    LinkTy ty -> hasUnbound ty
    LinkRow r -> rowUnbound r
    LinkCap _ -> pure False
    Bound {} -> pure False
  hasUnbound = \case
    TyVar n -> varUnbound n
    TyPrim _ -> pure False
    TyList t -> hasUnbound t
    TyFun l r -> do
      l' <- hasUnbound l
      if l' then pure l' else hasUnbound r
    TyNullary n -> hasUnbound n
    TyObject n -> rowUnbound n
    TyTable n -> rowUnbound n
    TyModRef _ -> pure False
    TyCapToken n -> capVarUnbound n
  capVarUnbound = \case
    CapVar n -> varUnbound n
    CapConcrete _ -> pure False
  rowUnbound = \case
    RowVar n -> varUnbound n
    RowConcrete n -> or <$> traverse hasUnbound n

checkReducible :: [TCPred s] -> i -> InferM s b i ()
checkReducible ps i =
  reduce ps i >>= \case
    [] -> pure ()
    _xs' -> error "unsupported: could not resolve all typeclass params"
      -- xs' <- traverse _dbgPred xs
      -- throwTypecheckError (UnsupportedTypeclassGeneralization xs') i

----------------------------------------------------------------------
---- Instantiations
---------------------------------------------------------------------

-- | Instantiate a typescheme with bound variables with fresh bound variables
-- Corresponds to the following inference rule
--
-- That is (∀E):
--     P | Γ ⊢ E : ∀a_1..a_n. ρ
--     b_1, ..,  b_n fresh
--     ---------------------------------------
--     P | Γ ⊢ E ~> E_f[b_1,..,b c_n] : ρ
-- instantiateWithTerm
--   :: TypeScheme (TvRef s)
--   -> TCTerm s b i
--   -> InferM s b i (TCType s, TCTerm s b i, [TCPred s])
-- instantiateWithTerm (TypeScheme ts preds ty) term = do
--   nts <- fmap TyVar <$> traverse (const newTvRef) ts
--   let m = zip ts nts
--   preds' <- traverse (instPred m) preds
--   ty' <- instBound m ty
--   case nts of
--     x:xs -> do
--       let tyapps = TyApp term (x:|xs) info
--       dvars <- traverse toDVar preds'
--       case dvars of
--         p:ps -> pure (ty', App tyapps (p:|ps) info, preds')
--         [] -> pure (ty', tyapps, [])
--     [] -> pure (ty', term, [])
--   where
--   info = term ^. termInfo
  -- toDVar p = do
  --   i <- newSupplyIx
  --   let n = OverloadedName ("_dict" <> T.pack (show i)) (OBuiltinDict p)
  --   pure $ Var n info
  -- instPred m (Pred n tt) =
  --   Pred n <$> instBound m tt
  -- instBound m = \case
  --   t@(TyVar tv) -> readTvRef tv >>= \case
  --     Bound{} -> case lookup tv m of
  --       Just t' -> pure t'
  --       Nothing -> pure t
  --     Link lt -> instBound m lt
  --     _ -> pure t
  --   TyPrim p -> pure (TyPrim p)
  --   TyFun l r ->
  --     TyFun <$> instBound m l <*> instBound m r
  --   TyList t -> TyList <$> instBound m t
  --   t -> pure t

instantiateImported
  :: TypeScheme DebruijnTypeVar
  -> i
  -> InferM s b i (TCType s, [TCTypeVar s], [TCPred s])
instantiateImported (TypeScheme tvs preds ty) _i = do
    ntvs <- traverse (const newTvRef) tvs
    let ntvs' = zipWith (\tvr (TypeVar _ kind) -> TypeVar tvr kind) ntvs tvs
    let rl = RAList.fromList (reverse ntvs)
    ty' <- inst rl ty
    preds' <- traverse (instPred rl) preds
    pure (ty', ntvs', preds')
  where
  instPred rl (Pred tc) = do
    Pred <$> traverse (inst rl) tc
  instNamed rl (TypeVar (NamedDeBruijn i' _) kind) =
    pure $ TypeVar (rl RAList.!! i') kind
  inst rl = \case
    TyVar v -> TyVar <$> instNamed rl v
    TyPrim p -> pure (TyPrim p)
    TyFun l r -> TyFun <$> inst rl l <*> inst rl r
    TyList t -> TyList <$> inst rl t
    TyModRef mr -> pure (TyModRef mr)
    TyNullary n -> TyNullary <$> inst rl n
    TyObject rc -> TyObject <$> instRow rl rc
    TyTable rc -> TyObject <$> instRow rl rc
    TyCapToken n -> TyCapToken <$> instCapVar rl n
  instCapVar rl = \case
    CapVar n -> CapVar <$> instNamed rl n
    CapConcrete fp -> pure (CapConcrete fp)
  instRow rl = \case
    RowVar n -> RowVar <$> instNamed rl n
    RowConcrete m ->
      RowConcrete <$> traverse (inst rl) m
    -- Impredicative type might work
    -- If we change unification.
    -- TyForall _ _ ->
    --   throwTypecheckError UnsupportedImpredicativity i

occurs
  :: i
  -> TCTypeVar s
  -> TCType s
  -> InferM s b' i ()
occurs i tv tct = case tct of
  TyVar tv' | tv == tv' ->
    error "error: infinite type"
    -- tv'' <- _dbgType tct
    -- throwTypecheckError (OccursCheckFailure tv'') i
  TyVar tv' -> bindRef tv'
  TyFun l r -> occurs i tv l *> occurs i tv r
  TyList l -> occurs i tv l
  _ -> pure ()
  where
  bindRef tv' = readTvRef tv' >>= \case
    Unbound n u l' -> do
      ml <- minLevel
      writeTvRef tv' (Unbound n u ml)
      where
      minLevel = readTvRef tv >>= \case
        Unbound _ _ l -> pure (min l l')
        _ -> pure l'
    LinkTy ty -> occurs i tv ty
    _ -> pure ()

ensureWellKinded
  :: TCTypeVar s
  -> PactKind
  -> InferM s' b i ()
ensureWellKinded (TypeVar _ k) expected
  | k == expected = pure ()
  | otherwise = error $ "failed kind checking, expected " <> show k <> ", got " <> show expected

unifyTyVar
  :: i
  -> TCTypeVar s
  -> TCType s
  -> InferM s b' i ()
unifyTyVar i tv t1 = readTvRef tv >>= \case
  Unbound{} -> do
    ensureWellKinded tv TyKind
    occurs i tv t1
    writeTvRef tv (LinkTy t1)
  Bound{} -> error "cannot unify with bound variable"
  LinkTy t2 -> unify i t2 t1
  _ -> error "type variable is not well kinded"

unifyRowVar
  :: i
  -> TCTypeVar s
  -> TCRowCtor s
  -> InferM s b' i ()
unifyRowVar _ tv t1 = readTvRef tv >>= \case
  Unbound{} -> do
    ensureWellKinded tv RowKind
    -- todo: occurs row
    -- occurs tv t1 i
    writeTvRef tv (LinkRow t1)
  Bound{} -> error "cannot unify with bound variable"
  _ -> error "type variable is not well kinded"

-- unifyTyVarRow
--   :: TCTypeVar s
--   -> TCRowCtor s
--   -> i
--   -> InferM s b' i ()
-- unifyTyVarRow tv t1 i
--   | tyVarKind tv == RowKind = readTvRef tv >>= \case
--   Unbound{} -> do
--     occurs tv t1 i
--     writeTvRef tv (LinkTy t1)
--   LinkTy t2 -> unify t2 t1 i
--   _ -> pure ()
--   | otherwise = error "attempted to unify a variable of incorrect kind"

-- unifyTyVarRow
--   :: TCTypeVar s
--   -> TCType s
--   -> i
--   -> InferM s b' i ()
-- unifyTyVarRow tv t1 i
--   | tyVarKind tv == TyKind = readTvRef tv >>= \case
--   Unbound{} -> do
--     occurs tv t1 i
--     writeTvRef tv (LinkTy t1)
--   LinkTy t2 -> unify t2 t1 i
--   _ -> pure ()
--   | otherwise = error "attempted to unify a variable of incorrect kind"

unify
  :: i
  -> TCType s
  -> TCType s
  -> InferM s b i ()
unify _i t1 t2 | t1 == t2 = pure ()
unify i (TyVar tv) t = unifyTyVar i tv t
unify i t (TyVar tv)  = unifyTyVar i tv t
unify i (TyFun l r) (TyFun l' r') = unify i l l' *> unify i r r'
unify i (TyList t) (TyList t') = unify i t t'
unify _ (TyPrim p) (TyPrim p') | p == p' = pure ()
unify i (TyObject r) (TyObject l) = unifyRow i l r
unify i (TyTable r) (TyTable l) = unifyRow i l r
unify _i (TyModRef mr) (TyModRef mr') | mr == mr' = pure ()
unify _i _t1 _t2 =
  error "unification error"
  -- t1' <- _dbgType t1
  -- t2' <- _dbgType t2
  -- throwTypecheckError (UnificationError t1' t2') i

unifyRow
  :: i
  -> TCRowCtor s
  -> TCRowCtor s
  -> InferM s b i ()
unifyRow i (RowVar tv) row =
  unifyRowVar i tv row
unifyRow i row (RowVar tv) =
  unifyRowVar i tv row
unifyRow i (RowConcrete lrow) (RowConcrete rrow) = do
  when (M.keys lrow /= M.keys rrow) $ error  $ "cannot unify rows"
  () <$ zipWithM (unify i) (M.elems lrow) (M.elems rrow)

checkTermType
  :: (TypeOfBuiltin b)
  => TCType s
  -> IR.Term Name IR.Type b i
  -> InferM s b i (TCType s, TCTerm s b i, [TCPred s])
checkTermType checkty = \case
  IR.Var irn@(Name n nk) i -> case nk of
    NBound u -> do
      views tcVarEnv (`RAList.lookup` u) >>= \case
        Just ty -> do
          let v' = Var irn i
          unify i checkty ty
          pure (ty, v', [])
        Nothing ->
          error "unbound term variable"
          -- throwTypecheckError (TCUnboundTermVariable n) i
    NTopLevel mn mh ->
      use (tcFree . at (FullyQualifiedName mn n mh)) >>= \case
        Just nty -> do
          let newVar = Var irn i
              rty = liftType nty
          unify i rty checkty
          pure (rty, newVar, [])
        _ ->
          error "unbound free variable"
    NModRef _ ifs -> case checkty of
      TyModRef mn -> do
        let newVar = Var irn i
        let ifSet = Set.fromList ifs
            ifImplements = Set.intersection mn ifSet
        if ifImplements `Set.isSubsetOf` mn  then
          pure (TyModRef ifImplements, newVar, [])
        else
          error "modref does not implement interface"
      v -> case ifs of
        [iface] -> do
          unify i v (TyModRef (Set.singleton iface))
          pure (TyModRef (Set.singleton iface), Var irn i, [])
        _ -> error "cannot infer modref type"
    _ -> error "dynref not supported"
  -- Todo: lambda checking cancan be a bit better.
  term@(IR.Lam _ _ i) -> do
    tup <- inferTerm term
    unify i (view _1 tup) checkty
    pure tup
    --   let (tl, ret) = tyFunToArgList checkty
    --   when (length tl /= NE.length ne) $ error "Arguments mismatch"
    --   let zipped = NE.zip ne (NE.fromList tl)
    --   traverse_ (uncurry unifyArg) zipped
    --   let args = RAList.fromList $ reverse tl
    --   (_, te', preds) <- locally tcVarEnv (args RAList.++) $ checkTermType ret te
    --   let ne' = over _1 fst <$> zipped
    --   pure (checkty, Lam ne' te' i, preds)
    -- where
    -- unifyArg (_, Just tl) tr = unify (liftType tl) tr i
    -- unifyArg _ _ = pure ()
  IR.Let (IR.Arg txt m_ty) e1 e2 i ->
    case m_ty of
      Just lty -> do
        let lty' = liftCoreType lty
        (_, e1', pe1) <- checkTermType lty' e1
        (_, e2', pe2) <-
          locally tcVarEnv (RAList.cons lty') $ checkTermType checkty e2
        let term' = Let (Arg txt lty') e1' e2' i
        pure (checkty, term', pe1 ++ pe2)
      Nothing -> do
        enterLevel
        (te1, e1', pe1) <- inferTerm e1
        leaveLevel
        (te2, e2', pe2) <-
          locally tcVarEnv (RAList.cons te1) $ checkTermType checkty e2
        let term' = Let (Arg txt te1) e1' e2' i
        pure (te2, term', pe1 ++ pe2)
  term@(IR.App _ _ i) -> do
    (termTy, term', preds) <- inferTerm term
    unify i termTy checkty
    pure (termTy, term', preds)
  IR.Sequence l r i -> do
    (_, l', pl) <- inferTerm l
    (_, r', pr) <- checkTermType checkty r
    pure (checkty, Sequence l' r' i, pl ++ pr)
  IR.Conditional cond i -> over _2 (`Conditional` i) <$>
    case cond of
      CAnd e1 e2 -> do
        unify i checkty TyBool
        (_, e1', pe1) <- checkTermType TyBool e1
        (_, e2', pe2) <- checkTermType TyBool e2
        pure (TyBool, CAnd e1' e2', pe1 ++ pe2)
      COr e1 e2 -> do
        unify i checkty TyBool
        (_, e1', pe1) <- checkTermType TyBool e1
        (_, e2', pe2) <- checkTermType TyBool e2
        pure (TyBool, COr e1' e2', pe1 ++ pe2)
      CIf c e1 e2 -> do
        (_, c', pc) <- checkTermType TyBool c
        (_, e1', pe1) <- checkTermType checkty e1
        (_, e2', pe2) <- checkTermType checkty e2
        pure (checkty, CIf c' e1' e2', pc ++ pe1 ++ pe2)
      CEnforce bExpr strExpr -> do
        (_, bExpr', pe1) <- checkTermType TyBool bExpr
        (_, strExpr', pe2) <- checkTermType TyString strExpr
        pure (TyBool, CEnforce bExpr' strExpr', pe1 ++ pe2)
      CEnforceOne{} -> error ""
  IR.Builtin b i -> do
    let tyImported = typeOfBuiltin b
    (ty, tvs, preds) <- instantiateImported tyImported i
    unify i checkty ty
    let term' = Builtin (b, TyVar <$> tvs, preds) i
    pure (ty, term', preds)
  IR.CapabilityForm cf i -> over _2 (`CapabilityForm` i) <$> case cf of
    WithCapability c_token body -> do
      tv <- newTvRef
      let tvar = TypeVar tv UserDefKind
          ty = TyCapToken (CapVar tvar)
      (tyTok, c_token', p1) <- inferTerm c_token
      unify i ty tyTok
      (ty', body', p2) <- checkTermType checkty body
      pure (ty', WithCapability c_token' body', p1 ++ p2)
    CreateUserGuard ne args -> do
      (_, Apply _ args' _, pe1) <- inferApply (Apply (IR.Var ne i) args i)
      unify i checkty TyGuard
      pure (TyGuard, CreateUserGuard ne args', pe1)
  -- Todo: numeric integer literal inference
  IR.Constant lit i -> case lit of
    -- LInteger{} -> do
    --   tv <- TyVar . (`TypeVar` TyKind) <$> newTvRef
    --   let p = Pred (Num tv)
    --   unify i checkty tv
    --   pure (tv, Constant lit i, [p])
    _ -> do
      let ty = typeOfLit lit
      unify i checkty ty
      pure (ty, Constant lit i, [])
  IR.ListLit tes i -> case checkty of
    TyList ty -> do
      liTup <- traverse (checkTermType ty) tes
      let preds = concat (view _3 <$> liTup)
          term' = ListLit ty (view _2 <$> liTup) i
      pure (TyList ty, term', preds)
    _ -> do
      tup <- inferTerm (IR.ListLit tes i)
      unify i (view _1 tup) checkty
      pure tup
  IR.Try errcase bodycase i -> do
    (_, err', p1) <- checkTermType checkty errcase
    (_, body', p2) <- checkTermType checkty bodycase
    pure (checkty, Try err' body' i, p1 ++ p2)
  IR.Nullary term i -> inferTerm (IR.Nullary term i)
  IR.ObjectLit fieldMap i -> do
    m <- traverse (\(f, t) -> (f,) <$> inferTerm t) fieldMap
    let objTyMap = view _1 <$> M.fromList m
        objTerms = over _2 (view _2) <$> m
        objTy = TyObject (RowConcrete objTyMap)
        preds = concat (view (_2._3) <$> m)
    unify i checkty objTy
    pure (objTy, ObjectLit objTerms i, preds)


-- -- Todo: bidirectionality
inferTerm
  :: (TypeOfBuiltin b)
  => IR.Term Name IR.Type b i
  -> InferM s b i (TCType s, TCTerm s b i, [TCPred s])
inferTerm = \case
  IR.Var irn@(Name n nk) i -> case nk of
    NBound u -> do
      views tcVarEnv (`RAList.lookup` u) >>= \case
        Just ty -> do
          let v' = Var irn i
          pure (ty, v', [])
        Nothing ->
          error "unbound term variable"
          -- throwTypecheckError (TCUnboundTermVariable n) i
    NTopLevel mn _mh ->
      use (tcFree . at (FullyQualifiedName mn n _mh)) >>= \case
        Just ty -> do
          let newVar = Var irn i
          pure (liftType ty, newVar, [])
        _ ->
          error "unbound free variable"
    NModRef _ ifs -> case ifs of
      [iface] -> do
        let v' = Var irn i
        pure (TyModRef (Set.singleton iface), v', [])
      [] -> error "Module reference does not implement any interfaces"
      _ -> error "Cannot infer module reference "
    NDynRef _ -> error "dynref"
  IR.Lam nts e i -> do
    -- let names = fst <$> nts
    ntys <- traverse withTypeInfo nts
    -- Todo: bidirectionality
    -- let m = IntMap.fromList $ NE.toList $ NE.zipWith (\n t ->  (_irUnique n, t)) names ntys
    let m = RAList.fromList (reverse (_argType <$> (NE.toList ntys)))
    (ty, e', preds) <- locally tcVarEnv (m RAList.++) $ inferTerm e
    let rty = foldr TyFun ty (_argType <$> ntys)
    pure (rty, Lam (NE.toList ntys) e' i, preds)
    where
    withTypeInfo (IR.Arg n p) = case p of
      Just ty -> pure (Arg n (liftCoreType ty))
      Nothing -> Arg n . TyVar . (`TypeVar` TyKind) <$> newTvRef
  IR.App te [] i -> do
    tv1 <- TyVar . (`TypeVar` TyKind) <$> newTvRef
    (tfun, te', pe1) <- inferTerm te
    unify i (TyNullary tv1) tfun
    pure (tv1, App te' [] i, pe1)
  IR.App te (h:hs) i -> do
    (tfun, te', pe1) <- inferTerm te
    (rty, xs, ps) <- foldlM inferFunctionArgs (tfun,[], []) (h:hs)
    let term' = App te' (reverse xs) i
    pure (rty, term', pe1 ++ ps)
    where
    inferFunctionArgs (ta, xs, ps) fnArg = case ta of
      TyFun arg ret -> do
        (_, x', p) <- checkTermType arg fnArg
        pure (ret, x':xs, ps ++ p)
      _ -> do
        tv1 <- TyVar . (`TypeVar` TyKind) <$> newTvRef
        (tArg, fnArg', predsArg) <- inferTerm fnArg
        unify i ta (TyFun tArg tv1)
        pure (tv1,fnArg':xs,ps ++ predsArg)
        -- as <- traverse inferTerm args
        -- let tys = view _1 <$> as
        --     args' = view _2 <$> as
        --     preds' = concat (pte : NE.toList (view _3 <$> as))
        -- unify te (foldr TyFun tv1 tys) i
        -- pure (tv1, App e' args' i, preds')
  IR.Let (IR.Arg n mty) e1 e2 i -> do
    enterLevel
    (te1, e1', pe1) <- case mty of
      Nothing -> inferTerm e1
      Just ty -> checkTermType (liftCoreType ty) e1
    leaveLevel
    -- Note: generalization is turned off.
    -- (ts, e1Qual, deferred) <- generalizeWithTerm te1 pe1 e1Unqual
    (te2, e2', pe2) <- locally tcVarEnv (RAList.cons te1) $ inferTerm e2
    pure (te2, Let (Arg n te1) e1' e2' i, pe1 ++ pe2)
  IR.Sequence e1 e2 i -> do
    (_, e1', pe1) <- inferTerm e1
    (te2, e2', pe2) <- inferTerm e2
    pure (te2, Sequence e1' e2' i, pe1 ++ pe2)
  -- Todo: Here, convert to dictionary
  IR.CapabilityForm cf i -> over _2 (`CapabilityForm` i) <$> case cf of
    WithCapability c_token te -> do
      (tyToken, c_token', p1) <- inferTerm c_token
      tv <- newTvRef
      let tvar = TypeVar tv UserDefKind
          ty = TyCapToken (CapVar tvar)
      unify i tyToken ty
      (ty', te', p2) <- inferTerm te
      pure (ty', WithCapability c_token' te', p1 ++ p2)
    CreateUserGuard ne args -> do
      -- Todo: use a smart ctor for this
      (_, Apply _ args' _, pe1) <- inferApply (Apply (IR.Var ne i) args i)
      pure (TyGuard, CreateUserGuard ne args', pe1)

  IR.Conditional cond i -> over _2 (`Conditional` i) <$>
    case cond of
      CAnd e1 e2 -> do
        (_, e1', pe1) <- checkTermType TyBool e1
        (_, e2', pe2) <- checkTermType TyBool e2
        pure (TyBool, CAnd e1' e2', pe1 ++ pe2)
      COr e1 e2 -> do
        (_, e1', pe1) <- checkTermType TyBool e1
        (_, e2', pe2) <- checkTermType TyBool e2
        pure (TyBool, COr e1' e2', pe1 ++ pe2)
      CIf c e1 e2 -> do
        (_, c', pc) <- checkTermType TyBool c
        (te1, e1', pe1) <- inferTerm e1
        (te2, e2', pe2) <- inferTerm e2
        unify i te1 te2
        pure (te1, CIf c' e1' e2', pc ++ pe1 ++ pe2)
      CEnforce e str -> do
        (_, e', pe1) <- checkTermType TyBool e
        (_, str', pe2) <- checkTermType TyString str
        pure (TyBool, CEnforce e' str', pe1 ++ pe2)
      CEnforceOne e eli -> do
        (_, e', pe1) <- checkTermType TyString e
        condsList <- traverse (checkTermType TyBool) eli
        let ps = concat (view _3 <$> condsList)
            eli' = view _2 <$> condsList
        pure (TyBool, CEnforceOne e' eli', pe1 ++ ps)
  IR.Builtin b i -> do
    let tyImported = typeOfBuiltin b
    (ty, tvs, preds) <- instantiateImported tyImported i
    let tvs' = TyVar <$> tvs
    let term' = Builtin (b, tvs', preds) i
    pure (ty, term', preds)
  -- TODO: note,
  -- for this to work, we have to have proper bidirectionality working, including scoped type variables working fine
  IR.Constant lit i -> case lit of
    LInteger{} -> do
      tv <- TyVar . (`TypeVar` TyKind) <$> newTvRef
      let p = Pred (Num tv)
      pure (tv, Constant lit i, [p])
    _ ->
      pure (typeOfLit lit, Constant lit i, [])
  IR.ListLit li i -> do
    tv <- TyVar . (`TypeVar` TyKind) <$> newTvRef
    liTup <- traverse inferTerm li
    let preds = concat (view _3 <$> liTup)
    traverse_ (\(t,_, _) -> unify i tv t) liTup
    pure (TyList tv, ListLit tv (view _2 <$> liTup) i, preds)
  IR.Try e1 e2 i -> do
    (te1, e1', p1) <- inferTerm e1
    (te2, e2', p2)<- inferTerm e2
    unify i te1 te2
    pure (te1, Try e1' e2' i, p1 ++ p2)
  IR.Nullary e i -> do
    (te', e', p) <- inferTerm e
    pure (TyNullary te', Lam [] e' i, p)
  IR.ObjectLit fieldMap i -> do
    m <- traverse (\(f, t) -> (f,) <$> inferTerm t) fieldMap
    let objTyMap = view _1 <$> M.fromList m
        objTerms = over _2 (view _2) <$> m
        objTy = TyObject (RowConcrete objTyMap)
        preds = concat (view (_2._3) <$> m)
    pure (objTy, ObjectLit objTerms i, preds)

inferApply
  :: TypeOfBuiltin b
  => Apply (IRTerm b i) i
  -> InferM s b i (TCType s, Apply (TCTerm s b i) i, [TCPred s])
inferApply (Apply fun args i) = case args of
  [] -> do
    tv1 <- TyVar . (`TypeVar` TyKind) <$> newTvRef
    (tfun, fun', pe1) <- inferTerm fun
    unify i (TyNullary tv1) tfun
    pure (tv1, Apply fun' [] i, pe1)
  h:hs -> do
    (tfun, te', pe1) <- inferTerm fun
    (rty, xs, ps) <- foldlM inferFunctionArgs (tfun,[], []) (h:hs)
    let term' = Apply te' (reverse xs) i
    pure (rty, term', pe1 ++ ps)
    where
    inferFunctionArgs (ta, xs, ps) fnArg = case ta of
      TyFun arg ret -> do
        (_, x', p) <- checkTermType arg fnArg
        pure (ret, x':xs, ps ++ p)
      _ -> do
        tv1 <- TyVar . (`TypeVar` TyKind) <$> newTvRef
        (tArg, fnArg', predsArg) <- inferTerm fnArg
        unify i ta (TyFun tArg tv1)
        pure (tv1,fnArg':xs,ps ++ predsArg)

enforceTypeIsPresent :: Maybe IR.Type -> Type a
enforceTypeIsPresent = \case
  Just ty -> liftCoreType ty
  _ -> error "pact-core requires type annotations"

enforceArgType :: IR.Arg IR.Type -> Arg a
enforceArgType (IR.Arg n ty) =
  Arg n (enforceTypeIsPresent ty)

-- -- Todo: generic types?
-- -- We can't generalize yet since
-- -- we're not allowing type schemes just yet.
inferDefun
  :: TypeOfBuiltin b
  => ModuleName
  -> ModuleHash
  -> IR.Defun Name IR.Type b i
  -> InferM s b i (TypedDefun b i)
inferDefun mn mh (IR.Defun name dfargs dfRetType term info) = do
  let typedArgs = enforceArgType <$> dfargs
      rty = enforceTypeIsPresent dfRetType
  enterLevel
  let dfTy = foldr TyFun rty (_argType <$> typedArgs)
  (termTy, term', preds) <- inferTerm term
  leaveLevel
  checkReducible preds (view IR.termInfo term)
  -- fail "typeclass constraints not supported in defun"
  unify info (liftType dfTy) termTy
  fterm <- noTyVarsinTerm info term'
  tcFree %= M.insert (FullyQualifiedName mn name mh) dfTy
  pure (Defun name (fmap absurd <$> typedArgs) (liftType rty) fterm info)

inferDefConst
  :: ModuleName
  -> ModuleHash
  -> IR.DefConst Name IR.Type b i
  -> InferM s b i (TypedDefConst b i)
inferDefConst mn mh (IR.DefConst name dcTy cv info) = case cv of
  IR.EvaledConst v -> do
    pvt <- inferPactValue info v
    let dcTy' = liftCoreType <$> dcTy
    _ <- traverse (unify info pvt) dcTy'
    rty <- ensureNoTyVars info (maybe pvt id dcTy')
    tcFree %= M.insert (FullyQualifiedName mn name mh) rty
    pure (DefConst name (liftType rty) v info)
  _ -> error "invariant violated: defconst not evalted"

inferPactValue :: i -> PactValue -> InferM s b i (TCType s)
inferPactValue i = \case
  PLiteral l -> pure $ typeOfLit l
  PGuard _ -> pure $ TyGuard
  PObject o ->
    TyObject . RowConcrete <$>
      traverse (inferPactValue i) o
  PList l -> do
    v <- TyVar . (`TypeVar` TyKind) <$> newTvRef
    _ <- traverse (inferPactValue i >=> unify i v) l
    pure (TyList v)
  PModRef mr -> case _mrRefined mr of
    Just s -> pure (TyModRef s)
    Nothing -> error "cannot infer non-refined modref"
  PCapToken ct ->
    pure $ TyCapToken $ CapConcrete $ fqnToQualName $ _ctName ct
  PTime _ -> pure $ TyTime




inferDefCap
  :: TypeOfBuiltin b
  => ModuleName
  -> ModuleHash
  -> IR.DefCap Name IR.Type b i
  -> InferM s b i (TypedDefCap b i)
inferDefCap mn mh (IR.DefCap name args rty term meta i) = do
  let args' = enforceArgType <$> args
  rty' <- maybe (TyVar . (`TypeVar` TyKind) <$> newTvRef) (pure . liftCoreType) rty
  let ty = foldr TyFun rty' (liftType . _argType <$> args')
  (termTy, term', preds) <- inferTerm term
  checkReducible preds i
  unify i ty termTy
  fterm <- noTyVarsinTerm i term'
  rtyf <- ensureNoTyVars i rty'
  let argsF = over argType liftType <$> args'
  let capRty = foldr TyFun (TyCapToken (CapConcrete (QualifiedName name mn))) (_argType <$> args')
  tcFree %= M.insert (FullyQualifiedName mn name mh) capRty
  pure (DefCap name argsF rtyf fterm meta i)

inferTable
  :: ModuleName
  -> ModuleHash
  -> IR.DefTable Name i
  -> InferM s b i (DefTable i)
inferTable mn mh (IR.DefTable tn (IR.ResolvedTable (IR.Schema qn schema)) info) = do
  let schema' = liftCoreType <$> schema
  tcFree %= M.insert (FullyQualifiedName mn tn mh) (TyTable (RowConcrete schema'))
  pure (DefTable tn (Schema qn schema') info)

inferDefSchema :: IR.DefSchema IR.Type info -> DefSchema ty info
inferDefSchema (IR.DefSchema n ds i) = DefSchema n (liftCoreType <$> ds) i

inferDef
  :: TypeOfBuiltin b
  => ModuleName
  -> ModuleHash
  -> IR.Def Name IR.Type b i
  -> InferM s b i (TypedDef b i)
inferDef mn mh = \case
  IR.Dfun d -> Dfun <$> inferDefun mn mh d
  IR.DConst d -> DConst <$> inferDefConst mn mh d
  IR.DCap dc -> DCap <$> inferDefCap mn mh dc
  IR.DTable tbl -> DTable <$> inferTable mn mh tbl
  IR.DSchema s -> pure $ DSchema (inferDefSchema s)
  IR.DPact{} -> error "todo: defpacts"

inferIfDef
  :: IR.IfDef Name IR.Type b i
  -> InferM s b' i (TypedIfDef b i)
inferIfDef = error "todo: implement"

inferModule
  :: TypeOfBuiltin b
  => IR.Module Name IR.Type b i
  -> InferM s b i (TypedModule b i)
inferModule (IR.Module mname mgov defs blessed imports impl mh info) = do
  defs' <- traverse (inferDef mname mh) defs
  pure (Module mname mgov defs' blessed imports impl mh info)

-- inferInterface
--   :: TypeOfBuiltin b
--   => IRInterface b info
--   -> InferM s b' info (TypedInterface b info)
-- inferInterface (IR.Interface n defns h info) = do
--   defns' <- traverse inferIfDef defns
--   pure (Interface n defns' h info)

-- | Note: debruijnizeType will
-- ensure that terms that are generic will fail
inferTermNonGen
  :: TypeOfBuiltin b
  => IRTerm b i
  -> InferM s b i (TypeScheme NamedDeBruijn, TypedTerm b i)
inferTermNonGen t = do
  (ty, t', preds) <- inferTerm t
  checkReducible preds (view IR.termInfo t)
  tys <- ensureNoTyVars (view IR.termInfo t) ty
  tt <- noTyVarsinTerm (view IR.termInfo t) t'
  pure (TypeScheme [] [] tys, tt)

-- inferTermGen
--   :: TypeOfBuiltin b
--   => IRTerm b i
--   -> InferM s b' i (TypeScheme NamedDeBruijn, TypedGenTerm b i)
-- inferTermGen term = do
--   let info = view IR.termInfo term
--   enterLevel
--   (ty, term0 , preds) <- inferTerm term
--   leaveLevel
--   (tys', typedTerm, deferred) <- generalizeWithTerm ty preds term0
--   unless (null deferred) $ do
--       deferred' <- traverse _dbgPred deferred
--       throwTypecheckError (UnsupportedTypeclassGeneralization deferred') (view IR.termInfo term)
--   dbjTyScheme <- debruijnizeTypeScheme info tys'
--   dbjTerm <- debruijnizeTermTypes info typedTerm
--   pure (dbjTyScheme, dbjTerm)

inferTopLevel
  :: TypeOfBuiltin b
  => IRTopLevel b i
  -> InferM s b i (TypedTopLevel b i)
inferTopLevel = \case
  IR.TLModule m ->
    TLModule <$> inferModule m
  IR.TLTerm m -> TLTerm . snd <$> inferTermNonGen m
  IR.TLInterface _i -> undefined
  IR.TLUse tl i -> pure (TLUse tl i)

inferReplTopLevel
  :: TypeOfBuiltin b
  => IR.ReplTopLevel Name IR.Type b i
  -> InferM s b i (TypedReplTopLevel b i)
inferReplTopLevel = \case
  IR.RTLDefun dfn -> do
    dfn' <- inferDefun replModuleName replModuleHash dfn
    pure (RTLDefun dfn')
  IR.RTLDefConst dconst -> do
    dc <- inferDefConst replModuleName replModuleHash dconst
    pure (RTLDefConst dc)



-- | Transform types into their debruijn-indexed version
-- Essentially: Start at depth 0:
--  rename : (Term, Γ, Int) -> IxTerm
--  rename (ΛX.e, tyEnv, DEPTH) = Λ. (rename (e, tyEnv[depth/X], DEPTH+1))
--  .. other cases are simply renaming recursively and calling `renameType`
--  on occurences of Type
--
--  NOTE: the passed in DEPTH is 1 higher than the highest binder.
--
--  renameType : (Type, Γ, Int) -> IxType
--  renameType (a, env, DEPTH) = DEPTH - env(a) - 1
--  .. other recursive cases are straightforward
--
--  Quip: when we debruijnize types, we expect no impredicative polymorphism atm,
--  thus we will fail on universially quantified types found in application and
--  var binding sites.
--  The typechecker does not spit out impredicative polymorphism, but while
--  it would be trivial to support their renaming here, I'd rather fail
--  for now as the typechecker does not support it and it functions as a sanity check
debruijnizeTermTypes
  :: forall s b b' i. i
  -> TCTerm s b i
  -> InferM s b' i (Term Name NamedDeBruijn (b, [Type NamedDeBruijn], [Pred (Type NamedDeBruijn)]) i)
debruijnizeTermTypes info = dbj [] 0
  where
  dbj
    :: [(TCTypeVar s, NamedDeBruijn)]
    -> DeBruijn
    -> TCTerm s b i
    -> InferM s b' i (Term Name NamedDeBruijn (b, [Type NamedDeBruijn], [Pred (Type NamedDeBruijn)]) i)
  dbj env depth = go
    where
    go = \case
      Var n i ->
        pure (Var n i)
      Lam nts e i -> do
        nts' <- (traversed.argType) (dbjTyp info env) nts
        e' <- dbj env depth e
        pure (Lam nts' e' i)
      App l r i ->
        App <$> dbj env depth l <*> traverse (dbj env depth) r <*> pure i
      Let n e1 e2 i -> do
        n' <- argType (dbjTyp info env) n
        e1' <- go e1
        e2' <- go e2
        pure (Let n' e1' e2' i)
      TyAbs ntys e i -> do
        let len = fromIntegral (NE.length ntys)
            ixs = NE.fromList [depth .. depth + len - 1]
        names <- traverse (nameTvs info (depth + len)) (NE.zip ntys ixs)
        let env' = NE.toList $ NE.zip ntys names
        TyAbs names <$> dbj (env' ++ env) (depth + len) e <*> pure i
      TyApp e args i -> do
        e' <- dbj env depth e
        args' <- traverse (dbjTyp info env) args
        pure (TyApp e' args' i)
      Sequence e1 e2 i ->
        Sequence <$> dbj env depth e1 <*> dbj env depth e2 <*> pure i
      Conditional c i ->
        Conditional <$> traverse (dbj env depth) c <*> pure i
      Try e1 e2 i ->
        Try <$> dbj env depth e1 <*> dbj env depth e2 <*> pure i
      ListLit ty v i ->
        ListLit <$> dbjTyp info env ty <*> traverse (dbj env depth) v <*> pure i
      Builtin (b, tys, preds) i -> do
        tys' <- traverse (dbjTyp info env) tys
        preds' <- traverse (dbjPred info env) preds
        pure (Builtin (b, tys', preds') i)
      CapabilityForm cf i ->
        CapabilityForm <$> traverse (dbj env depth) cf <*> pure i
      Constant l i -> pure (Constant l i)
      ObjectLit l i ->
        ObjectLit <$> (traversed._2) go l <*> pure i


nameTvs
  :: i
  -> DeBruijn
  -> (TCTypeVar s, DeBruijn)
  -> InferM s b i NamedDeBruijn
nameTvs _info depth (nt, i) = readTvRef nt >>= \case
  Bound n _ ->
    pure (NamedDeBruijn (depth - i - 1) n)
  _ ->
    error "type var unbound"
    -- throwTypecheckError (TCInvariantFailure "Found unbound variable during generalization") info

ensureNoTyVars
  :: i
  -> TCType s
  -> InferM s b i (Type a)
ensureNoTyVars i = \case
  TyVar n -> readTvRef n >>= \case
    LinkTy ty -> ensureNoTyVars i ty
    _ ->
      error "Inferred generic signature"
      -- throwTypecheckError (DisabledGeneralization "Inferred generic signature") i
  TyPrim p -> pure (TyPrim p)
  TyFun l r -> TyFun <$> ensureNoTyVars i l <*> ensureNoTyVars i r
  TyList l -> TyList <$> ensureNoTyVars i l
  TyModRef mr -> pure (TyModRef mr)
  TyObject r -> TyObject <$> ensureNoTvRowCtor r
  TyTable r -> TyTable <$> ensureNoTvRowCtor r
  TyNullary r -> TyNullary <$> ensureNoTyVars i r
  TyCapToken r -> case r of
    CapVar rv -> readTvRef rv >>= \case
      LinkCap t -> ensureNoTyVars i (TyCapToken t)
      _ -> error "inferred generic signature"
    CapConcrete fq -> pure (TyCapToken (CapConcrete fq))
  where
  ensureNoTvRowCtor (RowVar rv) = readTvRef rv >>= \case
    LinkRow row -> ensureNoTvRowCtor row
    _ -> error "inferred generic signature"
  ensureNoTvRowCtor (RowConcrete row) =
    RowConcrete <$> traverse (ensureNoTyVars i) row

ensureNoTyVarsPred
  :: i
  -> TCPred s
  -> InferM s b i (Pred (Type NamedDeBruijn))
ensureNoTyVarsPred i (Pred tc) =
  Pred <$> traverse (ensureNoTyVars i) tc

noTyVarsinTerm
  :: i
  -> TCTerm s b' i
  -> InferM s b i (TypedTerm b' i)
noTyVarsinTerm info = \case
  Var n i ->
    pure (Var n i)
  Lam nts e i ->
    Lam <$> (traversed.argType) (ensureNoTyVars info) nts <*> noTyVarsinTerm info e <*> pure i
  App e args i ->
    App <$> noTyVarsinTerm info e <*> traverse (noTyVarsinTerm info) args <*> pure i
  Let n e1 e2 i ->
    Let <$> argType (ensureNoTyVars i) n
        <*> noTyVarsinTerm info e1
        <*> noTyVarsinTerm info e2
        <*> pure i
  Builtin (b, ty, p) i -> do
    ty' <- traverse (ensureNoTyVars info) ty
    p' <- traverse (ensureNoTyVarsPred info) p
    pure $ Builtin (b, ty', p') i
  TyAbs _ns _e _i ->
    error "Generic terms are disabled"
  Constant l i ->
    pure (Constant l i)
  TyApp l tys i ->
    TyApp
      <$> noTyVarsinTerm info l
      <*> traverse (ensureNoTyVars info) tys
      <*> pure i
  Sequence e1 e2 i ->
    Sequence <$> noTyVarsinTerm info e1 <*> noTyVarsinTerm info e2 <*> pure i
  Conditional c i ->
    Conditional <$> traverse (noTyVarsinTerm info) c <*> pure i
  ListLit ty li i ->
    ListLit <$> ensureNoTyVars info ty <*> traverse (noTyVarsinTerm info) li <*> pure i
  Try e1 e2 i ->
    Try <$> noTyVarsinTerm info e1 <*> noTyVarsinTerm info e2 <*> pure i
  CapabilityForm cf i ->
    CapabilityForm <$> traverse (noTyVarsinTerm info) cf <*> pure i
  ObjectLit fields i ->
    ObjectLit <$> (traversed._2) (noTyVarsinTerm info) fields <*> pure i

debruijnizeTypeScheme
  :: i
  -> TypeScheme (TCTypeVar s)
  -> InferM s b i (TypeScheme NamedDeBruijn)
debruijnizeTypeScheme i (TypeScheme tvs preds t) = do
    let len = fromIntegral (length tvs)
    let ixs = [0.. len - 1]
    names <- traverse (nameTvs i len) (zip tvs ixs)
    let env = zip tvs names
    t' <- dbjTyp i env t
    preds' <- traverse (dbjPred i env) preds
    pure (TypeScheme names preds' t')

dbjPred
  :: i
  -> [(TCTypeVar s, NamedDeBruijn)]
  -> TCPred s
  -> InferM s b i (Pred (Type NamedDeBruijn))
dbjPred i env (Pred tc) = do
  Pred <$> traverse (dbjTyp i env) tc

dbjTyp
  :: i
  -> [(TCTypeVar s, NamedDeBruijn)]
  -> TCType s
  -> InferM s b i (Type NamedDeBruijn)
dbjTyp _i env = go
  where
  go = \case
    TyVar n -> case lookup n env of
      Just v -> pure (TyVar v)
      Nothing -> readTvRef n >>= \case
        Unbound {} ->
          error "Found unbound type variable after type checking"
        Bound{} ->
          error "Found bound variable outside of the calculated set"
        LinkTy ty -> go ty
        _ -> error "invariant failure: typevar as row"
    TyPrim p -> pure (TyPrim p)
    TyFun l r -> TyFun <$> go l <*> go r
    TyList l -> TyList <$> go l
    TyModRef m -> pure (TyModRef m)
    TyObject r -> TyObject <$> dbjRowCtor r
    TyTable r -> TyTable <$> dbjRowCtor r
    TyNullary t -> TyNullary <$> go t
    TyCapToken r -> case r of
      CapVar cv -> case lookup cv env of
        Just v -> pure (TyCapToken (CapVar v))
        Nothing -> readTvRef cv >>= \case
          LinkCap cv' -> go (TyCapToken cv')
          _ -> error "invariant failure"
      CapConcrete fq -> pure (TyCapToken (CapConcrete fq))
  dbjRowCtor = \case
    RowVar rv ->  case lookup rv env of
      Just v -> pure (RowVar v)
      Nothing -> readTvRef rv >>= \case
        Unbound {} ->
          error "Found unbound type variable after type checking"
        Bound{} ->
          error "Found bound variable outside of the calculated set"
        LinkRow row -> dbjRowCtor row
        _ -> error "invariant failure"
    RowConcrete r ->
      RowConcrete <$> traverse go r

-- -- -----------------------------------------
-- -- --- Built-in type wiring
-- -- ------------------------------------------

-- -- mkFree :: Loaded builtin info -> Map ModuleName (Map Text (Type Void))
-- -- mkFree loaded = let
-- --   tl = _loModules loaded
-- --   toTy d = (UndefName d, UndefType d)
-- --   mdefs =  Un_mDefs . _mdModule <$> tl
-- --   in M.fromList . fmap toTy <$> mdefs

runInfer
  :: Loaded b i
  -> TCState b i
  -> InferM s b i a
  -> ST s (Either (TypecheckFailure i) a, TCState b i)
runInfer lo tcs (InferM act) = do
  uref <- newSTRef 0
  lref <- newSTRef 1
  let env = TCEnv uref mempty lref lo
  runStateT (runReaderT (runExceptT act) env) tcs

runInferTerm
  :: TypeOfBuiltin b
  => Loaded b i
  -> TCState b i
  -> IRTerm b i
  -> Either (TypecheckFailure i) (TypeScheme NamedDeBruijn, TypedTerm b i)
runInferTerm lo tcs term0 =
  fst $ runST $ runInfer lo tcs (inferTermNonGen term0)

-- runInferTermNonGen
--   :: TypeOfBuiltin b
--   => Loaded b' i
--   -> IRTerm b i
--   -> Either (PactError i) (TypeScheme NamedDeBruijn, TypedTerm b i)
-- runInferTermNonGen loaded term0 = runST $
--   runInfer loaded $ inferTermNonGen term0

runInferModule
  :: TypeOfBuiltin b
  => Loaded b i
  -> TCState b i
  -> IRModule b i
  -> (Either (TypecheckFailure i) (TypedModule b i), TCState b i)
runInferModule lo tcs m =
  runST $ runInfer lo tcs (inferModule m)

runInferTopLevel
  :: TypeOfBuiltin b
  => Loaded b i
  -> TCState b i
  -> IR.TopLevel Name IR.Type b i
  -> (Either (TypecheckFailure i) (TypedTopLevel b i), TCState b i)
runInferTopLevel l tcs tl =
  runST $ runInfer l tcs (inferTopLevel tl)


runInferReplTopLevel
  :: TypeOfBuiltin b
  => Loaded b i
  -> TCState b i
  -> IR.ReplTopLevel Name IR.Type b i
  -> (Either (TypecheckFailure i) (TypedReplTopLevel b i), TCState b i)
runInferReplTopLevel l tcs tl =
  runST $ runInfer l tcs (inferReplTopLevel tl)
