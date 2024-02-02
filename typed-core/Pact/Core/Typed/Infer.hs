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
module Pact.Core.Typed.Infer where
--  ( runInferTerm
--  , runInferTermNonGen
--  , runInferModule
--  , runInferTopLevel
--  , runInferReplTopLevel
--  , TypeOfBuiltin(..)
--  ) where

import Control.Lens hiding (Level)
import Control.Monad ( when, unless, zipWithM )
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
import Data.Functor(($>))
import Data.STRef
import Data.Maybe(mapMaybe)
import Data.Map(Map)
import Data.Text(Text)
import Data.List.NonEmpty(NonEmpty(..))

import qualified Data.Map.Strict as M
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.RAList as RAList
import qualified Data.Set as Set

import Pact.Core.Builtin
import Pact.Core.Names
import Pact.Core.Errors
import Pact.Core.Persistence
import Pact.Core.Capabilities
import qualified Pact.Core.IR.Term as IR
import qualified Pact.Core.Type as IR
-- import qualified Pact.Core.Term as Typed

import Pact.Core.Typed.Term
import Pact.Core.Typed.Type

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
type TCPred s = Pred (TCTypeVar s)
type TCRowCtor s = RowCtor (TCTypeVar s)

data Tv s
  = Unbound !Text !Unique !Level
  | Bound !Text !Unique
  | LinkTy !(TCType s)
  | LinkRow !(RowCtor (TCTypeVar s))
  | LinkCap !(CapRef (TCTypeVar s))
  deriving Eq

-- Note: TyVar equality
-- is reference equality
newtype TvRef s =
  TvRef (STRef s (Tv s))
  deriving Eq

data TCEnv s b i
  = TCState
  { _tcSupply :: UniqueSupply s
  -- ^ Supply for fresh variables.
  , _tcVarEnv :: RAList (TCType s)
  -- ^ Builtins map, that uses the enum instance
  -- , _tcFree :: Map ModuleName (Map Text (Type Void))
  , _tcFree :: Map FullyQualifiedName (Type Void)
  -- ^ Free variables
  , _tcLevel :: STRef s Level
  -- ^ Type Variable "Region"
  , _tcModules :: Map ModuleName (ModuleData b i)
  }

makeLenses ''TCEnv

-- | Term emitted by desugar
type IRTerm b i = IR.Term Name IR.Type b i
type IRModule b i = IR.Module Name b i
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
  InferT (ExceptT (TypecheckFailure i) (ReaderT (TCEnv s b i) (ST s)) a)
  deriving
    ( Functor, Applicative, Monad
    , MonadReader (TCEnv s b i)
    , MonadError (TypecheckFailure i))
  via (ExceptT (TypecheckFailure i) (ReaderT (TCEnv s b i) (ST s)))

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
      in TypeScheme [aVar] [Pred Fractional a] (a :~> a :~> a)
    -- ListLike
    CoreLength ->
      let aVar = nd "a" 0
          a = TyVar aVar
      in TypeScheme [aVar] [Pred ListLike a] (a :~> TyInt)
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
      in TypeScheme [aVar] [Pred Eq a] (a :~> TyList a :~> TyBool)
    CoreSort ->
      let aVar = nd "a" 0
          a = TyVar aVar
      in TypeScheme [aVar] [Pred Ord a] (TyList a :~> TyList a)
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
      in TypeScheme [aVar] [Pred Ord a] (TyList a :~> TyList a)
    CoreFormat ->
      error "todo: format not supported"
    CoreEnumerate ->
      NonGeneric (TyInt :~> TyInt :~> TyList TyInt)
    CoreEnumerateStepN ->
      NonGeneric (TyInt :~> TyInt :~> TyList TyInt)
    CoreShow ->
      let aVar = nd "a" 0
          a = TyVar aVar
      in TypeScheme [aVar] [Pred Ord a] (TyList a :~> TyList a)
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
      in TypeScheme [aVar] [Pred EnforceRead a] (a :~> TyBool)
    CoreEnforceKeyset ->
      let aVar = nd "a" 0
          a = TyVar aVar
      in TypeScheme [aVar] [Pred EnforceRead a] (a :~> TyBool)
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
    where
    nd = flip TypeVariable
    unaryNumType =
      let aVar = nd "a" 0
          a = TyVar aVar
      in TypeScheme [aVar] [Pred Num a] (a :~> a)
    unaryFractional =
      let aVar = nd "a" 0
          a = TyVar aVar
      in TypeScheme [aVar] [Pred Fractional a] (a :~> TyDecimal)
    addBinopType =
      let aVar = nd "a" 0
          a = TyVar aVar
      in TypeScheme [aVar] [Pred Add a] (a :~> a :~> a)
    numBinopType =
      let aVar = nd "a" 0
          a = TyVar aVar
      in TypeScheme [aVar] [Pred Num a] (a :~> a :~> a)
    eqTyp =
      let aVar = nd "a" 0
          a = TyVar aVar
      in TypeScheme [aVar] [Pred Eq a] (a :~> a :~> TyBool)
    ordTyp =
      let aVar = nd "a" 0
          a = TyVar aVar
      in TypeScheme [aVar] [Pred Ord a] (a :~> a :~> TyBool)
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
      in TypeScheme [aVar] [Pred ListLike a] (TyInt :~> a :~> a)

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
liftST action = InferT (lift (lift action))

-- throwTypecheckError :: TypecheckError -> i -> InferM s b i a
-- throwTypecheckError te = throwError . PETypecheckError te

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
byInst (Pred p ty) = case p of
  Eq -> eqInst ty
  Add -> addInst ty
  Num -> numInst ty
  Ord -> ordInst ty
  Show -> showInst ty
  ListLike -> listLikeInst ty
  Fractional -> fractionalInst ty
  _ -> error "boom"

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
--  instance (Eq {l1:t1, .., ln:tn}) where t1..tn are monotypes without type variables.
--
eqInst :: TCType s -> InferM s b i (Maybe [TCPred s])
eqInst = \case
  TyVar tv -> readTvRef tv >>= \case
    LinkTy ty -> eqInst ty
    _ -> pure Nothing
  -- All prims have an EQ instance
  TyPrim _ -> pure (Just [])
  TyModRef _ -> pure (Just [])
  TyList t -> pure (Just [Pred Eq t])
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
  TyList t -> pure (Just [Pred Ord t])
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
  TyList t -> pure (Just [Pred Show t])
  _ -> pure Nothing

entail :: [TCPred s] -> TCPred s -> InferM s b i Bool
entail ps p = byInst p >>= \case
  Nothing -> pure False
  Just qs -> and <$> traverse (entail ps) qs

isHnf :: TCPred s -> InferM s b i Bool
isHnf (Pred c t) = case t of
  TyVar tv -> readTvRef tv >>= \case
    LinkTy ty -> isHnf (Pred c ty)
    _ -> pure True
  _ -> pure False

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
  partition' (ds, rs) (p@(Pred _ ty) : xs) = do
    cond <- hasUnbound ty
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

-- -- | Instantiate a typescheme with bound variables with fresh bound variables
-- -- Corresponds to the following inference rule
-- --
-- -- That is (∀E):
-- --     P | Γ ⊢ E : ∀a_1..a_n. ρ
-- --     b_1, ..,  b_n fresh
-- --     ---------------------------------------
-- --     P | Γ ⊢ E ~> E_f[b_1,..,b c_n] : ρ
-- -- instantiateWithTerm
-- --   :: TypeScheme (TvRef s)
-- --   -> TCTerm s b i
-- --   -> InferM s b i (TCType s, TCTerm s b i, [TCPred s])
-- -- instantiateWithTerm (TypeScheme ts preds ty) term = do
-- --   nts <- fmap TyVar <$> traverse (const newTvRef) ts
-- --   let m = zip ts nts
-- --   preds' <- traverse (instPred m) preds
-- --   ty' <- instBound m ty
-- --   case nts of
-- --     x:xs -> do
-- --       let tyapps = TyApp term (x:|xs) info
-- --       dvars <- traverse toDVar preds'
-- --       case dvars of
-- --         p:ps -> pure (ty', App tyapps (p:|ps) info, preds')
-- --         [] -> pure (ty', tyapps, [])
-- --     [] -> pure (ty', term, [])
-- --   where
-- --   info = term ^. termInfo
--   -- toDVar p = do
--   --   i <- newSupplyIx
--   --   let n = OverloadedName ("_dict" <> T.pack (show i)) (OBuiltinDict p)
--   --   pure $ Var n info
--   -- instPred m (Pred n tt) =
--   --   Pred n <$> instBound m tt
--   -- instBound m = \case
--   --   t@(TyVar tv) -> readTvRef tv >>= \case
--   --     Bound{} -> case lookup tv m of
--   --       Just t' -> pure t'
--   --       Nothing -> pure t
--   --     Link lt -> instBound m lt
--   --     _ -> pure t
--   --   TyPrim p -> pure (TyPrim p)
--   --   TyFun l r ->
--   --     TyFun <$> instBound m l <*> instBound m r
--   --   TyList t -> TyList <$> instBound m t
--   --   t -> pure t

instantiateImported
  :: TypeScheme DebruijnTypeVar
  -> i
  -> InferM s b' i (TCType s, [TCTypeVar s], [TCPred s])
instantiateImported (TypeScheme tvs preds ty) _i = do
    ntvs <- traverse (const newTvRef) tvs
    let ntvs' = zipWith (\tvr (TypeVar _ kind) -> TypeVar tvr kind) ntvs tvs
    let rl = RAList.fromList (reverse ntvs)
    ty' <- inst rl ty
    preds' <- traverse (instPred rl) preds
    pure (ty', ntvs', preds')
  where
  instPred rl (Pred tc pty) = do
    Pred <$> traverse (instNamed rl) tc <*> inst rl pty
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
  :: TCTypeVar s
  -> TCType s
  -> i
  -> InferM s b' i ()
occurs tv tct i = case tct of
  TyVar tv' | tv == tv' ->
    error "error: infinite type"
    -- tv'' <- _dbgType tct
    -- throwTypecheckError (OccursCheckFailure tv'') i
  TyVar tv' -> bindRef tv'
  TyFun l r -> occurs tv l i *> occurs tv r i
  TyList l -> occurs tv l i
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
    LinkTy ty -> occurs tv ty i
    _ -> pure ()

unifyTyVar
  :: TCTypeVar s
  -> TCType s
  -> i
  -> InferM s b' i ()
unifyTyVar tv t1 i = readTvRef tv >>= \case
  Unbound{} -> do
    occurs tv t1 i
    writeTvRef tv (LinkTy t1)
  LinkTy t2 -> unify t2 t1 i
  _ -> pure ()

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
  :: TCType s
  -> TCType s
  -> i
  -> InferM s b' i ()
unify t1 t2 _ | t1 == t2 = pure ()
unify (TyVar tv) t i = unifyTyVar tv t i
unify t (TyVar tv) i = unifyTyVar tv t i
unify (TyFun l r) (TyFun l' r') i = unify l l' i *> unify r r' i
unify (TyList t) (TyList t') i = unify t t' i
unify (TyPrim p) (TyPrim p') _ | p == p' = pure ()
unify (TyModRef mr) (TyModRef mr') _ | mr == mr' = pure ()
unify _t1 _t2 _i =
  error "unification error"
  -- t1' <- _dbgType t1
  -- t2' <- _dbgType t2
  -- throwTypecheckError (UnificationError t1' t2') i

-- -- | We essentially only
-- -- generalize on lambdas atm.
generalizeWithTerm
  :: TCType s
  -> [TCPred s]
  -> TCTerm s b i
  -> InferM s b i (TypeScheme (TCTypeVar s), TCTerm s b i, [TCPred s])
generalizeWithTerm ty pp term
  | isValue term = generalizeWithTerm' ty pp term
  | otherwise = do
    pp' <- reduce pp (view termInfo term)
    pure (TypeScheme [] [] ty, term, pp')
  where
  isValue = \case
    Var{} -> True
    Constant{} -> True
    Lam{} -> True
    -- Error{} -> True
    Builtin{} -> True
    _ -> False

-- Generalization that emits a typed term
-- Note: Deferred predicates are purely for the sake of
-- callsite dictionary overloaded variables.
-- These are currently disabled.
generalizeWithTerm'
  :: TCType s
  -> [TCPred s]
  -> TCTerm s b i
  -> InferM s b i (TypeScheme (TCTypeVar s), TCTerm s b i, [TCPred s])
generalizeWithTerm' ty pp term = do
  preds <- nubPreds pp
  ((ftvs, ty'), s) <- runStateT (gen' ty) Set.empty
  (deferred, retained) <- split preds (view termInfo term)
  retained' <- evalStateT (traverse genPred retained) s
  -- when (retained' /= []) $
    -- retained'' <- traverse _dbgPred retained
    -- throwTypecheckError (UnsupportedTypeclassGeneralization retained'') info
  case ftvs of
    [] -> do
      pure (TypeScheme [] retained' ty' , term, deferred)
    (x:xs) -> do
      pure (TypeScheme ftvs retained' ty', TyAbs (x :| xs) term info, deferred)
  where
  nubPreds li = nubPreds' li []
  -- we expect
  nubPreds' (p@(Pred tc x) : xs) elems = case x of
    TyVar rv -> readTvRef rv >>= \case
      LinkTy tl -> nubPreds' (Pred tc tl :xs) elems
      _ ->
        if elem p elems
        then nubPreds' xs elems
        else nubPreds' xs (Pred tc x:elems)
    _ -> nubPreds' xs elems
  nubPreds' [] elems = pure (reverse elems)
  info = term ^. termInfo
  genPred (Pred t pty) = do
    (o, pty')  <- gen' pty
    when (o /= []) $
      error "Predicate is introducing an unquantified type variable"
    pure (Pred t pty')
  gen' (TyVar tv) = lift (readTvRef tv) >>= \case
    Unbound n u l -> do
      cl <- lift currentLevel
      if l > cl then do
        lift (writeTvRef tv (Bound n u))
        gets (Set.member u) >>= \case
          True -> pure ([], TyVar tv)
          False -> modify' (Set.insert u) $> ([tv], TyVar tv)
      else pure ([], TyVar tv)
    LinkTy t' -> gen' t'
    LinkRow _ -> error "invariant failure: tyvar instantiated as row"
    LinkCap _ -> error "invariant failure: tyvar instantiated as cap"
    Bound _ _ -> pure ([], TyVar tv)
  gen' (TyFun l r) = do
    (ftvl, l') <- gen' l
    (ftvr, r') <- gen' r
    pure (ftvl ++ ftvr,TyFun l' r')
  gen' t@TyPrim{} = pure ([], t)
  gen' (TyList t) = over _2 TyList <$> gen' t
  gen' (TyModRef mr) = pure ([], TyModRef mr)
  gen' (TyCapToken cv) = case cv of
    CapVar tv ->
      lift (readTvRef tv) >>= \case
        Unbound n u l -> do
          cl <- lift currentLevel
          if l > cl then do
            lift (writeTvRef tv (Bound n u))
            gets (Set.member u) >>= \case
              True -> pure ([], TyCapToken (CapVar tv))
              False -> modify' (Set.insert u) $> ([tv], TyCapToken (CapVar tv))
          else pure ([], TyCapToken (CapVar tv))
        LinkTy _ -> error "invariant failure: tycaptoken instantiated as type"
        LinkRow _ -> error "invariant failure: tyvar instantiated as row"
        LinkCap qn -> gen' (TyCapToken qn)
        Bound _ _ -> pure ([], TyCapToken cv)
    CapConcrete qn -> pure ([], TyCapToken (CapConcrete qn))
  gen' (TyNullary n) = over _2 TyNullary <$> gen' n
  gen' (TyObject row) = over _2 TyObject <$> genRow' row
  gen' (TyTable row) = over _2 TyTable <$> genRow' row
  genRow' (RowVar tv) = lift (readTvRef tv) >>= \case
    Unbound n u l -> do
      cl <- lift currentLevel
      if l > cl then do
        lift (writeTvRef tv (Bound n u))
        gets (Set.member u) >>= \case
          True -> pure ([], RowVar tv)
          False -> modify' (Set.insert u) $> ([tv], RowVar tv)
      else pure ([], RowVar tv)
    LinkTy _ -> error "invariant failure: rowvar instantiated as type"
    LinkRow row -> genRow' row
    LinkCap _ -> error "invariant failure: rowvar instantiated as cap"
    Bound _ _ -> pure ([], RowVar tv)
  genRow' (RowConcrete row) = do
    rowTup <- traverse gen' row
    let row' = view _2 <$> rowTup
        ps = concat (M.elems (view _1 <$> rowTup))
    pure (ps, RowConcrete row')


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
          unify checkty ty i
          pure (ty, v', [])
        Nothing ->
          error "unbound term variable"
          -- throwTypecheckError (TCUnboundTermVariable n) i
    NTopLevel mn _mh ->
      view (tcFree . at (FullyQualifiedName mn n _mh)) >>= \case
        Just nty -> do
          let newVar = Var irn i
              rty = liftType nty
          unify rty checkty i
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
      TyVar tv -> do
        let ifSet = Set.fromList ifs
        -- Todo: this is sus... potentially very very sus
        unify (TyVar tv) (TyModRef ifSet) i
        pure (TyModRef ifSet, Var irn i, [])
      _ -> error "checking modref against incorrect type"
    _ -> error "dynref not supported"
  term@(IR.Lam _ _ i) -> do
    tup <- inferTerm term
    unify (view _1 tup) checkty i
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
    unify termTy checkty i
    pure (termTy, term', preds)
  -- IR.App te (h : hs) i ->
    -- (tapp, te', pe1) <- inferTerm te
    -- (rty, xs, ps) <- foldlM inferFunctionArgs (tapp, [], []) (h:hs)
    -- unify rty checkty i
    -- let term' = App te' (NE.fromList (reverse xs)) i
    -- pure (checkty, term', pe1 ++ ps)
    -- where
    -- inferFunctionArgs (ta, xs, ps) x = case ta of
    --   TyFun arg ret -> do
    --     (_, x', p) <- checkTermType arg x
    --     pure (ret, x':xs, ps ++ p)
    --   _ -> error "not a function"
  IR.Sequence l r i -> do
    (_, l', pl) <- inferTerm l
    (_, r', pr) <- checkTermType checkty r
    pure (checkty, Sequence l' r' i, pl ++ pr)
  IR.Conditional cond i -> over _2 (`Conditional` i) <$>
    case cond of
      CAnd e1 e2 -> do
        unify checkty TyBool i
        (_, e1', pe1) <- checkTermType TyBool e1
        (_, e2', pe2) <- checkTermType TyBool e2
        pure (TyBool, CAnd e1' e2', pe1 ++ pe2)
      COr e1 e2 -> do
        unify checkty TyBool i
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
    unify checkty ty i
    let term' = Builtin (b, TyVar <$> tvs, preds) i
    pure (ty, term', preds)
  IR.CapabilityForm cf i -> over _2 (`CapabilityForm` i) <$> case cf of
    WithCapability c_token body -> do
      tv <- newTvRef
      let tvar = TypeVar tv UserDefKind
          ty = TyCapToken (CapVar tvar)
      (tyTok, c_token', p1) <- inferTerm c_token
      unify ty tyTok i
      (ty', body', p2) <- checkTermType checkty body
      pure (ty', WithCapability c_token' body', p1 ++ p2)
    -- RequireCapability na tes -> do
    --   unify checkty TyUnit i
    --   (tes', p1) <- checkCapArgs na tes
    --   pure (TyUnit, RequireCapability na tes', p1)
    -- ComposeCapability na tes -> do
    --   unify checkty TyUnit i
    --   (tes', p1) <- checkCapArgs na tes
    --   pure (TyUnit, ComposeCapability na tes', p1)
    -- InstallCapability na tes -> do
    --   unify checkty TyUnit i
    --   (tes', p1) <- checkCapArgs na tes
    --   pure (TyUnit, InstallCapability na tes', p1)
    -- EmitEvent na tes -> do
    --   unify checkty TyUnit i
    --   (tes', p1) <- checkCapArgs na tes
    --   pure (TyUnit, EmitEvent na tes', p1)
    -- TODO: Enforce `na` is a name of a dfun and not a dcap
    -- as a matter of fact, the whole above block needs the same enforcement just
    -- for dfuns
    CreateUserGuard ne args -> do
      (_, Apply _ args' _, pe1) <- inferApply (Apply (IR.Var ne i) args i)
      unify checkty TyGuard i
      pure (TyGuard, CreateUserGuard ne args', pe1)
  IR.Constant lit i -> do
    let ty = typeOfLit lit
    unify checkty ty i
    pure (ty, Constant lit i, [])
  IR.ListLit tes i -> case checkty of
    TyList ty -> do
      liTup <- traverse (checkTermType ty) tes
      let preds = concat (view _3 <$> liTup)
          term' = ListLit ty (view _2 <$> liTup) i
      pure (TyList ty, term', preds)
    _ -> do
      tup <- inferTerm (IR.ListLit tes i)
      unify (view _1 tup) checkty i
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
    unify checkty objTy i
    pure (objTy, ObjectLit objTerms i, preds)



-- checkCapArgs
--   :: TypeOfBuiltin raw
--   => Name
--   -> [IRTerm raw i]
--   -> InferM s reso i ([TCTerm s raw i], [TCPred s])
-- checkCapArgs na tes = case _nKind na of
--   NTopLevel mn mh ->
--     view (tcFree . at (FullyQualifiedName mn (_nName na) mh)) >>= \case
--       Just (DefcapType dcargs _) -> do
--         when (length dcargs /= length tes) $ error "invariant broken dcap args"
--         vs <- zipWithM (checkTermType . liftType) dcargs tes
--         pure (view _2 <$> vs, concat (view _3 <$> vs))
--       _ -> error "invariant broken"
--   _ -> error "invariant broken"

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
      view (tcFree . at (FullyQualifiedName mn n _mh)) >>= \case
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
    unify (TyNullary tv1) tfun i
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
        unify ta (TyFun tArg tv1) i
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
      unify tyToken ty i
      (ty', te', p2) <- inferTerm te
      pure (ty', WithCapability c_token' te', p1 ++ p2)
    CreateUserGuard ne args -> do
      -- Todo: use a smart ctor for this
      (_, Apply _ args' _, pe1) <- inferApply (Apply (IR.Var ne i) args i)
      pure (TyGuard, CreateUserGuard ne args', pe1)
      -- (tes', p1) <- checkCapArgs na tes
      -- pure (TyGuard, CreateUserGuard na tes', p1)

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
        unify te1 te2 i
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
  IR.Constant l i ->
    pure (typeOfLit l, Constant l i,[])
  -- -- note: object literals are closed rows.
  -- IR.ObjectLit _obj _i -> undefined
  -- -- Todo: comment this case out better.
  -- IR.ObjectOp _oop _i -> undefined
  IR.ListLit li i -> do
    tv <- TyVar . (`TypeVar` TyKind) <$> newTvRef
    liTup <- traverse inferTerm li
    let preds = concat (view _3 <$> liTup)
    traverse_ (\(t,_, _) -> unify tv t i) liTup
    pure (TyList tv, ListLit tv (view _2 <$> liTup) i, preds)
  IR.Try e1 e2 i -> do
    (te1, e1', p1) <- inferTerm e1
    (te2, e2', p2)<- inferTerm e2
    unify te1 te2 i
    pure (te1, Try e1' e2' i, p1 ++ p2)
  IR.Nullary{} -> error "todo: finish"
  IR.ObjectLit{} -> error "Todo: finish"

inferApply
  :: TypeOfBuiltin b
  => Apply (IRTerm b i) i
  -> InferM s b i (TCType s, Apply (TCTerm s b i) i, [TCPred s])
inferApply (Apply fun args i) = case args of
  [] -> do
    tv1 <- TyVar . (`TypeVar` TyKind) <$> newTvRef
    (tfun, fun', pe1) <- inferTerm fun
    unify (TyNullary tv1) tfun i
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
        unify ta (TyFun tArg tv1) i
        pure (tv1,fnArg':xs,ps ++ predsArg)



-- -- Todo: generic types?
-- -- We can't generalize yet since
-- -- we're not allowing type schemes just yet.
-- inferDefun
--   :: TypeOfBuiltin b
--   => IR.Defun Name b i
--   -> InferM s b' i (TypedDefun b i)
-- inferDefun (IR.Defun name dfargs dfRetType term info) = do
--   enterLevel
--   let dfTy = foldr TyFun retType dfArgs'
--   (termTy, term', preds) <- inferTerm term
--   leaveLevel
--   checkReducible preds (view IR.termInfo term)
--   -- fail "typeclass constraints not supported in defun"
--   unify (liftType dfTy) termTy info
--   fterm <- noTyVarsinTerm info term'
--   pure (Defun name (liftType dfTy) fterm info)

-- inferDefConst
--   :: TypeOfBuiltin b
--   => IR.DefConst Name b i
--   -> InferM s b' i (TypedDefConst b i)
-- inferDefConst (IR.DefConst name dcTy term info) = do
--   enterLevel
--   (termTy, term', preds) <- inferTerm term
--   leaveLevel
--   checkReducible preds info
--   fterm <- noTyVarsinTerm info term'
--   let dcTy' = liftType <$> dcTy
--   _ <- maybe (pure ()) (\dct -> unify dct termTy info) dcTy'
--   rty' <- ensureNoTyVars info (maybe termTy id dcTy')
--   pure (DefConst name rty' fterm info)

-- inferDefCap
--   :: TypeOfBuiltin b
--   => IR.DefCap Name b i
--   -> InferM s b' i (TypedDefCap b i)
-- inferDefCap (IR.DefCap name arity argtys rty term meta i) = do
--   let ty = foldr TyFun rty argtys
--   (termTy, term', preds) <- checkTermType (liftType ty) term
--   checkReducible preds i
--   unify (liftType ty) (termTy) i
--   fterm <- noTyVarsinTerm i term'
--   pure (DefCap name arity argtys rty fterm meta i)

-- inferDef
--   :: TypeOfBuiltin b
--   => IR.Def Name b i
--   -> InferM s b' i (TypedDef b i)
-- inferDef = \case
--   IR.Dfun d -> Dfun <$> inferDefun d
--   IR.DConst d -> DConst <$> inferDefConst d
--   IR.DCap dc -> DCap <$> inferDefCap dc

-- inferIfDef
--   :: TypeOfBuiltin b
--   => IR.IfDef Name b i
--   -> InferM s b' i (TypedIfDef b i)
-- inferIfDef = \case
--   IR.IfDfun ifd ->
--     pure (IfDfun (IfDefun (IR._ifdName ifd) (IR._ifdType ifd) (IR._ifdInfo ifd)))
--   IR.IfDConst dc ->
--     IfDConst <$> inferDefConst dc
--   IR.IfDCap (IR.IfDefCap n argtys rty i) ->
--     pure $ IfDCap (IfDefCap n argtys rty i)

-- inferModule
--   :: TypeOfBuiltin b
--   => IR.Module Name b i
--   -> InferM s b' i (TypedModule b i)
-- inferModule (IR.Module mname mgov defs blessed imports impl mh info) = do
--   fv <- view tcFree
--   (defs', _) <- foldlM infer' ([], fv) defs
--   pure (Module mname mgov (reverse defs') blessed imports impl mh info)
--   where
--   infer' (xs, m) d = do
--     def' <- local (set tcFree m) (inferDef d)
--     let name' = FullyQualifiedName mname (defName def') mh
--         dty = fmap absurd (defType def')
--         m' = M.insert name' dty  m
--     pure (def':xs, m')

-- inferInterface
--   :: TypeOfBuiltin b
--   => IRInterface b info
--   -> InferM s b' info (TypedInterface b info)
-- inferInterface (IR.Interface n defns h info) = do
--   defns' <- traverse inferIfDef defns
--   pure (Interface n defns' h info)

-- -- | Note: debruijnizeType will
-- -- ensure that terms that are generic will fail
-- inferTermNonGen
--   :: TypeOfBuiltin b
--   => IRTerm b i
--   -> InferM s b' i (TypeScheme NamedDeBruijn, TypedTerm b i)
-- inferTermNonGen t = do
--   (ty, t', preds) <- inferTerm t
--   checkReducible preds (view IR.termInfo t)
--   tys <- ensureNoTyVars (view IR.termInfo t) ty
--   tt <- noTyVarsinTerm (view IR.termInfo t) t'
--   pure (TypeScheme [] [] tys, tt)

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

-- inferTopLevel
--   :: TypeOfBuiltin b
--   => Loaded reso i
--   -> IR.TopLevel Name b i
--   -> InferM s reso i (TypedTopLevel b i, Loaded reso i)
-- inferTopLevel loaded = \case
--   IR.TLModule m -> do
--     tcm <- inferModule m
--     let toFqn df = FullyQualifiedName (_mName tcm) (defName df) (_mHash tcm)
--         newTLs = M.fromList $ (\df -> (toFqn df, defType df)) <$> _mDefs tcm
--         loaded' = over loAllTyped (M.union newTLs) loaded
--     pure (TLModule tcm, loaded')
--   IR.TLTerm m -> (, loaded) . TLTerm . snd <$> inferTermNonGen m
--   IR.TLInterface i -> do
--     tci <- inferInterface i
--     let toFqn dc = FullyQualifiedName (_ifName tci) (_dcName dc) (_ifHash tci)
--         newTLs = M.fromList $ fmap (\df -> (toFqn df, DefunType (_dcType df))) $ mapMaybe (preview _IfDConst) (_ifDefns tci)
--         loaded' = over loAllTyped (M.union newTLs) loaded
--     pure (TLInterface tci, loaded')

-- inferReplTopLevel
--   :: TypeOfBuiltin b
--   => Loaded reso i
--   -> IR.ReplTopLevel Name b i
--   -> InferM s reso i (TypedReplTopLevel b i, Loaded reso i)
-- inferReplTopLevel loaded = \case
--   IR.RTLModule m ->  do
--     tcm <- inferModule m
--     let toFqn df = FullyQualifiedName (_mName tcm) (defName df) (_mHash tcm)
--         newTLs = M.fromList $ (\df -> (toFqn df, defType df)) <$> _mDefs tcm
--         loaded' = over loAllTyped (M.union newTLs) loaded
--     pure (RTLModule tcm, loaded')
--   IR.RTLTerm m -> (, loaded) . RTLTerm . snd <$> inferTermNonGen m
--   -- Todo: if we don't update the module hash to update linking,
--   -- repl defuns and defconsts will break invariants about
--   IR.RTLDefun dfn -> do
--     dfn' <- inferDefun dfn
--     let newFqn = FullyQualifiedName replModuleName (_dfunName dfn') replModuleHash
--     let loaded' = over loAllTyped (M.insert newFqn (DefunType (_dfunType dfn'))) loaded
--     pure (RTLDefun dfn', loaded')
--   IR.RTLDefConst dconst -> do
--     dc <- inferDefConst dconst
--     let newFqn = FullyQualifiedName replModuleName (_dcName dc) replModuleHash
--     let loaded' = over loAllTyped (M.insert newFqn (DefunType (_dcType dc))) loaded
--     pure (RTLDefConst dc, loaded')
--   IR.RTLInterface i -> do
--     tci <- inferInterface i
--     let toFqn dc = FullyQualifiedName (_ifName tci) (_dcName dc) (_ifHash tci)
--         newTLs = M.fromList $ fmap (\df -> (toFqn df, DefunType (_dcType df))) $ mapMaybe (preview _IfDConst) (_ifDefns tci)
--         loaded' = over loAllTyped (M.union newTLs) loaded
--     pure (RTLInterface tci, loaded')


-- -- | Transform types into their debruijn-indexed version
-- -- Essentially: Start at depth 0:
-- --  rename : (Term, Γ, Int) -> IxTerm
-- --  rename (ΛX.e, tyEnv, DEPTH) = Λ. (rename (e, tyEnv[depth/X], DEPTH+1))
-- --  .. other cases are simply renaming recursively and calling `renameType`
-- --  on occurences of Type
-- --
-- --  NOTE: the passed in DEPTH is 1 higher than the highest binder.
-- --
-- --  renameType : (Type, Γ, Int) -> IxType
-- --  renameType (a, env, DEPTH) = DEPTH - env(a) - 1
-- --  .. other recursive cases are straightforward
-- --
-- --  Quip: when we debruijnize types, we expect no impredicative polymorphism atm,
-- --  thus we will fail on universially quantified types found in application and
-- --  var binding sites.
-- --  The typechecker does not spit out impredicative polymorphism, but while
-- --  it would be trivial to support their renaming here, I'd rather fail
-- --  for now as the typechecker does not support it and it functions as a sanity check
-- debruijnizeTermTypes
--   :: forall s b b' i. i
--   -> TCTerm s b i
--   -> InferM s b' i (Term Name NamedDeBruijn (b, [Type NamedDeBruijn], [Pred NamedDeBruijn]) i)
-- debruijnizeTermTypes info = dbj [] 0
--   where
--   dbj
--     :: [(TvRef s, NamedDeBruijn)]
--     -> DeBruijn
--     -> TCTerm s b i
--     -> InferM s b' i (Term Name NamedDeBruijn (b, [Type NamedDeBruijn], [Pred NamedDeBruijn]) i)
--   dbj env depth = \case
--     Var n i ->
--       pure (Var n i)
--     Lam nts e i -> do
--       nts' <- (traversed._2) (dbjTyp info env depth) nts
--       e' <- dbj env depth e
--       pure (Lam nts' e' i)
--     App l r i ->
--       App <$> dbj env depth l <*> traverse (dbj env depth) r <*> pure i
--     Let n e1 e2 i -> do
--       e1' <- dbj env depth e1
--       e2' <- dbj env depth e2
--       pure (Let n e1' e2' i)
--     TyAbs ntys e i -> do
--       let len = fromIntegral (NE.length ntys)
--           ixs = NE.fromList [depth .. depth + len - 1]
--       names <- traverse (nameTvs info (depth + len)) (NE.zip ntys ixs)
--       let env' = NE.toList $ NE.zip ntys names
--       TyAbs names <$> dbj (env' ++ env) (depth + len) e <*> pure i
--     TyApp e args i -> do
--       e' <- dbj env depth e
--       args' <- traverse (dbjTyp info env depth) args
--       pure (TyApp e' args' i)
--     Sequence e1 e2 i ->
--       Sequence <$> dbj env depth e1 <*> dbj env depth e2 <*> pure i
--     Conditional c i ->
--       Conditional <$> traverse (dbj env depth) c <*> pure i
--     Try e1 e2 i ->
--       Try <$> dbj env depth e1 <*> dbj env depth e2 <*> pure i
--     Error t e i -> do
--       ty <- dbjTyp info env depth t
--       pure (Error ty e i)
--     ListLit ty v i ->
--       ListLit <$> dbjTyp info env depth ty <*> traverse (dbj env depth) v <*> pure i
--     DynInvoke n t i ->
--       DynInvoke <$> dbj env depth n <*> pure t <*> pure i
--     Builtin (b, tys, preds) i -> do
--       tys' <- traverse (dbjTyp info env depth) tys
--       preds' <- traverse (dbjPred info env depth) preds
--       pure (Builtin (b, tys', preds') i)
--     CapabilityForm cf i ->
--       CapabilityForm <$> traverse (dbj env depth) cf <*> pure i
--     Constant l i -> pure (Constant l i)


-- nameTvs
--   :: i
--   -> DeBruijn
--   -> (TvRef s, DeBruijn)
--   -> InferM s b i NamedDeBruijn
-- nameTvs info depth (nt, i) = readTvRef nt >>= \case
--   Bound n _ ->
--     pure (NamedDeBruijn (depth - i - 1) n)
--   _ ->
--     throwTypecheckError (TCInvariantFailure "Found unbound variable during generalization") info

-- ensureNoTyVars
--   :: i
--   -> TCType s
--   -> InferM s b i (Type a)
-- ensureNoTyVars i = \case
--   TyVar n -> readTvRef n >>= \case
--     Link ty -> ensureNoTyVars i ty
--     _ ->
--       throwTypecheckError (DisabledGeneralization "Inferred generic signature") i
--   TyPrim p -> pure (TyPrim p)
--   TyFun l r -> TyFun <$> ensureNoTyVars i l <*> ensureNoTyVars i r
--   TyList l -> TyList <$> ensureNoTyVars i l
--   TyModRef mr -> pure (TyModRef mr)
--   TyForall _ _ ->
--     throwTypecheckError (TCInvariantFailure "Encountered universal quantification emitted by the typechecker. Impossible") i

-- ensureNoTyVarsPred
--   :: i
--   -> TCPred s
--   -> InferM s b i (Pred NamedDeBruijn)
-- ensureNoTyVarsPred i (Pred tc ty) = Pred tc <$> ensureNoTyVars i ty

-- noTyVarsinTerm
--   :: i
--   -> TCTerm s b' i
--   -> InferM s b i (TypedTerm b' i)
-- noTyVarsinTerm info = \case
--   Var n i ->
--     pure (Var n i)
--   Lam nts e i ->
--     Lam <$> (traversed._2) (ensureNoTyVars info) nts <*> noTyVarsinTerm info e <*> pure i
--   App e args i ->
--     App <$> noTyVarsinTerm info e <*> traverse (noTyVarsinTerm info) args <*> pure i
--   Let n e1 e2 i ->
--     Let n <$> noTyVarsinTerm info e1 <*> noTyVarsinTerm info e2 <*> pure i
--   Builtin (b, ty, p) i -> do
--     ty' <- traverse (ensureNoTyVars info) ty
--     p' <- traverse (ensureNoTyVarsPred info) p
--     pure $ Builtin (b, ty', p') i
--   TyAbs _ns _e _i ->
--     throwTypecheckError (DisabledGeneralization "Generic terms are disabled") info
--   Constant l i ->
--     pure (Constant l i)
--   TyApp l tys i ->
--     TyApp
--       <$> noTyVarsinTerm info l
--       <*> traverse (ensureNoTyVars info) tys
--       <*> pure i
--   Sequence e1 e2 i ->
--     Sequence <$> noTyVarsinTerm info e1 <*> noTyVarsinTerm info e2 <*> pure i
--   Conditional c i ->
--     Conditional <$> traverse (noTyVarsinTerm info) c <*> pure i
--   ListLit ty li i ->
--     ListLit <$> ensureNoTyVars info ty <*> traverse (noTyVarsinTerm info) li <*> pure i
--   Try e1 e2 i ->
--     Try <$> noTyVarsinTerm info e1 <*> noTyVarsinTerm info e2 <*> pure i
--   DynInvoke e1 t i ->
--     DynInvoke <$> noTyVarsinTerm info e1 <*> pure t <*> pure i
--   CapabilityForm cf i ->
--     CapabilityForm <$> traverse (noTyVarsinTerm info) cf <*> pure i
--   Error t e i ->
--     Error <$> ensureNoTyVars info t <*> pure e <*> pure i

-- -- dbjName
-- --   :: [(TvRef s, NamedDeBruijn)]
-- --   -> DeBruijn
-- --   -> OverloadedName (TCPred s)
-- --   -> InferM s b i (OverloadedName (Pred NamedDeBruijn))
-- -- dbjName env depth (OverloadedName n kind) = fmap (OverloadedName n) $ case kind of
-- --   OBound b -> pure (OBound b)
-- --   OTopLevel m h -> pure (OTopLevel m h)
-- --   OBuiltinDict b -> OBuiltinDict <$> dbjPred env depth b

-- debruijnizeTypeScheme
--   :: i
--   -> TypeScheme (TvRef s)
--   -> InferM s b i (TypeScheme NamedDeBruijn)
-- debruijnizeTypeScheme i (TypeScheme tvs preds t) = do
--     let len = fromIntegral (length tvs)
--     let ixs = [0.. len - 1]
--     names <- traverse (nameTvs i len) (zip tvs ixs)
--     let env = zip tvs names
--     t' <- dbjTyp i env len t
--     preds' <- traverse (dbjPred i env len) preds
--     pure (TypeScheme names preds' t')

-- debruijnizeType
--   :: i
--   -> TCType s
--   -> InferM s b i (Type NamedDeBruijn)
-- debruijnizeType i = dbjTyp i [] 0

-- dbjPred
--   :: i
--   -> [(TvRef s, NamedDeBruijn)]
--   -> DeBruijn
--   -> TCPred s
--   -> InferM s b i (Pred NamedDeBruijn)
-- dbjPred i env depth (Pred tc ty) =
--   Pred tc <$> dbjTyp i env depth ty

-- dbjTyp
--   :: i
--   -> [(TvRef s, NamedDeBruijn)]
--   -> DeBruijn
--   -> TCType s
--   -> InferM s b i (Type NamedDeBruijn)
-- dbjTyp i env depth = \case
--   TyVar n -> case lookup n env of
--     Just v -> pure (TyVar v)
--     Nothing -> readTvRef n >>= \case
--       Unbound {} ->
--         throwTypecheckError (TCInvariantFailure "Found unbound type variable after type checking") i
--       Bound{} ->
--         throwTypecheckError (TCInvariantFailure "Found bound variable outside of the calculated set") i
--       Link ty -> dbjTyp i env depth ty
--   TyPrim p -> pure (TyPrim p)
--   TyFun l r -> TyFun <$> dbjTyp i env depth l <*> dbjTyp i env depth r
--   TyList l -> TyList <$> dbjTyp i env depth l
--   TyModRef m -> pure (TyModRef m)
--   TyForall{} ->
--     throwTypecheckError (TCInvariantFailure "Found impredicative Type") i

-- -- -----------------------------------------
-- -- --- Built-in type wiring
-- -- ------------------------------------------

-- -- mkFree :: Loaded builtin info -> Map ModuleName (Map Text (Type Void))
-- -- mkFree loaded = let
-- --   tl = _loModules loaded
-- --   toTy d = (UndefName d, UndefType d)
-- --   mdefs =  Un_mDefs . _mdModule <$> tl
-- --   in M.fromList . fmap toTy <$> mdefs

-- runInfer
--   :: Loaded b i
--   -> InferM s b i a
--   -> ST s (Either (PactError i) a)
-- runInfer loaded (InferT act) = do
--   uref <- newSTRef 0
--   lref <- newSTRef 1
--   let tcs = TCState uref mempty (_loAllTyped loaded) lref (_loModules loaded)
--   runReaderT (runExceptT act) tcs

-- runInferTerm
--   :: TypeOfBuiltin b
--   => Loaded b' i
--   -> IRTerm b i
--   -> Either (PactError i) (TypeScheme NamedDeBruijn, TypedGenTerm b i)
-- runInferTerm loaded term0 = runST $
--   runInfer loaded $ inferTermGen term0

-- runInferTermNonGen
--   :: TypeOfBuiltin b
--   => Loaded b' i
--   -> IRTerm b i
--   -> Either (PactError i) (TypeScheme NamedDeBruijn, TypedTerm b i)
-- runInferTermNonGen loaded term0 = runST $
--   runInfer loaded $ inferTermNonGen term0

-- runInferModule
--   :: TypeOfBuiltin b
--   => Loaded b' i
--   -> IRModule b i
--   -> Either (PactError i) (TypedModule b i)
-- runInferModule loaded term0 =
--   runST $ runInfer loaded (inferModule term0)

-- runInferTopLevel
--   :: TypeOfBuiltin b
--   => Loaded reso i
--   -> IR.TopLevel Name b i
--   -> Either (PactError i) (TypedTopLevel b i, Loaded reso i)
-- runInferTopLevel l tl =
--   runST $ runInfer l (inferTopLevel l tl)


-- runInferReplTopLevel
--   :: TypeOfBuiltin b
--   => Loaded reso i
--   -> IR.ReplTopLevel Name b i
--   -> Either (PactError i) (TypedReplTopLevel b i, Loaded reso i)
-- runInferReplTopLevel l tl =
--   runST $ runInfer l (inferReplTopLevel l tl)
