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
  (typecheckModule
  , TypecheckError(..)
  , TypecheckFailure(..)
  )
  where


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
import Data.List (nub)
import Data.Foldable(traverse_, foldlM)
import Data.Default
import Data.STRef
import Data.Graph
import Data.Map(Map)
import Data.Text(Text)
import Data.List.NonEmpty(NonEmpty(..))

import qualified Data.Map.Strict as M
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.RAList as RAList
import qualified Data.Set as S

import Pact.Core.Builtin
import Pact.Core.Names
import Pact.Core.Persistence
import Pact.Core.ModRefs
import Pact.Core.Typed.Term
import Pact.Core.Typed.Type
import Pact.Core.PactValue
import Pact.Core.Hash

import qualified Pact.Core.IR.Term as IR
import qualified Pact.Core.Type as IR
import qualified Pact.Core.Typed.Term as Typed
import Pact.Core.Environment

import Debug.Trace

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
type TCRowCtor s = RowTy (TCTypeVar s)
type TCRoseRow s = RoseRow (TCTypeVar s)

data Tv s
  = Unbound !Text !Unique !Level
  | Bound !Text !Unique
  | LinkTy !(TCType s)
  | LinkRow !(RowTy (TCTypeVar s))
  -- | LinkCap !(CapRef (TCTypeVar s))
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
  , _tcVarEnv :: RAList (TypeScheme (TCTypeVar s))
  -- ^ Builtins map, that uses the enum instance
  , _tcLevel :: STRef s Level
  }

makeLenses ''TCEnv

data TCState b i
  = TCState
  { _tcFree :: Map FullyQualifiedName (TypeScheme (TypeVar NamedDeBruijn))
  , _tcInterfaces :: Map ModuleName (Interface Void i)
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
type TCTerm s b i = Term Name (TCTypeVar s) b i

-- Term/defun outputs post typechecking
-- with ST monad existential removed
type TypedTerm b i = Term Name DebruijnTypeVar b i

type TypedDefun b i = Defun Name DebruijnTypeVar b i

type TypedDefCap b i = DefCap Name DebruijnTypeVar b i

type TypedDefConst b i = DefConst DebruijnTypeVar i

type TypedDef b i = Def Name DebruijnTypeVar b i

type TypedIfDef b i = IfDef DebruijnTypeVar i

type TypedTopLevel b i = TopLevel Name DebruijnTypeVar b i

type TypedReplTopLevel b i = ReplTopLevel Name DebruijnTypeVar b i

type TypedModule b i = Module Name DebruijnTypeVar b i

type TypedInterface b i = Interface DebruijnTypeVar i

data TypecheckError i =
  TypecheckError TypecheckFailure i
  deriving (Show, Eq)

data TypecheckFailure
  = UnificationFailure (Type Text) (Type Text)
  | InvariantRowInTypeVarPosition (RowTy Text)
  | InvariantTypeInRowVarPosition (Type Text)
  | InvariantUnboundTypeVariable Text
  | InvariantUnboundTermVariable Text
  | InvariantUnboundFreeVariable FullyQualifiedName
  | InvariantDefconstNotEvaluated FullyQualifiedName
  | InvariantOther Text
  | ModuleImplementsNoInterfaces ModuleName
  | ModuleLacksImplementedInterfaces ModuleName (S.Set ModuleName)
  | WrongKind PactKind
  | InfiniteType (Type Text) (Type Text)
  | InfiniteRow (RowTy Text) (RowTy Text)
  | DisallowedGenericSignature (Type Text)
  | CannotResolveConstraints [Pred Text]
  deriving (Show, Eq)

-- | Our inference monad, where we can plumb through generalization "regions",
-- our variable environment and our "supply" of unique names
newtype InferM s b i a =
  InferM (ExceptT (TypecheckError i) (ReaderT (TCEnv s b i) (StateT (TCState b i) (ST s))) a)
  deriving
    ( Functor, Applicative, Monad
    , MonadReader (TCEnv s b i)
    , MonadState (TCState b i)
    , MonadError (TypecheckError i))
  via (ExceptT (TypecheckError i) (ReaderT (TCEnv s b i) (StateT (TCState b i) (ST s))))

data SpecialOverload
  = AddOverload
  | AccessOverload
  | FormatOverload
  deriving (Eq, Ord, Show)

coreBuiltinToSpecialOverload :: CoreBuiltin -> Maybe SpecialOverload
coreBuiltinToSpecialOverload = \case
  CoreAdd -> Just AddOverload
  CoreAt -> Just AccessOverload
  CoreFormat -> Just FormatOverload
  _ -> Nothing

toSpecialOverload :: AsCoreBuiltin b => b -> Maybe SpecialOverload
toSpecialOverload b = preview _CoreBuiltin b >>= coreBuiltinToSpecialOverload

specialOverLoadToCoreBuiltin :: SpecialOverload -> CoreBuiltin
specialOverLoadToCoreBuiltin = \case
  AddOverload -> CoreAdd
  AccessOverload -> CoreAt
  FormatOverload -> CoreFormat

class (AsCoreBuiltin b, Show b) => TypeOfBuiltin b where
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
      TypeScheme [] [] (TyString :~> TyList TyString :~> TyString)
    CoreEnumerate ->
      NonGeneric (TyInt :~> TyInt :~> TyList TyInt)
    CoreEnumerateStepN ->
      NonGeneric (TyInt :~> TyInt :~> TyList TyInt)
    CoreShow ->
      let aVar = nd "a" 0
          a = TyVar aVar
      in TypeScheme [aVar] [Pred (Show a)] (a :~> TyString)
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
      TypeScheme [] [] (TyCapToken :~> TyGuard)
    CoreCreateCapabilityPactGuard ->
      TypeScheme [] [] (TyCapToken :~> TyGuard)
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
      TypeScheme [] [] (TyCapToken :~> TyBool)
    CoreComposeCapability ->
      TypeScheme [] [] (TyCapToken :~> TyBool)
    CoreInstallCapability ->
      TypeScheme [] [] (TyCapToken :~> TyString)
    CoreEmitEvent ->
      TypeScheme [] [] (TyCapToken :~> TyString)
    CoreCreateTable ->
      TypeScheme [] [] (TyCapToken :~> TyString)
    CoreDescribeKeyset ->
      TypeScheme [] [] (TyString :~> TyGuard)
    CoreDescribeModule -> let
      o = TyObject (RowConcrete schema)
      in TypeScheme [] [] (TyString :~> o)
      where
      schema = M.fromList
        [(Field "hash", TyString)
        ,(Field "interfaces", TyList TyString)
        ,(Field "name", TyString)]
    CoreDescribeTable -> let
      o = TyObject (RowConcrete schema)
      in TypeScheme [] [] (TyString :~> o)
      where
      schema = M.fromList
        [(Field "type", TyString)
        ,(Field "module", TyString)
        ,(Field "name", TyString)]
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
    CoreKeys -> let
      rowVar = RowVariable 0 "row"
      fnTy = TyTable (RowVar rowVar) :~> TyList TyString
      in TypeScheme [rowVar] [] fnTy
    CoreRead -> let
      rowVar = RowVariable 0 "row"
      fnTy = TyTable (RowVar rowVar) :~> TyString :~> TyObject (RowVar rowVar)
      in TypeScheme [rowVar] [] fnTy
    CoreSelect -> let
      rowVar = RowVariable 0 "row"
      fnTy = TyTable (RowVar rowVar) :~> (TyObject (RowVar rowVar) :~> TyBool) :~> TyList (TyObject (RowVar rowVar))
      in TypeScheme [rowVar] [] fnTy
    CoreSelectWithFields ->
      error "todo: not supported"
    -- update : forall (r1: ROW, r2: ROW) . (r2 ≼ r1) => table<r1> -> string -> object<r2> -> string
    CoreUpdate -> let
      -- r1
      r1 = RowVariable 1 "r1"
      -- r2
      r2 = RowVariable 0 "r2"
      -- r1 ≼ r2
      rowConstr = RoseSubRow (RoseRowTy (RowVar r2)) (RoseRowTy (RowVar r1))
      --
      fnTy = TyTable (RowVar r1) :~> TyString :~> TyObject (RowVar r2) :~> TyString
      in TypeScheme [r1, r2] [Pred rowConstr] fnTy
    -- with-default-read : forall (r1: ROW) . table<r1> -> string -> object<r1> -> (object<r1> -> a) -> a
    CoreWithDefaultRead -> let
      r1 = RowVariable 1 "r1"
      aVar = TypeVariable 0 "a1"
      fnTy = TyTable (RowVar r1) :~> TyString :~> TyObject (RowVar r1) :~> (TyObject (RowVar r1) :~> TyVar aVar) :~> TyVar aVar
      in TypeScheme [r1, aVar] [] fnTy
    CoreWithRead -> let
      r1 = RowVariable 1 "r1"
      aVar = TypeVariable 0 "a1"
      fnTy = TyTable (RowVar r1) :~> TyString :~> (TyObject (RowVar r1) :~> TyVar aVar) :~> TyVar aVar
      in TypeScheme [r1, aVar] [] fnTy
    CoreWrite -> let
      r1 = RowVariable 0 "r1"
      fnTy = TyTable (RowVar r1) :~> TyString :~> TyObject (RowVar r1) :~> TyString
      in TypeScheme [r1] [] fnTy
    CoreTxHash ->
      TypeScheme [] [] (TyNullary TyString)
    CoreAndQ -> let
        aVar = nd "a" 0
        a = TyVar aVar
        cloTy = a :~> TyBool
      in TypeScheme [aVar] [] (cloTy :~> cloTy :~> a :~> TyBool)
    CoreOrQ -> let
        aVar = nd "a" 0
        a = TyVar aVar
        cloTy = a :~> TyBool
      in TypeScheme [aVar] [] (cloTy :~> cloTy :~> a :~> TyBool)
    CoreWhere ->
      error "where must be pattern matched"
    CoreNotQ -> let
        aVar = nd "a" 0
        a = TyVar aVar
        cloTy = a :~> TyBool
      in TypeScheme [aVar] [] (cloTy :~> a :~> TyBool)
    CoreHash -> let
        aVar = nd "a" 0
        a = TyVar aVar
      in TypeScheme [aVar] [Pred (IsValue a)] (a :~> TyString)
    -- note: continue is basically the indentity function
    CoreContinue -> let
        aVar = nd "a" 0
        a = TyVar aVar
      in TypeScheme [aVar] [] (a :~> a)
    CoreParseTime ->
      TypeScheme [] [] (TyString :~> TyString :~> TyTime)
    CoreFormatTime ->
      TypeScheme [] [] (TyString :~> TyTime :~> TyString)
    CoreTime ->
      TypeScheme [] [] (TyString :~> TyTime)
    CoreAddTime -> let
        aVar = nd "a" 0
        a = TyVar aVar
      in TypeScheme [aVar] [Pred (Num a)] (TyTime :~> a :~> TyTime)
    CoreDiffTime ->
      TypeScheme [] [] (TyTime :~> TyTime :~> TyDecimal)
    CoreHours -> let
        aVar = nd "a" 0
        a = TyVar aVar
      in TypeScheme [aVar] [Pred (Num a)] (a :~> TyDecimal)
    CoreMinutes -> let
        aVar = nd "a" 0
        a = TyVar aVar
      in TypeScheme [aVar] [Pred (Num a)] (a :~> TyDecimal)
    CoreDays -> let
        aVar = nd "a" 0
        a = TyVar aVar
      in TypeScheme [aVar] [Pred (Num a)] (a :~> TyDecimal)
    CoreCompose -> let
        aVar = TypeVariable 2 "a"
        bVar = TypeVariable 1 "b"
        cVar = TypeVariable 0 "c"
        a = TyVar aVar
      in TypeScheme [aVar] [Pred (Num a)]
          ((TyVar aVar :~> TyVar bVar)
            :~> (TyVar bVar :~> TyVar cVar)
            :~> TyVar aVar
            :~> TyVar cVar)
    CoreCreatePrincipal ->
      TypeScheme [] [] (TyGuard :~> TyString)
    CoreIsPrincipal ->
      TypeScheme [] [] (TyString :~> TyBool)
    CoreTypeOfPrincipal ->
      TypeScheme [] [] (TyString :~> TyString)
    CoreValidatePrincipal ->
      TypeScheme [] [] (TyGuard :~> TyString :~> TyBool)
    CoreNamespace ->
      TypeScheme [] [] (TyString :~> TyString)
    CoreDefineNamespace ->
      TypeScheme [] [] (TyString :~> TyGuard :~> TyGuard :~> TyString)
    CoreDescribeNamespace -> let
      in TypeScheme [] [] (TyString :~> TyObject (RowConcrete schema))
      where
      schema = M.fromList
        [ (Field "admin-guard", TyGuard)
        , (Field "namespace-name", TyString)
        , (Field "user-guard", TyGuard)]
    CoreZkPairingCheck ->
      error "todo: support function"
    CoreZKScalarMult ->
      error "todo: support function"
    CoreZkPointAdd ->
      error "todo: support function"
    CorePoseidonHashHackachain ->
      error "todo: support function"
    CoreChainData ->
      TypeScheme [] [] (TyNullary (TyObject (RowConcrete chainDataSchema)))
    CoreIsCharset ->
      TypeScheme [] [] (TyInt :~> TyString :~> TyBool)
    CorePactId ->
      TypeScheme [] [] (TyNullary TyString)
    CoreTypeOf -> let
        aVar = nd "a" 0
        a = TyVar aVar
        in TypeScheme [aVar] [Pred (IsValue a)] (a :~> TyString)
    CoreDec ->
      TypeScheme [] [] (TyInt :~> TyDecimal)
    CoreCond -> let
        aVar = nd "a" 0
        a = TyVar aVar
      in TypeScheme [aVar] [] (TyNullary a :~> a)
    _ -> error "stubbed"
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
      let aVar = TypeVariable 0 "a"
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
    RBuiltinRepl _ -> error "Repl natives are not supported in typechecking"

chainDataSchema :: Map Field (Type n)
chainDataSchema = M.fromList
  [ (Field "chain-id", TyString)
  , (Field "block-height", TyInt)
  , (Field "block-time", TyTime)
  , (Field "prev-block-hash", TyString)
  , (Field "sender", TyString)
  , (Field "gas-limit", TyInt)
  , (Field "gas-price", TyDecimal)]
-- [ chain-id:string
-- , block-height:integer
-- , block-time:time
-- , prev-block-hash:string
-- , sender:string
-- , gas-limit:integer
-- , gas-price:decimal ]

liftST :: ST s a -> InferM s b i a
liftST action = InferM $ lift $ lift $ lift action

throwTypecheckError :: i -> TypecheckFailure -> InferM s b i a
throwTypecheckError i msg = throwError (TypecheckError msg i)

_dbgTypedArg :: TypedArg (TCType s) i -> InferM s b i (TypedArg (Type Text) i)
_dbgTypedArg (TypedArg n ty i) =
  TypedArg n <$> _dbgType ty <*> pure i

_dbgTypedTerm
  :: TCTerm s b i
  -> InferM s b i (Term Text Text b i)
_dbgTypedTerm = \case
  Var n i -> pure (Var (_nName n) i)
  Lam nel body i -> do
    nel' <- traverse _dbgTypedArg nel
    body' <- _dbgTypedTerm body
    pure (Lam nel' body' i)
  App fn body i ->
    App <$> _dbgTypedTerm fn <*> traverse _dbgTypedTerm body <*> pure i
  Let n e1 e2 i -> do
    Let <$> _dbgTypedArg n <*> _dbgTypedTerm e1 <*> _dbgTypedTerm e2 <*> pure i
  Builtin b i -> pure (Builtin b i)
  Constant l i -> pure (Constant l i)
  TyApp t nelty i ->
    TyApp <$> _dbgTypedTerm t <*> traverse _dbgTypeApp nelty <*> pure i
    where
    _dbgTypeApp = \case
      TyAppVar tyn -> TyAppType <$> _dbgVar tyn
      TyAppRow row -> TyAppRow <$> _dbgRowCtor row
      TyAppType ty -> TyAppType <$> _dbgType ty
  TyAbs tys dicts term i -> do
    tys' <- traverse _dbgVar' tys
    dicts' <- traverse _dbgBuiltinTC dicts
    term' <- _dbgTypedTerm term
    pure (TyAbs tys' dicts' term' i)
  Sequence e1 e2 i ->
    Sequence <$> _dbgTypedTerm e1 <*> _dbgTypedTerm e2 <*> pure i
  BuiltinForm c i ->
    BuiltinForm <$> traverse _dbgTypedTerm c <*> pure i
  Try e1 e2 i ->
    Try <$> _dbgTypedTerm e1 <*> _dbgTypedTerm e2 <*> pure i
  ListLit ty li i ->
    ListLit <$> _dbgType ty <*> traverse _dbgTypedTerm li <*> pure i
  ObjectOp o i -> pure (ObjectOp o i)
  ObjectLit m i ->
    ObjectLit <$> (traverse._2) _dbgTypedTerm m <*> pure i
  DictApp term a i ->
    DictApp <$> _dbgTypedTerm term <*> traverse _dbgBuiltinTC a <*> pure i
  Format o o' i ->
    Format <$> _dbgTypedTerm o <*> traverse _dbgTypedTerm o' <*> pure i
  -- ObjectLit obj i ->
  --   ObjectLit <$> traverse _dbgTypedTerm obj <*> pure i
  -- ObjectOp oop i ->
  --   ObjectOp <$> traverse _dbgTypedTerm oop <*> pure i

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

_dbgRoseRow :: RoseRow (TCTypeVar s) -> InferM s b i (RoseRow Text)
_dbgRoseRow = \case
  RoseRowTy ty -> RoseRowTy <$> _dbgRowCtor ty
  RoseRowCat r r' ->
    RoseRowCat <$> _dbgRoseRow r <*> _dbgRoseRow r'


_dbgBuiltinTC :: BuiltinTC (TCTypeVar s) -> InferM s b i (BuiltinTC Text)
_dbgBuiltinTC = \case
  Eq ty -> Eq <$> _dbgType ty
  Ord ty -> Ord <$> _dbgType ty
  Show ty -> Show <$> _dbgType ty
  Add ty -> Add <$> _dbgType ty
  Num ty -> Num <$> _dbgType ty
  ListLike ty -> ListLike <$> _dbgType ty
  Fractional ty -> Fractional <$> _dbgType ty
  EnforceRead ty -> EnforceRead <$> _dbgType ty
  IsValue ty -> IsValue <$> _dbgType ty
  EqRow rt -> EqRow <$> _dbgRowCtor rt
  RoseSubRow r1 r2 ->
    RoseSubRow <$> _dbgRoseRow r1 <*> _dbgRoseRow r2
  RoseRowEq r1 r2 ->
    RoseSubRow <$> _dbgRoseRow r1 <*> _dbgRoseRow r2

_dbgPred :: TCPred s -> InferM s b i (Pred Text)
_dbgPred (Pred tc) = Pred <$> _dbgBuiltinTC tc

_dbgType :: TCType s -> InferM s b i (Type Text)
_dbgType = \case
  TyVar tv -> _dbgVar tv
  TyFun l r -> TyFun <$> _dbgType l <*> _dbgType r
  TyList t -> TyList <$> _dbgType t
  TyPrim p -> pure (TyPrim p)
  TyModRef mr -> pure (TyModRef mr)
  TyObject o -> TyObject <$> _dbgRowCtor o
  TyTable o -> TyTable <$> _dbgRowCtor o
  TyCapToken -> pure TyCapToken
  TyNullary t -> TyNullary <$> _dbgType t

_dbgRowCtor :: RowTy (TCTypeVar s) -> InferM s b i (RowTy Text)
_dbgRowCtor = \case
  RowConcrete o -> do
    o' <- traverse _dbgType o
    pure (RowConcrete o')
  RowVar tv -> readTvRef tv >>= \case
    Unbound u _ _ -> pure $ RowVar u
    Bound u _ -> pure $ RowVar u
    LinkTy _ty -> error "invariant type in row var posn"
    LinkRow ty -> _dbgRowCtor ty

_dbgVar :: TCTypeVar s -> InferM s b i (Type Text)
_dbgVar tv = readTvRef tv >>= \case
  Unbound u l _ -> pure (TyVar ("unbound" <> T.pack (show (u, l))))
  Bound u l -> pure (TyVar ("bound" <> T.pack (show (u, l))))
  LinkTy ty -> _dbgType ty
  LinkRow ty -> _dbgType (TyObject ty)
    -- LinkCap t -> _dbgType (TyCapToken t)

_dbgVar' :: TCTypeVar s -> InferM s b i Text
_dbgVar' tv = readTvRef tv >>= \case
  Unbound u l _ -> pure ("unbound" <> T.pack (show (u, l)))
  Bound u l -> pure ("bound" <> T.pack (show (u, l)))
  _ -> error "Boom"


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

-- newCapTokenRef :: QualifiedName -> InferM s b i (TCTypeVar s)
-- newCapTokenRef qn = do
--   uref <- asks _tcSupply
--   liftST (modifySTRef' uref (+ 1))
--   (`TypeVar` UserDefKind) . TvRef <$> liftST (newSTRef (LinkCap (CapConcrete qn)))

newTvRef :: InferM s b i (TvRef s)
newTvRef = do
  uref <- asks _tcSupply
  u <- liftST (readSTRef uref)
  let tvName = "'a_" <> T.pack (show u)
  l <- currentLevel
  liftST (modifySTRef' uref (+ 1))
  TvRef <$> liftST (newSTRef (Unbound tvName u l))

newTypeVar :: PactKind -> InferM s b i (TCTypeVar s)
newTypeVar pk =
  TypeVar <$> newTvRef <*> pure pk

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
byInst :: i -> TCPred s -> InferM s b i (Maybe [TCPred s])
byInst i (Pred p) = case p of
  Eq ty -> eqInst ty
  Add ty -> addInst ty
  Num ty -> numInst ty
  Ord ty -> ordInst ty
  Show ty -> showInst ty
  ListLike ty -> listLikeInst ty
  Fractional ty -> fractionalInst ty
  EnforceRead ty -> enforceReadInst ty
  EqRow l -> eqRowInst l
  RoseSubRow l r -> roseSubRow i l r
  IsValue v -> isValueInst v
  RoseRowEq l r -> roseRowEq i l r

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
  TyCapToken -> pure (Just [])
  --  case cr of
    -- CapConcrete{} -> pure $ Just []
    -- CapVar tv -> readTvRef tv >>= \case
      -- LinkCap cvar' -> eqInst (TyCapToken cvar')
      -- _ -> pure Nothing
  TyObject r -> pure $ Just [Pred (EqRow r)]
    -- RowVar rv -> readTvRef rv >>= \case
    --   LinkRow row' -> eqInst (TyObject row')
    --   _ -> pure Nothing
    -- RowConcrete row -> do
    --   preds <- M.elems <$> traverse eqInst row
    --   pure $ concat <$> sequence preds
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
    PrimTime -> pure (Just [])
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

isValueInst :: TCType s -> InferM s b i (Maybe [TCPred s])
isValueInst = \case
  TyVar tv -> readTvRef tv >>= \case
    LinkTy ty -> isValueInst ty
    _ -> pure Nothing
  -- All prims are pact values
  TyPrim _p -> pure (Just [])
  -- All lists are values by construction
  TyList t -> isValueInst t
  -- Assume objects are values, simply because
  -- construction will assume it so
  -- TODO: this doesn't seem right, we're assuming this from
  -- object construction, but this invariatn
  TyObject _ -> pure (Just [])
  TyCapToken -> pure (Just [])
  TyModRef{} -> pure (Just [])
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

enforceReadInst :: TCType s -> InferM s b i (Maybe [a])
enforceReadInst = \case
  TyVar tv -> readTvRef tv >>= \case
    LinkTy ty -> enforceReadInst ty
    _ -> pure Nothing
  TyPrim p -> pure $ case p of
    PrimString -> Just []
    PrimGuard -> Just []
    _ -> Nothing
  _ -> pure Nothing

eqRowInst :: TCRowCtor s -> InferM s b i (Maybe [TCPred s])
eqRowInst = \case
    RowVar rv -> readTvRef rv >>= \case
      LinkRow row' -> eqRowInst row'
      _ -> pure Nothing
    RowConcrete row -> do
      preds <- M.elems <$> traverse eqInst row
      pure $ concat <$> sequence preds

roseSubRow :: i -> TCRoseRow s -> TCRoseRow s -> InferM s b i (Maybe [TCPred s])
roseSubRow i l r = do
  l' <- normalizeRoseSubrow l
  r' <- normalizeRoseSubrow r
  case (l', r') of
    (RoseConcrete lrow, RoseConcrete rrow) -> do
      let lkeys = S.fromList (M.keys lrow)
          rkeys = S.fromList (M.keys rrow)
      unless (lkeys `S.isSubsetOf` rkeys) $ error "cannot satisfy subrow constraint: objects do not match"
      forM_ (M.toList lrow) $ \(k, v) -> unify i v (rrow M.! k)
      pure $ Just []
    _ -> pure Nothing


roseRowEq :: i -> TCRoseRow s -> TCRoseRow s -> InferM s b i (Maybe [TCPred s])
roseRowEq i l r = do
  l' <- normalizeRoseSubrow l
  r' <- normalizeRoseSubrow r
  case (l', r') of
    (RoseConcrete lrow, RoseVar rvar) ->
      unifyConcrete rvar lrow *> pure (Just [])
    (RoseVar lvar, RoseConcrete rrow) ->
      unifyConcrete lvar rrow *> pure (Just [])
    (RoseConcrete lrow, RoseConcrete rrow) -> do
      let lkeys = S.fromList (M.keys lrow)
          rkeys = S.fromList (M.keys rrow)
      unless (lkeys == rkeys) $ error "cannot satisfy row eq constraint: objects do not match"
      zipWithM_ (unify i) (M.elems lrow) (M.elems rrow)
      pure $ Just []
    (RoseVar lvar, RoseVar rvar) ->
      unifyRowVar i lvar (RowVar rvar ) *> pure (Just [])
    _ -> pure Nothing
  where
  unifyConcrete var row =
    unifyRowVar i var (RowConcrete row)

normalizeRoseSubrow
  :: RoseRow (TCTypeVar s)
  -> InferM s b i (RoseRow (TCTypeVar s))
normalizeRoseSubrow (RoseRowCat l r) = do
  l' <- normalizeRoseSubrow l
  r' <- normalizeRoseSubrow r
  case (l', r') of
    (RoseConcrete lrow, RoseConcrete rrow) ->
      pure $ RoseConcrete (lrow <> rrow)
    _ -> pure (RoseRowCat l' r')
normalizeRoseSubrow (RoseRowTy ty) = case ty of
  rv -> case rv of
    RowVar v -> readTvRef v >>= \case
      LinkRow row -> case row of
        RowVar rowVar' -> normalizeRoseSubrow (RoseVar rowVar')
        RowConcrete r -> pure (RoseConcrete r)
      _ -> pure (RoseRowTy ty)
    _ -> pure (RoseRowTy ty)


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

entail :: i -> [TCPred s] -> TCPred s -> InferM s b i Bool
entail i ps p = byInst i p >>= \case
  Nothing -> pure False
  Just qs -> and <$> traverse (entail i ps) qs

typedArgToTypeScheme :: TypedArg (Type n) i -> TypeScheme n
typedArgToTypeScheme (TypedArg _ t _) = TypeScheme [] [] t

isHnf :: TCPred s -> InferM s b i Bool
isHnf (Pred t) = case t of
  Eq ty -> tyHnf ty
  Add ty -> tyHnf ty
  Num ty -> tyHnf ty
  Ord ty -> tyHnf ty
  Show ty -> tyHnf ty
  ListLike ty -> tyHnf ty
  Fractional ty -> tyHnf ty
  EnforceRead ty -> tyHnf ty
  IsValue ty -> tyHnf ty
  EqRow ty -> rowTyHnf ty
  RoseSubRow l r -> do
    (||) <$> roseRowHnf l <*> roseRowHnf r
  RoseRowEq l r -> (||) <$> roseRowHnf l <*> roseRowHnf r
  where
  roseRowHnf row = case row of
    RoseRowTy rty -> rowTyHnf rty
    RoseRowCat l r -> (||) <$> roseRowHnf l <*> roseRowHnf r
  rowTyHnf = \case
    RowVar v -> readTvRef v >>= \case
      LinkRow r -> rowTyHnf r
      _ -> pure True
    RowConcrete rows ->
      or <$> traverse tyHnf rows

  -- unbounds <- traverseTCType tyHnf t
  -- pure $ and unbounds

tyHnf :: TCType s -> InferM s b i Bool
tyHnf = \case
  TyVar tv -> readTvRef tv >>= \case
    LinkTy ty -> tyHnf ty
    _ -> pure True
  TyObject c -> rowCtorHnf c
  TyTable c -> rowCtorHnf c
  _ -> pure False
  where
  rowCtorHnf = \case
    RowVar v -> readTvRef v >>= \case
      LinkRow r -> rowCtorHnf r
      Bound{} -> pure True
      _ -> pure False
    RowConcrete rows ->
      and <$> traverse tyHnf rows

toHnf :: i -> TCPred s -> InferM s b i [TCPred s]
toHnf i p = isHnf p >>= \case
  True -> pure [p]
  False -> byInst i p >>= \case
    Nothing -> do
      t' <- _dbgPred p
      error $ "context reduction error "  <> show t'
    Just ps -> toHnfs i ps

toHnfs :: i -> [TCPred s] -> InferM s b i [TCPred s]
toHnfs i ps = do
  pss <- traverse (toHnf i) ps
  pure (concat pss)

simplify :: i -> [TCPred s] -> InferM s b i [TCPred s]
simplify i = loop []
  where
  loop rs [] = pure rs
  loop rs (p:ps) = entail i (rs ++ ps) p >>= \cond ->
    if cond then loop rs ps else loop (p:rs) ps

reduce :: i -> [TCPred s] -> InferM s b i [TCPred s]
reduce i ps = toHnfs i ps >>= simplify i

-- | Split typeclass constraints into
--   those that are resolved and those that have unresolved type variables
split
  :: [TCPred s]
  -> i
  -> InferM s b i ([TCPred s], [TCPred s])
split ps i = do
  ps' <- reduce i ps
  partition' ([], []) ps'
  where
  partition' (ds, rs) (p@(Pred ty) : xs) = do
    cond <- tcHasUnbound ty
    if cond then partition' (p:ds, rs) xs
    else partition' (ds, p:rs) xs
  partition' (ds, rs) [] =
    pure (reverse ds, reverse rs)
  varUnbound ref = readTvRef ref >>= \case
    Unbound{} -> pure True
    LinkTy ty -> hasUnbound ty
    LinkRow r -> rowUnbound r
    Bound {} -> pure False
  tcHasUnbound = \case
    Eq t -> hasUnbound t
    Ord t -> hasUnbound t
    Show t -> hasUnbound t
    Add t -> hasUnbound t
    Num t -> hasUnbound t
    ListLike t -> hasUnbound t
    Fractional t -> hasUnbound t
    EnforceRead t -> hasUnbound t
    IsValue t -> hasUnbound t
    EqRow t -> rowUnbound t
    RoseSubRow l r ->
      (&&) <$> roseUnbound l <*> roseUnbound r
    RoseRowEq l r ->
      (&&) <$> roseUnbound l <*> roseUnbound r
  roseUnbound = \case
    RoseRowTy r -> rowUnbound r
    RoseRowCat l r -> (&&) <$> roseUnbound l <*> roseUnbound r
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
    TyCapToken -> pure False
  rowUnbound = \case
    RowVar n -> varUnbound n
    RowConcrete n -> or <$> traverse hasUnbound n

checkReducible :: i -> [TCPred s] -> InferM s b i ()
checkReducible i ps =
  reduce i ps >>= \case
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
instantiateWithTerm
  :: TypeScheme (TCTypeVar s)
  -> TCTerm s b i
  -> InferM s b i (TCType s, TCTerm s b i, [TCPred s])
instantiateWithTerm (TypeScheme [] [] t) te = pure (t, te, [])
instantiateWithTerm (TypeScheme ts preds ty) term = do
  nts <- traverse (\tv -> newTypeVar (_tpKind tv)) ts
  let m = zip ts nts
  preds' <- traverse (instPred m) preds
  ty' <- instBound m ty
  term' <- case TyAppVar <$> nts of
    x:xs -> do
      pure $ TyApp term (x:|xs) info
    [] -> pure term
  pure (ty', term', preds')
  where
  info = term ^. termInfo
  instPred m (Pred tt) =
    Pred <$> traverseTCType (instBound m) tt
  instRow m = \case
    r@(RowVar rv) -> readTvRef rv >>= \case
      Bound{} -> case lookup rv m of
        Just t' -> pure (RowVar t')
        Nothing -> pure r
      LinkRow lt -> instRow m lt
      _ -> pure r
    RowConcrete r ->
      RowConcrete <$> traverse (instBound m) r
  instBound m = \case
    t@(TyVar tv) -> readTvRef tv >>= \case
      Bound{} -> case lookup tv m of
        Just t' -> pure (TyVar t')
        Nothing -> pure t
      LinkTy lt -> instBound m lt
      _ -> pure t
    TyPrim p -> pure (TyPrim p)
    TyCapToken -> pure TyCapToken
    TyNullary t -> TyNullary <$> instBound m t
    TyModRef mrs -> pure (TyModRef mrs)
    TyFun l r ->
      TyFun <$> instBound m l <*> instBound m r
    TyList t -> TyList <$> instBound m t
    TyObject o -> TyObject <$> instRow m o
    TyTable o -> TyTable <$> instRow m o

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
    Pred <$> instTC rl tc
  instNamed rl (TypeVar (NamedDeBruijn i' _) kind) =
    pure $ TypeVar (rl RAList.!! i') kind
  instTC rl = \case
    Eq t -> Eq <$> inst rl t
    Ord t -> Ord <$> inst rl t
    Show t -> Show <$> inst rl t
    Add t -> Add <$> inst rl t
    Num t -> Num <$> inst rl t
    ListLike t -> ListLike <$> inst rl t
    Fractional t -> Fractional <$> inst rl t
    EnforceRead t -> EnforceRead <$> inst rl t
    IsValue t -> IsValue <$> inst rl t
    EqRow rty -> EqRow <$> instRow rl rty
    RoseSubRow l r ->
      RoseSubRow <$> instRoseSubrow rl l <*> instRoseSubrow rl r
    RoseRowEq l r ->
      RoseRowEq <$> instRoseSubrow rl l <*> instRoseSubrow rl r
  instRoseSubrow rl = \case
    RoseRowTy r -> RoseRowTy <$> instRow rl r
    RoseRowCat l r -> RoseRowCat <$> instRoseSubrow rl l <*> instRoseSubrow rl r
  inst rl = \case
    TyVar v -> TyVar <$> instNamed rl v
    TyPrim p -> pure (TyPrim p)
    TyFun l r -> TyFun <$> inst rl l <*> inst rl r
    TyList t -> TyList <$> inst rl t
    TyModRef mr -> pure (TyModRef mr)
    TyNullary n -> TyNullary <$> inst rl n
    TyObject rc -> TyObject <$> instRow rl rc
    TyTable rc -> TyTable <$> instRow rl rc
    TyCapToken -> pure TyCapToken
  -- instCapVar rl = \case
  --   CapVar n -> CapVar <$> instNamed rl n
  --   CapConcrete fp -> pure (CapConcrete fp)
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
occurs i tv tct = go tct
  where
  go = \case
    TyVar tv' | tv == tv' -> do
      tl <- _dbgType (TyVar tv)
      tr <- _dbgType tct
      throwTypecheckError i $ InfiniteType tl tr
    TyVar tv' -> bindRef tv'
    TyFun l r -> go l *> go r
    TyNullary l -> go l
    TyObject l -> occursRow' l
    TyTable l -> occursRow' l
    TyList l -> go l
    TyPrim{} -> pure ()
    TyModRef{} -> pure ()
    TyCapToken -> pure ()
  occursRow' = \case
    RowVar tv' | tv == tv' -> do
      tl <- _dbgType (TyVar tv)
      tr <- _dbgType tct
      throwTypecheckError i $ InfiniteType tl tr
    RowVar tv' -> bindRef tv'
    RowConcrete t -> traverse_ go t
  bindRef tv' = readTvRef tv' >>= \case
    Unbound n u l' -> do
      ml <- minLevel
      writeTvRef tv' (Unbound n u ml)
      where
      minLevel = readTvRef tv >>= \case
        Unbound _ _ l -> pure (min l l')
        _ -> pure l'
    LinkTy ty -> go ty
    _ -> pure ()

occursRow
  :: i
  -> TCTypeVar s
  -> TCRowCtor s
  -> InferM s b' i ()
occursRow i tv tct = go tct
  where
  go = \case
    RowVar tv' | tv == tv' -> do
      tl <- _dbgType (TyVar tv)
      -- Todo: TyObject is not the correct type here for the occurs check
      tr <- _dbgType (TyObject tct)
      throwTypecheckError i $ InfiniteType tl tr
    RowVar tv' -> bindRef tv'
    RowConcrete t -> traverse_ (occurs i tv) t
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
  :: i
  -> TCTypeVar s
  -> PactKind
  -> InferM s' b i ()
ensureWellKinded i (TypeVar _ k) expected
  | k == expected = pure ()
  | otherwise =
    throwTypecheckError i (WrongKind k)


unifyTyVar
  :: i
  -> TCTypeVar s
  -> TCType s
  -> InferM s b' i ()
unifyTyVar i tv t1 = do
  ensureWellKinded i tv TyKind
  readTvRef tv >>= \case
    Unbound{} -> do
      occurs i tv t1
      writeTvRef tv (LinkTy t1)
    Bound{} -> do
      t1' <- _dbgType t1
      t2' <- _dbgType $ TyVar tv
      throwTypecheckError i $ UnificationFailure t1' t2'
    LinkTy t2 -> unify i t2 t1
    LinkRow row -> do
      row' <- _dbgRowCtor row
      throwTypecheckError i $ InvariantRowInTypeVarPosition row'

unifyRowVar
  :: i
  -> TCTypeVar s
  -> TCRowCtor s
  -> InferM s b' i ()
unifyRowVar i tv t1 = do
  ensureWellKinded i tv RowKind
  readTvRef tv >>= \case
    Unbound{} -> do
      -- todo: occurs check for rows row
      -- occurs tv t1 i
      occursRow i tv t1
      traceM "writing linkRow to unbound"
      writeTvRef tv (LinkRow t1)
    LinkRow row -> do
      traceM $ "unifying row"
      unifyRow i row t1
    Bound{} -> error "cannot unify with bound variable"
    _ -> error "Impossible"

-- TODO: Captoken unif.
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
unify _ TyCapToken TyCapToken = pure ()
unify i (TyObject r) (TyObject l) = unifyRow i l r
unify i (TyTable r) (TyTable l) = unifyRow i l r
unify i (TyNullary l) (TyNullary r) = unify i l r
unify _i (TyModRef mr) (TyModRef mr') | mr == mr' = pure ()
unify i t1 t2 = do
  t1' <- _dbgType t1
  t2' <- _dbgType t2
  throwTypecheckError i (UnificationFailure t1' t2')

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
  when (M.keys lrow /= M.keys rrow) $ error "cannot unify rows"
  () <$ zipWithM (unify i) (M.elems lrow) (M.elems rrow)

-- | Traverse a type and normalize all `LinkTy` and `LinkRow`s
traverseTypeLinks :: TCType s -> InferM s b i (TCType s)
traverseTypeLinks = transformM $ \case
  TyVar rv -> readTvRef rv >>= \case
    LinkTy tl -> pure tl
      -- nubPreds' (Pred tc tl :xs) elems
    LinkRow _ -> error "invariant: row in type var position"
    _ -> pure (TyVar rv)
  TyObject rv -> TyObject <$> linkRow rv
  TyTable rv -> TyTable <$> linkRow rv
  t -> pure t
  where
  linkRow = \case
    RowVar rv -> readTvRef rv >>= \case
      LinkRow row -> linkRow row
      LinkTy{} -> error "invariant: type linked in row position"
      _ -> pure (RowVar rv)
    RowConcrete rv -> pure (RowConcrete rv)

-- -- | We essentially only
-- -- generalize on lambdas atm.
generalizeWithTerm
  :: TCType s
  -> [TCPred s]
  -> TCTerm s b i
  -> InferM s b i (TypeScheme (TCTypeVar s), TCTerm s b i, [TCPred s])
generalizeWithTerm ty pp term
  | isValue term = generalizeWithTerm'
  | otherwise = do
    pp' <- reduce (view Typed.termInfo term) pp
    pure (TypeScheme [] [] ty, term, pp')
  where
  isValue = \case
    Typed.Var{} -> True
    Typed.Constant{} -> True
    Typed.Lam{} -> True
    Typed.Builtin{} -> True
    _ -> False

-- Generalization that emits a typed term
-- Note: Deferred predicates are purely for the sake of
-- callsite dictionary overloaded variables.
-- These are currently disabled.
  generalizeWithTerm' = do
    preds <- nubPreds pp
    (deferred, retained) <- split preds (view Typed.termInfo term)
    (ty', (uniques, reverse -> ftvs)) <- runStateT (generalizeOnType ty) (S.empty, [])
    (retained', (uniques', _)) <- runStateT (traverse genPred retained) (uniques, [])
    -- Todo: better error
    when (uniques /= uniques') $ error "type in preds that's not in the return tyoe"
    case (ftvs, retained') of
      ([], []) -> do
        pure (TypeScheme [] [] ty' , term, deferred)
      ([], _) -> error "retained predicates despite no generalized type variables"
      (x:xs, p) -> do
        pure (TypeScheme ftvs p ty', Typed.TyAbs (x:|xs) (_typeclassPredicate <$> p) term info, deferred)
    where
    nubPreds li = do
      nub <$> (traverse.typeclassPredicate.traverseTCType) traverseTypeLinks li
    info = term ^. Typed.termInfo
    genPred (Pred p) = do
      Pred <$> traverseTCType generalizeOnType p
    generalizeOnType = (lift . traverseTypeLinks) >=> generalizeOnType'
    generalizeOnType' = \case
      TyVar tv -> lift (readTvRef tv) >>= \case
        Unbound n u l -> do
          cl <- lift currentLevel
          -- If the generalization depth is _higher_ than the current level, then we will
          -- generalize on that variable
          traceM $ "LEVEL OF UNBOUND" <> show l
          traceM $ "CURR LEVEL" <> show cl
          when (l > cl) $ do
            lift (writeTvRef tv (Bound n u))
            (uniques, ftvs) <- get
            -- If we've already recorded our generalization over this variable, then do nothing
            when (S.notMember u uniques) $ put (S.insert u uniques, tv:ftvs)
            -- Otherwise, record its generalization and
          pure $ TyVar tv
        _ -> pure (TyVar tv)
      TyObject o -> TyObject <$> generalizeRowVars o
      TyTable o -> TyTable <$> generalizeRowVars o
      TyPrim p -> pure (TyPrim p)
      TyFun l r -> do
        l' <- generalizeOnType' l
        r' <- generalizeOnType' r
        pure (TyFun l' r')
      TyNullary r -> TyNullary <$> generalizeOnType' r
      TyModRef mr -> pure (TyModRef mr)
      TyList t -> TyList <$> generalizeOnType' t
      TyCapToken -> pure TyCapToken
    generalizeRowVars = \case
      RowVar tv -> lift (readTvRef tv) >>= \case
        Unbound n u l -> do
          cl <- lift currentLevel
          -- If the generalization depth is _higher_ than the current level, then we will
          -- generalize on that variable
          when (l > cl) $ do
            lift (writeTvRef tv (Bound n u))
            (uniques, ftvs) <- get
            -- If we've already recorded our generalization over this variable, then do nothing
            when (S.notMember u uniques) $ put (S.insert u uniques, tv:ftvs)
            -- Otherwise, record its generalization and
          pure $ RowVar tv
        -- LinkTy t' -> generalizeOnType t'
        -- Bound _ _ -> pure (TyVar tv)
        _ -> pure (RowVar tv)
      RowConcrete r ->
        RowConcrete <$> traverse generalizeOnType' r

    -- _ -> undefined)
    -- (o, pty')  <- gen' pty
    -- when (o /= []) $
    --   lift (throwTypecheckError (TCInvariantFailure "Generalizing predicates") info)
    -- pure (Pred t pty')
  -- gen' (TyVar tv) = lift (readTvRef tv) >>= \case
  --   Unbound n u l -> do
  --     cl <- lift currentLevel
  --     -- If the generalization depth is _higher_ than the current level, then we will
  --     -- generalize on that variable
  --     when (l > cl) $ do
  --       lift (writeTvRef tv (Bound n u))
  --       (uniques, ftvs) <- get
  --       -- If we've already recorded our generalization over this variable, then do nothing
  --       when (S.notMember u uniques) $ put (S.insert u uniques, tv:ftvs)
  --       -- Otherwise, record its generalization and
  --     pure $ TyVar tv
  --   LinkTy t' -> gen' t'
  --   Bound _ _ -> pure ([], TyVar tv)
  --   _ -> error "Invariant failure: row bound as type"
  -- gen' (TyFun l r) = do
  --   (ftvl, l') <- gen' l
  --   (ftvr, r') <- gen' r
  --   pure (ftvl ++ ftvr,TyFun l' r')
  -- gen' t@TyPrim{} = pure ([], t)
  -- gen' (TyList t) = over _2 TyList <$> gen' t
  -- gen' (TyModRef mr) = pure ([], TyModRef mr)
  -- gen' (TyNullary n) = over _2 TyNullary <$> gen' n
  -- gen' (TyObject o) = undefined
  -- gen' (TyTable o) = undefined
  -- gen' TyCapToken = pure TyCapToken

checkTermType
  :: (TypeOfBuiltin b)
  => TCType s
  -> IR.Term Name IR.Type b i
  -> InferM s b i (TCType s, TCTerm s b i, [TCPred s])
checkTermType checkty = \case
  IR.Var irn@(Name n nk) i -> case nk of
    NBound u -> do
      views tcVarEnv (`RAList.lookup` u) >>= \case
        Just nty -> do
          traceM "looking up bound var"
          let newVar = Var irn i
          (rty, term', preds) <- instantiateWithTerm nty newVar
          traceM "post instantiate"
          unify i rty checkty
          pure (rty, term', preds)
        Nothing ->
          throwTypecheckError i $ InvariantUnboundTermVariable n
    NTopLevel mn mh -> do
      let fqn = FullyQualifiedName mn n mh
      use (tcFree . at fqn) >>= \case
        Just nty -> do
          (rty, tyvars, preds) <- instantiateImported nty i
          unify i rty checkty
          let newVar = Var irn i
          case tyvars of
            [] -> pure (rty, newVar, preds)
            (x:xs) -> pure (rty, TyApp newVar (TyAppVar x :| fmap TyAppVar xs) i, preds)
        _ ->
          throwTypecheckError i $ InvariantUnboundFreeVariable fqn
    NModRef m ifs -> case checkty of
      TyModRef mn -> do
        let newVar = Var irn i
        let ifSet = S.fromList ifs
        if mn `S.isSubsetOf` ifSet  then
          pure (TyModRef mn, newVar, [])
        else
          throwTypecheckError i $ ModuleLacksImplementedInterfaces m (ifSet `S.difference` mn)
      v -> do
          let implInterfacesSet = S.fromList ifs
          unify i v (TyModRef implInterfacesSet)
          pure (TyModRef implInterfacesSet, Var irn i, [])
    _ -> error "TODO :: dynref not supported"
  -- Todo: lambda checking cancan be a bit better.
  term@(IR.Lam _ _ i) -> do
    traceM "in checkTy lam"
    tup <- inferTerm term
    unify i (view _1 tup) checkty
    pure tup
  IR.Let (Arg txt m_ty largInfo) e1 e2 i -> do
    traceM "in checkTy let"
    case m_ty of
      Just lty -> do
        let lty' = liftCoreType lty
        let ltys = TypeScheme [] [] lty'
        (_, e1', pe1) <- checkTermType lty' e1
        (_, e2', pe2) <-
          locally tcVarEnv (RAList.cons ltys) $ checkTermType checkty e2
        let term' = Let (TypedArg txt lty' largInfo) e1' e2' i
        pure (checkty, term', pe1 ++ pe2)
      Nothing -> do
        enterLevel
        (te1, e1', pe1) <- inferTerm e1
        leaveLevel
        -- Note:
        -- (ts, e1Qual, deferred) <- generalizeWithTerm te1 pe1 e1'
        (te2, e2', pe2) <-
          locally tcVarEnv (RAList.cons (TypeScheme [] [] te1)) $ checkTermType checkty e2
        let term' = Let (TypedArg txt te1 largInfo) e1' e2' i
        pure (te2, term', pe1 ++ pe2)
  term@(IR.App _ _ i) -> do
    (termTy, term', preds) <- inferTerm term
    unify i termTy checkty
    pure (termTy, term', preds)
  IR.Sequence l r i -> do
    (_, l', pl) <- inferTerm l
    (_, r', pr) <- checkTermType checkty r
    pure (checkty, Sequence l' r' i, pl ++ pr)
  IR.BuiltinForm cond i -> over _2 (`BuiltinForm` i) <$>
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
      CEnforceOne o li -> do
        unify i checkty TyBool
        (_, o', pe1) <- checkTermType TyString o
        (_, li', pe2) <- checkTermType (TyList TyBool) li
        pure (TyBool, CEnforceOne o' li', pe1 ++ pe2)
      CTry catchE tryE -> do
        (_, catchE', pe1) <- checkTermType checkty catchE
        (_, tryE', pe2) <- checkTermType checkty tryE
        pure (checkty, CTry catchE' tryE', pe1 ++ pe2)
      CWithCapability ct body -> do
        (_, ct', pe1) <- checkTermType TyCapToken ct
        (_, body', pe2) <- checkTermType checkty body
        pure (checkty, CWithCapability ct' body', pe1 ++ pe2)
      CCreateUserGuard c -> case c of
        IR.App{} -> do
          unify i checkty TyGuard
          (t, c', pe1) <- inferTerm c
          pure (TyGuard, CCreateUserGuard c', Pred (IsValue t) : pe1)
        _ -> throwTypecheckError i $ InvariantOther "create-user-guard must take a singular app form"
  IR.Builtin b i -> do
    let tyImported = typeOfBuiltin b
    (ty, tvs, preds) <- instantiateImported tyImported i
    unify i checkty ty
    let term = Builtin b i
    let term' = case TyAppVar <$> tvs of
            [] -> term
            (x:xs) -> TyApp term (x :| xs) i
    pure (ty, term', preds)
  IR.Constant lit i -> case lit of
    -- Todo: constant coercions?
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
  -- IR.Try errcase bodycase i -> do
  --   (_, err', p1) <- checkTermType checkty errcase
  --   (_, body', p2) <- checkTermType checkty bodycase
  --   pure (checkty, Try err' body' i, p1 ++ p2)
  IR.Nullary term i -> inferTerm (IR.Nullary term i)
  IR.ObjectLit fieldMap i -> do
    traceM "inferring object lit"
    m <- traverse (\(f, t) -> (f,) <$> inferTerm t) fieldMap
    traceM "done inferring nested objs"
    let objTyMap = view _1 <$> M.fromList m
        objTerms = over _2 (view _2) <$> m
        objTy = TyObject (RowConcrete objTyMap)
        preds = concat (view (_2._3) <$> m)
    unify i checkty objTy
    traceM "unifying row"
    pure (objTy, ObjectLit objTerms i, preds)
  IR.InlineValue _ _ -> error "InlineValue not supported"


-- -- Todo: bidirectionality
inferTerm
  :: (TypeOfBuiltin b)
  => IR.Term Name IR.Type b i
  -> InferM s b i (TCType s, TCTerm s b i, [TCPred s])
inferTerm = \case
  IR.Var irn@(Name n nk) i -> case nk of
    NBound u -> do
      views tcVarEnv (`RAList.lookup` u) >>= \case
        Just nty -> do
          traceM "inferring local var"
          let newVar = Var irn i
          (rty, term', preds) <- instantiateWithTerm nty newVar
          pure (rty, term', preds)
        Nothing ->
          throwTypecheckError i $ InvariantUnboundTermVariable n
    NTopLevel mn mh -> do
      let fqn = FullyQualifiedName mn n mh
      use (tcFree . at fqn) >>= \case
        Just nty -> do
          (rty, tyvars, preds) <- instantiateImported nty i
          let newVar = Var irn i
          case tyvars of
            [] -> pure (rty, newVar, preds)
            (x:xs) -> pure (rty, TyApp newVar (TyAppVar x :| fmap TyAppVar xs) i, preds)
        _ ->
          throwTypecheckError i $ InvariantUnboundFreeVariable fqn
    NModRef _ ifs -> case ifs of
      [iface] -> do
        let v' = Var irn i
        pure (TyModRef (S.singleton iface), v', [])
      [] -> error "Module reference does not implement any interfaces"
      _ -> error "Cannot infer module reference "
    NDynRef (DynamicRef m r) -> error "dynref"
  IR.Lam nts e i -> do
    traceM "In inferTerm Lam"
    -- let names = fst <$> nts
    ntys <- traverse withTypeInfo nts
    -- Todo: bidirectionality
    let m = RAList.fromList (reverse (typedArgToTypeScheme <$> (NE.toList ntys)))
    (ty, e', preds) <- locally tcVarEnv (m RAList.++) $ inferTerm e
    let rty = foldr TyFun ty (_targType <$> ntys)
    pure (rty, Lam (NE.toList ntys) e' i, preds)
    where
    withTypeInfo (Arg n p i') = case p of
      Just ty -> pure (TypedArg n (liftCoreType ty) i')
      Nothing -> do
        ty <- TyVar <$> newTypeVar TyKind
        pure $ TypedArg n ty i'
  IR.App te apps i -> do
    (ty, retTerm, preds) <- inferApply (Apply te apps i)
    pure (ty, retTerm, preds)
  IR.Let (Arg n mty arg_info) e1 e2 i -> do
    traceM $ "in infer let " <> show n
    enterLevel
    (te1, e1', pe1) <- case mty of
      Nothing -> inferTerm e1
      Just ty -> checkTermType (liftCoreType ty) e1
    leaveLevel
    te1Dbg <- _dbgType te1
    traceM $ "in infer let type" <> show te1Dbg
    let tsArg = TypeScheme [] [] te1
    -- (ts, e1Qual, deferred) <- generalizeWithTerm te1 pe1 e1'
    (te2, e2', pe2) <- locally tcVarEnv (RAList.cons tsArg) $ inferTerm e2
    pure (te2, Let (TypedArg n te1 arg_info) e1' e2' i, pe1 ++ pe2)
  IR.Sequence e1 e2 i -> do
    (_, e1', pe1) <- inferTerm e1
    (te2, e2', pe2) <- inferTerm e2
    pure (te2, Sequence e1' e2' i, pe1 ++ pe2)
  -- Todo: Here, convert to dictionary
  IR.BuiltinForm cond i -> over _2 (`BuiltinForm` i) <$>
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
      CEnforceOne o li -> do
        (_, o', pe1) <- checkTermType TyString o
        (_, li', pe2) <- checkTermType (TyList TyBool) li
        pure (TyBool, CEnforceOne o' li', pe1 ++ pe2)
      CTry catchE tryE -> do
        (tyCatch, catchE', pe1) <- inferTerm catchE
        (tyTry, tryE', pe2) <- inferTerm tryE
        unify i tyCatch tyTry
        pure (tyCatch, CTry catchE' tryE', pe1 ++ pe2)
      CWithCapability ct body -> do
        (_, ct', pe1) <- checkTermType TyCapToken ct
        (rty, body', pe2) <- inferTerm body
        pure (rty, CWithCapability ct' body', pe1 ++ pe2)
      CCreateUserGuard c -> case c of
        IR.App{} -> do
          (t, c', pe1) <- inferTerm c
          pure (TyGuard, CCreateUserGuard c', Pred (IsValue t) : pe1)
        _ -> throwTypecheckError i $ InvariantOther "create-user-guard must take a singular app form"
  IR.Builtin b i -> do
    let tyImported = typeOfBuiltin b
    (ty, tvs, preds) <- instantiateImported tyImported i
    let term = Builtin b i
    let term' = case TyAppVar <$> tvs of
            [] -> term
            (x:xs) -> TyApp term (x :| xs) i
    pure (ty, term', preds)
  -- TODO: note,
  -- for this to work, we have to have proper bidirectionality working, including scoped type variables working fine
  IR.Constant lit i -> case lit of
    -- LInteger{} -> do
    --   tv <- TyVar . (`TypeVar` TyKind) <$> newTvRef
    --   let p = Pred (Num tv)
    --   pure (tv, Constant lit i, [p])
    _ ->
      pure (typeOfLit lit, Constant lit i, [])
  IR.ListLit li i -> do
    tv <- TyVar . (`TypeVar` TyKind) <$> newTvRef
    liTup <- traverse inferTerm li
    let preds = concat (view _3 <$> liTup)
    traverse_ (\(t,_, _) -> unify i tv t) liTup
    pure (TyList tv, ListLit tv (view _2 <$> liTup) i, preds)
  -- IR.Try e1 e2 i -> do
  --   (te1, e1', p1) <- inferTerm e1
  --   (te2, e2', p2)<- inferTerm e2
  --   unify i te1 te2
  --   pure (te1, Try e1' e2' i, p1 ++ p2)
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
  IR.InlineValue{} -> error "InlineValue not supported"

inferApply
  :: TypeOfBuiltin b
  => Apply (IRTerm b i) i
  -> InferM s b i (TCType s, TCTerm s b i, [TCPred s])
inferApply (Apply (IR.Builtin (toSpecialOverload -> Just b) bi) args i) = do
  inferCoreBuiltinOverload i (b, bi) args
inferApply (Apply fun args i) = case args of
  [] -> do
    traceM "inferring app case 1"
    tv1 <- TyVar . (`TypeVar` TyKind) <$> newTvRef
    (tfun, fun', pe1) <- inferTerm fun
    unify i (TyNullary tv1) tfun
    pure (tv1, App fun' [] i, pe1)
  h:hs -> do
    traceM "inferring app case 2"
    (tfun, te', pe1) <- inferTerm fun
    traceM "inferring fn caller"
    (rty, xs, ps) <- foldlM inferFunctionArgs (tfun,[], []) (h:hs)
    traceM "inferred function args"
    let term' = App te' (reverse xs) i
    pure (rty, term', pe1 ++ ps)
    where
    inferFunctionArgs (ta, xs, ps) fnArg = case ta of
      TyFun arg ret -> do
        traceM "inferring function args case 1"
        (_, x', p) <- checkTermType arg fnArg
        pure (ret, x':xs, ps ++ p)
      _ -> do
        traceM "inferring function args case 2"
        tv1 <- TyVar . (`TypeVar` TyKind) <$> newTvRef
        (tArg, fnArg', predsArg) <- inferTerm fnArg
        unify i ta (TyFun tArg tv1)
        pure (tv1,fnArg':xs,ps ++ predsArg)

inferCoreBuiltinOverload
  :: TypeOfBuiltin b
  => i
  -> (SpecialOverload, i)
  -> [IRTerm b i]
  -> InferM s b i (TCType s, TCTerm s b i, [TCPred s])
inferCoreBuiltinOverload info (b, bi) args =
  inferCoreBuiltinOverload' b args
  where
  inferCoreBuiltinOverload' FormatOverload [x, IR.ListLit lis _li] = do
    (ty, x', p1) <- inferTerm x
    unify info ty TyString
    inferred <- traverse inferTerm lis
    let constrs = concat [ (Pred (Show t):preds) | (t, _, preds) <- inferred ]
    let li' = view _2 <$> inferred
    let retTerm = Typed.Format x' li' info
    pure (TyString, retTerm, p1 ++ constrs)
  inferCoreBuiltinOverload' AccessOverload (IR.Constant (LString field) _:xs) = do
    (ty, tvs, preds) <- instantiateImported (objAccessType (Field field)) bi
    (rty, xs', ps) <- foldlM inferFunctionArgs (ty, [], []) xs
    let term' = applyTypeVars (ObjectOp (ObjAccess (Field field)) info) bi tvs
    pure (rty, Typed.App term' (reverse xs') info, preds ++ ps)
  inferCoreBuiltinOverload' AddOverload [x, y] = do
    (t1, x', px) <- inferTerm x
    (t2, y', py) <- inferTerm y
    (tyAdd, tvs, preds)  <- case (t1, t2) of
      (TyObject{}, _) -> instantiateImported addObjType bi
      (_, TyObject{}) -> instantiateImported addObjType bi
      _ -> instantiateImported (typeOfBuiltin CoreAdd) bi
    tv <- TyVar <$> newTypeVar TyKind
    unify info tyAdd (t1 :~> t2 :~> tv)
    -- TODO: this needs to be turned into a special form for sure
    let retTerm = Typed.App (applyTypeVars (Typed.Builtin (review _CoreBuiltin CoreAdd) bi) bi tvs)  [x', y'] info
    pure (tv, retTerm, px ++ py ++ preds)
  inferCoreBuiltinOverload' (specialOverLoadToCoreBuiltin -> b') xs = do
    (ty, tvs, preds) <- instantiateImported (typeOfBuiltin b') bi
    (rty, xs', ps) <- foldlM inferFunctionArgs (ty, [], []) xs
    let term = applyTypeVars (Builtin (review _CoreBuiltin b') bi) bi tvs
    pure (rty, Typed.App term (reverse xs') info, preds ++ ps)
  inferFunctionArgs (ta, xs, ps) fnArg = case ta of
    TyFun arg ret -> do
      (_, x', p) <- checkTermType arg fnArg
      pure (ret, x':xs, ps ++ p)
    _ -> do
      tv1 <- TyVar . (`TypeVar` TyKind) <$> newTvRef
      (tArg, fnArg', predsArg) <- inferTerm fnArg
      unify info ta (TyFun tArg tv1)
      pure (tv1,fnArg':xs,ps ++ predsArg)

  objAccessType field =
    let z1 = RowVariable 1 "z"
        tfield = TypeVariable 0 "t"
        concreteRow = M.singleton field (TyVar tfield)
        constr = Pred $ RoseSubRow (RoseRowTy (RowConcrete concreteRow)) (RoseRowTy (RowVar z1))
    in TypeScheme [z1, tfield] [constr] (TyObject (RowVar z1) :~> TyVar tfield)
  addObjType =
    let z1 = RowVariable 2 "z1"
        z2 = RowVariable 1 "z2"
        z3 = RowVariable 0 "z3"
        constrL = RoseRowCat (RoseRowTy (RowVar z1)) (RoseRowTy (RowVar z2))
        constrR = RoseRowTy (RowVar z3)
        constr = Pred $ RoseRowEq constrL constrR
    in
      TypeScheme [z1, z2, z3] [constr] (TyObject (RowVar z1) :~> TyObject (RowVar z2) :~> TyObject (RowVar z3))

applyTypeVars :: Term name tyname builtin info -> info -> [tyname] -> Term name tyname builtin info
applyTypeVars term i = \case
  [] -> term
  (x:xs) -> TyApp term (TyAppVar x :| (TyAppVar <$> xs)) i


enforceTypeIsPresent :: Maybe IR.Type -> Type a
enforceTypeIsPresent = \case
  Just ty -> liftCoreType ty
  _ -> error "pact-core requires type annotations"

enforceArgType :: Arg IR.Type i -> InferM s b i (TypedArg (Type (TCTypeVar s)) i)
enforceArgType (Arg n mty i) =
  case mty of
    Nothing -> do
      tv <- newTypeVar TyKind
      pure $ TypedArg n (TyVar tv) i
    Just ty ->
     pure $ TypedArg n (liftCoreType ty) i

-- -- Todo: generic types?
-- -- We can't generalize yet since
-- -- we're not allowing type schemes just yet.
inferDefun
  :: TypeOfBuiltin b
  => ModuleName
  -> ModuleHash
  -> IR.Defun Name IR.Type b i
  -> InferM s b i (TypedDefun b i)
inferDefun mn mh (IR.Defun spec dfargs term info) = do
  traceM $ "Inferring defun" <> show (() <$ spec)
  enterLevel
  let name = _argName spec
  typedArgs <- traverse enforceArgType dfargs
  spec' <- enforceArgType spec
  let dfTy = case typedArgs of
        [] -> TyNullary (_targType spec')
        _ -> foldr TyFun (_targType spec') (_targType <$> typedArgs)
  (termTy, term', preds) <- inferTerm term
  leaveLevel
  traceM "done inferring defun"
  -- checkReducible info preds
  -- fail "typeclass constraints not supported in defun"
  unify info dfTy termTy
  traceM "unification no links"
  (tys@(TypeScheme tvs _ ty), tcterm, deferred) <- generalizeWithTerm termTy preds term'
  tyDbged <- _dbgType ty
  varsDbged <- traverse _dbgVar tvs
  traceM $ "term gen worked " <> show varsDbged <> show tyDbged
  deferred' <- reduce info deferred
  when (deferred' /= []) $ do
    deferredDbg <- traverse _dbgPred deferred
    throwTypecheckError info $ CannotResolveConstraints deferredDbg
  debruijnizedTypeScheme <- debruijnizeTypeScheme info tys
  t' <- _dbgType ty
  traceM $ "here2 " <> show t'
  -- fterm <- noTyVarsinTerm info term'
  fterm <- debruijnizeTermTypes info tcterm
  let deleteArgTypes (TypedArg n _ i) = Arg n Nothing i
  tcFree %= M.insert (FullyQualifiedName mn name mh) debruijnizedTypeScheme
  pure (Defun name (deleteArgTypes <$> typedArgs) debruijnizedTypeScheme fterm info)

inferDefConst
  :: ModuleName
  -> ModuleHash
  -> IR.DefConst Name IR.Type b i
  -> InferM s b i (TypedDefConst b i)
inferDefConst mn mh (IR.DefConst spec cv info) = case cv of
  IR.EvaledConst v -> do
    let name = _argName spec
    pvt <- inferPactValue info v
    let dcTy' = liftCoreType <$> (_argType spec)
    _ <- traverse (unify info pvt) dcTy'
    rty <- ensureNoTyVars info (maybe pvt id dcTy')
    tcFree %= M.insert (FullyQualifiedName mn name mh) (TypeScheme [] [] rty)
    pure (DefConst name rty v info)
  _ -> do
    throwTypecheckError info $ InvariantDefconstNotEvaluated $ FullyQualifiedName mn (_argName spec) mh

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
  PModRef mr -> pure $ TyModRef (_mrImplemented mr)
  PCapToken _ ->
    pure $ TyCapToken
  PTable tbl -> do
    let (IR.Schema _ schema) = _tvSchema tbl
    let schemaTys = liftCoreType <$> schema
    pure $ TyTable $ RowConcrete $ schemaTys
  PTime _ -> pure $ TyTime


inferDefCap
  :: TypeOfBuiltin b
  => ModuleName
  -> ModuleHash
  -> IR.DefCap Name IR.Type b i
  -> InferM s b i (TypedDefCap b i)
inferDefCap mn mh (IR.DefCap spec args term meta info) = do
  let name = _argName spec
  enterLevel
  retArg <- enforceArgType spec
  args' <- traverse enforceArgType args
  let rty' = _targType retArg
  let fnCapTy = case args of
        [] -> TyNullary rty'
        _ -> foldr TyFun rty' (_targType <$> args')
  let m = RAList.fromList (reverse (typedArgToTypeScheme <$> args'))
  (termTy, term', preds) <- locally tcVarEnv (m RAList.++) $ inferTerm term
  leaveLevel
  unify info fnCapTy (set returnType termTy fnCapTy)
  -- Note: This can have some kind of gnarly consequences if for whatever reason there is a constraint that
  -- the return type requires.
  --
  -- However for defcaps, we ideally want to avoid this in the first place, so
  -- if this errors out here, that's likely a good thing, but we will need a good error
  -- in the future in case it does
  let tygen = set returnType TyCapToken fnCapTy

  (tys, tcterm, deferred) <- generalizeWithTerm tygen preds term'
  reduced <- reduce info deferred
  when (reduced /= []) $ do
    deferred' <- traverse _dbgPred deferred
    throwTypecheckError info $ CannotResolveConstraints deferred'
  debruijnizedTypeScheme <- debruijnizeTypeScheme info tys
  fterm <- debruijnizeTermTypes info tcterm

  tcFree %= M.insert (FullyQualifiedName mn name mh) debruijnizedTypeScheme
  let deleteArgTypes (TypedArg n _ i) = Arg n Nothing i

  pure (DefCap name (deleteArgTypes <$> args') debruijnizedTypeScheme fterm meta info)

inferTable
  :: ModuleName
  -> ModuleHash
  -> IR.DefTable Name i
  -> InferM s b i (DefTable i)
inferTable mn mh (IR.DefTable tn (IR.ResolvedTable (IR.Schema qn schema)) info) = do
  let schema' = liftCoreType <$> schema
  tcFree %= M.insert (FullyQualifiedName mn tn mh) (TypeScheme [] [] (TyTable (RowConcrete (liftType <$> schema'))))
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
inferModule (IR.Module mname mgov defs blessed imports impl mh _ _ info) = do
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
-- inferTermNonGen
--   :: TypeOfBuiltin b
--   => IRTerm b i
--   -> InferM s b i (TypeScheme NamedDeBruijn, TypedTerm b i)
-- inferTermNonGen t = do
--   (ty, t', preds) <- inferTerm t
--   checkReducible (view IR.termInfo t) preds
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
--   => IRTopLevel b i
--   -> InferM s b i (TypedTopLevel b i)
-- inferTopLevel = \case
--   IR.TLModule m ->
--     TLModule <$> inferModule m
--   IR.TLTerm m -> TLTerm . snd <$> inferTermNonGen m
--   IR.TLInterface _i -> undefined
--   IR.TLUse tl i -> pure (TLUse tl i)

-- inferReplTopLevel
--   :: TypeOfBuiltin b
--   => IR.ReplTopLevel Name IR.Type b i
--   -> InferM s b i (TypedReplTopLevel b i)
-- inferReplTopLevel = \case
--   IR.RTLDefun dfn -> do
--     dfn' <- inferDefun replModuleName replModuleHash dfn
--     pure (RTLDefun dfn')
--   IR.RTLDefConst dconst -> do
--     dc <- inferDefConst replModuleName replModuleHash dconst
--     pure (RTLDefConst dc)

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
  -> InferM s b' i (Term Name DebruijnTypeVar b i)
debruijnizeTermTypes info = dbj [] 0
  where
  debruijnizeTypeApp env = \case
    TyAppVar n -> case lookup n env of
      Just v -> pure (TyAppVar v)
      Nothing -> readTvRef n >>= \case
        Unbound n' _ _ ->
          throwTypecheckError info $ InvariantUnboundTypeVariable n'
        Bound n' _ ->
          throwTypecheckError info $ InvariantUnboundTypeVariable n'
        LinkTy ty ->
          TyAppType <$> debruijnizeType info env ty
        LinkRow row ->
          TyAppRow <$> debruijnizeRowCtor info env row
    TyAppType ty -> TyAppType <$> debruijnizeType info env ty
    TyAppRow row -> TyAppRow <$> debruijnizeRowCtor info env row
  dbj
    :: [(TCTypeVar s, DebruijnTypeVar)]
    -> DeBruijn
    -> TCTerm s b i
    -> InferM s b' i (Term Name DebruijnTypeVar b i)
  dbj env depth = go
    where
    go = \case
      Var n i ->
        pure (Var n i)
      Lam nts e i -> do
        nts' <- (traversed.targType) (debruijnizeType info env) nts
        e' <- dbj env depth e
        pure (Lam nts' e' i)
      App l r i ->
        App <$> go l <*> traverse go r <*> pure i
      Let n e1 e2 i -> do
        n' <- targType (debruijnizeType info env) n
        e1' <- go e1
        e2' <- go e2
        pure (Let n' e1' e2' i)
      TyAbs ntys btc e i -> do
        let len = fromIntegral (NE.length ntys)
            ixs = NE.fromList [depth .. depth + len - 1]
        names <- traverse (nameTvs info (depth + len)) (NE.zip ntys ixs)
        let env' = (NE.toList $ NE.zip ntys names) ++ env
        btc' <- traverse (debruijnizeBuiltinTC i env') btc
        e' <- dbj env' (depth + len) e
        pure $ TyAbs names btc' e' i
      DictApp e btc i -> do
        e' <- dbj env depth e
        btc' <- traverse (debruijnizeBuiltinTC i env) btc
        pure (DictApp e' btc' i)
      TyApp e args i -> do
        e' <- dbj env depth e
        args' <- traverse (debruijnizeTypeApp env) args
        pure (TyApp e' args' i)
      Sequence e1 e2 i ->
        Sequence <$> dbj env depth e1 <*> dbj env depth e2 <*> pure i
      BuiltinForm c i ->
        BuiltinForm <$> traverse (dbj env depth) c <*> pure i
      Try e1 e2 i ->
        Try <$> dbj env depth e1 <*> dbj env depth e2 <*> pure i
      ListLit ty v i ->
        ListLit <$> debruijnizeType info env ty <*> traverse (dbj env depth) v <*> pure i
      Builtin b i ->
        pure (Builtin b i)
      ObjectOp o i ->
        pure (ObjectOp o i)
      Format o o' i ->
        Format <$> dbj env depth o <*> traverse (dbj env depth) o' <*> pure i
      -- CapabilityForm cf i ->
      --   CapabilityForm <$> traverse (dbj env depth) cf <*> pure i
      Constant l i -> pure (Constant l i)
      ObjectLit l i ->
        ObjectLit <$> (traversed._2) go l <*> pure i


nameTvs
  :: i
  -> DeBruijn
  -> (TCTypeVar s, DeBruijn)
  -> InferM s b i DebruijnTypeVar
nameTvs info depth (tv@(TypeVar _ k), i) = readTvRef tv >>= \case
  Bound n _ ->
    pure (TypeVar (NamedDeBruijn (depth - i - 1) n) k)
  _ ->
    throwTypecheckError info $ InvariantUnboundTypeVariable "_todo"

ensureNoTyVars
  :: i
  -> TCType s
  -> InferM s b i (Type a)
ensureNoTyVars i = \case
  TyVar n -> readTvRef n >>= \case
    LinkTy ty -> ensureNoTyVars i ty
    LinkRow row -> ensureNoTyVars i (TyObject row)
    -- LinkCap r -> ensureNoTyVars i (TyCapToken r)
    _ -> do
      dbg <- _dbgType (TyVar n)
      throwTypecheckError i $ DisallowedGenericSignature dbg
  TyPrim p -> pure (TyPrim p)
  TyFun l r -> TyFun <$> ensureNoTyVars i l <*> ensureNoTyVars i r
  TyList l -> TyList <$> ensureNoTyVars i l
  TyModRef mr -> pure (TyModRef mr)
  TyObject r -> TyObject <$> ensureNoTvRowCtor r
  TyTable r -> TyTable <$> ensureNoTvRowCtor r
  TyNullary r -> TyNullary <$> ensureNoTyVars i r
  TyCapToken -> pure TyCapToken
    -- CapVar rv -> readTvRef rv >>= \case
    --   LinkCap t -> ensureNoTyVars i (TyCapToken t)
    --   _ -> error "inferred generic signature"
    -- CapConcrete fq -> pure (TyCapToken (CapConcrete fq))
  where
  ensureNoTvRowCtor (RowVar rv) = readTvRef rv >>= \case
    LinkRow row -> ensureNoTvRowCtor row
    _ -> error "inferred generic signature"
  ensureNoTvRowCtor (RowConcrete row) =
    RowConcrete <$> traverse (ensureNoTyVars i) row

-- ensureNoTyVarsPred
--   :: i
--   -> TCPred s
--   -> InferM s b i (Pred (Type NamedDeBruijn))
-- ensureNoTyVarsPred i (Pred tc) =
--   Pred <$> traverse (ensureNoTyVars i) tc

-- noTyVarsinTerm
--   :: i
--   -> TCTerm s b' i
--   -> InferM s b i (TypedTerm b' i)
-- noTyVarsinTerm info = \case
--   Var n i ->
--     pure (Var n i)
--   Lam nts e i ->
--     Lam <$> (traversed.argType) (ensureNoTyVars info) nts <*> noTyVarsinTerm info e <*> pure i
--   App e args i ->
--     App <$> noTyVarsinTerm info e <*> traverse (noTyVarsinTerm info) args <*> pure i
--   Let n e1 e2 i ->
--     Let <$> argType (ensureNoTyVars i) n
--         <*> noTyVarsinTerm info e1
--         <*> noTyVarsinTerm info e2
--         <*> pure i
--   Builtin (b, ty, p) i -> do
--     ty' <- traverse (ensureNoTyVars info) ty
--     p' <- traverse (ensureNoTyVarsPred info) p
--     pure $ Builtin (b, ty', p') i
--   TyAbs _ns _e _i ->
--     error "Generic terms are disabled"
--   Constant l i ->
--     pure (Constant l i)
--   TyApp l tys i ->
--     TyApp
--       <$> noTyVarsinTerm info l
--       <*> traverse (ensureNoTyVars info) tys
--       <*> pure i
--   Sequence e1 e2 i ->
--     Sequence <$> noTyVarsinTerm info e1 <*> noTyVarsinTerm info e2 <*> pure i
--   BuiltinForm c i ->
--     Conditional <$> traverse (noTyVarsinTerm info) c <*> pure i
--   ListLit ty li i ->
--     ListLit <$> ensureNoTyVars info ty <*> traverse (noTyVarsinTerm info) li <*> pure i
--   Try e1 e2 i ->
--     Try <$> noTyVarsinTerm info e1 <*> noTyVarsinTerm info e2 <*> pure i
  -- CapabilityForm cf i ->
  --   CapabilityForm <$> traverse (noTyVarsinTerm info) cf <*> pure i
  -- ObjectLit fields i ->
  --   ObjectLit <$> (traversed._2) (noTyVarsinTerm info) fields <*> pure i

debruijnizeTypeScheme
  :: i
  -> TypeScheme (TCTypeVar s)
  -> InferM s b i (TypeScheme DebruijnTypeVar)
debruijnizeTypeScheme i (TypeScheme tvs preds t) = do
    let len = fromIntegral (length tvs)
    let ixs = [0.. len - 1]
    names <- traverse (nameTvs i len) (zip tvs ixs)
    let env = zip tvs names
    t' <- debruijnizeType i env t
    preds' <- traverse (debruijnizePred i env) preds
    pure (TypeScheme names preds' t')

debruijnizeBuiltinTC :: i -> [(TCTypeVar s, DebruijnTypeVar)] -> BuiltinTC (TCTypeVar s) -> InferM s b i (BuiltinTC DebruijnTypeVar)
debruijnizeBuiltinTC i env = \case
  Eq ty -> Eq <$> debruijnizeType i env ty
  Ord ty -> Ord <$> debruijnizeType i env ty
  Show ty -> Show <$> debruijnizeType i env ty
  Add ty -> Add <$> debruijnizeType i env ty
  Num ty -> Num <$> debruijnizeType i env ty
  ListLike ty -> ListLike <$> debruijnizeType i env ty
  Fractional ty -> Fractional <$> debruijnizeType i env ty
  EnforceRead ty -> EnforceRead <$> debruijnizeType i env ty
  IsValue ty -> IsValue <$> debruijnizeType i env ty
  EqRow rt -> EqRow <$> debruijnizeRowCtor i env rt
  RoseSubRow r1 r2 ->
    RoseSubRow <$> debruijnizeRoseRow i env r1 <*> debruijnizeRoseRow i env r2
  RoseRowEq r1 r2 ->
    RoseRowEq <$> debruijnizeRoseRow i env r1 <*> debruijnizeRoseRow i env r2

debruijnizePred
  :: i
  -> [(TCTypeVar s, DebruijnTypeVar)]
  -> TCPred s
  -> InferM s b i (Pred DebruijnTypeVar)
debruijnizePred i env (Pred tc) =
  Pred <$> debruijnizeBuiltinTC i env tc


  -- Pred <$> traverseTCType (debruijnizeType i env) tc

debruijnizeType
  :: i
  -> [(TCTypeVar s, DebruijnTypeVar)]
  -> TCType s
  -> InferM s b i (Type DebruijnTypeVar)
debruijnizeType i env = go
  where
  go = \case
    TyVar n -> case lookup n env of
      Just v -> pure (TyVar v)
      Nothing -> readTvRef n >>= \case
        Unbound nraw _ _ ->
          throwTypecheckError i $ InvariantUnboundTypeVariable nraw
        Bound nraw _ ->
          throwTypecheckError i $ InvariantUnboundTypeVariable nraw
        LinkTy ty -> go ty
        _ -> error "invariant failure: typevar as row"
    TyPrim p -> pure (TyPrim p)
    TyFun l r -> TyFun <$> go l <*> go r
    TyList l -> TyList <$> go l
    TyModRef m -> pure (TyModRef m)
    TyObject r -> TyObject <$> debruijnizeRowCtor i env r
    TyTable r -> TyTable <$> debruijnizeRowCtor i env r
    TyNullary t -> TyNullary <$> go t
    TyCapToken -> pure TyCapToken

debruijnizeRowCtor
  :: i
  -> [(TCTypeVar s, DebruijnTypeVar)]
  -> TCRowCtor s -> InferM s b i (RowTy DebruijnTypeVar)
debruijnizeRowCtor i env = \case
  RowVar rv ->  case lookup rv env of
    Just v -> pure (RowVar v)
    Nothing -> readTvRef rv >>= \case
      Unbound nraw _ _ ->
        throwTypecheckError i $ InvariantUnboundTypeVariable nraw
      Bound nraw _ ->
        throwTypecheckError i $ InvariantUnboundTypeVariable nraw
      LinkRow row -> debruijnizeRowCtor i env row
      _ ->
        error "invariant failure: Found type link in row position"
  RowConcrete r ->
    RowConcrete <$> traverse (debruijnizeType i env) r

debruijnizeRoseRow :: i -> [(TCTypeVar s, DebruijnTypeVar)] -> RoseRow (TCTypeVar s) -> InferM s b i (RoseRow DebruijnTypeVar)
debruijnizeRoseRow i env = \case
  RoseRowTy ty -> RoseRowTy <$> debruijnizeRowCtor i env ty
  RoseRowCat r1 r2 ->
    RoseRowCat <$> debruijnizeRoseRow i env r1 <*> debruijnizeRoseRow i env r2

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
  :: InferM s b i a
  -> ST s (Either (TypecheckError i) a, TCState b i)
runInfer (InferM act) = do
  uref <- newSTRef 0
  lref <- newSTRef 1
  let env = TCEnv uref mempty lref
  runStateT (runReaderT (runExceptT act) env) def

defFqns :: IR.EvalDef b i -> [FullyQualifiedName]
defFqns d = S.toList $ execState (IR.traverseDefTerm getFqFDeps d) mempty
  where
  getFqFDeps = transformM $ \case
    v@(IR.Var (Name n (NTopLevel mn mh)) _) -> do
      modify' (S.insert (FullyQualifiedName mn n mh))
      pure v
    v -> pure v

typecheckModule
  :: TypeOfBuiltin b
  => i
  -> ModuleName
  -> EvalM e b i (Either (TypecheckError i) [Def Name DebruijnTypeVar b i])
typecheckModule i mn = do
  (m, deps) <- getModuleWithDependencies i mn
  let allDeps = M.toList $ M.fromList (toFqDep (IR._mName m) (IR._mHash m) <$> (IR._mDefs m)) <> deps
  let sccInput = flattenSCCs $ stronglyConnComp [ (defn, fqn, defFqns defn)  | (fqn, defn) <- allDeps]
  pure $ fst $ runST (runInfer (traverse (inferDef (IR._mName m) (IR._mHash m))  sccInput))


-- runInferTerm
--   :: TypeOfBuiltin b
--   => Loaded b i
--   -> TCState b i
--   -> IRTerm b i
--   -> Either (TypecheckFailure i) (TypeScheme NamedDeBruijn, TypedTerm b i)
-- runInferTerm lo tcs term0 =
--   fst $ runST $ runInfer lo tcs (inferTermNonGen term0)

-- runInferTermNonGen
--   :: TypeOfBuiltin b
--   => Loaded b' i
--   -> IRTerm b i
--   -> Either (PactError i) (TypeScheme NamedDeBruijn, TypedTerm b i)
-- runInferTermNonGen loaded term0 = runST $
--   runInfer loaded $ inferTermNonGen term0

-- runInferModule
--   :: TypeOfBuiltin b
--   => Loaded b i
--   -> TCState b i
--   -> IRModule b i
--   -> (Either (TypecheckFailure i) (TypedModule b i), TCState b i)
-- runInferModule lo tcs m =
--   runST $ runInfer lo tcs (inferModule m)

-- runInferTopLevel
--   :: TypeOfBuiltin b
--   => Loaded b i
--   -> TCState b i
--   -> IR.TopLevel Name IR.Type b i
--   -> (Either (TypecheckFailure i) (TypedTopLevel b i), TCState b i)
-- runInferTopLevel l tcs tl =
--   runST $ runInfer l tcs (inferTopLevel tl)


-- runInferReplTopLevel
--   :: TypeOfBuiltin b
--   => Loaded b i
--   -> TCState b i
--   -> IR.ReplTopLevel Name IR.Type b i
--   -> (Either (TypecheckFailure i) (TypedReplTopLevel b i), TCState b i)
-- runInferReplTopLevel l tcs tl =
--   runST $ runInfer l tcs (inferReplTopLevel tl)