{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
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
module Pact.Core.IR.Typecheck
 ( runInferTerm
 , runInferTermNonGen
 , runInferModule
 , runInferTopLevel
 , runInferReplTopLevel
 , TypeOfBuiltin(..)
 ) where

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
import Data.Foldable(traverse_, foldlM, toList)
import Data.Functor(($>))
import Data.STRef
import Data.Maybe(mapMaybe, fromMaybe)
import Data.Map(Map)
import Data.Text(Text)
import Data.List.NonEmpty(NonEmpty(..))

import qualified Data.Map.Strict as Map
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.RAList as RAList
import qualified Data.Set as Set

import Pact.Core.Builtin
import Pact.Core.Hash (ModuleHash)
import Pact.Core.Type(PrimType(..), Arg(..), TypedArg(..), BuiltinTC(..))
import Pact.Core.Typed.Type
import Pact.Core.Names
import Pact.Core.Persistence
import Pact.Core.Capabilities
import qualified Pact.Core.Type as IR
import qualified Pact.Core.IR.Term as IR
import qualified Pact.Core.Typed.Term as Typed
import qualified Pact.Core.Typed.Type as Typed

-- inference based on https://okmij.org/ftp/ML/generalization.html
-- Note: Type inference levels in the types
-- a-la sound-lazy might be worth implementing later on.
-- The eager implementation is simpler to maintain and extend to typeclasses.

-- note, we will debruijnize, so this is purely for
-- Display purposes
type UniqueSupply s = STRef s Unique
type Level = Int

data TypecheckError
  = UnificationError (Type Text) (Type Text)
  | ContextReductionError (Pred Text)
  | UnsupportedTypeclassGeneralization [Pred Text]
  | UnsupportedImpredicativity
  | OccursCheckFailure (Type Text)
  | TCInvariantFailure Text
  | TCUnboundTermVariable Text
  | TCUnboundFreeVariable ModuleName Text
  | DisabledGeneralization Text
  deriving Show

data Tv s
  = Unbound !Text !Unique !Level
  | Bound !Text !Unique
  | Link !(Type (TvRef s))
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
  , _tcVarEnv :: RAList (Type (TvRef s))
  -- ^ Builtins map, that uses the enum instance
  -- , _tcFree :: Map ModuleName (Map Text (Type Void))
  , _tcLoaded :: Loaded b i
  -- ^ Free variables
  , _tcLevel :: STRef s Level
  -- ^ Type Variable "Region"
  , _tcModules :: Map ModuleName (ModuleData b i)
  }

makeLenses ''TCEnv

type TCType s = Typed.Type (TvRef s)
type TCPred s = Typed.Pred (TvRef s)

-- | Term emitted by desugar
type IRType = IR.Type
type IRTerm b i = IR.Term Name IRType b i
type IRModule b i = IR.Module Name IRType b i
type IRInterface b i = IR.Interface Name IRType b i

-- | Term emitted by the typechecker prior to final generalization/unification.
type TCTerm s b i = Typed.Term Name (TvRef s) (b, [TCType s], [TCPred s]) i

-- Term/defun outputs post typechecking
-- with ST monad existential removed
type TypedTerm b i = Typed.OverloadedTerm NamedDeBruijn b i

type TypedGenTerm b i = Typed.OverloadedTerm NamedDeBruijn b i

type TypedDefun b i = Typed.OverloadedDefun NamedDeBruijn b i

type TypedDefCap b i = Typed.OverloadedDefCap NamedDeBruijn b i

type TypedDefConst b i = Typed.OverloadedDefConst NamedDeBruijn b i

type TypedDef b i = Typed.OverloadedDef NamedDeBruijn b i

type TypedIfDef b i = Typed.OverloadedIfDef NamedDeBruijn b i

type TypedTopLevel b i = Typed.OverloadedTopLevel NamedDeBruijn b i

type TypedReplTopLevel b i = Typed.OverloadedReplTopLevel NamedDeBruijn b i

type TypedModule b i = Typed.OverloadedModule NamedDeBruijn b i

type TypedInterface b i = Typed.OverloadedInterface NamedDeBruijn b i

-- | Our inference monad, where we can plumb through generalization "regions",
-- our variable environment and our "supply" of unique names
newtype InferM s b i a =
  InferT (ExceptT TypecheckError (ReaderT (TCEnv s b i) (ST s)) a)
  deriving
    ( Functor, Applicative, Monad
    , MonadReader (TCEnv s b i)
    , MonadError TypecheckError)
  via (ExceptT TypecheckError (ReaderT (TCEnv s b i) (ST s)))

class TypeOfBuiltin b where
  typeOfBuiltin :: b -> TypeScheme NamedDeBruijn

instance TypeOfBuiltin RawBuiltin where
  typeOfBuiltin = \case
    -- Num Op
    -- forall a. Num a => a -> a -> a
    RawAdd ->
      addBinopType
    -- forall a. Num a => a -> a -> a
    RawSub ->
      numBinopType
    -- forall a. Num a => a -> a -> a
    RawMultiply ->
      numBinopType
    -- forall a. Num a => a -> a -> a
    RawDivide ->
      numBinopType
    -- forall a. Num a => a -> a
    RawNegate ->
      unaryNumType
    RawPow ->
      numBinopType
    -- forall a. Num a => a -> a -> a
    RawAbs ->
      unaryNumType
    -- Bool ops
    -- bool -> bool
    RawNot ->
      TypeScheme [] [] (TyBool :~> TyBool)
    -- Eq ops
    -- forall a. Eq a => a -> a -> Bool
    RawEq ->
      eqTyp
    -- forall a. Num a => a -> a -> Bool
    RawNeq ->
      eqTyp
    -- Ord ops
    -- forall a. Ord a => a -> a -> Bool
    RawGT ->
      ordTyp
    -- forall a. Ord a => a -> a -> Bool
    RawGEQ ->
      ordTyp
    -- forall a. Ord a => a -> a -> Bool
    RawLT ->
      ordTyp
    -- forall a. Ord a => a -> a -> Bool
    RawLEQ ->
      ordTyp
    -- Integer ops
    -- integer -> integer -> integer
    RawBitwiseAnd ->
      binaryInt
    -- integer -> integer -> integer
    RawBitwiseOr ->
      binaryInt
    -- integer -> integer -> integer
    RawBitwiseXor ->
      binaryInt
    -- integer -> integer
    RawBitwiseFlip ->
      unaryInt
    -- integer -> integer -> integer
    RawMod ->
      binaryInt
    -- integer -> integer
    RawBitShift ->
      binaryInt
    -- Rounding ops
    -- decimal -> integer
    RawRound ->
      roundingFn
    -- decimal -> integer
    RawCeiling ->
      roundingFn
    -- decimal -> integer
    RawFloor ->
      roundingFn
    -- Fractional
    -- forall a. Fractional a => a -> decimal
    RawExp ->
      unaryFractional
    -- forall a. Fractional a => a -> decimal
    RawLn ->
      unaryFractional
    -- forall a. Fractional a => a -> decimal
    RawSqrt ->
      unaryFractional
    -- forall a. Fractional a => a -> a -> a
    RawLogBase ->
      let aVar = nd "a" 0
          a = TyVar aVar
      in TypeScheme [aVar] [Pred Fractional a] (a :~> a :~> a)
    -- ListLike
    RawConcat ->
      let aVar = nd "a" 0
          a = TyVar aVar
      in TypeScheme [aVar] [Pred ListLike a] (TyList a :~> a)
    RawTake ->
      takeDropTy
    RawDrop ->
      takeDropTy
    RawLength ->
      let aVar = nd "a" 0
          a = TyVar aVar
      in TypeScheme [aVar] [Pred ListLike a] (a :~> TyInt)
    RawReverse ->
      let aVar = nd "a" 0
          a = TyVar aVar
      in TypeScheme [aVar] [Pred ListLike a] (a :~> a)
    -- General
    RawMap ->
      let aVar = nd "a" 1
          bVar = nd "b" 0
          a = TyVar aVar
          b = TyVar bVar
      in TypeScheme [aVar, bVar] [] ((a :~> b) :~> TyList a :~> TyList b)
    RawFold ->
      let aVar = nd "a" 1
          bVar = nd "b" 0
          a = TyVar aVar
          b = TyVar bVar
      in TypeScheme [aVar, bVar] [] ((a :~> b :~> a) :~> a :~> TyList b :~> a)
    RawFilter ->
      let aVar = nd "a" 0
          a = TyVar aVar
      in TypeScheme [aVar] [] ((a :~> TyBool) :~> TyList a :~> TyList a)
    RawZip ->
      let aVar = nd "a" 2
          a = TyVar aVar
          bVar = nd "b" 1
          b = TyVar bVar
          cVar = nd "c" 0
          c = TyVar cVar
      in TypeScheme [aVar, bVar, cVar] [] ((a :~> b :~> c) :~> TyList a :~> TyList b :~> TyList c)
    -- RawIf ->
    --   let aVar = nd "a" 0
    --       a = TyVar aVar
    --   in TypeScheme [aVar] [] (TyBool :~> (TyUnit :~> a) :~> (TyUnit :~> a) :~> a)
    RawIntToStr ->
      TypeScheme [] [] (TyInt :~> TyString)
    RawStrToInt ->
      TypeScheme  [] [] (TyString :~> TyInt)
    RawDistinct ->
      let aVar  = nd "a" 0
          a = TyVar aVar
      in TypeScheme [aVar] [Pred Eq a] (TyList a :~> TyList a)
    RawEnforce ->
      TypeScheme [] [] (TyBool :~> TyString :~> TyUnit)
    RawEnforceOne -> error "todo"
    RawEnumerate ->
      TypeScheme [] [] (TyInt :~> TyInt :~> TyList TyInt)
    RawEnumerateStepN ->
      TypeScheme [] [] (TyInt :~> TyInt :~> TyInt :~> TyList TyInt)
    RawShow ->
      let aVar = nd "a" 0
          a = TyVar aVar
      in TypeScheme [aVar] [Pred Show a] (a :~> TyString)
    RawReadInteger ->
      TypeScheme [] [] (TyString :~> TyInt)
    RawReadDecimal ->
      TypeScheme [] [] (TyString :~> TyDecimal)
    RawReadString ->
      TypeScheme [] [] (TyString :~> TyString)
    RawReadKeyset ->
      TypeScheme [] [] (TyString :~> TyGuard)
    RawEnforceGuard ->
      TypeScheme [] [] (TyGuard :~> TyUnit)
    RawKeysetRefGuard ->
      TypeScheme [] [] (TyString :~> TyGuard)
    -- RawCreateUserGuard -> let
    --   a = nd "a" 0
    --   in TypeScheme [a] [] ((TyUnit :~> TyVar a) :~> TyGuard)
    RawAt -> let
      a = nd "a" 0
      in TypeScheme [a] [] (TyInt :~> TyList (TyVar a) :~> TyVar a)
    RawMakeList -> let
      a = nd "a" 0
      in TypeScheme [a] [] (TyInt :~>  TyVar a :~> TyList (TyVar a))
    RawB64Encode ->
      TypeScheme [] [] (TyString :~> TyString)
    RawB64Decode ->
      TypeScheme [] [] (TyString :~> TyString)
    RawStrToList ->
      TypeScheme [] [] (TyString :~> TyList TyString)
    RawSort -> let
      aVar = nd "a" 0
      a = TyVar aVar
      in TypeScheme [aVar] [Pred Ord a] (TyList a :~> TyList a)
    RawSortObject -> error "sort object TODO" -- TODO
    RawContains -> error "contains TODO" -- TODO
    RawRemove -> error "remove TODO" -- TODO
    where
    nd b a = NamedDeBruijn a b
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
    unaryInt = TypeScheme [] [] (TyInt :~> TyInt)
    -- integer -> integer -> integer
    binaryInt = TypeScheme [] [] (TyInt :~> TyInt :~> TyInt)
    -- decimal -> integer
    roundingFn = TypeScheme [] [] (TyDecimal :~> TyInt)
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
    RExpect -> let
      aVar = nd "a" 0
      aTv = TyVar aVar
      in TypeScheme [aVar] [Pred Eq aTv, Pred Show aTv] (TyString :~> aTv :~> (TyUnit :~> aTv) :~> TyString)
    RExpectFailure -> let
      aVar = nd "a" 0
      aTv = TyVar aVar
      in TypeScheme [aVar] [] (TyString :~> (TyUnit :~> aTv) :~> TyString)
    RExpectThat -> let
      aVar = nd "a" 0
      aTv = TyVar aVar
      in TypeScheme [aVar] [] (TyString :~> (aTv :~> TyBool) :~> aTv :~> TyString)
    RPrint -> let
      aVar = nd "a" 0
      aTv = TyVar aVar
      in TypeScheme [aVar] [Pred Show aTv] (aTv :~> TyUnit)
    where
    nd b a = NamedDeBruijn a b

liftST :: ST s a -> InferM s b i a
liftST action = InferT (ExceptT (Right <$> ReaderT (const action)))

throwTypecheckError :: TypecheckError -> i -> InferM s b i a
throwTypecheckError te i = throwError te

_dbgTypedTerm
  :: TCTerm s b i
  -> InferM s b i (Typed.Term Text Text (b, [Type Text], [Pred Text]) i)
_dbgTypedTerm = \case
  Typed.Var n i -> pure (Typed.Var (_nName n) i)
  Typed.Lam nel body i -> do
    nel' <- (traversed._2) _dbgType nel
    body' <- _dbgTypedTerm body
    pure (Typed.Lam nel' body' i)
  Typed.App fn body i ->
    Typed.App <$> _dbgTypedTerm fn <*> traverse _dbgTypedTerm body <*> pure i
  Typed.Let n e1 e2 i ->
    Typed.Let n <$> _dbgTypedTerm e1 <*> _dbgTypedTerm e2 <*> pure i
  Typed.Builtin (b, tys, preds) i -> do
    tys' <- traverse _dbgTCType tys
    preds' <- traverse _dbgTCPred preds
    pure (Typed.Builtin (b, tys', preds') i)
  Typed.Constant l i -> pure (Typed.Constant l i)
  Typed.TyApp t nelty i ->
    Typed.TyApp <$> _dbgTypedTerm t <*> traverse _dbgType nelty <*> pure i
  Typed.TyAbs tys term i -> do
    tys' <- traverse _dbgTvRef tys
    term' <- _dbgTypedTerm term
    pure (Typed.TyAbs tys' term' i)
  Typed.Sequence e1 e2 i ->
    Typed.Sequence <$> _dbgTypedTerm e1 <*> _dbgTypedTerm e2 <*> pure i
  Typed.Conditional c i ->
    Typed.Conditional <$> traverse _dbgTypedTerm c <*> pure i
  Typed.Try e1 e2 i ->
    Typed.Try <$> _dbgTypedTerm e1 <*> _dbgTypedTerm e2 <*> pure i
  Typed.Error t e i ->
    Typed.Error <$> _dbgType t <*> pure e <*> pure i
  Typed.DynInvoke n t i ->
    Typed.DynInvoke <$> _dbgTypedTerm n <*> pure t <*> pure i
  Typed.CapabilityForm cf i ->
    let cf' = over capFormName _nName cf
    in Typed.CapabilityForm <$> traverse _dbgTypedTerm cf' <*> pure i
  Typed.ListLit ty li i ->
    Typed.ListLit <$> _dbgType ty <*> traverse _dbgTypedTerm li <*> pure i
  -- Typed.ObjectLit obj i ->
  --   Typed.ObjectLit <$> traverse _dbgTypedTerm obj <*> pure i
  -- Typed.ObjectOp oop i ->
  --   Typed.ObjectOp <$> traverse _dbgTypedTerm oop <*> pure i

_dbgTypeScheme :: TypeScheme (TvRef s) -> InferM s b i (TypeScheme Text)
_dbgTypeScheme (TypeScheme tvs preds ty) = do
  tvs' <- traverse rv tvs
  preds' <- traverse _dbgPred preds
  ty' <- _dbgType ty
  pure (TypeScheme tvs' preds' ty')
  where
  rv n = readTvRef n >>= \case
    Unbound u l _ -> pure ("unbound" <> T.pack (show (u, l)))
    Bound u l -> pure ("bound" <> T.pack (show (u, l)))
    Link _ -> pure "linktv"

_dbgTvRef :: TvRef s -> InferM s b i Text
_dbgTvRef tv = readTvRef tv >>= \case
    Unbound u l _ -> pure ("unbound" <> T.pack (show (u, l)))
    Bound u l -> pure ("bound" <> T.pack (show (u, l)))
    Link ty -> do
      ty' <- _dbgType ty
      pure $ "linked type<" <> T.pack (show ty') <> ">"

_dbgTCPred :: TCPred s -> InferM s b i (Pred Text)
_dbgTCPred = error "dbgPred" -- TODO predicates

_dbgTCType :: TCType s -> InferM s b i (Type Text)
_dbgTCType = \case
  TyVar tv -> readTvRef tv >>= \case
    Unbound u l _ -> pure (TyVar ("unbound" <> T.pack (show (u, l))))
    Bound u l -> pure (TyVar ("bound" <> T.pack (show (u, l))))
    Link ty -> _dbgType ty
  TyFun l r -> TyFun <$> _dbgType l <*> _dbgType r
  TyList t -> TyList <$> _dbgType t
  TyPrim p -> pure (TyPrim p)
  TyModRef mr -> pure (TyModRef mr)
  TyForall {} -> error "impredicative"

_dbgPred :: Pred (TvRef s) -> InferM s b i (Pred Text)
_dbgPred (Pred b t) = Pred b <$> _dbgType t

_dbgType :: Type (TvRef s) -> InferM s b i (Type Text)
_dbgType = \case
  TyVar tv -> readTvRef tv >>= \case
    Unbound u l _ -> pure (TyVar ("unbound" <> T.pack (show (u, l))))
    Bound u l -> pure (TyVar ("bound" <> T.pack (show (u, l))))
    Link ty -> _dbgType ty
  TyFun l r -> TyFun <$> _dbgType l <*> _dbgType r
  TyList t -> TyList <$> _dbgType t
  TyPrim p -> pure (TyPrim p)
  TyModRef mr -> pure (TyModRef mr)
  TyForall {} -> error "impredicative"


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

readTvRef :: TvRef s -> InferM s b i (Tv s)
readTvRef (TvRef tv) = liftST (readSTRef tv)

writeTvRef :: TvRef s -> Tv s -> InferM s b i ()
writeTvRef (TvRef tv) t = liftST (writeSTRef tv t)

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

-- | For some typeclass C,
-- is there an instance of C t?
-- If so, return the qualifiers of the instance.
-- that is, for (C a_1, .., C a_n) => C t
-- byInst (C t) returns Just [C a_1, .., C a_n].
-- Note: if these were user defined, if we decide to extend to this
-- byInst would have to match the type of C (K t) to an instantiated version
-- of the qualified type (C a_1, .., C a_n) => C (K t_1) for type constructors
byInst :: Pred (TvRef s) -> InferM s b i (Maybe [Pred (TvRef s)])
byInst (Pred p ty) = case p of
  Eq -> eqInst ty
  Add -> addInst ty
  Num -> numInst ty
  Ord -> ordInst ty
  Show -> showInst ty
  ListLike -> listLikeInst ty
  Fractional -> fractionalInst ty

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
eqInst :: TCType s -> InferM s b i (Maybe [Pred (TvRef s)])
eqInst = \case
  TyVar tv -> readTvRef tv >>= \case
    Link ty -> eqInst ty
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
ordInst :: TCType s -> InferM s b i (Maybe [Pred (TvRef s)])
ordInst = \case
  TyVar tv -> readTvRef tv >>= \case
    Link ty -> ordInst ty
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


-- | Instances of Add:
--
--  instance Add integer
--  instance Add decimal
--  instance Add string
--  instance Add (list 'a)
--
--
addInst :: TCType s -> InferM s b i (Maybe [Pred (TvRef s)])
addInst = \case
  TyVar tv -> readTvRef tv >>= \case
    Link ty -> addInst ty
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
numInst :: TCType s -> InferM s b i (Maybe [Pred (TvRef s)])
numInst = \case
  TyVar tv -> readTvRef tv >>= \case
    Link ty -> numInst ty
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
fractionalInst :: TCType s -> InferM s b i (Maybe [Pred (TvRef s)])
fractionalInst = \case
  TyVar tv -> readTvRef tv >>= \case
    Link ty -> fractionalInst ty
    _ -> pure Nothing
  TyPrim p -> pure $ case p of
    PrimInt -> Just []
    PrimDecimal -> Just []
    _ -> Nothing
  _ -> pure Nothing

listLikeInst :: TCType s -> InferM s b i (Maybe [Pred (TvRef s)])
listLikeInst = \case
  TyVar tv -> readTvRef tv >>= \case
    Link ty -> listLikeInst ty
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
showInst :: TCType s -> InferM s b i (Maybe [Pred (TvRef s)])
showInst = \case
  TyVar tv -> readTvRef tv >>= \case
    Link ty -> showInst ty
    _ -> pure Nothing
  -- All prims have an EQ instance
  TyPrim _p -> pure (Just [])
  TyList t -> pure (Just [Pred Show t])
  _ -> pure Nothing

entail :: [Pred (TvRef s)] -> Pred (TvRef s) -> InferM s b i Bool
entail ps p = byInst p >>= \case
  Nothing -> pure False
  Just qs -> and <$> traverse (entail ps) qs

isHnf :: Pred (TvRef s) -> InferM s b i Bool
isHnf (Pred c t) = case t of
  TyVar tv -> readTvRef tv >>= \case
    Link ty -> isHnf (Pred c ty)
    _ -> pure True
  _ -> pure False

toHnf :: Pred (TvRef s) -> i -> InferM s b i [Pred (TvRef s)]
toHnf p i = isHnf p >>= \case
  True -> pure [p]
  False -> byInst p >>= \case
    Nothing -> do
      p' <- _dbgPred p
      throwTypecheckError (ContextReductionError p') i
    Just ps -> toHnfs ps i

toHnfs :: [Pred (TvRef s)] -> i -> InferM s b i [Pred (TvRef s)]
toHnfs ps i = do
  pss <- traverse (`toHnf` i) ps
  pure (concat pss)

simplify :: [Pred (TvRef s)] -> InferM s b i [Pred (TvRef s)]
simplify = loop []
  where
  loop rs [] = pure rs
  loop rs (p:ps) = entail (rs ++ ps) p >>= \cond ->
    if cond then loop rs ps else loop (p:rs) ps

reduce :: [Pred (TvRef s)]-> i -> InferM s b i [Pred (TvRef s)]
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
    Link ty -> hasUnbound ty
    Bound {} -> pure False
  hasUnbound = \case
    TyVar n -> varUnbound n
    TyPrim _ -> pure False
    TyList t -> hasUnbound t
    TyFun l r -> do
      l' <- hasUnbound l
      if l' then pure l' else hasUnbound r
    _ -> pure False

checkReducible :: [Pred (TvRef s)] -> i -> InferM s b i ()
checkReducible ps i =
  reduce ps i >>= \case
    [] -> pure ()
    xs -> do
      xs' <- traverse _dbgPred xs
      throwTypecheckError (UnsupportedTypeclassGeneralization xs') i

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
--       let tyapps = Typed.TyApp term (x:|xs) info
--       dvars <- traverse toDVar preds'
--       case dvars of
--         p:ps -> pure (ty', Typed.App tyapps (p:|ps) info, preds')
--         [] -> pure (ty', tyapps, [])
--     [] -> pure (ty', term, [])
--   where
--   info = term ^. Typed.termInfo
  -- toDVar p = do
  --   i <- newSupplyIx
  --   let n = OverloadedName ("_dict" <> T.pack (show i)) (OBuiltinDict p)
  --   pure $ Typed.Var n info
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
  :: TypeScheme NamedDeBruijn
  -> i
  -> InferM s b' i (TCType s, [TvRef s], [TCPred s])
instantiateImported (TypeScheme tvs preds ty) i = do
    ntvs <- traverse (const newTvRef) tvs
    let rl = RAList.fromList (reverse ntvs)
    ty' <- inst rl ty
    preds' <- traverse (instPred rl) preds
    pure (ty', ntvs, preds')
  where
  instPred rl (Pred tc pty) =
    Pred tc <$> inst rl pty
  inst rl = \case
    TyVar (NamedDeBruijn i' _) -> pure (TyVar (rl RAList.!! i'))
    TyPrim p -> pure (TyPrim p)
    TyFun l r -> TyFun <$> inst rl l <*> inst rl r
    TyList t -> TyList <$> inst rl t
    TyModRef mr -> pure (TyModRef mr)
    -- Impredicative type might work
    -- If we change unification.
    TyForall _ _ ->
      throwTypecheckError UnsupportedImpredicativity i

occurs
  :: TvRef s
  -> TCType s
  -> i
  -> InferM s b' i ()
occurs tv tct i = case tct of
  TyVar tv' | tv == tv' -> do
    tv'' <- _dbgType tct
    throwTypecheckError (OccursCheckFailure tv'') i
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
    Link ty -> occurs tv ty i
    _ -> pure ()

unifyTyVar
  :: TvRef s
  -> TCType s
  -> i
  -> InferM s b' i ()
unifyTyVar tv t1 i = readTvRef tv >>= \case
  Unbound{} -> do
    occurs tv t1 i
    writeTvRef tv (Link t1)
  Link t2 -> unify t2 t1 i
  _ -> pure ()

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
unify t1 t2 i = do
  t1' <- _dbgType t1
  t2' <- _dbgType t2
  throwTypecheckError (UnificationError t1' t2') i

-- | We essentially only
-- generalize on lambdas atm.
generalizeWithTerm
  :: TCType s
  -> [TCPred s]
  -> TCTerm s b i
  -> InferM s b' i (TypeScheme (TvRef s), TCTerm s b i, [TCPred s])
generalizeWithTerm ty pp term
  | isValue term = generalizeWithTerm' ty pp term
  | otherwise = do
    pp' <- reduce pp (view Typed.termInfo term)
    pure (TypeScheme [] [] ty, term, pp')
  where
  isValue = \case
    Typed.Var{} -> True
    Typed.Constant{} -> True
    Typed.Lam{} -> True
    Typed.Error{} -> True
    Typed.Builtin{} -> True
    _ -> False

-- Generalization that emits a typed term
-- Note: Deferred predicates are purely for the sake of
-- callsite dictionary overloaded variables.
-- These are currently disabled.
generalizeWithTerm'
  :: TCType s
  -> [TCPred s]
  -> TCTerm s b i
  -> InferM s b' i (TypeScheme (TvRef s), TCTerm s b i, [TCPred s])
generalizeWithTerm' ty pp term = do
  preds <- nubPreds pp
  ((ftvs, ty'), s) <- runStateT (gen' ty) Set.empty
  (deferred, retained) <- split preds (view Typed.termInfo term)
  retained' <- evalStateT (traverse genPred retained) s
  when (retained' /= []) $ do
    retained'' <- traverse _dbgPred retained
    throwTypecheckError (UnsupportedTypeclassGeneralization retained'') info
  case ftvs of
    [] -> do
      pure (TypeScheme [] [] ty' , term, deferred)
    (x:xs) -> do
      pure (TypeScheme ftvs [] ty', Typed.TyAbs (x:|xs) term info, deferred)
  where
  nubPreds li = nubPreds' li []
  -- we expect
  nubPreds' (p@(Pred tc x) : xs) elems = case x of
    TyVar rv -> readTvRef rv >>= \case
      Link tl -> nubPreds' (Pred tc tl :xs) elems
      _ ->
        if p `elem` elems
        then nubPreds' xs elems
        else nubPreds' xs (Pred tc x:elems)
    _ -> nubPreds' xs elems
  nubPreds' [] elems = pure (reverse elems)
  info = term ^. Typed.termInfo
  genPred (Pred t pty) = do
    (o, pty')  <- gen' pty
    when (o /= []) $
      lift (throwTypecheckError (TCInvariantFailure "Generalizing predicates") info)
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
    Link t' -> gen' t'
    Bound _ _ -> pure ([], TyVar tv)
  gen' (TyFun l r) = do
    (ftvl, l') <- gen' l
    (ftvr, r') <- gen' r
    pure (ftvl ++ ftvr,TyFun l' r')
  gen' t@TyPrim{} = pure ([], t)
  gen' (TyList t) = over _2 TyList <$> gen' t
  gen' (TyModRef mr) = pure ([], TyModRef mr)
  gen' t@TyForall{} = pure ([], t)

liftNoFreeVars :: Type Void -> Type a
liftNoFreeVars = \case
  TyVar n -> absurd n
  TyPrim prim -> TyPrim prim
  TyFun t1 t2 -> TyFun (liftNoFreeVars t1) (liftNoFreeVars t2)
  TyList t -> TyList (liftNoFreeVars t)
  TyModRef mn -> TyModRef mn
  TyForall vars t -> TyForall (absurd <$> vars) (liftNoFreeVars t)

liftType :: IR.Type -> Type a
liftType = \case
  IR.TyPrim prim -> TyPrim prim
  IR.TyList ty -> TyList $ liftType ty
  IR.TyModRef modName -> TyModRef modName
  IR.TyObject _schema -> error "TODO" -- TyObject schema
  IR.TyTable _schema -> error "TODO" -- TyTable schema

toTypedArg :: Arg ty -> TypedArg ty
toTypedArg (Arg n (Just ty)) = TypedArg n ty
toTypedArg (Arg _ Nothing) = error "toTypedArg TODO must have type"

unifyFunArgs
  :: Traversable f
  => [TCType s]
  -> f (Arg IR.Type)
  -> i
  -> InferM s b i ()
unifyFunArgs tys irArgs info
  | Just irTys <- traverse _argType irArgs = do
    when (length tys /= length irTys) $ error "Arguments mismatch"
    let zipped = zip (toList irTys) tys
    traverse_ (\(irTy, ty) -> unify (liftType irTy) ty info) zipped
  | otherwise = error "unspecified arg types"

unifyFun
  :: Traversable f
  => TCType s
  -> f (Arg IR.Type)
  -> Maybe IR.Type
  -> i
  -> InferM s b i ()
unifyFun funty irArgs (Just irRet) info = do
  unifyFunArgs tys irArgs info
  unify ret (liftType irRet) info
  where
    (tys, ret) = tyFunToArgList funty
unifyFun _ _ Nothing _ = error "unannotated return type"

getTopLevelDef
  :: MonadReader (TCEnv s b i) m
  => Text
  -> ModuleName
  -> ModuleHash
  -> m (Maybe (IR.Def Name IR.Type b i))
getTopLevelDef name modname modhash =
  view (tcLoaded . loAllLoaded . at (FullyQualifiedName modname name modhash))

checkTermType
  :: (TypeOfBuiltin b)
  => TCType s
  -> IRTerm b i
  -> InferM s b' i (TCType s, TCTerm s b i, [TCPred s])
checkTermType checkty = \case
  IR.Var irn@(Name n nk) i -> case nk of
    NBound u -> do
      views tcVarEnv (`RAList.lookup` u) >>= \case
        Just ty -> do
          let v' = Typed.Var irn i
          unify checkty ty i
          pure (ty, v', [])
        Nothing ->
          throwTypecheckError (TCUnboundTermVariable n) i
    NTopLevel mn mh ->
      getTopLevelDef n mn mh >>= \case
        Just (IR.Dfun df) -> do
          unifyFun checkty (IR._dfunArgs df) (IR._dfunRType df) i
          let rty = snd $ tyFunToArgList checkty
          pure (rty, Typed.Var irn i, [])
        _ -> throwTypecheckError (TCUnboundFreeVariable mn n) i
    NModRef _ ifs -> case checkty of
      TyModRef mn -> do
        let newVar = Typed.Var irn i
        if mn `elem` ifs then
          pure (TyModRef mn, newVar, [])
        else
          error "modref does not implement interface"
      TyVar tv -> case ifs of
        [iface] -> do
          unify (TyVar tv) (TyModRef iface) i
          pure (TyModRef iface, Typed.Var irn i, [])
        _ -> error "incorrect type"
      _ -> error "checking modref against incorrect type"
  IR.Lam _info irArgs te i -> do
    let (tl, ret) = tyFunToArgList checkty
    unifyFunArgs tl irArgs i
    let args = RAList.fromList $ reverse tl
    (_, te', preds) <- locally tcVarEnv (args RAList.++) $ checkTermType ret te
    let ne' = over _1 _argName <$> NE.zip irArgs (NE.fromList tl)
    pure (checkty, Typed.Lam ne' te' i, preds)
  IR.Let (Arg name mlty) e1 e2 i
    | Just lty <- mlty -> do
      (_, e1', pe1) <- checkTermType (liftType lty) e1
      (_, e2', pe2) <- locally tcVarEnv (RAList.cons (liftType lty)) $ checkTermType checkty e2
      let term' = Typed.Let name e1' e2' i
      pure (checkty, term', pe1 ++ pe2)
    | otherwise -> error "must have the type"
  IR.App te (h :| hs) i -> do
    (tapp, te', pe1) <- inferTerm te
    (rty, xs, ps) <- foldlM inferFunctionArgs (tapp, [], []) (h:hs)
    unify rty checkty i
    let term' = Typed.App te' (NE.fromList (reverse xs)) i
    pure (checkty, term', pe1 ++ ps)
    where
    inferFunctionArgs (ta, xs, ps) x = case ta of
      TyFun arg ret -> do
        (_, x', p) <- checkTermType arg x
        pure (ret, x':xs, ps ++ p)
      _ -> error "not a function"
  IR.Sequence l r i -> do
    (_, l', pl) <- inferTerm l
    (_, r', pr) <- checkTermType checkty r
    pure (checkty, Typed.Sequence l' r' i, pl ++ pr)
  IR.Conditional cond i -> over _2 (`Typed.Conditional` i) <$>
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
  IR.Builtin b i -> do
    let tyImported = typeOfBuiltin b
    (ty, tvs, preds) <- instantiateImported tyImported i
    unify checkty ty i
    let term' = Typed.Builtin (b, TyVar <$> tvs, preds) i
    pure (ty, term', preds)
  IR.CapabilityForm cf i -> over _2 (`Typed.CapabilityForm` i) <$> case cf of
    WithCapability na tes te -> do
      (tes', p1) <- checkCapArgs na tes
      (ty', te', p2) <- checkTermType checkty te
      pure (ty', WithCapability na tes' te', p1 ++ p2)
    RequireCapability na tes -> do
      unify checkty TyUnit i
      (tes', p1) <- checkCapArgs na tes
      pure (TyUnit, RequireCapability na tes', p1)
    ComposeCapability na tes -> do
      unify checkty TyUnit i
      (tes', p1) <- checkCapArgs na tes
      pure (TyUnit, ComposeCapability na tes', p1)
    InstallCapability na tes -> do
      unify checkty TyUnit i
      (tes', p1) <- checkCapArgs na tes
      pure (TyUnit, InstallCapability na tes', p1)
    EmitEvent na tes -> do
      unify checkty TyUnit i
      (tes', p1) <- checkCapArgs na tes
      pure (TyUnit, EmitEvent na tes', p1)
    -- TODO: Enforce `na` is a name of a dfun and not a dcap
    -- as a matter of fact, the whole above block needs the same enforcement just
    -- for dfuns
    CreateUserGuard na tes -> case _nKind na of
      NTopLevel modname modhash ->
        getTopLevelDef (_nName na) modname modhash >>= \case
          Just (IR.Dfun (IR.Defun _name mirArgs mrty _term _info))
            | Just rty <- mrty
            , Just irArgs <- traverse _argType mirArgs -> do
              let args = liftType <$> irArgs
              unify (liftType rty) TyUnit i
              when (length args /= length tes) $ error "invariant broken"
              vs <- zipWithM checkTermType args tes
              let tes' = view _2 <$> vs
              pure (TyGuard, CreateUserGuard na tes', concatMap (view _3) vs)
            | otherwise -> error "unannotated types"
          _ -> error "boom"
      _ -> error "invariant broken, must refer to a top level name"

      -- unify checkty TyGuard i
      -- (tes', p1) <- checkCapArgs na tes
      -- pure (TyGuard, CreateUserGuard na tes', p1)
  IR.Constant lit i -> do
    let ty = typeOfLit lit
    unify checkty ty i
    pure (ty, Typed.Constant lit i, [])
  IR.ListLit tes i -> case checkty of
    TyList ty -> do
      liTup <- traverse (checkTermType ty) tes
      let preds = concatMap (view _3) liTup
          term' = Typed.ListLit ty (view _2 <$> liTup) i
      pure (TyList ty, term', preds)
    _ -> do
      tup <- inferTerm (IR.ListLit tes i)
      unify (view _1 tup) checkty i
      pure tup
  IR.Try errcase bodycase i -> do
    (_, err', p1) <- checkTermType checkty errcase
    (_, body', p2) <- checkTermType checkty bodycase
    pure (checkty, Typed.Try err' body' i, p1 ++ p2)
  IR.DynInvoke mref fn i -> do
    (tmref, mref', preds) <- inferTerm mref
    case tmref of
      TyModRef m -> view (tcModules . at m) >>= \case
        Just (InterfaceData iface _) -> case IR.findIfDef fn iface of
          Just (IR.IfDfun (IR.IfDefun _name irArgs irMRet _info)) -> do
            unifyFun checkty irArgs irMRet i
            pure (checkty, Typed.DynInvoke mref' fn i, preds)
          _ -> error "boom"
        _ -> error "boom"
      _ -> error "boom"
  IR.Error txt i -> pure (checkty, Typed.Error checkty txt i, [])
  IR.ObjectLit{} -> error "TODO" -- TODO new ctor


checkCapArgs
  :: TypeOfBuiltin raw
  => Name
  -> [IRTerm raw i]
  -> InferM s reso i ([TCTerm s raw i], [TCPred s])
checkCapArgs na tes = case _nKind na of
  NTopLevel mn mh ->
    getTopLevelDef (_nName na) mn mh >>= \case
      Just (IR.DCap dc)
        | Just dcargs <- traverse IR._argType $ IR._dcapArgs dc -> do
          when (length dcargs /= length tes) $ error "invariant broken dcap args"
          vs <- zipWithM (checkTermType . liftType) dcargs tes
          pure (view _2 <$> vs, concat (view _3 <$> vs))
        | otherwise -> error "unannotated types"
      _ -> error "invariant broken"
  _ -> error "invariant broken"

irFunToTc
  :: [Arg IR.Type]
  -> Maybe IR.Type
  -> InferM s b i ([Type a], Type a)
irFunToTc irMArgs (Just irRet)
  | Just irArgs <- traverse IR._argType irMArgs = do
    pure (liftType <$> irArgs, liftType irRet)
  | otherwise = error "unannotated arguments"
irFunToTc _ Nothing = error "unannotated return type"

-- Todo: bidirectionality
inferTerm
  :: (TypeOfBuiltin b)
  => IRTerm b i
  -> InferM s b' i (TCType s, TCTerm s b i, [TCPred s])
inferTerm = \case
  IR.Var irn@(Name n nk) i -> case nk of
    NBound u -> do
      views tcVarEnv (`RAList.lookup` u) >>= \case
        Just ty -> do
          let v' = Typed.Var irn i
          pure (ty, v', [])
        Nothing ->
          throwTypecheckError (TCUnboundTermVariable n) i
    NTopLevel mn mh ->
      getTopLevelDef n mn mh >>= \case
        Just (IR.Dfun df) -> do
          (args, ret) <- irFunToTc (IR._dfunArgs df) (IR._dfunRType df)
          let newVar = Typed.Var irn i
          pure (argListToTyFun args ret, newVar, [])
        _ -> throwTypecheckError (TCUnboundFreeVariable mn n) i
    NModRef _ ifs -> case ifs of
      [iface] -> do
        let v' = Typed.Var irn i
        pure (TyModRef iface, v', [])
      [] -> error "Module reference does not implement any interfaces"
      _ -> error "Cannot infer module reference "
  IR.Lam _info nts e i -> do
    let names = _argName <$> nts
    ntys <- traverse withTypeInfo nts
    -- Todo: bidirectionality
    -- let m = IntMap.fromList $ NE.toList $ NE.zipWith (\n t ->  (_irUnique n, t)) names ntys
    let m = RAList.fromList (reverse (NE.toList ntys))
    (ty, e', preds) <- locally tcVarEnv (m RAList.++) $ inferTerm e
    let nts' = NE.zip names ntys
        rty = foldr TyFun ty ntys
    pure (rty, Typed.Lam nts' e' i, preds)
    where
    withTypeInfo p = case _argType p of
      Just ty -> pure (liftType ty)
      Nothing -> TyVar <$> newTvRef
  IR.App te (h :| hs) i -> do
    (tapp, te', pe1) <- inferTerm te
    (rty, xs, ps) <- foldlM inferFunctionArgs (tapp, [], []) (h:hs)
    let term' = Typed.App te' (NE.fromList (reverse xs)) i
    pure (rty, term', pe1 ++ ps)
    where
    inferFunctionArgs (ta, xs, ps) x = case ta of
      TyFun arg ret -> do
        (_, x', p) <- checkTermType arg x
        pure (ret, x':xs, ps ++ p)
      _ -> error "not a function"
  --   tv1 <- TyVar <$> newTvRef
  --   (te, e', pte) <- inferTerm e
  --   as <- traverse inferTerm args
  --   let tys = view _1 <$> as
  --       args' = view _2 <$> as
  --       preds' = concat (pte : NE.toList (view _3 <$> as))
  --   unify te (foldr TyFun tv1 tys) i
  --   pure (tv1, Typed.App e' args' i, preds')
  IR.Let (Arg name mlty) e1 e2 i
    | Just lty <- mlty -> do
      enterLevel
      (te1, e1', pe1) <- checkTermType (liftType lty) e1
      leaveLevel
      -- Note: generalization is turned off.
      -- (ts, e1Qual, deferred) <- generalizeWithTerm te1 pe1 e1Unqual
      (te2, e2', pe2) <- locally tcVarEnv (RAList.cons te1) $ inferTerm e2
      pure (te2, Typed.Let name e1' e2' i, pe1 ++ pe2)
    | otherwise -> error "must have the type annotated here"
  IR.Sequence e1 e2 i -> do
    (_, e1', pe1) <- inferTerm e1
    (te2, e2', pe2) <- inferTerm e2
    pure (te2, Typed.Sequence e1' e2' i, pe1 ++ pe2)
  -- Todo: Here, convert to dictionary
  IR.CapabilityForm cf i -> over _2 (`Typed.CapabilityForm` i) <$> case cf of
    WithCapability na tes te -> do
      (tes', p1) <- checkCapArgs na tes
      (ty', te', p2) <- inferTerm te
      pure (ty', WithCapability na tes' te', p1 ++ p2)
    RequireCapability na tes -> do
      (tes', p1) <- checkCapArgs na tes
      pure (TyUnit, RequireCapability na tes', p1)
    ComposeCapability na tes -> do
      (tes', p1) <- checkCapArgs na tes
      pure (TyUnit, ComposeCapability na tes', p1)
    InstallCapability na tes -> do
      (tes', p1) <- checkCapArgs na tes
      pure (TyUnit, InstallCapability na tes', p1)
    EmitEvent na tes -> do
      (tes', p1) <- checkCapArgs na tes
      pure (TyUnit, EmitEvent na tes', p1)
    CreateUserGuard na tes -> do
      (tes', p1) <- checkCapArgs na tes
      pure (TyGuard, CreateUserGuard na tes', p1)

  IR.Conditional cond i -> over _2 (`Typed.Conditional` i) <$>
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
  IR.Builtin b i -> do
    let tyImported = typeOfBuiltin b
    (ty, tvs, preds) <- instantiateImported tyImported i
    let tvs' = TyVar <$> tvs
    let term' = Typed.Builtin (b, tvs', preds) i
    pure (ty, term', preds)
  -- TODO: note,
  -- for this to work, we have to have proper bidirectionality working, including scoped type variables working fine
  IR.Constant l i ->
    pure (typeOfLit l, Typed.Constant l i,[])
  -- -- note: object literals are closed rows.
  -- IR.ObjectLit _obj _i -> undefined
  -- -- Todo: comment this case out better.
  -- IR.ObjectOp _oop _i -> undefined
  IR.ListLit li i -> do
    tv <- TyVar <$> newTvRef
    liTup <- traverse inferTerm li
    let preds = concatMap (view _3) liTup
    traverse_ (\(t,_, _) -> unify tv t i) liTup
    pure (TyList tv, Typed.ListLit tv (view _2 <$> liTup) i, preds)
  IR.Try e1 e2 i -> do
    (te1, e1', p1) <- inferTerm e1
    (te2, e2', p2)<- inferTerm e2
    unify te1 te2 i
    pure (te1, Typed.Try e1' e2' i, p1 ++ p2)
  IR.DynInvoke mref fn i -> do
    (tmref, mref', preds) <- inferTerm mref
    case tmref of
      TyModRef m -> view (tcModules . at m) >>= \case
        Just (InterfaceData iface _) -> case IR.findIfDef fn iface of
          Just (IR.IfDfun df) -> do
            (args, ret) <- irFunToTc (IR._ifdArgs df) (IR._ifdRType df)
            pure (argListToTyFun args ret, Typed.DynInvoke mref' fn i, preds)
          _ -> error "boom"
        _ -> error "boom"
      _ -> error "boom"
  IR.Error e i -> do
    ty <- TyVar <$> newTvRef
    pure (ty, Typed.Error ty e i, [])
  IR.ObjectLit{} -> error "inferTerm TODO" -- TODO new ctor

toTypedArgs :: [Arg ty] -> [Type Void] -> [TypedArg (Type a)]
toTypedArgs = zipWith (\irArg ty -> TypedArg (IR._argName irArg) (liftNoFreeVars ty))

-- Todo: generic types?
-- We can't generalize yet since
-- we're not allowing type schemes just yet.
inferDefun
  :: TypeOfBuiltin b
  => IR.Defun Name IRType b i
  -> InferM s b' i (TypedDefun b i)
inferDefun (IR.Defun name dfargs dfRetType term info) = do
  enterLevel
  (argTys, ret) <- irFunToTc dfargs dfRetType
  let args = toTypedArgs dfargs argTys
  (termTy, term', preds) <- inferTerm term
  leaveLevel
  checkReducible preds (view IR.termInfo term)
  -- fail "typeclass constraints not supported in defun"
  unify (liftNoFreeVars $ argListToTyFun argTys ret) termTy info
  fterm <- noTyVarsinTerm info term'
  pure (Typed.Defun name args (liftNoFreeVars ret) fterm info)

inferDefConst
  :: TypeOfBuiltin b
  => IR.DefConst Name IRType b i
  -> InferM s b' i (TypedDefConst b i)
inferDefConst (IR.DefConst name dcTy term info) = do
  enterLevel
  (termTy, term', preds) <- inferTerm term
  leaveLevel
  checkReducible preds info
  fterm <- noTyVarsinTerm info term'
  let dcTy' = liftType <$> dcTy
  _ <- maybe (pure ()) (\dct -> unify dct termTy info) dcTy'
  rty' <- ensureNoTyVars info (fromMaybe termTy dcTy')
  pure (Typed.DefConst name rty' fterm info)

inferDefCap
  :: TypeOfBuiltin b
  => IR.DefCap Name IRType b i
  -> InferM s b' i (TypedDefCap b i)
inferDefCap (IR.DefCap name arity dcargs dcRetType term meta i) = do
  (argtys, rty) <- irFunToTc dcargs dcRetType
  let ty = liftNoFreeVars $ argListToTyFun argtys rty
      args = toTypedArgs dcargs argtys
  (termTy, term', preds) <- checkTermType ty term
  checkReducible preds i
  unify ty termTy i
  fterm <- noTyVarsinTerm i term'
  pure (Typed.DefCap name arity args (liftNoFreeVars rty) fterm meta i)

inferDef
  :: TypeOfBuiltin b
  => IR.Def Name IRType b i
  -> InferM s b' i (TypedDef b i)
inferDef = \case
  IR.Dfun d -> Typed.Dfun <$> inferDefun d
  IR.DConst d -> Typed.DConst <$> inferDefConst d
  IR.DCap dc -> Typed.DCap <$> inferDefCap dc

inferIfDef
  :: TypeOfBuiltin b
  => IR.IfDef Name IRType b i
  -> InferM s b' i (TypedIfDef b i)
inferIfDef = \case
  IR.IfDfun ifd -> do
    let irArgs = IR._ifdArgs ifd
    (argtys, rty) <- irFunToTc irArgs (IR._ifdRType ifd)
    let args = toTypedArgs irArgs argtys
    pure (Typed.IfDfun (Typed.IfDefun (IR._ifdName ifd) args rty (IR._ifdInfo ifd)))
  IR.IfDConst dc ->
    Typed.IfDConst <$> inferDefConst dc
  IR.IfDCap (IR.IfDefCap n irArgs irRty i) -> do
    (argtys, rty) <- irFunToTc irArgs irRty
    pure $ Typed.IfDCap (Typed.IfDefCap n argtys rty i)

inferModule
  :: TypeOfBuiltin b
  => IR.Module Name IRType b i
  -> InferM s b' i (TypedModule b i)
inferModule (IR.Module mname mgov defs blessed imports impl mh info) = do
  fv <- view (tcLoaded . loAllLoaded)
  (defs', _) <- foldlM infer' ([], fv) defs
  pure (Typed.Module mname mgov (reverse defs') blessed imports impl mh info)
  where
  infer' (xs, m) d = do
    def' <- local (set (tcLoaded . loAllLoaded) m) (inferDef d)
    let name' = FullyQualifiedName mname (Typed.defName def') mh
        dty = fmap absurd (Typed.defType def')
        m' = Map.insert name' dty  m
    pure (def':xs, m')

inferInterface
  :: TypeOfBuiltin b
  => IRInterface b info
  -> InferM s b' info (TypedInterface b info)
inferInterface (IR.Interface n defns h info) = do
  defns' <- traverse inferIfDef defns
  pure (Typed.Interface n defns' h info)

-- | Note: debruijnizeType will
-- ensure that terms that are generic will fail
inferTermNonGen
  :: TypeOfBuiltin b
  => IRTerm b i
  -> InferM s b' i (TypeScheme NamedDeBruijn, TypedTerm b i)
inferTermNonGen t = do
  (ty, t', preds) <- inferTerm t
  checkReducible preds (view IR.termInfo t)
  tys <- ensureNoTyVars (view IR.termInfo t) ty
  tt <- noTyVarsinTerm (view IR.termInfo t) t'
  pure (TypeScheme [] [] tys, tt)

inferTermGen
  :: TypeOfBuiltin b
  => IRTerm b i
  -> InferM s b' i (TypeScheme NamedDeBruijn, TypedGenTerm b i)
inferTermGen term = do
  let info = view IR.termInfo term
  enterLevel
  (ty, term0 , preds) <- inferTerm term
  leaveLevel
  (tys', typedTerm, deferred) <- generalizeWithTerm ty preds term0
  unless (null deferred) $ do
      deferred' <- traverse _dbgPred deferred
      throwTypecheckError (UnsupportedTypeclassGeneralization deferred') (view IR.termInfo term)
  dbjTyScheme <- debruijnizeTypeScheme info tys'
  dbjTerm <- debruijnizeTermTypes info typedTerm
  pure (dbjTyScheme, dbjTerm)

inferTopLevel
  :: TypeOfBuiltin b
  => Loaded reso i
  -> IR.TopLevel Name IRType b i
  -> InferM s reso i (TypedTopLevel b i, Loaded reso i)
inferTopLevel loaded = \case
  IR.TLModule m -> do
    tcm <- inferModule m
    let toFqn df = FullyQualifiedName (Typed._mName tcm) (Typed.defName df) (Typed._mHash tcm)
        newTLs = Map.fromList $ (\df -> (toFqn df, Typed.defType df)) <$> Typed._mDefs tcm
        loaded' = over loAllTyped (Map.union newTLs) loaded
    pure (Typed.TLModule tcm, loaded')
  IR.TLTerm m -> (, loaded) . Typed.TLTerm . snd <$> inferTermNonGen m
  IR.TLInterface i -> do
    tci <- inferInterface i
    let toFqn dc = FullyQualifiedName (Typed._ifName tci) (Typed._dcName dc) (Typed._ifHash tci)
        newTLs = Map.fromList $ fmap (\df -> (toFqn df, DefunType (Typed._dcType df))) $ mapMaybe (preview Typed._IfDConst) (Typed._ifDefns tci)
        loaded' = over loAllTyped (Map.union newTLs) loaded
    pure (Typed.TLInterface tci, loaded')

inferReplTopLevel
  :: TypeOfBuiltin b
  => Loaded reso i
  -> IR.ReplTopLevel Name IRType b i
  -> InferM s reso i (TypedReplTopLevel b i)
inferReplTopLevel loaded = \case
  IR.RTLModule m ->  do
    tcm <- inferModule m
    let toFqn df = FullyQualifiedName (Typed._mName tcm) (Typed.defName df) (Typed._mHash tcm)
        newTLs = Map.fromList $ (\df -> (toFqn df, Typed.defType df)) <$> Typed._mDefs tcm
        loaded' = over loAllTyped (Map.union newTLs) loaded
    pure (Typed.RTLModule tcm)
  IR.RTLTerm m -> Typed.RTLTerm . snd <$> inferTermNonGen m
  -- Todo: if we don't update the module hash to update linking,
  -- repl defuns and defconsts will break invariants about
  IR.RTLDefun dfn -> do
    dfn' <- inferDefun dfn
    let newFqn = FullyQualifiedName replModuleName (Typed._dfunName dfn') replModuleHash
    pure (Typed.RTLDefun dfn')
  IR.RTLDefConst dconst -> do
    dc <- inferDefConst dconst
    let newFqn = FullyQualifiedName replModuleName (Typed._dcName dc) replModuleHash
    pure (Typed.RTLDefConst dc)
  IR.RTLInterface i -> do
    tci <- inferInterface i
    let toFqn dc = FullyQualifiedName (Typed._ifName tci) (Typed._dcName dc) (Typed._ifHash tci)
        newTLs = Map.fromList $ fmap (\df -> (toFqn df, DefunType (Typed._dcType df))) $ mapMaybe (preview Typed._IfDConst) (Typed._ifDefns tci)
    pure (Typed.RTLInterface tci)


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
  -> InferM s b' i (Typed.Term Name NamedDeBruijn (b, [Type NamedDeBruijn], [Pred NamedDeBruijn]) i)
debruijnizeTermTypes info = dbj [] 0
  where
  dbj
    :: [(TvRef s, NamedDeBruijn)]
    -> DeBruijn
    -> TCTerm s b i
    -> InferM s b' i (Typed.Term Name NamedDeBruijn (b, [Type NamedDeBruijn], [Pred NamedDeBruijn]) i)
  dbj env depth = \case
    Typed.Var n i ->
      pure (Typed.Var n i)
    Typed.Lam nts e i -> do
      nts' <- (traversed._2) (dbjTyp info env depth) nts
      e' <- dbj env depth e
      pure (Typed.Lam nts' e' i)
    Typed.App l r i ->
      Typed.App <$> dbj env depth l <*> traverse (dbj env depth) r <*> pure i
    Typed.Let n e1 e2 i -> do
      e1' <- dbj env depth e1
      e2' <- dbj env depth e2
      pure (Typed.Let n e1' e2' i)
    Typed.TyAbs ntys e i -> do
      let len = fromIntegral (NE.length ntys)
          ixs = NE.fromList [depth .. depth + len - 1]
      names <- traverse (nameTvs info (depth + len)) (NE.zip ntys ixs)
      let env' = NE.toList $ NE.zip ntys names
      Typed.TyAbs names <$> dbj (env' ++ env) (depth + len) e <*> pure i
    Typed.TyApp e args i -> do
      e' <- dbj env depth e
      args' <- traverse (dbjTyp info env depth) args
      pure (Typed.TyApp e' args' i)
    Typed.Sequence e1 e2 i ->
      Typed.Sequence <$> dbj env depth e1 <*> dbj env depth e2 <*> pure i
    Typed.Conditional c i ->
      Typed.Conditional <$> traverse (dbj env depth) c <*> pure i
    Typed.Try e1 e2 i ->
      Typed.Try <$> dbj env depth e1 <*> dbj env depth e2 <*> pure i
    Typed.Error t e i -> do
      ty <- dbjTyp info env depth t
      pure (Typed.Error ty e i)
    Typed.ListLit ty v i ->
      Typed.ListLit <$> dbjTyp info env depth ty <*> traverse (dbj env depth) v <*> pure i
    Typed.DynInvoke n t i ->
      Typed.DynInvoke <$> dbj env depth n <*> pure t <*> pure i
    Typed.Builtin (b, tys, preds) i -> do
      tys' <- traverse (dbjTyp info env depth) tys
      preds' <- traverse (dbjPred info env depth) preds
      pure (Typed.Builtin (b, tys', preds') i)
    Typed.CapabilityForm cf i ->
      Typed.CapabilityForm <$> traverse (dbj env depth) cf <*> pure i
    Typed.Constant l i -> pure (Typed.Constant l i)


nameTvs
  :: i
  -> DeBruijn
  -> (TvRef s, DeBruijn)
  -> InferM s b i NamedDeBruijn
nameTvs info depth (nt, i) = readTvRef nt >>= \case
  Bound n _ ->
    pure (NamedDeBruijn (depth - i - 1) n)
  _ ->
    throwTypecheckError (TCInvariantFailure "Found unbound variable during generalization") info

ensureNoTyVars
  :: i
  -> TCType s
  -> InferM s b i (Type a)
ensureNoTyVars i = \case
  TyVar n -> readTvRef n >>= \case
    Link ty -> ensureNoTyVars i ty
    _ ->
      throwTypecheckError (DisabledGeneralization "Inferred generic signature") i
  TyPrim p -> pure (TyPrim p)
  TyFun l r -> TyFun <$> ensureNoTyVars i l <*> ensureNoTyVars i r
  TyList l -> TyList <$> ensureNoTyVars i l
  TyModRef mr -> pure (TyModRef mr)
  TyForall _ _ ->
    throwTypecheckError (TCInvariantFailure "Encountered universal quantification emitted by the typechecker. Impossible") i

ensureNoTyVarsPred
  :: i
  -> TCPred s
  -> InferM s b i (Pred NamedDeBruijn)
ensureNoTyVarsPred i (Pred tc ty) = Pred tc <$> ensureNoTyVars i ty

-- TODO here and in ensure* functions,
-- is it really needed, or can we do the same trick as with `Type Void`?
noTyVarsinTerm
  :: i
  -> TCTerm s b' i
  -> InferM s b i (TypedTerm b' i)
noTyVarsinTerm info = \case
  Typed.Var n i ->
    pure (Typed.Var n i)
  Typed.Lam nts e i ->
    Typed.Lam <$> (traversed._2) (ensureNoTyVars info) nts <*> noTyVarsinTerm info e <*> pure i
  Typed.App e args i ->
    Typed.App <$> noTyVarsinTerm info e <*> traverse (noTyVarsinTerm info) args <*> pure i
  Typed.Let n e1 e2 i ->
    Typed.Let n <$> noTyVarsinTerm info e1 <*> noTyVarsinTerm info e2 <*> pure i
  Typed.Builtin (b, ty, p) i -> do
    ty' <- traverse (ensureNoTyVars info) ty
    p' <- traverse (ensureNoTyVarsPred info) p
    pure $ Typed.Builtin (b, ty', p') i
  Typed.TyAbs _ns _e _i ->
    throwTypecheckError (DisabledGeneralization "Generic terms are disabled") info
  Typed.Constant l i ->
    pure (Typed.Constant l i)
  Typed.TyApp l tys i ->
    Typed.TyApp
      <$> noTyVarsinTerm info l
      <*> traverse (ensureNoTyVars info) tys
      <*> pure i
  Typed.Sequence e1 e2 i ->
    Typed.Sequence <$> noTyVarsinTerm info e1 <*> noTyVarsinTerm info e2 <*> pure i
  Typed.Conditional c i ->
    Typed.Conditional <$> traverse (noTyVarsinTerm info) c <*> pure i
  Typed.ListLit ty li i ->
    Typed.ListLit <$> ensureNoTyVars info ty <*> traverse (noTyVarsinTerm info) li <*> pure i
  Typed.Try e1 e2 i ->
    Typed.Try <$> noTyVarsinTerm info e1 <*> noTyVarsinTerm info e2 <*> pure i
  Typed.DynInvoke e1 t i ->
    Typed.DynInvoke <$> noTyVarsinTerm info e1 <*> pure t <*> pure i
  Typed.CapabilityForm cf i ->
    Typed.CapabilityForm <$> traverse (noTyVarsinTerm info) cf <*> pure i
  Typed.Error t e i ->
    Typed.Error <$> ensureNoTyVars info t <*> pure e <*> pure i

-- dbjName
--   :: [(TvRef s, NamedDeBruijn)]
--   -> DeBruijn
--   -> OverloadedName (TCPred s)
--   -> InferM s b i (OverloadedName (Pred NamedDeBruijn))
-- dbjName env depth (OverloadedName n kind) = fmap (OverloadedName n) $ case kind of
--   OBound b -> pure (OBound b)
--   OTopLevel m h -> pure (OTopLevel m h)
--   OBuiltinDict b -> OBuiltinDict <$> dbjPred env depth b

debruijnizeTypeScheme
  :: i
  -> TypeScheme (TvRef s)
  -> InferM s b i (TypeScheme NamedDeBruijn)
debruijnizeTypeScheme i (TypeScheme tvs preds t) = do
    let len = fromIntegral (length tvs)
    let ixs = [0.. len - 1]
    names <- traverse (nameTvs i len) (zip tvs ixs)
    let env = zip tvs names
    t' <- dbjTyp i env len t
    preds' <- traverse (dbjPred i env len) preds
    pure (TypeScheme names preds' t')

debruijnizeType
  :: i
  -> TCType s
  -> InferM s b i (Type NamedDeBruijn)
debruijnizeType i = dbjTyp i [] 0

dbjPred
  :: i
  -> [(TvRef s, NamedDeBruijn)]
  -> DeBruijn
  -> TCPred s
  -> InferM s b i (Pred NamedDeBruijn)
dbjPred i env depth (Pred tc ty) =
  Pred tc <$> dbjTyp i env depth ty

dbjTyp
  :: i
  -> [(TvRef s, NamedDeBruijn)]
  -> DeBruijn
  -> TCType s
  -> InferM s b i (Type NamedDeBruijn)
dbjTyp i env depth = \case
  TyVar n -> case lookup n env of
    Just v -> pure (TyVar v)
    Nothing -> readTvRef n >>= \case
      Unbound {} ->
        throwTypecheckError (TCInvariantFailure "Found unbound type variable after type checking") i
      Bound{} ->
        throwTypecheckError (TCInvariantFailure "Found bound variable outside of the calculated set") i
      Link ty -> dbjTyp i env depth ty
  TyPrim p -> pure (TyPrim p)
  TyFun l r -> TyFun <$> dbjTyp i env depth l <*> dbjTyp i env depth r
  TyList l -> TyList <$> dbjTyp i env depth l
  TyModRef m -> pure (TyModRef m)
  TyForall{} ->
    throwTypecheckError (TCInvariantFailure "Found impredicative Type") i

-- -----------------------------------------
-- --- Built-in type wiring
-- ------------------------------------------

-- mkFree :: Loaded builtin info -> Map ModuleName (Map Text (Type Void))
-- mkFree loaded = let
--   tl = _loModules loaded
--   toTy d = (Untyped.defName d, Untyped.defType d)
--   mdefs =  Untyped._mDefs . _mdModule <$> tl
--   in Map.fromList . fmap toTy <$> mdefs

runInfer
  :: Loaded b i
  -> InferM s b i a
  -> ST s (Either TypecheckError a)
runInfer loaded (InferT act) = do
  uref <- newSTRef 0
  lref <- newSTRef 1
  let tcs = TCState uref mempty loaded lref (_loModules loaded)
  runReaderT (runExceptT act) tcs

runInferTerm
  :: TypeOfBuiltin b
  => Loaded b' i
  -> IRTerm b i
  -> Either TypecheckError (TypeScheme NamedDeBruijn, TypedGenTerm b i)
runInferTerm loaded term0 = runST $
  runInfer loaded $ inferTermGen term0

runInferTermNonGen
  :: TypeOfBuiltin b
  => Loaded b' i
  -> IRTerm b i
  -> Either TypecheckError (TypeScheme NamedDeBruijn, TypedTerm b i)
runInferTermNonGen loaded term0 = runST $
  runInfer loaded $ inferTermNonGen term0

runInferModule
  :: TypeOfBuiltin b
  => Loaded b' i
  -> IRModule b i
  -> Either TypecheckError (TypedModule b i)
runInferModule loaded term0 =
  runST $ runInfer loaded (inferModule term0)

runInferTopLevel
  :: TypeOfBuiltin b
  => Loaded reso i
  -> IR.TopLevel Name IRType b i
  -> Either TypecheckError (TypedTopLevel b i, Loaded reso i)
runInferTopLevel l tl =
  runST $ runInfer l (inferTopLevel l tl)


runInferReplTopLevel
  :: TypeOfBuiltin b
  => Loaded reso i
  -> IR.ReplTopLevel Name IRType b i
  -> Either TypecheckError (TypedReplTopLevel b i, Loaded reso i)
runInferReplTopLevel l tl =
  runST $ runInfer l (inferReplTopLevel l tl)
