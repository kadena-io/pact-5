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
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}


-- |
-- Module      :  Pact.Core.IR.Typecheck
-- Copyright   :  (C) 2022 Kadena
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jose Cardona <jose@kadena.io>
--
-- HM type inference for Pact 5 IR.
--
module Pact.Core.Typed.Infer
  ( typecheckModule
  , TypecheckError(..)
  , renderTypecheckError
  )
  where


import Control.Lens hiding (Level)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.ST
import Control.Monad.State.Strict
import Control.Monad.Except
import Data.Void
import Data.RAList(RAList)
import Data.List (nubBy)
import Data.Foldable(traverse_, foldlM)
import Data.Default
import Data.STRef
import Data.Graph
import Data.Map(Map)
import Data.Text(Text)
import Data.List.NonEmpty(NonEmpty(..))

import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IntMap
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
import Pact.Core.Info
import Pact.Core.Pretty

import qualified Pact.Core.IR.Term as IR
import qualified Pact.Core.Type as IR
import qualified Pact.Core.Typed.Term as Typed
import Pact.Core.Environment
import Pact.Core.Capabilities
import Pact.Core.Repl.Utils

-- [Inference]
-- inference based on https://okmij.org/ftp/ML/generalization.html
--
-- Note for future self (and other maintaners):
-- Type inference levels in the types
-- a-la sound-lazy might be worth implementing later on.
-- The eager implementation is simpler to maintain and extend to typeclasses.

-- | Our generator of uniques, which is simply an incremental counter.
-- This doesn't have to be anythign more than this, as each call to `typecheck`
-- generates a new one
type UniqueSupply s = STRef s Unique

-- Our current generalization level (or in other words, depth)
type Level = Int

-- Aliases for convenience, since basically everything uses `TCTypeVar`
type TCTypeVar s = TypeVar (TvRef s)
type TCType s = Type (TCTypeVar s)
type TCPred i s = Pred i (TCTypeVar s)
type TCRowCtor s = RowTy (TCTypeVar s)
type TCRoseRow s = RoseRow (TCTypeVar s)

-- | Our type for mutable type variable cases.
--   In other words, every unification variable generated starts out as a mutable ref to an `Unbound` and unification
--   can update that reference to any of the `Link` types, as long as the corresponding `Link` update is well kinded.
--
--   A `Tv s` always gets created via `newTypeVar` as `Unbound` within a `TvRef s`
data Tv s
  = Unbound !Text !Unique !Level
  | Bound !Text !Unique
  | LinkTy !(TCType s)
  | LinkRow !(RowTy (TCTypeVar s))
  | LinkRef !(MRef (TCTypeVar s))
  deriving (Eq)

-- | The type of our mutable type variables. All new type variables generated in `newTypeVar` are
--   TvRef
-- Note: TyVar equality
-- is reference equality
newtype TvRef s =
  TvRef (STRef s (Tv s))
  deriving (Eq)

-- | Our "step context", in other words, are we typechecking a defun or defcap,
--   or are we typechecking a particular defpact step?
data StepContext
  = NotInStep
  | StepIndex Int
  deriving (Eq, Show, Ord)

-- | Our typechecking "environment" which tracks a few things:
--   - locally bound variables (debruijn style, since that's what core IR uses)
--   - A supply of uniques for use during inference
--   - The current generalization "level"
data TCEnv s b i
  = TCEnv
  { _tcSupply :: UniqueSupply s
  -- ^ Supply for fresh variables.
  , _tcVarEnv :: RAList (TypeScheme (TCTypeVar s))
  -- ^ Builtins map, that uses the enum instance
  , _tcLevel :: STRef s Level
  -- ^ our mem region "level"
  , _tcStepCtx :: StepContext
  -- ^ The current "step context", which is primarily for DefPacts
  , _tcLoaded :: Loaded b i
  -- ^ All Loaded names currently in scope, provided from EvalM
  , _tcInDefPact :: Bool
  -- ^ Are we typechecking a defpact?
  }

makeLenses ''TCEnv

-- | Our typechecking state, which tracks:
--   - All currently typechecked definitions
--   - Interfaces that are valid for typechecking
--   - Interface definitions, for convenience and use during inference `n::f`
--   - The current type of the "yield" in a defpact step, for use in typechecking
--   - yield/resume pairs across defpacts
data TCState s i
  = TCState
  { _tcFree :: Map FullyQualifiedName (DefnType (TypeVar NamedDeBruijn))
  -- ^ Map from free variables (module definitions)
  --   To their corresponding type
  , _tcInferredInterfaces :: Map ModuleName (Interface i)
  -- ^ All interfaces that have been typechecked successfully
  , _tcInterfaceDefns :: Map QualifiedName (Located i (Type Void))
  -- ^ Inferface vcall mapping to the corresponding type
  , _tcYieldTy :: Maybe (Located i (TCType s))
  -- ^ The type of a previous "yield" call between steps
  }

instance Default (TCState s i) where
  def = TCState mempty mempty mempty Nothing

makeLenses ''TCState

-- | Term emitted by the typechecker prior to final generalization/unification.
type TCTerm s b i = Term Name (TCTypeVar s) b i

-- Term/defun outputs post typechecking
-- with ST monad existential removed
type TypedDefun b i = Defun Name DebruijnTypeVar b i

type TypedDefCap b i = DefCap Name DebruijnTypeVar b i

type TypedDef b i = Def Name DebruijnTypeVar b i

type TypedModule b i = Module Name DebruijnTypeVar b i

-- | Type for errors possible to emit during typechecking
data TypecheckError i
  = UnificationFailure (Located i (Type Text)) (Located i (Type Text))
  -- ^ Self explanatory. Means t1 ~ t2 does not hold.
  | RowUnificationFailure (Located i (RowTy Text)) (Located i (RowTy Text))
  -- ^ Same as UnificationFailure but for rows specifically
  | InvariantRowInTypeVarPosition (RowTy Text) i
  -- ^ Kind invariant failure. A type var is pointing to a row.
  | InvariantTypeInRowVarPosition (Type Text) i
  -- ^ Kind invariant failure. A row var is pointing to a type
  | InvariantUnboundTypeVariable Text (Type Text) i
  -- ^ Type variable is unbound
  | InvariantUnboundTermVariable Text i
  -- ^ Term variable is unbound
  | InvariantUnboundFreeVariable FullyQualifiedName i
  -- ^ Free variable is unbound
  | InvariantDefconstNotEvaluated FullyQualifiedName i
  -- ^ DefConst is not evaluated. This one is not possible to occur at all currently but
  --   it can happen if the compiler stages aren't used properly
  | UserGuardMustBeApp Text i
  -- ^ Actually impossible, but same as case above. Desugar.hs guarantees this won't happen, but
  --   if for whatever reason `Desugar` isn't used, then this is a possibility
  | ModuleLacksImplementedInterfaces ModuleName (S.Set ModuleName) i
  -- ^ Self explanatory
  | ExpectedKind PactKind PactKind i
  -- ^ Attempting to unify <expected> kind with <actual> kind. This also implies that <expected> != <actual>
  | InfiniteType (Located i (Type Text)) (Located i (Type Text))
  -- ^ Attempted to create a type such as `a ~ a -> a`
  | InfiniteRow (Located i (RowTy Text)) (Located i (RowTy Text))
  -- ^ Same as infiniteType, but for rows, such as `a ~ {}
  | DisallowedGenericSignature (Type Text) i
  -- ^ Generic signature is not allowed, so in other words, attempting to create a `defconst`
  --   that cannot be statically typechecked
  | CannotUnifyWithBoundVariable Text i
  -- ^ Attempting to perform unification with a bound variable.
  | CannotResolveConstraints [Pred i Text] i
  -- ^ Failed to resolved typeclass constraints
  | CannotStaticallyDetermineRowOpSig Text i
  -- ^ The signature of a function that uses row types was not able to be inferred.
  --   To elaborate on this error: row operations require string literal, such as `(at "f")`.
  --   They must be statically determined, so for example `(at s)` where `s` is a
  --   calculated string cannot be inferred
  | ArgumentRequiresTypeAnnotation Text i
  -- ^ Parameter to a function requires a type argument. Primarily necessary for interfaces, which
  --   we cannot infer a type for if it is not declared
  | InvalidDefcapManagerFun (Located i (Type Void)) (Located i (Type Void))
  -- ^ Defcap manager function does not match the expected type
  | CannotDetermineDynamicInvoke Name (S.Set ModuleName) Text i
  -- ^ We cannot determine the type of a dynamic invoke `m::f` statically
  | CannotInferTypeAsModRef (Located i (Type Text))
  -- ^ We cannot
  | TypecheckingDoesNotSupportType IR.Type i
  -- ^ A particular type is unsupported, such as `list` or `object` (Without schema)
  | DefPactStepIndexOutOfBounds FullyQualifiedName Int i
  deriving (Show, Eq)

-- | Our inference monad, where we can plumb through generalization "regions",
-- our variable environment and our "supply" of unique names
newtype InferM s b i a =
  InferM (ExceptT (TypecheckError i) (ReaderT (TCEnv s b i) (StateT (TCState s i) (ST s))) a)
  deriving
    ( Functor, Applicative, Monad
    , MonadReader (TCEnv s b i)
    , MonadState (TCState s i)
    , MonadError (TypecheckError i))
  via (ExceptT (TypecheckError i) (ReaderT (TCEnv s b i) (StateT (TCState s i) (ST s))))

-- | Forms which are "special" and require specific overloading
--   for example, (at "f") typechecks differently than `(at k)` for some variable `k`
data SpecialOverload
  = AddOverload
  | AccessOverload
  | FormatOverload
  | RemoveOverload
  | SortObjOverload
  | YieldOverload
  | YieldToChainOverload
  | ResumeOverload
  | ContainsOverload
  | WhereOverload
  | SelectWithFieldsOverload
  | ReadFieldsOverload
  | ZkScalarMulOverload
  | ZkPointAddOverload
  deriving (Eq, Ord, Show)

-- | Check if a `CoreBuiltin` has a `SpecialOverload` case
coreBuiltinToSpecialOverload :: CoreBuiltin -> Maybe SpecialOverload
coreBuiltinToSpecialOverload = \case
  CoreAdd -> Just AddOverload
  CoreAt -> Just AccessOverload
  CoreFormat -> Just FormatOverload
  CoreYield -> Just YieldOverload
  CoreYieldToChain -> Just YieldToChainOverload
  CoreRemove -> Just RemoveOverload
  CoreResume -> Just ResumeOverload
  CoreContains -> Just ContainsOverload
  CoreSortObject -> Just SortObjOverload
  CoreWhere -> Just WhereOverload
  CoreSelectWithFields -> Just SelectWithFieldsOverload
  CoreZKScalarMult -> Just ZkScalarMulOverload
  CoreZkPointAdd -> Just ZkPointAddOverload
  CoreReadWithFields -> Just ReadFieldsOverload
  _ -> Nothing

toSpecialOverload :: AsCoreBuiltin b => b -> Maybe SpecialOverload
toSpecialOverload b = preview _CoreBuiltin b >>= coreBuiltinToSpecialOverload

fromSpecialOverload :: AsCoreBuiltin b => SpecialOverload -> b
fromSpecialOverload = review _CoreBuiltin . specialOverLoadToCoreBuiltin

lookupDefnType :: i -> FullyQualifiedName -> InferM s b i (TypeScheme DebruijnTypeVar)
lookupDefnType info fqn =
  use (tcFree . at fqn) >>= \case
  Just defnTy -> case defnTy of
    NotIndexed t -> pure t
    -- `IndexedDefpactStepType` guarantees at least 1 step exists
    -- so IntMap.! 0 is safe
    IndexedDefpactStepType im -> view tcStepCtx >>= \case
      NotInStep -> pure $ NonGeneric $ liftType (im IntMap.! 0)
      StepIndex i -> case IntMap.lookup i im of
        Just ty -> pure $ NonGeneric $ liftType ty
        Nothing -> throwTypecheckError (DefPactStepIndexOutOfBounds fqn i info)
  _ ->
    throwTypecheckError $ InvariantUnboundFreeVariable fqn info

specialOverLoadToCoreBuiltin :: SpecialOverload -> CoreBuiltin
specialOverLoadToCoreBuiltin = \case
  AddOverload -> CoreAdd
  AccessOverload -> CoreAt
  FormatOverload -> CoreFormat
  RemoveOverload -> CoreRemove
  SortObjOverload -> CoreSortObject
  YieldOverload -> CoreYield
  YieldToChainOverload -> CoreYieldToChain
  ResumeOverload -> CoreResume
  ContainsOverload -> CoreContains
  WhereOverload -> CoreWhere
  SelectWithFieldsOverload -> CoreSelectWithFields
  ZkScalarMulOverload -> CoreZKScalarMult
  ZkPointAddOverload -> CoreZkPointAdd
  ReadFieldsOverload -> CoreReadWithFields

-- | Lift an IR.Type into a `Typed.Type`
--   This conversion is partial, because the TC does not support all types in legacy,
--   which includes untyped objects and untyped lists
liftCoreType :: i -> IR.Type -> InferM s b i (Type a)
liftCoreType i = \case
  IR.TyPrim p -> pure $ TyPrim (fromCorePrimType p)
  IR.TyList t ->
    TyList <$> liftCoreType i t
  IR.TyModRef mns -> pure $ TyModRef (MConcrete mns)
  IR.TyObject (IR.Schema _qn m) ->
    TyObject . RowConcrete <$> traverse (liftCoreType i) m
  IR.TyTable (IR.Schema _qn m) ->
    TyTable . RowConcrete <$> traverse (liftCoreType i) m
  IR.TyCapToken -> pure TyCapToken
  t -> throwTypecheckError (TypecheckingDoesNotSupportType t i)

-- | A bit of an annoying workaround for the fact that
--   `ensureWellKinded` doesn't provide us a proof that a particular
--   type variable is well kinded, which forces us to handle pattern matches
--   for `LinkRow` and `LinkRef` that are handled.
wellKindedGuaranteed :: InferM s b i a
wellKindedGuaranteed = error "well-kindedness guaranteed by enforceWellKinded"

-- | A typeclass which simply abstracts over the type ofa particular builtin.
--   It might not be immediately clear why this returns inside of `InferM`, but
--   the reason is due to not all natives being typecheckable in a vanilla sense.
--
--   For example, `(point-add)` won't resolve to a type, but `(point-add "g1")` will.
--   Some builtin patterns require specific term shapes to actually infer, as they're essentially
--   special forms for us to typecheck, but pact being a dynamically typed language at runtime can still evaluate
--   some expressions for which a static type cannot be inferred.
class (AsCoreBuiltin b, Show b, Pretty b) => TypeOfBuiltin b where
  typeOfBuiltin :: i -> b -> InferM s b' i (TypeScheme DebruijnTypeVar)

instance TypeOfBuiltin CoreBuiltin where
  typeOfBuiltin :: i -> CoreBuiltin -> InferM s b i (TypeScheme DebruijnTypeVar)
  typeOfBuiltin info = \case
    -- Add functions
    -- (+) : forall a. Add a => a -> a -> a
    CoreAdd ->
      pure $ addBinopType
    -- Num functions
    -- (-) : forall a. Num a => a -> a -> a
    CoreSub ->
      pure numBinopType
    -- (*) : forall a. Num a => a -> a -> a
    CoreMultiply ->
      pure numBinopType
    -- (/) : forall a. Num a => a -> a -> a
    CoreDivide ->
      pure numBinopType
    -- negate : forall a. Num a => a -> a
    CoreNegate ->
      pure unaryNumType
    -- abs : forall a. Num a => a -> a
    CoreAbs ->
      pure unaryNumType
    -- (^) : forall a. Num a => a -> a -> a
    CorePow ->
      pure numBinopType
    -- Boolean ops
    -- not : bool -> bool
    CoreNot ->
      pure $ NonGeneric (TyBool :~> TyBool)
    -- Eq functions
    -- (=) : forall a. Eq a => a -> a -> bool
    CoreEq ->
      pure eqTyp
    -- (!=) : forall a. Eq a => a -> a -> bool
    CoreNeq ->
      pure eqTyp
    -- Ord functions
    -- (>) : forall a. Ord a => a -> a -> bool
    CoreGT ->
      pure ordTyp
    -- (>=) : forall a. Ord a => a -> a -> bool
    CoreGEQ ->
      pure ordTyp
    -- (<) : forall a. Ord a => a -> a -> bool
    CoreLT ->
      pure ordTyp
    -- (<=) : forall a. Ord a => a -> a -> bool
    CoreLEQ ->
      pure ordTyp
    -- Integer ops
    -- (&) : integer -> integer -> integer
    CoreBitwiseAnd ->
      pure binaryInt
    -- (|) : integer -> integer -> integer
    CoreBitwiseOr ->
      pure binaryInt
    -- (xor) : integer -> integer -> integer
    CoreBitwiseXor ->
      pure binaryInt
    -- (~) : integer -> integer -> integer
    CoreBitwiseFlip ->
      pure unaryInt
    -- shift : integer -> integer -> integer
    CoreBitShift ->
      pure binaryInt
    -- mod : integer -> integer -> integer
    CoreMod ->
      pure binaryInt
    -- Rounding functions
    -- round : decimal -> integer
    CoreRound -> pure roundingFn
    -- ceiling : decimal -> integer
    CoreCeiling -> pure roundingFn
    -- floor : decimal -> integer
    CoreFloor -> pure roundingFn
    -- round-prec : decimal -> integer -> decimal
    CoreRoundPrec -> pure roundingPrecFn
    -- ceiling-prec : decimal -> integer -> decimal
    CoreCeilingPrec -> pure roundingPrecFn
    -- floor-prec : decimal -> integer -> decimal
    CoreFloorPrec -> pure roundingPrecFn
    -- Fractional
    -- exp : forall a. Fractional a => a -> decimal
    CoreExp ->
      pure unaryFractional
    -- ln : forall a. Fractional a => a -> decimal
    CoreLn ->
      pure unaryFractional
    -- sqrt : forall a. Fractional a => a -> decimal
    CoreSqrt ->
      pure unaryFractional
    -- log-base : forall a. Fractional a => a -> a -> decimal
    CoreLogBase -> do
      let aVar = TypeVariable 0 "a"
          a = TyVar aVar
      pure $ TypeScheme [aVar] [Fractional a] (a :~> a :~> a)
    -- ListLike
    -- length : forall a. ListLike a => a -> integer
    CoreLength -> do
      let aVar = TypeVariable 0 "a"
          a = TyVar aVar
      pure $ TypeScheme [aVar] [ListLike a] (a :~> TyInt)
    -- take : forall a. ListLike a => integer -> a -> a
    CoreTake -> pure takeDropTy
    -- drop : forall a. ListLike a => integer -> a -> a
    CoreDrop -> pure takeDropTy
    -- concat : [string] -> string
    -- todo: concat could most definitely be a `ListLike`
    CoreConcat ->
      pure $ NonGeneric (TyList TyString :~> TyString)
    -- reverse : forall a. [a] -> [a]
    CoreReverse -> do
      let aVar = TypeVariable 0 "a"
          a = TyVar aVar
      pure $ TypeScheme [aVar] [ListLike a] (a :~> a)
    -- map: forall a b. (a -> b) -> [a] -> [b]
    CoreMap -> do
      let aVar = TypeVariable 1 "a"
          bVar = TypeVariable 0 "b"
          a = TyVar aVar
          b = TyVar bVar
      pure $ TypeScheme [aVar, bVar] [] ((a :~> b) :~> TyList a :~> TyList b)
    -- filter : forall a. (a -> bool) -> [a] -> [a]
    CoreFilter -> do
      let aVar = TypeVariable 0 "a"
          a = TyVar aVar
      pure $ TypeScheme [aVar] [] ((a :~> TyBool) :~> TyList a :~> TyList a)
    -- TODO: contains needs a row overload
    -- contains: forall a. Eq a => a -> [a] -> bool
    CoreContains -> do
      let aVar = TypeVariable 0 "a"
          a = TyVar aVar
      pure $ TypeScheme [aVar] [Eq a] (a :~> TyList a :~> TyBool)
    -- sort : forall a. Ord a => [a] -> [a]
    CoreSort -> do
      let aVar = TypeVariable 0 "a"
          a = TyVar aVar
      pure $ TypeScheme [aVar] [Ord a] (TyList a :~> TyList a)
    -- sort for objects must look like
    -- (sort ['f, 'a, 'b]), so it must be handled by special overload
    -- If the field list after sort cannot be statically determined,
    -- it will throw this error
    CoreSortObject ->
      throwTypecheckError $ CannotStaticallyDetermineRowOpSig "sort" info
    -- (remove "f") : forall r1, r2, t. (({"f":t} ⊙ r2) ~ r1) => object {r1} -> object {r2}
    CoreRemove ->
      throwTypecheckError $ CannotStaticallyDetermineRowOpSig "remove" info
    -- if : forall a. bool -> a -> a -> a
    CoreIntToStr ->
      pure $ NonGeneric (TyInt :~> TyInt :~> TyString)
    -- str-to-int : string -> integer
    CoreStrToInt ->
      pure $ NonGeneric (TyString :~> TyInt)
    -- str-to-int-base : integer -> string -> integer
    CoreStrToIntBase ->
      pure $ NonGeneric (TyInt :~> TyString :~> TyInt)
    -- fold : forall a b. (b -> a -> b) -> b -> [a] -> b
    CoreFold -> do
      let aVar = TypeVariable 0 "a"
          bVar = TypeVariable 1 "b"
          a = TyVar aVar
          b = TyVar bVar
      pure $ TypeScheme [bVar, aVar] [] ((b :~> a :~> b) :~> b :~> TyList a :~> b)
    -- zip : forall a b c. (a -> b -> c) -> [a] -> [b] -> [c]
    CoreZip -> do
      let aVar = TypeVariable 2 "a"
          a = TyVar aVar
          bVar = TypeVariable 1 "b"
          b = TyVar bVar
          cVar = TypeVariable 0 "c"
          c = TyVar cVar
      pure $ TypeScheme [aVar, bVar, cVar] [] ((a :~> b :~> c) :~> TyList a :~> TyList b :~> TyList c)
    -- distinct : forall a. Ord a => [a] -> [a]
    CoreDistinct -> do
      let aVar = TypeVariable 0 "a"
          a = TyVar aVar
      pure $ TypeScheme [aVar] [Eq a] (TyList a :~> TyList a)
    -- Note: the type here is a fallback type
    -- The actual type of format when given a list literal is handled in `inferCoreBuiltinOverload`
    -- format : string -> [string] -> string
    CoreFormat ->
      pure $ NonGeneric (TyString :~> TyList TyString :~> TyString)
    -- enumerate : integer -> integer -> integer
    CoreEnumerate ->
      pure $ NonGeneric (TyInt :~> TyInt :~> TyList TyInt)
    -- enumerate-step-n : integer -> integer -> integer -> [integer]
    CoreEnumerateStepN ->
      pure $ NonGeneric (TyInt :~> TyInt :~> TyInt :~> TyList TyInt)
    -- show : forall a. Show a => a -> String
    CoreShow -> do
      let aVar = TypeVariable 0 "a"
          a = TyVar aVar
      pure $ TypeScheme [aVar] [IsValue a] (a :~> TyString)
    -- The type of read-msg is kind of... anything it wants to be, because
    -- this is dependent on env dependent data.
    -- it also means that it will also
    -- read-msg : forall a. string -> a
    CoreReadMsg -> do
      let aVar = TypeVariable 0 "a"
          a = TyVar aVar
      pure $ TypeScheme [aVar] [IsValue a] (TyString :~> a)
    -- Note: this will fail at runtime, it comes from the env data so we can't
    -- do much about its typing.
    -- read-msg : forall a. string -> a
    CoreReadMsgDefault -> do
      let aVar = TypeVariable 0 "a"
          a = TyVar aVar
      pure $ TypeScheme [aVar] [IsValue a] (TyNullary a)
    -- read-integer : string -> integer
    CoreReadInteger ->
      pure $ NonGeneric (TyString :~> TyInt)
    -- read-decimal : string -> decimal
    CoreReadDecimal ->
      pure $ NonGeneric (TyString :~> TyDecimal)
    -- read-string : string -> string
    CoreReadString ->
      pure $ NonGeneric (TyString :~> TyString)
    -- read-keyset : string -> guard
    CoreReadKeyset ->
      pure $ NonGeneric (TyString :~> TyGuard)
    -- Note: EnforceRead as a typeclass exists because enforce-guard and enforce-keyset
    -- also can take a string argument, which effectively interprets
    -- (enforce-guard "foo") == (enforce-guard (keyset-ref-guard "foo"))
    --
    -- enforce-guard : forall a. EnforceRead a => a -> bool
    CoreEnforceGuard -> do
      let aVar = TypeVariable 0 "a"
          a = TyVar aVar
      pure $ TypeScheme [aVar] [EnforceRead a] (a :~> TyBool)
    -- enforce-keyset : forall a. EnforceRead a => a -> bool
    CoreEnforceKeyset -> do
      let aVar = TypeVariable 0 "a"
          a = TyVar aVar
      pure $ TypeScheme [aVar] [EnforceRead a] (a :~> TyBool)
    -- keyset-ref-guard : string -> guard
    CoreKeysetRefGuard ->
      pure $ NonGeneric (TyString :~> TyGuard)
    -- create-capability-guard : cap-token -> guard
    CoreCreateCapabilityGuard ->
      pure $ TypeScheme [] [] (TyCapToken :~> TyGuard)
    -- create-capability-pact-guard : cap-token -> guard
    CoreCreateCapabilityPactGuard ->
      pure $ TypeScheme [] [] (TyCapToken :~> TyGuard)
    -- Note: this native is deprecated
    -- create-module-guard : string -> guard
    CoreCreateModuleGuard ->
      pure $ NonGeneric (TyString :~> TyGuard)
    -- Note: this native is deprecated
    -- create-pact-guard : string -> guard
    CoreCreateDefPactGuard ->
      pure $ NonGeneric (TyString :~> TyGuard)

    -- at : forall a. integer -> [a] -> a
    -- at k throws an error if it is out of bounds
    --
    -- However, if `(at "f")` is used, where "f" is a string literal
    -- then its type changes into
    -- (at "f") : forall (r: row, a:type). ({"f":a} <= r) => object{r} -> a
    CoreAt -> do
      let aVar = TypeVariable 0 "a"
          a = TyVar aVar
      pure $ TypeScheme [aVar] [] (TyInt :~> TyList a :~> a)
    -- make-list : forall a. integer -> a -> [a]
    CoreMakeList -> do
      let aVar = TypeVariable 0 "a"
          a = TyVar aVar
      pure $ TypeScheme [aVar] [] (TyInt :~> a :~> TyList a)
    -- base64-encode : string -> string
    CoreB64Encode ->
      pure $ NonGeneric (TyString :~> TyString)
    -- base64-decode : string -> string
    CoreB64Decode ->
      pure $ NonGeneric (TyString :~> TyString)
    -- str-to-list : string -> [string]
    CoreStrToList ->
      pure $ NonGeneric (TyString :~> TyList TyString)
    -- yield : forall (a: ROW) . object{a} -> object{a}
    CoreYield -> do
      let aVar = RowVariable 0 "a"
      pure $ TypeScheme [aVar] [] (TyObject (RowVar aVar) :~> TyObject (RowVar aVar))
    -- yield-to-chain : forall (a: ROW) . object{a} -> string -> object{a}
    CoreYieldToChain -> do
      let aVar = RowVariable 0 "a"
      pure $ TypeScheme [aVar] [] (TyObject (RowVar aVar) :~> TyString :~> TyObject (RowVar aVar))
    -- resume needs the previous yield form to typecheck, otherwise it simply will not.
    CoreResume ->
        throwTypecheckError $ CannotStaticallyDetermineRowOpSig "resume" info
    -- bind : forall (a: ROW, b: TYPE) . object{a} -> (object{a} -> b) -> b
    CoreBind -> do
      let aVar = RowVariable 1 "a"
          bodyVar = TypeVariable 0 "b"
      pure $ TypeScheme [aVar, bodyVar] [] (TyObject (RowVar aVar) :~> (TyObject (RowVar aVar) :~> TyVar bodyVar) :~> TyVar bodyVar)
    -- require-capability : cap-token -> bool
    CoreRequireCapability ->
      pure $ NonGeneric (TyCapToken :~> TyBool)
    -- compose-capability : cap-token -> bool
    CoreComposeCapability ->
      pure $ TypeScheme [] [] (TyCapToken :~> TyBool)
    -- install-capability : cap-token -> string
    CoreInstallCapability ->
      pure $ TypeScheme [] [] (TyCapToken :~> TyString)
    -- emit-event : cap-token -> string
    CoreEmitEvent -> do
      pure $ TypeScheme [] [] (TyCapToken :~> TyBool)
    CoreCreateTable -> do
      let r = RowVariable 0 "a"
      pure $ TypeScheme [r] [] (TyTable (RowVar r) :~> TyString)
    -- describe-keyset : string -> guard
    CoreDescribeKeyset ->
      pure $ TypeScheme [] [] (TyString :~> TyGuard)
    -- describe-module : string -> object{module-info}
    -- where
    -- (defschema module-info
    --   hash:string
    --   interfaces:[string]
    --   name:string)
    CoreDescribeModule -> do
      pure $ NonGeneric (TyString :~> TyObject (RowConcrete schema))
      where
      schema = M.fromList
        [(Field "hash", TyString)
        ,(Field "interfaces", TyList TyString)
        ,(Field "name", TyString)
        ,(Field "code", TyString)
        ,(Field "tx-hash", TyString)
        ]
    -- describe-module : string -> object{table-info}
    -- where
    -- (defschema table-info
    --   type:tystring
    --   module:string
    --   name:string)
    CoreDescribeTable -> do
      let rv = RowVariable 0 "a"
      pure $ TypeScheme [rv] [] (TyTable (RowVar rv) :~> TyObject (RowConcrete schema))
      where
      schema = M.fromList
        [(Field "type", TyString)
        ,(Field "module", TyString)
        ,(Field "name", TyString)]
    CoreDefineKeySet ->
      pure $ NonGeneric (TyString :~> TyGuard :~> TyString)
    CoreDefineKeysetData ->
      pure $ NonGeneric (TyString :~> TyString)
    -- fold-db : forall (r: ROW) (out : TYPE)
    --           .  table<r>
    --           -> (string -> object<r> -> bool)
    --           -> (string -> object<r> -> <out>)
    --           -> [<out>]
    CoreFoldDb -> do
      let rowVar = RowVariable 1 "row"
          outVar = TypeVariable 0 "a"
          queryLam = TyString :~> TyObject (RowVar rowVar) :~> TyBool
          appLam = TyString :~> TyObject (RowVar rowVar) :~> TyVar outVar
          fnTy = TyTable (RowVar rowVar) :~> queryLam :~> appLam :~> TyList (TyVar outVar)
      pure $ TypeScheme [rowVar, outVar] [] fnTy
    -- insert : forall (r: ROW) . table<r> -> string -> object<r> -> string
    CoreInsert -> do
      let rowVar = RowVariable 0 "row"
          fnTy = TyTable (RowVar rowVar) :~> TyString :~> TyObject (RowVar rowVar) :~> TyString
      pure $ TypeScheme [rowVar] [] fnTy
    -- keys : forall (r : Row) . table{r} -> [string]
    CoreKeys -> do
      let rowVar = RowVariable 0 "row"
          fnTy = TyTable (RowVar rowVar) :~> TyList TyString
      pure $ TypeScheme [rowVar] [] fnTy
    --
    CoreRead -> do
      let rowVar = RowVariable 0 "row"
          fnTy = TyTable (RowVar rowVar) :~> TyString :~> TyObject (RowVar rowVar)
      pure $ TypeScheme [rowVar] [] fnTy
    CoreSelect -> do
      let rowVar = RowVariable 0 "row"
          fnTy = TyTable (RowVar rowVar) :~> (TyObject (RowVar rowVar) :~> TyBool) :~> TyList (TyObject (RowVar rowVar))
      pure $ TypeScheme [rowVar] [] fnTy
    CoreSelectWithFields ->
      throwTypecheckError $ CannotStaticallyDetermineRowOpSig "select" info
    -- update : forall (r1: ROW, r2: ROW) . (r2 ≼ r1) => table<r1> -> string -> object<r2> -> string
    CoreUpdate -> do
          -- r1
      let r1 = RowVariable 1 "r1"
          -- r2
          r2 = RowVariable 0 "r2"
          -- r1 ≼ r2
          rowConstr = RoseSubRow (RoseRowTy (RowVar r2)) (RoseRowTy (RowVar r1))
          -- table{r1} -> string -> object{r2} -> string
          fnTy = TyTable (RowVar r1) :~> TyString :~> TyObject (RowVar r2) :~> TyString
      -- forall (r1: ROW, r2: ROW) . (r2 ≼ r1) => table<r1> -> string -> object<r2> -> string
      pure $ TypeScheme [r1, r2] [rowConstr] fnTy
    -- with-default-read : forall (r1: ROW) . table<r1> -> string -> object<r1> -> (object<r1> -> a) -> a
    CoreWithDefaultRead -> do
      let r1 = RowVariable 1 "r1"
      let aVar = TypeVariable 0 "a1"
      let fnTy = TyTable (RowVar r1) :~> TyString :~> TyObject (RowVar r1) :~> (TyObject (RowVar r1) :~> TyVar aVar) :~> TyVar aVar
      pure $ TypeScheme [r1, aVar] [] fnTy
    CoreWithRead -> do
      let r1 = RowVariable 1 "r1"
      let aVar = TypeVariable 0 "a1"
      let fnTy = TyTable (RowVar r1) :~> TyString :~> (TyObject (RowVar r1) :~> TyVar aVar) :~> TyVar aVar
      pure $ TypeScheme [r1, aVar] [] fnTy
    CoreWrite -> do
      let r1 = RowVariable 0 "r1"
      let fnTy = TyTable (RowVar r1) :~> TyString :~> TyObject (RowVar r1) :~> TyString
      pure $ TypeScheme [r1] [] fnTy
    CoreTxHash ->
      pure $ NonGeneric (TyNullary TyString)
    CoreAndQ -> do
      let aVar = TypeVariable 0 "a"
      let a = TyVar aVar
      let cloTy = a :~> TyBool
      pure $ TypeScheme [aVar] [] (cloTy :~> cloTy :~> a :~> TyBool)
    CoreOrQ -> do
      let aVar = TypeVariable 0 "a"
      let a = TyVar aVar
      let cloTy = a :~> TyBool
      pure $ TypeScheme [aVar] [] (cloTy :~> cloTy :~> a :~> TyBool)
    CoreWhere ->
      throwTypecheckError $ CannotStaticallyDetermineRowOpSig "where" info
    CoreNotQ -> do
      let aVar = TypeVariable 0 "a"
      let a = TyVar aVar
      let cloTy = a :~> TyBool
      pure $ TypeScheme [aVar] [] (cloTy :~> a :~> TyBool)
    CoreHash -> do
      let aVar = TypeVariable 0 "a"
      let a = TyVar aVar
      pure $ TypeScheme [aVar] [IsValue a] (a :~> TyString)
    -- note: continue is basically the indentity function
    CoreContinue -> do
      let aVar = TypeVariable 0 "a"
      let a = TyVar aVar
      pure $ TypeScheme [aVar] [] (a :~> a)
    CoreParseTime ->
      pure $ NonGeneric (TyString :~> TyString :~> TyTime)
    CoreFormatTime ->
      pure $ NonGeneric (TyString :~> TyTime :~> TyString)
    CoreTime ->
      pure $ NonGeneric (TyString :~> TyTime)
    CoreAddTime -> do
      let aVar = TypeVariable 0 "a"
      let a = TyVar aVar
      pure $ TypeScheme [aVar] [Num a] (TyTime :~> a :~> TyTime)
    CoreDiffTime ->
      pure $ NonGeneric (TyTime :~> TyTime :~> TyDecimal)
    CoreHours -> do
      let aVar = TypeVariable 0 "a"
      let a = TyVar aVar
      pure $ TypeScheme [aVar] [Num a] (a :~> TyDecimal)
    CoreMinutes -> do
      let aVar = TypeVariable 0 "a"
      let a = TyVar aVar
      pure $ TypeScheme [aVar] [Num a] (a :~> TyDecimal)
    CoreDays -> do
      let aVar = TypeVariable 0 "a"
      let a = TyVar aVar
      pure $ TypeScheme [aVar] [Num a] (a :~> TyDecimal)
    CoreCompose -> do
      let aVar = TypeVariable 2 "a"
      let bVar = TypeVariable 1 "b"
      let cVar = TypeVariable 0 "c"
      pure $ TypeScheme [aVar, bVar, cVar] []
          ((TyVar aVar :~> TyVar bVar)
            :~> (TyVar bVar :~> TyVar cVar)
            :~> TyVar aVar
            :~> TyVar cVar)
    CoreCreatePrincipal ->
      pure $ NonGeneric (TyGuard :~> TyString)
    CoreIsPrincipal ->
      pure $ NonGeneric (TyString :~> TyBool)
    CoreTypeOfPrincipal ->
      pure $ NonGeneric (TyString :~> TyString)
    CoreValidatePrincipal ->
      pure $ NonGeneric (TyGuard :~> TyString :~> TyBool)
    CoreNamespace ->
      pure $ NonGeneric (TyString :~> TyString)
    CoreDefineNamespace ->
      pure $ NonGeneric (TyString :~> TyGuard :~> TyGuard :~> TyString)
    CoreDescribeNamespace ->
      pure $ NonGeneric  (TyString :~> TyObject (RowConcrete schema))
      where
      schema = M.fromList
        [ (Field "admin-guard", TyGuard)
        , (Field "namespace-name", TyString)
        , (Field "user-guard", TyGuard)]
    CoreZkPairingCheck ->
      pure $ NonGeneric $
        TyList (TyObject (RowConcrete (zkSchemaFromGroup ZKG1))) :~> TyList (TyObject (RowConcrete (zkSchemaFromGroup ZKG2))) :~> TyBool
    CoreZKScalarMult ->
      throwTypecheckError $ CannotStaticallyDetermineRowOpSig "read" info
    CoreZkPointAdd ->
      throwTypecheckError $ CannotStaticallyDetermineRowOpSig "point-add" info
    CorePoseidonHashHackachain ->
      pure $ NonGeneric $ TyList TyInt :~> TyInt
    CoreChainData ->
      pure $ NonGeneric (TyNullary (TyObject (RowConcrete chainDataSchema)))
    CoreIsCharset ->
      pure $ NonGeneric (TyInt :~> TyString :~> TyBool)
    CorePactId ->
      pure $ NonGeneric (TyNullary TyString)
    CoreTypeOf -> do
        let aVar = TypeVariable 0 "a"
        let a = TyVar aVar
        pure $ TypeScheme [aVar] [IsValue a] (a :~> TyString)
    CoreDec ->
      pure $ NonGeneric (TyInt :~> TyDecimal)
    CoreCond -> do
      let aVar = TypeVariable 0 "a"
      let a = TyVar aVar
      pure $ TypeScheme [aVar] [] (TyNullary a :~> a)
    CoreIdentity -> do
      let aVar = TypeVariable 0 "a"
      let a = TyVar aVar
      pure $ TypeScheme [aVar] [] (a :~> a)
    CoreVerifySPV -> do
      let aVar = RowVariable 0 "a"
      pure $ TypeScheme [aVar] [] (TyString :~> TyObject (RowVar aVar) :~> TyObject (RowVar aVar))
    CoreEnforceVerifier ->
      pure $ NonGeneric (TyString :~> TyBool)
    CoreAcquireModuleAdmin -> do
      let aVar = MRefVariable 0 "a"
      pure $ TypeScheme [aVar] [] (TyModRef (MRefVar aVar) :~> TyString)
    CoreReadWithFields ->
      throwTypecheckError $ CannotStaticallyDetermineRowOpSig "read" info
    CoreListModules ->
      pure $ NonGeneric (TyNullary (TyList TyString))
    CoreStaticRedeploy -> do
      pure $ NonGeneric (TyString :~> TyUnit)
    CoreHashKeccak256 -> do
      pure $ NonGeneric (TyList TyString :~> TyString)
    CoreHashPoseidon ->
      pure $ NonGeneric (TyList TyInt :~> TyInt)
    CoreHyperlaneMessageId ->
      pure $ NonGeneric (TyObject (RowConcrete hyperlaneMsgObjectSchema) :~> TyString)
    CoreHyperlaneDecodeMessage ->
      pure $ NonGeneric (TyString :~> TyObject (RowConcrete schema))
      where
      schema = M.fromList
        [ (Field "recipient", TyGuard)
        , (Field "amount", TyDecimal)
        , (Field "chaindId", TyString)
        ]
    CoreHyperlaneEncodeMessage ->
      pure $ NonGeneric (TyObject (RowConcrete hyperlaneTokenMsgSchema) :~> TyString)
    where
    hyperlaneTokenMsgSchema = M.fromList
      [ (Field "recipient", TyString)
      , (Field "amount", TyDecimal)
      , (Field "chainId", TyString)
      ]
    hyperlaneMsgObjectSchema = M.fromList
      [ (Field "version", TyInt)
      , (Field "nonce", TyInt)
      , (Field "originDomain", TyInt)
      , (Field "sender", TyString)
      , (Field "destinationDomain", TyInt)
      , (Field "recipient", TyString)
      , (Field "messageBody", TyString)
      ]
    unaryNumType =
      let aVar = TypeVariable 0 "a"
          a = TyVar aVar
      in TypeScheme [aVar] [Num a] (a :~> a)
    unaryFractional =
      let aVar = TypeVariable 0 "a"
          a = TyVar aVar
      in TypeScheme [aVar] [Fractional a] (a :~> TyDecimal)
    addBinopType =
      let aVar = TypeVariable 0 "a"
          a = TyVar aVar
      in TypeScheme [aVar] [Add a] (a :~> a :~> a)
    numBinopType =
      let aVar = TypeVariable 0 "a"
          a = TyVar aVar
      in TypeScheme [aVar] [Num a] (a :~> a :~> a)
    eqTyp =
      let aVar = TypeVariable 0 "a"
          a = TyVar aVar
      in TypeScheme [aVar] [Eq a] (a :~> a :~> TyBool)
    ordTyp =
      let aVar = TypeVariable 0 "a"
          a = TyVar aVar
      in TypeScheme [aVar] [Ord a] (a :~> a :~> TyBool)
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
      let aVar = TypeVariable 0 "a"
          a = TyVar aVar
      in TypeScheme [aVar] [ListLike a] (TyInt :~> a :~> a)

instance TypeOfBuiltin b => TypeOfBuiltin (ReplBuiltin b) where
  typeOfBuiltin info = \case
    RBuiltinWrap b -> typeOfBuiltin info b
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


liftST :: ST s a -> InferM s b i a
liftST action = InferM $ lift $ lift $ lift action

throwTypecheckError :: TypecheckError i -> InferM s b i a
throwTypecheckError = throwError

---------------------------------------------------------------
-- _dbg functions exist to turn a `TCType s` into a string representation
-- ---------------------------------------------------------------

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
      TyAppRef t' -> TyAppRef <$> _dbgMRef t'
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

_dbgTypeScheme :: TypeScheme (TCTypeVar s) -> InferM s b i (TypeScheme Text)
_dbgTypeScheme (TypeScheme tvs preds ty) = do
  tvs' <- traverse _dbgVar' tvs
  preds' <- traverse _dbgBuiltinTC preds
  ty' <- _dbgType ty
  pure (TypeScheme tvs' preds' ty')

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
  RoseSubRow r1 r2 ->
    RoseSubRow <$> _dbgRoseRow r1 <*> _dbgRoseRow r2
  RoseRowEq r1 r2 ->
    RoseSubRow <$> _dbgRoseRow r1 <*> _dbgRoseRow r2

_dbgPred :: TCPred i s -> InferM s b i (Pred i Text)
_dbgPred (Located i a) = Located i <$> _dbgBuiltinTC a

_dbgType :: TCType s -> InferM s b i (Type Text)
_dbgType = \case
  TyVar tv -> _dbgVar tv
  TyFun l r -> TyFun <$> _dbgType l <*> _dbgType r
  TyList t -> TyList <$> _dbgType t
  TyPrim p -> pure (TyPrim p)
  -- TODO: implement the correct _dbg
  TyModRef mref -> TyModRef <$> _dbgMRef mref
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
    Unbound u _ l -> pure $ RowVar $ "unbound" <> T.pack (show (u, l))
    Bound u l -> pure $ RowVar $ "bound" <> T.pack (show (u, l))
    LinkRow ty -> _dbgRowCtor ty
    _ -> wellKindedGuaranteed

_dbgMRef :: MRef (TCTypeVar s) -> InferM s b i (MRef Text)
_dbgMRef = \case
  MRefVar n -> _dbgMRefVar n
  MConcrete s -> pure (MConcrete s)
  where
  _dbgMRefVar tv = readTvRef tv >>= \case
    Unbound u l _ -> pure $ MRefVar ("unbound" <> T.pack (show (u, l)))
    Bound u l -> pure $ MRefVar ("bound" <> T.pack (show (u, l)))
    LinkRef m -> _dbgMRef m
    _ -> wellKindedGuaranteed


_dbgVar :: TCTypeVar s -> InferM s b i (Type Text)
_dbgVar tv = readTvRef tv >>= \case
  Unbound u l _ -> pure (TyVar ("unbound" <> T.pack (show (u, l))))
  Bound u l -> pure (TyVar ("bound" <> T.pack (show (u, l))))
  LinkTy ty -> _dbgType ty
  LinkRow ty -> _dbgType (TyObject ty)
  LinkRef _ -> error "invariant ref in row var posn"

_dbgVar' :: TCTypeVar s -> InferM s b i Text
_dbgVar' tv = readTvRef tv >>= \case
  Unbound u l _ -> pure ("unbound" <> T.pack (show (u, l)))
  Bound u l -> pure ("bound" <> T.pack (show (u, l)))
  _ -> error "invariant"


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

newTvRef :: InferM s b i (TvRef s)
newTvRef = do
  u <- newSupplyIx
  let tvName = "a_" <> T.pack (show u)
  l <- currentLevel
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
byInst :: TCPred i s -> InferM s b i (Maybe [TCPred i s])
byInst (Located i p) = case p of
  Eq ty -> eqInst i ty
  Add ty -> addInst ty
  Num ty -> numInst ty
  Ord ty -> ordInst i ty
  Show ty -> showInst i ty
  ListLike ty -> listLikeInst ty
  Fractional ty -> fractionalInst ty
  EnforceRead ty -> enforceReadInst ty
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
--  instance Eq time
--
--  instance (Eq 'a) => Eq (list 'a)
--
--  For rows, in theory it should be:
--  instance (Eq t_1,..,Eq t_n) => Eq (RowConcrete {l_1:t1,..,l_n)
--    where t_1..t_n are monotypes without type variables.
--  instance (EqRow r) => Eq (Object {r})
--
--  However, since all objects can only hold values for which there is an `IsValue` instance to them,
--  and all `IsValue` values have an `Eq` instance, then it suffices to say that all objects
--  have an eq instance
eqInst :: i -> TCType s -> InferM s b i (Maybe [TCPred i s])
eqInst i = \case
  TyVar tv -> readTvRef tv >>= \case
    LinkTy ty -> eqInst i ty
    _ -> pure Nothing
  -- All prims have an EQ instance
  TyPrim _ -> pure (Just [])
  TyModRef _ -> pure (Just [])
  TyList t -> pure (Just [Located i (Eq t)])
  TyCapToken -> pure (Just [])
  TyObject _ -> pure $ Just []
  _ -> pure Nothing

-- | Instances of Ord:
--
--  instance Ord integer
--  instance Ord decimal
--  instance Ord string
--  instance Ord unit
--  instance Ord time
--
--  instance (Ord 'a) => Ord (list 'a)
--
ordInst :: i -> TCType s -> InferM s b i (Maybe [TCPred i s])
ordInst i = \case
  TyVar tv -> readTvRef tv >>= \case
    LinkTy ty -> ordInst i ty
    _ -> pure Nothing
  -- All prims have an Ord instance
  TyPrim p -> case p of
    PrimInt -> pure (Just [])
    PrimDecimal -> pure (Just [])
    PrimString -> pure (Just [])
    PrimTime -> pure (Just [])
    PrimUnit -> pure (Just [])
    PrimBool -> pure (Just [])
    _ -> pure Nothing
  TyList t -> pure (Just [Located i (Ord t)])
  _ -> pure Nothing


-- | Instances of Add:
--
--  instance Add integer
--  instance Add decimal
--  instance Add string
--  instance Add (list 'a)
--
--  Note: you can still call + on objects with our typechecker, but that's handled by `SpecialOverload`
addInst :: TCType s -> InferM s b i (Maybe [TCPred i s])
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

-- | A typeclass representing `PactValue`s, aka any "value" reduction that is not a closure
--
--  Instances:
--  instance IsValue string
--  instance IsValue integer
--  instance IsValue decimal
--  instance IsValue time
--  instance IsValue a => IsValue [a]
--  forall (l:label). instance IsValue a => IsValue object{l:a}
--  forall (l:label). instance IsValue a => IsValue table{l:a}
isValueInst :: TCType s -> InferM s b i (Maybe [TCPred i s])
isValueInst = \case
  TyVar tv -> readTvRef tv >>= \case
    LinkTy ty -> isValueInst ty
    _ -> pure Nothing
  -- All prims are pact values
  TyPrim _p -> pure (Just [])
  -- All lists are values by construction
  TyList t -> isValueInst t
  -- Assume objects are values
  TyObject _ -> pure (Just [])
  TyCapToken -> pure (Just [])
  TyModRef{} -> pure (Just [])
  TyTable{} -> pure (Just [])
  TyFun {} -> pure Nothing
  TyNullary{} -> pure Nothing
  -- _ -> pure Nothing


-- | class Num a where
--     ; arith functions
--     * : a -> a -> a
--     / : a -> a -> a
--     - : a -> a -> a
--     negate : a -> a -> a
--     abs : a -> a
--     pow : a -> a -> a
--
--     ; time functions
--     add-time : time -> a -> time
--     hours : a -> decimal
--     minutes : a -> decimal
--     days : a -> decimal
--
--
-- Instances of num:
-- instance Num integer
-- instance Num decimal
numInst :: TCType s -> InferM s b i (Maybe [TCPred i s])
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

-- | class Fractional a where
--     ln : a -> decimal
--     sqrt : a -> decimal
--     exp : a -> decimal
--     log : a -> a -> a
--     sqrt : a -> decimal
--
-- Instances of fractional:
-- instance Fractional integer
-- instance Fractional decimal
fractionalInst :: TCType s -> InferM s b i (Maybe [TCPred i s])
fractionalInst = \case
  TyVar tv -> readTvRef tv >>= \case
    LinkTy ty -> fractionalInst ty
    _ -> pure Nothing
  TyPrim p -> pure $ case p of
    PrimInt -> Just []
    PrimDecimal -> Just []
    _ -> Nothing
  _ -> pure Nothing

-- | class ListLike a where
--     take : integer -> a -> a
--     drop : integer -> a -> a
--     reverse : a -> a
--     length : a -> integer
--
-- Instances of `ListLike`:
-- instance ListLike string
-- instance ListLike ['a]
--
listLikeInst :: TCType s -> InferM s b i (Maybe [TCPred i s])
listLikeInst = \case
  TyVar tv -> readTvRef tv >>= \case
    LinkTy ty -> listLikeInst ty
    _ -> pure Nothing
  TyPrim p -> pure $ case p of
    PrimString -> Just []
    _ -> Nothing
  TyList _ -> pure $ Just []
  _ -> pure Nothing

-- | class EnforceRead a where
--    enforce-keyset : a -> guard
--    enforce-guard : a -> guard
--
-- Instances of `EnforceRead`:
-- instance EnforceRead string
-- instance EnforceRead guard
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

roseSubRow :: i -> TCRoseRow s -> TCRoseRow s -> InferM s b i (Maybe [TCPred i s])
roseSubRow i l r = do
  l' <- normalizeRoseSubrow l
  r' <- normalizeRoseSubrow r
  case (l', r') of
    (RoseConcrete lrow, RoseConcrete rrow) -> do
      let lkeys = S.fromList (M.keys lrow)
          rkeys = S.fromList (M.keys rrow)
      if (lkeys `S.isSubsetOf` rkeys) then do
        forM_ (M.toList lrow) $ \(k, v) -> unify (Located i v) (Located i (rrow M.! k))
        pure $ Just []
      else pure Nothing
    _ -> pure Nothing


roseRowEq :: i -> TCRoseRow s -> TCRoseRow s -> InferM s b i (Maybe [TCPred i s])
roseRowEq i l r = do
  l' <- normalizeRoseSubrow l
  r' <- normalizeRoseSubrow r
  -- Todo:
  case (l', r') of
    (RoseConcrete lrow, RoseVar rvar) ->
      unifyConcrete (Located i rvar) (Located i lrow) *> pure (Just [])
    (RoseVar lvar, RoseConcrete rrow) ->
      unifyConcrete (Located i lvar) (Located i rrow) *> pure (Just [])
    (RoseRowCat (RoseVar n) (RoseConcrete lrow), RoseConcrete rrow) ->
      resolveMissingConstr n lrow rrow
    (RoseRowCat (RoseConcrete lrow) (RoseVar n), RoseConcrete rrow) ->
      resolveMissingConstr n lrow rrow
    (RoseConcrete lrow, RoseConcrete rrow) -> do
      let lkeys = S.fromList (M.keys lrow)
          rkeys = S.fromList (M.keys rrow)
      unless (lkeys == rkeys) $ do
        lrow' <- _dbgRowCtor (RowConcrete lrow)
        rrow' <- _dbgRowCtor (RowConcrete rrow)
        throwTypecheckError (RowUnificationFailure (Located i lrow') (Located i rrow'))
      zipWithM_ (\x y -> unify (Located i x) (Located i y)) (M.elems lrow) (M.elems rrow)
      pure $ Just []
    _ -> pure Nothing
  where
  resolveMissingConstr n lrow rrow = do
      let keysL = M.keysSet lrow
          keysR = M.keysSet rrow
      if (keysL `S.isProperSubsetOf` keysR) then do
        let remainingFields = M.difference rrow lrow
        zipWithM_ (\x y -> unify (Located i x) (Located i y)) (M.elems lrow) (M.elems (M.restrictKeys rrow keysL))
        unifyRowVar (Located i n) (Located i (RowConcrete remainingFields))
        pure (Just [])
      else pure Nothing
  unifyConcrete var row =
    unifyRowVar var (RowConcrete <$> row)

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
--  instance (Show {l1:t1, .., ln:tn}) where t1..tn are monotypes without type variables.
--
showInst :: i -> TCType s -> InferM s b i (Maybe [TCPred i s])
showInst i = \case
  TyVar tv -> readTvRef tv >>= \case
    LinkTy ty -> showInst i ty
    _ -> pure Nothing
  -- All prims have a show instance
  TyPrim _p -> pure (Just [])
  TyCapToken -> pure (Just [])
  TyObject{} -> pure (Just [])
  TyTable{} -> pure (Just [])
  TyModRef{} -> pure (Just [])
  TyList t -> pure (Just [Located i (Show t)])
  _ -> pure Nothing

entail :: i -> [TCPred i s] -> TCPred i s -> InferM s b i Bool
entail i ps p = byInst p >>= \case
  Nothing -> pure False
  Just qs -> and <$> traverse (entail i ps) qs

typedArgToTypeScheme :: TypedArg (Type n) i -> TypeScheme n
typedArgToTypeScheme (TypedArg _ t _) = TypeScheme [] [] t

isHnf :: TCPred i s -> InferM s b i Bool
isHnf (Located _ t) = case t of
  Eq ty -> tyHnf ty
  Add ty -> tyHnf ty
  Num ty -> tyHnf ty
  Ord ty -> tyHnf ty
  Show ty -> tyHnf ty
  ListLike ty -> tyHnf ty
  Fractional ty -> tyHnf ty
  EnforceRead ty -> tyHnf ty
  IsValue ty -> tyHnf ty
  RoseSubRow l r -> roseSubRowHNF l r
  RoseRowEq l r -> rowEqHNF l r
  where
  rowEqHNF (RoseVar _) (RoseVar _) = pure True
  rowEqHNF (RoseRowCat (RoseVar _) _) (RoseVar _) = pure True
  rowEqHNF (RoseRowCat _ (RoseVar _)) (RoseVar _) = pure True
  rowEqHNF _ _ = pure False

  roseSubRowHNF (RoseVar _) _ = pure True
  roseSubRowHNF _ (RoseVar _) = pure True
  roseSubRowHNF _ _ = pure False

tyHnf :: TCType s -> InferM s b i Bool
tyHnf = go
  where
  go = \case
    TyVar tv -> readTvRef tv >>= \case
      LinkTy ty -> go ty
      _ -> pure True
    TyObject c -> rowCtorHnf c
    TyTable c -> rowCtorHnf c
    _ -> pure False
  rowCtorHnf = \case
    RowVar v -> readTvRef v >>= \case
      LinkRow r -> rowCtorHnf r
      _ -> pure False
    RowConcrete rows ->
      and <$> traverse go rows

toHnf :: i -> TCPred i s -> InferM s b i [TCPred i s]
toHnf i p = isHnf p >>= \case
  True -> pure [p]
  False -> byInst p >>= \case
    Nothing -> do
      t' <- _dbgPred p
      throwTypecheckError $ CannotResolveConstraints [t'] i
    Just ps -> toHnfs i ps

toHnfs :: i -> [TCPred i s] -> InferM s b i [TCPred i s]
toHnfs i ps = do
  pss <- traverse (toHnf i) ps
  pure (concat pss)

simplify :: i -> [TCPred i s] -> InferM s b i [TCPred i s]
simplify i = loop []
  where
  loop rs [] = pure rs
  loop rs (p:ps) = entail i (rs ++ ps) p >>= \cond ->
    if cond then loop rs ps else loop (p:rs) ps



reduce :: i -> [TCPred i s] -> InferM s b i [TCPred i s]
reduce i ps = do
  ps' <- (traverse.locElem) traverseTCLinks ps >>= toHnfs i >>= simplify i
  -- Interesting condition: it's possible that through constraint simplification, we don't actually solve _all_ constraints
  -- the first time. The reason being that unifying rows may make other constraints solvable, so we check that
  -- all constraints left aren't in head normal form. If there is at least one that's solvable, we reduce again
  --
  -- Note: this could be done more efficiently, a future PR can address this if it ever becomes a perf bottleneck, but in practice it shouldn't.
  -- it's highly unlikely a user is generalizing over that many predicates, so most will be solved by the second recursive call to reduce
  allSolved <- traverse isHnf ps'
  if not (and allSolved) then reduce i ps'
  else pure ps'

-- | Split typeclass constraints into
--   those that are resolved and those that have unresolved type variables
split
  :: [TCPred i s]
  -> i
  -> InferM s b i ([TCPred i s], [TCPred i s])
split ps _i = do
  -- ps' <- reduce i ps
  partition' ([], []) ps
  where
  partition' (ds, rs) (p@(Located _ ty) : xs) = do
    cond <- tcHasUnbound ty
    if cond then partition' (p:ds, rs) xs
    else partition' (ds, p:rs) xs
  partition' (ds, rs) [] =
    pure (reverse ds, reverse rs)
  varUnbound ref = readTvRef ref >>= \case
    Unbound{} -> pure True
    LinkTy ty -> hasUnbound ty
    LinkRow r -> rowUnbound r
    LinkRef _ -> pure False
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
  :: i
  -> TypeScheme (TCTypeVar s)
  -> TCTerm s b i
  -> InferM s b i (TCType s, TCTerm s b i, [TCPred i s])
instantiateWithTerm _ (TypeScheme [] [] t) te = pure (t, te, [])
instantiateWithTerm i (TypeScheme ts preds ty) term = do
  nts <- traverse (\tv -> newTypeVar (_tpKind tv)) ts
  let m = zip ts nts
  preds' <- traverse (instTC m) preds
  ty' <- instBound m ty
  term' <- case TyAppVar <$> nts of
    x:xs -> do
      pure $ TyApp term (x:|xs) info
    [] -> pure term
  pure (ty', term', (Located i <$> preds'))
  where
  info = term ^. termInfo
  instTC m tt =
    traverseTCType (instBound m) tt
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
        Nothing ->
          pure t
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

-- Instantiate a type scheme such as
-- forall a. a -> a into
-- a_fresh_k -> a_fresh_k
instantiateImported
  :: i
  -> TypeScheme DebruijnTypeVar
  -> InferM s b i (TCType s, [TCTypeVar s], [TCPred i s])
instantiateImported i (TypeScheme tvs preds ty) = do
    ntvs <- traverse (const newTvRef) tvs
    let ntvs' = zipWith (\tvr (TypeVar _ kind) -> TypeVar tvr kind) ntvs tvs
    let rl = RAList.fromList (reverse ntvs)
    ty' <- inst rl ty
    preds' <- traverse (instTC rl) preds
    pure (ty', ntvs', (Located i <$> preds'))
  where
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
    TyModRef mr -> TyModRef <$> instMRef rl mr
    TyNullary n -> TyNullary <$> inst rl n
    TyObject rc -> TyObject <$> instRow rl rc
    TyTable rc -> TyTable <$> instRow rl rc
    TyCapToken -> pure TyCapToken
  instMRef rl = \case
    MRefVar v -> MRefVar <$> instNamed rl v
    MConcrete m -> pure (MConcrete m)
  instRow rl = \case
    RowVar n -> RowVar <$> instNamed rl n
    RowConcrete m ->
      RowConcrete <$> traverse (inst rl) m

-- Check that we are not unifying infinite types, that is,
-- for example `a ~ a -> a`. Pact doesn't have a notion of recursive types
-- (yet? :) ) so we perform this check.
occurs
  :: (Located i (TCTypeVar s))
  -> (Located i (TCType s))
  -> InferM s b i ()
occurs (Located tvi tv) (Located tyi tct) = go tct
  where
  go = \case
    TyVar tv' | tv == tv' -> do
      tl <- _dbgType (TyVar tv)
      tr <- _dbgType tct
      throwTypecheckError $ InfiniteType (Located tvi tl) (Located tyi tr)
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
      throwTypecheckError $ InfiniteType (Located tvi tl) (Located tyi tr)
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
  :: Located i (TCTypeVar s)
  -> Located i (TCRowCtor s)
  -> InferM s b i ()
occursRow locTv@(Located tvi tv) (Located tci tct) = go tct
  where
  go = \case
    RowVar tv' | tv == tv' -> do
      tl <- _dbgRowCtor (RowVar tv)
      -- Todo: TyObject is not the correct type here for the occurs check
      tr <- _dbgRowCtor tct
      throwTypecheckError $ InfiniteRow (Located tvi tl) (Located tci tr)
    RowVar tv' -> bindRef tv'
    RowConcrete t -> traverse_ (\r -> occurs locTv (Located tci r)) t
  bindRef tv' = readTvRef tv' >>= \case
    Unbound n u l' -> do
      ml <- minLevel
      writeTvRef tv' (Unbound n u ml)
      where
      minLevel = readTvRef tv >>= \case
        Unbound _ _ l -> pure (min l l')
        _ -> pure l'
    LinkRow ty -> occursRow locTv (Located tci ty)
    _ -> pure ()

occursRef
  :: Located i (TCTypeVar s)
  -> Located i (MRef (TCTypeVar s))
  -> InferM s b i ()
occursRef (Located _ tv) (Located _ tct) = go tct
  where
  go = \case
    MRefVar tv' -> bindRef tv'
    MConcrete _ -> pure ()
  bindRef tv' = readTvRef tv' >>= \case
    Unbound n u l' -> do
      ml <- minLevel
      writeTvRef tv' (Unbound n u ml)
      where
      minLevel = readTvRef tv >>= \case
        Unbound _ _ l -> pure (min l l')
        _ -> pure l'
    _ -> pure ()

ensureWellKinded
  :: i
  -> TCTypeVar s
  -> PactKind
  -> InferM s b i ()
ensureWellKinded i (TypeVar _ k) expected
  | k == expected = pure ()
  | otherwise =
    throwTypecheckError $ ExpectedKind expected k i


unifyTyVar
  :: Located i (TCTypeVar s)
  -> Located i (TCType s)
  -> InferM s b i ()
unifyTyVar locTv@(Located li tv) locTy@(Located ri t1) = do
  ensureWellKinded li tv TyKind
  readTvRef tv >>= \case
    Unbound{} -> do
      occurs locTv locTy
      writeTvRef tv (LinkTy t1)
    Bound{} -> do
      t1' <- _dbgType t1
      t2' <- _dbgType $ TyVar tv
      throwTypecheckError $ UnificationFailure (Located li t1') (Located ri t2')
    LinkTy tyLink -> unify (Located li tyLink) locTy
    _ -> wellKindedGuaranteed

unifyRowVar
  :: Located i (TCTypeVar s)
  -> Located i (TCRowCtor s)
  -> InferM s b i ()
unifyRowVar loc@(Located i tv) locRow@(Located _ row) = do
  ensureWellKinded i tv RowKind
  readTvRef tv >>= \case
    Unbound{} -> do
      occursRow loc locRow
      writeTvRef tv (LinkRow row)
    LinkRow linkedRow -> do
      unifyRow (Located i linkedRow) locRow
    Bound n _ ->
      throwTypecheckError $ CannotUnifyWithBoundVariable n i
    _ -> wellKindedGuaranteed

-- | Solve the constraint `t1 ~ t2`
--   Since we use mutable type variables, `unify` simply returns unit.
--   Unification of a type variable with a
--
--   We've got to ensure we thread the right info from the `Located` instance whenver
--   we do any sort of recursive call for better error messages.
unify
  :: (Located i (TCType s))
  -> (Located i (TCType s))
  -> InferM s b i ()
unify lloc@(Located li lt) rloc@(Located ri rt) = unify' lt rt
  where
  unify' t1 t2 | t1 == t2 = pure ()
  unify' (TyVar tv) _ = unifyTyVar (Located li tv) rloc
  unify' _ (TyVar tv)  = unifyTyVar (Located ri tv) lloc
  unify' (TyFun l r) (TyFun l' r') =
    unify (Located li l) (Located ri l') *> unify (Located li r) (Located ri r')
  unify' (TyList t) (TyList t') = unify (Located li t) (Located li t')
  unify' (TyPrim p) (TyPrim p') | p == p' = pure ()
  unify' TyCapToken TyCapToken = pure ()
  unify' (TyObject r) (TyObject l) = unifyRow (Located li l) (Located ri r)
  unify' (TyTable r) (TyTable l) = unifyRow (Located li l) (Located ri r)
  unify' (TyNullary l) (TyNullary r) = unify (Located li l) (Located ri r)
  unify' (TyModRef mr) (TyModRef mr')
    | mr == mr' = pure ()
    | otherwise = unifyModRef (Located li mr) (Located ri mr')
  unify' t1 t2 = do
    t1' <- _dbgType t1
    t2' <- _dbgType t2
    throwTypecheckError (UnificationFailure (Located li t1') (Located ri t2'))

unifyModRef
  :: Located i (MRef (TCTypeVar s))
  -> Located i (MRef (TCTypeVar s))
  -> InferM s b i ()
unifyModRef _locl@(Located li lm) _locr@(Located ri rm) =
  unifyMRef' lm rm
  where
  unifyMRef' (MConcrete l) (MConcrete r)
    | l == r = pure ()
    | otherwise = do
    t1' <- _dbgType (TyModRef lm)
    t2' <- _dbgType (TyModRef rm)
    throwTypecheckError (UnificationFailure (Located li t1') (Located ri t2'))
  unifyMRef' (MRefVar n) m = unifyMRefVar (Located li n) (Located ri m)
  unifyMRef' m (MRefVar n) = unifyMRefVar (Located ri n) (Located li m)

unifyMRefVar
  :: Located i (TCTypeVar s)
  -> Located i (MRef (TCTypeVar s))
  -> InferM s b i ()
unifyMRefVar loc@(Located i tv) loMRef@(Located _ ref) = do
  ensureWellKinded i tv ModRefKind
  readTvRef tv >>= \case
    Unbound{} -> do
      occursRef loc loMRef
      writeTvRef tv (LinkRef ref)
    LinkRef linkedRef -> do
      unifyModRef (Located i linkedRef) loMRef
    Bound n _ ->
      throwTypecheckError $ CannotUnifyWithBoundVariable n i
    _ -> wellKindedGuaranteed

unifyRow
  :: Located i (TCRowCtor s)
  -> Located i (TCRowCtor s)
  -> InferM s b i ()
unifyRow locRowL@(Located li lrow) locRowR@(Located ri rrow) =
  unifyRow' lrow rrow
  where
  unifyRow' (RowVar tv) _ =
    unifyRowVar (Located li tv) locRowR
  unifyRow' _ (RowVar tv) =
    unifyRowVar (Located ri tv) locRowL
  unifyRow' (RowConcrete lrow') (RowConcrete rrow') = do
    when (M.keys lrow' /= M.keys rrow') $ do
      l' <- _dbgRowCtor lrow
      r' <- _dbgRowCtor rrow
      throwTypecheckError (RowUnificationFailure (Located li l') (Located ri r'))
    () <$ zipWithM (\lty rty -> unify (Located li lty) (Located ri rty)) (M.elems lrow') (M.elems rrow')

-- | Traverse a type and normalize all `LinkTy` and `LinkRow`s
traverseTypeLinks :: TCType s -> InferM s b i (TCType s)
traverseTypeLinks = \case
  TyVar rv -> readTvRef rv >>= \case
    LinkTy tl -> traverseTypeLinks tl
    _ -> pure (TyVar rv)
  TyFun l r ->
    TyFun <$> traverseTypeLinks l <*> traverseTypeLinks r
  TyNullary t ->
    TyNullary <$> traverseTypeLinks t
  TyPrim p -> pure (TyPrim p)
  TyList t ->
    TyList <$> traverseTypeLinks t
  TyObject rv -> TyObject <$> traverseRowLinks rv
  TyTable rv -> TyTable <$> traverseRowLinks rv
  TyModRef mref -> TyModRef <$> linkMRef mref
  TyCapToken -> pure TyCapToken
  where
  linkMRef = \case
    MRefVar rv -> readTvRef rv >>= \case
      LinkRef ref -> linkMRef ref
      _ -> pure (MRefVar rv)
    MConcrete mv -> pure (MConcrete mv)


traverseRowLinks :: RowTy (TCTypeVar s) -> InferM s b i (RowTy (TCTypeVar s))
traverseRowLinks = \case
  RowVar rv -> readTvRef rv >>= \case
    LinkRow row -> traverseRowLinks row
    _ -> pure (RowVar rv)
  RowConcrete rv -> pure (RowConcrete rv)

traverseTCLinks :: BuiltinTC (TCTypeVar s) -> InferM s b i (BuiltinTC (TCTypeVar s))
traverseTCLinks = \case
  Eq t -> Eq <$> traverseTypeLinks t
  Ord t -> Ord <$> traverseTypeLinks t
  Show t -> Show <$> traverseTypeLinks t
  Add t -> Add <$> traverseTypeLinks t
  Num t -> Num <$> traverseTypeLinks t
  ListLike t -> ListLike <$> traverseTypeLinks t
  Fractional t -> Fractional <$> traverseTypeLinks t
  EnforceRead t -> EnforceRead <$> traverseTypeLinks t
  IsValue t -> IsValue <$> traverseTypeLinks t
  RoseSubRow l r ->
    RoseSubRow <$> traverseRoseLinks l <*> traverseRoseLinks r
  RoseRowEq l r ->
    RoseRowEq <$> traverseRoseLinks l <*> traverseRoseLinks r
  where
  traverseRoseLinks (RoseRowTy n) =
    RoseRowTy <$> traverseRowLinks n
  traverseRoseLinks (RoseRowCat l r) =
    RoseRowCat <$> traverseRoseLinks l <*> traverseRoseLinks r

-- | We will only generalize on syntactic "values"
generalizeWithTerm
  :: TCType s
  -> [TCPred i s]
  -> TCTerm s b i
  -> InferM s b i (TypeScheme (TCTypeVar s), TCTerm s b i, [TCPred i s])
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
    preds' <- reduce (view Typed.termInfo term) preds
    (ty', (uniques, reverse -> ftvs)) <- runStateT (generalizeOnType ty) (S.empty, [])
    (deferred, retained) <- split preds' (view Typed.termInfo term)
    (retained', (uniques', _)) <- runStateT (traverse genPred retained) (uniques, [])
    -- Todo: better error
    when (uniques /= uniques') $ error "Invariant: type in preds that's not in the return type"
    case (ftvs, retained') of
      ([], []) -> do
        pure (TypeScheme [] [] ty' , term, deferred)
      ([], d) -> do
        d' <- traverse _dbgPred d
        error $ "retained predicates despite no generalized type variables " <> show (_locElem <$> d')
      (x:xs, (fmap _locElem -> p)) -> do
        pure (TypeScheme ftvs p ty', Typed.TyAbs (x:|xs) p term info, deferred)
    where
    nubPreds li = do
      nubBy (\l r -> _locElem l == _locElem r) <$> (traverse.locElem.traverseTCType) traverseTypeLinks li
    info = term ^. Typed.termInfo
    genPred (Located i p) = do
      Located i <$> traverseTCType generalizeOnType p
    generalizeOnType = (lift . traverseTypeLinks) >=> generalizeOnType'
    generalizeOnType' = \case
      TyVar tv -> lift (readTvRef tv) >>= \case
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
          when (l > cl) $ do
            lift (writeTvRef tv (Bound n u))
            (uniques, ftvs) <- get
            when (S.notMember u uniques) $ put (S.insert u uniques, tv:ftvs)
          pure $ RowVar tv
        _ -> pure (RowVar tv)
      RowConcrete r ->
        RowConcrete <$> traverse generalizeOnType' r

-- | Statically check the output "Yield type" in a best effort manner.
-- Yield type emitted by both branches, check normally
checkYieldTys :: Maybe (Located i (TCType s)) -> Maybe (Located i (TCType s)) -> Maybe (Located i (TCType s)) -> InferM s b i ()
checkYieldTys Nothing (Just l) (Just r) = do
  unify l r
  tcYieldTy .= Just r
-- Yield type might have changed in branches! We can discard the old yieldType
checkYieldTys (Just _) (Just m) (Just r) = do
  unify m r
  tcYieldTy .= Just r
-- Only right branch changed the yield type, so we will try and unify
checkYieldTys (Just l) _ (Just r) = do
  unify l r
  tcYieldTy .= Just r
-- Only left branch changed the yield type, so we will try and unnify
checkYieldTys (Just l) (Just r) _ = do
  unify l r
  tcYieldTy .= Just r
-- Yield type was not set in old branches
checkYieldTys (Just r) Nothing Nothing =
  tcYieldTy .= Just r
checkYieldTys _ _ _ = pure ()

getAndResetYieldTy :: InferM s b i (Maybe (Located i (TCType s)))
getAndResetYieldTy = do
  ty <- use tcYieldTy
  tcYieldTy .= Nothing
  pure ty

checkTermType
  :: (TypeOfBuiltin b)
  => Located i (TCType s)
  -> IR.Term Name IR.Type b i
  -> InferM s b i (Located i (TCType s), TCTerm s b i, [TCPred i s])
checkTermType checkty = \case
  IR.Var irn@(Name n nk) i -> case nk of
    NBound u -> do
      views tcVarEnv (`RAList.lookup` u) >>= \case
        Just nty -> do
          let newVar = Var irn i
          (rty, term', preds) <- instantiateWithTerm i nty newVar
          unify checkty (Located i rty)
          pure (Located i rty, term', preds)
        Nothing ->
          throwTypecheckError $ InvariantUnboundTermVariable n i
    NTopLevel mn mh -> do
      let fqn = FullyQualifiedName mn n mh
      nty <- lookupDefnType i fqn
      (rty, tyvars, preds) <- instantiateImported i nty
      unify checkty (Located i rty)
      let newVar = Var irn i
      case tyvars of
        [] -> pure (Located i rty, newVar, preds)
        (x:xs) -> pure (Located i rty, TyApp newVar (TyAppVar x :| fmap TyAppVar xs) i, preds)
    -- This rule essentially adds a sort of subsumption, which we will call
    -- "modref subsumption", and the entire reason why our
    -- typechecker is bidirectional in the first place (Seriously!!!)
    --
    -- Module references need to be able to operate where "weaker" bounds are required, but
    -- not when stronger ones are. That is:
    -- a `module{iface1, iface2}` may be used wherever a `module{iface1}` is required
    -- a `module{iface1}` may NOT be used wherever a `module{iface1, iface2}` is required
    --
    -- This means that modrefs do not play nice without type annotations or the assistance of
    -- bidirectionality.
    -- Code such as (lambda (m) (m::f)) will not infer to a type if that's the whole expression,
    -- but `(map (lambda (m) (m::f)) [k])` should
    NModRef m ifs -> case _locElem checkty of
      TyModRef (MConcrete mn) -> do
        let newVar = Var irn i
        let ifSet = S.fromList ifs
        if mn `S.isSubsetOf` ifSet  then
          pure (Located i (TyModRef (MConcrete mn)), newVar, [])
        else
          throwTypecheckError $ ModuleLacksImplementedInterfaces m (ifSet `S.difference` mn) i
      _ -> do
          let implInterfacesSet = S.fromList ifs
          unify checkty (Located i (TyModRef (MConcrete implInterfacesSet)))
          pure (Located i (TyModRef (MConcrete implInterfacesSet)), Var irn i, [])
    NDynRef (DynamicRef fnToCall r) -> do
      views tcVarEnv (`RAList.lookup` r) >>= traverse traverseTSLink >>= \case
        Just (TyModRef (MConcrete mrefs)) -> do
          traverse_ checkInterfaceWasTypechecked mrefs
          (Located i' ty) <- over locElem liftType <$> findSig (S.toList mrefs)
          unify checkty (Located i' ty)
          let newVar = Var irn i
          pure (Located i ty, newVar, [])
          where
          findSig [] = throwTypecheckError $ CannotDetermineDynamicInvoke irn mrefs fnToCall i
          findSig (m:ms) = uses tcInterfaceDefns (M.lookup (QualifiedName fnToCall m)) >>= \case
            Just fn -> pure fn
            Nothing -> findSig ms
        Just _ty -> do
          _ty' <- _dbgType _ty
          throwTypecheckError $ CannotInferTypeAsModRef (Located i _ty')
        Nothing ->
          throwTypecheckError $ InvariantUnboundTermVariable n i
      where
      traverseTSLink (TypeScheme _ _ ty) = traverseTypeLinks ty
  (IR.Lam nts body i) -> do
    nts' <- traverse argToTypedArg nts
    checkFun nts' (_locElem checkty) (NE.toList nts') []
    where
    checkFun nts' (TyFun argTy ret) (p:ps) acc = do
      unify (Located (_locLocation checkty) argTy) (Located (_targInfo p) (_targType p))
      checkFun nts' ret ps (NonGeneric argTy:acc)
    -- We're out of lambda arguments, so they've been all popped in the env
    -- Straightforward checking case
    checkFun nts' ret [] acc = do
      (ty, body', preds) <- locally tcVarEnv (RAList.fromList acc RAList.++) $ checkTermType (ret <$ checkty) body
      let lam' = Lam (NE.toList nts') body' i
          rtype = foldr TyFun (_locElem ty) (_targType <$> nts')
      pure (Located i rtype, lam', preds)
    -- This case is more interesting.
    -- It's possible that the `checkty` is not a `TyFun`, possibly because it's the result of
    -- a fresh type variable introduced somewhere, but it may happen at any point along the function, so
    -- in this case we fall back into "inference mode" for the rest of the lambda arguments
    checkFun nts' cty remainingArgs acc = do
      let env' = RAList.fromList $ (NonGeneric . _targType <$> reverse remainingArgs) ++ acc
      (ty, body', preds) <- locally tcVarEnv (env' RAList.++) $ inferTerm body
      unify (cty <$ checkty) (Located (_locLocation ty) (foldr TyFun (_locElem ty) (_targType <$> remainingArgs)))
      let lam' = Lam (NE.toList nts') body' i
          rtype = foldr TyFun (_locElem ty) (_targType <$> nts')
      pure (Located i rtype, lam', preds)
  IR.Let (Arg txt m_ty largInfo) e1 e2 i -> do
    (locTye1, e1', pe1) <- case m_ty of
        Just lty -> do
          lty' <- liftCoreType largInfo lty
          checkTermType (Located largInfo lty') e1
        Nothing -> inferTerm e1
    let letArgTy = NonGeneric (_locElem locTye1)
    (te2, e2', pe2) <- locally tcVarEnv (RAList.cons letArgTy) $ checkTermType checkty e2
    let term' = Let (TypedArg txt (_locElem locTye1) largInfo) e1' e2' i
    pure (te2, term', pe1 ++ pe2)
  term@(IR.App _ _ _i) -> do
    (termTy, term', preds) <- inferTerm term
    unify termTy checkty
    pure (termTy, term', preds)
  IR.Sequence l r i -> do
    (_, l', pl) <- inferTerm l
    (ty, r', pr) <- checkTermType checkty r
    pure (ty, Sequence l' r' i, pl ++ pr)
  IR.BuiltinForm cond i -> over _2 (`BuiltinForm` i) <$>
    case cond of
      CAnd e1 e2 -> do
        unify checkty (Located i TyBool)
        (_, e1', pe1) <- checkTermType (Located i TyBool) e1
        (_, e2', pe2) <- checkTermType (Located i TyBool) e2
        pure (Located i TyBool, CAnd e1' e2', pe1 ++ pe2)
      COr e1 e2 -> do
        unify checkty (Located i TyBool)
        (_, e1', pe1) <- checkTermType (Located i TyBool) e1
        (_, e2', pe2) <- checkTermType (Located i TyBool) e2
        pure (Located i TyBool, COr e1' e2', pe1 ++ pe2)
      CIf c e1 e2 -> do
        (_, c', pc) <- checkTermType (Located i TyBool) c
        oldYield <- use tcYieldTy
        (te1, e1', pe1) <- checkTermType checkty e1
        yieldIfBranch <- getAndResetYieldTy
        tcYieldTy .= oldYield
        (te2, e2', pe2) <- checkTermType checkty e2
        yieldElseBranch <- getAndResetYieldTy
        unify te1 te2
        checkYieldTys oldYield yieldIfBranch yieldElseBranch
        let tyLoc = Located i (_locElem te1)
        pure (tyLoc, CIf c' e1' e2', pc ++ pe1 ++ pe2)
      CEnforce bExpr strExpr -> do
        unify (Located i TyBool) checkty
        (_, bExpr', pe1) <- checkTermType (Located i TyBool)  bExpr
        (_, strExpr', pe2) <- checkTermType (Located i TyString) strExpr
        pure (Located i TyBool, CEnforce bExpr' strExpr', pe1 ++ pe2)
      CEnforceOne o li -> do
        unify (Located i TyBool) checkty
        (_, o', pe1) <- checkTermType (Located i TyString) o
        (_, li', pe2) <- checkTermType (Located i (TyList TyBool)) li
        pure (Located i TyBool, CEnforceOne o' li', pe1 ++ pe2)
      CTry catchE tryE -> do
        (_, catchE', pe1) <- checkTermType checkty catchE
        (_, tryE', pe2) <- checkTermType checkty tryE
        pure (Located i (_locElem checkty), CTry catchE' tryE', pe1 ++ pe2)
      CWithCapability ct body -> do
        (_, ct', pe1) <- checkTermType (Located i TyCapToken) ct
        (_, body', pe2) <- checkTermType checkty body
        pure (Located i (_locElem checkty), CWithCapability ct' body', pe1 ++ pe2)
      CNonReentrant term -> do
        (outTy, outTerm, outPreds) <- checkTermType checkty term
        pure (outTy, CNonReentrant outTerm, outPreds)
      CCreateUserGuard c -> case c of
        IR.App{} -> do
          unify checkty (Located i TyGuard)
          (t, c', pe1) <- inferTerm c
          -- Note: The `isValue` constraint here means that the type is most certainly
          -- not a partial application
          pure (Located i TyGuard, CCreateUserGuard c', (IsValue <$> t) : pe1)
        _ -> do
          throwTypecheckError $ UserGuardMustBeApp (renderCompactText c) i
  IR.Builtin b i -> do
    tyImported <- typeOfBuiltin i b
    (ty, tvs, preds) <- instantiateImported i tyImported
    unify checkty (Located i ty)
    let term = Builtin b i
    let term' = case TyAppVar <$> tvs of
            [] -> term
            (x:xs) -> TyApp term (x :| xs) i
    pure (Located i ty, term', preds)
  IR.Constant lit i -> do
      let ty = typeOfLit lit
      unify (Located i ty) checkty
      pure (Located i ty, Constant lit i, [])
  IR.ListLit tes i -> case _locElem checkty of
    TyList ty -> do
      liTup <- traverse (checkTermType (ty <$ checkty)) tes
      let preds = concat (view _3 <$> liTup)
          term' = ListLit ty (view _2 <$> liTup) i
      pure (Located i (TyList ty), term', preds)
    _ -> do
      tup <- inferTerm (IR.ListLit tes i)
      unify (view _1 tup) checkty
      pure tup
  IR.Nullary term i -> do
    t@(te', _, _) <- inferTerm (IR.Nullary term i)
    unify checkty te'
    pure t
  IR.ObjectLit fieldMap i -> do
    m <- traverse (\(f, t) -> (f,) <$> inferTerm t) fieldMap
    let objTyMap = views _1 _locElem <$> M.fromList m
        objTerms = over _2 (view _2) <$> m
        objTy = TyObject (RowConcrete objTyMap)
        isValuePreds = [ IsValue <$> ty | (_, (ty, _, _)) <- m]
        preds = concat (view (_2._3) <$> m)
    unify (Located i objTy) checkty
    pure (Located i objTy, ObjectLit objTerms i, isValuePreds ++ preds)
  IR.InlineValue _ _ -> error "todo: better error"


inferTerm
  :: (TypeOfBuiltin b)
  => IR.Term Name IR.Type b i
  -> InferM s b i (Located i (TCType s), TCTerm s b i, [TCPred i s])
inferTerm = \case
  IR.Var irn@(Name n nk) i -> case nk of
    NBound u -> do
      views tcVarEnv (`RAList.lookup` u) >>= \case
        Just nty -> do
          let newVar = Var irn i
          (rty, term', preds) <- instantiateWithTerm i nty newVar
          pure (Located i rty, term', preds)
        Nothing ->
          throwTypecheckError $ InvariantUnboundTermVariable n i
    NTopLevel mn mh -> do
      let fqn = FullyQualifiedName mn n mh
      nty <- lookupDefnType i fqn
      (rty, tyvars, preds) <- instantiateImported i nty
      let newVar = Var irn i
      case tyvars of
        [] -> pure (Located i rty, newVar, preds)
        (x:xs) -> pure (Located i rty, TyApp newVar (TyAppVar x :| fmap TyAppVar xs) i, preds)
    NModRef _ ifs -> do
        let v' = Var irn i
        pure (Located i (TyModRef (MConcrete (S.fromList ifs))), v', [])
    NDynRef (DynamicRef fnToCall r) -> do
      views tcVarEnv (`RAList.lookup` r) >>= traverse traverseTSLink >>= \case
        Just (TyModRef (MConcrete mrefs)) -> do
          traverse_ checkInterfaceWasTypechecked mrefs
          ty <- findSig (S.toList mrefs)
          let newVar = Var irn i
          pure (Located i (liftType (_locElem ty)), newVar, [])
          where
          findSig [] = throwTypecheckError $ CannotDetermineDynamicInvoke irn mrefs fnToCall i
          findSig (m:ms) = uses tcInterfaceDefns (M.lookup (QualifiedName fnToCall m)) >>= \case
            Just fn -> pure fn
            Nothing -> findSig ms
        Just _ty -> do
          _ty' <- _dbgType _ty
          throwTypecheckError $ CannotInferTypeAsModRef (Located i _ty')
        Nothing ->
          throwTypecheckError $ InvariantUnboundTermVariable n i
      where
      traverseTSLink (TypeScheme _ _ ty) = traverseTypeLinks ty
  IR.Lam nts e i -> do
    ntys <- traverse argToTypedArg nts
    let m = RAList.fromList (reverse (typedArgToTypeScheme <$> (NE.toList ntys)))
    (ty, e', preds) <- locally tcVarEnv (m RAList.++) $ inferTerm e
    let rty = foldr TyFun (_locElem ty) (_targType <$> ntys)
    pure (Located i rty, Lam (NE.toList ntys) e' i, preds)
  IR.App te apps i -> do
    (ty, retTerm, preds) <- inferApply (Apply te apps i)
    pure (ty, retTerm, preds)
  IR.Let (Arg n mty arg_info) e1 e2 i -> do
    enterLevel
    (te1, e1', pe1) <- case mty of
      Nothing -> inferTerm e1
      Just ty -> do
        ty' <- liftCoreType arg_info ty
        checkTermType (Located arg_info ty') e1
    leaveLevel
    let tsArg = TypeScheme [] [] (_locElem te1)
    (te2, e2', pe2) <- locally tcVarEnv (RAList.cons tsArg) $ inferTerm e2
    pure (te2, Let (TypedArg n (_locElem te1) arg_info) e1' e2' i, pe1 ++ pe2)
  IR.Sequence e1 e2 i -> do
    (_, e1', pe1) <- inferTerm e1
    (te2, e2', pe2) <- inferTerm e2
    pure (te2, Sequence e1' e2' i, pe1 ++ pe2)
  IR.BuiltinForm cond i -> over _2 (`BuiltinForm` i) <$>
    case cond of
      CAnd e1 e2 -> do
        (_, e1', pe1) <- checkTermType (Located i TyBool) e1
        (_, e2', pe2) <- checkTermType (Located i TyBool) e2
        pure (Located i TyBool, CAnd e1' e2', pe1 ++ pe2)
      COr e1 e2 -> do
        (_, e1', pe1) <- checkTermType (Located i TyBool) e1
        (_, e2', pe2) <- checkTermType (Located i TyBool) e2
        pure (Located i TyBool, COr e1' e2', pe1 ++ pe2)
      CIf c e1 e2 -> do
        (_, c', pc) <- checkTermType (Located i TyBool) c
        oldYield <- use tcYieldTy
        (te1, e1', pe1) <- inferTerm e1
        yieldIfBranch <- getAndResetYieldTy
        tcYieldTy .= oldYield
        (te2, e2', pe2) <- inferTerm e2
        yieldElseBranch <- getAndResetYieldTy
        unify te1 te2
        checkYieldTys oldYield yieldIfBranch yieldElseBranch
        pure (Located i (_locElem te1), CIf c' e1' e2', pc ++ pe1 ++ pe2)
      CEnforce e str -> do
        (_, e', pe1) <- checkTermType (Located i TyBool) e
        (_, str', pe2) <- checkTermType (Located i TyString) str
        pure (Located i TyBool, CEnforce e' str', pe1 ++ pe2)
      CEnforceOne o li -> do
        (_, o', pe1) <- checkTermType (Located i TyString) o
        (_, li', pe2) <- checkTermType (Located i (TyList TyBool)) li
        pure (Located i TyBool, CEnforceOne o' li', pe1 ++ pe2)
      CTry catchE tryE -> do
        (tyCatch, catchE', pe1) <- inferTerm catchE
        (tyTry, tryE', pe2) <- inferTerm tryE
        unify tyCatch tyTry
        pure (tyCatch, CTry catchE' tryE', pe1 ++ pe2)
      CWithCapability ct body -> do
        (_, ct', pe1) <- checkTermType (Located i TyCapToken) ct
        (rty, body', pe2) <- inferTerm body
        pure (rty, CWithCapability ct' body', pe1 ++ pe2)
      CNonReentrant term -> do
        (outTy, outTerm, outPreds) <- inferTerm term
        pure (outTy, CNonReentrant outTerm, outPreds)
      CCreateUserGuard c -> case c of
        IR.App{} -> do
          (t, c', pe1) <- inferTerm c
          pure (Located i TyGuard, CCreateUserGuard c', (IsValue <$> t) : pe1)
        _ -> throwTypecheckError $ UserGuardMustBeApp (renderCompactText c) i
  IR.Builtin b i -> do
    tyImported <- typeOfBuiltin i b
    (ty, tvs, preds) <- instantiateImported i tyImported
    let term = Builtin b i
    let term' = case TyAppVar <$> tvs of
            [] -> term
            (x:xs) -> TyApp term (x :| xs) i
    pure (Located i ty, term', preds)
  IR.Constant lit i ->
      pure (Located i (typeOfLit lit), Constant lit i, [])
  IR.ListLit li i -> do
    tv <- TyVar . (`TypeVar` TyKind) <$> newTvRef
    liTup <- traverse (checkTermType (Located i tv)) li
    let preds = concat (view _3 <$> liTup)
    pure (Located i (TyList tv), ListLit tv (view _2 <$> liTup) i, preds)
  IR.Nullary e i -> do
    (te', e', p) <- inferTerm e
    pure (TyNullary <$> te', Lam [] e' i, p)
  IR.ObjectLit fieldMap i -> do
    m <- (traverse._2) inferTerm fieldMap
    let objTyMap = views _1 _locElem <$> M.fromList m
        objTerms = over _2 (view _2) <$> m
        objTy = TyObject (RowConcrete objTyMap)
        isValuePreds = [ IsValue <$> ty | (_, (ty, _, _)) <- m]
        preds = concat (view (_2._3) <$> m)
    pure (Located i objTy, ObjectLit objTerms i, isValuePreds ++ preds)
  IR.InlineValue{} -> error "InlineValue not supported"

inferApply
  :: TypeOfBuiltin b
  => Apply (IR.EvalTerm b i) i
  -> InferM s b i (Located i (TCType s), TCTerm s b i, [TCPred i s])
inferApply (Apply (IR.Builtin (toSpecialOverload -> Just b) bi) args i) = do
  inferCoreBuiltinOverload i (b, bi) args
inferApply (Apply fun args i) = case args of
  [] -> do
    tv1 <- TyVar . (`TypeVar` TyKind) <$> newTvRef
    (tfun, fun', pe1) <- inferTerm fun
    unify (Located i (TyNullary tv1)) tfun
    pure (Located i tv1, App fun' [] i, pe1)
  h:hs -> do
    (tfun, te', pe1) <- inferTerm fun
    (rty, xs, ps) <- foldlM (inferFunctionArgs i) (tfun,[], []) (h:hs)
    let term' = App te' (reverse xs) i
    pure (rty, term', pe1 ++ ps)

inferFunctionArgs
  :: TypeOfBuiltin b
  => i
  -> (Located i (Type (TCTypeVar s)), [TCTerm s b i], [TCPred i s])
  -> IR.Term Name IR.Type b i
  -> InferM s b i (Located i (Type (TCTypeVar s)), [TCTerm s b i], [TCPred i s])
inferFunctionArgs appInfo (ta, xs, ps) fnArg = case _locElem ta of
  TyFun arg ret -> do
    (_, x', p) <- checkTermType (arg <$ ta) fnArg
    pure (ret <$ ta, x':xs, ps ++ p)
  _ -> do
    tv1 <- TyVar . (`TypeVar` TyKind) <$> newTvRef
    (Located iterm tArg, fnArg', predsArg) <- inferTerm fnArg
    unify ta (Located iterm (tArg :~> tv1))
    pure (Located appInfo tv1,fnArg':xs,ps ++ predsArg)

pattern LitZKG1 :: IR.Term n t b i
pattern LitZKG1 <- IR.Constant (LString "g1") _

zkG1Schema :: M.Map Field (Type n)
zkG1Schema = M.fromList
  [ (Field "x", TyInt)
  , (Field "y", TyInt)]

pattern LitZKG2 :: IR.Term n t b i
pattern LitZKG2 <- IR.Constant (LString "g2") _

zkG2Schema :: M.Map Field (Type n)
zkG2Schema = M.fromList
  [ (Field "x", TyList TyInt)
  , (Field "y", TyList TyInt)]


zkSchemaFromGroup :: ZKGroup -> Map Field (Type n)
zkSchemaFromGroup ZKG1 = zkG1Schema
zkSchemaFromGroup ZKG2 = zkG2Schema

inferCoreBuiltinOverload
  :: TypeOfBuiltin b
  => i
  -> (SpecialOverload, i)
  -> [IR.EvalTerm b i]
  -> InferM s b i (Located i (TCType s), TCTerm s b i, [TCPred i s])
inferCoreBuiltinOverload info (b, bi) args = case b of
  -- (format k [args])
  -- format forms outside of this will not infer unless they are all strings
  FormatOverload | [x, IR.ListLit lis _li] <- args -> do
    (ty, x', p1) <- inferTerm x
    unify ty (Located info TyString)
    inferred <- traverse inferTerm lis
    let constrs = concat [ (Show <$> t):preds | (t, _, preds) <- inferred ]
    let li' = view _2 <$> inferred
    let retTerm = Typed.Format x' li' info
    pure (Located info TyString, retTerm, p1 ++ constrs)
  -- (at "f")
  AccessOverload | (IR.Constant (LString field) _:xs) <- args -> do
    (ty, tvs, preds) <- instantiateImported bi (objAccessType (Field field))
    (rty, xs', ps) <- foldlM (inferFunctionArgs info) (Located bi ty, [], []) xs
    let term' = applyTypeVars (ObjectOp (ObjAccess (Field field)) info) bi tvs
    pure (rty, Typed.App term' (reverse xs') info, preds ++ ps)
  RemoveOverload | (IR.Constant (LString field) _:xs) <- args -> do
    (ty, tvs, preds) <- instantiateImported bi (objRemoveType (Field field))
    (rty, xs', ps) <- foldlM (inferFunctionArgs info) (Located bi ty, [], []) xs
    let term' = applyTypeVars (ObjectOp (ObjAccess (Field field)) info) bi tvs
    pure (rty, Typed.App term' (reverse xs') info, preds ++ ps)
  WhereOverload | (IR.Constant (LString field) _:xs) <- args -> do
    (ty, tvs, preds) <- instantiateImported bi (objWhereType (Field field))
    (rty, xs', ps) <- foldlM (inferFunctionArgs info) (Located bi ty, [], []) xs
    let term' = applyTypeVars (ObjectOp (ObjWhere (Field field)) info) bi tvs
    pure (rty, Typed.App term' (reverse xs') info, preds ++ ps)
  YieldOverload | [_] <- args -> do
    (locrty, term', preds) <- inferFallback
    rty' <- traverseTypeLinks (_locElem locrty)
    inDefpact <- view tcInDefPact
    when inDefpact $ case rty' of
      TyObject{} -> do
        tcYieldTy .= Just (Located info rty')
      _ -> pure ()
    pure (rty' <$ locrty, term', preds)
  YieldToChainOverload -> do
    (locrty, term', preds) <- inferFallback
    rty' <- traverseTypeLinks (_locElem locrty)
    inDefpact <- view tcInDefPact
    when inDefpact $ case rty' of
      TyObject{} -> do
        tcYieldTy .= Just (Located info rty')
      _ -> pure ()
    pure (rty' <$ locrty, term', preds)
  ResumeOverload | [_] <- args -> do
    use tcYieldTy >>= \case
      Just (Located yield_i yield_ty) -> do
        tv <- TyVar <$> newTypeVar TyKind
        let funTy = (yield_ty :~> tv) :~> tv
        (rty', xs, ps) <- foldlM (inferFunctionArgs info) (Located yield_i funTy, [], []) args
        pure (rty', App (Builtin (fromSpecialOverload b) bi) (reverse xs) info, ps)
      Nothing ->
        throwTypecheckError $ CannotStaticallyDetermineRowOpSig "resume" info
  ContainsOverload | [x, y] <- args -> do
    (t1, x', px) <- inferTerm x
    (t2, y', py) <- inferTerm y
    (tyAdd, tvs, preds)  <- case (t1, t2) of
      (Located _ TyString{}, Located _ TyString{}) ->
        pure (TyString :~> TyString :~> TyBool, [], [])
      _ -> do
        ty <- typeOfBuiltin bi CoreContains
        instantiateImported bi ty
    tv <- TyVar <$> newTypeVar TyKind
    unify (Located bi tyAdd) (Located info (_locElem t1 :~> _locElem t2 :~> tv))
    -- TODO: this needs to be turned into a special form for sure
    let retTerm = Typed.App (applyTypeVars (Typed.Builtin (review _CoreBuiltin CoreContains) bi) bi tvs)  [x', y'] info
    pure (Located info tv, retTerm, px ++ py ++ preds)
  SortObjOverload | (IR.ListLit li _:hs) <- args, Just fields <- traverse _stringLit li -> do
    (ty, tvs, preds) <- instantiateImported bi (sortObjType fields)
    (rty, xs', ps) <- foldlM (inferFunctionArgs info) (Located bi ty, [], []) hs
    let term = applyTypeVars (ObjectOp (SortObj fields) bi) bi tvs
    pure (rty, Typed.App term (reverse xs') info, preds ++ ps)
  SelectWithFieldsOverload | (h:IR.ListLit li _:hs) <- args, Just fields <- traverse _stringLit li -> do
    (ty, tvs, preds) <- instantiateImported bi (selectWithFieldsType fields)
    (rty, xs', ps) <- foldlM (inferFunctionArgs info) (Located bi ty, [], []) (h:hs)
    let term = applyTypeVars (ObjectOp (SelectObj fields) bi) bi tvs
    pure (rty, Typed.App term (reverse xs') info, preds ++ ps)

  ReadFieldsOverload | [tbl, k, IR.ListLit li _] <- args, Just fields <- traverse _stringLit li, not (null fields) -> do
    rowVar <- newTypeVar RowKind
    (_, tbl', ptbl) <- checkTermType (Located bi (TyTable (RowVar rowVar))) tbl
    (_, k', pk) <- checkTermType (Located bi TyString) k
    fieldTyVars <- traverse (const (newTypeVar TyKind)) fields
    let outTySchema = M.fromList (zip fields (TyVar <$> fieldTyVars))
    let constr = RoseSubRow (RoseConcrete outTySchema) (RoseVar rowVar)
    let term = applyTypeVars (ObjectOp (ReadObj fields) bi) bi fieldTyVars
    pure (Located info (TyObject (RowConcrete outTySchema)), Typed.App term [tbl', k'] info, ptbl ++ pk ++ [Located bi constr])

  ZkPointAddOverload | LitZKG1:hs <- args -> doPointAdd ZKG1 hs
  ZkPointAddOverload | LitZKG2:hs <- args -> doPointAdd ZKG2 hs

  ZkScalarMulOverload | LitZKG1:hs <- args -> doScalarMult ZKG1 hs
  ZkScalarMulOverload | LitZKG2:hs <- args -> doScalarMult ZKG2 hs

  AddOverload | [x, y] <- args -> do
    (t1, x', px) <- inferTerm x
    (t2, y', py) <- inferTerm y
    (tyAdd, tvs, preds)  <- case (t1, t2) of
      (Located _ TyObject{}, _) -> instantiateImported bi addObjType
      (_, Located _ TyObject{}) -> instantiateImported bi addObjType
      _ -> do
        ty <- typeOfBuiltin bi CoreAdd
        instantiateImported bi ty
    tv <- TyVar <$> newTypeVar TyKind
    unify (Located bi tyAdd) (Located info (_locElem t1 :~> _locElem t2 :~> tv))
    -- TODO: this needs to be turned into a special form for sure
    let retTerm = Typed.App (applyTypeVars (Typed.Builtin (review _CoreBuiltin CoreAdd) bi) bi tvs)  [x', y'] info
    pure (Located info tv, retTerm, px ++ py ++ preds)
  _ -> inferFallback
  where
  _stringLit (IR.Constant (LString f) _) = Just (Field f)
  _stringLit _ = Nothing
  inferFallback = do
    let b' = fromSpecialOverload b
    builtinTyScheme <- typeOfBuiltin bi b'
    (ty, tvs, preds) <- instantiateImported bi builtinTyScheme
    (rty, xs', ps) <- foldlM (inferFunctionArgs info) (Located bi ty, [], []) args
    let term = applyTypeVars (Builtin b' bi) bi tvs
    pure (rty, Typed.App term (reverse xs') info, preds ++ ps)
  sortObjType fields =
    let vars = [TypeVariable (fromIntegral k) ("a_" <> T.pack (show k)) | k <- [0.. length fields - 1]]
        row = RowVariable (fromIntegral (length fields)) "r"
        mty = M.fromList $ zip fields (TyVar <$> vars)
        constr = RoseSubRow (RoseConcrete mty) (RoseVar row)
    in TypeScheme (row : reverse vars) [constr] (TyList (TyObject (RowVar row)) :~> TyList (TyObject (RowVar row)))
  selectWithFieldsType fields =
    let vars = [TypeVariable (fromIntegral k) ("a_" <> T.pack (show k)) | k <- [0.. length fields - 1]]
        row = RowVariable (fromIntegral (length fields)) "r"
        mty = M.fromList $ zip fields (TyVar <$> vars)
        constr = RoseSubRow (RoseConcrete mty) (RoseVar row)
    in TypeScheme (row : reverse vars) [constr] $ TyTable (RowVar row) :~> (TyObject (RowVar row) :~> TyBool) :~> TyList (TyObject (RowConcrete mty))
  objAccessType field =
    let z1 = RowVariable 1 "z"
        tfield = TypeVariable 0 "t"
        concreteRow = M.singleton field (TyVar tfield)
        constr = RoseSubRow (RoseRowTy (RowConcrete concreteRow)) (RoseVar z1)
    in TypeScheme [z1, tfield] [constr] (TyObject (RowVar z1) :~> TyVar tfield)

  objRemoveType field =
    let z1 = RowVariable 2 "z1"
        z2 = RowVariable 1 "z2"
        tfield = TypeVariable 0 "t"
        concreteRow = M.singleton field (TyVar tfield)
        constrL = RoseRowCat (RoseVar z2) (RoseConcrete concreteRow)
        constr = RoseRowEq constrL (RoseRowTy (RowVar z1))
    in TypeScheme [z1, z2, tfield] [constr] (TyObject (RowVar z1) :~> TyObject (RowVar z2))

  objWhereType field =
    let z1 = RowVariable 1 "z"
        tfield = TypeVariable 0 "t"
        concreteRow = M.singleton field (TyVar tfield)
        constr = RoseSubRow (RoseRowTy (RowConcrete concreteRow)) (RoseVar z1)
    in TypeScheme [z1, tfield] [constr] ((TyVar tfield :~> TyBool) :~> TyObject (RowVar z1) :~> TyBool)

  addObjType =
    let z1 = RowVariable 2 "z1"
        z2 = RowVariable 1 "z2"
        z3 = RowVariable 0 "z3"
        constrL = RoseRowCat (RoseVar z1) (RoseVar z2)
        constrR = RoseRowTy (RowVar z3)
        constr = RoseRowEq constrL constrR
    in
      TypeScheme [z1, z2, z3] [constr] (TyObject (RowVar z1) :~> TyObject (RowVar z2) :~> TyObject (RowVar z3))

  scalarMulType scm =
    NonGeneric $ TyObject (RowConcrete scm) :~> TyInt :~> TyObject (RowConcrete scm)

  pointAddType scm =
    NonGeneric $ TyObject (RowConcrete scm) :~> TyObject (RowConcrete scm) :~> TyObject (RowConcrete scm)

  doPointAdd grp xs = do
    (ty, tvs, preds) <- instantiateImported bi (pointAddType (zkSchemaFromGroup grp))
    (rty, xs', ps) <- foldlM (inferFunctionArgs info) (Located bi ty, [], []) xs
    let term' = applyTypeVars (ObjectOp (PointAdd grp) info) bi tvs
    pure (rty, Typed.App term' (reverse xs') info, preds ++ ps)

  doScalarMult grp xs = do
    (ty, tvs, preds) <- instantiateImported bi (scalarMulType (zkSchemaFromGroup grp))
    (rty, xs', ps) <- foldlM (inferFunctionArgs info) (Located bi ty, [], []) xs
    let term' = applyTypeVars (ObjectOp (ScalarMult grp) info) bi tvs
    pure (rty, Typed.App term' (reverse xs') info, preds ++ ps)

applyTypeVars :: Term name tyname builtin info -> info -> [tyname] -> Term name tyname builtin info
applyTypeVars term i = \case
  [] -> term
  (x:xs) -> TyApp term (TyAppVar x :| (TyAppVar <$> xs)) i

checkInterfaceWasTypechecked :: ModuleName -> InferM s b i ()
checkInterfaceWasTypechecked mn = uses tcInferredInterfaces (M.lookup mn) >>= \case
  Nothing -> do
    iface <- getInterface mn
    () <$ inferInterface iface
  Just _ -> pure ()

argToTypedArg :: Arg IR.Type i -> InferM s b i (TypedArg (Type (TCTypeVar s)) i)
argToTypedArg (Arg n mty i) =
  case mty of
    Nothing -> do
      tv <- newTypeVar TyKind
      pure $ TypedArg n (TyVar tv) i
    Just ty -> do
      ty' <- liftCoreType i ty
      pure $ TypedArg n ty' i

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
  enterLevel
  let name = _argName spec
  typedArgs <- traverse argToTypedArg dfargs
  spec' <- argToTypedArg spec
  let dfTy = case typedArgs of
        [] -> TyNullary (_targType spec')
        _ -> foldr TyFun (_targType spec') (_targType <$> typedArgs)
  let m = RAList.fromList (reverse (typedArgToTypeScheme <$> typedArgs))
  -- We don't want the error reported to be the location of the entire lambda, which is the same
  -- location as the entire function, so we "peel" the enclosing lambda off here, but
  -- inject the arguments into the env, then we infer, and unify. This way, we get a better error location
  -- in the case that the unification a few lines below fails
  (termTy, rawTerm, preds) <- locally tcVarEnv (m RAList.++) $ inferTerm (unLam term)
  unify (Located (_targInfo spec') (_targType spec')) termTy
  leaveLevel
  -- Re-construct the term as a lambda
  let term' = Lam typedArgs rawTerm info


  (tys, tcterm, deferred) <- generalizeWithTerm dfTy preds term'
  deferred' <- reduce info deferred
  unless (null deferred') $ do
    deferredDbg <- traverse _dbgPred deferred'
    throwTypecheckError $ CannotResolveConstraints deferredDbg info
  debruijnizedTypeScheme <- debruijnizeTypeScheme info tys
  -- fterm <- noTyVarsinTerm info term'
  fterm <- debruijnizeTermTypes info tcterm
  let deleteArgTypes (TypedArg n _ i) = Arg n Nothing i
  tcFree %= M.insert (FullyQualifiedName mn name mh) (NotIndexed debruijnizedTypeScheme)
  pure (Defun name (deleteArgTypes <$> typedArgs) debruijnizedTypeScheme fterm info)
  where
  unLam (IR.Lam _ t _) = t
  unLam (IR.Nullary t _) = t
  unLam t = t

-- Defpacts will not be allowed to not
inferDefPact
  :: TypeOfBuiltin b
  => ModuleName
  -> ModuleHash
  -> IR.DefPact Name IR.Type b info
  -> InferM s b info (DefPact Name DebruijnTypeVar b info)
inferDefPact mn mh (IR.DefPact spec args steps info) = locally tcInDefPact (const True) $ do
  -- First, we ignore the defpact return type. There's really no point in it,
  -- it simply restricts what we can infer, and I suspect most defpacts will _not_ typecheck like this.
  args' <- traverse enforceArgType args
  let argtys = RAList.fromList (reverse (NonGeneric . liftType . _targType <$> args'))
  let stepsWithIx = NE.zip steps (NE.fromList [0..])
  steps' <- forM stepsWithIx $ \(step, i) -> case step of
    IR.Step term -> do
      (ty, term', preds) <-
        locally tcStepCtx (const (StepIndex i))
        $ locally tcVarEnv (argtys RAList.++)
        $ inferTerm term
      p <- reduce info preds
      ty' <- ensureNoTyVars (_locLocation ty) (_locElem ty)
      unless (null p) $ do
        p' <- traverse _dbgPred p
        throwTypecheckError $ CannotResolveConstraints p' info
      term'' <- debruijnizeTermTypes info term'
      pure (ty', Step term'', i)
    IR.StepWithRollback s rb -> do
      fstYieldTy <- use tcYieldTy
      (ty, s', preds) <-
        locally tcStepCtx (const (StepIndex i))
        $ locally tcVarEnv (argtys RAList.++)
        $ inferTerm s
      currYieldTy <- use tcYieldTy
      -- Ensure we set the yield type to whatever it was before the first step was evaluated
      tcYieldTy .= fstYieldTy
      (_, rb', preds') <- locally tcStepCtx (const (StepIndex i))
        $ locally tcVarEnv (argtys RAList.++)
        $ inferTerm rb
      p <- reduce info (preds ++ preds')
      ty' <- ensureNoTyVars (_locLocation ty) (_locElem ty)
      unless (null p) $ do
        p' <- traverse _dbgPred p
        throwTypecheckError $ CannotResolveConstraints p' info
      -- Restore the yield type to be the non-rollback yield type
      tcYieldTy .= currYieldTy
      s'' <- debruijnizeTermTypes info s'
      rb'' <- debruijnizeTermTypes info rb'
      pure (ty', StepWithRollback s'' rb'', i)
    _ -> error "LegacyStep forms are not supported, and cannot appear in the repl"
  let types = IntMap.fromList [(i, foldr TyFun ty (_targType <$> args')) | (ty, _, i) <- NE.toList steps']
  tcFree %= M.insert (FullyQualifiedName mn (_argName spec) mh) (IndexedDefpactStepType types)
  tcYieldTy .= Nothing
  pure (DefPact (_argName spec) args' types (view _2 <$> steps') info)


inferDefConst
  :: ModuleName
  -> ModuleHash
  -> IR.DefConst Name IR.Type b i
  -> InferM s b i (DefConst i)
inferDefConst mn mh (IR.DefConst spec cv info) = case cv of
  IR.EvaledConst v -> do
    let name = _argName spec
    pvt <- inferPactValue info v
    dcTy' <- traverse (liftCoreType info) (_argType spec)
    _ <- traverse (unify (Located info pvt) . Located info) dcTy'
    rty <- ensureNoTyVars info (maybe pvt id dcTy')
    tcFree %= M.insert (FullyQualifiedName mn name mh) (NotIndexed (NonGeneric (liftType rty)))
    pure (DefConst name rty v info)
  _ -> do
    throwTypecheckError $ InvariantDefconstNotEvaluated (FullyQualifiedName mn (_argName spec) mh) info

inferPactValue :: i -> PactValue -> InferM s b i (TCType s)
inferPactValue i = \case
  PLiteral l -> pure $ typeOfLit l
  PGuard _ -> pure $ TyGuard
  PObject o ->
    TyObject . RowConcrete <$>
      traverse (inferPactValue i) o
  PList l -> do
    v <- TyVar . (`TypeVar` TyKind) <$> newTvRef
    _ <- traverse (inferPactValue i >=> unify (Located i v) . Located i) l
    pure (TyList v)
  PModRef mr -> pure $ TyModRef (MConcrete (_mrImplemented mr))
  PCapToken _ ->
    pure $ TyCapToken
  PTable tbl -> do
    let (IR.Schema _ schema) = _tvSchema tbl
    schemaTys <- traverse (liftCoreType i) schema
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

  retArg <- argToTypedArg spec
  args' <- traverse argToTypedArg args
  let rty = _targType retArg
  let fnCapTy = case args of
        [] -> TyNullary rty
        _ -> foldr TyFun rty (_targType <$> args')
  let m = RAList.fromList (reverse (typedArgToTypeScheme <$> args'))
  (termTy, term', preds) <- locally tcVarEnv (m RAList.++) $ inferTerm term
  unify (Located (_targInfo retArg) rty) termTy

  leaveLevel
  -- Note: This can have some kind of gnarly consequences if for whatever reason there is a constraint that
  -- the return type requires.
  --
  -- However for defcaps, we ideally want to avoid this in the first place, so
  -- if this errors out here, that's likely a good thing, but we will need a good error
  -- in the future in case it does
  let tygen = set returnType TyCapToken fnCapTy

  (tys, tcterm, deferred) <- generalizeWithTerm tygen preds term'
  reduced <- reduce info deferred
  unless (null reduced) $ do
    deferred' <- traverse _dbgPred deferred
    throwTypecheckError $ CannotResolveConstraints deferred' info
  debruijnizedTypeScheme <- debruijnizeTypeScheme info tys
  fterm <- debruijnizeTermTypes info tcterm

  tcFree %= M.insert (FullyQualifiedName mn name mh) (NotIndexed debruijnizedTypeScheme)
  let deleteArgTypes (TypedArg n _ i) = Arg n Nothing i

  pure (DefCap name (deleteArgTypes <$> args') debruijnizedTypeScheme fterm meta info)

inferTable
  :: ModuleName
  -> ModuleHash
  -> IR.DefTable Name i
  -> InferM s b i (DefTable i)
inferTable mn mh (IR.DefTable tn (IR.ResolvedTable (IR.Schema qn schema)) info) = do
  schema' <- traverse (liftCoreType info) schema
  let rty = TyTable (RowConcrete (liftType <$> schema'))
  tcFree %= M.insert (FullyQualifiedName mn tn mh) (NotIndexed ((NonGeneric rty)))
  pure (DefTable tn (Schema qn schema') info)

inferDefSchema :: IR.DefSchema IR.Type info -> InferM s b info (DefSchema info)
inferDefSchema (IR.DefSchema n ds i) = DefSchema n <$> traverse (liftCoreType i) ds <*> pure i

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
  IR.DSchema s -> DSchema <$> (inferDefSchema s)
  IR.DPact dp -> DPact <$> inferDefPact mn mh dp

inferInterface
  :: IR.EvalInterface b i
  -> InferM s b i (Interface i)
inferInterface (IR.Interface ifn defns imps ifh iftxh _ifcode ifinfo) = do
  let defns' = flattenSCCs $ stronglyConnComp [ (d, IR.ifDefName d, ifDefDeps d) | d <- defns]
  defnsTypechecked <- traverse (inferIfDef ifn ifh) defns'
  let iface = Interface ifn defnsTypechecked imps ifh iftxh ifinfo
  tcInferredInterfaces %= M.insert ifn iface
  pure iface

enforceArgType :: Arg IR.Type i -> InferM s b i (TypedArg (Type Void) i)
enforceArgType (Arg n ty i) = case ty of
  Just ty' -> TypedArg n <$> liftCoreType i ty' <*> pure i
  Nothing -> throwTypecheckError $ ArgumentRequiresTypeAnnotation n i

inferIfDefun :: ModuleName -> IR.IfDefun IR.Type info -> InferM s b info (IfDefun info)
inferIfDefun mn (IR.IfDefun spec args info) = do
  spec' <- enforceArgType spec
  args' <- traverse enforceArgType args
  let rtype = case args' of
        [] -> TyNullary (_targType spec')
        _ -> foldr TyFun (_targType spec') (_targType <$> args')
  let defunQn = QualifiedName (_targName spec') mn
  tcInterfaceDefns %= M.insert defunQn (Located info rtype)
  pure $ IfDefun spec' args' rtype info

inferIfDefPact :: ModuleName -> IR.IfDefPact IR.Type info -> InferM s b info (IfDefPact info)
inferIfDefPact mn (IR.IfDefPact spec args info) = do
  spec' <- enforceArgType spec
  args' <- traverse enforceArgType args
  let rtype = case args' of
        [] -> TyNullary (_targType spec')
        _ -> foldr TyFun (_targType spec') (_targType <$> args')
  let defunQn = QualifiedName (_targName spec') mn
  tcInterfaceDefns %= M.insert defunQn (Located info rtype)
  pure $ IfDefPact spec' args' rtype info

inferIfDefcap :: ModuleName -> IR.IfDefCap Name IR.Type info -> InferM s b info (IfDefCap info)
inferIfDefcap mn (IR.IfDefCap spec args meta info) = do
  spec' <- enforceArgType spec
  args' <- traverse enforceArgType args
  case meta of
    -- Desugaring enforces `ix` is in bounds of `args`
    DefManaged (DefManagedMeta (idx, _) (BareName n)) -> do
      let argTy = _targType (args' !! idx)
      let expectedTy = argTy :~> argTy :~> argTy
      uses tcInterfaceDefns (M.lookup (QualifiedName n mn)) >>= \case
        Just (Located i ty) -> when (expectedTy /= ty) $
          throwTypecheckError $ InvalidDefcapManagerFun (Located info expectedTy) (Located i ty)
        Nothing -> error "inferIfDefcap: impossible case, guaranteed by desugar"
    _ -> pure ()
  let rtype = case args' of
        [] -> TyNullary TyCapToken
        _ -> foldr TyFun TyCapToken (_targType <$> args')
  let defunQn = QualifiedName (_targName spec') mn
  tcInterfaceDefns %= M.insert defunQn (Located info rtype)
  pure $ IfDefCap spec' args' rtype meta info

inferIfDef
  :: ModuleName
  -> ModuleHash
  -> IR.IfDef Name IR.Type b i
  -> InferM s b i (IfDef i)
inferIfDef mn mh = \case
  IR.IfDfun df -> IfDfun <$> inferIfDefun mn df
  IR.IfDCap dc -> IfDCap <$> inferIfDefcap mn dc
  IR.IfDConst dc -> IfDConst <$> inferDefConst mn mh dc
  IR.IfDSchema dc -> IfDSchema <$> inferDefSchema dc
  IR.IfDPact dp -> IfDPact <$> inferIfDefPact mn dp

inferModule
  :: TypeOfBuiltin b
  => IR.EvalModule b i
  -- ^ The module to infer
  -> M.Map FullyQualifiedName (IR.EvalDef b i)
  -- ^ The module's dependencies
  -> InferM s b i (TypedModule b i, Map FullyQualifiedName (DefnType (TypeVar NamedDeBruijn)))
inferModule (IR.Module mname mgov defs blessed imports impl mh _ _ info) deps = do
  let allDeps = M.toList $ M.fromList (toFqDep mname mh <$> defs) <> deps
  let sccInput = flattenSCCs $ stronglyConnCompR [ (defn, fqn, defFqns defn) | (fqn, defn) <- allDeps]
  defs' <- forM sccInput $ \(defn, fqn, _) -> do
    defn' <- inferDef (_fqModule fqn) (_fqHash fqn) defn
    pure (defn', fqn)
  let moduleDefs = fmap fst $ filter (\(_, fqn) -> _fqModule fqn == mname && _fqHash fqn == mh) defs'
  defns <- uses tcFree (M.filterWithKey (\k _ -> _fqModule k == mname && _fqHash k == mh))
  pure (Module mname mgov moduleDefs blessed imports impl mh info, defns)

getInterface :: ModuleName -> InferM s b i (IR.EvalInterface b i)
getInterface mn = views (tcLoaded.loModules) (M.lookup mn) >>= \case
  Just (InterfaceData iface _) -> pure iface
  _ -> error $ "Invariant: Expected interface " <> show mn

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
debruijnizeTermTypes
  :: i
  -> TCTerm s b i
  -> InferM s b i (Term Name DebruijnTypeVar b i)
debruijnizeTermTypes info = dbj [] 0
  where
  debruijnizeTypeApp env = \case
    TyAppVar n -> case lookup n env of
      Just v -> pure (TyAppVar v)
      Nothing -> readTvRef n >>= \case
        Unbound n' _ _ ->
          throwTypecheckError $ InvariantUnboundTypeVariable n' (TyVar n') info
        Bound n' _ ->
          throwTypecheckError $ InvariantUnboundTypeVariable n' (TyVar n') info
        LinkTy ty ->
          TyAppType <$> debruijnizeType info env ty
        LinkRow row ->
          TyAppRow <$> debruijnizeRowCtor info env row
        LinkRef r ->
          TyAppRef <$> debruijnizeMRef info env r
    TyAppType ty -> TyAppType <$> debruijnizeType info env ty
    TyAppRow row -> TyAppRow <$> debruijnizeRowCtor info env row
    TyAppRef r -> TyAppRef <$> debruijnizeMRef info env r
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
  Unbound n _ _ ->
    throwTypecheckError $ InvariantUnboundTypeVariable n (TyVar n) info
  _ -> wellKindedGuaranteed


ensureNoTyVars
  :: forall i b s a
  .  i
  -> TCType s
  -> InferM s b i (Type a)
ensureNoTyVars i ty = go ty
  where
  go = \case
    TyVar n -> readTvRef n >>= \case
      LinkTy t -> ensureNoTyVars i t
      LinkRow row -> ensureNoTyVars i (TyObject row)
      _ -> disallowedGenericSig
    TyFun l r -> TyFun <$> ensureNoTyVars i l <*> ensureNoTyVars i r
    TyList l -> TyList <$> ensureNoTyVars i l
    TyModRef mr -> TyModRef <$> ensureNoTvMRef mr
    TyObject r -> TyObject <$> ensureNoTvRowCtor r
    TyTable r -> TyTable <$> ensureNoTvRowCtor r
    TyNullary r -> TyNullary <$> ensureNoTyVars i r
    TyCapToken -> pure TyCapToken
    TyPrim p -> pure (TyPrim p)
  disallowedGenericSig :: forall k. InferM s b i k
  disallowedGenericSig = do
    tyDbg <- _dbgType ty
    throwTypecheckError $ DisallowedGenericSignature tyDbg i
  ensureNoTvMRef (MRefVar mv) = readTvRef mv >>= \case
    LinkRef mr -> ensureNoTvMRef mr
    _ -> disallowedGenericSig
  ensureNoTvMRef (MConcrete mv) = pure (MConcrete mv)
  ensureNoTvRowCtor (RowVar rv) = readTvRef rv >>= \case
    LinkRow row -> ensureNoTvRowCtor row
    _ -> disallowedGenericSig
  ensureNoTvRowCtor (RowConcrete row) =
    RowConcrete <$> traverse go row

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
    preds' <- traverse (debruijnizeBuiltinTC i env) preds
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
  RoseSubRow r1 r2 ->
    RoseSubRow <$> debruijnizeRoseRow i env r1 <*> debruijnizeRoseRow i env r2
  RoseRowEq r1 r2 ->
    RoseRowEq <$> debruijnizeRoseRow i env r1 <*> debruijnizeRoseRow i env r2

debruijnizeType
  :: i
  -> [(TCTypeVar s, DebruijnTypeVar)]
  -> TCType s
  -> InferM s b i (Type DebruijnTypeVar)
debruijnizeType i env typ = go typ
  where
  go = \case
    TyVar n -> case lookup n env of
      Just v -> pure (TyVar v)
      Nothing -> readTvRef n >>= \case
        Unbound nraw _ _ -> do
          ty' <- _dbgType typ
          throwTypecheckError $ InvariantUnboundTypeVariable nraw ty' i
        Bound nraw _ -> do
          ty' <- _dbgType typ
          throwTypecheckError $ InvariantUnboundTypeVariable nraw ty' i
        LinkTy ty -> go ty
        _ -> wellKindedGuaranteed
    TyPrim p -> pure (TyPrim p)
    TyFun l r -> TyFun <$> go l <*> go r
    TyList l -> TyList <$> go l
    TyModRef m -> TyModRef <$> debruijnizeMRef i env m
    TyObject r -> TyObject <$> debruijnizeRowCtor i env r
    TyTable r -> TyTable <$> debruijnizeRowCtor i env r
    TyNullary t -> TyNullary <$> go t
    TyCapToken -> pure TyCapToken

debruijnizeMRef :: i -> [(TypeVar (TvRef s), n)] -> MRef (TypeVar (TvRef s)) -> InferM s b i (MRef n)
debruijnizeMRef i env = \case
  MRefVar rv -> case lookup rv env of
    Just v -> pure (MRefVar v)
    Nothing -> readTvRef rv >>= \case
      Unbound nraw _ _ ->
        throwTypecheckError $ InvariantUnboundTypeVariable nraw (TyVar nraw) i
      Bound nraw _ ->
        throwTypecheckError $ InvariantUnboundTypeVariable nraw (TyVar nraw) i
      LinkRef r -> debruijnizeMRef i env r
      _ -> wellKindedGuaranteed
  MConcrete m -> pure (MConcrete m)

debruijnizeRowCtor
  :: i
  -> [(TCTypeVar s, DebruijnTypeVar)]
  -> TCRowCtor s -> InferM s b i (RowTy DebruijnTypeVar)
debruijnizeRowCtor i env = \case
  RowVar rv ->  case lookup rv env of
    Just v -> pure (RowVar v)
    Nothing -> readTvRef rv >>= \case
      Unbound nraw _ _ ->
        throwTypecheckError $ InvariantUnboundTypeVariable nraw (TyVar nraw) i
      Bound nraw _ ->
        throwTypecheckError $ InvariantUnboundTypeVariable nraw (TyVar nraw) i
      LinkRow row -> debruijnizeRowCtor i env row
      _ -> wellKindedGuaranteed
  RowConcrete r ->
    RowConcrete <$> traverse (debruijnizeType i env) r

debruijnizeRoseRow :: i -> [(TCTypeVar s, DebruijnTypeVar)] -> RoseRow (TCTypeVar s) -> InferM s b i (RoseRow DebruijnTypeVar)
debruijnizeRoseRow i env = \case
  RoseRowTy ty -> RoseRowTy <$> debruijnizeRowCtor i env ty
  RoseRowCat r1 r2 ->
    RoseRowCat <$> debruijnizeRoseRow i env r1 <*> debruijnizeRoseRow i env r2

runInfer
  :: Loaded b i
  -> InferM s b i a
  -> ST s (Either (TypecheckError i) a)
runInfer lo (InferM act) = do
  uref <- newSTRef 0
  lref <- newSTRef 1
  let env = TCEnv uref mempty lref NotInStep lo False
  evalStateT (runReaderT (runExceptT act) env) def

defFqns :: IR.EvalDef b i -> [FullyQualifiedName]
defFqns d = S.toList $ execState (IR.traverseDefTerm getFqFDeps d) mempty
  where
  getFqFDeps = transformM $ \case
    v@(IR.Var (Name n (NTopLevel mn mh)) _) -> do
      modify' (S.insert (FullyQualifiedName mn n mh))
      pure v
    v -> pure v

-- The only dependent names that can show up in any interface definitions are interface defcap manager
-- functions
ifDefDeps :: IR.EvalIfDef b i -> [Text]
ifDefDeps = \case
  IR.IfDCap dc -> case IR._ifdcMeta dc of
    DefManaged (DefManagedMeta _ (BareName n)) -> [n]
    _ -> []
  _ -> []

loadIFaceTypes :: i -> IR.Type -> EvalM e b i ()
loadIFaceTypes i = \case
  IR.TyList t -> loadIFaceTypes i t
  IR.TyModRef mrs ->
    traverse_ (lookupModuleData i) mrs
  IR.TyObject (IR.Schema _ tys) ->
    traverse_ (loadIFaceTypes i) tys
  IR.TyTable (IR.Schema _ tys) ->
    traverse_ (loadIFaceTypes i) tys
  IR.TyPrim _ -> pure ()
  _ -> pure ()

loadArgIface :: Arg IR.Type i -> EvalM e b i ()
loadArgIface (Arg _ ty i) =
  traverse_ (loadIFaceTypes i) ty

loadTermTypes :: IR.EvalTerm b i -> EvalM e b i ()
loadTermTypes term =
  traverse_ go (universe term)
  where
  go = \case
    IR.Let arg _ _ _ -> loadArgIface arg
    IR.Lam nel _ _ ->
      traverse_ loadArgIface nel
    _ -> pure ()

loadDefunTypes :: IR.Defun Name IR.Type b i -> EvalM e b i ()
loadDefunTypes (IR.Defun spec args t _) = do
  loadArgIface spec
  traverse_ loadArgIface args
  loadTermTypes t

loadDefPactTypes :: IR.DefPact Name IR.Type b i -> EvalM e b i ()
loadDefPactTypes (IR.DefPact spec args steps _) = do
  loadArgIface spec
  traverse_ loadArgIface args
  traverse_ loadStepArgs steps
  where
  loadStepArgs (IR.Step t) = loadTermTypes t
  loadStepArgs (IR.StepWithRollback l r) = loadTermTypes l *> loadTermTypes r
  loadStepArgs _ = pure ()

loadDefCapTypes :: IR.DefCap Name IR.Type b i -> EvalM e b i ()
loadDefCapTypes (IR.DefCap spec args t _ _) = do
  loadArgIface spec
  traverse_ loadArgIface args
  loadTermTypes t

loadDefSchemaTypes :: IR.DefSchema IR.Type i -> EvalM e b i ()
loadDefSchemaTypes (IR.DefSchema _ tys info) = do
  traverse_ (loadIFaceTypes info) tys

loadDefConstTypes :: IR.DefConst name IR.Type builtin i -> EvalM e b i ()
loadDefConstTypes (IR.DefConst spec _cv _) =
  loadArgIface spec

loadDefTypes :: IR.Def Name IR.Type builtin i -> EvalM e builtin i ()
loadDefTypes = \case
  IR.Dfun d -> loadDefunTypes d
  IR.DConst d -> loadDefConstTypes d
  IR.DCap d -> loadDefCapTypes d
  IR.DSchema d -> loadDefSchemaTypes d
  IR.DPact t -> loadDefPactTypes t
  IR.DTable{} -> pure ()

typecheckModule
  :: TypeOfBuiltin b
  => i
  -> ModuleName
  -> EvalM e b i (Either (TypecheckError i) (TypedModule b i, Map FullyQualifiedName (DefnType (TypeVar NamedDeBruijn))))
typecheckModule i mn = do
  (m, deps) <- getModuleWithDependencies i mn
  -- Load the interfaces used by this module into our `Loaded` env
  traverse_ (lookupModuleData i) (IR._mImplements m)
  traverse_ loadDefTypes deps
  traverse_ loadDefTypes (IR._mDefs m)
  lo <- use loaded
  pure $ runST $ runInfer lo $ inferModule m deps

renderTypecheckError :: TypecheckError FileLocSpanInfo -> ReplM b Text
renderTypecheckError = \case
  UnificationFailure (Located li lty) (Located ri rty) -> do
    lslice <- mkReplErrorLocSlice li
    rslice <- mkReplErrorLocSlice ri
    pure $ renderCompactText' $
      "Couldnt match type" <+> pretty lty <+> "with" <+> pretty rty <> hardline
      <> "Expected:" <+> pretty lty <> hardline <> pretty lslice <> hardline
      <> "Actual:" <+> pretty rty <> hardline <> pretty rslice
  RowUnificationFailure (Located li lty) (Located ri rty) -> do
    lslice <- mkReplErrorLocSlice li
    rslice <- mkReplErrorLocSlice ri
    pure $ renderCompactText' $
      "Couldnt match schema" <+> pretty lty <+> "with" <+> pretty rty <> hardline
      <> "Expected:" <+> pretty lty <> hardline <> pretty lslice <> hardline
      <> "Actual:" <+> pretty rty <> hardline <> pretty rslice
  InvariantRowInTypeVarPosition rty i ->
    invariantErr i $ "Row schema or variable found where a type was expected" <+> pretty rty
  InvariantTypeInRowVarPosition ty i ->
    invariantErr i $ "Type found where row schema was expected" <+> pretty ty
  InvariantUnboundTypeVariable t ty i ->
    invariantErr i $ "Unbound type variable: " <+> pretty t <+> "in type" <+> pretty ty
  InvariantUnboundTermVariable t i ->
    invariantErr i $ "Unbound term variable" <+> pretty t
  InvariantUnboundFreeVariable fqn i ->
    invariantErr i $ "Unbound free variable:" <+> pretty fqn
  InvariantDefconstNotEvaluated fqn i ->
    invariantErr i $ "Defconst not evaluated" <+> pretty fqn
  UserGuardMustBeApp t i ->
    singleLocError i $ "create-user-guard should take only an application in the form (f a b .. z), received" <+> pretty t
  ModuleLacksImplementedInterfaces mn mns i ->
    singleLocError i $ "Module" <+> pretty mn <+> "does not implement the requested interfaces: " <+> commaSep (S.toList mns)
  ExpectedKind expected actual i -> do
    singleLocError i $ "Expected kind" <+> pretty expected <+> "but got" <+> pretty actual
  InfiniteType (Located li lty) (Located _ rty) -> do
    singleLocError li $ "Cannot construct the infinite type" <+> pretty lty <+> "~" <+> pretty rty
  InfiniteRow (Located li lrow) (Located _ rrow) ->
    singleLocError li $ "Cannot construct the infinite type" <+> pretty lrow <+> "~" <+> pretty rrow
  DisallowedGenericSignature ty i ->
    singleLocError i $ "Generic type" <+> pretty ty <+> "is not allowed in this position"
  CannotUnifyWithBoundVariable t i ->
    invariantErr i $ "Cannot unify with bound variable" <+> pretty t
  CannotResolveConstraints pts i -> do
    constrRender <- forM pts $ \(Located i' constr) -> do
      predSlice <- mkReplErrorLocSlice i'
      pure $ constrErrorMsg constr <> hardline <> pretty predSlice
    overallSlice <- mkReplErrorLocSlice i
    pure $ renderCompactText' $ vsep constrRender <> "at" <> hardline <> pretty overallSlice
  CannotStaticallyDetermineRowOpSig sig i -> do
    singleLocError i $
      "Cannot statically determine the type of the following operation:" <+> pretty sig <> ". Calculated field labels are not supported in types"
  ArgumentRequiresTypeAnnotation arg i ->
    singleLocError i $ "Argument" <+> pretty arg <+> "requires type annotation"
  InvalidDefcapManagerFun (Located li lty) (Located ri rty) -> do
    lslice <- mkReplErrorLocSlice li
    rslice <- mkReplErrorLocSlice ri
    pure $ renderCompactText' $
      "Couldnt match type" <+> pretty lty <+> "with" <+> pretty rty <> "as a defcap manager defun" <> hardline
      <> "Expected:" <+> pretty lty <> hardline <> pretty lslice <> hardline
      <> "Actual:" <+> pretty rty <> hardline <> pretty rslice
  CannotDetermineDynamicInvoke n mrefs t i ->
    singleLocError i $
    "Cannot statically determine the type of the call" <+> pretty (_nName n) <> "::" <> pretty t <> "."
    <+> "None of the following interfaces implements function" <+> pretty t <> ":" <+> commaSep (S.toList mrefs)
  TypecheckingDoesNotSupportType t i ->
    singleLocError i $ "Static typechecking unsupported for type:" <+> pretty t
    <> ". If you are using the type list or object, consider moving to typed lists (e.g [integer])"
    <+> "or to typed objects (e.g for some (defschema my-schema balance:integer), object{my-schema})"
  DefPactStepIndexOutOfBounds fqn idx i ->
    singleLocError i $
      pretty fqn <+> "does not have a step" <+> pretty idx <> ", so we cannot infer the type if invoked at that step"
  CannotInferTypeAsModRef (Located i t) ->
    singleLocError i $ "Expression of type" <+> pretty t <+> "cannot be used in a dynamic call. Make sure your dynamic call is using a module reference"
  where
  constrErrorMsg (IsValue t) = "Type" <+> pretty t <+> "is a partially applied function or lambda closure, but expected a value. Maybe a function application is missing arguments?"
  constrErrorMsg (Add t) = "Cannot resolve operator + for type" <+> pretty t
  constrErrorMsg (Ord t) = "Type" <+> pretty t <+> "has no canonical ordering"
  constrErrorMsg (Eq t) = "Type" <+> pretty t <+> "cannot be compared for equality"
  constrErrorMsg (Show t) = "Type" <+> pretty t <+> "can't be converted to a string"
  constrErrorMsg (ListLike t) = "Expected type string or ['a], got" <+> pretty t
  constrErrorMsg (Num t) = "Expected numeric type integer or decimal, got" <+> pretty t
  constrErrorMsg (Fractional t) = "Expected numeric type integer or decimal, got" <+> pretty t
  constrErrorMsg (EnforceRead t) = "Expected type guard or string, got" <+> pretty t
  constrErrorMsg constr =  "Cannot resolve" <+> pretty constr
  invariantErr i msg =
    let msg' = "Typechecker invariant error! Please report this to the Pact team @kadena:" <+> msg
    in singleLocError i msg'
  singleLocError i msg = do
    slice <- mkReplErrorLocSlice i
    pure $ renderCompactText' $ msg <> hardline <> pretty slice

