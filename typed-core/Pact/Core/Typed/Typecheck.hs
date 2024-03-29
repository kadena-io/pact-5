{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Pact.Core.Typed.Typecheck where

import Control.Monad
import Control.Lens
import Control.Monad.Reader
import Control.Monad.Except
import Data.Foldable(foldlM)
import Data.List.NonEmpty(NonEmpty(..))
import Data.Map.Strict(Map)
import Data.RAList(RAList)
import qualified Data.Map.Strict as M
import qualified Data.List.NonEmpty as NE
import qualified Data.RAList as RAList

import Pact.Core.Builtin
import Pact.Core.Typed.Term
import Pact.Core.Names
import Pact.Core.Type


data TCEnv tyname builtin = TCEnv
  { _tcLocalVarEnv :: RAList (Type tyname)
  , _tcFreeVarEnv :: Map FullyQualifiedName (Type tyname)
  , _tcBuiltin :: builtin -> Type tyname }

makeLenses ''TCEnv

typeUnifies :: Ord n => Type n -> Type n -> Bool
typeUnifies (TyVar n) (TyVar n') = n == n'
typeUnifies (TyPrim l) (TyPrim r) = l == r
typeUnifies (TyFun l r) (TyFun l' r') =
  typeUnifies l l' && typeUnifies r r'
typeUnifies (TyForall as ty) (TyForall as' ty') =
  let tys = NE.zip as (TyVar <$>  as')
      env = M.fromList (NE.toList tys)
  in typeUnifies (substInTy env ty) ty'
typeUnifies _ _ = False

applyFunctionType :: (Ord n, MonadError String f) => Type n -> Type n -> f (Type n)
applyFunctionType (TyFun l r) l'
  | typeUnifies l l' = pure r
  | otherwise  = throwError "function type mismatch"
applyFunctionType _ _ =
  throwError "term application to non-function"

applyType :: (MonadError String m, Ord n, Show n) => Type n -> Type n -> m (Type n)
applyType (TyForall (t:|ts) tfty) ty = case ts of
  [] -> pure $ substInTy (M.singleton t ty) tfty
  t':ts' -> pure $ TyForall (t':|ts') $ substInTy (M.singleton t ty) tfty
applyType t1 tyr =
  throwError $ "Cannot apply: " <> show t1 <> " to: " <> show tyr

substInTy :: Ord n => M.Map n (Type n) -> Type n -> Type n
substInTy s (TyVar n) =
  case M.lookup n s of
    Just ty -> ty
    Nothing -> TyVar n
substInTy s (TyFun l r) =
  TyFun (substInTy s l) (substInTy s r)
substInTy _ t = t

typecheck'
  :: (MonadReader (TCEnv tyname builtin) m,
      MonadError String m,
      Ord tyname, Show tyname)
  => Term Name tyname builtin info
  -> m (Type tyname)
typecheck' = \case
  --   x:t ∈ Γ
  -- ----------- (T-Var)
  --  Γ ⊢ x:t
  Var n _ -> case _nKind n of
    NBound wo ->
      views tcLocalVarEnv (`RAList.lookup` wo) >>= \case
        Nothing -> throwError "Found free var"
        Just k -> pure k
    NTopLevel mn mh ->
      view (tcFreeVarEnv . at (FullyQualifiedName mn (_nName n) mh)) >>= \case
        Nothing -> throwError "Found free var"
        Just k -> pure k
    NModRef _mn _ -> error "implement modrevs "

    -- view (tcVarEnv . at n) >>= \case
    --   Nothing -> throwError "Found free var"
    --   Just k -> pure k
  -- Γ, x:t1 ⊢ e:t2
  -- ------------------- (T-Abs)
  -- Γ ⊢ (λx:t1.e):t1→t2
  Lam args term _ -> do
    -- let inEnv e = foldl' (\m (n', t') -> M.insert n' t' m) e args
    let env' = RAList.fromList $ view _2 <$> NE.toList args
    ret <- locally tcLocalVarEnv (env' <>) (typecheck' term)
    -- ret <- locally tcVarEnv (M.union (M.fromList (NE.toList args))) (typecheck' term)
    pure $ foldr TyFun ret (snd <$> args)
  -- Γ ⊢ x:t1→t2     Γ ⊢ y:t1
  -- ------------------------ (T-App  )
  -- Γ ⊢ (x y):t2
  App app term _ -> do
    tf <- typecheck' app
    targs <- traverse typecheck' term
    foldlM applyFunctionType tf targs
  -- Γ ⊢ e1:t1    Γ, n:t1 ⊢ e:t2
  -- ------------------- (T-Abs)
  -- Γ ⊢ let n = e1 in e2 : t2
  Let _n e1 e2 _ -> do
    t1 <- typecheck' e1
    locally tcLocalVarEnv (RAList.cons t1) $ typecheck' e2
  -- Γ ⊢ x:∀X.t1
  -- ------------------------ (T-TApp)
  -- Γ ⊢ x[τ]:t1[X→τ]
  TyApp term tyApps _ -> do
    ty <- typecheck' term
    foldlM applyType ty tyApps

  -- Todo: All type checks should ensure that types emitted (including type variables)
  -- are checked to not be unbound.
  TyAbs tys term _ -> do
    termty <- typecheck' term
    pure (TyForall tys termty)
  -- b : t ∈ B                (where B = { b : t | for b in Builtins, t in T})
  -- ------------------------ (T-Builtin)
  -- Γ ⊢ b : t
  Builtin b _ ->
    views tcBuiltin ($ b)
  -- a_1, ..., a_n : t
  -- ------------------------ (T-List)
  -- Γ ⊢ [a_1, ..., a_n] @ t : List t
  ListLit ty ts _ -> do
    tys <- traverse typecheck' ts
    unless (all (typeUnifies ty) tys) $ throwError "List literal type element mismatch"
    pure (TyList ty)
  -- todo: objectlit type ann seems unnecessary
  -- a_1, ..., a_n : t
  -- ------------------------ (T-Builtin)
  -- Γ ⊢ [a_1, ..., a_n] : List t
  -- ObjectLit _fields _ -> error "unimplemented"
  -- ObjectOp _ _ -> error "unimplemented"

  -- e_1:t_1, ... , e_n : t_n
  -- ------------------------ (T-Builtin)
  -- Γ ⊢  {e_1:t_1,...,e_n:t_n} : t_n
  Sequence e1 e2 _ ->
    typecheck' e1 *> typecheck' e2

  Conditional c _ -> case c of
    CAnd t1 t2 -> do
      t1ty <- typecheck' t1
      t2ty <- typecheck' t2
      unless (t1ty == t2ty && t2ty == TyBool) $ throwError "mismatch"
      pure TyBool
    COr t1 t2 -> do
      t1ty <- typecheck' t1
      t2ty <- typecheck' t2
      unless (t1ty == t2ty && t2ty == TyBool) $ throwError "mismatch"
      pure TyBool
    CIf cond e1 e2 -> do
      condTy <- typecheck' cond
      unless (condTy == TyBool) $ throwError "mismatch"
      te1 <- typecheck' e1
      te2 <- typecheck' e2
      unless (te1 == te2) $ throwError "mismatch"
      pure te1
  -- k : p ∈ K                (where K = builtin types, constants)
  -- ------------------------ (T-Const)
  -- Γ ⊢ k : Prim p
  Constant l _ ->
    pure (typeOfLit l)
  CapabilityForm{} -> error "implement caps"
  DynInvoke{} -> error "implement dyn ref"

  Try e1 e2 _ -> do
    te1 <- typecheck' e1
    te2 <- typecheck' e2
    when (te1 /= te2) $ throwError "Type mismatch in try"
    pure te1

  Error t _ _ ->
    pure t
