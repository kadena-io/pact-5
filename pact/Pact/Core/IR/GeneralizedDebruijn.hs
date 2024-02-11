{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

module Pact.Core.IR.GeneralizedDebruijn where

-- import Control.Lens
-- import Control.Monad.Reader
-- import Data.Text(Text)
-- import Data.Foldable
-- import Data.Map.Strict(Map)
-- import Data.List.NonEmpty(NonEmpty(..))
-- import qualified Data.Map.Strict as M
-- import qualified Data.List.NonEmpty as NE


-- import Pact.Core.Names
-- import Pact.Core.IR.Term
-- import Pact.Core.Builtin
-- import Pact.Core.Literal
-- import Pact.Core.Environment
-- import Pact.Core.Builtin
-- import Pact.Core.Capabilities
-- import Pact.Core.Type
-- import Pact.Core.Info

-- import qualified Pact.Core.Syntax.ParseTree as Lisp

-- data RenamerScope b =
--   RenamerScope
--   { _rnDepth :: DeBruijn
--   , _rnBinds :: Map Text DeBruijn
--   , _rnNatives :: Map Text b
--   }

-- makeLenses ''RenamerScope

-- askRenamer :: Applicative m => DeBruijnT b m (RenamerScope k b)
-- askRenamer = DeBruijnT (ReaderT pure)

-- localRenamer :: (RenamerScope b -> RenamerScope b) -> DeBruijnT k b m a -> DeBruijnT k b m a
-- localRenamer f ma = DeBruijnT $ ReaderT (\env -> runDBJ (f env) ma)

-- viewVarDepth :: Monad m => DeBruijnT k b m DeBruijn
-- viewVarDepth = view rnDepth <$> askRenamer

-- viewBinds :: Monad m => DeBruijnT k b m (Map k DeBruijn)
-- viewBinds = view rnBinds <$> askRenamer

-- viewNatives :: Monad m => DeBruijnT k b m (Map k b)
-- viewNatives = view rnNatives <$> askRenamer

-- debruijnNVars :: (Monad m, Ord k) => [k] -> DeBruijnT k b m a -> DeBruijnT k b m a
-- debruijnNVars vars ma = do
--   depth <- viewVarDepth
--   let len = fromIntegral (length vars)
--       newDepth = depth + len
--       ixs = [depth .. newDepth - 1]
--       m = M.fromList $ zip vars ixs
--       inEnv  = over rnBinds (M.union m) . set rnDepth newDepth
--   localRenamer inEnv ma

-- pattern MapV :: i -> Lisp.Expr i
-- pattern MapV info = Lisp.Var (BN (BareName "map")) info
-- pattern FilterV :: i -> Lisp.Expr i
-- pattern FilterV info = Lisp.Var (BN (BareName "map")) info
-- pattern FoldV :: i -> Lisp.Expr i
-- pattern FoldV info = Lisp.Var (BN (BareName "map")) info
-- pattern ZipV :: i -> Lisp.Expr i
-- pattern ZipV info = Lisp.Var (BN (BareName "map")) info

-- newtype DeBruijnT k b m a
--   = DeBruijnT (ReaderT (RenamerScope k b) m a)
--   deriving
--   ( Functor
--   , Applicative
--   , Monad
--   ) via (ReaderT (RenamerScope k b) m)

-- lookupDbj :: (Monad m, Ord k) => k -> DeBruijnT k b m (Either k DeBruijn)
-- lookupDbj name = viewBinds >>= \bmap -> case M.lookup name bmap of
--   Just d -> do
--     depth <- viewVarDepth
--     pure (Right (depth - d -1))
--   Nothing -> pure (Left name)

-- runDBJ :: RenamerScope k b -> DeBruijnT k b m a -> m a
-- runDBJ env (DeBruijnT ma) = runReaderT ma env

-- toArg
--   :: Lisp.MArg
--   -> Arg Lisp.Type
-- toArg (Lisp.MArg n mty) = Arg n mty

-- debruijnize
--   :: (Ord name, Monad m)
--   => Lisp.Expr i -> DeBruijnT name b m (Term (Either name DeBruijn) (PactType ParsedTyName) b i)
-- debruijnize liftText = undefined
--   -- Lisp.Var n i -> do
--   --   m <- viewNatives
--   --   case M.lookup n m of
--   --     Just b -> pure (Builtin b i)
--   --     Nothing -> Var <$> lookupDbj n <*> pure i
--   -- Lisp.LetIn nebinders

-- desugarLispTerm
--   :: Lisp.Expr name i
--   -> Term name Lisp.Type b i
-- desugarLispTerm = \case
--   Lisp.Var n i -> Var n i
--     -- do
--     -- reservedNatives <- viewEvalEnv eeNatives
--     -- case M.lookup (_bnName n) reservedNatives of
--     --   Just b -> pure (Builtin b i)
--     --   Nothing
--     --     | n == BareName "constantly" -> do
--     --       let c1 = Arg cvar1 Nothing
--     --           c2 = Arg cvar2 Nothing
--     --       pure $ Lam (c1 :| [c2]) (Var (BN (BareName cvar1)) i) i
--     --     | n == BareName "identity" -> do
--     --       let c1 = Arg ivar1 Nothing
--     --       pure $ Lam (c1 :| []) (Var (BN (BareName ivar1)) i) i
--     --     | n == BareName "CHARSET_ASCII" -> pure (Constant (LInteger 0) i)
--     --     | n == BareName "CHARSET_LATIN1" -> pure (Constant (LInteger 1) i)
--     --     | otherwise ->
--     --       pure (Var (BN n) i)
--     -- where
--     -- cvar1 = "#constantlyA1"
--     -- cvar2 = "#constantlyA2"
--     -- ivar1 = "#identityA1"
--   Lisp.Block nel i -> let
--     nel' = fmap desugarLispTerm nel
--     in foldr (\a b -> Sequence a b i) (NE.last nel') (NE.init nel')
--   Lisp.LetIn binders expr i -> let
--     expr' = desugarLispTerm expr
--     in foldr (binderToLet i) expr' binders
--   Lisp.Lam [] body i ->
--     Nullary (desugarLispTerm body) i
--   Lisp.Lam (x:xs) body i ->
--     let nsts = x :| xs
--         args = (\(Lisp.MArg n t) -> Arg n t) <$> nsts
--         body' = desugarLispTerm body
--     in (Lam args body' i)
--   Lisp.Suspend body i -> desugarLispTerm (Lisp.Lam [] body i)
--   Lisp.Binding fs hs i -> error "todo: bindings"
--     -- hs' = fmap desugarLispTerm hs
--     -- body = bindingBody hs'
--     -- bodyLam b = Lam (pure (Arg objFreshText Nothing)) b i
--     -- in bodyLam $ foldr bindToLet body fs
--     --   where
--     --   bindingBody hs' = case reverse hs' of
--     --     [] -> error "empty binding body"
--     --     x:xs ->  foldl' (\acc e -> Sequence e acc i) x xs
--     --   objFreshText = "#bindObject"
--     --   objFreshVar = Var (BN (BareName objFreshText)) i
--     --   bindToLet (Field field, marg) body =
--     --     let arg = toArg marg
--     --         fieldLit = Constant (LString field) i
--     --         access = App (Builtin (liftRaw RawAt) i) [fieldLit, objFreshVar] i
--     --     in Let arg access body i
--   Lisp.If e1 e2 e3 i -> Conditional (CIf (desugarLispTerm e1) (desugarLispTerm e2) (desugarLispTerm e3)) i
--   Lisp.App (Lisp.Operator o _oi) [e1, e2] i -> case o of
--     Lisp.AndOp ->
--       Conditional (CAnd (desugarLispTerm e1) (desugarLispTerm e2)) i
--     Lisp.OrOp ->
--       Conditional (COr (desugarLispTerm e1) (desugarLispTerm e2)) i
--     Lisp.EnforceOp ->
--       Conditional (CEnforce (desugarLispTerm e1) (desugarLispTerm e2)) i
--     Lisp.EnforceOneOp -> case e2 of
--       Lisp.List e _ ->
--         Conditional (CEnforceOne (desugarLispTerm e1) (desugarLispTerm <$> e)) i
--       _ ->
--         error "enforce-one: expected argument list" i
--   Lisp.App e hs i -> App (desugarLispTerm e) (desugarLispTerm <$> hs) i
--     -- e' = desugarLispTerm e
--     -- hs' = traverse desugarLispTerm hs
--     -- in case e' of
--     --   Builtin b _ -> desugarAppArity i b hs'
--     --   _ -> pure (App e' hs' i)
--   -- Lisp.Operator _bop _i -> error "desugaring operators WIP"
--   -- Lisp.List e1 i ->
--   --   ListLit (desugarLispTerm <$> e1) i
--   -- Lisp.Constant l i ->
--   --   Constant l i
--   -- Lisp.Try e1 e2 i ->
--   --   Try (desugarLispTerm e1) (desugarLispTerm e2) i
--   -- Lisp.Object fields i ->
--   --   ObjectLit (over _2 desugarLispTerm <$> fields) i
--   -- Lisp.CapabilityForm cf i -> (`CapabilityForm` i) $ case cf of
--   --   Lisp.WithCapability cap body ->
--   --     WithCapability (desugarLispTerm cap) (desugarLispTerm body)
--   --   Lisp.CreateUserGuard pn exs ->
--   --     CreateUserGuard pn (desugarLispTerm <$> exs)
--   -- where
--   -- binderToLet i (Lisp.Binder n mty expr) term = let
--   --   expr' = desugarLispTerm expr
--   --   in Let (Arg n mty) expr' term i
