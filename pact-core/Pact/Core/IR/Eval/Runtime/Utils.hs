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

module Pact.Core.IR.Eval.Runtime.Utils
 ( mkBuiltinFn
 , fromPactValue
 , checkPactValueType
 , cfFQN
 , viewCEKEnv, viewsCEKEnv
 , setCekState, overCekState, useCekState, usesCekState
 , getAllStackCaps
 , checkSigCaps
 ) where

import Control.Lens hiding ((%%=))
import Data.Void
import Data.Map.Strict(Map)
import Data.Set(Set)
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Pact.Core.Names
import Pact.Core.PactValue
import Pact.Core.Builtin
import Pact.Core.Hash
import Pact.Core.Type
import Pact.Core.IR.Eval.Runtime.Types


mkBuiltinFn
  :: (BuiltinArity b)
  => (Cont b i m -> CEKErrorHandler b i m -> [CEKValue b i m] -> m (EvalResult b i m))
  -> b
  -> BuiltinFn b i m
mkBuiltinFn fn b =
  BuiltinFn b fn (builtinArity b) []
{-# INLINE mkBuiltinFn #-}

cfFQN :: Lens' (CapFrame b i) FullyQualifiedName
cfFQN f = \case
  WithCapFrame fqn b -> (`WithCapFrame` b) <$> f fqn
  RequireCapFrame fqn -> RequireCapFrame <$> f fqn
  ComposeCapFrame fqn -> ComposeCapFrame <$> f fqn
  InstallCapFrame fqn -> InstallCapFrame <$> f fqn
  EmitEventFrame fqn -> EmitEventFrame <$> f fqn
  CreateUserGuardFrame fqn -> CreateUserGuardFrame <$> f fqn

getAllStackCaps
  :: MonadEval b i m
  => m (Set CapToken)
getAllStackCaps = do
  Set.fromList . concatMap capToList <$> useCekState (esCaps . csSlots)
  where
  capToList (CapSlot c cs) = c:cs

checkSigCaps
  :: MonadEval b i m
  => Map PublicKeyText (Set CapToken)
  -> m (Map PublicKeyText (Set CapToken))
checkSigCaps sigs = do
  granted <- getAllStackCaps
  pure $ Map.filter (match granted) sigs
  where
  match granted sigCaps =
    Set.null sigCaps || not (Set.null (Set.intersection granted sigCaps))


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

setCekState :: (MonadEval b i m) => Lens' (EvalState b i) s -> s -> m ()
setCekState l s = modifyCEKState (set l s)

overCekState :: (MonadEval b i m) => Lens' (EvalState b i) s -> (s -> s) -> m ()
overCekState l f = modifyCEKState (over l f)

useCekState :: (MonadEval b i m) => Lens' (EvalState b i) s -> m s
useCekState l = view l <$> getCEKState

usesCekState :: (MonadEval b i m) => Lens' (EvalState b i) s -> (s -> s') -> m s'
usesCekState l f = views l f <$> getCEKState

viewCEKEnv :: (MonadEval b i m) => Lens' (CEKRuntimeEnv b i m) s -> m s
viewCEKEnv l = view l <$> cekReadEnv

viewsCEKEnv :: (MonadEval b i m) => Lens' (CEKRuntimeEnv b i m) s -> (s -> a) -> m a
viewsCEKEnv l f = views f l <$> cekReadEnv f
