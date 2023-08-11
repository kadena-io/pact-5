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
--  , viewCEKEnv, viewsCEKEnv
 , setCekState, (%%=), useCekState, usesCekState
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
import Pact.Core.Guards
import Pact.Core.Type
import Pact.Core.IR.Eval.Runtime.Types
    ( CEKErrorHandler,
      Cont,
      CapFrame(..),
      CapSlot(CapSlot),
      CapToken,
      BuiltinFn(BuiltinFn),
      MonadEvalState(getCEKState, modifyCEKState),
      MonadEval,
      EvalState,
      EvalResult,
      CEKValue(VModRef, VLiteral, VList, VGuard),
      esCaps,
      csSlots )


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

-- Note: The following functions
-- when placed in this file are causing GHC 9.6.2 to bork with the following error:
-- <no location info>: error:
--     panic! (the 'impossible' happened)
--   GHC version 9.6.2:
-- 	lookupIdSubst
--   $dMonadEvalEnv_aO5i
--   InScope {b_aNXG i_aNXH m_aNXI s_aNXJ a_aNXK $d(%,,,%)_aNXL
--            mkBuiltinFn cfFQN fromPactValue setCekState overCekState
--            useCekState usesCekState viewCEKEnv}
--   Call stack:
--       CallStack (from HasCallStack):
--         callStackDoc, called at compiler/GHC/Utils/Panic.hs:189:37 in ghc:GHC.Utils.Panic
--         pprPanic, called at compiler/GHC/Core/Subst.hs:197:17 in ghc:GHC.Core.Subst
--   CallStack (from HasCallStack):
--     panic, called at compiler/GHC/Utils/Error.hs:454:29 in ghc:GHC.Utils.Error
-- viewCEKEnv :: (MonadEval b i m) => Lens' (CEKRuntimeEnv b i m) s -> m s
-- viewCEKEnv l = view l <$> cekReadEnv

-- viewsCEKEnv :: (MonadEval b i m) => Lens' (CEKRuntimeEnv b i m) s -> (s -> a) -> m a
-- viewsCEKEnv l f = views f l <$> cekReadEnv f

setCekState :: (MonadEval b i m) => Lens' (EvalState b i) s -> s -> m ()
setCekState l s = modifyCEKState (set l s)

-- overCekState :: (MonadEval b i m) => Lens' (EvalState b i) s -> (s -> s) -> m ()
-- overCekState l f = modifyCEKState (over l f)

(%%=) :: (MonadEval b i m) => Lens' (EvalState b i) s -> (s -> s) -> m ()
l %%= f = modifyCEKState (over l f)

infix 4 %%=

useCekState :: (MonadEval b i m) => Lens' (EvalState b i) s -> m s
useCekState l = view l <$> getCEKState

usesCekState :: (MonadEval b i m) => Lens' (EvalState b i) s -> (s -> s') -> m s'
usesCekState l f = views l f <$> getCEKState
