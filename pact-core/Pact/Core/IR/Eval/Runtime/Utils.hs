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
 , cfFQN
 , enforcePactValue
--  , viewCEKEnv, viewsCEKEnv
 , setEvalState, (%%=), useEvalState, usesEvalState
 , getAllStackCaps
 , checkSigCaps
 , lookupFqName
 , typecheckArgument
 , maybeTCType
 , safeTail
 ) where

import Control.Lens hiding ((%%=))
import Data.Map.Strict(Map)
import Data.Set(Set)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Pact.Core.Names
import Pact.Core.PactValue
import Pact.Core.Builtin
import Pact.Core.Guards
import Pact.Core.IR.Term
import Pact.Core.ModRefs
import Pact.Core.Type
import Pact.Core.IR.Eval.Runtime.Types

mkBuiltinFn
  :: (BuiltinArity b)
  => i
  -> (Cont b i m -> CEKErrorHandler b i m -> [CEKValue b i m] -> m (EvalResult b i m))
  -> b
  -> NativeFn b i m
mkBuiltinFn i fn b =
  NativeFn b fn (builtinArity b) i
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
  Set.fromList . concatMap capToList <$> useEvalState (esCaps . csSlots)
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

enforcePactValue :: Applicative f => CEKValue b i m -> f PactValue
enforcePactValue = \case
  VPactValue pv -> pure pv
  VClosure{} -> error "closure is not a pact value"

-- Note: The following functions
-- when placed in this file are causing GHC 9.6.2 to bork with the following error:
-- <no location info>: error:
--     panic! (the 'impossible' happened)
--   GHC version 9.6.2:
-- 	lookupIdSubst
--   $dMonadEvalEnv_aO5i
--   InScope {b_aNXG i_aNXH m_aNXI s_aNXJ a_aNXK $d(%,,,%)_aNXL
--            mkBuiltinFn cfFQN fromPactValue setEvalState overEvalState
--            useEvalState usesEvalState viewCEKEnv}
--   Call stack:
--       CallStack (from HasCallStack):
--         callStackDoc, called at compiler/GHC/Utils/Panic.hs:189:37 in ghc:GHC.Utils.Panic
--         pprPanic, called at compiler/GHC/Core/Subst.hs:197:17 in ghc:GHC.Core.Subst
--   CallStack (from HasCallStack):
--     panic, called at compiler/GHC/Utils/Error.hs:454:29 in ghc:GHC.Utils.Error
-- viewCEKEnv :: (MonadEval b i m) => Lens' (EvalEnv b i m) s -> m s
-- viewCEKEnv l = view l <$> cekReadEnv

-- viewsCEKEnv :: (MonadEval b i m) => Lens' (EvalEnv b i m) s -> (s -> a) -> m a
-- viewsCEKEnv l f = views f l <$> cekReadEnv f

setEvalState :: (MonadEval b i m) => Lens' (EvalState b i) s -> s -> m ()
setEvalState l s = modifyEvalState (set l s)

-- overEvalState :: (MonadEval b i m) => Lens' (EvalState b i) s -> (s -> s) -> m ()
-- overEvalState l f = modifyCEKState (over l f)

(%%=) :: (MonadEval b i m) => Lens' (EvalState b i) s -> (s -> s) -> m ()
l %%= f = modifyEvalState (over l f)

infix 4 %%=

useEvalState :: (MonadEval b i m) => Lens' (EvalState b i) s -> m s
useEvalState l = view l <$> getEvalState

usesEvalState :: (MonadEval b i m) => Lens' (EvalState b i) s -> (s -> s') -> m s'
usesEvalState l f = views l f <$> getEvalState

lookupFqName :: (MonadEval b i m) => FullyQualifiedName -> m (Maybe (EvalDef b i))
lookupFqName fqn =
  Map.lookup fqn . view eeLoaded <$> readEnv

typecheckArgument :: (MonadEval b i m) => PactValue -> Type -> m PactValue
typecheckArgument pv ty = case (pv, checkPvType ty pv) of
  (PModRef mr, Just (TyModRef m))
    | _mrRefined mr == Nothing -> pure (PModRef (mr & mrRefined ?~ m))
    | otherwise -> pure (PModRef mr)
  (_, Just _) -> pure pv
  (_, Nothing) -> error $ "runtime tc error" <> show (pv, ty)

maybeTCType :: (MonadEval b i m) => PactValue -> Maybe Type -> m PactValue
maybeTCType pv = maybe (pure pv) (typecheckArgument pv)

safeTail :: [a] -> [a]
safeTail (_:xs) = xs
safeTail [] = []
