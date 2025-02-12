{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ConstraintKinds #-}


module Pact.Core.Environment.Utils
 ( viewEvalEnv
 , viewsEvalEnv
 , throwExecutionError
 , toFqDep
 , mangleNamespace
 , getAllStackCaps
 , checkSigCaps
 , allModuleExports
 , liftDbFunction
 , throwUserRecoverableError
 , throwUserRecoverableError'
 , throwNativeExecutionError
 , versionedNatives
 , versionedReplNatives
 ) where

import Control.Lens
import Data.IORef
import Data.Foldable
import Control.Applicative((<|>))
import Control.Monad.Except
import Control.Monad.Reader hiding (MonadIO(..))
import Control.Monad.IO.Class(MonadIO(..))
import Data.Text(Text)
import Data.Maybe(mapMaybe)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Pact.Core.Names
import Pact.Core.Persistence.Types
import Pact.Core.IR.Term
import Pact.Core.Errors
import Pact.Core.Environment.Types
import Pact.Core.Hash
import Pact.Core.Namespace
import Pact.Core.Guards
import Pact.Core.Capabilities
import Pact.Core.PactValue
import Pact.Core.Builtin


viewEvalEnv :: Lens' (EvalEnv b i) s -> EvalM e b i s
viewEvalEnv l = ask >>= \case
  ExecEnv e -> pure (view l e)
  ReplEnv r ->
     view (replEvalEnv . l) <$> liftIO (readIORef r)

viewsEvalEnv :: Lens' (EvalEnv b i) s -> (s -> a) -> EvalM e b i a
viewsEvalEnv l f = ask >>= \case
  ExecEnv e -> pure (views l f e)
  ReplEnv r ->
     views (replEvalEnv . l) f <$> liftIO (readIORef r)


toFqDep :: ModuleName -> ModuleHash -> Def name t b i -> (FullyQualifiedName, Def name t b i)
toFqDep modName mhash defn =
  let fqn = FullyQualifiedName modName (defName defn) mhash
  in (fqn, defn)

allModuleExports :: ModuleData b i -> M.Map FullyQualifiedName (EvalDef b i)
allModuleExports = \case
  ModuleData newMdl deps ->
    let allNewDeps = M.fromList $ toFqDep (_mName newMdl) (_mHash newMdl) <$> _mDefs newMdl
    in allNewDeps <> deps
  InterfaceData iface deps ->
    let defs = mapMaybe ifDefToDef (_ifDefns iface)
        allNewDeps = M.fromList $ toFqDep (_ifName iface) (_ifHash iface) <$> defs
    in allNewDeps <> deps

liftDbFunction
  :: i
  -> IO a
  -> EvalM e b i a
liftDbFunction _ action = liftIO action


throwUserRecoverableError :: i -> UserRecoverableError -> EvalM e b i a
throwUserRecoverableError i err = do
  st <- use esStack
  throwUserRecoverableError' i st err

throwUserRecoverableError' :: i -> [StackFrame i] -> UserRecoverableError -> EvalM e b i a
throwUserRecoverableError' info stack err = throwError (PEUserRecoverableError err stack info)

throwExecutionError :: i -> EvalError -> EvalM e b i a
throwExecutionError i e = do
  st <- use esStack
  throwError (PEExecutionError e st i)

throwNativeExecutionError :: IsBuiltin b => i -> b -> Text -> EvalM e b i a
throwNativeExecutionError info b msg =
  throwExecutionError info (NativeExecutionError (builtinName b) msg)



mangleNamespace :: ModuleName -> EvalM e b i ModuleName
mangleNamespace mn@(ModuleName mnraw ns) =
  use (esLoaded . loNamespace) >>= \case
    Nothing -> pure mn
    Just (Namespace currNs _ _) -> pure (ModuleName mnraw (ns <|> Just currNs))

getAllStackCaps
  :: EvalM e b i (S.Set (CapToken QualifiedName PactValue))
getAllStackCaps = do
  S.fromList . concatMap capToList <$> use (esCaps . csSlots)
  where
  capToList (CapSlot c cs) = c:cs

-- Todo: capautonomous
checkSigCaps
  :: M.Map PublicKeyText (S.Set (CapToken QualifiedName PactValue))
  -> EvalM e b i (M.Map PublicKeyText (S.Set (CapToken QualifiedName PactValue)))
checkSigCaps sigs = do
  capsBeingEvaluated <- use (esCaps . csCapsBeingEvaluated)
  granted <- if S.null capsBeingEvaluated then getAllStackCaps else pure capsBeingEvaluated
  autos <- use (esCaps . csAutonomous)
  -- Pretty much, what this means is:
  -- if you installed a capability from code (using `install-capability`)
  -- then we disable unscoped sigs. Why?
  -- Because we do not want to allow the installation of managed caps with
  -- resources when a user explicitly did not sign for it.
  pure $ M.filter (match (S.null autos) granted) sigs
  where
  match allowEmpty granted sigCaps =
    (S.null sigCaps && allowEmpty) ||
    not (S.null (S.intersection granted sigCaps))

-- | Natives enabled by the pact 5.1 fork
pact51Natives :: S.Set Text
pact51Natives = S.fromList $ ["hash-poseidon", "hash-keccak256"]

-- | Get the natives disabled by a particular execution flag
nativesDisabledByFlag :: ExecutionFlag -> Maybe (S.Set Text)
nativesDisabledByFlag = \case
  FlagDisablePact51 -> Just pact51Natives
  _ -> Nothing

versionNativesByFlag :: S.Set ExecutionFlag -> M.Map Text b -> M.Map Text b
versionNativesByFlag ec m =
  let nativesDisabled = foldl' (\s f -> maybe s (S.union s) (nativesDisabledByFlag f)) mempty ec
  in M.withoutKeys m nativesDisabled

versionedNatives :: S.Set ExecutionFlag -> M.Map Text CoreBuiltin
versionedNatives ec = versionNativesByFlag ec coreBuiltinMap

versionedReplNatives :: S.Set ExecutionFlag -> M.Map Text ReplCoreBuiltin
versionedReplNatives ec = versionNativesByFlag ec replBuiltinMap