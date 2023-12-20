{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ConstraintKinds #-}


module Pact.Core.Environment.Utils
 ( setEvalState
 , (%==), useEvalState, usesEvalState
 , (.==)
 , viewEvalEnv
 , viewsEvalEnv
 , getModuleData
 , getModule
 , getModuleMember
 , lookupModule
 , lookupModuleData
 , throwExecutionError
 , throwExecutionError'
 , toFqDep
 , mangleNamespace
 , getAllStackCaps
 , checkSigCaps
 , isKeysetInSigs
 , isKeysetNameInSigs
 , enforceKeysetNameAdmin
 ) where

import Control.Lens
import Control.Applicative((<|>))
import Control.Monad(unless)
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Default
import Data.Maybe(mapMaybe)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Pact.Core.Names
import Pact.Core.Persistence
import Pact.Core.IR.Term
import Pact.Core.Errors
import Pact.Core.Environment.Types
import Pact.Core.Hash
import Pact.Core.Namespace
import Pact.Core.Guards
import Pact.Core.Capabilities
import Pact.Core.PactValue

viewEvalEnv :: (MonadEvalEnv b i m) => Lens' (EvalEnv b i) s -> m s
viewEvalEnv l = view l <$> readEnv

viewsEvalEnv :: (MonadEvalEnv b i m) => Lens' (EvalEnv b i) s -> (s -> a) -> m a
viewsEvalEnv f l = views f l <$> readEnv

setEvalState :: (MonadEvalState b i m) => Traversal' (EvalState b i) s -> s -> m ()
setEvalState l s = modifyEvalState (set l s)

(.==) :: (MonadEvalState b i m) => Traversal' (EvalState b i) s -> s -> m ()
l .== s = modifyEvalState (set l s)

(%==) :: (MonadEvalState b i m) => Traversal' (EvalState b i) s -> (s -> s) -> m ()
l %== f = modifyEvalState (over l f)

infix 4 %==, .==

useEvalState :: (MonadEvalState b i m) => Lens' (EvalState b i) s -> m s
useEvalState l = view l <$> getEvalState

usesEvalState :: (MonadEvalState b i m) => Lens' (EvalState b i) s -> (s -> s') -> m s'
usesEvalState l f = views l f <$> getEvalState

toFqDep :: ModuleName -> ModuleHash -> Def name t b i -> (FullyQualifiedName, Def name t b i)
toFqDep modName mhash defn =
  let fqn = FullyQualifiedName modName (defName defn) mhash
  in (fqn, defn)

throwExecutionError :: (MonadEval b i m) => i -> EvalError -> m a
throwExecutionError i e = throwError (PEExecutionError e i)

throwExecutionError' :: (MonadEval b i m) => EvalError -> m a
throwExecutionError' = throwExecutionError def

-- | lookupModuleData for only modules
lookupModule :: (MonadEval b i m) => i -> PactDb b i -> ModuleName -> m (Maybe (EvalModule b i))
lookupModule info pdb mn =
 useEvalState (esLoaded . loModules . at mn) >>= \case
   Just (ModuleData md _) -> pure (Just md)
   Just (InterfaceData _ _) ->
    throwExecutionError info (ExpectedModule mn)
   Nothing -> do
    liftDbFunction info (_pdbRead pdb DModules mn) >>= \case
      Just mdata@(ModuleData md deps) -> do
        let newLoaded = M.fromList $ toFqDep mn (_mHash md) <$> _mDefs md
        (esLoaded . loAllLoaded) %== M.union newLoaded . M.union deps
        (esLoaded . loModules) %== M.insert mn mdata
        pure (Just md)
      Just (InterfaceData _ _) ->
        throwExecutionError info (ExpectedModule mn)
      Nothing -> pure Nothing

-- | lookupModuleData modules and interfaces
lookupModuleData :: (MonadEval b i m) => i -> PactDb b i -> ModuleName -> m (Maybe (ModuleData b i))
lookupModuleData info pdb mn =
 useEvalState (esLoaded . loModules . at mn) >>= \case
   Just md -> pure (Just md)
   Nothing -> do
    liftDbFunction info (_pdbRead pdb DModules mn) >>= \case
      Just mdata@(ModuleData md deps) -> do
        let newLoaded = M.fromList $ toFqDep mn (_mHash md) <$> _mDefs md
        (esLoaded . loAllLoaded) %== M.union newLoaded . M.union deps
        (esLoaded . loModules) %== M.insert mn mdata
        pure (Just mdata)
      Just mdata@(InterfaceData iface deps) -> do
        let ifDefs = mapMaybe ifDefToDef (_ifDefns iface)
        let newLoaded = M.fromList $ toFqDep mn (_ifHash iface) <$> ifDefs
        (esLoaded . loAllLoaded) %== M.union newLoaded . M.union deps
        (esLoaded . loModules) %== M.insert mn mdata
        pure (Just mdata)
      Nothing -> pure Nothing


-- | getModuleData, but only for modules, no interfaces
getModule :: (MonadEval b i m) => i -> PactDb b i -> ModuleName -> m (EvalModule b i)
getModule info pdb mn = lookupModule info pdb mn >>= \case
  Just md -> pure md
  Nothing -> throwExecutionError info (ModuleDoesNotExist mn)

-- | Get or load a module or interface based on the module name
getModuleData :: (MonadEval b i m) => i -> PactDb b i -> ModuleName -> m (ModuleData b i)
getModuleData info pdb mn = lookupModuleData info pdb mn >>= \case
  Just md -> pure md
  Nothing -> throwExecutionError info (ModuleDoesNotExist mn)

-- | getModuleData, but only for modules, no interfaces
getModuleMember :: (MonadEval b i m) => i -> PactDb b i -> QualifiedName -> m (EvalDef b i)
getModuleMember info pdb (QualifiedName qn mn) = do
  md <- getModule info pdb mn
  case findDefInModule qn md of
    Just d -> pure d
    Nothing -> do
      let fqn = FullyQualifiedName mn qn (_mHash md)
      throwExecutionError info (NameNotInScope fqn)


mangleNamespace :: (MonadEvalState b i m) => ModuleName -> m ModuleName
mangleNamespace mn@(ModuleName mnraw ns) =
  useEvalState (esLoaded . loNamespace) >>= \case
    Nothing -> pure mn
    Just (Namespace currNs _ _) -> pure (ModuleName mnraw (ns <|> Just currNs))

isKeysetInSigs
  :: MonadEval b i m
  => KeySet QualifiedName
  -> m Bool
isKeysetInSigs (KeySet kskeys ksPred) = do
  matchedSigs <- M.filterWithKey matchKey <$> viewEvalEnv eeMsgSigs
  sigs <- checkSigCaps matchedSigs
  runPred (M.size sigs)
  where
  matchKey k _ = k `elem` kskeys
  atLeast t m = m >= t
  count = S.size kskeys
  runPred matched =
    case ksPred of
      KeysAll -> run atLeast
      KeysAny -> run (\_ m -> atLeast 1 m)
      Keys2 -> run (\_ m -> atLeast 2 m)
    where
    run p = pure (p count matched)

getAllStackCaps
  :: (MonadEval b i m)
  => m (S.Set (CapToken QualifiedName PactValue))
getAllStackCaps = do
  S.fromList . concatMap capToList <$> useEvalState (esCaps . csSlots)
  where
  capToList (CapSlot c cs) = c:cs

-- Todo: capautonomous
checkSigCaps
  :: (MonadEval b i m)
  => M.Map PublicKeyText (S.Set (CapToken QualifiedName PactValue))
  -> m (M.Map PublicKeyText (S.Set (CapToken QualifiedName PactValue)))
checkSigCaps sigs = do
  granted <- getAllStackCaps
  autos <- useEvalState (esCaps . csAutonomous)
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

isKeysetNameInSigs
  :: (MonadEval b i m)
  => i
  -> PactDb b i
  -> KeySetName
  -> m Bool
isKeysetNameInSigs info pdb ksn = do
  liftIO (readKeySet pdb ksn) >>= \case
    Just ks -> isKeysetInSigs ks
    Nothing ->
      throwExecutionError info (NoSuchKeySet ksn)

enforceKeysetNameAdmin
  :: MonadEval b i m
  => i
  -> ModuleName
  -> KeySetName
  -> m ()
enforceKeysetNameAdmin i modName ksn = do
  pdb <- viewEvalEnv eePactDb
  signed <- isKeysetNameInSigs i pdb ksn
  unless signed $ throwExecutionError i (ModuleGovernanceFailure modName)
