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
 ) where

import Control.Lens
import Control.Applicative((<|>))
import Control.Monad.Except
import Data.Default
import Data.Maybe(mapMaybe)
import qualified Data.Map.Strict as M

import Pact.Core.Names
import Pact.Core.Persistence
import Pact.Core.IR.Term
import Pact.Core.Errors
import Pact.Core.Environment.Types
import Pact.Core.Hash
import Pact.Core.Namespace

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
getModule info pdb mn =
 useEvalState (esLoaded . loModules . at mn) >>= \case
   Just (ModuleData md _) -> pure md
   Just (InterfaceData _ _) ->
    throwExecutionError info (ExpectedModule mn)
   Nothing -> do
    liftDbFunction info (_pdbRead pdb DModules mn) >>= \case
      Just mdata@(ModuleData md deps) -> do
        let newLoaded = M.fromList $ toFqDep mn (_mHash md) <$> _mDefs md
        (esLoaded . loAllLoaded) %== M.union newLoaded . M.union deps
        (esLoaded . loModules) %== M.insert mn mdata
        pure md
      Just (InterfaceData _ _) ->
        throwExecutionError info (ExpectedModule mn)
      Nothing ->
        throwExecutionError info (ModuleDoesNotExist mn)

-- | Get or load a module or interface based on the module name
getModuleData :: (MonadEval b i m) => i -> PactDb b i -> ModuleName -> m (ModuleData b i)
getModuleData info pdb mn =
 useEvalState (esLoaded . loModules . at mn) >>= \case
   Just md -> pure md
   Nothing -> do
    liftDbFunction info (_pdbRead pdb DModules mn) >>= \case
      Just mdata@(ModuleData md deps) -> do
        let newLoaded = M.fromList $ toFqDep mn (_mHash md) <$> _mDefs md
        (esLoaded . loAllLoaded) %== M.union newLoaded . M.union deps
        (esLoaded . loModules) %== M.insert mn mdata
        pure mdata
      Just ifdata@(InterfaceData iface deps) -> do
        let mdefs = mapMaybe ifDefToDef (_ifDefns iface)
        let newLoaded = M.fromList $ toFqDep mn (_ifHash iface) <$> mdefs
        (esLoaded . loAllLoaded) %== M.union newLoaded . M.union deps
        (esLoaded . loModules) %== M.insert mn ifdata
        pure ifdata
      Nothing ->
        throwExecutionError info (ModuleDoesNotExist mn)

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
