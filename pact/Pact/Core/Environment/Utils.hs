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
 , getModuleData
 , getModule
 , getModuleMember
 , getModuleMemberWithHash
 , lookupModule
 , lookupModuleData
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
 ) where

import Control.Lens
import Data.IORef
import Control.Applicative((<|>))
import Control.Monad.Except
import Control.Exception.Safe
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
liftDbFunction info action = do
  liftIO (try action) >>= \case
    Left dbopErr -> throwExecutionError info (DbOpFailure dbopErr)
    Right e -> pure e

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


-- | lookupModuleData for only modules
lookupModule :: i -> PactDb b i -> ModuleName -> EvalM e b i (Maybe (EvalModule b i))
lookupModule info pdb mn =
 use (esLoaded . loModules . at mn) >>= \case
   Just (ModuleData md _) -> pure (Just md)
   Just (InterfaceData _ _) ->
    throwExecutionError info (ExpectedModule mn)
   Nothing -> do
    liftDbFunction info (_pdbRead pdb DModules mn) >>= \case
      Just mdata@(ModuleData md deps) -> do
        let newLoaded = M.fromList $ toFqDep mn (_mHash md) <$> _mDefs md
        (esLoaded . loAllLoaded) %= M.union newLoaded . M.union deps
        (esLoaded . loModules) %= M.insert mn mdata
        pure (Just md)
      Just (InterfaceData _ _) ->
        throwExecutionError info (ExpectedModule mn)
      Nothing -> pure Nothing

-- | lookupModuleData modules and interfaces
lookupModuleData :: i -> PactDb b i -> ModuleName -> EvalM e b i (Maybe (ModuleData b i))
lookupModuleData info pdb mn =
 use (esLoaded . loModules . at mn) >>= \case
   Just md -> pure (Just md)
   Nothing -> do
    liftDbFunction info (_pdbRead pdb DModules mn) >>= \case
      Just mdata@(ModuleData md deps) -> do
        let newLoaded = M.fromList $ toFqDep mn (_mHash md) <$> _mDefs md
        (esLoaded . loAllLoaded) %= M.union newLoaded . M.union deps
        (esLoaded . loModules) %= M.insert mn mdata
        pure (Just mdata)
      Just mdata@(InterfaceData iface deps) -> do
        let ifDefs = mapMaybe ifDefToDef (_ifDefns iface)
        let newLoaded = M.fromList $ toFqDep mn (_ifHash iface) <$> ifDefs
        (esLoaded . loAllLoaded) %= M.union newLoaded . M.union deps
        (esLoaded . loModules) %= M.insert mn mdata
        pure (Just mdata)
      Nothing -> pure Nothing


-- | getModuleData, but only for modules, no interfaces
getModule :: i -> PactDb b i -> ModuleName -> EvalM e b i (EvalModule b i)
getModule info pdb mn = lookupModule info pdb mn >>= \case
  Just md -> pure md
  Nothing -> throwExecutionError info (ModuleDoesNotExist mn)

-- | Get or load a module or interface based on the module name
getModuleData :: i -> PactDb b i -> ModuleName -> EvalM e b i (ModuleData b i)
getModuleData info pdb mn = lookupModuleData info pdb mn >>= \case
  Just md -> pure md
  Nothing -> throwExecutionError info (ModuleDoesNotExist mn)

-- | Returns a module member, but only for modules, no interfaces
getModuleMember :: i -> PactDb b i -> QualifiedName -> EvalM e b i (EvalDef b i)
getModuleMember info pdb (QualifiedName qn mn) = do
  md <- getModule info pdb mn
  case findDefInModule qn md of
    Just d -> pure d
    Nothing -> do
      let fqn = FullyQualifiedName mn qn (_mHash md)
      throwExecutionError info (ModuleMemberDoesNotExist fqn)

getModuleMemberWithHash :: i -> PactDb b i -> QualifiedName -> EvalM e b i (EvalDef b i, ModuleHash)
getModuleMemberWithHash info pdb (QualifiedName qn mn) = do
  md <- getModule info pdb mn
  case findDefInModule qn md of
    Just d -> pure (d, _mHash md)
    Nothing -> do
      let fqn = FullyQualifiedName mn qn (_mHash md)
      throwExecutionError info (ModuleMemberDoesNotExist fqn)


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


