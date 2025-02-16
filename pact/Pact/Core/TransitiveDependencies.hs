{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Pact.Core.TransitiveDependencies(getAllTransitiveDependencies) where

import Control.Lens
import Control.Monad
import Control.Monad.State.Strict
import Data.Foldable(traverse_)
import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Pact.Core.Environment
import Pact.Core.Names
import Pact.Core.IR.Term
import Pact.Core.Persistence
import Pact.Core.IR.Eval.Runtime.Utils
import Pact.Core.Capabilities
import Pact.Core.SizeOf
import Pact.Core.Gas

newtype WorkingSet =
  WorkingSet (S.Set FullyQualifiedName)
  deriving (Eq, Show)

newtype DependencySet =
  DependencySet (S.Set FullyQualifiedName)
  deriving (Eq, Show)


data TransitiveClosureState =
  TransitiveClosureState
  { _workingSet :: WorkingSet
  , _dependencySet :: DependencySet
  }

makeLenses ''TransitiveClosureState

isInDependencySet :: FullyQualifiedName -> DependencySet -> Bool
isInDependencySet fqn (DependencySet s) = S.member fqn s

addToDependencySet :: FullyQualifiedName -> DependencySet -> DependencySet
addToDependencySet fqn (DependencySet s) = DependencySet (S.insert fqn s)

addToWorkingSet :: FullyQualifiedName -> WorkingSet -> WorkingSet
addToWorkingSet fqn (WorkingSet s) = WorkingSet (S.insert fqn s)

popFromWorkingSet :: WorkingSet -> Maybe (FullyQualifiedName, WorkingSet)
popFromWorkingSet (WorkingSet s) = over _2 WorkingSet <$> S.minView s

type TransitiveClosureM a = State TransitiveClosureState a

-- | Get all term dependents, and insert them into the working set
getTermDependents :: EvalTerm b i -> TransitiveClosureM ()
getTermDependents = fmap void $ transformM $ \case
  e@(Var n _) -> case nameToFullyQualifiedName n of
    Just fqn -> e <$ checkDependency fqn
    Nothing -> pure e
  e -> pure e

checkDependency :: FullyQualifiedName -> TransitiveClosureM ()
checkDependency fqn = do
    ds <- use dependencySet
    unless (isInDependencySet fqn ds) $ do
      workingSet %= addToWorkingSet fqn
      dependencySet %= addToDependencySet fqn

getDefunDependents :: EvalDefun b i -> TransitiveClosureM ()
getDefunDependents (Defun _spec _args term _) =
  getTermDependents term

getDefcapMetaDependents :: DefCapMeta (FQNameRef Name) -> TransitiveClosureM ()
getDefcapMetaDependents = \case
  DefManaged (DefManagedMeta _ (FQName fq)) -> checkDependency fq
  _ -> pure ()

getDefcapDependents :: EvalDefCap b i -> TransitiveClosureM ()
getDefcapDependents (DefCap _spec _args term meta _) =
  getDefcapMetaDependents meta *> getTermDependents term

getDefPactDependents :: EvalDefPact b i -> TransitiveClosureM ()
getDefPactDependents (DefPact _spec _args steps _) =
  traverse_ getStepDependents steps
  where
  getStepDependents = \case
    Step t -> getTermDependents t
    StepWithRollback t t' ->
      getTermDependents t *> getTermDependents t'
    LegacyStepWithEntity t t' ->
      getTermDependents t *> getTermDependents t'
    LegacyStepWithRBEntity t1 t2 t3 ->
      getTermDependents t1 *> getTermDependents t2 *> getTermDependents t3


getDefDependents :: EvalDef b i -> TransitiveClosureM ()
getDefDependents = \case
  Dfun d -> getDefunDependents d
  DCap d -> getDefcapDependents d
  DPact d -> getDefPactDependents d
  DTable _ -> pure ()
  DSchema _ -> pure ()
  --NOTE: defconsts _should_ be evaluated by this point, and thus do not need
  -- their transitive closure computed
  DConst _ -> pure ()

canHaveDependents :: EvalDef b i -> Bool
canHaveDependents = \case
  Dfun{} -> True
  DCap{} -> True
  DPact{} -> True
  _ -> False

-- | Get all transitive depencen
getAllTransitiveDependencies
  :: (SizeOf b, SizeOf i)
  => i
  -> S.Set ModuleName
  -> EvalModule b i
  -> EvalM e b i (M.Map FullyQualifiedName (EvalDef b i))
getAllTransitiveDependencies i parentDirectDepSet mdl = isExecutionFlagSet FlagDisablePact51 >>= \case
  True -> oldDependencies
  False -> allDependencies
  where
  -- When we traverse a term's dependents, if we don't see
  -- a member already in the dependency set, then add it to the worklist and the dep set
  computeTransitiveDeps worklist depSet = case popFromWorkingSet worklist of
    -- We are done
    Nothing -> return depSet
    Just (e, rest) -> do
      defn <- lookupFqNameOrFail i e
      if canHaveDependents defn then do
        defnSz <- sizeOf (defInfo defn) SizeOfV0 defn
        chargeGasArgs i (GModuleOp (MOpFindTransitiveDep defnSz))
        let (TransitiveClosureState newWorkList newDepSet) = execState (getDefDependents defn) (TransitiveClosureState rest depSet)
        computeTransitiveDeps newWorkList newDepSet
      else computeTransitiveDeps rest depSet

  allDependencies = do
    let initialDepSet =  S.fromList $ view _1 . toFqDep (_mName mdl) (_mHash mdl) <$> _mDefs mdl
    DependencySet depSet <- computeTransitiveDeps (WorkingSet initialDepSet) (DependencySet initialDepSet)
    let finalDepSet = depSet `S.difference` initialDepSet
    (`M.restrictKeys` finalDepSet) <$> use (esLoaded . loAllLoaded)
  oldDependencies = do
    lo <- use esLoaded
    pure $ M.filterWithKey (\k _ -> S.member (_fqModule k) parentDirectDepSet) (_loAllLoaded lo)
