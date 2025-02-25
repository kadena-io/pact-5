{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Pact.Core.TransitiveDependencies(getAllTransitiveDependencies) where

import Control.Lens
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Except
import Data.Foldable(traverse_)
import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Pact.Core.Environment
import Pact.Core.Names
import Pact.Core.IR.Term
import Pact.Core.Persistence
import Pact.Core.IR.Eval.Runtime.Utils
import Pact.Core.Capabilities
import Pact.Core.Gas
import Pact.Core.Errors

newtype WorkingSet =
  WorkingSet (S.Set FullyQualifiedName)
  deriving (Eq, Show)

newtype DependencySet =
  DependencySet (S.Set FullyQualifiedName)
  deriving (Eq, Show)

data TransitiveClosureState b i =
  TransitiveClosureState
  { _workingSet :: !WorkingSet
  , _dependencySet :: !DependencySet
  , _gasConsumed :: !MilliGas
  , _gasLimit :: !(Maybe MilliGasLimit)
  , _allDependencies :: !(M.Map FullyQualifiedName (EvalDef b i))
  }

makeLenses ''TransitiveClosureState

data TransitiveClosureCalcErr
  = TCUnboundFreeVarError FullyQualifiedName
  | TCGasLimitExceeded MilliGasLimit MilliGas
  deriving (Show)

isInDependencySet :: FullyQualifiedName -> DependencySet -> Bool
isInDependencySet fqn (DependencySet s) = S.member fqn s

addToDependencySet :: FullyQualifiedName -> DependencySet -> DependencySet
addToDependencySet fqn (DependencySet s) = DependencySet (S.insert fqn s)

addToWorkingSet :: FullyQualifiedName -> WorkingSet -> WorkingSet
addToWorkingSet fqn (WorkingSet s) = WorkingSet (S.insert fqn s)

popFromWorkingSet :: WorkingSet -> Maybe (FullyQualifiedName, WorkingSet)
popFromWorkingSet (WorkingSet s) = over _2 WorkingSet <$> S.minView s

type TransitiveClosureM b i a = ExceptT TransitiveClosureCalcErr (State (TransitiveClosureState b i)) a

costPerFQNComparison :: SatWord
costPerFQNComparison = 200

restrictKeysCost :: M.Map k a -> S.Set k -> SatWord
restrictKeysCost inputMap inputSet
  | msz == 0 = 0
  | otherwise =
      ceiling (m * log ((n / m) + 1)) * costPerFQNComparison
  where
  !msz = M.size inputMap
  !n = fromIntegral msz :: Double
  !m = fromIntegral (S.size inputSet) :: Double

-- Essentially `chargeGasArgsM` except without a gas log.
-- This is on purpose. Why?
-- chargeTransitiveGas is intended to work on small, incremental amounts of gas charge,
-- but we do not want to flood the gas log with a ton of small charge entries. This was already this case
-- before (See git history of GCountBytes)
chargeTransitiveGas :: MilliGas -> TransitiveClosureM b i ()
chargeTransitiveGas mg = use gasLimit >>= \case
  Nothing -> pure ()
  Just mlim@(MilliGasLimit mgLim) -> do
    !curr <- use gasConsumed
    let !newGasTotal = curr <> mg
    gasConsumed .= newGasTotal
    if mgLim >= newGasTotal then pure ()
    else throwError (TCGasLimitExceeded mlim newGasTotal)

-- Search is O(log n)
chargeSetSearch :: S.Set a -> TransitiveClosureM b i ()
chargeSetSearch s
  -- Gotta be careful here
  | not (S.null s) =
    let szDouble = fromIntegral (S.size s) :: Double
    in chargeTransitiveGas $ MilliGas (ceiling (log szDouble) * costPerFQNComparison)
  | otherwise = pure ()

-- | Charge for a single tick of this algorithm during tree search
--   To ensure we charge at least something in traversing our trees
chargeTick :: TransitiveClosureM b i ()
chargeTick = chargeTransitiveGas (MilliGas 5)

-- | Get all term dependents, and insert them into the working set
getTermDependents :: EvalTerm b i -> TransitiveClosureM b i ()
getTermDependents = fmap void $ transformM $ \case
  e@(Var n _) -> chargeTick *> case nameToFullyQualifiedName n of
    Just fqn -> e <$ checkDependency fqn
    Nothing -> pure e
  e -> do
    chargeTick
    pure e

-- | Check whether a particular fully qualified name is part of our currently computed dependency set.
--   If it isn't, add it to the dependency set, as well as the worklist.
checkDependency :: FullyQualifiedName -> TransitiveClosureM b i ()
checkDependency fqn = do
    ds@(DependencySet d) <- use dependencySet
    chargeSetSearch d
    unless (isInDependencySet fqn ds) $ do
      WorkingSet w <- use workingSet
      -- Insert costs the same as search, so charge for this as well
      chargeSetSearch d
      chargeSetSearch w
      workingSet %= addToWorkingSet fqn
      dependencySet %= addToDependencySet fqn

getDefunDependents :: EvalDefun b i -> TransitiveClosureM b i ()
getDefunDependents (Defun _spec _args term _) =
  getTermDependents term

getDefcapMetaDependents :: DefCapMeta (FQNameRef Name) -> TransitiveClosureM b i ()
getDefcapMetaDependents = \case
  DefManaged (DefManagedMeta _ (FQName fq)) -> checkDependency fq
  _ -> pure ()

getDefcapDependents :: EvalDefCap b i -> TransitiveClosureM b i ()
getDefcapDependents (DefCap _spec _args term meta _) =
  getDefcapMetaDependents meta *> getTermDependents term

getDefPactDependents :: EvalDefPact b i -> TransitiveClosureM b i ()
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


getDefDependents :: EvalDef b i -> TransitiveClosureM b i ()
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

popWorkingSet :: TransitiveClosureM b i (Maybe FullyQualifiedName)
popWorkingSet =
  uses workingSet popFromWorkingSet >>= \case
    Nothing -> pure Nothing
    Just (e, rest) -> do
      workingSet .= rest
      pure (Just e)

lookupFqn :: FullyQualifiedName -> TransitiveClosureM b i (EvalDef b i)
lookupFqn fqn = uses allDependencies (M.lookup fqn) >>= \case
  Just defn -> pure defn
  Nothing -> throwError (TCUnboundFreeVarError fqn)

-- | Compute all transitive dependencies from our initial working set.
--   The algorithm is pretty simple: If a particular component can have dependents,
--   then we traverse its tree and collect all of its dependents, and push those not currently in
--   the dependency set into it as well as the working set.
--
--   The algorithm only ever grows the dependency set, it will never shrink it, thus
--   it is guaranteed to terminate
computeTransitiveDeps :: TransitiveClosureM b i ()
computeTransitiveDeps = computeUntilDone
  where
  computeUntilDone =
    popWorkingSet >>= \case
      -- We are done
      Nothing -> pure ()
      -- We popped an element form the worklist
      Just e -> do
        defn <- lookupFqn e
        when (canHaveDependents defn) $ getDefDependents defn
        computeUntilDone

-- | Get all transitive dependencies for a particular module, falling back to
--   Pre-pact 5.1 bug if the flag is not enabled
getAllTransitiveDependencies
  :: i
  -> S.Set ModuleName
  -> EvalModule b i
  -> EvalM e b i (M.Map FullyQualifiedName (EvalDef b i))
getAllTransitiveDependencies i parentDirectDepSet mdl = isExecutionFlagSet FlagDisablePact51 >>= \case
  True -> oldDependencies
  False -> getAllDependencies
  where
  tcloErrToPactError = \case
    TCGasLimitExceeded mlim amt ->
      PEExecutionError (GasExceeded (milliGasToGasLimit mlim) (milliGasToGas amt)) [] i
    TCUnboundFreeVarError fqn -> PEExecutionError (InvariantFailure (InvariantUnboundFreeVariable fqn)) [] i

  -- Calculate all of our dependencies from `computeTransitiveDeps`
  getAllDependencies = do
    let initialDepSet = S.fromList $ view _1 . toFqDep (_mName mdl) (_mHash mdl) <$> _mDefs mdl
    allDeps <- use loAllLoaded
    currGasConsumed@(MilliGas currGas) <- getGas
    mgasLimit <- viewEvalEnv (eeGasEnv . geGasModel . gmGasLimit)
    let tcloState = TransitiveClosureState
                    { _workingSet = WorkingSet initialDepSet
                    , _dependencySet = DependencySet initialDepSet
                    , _gasConsumed = currGasConsumed
                    , _gasLimit = mgasLimit
                    , _allDependencies = allDeps }
    case runState (runExceptT computeTransitiveDeps) tcloState of
      (c, TransitiveClosureState _ws (DependencySet depSet) (MilliGas consumedByAlgorithm) _glim _allDeps) -> do
        -- The way this module works, it starts at the current gas consumed, but does not charge gas
        -- to our gas ref directly, so what we want is the difference of gas consumed post algorithm
        -- exec vs gas pre-algorithm exec
        -- We charge gas regardless here, which may raise
        let consumedAmt = consumedByAlgorithm - currGas + restrictKeysCost allDeps depSet
        chargeGasArgs i (GModuleOp (MOpFindTransitiveDep consumedAmt))
        case c of
          -- Note: this case is actually impossible _UNLESS_ there is an invariant error, because
          -- the call to chargeGasArgs above will raise the gas limit exceeded error before we ever
          -- reach this branch
          Left err ->
            throwError $ tcloErrToPactError err
          Right _ -> do
            let finalDepSet = depSet `S.difference` initialDepSet
            (`M.restrictKeys` finalDepSet) <$> use (esLoaded . loAllLoaded)
  -- Old pre-pact 5.1 behavior
  oldDependencies = do
    lo <- use esLoaded
    pure $ M.filterWithKey (\k _ -> S.member (_fqModule k) parentDirectDepSet) (_loAllLoaded lo)
