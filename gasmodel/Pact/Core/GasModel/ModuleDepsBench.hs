
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Pact.Core.GasModel.ModuleDepsBench where

-- |
-- Module      :  Pact.Core.GasModel.ModuleDepsBench
-- Copyright   :  (C) 2025 Kadena
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jose Cardona <jose@kadena.io>
--
-- Benchmarks for our transitive dependency handling
--
import Criterion as C
import Control.Monad.State
import Control.Monad
import Control.Monad.Reader
import Control.Lens
import Data.IORef
import Pact.Core.Names
import Pact.Core.Hash
import Pact.Core.Gen
import Pact.Core.IR.Term

import Hedgehog hiding (Var)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Set as S
import NeatInterpolation (text)
import Pact.Core.Builtin
import Pact.Core.Info (LineInfo)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Pact.Core.Type
import Data.Default
import Pact.Core.Capabilities
import Pact.Core.Guards
import Pact.Core.Environment
import Pact.Core.TransitiveDependencies
import Pact.Core.Persistence
import Pact.Core.PactValue
import Pact.Core.Errors
import Pact.Core.SPV
import Pact.Core.Namespace
import Pact.Core.Gas
import Pact.Core.Persistence.MockPersistence (mockPactDb)
import Pact.Core.Serialise (serialisePact_lineinfo_pact51)
import Pact.Core.GasModel.ModuleLoadBench(sampleWithSeed)

data DPGenState
  = DPGenState
  { _validIdents :: M.Map FullyQualifiedName (S.Set FullyQualifiedName)
  -- ^ A map from a generated fully qualified name to the set of its dependencies
  , _currentDef :: FullyQualifiedName
  -- ^ The current definition for which we are generating terms
  , _existingModules :: S.Set ModuleName
  -- ^ The set of existing modules
  , _allDefs :: M.Map FullyQualifiedName (EvalDef CoreBuiltin LineInfo)
  -- ^ The map of all generated defs
  }

makeLenses 'DPGenState

-- | A DepGenM action:
--   - Can generate a term within a particular module context
--   - Can record the set of dependencies for a newly generated function
type DepGenM a = ReaderT (ModuleName, ModuleHash) (StateT DPGenState Gen) a


emptyModule :: ModuleName
emptyModule = ModuleName "" Nothing
emptyModuleHash :: ModuleHash
emptyModuleHash = ModuleHash (Hash "")

-- | Run a DepGenM action and return the set of all generated definitions
runDepGenM :: DepGenM a -> Gen (a, M.Map FullyQualifiedName (EvalDef CoreBuiltin LineInfo))
runDepGenM m = do
  let initialState = DPGenState mempty (FullyQualifiedName emptyModule "" emptyModuleHash) mempty mempty
  over _2 (view allDefs) <$> runStateT (runReaderT m (ModuleName "" Nothing, ModuleHash defaultPactHash)) initialState

-- | Generate `BuiltinForm`s for our generated `EvalTerm`s
evalTermBuiltinFormGen :: DepGenM (BuiltinForm (EvalTerm CoreBuiltin LineInfo))
evalTermBuiltinFormGen = Gen.choice
  [ CAnd <$> evalTermGen <*> evalTermGen
  , COr <$> evalTermGen <*> evalTermGen
  , CIf <$> evalTermGen <*> evalTermGen <*> evalTermGen
  , CEnforceOne <$> evalTermGen <*> (ListLit <$> Gen.list (Range.linear 0 16) (evalTermGen) <*> (liftGen lineInfoGen))
  , CEnforce <$> evalTermGen <*> evalTermGen
  , CWithCapability <$> evalTermGen <*> evalTermGen
  , CTry <$> evalTermGen <*> evalTermGen
  , CCreateUserGuard <$> evalTermGen
  ]

liftGen :: Gen a -> DepGenM a
liftGen = lift . lift

-- | Generate a new definition name, ensuring it does not clash with one of the already generated names.
--   newModuleDefNameGen creates a new fqn to add to the dep set
newModuleDefNameGen :: DepGenM FullyQualifiedName
newModuleDefNameGen = do
  (mdl, mh) <- ask
  ident <- liftGen identGen
  let newFQN = FullyQualifiedName mdl ident mh
  idents <- use validIdents
  liftGen $ guard (M.notMember newFQN idents)
  -- Dependency list starts empty
  validIdents %= M.insert newFQN S.empty
  pure newFQN

-- | Picks a fully qualified name reference from the set of valid fqns,
--
--   Adds the selected identifier's dependencies to the current function being generateed's dependencies,
--   to ensure that it maintains a correct by construction transitive closure
fqNameGen :: DepGenM FullyQualifiedName
fqNameGen = do
  idents <- use validIdents
  currDef <- use currentDef

  -- Select any of the already generated identifiers. Recursive is fine,
  -- even if pact doesn't produce recursive definitions, the algorithm for the
  -- transitive closure should handle recursion naturally
  fqn <- liftGen $ Gen.choice (pure <$> (M.keys idents))


  -- Inductive step:
  -- For every new dependency pulled in by this generator, we must include its dependencies
  -- into the dep list of the current def. This way, we generate a correct by construction
  -- set of dependents.
  --
  -- In other words, if n ∈ Deps(f), then {n} ⋃ Deps(n) ⊂ Deps(f)
  dependents <- S.insert fqn <$> uses validIdents (M.! fqn)

  validIdents %= M.insertWith S.union currDef dependents
  pure fqn

-- | Generates a valid Name reference
--   see See: fqNameGen.
evalNameGen :: DepGenM Name
evalNameGen = do
  FullyQualifiedName mn n mh <- fqNameGen
  pure (Name n (NTopLevel mn mh))

-- | Generate a term where the only free variables references are in the valid set we have generated
evalTermGen :: DepGenM (EvalTerm CoreBuiltin LineInfo)
evalTermGen = Gen.recursive Gen.choice
  [ Var <$> evalNameGen <*> liftGen lineInfoGen
  , Builtin <$> Gen.enumBounded <*> liftGen lineInfoGen
  , Constant <$> liftGen literalGen <*> liftGen lineInfoGen
  ]
  [ Lam <$> Gen.nonEmpty (Range.linear 1 16) (liftGen (argGen lineInfoGen)) <*> evalTermGen <*> liftGen lineInfoGen
  , Let <$> liftGen (argGen lineInfoGen) <*> evalTermGen <*> evalTermGen <*> liftGen lineInfoGen
  , App <$> evalTermGen <*> Gen.list (Range.linear 0 16) (evalTermGen) <*> liftGen lineInfoGen
  , Sequence <$> evalTermGen <*> evalTermGen <*> liftGen lineInfoGen
  , Nullary <$> evalTermGen <*> liftGen lineInfoGen
  , BuiltinForm <$> evalTermBuiltinFormGen <*> liftGen lineInfoGen
  , ListLit <$> Gen.list (Range.linear 1 16) (evalTermGen)<*> liftGen lineInfoGen
  , ObjectLit <$> Gen.list (Range.linear 1 16) ((,) <$> liftGen fieldGen <*> evalTermGen) <*> liftGen lineInfoGen
  ]

evalDefunGen :: DepGenM (Defun Name Type CoreBuiltin LineInfo)
evalDefunGen = do
  fqn <- newModuleDefNameGen
  currentDef .= fqn
  args <- Gen.list (Range.linear 0 10) (liftGen (argGen lineInfoGen))
  term <- evalTermGen
  let spec = Arg (_fqName fqn) Nothing def
  pure $ Defun spec args term def

evalDefCapGen :: DepGenM (DefCap Name Type CoreBuiltin LineInfo)
evalDefCapGen = do
  fqn <- newModuleDefNameGen
  currentDef .= fqn
  args <- Gen.list (Range.linear 0 10) (liftGen (argGen lineInfoGen))
  term <- evalTermGen
  mgd <- managedGen
  let spec = Arg (_fqName fqn) Nothing def
  pure $ DefCap spec args term mgd def
  where
  managedGen = Gen.choice
    [ pure Unmanaged, pure DefEvent, DefManaged . DefManagedMeta (0, "a") . FQName <$> fqNameGen]

evalDefPactGen :: DepGenM (EvalDefPact CoreBuiltin LineInfo)
evalDefPactGen = do
  fqn <- newModuleDefNameGen
  currentDef .= fqn
  args <- Gen.list (Range.linear 0 10) (liftGen (argGen lineInfoGen))
  let spec = Arg (_fqName fqn) Nothing def
  steps <- Gen.nonEmpty (Range.constant 1 5) stepGen'
  pure $ DefPact spec args steps def
  where
  stepGen' = Gen.choice
    [ Step <$> evalTermGen, StepWithRollback <$> evalTermGen <*> evalTermGen ]

evalDefGen :: DepGenM (EvalDef CoreBuiltin LineInfo)
evalDefGen =
  Gen.choice
  [ DCap <$> evalDefCapGen
  , Dfun <$> evalDefunGen
  , DPact <$> evalDefPactGen
  ]

-- | Generate a module along with its set of dependent functions.
newModuleGen :: DepGenM (EvalModule CoreBuiltin LineInfo, S.Set FullyQualifiedName)
newModuleGen = do
  -- create a new module name, and ensure it doesn't overlap with the others
  ident <- liftGen moduleNameGen
  use existingModules >>= guard . S.notMember ident
  -- Add it to the set of new modules
  existingModules %= S.insert ident
  mh <- liftGen moduleHashGen
  local (const (ident, mh)) $ do
    defs <- Gen.list (Range.singleton 10) evalDefGen
    let gov = KeyGov (KeySetName "foo" Nothing)
    let mcode = ModuleCode ""
        fqDeps = toFqDep ident mh <$> defs
        moduleKeys = S.fromList $ fmap fst $ fqDeps
    allDefs %= (`M.union` M.fromList fqDeps)
    allIdents <- use validIdents
    let identDeps = M.restrictKeys allIdents moduleKeys
        expectedDeps = S.filter (\f -> _fqModule f /= ident) $ S.unions identDeps
        outModule = Module ident gov defs mempty [] [] mh defaultPactHash mcode def
    pure (outModule, expectedDeps)

getRightIO :: Either (PactError LineInfo) b -> IO b
getRightIO = either (error . show) pure

resetEEGas :: EvalEnv b i -> IO ()
resetEEGas ee =
  writeIORef (_geGasRef $ _eeGasEnv ee) mempty

numModules :: Int
numModules = 50

mdls :: [(EvalModule CoreBuiltin LineInfo, S.Set FullyQualifiedName)]
allGeneratedDeps :: M.Map FullyQualifiedName (EvalDef CoreBuiltin LineInfo)
(mdls, allGeneratedDeps) = sampleWithSeed 1789 $ runDepGenM $ Gen.list (Range.singleton numModules) newModuleGen

-- Compute the transitive dep gas consumed for one of our generated deps
--
-- We float this out of the criterion env functions to make sure we don't actually manipulate criterion-generated data
--
getTransDepsGas :: Int -> IO SatWord
getTransDepsGas mdlIndex = do
  let (m, moduleDepSet) = mdls !! mdlIndex
  let moduleDeps = M.restrictKeys allGeneratedDeps moduleDepSet
  let allDeps = M.fromList (toFqDep (_mName m) (_mHash m) <$> _mDefs m) <> moduleDeps
  pdb <- mockPactDb serialisePact_lineinfo_pact51
  ee <- setupBenchEvalEnv pdb mempty PUnit
  let es = def & esLoaded . loAllLoaded .~ allDeps
  _ <- getRightIO =<< runEvalMResult (ExecEnv ee) es
      (getAllTransitiveDependencies def mempty m)
  MilliGas gasConsumed <- readIORef (view (eeGasEnv . geGasRef) ee)
  pure $ gasConsumed


-- To get the gas consumed vs time graph, output the graph with
-- ✗ cabal run gasmodel -- --csv <path> -m ipattern "transitive"
-- Remove all dquotes(") from the CSV, and add an extra column after `Name` that says
-- "Gas Charged".
-- 
-- For example, the csv's header will look like this
-- `Name,Mean,MeanLB,MeanUB,Stddev,StddevLB,StddevUB`
--
-- Simply change it to
-- `Name,Gas Charged,Mean,MeanLB,MeanUB,Stddev,StddevLB,StddevUB`
-- and the csv should load easily into a spreadsheet editor that can generate graphs
--
benchmarks :: IO Benchmark
benchmarks = do
  let moduleIndexes = [0..numModules - 1]
  depsGas <- traverse getTransDepsGas moduleIndexes
  pure $ C.bgroup "Transitive Dependency benchmarks" (runModuleDepsBench <$> zip moduleIndexes depsGas)


runModuleDepsBench :: (Int, SatWord) -> Benchmark
runModuleDepsBench (mdlIndex, gas) =
  bench (title (T.pack (show gas))) $ perRunEnv mkEnv $ \(ee, es, m) ->
     (getRightIO =<<) $ runEvalMResult (ExecEnv ee) es $ getAllTransitiveDependencies def mempty m
  where
  title bs =
    T.unpack [text| Benching transitive dep time vs gas consumed, ${bs} |]
  mkEnv = do
    let (m, moduleDepSet) = mdls !! mdlIndex
    let moduleDeps = M.restrictKeys allGeneratedDeps moduleDepSet
    let allDeps = M.fromList (toFqDep (_mName m) (_mHash m) <$> _mDefs m) <> moduleDeps
    pdb <- mockPactDb serialisePact_lineinfo_pact51
    ee <- setupBenchEvalEnv pdb mempty PUnit
    let es = def & esLoaded . loAllLoaded .~ allDeps
    pure (ee, es, m)

setupBenchEvalEnv
  :: PactDb CoreBuiltin i
  -> M.Map PublicKeyText (S.Set (CapToken QualifiedName PactValue))
  -> PactValue -> IO (EvalEnv CoreBuiltin i)
setupBenchEvalEnv pdb signers mBody = do
  gasRef <- newIORef mempty
  let
    gasEnv = GasEnv
      { _geGasRef = gasRef
      , _geGasLog = Nothing
      , _geGasModel = tableGasModel (MilliGasLimit (MilliGas 200_000_000))
      }
  pure $ EvalEnv
    { _eeMsgSigs = signers
    , _eeMsgVerifiers = mempty
    , _eePactDb = pdb
    , _eeMsgBody = mBody
    , _eeHash = defaultPactHash
    , _eePublicData = def
    , _eeDefPactStep = Nothing
    , _eeMode = Transactional
    , _eeFlags = mempty
    , _eeNatives = coreBuiltinMap
    , _eeNamespacePolicy = SimpleNamespacePolicy
    , _eeGasEnv = gasEnv
    , _eeSPVSupport = noSPVSupport
    , _eeWarnings = Nothing
    }

