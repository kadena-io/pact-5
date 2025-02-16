{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}


-- |
-- Module      :  Pact.Core.Test.TransitiveDependencyTests
-- Copyright   :  (C) 2025 Kadena
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jose Cardona <jose@kadena.io>
--
-- Test that our transitive dependency computations
-- work for a pregenerated set of modules with known dependencies
--
module Pact.Core.Test.TransitiveDependencyTests(tests) where

import Control.Monad.State
import Control.Monad
import Control.Monad.Reader
import Control.Lens
import Pact.Core.Names
import Pact.Core.Hash
import Pact.Core.Gen
import Pact.Core.IR.Term

import Test.Tasty
import Test.Tasty.Hedgehog
import Hedgehog hiding (Var)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
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
import Pact.Core.Persistence.MockPersistence (mockPactDb)
import Pact.Core.Serialise (serialisePact_lineinfo_pact51)
import Pact.Core.Pretty

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
    defs <- Gen.list (Range.constant 1 15) evalDefGen
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

testTransitiveDeps :: TestTree
testTransitiveDeps =
  testProperty "Transitive dependency checker passes for all generated modules" $ property $ do
    (modules, allDeps) <- forAllWith (\(m, _) -> renderCompactString' (vsep (pretty . fst <$> m))) $ runDepGenM $ Gen.list (Range.constant 1 10) newModuleGen
    pdb <- liftIO $ mockPactDb serialisePact_lineinfo_pact51
    ee <- liftIO $ defaultEvalEnv pdb coreBuiltinMap
    let es = def & esLoaded . loAllLoaded .~ allDeps
    forM_ modules $ \(m, deps) -> do
      liftIO (runEvalM (ExecEnv ee) es $ getAllTransitiveDependencies def mempty m) >>= \case
        (Right deps', _) ->
          deps' === M.restrictKeys allDeps deps
        (Left err, _) -> do
          footnoteShow err
          failure

tests :: TestTree
tests = testTransitiveDeps