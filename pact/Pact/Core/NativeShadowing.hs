{-# LANGUAGE DerivingVia #-}


-- |
-- Module      :  Pact.Core.IR.Term
-- Copyright   :  (C) 2022 Kadena
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jose Cardona <jose@kadena.io>
--
-- Checks our parse tree for any sort of native shadowing.
-- In pact 5 we don't allow natives to be shadowed in any way, so
-- the functions in this module emit an error when natives are shadowed in
-- locally bound variables, lambdas, module definitions, interface definitions,
-- module names and interface names
--
module Pact.Core.NativeShadowing
 ( runShadowsM
 , ShadowsM
 , checkTopLevelShadows
 , checkReplTopLevelShadows
 , Shadows(..))
 where

import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Foldable(traverse_)
import Data.Text(Text)

import Pact.Core.Names
import Pact.Core.Syntax.ParseTree
import Pact.Core.Type
import Pact.Core.Pretty

import qualified Data.Map.Strict as M

-- | The context of what is being shadowed,
--   that is, the location and type of definition where shadowing is occuring.
data ShadowCtx
  = DefNameCtx DefKind QualifiedName
  -- ^ Shadowing occurs in a definition's name
  | DefArgContext DefKind Int QualifiedName
  -- ^ Shadowing occurs in a definition's argument list
  | DefBodyContext DefKind QualifiedName
  -- ^ Shadowing occurs in a definition's body (if it has one)
  | ModuleCtx ModuleName
  -- ^ Shadowing occurs in a module's name
  | InterfaceCtx ModuleName
  -- ^ Shadowing occurs in an interface's name
  | InterfaceDefCtx DefKind QualifiedName
  -- ^ Shadowing occurs in an interface's member definition name
  | InterfaceDefArgCtx DefKind Int QualifiedName
  -- ^ Shadowing occurs in an interface's member definition's arguments
  | TopLevelExprCtx
  deriving (Eq, Show)

instance Pretty ShadowCtx where
  pretty = \case
    DefNameCtx dk qn ->
      "in the definition name of" <+> pretty dk <+> pretty qn
    DefArgContext dk argIx qn ->
      "in definition" <+> pretty dk <+> pretty qn <+> "at argument position" <+> pretty argIx
    DefBodyContext dk qn ->
      "in the body of definition" <+> pretty dk <+> pretty qn
    ModuleCtx mn ->
      "in module name" <+> pretty mn
    InterfaceCtx mn ->
      "in interface name" <+> pretty mn
    InterfaceDefCtx dk qn ->
      "in the definition name of" <+> pretty dk <+> pretty qn
    InterfaceDefArgCtx dk argIx qn ->
      "in definition" <+> pretty dk <+> pretty qn <+> "at argument position" <+> pretty argIx
    TopLevelExprCtx ->
      "in top level expression"

data Shadows i
  = Shadows ShadowCtx Text i
  deriving (Eq, Show)

instance Pretty i => Pretty (Shadows i) where
  pretty (Shadows ctx shadowedVar _) =
    "Variable" <+> pretty shadowedVar <+> "shadows native of the same name" <+> pretty ctx

data ShadowsEnv b
  = ShadowsEnv
  { _context :: ShadowCtx
  , _natives :: M.Map Text b
  }

newtype ShadowsM b i a
  = ShadowsM (StateT [Shadows i] (Reader (ShadowsEnv b)) a)
  deriving
  ( Functor
  , Applicative
  , Monad
  , MonadState [Shadows i]
  , MonadReader (ShadowsEnv b)
  ) via (StateT [Shadows i] (Reader (ShadowsEnv b)))


withContext :: ShadowCtx -> ShadowsM b i a -> ShadowsM b i a
withContext ctx = local (\(ShadowsEnv _ n) -> ShadowsEnv ctx n)

viewContext :: ShadowsM b i ShadowCtx
viewContext = asks _context

-- | Enrich the context of a Def within a module (or a top level repl def)
--   with its qualified name
withModuleDefCtx :: MArg i -> (QualifiedName -> ShadowCtx) -> ShadowsM b i a -> ShadowsM b i a
withModuleDefCtx (MArg defname _ _) mkCtx act =
  viewContext >>= \case
    ModuleCtx mn -> withContext (mkCtx (QualifiedName defname mn)) act
    InterfaceCtx mn -> withContext (mkCtx (QualifiedName defname mn)) act
    -- A `def` cannot exist outside a `ModuleCtx` outside of the REPL, so we can safely assume this is a
    -- REPL def
    _ -> withContext (mkCtx (QualifiedName defname replModuleName)) act

checkExprShadows :: Expr i -> ShadowsM b i ()
checkExprShadows expr =
  () <$ transformM errorOnShadowing expr
  where
  errorOnShadowing = \case
    Let lf bndrs e i -> do
      traverse_ checkBndr bndrs
      pure $ Let lf bndrs e i
    Lam bnds exprs i -> do
      traverse_ checkMArgShadows bnds
      pure $ Lam bnds exprs i
    e -> pure e
    where
    checkBndr (Binder marg _) = checkMArgShadows marg

checkMArgShadows :: MArg i -> ShadowsM b i ()
checkMArgShadows (MArg name _mty i) = checkNativeShadows name i


checkNativeShadows :: Text -> info -> ShadowsM b info ()
checkNativeShadows name i = do
  ShadowsEnv ctx natives <- ask
  when (M.member name natives) $
    modify' (Shadows ctx name i:)


checkDefunShadows :: Defun i -> ShadowsM b i ()
checkDefunShadows (Defun spec args term _anns _) = do
  withModuleDefCtx spec (DefNameCtx DKDefun) $ checkMArgShadows spec
  forM_ (zip [0..] args) $ \(idx, arg)  ->
    withModuleDefCtx spec (DefArgContext DKDefun idx) $ checkMArgShadows arg
  withModuleDefCtx spec (DefBodyContext DKDefun) $ traverse_ checkExprShadows term

checkDefcapShadows :: DefCap i -> ShadowsM b i ()
checkDefcapShadows (DefCap spec args term _ _ _) = do
  withModuleDefCtx spec (DefNameCtx DKDefCap) $ checkMArgShadows spec
  forM_ (zip [0..] args) $ \(idx, arg)  ->
    withModuleDefCtx spec (DefArgContext DKDefCap idx) $ checkMArgShadows arg
  withModuleDefCtx spec (DefBodyContext DKDefCap) $ traverse_ checkExprShadows term

checkDefconstShadows :: DefConst i -> ShadowsM b i ()
checkDefconstShadows (DefConst spec term _ _) = do
  withModuleDefCtx spec (DefNameCtx DKDefConst) $ checkMArgShadows spec
  withModuleDefCtx spec (DefBodyContext DKDefConst) $ checkExprShadows term
-- Note: We don't have to check whether `args` shadow here
-- since they're object fields
checkDefSchemaShadows :: DefSchema i -> ShadowsM b i ()
checkDefSchemaShadows (DefSchema name _args _ i) = viewContext >>= \case
    ModuleCtx mn -> do
      let qn = QualifiedName name mn
      withContext (DefNameCtx (DKDefSchema (fakeSchema qn)) qn) act
    -- A `def` cannot exist outside a `ModuleCtx` outside of the REPL, so we can safely assume this is a
    -- REPL def
    -- you can't define repl defschemas yet so this case is actually impossible, but in the future it might not be
    _ -> do
      let qn = QualifiedName name replModuleName
      withContext (DefNameCtx (DKDefSchema (fakeSchema qn)) qn) act
  where
  act = checkNativeShadows name i
  -- We kind of need this because I unfortunately made `DefKind` take a parameter :(
  --
  fakeSchema qn = Schema qn mempty

checkDeftableShadows :: DefTable i -> ShadowsM b i ()
checkDeftableShadows (DefTable name _ _ i) =
  withModuleDefCtx (MArg name Nothing i) (DefNameCtx DKDefTable) $
    checkNativeShadows name i

checkDefpactShadows :: DefPact i -> ShadowsM b i ()
checkDefpactShadows (DefPact spec args steps _ann _) = do
  withModuleDefCtx spec (DefNameCtx DKDefPact) $ checkMArgShadows spec
  forM_ (zip [0..] args) $ \(idx, arg)  ->
    withModuleDefCtx spec (DefArgContext DKDefPact idx) $ checkMArgShadows arg
  withModuleDefCtx spec (DefBodyContext DKDefPact) $ traverse_ checkShadowedStep steps
  where
  checkShadowedStep = \case
    Step me e _ -> do
      traverse_ checkExprShadows me
      checkExprShadows e
    StepWithRollback me e1 e2 _ -> do
      traverse_ checkExprShadows me
      traverse_ checkExprShadows [e1, e2]

checkDefShadows :: Def i -> ShadowsM b i ()
checkDefShadows = \case
  Dfun d -> checkDefunShadows d
  DConst d -> checkDefconstShadows d
  DCap d -> checkDefcapShadows d
  DSchema d -> checkDefSchemaShadows d
  DTable d -> checkDeftableShadows d
  DPact d -> checkDefpactShadows d

checkModuleShadows :: Module i -> ShadowsM b i ()
checkModuleShadows (Module mname _gov _exts defs _anns i) = withContext (ModuleCtx (ModuleName mname Nothing)) $ do
  checkNativeShadows mname i
  traverse_ checkDefShadows defs

checkIfDefunShadows :: IfDefun i -> ShadowsM b i ()
checkIfDefunShadows (IfDefun spec args _anns _) = do
  withModuleDefCtx spec (DefNameCtx DKDefun) $ checkMArgShadows spec
  forM_ (zip [0..] args) $ \(idx, arg)  ->
    withModuleDefCtx spec (DefArgContext DKDefun idx) $ checkMArgShadows arg

checkIfDefcapShadows :: IfDefCap i -> ShadowsM b i ()
checkIfDefcapShadows (IfDefCap spec args _ _ _) = do
  withModuleDefCtx spec (DefNameCtx DKDefCap) $ checkMArgShadows spec
  forM_ (zip [0..] args) $ \(idx, arg)  ->
    withModuleDefCtx spec (DefArgContext DKDefCap idx) $ checkMArgShadows arg

checkIfDefpactShadows :: IfDefPact i -> ShadowsM b i ()
checkIfDefpactShadows (IfDefPact spec args _ _) = do
  withModuleDefCtx spec (DefNameCtx DKDefPact) $ checkMArgShadows spec
  forM_ (zip [0..] args) $ \(idx, arg)  ->
    withModuleDefCtx spec (DefArgContext DKDefPact idx) $ checkMArgShadows arg

checkIfDefShadows :: IfDef i  -> ShadowsM b i ()
checkIfDefShadows = \case
  IfDfun d -> checkIfDefunShadows d
  IfDConst d -> checkDefconstShadows d
  IfDCap dc -> checkIfDefcapShadows dc
  IfDSchema ds -> checkDefSchemaShadows ds
  IfDPact dp -> checkIfDefpactShadows dp

checkInterfaceShadows :: Interface i -> ShadowsM b i ()
checkInterfaceShadows (Interface ifn defns _ _ i) = withContext (InterfaceCtx (ModuleName ifn Nothing)) $ do
  checkNativeShadows ifn i
  traverse_ checkIfDefShadows defns

checkTopLevelShadows :: TopLevel i -> ShadowsM b i ()
checkTopLevelShadows = \case
  TLModule m -> checkModuleShadows m
  TLInterface i -> checkInterfaceShadows i
  TLTerm e -> checkExprShadows e
  TLUse _ -> pure ()

checkReplTopLevelShadows :: ReplTopLevel i -> ShadowsM b i ()
checkReplTopLevelShadows = \case
  RTLTopLevel tl -> checkTopLevelShadows tl
  RTLDefun df -> checkDefunShadows df
  RTLDefConst dc -> checkDefconstShadows dc

runShadowsM :: M.Map Text b -> ShadowsM b i a -> (a, [Shadows i])
runShadowsM e (ShadowsM act) =
  runReader (runStateT act []) (ShadowsEnv TopLevelExprCtx e)