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
 ( liftShadowsMEvalM
 , runShadowsM
 , ShadowsM
 , checkTopLevelShadows
 , checkReplTopLevelShadows)
 where

import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except
import Data.Foldable(traverse_)
import Data.Text(Text)

import Pact.Core.Errors
import Pact.Core.Environment
import Pact.Core.Syntax.ParseTree

import qualified Data.Map.Strict as M

newtype ShadowsM b i a
  = ShadowsM (ExceptT (PactError i) (Reader (M.Map Text b)) a)
  deriving
  ( Functor
  , Applicative
  , Monad
  , MonadError (PactError i)
  , MonadReader (M.Map Text b)
  ) via (ExceptT (PactError i) (Reader (M.Map Text b)))

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
  natives <- ask
  when (M.member name natives) $
    throwError (PEDesugarError (InvalidNativeShadowing name) i)


checkDefunShadows :: Defun i -> ShadowsM b i ()
checkDefunShadows (Defun spec args term _anns _) = do
  traverse_ checkMArgShadows (spec:args)
  traverse_ checkExprShadows term

checkDefcapShadows :: DefCap i -> ShadowsM b i ()
checkDefcapShadows (DefCap spec args term _ _ _) = do
  traverse_ checkMArgShadows (spec:args)
  traverse_ checkExprShadows term

checkDefconstShadows :: DefConst i -> ShadowsM b i ()
checkDefconstShadows (DefConst spec term _ _) = do
  checkMArgShadows spec
  checkExprShadows term

-- Note: We don't have to check whether `args` shadow here
-- since they're object fields
checkDefSchemaShadows :: DefSchema i -> ShadowsM b i ()
checkDefSchemaShadows (DefSchema name _args _ i) = do
  checkNativeShadows name i

checkDeftableShadows :: DefTable i -> ShadowsM b i ()
checkDeftableShadows (DefTable name _ _ i) = checkNativeShadows name i

checkDefpactShadows :: DefPact i -> ShadowsM b i ()
checkDefpactShadows (DefPact spec args steps _ann _) = do
  traverse_ checkMArgShadows (spec:args)
  traverse_ checkShadowedStep steps
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
checkModuleShadows (Module mname _gov _exts defs _anns i) = do
  checkNativeShadows mname i
  traverse_ checkDefShadows defs

checkIfDefunShadows :: IfDefun i -> ShadowsM b i ()
checkIfDefunShadows (IfDefun spec args _anns _) =
  traverse_ checkMArgShadows (spec:args)

checkIfDefcapShadows :: IfDefCap i -> ShadowsM b i ()
checkIfDefcapShadows (IfDefCap spec args _ _ _) =
  traverse_ checkMArgShadows (spec:args)

checkIfDefpactShadows :: IfDefPact i -> ShadowsM b i ()
checkIfDefpactShadows (IfDefPact spec args _ _) =
  traverse_ checkMArgShadows (spec:args)

checkIfDefShadows :: IfDef i  -> ShadowsM b i ()
checkIfDefShadows = \case
  IfDfun d -> checkIfDefunShadows d
  IfDConst d -> checkDefconstShadows d
  IfDCap dc -> checkIfDefcapShadows dc
  IfDSchema ds -> checkDefSchemaShadows ds
  IfDPact dp -> checkIfDefpactShadows dp

checkInterfaceShadows :: Interface i -> ShadowsM b i ()
checkInterfaceShadows (Interface ifn defns _ _ i) = do
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

runShadowsM :: M.Map Text b -> ShadowsM b i a -> Either (PactError i) a
runShadowsM e (ShadowsM act) =
  runReader (runExceptT act) e

liftShadowsMEvalM :: ShadowsM b i a -> EvalM e b i a
liftShadowsMEvalM act = do
  natives <- viewEvalEnv eeNatives
  liftEither $ runShadowsM natives act
