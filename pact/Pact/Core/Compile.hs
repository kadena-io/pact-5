{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DerivingStrategies #-}

module Pact.Core.Compile
 ( interpretTopLevel
 , compileDesugarOnly
 , compileValueToPactValue
 , evalTopLevel
 , CompileValue(..)
 , parseOnlyProgram
 ) where

import Control.Lens
import Control.Monad
import Data.Maybe(mapMaybe)
import Data.Text(Text)
import Codec.Serialise(Serialise)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text.IO as T

import Pact.Core.Debug
import Pact.Core.Builtin
import Pact.Core.Info
import Pact.Core.Persistence
import Pact.Core.Names
import Pact.Core.IR.Desugar
import Pact.Core.Errors
import Pact.Core.Pretty
import Pact.Core.IR.Term
import Pact.Core.Guards
import Pact.Core.Environment
import Pact.Core.Capabilities
import Pact.Core.Literal
import Pact.Core.Imports
import Pact.Core.Namespace
import Pact.Core.PactValue
import Pact.Core.Hash
import Pact.Core.Interpreter
import Pact.Core.Serialise.CBOR_V1(SerialiseV1)

import qualified Pact.Core.IR.ModuleHashing as MHash
import qualified Pact.Core.IR.ConstEval as ConstEval
import qualified Pact.Core.Syntax.Lexer as Lisp
import qualified Pact.Core.Syntax.Parser as Lisp
import qualified Pact.Core.Syntax.ParseTree as Lisp
import Pact.Core.Gas
import Pact.Core.SizeOf

type HasCompileEnv b i
  = ( DesugarBuiltin b
    , Pretty b
    , IsBuiltin b
    , SizeOf i
    , SizeOf b
    , Serialise (SerialiseV1 b)
    )

parseOnlyProgram
  :: Text -> Either PactErrorI [Lisp.TopLevel SpanInfo]
parseOnlyProgram =
  Lisp.lexer >=> Lisp.parseProgram

_parseOnlyFile :: FilePath -> IO (Either PactErrorI [Lisp.TopLevel SpanInfo])
_parseOnlyFile fp = parseOnlyProgram <$> T.readFile fp


data CompileValue i
  = LoadedModule ModuleName ModuleHash
  | LoadedInterface ModuleName ModuleHash
  | LoadedImports Import
  | InterpretValue PactValue i
  deriving stock (Eq, Show)


compileValueToPactValue :: CompileValue i -> PactValue
compileValueToPactValue = \case
  InterpretValue v _ -> v
  cv -> PString (renderCompactText cv)

instance Pretty (CompileValue i) where
  pretty = \case
    LoadedModule mn mh ->
      "Loaded module" <+> pretty mn <> ", hash" <+> pretty (moduleHashToText mh)
    LoadedInterface mn mh ->
      "Loaded interface" <+> pretty mn <> ", hash" <+> pretty (moduleHashToText mh)
    InterpretValue v _ -> pretty v
    LoadedImports i ->
      "Loaded imports from" <> pretty (_impModuleName i)


enforceNamespaceInstall
  :: i
  -> Interpreter e b i
  -> EvalM e b i ()
enforceNamespaceInstall info interpreter =
  use (esLoaded . loNamespace) >>= \case
    Just ns ->
      void $ interpretGuard interpreter info (_nsUser ns)
    Nothing ->
      enforceRootNamespacePolicy
    where
    enforceRootNamespacePolicy = do
      policy <- viewEvalEnv eeNamespacePolicy
      unless (allowRoot policy) $
        throwExecutionError info RootNamespaceInstallError
    allowRoot SimpleNamespacePolicy = True
    allowRoot (SmartNamespacePolicy ar _) = ar

-- | Evaluate module governance
evalModuleGovernance
  :: (HasCompileEnv b i)
  => Interpreter e b i
  -> Lisp.TopLevel i
  -> EvalM e b i ()
evalModuleGovernance interpreter tl = do
  lo <- use esLoaded
  case tl of
    Lisp.TLModule m -> do
      let info = Lisp._mInfo m
      let unmangled = Lisp._mName m
      mname <- mangleNamespace unmangled
      lookupModule (Lisp._mInfo m) mname >>= \case
        Just targetModule -> do
          case _mGovernance targetModule of
            KeyGov ksn -> do
              let ksnTerm = Constant (LString (renderKeySetName ksn)) info
                  ksrg = App (Builtin (liftCoreBuiltin CoreKeysetRefGuard) info) (pure ksnTerm) info
                  term = App (Builtin (liftCoreBuiltin CoreEnforceGuard) info) (pure ksrg) info
              void $ eval interpreter PImpure term
            CapGov (FQName fqn) -> do
              hasModAdmin <- uses (esCaps . csModuleAdmin) (S.member mname)
              -- check whether we already have module admin.
              -- if we do, we don't need to run governance
              if hasModAdmin then pure ()
              else do
                let unitBody = Constant LUnit info
                let ct = CapToken (fqnToQualName fqn) []
                void $ evalWithCapability interpreter info PImpure ct unitBody
                esCaps . csModuleAdmin %= S.insert mname
          -- | Restore the state to pre-module admin acquisition
          esLoaded .= lo
        Nothing -> enforceNamespaceInstall info interpreter
    Lisp.TLInterface iface -> do
      let info = Lisp._ifInfo iface
      let unmangled = Lisp._ifName iface
      ifn <- mangleNamespace unmangled
      lookupModuleData info ifn >>= \case
        Nothing -> enforceNamespaceInstall info interpreter
        Just _ ->
          throwExecutionError info  (CannotUpgradeInterface ifn)
    _ -> pure ()


compileDesugarOnly
  :: forall e b i
  .  (HasCompileEnv b i)
  => Interpreter e b i
  -> Lisp.TopLevel i
  -> EvalM e b i (EvalTopLevel b i, S.Set ModuleName)
compileDesugarOnly interpreter tl = do
  evalModuleGovernance interpreter tl
  -- Todo: pretty instance for modules and all of toplevel
  debugPrint (DPParser @b) tl
  (DesugarOutput ds deps) <- runDesugarTopLevel tl
  constEvaled <- ConstEval.evalTLConsts interpreter ds
  let tlFinal = MHash.hashTopLevel constEvaled
  debugPrint DPDesugar ds
  pure (tlFinal, deps)

interpretTopLevel
  :: forall e b i
  .  (HasCompileEnv b i)
  => Interpreter e b i
  -> Lisp.TopLevel i
  -> EvalM e b i (CompileValue i)
interpretTopLevel interpreter tl = do
  evalModuleGovernance interpreter tl
  -- Todo: pretty instance for modules and all of toplevel
  debugPrint (DPParser @b) tl
  (DesugarOutput ds deps) <- runDesugarTopLevel tl
  constEvaled <- ConstEval.evalTLConsts interpreter ds
  let tlFinal = MHash.hashTopLevel constEvaled
  debugPrint DPDesugar ds
  evalTopLevel interpreter tlFinal deps

evalTopLevel
  :: forall e b i
  .  (HasCompileEnv b i)
  => Interpreter e b i
  -> EvalTopLevel b i
  -> S.Set ModuleName
  -> EvalM e b i (CompileValue i)
evalTopLevel interpreter tlFinal deps = do
  lo0 <- use esLoaded
  pdb <- viewEvalEnv eePactDb
  case tlFinal of
    TLModule m -> do
      -- enforce new module keyset on install
      case _mGovernance m of
        KeyGov ksn ->
          () <$ interpretGuard interpreter (_mInfo m) (GKeySetRef ksn)
      -- governance is granted on install without testing the cap.
      -- rationale is governance might be some vote or something
      -- that doesn't exist yet, or something like non-upgradable governance.
      -- Of course, if governance is
      -- busted somehow, this means we won't find out, and
      -- can't fix it later.
        CapGov _ -> pure ()
      let deps' = M.filterWithKey (\k _ -> S.member (_fqModule k) deps) (_loAllLoaded lo0)
          mdata = ModuleData m deps'
      mSize <- sizeOf (_mInfo m) SizeOfV0 m
      chargeGasArgs (_mInfo m) (GModuleMemory mSize)
      evalWrite (_mInfo m) pdb Write DModules (view mName m) mdata
      let fqDeps = toFqDep (_mName m) (_mHash m) <$> _mDefs m
          newLoaded = M.fromList fqDeps
          newTopLevel = M.fromList $ (\(fqn, d) -> (_fqName fqn, (fqn, defKind (_mName m) d))) <$> fqDeps
          loadNewModule =
            over loModules (M.insert (_mName m) mdata) .
            over loAllLoaded (M.union newLoaded) .
            over loToplevel (M.union newTopLevel)
      esLoaded %= loadNewModule
      esCaps . csModuleAdmin %= S.union (S.singleton (_mName m))
      pure (LoadedModule (_mName m) (_mHash m))
    TLInterface iface -> do
      let deps' = M.filterWithKey (\k _ -> S.member (_fqModule k) deps) (_loAllLoaded lo0)
          mdata = InterfaceData iface deps'
      ifaceSize <- sizeOf (_ifInfo iface) SizeOfV0 iface
      chargeGasArgs (_ifInfo iface) (GModuleMemory ifaceSize)
      evalWrite (_ifInfo iface) pdb Write DModules (view ifName iface) mdata
      let fqDeps = toFqDep (_ifName iface) (_ifHash iface)
                  <$> mapMaybe ifDefToDef (_ifDefns iface)
          newLoaded = M.fromList fqDeps
          newTopLevel = M.fromList
                        $ (\(fqn, d) -> (_fqName fqn, (fqn, defKind (_ifName iface) d)))
                        <$> fqDeps
          loadNewModule =
            over loModules (M.insert (_ifName iface) mdata) .
            over loAllLoaded (M.union newLoaded) .
            over loToplevel (M.union newTopLevel)
      esLoaded %= loadNewModule
      pure (LoadedInterface (view ifName iface) (view ifHash iface))
    TLTerm term -> (`InterpretValue` (view termInfo term)) <$> eval interpreter PImpure term
    TLUse imp _ -> pure (LoadedImports imp)
{-# INLINE evalTopLevel #-}
