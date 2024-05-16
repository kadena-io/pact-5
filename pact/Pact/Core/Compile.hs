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

module Pact.Core.Compile
 ( interpretTopLevel
 , compileDesugarOnly
 , evalTopLevel
 , CompileValue(..)
 ) where

import Control.Lens
import Control.Monad.Except
import Control.Monad
import Data.Maybe(mapMaybe)
import Data.Text(Text)
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
import Pact.Core.IR.Eval.Runtime

import qualified Pact.Core.IR.Eval.CEK as Eval
import qualified Pact.Core.IR.ModuleHashing as MHash
import qualified Pact.Core.IR.ConstEval as ConstEval
import qualified Pact.Core.Syntax.Lexer as Lisp
import qualified Pact.Core.Syntax.Parser as Lisp
import qualified Pact.Core.Syntax.ParseTree as Lisp
import Pact.Core.Gas
import Pact.Core.SizeOf

type HasCompileEnv step b i m
  = ( MonadEval b i m
    , DesugarBuiltin b
    , Pretty b
    , IsBuiltin b
    , PhaseDebug b i m
    , SizeOf i
    , SizeOf b
    , Eval.CEKEval step b i m)

_parseOnly
  :: Text -> Either PactErrorI [Lisp.TopLevel SpanInfo]
_parseOnly source = do
  lexed <- liftEither (Lisp.lexer source)
  liftEither (Lisp.parseProgram lexed)

_parseOnlyFile :: FilePath -> IO (Either PactErrorI [Lisp.TopLevel SpanInfo])
_parseOnlyFile fp = _parseOnly <$> T.readFile fp

data CompileValue i
  = LoadedModule ModuleName ModuleHash
  | LoadedInterface ModuleName ModuleHash
  | LoadedImports Import
  | InterpretValue PactValue i
  deriving Show


enforceNamespaceInstall
  :: (HasCompileEnv step b i m)
  => i
  -> BuiltinEnv step b i m
  -> m ()
enforceNamespaceInstall info bEnv =
  useEvalState (esLoaded . loNamespace) >>= \case
    Just ns ->
      void $ Eval.interpretGuard info bEnv (_nsUser ns)
    Nothing ->
      enforceRootNamespacePolicy
    where
    enforceRootNamespacePolicy = do
      policy <- viewEvalEnv eeNamespacePolicy
      unless (allowRoot policy) $
        throwExecutionError info (NamespaceInstallError "cannot install in root namespace")
    allowRoot SimpleNamespacePolicy = True
    allowRoot (SmartNamespacePolicy ar _) = ar
{-# SPECIALIZE enforceNamespaceInstall
  :: ()
  -> CoreBuiltinEnv
  -> Eval ()  #-}

-- | Evaluate module governance
evalModuleGovernance
  :: (HasCompileEnv step b i m)
  => BuiltinEnv step b i m
  -> Lisp.TopLevel i
  -> m ()
evalModuleGovernance bEnv tl = do
  lo <- useEvalState esLoaded
  pdb <- viewEvalEnv eePactDb
  case tl of
    Lisp.TLModule m -> do
      let info = Lisp._mInfo m
      let unmangled = Lisp._mName m
      mname <- mangleNamespace unmangled
      lookupModule (Lisp._mInfo m) pdb mname >>= \case
        Just targetModule -> do
          case _mGovernance targetModule of
            KeyGov ksn -> do
              let ksnTerm = Constant (LString (renderKeySetName ksn)) info
                  ksrg = App (Builtin (liftCoreBuiltin CoreKeysetRefGuard) info) (pure ksnTerm) info
                  term = App (Builtin (liftCoreBuiltin CoreEnforceGuard) info) (pure ksrg) info
              void $ Eval.eval PImpure bEnv term
            CapGov (FQName fqn) -> do
              hasModAdmin <- usesEvalState (esCaps . csModuleAdmin) (S.member mname)
              if hasModAdmin then pure ()
              else do
                -- check whether we already have module admin.
                -- if we do, we don't need to run this.
                let cgBody = Constant LUnit info
                    withCapApp = App (Var (fqnToName fqn) info) [] info
                    term = CapabilityForm (WithCapability withCapApp cgBody) info
                void $ Eval.eval PImpure bEnv term
                esCaps . csModuleAdmin %== S.insert mname
          -- | Restore the state to pre-module admin acquisition
          esLoaded .== lo
        Nothing -> enforceNamespaceInstall info bEnv
    Lisp.TLInterface iface -> do
      let info = Lisp._ifInfo iface
      let unmangled = Lisp._ifName iface
      ifn <- mangleNamespace unmangled
      lookupModuleData info pdb ifn >>= \case
        Nothing -> enforceNamespaceInstall info bEnv
        Just _ ->
          throwExecutionError info  (CannotUpgradeInterface ifn)
    _ -> pure ()
{-# SPECIALIZE evalModuleGovernance
  :: CoreBuiltinEnv
  -> Lisp.TopLevel ()
  -> Eval ()  #-}

compileDesugarOnly
  :: forall step b i m
  .  (HasCompileEnv step b i m)
  => BuiltinEnv step b i m
  -> Lisp.TopLevel i
  -> m (EvalTopLevel b i, S.Set ModuleName)
compileDesugarOnly bEnv tl = do
  evalModuleGovernance bEnv tl
  -- Todo: pretty instance for modules and all of toplevel
  debugPrint (DPParser @b) tl
  (DesugarOutput ds deps) <- runDesugarTopLevel tl
  constEvaled <- ConstEval.evalTLConsts bEnv ds
  let tlFinal = MHash.hashTopLevel constEvaled
  debugPrint DPDesugar ds
  pure (tlFinal, deps)

interpretTopLevel
  :: forall step b i m
  .  (HasCompileEnv step b i m)
  => BuiltinEnv step b i m
  -> Lisp.TopLevel i
  -> m (CompileValue i)
interpretTopLevel bEnv tl = do
  evalModuleGovernance bEnv tl
  -- Todo: pretty instance for modules and all of toplevel
  debugPrint (DPParser @b) tl
  (DesugarOutput ds deps) <- runDesugarTopLevel tl
  constEvaled <- ConstEval.evalTLConsts bEnv ds
  let tlFinal = MHash.hashTopLevel constEvaled
  debugPrint DPDesugar ds
  evalTopLevel bEnv tlFinal deps
{-# SPECIALIZE interpretTopLevel
  :: CoreBuiltinEnv
  -> Lisp.TopLevel ()
  -> Eval (CompileValue ())  #-}

evalTopLevel
  :: forall step b i m
  .  (HasCompileEnv step b i m)
  => BuiltinEnv step b i m
  -> EvalTopLevel b i
  -> S.Set ModuleName
  -> m (CompileValue i)
evalTopLevel bEnv tlFinal deps = do
  lo0 <- useEvalState esLoaded
  pdb <- viewEvalEnv eePactDb
  case tlFinal of
    TLModule m -> do
      -- enforce new module keyset on install
      case _mGovernance m of
        KeyGov ksn ->
          () <$ Eval.interpretGuard (_mInfo m) bEnv (GKeySetRef ksn)
      -- governance is granted on install without testing the cap.
      -- rationale is governance might be some vote or something
      -- that doesn't exist yet, or something like non-upgradable governance.
      -- Of course, if governance is
      -- busted somehow, this means we won't find out, and
      -- can't fix it later.
        CapGov _ -> pure ()
      let deps' = M.filterWithKey (\k _ -> S.member (_fqModule k) deps) (_loAllLoaded lo0)
          mdata = ModuleData m deps'
      mSize <- sizeOf SizeOfV0 m
      chargeGasArgs (_mInfo m) (GModuleMemory mSize)
      writeModule (_mInfo m) pdb Write (view mName m) mdata
      let fqDeps = toFqDep (_mName m) (_mHash m) <$> _mDefs m
          newLoaded = M.fromList fqDeps
          newTopLevel = M.fromList $ (\(fqn, d) -> (_fqName fqn, (fqn, defKind (_mName m) d))) <$> fqDeps
          loadNewModule =
            over loModules (M.insert (_mName m) mdata) .
            over loAllLoaded (M.union newLoaded) .
            over loToplevel (M.union newTopLevel)
      esLoaded %== loadNewModule
      esCaps . csModuleAdmin %== S.union (S.singleton (_mName m))
      pure (LoadedModule (_mName m) (_mHash m))
    TLInterface iface -> do
      let deps' = M.filterWithKey (\k _ -> S.member (_fqModule k) deps) (_loAllLoaded lo0)
          mdata = InterfaceData iface deps'
      ifaceSize <- sizeOf SizeOfV0 iface
      chargeGasArgs (_ifInfo iface) (GModuleMemory ifaceSize)
      writeModule (_ifInfo iface) pdb Write (view ifName iface) mdata
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
      esLoaded %== loadNewModule
      pure (LoadedInterface (view ifName iface) (view ifHash iface))
    TLTerm term -> (`InterpretValue` (view termInfo term)) <$> Eval.eval PImpure bEnv term
    TLUse imp _ -> pure (LoadedImports imp)
{-# SPECIALIZE evalTopLevel
  :: CoreBuiltinEnv
  -> EvalTopLevel CoreBuiltin ()
  -> S.Set ModuleName
  -> Eval (CompileValue ())  #-}
{-# INLINE evalTopLevel #-}

