{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Pact.Core.Compile
 ( interpretTopLevel
 , CompileValue(..)
 ) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Except ( MonadError(throwError), liftEither )
import Control.Monad
import Data.Maybe(mapMaybe)
import Data.Foldable(find)
import Data.ByteString(ByteString)
import qualified Data.Map.Strict as M
import qualified Data.ByteString as B
import qualified Data.Set as Set

import Pact.Core.Debug
import Pact.Core.Builtin
import Pact.Core.Info
import Pact.Core.Persistence
import Pact.Core.Names
import Pact.Core.IR.Desugar
import Pact.Core.Errors
import Pact.Core.Pretty
import Pact.Core.IR.Term
import Pact.Core.Interpreter
import Pact.Core.Guards
import Pact.Core.Environment
import Pact.Core.Capabilities
import Pact.Core.Literal
import Pact.Core.Imports


import qualified Pact.Core.Syntax.Lexer as Lisp
import qualified Pact.Core.Syntax.Parser as Lisp
import qualified Pact.Core.Syntax.ParseTree as Lisp

type HasCompileEnv b i  m
  = ( MonadEval b i m
    , DesugarBuiltin b
    , Pretty b
    , PhaseDebug b i m)

_parseOnly
  :: ByteString -> Either PactErrorI [Lisp.TopLevel SpanInfo]
_parseOnly source = do
  lexed <- liftEither (Lisp.lexer source)
  liftEither (Lisp.parseProgram lexed)

_parseOnlyFile :: FilePath -> IO (Either PactErrorI [Lisp.TopLevel SpanInfo])
_parseOnlyFile fp = _parseOnly <$> B.readFile fp

data CompileValue b
  = LoadedModule ModuleName
  | LoadedInterface ModuleName
  | LoadedImports Import
  | InterpretValue InterpretValue
  deriving Show

-- | Evaluate module governance
evalModuleGovernance
  :: (HasCompileEnv b i m)
  => Interpreter b i m
  -> Lisp.TopLevel i
  -> m ()
evalModuleGovernance interp tl = do
  pdb <- viewEvalEnv eePactDb
  case tl of
    Lisp.TLModule m -> liftIO (readModule pdb (Lisp._mName m)) >>= \case
      -- Existing module found
      Just (ModuleData md _) ->
        case _mGovernance md of
          KeyGov (KeySetName ksn) -> do
            let info = Lisp._mInfo m
                ksnTerm = Constant (LString ksn) info
                ksrg = App (Builtin (liftRaw RawKeysetRefGuard) info) (pure ksnTerm) info
                term = App (Builtin (liftRaw RawEnforceGuard) info) (pure ksrg) info
            void (_interpret interp term)
          CapGov (ResolvedGov fqn) ->
            -- Todo: this does not allow us to delegate governance, which is an issue.
            case find (\d -> defName d == _fqName fqn) (_mDefs md) of
              Just (DCap d) ->
                void (_interpret interp (_dcapTerm d))
              _ ->
                throwError (PEExecutionError (ModuleGovernanceFailure (Lisp._mName m)) (Lisp._mInfo m))
      -- Found an interface, oopsie it's not upgradeable.
      Just (InterfaceData iface _) ->
        throwError (PEExecutionError (CannotUpgradeInterface (_ifName iface)) (_ifInfo iface))
      Nothing -> pure ()
    _ -> pure ()

interpretTopLevel
  :: forall b i  m
  .  (HasCompileEnv b i m)
  => Interpreter b i m
  -> Lisp.TopLevel i
  -> m (CompileValue b)
interpretTopLevel interp tl = do
  evalModuleGovernance interp tl
  pdb <- viewEvalEnv eePactDb
  -- Todo: pretty instance for modules and all of toplevel
  debugPrint (DPParser @b) tl
  (DesugarOutput ds _deps) <- runDesugarTopLevel tl
  debugPrint DPDesugar ds
  lo0 <- useEvalState esLoaded
  case ds of
    TLModule m -> do
      -- let deps' = M.filterWithKey (\k _ -> Set.member (_fqModule k) deps) (_loAllLoaded lo0)
      -- Todo: deps are not being calculated properly by the renamer
      let deps' = _loAllLoaded lo0
          mdata = ModuleData m deps'
      liftDbFunction (_mInfo m) (writeModule pdb Write (view mName m) mdata)
      let newLoaded = M.fromList $ toFqDep (_mName m) (_mHash m) <$> _mDefs m
          loadNewModule =
            over loModules (M.insert (_mName m) mdata) .
            over loAllLoaded (M.union newLoaded)
      esLoaded %== loadNewModule
      esCaps . csModuleAdmin %== Set.union (Set.singleton (_mName m))
      pure (LoadedModule (_mName m))
    TLInterface iface -> do
      -- Todo: deps are not being calculated properly by the renamer
      -- let deps' = M.filterWithKey (\k _ -> Set.member (_fqModule k) deps) (_loAllLoaded lo0)
      let deps' = _loAllLoaded lo0
          mdata = InterfaceData iface deps'
      liftDbFunction (_ifInfo iface) (writeModule pdb Write (view ifName iface) mdata)
      let newLoaded = M.fromList $ toFqDep (_ifName iface) (_ifHash iface)
                      <$> mapMaybe ifDefToDef (_ifDefns iface)
          loadNewModule =
            over loModules (M.insert (_ifName iface) mdata) .
            over loAllLoaded (M.union newLoaded)
      esLoaded %== loadNewModule
      pure (LoadedInterface (view ifName iface))
    TLTerm term -> InterpretValue <$> _interpret interp term
    TLUse imp _ -> pure (LoadedImports imp)
