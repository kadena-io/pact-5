{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Pact.Core.Compile where

import Control.Lens
import Control.Monad.State.Strict ( MonadIO(..), MonadState )
import Control.Monad.Except ( MonadError(throwError), liftEither )
import Control.Monad
import Data.Maybe(mapMaybe)
import Data.Proxy
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
import Pact.Core.Type
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

type HasCompileEnv b s m
  = ( MonadError PactErrorI m
    , MonadState s m
    , HasEvalState s b SpanInfo
    , DesugarBuiltin b
    , Pretty b
    , MonadIO m
    , PhaseDebug m)

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


compileProgram
  :: (HasCompileEnv b s m)
  => ByteString
  -> PactDb b SpanInfo
  -> Interpreter b m
  -> m [CompileValue b]
compileProgram source pdb interp = do
  lexed <- liftEither (Lisp.lexer source)
  debugPrint DebugLexer lexed
  parsed <- liftEither (Lisp.parseProgram lexed)
  lo <- use (evalState . loaded)
  traverse (go lo) parsed
  where
  go lo =
    evalModuleGovernance pdb interp
    >=> runDesugarTopLevel Proxy pdb lo
    >=> interpretTopLevel pdb interp

-- | Evaluate module governance
evalModuleGovernance
  :: (HasCompileEnv b s m)
  => PactDb b SpanInfo
  -> Interpreter b m
  -> Lisp.TopLevel SpanInfo
  -> m (Lisp.TopLevel SpanInfo)
evalModuleGovernance pdb interp = \case
  tl@(Lisp.TLModule m) -> liftIO (readModule pdb (Lisp._mName m)) >>= \case
    Just (ModuleData md _) ->
      case _mGovernance md of
        KeyGov (KeySetName ksn) -> do
          let info = Lisp._mInfo m
              ksnTerm = Constant (LString ksn) info
              ksrg = App (Builtin (liftRaw RawKeysetRefGuard) info) (pure ksnTerm) info
              term = App (Builtin (liftRaw RawEnforceGuard) info) (pure ksrg) info
          _interpret interp term *> pure tl
        CapGov (ResolvedGov fqn) ->
          use (evalState . loaded . loAllLoaded . at fqn) >>= \case
            Just (DCap d) ->
              _interpret interp (_dcapTerm d) *> pure tl
            -- Todo: Definitely fixable with a GADT
            _ -> throwError (PEExecutionError (ModuleGovernanceFailure (Lisp._mName m)) (Lisp._mInfo m))
    Just (InterfaceData iface _) ->
      throwError (PEExecutionError (CannotUpgradeInterface (_ifName iface)) (_ifInfo iface))
    Nothing -> pure tl
  a -> pure a

interpretTopLevel
  :: (HasCompileEnv b s m)
  => PactDb b SpanInfo
  -> Interpreter b m
  -> DesugarOutput b SpanInfo (TopLevel Name Type b SpanInfo)
  -> m (CompileValue b)
interpretTopLevel pdb interp (DesugarOutput ds lo0 deps) = do
  debugPrint DebugDesugar ds
  evalState . loaded .= lo0
  case ds of
    TLModule m -> do
      let deps' = M.filterWithKey (\k _ -> Set.member (_fqModule k) deps) (_loAllLoaded lo0)
          mdata = ModuleData m deps'
      liftDbFunction (_mInfo m) (writeModule pdb Write (view mName m) mdata)
      let newLoaded = M.fromList $ toFqDep (_mName m) (_mHash m) <$> _mDefs m
          loadNewModule =
            over loModules (M.insert (_mName m) mdata) .
            over loAllLoaded (M.union newLoaded)
      evalState . loaded %= loadNewModule
      evalState . esCaps . csModuleAdmin %= Set.union (Set.singleton (_mName m))
      pure (LoadedModule (_mName m))
    TLInterface iface -> do
      let deps' = M.filterWithKey (\k _ -> Set.member (_fqModule k) deps) (_loAllLoaded lo0)
          mdata = InterfaceData iface deps'
      liftDbFunction (_ifInfo iface) (writeModule pdb Write (view ifName iface) mdata)
      let newLoaded = M.fromList $ toFqDep (_ifName iface) (_ifHash iface)
                      <$> mapMaybe (fmap DConst . preview _IfDConst) (_ifDefns iface)
          loadNewModule =
            over loModules (M.insert (_ifName iface) mdata) .
            over loAllLoaded (M.union newLoaded)
      evalState . loaded %= loadNewModule
      pure (LoadedInterface (view ifName iface))
    TLTerm term -> InterpretValue <$> _interpret interp term
    TLUse imp _ -> pure (LoadedImports imp)
  where
  toFqDep modName mhash defn =
    let fqn = FullyQualifiedName modName (defName defn) mhash
    in (fqn, defn)
