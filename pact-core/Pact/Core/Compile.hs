{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Pact.Core.Compile where

import Control.Lens
-- import Control.Monad.IO.Class(MonadIO)
import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Monad
import Data.Maybe(mapMaybe)
import Data.Proxy
import Data.Foldable(traverse_)
import Data.ByteString(ByteString)
import qualified Data.Map.Strict as M
import qualified Data.ByteString as B
import qualified Data.Set as Set

import Pact.Core.Debug
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


-- import qualified Pact.Core.Syntax.LexUtils as Lisp
import qualified Pact.Core.Syntax.Lexer as Lisp
import qualified Pact.Core.Syntax.Parser as Lisp
import qualified Pact.Core.Syntax.ParseTree as Lisp

type HasCompileEnv b s m
  = ( MonadError PactErrorI m
    , MonadState s m
    , HasLoaded s b SpanInfo
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
  lo <- use loaded
  traverse (go lo) parsed
  where
  go lo =
    evalModuleGovernance pdb interp
    >=> runDesugarTopLevel Proxy pdb lo
    >=> interpretTopLevel pdb interp

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
        KeyGov _ksn -> error "TODO: implement enforcing keyset names"
        CapGov (Name n nk) -> case nk of
          NTopLevel mn mh ->
            use (loaded . loAllLoaded . at (FullyQualifiedName mn n mh)) >>= \case
              Just (DCap d) ->
                _interpret interp (_dcapTerm d) >>= \case
                  IPV{} -> pure tl
                  _ -> error "governance failure"
              -- Todo: Definitely fixable with a GADT
              _ -> error "invalid governance: not a defcap"
          _ -> error "invariant failure: governance is not a fully qualified name"
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
  loaded .= lo0
  case ds of
    TLModule m -> do
      let deps' = M.filterWithKey (\k _ -> Set.member (_fqModule k) deps) (_loAllLoaded lo0)
          mdata = ModuleData m deps'
      liftIO (writeModule pdb (view mName m) mdata)
      let newLoaded = M.fromList $ toFqDep (_mName m) (_mHash m) <$> _mDefs m
          loadNewModule =
            over loModules (M.insert (_mName m) mdata) .
            over loAllLoaded (M.union newLoaded)
      loaded %= loadNewModule
      pure (LoadedModule (_mName m))
    TLInterface iface -> do
      let deps' = M.filterWithKey (\k _ -> Set.member (_fqModule k) deps) (_loAllLoaded lo0)
          mdata = InterfaceData iface deps'
      liftIO (writeModule pdb (view ifName iface) mdata)
      let newLoaded = M.fromList $ toFqDep (_ifName iface) (_ifHash iface)
                      <$> mapMaybe (fmap DConst . preview _IfDConst) (_ifDefns iface)
          loadNewModule =
            over loModules (M.insert (_ifName iface) mdata) .
            over loAllLoaded (M.union newLoaded)
      loaded %= loadNewModule
      pure (LoadedInterface (view ifName iface))
    TLTerm term -> InterpretValue <$> _interpret interp term
  where
  toFqDep modName mhash defn =
    let fqn = FullyQualifiedName modName (defName defn) mhash
    in (fqn, defn)
