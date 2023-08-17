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
import Data.ByteString(ByteString)
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as B
import qualified Data.Set as Set

import Pact.Core.Debug
import Pact.Core.Info
import Pact.Core.Persistence
import Pact.Core.Names
import Pact.Core.IR.Desugar
import Pact.Core.Errors
import Pact.Core.Pretty
import Pact.Core.IR.Term
import Pact.Core.PactValue


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

newtype Interpreter b s m
  = Interpreter {
    _interpret :: HasCompileEnv b s m => Term Name b SpanInfo -> m InterpretValue
  }


data InterpretValue
  = IPV PactValue SpanInfo
  | IPClosure
  deriving Show

compileProgram
  :: (HasCompileEnv b s m)
  => ByteString
  -> PactDb b SpanInfo
  -> Interpreter b s m
  -> m [CompileValue b]
compileProgram source pdb interp = do
  lexed <- liftEither (Lisp.lexer source)
  debugPrint DebugLexer lexed
  parsed <- liftEither (Lisp.parseProgram lexed)
  lo <- use loaded
  traverse (runDesugarTopLevel Proxy pdb lo >=> interpretTopLevel pdb interp) parsed

interpretTopLevel
  :: (HasCompileEnv b s m)
  => PactDb b SpanInfo
  -> Interpreter b s m
  -> DesugarOutput b SpanInfo (TopLevel Name b SpanInfo)
  -> m (CompileValue b)
interpretTopLevel pdb interp (DesugarOutput ds lo0 deps) = do
  loaded .= lo0
  case ds of
    TLModule m -> do
      let deps' = Map.filterWithKey (\k _ -> Set.member (_fqModule k) deps) (_loAllLoaded lo0)
          mdata = ModuleData m deps'
      liftIO (writeModule pdb (view mName m) mdata)
      let newLoaded = Map.fromList $ toFqDep (_mName m) (_mHash m) <$> _mDefs m
          loadNewModule =
            over loModules (Map.insert (_mName m) mdata) .
            over loAllLoaded (Map.union newLoaded)
      loaded %= loadNewModule
      pure (LoadedModule (_mName m))
    TLInterface iface -> do
      let deps' = Map.filterWithKey (\k _ -> Set.member (_fqModule k) deps) (_loAllLoaded lo0)
          mdata = InterfaceData iface deps'
      liftIO (writeModule pdb (view ifName iface) mdata)
      let newLoaded = Map.fromList $ toFqDep (_ifName iface) (_ifHash iface)
                      <$> mapMaybe (fmap DConst . preview _IfDConst) (_ifDefns iface)
          loadNewModule =
            over loModules (Map.insert (_ifName iface) mdata) .
            over loAllLoaded (Map.union newLoaded)
      loaded %= loadNewModule
      pure (LoadedInterface (view ifName iface))
    TLTerm term -> InterpretValue <$> _interpret interp term
  where
  toFqDep modName mhash defn =
    let fqn = FullyQualifiedName modName (defName defn) mhash
    in (fqn, defn)
