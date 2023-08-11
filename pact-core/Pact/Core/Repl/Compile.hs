{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}


module Pact.Core.Repl.Compile
 ( InterpretOutput(..)
 , interpretExpr
 , compileProgram
 , interpretProgram
 , interpretReplProgram
 ) where

import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class(liftIO)
import Data.Text(Text)
import Data.ByteString(ByteString)
import Data.Proxy
import Data.Maybe(mapMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.ByteString as B
import qualified Data.Text as T

import Pact.Core.Info
import Pact.Core.Persistence
import Pact.Core.Builtin
import Pact.Core.Gas
import Pact.Core.Names
import Pact.Core.Repl.Utils
import Pact.Core.IR.Desugar
import Pact.Core.Errors
import Pact.Core.IR.Term


import Pact.Core.IR.Eval.Runtime
import Pact.Core.Repl.Runtime
import Pact.Core.Repl.Runtime.ReplBuiltin

import qualified Pact.Core.IR.Term as IR
import qualified Pact.Core.Syntax.ParseTree as Lisp
import qualified Pact.Core.Syntax.Lexer as Lisp
import qualified Pact.Core.Syntax.Parser as Lisp

data InterpretOutput b i
  = InterpretValue (CEKValue b i (ReplEvalM ReplRawBuiltin SpanInfo)) SpanInfo
  | InterpretLog Text
  deriving Show

interpretExpr
  :: ByteString
  -> ReplM ReplRawBuiltin (ReplEvalResult RawBuiltin SpanInfo)
interpretExpr source = do
  pactdb <- use replPactDb
  loaded <- use replLoaded
  lexx <- liftEither (Lisp.lexer source)
  debugIfFlagSet ReplDebugLexer lexx
  parsed <- liftEither $ Lisp.parseExpr lexx
  debugIfFlagSet ReplDebugParser parsed
  (DesugarOutput desugared loaded' _) <- runDesugarTermLisp Proxy pactdb loaded parsed
  evalGas <- use replGas
  evalLog <- use replEvalLog
  mhashes <- uses (replLoaded . loModules) (fmap (view mdModuleHash))
  let rEnv = ReplEvalEnv evalGas evalLog
      cekEnv = CEKRuntimeEnv
             { _cekBuiltins = replRawBuiltinRuntime
             , _cekLoaded = _loAllLoaded loaded'
             , _cekGasModel = freeGasEnv
             , _cekMHashes = mhashes
             , _cekMsgSigs = mempty
             , _cekPactDb = pactdb }
      rState = ReplEvalState cekEnv (EvalState (CapState [] mempty) [] [] False)
  value <- liftEither =<< liftIO (runReplCEK rEnv rState desugared)
  replLoaded .= loaded'
  pure value

-- Small internal debugging function for playing with file loading within
-- this module
loadFile :: FilePath -> ReplM ReplRawBuiltin [InterpretOutput ReplRawBuiltin SpanInfo]
loadFile source = liftIO (B.readFile source) >>= interpretReplProgram

compileProgram
  :: ByteString
  -> ReplM ReplRawBuiltin [DesugarOutput ReplRawBuiltin SpanInfo (TopLevel Name ReplRawBuiltin SpanInfo)]
compileProgram source = do
  loaded <- use replLoaded
  pactdb <- use replPactDb
  lexx <- liftEither (Lisp.lexer source)
  debugIfFlagSet ReplDebugLexer lexx
  parsed <- liftEither $ Lisp.parseProgram lexx
  ts <- traverse (pipe pactdb) parsed
  replLoaded .= loaded
  pure ts
  where
  pipe pactdb tl = do
    lastLoaded <- use replLoaded
    dsOut <- runDesugarTopLevelLisp (Proxy @ReplRawBuiltin) pactdb lastLoaded tl
    replLoaded .= (_dsLoaded dsOut)
    pure dsOut



interpretReplProgram
  :: ByteString
  -> ReplM ReplRawBuiltin [InterpretOutput ReplRawBuiltin SpanInfo]
interpretReplProgram source = do
  pactdb <- use replPactDb
  lexx <- liftEither (Lisp.lexer source)
  debugIfFlagSet ReplDebugLexer lexx
  parsed <- liftEither $ Lisp.parseReplProgram lexx
  concat <$> traverse (pipe pactdb) parsed
  where
  debugIfLispExpr = \case
    Lisp.RTLTerm t -> debugIfFlagSet ReplDebugParser t
    _ -> pure ()
  debugIfIRExpr flag = \case
    IR.RTLTerm t -> debugIfFlagSet flag t
    _ -> pure ()
  pipe pactdb = \case
    Lisp.RTL rtl -> pure <$> pipe' pactdb rtl
    Lisp.RTLReplSpecial rsf -> case rsf of
      Lisp.ReplLoad txt b _ -> do
        when b $ replLoaded .= mempty
        loadFile (T.unpack txt)
  pipe' pactdb tl = do
    debugIfLispExpr tl
    lastLoaded <- use replLoaded
    ds <- runDesugarReplTopLevel (Proxy @ReplRawBuiltin) pactdb lastLoaded tl
    debugIfIRExpr ReplDebugDesugar (_dsOut ds)
    replLoaded .= (_dsLoaded ds)
    interpret ds
  interpret (DesugarOutput tl _ deps) = do
    pdb <- use replPactDb
    loaded <- use replLoaded
    case tl of
      RTLModule m -> do
        let deps' = Map.filterWithKey (\k _ -> Set.member (_fqModule k) deps) (_loAllLoaded loaded)
            mdata = ModuleData m deps'
        liftIO (writeModule pdb (view mName m) mdata)
        let out = "Loaded module " <> renderModuleName (_mName m)
            newLoaded = Map.fromList $ toFqDep (_mName m) (_mHash m) <$> _mDefs m
            loadNewModule =
              over loModules (Map.insert (_mName m) mdata) .
              over loAllLoaded (Map.union newLoaded)
        replLoaded %= loadNewModule
        pure (InterpretLog out)
        where
        toFqDep modName mhash defn =
          let fqn = FullyQualifiedName modName (defName defn) mhash
          in (fqn, defn)
      RTLTerm te -> do
        debugIfFlagSet ReplDebugUntyped te
        let i = view termInfo te
        evalGas <- use replGas
        evalLog <- use replEvalLog
        -- todo: cache?
        mhashes <- uses (replLoaded . loModules) (fmap (view mdModuleHash))
        let rEnv = ReplEvalEnv evalGas evalLog
            cekEnv = CEKRuntimeEnv
                  { _cekBuiltins = replRawBuiltinRuntime
                  , _cekLoaded = _loAllLoaded loaded
                  , _cekGasModel = freeGasEnv
                  , _cekMHashes = mhashes
                  , _cekMsgSigs = mempty
                  , _cekPactDb = pdb }
            rState = ReplEvalState cekEnv (EvalState (CapState [] mempty) [] [] False)
        -- Todo: Fix this with `returnCEKValue`
        liftIO (runReplCEK rEnv rState te) >>= liftEither >>= \case
          VError txt ->
            throwError (PEExecutionError (ExecutionError txt) i)
          EvalValue v -> do
            replLoaded .= loaded
            pure (InterpretValue v i)
      RTLDefun df -> do
        let fqn = FullyQualifiedName replModuleName (_dfunName df) replModuleHash
        replLoaded . loAllLoaded %= Map.insert fqn (Dfun df)
        pure $ InterpretLog $ "Loaded repl defun: " <> _dfunName df
      RTLDefConst dc -> do
        let fqn = FullyQualifiedName replModuleName (_dcName dc) replModuleHash
        replLoaded . loAllLoaded %= Map.insert fqn (DConst dc)
        pure $ InterpretLog $ "Loaded repl defconst: " <> _dcName dc
      RTLInterface iface -> do
        let deps' = Map.filterWithKey (\k _ -> Set.member (_fqModule k) deps) (_loAllLoaded loaded)
            mdata = InterfaceData iface deps'
        liftIO (writeModule pdb (view ifName iface) mdata)
        let out = "Loaded iface " <> renderModuleName (_ifName iface)
            newLoaded = Map.fromList $ toFqDep (_ifName iface) (_ifHash iface)
                        <$> mapMaybe (fmap DConst . preview _IfDConst) (_ifDefns iface)
            loadNewModule =
              over loModules (Map.insert (_ifName iface) mdata) .
              over loAllLoaded (Map.union newLoaded)
        replLoaded %= loadNewModule
        pure (InterpretLog out)
        where
        toFqDep modName mhash defn =
          let fqn = FullyQualifiedName modName (defName defn) mhash
          in (fqn, defn)


interpretProgram
  :: ByteString
  -> ReplM ReplRawBuiltin [InterpretOutput ReplRawBuiltin SpanInfo]
interpretProgram source = do
  compileProgram source >>= traverse interpret
  where
  interpret (DesugarOutput tl l deps) = do
    pdb <- use replPactDb
    replLoaded <>= l
    loaded <- use replLoaded
    case tl of
      TLModule m -> do
        let deps' = Map.filterWithKey (\k _ -> Set.member (_fqModule k) deps) (_loAllLoaded loaded)
            mdata = ModuleData m deps'
        liftIO (writeModule pdb (view mName m) mdata)
        let out = "Loaded module " <> renderModuleName (_mName m)
            newLoaded = Map.fromList $ toFqDep (_mName m) (_mHash m) <$> _mDefs m
            loadNewModule =
              over loModules (Map.insert (_mName m) mdata) .
              over loAllLoaded (Map.union newLoaded)
        replLoaded %= loadNewModule
        pure (InterpretLog out)
        where
        toFqDep modName mhash defn =
          let fqn = FullyQualifiedName modName (defName defn) mhash
          in (fqn, defn)
      TLInterface _ -> error "interace stub"
      TLTerm te -> do
        let i = view termInfo te
        evalGas <- use replGas
        evalLog <- use replEvalLog
        mhashes <- uses (replLoaded . loModules) (fmap (view mdModuleHash))
        let rEnv = ReplEvalEnv evalGas evalLog
            cekEnv = CEKRuntimeEnv
                  { _cekBuiltins = replRawBuiltinRuntime
                  , _cekLoaded = _loAllLoaded loaded
                  , _cekGasModel = freeGasEnv
                  , _cekMHashes = mhashes
                  , _cekMsgSigs = mempty
                  , _cekPactDb = pdb}
            rState = ReplEvalState cekEnv (EvalState (CapState [] mempty) [] [] False)
        -- Todo: Fix this with `returnCEKValue`
        liftIO (runReplCEK rEnv rState te) >>= liftEither >>= \case
          VError txt ->
            throwError (PEExecutionError (ExecutionError txt) i)
          EvalValue v -> do
            replLoaded .= loaded
            pure (InterpretValue v i)
