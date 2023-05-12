{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}


module Pact.Core.Repl.Compile
 ( InterpretOutput(..)
 , interpretExprLisp
 , lispInterpretBundle
 , interpretExprTypeLisp
 , InterpretBundle(..)
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
import Pact.Core.Untyped.Term
import Pact.Core.Untyped.Utils
import Pact.Core.IR.Desugar
import Pact.Core.IR.Typecheck
import Pact.Core.Type
import Pact.Core.Typed.Overload
import Pact.Core.Errors


import Pact.Core.Untyped.Eval.Runtime
import Pact.Core.Repl.Runtime
import Pact.Core.Repl.Runtime.ReplBuiltin

import qualified Pact.Core.Pretty as Pretty
import qualified Pact.Core.IR.Term as IR
import qualified Pact.Core.Typed.Term as Typed
import qualified Pact.Core.Syntax.Lisp.ParseTree as Lisp
import qualified Pact.Core.Syntax.Lisp.Lexer as Lisp
import qualified Pact.Core.Syntax.Lisp.Parser as Lisp

data InterpretOutput b i
  = InterpretValue (CEKValue b i (ReplEvalM ReplCoreBuiltin SpanInfo)) SpanInfo
  | InterpretLog Text
  deriving Show

-- | Auxiliary type
-- to assist in swapping from the lisp frontend
data InterpretBundle
  = InterpretBundle
  { expr :: ByteString -> ReplM ReplCoreBuiltin (ReplEvalResult CoreBuiltin SpanInfo)
  , exprType :: ByteString -> ReplM ReplCoreBuiltin (TypeScheme NamedDeBruijn)
  , program :: ByteString -> ReplM ReplCoreBuiltin [InterpretOutput ReplCoreBuiltin SpanInfo]
  }

lispInterpretBundle :: InterpretBundle
lispInterpretBundle =
    InterpretBundle
  { expr = interpretExprLisp
  , exprType = interpretExprTypeLisp
  , program = interpretProgram }

interpretExprLisp
  :: ByteString
  -> ReplM ReplCoreBuiltin (ReplEvalResult CoreBuiltin SpanInfo)
interpretExprLisp source = do
  pactdb <- use replPactDb
  loaded <- use replLoaded
  lexx <- liftEither (Lisp.lexer source)
  debugIfFlagSet ReplDebugLexer lexx
  parsed <- liftEither $ Lisp.parseExpr lexx
  debugIfFlagSet ReplDebugParser parsed
  desugared <- runDesugarTermLisp Proxy pactdb loaded parsed
  interpretExpr desugared

interpretExpr
  :: DesugarOutput ReplCoreBuiltin SpanInfo (IR.Term Name ReplRawBuiltin SpanInfo)
  -> ReplM ReplCoreBuiltin (ReplEvalResult CoreBuiltin SpanInfo)
interpretExpr (DesugarOutput desugared loaded' _) = do
  debugIfFlagSet ReplDebugDesugar desugared
  (ty, typed) <- liftEither (runInferTerm loaded' desugared)
  debugIfFlagSet ReplDebugTypecheckerType ty
  debugIfFlagSet ReplDebugTypechecker typed
  resolved <- liftEither (runOverloadTerm typed)
  debugIfFlagSet ReplDebugSpecializer resolved
  let untyped = fromTypedTerm resolved
  debugIfFlagSet ReplDebugUntyped untyped
  evalGas <- use replGas
  evalLog <- use replEvalLog
  mhashes <- uses (replLoaded . loModules) (fmap (view mdModuleHash))
  let rEnv = ReplEvalEnv evalGas evalLog
      cekEnv = CEKRuntimeEnv
             { _cekBuiltins = replCoreBuiltinRuntime
             , _cekLoaded = _loAllLoaded loaded'
             , _cekGasModel = freeGasEnv
             , _cekMHashes = mhashes }
      rState = ReplEvalState cekEnv (EvalState (CapState [] mempty) [] False)
  value <- liftEither =<< liftIO (runReplCEK rEnv rState untyped)
  replLoaded .= loaded'
  pure value


interpretExprTypeLisp :: ByteString -> ReplM ReplCoreBuiltin (TypeScheme NamedDeBruijn)
interpretExprTypeLisp source = do
  pactdb <- use replPactDb
  loaded <- use replLoaded
  lexx <- liftEither (Lisp.lexer source)
  debugIfFlagSet ReplDebugLexer lexx
  parsed <- liftEither $ Lisp.parseExpr lexx
  debugIfFlagSet ReplDebugParser parsed
  desugared <- runDesugarTermLisp Proxy pactdb loaded parsed
  interpretExprType desugared

interpretExprType
  :: DesugarOutput ReplCoreBuiltin SpanInfo (IR.Term Name ReplRawBuiltin SpanInfo)
  -> ReplM ReplCoreBuiltin (TypeScheme NamedDeBruijn)
interpretExprType (DesugarOutput desugared loaded' _) = do
  debugIfFlagSet ReplDebugDesugar desugared
  (ty, typed) <- liftEither (runInferTerm loaded' desugared)
  debugIfFlagSet ReplDebugTypecheckerType ty
  debugIfFlagSet ReplDebugTypechecker typed
  pure ty

-- Small internal debugging function for playing with file loading within
-- this module
loadFile :: FilePath -> ReplM ReplCoreBuiltin [InterpretOutput ReplCoreBuiltin SpanInfo]
loadFile source = liftIO (B.readFile source) >>= interpretReplProgram

compileProgram
  :: ByteString
  -> ReplM ReplCoreBuiltin [DesugarOutput ReplCoreBuiltin SpanInfo (TopLevel Name ReplCoreBuiltin SpanInfo)]
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
    (DesugarOutput desugared loaded' deps) <- runDesugarTopLevelLisp (Proxy @ReplRawBuiltin) pactdb lastLoaded tl
    (typechecked, loadedWithTc) <- liftEither (runInferTopLevel loaded' desugared)
    replLoaded .= loadedWithTc
    overloaded <- liftEither (runOverloadTopLevel typechecked)
    pure (DesugarOutput (fromTypedTopLevel overloaded) loadedWithTc deps)

    -- replLoaded .= loaded'
    -- pure (DesugarOutput (fromIRTopLevel desugared) loaded' deps)



interpretReplProgram
  :: ByteString
  -> ReplM ReplCoreBuiltin [InterpretOutput ReplCoreBuiltin SpanInfo]
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
  debugIfTypedExpr flag = \case
    Typed.RTLTerm t -> debugIfFlagSet flag t
    _ -> pure ()
  -- debugIfUntyped flag = \case
  --   RTLTerm t -> debugIfFlagSet flag t
  --   _ -> pure ()
  tcExpr loaded pdb e = do
    (DesugarOutput desugared loaded' _) <- runDesugarTermLisp (Proxy @ReplRawBuiltin) pdb loaded e
    pure (runInferTerm loaded' desugared)
  pipe pactdb = \case
    Lisp.RTL rtl -> pure <$> pipe' pactdb rtl
    Lisp.RTLReplSpecial rsf -> case rsf of
      Lisp.ReplLoad txt b _ -> do
        when b $ replLoaded .= mempty
        loadFile (T.unpack txt)
      Lisp.ReplTypechecks msg ex i -> do
        loaded <- use replLoaded
        tcExpr loaded pactdb ex >>= \case
           Left err -> let
            errRender = renderPactError err
            errMsg = "FAILURE: expect-typecheck: " <> msg <> ". Expression failed to to typecheck with error: " <> errRender
            in pure [InterpretValue (VString errMsg) i]
           Right _ -> pure [InterpretValue (VString ("Success: expect-typecheck: " <> msg <> ": " <> Pretty.renderText ex)) i]
      Lisp.ReplTypecheckFail msg ex i -> do
        loaded <- use replLoaded
        let exprRender = Pretty.renderText ex
        tcExpr loaded pactdb ex >>= \case
           Left _ ->
            let succMsg = "Success: expect-typecheck-failure: " <> msg <> ". Expression failed to typecheck: " <> exprRender
            in pure [InterpretValue (VString succMsg) i]
           Right _ ->
            let errMsg = "FAILURE: expect-typecheck-failure: " <> msg <> ". Expression successfully typechecked when expected to fail: " <> exprRender
            in pure [InterpretValue (VString errMsg) i]
  pipe' pactdb tl = do
    debugIfLispExpr tl
    lastLoaded <- use replLoaded
    (DesugarOutput desugared loaded' deps) <- runDesugarReplTopLevel (Proxy @ReplRawBuiltin) pactdb lastLoaded tl
    debugIfIRExpr ReplDebugDesugar desugared
    (typechecked, loadedWithTc) <- liftEither (runInferReplTopLevel loaded' desugared)
    debugIfTypedExpr ReplDebugTypechecker typechecked
    replLoaded .= loadedWithTc
    overloaded <- liftEither (runOverloadReplTopLevel typechecked)
    debugIfTypedExpr ReplDebugSpecializer overloaded
    interpret (DesugarOutput (fromTypedReplTopLevel overloaded) loaded' deps)
  interpret (DesugarOutput tl _ deps) = do
    pdb <- use replPactDb
    loaded <- use replLoaded
    case tl of
      RTLModule m -> do
        let deps' = Map.filterWithKey (\k _ -> Set.member (_fqModule k) deps) (_loAllLoaded loaded)
            mdata = ModuleData m deps'
        _writeModule pdb mdata
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
                  { _cekBuiltins = replCoreBuiltinRuntime
                  , _cekLoaded = _loAllLoaded loaded
                  , _cekGasModel = freeGasEnv
                  , _cekMHashes = mhashes }
            rState = ReplEvalState cekEnv (EvalState (CapState [] mempty) [] False)
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
        _writeModule pdb mdata
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
  -> ReplM ReplCoreBuiltin [InterpretOutput ReplCoreBuiltin SpanInfo]
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
        _writeModule pdb mdata
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
                  { _cekBuiltins = replCoreBuiltinRuntime
                  , _cekLoaded = _loAllLoaded loaded
                  , _cekGasModel = freeGasEnv
                  , _cekMHashes = mhashes }
            rState = ReplEvalState cekEnv (EvalState (CapState [] mempty) [] False)
        -- Todo: Fix this with `returnCEKValue`
        liftIO (runReplCEK rEnv rState te) >>= liftEither >>= \case
          VError txt ->
            throwError (PEExecutionError (ExecutionError txt) i)
          EvalValue v -> do
            replLoaded .= loaded
            pure (InterpretValue v i)
