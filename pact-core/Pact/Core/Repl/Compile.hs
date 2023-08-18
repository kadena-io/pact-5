{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}


module Pact.Core.Repl.Compile
 ( ReplCompileValue(..)
--  , interpretExpr
--  , compileProgram
 , interpretReplProgram
 ) where

import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class(liftIO)

import qualified Data.Set as Set
import Data.Text(Text)
import Data.ByteString(ByteString)
import Data.Proxy
-- import Data.Maybe(mapMaybe)
import qualified Data.Map.Strict as Map
-- import qualified Data.Set as Set
import qualified Data.ByteString as B
import qualified Data.Text as T

-- import Pact.Core.Info
import Pact.Core.Persistence
import Pact.Core.Builtin
import Pact.Core.Gas
import Pact.Core.Names
import Pact.Core.Pretty hiding (pipe)
import Pact.Core.Repl.Types
import Pact.Core.IR.Desugar
import Pact.Core.Errors
import Pact.Core.IR.Term
import Pact.Core.Compile


import Pact.Core.IR.Eval.Runtime
import Pact.Core.Repl.Runtime
import Pact.Core.Repl.Runtime.ReplBuiltin

import qualified Pact.Core.Syntax.ParseTree as Lisp
import qualified Pact.Core.Syntax.Lexer as Lisp
import qualified Pact.Core.Syntax.Parser as Lisp

-- data InterpretOutput b i
--   = InterpretValue (CEKValue b i (ReplEvalM ReplRawBuiltin SpanInfo)) SpanInfo
--   | InterpretLog Text
--   deriving Show

-- interpretExpr
--   :: ByteString
--   -> ReplM ReplRawBuiltin (ReplEvalResult RawBuiltin SpanInfo)
-- interpretExpr source = do
--   pactdb <- use replPactDb
--   loaded <- use replLoaded
--   lexx <- liftEither (Lisp.lexer source)
--   debugIfFlagSet ReplDebugLexer lexx
--   parsed <- liftEither $ Lisp.parseExpr lexx
--   debugIfFlagSet ReplDebugParser parsed
--   (DesugarOutput desugared loaded' _) <- runDesugarTermLisp Proxy pactdb loaded parsed
--   evalGas <- use replGas
--   evalLog <- use replEvalLog
--   mhashes <- uses (replLoaded . loModules) (fmap (view mdModuleHash))
--   let rEnv = ReplEvalEnv evalGas evalLog
--       cekEnv = EvalEnv
--              { _eeBuiltins = replRawBuiltinRuntime
--              , _eeLoaded = _loAllLoaded loaded'
--              , _eeGasModel = freeGasEnv
--              , _eeMHashes = mhashes
--              , _eeMsgSigs = mempty
--              , _eePactDb = pactdb }
--       rState = ReplEvalState cekEnv (EvalState (CapState [] mempty) [] [] False)
--   value <- liftEither =<< liftIO (runReplCEK rEnv rState desugared)
--   replLoaded .= loaded'
--   pure value

-- Small internal debugging function for playing with file loading within
-- this module
data ReplCompileValue
  = RCompileValue (CompileValue ReplRawBuiltin)
  | RLoadedDefun Text
  | RLoadedDefConst Text
  deriving Show

loadFile :: FilePath -> ReplM ReplRawBuiltin [ReplCompileValue]
loadFile source = liftIO (B.readFile source) >>= interpretReplProgram


interpretReplProgram
  :: ByteString
  -> ReplM ReplRawBuiltin [ReplCompileValue]
interpretReplProgram source = do
  pactdb <- use replPactDb
  lexx <- liftEither (Lisp.lexer source)
  printIfReplFlagSet ReplDebugLexer lexx
  parsed <- liftEither $ Lisp.parseReplProgram lexx
  concat <$> traverse (pipe pactdb) parsed
  where
  debugIfLispExpr = \case
    Lisp.RTLTerm t -> printIfReplFlagSet ReplDebugParser t
    _ -> pure ()
  debugIfIRExpr flag = \case
    RTLTerm t -> printIfReplFlagSet flag t
    _ -> pure ()
  pipe pactdb = \case
    Lisp.RTL rtl ->
      pure <$> pipe' pactdb rtl
    Lisp.RTLReplSpecial rsf -> case rsf of
      Lisp.ReplLoad txt b _ -> do
        when b $ replLoaded .= mempty
        loadFile (T.unpack txt)
  pipe' pactdb tl = do
    debugIfLispExpr tl
    lastLoaded <- use replLoaded
    ds <- runDesugarReplTopLevel (Proxy @ReplRawBuiltin) pactdb lastLoaded tl
    debugIfIRExpr ReplDebugDesugar (_dsOut ds)
    replLoaded .= _dsLoaded ds
    interpret ds
  interpret (DesugarOutput tl _ deps) = do
    pdb <- use replPactDb
    lo <- use replLoaded
    case tl of
      RTLTopLevel tt -> do
        let interp = Interpreter interpreter
        RCompileValue <$> interpretTopLevel pdb interp (DesugarOutput tt lo deps)
        where
        interpreter te = do
          printIfReplFlagSet ReplDebugUntyped te
          let i = view termInfo te
          evalGas <- use replGas
          evalLog <- use replEvalLog
          -- todo: cache?
          mhashes <- uses (replLoaded . loModules) (fmap (view mdModuleHash))
          let rEnv = ReplEvalEnv evalGas evalLog
              cekEnv = EvalEnv
                    { _eeBuiltins = replRawBuiltinRuntime
                    , _eeLoaded = _loAllLoaded lo
                    , _eeGasModel = freeGasEnv
                    , _eeMHashes = mhashes
                    , _eeMsgSigs = mempty
                    , _eePactDb = pdb }
              rState = ReplEvalState cekEnv (EvalState (CapState [] mempty) [] [] False)
          -- Todo: Fix this with `returnCEKValue`
          liftIO (runReplCEK rEnv rState te) >>= liftEither >>= \case
            VError txt ->
              throwError (PEExecutionError (EvalError txt) i)
            EvalValue v -> case v of
              VClosure{} -> do
                replLoaded .= lo
                pure IPClosure
              VPactValue pv -> do
                replLoaded .= lo
                pure (IPV pv (view termInfo te))

      -- RTLModule m -> do
      --   let deps' = Map.filterWithKey (\k _ -> Set.member (_fqModule k) deps) (_loAllLoaded lo)
      --       mdata = ModuleData m deps'
      --   liftIO (writeModule pdb (view mName m) mdata)
      --   let out = "Loaded module " <> renderModuleName (_mName m)
      --       newLoaded = Map.fromList $ toFqDep (_mName m) (_mHash m) <$> _mDefs m
      --       loadNewModule =
      --         over loModules (Map.insert (_mName m) mdata) .
      --         over loAllLoaded (Map.union newLoaded)
      --   replLoaded %= loadNewModule
      --   pure (InterpretLog out)
      --   where
      --   toFqDep modName mhash defn =
      --     let fqn = FullyQualifiedName modName (defName defn) mhash
      --     in (fqn, defn)
      -- RTLTerm te -> do
      --   debugIfFlagSet ReplDebugUntyped te
      --   let i = view termInfo te
      --   evalGas <- use replGas
      --   evalLog <- use replEvalLog
      --   -- todo: cache?
      --   mhashes <- uses (replLoaded . loModules) (fmap (view mdModuleHash))
      --   let rEnv = ReplEvalEnv evalGas evalLog
      --       cekEnv = EvalEnv
      --             { _eeBuiltins = replRawBuiltinRuntime
      --             , _eeLoaded = _loAllLoaded lo
      --             , _eeGasModel = freeGasEnv
      --             , _eeMHashes = mhashes
      --             , _eeMsgSigs = mempty
      --             , _eePactDb = pdb }
      --       rState = ReplEvalState cekEnv (EvalState (CapState [] mempty) [] [] False)
      --   -- Todo: Fix this with `returnCEKValue`
      --   liftIO (runReplCEK rEnv rState te) >>= liftEither >>= \case
      --     VError txt ->
      --       throwError (PEExecutionError (EvalError txt) i)
      --     EvalValue v -> do
      --       replLoaded .= lo
      --       pure (InterpretValue v i)
      RTLDefun df -> do
        let fqn = FullyQualifiedName replModuleName (_dfunName df) replModuleHash
        replLoaded . loAllLoaded %= Map.insert fqn (Dfun df)
        pure $ RLoadedDefun $ _dfunName df
      RTLDefConst dc -> do
        let fqn = FullyQualifiedName replModuleName (_dcName dc) replModuleHash
        replLoaded . loAllLoaded %= Map.insert fqn (DConst dc)
        pure $ RLoadedDefConst $ _dcName dc
      -- RTLInterface iface -> do
      --   let deps' = Map.filterWithKey (\k _ -> Set.member (_fqModule k) deps) (_loAllLoaded lo)
      --       mdata = InterfaceData iface deps'
      --   liftIO (writeModule pdb (view ifName iface) mdata)
      --   let out = "Loaded iface " <> renderModuleName (_ifName iface)
      --       newLoaded = Map.fromList $ toFqDep (_ifName iface) (_ifHash iface)
      --                   <$> mapMaybe (fmap DConst . preview _IfDConst) (_ifDefns iface)
      --       loadNewModule =
      --         over loModules (Map.insert (_ifName iface) mdata) .
      --         over loAllLoaded (Map.union newLoaded)
      --   replLoaded %= loadNewModule
      --   pure (InterpretLog out)
      --   where
      --   toFqDep modName mhash defn =
      --     let fqn = FullyQualifiedName modName (defName defn) mhash
      --     in (fqn, defn)

-- | Print debugging information for a particular Repl debug flag
-- if set in the Repl environment.
--
printIfReplFlagSet :: Pretty a => ReplDebugFlag -> a -> ReplM b ()
printIfReplFlagSet flag a =
  whenReplFlagSet flag $ liftIO (printDebug a flag)

-- | Set Repl debug flag in the repl monad.
--
replFlagSet
  :: ReplDebugFlag
  -> ReplM b Bool
replFlagSet flag =
  uses replFlags (Set.member flag)

-- | Execute an action if a particular debug flag is set in
-- in the Repl environment.
--
whenReplFlagSet :: ReplDebugFlag -> ReplM b () -> ReplM b ()
whenReplFlagSet flag ma =
  replFlagSet flag >>= \b -> when b ma

-- | Execute an action if a particular debug flag is not set in
-- in the Repl environment.
--
_unlessReplFlagSet :: ReplDebugFlag -> ReplM b () -> ReplM b ()
_unlessReplFlagSet flag ma =
  replFlagSet flag >>= \b -> unless b ma

-- | Print configuration for repl debug flags
--
-- TODO: this seems useful, but stale.
printDebug :: Pretty a => a -> ReplDebugFlag -> IO ()
printDebug a = \case
  ReplDebugLexer -> do
    putStrLn "----------- Lexer output -----------------"
    print (pretty a)
  ReplDebugParser -> do
    putStrLn "----------- Parser output ----------------"
    print (pretty a)
  ReplDebugDesugar -> do
    putStrLn "----------- Desugar output ---------------"
    print (pretty a)
  ReplDebugTypechecker -> do
    putStrLn "----------- Typechecker output -----------"
    print (pretty a)
  ReplDebugTypecheckerType -> do
    putStrLn "----------- Inferred type output ---------"
    print (pretty a)
  ReplDebugSpecializer -> do
    putStrLn "----------- Specializer output -----------"
    print (pretty a)
  ReplDebugUntyped -> do
    putStrLn "----------- Untyped core output ----------"
    print (pretty a)
