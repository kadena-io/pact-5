{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.Text(Text)
import Data.Proxy
import Data.Default
import qualified Data.Map.Strict as M
import qualified Data.ByteString as B
import qualified Data.Text as T

import Pact.Core.Persistence
import Pact.Core.Builtin
import Pact.Core.Names
import Pact.Core.Repl.Utils
import Pact.Core.IR.Desugar
import Pact.Core.Errors
import Pact.Core.IR.Term
import Pact.Core.Compile
import Pact.Core.Interpreter
import Pact.Core.Environment


import Pact.Core.IR.Eval.Runtime
import Pact.Core.Repl.Runtime
import Pact.Core.Repl.Runtime.ReplBuiltin
import Pact.Core.Hash
import Pact.Core.PactValue (EnvData(..))

import qualified Pact.Core.Syntax.ParseTree as Lisp
import qualified Pact.Core.Syntax.Lexer as Lisp
import qualified Pact.Core.Syntax.Parser as Lisp

-- Small internal debugging function for playing with file loading within
-- this module
data ReplCompileValue
  = RCompileValue (CompileValue ReplRawBuiltin)
  | RLoadedDefun Text
  | RLoadedDefConst Text
  deriving Show

loadFile :: FilePath -> ReplM ReplRawBuiltin [ReplCompileValue]
loadFile loc = do
  source <- SourceCode <$> liftIO (B.readFile loc)
  replCurrSource .= source
  interpretReplProgram source

defaultEvalEnv :: PactDb b i -> EvalEnv b i
defaultEvalEnv pdb =
  EvalEnv mempty pdb (EnvData mempty) (Hash "default") def Nothing Transactional mempty

interpretReplProgram
  :: SourceCode
  -> ReplM ReplRawBuiltin [ReplCompileValue]
interpretReplProgram sc@(SourceCode source) = do
  lexx <- liftEither (Lisp.lexer source)
  debugIfFlagSet ReplDebugLexer lexx
  parsed <- liftEither $ Lisp.parseReplProgram lexx
  concat <$> traverse pipe parsed
  where
  debugIfLispExpr = \case
    Lisp.RTLTerm t -> debugIfFlagSet ReplDebugParser t
    _ -> pure ()
  debugIfIRExpr flag = \case
    RTLTerm t -> debugIfFlagSet flag t
    _ -> pure ()
  pipe = \case
    Lisp.RTL rtl ->
      pure <$> pipe' rtl
    Lisp.RTLReplSpecial rsf -> case rsf of
      Lisp.ReplLoad txt b _
        | b -> do
          oldSrc <- use replCurrSource
          evalState .= def
          pactdb <- liftIO mockPactDb
          replPactDb .= pactdb
          replEvalEnv .= defaultEvalEnv pactdb
          out <- loadFile (T.unpack txt)
          replCurrSource .= oldSrc
          pure out
        | otherwise -> do
          oldSrc <- use replCurrSource
          oldEs <- use evalState
          oldEE <- use replEvalEnv
          when b $ evalState .= def
          out <- loadFile (T.unpack txt)
          replEvalEnv .= oldEE
          evalState .= oldEs
          replCurrSource .= oldSrc
          pure out
  pipe' tl = do
    pactdb <- use replPactDb
    debugIfLispExpr tl
    _ <- moduleGov pactdb tl
    lastLoaded <- use loaded
    ds <- runDesugarReplTopLevel (Proxy @ReplRawBuiltin) pactdb lastLoaded tl
    debugIfIRExpr ReplDebugDesugar (_dsOut ds)
    loaded .= _dsLoaded ds
    interpret ds

  moduleGov pactdb (Lisp.RTLTopLevel tl) =
    () <$ evalModuleGovernance pactdb interpreter tl
  moduleGov _ _ = pure ()

  interpreter = Interpreter $ \te -> do
    evalGas <- use replGas
    evalLog <- use replEvalLog
    es <- use replEvalState
    tx <- use replTx
    ee <- use replEvalEnv
    -- todo: cache?
    -- mhashes <- uses (loaded . loModules) (fmap (view mdModuleHash))
    let rEnv = ReplEvalEnv evalGas evalLog replBuiltinEnv
        rState = ReplEvalState ee es sc tx
    (out, st) <- liftIO (runReplCEK rEnv rState te)
    replTx .= view reTx st
    evalState .= view reState st
    replEvalEnv .= view reEnv st
    liftEither out >>= \case
      VError txt _ ->
        throwError (PEExecutionError (EvalError txt) (view termInfo te))
      EvalValue v -> do
        loaded .= view (reState . esLoaded) st
        (replEvalState . esPactExec) .= view (reState . esPactExec) st
        case v of
          VClosure{} -> do
            pure IPClosure
          VTable tv -> pure (IPTable (_tvName tv))
          VPactValue pv -> do
            pure (IPV pv (view termInfo te))
  interpret (DesugarOutput tl _ deps) = do
    pdb <- use replPactDb
    lo <- use loaded
    case tl of
      RTLTopLevel tt -> do
        RCompileValue <$> interpretTopLevel pdb interpreter (DesugarOutput tt lo deps)
        where
      RTLDefun df -> do
        let fqn = FullyQualifiedName replModuleName (_dfunName df) replModuleHash
        loaded . loAllLoaded %= M.insert fqn (Dfun df)
        pure $ RLoadedDefun $ _dfunName df
      RTLDefConst dc -> do
        let fqn = FullyQualifiedName replModuleName (_dcName dc) replModuleHash
        loaded . loAllLoaded %= M.insert fqn (DConst dc)
        pure $ RLoadedDefConst $ _dcName dc
