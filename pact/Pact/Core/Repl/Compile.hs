{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}



module Pact.Core.Repl.Compile
 ( ReplCompileValue(..)
 , interpretReplProgram
 , interpretReplProgramSmallStep
 , loadFile
 ) where

import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class(liftIO)
import Data.Text(Text)
import Data.Default
import System.FilePath.Posix


import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Pact.Core.Persistence
import Pact.Core.Persistence.MockPersistence (mockPactDb)
import Pact.Core.Builtin
import Pact.Core.Names
import Pact.Core.Repl.Utils
import Pact.Core.IR.Desugar
import Pact.Core.IR.Term
import Pact.Core.Compile
import Pact.Core.Type
import Pact.Core.Environment
import Pact.Core.Info
import Pact.Core.PactValue
import Pact.Core.Errors
import Pact.Core.Serialise (serialisePact_repl_spaninfo)


import Pact.Core.IR.Eval.Runtime
import Pact.Core.IR.Eval.CEK(CEKEval)
import Pact.Core.Repl.Runtime.ReplBuiltin

import Pact.Core.Repl.UserDocs
import Pact.Core.Repl.BuiltinDocs

import qualified Pact.Core.Syntax.ParseTree as Lisp
import qualified Pact.Core.Syntax.Lexer as Lisp
import qualified Pact.Core.Syntax.Parser as Lisp
import qualified Pact.Core.IR.Eval.CEK as CEK

type Repl = ReplM ReplCoreBuiltin

-- Small internal debugging function for playing with file loading within
-- this module
data ReplCompileValue
  = RCompileValue (CompileValue SpanInfo)
  | RLoadedDefun Text
  | RLoadedDefConst Text
  | RBuiltinDoc Text
  | RUserDoc (EvalDef ReplCoreBuiltin SpanInfo) (Maybe Text)
  deriving Show

-- | Internal function for loading a file.
--   Exported because it is used in the tests.
loadFile
  :: (CEKEval step ReplCoreBuiltin SpanInfo Repl)
  => FilePath
  -> BuiltinEnv step ReplCoreBuiltin SpanInfo Repl
  -> (ReplCompileValue -> ReplM ReplCoreBuiltin ())
  -> ReplM ReplCoreBuiltin [ReplCompileValue]
loadFile loc rEnv display = do
  source <- SourceCode loc <$> liftIO (T.readFile loc)
  replCurrSource .= source
  interpretReplProgram' rEnv source display


interpretReplProgram
  :: SourceCode
  -> (ReplCompileValue -> ReplM ReplCoreBuiltin ())
  -> ReplM ReplCoreBuiltin [ReplCompileValue]
interpretReplProgram = interpretReplProgram' (replBuiltinEnv @CEKBigStep)

interpretReplProgramSmallStep
  :: SourceCode
  -> (ReplCompileValue -> ReplM ReplCoreBuiltin ())
  -> ReplM ReplCoreBuiltin [ReplCompileValue]
interpretReplProgramSmallStep = interpretReplProgram' (replBuiltinEnv @CEKSmallStep)

checkReplNativesEnabled :: TopLevel n t (ReplBuiltin b) SpanInfo -> ReplM ReplCoreBuiltin ()
checkReplNativesEnabled = \case
  TLModule m -> do
    flag <- use replNativesEnabled
    unless flag $
      () <$ traverseModuleTerm hasReplNatives m
  _ -> pure ()
  where
  hasReplNatives = transformM $ \case
    Builtin (RBuiltinRepl _) i ->
      throwExecutionError i (EvalError "repl native disallowed in module code. If you want to use this, enable them with (env-enable-repl-natives true)")
    a ->  pure a

interpretReplProgram'
  :: (CEKEval step ReplCoreBuiltin SpanInfo Repl)
  => BuiltinEnv step ReplCoreBuiltin SpanInfo Repl
  -> SourceCode
  -> (ReplCompileValue -> ReplM ReplCoreBuiltin ())
  -> ReplM ReplCoreBuiltin [ReplCompileValue]
interpretReplProgram' replEnv (SourceCode _ source) display = do
  lexx <- liftEither (Lisp.lexer source)
  debugIfFlagSet ReplDebugLexer lexx
  parsed <- liftEither $ Lisp.parseReplProgram lexx
  concat <$> traverse pipe parsed
  where
  displayValue p = p <$ display p
  pipe = \case
    Lisp.RTL rtl ->
      pure <$> pipe' rtl
    Lisp.RTLReplSpecial rsf -> case rsf of
      Lisp.ReplLoad txt reset i -> do
        let loading = RCompileValue (InterpretValue (PString ("Loading " <> txt <> "...")) i)
        display loading
        oldSrc <- use replCurrSource
        pactdb <- liftIO (mockPactDb serialisePact_repl_spaninfo)
        oldEE <- use replEvalEnv
        when reset $ do
          ee <- liftIO (defaultEvalEnv pactdb replCoreBuiltinMap)
          evalState .= def
          replEvalEnv .= ee
        fp <- mangleFilePath (T.unpack txt)
        when (isPactFile fp) $ esLoaded . loToplevel .= mempty
        out <- loadFile fp replEnv display
        replCurrSource .= oldSrc
        unless reset $ do
          replEvalEnv .= oldEE
        pure out
  mangleFilePath fp = do
    (SourceCode currFile _) <- use replCurrSource
    case currFile of
      "(interactive)" -> pure fp
      _ | isAbsolute fp -> pure fp
        | takeFileName currFile == currFile -> pure fp
        | otherwise -> pure $ combine (takeDirectory currFile) fp
  pipe' tl = case tl of
    Lisp.RTLTopLevel toplevel -> case topLevelHasDocs toplevel of
      Just doc -> displayValue $ RBuiltinDoc doc
      Nothing -> do
        functionDocs toplevel
        (ds, deps) <- compileDesugarOnly replEnv toplevel
        checkReplNativesEnabled ds
        case ds of
          TLTerm (Var (Name n (NTopLevel mn mh)) varI) -> do
            let fqn = FullyQualifiedName mn n mh
            lookupFqName fqn >>= \case
              Just d -> do
                let qn = QualifiedName n mn
                docs <- uses replUserDocs (M.lookup qn)
                displayValue (RUserDoc d docs)
              Nothing ->
                failInvariant varI "repl invariant violated: resolved to a top level free variable without a binder"
          _ -> do
            v <- evalTopLevel replEnv ds deps
            displayValue (RCompileValue v)
    _ ->  do
      ds <- runDesugarReplTopLevel tl
      interpret ds
  isPactFile f = takeExtension f == ".pact"
  interpret (DesugarOutput tl _deps) = do
    case tl of
      RTLDefun df -> do
        let fqn = FullyQualifiedName replModuleName (_argName $ _dfunSpec df) replModuleHash
        loaded . loAllLoaded %= M.insert fqn (Dfun df)
        displayValue $ RLoadedDefun $ _argName $ _dfunSpec df
      RTLDefConst dc -> case _dcTerm dc of
        TermConst term -> do
          pv <- CEK.eval PSysOnly replEnv term
          pv' <- maybeTCType (_dcInfo dc) pv (_argType $ _dcSpec dc)
          let dc' = set dcTerm (EvaledConst pv') dc
          let fqn = FullyQualifiedName replModuleName (_argName $ _dcSpec dc) replModuleHash
          loaded . loAllLoaded %= M.insert fqn (DConst dc')
          displayValue $ RLoadedDefConst $ _argName $ _dcSpec dc'
        EvaledConst _ -> do
          let fqn = FullyQualifiedName replModuleName (_argName $ _dcSpec dc) replModuleHash
          loaded . loAllLoaded %= M.insert fqn (DConst dc)
          displayValue $ RLoadedDefConst $ _argName $ _dcSpec dc
