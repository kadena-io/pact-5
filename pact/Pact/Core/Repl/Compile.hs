{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}



module Pact.Core.Repl.Compile
 ( ReplCompileValue(..)
 , interpretReplProgramBigStep
 , loadFile
 , interpretReplProgramDirect
 , interpretEvalBigStep
 , interpretEvalDirect
 , interpretReplProgram
 , ReplInterpreter
 , isPactFile
 ) where

import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.State.Strict
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
import Pact.Core.Interpreter
import Pact.Core.Pretty hiding (pipe)
import Pact.Core.Serialise (serialisePact_repl_spaninfo)


import Pact.Core.IR.Eval.Runtime
import Pact.Core.Repl.Runtime.ReplBuiltin

import Pact.Core.Repl.UserDocs
import Pact.Core.Repl.BuiltinDocs

import qualified Pact.Core.Syntax.ParseTree as Lisp
import qualified Pact.Core.Syntax.Lexer as Lisp
import qualified Pact.Core.Syntax.Parser as Lisp
import qualified Pact.Core.IR.Eval.CEK as CEK
import qualified Pact.Core.IR.Eval.Direct.Evaluator as Direct
import qualified Pact.Core.IR.Eval.Direct.ReplBuiltin as Direct

type ReplInterpreter = Interpreter ReplRuntime ReplCoreBuiltin SpanInfo

-- Small internal debugging function for playing with file loading within
-- this module
data ReplCompileValue
  = RCompileValue (CompileValue SpanInfo)
  | RLoadedDefun Text
  | RLoadedDefConst Text
  | RBuiltinDoc Text
  | RUserDoc (EvalDef ReplCoreBuiltin SpanInfo) (Maybe Text)
  deriving Show

instance Pretty ReplCompileValue where
  pretty = \case
    RCompileValue cv -> pretty cv
    RLoadedDefun mn ->
      "Loaded repl defun" <+> pretty mn
    RLoadedDefConst mn ->
      "Loaded repl defconst" <+> pretty mn
    RBuiltinDoc doc -> pretty doc
    RUserDoc qn doc ->
      vsep [pretty qn, "Docs:", maybe mempty pretty doc]


-- | Internal function for loading a file.
--   Exported because it is used in the tests.
loadFile
  :: FilePath
  -> ReplInterpreter
  -> (ReplCompileValue -> ReplM ReplCoreBuiltin ())
  -> ReplM ReplCoreBuiltin [ReplCompileValue]
loadFile loc rEnv display = do
  source <- SourceCode loc <$> liftIO (T.readFile loc)
  replCurrSource .== source
  interpretReplProgram rEnv source display


interpretReplProgramBigStep
  :: SourceCode
  -> (ReplCompileValue -> ReplM ReplCoreBuiltin ())
  -> ReplM ReplCoreBuiltin [ReplCompileValue]
interpretReplProgramBigStep = interpretReplProgram interpretEvalBigStep


interpretReplProgramDirect
  :: SourceCode
  -> (ReplCompileValue -> ReplM ReplCoreBuiltin ())
  -> ReplM ReplCoreBuiltin [ReplCompileValue]
interpretReplProgramDirect = interpretReplProgram interpretEvalDirect

checkReplNativesEnabled :: TopLevel n t (ReplBuiltin b) SpanInfo -> ReplM ReplCoreBuiltin ()
checkReplNativesEnabled = \case
  TLModule m -> do
    flag <- useReplState replNativesEnabled
    unless flag $
      () <$ traverseModuleTerm hasReplNatives m
  _ -> pure ()
  where
  hasReplNatives = transformM $ \case
    Builtin (RBuiltinRepl _) i ->
      throwExecutionError i (EvalError "repl native disallowed in module code. If you want to use this, enable them with (env-enable-repl-natives true)")
    a ->  pure a

interpretEvalBigStep :: ReplInterpreter
interpretEvalBigStep =
  Interpreter { eval = evalBigStep, resumePact = evalResumePact, interpretGuard = interpretGuardBigStep}
  where
  evalBigStep purity term =
    CEK.eval purity replBuiltinEnv term
  evalResumePact info pactExec =
    CEK.evalResumePact info replBuiltinEnv pactExec
  interpretGuardBigStep info g =
    CEK.interpretGuard info replBuiltinEnv g

interpretEvalDirect :: ReplInterpreter
interpretEvalDirect =
  Interpreter { eval = evalDirect, resumePact = evalResumePact, interpretGuard = interpretGuardDirect}
  where
  evalDirect purity term =
    Direct.eval purity Direct.replBuiltinEnv term
  evalResumePact info pactExec =
    Direct.evalResumePact info Direct.replBuiltinEnv pactExec
  interpretGuardDirect info g =
    Direct.interpretGuard info Direct.replBuiltinEnv g

isPactFile :: FilePath -> Bool
isPactFile f = takeExtension f == ".pact"

interpretReplProgram
  :: ReplInterpreter
  -> SourceCode
  -> (ReplCompileValue -> ReplM ReplCoreBuiltin ())
  -> ReplM ReplCoreBuiltin [ReplCompileValue]
interpretReplProgram interpreter (SourceCode sourceFp source) display = do
  lexx <- liftEither (Lisp.lexer source)
  debugIfFlagSet ReplDebugLexer lexx
  parsed <- parseSource lexx
  setBuiltinResolution
  concat <$> traverse pipe parsed
  where
  sourceIsPactFile = isPactFile sourceFp
  parseSource lexerOutput
    | sourceIsPactFile = (fmap.fmap) (Lisp.RTL . Lisp.RTLTopLevel) $ liftEither $ Lisp.parseProgram lexerOutput
    | otherwise = liftEither $ Lisp.parseReplProgram lexerOutput
  setBuiltinResolution
    | sourceIsPactFile =
      replEvalEnv . eeNatives .== replCoreBuiltinOnlyMap
    | otherwise =
      replEvalEnv . eeNatives .== replBuiltinMap
  displayValue p = p <$ display p
  pipe = \case
    Lisp.RTL rtl ->
      pure <$> pipe' rtl
    Lisp.RTLReplSpecial rsf -> case rsf of
      -- Load is a bit special
      Lisp.ReplLoad txt reset i -> do
        let loading = RCompileValue (InterpretValue (PString ("Loading " <> txt <> "...")) i)
        display loading
        oldSrc <- useReplState replCurrSource
        pactdb <- liftIO (mockPactDb serialisePact_repl_spaninfo)
        oldEE <- useReplState replEvalEnv
        when reset $ do
          ee <- liftIO (defaultEvalEnv pactdb replBuiltinMap)
          put def
          replEvalEnv .== ee
        fp <- mangleFilePath (T.unpack txt)
        when (isPactFile fp) $ esLoaded . loToplevel .= mempty
        out <- loadFile fp interpreter display
        replCurrSource .== oldSrc
        unless reset $ do
          replEvalEnv .== oldEE
        setBuiltinResolution
        pure out
  mangleFilePath fp = do
    (SourceCode currFile _) <- useReplState replCurrSource
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
        (ds, deps) <- compileDesugarOnly interpreter toplevel
        checkReplNativesEnabled ds
        case ds of
          TLTerm (Var (Name n (NTopLevel mn mh)) varI) -> do
            let fqn = FullyQualifiedName mn n mh
            lookupFqName fqn >>= \case
              Just d -> do
                let qn = QualifiedName n mn
                docs <- usesReplState replUserDocs (M.lookup qn)
                displayValue (RUserDoc d docs)
              Nothing ->
                throwExecutionError varI $ EvalError "repl invariant violated: resolved to a top level free variable without a binder"
          _ -> do
            v <- evalTopLevel interpreter ds deps
            displayValue (RCompileValue v)
    _ ->  do
      ds <- runDesugarReplTopLevel tl
      interpret ds
  interpret (DesugarOutput tl _deps) = do
    case tl of
      RTLDefun df -> do
        let fqn = FullyQualifiedName replModuleName (_argName $ _dfunSpec df) replModuleHash
        loaded . loAllLoaded %= M.insert fqn (Dfun df)
        displayValue $ RLoadedDefun $ _argName $ _dfunSpec df
      RTLDefConst dc -> case _dcTerm dc of
        TermConst term -> do
          pv <- eval interpreter PSysOnly term
          maybeTCType (_dcInfo dc) (_argType $ _dcSpec dc) pv
          let dc' = set dcTerm (EvaledConst pv) dc
          let fqn = FullyQualifiedName replModuleName (_argName $ _dcSpec dc) replModuleHash
          loaded . loAllLoaded %= M.insert fqn (DConst dc')
          displayValue $ RLoadedDefConst $ _argName $ _dcSpec dc'
        EvaledConst _ -> do
          let fqn = FullyQualifiedName replModuleName (_argName $ _dcSpec dc) replModuleHash
          loaded . loAllLoaded %= M.insert fqn (DConst dc)
          displayValue $ RLoadedDefConst $ _argName $ _dcSpec dc
