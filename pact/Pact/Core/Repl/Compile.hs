{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms #-}



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
 , ReplLoadFile(..)
 , topLevelIsReplLoad
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
import Pact.Core.Literal

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
  Interpreter
  { eval = evalBigStep
  , resumePact = evalResumePact
  , interpretGuard = interpretGuardBigStep
  , evalWithCapability = evalWithCap}
  where
  evalBigStep purity term =
    CEK.eval purity replBuiltinEnv term
  evalResumePact info pactExec =
    CEK.evalResumePact info replBuiltinEnv pactExec
  interpretGuardBigStep info g =
    CEK.interpretGuard info replBuiltinEnv g
  evalWithCap info purity ct term =
    CEK.evalWithinCap info purity replBuiltinEnv ct term

interpretEvalDirect :: ReplInterpreter
interpretEvalDirect =
  Interpreter { eval = evalDirect
  , resumePact = evalResumePact
  , interpretGuard = interpretGuardDirect
  , evalWithCapability = evalWithCap}
  where
  evalDirect purity term =
    Direct.eval purity Direct.replBuiltinEnv term
  evalResumePact info pactExec =
    Direct.evalResumePact info Direct.replBuiltinEnv pactExec
  interpretGuardDirect info g =
    Direct.interpretGuard info Direct.replBuiltinEnv g
  evalWithCap info purity ct term =
    Direct.evalWithinCap info purity Direct.replBuiltinEnv ct term

isPactFile :: FilePath -> Bool
isPactFile f = takeExtension f == ".pact"

pattern PReplLoadWithClear :: Text -> Bool -> i -> Lisp.ReplTopLevel i
pattern PReplLoadWithClear file reset info <-
  Lisp.RTLTopLevel (
    Lisp.TLTerm (Lisp.App (Lisp.Var (BN (BareName "load")) _)
    [ Lisp.Constant (LString file) _
    , Lisp.Constant (LBool reset) _]
    info)
  )

pattern PReplLoad :: Text -> i -> Lisp.ReplTopLevel i
pattern PReplLoad file info <-
  Lisp.RTLTopLevel (
    Lisp.TLTerm (Lisp.App (Lisp.Var (BN (BareName "load")) _)
    [ Lisp.Constant (LString file) _]
    info)
  )

data ReplLoadFile i
  = ReplLoadFile
  { _rlFile :: Text
  , _rlReset :: Bool
  , _rlInfo :: i
  } deriving (Show)

topLevelIsReplLoad :: Lisp.ReplTopLevel i -> Either (Lisp.ReplTopLevel i) (ReplLoadFile i)
topLevelIsReplLoad = \case
  PReplLoad file i -> Right (ReplLoadFile file False i)
  PReplLoadWithClear file reset i -> Right (ReplLoadFile file reset i)
  t -> Left t

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
    | sourceIsPactFile = (fmap.fmap) (Lisp.RTLTopLevel) $ liftEither $ Lisp.parseProgram lexerOutput
    | otherwise = liftEither $ Lisp.parseReplProgram lexerOutput
  setBuiltinResolution
    | sourceIsPactFile =
      replEvalEnv . eeNatives .== replCoreBuiltinOnlyMap
    | otherwise =
      replEvalEnv . eeNatives .== replBuiltinMap
  pipe t = case topLevelIsReplLoad t of
    Left tl -> pure <$> pipe' tl
    Right (ReplLoadFile file reset info) -> doLoadFile file reset info
  displayValue p = p <$ display p
  doLoadFile txt reset i = do
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
