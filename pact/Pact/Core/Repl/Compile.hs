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
 , interpretWithBreakpoints
 ) where

import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class(liftIO, MonadIO)
import Control.Monad.State(MonadState)
import Data.Text(Text)
import Data.Default
import System.FilePath.Posix


import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Kind as K

import Pact.Core.Persistence
import Pact.Core.Persistence.MockPersistence (mockPactDb)
import Pact.Core.Builtin
import Pact.Core.Names
import Pact.Core.Repl.Utils
import Pact.Core.IR.Desugar
import Pact.Core.IR.Term
import Pact.Core.Compile
import Pact.Core.Environment
import Pact.Core.Info
import Pact.Core.Errors
import Pact.Core.PactValue
import Pact.Core.Serialise (serialisePact_repl_spaninfo)


import Pact.Core.IR.Eval.Runtime
import Pact.Core.IR.Eval.CEK
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

loadFile
  :: (MonadIO m, MonadState (ReplState b) m)
  => FilePath
  -> m SourceCode
loadFile loc = do
  source <- SourceCode loc <$> liftIO (T.readFile loc)
  replCurrSource .= source
  pure source

smallStepEnv :: BuiltinEnv   CEKSmallStep   (ReplBuiltin CoreBuiltin)   SpanInfo   (ReplM (ReplBuiltin CoreBuiltin))
smallStepEnv = replBuiltinEnv @CEKSmallStep

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

type WithReplTypes ty = ty ReplCoreBuiltin SpanInfo (ReplM ReplCoreBuiltin)

data BpEvalKind = BpEvalStep | BpEvalManySteps

data BpEvalResult :: BpEvalKind -> K.Type where
  FinishedEval :: WithReplTypes (EvalResult CEKSmallStep) -> BpEvalResult k
  BpNotHit :: BpEvalResult BpEvalStep
  BpHit :: BpEvalResult k -- TODO carry around the frame/context

type BpStepEvalResult = BpEvalResult BpEvalStep
type BpManyStepsEvalResult = BpEvalResult BpEvalManySteps
type BpMachineResult = WithReplTypes CEKReturn

isFinal :: CEKReturn b i m -> Bool
isFinal (CEKReturn Mt CEKNoHandler _) = True
isFinal _ = False

isBreakpoint :: EvalTerm ReplCoreBuiltin SpanInfo -> ReplM ReplCoreBuiltin Bool
isBreakpoint term = do
  currSrc <- use replCurrSource
  bps <- M.findWithDefault mempty (_scFileName currSrc) <$> use replBreakpoints
  pure False
  where
  info = view termInfo term

evalStepBp :: BpMachineResult -> ReplM ReplCoreBuiltin (BpStepEvalResult, BpMachineResult)
evalStepBp c@(CEKReturn cont handler result)
  | isFinal c = pure (FinishedEval result, c)
  | otherwise = (BpNotHit, ) <$> CEK.returnCEK cont handler result
evalStepBp c@(CEKEvaluateTerm cont handler cekEnv term) = do
  isBp <- isBreakpoint term
  if isBp
     then pure (BpHit, c)
     else (BpNotHit, ) <$> CEK.evaluateTerm cont handler cekEnv term

evalUntilBp :: BpMachineResult -> ReplM ReplCoreBuiltin (BpManyStepsEvalResult, BpMachineResult)
evalUntilBp c = do
  (bpr, c') <- evalStepBp c
  case bpr of
    BpNotHit -> evalUntilBp c'
    BpHit -> pure (BpHit, c')
    FinishedEval r -> pure (FinishedEval r, c')

data ReplProgramContext
  = HaltedInterpretation (BpManyStepsEvalResult, BpMachineResult) [Lisp.ReplSpecialTL SpanInfo] [ReplCompileValue]
  | InterpretationComplete [ReplCompileValue]

ctxValues :: ReplProgramContext -> [ReplCompileValue]
ctxValues = \case
  HaltedInterpretation _ _ vals -> vals
  InterpretationComplete vals -> vals

interpretWithBreakpoints
  :: SourceCode
  -> (ReplCompileValue -> ReplM ReplCoreBuiltin ())
  -> ReplM ReplCoreBuiltin ReplProgramContext
interpretWithBreakpoints (SourceCode _ source) display = do
  lexx <- liftEither (Lisp.lexer source)
  debugIfFlagSet ReplDebugLexer lexx
  parsed <- liftEither $ Lisp.parseReplProgram lexx
  go parsed []
  where
  go [] acc = pure $ InterpretationComplete (reverse acc)
  go (toInterpret : rest) acc = case toInterpret of
    Lisp.RTL rtl ->
      goTL rtl rest acc
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
        src <- loadFile fp
        -- TODO better handling of halted interpretation
        result <- interpretWithBreakpoints src display
        replCurrSource .= oldSrc
        unless reset $ do
          replEvalEnv .= oldEE
        go rest (ctxValues result ++ acc)
  mangleFilePath fp = do
    (SourceCode currFile _) <- use replCurrSource
    case currFile of
      "(interactive)" -> pure fp
      _ | isAbsolute fp -> pure fp
        | takeFileName currFile == currFile -> pure fp
        | otherwise -> pure $ combine (takeDirectory currFile) fp
  goTL tl rest acc = case tl of
    -- Todo: this is actually referring to builtin docs
    -- The naming is sort of confusing
    Lisp.RTLTopLevel toplevel -> case topLevelHasDocs toplevel of
      Just doc -> do
        let v = RBuiltinDoc doc
        display v
        go rest (v:acc)
      Nothing -> do
        -- `functionDocs` handles user-defined docs for functions within a module
        functionDocs toplevel
        (ds, deps) <- compileDesugarOnly smallStepEnv toplevel
        -- TODO TODO: Set breakpoint here.
        case ds of
          TLTerm (Var (Name n (NTopLevel mn mh)) varI) -> do
            let fqn = FullyQualifiedName mn n mh
            lookupFqName fqn >>= \case
              Just d -> do
                let qn = QualifiedName n mn
                docs <- uses replUserDocs (M.lookup qn)
                let v = RUserDoc d docs
                display v
                go rest (v:acc)
              Nothing ->
                failInvariant varI "repl invariant violated: resolved to a top level free variable without a binder"
          TLTerm t -> do
            -- todo: breakpoint traversal
            ee <- readEnv
            let cekEnv = envFromPurity PImpure (CEKEnv mempty (_eePactDb ee) smallStepEnv (_eeDefPactStep ee) False)
            evalUntilBp (CEKEvaluateTerm Mt CEKNoHandler cekEnv t) >>= \case
              tup@(BpHit, _) -> pure $ HaltedInterpretation tup rest acc
              (FinishedEval r, _) -> case r of
                VError txt i -> throwExecutionError i (EvalError txt)
                EvalValue ev -> case ev of
                  VPactValue pv -> do
                    let v = RCompileValue (InterpretValue pv (view termInfo t))
                    display v
                    go rest (v:acc)
                  _ ->
                    throwExecutionError (view termInfo t) (EvalError "Evaluation did not reduce to a value")
          _ -> do
            v <- RCompileValue <$> evalTopLevel smallStepEnv ds deps
            display v
            go rest (v:acc)
    -- Case handling special top-level repl-only cases
    _ ->  do
      ds <- runDesugarReplTopLevel tl
      interpret rest acc ds
  isPactFile f = takeExtension f == ".pact"
  interpret rest acc (DesugarOutput tl _deps) = do
    case tl of
      RTLDefun df -> do
        let fqn = FullyQualifiedName replModuleName (_dfunName df) replModuleHash
        loaded . loAllLoaded %= M.insert fqn (Dfun df)
        let v = RLoadedDefun $ _dfunName df
        display v
        go rest (v:acc)
      RTLDefConst dc -> case _dcTerm dc of
        TermConst term -> do
          pv <- CEK.eval PSysOnly smallStepEnv term
          pv' <- maybeTCType (_dcInfo dc) pv (_dcType dc)
          let dc' = set dcTerm (EvaledConst pv') dc
          let fqn = FullyQualifiedName replModuleName (_dcName dc) replModuleHash
          loaded . loAllLoaded %= M.insert fqn (DConst dc')
          let v = RLoadedDefConst $ _dcName dc'
          display v
          go rest (v:acc)
        EvaledConst _ -> do
          let fqn = FullyQualifiedName replModuleName (_dcName dc) replModuleHash
          loaded . loAllLoaded %= M.insert fqn (DConst dc)
          let v = RLoadedDefConst $ _dcName dc
          display v
          go rest (v:acc)



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
        src <- loadFile fp
        out <- interpretReplProgram' smallStepEnv src display
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
    -- Todo: this is actually referring to builtin docs
    -- The naming is sort of confusing
    Lisp.RTLTopLevel toplevel -> case topLevelHasDocs toplevel of
      Just doc -> displayValue $ RBuiltinDoc doc
      Nothing -> do
        -- `functionDocs` handles user-defined docs for functions within a module
        functionDocs toplevel
        (ds, deps) <- compileDesugarOnly replEnv toplevel
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
    -- Case handling special top-level repl-only cases
    _ ->  do
      ds <- runDesugarReplTopLevel tl
      interpret ds
  isPactFile f = takeExtension f == ".pact"
  interpret (DesugarOutput tl _deps) = do
    case tl of
      RTLDefun df -> do
        let fqn = FullyQualifiedName replModuleName (_dfunName df) replModuleHash
        loaded . loAllLoaded %= M.insert fqn (Dfun df)
        displayValue $ RLoadedDefun $ _dfunName df
      RTLDefConst dc -> case _dcTerm dc of
        TermConst term -> do
          pv <- CEK.eval PSysOnly replEnv term
          pv' <- maybeTCType (_dcInfo dc) pv (_dcType dc)
          let dc' = set dcTerm (EvaledConst pv') dc
          let fqn = FullyQualifiedName replModuleName (_dcName dc) replModuleHash
          loaded . loAllLoaded %= M.insert fqn (DConst dc')
          displayValue $ RLoadedDefConst $ _dcName dc'
        EvaledConst _ -> do
          let fqn = FullyQualifiedName replModuleName (_dcName dc) replModuleHash
          loaded . loAllLoaded %= M.insert fqn (DConst dc)
          displayValue $ RLoadedDefConst $ _dcName dc
