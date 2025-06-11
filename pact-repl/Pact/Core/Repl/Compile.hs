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
 , interpretReplProgramDirect
 , interpretEvalBigStep
 , interpretEvalDirect
 , interpretReplProgram
 , ReplInterpreter
 , isPactFile
 , loadFile
 , defaultLoadFile
 , mkReplState
 , mkReplState'
 , checkParsedShadows
 ) where

import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Text(Text)
import Data.Default
import Data.IORef
import Data.Foldable
import System.FilePath.Posix


import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
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
import Pact.Core.Environment
import Pact.Core.Info
import Pact.Core.Errors
import Pact.Core.Interpreter
import Pact.Core.Pretty hiding (pipe, line)
import Pact.Core.Serialise
import Pact.Core.PactValue
import Pact.Core.NativeShadowing
import Pact.Core.Coverage.Types
import Pact.Core.Coverage


import Pact.Core.IR.Eval.Runtime
import Pact.Core.Repl.Runtime.ReplBuiltin

import Pact.Core.Repl.UserDocs
import Pact.Core.Repl.BuiltinDocs

import qualified Pact.Core.Syntax.ParseTree as Lisp
import qualified Pact.Core.Syntax.Lexer as Lisp
import qualified Pact.Core.Syntax.Parser as Lisp
import qualified Pact.Core.IR.Eval.CEK.Evaluator as CEK
import qualified Pact.Core.IR.Eval.Direct.Evaluator as Direct
import qualified Pact.Core.IR.Eval.Direct.ReplBuiltin as Direct
import Data.Maybe (catMaybes)

type ReplInterpreter = Interpreter ReplRuntime ReplCoreBuiltin FileLocSpanInfo

-- Small internal debugging function for playing with file loading within
-- this module
data ReplCompileValue
  = RCompileValue (CompileValue FileLocSpanInfo)
  | RLoadedDefun Text
  | RLoadedDefConst Text
  | RBuiltinDoc Text
  | RUserDoc (EvalDef ReplCoreBuiltin FileLocSpanInfo) (Maybe Text)
  deriving Show

mkReplState
  :: EvalEnv b FileLocSpanInfo
  -> (FileLocSpanInfo -> Text -> EvalM 'ReplRuntime b FileLocSpanInfo ())
  -> (FilePath -> Bool -> EvalM 'ReplRuntime b FileLocSpanInfo ())
  -> ReplState b
mkReplState ee printfn loadFn =
  ReplState
    { _replFlags = mempty
    , _replEvalEnv = ee
    , _replLogType = ReplStdOut
    , _replCurrSource = defaultSrc
    , _replUserDocs = mempty
    , _replTLDefPos = mempty
    , _replTx = Nothing
    , _replNativesEnabled = False
    , _replTraceLine = printfn
    , _replPrintLine = printfn
    , _replLoad = loadFn
    , _replLoadedFiles = mempty
    , _replTestResults = []
    , _replCoverage = ReplCoverage False (LcovReport mempty)
    }
  where
  defaultSrc = SourceCode "(interactive)" mempty

mkReplState'
  :: EvalEnv ReplCoreBuiltin FileLocSpanInfo
  -> (FileLocSpanInfo -> Text -> EvalM 'ReplRuntime ReplCoreBuiltin FileLocSpanInfo ())
  -> ReplState ReplCoreBuiltin
mkReplState' ee printfn =
  ReplState
    { _replFlags = mempty
    , _replEvalEnv = ee
    , _replLogType = ReplStdOut
    , _replCurrSource = defaultSrc
    , _replUserDocs = mempty
    , _replTLDefPos = mempty
    , _replTx = Nothing
    , _replNativesEnabled = False
    , _replTraceLine = printfn
    , _replPrintLine = printfn
    , _replLoad = \f reset -> void (loadFile interpretEvalDirect f reset)
    , _replLoadedFiles = mempty
    , _replTestResults = []
    , _replCoverage = ReplCoverage False (LcovReport mempty)
    }
  where
  defaultSrc = SourceCode "(interactive)" mempty

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



interpretReplProgramBigStep
  :: SourceCode
  -> ReplM ReplCoreBuiltin [ReplCompileValue]
interpretReplProgramBigStep = interpretReplProgram interpretEvalBigStep


interpretReplProgramDirect
  :: SourceCode
  -> ReplM ReplCoreBuiltin [ReplCompileValue]
interpretReplProgramDirect = interpretReplProgram interpretEvalDirect

checkReplNativesEnabled :: TopLevel n t (ReplBuiltin b) FileLocSpanInfo -> ReplM ReplCoreBuiltin ()
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


setBuiltinResolution :: SourceCode -> ReplM (ReplBuiltin CoreBuiltin) ()
setBuiltinResolution (SourceCode fp _)
  | sourceIsPactFile =
    replEvalEnv . eeNatives .== replCoreBuiltinOnlyMap
  | otherwise =
    replEvalEnv . eeNatives .== replBuiltinMap
  where
  sourceIsPactFile = isPactFile fp

defaultLoadFile :: FilePath -> Bool -> EvalM ReplRuntime ReplCoreBuiltin FileLocSpanInfo ()
defaultLoadFile f reset = () <$ loadFile interpretEvalDirect f reset

-- | Load a file onto the repl, optionally resetting all state.
loadFile :: ReplInterpreter -> FilePath -> Bool -> EvalM ReplRuntime ReplCoreBuiltin FileLocSpanInfo [ReplCompileValue]
loadFile interpreter txt reset  = do
  -- loadFile may be called between expressions, so
  -- we have to preserve some state before and after the call.

  -- When the repl enters another file and finishes,
  -- all other code executed must have a valid reference to the "current" source
  checkOrCreateFileReport txt
  oldSrc <- useReplState replCurrSource
  pactdb <- liftIO (mockPactDb serialisePact_repl_fileLocSpanInfo)
  -- Similarly, the eval env is preseved
  oldEE <- useReplState replEvalEnv
  when reset $ do
    ee <- liftIO (defaultEvalEnv pactdb replBuiltinMap)
    -- Reset the eval state, so name resolution starts from scratch in the new file
    put def
    replEvalEnv .== ee
  fp <- mangleFilePath txt
  when (isPactFile fp) $ esLoaded . loToplevel .= mempty
  source <- SourceCode fp <$> liftIO (T.readFile fp)
  replCurrSource .== source
  out <- interpretReplProgram interpreter source
  replCurrSource .== oldSrc

  -- We reset our native resolution to be the one scoped to this file, for consistency.
  -- We allow eval env changes to persist across files
  replEvalEnv . eeNatives .== view eeNatives oldEE

  pure out

mangleFilePath :: FilePath -> EvalM ReplRuntime b FileLocSpanInfo FilePath
mangleFilePath fp = do
  (SourceCode currFile _) <- useReplState replCurrSource
  case currFile of
    "(interactive)" -> pure fp
    _ | isAbsolute fp -> pure fp
      | takeFileName currFile == currFile -> pure fp
      | otherwise -> pure $ combine (takeDirectory currFile) fp

checkParsedShadows :: FilePath -> IO ()
checkParsedShadows fp = do
  source <- T.readFile fp
  case Lisp.lexer source >>= Lisp.parseReplProgram of
    Left err -> do
      let errorLoc = view peInfo err
          msg = pretty errorLoc <> ":" <+> pretty err
      T.putStrLn (renderCompactText' msg)
    Right program -> do
      let (_, reverse -> shadows) = runShadowsM replBuiltinMap (traverse checkReplTopLevelShadows program)
      case shadows of
        [] -> pure ()
        _ -> do
          let shadowOutput = vsep $ fmap (\s@(Shadows _ _ i) -> pretty i <> ":" <+> pretty s) shadows
          T.putStrLn $ renderCompactText' shadowOutput

liftShadowsReplM :: ShadowsM b FileLocSpanInfo a -> ReplM b a
liftShadowsReplM act = do
  natives <- viewEvalEnv eeNatives
  let (a, shadows) = runShadowsM natives act
  case reverse shadows of
    [] -> pure a
    h:rest -> do
      traverse_ printShadow (h:rest)
      throwError $ toShadowingError h
  where
  printShadow s@(Shadows _ _ i) = replPrintLn i s
  toShadowingError (Shadows _ arg i) = PEDesugarError (InvalidNativeShadowing arg) i

coverTopLevel :: TopLevel name ty b1 FileLocSpanInfo -> EvalM ReplRuntime b2 FileLocSpanInfo ()
coverTopLevel = \case
  TLModule mdl -> coverModule mdl
  _ -> pure ()

coverModule :: Module name ty b1 FileLocSpanInfo -> EvalM ReplRuntime b2 FileLocSpanInfo ()
coverModule mdl = do
  let (FileLocSpanInfo file _) = _mInfo mdl
  ReplCoverage enabled _ <- useReplState replCoverage
  when enabled $ do
    fqns <- catMaybes <$> traverse coverDefs (_mDefs mdl)
    let functionCovers = M.fromList
          [ (fqn, FunctionReport fqn line 0)
          | (fqn, FileLocSpanInfo _ si) <- fqns
          , let line = _liStartLine si]
    replCoverage .covReport . lcovReport %== M.adjust (over fileReportFunctions (M.union functionCovers)) file
  where
  createLineTick (FileLocSpanInfo file info) = do
    let line = _liStartLine info
        updateLine f = f & fileReportLines %~ IM.insertWith mergeLineReport line (LineReport line 0)
    replCoverage . covReport . lcovReport %== M.adjust updateLine file
  createBranchTick (FileLocSpanInfo file info) = do
    let line = _liStartLine info
        updateBranch f = f & fileReportBranches %~ IM.insertWith mergeBranchReport line (BranchReport line 0 0)
    replCoverage . covReport . lcovReport %== M.adjust updateBranch file
  coverTerms t = forM_ (universe t) $ \case
    BuiltinForm (CIf _ _ _) i -> do
      createLineTick i
      createBranchTick i
    c -> createLineTick (view termInfo c)
  coverDefs = \case
    DSchema{} -> pure Nothing
    DTable{} -> pure Nothing
    DConst{} -> pure Nothing
    defn@(Dfun dfn) -> do
      let fqn = FullyQualifiedName (_mName mdl) (defName defn) (_mHash mdl)
      coverTerms (_dfunTerm dfn)
      pure $ Just (fqn, defInfo defn)
    defn@(DCap dfn) -> do
      let fqn = FullyQualifiedName (_mName mdl) (defName defn) (_mHash mdl)
      coverTerms (_dcapTerm dfn)
      pure $ Just (fqn, defInfo defn)
    defn@(DPact dp) -> do
      let fqn = FullyQualifiedName (_mName mdl) (defName defn) (_mHash mdl)
      (traverse_.traverseDefPactStep) (\c -> c <$ coverTerms c) (_dpSteps dp)
      pure $ Just (fqn, defInfo defn)


interpretReplProgram
  :: ReplInterpreter
  -> SourceCode
  -> ReplM ReplCoreBuiltin [ReplCompileValue]
interpretReplProgram interpreter sc@(SourceCode sourceFp source) = do
  replLoadedFiles %== M.insert sourceFp sc
  lexx <- liftEither $ over _Left (fmap toFileLoc) (Lisp.lexer source)
  debugIfFlagSet ReplDebugLexer lexx
  parsed <- liftEither $ bimap (fmap toFileLoc) ((fmap.fmap) toFileLoc) (parseSource lexx)
  setBuiltinResolution sc
  traverse_ (liftShadowsReplM . checkReplTopLevelShadows) parsed
  traverse pipe' parsed
  where
  renderDoc info doc = liftIO (renderBuiltinDoc doc) >>= \case
    Right d -> pure d
    Left err -> do
      let errMsg = "INTERNAL ERROR: Please report to the pact team: " <> T.pack (show err)
      throwExecutionError info (UnknownException errMsg)
  toFileLoc = FileLocSpanInfo sourceFp
  sourceIsPactFile = isPactFile sourceFp
  parseSource lexerOutput
    | sourceIsPactFile = (fmap.fmap) (Lisp.RTLTopLevel) $ Lisp.parseProgram lexerOutput
    | otherwise = Lisp.parseReplProgram lexerOutput
  displayValue :: FileLocSpanInfo -> ReplCompileValue -> ReplM ReplCoreBuiltin ReplCompileValue
  displayValue _info v@(RCompileValue (InterpretValue PUnit _)) = pure v
  displayValue info p = p <$ replTraceLn info p
  sliceCode = \case
    Lisp.TLModule{} -> sliceFromSource
    Lisp.TLInterface{} -> sliceFromSource
    Lisp.TLTerm{} -> \_ _ -> mempty
    Lisp.TLUse{} -> \_ _ -> mempty
  pipe' tl = case tl of
    Lisp.RTLTopLevel toplevel -> case topLevelHasDocs toplevel of
      Just doc -> do
        ansiDoc <- renderDoc tlInfo doc
        displayValue tlInfo $ RBuiltinDoc ansiDoc
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
                displayValue tlInfo (RUserDoc d docs)
              Nothing ->
                throwExecutionError varI $ EvalError "repl invariant violated: resolved to a top level free variable without a binder"
          _ -> do
            coverTopLevel ds
            let sliced = sliceCode toplevel source (view spanInfo tlInfo)
            v <- RCompileValue <$> evalTopLevel interpreter (RawCode sliced) ds deps
            emitWarnings
            displayValue tlInfo v
      where
      tlInfo = view Lisp.topLevelInfo toplevel
    _ ->  do
      ds <- runDesugarReplTopLevel tl
      interpret ds
  emitWarnings =
    viewEvalEnv eeWarnings >>= \case
      Nothing -> pure ()
      Just ref -> do
        warnings <- liftIO $
          atomicModifyIORef' ref (\old -> (newDefaultWarningStack, getWarningStack old))
        -- Todo: print located line
        -- Note: warnings are pushed FIFO, so we reverse to get the right order
        traverse_ (\(Located loc e) -> replPrintLn loc e) (reverse warnings)
  interpret (DesugarOutput tl _deps) = do
    case tl of
      RTLDefun df -> do
        let fqn = FullyQualifiedName replModuleName (_argName $ _dfunSpec df) replModuleHash
        loaded . loAllLoaded %= M.insert fqn (Dfun df)
        displayValue (_dfunInfo df)  $ RLoadedDefun $ _argName $ _dfunSpec df
      RTLDefConst dc -> case _dcTerm dc of
        TermConst term -> do
          pv <- eval interpreter PSysOnly term
          emitWarnings
          maybeTCType (_dcInfo dc) (_argType $ _dcSpec dc) pv
          let dc' = set dcTerm (EvaledConst pv) dc
          let fqn = FullyQualifiedName replModuleName (_argName $ _dcSpec dc) replModuleHash
          loaded . loAllLoaded %= M.insert fqn (DConst dc')
          displayValue (_dcInfo dc) $ RLoadedDefConst $ _argName $ _dcSpec dc'
        EvaledConst _ -> do
          let fqn = FullyQualifiedName replModuleName (_argName $ _dcSpec dc) replModuleHash
          loaded . loAllLoaded %= M.insert fqn (DConst dc)
          displayValue (_dcInfo dc) $ RLoadedDefConst $ _argName $ _dcSpec dc
