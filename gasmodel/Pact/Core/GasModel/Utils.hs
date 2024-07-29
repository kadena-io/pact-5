{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Pact.Core.GasModel.Utils where

import Control.Lens
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.DeepSeq
import Data.Default
import Data.Text (Text)
import Data.Map.Strict(Map)
import Data.Monoid
import qualified Criterion as C
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Set as S


import Pact.Core.Builtin
import Pact.Core.Environment
import Pact.Core.Errors
import Pact.Core.Names
import Pact.Core.Literal
import Pact.Core.Type
import Pact.Core.Capabilities
import Pact.Core.IR.Desugar
import Pact.Core.IR.Eval.Runtime
import Pact.Core.IR.Eval.CoreBuiltin
import Pact.Core.PactValue
import Pact.Core.IR.Term
import Pact.Core.Persistence
import Pact.Core.Hash
import Pact.Core.Guards
import Pact.Core.Evaluate hiding (Cont(..))
import Pact.Core.Namespace
import Pact.Core.IR.Eval.CEK.Types hiding (Eval)
import qualified Pact.Core.IR.Eval.CEK as Eval

type CoreDb = PactDb CoreBuiltin Info
type MachineResult = CEKReturn ExecRuntime CoreBuiltin Info
type ApplyContToVEnv =
  ( EvalEnv CoreBuiltin Info
  , EvalState CoreBuiltin Info
  , Cont ExecRuntime CEKSmallStep CoreBuiltin Info
  , CEKErrorHandler ExecRuntime CEKSmallStep CoreBuiltin Info
  , CEKValue ExecRuntime CEKSmallStep CoreBuiltin Info)

benchmarkEnv :: BuiltinEnv ExecRuntime CEKSmallStep CoreBuiltin Info
benchmarkEnv = coreBuiltinEnv @ExecRuntime @CEKSmallStep

benchmarkBigStepEnv :: BuiltinEnv ExecRuntime CEKBigStep CoreBuiltin Info
benchmarkBigStepEnv = coreBuiltinEnv @ExecRuntime @CEKBigStep

newtype NoNF a
  = NoNf a
  deriving (Eq, Show)

instance NFData (NoNF a) where
  rnf _ = ()

gmSigs :: Map PublicKeyText (S.Set (CapToken QualifiedName PactValue))
gmSigs = M.fromList
  [(gmPublicKeyText1, mempty)]

defaultGasEvalEnv :: PactDb CoreBuiltin i -> IO (EvalEnv CoreBuiltin i)
defaultGasEvalEnv pdb = do
  ee <- defaultEvalEnv pdb coreBuiltinMap
  pure $ set eeMsgSigs gmSigs $ ee

defaultGasEvalState :: EvalState CoreBuiltin Info
defaultGasEvalState =
  EvalState
  {_esStack=[]
  , _esLoaded=gmLoaded
  , _esEvents=[]
  , _esDefPactExec=Nothing
  , _esCaps=capState
  , _esCheckRecursion = pure (RecursionCheck mempty)
  , _esTraceOutput = []
  }
  where
  capState =
    def{_csModuleAdmin = S.singleton gmModuleName}

gmModuleName :: ModuleName
gmModuleName = ModuleName "gasModel" Nothing

gmModuleHash :: ModuleHash
gmModuleHash = ModuleHash (pactHash "gasmodel")

gmKeysetName :: KeySetName
gmKeysetName = KeySetName "gasModelKeyset" Nothing

gasModelSampleSchema :: Schema
gasModelSampleSchema =  Schema (QualifiedName "gasModelSchema" gmModuleName) $ M.fromList
  [(Field "a", TyInt)
  ,(Field "b", TyString)
  ,(Field "c", TyBool)]

gasModelTable :: TableName
gasModelTable = TableName "gasmodelTable" gmModuleName

gasModelTable2 :: TableName
gasModelTable2 = TableName "gasmodelTable2" gmModuleName

gasModelTableValue :: TableValue
gasModelTableValue =
  TableValue gasModelTable gmModuleHash gasModelSampleSchema

gasModelTableValue2 :: TableValue
gasModelTableValue2 =
  TableValue gasModelTable2 gmModuleHash gasModelSampleSchema

gmTableK1 :: RowKey
gmTableK1 = RowKey "k1"

gmTableV1 :: RowData
gmTableV1 = RowData $ M.fromList
  [ (Field "a", PInteger 1)
  , (Field "b", PString "value1")
  , (Field "c", PBool True)]

gmTableK2 :: RowKey
gmTableK2 = RowKey "k2"

gmTableV2 :: RowData
gmTableV2 = RowData $ M.fromList
  [ (Field "a", PInteger 2)
  , (Field "b", PString "value2")
  , (Field "c", PBool True)]

gmTableK3 :: RowKey
gmTableK3 = RowKey "k3"

gmTableV3 :: RowData
gmTableV3 = RowData $ M.fromList
  [ (Field "a", PInteger 3)
  , (Field "b", PString "value3")
  , (Field "c", PBool True)]

mkGasModelFqn :: Text -> FullyQualifiedName
mkGasModelFqn t = FullyQualifiedName gmModuleName t gmModuleHash

gmPublicKeyText1 :: PublicKeyText
gmPublicKeyText1 = PublicKeyText "jose"

gmKeyset :: KeySet
gmKeyset = KeySet (S.fromList [gmPublicKeyText1]) KeysAll

gmTableGuard :: Guard QualifiedName PactValue
gmTableGuard = GKeyset gmKeyset

gmNamespaceName :: NamespaceName
gmNamespace :: Namespace
gmNamespaceName = NamespaceName "gasmodel"
gmNamespace = Namespace gmNamespaceName gmTableGuard gmTableGuard

gmDcapUnmanagedName :: Text
gmDcapUnmanagedName = "gasModelCapUnmanaged"

gmDcapAutoManagedName :: Text
gmDcapAutoManagedName = "gasModelCapAutoManaged"

gmDcapManagedName :: Text
gmDcapManagedName = "gasModelCapManaged"

gmManagerDfunName :: Text
gmManagerDfunName = "gasModelDfunManager"

gmDcapEventName :: Text
gmDcapEventName = "gasModelDCapEvent"

gmDcapUnmanaged :: EvalDefCap CoreBuiltin Info
gmDcapUnmanaged = DefCap
  { _dcapTerm = boolConst True
  , _dcapSpec = Arg gmDcapUnmanagedName Nothing def
  , _dcapMeta=DefEvent
  , _dcapInfo= def
  , _dcapArgs=[]
  }

gmDcapAutomanaged :: EvalDefCap CoreBuiltin Info
gmDcapAutomanaged = DefCap
  { _dcapTerm = boolConst True
  , _dcapSpec = Arg gmDcapAutoManagedName Nothing def
  , _dcapMeta= DefManaged AutoManagedMeta
  , _dcapInfo= def
  , _dcapArgs=[]}

gmManagerDfun :: EvalDefun CoreBuiltin Info
gmManagerDfun =
  Defun
  { _dfunTerm = intConst 1
  , _dfunSpec = Arg gmManagerDfunName Nothing def
  , _dfunInfo=def
  , _dfunArgs=[Arg "arg1" Nothing def, Arg "arg2" Nothing def]
  }

gmDcapManaged :: EvalDefCap CoreBuiltin Info
gmDcapManaged = DefCap
  { _dcapTerm = boolConst True
  , _dcapSpec = Arg gmDcapAutoManagedName Nothing def
  , _dcapMeta= DefManaged (DefManagedMeta (0, "arg1") (FQName (mkGasModelFqn gmManagerDfunName)))
  , _dcapInfo=def
  , _dcapArgs=[Arg "arg1" Nothing def]
  }

gmModuleDefns :: [EvalDef CoreBuiltin Info]
gmModuleDefns =
  [ DCap gmDcapManaged
  , DCap gmDcapAutomanaged
  , DCap gmDcapUnmanaged
  , Dfun gmManagerDfun]


gmModule :: EvalModule CoreBuiltin Info
gmModule = Module
  { _mName= gmModuleName
  , _mInfo=def
  , _mImports=[]
  , _mImplements=[]
  , _mHash=gmModuleHash
  , _mGovernance=KeyGov gmKeysetName
  , _mDefs=gmModuleDefns
  , _mTxHash = Hash mempty
  , _mBlessed=mempty}

gmModuleData :: ModuleData CoreBuiltin Info
gmModuleData = ModuleData gmModule mempty

gmFqMap :: Map FullyQualifiedName (EvalDef CoreBuiltin Info)
gmFqMap = M.fromList $ toFqDep gmModuleName gmModuleHash <$> gmModuleDefns

gmLoaded :: Loaded CoreBuiltin Info
gmLoaded = Loaded
  {_loToplevel=mempty
  , _loNamespace=Nothing
  , _loModules=M.singleton gmModuleName gmModuleData
  , _loAllLoaded=gmFqMap}

prepopulateDb :: PactDb CoreBuiltin Info -> GasM CoreBuiltin Info ()
prepopulateDb pdb = do
  _ <- liftIO $ _pdbBeginTx pdb Transactional
  _pdbCreateUserTable pdb gasModelTable
  _pdbWrite pdb Write (DUserTables gasModelTable) gmTableK1 gmTableV1
  _pdbWrite pdb Write (DUserTables gasModelTable) gmTableK1 gmTableV1
  _pdbWrite pdb Write DNamespaces gmNamespaceName gmNamespace
  _pdbWrite pdb Write DKeySets gmKeysetName gmKeyset
  _ <- liftIO $ _pdbCommitTx pdb
  pure ()

evaluateN
  :: EvalEnv CoreBuiltin Info
  -> EvalState CoreBuiltin Info
  -> Text
  -> Int
  -> IO (Either (PactError Info) MachineResult, EvalState CoreBuiltin Info)
evaluateN evalEnv es source nSteps = runEvalM (ExecEnv evalEnv) es $ do
  term <- compileTerm source
  let pdb = _eePactDb evalEnv
      ps = _eeDefPactStep evalEnv
      env = CEKEnv { _cePactDb=pdb
                   , _ceLocal=mempty
                   , _ceInCap=False
                   , _ceDefPactStep=ps
                   , _ceBuiltins = benchmarkEnv }
  step1 <- Eval.evaluateTermSmallStep Mt CEKNoHandler env term
  evalNSteps (nSteps - 1) step1

isFinal :: MachineResult -> Bool
isFinal (CEKReturn Mt CEKNoHandler _) = True
isFinal _ = False

evalStep :: MachineResult -> Eval MachineResult
evalStep c@(CEKReturn cont handler result)
  | isFinal c = return c
  | otherwise = Eval.returnCEK cont handler result
evalStep (CEKEvaluateTerm cont handler cekEnv term) = Eval.evaluateTermSmallStep cont handler cekEnv term

unsafeEvalStep :: MachineResult -> Eval MachineResult
unsafeEvalStep (CEKReturn cont handler result) = Eval.returnCEK cont handler result
unsafeEvalStep (CEKEvaluateTerm cont handler cekEnv term) = Eval.evaluateTermSmallStep cont handler cekEnv term

evalNSteps :: Int -> MachineResult -> Eval MachineResult
evalNSteps i c
  | i <= 0 = return c
  | otherwise = evalStep c >>= evalNSteps (i - 1)

compileTerm
  :: Text
  -> Eval (CoreTerm Info)
compileTerm source = do
  parsed <- liftEither $ compileOnlyTerm (RawCode source)
  DesugarOutput term _  <- runDesugarTerm parsed
  pure term

type BenchEvalEnv = EvalEnv CoreBuiltin Info
type BenchEvalState = EvalState CoreBuiltin Info

runCompileTerm
  :: BenchEvalEnv
  -> BenchEvalState
  -> Text
  -> IO (Either (PactError Info) (CoreTerm Info), EvalState CoreBuiltin Info)
runCompileTerm ee es = runEvalM (ExecEnv ee) es . compileTerm

runNativeBenchmark'
  :: (BenchEvalEnv -> IO BenchEvalEnv)
  -> (BenchEvalState -> IO BenchEvalState)
  -> PactDb CoreBuiltin Info
  -> String
  -> Text
  -> C.Benchmark
runNativeBenchmark' envMod stMod pdb title src = C.env mkEnv $ \ ~(term, es, ee) ->
  C.bench title $ C.nfAppIO (fmap (ensureNonError . fst) . runEvalM (ExecEnv ee) es . Eval.eval PImpure benchmarkBigStepEnv) term
  where
  ensureNonError = either (error . show) id
  mkEnv = do
    ee <- envMod =<< defaultGasEvalEnv pdb
    es <- stMod defaultGasEvalState
    (Right term, es') <- runCompileTerm ee es src
    pure (term, es', ee)

runNativeBenchmark
  :: PactDb CoreBuiltin Info
  -> String
  -> Text
  -> C.Benchmark
runNativeBenchmark = runNativeBenchmark' pure pure

withLoaded :: [(Text, PactValue)] -> (BenchEvalState -> BenchEvalState)
withLoaded envVars = esLoaded .~ synthLoaded
  where
  synthLoaded = Loaded
    { _loModules = mempty
    , _loToplevel = M.fromList [ (n, (mkGasModelFqn n, DKDefConst)) | n <- fst <$> envVars ]
    , _loNamespace = Nothing
    , _loAllLoaded = M.fromList [ (mkGasModelFqn n, DConst $ DefConst (Arg n Nothing def) (EvaledConst v) def) | (n, v) <- envVars ]
    }

runNativeBenchmarkPrepared
  :: [(Text, PactValue)]
  -> PactDb CoreBuiltin Info
  -> String
  -> Text
  -> C.Benchmark
runNativeBenchmarkPrepared envVars = runNativeBenchmark' pure (pure . withLoaded envVars)

type EnvMod = Endo BenchEvalEnv

msgBody :: Map Field PactValue -> EnvMod
msgBody body = Endo $ eeMsgBody .~ PObject body

msgSigsNoCap :: [PublicKeyText] -> EnvMod
msgSigsNoCap pkts = Endo $ eeMsgSigs .~ M.fromList ((, mempty) <$> pkts)

runNativeBenchmarkPreparedEnvMod
  :: EnvMod
  -> [(Text, PactValue)]
  -> PactDb CoreBuiltin Info
  -> String
  -> Text
  -> C.Benchmark
runNativeBenchmarkPreparedEnvMod (Endo envMod) envVars = runNativeBenchmark' (pure . envMod) (pure . withLoaded envVars)

type StMod = Endo BenchEvalState

stCaps :: [CapToken QualifiedName PactValue] -> StMod
stCaps capToks = Endo $ esCaps.csSlots .~ ((`CapSlot` []) <$> capToks)

stManaged :: [ManagedCap QualifiedName PactValue] -> StMod
stManaged manageds = Endo $ esCaps.csManaged .~ S.fromList manageds

stAddDef :: Text -> Def Name Type CoreBuiltin Info -> StMod
stAddDef name dfn = Endo $ (esLoaded.loToplevel %~ M.insert name (fqn, defKind gmModuleName dfn))
                         . (esLoaded.loAllLoaded %~ M.insert fqn dfn)
  where
  fqn = mkGasModelFqn name

stStack :: [StackFrame Info] -> StMod
stStack s = Endo $ esStack .~ s

stModAdmin :: [ModuleName] -> StMod
stModAdmin names = Endo $ esCaps.csModuleAdmin .~ S.fromList names

runNativeBenchmarkPreparedStMod
  :: StMod
  -> [(Text, PactValue)]
  -> PactDb CoreBuiltin Info
  -> String
  -> Text
  -> C.Benchmark
runNativeBenchmarkPreparedStMod (Endo stMod) envVars = runNativeBenchmark' pure (pure . stMod . withLoaded envVars)


dummyTx :: PactDb b i -> IO () -> [C.Benchmark] -> [C.Benchmark]
dummyTx pdb initDbState bs = C.envWithCleanup (_pdbBeginTx pdb Transactional >> initDbState) (const $ _pdbRollbackTx pdb) . const <$> bs

ignoreWrites :: PactDb b i -> PactDb b i
ignoreWrites pdb = pdb { _pdbWrite = \_ _ _ _ -> pure () }

-- Closures
unitClosureNullary :: CEKEnv ExecRuntime step CoreBuiltin Info -> Closure ExecRuntime step CoreBuiltin Info
unitClosureNullary env
  = Closure
  { _cloFqName = FullyQualifiedName (ModuleName "foomodule" Nothing) "foo" placeholderHash
  , _cloTypes = NullaryClosure
  , _cloArity = 0
  , _cloTerm = unitConst
  , _cloRType = Nothing
  , _cloEnv = env
  , _cloInfo = def}


unitClosureUnary :: CEKEnv ExecRuntime step CoreBuiltin Info -> Closure ExecRuntime step CoreBuiltin Info
unitClosureUnary env
  = Closure
  { _cloFqName = FullyQualifiedName (ModuleName "foomodule" Nothing) "foo" placeholderHash
  , _cloTypes = ArgClosure (NE.fromList [Arg "fooCloArg" Nothing def])
  , _cloArity = 1
  , _cloTerm = unitConst
  , _cloRType = Nothing
  , _cloEnv = env
  , _cloInfo = def}

unitClosureBinary :: CEKEnv ExecRuntime step CoreBuiltin Info -> Closure ExecRuntime step CoreBuiltin Info
unitClosureBinary env
  = Closure
  { _cloFqName = FullyQualifiedName (ModuleName "foomodule" Nothing) "foo" placeholderHash
  , _cloTypes = ArgClosure (NE.fromList [Arg "fooCloArg1" Nothing def, Arg "fooCloArg2" Nothing def])
  , _cloArity = 2
  , _cloTerm = unitConst
  , _cloRType = Nothing
  , _cloEnv = env
  , _cloInfo = def}


boolClosureUnary :: Bool -> CEKEnv e step b Info -> Closure e step b Info
boolClosureUnary b env
  = Closure
  { _cloFqName = FullyQualifiedName (ModuleName "foomodule" Nothing) "foo" placeholderHash
  , _cloTypes = ArgClosure (NE.fromList [Arg "fooCloArg1" Nothing def])
  , _cloArity = 1
  , _cloTerm = boolConst b
  , _cloRType = Nothing
  , _cloEnv = env
  , _cloInfo = def}

boolClosureBinary :: Bool -> CEKEnv e step b Info -> Closure e step b Info
boolClosureBinary b env
  = Closure
  { _cloFqName = FullyQualifiedName (ModuleName "foomodule" Nothing) "foo" placeholderHash
  , _cloTypes = ArgClosure (NE.fromList [Arg "fooCloArg1" Nothing def, Arg "fooCloArg2" Nothing def])
  , _cloArity = 2
  , _cloTerm = boolConst b
  , _cloRType = Nothing
  , _cloEnv = env
  , _cloInfo = def}

intClosureBinary :: Integer -> CEKEnv e step b Info -> Closure e step b Info
intClosureBinary b env
  = Closure
  { _cloFqName = FullyQualifiedName (ModuleName "foomodule" Nothing) "foo" placeholderHash
  , _cloTypes = ArgClosure (NE.fromList [Arg "fooCloArg1" Nothing def, Arg "fooCloArg2" Nothing def])
  , _cloArity = 2
  , _cloTerm = intConst b
  , _cloRType = Nothing
  , _cloEnv = env
  , _cloInfo = def}


unitConst :: CoreTerm Info
unitConst = Constant LUnit def

boolConst :: Bool -> Term name ty builtin Info
boolConst b = Constant (LBool b) def

strConst :: Text -> Term name ty builtin Info
strConst b = Constant (LString b) def

intConst :: Integer -> Term name ty builtin Info
intConst b = Constant (LInteger b) def
