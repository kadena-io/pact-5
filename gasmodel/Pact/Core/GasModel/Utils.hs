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
import Pact.Core.Gas
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
import Pact.Core.Evaluate
import Pact.Core.Namespace
import Pact.Core.IR.Eval.CEK.Types hiding (Eval)
import qualified Pact.Core.IR.Eval.CEK as Eval

type CoreDb = PactDb CoreBuiltin ()
type MachineResult = CEKReturn ExecRuntime CoreBuiltin ()
type ApplyContToVEnv =
  ( EvalEnv CoreBuiltin ()
  , EvalState CoreBuiltin ()
  , Cont ExecRuntime CEKSmallStep CoreBuiltin ()
  , CEKErrorHandler ExecRuntime CEKSmallStep CoreBuiltin ()
  , CEKValue ExecRuntime CEKSmallStep CoreBuiltin ())

benchmarkEnv :: BuiltinEnv ExecRuntime CEKSmallStep CoreBuiltin ()
benchmarkEnv = coreBuiltinEnv @ExecRuntime @CEKSmallStep

benchmarkBigStepEnv :: BuiltinEnv ExecRuntime CEKBigStep CoreBuiltin ()
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

defaultGasEvalState :: EvalState CoreBuiltin ()
defaultGasEvalState =
  EvalState
  {_esStack=[]
  , _esLoaded=gmLoaded
  , _esEvents=[]
  , _esDefPactExec=Nothing
  , _esCaps=capState
  , _esGasLog=Nothing
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

gmDcapUnmanaged :: EvalDefCap CoreBuiltin ()
gmDcapUnmanaged = DefCap
  { _dcapTerm = boolConst True
  , _dcapSpec = Arg gmDcapUnmanagedName Nothing ()
  , _dcapMeta=DefEvent
  , _dcapInfo=()
  , _dcapArgs=[]
  }

gmDcapAutomanaged :: EvalDefCap CoreBuiltin ()
gmDcapAutomanaged = DefCap
  { _dcapTerm = boolConst True
  , _dcapSpec = Arg gmDcapAutoManagedName Nothing ()
  , _dcapMeta= DefManaged AutoManagedMeta
  , _dcapInfo=()
  , _dcapArgs=[]}

gmManagerDfun :: EvalDefun CoreBuiltin ()
gmManagerDfun =
  Defun
  { _dfunTerm = intConst 1
  , _dfunSpec = Arg gmManagerDfunName Nothing ()
  , _dfunInfo=()
  , _dfunArgs=[Arg "arg1" Nothing (), Arg "arg2" Nothing ()]
  }

gmDcapManaged :: EvalDefCap CoreBuiltin ()
gmDcapManaged = DefCap
  { _dcapTerm = boolConst True
  , _dcapSpec = Arg gmDcapAutoManagedName Nothing ()
  , _dcapMeta= DefManaged (DefManagedMeta (0, "arg1") (FQName (mkGasModelFqn gmManagerDfunName)))
  , _dcapInfo=()
  , _dcapArgs=[Arg "arg1" Nothing ()]
  }

gmModuleDefns :: [EvalDef CoreBuiltin ()]
gmModuleDefns =
  [ DCap gmDcapManaged
  , DCap gmDcapAutomanaged
  , DCap gmDcapUnmanaged
  , Dfun gmManagerDfun]


gmModule :: EvalModule CoreBuiltin ()
gmModule = Module
  { _mName= gmModuleName
  , _mInfo=()
  , _mImports=[]
  , _mImplements=[]
  , _mHash=gmModuleHash
  , _mGovernance=KeyGov gmKeysetName
  , _mDefs=gmModuleDefns
  , _mBlessed=mempty}

gmModuleData :: ModuleData CoreBuiltin ()
gmModuleData = ModuleData gmModule mempty

gmFqMap :: Map FullyQualifiedName (EvalDef CoreBuiltin ())
gmFqMap = M.fromList $ toFqDep gmModuleName gmModuleHash <$> gmModuleDefns

gmLoaded :: Loaded CoreBuiltin ()
gmLoaded = Loaded
  {_loToplevel=mempty
  , _loNamespace=Nothing
  , _loModules=M.singleton gmModuleName gmModuleData
  , _loAllLoaded=gmFqMap}

prepopulateDb :: PactDb CoreBuiltin i -> GasM (PactError i) CoreBuiltin ()
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
  :: EvalEnv CoreBuiltin ()
  -> EvalState CoreBuiltin ()
  -> Text
  -> Int
  -> IO (Either (PactError ()) MachineResult, EvalState CoreBuiltin ())
evaluateN evalEnv es source nSteps = runEvalM (ExecEnv evalEnv) es $ do
  term <- compileTerm source
  let pdb = _eePactDb evalEnv
      ps = _eeDefPactStep evalEnv
      env = CEKEnv { _cePactDb=pdb
                   , _ceLocal=mempty
                   , _ceInCap=False
                   , _ceDefPactStep=ps
                   , _ceBuiltins= benchmarkEnv }
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
  -> Eval CoreTerm
compileTerm source = do
  parsed <- liftEither $ compileOnlyTerm (RawCode source)
  DesugarOutput term _  <- runDesugarTerm parsed
  pure term

type BenchEvalEnv = EvalEnv CoreBuiltin ()
type BenchEvalState = EvalState CoreBuiltin ()

runCompileTerm
  :: BenchEvalEnv
  -> BenchEvalState
  -> Text
  -> IO (Either (PactError ()) CoreTerm, EvalState CoreBuiltin ())
runCompileTerm ee es = runEvalM (ExecEnv ee) es . compileTerm

runNativeBenchmark'
  :: (BenchEvalEnv -> IO BenchEvalEnv)
  -> (BenchEvalState -> IO BenchEvalState)
  -> PactDb CoreBuiltin ()
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
  :: PactDb CoreBuiltin ()
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
    , _loAllLoaded = M.fromList [ (mkGasModelFqn n, DConst $ DefConst (Arg n Nothing ()) (EvaledConst v) ()) | (n, v) <- envVars ]
    }

runNativeBenchmarkPrepared
  :: [(Text, PactValue)]
  -> PactDb CoreBuiltin ()
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
  -> PactDb CoreBuiltin ()
  -> String
  -> Text
  -> C.Benchmark
runNativeBenchmarkPreparedEnvMod (Endo envMod) envVars = runNativeBenchmark' (pure . envMod) (pure . withLoaded envVars)

type StMod = Endo BenchEvalState

stCaps :: [CapToken QualifiedName PactValue] -> StMod
stCaps capToks = Endo $ esCaps.csSlots .~ ((`CapSlot` []) <$> capToks)

stManaged :: [ManagedCap QualifiedName PactValue] -> StMod
stManaged manageds = Endo $ esCaps.csManaged .~ S.fromList manageds

stAddDef :: Text -> Def Name Type CoreBuiltin () -> StMod
stAddDef name dfn = Endo $ (esLoaded.loToplevel %~ M.insert name (fqn, defKind gmModuleName dfn))
                         . (esLoaded.loAllLoaded %~ M.insert fqn dfn)
  where
  fqn = mkGasModelFqn name

stStack :: [StackFrame ()] -> StMod
stStack s = Endo $ esStack .~ s

stModAdmin :: [ModuleName] -> StMod
stModAdmin names = Endo $ esCaps.csModuleAdmin .~ S.fromList names

runNativeBenchmarkPreparedStMod
  :: StMod
  -> [(Text, PactValue)]
  -> PactDb CoreBuiltin ()
  -> String
  -> Text
  -> C.Benchmark
runNativeBenchmarkPreparedStMod (Endo stMod) envVars = runNativeBenchmark' pure (pure . stMod . withLoaded envVars)


dummyTx :: PactDb b i -> IO () -> [C.Benchmark] -> [C.Benchmark]
dummyTx pdb initDbState bs = C.envWithCleanup (_pdbBeginTx pdb Transactional >> initDbState) (const $ _pdbRollbackTx pdb) . const <$> bs

ignoreWrites :: PactDb b i -> PactDb b i
ignoreWrites pdb = pdb { _pdbWrite = \_ _ _ _ -> pure () }

-- Closures
unitClosureNullary :: CEKEnv ExecRuntime step CoreBuiltin () -> Closure ExecRuntime step CoreBuiltin ()
unitClosureNullary env
  = Closure
  { _cloFqName = FullyQualifiedName (ModuleName "foomodule" Nothing) "foo" placeholderHash
  , _cloTypes = NullaryClosure
  , _cloArity = 0
  , _cloTerm = unitConst
  , _cloRType = Nothing
  , _cloEnv = env
  , _cloInfo = ()}


unitClosureUnary :: CEKEnv ExecRuntime step CoreBuiltin () -> Closure ExecRuntime step CoreBuiltin ()
unitClosureUnary env
  = Closure
  { _cloFqName = FullyQualifiedName (ModuleName "foomodule" Nothing) "foo" placeholderHash
  , _cloTypes = ArgClosure (NE.fromList [Arg "fooCloArg" Nothing ()])
  , _cloArity = 1
  , _cloTerm = unitConst
  , _cloRType = Nothing
  , _cloEnv = env
  , _cloInfo = ()}

unitClosureBinary :: CEKEnv ExecRuntime step CoreBuiltin () -> Closure ExecRuntime step CoreBuiltin ()
unitClosureBinary env
  = Closure
  { _cloFqName = FullyQualifiedName (ModuleName "foomodule" Nothing) "foo" placeholderHash
  , _cloTypes = ArgClosure (NE.fromList [Arg "fooCloArg1" Nothing (), Arg "fooCloArg2" Nothing ()])
  , _cloArity = 2
  , _cloTerm = unitConst
  , _cloRType = Nothing
  , _cloEnv = env
  , _cloInfo = ()}


boolClosureUnary :: Bool -> CEKEnv e step b () -> Closure e step b ()
boolClosureUnary b env
  = Closure
  { _cloFqName = FullyQualifiedName (ModuleName "foomodule" Nothing) "foo" placeholderHash
  , _cloTypes = ArgClosure (NE.fromList [Arg "fooCloArg1" Nothing ()])
  , _cloArity = 1
  , _cloTerm = boolConst b
  , _cloRType = Nothing
  , _cloEnv = env
  , _cloInfo = ()}

boolClosureBinary :: Bool -> CEKEnv e step b () -> Closure e step b ()
boolClosureBinary b env
  = Closure
  { _cloFqName = FullyQualifiedName (ModuleName "foomodule" Nothing) "foo" placeholderHash
  , _cloTypes = ArgClosure (NE.fromList [Arg "fooCloArg1" Nothing (), Arg "fooCloArg2" Nothing ()])
  , _cloArity = 2
  , _cloTerm = boolConst b
  , _cloRType = Nothing
  , _cloEnv = env
  , _cloInfo = ()}

intClosureBinary :: Integer -> CEKEnv e step b () -> Closure e step b ()
intClosureBinary b env
  = Closure
  { _cloFqName = FullyQualifiedName (ModuleName "foomodule" Nothing) "foo" placeholderHash
  , _cloTypes = ArgClosure (NE.fromList [Arg "fooCloArg1" Nothing (), Arg "fooCloArg2" Nothing ()])
  , _cloArity = 2
  , _cloTerm = intConst b
  , _cloRType = Nothing
  , _cloEnv = env
  , _cloInfo = ()}


unitConst :: CoreTerm
unitConst = Constant LUnit ()

boolConst :: Bool -> Term name ty builtin ()
boolConst b = Constant (LBool b) ()

strConst :: Text -> Term name ty builtin ()
strConst b = Constant (LString b) ()

intConst :: Integer -> Term name ty builtin ()
intConst b = Constant (LInteger b) ()
