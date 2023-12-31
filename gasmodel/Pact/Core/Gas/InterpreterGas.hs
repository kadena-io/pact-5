{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Control.Lens
-- import Control.Monad
import Control.Monad.Except
-- import Control.Monad.IO.Class
import Data.Default
import Data.Text (Text)
-- import Data.Functor(void)
-- import Data.Bifunctor(bimap)
-- import Criterion.Types(Report)
import qualified Data.RAList as RA
import qualified Data.List.NonEmpty as NE
import qualified Criterion as C
import qualified Criterion.Main as C
-- import qualified Criterion.Report as C
-- import qualified Criterion.Analysis as C
import qualified Data.Text as T
import qualified Data.Map.Strict as M

import Pact.Core.Builtin
import Pact.Core.Environment
import Pact.Core.Errors
import Pact.Core.Names
import Pact.Core.Gas
import Pact.Core.Literal
import Pact.Core.Type
import Pact.Core.Capabilities
import Pact.Core.IR.Desugar
import Pact.Core.IR.Eval.Runtime
import Pact.Core.IR.Eval.RawBuiltin
import Pact.Core.PactValue
import Pact.Core.IR.Term
import Pact.Core.Persistence
import Pact.Core.Hash
import Pact.Core.Persistence.SQLite
import Pact.Core.Serialise (serialisePact)
import Pact.Core.Evaluate(compileOnlyTerm, RawCode(..))
import qualified Pact.Core.IR.Eval.CEK as Eval

type CoreDb = PactDb RawBuiltin ()
type MachineResult = CEKReturn RawBuiltin () Eval

unitConst :: CoreTerm
unitConst = Constant LUnit ()

benchmarkEnv :: BuiltinEnv CEKSmallStep RawBuiltin () Eval
benchmarkEnv = rawBuiltinEnv @CEKSmallStep

benchmarkBigStepEnv :: BuiltinEnv CEKBigStep RawBuiltin () Eval
benchmarkBigStepEnv = rawBuiltinEnv @CEKBigStep

compileTerm
  :: Text
  -> Eval CoreTerm
compileTerm source = do
  parsed <- liftEither $ compileOnlyTerm (RawCode source)
  DesugarOutput term _  <- runDesugarTerm parsed
  pure term

runCompileTerm
  :: EvalEnv RawBuiltin ()
  -> EvalState RawBuiltin ()
  -> Text
  -> IO (Either (PactError ()) CoreTerm, EvalState RawBuiltin ())
runCompileTerm es ee = runEvalM es ee . compileTerm

evaluateN
  :: EvalEnv RawBuiltin ()
  -> EvalState RawBuiltin ()
  -> Text
  -> Int
  -> IO (Either (PactError ()) MachineResult, EvalState RawBuiltin ())
evaluateN evalEnv es source nSteps = runEvalM evalEnv es $ do
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


testGasFromMachineResult :: MachineResult -> EvalEnv RawBuiltin () -> EvalState RawBuiltin () -> C.Benchmark
testGasFromMachineResult machineResult es ee = do
  C.env (pure (machineResult,es,ee)) $ \ ~(ms, es', ee') ->
    C.bench "TODO:title" $ C.nfAppIO (runEvalM es' ee' . unsafeEvalStep) ms

gasVarBound :: Int -> EvalEnv RawBuiltin () -> EvalState RawBuiltin () -> C.Benchmark
gasVarBound n ee es = do
  let term = Var (Name "_" (NBound (fromIntegral (n-1)))) ()
  let pdb = _eePactDb ee
      ps = _eeDefPactStep ee
      env = CEKEnv { _cePactDb=pdb
                  , _ceLocal = RA.fromList (replicate n VUnit)
                  , _ceInCap=False
                  , _ceDefPactStep=ps
                  , _ceBuiltins= benchmarkEnv }
  let title = "Var: " <> show n <> "th var case"
  C.env (pure (term, es, ee, env)) $ \ ~(term', es', ee', env') -> do
    C.bench title $ C.nfAppIO (runEvalM ee' es' . Eval.evaluateTermSmallStep Mt CEKNoHandler env') term'

varGas :: CoreDb -> C.Benchmark
varGas pdb = do
  ee <- defaultEvalEnv pdb rawBuiltinMap
      ees = def
  C.bgroup "Variables: bound" $ (\i -> gasVarBound i ee ee) <$> [1..5]

simpleTermGas :: CoreTerm -> String -> CoreDb -> C.Benchmark
simpleTermGas term title pdb = do
  ee <- defaultEvalEnv pdb rawBuiltinMap
  let es = def
      ps = _eeDefPactStep ee
      env = CEKEnv { _cePactDb=pdb
                   , _ceLocal=mempty
                   , _ceInCap=False
                   , _ceDefPactStep=ps
                   , _ceBuiltins=benchmarkEnv }
  C.env (pure (term, es, ee, env)) $ \ ~(term', es', ee', env') -> do
    C.bench title $ C.nfAppIO (runEvalM ee' es' . Eval.evaluateTermSmallStep Mt CEKNoHandler env') term'

-- Constant gas simply wraps the result in VLiteral
constantGas :: CoreDb -> C.Benchmark
constantGas = simpleTermGas unitConst "Constant Node"

-- App simply enriches the continuation and continues eval
appGas :: CoreDb -> C.Benchmark
appGas = simpleTermGas (App unitConst [] ()) "App Node"

nullaryGas :: CoreDb -> C.Benchmark
nullaryGas = simpleTermGas (Nullary unitConst ()) "Nullary Node"

letGas :: CoreDb ->  C.Benchmark
letGas =
  let letBind = Let (Arg "_" Nothing) unitConst unitConst ()
  in simpleTermGas letBind "Let Node"

constantExample :: CoreTerm -> CEKValue CEKSmallStep RawBuiltin () Eval
constantExample (Constant LUnit ()) = VPactValue (PLiteral LUnit)
constantExample _ = error "boom"

constantGasEquiv :: C.Benchmark
constantGasEquiv = do
  let term = (Constant LUnit ()) :: CoreTerm
  C.env (pure term) $ \ ~(c) ->
    C.bench "constant example" $ C.nf constantExample c

-- Simple case for evaluating to normal form for (+ 1 2)
plusOneTwo :: CoreDb -> C.Benchmark
plusOneTwo pdb = do
  ee <- defaultEvalEnv pdb rawBuiltinMap
  let es = def
      ps = _eeDefPactStep ee
      env = CEKEnv { _cePactDb=pdb
                   , _ceLocal=mempty
                   , _ceInCap=False
                   , _ceDefPactStep=ps
                   , _ceBuiltins=benchmarkEnv }
  let term = App (Builtin RawAdd ()) [Constant (LInteger 1) (), Constant (LInteger 2) ()] ()
  C.env (pure (term, es, ee, env)) $ \ ~(term', es', ee', env') -> do
    C.bench "(+ 1 2)" $ C.nfAppIO (runEvalM ee' es' . Eval.evalNormalForm env') term'

-- Gas for a lambda with N arguments
gasLamNArgs :: Int -> EvalEnv RawBuiltin () -> EvalState RawBuiltin () -> C.Benchmark
gasLamNArgs n ee es = do
  let mkArg i = Arg ("Arg#" <> T.pack (show i)) Nothing
      args = mkArg <$> [1..n]
      term = Lam (NE.fromList args) (Constant LUnit ()) ()
      pdb = _eePactDb ee
      ps = _eeDefPactStep ee
      env = CEKEnv { _cePactDb=pdb
                  , _ceLocal = RA.fromList mempty
                  , _ceInCap=False
                  , _ceDefPactStep=ps
                  , _ceBuiltins= benchmarkEnv }
  let title = "Lam: " <> show n <> " args case"
  C.env (pure (term, es, ee, env)) $ \ ~(term', es', ee', env') -> do
    C.bench title $ C.nfAppIO (runEvalM ee' es' . Eval.evaluateTermSmallStep Mt CEKNoHandler env') term'

lamGas :: CoreDb -> C.Benchmark
lamGas pdb = do
  ee <- defaultEvalEnv pdb rawBuiltinMap
  let es = def
  C.bgroup "Lambda Node" $ [ gasLamNArgs i ee es | i <- [1..25]]

seqGas :: CoreDb -> C.Benchmark
seqGas = simpleTermGas (Sequence unitConst unitConst ()) "Seq Node"

condCAndGas :: CoreDb -> C.Benchmark
condCAndGas = simpleTermGas (Conditional (CAnd unitConst unitConst) ()) "Conditional CAnd Node"

condCOrGas :: CoreDb -> C.Benchmark
condCOrGas = simpleTermGas (Conditional (COr unitConst unitConst) ()) "Conditional If Node"

condCIfGas :: CoreDb -> C.Benchmark
condCIfGas = simpleTermGas (Conditional (CIf unitConst unitConst unitConst) ()) "Conditional CIf Node"

condCEnforceOneGas :: CoreDb -> C.Benchmark
condCEnforceOneGas pdb =
  C.bgroup "CondCEnforceOne" $
    [ simpleTermGas (Conditional (CEnforceOne unitConst []) ()) "Conditional CEnforceOne []" pdb
    , simpleTermGas (Conditional (CEnforceOne unitConst [unitConst]) ()) "Conditional CEnforceOne [x]" pdb
    , simpleTermGas (Conditional (CEnforceOne unitConst [unitConst, unitConst]) ()) "Conditional CEnforceOne [x,x]" pdb ]

condCEnforceGas :: CoreDb -> C.Benchmark
condCEnforceGas = simpleTermGas (Conditional (CEnforce unitConst unitConst) ()) "Conditional CIf Node"

builtinNodeGas :: CoreDb -> C.Benchmark
builtinNodeGas = simpleTermGas (Builtin RawAt ()) "Builtin node"

listLitGas :: CoreDb -> C.Benchmark
listLitGas pdb =
  C.bgroup "ListLit" $
    [ simpleTermGas (ListLit [] ()) "[]" pdb
    , simpleTermGas (ListLit [unitConst] ()) "[x]" pdb
    , simpleTermGas (ListLit [unitConst, unitConst] ()) "[x,x]" pdb ]

tryGas :: CoreDb -> C.Benchmark
tryGas =
  simpleTermGas (Try unitConst unitConst ()) "Try Node"

objectLitGas :: CoreDb -> C.Benchmark
objectLitGas pdb =
  C.bgroup "ObjectLit" $
    [ simpleTermGas (ObjectLit [] ()) "{}" pdb
    , simpleTermGas (ObjectLit [(Field "x", unitConst)] ()) "{x:()}" pdb
    , simpleTermGas (ObjectLit [(Field "x", unitConst), (Field "y", unitConst)] ()) "{x:(), y:()}" pdb ]

termGas :: CoreDb -> [C.Benchmark]
termGas pdb = benchmarkNodeType pdb <$> [minBound .. maxBound]

withCapFormGas :: CoreDb -> C.Benchmark
withCapFormGas =
  simpleTermGas (CapabilityForm (WithCapability unitConst unitConst) ()) "Capability node"


createUserGuardGasNArgs :: Int -> CoreDb -> C.Benchmark
createUserGuardGasNArgs nArgs pdb = do
  let args =  [ Arg ("_foo" <> T.pack (show i)) Nothing| i <- [2..nArgs] ]
  ee <- defaultEvalEnv pdb rawBuiltinMap
      mn = ModuleName "foomodule" Nothing
      mh = ModuleHash (pactHash "foo")
      fqn = FullyQualifiedName mn "foo" mh
      dfun = Defun "foo" args Nothing unitConst ()
      es = over (esLoaded . loAllLoaded) (M.insert fqn (Dfun dfun)) $ def
      name = Name "foo" (NTopLevel mn mh)
      term = CapabilityForm (CreateUserGuard name (replicate nArgs unitConst)) ()
      ps = _eeDefPactStep ee
      env = CEKEnv { _cePactDb=pdb
                   , _ceLocal=mempty
                   , _ceInCap=False
                   , _ceDefPactStep=ps
                   , _ceBuiltins=benchmarkEnv }
      title = "Create User Guard, " <> show nArgs <> " args"
  C.env (pure (term, es, ee, env)) $ \ ~(term', es', ee', env') -> do
    C.bench title $ C.nfAppIO (runEvalM ee' es' . Eval.evaluateTermSmallStep Mt CEKNoHandler env') term'

createUserGuardGas :: CoreDb -> C.Benchmark
createUserGuardGas pdb =
  C.bgroup "Create user guard node" [ createUserGuardGasNArgs i pdb | i <- [0..5]]

main :: IO ()
main = withSqlitePactDb serialisePact ":memory:" $ \pdb -> do
  C.defaultMain $ [C.bgroup "pact-core-term-gas" (termGas pdb)]

errorGas :: CoreDb -> C.Benchmark
errorGas = simpleTermGas (Error "foo" ()) "Error node"

benchmarkNodeType :: CoreDb -> NodeType -> C.Benchmark
benchmarkNodeType pdb = \case
  VarNode -> varGas pdb
  LamNode -> lamGas pdb
  LetNode -> letGas pdb
  AppNode -> appGas pdb
  SeqNode -> seqGas pdb
  NullaryNode -> nullaryGas pdb
  -- -- conditional nodes
  CondCAndNode -> condCAndGas pdb
  CondCOrNode -> condCOrGas pdb
  CondIfNode -> condCIfGas pdb
  CondEnforceOneNode -> condCEnforceOneGas pdb
  CondEnforceNode -> condCEnforceGas pdb
  --
  BuiltinNode -> builtinNodeGas pdb
  ConstantNode -> constantGas pdb
  ListNode -> listLitGas pdb
  TryNode -> tryGas pdb
  ObjectLitNode -> objectLitGas pdb
  CapFormWithCapNode -> withCapFormGas pdb
  CapFormCreateUGNode -> createUserGuardGas pdb
  ErrorNode -> errorGas pdb


-- Gas for a lambda with N
gasMtReturnNoHandler :: PactDb RawBuiltin () -> C.Benchmark
gasMtReturnNoHandler pdb = do
  ee <- defaultEvalEnv pdb rawBuiltinMap
  let es = def
      frame = Mt
      value = VUnit
      handler = CEKNoHandler
  C.env (pure (es, ee)) $ \ ~(es', ee') -> do
    C.bench "MtReturnNoHandler" $ C.nfAppIO (runEvalM ee' es' . Eval.applyContToValueSmallStep frame handler) value

-- Gas for a lambda with N
gasMtWithHandlerValue :: PactDb RawBuiltin () -> C.Benchmark
gasMtWithHandlerValue pdb = do
  ee <- defaultEvalEnv pdb rawBuiltinMap
  let es = def
      ps = _eeDefPactStep ee
      frame = Mt
      value = VUnit
      env = CEKEnv { _cePactDb=pdb
                   , _ceLocal=mempty
                   , _ceInCap=False
                   , _ceDefPactStep=ps
                   , _ceBuiltins=benchmarkEnv }
      handler = CEKHandler env unitConst Mt (ErrorState def []) CEKNoHandler
  C.env (pure (es, ee)) $ \ ~(es', ee') -> do
    C.bench "MtWithHandlerValue" $ C.nfAppIO (runEvalM ee' es' . Eval.applyContToValueSmallStep frame handler) value

-- Gas for a lambda with N
gasMtWithHandlerError :: PactDb RawBuiltin () -> C.Benchmark
gasMtWithHandlerError pdb = do
  ee <- defaultEvalEnv pdb rawBuiltinMap
  let es = def
      ps = _eeDefPactStep ee
      frame = Mt
      value = VError "foo" ()
      env = CEKEnv { _cePactDb=pdb
                   , _ceLocal=mempty
                   , _ceInCap=False
                   , _ceDefPactStep=ps
                   , _ceBuiltins=benchmarkEnv }
      handler = CEKHandler env unitConst Mt (ErrorState def []) CEKNoHandler
  C.env (pure (es, ee)) $ \ ~(es', ee') ->
    C.bench "MtWithHandlerError" $ C.nfAppIO (runEvalM ee' es' . Eval.applyContSmallStep frame handler) value

gasArgsWithRemainingArgs :: PactDb RawBuiltin () -> C.Benchmark
gasArgsWithRemainingArgs pdb = do
  ee <- defaultEvalEnv pdb rawBuiltinMap
  let es = def
      ps = _eeDefPactStep ee
      env = CEKEnv { _cePactDb=pdb
                   , _ceLocal=mempty
                   , _ceInCap=False
                   , _ceDefPactStep=ps
                   , _ceBuiltins=benchmarkEnv }
      value = VClosure (C (unitClosureUnary env))
      frame = Args env () [unitConst] Mt
      handler = CEKHandler env unitConst Mt (ErrorState def []) CEKNoHandler
  C.env (pure (es, ee, frame, value)) $ \ ~(es', ee', f', v') ->
    C.bench "Args Frame" $ C.nfAppIO (runEvalM ee' es' . Eval.applyContToValueSmallStep f' handler) v'

gasFnWithRemainingArgs :: PactDb RawBuiltin () -> C.Benchmark
gasFnWithRemainingArgs pdb = do
  ee <- defaultEvalEnv pdb rawBuiltinMap
  let es = def
      ps = _eeDefPactStep ee
      env = CEKEnv { _cePactDb=pdb
                   , _ceLocal=mempty
                   , _ceInCap=False
                   , _ceDefPactStep=ps
                   , _ceBuiltins=benchmarkEnv }
      clo = C (unitClosureBinary env)
      frame = Fn clo env [unitConst] [VUnit] Mt
      value = VUnit
      handler = CEKHandler env unitConst Mt (ErrorState def []) CEKNoHandler
  C.env (pure (es, ee, frame, value)) $ \ ~(es', ee', f', v') ->
    C.bench "Fn Frame" $ C.nfAppIO (runEvalM ee' es' . Eval.applyContToValueSmallStep f' handler) v'


unitClosureNullary :: CEKEnv step RawBuiltin () m -> Closure step RawBuiltin () m
unitClosureNullary env
  = Closure
  { _cloFnName = "foo"
  , _cloModName = ModuleName "foomodule" Nothing
  , _cloTypes = NullaryClosure
  , _cloArity = 0
  , _cloTerm = unitConst
  , _cloRType = Nothing
  , _cloEnv = env
  , _cloInfo = ()}


unitClosureUnary :: CEKEnv step RawBuiltin () m -> Closure step RawBuiltin () m
unitClosureUnary env
  = Closure
  { _cloFnName = "foo"
  , _cloModName = ModuleName "foomodule" Nothing
  , _cloTypes = ArgClosure (NE.fromList [Nothing])
  , _cloArity = 1
  , _cloTerm = unitConst
  , _cloRType = Nothing
  , _cloEnv = env
  , _cloInfo = ()}

unitClosureBinary :: CEKEnv step RawBuiltin () m -> Closure step RawBuiltin () m
unitClosureBinary env
  = Closure
  { _cloFnName = "foo"
  , _cloModName = ModuleName "foomodule" Nothing
  , _cloTypes = ArgClosure (NE.fromList [Nothing, Nothing])
  , _cloArity = 2
  , _cloTerm = unitConst
  , _cloRType = Nothing
  , _cloEnv = env
  , _cloInfo = ()}


boolClosureUnary :: Bool -> CEKEnv step b () m -> Closure step b () m
boolClosureUnary b env
  = Closure
  { _cloFnName = "foo"
  , _cloModName = ModuleName "foomodule" Nothing
  , _cloTypes = ArgClosure (NE.fromList [Nothing])
  , _cloArity = 1
  , _cloTerm = boolConst b
  , _cloRType = Nothing
  , _cloEnv = env
  , _cloInfo = ()}

boolClosureBinary :: Bool -> CEKEnv step b () m -> Closure step b () m
boolClosureBinary b env
  = Closure
  { _cloFnName = "foo"
  , _cloModName = ModuleName "fooModule" Nothing
  , _cloTypes = ArgClosure (NE.fromList [Nothing, Nothing])
  , _cloArity = 2
  , _cloTerm = boolConst b
  , _cloRType = Nothing
  , _cloEnv = env
  , _cloInfo = ()}


gasLetC :: PactDb RawBuiltin () -> C.Benchmark
gasLetC pdb = do
  ee <- defaultEvalEnv pdb rawBuiltinMap
  let es = def
      ps = _eeDefPactStep ee
      env = CEKEnv { _cePactDb=pdb
                   , _ceLocal=mempty
                   , _ceInCap=False
                   , _ceDefPactStep=ps
                   , _ceBuiltins=benchmarkEnv }
      frame = LetC env unitConst Mt
      value = VUnit
      handler = CEKHandler env unitConst Mt (ErrorState def []) CEKNoHandler
  C.env (pure (es, ee, frame, value)) $ \ ~(es', ee', f', v') ->
    C.bench "LetC frame" $ C.nfAppIO (runEvalM ee' es' . Eval.applyContToValueSmallStep f' handler) v'

gasSeqC :: PactDb RawBuiltin () -> C.Benchmark
gasSeqC pdb = do
  ee <- defaultEvalEnv pdb rawBuiltinMap
  let es = def
      ps = _eeDefPactStep ee
      env = CEKEnv { _cePactDb=pdb
                   , _ceLocal=mempty
                   , _ceInCap=False
                   , _ceDefPactStep=ps
                   , _ceBuiltins=benchmarkEnv }
      frame = SeqC env unitConst Mt
      value = VUnit
      handler = CEKHandler env unitConst Mt (ErrorState def []) CEKNoHandler
      title = "SeqC Frame"
  C.env (pure (es, ee, frame, value)) $ \ ~(es', ee', f', v') ->
    C.bench title $ C.nfAppIO (runEvalM ee' es' . Eval.applyContToValueSmallStep f' handler) v'

boolConst :: Bool -> Term name ty builtin ()
boolConst b = Constant (LBool b) ()

strConst :: Text -> Term name ty builtin ()
strConst b = Constant (LString b) ()

gasAndC :: PactDb RawBuiltin () -> Bool -> C.Benchmark
gasAndC pdb b = do
  ee <- defaultEvalEnv pdb rawBuiltinMap
  let es = def
      ps = _eeDefPactStep ee
      env = CEKEnv { _cePactDb=pdb
                   , _ceLocal=mempty
                   , _ceInCap=False
                   , _ceDefPactStep=ps
                   , _ceBuiltins=benchmarkEnv }
      frame = CondC env () (AndC (boolConst b)) Mt
      value = VBool b
      handler = CEKHandler env unitConst Mt (ErrorState def []) CEKNoHandler
      title = "OrC gas with VBool(" <> show b <> ")"
  C.env (pure (es, ee, frame, value)) $ \ ~(es', ee', f', v') ->
    C.bench title $ C.nfAppIO (runEvalM ee' es' . Eval.applyContToValueSmallStep f' handler) v'

gasOrC :: PactDb RawBuiltin () -> Bool -> C.Benchmark
gasOrC pdb b = do
  ee <- defaultEvalEnv pdb rawBuiltinMap
  let es = def
      ps = _eeDefPactStep ee
      env = CEKEnv { _cePactDb=pdb
                   , _ceLocal=mempty
                   , _ceInCap=False
                   , _ceDefPactStep=ps
                   , _ceBuiltins=benchmarkEnv }
      frame = CondC env () (OrC (boolConst b)) Mt
      value = VBool b
      handler = CEKHandler env unitConst Mt (ErrorState def []) CEKNoHandler
      title = "OrC gas with VBool(" <> show b <> ")"
  C.env (pure (es, ee, frame, value)) $ \ ~(es', ee', f', v') ->
    C.bench title $ C.nfAppIO (runEvalM ee' es' . Eval.applyContToValueSmallStep f' handler) v'

gasIfC :: PactDb RawBuiltin () -> Bool -> C.Benchmark
gasIfC pdb b = do
  ee <- defaultEvalEnv pdb rawBuiltinMap
  let es = def
      ps = _eeDefPactStep ee
      env = CEKEnv { _cePactDb=pdb
                   , _ceLocal=mempty
                   , _ceInCap=False
                   , _ceDefPactStep=ps
                   , _ceBuiltins=benchmarkEnv }
      frame = CondC env () (OrC (boolConst b)) Mt
      value = VBool b
      handler = CEKHandler env unitConst Mt (ErrorState def []) CEKNoHandler
      title = "IfC gas with VBool(" <> show b <> ")"
  C.env (pure (es, ee, frame, value)) $ \ ~(es', ee', f', v') ->
    C.bench title $ C.nfAppIO (runEvalM ee' es' . Eval.applyContToValueSmallStep f' handler) v'

gasEnforceC :: PactDb RawBuiltin () -> Bool -> C.Benchmark
gasEnforceC pdb b = do
  ee <- defaultEvalEnv pdb rawBuiltinMap
  let es = def
      ps = _eeDefPactStep ee
      env = CEKEnv { _cePactDb=pdb
                   , _ceLocal=mempty
                   , _ceInCap=False
                   , _ceDefPactStep=ps
                   , _ceBuiltins=benchmarkEnv }
      frame = CondC env () (EnforceC (strConst "boom")) Mt
      value = VBool b
      handler = CEKHandler env unitConst Mt (ErrorState def []) CEKNoHandler
      title = "EnforceC gas with VBool(" <> show b <> ")"
  C.env (pure (es, ee, frame, value)) $ \ ~(es', ee', f', v') ->
    C.bench title $ C.nfAppIO (runEvalM ee' es' . Eval.applyContToValueSmallStep f' handler) v'

-- Note: FilterC applies a reverse
gasFilterCEmptyNElems :: PactDb RawBuiltin () -> Bool -> Int -> C.Benchmark
gasFilterCEmptyNElems pdb b i = do
  ee <- defaultEvalEnv pdb rawBuiltinMap
  let es = def
      ps = _eeDefPactStep ee
      env = CEKEnv { _cePactDb=pdb
                   , _ceLocal=mempty
                   , _ceInCap=False
                   , _ceDefPactStep=ps
                   , _ceBuiltins=benchmarkEnv }
      clo = boolClosureUnary True env
      frame = CondC env () (FilterC (C clo) (PLiteral LUnit) [] (replicate i PUnit)) Mt
      value = VBool b
      handler = CEKHandler env unitConst Mt (ErrorState def []) CEKNoHandler
  C.env (pure (es, ee, frame, value)) $ \ ~(es', ee', f', v') ->
    C.bench "FilterC empty acc case" $ C.nfAppIO (runEvalM ee' es' . Eval.applyContToValueSmallStep f' handler) v'

_gasContType :: PactDb RawBuiltin () -> ContType -> C.Benchmark
_gasContType pdb = \case
  CTFn ->
    -- Note: applyLam case
    gasFnWithRemainingArgs pdb
  CTArgs ->
    -- Note: applyLam case is not handled
    gasArgsWithRemainingArgs pdb
  CTLetC ->
    gasLetC pdb
  CTSeqC ->
    gasSeqC pdb
  CTListC -> undefined
  -- Conditionals
  CTAndC ->
    C.bgroup "AndC Cases" $ (gasAndC pdb) <$> [minBound .. maxBound]
  CTOrC ->
    C.bgroup "OrC Cases" $ (gasOrC pdb) <$> [minBound .. maxBound]
  CTEnforceC ->
    C.bgroup "EnforceC Cases" $ (gasEnforceC pdb) <$> [minBound .. maxBound]
  CTEnforceOneC -> undefined
  CTFilterC ->
    C.bgroup "FilterC Cases" $
      [gasFilterCEmptyNElems pdb b 10
      | b <- [False, True] ]
  CTAndQC -> undefined
  CTOrQC -> undefined
  CTNotQC -> undefined
  -- Builtin forms
  CTMapC -> undefined
  CTFoldC -> undefined
  CTZipC -> undefined
  CTPreSelectC -> undefined
  CTPreFoldDbC -> undefined
  CTSelectC -> undefined
  CTFoldDbFilterC -> undefined
  CTFoldDbMapC -> undefined
  CTReadC -> undefined
  CTWriteC -> undefined
  CTWithReadC -> undefined
  CTWithDefaultReadC -> undefined
  CTKeysC -> undefined
  CTTxIdsC -> undefined
  CTTxLogC -> undefined
  CTKeyLogC -> undefined
  CTCreateTableC -> undefined
  CTEmitEventC -> undefined
  --
  CTObjC -> undefined
  CTCapInvokeC -> undefined
  CTCapBodyC -> undefined
  CTCapPopC -> undefined
  CTDefPactStepC -> undefined
  CTNestedDefPactStepC -> undefined
  CTIgnoreValueC -> undefined
  CTEnforceBoolC -> undefined
  CTEnforcePactValueC -> undefined
  CTModuleAdminC -> undefined
  CTStackPopC -> undefined
  CTEnforceErrorC -> undefined
  CTMt -> undefined
