{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Pact.Core.GasModel.InterpreterGas(benchmarks) where

import Control.Lens
-- import Control.Monad
import Control.Monad.IO.Class
import Data.Default
-- import Data.Functor(void)
-- import Data.Bifunctor(bimap)
-- import Criterion.Types(Report)
import qualified Data.RAList as RA
import qualified Data.List.NonEmpty as NE
import qualified Criterion as C
-- import qualified Criterion.Report as C
-- import qualified Criterion.Analysis as C
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Database.SQLite3 as SQL

import Pact.Core.Builtin
import Pact.Core.Environment
import Pact.Core.Errors
import Pact.Core.Names
import Pact.Core.Gas
import Pact.Core.Literal
import Pact.Core.Type
import Pact.Core.Capabilities
import Pact.Core.IR.Eval.Runtime
import Pact.Core.PactValue
import Pact.Core.IR.Term
import Pact.Core.Persistence
import Pact.Core.Hash
import Pact.Core.Persistence.SQLite
import Pact.Core.Serialise (serialisePact)
import Pact.Core.IR.Eval.CEK.Types
import qualified Pact.Core.IR.Eval.CEK as Eval

import Pact.Core.GasModel.Utils


runEvalDropState
  :: EvalEnv b i
  -> EvalState b i
  -> EvalM b i a
  -> IO (Either (PactError i) a)
runEvalDropState ee es = fmap fst . runEvalM ee es

benchmarks :: C.Benchmark
benchmarks = C.envWithCleanup mkPactDb cleanupPactDb $ \ ~(pdb, _db) -> do
  C.bgroup "pact-core-term-gas" [staticExecutionBenchmarks pdb, termGas pdb, interpReturnGas pdb]
  where
  mkPactDb = do
    tup@(pdb, _) <- unsafeCreateSqlitePactDb serialisePact ":memory:"
    ignoreGas def $ prepopulateDb pdb
    _ <- _pdbBeginTx pdb Transactional
    pure tup

  cleanupPactDb (_, db) = SQL.close db

gasVarBound :: Int -> EvalEnv CoreBuiltin () -> EvalState CoreBuiltin () -> C.Benchmark
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
    C.bench title $ C.nfAppIO (runEvalDropState ee' es' . Eval.evaluateTermSmallStep Mt CEKNoHandler env') term'

varGas :: CoreDb -> C.Benchmark
varGas pdb =
  C.env mkEnv $ \ ~(ee, es) ->
      C.bgroup "Variables: bound" $ (\i -> gasVarBound i ee es) <$> [10, 50, 100, 150, 200, 250, 300, 400, 450]
  where
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
    pure (ee, es)

simpleTermGas :: CoreTerm -> String -> CoreDb -> C.Benchmark
simpleTermGas term title pdb =
  C.env mkEnv $ \ ~(term', es', ee', env') -> do
    C.bench title $ C.nfAppIO (runEvalDropState ee' es' . Eval.evaluateTermSmallStep Mt CEKNoHandler env') term'
  where
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
        ps = _eeDefPactStep ee
        env = CEKEnv { _cePactDb=pdb
                    , _ceLocal=mempty
                    , _ceInCap=False
                    , _ceDefPactStep=ps
                    , _ceBuiltins=benchmarkEnv }
    pure (term, es, ee, env)

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
  let letBind = Let (Arg "_" Nothing ()) unitConst unitConst ()
  in simpleTermGas letBind "Let Node"

constantExample :: CoreTerm -> CEKValue CEKSmallStep CoreBuiltin () Eval
constantExample (Constant LUnit ()) = VPactValue (PLiteral LUnit)
constantExample _ = error "boom"

constantGasEquiv :: C.Benchmark
constantGasEquiv = do
  let term = (Constant LUnit ()) :: CoreTerm
  C.env (pure term) $ \ ~(c) ->
    C.bench "constant example: no monadic overhead" $ C.nf constantExample c

-- Simple case for evaluating to normal form for (+ 1 2)
plusOneTwo :: CoreDb -> C.Benchmark
plusOneTwo pdb = do
  C.env mkEnv $ \ ~(term', es', ee', env') -> do
    C.bench "(+ 1 2)" $ C.nfAppIO (runEvalDropState ee' es' . Eval.evalNormalForm env') term'
  where
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
        ps = _eeDefPactStep ee
        env = CEKEnv { _cePactDb=pdb
                    , _ceLocal=mempty
                    , _ceInCap=False
                    , _ceDefPactStep=ps
                    , _ceBuiltins=benchmarkBigStepEnv }
    let term = App (Builtin CoreAdd ()) [Constant (LInteger 1) (), Constant (LInteger 2) ()] ()
    pure (term, es, ee, env)

constExpr :: CoreDb -> C.Benchmark
constExpr pdb = do
  C.env mkEnv $ \ ~(term', es', ee', env') -> do
    C.bench "constExpr" $ C.nfAppIO (runEvalDropState ee' es' . Eval.evalNormalForm env') term'
  where
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
        ps = _eeDefPactStep ee
        env = CEKEnv { _cePactDb=pdb
                    , _ceLocal=mempty
                    , _ceInCap=False
                    , _ceDefPactStep=ps
                    , _ceBuiltins=benchmarkBigStepEnv }
    let lamTerm = Lam (NE.fromList [Arg "_" Nothing (), Arg "_" Nothing ()]) (Var (Name "boop" (NBound 1)) ()) ()
    let term = App lamTerm [Constant (LInteger 1) (), Constant (LInteger 2) ()] ()
    pure (term, es, ee, env)

constExpr2 :: CoreDb -> C.Benchmark
constExpr2 pdb = do
  C.env mkEnv $ \ ~(term', es', ee', env') -> do
    C.bench "(map (+ 1) (enumerate 0 999999))" $ C.nfAppIO (runEvalDropState ee' es' . Eval.evalNormalForm env') term'
  where
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
        ps = _eeDefPactStep ee
        env = CEKEnv { _cePactDb=pdb
                    , _ceLocal=mempty
                    , _ceInCap=False
                    , _ceDefPactStep=ps
                    , _ceBuiltins=benchmarkBigStepEnv }
    let lamTerm = App (Builtin CoreAdd ()) [intConst 1] ()
        enumerateTerm = App (Builtin CoreEnumerate ()) [intConst 0, intConst 999999] ()
    let term = App (Builtin CoreMap ()) [lamTerm, enumerateTerm] ()
    pure (term, es, ee, env)

-- Gas for a lambda with N arguments
-- gasLamNArgs :: Int -> EvalEnv CoreBuiltin () -> EvalState CoreBuiltin () -> C.Benchmark
gasLamNArgs :: Int -> CoreDb -> C.Benchmark
gasLamNArgs n pdb =
  C.env mkEnv $ \ ~(term', es', ee', env') ->
    C.bench title $ C.nfAppIO (runEvalDropState ee' es' . Eval.evaluateTermSmallStep Mt CEKNoHandler env') term'
  where
  title = "Lam: " <> show n <> " args case"
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
        mkArg i = Arg ("Arg#" <> T.pack (show i)) Nothing ()
        args = mkArg <$> [1..n]
        term = Lam (NE.fromList args) (Constant LUnit ()) ()
        ps = _eeDefPactStep ee
        env = CEKEnv { _cePactDb=pdb
                    , _ceLocal = mempty
                    , _ceInCap=False
                    , _ceDefPactStep=ps
                    , _ceBuiltins= benchmarkEnv }

    pure (term, es, ee, env)

lamGas :: CoreDb -> C.Benchmark
lamGas pdb =
  C.bgroup "Lambda Node" $ [ gasLamNArgs i pdb | i <- [1..25]]

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
builtinNodeGas = simpleTermGas (Builtin CoreAt ()) "Builtin node"

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

termGas :: CoreDb -> C.Benchmark
termGas pdb = C.bgroup "term reduction benchmarks" (benchmarkNodeType pdb <$> [minBound .. maxBound])

staticExecutionBenchmarks :: CoreDb -> C.Benchmark
staticExecutionBenchmarks pdb =
  C.bgroup "Simple reduction benchmarks"
  [ plusOneTwo pdb
  , constExpr pdb
  , constExpr2 pdb
  , constantGasEquiv]

interpReturnGas :: CoreDb -> C.Benchmark
interpReturnGas pdb =
  C.bgroup "CEKH continuation control flow benches" $ gasContType pdb <$> [minBound .. maxBound]

withCapFormGas :: CoreDb -> C.Benchmark
withCapFormGas =
  simpleTermGas (CapabilityForm (WithCapability unitConst unitConst) ()) "Capability node"


createUserGuardGasNArgs :: Int -> CoreDb -> C.Benchmark
createUserGuardGasNArgs nArgs pdb =
  C.env mkEnv $ \ ~(term', es', ee', env') -> do
    C.bench title $ C.nfAppIO (runEvalDropState ee' es' . Eval.evaluateTermSmallStep Mt CEKNoHandler env') term'
  where
  title = "Create User Guard, " <> show nArgs <> " args"
  mkEnv = do
    let args =  [ Arg ("_foo" <> T.pack (show i)) Nothing () | i <- [2..nArgs] ]
    ee <- liftIO $ defaultGasEvalEnv pdb
    let mn = ModuleName "foomodule" Nothing
        mh = ModuleHash (pactHash "foo")
        fqn = FullyQualifiedName mn "foo" mh
        dfun = Defun (Arg "foo" Nothing ()) args unitConst ()
        es = over (esLoaded . loAllLoaded) (M.insert fqn (Dfun dfun)) def
        name = Name "foo" (NTopLevel mn mh)
        term = CapabilityForm (CreateUserGuard name (replicate nArgs unitConst)) ()
        ps = _eeDefPactStep ee
        env = CEKEnv { _cePactDb=pdb
                    , _ceLocal=mempty
                    , _ceInCap=False
                    , _ceDefPactStep=ps
                    , _ceBuiltins=benchmarkEnv }
    pure (term, es, ee, env)

createUserGuardGas :: CoreDb -> C.Benchmark
createUserGuardGas pdb =
  C.bgroup "Create user guard node" [ createUserGuardGasNArgs i pdb | i <- [0..5]]


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


-- Gas for a lambda with N
gasMtReturnNoHandler :: PactDb CoreBuiltin () -> C.Benchmark
gasMtReturnNoHandler pdb =
  C.env mkEnv $ \ ~(ee, es, frame, handler, v) -> do
    C.bench "MtReturnNoHandler" $ C.nfAppIO (runEvalDropState ee es . Eval.applyContToValueSmallStep frame handler) v
  where
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
        frame = Mt
        value = VUnit
        handler = CEKNoHandler
    pure (ee, es, frame, handler, value)

-- Gas for a lambda with N
gasMtWithHandlerValue :: PactDb CoreBuiltin () -> C.Benchmark
gasMtWithHandlerValue pdb = do
  C.env mkEnv $ \ ~(ee, es, frame, handler, v) -> do
    C.bench "MtWithHandlerValue" $ C.nfAppIO (runEvalDropState ee es . Eval.applyContToValueSmallStep frame handler) v
  where
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
        ps = _eeDefPactStep ee
        frame = Mt
        value = VUnit
        env = CEKEnv { _cePactDb=pdb
                    , _ceLocal=mempty
                    , _ceInCap=False
                    , _ceDefPactStep=ps
                    , _ceBuiltins=benchmarkEnv }
        handler = CEKHandler env unitConst Mt (ErrorState def [] (pure def)) CEKNoHandler
    pure (ee, es, frame, handler, value)

-- Gas for a lambda with N
gasMtWithHandlerError :: PactDb CoreBuiltin () -> C.Benchmark
gasMtWithHandlerError pdb =
  C.env mkEnv $ \ ~(ee, es, frame, handler, value) ->
    C.bench "MtWithHandlerError" $ C.nfAppIO (runEvalDropState ee es . Eval.applyContSmallStep frame handler) value
  where
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
        ps = _eeDefPactStep ee
        frame = Mt
        value = VError "foo" ()
        env = CEKEnv { _cePactDb=pdb
                    , _ceLocal=mempty
                    , _ceInCap=False
                    , _ceDefPactStep=ps
                    , _ceBuiltins=benchmarkEnv }
        handler = CEKHandler env unitConst Mt (ErrorState def [] (pure def)) CEKNoHandler
    pure (ee, es, frame, handler, value)

gasArgsWithRemainingArgs :: PactDb CoreBuiltin () -> C.Benchmark
gasArgsWithRemainingArgs pdb =
  C.env mkEnv $ \ ~(ee, es, frame, handler, value) ->
    C.bench "Args Frame" $ C.nfAppIO (runEvalDropState ee es . Eval.applyContToValueSmallStep frame handler) value
  where
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
        ps = _eeDefPactStep ee
        env = CEKEnv { _cePactDb=pdb
                    , _ceLocal=mempty
                    , _ceInCap=False
                    , _ceDefPactStep=ps
                    , _ceBuiltins=benchmarkEnv }
        value = VClosure (C (unitClosureUnary env))
        frame = Args env () [unitConst] Mt
        handler = CEKNoHandler
    pure (ee, es, frame, handler, value)

gasFnWithRemainingArgs :: PactDb CoreBuiltin () -> C.Benchmark
gasFnWithRemainingArgs pdb =
  C.env mkEnv $ \ ~(ee, es, frame, handler, value) ->
    C.bench "Fn Frame" $ C.nfAppIO (runEvalDropState ee es . Eval.applyContToValueSmallStep frame handler) value
  where
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
        ps = _eeDefPactStep ee
        env = CEKEnv { _cePactDb=pdb
                    , _ceLocal=mempty
                    , _ceInCap=False
                    , _ceDefPactStep=ps
                    , _ceBuiltins=benchmarkEnv }
        clo = C (unitClosureBinary env)
        frame = Fn clo env [unitConst] [VUnit] Mt
        value = VUnit
        handler = CEKNoHandler
    pure (ee, es, frame, handler, value)


gasLetC :: PactDb CoreBuiltin () -> C.Benchmark
gasLetC pdb =
  C.env mkEnv $ \ ~(ee, es, frame, handler, value) ->
    C.bench "LetC frame" $ C.nfAppIO (runEvalDropState ee es . Eval.applyContToValueSmallStep frame handler) value
  where
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
        ps = _eeDefPactStep ee
        env = CEKEnv { _cePactDb=pdb
                    , _ceLocal=mempty
                    , _ceInCap=False
                    , _ceDefPactStep=ps
                    , _ceBuiltins=benchmarkEnv }
        frame = LetC env unitConst Mt
        value = VUnit
        handler = CEKNoHandler
    pure (ee, es, frame, handler, value)

gasSeqC :: PactDb CoreBuiltin () -> C.Benchmark
gasSeqC pdb = do
  C.env mkEnv $ \ ~(ee, es, frame, handler, value) ->
    C.bench title $ C.nfAppIO (runEvalDropState ee es . Eval.applyContToValueSmallStep frame handler) value
  where
  title = "SeqC Frame"
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
        ps = _eeDefPactStep ee
        env = CEKEnv { _cePactDb=pdb
                    , _ceLocal=mempty
                    , _ceInCap=False
                    , _ceDefPactStep=ps
                    , _ceBuiltins=benchmarkEnv }
        frame = SeqC env unitConst Mt
        value = VUnit
        handler = CEKNoHandler
    pure (ee, es, frame, handler, value)

gasAndC :: PactDb CoreBuiltin () -> Bool -> C.Benchmark
gasAndC pdb b =
  C.env mkEnv $ \ ~(ee, es, frame, handler, value) ->
    C.bench title $ C.nfAppIO (runEvalDropState ee es . Eval.applyContToValueSmallStep frame handler) value
  where
  title = "AndC gas with VBool(" <> show b <> ")"
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
        ps = _eeDefPactStep ee
        env = CEKEnv { _cePactDb=pdb
                    , _ceLocal=mempty
                    , _ceInCap=False
                    , _ceDefPactStep=ps
                    , _ceBuiltins=benchmarkEnv }
        frame = CondC env () (AndC (boolConst b)) Mt
        value = VBool b
        handler = CEKNoHandler
    pure (ee, es, frame, handler, value)

gasOrC :: PactDb CoreBuiltin () -> Bool -> C.Benchmark
gasOrC pdb b =
  benchApplyContToValue mkEnv title
  where
  title = "OrC gas with VBool(" <> show b <> ")"
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
        ps = _eeDefPactStep ee
        env = CEKEnv { _cePactDb=pdb
                    , _ceLocal=mempty
                    , _ceInCap=False
                    , _ceDefPactStep=ps
                    , _ceBuiltins=benchmarkEnv }
        frame = CondC env () (OrC (boolConst b)) Mt
        value = VBool b
        handler = CEKNoHandler
    pure (ee, es, frame, handler, value)

gasIfC :: PactDb CoreBuiltin () -> Bool -> C.Benchmark
gasIfC pdb b =
  benchApplyContToValue mkEnv title
  where
  title = "IfC gas with VBool(" <> show b <> ")"
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
        ps = _eeDefPactStep ee
        env = CEKEnv { _cePactDb=pdb
                    , _ceLocal=mempty
                    , _ceInCap=False
                    , _ceDefPactStep=ps
                    , _ceBuiltins=benchmarkEnv }
        frame = CondC env () (IfC (boolConst b) (boolConst b)) Mt
        value = VBool b
        handler = CEKNoHandler
    pure (ee, es, frame, handler, value)

gasEnforceC :: PactDb CoreBuiltin () -> Bool -> C.Benchmark
gasEnforceC pdb b =
  benchApplyContToValue mkEnv title
  where
  title = "EnforceC gas with VBool(" <> show b <> ")"
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
        ps = _eeDefPactStep ee
        env = CEKEnv { _cePactDb=pdb
                    , _ceLocal=mempty
                    , _ceInCap=False
                    , _ceDefPactStep=ps
                    , _ceBuiltins=benchmarkEnv }
        frame = CondC env () (EnforceC (strConst "boom")) Mt
        value = VBool b
        handler = CEKNoHandler
    pure (ee, es, frame, handler, value)

-- Note: FilterC applies a reverse
gasFilterCEmptyNElems :: PactDb CoreBuiltin () -> Bool -> Int -> C.Benchmark
gasFilterCEmptyNElems pdb b i =
  benchApplyContToValue mkEnv "FilterC empty acc case"
  where
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
        ps = _eeDefPactStep ee
        env = CEKEnv { _cePactDb=pdb
                    , _ceLocal=mempty
                    , _ceInCap=False
                    , _ceDefPactStep=ps
                    , _ceBuiltins=benchmarkEnv }
        clo = boolClosureUnary True env
        frame = CondC env () (FilterC (C clo) (PLiteral LUnit) [] (replicate i PUnit)) Mt
        value = VBool b
        handler = CEKNoHandler
    pure (ee, es, frame, handler, value)

gasAndQC :: PactDb CoreBuiltin () -> Bool -> C.Benchmark
gasAndQC pdb b =
  benchApplyContToValue mkEnv "AndQC boolean case"
  where
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
        ps = _eeDefPactStep ee
        env = CEKEnv { _cePactDb=pdb
                    , _ceLocal=mempty
                    , _ceInCap=False
                    , _ceDefPactStep=ps
                    , _ceBuiltins=benchmarkEnv }
        clo = boolClosureUnary b env
        frame = CondC env () (AndQC (C clo) (PLiteral LUnit)) Mt
        value = VBool b
        handler = CEKNoHandler
    pure (ee, es, frame, handler, value)

gasOrQC :: PactDb CoreBuiltin () -> Bool -> C.Benchmark
gasOrQC pdb b =
  benchApplyContToValue mkEnv "OrQC boolean case"
  where
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
        ps = _eeDefPactStep ee
        env = CEKEnv { _cePactDb=pdb
                    , _ceLocal=mempty
                    , _ceInCap=False
                    , _ceDefPactStep=ps
                    , _ceBuiltins=benchmarkEnv }
        clo = boolClosureUnary b env
        frame = CondC env () (OrQC (C clo) (PLiteral LUnit)) Mt
        value = VBool b
        handler = CEKNoHandler
    pure (ee, es, frame, handler, value)

gasNotQC :: PactDb CoreBuiltin () -> C.Benchmark
gasNotQC pdb =
  benchApplyContToValue mkEnv "NotQC boolean case"
  where
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
        ps = _eeDefPactStep ee
        env = CEKEnv { _cePactDb=pdb
                    , _ceLocal=mempty
                    , _ceInCap=False
                    , _ceDefPactStep=ps
                    , _ceBuiltins=benchmarkEnv }
        frame = CondC env () (NotQC) Mt
        value = VBool True
        handler = CEKNoHandler
    pure (ee, es, frame, handler, value)

gasMapC :: PactDb CoreBuiltin () -> C.Benchmark
gasMapC pdb =
  benchApplyContToValue mkEnv "MapC"
  where
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
        ps = _eeDefPactStep ee
        env = CEKEnv { _cePactDb=pdb
                    , _ceLocal=mempty
                    , _ceInCap=False
                    , _ceDefPactStep=ps
                    , _ceBuiltins=benchmarkEnv }
        clo = unitClosureUnary env
        bframe = MapC (C clo) [PUnit] []
        frame = BuiltinC env () bframe Mt
        value = VUnit
        handler = CEKNoHandler
    pure (ee, es, frame, handler, value)

gasFoldC :: PactDb CoreBuiltin () -> C.Benchmark
gasFoldC pdb =
  benchApplyContToValue mkEnv "FoldC"
  where
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
        ps = _eeDefPactStep ee
        env = CEKEnv { _cePactDb=pdb
                    , _ceLocal=mempty
                    , _ceInCap=False
                    , _ceDefPactStep=ps
                    , _ceBuiltins=benchmarkEnv }
        clo = unitClosureBinary env
        bframe = FoldC (C clo) [PUnit]
        frame = BuiltinC env () bframe Mt
        value = VUnit
        handler = CEKNoHandler
    pure (ee, es, frame, handler, value)

gasZipC :: PactDb CoreBuiltin () ->C.Benchmark
gasZipC pdb =
  benchApplyContToValue mkEnv "ZipC"
  where
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
        ps = _eeDefPactStep ee
        env = CEKEnv { _cePactDb=pdb
                    , _ceLocal=mempty
                    , _ceInCap=False
                    , _ceDefPactStep=ps
                    , _ceBuiltins=benchmarkEnv }
        clo = unitClosureBinary env
        bframe = ZipC (C clo) ([PUnit], [PUnit]) []
        frame = BuiltinC env () bframe Mt
        value = VUnit
        handler = CEKNoHandler
    pure (ee, es, frame, handler, value)

gasPreSelectC :: PactDb CoreBuiltin () ->C.Benchmark
gasPreSelectC pdb =
  benchApplyContToValue mkEnv "PreSelectC"
  where
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
        ps = _eeDefPactStep ee
        env = CEKEnv { _cePactDb=pdb
                    , _ceLocal=mempty
                    , _ceInCap=False
                    , _ceDefPactStep=ps
                    , _ceBuiltins=benchmarkEnv }
        clo = boolClosureUnary True env
        bframe = PreSelectC gasModelTableValue (C clo) Nothing
        frame = BuiltinC env () bframe Mt
        value = VUnit
        handler = CEKNoHandler
    pure (ee, es, frame, handler, value)

gasPreFoldDbC :: PactDb CoreBuiltin () ->C.Benchmark
gasPreFoldDbC pdb =
  benchApplyContToValue mkEnv "PreFoldDBC"
  where
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
        ps = _eeDefPactStep ee
        env = CEKEnv { _cePactDb=pdb
                    , _ceLocal=mempty
                    , _ceInCap=False
                    , _ceDefPactStep=ps
                    , _ceBuiltins=benchmarkEnv }
        queryClo = boolClosureUnary True env
        appClo = unitClosureBinary env
        bframe = PreFoldDbC gasModelTableValue (C queryClo) (C appClo)
        frame = BuiltinC env () bframe Mt
        value = VUnit
        handler = CEKNoHandler
    pure (ee, es, frame, handler, value)

gasSelectC :: PactDb CoreBuiltin () ->C.Benchmark
gasSelectC pdb =
  benchApplyContToValue mkEnv "SelectC"
  where
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
        ps = _eeDefPactStep ee
        env = CEKEnv { _cePactDb=pdb
                    , _ceLocal=mempty
                    , _ceInCap=False
                    , _ceDefPactStep=ps
                    , _ceBuiltins=benchmarkEnv }
        queryClo = boolClosureUnary True env
        bframe = SelectC gasModelTableValue (C queryClo) (rowDataToObjectData gmTableV1) [gmTableK2] [] Nothing
        frame = BuiltinC env () bframe Mt
        value = VBool True
        handler = CEKNoHandler
    pure (ee, es, frame, handler, value)

gasFoldDbFilterC :: PactDb CoreBuiltin () ->C.Benchmark
gasFoldDbFilterC pdb =
  benchApplyContToValue mkEnv "FoldDbFilterC"
  where
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
        ps = _eeDefPactStep ee
        env = CEKEnv { _cePactDb=pdb
                    , _ceLocal=mempty
                    , _ceInCap=False
                    , _ceDefPactStep=ps
                    , _ceBuiltins=benchmarkEnv }
        queryClo = boolClosureUnary True env
        appClo = unitClosureBinary env
        bframe = FoldDbFilterC gasModelTableValue (C queryClo) (C appClo) (gmTableK1,rowDataToObjectData gmTableV1) [gmTableK2] []
        frame = BuiltinC env () bframe Mt
        value = VBool True
        handler = CEKNoHandler
    pure (ee, es, frame, handler, value)

gasFoldDbMapC :: PactDb CoreBuiltin () ->C.Benchmark
gasFoldDbMapC pdb =
  benchApplyContToValue mkEnv "FoldDbMapC"
  where
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
        ps = _eeDefPactStep ee
        env = CEKEnv { _cePactDb=pdb
                    , _ceLocal=mempty
                    , _ceInCap=False
                    , _ceDefPactStep=ps
                    , _ceBuiltins=benchmarkEnv }
        appClo = unitClosureBinary env
        (RowData v) = gmTableV1
        bframe = FoldDbMapC gasModelTableValue (C appClo) [(gmTableK1, PObject v)] []
        frame = BuiltinC env () bframe Mt
        value = VUnit
        handler = CEKNoHandler
    pure (ee, es, frame, handler, value)

gasReadC :: PactDb CoreBuiltin () ->C.Benchmark
gasReadC pdb =
  benchApplyContToValue mkEnv "ReadC"
  where
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
        ps = _eeDefPactStep ee
        env = CEKEnv { _cePactDb=pdb
                    , _ceLocal=mempty
                    , _ceInCap=False
                    , _ceDefPactStep=ps
                    , _ceBuiltins=benchmarkEnv }
        bframe = ReadC gasModelTableValue gmTableK1
        frame = BuiltinC env () bframe Mt
        value = VUnit
        handler = CEKNoHandler
    pure (ee, es, frame, handler, value)

-- Todo: further gas model work will have gasWriteC work on
gasWriteC :: PactDb CoreBuiltin () -> C.Benchmark
gasWriteC pdb =
  benchApplyContWithRollback  mkEnv "WriteC"
  where
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
        ps = _eeDefPactStep ee
        env = CEKEnv { _cePactDb=pdb
                    , _ceLocal=mempty
                    , _ceInCap=False
                    , _ceDefPactStep=ps
                    , _ceBuiltins=benchmarkEnv }
        bframe = WriteC gasModelTableValue Write gmTableK3 (rowDataToObjectData gmTableV3)
        frame = BuiltinC env () bframe Mt
        value = VUnit
        handler = CEKNoHandler
    pure (ee, es, frame, handler, value)

gasWithDefaultReadC :: PactDb CoreBuiltin () -> C.Benchmark
gasWithDefaultReadC pdb =
  benchApplyContToValue mkEnv "WithDefaultReadC"
  where
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
        ps = _eeDefPactStep ee
        env = CEKEnv { _cePactDb=pdb
                    , _ceLocal=mempty
                    , _ceInCap=False
                    , _ceDefPactStep=ps
                    , _ceBuiltins=benchmarkEnv }
        clo = unitClosureUnary env
        bframe = WithDefaultReadC gasModelTableValue gmTableK2 (rowDataToObjectData gmTableV2) (C clo)
        frame = BuiltinC env () bframe Mt
        value = VUnit
        handler = CEKNoHandler
    pure (ee, es, frame, handler, value)

gasKeysC :: PactDb CoreBuiltin () -> C.Benchmark
gasKeysC pdb =
  benchApplyContToValue mkEnv "KeysC"
  where
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
        ps = _eeDefPactStep ee
        env = CEKEnv { _cePactDb=pdb
                    , _ceLocal=mempty
                    , _ceInCap=False
                    , _ceDefPactStep=ps
                    , _ceBuiltins=benchmarkEnv }
        bframe = KeysC gasModelTableValue
        frame = BuiltinC env () bframe Mt
        value = VUnit
        handler = CEKNoHandler
    pure (ee, es, frame, handler, value)

gasTxIdsC :: PactDb CoreBuiltin () -> C.Benchmark
gasTxIdsC pdb =
  benchApplyContToValue mkEnv "TxIdsC"
  where
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
        ps = _eeDefPactStep ee
        env = CEKEnv { _cePactDb=pdb
                    , _ceLocal=mempty
                    , _ceInCap=False
                    , _ceDefPactStep=ps
                    , _ceBuiltins=benchmarkEnv }
        bframe = TxIdsC gasModelTableValue 0
        frame = BuiltinC env () bframe Mt
        value = VUnit
        handler = CEKNoHandler
    pure (ee, es, frame, handler, value)

gasTxLogC :: PactDb CoreBuiltin () -> C.Benchmark
gasTxLogC pdb =
  benchApplyContToValue mkEnv "TxLogC"
  where
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
        ps = _eeDefPactStep ee
        env = CEKEnv { _cePactDb=pdb
                    , _ceLocal=mempty
                    , _ceInCap=False
                    , _ceDefPactStep=ps
                    , _ceBuiltins=benchmarkEnv }
        bframe = TxLogC gasModelTableValue 0
        frame = BuiltinC env () bframe Mt
        value = VUnit
        handler = CEKNoHandler
    pure (ee, es, frame, handler, value)

gasKeyLogC :: PactDb CoreBuiltin () -> C.Benchmark
gasKeyLogC pdb =
  benchApplyContToValue mkEnv "KeyLogC"
  where
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
        ps = _eeDefPactStep ee
        env = CEKEnv { _cePactDb=pdb
                    , _ceLocal=mempty
                    , _ceInCap=False
                    , _ceDefPactStep=ps
                    , _ceBuiltins=benchmarkEnv }
        bframe = KeyLogC gasModelTableValue gmTableK1 0
        frame = BuiltinC env () bframe Mt
        value = VUnit
        handler = CEKNoHandler
    pure (ee, es, frame, handler, value)

gasCreateTableC :: PactDb CoreBuiltin () -> C.Benchmark
gasCreateTableC pdb =
  benchApplyContWithRollback mkEnv "SelectC"
  where
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
        ps = _eeDefPactStep ee
        env = CEKEnv { _cePactDb=pdb
                    , _ceLocal=mempty
                    , _ceInCap=False
                    , _ceDefPactStep=ps
                    , _ceBuiltins=benchmarkEnv }
        bframe = CreateTableC gasModelTableValue2
        frame = BuiltinC env () bframe Mt
        value = VUnit
        handler = CEKNoHandler
    pure (ee, es, frame, handler, value)

gasEmitEventC :: PactDb CoreBuiltin () -> C.Benchmark
gasEmitEventC pdb =
  benchApplyContToValue mkEnv "EmitEventC"
  where
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
        ps = _eeDefPactStep ee
        env = CEKEnv { _cePactDb=pdb
                    , _ceLocal=mempty
                    , _ceInCap=False
                    , _ceDefPactStep=ps
                    , _ceBuiltins=benchmarkEnv }
        bframe = EmitEventC (CapToken (mkGasModelFqn gmDcapUnmanagedName) [])
        frame = BuiltinC env () bframe Mt
        value = VUnit
        handler = CEKNoHandler
    pure (ee, es, frame, handler, value)


gasDefineKeysetC :: PactDb CoreBuiltin () -> C.Benchmark
gasDefineKeysetC pdb =
  benchApplyContWithRollback mkEnv "DefineKeysetC"
  where
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
        ps = _eeDefPactStep ee
        env = CEKEnv { _cePactDb=pdb
                    , _ceLocal=mempty
                    , _ceInCap=False
                    , _ceDefPactStep=ps
                    , _ceBuiltins=benchmarkEnv }
        bframe = DefineKeysetC gmKeysetName gmKeyset
        frame = BuiltinC env () bframe Mt
        value = VUnit
        handler = CEKNoHandler
    pure (ee, es, frame, handler, value)

gasDefineNamespaceC :: PactDb CoreBuiltin () -> C.Benchmark
gasDefineNamespaceC pdb =
  benchApplyContWithRollback mkEnv "DefineNamespaceC"
  where
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
        ps = _eeDefPactStep ee
        env = CEKEnv { _cePactDb=pdb
                    , _ceLocal=mempty
                    , _ceInCap=False
                    , _ceDefPactStep=ps
                    , _ceBuiltins=benchmarkEnv }
        bframe = DefineNamespaceC gmNamespace
        frame = BuiltinC env () bframe Mt
        value = VUnit
        handler = CEKNoHandler
    pure (ee, es, frame, handler, value)

gasObjC :: PactDb CoreBuiltin () -> C.Benchmark
gasObjC pdb =
  benchApplyContWithRollback mkEnv "ObjC"
  where
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
        ps = _eeDefPactStep ee
        env = CEKEnv { _cePactDb=pdb
                    , _ceLocal=mempty
                    , _ceInCap=False
                    , _ceDefPactStep=ps
                    , _ceBuiltins=benchmarkEnv }
        frame = ObjC env () (Field "a") [(Field "b", intConst 1)] [] Mt
        value = VUnit
        handler = CEKNoHandler
    pure (ee, es, frame, handler, value)

gasCapInvokeCUserGuard :: PactDb CoreBuiltin () -> C.Benchmark
gasCapInvokeCUserGuard pdb =
  benchApplyContWithRollback mkEnv "CapInvokeCUserGuard"
  where
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
        ps = _eeDefPactStep ee
        env = CEKEnv { _cePactDb=pdb
                    , _ceLocal=mempty
                    , _ceInCap=False
                    , _ceDefPactStep=ps
                    , _ceBuiltins=benchmarkEnv }
        cframe = CreateUserGuardC (mkGasModelFqn gmManagerDfunName) [intConst 1] []
        frame = CapInvokeC env () cframe Mt
        value = VInteger 1
        handler = CEKNoHandler
    pure (ee, es, frame, handler, value)

gasCapInvokeCWithCapC :: PactDb CoreBuiltin () -> C.Benchmark
gasCapInvokeCWithCapC pdb =
  benchApplyContWithRollback mkEnv "CapInvokeCWithCapC"
  where
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
        ps = _eeDefPactStep ee
        env = CEKEnv { _cePactDb=pdb
                    , _ceLocal=mempty
                    , _ceInCap=False
                    , _ceDefPactStep=ps
                    , _ceBuiltins=benchmarkEnv }
        cframe = WithCapC unitConst
        frame = CapInvokeC env () cframe Mt
        value = VCapToken (CapToken (mkGasModelFqn gmDcapUnmanagedName) [])
        handler = CEKNoHandler
    pure (ee, es, frame, handler, value)

gasCapInvokeCWithCapCManaged :: PactDb CoreBuiltin () -> C.Benchmark
gasCapInvokeCWithCapCManaged pdb =
  benchApplyContWithRollback mkEnv "CapInvokeCWithCapCManaged"
  where
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
        ps = _eeDefPactStep ee
        env = CEKEnv { _cePactDb=pdb
                    , _ceLocal=mempty
                    , _ceInCap=False
                    , _ceDefPactStep=ps
                    , _ceBuiltins=benchmarkEnv }
        capTokenFqn = CapToken (mkGasModelFqn gmDcapManagedName) [PInteger 1]
        capTokenQn = CapToken (QualifiedName gmDcapManagedName gmModuleName) [PInteger 1]
        -- Insert the managed cap into the the environment
        signedEs = over eeMsgSigs (M.insert gmPublicKeyText1 (S.singleton capTokenQn)) ee
        cframe = WithCapC unitConst
        frame = CapInvokeC env () cframe Mt
        value = VCapToken capTokenFqn
        handler = CEKNoHandler
    pure (signedEs, es, frame, handler, value)

gasCapInvokeCWithCapAutoManaged :: PactDb CoreBuiltin () -> C.Benchmark
gasCapInvokeCWithCapAutoManaged pdb =
  benchApplyContWithRollback mkEnv "CapInvokeCWithCapAutoManaged"
  where
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
        ps = _eeDefPactStep ee
        env = CEKEnv { _cePactDb=pdb
                    , _ceLocal=mempty
                    , _ceInCap=False
                    , _ceDefPactStep=ps
                    , _ceBuiltins=benchmarkEnv }
        capTokenFqn = CapToken (mkGasModelFqn gmDcapAutoManagedName) []
        capTokenQn = CapToken (QualifiedName gmDcapAutoManagedName gmModuleName) []
        -- Insert the managed cap into the the environment
        signedEs = over eeMsgSigs (M.insert gmPublicKeyText1 (S.singleton capTokenQn)) ee
        cframe = WithCapC unitConst
        frame = CapInvokeC env () cframe Mt
        value = VCapToken capTokenFqn
        handler = CEKNoHandler
    pure (signedEs, es, frame, handler, value)

gasCapInvokeCApplyMgrFun :: PactDb CoreBuiltin () -> C.Benchmark
gasCapInvokeCApplyMgrFun pdb =
  benchApplyContWithRollback mkEnv "ApplyMgrFunC"
  where
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
        ps = _eeDefPactStep ee
        env = CEKEnv { _cePactDb=pdb
                    , _ceLocal=mempty
                    , _ceInCap=False
                    , _ceDefPactStep=ps
                    , _ceBuiltins=benchmarkEnv }
        capTokenFqn = CapToken (mkGasModelFqn gmDcapManagedName) [PInteger 1]
        capTokenQnFiltered = CapToken (QualifiedName gmDcapManagedName gmModuleName) []
        capTokenQn = CapToken (QualifiedName gmDcapManagedName gmModuleName) [PInteger 1]
        -- Insert the managed cap into the the environment
        clo = intClosureBinary 1 env
        mgrFunFqn = mkGasModelFqn gmManagerDfunName
        mgdCap = ManagedCap {_mcOriginalCap=capTokenQn, _mcManaged=ManagedParam mgrFunFqn (PInteger 0) 0, _mcCap=capTokenQnFiltered}
        signedEs = over eeMsgSigs (M.insert gmPublicKeyText1 (S.singleton capTokenQn)) ee
        esWithMgd = over (esCaps . csManaged) (S.insert mgdCap) es
        cframe = ApplyMgrFunC mgdCap clo (PInteger 1) (PInteger 1)
        frame = CapInvokeC env () cframe Mt
        value = VCapToken capTokenFqn
        handler = CEKNoHandler
    pure (signedEs, esWithMgd, frame, handler, value)

gasCapInvokeCUpdMgrFun :: PactDb CoreBuiltin () -> C.Benchmark
gasCapInvokeCUpdMgrFun pdb =
  benchApplyContWithRollback mkEnv "UpdMgrFunC"
  where
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
        ps = _eeDefPactStep ee
        env = CEKEnv { _cePactDb=pdb
                    , _ceLocal=mempty
                    , _ceInCap=False
                    , _ceDefPactStep=ps
                    , _ceBuiltins=benchmarkEnv }
        capTokenFqn = CapToken (mkGasModelFqn gmDcapManagedName) [PInteger 1]
        capTokenQnFiltered = CapToken (QualifiedName gmDcapManagedName gmModuleName) []
        capTokenQn = CapToken (QualifiedName gmDcapManagedName gmModuleName) [PInteger 1]
        -- Insert the managed cap into the the environment
        mgrFunFqn = mkGasModelFqn gmManagerDfunName
        mgdCap = ManagedCap {_mcOriginalCap=capTokenQn, _mcManaged=ManagedParam mgrFunFqn (PInteger 0) 0, _mcCap=capTokenQnFiltered}
        signedEs = over eeMsgSigs (M.insert gmPublicKeyText1 (S.singleton capTokenQn)) ee
        esWithMgd = over (esCaps . csManaged) (S.insert mgdCap) es
        cframe = UpdateMgrFunC mgdCap
        frame = CapInvokeC env () cframe Mt
        value = VCapToken capTokenFqn
        handler = CEKNoHandler
    pure (signedEs, esWithMgd, frame, handler, value)

gasCapBodyC :: PactDb CoreBuiltin () -> C.Benchmark
gasCapBodyC pdb =
  benchApplyContWithRollback mkEnv "CapBodyC"
  where
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
        ps = _eeDefPactStep ee
        env = CEKEnv { _cePactDb=pdb
                    , _ceLocal=mempty
                    , _ceInCap=False
                    , _ceDefPactStep=ps
                    , _ceBuiltins=benchmarkEnv }

        value = VUnit
        cbState = CapBodyState PopCapInvoke Nothing Nothing unitConst
        frame = CapBodyC env () cbState Mt
        handler = CEKNoHandler
    pure (ee, es, frame, handler, value)

gasCapPopCInvoke :: PactDb CoreBuiltin () -> C.Benchmark
gasCapPopCInvoke pdb =
  benchApplyContWithRollback mkEnv "CapPopCInvoke "
  where
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
        ps = _eeDefPactStep ee
        env = CEKEnv { _cePactDb=pdb
                    , _ceLocal=mempty
                    , _ceInCap=False
                    , _ceDefPactStep=ps
                    , _ceBuiltins=benchmarkEnv }
        capTokenQn = CapToken (QualifiedName gmDcapManagedName gmModuleName) [PInteger 1]
        es' = over (esCaps . csSlots) (CapSlot capTokenQn [] :) es
        cbState = CapBodyState PopCapInvoke Nothing Nothing unitConst
        frame = CapBodyC env () cbState Mt
        handler = CEKNoHandler
        value = VUnit
    pure (ee, es', frame, handler, value)

gasCapPopCComposed :: PactDb CoreBuiltin () -> C.Benchmark
gasCapPopCComposed pdb =
  benchApplyContWithRollback mkEnv "CapPopCComposed"
  where
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
        ps = _eeDefPactStep ee
        env = CEKEnv { _cePactDb=pdb
                    , _ceLocal=mempty
                    , _ceInCap=False
                    , _ceDefPactStep=ps
                    , _ceBuiltins=benchmarkEnv }
        capTokenQn = CapToken (QualifiedName gmDcapManagedName gmModuleName) [PInteger 1]
        es' = over (esCaps . csSlots) (CapSlot capTokenQn [] :) es
        cbState = CapBodyState PopCapInvoke Nothing Nothing unitConst
        frame = CapBodyC env () cbState Mt
        handler = CEKNoHandler
        value = VUnit
    pure (ee, es', frame, handler, value)


gasIgnoreValueC :: PactDb CoreBuiltin () -> C.Benchmark
gasIgnoreValueC pdb =
  benchApplyContWithRollback mkEnv "IgnoreValueC"
  where
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
        frame = IgnoreValueC PUnit Mt
        handler = CEKNoHandler
        value = VUnit
    pure (ee, es, frame, handler, value)

gasStackPopC :: PactDb CoreBuiltin () -> C.Benchmark
gasStackPopC pdb =
  benchApplyContWithRollback mkEnv "StackPopC"
  where
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
        frame = IgnoreValueC PUnit Mt
        handler = CEKNoHandler
        value = VUnit
    pure (ee, es, frame, handler, value)

gasModuleAdminC :: PactDb CoreBuiltin () -> C.Benchmark
gasModuleAdminC pdb =
  benchApplyContWithRollback mkEnv "ModuleAdminC"
  where
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
        frame = ModuleAdminC gmModuleName Mt
        handler = CEKNoHandler
        value = VUnit
    pure (ee, es, frame, handler, value)

gasEnforceBoolC :: PactDb CoreBuiltin () -> C.Benchmark
gasEnforceBoolC pdb =
  benchApplyContWithRollback mkEnv "EnforceBoolC"
  where
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
        frame = EnforceBoolC () Mt
        handler = CEKNoHandler
        value = VBool True
    pure (ee, es, frame, handler, value)

gasEnforcePactValueC :: PactDb CoreBuiltin () -> C.Benchmark
gasEnforcePactValueC pdb =
  benchApplyContWithRollback mkEnv "EnforcePactValueC"
  where
  mkEnv = do
    ee <- defaultGasEvalEnv pdb
    let es = defaultGasEvalState
        frame = EnforcePactValueC () Mt
        handler = CEKNoHandler
        value = VBool True
    pure (ee, es, frame, handler, value)


benchApplyContToValue
  :: IO ApplyContToVEnv
  -> String
  -> C.Benchmark
benchApplyContToValue mkEnv title =
  C.env mkEnv $ \ ~(ee, es, frame, handler, value) ->
    C.bench title $ C.nfAppIO (runEvalDropState ee es . Eval.applyContToValueSmallStep frame handler) value

-- Note: we preform a rollback, then begin tx right after.
-- This prevents us from say, inserting a value twice and changing
-- the asymptotics of the benchmark. Generally, for db inserts, we want to
-- benchmark the state of the world in the same way every time.
benchApplyContWithRollback
  :: IO ApplyContToVEnv
  -> String
  -> C.Benchmark
benchApplyContWithRollback mkEnv title =
  C.bench title $ C.perRunEnvWithCleanup mkEnv cleanup $ \ ~(ee, es, frame, handler, value) ->
    runEvalDropState ee es $ Eval.applyContToValueSmallStep frame handler value
  where
  cleanup (ee, _, _, _, _) = do
    let pdb = _eePactDb ee
    _pdbRollbackTx pdb
    _ <- _pdbBeginTx pdb Transactional
    pure ()


gasContType :: PactDb CoreBuiltin () -> ContType -> C.Benchmark
gasContType pdb = \case
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
  CTListC -> C.bgroup "list benchmarks" [] -- TODO
  -- Conditionals
  CTAndC ->
    C.bgroup "AndC Cases" $ (gasAndC pdb) <$> [minBound .. maxBound]
  CTOrC ->
    C.bgroup "OrC Cases" $ (gasOrC pdb) <$> [minBound .. maxBound]
  CTIfC ->
    C.bgroup "IfC Cases" $
      [ gasIfC pdb b
      | b <- [False, True] ]
  CTEnforceC ->
    C.bgroup "EnforceC Cases" $ (gasEnforceC pdb) <$> [minBound .. maxBound]
  CTEnforceOneC -> C.bgroup "enforce one benchmarks" [] -- TODO
  CTFilterC ->
    C.bgroup "FilterC Cases" $
      [gasFilterCEmptyNElems pdb b 10
      | b <- [False, True] ]
  CTAndQC ->
    C.bgroup "AndQC Cases" $
      [ gasAndQC pdb b
      | b <- [False, True] ]
  CTOrQC ->
    C.bgroup "OrQC Cases" $
      [ gasOrQC pdb b
      | b <- [False, True] ]
  CTNotQC ->
    gasNotQC pdb
  -- Builtin forms
  CTMapC ->
    gasMapC pdb
  CTFoldC ->
    gasFoldC pdb
  CTZipC ->
    gasZipC pdb
  CTPreSelectC ->
    gasPreSelectC pdb
  CTPreFoldDbC ->
    gasPreFoldDbC pdb
  CTSelectC ->
    gasSelectC pdb
  CTFoldDbFilterC ->
    gasFoldDbFilterC pdb
  CTFoldDbMapC ->
    gasFoldDbMapC pdb
  CTReadC ->
    gasReadC pdb
  CTWriteC ->
    gasWriteC pdb
  CTWithDefaultReadC ->
    gasWithDefaultReadC pdb
  CTKeysC ->
    gasKeysC pdb
  CTTxIdsC ->
    gasTxIdsC pdb
  CTTxLogC ->
    gasTxLogC pdb
  CTKeyLogC ->
    gasKeyLogC pdb
  CTCreateTableC ->
    gasCreateTableC pdb
  CTEmitEventC ->
    gasEmitEventC pdb
  CTDefineNamespaceC ->
    gasDefineNamespaceC pdb
  CTDefineKeysetC ->
    gasDefineKeysetC pdb
  CTObjC ->
    gasObjC pdb
  CTCapInvokeC ->
    C.bgroup "CapInvokeC"
      [ gasCapInvokeCUserGuard pdb
      , gasCapInvokeCWithCapC pdb
      , gasCapInvokeCWithCapAutoManaged pdb
      , gasCapInvokeCWithCapCManaged pdb
      , gasCapInvokeCApplyMgrFun pdb
      , gasCapInvokeCUpdMgrFun pdb]
  CTCapBodyC ->
    gasCapBodyC pdb
  CTCapPopC ->
    C.bgroup "CapBodyC" [gasCapPopCInvoke pdb, gasCapPopCComposed pdb ]
  CTDefPactStepC ->
    -- Todo: defpactstep benchmarks
    C.bgroup "DefPactStepC" []
  CTNestedDefPactStepC ->
    -- Todo: nesteddefpactstepC benchmarks
    C.bgroup "NestedDefPactStepC" []
  CTIgnoreValueC -> gasIgnoreValueC pdb
  CTEnforceBoolC -> gasEnforceBoolC pdb
  CTEnforcePactValueC ->
    gasEnforcePactValueC pdb
  CTModuleAdminC ->
    gasModuleAdminC pdb
  CTStackPopC -> gasStackPopC pdb
  CTEnforceErrorC ->
    gasEnforceBoolC pdb
  CTMt -> C.bgroup "Mt"
    [ gasMtReturnNoHandler pdb
    , gasMtWithHandlerError pdb
    , gasMtWithHandlerValue pdb]


