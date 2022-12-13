{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ConstraintKinds #-}

-- |
-- Module      :  Pact.Core.IR.Typecheck
-- Copyright   :  (C) 2022 Kadena
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jose Cardona <jose@kadena.io>
--
-- CEK Evaluator for untyped core.
--

module Pact.Core.Untyped.Eval.CEK
 ( eval
 , evalCEK
 , returnCEK
 , returnCEKValue
 , failInvariant
 , throwExecutionError'
 , unsafeApplyOne
 , unsafeApplyTwo
 ) where

import Control.Lens
import Control.Monad.Except
-- import Control.Monad(when)
-- import Control.Monad.Catch
-- import Control.Monad.IO.Class
-- import Data.IORef
import Data.Default
import Data.Text(Text)
import qualified Data.Map.Strict as Map
import qualified Data.RAList as RAList
import qualified Data.Text as T
import qualified Data.Vector as V

import Pact.Core.Builtin
import Pact.Core.Names
import Pact.Core.Errors
import Pact.Core.Gas
import Pact.Core.Literal

import Pact.Core.Untyped.Term
import Pact.Core.Untyped.Eval.Runtime

-- chargeGas :: MonadCEK b i m => Gas -> m ()
-- chargeGas g = do
  -- ref <- view cekGas
  -- gCurr <- liftIO (readIORef ref)
  -- gLimit <- view (cekGasModel . geGasLimit)
  -- let gUsed = g + gCurr
  --     msg = "Gas Limit (" <> T.pack (show gLimit) <> ") exceeeded: " <> T.pack (show gUsed)
  -- when (gUsed > gLimit) $ throwM (GasExceeded msg)

chargeNodeGas :: MonadCEK b i m => NodeType -> m ()
chargeNodeGas nt = do
  gm <- view (cekGasModel . geGasModel . gmNodes) <$> cekReadEnv
  cekChargeGas (gm nt)
  -- gm <- view (cekGasModel . geGasModel . gmNodes)
  -- chargeGas (gm nt)

chargeNative :: MonadCEK b i m => b -> m ()
chargeNative native = do
  gm <- view (cekGasModel . geGasModel . gmNatives) <$> cekReadEnv
  cekChargeGas (gm native)
  -- gm <- view (cekGasModel . geGasModel . gmNatives)
  -- chargeGas (gm native)

-- Todo: exception handling? do we want labels
-- Todo: `traverse` usage should be perf tested.
-- It might be worth making `Arg` frames incremental, as opposed to a traverse call
eval
  :: forall b i m. (MonadCEK b i m)
  => CEKEnv b i m
  -> EvalTerm b i
  -> m (EvalResult b i m)
eval = evalCEK Mt CEKNoHandler

evalCEK
  :: (MonadCEK b i m)
  => Cont b i m
  -> CEKErrorHandler b i m
  -> CEKEnv b i m
  -> EvalTerm b i
  -> m (EvalResult b i m)
evalCEK cont handler env (Var n info)  = do
  chargeNodeGas VarNode
  case _nKind n of
    NBound i -> case RAList.lookup env i of
      Just v -> returnCEKValue cont handler v
      Nothing -> failInvariant' ("unbound identifier" <> T.pack (show n)) info
      -- returnCEK cont (env RAList.!! i)
    -- Top level names are not closures, so we wipe the env
    NTopLevel mname mh -> do
      let fqn = FullyQualifiedName mname (_nName n) mh
      cekReadEnv >>= \renv -> case views cekLoaded (Map.lookup fqn) renv of
        Just d -> evalCEK cont handler RAList.Nil (defTerm d)
        Nothing -> failInvariant' ("top level name " <> T.pack (show fqn) <> " not in scope") info
evalCEK cont handler _env (Constant l _) = do
  chargeNodeGas ConstantNode
  returnCEKValue cont handler (VLiteral l)
evalCEK cont handler env (App fn arg _) = do
  chargeNodeGas AppNode
  evalCEK (Arg env arg cont) handler env fn
evalCEK cont handler env (Lam body _) = do
  chargeNodeGas LamNode
  returnCEKValue cont handler (VClosure body env)
evalCEK cont handler _env (Builtin b _) = do
  chargeNodeGas BuiltinNode
  builtins <- view cekBuiltins <$> cekReadEnv
  returnCEKValue cont handler (VNative (builtins b))
evalCEK cont handler env (Sequence e1 e2 _) = do
  chargeNodeGas SeqNode
  evalCEK (SeqC env e2 cont) handler env e1
evalCEK cont handler env (Conditional c _) = case c of
  CAnd te te' ->
    evalCEK (CondC env (AndFrame te') cont) handler env te
  COr te te' ->
    evalCEK (CondC env (OrFrame te') cont) handler env te
  CIf cond e1 e2 ->
    evalCEK (CondC env (IfFrame e1 e2) cont) handler env cond
evalCEK cont handler env (ListLit ts _) = do
  chargeNodeGas ListNode
  case ts of
    [] -> returnCEKValue cont handler (VList mempty)
    x:xs -> evalCEK (ListC env xs [] cont) handler env x
evalCEK cont handler env (Try e1 rest _) = do
  let handler' = CEKHandler env e1 cont handler
  evalCEK Mt handler' env rest
-- Error terms ignore the current cont
evalCEK _ handler _ (Error e _) =
  returnCEK Mt handler (VError e)

returnCEK :: (MonadCEK b i m)
  => Cont b i m
  -> CEKErrorHandler b i m
  -> EvalResult b i m
  -> m (EvalResult b i m)
returnCEK Mt handler v =
  case handler of
    CEKNoHandler -> return v
    CEKHandler env term cont' handler' -> case v of
      VError{} -> evalCEK cont' handler' env term
      EvalValue v' -> returnCEKValue cont' handler' v'
returnCEK cont handler v = case v of
  VError{} -> returnCEK Mt handler v
  EvalValue v' -> returnCEKValue cont handler v'

returnCEKValue
  :: (MonadCEK b i m)
  => Cont b i m
  -> CEKErrorHandler b i m
  -> CEKValue b i m
  -> m (EvalResult b i m)
returnCEKValue Mt handler v =
  case handler of
    CEKNoHandler -> return (EvalValue v)
    CEKHandler _env _term cont' handler' -> returnCEKValue cont' handler' v
      -- VError{} -> evalCEK cont' handler' env term
      -- _ ->
-- Error terms that don't simply returnt the empty continuation
-- "Zero out" the continuation up to the latest handler
-- returnCEKValue _cont handler v@VError{} =
--   returnCEK Mt handler v
returnCEKValue (Arg env arg cont) handler fn =
  evalCEK (Fn fn cont) handler env arg
returnCEKValue (Fn fn cont) handler arg =
  applyLam fn arg cont handler
returnCEKValue (SeqC env e cont) handler _ =
  evalCEK cont handler env e
returnCEKValue (CondC env frame cont) handler v = case v of
  (VLiteral (LBool b)) -> case frame of
    AndFrame te ->
      if b then evalCEK cont handler env te
      else returnCEKValue cont handler v
    OrFrame te ->
      if b then returnCEKValue cont handler v
      else evalCEK cont handler env te
    IfFrame ifExpr elseExpr ->
      if b then evalCEK cont handler env ifExpr
      else evalCEK cont handler env elseExpr
  _ -> failInvariant "Evaluation of conditional expression yielded non-boolean value"
returnCEKValue (ListC env args vals cont) handler v = do
  case args of
    [] ->
      returnCEKValue cont handler (VList (V.fromList (reverse (v:vals))))
    e:es ->
      evalCEK (ListC env es (v:vals) cont) handler env e

applyLam
  :: (MonadCEK b i m)
  => CEKValue b i m
  -> CEKValue b i m
  -> Cont b i m
  -> CEKErrorHandler b i m
  -> m (EvalResult b i m)
applyLam (VClosure body env) arg cont handler =
  evalCEK cont handler (RAList.cons arg env) body
applyLam (VNative (BuiltinFn b fn arity args)) arg cont handler
  | arity - 1 == 0 = do
    chargeNative b
    fn cont handler (reverse (arg:args))
  | otherwise = returnCEKValue cont handler (VNative (BuiltinFn b fn (arity - 1) (arg:args)))
applyLam _ _ _ _ = failInvariant' "Applying value to non-function" def

-- runCEK
--   :: forall b i m. MonadCEK b i m
--   => CEKRuntimeEnv b i m
--   -- ^ runtime environment
--   -> EvalTerm b i
--   -- ^ Term to evaluate
--   -> m (CEKValue b i m)
-- runCEK env term =
--   runEvalT env (eval RAList.Nil term)

failInvariant :: MonadCEK b i m => Text -> m a
failInvariant b =
  let e = PEExecutionError (FatalExecutionError b) def
  in throwError e

failInvariant' :: MonadCEK b i m => Text -> i -> m a
failInvariant' b i =
  let e = PEExecutionError (FatalExecutionError b) i
  in throwError e

throwExecutionError' :: (MonadCEK b i m) => ExecutionError -> m a
throwExecutionError' e = throwError (PEExecutionError e def)

unsafeApplyOne
  :: MonadCEK b i m
  => CEKValue b i m
  -> CEKValue b i m
  -> m (EvalResult b i m)
unsafeApplyOne (VClosure body env) arg = eval (RAList.cons arg env) body
unsafeApplyOne (VNative (BuiltinFn b fn arity args)) arg =
  if arity - 1 <= 0 then fn Mt CEKNoHandler (reverse (arg:args))
  else pure (EvalValue (VNative (BuiltinFn b fn (arity - 1) (arg:args))))
unsafeApplyOne _ _ = failInvariant "Applied argument to non-closure in native"

unsafeApplyTwo
  :: MonadCEK b i m
  => CEKValue b i m
  -> CEKValue b i m
  -> CEKValue b i m
  -> m (EvalResult b i m)
unsafeApplyTwo (VClosure (Lam body _) env) arg1 arg2 =
  eval (RAList.cons arg2 (RAList.cons arg1 env)) body
unsafeApplyTwo (VNative (BuiltinFn b fn arity args)) arg1 arg2 =
  if arity - 2 <= 0 then fn Mt CEKNoHandler (reverse (arg1:arg2:args))
  else pure $ EvalValue $ VNative $ BuiltinFn b fn (arity - 2) (arg1:arg2:args)
unsafeApplyTwo _ _ _ = failInvariant "Applied argument to non-closure in native"
