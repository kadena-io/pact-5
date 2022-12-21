{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE InstanceSigs #-}


module Pact.Core.Untyped.Eval.CEK.SmallStep where

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
-- import Pact.Core.Gas
import Pact.Core.Literal

import Pact.Core.Untyped.Term
import Pact.Core.Untyped.Eval.CEK.SmallRuntime

pattern REvalValue :: CEKValue b i m -> Either a (EvalResult b i m)
pattern REvalValue v = Right (EvalValue v)

evalCEK :: forall b i m. (MonadEval b i m)
  => Cont b i m
  -> CEKErrorHandler b i m
  -> CEKEnv b i m
  -> Term Name b i
  -> m (Either (Term Name b i, CEKEnv b i m) (EvalResult b i m), Cont b i m, CEKErrorHandler b i m)
evalCEK cont handler env (Var n info) = do
  case _nKind n of
    NBound i -> case RAList.lookup env i of
      Just v -> pure (REvalValue v, cont, handler)
      Nothing -> failInvariant' ("unbound identifier" <> T.pack (show n)) info
      -- returnCEK cont (env RAList.!! i)
    -- Top level names are not closures, so we wipe the env
    NTopLevel mname mh -> do
      let fqn = FullyQualifiedName mname (_nName n) mh
      cekReadEnv >>= \renv -> case views cekLoaded (Map.lookup fqn) renv of
        Just d ->
          pure (Left (defTerm d, mempty), cont, handler)
          -- evalCEK cont handler RAList.Nil (defTerm d)
        Nothing -> failInvariant' ("top level name " <> T.pack (show fqn) <> " not in scope") info
evalCEK cont handler _env (Constant l _) =
  pure (REvalValue (VLiteral l), cont, handler)
-- What to do here???
evalCEK cont handler env (App fn arg _) =
  pure (Left (fn, env), Arg env arg cont, handler)
evalCEK cont handler env (Lam body _) =
  pure (REvalValue (VClosure body env), cont, handler)
evalCEK cont handler _ (Builtin b _) = do
  builtins <- view cekBuiltins <$> cekReadEnv
  pure (REvalValue (VNative (builtins b)), cont, handler)
evalCEK cont handler env (Sequence e1 e2 _) =
  pure (Left (e1, env), SeqC env e2 cont, handler)
evalCEK cont handler env (Conditional c _) = case c of
  CAnd te te' ->
    pure (Left (te, env), CondC env (AndFrame te') cont, handler)
  COr te te' ->
    pure (Left (te, env), CondC env (OrFrame te') cont, handler)
  CIf cond e1 e2 ->
    pure (Left (cond, env), CondC env (IfFrame e1 e2) cont, handler)
evalCEK cont handler env (ListLit ts _) = case ts of
  [] -> pure (REvalValue (VList mempty), cont, handler)
  (x:xs) -> pure (Left (x, env), ListC env xs [] cont, handler)
evalCEK cont handler env (Try e1 rest _) =
  let handler' = CEKHandler env e1 cont handler
  in pure (Left (rest, env), Mt, handler')
evalCEK _ handler _ (Error e _) =
  pure (Right (VError e), Mt, handler)

evalUntilDone
  :: MonadEval b i m
  => Term Name b i
  -> m (EvalResult b i m)
evalUntilDone term =
  evalCEK Mt CEKNoHandler mempty term >>= loop
  where
  loop (Right v, Mt, CEKNoHandler) = pure v
  loop (Right v, cont, handler) = evalCEKResult cont handler v >>= loop
  loop (Left (te, env), cont, handler) = evalCEK cont handler env te >>= loop

evalCEKResult
  :: MonadEval b i m
  => Cont b i m
  -> CEKErrorHandler b i m
  -> EvalResult b i m
  -> m (Either (Term Name b i, CEKEnv b i m) (EvalResult b i m),
        Cont b i m, CEKErrorHandler b i m)
evalCEKResult Mt handler v = case handler of
  CEKNoHandler -> pure (Right v, Mt, CEKNoHandler)
  CEKHandler env term cont' handler' -> case v of
    VError{} -> evalCEK cont' handler' env term
    EvalValue v' -> evalCEKValue cont' handler' v'
evalCEKResult cont handler v = case v of
  VError {} -> pure (Right v, Mt, handler)
  EvalValue v' -> evalCEKValue cont handler v'

evalCEKValue
  :: (MonadEval b i m)
  => Cont b i m
  -> CEKErrorHandler b i m
  -> CEKValue b i m
  -> m (Either (Term Name b i, CEKEnv b i m) (EvalResult b i m),
        Cont b i m, CEKErrorHandler b i m)
evalCEKValue Mt handler v =
  case handler of
    CEKNoHandler -> pure (Right (EvalValue v), Mt, CEKNoHandler)
    CEKHandler _env _term cont' handler' ->
      evalCEKValue cont' handler' v
      -- pure (Right (EvalValue v), cont', handler')
evalCEKValue (Arg env arg cont) handler fn =
  evalCEK (Fn fn cont) handler env arg
evalCEKValue (Fn fn cont) handler arg =
  applyLam fn arg cont handler
evalCEKValue (SeqC env e cont) handler _ =
  evalCEK cont handler env e
evalCEKValue (CondC env frame cont) handler v = case v of
  (VLiteral (LBool b)) -> case frame of
    AndFrame te ->
      if b then evalCEK cont handler env te
      else evalCEKValue cont handler v
    OrFrame te ->
      if b then evalCEKValue cont handler v
      else evalCEK cont handler env te
    IfFrame ifExpr elseExpr ->
      if b then evalCEK cont handler env ifExpr
      else evalCEK cont handler env elseExpr
  _ -> failInvariant "Evaluation of conditional expression yielded non-boolean value"
evalCEKValue (ListC env args vals cont) handler v = do
  case args of
    [] ->
      evalCEKValue cont handler (VList (V.fromList (reverse (v:vals))))
    e:es ->
      evalCEK (ListC env es (v:vals) cont) handler env e

failInvariant :: MonadEval b i m => Text -> m a
failInvariant b =
  let e = PEExecutionError (InvariantFailure b) def
  in throwError e

failInvariant' :: MonadEval b i m => Text -> i -> m a
failInvariant' b i =
  let e = PEExecutionError (InvariantFailure b) i
  in throwError e

throwExecutionError' :: (MonadEval b i m) => ExecutionError -> m a
throwExecutionError' e = throwError (PEExecutionError e def)

applyLam
  :: (MonadEval b i m)
  => CEKValue b i m
  -> CEKValue b i m
  -> Cont b i m
  -> CEKErrorHandler b i m
  -> m (Either (Term Name b i, CEKEnv b i m) (EvalResult b i m),
        Cont b i m, CEKErrorHandler b i m)
applyLam (VClosure body env) arg cont handler =
  evalCEK cont handler (RAList.cons arg env) body
applyLam (VNative (BuiltinFn b fn arity args)) arg cont handler
  | arity - 1 == 0 = do
    -- chargeNative b
    over _1 Right <$> fn cont handler (reverse (arg:args))
  | otherwise =
      pure (REvalValue (VNative (BuiltinFn b fn (arity - 1) (arg:args))), cont, handler)
applyLam _ _ _ _ = failInvariant' "Applying value to non-function" def
