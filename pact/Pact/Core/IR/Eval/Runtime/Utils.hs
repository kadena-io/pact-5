{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}

module Pact.Core.IR.Eval.Runtime.Utils
 ( checkSigCaps
 , lookupFqName
 , getDefCap
 , getDefun
 , typecheckArgument
 , maybeTCType
 , safeTail
 , asString
 , asBool
 , throwExecutionError
 , throwExecutionError'
 , findCallingModule
 , getCallingModule
 , calledByModule
 , failInvariant
 , isExecutionFlagSet
 , checkNonLocalAllowed
 , evalStateToErrorState
 , restoreFromErrorState
 , getDefPactId
 , tvToDomain
 , unsafeUpdateManagedParam
 , chargeFlatNativeGas
 , chargeGasArgs
 , getGas
 , putGas
 , litCmpGassed
 , valEqGassed
 , enforceBlessedHashes
 ) where

import Control.Lens
import Control.Monad(when)
import Control.Monad.IO.Class
import Data.IORef
import Data.Foldable(find, toList)
import Data.Maybe(listToMaybe)
import Data.Text(Text)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Pact.Core.Names
import Pact.Core.PactValue
import Pact.Core.Builtin
import Pact.Core.IR.Term
import Pact.Core.ModRefs
import Pact.Core.Type
import Pact.Core.Errors
import Pact.Core.IR.Eval.Runtime.Types
import Pact.Core.Literal
import Pact.Core.Persistence
import Pact.Core.Environment
import Pact.Core.DefPacts.Types
import Pact.Core.Gas
import Pact.Core.Guards
import Pact.Core.Capabilities
import Pact.Core.Hash


lookupFqName :: (MonadEval b i m) => FullyQualifiedName -> m (Maybe (EvalDef b i))
lookupFqName fqn =
  views (esLoaded.loAllLoaded) (M.lookup fqn) <$> getEvalState

getDefCap :: (MonadEval b i m) => i -> FullyQualifiedName -> m (EvalDefCap b i)
getDefCap info fqn = lookupFqName fqn >>= \case
  Just (DCap d) -> pure d
  Just _ -> failInvariant info "Expected DefCap"
  _ -> failInvariant info "Expected DefCap; got nothing"

getDefun :: (MonadEval b i m) => i -> FullyQualifiedName -> m (EvalDefun b i)
getDefun info fqn = lookupFqName fqn >>= \case
  Just (Dfun d) -> pure d
  _ -> failInvariant info "Expected Defun"

unsafeUpdateManagedParam :: v -> ManagedCap name v -> ManagedCap name v
unsafeUpdateManagedParam newV (ManagedCap mc orig (ManagedParam fqn _oldV i)) =
  ManagedCap mc orig (ManagedParam fqn newV i)
unsafeUpdateManagedParam _ a = a

typecheckArgument :: (MonadEval b i m) => i -> PactValue -> Type -> m PactValue
typecheckArgument info pv ty = case (pv, checkPvType ty pv) of
  (PModRef mr, Just (TyModRef m))
    | _mrRefined mr == Nothing -> pure (PModRef (mr & mrRefined ?~ m))
    | otherwise -> pure (PModRef mr)
  (_, Just _) -> pure pv
  (_, Nothing) ->
    throwExecutionError info (RunTimeTypecheckFailure (pvToArgTypeError pv) ty)

maybeTCType :: (MonadEval b i m) => i -> PactValue -> Maybe Type -> m PactValue
maybeTCType i pv = maybe (pure pv) (typecheckArgument i pv)

pvToArgTypeError :: PactValue -> ArgTypeError
pvToArgTypeError = \case
  PLiteral l -> ATEPrim (literalPrim l)
  PTime _ -> ATEPrim PrimTime
  PList _ -> ATEList
  PObject _ -> ATEObject
  PGuard _ -> ATEPrim PrimGuard
  PModRef _ -> ATEModRef
  PCapToken _ -> ATEClosure

findCallingModule :: (MonadEval b i m) => m (Maybe ModuleName)
findCallingModule = do
  stack <- useEvalState esStack
  pure $ listToMaybe $ fmap (_fqModule . _sfName) stack

calledByModule
  :: (MonadEval b i m)
  => ModuleName
  -> m Bool
calledByModule mn = do
  stack <- useEvalState esStack
  case find (\sf -> (_fqModule . _sfName) sf == mn) stack of
    Just _ -> pure True
    Nothing -> pure False

-- | Throw an invariant failure, that is
-- an error which we do not expect to see during regular pact
-- execution. If this case is ever hit, we have a problem with
-- some invalid state in interpretation
failInvariant :: MonadEval b i m => i -> Text -> m a
failInvariant i reason =
  throwExecutionError i (InvariantFailure reason)

-- Todo: MaybeT cleans this up
getCallingModule :: (MonadEval b i m) => i -> m (EvalModule b i)
getCallingModule info = findCallingModule >>= \case
  Just mn -> useEvalState (esLoaded . loModules . at mn) >>= \case
    Just (ModuleData m _) -> pure m
    Just (InterfaceData _m _) ->
      failInvariant info "getCallingModule points to interface"
    Nothing ->
      failInvariant info "getCallingModule points to no loaded module"
  Nothing -> failInvariant info "Error: No Module in stack"

safeTail :: [a] -> [a]
safeTail (_:xs) = xs
safeTail [] = []

isExecutionFlagSet :: (MonadEval b i m) => ExecutionFlag -> m Bool
isExecutionFlagSet flag = viewsEvalEnv eeFlags (S.member flag)

evalStateToErrorState :: EvalState b i -> ErrorState i
evalStateToErrorState es =
  ErrorState (_esCaps es) (_esStack es) (_esCheckRecursion es)

restoreFromErrorState :: ErrorState i -> EvalState b i -> EvalState b i
restoreFromErrorState (ErrorState caps stack recur) =
  set esCaps caps . set esStack stack . set esCheckRecursion recur

checkNonLocalAllowed :: (MonadEval b i m) => i -> m ()
checkNonLocalAllowed info = do
  disabledInTx <- isExecutionFlagSet FlagDisableHistoryInTransactionalMode
  mode <- viewEvalEnv eeMode
  when (mode == Transactional && disabledInTx) $ failInvariant info
    "Operation only permitted in local execution mode"

{-# SPECIALIZE asString
   :: ()
   -> CoreBuiltin
   -> PactValue
   -> Eval Text
    #-}
asString
  :: (MonadEval b i m)
  => i
  -> b
  -> PactValue
  -> m Text
asString _ _ (PLiteral (LString b)) = pure b
asString info b pv =
  throwExecutionError info (NativeArgumentsError (builtinName b) [pvToArgTypeError pv])

{-# SPECIALIZE asBool
   :: ()
   -> CoreBuiltin
   -> PactValue
   -> Eval Bool
    #-}
asBool
  :: (MonadEval b i m)
  => i
  -> b
  -> PactValue
  -> m Bool
asBool _ _ (PLiteral (LBool b)) = pure b
asBool info b pv =
  throwExecutionError info (NativeArgumentsError (builtinName b) [pvToArgTypeError pv])



getDefPactId :: (MonadEval b i m) => i -> m DefPactId
getDefPactId info =
  useEvalState esDefPactExec >>= \case
    Just pe -> pure (_peDefPactId pe)
    Nothing ->
      throwExecutionError info NotInDefPactExecution

tvToDomain :: TableValue -> Domain RowKey RowData b i
tvToDomain tv =
  DUserTables (_tvName tv)

{-# SPECIALIZE chargeGasArgs
   :: ()
   -> GasArgs
   -> Eval ()
    #-}
chargeGasArgs :: (MonadEval b i m) => i -> GasArgs -> m ()
chargeGasArgs info ga = do
  model <- viewEvalEnv eeGasModel
  !currGas <- getGas
  let limit@(MilliGasLimit gasLimit) = _gmGasLimit model
      !g1 = _gmRunModel model ga
      !gUsed = currGas <> g1
  esGasLog %== fmap (GasLogEntry (Left ga) gUsed g1 :)
  putGas gUsed
  when (gUsed > gasLimit) $
    throwExecutionError info (GasExceeded limit gUsed)

{-# SPECIALIZE chargeFlatNativeGas
   :: ()
   -> CoreBuiltin
   -> Eval ()
    #-}
chargeFlatNativeGas :: (MonadEval b i m) => i -> b -> m ()
chargeFlatNativeGas info nativeArg = do
  model <- viewEvalEnv eeGasModel
  !currGas <- getGas
  let limit@(MilliGasLimit gasLimit) = _gmGasLimit model
      !g1 = _gmNatives model nativeArg
      !gUsed = currGas <> g1
  esGasLog %== fmap (GasLogEntry (Right nativeArg) gUsed g1 :)
  putGas gUsed
  when (gUsed > gasLimit && gasLimit >= currGas) $
    throwExecutionError info (GasExceeded limit gUsed)


getGas :: (MonadEval b i m) => m MilliGas
getGas =
  viewEvalEnv eeGasRef >>= liftIO . readIORef
{-# SPECIALIZE getGas
    :: Eval MilliGas
    #-}
{-# INLINE getGas #-}

putGas :: (MonadEval b i m) => MilliGas -> m ()
putGas !g = do
  gasRef <- viewEvalEnv eeGasRef
  liftIO (writeIORef gasRef g)
{-# INLINE putGas #-}
{-# SPECIALIZE putGas
    :: MilliGas -> Eval ()
    #-}

litCmpGassed :: (MonadEval b i m) => i -> Literal -> Literal -> m (Maybe Ordering)
litCmpGassed info = cmp
  where
  cmp (LInteger l) (LInteger r) = do
    chargeGasArgs info (GComparison (IntComparison l r))
    pure $ Just $ compare l r
  cmp (LBool l) (LBool r) = pure $ Just $ compare l r
  cmp (LDecimal l) (LDecimal r) = do
    chargeGasArgs info (GComparison (DecimalComparison l r))
    pure $ Just $ compare l r
  cmp (LString l) (LString r) = do
    chargeGasArgs info (GComparison (TextComparison l))
    pure $ Just $ compare l r
  cmp LUnit LUnit = pure $ Just EQ
  cmp _ _ = pure Nothing
{-# SPECIALIZE litCmpGassed
    :: () -> Literal -> Literal -> Eval (Maybe Ordering)
    #-}

valEqGassed :: (MonadEval b i m) => i -> PactValue -> PactValue -> m Bool
valEqGassed info = go
  where
  go (PLiteral l1) (PLiteral l2) = litCmpGassed info l1 l2 >>= \case
    Just EQ -> pure True
    _ -> pure False
  go (PList vs1) (PList vs2)
    | length vs1 == length vs2 = do
      chargeGasArgs info (GComparison (ListComparison $ length vs1))
      goList (toList vs1) (toList vs2)
  go (PGuard g1) (PGuard g2) = goGuard g1 g2
  go (PObject o1) (PObject o2)
    | length o1 == length o2 = do
      chargeGasArgs info (GComparison (ObjComparison $ length o1))
      if M.keys o1 == M.keys o2
         then goList (toList o1) (toList o2)
         else pure False
  go (PModRef mr1) (PModRef mr2) = pure $ mr1 == mr2
  go (PCapToken (CapToken n1 args1)) (PCapToken (CapToken n2 args2))
    | n1 == n2 && length args1 == length args2 = do
      chargeGasArgs info (GComparison (ListComparison $ length args1))
      goList args1 args2
  go (PTime t1) (PTime t2) = pure $ t1 == t2
  go _ _ = pure False

  goList [] [] = pure True
  goList (x:xs) (y:ys) = do
    r <- x `go` y
    if r then goList xs ys else pure False
  goList _ _ = pure False

  goGuard (GKeyset ks1) (GKeyset ks2) = pure $ ks1 == ks2
  goGuard (GKeySetRef ksn1) (GKeySetRef ksn2) = pure $ ksn1 == ksn2
  goGuard (GUserGuard (UserGuard f1 args1)) (GUserGuard (UserGuard f2 args2))
    | f1 == f2 && length args1 == length args2 = do
      chargeGasArgs info (GComparison (ListComparison $ length args1))
      goList args1 args2
  goGuard (GCapabilityGuard (CapabilityGuard n1 args1 pid1)) (GCapabilityGuard (CapabilityGuard n2 args2 pid2))
    | n1 == n2 && pid1 == pid2 && length args1 == length args2 = do
      chargeGasArgs info (GComparison (ListComparison $ length args1))
      goList args1 args2
  goGuard (GModuleGuard g1) (GModuleGuard g2) = pure $ g1 == g2
  goGuard (GDefPactGuard g1) (GDefPactGuard g2) = pure $ g1 == g2
  goGuard _ _ = pure False
{-# SPECIALIZE valEqGassed
    :: () -> PactValue -> PactValue -> Eval Bool
    #-}

enforceBlessedHashes :: (MonadEval b i m) => i -> EvalModule b i -> ModuleHash -> m ()
enforceBlessedHashes info md mh
  | _mHash md == mh = return ()
  | mh `S.member` _mBlessed md = return ()
  | otherwise = throwExecutionError info (HashNotBlessed (_mName md) mh)
