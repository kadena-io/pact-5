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
 ( mkBuiltinFn
 , enforcePactValue
 , checkSigCaps
 , lookupFqName
 , getDefCap
 , getDefun
 , typecheckArgument
 , maybeTCType
 , safeTail
 , toArgTypeError
 , asString
 , asBool
 , throwExecutionError
 , throwExecutionError'
 , argsError
 , findCallingModule
 , getCallingModule
 , readOnlyEnv
 , sysOnlyEnv
 , calledByModule
 , failInvariant
 , isExecutionFlagSet
 , checkNonLocalAllowed
 , evalStateToErrorState
 , restoreFromErrorState
 , getDefPactId
 , tvToDomain
 , envFromPurity
 , unsafeUpdateManagedParam
 , chargeFlatNativeGas
 , chargeGasArgs
 , getGas
 , putGas
 ) where

import Control.Lens
import Control.Monad(when)
import Control.Monad.IO.Class
import Control.Monad.Except(MonadError(..))
import Data.IORef
import Data.Text(Text)
import Data.Maybe(listToMaybe)
import Data.Foldable(find)
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

mkBuiltinFn
  :: (IsBuiltin b)
  => i
  -> b
  -> CEKEnv step b i m
  -> NativeFunction step b i m
  -> NativeFn step b i m
mkBuiltinFn i b env fn =
  NativeFn b env fn (builtinArity b) i
{-# INLINE mkBuiltinFn #-}

enforcePactValue :: (MonadEval b i m) => i -> CEKValue step b i m -> m PactValue
enforcePactValue info = \case
  VPactValue pv -> pure pv
  _ -> throwExecutionError info ExpectedPactValue

lookupFqName :: (MonadEval b i m) => FullyQualifiedName -> m (Maybe (EvalDef b i))
lookupFqName fqn =
  views (esLoaded.loAllLoaded) (M.lookup fqn) <$> getEvalState

getDefCap :: (MonadEval b i m) => i -> FullyQualifiedName -> m (EvalDefCap b i)
getDefCap info fqn = lookupFqName fqn >>= \case
  Just (DCap d) -> pure d
  _ -> failInvariant info "Expected DefCap"

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
    throwExecutionError info (RunTimeTypecheckFailure (toArgTypeError (VPactValue pv)) ty)

maybeTCType :: (MonadEval b i m) => i -> PactValue -> Maybe Type -> m PactValue
maybeTCType i pv = maybe (pure pv) (typecheckArgument i pv)

findCallingModule :: (MonadEval b i m) => m (Maybe ModuleName)
findCallingModule = do
  stack <- useEvalState esStack
  pure $ listToMaybe $ fmap _sfModule stack

calledByModule
  :: (MonadEval b i m)
  => ModuleName
  -> m Bool
calledByModule mn = do
  stack <- useEvalState esStack
  case find (\sf -> _sfModule sf == mn) stack of
    Just _ -> pure True
    Nothing -> pure False

failInvariant :: MonadEval b i m => i -> Text -> m a
failInvariant i b =
  let e = PEExecutionError (InvariantFailure b) i
  in throwError e

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

evalStateToErrorState :: EvalState b i -> ErrorState
evalStateToErrorState es =
  ErrorState (_esCaps es) (_esStack es)

restoreFromErrorState :: ErrorState -> EvalState b i -> EvalState b i
restoreFromErrorState (ErrorState caps stack) =
  set esCaps caps . set esStack stack

checkNonLocalAllowed :: (MonadEval b i m) => i -> m ()
checkNonLocalAllowed info = do
  disabledInTx <- isExecutionFlagSet FlagDisableHistoryInTransactionalMode
  mode <- viewEvalEnv eeMode
  when (mode == Transactional && disabledInTx) $ failInvariant info
    "Operation only permitted in local execution mode"

toArgTypeError :: CEKValue step b i m -> ArgTypeError
toArgTypeError = \case
  VPactValue pv -> case pv of
    PLiteral l -> ATEPrim (literalPrim l)
    PTime _ -> ATEPrim PrimTime
    PList _ -> ATEList
    PObject _ -> ATEObject
    PGuard _ -> ATEPrim PrimGuard
    PModRef _ -> ATEModRef
    PCapToken _ -> ATEClosure
  VTable{} -> ATETable
  VClosure{} -> ATEClosure

{-# SPECIALIZE argsError
   :: ()
   -> CoreBuiltin
   -> [CEKValue step CoreBuiltin () Eval]
   -> Eval (EvalResult step CoreBuiltin () Eval)
    #-}
argsError
  :: (MonadEval b i m, IsBuiltin b)
  => i
  -> b
  -> [CEKValue step b i m]
  -> m a
argsError info b args =
  throwExecutionError info (NativeArgumentsError (builtinName b) (toArgTypeError <$> args))


{-# SPECIALIZE asString
   :: ()
   -> CoreBuiltin
   -> PactValue
   -> Eval Text
    #-}
asString
  :: (MonadEval b i m, IsBuiltin b)
  => i
  -> b
  -> PactValue
  -> m Text
asString _ _ (PLiteral (LString b)) = pure b
asString i b pv = argsError i b [VPactValue pv]

{-# SPECIALIZE asBool
   :: ()
   -> CoreBuiltin
   -> PactValue
   -> Eval Bool
    #-}
asBool
  :: (MonadEval b i m, IsBuiltin b)
  => i
  -> b
  -> PactValue
  -> m Bool
asBool _ _ (PLiteral (LBool b)) = pure b
asBool i b pv = argsError i b [VPactValue pv]

envFromPurity :: Purity -> CEKEnv step b i m -> CEKEnv step b i m
envFromPurity PImpure = id
envFromPurity PReadOnly = readOnlyEnv
envFromPurity PSysOnly = sysOnlyEnv

readOnlyEnv :: CEKEnv step b i m -> CEKEnv step b i m
readOnlyEnv e
  | view (cePactDb . pdbPurity) e == PSysOnly = e
  | otherwise =
      let pdb = view cePactDb e
          newPactdb =
              PactDb
             { _pdbPurity = PReadOnly
             , _pdbRead = _pdbRead pdb
             , _pdbWrite = \_ _ _ _ -> dbOpDisallowed
             , _pdbKeys = \_ -> dbOpDisallowed
             , _pdbCreateUserTable = \_ -> dbOpDisallowed
             , _pdbBeginTx = \_ -> dbOpDisallowed
             , _pdbCommitTx = dbOpDisallowed
             , _pdbRollbackTx = dbOpDisallowed
             , _pdbTxIds = \_ _ -> dbOpDisallowed
             , _pdbGetTxLog = \_ _ -> dbOpDisallowed
             }
      in set cePactDb newPactdb e

sysOnlyEnv :: forall step b i m. CEKEnv step b i m -> CEKEnv step b i m
sysOnlyEnv e
  | view (cePactDb . pdbPurity) e == PSysOnly = e
  | otherwise =
  let newPactdb =
          PactDb
         { _pdbPurity = PSysOnly
         , _pdbRead = read'
         , _pdbWrite = \_ _ _ _ -> dbOpDisallowed
         , _pdbKeys = const dbOpDisallowed
         , _pdbCreateUserTable = \_ -> dbOpDisallowed
         , _pdbBeginTx = const dbOpDisallowed
         , _pdbCommitTx = dbOpDisallowed
         , _pdbRollbackTx = dbOpDisallowed
         , _pdbTxIds = \_ _ -> dbOpDisallowed
         , _pdbGetTxLog = \_ _ -> dbOpDisallowed
         }
  in set cePactDb newPactdb e
  where
  pdb = view cePactDb e
  read' :: Domain k v b i -> k -> IO (Maybe v)
  read' dom k = case dom of
    DUserTables _ -> dbOpDisallowed
    _ -> _pdbRead pdb dom k

getDefPactId :: (MonadEval b i m) => i -> m DefPactId
getDefPactId info =
  useEvalState esDefPactExec >>= \case
    Just pe -> pure (_peDefPactId pe)
    Nothing ->
      throwExecutionError info NotInDefPactExecution

tvToDomain :: TableValue -> Domain RowKey RowData b i
tvToDomain tv =
  DUserTables (_tvName tv)

-- Todo: GasLog
{-# SPECIALIZE chargeGasArgs
   :: ()
   -> GasArgs
   -> Eval ()
    #-}
chargeGasArgs :: (MonadEval b i m) => i -> GasArgs -> m ()
chargeGasArgs info ga = do
  model <- viewEvalEnv eeGasModel
  currGas <- getGas
  let limit@(MilliGasLimit gasLimit) = _gmGasLimit model
      gUsed = currGas <> (_gmRunModel model) ga
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
  currGas <- getGas
  let limit@(MilliGasLimit gasLimit) = _gmGasLimit model
      gUsed = currGas <> (_gmNatives model) nativeArg
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
