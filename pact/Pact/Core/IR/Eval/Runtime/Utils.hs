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
 , findCallingModule
 , getCallingModule
 , calledByModule
 , failInvariant
 , isExecutionFlagSet
 , whenExecutionFlagSet
 , unlessExecutionFlagSet
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
 , enforceStackTopIsDefcap
 , anyCapabilityBeingEvaluated
 , checkSchema
 , checkPartialSchema
 , emitEvent
 , emitReservedEvent
 , emitCapability
 , fqctToPactEvent
 , emitEventUnsafe
 ) where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Data.Monoid
import Data.Foldable(find, toList)
import Data.Maybe(listToMaybe)
import Data.Text(Text)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Pact.Core.Names
import Pact.Core.PactValue
import Pact.Core.Builtin
import Pact.Core.IR.Term
import Pact.Core.Type
import Pact.Core.Errors
import Pact.Core.IR.Eval.Runtime.Types
import Pact.Core.Literal
import Pact.Core.Persistence
import Pact.Core.Environment
import Pact.Core.DefPacts.Types
import Pact.Core.Gas.Types
import qualified Pact.Core.Gas.Utils as Gas.Utils
import Pact.Core.Guards
import Pact.Core.Capabilities
import Pact.Core.Hash
import Control.Monad.Except

emitReservedEvent :: Text -> [PactValue] -> ModuleHash -> EvalM e b i ()
emitReservedEvent name params mhash = do
  let pactModule = ModuleName "pact" Nothing
  let pe = PactEvent name params pactModule mhash
  emitEventUnsafe pe

emitEvent
  :: i
  -> PactEvent PactValue
  -> EvalM e b i ()
emitEvent info pe = findCallingModule >>= \case
    Just mn -> do
      let ctModule = _peModule pe
      if ctModule == mn then emitEventUnsafe pe
      else throwExecutionError info (EventDoesNotMatchModule mn)
    Nothing -> throwExecutionError info (EventDoesNotMatchModule (_peModule pe))

emitEventUnsafe
  :: PactEvent PactValue
  -> EvalM e b i ()
emitEventUnsafe pe =
  unlessExecutionFlagSet FlagDisablePactEvents $
    esEvents %= (pe:)

emitCapability
  :: i
  -> CapToken FullyQualifiedName PactValue
  -> EvalM e b i ()
emitCapability info tkn =
  emitEvent info (fqctToPactEvent tkn)

fqctToPactEvent :: CapToken FullyQualifiedName PactValue -> PactEvent PactValue
fqctToPactEvent (CapToken fqn args) = PactEvent (_fqName fqn) args (_fqModule fqn) (_fqHash fqn)

lookupFqName :: FullyQualifiedName -> EvalM e b i (Maybe (EvalDef b i))
lookupFqName fqn =
  uses (esLoaded.loAllLoaded) (M.lookup fqn)
{-# INLINABLE lookupFqName #-}

getDefCap :: i -> FullyQualifiedName -> EvalM e b i (EvalDefCap b i)
getDefCap info fqn = lookupFqName fqn >>= \case
  Just (DCap d) -> pure d
  Just _ -> failInvariant info (InvariantExpectedDefCap fqn)
  _ -> failInvariant info (InvariantUnboundFreeVariable fqn)

getDefun :: i -> FullyQualifiedName -> EvalM e b i (EvalDefun b i)
getDefun info fqn = lookupFqName fqn >>= \case
  Just (Dfun d) -> pure d
  Just _ -> failInvariant info (InvariantExpectedDefun fqn)
  _ -> failInvariant info (InvariantUnboundFreeVariable fqn)

unsafeUpdateManagedParam :: v -> ManagedCap name v -> ManagedCap name v
unsafeUpdateManagedParam newV (ManagedCap mc orig (ManagedParam fqn _oldV i)) =
  ManagedCap mc orig (ManagedParam fqn newV i)
unsafeUpdateManagedParam _ a = a

typecheckArgument :: i -> PactValue -> Type -> EvalM e b i ()
typecheckArgument info pv ty =
  unless (checkPvType ty pv) $ throwExecutionError info (RunTimeTypecheckFailure (pvToArgTypeError pv) ty)

maybeTCType :: i -> Maybe Type -> PactValue -> EvalM e b i ()
maybeTCType i mty pv = maybe (pure ()) (typecheckArgument i pv) mty


pvToArgTypeError :: PactValue -> ArgTypeError
pvToArgTypeError = \case
  PLiteral l -> ATEPrim (literalPrim l)
  PTime _ -> ATEPrim PrimTime
  PList _ -> ATEList
  PObject _ -> ATEObject
  PGuard _ -> ATEPrim PrimGuard
  PModRef _ -> ATEModRef
  PCapToken _ -> ATEClosure

findCallingModule :: EvalM e b i (Maybe ModuleName)
findCallingModule = do
  stack <- use esStack
  pure $ listToMaybe $ fmap (_fqModule . _sfName) stack

calledByModule
  :: ModuleName
  -> EvalM e b i Bool
calledByModule mn = do
  stack <- use esStack
  case find (\sf -> (_fqModule . _sfName) sf == mn) stack of
    Just _ -> pure True
    Nothing -> pure False

-- | Throw an invariant failure, that is
-- an error which we do not expect to see during regular pact
-- execution. If this case is ever hit, we have a problem with
-- some invalid state in interpretation
failInvariant :: i -> InvariantError -> EvalM e b i a
failInvariant i reason =
  throwExecutionError i (InvariantFailure reason)

getCallingModule :: i -> EvalM e b i (EvalModule b i)
getCallingModule info = findCallingModule >>= \case
  Just mn -> do
    pdb <- viewEvalEnv eePactDb
    getModule info pdb mn
  Nothing ->
    throwExecutionError info (EvalError "no module call in stack")

safeTail :: [a] -> [a]
safeTail (_:xs) = xs
safeTail [] = []

isExecutionFlagSet :: ExecutionFlag -> EvalM e b i Bool
isExecutionFlagSet flag = viewsEvalEnv eeFlags (S.member flag)

whenExecutionFlagSet :: ExecutionFlag -> EvalM e b i () -> EvalM e b i ()
whenExecutionFlagSet flag act =
  isExecutionFlagSet flag >>= (`when` act)


unlessExecutionFlagSet :: ExecutionFlag -> EvalM e b i () -> EvalM e b i ()
unlessExecutionFlagSet flag act =
  isExecutionFlagSet flag >>= (`unless` act)

evalStateToErrorState :: EvalState b i -> ErrorState i
evalStateToErrorState es =
  ErrorState (_esCaps es) (_esStack es) (_esCheckRecursion es)

restoreFromErrorState :: ErrorState i -> EvalState b i -> EvalState b i
restoreFromErrorState (ErrorState caps stack recur) =
  set esCaps caps . set esStack stack . set esCheckRecursion recur

checkNonLocalAllowed :: IsBuiltin b => i -> b -> EvalM e b i ()
checkNonLocalAllowed info b = do
  disabledInTx <- isExecutionFlagSet FlagDisableHistoryInTransactionalMode
  mode <- viewEvalEnv eeMode
  when (mode == Transactional && disabledInTx) $ throwExecutionError info $
    OperationIsLocalOnly (builtinName b)
{-# INLINABLE checkNonLocalAllowed #-}

asString
  :: (IsBuiltin b)
  => i
  -> b
  -> PactValue
  -> EvalM e b i Text
asString _ _ (PLiteral (LString b)) = pure b
asString info b pv =
  throwExecutionError info (NativeArgumentsError (builtinName b) [pvToArgTypeError pv])
{-# INLINABLE asString #-}

asBool
  :: (IsBuiltin b)
  => i
  -> b
  -> PactValue
  -> EvalM e b i Bool
asBool _ _ (PLiteral (LBool b)) = pure b
asBool info b pv =
  throwExecutionError info (NativeArgumentsError (builtinName b) [pvToArgTypeError pv])
{-# INLINABLE asBool #-}

-- | Todo: revisit
checkSchema :: M.Map Field PactValue -> Schema -> Bool
checkSchema o (Schema _ sc) =
  M.size o == M.size sc &&
  getAll (M.foldMapWithKey (\k v -> All $ maybe False (`checkPvType` v) (M.lookup k sc)) o)

-- | Todo: revisit
checkPartialSchema :: M.Map Field PactValue -> Schema -> Bool
checkPartialSchema o (Schema _ sc) =
  M.isSubmapOfBy (\obj ty -> checkPvType ty obj) o sc


getDefPactId :: i -> EvalM e b i DefPactId
getDefPactId info =
  use esDefPactExec >>= \case
    Just pe -> pure (_peDefPactId pe)
    Nothing ->
      throwExecutionError info NotInDefPactExecution

tvToDomain :: TableValue -> Domain RowKey RowData b i
tvToDomain tv =
  DUserTables (_tvName tv)

chargeGasArgs :: i -> GasArgs b -> EvalM e b i ()
chargeGasArgs info ga = do
  stack <- use esStack
  gasEnv <- viewEvalEnv eeGasEnv
  either throwError return =<<
    liftIO (Gas.Utils.chargeGasArgsM gasEnv info stack ga)

chargeFlatNativeGas :: i -> b -> EvalM e b i ()
chargeFlatNativeGas info nativeArg =
  chargeGasArgs info (GNative nativeArg)

getGas :: EvalM e b i MilliGas
getGas =
  viewEvalEnv (eeGasEnv . geGasRef) >>= liftIO . readIORef
{-# INLINE getGas #-}

putGas :: MilliGas -> EvalM e b i ()
putGas !g = do
  gasRef <- viewEvalEnv (eeGasEnv . geGasRef)
  liftIO (writeIORef gasRef g)
{-# INLINE putGas #-}


litCmpGassed :: i -> Literal -> Literal -> EvalM e b i (Maybe Ordering)
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


valEqGassed :: i -> PactValue -> PactValue -> EvalM e b i Bool
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

enforceBlessedHashes :: i -> EvalModule b i -> ModuleHash -> EvalM e b i ()
enforceBlessedHashes info md mh
  | _mHash md == mh = return ()
  | mh `S.member` _mBlessed md = return ()
  | otherwise = throwExecutionError info (HashNotBlessed (_mName md) mh)

enforceStackTopIsDefcap
  :: IsBuiltin b
  => i
  -> b
  -> EvalM e b i ()
enforceStackTopIsDefcap info b = do
  let errMsg = "native must be called within a defcap body"
  use esStack >>= \case
      sf:_ -> do
        when (_sfFnType sf /= SFDefcap) $
          throwNativeExecutionError info b errMsg
      _ ->
        throwNativeExecutionError info b errMsg


anyCapabilityBeingEvaluated
  :: S.Set (CapToken QualifiedName PactValue)
  -> EvalM e b i Bool
anyCapabilityBeingEvaluated caps = do
  capsBeingEvaluated <- use (esCaps . csCapsBeingEvaluated)
  return $! any (`S.member` caps) capsBeingEvaluated
