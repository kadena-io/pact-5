{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ConstraintKinds #-}

module Pact.Core.IR.Eval.Runtime.Utils
 ( mkBuiltinFn
 , cfFQN
 , enforcePactValue
 , setEvalState, (%%=), useEvalState, usesEvalState
 , getAllStackCaps
 , checkSigCaps
 , lookupFqName
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
 , getModule
 , getCallingModule
 , readOnlyEnv
 , sysOnlyEnv
 , viewCEKEnv
 , viewsCEKEnv
 , calledByModule
 , failInvariant
 , getModuleData
 ) where

import Control.Lens hiding ((%%=))
import Control.Monad.Except(MonadError(..))
import Data.Map.Strict(Map)
import Data.Text(Text)
import Data.Set(Set)
import Data.Default(def)
import Data.Maybe(listToMaybe, mapMaybe)
import Data.Foldable(find)
import qualified Data.Map.Strict as M
import qualified Data.Set as Set

import Pact.Core.Names
import Pact.Core.PactValue
import Pact.Core.Builtin
import Pact.Core.Guards
import Pact.Core.IR.Term
import Pact.Core.ModRefs
import Pact.Core.Type
import Pact.Core.Errors
import Pact.Core.IR.Eval.Runtime.Types
import Pact.Core.Literal
import Pact.Core.Capabilities
import Pact.Core.Persistence
import Pact.Core.Hash

mkBuiltinFn
  :: (IsBuiltin b)
  => i
  -> b
  -> CEKEnv b i m
  -> NativeFunction b i m
  -> NativeFn b i m
mkBuiltinFn i b env fn =
  NativeFn b env fn (builtinArity b) i
{-# INLINE mkBuiltinFn #-}

cfFQN :: Lens' (CapFrame b i) FullyQualifiedName
cfFQN f = \case
  WithCapFrame fqn b -> (`WithCapFrame` b) <$> f fqn
  -- RequireCapFrame fqn -> RequireCapFrame <$> f fqn
  -- ComposeCapFrame fqn -> ComposeCapFrame <$> f fqn
  -- InstallCapFrame fqn -> InstallCapFrame <$> f fqn
  -- EmitEventFrame fqn -> EmitEventFrame <$> f fqn
  CreateUserGuardFrame fqn -> CreateUserGuardFrame <$> f fqn

getAllStackCaps
  :: MonadEval b i m
  => m (Set (CapToken QualifiedName PactValue))
getAllStackCaps = do
  Set.fromList . concatMap capToList <$> useEvalState (esCaps . csSlots)
  where
  capToList (CapSlot c cs) = c:cs

-- Todo: capautonomous
checkSigCaps
  :: MonadEval b i m
  => Map PublicKeyText (Set (CapToken QualifiedName PactValue))
  -> m (Map PublicKeyText (Set (CapToken QualifiedName PactValue)))
checkSigCaps sigs = do
  granted <- getAllStackCaps
  autos <- useEvalState (esCaps . csAutonomous)
  -- liftIO $ print granted
  -- liftIO $ print sigs
  pure $ M.filter (match (Set.null autos) granted) sigs
  where
  match allowEmpty granted sigCaps =
    (Set.null sigCaps && allowEmpty) ||
    not (Set.null (Set.intersection granted sigCaps))

enforcePactValue :: Applicative f => CEKValue b i m -> f PactValue
enforcePactValue = \case
  VPactValue pv -> pure pv
  VTable{} -> error "a table is not a pact value"
  VClosure{} -> error "closure is not a pact value"

-- Note: The following functions
-- when placed in this file are causing GHC 9.6.2 to bork with the following error:
-- <no location info>: error:
--     panic! (the 'impossible' happened)
--   GHC version 9.6.2:
-- 	lookupIdSubst
--   $dMonadEvalEnv_aO5i
--   InScope {b_aNXG i_aNXH m_aNXI s_aNXJ a_aNXK $d(%,,,%)_aNXL
--            mkBuiltinFn cfFQN fromPactValue setEvalState overEvalState
--            useEvalState usesEvalState viewCEKEnv}
--   Call stack:
--       CallStack (from HasCallStack):
--         callStackDoc, called at compiler/GHC/Utils/Panic.hs:189:37 in ghc:GHC.Utils.Panic
--         pprPanic, called at compiler/GHC/Core/Subst.hs:197:17 in ghc:GHC.Core.Subst
--   CallStack (from HasCallStack):
--     panic, called at compiler/GHC/Utils/Error.hs:454:29 in ghc:GHC.Utils.Error
viewCEKEnv :: (MonadEval b i m) => Lens' (EvalEnv b i) s -> m s
viewCEKEnv l = view l <$> readEnv

viewsCEKEnv :: (MonadEval b i m) => Lens' (EvalEnv b i) s -> (s -> a) -> m a
viewsCEKEnv f l = views f l <$> readEnv

setEvalState :: (MonadEval b i m) => Traversal' (EvalState b i) s -> s -> m ()
setEvalState l s = modifyEvalState (set l s)

-- overEvalState :: (MonadEval b i m) => Lens' (EvalState b i) s -> (s -> s) -> m ()
-- overEvalState l f = modifyCEKState (over l f)

(%%=) :: (MonadEval b i m) => Traversal' (EvalState b i) s -> (s -> s) -> m ()
l %%= f = modifyEvalState (over l f)

infix 4 %%=

useEvalState :: (MonadEval b i m) => Lens' (EvalState b i) s -> m s
useEvalState l = view l <$> getEvalState

usesEvalState :: (MonadEval b i m) => Lens' (EvalState b i) s -> (s -> s') -> m s'
usesEvalState l f = views l f <$> getEvalState

lookupFqName :: (MonadEval b i m) => FullyQualifiedName -> m (Maybe (EvalDef b i))
lookupFqName fqn =
  views (esLoaded.loAllLoaded) (M.lookup fqn) <$> getEvalState

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

toFqDep :: ModuleName -> ModuleHash -> Def name t b i -> (FullyQualifiedName, Def name t b i)
toFqDep modName mhash defn =
  let fqn = FullyQualifiedName modName (defName defn) mhash
  in (fqn, defn)

getModule :: (MonadEval b i m) => i -> CEKEnv b i m -> ModuleName -> m (EvalModule b i)
getModule info env mn =
 useEvalState (esLoaded . loModules . at mn) >>= \case
   Just (ModuleData md _) -> pure md
   Just (InterfaceData _ _) ->
    throwExecutionError info (ExpectedModule mn)
   Nothing -> do
    let pdb = view cePactDb env
    liftDbFunction info (_pdbRead pdb DModules mn) >>= \case
      Just mdata@(ModuleData md deps) -> do
        let newLoaded = M.fromList $ toFqDep mn (_mHash md) <$> (_mDefs md)
        (esLoaded . loAllLoaded) %%= M.union newLoaded . M.union deps
        (esLoaded . loModules) %%= M.insert mn mdata
        pure md
      Just (InterfaceData _ _) ->
        throwExecutionError info (ExpectedModule mn)
      Nothing ->
        throwExecutionError info (ModuleDoesNotExist mn)

getModuleData :: (MonadEval b i m) => i -> CEKEnv b i m -> ModuleName -> m (ModuleData b i)
getModuleData info env mn =
 useEvalState (esLoaded . loModules . at mn) >>= \case
   Just md -> pure md
   Nothing -> do
    let pdb = view cePactDb env
    liftDbFunction info (_pdbRead pdb DModules mn) >>= \case
      Just mdata@(ModuleData md deps) -> do
        let newLoaded = M.fromList $ toFqDep mn (_mHash md) <$> (_mDefs md)
        (esLoaded . loAllLoaded) %%= M.union newLoaded . M.union deps
        (esLoaded . loModules) %%= M.insert mn mdata
        pure mdata
      Just ifdata@(InterfaceData iface deps) -> do
        let mdefs = mapMaybe ifDefToDef (_ifDefns iface)
        let newLoaded = M.fromList $ toFqDep mn (_ifHash iface) <$> mdefs
        (esLoaded . loAllLoaded) %%= M.union newLoaded . M.union deps
        (esLoaded . loModules) %%= M.insert mn ifdata
        pure ifdata
      Nothing ->
        throwExecutionError info (ModuleDoesNotExist mn)

safeTail :: [a] -> [a]
safeTail (_:xs) = xs
safeTail [] = []

toArgTypeError :: CEKValue b i m -> ArgTypeError
toArgTypeError = \case
  VPactValue pv -> case pv of
    PLiteral l -> ATEPrim (literalPrim l)
    PList _ -> ATEList
    PObject _ -> ATEObject
    PGuard _ -> ATEPrim PrimGuard
    PModRef _ -> ATEModRef
    PCapToken _ -> ATEClosure
  VTable{} -> ATETable
  VClosure{} -> ATEClosure

argsError
  :: (MonadEval b i m, IsBuiltin b)
  => i
  -> b
  -> [CEKValue b3 i2 m2]
  -> m a
argsError info b args =
  throwExecutionError info (NativeArgumentsError (builtinName b) (toArgTypeError <$> args))


asString
  :: (MonadEval b i m, IsBuiltin b)
  => i
  -> b
  -> PactValue
  -> m Text
asString _ _ (PLiteral (LString b)) = pure b
asString i b pv = argsError i b [VPactValue pv]

asBool
  :: (MonadEval b i m, IsBuiltin b)
  => i
  -> b
  -> PactValue
  -> m Text
asBool _ _ (PLiteral (LString b)) = pure b
asBool i b pv = argsError i b [VPactValue pv]

throwExecutionError :: (MonadEval b i m) => i -> EvalError -> m a
throwExecutionError i e = throwError (PEExecutionError e i)

throwExecutionError' :: (MonadEval b i m) => EvalError -> m a
throwExecutionError' = throwExecutionError def

readOnlyEnv :: CEKEnv b i m -> CEKEnv b i m
readOnlyEnv e
  | view (cePactDb . pdbPurity) e == PSysOnly = e
  | otherwise =
  let pdb = view cePactDb  e
      newPactdb =
          PactDb
         { _pdbPurity = PReadOnly
         , _pdbRead = _pdbRead pdb
         , _pdbWrite = \_ _ _ _ -> dbOpDisallowed
         , _pdbKeys = \_ -> dbOpDisallowed
         , _pdbCreateUserTable = \_ _ -> dbOpDisallowed
         , _pdbBeginTx = \_ -> dbOpDisallowed
         , _pdbCommitTx = dbOpDisallowed
         , _pdbRollbackTx = dbOpDisallowed
         , _pdbTxIds = \_ _ -> dbOpDisallowed
         , _pdbGetTxLog = \_ _ -> dbOpDisallowed
         }
  in set cePactDb newPactdb e

sysOnlyEnv :: forall b i m. CEKEnv b i m -> CEKEnv b i m
sysOnlyEnv e
  | view (cePactDb . pdbPurity) e == PSysOnly = e
  | otherwise =
  let newPactdb =
          PactDb
         { _pdbPurity = PSysOnly
         , _pdbRead = read'
         , _pdbWrite = \_ _ _ _ -> dbOpDisallowed
         , _pdbKeys = \_ -> dbOpDisallowed
         , _pdbCreateUserTable = \_ _ -> dbOpDisallowed
         , _pdbBeginTx = \_ -> dbOpDisallowed
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
    DKeySets -> _pdbRead pdb DKeySets k
    DModules -> _pdbRead pdb dom k
    DPacts -> _pdbRead pdb dom k

