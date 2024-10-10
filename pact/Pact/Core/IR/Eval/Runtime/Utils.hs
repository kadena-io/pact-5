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
 , getDefCapQN
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
 , readKeyset'
 , renderPactValue
 , createPrincipalForGuard
 , createEnumerateList
 , guardForModuleCall
 , guardTable
 , emitPactWarning
 ) where

import Control.Lens hiding (from, to)
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Data.Vector(Vector)
import Data.Foldable(find, toList)
import Data.Maybe(maybeToList)
import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.Set as S
import qualified Data.ByteString as BS

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
import Pact.Core.Gas
import Pact.Core.Info
import Pact.Core.ModRefs

import Pact.Core.Guards
import Pact.Core.Capabilities
import Pact.Core.Hash
import Pact.Core.SizeOf
import Pact.Core.StableEncoding
import qualified Pact.Core.Pretty as Pretty
import qualified Pact.Core.Principal as Pr

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

guardTable
  :: ()
  => i
  -> TableValue
  -> GuardTableOp
  -> EvalM e b i ()
guardTable i (TableValue tn mh _) dbop = do
  let mn = _tableModuleName tn
  checkLocalBypass $
    guardForModuleCall i mn $ do
      mdl <- getModule i mn
      enforceBlessedHashes i mdl mh
  where
  checkLocalBypass notBypassed = do
    enabled <- isExecutionFlagSet FlagAllowReadInLocal
    case dbop of
      GtWrite -> notBypassed
      GtCreateTable -> notBypassed
      _ | enabled -> pure ()
        | otherwise -> notBypassed

getDefCapQN :: i -> QualifiedName -> EvalM e b i (EvalDefCap b i, ModuleHash)
getDefCapQN info qn = do
  getModuleMemberWithHash info qn >>= \case
    (DCap d, mh) -> pure (d, mh)
    (_, mh) -> failInvariant info (InvariantUnboundFreeVariable (qualNameToFqn qn mh))

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
typecheckArgument info pv ty = do
  c <- gassedRuntimeTypecheck info ty pv
  unless c $ throwExecutionError info (RunTimeTypecheckFailure (pvToArgTypeError pv) ty)

-- | Runtime typechecking. For "simple" non-recursive types,
--   this is free. Otherwise, we will charge a small amount of gas per
--   "layer" of typechecking
gassedRuntimeTypecheck :: i -> Type -> PactValue -> EvalM e b i Bool
gassedRuntimeTypecheck _ TyAny = const (pure True)
gassedRuntimeTypecheck i ty = \case
  PLiteral l -> pure (typeOfLit l == ty)
  PGuard{} -> pure $ ty == TyGuard
  PTime _ -> pure $ ty == TyTime
  PCapToken _ -> pure $ ty == TyCapToken
  PModRef (ModRef _orig ifs) -> case ty of
    TyModRef mns
      -- Note: size is O(1)
      | S.size ifs < 10 ->
        pure (mns `S.isSubsetOf` ifs)
      | otherwise -> do
        chargeGasArgs i (GAConstant (scalarMulMilliGas constantWorkNodeGas (S.size ifs)))
        pure  (mns `S.isSubsetOf` ifs)
    _ -> pure False
  PList pli -> case ty of
    TyAnyList -> pure True
    TyList t -> do
      -- Note: length is O(1)
      chargeGasArgs i (GAConstant (scalarMulMilliGas constantWorkNodeGas (V.length pli)))
      vs <- traverse (gassedRuntimeTypecheck i t) pli
      pure (and vs)
    _ -> pure False
  PObject o -> case ty of
    TyAnyObject -> pure True
    TyObject sc -> gassedTypecheckObj i o sc
    _ -> pure False

-- | Typecheck an object against a schema, charge gas
gassedTypecheckObj :: i -> M.Map Field PactValue -> Schema -> EvalM e b i Bool
gassedTypecheckObj i o (Schema _ sc)
  | M.size o == M.size sc = do
    chargeGasArgs i (GAConstant (scalarMulMilliGas constantWorkNodeGas (M.size o)))
    go (M.toList o) (M.toList sc)
  | otherwise = pure False
  where
  -- We rely on field ordering here.
  -- If the object and the schema have different fields, then at some point they'll differ
  go ((f, pv):xs)  ((f', ty):ys)
    | f == f' = do
      c <- gassedRuntimeTypecheck i ty pv
      if c then go xs ys
      else pure c
    | otherwise = pure False
  -- xs and ys are guaranteed to be of the same size
  go _ _ = pure True

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
  use esStack >>= \case
    (StackFrame fqn _ _ _) : _ -> pure (Just (_fqModule fqn))
    _ -> pure Nothing

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
  Just mn -> getModule info mn
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
checkSchema :: i -> M.Map Field PactValue -> Schema -> EvalM e b i Bool
checkSchema = gassedTypecheckObj

-- | Todo: revisit
checkPartialSchema :: i -> M.Map Field PactValue -> Schema -> EvalM e b i Bool
checkPartialSchema  info o (Schema q sc) = do
  chargeGasArgs info (GAConstant (scalarMulMilliGas constantWorkNodeGas (M.size o)))
  let keys = M.keys o
  if all (`M.member` sc) keys then
    gassedTypecheckObj info o (Schema q (M.restrictKeys sc (S.fromList keys)))
  else pure False

getDefPactId :: i -> EvalM e b i DefPactId
getDefPactId info =
  use esDefPactExec >>= \case
    Just pe -> pure (_peDefPactId pe)
    Nothing ->
      throwExecutionError info NotInDefPactExecution

tvToDomain :: TableValue -> Domain RowKey RowData b i
tvToDomain tv =
  DUserTables (_tvName tv)



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

guardForModuleCall
  :: ()
  => i
  -> ModuleName
  -> EvalM e b i a
  -> EvalM e b i a
guardForModuleCall i currMod onFound =
  findCallingModule >>= \case
    Just mn | mn == currMod -> onFound
    _ -> do
      mc <- use (esCaps . csModuleAdmin)
      if S.member currMod mc then onFound
      else
        throwExecutionError i (ModuleAdminNotAcquired currMod)

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

readKeyset' :: i -> Text -> EvalM e b i (Maybe KeySet)
readKeyset' info ksn = do
  viewEvalEnv eeMsgBody >>= \case
    PObject envData -> do
      chargeGasArgs info $ GObjOp $ ObjOpLookup ksn $ M.size envData
      case M.lookup (Field ksn) envData of
        Just (PGuard (GKeyset ks)) -> pure (Just ks)
        Just (PObject dat) -> do
          chargeGasArgs info $ GObjOp $ ObjOpLookup "keys" objSize
          chargeGasArgs info $ GObjOp $ ObjOpLookup "pred" objSize
          case parseObj dat of
            Nothing -> pure Nothing
            Just (ks, p) -> do
              chargeGasArgs info $ GStrOp $ StrOpParse $ T.length p
              pure $ KeySet ks <$> readPredicate p
          where
          objSize = M.size dat
          parseObj d = do
            keys <- M.lookup (Field "keys") d
            keyText <- preview _PList keys >>= traverse (fmap PublicKeyText . preview (_PLiteral . _LString))
            predRaw <- M.lookup (Field "pred") d
            p <- preview (_PLiteral . _LString) predRaw
            let ks = S.fromList (V.toList keyText)
            pure (ks, p)
          readPredicate = \case
            "keys-any" -> pure KeysAny
            "keys-2" -> pure Keys2
            "keys-all" -> pure KeysAll
            n | Just pn <- parseParsedTyName n -> pure (CustomPredicate pn)
            _ -> Nothing
        Just (PList li) ->
          case parseKeyList li of
            Just ks -> pure (Just (KeySet ks KeysAll))
            Nothing -> pure Nothing
          where
          parseKeyList d =
            S.fromList . V.toList . fmap PublicKeyText <$> traverse (preview (_PLiteral . _LString)) d
        _ -> pure Nothing
    _ -> pure Nothing

renderPactValue :: i -> PactValue -> EvalM e b i Text
renderPactValue info pv = do
  sz <- sizeOf info SizeOfV0 pv
  chargeGasArgs info $ GConcat $ TextConcat $ GasTextLength $ fromIntegral sz
  pure $ Pretty.renderCompactText pv


createPrincipalForGuard
  :: i
  -> Guard QualifiedName PactValue
  -> EvalM e b i Pr.Principal
createPrincipalForGuard info = \case
  GKeyset (KeySet ks pf) -> case (toList ks, pf) of
    ([k], KeysAll)
      | ed25519HexFormat k -> Pr.K k <$ chargeGas 1_000
    (l, _) -> do
      h <- mkHash $ map (T.encodeUtf8 . _pubKey) l
      case pf of
        CustomPredicate (TQN (QualifiedName n (ModuleName mn mns))) -> do
          let totalLength = T.length n + T.length mn + maybe 0 (T.length . _namespaceName) mns
          chargeGasArgs info $ GConcat $ TextConcat $ GasTextLength totalLength
        _ -> pure ()
      pure $ Pr.W (hashToText h) (predicateToText pf)
  GKeySetRef ksn ->
    Pr.R ksn <$ chargeGas 1_000
  GModuleGuard (ModuleGuard mn n) ->
    Pr.M mn n <$ chargeGas 1_000
  GUserGuard (UserGuard f args) -> do
    h <- mkHash $ map encodeStable args
    pure $ Pr.U (renderQualName f) (hashToText h)
    -- TODO orig pact gets here ^^^^ a Name
    -- which can be any of QualifiedName/BareName/DynamicName/FQN,
    -- and uses the rendered string here. Need to double-check equivalence.
  GCapabilityGuard (CapabilityGuard f args pid) -> do
    let args' = map encodeStable args
        f' = T.encodeUtf8 $ renderQualName f
        pid' = T.encodeUtf8 . renderDefPactId <$> pid
    h <- mkHash $ f' : args' ++ maybeToList pid'
    pure $ Pr.C $ hashToText h
  GDefPactGuard (DefPactGuard dpid name) -> Pr.P dpid name <$ chargeGas 1_000
  where
    chargeGas mg = chargeGasArgs info (GAConstant (MilliGas mg))
    mkHash bss = do
      let bs = mconcat bss
          gasChargeAmt = 1_000 + fromIntegral (BS.length bs `quot` 64) * 1_000
      chargeGas gasChargeAmt
      pure $ pactHash bs

createEnumerateList
  :: i
  -- ^ info
  -> Integer
  -- ^ from
  -> Integer
  -- ^ to
  -> Integer
  -- ^ Step
  -> EvalM e b i (Vector Integer)
createEnumerateList info from to inc
  | from == to = do
    fromSize <- sizeOf info SizeOfV0 from
    chargeGasArgs info (GMakeList 1 fromSize)
    pure (V.singleton from)
  | inc == 0 = pure mempty -- note: covered by the flat cost
  | from < to, from + inc < from =
    throwExecutionError info (EnumerationError "enumerate: increment diverges below from interval bounds.")
  | from > to, from + inc > from =
    throwExecutionError info (EnumerationError "enumerate: increment diverges above from interval bounds.")
  | otherwise = do
    let len = succ (abs (from - to) `div` abs inc)
    listSize <- sizeOf info SizeOfV0 (max (abs from) (abs to))
    chargeGasArgs info (GMakeList len listSize)
    pure $ V.enumFromStepN from inc (fromIntegral len)

emitPactWarning :: i -> PactWarning -> EvalM e b i ()
emitPactWarning i pw =
  viewEvalEnv eeWarnings >>= \case
    Nothing -> pure ()
    Just warnRef ->
      liftIO $ modifyIORef' warnRef (pushWarning (Located i pw))
