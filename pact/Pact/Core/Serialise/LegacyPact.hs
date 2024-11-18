-- |
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Pact.Core.Serialise.LegacyPact
  ( decodeModuleData
  , decodeModuleData'
  , decodeKeySet
  , decodeDefPactExec
  , decodeNamespace
  , decodeRowData
  --
  , fromLegacyRowData
  , fromLegacyModule
  , fromLegacyModuleData
  , fromLegacyKeySet
  , fromLegacyNamespace
  , fromLegacyDefPactExec
  , runTranslateM
  ) where

import Control.Lens
import Bound (Scope)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Monad
import Data.List.NonEmpty(NonEmpty(..))
import Data.List (findIndex)
#if !MIN_VERSION_base(4,20,0)
import Data.List(foldl')
#endif
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Map.Strict(Map)
import Data.Foldable (foldrM)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import qualified Data.List.NonEmpty as NE
import qualified Bound

import Pact.Core.Builtin
import Pact.Core.ModRefs
import Pact.Core.Literal
import Pact.Core.Type
import Pact.Core.Imports
import Pact.Core.IR.Desugar
import Pact.Core.Capabilities
import Pact.Core.ChainData
import Pact.Core.Names
import Pact.Core.Persistence
import Pact.Core.Guards
import Pact.Core.DefPacts.Types
import Pact.Core.Namespace
import Pact.Core.PactValue
import Pact.Core.Hash

import qualified Pact.JSON.Decode as JD
import Pact.Core.IR.Term
import qualified Pact.Core.Serialise.CBOR_V1 as CBOR
import qualified Pact.Core.Serialise.LegacyPact.Types as Legacy




type LegacyRef = Legacy.Ref' Legacy.PersistDirect
type CoreTerm = EvalTerm CoreBuiltin ()
type CorePreNormalizedTerm = Term (Name, DeBruijn) Type CoreBuiltin ()
type CoreDef = EvalDef CoreBuiltin ()

data TranslateState =
  TranslateState
  { _tsDefns :: Map FullyQualifiedName CoreDef
  , _tsGenerated :: Map QualifiedName FullyQualifiedName
  }

makeLenses ''TranslateState


newtype TranslateEnv
  = TranslateEnv
  { _teDepth :: DeBruijn
  } deriving (Show, Eq)

makeLenses ''TranslateEnv

type TranslateM = ReaderT TranslateEnv (StateT TranslateState (Except String))

pattern UnitVal :: Term name ty builtin info
pattern UnitVal <- InlineValue (PLiteral LUnit) _

runTranslateM :: TranslateM a -> Either String a
runTranslateM a =
  let initialEnv = TranslateEnv 0
      initialState = TranslateState mempty mempty
  in runExcept (evalStateT (runReaderT a initialEnv) initialState)

decodeModuleData :: ByteString -> Maybe (ModuleData CoreBuiltin ())
decodeModuleData bs = do
  obj <- JD.decodeStrict' bs
  case runTranslateM (fromLegacyModuleData obj) of
    Left _ -> Nothing
    Right v -> Just v

decodeModuleData' :: ByteString -> Either String (ModuleData CoreBuiltin ())
decodeModuleData' bs = do
  obj <- maybe (Left "decodingError") Right $ JD.decodeStrict' bs
  runTranslateM (fromLegacyModuleData obj)

fromLegacyModuleData
  :: Legacy.ModuleData (Legacy.Ref' Legacy.PersistDirect)
  -> TranslateM (ModuleData CoreBuiltin ())
fromLegacyModuleData (Legacy.ModuleData md mref mdeps) = do
  case md of
    Legacy.MDModule m -> do
      let mh = fromLegacyModuleHash (Legacy._mHash m)
      deps <- fromLegacyDeps mh mdeps
      m' <- fromLegacyModule mh m mref
      newDeps <- use tsDefns
      pure (ModuleData m' (deps <> newDeps))
    Legacy.MDInterface i -> do
      let ifn = fromLegacyModuleName (Legacy._interfaceName i)
      let mh = ModuleHash $ pactHash $ T.encodeUtf8 (renderModuleName ifn)
      i'<- fromLegacyInterface mh i mref
      pure (InterfaceData i' M.empty)

fromLegacyInterface
  :: ModuleHash
  -> Legacy.Interface
  -> HM.HashMap T.Text LegacyRef
  -> TranslateM (EvalInterface CoreBuiltin ())
fromLegacyInterface mh (Legacy.Interface n code imp) mref = do
  let n' = fromLegacyModuleName n
      use' = fmap fromLegacyUse imp
  defs <- traverse (fromLegacyInterfaceDefRef mh) $ HM.elems mref
  pure (Interface n' defs use' mh (Hash mempty) (ModuleCode code) ())

fromLegacyDeps
  :: ModuleHash
  -> HM.HashMap Legacy.FullyQualifiedName (Legacy.Ref' Legacy.PersistDirect)
  -> TranslateM (Map FullyQualifiedName (EvalDef CoreBuiltin ()))
fromLegacyDeps mh hm = M.fromList <$> traverse f (HM.toList hm)
  where
  f (k,v) = (fromLegacyFullyQualifiedName k,) <$> fromLegacyDefRef mh v

fromLegacyDefRef :: ModuleHash -> LegacyRef -> TranslateM CoreDef
fromLegacyDefRef mh = \case
  Legacy.Ref (Legacy.TDef d) ->
    fromLegacyDef mh $ Right <$> d

  Legacy.Ref (Legacy.TTable tn mn mh' ty) ->
    fromLegacyTableDef tn mn mh' ty

  Legacy.Ref (Legacy.TSchema sn m fields) ->
    DSchema <$> fromLegacySchemaDef sn m fields

  Legacy.Ref (Legacy.TConst arg m ce) ->
    DConst <$> fromLegacyConstDef mh arg m ce

  Legacy.Ref t -> throwError $ "fromLegacyDefRef: " <>  show t
  Legacy.Direct _d -> throwError "fromLegacyDefRef: invariant Direct"

fromLegacyConstDef
  :: ModuleHash
  -> Legacy.Arg (Legacy.Term LegacyRef)
  -> Maybe Legacy.ModuleName
  -> Legacy.ConstVal (Legacy.Term LegacyRef)
  -> TranslateM (DefConst n Type b ())
fromLegacyConstDef mh arg _mn cv = do
  let arg' = (fmap.fmap) Right arg
  arg'' <- fromLegacyArg arg'
  cval <- case cv of
    Legacy.CVRaw _ -> throwError "fromLegacyConstDef: invariant"
    Legacy.CVEval _ t -> fromLegacyTerm mh (Right <$> t) >>= \case
      InlineValue pv _ -> pure pv
      _ -> throwError "fromLegacyConstDef: invariant, not InlineValue"
  pure (DefConst arg'' (EvaledConst cval) ())

fromLegacySchemaDef
  :: Legacy.TypeName
  -> Maybe Legacy.ModuleName
  -> [Legacy.Arg (Legacy.Term LegacyRef)]
  -> TranslateM (DefSchema Type ())
fromLegacySchemaDef (Legacy.TypeName sn) _mn largs = do
  let largs' = (fmap.fmap.fmap) Right largs
  args <- traverse fromLegacyArg largs'
  schema <- traverse (\(Arg n mty _) -> pure (Field n, fromMaybe TyAny mty)) args
  pure (DefSchema sn (M.fromList schema) ())

fromLegacyTableDef
  :: Legacy.TableName
  -> Legacy.ModuleName
  -> Legacy.ModuleHash
  -> Legacy.Type (Legacy.Term LegacyRef)
  -> TranslateM CoreDef
fromLegacyTableDef (Legacy.TableName tn) _mn _mh ty = do
  case ty of
    Legacy.TyUser t -> case unTVar (Right <$> t) of
      Legacy.TSchema (Legacy.TypeName n) (Just mn) f -> do
        let qn = QualifiedName n (fromLegacyModuleName mn)
        args <- traverse (\(Legacy.Arg n' ty') -> (Field n',) <$> fromLegacyType ty') f
        let sc = Schema qn (M.fromList args)
        pure (DTable (DefTable tn (ResolvedTable sc) ()))
      _ -> throwError "fromLegacyTableDef: invariant 1"
    _ -> throwError "fromLegacyTableDef: invariant 2"

fromLegacyTableDef'
  :: Legacy.TableName
  -> Legacy.ModuleName
  -> Legacy.ModuleHash
  -> Legacy.Type (Legacy.Term (Either CorePreNormalizedTerm LegacyRef))
  -> TranslateM CoreDef
fromLegacyTableDef' (Legacy.TableName tn) _mn _mh ty = do
  case ty of
    Legacy.TyUser t -> case unTVar t of
      Legacy.TSchema (Legacy.TypeName n) (Just mn) f -> do
        let qn = QualifiedName n (fromLegacyModuleName mn)
        args <- traverse (\(Legacy.Arg n' ty') -> (Field n',) <$> fromLegacyType ty') f
        let sc = Schema qn (M.fromList args)
        pure (DTable (DefTable tn (ResolvedTable sc) ()))
      _ -> throwError "fromLegacyTableDef: invariant 1"
    _ -> throwError "fromLegacyTableDef: invariant 2"

fromLegacyInterfaceDefRef :: ModuleHash -> LegacyRef -> TranslateM (EvalIfDef CoreBuiltin ())
fromLegacyInterfaceDefRef mh = \case
  Legacy.Ref (Legacy.TDef d) ->
    fromLegacyInterfDef (Right <$> d)

  Legacy.Ref (Legacy.TSchema sn m fields) ->
    IfDSchema <$> fromLegacySchemaDef sn m fields


  Legacy.Ref (Legacy.TConst arg m ce) ->
    IfDConst <$> fromLegacyConstDef mh arg m ce

  Legacy.Ref t -> throwError $ "fromLegacyDefRef: " <> show t
  Legacy.Direct _d -> throwError "fromLegacyDefRef: invariant Direct"

fromLegacyFullyQualifiedName
  :: Legacy.FullyQualifiedName
  -> FullyQualifiedName
fromLegacyFullyQualifiedName (Legacy.FullyQualifiedName n mn h) = let
  mn' = fromLegacyModuleName mn
  h' = fromLegacyHash h
  in FullyQualifiedName mn' n (ModuleHash h')

fromLegacyQualifiedName
  :: Legacy.QualifiedName
  -> QualifiedName
fromLegacyQualifiedName (Legacy.QualifiedName mn n) =
  QualifiedName n (fromLegacyModuleName mn)

fromLegacyDef
  :: ModuleHash -- parent module hash
  -> Legacy.Def (Either CorePreNormalizedTerm LegacyRef)
  -> TranslateM CoreDef
fromLegacyDef mh (Legacy.Def (Legacy.DefName n) _mn dt funty body meta) = do
  let mn = fromLegacyModuleName _mn
  ret <- fromLegacyType (Legacy._ftReturn funty)
  let lArgs = Legacy._ftArgs funty
  args <- traverse fromLegacyArg lArgs
  go mn ret args
  where
    lamFromArgs args b = case args of
      [] -> Nullary b ()
      (x:xs) -> Lam (x :| xs) b ()
    go mn ret args =
      case dt of
        Legacy.Defun -> do
          bodyForm' <- fromLegacyBodyForm' mh args body
          -- Increment the depth by the number of arguments and fix the indices
          -- before converting the body to its new form.
          body' <- local (over teDepth (+ fromIntegral (length args))) $ fixTreeIndices bodyForm'
          pure $ Dfun $ Defun
            (Arg n (Just ret) ())
            args
            (lamFromArgs args body')
            () -- info
        Legacy.Defpact -> do
          steps' <- fromLegacyStepForm' mh args body
          -- Increment the depth by the number of arguments and fix the indices
          steps'' <- local (over teDepth (+ fromIntegral (length args))) $ (traversed.traverseDefPactStep) fixTreeIndices steps'
          pure $ DPact (DefPact (Arg n (Just ret) ()) args steps'' ())
        Legacy.Defcap -> do
            bodyForm' <- fromLegacyBodyForm' mh args body
            -- Increment the depth by the number of argument and fix the indices
            body' <- local (over teDepth (+ fromIntegral (length args))) $ fixTreeIndices bodyForm'
            meta' <- case meta of
              -- Note: Empty `meta` implies the cap is
              -- unmanaged.
              Just meta' -> fromLegacyDefMeta mn mh args meta'
              Nothing -> pure Unmanaged
            pure $ DCap (DefCap (Arg n (Just ret) ()) args body' meta' ())

fromLegacyInterfDef
  :: Legacy.Def (Either CorePreNormalizedTerm LegacyRef)
  -> TranslateM (EvalIfDef CoreBuiltin ())
fromLegacyInterfDef (Legacy.Def (Legacy.DefName n) _mn dt funty _body meta) = do
  ret <- fromLegacyType (Legacy._ftReturn funty)
  let lArgs = Legacy._ftArgs funty
  args <- traverse fromLegacyArg lArgs
  case dt of
    Legacy.Defun -> do
      pure $ IfDfun $ IfDefun
        (Arg n (Just ret) ())  -- defun name
        args -- args
        () -- info
    Legacy.Defpact -> do
      pure $ IfDPact (IfDefPact (Arg n (Just ret) ()) args ())
    Legacy.Defcap -> do
        meta' <- case meta of
          -- Note: Empty `meta` implies the cap is
          -- unmanaged.
          Just m -> fromLegacyDefMetaInterface args m
          Nothing -> pure Unmanaged
        pure $ IfDCap (IfDefCap (Arg n (Just ret) ()) args meta' ())

fromLegacyDefMetaInterface
  :: [Arg Type ()]
  -> Legacy.DefMeta (Legacy.Term (Either CorePreNormalizedTerm LegacyRef))
  -> TranslateM (DefCapMeta BareName)
fromLegacyDefMetaInterface args = \case
  Legacy.DMDefcap (Legacy.DefcapManaged m) -> case m of
    Nothing -> pure (DefManaged AutoManagedMeta)
    Just (p,f) -> case findIndex (\x -> _argName x == p) args of
      Nothing -> throwError "fromLegacyDefMeta: invariant, index not found!"
      Just idx' -> case unTVar f of
        Legacy.TDef td -> do
          let (Legacy.DefName dn) = Legacy._dDefName td
          pure (DefManaged (DefManagedMeta (idx', p) (BareName dn)))
        f' -> throwError $ "invariant: interface defmeta invariant violated " <> show f'
  Legacy.DMDefcap Legacy.DefcapEvent -> pure DefEvent

fromLegacyDefMeta
  :: ModuleName
  -> ModuleHash
  -> [Arg Type ()]
  -> Legacy.DefMeta (Legacy.Term (Either CorePreNormalizedTerm LegacyRef))
  -> TranslateM (DefCapMeta (FQNameRef Name))
fromLegacyDefMeta _ mh args = \case
  Legacy.DMDefcap (Legacy.DefcapManaged m) -> case m of
    Nothing -> pure (DefManaged AutoManagedMeta)
    Just (p, f) -> case findIndex (\x -> _argName x == p) args of
      Nothing -> throwError "fromLegacyDefMeta: invariant, index not found!"
      Just idx' -> case unTVar f of
        Legacy.TDef tdef@(Legacy.Def (Legacy.DefName dn) mn _dt (Legacy.FunType _args _) _body _) -> do
          let mn' = fromLegacyModuleName mn
          let qn = QualifiedName dn mn'
          uses tsGenerated (M.lookup qn) >>= \case
            Just fqn -> do
              pure (DefManaged (DefManagedMeta (idx', p) (FQName fqn)))
            -- No def exists! We must make it because clearly it isn't anywhere else
            Nothing -> do
              let h = CBOR.encodeModuleName mn' <> T.encodeUtf8 dn <> hashToByteString (_mhHash mh)
                  newHash = ModuleHash (pactHash h)
                  newFqn = FullyQualifiedName mn' dn newHash
              defn <- fromLegacyDef newHash tdef
              tsGenerated %= M.insert qn newFqn
              tsDefns %= M.insert newFqn defn
              pure (DefManaged (DefManagedMeta (idx', p) (FQName newFqn)))
        Legacy.TVar (Right (Legacy.Direct (Legacy.PDFreeVar fqn))) -> do
          let fqn' = fromLegacyFullyQualifiedName fqn
          pure $ DefManaged (DefManagedMeta (idx', p) (FQName fqn'))
        f' -> throwError $ "invariant: interface defmeta invariant2 violated " <> show f'
  Legacy.DMDefcap Legacy.DefcapEvent -> pure DefEvent

fromLegacyModuleHash
  :: Legacy.ModuleHash
  -> ModuleHash
fromLegacyModuleHash (Legacy.ModuleHash h) = ModuleHash (fromLegacyHash h)

fromLegacyHash
  :: Legacy.Hash
  -> Hash
fromLegacyHash (Legacy.Hash h) = Hash h

fromLegacyModule
  :: ModuleHash
  -> Legacy.Module (Legacy.Def LegacyRef)
  -> HM.HashMap T.Text LegacyRef
  -> TranslateM (EvalModule CoreBuiltin ())
fromLegacyModule mh lm depMap = do
  let mn = fromLegacyModuleName (Legacy._mName lm)
      mhash = fromLegacyModuleHash (Legacy._mHash lm)
      impl = fmap fromLegacyModuleName (Legacy._mInterfaces lm)
      blessed = fmap fromLegacyModuleHash (HS.toList (Legacy._mBlessed lm))
      imps = fmap fromLegacyUse (Legacy._mImports lm)
      gov = fromLegacyGovernance mh (Legacy._mGovernance lm)
      code = ModuleCode (Legacy._mCode lm)
  let moduleDefKeys = HM.keys depMap
  let moduleDefs = M.fromList $ (\k -> (QualifiedName k mn, FullyQualifiedName mn k mhash)) <$> moduleDefKeys
  tsGenerated %= M.union moduleDefs
  defs <- traverse (fromLegacyDefRef mh) $ HM.elems depMap
  tsGenerated %= (`M.withoutKeys` (M.keysSet moduleDefs))
  pure (Module mn gov defs (S.fromList blessed) imps impl mhash (Hash mempty) code ())

fromLegacyBodyForm'
  :: ModuleHash -- parent module hash
  -> [Arg Type ()]
  -> Scope Int Legacy.Term (Either CorePreNormalizedTerm LegacyRef)
  -> TranslateM CorePreNormalizedTerm
fromLegacyBodyForm' mh args body = local (over teDepth (+ fromIntegral (length args))) $ do
  currDepth <- view teDepth
  case debruijnize currDepth args body of
    Legacy.TList li _ -> traverse (fromLegacyTerm mh) (reverse (V.toList li)) >>= \case
      x:xs -> pure $ foldl' (\a b -> Sequence b a ()) x xs
      _ -> throwError "fromLegacyBodyForm': invariant 1"
    _ -> throwError "fromLegacyBodyForm': invariant 2"

fromLegacyStepForm'
  :: ModuleHash
  -> [Arg Type ()]
  -> Scope Int Legacy.Term (Either CorePreNormalizedTerm LegacyRef)
  -> TranslateM (NonEmpty (Step (Name, DeBruijn) Type CoreBuiltin ()))
fromLegacyStepForm' mh args body = local (over teDepth (+ fromIntegral (length args))) $ do
  currDepth <- view teDepth
  case debruijnize currDepth args body of
    Legacy.TList li _ -> traverse fromStepForm (V.toList li) >>= \case
      x:xs -> pure (x NE.:| xs)
      _ -> throwError "fromLegacyStepForm': invariant"
    _ -> throwError "fromLegacyBodyForm': invariant"
  where
  fromStepForm = \case
    Legacy.TStep step -> fromLegacyStep mh step
    _ -> throwError "fromLegacyStepForm: invariant"

fromLegacyStep
  :: ModuleHash
  -> Legacy.Step (Legacy.Term (Either CorePreNormalizedTerm LegacyRef))
  -> TranslateM (Step (Name, DeBruijn) Type CoreBuiltin ())
fromLegacyStep mh (Legacy.Step _ t mrb) = do
  t' <- fromLegacyTerm mh t
  case mrb of
    Nothing -> pure (Step t')
    Just rb ->
      StepWithRollback t' <$> fromLegacyTerm mh rb

debruijnize
  :: DeBruijn
  -> [Arg ty i]
  -> Scope Int Legacy.Term (Either CorePreNormalizedTerm LegacyRef)
  -> Legacy.Term (Either CorePreNormalizedTerm LegacyRef)
debruijnize depth args = Bound.instantiate $ \i ->
    let
      totalLen = length args
      boundVar = NBound $ fromIntegral (totalLen - i - 1)
      -- Note, this action is safe as we knoe that there is
      -- at least one `args`.
      Arg n _ _ = args !! i
    in Legacy.TVar (Left (Var (Name n boundVar, depth) ()))

fromLegacyPactValue :: Legacy.PactValue -> Either String PactValue
fromLegacyPactValue = \case
  Legacy.PLiteral l -> pure $ either PLiteral id $ fromLegacyLiteral l
  Legacy.PList p -> do
    l <- traverse fromLegacyPactValue p
    pure (PList l)
  Legacy.PObject (Legacy.ObjectMap om) -> do
    om' <- traverse fromLegacyPactValue om
    pure (PObject $ M.mapKeys (\(Legacy.FieldKey k) -> Field k) om')
  Legacy.PGuard g -> case g of
    Legacy.GPact (Legacy.PactGuard p n) -> let
      p' = fromLegacyPactId p
      in pure (PGuard (GDefPactGuard (DefPactGuard p' n)))
    Legacy.GKeySet (Legacy.KeySet k pred') ->  let
      ks = S.map (PublicKeyText . Legacy._pubKey)  k
      p' = \case
        (Legacy.Name (Legacy.BareName bn))
          | bn == "keys-all" -> pure KeysAll
          | bn == "keys-any" -> pure KeysAny
          | bn == "keys-2"   -> pure Keys2
        (Legacy.QName qn) -> pure (CustomPredicate (TQN $ fromLegacyQualifiedName qn))
        o -> Left $ "fromLegacyPactValue: pred invariant: " <> show o
      in (PGuard . GKeyset . KeySet ks <$> p' pred')
    Legacy.GKeySetRef (Legacy.KeySetName ksn ns) -> let
      ns' = fromLegacyNamespaceName <$> ns
      in pure (PGuard . GKeySetRef $ KeySetName ksn ns')
    Legacy.GModule (Legacy.ModuleGuard mn n) -> let
      mn' = fromLegacyModuleName mn
      in pure (PGuard $ GModuleGuard (ModuleGuard mn' n))
    Legacy.GUser (Legacy.UserGuard n a) -> case n of
      Legacy.QName n' -> do
        let qn = fromLegacyQualifiedName n'
        args <- traverse fromLegacyPactValue a
        pure (PGuard $ GUserGuard (UserGuard qn args))
      _ -> Left "fromLegacyPactValue: invariant"
    Legacy.GCapability (Legacy.CapabilityGuard n a i) -> do
      let qn = fromLegacyQualifiedName n
      args <- traverse fromLegacyPactValue a
      pure (PGuard $ GCapabilityGuard (CapabilityGuard qn args (fromLegacyPactId <$> i)))
  Legacy.PModRef (Legacy.ModRef mn mmn) -> let
    mn' = fromLegacyModuleName mn
    imp = S.fromList $ fmap fromLegacyModuleName (fromMaybe [] mmn)
    in pure (PModRef $ ModRef mn' imp)


mkTwoArgLam :: (CorePreNormalizedTerm -> CorePreNormalizedTerm -> CorePreNormalizedTerm) -> TranslateM CorePreNormalizedTerm
mkTwoArgLam f = do
  depth <- view teDepth
  let arg1Name = "`lamArg1"
      arg2Name = "`lamArg2"
      arg1 = Arg arg1Name Nothing ()
      arg2 = Arg arg2Name Nothing ()
      arg1Var = Var (Name arg1Name (NBound 1), depth+2) ()
      arg2Var = Var (Name arg2Name (NBound 0), depth+2) ()
  pure $ Lam (arg1 :| [arg2]) (f arg1Var arg2Var) ()

mkOneArgLam :: (CorePreNormalizedTerm -> CorePreNormalizedTerm) -> TranslateM CorePreNormalizedTerm
mkOneArgLam f = do
  depth <- view teDepth
  let arg1Name = "`lamArg1"
      arg1 = Arg arg1Name Nothing ()
      arg1Var = Var (Name arg1Name (NBound 1), depth+1) ()
  pure $ Lam (arg1 :| []) (f arg1Var) ()

mkThreeArgLam
  :: (CorePreNormalizedTerm -> CorePreNormalizedTerm -> CorePreNormalizedTerm -> CorePreNormalizedTerm)
  -> TranslateM CorePreNormalizedTerm
mkThreeArgLam f = do
  depth <- view teDepth
  let arg1Name = "`lamArg1"
      arg2Name = "`lamArg2"
      arg3Name = "`lamArg3"
      arg1 = Arg arg1Name Nothing ()
      arg2 = Arg arg2Name Nothing ()
      arg3 = Arg arg3Name Nothing ()
      arg1Var = Var (Name arg1Name (NBound 2), depth+3) ()
      arg2Var = Var (Name arg2Name (NBound 1), depth+3) ()
      arg3Var = Var (Name arg3Name (NBound 0), depth+3) ()
  pure $ Lam (arg1 :| [arg2, arg3]) (f arg1Var arg2Var arg3Var) ()


fromLegacyPersistDirect
  :: Legacy.PersistDirect
  -> TranslateM CorePreNormalizedTerm
fromLegacyPersistDirect = \case
  Legacy.PDValue v ->
    liftEither $ (`InlineValue` ()) <$> fromLegacyPactValue v
  Legacy.PDNative (Legacy.NativeDefName n)
    | n == "enforce" -> pure (BuiltinForm (CEnforce unitValue unitValue) ())
    | n == "enforce-one" -> pure (BuiltinForm (CEnforceOne unitValue unitValue) ())
    | n == "if" -> pure (BuiltinForm (CIf unitValue unitValue unitValue) ())
    | n == "and" -> pure (BuiltinForm (CAnd unitValue unitValue) ())
    | n == "or" -> pure (BuiltinForm (COr unitValue unitValue) ())
    | n == "with-capability" -> pure (BuiltinForm (CWithCapability unitValue unitValue) ())
    | n == "create-user-guard" -> pure (BuiltinForm (CCreateUserGuard unitValue) ())
    | n == "try" -> pure (BuiltinForm (CTry unitValue unitValue) ())
    | n == "CHARSET_ASCII" -> pure (Constant (LInteger 0) ()) -- see Desugar.hs
    | n == "CHARSET_LATIN1" -> pure (Constant (LInteger 1) ())
    | n == "constantly" -> do
        let c1 = Arg "#constantlyA1" Nothing ()
        let c2 = Arg "#constantlyA2" Nothing ()
        d <- view teDepth
        pure $ Lam (c1 :| [c2]) (Var (Name "#constantlyA1" (NBound 1), d+1) ()) ()

    | otherwise -> case M.lookup n coreBuiltinMap of
        Just b -> pure (Builtin b ())
        _ -> throwError $ "fromLegacyPersistDirect: invariant -> " <> show n
  Legacy.PDFreeVar fqn -> let
    fqn' = fromLegacyFullyQualifiedName fqn
    in pure $ Var (fqnToName fqn', 0) ()
  where
    -- Note: unit* is used as placeholder, which gets replaced in `fromLegacyTerm`
    unitValue = InlineValue PUnit ()

objBindingToLet
  :: ModuleHash
  -> [Legacy.BindPair (Legacy.Term (Either CorePreNormalizedTerm LegacyRef))]
  -> Scope Int Legacy.Term (Either CorePreNormalizedTerm LegacyRef)
  -> TranslateM CorePreNormalizedTerm
objBindingToLet mh bps scope = do
  let len = length bps -- 1
  currDepth <- view teDepth
  args' <- traverse (fromLegacyArg . Legacy._bpArg) bps
  term' <- local (over teDepth (+ 1)) $ fromLegacyBodyForm' mh args' scope
  (finalBody', _) <- foldrM (mkAccess (succ currDepth)) (term', fromIntegral len) bps
  pure $ baseLam finalBody'
  where
  mkAccess currDepth bp (!body, !len) = do
    let !len' = len - 1 -- 0
    body' <- bpToObjLet (currDepth + len') len' bp body
    pure (body', len')
  objBindVar = "`objBind"
  baseLam term =
    Lam (pure (Arg objBindVar Nothing ())) term ()
  bpToObjLet currDepth objVarIx (Legacy.BindPair (Legacy.Arg n _) v) body = do
    fromLegacyTerm mh v >>= \case
      fieldLit@(Constant (LString _) _) -> do
        let larg = Arg n Nothing ()
            accessObj = Var (Name objBindVar (NBound objVarIx), currDepth) ()
            accessTerm = App (Builtin CoreAt ()) [fieldLit, accessObj] ()
        pure $ Let larg accessTerm body ()
      _ -> throwError "fromLegacyBindPair: Invariant"

desugarApp
  :: Term (Name, DeBruijn) Type CoreBuiltin ()
  -> [Term (Name, DeBruijn) Type CoreBuiltin ()]
  -> TranslateM CorePreNormalizedTerm
desugarApp fn args = case fn of
  Builtin b _ -> pure $ desugarAppArity () b args
  BuiltinForm c _ -> case c of
    CEnforce UnitVal UnitVal -> case args of
      [t1,t2] -> pure (BuiltinForm (CEnforce t1 t2) ())
      [t1] -> mkOneArgLam $ \x -> BuiltinForm (CEnforce t1 x) ()
      args' -> do
        lam <- mkTwoArgLam $ \x y -> BuiltinForm (CEnforce x y) ()
        pure $ App lam args' ()
    CEnforceOne UnitVal UnitVal -> case args of
      [t1, t2] -> pure (BuiltinForm (CEnforceOne t1 t2) ())
      args' -> do
        lam <- mkTwoArgLam $ \x y -> BuiltinForm (CEnforceOne x y) ()
        pure $ App lam args' ()
    CIf UnitVal UnitVal UnitVal -> case args of
      [cond, b1, b2] -> pure (BuiltinForm (CIf cond b1 b2) ())
      [cond, b1] -> mkOneArgLam $ \x -> BuiltinForm (CIf cond b1 x) ()
      [cond] -> mkTwoArgLam $ \x y -> BuiltinForm (CIf cond x y) ()
      args' -> do
        lam <- mkThreeArgLam $ \x y z -> BuiltinForm (CIf x y z) ()
        pure $ App lam args' ()
    CAnd UnitVal UnitVal -> case args of
        [b1, b2] -> pure (BuiltinForm (CAnd b1 b2) ())
        [b1] -> mkOneArgLam $ \x -> BuiltinForm (CAnd b1 x) ()
        args' -> do
          lam <- mkTwoArgLam $ \x y -> BuiltinForm (CAnd x y) ()
          pure $ App lam args' ()
    COr UnitVal UnitVal -> case args of
      [b1, b2] -> pure (BuiltinForm (COr b1 b2) ())
      [b1] -> mkOneArgLam $ \x -> BuiltinForm (COr b1 x) ()
      args' -> do
        lam <- mkTwoArgLam $ \x y -> BuiltinForm (COr x y) ()
        pure $ App lam args' ()
    CWithCapability UnitVal UnitVal -> case args of
      [t1, ListLit t2 _] -> case reverse t2 of
        [] -> throwError "invariant failure: with-cap empty body"
        x:xs -> do
          let body' = foldl' (\r l -> Sequence l r ()) x xs
          pure (BuiltinForm (CWithCapability t1 body') ())
      _ -> throwError "invariant failure: with-capability"
    CCreateUserGuard UnitVal -> case args of
        [x] ->
          pure (BuiltinForm (CCreateUserGuard x) ())
        args' -> do
          lam <- mkOneArgLam $ \x -> BuiltinForm (CCreateUserGuard x) ()
          pure $ App lam args' ()
    CTry UnitVal UnitVal -> case args of
        [t1, t2] -> pure (BuiltinForm (CTry t1 t2) ())
        args' -> do
          lam <- mkTwoArgLam $ \x y -> BuiltinForm (CTry x y) ()
          pure $ App lam args' ()
    _ -> pure $ App fn args ()
  _ -> pure $ App fn args ()

higherOrder1Arg :: [CoreBuiltin]
higherOrder1Arg = [CoreMap, CoreFilter]

higherOrder2Arg :: [CoreBuiltin]
higherOrder2Arg = [CoreFold, CoreZip]

fromLegacyTerm
  :: ModuleHash
  -> Legacy.Term (Either CorePreNormalizedTerm LegacyRef)
  -> TranslateM CorePreNormalizedTerm
fromLegacyTerm mh = \case
  Legacy.TVar n -> case n of
    Left t -> pure t
    Right v -> case v of
      Legacy.Direct v' -> fromLegacyPersistDirect v'
      Legacy.Ref t -> fromLegacyTerm mh (Right <$> t)

  Legacy.TApp (Legacy.App fn args) -> do
    fn' <- fromLegacyTerm mh fn
    case fn' of
      Builtin b _  | b `elem` higherOrder1Arg
          , Legacy.TApp (Legacy.App mapOperator mapOperands): xs <- args -> do
        -- [HOF Translation]
        -- Note: The following sections of translation are explained as follows:
        -- we transform, for example `(map (+ k) other-arg)` into
        -- `(map (lambda (arg) (+ k arg)) other-arg)
        -- This eta expansion is necessary to

          d <- view teDepth
          let injectedArg = (Var (Name "iArg" (NBound 0), d + 1) () :: CorePreNormalizedTerm)
          let containingLam e = Lam (pure (Arg "lArg" Nothing ())) e ()
          (mapOperator', mapOperands') <- local (over teDepth (+ 1)) $ (,) <$> fromLegacyTerm mh mapOperator <*> traverse (fromLegacyTerm mh) mapOperands
          body <- containingLam <$> desugarApp mapOperator' (mapOperands' ++ [injectedArg])
          xs' <- traverse (fromLegacyTerm mh) xs
          pure (App fn' (body:xs') ())

      Builtin b _ | b `elem` higherOrder2Arg
          , Legacy.TApp (Legacy.App mapOperator mapOperands): xs <- args -> do
          d <- view teDepth
          let injectedArg1 = (Var (Name "iArg1" (NBound 1), d + 2) () :: CorePreNormalizedTerm)
              injectedArg2 = (Var (Name "iArg2" (NBound 0), d + 2) () :: CorePreNormalizedTerm)
          let containingLam e = Lam (Arg "iArg1" Nothing () :| [Arg "iArg2" Nothing ()]) e ()
          (mapOperator', mapOperands') <- local (over teDepth (+ 2)) $ (,) <$> fromLegacyTerm mh mapOperator <*> traverse (fromLegacyTerm mh) mapOperands
          body <- containingLam <$> desugarApp mapOperator' (mapOperands' ++ [injectedArg1, injectedArg2])
          xs' <- traverse (fromLegacyTerm mh) xs
          pure (App fn' (body:xs') ())
      
      _ -> do
        args' <- traverse (fromLegacyTerm mh) args
        desugarApp fn' args'


  Legacy.TLam (Legacy.Lam _ (Legacy.FunType args _) body) -> do
    args' <- traverse fromLegacyArg args
    body' <- fromLegacyBodyForm' mh args' body
    case args' of
      [] -> pure $ Nullary body' ()
      x:xs -> pure (Lam (x :| xs) body' ())

  Legacy.TList l _ -> do
    l' <- traverse (fromLegacyTerm mh) (V.toList l)
    pure (ListLit l' ())

  Legacy.TConst _args _module (Legacy.CVEval _ v) ->
    fromLegacyTerm mh v
  -- Note: this use case may appear in the `TConst` constructor

  Legacy.TGuard g ->
    (\v -> InlineValue (PGuard v) ()) <$> fromLegacyGuard mh g

  -- Todo: binding pairs should be done like in `Desugar.hs`
  Legacy.TBinding bps body bt -> case bt of
    Legacy.BindLet -> do
      args' <- traverse (fromLegacyArg . Legacy._bpArg) bps
      body' <- fromLegacyBodyForm' mh args' body
      goLet (fromIntegral (length bps)) (reverse bps) body'
      where
      goLet d (Legacy.BindPair arg val : xs) rest = do
        arg' <- fromLegacyArg arg
        v' <- local (over teDepth (+ (d - 1))) $ fromLegacyTerm mh val
        goLet (d - 1) xs (Let arg' v' rest ())
      goLet _ [] rest = pure rest
    Legacy.BindSchema _ ->
      objBindingToLet mh bps body

  Legacy.TObject (Legacy.Object o _ _) -> do
   let m = M.toList (Legacy._objectMap o)
   obj <- traverse (\(Legacy.FieldKey f, t) -> (Field f,) <$> fromLegacyTerm mh t) m
   pure (ObjectLit obj ())

  -- Note: this does not show up in the prod database
  -- Legacy.TNative{} -> throwError "fromLegacyTerm: invariant"

  Legacy.TLiteral l ->
    pure $ either (`Constant` ()) (`InlineValue` ()) $ fromLegacyLiteral l

  (Legacy.TTable tn@(Legacy.TableName tbl) mn mh' ty) -> do
    let mn' = fromLegacyModuleName mn
        qn = QualifiedName tbl mn'
    uses tsGenerated (M.lookup qn) >>= \case
      Just _ -> do
        let mh'' = fromLegacyModuleHash mh'
            nk = NTopLevel mn' mh''
        pure (Var (Name tbl nk, 0) ())
      Nothing -> do
        let mh'' = fromLegacyModuleHash mh'
            newFqn = FullyQualifiedName mn' tbl mh''
            nk = NTopLevel mn' mh''
        defn <- fromLegacyTableDef' tn mn mh' ty
        tsGenerated %= M.insert qn newFqn
        tsDefns %= M.insert newFqn defn
        pure (Var (Name tbl nk, 0) ())

  -- Note: impossible
  Legacy.TModule{} -> throwError "fromLegacyTerm: invariant"

  -- Note: impossible
  Legacy.TStep{} -> throwError "fromLegacyTerm: invariant"

  -- Note: TDef may show up in some old modules
  Legacy.TDef d@(Legacy.Def n mn _dt (Legacy.FunType _args _) _body _) -> do
    let mn' = fromLegacyModuleName mn
        dn  = Legacy._unDefName n
    let qn = QualifiedName dn mn'
    uses tsGenerated (M.lookup qn) >>= \case
      Just (FullyQualifiedName _ _ mhash) -> do
        let nk = NTopLevel mn' mhash
            name = Name dn nk
        depth <- view teDepth
        pure (Var (name, depth) ())
      Nothing -> do
        depth <- view teDepth
        let h = CBOR.encodeModuleName mn' <> T.encodeUtf8 dn <> hashToByteString (_mhHash mh)
            newHash = ModuleHash (pactHash h)
            nk = NTopLevel mn' newHash
            newFqn = FullyQualifiedName mn' dn newHash
            name = Name dn nk
        defn <- fromLegacyDef newHash d
        tsGenerated %= M.insert qn newFqn
        tsDefns %= M.insert newFqn defn
        pure (Var (name, depth) ())


  Legacy.TDynamic mr dm -> do
    mr' <- fromLegacyTerm mh mr
    case mr' of
      Var (Name n (NBound db),depth) _ -> case unTVar dm of
        Legacy.TDef d -> let
          dname = Legacy._unDefName (Legacy._dDefName d)
          name = Name n (NDynRef (DynamicRef dname db))
          in pure (Var (name, depth) ())
        _ -> throwError "fromLegacyTerm: invariant not a TDEF"
      _ -> throwError "fromLegacyTerm: invariant"

  -- Note: impossible
  Legacy.TSchema{} -> throwError "fromLegacyTerm: invariant"

  -- Note: impossible in terms
  Legacy.TUse{} -> throwError "fromLegacyTerm: invariant"

  Legacy.TModRef (Legacy.ModRef mn mmn) -> let
    mn' = fromLegacyModuleName mn
    imp = S.fromList $ fmap fromLegacyModuleName (fromMaybe [] mmn)
    in pure (InlineValue (PModRef (ModRef mn' imp)) ())

  _ -> throwError "fromLegacyTerm: invariant"

fromLegacyGuard
  :: ModuleHash
  -> Legacy.Guard (Legacy.Term (Either CorePreNormalizedTerm LegacyRef))
  -> TranslateM (Guard QualifiedName PactValue)
fromLegacyGuard mh = \case
  Legacy.GPact (Legacy.PactGuard i n) -> let
    Legacy.PactId pid = i
    in pure (GDefPactGuard (DefPactGuard (DefPactId pid) n))
  Legacy.GKeySet ks -> liftEither (GKeyset <$> fromLegacyKeySet ks)
  Legacy.GKeySetRef ks -> pure (GKeySetRef $ fromLegacyKeySetName ks)

  Legacy.GCapability (Legacy.CapabilityGuard n a i) -> do
    let qn = fromLegacyQualifiedName n
    args <- traverse (extract <=< fromLegacyTerm mh) a
    let pid = fmap fromLegacyPactId i
    pure (GCapabilityGuard (CapabilityGuard qn args pid))

  Legacy.GModule (Legacy.ModuleGuard mn n) -> let
    mn' = fromLegacyModuleName mn
    in pure (GModuleGuard (ModuleGuard mn' n))

  Legacy.GUser (Legacy.UserGuard n a) -> case n of
    Legacy.QName n' -> do
      let qn = fromLegacyQualifiedName n'
      args <- traverse (extract <=< fromLegacyTerm mh) a
      pure (GUserGuard (UserGuard qn args))
    _ -> error "invariant: this cannot happen"
 where
  extract = \case
     InlineValue p _ -> pure p
     _ -> throwError "fromLegacyGuard: extract invariant"


fromLegacyPactId
  :: Legacy.PactId
  -> DefPactId
fromLegacyPactId (Legacy.PactId pid) = DefPactId pid

fromLegacyLiteral
  :: Legacy.Literal
  -> Either Literal PactValue
fromLegacyLiteral = \case
  Legacy.LString s -> Left (LString s)
  Legacy.LInteger i -> Left (LInteger i)
  Legacy.LDecimal d -> Left (LDecimal d)
  Legacy.LBool b -> Left (LBool b)
  Legacy.LTime l -> Right $ PTime l

fromLegacyUse
  :: Legacy.Use
  -> Import
fromLegacyUse (Legacy.Use mn mh imp) = let
  mn' = fromLegacyModuleName mn
  mh' = fromLegacyModuleHash <$> mh
  imp' = V.toList <$> imp
  in Import mn' mh' imp'

fromLegacyArg
  :: Legacy.Arg (Legacy.Term (Either CorePreNormalizedTerm LegacyRef))
  -> TranslateM (Arg Type ())
fromLegacyArg (Legacy.Arg n ty) = (\t -> Arg n (Just t) ()) <$> fromLegacyType ty

fromLegacyType
  :: Legacy.Type (Legacy.Term (Either CorePreNormalizedTerm LegacyRef))
  -> TranslateM Type
fromLegacyType = \case
  Legacy.TyAny -> pure TyAny
  Legacy.TyList Legacy.TyAny -> pure TyAnyList
  Legacy.TyList t -> TyList <$> fromLegacyType t
  Legacy.TyPrim prim -> pure $ TyPrim (fromLegacyPrimType prim)
  Legacy.TySchema s ty _ -> fromLegacySchema s ty
  Legacy.TyFun _ -> throwError "invariant failure: tyfun"
  Legacy.TyModule m -> fromLegacyTypeModule m
  -- This specific case might cause a bit of semantic divergence, but
  -- this can happen since pact 4 has some interesting bugs.
  Legacy.TyUser _ -> pure TyAny
  Legacy.TyVar _ -> pure TyAny

unTVar
  :: Legacy.Term (Either CorePreNormalizedTerm LegacyRef)
  -> Legacy.Term (Either CorePreNormalizedTerm LegacyRef)
unTVar = \case
  Legacy.TVar (Right (Legacy.Ref t)) -> unTVar (Right <$> t)
  d -> d

fromLegacyTypeModule
  :: Maybe [Legacy.Term (Either CorePreNormalizedTerm LegacyRef)]
  -> TranslateM Type
fromLegacyTypeModule = \case
  Nothing -> throwError "fromLegacyTypeModule: invariant"
  Just [] -> throwError "fromLegacyTypeModule: invariant"
  Just l -> do
    let l' = unTVar <$> l
    TyModRef . S.fromList <$> traverse extract l'
  where
    extract = \case
      Legacy.TModRef (Legacy.ModRef mn _) -> pure (fromLegacyModuleName mn)
      _ -> throwError "fromLegacyTypeModule: invariant"

fromLegacySchema
  :: Legacy.SchemaType
  -> Legacy.Type (Legacy.Term (Either CorePreNormalizedTerm LegacyRef))
  -> TranslateM Type
fromLegacySchema st ty = case (st, ty) of
  (Legacy.TyTable, Legacy.TyUser t) -> case unTVar t of
    Legacy.TSchema (Legacy.TypeName n) (Just mn) f -> do
      let qn = QualifiedName n (fromLegacyModuleName mn)
      args <- traverse (\(Legacy.Arg n' ty') -> (Field n',) <$> fromLegacyType ty') f
      pure (TyTable (Schema qn (M.fromList args)))
    _ -> throwError "fromLegacySchema: invariant 1"
  (Legacy.TyObject, Legacy.TyUser t) -> case unTVar t of
    Legacy.TSchema (Legacy.TypeName n) (Just mn) f -> do
      let qn = QualifiedName n (fromLegacyModuleName mn)
      args <- traverse (\(Legacy.Arg n' ty') -> (Field n',) <$> fromLegacyType ty') f
      pure (TyObject (Schema qn (M.fromList args)))
    _ -> throwError "fromLegacySchema: invariant tyobject"

  (Legacy.TyObject, Legacy.TyAny) -> pure TyAnyObject

  (Legacy.TyBinding, _) -> throwError "invariant failure: tybinding"

  (s,t) -> throwError $ "fromLegacySchema: invariant 2: " <> show s <> " : " <> show t


fromLegacyPrimType
  :: Legacy.PrimType
  -> PrimType
fromLegacyPrimType = \case
  Legacy.TyInteger -> PrimInt
  Legacy.TyDecimal -> PrimDecimal
  Legacy.TyTime -> PrimTime
  Legacy.TyBool -> PrimBool
  Legacy.TyString -> PrimString
  Legacy.TyGuard _ -> PrimGuard

fromLegacyGovernance
  :: ModuleHash
  -> Legacy.Governance (Legacy.Def LegacyRef)
  -> Governance Name
fromLegacyGovernance _ (Legacy.Governance (Left ks)) = KeyGov (fromLegacyKeySetName ks)
fromLegacyGovernance mh (Legacy.Governance (Right n)) = let
  fqn = FullyQualifiedName
    (fromLegacyModuleName $ Legacy._dModule n)
    (Legacy._unDefName $ Legacy._dDefName n)
    mh
  in CapGov (FQName fqn)

fromLegacyKeySetName
  :: Legacy.KeySetName
  -> KeySetName
fromLegacyKeySetName (Legacy.KeySetName ksn ns)
  = KeySetName ksn (fromLegacyNamespaceName <$> ns)

fromLegacyNamespaceName :: Legacy.NamespaceName -> NamespaceName
fromLegacyNamespaceName (Legacy.NamespaceName ns) = NamespaceName ns

fromLegacyModuleName
  :: Legacy.ModuleName
  -> ModuleName
fromLegacyModuleName (Legacy.ModuleName n ns)
  = ModuleName n (fromLegacyNamespaceName <$> ns)

decodeKeySet :: ByteString -> Maybe KeySet
decodeKeySet bs = do
  obj <- JD.decodeStrict' bs
  either (const Nothing) Just (fromLegacyKeySet obj)

fromLegacyKeySet
  :: Legacy.KeySet
  -> Either String KeySet
fromLegacyKeySet (Legacy.KeySet ks p) = do
  let ks' = S.map fromLegacyPublicKeyText ks
  pred' <- case p of
    Legacy.Name (Legacy.BareName "keys-all") -> pure KeysAll
    Legacy.Name (Legacy.BareName "keys-2") -> pure Keys2
    Legacy.Name (Legacy.BareName "keys-any") -> pure KeysAny
    Legacy.QName qn -> pure (CustomPredicate (TQN $ fromLegacyQualifiedName qn))
    other -> Left $ "fromLegacyKeySet: pred invariant" <> show other
  pure (KeySet ks' pred')

fromLegacyPublicKeyText
  :: Legacy.PublicKeyText
  -> PublicKeyText
fromLegacyPublicKeyText (Legacy.PublicKeyText t) = PublicKeyText t

decodeDefPactExec :: ByteString -> Maybe (Maybe DefPactExec)
decodeDefPactExec o = do
  obj <- JD.decodeStrict' o
  either (const Nothing) Just (fromLegacyDefPactExec obj)

fromLegacyDefPactExec'
  :: Legacy.PactExec
  -> Either String DefPactExec
fromLegacyDefPactExec' (Legacy.PactExec sc y _ step pid cont rb nest) = do
  y' <- traverse fromLegacyYield y
  cont' <- fromLegacyContinuation cont
  nest' <- traverse
    (\(k,v) -> (fromLegacyPactId k,) <$> fromLegacyNestedDefPactExec' v)
    (M.toList nest)
  pure $
    DefPactExec sc y' step (fromLegacyPactId pid)
    cont'
    rb
    (M.fromList nest')

fromLegacyNestedDefPactExec'
  :: Legacy.NestedPactExec
  -> Either String NestedDefPactExec
fromLegacyNestedDefPactExec' (Legacy.NestedPactExec sc y _ step pid cont nest) = do
  y' <- traverse fromLegacyYield y
  cont' <- fromLegacyContinuation cont
  nest' <- traverse
    (\(k,v) -> (fromLegacyPactId k,) <$> fromLegacyNestedDefPactExec' v)
    (M.toList nest)
  pure $
    NestedDefPactExec sc y' step (fromLegacyPactId pid)
    cont'
    (M.fromList nest')

fromLegacyDefPactExec
  :: Maybe Legacy.PactExec
  -> Either String (Maybe DefPactExec)
fromLegacyDefPactExec = \case
  Nothing -> pure Nothing
  Just n -> Just <$> fromLegacyDefPactExec' n



fromLegacyContinuation
  :: Legacy.PactContinuation
  -> Either String (DefPactContinuation QualifiedName PactValue)
fromLegacyContinuation (Legacy.PactContinuation n args) = do
  n' <- toQualifiedName n
  args' <- traverse fromLegacyPactValue args
  pure (DefPactContinuation n' args')
  where
  toQualifiedName = \case
    Legacy.QName qn -> pure (fromLegacyQualifiedName qn)
    _ -> Left "fromLegacyContinuation invariant: expected qualified name"


fromLegacyYield :: Legacy.Yield -> Either String Yield
fromLegacyYield (Legacy.Yield (Legacy.ObjectMap o) yprov ychain)
  = do
  o' <- traverse (\(k, v) -> (fromLegacyField k,) <$> fromLegacyPactValue v) (M.toList o)
  pure $ Yield
      (M.fromList o')
      (fromLegacyProvenance <$> yprov)
      (fromLegacyChainId <$> ychain)

fromLegacyField :: Legacy.FieldKey -> Field
fromLegacyField (Legacy.FieldKey f) = Field f

fromLegacyChainId :: Legacy.ChainId -> ChainId
fromLegacyChainId (Legacy.ChainId i) = ChainId i

fromLegacyProvenance :: Legacy.Provenance -> Provenance
fromLegacyProvenance (Legacy.Provenance tchain mh)
  = Provenance (fromLegacyChainId tchain) (fromLegacyModuleHash mh)


decodeNamespace :: ByteString -> Maybe Namespace
decodeNamespace o = do
  obj <- JD.decodeStrict' o
  either (const Nothing) Just (fromLegacyNamespace obj)

fromLegacyNamespace
  :: Legacy.Namespace Legacy.PactValue
  -> Either String Namespace
fromLegacyNamespace (Legacy.Namespace ns u a) = do
  let ns' = fromLegacyNamespaceName ns
  u' <- fromLegacyGuard' u
  a' <- fromLegacyGuard' a
  pure (Namespace ns' u' a')

fromLegacyGuard'
  :: Legacy.Guard Legacy.PactValue
  -> Either String (Guard QualifiedName PactValue)
fromLegacyGuard' = \case
  Legacy.GPact (Legacy.PactGuard i n) -> let
    Legacy.PactId pid = i
    in pure (GDefPactGuard (DefPactGuard (DefPactId pid) n))
  Legacy.GKeySet ks -> GKeyset <$> fromLegacyKeySet ks
  Legacy.GKeySetRef (Legacy.KeySetName ksn ns) -> let
    ns' = fromLegacyNamespaceName <$>  ns
    in pure (GKeySetRef $ KeySetName ksn ns')
  Legacy.GCapability (Legacy.CapabilityGuard n a i) -> do
    let qn = fromLegacyQualifiedName n
    let pid = fmap fromLegacyPactId i
    args <- traverse fromLegacyPactValue a
    pure (GCapabilityGuard (CapabilityGuard qn args pid))

  Legacy.GModule (Legacy.ModuleGuard mn n) -> let
    mn' = fromLegacyModuleName mn
    in pure (GModuleGuard (ModuleGuard mn' n))

  Legacy.GUser (Legacy.UserGuard n a) -> case n of
    Legacy.QName n' -> do
      let qn = fromLegacyQualifiedName n'
      args <- traverse fromLegacyPactValue a
      pure (GUserGuard (UserGuard qn args))
    _ -> error "impossible: this cannot happen"

decodeRowData :: ByteString -> Maybe RowData
decodeRowData o = do
  obj <- JD.decodeStrict' o
  either (const Nothing) Just (fromLegacyRowData obj)

fromLegacyRowData
  :: Legacy.RowData
  -> Either String RowData
fromLegacyRowData (Legacy.RowData _ (Legacy.ObjectMap m)) = do
  let f = fromLegacyPactValue .rowDataToPactValue
  m' <- traverse (\(k,v) -> (fromLegacyField k,) <$> f v) (M.toList m)
  pure (RowData (M.fromList m'))

rowDataToPactValue :: Legacy.RowDataValue -> Legacy.PactValue
rowDataToPactValue rdv = case rdv of
  Legacy.RDLiteral l -> Legacy.PLiteral l
  Legacy.RDList l -> Legacy.PList $ recur l
  Legacy.RDObject o -> Legacy.PObject $ recur o
  Legacy.RDGuard g -> Legacy.PGuard $ recur g
  Legacy.RDModRef m -> Legacy.PModRef m
  where
    recur :: Functor f => f Legacy.RowDataValue -> f Legacy.PactValue
    recur = fmap rowDataToPactValue


fixTreeIndices :: CorePreNormalizedTerm -> TranslateM CoreTerm
fixTreeIndices = \case
  Var (n, depthAtInstantiate) info -> do
    currDepth <- view teDepth
    case _nKind n of
      NBound i
        | depthAtInstantiate < currDepth -> do
          let n' = Name (_nName n) (NBound (currDepth - depthAtInstantiate + i))
          pure (Var n' info)
        | otherwise -> pure (Var n info)
      NDynRef (DynamicRef fn i)
        | depthAtInstantiate < currDepth -> do
          let n' = Name (_nName n) $ NDynRef (DynamicRef fn (currDepth - depthAtInstantiate + i))
          pure (Var n' info)
        | otherwise -> pure (Var n info)
      _ -> pure (Var n info)
  Lam args term i -> do
    Lam args <$> local (over teDepth (+ fromIntegral (length args))) (fixTreeIndices term) <*> pure i
  Let arg e1 e2 i -> do
    e1' <- fixTreeIndices e1
    e2' <- local (over teDepth (+ 1)) $ fixTreeIndices e2
    pure $ Let arg e1' e2' i
  App fn args i ->
    App <$> fixTreeIndices fn <*> traverse fixTreeIndices args <*> pure i
  BuiltinForm bfn i ->
    BuiltinForm <$> traverse fixTreeIndices bfn <*> pure i
  Builtin b i ->
    pure $ Builtin b i
  Constant l i ->
    pure $ Constant l i
  Sequence l r i ->
    Sequence <$> fixTreeIndices l <*> fixTreeIndices r <*> pure i
  Nullary t i -> Nullary <$> fixTreeIndices t <*> pure i
  ListLit args i ->
    ListLit <$> traverse fixTreeIndices args <*> pure i
  ObjectLit fields i ->
    ObjectLit <$> (traversed._2) fixTreeIndices fields <*> pure i
  InlineValue p i ->
    pure $ InlineValue p i
