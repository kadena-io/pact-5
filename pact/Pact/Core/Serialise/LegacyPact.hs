-- |
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pact.Core.Serialise.LegacyPact
  ( decodeModuleData
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
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Map.Strict(Map)
import Data.Foldable (foldl', foldrM)
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
import qualified Pact.Core.Pretty as Pretty

import Text.Show.Deriving
import Data.Functor.Classes (Show1(..))

import Debug.Trace



type LegacyRef = Legacy.Ref' Legacy.PersistDirect
type CoreTerm = EvalTerm CoreBuiltin ()
type CorePreNormalizedTerm = Term (Name, DeBruijn) Type CoreBuiltin ()
type CoreDef = EvalDef CoreBuiltin ()

type TranslateState = [CoreDef]


type TranslateM = ReaderT DeBruijn (StateT TranslateState (Except String))

instance Show1 Legacy.Def where
  liftShowsPrec = $(makeLiftShowsPrec ''Legacy.Def)
instance Show1 Legacy.Lam where
  liftShowsPrec = $(makeLiftShowsPrec ''Legacy.Lam)
instance Show1 Legacy.Object where
  liftShowsPrec = $(makeLiftShowsPrec ''Legacy.Object)
instance Show1 Legacy.Term where
  liftShowsPrec = $(makeLiftShowsPrec ''Legacy.Term)

instance Show1 Legacy.Type where
  liftShowsPrec = $(makeLiftShowsPrec ''Legacy.Type)
instance Show1 Legacy.TypeVar where
  liftShowsPrec = $(makeLiftShowsPrec ''Legacy.TypeVar)
instance Show1 Legacy.FunType where
  liftShowsPrec = $(makeLiftShowsPrec ''Legacy.FunType)
instance Show1 Legacy.Arg where
  liftShowsPrec = $(makeLiftShowsPrec ''Legacy.Arg)

instance Show1 Legacy.Guard where
  liftShowsPrec = $(makeLiftShowsPrec ''Legacy.Guard)
instance Show1 Legacy.CapabilityGuard where
  liftShowsPrec = $(makeLiftShowsPrec ''Legacy.CapabilityGuard)
instance Show1 Legacy.UserGuard where
  liftShowsPrec = $(makeLiftShowsPrec ''Legacy.UserGuard)
instance Show1 Legacy.BindPair where
  liftShowsPrec = $(makeLiftShowsPrec ''Legacy.BindPair)
instance Show1 Legacy.App where
  liftShowsPrec = $(makeLiftShowsPrec ''Legacy.App)
instance Show1 Legacy.ObjectMap where
  liftShowsPrec = $(makeLiftShowsPrec ''Legacy.ObjectMap)
instance Show1 Legacy.BindType where
  liftShowsPrec = $(makeLiftShowsPrec ''Legacy.BindType)
instance Show1 Legacy.ConstVal where
  liftShowsPrec = $(makeLiftShowsPrec ''Legacy.ConstVal)
instance Show1 Legacy.DefcapMeta where
  liftShowsPrec = $(makeLiftShowsPrec ''Legacy.DefcapMeta)
instance Show1 Legacy.DefMeta where
  liftShowsPrec = $(makeLiftShowsPrec ''Legacy.DefMeta)
instance Show1 Legacy.ModuleDef where
  liftShowsPrec = $(makeLiftShowsPrec ''Legacy.ModuleDef)
instance Show1 Legacy.Module where
  liftShowsPrec = $(makeLiftShowsPrec ''Legacy.Module)
instance Show1 Legacy.Governance where
  liftShowsPrec = $(makeLiftShowsPrec ''Legacy.Governance)
instance Show1 Legacy.Step where
  liftShowsPrec = $(makeLiftShowsPrec ''Legacy.Step)

runTranslateM :: TranslateM a -> Either String a
runTranslateM a = runExcept (evalStateT (runReaderT a 0) [])

decodeModuleData :: ByteString -> Maybe (ModuleData CoreBuiltin ())
decodeModuleData bs = do
  obj <- JD.decodeStrict' bs
  either (const Nothing) Just (runTranslateM (fromLegacyModuleData obj))


fromLegacyModuleData
  :: Legacy.ModuleData (Legacy.Ref' Legacy.PersistDirect)
  -> TranslateM (ModuleData CoreBuiltin ())
fromLegacyModuleData (Legacy.ModuleData md mref mdeps) = do
  case md of
    Legacy.MDModule m -> do
      let mh = fromLegacyModuleHash (Legacy._mHash m)
      deps <- fromLegacyDeps mh mdeps
      m' <- fromLegacyModule mh m mref
      pure (ModuleData m' deps)
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
fromLegacyInterface mh (Legacy.Interface n imp) mref = do
  let n' = fromLegacyModuleName n
      use' = fmap fromLegacyUse imp
  defs <- traverse (fromLegacyInterfaceDefRef mh) $ HM.elems mref
  pure (Interface n' defs use' mh ())

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
  Arg n mty <- fromLegacyArg arg'
  cval <- case cv of
    Legacy.CVRaw _ -> throwError "fromLegacyConstDef: invariant"
    Legacy.CVEval _ t -> fromLegacyTerm mh (Right <$> t) >>= \case
      InlineValue pv _ -> pure pv
      _ -> throwError "fromLegacyConstDef: invariant, not InlineValue"
  pure (DefConst n mty (EvaledConst cval) ())

fromLegacySchemaDef
  :: Legacy.TypeName
  -> Maybe Legacy.ModuleName
  -> [Legacy.Arg (Legacy.Term LegacyRef)]
  -> TranslateM (DefSchema Type ())
fromLegacySchemaDef (Legacy.TypeName sn) _mn largs = do
  let largs' = (fmap.fmap.fmap) Right largs
  args <- traverse fromLegacyArg largs'
  schema <- traverse (\(Arg n mty) -> pure (Field n, fromMaybe TyAny mty)) args
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
      Legacy.TSchema (Legacy.TypeName n) mmn f -> do
        let qn = QualifiedName n . fromLegacyModuleName <$> mmn
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
  catchError (go mn ret args) $ \e -> do
    traceM $ "For function " <> show mn <> show n
    traceM $ show body
    throwError e

  where
    lamFromArgs args b = case args of
      [] -> Nullary b ()
      (x:xs) -> Lam (x :| xs) b ()
    go mn ret args =
      case dt of
        Legacy.Defun -> do
          body' <- local (+ fromIntegral (length args)) . fixTreeIndices =<< fromLegacyBodyForm' mh args body
          pure $ Dfun $ Defun
            n  -- defun name
            args -- args
            (Just ret)
            (lamFromArgs args body')
            () -- info
        Legacy.Defpact -> do
          steps' <- fromLegacyStepForm' mh args body >>= local (+ fromIntegral (length args)) . (traversed.traverseDefPactStep) fixTreeIndices
          pure $ DPact (DefPact n args (Just ret) steps' ())
        Legacy.Defcap -> do
            body' <- local (+ fromIntegral (length args)) . fixTreeIndices =<< fromLegacyBodyForm' mh args body
            meta' <- case meta of
              -- Note: Empty `meta` implies the cap is
              -- unmanaged.
              Just meta' -> fromLegacyDefMeta mn mh args meta'
              Nothing -> pure Unmanaged
            pure $ DCap (DefCap n args (Just ret) body' meta' ())


fromLegacyInterfDef
  :: Legacy.Def (Either CoreTerm LegacyRef)
  -> TranslateM (EvalIfDef CoreBuiltin ())
fromLegacyInterfDef (Legacy.Def (Legacy.DefName n) _mn dt funty _body meta) = do
  ret <- fromLegacyType (Legacy._ftReturn funty)
  let lArgs = Legacy._ftArgs funty
  args <- traverse fromLegacyArg lArgs
  case dt of
    Legacy.Defun -> do
      pure $ IfDfun $ IfDefun
        n  -- defun name
        args -- args
        (Just ret)
        () -- info
    Legacy.Defpact -> do
      pure $ IfDPact (IfDefPact n args (Just ret) ())
    Legacy.Defcap -> do
        meta' <- case meta of
          -- Note: Empty `meta` implies the cap is
          -- unmanaged.
          Just meta' -> fromLegacyDefMetaInterface args meta'
          Nothing -> pure Unmanaged
        pure $ IfDCap (IfDefCap n args (Just ret) meta' ())

fromLegacyDefMetaInterface
  :: [Arg Type]
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
        f' -> throwError $ "invariant: interface defmeta invariant1 violated " <> show f'
  Legacy.DMDefcap Legacy.DefcapEvent -> pure DefEvent


fromLegacyDefMeta
  :: ModuleName
  -> ModuleHash
  -> [Arg Type]
  -> Legacy.DefMeta (Legacy.Term (Either CorePreNormalizedTerm LegacyRef))
  -> TranslateM (DefCapMeta (FQNameRef Name))
fromLegacyDefMeta mn mh args = \case
  Legacy.DMDefcap (Legacy.DefcapManaged m) -> case m of
    Nothing -> pure (DefManaged AutoManagedMeta)
    Just (p, f) -> case findIndex (\x -> _argName x == p) args of
      Nothing -> throwError "fromLegacyDefMeta: invariant, index not found!"
      Just idx' -> case unTVar f of
        Legacy.TDef td -> do
          let (Legacy.DefName dn) = Legacy._dDefName td
          let fqn = FullyQualifiedName mn dn mh
          pure (DefManaged (DefManagedMeta (idx', p) (FQName fqn)))
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

  defs <- traverse (fromLegacyDefRef mh) $ HM.elems depMap
  pure (Module mn gov defs (S.fromList blessed) imps impl mhash ())


fromLegacyBodyForm'
  :: ModuleHash -- parent module hash
  -> [Arg Type]
  -> Scope Int Legacy.Term (Either CorePreNormalizedTerm LegacyRef)
  -> TranslateM CorePreNormalizedTerm
fromLegacyBodyForm' mh args body = local (+ fromIntegral (length args)) $ do
  currDepth <- ask
  case debruijnize (length args) currDepth args body of
    Legacy.TList li _ -> traverse (fromLegacyTerm mh) (reverse (V.toList li)) >>= \case
      x:xs -> pure $ foldl' (\a b -> Sequence b a ()) x xs
      _ -> throwError "fromLegacyBodyForm': invariant 1"
    _ -> throwError "fromLegacyBodyForm': invariant 2"


fromLegacyStepForm'
  :: ModuleHash
  -> [Arg Type]
  -> Scope Int Legacy.Term (Either CorePreNormalizedTerm LegacyRef)
  -> TranslateM (NonEmpty (Step (Name, DeBruijn) Type CoreBuiltin ()))
fromLegacyStepForm' mh args body = do
  currDepth <- ask
  case debruijnize (length args) currDepth args body of
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
  :: Int
  -> DeBruijn
  -> _
  -> Scope Int Legacy.Term (Either CorePreNormalizedTerm LegacyRef)
  -> Legacy.Term (Either CorePreNormalizedTerm LegacyRef)
debruijnize totalLen depth args = Bound.instantiate $ \i ->
  -- Todo: get the argname from the provided args
    let boundVar = NBound $ fromIntegral (totalLen - i - 1)
        Arg n _ = args !! i
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
    imp = fmap fromLegacyModuleName (fromMaybe [] mmn)
    in pure (PModRef $ ModRef mn' imp Nothing)


fromLegacyPersistDirect
  :: Legacy.PersistDirect
  -> TranslateM CorePreNormalizedTerm
fromLegacyPersistDirect = \case
  Legacy.PDValue v ->
    liftEither $ (`InlineValue` ()) <$> fromLegacyPactValue v
  Legacy.PDNative (Legacy.NativeDefName n)
    | n == "enforce" -> pure (Conditional (CEnforce unitValue unitValue) ())
    | n == "enforce-one" -> pure (Conditional (CEnforceOne unitValue [unitValue]) ())
    | n == "if" -> pure (Conditional (CIf unitValue unitValue unitValue) ())
    | n == "and" -> pure (Conditional (CAnd unitValue unitValue) ())
    | n == "or" -> pure (Conditional (COr unitValue unitValue) ())
    | n == "with-capability" -> pure (CapabilityForm (WithCapability unitValue unitValue) ())
    | n == "create-capability" -> pure (CapabilityForm (CreateUserGuard unitName [unitValue]) ()) -- TODO: Jose ?
    | n == "create-user-guard" -> pure (CapabilityForm (CreateUserGuard unitName [unitValue]) ())
    | n == "try" -> pure (Try unitValue unitValue ())

    | n == "CHARSET_ASCII" -> pure (Constant (LInteger 0) ()) -- see Desugar.hs
    | n == "CHARSET_LATIN1" -> pure (Constant (LInteger 1) ())
    | n == "constantly" -> do
        let c1 = Arg "#constantlyA1" Nothing
        let c2 = Arg "#constantlyA2" Nothing
        d <- ask
        pure $ Lam (c1 :| [c2]) (Var (Name "#constantlyA1" (NBound 1), d) ()) ()

    | otherwise -> case M.lookup n coreBuiltinMap of
        Just b -> pure (Builtin b ())
        _ -> throwError $ "fromLegacyPersistDirect: invariant -> " <> show n
  Legacy.PDFreeVar fqn -> let
    fqn' = fromLegacyFullyQualifiedName fqn
    in pure $ Var (fqnToName fqn', 0) ()
  where
    -- Note: unit* is used as placeholder, which gets replaced in `fromLegacyTerm`
    unitValue = InlineValue PUnit ()
    unitName = (Name "unitName" (NBound 0), 0)

{-
{"balance" := balance, "rob" := rob}
  (+ balance rob) ==>

(let
  _a(n+1) = (at "balance" someObj)
  _an = (at "rob" someObj)
  in
  (+ _a1 a0)
  )
-}
objBindingToLet
  :: ModuleHash
  -> [Legacy.BindPair (Legacy.Term (Either CorePreNormalizedTerm LegacyRef))]
  -> Scope Int Legacy.Term (Either CorePreNormalizedTerm LegacyRef)
  -> TranslateM CorePreNormalizedTerm
objBindingToLet mh bps scope = do
  let len = length bps -- 1
  currDepth <- ask
  args' <- traverse (fromLegacyArg . Legacy._bpArg) bps
  term' <- local (+ 1) $ fromLegacyBodyForm' mh args' scope
  (finalBody', _) <- foldrM (mkAccess (succ currDepth)) (term', fromIntegral len) bps
  pure $ baseLam finalBody'
  where
  mkAccess currDepth bp (!body, !len) = do
    let !len' = len - 1 -- 0
    body' <- bpToObjLet (currDepth + len') len' bp body
    pure (body', len')
  objBindVar = "`objBind"
  baseLam term =
    Lam (pure (Arg objBindVar Nothing)) term ()
  bpToObjLet currDepth objVarIx (Legacy.BindPair (Legacy.Arg n _) v) body = do
    fromLegacyTerm mh v >>= \case
      fieldLit@(Constant (LString _) _) -> do
        let larg = Arg n Nothing
            accessObj = Var (Name objBindVar (NBound objVarIx), currDepth) ()
            accessTerm = App (Builtin CoreAt ()) [fieldLit, accessObj] ()
        pure $ Let larg accessTerm body ()
      _ -> throwError "fromLegacyBindPair: Invariant"

desugarApp :: DesugarBuiltin b => Term n dt b i -> [Term n dt b i] -> i -> Term n dt b i
desugarApp fn args i = case fn of
  Builtin b _ -> desugarAppArity i b args
  _ -> App fn args i

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
    -- args' <- traverse (fromLegacyTerm mh) args
    case fn' of
      Builtin b _ -> case b of
        CoreBind -> case args of
          [bObj, Legacy.TBinding bps scope _] -> do
            bObj' <- fromLegacyTerm mh bObj
            lam <- objBindingToLet mh bps scope
            pure (App fn' [bObj', lam] ())

          _ -> throwError $ "invariant failure: CoreBind"
        CoreWithRead -> case args of
          [tbl, rowkey, Legacy.TBinding bps scope _] -> do
            tbl' <- fromLegacyTerm mh tbl
            rowkey' <- fromLegacyTerm mh rowkey
            lam <- objBindingToLet mh bps scope
            pure (App fn' [tbl', rowkey', lam] ())

          _ -> throwError "invariant failure: CoreWithRead"
        CoreWithDefaultRead -> case args of
          [tbl, rowkey, defaultObj, Legacy.TBinding bps scope _] -> do
            tbl' <- fromLegacyTerm mh tbl
            rowkey' <- fromLegacyTerm mh rowkey
            defaultObj' <- fromLegacyTerm mh defaultObj
            lam <- objBindingToLet mh bps scope
            pure (App fn' [tbl', rowkey', defaultObj', lam] ())

          _ -> throwError "invariant failure: CoreWithDefaultRead"
        CoreResume -> case args of
          [Legacy.TBinding bps scope _] -> do
            lam <- objBindingToLet mh bps scope
            pure (App fn' [lam] ())

          _ -> throwError "invariant failure: CoreWithRead"
        -- [HOF Translation]
        -- Note: The following sections of translation are explained as follows:
        -- we transform, for example `(map (+ k) other-arg)` into
        -- `(map (lambda (arg) (+ k arg)) other-arg)
        -- This eta expansion is necessary to
        _ | b `elem` higherOrder1Arg
          , Legacy.TApp (Legacy.App mapOperator mapOperands): xs <- args -> do
          d <- ask
          let injectedArg = (Var (Name "" (NBound 0), d + 1) () :: CorePreNormalizedTerm)
          let containingLam e = Lam (pure (Arg "" Nothing)) e ()
          (mapOperator', mapOperands') <- local (+ 1) $ (,) <$> fromLegacyTerm mh mapOperator <*> traverse (fromLegacyTerm mh) mapOperands
          let body = containingLam (desugarApp mapOperator' (mapOperands' ++ [injectedArg]) ())
          xs' <- traverse (fromLegacyTerm mh) xs
          pure (App fn' (body:xs') ())

        _ | b `elem` higherOrder2Arg
          , Legacy.TApp (Legacy.App mapOperator mapOperands): xs <- args -> do
          d <- ask
          let injectedArg1 = (Var (Name "" (NBound 1), d + 2) () :: CorePreNormalizedTerm)
              injectedArg2 = (Var (Name "" (NBound 0), d + 2) () :: CorePreNormalizedTerm)
          let containingLam e = Lam (pure (Arg "" Nothing)) e ()
          (mapOperator', mapOperands') <- local (+ 2) $ (,) <$> fromLegacyTerm mh mapOperator <*> traverse (fromLegacyTerm mh) mapOperands
          let body = containingLam (desugarApp mapOperator' (mapOperands' ++ [injectedArg1, injectedArg2]) ())
          xs' <- traverse (fromLegacyTerm mh) xs
          pure (App fn' (body:xs') ())

        _ -> do
          args' <- traverse (fromLegacyTerm mh) args
          pure (desugarAppArity () b args')

      -- TODO: Add addtional cases
      Conditional CEnforce{} _ -> traverse (fromLegacyTerm mh) args >>= \case
        [t1,t2] -> pure (Conditional (CEnforce t1 t2) ())
        [_t1]    -> error "cenforce case TODO: JOSE"
        _ -> error "TODO"

      Conditional CIf{} _ -> traverse (fromLegacyTerm mh) args >>= \case
        [cond, b1, b2] -> pure (Conditional (CIf cond b1 b2) ())
        _ -> error "if case TODO: JOSE"

      CapabilityForm WithCapability{} _ -> traverse (fromLegacyTerm mh) args >>= \case
        [t1, t2] -> pure (CapabilityForm (WithCapability t1 t2) ())
        _ -> error "withcapability case TODO: JOSE"

      CapabilityForm CreateUserGuard{} _ ->
        traverse (fromLegacyTerm mh) args >>= \case
        -- TODO case is wrong
        [App (Var n _) cugargs _] ->
          pure (CapabilityForm (CreateUserGuard n cugargs) ())
        t -> error $ "createuserguard case TODO: JOSE" <> show t

      Try _ _ _ -> traverse (fromLegacyTerm mh) args >>= \case
        [t1, t2] -> pure (Try t1 t2 ())
        t -> error $ "try case TODO: JOSE" <> show t

      _ -> do
        args' <- traverse (fromLegacyTerm mh) args
        pure (App fn' args' ())


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
      foldrM goLet body' bps
      where
      goLet (Legacy.BindPair arg val) rest = do
        arg' <- fromLegacyArg arg
        v' <- fromLegacyTerm mh val
        pure $ Let arg' v' rest ()
    _ -> throwError "unsupported: object binds outside of designated callsite"

  Legacy.TObject (Legacy.Object o _ _) -> do
   let m = M.toList (Legacy._objectMap o)
   obj <- traverse (\(Legacy.FieldKey f, t) -> (Field f,) <$> fromLegacyTerm mh t) m
   pure (ObjectLit obj ())

  -- Note: this does not show up in the prod database
  -- Legacy.TNative{} -> throwError "fromLegacyTerm: invariant"

  Legacy.TLiteral l ->
    pure $ either (`Constant` ()) (`InlineValue` ()) $ fromLegacyLiteral l

  Legacy.TTable (Legacy.TableName tbl) mn mh' _ -> let
    mn' = fromLegacyModuleName mn
    mh'' = fromLegacyModuleHash mh'
    nk = NTopLevel mn' mh''
    in pure (Var (Name tbl nk, 0) ())

  -- Note: impossible
  Legacy.TModule{} -> throwError "fromLegacyTerm: invariant"

  -- Note: impossible
  Legacy.TStep{} -> throwError "fromLegacyTerm: invariant"

  -- Note: TDef may show up in some old modules
  Legacy.TDef d@(Legacy.Def n mn _dt (Legacy.FunType _args _) _body _) -> do
    let mn' = fromLegacyModuleName mn
        dn  = Legacy._unDefName n
        h = CBOR.encodeModuleName mn' <> T.encodeUtf8 dn <> CBOR.encodeModuleHash mh
        newHash = unsafeBsToModuleHash h
        nk = NTopLevel mn' newHash
        name = Name dn nk

    def <- fromLegacyDef mh d
    modify' (def:)
    depth <- ask
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
    imp = fmap fromLegacyModuleName (fromMaybe [] mmn)
    in pure (InlineValue (PModRef (ModRef mn' imp Nothing)) ())

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
    _ -> error "invariant"
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
  -> TranslateM (Arg Type)
fromLegacyArg (Legacy.Arg n ty) = Arg n . Just <$> fromLegacyType ty

fromLegacyType
  :: Legacy.Type (Legacy.Term (Either CorePreNormalizedTerm LegacyRef))
  -> TranslateM Type
fromLegacyType = \case
  Legacy.TyAny -> pure TyAny
  Legacy.TyList Legacy.TyAny -> pure TyAnyList
  Legacy.TyList t -> TyList <$> fromLegacyType t
  Legacy.TyPrim prim -> pure $ TyPrim (fromLegacyPrimType prim)
  Legacy.TySchema s ty _ -> fromLegacySchema s ty
  Legacy.TyFun _ -> error "tyfun"
  Legacy.TyModule m -> fromLegacyTypeModule m
  Legacy.TyUser t -> throwError $ "fromLegacyType: TyUser invariant: " <> show t
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
    Legacy.TSchema (Legacy.TypeName n) mmn f -> do
      let qn = QualifiedName n . fromLegacyModuleName <$> mmn
      args <- traverse (\(Legacy.Arg n' ty') -> (Field n',) <$> fromLegacyType ty') f
      pure (TyTable (Schema qn (M.fromList args)))
    _ -> throwError "fromLegacySchema: invariant 1"
  (Legacy.TyObject, Legacy.TyUser t) -> case unTVar t of
    Legacy.TSchema (Legacy.TypeName n) mmn f -> do
      let qn = QualifiedName n . fromLegacyModuleName <$> mmn
      args <- traverse (\(Legacy.Arg n' ty') -> (Field n',) <$> fromLegacyType ty') f
      pure (TyObject (Schema qn (M.fromList args)))
    _ -> throwError "fromLegacySchema: invariant tyobject"

  (Legacy.TyObject, Legacy.TyAny) -> pure TyAnyObject

  (Legacy.TyBinding, _) -> throwError "tybinding"
  -- (Legacy.TyObject, Legacy.TySchema _ _ _) -> throwError "tySchema 1"
  -- (Legacy.TyTable, Legacy.TySchema _ _ _) -> throwError "tySchema 2"

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
    (\(k,v) -> (fromLegacyPactId k,) <$> fromLegacyDefPactExec' (fromNestedPactExec rb v))
    (M.toList nest)
  pure $
    DefPactExec sc y' step (fromLegacyPactId pid)
    cont'
    rb
    (M.fromList nest')

fromLegacyDefPactExec
  :: Maybe Legacy.PactExec
  -> Either String (Maybe DefPactExec)
fromLegacyDefPactExec = \case
  Nothing -> pure Nothing
  Just n -> Just <$> fromLegacyDefPactExec' n


fromNestedPactExec :: Bool -> Legacy.NestedPactExec -> Legacy.PactExec
fromNestedPactExec rollback (Legacy.NestedPactExec stepCount yield exec step pid cont nested) =
  Legacy.PactExec stepCount yield exec step pid cont rollback nested

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
    _ -> error "todo: jose, other cases relevant?"

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
    currDepth <- ask
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
    Lam args <$> local (+ fromIntegral (length args)) (fixTreeIndices term) <*> pure i
  Let arg e1 e2 i -> do
    e1' <- fixTreeIndices e1
    e2' <- local (+ 1) $ fixTreeIndices e2
    pure $ Let arg e1' e2' i
  App fn args i ->
    App <$> fixTreeIndices fn <*> traverse fixTreeIndices args <*> pure i
  Conditional bfn i ->
    Conditional <$> traverse fixTreeIndices bfn <*> pure i
  Builtin b i ->
    pure $ Builtin b i
  Constant l i ->
    pure $ Constant l i
  Sequence l r i ->
    Sequence <$> fixTreeIndices l <*> fixTreeIndices r <*> pure i
  Nullary t i -> Nullary <$> fixTreeIndices t <*> pure i
  ListLit args i ->
    ListLit <$> traverse fixTreeIndices args <*> pure i
  Try c e i ->
    Try <$> fixTreeIndices c <*> fixTreeIndices e <*> pure i
  ObjectLit fields i ->
    ObjectLit <$> (traversed._2) fixTreeIndices fields <*> pure i
  CapabilityForm cf i -> fmap (`CapabilityForm` i) $ case cf of
    CreateUserGuard (n, _) e ->
      CreateUserGuard n <$> traverse fixTreeIndices e
    WithCapability cap body ->
      WithCapability <$> fixTreeIndices cap <*> fixTreeIndices body
  InlineValue p i ->
    pure $ InlineValue p i

