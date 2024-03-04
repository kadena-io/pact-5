-- |
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE PartialTypeSignatures  #-}
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

--import Control.Lens
import Pact.Core.Names
import Pact.Core.Persistence
import Pact.Core.Guards
import Pact.Core.DefPacts.Types
import Pact.Core.Namespace
import Pact.Core.PactValue

import Data.List.NonEmpty(NonEmpty(..))
import Data.List (findIndex)
import Data.ByteString (ByteString)
import qualified Data.Text.Encoding as T
import Data.Maybe (fromMaybe)

import Pact.Core.Builtin
import Pact.Core.ModRefs
import Pact.Core.Literal
import Pact.Core.Type
import Pact.Core.Imports
import Pact.Core.IR.Desugar
import Pact.Core.Capabilities
import Pact.Core.ChainData
import qualified Pact.JSON.Decode as JD
import qualified Data.Text as T
import Data.Map.Strict(Map)
import Control.Monad

import Pact.Core.IR.Term
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import qualified Pact.Core.Serialise.LegacyPact.Types as Legacy
import Pact.Core.Hash
import qualified Pact.Core.Serialise.CBOR_V1 as CBOR
import qualified Data.List.NonEmpty as NE
import Control.Monad.State.Strict
import Control.Monad.Except

import Bound (Scope)
import qualified Bound
import Data.Foldable (foldl')


type LegacyRef = Legacy.Ref' Legacy.PersistDirect
type CoreTerm = EvalTerm CoreBuiltin ()
type CoreDef = EvalDef CoreBuiltin ()


type TranslateM = StateT [CoreDef] (Except String)

runTranslateM :: TranslateM a -> Either String a
runTranslateM a = runExcept (evalStateT a [])

decodeModuleData :: ByteString -> Maybe (ModuleData CoreBuiltin ())
decodeModuleData bs = do
  obj <- JD.decodeStrict' bs
  let mhash = placeholderHash
  either (const Nothing) Just (runTranslateM (fromLegacyModuleData mhash obj))


fromLegacyModuleData
  :: ModuleHash
  -> Legacy.ModuleData (Legacy.Ref' Legacy.PersistDirect)
  -> TranslateM (ModuleData CoreBuiltin ())
fromLegacyModuleData mh (Legacy.ModuleData md mref mdeps) = do
  deps <- fromLegacyDeps mh mdeps
  case md of
    Legacy.MDModule m -> do
      m' <- fromLegacyModule mh m mref
      pure (ModuleData m' deps)
    Legacy.MDInterface i -> do
      i'<- fromLegacyInterface mh i mref
      pure (InterfaceData i' deps)


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

  Legacy.Ref t -> throwError $ "fromLegacyDefRef: " <>  show t
  Legacy.Direct _d -> throwError "fromLegacyDefRef: invariant Direct"

fromLegacyTableDef
  :: Legacy.TableName
  -> Legacy.ModuleName
  -> Legacy.ModuleHash
  -> Legacy.Type (Legacy.Term LegacyRef)
  -> TranslateM CoreDef
fromLegacyTableDef (Legacy.TableName tn) _mn _mh ty = do
  let ty' = (fmap.fmap) Right ty
  fromLegacyType ty' >>= \case
    TyTable s -> pure (DTable (DefTable tn (ResolvedTable s) ()))
    t -> throwError $ "fromLegacyDefTable: invariant: " <> show t

fromLegacyInterfaceDefRef :: ModuleHash -> LegacyRef -> TranslateM (EvalIfDef CoreBuiltin ())
fromLegacyInterfaceDefRef mh = \case
  Legacy.Ref (Legacy.TDef d) ->
    fromLegacyInterfDef mh $ Right <$> d
  _ -> throwError "fromLegacyDefRef: invariant"


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
  -> Legacy.Def (Either CoreTerm LegacyRef)
  -> TranslateM CoreDef
fromLegacyDef mh (Legacy.Def (Legacy.DefName n) _mn dt funty body meta) = do
  ret <- fromLegacyType (Legacy._ftReturn funty)
  let lArgs = Legacy._ftArgs funty
  args <- traverse fromLegacyArg lArgs
  case dt of
    Legacy.Defun -> do
      body' <- fromLegacyBodyForm' mh args body
      pure $ Dfun $ Defun
        n  -- defun name
        args -- args
        (Just ret)
        body'
        () -- info
    Legacy.Defpact -> do
      steps' <- fromLegacyStepForm' mh args body
      pure $ DPact (DefPact n args (Just ret) steps' ())
    Legacy.Defcap -> do
        body' <- fromLegacyBodyForm' mh args body
        meta' <- case meta of
          -- Note: Empty `meta` implies the cap is
          -- unmanaged.
          Just meta' -> fromLegacyDefMeta mh args meta'
          Nothing -> pure Unmanaged
        pure $ DCap (DefCap n args (Just ret) body' meta' ())


fromLegacyInterfDef
  :: ModuleHash -- parent module hash
  -> Legacy.Def (Either CoreTerm LegacyRef)
  -> TranslateM (EvalIfDef CoreBuiltin ())
fromLegacyInterfDef mh (Legacy.Def (Legacy.DefName n) _mn dt funty _body meta) = do
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
          Just meta' -> fromLegacyDefMetaInterface mh args meta'
          Nothing -> pure Unmanaged
        pure $ IfDCap (IfDefCap n args (Just ret) meta' ())

fromLegacyDefMetaInterface
  :: ModuleHash
  -> [Arg Type]
  -> Legacy.DefMeta (Legacy.Term (Either CoreTerm LegacyRef))
  -> TranslateM (DefCapMeta BareName)
fromLegacyDefMetaInterface mh args
  = \case
  Legacy.DMDefcap (Legacy.DefcapManaged m) -> case m of
    Nothing -> pure (DefManaged AutoManagedMeta)
    Just (p, f) -> case findIndex (\x -> _argName x == p) args of
      Nothing -> throwError "fromLegacyDefMeta: invariant, index not found!"
      Just idx' -> do
        f' <- fromLegacyTerm mh f >>= \case
          Var (Name _n (NTopLevel _mn _mh')) _ -> error "todo: Jose ??"
          _ -> throwError ""
        pure (DefManaged $ DefManagedMeta (idx', p) f')
  Legacy.DMDefcap Legacy.DefcapEvent -> pure DefEvent



fromLegacyDefMeta
  :: ModuleHash
  -> [Arg Type]
  -> Legacy.DefMeta (Legacy.Term (Either CoreTerm LegacyRef))
  -> TranslateM (DefCapMeta (FQNameRef Name))
fromLegacyDefMeta mh args = \case
  Legacy.DMDefcap (Legacy.DefcapManaged m) -> case m of
    Nothing -> pure (DefManaged AutoManagedMeta)
    Just (p, f) -> case findIndex (\x -> _argName x == p) args of
      Nothing -> throwError "fromLegacyDefMeta: invariant, index not found!"
      Just idx' -> do
        f' <- fromLegacyTerm mh f >>= \case
          Var (Name n (NTopLevel mn mh')) _ -> pure (FullyQualifiedName mn n mh')
          _ -> throwError "fromLegacyDefMeta: invariant, expected FullyQualifiedName!"
        pure (DefManaged $ DefManagedMeta (idx', p) (FQName f'))
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
  :: Foldable t
  => ModuleHash -- parent module hash
  -> t a
  -> Scope Int Legacy.Term (Either CoreTerm LegacyRef)
  -> TranslateM CoreTerm
fromLegacyBodyForm' mh args body = do
  case debruijnize (length args) body of
    Legacy.TList li _ -> traverse (fromLegacyTerm mh) (reverse (V.toList li)) >>= \case
      x:xs -> pure $ foldl' (\a b -> Sequence b a ()) x xs
      _ -> throwError "fromLegacyBodyForm': invariant"
    _ -> throwError "fromLegacyBodyForm': invariant"


fromLegacyStepForm'
  :: Foldable t
  => ModuleHash
  -> t a
  -> Scope Int Legacy.Term (Either CoreTerm LegacyRef)
  -> TranslateM (NonEmpty (EvalStep CoreBuiltin ()))
fromLegacyStepForm' mh args body = case debruijnize (length args) body of
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
  -> Legacy.Step (Legacy.Term (Either CoreTerm LegacyRef))
  -> TranslateM (EvalStep CoreBuiltin ())
fromLegacyStep mh (Legacy.Step _ t mrb) = do
  t' <- fromLegacyTerm mh t
  case mrb of
    Nothing -> pure (Step t')
    Just rb ->
      StepWithRollback t' <$> fromLegacyTerm mh rb

debruijnize
  :: Int
  -> Scope Int Legacy.Term (Either CoreTerm LegacyRef)
  -> Legacy.Term (Either CoreTerm LegacyRef)
debruijnize totalLen = Bound.instantiate $ \i ->
  -- Todo: get the argname from the provided args
    let boundVar = NBound $ fromIntegral (totalLen - i - 1)
    in Legacy.TVar (Left (Var (Name "_" boundVar) ()))


fromLegacyPactValue :: Legacy.PactValue -> Either String PactValue
fromLegacyPactValue = \case
  Legacy.PLiteral l -> pure $ fromLegacyLiteral l
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
  -> Either String CoreTerm
fromLegacyPersistDirect = \case
  Legacy.PDValue v -> (`InlineValue` ()) <$> fromLegacyPactValue v
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

    | otherwise -> case M.lookup n coreBuiltinMap of
        Just b -> pure (Builtin b ())
        _ -> Left $ "fromLegacyPersistDirect: invariant -> " <> show n
  Legacy.PDFreeVar fqn -> let
    fqn' = fromLegacyFullyQualifiedName fqn
    in pure $ Var (fqnToName fqn') ()
  where
    -- Note: unit* is used as placeholder, which gets replaced in `fromLegacyTerm`
    unitValue = InlineValue PUnit ()
    unitName = Name "unitName" (NBound 0)

fromLegacyTerm
  :: ModuleHash
  -> Legacy.Term (Either CoreTerm LegacyRef)
  -> TranslateM CoreTerm
fromLegacyTerm mh = \case
  Legacy.TVar n -> case n of
    Left t -> pure t
    Right v -> case v of
      Legacy.Direct v' -> liftEither $ fromLegacyPersistDirect v'
      Legacy.Ref t -> fromLegacyTerm mh (Right <$> t)

  Legacy.TApp (Legacy.App fn args) -> do
    fn' <- fromLegacyTerm mh fn
    args' <- traverse (fromLegacyTerm mh) args
    case (fn', args') of
      (Builtin b _, _)
        -- | b == CoreMap -> error "todo: implement all special forms"
        | otherwise -> pure (desugarAppArity () b args')

      -- TODO: Add addtional cases
      (Conditional CEnforce{} _, arg) -> case arg of
        [t1,t2] -> pure (Conditional (CEnforce t1 t2) ())
        [_t1]    -> error "cenforce case TODO: JOSE"
        _ -> error "TODO"

      (Conditional CIf{} _, arg) -> case arg of
        [cond, b1, b2] -> pure (Conditional (CIf cond b1 b2) ())
        _ -> error "if case TODO: JOSE"

      (CapabilityForm WithCapability{} _, arg) -> case arg of
        [t1, t2] -> pure (CapabilityForm (WithCapability  t1 t2) ())
        _ -> error "withcapability case TODO: JOSE"

      (CapabilityForm CreateUserGuard{} _, arg) -> case arg of
        -- TODO case is wrong
        [t2] -> pure (CapabilityForm (CreateUserGuard  undefined [t2]) ())
        t -> error $ "createuserguard case TODO: JOSE" <> show t

      (Try t1 t2 _, _) -> pure (Try t1 t2 ())

      (var@Var{},_) -> pure (App var args' ())
      _ -> throwError "fromLegacyTerm: invariant"


  Legacy.TLam (Legacy.Lam _ (Legacy.FunType args _) body) -> do
    args' <- traverse fromLegacyArg args
    body' <- fromLegacyBodyForm' mh args body
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
  Legacy.TBinding _bp _body _ ->
    pure (Var undefined ())
    --error "todo: bind pairs"

  Legacy.TObject (Legacy.Object o _ _) -> do
   let m = M.toList (Legacy._objectMap o)
   obj <- traverse (\(Legacy.FieldKey f, t) -> (Field f,) <$> fromLegacyTerm mh t) m
   pure (ObjectLit obj ())

  -- Note: this does not show up in the prod database
  Legacy.TNative{} -> throwError "fromLegacyTerm: invariant"

  Legacy.TLiteral l ->
    pure $ InlineValue (fromLegacyLiteral l) ()

  Legacy.TTable (Legacy.TableName tbl) mn mh' _ -> let
    mn' = fromLegacyModuleName mn
    mh'' = fromLegacyModuleHash mh'
    nk = NTopLevel mn' mh''
    in pure (Var (Name tbl nk) ())

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
    pure (Var name ())

  Legacy.TDynamic mr dm -> do
    mr' <- fromLegacyTerm mh mr
    case mr' of
     InlineValue (PModRef (ModRef m imp _)) _ -> case dm of
       Legacy.TVar (Right (Legacy.Ref (Legacy.TDef d))) -> do
         let
           dname = Legacy._unDefName (Legacy._dDefName d)
           name = Name dname (NModRef m imp)
         pure (Var name ())
       _ -> throwError "fromLegacyTerm: TDynamic dm invariant"
     _ -> throwError "fromLegacyTerm: TDynamic invariant"

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
  -> Legacy.Guard (Legacy.Term (Either CoreTerm LegacyRef))
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
  -> PactValue
fromLegacyLiteral = \case
  Legacy.LString s -> PLiteral (LString s)
  Legacy.LInteger i -> PLiteral (LInteger i)
  Legacy.LDecimal d -> PLiteral (LDecimal d)
  Legacy.LBool b -> PLiteral (LBool b)
  Legacy.LTime l -> PTime l

fromLegacyUse
  :: Legacy.Use
  -> Import
fromLegacyUse (Legacy.Use mn mh imp) = let
  mn' = fromLegacyModuleName mn
  mh' = fromLegacyModuleHash <$> mh
  imp' = V.toList <$> imp
  in Import mn' mh' imp'

fromLegacyArg
  :: Legacy.Arg (Legacy.Term (Either CoreTerm LegacyRef))
  -> TranslateM (Arg Type)
fromLegacyArg (Legacy.Arg n ty) = Arg n . Just <$> fromLegacyType ty

fromLegacyType
  :: Legacy.Type (Legacy.Term (Either CoreTerm LegacyRef))
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
  :: Legacy.Term (Either CoreTerm LegacyRef)
  -> Legacy.Term (Either CoreTerm LegacyRef)
unTVar = \case
  Legacy.TVar (Right (Legacy.Ref t)) -> unTVar (Right <$> t)
  d -> d

fromLegacyTypeModule
  :: Maybe [Legacy.Term (Either CoreTerm LegacyRef)]
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
  -> Legacy.Type (Legacy.Term (Either CoreTerm LegacyRef))
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

  (Legacy.TyBinding, _) -> throwError "tybinding"
  -- (Legacy.TyObject, Legacy.TySchema _ _ _) -> throwError "tySchema 1"
  -- (Legacy.TyTable, Legacy.TySchema _ _ _) -> throwError "tySchema 2"

  (s,_) -> throwError $ "fromLegacySchema: invariant 2: " <> show s


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
