{-# LANGUAGE GADTs #-}

module Pact.Core.IR.ModuleHashing
 ( hashInterfaceAndReplace
 , hashModuleAndReplace
 , hashTopLevel
 ) where

import Control.Lens
import Data.Decimal(DecimalRaw(..))
import Data.List(intersperse)
import Data.Foldable(fold)
import Data.ByteString.Builder (Builder)

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector as V
import qualified Pact.Time as PactTime

import Pact.Core.Capabilities
import Pact.Core.Builtin
import Pact.Core.Type
import Pact.Core.Guards
import Pact.Core.IR.Term
import Pact.Core.Hash
import Pact.Core.Names
import Pact.Core.Literal
import Pact.Core.PactValue
import Pact.Core.ModRefs
import Pact.Core.Imports

hashTopLevel :: IsBuiltin b => TopLevel Name Type b i -> TopLevel Name Type b i
hashTopLevel = \case
  TLTerm t -> TLTerm t
  TLModule m -> TLModule $ hashModuleAndReplace m
  TLInterface iface -> TLInterface $ hashInterfaceAndReplace iface
  TLUse u i -> TLUse u i

hashModuleAndReplace :: IsBuiltin b => Module Name Type b i -> Module Name Type b i
hashModuleAndReplace m@(Module mname mgov defs mblessed imports mimps _mh info) =
  let defs' = updateDefHashes mname newMhash <$> defs
  in Module mname gov' defs' mblessed imports mimps newMhash info
  where
  newMhash = ModuleHash $ hash $ B.toStrict $ B.toLazyByteString (encodeModule m)
  gov' = case mgov of
    KeyGov n -> KeyGov n
    CapGov (ResolvedGov fqn) -> CapGov $ ResolvedGov $ set fqHash newMhash fqn

hashInterfaceAndReplace :: IsBuiltin b => Interface Name Type b i -> Interface Name Type b i
hashInterfaceAndReplace iface@(Interface ifn defs imps _mh info) =
  Interface ifn defs imps newMhash info
  where
  newMhash = ModuleHash $ hash $ B.toStrict $ B.toLazyByteString (encodeInterface iface)

updateDefHashes :: ModuleName -> ModuleHash -> Def Name Type b i -> Def Name Type b i
updateDefHashes mname mhash = \case
  Dfun d -> Dfun $ over dfunTerm (updateTermHashes mname mhash) d
  DConst d -> DConst $ case _dcTerm d of
    TermConst t -> set dcTerm (TermConst (updateTermHashes mname mhash t)) d
    EvaledConst v -> set dcTerm (EvaledConst (updatePactValueHash mname mhash v)) d
  DCap d ->
    DCap $ over dcapTerm (updateTermHashes mname mhash)
         $ over (dcapMeta.dcMetaFqName) (updateFqNameHash mname mhash) d
  DPact d ->
    let updateStep (Step e1) = Step (updateTermHashes mname mhash e1)
        updateStep (StepWithRollback e1 e2) = StepWithRollback (updateTermHashes mname mhash e1) (updateTermHashes mname mhash e2)
    in DPact $ over dpSteps (fmap updateStep) d
  DTable d -> DTable d
  DSchema s -> DSchema s

updateTermHashes :: ModuleName -> ModuleHash -> Term Name Type b i -> Term Name Type b i
updateTermHashes mname mhash = transform $ \case
  Var n i -> Var (updateNameHash mname mhash n) i
  CapabilityForm cf i ->
    CapabilityForm (over capFormName (updateNameHash mname mhash) cf) i
  a -> a

updateNameHash :: ModuleName -> ModuleHash -> Name -> Name
updateNameHash mname mhash (Name n nk) = case nk of
  NTopLevel tlmod _ | tlmod == mname -> Name n (NTopLevel tlmod mhash)
  _ -> Name n nk

updateFqNameHash :: ModuleName -> ModuleHash -> FullyQualifiedName -> FullyQualifiedName
updateFqNameHash mname mhash (FullyQualifiedName tlmod n mh)
  | tlmod == mname = FullyQualifiedName tlmod n mhash
  | otherwise = FullyQualifiedName tlmod n mh

-- updateGuardHash
--   :: ModuleName
--   -> ModuleHash
--   -> Guard QualifiedName PactValue
--   -> Guard QualifiedName PactValue
-- updateGuardHash mname mhash = \case
--   GKeyset ks -> GKeyset ks
--   GKeySetRef ksn -> GKeySetRef ksn
--   GUserGuard (UserGuard fqn pvs) ->
--     GUserGuard $
--       UserGuard
--         (updateFqNameHash mname mhash fqn)
--         (updatePactValueHash mname mhash <$> pvs)
--   GCapabilityGuard (CapabilityGuard fqn pvs pid) ->
--     GCapabilityGuard $
--       CapabilityGuard
--         (updateFqNameHash mname mhash fqn)
--         (updatePactValueHash mname mhash <$> pvs)
--         pid
--   GModuleGuard mg -> GModuleGuard mg
--   g@GDefPactGuard{} -> g

updatePactValueHash :: ModuleName -> ModuleHash -> PactValue -> PactValue
updatePactValueHash mname mhash = \case
  PLiteral l -> PLiteral l
  PList l ->
    PList $ updatePactValueHash mname mhash <$> l
  PGuard g -> PGuard g
  PObject o -> PObject $ updatePactValueHash mname mhash <$> o
  PModRef m -> PModRef m
  PCapToken (CapToken ct pvs) ->
    PCapToken $ CapToken (updateFqNameHash mname mhash ct) (updatePactValueHash mname mhash <$> pvs)
  PTime t -> PTime t

encodeModule :: (IsBuiltin b) => Module Name Type b i -> Builder
encodeModule (Module mname mgov defs mblessed imports mimps _mh _mi) = parens $
  "module"
  <+> encodeModuleName mname
  <+> encodeGov mgov
  <> (if null mblessed then mempty else hsep (encodeBless <$> S.toList mblessed))
  <> (if null imports then mempty else hsep (encodeImport <$> imports) )
  <> (if null mimps then mempty else hsep (encodeModuleName <$> mimps))
  <> hsep (encodeDef <$> defs)
  where
  encodeGov :: Governance Name -> Builder
  encodeGov (KeyGov (KeySetName name mNs)) = encodeMNamespace mNs <> encodeText name
  encodeGov (CapGov (ResolvedGov fqn)) = encodeFqnAsQual fqn
  encodeBless (ModuleHash (Hash s)) = parens ("bless" <+> B.shortByteString s)

encodeMNamespace :: Maybe NamespaceName -> Builder
encodeMNamespace Nothing = mempty
encodeMNamespace (Just (NamespaceName ns)) = encodeText ns <> "."

encodeInterface :: (IsBuiltin b) => Interface Name Type b i -> Builder
encodeInterface (Interface ifn idefs imports _h _i) = parens $
  "interface"
  <+> encodeModuleName ifn
  <> (if null imports then mempty else hsep (encodeImport <$> imports) )
  <> hsep (encodeIfDef <$> idefs)

encodeText :: T.Text -> Builder
encodeText = T.encodeUtf8Builder

encodeName :: Name -> Builder
encodeName (Name n nk) = case nk of
  NTopLevel mn (ModuleHash (Hash sbs)) ->
    encodeText (renderModuleName mn)
      <> "."
      <> encodeText n
      <> braces (B.shortByteString sbs)
  NBound _ -> encodeText n
  NDynRef (DynamicRef arg _) -> encodeText n <> "::" <> encodeText arg
  NModRef mn _ -> encodeText (renderModuleName mn)

list :: [Builder] -> Builder
list = brackets . fold . intersperse " "

hsep :: [Builder] -> Builder
hsep = fold . intersperse " "

commaSep :: [Builder] -> Builder
commaSep = fold . intersperse ", "

parens :: Builder -> Builder
parens b = "(" <> b <> ")"

braces :: Builder -> Builder
braces b = "{" <> b <> "}"

brackets :: Builder -> Builder
brackets b = "[" <> b <> "]"

lpad :: Builder -> Builder
lpad a = " " <> a

-- rpad :: Builder -> Builder
-- rpad a = a <> " "

(<+>) :: Builder -> Builder -> Builder
l <+> r = l <> " " <> r
infixr 6 <+> -- Same as <>

encodePactValue :: PactValue -> Builder
encodePactValue = \case
  PLiteral l -> encodeLiteral l
  PList l -> list $ encodePactValue <$> V.toList l
  PObject o ->
    braces $ hsep $ go <$> M.toList o
    where
    go (Field f, v) = "'" <> encodeText f <> ":" <> encodePactValue v
  PModRef mr ->
    encodeText (renderModuleName (_mrModule mr))
  PCapToken (CapToken n args) -> parens $
    encodeText (renderQualName (fqnToQualName n)) <+> hsep (encodePactValue <$> args)
  PGuard g -> encodeGuard g
  PTime time ->
    B.int64HexFixed (PactTime.toPosixTimestampMicros time)

encodeGuard :: Guard QualifiedName PactValue -> Builder
encodeGuard = \case
  GKeyset (KeySet ks pf) ->
    brackets $ commaSep
      [ "'keys:" <> list (encodeText . _pubKey <$> S.toList ks)
      , "'pred" <> encodePred pf]
    where
    encodePred = \case
      KeysAll -> "keys-all"
      Keys2 -> "keys-2"
      KeysAny -> "keys-any"
  GKeySetRef (KeySetName name mNs) -> "KeySetName" <> parens (encodeMNamespace mNs <> encodeText name)
  GUserGuard (UserGuard fn args) ->
    "UG" <> encodeApp (encodeQualName fn) (encodePactValue <$> args)
  GCapabilityGuard (CapabilityGuard ct args _) ->
    "CapGuard" <> encodeApp (encodeQualName ct) (encodePactValue <$> args)
  GModuleGuard (ModuleGuard mg n) ->
    "ModuleGuard" <> parens (encodeModuleName mg <+> encodeText n)
  GDefPactGuard (DefPactGuard (DefPactId dpid) name) -> "DefPactGuard" <> parens (encodeText dpid <+> encodeText name)

encodeModuleName :: ModuleName -> Builder
encodeModuleName = encodeText . renderModuleName

encodeQualName :: QualifiedName -> Builder
encodeQualName = encodeText . renderQualName

encodeFqnAsQual :: FullyQualifiedName -> Builder
encodeFqnAsQual = encodeQualName . fqnToQualName

encodeApp :: Builder -> [Builder] -> Builder
encodeApp operator operands =
  parens $ hsep (operator:operands)

encodeSchema :: Schema -> Builder
encodeSchema (Schema sc) =
  hsep (encodePair <$> M.toList sc)
  where
  encodePair (Field f, ty) =
    T.encodeUtf8Builder f <> ":" <> encodeType ty

encodePrim :: PrimType -> Builder
encodePrim = \case
  PrimInt -> "integer"
  PrimDecimal -> "decimal"
  PrimBool -> "bool"
  PrimString -> "string"
  PrimGuard -> "guard"
  PrimUnit -> "unit"
  PrimTime -> "time"

encodeType :: Type -> Builder
encodeType = \case
  TyPrim p -> encodePrim p
  TyList l -> brackets (encodeType l)
  TyModRef m -> "module" <> braces (commaSep (T.encodeUtf8Builder . renderModuleName <$> S.toList m))
  -- Todo: this seems potentially inefficient?
  -- Maybe we preserve schema names instead
  TyObject sc -> "object" <> braces (encodeSchema sc)
  TyTable tbl -> "table" <> braces (encodeSchema tbl)
  TyCapToken -> "captoken"
  TyAnyList -> "list"
  TyAnyObject -> "object"

encodeImport :: Import -> Builder
encodeImport (Import mname mmh mimps) = parens $
  "use" <+> encodeModuleName mname
  <> maybe mempty (lpad . encodeModuleHash) mmh
  <> maybe mempty (lpad . list . fmap encodeText) mimps

encodeArg :: Arg Type -> Builder
encodeArg (Arg n mty) =
  T.encodeUtf8Builder n <> maybe mempty ((":" <>) . encodeType) mty

encodeArgList :: [Arg Type] -> Builder
encodeArgList li =
  parens $ hsep $ encodeArg <$> li

encodeModuleHash :: ModuleHash -> Builder
encodeModuleHash (ModuleHash (Hash s)) = B.shortByteString s

-- Note this isn't a prettyprinter.
encodeString :: T.Text -> Builder
encodeString t = "\"" <+> encodeText t <> "\""

encodeLiteral :: Literal -> Builder
encodeLiteral = \case
  LString t -> encodeString t
  LInteger i -> B.integerDec i
  LDecimal (Decimal places mantissa) ->
    B.integerDec mantissa <> "/10^" <> B.word8Dec places
  LUnit -> "()"
  LBool b -> if b then "true" else "false"


encodeTerm ::  (IsBuiltin b) => Term Name Type b i -> Builder
encodeTerm = \case
  Var n _ -> encodeName n
  Lam _li args e _ -> parens $
    "lambda" <> encodeArgList (NE.toList args) <+> encodeTerm e
  -- Todo: collect let args
  Let arg e1 e2 _ -> parens $
    "let" <+> parens (encodeArg arg <+> encodeTerm e1) <> encodeTerm e2
  App e1 args _ ->
    parens (encodeTerm e1 <+> hsep (encodeTerm <$> args))
  Sequence e1 e2 _ ->
    parens ("seq" <+> encodeTerm e1 <+> encodeTerm e2)
  Nullary e _ ->
    parens ("suspend" <+> encodeTerm e)
  Conditional cb _ -> parens $ case cb of
    CAnd e1 e2 ->
      "and" <+> encodeTerm e1 <+> encodeTerm e2
    COr e1 e2 ->
      "or" <+> encodeTerm e1 <+> encodeTerm e2
    CIf e1 e2 e3 ->
      "if" <+> encodeTerm e1 <+> encodeTerm e2 <+> encodeTerm e3
    CEnforce e1 e2 ->
      "enforce" <+> encodeTerm e1 <+> encodeTerm e2
    CEnforceOne e1 args ->
      "enforce-one" <+> encodeTerm e1 <+> list (encodeTerm <$> args)
  Builtin b _ ->
    encodeText (_natName (builtinName b))
  Constant l _ -> encodeLiteral l
  ListLit terms _ ->
    list (encodeTerm <$> terms)
  Try e1 e2 _ ->
    parens ("try" <+> encodeTerm e1 <+> encodeTerm e2)
  ObjectLit pairs _ ->
    braces (fold (intersperse ", " (encodePair <$> pairs)))
    where
    encodePair (Field f, term) =
      "'" <> T.encodeUtf8Builder f <> ":" <> encodeTerm term
  CapabilityForm cf _ -> parens $ case cf of
    WithCapability n args body ->
      "with-capability" <+> encodeName n <+> hsep (encodeTerm <$> args) <+> encodeTerm body
    CreateUserGuard n args ->
      "with-capability" <+> encodeName n <+> hsep (encodeTerm <$> args)
  Error e _ ->
    parens ("error" <+> encodeText e)

encodeTyAnn :: Maybe Type -> Builder
encodeTyAnn = maybe mempty ((":" <>) . encodeType)

encodeDef :: IsBuiltin b => Def Name Type b i -> Builder
encodeDef = \case
  Dfun d -> encodeDefun d
  DConst d -> encodeDefConst d
  DCap d -> encodeDefCap d
  DSchema s -> encodeDefSchema s
  DTable d -> encodeDefTable d
  DPact d -> encodeDefPact d

encodeDefun :: IsBuiltin b => Defun Name Type b i -> Builder
encodeDefun (Defun defnName args rty term _) = parens $
  "defun" <+> encodeText defnName <> encodeTyAnn rty <+> encodeArgList args <+> encodeTerm term

encodeDefConst :: IsBuiltin b => DefConst Name Type b i -> Builder
encodeDefConst (DefConst dcn dcty cv _i) = parens $
  "defconst" <+> encodeText dcn <> encodeTyAnn dcty <+> go cv
  where
  go (TermConst term) = encodeTerm term
  go (EvaledConst pv) = encodePactValue pv

encodeDefPact :: IsBuiltin b => DefPact Name Type b i -> Builder
encodeDefPact (DefPact dpn args rty steps _i) = parens $
  "defpact" <+> encodeText dpn <> encodeTyAnn rty <+> encodeArgList args <+>
    hsep (encodeStep <$> NE.toList steps)
  where
  encodeStep (Step t1) = parens ("step" <+> encodeTerm t1)
  encodeStep (StepWithRollback t1 t2) =
    parens ("step-with-rollback" <+> encodeTerm t1 <+> encodeTerm t2)

-- todo: defcap meta
encodeDefCap :: IsBuiltin b => DefCap Name Type b i -> Builder
encodeDefCap (DefCap dn _ args rty term _meta _info) = parens $
  "defcap" <+> encodeText dn <> encodeTyAnn rty <+> encodeArgList args <+> encodeTerm term

encodeDefSchema :: DefSchema Type info -> Builder
encodeDefSchema (DefSchema dcn sch _i) =
  parens $ "defschema" <+> encodeText dcn <+> encodeSchema (Schema sch)

encodeDefTable :: DefTable Name info -> Builder
encodeDefTable (DefTable dtn (ResolvedTable sc) _i) = parens $
  "deftable" <+> encodeText dtn <> ":" <> braces (encodeSchema sc)

encodeIfDefun :: IfDefun Type info -> Builder
encodeIfDefun (IfDefun dn args rty _i) = parens $
  "defun" <+> encodeText dn <> encodeTyAnn rty <+> encodeArgList args

-- todo: defcap meta
encodeIfDefCap :: IfDefCap Name Type info -> Builder
encodeIfDefCap (IfDefCap dn args rty _meta _i) = parens $
  "defcap" <+> encodeText dn <> encodeTyAnn rty <+> encodeArgList args

encodeIfDefPact :: IfDefPact Type info -> Builder
encodeIfDefPact (IfDefPact dn args rty _i) = parens $
  "defpact" <+> encodeText dn <> encodeTyAnn rty <+> encodeArgList args

encodeIfDef :: IsBuiltin b => IfDef Name Type b i -> Builder
encodeIfDef = \case
  IfDfun df -> encodeIfDefun df
  IfDCap dc -> encodeIfDefCap dc
  IfDPact dp -> encodeIfDefPact dp
  IfDConst dc -> encodeDefConst dc
  IfDSchema dc -> encodeDefSchema dc
