{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Pact.Core.Syntax.ParseTree where

import Control.DeepSeq
import Control.Lens hiding (List, op)
import Data.Text(Text)
import Data.List.NonEmpty(NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import GHC.Generics

import Pact.Core.Literal
import Pact.Core.Names
import Pact.Core.Pretty
import Pact.Core.Type(PrimType(..))
import Pact.Core.Guards


data Operator
  = AndOp
  | OrOp
  | EnforceOp
  | EnforceOneOp
  deriving (Show, Eq, Enum, Bounded, Generic, NFData)

renderOp :: Operator -> Text
renderOp = \case
    AndOp -> "and"
    OrOp -> "or"
    EnforceOp -> "enforce"
    EnforceOneOp -> "enforce-one"

instance Pretty Operator where
  pretty = \case
    AndOp -> "and"
    OrOp -> "or"
    EnforceOp -> "enforce"
    EnforceOneOp -> "enforce-one"

-- | Type representing all pact syntactic types.
-- Note: A few types (mainly TyPoly*) do not exist in pact-core.
data Type
  = TyPrim PrimType
  | TyList Type
  | TyPolyList
  | TyModRef [ModuleName]
  | TyKeyset
  | TyObject ParsedTyName
  | TyTable ParsedTyName
  | TyPolyObject
  | TyAny
  deriving (Show, Eq, Generic, NFData)

pattern TyInt :: Type
pattern TyInt = TyPrim PrimInt

pattern TyDecimal :: Type
pattern TyDecimal = TyPrim PrimDecimal

pattern TyBool :: Type
pattern TyBool = TyPrim PrimBool

pattern TyString :: Type
pattern TyString = TyPrim PrimString

pattern TyTime :: Type
pattern TyTime = TyPrim PrimTime

pattern TyUnit :: Type
pattern TyUnit = TyPrim PrimUnit

pattern TyGuard :: Type
pattern TyGuard = TyPrim PrimGuard

instance Pretty Type where
  pretty = \case
    TyPrim prim -> pretty prim
    TyList t -> brackets (pretty t)
    TyModRef mn -> "module" <> braces (hsep (punctuate comma (pretty <$> mn)))
    TyPolyList -> "list"
    TyKeyset -> "keyset"
    TyObject qn -> "object" <> braces (pretty qn)
    TyPolyObject -> "object"
    TyTable o -> "table" <> braces (pretty o)
    TyAny -> "*"


----------------------------------------------------
-- Common structures
----------------------------------------------------

data Arg i
  = Arg
  { _argName :: Text
  , _argType :: Type
  , _argInfo :: i
  } deriving (Show, Functor, Generic, NFData)

data MArg i
  = MArg
  { _margName :: Text
  , _margType :: Maybe Type
  , _margInfo :: i
  } deriving (Eq, Show, Functor, Generic, NFData)

defName :: Def i -> Text
defName = \case
  Dfun d -> _margName $ _dfunSpec d
  DConst d ->_margName $ _dcSpec d
  DCap d -> _margName $ _dcapSpec d
  DTable d -> _dtName d
  DPact d -> _margName $ _dpSpec d
  DSchema d -> _dscName d

findDocFromAnns :: [PactAnn i] -> Maybe Text
findDocFromAnns [] = Nothing
findDocFromAnns (PactDoc _ doc:_) = Just doc
findDocFromAnns (_:xs) = findDocFromAnns xs

defDocs :: Def i -> Maybe Text
defDocs = \case
  Dfun d -> findDocFromAnns (_dfunAnns d)
  DConst d -> view _1 <$> _dcDocs d
  DCap d -> findDocFromAnns (_dcapAnns d)
  DTable d -> view _1 <$> _dtDocs d
  DPact d -> findDocFromAnns (_dpAnns d)
  DSchema d -> findDocFromAnns (_dscAnns d)

defInfo :: Def i -> i
defInfo = \case
  Dfun d -> _dfunInfo d
  DConst d -> _dcInfo d
  DCap d -> _dcapInfo d
  DTable d -> _dtInfo d
  DPact d -> _dpInfo d
  DSchema d -> _dscInfo d

data PactDocType
  = PactDocAnn
  | PactDocString
  deriving (Eq, Show, Generic)

instance NFData PactDocType

-- | A data type for our pact def annotations, either
--   an @doc, @model or a docstring.
data PactAnn i
  = PactDoc PactDocType Text
  | PactModel [PropertyExpr i]
  deriving (Eq, Show, Generic, Functor)

instance NFData i => NFData (PactAnn i)

instance Pretty (PactAnn i) where
  pretty (PactDoc dt doc) = case dt of
    PactDocAnn ->
      "@doc" <+> dquotes (pretty doc)
    PactDocString ->
      dquotes (pretty doc)
  pretty (PactModel m) =
    "@model" <+> brackets (space <> (align $ vsep (pretty <$> m)))

data Defun i
  = Defun
  { _dfunSpec :: MArg i  -- ^ 'MArg' contains the name ('_margName') and
                         -- optional return type ('_margType'). The 'i' reflects the name info.
  , _dfunArgs :: [MArg i]
  , _dfunTerm :: NonEmpty (Expr i)
  , _dfunAnns :: [PactAnn i]
  , _dfunInfo :: i
  } deriving (Show, Functor, Generic, NFData)

instance Pretty (Defun i) where
  pretty (Defun da args term anns _) =
    parens $
      "defun" <+>
      pretty da <+>
      parensSep (pretty <$> args) <>
      (nest 2 $
        line <>
        prettyAnnListWithNewline anns <>
        vsep (pretty <$> NE.toList term)
        )

-- | Our defconst parsed representation
data DefConst i
  = DefConst
  { _dcSpec :: MArg i  -- ^ 'MArg' contains the name ('_margName') and
                       -- optional return type ('_margType'). The 'i' reflects the name info.
  , _dcTerm :: Expr i
  , _dcDocs :: Maybe (Text, PactDocType)
  , _dcInfo :: i
  } deriving (Show, Functor, Generic, NFData)

instance Pretty (DefConst i) where
  pretty (DefConst marg term doc _) =
    parens $
      "defconst" <+>
      pretty marg <+>
      (nest 2 $
        line <>
        maybe mempty (\(d, ann) -> pretty (PactDoc ann d) <> line) doc
        <> pretty term
        )

data DCapMeta
  = DefEvent
  | DefManaged (Maybe (Text, ParsedName))
  deriving (Show, Generic, NFData)

instance Pretty DCapMeta where
  pretty = \case
    DefEvent -> "@event"
    DefManaged m -> "@managed" <> case m of
      Nothing -> mempty
      Just (n, pn) -> space <> hsep [pretty n, pretty pn]

data DefCap i
  = DefCap
  { _dcapSpec :: MArg i  -- ^ 'MArg' contains the name ('_margName') and
                         -- optional return type ('_margType'). The 'i' reflects the name info.
  , _dcapArgs :: ![MArg i]
  , _dcapTerm :: NonEmpty (Expr i)
  , _dcapAnns :: [PactAnn i]
  , _dcapMeta :: Maybe DCapMeta
  , _dcapInfo :: i
  } deriving (Show, Functor, Generic, NFData)

instance Pretty (DefCap i) where
  pretty (DefCap n args term anns meta _) =
    parens $
      "defcap" <+>
      pretty n <+>
      parensSep (pretty <$> args) <>
      (nest 2 $
        line <>
        prettyAnnListWithNewline anns <>
        (maybe mempty ((<> line) . pretty) meta) <>
        vsep (pretty <$> NE.toList term)
        )

data DefSchema i
  = DefSchema
  { _dscName :: Text
  , _dscArgs :: [Arg i]
  , _dscAnns :: [PactAnn i]
  , _dscInfo :: i
  } deriving (Show, Functor, Generic, NFData)

instance Pretty (DefSchema i) where
  pretty (DefSchema n args anns _) =
    parens $
      "defschema" <+> pretty n <>
      (nest 2 $
        line <>
        prettyAnnListWithNewline anns <>
        vsep (pretty <$> args)
      )

prettyAnnListWithNewline :: Pretty a => [a] -> Doc ann
prettyAnnListWithNewline anns =
  if null anns then mempty else vsep (pretty <$> anns) <> line

data DefTable i
  = DefTable
  { _dtName :: Text
  , _dtSchema :: ParsedName
  , _dtDocs :: Maybe (Text, PactDocType)
  , _dtInfo :: i
  } deriving (Show, Functor, Generic, NFData)

instance Pretty (DefTable i) where
  pretty (DefTable n schema docs _) =
    parens $
      "deftable"
      <+> pretty n
      <> ":"
      <> brackets (pretty schema)
      <> maybe mempty (\d -> line <> pretty (uncurry (flip PactDoc) d)) docs

data PactStep i
  = Step (Expr i) (Maybe [PropertyExpr i])
  | StepWithRollback (Expr i) (Expr i) (Maybe [PropertyExpr i])
  deriving (Show, Functor, Generic, NFData)

instance Pretty (PactStep i) where
  pretty = \case
    Step e1 anns ->
      parens $
        "step" <+> pretty e1 <> nest 2
          (maybe mempty (\a -> line <> pretty (PactModel a)) anns)
    StepWithRollback e1 e2 anns ->
      parens $
        "step-with-rollback" <+> pretty e1 <+> pretty e2 <> nest 2
          (maybe mempty (\a -> line <> pretty (PactModel a)) anns)

data DefPact i
  = DefPact
  { _dpSpec :: MArg i -- ^ 'MArg' contains the name ('_margName') and
                      -- optional return type ('_margType'). The 'i' reflects the name info.
  , _dpArgs :: [MArg i]
  , _dpSteps :: NonEmpty (PactStep i)
  , _dpAnns :: [PactAnn i]
  , _dpInfo :: i
  } deriving (Show, Functor, Generic, NFData)

instance Pretty (DefPact i) where
  pretty (DefPact n args steps anns _) =
    parens $
      "defpact" <+>
      pretty n <+>
      parensSep (pretty <$> args) <>
      (nest 2 $
        line <>
        prettyAnnListWithNewline anns <>
        vsep (pretty <$> NE.toList steps)
        )


data Managed
  = AutoManaged
  | Managed Text ParsedName
  deriving (Show)

data Def i
  = Dfun (Defun i)
  | DConst (DefConst i)
  | DCap (DefCap i)
  | DSchema (DefSchema i)
  | DTable (DefTable i)
  | DPact (DefPact i)
  deriving (Show, Functor, Generic, NFData)

instance Pretty (Def i) where
  pretty = \case
    Dfun df -> pretty df
    DConst dc -> pretty dc
    DCap dc -> pretty dc
    DSchema dc -> pretty dc
    DTable dc -> pretty dc
    DPact dc -> pretty dc

data Import
  = Import
  { _impModuleName  :: ModuleName
  , _impModuleHash :: Maybe Text
  , _impImported :: Maybe [Text] }
  deriving (Show, Generic, NFData)

instance Pretty Import where
  pretty (Import mn mh imp) =
    parens $
      "use" <+> pretty mn <> prettyMh mh <> prettyImp imp
    where
    prettyMh = maybe mempty (\h -> space <> dquotes (pretty h))
    prettyImp = maybe mempty (\imps -> space <> commaBrackets (dquotes . pretty <$> imps))

data ExtDecl
  = ExtBless Text
  | ExtImport Import
  | ExtImplements ModuleName
  deriving (Show, Generic, NFData)

instance Pretty ExtDecl where
  pretty = \case
    ExtBless t ->
      parens $ "bless" <+> dquotes (pretty t)
    ExtImport imp -> pretty imp
    ExtImplements m ->
      parens $ "implements" <+> pretty m

data Module i
  = Module
  { _mName :: ModuleName
  , _mGovernance :: Governance ParsedName
  , _mExternal :: [ExtDecl]
  , _mDefs :: NonEmpty (Def i)
  , _mAnns :: [PactAnn i]
  , _mInfo :: i
  } deriving (Show, Functor, Generic, NFData)

instance Pretty (Module i) where
  pretty (Module mname mgov mexts defs anns _) =
    parens $
      "module" <+> pretty (_mnName mname) <+> pretty mgov <>
        nest 2
        ( line <>
          prettyAnnListWithNewline anns <>
          prettyAnnListWithNewline mexts <>
          vsep (pretty <$> NE.toList defs)
        )

data TopLevel i
  = TLModule (Module i)
  | TLInterface (Interface i)
  | TLTerm (Expr i)
  | TLUse Import i
  deriving (Show, Functor, Generic, NFData)

instance Pretty (TopLevel i) where
  pretty = \case
    TLModule m -> pretty m
    TLInterface m -> pretty m
    TLTerm term -> pretty term
    TLUse i _ -> pretty i

data Interface i
  = Interface
  { _ifName :: ModuleName
  , _ifDefns :: [IfDef i]
  , _ifImports :: [Import]
  , _ifAnns :: [PactAnn i]
  , _ifInfo :: i
  } deriving (Show, Functor, Generic, NFData)

instance Pretty (Interface i) where
  pretty (Interface ifn defs imports anns _) =
    parens $
    "interface"
    <+> pretty ifn
    <> nest 2
    ( line <>
      prettyAnnListWithNewline anns <>
      prettyAnnListWithNewline imports <>
      vsep (pretty <$> defs)
    )

data IfDefun i
  = IfDefun
  { _ifdSpec :: MArg i  -- ^ 'MArg' contains the name ('_margName') and
                        -- optional return type ('_margType'). The 'i' reflects the name info.
  , _ifdArgs :: [MArg i]
  , _ifdAnns :: [PactAnn i]
  , _ifdInfo :: i
  } deriving (Show, Functor, Generic, NFData)

instance Pretty (IfDefun i) where
  pretty (IfDefun n args anns _) =
    parens $
      "defun" <+> pretty n
      <+> parensSep (pretty <$> args)
      <> nest 2 (if null anns then mempty else vsep (pretty <$> anns))

data IfDefCap i
  = IfDefCap
  { _ifdcSpec :: MArg i  -- ^ 'MArg' contains the name ('_margName') and
                         -- optional return type ('_margType'). The 'i' reflects the name info.
  , _ifdcArgs :: [MArg i]
  , _ifdcAnns :: [PactAnn i]
  , _ifdcMeta :: Maybe DCapMeta
  , _ifdcInfo :: i
  } deriving (Show, Functor, Generic, NFData)

instance Pretty (IfDefCap i) where
  pretty (IfDefCap n args anns meta _) =
    parens $
      "defcap" <+> pretty n
      <+> parensSep (pretty <$> args)
      <> nest 2
      ( if null p then mempty else
        line <> vsep p
      )
    where
    p = fmap pretty anns ++ maybe [] (pure . pretty) meta

data IfDefPact i
  = IfDefPact
  { _ifdpSpec :: MArg i  -- ^ 'MArg' contains the name ('_margName') and
                         -- optional return type ('_margType'). The 'i' reflects the name info.
  , _ifdpArgs :: [MArg i]
  , _ifdpAnns :: [PactAnn i]
  , _ifdpInfo :: i
  } deriving (Show, Functor, Generic, NFData)

instance Pretty (IfDefPact i) where
  pretty (IfDefPact n args anns _) =
    parens $
      "defpact" <+> pretty n
      <+> parensSep (pretty <$> args)
      <> nest 2 (if null anns then mempty else vsep (pretty <$> anns))

data PropKeyword
  = KwLet
  | KwLambda
  | KwDefProperty
  deriving (Eq, Show, Generic, NFData)

instance Pretty PropKeyword where
  pretty = \case
    KwLet -> "let"
    KwLambda -> "lambda"
    KwDefProperty -> "defproperty"

data PropDelim
  = DelimLBracket
  | DelimRBracket
  | DelimLBrace
  | DelimRBrace
  | DelimComma
  | DelimColon
  | DelimWalrus -- := operator
  deriving (Show, Eq, Generic, NFData)

instance Pretty PropDelim where
  pretty = \case
    DelimLBracket -> "["
    DelimRBracket -> "]"
    DelimLBrace -> "{"
    DelimRBrace -> "}"
    DelimComma -> ","
    DelimColon -> ":"
    DelimWalrus -> ":="


data PropertyExpr i
  = PropAtom ParsedName i
  | PropKeyword PropKeyword i
  | PropDelim PropDelim i
  | PropSequence [PropertyExpr i] i
  | PropConstant Literal i
  deriving (Show, Functor, Eq, Generic, NFData)

instance Pretty (PropertyExpr i) where
  pretty = \case
    PropAtom pn _ -> pretty pn
    PropKeyword kw _ -> pretty kw
    PropDelim delim _ -> pretty delim
    PropSequence exprs _ -> case exprs of
      [] -> "()"
      x:xs -> pretty (PrettyLispApp x xs)
    PropConstant lit _ ->
      pretty lit


-- Interface definitions may be one of:
--   Defun sig
--   Defconst
--   Defschema
--   Defpact sig
--   Defcap Sig
data IfDef i
  = IfDfun (IfDefun i)
  | IfDConst (DefConst i)
  | IfDCap (IfDefCap i)
  | IfDSchema (DefSchema i)
  | IfDPact (IfDefPact i)
  deriving (Show, Functor, Generic, NFData)

instance Pretty (IfDef i) where
  pretty = \case
    IfDfun d -> pretty d
    IfDConst d -> pretty d
    IfDCap d -> pretty d
    IfDSchema d -> pretty d
    IfDPact d -> pretty d


instance Pretty (Arg i) where
  pretty (Arg n ty _) =
    pretty n <> ":" <+> pretty ty

instance Pretty (MArg i) where
  pretty (MArg n mty _) =
    pretty n <> maybe mempty ((":" <>) . pretty) mty


data Binder i =
  Binder Text (Maybe Type) (Expr i)
  deriving (Show, Eq, Functor, Generic, NFData)

instance Pretty (Binder i) where
  pretty (Binder ident ty e) =
    parens $ pretty ident <> maybe mempty ((":" <>) . pretty) ty <+> pretty e

data CapForm i
  = WithCapability (Expr i) (Expr i)
  | CreateUserGuard ParsedName [Expr i]
  deriving (Show, Eq, Functor, Generic, NFData)

-- | LetForm is only to differentiate between
-- let and let*
-- Their semantics are exactly the same in pact-5 but this lets us pretty print
-- more accurately
data LetForm
  = LFLetNormal
  | LFLetStar
  deriving (Eq, Show, Generic)

instance Pretty LetForm where
  pretty LFLetNormal = mempty
  pretty LFLetStar = "*"

instance NFData LetForm

data Expr i
  = Var ParsedName i
  | Let LetForm (NonEmpty (Binder i)) (NonEmpty (Expr i)) i
  | Lam [MArg i] (NonEmpty (Expr i)) i
  | App (Expr i) [Expr i] i
  | List [Expr i] i
  | Constant Literal i
  | Object [(Field, Expr i)] i
  | Binding [(Field, MArg i)] [Expr i] i
  deriving (Show, Eq, Functor, Generic, NFData)

data ReplTopLevel i
  = RTLTopLevel (TopLevel i)
  | RTLDefun (Defun i)
  | RTLDefConst (DefConst i)
  deriving (Show, Generic, NFData)

pattern RTLModule :: Module i -> ReplTopLevel i
pattern RTLModule m = RTLTopLevel (TLModule m)

pattern RTLInterface :: Interface i -> ReplTopLevel i
pattern RTLInterface m = RTLTopLevel (TLInterface m)

pattern RTLTerm :: Expr i -> ReplTopLevel i
pattern RTLTerm te = RTLTopLevel (TLTerm te)

pattern RTLUse :: Import -> i -> ReplTopLevel i
pattern RTLUse imp i = RTLTopLevel (TLUse imp i)


termInfo :: Lens' (Expr i) i
termInfo f = \case
  Var n i -> Var n <$> f i
  Let lf bnds e1 i ->
    Let lf bnds e1 <$> f i
  Lam nel e i ->
    Lam nel e <$> f i
  App e1 args i ->
    App e1 args <$> f i
  Object m i -> Object m <$> f i
  List nel i ->
    List nel <$> f i
  Constant l i ->
    Constant l <$> f i
  Binding t e i ->
    Binding t e <$> f i

topLevelInfo :: Lens' (TopLevel i) i
topLevelInfo f = \case
  TLModule m -> (\i -> TLModule m{_mInfo=i}) <$> f (_mInfo m)
  TLInterface m -> (\i -> TLInterface m{_ifInfo=i}) <$> f (_ifInfo m)
  TLTerm t -> TLTerm <$> termInfo f t
  TLUse imp i ->
    TLUse imp <$> f i

instance Pretty (Expr i) where
  pretty = \case
    Var n _ -> pretty n
    Let lf bnds e _ ->
      parens ("let" <> pretty lf <+> parens (prettyNEL bnds) <+> prettyNEL e)
    Lam nel e _ ->
      parens ("lambda" <+> parensSep (pretty <$> nel) <+> prettyNEL e)
    App e1 nel _ ->
      pretty (PrettyLispApp e1 nel)
    Constant l _ ->
      pretty l
    List nel _ ->
      brackets (commaSep nel)
    Object m _ ->
      braces (hsep (punctuate "," (prettyObj m)))
    Binding binds body _ ->
      braces (hsep $ punctuate "," $ fmap prettyBind binds) <+>
        hsep (pretty <$> body)
    where
    prettyNEL nel = hsep (NE.toList (pretty <$> nel))
    prettyBind (f, e) = pretty f <+> ":=" <+> pretty e
    prettyObj = fmap (\(n, k) -> dquotes (pretty n) <> ":" <> pretty k)

makeLenses ''Arg
makeLenses ''MArg
makeLenses ''Defun
makeLenses ''DefConst
makeLenses ''DefCap
makeLenses ''DefSchema
makeLenses ''DefTable
makeLenses ''DefPact
makeLenses ''Def
