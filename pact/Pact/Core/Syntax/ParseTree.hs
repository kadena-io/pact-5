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
import Data.Foldable(fold)
import Data.Text(Text)
import Data.List.NonEmpty(NonEmpty(..))
import Data.List(intersperse)
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

defDocs :: Def i -> Maybe Text
defDocs = \case
  Dfun d -> _dfunDocs d
  DConst d -> _dcDocs d
  DCap d -> _dcapDocs d
  DTable d -> _dtDocs d
  DPact d -> _dpDocs d
  DSchema d -> _dscDocs d

defInfo :: Def i -> i
defInfo = \case
  Dfun d -> _dfunInfo d
  DConst d -> _dcInfo d
  DCap d -> _dcapInfo d
  DTable d -> _dtInfo d
  DPact d -> _dpInfo d
  DSchema d -> _dscInfo d


data Defun i
  = Defun
  { _dfunSpec :: MArg i  -- ^ 'MArg' contains the name ('_margName') and
                         -- optional return type ('_margType'). The 'i' reflects the name info.
  , _dfunArgs :: [MArg i]
  , _dfunTerm :: NonEmpty (Expr i)
  , _dfunDocs :: Maybe Text
  , _dfunModel :: [PropertyExpr i]
  , _dfunInfo :: i
  } deriving (Show, Functor, Generic, NFData)

data DefConst i
  = DefConst
  { _dcSpec :: MArg i  -- ^ 'MArg' contains the name ('_margName') and
                       -- optional return type ('_margType'). The 'i' reflects the name info.
  , _dcTerm :: Expr i
  , _dcDocs :: Maybe Text
  , _dcInfo :: i
  } deriving (Show, Functor, Generic, NFData)

data DCapMeta
  = DefEvent
  | DefManaged (Maybe (Text, ParsedName))
  deriving (Show, Generic, NFData)

data DefCap i
  = DefCap
  { _dcapSpec :: MArg i  -- ^ 'MArg' contains the name ('_margName') and
                         -- optional return type ('_margType'). The 'i' reflects the name info.
  , _dcapArgs :: ![MArg i]
  , _dcapTerm :: NonEmpty (Expr i)
  , _dcapDocs :: Maybe Text
  , _dcapModel :: [PropertyExpr i]
  , _dcapMeta :: Maybe DCapMeta
  , _dcapInfo :: i
  } deriving (Show, Functor, Generic, NFData)

data DefSchema i
  = DefSchema
  { _dscName :: Text
  , _dscArgs :: [Arg i]
  , _dscDocs :: Maybe Text
  , _dscModel :: [PropertyExpr i]
  , _dscInfo :: i
  } deriving (Show, Functor, Generic, NFData)

data DefTable i
  = DefTable
  { _dtName :: Text
  , _dtSchema :: ParsedName
  , _dtDocs :: Maybe Text
  , _dtInfo :: i
  } deriving (Show, Functor, Generic, NFData)

data PactStep i
  = Step (Expr i) [PropertyExpr i]
  | StepWithRollback (Expr i) (Expr i) [PropertyExpr i]
  deriving (Show, Functor, Generic, NFData)

data DefPact i
  = DefPact
  { _dpSpec :: MArg i -- ^ 'MArg' contains the name ('_margName') and
                      -- optional return type ('_margType'). The 'i' reflects the name info.
  , _dpArgs :: [MArg i]
  , _dpSteps :: NonEmpty (PactStep i)
  , _dpDocs :: Maybe Text
  , _dpModel :: [PropertyExpr i]
  , _dpInfo :: i
  } deriving (Show, Functor, Generic, NFData)

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

data Import
  = Import
  { _impModuleName  :: ModuleName
  , _impModuleHash :: Maybe Text
  , _impImported :: Maybe [Text] }
  deriving (Show, Generic, NFData)

data ExtDecl
  = ExtBless Text
  | ExtImport Import
  | ExtImplements ModuleName
  deriving (Show, Generic, NFData)

data Module i
  = Module
  { _mName :: ModuleName
  , _mGovernance :: Governance ParsedName
  , _mExternal :: [ExtDecl]
  , _mDefs :: NonEmpty (Def i)
  , _mDoc :: Maybe Text
  , _mModel :: [PropertyExpr i]
  , _mInfo :: i
  } deriving (Show, Functor, Generic, NFData)

data TopLevel i
  = TLModule (Module i)
  | TLInterface (Interface i)
  | TLTerm (Expr i)
  | TLUse Import i
  deriving (Show, Functor, Generic, NFData)

data Interface i
  = Interface
  { _ifName :: ModuleName
  , _ifDefns :: [IfDef i]
  , _ifImports :: [Import]
  , _ifDocs :: Maybe Text
  , _ifModel :: [PropertyExpr i]
  , _ifInfo :: i
  } deriving (Show, Functor, Generic, NFData)

data IfDefun i
  = IfDefun
  { _ifdSpec :: MArg i  -- ^ 'MArg' contains the name ('_margName') and
                        -- optional return type ('_margType'). The 'i' reflects the name info.
  , _ifdArgs :: [MArg i]
  , _ifdDocs :: Maybe Text
  , _ifdModel :: [PropertyExpr i]
  , _ifdInfo :: i
  } deriving (Show, Functor, Generic, NFData)

data IfDefCap i
  = IfDefCap
  { _ifdcSpec :: MArg i  -- ^ 'MArg' contains the name ('_margName') and
                         -- optional return type ('_margType'). The 'i' reflects the name info.
  , _ifdcArgs :: [MArg i]
  , _ifdcDocs :: Maybe Text
  , _ifdcModel :: [PropertyExpr i]
  , _ifdcMeta :: Maybe DCapMeta
  , _ifdcInfo :: i
  } deriving (Show, Functor, Generic, NFData)

data IfDefPact i
  = IfDefPact
  { _ifdpSpec :: MArg i  -- ^ 'MArg' contains the name ('_margName') and
                         -- optional return type ('_margType'). The 'i' reflects the name info.
  , _ifdpArgs :: [MArg i]
  , _ifdpDocs :: Maybe Text
  , _ifdpModel :: [PropertyExpr i]
  , _ifdpInfo :: i
  } deriving (Show, Functor, Generic, NFData)

data PropKeyword
  = KwLet
  | KwLambda
  | KwDefProperty
  deriving (Eq, Show, Generic, NFData)

data PropDelim
  = DelimLBracket
  | DelimRBracket
  | DelimLBrace
  | DelimRBrace
  | DelimComma
  | DelimColon
  | DelimWalrus -- := operator
  deriving (Show, Generic, NFData)

data PropertyExpr i
  = PropAtom ParsedName i
  | PropKeyword PropKeyword i
  | PropDelim PropDelim i
  | PropSequence [PropertyExpr i] i
  | PropConstant Literal i
  deriving (Show, Functor, Generic, NFData)


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

instance Pretty (DefConst i) where
  pretty (DefConst (MArg dcn dcty _) term _ _) =
    parens ("defconst" <+> pretty dcn <> mprettyTy dcty <+> pretty term)
    where
    mprettyTy = maybe mempty ((":" <>) . pretty)

instance Pretty (Arg i) where
  pretty (Arg n ty _) =
    pretty n <> ":" <+> pretty ty

instance Pretty (MArg i) where
  pretty (MArg n mty _) =
    pretty n <> maybe mempty (\ty -> ":" <+> pretty ty) mty

instance Pretty (Defun i) where
  pretty (Defun (MArg n rettype _) args term _ _ _) =
    parens ("defun" <+> pretty n <+> parens (commaSep args)
      <> ":" <+> pretty rettype <+> "=" <+> pretty term)

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

instance Pretty (Expr i) where
  pretty = \case
    Var n _ -> pretty n
    Let lf bnds e _ ->
      parens ("let" <> lf' <+> parens (prettyNEL bnds) <+> prettyNEL e)
      where
      lf' = case lf of
        LFLetNormal -> mempty
        LFLetStar -> "*"
    Lam nel e _ ->
      parens ("lambda" <+> parens (renderLamTypes nel) <+> prettyNEL e)
    App e1 [] _ ->
      parens (pretty e1)
    App e1 nel _ ->
      parens (pretty e1 <+> hsep (pretty <$> nel))
    Constant l _ ->
      pretty l
    List nel _ ->
      "[" <> commaSep nel <> "]"
    Object m _ ->
      braces (hsep (punctuate "," (prettyObj m)))
    Binding binds body _ ->
      braces (hsep $ punctuate "," $ fmap prettyBind binds) <+>
        hsep (pretty <$> body)
    where
    prettyNEL nel = hsep (NE.toList (pretty <$> nel))
    prettyBind (f, e) = pretty f <+> ":=" <+> pretty e
    prettyObj = fmap (\(n, k) -> dquotes (pretty n) <> ":" <> pretty k)
    renderLamPair (MArg n mt _) = case mt of
      Nothing -> pretty n
      Just t -> pretty n <> ":" <> pretty t
    renderLamTypes = fold . intersperse " " . fmap renderLamPair

makeLenses ''Arg
makeLenses ''MArg
makeLenses ''Defun
makeLenses ''DefConst
makeLenses ''DefCap
makeLenses ''DefSchema
makeLenses ''DefTable
makeLenses ''DefPact
makeLenses ''Def
