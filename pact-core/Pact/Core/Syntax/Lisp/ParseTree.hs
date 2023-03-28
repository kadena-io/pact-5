{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternSynonyms #-}

module Pact.Core.Syntax.Lisp.ParseTree where

import Control.Lens hiding (List, op)
import Data.Foldable(fold)
import Data.Text(Text)
import Data.List.NonEmpty(NonEmpty(..))
import Data.List(intersperse)

import qualified Data.List.NonEmpty as NE

import Pact.Core.Literal
import Pact.Core.Names
import Pact.Core.Pretty
import Pact.Core.Type(PrimType(..))
import Pact.Core.Imports
import Pact.Core.Guards


data Operator
  = AddOp
  | SubOp
  | MultOp
  | DivOp
  | GTOp
  | GEQOp
  | LTOp
  | LEQOp
  | EQOp
  | NEQOp
  | BitAndOp
  | BitOrOp
  | BitComplementOp
  | AndOp
  | OrOp
  | PowOp
  deriving (Show, Eq, Enum, Bounded)

instance Pretty Operator where
  pretty = \case
    AddOp -> "+"
    SubOp -> "-"
    MultOp -> "*"
    DivOp -> "/"
    GTOp -> ">"
    GEQOp -> ">="
    LTOp -> "<"
    LEQOp -> "<="
    EQOp -> "="
    NEQOp -> "!="
    BitAndOp -> "&"
    BitOrOp -> "|"
    AndOp -> "and"
    OrOp -> "or"
    PowOp -> "^"
    BitComplementOp -> "~"

-- Todo: type constructors aren't 1-1 atm.
data Type
  = TyPrim PrimType
  | TyList Type
  | TyPolyList
  | TyModRef ModuleName
  | TyGuard
  | TyKeyset
  | TyObject ParsedName
  | TyTime
  | TyPolyObject
  deriving (Show, Eq)

pattern TyInt :: Type
pattern TyInt = TyPrim PrimInt

pattern TyDecimal :: Type
pattern TyDecimal = TyPrim PrimDecimal

-- pattern TyTime :: Type
-- pattern TyTime = TyPrim PrimTime

pattern TyBool :: Type
pattern TyBool = TyPrim PrimBool

pattern TyString :: Type
pattern TyString = TyPrim PrimString

pattern TyUnit :: Type
pattern TyUnit = TyPrim PrimUnit

-- | Do we render parenthesis for the type if it shows nested in another
instance Pretty Type where
  pretty = \case
    TyPrim prim -> pretty prim
    TyList t -> brackets (pretty t)
    TyModRef mn -> "module" <> braces (pretty mn)
    TyPolyList -> "list"
    TyGuard -> "guard"
    TyKeyset -> "keyset"
    TyObject qn -> "object" <> brackets (pretty qn)
    TyPolyObject -> "object"
    TyTime -> "time"


----------------------------------------------------
-- Common structures
----------------------------------------------------

data Arg
  = Arg
  { _argName :: Text
  , _argType :: Type }
  deriving Show

data MArg
  = MArg
  { _margName :: Text
  , _margType :: Maybe Type }
  deriving (Eq, Show)

data Defun i
  = Defun
  { _dfunName :: !Text
  , _dfunArgs :: ![MArg]
  , _dfunRetType :: Maybe Type
  , _dfunTerm :: !(Expr i)
  , _dfunDocs :: Maybe Text
  , _dfunModel :: Maybe [Expr i]
  , _dfunInfo :: i
  } deriving Show

data DefConst i
  = DefConst
  { _dcName :: Text
  , _dcType :: Maybe Type
  , _dcTerm :: Expr i
  , _dcDocs :: Maybe Text
  , _dcInfo :: i
  } deriving Show

data DCapMeta
  = DefEvent
  | DefManaged (Maybe (Text, ParsedName))
  deriving Show

data DefCap i
  = DefCap
  { _dcapName :: Text
  , _dcapArgs :: ![MArg]
  , _dcapRetType :: Maybe Type
  , _dcapTerm :: Expr i
  , _dcapDocs :: Maybe Text
  , _dcapModel :: Maybe [Expr i]
  , _dcapMeta :: Maybe DCapMeta
  , _dcapInfo :: i
  } deriving Show

data DefSchema i
  = DefSchema
  { _dscName :: Text
  , _dscArgs :: [Arg]
  , _dscDocs :: Maybe Text
  , _dscModel :: Maybe [Expr i]
  , _dscInfo :: i
  } deriving Show

data DefTable i
  = DefTable
  { _dtName :: Text
  , _dtSchema :: ParsedName
  , _dtDocs :: Maybe Text
  , _dtInfo :: i
  } deriving Show

data PactStep i
  = Step (Expr i) (Maybe [Expr i])
  | StepWithRollback (Expr i) (Expr i) (Maybe [Expr i])
  deriving Show

data DefPact i
  = DefPact
  { _dpName :: Text
  , _dpArgs :: [MArg]
  , _dpRetType :: Maybe Type
  , _dpSteps :: [PactStep i]
  , _dpDocs :: Maybe Text
  , _dpModel :: Maybe [Expr i]
  , _dpInfo :: i
  } deriving Show

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
  deriving Show

data ExtDecl
  = ExtBless Text
  | ExtImport Import
  | ExtImplements ModuleName
  deriving Show

data DefProperty i
  = DefProperty
  { _dpropName :: Text
  , _dpropArgs :: [Arg]
  , _dpropExp :: Expr i
  } deriving Show

-- Todo: fix nonempty cond on module
data Module i
  = Module
  { _mName :: ModuleName
  , _mGovernance :: Governance Text
  , _mExternal :: [ExtDecl]
  , _mDefs :: NonEmpty (Def i)
  , _mDoc :: Maybe Text
  , _mModel :: [DefProperty i]
  } deriving Show

data TopLevel i
  = TLModule (Module i)
  | TLInterface (Interface i)
  | TLTerm (Expr i)
  deriving Show

data Interface i
  = Interface
  { _ifName :: ModuleName
  , _ifDefns :: [IfDef i]
  , _ifDocs :: Maybe Text
  , _ifModel :: Maybe [Expr i]
  } deriving Show

data IfDefun i
  = IfDefun
  { _ifdName :: Text
  , _ifdArgs :: [MArg]
  , _ifdRetType :: Maybe Type
  , _ifdDocs :: Maybe Text
  , _ifdModel :: Maybe [Expr i]
  , _ifdInfo :: i
  } deriving Show

data IfDefCap i
  = IfDefCap
  { _ifdcName :: Text
  , _ifdcArgs :: [MArg]
  , _ifdcRetType :: Maybe Type
  , _ifdcDocs :: Maybe Text
  , _ifdcModel :: Maybe [Expr i]
  , _ifdcMeta :: Maybe DCapMeta
  , _ifdcInfo :: i
  } deriving Show

data IfDefPact i
  = IfDefPact
  { _ifdpName :: Text
  , _ifdpArgs :: [MArg]
  , _ifdpRetType :: Maybe Type
  , _ifdpDocs :: Maybe Text
  , _ifdpModel :: Maybe [Expr i]
  , _ifdpInfo :: i
  } deriving Show


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
  deriving Show

instance Pretty (DefConst i) where
  pretty (DefConst dcn dcty term _ _) =
    parens ("defconst" <+> pretty dcn <> mprettyTy dcty <+> pretty term)
    where
    mprettyTy = maybe mempty ((":" <>) . pretty)

instance Pretty Arg where
  pretty (Arg n ty) =
    pretty n <> ":" <+> pretty ty

instance Pretty MArg where
  pretty (MArg n mty) =
    pretty n <> maybe mempty (\ty -> ":" <+> pretty ty) mty

instance Pretty (Defun i) where
  pretty (Defun n args rettype term _ _ _) =
    parens ("defun" <+> pretty n <+> parens (prettyCommaSep args)
      <> ":" <+> pretty rettype <+> "=" <+> pretty term)

data Binder i =
  Binder Text (Maybe Type) (Expr i)
  deriving (Show, Eq, Functor)

instance Pretty (Binder i) where
  pretty (Binder ident ty e) =
    parens $ pretty ident <> maybe mempty ((":" <>) . pretty) ty <+> pretty e

data CapForm i
  = WithCapability ParsedName [Expr i] (Expr i)
  | RequireCapability ParsedName [Expr i]
  | ComposeCapability ParsedName [Expr i]
  | InstallCapability ParsedName [Expr i]
  | EmitEvent ParsedName [Expr i]
  deriving (Show, Eq, Functor)

data Expr i
  = Var ParsedName i
  | LetIn (NonEmpty (Binder i)) (Expr i) i
  | Lam [(Text, Maybe Type)] (Expr i) i
  | If (Expr i) (Expr i) (Expr i) i
  | App (Expr i) [Expr i] i
  | Block (NonEmpty (Expr i)) i
  | Operator Operator i
  | List [Expr i] i
  | Constant Literal i
  | Try (Expr i) (Expr i) i
  | Suspend (Expr i) i
  | DynAccess (Expr i) Text i
  | Object [(Field, Expr i)] i
  | Binding [(Field, MArg)] [Expr i] i
  | CapabilityForm (CapForm i) i
  | Error Text i
  deriving (Show, Eq, Functor)

data ReplSpecialForm i
  = ReplLoad Text Bool i
  | ReplTypechecks Text (Expr i) i
  | ReplTypecheckFail Text (Expr i) i
  deriving Show

data ReplSpecialTL i
  = RTL (ReplTopLevel i)
  | RTLReplSpecial (ReplSpecialForm i)
  deriving Show

data ReplTopLevel i
  = RTLModule (Module i)
  | RTLInterface (Interface i)
  | RTLDefun (Defun i)
  | RTLDefConst (DefConst i)
  | RTLTerm (Expr i)
  deriving Show

termInfo :: Lens' (Expr i) i
termInfo f = \case
  Var n i -> Var n <$> f i
  LetIn bnds e1 i ->
    LetIn bnds e1 <$> f i
  Lam nel e i ->
    Lam nel e <$> f i
  If e1 e2 e3 i ->
    If e1 e2 e3 <$> f i
  App e1 args i ->
    App e1 args <$> f i
  Block nel i ->
    Block nel <$> f i
  Object m i -> Object m <$> f i
  -- UnaryOp _op e i -> UnaryOp _op e <$> f i
  Operator op i ->
    Operator op <$> f i
  List nel i ->
    List nel <$> f i
  Suspend e i ->
    Suspend e <$> f i
  -- ObjectOp o i -> ObjectOp o <$> f i
  DynAccess e fn i -> DynAccess e fn <$> f i
  Constant l i ->
    Constant l <$> f i
  Try e1 e2 i ->
    Try e1 e2 <$> f i
  CapabilityForm e i ->
    CapabilityForm e <$> f i
  Error t i ->
    Error t <$> f i
  Binding t e i ->
    Binding t e <$> f i

instance Pretty (Expr i) where
  pretty = \case
    Var n _ -> pretty n
    LetIn bnds e _ ->
      parens ("let" <+> parens (hsep (NE.toList (pretty <$> bnds))) <+> pretty e)
    Lam nel e _ ->
      parens ("lambda" <+> parens (renderLamTypes nel) <+> pretty e)
    If cond e1 e2 _ ->
      parens ("if" <+> pretty cond <+> pretty e1 <+> pretty e2)
    App e1 [] _ ->
      parens (pretty e1)
    App e1 nel _ ->
      parens (pretty e1 <+> hsep (pretty <$> nel))
    Operator b _ -> pretty b
    Block nel _ ->
      parens ("progn" <+> hsep (pretty <$> NE.toList nel))
    Constant l _ ->
      pretty l
    List nel _ ->
      "[" <> prettyCommaSep nel <> "]"
    Try e1 e2 _ ->
      parens ("try" <+> pretty e1 <+> pretty e2)
    Error e _ ->
      parens ("error \"" <> pretty e <> "\"")
    DynAccess e f _ ->
      pretty e <> "::" <> pretty f
    Suspend e _ ->
      parens ("suspend" <+> pretty e)
    CapabilityForm c _ -> case c of
      WithCapability pn exs ex ->
        parens ("with-capability" <+> capApp pn exs <+> pretty ex)
      RequireCapability pn exs ->
        parens ("require-capability" <+> capApp pn exs)
      ComposeCapability pn exs ->
        parens ("compose-capability" <+> capApp pn exs)
      InstallCapability pn exs ->
        parens ("install-capability" <+> capApp pn exs)
      EmitEvent pn exs ->
        parens ("require-capability" <+> capApp pn exs)
      where
      capApp pn exns =
        parens (pretty pn <+> hsep (pretty <$> exns))
    Object m _ ->
      "{" <> pretty m <> "}"
    Binding{} -> error "boom"
    where
    renderLamPair (n, mt) = case mt of
      Nothing -> pretty n
      Just t -> pretty n <> ":" <> pretty t
    renderLamTypes = fold . intersperse " " . fmap renderLamPair
