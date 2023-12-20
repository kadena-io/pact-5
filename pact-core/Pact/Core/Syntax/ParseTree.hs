{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternSynonyms #-}

module Pact.Core.Syntax.ParseTree where

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
import Pact.Core.Guards


data Operator
  = AndOp
  | OrOp
  | EnforceOp
  | EnforceOneOp
  deriving (Show, Eq, Enum, Bounded)

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
  deriving (Show, Eq)

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


----------------------------------------------------
-- Common structures
----------------------------------------------------

data Arg
  = Arg
  { _argName :: Text
  , _argType :: Type
  } deriving Show

data MArg
  = MArg
  { _margName :: Text
  , _margType :: Maybe Type
  } deriving (Eq, Show)

data Defun i
  = Defun
  { _dfunName :: Text
  , _dfunArgs :: [MArg]
  , _dfunRetType :: Maybe Type
  , _dfunTerm :: Expr i
  , _dfunDocs :: Maybe Text
  , _dfunModel :: Maybe [FVFunModel i]
  , _dfunInfo :: i
  } deriving (Show, Functor)

data DefConst i
  = DefConst
  { _dcName :: Text
  , _dcType :: Maybe Type
  , _dcTerm :: Expr i
  , _dcDocs :: Maybe Text
  , _dcInfo :: i
  } deriving (Show, Functor)

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
  , _dcapModel :: Maybe [FVFunModel i]
  , _dcapMeta :: Maybe DCapMeta
  , _dcapInfo :: i
  } deriving (Show, Functor)

data DefSchema i
  = DefSchema
  { _dscName :: Text
  , _dscArgs :: [Arg]
  , _dscDocs :: Maybe Text
  , _dscModel :: Maybe [FVFunModel i]
  , _dscInfo :: i
  } deriving (Show, Functor)

data DefTable i
  = DefTable
  { _dtName :: Text
  , _dtSchema :: ParsedName
  , _dtDocs :: Maybe Text
  , _dtInfo :: i
  } deriving (Show, Functor)

data PactStep i
  = Step (Expr i) (Maybe [FVFunModel i])
  | StepWithRollback (Expr i) (Expr i) (Maybe [FVFunModel i])
  deriving (Show, Functor)

data DefPact i
  = DefPact
  { _dpName :: Text
  , _dpArgs :: [MArg]
  , _dpRetType :: Maybe Type
  , _dpSteps :: [PactStep i]
  , _dpDocs :: Maybe Text
  , _dpModel :: Maybe [FVFunModel i]
  , _dpInfo :: i
  } deriving (Show, Functor)

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
  deriving (Show, Functor)

data Import
  = Import
  { _impModuleName  :: ModuleName
  , _impModuleHash :: Maybe Text
  , _impImported :: Maybe [Text] }
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
  } deriving (Show, Functor)

newtype Property i
  = Property (Expr i)
  deriving (Show, Functor)

newtype Invariant i
  = Invariant (Expr i)
  deriving (Show, Functor)

data FVModel i
  = FVDefProperty (DefProperty i)
  | FVProperty (Property i)
  | FVInvariant (Invariant i)
  deriving (Show, Functor)

data FVFunModel i
  = FVFunProperty (Property i)
  | FVFunInvariant (Invariant i)
  deriving (Show, Functor)

data Module i
  = Module
  { _mName :: ModuleName
  , _mGovernance :: Governance ParsedName
  , _mExternal :: [ExtDecl]
  , _mDefs :: NonEmpty (Def i)
  , _mDoc :: Maybe Text
  , _mModel :: [FVModel i]
  , _mInfo :: i
  } deriving (Show, Functor)

data TopLevel i
  = TLModule (Module i)
  | TLInterface (Interface i)
  | TLTerm (Expr i)
  | TLUse Import i
  deriving (Show, Functor)

data Interface i
  = Interface
  { _ifName :: ModuleName
  , _ifDefns :: [IfDef i]
  , _ifImports :: [Import]
  , _ifDocs :: Maybe Text
  , _ifModel :: [FVModel i]
  , _ifInfo :: i
  } deriving (Show, Functor)

data IfDefun i
  = IfDefun
  { _ifdName :: Text
  , _ifdArgs :: [MArg]
  , _ifdRetType :: Maybe Type
  , _ifdDocs :: Maybe Text
  , _ifdModel :: Maybe [FVFunModel i]
  , _ifdInfo :: i
  } deriving (Show, Functor)

data IfDefCap i
  = IfDefCap
  { _ifdcName :: Text
  , _ifdcArgs :: [MArg]
  , _ifdcRetType :: Maybe Type
  , _ifdcDocs :: Maybe Text
  , _ifdcModel :: Maybe [FVFunModel i]
  , _ifdcMeta :: Maybe DCapMeta
  , _ifdcInfo :: i
  } deriving (Show, Functor)

data IfDefPact i
  = IfDefPact
  { _ifdpName :: Text
  , _ifdpArgs :: [MArg]
  , _ifdpRetType :: Maybe Type
  , _ifdpDocs :: Maybe Text
  , _ifdpModel :: Maybe [FVFunModel i]
  , _ifdpInfo :: i
  } deriving (Show, Functor)


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
  deriving (Show, Functor)

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
    parens ("defun" <+> pretty n <+> parens (commaSep args)
      <> ":" <+> pretty rettype <+> "=" <+> pretty term)

data Binder i =
  Binder Text (Maybe Type) (Expr i)
  deriving (Show, Eq, Functor)

instance Pretty (Binder i) where
  pretty (Binder ident ty e) =
    parens $ pretty ident <> maybe mempty ((":" <>) . pretty) ty <+> pretty e

data CapForm i
  = WithCapability (Expr i) (Expr i)
  | CreateUserGuard ParsedName [Expr i]
  deriving (Show, Eq, Functor)

data Expr i
  = Var ParsedName i
  | LetIn (NonEmpty (Binder i)) (Expr i) i
  | Lam [MArg] (Expr i) i
  | If (Expr i) (Expr i) (Expr i) i
  | App (Expr i) [Expr i] i
  | Block (NonEmpty (Expr i)) i
  | Operator Operator i
  | List [Expr i] i
  | Constant Literal i
  | Try (Expr i) (Expr i) i
  | Suspend (Expr i) i
  | Object [(Field, Expr i)] i
  | Binding [(Field, MArg)] [Expr i] i
  | CapabilityForm (CapForm i) i
  | Error Text i
  deriving (Show, Eq, Functor)

data ReplSpecialForm i
  = ReplLoad Text Bool i
  deriving Show

data ReplSpecialTL i
  = RTL (ReplTopLevel i)
  | RTLReplSpecial (ReplSpecialForm i)
  deriving Show

data ReplTopLevel i
  = RTLTopLevel (TopLevel i)
  | RTLDefun (Defun i)
  | RTLDefConst (DefConst i)
  deriving Show

pattern RTLModule :: Module i -> ReplTopLevel i
pattern RTLModule m = RTLTopLevel (TLModule m)

pattern RTLInterface :: Interface i -> ReplTopLevel i
pattern RTLInterface m = RTLTopLevel (TLInterface m)

pattern RTLTerm :: Expr i -> ReplTopLevel i
pattern RTLTerm te = RTLTopLevel (TLTerm te)


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
  Operator op i ->
    Operator op <$> f i
  List nel i ->
    List nel <$> f i
  Suspend e i ->
    Suspend e <$> f i
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
      "[" <> commaSep nel <> "]"
    Try e1 e2 _ ->
      parens ("try" <+> pretty e1 <+> pretty e2)
    Error e _ ->
      parens ("error \"" <> pretty e <> "\"")
    Suspend e _ ->
      parens ("suspend" <+> pretty e)
    CapabilityForm c _ -> case c of
      WithCapability cap body ->
        parens ("with-capability" <+> pretty cap <+> pretty body)
      CreateUserGuard pn exs ->
        parens ("create-user-guard" <> capApp pn exs)
      where
      capApp pn exns =
        parens (pretty pn <+> hsep (pretty <$> exns))
    Object m _ ->
      braces (hsep (punctuate "," (prettyObj m)))
    Binding binds body _ ->
      braces (hsep $ punctuate "," $ fmap prettyBind binds) <+>
        hsep (pretty <$> body)
    where
    prettyBind (f, e) = pretty f <+> ":=" <+> pretty e
    prettyObj = fmap (\(n, k) -> dquotes (pretty n) <> ":" <> pretty k)
    renderLamPair (MArg n mt) = case mt of
      Nothing -> pretty n
      Just t -> pretty n <> ":" <> pretty t
    renderLamTypes = fold . intersperse " " . fmap renderLamPair
