{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}


module Pact.Core.Syntax.ParseTree where

import Control.Lens hiding (List, op, _head)
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

import Data.Functor.Const()

data Operator
  = AndOp
  | OrOp
  | EnforceOp
  | EnforceOneOp
  deriving (Show, Eq, Enum, Bounded)

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
data Type i
  = TyPrim PrimType i
  | TyList (Type i) i
  | TyPolyList i
  | TyModRef [ModuleName] i
  | TyKeyset i
  | TyObject ParsedTyName i
  | TyTable ParsedTyName i
  | TyPolyObject i
  deriving (Show, Eq, Functor)

tyInfo :: Type i -> i
tyInfo = \case
    TyPrim _ i -> i
    TyList _ i -> i
    TyModRef _ i -> i
    TyPolyList i -> i
    TyKeyset i -> i
    TyObject _ i -> i
    TyPolyObject i -> i
    TyTable _ i -> i

pattern TyInt :: i -> Type i
pattern TyInt i = TyPrim PrimInt i

pattern TyDecimal :: i -> Type i
pattern TyDecimal i = TyPrim PrimDecimal i

pattern TyBool :: i -> Type i
pattern TyBool i = TyPrim PrimBool i

pattern TyString :: i -> Type i
pattern TyString i = TyPrim PrimString i

pattern TyTime :: i -> Type i
pattern TyTime i = TyPrim PrimTime i

pattern TyUnit :: i -> Type i
pattern TyUnit i = TyPrim PrimUnit i

pattern TyGuard :: i -> Type i
pattern TyGuard i = TyPrim PrimGuard i

instance Pretty (Type i) where
  pretty = \case
    TyPrim prim _ -> pretty prim
    TyList t _ -> brackets (pretty t)
    TyModRef mn _ -> "module" <> braces (hsep (punctuate comma (pretty <$> mn)))
    TyPolyList _ -> "list"
    TyKeyset _ -> "keyset"
    TyObject qn _ -> "object" <> braces (pretty qn)
    TyPolyObject _ -> "object"
    TyTable o _ -> "table" <> braces (pretty o)

----------------------------------------------------
-- Common structures
----------------------------------------------------

data Arg i
  = Arg
  { _argName :: Text
  , _argType :: Type i
  , _argInfo :: i
  } deriving (Show , Functor)

data MArg i
  = MArg
  { _margName :: Text
  , _margType :: Maybe (Type i)
  , _margInfo :: i
  } deriving (Eq, Show, Functor)

data DefinedIdentifier i
  = DefinedIdentifier
  { _diText :: Text
  , _diInfo :: i
  } deriving (Show , Functor)

data ParsedDoc i
  = ParsedDoc
  { _dText :: Text
  , _dInfo :: i
  } deriving (Show , Functor)


defName :: Def i -> Text
defName = _diText . (\case
  Dfun d -> _dfunName d
  DConst d -> _dcName d
  DCap d -> _dcapName d
  DTable d -> _dtName d
  DPact d -> _dpName d
  DSchema d -> _dscName d)

defDocs :: Def i -> Maybe Text
defDocs = fmap _dText . (\case
  Dfun d -> _dfunDocs d
  DConst d -> _dcDocs d
  DCap d -> _dcapDocs d
  DTable d -> _dtDocs d
  DPact d -> _dpDocs d
  DSchema d -> _dscDocs d)

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
  { _dfunName :: DefinedIdentifier i
  , _dfunArgs :: [MArg i]
  , _dfunRetType :: Maybe (Type i)
  , _dfunTerm :: Expr i
  , _dfunDocs :: Maybe (ParsedDoc i)
  , _dfunModel :: [PropertyExpr i]
  , _dfunInfo :: i
  } deriving (Show, Functor)

data DefConst i
  = DefConst
  { _dcName :: DefinedIdentifier i
  , _dcType :: Maybe (Type i)
  , _dcTerm :: Expr i
  , _dcDocs :: Maybe (ParsedDoc i)
  , _dcInfo :: i
  } deriving (Show, Functor)

data DCapMeta
  = DefEvent
  | DefManaged (Maybe (Text, ParsedName))
  deriving Show

data DefCap i
  = DefCap
  { _dcapName :: DefinedIdentifier i
  , _dcapArgs :: ![MArg i]
  , _dcapRetType :: Maybe (Type i)
  , _dcapTerm :: Expr i
  , _dcapDocs :: Maybe (ParsedDoc i)
  , _dcapModel :: [PropertyExpr i]
  , _dcapMeta :: Maybe DCapMeta
  , _dcapInfo :: i
  } deriving (Show, Functor)

data DefSchema i
  = DefSchema
  { _dscName :: DefinedIdentifier i
  , _dscArgs :: [Arg i]
  , _dscDocs :: Maybe (ParsedDoc i)
  , _dscModel :: [PropertyExpr i]
  , _dscInfo :: i
  } deriving (Show, Functor)

data DefTable i
  = DefTable
  { _dtName :: DefinedIdentifier i
  , _dtSchema :: ParsedName
  , _dtDocs :: Maybe (ParsedDoc i)
  , _dtInfo :: i
  } deriving (Show, Functor)

data PactStep i
  = Step (Expr i) [PropertyExpr i] i
  | StepWithRollback (Expr i) (Expr i) [PropertyExpr i] i
  deriving (Show, Functor)

data DefPact i
  = DefPact
  { _dpName :: DefinedIdentifier i
  , _dpArgs :: [MArg i]
  , _dpRetType :: Maybe (Type i)
  , _dpSteps :: [PactStep i]
  , _dpDocs :: Maybe (ParsedDoc i)
  , _dpModel :: [PropertyExpr i]
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

data Module i
  = Module
  { _mName :: ModuleName
  , _mGovernance :: Governance ParsedName
  , _mExternal :: [ExtDecl]
  , _mDefs :: NonEmpty (Def i)
  , _mDoc :: Maybe (ParsedDoc i)
  , _mModel :: [PropertyExpr i]
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
  , _ifDocs :: Maybe (ParsedDoc i)
  , _ifModel :: [PropertyExpr i]
  , _ifInfo :: i
  } deriving (Show, Functor)

data IfDefun i
  = IfDefun
  { _ifdName :: DefinedIdentifier i
  , _ifdArgs :: [MArg i]
  , _ifdRetType :: Maybe (Type i)
  , _ifdDocs :: Maybe (ParsedDoc i)
  , _ifdModel :: [PropertyExpr i]
  , _ifdInfo :: i
  } deriving (Show, Functor)

data IfDefCap i
  = IfDefCap
  { _ifdcName :: DefinedIdentifier i
  , _ifdcArgs :: [MArg i]
  , _ifdcRetType :: Maybe (Type i)
  , _ifdcDocs :: Maybe (ParsedDoc i)
  , _ifdcModel :: [PropertyExpr i]
  , _ifdcMeta :: Maybe DCapMeta
  , _ifdcInfo :: i
  } deriving (Show, Functor)

data IfDefPact i
  = IfDefPact
  { _ifdpName :: DefinedIdentifier i
  , _ifdpArgs :: [MArg i]
  , _ifdpRetType :: Maybe (Type i)
  , _ifdpDocs :: Maybe (ParsedDoc i)
  , _ifdpModel :: [PropertyExpr i]
  , _ifdpInfo :: i
  } deriving (Show, Functor)

data PropKeyword
  = KwLet
  | KwLambda
  | KwIf
  | KwProgn
  | KwSuspend
  | KwTry
  | KwCreateUserGuard
  | KwWithCapability
  | KwEnforce
  | KwEnforceOne
  | KwAnd
  | KwOr
  | KwDefProperty
  deriving (Eq, Show)

data PropDelim
  = DelimLBracket
  | DelimRBracket
  | DelimLBrace
  | DelimRBrace
  | DelimComma
  | DelimColon
  | DelimWalrus -- := operator
  deriving Show

data PropertyExpr i
  = PropAtom ParsedName i
  | PropKeyword PropKeyword i
  | PropDelim PropDelim i
  | PropSequence [PropertyExpr i] i
  | PropConstant Literal i
  deriving (Show, Functor)


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

instance Pretty (DefinedIdentifier i) where
  pretty (DefinedIdentifier t _) = pretty t

instance Pretty (ParsedField i) where
  pretty (ParsedField f _) = pretty f

instance Pretty (DefConst i) where
  pretty (DefConst dcn dcty term _ _) =
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
  pretty (Defun n args rettype term _ _ _) =
    parens ("defun" <+> pretty n <+> parens (commaSep args)
      <> ":" <+> pretty rettype <+> "=" <+> pretty term)

data Binder i =
  Binder Text (Maybe (Type i)) (Expr i) i
  deriving (Show, Eq, Functor)

instance Pretty (Binder i) where
  pretty (Binder ident ty e _) =
    parens $ pretty ident <> maybe mempty ((":" <>) . pretty) ty <+> pretty e

data CapName i
  = CapName
  { _cnParsedName :: ParsedName
  , _cnInfo :: i
  } deriving (Show, Eq, Functor)

data CapForm i
  = WithCapability (Expr i) (Expr i) i
  | CreateUserGuard (CapName i) [Expr i]
  deriving (Show, Eq, Functor)

mbCapName :: CapForm i -> Maybe (CapName i)
mbCapName = \case
  WithCapability _ _ _ -> Nothing
  CreateUserGuard cn _ -> Just cn
  
data ParsedField i
  = ParsedField
  { _pfField :: Field
  , _pfInfo :: i
  } deriving (Show , Functor, Eq)


data Expr i
  = Var ParsedName i
  | LetIn (NonEmpty (Binder i)) (Expr i) i
  | Lam [MArg i] (Expr i) i
  | If (Expr i) (Expr i) (Expr i) i
  | App (Expr i) [Expr i] i
  | Block (NonEmpty (Expr i)) i
  | Operator Operator i
  | List [Expr i] i
  | Constant Literal i
  | Try (Expr i) (Expr i) i
  | Suspend (Expr i) i
  | Object [(ParsedField i, Expr i)] i
  | Binding [(ParsedField i, MArg i)] [Expr i] i
  | CapabilityForm (CapForm i) i
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

pattern RTLUse :: Import -> i -> ReplTopLevel i
pattern RTLUse imp i = RTLTopLevel (TLUse imp i)


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
    Suspend e _ ->
      parens ("suspend" <+> pretty e)
    CapabilityForm c _ -> case c of
      WithCapability cap body _ ->
        parens ("with-capability" <+> pretty cap <+> pretty body)
      CreateUserGuard pn exs ->
        parens ("create-user-guard" <> capApp (_cnParsedName pn) exs)
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
    renderLamPair (MArg n mt _) = case mt of
      Nothing -> pretty n
      Just t -> pretty n <> ":" <> pretty t
    renderLamTypes = fold . intersperse " " . fmap renderLamPair

