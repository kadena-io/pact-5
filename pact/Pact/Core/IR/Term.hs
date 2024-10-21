{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}


-- |
-- Module      :  Pact.Core.IR.Term
-- Copyright   :  (C) 2022 Kadena
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jose Cardona <jose@kadena.io>
--
-- Our Core IR, which is inspected for static guarantees before interpretation
-- The core IR manages to
--

module Pact.Core.IR.Term
  ( Term(..)
  -- ^ Our Primary Term representation
  , _Var, _Lam, _Let
  , _App, _BuiltinForm, _Builtin
  , _Constant, _Sequence, _Nullary
  , _ListLit, _ObjectLit
  , _InlineValue
  -- ^ Term prisms
  , termType, termInfo
  -- ^ term related lenses
  , ConstVal(..)
  -- ^ Defconst holder type.
  , TopLevel(..)
  , Module(..)
  -- ^ Our module representation
  , mName, mGovernance, mDefs
  , mBlessed, mImports, mImplements
  , mHash, mInfo, mTxHash, mCode
  , ModuleCode(..)
  -- ^ Module lenses
  , Defun(..)
  -- ^ Our repr for user defuns
  , dfunSpec, dfunArgs, dfunTerm
  , dfunInfo
  -- ^ Defun lenses
  , DefCap(..)
  -- ^ Our defcap repr
  , dcapSpec, dcapArgs, dcapTerm
  , dcapMeta, dcapInfo
  -- ^ Defcap lenses
  , DefSchema(..)
  , DefPact(..)
  -- ^ Our depact repr
  , dpSpec, dpArgs, dpSteps, dpInfo
  -- ^ defpact lenses
  , Step(..)
  , DefTable(..)
  , TableSchema(..)
  , DefConst(..)
  -- ^ Defconst
  , dcSpec, dcTerm, dcInfo
  -- ^ Defconst lenses
  , Def(..)
  -- ^ Our structure containing all different kind of definitions
  , _Dfun, _DConst, _DCap
  , _DSchema, _DTable, _DPact
  -- ^ def prisms
  , IfDefun(..)
  -- ^ Interface Defun signatures
  , ifdSpec, ifdArgs, ifdInfo
  -- ^ Interface Defun sig lenses
  , IfDefCap(..)
  -- ^ Interface defcap signature
  , ifdcSpec, ifdcArgs, ifdcMeta, ifdcInfo
  -- ^ Interface defcap signature lenses
  , IfDefPact(..)
  -- ^ Interface defpact signature lenses
  , ifdpSpec, ifdpArgs, ifdpInfo
  -- ^ Interface defpact signature lenses
  , IfDef(..)
  -- ^our interface definitions
  , _IfDfun, _IfDConst, _IfDCap
  , _IfDPact, _IfDSchema
  , Interface(..)
  , ReplTopLevel(..)
  -- ^ Interface repr
  , ifName, ifDefns, ifImports
  , ifHash, ifTxHash, ifInfo
  , ifCode
  -- ^ Interface lenses
  , hasRollback
  , ordinaryDefPactStepExec
  , defName
  , findDefInModule
  , defKind
  , ifDefKind
  , ifDefName
  , defInfo
  , defNameInfo
  , ifDefToDef
  , findDefInInterface
  , ifDefInfo
  , ifDefNameInfo
  -- ^ Misc utility functions
  , EvalTerm
  , EvalTopLevel
  , EvalDef
  , EvalDefun
  , EvalDefConst
  , EvalDefCap
  , EvalDefPact
  , EvalModule
  , EvalInterface
  , EvalIfDef
  , EvalTable
  , EvalSchema
  , EvalStep
  -- ^ Useful type aliases for evaluation
  , topLevelTerms
  , traverseDefunTerm
  , traverseTerm
  , termBuiltin
  , traverseModuleTerm
  , traverseIfDefTerm
  , traverseDefPactStep
  , traverseDefTerm
  , _dfunRType
  -- ^ Misc traversals
  , getHashedModuleName
  , getHashedModuleNameIface
  )
  where

import Control.Lens
import Data.Foldable(fold, find)
import Data.Text(Text)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict(Map)
import qualified Data.Set as Set
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Control.DeepSeq
import GHC.Generics

import Pact.Core.Guards
import Pact.Core.Builtin
import Pact.Core.Hash
import Pact.Core.Literal
import Pact.Core.Type
import Pact.Core.Names
import Pact.Core.Imports
import Pact.Core.Capabilities
import Pact.Core.PactValue
import Pact.Core.Pretty

-- | Core's main term representation
-- Todo: a few nodes could be merged into one representation, that is:
-- Nullary = Lam []
-- CapabilityForm and BuiltinForm could be merged into one
data Term name ty builtin info
  = Var name info
  -- ^ single variables e.g x
  | Lam (NonEmpty (Arg ty info)) (Term name ty builtin info) info
  -- ^ $f = \x.e
  -- Lambdas are named for the sake of the callstack.
  | Let (Arg ty info) (Term name ty builtin info) (Term name ty builtin info) info
  -- ^ let x = e1 in e2
  | App (Term name ty builtin info) [Term name ty builtin info] info
  -- ^ (e1 e2)
  | BuiltinForm (BuiltinForm (Term name ty builtin info)) info
  -- ^ BuiltinForm terms
  | Builtin builtin info
  -- ^ Built-in ops, e.g (+)
  | Constant Literal info
  -- ^ Literals
  | Sequence (Term name ty builtin info) (Term name ty builtin info) info
  -- ^ sequencing, that is e1 `Sequence` e2 evaluates e1
  -- discards the result and then evaluates and returns the result of e2
  | Nullary (Term name ty builtin info) info
  -- ^ "Lazy terms of arity zero"
  | ListLit [Term name ty builtin info] info
  -- ^ List Literals
  | ObjectLit [(Field, Term name ty builtin info)] info
  -- ^ Capability Natives
  | InlineValue PactValue info
  -- ^ Node for compatibility with production. this never shows up in our term language or the parser,
  -- but unfortunately, It's possible that this shows up as a value from pact < 5
  deriving (Show, Functor, Eq, Generic)

data ConstVal term
  = TermConst term
  | EvaledConst PactValue
  deriving (Show, Functor, Foldable, Traversable, Eq, Generic)

-- Note about `def*`: The name and return type is part of the `*Spec :: Arg ty info`
-- to include position information of the name

-- | Our defun representation, that is
-- (defun <name>(:<ty>)? (<args>*) <body>))
-- note our IR does not spit out docs.
-- In that case: refer to the repl.
data Defun name ty builtin info
  = Defun
  { _dfunSpec :: Arg ty info
  , _dfunArgs :: [Arg ty info]
  , _dfunTerm :: Term name ty builtin info
  , _dfunInfo :: info
  } deriving (Show, Functor, Eq, Generic)

-- | accessor for compat
_dfunRType :: Defun name ty builtin info -> Maybe ty
_dfunRType (Defun (Arg _ mty _) _ _ _) = mty

-- | (step <expr>)
-- or (step-with-rollback <expr1> <expr2>)
data Step name ty builtin info
  = Step (Term name ty builtin info)
  | StepWithRollback
    (Term name ty builtin info)
    (Term name ty builtin info)
  deriving (Show, Functor, Eq, Generic)

-- | (defpact <name>:<ret_ty> (arglist*) <steps>)
data DefPact name ty builtin info
  = DefPact
  { _dpSpec :: Arg ty info
  , _dpArgs :: [Arg ty info]
  , _dpSteps :: NonEmpty (Step name ty builtin info)
  , _dpInfo :: info
  } deriving (Show, Functor, Eq, Generic)

-- | Our defconst representation, that is
-- (defconst <name>(:<ty>)* <expr>)
-- Todo: ConstVal is not precisely type-safe.
-- Maybe a different IR is needed here?
data DefConst name ty builtin info
  = DefConst
  { _dcSpec :: Arg ty info
  , _dcTerm :: ConstVal (Term name ty builtin info)
  , _dcInfo :: info
  } deriving (Show, Functor, Eq, Generic)

-- | Our defcap representation, that is
-- (defcap <name>:<ty> (<args>) <meta> <body>)
data DefCap name ty builtin info
  = DefCap
  { _dcapSpec :: Arg ty info
  , _dcapArgs :: [Arg ty info]
  , _dcapTerm :: Term name ty builtin info
  , _dcapMeta :: DefCapMeta (FQNameRef name)
  , _dcapInfo :: info
  } deriving (Show, Functor, Eq, Generic)

data DefSchema ty info
  = DefSchema
  { _dsName :: Text
  , _dsSchema :: Map Field ty
  , _dsInfo :: info
  } deriving (Show, Functor, Eq, Generic)

hasRollback :: Step n t b i -> Bool
hasRollback Step{} = False
hasRollback StepWithRollback{} = True

ordinaryDefPactStepExec :: Step name ty builtin info -> Term name ty builtin info
ordinaryDefPactStepExec (Step expr) = expr
ordinaryDefPactStepExec (StepWithRollback expr _) = expr

-- | The type of our desugared table schemas
-- TODO: This GADT is unnecessarily complicated and only really necessary
-- because currently, renaming and desugaring are not in sequence. That is:
-- renaming and desugaring a module happens as a full desugar into a full rename.
-- if they ran one after another, this type would not be necessary
data TableSchema name where
  DesugaredTable :: ParsedName -> TableSchema ParsedName
  ResolvedTable :: Schema -> TableSchema Name

instance NFData (TableSchema name) where
  rnf (DesugaredTable pn) = rnf pn
  rnf (ResolvedTable sc) = rnf sc

instance Eq name => Eq (TableSchema name) where
  (DesugaredTable a) == (DesugaredTable b) = a == b
  (ResolvedTable a) == (ResolvedTable b) = a == b

instance Show (TableSchema name) where
  show (DesugaredTable t) = "DesugardTable(" <> show t <> ")"
  show (ResolvedTable t) = "ResolvedTable(" <> show t <> ")"

data DefTable name info
  = DefTable
  { _dtName :: Text
  , _dtSchema :: TableSchema name
  , _dtInfo :: info
  } deriving (Show, Functor, Eq, Generic)

data Def name ty builtin info
  = Dfun (Defun name ty builtin info)
  | DConst (DefConst name ty builtin info)
  | DCap (DefCap name ty builtin info)
  | DSchema (DefSchema ty info)
  | DTable (DefTable name info)
  | DPact (DefPact name ty builtin info)
  deriving (Show, Functor, Eq, Generic)

newtype ModuleCode
  = ModuleCode { _moduleCode :: Text }
  deriving newtype (Show, Eq, NFData)

data Module name ty builtin info
  = Module
  { _mName :: ModuleName
  -- ^ The module's name + namespace
  , _mGovernance :: Governance name
  -- ^ Module's governance type
  , _mDefs :: [Def name ty builtin info]
  -- ^ The module's definitions
  , _mBlessed :: !(Set.Set ModuleHash)
  -- ^ The blessed module hashes
  , _mImports :: [Import]
  -- ^ All imports from the modules
  , _mImplements :: [ModuleName]
  -- ^ Interfaces implemented by the module
  , _mHash :: ModuleHash
  -- ^ Module's hash
  , _mTxHash :: Hash
  -- ^ The tx where the module was deployed
  , _mCode :: ModuleCode
  -- ^ The module's code. This is a legacy compat field. This is only
  -- populated in legacy deployed module
  , _mInfo :: info
  -- ^ The module's location info
  } deriving (Show, Functor, Eq, Generic)

data Interface name ty builtin info
  = Interface
  { _ifName :: ModuleName
  -- ^ The interface's name + namespace
  , _ifDefns :: [IfDef name ty builtin info]
  -- ^ The interface's abstract definitions + constants
  , _ifImports :: [Import]
  -- ^ The imports the interface depends on
  , _ifHash :: ModuleHash
  -- ^ The interface's hash
  , _ifTxHash :: Hash
  -- ^ The tx where the interface was declared
  , _ifCode :: ModuleCode
  -- ^ The interface's source code. Note: this is a legacy compat field This is only
  -- populated in legacy deployed module
  , _ifInfo :: info
  -- ^ The interface's location info
  } deriving (Show, Eq, Functor, Generic)

data IfDefPact ty info
  = IfDefPact
  { _ifdpSpec :: Arg ty info
  , _ifdpArgs :: [Arg ty info]
  , _ifdpInfo :: info
  } deriving (Show, Eq, Functor, Generic)

data IfDefun ty info
  = IfDefun
  { _ifdSpec :: Arg ty info
  , _ifdArgs :: [Arg ty info]
  , _ifdInfo :: info
  } deriving (Show, Eq, Functor, Generic)

data IfDefCap name ty info
  = IfDefCap
  { _ifdcSpec :: Arg ty info
  , _ifdcArgs :: [Arg ty info]
  , _ifdcMeta :: DefCapMeta BareName
  , _ifdcInfo :: info
  } deriving (Show, Eq, Functor, Generic)

data IfDef name ty builtin info
  = IfDfun (IfDefun ty info)
  | IfDConst (DefConst name ty builtin info)
  | IfDCap (IfDefCap name ty info)
  | IfDPact (IfDefPact ty info)
  | IfDSchema (DefSchema ty info)
  deriving (Show, Eq, Functor, Generic)

data TopLevel name ty builtin info
  = TLModule (Module name ty builtin info)
  | TLInterface (Interface name ty builtin info)
  | TLTerm (Term name ty builtin info)
  | TLUse Import info
  deriving (Show, Functor)

data ReplTopLevel name ty builtin info
  = RTLDefConst (DefConst name ty builtin info)
  | RTLDefun (Defun name ty builtin info)
  deriving (Show, Functor)

defName :: Def name t b i -> Text
defName (Dfun d) = _argName $ _dfunSpec d
defName (DConst d) = _argName $ _dcSpec d
defName (DCap d) = _argName $ _dcapSpec d
defName (DSchema d) = _dsName d
defName (DTable d) = _dtName d
defName (DPact d) = _argName $ _dpSpec d

findDefInModule :: Text -> Module name ty b i -> Maybe (Def name ty b i)
findDefInModule defnName targetModule =
  find ((==) defnName . defName) (_mDefs targetModule)

defKind :: ModuleName -> Def name Type b i -> DefKind
defKind mn = \case
  Dfun{} -> DKDefun
  DConst{} -> DKDefConst
  DCap{} -> DKDefCap
  DSchema ds -> DKDefSchema (Schema (QualifiedName (_dsName ds) mn) (_dsSchema ds))
  DTable{} -> DKDefTable
  DPact{} -> DKDefPact

ifDefKind :: ModuleName -> IfDef name Type b i -> Maybe DefKind
ifDefKind mn = \case
  IfDfun{} -> Nothing
  IfDCap{} -> Nothing
  IfDConst{} -> Just DKDefConst
  IfDPact{} -> Nothing
  IfDSchema ds -> Just $ DKDefSchema (Schema (QualifiedName (_dsName ds) mn) (_dsSchema ds))

ifDefName :: IfDef name ty builtin i -> Text
ifDefName = \case
  IfDfun ifd -> _argName $ _ifdSpec ifd
  IfDConst dc -> _argName $ _dcSpec dc
  IfDCap ifd -> _argName $ _ifdcSpec ifd
  IfDPact ifd -> _argName $ _ifdpSpec ifd
  IfDSchema dc -> _dsName dc

defInfo :: Def name ty b i -> i
defInfo = \case
  Dfun de -> _dfunInfo de
  DConst dc -> _dcInfo dc
  DCap dc -> _dcapInfo dc
  DSchema dc -> _dsInfo dc
  DTable dt -> _dtInfo dt
  DPact dp -> _dpInfo dp

defNameInfo :: Def name ty b i -> i
defNameInfo = \case
  Dfun de -> _argInfo $ _dfunSpec de
  DConst dc -> _argInfo $ _dcSpec dc
  DCap dc -> _argInfo $  _dcapSpec dc
  DSchema dc -> _dsInfo dc
  DTable dt -> _dtInfo dt
  DPact dp -> _argInfo $ _dpSpec dp

ifDefToDef :: IfDef name ty b i -> Maybe (Def name ty b i)
ifDefToDef = \case
  IfDfun _ -> Nothing
  IfDConst dc -> Just (DConst dc)
  IfDCap _ -> Nothing
  IfDPact _ -> Nothing
  IfDSchema dc -> Just (DSchema dc)

findDefInInterface :: Text -> Interface name ty b i -> Maybe (Def name ty b i)
findDefInInterface defnName targetIface =
  find ((==) defnName . ifDefName) (_ifDefns targetIface) >>= ifDefToDef

ifDefInfo :: IfDef name ty b i -> i
ifDefInfo = \case
  IfDfun de -> _ifdInfo de
  IfDConst dc -> _dcInfo dc
  IfDCap d -> _ifdcInfo d
  IfDPact d -> _ifdpInfo d
  IfDSchema d -> _dsInfo d

ifDefNameInfo :: IfDef name ty b i -> i
ifDefNameInfo = \case
  IfDfun de -> _argInfo $ _ifdSpec de
  IfDConst dc -> _argInfo $ _dcSpec dc
  IfDCap d -> _argInfo $ _ifdcSpec d
  IfDPact d -> _argInfo $ _ifdpSpec d
  IfDSchema d -> _dsInfo d


instance (Pretty name, Pretty builtin, Pretty ty) => Pretty (Term name ty builtin info) where
  pretty = \case
    Var name _ -> pretty name
    Lam ne te _ ->
      parens ("lambda" <+> parens (fold (NE.intersperse " " (prettyLamArg <$> ne))) <+> pretty te)
    Let n te te' _ ->
      let (args, out) = unrollLets te'
          prettiedArgs = align $ vsep ((\(v, expr) -> parens (pretty v <+> pretty expr)) <$> (n, te):args)
      in parens $ "let" <+> parens prettiedArgs <> nest 2 (line <> pretty out)
      where
      unrollLets = \case
        Let name arg expr _ ->
          let (acc, e) = unrollLets expr
          in ((name, arg):acc, e)
        expr -> ([], expr)
    App te ne _ ->
      parens (pretty te <> if null ne then mempty else space <> hsep (pretty <$> ne))
    Sequence te te' _ ->
      let terms = te : unSeqTerm te'
      in parens ("do" <+> nest 2 (line <> vsep (pretty <$> terms)))
    BuiltinForm o _ ->
      pretty o
    Builtin builtin _ -> pretty builtin
    Constant lit _ ->
      pretty lit
    Nullary term _ ->
      parens ("suspend" <+> pretty term)
    ListLit tes _ ->
      pretty tes
    ObjectLit n _ ->
      braces (hsep $ punctuate "," $ fmap (\(f, t) -> dquotes (pretty f) <> ":" <> pretty t) n)
    InlineValue pv _ ->
      -- Note: This term is only used for back compat. with Pact < 5
      pretty pv
    where
    prettyTyAnn = maybe mempty ((":" <>) . pretty)
    prettyLamArg (Arg n ty _) =
      pretty n <> prettyTyAnn ty

unSeqTerm :: Term name ty builtin info -> [Term name ty builtin info]
unSeqTerm (Sequence e1 e2 _) = e1 : unSeqTerm e2
unSeqTerm e = [e]

instance (Pretty name, Pretty builtin, Pretty ty) => Pretty (TopLevel name ty builtin info) where
  pretty = \case
    TLTerm tm -> pretty tm
    TLModule m -> pretty m
    _ -> "todo: pretty interfaces"

instance (Pretty name, Pretty ty, Pretty b) => Pretty (Defun name ty b i) where
  pretty (Defun spec args e _) =
    parens $ "defun" <+> pretty spec <+> parens (hsep (pretty <$> args)) <+> nest 2 (line <> pretty e)

instance (Pretty name, Pretty ty, Pretty b) => Pretty (DefPact name ty b i) where
  pretty (DefPact spec args e _) =
    parens $ "defpact" <+> pretty spec <+> parens (hsep (pretty <$> args)) <+> nest 2 (line <> vsep (pretty <$> NE.toList e))

instance (Pretty name, Pretty ty, Pretty b) => Pretty (DefCap name ty b i) where
  pretty (DefCap spec args e mged _) = case mged of
    Unmanaged ->
      parens $ "defcap" <+> pretty spec <+> parens (hsep (pretty <$> args)) <+> nest 2 (line <> pretty e)
    _ ->
      parens $ "defun" <+> pretty spec <+> parens (hsep (pretty <$> args)) <+> nest 2 (line <> vsep [pretty mged, pretty e])

instance Pretty ty => Pretty (DefSchema ty info) where
  pretty (DefSchema n schema i) =
    let argList = [Arg k (Just t) i | (Field k, t) <- M.toList schema]
    in pretty $ PrettyLispApp ("defschema " <> n) argList

instance Pretty (TableSchema name) where
  pretty (DesugaredTable pn) = pretty pn
  pretty (ResolvedTable (Schema pn _)) = pretty pn

instance Pretty (DefTable name info) where
  pretty (DefTable tblname schema _) =
    parens $ "deftable" <+> pretty tblname <> ":" <> pretty schema

instance Pretty term => Pretty (ConstVal term) where
  pretty = \case
    TermConst t -> pretty t
    EvaledConst v -> pretty v

instance (Pretty name, Pretty builtin, Pretty ty) => Pretty (Step name ty builtin info) where
  pretty = \case
    Step t -> parens ("step" <+> pretty t)
    StepWithRollback t1 t2 -> parens ("step-with-rollback" <+> pretty t1 <+> pretty t2)


instance (Pretty name, Pretty ty, Pretty b) => Pretty (DefConst name ty b i) where
  pretty (DefConst (Arg n mty _) term _) =
    parens $ "defconst" <+> pretty n <> maybe mempty ((":" <>) . pretty) mty <+> pretty term

instance (Pretty name, Pretty ty, Pretty b) => Pretty (Def name ty b i) where
  pretty = \case
    Dfun d -> pretty d
    DConst d -> pretty d
    DCap d -> pretty d
    DSchema d -> pretty d
    DTable d -> pretty d
    DPact d -> pretty d

instance (Pretty name, Pretty ty, Pretty b) => Pretty (Module name ty b i) where
  pretty (Module mname gov defs _blessed imports implements mhash _mtxh _mcode _minfo) =
    parens $
      "module"
      <+> pretty mname
      <+> pretty gov
      <> nest 2 (
        line <>
        "; HASH:" <+> dquotes (pretty mhash) <> line <>
        (if null imports then mempty else vsep (pretty <$> imports) <> line) <>
        (if null implements then mempty else vsep ((\p -> parens ("implements" <+> pretty p)) <$> implements) <> line) <>
        vsep (pretty <$> defs)
      )

makeLenses ''Module
makeLenses ''Interface
makeLenses ''Defun
makeLenses ''DefConst
makeLenses ''DefCap
makeLenses ''DefPact
makePrisms ''Def
makePrisms ''Term
makePrisms ''IfDef

makeLenses ''IfDefun
makeLenses ''IfDefPact
makeLenses ''IfDefCap

-----------------------------------------
-- Term traversals and builtins
-----------------------------------------
termType :: Traversal (Term n t b i) (Term n t' b i) t t'
termType f  = \case
  Var n i -> pure (Var n i)
  Lam ne te i ->
    Lam <$> (traversed.argType._Just) f ne <*> termType f te <*> pure i
  Let n te te' i ->
    Let <$> (argType . _Just) f n <*> termType f te <*> termType f te' <*> pure i
  App te ne i ->
    App <$> termType f te <*> traverse (termType f) ne <*> pure i
  Sequence te te' i ->
    Sequence <$> termType f te <*> termType f te' <*> pure i
  BuiltinForm bf i ->
    BuiltinForm <$> traverse (termType f) bf <*> pure i
  Builtin b i -> pure (Builtin b i)
  Nullary term i ->
    Nullary <$> termType f term <*> pure i
  Constant lit i ->
    pure (Constant lit i)
  ListLit tes i ->
    ListLit <$> traverse (termType f) tes <*> pure i
  ObjectLit m i ->
    ObjectLit <$> (traverse._2) (termType f) m <*> pure i
  InlineValue v i ->
    pure (InlineValue v i)

-- | Traverse all terms
termBuiltin :: Traversal (Term n t b i) (Term n t b' i) b b'
termBuiltin f = \case
  Var n i -> pure (Var n i)
  Lam ne te i ->
    Lam ne <$> termBuiltin f te <*> pure i
  Let n te te' i ->
    Let n <$> termBuiltin f te <*> termBuiltin f te' <*> pure i
  App te ne i ->
    App <$> termBuiltin f te <*> traverse (termBuiltin f) ne <*> pure i
  Sequence te te' i ->
    Sequence <$> termBuiltin f te <*> termBuiltin f te' <*> pure i
  BuiltinForm bf i ->
    BuiltinForm <$> traverse (termBuiltin f) bf <*> pure i
  Builtin b i ->
    Builtin <$> f b <*> pure i
  Nullary term i ->
    Nullary <$> termBuiltin f term <*> pure i
  Constant lit i ->
    pure (Constant lit i)
  ListLit tes i ->
    ListLit <$> traverse (termBuiltin f) tes <*> pure i
  ObjectLit m i ->
    ObjectLit <$> (traverse._2) (termBuiltin f) m <*> pure i
  InlineValue v i -> pure (InlineValue v i)

termInfo :: Lens' (Term name ty builtin info) info
termInfo f = \case
  Var n i -> Var n <$> f i
  Let n t1 t2 i ->
    Let n t1 t2 <$> f i
  Lam ns term i -> Lam ns term <$> f i
  App t1 t2 i -> App t1 t2 <$> f i
  Builtin b i -> Builtin b <$> f i
  Constant l i -> Constant l <$> f i
  Sequence e1 e2 i -> Sequence e1 e2 <$> f i
  BuiltinForm o i ->
    BuiltinForm o <$> f i
  ListLit l i  -> ListLit l <$> f i
  Nullary term i ->
    Nullary term <$> f i
  ObjectLit m i -> ObjectLit m <$> f i
  InlineValue v i -> InlineValue v <$> f i

-- TODO: add test cases for all traversal
traverseTerm
  :: Traversal' (Term name ty builtin info)
                (Term name ty builtin info)
traverseTerm f x= case x of
  Var n i -> f (Var n i)
  Lam ne te i ->
    Lam ne <$> traverseTerm f te <*> pure i
  Let n te te' i ->
    Let n <$> traverseTerm f te <*> traverseTerm f te' <*> pure i
  App te ne i ->
    App <$> traverseTerm f te <*> traverse (traverseTerm f) ne <*> pure i
  Sequence te te' i ->
    Sequence <$> traverseTerm f te <*> traverseTerm f te' <*> pure i
  BuiltinForm bf i ->
    BuiltinForm <$> traverse (traverseTerm f) bf <*> pure i
  Builtin b i -> f (Builtin b i)
  Nullary term i ->
    Nullary <$> traverseTerm f term <*> pure i
  Constant lit i ->
    f (Constant lit i)
  ListLit tes i ->
    ListLit <$> traverse (traverseTerm f) tes <*> pure i
  ObjectLit m i ->
    ObjectLit <$> (traverse._2) (traverseTerm f) m <*> pure i
  InlineValue v i ->
    f (InlineValue v i)

topLevelTerms :: Traversal' (TopLevel name ty builtin info) (Term name ty builtin info)
topLevelTerms f = \case
  TLModule md -> TLModule <$> traverseModuleTerm f md
  TLInterface iface -> pure (TLInterface iface)
  TLTerm t -> TLTerm <$> f t
  TLUse u i -> pure (TLUse u i)

getHashedModuleName :: Module name ty b i -> HashedModuleName
getHashedModuleName m =
  HashedModuleName (_mName m) (_mHash m)

getHashedModuleNameIface :: Interface name ty b i -> HashedModuleName
getHashedModuleNameIface m =
  HashedModuleName (_ifName m) (_ifHash m)

traverseDefunTerm
  :: Traversal (Defun name ty builtin info)
               (Defun name' ty builtin' info)
               (Term name ty builtin info)
               (Term name' ty builtin' info)
traverseDefunTerm f (Defun spec args term i) =
  (\term' -> Defun spec args term' i) <$> f term

traverseDefConstTerm
  :: Traversal (DefConst name ty builtin info)
               (DefConst name' ty builtin' info)
               (Term name ty builtin info)
               (Term name' ty builtin' info)
traverseDefConstTerm f (DefConst spec term i) =
  (\term' -> DefConst spec term' i)  <$> traverse f term

traverseDefCapTerm
  :: Traversal (DefCap name ty builtin info)
               (DefCap name ty builtin' info)
               (Term name ty builtin info)
               (Term name ty builtin' info)
traverseDefCapTerm f (DefCap spec args term meta i) =
  (\term' -> DefCap spec args term' meta i) <$> f term


traverseDefPactStep
  :: Traversal (Step name ty builtin info)
               (Step name' ty' builtin' info')
               (Term name ty builtin info)
               (Term name' ty' builtin' info')
traverseDefPactStep f = \case
  Step t -> Step <$> f t
  StepWithRollback a1 a2 ->
    StepWithRollback <$> f a1 <*> f a2

traverseDefPactTerm
  :: Traversal (DefPact name ty builtin info)
               (DefPact name ty builtin' info)
               (Term name ty builtin info)
               (Term name ty builtin' info)
traverseDefPactTerm f (DefPact spec args steps info) =
  (\steps' -> DefPact spec args steps' info) <$> traverse (traverseDefPactStep f) steps


traverseDefTerm
  :: Traversal (Def name ty builtin info)
               (Def name ty builtin' info)
               (Term name ty builtin info)
               (Term name ty builtin' info)
traverseDefTerm f = \case
  Dfun d -> Dfun <$> traverseDefunTerm f d
  DCap d -> DCap <$> traverseDefCapTerm f d
  DConst d -> DConst <$> traverseDefConstTerm f d
  DSchema d -> pure (DSchema d)
  DTable d -> pure (DTable d)
  DPact d -> DPact <$> traverseDefPactTerm f d

traverseModuleTerm
  :: Traversal (Module name ty builtin info)
               (Module name ty builtin' info)
               (Term name ty builtin info)
               (Term name ty builtin' info)
traverseModuleTerm f m =
  (mDefs . traversed) (traverseDefTerm f) m


traverseIfDefTerm
  :: Traversal (IfDef name ty builtin info)
               (IfDef name ty builtin' info)
               (Term name ty builtin info)
               (Term name ty builtin' info)
traverseIfDefTerm f = \case
  IfDfun d -> pure (IfDfun d)
  IfDConst d -> IfDConst <$> traverseDefConstTerm f d
  IfDCap d -> pure (IfDCap d)
  IfDPact d -> pure (IfDPact d)
  IfDSchema d -> pure (IfDSchema d)

instance Plated (Term name ty builtin info) where
  plate f = \case
    Var n i -> pure (Var n i)
    Lam ns term i -> Lam ns <$> f term <*> pure i
    Let n t1 t2 i -> Let n <$> f t1 <*> f t2 <*> pure i
    App t1 t2 i -> App <$> f t1 <*> traverse f t2 <*> pure i
    Builtin b i -> pure (Builtin b i)
    Constant l i -> pure (Constant l i)
    Sequence e1 e2 i -> Sequence <$> f e1 <*> f e2 <*> pure i
    BuiltinForm o i ->
      BuiltinForm <$> traverse f o <*> pure i
    ListLit m i -> ListLit <$> traverse f m <*> pure i
    Nullary term i ->
      Nullary <$> f term <*> pure i
    ObjectLit o i ->
      ObjectLit <$> (traverse._2) f o <*> pure i
    InlineValue v i -> pure (InlineValue v i)


-----------------------------------------
-- Type Aliases for evaluation
-----------------------------------------
type EvalTerm b i = Term Name Type b i
type EvalTopLevel b i = TopLevel Name Type b i
type EvalDef b i = Def Name Type b i
type EvalDefun b i = Defun Name Type b i
type EvalDefConst b i = DefConst Name Type b i
type EvalDefCap b i = DefCap Name Type b i
type EvalDefPact b i = DefPact Name Type b i
type EvalModule b i = Module Name Type b i
type EvalInterface b i = Interface Name Type b i
type EvalIfDef b i = IfDef Name Type b i
type EvalTable i = DefTable Name i
type EvalSchema i = DefSchema Type i
type EvalStep b i = Step Name Type b i

instance (NFData name, NFData ty, NFData b, NFData info) => NFData (Term name ty b info)
instance (NFData name, NFData ty, NFData b, NFData info) => NFData (Def name ty b info)
instance (NFData name, NFData info) => NFData (DefSchema name info)
instance (NFData name, NFData ty, NFData b, NFData info) => NFData (Defun name ty b info)
instance (NFData name, NFData ty, NFData b, NFData info) => NFData (DefConst name ty b info)
instance (NFData name, NFData ty, NFData b, NFData info) => NFData (DefCap name ty b info)
instance (NFData name, NFData ty, NFData b, NFData info) => NFData (DefPact name ty b info)
instance (NFData name, NFData ty, NFData b, NFData info) => NFData (Step name ty b info)
instance (NFData name, NFData ty, NFData b, NFData info) => NFData (Module name ty b info)
instance (NFData name, NFData ty, NFData b, NFData info) => NFData (Interface name ty b info)
instance (NFData name, NFData ty, NFData b, NFData info) => NFData (IfDef name ty b info)
instance (NFData ty, NFData info) => NFData (IfDefun ty info)
instance (NFData ty, NFData info) => NFData (IfDefPact ty info)
instance (NFData name, NFData ty, NFData info) => NFData (IfDefCap name ty info)
instance (NFData name, NFData info) => NFData (DefTable name info)
instance (NFData term) => NFData (ConstVal term)
