{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}


-- |
-- Module      :  Pact.Core.IR.Term
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jose Cardona <jose@kadena.io>
--
-- Our Core IR, which is inspected for static guarantees before interpretation
-- The core IR manages to
--

module Pact.Core.IR.Term where

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

-- | Core's IR term
-- Todo: a few nodes could be merged into one representation, that is:
-- Nullary = Lam []
-- CapabilityForm and Conditional could be merged into one
data Term name ty builtin info
  = Var name info
  -- ^ single variables e.g x
  | Lam (NonEmpty (Arg ty)) (Term name ty builtin info) info
  -- ^ $f = \x.e
  -- Lambdas are named for the sake of the callstack.
  | Let (Arg ty) (Term name ty builtin info) (Term name ty builtin info) info
  -- ^ let x = e1 in e2
  | App (Term name ty builtin info) [Term name ty builtin info] info
  -- ^ (e1 e2)
  | Conditional (BuiltinForm (Term name ty builtin info)) info
  -- ^ Conditional terms
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
  | Try (Term name ty builtin info) (Term name ty builtin info) info
  -- ^ try (catch expr) (try-expr)
  | ObjectLit [(Field, Term name ty builtin info)] info
  -- ^ an object literal
  | CapabilityForm (CapForm name (Term name ty builtin info)) info
  -- ^ Capability Natives
  deriving (Show, Functor, Eq, Generic)

data ConstVal term
  = TermConst term
  | EvaledConst PactValue
  deriving (Show, Functor, Foldable, Traversable, Eq, Generic)

-- | Our defun representation, that is
-- (defun <name>(:<ty>)? (<args>*) <body>))
-- note our IR does not spit out docs.
-- In that case: refer to the repl.
data Defun name ty builtin info
  = Defun
  { _dfunName :: Text
  , _dfunArgs :: [Arg ty]
  , _dfunRType :: Maybe ty
  , _dfunTerm :: Term name ty builtin info
  , _dfunInfo :: info
  } deriving (Show, Functor, Eq, Generic)

data Step name ty builtin info
  = Step (Term name ty builtin info)
  | StepWithRollback
    (Term name ty builtin info)
    (Term name ty builtin info)
  deriving (Show, Functor, Eq, Generic)

data DefPact name ty builtin info
  = DefPact
  { _dpName :: Text
  , _dpArgs :: [Arg ty]
  , _dpRetType :: Maybe ty
  , _dpSteps :: NonEmpty (Step name ty builtin info)
  , _dpInfo :: info
  } deriving (Show, Functor, Eq, Generic)

-- | Our defconst representation, that is
-- (defconst <name>(:<ty>)* <expr>)
-- Todo: ConstVal is not precisely type-safe.
-- Maybe a different IR is needed here?
data DefConst name ty builtin info
  = DefConst
  { _dcName :: Text
  , _dcType :: Maybe ty
  , _dcTerm :: ConstVal (Term name ty builtin info)
  , _dcInfo :: info
  } deriving (Show, Functor, Eq, Generic)

-- | Our defcap representation, that is
-- (defcap <name>:<ty> (<args>) <meta> <body>)
data DefCap name ty builtin info
  = DefCap
  { _dcapName :: Text
  , _dcapArgs :: [Arg ty]
  , _dcapRType :: Maybe ty
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

data Module name ty builtin info
  = Module
  { _mName :: ModuleName
  , _mGovernance :: Governance name
  , _mDefs :: [Def name ty builtin info]
  , _mBlessed :: !(Set.Set ModuleHash)
  , _mImports :: [Import]
  , _mImplements :: [ModuleName]
  , _mHash :: ModuleHash
  , _mInfo :: info
  } deriving (Show, Functor, Eq, Generic)

data Interface name ty builtin info
  = Interface
  { _ifName :: ModuleName
  , _ifDefns :: [IfDef name ty builtin info]
  , _ifImports :: [Import]
  , _ifHash :: ModuleHash
  , _ifInfo :: info
  } deriving (Show, Eq, Functor, Generic)

data IfDefPact ty info
  = IfDefPact
  { _ifdpName :: Text
  , _ifdpArgs :: [Arg ty]
  , _ifdpRType :: Maybe ty
  , _ifdpInfo :: info
  } deriving (Show, Eq, Functor, Generic)

data IfDefun ty info
  = IfDefun
  { _ifdName :: Text
  , _ifdArgs :: [Arg ty]
  , _ifdRType :: Maybe ty
  , _ifdInfo :: info
  } deriving (Show, Eq, Functor, Generic)

data IfDefCap name ty info
  = IfDefCap
  { _ifdcName :: Text
  , _ifdcArgs :: [Arg ty]
  , _ifdcRType :: Maybe ty
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
defName (Dfun d) = _dfunName d
defName (DConst d) = _dcName d
defName (DCap d) = _dcapName d
defName (DSchema d) = _dsName d
defName (DTable d) = _dtName d
defName (DPact d) = _dpName d

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
  IfDSchema ds -> Just $ DKDefSchema $ (Schema (QualifiedName (_dsName ds) mn) (_dsSchema ds))

ifDefName :: IfDef name ty builtin i -> Text
ifDefName = \case
  IfDfun ifd -> _ifdName ifd
  IfDConst dc -> _dcName dc
  IfDCap ifd -> _ifdcName ifd
  IfDPact ifd -> _ifdpName ifd
  IfDSchema dc -> _dsName dc

defInfo :: Def name ty b i -> i
defInfo = \case
  Dfun de -> _dfunInfo de
  DConst dc -> _dcInfo dc
  DCap dc -> _dcapInfo dc
  DSchema dc -> _dsInfo dc
  DTable dt -> _dtInfo dt
  DPact dp -> _dpInfo dp

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


instance (Pretty name, Pretty builtin, Pretty ty) => Pretty (Term name ty builtin info) where
  pretty = \case
    Var name _ -> pretty name
    Lam ne te _ ->
      parens ("lambda" <+> parens (fold (NE.intersperse ":" (prettyLamArg <$> ne))) <+> pretty te)
    Let n te te' _ ->
      parens $ "let" <+> parens (pretty n <+> pretty te) <+> pretty te'
    App te ne _ ->
      parens (pretty te <+> hsep (pretty <$> ne))
    Sequence te te' _ ->
      parens ("seq" <+> pretty te <+> pretty te')
    Conditional o _ ->
      pretty o
    Builtin builtin _ -> pretty builtin
    Constant lit _ ->
      pretty lit
    Nullary term _ ->
      parens ("suspend" <+> pretty term)
    ListLit tes _ ->
      pretty tes
    CapabilityForm cf _ ->
      pretty cf
    Try te te' _ ->
      parens ("try" <+> pretty te <+> pretty te')
    ObjectLit n _ ->
      braces (hsep $ punctuate "," $ fmap (\(f, t) -> pretty f <> ":" <> pretty t) n)
    where
    prettyTyAnn = maybe mempty ((":" <>) . pretty)
    prettyLamArg (Arg n ty) =
      pretty n <> prettyTyAnn ty

instance (Pretty name, Pretty builtin, Pretty ty) => Pretty (TopLevel name ty builtin info) where
  pretty = \case
    TLTerm tm -> pretty tm
    _ -> "todo: pretty defs/modules"

prettyDef :: Pretty ty => Doc ann -> Text -> Maybe ty -> [Arg ty] -> Doc ann
prettyDef deftoken defname defRTy defArgs =
  let dfNameArg = Arg defname defRTy
      argList = parens (hsep (pretty <$> defArgs))
  in parens $ deftoken <+> pretty dfNameArg <+> argList

instance Pretty ty => Pretty (Defun name ty b i) where
  pretty (Defun name args rty _ _) =
    prettyDef "defun" name rty args

instance Pretty ty => Pretty (DefPact name ty b i) where
  pretty (DefPact name args rty _ _) =
    prettyDef "defpact" name rty args

instance Pretty ty => Pretty (DefCap name ty b i) where
  pretty (DefCap name args rty _ _ _) =
    prettyDef "defcap" name rty args

instance Pretty ty => Pretty (DefSchema ty info) where
  pretty (DefSchema n schema _) =
    let argList = [pretty arg | (Field k, t) <- M.toList schema, let arg = Arg k (Just t)]
    in parens $ "defschema" <+> pretty n <> (if null argList then mempty else " " <> hsep argList)

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

instance (Pretty name, Pretty ty, Pretty b) => Pretty (DefConst name ty b i) where
  pretty (DefConst n ty term _) =
    parens $ "defconst" <+> pretty n <> maybe mempty ((":" <>) . pretty) ty <+> pretty term

instance (Pretty name, Pretty ty, Pretty b) => Pretty (Def name ty b i) where
  pretty = \case
    Dfun d -> pretty d
    DConst d -> pretty d
    DCap d -> pretty d
    DSchema d -> pretty d
    DTable d -> pretty d
    DPact d -> pretty d


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
  Conditional bf i ->
    Conditional <$> traverse (termType f) bf <*> pure i
  Builtin b i -> pure (Builtin b i)
  Nullary term i ->
    Nullary <$> termType f term <*> pure i
  Constant lit i ->
    pure (Constant lit i)
  ListLit tes i ->
    ListLit <$> traverse (termType f) tes <*> pure i
  Try te te' i ->
    Try <$> termType f te <*> termType f te' <*> pure i
  CapabilityForm cf i ->
    CapabilityForm <$> traverse (termType f) cf <*> pure i
  ObjectLit m i ->
    ObjectLit <$> (traverse._2) (termType f) m <*> pure i

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
  Conditional bf i ->
    Conditional <$> traverse (termBuiltin f) bf <*> pure i
  Builtin b i ->
    Builtin <$> f b <*> pure i
  Nullary term i ->
    Nullary <$> termBuiltin f term <*> pure i
  Constant lit i ->
    pure (Constant lit i)
  ListLit tes i ->
    ListLit <$> traverse (termBuiltin f) tes <*> pure i
  Try te te' i ->
    Try <$> termBuiltin f te <*> termBuiltin f te' <*> pure i
  CapabilityForm cf i ->
    CapabilityForm <$> traverse (termBuiltin f) cf <*> pure i
  ObjectLit m i ->
    ObjectLit <$> (traverse._2) (termBuiltin f) m <*> pure i

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
  Conditional o i ->
    Conditional o <$> f i
  ListLit l i  -> ListLit l <$> f i
  Try e1 e2 i -> Try e1 e2 <$> f i
  Nullary term i ->
    Nullary term <$> f i
  CapabilityForm cf i -> CapabilityForm cf <$> f i
  ObjectLit m i -> ObjectLit m <$> f i

traverseDefunTerm
  :: Traversal (Defun name ty builtin info)
               (Defun name' ty builtin' info)
               (Term name ty builtin info)
               (Term name' ty builtin' info)
traverseDefunTerm f (Defun n args ret term i) =
  (\term' -> Defun n args ret term' i) <$> f term

traverseDefConstTerm
  :: Traversal (DefConst name ty builtin info)
               (DefConst name' ty builtin' info)
               (Term name ty builtin info)
               (Term name' ty builtin' info)
traverseDefConstTerm f (DefConst n ret term i) =
  (\term' -> DefConst n ret term' i)  <$> traverse f term

traverseDefCapTerm
  :: Traversal (DefCap name ty builtin info)
               (DefCap name ty builtin' info)
               (Term name ty builtin info)
               (Term name ty builtin' info)
traverseDefCapTerm f (DefCap n args ret term meta i) =
  (\term' -> DefCap n args ret term' meta i) <$> f term


traverseDefPactStep
  :: Traversal (Step name ty builtin info)
               (Step name ty builtin' info)
               (Term name ty builtin info)
               (Term name ty builtin' info)
traverseDefPactStep f = \case
  Step t -> Step <$> f t
  StepWithRollback a1 a2 ->
    StepWithRollback <$> f a1 <*> f a2

traverseDefPactTerm
  :: Traversal (DefPact name ty builtin info)
               (DefPact name ty builtin' info)
               (Term name ty builtin info)
               (Term name ty builtin' info)
traverseDefPactTerm f (DefPact n args ty steps info) =
  (\steps' -> DefPact n args ty steps' info) <$> traverse (traverseDefPactStep f) steps


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


instance Plated (Term name ty builtin info) where
  plate f = \case
    Var n i -> pure (Var n i)
    Lam ns term i -> Lam ns <$> f term <*> pure i
    Let n t1 t2 i -> Let n <$> f t1 <*> f t2 <*> pure i
    App t1 t2 i -> App <$> f t1 <*> traverse f t2 <*> pure i
    Builtin b i -> pure (Builtin b i)
    Constant l i -> pure (Constant l i)
    Sequence e1 e2 i -> Sequence <$> f e1 <*> f e2 <*> pure i
    Conditional o i ->
      Conditional <$> traverse (plate f) o <*> pure i
    ListLit m i -> ListLit <$> traverse f m <*> pure i
    Nullary term i ->
      Nullary <$> f term <*> pure i
    CapabilityForm cf i ->
      CapabilityForm <$> traverse f cf <*> pure i
    Try e1 e2 i ->
      Try <$> f e1 <*> f e2 <*> pure i
    ObjectLit o i ->
      ObjectLit <$> (traverse._2) f o <*> pure i

makeLenses ''Module
makeLenses ''Interface
makeLenses ''Defun
makeLenses ''DefConst
makeLenses ''DefCap
makeLenses ''DefPact
makePrisms ''Def
makePrisms ''Term
makePrisms ''IfDef

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

type EvalTable i = DefTable Name i
type EvalSchema i = DefSchema Type i

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
