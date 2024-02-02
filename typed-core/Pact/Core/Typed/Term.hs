{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}

module Pact.Core.Typed.Term
 ( Defun(..)
 , DefConst(..)
 , Def(..)
 , DefCap(..)
 , Term(..)
 , Module(..)
 , Interface(..)
 , IfDefun(..)
 , IfDefCap(..)
 , IfDef(..)
 , TopLevel(..)
 , ReplTopLevel(..)
 , Literal(..)
 , termInfo
 , termBuiltin
 -- Post-overload
 , OverloadedTerm
 , OverloadedDefun
 , OverloadedDefConst
 , OverloadedDefCap
 , OverloadedIfDef
 , OverloadedDef
 , OverloadedModule
 , OverloadedTopLevel
 , OverloadedReplTopLevel
 , OverloadedInterface
 -- On-chain eval terms
 , CoreEvalTerm
 , CoreEvalDefun
 , CoreEvalDefConst
 , CoreEvalDef
 , CoreEvalModule
 , CoreEvalTopLevel
 , CoreEvalReplTopLevel
 , defName
--  , defType
--  , defTerm
 -- Prisms and lenses
 , _IfDfun
 , _IfDConst
 , hasRollback
 , ordinaryDefPactStepExec
 , Apply(..)
 ) where

import Control.Lens
import Control.DeepSeq
import Data.Text(Text)
import Data.List.NonEmpty(NonEmpty)
import Data.Map.Strict(Map)
import Data.Void
import qualified Data.Set as Set
import qualified Data.List.NonEmpty as NE
import GHC.Generics


import Pact.Core.Builtin
import Pact.Core.Literal
import Pact.Core.Names
import Pact.Core.Imports
import Pact.Core.Hash
import Pact.Core.Guards
import Pact.Core.Capabilities
import Pact.Core.PactValue(PactValue)
import Pact.Core.Pretty(Pretty(..), pretty, (<+>))

import Pact.Core.Typed.Type

import qualified Pact.Core.Pretty as Pretty

-- data TypeApp tyname
--   = TyAppType (Type tyname)
--   | RowAppType (RowCtor )

-- | Typed pact core terms
data Term name tyname builtin info
  = Var name info
  -- ^ single variables, e.g the term `x`
  | Lam [Arg tyname] (Term name tyname builtin info) info
  -- ^ f = \a b c -> e
  -- All lambdas, even anonymous ones, are named, for the sake of them adding a stack frame
  | App (Term name tyname builtin info) [Term name tyname builtin info] info
  -- let n = e1 in e2
  | Let (Arg tyname) (Term name tyname builtin info) (Term name tyname builtin info) info
  -- ^ (e_1 e_2 .. e_n)
  | Builtin builtin info
  -- ^ Built-in functions (or natives)
  | Constant Literal info
  -- ^ Constant/Literal values
  | TyApp (Term name tyname builtin info) (NonEmpty (Type tyname)) info
  -- ^ (e_1 @t)
  | TyAbs (NonEmpty tyname) (Term name tyname builtin info) info
  -- /\a. e where a is a type variable
  | Sequence (Term name tyname builtin info) (Term name tyname builtin info) info
  -- ^ Blocks (to be replaced by Seq)
  | Conditional (BuiltinForm (Term name tyname builtin info)) info
  -- ^ Conditional exprs
  | ListLit (Type tyname) [Term name tyname builtin info] info
  -- ^ List literals
  | ObjectLit [(Field, Term name tyname builtin info)] info
  -- ^ an object literal
  | Try (Term name tyname builtin info) (Term name tyname builtin info) info
  -- ^ Error handling
  | CapabilityForm (CapForm name (Term name tyname builtin info)) info
  -- ^ Capabilities
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic)

data Apply term i =
  Apply term [term] i
  deriving (Show, Functor, Foldable, Traversable)

-- | Our defun representation, that is
-- (defun <name>(:<ty>)? (<args>*) <body>))
-- note our IR does not spit out docs.
-- In that case: refer to the repl.
data Defun name tyname builtin info
  = Defun
  { _dfunName :: Text
  , _dfunArgs :: [Arg tyname]
  , _dfunRType :: Type tyname
  , _dfunTerm :: Term name tyname builtin info
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
  , _dpRetType :: Type ty
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
  , _dcTerm :: PactValue
  , _dcInfo :: info
  } deriving (Show, Functor, Eq, Generic)

-- | Our defcap representation, that is
-- (defcap <name>:<ty> (<args>) <meta> <body>)
data DefCap name ty builtin info
  = DefCap
  { _dcapName :: Text
  , _dcapArgs :: [Arg ty]
  , _dcapRType :: Type ty
  , _dcapTerm :: Term name ty builtin info
  , _dcapMeta :: DefCapMeta FullyQualifiedName
  , _dcapInfo :: info
  } deriving (Show, Functor, Eq, Generic)

data DefSchema ty info
  = DefSchema
  { _dsName :: Text
  , _dsSchema :: Map Field (Type Void)
  , _dsInfo :: info
  } deriving (Show, Functor, Eq, Generic)

data DefTable info
  = DefTable
  { _dtName :: Text
  , _dtSchema :: Map Field (Type Void)
  , _dtInfo :: info
  } deriving (Show, Functor, Eq, Generic)

hasRollback :: Step n t b i -> Bool
hasRollback Step{} = False
hasRollback StepWithRollback{} = True

ordinaryDefPactStepExec :: Step name ty builtin info -> Term name ty builtin info
ordinaryDefPactStepExec (Step expr) = expr
ordinaryDefPactStepExec (StepWithRollback expr _) = expr

data Def name ty builtin info
  = Dfun (Defun name ty builtin info)
  | DConst (DefConst name ty builtin info)
  | DCap (DefCap name ty builtin info)
  | DSchema (DefSchema ty info)
  | DTable (DefTable info)
  | DPact (DefPact name ty builtin info)
  deriving (Show, Functor, Eq, Generic)

-- Todo: deftypes to support
-- DCap (DefCap name builtin info)
-- DPact (DefPact name builtin info)
-- DSchema (DefSchema name info)
-- DTable (DefTable name info)
-- defType :: Def name tyname builtin info -> TypeOfDef Void
-- defType = \case
--   Dfun d -> DefunType (_dfunType d)
--   DConst d -> DefunType (_dcType d)
--   DCap d -> DefcapType (_dcapArgTypes d) (_dcapRType d)

defName :: Def name tyname builtin i -> Text
defName = \case
  Dfun d -> _dfunName d
  DConst d -> _dcName d
  DCap d -> _dcapName d
  DTable t -> _dtName t
  DSchema s -> _dsName s
  DPact d -> _dpName d

data Module name tyname builtin info
  = Module
  { _mName :: ModuleName
  , _mGovernance :: Governance name
  , _mDefs :: [Def name tyname builtin info]
  , _mBlessed :: !(Set.Set ModuleHash)
  , _mImports :: [Import]
  , _mImplemented :: [ModuleName]
  , _mHash :: ModuleHash
  , _mInfo :: info
  } deriving (Show, Generic)

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

data TopLevel name tyname builtin info
  = TLModule (Module name tyname builtin info)
  | TLInterface (Interface name tyname builtin info)
  | TLTerm (Term name tyname builtin info)
  | TLUse Import info
  deriving (Show, Generic)

data ReplTopLevel name tyname builtin info
  = RTLModule (Module name tyname builtin info)
  | RTLInterface (Interface name tyname builtin info)
  | RTLDefun (Defun name tyname builtin info)
  | RTLDefConst (DefConst name tyname builtin info)
  | RTLTerm (Term name tyname builtin info)
  deriving Show

-- Post Typecheck terms + modules
type OverloadedTerm tyname b i =
  Term Name tyname (b, [Type tyname], [Pred tyname]) i

type OverloadedDefun tyname b i =
  Defun Name tyname (b, [Type tyname], [Pred tyname]) i

type OverloadedDefConst tyname b i =
  DefConst Name tyname (b, [Type tyname], [Pred tyname]) i

type OverloadedDefCap tyname b i =
  DefCap Name tyname (b, [Type tyname], [Pred tyname]) i

type OverloadedDef tyname b i =
  Def Name tyname (b, [Type tyname], [Pred tyname]) i

type OverloadedIfDef tyname b i =
  IfDef Name tyname (b, [Type tyname], [Pred tyname]) i

type OverloadedModule tyname b i =
  Module Name tyname (b, [Type tyname], [Pred tyname]) i

type OverloadedInterface tyname b i =
  Interface Name tyname (b, [Type tyname], [Pred tyname]) i

type OverloadedTopLevel tyname b i =
  TopLevel Name tyname (b, [Type tyname], [Pred tyname]) i

type OverloadedReplTopLevel tyname b i =
  ReplTopLevel Name tyname (b, [Type tyname], [Pred tyname]) i

-- type OverloadedDefCap b i =
--   DefCap Name (b, [Type Void], [Pred Void]) i

-- On-chain, core builtin-types
type CoreEvalTerm tyname  i = Term Name tyname CoreBuiltin i
type CoreEvalDefun tyname i = Defun Name tyname CoreBuiltin i
type CoreEvalDefConst tyname i = DefConst Name tyname CoreBuiltin i
type CoreEvalDef tyname i = Def Name tyname CoreBuiltin i
type CoreEvalModule tyname i = Module Name tyname CoreBuiltin i
type CoreEvalTopLevel tyname i = TopLevel Name tyname CoreBuiltin i
type CoreEvalReplTopLevel tyname i = ReplTopLevel Name tyname CoreBuiltin i


instance (Pretty n, Pretty tn, Pretty b) => Pretty (Term n tn b i) where
  pretty = \case
    Var n _ -> pretty n
    Lam ns body _ ->
      let lamArgs = Pretty.parens $ Pretty.hsep (pretty <$> ns)
      in "λ" <> lamArgs <> "." <+> pretty body
    App l nel _ ->
      Pretty.parens (pretty l <+> Pretty.hsep (pretty <$> nel))
    Let n e1 e2 _ ->
      "let" <+> pretty n <+> "=" <+> pretty e1 <+> prettyFollowing e2
      where
      prettyFollowing e@Let{} = Pretty.hardline <> pretty e
      prettyFollowing e = Pretty.hardline <> "in" <+> pretty e
    TyApp t (NE.toList -> apps) _ ->
      pretty t <+> Pretty.hsep (fmap prettyTyApp apps)
    TyAbs (NE.toList -> ns) term _ ->
      "/\\" <> Pretty.hsep (pretty <$> ns) <> "." <+> pretty term
    Sequence e1 e2 _ ->
      Pretty.parens ("seq" <+> pretty e1 <+> pretty e2)
    Conditional c _ -> pretty c
    ListLit ty li _ ->
      Pretty.brackets (Pretty.hsep $ Pretty.punctuate Pretty.comma $ (pretty <$> li)) <> if null li then prettyTyApp ty else mempty
    Builtin b _ -> pretty b
    Constant l _ -> pretty l
    Try e1 e2 _ ->
      Pretty.parens ("try" <+> pretty e1 <+> pretty e2)
    CapabilityForm cf _ -> pretty cf
    ObjectLit n _ ->
      Pretty.braces (Pretty.hsep $ Pretty.punctuate "," $ fmap (\(f, t) -> pretty f <> ":" <> pretty t) n)
    where
    prettyTyApp ty = "@(" <> pretty ty <> ")"

termBuiltin
  :: Traversal (Term name tyname builtin info)
               (Term name tyname builtin' info)
               builtin
               builtin'
termBuiltin f = \case
  Var name info -> pure (Var name info)
  Lam ne te info ->
    Lam ne <$> termBuiltin f te <*> pure info
  App te ne info ->
    App <$> termBuiltin f te <*> traverse (termBuiltin f) ne <*> pure info
  Let name e1 e2 info ->
    Let name <$> termBuiltin f e1 <*> termBuiltin f e2 <*> pure info
  Builtin builtin info ->
    Builtin <$> f builtin <*> pure info
  Constant lit info ->
    pure (Constant lit info)
  TyApp te ne info ->
    TyApp <$> termBuiltin f te <*> pure ne <*> pure info
  TyAbs ne te info ->
    TyAbs ne <$> termBuiltin f te <*> pure info
  Sequence te te' info ->
    Sequence <$> termBuiltin f te <*> termBuiltin f te' <*> pure info
  Conditional o info ->
    Conditional <$> traverse (termBuiltin f) o <*> pure info
  ListLit ty tes info ->
    ListLit ty <$> traverse (termBuiltin f) tes <*> pure info
  Try te te' info ->
    Try <$> termBuiltin f te <*> termBuiltin f te' <*> pure info
  CapabilityForm cf i ->
    CapabilityForm <$> traverse (termBuiltin f) cf <*> pure i
  ObjectLit m i ->
    ObjectLit <$> (traverse._2) (termBuiltin f) m <*> pure i

termInfo :: Lens' (Term name tyname builtin info) info
termInfo f = \case
  Var n i ->
    Var n <$> f i
  Lam ns term i ->
    Lam ns term <$> f i
  App t1 t2 i ->
    App t1 t2 <$> f i
  Let n e1 e2 i ->
    Let n e1 e2 <$> f i
  TyApp term ty i ->
    TyApp term ty <$> f i
  TyAbs ns e i ->
    TyAbs ns e <$> f i
  Sequence e1 e2 i ->
    Sequence e1 e2 <$> f i
  Conditional o info ->
    Conditional o <$> f info
  ListLit ty v i ->
    ListLit ty v <$> f i
  ObjectLit m i ->
    ObjectLit m <$> f i
  Builtin b i ->
    Builtin b <$> f i
  Constant l i ->
    Constant l <$> f i
  Try e1 e2 i ->
    Try e1 e2 <$> f i
  CapabilityForm cf i ->
    CapabilityForm cf <$> f i

instance Plated (Term name tyname builtin info) where
  plate f = \case
    Var n i -> pure (Var n i)
    Lam ns term i -> Lam ns <$> f term <*> pure i
    App t1 t2 i -> App <$> f t1 <*> traverse f t2 <*> pure i
    Let n e1 e2 i ->
      Let n <$> f e1 <*> f e2 <*> pure i
    TyApp term ty i -> TyApp <$> f term <*> pure ty <*> pure i
    TyAbs ns term i ->
      TyAbs ns <$> f term <*> pure i
    ListLit ty ts i ->
      ListLit ty <$> traverse f ts <*> pure i
    ObjectLit ts i ->
      ObjectLit <$> (traverse._2) f ts <*> pure i
    Sequence e1 e2 i ->
      Sequence <$> f e1 <*> f e2 <*> pure i
    Conditional o i ->
      Conditional <$> traverse (plate f) o <*> pure i
    Builtin b i -> pure (Builtin b i)
    Constant l i -> pure (Constant l i)
    Try e1 e2 i ->
      Try <$> f e1 <*> f e2 <*> pure i
    CapabilityForm cf i ->
      CapabilityForm <$> traverse f cf <*> pure i

makePrisms ''IfDef
makePrisms ''Def

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
instance (NFData info) => NFData (DefTable info)
