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
 , DefTable(..)
 , DefSchema(..)
 , DefPact(..)
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
 , TypeApp(..)
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

data TypeApp tyname
  = TyAppType (Type tyname)
  | TyAppVar tyname
  deriving (Show, Eq, Generic)

-- | Typed pact core terms
data Term name tyname builtin info
  = Var name info
  -- ^ single variables, e.g the term `x`
  | Lam [TypedArg (Type tyname) info] (Term name tyname builtin info) info
  -- ^ f = \a b c -> e
  -- All lambdas, even anonymous ones, are named, for the sake of them adding a stack frame
  | App (Term name tyname builtin info) [Term name tyname builtin info] info
  -- let n = e1 in e2
  | Let (TypedArg (Type tyname) info) (Term name tyname builtin info) (Term name tyname builtin info) info
  -- ^ (e_1 e_2 .. e_n)
  | Builtin builtin info
  -- ^ Built-in functions (or natives)
  | Constant Literal info
  -- ^ Constant/Literal values
  | TyApp (Term name tyname builtin info) (NonEmpty (TypeApp tyname)) info
  -- ^ (e_1 @t)
  | DictApp (Term name tyname builtin info) (NonEmpty (BuiltinTC tyname)) info
  -- ^ (e_1 dict_var)
  | TyAbs (NonEmpty tyname) [BuiltinTC tyname] (Term name tyname builtin info) info
  -- /\a. e where a is a type variable
  | Sequence (Term name tyname builtin info) (Term name tyname builtin info) info
  -- ^ Blocks (to be replaced by Seq)
  | BuiltinForm (BuiltinForm (Term name tyname builtin info)) info
  -- ^ BuiltinForm exprs
  | ListLit (Type tyname) [Term name tyname builtin info] info
  -- ^ List literals
  | ObjectLit [(Field, Term name tyname builtin info)] info
  -- ^ an object literal
  | Try (Term name tyname builtin info) (Term name tyname builtin info) info
  -- ^ Error handling
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
  , _dfunArgs :: [Arg Void info]
  , _dfunType :: TypeScheme tyname
  , _dfunTerm :: Term name tyname builtin info
  , _dfunInfo :: info
  } deriving (Show, Functor, Eq, Generic)

data Step name ty builtin info
  = Step (Term name ty builtin info)
  | StepWithRollback
    (Term name ty builtin info)
    (Term name ty builtin info)
  deriving (Show, Functor, Eq, Generic)

data DefPact name tyname builtin info
  = DefPact
  { _dpName :: Text
  , _dpArgs :: [Arg Void info]
  , _dpType :: TypeScheme tyname
  , _dpSteps :: NonEmpty (Step name tyname builtin info)
  , _dpInfo :: info
  } deriving (Show, Functor, Eq, Generic)

-- | Our defconst representation, that is
-- (defconst <name>(:<ty>)* <expr>)
-- Todo: ConstVal is not precisely type-safe.
-- Maybe a different IR is needed here?
data DefConst ty info
  = DefConst
  { _dcName :: Text
  , _dcType :: Type ty
  , _dcTerm :: PactValue
  , _dcInfo :: info
  } deriving (Show, Functor, Eq, Generic)

-- | Our defcap representation, that is
-- (defcap <name>:<ty> (<args>) <meta> <body>)
data DefCap name tyname builtin info
  = DefCap
  { _dcapName :: Text
  , _dcapArgs :: [Arg Void info]
  , _dcapType :: TypeScheme tyname
  , _dcapTerm :: Term name tyname builtin info
  , _dcapMeta :: DefCapMeta (FQNameRef Name)
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
  , _dtSchema :: Schema
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
  | DConst (DefConst ty info)
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

data Interface ty info
  = Interface
  { _ifName :: ModuleName
  , _ifDefns :: [IfDef ty info]
  , _ifImports :: [Import]
  , _ifHash :: ModuleHash
  , _ifInfo :: info
  } deriving (Show, Eq, Functor, Generic)

data IfDefPact ty info
  = IfDefPact
  { _ifdpName :: Text
  , _ifdpArgs :: [Arg ty info]
  , _ifdpRType :: Type ty
  , _ifdpInfo :: info
  } deriving (Show, Eq, Functor, Generic)

data IfDefun ty info
  = IfDefun
  { _ifdName :: Text
  , _ifdArgs :: [Arg ty info]
  , _ifdRType :: Type ty
  , _ifdInfo :: info
  } deriving (Show, Eq, Functor, Generic)

data IfDefCap ty info
  = IfDefCap
  { _ifdcName :: Text
  , _ifdcArgs :: [Arg ty info]
  , _ifdcRType :: Type ty
  , _ifdcMeta :: DefCapMeta BareName
  , _ifdcInfo :: info
  } deriving (Show, Eq, Functor, Generic)

data IfDef ty info
  = IfDfun (IfDefun ty info)
  | IfDConst (DefConst ty info)
  | IfDCap (IfDefCap ty info)
  | IfDPact (IfDefPact ty info)
  | IfDSchema (DefSchema ty info)
  deriving (Show, Eq, Functor, Generic)

data TopLevel name tyname builtin info
  = TLModule (Module name tyname builtin info)
  | TLInterface (Interface tyname info)
  | TLTerm (Term name tyname builtin info)
  | TLUse Import info
  deriving (Show, Generic)

data ReplTopLevel name ty builtin info
  = RTLDefConst (DefConst ty info)
  | RTLDefun (Defun name ty builtin info)
  deriving (Show, Functor)


-- type OverloadedDefCap b i =
--   DefCap Name (b, [Type Void], [Pred Void]) i

-- On-chain, core builtin-types
type CoreEvalTerm tyname  i = Term Name tyname CoreBuiltin i
type CoreEvalDefun tyname i = Defun Name tyname CoreBuiltin i
type CoreEvalDefConst tyname i = DefConst tyname i
type CoreEvalDef tyname i = Def Name tyname CoreBuiltin i
type CoreEvalModule tyname i = Module Name tyname CoreBuiltin i
type CoreEvalTopLevel tyname i = TopLevel Name tyname CoreBuiltin i
type CoreEvalReplTopLevel tyname i = ReplTopLevel Name tyname CoreBuiltin i

instance Pretty tyname => Pretty (TypeApp tyname) where
  pretty = \case
    TyAppType t ->
      "@" <> Pretty.parens (pretty t)
    TyAppVar tn ->
      "@" <> Pretty.parens (pretty tn)

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
      pretty t <+> Pretty.hsep (pretty <$> apps)
    -- Todo: pretty dicts as part of term abs
    TyAbs (NE.toList -> ns) _dicts term _ ->
      "/\\" <> Pretty.hsep (pretty <$> ns) <> "." <+> pretty term
    -- todo: instance is pass-through for now
    DictApp term _dicts _ ->
      pretty term
    Sequence e1 e2 _ ->
      Pretty.parens ("seq" <+> pretty e1 <+> pretty e2)
    BuiltinForm c _ -> pretty c
    ListLit ty li _ ->
      Pretty.brackets (Pretty.hsep $ Pretty.punctuate Pretty.comma $ (pretty <$> li)) <> if null li then pretty (TyAppType ty) else mempty
    Builtin b _ -> pretty b
    Constant l _ -> pretty l
    Try e1 e2 _ ->
      Pretty.parens ("try" <+> pretty e1 <+> pretty e2)
    ObjectLit n _ ->
      Pretty.braces (Pretty.hsep $ Pretty.punctuate "," $ fmap (\(f, t) -> pretty f <> ":" <> pretty t) n)

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
  DictApp term dicts info ->
    DictApp <$> termBuiltin f term <*> pure dicts <*> pure info
  TyAbs ne btc te info ->
    TyAbs ne btc <$> termBuiltin f te <*> pure info
  Sequence te te' info ->
    Sequence <$> termBuiltin f te <*> termBuiltin f te' <*> pure info
  BuiltinForm o info ->
    BuiltinForm <$> traverse (termBuiltin f) o <*> pure info
  ListLit ty tes info ->
    ListLit ty <$> traverse (termBuiltin f) tes <*> pure info
  Try te te' info ->
    Try <$> termBuiltin f te <*> termBuiltin f te' <*> pure info
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
  TyAbs ns btc e i ->
    TyAbs ns btc e <$> f i
  DictApp t a i ->
    DictApp t a <$> f i
  Sequence e1 e2 i ->
    Sequence e1 e2 <$> f i
  BuiltinForm o info ->
    BuiltinForm o <$> f info
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

instance Plated (Term name tyname builtin info) where
  plate f = \case
    Var n i -> pure (Var n i)
    Lam ns term i -> Lam ns <$> f term <*> pure i
    App t1 t2 i -> App <$> f t1 <*> traverse f t2 <*> pure i
    Let n e1 e2 i ->
      Let n <$> f e1 <*> f e2 <*> pure i
    TyApp term ty i -> TyApp <$> f term <*> pure ty <*> pure i
    TyAbs ns btc term i ->
      TyAbs ns btc <$> f term <*> pure i
    DictApp term dicts i ->
      DictApp <$> f term <*> pure dicts <*> pure i
    ListLit ty ts i ->
      ListLit ty <$> traverse f ts <*> pure i
    ObjectLit ts i ->
      ObjectLit <$> (traverse._2) f ts <*> pure i
    Sequence e1 e2 i ->
      Sequence <$> f e1 <*> f e2 <*> pure i
    BuiltinForm o i ->
      BuiltinForm <$> traverse f o <*> pure i
    Builtin b i -> pure (Builtin b i)
    Constant l i -> pure (Constant l i)
    Try e1 e2 i ->
      Try <$> f e1 <*> f e2 <*> pure i

makePrisms ''IfDef
makePrisms ''Def

instance (NFData name) => NFData (TypeApp name)
instance (NFData name, NFData ty, NFData b, NFData info) => NFData (Term name ty b info)
instance (NFData name, NFData ty, NFData b, NFData info) => NFData (Def name ty b info)
instance (NFData name, NFData info) => NFData (DefSchema name info)
instance (NFData name, NFData ty, NFData b, NFData info) => NFData (Defun name ty b info)
instance (NFData ty, NFData info) => NFData (DefConst ty info)
instance (NFData name, NFData ty, NFData b, NFData info) => NFData (DefCap name ty b info)
instance (NFData name, NFData ty, NFData b, NFData info) => NFData (DefPact name ty b info)
instance (NFData name, NFData ty, NFData b, NFData info) => NFData (Step name ty b info)
instance (NFData name, NFData ty, NFData b, NFData info) => NFData (Module name ty b info)
instance (NFData ty, NFData info) => NFData (Interface ty info)
instance (NFData ty, NFData info) => NFData (IfDef ty info)
instance (NFData ty, NFData info) => NFData (IfDefun ty info)
instance (NFData ty, NFData info) => NFData (IfDefPact ty info)
instance (NFData ty, NFData info) => NFData (IfDefCap ty info)
instance (NFData info) => NFData (DefTable info)
