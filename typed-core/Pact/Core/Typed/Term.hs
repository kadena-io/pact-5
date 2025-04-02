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
 , defType
 , defTerm
 -- Prisms and lenses
 , _IfDfun
 , _IfDConst
 ) where

import Control.Lens
import Data.Text(Text)
import Data.List.NonEmpty(NonEmpty)
import Data.Void
import qualified Data.Set as Set
import qualified Data.List.NonEmpty as NE

import Pact.Core.Builtin
import Pact.Core.Literal
import Pact.Core.Names
import Pact.Core.Type
import Pact.Core.Imports
import Pact.Core.Hash
import Pact.Core.Guards
import Pact.Core.Capabilities
import Pact.Core.Pretty(Pretty(..), pretty, (<+>))

import qualified Pact.Core.Pretty as Pretty

data Defun name tyname builtin info
  = Defun
  { _dfunName :: Text
  , _dfunType :: Type Void
  , _dfunTerm :: Term name tyname builtin info
  , _dfunInfo :: info
  } deriving Show

data DefConst name tyname builtin info
  = DefConst
  { _dcName :: Text
  , _dcType :: Type Void
  , _dcTerm :: Term name tyname builtin info
  , _dcInfo :: info
  } deriving Show

data DefCap name tyname builtin info
  = DefCap
  { _dcapName :: Text
  , _dcapAppArity :: Int
  , _dcapArgTypes :: [Type Void]
  , _dcapRType :: Type Void
  , _dcapTerm :: Term name tyname builtin info
  , _dcapMeta :: Maybe (DefCapMeta name)
  , _dcapInfo :: info
  } deriving Show


data Def name tyname builtin info
  = Dfun (Defun name tyname builtin info)
  | DConst (DefConst name tyname builtin info)
  | DCap (DefCap name tyname builtin info)
  deriving Show

-- Todo: deftypes to support
-- DCap (DefCap name builtin info)
-- DPact (DefPact name builtin info)
-- DSchema (DefSchema name info)
-- DTable (DefTable name info)
defType :: Def name tyname builtin info -> TypeOfDef Void
defType = \case
  Dfun d -> DefunType (_dfunType d)
  DConst d -> DefunType (_dcType d)
  DCap d -> DefcapType (_dcapArgTypes d) (_dcapRType d)

defName :: Def name tyname builtin i -> Text
defName = \case
  Dfun d -> _dfunName d
  DConst d -> _dcName d
  DCap d -> _dcapName d

defTerm :: Def name tyname builtin info -> Term name tyname builtin info
defTerm = \case
  Dfun d -> _dfunTerm d
  DConst d -> _dcTerm d
  DCap d -> _dcapTerm d

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
  } deriving Show

data Interface name tyname builtin info
  = Interface
  { _ifName :: ModuleName
  , _ifDefns :: [IfDef name tyname builtin info]
  , _ifHash :: ModuleHash
  , _ifInfo :: info
  } deriving Show

data IfDefun info
  = IfDefun
  { _ifdName :: Text
  , _ifdType :: Type Void
  , _ifdInfo :: info
  } deriving Show

data IfDefCap info
  = IfDefCap
  { _ifdcName :: Text
  , _ifdcArgTys :: [Type Void]
  , _ifdcRType :: Type Void
  , _ifdcInfo :: info
  } deriving (Show, Functor)

data IfDef name tyname builtin info
  = IfDfun (IfDefun info)
  | IfDConst (DefConst name tyname builtin info)
  | IfDCap (IfDefCap info)
  deriving Show

data TopLevel name tyname builtin info
  = TLModule (Module name tyname builtin info)
  | TLInterface (Interface name tyname builtin info)
  | TLTerm (Term name tyname builtin info)
  deriving Show

data ReplTopLevel name tyname builtin info
  = RTLModule (Module name tyname builtin info)
  | RTLInterface (Interface name tyname builtin info)
  | RTLDefun (Defun name tyname builtin info)
  | RTLDefConst (DefConst name tyname builtin info)
  | RTLTerm (Term name tyname builtin info)
  deriving Show

-- | Typed pact core terms
data Term name tyname builtin info
  = Var name info
  -- ^ single variables, e.g the term `x`
  | Lam (NonEmpty (Text, Type tyname)) (Term name tyname builtin info) info
  -- ^ f = \a b c -> e
  -- All lambdas, even anonymous ones, are named, for the sake of them adding a stack frame
  | App (Term name tyname builtin info) (NonEmpty (Term name tyname builtin info)) info
  -- let n = e1 in e2
  | Let Text (Term name tyname builtin info) (Term name tyname builtin info) info
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
  | BuiltinForm (BuiltinForm (Term name tyname builtin info)) info
  -- ^ BuiltinForm exprs
  | ListLit (Type tyname) [Term name tyname builtin info] info
  -- ^ List literals
  | DynInvoke (Term name tyname builtin info) Text info
  -- ^ Dynamic invoke.
  -- | Try (Term name tyname builtin info) (Term name tyname builtin info) info
  -- ^ Error handling
  -- | CapabilityForm (CapForm name (Term name tyname builtin info)) info
  -- ^ Capabilities
  | Error (Type tyname) Text info
  -- ^ Error term
  deriving (Show, Functor, Foldable, Traversable)

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
    Lam (NE.toList -> ns) body _ ->
      "λ" <> Pretty.hsep (fmap (\(n, t) -> Pretty.parens (pretty n <> ":" <+> pretty t)) ns) <+> "->" <+> pretty body
    App l (NE.toList -> nel) _ ->
      pretty l <> Pretty.parens (Pretty.hsep (Pretty.punctuate Pretty.comma (pretty <$> nel)))
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
    BuiltinForm c _ -> pretty c
    ListLit ty li _ ->
      Pretty.brackets (Pretty.hsep $ Pretty.punctuate Pretty.comma $ (pretty <$> li)) <> if null li then prettyTyApp ty else mempty
    Builtin b _ -> pretty b
    Constant l _ -> pretty l
    Try e1 e2 _ ->
      Pretty.parens ("try" <+> pretty e1 <+> pretty e2)
    CapabilityForm cf _ -> pretty cf
    DynInvoke{} -> error "implement dyn invoke"
    Error _ e _ ->
      Pretty.parens ("error \"" <> pretty e <> "\"")
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
  BuiltinForm o info ->
    BuiltinForm <$> traverse (termBuiltin f) o <*> pure info
  ListLit ty tes info ->
    ListLit ty <$> traverse (termBuiltin f) tes <*> pure info
  Try te te' info ->
    Try <$> termBuiltin f te <*> termBuiltin f te' <*> pure info
  CapabilityForm cf i ->
    CapabilityForm <$> traverse (termBuiltin f) cf <*> pure i
  DynInvoke te t i ->
    DynInvoke <$> termBuiltin f te <*> pure t <*> pure i
  Error ty txt info ->
    pure (Error ty txt info)

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
  BuiltinForm o info ->
    BuiltinForm o <$> f info
  ListLit ty v i ->
    ListLit ty v <$> f i
  Builtin b i ->
    Builtin b <$> f i
  Constant l i ->
    Constant l <$> f i
  Try e1 e2 i ->
    Try e1 e2 <$> f i
  CapabilityForm cf i ->
    CapabilityForm cf <$> f i
  DynInvoke t e i -> DynInvoke t e <$> f i
  Error t e i ->
    Error t e <$> f i
  -- ObjectLit obj i -> ObjectLit obj <$> f i
  -- ObjectOp o i -> ObjectOp o <$> f i

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
    Sequence e1 e2 i ->
      Sequence <$> f e1 <*> f e2 <*> pure i
    BuiltinForm o i ->
      BuiltinForm <$> traverse (plate f) o <*> pure i
    Builtin b i -> pure (Builtin b i)
    Constant l i -> pure (Constant l i)
    Try e1 e2 i ->
      Try <$> f e1 <*> f e2 <*> pure i
    CapabilityForm cf i ->
      CapabilityForm <$> traverse f cf <*> pure i
    DynInvoke e1 t i ->
      DynInvoke <$> f e1 <*> pure t <*> pure i
    Error t e i -> pure (Error t e i)
    -- ObjectLit tm i ->
    --   ObjectLit <$> traverse f tm <*> pure i
    -- ObjectOp oop i ->
    --   ObjectOp <$> traverse f oop <*> pure i

makePrisms ''IfDef
makePrisms ''Def
