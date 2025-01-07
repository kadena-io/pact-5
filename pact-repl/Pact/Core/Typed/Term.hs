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
 , IfDefPact(..)
 , TopLevel(..)
 , ReplTopLevel(..)
 , Literal(..)
 , Step(..)
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
 -- Prisms and lenses
 , _IfDfun
 , _IfDConst
 , hasRollback
 , ordinaryDefPactStepExec
 , Apply(..)
 , TypeApp(..)
 , TypedObjectOp(..)
 , ZKGroup(..)
 ) where

import Control.Lens
import Control.DeepSeq
import Data.Text(Text)
import Data.List.NonEmpty(NonEmpty)
import Data.Map.Strict(Map)
import Data.Void
import Data.IntMap.Strict (IntMap)
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
import Pact.Core.Pretty
import Pact.Core.Typed.Type
import Pact.Core.Gas(ZKGroup(..))

import qualified Pact.Core.Pretty as Pretty
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM

data TypeApp tyname
  = TyAppType (Type tyname)
  | TyAppRow (RowTy tyname)
  | TyAppVar tyname
  | TyAppRef (MRef tyname)
  deriving (Show, Eq, Generic)

data TypedObjectOp
  = ObjConcat
  | ObjAccess Field
  | SortObj [Field]
  | SelectObj [Field]
  | ReadObj [Field]
  | ObjWhere Field
  | PointAdd ZKGroup
  | ScalarMult ZKGroup
  deriving (Eq, Show, Generic)

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
  | ObjectOp TypedObjectOp info
  | Format (Term name tyname builtin info) [Term name tyname builtin info] info
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
  , _dpArgs :: [TypedArg (Type Void) info]
  , _dpType :: IntMap (Type Void)
  , _dpSteps :: NonEmpty (Step name tyname builtin info)
  , _dpInfo :: info
  } deriving (Show, Functor, Eq, Generic)

-- | Our defconst representation, that is
-- (defconst <name>(:<ty>)* <expr>)
-- Todo: ConstVal is not precisely type-safe.
-- Maybe a different IR is needed here?
data DefConst info
  = DefConst
  { _dcName :: Text
  , _dcType :: Type Void
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

data DefSchema info
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
  | DConst (DefConst info)
  | DCap (DefCap name ty builtin info)
  | DSchema (DefSchema info)
  | DTable (DefTable info)
  | DPact (DefPact name ty builtin info)
  deriving (Show, Functor, Eq, Generic)

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

data Interface info
  = Interface
  { _ifName :: ModuleName
  , _ifDefns :: [IfDef info]
  , _ifImports :: [Import]
  , _ifHash :: ModuleHash
  , _ifTxHash :: Hash
  , _ifInfo :: info
  } deriving (Show, Eq, Functor, Generic)

data IfDefPact info
  = IfDefPact
  { _ifdpSpec :: TypedArg (Type Void) info
  , _ifdpArgs :: [TypedArg (Type Void) info]
  , _ifdpType :: Type Void
  , _ifdpInfo :: info
  } deriving (Show, Eq, Functor, Generic)

data IfDefun info
  = IfDefun
  { _ifdSpec :: TypedArg (Type Void) info
  , _ifdArgs :: [TypedArg (Type Void) info]
  , _ifdType :: Type Void
  , _ifdInfo :: info
  } deriving (Show, Eq, Functor, Generic)

data IfDefCap info
  = IfDefCap
  { _ifdcSpec :: TypedArg (Type Void) info
  , _ifdcArgs :: [TypedArg (Type Void) info]
  , _ifdcType :: Type Void
  , _ifdcMeta :: DefCapMeta BareName
  , _ifdcInfo :: info
  } deriving (Show, Eq, Functor, Generic)

data IfDef info
  = IfDfun (IfDefun info)
  | IfDConst (DefConst info)
  | IfDCap (IfDefCap info)
  | IfDPact (IfDefPact info)
  | IfDSchema (DefSchema info)
  deriving (Show, Eq, Functor, Generic)

data TopLevel name tyname builtin info
  = TLModule (Module name tyname builtin info)
  | TLInterface (Interface info)
  | TLTerm (Term name tyname builtin info)
  | TLUse Import info
  deriving (Show, Generic)

data ReplTopLevel name ty builtin info
  = RTLDefConst (DefConst info)
  | RTLDefun (Defun name ty builtin info)
  deriving (Show, Functor)

-- On-chain, core builtin-types
type CoreEvalTerm tyname  i = Term Name tyname CoreBuiltin i
type CoreEvalDefun tyname i = Defun Name tyname CoreBuiltin i
type CoreEvalDefConst tyname i = DefConst i
type CoreEvalDef tyname i = Def Name tyname CoreBuiltin i
type CoreEvalModule tyname i = Module Name tyname CoreBuiltin i
type CoreEvalTopLevel tyname i = TopLevel Name tyname CoreBuiltin i
type CoreEvalReplTopLevel tyname i = ReplTopLevel Name tyname CoreBuiltin i

instance Pretty TypedObjectOp where
  pretty = \case
    ObjConcat -> "+"
    ObjAccess f -> "at" <+> pretty f
    SortObj fs -> "sort" <+> Pretty.braces (Pretty.hsep (pretty <$> fs))
    ReadObj fs -> "read" <+> Pretty.braces (Pretty.hsep (pretty <$> fs))
    ObjWhere f -> "where" <+> pretty f
    PointAdd f -> "point-add" <+> "'" <> prettyZKGroup f
    ScalarMult f -> "scalar-mult" <+> "'" <> prettyZKGroup f
    SelectObj fs -> "select" <+> Pretty.braces (Pretty.hsep (pretty <$> fs))
    where
    prettyZKGroup ZKG1 = "g1"
    prettyZKGroup ZKG2 = "g2"

instance Pretty tyname => Pretty (TypeApp tyname) where
  pretty = \case
    TyAppType t ->
      "@" <> Pretty.parens (pretty t)
    TyAppVar tn ->
      "@" <> Pretty.parens (pretty tn)
    TyAppRow row ->
      "@" <> Pretty.parens (pretty row)
    TyAppRef r ->
      "@ref" <> Pretty.braces (pretty r)


instance (Pretty name, Pretty ty, Pretty b) => Pretty (Defun name ty b i) where
  pretty (Defun name _args ty _term _) =
    parens $ "defun" <+> pretty name <> ":" <> pretty ty

instance (Pretty name, Pretty ty, Pretty b) => Pretty (DefPact name ty b i) where
  pretty (DefPact name _args tys _steps _info) =
    let dpTypes = vsep [ "step" <+> pretty stepIx <+> "type:" <+> pretty ty | (stepIx, ty) <- IM.toList tys]
    in "defpact" <+> pretty name <+> line <+> dpTypes

instance (Pretty name, Pretty ty, Pretty b) => Pretty (DefCap name ty b i) where
  pretty (DefCap name _args ty _ _ _) =
      parens $ "defcap" <+> pretty name <> ":" <> pretty ty

instance Pretty (DefSchema info) where
  pretty (DefSchema n schema i) =
    let argList = [Arg k (Just t) i | (Field k, t) <- M.toList schema]
    in pretty $ PrettyLispApp ("defschema " <> n) argList

instance Pretty (DefTable info) where
  pretty (DefTable tblname schema _) =
    parens $ "deftable" <+> pretty tblname <> ":" <> pretty schema


instance (Pretty name, Pretty builtin, Pretty ty) => Pretty (Step name ty builtin info) where
  pretty = \case
    Step t -> parens ("step" <+> pretty t)
    StepWithRollback t1 t2 -> parens ("step-with-rollback" <+> pretty t1 <+> pretty t2)

instance Pretty (DefConst i) where
  pretty (DefConst n ty v _) =
    parens $ "defconst" <+> pretty n <> ":" <> pretty ty <+> pretty v

instance (Pretty name, Pretty ty, Pretty b) => Pretty (Def name ty b i) where
  pretty = \case
    Dfun d -> pretty d
    DConst d -> pretty d
    DCap d -> pretty d
    DSchema d -> pretty d
    DTable d -> pretty d
    DPact d -> pretty d

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
    ObjectOp o _ -> pretty o
    Format o o' _ ->
      Pretty.parens ("format" <+> pretty o <+> Pretty.hsep (pretty <$> o'))

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
  ObjectOp o i ->
    pure (ObjectOp o i)
  Format o o' i ->
    Format <$> termBuiltin f o <*> traverse (termBuiltin f) o' <*> pure i

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
  ObjectOp o i ->
    ObjectOp o <$> f i
  Format o o' i ->
    Format o o' <$> f i


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
    ObjectOp o i -> pure (ObjectOp o i)
    Format o o' i ->
      Format <$> f o <*> traverse f o' <*> pure i

makePrisms ''IfDef
makePrisms ''Def

instance NFData TypedObjectOp
instance (NFData name) => NFData (TypeApp name)
instance (NFData name, NFData ty, NFData b, NFData info) => NFData (Term name ty b info)
instance (NFData name, NFData ty, NFData b, NFData info) => NFData (Def name ty b info)
instance (NFData info) => NFData (DefSchema info)
instance (NFData name, NFData ty, NFData b, NFData info) => NFData (Defun name ty b info)
instance (NFData info) => NFData (DefConst info)
instance (NFData name, NFData ty, NFData b, NFData info) => NFData (DefCap name ty b info)
instance (NFData name, NFData ty, NFData b, NFData info) => NFData (DefPact name ty b info)
instance (NFData name, NFData ty, NFData b, NFData info) => NFData (Step name ty b info)
instance (NFData name, NFData ty, NFData b, NFData info) => NFData (Module name ty b info)
instance (NFData info) => NFData (Interface info)
instance (NFData info) => NFData (IfDef info)
instance (NFData info) => NFData (IfDefun info)
instance (NFData info) => NFData (IfDefPact info)
instance (NFData info) => NFData (IfDefCap info)
instance (NFData info) => NFData (DefTable info)
