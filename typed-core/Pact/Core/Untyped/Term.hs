{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}

module Pact.Core.Untyped.Term
 ( Defun(..)
 , DefConst(..)
 , DefCap(..)
 , Def(..)
 , defType
 , defName
 , defTerm
 , defKind
 , ifDefName
 , Module(..)
 , Interface(..)
 , IfDefun(..)
 , IfDefCap(..)
 , IfDef(..)
 , TopLevel(..)
 , ReplTopLevel(..)
 , Term(..)
 , EvalTerm
 , EvalModule
 , EvalInterface
 , EvalDef
 , EvalDefConst
 , fromIRTerm
 , fromIRDef
 , fromIRModule
 , fromIRTopLevel
 , fromIRReplTopLevel
 , termInfo
 -- Module Lenses
 , mName
 , mDefs
 , mBlessed
 , mImports
 , mImplements
 , mHash
 , mGovernance
 , mInfo
 -- Interface lenses
 , ifName
 , ifDefns
 , ifHash
 , ifInfo
 , findIfDef
 , _IfDfun
 , _IfDConst
 ) where

import Control.Lens
import Data.Text(Text)
import Data.Void
import Data.Foldable(foldl', find)
import qualified Data.Set as Set

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
import qualified Pact.Core.IR.Term as IR

data Defun name builtin info
  = Defun
  { _dfunName :: Text
  , _dfunType :: Type Void
  , _dfunTerm :: Term name builtin info
  , _dfunInfo :: info
  } deriving Show

data DefConst name builtin info
  = DefConst
  { _dcName :: Text
  , _dcType :: Type Void
  , _dcTerm :: Term name builtin info
  , _dcInfo :: info
  } deriving Show

data DefCap name builtin info
  = DefCap
  { _dcapName :: Text
  , _dcapAppArity :: Int
  , _dcapArgTypes :: [Type Void]
  , _dcapRType :: Type Void
  , _dcapTerm :: Term name builtin info
  , _dcapMeta :: Maybe (DefCapMeta name)
  , _dcapInfo :: info
  } deriving Show

data Def name builtin info
  = Dfun (Defun name builtin info)
  | DConst (DefConst name builtin info)
  | DCap (DefCap name builtin info)
  deriving Show

-- DCap (DefCap name builtin info)
-- DPact (DefPact name builtin info)
-- DSchema (DefSchema name info)
-- DTable (DefTable name info)

-- Todo: Remove this, not all top level defs have a proper
-- associated type, and DCap types are w holly irrelevant, we cannot simply
-- call them, they can only be evaluated within `with-capability`.
defType :: Def name builtin info -> TypeOfDef Void
defType = \case
  Dfun d -> DefunType (_dfunType d)
  DConst d -> DefunType $ _dcType d
  DCap d -> DefcapType (_dcapArgTypes d) (_dcapRType d)

defName :: Def name builtin i -> Text
defName = \case
  Dfun d -> _dfunName d
  DConst d -> _dcName d
  DCap d -> _dcapName d

defKind :: Def name builtin i -> DefKind
defKind = \case
  Dfun _ -> DKDefun
  DConst _ -> DKDefConst
  DCap _ -> DKDefCap

ifDefName :: IfDef name builtin i -> Text
ifDefName = \case
  IfDfun ifd -> _ifdName ifd
  IfDConst dc -> _dcName dc
  IfDCap d -> _ifdcName d

defTerm :: Def name builtin info -> Term name builtin info
defTerm = \case
  Dfun d -> _dfunTerm d
  DConst d -> _dcTerm d
  DCap d -> _dcapTerm d

data Module name builtin info
  = Module
  { _mName :: ModuleName
  , _mGovernance :: Governance name
  , _mDefs :: [Def name builtin info]
  , _mBlessed :: !(Set.Set ModuleHash)
  , _mImports :: [Import]
  , _mImplements :: [ModuleName]
  , _mHash :: ModuleHash
  , _mInfo :: info
  } deriving Show

data Interface name builtin info
  = Interface
  { _ifName :: ModuleName
  , _ifDefns :: [IfDef name builtin info]
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

data IfDef name builtin info
  = IfDfun (IfDefun info)
  | IfDConst (DefConst name builtin info)
  | IfDCap (IfDefCap info)
  deriving Show

data TopLevel name builtin info
  = TLModule (Module name builtin info)
  | TLInterface (Interface name builtin info)
  | TLTerm (Term name builtin info)
  deriving Show

data ReplTopLevel name builtin info
  = RTLModule (Module name builtin info)
  | RTLInterface (Interface name builtin info)
  | RTLDefun (Defun name builtin info)
  | RTLDefConst (DefConst name builtin info)
  | RTLTerm (Term name builtin info)
  deriving Show

-- | Untyped pact core terms
data Term name builtin info
  = Var name info
  -- ^ single variables, e.g the term `x`
  | Lam (Term name builtin info) info
  -- ^ f = \a b c -> e
  -- All lambdas, even anonymous ones, are named, for the sake of them adding a stack frame
  | App (Term name builtin info) (Term name builtin info) info
  -- ^ Constant/Literal values
  | Sequence (Term name builtin info) (Term name builtin info) info
  -- ^ (e_1 e_2 .. e_n)
  | Conditional (BuiltinForm (Term name builtin info)) info
  -- ^ Special nodes for If, And and Or.
  | Builtin builtin info
  -- ^ Built-in functions (or natives)
  | Constant Literal info
  -- ^ ΛX.e
  | ListLit [Term name builtin info] info
  -- ^ [e_1, e_2, .., e_n]
  | Try (Term name builtin info) (Term name builtin info) info
  -- ^ try (catch expr) (try-expr)
  | DynInvoke (Term name builtin info) Text info
  -- ^ dynamic module reference invocation m::f
  | CapabilityForm (CapForm name (Term name builtin info)) info
  -- ^ Capability
  | Error Text info
  -- ^ Error catching
  deriving (Show, Functor, Foldable, Traversable)

-- Post Typecheck terms + modules
type EvalTerm b i = Term Name b i
type EvalDefConst b i = DefConst Name b i
type EvalDef b i = Def Name b i
type EvalModule b i = Module Name b i
type EvalInterface b i = Interface Name b i

fromIRTerm :: IR.Term n b i -> Term n b i
fromIRTerm = \case
  IR.Var n i -> Var n i
  IR.Lam nsts body i ->
    foldr (\_ t -> Lam t i) (fromIRTerm body) nsts
  IR.Let _ _ e1 e2 i ->
    App (Lam (fromIRTerm e2) i) (fromIRTerm e1) i
  IR.App fn apps i ->
    foldl' (\f arg -> App f (fromIRTerm arg) i) (fromIRTerm fn) apps
  IR.Builtin b i ->
    Builtin b i
  IR.Constant lit i ->
    Constant lit i
  IR.Sequence e1 e2 i ->
    Sequence (fromIRTerm e1) (fromIRTerm e2) i
  IR.Conditional c i ->
    Conditional (fromIRTerm <$> c) i
  IR.ListLit v i ->
    ListLit (fromIRTerm <$> v) i
  IR.Try e1 e2 i ->
    Try (fromIRTerm e1) (fromIRTerm e2) i
  IR.DynInvoke n t i ->
    DynInvoke (fromIRTerm n) t i
  IR.CapabilityForm cf i ->
    CapabilityForm (fmap fromIRTerm cf) i
  IR.Error e i ->
    Error e i

fromIRDefun
  :: IR.Defun name builtin info
  -> Defun name builtin info
fromIRDefun (IR.Defun n ty term i) =
  Defun n (fmap absurd ty) (fromIRTerm term) i

fromIRIfDefun :: IR.IfDefun info -> IfDefun info
fromIRIfDefun (IR.IfDefun dfn ty i) =
  IfDefun dfn ty i

fromIRIfDefCap :: IR.IfDefCap info -> IfDefCap info
fromIRIfDefCap (IR.IfDefCap dfn argtys rty i) =
  IfDefCap dfn argtys rty i

fromIRDConst
  :: IR.DefConst name builtin info
  -> DefConst name builtin info
fromIRDConst (IR.DefConst n ty term i) =
  DefConst n (maybe TyUnit (fmap absurd) ty) (fromIRTerm term) i

fromIRDCap :: IR.DefCap name builtin info -> DefCap name builtin info
fromIRDCap (IR.DefCap name arity argtys rtype body meta i) =
  DefCap  name arity argtys rtype (fromIRTerm body) meta i

fromIRDef
  :: IR.Def name builtin info
  -> Def name builtin info
fromIRDef = \case
  IR.Dfun d -> Dfun (fromIRDefun d)
  IR.DConst d -> DConst (fromIRDConst d)
  IR.DCap d -> DCap (fromIRDCap d)

fromIRIfDef
  :: IR.IfDef name builtin info
  -> IfDef name builtin info
fromIRIfDef = \case
  IR.IfDfun d -> IfDfun (fromIRIfDefun d)
  IR.IfDConst d -> IfDConst (fromIRDConst d)
  IR.IfDCap d -> IfDCap (fromIRIfDefCap d)

fromIRModule
  :: IR.Module name builtin info
  -> Module name builtin info
fromIRModule (IR.Module mn gov defs blessed imports implements hs i) =
  Module mn gov (fromIRDef <$> defs) blessed imports implements hs i

fromIRInterface
  :: IR.Interface name builtin info
  -> Interface name builtin info
fromIRInterface (IR.Interface ifn ifdefs ifhash i) =
  Interface ifn (fromIRIfDef <$> ifdefs) ifhash i

fromIRTopLevel
  :: IR.TopLevel name builtin info
  -> TopLevel name builtin info
fromIRTopLevel = \case
  IR.TLModule m -> TLModule (fromIRModule m)
  IR.TLInterface iface ->
    TLInterface (fromIRInterface iface)
  IR.TLTerm e -> TLTerm (fromIRTerm e)

fromIRReplTopLevel
  :: IR.ReplTopLevel name builtin info
  -> ReplTopLevel name builtin info
fromIRReplTopLevel = \case
  IR.RTLModule m -> RTLModule (fromIRModule m)
  IR.RTLInterface iface -> RTLInterface (fromIRInterface iface)
  IR.RTLTerm e -> RTLTerm (fromIRTerm e)
  IR.RTLDefun df -> RTLDefun (fromIRDefun df)
  IR.RTLDefConst dc -> RTLDefConst (fromIRDConst dc)

findIfDef :: Text -> Interface name builtin info -> Maybe (IfDef name builtin info)
findIfDef f iface =
  find ((== f) . ifDefName) (_ifDefns iface)

instance (Pretty name, Pretty builtin) => Pretty (Term name builtin info) where
  pretty = \case
    Var n _ ->
      pretty n
    Lam term _ ->
      Pretty.parens ("λ." <> pretty term)
    App t1 t2 _ ->
      Pretty.parens (Pretty.hsep [pretty t1, pretty t2])
    Builtin b _ -> pretty b
    Constant l _ -> pretty l
    Sequence e1 e2 _ -> Pretty.parens ("seq" <+> pretty e1 <+> pretty e2)
    Conditional c _ -> pretty c
    ListLit li _ ->
      Pretty.brackets $
      Pretty.hsep $
      Pretty.punctuate Pretty.comma (pretty <$> li)
    Try e1 e2 _ ->
      Pretty.parens ("try" <+> pretty e1 <+> pretty e2)
    DynInvoke n t _ ->
      pretty n <> "::" <> pretty t
    CapabilityForm _ _ -> error "pretty capform"
    Error e _ ->
      Pretty.parens ("error \"" <> pretty e <> "\"")
    -- ObjectLit (M.toList -> obj) _ ->
    --   Pretty.braces $
    --   Pretty.hsep $
    --   Pretty.punctuate Pretty.comma $
    --   fmap (\(f, o) -> pretty f <> ":" <+> pretty o) obj
    -- ObjectOp oop _ -> case oop of
    --   ObjectAccess fi te ->
    --     Pretty.parens $ Pretty.hsep ["@" <> pretty fi, pretty te]
    --   ObjectRemove fi te ->
    --     Pretty.parens $ Pretty.hsep ["#" <> pretty fi, pretty te]
    --   ObjectExtend fi v o ->
    --     Pretty.braces $ Pretty.hsep [pretty fi <> ":" <> pretty v, "|", pretty o]

termInfo :: Lens' (Term name builtin info) info
termInfo f = \case
  Var n i -> Var n <$> f i
  Lam term i -> Lam term <$> f i
  App t1 t2 i -> App t1 t2 <$> f i
  Sequence e1 e2 i -> Sequence e1 e2 <$> f i
  Conditional c i -> Conditional c <$> f i
  ListLit v i -> ListLit v <$> f i
  Builtin b i -> Builtin b <$> f i
  Constant l i -> Constant l <$> f i
  Try e1 e2 i ->
    Try e1 e2 <$> f i
  DynInvoke n t i -> DynInvoke n t <$> f i
  CapabilityForm cf i ->
    CapabilityForm cf <$> f i
  Error e i ->
    Error e <$> f i

makeLenses ''Module
makeLenses ''Interface
makePrisms ''IfDef
