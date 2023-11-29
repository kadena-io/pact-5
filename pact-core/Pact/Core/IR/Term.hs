{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}


-- |
-- Module      :  Pact.Core.IR.Term
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jose Cardona <jose@kadena.io>
--
-- Our Core IR, which is inspected for static guarantees before interpretation
-- The core IR manages to
--

-- Todo: Enumerate imports
module Pact.Core.IR.Term where

import Control.Lens
import Data.Foldable(fold, find)
import Data.Text(Text)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict(Map)
import qualified Data.Set as Set
import qualified Data.List.NonEmpty as NE

import Pact.Core.Guards
import Pact.Core.Builtin
import Pact.Core.Hash
import Pact.Core.Literal
import Pact.Core.Type
import Pact.Core.Names
import Pact.Core.Imports
import Pact.Core.Capabilities
import Pact.Core.Pretty

data Defun name ty builtin info
  = Defun
  { _dfunName :: Text
  , _dfunArgs :: [Arg ty]
  , _dfunRType :: Maybe ty
  , _dfunTerm :: Term name ty builtin info
  , _dfunInfo :: info
  } deriving (Show, Functor, Eq)

data Step name ty builtin info
  = Step (Term name ty builtin info) (Maybe [Term name ty builtin info])
  | StepWithRollback
    (Term name ty builtin info)
    (Term name ty builtin info)
    (Maybe [Term name ty builtin info])
  deriving (Show, Functor, Eq)

hasRollback :: Step n t b i -> Bool
hasRollback Step{} = False
hasRollback StepWithRollback{} = True

ordinaryDefPactStepExec :: Step name ty builtin info -> Term name ty builtin info
ordinaryDefPactStepExec (Step expr _) = expr
ordinaryDefPactStepExec (StepWithRollback expr _ _) = expr

data DefPact name ty builtin info
  = DefPact
  { _dpName :: Text
  , _dpArgs :: [Arg ty]
  , _dpRetType :: Maybe ty
  , _dpSteps :: NonEmpty (Step name ty builtin info)
  , _dpInfo :: info
  } deriving (Show, Functor, Eq)

data DefConst name ty builtin info
  = DefConst
  { _dcName :: Text
  , _dcType :: Maybe ty
  , _dcTerm :: Term name ty builtin info
  , _dcInfo :: info
  } deriving (Show, Functor, Eq)

data DefCap name ty builtin info
  = DefCap
  { _dcapName :: Text
  , _dcapAppArity :: Int
  , _dcapArgs :: [Arg ty]
  , _dcapRType :: Maybe ty
  , _dcapTerm :: Term name ty builtin info
  , _dcapMeta :: DefCapMeta name
  , _dcapInfo :: info
  } deriving (Show, Functor, Eq)

data DefSchema ty info
  = DefSchema
  { _dsName :: Text
  , _dsSchema :: Map Field ty
  , _dsInfo :: info
  } deriving (Show, Functor, Eq)

-- | The type of our desugared table schemas
-- TODO: This GADT is unnecessarily complicated and only really necessary
-- because currently, renaming and desugaring are not in sequence. That is:
-- renaming and desugaring a module happens as a full desugar into a full rename.
-- if they ran one after another, this type would not be necessary
data TableSchema name where
  DesugaredTable :: ParsedName -> TableSchema ParsedName
  ResolvedTable :: Schema -> TableSchema Name

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
  } deriving (Show, Functor, Eq)

data Def name ty builtin info
  = Dfun (Defun name ty builtin info)
  | DConst (DefConst name ty builtin info)
  | DCap (DefCap name ty builtin info)
  | DSchema (DefSchema ty info)
  | DTable (DefTable name info)
  | DPact (DefPact name ty builtin info)
  deriving (Show, Functor, Eq)


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
  } deriving (Show, Functor, Eq)

data Interface name ty builtin info
  = Interface
  { _ifName :: ModuleName
  , _ifDefns :: [IfDef name ty builtin info]
  , _ifHash :: ModuleHash
  , _ifInfo :: info
  } deriving (Show, Eq, Functor)

data IfDefPact ty info
  = IfDefPact
  { _ifdpName :: Text
  , _ifdpArgs :: [Arg ty]
  , _ifdpRType :: Maybe ty
  , _ifdpInfo :: info
  } deriving (Show, Eq, Functor)

data IfDefun ty info
  = IfDefun
  { _ifdName :: Text
  , _ifdArgs :: [Arg ty]
  , _ifdRType :: Maybe ty
  , _ifdInfo :: info
  } deriving (Show, Eq, Functor)

data IfDefCap ty info
  = IfDefCap
  { _ifdcName :: Text
  , _ifdcArgs :: [Arg ty]
  , _ifdcRType :: Maybe ty
  , _ifdcInfo :: info
  } deriving (Show, Eq, Functor)

data IfDef name ty builtin info
  = IfDfun (IfDefun ty info)
  | IfDConst (DefConst name ty builtin info)
  | IfDCap (IfDefCap ty info)
  | IfDPact (IfDefPact ty info)
  | IfDSchema (DefSchema ty info)
  deriving (Show, Eq, Functor)

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

defKind :: Def name Type b i -> DefKind
defKind = \case
  Dfun{} -> DKDefun
  DConst{} -> DKDefConst
  DCap{} -> DKDefCap
  DSchema ds -> DKDefSchema (Schema (_dsSchema ds))
  DTable{} -> DKDefTable
  DPact{} -> DKDefPact

ifDefKind :: IfDef name Type b i -> Maybe DefKind
ifDefKind = \case
  IfDfun{} -> Nothing
  IfDCap{} -> Nothing
  IfDConst{} -> Just DKDefConst
  IfDPact{} -> Nothing

  IfDSchema ds -> Just (DKDefSchema (Schema (_dsSchema ds)))

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

type EvalTerm b i = Term Name Type b i
type EvalDef b i = Def Name Type b i
type EvalModule b i = Module Name Type b i
type EvalInterface b i = Interface Name Type b i

data LamInfo
  = TLDefun ModuleName Text
  | TLDefCap ModuleName Text
  | TLDefPact ModuleName Text
  | AnonLamInfo
  deriving (Show, Eq)

-- | Core IR
data Term name ty builtin info
  = Var name info
  -- ^ single variables e.g x
  | Lam LamInfo (NonEmpty (Arg ty)) (Term name ty builtin info) info
  -- ^ $f = \x.e
  -- Lambdas are named for the sake of the callstack.
  | Let (Arg ty) (Term name ty builtin info) (Term name ty builtin info) info
  -- ^ let x = e1 in e2
  | App (Term name ty builtin info) [Term name ty builtin info] info
  -- ^ (e1 e2)
  | Sequence (Term name ty builtin info) (Term name ty builtin info) info
  -- ^ sequencing, that is e1 `Sequence` e2 evaluates e1
  -- discards the result and then evaluates and returns the result of e2
  | Nullary (Term name ty builtin info) info
  -- ^ "Lazy terms of arity zero"
  | Conditional (BuiltinForm (Term name ty builtin info)) info
  -- ^ Conditional terms
  | Builtin builtin info
  -- ^ Built-in ops, e.g (+)
  | Constant Literal info
  -- ^ Literals
  | ListLit [Term name ty builtin info] info
  -- ^ List Literals
  | Try (Term name ty builtin info) (Term name ty builtin info) info
  -- ^ try (catch expr) (try-expr)
  | ObjectLit [(Field, Term name ty builtin info)] info
  -- ^ an object literal
  -- | DynInvoke (Term name ty builtin info) Text info
  -- ^ dynamic module reference invocation m::f
  | CapabilityForm (CapForm name (Term name ty builtin info)) info
  -- ^ Capability Natives
  | Error Text info
  -- ^ Error term
  deriving (Show, Functor, Eq)

instance (Pretty name, Pretty builtin, Pretty ty) => Pretty (Term name ty builtin info) where
  pretty = \case
    Var name _ -> pretty name
    Lam _ ne te _ ->
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
    -- DynInvoke n t _ ->
    --   pretty n <> "::" <> pretty t
    ObjectLit n _ ->
      braces (hsep $ punctuate "," $ fmap (\(f, t) -> pretty f <> ":" <> pretty t) n)
    Error txt _ ->
      parens ("error" <> pretty txt)
    where
    prettyTyAnn = maybe mempty ((":" <>) . pretty)
    prettyLamArg (Arg n ty) =
      pretty n <> prettyTyAnn ty

instance (Pretty name, Pretty builtin, Pretty ty) => Pretty (TopLevel name ty builtin info) where
  pretty = \case
    TLTerm tm -> pretty tm
    _ -> "todo: pretty defs/modules"


----------------------------
-- Aliases for convenience
----------------------------
termType :: Traversal (Term n t b i) (Term n t' b i) t t'
termType f  = \case
  Var n i -> pure (Var n i)
  Lam li ne te i ->
    Lam li <$> (traversed.argType._Just) f ne <*> termType f te <*> pure i
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
  Error txt i -> pure (Error txt i)

termBuiltin :: Traversal (Term n t b i) (Term n t b' i) b b'
termBuiltin f = \case
  Var n i -> pure (Var n i)
  Lam li ne te i ->
    Lam li ne <$> termBuiltin f te <*> pure i
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
  Error txt i -> pure (Error txt i)

termInfo :: Lens' (Term name ty builtin info) info
termInfo f = \case
  Var n i -> Var n <$> f i
  Let n t1 t2 i ->
    Let n t1 t2 <$> f i
  Lam li ns term i -> Lam li ns term <$> f i
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
  Error t i -> Error t <$> f i
  ObjectLit m i -> ObjectLit m <$> f i

termInfo' :: Traversal (Term name ty builtin info) (Term name ty builtin info') info info'
termInfo' f = \case
  Var n i -> Var n <$> f i
  Let n t1 t2 i ->
    Let n <$> termInfo' f t1 <*> termInfo' f t2 <*> f i
  Lam li ns term i -> Lam li ns <$> termInfo' f term <*> f i
  App t1 t2 i -> App <$> termInfo' f t1 <*> traverse (termInfo' f) t2 <*> f i
  Builtin b i -> Builtin b <$> f i
  Constant l i -> Constant l <$> f i
  Sequence e1 e2 i -> Sequence <$> termInfo' f e1 <*> termInfo' f e2 <*> f i
  Conditional o i ->
    Conditional <$> traverse (termInfo' f) o <*> f i
  ListLit l i  -> ListLit <$> traverse (termInfo' f) l <*> f i
  Try e1 e2 i -> undefined -- Try e1 e2 <$> f i
  Nullary term i ->
    undefined -- Nullary term <$> f i
  CapabilityForm cf i -> undefined -- CapabilityForm cf <$> f i
  Error t i -> Error t <$> f i
  ObjectLit m i -> undefined -- ObjectLit m <$> f i
instance Plated (Term name ty builtin info) where
  plate f = \case
    Var n i -> pure (Var n i)
    Lam li ns term i -> Lam li ns <$> f term <*> pure i
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
    Error e i -> pure (Error e i)

-- defType :: Lens (Def n t b i) (Def n t' b i) t t'
-- defType f = \case
--   Dfun (Defun df)

-- Todo: qualify all of these
makeLenses ''Module
makeLenses ''Interface
makeLenses ''Defun
makeLenses ''DefConst
makeLenses ''DefCap
makePrisms ''Def
makePrisms ''Term
makePrisms ''IfDef
