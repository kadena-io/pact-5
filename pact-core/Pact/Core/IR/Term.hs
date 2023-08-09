{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

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
import Data.Foldable(fold)
import Data.Void(Void)
import Data.Text(Text)
import Data.List.NonEmpty (NonEmpty)
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

data Defun name builtin info
  = Defun
  { _dfunName :: Text
  , _dfunArgs :: [Arg Void]
  , _dfunRType :: Maybe (Type Void)
  , _dfunTerm :: Term name builtin info
  , _dfunInfo :: info
  } deriving (Show, Functor)

data DefConst name builtin info
  = DefConst
  { _dcName :: Text
  , _dcType :: Maybe (Type Void)
  , _dcTerm :: Term name builtin info
  , _dcInfo :: info
  } deriving (Show, Functor)

data DefCap name builtin info
  = DefCap
  { _dcapName :: Text
  , _dcapAppArity :: Int
  , _dcapArgs :: [Arg Void]
  , _dcapRType :: Maybe (Type Void)
  , _dcapTerm :: Term name builtin info
  , _dcapMeta :: Maybe (DefCapMeta name)
  , _dcapInfo :: info
  } deriving (Show, Functor)

data Def name builtin info
  = Dfun (Defun name builtin info)
  | DConst (DefConst name builtin info)
  | DCap (DefCap name builtin info)
  deriving (Show, Functor)

defName :: Def name b i -> Text
defName (Dfun d) = _dfunName d
defName (DConst d) = _dcName d
defName (DCap d) = _dcapName d

defKind :: Def name b i -> DefKind
defKind = \case
  Dfun{} -> DKDefun
  DConst{} -> DKDefConst
  DCap{} -> DKDefCap

ifDefName :: IfDef name builtin i -> Text
ifDefName = \case
  IfDfun ifd -> _ifdName ifd
  IfDConst dc -> _dcName dc
  IfDCap ifd -> _ifdcName ifd

defInfo :: Def name b i -> i
defInfo = \case
  Dfun de -> _dfunInfo de
  DConst dc -> _dcInfo dc
  DCap dc -> _dcapInfo dc

ifDefInfo :: IfDef name b i -> i
ifDefInfo = \case
  IfDfun de -> _ifdInfo de
  IfDConst dc -> _dcInfo dc
  IfDCap d -> _ifdcInfo d

-- TODO:
-- Support module guards
-- Support governance
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
  } deriving (Show, Functor)

data Interface name builtin info
  = Interface
  { _ifName :: ModuleName
  , _ifDefns :: [IfDef name builtin info]
  , _ifHash :: ModuleHash
  , _ifInfo :: info
  } deriving (Show, Functor)

data IfDefun info
  = IfDefun
  { _ifdName :: Text
  , _ifdArgs :: [Arg Void]
  , _ifdRType :: Maybe (Type Void)
  , _ifdInfo :: info
  } deriving (Show, Functor)

data IfDefCap info
  = IfDefCap
  { _ifdcName :: Text
  , _ifdcArgs :: [Arg Void]
  , _ifdcRType :: Maybe (Type Void)
  , _ifdcInfo :: info
  } deriving (Show, Functor)

data IfDef name builtin info
  = IfDfun (IfDefun info)
  | IfDConst (DefConst name builtin info)
  | IfDCap (IfDefCap info)
  deriving (Show, Functor)

data TopLevel name builtin info
  = TLModule (Module name builtin info)
  | TLInterface (Interface name builtin info)
  | TLTerm (Term name builtin info)
  deriving (Show, Functor)

data ReplTopLevel name builtin info
  = RTLModule (Module name builtin info)
  | RTLInterface (Interface name builtin info)
  | RTLDefConst (DefConst name builtin info)
  | RTLDefun (Defun name builtin info)
  | RTLTerm (Term name builtin info)
  deriving (Show, Functor)


type EvalTerm b i = Term Name b i
type EvalDef b i = Def Name b i

data LamInfo
  = TLLamInfo ModuleName Text
  | AnonLamInfo
  deriving Show

-- | Core IR
data Term name builtin info
  = Var name info
  -- ^ single variables e.g x
  | Lam LamInfo (NonEmpty (Arg Void)) (Term name builtin info) info
  -- ^ $f = \x.e
  -- Lambdas are named for the sake of the callstack.
  | Let Text (Maybe (Type Void)) (Term name builtin info) (Term name builtin info) info
  -- ^ let x = e1 in e2
  | App (Term name builtin info) (NonEmpty (Term name builtin info)) info
  -- ^ (e1 e2)
  | Sequence (Term name builtin info)  (Term name builtin info) info
  -- ^ error term , error "blah"
  | Conditional (BuiltinForm (Term name builtin info)) info
  -- ^ Conditional terms
  | Builtin builtin info
  -- ^ Built-in ops, e.g (+)
  | Constant Literal info
  -- ^ Literals
  | ListLit [Term name builtin info] info
  -- ^ List Literals
  | Try (Term name builtin info) (Term name builtin info) info
  -- ^ try (catch expr) (try-expr)
  | CapabilityForm (CapForm name (Term name builtin info)) info
  -- ^ Capability Natives
  | DynInvoke (Term name builtin info) Text info
  -- ^ dynamic module reference invocation m::f
  | Error Text info
  -- ^ Error term
  deriving (Show, Functor)

instance (Pretty name, Pretty builtin) => Pretty (Term name builtin info) where
  pretty = \case
    Var name _ -> pretty name
    Lam _ ne te _ ->
      parens ("lambda" <+> parens (fold (NE.intersperse ":" (prettyLamArg <$> ne))) <+> pretty te)
    Let name m_ty te te' _ ->
      parens $ "let" <+> parens (pretty name <> prettyTyAnn m_ty <+> pretty te) <+> pretty te'
    App te ne _ ->
      parens (pretty te <+> hsep (NE.toList (pretty <$> ne)))
    Sequence te te' _ ->
      parens ("seq" <+> pretty te <+> pretty te')
    Conditional o _ ->
      pretty o
    Builtin builtin _ -> pretty builtin
    Constant lit _ ->
      pretty lit
    ListLit tes _ ->
      pretty tes
    CapabilityForm cf _ ->
      pretty cf
    Try te te' _ ->
      parens ("try" <+> pretty te <+> pretty te')
    DynInvoke n t _ ->
      pretty n <> "::" <> pretty t
    Error txt _ ->
      parens ("error" <> pretty txt)
    where
    prettyTyAnn = maybe mempty ((":" <>) . pretty)
    prettyLamArg (Arg n ty) =
      pretty n <> prettyTyAnn ty


----------------------------
-- Aliases for convenience
----------------------------
termBuiltin :: Traversal (Term n b i) (Term n b' i) b b'
termBuiltin f = \case
  Var n i -> pure (Var n i)
  Lam li ne te i ->
    Lam li ne <$> termBuiltin f te <*> pure i
  Let n m_ty te te' i ->
    Let n m_ty <$> termBuiltin f te <*> termBuiltin f te' <*> pure i
  App te ne i ->
    App <$> termBuiltin f te <*> traverse (termBuiltin f) ne <*> pure i
  Sequence te te' i ->
    Sequence <$> termBuiltin f te <*> termBuiltin f te' <*> pure i
  Conditional bf i ->
    Conditional <$> traverse (termBuiltin f) bf <*> pure i
  Builtin b i ->
    Builtin <$> f b <*> pure i
  Constant lit i ->
    pure (Constant lit i)
  ListLit tes i ->
    ListLit <$> traverse (termBuiltin f) tes <*> pure i
  Try te te' i ->
    Try <$> termBuiltin f te <*> termBuiltin f te' <*> pure i
  CapabilityForm cf i ->
    CapabilityForm <$> traverse (termBuiltin f) cf <*> pure i
  DynInvoke n t i ->
    DynInvoke <$> termBuiltin f n <*> pure t <*> pure i
  Error txt i -> pure (Error txt i)

termInfo :: Lens' (Term name builtin info) info
termInfo f = \case
  Var n i -> Var n <$> f i
  Let n mty t1 t2 i ->
    Let n mty t1 t2 <$> f i
  Lam li ns term i -> Lam li ns term <$> f i
  App t1 t2 i -> App t1 t2 <$> f i
  Builtin b i -> Builtin b <$> f i
  Constant l i -> Constant l <$> f i
  Sequence e1 e2 i -> Sequence e1 e2 <$> f i
  Conditional o i ->
    Conditional o <$> f i
  ListLit l i  -> ListLit l <$> f i
  Try e1 e2 i -> Try e1 e2 <$> f i
  DynInvoke n t i -> DynInvoke n t <$> f i
  CapabilityForm cf i -> CapabilityForm cf <$> f i
  Error t i -> Error t <$> f i
  -- ObjectLit m i -> ObjectLit m <$> f i
  -- ObjectOp o i -> ObjectOp o <$> f i

instance Plated (Term name builtin info) where
  plate f = \case
    Var n i -> pure (Var n i)
    Lam li ns term i -> Lam li ns <$> f term <*> pure i
    Let n mty t1 t2 i -> Let n mty <$> f t1 <*> f t2 <*> pure i
    App t1 t2 i -> App <$> f t1 <*> traverse f t2 <*> pure i
    Builtin b i -> pure (Builtin b i)
    Constant l i -> pure (Constant l i)
    Sequence e1 e2 i -> Sequence <$> f e1 <*> f e2 <*> pure i
    Conditional o i ->
      Conditional <$> traverse (plate f) o <*> pure i
    ListLit m i -> ListLit <$> traverse f m <*> pure i
    CapabilityForm cf i ->
      CapabilityForm <$> traverse f cf <*> pure i
    Try e1 e2 i ->
      Try <$> f e1 <*> f e2 <*> pure i
    DynInvoke n t i ->
      pure (DynInvoke n t i)
    Error e i -> pure (Error e i)

-- Todo: qualify all of these
makeLenses ''Module
makeLenses ''Interface
makeLenses ''Defun
makeLenses ''DefConst
makeLenses ''DefCap
makePrisms ''Def
makePrisms ''Term
makePrisms ''IfDef
