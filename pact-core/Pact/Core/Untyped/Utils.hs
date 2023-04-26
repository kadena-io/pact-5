{-# LANGUAGE LambdaCase #-}

module Pact.Core.Untyped.Utils where

import Data.Foldable(foldl')

import Pact.Core.Untyped.Term
import qualified Pact.Core.Typed.Term as Typed

fromTypedTerm :: Typed.Term name tyname b i -> Term name b i
fromTypedTerm = \case
  Typed.Var n i -> Var n i
  Typed.Lam args body i ->
    foldr (\_ t -> Lam t i) (fromTypedTerm body) args
  Typed.App fn apps i ->
    foldl' (\f arg -> App f (fromTypedTerm arg) i) (fromTypedTerm fn) apps
  Typed.Let _ e1 e2 i ->
    App (Lam (fromTypedTerm e2) i) (fromTypedTerm e1) i
  Typed.Builtin b i ->
    Builtin b i
  Typed.Constant lit i ->
    Constant lit i
  Typed.TyApp te _ _ ->
    fromTypedTerm te
  Typed.TyAbs _ term _ ->
    fromTypedTerm term
  Typed.Sequence e1 e2 i ->
    Sequence (fromTypedTerm e1) (fromTypedTerm e2) i
  Typed.Conditional c i ->
    Conditional (fromTypedTerm <$> c) i
  Typed.ListLit _ vec i ->
    ListLit (fromTypedTerm <$> vec) i
  Typed.Try e1 e2 i ->
    Try (fromTypedTerm e1) (fromTypedTerm e2) i
  Typed.DynInvoke term t i ->
    DynInvoke (fromTypedTerm term) t i
  Typed.CapabilityForm cf i ->
    CapabilityForm (fromTypedTerm <$> cf) i
  Typed.Error _ e i -> Error e i
  -- Typed.ObjectLit m i ->
  --   ObjectLit (fromTypedTerm <$> m) i
  -- Typed.ObjectOp oo i ->
  --   ObjectOp (fromTypedTerm <$> oo) i


fromTypedDefun
  :: Typed.Defun name tyname builtin info
  -> Defun name builtin info
fromTypedDefun (Typed.Defun n ty term i) =
  Defun n ty (fromTypedTerm term) i

fromTypedIfDefun
  :: Typed.IfDefun name info
  -> IfDefun info
fromTypedIfDefun (Typed.IfDefun n ty i) =
  IfDefun n ty i

fromTypedDConst
  :: Typed.DefConst name tyname builtin info
  -> DefConst name builtin info
fromTypedDConst (Typed.DefConst n ty term i) =
  DefConst n ty (fromTypedTerm term) i

fromTypedDCap
  :: Typed.DefCap name tyname builtin info
  -> DefCap name builtin info
fromTypedDCap (Typed.DefCap name ty term meta i) =
  DefCap name ty (fromTypedTerm term) meta i

fromTypedDef
  :: Typed.Def name tyname builtin info
  -> Def name builtin info
fromTypedDef = \case
  Typed.Dfun d -> Dfun (fromTypedDefun d)
  Typed.DConst d -> DConst (fromTypedDConst d)
  Typed.DCap d -> DCap (fromTypedDCap d)

fromTypedIfDef
  :: Typed.IfDef name tyname builtin info
  -> IfDef name builtin info
fromTypedIfDef = \case
  Typed.IfDfun d -> IfDfun (fromTypedIfDefun d)
  Typed.IfDConst d ->
    IfDConst (fromTypedDConst d)

fromTypedModule
  :: Typed.Module name tyname builtin info
  -> Module name builtin info
fromTypedModule (Typed.Module mn mgov defs blessed imports implements hs) =
  Module mn mgov (fromTypedDef <$> defs) blessed imports implements hs

fromTypedInterface
  :: Typed.Interface name tyname builtin info
  -> Interface name builtin info
fromTypedInterface (Typed.Interface ifname ifdefs ifh) =
  Interface ifname (fromTypedIfDef <$> ifdefs) ifh

fromTypedTopLevel
  :: Typed.TopLevel name tyname builtin info
  -> TopLevel name builtin info
fromTypedTopLevel = \case
  Typed.TLModule m ->
    TLModule (fromTypedModule m)
  Typed.TLInterface iface ->
    TLInterface (fromTypedInterface iface)
  Typed.TLTerm e ->
    TLTerm (fromTypedTerm e)

fromTypedReplTopLevel
  :: Typed.ReplTopLevel name tyname builtin info
  -> ReplTopLevel name builtin info
fromTypedReplTopLevel = \case
  Typed.RTLModule m ->
    RTLModule (fromTypedModule m)
  Typed.RTLDefun de ->
    RTLDefun (fromTypedDefun de)
  Typed.RTLDefConst dc ->
    RTLDefConst (fromTypedDConst dc)
  Typed.RTLTerm te ->
    RTLTerm (fromTypedTerm te)
  Typed.RTLInterface i ->
    RTLInterface (fromTypedInterface i)
