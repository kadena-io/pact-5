{-# LANGUAGE GADTs #-}

module Pact.Core.IR.ModuleHashing
 ( hashInterfaceAndReplace
 , hashModuleAndReplace
 , hashTopLevel
 ) where

import Control.Lens

import Codec.Serialise(serialise, Serialise)

import qualified Data.ByteString as B

import Pact.Core.Capabilities
import Pact.Core.Builtin
import Pact.Core.Type
import Pact.Core.Guards
import Pact.Core.IR.Term
import Pact.Core.Hash
import Pact.Core.Names
import Pact.Core.PactValue
import Pact.Core.Serialise.CBOR_V1
import Data.Functor (void)

hashTopLevel :: (Serialise (SerialiseV1 b)) => TopLevel Name Type b i -> TopLevel Name Type b i
hashTopLevel = \case
  TLTerm t -> TLTerm t
  TLModule m -> TLModule $ hashModuleAndReplace m
  TLInterface iface -> TLInterface $ hashInterfaceAndReplace iface
  TLUse u i -> TLUse u i

hashModuleAndReplace :: (Serialise (SerialiseV1 b)) => Module Name Type b i -> Module Name Type b i
hashModuleAndReplace m@(Module mname mgov defs mblessed imports mimps _mh txh info) =
  let defs' = updateDefHashes mname newMhash <$> defs
  in Module mname gov' defs' mblessed imports mimps newMhash txh info
  where
  newMhash = ModuleHash $ hash $ encodeModule m
  gov' = case mgov of
    KeyGov n -> KeyGov n
    CapGov (FQName fqn) -> CapGov $ FQName $ set fqHash newMhash fqn

hashInterfaceAndReplace :: (Serialise (SerialiseV1 b)) => Interface Name Type b i -> Interface Name Type b i
hashInterfaceAndReplace iface@(Interface ifn defs imps _mh txh info) =
  Interface ifn defs imps newMhash txh info
  where
  newMhash = ModuleHash $ hash $ encodeInterface iface

updateDefHashes :: ModuleName -> ModuleHash -> Def Name Type b i -> Def Name Type b i
updateDefHashes mname mhash = \case
  Dfun d -> Dfun $ over dfunTerm (updateTermHashes mname mhash) d
  DConst d -> DConst $ case _dcTerm d of
    TermConst t -> set dcTerm (TermConst (updateTermHashes mname mhash t)) d
    EvaledConst v -> set dcTerm (EvaledConst (updatePactValueHash mname mhash v)) d
  DCap d ->
    DCap $ over dcapTerm (updateTermHashes mname mhash)
         $ over (dcapMeta.dcMetaFqName) (updateFqNameHash mname mhash) d
  DPact d ->
    let updateStep (Step e1) = Step (updateTermHashes mname mhash e1)
        updateStep (StepWithRollback e1 e2) = StepWithRollback (updateTermHashes mname mhash e1) (updateTermHashes mname mhash e2)
    in DPact $ over dpSteps (fmap updateStep) d
  DTable d -> DTable d
  DSchema s -> DSchema s

updateTermHashes :: ModuleName -> ModuleHash -> Term Name Type b i -> Term Name Type b i
updateTermHashes mname mhash = transform $ \case
  Var n i -> Var (updateNameHash mname mhash n) i
  CapabilityForm cf i ->
    CapabilityForm (over capFormName (updateNameHash mname mhash) cf) i
  a -> a

updateNameHash :: ModuleName -> ModuleHash -> Name -> Name
updateNameHash mname mhash (Name n nk) = case nk of
  NTopLevel tlmod _ | tlmod == mname -> Name n (NTopLevel tlmod mhash)
  _ -> Name n nk

updateFqNameHash :: ModuleName -> ModuleHash -> FullyQualifiedName -> FullyQualifiedName
updateFqNameHash mname mhash (FullyQualifiedName tlmod n mh)
  | tlmod == mname = FullyQualifiedName tlmod n mhash
  | otherwise = FullyQualifiedName tlmod n mh

updatePactValueHash :: ModuleName -> ModuleHash -> PactValue -> PactValue
updatePactValueHash mname mhash = \case
  PLiteral l -> PLiteral l
  PList l ->
    PList $ updatePactValueHash mname mhash <$> l
  PGuard g -> PGuard g
  PObject o -> PObject $ updatePactValueHash mname mhash <$> o
  PModRef m -> PModRef m
  PCapToken (CapToken ct pvs) ->
    PCapToken $ CapToken (updateFqNameHash mname mhash ct) (updatePactValueHash mname mhash <$> pvs)
  PTime t -> PTime t

encodeModule :: (Serialise (SerialiseV1 b)) => Module Name Type b i -> B.ByteString
encodeModule (void -> Module mname mgov defs mblessed imports mimps _mh _txh _i) =
  B.toStrict $ serialise (SerialiseV1 (Module mname mgov defs mblessed imports mimps (ModuleHash (Hash mempty)) (Hash mempty) ()))
{-# SPECIALISE encodeModule :: Module Name Type CoreBuiltin i -> B.ByteString #-}

encodeInterface :: (Serialise (SerialiseV1 b)) => Interface Name Type b i -> B.ByteString
encodeInterface (void -> Interface ifn defns imports _mh _txh _i) =
  B.toStrict $ serialise (SerialiseV1 (Interface ifn defns imports (ModuleHash (Hash mempty)) (Hash mempty) ()))
{-# SPECIALISE encodeInterface :: Interface Name Type CoreBuiltin i -> B.ByteString #-}

