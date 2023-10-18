-- | 

module Pact.Core.StableEncoding
where

import Pact.Core.PactValue

newtype StableEncoding a
  = StableEncoding a


instance J.Encode (StableEncoding Literal) where
  build (StableEncoding lit) = case lit of
    LString t -> undefined
    LInteger i -> undefined
    LDecimal d -> undefined
    LUnit -> undefined
    LBool b -> undefined
  {-# INLINABLE build #-}

instance J.Encode (StableEncoding (Guard FullyQualifiedName PactValue)) where
  build (StableEncoding g) = case g of
    GKetset ks -> undefined
    GKetSetRef ksn -> undefined
    GUserGuard -> undefined
    GCapabilityGuard cg -> undefined
    GModuleGuard mg -> undefined
  {-# INLINABLE build #-}

instance J.Encode (StableEncoding (CapabilityGuard FullyQualifiedName PactValue)) where
  build (StableEncoding (CapabilityGuard name args)) = undefined
  {-# INLINABLE build #-}

instance J.Encode (StableEncoding ModuleGuards) where
  build (StableEncoding (ModuleGuard mod name)) = undefined
  {-# INLINABLE build #-}

instance J.Encode (StableEncoding (UserGuard FullyQualifiedName PactValue)) where
  build (StableEncoding (UserGuard fun args)) = undefined
  {-# INLINABLE build #-}

instance J.Encode (StableEncoding KeySetName) where
  build (StableEncoding (KeySetName ksn)) = undefined
  {-# INLINABLE build #-}

instance J.Encode (StableEncoding (KeySet FullyQualifiedName)) where
  build (StableEncoding (KeySet keys predFun)) = undefined
  {-# INLINABLE build #-}

instance J.Encode (StableEncoding (KSPredicate FullyQualifiedName)) where
  build (StableEncoding ksp) = case ksp of
    KeysAll -> undefined
    Keys2 -> undefined
    KeysAny -> undefined
  {-# INLINABLE build #-}



instance J.Encode (StableEncoding PublicKeyText) where
  build (StableEncoding (PublicKeyText pkt)) = undefined
  {-# INLINABLE build #-}



instance J.Encode (StableEncoding PactValue) where
  build (StableEncoding pv) = case pv of
    PLiteral lit  J.build (StableEncoding lit)
    PList l -> J.build (J.Array (StableEncoding <$> v))
    PGuard g -> undefined
    PObject o -> undefined
    PModRef mr -> undefined
    PCapToken ct -> undefined
    PTime pt -> undefined
  {-# INLINABLE build #-}
