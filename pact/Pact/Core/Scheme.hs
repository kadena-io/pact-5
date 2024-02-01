{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies, GADTs, DataKinds #-}


module Pact.Core.Scheme
  ( PPKScheme(..)
  , defPPKScheme
  , SPPKScheme(..)
  ) where

import GHC.Generics
import Control.DeepSeq
import Data.Kind (Type)

import qualified Pact.JSON.Encode as J


--------- PPKSCHEME DATA TYPE ---------

data PPKScheme = ED25519 | WebAuthn
  deriving (Show, Eq, Ord, Generic, Bounded, Enum)

instance NFData PPKScheme

instance J.Encode PPKScheme where
  build ED25519 = J.text "ED25519"
  build WebAuthn = J.text "WebAuthn"
  {-# INLINE build #-}

defPPKScheme :: PPKScheme
defPPKScheme = ED25519

-- Run-time witness to PPKScheme kind.

data SPPKScheme :: PPKScheme -> Type where
  SED25519 :: SPPKScheme 'ED25519
  SWebAuthn :: SPPKScheme 'WebAuthn
instance Show (SPPKScheme a) where
  show SED25519 = show ED25519
  show SWebAuthn = show WebAuthn
