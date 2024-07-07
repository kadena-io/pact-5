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
  ) where

import GHC.Generics
import Control.DeepSeq

import qualified Pact.JSON.Decode as JD
import qualified Pact.JSON.Encode as J


--------- PPKSCHEME DATA TYPE ---------

-- | 
data PPKScheme = ED25519 | WebAuthn
  deriving (Show, Eq, Ord, Generic, Bounded, Enum)

instance NFData PPKScheme

instance J.Encode PPKScheme where
  build ED25519 = J.text "ED25519"
  build WebAuthn = J.text "WebAuthn"
  {-# INLINE build #-}

instance JD.FromJSON PPKScheme where
  parseJSON = JD.withText "PPKScheme" $ \case
    "ED25519" -> pure ED25519
    "WebAuthn" -> pure WebAuthn
    _ -> fail "Invalid PPKScheme"

defPPKScheme :: PPKScheme
defPPKScheme = ED25519
