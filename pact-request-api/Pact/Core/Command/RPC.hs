{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}


-- |
-- Module      :  Pact.Types.RPC
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Pact API RPC types.
--

module Pact.Core.Command.RPC
  ( -- * Types
    PactRPC(..)
  , _Exec
  , _Continuation
  , ExecMsg(..)
  , pmCode
  , pmData
  , ContMsg(..)
  , cmPactId
  , cmStep
  , cmRollback
  , cmData
  , cmProof
  ) where

import Control.Applicative
import Control.DeepSeq
import Control.Lens

import GHC.Generics

import Pact.Core.SPV
import Pact.Core.Names

import Pact.JSON.Decode
import Pact.Core.StableEncoding
import qualified Pact.JSON.Encode as J
import Pact.Core.PactValue


data PactRPC c =
    Exec !(ExecMsg c) |
    Continuation !ContMsg
    deriving (Eq,Show,Generic,Functor,Foldable,Traversable)

instance NFData c => NFData (PactRPC c)
instance FromJSON c => FromJSON (PactRPC c) where
    parseJSON =
        withObject "RPC" $ \o ->
            (Exec <$> o .: "exec") <|> (Continuation <$> o .: "cont")
    {-# INLINE parseJSON #-}

instance J.Encode c => J.Encode (PactRPC c) where
  build (Exec p) = J.object ["exec" J..= p]
  build (Continuation p) = J.object ["cont" J..= p]
  {-# INLINE build #-}

data ExecMsg c = ExecMsg
  { _pmCode :: c
  , _pmData :: PactValue
  } deriving (Eq,Generic,Show,Functor,Foldable,Traversable)

instance NFData c => NFData (ExecMsg c)
instance FromJSON c => FromJSON (ExecMsg c) where
  parseJSON =
      withObject "PactMsg" $ \o ->
          ExecMsg <$> o .: "code" <*> (maybe PUnit _stableEncoding <$> o .:? "data")
  {-# INLINE parseJSON #-}


instance J.Encode c => J.Encode (ExecMsg c) where
  build o = J.object
    [ "data" J..= StableEncoding (_pmData o)
    , "code" J..= _pmCode o
    ]
  {-# INLINE build #-}

data ContMsg = ContMsg
  { _cmPactId :: !DefPactId
  , _cmStep :: !Int
  , _cmRollback :: !Bool
  , _cmData :: !PactValue
  , _cmProof :: !(Maybe ContProof)
  } deriving (Eq,Show,Generic)

instance NFData ContMsg
instance FromJSON ContMsg where
  parseJSON =
      withObject "ContMsg" $ \o -> do
          StableEncoding defPactId <- o .: "pactId"
          step <- o .: "step"
          rollback <- o .: "rollback"
          StableEncoding msgData <- o .: "data"
          maybeProof <- o .:? "proof"
          pure $ ContMsg defPactId step rollback msgData maybeProof
          -- ContMsg <$> o .: "pactId" <*> o .: "step" <*> o .: "rollback" <*> o .: "data"
          -- <*> o .: "proof"
  {-# INLINE parseJSON #-}

instance J.Encode ContMsg where
  build o = J.object
    [ "proof" J..= _cmProof o
    , "data" J..= StableEncoding (_cmData o)
    , "pactId" J..= StableEncoding (_cmPactId o)
    , "rollback" J..= _cmRollback o
    , "step" J..= J.Aeson (_cmStep o)
    ]
  {-# INLINE build #-}

makePrisms ''PactRPC
makeLenses ''ExecMsg
makeLenses ''ContMsg
