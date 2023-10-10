{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE InstanceSigs #-}


module Pact.Core.Environment
 ( EvalEnv(..)
 , eeMsgSigs
 , eePactDb
 , eeHash
--  , eeWarning
 , eeMsgBody
 , PactState(..)
 , psLoaded
 ) where

import Control.Lens
-- import Data.Text(Text)
import Data.Set(Set)
import Data.Map.Strict(Map)
-- import Data.IORef(IORef)

import Pact.Core.Persistence
import Pact.Core.Capabilities
-- import Pact.Core.Gas
import Pact.Core.Guards
import Pact.Core.PactValue
import Pact.Core.Hash
-- import Pact.Core.Names


-- From pact
-- | All of the types included in our evaluation environment.
data EvalEnv b i
  = EvalEnv
  { _eeMsgSigs :: Map PublicKeyText (Set FQCapToken)
  , _eePactDb :: PactDb b i
  , _eeMsgBody :: EnvData PactValue
  -- Todo: `PactWarning`
  -- , _eeWarning :: IORef (Set Text)
  , _eeHash :: Hash
--  , _eePactStep :: !(Maybe PactStep)
  --   _cekGas :: IORef Gas
  -- , _cekEvalLog :: IORef (Maybe [(Text, Gas)])
  -- , _ckeData :: EnvData PactValue
  }

makeLenses ''EvalEnv

newtype PactState b i
  = PactState
  { _psLoaded :: Loaded b i
  }

makeLenses ''PactState
