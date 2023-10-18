{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Pact.Core.Pacts.Types
 ( PactId(..)
 , PactContinuation(..)
 , pcName, pcArgs
 , PactStep(..)
 , psStep, psRollback, psPactId, psResume
 , PactExec(..)
 , peStepCount, peYield, peStep, peContinuation, peStepHasRollback, pePactId
 , peNestedPactExec
 , Yield(..)
 , hashToPactId
 , mkNestedPactId
 ) where

-- Todo: yield
import Data.Text(Text)
import Pact.Core.Pretty
import Control.Lens
import Data.Map.Strict (Map)
import Pact.Core.PactValue
import Pact.Core.Names
import Pact.Core.Hash (Hash, hashToText, pactHash)
import qualified Data.Text.Encoding as T
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8

newtype PactId
  = PactId Text
  deriving (Eq,Ord,Show,Pretty)

hashToPactId :: Hash -> PactId
hashToPactId = PactId . hashToText

data PactContinuation name v
  = PactContinuation
  { _pcName :: name
  , _pcArgs :: [v]
  } deriving (Eq, Show)

makeLenses ''PactContinuation

encodePactContinuation :: PactContinuation FullyQualifiedName PactValue -> ByteString
encodePactContinuation = BS8.pack . show

mkNestedPactId :: PactContinuation FullyQualifiedName PactValue -> PactId -> PactId
mkNestedPactId pc (PactId parent) =
  hashToPactId (pactHash (T.encodeUtf8 parent <> ":" <> encodePactContinuation pc)) -- TODOL add pc

-- | `Yield` representing an object
newtype Yield
  = Yield {unYield :: Map Field PactValue}
  deriving (Show)

-- | Internal representation of pacts
data PactExec
  = PactExec
  { _peStepCount :: Int
  , _peYield :: Maybe Yield
  , _peStep :: Int
  , _pePactId :: PactId
  , _peContinuation :: PactContinuation FullyQualifiedName PactValue
  , _peStepHasRollback :: Bool
  , _peNestedPactExec :: Map PactId PactExec
  } deriving Show

makeLenses ''PactExec

data PactStep = PactStep
  { _psStep :: !Int
  , _psRollback :: !Bool
  , _psPactId :: !PactId
  , _psResume :: !(Maybe Yield)
  } deriving Show

makeLenses ''PactStep

