{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveTraversable #-}

module Pact.Core.Info
 ( SpanInfo(..)
 , combineSpan
 , NoInfo(..)
 , Located(..)
 ) where

import Data.Default
import GHC.Generics
import Control.DeepSeq (NFData)
import Pact.Core.Pretty

-- | A simple data type that signals we intentionally
--   have emptied out node annotations
data NoInfo
  = NoInfo
  deriving (Eq, Show, Generic)

instance NFData NoInfo

-- | An info span that contains line location data
data SpanInfo
  = SpanInfo
  { _liStartLine   :: !Int
  , _liStartColumn :: !Int
  , _liEndLine     :: !Int
  , _liEndColumn   :: !Int
  } deriving (Eq, Show, Generic, NFData)

instance Default SpanInfo where
  def = SpanInfo 0 0 0 0

instance Pretty SpanInfo where
  pretty SpanInfo{..} =
    pretty _liStartLine <> ":" <> pretty _liStartColumn
    <> "-" <> pretty _liEndLine <> ":" <> pretty _liEndColumn

    

-- | Combine two Span infos
--   and spit out how far down the expression spans.
combineSpan :: SpanInfo -> SpanInfo -> SpanInfo
combineSpan (SpanInfo l1 c1 _ _) (SpanInfo _ _ l2 c2) =
  SpanInfo l1 c1 l2 c2

data Located i a
  = Located
  { _locLocation :: i
  , _locElem :: a }
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
