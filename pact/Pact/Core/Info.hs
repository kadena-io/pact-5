module Pact.Core.Info
 ( SpanInfo(..)
 , combineSpan
 , isWithin
 ) where

import Data.Default
import GHC.Generics

data SpanInfo
  = SpanInfo
  { _liStartLine   :: !Int
  , _liStartColumn :: !Int
  , _liEndLine     :: !Int
  , _liEndColumn   :: !Int
  } deriving (Eq, Ord, Show, Generic)

instance Default SpanInfo where
  def = SpanInfo 0 0 0 0

-- | Combine two Span infos
-- and spit out how far down the expression spans.
combineSpan :: SpanInfo -> SpanInfo -> SpanInfo
combineSpan (SpanInfo l1 c1 _ _) (SpanInfo _ _ l2 c2) =
  SpanInfo l1 c1 l2 c2

isWithin :: (Int, Int) -> SpanInfo -> Bool
isWithin (l, c) (SpanInfo sl sc el ec)
  -- if it's a one-line span, just check the column:
  | sl == el = l == sl && c >= sc && c <= ec
  -- otherwise, it's a multiline span, so if the line is the first line of the span, check the column:
  | l == sl = c >= sc
  -- similarly for the last line of the span:
  | l == el = c <= ec
  -- otherwise the position could only be between the starting and ending lines
  -- or outside the span, and columns need not be checked:
  | otherwise = l > sl && l < el
