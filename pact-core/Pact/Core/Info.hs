module Pact.Core.Info
 ( SpanInfo(..)
 , combineSpan
 , SourceLocation(..)
 , spanInfoStart , spanInfoEnd
 ) where

import Data.Default
import GHC.Generics


data SourceLocation = SourceLocation
  { _clLine   :: !Int
  , _clColumn :: !Int
  } deriving (Eq, Show)

instance Ord SourceLocation where
  compare x y = compare (_clLine x , _clColumn x) (_clLine y , _clColumn y)

data SpanInfo
  = SpanInfo
  { _liStartLine   :: !Int
  , _liStartColumn :: !Int
  , _liEndLine     :: !Int
  , _liEndColumn   :: !Int
  } deriving (Eq, Show, Generic)

spanInfoStart :: SpanInfo -> SourceLocation
spanInfoStart si =  SourceLocation (_liStartLine si) (_liStartColumn si)

spanInfoEnd :: SpanInfo -> SourceLocation
spanInfoEnd si =  SourceLocation (_liEndLine si) (_liEndColumn si)

instance Default SpanInfo where
  def = SpanInfo 0 0 0 0

-- | Combine two Span infos
-- and spit out how far down the expression spans.
combineSpan :: SpanInfo -> SpanInfo -> SpanInfo
combineSpan (SpanInfo l1 c1 _ _) (SpanInfo _ _ l2 c2) =
  SpanInfo l1 c1 l2 c2

  
