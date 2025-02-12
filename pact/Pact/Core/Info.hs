{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE InstanceSigs #-}

module Pact.Core.Info
 ( SpanInfo(..)
 , combineSpan
 , NoInfo(..)
 , Located(..)
 , sliceFromSource
 , sliceFromSourceLines
 , LineInfo(..)
 , spanInfoToLineInfo
 , FileLocSpanInfo(..)
 , HasSpanInfo(..)
 ) where

import Control.Lens
import Data.Default
import Data.Text(Text)
import Data.List(intersperse)
import qualified Data.Text as T
import GHC.Generics
import Control.DeepSeq (NFData)
import Pact.Core.Pretty

import qualified Pact.JSON.Encode as J
import qualified Pact.JSON.Decode as JD

-- | A simple data type that signals we intentionally
--   have emptied out node annotations
data NoInfo
  = NoInfo
  deriving (Eq, Show, Generic)

instance NFData NoInfo

newtype LineInfo
  = LineInfo { _lineInfo :: Int }
  deriving newtype (Eq, Show, NFData)

instance Default LineInfo where
  def = LineInfo 0

instance JD.FromJSON LineInfo where
  parseJSON = JD.withObject "LineInfo" $ \o ->
    LineInfo <$> o JD..: "sourceLine"

instance J.Encode LineInfo where
  build (LineInfo i) = J.object [ "sourceLine" J..= J.Aeson i]

instance Pretty LineInfo where
  pretty (LineInfo i) = "line:" <+> pretty i

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

spanInfoToLineInfo :: SpanInfo -> LineInfo
spanInfoToLineInfo = LineInfo . _liStartLine

data FileLocSpanInfo
  = FileLocSpanInfo
  { _flsiFile :: !String
  , _flsiSpan :: !SpanInfo
  } deriving (Eq, Show, Generic, NFData)

-- | Combine two Span infos
--   and spit out how far down the expression spans.
combineSpan :: SpanInfo -> SpanInfo -> SpanInfo
combineSpan (SpanInfo l1 c1 _ _) (SpanInfo _ _ l2 c2) =
  SpanInfo l1 c1 l2 c2

sliceFromSourceLines :: [Text] -> SpanInfo -> Text
sliceFromSourceLines codeLines (SpanInfo startLine startCol endLine endCol) =
    -- Drop until the start line, and take (endLine - startLine). Note:
    -- Span info locations are absolute, so `endLine` is not relative to start line, but
    -- relative to the whole file.
    --
    -- Note: we take `end - start + 1` since end is inclusive.
    let lineSpan = take (max 1 (endLine - startLine + 1)) $ drop startLine codeLines
    -- Note: we can't use `unlines` here. it adds an extra terminating newline
    in T.concat $ intersperse "\n" (over _head (T.drop startCol) . over _last (T.take endCol) $ lineSpan)

sliceFromSource :: Text -> SpanInfo -> Text
sliceFromSource t si = sliceFromSourceLines (T.lines t) si

data Located i a
  = Located
  { _locLocation :: i
  , _locElem :: a }
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeClassy ''SpanInfo

instance HasSpanInfo FileLocSpanInfo where
  spanInfo = lens _flsiSpan (\s i -> s { _flsiSpan = i })

instance Pretty FileLocSpanInfo where
  pretty (FileLocSpanInfo f s) = pretty f <> ":" <> pretty s

instance Default FileLocSpanInfo where
  def = FileLocSpanInfo "" def
