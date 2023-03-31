{-# LANGUAGE RecordWildCards #-}
-- |

module Pact.Core.LSP.Completion where
import qualified Language.LSP.Types as LSP

import qualified Data.Text.Utf16.Rope as Rope
import Control.Lens (both, (%~), (&))
import qualified Data.Text as T

data CompletionContext
  = CompletionContext
  { _ccLeading :: T.Text
  , _ccPrefix  :: T.Text
  , _ccPostfix :: T.Text
  } deriving (Show, Eq)

completionCtxAt :: Rope.Rope -> LSP.Position -> CompletionContext
completionCtxAt src (LSP.Position l c) = CompletionContext{..}
  where
    (preL, postL) = Rope.splitAtLine (fromIntegral l) src & both %~ Rope.toText
    (preFocus, postFocus) = T.splitAt (fromIntegral c) postL & both %~ T.stripStart
    (firstWord, rest) = T.break (`elem` breaks)
      (T.reverse (preL <> preFocus)) & both %~ T.strip
    _ccPrefix = T.reverse firstWord
    _ccLeading = T.reverse (takeWord rest)
    _ccPostfix = takeWord postFocus
    takeWord = T.takeWhile (`notElem` breaks)
    breaks = " \t\n()"
