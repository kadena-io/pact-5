{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Pact.Core.Test.DocsTests (tests) where

import Control.Monad
import Control.Lens
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import PropertyMatchers ((?))
import qualified PropertyMatchers as P
import Pact.Core.Builtin
import Pact.Core.Repl.BuiltinDocs
import Pact.Core.Repl.BuiltinDocs.Internal
import System.Directory
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit

tests :: IO TestTree
tests = do
  let baseDir = "docs/builtins"
  cats <- listDirectory baseDir
  let getWithFullPath ((baseDir </>) -> dir) = fmap (dir </>) <$> listDirectory dir
  allBuiltinFiles <- join <$> traverse getWithFullPath cats
  pure $
    testGroup
      "Documentation Tests"
      [ docsExistsTest (T.pack . takeBaseName <$> allBuiltinFiles)
      , testGroup "Documentation is valid markdown" (docCompilesAsValidMarkdown <$> allBuiltinFiles) ]

docsExistsTest :: [Text] -> TestTree
docsExistsTest b = testCase "Builtins should have docs" $ do
  let normBuiltins = builtinToNormalizedName <$> replCoreBuiltinNames
  let diff = (S.fromList normBuiltins `S.difference` excluded) `S.difference` S.fromList b
  assertEqual "Missing builtins should be empty" S.empty diff
  where
    excluded = S.fromList ["env-stackframe"]

docCompilesAsValidMarkdown :: FilePath -> TestTree
docCompilesAsValidMarkdown file = testCase (takeBaseName file <> " has valid markdown") $ do
  d <- T.readFile file
  renderBuiltinDoc d >>= P.match _Right ? P.succeed
