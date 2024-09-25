module Pact.Core.Test.DocsTests (tests) where

import Control.Monad
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Pact.Core.Builtin
import Pact.Core.Repl.BuiltinDocs.Internal
import System.Directory
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit

tests :: IO TestTree
tests = do
  let baseDir = "docs/builtins"
  cats <- listDirectory baseDir
  builtinWithDocs <- join <$> traverse (\cat -> map (T.pack . takeBaseName) <$> listDirectory (baseDir </> cat)) cats
  pure $
    testGroup
      "Documentation Tests"
      [ docsExistsTest builtinWithDocs
      ]

docsExistsTest :: [Text] -> TestTree
docsExistsTest b = testCase "Builtins should have docs" $ do
  let normBuiltins = builtinToNormalizedName <$> replCoreBuiltinNames
  let diff = (S.fromList normBuiltins `S.difference` excluded) `S.difference` S.fromList b
  assertEqual "Missing builtins should be empty" S.empty diff
  where
    excluded = S.fromList
      ["cond", "env-ask-gasmodel", "env-chain-data", "env-exec-config"
      ,"env-gaslog", "env-gasmodel-fixed", "env-milligas", "env-module-admin"
      ,"env-set-milligas", "env-stackframe", "env-verifiers", "negate"
      ,"pact-state", "print", "reset-pact-state", "rollback-tx", "show"
      ,"sig-keyset", "test-capability", "env-set-debug-flag"]
