module Pact.Core.Test.DocsTests (tests) where

import Control.Monad
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Pact.Core.Builtin
import Paths_pact_tng
import System.Directory
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit

tests :: IO TestTree
tests = do
  baseDir <- (</> "docs/builtins") <$> getDataDir
  cats <- listDirectory baseDir
  builtinWithDocs <- join <$> traverse (\cat -> map (T.pack . takeBaseName) <$> listDirectory (baseDir </> cat)) cats
  pure $
    testGroup
      "Documentation Tests"
      [ docsExistsTest builtinWithDocs
      ]

docsExistsTest :: [Text] -> TestTree
docsExistsTest b = testCase "Builtins should have docs" $ do
  let normBuiltins = normalizeBuiltinName <$> replCoreBuiltinNames
  let diff = (S.fromList normBuiltins `S.difference` excluded) `S.difference` S.fromList b
  assertEqual "Missing builtins should be empty" S.empty diff
  where
    excluded = S.fromList
      ["cond", "env-ask-gasmodel", "env-chain-data", "env-exec-config"
      ,"env-gaslog", "env-gasmodel-fixed", "env-milligas", "env-module-admin"
      ,"env-set-milligas", "env-stackframe", "env-verifiers", "negate"
      ,"pact-state", "print", "reset-pact-state", "rollback-tx", "show"
      ,"sig-keyset", "test-capability"]

normalizeBuiltinName :: Text -> Text
normalizeBuiltinName = \case
  "!=" -> "neq"
  "&" -> "and"
  "*" -> "mult"
  "+" -> "add"
  "-" -> "sub"
  "/" -> "div"
  "<" -> "lt"
  "<=" -> "leq"
  "=" -> "eq"
  ">" -> "gt"
  ">=" -> "geq"
  "^" -> "pow"
  "and?" -> "and-q"
  "not?" -> "not-q"
  "or?" -> "or-q"
  "|" -> "bitwise-or"
  "~" -> "bitwise-reverse"
  "begin-named-tx" -> "begin-tx"
  "continue-pact-rollback-yield" -> "continue-pact"
  "continue-pact-rollback-yield-object" -> "continue-pact"
  "continue-pact-with-rollback" -> "continue-pact"
  "enforce-pact-version-range" -> "enforce-pact-version"
  "env-set-gas" -> "env-gas"
  "expect-failure-match" -> "expect-failure"
  other -> other
