{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Pact.Core.Test.StaticErrorTests(tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Lens
import Data.Text (Text)
import Data.Maybe(isJust)
import System.FilePath

import qualified Data.Text.IO as T

import Pact.Core.Builtin
import Pact.Core.Evaluate
import Pact.Core.Persistence
import Pact.Core.Environment
import Pact.Core.Errors

import Pact.Core.Test.TestPrisms

staticTestDir :: [Char]
staticTestDir = "pact-core-tests" </> "static-tests"

isDesugarError :: Prism' DesugarError a -> PactErrorI -> Bool
isDesugarError p s = isJust $ preview (_PEDesugarError . _1 . p) s

runStaticTest :: FilePath -> Text -> (PactErrorI -> Bool) -> Assertion
runStaticTest fp src predicate = do
  pdb <- mockPactDb
  let evalEnv = defaultEvalEnv pdb rawBuiltinMap
  v <- fst <$> evaluate evalEnv src
  case v of
    Left err ->
      assertBool ("Expected Error to match predicate, but got " <> show err <> " instead") (predicate err)
    Right _v -> assertFailure ("Error: Static failure test succeeded for file: " <> fp)

staticTests :: [(FilePath, PactErrorI -> Bool)]
staticTests =
  [ ("no_bind_body", isDesugarError _EmptyBindingBody)
  , ("defpact_last_step_rollback", isDesugarError _LastStepWithRollback)
  , ("interface_defcap_meta_impl", isDesugarError _ImplementationError)
  , ("enforce-one_no_list", isDesugarError _InvalidSyntax)
  -- TODO unable to trigger Desugar.hs:336/344 in `desugarDefun`, parser gets there first
  -- , ("defun_outside_module", isDesugarError _NotAllowedOutsideModule)
  -- TODO ditto in desugarDefPact
  -- , ("defpact_empty", isDesugarError _EmptyDefPact)
  -- TODO ditto in desugarDefPact
  -- , ("defpact_outside_module", isDesugarError _NotAllowedOutsideModule)
  -- TODO ditto in desugarDefCap
  -- , ("defcap_outside_module", isDesugarError _NotAllowedOutsideModule)
  , ("managed_invalid", isDesugarError _InvalidManagedArg)
  , ("interface_unannotated_defun", isDesugarError _UnannotatedReturnType)
  , ("interface_unannotated_defpact", isDesugarError _UnannotatedReturnType)
  , ("import_invalid_set", isDesugarError _InvalidImports)
  ]

tests :: TestTree
tests =
  testGroup "CoreStaticTests" (go <$> staticTests)
  where
  go (fp, p) = testCase fp $ do
    srcText <- T.readFile (staticTestDir </> (fp <> ".pact"))
    runStaticTest fp srcText p
