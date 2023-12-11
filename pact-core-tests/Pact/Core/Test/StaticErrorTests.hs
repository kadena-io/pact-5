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
  [ ("no_bind_body.pact", isDesugarError _EmptyBindingBody)
  , ("defpact_last_step_rollback.pact", isDesugarError _LastStepWithRollback)
  , ("interface_defcap_meta_impl.pact", isDesugarError _ImplementationError)
  , ("enforce-one_no_list.pact", isDesugarError _InvalidSyntax)
  -- TODO unable to trigger Desugar.hs:336/344 in `desugarDefun`, parser gets there first
  -- , ("defun_outside_module.pact", isDesugarError _NotAllowedOutsideModule)
  ]

tests :: TestTree
tests =
  testGroup "CoreStaticTests" (go <$> staticTests)
  where
  go (fp, p) = testCase fp $ do
    srcText <- T.readFile (staticTestDir </> fp)
    runStaticTest fp srcText p
