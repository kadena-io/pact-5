-- | 

module Pact.Core.Test.LanguageServer
  ( tests
  ) where

import Language.LSP.Test
import Language.LSP.Protocol.Types

import System.Environment
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Aeson.KeyMap as A
import Data.Aeson as A

tests :: TestTree
tests = testGroup "Pact LSP"
  [ testCase "Syntax error" $ do
      diags <- getDiagnostics "syntax-failure.repl"
      assertBool "Should be one error" (length diags == 1)

  , testCase "No error" $ do
      diags <- getDiagnostics "syntax-ok.repl"
      assertBool "Should be ne error" (null diags)
  ]


cfg :: SessionConfig
cfg = defaultConfig
  { messageTimeout = 10
  , lspConfig = A.singleton "pact" A.Null
  }

getDiagnostics :: String -> IO [Diagnostic]
getDiagnostics f = do
  -- We need to check if we we are buing executed via a nix build
  -- or by using cabal and run the corresponding binary.
  cmd <- lookupEnv "PACT_CORE_NIXBUILD" >>= \case
    Just p -> pure (p <> " --lsp")
    Nothing -> pure "cabal exec repl -- --lsp"

  runSessionWithConfig cfg cmd fullCaps "pact-core-tests/pact-tests-lsp" $ do
    _doc <- openDoc f "pact"
    waitForDiagnostics

