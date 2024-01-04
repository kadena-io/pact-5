-- | 

module Pact.Core.Test.LanguageServer
  ( tests
  ) where

import Language.LSP.Test
import Language.LSP.Protocol.Types

import Test.Tasty
import Test.Tasty.HUnit
import Data.Aeson.KeyMap as A
import Data.Aeson as A

tests :: TestTree
tests = testGroup "Pact LSP"
  [ testCase "Syntax error" $ do
      diags <- getDiagnostics "syntax-failure.repl"
      assertBool "Should be one erro" (length diags == 1)
  ]


cfg :: SessionConfig
cfg = defaultConfig
  { messageTimeout = 5
  --, ignoreConfigurationRequests = True
  , lspConfig = A.singleton "pact" A.Null
  }

getDiagnostics :: String -> IO [Diagnostic]
getDiagnostics f = runSessionWithConfig cfg "cabal exec repl -- --lsp" fullCaps "pact-core-tests/pact-tests-lsp" $ do
    _doc <- openDoc f "pact"
    waitForDiagnostics
