{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}


-- |

module Pact.Core.Test.LanguageServer
  ( tests
  ) where

import Language.LSP.Test
import Language.LSP.Protocol.Types
import Language.LSP.Protocol.Lens hiding (length, title, rename)

import Control.Lens

import System.Environment
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Golden
import qualified Data.Aeson.KeyMap as A
import Data.Aeson as A
import Control.Monad.IO.Class
import Pact.Core.Builtin
import Data.Maybe (isJust)
import qualified Data.Map.Strict as M
import Pact.Core.Repl.BuiltinDocs
import Data.Either (isLeft)
import Data.Text.Encoding
import qualified Data.ByteString.Lazy as LBS

tests :: TestTree
tests = testGroup "Pact LSP"
  [ testGroup "Diagnostics" diagnosticTests
  , testGroup "Hovers" [
      testGroup "Builtin docs" builtinHoverTests,
      testGroup "User docs" userDocHoverTests,
      testGroup "Overloaded builtins" overloadBuiltinHoverTests
      ]
  , testGroup "Definition" definitionRequestTests
  , renameTests
  ]

diagnosticTests :: [TestTree]
diagnosticTests
  = [ testCase "Failure Diagnostic" $ runPactLSP $ do
        _ <- openDoc "syntax-failure.repl" "pact"
        diags <- waitForDiagnostics
        liftIO $ assertBool "Should be an error" (length diags == 1)

    , testCase "No Diagnostic" $ runPactLSP $ do
        _ <- openDoc "syntax-ok.repl" "pact"
        diags <- waitForDiagnostics
        liftIO $ assertBool "Should have no diagnostics" (null diags)
  ]

userDocHoverTests :: [TestTree]
userDocHoverTests = [hoverTest "defun my-fun" 11 "This is my-fun documentation"]
  where
    title b = "Get hover docs for: "  <> b
    hoverTest b l expected = testCase (title b) $ runPactLSP $ do
      doc <- openDoc "userdocs-hover.repl" "pact"
      h <- getHover doc (Position l 2)
      liftIO $ do
        assertBool "Return hover information" (isJust h)
        let Just hov' = h
        assertEqual "Match builtin docs" (view contents hov') (InL $ MarkupContent MarkupKind_PlainText expected)


definitionRequestTests :: [TestTree]
definitionRequestTests = [defTest "my-fun-1" (Position 13 2) (Range (Position 5 2) (Position 6 6))
                         ,defTest "my-fun-1 (inside my-fun-2)" (Position 9 6) (Range (Position 5 2) (Position 6 6))]
  where
    title b = "Get toplevel definition of: "  <> b
    defTest b pos expected = testCase (title b) $ runPactLSP $ do
      doc <- openDoc "definition-request.repl" "pact"
      _ <- waitForDiagnostics
      def <- toEither <$> getDefinitions doc pos
      liftIO $ do
        assertBool "Return definition position" (isLeft def)
        let Left defPos = def
        assertEqual "Match position" defPos (Definition . InL $ Location (view uri doc) expected)


builtinHoverTests :: [TestTree]
builtinHoverTests
  = [ hoverTest (RBuiltinWrap CoreNeq) 5
    , hoverTest (RBuiltinWrap CoreBitwiseAnd) 8
    , hoverTest (RBuiltinWrap CoreMultiply) 11
    , hoverTest (RBuiltinWrap CoreAdd) 14
    , hoverTest (RBuiltinWrap CoreSub) 17
    , hoverTest (RBuiltinWrap CoreDivide) 20
    , hoverTest (RBuiltinWrap CoreLEQ) 26
    , hoverTest (RBuiltinWrap CoreEq) 29
    , hoverTest (RBuiltinWrap CoreGT) 32
    , hoverTest (RBuiltinWrap CoreGEQ) 35
    ]
  where
    title b = "Get hover docs for: "  <> show (replCoreBuiltinToUserText b)
    hoverTest b l = testCase (title b) $ runPactLSP $ do
      doc <- openDoc "builtin-hover.repl" "pact"
      h <- getHover doc (Position l 2)
      liftIO $ do
        assertBool "Return hover information" (isJust h)
        let
          Just hov' = h
          Just expectedDocs =  M.lookup (replCoreBuiltinToUserText b) builtinDocs
        assertEqual "Match builtin docs" (view contents hov') (InL $ MarkupContent MarkupKind_PlainText expectedDocs)

overloadBuiltinHoverTests :: [TestTree]
overloadBuiltinHoverTests
  = hoverTest <$> [(CoreEnumerate, CoreEnumerateStepN, 14)
                  ,(CoreSelect, CoreSelectWithFields, 18)
                  ,(CoreSort, CoreSortObject, 22)
                  ,(CoreRound, CoreRoundPrec, 26)
                  ,(CoreCeiling, CoreCeilingPrec, 30)
                  ,(CoreFloor, CoreFloorPrec, 34)
                  ,(CoreStrToInt, CoreStrToIntBase, 38)
                  ,(CoreReadMsg, CoreReadMsgDefault, 42)
                  ,(CoreDefineKeySet, CoreDefineKeysetData, 46)
                  ,(CoreYield, CoreYieldToChain, 50)]
    where
      title b ob = "Check hover docs for overload: "
        <> show (coreBuiltinToText b) <> " / " <> show (coreBuiltinToText ob)
      hoverTest (b, ob, p) = testCase (title b ob) $ runPactLSP $ do
        doc <- openDoc "builtin-overloads-hover.repl" "pact"
        h1 <- getHover doc (Position p 8)
        h2 <- getHover doc (Position (succ p) 8)
        liftIO $ do
          let
            Just h1' = h1
            Just h2' = h2
          assertEqual "Match builtin docs" (view contents h1') (view contents h2')


renameTests :: TestTree
renameTests = goldenVsString "Renaming" "pact-tests/pact-tests-lsp/rename-test.golden" $
  runPactLSP $ do
  doc <- openDoc "rename-test.repl" "pact"
  rename doc (Position 12 7) "new-param"
  cnt <- documentContents doc
  pure $ LBS.fromStrict (encodeUtf8 cnt)

cfg :: SessionConfig
cfg = defaultConfig
  { messageTimeout = 10
  , lspConfig = A.singleton "pact" A.Null
  }


runPactLSP :: Session a -> IO a
runPactLSP f = do
  -- We need to check if we we are buing executed via a nix build
  -- or by using cabal and run the corresponding binary.
  cmd <- lookupEnv "PACT_CORE_NIXBUILD" >>= \case
    Just p -> pure (p <> " --lsp")
    Nothing -> pure "cabal exec pact -- --lsp"

  runSessionWithConfig cfg cmd fullCaps "pact-tests/pact-tests-lsp" f
