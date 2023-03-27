{-# language OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (forkIO, killThread)
import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import Language.LSP.Test
import Pact.Core.LSP.Server (runWith)
import System.IO
import System.Process
import Test.Hspec
import Language.LSP.Types (Position(..), Diagnostic (..), Range (..),
                           DiagnosticSeverity (DsError))
import qualified Colog.Core as L

withLSPServer :: ((Handle, Handle) -> IO ()) -> IO ()
withLSPServer f = do
  (inR, inW) <- createPipe
  (outR, outW) <- createPipe

  bracket
    (forkIO $ runWith inR outW (L.LogAction (const (pure ()))))
    killThread
    (const (f (inW, outR)))

defConfig :: SessionConfig
defConfig = defaultConfig{logMessages=False, logStdErr=False}

main :: IO ()
main = hspec $ around withLSPServer $ do
  describe "diagnostic" $ do
    it "should send no diagnostic" $ \(hin, hout) ->
      runSessionWithHandles hin hout defConfig fullCaps "pact-core-lsp/test/data/" $ do
        _ <- openDoc "no-diagnostic.pact" "pact"
        noDiagnostics
        _ <- openDoc "no-diagnostic.repl" "pact"
        noDiagnostics

    it "should report parsing error" $ \(hin,hout) ->
      runSessionWithHandles hin hout defConfig fullCaps "pact-core-lsp/test/data/" $ do
        _ <- openDoc "parsing-error.pact" "pact"
        diags <- waitForDiagnostics
        liftIO $ diags `shouldBe`
          [Diagnostic (Range (Position 0 13) (Position 0 18))
           (Just DsError)
           Nothing (Just "Parser")
           "No such type: hallo"
           Nothing Nothing]
