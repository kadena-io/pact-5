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
import Language.LSP.Types (Position(..))
import qualified Colog.Core as L


withLSPServer :: ((Handle, Handle) -> IO ()) -> IO ()
withLSPServer f = do
  (inR, inW) <- createPipe
  (outR, outW) <- createPipe

  bracket
    (forkIO $ runWith inR outW L.logStringStderr)
    killThread
    (const (f (inW, outR)))

main :: IO ()
main = hspec $ around withLSPServer $ do
  describe "diagnostic" $ do
    -- it "should send no diagnostic" $  \(hin, hout) ->
    --   runSessionWithHandles hin hout defaultConfig fullCaps "tests/data/" $ do
    --   _ <- openDoc "test.pact" "pact"
    --   diags <- waitForDiagnostics
    --   liftIO $ diags `shouldBe` []


    describe "hover" $ do
      it "should return app" $ \(hin,hout) ->
        runSessionWithHandles hin hout defaultConfig fullCaps "pact-core-lsp/test/data/"$ do
        doc <- openDoc "hover.pact" "pact"
        hover <- getHover doc (Position 0 0)
        liftIO $ hover `shouldBe` Nothing
