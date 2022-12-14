module Pact.Core.Test.Repl where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Lens
import Control.Monad(when)
import Data.IORef
import Data.ByteString(ByteString)
import Data.Foldable(traverse_)
import Data.Text(Text)
import Data.Text.Encoding(decodeUtf8)
import System.Directory
import System.FilePath

import qualified Data.Text as T
import qualified Data.ByteString as B

import Pact.Core.Gas
import Pact.Core.Errors
import Pact.Core.Literal
import Pact.Core.Persistence
import Pact.Core.Info

import Pact.Core.Untyped.Eval.Runtime

import Pact.Core.Repl.Runtime
import Pact.Core.Repl.Runtime.ReplBuiltin
import Pact.Core.Repl.Utils
import Pact.Core.Repl.Compile

replError
  :: ReplSource
  -- ^ The actual source of text to slice from
  -> Text
  -- ^ The rendered error
  -> LineInfo
  -- ^ The source loc of the error
  -> Text
replError (ReplSource file src) errRender pei =
  let srcLines = T.lines src
      slice = withLine (_liLine pei) $ take (max 1 (_liSpan pei)) $ drop (_liLine pei) srcLines
      colMarker = padBlankr 5 <> "| " <> T.replicate (_liColumn pei - 1) " " <> "^"
      fileErr = file <> ":" <> T.pack (show (_liLine pei)) <> ":" <> T.pack (show (_liColumn pei)) <> ": "
  in T.unlines ([fileErr <> errRender] ++ slice ++ [colMarker])
  where
  padr n r = n <> T.replicate (r - T.length n) " "
  padBlankr r = padr "" r
  withLine st lns = zipWith (\i e -> padr (T.pack (show i)) 5 <> "| " <> e) [st ..] lns


tests :: IO TestTree
tests = do
  files <- replTestFiles
  pure $ testGroup "Core repl tests" (runFileReplTest <$> files)


replTestDir :: [Char]
replTestDir = "pact-core-tests" </> "pact-tests"

replTestFiles :: IO [FilePath]
replTestFiles = do
  filter (\f -> isExtensionOf "repl" f || isExtensionOf "pact" f) <$> getDirectoryContents replTestDir

runFileReplTest :: TestName -> TestTree
runFileReplTest file = testCase file $ B.readFile (replTestDir </> file) >>= runReplTest file

runReplTest :: FilePath -> ByteString -> Assertion
runReplTest file src = do
  gasRef <- newIORef (Gas 0)
  gasLog <- newIORef Nothing
  pdb <- mockPactDb
  let rstate = ReplState
            { _replFlags =  mempty
            , _replLoaded = mempty
            , _replPactDb = pdb
            , _replGas = gasRef
            , _replEvalLog = gasLog }
  stateRef <- newIORef rstate
  runReplT stateRef (compileAndInterpretProgram src) >>= \case
    Left e -> let
      errRender = renderPactError e
      rendered = replError (ReplSource (T.pack file) (decodeUtf8 src)) errRender (view peInfo e)
      in assertFailure (T.unpack rendered)
    Right output -> traverse_ ensurePassing output
  where
  ensurePassing = \case
    InterpretLog _ ->  pure ()
    InterpretValue v i -> case v of
      VLiteral (LString msg) -> do
        let render = replError (ReplSource (T.pack file) (decodeUtf8 src)) msg i
        when (T.isPrefixOf "FAILURE:" msg) $ assertFailure (T.unpack render)
      _ -> pure ()

