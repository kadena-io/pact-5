module Main where


import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad(when,forM)
import Data.IORef
import Data.Default
import Data.Foldable(traverse_)
import Data.Maybe
import System.Directory
import System.FilePath

import Data.List

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Pact.Core.Gas
import Pact.Core.Literal
import Pact.Core.Persistence.MockPersistence

import Pact.Core.Repl.Utils
import Pact.Core.Persistence (PactDb)
import Pact.Core.Persistence.SQLite (withSqlitePactDb)

import Pact.Core.Info (SpanInfo)
import Pact.Core.Compile
import Pact.Core.Repl.Compile
import Pact.Core.PactValue
import Pact.Core.Builtin
import Pact.Core.Errors
import Pact.Core.Serialise
import Pact.Core.Pretty

import Pact.Core.Test.ReplTests


main :: IO ()
main = do
  files <- reverse <$> corpusFiles
  defaultMain $ testGroup "ReplTests"
    [ -- testGroup "in-memory db:bigstep" (runCorpusFileReplTest interpretReplProgram <$> files)
      testGroup "in-memory db:bigstep" ((runCorpusFileParseTest) <$> files)
        -- testGroup "sqlite db:bigstep" (runCorpusFileReplTest interpretReplProgram <$> files)
    -- , testGroup "in-memory db:smallstep" (runCorpusFileReplTest interpretReplProgramSmallStep <$> files)
    -- testGroup "sqlite db:smallstep" (runCorpusFileReplTest interpretReplProgramSmallStep <$> files)
    ]


saveInvalid :: IO ()
saveInvalid = do
  files <- reverse <$> corpusFiles
  errs <- catMaybes <$> forM files getCorpusFileParseError

  T.writeFile "/Users/marcin/badList"
    $ T.unlines (
          T.pack ("uniqe sources: " ++ show (length files))
        : T.pack ("failed: " ++ show (length errs))
        : ""
        :  errs)

replCmdAll :: IO ()
replCmdAll = do
  files <- sort <$> corpusFiles
  T.writeFile "/Users/marcin/corpus.repl" $
    T.unlines ((T.pack . (\f -> "(load \"" ++ f ++ "\")")) <$> files
                 -- (filter (\f -> "06.pact" == reverse (take 7 (reverse f))) files)
              )

corpusDir :: IO [Char]
corpusDir = do
  ud <- getUserDocumentsDirectory
  return $ ud </> "pact-corpus"

corpusFiles' :: IO [FilePath]
corpusFiles' = pure $ ["/Users/marcin/pact-corpus/0003313653-12.pact"]
  
corpusFiles :: IO [FilePath]
corpusFiles = do
  cd <- corpusDir
  map (\x -> cd </> x) <$> (filter (isExtensionOf "pact") <$> getDirectoryContents cd)


runCorpusFileParseTest :: TestName -> TestTree
runCorpusFileParseTest file = testCase file $ do
  src <- T.readFile file
  case (_parseOnly src) of
    Left e -> let
      rendered = replError (SourceCode file src) e
      in assertFailure (T.unpack rendered)
      
    Right _ -> pure () 

getCorpusFileParseError :: TestName -> IO (Maybe T.Text)
getCorpusFileParseError file = do
  src <- T.readFile file
  return $ case (_parseOnly src) of
    Left e -> let rendered = replError (SourceCode file src) e
      in (Just rendered)
      
    Right _ -> Nothing  
