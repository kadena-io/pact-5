module Main where


import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad(when,forM)
import Data.IORef
import Data.Default
import Data.Foldable(traverse_)
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
import Pact.Core.Environment
import Pact.Core.Builtin
import Pact.Core.Errors
import Pact.Core.Serialise

import Pact.Core.Test.ReplTests


main :: IO ()
main = do
  files <- reverse <$> corpusFiles
  defaultMain $ testGroup "ReplTests"
    [ testGroup "in-memory db:bigstep" (runCorpusFileReplTest interpretReplProgram <$> files)
        -- testGroup "sqlite db:bigstep" (runCorpusFileReplTest interpretReplProgram <$> files)
    -- , testGroup "in-memory db:smallstep" (runCorpusFileReplTest interpretReplProgramSmallStep <$> files)
    -- testGroup "sqlite db:smallstep" (runCorpusFileReplTest interpretReplProgramSmallStep <$> files)
    ]


main' :: IO ()
main' = do
  files <- reverse <$> corpusFiles
  results <- forM (zip [0..] files) $ \(i , file) -> do
    putStrLn $ show i ++ " : " ++ file
    pdb <- mockPactDb serialisePact_repl_spaninfo
    src <- T.readFile file
    runReplTest' pdb file src interpretReplProgram
  let passedTests = filter snd results
  putStrLn $ "Passed tests: " ++ show (length passedTests)
  print $ fst <$> passedTests

replCmdAll :: IO ()
replCmdAll = do
  files <- sort <$> corpusFiles
  T.writeFile "/Users/marcin/corpus.repl" $
    T.unlines ((T.pack . (\f -> "(load \"" ++ f ++ "\")")) <$>
                 (filter (\f -> "06.pact" == reverse (take 7 (reverse f))) files) )

-- tests :: IO TestTree
-- tests = do
--   files <- corpusFiles
--   pure $ testGroup "ReplTests"
--     [ testGroup "in-memory db:bigstep" (runFileReplTest interpretReplProgram <$> files)
--     , testGroup "sqlite db:bigstep" (runFileReplTestSqlite interpretReplProgram <$> files)
--     , testGroup "in-memory db:smallstep" (runFileReplTest interpretReplProgramSmallStep <$> files)
--     , testGroup "sqlite db:smallstep" (runFileReplTestSqlite interpretReplProgramSmallStep <$> files)
--     ]

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

-- runFileReplTest :: Interpreter -> TestName -> TestTree
-- runFileReplTest interp file = testCase file $ do
--   pdb <- mockPactDb serialisePact_repl_spaninfo
--   src <- T.readFile (replTestDir </> file)
--   runReplTest pdb file src interp

runCorpusFileReplTest :: Interpreter -> TestName -> TestTree
runCorpusFileReplTest interp file = testCase file $ do
  pdb <- mockPactDb serialisePact_repl_spaninfo
  src <- T.readFile file
  runReplTest pdb file src interp
-- /Users/marcin/pact-corpus/0003313653-12.pact


-- runCorpusFileReplTest' :: Interpreter -> TestName -> TestTree
-- runCorpusFileReplTest' interp file = testCase file $ do
--   pdb <- mockPactDb serialisePact_repl_spaninfo
--   src <- T.readFile file
--   runReplTest' pdb file src interp
-- -- /Users/marcin/pact-corpus/0003313653-12.pact
