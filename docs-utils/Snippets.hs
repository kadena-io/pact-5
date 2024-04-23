{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Snippets where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text             as Text
import System.Process (callCommand)
import Control.Monad (filterM, foldM, forM_ , forM)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeExtension)
import qualified Data.Text.IO as TIO
import Data.List (findIndices)
import Data.Map (insertWith)
import Control.Concurrent.Async (mapConcurrently)
import Control.Lens.TH


type Repo = (Text , FilePath)

-- first field is url of repository, secound is relative path of folder of our interest

data Snippet = Snippet 
  { _sLines :: (Int , Int)
  , _sRepo :: Repo
  , _sFilePath :: FilePath
  , _sContent :: Text
 } deriving (Eq, Show)


$(makeLenses ''Snippet)

cloneRepo :: FilePath -> Repo -> IO ()
cloneRepo basePath (url, _) =
   callCommand $ "git -C " ++ basePath  ++ " pull || git clone " ++ Text.unpack url ++ " " ++ basePath


getPactOrReplFiles :: FilePath -> IO [FilePath]
getPactOrReplFiles dir = do
  entries <- listDirectory dir
  let absEntries = map (dir </>) entries             -- get absolute paths of entries
  (dirs, files) <- partitionM doesDirectoryExist absEntries    -- separate directories and files
  let pactOrReplFiles = filter (\f -> takeExtension f `elem` [".pact", ".repl"]) files  -- filter for .pact or .repl files
  filesInSubdirs <- fmap concat (mapM getPactOrReplFiles dirs)  -- recursively look in subdirectories
  return (pactOrReplFiles ++ filesInSubdirs)

-- Helper function to partition a list based on a predicate
partitionM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM p = foldM (\(ts, fs) x -> do isT <- p x; return $ if isT then (x:ts, fs) else (ts, x:fs)) ([],[])


searchPhraseInFile :: Text -> FilePath -> IO [Int]
searchPhraseInFile phrase file = do
    content <- TIO.readFile file
    let lines = Text.lines content
    return $ findIndices (Text.isInfixOf phrase) lines

createSnippets :: [Text] -> FilePath -> FilePath -> Repo -> IO (Map Text [Snippet])
createSnippets phrases file relP repo = foldM addPhraseSnippets Map.empty phrases
  where
    addPhraseSnippets :: Map Text [Snippet] -> Text -> IO (Map Text [Snippet])
    addPhraseSnippets map phrase = do
        lineNums <- searchPhraseInFile phrase file
        foldM (addLineSnippet phrase) map lineNums

    addLineSnippet :: Text -> Map Text [Snippet] -> Int -> IO (Map Text [Snippet])
    addLineSnippet phrase map lineNum = do
        snippet <- createSnippetFromFile file relP repo (lineNum-2) (lineNum+2)
        return $ insertWith (++) phrase [snippet] map
            
createSnippetFromFile :: FilePath -> FilePath -> Repo -> Int -> Int -> IO Snippet
createSnippetFromFile file relP repo start end = do
    content <- TIO.readFile file
    let lines = Text.lines content
    let snippetLines = take (end-start+1) (drop start lines)
    return Snippet {_sLines = (start, end), _sRepo = repo, _sFilePath = relP, _sContent = Text.unlines snippetLines}

findSnippets :: [Text] -> [Repo] -> IO (Map Text [Snippet])
findSnippets phrases repos = do
  repoSnippets <- mapConcurrently (findSnippetsInRepo phrases) repos
  return $ Map.unionsWith (++) repoSnippets

findSnippetsInRepo :: [Text] -> Repo -> IO (Map Text [Snippet])
findSnippetsInRepo phrases repo@(url, relativePath) = do
    let repoPath = "/tmp" </> "temprepo" ---Text.unpack url     -- assuming '/tmp' as base path
    cloneRepo repoPath repo
    files <- getPactOrReplFiles (repoPath </> relativePath)
    fileSnippetMaps <- mapM (\file -> createSnippets phrases file (drop (length (repoPath </> relativePath)) file) repo) files
    return $ Map.unionsWith (++) fileSnippetMaps


prettyPrint :: Map Text [Snippet] -> IO ()
prettyPrint m = forM_ (Map.toList m) $ \(phrase, snippets) -> do
    putStrLn $ "Phrase: " ++ (Text.unpack phrase)
    putStrLn $ "Number of snippets: " ++ (show $ length snippets)
    forM_ snippets $ \snippet -> do
        let (url, path) = _sRepo snippet
        putStrLn $ "  Repository: " ++ (Text.unpack url) ++ " " ++ path
        let (startLine, endLine) = _sLines snippet
        putStrLn $ "  File: " ++ (show $ _sFilePath snippet)
        putStrLn $ "  Lines: " ++ (show startLine) ++ " - " ++ (show endLine)
        putStrLn $ "  Content:\n" ++ (unlines . map ("    " ++) . lines . Text.unpack $ _sContent snippet)


reposToIncludeInStubsPrompt :: [Repo]
reposToIncludeInStubsPrompt =
   [("https://github.com/kadena-io/chainweb-node.git", "pact")]



snippetsTest :: IO ()
snippetsTest =
    findSnippets ["with-read" , "enforce"] repos >>= prettyPrint 

  where
    repos =
       [ --("https://github.com/kadena-io/chainweb-node.git", "pact")
         ("https://github.com/eckoDAO-org/kda-bonding.git", "pact")
       ] 
