{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.ByteString as B

import Pact.Core.Compile
import Pact.Core.Syntax.Node.Node
import Pact.Core.Syntax.ParseTreeNode


import Control.Monad
import System.Environment (getArgs)
import System.FilePath
import System.Directory 
  (doesDirectoryExist
  , doesFileExist
  , createDirectoryIfMissing
  , listDirectory
  , copyFile)
import Path.Posix (parseAbsDir)
import Path.IO (copyDirRecur)


import System.Directory
import System.FilePath
import System.FilePath.Posix ((</>))
import System.IO (writeFile)
import qualified Data.Text as T
import qualified Data.Text.IO as TextIO


usage :: IO ()
usage = do
   putStrLn "  Command to process a file: cmd <source_file.pact> <output_dir>"
   putStrLn "  Command to process a directory: cmd <source_dir> <output_dir>"
   putStrLn "Here, "
   putStrLn "  <source_file.pact> is a .pact file to process."
   putStrLn "  <source_dir> is a directory which contains .pact files to process."
   putStrLn "  <output_dir> is the directory where the outputs will be stored."

main :: IO ()
main = 
  getArgs >>= \case
    ("-h" : _) -> do putStrLn "HTML backend for pact compiler:"
                     usage
    (target:output:_) -> do
       if (takeExtension target ==".pact")
       then processFile target output
       else do processDir target output
               createHTMLIndex output
      
    _ -> do
         putStrLn "Incorrect usage. Here are the correct ways to use this tool:"
         usage

         


folderWithAssets :: FilePath
folderWithAssets = "/Users/marcin/www-pact"


assetsToCopy :: [FilePath]
assetsToCopy = ["main.js", "style.css", "webfonts","dat"]


runRenderHTML :: FilePath -> IO B.ByteString
runRenderHTML file = do

  r <- _parseOnlyFile file 
  case r of
    Left e -> return "todo: errorhandling" --putStr (show e)
    Right r -> do

      reportASTIndentANSI "" (skipWrappers $ toAST r)
      srcContent <- B.readFile file
      createHTMLFile srcContent (toAST r)

processFile :: FilePath -> FilePath -> IO ()
processFile file output = do
  createDirectoryIfMissing True output
  content <- runRenderHTML file
  let targetFile = output </> "index.html"
  B.writeFile targetFile content
  copyAssets folderWithAssets output


processDir :: FilePath -> FilePath -> IO ()
processDir root output = do
  createDirectoryIfMissing True output
  visitPath root output

visitPath :: FilePath -> FilePath -> IO ()
visitPath root output = do
  entries <- listDirectory root
  forM_ entries $ \entry -> do
    let fullPath = root </> entry
    isDir <- doesDirectoryExist fullPath
    isFile <- doesFileExist fullPath
    when isDir $ visitPath fullPath (output </> entry)
    when (isFile && takeExtension entry == ".pact") $ do
      processPactFile root output fullPath

processPactFile :: FilePath -> FilePath -> FilePath -> IO ()
processPactFile root output pactFile = do
  let relativePath = makeRelative root pactFile
  let targetHtmlFile = replaceExtension relativePath "html" -- Corrected extension
  let outputDir = takeDirectory $ output </> targetHtmlFile
  createDirectoryIfMissing True outputDir
  content <- runRenderHTML (pactFile)
  B.writeFile (output </> targetHtmlFile) content
  copyAssets folderWithAssets outputDir -- Copy assets into the directory of the HTML file

copyAssets :: FilePath -> FilePath -> IO ()
copyAssets source dest
  | source == dest = return ()
  | otherwise = mapM_ (copyAsset source dest) assetsToCopy


copyAsset :: FilePath -> FilePath -> FilePath -> IO ()
copyAsset source dest asset = do
  let sourcePath = source </> asset
  let destPath = dest </> asset
  isDir <- doesDirectoryExist sourcePath
  if isDir
    then do
     s <- parseAbsDir sourcePath
     d <- parseAbsDir destPath
     copyDirRecur s d 
    else do
      copyFile (source </> asset) (dest </> asset)

data Tree a = Node a [Tree a]

createHTMLIndex :: FilePath -> IO ()
createHTMLIndex path = do
    tree <- getDirectoryTreeRecursive path
    let html = "<!DOCTYPE html><html><head><link rel=\"stylesheet\" href=\"root/style.css\"></head><body class=\"proj-index\">" ++ (htmlTreeList tree) ++ "</body></html>"
    
    
    writeFile (path </> "index.html") html

getDirectoryTreeRecursive :: FilePath -> IO (Tree FilePath)
getDirectoryTreeRecursive top = do
    dirContent <- listDirectory top
    paths <- forM dirContent $ \name -> do
        let path = top </> name
        isDir <- doesDirectoryExist path
        if isDir
        then getDirectoryTreeRecursive path
        else return (Node path [])
    return (Node top paths)

htmlTreeList :: Tree FilePath -> String
htmlTreeList (Node path nodes) =
 case (takeFileName  path , takeExtension path) of
    ("dat",_) -> ""
    ("webfonts",_) -> ""
    ("index.html",_) -> ""
    (_ , ".html") -> 
         ("<ul>\n" ++ 
         "<li><a target=\"_parent\" href=\"" ++ path ++ "\">" ++ reverse (drop 5 (reverse (takeFileName path))) ++ ".pact</a></li>\n" ++
         (concatMap htmlTreeList nodes) ++ 
         "</ul>\n")
    (_ , "") -> 
         ("<ul>\n" ++ 
         "<li><span>" ++ takeFileName  path ++ "</span></li>\n" ++
         (concatMap htmlTreeList nodes) ++ 
         "</ul>\n")
    (_ , _) -> ""

