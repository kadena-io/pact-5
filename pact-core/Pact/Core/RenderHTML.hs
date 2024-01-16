{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.ByteString as B

import Pact.Core.Compile
import Pact.Core.Syntax.Node.Node
import Pact.Core.Syntax.ParseTreeNode

-- import import System.File.Tree (getDirectory, copyTo_)

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


main :: IO ()
main = 
  getArgs >>= \case
    (target:output:_) -> do
       if (takeExtension target ==".pact")
       then processFile target output
       else processDir target output
      
    _ -> putStrLn "TODO: usage info"

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
