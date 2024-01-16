{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.ByteString as B

import Pact.Core.Compile
import Pact.Core.Syntax.Node.Node
import Pact.Core.Syntax.ParseTreeNode



import System.Environment (getArgs)
import System.FilePath
import System.Directory 
  (doesDirectoryExist
  , doesFileExist
  , createDirectoryIfMissing
  , listDirectory
  , copyFile)



main :: IO ()
main = do
  (target:output:_) <- getArgs
  isFile <- doesFileExist target
  isDir <- doesDirectoryExist target
  if isFile then processFile target output
  else if isDir then processDir target output
  else putStrLn "The given target is neither a file nor a directory."


folderWithAssets :: FilePath
folderWithAssets = "/Users/marcin/www-pact"


assetsToCopy :: [FilePath]
assetsToCopy = ["main.js", "style.css", "webfonts"]


runRenderHTML :: FilePath -> IO B.ByteString
runRenderHTML file = do

  -- let src = "/Users/marcin/pact-projects/marmalade/pact/root/coin.pact"
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
  files <- listDirectory root
  let pactFiles = filter ((==".pact") . takeExtension) files
  mapM_ (processPactFile root output) pactFiles
  copyAssets folderWithAssets output

processPactFile :: FilePath -> FilePath -> FilePath -> IO ()
processPactFile root output pactFile = do
  let relativePath = makeRelative root pactFile
  let targetHtmlFile = replaceExtension relativePath ".html"
  let outputDir = takeDirectory $ output </> targetHtmlFile
  createDirectoryIfMissing True outputDir
  content <- runRenderHTML pactFile
  B.writeFile (output </> targetHtmlFile) content
  copyAssets folderWithAssets outputDir

copyAssets :: FilePath -> FilePath -> IO ()
copyAssets source dest
  | source == dest = return ()
  | otherwise = mapM_ (copyAsset source dest) assetsToCopy

copyAsset :: FilePath -> FilePath -> FilePath -> IO ()
copyAsset source dest asset = copyFile (source </> asset) (dest </> asset)
