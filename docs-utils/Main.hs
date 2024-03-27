{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Directory (listDirectory, doesFileExist)
import System.FilePath ((</>),(<.>), takeBaseName, takeExtension)
import Data.List (sort, (\\),intersperse,sortOn)
import Control.Monad (filterM,forM,forM_, when, unless)
import Pact.Core.Builtin

import qualified Data.Text.IO          as T
import qualified Data.Text.Lazy.IO          as LT
import Data.Text (Text)
import qualified Data.Text as Text


import Control.Monad ()
import Data.Maybe (catMaybes, isJust, mapMaybe)

import qualified Data.Text             as Text
import Text.MMark (MMark)            
import qualified Text.MMark            as MMark
import qualified Text.Megaparsec        as M

import System.IO (hPutStrLn, stderr, withFile, IOMode(..))
import Text.Printf (printf)
import Data.List (intercalate)

import Text.MMark.Extension

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..))

import System.Console.ANSI

import Linter
import LintRules

import ExtractLegacyDefs

import qualified Data.Map as Map

import qualified Lucid             as L

docsDir :: FilePath
docsDir = "/Users/marcin/pact-core/docs/functions"


data DocumentationReport = DocumentationReport
  { missingDocs :: [CoreBuiltin]
  , extraFiles :: [FilePath]
  } deriving (Show, Eq)




ident :: Int -> String -> String
ident k = concat . map  (++ ('\n' : ([0..k] *> pure ' '))) . lines

allCoreBuiltins  :: [CoreBuiltin]
allCoreBuiltins = [minBound .. maxBound]

-- Altering the `checkCoreBuiltinDocs` function
checkCoreBuiltinDocs :: FilePath -> DocUtilMode -> IO ()
checkCoreBuiltinDocs dir mode = do
  files <- listDirectory dir
  markdownFiles <- filterM (isMarkdownFile dir) files
  let presentFiles = map takeBaseName markdownFiles

  -- Process each expected file, collecting possible violations
  results <- forM allCoreBuiltins $ \builtin -> do
    let filename = Text.unpack $ coreBuiltinToText builtin
    if filename `elem` presentFiles
      then do
        bnis <- toBNIs $ dir </> filename <.> "md"
        -- when (builtin == CoreDefineKeySet) $ putStrLn $ show bnis
        violations <- applyRules builtin bnis
        return (Just (builtin , filename), violations)
      else return (Nothing, [])

  let -- Collect statistics
      invalidFiles = filter (not . null . snd) results
      totalViolations = sum $ map (length . snd) invalidFiles
      validFiles = filter (isJust) (map fst results) \\ map fst invalidFiles

  -- Mode-dependent output handling
  printDocumentationStats presentFiles mode
  case mode of
    Basic       -> printBasicStats (length validFiles) (length invalidFiles) totalViolations
    Verbose     -> do
       printBasicStats (length validFiles) (length invalidFiles) totalViolations
       printVerboseReport invalidFiles
    Interactive -> do
       printBasicStats (length validFiles) (length invalidFiles) totalViolations
       interactWithUser invalidFiles

  where

    isMarkdownFile :: FilePath -> FilePath -> IO Bool
    isMarkdownFile directory file = do
      exists <- doesFileExist (directory </> file)
      return $ exists && takeExtension file == ".md"



    printBasicStats validCount invalidCount totalViolations = do
      setSGR [SetConsoleIntensity BoldIntensity]
      putStrLn "Basic Mode Documentation Report:"
      setSGR [Reset]

      setSGR [SetColor Foreground Vivid Green]
      putStrLn $ "Valid Documentation Files: " ++ show validCount
      setSGR [Reset]

      setSGR [SetColor Foreground Vivid Red]
      putStrLn $ "Invalid Documentation Files: " ++ show invalidCount
      putStrLn $ "Total Violations: " ++ show totalViolations
      setSGR [Reset]


    printVerboseReport invalidFiles = do
      setSGR [SetConsoleIntensity BoldIntensity]
      putStrLn "Verbose Mode Documentation Report:"
      setSGR [Reset]

      forM_ invalidFiles $ \(Just (bi , filename), violations) -> do
        setSGR [SetColor Foreground Vivid Blue]
        putStrLn $ "File: " ++ filename ++ ".md (" ++ show bi ++ ")"
        setSGR [Reset]

        forM_ violations $ \(rule, (violation, _)) -> do
          setSGR [SetColor Foreground Vivid Blue]
          putStrLn $ "  Violation: " ++ (ident 13 $ Text.unpack violation)
          setSGR [Reset]
        putStrLn "" -- Print an extra newline for better readability


    interactWithUser invalidFiles = do
      forM_ invalidFiles $ \(Just (bi , filename), violations) -> do
        setSGR [SetColor Foreground Dull Cyan]
        putStrLn $ "File: " ++ filename ++ ".md (" ++ show bi ++ ")"
        setSGR [Reset]

        forM_ violations $ \(rule, (violationTxt, fix)) -> do
          setSGR [SetColor Foreground Dull Red]
          putStrLn $ "Violation: " ++ (ident 13 $ Text.unpack violationTxt)
          setSGR [Reset]
          interactWithViolation rule fix


    interactWithViolation rule fix = do
      setSGR [SetConsoleIntensity BoldIntensity]
      putStrLn "Choose an action:"
      setSGR [Reset]

      setSGR [SetColor Foreground Vivid Magenta]
      putStrLn "  1. Do nothing"
      putStrLn "  2. Show description of violation"
      putStrLn "  3. Attempt to fix (if available)"
      setSGR [Reset]

      choice <- getLine
      case choice of
        "1" -> return ()
        "2" -> do
          setSGR [SetColor Foreground Vivid Blue]
          putStrLn $ Text.unpack $ _chDesc (rules rule)
          setSGR [Reset]
        "3" -> case fix of
                 Just fixAction -> fixAction
                 Nothing -> do
                   setSGR [SetColor Foreground Vivid Red]
                   putStrLn "No fix available for this violation"
                   setSGR [Reset]
        _ -> do
          setSGR [SetColor Foreground Vivid Red]
          putStrLn "Invalid option selected"
          setSGR [Reset]

    


    -- Function to print documentation statistics with ANSI colors for fancier output
    printDocumentationStats :: [FilePath] -> DocUtilMode -> IO ()
    printDocumentationStats presentFiles mode = do
      let expectedFiles = map (Text.unpack . coreBuiltinToText) allCoreBuiltins
          missingBuiltins = [builtin | builtin <- allCoreBuiltins, Text.unpack (coreBuiltinToText builtin) `notElem` presentFiles]
          unexpectedFiles = presentFiles \\ expectedFiles

      setSGR [SetConsoleIntensity BoldIntensity]
      putStrLn "Documentation Stats:"
      setSGR [Reset]

      setSGR [SetColor Foreground Vivid Green]
      putStrLn $ "Expected files present: " ++ show (length presentFiles)
      setSGR [Reset]

      case mode of
        Basic -> do
          printUnexpectedFiles unexpectedFiles
          printMissingBuiltins missingBuiltins
        Verbose -> do
          unless (null unexpectedFiles) $ do
            setSGR [SetColor Foreground Vivid Red]
            putStrLn "Unexpected files in documentation directory:"
            setSGR [Reset]
            mapM_ putStrLn unexpectedFiles
          printMissingBuiltins missingBuiltins
        _ -> do
          printUnexpectedFiles unexpectedFiles
          printMissingBuiltins missingBuiltins

      setSGR [Reset] -- Reset all styling
      putStrLn "\n" -- Print an extra newline for better readability

    printUnexpectedFiles :: [FilePath] -> IO ()
    printUnexpectedFiles unexpectedFiles = do
      unless (null unexpectedFiles) $ do
        setSGR [SetColor Foreground Vivid Red]
        putStrLn $ "Unexpected files: " ++ show (length unexpectedFiles)
        setSGR [Reset]

    printMissingBuiltins :: [CoreBuiltin] -> IO ()
    printMissingBuiltins missingBuiltins = do
      unless (null missingBuiltins) $ do
        setSGR [SetColor Foreground Vivid Red]
        putStrLn $ "Missing built-ins documentation: " ++ show (length missingBuiltins)

        if mode == Interactive then
          interactiveMissingBuiltins missingBuiltins
        else
          do
            setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Red]
            putStrLn "Missing documentation for the following functions:"
            setSGR [Reset] -- Reset before printing the names
            mapM_ (putStr . ((++) " ,") . Text.unpack . coreBuiltinToText) missingBuiltins
            setSGR [Reset]

    interactiveMissingBuiltins :: [CoreBuiltin] -> IO ()
    interactiveMissingBuiltins missingBuiltins = do
      putStrLn "Do you want to generate stubs for the missing documentation files? (y/n)"
      choice <- getLine
      case choice of
        "y" -> do
          putStrLn "Generating stubs..."
          generateStubsForMissingFiles missingBuiltins dir
        "n" -> return ()
        _ -> do
          setSGR [SetColor Foreground Vivid Red]
          putStrLn "Invalid option selected, please type 'y' or 'n'"
          setSGR [Reset]
          interactiveMissingBuiltins missingBuiltins


checkSingleBuiltin :: CoreBuiltin -> IO Text
checkSingleBuiltin b = do
  let filename = Text.unpack $ coreBuiltinToText b
      fPath = docsDir </> filename <.> "md"
  exists <- doesFileExist fPath
  if exists
     then do bnis <- toBNIs $ fPath
             (\violations ->
                 Text.pack $ if null violations then "✅(0)" else ("❗(" ++ (show (length violations)) ++ ")"))
               <$> (applyRules b bnis)
             
     else return $ "❎"

data DocUtilMode =
     Basic
   | Verbose
   | Interactive
   deriving (Eq)



main :: IO ()
main = do
    clearScreen
    setCursorPosition 0 0
    setSGR [SetColor Foreground Vivid Blue]
    putStrLn "Please select the documentation utility mode:"
    setSGR [Reset]
    putStrLn "Press 'b' for Basic mode"
    putStrLn "Press 'v' for Verbose mode"
    putStrLn "Press 'i' for Interactive mode"
    setSGR [SetUnderlining SingleUnderline]
    putStrLn "Choice: "
    setSGR [Reset]

    mode <- getModeSelection
    checkCoreBuiltinDocs docsDir mode

-- Helper function to get the documentation utility mode selection from user
getModeSelection :: IO DocUtilMode
getModeSelection = do
    setSGR [SetColor Foreground Vivid Blue]
    selection <- getChar
    putStrLn "" -- to move to the next line after the user input
    setSGR [Reset]
    case selection of
      'b' -> return Basic
      'v' -> return Verbose
      'i' -> return Interactive
      _   -> do
        setSGR [SetColor Foreground Vivid Red]
        putStrLn "Invalid selection, please choose 'b', 'v' or 'i'."
        setSGR [Reset]
        getModeSelection


skipFileGen :: CoreBuiltin -> Bool
skipFileGen bi = (Text.length $ coreBuiltinToText bi ) == 1

-- A helper function to generate stubs for all missing files
generateStubsForMissingFiles :: [CoreBuiltin] -> FilePath -> IO ()
generateStubsForMissingFiles missingBuiltins dir = do
  forM_ missingBuiltins $ \builtin -> (unless $ skipFileGen builtin) $ do
    let filename = Text.unpack (coreBuiltinToText builtin) ++ ".md"
        filepath = dir </> filename
        content = createStubContent builtin

    withFile filepath WriteMode $ \handle -> do
      hPutStrLn handle content

  putStrLn $ "Generated stubs for " ++ show (length missingBuiltins) ++ " missing documentation files."

-- Function to create stub content with sections
createStubContent :: CoreBuiltin -> String
createStubContent builtin =
  let title = Text.unpack $ coreBuiltinToText builtin
      sections = unlines $ map (\h -> "## " ++ Text.unpack h) correctHeaders
  in unlines
     [ "# " ++ title
     , ""
     , sections
     ]

-- Function to create markdown table
markdownTable :: [[Text]] -> Text
markdownTable [] = ""
markdownTable (h:t) = Text.unlines $ header : separator : body
  where
    header = rowToString h
    separator = rowToString $ replicate (length h) "---"
    body = rowToString <$> t

-- Function to convert row List[Text] to Text
rowToString :: [Text] -> Text
rowToString t = Text.intercalate " | " ([""] ++ t ++ [""]) 

makeReport :: IO ()
makeReport = do
 lDefs <- getAllDefsMap 
 tD <- forM allCoreBuiltins
   (\b -> do
      let s = coreBuiltinToText b
      let lDefsCells = (\b -> if b then "✔" else "✗") <$>
             (case (Map.lookup s lDefs) of
               Nothing -> [False , False]
               Just y -> [Map.member "legacy" y , Map.member "kadena.js" y])
      docState <- checkSingleBuiltin b          
      return ([coreBuiltinToText b , docState] ++ lDefsCells))
 T.writeFile "/Users/marcin/table.md" (markdownTable
                                        (["" , "pact-core" ,"legacy-pact" ,"kadena.js"] : (presentFirst  tD)))

 where
 presentFirst :: [[Text]] -> [[Text]]
 presentFirst = reverse . sortOn (Text.length . head . (drop 1))
