{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Directory (listDirectory, doesFileExist)
import System.FilePath ((</>),(<.>), takeBaseName, takeExtension)
import Data.List (sort, (\\),intersperse)
import Control.Monad (filterM,forM,forM_, when, unless)
import Pact.Core.Builtin

import qualified Data.Text.IO          as T
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

someMd :: FilePath
someMd = "/Users/marcin/pact-core/docs/native/define-keyset.md"

docsDir :: FilePath
docsDir = "/Users/marcin/pact-core/docs/native"


data DocumentationReport = DocumentationReport
  { missingDocs :: [CoreBuiltin]
  , extraFiles :: [FilePath]
  } deriving (Show, Eq)



-- Helper function to apply rules to a given CoreBuiltin and its associated Bni list
applyRules :: CoreBuiltin -> [Bni] -> IO [(Rule, (Text, Maybe (IO ())))] -- Collects (rule, violation)
applyRules cb bnis = do
  ruleViolations <- forM (enumFrom (toEnum 0)) $ \r -> do
    let rule = rules r
    validationResult <- _chValidator rule cb bnis
    return $ fmap (\v -> (r, v)) validationResult
  return $ catMaybes ruleViolations


ident :: Int -> String -> String
ident k = concat . map  (++ ('\n' : ([0..k] *> pure ' '))) . lines

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
        when (builtin == CoreDefineKeySet) $ putStrLn $ show bnis
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
    allCoreBuiltins = [minBound .. maxBound] :: [CoreBuiltin]
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


data BuiltInDocRule = BuiltInDocRule
 { _chName :: Text -- name of the rule
 , _chDesc :: Text -- description of the rule
 , _chValidator :: CoreBuiltin -> [Bni] -> IO (Maybe (Text , Maybe (IO ())))
     -- returnign just onlly in case of violation of the rule
     -- secound field of the tuple might cointain "fix" for the rule, wich when executes, fixes violation
 }

data Rule =
   HeaderIsNameOfNative
 | SectionsHeaders
 deriving (Enum)


toBNIs :: FilePath -> IO [Bni]
toBNIs input = do
  txt <- T.readFile input
  case MMark.parse input txt of -- (2)
    Left bundle -> error (M.errorBundlePretty bundle) -- (3)
    Right r -> do
      let i = MMark.runScanner r (scanner [] (\x bni -> x ++ [bni]))
      return i
   


simpleHeadingView :: Bni -> Maybe (Int , Text) 
simpleHeadingView = \case
  Heading1 (Plain t :| _) -> Just (1 , t)
  Heading2 (Plain t :| _) -> Just (2 , t)
  Heading3 (Plain t :| _) -> Just (3 , t)
  Heading4 (Plain t :| _) -> Just (4 , t)
  Heading5 (Plain t :| _) -> Just (5 , t)
  Heading6 (Plain t :| _) -> Just (6 , t)  
  _ -> Nothing

correctHeaders :: [Text]
correctHeaders =
   [ "Basic usage"
   , "Prerequisites"
   , "Arguments"
   , "Return values"
   , "Examples"
   ]




rules :: Rule -> BuiltInDocRule
rules = \case
  HeaderIsNameOfNative -> BuiltInDocRule
     { _chName = "HeaderIsNameOfNative"
     , _chDesc = "abs"
     , _chValidator = \t -> \case
          (Heading1 ((Plain t') :| _) : _) -> return $
              if (coreBuiltinToText t == t') then Nothing
              else (Just ("bad content of first header", Nothing))
          _ -> return (Just ("bad content of first header", Nothing))
     }
    
  SectionsHeaders -> BuiltInDocRule
     { _chName = "SectionHeaders"
     , _chDesc = "abs"
     , _chValidator = \ _ l -> 
          case (catMaybes $ map simpleHeadingView l) of
            hdrs@(( 1 , _) : r) -> return $
              if (all ((== 2 ) . fst) r) && (map snd r == correctHeaders)
              then Nothing
              else let msg = Text.intercalate "\n" (map snd hdrs)
                   in Just ("Dettected headers:\n " <> msg  , Nothing)
            _ -> return (Just undefined)           
     }


     

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
