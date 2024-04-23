
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}


module StubGen where

import Text.RawString.QQ
import qualified ExtractLegacyDefs as Extract
import qualified GPTUtils as GPT
import qualified Linter as Lint
import qualified LintRules as Rules
import qualified Snippets as Snippets

import Pact.Core.Builtin
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)

import qualified Data.Text.IO as TextIO

import GHC.Generics (Generic)
import Data.Aeson
import GPTUtils (Message(..))
import System.FilePath


-- The maximum number of snippets to include for each phrase
maxSnippetsPerPhrase :: Int
maxSnippetsPerPhrase = 30 -- example limit

-- A wrapper around Snippets functions to generate stub text for a given built-in
generateStubFromSnippets :: CoreBuiltin -> IO String
generateStubFromSnippets builtin = do
  let builtinText = coreBuiltinToText builtin
  snippetMap <- Snippets.findSnippets [builtinText] Snippets.reposToIncludeInStubsPrompt
  let limitedSnippets = fmap (take maxSnippetsPerPhrase) snippetMap
  let stubText = Map.foldMapWithKey (\phrase snippets -> snippetsToText phrase snippets) limitedSnippets
  return (Text.unpack stubText)

snippetToText :: Snippets.Snippet -> Text
snippetToText snippet = Text.concat [
    "File: ", Text.pack (Snippets._sFilePath snippet), "\n",
    "Lines: ", Text.pack (show $ Snippets._sLines snippet), "\n\n",
    Snippets._sContent snippet
  ]

snippetToTextWithLineNumbers :: Snippets.Snippet -> Text
snippetToTextWithLineNumbers snippet =
  let (startLine, _) = Snippets._sLines snippet
      lineNumbers = [startLine ..]
      contentWithLineNumbers = Text.unlines $ zipWith formatLine lineNumbers (Text.lines $ Snippets._sContent snippet)
  in Text.concat [
      "File: ", Text.pack (Snippets._sFilePath snippet), "\n",
      "Lines: ", Text.pack (show $ Snippets._sLines snippet), "\n\n",
      contentWithLineNumbers
    ]
  where
    formatLine :: Int -> Text -> Text
    formatLine lineNum lineContent = Text.pack (show lineNum) `Text.append` ": " `Text.append` lineContent

-- Helper function to transform snippets to a formatted text block
snippetsToText :: Text -> [Snippets.Snippet] -> Text
snippetsToText phrase snippets = Text.concat [
    "### ", phrase, " Snippets\n\n",
    Text.intercalate "\n\n" (map snippetToTextWithLineNumbers snippets)
  ]



generateSectionPrompt :: String -> String -> String -> ((Bool , Text) , String) -> CoreBuiltin -> [Message]
generateSectionPrompt docOfCurrentFromLegacyDocs snippets exampleDocInNewFormat ((isRequired, sectionHeading), sectionDescription) builtIn = 
  [ Message "user" $ "You are updating the documentation for the built-in function: " ++ builtInName ++ "."
  , Message "user" $ "The next section to document is: " ++ Text.unpack sectionHeading ++ "."
  , Message "user" $ "This section is " ++ requirement ++ ". Please ensure that the information is accurate and complete."
  , Message "user" $ "Below is an example documentation for `at` functioon compliant with the new format documentation to guide you:"
  , Message "user" exampleDocInNewFormat
  , Message "user" $ "To help you with this, here's the the legacy documentation for `" ++ builtInName ++ "` that might contain relevant information:"
  , Message "user" docOfCurrentFromLegacyDocs
  , Message "user" $ "Additionally, consider the following snippets from our repositories which show examples or use-cases of the `" ++ builtInName ++ "` function:"
  , Message "user" snippets
  , Message "user" $ "Using the provided example, legacy documentation, and code snippets, please write the '" ++ Text.unpack sectionHeading ++ "' section of the documentation taking into account the following description and instruction, remember! Generate only requested section, nothing more! :"
  , Message "user" sectionDescription
  ]
  where
    builtInName = Text.unpack $ coreBuiltinToText builtIn
    requirement = if isRequired then "REQUIRED" else "OPTIONAL"                                


-- Wrapper for getChapterInAllContexts that operates on CoreBuiltins
getLegacyDocsForBuiltin :: CoreBuiltin -> IO String
getLegacyDocsForBuiltin builtin = do
  let chapterName = coreBuiltinToText builtin
  chapters <- Extract.getChapterInAllContexts chapterName
  let combinedDocs = Map.foldMapWithKey (\origin content -> formatChapter origin content) chapters
  return (Text.unpack combinedDocs)

-- Helper function to format content by marking the origin and adding the content
formatChapter :: String -> Text -> Text
formatChapter origin content = Text.concat [
    "\n## From: ", Text.pack origin, "\n\n",
    content, "\n\n"
  ]




generateDocumentationForBuiltin :: Bool -> CoreBuiltin -> IO Text
generateDocumentationForBuiltin verbose builtin = do
  -- Extracting necessary parts using predefined functions
  let builtinText = coreBuiltinToText builtin
  docOfCurrentFromLegacyDocs <- getLegacyDocsForBuiltin builtin
  snippetsText <- generateStubFromSnippets builtin
  

  -- Generating each section using GPT-3
  sectionsContent <- mapM (generateSectionForBuiltin verbose docOfCurrentFromLegacyDocs snippetsText exampleDocContent builtinText) Rules.sectionsSpecs

  let fullDocumentation = Text.unlines $ map Text.pack sectionsContent

  -- Output or save the content as needed
  when verbose $
    TextIO.putStrLn fullDocumentation
  return fullDocumentation

    where

      generateSectionForBuiltin :: Bool -> String -> String -> String -> Text -> ((Bool , Text) , String) -> IO String
      generateSectionForBuiltin verbose legacyDocs snippets exampleDoc builtInName sectionSpec = do
        let messages = generateSectionPrompt legacyDocs snippets exampleDoc sectionSpec builtIn
        -- Use the ChatGPT API to generate content for the section
        maybeSectionContent <- GPT.makeChatGPTRequest' messages
        case maybeSectionContent of
          Just sectionContent -> return sectionContent
          Nothing -> do
            -- Log or handle the error case here
            when verbose $
              putStrLn $ "Error: Failed to generate content for section " ++ show (snd sectionSpec)
            return $ "### " ++ (snd sectionSpec) ++ "\n\nCould not generate content."

      -- Convert CoreBuiltin into the same type as used in Section Prompt
      builtIn = builtin


exampleDocContent :: String
exampleDocContent = [r|
# at

Use `at` to retrieve the value at the location specified by an *index* number or by a *key* string in a collection. If you specify an index number, the collection must be a list of values.
If you specify a key string, the collection must be an object.

## Basic syntax

To get a value using the specified index location from a list of values, use the following syntax:

```pact
at *index*:integer [list]
```

To get a value using the specified string from an object, use the following syntax:

```pact
at *key*:string {object}
```

## Arguments

Use one of the following argument to define the value you want to retrieve using the `at` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| index | integer | Specifies the information you want to retrieve. If you specify an index number, the function returns the value from that location in a list of values. |
| key | string | Specifies the information you want to retrieve. If you specify a key string, the function returns the value corresponding to that key from an object |

## Return values

The `at` function returns the value found at the specified index or using the specified key. The return value can be any data type.

## Examples

The following example returns the value found at the *index* location—starting with 0—from a list of values:

```pact
(at 3 [20 18 16 14 12 10])
14
```

You can use the `at` function to return any type of data from a list.
For example:

```pact
(at 1 ["blue","green","red","yellow"])
"green"
```

The following example returns the value found at the specified *key* from an object:

```pact
(at "last-name" { "first-name": "maya", "last-name": "tea"})
"tea"
```

You can use the `at` function to return any type of data using the specified key from an object.
For example:

```pact
(at "chainId" { "networkId": "development", "chainId": 1, "auth": 0})
1
```

## Property validation

For property checking, you can use the `at` list operator when specifying an invariant or a property to test your code against.

|]
