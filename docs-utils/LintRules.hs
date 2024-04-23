{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module LintRules where

import Text.RawString.QQ
import System.Directory (listDirectory, doesFileExist)
import System.FilePath ((</>),(<.>), takeBaseName, takeExtension)
import Data.List (sort, (\\),intersperse)
import Control.Monad (filterM,forM,forM_, when, unless)
import Pact.Core.Builtin

import qualified Data.Text.IO          as T
import Data.Text (Text,pack)
import qualified Data.Text as Text


import Control.Monad ()
import Data.Maybe (catMaybes, isJust, mapMaybe)

import qualified Data.Text             as Text
import Text.MMark (MMark)            
import qualified Text.MMark            as MMark
import qualified Text.Megaparsec        as M

import System.IO (hPutStrLn, stderr, withFile, IOMode(..))
import Text.Printf (printf)
import Data.List (intercalate,findIndex)

import Text.MMark.Extension

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..))

import System.Console.ANSI

import Linter

applyRules :: CoreBuiltin
                    -> [Bni] -> IO [(Rule, (Text, Maybe (IO ())))]
applyRules = applyRules' rules

data Rule =
   HeaderIsNameOfNative
 | SectionsHeaders
 deriving (Enum)


-- sectionsSpecs :: [((Bool , Text) , String)]
-- sectionsSpecs =
--    [ ((obligatory , "Basic syntax") , "")
--    , ((obligatory , "Arguments") , "")
--    , ((optional , "Prerequisites")  , "")
   
--    , ((obligatory , "Return values")  , "")
--    , ((obligatory , "Examples") , "")
--    , ((optional , "Options") , "")
--    , ((optional , "Property validation") , "")
--    , ((optional , "Gotchas") , "")
--    ]
--  where
--    obligatory = True
--    optional = False

sectionsSpecs :: [((Bool , Text) , String)]
sectionsSpecs =
   [ ((obligatory , "Basic syntax") , basicSyntax)
   , ((obligatory , "Arguments") , arguments)
   , ((optional , "Prerequisites") , prerequisites)
   
   , ((obligatory , "Return values") , returnValues)
   , ((obligatory , "Examples") , examples)
   , ((optional , "Options") , options)
   , ((optional , "Property validation") , propertyValidation)
   , ((optional , "Gotchas") , gotchas)
   ]
 where
   obligatory = True
   optional = False

   basicSyntax = [r|
Generate a clear and concise explanation of the basic syntax for your function. This section should contain at least one code snippet demonstrating how to use the function. The code should be provided in the format: 

'''pact
your function syntax
'''

If your function can be overloaded, provide additional code snippets to reflect its multiple uses. Overall, aim to describe the syntax in a way that is easy to comprehend, including any necessary arguments and acceptable data types.
|]

   arguments = [r|
In this section, provide a detailed explanation of all the arguments of your function. Create a markdown table with each row representing a different argument. Your table should include the following fields:

| Argument | Type | Description |

Make sure the 'Argument' field contains the name of the argument, 'Type' lists the data type of the argument, and 'Description' holds a clear, concise explanation of what the argument means in the context of your function. 

Ensure the number of rows in your table matches the arity of your function. 
|]

   prerequisites = [r|
If your function needs any prerequisites to run successfully, describe them here. If there are no prerequisites, respond with 'N/A'.
|]

   returnValues = [r|
In this section, detail what your function returns. Describe the type and purpose of the returned value, and explain in what context this return value would be useful. 

Remember, this section should not be left empty - if the function does not return anything, clearly state that this is the case.
|]

   examples = [r|
Provide few code examples demonstrating the use of your function. Each example should be contained within the markdown code block: 

'''pact
your function usage example
'''

The examples should be clear and easy to understand. They should demonstrate the use of different arguments or use cases where applicable.
|]

   options = [r|
If your function has any configurable options, describe them here in the format similar to the 'Arguments'. That is, a markdown table with 'Option', 'Type' and 'Description' as columns. Make sure to clearly explain the effect of each option on your function's execution. If there are no options, respond with 'N/A'.
|]

   propertyValidation = [r|
If your function includes any form of property validation, explain it here. Clearly explain the rules that the function follows to verify its arguments and error conditions. If there is no property validation involved in your function, respond with 'N/A'.
|]

   gotchas = [r|
In this section, discuss any unintuitive behavior, potential pitfalls, or common mistakes to avoid while using your function. Make sure to present this information in a clear and concise manner to help your users avoid these issues. If there are no known gotchas associated with your function, respond with 'N/A'.
|]


correctHeaders :: [(Bool , Text)]
correctHeaders = fmap fst sectionsSpecs

rules :: Rule -> BuiltInDocRule
rules = \case
  HeaderIsNameOfNative -> BuiltInDocRule
     { _chName = "HeaderIsNameOfNative"
     , _chDesc = "Top section needs to be exactly name of the function"
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
              if (all ((== 2 ) . fst) r) -- && (map snd r == correctHeaders)
              then fmap
                     (\e -> (pack $ show e, Nothing))
                     (checkOptionalAndRequired correctHeaders (map snd r))
              else
                 (Just ("after first header, all following should be on 2nd level", Nothing))
            _ -> return (Just ("first header - name of the function - missing", Nothing))      
     }

data OptionalAndRequiredError a = 
   Missing a
 | Unexpected a
 deriving (Eq, Show)

checkOptionalAndRequired :: Eq a => [(Bool , a)] -> [a] -> Maybe (OptionalAndRequiredError a)
checkOptionalAndRequired [] [] = Nothing
checkOptionalAndRequired [] (x : _)  = Just (Unexpected x)
checkOptionalAndRequired ((isRequired, value):xs) [] = 
  if isRequired then (Just (Missing value)) else checkOptionalAndRequired xs []
checkOptionalAndRequired ((isRequired, value):xs) ys'@(y : ys) =
   if y == value
   then checkOptionalAndRequired xs ys
   else if isRequired then (Just (Missing value)) else checkOptionalAndRequired xs ys'

-- runTest :: (Show a, Eq a) => [(Bool, a)] -> [a] -> Maybe (OptionalAndRequiredError a) -> IO ()
-- runTest arg1 arg2 expected = 
--     if checkOptionalAndRequired arg1 arg2 == expected 
--       then putStrLn "Test passed" 
--       else putStrLn "Test failed"

-- checkOptionalAndRequiredTest :: IO ()
-- checkOptionalAndRequiredTest = do
--     runTest [(True, 1), (False, 2), (True, 3)] [1, 3] Nothing
--     runTest [(True, 1), (True, 2), (True, 3)] [1, 3] (Just $ Missing 2)
--     runTest [(True, 1), (True, 2), (True, 3)] [1, 2, 3, 4] (Just $ Unexpected 4)
--     runTest [(True, 1), (False, 2), (True, 3)] [1, 2, 3] Nothing
