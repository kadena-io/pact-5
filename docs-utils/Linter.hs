{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Linter where

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


data DocumentationReport = DocumentationReport
  { missingDocs :: [CoreBuiltin]
  , extraFiles :: [FilePath]
  } deriving (Show, Eq)




-- Helper function to apply rules to a given CoreBuiltin and its associated Bni list
applyRules' :: Enum r => (r -> BuiltInDocRule) -> CoreBuiltin -> [Bni] -> IO [(r, (Text, Maybe (IO ())))] -- Collects (rule, violation)
applyRules' rules cb bnis = do
  ruleViolations <- forM (enumFrom (toEnum 0)) $ \r -> do
    let rule = rules r
    validationResult <- _chValidator rule cb bnis
    return $ fmap (\v -> (r, v)) validationResult
  return $ catMaybes ruleViolations


data BuiltInDocRule = BuiltInDocRule
 { _chName :: Text -- name of the rule
 , _chDesc :: Text -- description of the rule
 , _chValidator :: CoreBuiltin -> [Bni] -> IO (Maybe (Text , Maybe (IO ())))
     -- returnign just onlly in case of violation of the rule
     -- secound field of the tuple might cointain "fix" for the rule, wich when executes, fixes violation
 }



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
