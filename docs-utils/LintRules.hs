{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module LintRules where

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

import Linter

applyRules :: CoreBuiltin
                    -> [Bni] -> IO [(Rule, (Text, Maybe (IO ())))]
applyRules = applyRules' rules

data Rule =
   HeaderIsNameOfNative
 | SectionsHeaders
 deriving (Enum)


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

