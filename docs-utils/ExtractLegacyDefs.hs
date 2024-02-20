{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ExtractLegacyDefs where

import System.Directory
import System.FilePath ((</>),takeExtension)
import Control.Monad (forM, foldM, forM_)
import Data.Monoid (mappend, mconcat)
import Data.Text (Text,pack)
import qualified Data.Text.IO as TIO

import Text.MMark.Extension
import qualified Text.MMark            as MMark
import qualified Text.Megaparsec        as M
import Data.List.NonEmpty (NonEmpty(..))

import Data.Text (Text, splitOn, pack)
import qualified Data.Text as T
import Data.List.NonEmpty (NonEmpty, fromList)

import qualified Data.Map as Map

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class

import Data.Maybe
import Data.List

type FileNameProcessor m = Text -> m







data ExtractCtx = ExtractCtx
    { originId :: String
    , mdPathInRepo :: FilePath
    , repoPathOnDisk :: FilePath
    }

legacyPactCtx :: ExtractCtx
legacyPactCtx =
    ExtractCtx
      "legacy"
      "docs/en"
      "/Users/marcin/pact"

kadenaJSCtx :: ExtractCtx
kadenaJSCtx =
    ExtractCtx
      "kadena.js"
      "packages/apps/docs/src/docs/pact/reference/functions/"
      "/Users/marcin/kadena.js"

allExtractCtxs :: [ExtractCtx]
allExtractCtxs = [legacyPactCtx ,kadenaJSCtx]
               
type ELD = ReaderT ExtractCtx IO

applyMonoidToFileContent :: (Show m , Monoid m) => FilePath -> FileNameProcessor m -> ELD m
applyMonoidToFileContent baseDir fun = do
    content <- liftIO $ listDirectory baseDir
    foldM step mempty content
  where 
    step acc name = do
      let path = baseDir </> name
      isDir <- liftIO $ doesDirectoryExist path
      if isDir
        then do
          value <- applyMonoidToFileContent path fun
          return $ acc `mappend` value
        else do
          if (takeExtension path == ".md") then do
            content <- liftIO $ TIO.readFile path
            return $ acc `mappend` (fun content)
          else return acc



-- abandondend since .md files from kadena.js do not conform to strict markdown rules aparently

-- -- | Function to get chapters from a list of BNi's
-- getChapters :: [Bni] -> [[Bni]]
-- getChapters = foldr collectChapters [[]] where
--   collectChapters :: Bni -> [[Bni]] -> [[Bni]]
--   collectChapters bni@(Heading2 _) (chapter:rest) =
--     if null chapter 
--       then [bni]:rest 
--       else [bni]:chapter:rest
--   collectChapters bni (chapter:rest) = (bni:chapter):rest


-- alwaysBNI :: Text -> [Bni]
-- alwaysBNI txt =
--   case MMark.parse "" txt of 
--     Left e -> [ Heading2 ((Plain (pack (show e))) :| []) ] 
--     Right r -> MMark.runScanner r (scanner [] (\x bni -> x ++ [bni]))
      


-- getDefNames :: Text -> [Text]
-- getDefNames =
--   concatMap (\case
--           Heading2 ((Plain t') :| _) -> [t']
--           _ -> [] ) .  alwaysBNI


type Markdown = Text
type Chapter = Text
type ChapterName = Text





-- Function to split the content into chapters.
splitIntoChapters :: Markdown -> [(ChapterName, Chapter)]
splitIntoChapters content =
  let (splited : _) =
         reverse $ sortOn length
         [ splitOn (pack ("\n" ++ (replicate k '#') ++ " ")) content 
         | k <- [2,3]]
  in    catMaybes
      $ map ((\case
                 [] -> Nothing 
                 (x : xs) -> Just (head (T.words x) , T.unlines xs)) . T.lines) 
      $ splited 
   

getDefs :: ExtractCtx -> IO [(ChapterName, Chapter)]
getDefs = runReaderT $ do
   ec <- ask 
   applyMonoidToFileContent (repoPathOnDisk ec </> mdPathInRepo ec) splitIntoChapters
  
showDefs :: ExtractCtx -> IO ()
showDefs ec = do
   dfs <- getDefs ec
   forM_ dfs $ \(cn , c) -> do
       liftIO $ TIO.putStrLn cn
       -- TIO.putStrLn c
       -- TIO.putStrLn ""


getDefsMap :: ExtractCtx -> IO (Map.Map Text Text)
getDefsMap ec = do
   dfs <- getDefs ec
   return (Map.unionsWith (\x y -> x <> "\n------\n"<> y) [ Map.singleton cN cC  | (cN , cC) <- dfs ])

getAllDefsMap :: IO (Map.Map Text (Map.Map String Text))
getAllDefsMap = do
   l <- mapM (\ec -> (fmap (Map.singleton $ originId ec))  <$> getDefsMap ec ) allExtractCtxs
   return $ Map.unionsWith (Map.union ) l


showAllDefsMapStats :: IO ()
showAllDefsMapStats = fmap Map.toList getAllDefsMap >>=
  (mapM_ $ \(defId , m) -> do
           TIO.putStr defId >> putStr (unwords $ ((:) ' ') <$> Map.keys m) >> putStrLn "" )
