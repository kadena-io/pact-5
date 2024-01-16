{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}


module Pact.Core.Syntax.Node.Node where

import Control.Lens hiding (List, op, _head, _tail)
import Control.Monad.State
import Data.Foldable(traverse_)
import Data.Text as T (Text)
import Data.Text.Encoding as TE

import qualified Data.List.NonEmpty as NE

import Data.Functor.Const()

import Data.Aeson (encode)


import Data.Set (Set)
import qualified Data.Set as Set
import Data.Aeson.Types (Value, Value(..))


import Data.Maybe (catMaybes, listToMaybe, isNothing,fromMaybe, maybeToList,
                   fromJust, isJust)
import Control.Monad (forM_,void,when)

import Pact.Core.Info

import Language.Haskell.TH

import qualified Data.Aeson.KeyMap as KM

import Data.List as List

import System.Console.ANSI

import qualified Data.ByteString.Lazy.Char8 as LBC8

import Control.Monad.Writer

import Data.String (fromString)

import Data.ByteString(ByteString)
import qualified Data.Map.Strict as M
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.UTF8 as BU


import System.Directory (doesFileExist)

import Pact.Core.Syntax.Lexer(lexer)
import Pact.Core.Syntax.LexUtils


data AST i = forall a. ASTNode a => AST
  { _head ::  a
  , _tail :: [(Maybe String , AST i)]
  }

getASTInfo :: AST SpanInfo -> Maybe SpanInfo
getASTInfo (AST h _) = ifIsInfoGetIt h


class HasOwnData a where
  ownData :: a -> M.Map ByteString (Set ByteString)
  ownData _ = M.empty


emptyOwnData :: a -> M.Map ByteString (Set ByteString)
emptyOwnData _ = M.empty


-- Function that checks the constraint
isInstanceOfHasOwnData :: Type -> Q Bool
isInstanceOfHasOwnData t = do
  -- We need the list of types to reify for a single type so use list here
  instances <- reifyInstances ''HasOwnData [t]
  return $ not $ List.null instances


class ASTNode a where
    
  ifIsInfoGetIt :: a -> Maybe SpanInfo
  ifIsInfoGetIt _ = Nothing
  
  getTailWithMbNamesAll :: a -> [(Maybe String, AST SpanInfo)]
  getTailWithMbNamesAll _ = []
  
    
  getOwnJSON :: a -> Maybe Value
  getOwnJSON _ = Nothing

  getTypeQName :: a -> String
    
  getConstructorQName :: a -> Maybe String
  getConstructorQName _ = Nothing 

  ownDataAST :: a -> M.Map ByteString (Set ByteString)
  ownDataAST _ = M.empty

getTailWithMbNames :: ASTNode a => a -> [(Maybe String, AST SpanInfo)]
getTailWithMbNames = List.filter
    (isNothing . getASTInfo  . snd )
    . getTailWithMbNamesAll

getTail :: ASTNode a => a -> [AST SpanInfo]
getTail x = [ast | (_, ast) <- getTailWithMbNames x]


toAST :: ASTNode a => a -> AST SpanInfo
toAST x = AST x (List.filter (isNothing . getASTInfo . snd) $ getTailWithMbNames x) 

isWrapper :: AST SpanInfo -> Bool
isWrapper ast@(AST h [x]) =
  case (getOwnJSON h , getInfo h) of
    (Nothing , Nothing) -> True
    _ -> False 
isWrapper _ = False

skipWrappers :: AST SpanInfo -> AST SpanInfo
skipWrappers x@(AST h l) | isWrapper x = skipWrappers $ snd $ head l 
skipWrappers (AST h l) = AST h (List.map (fmap skipWrappers) l)

getInfo :: ASTNode a => a -> Maybe SpanInfo
getInfo =
   listToMaybe . catMaybes . (List.map getASTInfo) . fmap snd . getTailWithMbNamesAll



instance ASTNode a => ASTNode [ a ] where 
  getTailWithMbNamesAll = List.map ((Nothing ,) . toAST)
  getTypeQName _ = "List"

instance ASTNode a => ASTNode (NE.NonEmpty a) where 
  getTailWithMbNamesAll = List.map ((Nothing ,) . toAST) . NE.toList  
  getTypeQName _ = "NonEmpty"

instance ASTNode a => ASTNode (Maybe a) where 
  getTailWithMbNamesAll (Just x) = [ (Nothing , toAST x) ]
  getTailWithMbNamesAll Nothing = []
  getTypeQName _ = "Maybe"
  getConstructorQName (Just _) = Just "Just"
  getConstructorQName Nothing = Just "Nothing"
  
instance (ASTNode a , ASTNode b) => ASTNode (a , b) where 
  getTailWithMbNamesAll (x , y) = [ (Nothing , toAST x) , (Nothing , toAST y) ]
  getTypeQName _ = "Pair"

instance ASTNode SpanInfo where 
  ifIsInfoGetIt x = Just x
  getTypeQName _ = "SpanInfo"

showSpanInfo :: SpanInfo -> String
showSpanInfo (SpanInfo startLine startCol endLine endCol) = 
       " SL: " ++ show startLine 
    ++ " SC: " ++ show startCol 
    ++ " EL: " ++ show endLine
    ++ " EC: " ++ show endCol

shortNodeData :: AST a -> String
shortNodeData (AST h _) =
  case getConstructorQName h of
    Nothing -> getTypeQName h ++
      case getOwnJSON h of
        Nothing -> ""
        Just v -> " " ++ (LBC8.unpack $ encode v)
    Just x -> x++"("++getTypeQName h++")"


reportAST :: AST SpanInfo -> IO ()
reportAST = reportASTIndent ""

reportASTIndent :: String -> AST SpanInfo -> IO ()
reportASTIndent indent ast@(AST h t) = do
    
    putStr (indent ++ " " ++ shortNodeData ast ++ " ")
    putStr (fromMaybe " " $ ((fmap showSpanInfo) (getInfo h)))
    putStrLn ""

    let newIndent = indent ++ "  "
    forM_ t (reportASTIndent newIndent . snd)

reportASTIndentANSI :: String -> AST SpanInfo -> IO ()
reportASTIndentANSI indent ast@(AST h t) = do

    when (isWrapper ast) (setSGR [SetColor Foreground Dull Yellow])
    putStr (indent ++ " " ++ shortNodeData ast ++ " ")
    setSGR [Reset]
    fromMaybe (pure ()) $
                ((fmap ((\x -> setSGR [SetColor Foreground Vivid Blue] >> putStr x)
                         . showSpanInfo)) (getInfo h))
    setSGR [Reset]
    putStrLn ""

    let newIndent = indent ++ "  "
    forM_ t (reportASTIndentANSI newIndent . snd)


traverseAST_ :: Monad m => (forall a. ASTNode a => a -> m x) -> AST SpanInfo -> m ()
traverseAST_ f (AST h t) = 
  f h >> mapM_ (traverseAST_ f . snd) t


traverseASTBottomUp_ :: Monad m => (forall a. ASTNode a => a -> m x) -> AST SpanInfo -> m ()
traverseASTBottomUp_ f (AST h t) = 
  mapM_ (traverseASTBottomUp_ f . snd) t >> (void $ f h) 

traverseASTBottomUp_withFields :: Monad m =>
 (forall a. ASTNode a => (Maybe String , a) -> m x) -> AST SpanInfo -> m ()
traverseASTBottomUp_withFields f x = f' (Nothing , x)
   
 where
 f' (x , (AST h t)) = mapM_ f' t >> (void $ f (x , h))


gatherSpanInfos :: AST SpanInfo -> [SpanInfo]
gatherSpanInfos x = snd (runWriter $ traverseASTBottomUp_ (tell . maybeToList . getInfo) x) 


data Marker =
   StartMarker (M.Map ByteString (Set ByteString))
 | EndMarker

type LineWithMarkers = [Either ByteString Marker]



byteString2SourceWithMarkers :: ByteString -> SourceWithMarkers 
byteString2SourceWithMarkers =
   SourceWithMarkers . M.fromList . zip [0..] . (List.map (List.singleton . Left)) . C.lines


newtype SourceWithMarkers =
    SourceWithMarkers (M.Map Int LineWithMarkers)
 

insertMarker :: SourceLocation -> Marker -> SourceWithMarkers -> SourceWithMarkers
insertMarker (SourceLocation line column) marker (SourceWithMarkers sm) = 
    case M.lookup line sm of
        Just lwm -> SourceWithMarkers $ M.insert line (insertMarkerInLine column marker lwm) sm
        Nothing  -> error "No such line"
    where
        insertMarkerInLine :: Int -> Marker -> LineWithMarkers -> LineWithMarkers
        insertMarkerInLine column marker lwm = 
            let (beforeLoc, fromLoc) = splitAtLocation column lwm
            in beforeLoc ++ [Right marker] ++ fromLoc

        splitAtLocation :: Int -> LineWithMarkers -> (LineWithMarkers, LineWithMarkers)
        splitAtLocation 0 xs = ([] , xs)
        splitAtLocation _ []                 = ([], [])
        splitAtLocation column (m:ms)
            | column > B.length byteString  = let (l,r) = splitAtLocation (column - B.length byteString) ms
                                               in (m:l, r)
            | otherwise                      = ([bimap (C.take column) id m] , (bimap (C.drop column) id m) : ms)
            where
                byteString = case m of
                    Left bs     -> bs
                    Right _     -> B.empty -- we're ignoring markers when counting columns



renderSourceWithMarkersToHTML :: SourceWithMarkers -> B.ByteString
renderSourceWithMarkersToHTML (SourceWithMarkers swm) =
  BU.fromString "<!DOCTYPE html><html><head><link rel=\"stylesheet\" href=\"style.css\"><link rel=\"stylesheet\" href=\"dat\\dat.gui.css\"><script src=\"dat\\dat.gui.min.js\"></script></head><body class=\"cap-lens\"><pre id=\"code\">" `B.append` (B.concat $ List.intersperse (BU.fromString "<br/>") $ List.map renderLine $ M.toAscList swm) `B.append` BU.fromString "</pre><script src=\"main.js\"></script></body></html>"
  where
    renderLine :: (Int, LineWithMarkers) -> B.ByteString
    renderLine (_, lwm) = B.concat $ List.map renderMarkerOrByteString lwm

    renderMarkerOrByteString :: Either B.ByteString Marker -> B.ByteString
    renderMarkerOrByteString (Left bs) = bs
    renderMarkerOrByteString (Right (StartMarker attrs)) = 
      BU.fromString "<span" `B.append` renderAttributes attrs `B.append` BU.fromString ">"
    renderMarkerOrByteString (Right EndMarker) = BU.fromString "</span>"

    -- Helper function to render attributes
    renderAttributes :: M.Map ByteString (Set ByteString) -> ByteString
    renderAttributes attrs = 
      M.foldrWithKey (\key value acc -> acc `B.append` (BU.fromString " " `B.append` key `B.append` (BU.fromString "=\"") `B.append` (renderAttributeValues value) `B.append` (BU.fromString "\""))) B.empty attrs

    -- Helper function to render attribute values
    renderAttributeValues :: Set ByteString -> ByteString
    renderAttributeValues values = B.intercalate " " (Set.toAscList values)


saveHTMLToFileInTmp :: B.ByteString -> IO ()
saveHTMLToFileInTmp html = do
   -- randomName <- fmap show (randomIO :: IO Int)  -- generate random numbers until we get one that's not already a file
   let randomName = "index"
   let filepath = "/Users/marcin/www-pact/" ++ randomName ++ ".html"

   fileExists <- doesFileExist filepath
   B.writeFile filepath html 


tokenName :: Token -> String
tokenName = \case
  TokenSingleTick _ -> "TokenSingleTick" 
  TokenIdent _ -> "TokenIdent" 
  TokenNumber _ -> "TokenNumber"
  TokenString _ -> "TokenString"
  x -> show x

tokenData :: Token -> Maybe Text
tokenData = \case
  TokenSingleTick t -> Just t 
  TokenIdent t -> Just t 
  TokenNumber t -> Just t
  TokenString t -> Just t
  _ -> Nothing

  
createHTMLFile :: B.ByteString -> AST SpanInfo -> IO B.ByteString
createHTMLFile bs ast = do
  let swm = byteString2SourceWithMarkers bs
      tokens = case lexer (decodeUtf8 bs) of
                 Left _ -> []
                 Right ts -> ts
      swm' = execState (traverse_
                         (\(PosToken t si) ->
                               let startLoc = spanInfoStart si
                                   endLoc = spanInfoEnd si
                               in modify (insertMarker startLoc
                                        (StartMarker
                                          (M.fromList [("token", Set.singleton
                                                         $ BU.fromString (tokenName t))]))))
                         tokens) swm            
      swm'' = execState (traverseASTBottomUp_withFields
                         (\(mbFN , x) -> case getInfo x of
                                 Nothing -> return ()
                                 Just si -> modify (addSpan x si mbFN)) ast) swm'
      swm''' = execState (traverse_
                         (\(PosToken t si) ->
                               let startLoc = spanInfoStart si
                                   endLoc = spanInfoEnd si
                               in modify (insertMarker endLoc EndMarker))
                         tokens) swm''            
      htmlData = renderSourceWithMarkersToHTML swm'''
  return htmlData  
  -- saveHTMLToFileInTmp htmlData

  where
    addSpan :: (ASTNode a) => a -> SpanInfo -> Maybe String -> SourceWithMarkers -> SourceWithMarkers
    addSpan x si mbFN swm = 
       let startLoc = spanInfoStart si
           endLoc = spanInfoEnd si
           fnAttr = case mbFN of
                      Nothing -> []
                      Just x -> [("field",Set.singleton $ BU.fromString x)]

           attrs = M.fromList ([ ("type", Set.singleton . BU.fromString . getTypeQName $ x)
                              , ("constructor", case getConstructorQName x of
                                                 Just cname -> Set.singleton . BU.fromString $ cname
                                                 Nothing -> Set.empty)
                              ] ++ fnAttr ++ -- looseDataAttr ++
                               (M.toList (ownDataAST x) )) 
           startMarker = StartMarker attrs
           swmStart = insertMarker startLoc startMarker swm
           swmEnd = insertMarker endLoc EndMarker swmStart
       in swmEnd


pruneLocalised :: AST SpanInfo -> AST SpanInfo
pruneLocalised (AST h l) =
   AST h $ List.filter (\(_ , (AST h t)) ->
                 (isNothing (getInfo h)) && (isJust (getOwnJSON h) || (not (List.null t))))
         $ List.map (fmap pruneLocalised) l
  
collectLooseData :: AST SpanInfo -> Maybe Value
collectLooseData (AST _ l) = collectTaillData l
  where
    collectTaillData :: [(Maybe String , AST SpanInfo)] -> Maybe Value
    collectTaillData l = 
        let ob = [ (fromString $
                      (case mbFN of
                         Nothing -> show i
                         Just k -> k)
                                ++ "." ++ getTypeQName h
                                ++ (maybe "" ((:) '.')
                                     (getConstructorQName h))
                    
                   , fromJust (collectLooseDataHelper x)) 
                 | (i , (mbFN , x@(AST h _))) <- zip [0..] l
                 , isJust (collectLooseDataHelper x) ]
        in if List.null ob then Nothing else
              (Just $ Object $ KM.fromList ob)
    
    collectLooseDataHelper :: AST SpanInfo -> Maybe Value
    collectLooseDataHelper (AST h l) =
       case (getOwnJSON h , getInfo h) of
            (_ , Just _) -> Nothing
            (Just v , _) -> Just v
            (_ , _) -> collectTaillData l
