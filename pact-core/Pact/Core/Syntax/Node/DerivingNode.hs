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


module Pact.Core.Syntax.Node.DerivingNode where

import Control.Lens hiding (List, op, _head)
import Control.Monad.State
import Data.Foldable(fold)
import Data.Text(Text)
import Data.List.NonEmpty(NonEmpty(..))
import Data.List(intersperse)

import qualified Data.List.NonEmpty as NE

import Control.Monad (void)
import Data.Functor.Const()

import Data.Aeson.Types (Value)

import Data.Aeson


import qualified Language.Haskell.TH as TH
import Control.Lens.TH (makePrisms)
import Data.Maybe (catMaybes, listToMaybe, Maybe(Just))
import Control.Monad (forM,replicateM, forM_)

import Pact.Core.Info

import Language.Haskell.TH
import Language.Haskell.TH.Syntax hiding (lift)
import Control.Monad.State


import Data.Map as Map
import Data.List as List
import Data.List.Split (splitOn)

import Pact.Core.Syntax.Node.Node

-- Get a list of module/type from a fully qualified name
moduleTypeFromName :: String -> (String, String)
moduleTypeFromName name = 
    let parts = splitOn "." name
        moduleName = intercalate "." (init parts)
        typeName = last parts
    in (moduleName, typeName)

-- Partition the given list into module/types and external modules
partitionTypesExts :: [String] -> ([(String, String)], [String])
partitionTypesExts names = 
    let (pactTypes, otherModules) = List.partition (\s -> "Pact" `isPrefixOf` s) names
    in (List.map moduleTypeFromName pactTypes, otherModules)

-- Produce final tuples for module/types and external modules
produceTuples :: [String] -> ((Map String [String]), [String])
produceTuples names = 
  let (moduleTypes, externs) = partitionTypesExts names
      moduleTypeMap = List.foldl' (\m (moduleName, typeName) -> Map.insertWith (++) moduleName [typeName] m) Map.empty moduleTypes
  in (moduleTypeMap, externs)

findTypes :: Name -> StateT [Name] Q ()
findTypes name = do
    visited <- get
    if name `elem` visited then return ()
    else do
        lift $ runIO $ putStrLn $ "Visiting: " ++ show name
        modify (name:)
        info <- lift $ reify name
        case info of
            TyConI dec -> case dec of
                DataD    _ _ _ _ constructors _ -> mapM_ findTypesInCon constructors
                NewtypeD _ _ _ _ constructor _ -> findTypesInCon constructor
                TySynD   _ _ typ             -> findTypesInType typ
                _                            -> return ()
            _ -> return ()

findTypesInCon :: Con -> StateT [Name] Q ()
findTypesInCon constructor = case constructor of
    NormalC conName st        -> mapM_ (findTypesInType . snd) st
    RecC conName varSt        -> mapM_ (findTypesInType . trd) varSt
    InfixC st1 conName st2    -> findTypesInType (snd st1) >> findTypesInType (snd st2)
    ForallC _ _ con           -> findTypesInCon con
    GadtC _ st _              -> mapM_ (findTypesInType . snd) st
    RecGadtC _ varSt _        -> mapM_ (findTypesInType . trd) varSt
    where trd (_,_,x) = x

findTypesInType :: Type -> StateT [Name] Q ()
findTypesInType typ = case typ of
    ForallT _ _ t       -> findTypesInType t
    AppT t1 t2          -> findTypesInType t1 >> findTypesInType t2
    SigT t _            -> findTypesInType t
    InfixT t1 name t2   -> findTypesInType t1 >> findTypesInType t2 >> findTypes name
    UInfixT t1 name t2  -> findTypesInType t1 >> findTypesInType t2 >> findTypes name
    ParensT t           -> findTypesInType t
    AppKindT t k        -> findTypesInType t
    ImplicitParamT _ t  -> findTypesInType t
    ConT name           -> findTypes name
    _                   -> return ()

traceTypes :: Name -> Q (Map String [String], [String]) -- change from Q [Name] to Q Exp
traceTypes name = do
    list <- execStateT (findTypes name) []
    return $ produceTuples (List.map show list) 



-- Function to generate an ASTNode instance
-- Example usage within your TH scope would be:
-- $(deriveASTNode ''YourDataTypeNameHere)
deriveASTNode :: Name -> Q [Dec]
deriveASTNode typeName = reify typeName >>= f


  where
  f = \case 
    TyConI (DataD _ name typeVars _ constructors _) -> do
      
      let -- Define the instance type (ASTNode YourType)
          
          -- Generate type variable names (a, b, c, ...)
          typeVarsNames = List.map (\(k , _) -> mkName ("a" ++ show k)) (zip ([0..]) typeVars)
          apliedTy = List.foldl AppT (ConT name) (List.map VarT typeVarsNames)
          instanceType = AppT (ConT ''ASTNode) apliedTy 
              -- case mbApply of
              -- False -> AppT (ConT ''ASTNode) (ConT name) 
              -- True -> AppT (ConT ''ASTNode) (AppT (ConT name) (VarT typeVar))
          context = [ AppT (ConT ''ASTNode) (VarT typeVar) | typeVar <- typeVarsNames ] 
          -- Function to create pattern matches for the `getTail` clause
          makeGetTailClause (NormalC ctorName fields) = do
            -- Generate names for the constructor fields (x0, x1, ..., xn)
            fieldNames <- replicateM (length fields) (newName "x")
            -- Create the pattern for the constructor (e.g., SomeName x0 x1 x2)
            let patternE = ConP ctorName [] (List.map VarP fieldNames)
            -- Generate the `toAST` call for each patterm (e.g., [ toAST x0, toAST x1, toAST x2 ])
            let body = ListE $ List.map (\name ->
                                         TupE [ Just ((ConE 'Nothing))
                                       ,  Just (AppE (VarE 'toAST) (VarE name))]) fieldNames
            -- Create a match clause (e.g., match (SomeName x0 x1 x2) (normalB [ toAST x0, toAST x1, toAST x2 ]) [])
            return $ ([Clause [patternE] (NormalB body) ([])]
                      , [Clause [patternE] (NormalB ((ConE 'Just) `AppE` LitE (StringL (nameBase ctorName)))) ([])])

          makeGetTailClause (RecC ctorName fields) = do
            -- Generate names for the constructor fields (x0, x1, ..., xn)
            fieldNames <- replicateM (length fields) (newName "x")
            -- Create the pattern for the constructor (e.g., SomeName x0 x1 x2)
            let patternE = ConP ctorName [] (List.map VarP fieldNames)
            -- Generate the `toAST` call for each patterm (e.g., [ toAST x0, toAST x1, toAST x2 ])
            let body = ListE $ List.map
                           (\(name , (fName , _ , _)) ->
                                TupE [ Just ((ConE 'Just) `AppE` LitE (StringL (nameBase fName)))
                                       ,  Just (AppE (VarE 'toAST) (VarE name))])
                             (zip fieldNames fields)
            -- Create a match clause (e.g., match (SomeName x0 x1 x2) (normalB [ toAST x0, toAST x1, toAST x2 ]) [])
            return $ ([Clause [patternE] (NormalB body) ([])]
                      , [Clause [patternE] (NormalB ((ConE 'Just) `AppE` LitE (StringL (nameBase ctorName)))) ([])])
          makeGetTailClause (GadtC x y _) = fmap mconcat $ mapM (\x -> makeGetTailClause (NormalC x y)) x
          makeGetTailClause (RecGadtC x y _) = fmap mconcat $  mapM (\x -> makeGetTailClause (RecC x y)) x

          makeGetTailClause x = fail $
            "Only normal constructors with fields are supported for deriveASTNode" ++ "\n" ++ show x

      -- Create `getTail` function clauses for each constructor
      isHOD <- isInstanceOfHasOwnData apliedTy
      let odADT =
             if isHOD
             then (VarE 'ownData)
             else (VarE 'emptyOwnData)
      (getTailClauses , getConstructorQNameClauses) <-
          fmap mconcat $ mapM makeGetTailClause constructors


      -- Define the instance declaration for ASTNode
      let astNodeInstance = InstanceD Nothing context instanceType
            [ FunD 'getTailWithMbNamesAll getTailClauses ,
              FunD 'getTypeQName [Clause [WildP] (NormalB (LitE (StringL (nameBase name)))) ([])],
              FunD 'getConstructorQName getConstructorQNameClauses,
              FunD 'ownDataAST [Clause [] (NormalB odADT) ([])]
               
              
            ]

      -- Return the instance declaration
      return [astNodeInstance]
    TyConI (NewtypeD v1 name typeVars v2 constructor v3) ->
       f (TyConI (DataD v1 name typeVars v2 [constructor] v3))
      
    x -> fail ("unsuported definition" ++ show x)

generateASTNodes :: Name -> Q [Dec]
generateASTNodes name = do
    -- first use the `traceTypes` function
    (typesMap, _) <- traceTypes name
    -- create fully qualified names
    let types = [mkName (m ++ "." ++ t) | (m, ts) <- Map.toList typesMap, t <- ts]
    -- apply `deriveASTNode` to each type and concatenate the results
    fmap concat $ mapM deriveASTNode types

