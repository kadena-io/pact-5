{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}


module Pact.Core.Syntax.ParseTreeNode where

import Control.Lens hiding (List, op, _head)
import Data.Foldable(fold)
import Data.Text(Text,pack,unpack)
import Data.List.NonEmpty(NonEmpty(..))
import Data.List(intersperse)

import qualified Data.List.NonEmpty as NE

import Pact.Core.Literal
import Pact.Core.Names
import Pact.Core.Pretty
import Pact.Core.Type(PrimType(..))
import Pact.Core.Imports
import Pact.Core.Guards

import Control.Monad (void)
import Data.Functor.Const()

import Data.Aeson.Types (Value)


import qualified Language.Haskell.TH as TH
import Control.Lens.TH (makePrisms)
import Data.Maybe (catMaybes, listToMaybe)
import Control.Monad (forM)

import Control.Monad.IO.Class (liftIO)

import Pact.Core.Info

import Pact.Core.Syntax.Node.Node
import Pact.Core.Syntax.Node.DerivingNode
import Pact.Core.Syntax.ParseTree

import Data.Decimal
import Data.ByteString.Short.Internal (ShortByteString)
import Pact.Core.Hash

import Data.Aeson.Types

import qualified Data.Map.Strict as M

import Data.Set (Set)
import qualified Data.Set as Set

import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Char8 as C

import Pact.Core.Builtin

instance ASTNode Text where
  getOwnJSON x = Just $ String x 
  getTypeQName _ = "Text" 
instance ASTNode Decimal where
  getOwnJSON x = Just $ String (pack $ show x)
  getTypeQName _ = "Decimal"
instance ASTNode Integer where
  getOwnJSON x = Just $ String (pack $ show x)
  getTypeQName _ = "Integer"
instance ASTNode Bool where
  getOwnJSON x = Just $ String (pack $ show x)
  getTypeQName _ = "Bool"
instance ASTNode ShortByteString where
  getOwnJSON x = Just $ String (pack $ show x)
  getTypeQName _ = "ShortByteString"

instance HasOwnData (DefinedIdentifier i) where
  ownData x = M.fromList [("defined-name", Set.singleton (C.pack (unpack (_diText x))))] 

instance HasOwnData (CapName i) where
  ownData (CapName n _) = M.fromList $
          [("bare-name", Set.singleton (C.pack (unpack (rawParsedName n))))]

instance HasOwnData (PropertyExpr i) where
  ownData (PropSequence ((PropAtom pn _) : (PropAtom pn' _) : _) _)
      | ((rawParsedName pn) == "defproperty") = 
             M.fromList $
              [("defprop-name", Set.singleton (C.pack (unpack (rawParsedName pn'))))]
  ownData (PropAtom pn _) =
    let rpn = rawParsedName pn
        mbBI =
          case (M.lookup rpn coreBuiltinMap) of
            Nothing -> []
            Just bi ->
              [("builtin" , (Set.singleton
                         (C.pack $ show bi)))]

    in (M.fromList $
          [("bare-name", Set.singleton (C.pack (unpack rpn)))] ++ mbBI)
  ownData _ = M.empty 
          
instance HasOwnData (Expr i) where
  ownData (Var pn _) =
    let rpn = rawParsedName pn
        mbBI =
          case (M.lookup rpn coreBuiltinMap) of
            Nothing -> []
            Just bi ->
              [("builtin" , (Set.singleton
                         (C.pack $ show bi)))]

    in (M.fromList $
          [("bare-name", Set.singleton (C.pack (unpack rpn)))] ++ mbBI)
  ownData (App (Var pn' _) [(App (Var pn _) _ _) ] _)
      | ((rawParsedName pn') == "require-capability") = M.fromList $
          [("cap-name", Set.singleton (C.pack (unpack (rawParsedName pn))))
          ,("cap-op", Set.singleton "require")]

          
  -- ownData (CapabilityForm cf _) = 
  --         let (CapName pn _ ) = capName cf
  --         in M.fromList $
  --             [("cap-name", Set.singleton (C.pack (unpack (rawParsedName pn))))]

  ownData (App (Var pn' _) ((Var pn _) : _) _)
      | ((rawParsedName pn') == "with-read") = M.fromList $
          [("db-table-name", Set.singleton (C.pack (unpack (rawParsedName pn))))
          ,("db-op", Set.singleton "R")]
  ownData (App (Var pn' _) ((Var pn _) : _) _)
      | ((rawParsedName pn') == "fold-db") = M.fromList $
          [("db-table-name", Set.singleton (C.pack (unpack (rawParsedName pn))))
          ,("db-op", Set.singleton "R")]
  ownData (App (Var pn' _) ((Var pn _) : _) _)
      | ((rawParsedName pn') == "insert") = M.fromList $
          [("db-table-name", Set.singleton (C.pack (unpack (rawParsedName pn))))
          ,("db-op", Set.singleton "C")]
  ownData (App (Var pn' _) ((Var pn _) : _) _)
      | ((rawParsedName pn') == "read") = M.fromList $
          [("db-table-name", Set.singleton (C.pack (unpack (rawParsedName pn))))
          ,("db-op", Set.singleton "R")]
  ownData (App (Var pn' _) ((Var pn _) : _) _)
      | ((rawParsedName pn') == "select") = M.fromList $
          [("db-table-name", Set.singleton (C.pack (unpack (rawParsedName pn))))
          ,("db-op", Set.singleton "R")]
  ownData (App (Var pn' _) ((Var pn _) : _) _)
      | ((rawParsedName pn') == "update") = M.fromList $
          [("db-table-name", Set.singleton (C.pack (unpack (rawParsedName pn))))
          ,("db-op", Set.singleton "U")]
  ownData (App (Var pn' _) ((Var pn _) : _) _)
      | ((rawParsedName pn') == "with-default-read") = M.fromList $
          [("db-table-name", Set.singleton (C.pack (unpack (rawParsedName pn))))
          ,("db-op", Set.singleton "R")]
  ownData (App (Var pn' _) ((Var pn _) : _) _)
      | ((rawParsedName pn') == "write") = M.fromList $
          [("db-table-name", Set.singleton (C.pack (unpack (rawParsedName pn))))
          ,("db-op", Set.singleton "U")]
  
          
  ownData _ = M.empty

  

  
$(generateASTNodes (''TopLevel))
