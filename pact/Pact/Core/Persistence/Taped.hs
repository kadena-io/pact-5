
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}

-- |
-- Module      :  Pact.Persistence.Taped
-- Copyright   :  (C) 2024 Kadena
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edmund Noble <edmund@kadena.io>
--
-- Tapes for recording and playing back database access.
--
module Pact.Core.Persistence.Taped (
   TxTapeElem(..),
   TxTape(..),
   _TxTape,
   TxTapeZipper(..),
   dbFromTape
   )
    where

import Control.Concurrent.MVar
import Control.DeepSeq (NFData(..))
import Control.Lens

import Data.Aeson
import Data.Maybe
import GHC.Generics (Generic)

import Pact.Core.Persistence
import Data.Text (Text)
import Pact.Core.Names

import Pact.Core.Legacy.LegacyPactValue
import Control.Monad.IO.Class
import Pact.Core.Builtin (CoreBuiltin)

type Domain' k v = Domain k v CoreBuiltin ()

eqForDomain :: Domain' k v -> ((Eq k, Eq v) => r) -> r
eqForDomain (DUserTables _) r = r
eqForDomain DKeySets r = r
eqForDomain DModules r = r
eqForDomain DNamespaces r = r
eqForDomain DDefPacts r = r

showForDomain :: Domain' k v -> ((Show k, Show v) => r) -> r
showForDomain (DUserTables _) r = r
showForDomain DKeySets r = r
showForDomain DModules r = r
showForDomain DNamespaces r = r
showForDomain DDefPacts r = r

nfDataForDomain :: Domain' k v -> ((NFData k, NFData v) => r) -> r
nfDataForDomain (DUserTables _) r = r
nfDataForDomain DKeySets r = r
nfDataForDomain DModules r = r
nfDataForDomain DNamespaces r = r
nfDataForDomain DDefPacts r = r

jsonDecodeForDomain
    :: Domain' k v
    -> ((FromJSON (Legacy k), FromJSON (Legacy v)) => r)
    -> r
jsonDecodeForDomain (DUserTables _) r = r
jsonDecodeForDomain DKeySets r = r
jsonDecodeForDomain DModules r = r
jsonDecodeForDomain DNamespaces r = r
jsonDecodeForDomain DDefPacts r = r

constraintsForDomain
    :: Domain' k v
    -> (
    (Show k, Show v
    , Eq k, Eq v
    , NFData k, NFData v
    -- , J.Encode (Legacy k), J.Encode (Legacy v)
    , FromJSON (Legacy k), FromJSON (Legacy v)
    ) => r) -> r
constraintsForDomain d r
    = eqForDomain d
    $ showForDomain d
    $ nfDataForDomain d
    -- $ jsonEncodeForDomain d
    $ jsonDecodeForDomain d
    $ r

-- Other operations not stored on tape because they're not available outside
-- the repl and local modes.
data TxTapeElem
  = forall k v. TxTapeWrite !WriteType !(Domain' k v) !k !v
  | forall k v. TxTapeRead !(Domain' k v) !k !(Maybe v)
  | forall k v. TxTapeKeys !(Domain' k v) ![k]
  | TxTapeCreateTable !TableName

checkDomain
  :: Domain' k1 v1
  -> Domain' k2 v2
  -> r
  -> ((k1 ~ k2, v1 ~ v2) => r)
  -> r
checkDomain domainActual domainExpected no yes =
    case (domainActual, domainExpected) of
      (DUserTables tnActual, DUserTables tnExpected)
        | tnActual == tnExpected -> yes
      (DKeySets, DKeySets) -> yes
      (DModules, DModules) -> yes
      (DNamespaces, DNamespaces) -> yes
      (DDefPacts, DDefPacts) -> yes
      _ -> no

instance Eq TxTapeElem where
  TxTapeWrite wt d k v == TxTapeWrite wt' d' k' v' = eqForDomain d $ checkDomain d d' False $
    wt == wt' && k == k' && v == v'
  TxTapeRead d k mv == TxTapeRead d' k' mv' = eqForDomain d $ checkDomain d d' False $
    k == k' && mv == mv'
  TxTapeKeys d ks == TxTapeKeys d' ks' = eqForDomain d $ checkDomain d d' False $
    ks == ks'
  TxTapeCreateTable tn == TxTapeCreateTable tn' =
    tn == tn'
  _ == _ = False

instance NFData TxTapeElem where
  rnf (TxTapeWrite wt d k v) = nfDataForDomain d $
    rnf wt `seq` rnf d `seq` rnf k `seq` rnf v
  rnf (TxTapeRead d k v) = nfDataForDomain d $
    rnf d `seq` rnf k `seq` rnf v
  rnf (TxTapeKeys d ks) = nfDataForDomain d $
    rnf d `seq` rnf d `seq` rnf ks
  rnf (TxTapeCreateTable tn) =
    rnf tn

instance Show TxTapeElem where
  showsPrec :: Int -> TxTapeElem -> ShowS
  showsPrec p (TxTapeWrite wt d k v) = showForDomain d $
    showParen (p > 10)
    $ showString "Write "
    . foldr (\x r -> x . showString " " . r) id
    [ showsPrec 11 wt
    , showsPrec 11 d
    , showsPrec 11 k
    , showsPrec 11 v
    ]
  showsPrec p (TxTapeRead d k mv) = showForDomain d $
    showParen (p > 10)
    $ showString "Read "
    . foldr (\x r -> x . showString " " . r) id
    [ showsPrec 11 d
    , showsPrec 11 k
    , showsPrec 11 mv
    ]
  showsPrec p (TxTapeKeys d ks) = showForDomain d $
    showParen (p > 10)
    $ showString "Keys "
    . foldr (\x r -> x . showString " " . r) id
    [ showsPrec 11 d
    , showsPrec 11 ks
    ]
  showsPrec p (TxTapeCreateTable tn) =
    showParen (p > 10)
    $ showString "CreateTable "
    . foldr (\x r -> x . showString " " . r) id
    [ showsPrec 11 tn
    ]

newtype TxTape = TxTape [TxTapeElem]
  deriving stock (Eq, Generic)
  deriving newtype (Show, NFData)

makePrisms ''TxTape

data TxTapeZipper = TxTapeZipper !TxTape !(Maybe TxTapeElem) !TxTape

dbFromTape
    :: MVar TxTapeZipper -> PactDb CoreBuiltin ()
dbFromTape tapeRef = PactDb
  { _pdbRead = \d k -> modifyTape $ doOperation "read" $ \case
    TxTapeRead d' k' mv -> Just $ showForDomain d $ eqForDomain d $ do
      checkDomain d d' (domainMismatch "read" d d') $ do
          checkAll mv $ catMaybes
            [ check "key" k k'
            ]
    _ -> Nothing

  , _pdbWrite = \wt d k v -> modifyTape $ doOperation "write" $ \case
    TxTapeWrite wt' d' k' v' -> Just $ constraintsForDomain d $ do
      checkDomain d d' (domainMismatch "write" d d') $ do
          checkAll () $ catMaybes
            [ check "key" k k'
            , check "value" v v'
            , check "write type" wt wt'
            ]
    _ -> Nothing

  , _pdbKeys = \d -> modifyTape $ doOperation "keys" $ \case
    TxTapeKeys d' ks -> Just $
      checkDomain d d' (domainMismatch "keys" d d') $ return ks
    _ -> Nothing

  , _pdbTxIds = \_ _ -> unsupportedOperation "_txids"
  , _pdbCreateUserTable = \tn -> modifyTape $ doOperation "create user table" $ \case
        TxTapeCreateTable tn' -> Just $ do
            checkAll () $ catMaybes
                [ check "table name" tn tn'
                ]
        _ -> Nothing
  , _pdbBeginTx = \_ -> unsupportedOperation "_beginTx"
  , _pdbCommitTx = unsupportedOperation "_commitTx"
  , _pdbRollbackTx = unsupportedOperation "_rollbackTx"
  , _pdbGetTxLog = \_ _ -> unsupportedOperation "_getTxLog"
  , _pdbPurity = PImpure
  }
  where
  unsupportedOperation operationName =
    throwEvalError $ "dbFromTape: unsupported operation " <> operationName
  mismatchedOperation :: String -> TxTapeElem -> IO r
  mismatchedOperation actualOp expectedOp = throwEvalError $ unwords
    [ "dbFromTape: attempted to", actualOp, "but next operation on tape was", show expectedOp ]
  throwEvalError :: String -> IO r
  -- TODO: improve when PactDb methods all return in an error monad
  throwEvalError msg = error msg
  domainMismatch operation domainActual domainExpected = throwEvalError $ unlines
    [ "dbFromTape: tape mismatch"
    , "operation " <> operation
    , unwords ["domain", "actual", show domainActual, "expected", show domainExpected]
    ]
  check :: (Show a, Eq a) => String -> a -> a -> Maybe String
  check name actual expected
    | actual == expected = Nothing
    | otherwise = Just $ unwords [name, "actual", show actual, "expected", show expected]
  checkAll :: r -> [String] -> IO r
  checkAll r [] = return r
  checkAll _ errs = throwEvalError $ unlines $ "dbFromTape: tape mismatch" : errs
  modifyTape
    :: MonadIO m
    => (TxTapeZipper -> IO (TxTapeZipper, a))
    -> m a
  modifyTape k = liftIO $ do
    modifyMVar tapeRef $ \tape -> do
      (tape', r) <- k tape
      return (tape', r)
  doOperation
    :: String
    -> (TxTapeElem -> Maybe (IO a))
    -> TxTapeZipper
    -> IO (TxTapeZipper, a)
  doOperation msg _ (TxTapeZipper _ Nothing _) =
    throwEvalError $ "dbFromTape: attempted " <> msg <> " beyond tape's end"
  doOperation msg g (TxTapeZipper (TxTape l) (Just c) r) = do
    res <- fromMaybe (mismatchedOperation msg c) $ g c
    return $ (,res) $ case r of
      TxTape (c' : r') ->
        TxTapeZipper (TxTape (c : l)) (Just c') (TxTape r')
      TxTape [] ->
        TxTapeZipper (TxTape (c : l)) Nothing (TxTape [])

instance FromJSON TxTapeElem where
  parseJSON = withObject "TxTapeElem" $ \o -> do
    operation <- o .: "op"
    case operation :: Text of
      "write" -> parseWrite o
      "read" -> parseRead o
      "keys" -> parseKeys o
      "createTable" -> parseCreateTable o
      _ -> fail "invalid op, expected write, read, keys, or createTable"
    where
    parseWrite o =
      o .: "domain" >>= \case
        Legacy (SomeDomain d) -> jsonDecodeForDomain d $ do
          wt <- _unLegacy <$> o .: "writeType"
          k <- _unLegacy <$> o .: "key"
          v <- _unLegacy <$> o .: "value"
          return $ TxTapeWrite wt d k v
    parseRead o =
      o .: "domain" >>= \case
        Legacy (SomeDomain d) -> jsonDecodeForDomain d $ do
          k <- _unLegacy <$> o .: "key"
          mv <- fmap _unLegacy <$> o .: "value"
          return $ TxTapeRead d k mv
    parseKeys o =
      o .: "domain" >>= \case
        Legacy (SomeDomain d) -> jsonDecodeForDomain d $ do
          ks <- fmap _unLegacy <$> o .: "keys"
          return $ TxTapeKeys d ks
    parseCreateTable o = do
      tn <- o .: "tableName"
      Legacy mn <- o .: "moduleName"
      return $ TxTapeCreateTable $ TableName
        { _tableName = tn
        , _tableModuleName = mn
        }
