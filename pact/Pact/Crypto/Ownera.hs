{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PackageImports #-}

-- | Implementation of Hyperlane natives.
module Pact.Crypto.Ownera
  ( OwneraSchemaId(..)
  , verifyOwneraSchemaStructure
  , decodeFinApiData
  , hashListSchema
  , owneraSchemaIdToText
  , textToOwneraSchemaId
  ) where

import Control.Lens ((^?), at, _Just)
import Control.Monad (when)

import Data.Map (Map,fromList,lookup)
import Data.Text (Text,unpack)
import Data.Text.Lazy (toStrict)

import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as TL

import Pact.Core.Pretty hiding (dot)
import Data.Traversable
import Data.Decimal()


import qualified Data.ByteArray as ByteArray

import Data.ByteString.Builder

import Pact.Core.Errors
import Pact.Core.PactValue
import Pact.Core.Names
import Data.Vector(Vector,toList,(!?))

import Text.Read(readMaybe)


import Crypto.Hash

data OwneraSchemaId =
      Deposit
    | PrimarySale
    | SecondarySale
    | Loan
    | Redeem
    | Withdraw

-- Function to convert OwneraSchemaId to lowercase Text with hyphens, using LambdaCase
owneraSchemaIdToText :: OwneraSchemaId -> Text
owneraSchemaIdToText = \case
    Deposit        -> "deposit"
    PrimarySale    -> "primary-sale"
    SecondarySale  -> "secondary-sale"
    Loan           -> "loan"
    Redeem         -> "redeem"
    Withdraw       -> "withdraw"

-- Map to associate Text representation with OwneraSchemaId
owneraSchemaIdMap :: Map Text OwneraSchemaId
owneraSchemaIdMap = fromList
    [ ("deposit", Deposit)
    , ("primary-sale", PrimarySale)
    , ("secondary-sale", SecondarySale)
    , ("loan", Loan)
    , ("redeem", Redeem)
    , ("withdraw", Withdraw)
    ]

owneraOperationNameSchemaIdMap :: Map Text OwneraSchemaId
owneraOperationNameSchemaIdMap = fromList
    [ ("deposit", Deposit)
    , ("issue", PrimarySale)
    , ("transfer", SecondarySale)
    , ("loan", Loan)
    , ("redeem", Redeem)
    , ("withdraw", Withdraw)
    ]


textToOwneraSchemaId :: Text -> Either OwneraError OwneraSchemaId
textToOwneraSchemaId txt =
    case Data.Map.lookup txt owneraSchemaIdMap of
        Just schemaId -> Right schemaId
        Nothing       -> Left $ OtherOwneraError $ "Invalid schema ID: " <> txt


operationToOwneraSchemaId :: Text -> Either OwneraError OwneraSchemaId
operationToOwneraSchemaId txt =
    case Data.Map.lookup txt owneraOperationNameSchemaIdMap of
        Just schemaId -> Right schemaId
        Nothing       -> Left $ OtherOwneraError $ "Invalid operation name: " <> txt


grabStrField :: Map Field PactValue -> Field ->  Either OwneraError Text
grabStrField m key = case m ^? at key . _Just . _PString of
  Nothing -> Left (OwneraErrorFailedToFindKey key)
  Just t -> Right t

grabObjField :: Map Field PactValue -> Field ->  Either OwneraError (Map Field PactValue)
grabObjField m key = case m ^? at key . _Just . _PObject of
  Nothing -> Left (OwneraErrorFailedToFindKey key)
  Just t -> Right t

grabListField :: Map Field PactValue -> Field ->  Either OwneraError (Vector PactValue)
grabListField m key = case m ^? at key . _Just . _PList of
  Nothing -> Left (OwneraErrorFailedToFindKey key)
  Just t -> Right t

-- grabHashGroup :: Map Field PactValue -> Field ->  Either OwneraError (Map Field PactValue)
-- grabHashGroup m key = case m ^? at key . _Just of
--   Just (PObject o) -> Right o
--   _ -> Left (OwneraErrorFailedToFindHashGroup key)
  

newtype HashListGroupSchema = HashListGroupSchema [Field]
newtype HashListSchema = HashListSchema [(Field , HashListGroupSchema)]

data HashListData =  HashListData
 { _hldHash :: Text
 , _hldData :: [Text] 
 }

data HashListsData = HashListsData
 { _hlsdSignature :: Text
 , _hlsdHash      :: Text
 , _hlsdData      :: [HashListData]
 }



verifyHashListDataHash :: HashListData -> Either OwneraError ()
verifyHashListDataHash hld =
  let calculatedHash = toStrict $ TL.decodeUtf8 $ toLazyByteString $ byteStringHex $ ByteArray.convert $ hashFinalize $
             hashUpdates (hashInitWith SHA3_256 ) $
                 (map T.encodeUtf8 (_hldData hld))
  in if calculatedHash == _hldHash hld
     then Right ()
     else Left $ OtherOwneraError $ calculatedHash <> " =/= " <>  _hldHash hld 
        
hashListDataAsPactValue :: HashListGroupSchema -> HashListData -> PactValue
hashListDataAsPactValue (HashListGroupSchema xs) (HashListData _ ys) =
   PObject $
    fromList $
      map (\x@(Field f , v) ->
            case (f , v) of
              ("amount" , PString s) -> (Field f ,
                                           case (readMaybe $ unpack s) of
                                             Just d -> PDecimal d
                                             _ -> PString s)
              _  -> x)
        (zip xs (fmap PString ys))
  

hashListsDataAsPactValue :: HashListSchema -> HashListsData -> PactValue
hashListsDataAsPactValue (HashListSchema ys) (HashListsData _ _ xs) = 
   PObject $
    fromList $
      [ (hgK ,  hashListDataAsPactValue hgS hgV) | ((hgK , hgS) , hgV) <- zip ys xs  ] 

  
hashListSchema :: OwneraSchemaId -> HashListSchema
hashListSchema = HashListSchema . hls .
  \case
    Deposit ->
     [("DHG" ,
          ["nonce"
          ,"operation"
          ,"assetType"
          ,"assetId"
          ,"dstAccountType"
          ,"dstAccount"
          ,"amount"])]
                
    PrimarySale ->
     [("AHG" ,
          [ "nonce"
          , "operation"
          , "assetType"
          , "assetId"
          , "dstAccountType"
          , "dstAccount"
          , "amount"])
          ,
      ("SHG" , 
          ["assetType"
          ,"assetId"
          ,"srcAccountType"
          ,"srcAccount"
          ,"dstAccountType"
          ,"dstAccount"
          , "amount"])]
    SecondarySale  ->
        [(("AHG"),
          ["nonce"
          ,"operation"
          ,"assetType"
          ,"assetId"
          ,"srcAccountType"
          ,"srcAccount"       
          ,"dstAccountType"
          ,"dstAccount"
          ,"amount"])
        ,(("SHG"),
           ["assetType"
           ,"assetId"
           ,"srcAccountType"
           ,"srcAccount"
           ,"dstAccountType"
           ,"dstAccount"
           ,"amount"])]

          
          
     
    Loan           ->
     [("HG",["nonce"
            ,"operation"
            ,"pledgeAssetType"
            ,"pledgeAssetId"
            ,"pledgeBorrowerAccountType"
            ,"pledgeBorrowerAccountId"
            ,"pledgeLenderAccountType"
            ,"pledgeLenderAccountId"
            ,"pledgeAmount"          
            ,"moneyAssetType"
            ,"moneyAssetId"
            ,"moneyLenderAccountType"
            ,"moneyLenderAccountId"
            ,"moneyBorrowerAccountType"
            ,"moneyBorrowerAccountId"
            ,"borrowedMoneyAmount"
            ,"returnedMoneyAmount"
            ,"openTime"
            ,"closeTime"])]
    Redeem         ->
     [("AHG",["nonce"
             ,"operation"
             ,"assetType"
             ,"assetId"
             ,"srcAccountType"
             ,"srcAccount"          
             ,"amount"])

     ,("SHG",["assetType"
             ,"assetId"
             ,"srcAccountType"
             ,"srcAccount"
             ,"dstAccountType"
             ,"dstAccount"
             ,"amount"])]

    Withdraw       ->
     [("HG" , ["nonce"
              ,"operation"
              ,"assetType"
              ,"assetId"
              ,"srcAccountType"
              ,"srcAccount"
              ,"dstAccountType"
              ,"dstAccount"
              ,"amount"])]

 where
   hls :: [(Text , [Text])] -> [(Field , HashListGroupSchema)]
   hls = fmap (\(x , y) -> (Field x , HashListGroupSchema (fmap Field y)))


recogniseSchema :: Map Field PactValue -> Either OwneraError OwneraSchemaId
recogniseSchema obj = do
  tObj <- grabObjField obj (Field "template")
  hgsLst <- grabListField tObj (Field "hashGroups")
  case hgsLst !? 0 of
    Just (PObject hg) -> do
       fLst <- grabListField hg (Field "fields")
       case fLst !? 1 of
        Just (PObject fo) -> do
          grabStrField fo (Field "value")
            >>= operationToOwneraSchemaId
        _ -> Left (OtherOwneraError ("unable tor ecognise schema, operation field missing"))
        
    _ -> Left (OtherOwneraError ("unable tor ecognise schema, first hash group missing"))
          
           
         
  
extractOfSchema :: HashListSchema -> Map Field PactValue ->
                          (Either OwneraError HashListsData)
extractOfSchema (HashListSchema hls) dObj = do
   sig <- grabStrField dObj (Field "signature")
   tObj <- grabObjField dObj (Field "template")
   _ <- grabStrField tObj (Field "type")
   h <- grabStrField tObj (Field "hash")
  
   hgsLst <- grabListField tObj (Field "hashGroups")
   accumRes <- mapAccumM 
         (curry (\case
             ([] , _) -> Left $
                 OtherOwneraError ("unexpected hash group!")
             (((_  , HashListGroupSchema hgFlds) : flds) , (PObject fldDataO)) -> do
                  hgFldsVec <- grabListField fldDataO (Field "fields")
                  h' <- grabStrField fldDataO (Field "hash")
                  ((,) flds . HashListData h') <$> consumeHashGroupFields hgFlds hgFldsVec
             (_ , _) -> Left $
                 OtherOwneraError ("hash group must by an object!")
               )) hls hgsLst
   case accumRes of
     ([] , x) -> pure (HashListsData sig h $ toList x)
     (unconsumedHGs , _) -> Left $
       OtherOwneraError ("missing hashGroups: " <>
                           (renderText' $ commaSep (fmap fst unconsumedHGs)))

 where

   
  consumeHashGroupFields :: [Field] -> Vector PactValue ->
                         Either OwneraError [Text]
  consumeHashGroupFields flds' vpv =
      mapAccumM (curry (\case
             ([] , _) -> Left $
                 OtherOwneraError ("unexpected field in hash group!")
             ((fld : flds) , (PObject fobj)) -> do
                  n <- grabStrField fobj (Field "name")
                  _ <- grabStrField fobj (Field "type")
                  v <- grabStrField fobj (Field "value")
                  if (Field n) == fld
                    then pure (flds , v)
                    else Left $ OtherOwneraError
                           ("expected field: " <> renderText fld <> " unexpectly got: " <> n)
             ((_ : _) , _) -> Left $
                 OtherOwneraError ("unexpected value!")
               )) flds' vpv
        >>= \case
              ([] , x) -> pure $ toList x
              (flds , _) -> Left $
                         OtherOwneraError ("missing fields in hashGroup: " <>
                           (renderText' $ commaSep flds))
      
     

decodeFinApiData :: OwneraSchemaId -> Map Field PactValue -> Either OwneraError PactValue
decodeFinApiData osId pKV = do
   extracted <- extractOfSchema (hashListSchema osId) pKV
   when False $
      mapM_ verifyHashListDataHash (_hlsdData extracted)
   pure $ hashListsDataAsPactValue (hashListSchema osId) extracted
     



verifyOwneraSchemaStructure :: Map Field PactValue -> Either OwneraError PactValue
verifyOwneraSchemaStructure pkV = do
  sId <- recogniseSchema pkV
  d <- decodeFinApiData sId pkV
  pure $ PObject $ fromList
       [ ((Field "verified") , (PBool True))
       , ((Field "data")     , d) ]
