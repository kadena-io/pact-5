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
import Data.Text (Text)
import Data.Text.Lazy (toStrict)

import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as TL

import Pact.Core.Pretty hiding (dot)
import Data.Traversable


import qualified Data.ByteArray as ByteArray

import Data.ByteString.Builder

import Pact.Core.Errors
import Pact.Core.PactValue
import Pact.Core.Names
import Data.Vector(Vector,toList,(!?))

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
      zip xs (fmap PString ys)
  

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

          
          
     
    Loan           -> undefined
    Redeem         -> undefined
    Withdraw       -> undefined

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
  
  
  
-- verifyOwneraSchemaStructure :: OwneraSchemaId -> Map Field PactValue -> Either OwneraError ()
-- verifyOwneraSchemaStructure =
--    \case
--       Deposit -> 
--         \om -> do
--           dhg <- grabHashGroup om (Field "DHG")
--           _ <- grabStrField dhg (Field "nonce")
--           _ <- grabStrField dhg (Field "operation")
--           _ <- grabStrField dhg (Field "assetType")
--           _ <- grabStrField dhg (Field "assetId")
--           _ <- grabStrField dhg (Field "dstAccountType")
--           _ <- grabStrField dhg (Field "dstAccount")
--           _ <- grabStrField dhg (Field "amount")
--           Right ()

-- -- Order	Value	Type	Comment
-- -- 1	nonce	string	Explanation provided above.
-- -- 2	operation	string	"deposit"
-- -- 3	assetType	string	"fiat", "cryptocurrency", "finp2p", "custom"
-- -- 4	assetId	string	Unique identifier of the asset (blank for "custom")
-- -- 5	dstAccountType	string	"finId"
-- -- 6	dstAccount	string	Account to deposit the asset to
-- -- 7	amount	string	Amount to deposit

          
--       PrimarySale -> 
--         \om -> do
--           ahg <- grabHashGroup om (Field "AHG")
--           _ <- grabStrField ahg (Field "nonce")
--           _ <- grabStrField ahg (Field "operation")
--           _ <- grabStrField ahg (Field "assetType")
--           _ <- grabStrField ahg (Field "assetId")
--           _ <- grabStrField ahg (Field "dstAccountType")
--           _ <- grabStrField ahg (Field "dstAccount")
--           _ <- grabStrField ahg (Field "amount")
          
--           shg <- grabHashGroup om (Field "SHG")
--           _ <- grabStrField shg (Field "assetType")
--           _ <- grabStrField shg (Field "assetId")
--           _ <- grabStrField shg (Field "srcAccountType")
--           _ <- grabStrField shg (Field "srcAccount")
--           _ <- grabStrField shg (Field "dstAccountType")
--           _ <- grabStrField shg (Field "dstAccount")
--           _ <- grabStrField shg (Field "amount")
--           Right ()
-- -- Order	Value	Type	Comment
-- -- 1	nonce	[]byte	Explanation provided above.
-- -- 2	operation	string	"issue"
-- -- 3	assetType	string	"finp2p"
-- -- 4	assetId	string	Unique identifier of the asset
-- -- 5	dstAccountType	string	"finId"
-- -- 6	dstAccount	string	Investor account finId address that will receive the tokens
-- -- 7	amount	string	Asset amount
-- -- Settlement Hash Group (SHG) Structure
-- -- Represents the payment/settlement instructions.

-- -- Order	Value	Type	Comment
-- -- 1	assetType	string	"finp2p", "fiat", "cryptocurrency"
-- -- 2	assetId	string	Unique identifier of the asset
-- -- 3	srcAccountType	string	"finId"
-- -- 4	srcAccount	string	Investor account finId address for the payment asset
-- -- 5	dstAccountType	string	"finId"
-- -- 6	dstAccount	string	Issuer account finId address for the payment asset
-- -- 7	amount	string	Settlement amount

        
--       SecondarySale ->
--         \om -> do
--           ahg <- grabHashGroup om (Field "AHG")
--           _ <- grabStrField ahg (Field "nonce")
--           _ <- grabStrField ahg (Field "operation")
--           _ <- grabStrField ahg (Field "assetType")
--           _ <- grabStrField ahg (Field "assetId")
--           _ <- grabStrField ahg (Field "srcAccountType")
--           _ <- grabStrField ahg (Field "srcAccount")          
--           _ <- grabStrField ahg (Field "dstAccountType")
--           _ <- grabStrField ahg (Field "dstAccount")
--           _ <- grabStrField ahg (Field "amount")
          
--           shg <- grabHashGroup om (Field "SHG")
--           _ <- grabStrField shg (Field "assetType")
--           _ <- grabStrField shg (Field "assetId")
--           _ <- grabStrField shg (Field "srcAccountType")
--           _ <- grabStrField shg (Field "srcAccount")
--           _ <- grabStrField shg (Field "dstAccountType")
--           _ <- grabStrField shg (Field "dstAccount")
--           _ <- grabStrField shg (Field "amount")
          
--           Right ()
          
-- -- Asset Hash Group (AHG) Structure
-- -- Represents the asset instructions.

-- -- Order	Value	Type	Comment
-- -- 1	nonce	[]byte	Explanation provided abo
-- -- 2	operation	string	"transfer"
-- -- 3	assetType	string	"finp2p"
-- -- 4	assetId	string	Unique identifier of the asset
-- -- 5	srcAccountType	string	"finId"
-- -- 6	srcAccount	string	Seller account finId address that will has the tokens
-- -- 7	dstAccountType	string	"finId"
-- -- 8	dstAccount	string	Buyer account finId address that will receive the tokens
-- -- 9	amount	string	Asset amount
-- -- Settlement Hash Group (SHG) Structure
-- -- Represents the payment/settlement instructions.

-- -- Order	Value	Type	Comment
-- -- 1	assetType	string	"finp2p", "fiat", "cryptocurrency"
-- -- 2	assetId	string	Unique identifier of the asset
-- -- 3	srcAccountType	string	"finId"
-- -- 4	srcAccount	string	Buyer account findId address for the payment asset
-- -- 5	dstAccountType	string	"finId"
-- -- 6	dstAccount	string	Seller account findId address for the payment asset
-- -- 7	amount	string	Settlement amount
      
--       Loan ->
--         \om -> do
--           hg <- grabHashGroup om (Field "HG")
--           _ <- grabStrField hg (Field "nonce")
--           _ <- grabStrField hg (Field "operation")
--           _ <- grabStrField hg (Field "pledgeAssetType")
--           _ <- grabStrField hg (Field "pledgeAssetId")
--           _ <- grabStrField hg (Field "pledgeBorrowerAccountType")
--           _ <- grabStrField hg (Field "pledgeBorrowerAccountId")
--           _ <- grabStrField hg (Field "pledgeLenderAccountType")
--           _ <- grabStrField hg (Field "pledgeLenderAccountId")
--           _ <- grabStrField hg (Field "pledgeAmount")          
--           _ <- grabStrField hg (Field "moneyAssetType")
--           _ <- grabStrField hg (Field "moneyAssetId")
--           _ <- grabStrField hg (Field "moneyLenderAccountType")
--           _ <- grabStrField hg (Field "moneyLenderAccountId")
--           _ <- grabStrField hg (Field "moneyBorrowerAccountType")
--           _ <- grabStrField hg (Field "moneyBorrowerAccountId")
--           _ <- grabStrField hg (Field "borrowedMoneyAmount")
--           _ <- grabStrField hg (Field "returnedMoneyAmount")
--           _ <- grabStrField hg (Field "openTime")
--           _ <- grabStrField hg (Field "closeTime")
--           Right ()

-- -- Order	Value	Type	Comment
-- -- 1	nonce	[]byte	Explanation provided above.
-- -- 2	operation	string	"loan"
-- -- 3	pledgeAssetType	string	"finp2p"
-- -- 4	pledgeAssetId	string	Unique identifier of the asset
-- -- 5	pledgeBorrowerAccountType	string	"finId"
-- -- 6	pledgeBorrowerAccountId	string	Borrower account
-- -- 7	pledgeLenderAccountType	string	"finId"
-- -- 8	pledgeLenderAccountId	string	Lender account
-- -- 9	pledgeAmount	string	Pledged asset amount
-- -- 10	moneyAssetType	string	"fiat"
-- -- 11	moneyAssetId	string	Unique identified of the asset
-- -- 12	moneyLenderAccountType	string	"finId"
-- -- 13	moneyLenderAccountId	string	Lender account
-- -- 14	moneyBorrowerAccountType	string	"finId"
-- -- 15	moneyBorrowerAccountId	string	Borrower account
-- -- 16	borrowedMoneyAmount	string	Lender settlement amount
-- -- 17	returnedMoneyAmount	string	Returned money at maturity, which includes applicable interest
-- -- 18	openTime	string	Settlement time for Loan, EPOCH in seconds
-- -- 19	closeTime	string	Maturity time for Loan, EPOCH in seconds
      
--       Redeem -> 
--         \om -> do
--           ahg <- grabHashGroup om (Field "AHG")
--           _ <- grabStrField ahg (Field "nonce")
--           _ <- grabStrField ahg (Field "operation")
--           _ <- grabStrField ahg (Field "assetType")
--           _ <- grabStrField ahg (Field "assetId")
--           _ <- grabStrField ahg (Field "srcAccountType")
--           _ <- grabStrField ahg (Field "srcAccount")          
--           _ <- grabStrField ahg (Field "amount")
          
--           shg <- grabHashGroup om (Field "SHG")
--           _ <- grabStrField shg (Field "assetType")
--           _ <- grabStrField shg (Field "assetId")
--           _ <- grabStrField shg (Field "srcAccountType")
--           _ <- grabStrField shg (Field "srcAccount")
--           _ <- grabStrField shg (Field "dstAccountType")
--           _ <- grabStrField shg (Field "dstAccount")
--           _ <- grabStrField shg (Field "amount")
          
--           Right ()


-- -- Asset Hash Group (AHG) Structure
-- -- Represents the asset instructions.

-- -- Order	Value	Type	Comment
-- -- 1	nonce	[]byte	Explanation provided above.
-- -- 2	operation	string	"redeem"
-- -- 3	assetType	string	"finp2p"
-- -- 4	assetId	string	Unique identifier of the asset
-- -- 5	srcAccountType	string	"finId"
-- -- 6	srcAccount	string	Investor account finId address
-- -- 7	amount	string	Asset amount
-- -- Settlement Hash Group (SHG) Structure
-- -- Represents the payment/settlement instructions.

-- -- Order	Value	Type	Comment
-- -- 1	assetType	string	"finp2p", "fiat", "cryptocurrency"
-- -- 2	assetId	string	Unique identifier of the asset
-- -- 3	srcAccountType	string	"finId"
-- -- 4	srcAccount	string	Issuer account findId address for the payment
-- -- 5	dstAccountType	string	"finId"
-- -- 6	dstAccount	string	Investor account finId address that will receive the funds
-- -- 7	amount	string	Settlement amount
      
--       Withdraw -> 
--         \om -> do
--           dhg <- grabHashGroup om (Field "HG")
--           _ <- grabStrField dhg (Field "nonce")
--           _ <- grabStrField dhg (Field "operation")
--           _ <- grabStrField dhg (Field "assetType")
--           _ <- grabStrField dhg (Field "assetId")
--           _ <- grabStrField dhg (Field "srcAccountType")
--           _ <- grabStrField dhg (Field "srcAccount")
--           _ <- grabStrField dhg (Field "dstAccountType")
--           _ <- grabStrField dhg (Field "dstAccount")
--           _ <- grabStrField dhg (Field "amount")
--           Right ()

-- -- Order	Value	Type	Comment
-- -- 1	nonce	string	Explanation provided above.
-- -- 2	operation	string	"withdraw"
-- -- 3	assetType	string	"fiat", "cryptocurrency", "finp2p", "custom"
-- -- 4	assetId	string	Unique identifier of the asset (blank for "custom")
-- -- 5	srcAccountType	string	"finId"
-- -- 6	srcAccount	string	Source account to withdraw funds from
-- -- 5	dstAccountType	string	"finId"
-- -- 6	dstAccount	string	Account to deposit the asset to
-- -- 7	amount	string	string representation of the amount

-- -- HG = hash('SHA3-256', [fields by order]);
-- -- hashList = hash('SHA3-256', [HG]);
-- -- Signature = sign(sender private secp256k1 key, hashList)
