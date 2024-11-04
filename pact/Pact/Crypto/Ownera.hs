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

-- | Implementation of Hyperlane natives.
module Pact.Crypto.Ownera
  ( OwneraSchemaId(..)
  , verifyOwneraSchemaStructure
  ) where

import Control.Lens ((^?), at, _Just)
-- import Control.Monad (unless)
-- import Control.Monad.Except (throwError)
-- import Data.Bifunctor (first)
-- import Data.Binary.Get (Get)
-- import Data.Binary.Get qualified as Bin
-- import Data.ByteString (ByteString)
-- import Data.ByteString qualified as BS
-- import Data.ByteString.Builder (Builder)
-- import Data.ByteString.Builder qualified as BB
-- import Data.ByteString.Lazy qualified as BL
-- import Data.ByteString.Short qualified as BSS
-- import Data.Decimal (Decimal)
import Data.Map (Map)
-- import Data.Ratio ((%))
import Data.Text (Text)
-- import Data.Text qualified as Text
-- import Data.Text.Encoding qualified as Text
-- import Data.Text.Read qualified as Text
-- import Data.WideWord.Word256 (Word256(..))
-- import Data.Word (Word8, Word16, Word32)
-- import Ethereum.Misc (keccak256, _getKeccak256Hash, _getBytesN)
-- import Pact.JSON.Decode qualified as J
-- import Pact.Core.StableEncoding 

import Pact.Core.Errors
import Pact.Core.PactValue
import Pact.Core.Names
-- import Pact.Core.Literal
-- import Pact.Core.Hash
-- import qualified Data.Map as M

data OwneraSchemaId =
      Deposit
    | PrimarySale
    | SecondarySale
    | Loan
    | Redeem
    | Withdraw


-- Map Field PactValue -> Either HyperlaneError HyperlaneMessage

grabStrField :: Map Field PactValue -> Field ->  Either OwneraError Text
grabStrField m key = case m ^? at key . _Just . _PString of
  Nothing -> Left (OwneraErrorFailedToFindKey key)
  Just t -> Right t

grabHashGroup :: Map Field PactValue -> Field ->  Either OwneraError (Map Field PactValue)
grabHashGroup m key = case m ^? at key . _Just of
  Just (PObject o) -> Right o
  _ -> Left (OwneraErrorFailedToFindHashGroup key)
  



verifyOwneraSchemaStructure :: OwneraSchemaId -> Map Field PactValue -> Either OwneraError ()
verifyOwneraSchemaStructure =
   \case
      Deposit -> 
        \om -> do
          dhg <- grabHashGroup om (Field "DHG")
          _ <- grabStrField dhg (Field "nonce")
          _ <- grabStrField dhg (Field "operation")
          _ <- grabStrField dhg (Field "assetType")
          _ <- grabStrField dhg (Field "assetId")
          _ <- grabStrField dhg (Field "dstAccountType")
          _ <- grabStrField dhg (Field "dstAccount")
          _ <- grabStrField dhg (Field "amount")
          Right ()

-- Order	Value	Type	Comment
-- 1	nonce	string	Explanation provided above.
-- 2	operation	string	"deposit"
-- 3	assetType	string	"fiat", "cryptocurrency", "finp2p", "custom"
-- 4	assetId	string	Unique identifier of the asset (blank for "custom")
-- 5	dstAccountType	string	"finId"
-- 6	dstAccount	string	Account to deposit the asset to
-- 7	amount	string	Amount to deposit

          
      PrimarySale -> 
        \om -> do
          ahg <- grabHashGroup om (Field "AHG")
          _ <- grabStrField ahg (Field "nonce")
          _ <- grabStrField ahg (Field "operation")
          _ <- grabStrField ahg (Field "assetType")
          _ <- grabStrField ahg (Field "assetId")
          _ <- grabStrField ahg (Field "dstAccountType")
          _ <- grabStrField ahg (Field "dstAccount")
          _ <- grabStrField ahg (Field "amount")
          
          shg <- grabHashGroup om (Field "SHG")
          _ <- grabStrField shg (Field "assetType")
          _ <- grabStrField shg (Field "assetId")
          _ <- grabStrField shg (Field "srcAccountType")
          _ <- grabStrField shg (Field "srcAccount")
          _ <- grabStrField shg (Field "dstAccountType")
          _ <- grabStrField shg (Field "dstAccount")
          _ <- grabStrField shg (Field "amount")
          Right ()
-- Order	Value	Type	Comment
-- 1	nonce	[]byte	Explanation provided above.
-- 2	operation	string	"issue"
-- 3	assetType	string	"finp2p"
-- 4	assetId	string	Unique identifier of the asset
-- 5	dstAccountType	string	"finId"
-- 6	dstAccount	string	Investor account finId address that will receive the tokens
-- 7	amount	string	Asset amount
-- Settlement Hash Group (SHG) Structure
-- Represents the payment/settlement instructions.

-- Order	Value	Type	Comment
-- 1	assetType	string	"finp2p", "fiat", "cryptocurrency"
-- 2	assetId	string	Unique identifier of the asset
-- 3	srcAccountType	string	"finId"
-- 4	srcAccount	string	Investor account finId address for the payment asset
-- 5	dstAccountType	string	"finId"
-- 6	dstAccount	string	Issuer account finId address for the payment asset
-- 7	amount	string	Settlement amount

        
      SecondarySale ->
        \om -> do
          ahg <- grabHashGroup om (Field "AHG")
          _ <- grabStrField ahg (Field "nonce")
          _ <- grabStrField ahg (Field "operation")
          _ <- grabStrField ahg (Field "assetType")
          _ <- grabStrField ahg (Field "assetId")
          _ <- grabStrField ahg (Field "srcAccountType")
          _ <- grabStrField ahg (Field "srcAccount")          
          _ <- grabStrField ahg (Field "dstAccountType")
          _ <- grabStrField ahg (Field "dstAccount")
          _ <- grabStrField ahg (Field "amount")
          
          shg <- grabHashGroup om (Field "SHG")
          _ <- grabStrField shg (Field "assetType")
          _ <- grabStrField shg (Field "assetId")
          _ <- grabStrField shg (Field "srcAccountType")
          _ <- grabStrField shg (Field "srcAccount")
          _ <- grabStrField shg (Field "dstAccountType")
          _ <- grabStrField shg (Field "dstAccount")
          _ <- grabStrField shg (Field "amount")
          
          Right ()
          
-- Asset Hash Group (AHG) Structure
-- Represents the asset instructions.

-- Order	Value	Type	Comment
-- 1	nonce	[]byte	Explanation provided abo
-- 2	operation	string	"transfer"
-- 3	assetType	string	"finp2p"
-- 4	assetId	string	Unique identifier of the asset
-- 5	srcAccountType	string	"finId"
-- 6	srcAccount	string	Seller account finId address that will has the tokens
-- 7	dstAccountType	string	"finId"
-- 8	dstAccount	string	Buyer account finId address that will receive the tokens
-- 9	amount	string	Asset amount
-- Settlement Hash Group (SHG) Structure
-- Represents the payment/settlement instructions.

-- Order	Value	Type	Comment
-- 1	assetType	string	"finp2p", "fiat", "cryptocurrency"
-- 2	assetId	string	Unique identifier of the asset
-- 3	srcAccountType	string	"finId"
-- 4	srcAccount	string	Buyer account findId address for the payment asset
-- 5	dstAccountType	string	"finId"
-- 6	dstAccount	string	Seller account findId address for the payment asset
-- 7	amount	string	Settlement amount
      
      Loan ->
        \om -> do
          hg <- grabHashGroup om (Field "HG")
          _ <- grabStrField hg (Field "nonce")
          _ <- grabStrField hg (Field "operation")
          _ <- grabStrField hg (Field "pledgeAssetType")
          _ <- grabStrField hg (Field "pledgeAssetId")
          _ <- grabStrField hg (Field "pledgeBorrowerAccountType")
          _ <- grabStrField hg (Field "pledgeBorrowerAccountId")
          _ <- grabStrField hg (Field "pledgeLenderAccountType")
          _ <- grabStrField hg (Field "pledgeLenderAccountId")
          _ <- grabStrField hg (Field "pledgeAmount")          
          _ <- grabStrField hg (Field "moneyAssetType")
          _ <- grabStrField hg (Field "moneyAssetId")
          _ <- grabStrField hg (Field "moneyLenderAccountType")
          _ <- grabStrField hg (Field "moneyLenderAccountId")
          _ <- grabStrField hg (Field "moneyBorrowerAccountType")
          _ <- grabStrField hg (Field "moneyBorrowerAccountId")
          _ <- grabStrField hg (Field "borrowedMoneyAmount")
          _ <- grabStrField hg (Field "returnedMoneyAmount")
          _ <- grabStrField hg (Field "openTime")
          _ <- grabStrField hg (Field "closeTime")
          Right ()

-- Order	Value	Type	Comment
-- 1	nonce	[]byte	Explanation provided above.
-- 2	operation	string	"loan"
-- 3	pledgeAssetType	string	"finp2p"
-- 4	pledgeAssetId	string	Unique identifier of the asset
-- 5	pledgeBorrowerAccountType	string	"finId"
-- 6	pledgeBorrowerAccountId	string	Borrower account
-- 7	pledgeLenderAccountType	string	"finId"
-- 8	pledgeLenderAccountId	string	Lender account
-- 9	pledgeAmount	string	Pledged asset amount
-- 10	moneyAssetType	string	"fiat"
-- 11	moneyAssetId	string	Unique identified of the asset
-- 12	moneyLenderAccountType	string	"finId"
-- 13	moneyLenderAccountId	string	Lender account
-- 14	moneyBorrowerAccountType	string	"finId"
-- 15	moneyBorrowerAccountId	string	Borrower account
-- 16	borrowedMoneyAmount	string	Lender settlement amount
-- 17	returnedMoneyAmount	string	Returned money at maturity, which includes applicable interest
-- 18	openTime	string	Settlement time for Loan, EPOCH in seconds
-- 19	closeTime	string	Maturity time for Loan, EPOCH in seconds
      
      Redeem -> 
        \om -> do
          ahg <- grabHashGroup om (Field "AHG")
          _ <- grabStrField ahg (Field "nonce")
          _ <- grabStrField ahg (Field "operation")
          _ <- grabStrField ahg (Field "assetType")
          _ <- grabStrField ahg (Field "assetId")
          _ <- grabStrField ahg (Field "srcAccountType")
          _ <- grabStrField ahg (Field "srcAccount")          
          _ <- grabStrField ahg (Field "amount")
          
          shg <- grabHashGroup om (Field "SHG")
          _ <- grabStrField shg (Field "assetType")
          _ <- grabStrField shg (Field "assetId")
          _ <- grabStrField shg (Field "srcAccountType")
          _ <- grabStrField shg (Field "srcAccount")
          _ <- grabStrField shg (Field "dstAccountType")
          _ <- grabStrField shg (Field "dstAccount")
          _ <- grabStrField shg (Field "amount")
          
          Right ()


-- Asset Hash Group (AHG) Structure
-- Represents the asset instructions.

-- Order	Value	Type	Comment
-- 1	nonce	[]byte	Explanation provided above.
-- 2	operation	string	"redeem"
-- 3	assetType	string	"finp2p"
-- 4	assetId	string	Unique identifier of the asset
-- 5	srcAccountType	string	"finId"
-- 6	srcAccount	string	Investor account finId address
-- 7	amount	string	Asset amount
-- Settlement Hash Group (SHG) Structure
-- Represents the payment/settlement instructions.

-- Order	Value	Type	Comment
-- 1	assetType	string	"finp2p", "fiat", "cryptocurrency"
-- 2	assetId	string	Unique identifier of the asset
-- 3	srcAccountType	string	"finId"
-- 4	srcAccount	string	Issuer account findId address for the payment
-- 5	dstAccountType	string	"finId"
-- 6	dstAccount	string	Investor account finId address that will receive the funds
-- 7	amount	string	Settlement amount
      
      Withdraw -> 
        \om -> do
          dhg <- grabHashGroup om (Field "HG")
          _ <- grabStrField dhg (Field "nonce")
          _ <- grabStrField dhg (Field "operation")
          _ <- grabStrField dhg (Field "assetType")
          _ <- grabStrField dhg (Field "assetId")
          _ <- grabStrField dhg (Field "srcAccountType")
          _ <- grabStrField dhg (Field "srcAccount")
          _ <- grabStrField dhg (Field "dstAccountType")
          _ <- grabStrField dhg (Field "dstAccount")
          _ <- grabStrField dhg (Field "amount")
          Right ()

-- Order	Value	Type	Comment
-- 1	nonce	string	Explanation provided above.
-- 2	operation	string	"withdraw"
-- 3	assetType	string	"fiat", "cryptocurrency", "finp2p", "custom"
-- 4	assetId	string	Unique identifier of the asset (blank for "custom")
-- 5	srcAccountType	string	"finId"
-- 6	srcAccount	string	Source account to withdraw funds from
-- 5	dstAccountType	string	"finId"
-- 6	dstAccount	string	Account to deposit the asset to
-- 7	amount	string	string representation of the amount

-- HG = hash('SHA3-256', [fields by order]);
-- hashList = hash('SHA3-256', [HG]);
-- Signature = sign(sender private secp256k1 key, hashList)
