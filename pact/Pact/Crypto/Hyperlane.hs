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
module Pact.Crypto.Hyperlane
  ( HyperlaneMessage(..)
  , TokenMessageERC20(..)
  , decodeHyperlaneMessageObject
  , packHyperlaneMessage
  , packTokenMessageERC20
  , unpackTokenMessageERC20
  , tokenMessageToTerm
  , decodeHyperlaneTokenMessageObject
  , getHyperlaneMessageId
  , eof
  ) where

import Control.Lens ((^?), at, _Just, Prism')
import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Data.Bifunctor (first)
import Data.Binary.Get (Get)
import Data.Binary.Get qualified as Bin
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Short qualified as BSS
import Data.Decimal (Decimal)
import Data.Map (Map)
import Data.Ratio ((%))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Read qualified as Text
import Data.WideWord.Word256 (Word256(..))
import Data.Word (Word8, Word16, Word32)
import Ethereum.Misc (keccak256, _getKeccak256Hash, _getBytesN)
import Pact.JSON.Decode qualified as J
import Pact.Core.StableEncoding 

import Pact.Core.Errors
import Pact.Core.PactValue
import Pact.Core.Names
import Pact.Core.Literal
import Pact.Core.Hash
import qualified Data.Map as M

----------------------------------------------
--         Hyperlane Message Types          --
----------------------------------------------

data HyperlaneMessage = HyperlaneMessage
  { hmVersion :: Word8 -- uint8
  , hmNonce :: Word32 -- uint32
  , hmOriginDomain :: Word32 -- uint32
  , hmSender :: ByteString -- 32x uint8
  , hmDestinationDomain :: Word32 -- uint32
  , hmRecipient :: ByteString -- 32x uint8
  , hmMessageBody :: ByteString -- variable
  }
  deriving stock (Eq, Show)

data TokenMessageERC20 = TokenMessageERC20
  { tmAmount :: Word256 -- uint256
  , tmChainId :: Word16 -- uint16
  , tmRecipient :: ByteString -- variable
  }
  deriving stock (Eq, Show)

----------------------------------------------
--    Hyperlane Message Binary Encoding     --
----------------------------------------------

packHyperlaneMessage :: HyperlaneMessage -> Builder
packHyperlaneMessage (HyperlaneMessage{..}) =
  BB.word8 hmVersion
  <> BB.word32BE hmNonce
  <> BB.word32BE hmOriginDomain
  <> BB.byteString hmSender
  <> BB.word32BE hmDestinationDomain
  <> BB.byteString hmRecipient
  <> BB.byteString hmMessageBody

-- The TokenMessage contains a recipient (text) and an amount (word-256).
-- A schematic of the message format:
-- 0000000000000000000000000000000000000000000000008ac7230489e80000 # amount = 10000000000000000000
-- 0000 # chainId = 0
-- 7B2270726564223A20226B6579732D616C6C222C20226B657973223A205B2264 # {"pred": "keys-all", "keys": ["da1a339bd82d2c2e9180626a00dc043275deb3ababb27b5738abf6b9dcee8db6"]}
-- 6131613333396264383264326332653931383036323661303064633034333237
-- 3564656233616261626232376235373338616266366239646365653864623622
-- 5D7D
--
-- Note that we use abi.encodePacked here and the data is being concatenated without structure.
-- For more details see https://docs.soliditylang.org/en/latest/abi-spec.html#non-standard-packed-mode
packTokenMessageERC20 :: TokenMessageERC20 -> Builder
packTokenMessageERC20 t =
  word256BE (tmAmount t)
  <> BB.word16BE (tmChainId t)
  <> BB.byteString (tmRecipient t)

unpackTokenMessageERC20 :: Get TokenMessageERC20
unpackTokenMessageERC20 = do
  tmAmount <- getWord256BE
  tmChainId <- Bin.getWord16be
  tmRecipient <- BL.toStrict <$> Bin.getRemainingLazyByteString

  pure $ TokenMessageERC20 {..}

----------------------------------------------
--        Hyperlane Message Hashing         --
----------------------------------------------

getHyperlaneMessageId :: HyperlaneMessage -> Text
getHyperlaneMessageId =
  Text.decodeUtf8
  . encodeBase64UrlUnpadded
  . keccak256Hash
  . BL.toStrict
  . BB.toLazyByteString
  . packHyperlaneMessage

keccak256Hash :: ByteString -> ByteString
keccak256Hash = BSS.fromShort . _getBytesN . _getKeccak256Hash . keccak256

decodeBase64 :: Field -> Text -> Either HyperlaneError ByteString
decodeBase64 key s =
  first (const (HyperlaneErrorInvalidBase64 key)) $ decodeBase64UrlUnpadded $ Text.encodeUtf8 s

decodeBase64AndValidate :: Field -> Int -> Text -> Either HyperlaneError ByteString
decodeBase64AndValidate key expected s = do
  decoded <- decodeBase64 key s

  unless (BS.length decoded == expected) $
    throwError $ HyperlaneErrorIncorrectSize key expected (BS.length decoded)

  return decoded

parseChainId :: forall a. (a ~ Word16) => Text -> Either HyperlaneError a
parseChainId s = do
  (cid, _) <- first (HyperlaneErrorInvalidChainId . Text.pack) $ Text.decimal s
  unless (cid >= minBound && cid <= maxBound) $ do
    throwError $ HyperlaneErrorInvalidChainId $ Text.pack $
      "ChainId must be in [" <> show @a minBound <> ", " <> show @a maxBound <> "]"
  pure cid

------------------------------------------------------
--      Hyperlane Message Pact Object Decoding      --
------------------------------------------------------

decodeHyperlaneMessageObject :: Map Field PactValue -> Either HyperlaneError HyperlaneMessage
decodeHyperlaneMessageObject om = do
  hmVersion           <- grabInt @Word8  om (Field "version")
  hmNonce             <- grabInt @Word32 om (Field "nonce")
  hmOriginDomain      <- grabInt @Word32 om (Field "originDomain")
  hmSender            <- decodeBase64AndValidate (Field "sender") 32 =<< grabField om (Field "sender") _LString
  hmDestinationDomain <- grabInt @Word32 om (Field "destinationDomain")
  hmRecipient         <- decodeBase64AndValidate (Field "recipient") 32 =<< grabField om (Field "recipient") _LString
  hmMessageBody       <- decodeBase64 (Field "messageBody") =<< grabField om (Field "messageBody") _LString

  pure HyperlaneMessage{..}

------------------------------------------------------------
--      Hyperlane Token Message Pact Object Decoding      --
------------------------------------------------------------

-- | Decodes Pact's object that represents a token message.
--
-- Important: the token message object here represents the message
-- we are sending to the hyperlane, which is different from the one
-- we recieve.
--
-- We are using the same ADT 'TokenMessageERC20', but the content
-- a bit different. Note 'decimalToWord' is not using 'ethInWei'.
-- This is because the necessary alignment is done on the chain —
-- since we use a different set of tokens that with different
-- precisions (e.g. USDC with precision 6).
decodeHyperlaneTokenMessageObject :: Map Field PactValue -> Either HyperlaneError TokenMessageERC20
decodeHyperlaneTokenMessageObject om = do
  tmRecipient <- decodeBase64 (Field "recipient") =<< grabField om (Field "recipient") _LString
  tmAmount    <- decimalToWord <$> grabField om (Field "amount") _LDecimal
  tmChainId   <- parseChainId =<< grabField om (Field "chainId") _LString

  pure TokenMessageERC20{..}

----------------------------------------------
--                Utilities                 --
----------------------------------------------

wordToDecimal :: Word256 -> Decimal
wordToDecimal w = fromRational (toInteger w % ethInWei)

decimalToWord :: Decimal -> Word256
decimalToWord = round -- we don't multiply by ethInWei here as the data on chain is already correct

ethInWei :: Num a => a
ethInWei = 1_000_000_000_000_000_000 -- 1e18

grabField :: Map Field PactValue -> Field -> Prism' Literal a -> Either HyperlaneError a
grabField m key p = case m ^? at key . _Just . _PLiteral . p of
  Nothing -> Left (HyperlaneErrorFailedToFindKey key)
  Just a -> Right a

-- | Grab a bounded integral value out of the pact object, and make sure
--   the integer received is a valid element of that type
grabInt :: forall a. (Integral a, Bounded a) => Map Field PactValue -> Field -> Either HyperlaneError a
grabInt m key = do
  i <- grabField m key _LInteger
  if i >= fromIntegral @a @Integer minBound && i <= fromIntegral @a @Integer maxBound
  then do
    pure (fromIntegral @Integer @a i)
  else do
    throwError (HyperlaneErrorNumberOutOfBounds key)

eof :: Get ()
eof = do
  done <- Bin.isEmpty
  unless done $ fail "pending bytes in input"

word256BE :: Word256 -> Builder
word256BE (Word256 a b c d) =
  BB.word64BE a <> BB.word64BE b <> BB.word64BE c <> BB.word64BE d

getWord256BE :: Get Word256
getWord256BE = do
  Word256 <$> Bin.getWord64be <*> Bin.getWord64be <*> Bin.getWord64be <*> Bin.getWord64be

tokenMessageToTerm :: TokenMessageERC20 -> Either HyperlaneDecodeError PactValue
tokenMessageToTerm tm =  do
  g <- first (const HyperlaneDecodeErrorParseRecipient)
         $ fmap (PGuard . _stableEncoding)
         $ J.eitherDecode (BL.fromStrict (tmRecipient tm))
  let chainId = Text.pack (show (toInteger (tmChainId tm)))
  pure $ PObject $ M.fromList
    [ (Field "recipient", g)
    , (Field "amount", PDecimal (wordToDecimal (tmAmount tm)))
    , (Field "chainId", PString chainId)
    ]
