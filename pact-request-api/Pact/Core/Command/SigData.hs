{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Pact.Types.SigData
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--

module Pact.Core.Command.SigData
  ( PublicKeyHex(..)
  , SigData(..)
  , commandToSigData
  , sigDataToCommand
  , sampleSigData
  ) where

-- import Control.Error
import Control.Lens
import Control.Monad (join)
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import Data.String
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Text.Encoding
import GHC.Generics

import Pact.Core.Command.Types

import qualified Pact.JSON.Encode as J

import qualified Pact.JSON.Legacy.HashMap as LHM
import Pact.Core.Hash
import Data.Maybe (catMaybes)
import Pact.Core.Command.Util
import Data.Bifunctor (first)
import Pact.Core.Pretty (renderCompactString)

newtype PublicKeyHex = PublicKeyHex { unPublicKeyHex :: Text }
  deriving (Eq,Ord,Show,Generic)

instance IsString PublicKeyHex where
  fromString = PublicKeyHex . pack
instance FromJSONKey PublicKeyHex

instance FromJSON PublicKeyHex where
  parseJSON = withText "PublicKeyHex" $ \t -> do
    if T.length t == 64 && has _Right (parseB16TextOnly t)
      then return $ PublicKeyHex t
      else fail "Public key must have 64 hex characters"

instance J.Encode PublicKeyHex where
  build (PublicKeyHex hex) = J.text hex

-- | This type is designed to represent signatures in all possible situations
-- where they might be wanted. It must satisfy at least the following
-- requirements:
--
-- 1. It must support multisig.
-- 2. It must have values representing state both before and after all the
-- signatures have been supplied.
-- 3. It must be usable in a cold wallet setting where transfer may be high
-- latency with low bandwidth. This means having the option to leave out the
-- payload to facilitate easier transmission via QR codes and other low
-- bandwidth channels.
data SigData a = SigData
  { _sigDataHash :: !Hash
  , _sigDataSigs :: ![(PublicKeyHex, Maybe UserSig)]
  -- ^ This is not a map because the order must be the same as the signers inside the command.
  , _sigDataCmd :: !(Maybe a)
  } deriving (Eq,Show,Generic)

instance FromJSON a => FromJSON (SigData a) where
  parseJSON = withObject "SigData" $ \o -> do
    h <- o .: "hash"
    s <- (o .: "sigs") >>= f
    c <- o .:? "cmd"
    pure $ SigData h s c
    where
      f v = flip (withObject "SigData Pairs") v $ \_ ->
        fmap (bimap PublicKeyHex (fmap ED25519Sig)) . LHM.sortByKey . HM.toList <$> parseJSON v

instance J.Encode a => J.Encode (SigData a) where
  build o = J.object
    [ "hash" J..= _sigDataHash o
    , "sigs" J..= LHM.fromList (bimap unPublicKeyHex (fmap extractEd25519Sig) <$> _sigDataSigs o)
      -- FIXME: this instance seems to violate the comment on the respective
      -- constructor field. Is that fine? Is it required for backward compat?
      -- This instance also does not roundtrip.
    , "cmd" J..?= _sigDataCmd o
    ]
    where
    extractEd25519Sig (ED25519Sig s) = s
    extractEd25519Sig _ = error "SigData cannot contain non-ED25519 signature"
  {-# INLINE build #-}

commandToSigData :: Command Text -> Either String (SigData Text)
commandToSigData c = do
  let ep = traverse (first renderCompactString <$> parsePact) =<< eitherDecodeStrict' (encodeUtf8 $ _cmdPayload c)
  case ep :: Either String (Payload Value ParsedCode) of
    Left e -> Left $ "Error decoding payload: " <> e
    Right p -> do
      let sigs = map (\s -> (PublicKeyHex $ _siPubKey s, Nothing)) $ _pSigners p
      Right $ SigData (_cmdHash c) sigs (Just $ _cmdPayload c)

sigDataToCommand :: SigData Text -> Either String (Command Text)
sigDataToCommand (SigData _ _ Nothing) = Left "Can't reconstruct command"
sigDataToCommand (SigData h sigList (Just c)) = do
  payload :: Payload Value ParsedCode <- traverse (first renderCompactString <$> parsePact) =<< eitherDecodeStrict' (encodeUtf8 c)
  let sigMap = M.fromList sigList
  -- It is ok to use a map here because we're iterating over the signers list and only using the map for lookup.
  let sigs = catMaybes
        $ map (\signer -> join $ M.lookup (PublicKeyHex $ _siPubKey signer) sigMap)
        $ _pSigners payload
  pure $ Command c sigs h

sampleSigData :: SigData Text
sampleSigData = SigData (maybe (error "boom") id $ parseHash "b57_gSRIwDEo6SAYseppem57tykcEJkmbTFlCHDs0xc")
  [("acbe76b30ccaf57e269a0cd5eeeb7293e7e84c7d68e6244a64c4adf4d2df6ea1", Nothing)] Nothing
