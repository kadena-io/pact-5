{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}


-- |
-- Module      :  Pact.Types.Command
-- Copyright   :  (C) 2016 Stuart Popejoy, Will Martino
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>, Will Martino <will@kadena.io>
--
-- Specifies types for commands in a consensus/DL setting.
--

module Pact.Core.Command.Types
  ( Command(..),cmdPayload,cmdSigs,cmdHash
  , verifyUserSig
  , verifyUserSigs
  , verifyCommand
  , PPKScheme(..)
  , Ed25519KeyPairCaps
  , ProcessedCommand(..),_ProcSucc,_ProcFail
  , Payload(..),pMeta,pNonce,pPayload,pSigners,pVerifiers,pNetworkId
  , ParsedCode(..),pcCode,pcExps
  , Signer(..),siScheme, siPubKey, siAddress, siCapList
  , UserSig(..)
  , PactResult(..)
  , CommandResult(..),crReqKey,crTxId,crResult,crGas,crLogs,crEvents
  , crContinuation,crMetaData
  , CommandExecInterface(..),ceiApplyCmd,ceiApplyPPCmd
  , ApplyCmd, ApplyPPCmd
  , RequestKey(..)
  , cmdToRequestKey
  , requestKeyToB16Text

  , DynKeyPair (DynEd25519KeyPair, DynWebAuthnKeyPair)
  , WebAuthnPubKeyPrefixed(..)
  ) where

import Control.Applicative
import Control.Lens hiding ((.=), elements)
import Control.Monad
import Control.DeepSeq

import Data.Aeson as A
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Short as ShortByteString
import qualified Data.ByteString.Base16 as B16
import Data.Foldable
import Data.Hashable (Hashable)
import Data.Serialize as SZ
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Maybe  (fromMaybe)

import GHC.Generics

import Pact.Core.Capabilities
import Pact.Core.ChainData
import Pact.Core.Compile
import Pact.Core.DefPacts.Types
import Pact.Core.Errors
import Pact.Core.Guards
import Pact.Core.Gas.Types
import Pact.Core.Names
import qualified Pact.Core.Hash as PactHash
import Pact.Core.Persistence.Types
import Pact.Core.Info
import Pact.Core.PactValue (PactValue(..))
import Pact.Core.Command.RPC
import Pact.Core.StableEncoding
import qualified Pact.Core.Syntax.ParseTree as Lisp
import Pact.Core.Verifiers

import qualified Pact.JSON.Decode as JD
import qualified Pact.JSON.Encode as J


import Pact.Core.Command.Crypto  as Base

-- | Command is the signed, hashed envelope of a Pact execution instruction or command.
-- In 'Command ByteString', the 'ByteString' payload is hashed and signed; the ByteString
-- being the JSON serialization of 'Payload Text', where the 'Text' is the pact code; when
-- executed this is parsed to 'ParsedCode'.
-- Thus, 'Command (Payload m ParsedCode)' (with m representing platform-specific metadata)
-- is the fully executable specialization.
data Command a = Command
  { _cmdPayload :: !a
  , _cmdSigs :: ![UserSig]
  , _cmdHash :: !PactHash.Hash
  } deriving (Eq,Show,Ord,Generic,Functor,Foldable,Traversable)

instance (FromJSON a) => FromJSON (Command a) where
    parseJSON = withObject "Command" $ \o ->
                Command <$> (o .: "cmd")
                        <*> (o .: "sigs")
                        <*> (o .: "hash")
    {-# INLINE parseJSON #-}

instance J.Encode a => J.Encode (Command a) where
  build o = J.object
    [ "hash" J..= _cmdHash o
    , "sigs" J..= J.Array (_cmdSigs o)
    , "cmd" J..= _cmdPayload o
    ]
  {-# INLINABLE build #-}

instance NFData a => NFData (Command a)

-- | Strict Either thing for attempting to deserialize a Command.
data ProcessedCommand m a =
  ProcSucc !(Command (Payload m a)) |
  ProcFail !String
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)
instance (NFData a,NFData m) => NFData (ProcessedCommand m a)


type Ed25519KeyPairCaps = (Ed25519KeyPair ,[SigCapability])

-- These two types in legacy pact had the same definition and
-- JSON encoding. Can they be unified?
type SigCapability = CapToken QualifiedName PactValue


-- | Pair parsed Pact expressions with the original text.
data ParsedCode = ParsedCode
  { _pcCode :: !Text
  , _pcExps :: ![Lisp.TopLevel ()]
  } deriving (Show,Generic)
instance NFData ParsedCode

parsePact :: Text -> Either String ParsedCode
parsePact t =
  ParsedCode t <$> first show (fmap stripInfo <$> parseOnlyProgram t)
  where
    stripInfo :: Lisp.TopLevel SpanInfo -> Lisp.TopLevel ()
    stripInfo = void

-- VALIDATING TRANSACTIONS

verifyCommand :: forall m. FromJSON m => Command ByteString -> ProcessedCommand m ParsedCode
verifyCommand orig@Command{..} =
  case parsedPayload of
    Right env' -> case verifiedHash of
      Right _ -> case (hasInvalidSigs _cmdHash _cmdSigs $ _pSigners env') of
        Nothing -> toProcSucc env'
        Just sigErr -> toProcFail sigErr
      Left hshErr -> toProcFail hshErr
    Left payloadErr -> toProcFail payloadErr
  where
    toProcSucc payload = ProcSucc $ orig { _cmdPayload = payload }
    toProcFail errStr = ProcFail $ "Invalid command: " ++ errStr

    parsedPayload :: Either String (Payload m ParsedCode)
    parsedPayload = traverse parsePact =<< A.eitherDecodeStrict' @(Payload m Text) _cmdPayload

    verifiedHash = PactHash.verifyHash _cmdHash _cmdPayload

hasInvalidSigs :: PactHash.Hash -> [UserSig] -> [Signer QualifiedName PactValue] -> Maybe String
hasInvalidSigs hsh sigs signers
  | not (length sigs == length signers)  = Just "Number of sig(s) does not match number of signer(s)"
  | otherwise                            = verifyUserSigs hsh (zip sigs signers)

verifyUserSigs :: PactHash.Hash -> [(UserSig, Signer QualifiedName PactValue)] -> Maybe String
verifyUserSigs hsh sigsAndSigners
  | null failedSigs = Nothing
  | otherwise = formatIssues
  where
  getFailedVerify (sig, signer) =
    [ (Text.pack $ show signer, Text.pack err) | Left err <- [verifyUserSig hsh sig signer] ]
  -- assumes nth Signer is responsible for the nth UserSig
  failedSigs = concatMap getFailedVerify sigsAndSigners
  formatIssues = Just $ "Invalid sig(s) found: " ++ show (J.encode . J.Object $ failedSigs)

verifyUserSig :: PactHash.Hash -> UserSig -> Signer QualifiedName PactValue -> Either String ()
verifyUserSig msg sig Signer{..} = do
  case (sig, scheme) of
    (ED25519Sig edSig, ED25519) -> do
      for_ _siAddress $ \addr -> do
        unless (_siPubKey == addr) $ Left "address does not match pubkey"
      pk <- over _Left ("failed to parse ed25519 pubkey: " <>) $
        parseEd25519PubKey =<< B16.decode (Text.encodeUtf8 _siPubKey)
      edSigParsed <- over _Left ("failed to parse ed25519 signature: " <>) $
        parseEd25519Signature =<< B16.decode (Text.encodeUtf8 edSig)
      verifyEd25519Sig msg pk edSigParsed

    (WebAuthnSig waSig, WebAuthn) -> do
      let
        strippedPrefix =
          fromMaybe _siPubKey (Text.stripPrefix webAuthnPrefix _siPubKey)
      -- we can't use parseWebAuthnPublicKeyText here because keys in the
      -- signers list might be unprefixed due to old webauthn.
      pk <- over _Left ("failed to parse webauthn pubkey: " <>) $
        parseWebAuthnPublicKey =<< B16.decode (Text.encodeUtf8 strippedPrefix)
      verifyWebAuthnSig msg pk waSig

    _ ->
      Left $ unwords
        [ "scheme of"
        , show _siScheme
        , "does not match signature type"
        , case sig of
          ED25519Sig _ -> "ED25519"
          WebAuthnSig _ -> "WebAuthn"
        ]
  where scheme = fromMaybe defPPKScheme _siScheme


instance J.Encode (Signer QualifiedName PactValue) where
  build o = J.object
    [ "addr" J..?= _siAddress o
    , "scheme" J..?= _siScheme o
    , "pubKey" J..= _siPubKey o
    , "clist" J..??= J.Array (StableEncoding  <$> _siCapList o)
    ]

instance FromJSON (Signer QualifiedName PactValue) where
  parseJSON = withObject "Signer" $ \o -> do
    scheme <- o .:? "scheme"
    pubKey <- o .: "pubKey"
    addr <- o .:? "addr"
    clist <- listMay <$> o .:? "clist"
    pure $ Signer scheme pubKey addr (_stableEncoding <$> clist)
    where
      listMay = fromMaybe []

-- | Payload combines a 'PactRPC' with a nonce and platform-specific metadata.
data Payload m c = Payload
  { _pPayload :: !(PactRPC c)
  , _pNonce :: !Text
  , _pMeta :: !m
  , _pSigners :: ![Signer QualifiedName PactValue]
  , _pVerifiers :: !(Maybe [Verifier ParsedVerifierProof])
  , _pNetworkId :: !(Maybe NetworkId)
  } deriving (Show, Eq, Generic, Functor, Foldable, Traversable)
instance (NFData a,NFData m) => NFData (Payload m a)

instance (J.Encode a, J.Encode m) => J.Encode (Payload m a) where
  build o = J.object
    [ "networkId" J..= fmap _networkId (_pNetworkId o)
    , "payload" J..= _pPayload o
    , "signers" J..= J.Array (_pSigners o)
    , "verifiers" J..?= fmap J.Array (_pVerifiers o)
    , "meta" J..= _pMeta o
    , "nonce" J..= _pNonce o
    ]
  {-# INLINE build #-}

instance (FromJSON a,FromJSON m) => FromJSON (Payload m a) where
  parseJSON = JD.withObject "Payload" $ \o -> do
    payload <- o .: "payload"
    nonce' <- o .: "nonce"
    meta <- o .: "meta"
    signers <- o .: "signers"
    verifiers <- o .:? "verifiers"
    networkId <- o .:? "networkId"
    pure $ Payload payload nonce' meta signers verifiers (fmap NetworkId networkId)

newtype PactResult = PactResult
  { _pactResult :: Either PactErrorI PactValue
  } deriving (Eq, Show, Generic, NFData)

instance J.Encode PactResult where
  build (PactResult (Right s)) = J.object
    [ "status" J..= J.text "success"
    , "data" J..= StableEncoding s
    ]
  build (PactResult (Left f)) = J.object
    [ "status" J..= J.text "failure"
    , "error" J..= StableEncoding f
    ]
  {-# INLINE build #-}

instance FromJSON PactResult where
  parseJSON (A.Object o) =
    PactResult <$> ((Left . _stableEncoding . _getUxPactError <$> o JD..: "error") <|> (Right . _stableEncoding <$> o .: "data"))
  parseJSON p = fail $ "Invalid PactResult " ++ show p

newtype UxPactError = UxPactError { _getUxPactError :: StableEncoding PactErrorI }
  deriving (Eq)
  deriving newtype (J.Encode, FromJSON)

-- -- TODO: Do we ever need to parse UxPactError's?
-- instance FromJSON UxPactError where
--   parseJSON = withObject "PactError" $ \o -> do
--     typ <- o .: "type"
--     doc <- o .: "message"
--     inf <- o .: "info"
--     sf <- parseSFs <$> o .: "callStack"
--     pure . UxPactError $ PactError typ (_stableEncoding inf) sf (prettyString doc)
--     where
--       parseSFs :: [Text] -> [StackFrame SpanInfo]
--       parseSFs sfs = case mapM (parseOnly parseUxStackFrame) sfs of
--         Left _e -> []
--         Right ss -> _getUxStackFrame <$> ss

-- | API result of attempting to execute a pact command, parametrized over level of logging type
data CommandResult l = CommandResult {
  -- | Request Key of command (the hash of the command payload)
    _crReqKey :: !RequestKey
  -- | Transaction id of this CommandResult
  , _crTxId :: !(Maybe TxId)
  -- | Pact execution result, either a PactError or the last pact expression output as a PactValue
  , _crResult :: !PactResult
  -- | Gas consummed by command
  , _crGas :: !Gas
  -- | Level of logging (i.e. full TxLog vs hashed logs)
  , _crLogs :: !(Maybe l)
  -- | Output of a Continuation if one occurred in the command.
  , _crContinuation :: !(Maybe DefPactExec)
  -- | Platform-specific data
  , _crMetaData :: !(Maybe Value)
  -- | Events
  , _crEvents :: ![PactEvent PactValue]
  } deriving (Eq,Show,Generic,Functor)

instance J.Encode l => J.Encode (CommandResult l) where
  build o = J.object
    [ "gas" J..= A.Number (fromIntegral (_gas (_crGas o)) )
    , "result" J..= _crResult o
    , "reqKey" J..= _crReqKey o
    , "logs" J..= _crLogs o
    , "events" J..??= J.Array (StableEncoding <$> _crEvents o)
    , "metaData" J..= _crMetaData o
    , "continuation" J..=  fmap StableEncoding (_crContinuation o)
    , "txId" J..= fmap (A.Number . fromIntegral . _txId) (_crTxId o)
    ]
  {-# INLINE build #-}

instance (FromJSON l) => FromJSON (CommandResult l) where
  parseJSON = withObject "CommandResult" $ \o -> CommandResult
      <$> o .: "reqKey"
      <*> (fmap TxId <$> o .: "txId")
      <*> o .: "result"
      <*> (Gas <$> o .: "gas")
      <*> o .: "logs"
      <*> (fmap _stableEncoding <$> o .: "continuation")
      <*> o .: "metaData"
      <*> (events . (fmap . fmap) _stableEncoding <$> o .:? "events")
    where
      events Nothing = []
      events (Just es) = es
instance NFData a => NFData (CommandResult a)

cmdToRequestKey :: Command a -> RequestKey
cmdToRequestKey Command {..} = RequestKey _cmdHash

type ApplyCmd l = ExecutionMode -> Command ByteString -> IO (CommandResult l)
type ApplyPPCmd m a l = ExecutionMode -> Command ByteString -> ProcessedCommand m a -> IO (CommandResult l)

data CommandExecInterface m a l = CommandExecInterface
  { _ceiApplyCmd :: ApplyCmd l
  , _ceiApplyPPCmd :: ApplyPPCmd m a l
  }


data WebAuthnPubKeyPrefixed
  = WebAuthnPubKeyPrefixed
  | WebAuthnPubKeyBare
  deriving (Eq, Show, Generic)
data DynKeyPair
  = DynEd25519KeyPair Ed25519KeyPair
  | DynWebAuthnKeyPair WebAuthnPubKeyPrefixed WebAuthnPublicKey WebauthnPrivateKey
  deriving (Eq, Show, Generic)

requestKeyToB16Text :: RequestKey -> Text
requestKeyToB16Text (RequestKey (PactHash.Hash h)) =
  T.decodeUtf8 $ B16.encode (ShortByteString.fromShort h)

newtype RequestKey = RequestKey { unRequestKey :: PactHash.Hash}
  deriving (Eq, Ord, Generic)
  deriving newtype (Serialize, Hashable, FromJSON, FromJSONKey, NFData, J.Encode
    )

instance Show RequestKey where
  show (RequestKey rk) = show rk

makeLenses ''UserSig
makeLenses ''Signer
makeLenses ''CommandExecInterface
makeLenses ''ExecutionMode
makeLenses ''Command
makeLenses ''ParsedCode
makeLenses ''Payload
makeLenses ''CommandResult
makePrisms ''ProcessedCommand
makePrisms ''ExecutionMode
