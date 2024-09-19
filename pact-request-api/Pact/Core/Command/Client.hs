{-# LANGUAGE RecordWildCards #-}

module Pact.Core.Command.Client (

  -- * Command construction
  mkCommand,
  mkCommand',
  mkUnsignedCommand,

  -- * Command construction with dynamic keys (Ed25519 and WebAuthn)
  mkCommandWithDynKeys,
  mkCommandWithDynKeys',
  ApiKeyPair(..),
  ApiSigner(..),
  ApiPublicMeta(..),
  ApiReq(..),
  AddSigsReq(..),
  mkKeyPairs,
  apiReq,
  uapiReq,
  uapiReq',
  mkApiReq,
  mkApiReqCmd,
  ApiReqParts,
  mkExec,
  mkCont,
  addSigsReq,
  combineSigs,
  combineSigDatas,
  signCmd,
  decodeYaml,
  returnCommandIfDone,
  loadSigData,
  SubmitBatch(..),
) where

import Control.Lens
import Control.Applicative((<|>))
import Control.Monad.Except
import Control.Exception.Safe
import Control.Monad
import Data.Default(def)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Foldable(traverse_, foldrM)
import qualified Data.Aeson.Types as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as AKM
import Data.List (intercalate)
import Data.Maybe (fromMaybe, mapMaybe, listToMaybe)
import Data.Bifunctor (first)
import Data.Either (partitionEithers)
import System.IO
import System.Exit hiding (die)
import Data.List.NonEmpty (NonEmpty(..))

import Pact.Time
import Pact.JSON.Yaml
import System.FilePath
import qualified Pact.JSON.Encode as J
import qualified Pact.JSON.Decode as JD
import qualified Data.Yaml as Y
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Short as SBS
import GHC.Generics

import Pact.Core.ChainData
import Pact.Core.Command.RPC
import Pact.Core.Command.Types
import Pact.Core.Command.Util
import Pact.Core.Command.Crypto
import Pact.Core.Guards
import Pact.Core.PactValue
import Pact.Core.Names
import Pact.Core.Verifiers
import Pact.Core.StableEncoding
import Pact.Core.Gas
import Pact.Core.Hash
import Pact.Core.SPV
import Pact.Core.Signer
import qualified Pact.Core.Hash as PactHash
import Pact.Core.Command.SigData
import Pact.Core.Pretty (renderCompactString)



-- -------------------------------------------------------------------------- --
-- ApiKeyPair

-- | For fully-signed commands
data ApiKeyPair = ApiKeyPair {
  _akpSecret :: PrivateKeyBS,
  _akpPublic :: Maybe PublicKeyBS,
  _akpAddress :: Maybe Text,
  _akpScheme :: Maybe PPKScheme,
  _akpCaps :: Maybe [SigCapability]
  } deriving (Eq, Show, Generic)

instance JD.FromJSON ApiKeyPair where
  parseJSON = JD.withObject "ApiKeyPair" $ \o -> do
    secret <- o JD..: "secret"
    pub <- o JD..:? "public"
    addr <- o JD..:? "address"
    scheme <- o JD..:? "scheme"
    caps <- o JD..:? "caps"
    pure $ ApiKeyPair
      {_akpSecret = secret
      , _akpPublic = pub
      , _akpAddress = addr
      , _akpScheme = scheme
      , _akpCaps = caps}


instance J.Encode ApiKeyPair where
  build o = J.object
    [ "address" J..= _akpAddress o
    , "secret" J..= _akpSecret o
    , "scheme" J..= _akpScheme o
    , "caps" J..= fmap J.Array (_akpCaps o)
    , "public" J..= _akpPublic o
    ]
  {-# INLINE build #-}

-- -------------------------------------------------------------------------- --
-- ApiSigner

-- | For unsigned commands
data ApiSigner = ApiSigner {
  _asPublic :: Text,
  _asAddress :: Maybe Text,
  _asScheme :: Maybe PPKScheme,
  _asCaps :: Maybe [SigCapability]
  } deriving (Eq, Show, Generic)

instance JD.FromJSON ApiSigner where
  parseJSON = JD.withObject "ApiSigner" $ \o -> do
    pub <- o JD..: "public"
    addr <- o JD..:? "address"
    scheme <- o JD..:? "scheme"
    caps <- o JD..:? "caps"
    pure $ ApiSigner
      { _asPublic = pub
      , _asAddress = addr
      , _asScheme = scheme
      , _asCaps = caps}


instance J.Encode ApiSigner where
  build o = J.object
    [ "address" J..= _asAddress o
    , "scheme" J..= _asScheme o
    , "caps" J..= fmap J.Array (_asCaps o)
    , "public" J..= _asPublic o
    ]
  {-# INLINE build #-}

-- -------------------------------------------------------------------------- --
-- ApiPublicMeta

data ApiPublicMeta = ApiPublicMeta
  { _apmChainId :: Maybe ChainId
  , _apmSender :: Maybe Text
  , _apmGasLimit :: Maybe GasLimit
  , _apmGasPrice :: Maybe GasPrice
  , _apmTTL :: Maybe TTLSeconds
  , _apmCreationTime :: Maybe TxCreationTime
  } deriving (Eq, Show, Generic)

(.:@?) :: JD.FromJSON (StableEncoding b) => JD.Object -> JD.Key -> A.Parser (Maybe b)
o .:@? s = fmap _stableEncoding <$> o JD..:? s

-- (.:@) :: JD.FromJSON (StableEncoding b) => JD.Object -> JD.Key -> Parser b
-- o .:@ s = fmap _stableEncoding <$> o JD..: s

instance JD.FromJSON ApiPublicMeta where
  parseJSON = JD.withObject "ApiPublicMeta" $ \o -> ApiPublicMeta
    <$> o .:@? "chainId"
    <*> o JD..:? "sender"
    <*> o .:@? "gasLimit"
    <*> o .:@? "gasPrice"
    <*> o .:@? "ttl"
    <*> o .:@? "creationTime"

instance J.Encode ApiPublicMeta where
  build o = J.object
    [ "creationTime" J..?= (StableEncoding <$> _apmCreationTime o)
    , "ttl" J..?= (StableEncoding <$> _apmTTL o)
    , "gasLimit" J..?= (StableEncoding <$> _apmGasLimit o)
    , "chainId" J..?= (StableEncoding <$> _apmChainId o)
    , "gasPrice" J..?= (StableEncoding <$> _apmGasPrice o)
    , "sender" J..?= _apmSender o
    ]
  -- Todo: revisit all inlinable pragmas
  {-# INLINABLE build #-}

-- -------------------------------------------------------------------------- --
-- ApiReq

data ApiReq = ApiReq {
  _ylType :: Maybe Text,
  _ylPactTxHash :: Maybe Hash,
  _ylStep :: Maybe Int,
  _ylRollback :: Maybe Bool,
  _ylData :: Maybe PactValue,
  _ylProof :: Maybe ContProof,
  _ylDataFile :: Maybe FilePath,
  _ylCode :: Maybe Text,
  _ylCodeFile :: Maybe FilePath,
  _ylKeyPairs :: Maybe [ApiKeyPair],
  _ylSigners :: Maybe [ApiSigner],
  _ylVerifiers :: Maybe [Verifier ParsedVerifierProof],
  _ylNonce :: Maybe Text,
  _ylPublicMeta :: Maybe ApiPublicMeta,
  _ylNetworkId :: Maybe NetworkId
  } deriving (Eq,Show,Generic)

instance JD.FromJSON ApiReq where
  parseJSON = JD.withObject "ApiReq" $ \o -> do
    publicMeta <- o JD..:? "publicMeta"
    proof <- o JD..:? "proof"
    data_ <- (fmap . fmap) _stableEncoding (o JD..:? "data")
    networkId <- o JD..:? "networkId"
    rollback <- o JD..:? "rollback"
    signers <- o JD..:? "signers"
    verifiers <- o JD..:? "verifiers"
    step <- o JD..:? "step"
    code <- o JD..:? "code"
    pactTxHash <- o JD..:? "pactTxHash"
    type_ <- o JD..:? "type"
    codeFile <- o JD..:? "codeFile"
    keyPairs <- o JD..:? "keyPairs"
    dataFile <- o JD..:? "dataFile"
    nonce <- o JD..:? "nonce"
    pure $ ApiReq
      { _ylType=type_
      , _ylPactTxHash=pactTxHash
      , _ylStep=step
      , _ylRollback=rollback
      , _ylData=data_
      , _ylProof=proof
      , _ylDataFile=dataFile
      , _ylCode=code
      , _ylCodeFile=codeFile
      , _ylKeyPairs=keyPairs
      , _ylSigners=signers
      , _ylVerifiers=verifiers
      , _ylNonce=nonce
      , _ylPublicMeta=publicMeta
      , _ylNetworkId=networkId}


instance J.Encode ApiReq where
  build o = J.object
    [ "publicMeta" J..= _ylPublicMeta o
    , "proof" J..= _ylProof o
    , "data" J..= StableEncoding (_ylData o)
    , "networkId" J..= _ylNetworkId o
    , "rollback" J..= _ylRollback o
    , "signers" J..= fmap J.Array (_ylSigners o)
    , "verifiers" J..= fmap J.Array (_ylVerifiers o)
    , "step" J..= fmap J.Aeson (_ylStep o)
    , "code" J..= _ylCode o
    , "pactTxHash" J..= _ylPactTxHash o
    , "type" J..= _ylType o
    , "codeFile" J..= fmap (J.text . T.pack) (_ylCodeFile o)
    , "keyPairs" J..= fmap J.Array (_ylKeyPairs o)
    , "dataFile" J..= fmap (J.text . T.pack) (_ylDataFile o)
    , "nonce" J..= _ylNonce o
    ]
  {-# INLINABLE build #-}

-- -------------------------------------------------------------------------- --
-- AddSigReq

data AddSigsReq = AddSigsReq
  { _asrUnsigned :: Command Text
  , _asrSigs :: [UserSig]
  } deriving (Eq,Show,Generic)

instance JD.FromJSON AddSigsReq where
  parseJSON = JD.withObject "AddSigsReq" $ \o -> AddSigsReq
    <$> o JD..: "unsigned"
    <*> o JD..: "sigs"

instance J.Encode AddSigsReq where
  build o = J.object
    [ "sigs" J..= J.Array (_asrSigs o)
    , "unsigned" J..= _asrUnsigned o
    ]
  {-# INLINABLE build #-}

-- | Submit new commands for execution
newtype SubmitBatch = SubmitBatch { _sbCmds :: NonEmpty (Command Text) }
  deriving (Eq,Generic,Show)


instance JD.FromJSON SubmitBatch where
   parseJSON = JD.withObject "SubmitBatch" $ \o ->
    SubmitBatch <$> o JD..: "cmds"

instance J.Encode SubmitBatch where
  build o = J.object [ "cmds" J..= J.Array (_sbCmds o) ]
  {-# INLINABLE build #-}


-- -------------------------------------------------------------------------- --
-- Functions

combineSigs :: [FilePath] -> Bool -> IO ByteString
combineSigs fs outputLocal = do
  sigs <- mapM loadSigData fs
  case partitionEithers sigs of
    ([], rs) -> combineSigDatas rs outputLocal
    (ls, _) -> do
      error $ unlines $ "One or more files had errors:" : ls

combineSigDatas :: [SigData Text] -> Bool -> IO ByteString
combineSigDatas [] _ = error "Nothing to combine"
combineSigDatas sds outputLocal = do
  (hsh, cmd) <- do
    let hashAndCmd :: Maybe (Hash, Text)
        hashAndCmd = (,)
          <$> listToMaybe (S.toList $ S.fromList $ map _sigDataHash sds)
          <*> listToMaybe (S.toList $ S.fromList $ mapMaybe _sigDataCmd sds)
    case hashAndCmd of
      Nothing -> do
        error "SigData files must contain exactly one unique hash and command.  Aborting..."
      Just x -> do
        pure x
  let sigs = foldl1 f $ map _sigDataSigs sds
  returnCommandIfDone outputLocal $ SigData hsh sigs (Just cmd)
  where
    f accum sigs
      | length accum /= length sigs = error "Sig lists have different lengths"
      | otherwise = zipWith g accum sigs
    g (pAccum,sAccum) (p,s) =
      if pAccum /= p
        then error $ unlines [ "Sig mismatch:"
                             , show pAccum
                             , show p
                             , "All signatures must be in the same order"
                             ]
        else (pAccum, sAccum <|> s)

loadSigData :: FilePath -> IO (Either String (SigData Text))
loadSigData fp = do
  res <- Y.decodeFileEither fp
  return $ case res of
    Left e -> Left $ "Error loading SigData file " <> fp <> ": " <> show e
    Right sd -> Right sd

addSigToSigData :: Ed25519KeyPair -> SigData a -> IO (SigData a)
addSigToSigData kp sd = do
  let sig = ED25519Sig $ signHash (_sigDataHash sd) kp
  let k = PublicKeyHex $ toB16Text $ getPublic kp
  return $ sd { _sigDataSigs = addSigToList k sig $ _sigDataSigs sd }

addSigToList
  :: PublicKeyHex
  -> UserSig
  -> [(PublicKeyHex, Maybe UserSig)]
  -> [(PublicKeyHex, Maybe UserSig)]
addSigToList _ _ [] = []
addSigToList k s ((pk,pus):ps) =
  if k == pk
    then (pk, Just s) : addSigToList k s ps
    else (pk, pus) : addSigToList k s ps

addSigsReq :: [FilePath] -> Bool -> ByteString -> IO ByteString
addSigsReq keyFiles outputLocal bs = do
  sd <- either (error . show) return $ Y.decodeEither' bs
  returnSigDataOrCommand outputLocal =<< foldM addSigReq sd keyFiles

-- | `returnSigDataOrCommand` will either:
--   - validate partial sig(s) added for a particular SigData, then re-serialize as SigData
--   - validate all signatures if all signatures are provided, and output the resulting `Command`
--   TODO: `IO ByteString` is fairly opaque, low hanging fruit to provide (Either Command SigData)
--   and serialize upstream most likely, but is not of much concern atm.
returnSigDataOrCommand :: Bool -> SigData Text -> IO ByteString
returnSigDataOrCommand  outputLocal sd
  | isPartialSigData = do
    case verifyPartialSigData sd of
      Right _ ->
        pure $ encodeYamlWith yamlOptions sd
      Left e -> do
        let msg = unlines ["Command verification failed!", e]
        hPutStrLn stderr msg >> hFlush stderr >> exitFailure
  | otherwise = returnCommandIfDone outputLocal sd
  where
  isPartialSigData = any (isn't _Just . snd) (_sigDataSigs sd)
  verifyPartialSigData (SigData h sigs (Just cmd)) = do
    payload :: Payload A.Value ParsedCode <- traverse (first renderCompactString <$> parsePact) =<< JD.eitherDecodeStrict' (T.encodeUtf8 cmd)
    let sigMap = M.fromList sigs
    when (length (_pSigners payload) /= length sigs) $
      Left "Number of signers in the payload does not match number of signers in the sigData"
    usrSigs <- traverse (toSignerPair sigMap) (_pSigners payload)
    traverse_ Left $ verifyUserSigs h [ (signer, sig) | (sig, Just signer) <- usrSigs ]
    _ <- verifyHash h (T.encodeUtf8 cmd)
    pure ()
    where
    toSignerPair sigMap signer =
      case M.lookup (PublicKeyHex $ _siPubKey signer) sigMap of
        Nothing -> Left $ "Signer in payload does not show up in signatures" <> show (_siPubKey signer)
        Just v -> pure (signer, v)
  verifyPartialSigData (SigData h sigs Nothing) = do
    sigs' <- foldrM toVerifPair [] sigs
    traverse_ Left $ verifyUserSigs h sigs'
    where
    toVerifPair (PublicKeyHex pktext, Just (ED25519Sig _usSig) ) m = do
      let sig = ED25519Sig _usSig
      let signer = Signer (Just ED25519) pktext Nothing []
      pure $ (sig, signer):m
    toVerifPair (_, _) m = pure m

returnCommandIfDone :: Bool -> SigData Text -> IO ByteString
returnCommandIfDone outputLocal sd =
  case sigDataToCommand sd of
    Left _ -> do
      return $ encodeYamlWith yamlOptions sd
    Right c -> do
      let res = verifyCommand $ fmap T.encodeUtf8 c
          out = if outputLocal then J.encode c else J.encode (SubmitBatch (c :| []))
      case res :: ProcessedCommand A.Value ParsedCode of
        ProcSucc _ -> pure $ BSL.toStrict out
        ProcFail e -> do
          let msg = unlines ["Command verification failed!", e]
          hPutStrLn stderr msg >> hFlush stderr >> exitFailure

addSigReq :: SigData Text -> FilePath -> IO (SigData Text)
addSigReq sd keyFile = do
  kp <- importKeyFile keyFile
  addSigToSigData kp sd

importKeyFile :: FilePath -> IO Ed25519KeyPair
importKeyFile keyFile = do
  v :: A.Value <- decodeYaml keyFile
  let ekp = do
        -- These keys are from genKeys in Main.hs. Might want to convert to a
        -- dedicated data type at some point.
        pub <- getKey "public" v
        sec <- getKey "secret" v

        importEd25519KeyPair (Just $ PubBS pub) (PrivBS sec)
  case ekp of
    Left e -> dieAR $ "Could not parse key file " <> keyFile <> ": " <> e
    Right kp -> return kp
 where
  getKey :: String -> A.Value -> Either String ByteString
  getKey k (A.Object o) = case AKM.lookup (AK.fromString k) o of
    Just (A.String t) -> parseB16TextOnly t
    _ -> Left $ "Error parsing " <> k <> " key"
  getKey k _ = Left $ "Error parsing " <> k <> " key"

yamlOptions :: Y.EncodeOptions
yamlOptions = Y.setFormat (Y.setWidth Nothing Y.defaultFormatOptions) Y.defaultEncodeOptions

apiReq :: FilePath -> Bool -> IO ()
apiReq fp local = do
  (_,exec) <- mkApiReq' False fp
  if local then
    putJSON exec
    else
    putJSON $ SubmitBatch $ exec :| []

uapiReq :: FilePath -> IO ()
uapiReq fp = uapiReq' fp BS.putStrLn

uapiReq' :: FilePath -> (ByteString -> IO ()) -> IO ()
uapiReq' fp p = do
  (_,exec) <- mkApiReq' True fp
  let doEncode :: J.Encode b => b -> IO ()
      doEncode = p . encodeYamlWith yamlOptions
  case commandToSigData exec of
    Left e -> dieAR $ "Error decoding command: " <> e
    Right a -> doEncode a

-- | parts read/rationalized from a processed ApiReq:
-- the ApiReq, code, msg data, PublicMeta
type ApiReqParts = (ApiReq,Text,PactValue,PublicMeta)

-- | Assemble a command and parts from a ApiReq YAML file.
mkApiReq :: FilePath -> IO (ApiReqParts,Command Text)
mkApiReq fp = mkApiReq' False fp

mkApiReq' :: Bool -> FilePath -> IO (ApiReqParts,Command Text)
mkApiReq' unsignedReq fp = mkApiReqCmd unsignedReq fp =<< decodeYaml fp

-- | Assemble a command and parts from an ApiReq.
mkApiReqCmd
  :: Bool
     -- ^ make "unsigned command" for offline signing
  -> FilePath
     -- ^ filepath for chdir for loading files
  -> ApiReq
     -- ^ the ApiReq
  -> IO (ApiReqParts, Command Text)
mkApiReqCmd unsignedReq fp ar@ApiReq{..} =
  case _ylType of
    Just "exec" -> mkApiReqExec unsignedReq ar fp
    Just "cont" -> mkApiReqCont unsignedReq ar fp
    Nothing -> mkApiReqExec unsignedReq ar fp -- Default
    _ -> dieAR "Expected a valid message type: either 'exec' or 'cont'"

-- | Decode yaml or fail in IO.
decodeYaml :: A.FromJSON b => FilePath -> IO b
decodeYaml fp = either (dieAR . show) return =<< Y.decodeFileEither fp

putJSON :: J.Encode b => b -> IO ()
putJSON = BSL.putStrLn . J.encode

-- | The formatting of the result and in particular the sorting items in the
-- result is not specified. Do not use this function if deterministc and
-- repeatable formatting is needed.
--
signCmd
  :: [FilePath]
  -> ByteString
  -- ^ Takes a base64url encoded ByteString
  -> IO ByteString
signCmd keyFiles bs = do
  case decodeBase64UrlUnpadded bs of
    Left e -> dieAR $ "stdin was not valid base64url: " <> e
    Right h -> do
      kps <- mapM importKeyFile keyFiles
      fmap (encodeYaml . J.Object) $ forM kps $ \kp -> do
            let sig = signHash (Hash $ SBS.toShort h) kp
            return ((toB16Text . _b16JsonBytes) (B16JsonBytes (getPublic kp)), sig)

withKeypairsOrSigner
  :: Bool
  -> ApiReq
  -> ([(DynKeyPair, [SigCapability])] -> IO a)
  -> ([Signer] -> IO a)
  -> IO a
withKeypairsOrSigner unsignedReq ApiReq{..} keypairAction signerAction =
  case (_ylSigners,_ylKeyPairs,unsignedReq) of
    (Nothing,Just kps,False) -> mkKeyPairs kps >>= keypairAction
    (Nothing,Nothing,False) -> keypairAction []
    (Just {},_,False) -> dieAR "'signers' invalid in command request"
    (Just ss,Nothing,True) -> signerAction $ map toSigner ss
    (Nothing,Nothing,True) -> dieAR "'signers' required for unsigned request"
    (_,Just {},True) -> dieAR "'keyPairs' invalid in unsigned request"
  where
    toSigner ApiSigner{..} = Signer _asScheme _asPublic _asAddress (fromMaybe [] _asCaps)


mkApiReqExec :: Bool -> ApiReq -> FilePath -> IO (ApiReqParts,Command Text)
mkApiReqExec unsignedReq ar@ApiReq{..} fp = do
  (code,cdata) <- do
    let dir = takeDirectory fp
    code <- case (_ylCodeFile,_ylCode) of
      (Nothing,Just c) -> return c
      (Just f,Nothing) -> T.decodeUtf8 <$> BS.readFile (dir </> f)
      _ -> dieAR "Expected either a 'code' or 'codeFile' entry"
    cdata <- case (_ylDataFile,_ylData) of
      (Nothing,Just v) -> return v -- either (\e -> dieAR $ "Data decode failed: " ++ show e) return $ eitherDecode (BSL.pack v)
      (Just f,Nothing) -> (BSL.readFile (dir </> f)) >>=
                          either (\e -> dieAR $ "Data file load failed: " ++ show e) (return . _stableEncoding) .
                          JD.eitherDecode
      (Nothing,Nothing) -> return PUnit
      _ -> dieAR "Expected either a 'data' or 'dataFile' entry, or neither"
    return (code,cdata)
  pubMeta <- mkPubMeta _ylPublicMeta
  cmd <- withKeypairsOrSigner unsignedReq ar
    (\ks -> mkExec code cdata pubMeta ks (fromMaybe [] _ylVerifiers) _ylNetworkId _ylNonce)
    (\ss -> mkUnsignedExec code cdata pubMeta ss (fromMaybe [] _ylVerifiers) _ylNetworkId _ylNonce)
  return ((ar,code,cdata,pubMeta), cmd)


-- | Get current time as TxCreationTime
getCurrentCreationTime :: IO TxCreationTime
getCurrentCreationTime = TxCreationTime
    . fromIntegral
    . (`div` 1000000)
    . toPosixTimestampMicros
    <$> getCurrentTime

mkPubMeta :: Maybe ApiPublicMeta -> IO PublicMeta
mkPubMeta apm = case apm of
  Nothing -> return def
  (Just ApiPublicMeta {..}) -> do
    ct <- case _apmCreationTime of
      Nothing -> getCurrentCreationTime
      Just t -> return t
    return $ PublicMeta
      { _pmChainId = fromMaybe (ChainId "") _apmChainId
      , _pmSender = fromMaybe "" _apmSender
      , _pmGasLimit = fromMaybe (GasLimit (Gas 0)) _apmGasLimit
      , _pmGasPrice = fromMaybe (GasPrice 0) _apmGasPrice
      , _pmTTL = fromMaybe (TTLSeconds 1800) _apmTTL
      , _pmCreationTime = ct
      }




mkNonce :: Maybe Text -> IO Text
mkNonce = maybe (T.pack . show <$> getCurrentTime) return

-- | Construct an Exec request message
--
mkExec
  :: Text
    -- ^ code
  -> PactValue
    -- ^ optional environment data
  -> PublicMeta
    -- ^ public metadata
  -> [(DynKeyPair, [SigCapability])]
    -- ^ signing keypairs + caplists
  -> [Verifier ParsedVerifierProof]
    -- ^ verifiers
  -> Maybe NetworkId
    -- ^ optional 'NetworkId'
  -> Maybe Text
    -- ^ optional nonce
  -> IO (Command Text)
mkExec code mdata pubMeta kps ves nid ridm = do
  rid <- mkNonce ridm
  cmd <- mkCommandWithDynKeys
         kps
         ves
         (StableEncoding pubMeta)
         rid
         nid
         (Exec (ExecMsg code mdata))
  return $ T.decodeUtf8 <$> cmd

-- | Construct an Exec request message
--
mkUnsignedExec
  :: Text
    -- ^ code
  -> PactValue
    -- ^ optional environment data
  -> PublicMeta
    -- ^ public metadata
  -> [Signer]
    -- ^ payload signers
  -> [Verifier ParsedVerifierProof]
    -- ^ payload verifiers
  -> Maybe NetworkId
    -- ^ optional 'NetworkId'
  -> Maybe Text
    -- ^ optional nonce
  -> IO (Command Text)
mkUnsignedExec code mdata pubMeta kps ves nid ridm = do
  rid <- mkNonce ridm
  cmd <- mkUnsignedCommand
         kps
         ves
         (StableEncoding pubMeta)
         rid
         nid
         (Exec (ExecMsg code mdata))
  return $ T.decodeUtf8 <$> cmd


mkApiReqCont :: Bool -> ApiReq -> FilePath -> IO (ApiReqParts,Command Text)
mkApiReqCont unsignedReq ar@ApiReq{..} fp = do
  apiPactId <- case _ylPactTxHash of
    Just t -> return t
    Nothing -> dieAR "Expected a 'pactTxHash' entry"

  step <- case _ylStep of
    Just s -> return s
    Nothing -> dieAR "Expected a 'step' entry"

  rollback <- case _ylRollback of
    Just r -> return r
    Nothing -> dieAR "Expected a 'rollback' entry"

  cdata <- do
    let dir = takeDirectory fp
    case (_ylDataFile,_ylData) of
      (Nothing,Just v) -> return v -- either (\e -> dieAR $ "Data decode failed: " ++ show e) return $ eitherDecode (BSL.pack v)
      (Just f,Nothing) -> BSL.readFile (dir </> f) >>=
                          either (\e -> dieAR $ "Data file load failed: " ++ show e) (return . _stableEncoding) .
                          JD.eitherDecode
      (Nothing,Nothing) -> return PUnit
      _ -> dieAR "Expected either a 'data' or 'dataFile' entry, or neither"
  let pactId = (DefPactId . hashToText) apiPactId
  pubMeta <- mkPubMeta _ylPublicMeta
  cmd <- withKeypairsOrSigner unsignedReq ar
    (\ks -> mkCont pactId step rollback cdata pubMeta ks (fromMaybe [] _ylVerifiers) _ylNonce _ylProof _ylNetworkId)
    (\ss -> mkUnsignedCont pactId step rollback cdata pubMeta ss (fromMaybe [] _ylVerifiers) _ylNonce _ylProof _ylNetworkId)
  return ((ar,"",cdata,pubMeta), cmd)

-- | Construct a Cont request message
--
mkCont
  :: DefPactId
    -- ^ pact tx hash of the continuation
  -> Int
    -- ^ cont step
  -> Bool
    -- ^ has rollback?
  -> PactValue
    -- ^ environment data
  -> PublicMeta
    -- ^ command public metadata
  -> [(DynKeyPair, [SigCapability])]
    -- ^ signing keypairs
  -> [Verifier ParsedVerifierProof]
    -- ^ verifiers
  -> Maybe Text
    -- ^ optional nonce
  -> Maybe ContProof
    -- ^ optional continuation proof (required for cross-chain)
  -> Maybe NetworkId
    -- ^ optional network id
  -> IO (Command Text)
mkCont txid step rollback mdata pubMeta kps ves ridm proof nid = do
  rid <- mkNonce ridm
  cmd <- mkCommandWithDynKeys
         kps
         ves
         (StableEncoding pubMeta)
         rid
         nid
         (Continuation (ContMsg txid step rollback mdata proof) :: (PactRPC ContMsg))
  return $ T.decodeUtf8 <$> cmd


-- | Construct a Cont request message
--
mkUnsignedCont
  :: DefPactId
    -- ^ pact tx hash of the continuation
  -> Int
    -- ^ cont step
  -> Bool
    -- ^ has rollback?
  -> PactValue
    -- ^ environment data
  -> PublicMeta
    -- ^ command public metadata
  -> [Signer]
    -- ^ payload signers
  -> [Verifier ParsedVerifierProof]
    -- ^ verifiers
  -> Maybe Text
    -- ^ optional nonce
  -> Maybe ContProof
    -- ^ optional continuation proof (required for cross-chain)
  -> Maybe NetworkId
    -- ^ optional network id
  -> IO (Command Text)
mkUnsignedCont txid step rollback mdata pubMeta kps ves ridm proof nid = do
  rid <- mkNonce ridm
  cmd <- mkUnsignedCommand
         kps
         ves
         (StableEncoding pubMeta)
         (T.pack $ show rid)
         nid
         (Continuation (ContMsg txid step rollback mdata proof) :: (PactRPC ContMsg))
  return $ T.decodeUtf8 <$> cmd

-- | Construct a `Command` from a `PactRPC` request, a nonce, and a set of credentials.
-- This is the main entry point for constructing commands.
mkCommand
  :: J.Encode c
  => J.Encode m
  => [(Ed25519KeyPair, [UserCapability])]
  -> [Verifier ParsedVerifierProof]
  -> m
  -> Text
  -> Maybe NetworkId
  -> PactRPC c
  -> IO (Command ByteString)
mkCommand creds vers meta nonce nid rpc = mkCommand' creds encodedPayload
  where
    payload = Payload rpc nonce meta (keyPairsToSigners creds) (nonemptyVerifiers vers) nid
    encodedPayload = J.encodeStrict payload


keyPairToSigner :: Ed25519KeyPair -> [UserCapability] -> Signer
keyPairToSigner cred caps = Signer scheme pub addr caps
      where
        scheme = Nothing
        pub = toB16Text $ exportEd25519PubKey $ fst cred
        addr = Nothing

keyPairsToSigners :: [Ed25519KeyPairCaps] -> [Signer]
keyPairsToSigners creds = map (uncurry keyPairToSigner) creds

signHash :: PactHash.Hash -> Ed25519KeyPair -> Text
signHash hsh (pub,priv) =
  toB16Text $ exportEd25519Signature $ signEd25519 pub priv hsh

-- | Make a Command without signing it. This is used in Chainweb's Rosetta Utils.
mkUnsignedCommand
  :: J.Encode m
  => J.Encode c
  => [Signer]
  -> [Verifier ParsedVerifierProof]
  -> m
  -> Text
  -> Maybe NetworkId
  -> PactRPC c
  -> IO (Command ByteString)
mkUnsignedCommand signers vers meta nonce nid rpc = mkCommand' [] encodedPayload
  where encodedPayload = J.encodeStrict payload
        payload = Payload rpc nonce meta signers (nonemptyVerifiers vers) nid

-- | Given an already-serialized payload, construct a `Command` by signing it with the
-- provided credentials.
mkCommand' :: [(Ed25519KeyPair ,a)] -> ByteString -> IO (Command ByteString)
mkCommand' creds env = do
  let hsh = PactHash.hash env   -- hash associated with a Command, aka a Command's Request Key
      toUserSig (cred,_) = ED25519Sig $ signHash hsh cred
  let sigs = toUserSig <$> creds
  return $ Command env sigs hsh


-- | A utility function used for testing.
-- It generalizes `mkCommand` by taking a `DynKeyPair`, which could contain mock
-- WebAuthn keys. If WebAuthn keys are encountered, this function does mock WebAuthn
-- signature generation when constructing the `Command`.
mkCommandWithDynKeys' :: [(DynKeyPair, a)] -> ByteString -> IO (Command ByteString)
mkCommandWithDynKeys' creds env = do
  let hsh = PactHash.hash env    -- hash associated with a Command, aka a Command's Request Key
  sigs <- traverse (toUserSig hsh) creds
  return $ Command env sigs hsh
  where
    toUserSig :: PactHash.Hash -> (DynKeyPair, a) -> IO UserSig
    toUserSig hsh = \case
      (DynEd25519KeyPair (pub, priv), _) ->
        pure $ ED25519Sig $ signHash hsh (pub, priv)
      (DynWebAuthnKeyPair _ pubWebAuthn privWebAuthn, _) -> do
        signResult <- runExceptT $ signWebauthn pubWebAuthn privWebAuthn "" hsh
        case signResult of
          Left e -> error $ "Failed to sign with mock WebAuthn keypair: " ++ e
          Right sig -> return $ WebAuthnSig sig


-- | Construct a `Command` from a `PactRPC` request, a nonce, and a set of credentials.
-- This function is mainy useful for testing, because DynKeyPairs are either
-- Ed25519 or WebAuthn keys. If you have Ed25519 keys, use `mkCommand`. You will
-- only have access to WebAuthn keys in test (because they are normally managed on
-- the client's and never exposed to the client or the server).
--
-- During testing, you can create fake WebAuthn credentials.
mkCommandWithDynKeys
  :: J.Encode c
  => J.Encode m
  => [(DynKeyPair, [SigCapability])]
  -> [Verifier ParsedVerifierProof]
  -> m
  -> Text
  -> Maybe NetworkId
  -> PactRPC c
  -> IO (Command ByteString)
mkCommandWithDynKeys creds vers meta nonce nid rpc = mkCommandWithDynKeys' creds encodedPayload
  where
    encodedPayload = J.encodeStrict payload
    payload = Payload rpc nonce meta (map credToSigner creds) (nonemptyVerifiers vers) nid
    credToSigner cred =
      case cred of
        (DynEd25519KeyPair (pubEd25519, _), caps) ->
          Signer
            { _siScheme = Nothing
            , _siPubKey = toB16Text (exportEd25519PubKey pubEd25519)
            , _siAddress = Nothing
            , _siCapList = caps
            }
        (DynWebAuthnKeyPair isPrefixed pubWebAuthn _, caps) ->
          let
            prefix = case isPrefixed of
              WebAuthnPubKeyBare -> ""
              WebAuthnPubKeyPrefixed -> webAuthnPrefix
          in Signer
            { _siScheme = Just WebAuthn
            , _siPubKey = prefix <> toB16Text (exportWebAuthnPublicKey pubWebAuthn)
            , _siAddress = Nothing
            , _siCapList = caps
            }

type UserCapability = SigCapability

-- | A utility function for handling the common case of commands
-- with no verifiers. `None` is distinguished from `Just []` in
-- our JSON encodings, which is important for maintaining forward
-- compatibility - old version of the interpreter did non include
-- a `verifiers` field.
nonemptyVerifiers :: [Verifier ParsedVerifierProof] -> Maybe [Verifier ParsedVerifierProof]
nonemptyVerifiers [] = Nothing
nonemptyVerifiers vs = Just vs

-- Parse `APIKeyPair`s into Ed25519 keypairs and WebAuthn keypairs.
-- The keypairs must not be prefixed with "WEBAUTHN-", it accepts
-- only the raw (unprefixed) keys.
mkKeyPairs :: [ApiKeyPair] -> IO [(DynKeyPair, [SigCapability])]
mkKeyPairs keyPairs = traverse mkPair keyPairs
  where

        importValidKeyPair
          :: Maybe PublicKeyBS
          -> PrivateKeyBS
          -> Maybe [SigCapability]
          -> Either String (Ed25519KeyPair, [SigCapability])
        importValidKeyPair pubEd25519 privEd25519 caps = fmap (,maybe [] id caps) $
          importEd25519KeyPair pubEd25519 privEd25519

        isEd25519 :: Maybe PPKScheme -> Bool
        isEd25519 = \case
          Nothing -> True
          Just ED25519 -> True
          _ -> False

        mkPair :: ApiKeyPair -> IO (DynKeyPair, [SigCapability])
        mkPair akp = case (_akpScheme akp, _akpPublic akp, _akpSecret akp, _akpAddress akp) of
          (scheme, pub, priv, Nothing) | isEd25519 scheme ->
            either dieAR (return . first DynEd25519KeyPair) (importValidKeyPair pub priv (_akpCaps akp))
          (scheme, pub, priv, Just addrT) | isEd25519 scheme -> do
            addrBS <- either dieAR return (parseB16TextOnly addrT)
            kp     <- either dieAR return (importValidKeyPair pub priv (_akpCaps akp))

            -- Enforces that user provided address matches the address derived from the Public Key
            -- for transparency and a better user experience. User provided address not used except
            -- for this purpose.
            if addrBS == getPublic (fst kp)
              then (return . first DynEd25519KeyPair) kp
              else dieAR $ "Address provided "
                          ++ show (toB16Text addrBS)
                          ++ " does not match actual Address "
                          ++ show (toB16Text $ getPublic $ fst kp)
          (Just WebAuthn, Just (PubBS pub), PrivBS priv, Nothing) -> do
            pubWebAuthn <- either dieAR return (parseWebAuthnPublicKey pub)
            privWebAuthn <- either dieAR return (parseWebAuthnPrivateKey priv)
            return $ (DynWebAuthnKeyPair WebAuthnPubKeyBare pubWebAuthn privWebAuthn, fromMaybe [] (_akpCaps akp))
          _ -> dieAR $ "Attempted to mix Ed25519 and WebAuthn keys."

dieAR :: String -> IO a
dieAR errMsg = throwM . userError $ intercalate "\n" $
  ["Failure reading request yaml. Valid fields are: "
  ,"Common fields:"
  ,"  type: 'exec' or 'cont', indicating Exec or Cont message type"
  ,"  data|dataFile: (optional) JSON transaction data, as yaml (data) or a yaml file (dataFile)"
  ,"  keyPairs: list of key pairs for signing (use pact -g to generate): ["
  ,"    public: base 16 public key"
  ,"    secret: base 16 secret key"
  ,"    address: (required for ETH) base 16 address"
  ,"    scheme: optional, 'ETH' or 'ED25519', default ED25519"
  ,"    caps: capability list as strings, in form \"[(module.CAP param1 param2)]\""
  ,"    ] "
  ,"  nonce: (optional) request nonce, defaults to current time"
  ,"  publicMeta: (optional) data for public-chain execution: ["
  ,"    chainId: chain or shard identifier"
  ,"    sender: gas-paying sender account"
  ,"    gasLimit: integer gas max limit"
  ,"    gasPrice: decimal gas unit price"
  ,"    ttl: TTL value in seconds"
  ,"    creationTime: epoch time integer value in seconds"
  ,"Exec-only fields:"
  ,"  code|codeFile: [exec only] Pact code, as a string (code) or file path (codeFile)"
  ,"Cont-only fields:"
  ,"  pactTxHash: pact ID to continue"
  ,"  step: step index to continue"
  ,"  rollback: rollback/cancel flag"
  ,"  proof: platform-specific continuation proof data"
  ,"Error message: " ++ errMsg
  ]
