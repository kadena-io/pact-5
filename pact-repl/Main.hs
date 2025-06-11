
module Main where

import Pact.Core.Repl
import Pact.Core.Builtin
import Pact.Core.LanguageServer

import qualified Options.Applicative as O
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Control.Applicative ((<|>))

import Data.Foldable
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Version (showVersion)
import qualified Data.Yaml as Y
import Pact.Core.Command.Client
import Pact.Core.Command.Crypto
import Pact.Core.Command.Server
import Pact.Core.Command.Util
import Pact.Core.Repl.Compile
import Pact.Core.Environment
import Pact.Core.Pretty
import System.IO
import qualified Pact.Core.Version as PI
import System.Directory
import System.Exit(exitFailure, exitSuccess)
import System.FilePath
import Pact.Core.SPV (noSPVSupport)

data OReplLoadFile
  = OReplLoadFile
  { _oFindScript :: Bool
  , _oDebug :: Bool
  , _oCoverage :: Bool
  , _oFile :: String
  } deriving (Eq, Show)

data ReplOpts
  = OVersion
  | OBuiltins
  | OLanguageServer
  | OLoad OReplLoadFile
  -- Sig-related options
  | OAddSigsReq { _oKeyFiles :: [FilePath], _oReqLocal :: Bool }
  | OCombineSigs { _oSigFiles :: [FilePath], _oReqLocal :: Bool }
  | OSignCmd { _oSigFile :: [FilePath] }
  -- Apireq-related
  | OApiReq { _oReqYaml :: FilePath, _oReqLocal :: Bool }
  | OUnsignedReq { _oReqYaml :: FilePath }
  | OServer FilePath
  -- Crypto
  | OGenKey
  | OCheckNativeShadowing FilePath
  deriving (Eq, Show)

replOpts :: O.Parser (Maybe ReplOpts)
replOpts = O.optional $
  O.flag' OVersion (O.short 'v' <> O.long "version" <> O.help "Display version")
  <|> O.flag' OBuiltins (O.short 'b' <> O.long "builtins" <> O.help "List builtins")
  <|> O.flag' OLanguageServer (O.long "lsp" <> O.help "Start Language Server")
  <|> apiReqFlag
  <|> unsignedReqFlag
  <|> O.flag' OGenKey (O.short 'g' <> O.long "genkey" <> O.help "Generate ED25519 keypair")
  <|> loadFlag
  <|> OServer <$> O.strOption (O.metavar "CONFIG" <> O.short 's' <> O.long "server" <> O.help "Run Pact-Server")
  <|> checkNativeShadowingFlag

-- Todo: trace output and coverage?
loadFlag :: O.Parser ReplOpts
loadFlag = fmap OLoad $
  OReplLoadFile
    <$> O.flag False True
        (O.short 'r' <> O.long "findscript" <>
        O.help "For .pact files, attempts to locate a .repl file to execute.")
    <*> O.flag False True
        (O.short 't' <> O.long "trace" <> O.help "Show trace output")
    <*> O.flag False True
        (O.short 'c' <> O.long "coverage" <> O.help "Generate coverage report coverage/lcov.info")
    <*> O.argument O.str
      (O.metavar "FILE" <> O.help "File path to compile (if .pact extension) or execute.")

checkNativeShadowingFlag :: O.Parser ReplOpts
checkNativeShadowingFlag =
  OCheckNativeShadowing
    <$> O.strOption(O.metavar "FILE" <> O.long "check-shadowing" <> O.help "Run a native shadowing check over a particular .pact or .repl file")

argParser :: O.ParserInfo (Maybe ReplOpts)
argParser = O.info (O.helper <*> replOpts)
            (O.fullDesc <> O.header "The Pact Smart Contract Language Interpreter")

apiReqFlag :: O.Parser ReplOpts
apiReqFlag =
  OApiReq
  <$> O.strOption (O.short 'a' <> O.long "apireq" <> O.metavar "REQ_YAML" <>
                  O.help "Format API request JSON using REQ_YAML file")
  <*> localFlag

unsignedReqFlag :: O.Parser ReplOpts
unsignedReqFlag = OUnsignedReq
  <$> O.strOption (O.short 'u' <> O.long "unsigned" <> O.metavar "REQ_YAML" <>
                  O.help "Format unsigned API request JSON using REQ_YAML file")

localFlag :: O.Parser Bool
localFlag = O.flag False True (O.short 'l' <> O.long "local" <> O.help "Format for /local endpoint")

exitFailureWithMessage :: String -> IO b
exitFailureWithMessage msg = hPutStrLn stderr msg >> hFlush stderr >> exitFailure

exitSuccessWithMessage :: String -> IO b
exitSuccessWithMessage msg = hPutStrLn stdout msg >> hFlush stdout >> exitSuccess

main :: IO ()
main = O.execParser argParser >>= \case
  Nothing -> runRepl
  Just v -> case v of
    OVersion -> printVersion
    OBuiltins -> printBuiltins
    OLanguageServer -> startLSP
    OUnsignedReq cf -> uapiReq cf
    OAddSigsReq kf l -> BS8.putStrLn =<< addSigsReq kf l =<< BS.getContents
    OCombineSigs sigs l -> BS8.putStrLn =<< combineSigs sigs l
    OApiReq cf l -> apiReq cf l
    OSignCmd kfs -> BS8.putStrLn =<< signCmd kfs =<< fmap (T.encodeUtf8 . T.strip) T.getContents
    OGenKey -> genKeys
    OLoad (OReplLoadFile findScript dbg coverage fp)
      | isPactFile fp -> do
        script <- if findScript then locatePactReplScript fp else return Nothing
        case script of
          Just s -> runScript s dbg coverage
          Nothing -> runScript fp dbg coverage
      | otherwise -> runScript fp dbg coverage
    OServer configPath -> Y.decodeFileEither configPath >>= \case
      Left perr -> putStrLn $ Y.prettyPrintParseException perr
      Right config -> runServer config noSPVSupport
    OCheckNativeShadowing fp -> checkParsedShadows fp
  where
    runScript f dolog cov = execScript dolog cov f >>= \case
      (Left pe, state) -> do
        let renderedError = renderLocatedPactErrorFromState state pe
        exitFailureWithMessage ((T.unpack renderedError) <> "\nLoad failed")
      (Right _, state) -> do
        let testResults = filter (\rs -> _trResult rs /= ReplTestPassed) $ reverse (_replTestResults state)
        case testResults of
          [] -> exitSuccessWithMessage "Load successful"
          (vsep . fmap pretty -> results) -> do
            T.putStrLn $ renderCompactText' results
            exitFailureWithMessage "Load failed"
    printVersion = putStrLn ("pact version " <> showVersion PI.version)
    printBuiltins = traverse_ (\bi -> T.putStrLn $ "\"" <> bi <> "\"") replCoreBuiltinNames

genKeys :: IO ()
genKeys = do
  kp <- genKeyPair
  putStrLn $ "public: " ++ T.unpack (toB16Text $ getPublic kp)
  putStrLn $ "secret: " ++ T.unpack (toB16Text $ getPrivate kp)

-- | Run heuristics to find a repl script. First is the file name with ".repl" extension;
-- if not, it will see if there is a single ".repl" file in the directory, and if so
-- use that.
locatePactReplScript :: FilePath -> IO (Maybe FilePath)
locatePactReplScript fp = do
  let r = dropExtension fp ++ ".repl"
  b <- doesFileExist r
  if b then return $ Just r
    else do
      let dir = takeDirectory fp
      rs <- filter ((== ".repl") . takeExtension) <$> getDirectoryContents dir
      case rs of
        [a] -> return $ Just $ combine dir a
        _ -> return Nothing
