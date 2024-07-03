
module Main where

import Pact.Core.Repl
import Pact.Core.Builtin
import Pact.Core.LanguageServer

import qualified Options.Applicative as O
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Control.Applicative ((<|>))

import qualified PackageInfo_pact_tng as PI
import Data.Version (showVersion)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import System.Directory
import System.FilePath
import Data.Foldable
import Pact.Core.Command.Client
import Pact.Core.Command.Crypto
import Pact.Core.Command.Util
import Pact.Core.Repl.Compile
import System.IO
import System.Exit(exitFailure, exitSuccess)

data ReplLoadFile
  = ReplLoadFile
  { _oFindScript :: Bool
  , _oDebug :: Bool
  , _oFile :: String
  } deriving (Eq, Show)

data ReplOpts
  = OVersion
  | OBuiltins
  | OLanguageServer
  | OLoad ReplLoadFile
  -- Sig-related options
  | OAddSigsReq { _oKeyFiles :: [FilePath], _oReqLocal :: Bool }
  | OCombineSigs { _oSigFiles :: [FilePath], _oReqLocal :: Bool }
  | OSignCmd { _oSigFile :: [FilePath] }
  -- Apireq-related
  | OApiReq { _oReqYaml :: FilePath, _oReqLocal :: Bool }
  | OUnsignedReq { _oReqYaml :: FilePath }
  -- Crypto
  | OGenKey
  deriving (Eq, Show)

replOpts :: O.Parser (Maybe ReplOpts)
replOpts = O.optional $
  O.flag' OVersion (O.short 'v' <> O.long "version" <> O.help "Display version")
  <|> O.flag' OBuiltins (O.short 'b' <> O.long "builtins" <> O.help "List builtins")
  <|> O.flag' OLanguageServer (O.long "lsp" <> O.help "Start Language Server")
  <|> apiReqFlag
  <|> unsignedReqFlag
  <|> loadFlag

-- Todo: trace output and coverage?
loadFlag :: O.Parser ReplOpts
loadFlag = fmap OLoad $
  ReplLoadFile
    <$> O.flag False True
        (O.short 'r' <> O.long "findscript" <>
        O.help "For .pact files, attempts to locate a .repl file to execute.")
    <*> O.flag False True
        (O.short 't' <> O.long "trace" <> O.help "Show trace output")
    -- <*> O.flag False True
    --     (O.short 'c' <> O.long "coverage" <> O.help "Generate coverage report coverage/lcov.info")
    <*> O.argument O.str
      (O.metavar "FILE" <> O.help "File path to compile (if .pact extension) or execute.")

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

die :: String -> IO b
die msg = hPutStrLn stderr msg >> hFlush stderr >> exitFailure

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
    OLoad (ReplLoadFile findScript dbg fp)
      | isPactFile fp -> do
        script <- if findScript then locatePactReplScript fp else return Nothing
        case script of
          Just s -> runScript s dbg
          Nothing -> runScript fp dbg
      | otherwise -> runScript fp dbg
  where
    exitEither _ Left {} = die "Load failed"
    exitEither m (Right t) = m t >> exitSuccess
    exitLoad = exitEither (\_ -> hPutStrLn stderr "Load successful" >> hFlush stderr)
    runScript f dolog = execScript dolog f >>= exitLoad
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
