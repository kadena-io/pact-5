{-# LANGUAGE InstanceSigs #-}
-- |

module Pact.Core.Command.Server.Config where

import qualified Data.Yaml as Y
import Control.Exception.Safe
import Data.Foldable
import System.Directory
import qualified Pact.JSON.Decode as JD

import GHC.Generics

-- | Commandline configuration for running a Pact server.
data Config = Config
  { _port :: Int
  , _persistDir :: Maybe FilePath
  , _logDir :: Maybe FilePath
  , _verbose :: Bool
  , _gasLimit :: Maybe Int
  } deriving (Eq,Show,Generic)


instance JD.FromJSON Config where
  parseJSON :: JD.Value -> Y.Parser Config
  parseJSON = JD.withObject "Config" $ \o ->
    Config <$> o JD..: "port"
    <*> o JD..:? "persistDir"
    <*> o JD..:? "logDir"
    <*> o JD..: "verbose"
    <*> o JD..:? "gasLimit"

usage :: String
usage = unlines
  [ "Config file is YAML format with the following properties:"
  , "port       - HTTP server port"
  , "persistDir - Directory for database files."
  , "             If omitted, runs in-memory only."
  , "logDir     - Directory for HTTP logs, defaults to no log dir"
  , "gasLimit   - Gas limit for each transaction, defaults to 0"
  , "gasRate    - Gas price per action, defaults to 0"
  , "execConfig - Pact runtime execution flags"
  , "verbose    - Output additional information"
  , "\n"
  ]

validateConfigFile :: FilePath -> IO Config
validateConfigFile fp = Y.decodeFileEither fp >>= \case
  Left pe -> do
    putStrLn usage
    throwIO $ userError $ "Error loading config file: " ++ show pe
  Right v -> do
    (traverse_.traverse_) (createDirectoryIfMissing True) [_persistDir v, _logDir v]
    pure v
