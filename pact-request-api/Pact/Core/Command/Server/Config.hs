-- | 

module Pact.Core.Command.Server.Config where

import qualified Data.Yaml as Y
--import Data.Word
import Data.Text (Text)
import qualified Pact.JSON.Decode as JD

import GHC.Generics

-- | Commandline configuration for running a Pact server.
data Config = Config
  { _port :: Int
  , _persistDir :: Maybe FilePath
  , _logDir :: FilePath
  -- , _pragmas :: [Pragma]
  -- , _verbose :: Bool
  -- , _gasLimit :: Maybe Int
  -- , _gasRate :: Maybe Int
  } deriving (Eq,Show,Generic)

-- | Pragma for configuring a SQLite database.
newtype Pragma = Pragma Text
  deriving (Eq, Show, Generic)


instance JD.FromJSON Config where
  parseJSON = JD.withObject "Config" $ \o ->
    Config <$> o JD..: "port"
    <*> o JD..:? "persistDir"
    <*> o JD..: "logDir"
  --  <*> o 

-- validateConfigFile :: FilePath -> IO Config
-- validateConfigFile fp = Y.decodeFileEither fp >>= \case
--   Left pe -> do
--     putStrLn usage
--     throwIO $ userError $ "Error loading config file: " ++ show pe
--   Right v -> do
--     traverse_ (createDirectoryIfMissing True) $ _persistDir v
--     createDirectoryIfMissing True $ _logDir vX2
--     pure v
