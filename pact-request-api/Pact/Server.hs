module Pact.Server where

import System.FilePath

-- pact -s
serve :: FilePath -> IO ()
serve configPath = do
  c <- validateConfig configPath
  runPactService c initPactService
  where
    runPactService c act
      = withLoggers c
      . withSqliteEnv c
      . withCorsPolicy c
      . withAsync
      $ act c

data Config = Config

runPactService :: Config -> (Config -> IO ()) -> IO ()
runPactService config =
  undefined

validateConfig = undefined
initPactService = undefined
