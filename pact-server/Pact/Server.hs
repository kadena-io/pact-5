module Pact.Server where


-- pact -s
serve :: Filepath -> IO ()
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

runPactService :: Config -> (Config -> IO ()) -> IO ()
runPactService config =
validateConfig = undefined
initPactService = undefined
