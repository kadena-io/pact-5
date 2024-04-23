{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module GPTUtils where


import Network.HTTP.Simple (getResponseBody, httpLBS, setRequestBodyJSON, setRequestHeader, setRequestHost, setRequestMethod, setRequestPath, setRequestPort, setRequestSecure, defaultRequest, getResponseStatusCode, Response)
import Data.Aeson
import GHC.Generics (Generic)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Lazy.Char8 as LBS


import Control.Monad (unless,when,forM)
import Data.Functor ((<&>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (lookupEnv,setEnv)
import Prelude

getEnvOrLetSet :: String -> IO (String)
getEnvOrLetSet varName = do
    varVal <- lookupEnv varName
    case varVal of
        Just value -> return value
        Nothing -> do
            putStrLn $ varName ++ " is not set. Please enter a value:"
            newValue <- getLine
            setEnv varName newValue
            return newValue


apiKey :: IO String
apiKey =
   getEnvOrLetSet "GPTAPIKEY"


-- Define a data type for the message
data Message = Message
  { role :: String
  , content :: String
  } deriving (Show, Generic)
instance FromJSON Message

instance ToJSON Message where
    toJSON (Message role content) =
        object ["role" .= role, "content" .= content]

data ChatResponse = ChatResponse
  { choices :: [Choice]
  } deriving (Show, Generic)

instance FromJSON ChatResponse

data Choice = Choice
  { message :: Message
  } deriving (Show, Generic)

instance FromJSON Choice


-- Define a data type for the request body
data ChatGPTRequest = ChatGPTRequest
  { model :: String
  , messages :: [Message]
  , temperature :: Double
  } deriving (Show, Generic)

instance ToJSON ChatGPTRequest where
    toJSON (ChatGPTRequest model messages temperature) =
        object ["model" .= model, "messages" .= messages, "temperature" .= temperature]


makeChatGPTRequest' :: [Message] -> IO (Maybe String)
makeChatGPTRequest' msgs = do
  apiKey' <- apiKey
  let requestBody = ChatGPTRequest "gpt-4" msgs 1.0
  let request = setRequestMethod "POST"
              $ setRequestSecure True
              $ setRequestPort 443
              $ setRequestHost "api.openai.com"
              $ setRequestPath "/v1/chat/completions"
              $ setRequestHeader "Content-Type" ["application/json"]
              $ setRequestHeader "Authorization" ["Bearer " <> LBS.toStrict (LBS.pack apiKey')]
              $ setRequestBodyJSON requestBody
              $ defaultRequest
  response <- httpLBS request
  putStrLn $ "Status code: " ++ show (getResponseStatusCode response)
  return $ getContentFromResponse (getResponseBody response)
    -- Just r -> return r
    -- Nothing -> error $ "unable to get recent msg from: " ++ LBS.unpack (getResponseBody response)


getContentFromResponse :: LBS.ByteString -> Maybe String
getContentFromResponse responseBody =
  case decode responseBody :: Maybe ChatResponse of
    Just chatResponse ->
      case choices chatResponse of
        (x:_) -> Just $ content $ message x
        [] -> Nothing
    Nothing -> Nothing


data GPTTask = GPTTask
   { _initialPrompt :: String
   , _maxAttempts :: Int
   , _evalFn :: String -> IO (Maybe String)
     -- will return Nothing if result is Correct, and Just feedback if it is not 
   }



simpleMathTask :: GPTTask
simpleMathTask = GPTTask
  { _initialPrompt = "What is 5 + 7?"
  , _maxAttempts = 3
  , _evalFn = \response -> return $ if response == "12" then Nothing else Just "Try again, the answer is not correct."
  }

triviaQuestionTask :: GPTTask
triviaQuestionTask = GPTTask
  { _initialPrompt = "Who wrote 'To Kill a Mockingbird'?"
  , _maxAttempts = 2
  , _evalFn = \response -> return $ if response == "Harper Lee" then Nothing else Just "Incorrect, please try again."
  }


tryWithGPT' :: Bool -> GPTTask -> IO (Maybe Text)
tryWithGPT' verbose task = tryWithGPTHelper verbose task 0 []

tryWithGPTHelper :: Bool -> GPTTask -> Int -> [Message] -> IO (Maybe Text)
tryWithGPTHelper verbose task attempt messages 
  | attempt >= _maxAttempts task = do
      when verbose $ do
          forM messages (putStrLn . show)
          TIO.putStrLn "Maximum attempts exceeded"
      return Nothing
  | otherwise = do
      when verbose $ TIO.putStrLn $ T.concat ["Attempt ", T.pack (show attempt), ": trying with ChatGPT"]
      let initialPrompt = _initialPrompt task
      -- Construct a new message for the initial prompt if this is the first attempt
      let initialMessage = if null messages 
                               then [Message "system" initialPrompt] 
                               else messages
      -- Call the ChatGPT API with the messages
      makeChatGPTRequest' initialMessage >>= \case
        Just result -> do
          when verbose $ TIO.putStrLn "Evaluating response..."
          _evalFn task result >>= \case
            Nothing -> do
              when verbose $ TIO.putStrLn "Task solved"
              return (Just $ T.pack result)
            Just feedback -> do
              when verbose $ TIO.putStrLn $ T.pack feedback
              -- Append the new attempt and feedback to the conversation history and retry
              let newMessages = initialMessage ++ [Message "user" result, Message "system" feedback]
              tryWithGPTHelper verbose task (attempt + 1) newMessages
        Nothing -> do
          when verbose $ TIO.putStrLn "Failed to get response from ChatGPT"
          return Nothing
