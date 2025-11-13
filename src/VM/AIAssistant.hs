{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module VM.AIAssistant where

import Data.Word
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson
import GHC.Generics
import Network.HTTP.Simple
import Network.HTTP.Types
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as BS
import Data.Aeson (encode, decode, object, (.=), (.:))
import Control.Concurrent
import Control.Concurrent.STM
import VM.OS
import VM.VM

-- AI Assistant configuration
data AIConfig = AIConfig
  { aiBaseURL :: Text
  , aiAPIKey :: Text
  , aiModel :: Text
  }

defaultAIConfig :: AIConfig
defaultAIConfig = AIConfig
  { aiBaseURL = "https://api.llm7.io/v1"
  , aiAPIKey = "unused"
  , aiModel = "gpt-4.1-2025-04-14"  -- Use powerful model
  }

-- AI Message
data AIMessage = AIMessage
  { role :: Text
  , content :: Text
  } deriving (Generic, ToJSON, FromJSON)

-- AI Request
data AIRequest = AIRequest
  { model :: Text
  , messages :: [AIMessage]
  , tools :: Maybe [AITool]
  } deriving (Generic, ToJSON, FromJSON)

-- AI Tool for OS control
data AITool = AITool
  { toolType :: Text
  , toolFunction :: AIToolFunction
  } deriving (Generic, ToJSON, FromJSON)

data AIToolFunction = AIToolFunction
  { functionName :: Text
  , functionDescription :: Text
  , functionParameters :: Value
  } deriving (Generic, ToJSON, FromJSON)

-- AI Response
data AIResponse = AIResponse
  { choices :: [AIChoice]
  } deriving (Generic, FromJSON)

data AIChoice = AIChoice
  { message :: AIMessage
  } deriving (Generic, FromJSON)

-- AI Assistant state
data AIAssistant = AIAssistant
  { config :: AIConfig
  , conversationHistory :: TVar [AIMessage]
  , kernel :: KernelState
  , vmInstance :: MVar VMInstance
  }

-- Create AI Assistant
createAIAssistant :: KernelState -> MVar VMInstance -> IO AIAssistant
createAIAssistant kernel vm = do
  history <- newTVarIO []
  return $ AIAssistant
    { config = defaultAIConfig
    , conversationHistory = history
    , kernel = kernel
    , vmInstance = vm
    }

-- Send message to AI
sendAIMessage :: AIAssistant -> Text -> IO (Maybe Text)
sendAIMessage assistant userMessage = do
  history <- readTVarIO (conversationHistory assistant)
  
  let messages = history ++ [AIMessage "user" userMessage]
      request = AIRequest
        { model = aiModel (config assistant)
        , messages = messages
        , tools = Just [createProcessTool, killProcessTool, getProcessesTool, systemCallTool, vmControlTool]
        }
  
  -- Make HTTP request to AI API
  response <- makeAIRequest (config assistant) request
  
  case response of
    Just aiResponse -> do
      if not (null (choices aiResponse))
        then do
          let assistantMessage = message (head (choices aiResponse))
          atomically $ do
            currentHistory <- readTVar (conversationHistory assistant)
            writeTVar (conversationHistory assistant) (currentHistory ++ [AIMessage "user" userMessage, assistantMessage])
          return $ Just (content assistantMessage)
        else return Nothing
    Nothing -> return Nothing

-- Make HTTP request to AI API
makeAIRequest :: AIConfig -> AIRequest -> IO (Maybe AIResponse)
makeAIRequest cfg request = do
  let url = T.unpack (aiBaseURL cfg) ++ "/chat/completions"
      apiKey = T.unpack (aiAPIKey cfg)
  
  -- Create HTTP request
  req <- parseRequest url
  let req' = setRequestMethod "POST"
           . setRequestHeader "Content-Type" ["application/json"]
           . setRequestHeader "Authorization" [B8.pack $ "Bearer " ++ apiKey]
           . setRequestBodyLBS (encode request)
           $ req
  
  -- Execute request
  response <- httpLBS req'
  let status = getResponseStatusCode response
      body = getResponseBody response
  
  if status == 200
    then case decode body of
      Just aiResponse -> return $ Just aiResponse
      Nothing -> return Nothing
    else return Nothing

-- AI Tools for OS control
createProcessTool :: AITool
createProcessTool = AITool
  { toolType = "function"
  , toolFunction = AIToolFunction
      { functionName = "create_process"
      , functionDescription = "Create a new process in the OS"
      , functionParameters = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "name" .= object
                  [ "type" .= ("string" :: Text)
                  , "description" .= ("Process name" :: Text)
                  ]
              ]
          , "required" .= (["name"] :: [Text])
          ]
      }
  }

killProcessTool :: AITool
killProcessTool = AITool
  { toolType = "function"
  , toolFunction = AIToolFunction
      { functionName = "kill_process"
      , functionDescription = "Kill a process by PID"
      , functionParameters = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "pid" .= object
                  [ "type" .= ("number" :: Text)
                  , "description" .= ("Process ID to kill" :: Text)
                  ]
              ]
          , "required" .= (["pid"] :: [Text])
          ]
      }
  }

getProcessesTool :: AITool
getProcessesTool = AITool
  { toolType = "function"
  , toolFunction = AIToolFunction
      { functionName = "get_processes"
      , functionDescription = "Get list of all running processes"
      , functionParameters = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object []
          , "required" .= ([] :: [Text])
          ]
      }
  }

systemCallTool :: AITool
systemCallTool = AITool
  { toolType = "function"
  , toolFunction = AIToolFunction
      { functionName = "system_call"
      , functionDescription = "Execute a system call"
      , functionParameters = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "syscall_id" .= object
                  [ "type" .= ("number" :: Text)
                  , "description" .= ("System call ID" :: Text)
                  ]
              , "args" .= object
                  [ "type" .= ("array" :: Text)
                  , "description" .= ("System call arguments" :: Text)
                  ]
              ]
          , "required" .= (["syscall_id", "args"] :: [Text])
          ]
      }
  }

vmControlTool :: AITool
vmControlTool = AITool
  { toolType = "function"
  , toolFunction = AIToolFunction
      { functionName = "vm_control"
      , functionDescription = "Control the VM (step, run, reset, load program)"
      , functionParameters = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "action" .= object
                  [ "type" .= ("string" :: Text)
                  , "enum" .= (["step", "run", "reset", "load"] :: [Text])
                  , "description" .= ("VM action to perform" :: Text)
                  ]
              ]
          , "required" .= (["action"] :: [Text])
          ]
      }
  }

-- Execute AI tool call
executeAITool :: AIAssistant -> Text -> Value -> IO (Maybe Text)
executeAITool assistant toolName params = case toolName of
  "create_process" -> do
    let name = T.pack $ maybe "unnamed" id (lookup "name" (objectToList params))
    pid <- createProcess (kernel assistant) name
    return $ Just (T.pack $ "Process created with PID: " ++ show pid)
  
  "kill_process" -> do
    let pid = maybe 0 fromIntegral (lookup "pid" (objectToList params))
    success <- killProcess (kernel assistant) pid
    return $ Just (if success then "Process killed" else "Process not found")
  
  "get_processes" -> do
    processes <- getProcesses (kernel assistant)
    return $ Just (T.pack $ show processes)
  
  "system_call" -> do
    let syscallId = maybe 0 fromIntegral (lookup "syscall_id" (objectToList params))
        args = maybe [] id (lookup "args" (objectToList params))
    result <- handleSystemCall (kernel assistant) syscallId args
    return $ Just (T.pack $ "System call result: " ++ show result)
  
  "vm_control" -> do
    let action = maybe "step" id (lookup "action" (objectToList params))
    vm <- takeMVar (vmInstance assistant)
    case action of
      "step" -> do
        _ <- stepVM vm
        putMVar (vmInstance assistant) vm
        return $ Just "VM stepped"
      "run" -> do
        _ <- runVM vm
        putMVar (vmInstance assistant) vm
        return $ Just "VM ran"
      "reset" -> do
        -- Reset VM
        putMVar (vmInstance assistant) vm
        return $ Just "VM reset"
      _ -> return $ Just "Unknown action"
  
  _ -> return Nothing

-- Helper to convert Value to list of key-value pairs
objectToList :: Value -> [(Text, Maybe Text)]
objectToList (Object obj) = map (\(k, v) -> (k, valueToText v)) (toList obj)
objectToList _ = []

valueToText :: Value -> Maybe Text
valueToText (String t) = Just t
valueToText (Number n) = Just (T.pack $ show n)
valueToText (Bool b) = Just (T.pack $ show b)
valueToText _ = Nothing

