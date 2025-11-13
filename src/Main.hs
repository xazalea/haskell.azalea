{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200, status404, status500)
import Network.Wai.Middleware.Cors
import Data.Aeson (ToJSON, FromJSON, encode, decode, Value, object, (.=))
import Data.ByteString.Lazy (toStrict, fromStrict)
import Data.ByteString.Char8 (pack, unpack)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import GHC.Generics
import System.Environment (getEnv)
import Control.Exception (try, SomeException)
import Control.Concurrent.MVar
import Data.IORef
import System.IO.Unsafe
import VM.VM
import VM.Core
import VM.Executor (exampleProgram)
import VM.WebSocket
import VM.OS
import VM.AIAssistant
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets as WS

-- API Response types
data ApiResponse = ApiResponse
  { success :: Bool
  , message :: Maybe Text
  , data_ :: Maybe Value
  } deriving (Generic, ToJSON, FromJSON)

data FileInfo = FileInfo
  { name :: Text
  , type_ :: Text
  } deriving (Generic, ToJSON, FromJSON)

data TerminalResponse = TerminalResponse
  { success :: Bool
  , output :: Text
  } deriving (Generic, ToJSON, FromJSON)

data VMResponse = VMResponse
  { success :: Bool
  , state :: Value
  } deriving (Generic, ToJSON, FromJSON)

-- Global VM instance with default resolution
vmInstance :: MVar VMInstance
vmInstance = unsafePerformIO $ do
  vm <- createVMInstance Nothing  -- Will use default resolution
  newMVar vm

-- Global kernel for OS features
kernelInstance :: KernelState
kernelInstance = unsafePerformIO createKernel

-- Global AI Assistant
aiAssistantInstance :: MVar AIAssistant
aiAssistantInstance = unsafePerformIO $ do
  assistant <- createAIAssistant kernelInstance vmInstance
  newMVar assistant

-- Main application with WebSocket support
app :: Application
app = WaiWS.websocketsOr WS.defaultConnectionOptions wsApp httpApp

-- WebSocket application
wsApp :: WS.ServerApp
wsApp pending = handleWebSocket vmInstance pending

-- HTTP application
httpApp :: Application
httpApp = cors (const $ Just simpleCorsResourcePolicy
  { corsOrigins = Nothing
  , corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
  , corsRequestHeaders = ["Content-Type"]
  }) $ \req respond -> do
    let path = pathInfo req
        method = requestMethod req
    
    response <- case (method, path) of
      ("OPTIONS", _) -> return $ responseLBS status200 [] ""
      ("GET", ["api", "terminal"]) -> handleTerminal req
      ("POST", ["api", "terminal"]) -> handleTerminalCommand req
      ("GET", ["api", "files"]) -> handleListFiles req
      ("GET", ["api", "health"]) -> handleHealth
      ("GET", ["api", "vm", "state"]) -> handleVMState req
      ("POST", ["api", "vm", "step"]) -> handleVMStep req
      ("POST", ["api", "vm", "run"]) -> handleVMRun req
      ("POST", ["api", "vm", "load"]) -> handleVMLoad req
      ("POST", ["api", "ai", "message"]) -> handleAIMessage req
      ("GET", ["api", "ai", "models"]) -> handleAIModels req
      ("GET", ["api", "processes"]) -> handleGetProcesses req
      ("POST", ["api", "processes"]) -> handleCreateProcess req
      ("DELETE", ["api", "processes", pid]) -> handleKillProcess req (read pid)
      _ -> return $ responseLBS status404 [] "Not Found"
    
    respond response

handleHealth :: IO Response
handleHealth = do
  let response = encode $ ApiResponse True (Just "Azalea Haskell API is running") Nothing
  return $ responseLBS status200 [("Content-Type", "application/json")] response

handleTerminal :: Request -> IO Response
handleTerminal _ = do
  let output = "Welcome to Azalea Linux Desktop\n$ "
      response = encode $ TerminalResponse True (pack output)
  return $ responseLBS status200 [("Content-Type", "application/json")] response

handleTerminalCommand :: Request -> IO Response
handleTerminalCommand req = do
  body <- requestBody req
  let bodyText = unpack $ decodeUtf8 $ toStrict body
      command = if null bodyText then "echo 'No command'" else bodyText
  
  -- Simulate command execution
  let output = "Command executed: " ++ command ++ "\n$ "
      response = encode $ TerminalResponse True (pack output)
  return $ responseLBS status200 [("Content-Type", "application/json")] response

handleListFiles :: Request -> IO Response
handleListFiles _ = do
  let files = [ FileInfo "home" "directory"
              , FileInfo "Documents" "directory"
              , FileInfo "Downloads" "directory"
              , FileInfo "Desktop" "directory"
              ]
      response = encode files
  return $ responseLBS status200 [("Content-Type", "application/json")] response

handleVMState :: Request -> IO Response
handleVMState _ = do
  vm <- takeMVar vmInstance
  stateJSON <- getVMStateJSON vm
  putMVar vmInstance vm
  let response = encode $ VMResponse True stateJSON
  return $ responseLBS status200 [("Content-Type", "application/json")] response

handleVMStep :: Request -> IO Response
handleVMStep _ = do
  vm <- takeMVar vmInstance
  newState <- stepVM vm
  stateJSON <- getVMStateJSON vm
  putMVar vmInstance vm
  let response = encode $ VMResponse True stateJSON
  return $ responseLBS status200 [("Content-Type", "application/json")] response

handleVMRun :: Request -> IO Response
handleVMRun _ = do
  vm <- takeMVar vmInstance
  finalState <- runVM vm
  stateJSON <- getVMStateJSON vm
  putMVar vmInstance vm
  let response = encode $ VMResponse True stateJSON
  return $ responseLBS status200 [("Content-Type", "application/json")] response

handleVMLoad :: Request -> IO Response
handleVMLoad req = do
  body <- requestBody req
  let bodyText = unpack $ decodeUtf8 $ toStrict body
  -- For now, load example program
  vm <- takeMVar vmInstance
  loadProgram vm exampleProgram
  let vm' = vm { vmProgram = exampleProgram }
  putMVar vmInstance vm'
  stateJSON <- getVMStateJSON vm'
  let response = encode $ VMResponse True stateJSON
  return $ responseLBS status200 [("Content-Type", "application/json")] response

handleAIMessage :: Request -> IO Response
handleAIMessage req = do
  body <- requestBody req
  let bodyText = unpack $ decodeUtf8 $ toStrict body
  case decode (fromStrict $ encodeUtf8 $ pack bodyText) of
    Just (Object obj) -> do
      let message = maybe "" unpack (obj .:? "message" :: Maybe (Maybe Text))
      assistant <- takeMVar aiAssistantInstance
      response <- sendAIMessage assistant (pack message)
      putMVar aiAssistantInstance assistant
      let responseText = maybe "No response" unpack response
          apiResponse = encode $ ApiResponse True (Just $ pack responseText) Nothing
      return $ responseLBS status200 [("Content-Type", "application/json")] apiResponse
    _ -> do
      let apiResponse = encode $ ApiResponse False (Just "Invalid request") Nothing
      return $ responseLBS status400 [("Content-Type", "application/json")] apiResponse

handleAIModels :: Request -> IO Response
handleAIModels _ = do
  let models = [ "gpt-4.1-nano-2025-04-14", "gpt-4.1-2025-04-14", "gpt-5-mini",
                 "gpt-o4-mini-2025-04-16", "deepseek-v3.1", "mistral-small-3.1-24b-instruct-2503",
                 "codestral-2405", "codestral-2501", "gemini-2.5-flash-lite", "gemini-search",
                 "llama-3.1-8B-instruct", "bidara", "glm-4.5-flash", "rtist" ]
      apiResponse = encode $ ApiResponse True Nothing (Just $ object ["models" .= models])
  return $ responseLBS status200 [("Content-Type", "application/json")] apiResponse

handleGetProcesses :: Request -> IO Response
handleGetProcesses _ = do
  processes <- getProcesses kernelInstance
  let apiResponse = encode processes
  return $ responseLBS status200 [("Content-Type", "application/json")] apiResponse

handleCreateProcess :: Request -> IO Response
handleCreateProcess req = do
  body <- requestBody req
  let bodyText = unpack $ decodeUtf8 $ toStrict body
  case decode (fromStrict $ encodeUtf8 $ pack bodyText) of
    Just (Object obj) -> do
      let name = maybe "unnamed" unpack (obj .:? "name" :: Maybe (Maybe Text))
      pid <- createProcess kernelInstance (pack name)
      let apiResponse = encode $ ApiResponse True (Just $ pack $ "Process created: " ++ show pid) (Just $ object ["pid" .= pid])
      return $ responseLBS status200 [("Content-Type", "application/json")] apiResponse
    _ -> do
      let apiResponse = encode $ ApiResponse False (Just "Invalid request") Nothing
      return $ responseLBS status400 [("Content-Type", "application/json")] apiResponse

handleKillProcess :: Request -> Word32 -> IO Response
handleKillProcess _ pid = do
  success <- killProcess kernelInstance pid
  let apiResponse = encode $ ApiResponse success (Just $ if success then "Process killed" else "Process not found") Nothing
  return $ responseLBS status200 [("Content-Type", "application/json")] apiResponse

main :: IO ()
main = do
  port <- maybe 3000 read <$> (try (getEnv "PORT") :: IO (Either SomeException String))
  putStrLn $ "Starting Azalea Haskell server on port " ++ show port
  putStrLn "AI Assistant enabled - OS control via natural language"
  run port app

