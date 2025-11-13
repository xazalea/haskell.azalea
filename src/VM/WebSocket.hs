{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module VM.WebSocket where

import Network.Wai
import Network.Wai.Handler.Warp
import Network.WebSockets
import qualified Network.Wai.Handler.WebSockets as WaiWS
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import System.IO.Unsafe
import VM.VM
import VM.Core
import VM.Executor (exampleProgram)

-- WebSocket message types
data WSMessage = WSMessage
  { msgType :: Text
  , msgData :: Value
  } deriving (Generic, ToJSON, FromJSON)

data WSRequest = WSRequest
  { reqType :: Text
  , reqId :: Maybe Text
  , reqData :: Value
  } deriving (Generic, ToJSON, FromJSON)

data WSResponse = WSResponse
  { respType :: Text
  , respId :: Maybe Text
  , respSuccess :: Bool
  , respData :: Value
  } deriving (Generic, ToJSON, FromJSON)

-- Client connection state
data ClientConnection = ClientConnection
  { clientId :: Text
  , clientConn :: Connection
  , clientVM :: MVar VMInstance
  , clientLastActivity :: TVar Int
  }

-- Handle WebSocket connection
handleWebSocket :: MVar VMInstance -> PendingConnection -> IO ()
handleWebSocket vmInstance pending = do
  conn <- acceptRequest pending
  clientId <- generateClientId
  lastActivity <- newTVarIO 0
  
  let client = ClientConnection
        { clientId = clientId
        , clientConn = conn
        , clientVM = vmInstance
        , clientLastActivity = lastActivity
        }
  
  -- Send initial connection message
  sendTextData conn $ encode $ WSResponse
    { respType = "connected"
    , respId = Nothing
    , respSuccess = True
    , respData = object ["clientId" .= clientId]
    }
  
  -- Start message handling loop
  handleMessages client

-- Generate unique client ID
generateClientId :: IO Text
generateClientId = do
  time <- getCurrentTimeMicroseconds
  return $ T.pack $ "client_" ++ show time

getCurrentTimeMicroseconds :: IO Int
getCurrentTimeMicroseconds = do
  -- Simplified - use thread ID as unique identifier
  tid <- myThreadId
  return $ fromIntegral $ hash (show tid)

hash :: String -> Int
hash = foldl (\acc c -> acc * 31 + fromEnum c) 0

-- Handle incoming messages
handleMessages :: ClientConnection -> IO ()
handleMessages client = do
  msg <- receiveDataMessage (clientConn client)
  case msg of
    (Text msgText _) -> do
      case decode (TE.encodeUtf8 msgText) of
        Just req -> handleRequest client req
        Nothing -> sendError client "Invalid JSON"
    (Binary msgBytes) -> do
      -- Handle binary messages (e.g., framebuffer updates)
      handleBinaryMessage client msgBytes
  handleMessages client

-- Handle JSON request
handleRequest :: ClientConnection -> WSRequest -> IO ()
handleRequest client req = do
  case reqType req of
    "vm_state" -> handleVMStateRequest client req
    "vm_step" -> handleVMStepRequest client req
    "vm_run" -> handleVMRunRequest client req
    "vm_load" -> handleVMLoadRequest client req
    "keyboard" -> handleKeyboardRequest client req
    "mouse" -> handleMouseRequest client req
    "ping" -> handlePingRequest client req
    _ -> sendError client $ "Unknown request type: " <> reqType req

-- Handle VM state request
handleVMStateRequest :: ClientConnection -> WSRequest -> IO ()
handleVMStateRequest client req = do
  vm <- readMVar (clientVM client)
  stateJSON <- getVMStateJSON vm
  sendResponse client req $ WSResponse
    { respType = "vm_state"
    , respId = reqId req
    , respSuccess = True
    , respData = stateJSON
    }

-- Handle VM step request
handleVMStepRequest :: ClientConnection -> WSRequest -> IO ()
handleVMStepRequest client req = do
  vm <- takeMVar (clientVM client)
  newState <- stepVM vm
  stateJSON <- getVMStateJSON vm
  putMVar (clientVM client) vm
  sendResponse client req $ WSResponse
    { respType = "vm_step"
    , respId = reqId req
    , respSuccess = True
    , respData = stateJSON
    }

-- Handle VM run request
handleVMRunRequest :: ClientConnection -> WSRequest -> IO ()
handleVMRunRequest client req = do
  vm <- takeMVar (clientVM client)
  finalState <- runVM vm
  stateJSON <- getVMStateJSON vm
  putMVar (clientVM client) vm
  sendResponse client req $ WSResponse
    { respType = "vm_run"
    , respId = reqId req
    , respSuccess = True
    , respData = stateJSON
    }

-- Handle VM load request
handleVMLoadRequest :: ClientConnection -> WSRequest -> IO ()
handleVMLoadRequest client req = do
  -- Extract program from request data
  vm <- takeMVar (clientVM client)
  -- For now, load example program
  loadProgram vm exampleProgram
  let vm' = vm { vmProgram = exampleProgram }
  putMVar (clientVM client) vm'
  stateJSON <- getVMStateJSON vm'
  sendResponse client req $ WSResponse
    { respType = "vm_load"
    , respId = reqId req
    , respSuccess = True
    , respData = stateJSON
    }

-- Handle keyboard input
handleKeyboardRequest :: ClientConnection -> WSRequest -> IO ()
handleKeyboardRequest client req = do
  -- Process keyboard input and update VM
  sendResponse client req $ WSResponse
    { respType = "keyboard"
    , respId = reqId req
    , respSuccess = True
    , respData = object []
    }

-- Handle mouse input
handleMouseRequest :: ClientConnection -> WSRequest -> IO ()
handleMouseRequest client req = do
  -- Process mouse input and update VM
  sendResponse client req $ WSResponse
    { respType = "mouse"
    , respId = reqId req
    , respSuccess = True
    , respData = object []
    }

-- Handle ping request
handlePingRequest :: ClientConnection -> WSRequest -> IO ()
handlePingRequest client req = do
  sendResponse client req $ WSResponse
    { respType = "pong"
    , respId = reqId req
    , respSuccess = True
    , respData = object []
    }

-- Handle binary message (e.g., framebuffer)
handleBinaryMessage :: ClientConnection -> BS.ByteString -> IO ()
handleBinaryMessage client msgBytes = do
  -- Handle binary data (could be framebuffer updates, etc.)
  return ()

-- Send response
sendResponse :: ClientConnection -> WSRequest -> WSResponse -> IO ()
sendResponse client req resp = do
  sendTextData (clientConn client) $ encode resp

-- Send error
sendError :: ClientConnection -> Text -> IO ()
sendError client errorMsg = do
  let resp = WSResponse
        { respType = "error"
        , respId = Nothing
        , respSuccess = False
        , respData = object ["message" .= errorMsg]
        }
  sendTextData (clientConn client) $ encode resp

