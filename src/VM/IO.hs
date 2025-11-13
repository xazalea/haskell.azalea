{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module VM.IO where

import Data.Word
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson
import GHC.Generics
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import qualified Data.ByteString as BS

-- I/O Device types
data IODevice = KeyboardDevice | MouseDevice | DisplayDevice | StorageDevice
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- I/O Device state
data DeviceState = DeviceState
  { deviceType :: IODevice
  , deviceId :: Word32
  , deviceBuffer :: TQueue Word8
  , deviceReady :: TVar Bool
  }

-- Keyboard input
data KeyEvent = KeyEvent
  { keyCode :: Word32
  , keyPressed :: Bool
  , keyChar :: Maybe Char
  } deriving (Generic, ToJSON, FromJSON)

-- Mouse input
data MouseEvent = MouseEvent
  { mouseX :: Int
  , mouseY :: Int
  , mouseButtons :: Word8  -- Bitmask for buttons
  , mouseWheel :: Int
  } deriving (Generic, ToJSON, FromJSON)

-- Create keyboard device
createKeyboard :: Word32 -> IO DeviceState
createKeyboard id = do
  buffer <- newTQueueIO
  ready <- newTVarIO True
  return $ DeviceState
    { deviceType = KeyboardDevice
    , deviceId = id
    , deviceBuffer = buffer
    , deviceReady = ready
    }

-- Create mouse device
createMouse :: Word32 -> IO DeviceState
createMouse id = do
  buffer <- newTQueueIO
  ready <- newTVarIO True
  return $ DeviceState
    { deviceType = MouseDevice
    , deviceId = id
    , deviceBuffer = buffer
    , deviceReady = ready
    }

-- Send key event to keyboard
sendKeyEvent :: DeviceState -> KeyEvent -> IO ()
sendKeyEvent dev event = do
  atomically $ do
    ready <- readTVar (deviceReady dev)
    when ready $ do
      -- Encode key event to buffer (simplified)
      writeTQueue (deviceBuffer dev) (fromIntegral $ keyCode event)

-- Send mouse event
sendMouseEvent :: DeviceState -> MouseEvent -> IO ()
sendMouseEvent dev event = do
  atomically $ do
    ready <- readTVar (deviceReady dev)
    when ready $ do
      -- Encode mouse event (simplified)
      writeTQueue (deviceBuffer dev) (fromIntegral $ mouseX event)
      writeTQueue (deviceBuffer dev) (fromIntegral $ mouseY event)
      writeTQueue (deviceBuffer dev) (mouseButtons event)

-- Read from device (non-blocking)
readDevice :: DeviceState -> IO (Maybe Word8)
readDevice dev = atomically $ tryReadTQueue (deviceBuffer dev)

