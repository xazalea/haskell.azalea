{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module VM.Network where

import Data.Word
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Data.Aeson
import GHC.Generics
import Control.Concurrent
import Control.Concurrent.STM

-- Network interface for VM
data NetworkInterface = NetworkInterface
  { netId :: Word32
  , netIP :: Word32  -- IPv4 address as Word32
  , netMAC :: [Word8]  -- 6-byte MAC address
  , netConnected :: Bool
  , netPackets :: TQueue NetworkPacket
  } deriving (Generic)

-- Network packet
data NetworkPacket = NetworkPacket
  { packetSrc :: Word32
  , packetDst :: Word32
  , packetData :: [Word8]
  , packetType :: PacketType
  } deriving (Generic, ToJSON, FromJSON)

data PacketType = TCP | UDP | ICMP | ARP
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- Create network interface
createNetworkInterface :: Word32 -> Word32 -> IO NetworkInterface
createNetworkInterface id ip = do
  mac <- generateMAC
  queue <- newTQueueIO
  return $ NetworkInterface
    { netId = id
    , netIP = ip
    , netMAC = mac
    , netConnected = False
    , netPackets = queue
    }

-- Generate random MAC address
generateMAC :: IO [Word8]
generateMAC = return [0x02, 0x00, 0x00, 0x00, 0x00, 0x01]  -- Simplified

-- Send packet
sendPacket :: NetworkInterface -> NetworkPacket -> IO Bool
sendPacket net packet = do
  if netConnected net
    then atomically $ writeTQueue (netPackets net) packet
    else return False
  return (netConnected net)

-- Receive packet (non-blocking)
receivePacket :: NetworkInterface -> IO (Maybe NetworkPacket)
receivePacket net = atomically $ tryReadTQueue (netPackets net)

-- Connect network interface
connectNetwork :: NetworkInterface -> IO NetworkInterface
connectNetwork net = return net { netConnected = True }

-- Disconnect network interface
disconnectNetwork :: NetworkInterface -> IO NetworkInterface
disconnectNetwork net = return net { netConnected = False }

