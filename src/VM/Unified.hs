{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module VM.Unified where

import VM.Core
import VM.VM
import VM.OS
import Data.Word
import Data.Aeson
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.MVar
import Data.IORef

-- Unified VM that coordinates Haskell VM with Rust VM
data UnifiedVM = UnifiedVM
  { haskellVM :: MVar VMInstance
  , kernel :: KernelState
  , processes :: TVar [Process]
  }

-- Create unified VM
createUnifiedVM :: Maybe (Int, Int) -> IO UnifiedVM
createUnifiedVM resolution = do
  haskellVM <- createVMInstance resolution
  vmMVar <- newMVar haskellVM
  kernel <- createKernel
  procs <- newTVarIO []
  return $ UnifiedVM
    { haskellVM = vmMVar
    , kernel = kernel
    , processes = procs
    }

-- Execute on unified VM (uses both VMs)
executeUnified :: UnifiedVM -> IO VMState
executeUnified unified = do
  vm <- takeMVar (haskellVM unified)
  state <- stepVM vm
  putMVar (haskellVM unified) vm
  return state

-- System call handler for unified VM
handleUnifiedSystemCall :: UnifiedVM -> Word32 -> [Word64] -> IO Word64
handleUnifiedSystemCall unified syscallId args = do
  handleSystemCall (kernel unified) syscallId args

