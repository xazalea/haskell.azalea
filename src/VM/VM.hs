{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module VM.VM where

import VM.Core
import VM.Executor (executeProgram, stepExecution, exampleProgram)
import VM.FileSystem
import Data.Word
import Data.Aeson
import GHC.Generics
import Control.Concurrent.MVar
import Data.IORef
import Data.List

-- VM instance with state
data VMInstance = VMInstance
  { vmState :: IORef VMState
  , vmProgram :: [Instruction]
  , vmFileSystem :: IORef VFS
  }

-- Create new VM instance
createVMInstance :: IO VMInstance
createVMInstance = do
  state <- createVM
  stateRef <- newIORef state
  vfs <- createVFS
  vfsRef <- newIORef vfs
  return $ VMInstance
    { vmState = stateRef
    , vmProgram = []
    , vmFileSystem = vfsRef
    }

-- Load program into VM
loadProgram :: VMInstance -> [Instruction] -> IO ()
loadProgram vm prog = do
  state <- readIORef (vmState vm)
  -- Store program in memory (simplified - in real implementation)
  return ()

-- Run VM (execute all instructions)
runVM :: VMInstance -> IO VMState
runVM vm = do
  state <- readIORef (vmState vm)
  finalState <- executeProgram state (vmProgram vm)
  writeIORef (vmState vm) finalState
  return finalState

-- Step VM (execute one instruction)
stepVM :: VMInstance -> IO VMState
stepVM vm = do
  state <- readIORef (vmState vm)
  newState <- stepExecution state (vmProgram vm)
  writeIORef (vmState vm) newState
  return newState

-- Get VM state as JSON
getVMStateJSON :: VMInstance -> IO Value
getVMStateJSON vm = do
  state <- readIORef (vmState vm)
  regs <- mapM (readReg state) [R0 .. R15]
  fb <- getFramebuffer state
  return $ object
    [ "registers" .= regs
    , "pc" .= pc state
    , "sp" .= sp state
    , "flags" .= flags state
    , "running" .= running state
    , "framebuffer" .= fb
    , "width" .= fbWidth state
    , "height" .= fbHeight state
    ]



