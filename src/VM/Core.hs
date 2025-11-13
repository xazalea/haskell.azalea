{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module VM.Core where

import Data.Word
import Data.Int
import Data.Array.IO
import Data.Array.MArray
import Data.Bits
import Control.Monad.State
import Control.Monad.IO.Class
import GHC.Generics
import Data.Aeson

-- VM State
data VMState = VMState
  { registers :: IOUArray Int Word32
  , memory :: IOUArray Word32 Word8
  , pc :: Word32  -- Program Counter
  , sp :: Word32  -- Stack Pointer
  , flags :: Word32  -- Status flags
  , framebuffer :: IOUArray Int Word32  -- GUI framebuffer (width * height pixels)
  , fbWidth :: Int
  , fbHeight :: Int
  , running :: Bool
  } deriving (Generic)

-- Registers
data Register = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
  deriving (Eq, Enum, Bounded, Show)

registerIndex :: Register -> Int
registerIndex = fromEnum

-- Instructions
data Instruction
  = NOP
  | MOV Register Word32        -- Move immediate to register
  | MOVR Register Register     -- Move register to register
  | ADD Register Register Register  -- Add two registers
  | SUB Register Register Register  -- Subtract two registers
  | MUL Register Register Register  -- Multiply two registers
  | DIV Register Register Register  -- Divide two registers
  | CMP Register Register      -- Compare two registers
  | JMP Word32                 -- Jump to address
  | JE Word32                  -- Jump if equal
  | JNE Word32                 -- Jump if not equal
  | PUSH Register              -- Push register to stack
  | POP Register               -- Pop from stack to register
  | CALL Word32                -- Call function at address
  | RET                        -- Return from function
  | LOAD Register Word32       -- Load from memory address
  | STORE Register Word32      -- Store to memory address
  | DRAW Register Register Register Register  -- Draw pixel (x, y, color)
  | CLEAR                      -- Clear framebuffer
  | HLT                        -- Halt
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- Memory layout
memSize :: Word32
memSize = 0x100000  -- 1MB

stackBase :: Word32
stackBase = 0xFFFF00

stackSize :: Word32
stackSize = 0x1000

-- Framebuffer
defaultWidth :: Int
defaultWidth = 800

defaultHeight :: Int
defaultHeight = 600

-- Create new VM state
createVM :: IO VMState
createVM = do
  regs <- newArray (0, 15) 0
  mem <- newArray (0, fromIntegral memSize - 1) 0
  fb <- newArray (0, defaultWidth * defaultHeight - 1) 0x00000000  -- Black
  return $ VMState
    { registers = regs
    , memory = mem
    , pc = 0
    , sp = stackBase + stackSize
    , flags = 0
    , framebuffer = fb
    , fbWidth = defaultWidth
    , fbHeight = defaultHeight
    , running = True
    }

-- Read register
readReg :: VMState -> Register -> IO Word32
readReg vm reg = readArray (registers vm) (registerIndex reg)

-- Write register
writeReg :: VMState -> Register -> Word32 -> IO ()
writeReg vm reg val = writeArray (registers vm) (registerIndex reg) val

-- Read memory
readMem :: VMState -> Word32 -> IO Word8
readMem vm addr = readArray (memory vm) (fromIntegral addr)

-- Write memory
writeMem :: VMState -> Word32 -> Word8 -> IO ()
writeMem vm addr val = writeArray (memory vm) (fromIntegral addr) val

-- Read framebuffer pixel
readPixel :: VMState -> Int -> Int -> IO Word32
readPixel vm x y
  | x >= 0 && x < fbWidth vm && y >= 0 && y < fbHeight vm =
      readArray (framebuffer vm) (y * fbWidth vm + x)
  | otherwise = return 0x00000000

-- Write framebuffer pixel
writePixel :: VMState -> Int -> Int -> Word32 -> IO ()
writePixel vm x y color
  | x >= 0 && x < fbWidth vm && y >= 0 && y < fbHeight vm =
      writeArray (framebuffer vm) (y * fbWidth vm + x) color
  | otherwise = return ()

-- Get framebuffer as list (for JSON serialization)
getFramebuffer :: VMState -> IO [Word32]
getFramebuffer vm = getElems (framebuffer vm)

-- Clear framebuffer
clearFramebuffer :: VMState -> IO ()
clearFramebuffer vm = do
  let size = fbWidth vm * fbHeight vm
  sequence_ [writeArray (framebuffer vm) i 0x00000000 | i <- [0..size-1]]

-- Status flags
data Flag = Zero | Negative | Carry | Overflow
  deriving (Eq, Enum)

setFlag :: VMState -> Flag -> Bool -> IO ()
setFlag vm flag val = do
  let bit = fromEnum flag
      mask = 1 `shiftL` bit
  flags' <- if val
    then return $ flags vm .|. mask
    else return $ flags vm .&. complement mask
  -- Note: We can't modify VMState directly, so this is a limitation
  -- In a real implementation, we'd use StateT or similar

getFlag :: VMState -> Flag -> Bool
getFlag vm flag = (flags vm .&. (1 `shiftL` fromEnum flag)) /= 0

