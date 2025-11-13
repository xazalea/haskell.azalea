{-# LANGUAGE OverloadedStrings #-}

module VM.Executor where

import VM.Core
import Data.Word
import Data.Int
import Data.Bits
import Control.Monad
import Control.Monad.IO.Class
import System.IO.Unsafe

-- Execute a single instruction
executeInstruction :: VMState -> Instruction -> IO VMState
executeInstruction vm@VMState{..} instr = case instr of
  NOP -> return vm
  
  MOV reg val -> do
    writeReg vm reg val
    return vm { pc = pc + 1 }
  
  MOVR dest src -> do
    val <- readReg vm src
    writeReg vm dest val
    return vm { pc = pc + 1 }
  
  ADD dest src1 src2 -> do
    v1 <- readReg vm src1
    v2 <- readReg vm src2
    let result = v1 + v2
    writeReg vm dest result
    -- Set flags
    let flags' = if result == 0 then flags .|. 1 else flags .&. complement 1
    return vm { pc = pc + 1, flags = flags' }
  
  SUB dest src1 src2 -> do
    v1 <- readReg vm src1
    v2 <- readReg vm src2
    let result = v1 - v2
    writeReg vm dest result
    let flags' = if result == 0 then flags .|. 1 else flags .&. complement 1
    return vm { pc = pc + 1, flags = flags' }
  
  MUL dest src1 src2 -> do
    v1 <- readReg vm src1
    v2 <- readReg vm src2
    let result = v1 * v2
    writeReg vm dest result
    return vm { pc = pc + 1 }
  
  DIV dest src1 src2 -> do
    v1 <- readReg vm src1
    v2 <- readReg vm src2
    if v2 == 0
      then return vm { running = False }  -- Division by zero
      else do
        let result = v1 `div` v2
        writeReg vm dest result
        return vm { pc = pc + 1 }
  
  CMP reg1 reg2 -> do
    v1 <- readReg vm reg1
    v2 <- readReg vm reg2
    let flags' = if v1 == v2 then flags .|. 1 else flags .&. complement 1
    return vm { pc = pc + 1, flags = flags' }
  
  JMP addr -> return vm { pc = addr }
  
  JE addr -> do
    if (flags .&. 1) /= 0  -- Zero flag set
      then return vm { pc = addr }
      else return vm { pc = pc + 1 }
  
  JNE addr -> do
    if (flags .&. 1) == 0  -- Zero flag not set
      then return vm { pc = addr }
      else return vm { pc = pc + 1 }
  
  PUSH reg -> do
    val <- readReg vm reg
    let newSp = sp - 4
    writeMem vm newSp (fromIntegral $ val .&. 0xFF)
    writeMem vm (newSp + 1) (fromIntegral $ (val `shiftR` 8) .&. 0xFF)
    writeMem vm (newSp + 2) (fromIntegral $ (val `shiftR` 16) .&. 0xFF)
    writeMem vm (newSp + 3) (fromIntegral $ (val `shiftR` 24) .&. 0xFF)
    return vm { pc = pc + 1, sp = newSp }
  
  POP reg -> do
    let addr = sp
    b0 <- readMem vm addr
    b1 <- readMem vm (addr + 1)
    b2 <- readMem vm (addr + 2)
    b3 <- readMem vm (addr + 3)
    let val = fromIntegral b0 .|. (fromIntegral b1 `shiftL` 8) .|.
              (fromIntegral b2 `shiftL` 16) .|. (fromIntegral b3 `shiftL` 24)
    writeReg vm reg val
    return vm { pc = pc + 1, sp = sp + 4 }
  
  CALL addr -> do
    -- Push return address
    let newSp = sp - 4
    writeMem vm newSp (fromIntegral $ (pc + 1) .&. 0xFF)
    writeMem vm (newSp + 1) (fromIntegral $ ((pc + 1) `shiftR` 8) .&. 0xFF)
    writeMem vm (newSp + 2) (fromIntegral $ ((pc + 1) `shiftR` 16) .&. 0xFF)
    writeMem vm (newSp + 3) (fromIntegral $ ((pc + 1) `shiftR` 24) .&. 0xFF)
    return vm { pc = addr, sp = newSp }
  
  RET -> do
    -- Pop return address
    let addr = sp
    b0 <- readMem vm addr
    b1 <- readMem vm (addr + 1)
    b2 <- readMem vm (addr + 2)
    b3 <- readMem vm (addr + 3)
    let retAddr = fromIntegral b0 .|. (fromIntegral b1 `shiftL` 8) .|.
                  (fromIntegral b2 `shiftL` 16) .|. (fromIntegral b3 `shiftL` 24)
    return vm { pc = retAddr, sp = sp + 4 }
  
  LOAD reg addr -> do
    b0 <- readMem vm addr
    b1 <- readMem vm (addr + 1)
    b2 <- readMem vm (addr + 2)
    b3 <- readMem vm (addr + 3)
    let val = fromIntegral b0 .|. (fromIntegral b1 `shiftL` 8) .|.
              (fromIntegral b2 `shiftL` 16) .|. (fromIntegral b3 `shiftL` 24)
    writeReg vm reg val
    return vm { pc = pc + 1 }
  
  STORE reg addr -> do
    val <- readReg vm reg
    writeMem vm addr (fromIntegral $ val .&. 0xFF)
    writeMem vm (addr + 1) (fromIntegral $ (val `shiftR` 8) .&. 0xFF)
    writeMem vm (addr + 2) (fromIntegral $ (val `shiftR` 16) .&. 0xFF)
    writeMem vm (addr + 3) (fromIntegral $ (val `shiftR` 24) .&. 0xFF)
    return vm { pc = pc + 1 }
  
  DRAW xReg yReg colorReg unused -> do
    x <- readReg vm xReg
    y <- readReg vm yReg
    color <- readReg vm colorReg
    writePixel vm (fromIntegral x) (fromIntegral y) color
    return vm { pc = pc + 1 }
  
  CLEAR -> do
    clearFramebuffer vm
    return vm { pc = pc + 1 }
  
  HLT -> return vm { running = False, pc = pc + 1 }

-- Execute a program (list of instructions)
executeProgram :: VMState -> [Instruction] -> IO VMState
executeProgram vm [] = return vm
executeProgram vm@VMState{..} program
  | not running = return vm
  | fromIntegral pc >= length program = return vm { running = False }
  | otherwise = do
      let instr = program !! fromIntegral pc
      vm' <- executeInstruction vm instr
      if running vm'
        then executeProgram vm' program
        else return vm'

-- Step execution (execute one instruction)
stepExecution :: VMState -> [Instruction] -> IO VMState
stepExecution vm@VMState{..} program
  | not running = return vm
  | fromIntegral pc >= length program = return vm { running = False }
  | otherwise = do
      let instr = program !! fromIntegral pc
      executeInstruction vm instr

-- Example program: Draw a simple pattern
exampleProgram :: [Instruction]
exampleProgram =
  [ CLEAR
  , MOV R0 100        -- x = 100
  , MOV R1 100        -- y = 100
  , MOV R2 0xFF0000   -- color = red
  , DRAW R0 R1 R2 R0   -- Draw pixel
  , MOV R0 200
  , MOV R1 200
  , MOV R2 0x00FF00   -- color = green
  , DRAW R0 R1 R2 R0
  , MOV R0 300
  , MOV R1 300
  , MOV R2 0x0000FF   -- color = blue
  , DRAW R0 R1 R2 R0
  , HLT
  ]

