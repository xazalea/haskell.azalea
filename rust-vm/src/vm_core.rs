// VM Core - Gasang-inspired implementation
// Provides powerful VM capabilities compatible with WASM

use wasm_bindgen::prelude::*;
use serde::{Deserialize, Serialize};

// Instruction types inspired by gasang
#[derive(Serialize, Deserialize, Clone, Debug)]
pub enum VMInstruction {
    Nop,
    Mov { dst: u8, src: u8 },
    MovImm { dst: u8, imm: u32 },
    Add { dst: u8, src1: u8, src2: u8 },
    Sub { dst: u8, src1: u8, src2: u8 },
    Mul { dst: u8, src1: u8, src2: u8 },
    Div { dst: u8, src1: u8, src2: u8 },
    And { dst: u8, src1: u8, src2: u8 },
    Or { dst: u8, src1: u8, src2: u8 },
    Xor { dst: u8, src1: u8, src2: u8 },
    Shl { dst: u8, src: u8, shift: u8 },
    Shr { dst: u8, src: u8, shift: u8 },
    Cmp { src1: u8, src2: u8 },
    Jmp { addr: u32 },
    Je { addr: u32 },
    Jne { addr: u32 },
    Call { addr: u32 },
    Ret,
    Load { reg: u8, addr: u32 },
    Store { reg: u8, addr: u32 },
    Push { reg: u8 },
    Pop { reg: u8 },
    Syscall { id: u32 },
    Hlt,
}

// Register file
pub struct RegisterFile {
    registers: [u32; 16],
    pc: u32,
    sp: u32,
    flags: u32,
}

impl RegisterFile {
    pub fn new() -> Self {
        RegisterFile {
            registers: [0; 16],
            pc: 0,
            sp: 0xFFFFFF00,
            flags: 0,
        }
    }

    pub fn get(&self, reg: u8) -> u32 {
        if (reg as usize) < self.registers.len() {
            self.registers[reg as usize]
        } else {
            0
        }
    }

    pub fn set(&mut self, reg: u8, value: u32) {
        if (reg as usize) < self.registers.len() {
            self.registers[reg as usize] = value;
        }
    }

    pub fn get_pc(&self) -> u32 {
        self.pc
    }

    pub fn set_pc(&mut self, pc: u32) {
        self.pc = pc;
    }

    pub fn get_sp(&self) -> u32 {
        self.sp
    }

    pub fn set_sp(&mut self, sp: u32) {
        self.sp = sp;
    }

    pub fn get_flags(&self) -> u32 {
        self.flags
    }

    pub fn set_flags(&mut self, flags: u32) {
        self.flags = flags;
    }

    pub fn get_all(&self) -> Vec<u32> {
        self.registers.to_vec()
    }
}

