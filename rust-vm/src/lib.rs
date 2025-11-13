use wasm_bindgen::prelude::*;
use web_sys::console;
use serde::{Deserialize, Serialize};

// Import gasang VM types and functions
// Note: This is a placeholder - you'll need to adapt based on gasang's actual API
#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}

// VM State
#[derive(Serialize, Deserialize, Clone)]
pub struct VMState {
    pub registers: Vec<u32>,
    pub memory: Vec<u8>,
    pub pc: u32,
    pub sp: u32,
    pub flags: u32,
    pub framebuffer: Vec<u32>,
    pub width: usize,
    pub height: usize,
    pub running: bool,
}

// Gasang VM wrapper
#[wasm_bindgen]
pub struct GasangVM {
    state: VMState,
    memory_size: usize,
}

#[wasm_bindgen]
impl GasangVM {
    #[wasm_bindgen(constructor)]
    pub fn new(width: usize, height: usize, memory_size: usize) -> GasangVM {
        console::log_1(&format!("Initializing Gasang VM: {}x{} with {}MB memory", width, height, memory_size / (1024 * 1024)).into());
        
        let framebuffer_size = width * height;
        GasangVM {
            state: VMState {
                registers: vec![0; 16],
                memory: vec![0; memory_size],
                pc: 0,
                sp: 0xFFFFFF00,
                flags: 0,
                framebuffer: vec![0; framebuffer_size],
                width,
                height,
                running: true,
            },
            memory_size,
        }
    }

    #[wasm_bindgen]
    pub fn get_state(&self) -> JsValue {
        serde_wasm_bindgen::to_value(&self.state).unwrap()
    }

    #[wasm_bindgen]
    pub fn step(&mut self) {
        if !self.state.running {
            return;
        }
        
        // Execute one instruction
        // This is a placeholder - integrate with actual gasang VM execution
        self.state.pc += 1;
        
        // Example: Simple increment operation
        if self.state.pc < 100 {
            let index = (self.state.pc as usize) % self.state.registers.len();
            self.state.registers[index] = self.state.registers[index].wrapping_add(1);
        } else {
            self.state.running = false;
        }
    }

    #[wasm_bindgen]
    pub fn run(&mut self, steps: usize) {
        for _ in 0..steps {
            if !self.state.running {
                break;
            }
            self.step();
        }
    }

    #[wasm_bindgen]
    pub fn load_program(&mut self, program: &[u8]) {
        // Load program into memory
        let len = program.len().min(self.state.memory.len());
        self.state.memory[..len].copy_from_slice(&program[..len]);
        self.state.pc = 0;
        self.state.running = true;
        console::log_1(&format!("Loaded program: {} bytes", len).into());
    }

    #[wasm_bindgen]
    pub fn draw_pixel(&mut self, x: usize, y: usize, color: u32) {
        if x < self.state.width && y < self.state.height {
            let index = y * self.state.width + x;
            self.state.framebuffer[index] = color;
        }
    }

    #[wasm_bindgen]
    pub fn clear_framebuffer(&mut self) {
        self.state.framebuffer.fill(0);
    }

    #[wasm_bindgen]
    pub fn get_framebuffer(&self) -> Vec<u32> {
        self.state.framebuffer.clone()
    }

    #[wasm_bindgen]
    pub fn get_memory(&self, address: usize, size: usize) -> Vec<u8> {
        let end = (address + size).min(self.state.memory.len());
        self.state.memory[address..end].to_vec()
    }

    #[wasm_bindgen]
    pub fn set_memory(&mut self, address: usize, data: &[u8]) {
        let end = (address + data.len()).min(self.state.memory.len());
        self.state.memory[address..end].copy_from_slice(&data[..(end - address)]);
    }

    #[wasm_bindgen]
    pub fn get_register(&self, index: usize) -> u32 {
        if index < self.state.registers.len() {
            self.state.registers[index]
        } else {
            0
        }
    }

    #[wasm_bindgen]
    pub fn set_register(&mut self, index: usize, value: u32) {
        if index < self.state.registers.len() {
            self.state.registers[index] = value;
        }
    }

    #[wasm_bindgen]
    pub fn reset(&mut self) {
        self.state.pc = 0;
        self.state.sp = 0xFFFFFF00;
        self.state.flags = 0;
        self.state.registers.fill(0);
        self.state.memory.fill(0);
        self.state.framebuffer.fill(0);
        self.state.running = true;
        console::log_1(&"VM Reset".into());
    }
}

// Helper function to initialize the module
#[wasm_bindgen(start)]
pub fn init() {
    console::log_1(&"Gasang VM WASM module loaded".into());
}

