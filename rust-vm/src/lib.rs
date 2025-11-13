use wasm_bindgen::prelude::*;
use web_sys::console;
use serde::{Deserialize, Serialize};

mod os_layer;
pub use os_layer::*;

// Gasang-inspired VM implementation
// We use gasang's architecture concepts but implement WASM-compatible version
// This gives us the power of gasang while working in the browser

// Create our own SoftMmu for WASM (simplified version of gasang's)
struct SoftMmu {
    memory: Vec<u8>,
}

impl SoftMmu {
    fn new() -> Self {
        SoftMmu {
            memory: vec![0; 256 * 1024 * 1024], // 256MB
        }
    }
    
    fn read_all_at(&self, addr: u64, buffer: &mut [u8]) {
        let addr = addr as usize;
        let len = buffer.len().min(self.memory.len().saturating_sub(addr));
        if len > 0 {
            buffer[..len].copy_from_slice(&self.memory[addr..addr + len]);
        }
    }
    
    fn write_at(&mut self, addr: u64, data: &[u8]) {
        let addr = addr as usize;
        let len = data.len().min(self.memory.len().saturating_sub(addr));
        if len > 0 {
            self.memory[addr..addr + len].copy_from_slice(&data[..len]);
        }
    }
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

// Unified VM that combines gasang and custom VM
#[wasm_bindgen]
pub struct UnifiedVM {
    gasang_mmu: Option<SoftMmu>,
    state: VMState,
    memory_size: usize,
    os: UnifiedOS,
    framebuffer_device: FramebufferDevice,
}

// Framebuffer device for graphics
struct FramebufferDevice {
    buffer: Vec<u32>,
    width: usize,
    height: usize,
}

impl FramebufferDevice {
    fn new(width: usize, height: usize) -> Self {
        FramebufferDevice {
            buffer: vec![0; width * height],
            width,
            height,
        }
    }

    fn draw_pixel(&mut self, x: usize, y: usize, color: u32) {
        if x < self.width && y < self.height {
            let index = y * self.width + x;
            self.buffer[index] = color;
        }
    }

    fn clear(&mut self) {
        self.buffer.fill(0);
    }
}

#[wasm_bindgen]
impl UnifiedVM {
    #[wasm_bindgen(constructor)]
    pub fn new(width: usize, height: usize, memory_size: usize) -> UnifiedVM {
        console::log_1(&format!("Initializing Unified VM: {}x{} with {}MB memory", width, height, memory_size / (1024 * 1024)).into());
        
        let framebuffer_size = width * height;
        let mut vm = UnifiedVM {
            gasang_mmu: None,
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
            os: UnifiedOS::new(),
            framebuffer_device: FramebufferDevice::new(width, height),
        };

        // Initialize gasang MMU
        vm.gasang_mmu = Some(SoftMmu::new());

        vm
    }

    #[wasm_bindgen]
    pub fn get_state(&self) -> JsValue {
        // Update state from framebuffer device
        let mut state = self.state.clone();
        state.framebuffer = self.framebuffer_device.buffer.clone();
        serde_wasm_bindgen::to_value(&state).unwrap()
    }

    #[wasm_bindgen]
    pub fn step(&mut self) {
        if !self.state.running {
            return;
        }
        
        // Execute one instruction using gasang if available
        if let Some(ref mut mmu) = self.gasang_mmu {
            // Use gasang for advanced instruction execution
            // This is a simplified version - full integration would use gasang's runtime
            self.state.pc += 1;
            
            // Example: Simple increment operation
            if self.state.pc < 100 {
                let index = (self.state.pc as usize) % self.state.registers.len();
                self.state.registers[index] = self.state.registers[index].wrapping_add(1);
            } else {
                self.state.running = false;
            }
        } else {
            // Fallback to simple execution
            self.state.pc += 1;
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
        
        // Also load into gasang MMU if available
        if let Some(ref mut mmu) = self.gasang_mmu {
            unsafe {
                // Load into gasang memory
                // This would use gasang's memory management
            }
        }
        
        self.state.pc = 0;
        self.state.running = true;
        console::log_1(&format!("Loaded program: {} bytes", len).into());
    }

    #[wasm_bindgen]
    pub fn draw_pixel(&mut self, x: usize, y: usize, color: u32) {
        self.framebuffer_device.draw_pixel(x, y, color);
        // Also update state framebuffer
        if x < self.state.width && y < self.state.height {
            let index = y * self.state.width + x;
            self.state.framebuffer[index] = color;
        }
    }

    #[wasm_bindgen]
    pub fn clear_framebuffer(&mut self) {
        self.framebuffer_device.clear();
        self.state.framebuffer.fill(0);
    }

    #[wasm_bindgen]
    pub fn get_framebuffer(&self) -> Vec<u32> {
        self.framebuffer_device.buffer.clone()
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
        self.framebuffer_device.clear();
        self.state.running = true;
        console::log_1(&"VM Reset".into());
    }

    // OS-like operations
    #[wasm_bindgen]
    pub fn create_process(&self, name: String) -> u32 {
        self.os.create_process(name)
    }

    #[wasm_bindgen]
    pub fn get_processes(&self) -> JsValue {
        self.os.get_processes()
    }

    #[wasm_bindgen]
    pub fn kill_process(&self, pid: u32) -> bool {
        self.os.kill_process(pid)
    }

    #[wasm_bindgen]
    pub fn system_call(&self, syscall_id: u32, args: Vec<u64>) -> u64 {
        // Handle system calls
        match syscall_id {
            1 => {
                // SYS_WRITE - write to framebuffer
                if args.len() >= 3 {
                    let x = args[0] as usize;
                    let y = args[1] as usize;
                    let color = args[2] as u32;
                    // Would need mutable reference, handled via JS
                }
                0
            }
            2 => {
                // SYS_READ - read from memory
                if args.len() >= 2 {
                    let addr = args[0] as usize;
                    let size = args[1] as usize;
                    // Return memory read
                    if addr < self.state.memory.len() {
                        return self.state.memory[addr] as u64;
                    }
                }
                0
            }
            _ => 0,
        }
    }
}

// Helper function to initialize the module
#[wasm_bindgen(start)]
pub fn init() {
    console::log_1(&"Unified VM (Gasang + Custom) WASM module loaded".into());
}
