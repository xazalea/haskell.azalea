// OS-like layer that coordinates both Haskell and Rust VMs
// Provides unified interface for process management, system calls, etc.

use wasm_bindgen::prelude::*;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Process {
    pub pid: u32,
    pub name: String,
    pub state: ProcessState,
    pub memory_usage: usize,
    pub cpu_time: u64,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub enum ProcessState {
    Running,
    Waiting,
    Stopped,
    Zombie,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct SystemCall {
    pub id: u32,
    pub args: Vec<u64>,
    pub result: Option<u64>,
}

// Unified OS layer
#[wasm_bindgen]
pub struct UnifiedOS {
    processes: Arc<Mutex<HashMap<u32, Process>>>,
    next_pid: Arc<Mutex<u32>>,
    system_calls: Arc<Mutex<Vec<SystemCall>>>,
}

#[wasm_bindgen]
impl UnifiedOS {
    #[wasm_bindgen(constructor)]
    pub fn new() -> UnifiedOS {
        UnifiedOS {
            processes: Arc::new(Mutex::new(HashMap::new())),
            next_pid: Arc::new(Mutex::new(1)),
            system_calls: Arc::new(Mutex::new(Vec::new())),
        }
    }

    #[wasm_bindgen]
    pub fn create_process(&self, name: String) -> u32 {
        let mut next_pid = self.next_pid.lock().unwrap();
        let pid = *next_pid;
        *next_pid += 1;

        let process = Process {
            pid,
            name,
            state: ProcessState::Running,
            memory_usage: 0,
            cpu_time: 0,
        };

        self.processes.lock().unwrap().insert(pid, process);
        pid
    }

    #[wasm_bindgen]
    pub fn get_processes(&self) -> JsValue {
        let processes: Vec<Process> = self.processes.lock().unwrap().values().cloned().collect();
        serde_wasm_bindgen::to_value(&processes).unwrap()
    }

    #[wasm_bindgen]
    pub fn kill_process(&self, pid: u32) -> bool {
        let mut processes = self.processes.lock().unwrap();
        if let Some(process) = processes.get_mut(&pid) {
            process.state = ProcessState::Stopped;
            processes.remove(&pid);
            true
        } else {
            false
        }
    }

    #[wasm_bindgen]
    pub fn register_system_call(&self, syscall: JsValue) {
        if let Ok(syscall) = serde_wasm_bindgen::from_value::<SystemCall>(syscall) {
            self.system_calls.lock().unwrap().push(syscall);
        }
    }

    #[wasm_bindgen]
    pub fn get_system_calls(&self) -> JsValue {
        let syscalls = self.system_calls.lock().unwrap().clone();
        serde_wasm_bindgen::to_value(&syscalls).unwrap()
    }
}

