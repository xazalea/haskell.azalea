// Unified OS - Coordinates both Haskell and Rust VMs
// Provides OS-like features: process management, system calls, multi-tasking

class UnifiedOS {
    constructor() {
        this.haskellVM = null;
        this.rustVM = null;
        this.processes = new Map();
        this.nextPID = 1;
        this.systemCalls = [];
        this.schedulerRunning = false;
    }

    async initialize(haskellVM, rustVM) {
        this.haskellVM = haskellVM;
        this.rustVM = rustVM;
        
        // Start OS scheduler
        this.startScheduler();
        
        console.log('Unified OS initialized with dual VM support');
    }

    // Process Management
    createProcess(name, vmType = 'auto') {
        const pid = this.nextPID++;
        const process = {
            pid,
            name,
            vmType,
            state: 'running',
            memoryUsage: 0,
            cpuTime: 0,
            startTime: Date.now()
        };
        
        this.processes.set(pid, process);
        return pid;
    }

    getProcesses() {
        return Array.from(this.processes.values());
    }

    killProcess(pid) {
        const process = this.processes.get(pid);
        if (process) {
            process.state = 'stopped';
            this.processes.delete(pid);
            return true;
        }
        return false;
    }

    // System Calls - Unified interface
    async systemCall(syscallId, args = []) {
        this.systemCalls.push({
            id: syscallId,
            args,
            timestamp: Date.now()
        });

        // Route to appropriate VM based on system call
        switch (syscallId) {
            case 1: // SYS_WRITE
                return this.handleWrite(args);
            case 2: // SYS_READ
                return this.handleRead(args);
            case 3: // SYS_OPEN
                return this.handleOpen(args);
            case 4: // SYS_CLOSE
                return this.handleClose(args);
            case 5: // SYS_FORK
                return this.handleFork();
            case 6: // SYS_EXEC
                return this.handleExec(args);
            default:
                return 0;
        }
    }

    async handleWrite(args) {
        // Write to framebuffer or console
        if (args.length >= 3) {
            const x = args[0];
            const y = args[1];
            const color = args[2];
            
            // Use both VMs for redundancy/performance
            if (this.rustVM) {
                this.rustVM.drawPixel(x, y, color);
            }
            if (this.haskellVM && this.haskellVM.connected) {
                // Would send to Haskell VM via WebSocket
            }
        }
        return 0;
    }

    async handleRead(args) {
        // Read from memory or device
        if (args.length >= 2) {
            const address = args[0];
            const size = args[1];
            
            if (this.rustVM) {
                return this.rustVM.getMemory(address, size);
            }
        }
        return 0;
    }

    async handleOpen(args) {
        // Open file or device
        return this.createProcess(`file_${args[0]}`);
    }

    async handleClose(args) {
        // Close file descriptor
        return this.killProcess(args[0]);
    }

    async handleFork() {
        // Fork process - create new process
        return this.createProcess('forked_process');
    }

    async handleExec(args) {
        // Execute program
        if (args.length > 0 && this.rustVM) {
            const program = args[0]; // Program data
            this.rustVM.loadProgram(program);
        }
        return 0;
    }

    // Task Scheduler
    startScheduler() {
        if (this.schedulerRunning) return;
        
        this.schedulerRunning = true;
        this.schedule();
    }

    async schedule() {
        while (this.schedulerRunning) {
            // Round-robin scheduling
            const processes = this.getProcesses().filter(p => p.state === 'running');
            
            for (const process of processes) {
                // Execute one step for each process
                if (process.vmType === 'rust' && this.rustVM) {
                    this.rustVM.step();
                } else if (process.vmType === 'haskell' && this.haskellVM) {
                    await this.haskellVM.stepVM();
                } else {
                    // Auto-select best VM
                    if (this.rustVM) {
                        this.rustVM.step();
                    } else if (this.haskellVM) {
                        await this.haskellVM.stepVM();
                    }
                }
                
                process.cpuTime += 1;
            }
            
            // Yield to browser
            await new Promise(resolve => setTimeout(resolve, 16)); // ~60 FPS
        }
    }

    stopScheduler() {
        this.schedulerRunning = false;
    }

    // Memory Management
    allocateMemory(size) {
        // Allocate memory across both VMs
        return {
            haskell: this.haskellVM ? size / 2 : 0,
            rust: this.rustVM ? size / 2 : size
        };
    }

    // Get system info
    getSystemInfo() {
        return {
            processes: this.processes.size,
            systemCalls: this.systemCalls.length,
            haskellVM: {
                available: this.haskellVM !== null,
                connected: this.haskellVM?.connected || false
            },
            rustVM: {
                available: this.rustVM !== null,
                initialized: this.rustVM !== null
            },
            scheduler: {
                running: this.schedulerRunning
            }
        };
    }
}

// Export
if (typeof window !== 'undefined') {
    window.UnifiedOS = UnifiedOS;
}

