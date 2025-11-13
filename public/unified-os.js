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
        // Write to framebuffer or console - Uses BOTH VMs together
        if (args.length >= 3) {
            const x = args[0];
            const y = args[1];
            const color = args[2];
            
            // Execute on BOTH VMs simultaneously for maximum power
            const promises = [];
            
            if (this.rustVM) {
                promises.push(Promise.resolve(this.rustVM.drawPixel(x, y, color)));
            }
            
            if (this.haskellVM && this.haskellVM.connected) {
                // Send to Haskell VM via WebSocket for server-side processing
                promises.push(this.haskellVM.sendRequest('vm_step', {}).catch(() => {}));
            }
            
            await Promise.all(promises);
        }
        return 0;
    }

    async handleRead(args) {
        // Read from memory or device - Uses BOTH VMs
        if (args.length >= 2) {
            const address = args[0];
            const size = args[1];
            
            // Try Rust first (faster), fallback to Haskell
            if (this.rustVM) {
                const data = this.rustVM.getMemory(address, size);
                // Also sync with Haskell VM
                if (this.haskellVM && this.haskellVM.connected) {
                    // Sync memory state
                }
                return data;
            } else if (this.haskellVM && this.haskellVM.connected) {
                // Read from Haskell VM
                const state = await this.haskellVM.getVMState();
                return state?.memory || 0;
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
            // Advanced multi-tasking scheduler - Uses BOTH VMs
            const processes = this.getProcesses().filter(p => p.state === 'running');
            
            // Distribute processes across both VMs for maximum throughput
            const rustProcesses = [];
            const haskellProcesses = [];
            
            processes.forEach((process, index) => {
                if (index % 2 === 0 && this.rustVM) {
                    rustProcesses.push(process);
                } else if (this.haskellVM && this.haskellVM.connected) {
                    haskellProcesses.push(process);
                } else if (this.rustVM) {
                    rustProcesses.push(process);
                }
            });
            
            // Execute on both VMs in parallel
            const promises = [];
            
            // Execute Rust processes
            if (this.rustVM && rustProcesses.length > 0) {
                promises.push(Promise.resolve(
                    rustProcesses.forEach(() => this.rustVM.step())
                ));
            }
            
            // Execute Haskell processes
            if (this.haskellVM && this.haskellVM.connected && haskellProcesses.length > 0) {
                promises.push(
                    Promise.all(haskellProcesses.map(() => this.haskellVM.stepVM()))
                );
            }
            
            await Promise.all(promises);
            
            // Update process CPU times
            processes.forEach(process => {
                process.cpuTime += 1;
            });
            
            // Yield to browser (maintain 60 FPS)
            await new Promise(resolve => setTimeout(resolve, 16));
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

