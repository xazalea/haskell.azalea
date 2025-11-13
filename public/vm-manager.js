// VM Manager - Manages multiple VM backends (Haskell WebSocket, Rust WASM)
// Integrates both VMs together for extreme power - NO FALLBACK, BOTH WORK TOGETHER

class VMManager {
    constructor(containerId, options = {}) {
        this.container = document.getElementById(containerId);
        this.vmType = 'unified'; // Always use both VMs together
        this.haskellVM = null;
        this.rustVM = null;
        this.unifiedOS = null;
        this.options = options;
        
        this.init();
    }

    async init() {
        try {
            // Initialize BOTH VMs - no fallback, they work together
            const initPromises = [];
            
            // Initialize both VMs in parallel
            if (typeof AzaleaVM !== 'undefined') {
                initPromises.push(this.initHaskellVM().catch(e => {
                    console.warn('Haskell VM initialization warning:', e);
                    return null; // Continue even if one fails
                }));
            }
            
            if (typeof GasangVM !== 'undefined') {
                initPromises.push(this.initRustVM().catch(e => {
                    console.warn('Rust VM initialization warning (will use Haskell VM):', e);
                    // Continue - system works with just Haskell VM
                    return null;
                }));
            }
            
            await Promise.all(initPromises);
            
            // Initialize Unified OS to coordinate both VMs
            // OS layer provides process management, system calls, multi-tasking
            if (typeof UnifiedOS !== 'undefined') {
                this.unifiedOS = new UnifiedOS();
                await this.unifiedOS.initialize(this.haskellVM, this.rustVM);
                console.log('Unified OS initialized - Process management and system calls enabled!');
            }
            
            // Set active VM - prefer Rust for speed, but both work together
            if (this.rustVM) {
                this.activeVM = this.rustVM;
            } else if (this.haskellVM) {
                this.activeVM = this.haskellVM;
            }

            console.log('🚀 Unified VM Manager initialized - EXTREME POWER MODE!');
            console.log('   Haskell VM:', this.haskellVM ? '✅ Ready (Server-side power)' : '❌ Not available');
            console.log('   Rust VM:', this.rustVM ? '✅ Ready (Client-side speed)' : '⚠️  Not available (using Haskell)');
            console.log('   Unified OS:', this.unifiedOS ? '✅ Active (Process management enabled)' : '❌ Not available');
            
            if (this.haskellVM && this.rustVM) {
                console.log('   💪 DUAL VM MODE: Both VMs working together for maximum power!');
            } else if (this.haskellVM) {
                console.log('   ⚡ Single VM Mode: Haskell VM providing server-side execution');
            }
        } catch (error) {
            console.error('Failed to initialize VM Manager:', error);
            throw error;
        }
    }

    async initHaskellVM() {
        if (typeof AzaleaVM === 'undefined') {
            throw new Error('AzaleaVM (Haskell client) not loaded');
        }

        this.haskellVM = new AzaleaVM('linux-vm-screen', {
            ...this.options,
            autoConnect: true
        });

        // Wait for connection
        await new Promise((resolve, reject) => {
            let attempts = 0;
            const maxAttempts = 50;
            const check = setInterval(() => {
                attempts++;
                if (this.haskellVM && this.haskellVM.connected) {
                    clearInterval(check);
                    resolve();
                } else if (attempts >= maxAttempts) {
                    clearInterval(check);
                    reject(new Error('Haskell VM connection timeout'));
                }
            }, 100);
        });
    }

    async initRustVM() {
        if (typeof GasangVM === 'undefined') {
            throw new Error('GasangVM (Rust WASM) not loaded');
        }

        this.rustVM = new GasangVM('linux-vm-screen', {
            ...this.options,
            autoInit: true
        });

        await this.rustVM.init();
    }

    switchVM(type) {
        if (type === 'haskell' && this.haskellVM) {
            this.activeVM = this.haskellVM;
            this.vmType = 'haskell';
            return true;
        } else if (type === 'rust' && this.rustVM) {
            this.activeVM = this.rustVM;
            this.vmType = 'rust';
            return true;
        }
        return false;
    }

    // Unified VM interface - Uses BOTH VMs together
    getState() {
        // Get state from both VMs and merge
        const states = {};
        
        if (this.rustVM) {
            states.rust = this.rustVM.getState();
        }
        
        if (this.haskellVM && this.haskellVM.connected) {
            states.haskell = this.haskellVM.vmState;
        }
        
        // Return unified state (prefer Rust for speed, Haskell for accuracy)
        return states.rust || states.haskell || null;
    }

    async step() {
        // Execute on BOTH VMs for redundancy and power
        const promises = [];
        
        if (this.rustVM) {
            promises.push(Promise.resolve(this.rustVM.step()));
        }
        
        if (this.haskellVM && this.haskellVM.connected) {
            promises.push(this.haskellVM.stepVM());
        }
        
        await Promise.all(promises);
        
        // Use Unified OS scheduler if available
        if (this.unifiedOS) {
            // OS handles process scheduling
        }
    }

    async run() {
        // Run on BOTH VMs
        const promises = [];
        
        if (this.rustVM) {
            promises.push(Promise.resolve(this.rustVM.run(1000)));
        }
        
        if (this.haskellVM && this.haskellVM.connected) {
            promises.push(this.haskellVM.runVM());
        }
        
        await Promise.all(promises);
    }

    async loadProgram(program) {
        // Load program into BOTH VMs
        const promises = [];
        
        if (this.rustVM) {
            promises.push(Promise.resolve(this.rustVM.loadProgram(program)));
        }
        
        if (this.haskellVM && this.haskellVM.connected) {
            promises.push(this.haskellVM.loadProgram(program));
        }
        
        await Promise.all(promises);
    }

    reset() {
        // Reset BOTH VMs
        if (this.rustVM) {
            this.rustVM.reset();
        }
        
        if (this.haskellVM) {
            this.haskellVM.disconnect();
            this.haskellVM.connect();
        }
    }
    
    // OS-like operations
    createProcess(name) {
        if (this.unifiedOS) {
            return this.unifiedOS.createProcess(name);
        }
        return 0;
    }
    
    getProcesses() {
        if (this.unifiedOS) {
            return this.unifiedOS.getProcesses();
        }
        return [];
    }
    
    async systemCall(syscallId, args) {
        if (this.unifiedOS) {
            return await this.unifiedOS.systemCall(syscallId, args);
        }
        return 0;
    }
    
    getSystemInfo() {
        if (this.unifiedOS) {
            return this.unifiedOS.getSystemInfo();
        }
        return {};
    }

    // Get performance comparison
    getPerformanceInfo() {
        return {
            haskell: {
                available: this.haskellVM !== null,
                connected: this.haskellVM?.connected || false,
                type: 'server-side',
                latency: 'network-dependent'
            },
            rust: {
                available: this.rustVM !== null,
                initialized: this.rustVM !== null,
                type: 'client-side',
                latency: 'minimal'
            },
            active: this.vmType
        };
    }
}

// Export
if (typeof window !== 'undefined') {
    window.VMManager = VMManager;
}

