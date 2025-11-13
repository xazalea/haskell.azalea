// VM Manager - Manages multiple VM backends (Haskell WebSocket, Rust WASM)
// Allows switching between or using both VMs simultaneously

class VMManager {
    constructor(containerId, options = {}) {
        this.container = document.getElementById(containerId);
        this.activeVM = null;
        this.vmType = options.vmType || 'auto'; // 'haskell', 'rust', 'auto', 'both'
        this.haskellVM = null;
        this.rustVM = null;
        this.options = options;
        
        this.init();
    }

    async init() {
        try {
            // Initialize based on selected VM type
            switch (this.vmType) {
                case 'haskell':
                    await this.initHaskellVM();
                    break;
                case 'rust':
                    await this.initRustVM();
                    break;
                case 'both':
                    await this.initHaskellVM();
                    await this.initRustVM();
                    this.activeVM = this.haskellVM; // Default to Haskell
                    break;
                case 'auto':
                default:
                    // Try Rust first (faster, client-side), fallback to Haskell
                    try {
                        await this.initRustVM();
                        this.activeVM = this.rustVM;
                    } catch (e) {
                        console.warn('Rust VM failed, falling back to Haskell:', e);
                        await this.initHaskellVM();
                        this.activeVM = this.haskellVM;
                    }
                    break;
            }

            if (!this.activeVM && this.haskellVM) {
                this.activeVM = this.haskellVM;
            } else if (!this.activeVM && this.rustVM) {
                this.activeVM = this.rustVM;
            }

            console.log('VM Manager initialized with:', this.vmType);
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

    // Unified VM interface
    getState() {
        if (!this.activeVM) return null;
        
        if (this.activeVM === this.haskellVM) {
            return this.haskellVM.vmState;
        } else if (this.activeVM === this.rustVM) {
            return this.rustVM.getState();
        }
        return null;
    }

    async step() {
        if (!this.activeVM) return;
        
        if (this.activeVM === this.haskellVM) {
            await this.haskellVM.stepVM();
        } else if (this.activeVM === this.rustVM) {
            this.rustVM.step();
        }
    }

    async run() {
        if (!this.activeVM) return;
        
        if (this.activeVM === this.haskellVM) {
            await this.haskellVM.runVM();
        } else if (this.activeVM === this.rustVM) {
            this.rustVM.run(1000);
        }
    }

    async loadProgram(program) {
        if (!this.activeVM) return;
        
        if (this.activeVM === this.haskellVM) {
            await this.haskellVM.loadProgram(program);
        } else if (this.activeVM === this.rustVM) {
            this.rustVM.loadProgram(program);
        }
    }

    reset() {
        if (!this.activeVM) return;
        
        if (this.activeVM === this.haskellVM) {
            this.haskellVM.disconnect();
            this.haskellVM.connect();
        } else if (this.activeVM === this.rustVM) {
            this.rustVM.reset();
        }
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

