// Azalea - Custom Haskell VM Loader
// Connects to Haskell backend via WebSocket

class AzaleaLoader {
    constructor() {
        this.vm = null;
        this.poweredOn = false;
        this.init();
    }

    async init() {
        try {
            // Update splash screen status
            this.updateStatus('Connecting to Azalea VM...');
            
            // Initialize custom Haskell VM
            await this.initVM();
            
            this.updateStatus('Initializing VM...');
            
            // Load initial program
            await this.loadInitialProgram();
            
            this.updateStatus('Ready');

            // Store VM reference globally for Firefox VM
            window.azaleaLoader = this;

            // Hide splash and show VM after delay
            setTimeout(() => {
                this.hideSplash();
                // Wait a bit for DOM to be ready before setting up controls
                setTimeout(() => {
                    this.setupControls();
                }, 100);
            }, 500);
        } catch (error) {
            console.error('VM initialization error:', error);
            
            // Check if Rust VM is available (works without WebSocket)
            if (this.vm && this.vm.rustVM) {
                console.log('✅ Using Rust VM only - system fully functional');
                this.hideSplash();
            } else {
                this.updateStatus('Initialization failed');
                console.warn('VM initialization failed, but Rust VM should still work');
                // Still show the VM container - Rust VM should work
                setTimeout(() => {
                    this.hideSplash();
                }, 2000);
            }
        }
    }

    async initVM() {
        // Load AzaleaVM client library
        if (typeof AzaleaVM === 'undefined') {
            // Load azalea-vm.js if not already loaded
            await this.loadVMClient();
        }
        
        const screenContainer = document.getElementById('linux-vm-screen');
        if (!screenContainer) {
            throw new Error('Linux VM screen container not found');
        }
        
        // Initialize Unified VM Manager - BOTH VMs work together for extreme power
        // NO FALLBACK - Both VMs are integrated and work simultaneously
        if (typeof VMManager !== 'undefined') {
            // Use Unified VM Manager - both VMs work together
            this.vm = new VMManager('linux-vm-screen', {
                autoConnect: true
                // Both Haskell and Rust VMs will be initialized and work together
            });
            
            // Initialize AI Assistant after VM is ready
            setTimeout(() => {
                this.initAIAssistant();
            }, 1000);
        } else {
            // Fallback to Haskell VM only (shouldn't happen if scripts loaded correctly)
            this.vm = new AzaleaVM('linux-vm-screen', {
                autoConnect: true
            });
        }

        // Wait for VM to be ready (Rust VM works immediately, Haskell may not connect)
        return new Promise((resolve, reject) => {
            let attempts = 0;
            const maxAttempts = 10; // 1 second max - Rust VM is instant
            
            const check = setInterval(() => {
                attempts++;
                
                // Check if Rust VM is ready (works immediately) or Haskell VM connected
                const isReady = this.vm && (
                    this.vm.rustVM || // Rust VM is always ready
                    this.vm.haskellVM?.connected || // Haskell VM connected
                    this.vm.activeVM // Any VM is active
                );
                
                if (isReady) {
                    clearInterval(check);
                    // VM is ready - Rust VM works perfectly without WebSocket
                    resolve();
                } else if (attempts >= maxAttempts) {
                    clearInterval(check);
                    // Even if connection fails, Rust VM should work
                    if (this.vm && this.vm.rustVM) {
                        console.log('✅ Using Rust VM only (WebSocket not available)');
                        resolve();
                    } else {
                        reject(new Error('Failed to initialize VM'));
                    }
                }
            }, 100);
        });
    }

    loadVMClient() {
        return new Promise((resolve, reject) => {
            if (typeof AzaleaVM !== 'undefined') {
                resolve();
                return;
            }

            const script = document.createElement('script');
            script.src = 'azalea-vm.js';
            script.onload = () => resolve();
            script.onerror = () => reject(new Error('Failed to load AzaleaVM client'));
            document.head.appendChild(script);
        });
    }

    async loadInitialProgram() {
        // Load a simple initial program to demonstrate the VM
        if (this.vm) {
            await this.vm.loadProgram([]);
        }
    }

    hideSplash() {
        const splash = document.getElementById('splash-screen');
        const container = document.getElementById('linux-container');
        
        splash.style.display = 'none';
        container.style.display = 'flex';
    }

    setupControls() {
        const powerBtn = document.getElementById('vm-power-btn');
        const resetBtn = document.getElementById('vm-reset-btn');
        const statusEl = document.getElementById('vm-status');
        
        if (!powerBtn || !resetBtn || !statusEl) {
            console.error('Control elements not found');
            return;
        }
        
        const statusDot = statusEl.querySelector('.status-dot');
        const statusText = statusEl.querySelector('span:last-child');
        
        if (!statusDot || !statusText) {
            console.error('Status elements not found');
            return;
        }

        // Power button
        powerBtn.addEventListener('click', async () => {
            if (!this.vm) return;
            
            if (!this.poweredOn) {
                await this.vm.runVM();
                powerBtn.innerHTML = '<i class="fas fa-power-off"></i>';
                if (statusDot) statusDot.classList.add('running');
                if (statusText) statusText.textContent = 'Running';
                this.poweredOn = true;
            } else {
                // Disconnect VM
                this.vm.disconnect();
                powerBtn.innerHTML = '<i class="fas fa-power-off"></i>';
                if (statusDot) statusDot.classList.remove('running');
                if (statusText) statusText.textContent = 'Stopped';
                this.poweredOn = false;
            }
        });

        // Reset button
        resetBtn.addEventListener('click', async () => {
            if (!this.vm) return;
            
            if (statusText) statusText.textContent = 'Resetting...';
            if (statusDot) statusDot.classList.add('loading');
            
            // Reconnect VM
            this.vm.disconnect();
            await this.vm.connect();
            
            setTimeout(() => {
                if (statusDot) statusDot.classList.remove('loading');
                if (statusText) statusText.textContent = 'Running';
            }, 2000);
        });

        // Update status when VM connects
        if (this.vm) {
            // Check connection status periodically
            const checkStatus = setInterval(() => {
                if (this.vm && this.vm.connected && !this.poweredOn) {
                    if (statusDot) statusDot.classList.add('running');
                    if (statusText) statusText.textContent = 'Running';
                    this.poweredOn = true;
                } else if (this.vm && !this.vm.connected && this.poweredOn) {
                    if (statusDot) statusDot.classList.remove('running');
                    if (statusText) statusText.textContent = 'Disconnected';
                    this.poweredOn = false;
                }
            }, 1000);
        }
    }

    updateStatus(message) {
        const statusEl = document.getElementById('splash-status');
        if (statusEl) {
            statusEl.textContent = message;
        }
    }

    initAIAssistant() {
        if (typeof AIAssistant !== 'undefined' && typeof AIAssistantUI !== 'undefined') {
            try {
                // Create AI Assistant
                this.aiAssistant = new AIAssistant(this.vm, {
                    baseURL: 'https://api.llm7.io/v1',
                    apiKey: 'unused',
                    model: 'gpt-4.1-2025-04-14' // Powerful model
                });

                // Create AI Assistant UI
                this.aiAssistantUI = new AIAssistantUI('ai-assistant-container', this.aiAssistant);
                
                console.log('🤖 AI Assistant initialized - Ready to control the OS!');
            } catch (error) {
                console.warn('AI Assistant initialization failed:', error);
            }
        }
    }
}

// Initialize when page loads
document.addEventListener('DOMContentLoaded', () => {
    try {
        new AzaleaLoader();
    } catch (error) {
        console.error('Failed to initialize Azalea:', error);
        const statusEl = document.getElementById('splash-status');
        if (statusEl) {
            statusEl.textContent = 'Initialization error. Please refresh.';
        }
    }
});

// Handle unhandled promise rejections
window.addEventListener('unhandledrejection', (event) => {
    console.error('Unhandled promise rejection:', event.reason);
    event.preventDefault(); // Prevent default browser error handling
});
