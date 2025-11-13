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

            // Hide splash and show VM after delay
            setTimeout(() => {
                this.hideSplash();
                // Wait a bit for DOM to be ready before setting up controls
                setTimeout(() => {
                    this.setupControls();
                }, 100);
            }, 500);
        } catch (error) {
            console.error('Failed to initialize VM:', error);
            const errorMsg = error?.message || error?.toString() || 'Unknown error';
            this.updateStatus('Error: ' + errorMsg);
            
            // Show helpful message in console
            console.error('VM initialization failed. Possible causes:');
            console.error('1. WebSocket connection failed');
            console.error('2. Haskell backend not running');
            console.error('3. Network connectivity issues');
            console.error('Please check the browser console for more details.');
            
            // Still show the VM container even if there's an error
            setTimeout(() => {
                this.hideSplash();
            }, 3000);
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
        
        // Initialize VM Manager - Supports both Haskell and Rust VMs
        // Automatically selects best VM based on availability and device capabilities
        if (typeof VMManager !== 'undefined') {
            // Use VM Manager for multi-VM support
            this.vm = new VMManager('linux-vm-screen', {
                vmType: 'auto', // Auto-select: tries Rust first, falls back to Haskell
                autoConnect: true
            });
        } else {
            // Fallback to Haskell VM only
            this.vm = new AzaleaVM('linux-vm-screen', {
                autoConnect: true
            });
        }

        // Wait for connection
        return new Promise((resolve, reject) => {
            let attempts = 0;
            const maxAttempts = 50; // 5 seconds max
            
            const check = setInterval(() => {
                attempts++;
                if (this.vm && this.vm.connected) {
                    clearInterval(check);
                    resolve();
                } else if (attempts >= maxAttempts) {
                    clearInterval(check);
                    reject(new Error('Failed to connect to VM'));
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
