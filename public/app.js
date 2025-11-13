// Azalea - Direct Linux VM Loader
// Removes desktop UI and loads directly into Linux

class AzaleaLoader {
    constructor() {
        this.vm = null;
        this.poweredOn = false;
        this.init();
    }

    async init() {
        try {
            // Update splash screen status
            this.updateStatus('Loading v86 emulator...');
            
            // Wait for v86 library
            await this.loadV86Library();
            
            this.updateStatus('Initializing VM...');
            
            // Initialize Linux VM
            await this.initVM();
            
            this.updateStatus('Booting Linux...');
            
            // Load Linux distribution
            await this.loadLinux();
            
            this.updateStatus('Ready');
            
            // Hide splash and show VM after delay
            setTimeout(() => {
                this.hideSplash();
                this.setupControls();
            }, 500);
        } catch (error) {
            console.error('Failed to initialize VM:', error);
            this.updateStatus('Error: ' + error.message);
            // Still show the VM container even if there's an error
            setTimeout(() => {
                this.hideSplash();
            }, 2000);
        }
    }

    loadV86Library() {
        return new Promise((resolve, reject) => {
            if (typeof V86Starter !== 'undefined') {
                resolve();
                return;
            }

            const script = document.createElement('script');
            script.src = 'https://cdn.jsdelivr.net/gh/copy/v86@master/build/libv86.js';
            script.onload = resolve;
            script.onerror = reject;
            document.head.appendChild(script);
        });
    }

    waitForV86() {
        return new Promise((resolve) => {
            const check = setInterval(() => {
                if (typeof V86Starter !== 'undefined') {
                    clearInterval(check);
                    resolve();
                }
            }, 100);
        });
    }

    async initVM() {
        await this.waitForV86();
        
        const screenContainer = document.getElementById('linux-vm-screen');
        if (!screenContainer) {
            throw new Error('Linux VM screen container not found');
        }
        
        this.vm = new V86Starter({
            screen_container: screenContainer,
            memory_size: 32 * 1024 * 1024, // 32MB
            vga_memory_size: 2 * 1024 * 1024, // 2MB
            bios: {
                url: "https://cdn.jsdelivr.net/gh/copy/v86@master/bios/seabios.bin"
            },
            vga_bios: {
                url: "https://cdn.jsdelivr.net/gh/copy/v86@master/bios/vgabios.bin"
            },
            cdrom: {
                url: "https://cdn.jsdelivr.net/gh/copy/v86@master/images/linux4.iso"
            },
            autostart: true
        });

        // Wait for VM to be ready or timeout after 5 seconds
        return new Promise((resolve) => {
            let resolved = false;
            const timeout = setTimeout(() => {
                if (!resolved) {
                    resolved = true;
                    console.log('VM initialization timeout, continuing anyway...');
                    resolve();
                }
            }, 5000);
            
            this.vm.add_listener("emulator-ready", () => {
                if (!resolved) {
                    resolved = true;
                    clearTimeout(timeout);
                    resolve();
                }
            });
        });
    }

    async loadLinux() {
        // Linux image is loaded via cdrom in initVM
        // Wait a bit for it to initialize
        return new Promise((resolve) => {
            setTimeout(resolve, 1000);
        });
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

        // Power button
        powerBtn.addEventListener('click', () => {
            if (!this.poweredOn) {
                this.vm.run();
                powerBtn.innerHTML = '<i class="fas fa-power-off"></i>';
                statusDot.classList.add('running');
                statusText.textContent = 'Running';
                this.poweredOn = true;
            } else {
                this.vm.stop();
                powerBtn.innerHTML = '<i class="fas fa-power-off"></i>';
                statusDot.classList.remove('running');
                statusText.textContent = 'Stopped';
                this.poweredOn = false;
            }
        });

        // Reset button
        resetBtn.addEventListener('click', () => {
            if (this.vm) {
                this.vm.restart();
                statusText.textContent = 'Resetting...';
                statusDot.classList.add('loading');
                setTimeout(() => {
                    statusDot.classList.remove('loading');
                    statusText.textContent = 'Running';
                }, 2000);
            }
        });

        // Update status when VM starts
        if (this.vm) {
            this.vm.add_listener("screen-update", () => {
                if (!this.poweredOn && statusDot && statusText) {
                    statusDot.classList.add('running');
                    statusText.textContent = 'Running';
                    this.poweredOn = true;
                }
            });
            
            // Auto-start the VM
            setTimeout(() => {
                if (this.vm && !this.poweredOn) {
                    this.vm.run();
                    if (statusDot) statusDot.classList.add('running');
                    if (statusText) statusText.textContent = 'Running';
                    this.poweredOn = true;
                }
            }, 2000);
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
    new AzaleaLoader();
});
