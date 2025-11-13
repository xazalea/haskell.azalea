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
            console.error('1. Network connectivity issues');
            console.error('2. CDN blocking or CORS restrictions');
            console.error('3. Browser compatibility issues');
            console.error('Please check the browser console for more details.');
            
            // Still show the VM container even if there's an error
            setTimeout(() => {
                this.hideSplash();
            }, 3000);
        }
    }

    loadV86Library() {
        return new Promise((resolve, reject) => {
            if (typeof V86Starter !== 'undefined') {
                resolve();
                return;
            }

            // Try multiple CDN sources for reliability
            // Note: v86 library must be loaded from a CDN that supports CORS
            const cdnSources = [
                'https://cdn.jsdelivr.net/gh/copy/v86@master/build/libv86.js',
                'https://unpkg.com/v86@latest/build/libv86.js'
            ];

            let currentIndex = 0;

            const tryLoad = () => {
                if (currentIndex >= cdnSources.length) {
                    reject(new Error('Failed to load v86 library from all CDN sources. Please check your internet connection.'));
                    return;
                }

                const script = document.createElement('script');
                script.src = cdnSources[currentIndex];
                
                script.onload = () => {
                    // Wait a bit for V86Starter to be available
                    setTimeout(() => {
                        if (typeof V86Starter !== 'undefined') {
                            resolve();
                        } else {
                            // Try next source if V86Starter still not available
                            currentIndex++;
                            tryLoad();
                        }
                    }, 200);
                };
                
                script.onerror = (event) => {
                    const errorDetails = event?.target?.src || cdnSources[currentIndex];
                    console.warn(`Failed to load v86 from ${errorDetails}, trying next source...`);
                    currentIndex++;
                    tryLoad();
                };
                
                document.head.appendChild(script);
            };

            tryLoad();
        });
    }

    waitForV86() {
        return new Promise((resolve, reject) => {
            let attempts = 0;
            const maxAttempts = 50; // 5 seconds max
            
            const check = setInterval(() => {
                attempts++;
                if (typeof V86Starter !== 'undefined') {
                    clearInterval(check);
                    resolve();
                } else if (attempts >= maxAttempts) {
                    clearInterval(check);
                    reject(new Error('v86 library failed to load'));
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
        
        // Use alternative CDN URLs for BIOS and images
        const biosBase = 'https://cdn.jsdelivr.net/gh/copy/v86@master/bios';
        const imagesBase = 'https://cdn.jsdelivr.net/gh/copy/v86@master/images';
        
        this.vm = new V86Starter({
            screen_container: screenContainer,
            memory_size: 32 * 1024 * 1024, // 32MB
            vga_memory_size: 2 * 1024 * 1024, // 2MB
            bios: {
                url: `${biosBase}/seabios.bin`
            },
            vga_bios: {
                url: `${biosBase}/vgabios.bin`
            },
            cdrom: {
                url: `${imagesBase}/linux4.iso`
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
        
        if (!statusDot || !statusText) {
            console.error('Status elements not found');
            return;
        }

        // Power button
        powerBtn.addEventListener('click', () => {
            if (!this.vm) return;
            
            if (!this.poweredOn) {
                this.vm.run();
                powerBtn.innerHTML = '<i class="fas fa-power-off"></i>';
                if (statusDot) statusDot.classList.add('running');
                if (statusText) statusText.textContent = 'Running';
                this.poweredOn = true;
            } else {
                this.vm.stop();
                powerBtn.innerHTML = '<i class="fas fa-power-off"></i>';
                if (statusDot) statusDot.classList.remove('running');
                if (statusText) statusText.textContent = 'Stopped';
                this.poweredOn = false;
            }
        });

        // Reset button
        resetBtn.addEventListener('click', () => {
            if (!this.vm) return;
            
            this.vm.restart();
            if (statusText) statusText.textContent = 'Resetting...';
            if (statusDot) statusDot.classList.add('loading');
            setTimeout(() => {
                if (statusDot) statusDot.classList.remove('loading');
                if (statusText) statusText.textContent = 'Running';
            }, 2000);
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
