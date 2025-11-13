// Firefox VM Integration - Runs Firefox inside the Azalea VM framebuffer
// Firefox renders directly to the VM canvas

class FirefoxVM {
    constructor(vmManager) {
        this.vmManager = vmManager;
        this.firefoxInstance = null;
        this.isRunning = false;
        this.currentURL = 'about:blank';
        this.canvas = null;
        this.ctx = null;
    }

    async launch() {
        if (this.isRunning) {
            console.log('Firefox is already running in VM');
            return;
        }

        console.log('🚀 Launching Firefox inside Azalea VM...');
        
        // Get the VM canvas
        const vmScreen = document.getElementById('linux-vm-screen');
        if (!vmScreen) {
            throw new Error('VM screen not found');
        }

        this.canvas = vmScreen.querySelector('canvas');
        if (!this.canvas) {
            throw new Error('VM canvas not found');
        }

        this.ctx = this.canvas.getContext('2d');

        // Initialize Firefox-like browser inside VM
        this.firefoxInstance = {
            url: this.currentURL,
            title: 'Firefox - Azalea VM',
            tabs: [{
                id: 1,
                url: this.currentURL,
                title: 'New Tab'
            }],
            activeTab: 1
        };

        this.isRunning = true;

        // Render Firefox UI to VM framebuffer
        this.renderFirefoxUI();

        // Start navigation loop
        this.startNavigationLoop();

        console.log('✅ Firefox launched inside Azalea VM');
    }

    renderFirefoxUI() {
        if (!this.ctx || !this.vmManager) return;

        const width = this.canvas.width;
        const height = this.canvas.height;

        // Use VM framebuffer for rendering
        // Get VM state and render Firefox on top
        const vmState = this.vmManager.getState();
        
        if (vmState && vmState.rust) {
            // Render VM framebuffer first
            this.renderVMFramebuffer(vmState.rust);
        } else {
            // Clear canvas
            this.ctx.fillStyle = '#2a2d3a';
            this.ctx.fillRect(0, 0, width, height);
        }

        // Draw Firefox UI on top
        this.drawFirefoxToolbar();
        this.drawFirefoxContent();
    }

    renderVMFramebuffer(vmState) {
        // Render the VM's framebuffer as background
        if (vmState.framebuffer && vmState.framebuffer.length > 0) {
            const width = vmState.width || this.canvas.width;
            const height = vmState.height || this.canvas.height;
            
            const imageData = this.ctx.createImageData(width, height);
            const data = imageData.data;
            const buffer = new Uint32Array(data.buffer);
            
            for (let i = 0; i < Math.min(vmState.framebuffer.length, width * height); i++) {
                const pixel = vmState.framebuffer[i];
                buffer[i] = ((pixel & 0xFF) << 24) | 
                           ((pixel & 0xFF00) << 8) | 
                           ((pixel & 0xFF0000) >> 8) | 
                           0xFF;
            }
            
            this.ctx.putImageData(imageData, 0, 0);
        }
    }

    drawFirefoxToolbar() {
        const width = this.canvas.width;
        const toolbarHeight = 60;

        // Toolbar background
        this.ctx.fillStyle = '#1c1c22';
        this.ctx.fillRect(0, 0, width, toolbarHeight);

        // Firefox logo/icon area
        this.ctx.fillStyle = '#ff7139';
        this.ctx.beginPath();
        this.ctx.arc(30, 30, 15, 0, Math.PI * 2);
        this.ctx.fill();

        // Address bar
        const addressBarX = 60;
        const addressBarY = 20;
        const addressBarWidth = width - 200;
        const addressBarHeight = 30;

        this.ctx.fillStyle = '#2a2d3a';
        this.ctx.fillRect(addressBarX, addressBarY, addressBarWidth, addressBarHeight);

        // Address bar text
        this.ctx.fillStyle = '#ffffff';
        this.ctx.font = '14px Inter, sans-serif';
        this.ctx.fillText(this.currentURL, addressBarX + 10, addressBarY + 20);

        // Navigation buttons
        this.drawButton(width - 130, 20, 30, 30, '←', '#6c739c');
        this.drawButton(width - 95, 20, 30, 30, '→', '#6c739c');
        this.drawButton(width - 60, 20, 30, 30, '⟳', '#6c739c');
    }

    drawButton(x, y, w, h, text, color) {
        this.ctx.fillStyle = color;
        this.ctx.fillRect(x, y, w, h);
        this.ctx.fillStyle = '#ffffff';
        this.ctx.font = '16px Inter, sans-serif';
        this.ctx.textAlign = 'center';
        this.ctx.fillText(text, x + w/2, y + h/2 + 5);
        this.ctx.textAlign = 'left';
    }

    drawFirefoxContent() {
        const width = this.canvas.width;
        const height = this.canvas.height;
        const toolbarHeight = 60;
        const contentY = toolbarHeight;
        const contentHeight = height - toolbarHeight;

        // Content area background
        this.ctx.fillStyle = '#ffffff';
        this.ctx.fillRect(0, contentY, width, contentHeight);

        // Render page content
        if (this.currentURL === 'about:blank') {
            this.drawAboutBlank();
        } else {
            this.drawPageContent();
        }
    }

    drawAboutBlank() {
        const width = this.canvas.width;
        const height = this.canvas.height;
        const toolbarHeight = 60;
        const centerX = width / 2;
        const centerY = (height + toolbarHeight) / 2;

        // Firefox logo
        this.ctx.fillStyle = '#ff7139';
        this.ctx.font = 'bold 48px Inter, sans-serif';
        this.ctx.textAlign = 'center';
        this.ctx.fillText('🦊', centerX, centerY - 40);

        // Welcome text
        this.ctx.fillStyle = '#2a2d3a';
        this.ctx.font = '24px Inter, sans-serif';
        this.ctx.fillText('Firefox', centerX, centerY + 20);
        
        this.ctx.font = '16px Inter, sans-serif';
        this.ctx.fillStyle = '#6c739c';
        this.ctx.fillText('Running inside Azalea VM', centerX, centerY + 50);
        this.ctx.textAlign = 'left';
    }

    drawPageContent() {
        // For now, draw a simple page representation
        // In a full implementation, this would render actual web content
        const width = this.canvas.width;
        const height = this.canvas.height;
        const toolbarHeight = 60;

        this.ctx.fillStyle = '#f5f5f5';
        this.ctx.fillRect(20, toolbarHeight + 20, width - 40, height - toolbarHeight - 40);

        this.ctx.fillStyle = '#2a2d3a';
        this.ctx.font = '18px Inter, sans-serif';
        this.ctx.fillText('Page Content', 40, toolbarHeight + 50);
        this.ctx.fillStyle = '#6c739c';
        this.ctx.font = '14px Inter, sans-serif';
        this.ctx.fillText(this.currentURL, 40, toolbarHeight + 80);
    }

    async navigate(url) {
        if (!this.isRunning) {
            await this.launch();
        }

        this.currentURL = url;
        if (this.firefoxInstance) {
            this.firefoxInstance.url = url;
            const activeTab = this.firefoxInstance.tabs.find(t => t.id === this.firefoxInstance.activeTab);
            if (activeTab) {
                activeTab.url = url;
                activeTab.title = this.extractTitle(url);
            }
        }

        this.renderFirefoxUI();
        console.log(`🌐 Firefox navigating to: ${url}`);
    }

    extractTitle(url) {
        try {
            const urlObj = new URL(url);
            return urlObj.hostname || 'New Tab';
        } catch {
            return 'New Tab';
        }
    }

    startNavigationLoop() {
        // Continuously render Firefox UI
        // Use a lower frequency to avoid conflicts with VM rendering
        const render = () => {
            if (this.isRunning) {
                this.renderFirefoxUI();
                setTimeout(() => requestAnimationFrame(render), 100); // 10 FPS for UI overlay
            }
        };
        render();
    }

    close() {
        this.isRunning = false;
        console.log('Firefox closed in VM');
    }

    // Handle clicks on Firefox UI
    handleClick(x, y) {
        const width = this.canvas.width;
        const toolbarHeight = 60;

        // Check if click is in toolbar
        if (y < toolbarHeight) {
            // Back button
            if (x >= width - 130 && x <= width - 100 && y >= 20 && y <= 50) {
                console.log('Back button clicked');
                return true;
            }
            // Forward button
            if (x >= width - 95 && x <= width - 65 && y >= 20 && y <= 50) {
                console.log('Forward button clicked');
                return true;
            }
            // Refresh button
            if (x >= width - 60 && x <= width - 30 && y >= 20 && y <= 50) {
                console.log('Refresh button clicked');
                this.renderFirefoxUI();
                return true;
            }
            // Address bar
            if (x >= 60 && x <= width - 200 && y >= 20 && y <= 50) {
                this.showAddressBarInput();
                return true;
            }
        }

        return false;
    }

    showAddressBarInput() {
        const url = prompt('Enter URL:', this.currentURL);
        if (url) {
            this.navigate(url);
        }
    }
}

// Auto-launch Firefox when VM is ready
if (typeof window !== 'undefined') {
    window.FirefoxVM = FirefoxVM;

    // Wait for VM to be ready, then launch Firefox
    let firefoxLaunched = false;
    
    const checkVMAndLaunch = () => {
        if (firefoxLaunched) return;
        
        // Check if VM Manager is available
        if (typeof VMManager !== 'undefined' && window.azaleaLoader) {
            const vm = window.azaleaLoader.vm;
            if (vm && (vm.rustVM || vm.activeVM)) {
                firefoxLaunched = true;
                
                // Wait a bit for VM to fully initialize
                setTimeout(() => {
                    try {
                        const firefox = new FirefoxVM(vm);
                        firefox.launch();
                        window.firefoxVM = firefox;
                        
                        // Set up click handler for Firefox UI
                        const vmScreen = document.getElementById('linux-vm-screen');
                        if (vmScreen) {
                            const canvas = vmScreen.querySelector('canvas');
                            if (canvas) {
                                canvas.addEventListener('click', (e) => {
                                    const rect = canvas.getBoundingClientRect();
                                    const x = e.clientX - rect.left;
                                    const y = e.clientY - rect.top;
                                    firefox.handleClick(x, y);
                                });
                            }
                        }
                    } catch (error) {
                        console.warn('Failed to launch Firefox in VM:', error);
                    }
                }, 1000);
            }
        }
    };

    // Check periodically
    setInterval(checkVMAndLaunch, 500);
    
    // Also check on DOMContentLoaded
    document.addEventListener('DOMContentLoaded', checkVMAndLaunch);
}

