// Azalea VM - Custom Haskell VM Client
// Connects to Haskell backend via WebSocket

class AzaleaVM {
    constructor(containerId, options = {}) {
        this.container = document.getElementById(containerId);
        this.ws = null;
        this.connected = false;
        this.vmState = null;
        this.canvas = null;
        this.ctx = null;
        this.requestId = 0;
        this.pendingRequests = new Map();
        this.frameRequestId = null;
        this.lastFrameTime = 0;
        this.fps = 0;
        this.frameCount = 0;
        this.options = {
            wsUrl: options.wsUrl || this.getWebSocketUrl(),
            width: options.width || 1920,  // Full HD default
            height: options.height || 1080,
            autoConnect: options.autoConnect !== false,
            targetFPS: options.targetFPS || 60
        };
        
        if (this.options.autoConnect) {
            this.init();
        }
    }

    getWebSocketUrl() {
        const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
        const host = window.location.host;
        return `${protocol}//${host}`;
    }

    async init() {
        this.setupCanvas();
        await this.connect();
        this.setupEventHandlers();
        this.startRenderLoop();
    }

    setupCanvas() {
        if (!this.container) {
            throw new Error('Container element not found');
        }

        this.canvas = document.createElement('canvas');
        this.canvas.width = this.options.width;
        this.canvas.height = this.options.height;
        this.canvas.style.width = '100%';
        this.canvas.style.height = '100%';
        this.canvas.style.imageRendering = 'pixelated';
        this.container.appendChild(this.canvas);
        this.ctx = this.canvas.getContext('2d');
    }

    connect() {
        return new Promise((resolve, reject) => {
            try {
                this.ws = new WebSocket(this.options.wsUrl);
                
                this.ws.onopen = () => {
                    this.connected = true;
                    console.log('Connected to Azalea VM');
                    resolve();
                };

                this.ws.onmessage = (event) => {
                    this.handleMessage(JSON.parse(event.data));
                };

                this.ws.onerror = (error) => {
                    console.error('WebSocket error:', error);
                    reject(error);
                };

                this.ws.onclose = () => {
                    this.connected = false;
                    console.log('Disconnected from Azalea VM');
                    // Attempt to reconnect after 3 seconds
                    setTimeout(() => {
                        if (!this.connected) {
                            this.connect().catch(console.error);
                        }
                    }, 3000);
                };
            } catch (error) {
                reject(error);
            }
        });
    }

    handleMessage(msg) {
        if (msg.respId && this.pendingRequests.has(msg.respId)) {
            const { resolve } = this.pendingRequests.get(msg.respId);
            this.pendingRequests.delete(msg.respId);
            resolve(msg);
        }

        // Handle different message types
        switch (msg.respType) {
            case 'connected':
                console.log('VM connected:', msg.respData);
                break;
            case 'vm_state':
                this.updateVMState(msg.respData);
                break;
            case 'framebuffer_update':
                this.updateFramebuffer(msg.respData);
                break;
            case 'error':
                console.error('VM error:', msg.respData);
                break;
        }
    }

    sendRequest(type, data = {}) {
        return new Promise((resolve, reject) => {
            if (!this.connected) {
                reject(new Error('Not connected to VM'));
                return;
            }

            const id = `req_${++this.requestId}_${Date.now()}`;
            const request = {
                reqType: type,
                reqId: id,
                reqData: data
            };

            this.pendingRequests.set(id, { resolve, reject });
            
            // Set timeout for request
            setTimeout(() => {
                if (this.pendingRequests.has(id)) {
                    this.pendingRequests.delete(id);
                    reject(new Error('Request timeout'));
                }
            }, 10000);

            this.ws.send(JSON.stringify(request));
        });
    }

    async getVMState() {
        try {
            const response = await this.sendRequest('vm_state');
            return response.respData;
        } catch (error) {
            console.error('Failed to get VM state:', error);
            return null;
        }
    }

    async stepVM() {
        try {
            const response = await this.sendRequest('vm_step');
            return response.respData;
        } catch (error) {
            console.error('Failed to step VM:', error);
            return null;
        }
    }

    async runVM() {
        try {
            const response = await this.sendRequest('vm_run');
            return response.respData;
        } catch (error) {
            console.error('Failed to run VM:', error);
            return null;
        }
    }

    async loadProgram(program) {
        try {
            const response = await this.sendRequest('vm_load', { program });
            return response.respData;
        } catch (error) {
            console.error('Failed to load program:', error);
            return null;
        }
    }

    updateVMState(state) {
        this.vmState = state;
        if (state.framebuffer) {
            this.updateFramebuffer(state);
        }
    }

    updateFramebuffer(state) {
        if (!this.ctx || !state.framebuffer) return;

        const width = state.width || this.options.width;
        const height = state.height || this.options.height;
        const framebuffer = state.framebuffer;

        // Ensure canvas size matches framebuffer
        if (this.canvas.width !== width || this.canvas.height !== height) {
            this.canvas.width = width;
            this.canvas.height = height;
        }

        // Use high-performance rendering with requestAnimationFrame
        if (this.frameRequestId) {
            cancelAnimationFrame(this.frameRequestId);
        }

        this.frameRequestId = requestAnimationFrame(() => {
            // Create ImageData from framebuffer with optimized conversion
            const imageData = this.ctx.createImageData(width, height);
            const data = imageData.data;
            const length = Math.min(framebuffer.length, width * height);

            // Optimized pixel conversion loop
            for (let i = 0; i < length; i++) {
                const pixel = framebuffer[i];
                const offset = i * 4;
                
                // Extract RGBA from 32-bit pixel value (ARGB format)
                data[offset] = (pixel >> 16) & 0xFF;     // R
                data[offset + 1] = (pixel >> 8) & 0xFF;  // G
                data[offset + 2] = pixel & 0xFF;         // B
                data[offset + 3] = 255;                   // A (fully opaque)
            }

            // Use high-quality rendering
            this.ctx.imageSmoothingEnabled = false; // Pixel-perfect for VM
            this.ctx.imageSmoothingQuality = 'high';
            
            // Clear and draw with perfect quality
            this.ctx.clearRect(0, 0, width, height);
            this.ctx.putImageData(imageData, 0, 0);
        });
    }

    setupEventHandlers() {
        if (!this.canvas) return;

        // Keyboard input
        this.canvas.addEventListener('keydown', (e) => {
            this.sendKeyboardEvent(e, true);
        });

        this.canvas.addEventListener('keyup', (e) => {
            this.sendKeyboardEvent(e, false);
        });

        // Mouse input
        this.canvas.addEventListener('mousedown', (e) => {
            this.sendMouseEvent(e, true);
        });

        this.canvas.addEventListener('mouseup', (e) => {
            this.sendMouseEvent(e, false);
        });

        this.canvas.addEventListener('mousemove', (e) => {
            this.sendMouseEvent(e, false);
        });

        // Make canvas focusable for keyboard events
        this.canvas.setAttribute('tabindex', '0');
    }

    sendKeyboardEvent(event, pressed) {
        if (!this.connected) return;

        const keyEvent = {
            keyCode: event.keyCode || 0,
            key: event.key,
            pressed: pressed
        };

        this.sendRequest('keyboard', keyEvent).catch(console.error);
    }

    sendMouseEvent(event, pressed) {
        if (!this.connected) return;

        const rect = this.canvas.getBoundingClientRect();
        const x = Math.floor((event.clientX - rect.left) * (this.canvas.width / rect.width));
        const y = Math.floor((event.clientY - rect.top) * (this.canvas.height / rect.height));

        const mouseEvent = {
            x: x,
            y: y,
            buttons: event.buttons || 0,
            button: event.button || 0,
            pressed: pressed
        };

        this.sendRequest('mouse', mouseEvent).catch(console.error);
    }

    startRenderLoop() {
        const render = async (timestamp) => {
            // Calculate FPS
            if (this.lastFrameTime > 0) {
                const delta = timestamp - this.lastFrameTime;
                this.frameCount++;
                if (this.frameCount >= 60) {
                    this.fps = Math.round(1000 / (delta / this.frameCount));
                    this.frameCount = 0;
                }
            }
            this.lastFrameTime = timestamp;

            if (this.connected) {
                // Request VM state update at target FPS
                const targetFrameTime = 1000 / this.options.targetFPS;
                const elapsed = timestamp - (this.lastFrameTime || timestamp);
                
                if (elapsed >= targetFrameTime) {
                    await this.getVMState();
                }
            }
            requestAnimationFrame(render);
        };
        requestAnimationFrame(render);
    }

    disconnect() {
        if (this.ws) {
            this.ws.close();
            this.ws = null;
        }
        this.connected = false;
    }
}

// Export for use
if (typeof window !== 'undefined') {
    window.AzaleaVM = AzaleaVM;
}

