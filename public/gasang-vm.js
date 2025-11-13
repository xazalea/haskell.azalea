// Gasang VM - Rust/WASM VM Client
// Provides a JavaScript interface to the Rust VM compiled to WebAssembly

class GasangVM {
    constructor(containerId, options = {}) {
        this.container = document.getElementById(containerId);
        this.wasmModule = null;
        this.vm = null;
        this.canvas = null;
        this.ctx = null;
        this.options = {
            width: options.width || 1920,
            height: options.height || 1080,
            memorySize: options.memorySize || 256 * 1024 * 1024, // 256MB
            wasmPath: options.wasmPath || 'gasang_vm_bg.wasm',
            autoInit: options.autoInit !== false
        };
        
        if (this.options.autoInit) {
            this.init();
        }
    }

    async init() {
        try {
            await this.loadWASM();
            this.setupCanvas();
            this.vm = new this.wasmModule.GasangVM(
                this.options.width,
                this.options.height,
                this.options.memorySize
            );
            console.log('Gasang VM initialized successfully');
        } catch (error) {
            console.error('Failed to initialize Gasang VM:', error);
            throw error;
        }
    }

    async loadWASM() {
        // Try to load the WASM module from wasm-pack output
        try {
            // wasm-pack generates a module that exports init() and the classes
            const wasmModule = await import('./pkg/gasang_vm.js');
            
            // Initialize the WASM module (loads the .wasm file)
            if (wasmModule.default) {
                await wasmModule.default();
            } else if (wasmModule.init) {
                await wasmModule.init();
            }
            
            this.wasmModule = wasmModule;
            console.log('Gasang VM WASM module loaded successfully');
            return;
        } catch (e) {
            console.warn('Failed to load WASM from pkg directory:', e);
            console.warn('This is okay - Haskell VM will handle execution');
            // Don't throw - allow system to work with just Haskell VM
            // The unified OS will coordinate both when available
            throw new Error('WASM module not available - will use Haskell VM');
        }
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
        this.canvas.style.imageRendering = 'auto';
        this.container.appendChild(this.canvas);
        this.ctx = this.canvas.getContext('2d');
    }

    getState() {
        if (!this.vm) return null;
        return this.vm.get_state();
    }

    step() {
        if (!this.vm) return;
        this.vm.step();
        this.updateDisplay();
    }

    run(steps = 1000) {
        if (!this.vm) return;
        this.vm.run(steps);
        this.updateDisplay();
    }

    loadProgram(program) {
        if (!this.vm) return;
        const programArray = new Uint8Array(program);
        this.vm.load_program(programArray);
        this.updateDisplay();
    }

    drawPixel(x, y, color) {
        if (!this.vm) return;
        this.vm.draw_pixel(x, y, color);
        this.updateDisplay();
    }

    clearFramebuffer() {
        if (!this.vm) return;
        this.vm.clear_framebuffer();
        this.updateDisplay();
    }

    updateDisplay() {
        if (!this.vm || !this.ctx) return;

        const state = this.vm.get_state();
        const framebuffer = state.framebuffer;
        const width = state.width;
        const height = state.height;

        // Create ImageData from framebuffer
        const imageData = this.ctx.createImageData(width, height);
        const data = imageData.data;

        for (let i = 0; i < framebuffer.length && i < width * height; i++) {
            const pixel = framebuffer[i];
            const offset = i * 4;
            
            // Extract RGBA from 32-bit pixel value
            data[offset] = (pixel >> 16) & 0xFF;     // R
            data[offset + 1] = (pixel >> 8) & 0xFF;  // G
            data[offset + 2] = pixel & 0xFF;         // B
            data[offset + 3] = 255;                   // A
        }

        this.ctx.putImageData(imageData, 0, 0);
    }

    reset() {
        if (!this.vm) return;
        this.vm.reset();
        this.updateDisplay();
    }

    getRegister(index) {
        if (!this.vm) return 0;
        return this.vm.get_register(index);
    }

    setRegister(index, value) {
        if (!this.vm) return;
        this.vm.set_register(index, value);
    }

    getMemory(address, size) {
        if (!this.vm) return new Uint8Array(0);
        return this.vm.get_memory(address, size);
    }

    setMemory(address, data) {
        if (!this.vm) return;
        const dataArray = new Uint8Array(data);
        this.vm.set_memory(address, dataArray);
    }
}

// Export for use
if (typeof window !== 'undefined') {
    window.GasangVM = GasangVM;
}

