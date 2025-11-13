// VM Display - Renders the VM framebuffer to canvas
class VMDisplay {
    constructor(canvasId) {
        const canvas = document.getElementById(canvasId);
        if (!canvas) {
            console.error(`Canvas with id ${canvasId} not found`);
            return;
        }
        this.canvas = canvas;
        this.ctx = this.canvas.getContext('2d');
        this.width = 800;
        this.height = 600;
        this.canvas.width = this.width;
        this.canvas.height = this.height;
        this.imageData = this.ctx.createImageData(this.width, this.height);
    }

    // Render framebuffer to canvas
    render(framebuffer) {
        const data = this.imageData.data;
        
        for (let i = 0; i < framebuffer.length; i++) {
            const pixel = framebuffer[i];
            const idx = i * 4;
            
            // Convert ARGB to RGBA
            data[idx] = (pixel >> 16) & 0xFF;     // Red
            data[idx + 1] = (pixel >> 8) & 0xFF;  // Green
            data[idx + 2] = pixel & 0xFF;        // Blue
            data[idx + 3] = 255;                  // Alpha
        }
        
        this.ctx.putImageData(this.imageData, 0, 0);
    }

    // Clear canvas
    clear() {
        this.ctx.fillStyle = '#000000';
        this.ctx.fillRect(0, 0, this.width, this.height);
    }
}

// VM Controller - Manages VM state and API calls
class VMController {
    constructor(canvasId, windowId) {
        this.canvasId = canvasId;
        this.windowId = windowId;
        this.display = new VMDisplay(canvasId);
        this.state = null;
        this.running = false;
    }

    async loadProgram() {
        try {
            const response = await fetch('/api/vm/load', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' }
            });
            const data = await response.json();
            if (data.success) {
                this.updateState(data.state);
            }
        } catch (error) {
            console.error('Failed to load program:', error);
        }
    }

    async getState() {
        try {
            const response = await fetch('/api/vm/state');
            const data = await response.json();
            if (data.success) {
                this.updateState(data.state);
            }
        } catch (error) {
            console.error('Failed to get VM state:', error);
        }
    }

    async step() {
        try {
            const response = await fetch('/api/vm/step', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' }
            });
            const data = await response.json();
            if (data.success) {
                this.updateState(data.state);
            }
        } catch (error) {
            console.error('Failed to step VM:', error);
        }
    }

    async run() {
        try {
            const response = await fetch('/api/vm/run', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' }
            });
            const data = await response.json();
            if (data.success) {
                this.updateState(data.state);
            }
        } catch (error) {
            console.error('Failed to run VM:', error);
        }
    }

    updateState(state) {
        this.state = state;
        
        // Update display
        if (state.framebuffer && Array.isArray(state.framebuffer)) {
            this.display.render(state.framebuffer);
        }
        
        // Update registers display
        this.updateRegisters(state.registers || []);
        
        // Update status
        this.updateStatus(state);
    }

    updateRegisters(registers) {
        const container = document.getElementById(`registers-display-${this.windowId}`);
        if (!container) return;
        
        container.innerHTML = registers.map((val, idx) => 
            `<div class="register-item">
                <span class="register-name">R${idx}</span>
                <span class="register-value">0x${val.toString(16).padStart(8, '0')}</span>
            </div>`
        ).join('');
    }

    updateStatus(state) {
        const pcEl = document.getElementById(`vm-pc-${this.windowId}`);
        const spEl = document.getElementById(`vm-sp-${this.windowId}`);
        const flagsEl = document.getElementById(`vm-flags-${this.windowId}`);
        const runningEl = document.getElementById(`vm-running-${this.windowId}`);
        
        if (pcEl) pcEl.textContent = `0x${state.pc.toString(16)}`;
        if (spEl) spEl.textContent = `0x${state.sp.toString(16)}`;
        if (flagsEl) flagsEl.textContent = `0x${state.flags.toString(16)}`;
        if (runningEl) {
            runningEl.textContent = state.running ? 'Running' : 'Stopped';
            runningEl.className = state.running ? 'status-running' : 'status-stopped';
        }
    }
}

