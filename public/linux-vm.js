// Linux VM using v86 emulator
// This runs a real Linux distribution in the browser

class LinuxVM {
    constructor(canvasId, options = {}) {
        this.canvasId = canvasId;
        this.v86 = null;
        this.initialized = false;
        this.options = {
            memory_size: 32 * 1024 * 1024, // 32MB
            vga_memory_size: 2 * 1024 * 1024, // 2MB
            screen_container: document.getElementById(canvasId),
            bios: {
                url: "https://cdn.jsdelivr.net/gh/copy/v86@master/bios/seabios.bin"
            },
            vga_bios: {
                url: "https://cdn.jsdelivr.net/gh/copy/v86@master/bios/vgabios.bin"
            },
            ...options
        };
    }

    async init() {
        if (this.initialized) return;

        // Load v86 library
        if (typeof V86Starter === 'undefined') {
            await this.loadV86Library();
        }

        // Wait for library to be available
        await this.waitForV86();

        // Initialize v86 emulator
        this.v86 = new V86Starter(this.options);
        
        this.setupEventHandlers();
        this.initialized = true;
    }

    loadV86Library() {
        return new Promise((resolve, reject) => {
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

    setupEventHandlers() {
        if (!this.v86) return;

        this.v86.add_listener("screen-update", () => {
            // Screen updated
        });

        this.v86.add_listener("serial0-output-char", (char) => {
            // Serial output
            if (this.onOutput) {
                this.onOutput(char);
            }
        });
    }

    // Load a Linux image
    async loadLinuxImage(imageUrl) {
        if (!this.v86) {
            await this.init();
        }

        return new Promise((resolve, reject) => {
            this.v86.load_file(imageUrl, (error, data) => {
                if (error) {
                    reject(error);
                } else {
                    this.v86.boot_from(data);
                    resolve();
                }
            });
        });
    }

    // Send keyboard input
    sendKey(key) {
        if (this.v86) {
            this.v86.keyboard_send_scancodes(key);
        }
    }

    // Send text
    sendText(text) {
        if (this.v86) {
            for (let i = 0; i < text.length; i++) {
                const char = text[i];
                const scancode = this.charToScancode(char);
                if (scancode) {
                    this.v86.keyboard_send_scancodes(scancode);
                }
            }
        }
    }

    charToScancode(char) {
        // Basic scancode mapping
        const map = {
            'a': 0x1e, 'b': 0x30, 'c': 0x2e, 'd': 0x20, 'e': 0x12,
            'f': 0x21, 'g': 0x22, 'h': 0x23, 'i': 0x17, 'j': 0x24,
            'k': 0x25, 'l': 0x26, 'm': 0x32, 'n': 0x31, 'o': 0x18,
            'p': 0x19, 'q': 0x10, 'r': 0x13, 's': 0x1f, 't': 0x14,
            'u': 0x16, 'v': 0x2f, 'w': 0x11, 'x': 0x2d, 'y': 0x15,
            'z': 0x2c,
            ' ': 0x39, // Space
            '\n': 0x1c, // Enter
            '\r': 0x1c
        };
        return map[char.toLowerCase()] ? [map[char.toLowerCase()]] : null;
    }

    // Power on/off
    powerOn() {
        if (this.v86) {
            this.v86.run();
        }
    }

    powerOff() {
        if (this.v86) {
            this.v86.stop();
        }
    }

    // Reset
    reset() {
        if (this.v86) {
            this.v86.restart();
        }
    }
}

// Pre-configured Linux distributions
const LinuxDistros = {
    // Tiny Core Linux - very small, fast boot
    tinycore: {
        image: "https://cdn.jsdelivr.net/gh/copy/v86@master/images/linux4.iso",
        name: "Tiny Core Linux",
        description: "Minimal Linux distribution"
    },
    // You can add more distros here
    // Note: v86 requires pre-built disk images
};

// Export for use in other files
if (typeof window !== 'undefined') {
    window.LinuxVM = LinuxVM;
    window.LinuxDistros = LinuxDistros;
}

