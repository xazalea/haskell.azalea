// Azalea Linux Desktop - Main Application
class DesktopEnvironment {
    constructor() {
        this.windows = new Map();
        this.windowCounter = 0;
        this.activeWindow = null;
        this.init();
    }

    init() {
        this.setupEventListeners();
        this.updateClock();
        setInterval(() => this.updateClock(), 1000);
    }

    setupEventListeners() {
        // Start menu
        const startButton = document.getElementById('start-button');
        const startMenu = document.getElementById('start-menu');
        
        startButton.addEventListener('click', (e) => {
            e.stopPropagation();
            startMenu.classList.toggle('show');
        });

        document.addEventListener('click', (e) => {
            if (!startMenu.contains(e.target) && !startButton.contains(e.target)) {
                startMenu.classList.remove('show');
            }
        });

        // Desktop icons
        document.querySelectorAll('.desktop-icon, .start-menu-item').forEach(icon => {
            icon.addEventListener('click', (e) => {
                const app = e.currentTarget.dataset.app;
                if (app) {
                    this.openApp(app);
                    startMenu.classList.remove('show');
                }
            });
        });
    }

    updateClock() {
        const now = new Date();
        const time = now.toLocaleTimeString('en-US', { 
            hour: '2-digit', 
            minute: '2-digit' 
        });
        document.getElementById('time').textContent = time;
    }

    openApp(appName) {
        const windowId = `window-${this.windowCounter++}`;
        let windowElement;

        switch(appName) {
            case 'terminal':
                windowElement = this.createTerminalWindow(windowId);
                break;
            case 'files':
                windowElement = this.createFileManagerWindow(windowId);
                break;
            case 'browser':
                windowElement = this.createBrowserWindow(windowId);
                break;
            case 'editor':
                windowElement = this.createEditorWindow(windowId);
                break;
            case 'settings':
                windowElement = this.createSettingsWindow(windowId);
                break;
            case 'vm':
                windowElement = this.createVMWindow(windowId);
                break;
            default:
                return;
        }

        this.windows.set(windowId, {
            id: windowId,
            element: windowElement,
            app: appName,
            title: this.getAppTitle(appName)
        });

        document.getElementById('windows-container').appendChild(windowElement);
        this.setActiveWindow(windowId);
        this.updateTaskbar();
    }

    createTerminalWindow(id) {
        const window = this.createWindow(id, 'Terminal', 800, 500);
        const content = window.querySelector('.window-content');
        content.className = 'window-content terminal-content';
        
        // Initialize xterm.js terminal
        if (typeof Terminal !== 'undefined') {
            try {
                const terminal = new Terminal({
                    cursorBlink: true,
                    theme: {
                        background: '#1e1e1e',
                        foreground: '#00ff00'
                    },
                    fontSize: 14,
                    fontFamily: 'Courier New, monospace'
                });
                
                if (typeof FitAddon !== 'undefined') {
                    const fitAddon = new FitAddon();
                    terminal.loadAddon(fitAddon);
                    terminal.open(content);
                    
                    // Fit terminal to container
                    setTimeout(() => {
                        fitAddon.fit();
                    }, 100);
                } else {
                    terminal.open(content);
                }
                
                terminal.writeln('Welcome to Azalea Linux Desktop');
                terminal.writeln('Type "help" for available commands');
                terminal.write('$ ');
                
                let currentLine = '';
                terminal.onData(data => {
                    if (data === '\r' || data === '\n') {
                        terminal.write('\r\n');
                        this.handleTerminalCommand(terminal, currentLine);
                        currentLine = '';
                        terminal.write('$ ');
                    } else if (data === '\x7f' || data === '\b') {
                        if (currentLine.length > 0) {
                            currentLine = currentLine.slice(0, -1);
                            terminal.write('\b \b');
                        }
                    } else if (data >= ' ') {
                        currentLine += data;
                        terminal.write(data);
                    }
                });
                
                // Store terminal reference
                window.currentTerminal = terminal;
            } catch (e) {
                console.error('Failed to initialize terminal:', e);
                content.innerHTML = `
                    <div style="color: #00ff00; font-family: monospace; padding: 10px;">
                        <div>Welcome to Azalea Linux Desktop</div>
                        <div>Terminal initialization error. Please refresh.</div>
                        <div>$ </div>
                    </div>
                `;
            }
        } else {
            // Fallback if xterm.js not loaded
            content.innerHTML = `
                <div style="color: #00ff00; font-family: monospace; padding: 10px; height: 100%;">
                    <div>Welcome to Azalea Linux Desktop</div>
                    <div>Loading terminal...</div>
                    <div>$ </div>
                </div>
            `;
        }
        
        return window;
    }

    handleTerminalCommand(terminal, command) {
        const cmd = command.trim().toLowerCase();
        
        if (cmd === 'help') {
            terminal.writeln('Available commands:');
            terminal.writeln('  help     - Show this help message');
            terminal.writeln('  clear    - Clear the terminal');
            terminal.writeln('  date     - Show current date and time');
            terminal.writeln('  ls       - List files');
            terminal.writeln('  pwd      - Print working directory');
            terminal.writeln('  whoami   - Show current user');
        } else if (cmd === 'clear') {
            terminal.clear();
        } else if (cmd === 'date') {
            terminal.writeln(new Date().toString());
        } else if (cmd === 'ls') {
            terminal.writeln('Desktop  Documents  Downloads  home');
        } else if (cmd === 'pwd') {
            terminal.writeln('/home/user');
        } else if (cmd === 'whoami') {
            terminal.writeln('user');
        } else if (cmd === '') {
            // Empty command, do nothing
        } else {
            terminal.writeln(`Command not found: ${command}`);
        }
    }

    createFileManagerWindow(id) {
        const window = this.createWindow(id, 'File Manager', 700, 500);
        const content = window.querySelector('.window-content');
        content.className = 'window-content';
        content.innerHTML = `
            <div class="file-manager">
                <div class="file-item">
                    <i class="fas fa-home"></i>
                    <span>Home</span>
                </div>
                <div class="file-item">
                    <i class="fas fa-folder"></i>
                    <span>Documents</span>
                </div>
                <div class="file-item">
                    <i class="fas fa-download"></i>
                    <span>Downloads</span>
                </div>
                <div class="file-item">
                    <i class="fas fa-desktop"></i>
                    <span>Desktop</span>
                </div>
                <div class="file-item">
                    <i class="fas fa-image"></i>
                    <span>Pictures</span>
                </div>
                <div class="file-item">
                    <i class="fas fa-music"></i>
                    <span>Music</span>
                </div>
                <div class="file-item">
                    <i class="fas fa-video"></i>
                    <span>Videos</span>
                </div>
            </div>
        `;
        return window;
    }

    createBrowserWindow(id) {
        const window = this.createWindow(id, 'Web Browser', 900, 600);
        const content = window.querySelector('.window-content');
        content.className = 'window-content browser-content';
        content.innerHTML = `
            <div class="browser-toolbar">
                <button><i class="fas fa-arrow-left"></i></button>
                <button><i class="fas fa-arrow-right"></i></button>
                <button><i class="fas fa-redo"></i></button>
                <input type="text" class="browser-url" value="https://azalea.haskell" placeholder="Enter URL">
                <button><i class="fas fa-home"></i></button>
            </div>
            <div class="browser-view">
                <h1>Welcome to Azalea Browser</h1>
                <p>This is a web browser running in your Linux desktop environment.</p>
                <p>You can navigate to any URL using the address bar above.</p>
            </div>
        `;
        return window;
    }

    createEditorWindow(id) {
        const window = this.createWindow(id, 'Text Editor', 800, 500);
        const content = window.querySelector('.window-content');
        content.className = 'window-content editor-content';
        content.innerHTML = `
            <div class="editor-line">// Welcome to Azalea Text Editor</div>
            <div class="editor-line"></div>
            <div class="editor-line">function hello() {</div>
            <div class="editor-line">    console.log("Hello, Azalea!");</div>
            <div class="editor-line">}</div>
            <div class="editor-line"></div>
            <div class="editor-line">hello();</div>
        `;
        content.contentEditable = true;
        return window;
    }

    createSettingsWindow(id) {
        const window = this.createWindow(id, 'Settings', 600, 500);
        const content = window.querySelector('.window-content');
        content.className = 'window-content settings-content';
        content.innerHTML = `
            <div class="settings-section">
                <h3>Appearance</h3>
                <div class="setting-item">
                    <span class="setting-label">Theme</span>
                    <select>
                        <option>Dark</option>
                        <option>Light</option>
                        <option>Auto</option>
                    </select>
                </div>
            </div>
            <div class="settings-section">
                <h3>System</h3>
                <div class="setting-item">
                    <span class="setting-label">Hostname</span>
                    <span>azalea.haskell</span>
                </div>
                <div class="setting-item">
                    <span class="setting-label">OS</span>
                    <span>Azalea Linux</span>
                </div>
            </div>
        `;
        return window;
    }

    createWindow(id, title, width, height) {
        const window = document.createElement('div');
        window.className = 'window';
        window.id = id;
        window.style.width = width + 'px';
        window.style.height = height + 'px';
        window.style.left = (Math.random() * 200 + 50) + 'px';
        window.style.top = (Math.random() * 100 + 50) + 'px';

        window.innerHTML = `
            <div class="window-header">
                <div class="window-title">${title}</div>
                <div class="window-controls">
                    <button class="window-control minimize" title="Minimize">
                        <i class="fas fa-minus"></i>
                    </button>
                    <button class="window-control maximize" title="Maximize">
                        <i class="fas fa-square"></i>
                    </button>
                    <button class="window-control close" title="Close">
                        <i class="fas fa-times"></i>
                    </button>
                </div>
            </div>
            <div class="window-content"></div>
        `;

        // Window controls
        const header = window.querySelector('.window-header');
        const closeBtn = window.querySelector('.window-control.close');
        const minimizeBtn = window.querySelector('.window-control.minimize');
        const maximizeBtn = window.querySelector('.window-control.maximize');

        // Make window draggable
        this.makeDraggable(window, header);

        // Close button
        closeBtn.addEventListener('click', () => this.closeWindow(id));

        // Minimize button
        minimizeBtn.addEventListener('click', () => {
            window.style.display = 'none';
            this.updateTaskbar();
        });

        // Maximize button
        maximizeBtn.addEventListener('click', () => {
            if (window.style.width === '100vw') {
                window.style.width = width + 'px';
                window.style.height = height + 'px';
            } else {
                window.style.width = '100vw';
                window.style.height = 'calc(100vh - 60px)';
                window.style.left = '0';
                window.style.top = '0';
            }
        });

        // Click to focus
        window.addEventListener('mousedown', () => this.setActiveWindow(id));

        return window;
    }

    makeDraggable(window, header) {
        let isDragging = false;
        let currentX, currentY, initialX, initialY;

        header.addEventListener('mousedown', (e) => {
            if (e.target.closest('.window-controls')) return;
            isDragging = true;
            initialX = e.clientX - window.offsetLeft;
            initialY = e.clientY - window.offsetTop;
            this.setActiveWindow(window.id);
        });

        document.addEventListener('mousemove', (e) => {
            if (isDragging) {
                e.preventDefault();
                currentX = e.clientX - initialX;
                currentY = e.clientY - initialY;
                window.style.left = currentX + 'px';
                window.style.top = currentY + 'px';
            }
        });

        document.addEventListener('mouseup', () => {
            isDragging = false;
        });
    }

    setActiveWindow(id) {
        if (this.activeWindow) {
            const prevWindow = this.windows.get(this.activeWindow);
            if (prevWindow) {
                prevWindow.element.classList.remove('active');
            }
        }
        this.activeWindow = id;
        const window = this.windows.get(id);
        if (window) {
            window.element.classList.add('active');
            window.element.style.zIndex = 200;
        }
        this.updateTaskbar();
    }

    closeWindow(id) {
        const window = this.windows.get(id);
        if (window) {
            window.element.remove();
            this.windows.delete(id);
            if (this.activeWindow === id) {
                this.activeWindow = null;
            }
            this.updateTaskbar();
        }
    }

    updateTaskbar() {
        const taskbarApps = document.getElementById('taskbar-apps');
        taskbarApps.innerHTML = '';

        this.windows.forEach((window, id) => {
            const app = document.createElement('div');
            app.className = 'taskbar-app';
            if (id === this.activeWindow) {
                app.classList.add('active');
            }
            app.innerHTML = `<i class="fas fa-${this.getAppIcon(window.app)}"></i> ${window.title}`;
            app.addEventListener('click', () => {
                if (window.element.style.display === 'none') {
                    window.element.style.display = 'flex';
                }
                this.setActiveWindow(id);
            });
            taskbarApps.appendChild(app);
        });
    }

    getAppTitle(app) {
        const titles = {
            terminal: 'Terminal',
            files: 'File Manager',
            browser: 'Web Browser',
            editor: 'Text Editor',
            settings: 'Settings',
            vm: 'Virtual Machine'
        };
        return titles[app] || app;
    }

    getAppIcon(app) {
        const icons = {
            terminal: 'terminal',
            files: 'folder',
            browser: 'globe',
            editor: 'code',
            settings: 'cog',
            vm: 'microchip'
        };
        return icons[app] || 'window-maximize';
    }

    createVMWindow(id) {
        const window = this.createWindow(id, 'Virtual Machine', 900, 700);
        const content = window.querySelector('.window-content');
        content.className = 'window-content vm-content';
        content.innerHTML = `
            <div class="vm-container">
                <div class="vm-controls">
                    <button id="vm-load-btn-${id}" class="vm-btn">
                        <i class="fas fa-download"></i> Load Program
                    </button>
                    <button id="vm-step-btn-${id}" class="vm-btn">
                        <i class="fas fa-step-forward"></i> Step
                    </button>
                    <button id="vm-run-btn-${id}" class="vm-btn">
                        <i class="fas fa-play"></i> Run
                    </button>
                </div>
                <div class="vm-display-container">
                    <canvas id="vm-canvas-${id}" width="800" height="600"></canvas>
                </div>
                <div class="vm-info">
                    <div class="vm-status">
                        <div class="status-item">
                            <span class="status-label">PC:</span>
                            <span id="vm-pc-${id}">0x0</span>
                        </div>
                        <div class="status-item">
                            <span class="status-label">SP:</span>
                            <span id="vm-sp-${id}">0x0</span>
                        </div>
                        <div class="status-item">
                            <span class="status-label">Flags:</span>
                            <span id="vm-flags-${id}">0x0</span>
                        </div>
                        <div class="status-item">
                            <span class="status-label">Status:</span>
                            <span id="vm-running-${id}" class="status-stopped">Stopped</span>
                        </div>
                    </div>
                    <div class="vm-registers">
                        <h4>Registers</h4>
                        <div id="registers-display-${id}" class="registers-grid"></div>
                    </div>
                </div>
            </div>
        `;
        
        // Initialize VM controller for this window
        setTimeout(() => {
            const controller = new VMController(`vm-canvas-${id}`, id);
            window.vmController = controller;
            controller.loadProgram();
            
            // Set up button handlers
            const loadBtn = document.getElementById(`vm-load-btn-${id}`);
            const stepBtn = document.getElementById(`vm-step-btn-${id}`);
            const runBtn = document.getElementById(`vm-run-btn-${id}`);
            
            if (loadBtn) loadBtn.addEventListener('click', () => controller.loadProgram());
            if (stepBtn) stepBtn.addEventListener('click', () => controller.step());
            if (runBtn) runBtn.addEventListener('click', () => controller.run());
            
            // Auto-refresh display
            setInterval(() => {
                if (window.style.display !== 'none') {
                    controller.getState();
                }
            }, 100);
        }, 100);
        
        return window;
    }
}

// Initialize desktop environment when page loads
document.addEventListener('DOMContentLoaded', () => {
    new DesktopEnvironment();
});

