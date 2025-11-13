# Azalea Linux Desktop

A full-featured web-based Linux desktop environment with a real virtual machine, built with Haskell and deployed on Vercel.

## Features

- 🖥️ **Linux Desktop Environment**: Complete desktop UI with windows, taskbar, and start menu
- 🖲️ **Real Virtual Machine**: Full VM implementation with CPU, registers, memory, and framebuffer
- 🎨 **GUI Rendering**: Canvas-based framebuffer display for VM graphics
- 💻 **Terminal Emulator**: Full-featured terminal with xterm.js integration
- 📁 **File Manager**: Browse and manage files in a familiar interface
- 🌐 **Web Browser**: Built-in browser for web navigation
- ✏️ **Text Editor**: Code editor for editing files
- ⚙️ **Settings**: System configuration panel

## Tech Stack

- **Backend**: Haskell (using WAI/Warp)
- **VM Core**: Custom VM implementation with CPU, memory, registers, and instruction set
- **Frontend**: Vanilla JavaScript, HTML5, CSS3, Canvas API
- **Terminal**: xterm.js
- **Deployment**: Vercel (using vercel-hs)

## Project Structure

```
azalea.haskell/
├── api/
│   └── index.ts          # Vercel serverless function wrapper
├── src/
│   ├── Main.hs           # Haskell backend API
│   └── VM/
│       ├── Core.hs       # VM core (CPU, registers, memory, framebuffer)
│       ├── Executor.hs   # Instruction execution engine
│       ├── FileSystem.hs # Virtual file system
│       └── VM.hs         # VM instance management
├── public/
│   ├── index.html        # Main desktop UI
│   ├── styles.css        # Desktop styling
│   ├── app.js            # Desktop environment logic
│   └── vm-display.js     # VM display and controller
├── azaela-haskell.cabal  # Haskell package configuration
├── stack.yaml            # Stack build configuration
├── vercel.json           # Vercel deployment configuration
└── package.json          # Node.js dependencies
```

## Local Development

### Prerequisites

- [Stack](https://docs.haskellstack.org/) (Haskell build tool)
- [Node.js](https://nodejs.org/) (for Vercel CLI)
- [Vercel CLI](https://vercel.com/cli) (optional, for local testing)

### Setup

1. Install dependencies:
```bash
stack build
npm install
```

2. Run locally:
```bash
# Run Haskell server directly
stack exec azaela-haskell

# Or use Vercel dev server
vercel dev
```

3. Open your browser to `http://localhost:3000`

## Deployment to Vercel

1. Install Vercel CLI (if not already installed):
```bash
npm i -g vercel
```

2. Deploy:
```bash
vercel
```

3. Follow the prompts to configure your deployment.

## Usage

Once deployed, you can:

- **Open Applications**: Click desktop icons or use the start menu
- **Run Virtual Machine**: Open the VM app to interact with the virtual machine
  - Load programs into the VM
  - Step through instructions one at a time
  - Run programs to completion
  - View registers, memory, and framebuffer output
- **Use Terminal**: Open the terminal app and run commands
- **Manage Files**: Browse your file system with the file manager
- **Edit Code**: Use the text editor for code editing
- **Browse Web**: Navigate the web with the built-in browser

## VM Instruction Set

The VM supports the following instructions:

- **Arithmetic**: `ADD`, `SUB`, `MUL`, `DIV`
- **Data Movement**: `MOV`, `MOVR`, `LOAD`, `STORE`
- **Control Flow**: `JMP`, `JE`, `JNE`, `CALL`, `RET`
- **Stack Operations**: `PUSH`, `POP`
- **Comparison**: `CMP`
- **Graphics**: `DRAW`, `CLEAR`
- **System**: `NOP`, `HLT`

## VM Features

- **16 Registers** (R0-R15): 32-bit general purpose registers
- **1MB Memory**: Addressable memory space
- **Stack**: Stack-based function calls and local variables
- **Framebuffer**: 800x600 pixel display for GUI rendering
- **Status Flags**: Zero, Negative, Carry, Overflow flags

## Available Terminal Commands

- `help` - Show available commands
- `clear` - Clear the terminal
- `date` - Show current date and time
- `ls` - List files
- `pwd` - Print working directory
- `whoami` - Show current user

## VM Architecture

The VM is implemented as a complete virtual machine with:

- **CPU**: Instruction execution engine with program counter and status flags
- **Memory**: 1MB addressable memory space
- **Registers**: 16 32-bit general purpose registers
- **Stack**: Stack pointer and stack-based operations
- **Framebuffer**: 800x600 pixel display buffer for GUI rendering
- **File System**: Virtual file system for file operations

The VM executes programs instruction by instruction, updating registers, memory, and the framebuffer. The framebuffer is rendered to a canvas element in the browser, providing real-time visual feedback.

## Notes

This project provides a real virtual machine implementation in Haskell, inspired by [CoolVM](https://github.com/MikeHaskel/CoolVM). The VM runs on the server and communicates with the web interface via REST API. While it provides actual VM capabilities, it runs on Vercel's serverless platform which has execution time limits. For production VM workloads, consider using IaaS providers like AWS, GCP, or Azure.

## License

MIT

