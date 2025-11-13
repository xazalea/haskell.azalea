#!/bin/bash
set -e

echo "🔨 Building Rust VM (gasang) to WebAssembly..."

# Check if Rust is installed
if ! command -v rustc &> /dev/null; then
    echo "⚠️  Rust not found. Installing Rust..."
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
    source $HOME/.cargo/env
fi

# Check if wasm-pack is installed
if ! command -v wasm-pack &> /dev/null; then
    echo "📦 Installing wasm-pack..."
    curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh
fi

# Build Rust VM to WASM
cd rust-vm

echo "🔧 Compiling Rust to WebAssembly..."
wasm-pack build --target web --out-dir ../public/pkg

if [ $? -eq 0 ]; then
    echo "✅ Rust VM build successful!"
    echo "   WASM module: public/pkg/gasang_vm_bg.wasm"
    echo "   JS bindings: public/pkg/gasang_vm.js"
else
    echo "⚠️  Rust build failed (this is optional - Haskell VM will still work)"
    exit 0  # Don't fail the entire build
fi

cd ..

echo "✅ Rust build process complete!"

