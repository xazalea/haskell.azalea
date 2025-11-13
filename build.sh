#!/bin/bash
set -e

echo "🚀 Building Azalea (Haskell + Rust)..."

# Build Rust VM first (optional, won't fail if it doesn't work)
if [ -d "rust-vm" ]; then
    echo "🔨 Building Rust VM to WebAssembly..."
    bash build-rust.sh || echo "⚠️  Rust build skipped (optional)"
fi

# Try to install system dependencies if possible (may not work on Vercel)
if command -v yum &> /dev/null; then
    echo "📦 Installing system dependencies..."
    yum install -y gmp-devel zlib-devel libffi-devel || true
elif command -v apt-get &> /dev/null; then
    echo "📦 Installing system dependencies..."
    apt-get update && apt-get install -y libgmp-dev zlib1g-dev libffi-dev || true
fi

# Install Stack if not present
if ! command -v stack &> /dev/null; then
    echo "📦 Installing Haskell Stack..."
    curl -sSL https://get.haskellstack.org/ | sh
    export PATH="$HOME/.local/bin:$PATH"
    export PATH="/vercel/.local/bin:$PATH"
fi

# Try to build Haskell, but don't fail if it doesn't work
# (Frontend works without it since VM can run client-side with Rust)
echo "🔨 Attempting to compile Haskell..."
if stack build --system-ghc 2>&1; then
    echo "✅ Haskell build successful!"
else
    echo "⚠️  Haskell build failed (this is okay - Rust VM can run client-side)"
    echo "   The VM can run entirely in the browser with Rust/WASM, so server-side Haskell is optional."
fi

echo "✅ Build process complete!"

