#!/bin/bash
set -e

echo "🚀 Building Azalea Haskell..."

# Install Stack if not present
if ! command -v stack &> /dev/null; then
    echo "📦 Installing Haskell Stack..."
    curl -sSL https://get.haskellstack.org/ | sh
    export PATH="$HOME/.local/bin:$PATH"
fi

# Build Haskell project
echo "🔨 Compiling Haskell..."
stack build --system-ghc

echo "✅ Build complete!"

