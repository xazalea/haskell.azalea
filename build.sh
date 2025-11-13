#!/bin/bash
set -e

echo "🚀 Building Azalea..."

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
# (Frontend works without it since Linux VM runs client-side)
echo "🔨 Attempting to compile Haskell..."
if stack build --system-ghc 2>&1; then
    echo "✅ Haskell build successful!"
else
    echo "⚠️  Haskell build failed (this is okay - frontend works without it)"
    echo "   The Linux VM runs entirely in the browser, so server-side Haskell is optional."
fi

echo "✅ Build process complete!"

