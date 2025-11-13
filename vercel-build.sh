#!/bin/bash
# Build script for Vercel deployment
# This installs Stack and builds the Haskell project

set -e

echo "Installing Haskell Stack..."
curl -sSL https://get.haskellstack.org/ | sh

echo "Building Haskell project..."
stack build --system-ghc

echo "Build complete!"

