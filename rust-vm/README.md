# Gasang VM - Rust WebAssembly Integration

This directory contains the Rust VM implementation that compiles to WebAssembly for client-side execution.

## Building

To build the Rust VM to WebAssembly:

```bash
# Install Rust (if not already installed)
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# Install wasm-pack
curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh

# Build to WebAssembly
wasm-pack build --target web --out-dir ../public/pkg
```

Or use the npm script:

```bash
npm run build:rust
```

## Integration with Gasang

To integrate the actual gasang VM code:

1. Clone or add gasang as a dependency:
   ```bash
   git submodule add https://github.com/ArtBlnd/gasang.git gasang-src
   ```

2. Update `Cargo.toml` to include gasang:
   ```toml
   [dependencies]
   gasang = { path = "../gasang-src" }
   ```

3. Update `src/lib.rs` to use gasang's actual VM implementation instead of the placeholder.

## Current Status

This is a placeholder implementation that provides the same interface as the Haskell VM. To get the full power of gasang, integrate the actual gasang VM code from https://github.com/ArtBlnd/gasang.

