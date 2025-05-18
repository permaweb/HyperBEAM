#!/bin/bash
set -e  # Exit on any error

# Function to build and copy NIF
build_nif() {
    local nif_name=$1
    echo "Building ${nif_name}..."
    cd native/${nif_name}
    cargo build --release
    mkdir -p ../../priv/crates/${nif_name}
    cp target/release/lib${nif_name}.so ../../priv/crates/${nif_name}/${nif_name}.so
    cd ../..
}

# Main script
echo "Starting deployment..."

# Build all NIFs
build_nif "load_revm_nif"
build_nif "kernel_em_nif"
build_nif "riscv_em_nif"

echo "All NIFs built and copied successfully"