# **Installing Rust and Cargo**

Rust is required for certain components in the HyperBEAM ecosystem.

## Installing Rust

Install Rust using rustup:

```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y --default-toolchain stable
```

## Load Rust Environment

After installation, you'll need to load the Rust environment in your current shell:

```bash
source "$HOME/.cargo/env"
```

To make this permanent, add this line to your shell profile (~/.bashrc, ~/.zshrc, etc.).

## Verify Installation

Verify that Rust and Cargo are installed correctly:

```bash
rustc --version
cargo --version
```

These commands should display the installed versions of the Rust compiler and Cargo package manager. 