# Setting Up a Local Simulated TPM on Ubuntu 22.04

This guide walks you through setting up a software TPM (Trusted Platform Module) emulator on Ubuntu 22.04 for development and testing of TPM-based applications. A simulated TPM provides all the functionality of a hardware TPM without requiring dedicated hardware.

## Quick Start

```bash
# Install packages
sudo apt update
sudo apt install -y libtss2-dev swtpm swtpm-tools tpm2-tools

# Create and initialize TPM
mkdir -p ~/.swtpm
swtpm_setup --tpmstate ~/.swtpm --tpm2 --create-ek-cert --create-platform-cert

# Start TPM (use $HOME instead of ~ to avoid path expansion issues)
swtpm socket --tpmstate dir=$HOME/.swtpm --tpm2 \
  --ctrl type=tcp,port=2322 --server type=tcp,port=2321 \
  --flags not-need-init &

# Set environment and initialize TPM
export TPM2TOOLS_TCTI="swtpm:host=127.0.0.1,port=2321"
tpm2_startup -c

# Test TPM
tpm2_getrandom 16 | xxd

# Read PCR values
tpm2_pcrread sha256:0,1,2

# Test capabilities
tpm2_getcap properties-fixed
```

## Helpers

```bash
# Restart/Clear software tpm
sudo pkill swtpm && sudo rm -rf ~/.swtpm/ && mkdir -p ~/.swtpm && swtpm_setup --tpmstate ~/.swtpm --tpm2 --create-ek-cert --create-platform-cert

swtpm socket --tpmstate dir=$HOME/.swtpm --tpm2 \
  --ctrl type=tcp,port=2322 --server type=tcp,port=2321 \
  --flags not-need-init &

```