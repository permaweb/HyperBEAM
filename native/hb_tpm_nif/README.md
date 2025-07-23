# Setting Up a Local Simulated TPM on Ubuntu 22.04

This guide walks you through setting up a software TPM (Trusted Platform Module) emulator on Ubuntu 22.04 for development and testing of TPM-based applications. A simulated TPM provides all the functionality of a hardware TPM without requiring dedicated hardware.

## Docker Setup

For a containerized TPM setup, use the provided Docker configuration:

```bash
# Or build and run manually
docker build -t swtpm-server .
docker run -d -p 2321:2321 -p 2322:2322 --name swtpm-server swtpm-server

# Connect to TPM from host (set environment variable)
export TPM2TOOLS_TCTI="swtpm:host=127.0.0.1,port=2321"

# Test from host
tpm2_getrandom 16 | xxd
tpm2_pcrread sha256:0,1,2

# Stop/restart container
docker stop swtpm-server
docker start swtpm-server

# View logs
docker logs swtpm-server
```