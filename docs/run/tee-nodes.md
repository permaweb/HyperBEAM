# Trusted Execution Environment (TEE)

!!! info "Documentation Coming Soon"
    Detailed documentation about Trusted Execution Environment support in HyperBEAM is currently being developed and will be available soon.

## Overview

HyperBEAM supports Trusted Execution Environments (TEEs) through the `~snp@1.0` device, which enables secure, trust-minimized computation on remote machines. TEEs provide hardware-level isolation and attestation capabilities that allow users to verify that their code is running in a protected environment, exactly as intended, even on untrusted hardware.

The `~snp@1.0` device in HyperBEAM is used to generate and validate proofs that a node is executing inside a Trusted Execution Environment. Nodes executing inside these environments use an ephemeral key pair that provably only exists inside the TEE, and can sign attestations of AO-Core executions in a trust-minimized way.

## Key Features

- Hardware-level isolation for secure computation
- Remote attestation capabilities
- Protected execution state
- Confidential computing support
- Compatibility with AMD SEV-SNP technology

## Coming Soon

Detailed documentation on the following topics will be added:

- TEE setup and configuration
- Using the `~snp@1.0` device
- Verifying TEE attestations
- Developing for TEEs
- Security considerations
- Performance characteristics

If you intend to offer TEE-based computation of AO-Core devices, please see the [HyperBEAM OS repository](https://github.com/permaweb/hb-os) for preliminary details on configuration and deployment. 