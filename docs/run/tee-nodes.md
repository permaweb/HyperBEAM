# Trusted Execution Environment (TEE)

!!! tip "Recommended Setup"
    Use [HyperBEAM OS](https://github.com/permaweb/hb-os) for the easiest TEE deployment with pre-configured AMD SEV-SNP support. **Note:** HB-OS is typically used for TEE operations, but is not necessary for router registration.

## Overview

HyperBEAM supports Trusted Execution Environments (TEEs) through the `~snp@1.0` device, enabling secure, verifiable computation on remote machines. TEEs provide hardware-level isolation and cryptographic attestation that allows users to verify their code is running in a protected environment exactly as intended, even on untrusted hardware.

The `~snp@1.0` device generates and validates attestation reports that prove:

- Code is running inside a genuine AMD SEV-SNP TEE
- The execution environment hasn't been tampered with
- Specific software components (firmware, kernel, initramfs) match trusted hashes
- Debug mode is disabled for security

## Configuration Files

Configuration can be set in either `config.json` (JSON) or `config.flat` (flat) format. For full details and examples of both formats, see [Configuration Reference](/docs/run/configuring-your-machine#configuration-configflat).

The examples below use JSON for clarity.

## When to use HB-OS

| Operation                | Use HB-OS?    | Purpose                                             |
|-------------------------|:-------------:|-----------------------------------------------------|
| **TEE Node (SNP)**      |   Recommended | Secure, attested computation (hardware isolation)    |
| **Router Registration** |   Optional    | Registering/joining a router (TEE not required)      |

- **If you are registering or running a router, you can do so without HB-OS.**
- **If you want to run a TEE node, HB-OS or an equivalent TEE setup is recommended for convenience and security.**

## Quick Start: TEE Node with HyperBEAM OS

### Prerequisites

- AMD EPYC processor with SEV-SNP support (Milan generation or newer)
- Host system with SEV-SNP enabled in BIOS

### Setup TEE Node

```bash
# Clone and build TEE-enabled HyperBEAM
# (Only needed for TEE nodes if you choose HB-OS)
git clone https://github.com/permaweb/hb-os.git && cd hb-os
./run init && ./run setup_host && ./run build_base_image && ./run build_guest_image

# Launch TEE-protected node
./run start
```

The VM boots with dm-verity protection, measured boot, and automatic attestation report generation.

## Using the SNP Device

### Generate Attestation Report

Request an attestation report from a TEE node:

```bash
curl https://your-tee-node.com/~snp@1.0/generate
```

Returns a signed attestation report containing:
- **Nonce**: Unique identifier preventing replay attacks
- **Address**: Node's ephemeral public key (only exists inside TEE)
- **Measurement**: Cryptographic hash of the execution environment
- **Report**: AMD SEV-SNP hardware attestation with certificate chain

### Verify Attestation Report

The verification process validates:
1. **Nonce integrity**: Ensures report freshness and prevents replay
2. **Signature validity**: Confirms the report was signed by the claimed address
3. **Address authenticity**: Verifies the signing key exists only in the TEE
4. **Debug disabled**: Ensures no debugging capabilities that could compromise security
5. **Trusted software**: Validates firmware, kernel, and initramfs hashes match approved versions
6. **Measurement accuracy**: Confirms the reported environment matches actual execution
7. **Hardware attestation**: Verifies AMD's cryptographic signature on the report

## Configuration

### Trusted Software Hashes (config.json example)

Configure which software components are trusted by setting `snp_trusted` in your node options:

```json
"snp_trusted": [
  // Trusted software hashes here
]
```

### Custom Trust Validation

Implement custom trust policies by specifying an `is-trusted-device`:

```bash
curl -X POST https://your-node.com/~snp@1.0/verify \
  -H "is-trusted-device: my-custom-validator@1.0" \
  -d '{"report": "...", "target": "self"}'
```

## Security Considerations

- **SEV-SNP capable CPU**: AMD EPYC Milan or newer
- **Firmware support**: Recent AMD firmware with SEV-SNP enabled
- **Memory encryption**: SME (Secure Memory Encryption) recommended
- **RMP table**: Sufficient memory reserved for Reverse Map Page Table

## Attestation Tools

HyperBEAM OS includes several attestation utilities:

- **`get_report`**: Generate attestation reports with custom data
- **`verify_report`**: Validate attestation report signatures
- **`sev_feature_info`**: Check host SEV-SNP capabilities
- **`idblock_generator`**: Create signed VM configuration blocks

## Integration Examples

### Router Registration with TEE (Advanced, config.json example)

If you want to register a TEE-protected router node, use the following configuration (see also the router registration guide):

```json
{
  "operator": "trustless",
  "initialized": "permanent",
  "snp_trusted": [ /* ... */ ],
  "on": {
    "request": {
      "device": "p4@1.0",
      "ledger-device": "lua@5.3a",
      "pricing-device": "simple-pay@1.0",
      "ledger-path": "/ledger~node-process@1.0",
      "module": ""
    },
    "response": {
      "device": "p4@1.0",
      "ledger-device": "lua@5.3a",
      "pricing-device": "simple-pay@1.0",
      "ledger-path": "/ledger~node-process@1.0",
      "module": ""
    }
  },
  "p4_non_chargable_routes": [
    {"template": "/.*~node-process@1.0/.*"},
    {"template": "/.*~greenzone@1.0/.*"},
    {"template": "/.*~router@1.0/.*"},
    {"template": "/.*~meta@1.0/.*"},
    {"template": "/schedule"},
    {"template": "/push"},
    {"template": "/~hyperbuddy@1.0/.*"}
  ],
  "node_process_spawn_codec": "ans104@1.0",
  "node_processes": {
    "ledger": {
      "device": "process@1.0",
      "execution-device": "lua@5.3a",
      "scheduler-device": "scheduler@1.0",
      "authority-match": 1,
      "admin": "",
      "token": "",
      "module": "",
      "authority": ""
    }
  },
  "router_opts": {
    "offered": [ /* ... */ ]
  },
  "green_zone_peer_location": "",
  "green_zone_peer_id": "",
  "p4_recipient": ""
}
```

### TEE-Related Operations

- **TEE attestation**
- **TEE-protected computation**
- **Trusted software validation**

## Further Reading

- [HyperBEAM OS Repository](https://github.com/permaweb/hb-os)
- See the router registration guide for non-TEE router setup.
- [Configuration Reference](/docs/run/configuring-your-machine#configuration-configflat) 