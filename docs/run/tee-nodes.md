# Trusted Execution Environment (TEE)

!!! tip "Recommended Setup"
    Use [HyperBEAM OS](https://github.com/permaweb/hb-os) for the easiest TEE deployment with pre-configured AMD SEV-SNP support.

## Overview

HyperBEAM supports Trusted Execution Environments (TEEs) through the `~snp@1.0` device, enabling secure, verifiable computation on remote machines. TEEs provide hardware-level isolation and cryptographic attestation that allows users to verify their code is running in a protected environment exactly as intended, even on untrusted hardware.

The `~snp@1.0` device generates and validates attestation reports that prove:

- Code is running inside a genuine AMD SEV-SNP TEE
- The execution environment hasn't been tampered with
- Specific software components (firmware, kernel, initramfs) match trusted hashes
- Debug mode is disabled for security

## Quick Start with HyperBEAM OS

### Prerequisites

- AMD EPYC processor with SEV-SNP support (Milan generation or newer)
- Host system with SEV-SNP enabled in BIOS:
    - `Secure Nested Paging` enabled
    - `Secure Memory Encryption` enabled
    - `SNP Memory Coverage` enabled
    - `Minimum SEV non-ES ASID` > 1

### Setup TEE Node

```bash
# Clone and build TEE-enabled HyperBEAM
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

### Trusted Software Hashes

Configure which software components are trusted by setting `snp_trusted` in your node options:

```erlang
{snp_trusted, [#{
    vcpus => 1,
    vcpu_type => 5,
    vmm_type => 1,
    guest_features => 1,
    firmware => <<"b8c5d4082d5738db6b0fb0294174992738645df70c44cdecf7fad3a62244b788e7e408c582ee48a74b289f3acec78510">>,
    kernel => <<"69d0cd7d13858e4fcef6bc7797aebd258730f215bc5642c4ad8e4b893cc67576">>,
    initrd => <<"853ebf56bc6ba5f08bd5583055a457898ffa3545897bee00103d3066b8766f5c">>,
    append => <<"6cb8a0082b483849054f93b203aa7d98439736e44163d614f79380ca368cc77e">>
}]}
```

### Custom Trust Validation

Implement custom trust policies by specifying an `is-trusted-device`:

```bash
curl -X POST https://your-node.com/~snp@1.0/verify \
  -H "is-trusted-device: my-custom-validator@1.0" \
  -d '{"report": "...", "target": "self"}'
```

## Security Considerations

### Hardware Requirements

- **SEV-SNP capable CPU**: AMD EPYC Milan or newer
- **Firmware support**: Recent AMD firmware with SEV-SNP enabled
- **Memory encryption**: SME (Secure Memory Encryption) recommended
- **RMP table**: Sufficient memory reserved for Reverse Map Page Table

### Verification Best Practices

1. **Always verify reports**: Never trust unverified attestation claims
2. **Check certificate chains**: Validate AMD's hardware root of trust
3. **Monitor debug flags**: Reject reports with debug mode enabled
4. **Validate measurements**: Ensure software hashes match your trusted versions
5. **Use fresh nonces**: Prevent replay attacks with unique identifiers

### Limitations

- **Performance overhead**: TEE operations have computational cost
- **Hardware dependency**: Requires specific AMD processor features
- **Measurement sensitivity**: Any software change invalidates measurements
- **Key management**: Ephemeral keys are lost on VM restart

## Attestation Tools

HyperBEAM OS includes several attestation utilities:

- **`get_report`**: Generate attestation reports with custom data
- **`verify_report`**: Validate attestation report signatures
- **`sev_feature_info`**: Check host SEV-SNP capabilities
- **`idblock_generator`**: Create signed VM configuration blocks

## Integration Examples

### Router Registration with TEE

```erlang
% Register a TEE-protected router node
hb:start_mainnet(#{
    router_peer_location => "https://forward.computer",
    router_prefix => <<"/tee-node~">>,
    router_template => <<"/tee-node~process@1.0/.*">>,
    router_price => 150,  % Premium for TEE security
    snp_trusted => [#{
        % Your trusted software configuration
    }]
}).
```

### Process with TEE Attestation

```lua
-- Request computation with TEE verification
ao.send({
    Target = "TEE_PROCESS_ID",
    Action = "compute",
    Data = "sensitive_data",
    ["Require-TEE"] = "true"
})
```

## Further Reading

- [HyperBEAM OS Repository](https://github.com/permaweb/hb-os)
