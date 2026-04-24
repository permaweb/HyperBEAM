LapEE attestation written on boot 20260423T132248Z.

On your verifier host (with the USB stick plugged in):

    ./lapee-baremetal/scripts/interpret-local-capture.sh \
        --label "Framework (USB roundtrip)" \
        /Volumes/LAPEE_ESP/attestation-latest.json

The JSON is the full envelope returned by
`~tpm2@2.0a/attestation`. The EK certificate (when the
platform TPM has one provisioned in NV storage) is embedded
inline as the `ek-cert-pem` field. Its provenance is
recorded under `ek-cert-source`:

    {"kind": "tpm-nv", "handle": "0x01C00002", "bytes": 1234}
        EK cert read from the TPM's NV storage at the TCG-
        standard low-range index. Chain-valid against the
        matching vendor EK root CA shipped in the verifier.

    {"kind": "absent", "reason": ...}
        The TPM has no EK cert provisioned at any of the
        TCG-standard indices. The attestation still carries
        the live TPM quote + PCR set; the EK chain check
        will fail because there is nothing to chain. This
        is expected on some AMD fTPM configurations where
        the cert is fetched out-of-band from AMD PSP. It is
        NOT an error from LapEE -- we deliberately never
        substitute a synthetic cert.
