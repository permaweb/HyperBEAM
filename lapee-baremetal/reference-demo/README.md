# reference-demo — Python concept demo (NOT a LapEE implementation)

This directory contains Python programs that talk to `swtpm` from the
macOS host and produce JSON files shaped like a LapEE attestation. They
are useful for understanding the attestation envelope format and for
exercising the verifier against synthetic inputs.

**This is not a LapEE implementation.** It does not boot a kernel, does
not run BEAM, does not use a TPM NIF, does not measure a real boot
chain, does not involve any of the hardware or software primitives LapEE
actually composes. It should never be shipped as evidence that LapEE
works end-to-end.

The real implementation lives in the parent directory alongside this
one: `buildroot-external/`, `lapee-init/`, `lapee-tpm/`, and so on. When
the real implementation produces an attestation, the verifier in
`reference-demo/verifier/verifier.py` can still be used to consume it —
the attestation envelope format is deliberately stable — but the
verifier is the only piece of this directory that has value beyond
pedagogy.
