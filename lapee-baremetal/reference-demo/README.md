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
one: `buildroot-external/`, `lapee-init/`, `lapee-tpm/`, the Erlang
`src/dev_tpm2.erl` / `src/dev_tpm_interpret.erl` devices, and so on.

## Verifier: use `verifier_hb.py`, not `verifier.py`

The current LapEE envelope (v0.3) uses base64url everywhere — the
HyperBEAM wire convention. The Phase-3 verifier for that schema is
`reference-demo/verifier/verifier_hb.py`:

```sh
python3 reference-demo/verifier/verifier_hb.py \
    out/evidence/att-baseline.json \
    out/evidence/ca-baseline.crt
```

The older `verifier.py` + `hyperbeam_node.py` in this directory target
the **Phase-1 hex-encoded schema** and are kept only for pedagogy —
they cannot consume an output from `src/dev_tpm2.erl` today. If you
want to reimplement the LapEE verifier in another language, start from
`verifier_hb.py`: the envelope it parses is the one the real HB device
produces.
