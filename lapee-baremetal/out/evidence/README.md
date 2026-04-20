# LapEE attestation evidence

Three real attestation envelopes captured inside the QEMU guest
(Buildroot kernel + HyperBEAM release + swtpm TPM 2.0) on
2026-04-19. Verified by `reference-demo/verifier/verifier_hb.py`.

## Files

| file | what it is |
|---|---|
| `att-baseline.json` | Envelope produced by `make hb-boot` with no user config. The guest ran only the enforced `on.start` hook and HB's defaults. **All 5 verifier checks pass.** |
| `att-user-diff.json` | Envelope after booting with `--user user-diff.flat`. The user added two harmless custom keys (`operator/label`, `operator/deployment_id`). The resulting `node_message_id` (43-char base64url) differs from baseline, proving the user config actually became part of the attested node message. Verifier still passes. |
| `att-hostile-override.json` | Envelope after booting with `--user user-hostile.flat`. The user tried to disable the attestation hook by setting `on/start/device: noop@1.0` / `on/start/path: nothing` / `on/start/method: GET`. The enforced config layer overrode those keys; the embedded `node_message.on.start` in the envelope shows `device: tpm2@2.0a, path: extend, method: POST` — the real hook. The runtime event log still contains `EV_HYPERBEAM_NODE_IDENTITY_EXTEND` at PCR 15. The user's non-colliding key (`operator/intent: hostile-override-attempt`) did pass through, as expected from a rightmost-wins merge. **Verifier still passes.** |
| `ca-baseline.crt` | Self-signed "LapEE Test TPM Vendor Root CA" emitted by the baseline boot on serial. The EK in `att-baseline.json` chains to this. |
| `ca-user-diff.crt` | Same, for the user-diff run. |
| `ca-hostile.crt`   | Same, for the hostile-override run. Each boot generates a fresh CA; verifying an envelope requires the CA from the **same** boot. |
| `user-diff.flat`, `user-hostile.flat` | The two user-supplied flat configs used as inputs. |

## How to reproduce

```bash
cd lapee-baremetal
make hb-initramfs                           # build guest image
./scripts/boot-hb.sh                        # -> out/attestation.json
python3 reference-demo/verifier/verifier_hb.py \
    out/attestation.json out/test-tpm-ca.crt

# Negative test — user config present but non-colliding:
./scripts/boot-hb.sh --user out/evidence/user-diff.flat
python3 reference-demo/verifier/verifier_hb.py \
    out/attestation.json out/test-tpm-ca.crt

# Hostile override — user tries to disable the hook, enforced wins:
./scripts/boot-hb.sh --user out/evidence/user-hostile.flat
python3 reference-demo/verifier/verifier_hb.py \
    out/attestation.json out/test-tpm-ca.crt
```

Each envelope matches what the paper calls the full chain:

    EK cert (chain → CA) → AK (via TPM policy) → TPM2_Quote
        → PCR 15 = sha256(0x00..00 || hb_message:id(node_message))
        → runtime_event_log entry for that extend
        → node_message itself, with the operator wallet bound

## After the ~tpm-interpret@1.0 phase (2026-04-19)

| file | what it is |
|---|---|
| `interpret-verify-baseline.json` | Full live output of `GET /~tpm2@2.0a/attestation/verify~tpm-interpret@1.0` against the baseline guest. Contains `verified: true`, all 5 crypto checks passing, **and** the rich `interpretation` block (envelope, tpm, ak, quote, pcrs, boot, kernel, ima, node). This is what the user's final target URL produces end-to-end. All binary fields are base64url (43-char SHA-256 digests under `pcrs[N].digest`; `boot.firmware_srtm`, `kernel.uki_image` etc. as base64url; no `_hex` fields anywhere). |
| `cross-node-verify-baseline.json` | Full live output of a **separate HB node outside QEMU** verifying the guest inside QEMU. `GET http://127.0.0.1:18735/~tpm-interpret@1.0/verify-peer?peer=http://127.0.0.1:18734`. Includes the five crypto checks + a link-free `summary` (envelope version, TPM identity, AK fingerprint, quote metadata, node identity, hook device, pcr15 event count). The verifier has its own OS process, its own BEAM VM, its own network origin, and its trust anchor was installed before the call. See `scripts/hb-cross-node-verify.sh`. |
