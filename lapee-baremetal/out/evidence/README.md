# LapEE attestation evidence

Preserved-from-history: three real attestation envelopes captured
inside the QEMU guest (Buildroot kernel + HyperBEAM release +
swtpm TPM 2.0) on 2026-04-19, plus the v1.1 parser + v1.2
interpreter output snapshots they produced. Retained as an
as-of-date record of what the chain looked like when each
milestone shipped; see `../../HISTORY.md` for the full
chronology.

On the v1.2.2 path the equivalent "reproduce me" step is
`./scripts/interpret-local-capture.sh <label> <envelope.json>`
against a real Framework-booted envelope (see `../../README.md`
quickstart). The Python reference verifier + standalone
`boot-hb.sh' QEMU harness that produced the 2026-04-19 files
were retired in batch 18.

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

## How to reproduce (historical; retained for audit trail)

The 2026-04-19 boot-hb.sh + verifier_hb.py tooling is not on the
v1.2.2 path; re-creating these files would require checking out
the tag at commit `6c21..` and running the then-current Makefile
targets. The chain below is how those three envelopes were
produced:

```
make hb-initramfs               # build guest image
./scripts/boot-hb.sh            # QEMU + swtpm + HB release
python3 reference-demo/verifier/verifier_hb.py \
    out/attestation.json out/test-tpm-ca.crt
```

Modern equivalent:

```
# On the Framework: boot a signed LapEE USB and let it run.
# The init writeback path emits /Volumes/LAPEE_ESP/attestation-
# latest.json. On the verifier Mac:
./scripts/interpret-local-capture.sh \
    --label 'Framework 13 v1.2.2' \
    /Volumes/LAPEE_ESP/attestation-latest.json
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

## 2026-04-20 pass — rich TCG event log parsing + evidence dashboard

| file | what it is |
|---|---|
| `dashboard.html` | (Historical) self-contained HTML dashboard that the 2026-04-20 `scripts/build-evidence-dashboard.py` generated from this directory + the deleted `out/acceptance/` battery. Generator + acceptance tooling were retired in batch 18. The v1.2.2 dashboard is what `interpret-local-capture.sh' opens in Chrome. |
| `interpret-verify-live.json` | Latest live output of the chain URL `/~tpm2@2.0a/attestation/verify~tpm-interpret@1.0`. Now carries the enriched interpretation tree: each `pcrs.<N>` entry has `digest`, `role`, `is_zero`, `events` (filtered by PCR, 1-indexed by seq), `event_count`, `reconstruction` (replayed_digest + matches_quoted), and `derived` (named fields extracted from this PCR's events — `crtm_version`, `secure_boot_enabled`, `pk_entry_count`, `option_rom_scanned`, `uki_kernel_version`, ...). Every field is AO-Core path-addressable. |
| `events-live.json` | Live `/events` — the full parsed TCG event log. 16 records on the SeaBIOS dev guest (SpecID, EV_ACTION × 2, EV_EVENT_TAG × 5, EV_SEPARATOR × 8). Each event has `seq`, `pcr`, `event_type`, `event_type_code`, `digests.{sha1,sha256,sha384,sha512}`, `event_data` (base64url), and `parsed` with per-type decoding. |
| `claim-live.json` | Live `/claim` — the flat policy surface. On this dev guest (QEMU + SeaBIOS, no UEFI, no UKI), every field is `"unknown"` because the event log doesn't carry the evidence to decide — distinguishing "no evidence" from "present and false". On real UEFI silicon, `secure_boot.enabled`, `firmware.crtm_version`, `kernel.uki_hash`, etc. would be populated concrete values. |
| `att-live.json` + `ca-live.crt` | The attestation envelope + per-boot test CA used by every `*-live.json` above. |
| `hyperbuddy-attestation.txt` / `hyperbuddy-interpret.txt` / `hyperbuddy-events.txt` / `hyperbuddy-claim.txt` | Plain-text renderings of the four AO-Core message trees (from `format~hyperbuddy@1.0&truncate-keys=1000`). Every line in these files is a **live addressable path** — a caller can navigate to any node with a URL like `.../interpret~tpm-interpret@1.0/pcrs/0/derived/spec_id`. |
