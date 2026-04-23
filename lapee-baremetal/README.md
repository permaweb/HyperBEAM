# lapee-baremetal

Working reference implementation of the LapEE attested-appliance
architecture for HyperBEAM, as described in `../lapee-paper/main.tex`.

## v1.0 -- Framework bare-metal boot PASSED (2026-04-22)

A commodity USB stick, written from `make hb-usb-image`, booted on
Sam's Framework 13 AMD Ryzen laptop (Insyde H2O BIOS `IFR30.03.04`)
through the firmware's real UEFI + AMD fTPM, produced a real signed
quote over PCRs `[0, 1, 7, 10, 11, 14, 15]`, wrote the attestation
envelope back to the ESP, and parsed end-to-end on the verifier side
(CRTM version, TME on, Secure Boot state, UKI hash, quote integrity
all extracted with tiered evidence). Full trace preserved at
`../out/local-capture/framework-13-v1-0-usb-roundtrip/`; end-to-end
narrative in [`STATUS.md` -> v1.0 Framework bare-metal bookend](STATUS.md#v10-framework-bare-metal-bookend-2026-04-22).

USB image build + write:

```bash
make hb-usb-image                          # build work/lapee-usb.img
make hb-usb-write DEV=/dev/diskN           # dd to real USB
# boot target laptop from that USB, then:
./scripts/interpret-local-capture.sh \
    --label 'my laptop' \
    /Volumes/LAPEE_ESP/attestation-latest.json
```

Pre-flight firmware checklist (Framework 13 / 16):

1. Disable Secure Boot in Insyde H2O (requires a Supervisor Password
   before the SB toggle appears -- this is the one firmware-settings
   gotcha).
2. Leave TPM 2.0 on (default).
3. Leave Memory Encryption (AMD SME / Intel TME) on if available.
4. Boot from USB; the image uses the UEFI fallback path
   `\EFI\Boot\BootX64.efi` so no NVRAM BootOrder entry is needed.

## Status -- paper-aligned chain works end-to-end

A real Linux kernel (Buildroot-built) boots under QEMU with a real
TPM 2.0 (swtpm), a HyperBEAM release runs as PID 2, the enforced
`on.start` hook fires and extends PCR 15 with
`hb_message:id(node_message)`, and any consumer can
`GET /~tpm2@2.0a/attestation` (with `accept: application/json@1.0 +
accept-bundle: true`) and feed the envelope through either the
Python reference verifier or a second HyperBEAM node's
`~tpm-interpret@1.0/verify-peer` to get a cryptographic proof of
the chain. All binary fields are base64url (no hex anywhere on
the wire).

### `~tpm-interpret@1.0` — discovery + verification endpoints

| endpoint | what |
|---|---|
| `.../verify-peer?peer=<url>` | Fetch peer's attestation with a **fresh random nonce challenge**, run the 5-check crypto battery locally, return link-free summary. Full trust decision. |
| `.../peer-summary?peer=<url>` | Fetch + interpret, no crypto (~10× cheaper than verify-peer). Dashboards. |
| `.../peer-status?peer=<url>` | Reachability + envelope version + wallet + node_message_id only. Cheapest probe. |
| `.../summary` | Same-node summary (takes envelope, returns link-free summary). |
| `.../checks` | Machine-readable description of the 5 crypto checks, with per-check `{name, purpose, failure_implies}`. |
| `.../info` | Full self-description: every handler's params + response shape + `wire_format` convention. |

Every `verify` path (same-node `/verify`, chain URL
`/attestation/verify~tpm-interpret@1.0`, and cross-node
`verify-peer`) returns a `trust_anchor_source` field so callers
see which CA was used: `"request"` / `"node_config"` / `"none"` —
no silent overrides. Inline trust anchor: `?trusted-ca=<base64url
PEM bytes>`. Raw-PEM `?trusted-ca-pem=` is back-compat but unsafe
over URL-encoded GET (form encoding mangles the PEM header).

`verify-peer` additionally enforces a **fresh-nonce challenge**
by default: the verifier generates a random 32-byte nonce per
call, passes it in the peer fetch, and rejects with
`nonce_freshness: "mismatch"` BEFORE any crypto if the envelope's
quote nonce doesn't match. Protects against replay of previously-
captured valid envelopes.

Quickest demo (Mac + Homebrew `qemu swtpm docker` + Rosetta):

```bash
make builders         # build Docker amd64 images (one-time)
make buildroot        # build the LapEE kernel + minimal initramfs (slow, one-time)
make hb-release       # build the HB release with the dev_tpm2 NIF (slow, one-time)
make hb-initramfs     # assemble the guest image with the HB release + libtss2
make hb-all           # boot guest → fetch attestation → verify
```

Expected tail:

```
[PASS] EK certificate chains to trusted TPM vendor root CA
[PASS] TPM2_Quote signature + pcrDigest + nonce all valid
[PASS] Runtime event log replay of PCR 15 matches quoted value
[PASS] PCR 15 extension commits to node_message_id
[PASS] Embedded node_message + id present and correct shape
VERDICT: ATTESTATION ACCEPTED
```

For the full positive+negative test matrix (3 envelopes verified,
7 byte-flips rejected, layered-config semantics proven):

```bash
make hb-acceptance    # 3 boots: baseline / benign user cfg / hostile user cfg
make hb-tamper-test   # 7 targeted byte-flips; each rejected at the expected check
```

Evidence (with README) in `out/evidence/`. Full chronological build
log — including the root cause and fix for the slirp-hostfwd issue
(Debian kernel's `virtio_net` is a module; Buildroot kernel builds
it in-tree) — in `STATUS.md`.

### Earlier milestones

`make all` still drives the original M2–M5 flow (Erlang TPM NIF in
a Linux container against swtpm, no QEMU):

| M  | What                                                       | Status |
|----|------------------------------------------------------------|--------|
| M2 | Erlang TPM NIF → `libtss2-esys` → swtpm                    | DONE   |
| M3 | NIF cross-compiled for Linux/amd64, loaded in BEAM         | DONE   |
| M4 | NIF `pcr_read` = `tpm2_pcrread` byte-for-byte              | DONE   |
| M5 | HyperBEAM-in-Linux emits signed `attestation.json`         | DONE   |
| —  | dev_tpm2 device + QEMU guest + verifier (see above)        | DONE   |
| —  | Buildroot kernel + minimal initramfs (`make buildroot`)    | DONE   |

## Quick start

Prerequisites: Docker Desktop (with Rosetta for aarch64 hosts),
`swtpm`, `qemu-system-x86_64` — all installable via Homebrew.

```bash
make builders   # build Docker amd64 Rosetta images (one-time, ~3 min)
make nif-linux  # cross-compile the NIF for x86_64 Linux (~30 sec)
make all        # swtpm + real BEAM + real NIF + attestation + verify
```

Expected tail of the last command:

```
[PASS] EK certificate chains to test TPM vendor root
[PASS] Event log replays to claimed PCR values
[PASS] Ephemeral pubkey is bound to PCR 15 via key-pubkey-extend
[PASS] TPM2_Quote signature valid under AK public key
[PASS] Quote nonce binds to AO-Core hashpath tip (anti-replay)
[PASS] AO-Core hashpath replays cleanly
[PASS] RSASSA-PSS signature over hashpath tip verifies under ephemeral pubkey
VERDICT: ATTESTATION ACCEPTED
```

## What's in the attestation

The envelope (`out/attestation.json`) links in one signed bundle the
three elements the project required:

1. **All known TPM + platform state, human-readable.** `machine_fields`
   gives CPU family, TPM manufacturer/type, TME state (honestly
   `false` in this container substrate with a note), Secure Boot
   posture, IOMMU policy, kernel lockdown, OS image golden hashes,
   HyperBEAM version, and a `measured_boot_source` key that labels
   whether the event log came from firmware or from software extend.
   The `tcg_event_log` enumerates every PCR extension with a named
   event type.

2. **The node's local key.** `node_ephemeral_key.public_pem` is the
   RSA-2048 PSS public key generated fresh inside swtpm for this run;
   it appears identically in `ak_pub_pem` and in
   `signature_over_hashpath_tip.public_key_pem`. The pubkey's SHA-256
   is the value extended into PCR 15 via the
   `EV_HYPERBEAM_KEY_BINDING` event — the verifier replays PCR 15 and
   confirms this.

3. **A TPM attestation we trust.** `ek_cert_pem` is signed by the test
   TPM-vendor CA (`out/test-tpm-ca.crt`). `pcr_quote` is a real
   `TPM2_Quote` over PCRs {0, 1, 7, 11, 14, 15} produced by the NIF
   via `Esys_Quote`; its nonce is
   `SHA-256("lapee/quote/" || hashpath_tip)`, linking the quote to
   the AO-Core computation transcript.

## Linking a signed AO-Core result to a LapEE node

The `signature_over_hashpath_tip` field is a PSS signature over the
AO-Core hashpath tip, produced by the same TPM-held ephemeral key
that is measured into PCR 15. The node's canonical "address" is
SHA-256 of the public key (written to `out/node-address.txt`). A
consumer of a signed AO-Core result can:

1. Compute the sender's address from the signature's public key.
2. Match that address against the `node_ephemeral_key.public_pem`
   in an attestation artefact.
3. Verify the attestation (all 7 checks above).
4. Accept the result iff every step succeeds.

## Layout

```
lapee-baremetal/
├── Makefile                 builders | nif-linux | demo | verify | all
├── STATUS.md                live build log (chronological, with the pivot)
├── README.md                this file
├── scripts/
│   ├── swtpm.sh             swtpm start/stop/status/reset
│   ├── boot-m1.sh           QEMU boot harness (stretch goal — see STATUS)
│   ├── secureboot-keys.sh   operator-enrolled PK/KEK/db generation (M6)
│   ├── uki.sh               UKI assembly + signing (M6)
│   └── verity.sh            dm-verity rootfs sealing (M6)
├── docker/
│   ├── Dockerfile.builder       Buildroot build container (amd64 Rosetta)
│   └── Dockerfile.hyperbeam     BEAM + NIF + attestation runtime (amd64)
├── lapee-tpm/               the real Erlang TPM NIF + attestation orchestrator
│   ├── rebar.config         macOS-host build (against Homebrew tss2)
│   ├── rebar.config.linux   Linux-container build (system libtss2-dev)
│   ├── c_src/
│   │   ├── lapee_tpm_nif.c      654 lines — Esys_* wrappers
│   │   └── tpm_helpers.c/.h     120 lines — PEM encoding, error mapping
│   ├── src/
│   │   ├── lapee_tpm_nif.erl    NIF loader + stubs
│   │   ├── lapee_tpm.erl        thin Erlang API
│   │   ├── lapee_hashpath.erl   AO-Core hashpath primitive
│   │   └── lapee_node.erl       end-to-end orchestrator (8 steps)
│   ├── test/real_quote_test.erl eunit acceptance test
│   └── RESULT.md                sub-agent's report on M2
├── buildroot-external/      Buildroot external tree (M6+ hardware path)
├── build-alpine/            Alpine netboot kernel (M1 stretch, see STATUS)
├── build-hyperbeam/         clean HyperBEAM source copy for container builds
├── reference-demo/          prior Python-only concept demo + shared verifier
├── out/                     attestation.json + keys + node-address.txt
└── work/                    swtpm state + NIF cross-compile sysroot
```

## Architecture (what actually runs when you type `make all`)

```
host (macOS aarch64)
├── swtpm (TCP 2321/2322)                             ← software TPM 2.0
├── Docker Desktop VM (Linux 6.5 amd64)
│   └── lapee-hyperbeam-builder container
│       ├── BEAM (OTP 27)
│       │   └── lapee_tpm application
│       │       └── priv/lapee_tpm_nif.so
│       │           ├── libtss2-esys.so.0
│       │           ├── libtss2-mu.so.0
│       │           └── libtss2-tctildr.so.0
│       │               └── TCTI: swtpm on
│       │                      host.docker.internal:2321
│       └── lapee_node:run/1 orchestrator (8 steps)
│           emits /out/attestation.json
└── reference-demo/verifier/verifier.py (offline)
    └── reads out/attestation.json, runs 7 checks
```

## What this doesn't do (honestly)

- **Does not boot its own signed kernel under QEMU with measured boot
  from firmware.** Scripts for this exist (`scripts/uki.sh`,
  `scripts/boot-m1.sh`, `buildroot-external/`) but the Buildroot-under-
  Rosetta first-build blew the overnight time budget — see
  `STATUS.md`. The kernel is real (Docker Desktop VM's Linux 6.5
  amd64) but was booted by macOS, not by a signed UKI we built. PCRs
  0/7/11/14 are extended by software at HyperBEAM startup, with the
  envelope explicitly labelling
  `measured_boot_source = software_extend_in_hyperbeam`.
- **Does not read `IA32_TME_ACTIVATE` / `SYSCFG` MSRs.** The envelope
  reports `tme_active: false` with a note. On real Intel vPro / AMD
  Ryzen Pro silicon this MSR read is one line of C.
- **Does not prove AK ↔ EK binding via credential activation.** The AK
  is a primary under the Endorsement hierarchy; binding it to the EK
  cert chain via `Esys_ActivateCredential` is a known follow-up.
- **Does not sign UKIs with operator-enrolled Secure Boot keys.** The
  tooling (`scripts/secureboot-keys.sh`, `scripts/uki.sh`) is ready to
  drive when the Buildroot path completes.

## What IS real

- `lapee-tpm/c_src/lapee_tpm_nif.c` — a real NIF, 807 lines, linking
  `libtss2-esys.so.0`. No subprocess calls to `tpm2_*` CLIs from
  Erlang.
- `lapee_tpm_nif:pcr_read(I)` returns byte-identical output to
  `tpm2_pcrread sha256:I` against the same swtpm (M4 acceptance).
- The `TPM2_Quote` in `out/attestation.json` was produced by the NIF
  via `Esys_Quote`. Its signature is validated by OpenSSL directly
  against the `TPMS_ATTEST` bytes (the verifier's Check 4).
- The orchestrator (`lapee_node.erl`, 230 lines) runs real TPM ops in
  sequence: create EK, create signing key under EK, extend PCRs
  0/7/11/14 with measured-boot hashes, extend PCR 15 with the signing
  key's pubkey-SHA-256, seed an AO-Core hashpath from the TPM state,
  run a 3-message conversation through the hashpath, quote the PCRs
  with a nonce committing to the hashpath tip, sign the hashpath tip
  with the ephemeral key, serialize everything to JSON.

## Hand-off to real hardware

When porting this to a physical LapEE:

1. Replace Docker Desktop's VM kernel with a Buildroot UKI booted by
   QEMU+OVMF (or by a physical machine directly).
2. Swap swtpm for the real TPM. The NIF's TCTI is one env var:
   `LAPEE_TPM_TCTI=device:/dev/tpmrm0`.
3. Have early userspace code (lapee-init or a systemd unit) read
   `IA32_TME_ACTIVATE` / `SYSCFG` and abort if memory encryption is
   off. One MSR read, ~20 lines of C.
4. Add `Esys_ActivateCredential` to the NIF and use it to prove
   AK ↔ EK at attestation time.

The Erlang code above stays as-is.
