# LapEE bare-metal -- status

LapEE boots on a Framework 13 (Intel/Ryzen, discrete or fTPM 2.0)
from a single USB stick, brings up wired or WiFi networking, and
serves a full HyperBEAM attestation envelope at
`/~tpm2@2.0a/attestation`. A separate Python verifier and the
`~tpm-interpret@1.0` device both validate the chain end-to-end.

The build-up history is preserved in [`HISTORY.md`](HISTORY.md).

## Acceptance

Live envelope from a Framework 13 boot, scored by the dashboard:

```
verdict   = attested-with-warnings (score 92)
criticals = 0  warnings = 1
machine   = Framework Computer, Inc. Laptop (13th Gen Intel Core)
firmware  = CRTM IFR30.03.04 (INSYDE H2O)
TPM       = Nuvoton discrete (trust-tier=strongest)
posture   = SB on, lockdown=confidentiality, TME on
boot      = boot-chain len=1
context   = tcg-pc-client
```

The single warning is `freshness-safe-false-stale-counters` -- HB
does not issue `TPM2_Shutdown(STATE)` at node shutdown (single-
purpose appliance power-cycle model), so the TPM reports `safe=NO`
across boots. Per-envelope freshness is intact (the quote's
`extraData` binds to the verifier's nonce); cross-envelope
counter-monotonic replay defence is weaker. Default LapEE policy
accepts this as warn; strict policy that mandates `safe=true`
would reject.

The independent Python verifier
(`secondary-external-verifier/verifier_hb.py`, in the
`agent-/sharp-lichterman` branch) reports six green checks
on the same envelope:

```
[PASS] EK certificate chains to NUVOTON ECC521 RootCA via ECC384 LeafCA
[PASS] TPM2_Quote signature + pcrDigest + nonce all valid
       (PCRs 0,1,7,10,11,14,15)
[PASS] Runtime event log replay of PCR 15 matches quoted value
[PASS] PCR 15 seq=0 commits to node-message-id
[PASS] PCR 15 seq=1 commits to AK pub PEM (paper P5)
[PASS] Embedded node-message + id present and 32-byte b64url
VERDICT: ATTESTATION ACCEPTED
```

## Pipeline

```bash
make builders         # one-time Docker amd64 toolchain
make buildroot        # one-time kernel + busybox
make hb-release       # HyperBEAM release with the TPM NIF
make hb-initramfs     # release + busybox + init + splash + WiFi fw
make hb-usb-image     # signed UEFI bootable image at work/lapee-usb.img
make hb-usb-write DEV=/dev/diskN   # writes the image
make hb-usb-qemu      # headless QEMU+OVMF+swtpm smoke-boot
make hb-usb-qemu-gui  # same with a Cocoa window
```

`make hb-wifi-apply` injects an operator-edited `wifi.conf` into the
ESP without re-signing the UKI -- changing networks across boots is
a 5-second operation.

## Boot splash

The splash is a separate BEAM VM forked off `init` right after the
basic mounts. It owns `/dev/console` exclusively at 12 fps and runs
its own phase machine:

```
boot     /run/lapee/primary-net not yet written
         "starting LapEE..."
net-up   primary-net has an ip=, /info not yet 200
         "network up (<ip>); starting HyperBEAM..."
hb-wait  /info still not 200
         "starting HyperBEAM... <ip> (Ns)"
ready    /info returned 200
         "Running at http://<ip>:8734/"
```

The laptop wireframe rotates throughout. `LAPEE_SPLASH_SCALE=<float>`
in the kernel cmdline overrides the auto-derived size (~50 % of
screen width).

The `/info` probe is a raw `gen_tcp` HTTP/1.0 round-trip rather than
an `httpc` call -- the URL `/~tpm2@2.0a/info` contains both `~` and
`@`, which trips OTP 27's URL parser; raw `gen_tcp` has no parser to
trip.

## Attestation envelope

`/~tpm2@2.0a/attestation` returns a v0.4 envelope:

| Field                        | Source |
|------------------------------|--------|
| `lapee-attestation-version`  | constant `"0.4"` |
| `wallet-address`             | running node's wallet |
| `node-message-id`            | `hb_message:id(NodeMsg, all, Opts)` |
| `node-message`               | the running node config |
| `ek-cert-pem`                | TPM NV `0x01C00002` |
| `ek-cert-source`             | `{kind: "tpm-nv" | "absent", handle, …}` |
| `ek-cert-chain-pem`          | TPM NV `0x01C00003` (when provisioned) |
| `ak-pub-pem`                 | dev_tpm2-managed AK under EH |
| `ak-hierarchy`               | `"endorsement"` |
| `tpm-quote`                  | `{nonce, pcr-selection, pcr-values, quoted, signature}` |
| `tpm-session-mode`           | `"hmac-aes128cfb"` (paper P4) |
| `runtime-event-log`          | three `EV_HYPERBEAM_*` extends into PCR 15 |
| `tcg-event-log`              | firmware event log |
| `platform-probes`            | `dmi-*`, `kernel-cmdline`, `lockdown`, `secure-boot`, `iommu-groups-count`, … |
| `tpm-properties`             | `TPM2_GetCapability` snapshot |
| `commitments`                | per-field signatures (httpsig@1.0) |
| `issued-at-unix`             | wall-clock at envelope assembly |

All 32-byte digests, IDs and nonces are base64url (43 chars, no
padding) per HB convention.

## Boot harness

`make hb-usb-qemu` boots `work/lapee-usb.img` under QEMU+OVMF+swtpm
and polls the serial chardev for `LAPEE-WRITEBACK-OK`. On success it
extracts the ESP and copies `attestation-latest.json` plus
`lapee-splash.log` into `out/qemu-usb-test/`. The marker is written
direct to `/dev/ttyS0` rather than going through printk so the splash
on `/dev/console` is not disturbed.

`make hb-usb-qemu-gui` opens a Cocoa window so an operator can watch
the splash visually; serial is still captured to file.

## Known limitations

- **Counter-monotonicity across boots** -- TPM `safe=false` until HB
  starts issuing `TPM2_Shutdown(STATE)` on shutdown, or LapEE
  switches to a TPM owner-policy that includes a monotonic counter.
  Tracked.
- **Secure Boot** -- the stock `make hb-usb-image` produces an
  unsigned UKI. The signed-UKI workflow (`scripts/sb-setup.sh` +
  operator-enrolled keys) is validated on iron; see
  [`HARDENING.md`](HARDENING.md).
- **Cross-CPU EK chains** -- the verifier ships 51 vendor root CAs;
  some AMD fTPM configurations fetch the EK cert out-of-band from
  AMD PSP rather than NV, in which case `ek-cert-source.kind` is
  `"absent"` and the chain check declines rather than fakes a cert.

## Files

```
initramfs-hb/
  init                  PID-1 boot script (mounts, splash launch,
                        WiFi, networking, HB launch + writeback)
  lapee_splash.erl      3D BEAM splash (Bresenham wireframe + phases)
  lapee-dhcp-hook       udhcpc action: claim primary lease, write
                        /run/lapee/primary-net for the splash
scripts/
  build-buildroot.sh    Buildroot kernel/initramfs builder
  build-hb-release.sh   HyperBEAM release builder (rebar3 as lapee)
  build-initramfs-hb.sh Pack release + busybox + WiFi fw + splash
  build-usb-image.sh    UKI + GPT + ESP wrapper
  boot-usb-image.sh     QEMU+OVMF+swtpm harness
  collect-buildroot-artefacts.sh    pull kernel + modules out of the
                                    Buildroot Docker stage
  fetch-ek-root-cas.sh  refresh priv/tpm-interpret/root-cas/
  hb-cross-node-verify.sh   peer attestation driver
  sb-setup.sh           operator Secure-Boot key enrolment
  interpret-local-capture.sh   verifier + dashboard pipeline
config/                 LapEE-enforced HB config (on.start hook)
priv/tpm-interpret/     vendor root CAs + verifier corpus
```
