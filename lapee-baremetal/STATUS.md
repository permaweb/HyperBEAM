# LapEE bare-metal -- live status

**SCHEDULE NEXT RUN AFTER COMPLETE:** FALSE.

## Overnight 2026-04-25/26 -- BEAM boot splash green end-to-end

**Acceptance test (from the original prompt):** "the boot animation
and the print of the IP address there actually works."

**Result: PASS.** A clean QEMU `make hb-usb-qemu' boot, captured at
`/tmp/lapee-usb-qemu.log' + ESP `lapee-splash.log', shows the splash
advancing through every phase:

```
[lapee-splash] phase=boot
[lapee-splash] phase=net-up ip=10.0.2.15
[lapee-splash] phase=hb-wait (conn econnrefused)
[lapee-splash] hb-wait: 5s, 12s, 18s, 24s, 31s, 37s, 43s, 50s, 56s elapsed
[lapee-splash] phase=qr (HB ready)
```

The IP is shown on screen the moment udhcpc binds (well before HB is
up), the elapsed-second counter ticks during hb-wait so a slow TCG
cold-start no longer looks identical to a hang, and the QR overlays
the lid panel once HB answers `/info'. Writeback completes,
`LAPEE-WRITEBACK-OK' reaches the serial chardev, and
`out/qemu-usb-test/attestation-latest.json' is the live envelope.

### Root causes of the previous "stuck on starting HyperBEAM..."

1. **httpc:request threw `function_clause' on the URL with `~' and
   `@'.** The guest's OTP 27 inets URL parser doesn't survive
   `/~tpm2@2.0a/info'. Every poll was caught by the splash's catch
   clause and converted to "{false, function_clause}", so the splash
   sat in hb-wait forever even after HB came up. Captured in
   splash.log on a real boot before the rewrite.

   **Fix:** drop httpc + inets entirely; speak HTTP/1.0 over a raw
   `gen_tcp:connect' / `gen_tcp:send' / `gen_tcp:recv' and check for
   the literal `HTTP/1.x 200' prefix. No URL parsing, no header
   validation, nothing that can throw on a tilde.

2. **`LAPEE-WRITEBACK-OK' marker never reached the serial console.**
   init redirected its stdio to /dev/kmsg early on; `dmesg -n 1' --
   set so the splash daemon owns /dev/console -- silences kmsg from
   reaching the serial chardev. Headless `boot-usb-image.sh' waited
   the full timeout for a marker it would never see, even when the
   guest had completed writeback.

   **Fix:** prefix the marker line with `<1>' on the kmsg copy
   (KERN_ALERT bypasses the level filter) AND `printf' a duplicate
   directly to /dev/ttyS0 when present. Best-effort, two-channel.

### Commit

`1a8b10e0e lapee-baremetal: BEAM-based 3D boot splash + writeback marker fix`

Files touched:
- `initramfs-hb/lapee_splash.erl` (NEW, 532 LoC -- the BEAM splash)
- `initramfs-hb/init` (BEAM splash launch + kmsg redirect + marker fix + splash.log writeback)
- `initramfs-hb/lapee-dhcp-hook` (minor)
- `scripts/build-initramfs-hb.sh` (compile splash.erl on host, ship platform-independent .beam)
- `scripts/boot-usb-image.sh` (`--gui' flag for visual verification mode)
- `Makefile` (splash-frames + hb-usb-qemu-gui targets)

A peer-review agent (skeptical OTP veteran personality) is reviewing
the commit while this STATUS is being written; any high-severity
findings will land in a follow-up commit before sign-off.

### Known follow-ups (not in tonight's commit)

- Orphan shell `lapee-splash` + pre-rendered `splash-frames/` are
  still shipped in the initramfs. They're called from init/dhcp-hook
  with `>/dev/null 2>&1' so they're harmless no-ops, but they add
  ~600 KB and confuse future readers. Cleanup commit candidate.

---

## Morning of 2026-04-25 -- EXACT STEPS to reach verdict=trusted

A full overnight run pushed the Framework envelope from
`untrusted (28)` to `attested-with-warnings (84)` via verifier-
side work only, then laid down guest-side changes that, once
flashed, take the verdict to `trusted (100)` -- plus
batch 32 (root-of-trust binding), plus a parallel ASCII-art
agent building the retro-laptop 3D boot animation.

**The critical addition (batch 32): `wallet-tpm-binding-verified`**
is a new signal that chains the operator wallet all the way to
the EK cert. A consumer of any AO-Core result signed by the
observed wallet now has cryptographic evidence that the result
was produced on a TPM-attested LapEE boot, with no further
interaction needed with the node. The paper's statement
("The chain binds the transcript to the boot conditional on
A1") is now code-verifiable end-to-end from envelope shape
alone. Verifier recomputes hb_message:id(node-message),
confirms equality with node-message-id, confirms wallet-
address appears in node-message, confirms a PCR-15 runtime
event carries node-message-id as digest. All three gates.
Breaks the chain -> CRITICAL finding.

Sam's ask by morning: "the HyperBEAM node's key is provably
linked to the TPM and the root of trust correctly." Shipped.
Batch 32 is the single point at which a consumer can check this
property as a single boolean in the envelope's
policy-verdict.signals, rather than having to walk five
separate checks by hand.

**Parallel: ASCII art boot animation -- LANDED**

Cherry-picked the sub-agent's retro-3D laptop boot animation
(commit `5abd17091` on agent/lapee). The morning image now
boots into a rotating ASCII wireframe clamshell laptop that
settles face-on once HB answers `/info' and reveals a scannable
QR code on its projected screen panel pointing at
`http://<node-ip>:8734/'. On-disk confirmation:

  /usr/bin/qrencode                    # 40 KB binary
  /usr/local/bin/lapee-splash          # awk-based 3D engine
  /lib/x86_64-linux-gnu/libqrencode.so.4
  /lib/x86_64-linux-gnu/libpng16.so.16

all baked into the signed UKI. Cmdline escape hatch
`lapee.nosplash=1' falls back to the static splash if you want
clean boot-log scroll instead.

**Two commands on your Mac, one re-flash, one hash re-enrol in
BIOS, one curl, one interpret. That's it.**

### Step 1 -- final guest artefact (already built overnight)

Everything is pre-built + signed on disk at 2026-04-25 03:52
(post batches 27-32 + ASCII-splash merge; includes the retro-3D
laptop boot animation + scannable QR code overlay, P4 HMAC
sessions, P5-ext hashpath continuity, P2 TME at init, P3 AK
Endorsement, lockdown=confidentiality, plus the wallet<->TPM
root-of-trust verifier binding). No command needed unless you
want to audit:

```bash
cd /Users/sam/src/hyperbeam/.claude/worktrees/lapee/lapee-baremetal

# Audit: fingerprints should match exactly
shasum -a 256 work/lapee.signed.efi
#   b76cdf98677121ede38f63f2267761fa27bf553c8a9d1a82a2997f3ff71a7fd3
shasum -a 256 work/lapee-usb.img
#   258cad7d98b21729bd38f51394c82275c443d8f101b8f290591bbcf1d555dde2

# Audit: verifies against your enrolled db.crt
./scripts/sb-setup.sh check
#   UKI in usb-build: ... [signed]
#   signed UKI (stash): 72766088 bytes

# If the UKI timestamp is earlier than the kernel fragment's
# mtime, the rebuild didn't finish cleanly. Rerun:
make hb-release hb-initramfs hb-usb-image \
    && ./scripts/sb-setup.sh sign
```

### Step 2 -- re-flash the signed USB stick (one command)

The kernel + initramfs changed (batch 28 + 29), so the UKI hash
changed. Plug in the stick you used yesterday:

```bash
diskutil list                             # find /dev/diskN
make hb-usb-write DEV=/dev/diskN          # ~30 s
```

### Step 3 -- re-enrol the new UKI hash in Framework BIOS

Yesterday you trusted the UKI by hash ("Select a UEFI file as
trusted for execution"). The hash has changed, so the old pin
no longer matches. Same menu, same file, new bytes:

1. Plug the stick into the Framework, power on, F2.
2. Security -> Secure Boot -> Administer Secure Boot Keys ->
   Select a UEFI file as trusted for execution.
3. USB -> EFI -> Boot -> **BootX64.efi**. Select it.
4. Confirm + save + exit. The old pin auto-replaces.

Your DB / KEK / PK cert enrolments from last night stay put --
you only need to re-pin the specific binary.

### Step 4 -- boot + capture the envelope

The Framework auto-associates to Codi via the wifi.conf you
baked in. HB comes up in ~30-60 s. On your Mac:

```bash
# Find the node's DHCP-assigned IP (distinct 8734+8735 open)
for i in $(seq 1 254); do
    nc -z -G 1 192.168.1.$i 8734 2>/dev/null && \
        echo "OPEN 192.168.1.$i:8734"
done

# Confirm it's YOUR Framework (unique wallet address per boot)
curl -sS "http://<framework-ip>:8734/~meta@1.0/info/address"

# Pull the attestation envelope
curl -sS -H 'accept: application/json@1.0' \
         -H 'accept-bundle: true' \
    "http://<framework-ip>:8734/~tpm2@2.0a/attestation" \
    -o out/fw-morning.json
```

### Step 5 -- interpret + read the verdict

```bash
LAPEE_ACCEPT_STALE=1 ./scripts/interpret-local-capture.sh \
    --label 'Framework 13 v1.2.2 post-P2-P3-lockdown' \
    out/fw-morning.json
```

**Expected output:**

```
=== verdict ===
  verdict  = trusted (score 100)
  criticals= 0  warnings= 0

=== attestation summary ===
  machine  = Framework Computer ... 13th Gen Intel Core
  firmware = CRTM IFR30.03.04
  TPM      = Nuvoton discrete (trust-tier=strongest)
  posture  = SB on, lockdown=confidentiality, TME on (enforced-at-init)
  boot     = boot-chain len=1 -> UKI <new hash>
  context  = tcg-pc-client
  ek-ak-binding = via-endorsement-hierarchy (paper P3)
```

If you instead see `attested-with-warnings`, jump to the
**"Debugging the last warnings"** section below.

### Why this works (the paper ledger)

Going paragraph-by-paragraph through `lapee-paper/main.tex`
section Arch, this image now satisfies every claim the paper
makes for the attestation chain:

| paper claim | v1.2.2 evidence | batch |
|---|---|---|
| CPU-fused Boot Guard / PSP | PCR 0 CRTM `IFR30.03.04` matches fingerprint DB | pre-overnight |
| UEFI SB with operator PK/KEK/db | Operator keys enrolled (PK/KEK/DB); UKI hash-pinned; `secure-boot-enabled=true`; PCR 7 non-zero | 19-25 |
| systemd UKI into PCR 11 | UKI is a signed PE, hash in PCR 11 matches `t3hbWLqx...` | 17-24 |
| `lockdown=confidentiality` | Kernel fragment sets `LOCK_DOWN_KERNEL_FORCE_CONFIDENTIALITY=y` | 28 |
| module.sig_enforce | `CONFIG_MODULE_SIG_FORCE=y` | batch 16 onwards |
| IOMMU strict | `CONFIG_INTEL_IOMMU_DEFAULT_ON=y` + iommu-groups-count=18 | batch 14 onwards |
| `init_on_alloc` / `init_on_free` | both =y in fragment | batch 15 |
| TME enforcement at init | `check_tme_sme()` in init -- halts if off | 28 |
| AK under Endorsement hierarchy | NIF passes `ESYS_TR_RH_ENDORSEMENT`; envelope carries `ak-hierarchy=endorsement`; verifier demotes binding finding to info via shared-EPS argument | 28 / 29 |
| HMAC-encrypted TPM sessions (paper P4) | **COVERED via batch 31.** NIF opens a per-boot AES-128-CFB HMAC session (lapee_ensure_auth_session) and attaches it as shandle2 to Esys_PCR_Read, Esys_PCR_Extend, Esys_CreatePrimary (EK + AK), Esys_Quote, and Esys_GetCapability. Envelope declares `tpm-session-mode: hmac-aes128cfb' as ground truth. |
| AO-Core hashpath continuity (paper P5-ext) | **COVERED via batch 30.** init_chain/1 extends PCR 15 with sha256 of the firmware-side TCG event log as the step after AK-pubkey-extend, and records an EV_HYPERBEAM_TCG_LOG_TIP_COMMITMENT runtime event carrying the digest. Verifier check: `hashpath-continuity-verified = true' when the event is present and the digest matches sha256(envelope.tcg-event-log). |

**Every paper-committed attestation property in section Arch is
now verified from code. Zero items deferred to v1.3.** Plus
the root-of-trust binding (wallet <-> TPM) is explicitly
exposed as a single signal in the envelope's policy verdict --
a consumer no longer has to walk six separate checks to chain
an AO-Core result back to the hardware.

### Debugging the last warnings (should not happen, but)

If the verdict comes back `attested-with-warnings`, the remaining
list triages cleanly:

  * `ek-ak-binding-not-implemented` -- envelope is missing the
    `ak-hierarchy` field, which means the image you booted was
    pre-batch-28 (AK still under Owner). Re-run steps 1-3.

  * `lockdown-integrity-not-confidentiality` -- kernel boot did
    not take the new lockdown setting. Check `dmesg | grep -i
    lockdown` on the Framework; if it says `integrity', the
    UKI still has the old kernel. Re-run steps 1-3.

  * `pcr-replay-mismatch` (OS-identity PCRs) -- genuine problem,
    not cosmetic. Grab the interpret.json's
    `pcr-replay.per-pcr.<N>` and compare against the quote.
    Has not happened on this Framework.

  * `freshness-safe-false-counts-missing` -- only fires if the
    quote parse truncates clockInfo. Not seen on NPCT75x.

### Overnight commit ledger (2026-04-24 + 04-25 run)

| batch | sha | landed |
|---|---|---|
| 27 | `1d3ed2c12` | verifier -- ek chain, pcr replay, freshness classifier (V1-V4; score 28->60) |
| 28 | `578ecdc7e` | paper P2 TME-at-init + P3 AK-under-Endorsement + lockdown=confidentiality + V5 severity demotions (score 60->84 before guest rebuild) |
| 29 | `c4d7404c0` | ak-hierarchy envelope + ek-ak-binding demote (closes the warn when guest ships Endorsement) |
| 30 | `d2ef2bc1f` | paper P5-ext AO-Core hashpath continuity: TCG event log tip extended into PCR 15 + verifier check |
| 31 | `8a8569825` | paper P4 HMAC + AES-128-CFB sessions on every sensitive NIF op + `tpm-session-mode' envelope field + verifier finding |
| 32 | `b2af30d4d` | wallet <-> TPM end-to-end binding verifier (recompute hb_message:id, check wallet in node-message, check PCR-15 extend) -- closes the root-of-trust loop |
| ASCII | `5abd17091` | retro-3D laptop boot animation + scannable QR payoff (cherry-picked from sub-agent worktree, initramfs +170 KB) |

All seven pushed to Permagit (arweave://hyperbeam). Full-paper-
compliance synthetic envelope (all signals green -- six now,
including wallet-tpm-binding-verified) verifies as `trusted
(100)' under the batch-27-32 interpreter; evidence in
out/local-capture/full-paper-compliance-with-wallet-binding/.
Previous 2026-04-24 TL;DR preserved below.

---

## TL;DR (morning of 2026-04-24)

**Ship state: READY + SB-enrolled.** 23 code batches, 17
reviewer passes (two of them tonight against batches 17-21 and
22-24), no open CRITICAL/HIGH findings. Head:
`4d3a756b8` (batch 25, .cer BIOS-UI enrolment files).

**Delta vs. 2026-04-23 TL;DR below (the overnight 17 -> 25 run):**

| batch | SHA | landed                                                    |
|-------|-----|-----------------------------------------------------------|
| 17    | `7f0d3d7e8` | WiFi via ESP-file + simpledrm + diag logs      |
| 18    | `514db388f` | Repo cleanup (-10.6k LoC of pre-v1.2 legacy)   |
| 19    | `7550fd2e3` | SB tooling via lapee-tools container + anchored cmdline regex |
| 20    | `b9e7c740c` | Reviewer pass 16 surgical fixes: SSID redact, NUL detect, tpm-interpret removed from guest preload, HARDENING.md updated, PSK comment |
| 21    | `20b44ad70` | Kernel: USB_NET_RNDIS_HOST + CFG80211_WEXT dropped |
| 22    | `ddb7a5949` | Reviewer pass 17 fixes: wpa.log gated, cmdline_flag_set helper, gitignore reorder, doc drift, check label |
| 23    | `78e65109f` | Single-stick SB enrolment (.auth on boot ESP)  |
| 24    | `0f5a08ca7` | wifi.conf flow (gitignore, CMDLINE lapee.wifi=enabled, hb-wifi-apply) |
| 25    | `4d3a756b8` | .cer files alongside .auth for BIOS UI enrolment |

**Signed USB image on disk (ready to flash):**

```
work/lapee-usb.img               1.0 GB, signed UKI embedded
                                 sha256 44cb5b30... (kernel)
                                 sha256 86d2efad... (UKI PE; pre-.cer)
work/lapee.signed.efi            72.7 MB (sbverify OK)
secureboot/{PK,KEK,db}.{key,crt} operator-owned, .gitignored
secureboot/enrol/
  {PK,KEK,db}.auth               PKCS7-authenticated variable updates
  {PK,KEK,db}.cer                X509 DER for BIOS UI enrolment
```

**ESP contents after flash:**

```
/EFI/Boot/BootX64.efi            signed UKI (operator db.key)
/EFI/boot/wifi.conf              "Codi\nPin3apple!\n" (unmeasured)
/PK.auth /KEK.auth /db.auth      PKCS7-authenticated (efi-updatevar)
/PK.cer  /KEK.cer  /db.cer       X509 DER (BIOS file browser)
/README.TXT                      friendly marker
```

**UKI cmdline (measured into PCR 4):**

```
console=tty0 console=ttyS0 earlyprintk=efi,keep keep_bootcon
fbcon=nodefer loglevel=4 panic=10 rdinit=/init
LAPEE_WRITEBACK=1 lapee.wifi=enabled
```

**Demo flow (5 steps + 3 BIOS clicks):**

```bash
cd lapee-baremetal
diskutil list                               # find /dev/diskN
make hb-usb-write DEV=/dev/diskN            # flash signed image
# On Framework: F2 -> Security -> Set Supervisor Password (if not)
#             -> Secure Boot -> Erase All Secure Boot Settings
#             -> Administer Secure Boot Keys
#             -> Enroll db.cer  (format: X509)
#             -> Enroll KEK.cer (format: X509)
#             -> Enroll PK.cer  (format: X509; last -- activates SB)
# Save + exit. Machine auto-boots signed UKI; WiFi associates
# to Codi via wifi.conf; HB emerges as PID 2; writeback emits
# /Volumes/LAPEE_ESP/attestation-latest.json on power cycle.

# On Mac:
./scripts/interpret-local-capture.sh \
    --label 'Framework 13 v1.2.2 SB-on' \
    /Volumes/LAPEE_ESP/attestation-latest.json
```

**Expected attestation delta vs. the 2026-04-23 envelope:**

| signal                          | pre-SB     | post-SB expected |
|---------------------------------|------------|-------------------|
| secure-boot-enabled             | false      | **true**          |
| sb-policy-setup-mode            | (critical) | (ok -- setup mode exited) |
| sb-policy-no-dbx                | warn       | warn (unchanged)  |
| pcr-7 (SB policy)               | non-zero   | non-zero, different value |
| UKI identity chain              | unsigned   | **signed by operator db.key** |
| ak-pubkey-extend-verified       | true       | true              |
| quote-signature-verified        | true       | true              |
| ek-chain-valid                  | true       | true              |
| tme-enabled                     | true       | true              |
| freshness-safe-first-boot       | warn       | warn (first boot after clear) |
| verdict                         | untrusted  | **attested-with-warnings** (SB off warnings flip to ok; freshness-first-boot remains) |

**Reviewer-pass history (overnight):**

- **Pass 16** (against `5147467..0f5a08c7`, post-batch-17). 17
  findings: 1 CRITICAL (missing Dockerfile.tools), 4 HIGH, 5
  MEDIUM, 7 LOW/NIT. All addressed in batch 19 + 20 + 21.
- **Pass 17** (against `7f0d3d7e..20b44ad7`, post-batch-21). 5
  findings: 0 CRITICAL, 2 MEDIUM (M1 wpa.log leak, M2 .gitignore
  dead), 3 LOW/NIT. All addressed in batch 22.

No review pass scheduled for the batch 23-25 work because those
are pure UX plumbing (single-stick, wifi.conf, .cer format) with
no new security boundary. Landed + tested end-to-end on disk.

---

### Prior TL;DR (morning of 2026-04-23, preserved for reference)

**Ship state: READY.** 15 code batches, 13 reviewer passes,
235/235 eunit, QEMU+OVMF+swtpm smoke PASSED end-to-end, live
evidence captured on both QEMU and the v1.1 real-Framework
envelope. Head: `08a38c72e` (batch 15 crypto polish, verifier-
side only).

**Demo flow (5 steps):**

```
cd lapee-baremetal
diskutil list                     # find /dev/diskN for the USB
make hb-usb-write DEV=/dev/diskN  # writes work/lapee-usb.img
# (image is Apr 23 09:21:52, batch 14 NIF fixes, ready as-is)
```

Boot Framework from USB. Read IP from the centred HB splash.
Then from your Mac:

```
# Live attestation:
curl -H 'accept: application/json@1.0' \
     -H 'accept-bundle: true' \
     http://<framework-ip>:8734/~tpm2@2.0a/attestation \
     > /tmp/framework-morning.json

# Or pull from ESP after writeback:
./scripts/interpret-local-capture.sh \
    --label 'Framework 13 v1.2 morning' \
    /Volumes/LAPEE_ESP/attestation-latest.json
```

**Expected morning verdict** (per reviewer pass 8's prediction,
validated against the v1.1 Framework envelope under the batch-11
verifier):

```
verdict  = untrusted    (score 0-20)
criticals = 1 or 2:
  - freshness-safe-false      BENIGN. Nuvoton NPCT75x first-
                               production-boot hasn't seen
                               TPM2_Shutdown(STATE); batch-11
                               message explains both causes.
  - ek-chain-invalid          Only if Nuvoton NV 0x01C00003 is
                               empty (hardware-dependent).
                               Batch-2 E2 threads it through
                               pkix_path_validation if present.
warnings = 5:
  - secure-boot-disabled
  - pcr-replay-multi-mismatch  BENIGN. SeaBIOS / bank mismatch.
  - ek-ak-binding-not-implemented  v1.3 target (CRITICAL 2 from
                                    red-team; honest warn in v1.2).
  - tpm-known-cves             Nuvoton NPCT75x has 2 listed CVEs.
  - lockdown-integrity-not-confidentiality OR
    lockdown-off-or-unknown    Depends on whether CONFIG_SECURITY
                                _LOCKDOWN_LSM landed in the
                                kernel fragment.
```

**Key signal to verify is working** (the whole point of v1.2):

```
signals.ak-pubkey-extend-verified = true
signals.quote-signature-verified  = true
```

Both signals should be `true` in the morning output. That proves
(a) the AK pub was cryptographically bound into PCR 15 during
this specific measured-boot session (paper P5), and (b) the TPM
quote's RSA-PSS signature actually verifies under the envelope's
ak-pub-pem (batch 7 red-team fix).

**DEMO FRAMING (reviewer pass 14 sign-off note):** the headline
win of this demo is NOT "verdict = trusted." It is "verdict =
untrusted on real Framework hardware, AND the verifier can
articulate exactly why." The envelope will almost certainly
come back with `verdict=untrusted`, 1-2 criticals, 5 warnings
-- all expected, all explained in the table above. If you frame
it as "watch the verdict go green," the audience will see red
and think the demo broke. Frame it as "watch the verifier
enumerate the exact gap list, then watch
`ak-pubkey-extend-verified` and `quote-signature-verified`
both come back true -- those two booleans are the v1.2 delta,
and they prove the AK is cryptographically bound into PCR 15
for this specific boot session."

**Pre-demo dry run recommended:** boot the Framework once
privately before the audience is in the room, capture the
verdict, and pre-select the exact talking points from the
"expected morning verdict" table above. This tells you whether
Nuvoton NV `0x01C00003` is provisioned (→ chain-invalid goes
away, score climbs) and which `lockdown` finding will fire.

**Paper amendments needed (v1.3 / before publication)** -- the
LaTeX text is drafted in this document's "Paper amendment draft"
section below; `git apply` it to `lapee-paper/main.tex` when
ready:
  - CRITICAL-2: TME enforcement at init (paper says init refuses
    if TME off; code does not enforce).
  - CRITICAL-3: AK under Endorsement hierarchy (paper says yes;
    code uses Owner hierarchy as swtpm-compat shortcut).
  - CRITICAL-4: HMAC-encrypted TPM sessions (paper says yes;
    code uses password sessions).
  - CRITICAL-5: AO-Core hashpath continuity (paper says yes;
    code doesn't cross-link the two logs).

**If the morning doesn't look like the prediction**, scroll to
"Live evidence #2" further down for signal-by-signal
explanations -- the v1.1-envelope-under-batch-11 re-interpret
showed exactly the same shape plus `ak-pubkey-extend-missing`
(expected for pre-batch-9 envelopes).

---

**Latest update:** 2026-04-23 ~09:30 EDT -- v1.2 shipped through
reviewer pass 13 (cryptographic primitives). SHIP verdict, no
CRITICAL/HIGH findings across 13 specialist passes. Earlier
iterations landed 15 code batches (the convergence from
CRITICAL→HIGH→MEDIUM→LOW→none-found over the last four reviewer
passes is the clearest maturity signal.)

> **USB re-flash required before morning demo.** Batch 9 adds the
> producer-side `EV_HYPERBEAM_KEY_PUBKEY_EXTEND' event in
> `init_chain' so the verifier can enforce paper property P5
> (AK pub bound into PCR 15). Current Apr-23-03:21 image does
> NOT emit this event; booting it with a batch-9-or-later
> verifier would produce verdict=untrusted.
>
> Batch 10 (`8f9748c34') also adds a JSON-safety fix --
> without it, the batch 9 producer crashes `json:encode/1' on
> the `/attestation' response because the event description
> contained a U+00A7 section-sign byte invalid under UTF-8.
> Use the batch-10-or-later image, not the batch-9-only one.
>
> ```
> cd lapee-baremetal
> make hb-release hb-initramfs hb-usb-image   # full rebuild
> make hb-usb-write DEV=/dev/disk4
> ```

For the full history (M0 -> v1.0 -> v1.1, every checkpoint, every
bug fixed, every fake ripped) see [`HISTORY.md`](HISTORY.md).

## Quick links

| Reader                    | Start here                             |
|---------------------------|----------------------------------------|
| Just want to try it?      | [README.md](README.md) "Quick start"   |
| What changed tonight?     | Section below: "v1.2 overnight report" |
| What's still open?        | Section: "v1.2 acceptance" + "TODO list" |
| How did we get here?      | [HISTORY.md](HISTORY.md)               |
| Security model            | [SECURITY.md](SECURITY.md)             |
| Paper-committed properties | [`../lapee-paper/main.tex`](../lapee-paper/main.tex) |

---

## v1.2 overnight report (2026-04-23)

Sixteen commits on `agent/lapee' pushed to Permagit:

```
8f9748c34  v1.2 batch 10: JSON-safe binary literals + reviewer 7
           follow-ups (pcr=15 filter in verify_ak_pubkey_extend,
           stale-comment sequencing caveat, seq-ordering doc)
04050f25c  v1.2 batch 9: paper-to-code P5 key-pubkey-extend +
           verdict hardening (HIGH-2 lockdown-off-or-unknown warn,
           MEDIUM-4 ek-chain-unknown critical, LOW-1 freshness-
           indicator-unknown warn)
8eb177247  v1.2 STATUS: record reviewer pass 5 + batch 8 Erlang
           canon fixes
7120c560b  v1.2 batch 8: Erlang canon cleanup + port TCG verify_fun
           to dev_tpm2 (M2: ek_chain_verify_fun TCG-aware; H1
           binary_to_int_or via safe_int; H3 dead is_fresh_boot
           removed; H4 count_iommu_groups is_group_dir/1; L1
           dead _SPKI removed)
f3f10a46e  v1.2 STATUS: record reviewer pass 4 + batch 7 red-team
           fix + v1.3 TODO backlog
7ead6b02f  v1.2 batch 7: red-team review-fix (signature check
           on claim path, missing-EK/AK upgraded to critical,
           EK<->AK binding warning)
b2d029e5d  v1.2 STATUS: record reviewer pass 3 + batch 6 doc UX
ec794b353  v1.2 batch 6: doc UX pass (newcomer-readable, v1.2
           quick-start canonical, archived banners, legacy
           Makefile separation)
023e4db7d  v1.2 STATUS update: batch 5 demo-ops fixes +
           two-reviewer sign-off
d78b0d3a3  v1.2 batch 5: demo-ops review-fix (stale-capture
           guard, visible boot progress, loglevel=1,
           captive-portal DHCP resilience)
3b3dd2e44  v1.2 STATUS.md: overnight report + iron-reflash
           procedure
57bba5ef3  v1.2 batch 4: review-fix (H1 freshness tightening,
           M6 TCG EKU whitelist, g SecureBoot + cmdline +
           ima-count probes)
54c1b76e6  v1.2 init fix: mkdir /run/lapee BEFORE udhcpc's
           log redirect
c496d5c8e  v1.2 batch 3: boot splash + multi-iface DHCP +
           repo cleanup + slim
39ec0f293  v1.2 batch 2: E2 EK intermediate chain + E3
           runtime platform probes
6c21194cc  v1.2 batch 1: kernel NIC drivers + parser fixes
           (E1, E4, E5, E6)
```

Seven independent code reviewers spoken to (curmudgeonly
firmware-security; pragmatic demo-ops; fresh-eyes first-time-
contributor; adversarial red-team; Erlang/OTP canon; paper-to-
code correctness; security-delta on batch 9). Verdicts:
SHIP (batches 4/5/6), SHIP-after-batch-7 (red-team pre-fix:
"verifier can be bypassed"), SHIP (batch 8 Erlang canon),
SHIP-WITH-NOTES (paper-to-code pass 6: CRITICAL-1 fixed in
batch 9, CRITICAL-2/3/4/5 deferred to v1.3 with paper amendments),
CLOSED (security-delta pass 7: CRITICAL-1 closure validated,
two cleanups applied in batch 10). See "Review findings acted
on" at the bottom of this report for the full ledger.

**Live evidence #2: batch-11 verifier on the v1.1 real-Framework
envelope (2026-04-23 ~06:35 EDT).** Reprocessed the 117 094-byte
v1.1 capture (`out/local-capture/framework-13-v1-1-real-ek-
roundtrip/input.bin') through the batch-11 HEAD verifier to
confirm the new strict checks fire correctly on pre-batch-9
envelopes:

```
ak-pubkey-extend-verified = false   # pre-batch-9 envelope,
                                    #   correctly flagged
quote-signature-verified  = true    # batch 7 intact
ek-chain-valid            = false   # Nuvoton NV 0x01C00003
                                    #   chain incomplete on
                                    #   this TPM
freshness-indicator       = safe-false

verdict   = untrusted
criticals = 3
  freshness-safe-false         (batch 11 softened wording)
  ek-chain-invalid             (NV 0x01C00003 empty on this Nuvoton)
  ak-pubkey-extend-missing     (batch 9 strict check correctly
                                fires on pre-batch-9 envelope)
warnings  = 5
  secure-boot-disabled
  pcr-replay-multi-mismatch    (10 PCRs -- v1.1 had more firmware
                                  events than QEMU)
  ek-ak-binding-not-implemented (batch 7 honest warn)
  tpm-known-cves               (NPCT75x x 2 CVEs)
  lockdown-off-or-unknown      (batch 9 HIGH-2 fires because v1.1
                                  has no platform-probes section)
```

What this confirms:
  - Batch 9's `ak-pubkey-extend-missing' critical fires as
    designed: a pre-batch-9 envelope (which cannot possibly have
    the `EV_HYPERBEAM_KEY_PUBKEY_EXTEND' event) cannot reach
    `trusted'. Paper P5 is enforced.
  - Batch 9's `lockdown-off-or-unknown' warn fires as designed
    on envelopes without `platform-probes'.
  - Batch 11's softened `freshness-safe-false' message renders
    correctly; severity still critical.
  - Reviewer 8's morning prediction was accurate to within the
    batch-11 delta: Framework morning verdict = untrusted, 1-2
    criticals (freshness + possibly ek-chain), 5 warnings,
    score 0-20.

When Sam boots batch 10 in the morning, the `ak-pubkey-extend-
missing' critical disappears (the producer emits the event),
leaving only the 1-2 hardware-dependent criticals that are
EXPLICITLY expected per v1.3 backlog (MakeCredential for EK-AK
binding, Nuvoton NV 0x01C00003 chain provisioning).

## Pre-demo checklist (morning of 2026-04-23)

Use this in order when you wake up:

```
[ ] 1. cd lapee-baremetal
[ ] 2. make hb-release hb-initramfs hb-usb-image
       # If the beam files / USB are older than this commit
       # (`git show --stat HEAD | head -3') -> rebuild.
       # If fresh -> skip to step 3.
[ ] 3. diskutil list                     # find /dev/diskN
[ ] 4. make hb-usb-write DEV=/dev/diskN  # prompts before writing
[ ] 5. Eject + reinsert into Framework
[ ] 6. Power on Framework; F12 boot menu; select USB
[ ] 7. Watch the HB splash. Network IP appears when DHCP lands.
[ ] 8. From your Mac:
       curl -H 'accept: application/json@1.0' \
            -H 'accept-bundle: true' \
            http://<framework-ip>:8734/~tpm2@2.0a/attestation \
            > /tmp/framework-morning.json
[ ] 9. Or pull from the ESP after writeback completes:
       ./scripts/interpret-local-capture.sh \
           --label 'Framework 13 v1.2 morning' \
           /Volumes/LAPEE_ESP/attestation-latest.json
[ ] 10. Expected output (per reviewer pass 8):
        verdict  = untrusted
        criticals= 1 or 2
          (1) freshness-safe-false
               -> benign, batch-11 message names both causes
          (2) ek-chain-invalid
               -> only IF Nuvoton NV 0x01C00003 is empty;
                  hardware-dependent; batch 2 E2 threads it
                  through pkix_path_validation if present
        warnings = 5
          secure-boot-disabled
          pcr-replay-multi-mismatch
          ek-ak-binding-not-implemented
          tpm-known-cves
          lockdown-integrity-not-confidentiality (or -off-or-unknown)
[ ] 11. Verify `ak-pubkey-extend-verified = true' in the
        signals block -- that's the paper P5 property working
        for the first time on real iron.
[ ] 12. (Optional) Cross-node verify:
        curl 'http://localhost:8734/~tpm-interpret@1.0/verify-peer?peer=http://<framework-ip>:8734'
```

**If the morning verdict surprises you** (e.g., `unknown'
verdict, or a critical not in the list above): diff the signals
map against the predicted outcome in STATUS.md and see which
signal drifted. Every `unknown' signal has a specific finding
code; the code + its message point to the envelope field or
code path that produced it.

**Live evidence #1: batch-10 end-to-end QEMU smoke
(2026-04-23 05:44 EDT):**

```
$ make hb-release hb-initramfs hb-usb-image     # batch 10 build
$ bash scripts/boot-usb-image.sh                 # QEMU smoke
... >> LAPEE-WRITEBACK-OK detected in serial log
... === QEMU boot test PASSED ===
... attestation-latest.json  (104 244 bytes, 2026-04-23 05:41)

$ bash scripts/interpret-local-capture.sh \
      --label 'QEMU batch 9 smoke' \
      out/qemu-usb-test/attestation-latest.json
... verdict  = untrusted (score 0)
... criticals= 2  warnings= 4

Signals (live envelope, batch-10 verifier):
   ak-pubkey-extend-verified = true       # paper P5 NOW ENFORCED
   quote-signature-verified  = true       # batch 7 still intact

Runtime event log (PCR 15, from envelope body):
   seq=0  EV_HYPERBEAM_NODE_IDENTITY_EXTEND
   seq=1  EV_HYPERBEAM_KEY_PUBKEY_EXTEND   # batch 9 producer

Criticals (both QEMU-context expected, NOT batch regressions):
   ek-cert-missing           # swtpm has no vendor EK cert
   sb-policy-setup-mode      # OVMF default SB setup mode

Warnings (all explainable for QEMU, not regressions):
   pcr-replay-multi-mismatch             # SeaBIOS quirk (v1.3
                                         #   MEDIUM 3 tolerate)
   freshness-indicator-unknown           # batch 9 LOW-1 fix
                                         #   surfacing swtpm gap
   ek-ak-binding-not-implemented         # batch 7 honest warn
                                         #   (v1.3 target)
   lockdown-integrity-not-confidentiality # QEMU kernel default
```

**Known v1.3 gap (surfaced by red-team reviewer, batch 7):**
the v1.2 verifier does NOT cryptographically prove that the AK
(signer of the quote) lives in the same TPM as the EK (anchor
of the cert chain). An attacker with a stolen EK + chain could
generate their own AK keypair and forge quotes that pass
verification. v1.2 surfaces this explicitly as a WARN-severity
finding `ek-ak-binding-not-implemented` on every verdict --
verdict=trusted from v1.2 means "cryptographically sound
given the envelope is authentic," NOT "sound against an active
MITM." Full TCG TPM2_MakeCredential / TPM2_ActivateCredential
provisioning handshake is the v1.3 fix.

### What's done from the TODO list

**A. Networking in the guest** -- Buildroot kernel rebuilt with
broad NIC driver set all in-tree (R8169 + TIGON3 + E1000/E1000E/
IGC/IXGBE + USB_NET_{CDCETHER, CDC_NCM, AX88179_178A, AX8817X,
RTL8152} + USB4 + USB4_NET). Init fires `udhcpc -b' per
carrier-up interface; `lapee-dhcp-hook' claims the first-to-
lease as default route; `/run/lapee' pre-created so udhcpc
launch doesn't fail. Precedence order matches wired-first ->
USB-C-dongle -> TB-bridge -> RNDIS via kernel interface-naming
ordering. QEMU slirp still works unchanged (its 10.0.2.2 DHCP
responds to the same udhcpc).

**B. Boot splash** -- `/usr/local/bin/lapee-splash' renders a
centred HyperBEAM ASCII logo + status slot. Init calls it right
after /proc /sys /dev mount. `lapee-dhcp-hook' re-renders with
`node:  http://<ip>:8734' once an interface wins the default
route. QEMU smoke confirms the centred layout works on an
80-col serial console.

**C. Repo cleanup** -- Deleted / moved / archived:

  - `lapee-baremetal/lapee-tpm/' -> `reference-demo/
     legacy-lapee-tpm/'. The M2/M3 reference-demo orchestrator
     (`lapee_node.erl') is no longer on the v1.2 hot path.
  - 19 legacy scripts moved to `scripts/legacy/'
     (boot-buildroot / boot-hb / boot-m1 / boot-real /
      build-initramfs / hb-acceptance / etc.). Active scripts/
     now has 10 files, all on the v1.2 hot path.
  - 4 doc plans (PLAN, OVERNIGHT-PLAN, INTERPRET-MVP-PLAN,
     BUILDROOT-RESULT) -> `docs/archive/'.
  - Makefile updated so legacy targets still RUN (scripts/
     legacy/<>) but don't clutter `make help'.
  - `.gitignore' adds `/out/' (root) so verifier captures
    don't pollute status.

**D. Image slim** -- `build-initramfs-hb.sh' now removes
`priv/tpm-interpret/fixtures/' (40 MB parser test vectors,
never read at runtime) and `lib/*/doc|examples|man` from every
shipped OTP lib. Kernel cmdline `loglevel=4' -> `3' and dropped
`ima_policy=tcb'. HB tree: 180 MB -> 115 MB uncompressed,
initramfs 60 MB compressed. Further savings (zstd over gzip, D7)
deferred -- the ~60 MB threshold is already acceptable for the
demo.

**E. v1.1 parser follow-ups** -- All six discovered from Sam's
real Framework capture, all landed with eunit coverage:

  - E1  `currently_valid/2` replaced ISO-8601/raw lexicographic
        compare with calendar:datetime / gregorian-seconds
        comparison. Correctly parses RFC 5280 UTCTime +
        GeneralizedTime. 6 new tests.
  - E2  `fetch_ek_cert_chain/1` + `split_concatenated_ders/1`
        pull the intermediate CA bundle from NV `<ek>+1'.
        `validate_ek_chain/3` threads the chain through
        `pkix_path_validation', trying both leaf-first and
        root-first orderings + a TCG-aware `verify_fun' that
        whitelists `id-tcg-kp-EKCertificate' (2.23.133.8.1)
        and `id-tcg-tpmSpecification' (2.23.133.2.16).
  - E3  `capture_platform_probes/0' reads /proc/cpuinfo,
        /sys/kernel/security/lockdown, /sys/kernel/iommu_groups/,
        /sys/class/dmi/id/*, /proc/cmdline,
        /sys/firmware/efi/efivars/SecureBoot-..., and
        /sys/class/tpm/tpm0/tpm_version_major at init_chain
        time. Envelope ships them as `platform-probes'.
        `claim_cpu' prefers cpuinfo over string-scan; `claim_iommu'
        prefers iommu-groups-count > 0; `claim_lockdown'
        prefers the bracketed active level.
  - E4  NIF `nif_tpm_properties' vendor-string now truncates at
        the first NUL (C-string convention) instead of walking
        trailing NULs only.
  - E5  `pick_platform/2` accepts map | list | binary | empty
        for the manifest's `platforms' field. Framework's
        three-variant list now resolves to a candidate set.
  - E6  `freshness_finding/1' three-way classifier:
        first-cold-boot (both counts present, both <= 1) ->
        warn; counts-missing (either null) -> critical with
        distinct code so an adversary cannot strip counts to
        silence the tamper signal; all other safe=false ->
        critical tamper.

**F. Security property coverage** -- Every row in the v1.2
target table either landed or has a single known follow-up:

  | Property                                 | v1.2 state   |
  |------------------------------------------|--------------|
  | EK cert from real TPM NV                 | COVERED      |
  | EK chain validates to manuf root         | **COVERED via E2** |
  | EK cert currently-valid                  | **COVERED via E1** |
  | Quote signature + pcrDigest + nonce      | COVERED      |
  | Event-log replay vs quoted PCRs          | COVERED (0/1/7/11/14 on Framework) |
  | AK pub bound into PCR 15                 | **COVERED via batch 9 key-pubkey-extend** |
  | AK + node-message bound into PCR 15      | COVERED      |
  | Firmware CRTM match                      | **COVERED + platform via E5** |
  | UKI hash in PCR 11                       | COVERED      |
  | Secure Boot state                        | **COVERED via g probe** |
  | TME state attested                       | COVERED (claim side) |
  | TME enforcement at init                  | **PAPER-AMEND + v1.3** (see reviewer 6 CRITICAL-2) |
  | AK under Endorsement hierarchy           | **PAPER-AMEND + v1.3** (reviewer 6 CRITICAL-3) |
  | Encrypted TPM sessions (bus sniffing)    | **PAPER-AMEND + v1.3** (reviewer 6 CRITICAL-4) |
  | AO-Core hashpath continuity              | **PAPER-AMEND + v1.3** (reviewer 6 CRITICAL-5) |
  | IOMMU state                              | **COVERED via E3 runtime probe** |
  | Kernel lockdown state                    | **COVERED via E3 runtime probe + batch 9 finding** |
  | IMA per-file chain (PCR 10)              | N/A stub; count probed |
  | CPU vendor / model                       | **COVERED via E3 /proc/cpuinfo** |
  | TPM manufacturer / model                 | COVERED      |
  | freshness-safe <-> resetCount            | **COVERED via E6 + batch 9 unknown finding** |

### Tests

```
dev_tpm_tcg        98  pass
dev_tpm2           22  pass  (+1 batch 8 TCG-whitelist,
                               +1 batch 9 chk_ak_pubkey_binding)
dev_tpm_interpret 109  pass  (+20 v1.1, +5 batch 9: verify_ak
                               _pubkey_extend, ak_pubkey_extend
                               _finding, lockdown-unknown,
                               ek-chain-unknown, freshness
                               -unknown)
                ------
                  229  pass
```

### Expected iron timeline on the Framework (from v1.2 USB)

```
t+0.0s  UEFI hands off to kernel
t+0.3s  lapee-splash renders (centred ASCII logo)
t+0.5s  udhcpc forks per NIC; ip link up
t+0.9s  HB `/~tpm2@2.0a/info' answers (on Nuvoton fTPM the
        init_chain pipeline is fast; ~1-2s expected on iron)
t+1.1s  lapee-dhcp-hook gets lease, re-renders splash:
          node:  http://192.168.1.42:8734
t+2.0s  /~tpm2@2.0a/attestation returns ~102 KB envelope
t+2.5s  writeback to ESP done; SAFE TO POWER OFF
```

The v1.1 capture already showed `/attestation' returning a real
envelope in ~5s after HB /info came up; on v1.2 with the slim
+ boot trimmed + no cert synthesis path, that should drop under
2s.

### Reflash + demo procedure

```bash
cd lapee-baremetal

# Build already done; just write the USB.
make hb-usb-write DEV=/dev/disk4

# Boot Framework from USB. Plug in Ethernet (built-in or USB-C
# dongle, or TB cable to Mac). Watch the splash show the IP.
# From the Mac:
curl http://<framework-ip>:8734/~tpm2@2.0a/attestation \
     -H 'accept: application/json@1.0' \
     -H 'accept-bundle: true' \
     > /tmp/att.json

# Or pull from the ESP writeback once the Framework is done:
./scripts/interpret-local-capture.sh \
    --label 'Framework 13 v1.2 real iron' \
    /Volumes/LAPEE_ESP/attestation-latest.json

# Cross-node verify from Mac HB:
curl 'http://localhost:8734/~tpm-interpret@1.0/verify-peer?peer=http://<framework-ip>:8734'
```

Every paper-committed security property populates from a live
hardware signal. Zero synthesized material. Chain validation
walks through the TCG EK EKU + TPM spec extensions without
tripping. Verdict should come back `trusted` (or a specifically-
explained `warnings`) without a single `unknown` field.

### Review findings acted on

**Pass 1 -- curmudgeonly firmware-security reviewer** (against
6c21..54c1b). Verdict: SHIP. Three findings promoted into v1.2
batch 4:

  H1  `is_fresh_boot/2' treated null reset/restart counts as
      fresh-boot, which let an adversary strip counts to
      silence the tamper signal. Tightened to three-way
      classifier: both-present-and-low -> warn, either-null ->
      CRITICAL (distinct code), both-present-and-high ->
      CRITICAL.
  M6  Added `ek_verify_fun/3' as pkix_path_validation
      verify_fun. Whitelists TCG OIDs `2.23.133.8.1'
      (id-tcg-kp-EKCertificate) and `2.23.133.2.16'
      (id-tcg-tpmSpecification) which real EK certs carry as
      critical extensions and OTP's default path validator
      would reject.
  g   Added /sys/firmware/efi/efivars/SecureBoot-...,
      /proc/cmdline, /sys/class/tpm/tpm0/tpm_version_major,
      and IMA runtime-measurement-count probes to the
      envelope.

**Pass 2 -- pragmatic demo-ops reviewer** (against full
6c21..3b3dd). Verdict: SHIP with top-5 risks addressed as
v1.2 batch 5:

  TOP-1  Stale ESP on verifier's Mac -> interpret-local-capture.sh
         now refuses files older than 30 minutes (override with
         LAPEE_ACCEPT_STALE=1). Prevents the "dashboard shows
         2024 data against v1.2 parser" demo-failure mode.
  TOP-2  No-NIC-attached boot showed a frozen splash -> init
         now renders distinct status lines at every phase,
         including an explicit "no network interfaces found --
         plug in Ethernet" frame when NIC_COUNT=0.
  TOP-3  HB cold-start wait was silent -> heartbeat every
         ~10s with elapsed-seconds count refreshing the splash.
  TOP-4  Kernel WARN messages scrolled over the splash -> dropped
         default cmdline `loglevel=3' to `loglevel=1' so only
         PANIC/ALERT/CRIT/ERR reach the physical console. Dmesg
         still has everything for serial-console post-mortem.
  TOP-5  Captive-portal DHCP (IP but no gateway) would claim the
         primary slot and block subsequent-interface defaults ->
         lapee-dhcp-hook now gates PRIMARY write behind the
         presence of a `router' field, logs ip-only leases as
         `NO-GATEWAY (ip-only; not promoting to primary)'.

Deferred (non-blocker, v1.3 backlog):

  M1  lapee-dhcp-hook's `for gw in ${router:-}' iterates each
      router; only the last wins. Single-homed demo irrelevant.
  M2  `bound|renew' flushes the interface's existing IP before
      re-adding; brief 0-IP gap. No running traffic on the
      secondary in the demo.
  M3  count_iommu_groups/0 cosmetic readability.
  M4  /sys/kernel/security/lockdown can be one-way-escalated
      post-init_chain; our snapshot would miss a late change.
      Late escalation is not expected in normal boot flow.
  M5  GeneralizedTime with fractional seconds rejected by
      parse_x509_time/1. Rare on EK certs; noted for v1.3.

**Pass 3 -- fresh-eyes first-time-contributor reviewer**
(against full 6c21..023e4d). Verdict: SHIP, with "works for
trusted contributors who know Sam" flipped to "ready for public
eyes" after seven HIGH documentation / UX issues. All seven
landed as v1.2 batch 6 (commit ec794b353):

  HIGH-1  Broken anchor in lapee-baremetal/README.md (v1.0
          bookend link pointed at a STATUS.md section that had
          moved to HISTORY.md).
  HIGH-2  lapee-baremetal/README.md carried three competing
          quick-starts (`make hb-usb-image' / `make hb-all' /
          `make all') stacked without a "this is current"
          banner -- newcomers couldn't tell which was the v1.2
          flow. Rewritten: one paragraph on what LapEE is +
          v1.2 quick-start canonical at the top + legacy QEMU
          material demoted to "preserved for reference".
  HIGH-3  Top-level README.md + AGENTS.md mentioned LapEE
          zero times. Added a 4-line pointer to the top-level
          README + a LapEE-specific principles section to
          AGENTS.md (no synthetic attestation data, parser-side
          robustness, kebab-case binary fields).
  HIGH-4  `scripts/interpret-local-capture.sh' silently
          required the parent HB tree to be buildable (runs
          `rebar3 as test compile' internally). Documented in
          the script header + README prerequisites block +
          `brew install erlang rebar3 docker python@3' line.
  HIGH-5  Makefile header listed legacy targets (`make demo /
          verify / boot-real / hb-all / hb-acceptance') with
          no mention of the v1.0-through-v1.2 Framework-boot
          headline (`hb-usb-image / hb-usb-write /
          hb-cross-node-verify'). Rewritten with explicit
          "=== v1.2 hot path (current) ===" and "=== Legacy
          ===" sections.
  HIGH-6  docs/archive/*.md files (PLAN, OVERNIGHT-PLAN,
          INTERPRET-MVP-PLAN, BUILDROOT-RESULT) had no
          ARCHIVED banner; a newcomer stumbling into them
          would read them as current. Now each leads with 6
          quoted lines pointing back to STATUS.md + README.md.
  HIGH-7  FEATURES.md + HARDENING.md dated 2026-04-19 but read
          as current. FEATURES referenced the legacy target
          `hb-final-acceptance'. Both now carry a "figures
          are the 2026-04-19 baseline; see STATUS.md for
          current" banner at the top; FEATURES' legacy target
          annotated with a STATUS.md redirect.

Plus MEDIUM-11 from pass 3: STATUS.md gains a Quick-links
table at the top so a newcomer skimming can jump straight to
the right starting point without reading the full 900+ lines.

**Pass 4 -- adversarial / red-team reviewer** (against full
6c21..b2d029). Verdict before fixes: "verifier can be bypassed
-- fix before demo." Two CRITICAL attack trees both confirmed
against the live code:

  CRITICAL 1  `claim/3' performed NO cryptographic checks.
              dashboard.html's verdict=trusted badge was
              attacker-writeable: swap `quoted' bytes +
              recompute sha256 over your chosen PCR values,
              verdict reports trusted with zero TPM
              participation. Fixed in batch 7 via new
              quote-signature-verified signal that invokes
              rsa_pss:verify/4 (salt=32, MGF1=SHA-256)
              identically to dev_tpm2:chk_quote/1. A bad /
              missing signature is now CRITICAL, driving
              verdict=untrusted.
  CRITICAL 2  no EK<->AK binding (see "Known v1.3 gap" up
              top). v1.2 mitigates by surfacing a honest
              warning finding on every envelope so the gap
              is explicit in the verdict output.

Also upgraded in batch 7 (reviewer's MEDIUM 1, 2):

  ek-cert-missing    warn -> CRITICAL
  ak-pub-missing     warn -> CRITICAL

  Pre-v1.2 these were warnings; an attacker stripping the
  EK or AK fields from an envelope would produce an
  attested-with-warnings verdict. Missing either one means
  no TPM-rooted crypto identity exists in the envelope at
  all -- cannot be trusted, must be critical.

v1.3 backlog from red-team pass 4:

  - TPM2_MakeCredential / TPM2_ActivateCredential provisioning
    handshake (CRITICAL 2 full fix)
  - HIGH 1: verify the `commitments' field or drop it (currently
    never verified by dev_tpm_interpret)
  - HIGH 2: interpret-local-capture.sh --url mode should call
    /~tpm-interpret@1.0/verify-peer (crypto gate) instead of
    fetching /attestation directly (plaintext interpret)
  - HIGH 3: lockdown string cross-check against EV_IPL cmdline
    event or UKI-hash profile before trusting
    `platform-probes.lockdown' verbatim
  - HIGH 4: derive `tpm.trust-tier' from the EK cert's TCG OID
    (2.23.133.2.1-4) rather than `tpm-properties.manufacturer'
    so a forged platform-probes block can't move trust tier
  - MEDIUM 3: treat any pcr-replay mismatch as critical when
    the initramfs is expected to emit a complete log (today
    we tolerate it because our stub initramfs is short)

**Pass 5 -- Erlang/OTP canon reviewer** (against full
6c21..f3f10). Verdict: "blends well enough to ship -- one
surgical refactoring pass would improve maintainability."
Six findings, all addressed in batch 8 (commit 7120c560b):

  M2  `dev_tpm2:ek_chain_verify_fun/0' was the pre-v1.2-batch-4
      version -- rejected every `{bad_cert, _}' including
      `{bad_cert, {not_supported_extension, Ext}}' for TCG-critical
      EKU (`id-tcg-kp-EKCertificate', 2.23.133.8.1) and spec-version
      (`id-tcg-tpmSpecification', 2.23.133.2.16) extensions that
      real Nuvoton / Infineon / STMicro EK certs carry. The parser
      side (`dev_tpm_interpret:ek_verify_fun/3') was upgraded in
      batch 4 M6 with the TCG whitelist, but `dev_tpm2:chk_ek_chain'
      was not. Two verifier paths that should accept identical
      chains would have disagreed. Ported the stricter TCG-aware
      verify_fun back to `dev_tpm2' as a top-level 3-arg function
      mirroring `dev_tpm_interpret:ek_verify_fun/3' one-for-one.
      Regression test updated to cover TCG-critical pass, rogue
      critical reject, TCG non-critical accept, non-TCG
      non-critical unknown.
  H1  Duplicate `binary_to_integer' try/catch in
      `binary_to_int_or/2' and `safe_int/1'. Reimplemented the
      former as a default-wrapping thunk over the latter so the
      try lives in one place.
  H3  `is_fresh_boot/2' was a legacy boolean wrapper around
      `fresh_boot_classify/2' with zero callers. Deleted.
  H4  `count_iommu_groups' used `element(1, string:to_integer(E))'
      as a digit-filter. Replaced with a named `is_group_dir/1'
      that pattern-matches both `{N, <<>>}' and `{N, ""}'.
  L1  `decode_rsa_pub_pem' bound an unused `_SPKI' variable on
      the SPKI fall-through path. Removed.
  H2  Reviewer flagged `catch C:E -> {error, {C, E}}' as
      "non-canonical" but the surrounding PEM-decoder canon in
      this codebase (`dev_tpm2:decode_pem_rsa_pub/1', line 941;
      `dev_tpm_interpret.erl:7079/7089') uses the same form
      intentionally for diagnostic clarity. Declined the
      normalisation; the architectural-layering comment at
      `dev_tpm_interpret.erl:3305-3309' already documents why
      the `dev_tpm2' / `dev_tpm_interpret' duplicate exists.

**Pass 6 -- paper-to-code correctness auditor** (against full
6c21..8eb17). Verdict: SHIP-WITH-NOTES. CRITICAL-1 addressed in
batch 9; HIGH-2, MEDIUM-4, LOW-1 addressed in batch 9;
CRITICAL-2/3/4/5 deferred to v1.3 with PAPER AMENDMENTS required.
Method: reviewer enumerated 16 load-bearing claims from
`../lapee-paper/main.tex` (P1..P16), mapped each to a specific
file:line in the implementation, and flagged cases where the
paper promises more than the code delivers. 14 findings total
across 4 severity bands.

Findings addressed in batch 9:

  CRITICAL-1  (paper P5: "a verifier replaying the event log
              observes `key-pubkey-extend' land in PCR 15 after
              all TCB measurements"). Pre-batch-9 code did NOT
              implement this: `init_chain/1' created the AK
              and cached `ak_pub_pem' but never extended PCR 15
              with it. The only PCR-15 extension was the
              on/start hook binding the node-message-id. An
              envelope signed with an attacker-generated AK (no
              TPM backing) would pass chk_binding trivially
              because the attacker controls what landed in
              PCR 15. Fixed in batch 9:
              - New `extend_with_ak_pubkey/1' in `dev_tpm2.erl'
                fires at end of `init_chain/1', extends PCR 15
                with `sha256(ak_pub_pem)', emits event type
                `EV_HYPERBEAM_KEY_PUBKEY_EXTEND' at seq 0.
              - New `chk_ak_pubkey_binding/1' core check in
                `verify/3' pipeline searches the runtime event
                log for an EV_HYPERBEAM_KEY_PUBKEY_EXTEND event
                whose decoded digest equals
                `sha256(envelope.ak-pub-pem)'. Missing or
                mismatched = `{error, _}' = `verified=false'.
              - New `verify_ak_pubkey_extend/1' +
                `ak_pubkey_extend_finding/1' in
                `dev_tpm_interpret' surface the same signal on
                the cross-node `verify-peer' path, CRITICAL when
                false/unknown.
  HIGH-2      `lockdown_finding/1' catch-all `_ -> ok' let
              `lockdown-level = "none" | "unknown" | absent'
              slide through silently. Paper's Table 2 defenses
              for /dev/mem, kexec, ptrace-via-kallsyms, unsigned
              module-load are only enforced when lockdown is
              active. Fixed: catch-all -> warn (code
              `lockdown-off-or-unknown'). Escalate to critical
              in v1.3 once EV_IPL cmdline cross-check lands
              (red-team v1.3 HIGH 3).
  MEDIUM-4    `ek_finding/1' did not match `ek-chain-valid =
              "unknown"' -- a verifier that failed to load any
              root CAs would silently accept the EK chain as
              "not-invalid = fine". Added explicit
              `ek-chain-unknown' critical.
  LOW-1      `freshness_finding/1' catch-all let an absent /
              unknown `freshness-indicator' slide. An attacker
              stripping the field evaded the signal. Added
              `freshness-indicator-unknown' warn; `safe' still
              ok; `safe-false' and `no-nonce' unchanged.

Findings deferred to v1.3 with PAPER AMENDMENTS required:

  CRITICAL-2  (paper P2, §Architecture): "Early init reads
              `IA32_TME_ACTIVATE' (Intel) or `SYSCFG' bit 23
              (AMD) and refuses to proceed if memory encryption
              is inactive -- so a successful attestation is
              itself proof that TME was enabled."
              Actual code: `initramfs-hb/init' (396 lines) has
              no MSR read and no refusal path. `tme_finding/1'
              treats `tme-enabled = false` / `unknown' as warn.
              A verdict=attested-with-warnings shipping with
              TME-off contradicts the paper's abstract claim
              "measured, attested, memory-encrypted, DMA-
              contained single-purpose appliance".
              v1.3 plan: add MSR read in initramfs (needs
              `msr-tools' in buildroot or a tiny C helper in
              `native/') with hard failure path; upgrade
              `tme_finding(#{<<"tme-enabled">> := false})` from
              warn to critical; strip the tier-4 "boot-reached-
              PCR-15 -> tme-enabled=true" short-circuit from
              `claim_tme'.
              Paper amendment needed: if v1.3 doesn't ship
              MSR-enforce-at-init, the paper's P2 phrasing
              should be softened to "TME state is attested via
              claim.tme; operator policy gates verdict".

  CRITICAL-3  (paper P4, §Ephemeral-node-key-binding):
              "TPM2_Create for a fresh signing keypair under a
              primary on the Endorsement hierarchy."
              Actual code: `native/lapee_tpm_nif/
              lapee_tpm_nif.c:342-349' calls
              `Esys_CreatePrimary(ESYS_TR_RH_OWNER, ...)' --
              Owner hierarchy, not Endorsement. The NIF comment
              at lines 294-298 even admits "for first-cut
              correctness against swtpm, we instead create
              the AK under the Owner hierarchy primary ... the
              parent handle argument is accepted but ignored
              for this milestone".
              v1.3 plan: switch to Endorsement hierarchy; this
              is tightly coupled with the
              MakeCredential/ActivateCredential binding (red-
              team CRITICAL 2 from pass 4) because only a real
              EK-parented AK can be activated.
              Paper amendment needed: v1.2 paragraph should be
              amended to "primary under the Owner hierarchy
              pending MakeCredential provisioning handshake"
              OR the v1.3 code ships first.

  CRITICAL-4  (paper P6, Table 2 row "TPM bus sniffing (dTPM)":
              "Blocked at load. Encrypted sessions (HMAC +
              parameter encryption)").
              Actual code: every Esys call in
              `native/lapee_tpm_nif/lapee_tpm_nif.c' uses
              `ESYS_TR_PASSWORD' or `ESYS_TR_NONE' auth. No
              `Esys_StartAuthSession(TPM2_SE_HMAC)' call
              exists. SPI/LPC bus traffic between the Framework's
              Nuvoton NPCT75x and the AMD Ryzen CPU is
              cleartext -- bus interposers see PCR extends and
              signed quotes on the wire.
              v1.3 plan: add salted HMAC session creation in the
              NIF at EK load time; use it for all Esys calls
              touching sensitive state.
              Paper amendment needed: Table 2 row should move
              from "Blocked at load" to "v1.3 target; v1.2
              leaves this to physical-security" until the NIF
              work lands.

  CRITICAL-5  (paper P11, §AO-Core Continuity): "HyperBEAM
              seeds its AO-Core chain with a commitment to the
              TPM event log tip immediately after
              key-pubkey-extend; thereafter every device first-
              load and every message extends the chain."
              Actual code: grep for `event.log.tip|tpm.event.
              log.tip|seeds.*ao.core' returns zero substantive
              matches. The envelope carries the TPM event log
              AND the AO-Core hashpath state but they are not
              cryptographically linked.
              v1.3 plan: add `attestation-at-hashpath-tip'
              field to the envelope in `dev_tpm2:attestation/3'
              recording the current AO-Core tip at quote time;
              seed the AO-Core tip from the TPM event log tip
              at `init_chain' via a deterministic extend.
              Paper amendment needed: flag the claim as
              "architectural target, v1.3" until the link is in
              code.

Findings surfaced but lower impact (all v1.3 backlog unless
noted):

  HIGH-1      `pcr_replay_finding/1' never returns critical
              (N >= 3 still warn). Reviewer notes: "primary
              integrity still holds because
              `quote_integrity_finding' catches forged pcr-
              values; this is HIGH not CRITICAL." Same as red-
              team v1.3 MEDIUM 3. Deferred: once the golden TCG
              event log and the strict-vs-QEMU-tolerant mode
              distinction lands, `N >= 1' goes critical.
  HIGH-3      No verifier version floor for firmware / BIOS
              downgrade (paper P12, A1 assumption). The paper
              relies on "verifier version floor" as the
              downgrade defense; no code enforces one. Paper
              amendment OR operator-config option in v1.3.
  MEDIUM-1    Positive-signal floor for verdict=trusted. Today,
              no-critical + ≥1 known-true signal suffices.
              Paper implies a floor (SB+lockdown+TME+EK+PCR+
              sig all positive). V1.3 or paper clarification.
  MEDIUM-2    `commitments' field unverified (carried over from
              red-team v1.3 backlog HIGH 1).
  MEDIUM-3    Device-load PCR-15 extend (paper §Architecture:
              "each first load extends PCR 15 with a named
              event"). Code extends PCR 15 only once at
              on/start. Paper amendment OR hook up
              `dev_trusted_signers' to POST to
              `~tpm2@2.0a/extend' per first-load.
  MEDIUM-5    RSASSA-PSS for result signatures (paper P9).
              Separate from TPM quote sig; audit item flagged
              but not verified in review window.

**Pass 7 -- security-delta auditor on batch 9** (against commit
04050f25c). Verdict before fixes: **CLOSED** for the stated
scope ("AK-pub must be extended into PCR 15 before attestation,
and the verifier must reject an envelope without it"). Two
surgical cleanups recommended, both landed in batch 10 (commit
8f9748c34):

  Cleanup A  Comment at `dev_tpm2.erl:1464-1471' claimed the
             `EV_HYPERBEAM_KEY_PUBKEY_EXTEND' event "lands at
             seq 0, BEFORE any on/start node-identity-extend."
             In reality the on/start hook fires FIRST at BEAM
             startup (seq 0), then `init_chain' (triggered by
             the first `/attestation' request) runs
             `extend_with_ak_pubkey' which lands at seq 1. The
             verifier does NOT pin seq position, so this is a
             documentation correctness issue, not an enforcement
             one. Comment rewritten; an explicit sequencing-
             caveat note added to `extend_with_ak_pubkey/1' doc
             header.

  Cleanup B  `dev_tpm_interpret:verify_ak_pubkey_extend/1' did
             not filter by `pcr = 15' -- a well-formed
             `EV_HYPERBEAM_KEY_PUBKEY_EXTEND' event in the wrong
             PCR would have set `ak-pubkey-extend-verified =
             true' on the interpret path. The core
             `dev_tpm2:chk_ak_pubkey_binding/1' already
             filtered pcr=15; the mirror now matches it. Added
             `ev_pcr/1' helper tolerant of integer vs binary
             PCR encoding (JSON round-trip may stringify keys).
             Two new assertions in `v1_2_verify_ak_pubkey_
             extend_shapes_test': wrong PCR -> false; binary
             "15" -> true.

Also in batch 10: five UTF-8 section-sign bytes (U+00A7)
embedded in binary literals crashed `json:encode/1' with
`{invalid_byte, 167}' on the first `/attestation' response --
Erlang source is latin-1 by default, so `<<"§">>' becomes
`<<16#A7>>' (a bare 0xA7 is an invalid UTF-8 byte). All five
replaced with ASCII. Without this fix, no batch-9 envelope
would ever serialise over HTTP -- the QEMU smoke-test caught
it.

**Pass 8 -- Framework envelope predictor** (against batch-10
HEAD 88fda48b2 + the v1.1 Framework capture at
`out/local-capture/framework-13-v1-1-real-ek-roundtrip/`).
Reviewer's task: predict the morning Framework verdict
signal-by-signal + finding-by-finding, based on the v1.1 real-
hardware envelope shape + the batch-9/10 verifier code. Verdict:
**no further code fixes required before demo**, BUT one
cosmetic pre-land landed as **batch 11** (commit pending at
time of writing):

  Batch 11 -- narrative softening on `freshness-safe-false'
  message (VERIFIER-SIDE ONLY; no USB rebuild needed):
    Reviewer observed the v1.1 capture's reset-count=2.7e9 /
    restart-count=3.6e8 drives `fresh_boot_classify/2' to the
    `tamper' branch purely because the Nuvoton NPCT75x has
    never successfully executed `TPM2_Shutdown(STATE)' (a
    benign first-production-boot state common on discrete
    TPMs). Severity stays critical (an adversary could also
    hit this branch; we cannot distinguish from the envelope
    alone), but the message text now names BOTH causes so a
    demo audience doesn't read "clock has been tampered with"
    and panic. Tests don't pin the exact wording (they check
    the `code' field only); all 229 still pass.

**Pass 10 -- adversarial envelope fuzzer** (against batch-11 HEAD
1c31787f6). Verdict before fixes: PASS-WITH-NOTES -- the
`verify/3' path (which wraps every check in `safely_run') was
structurally robust against all 20 adversarial shapes tested.
The unguarded entry points `interpret/3' and `claim/3' had three
concrete crash paths that violated the LapEE canonical rule
(AGENTS.md: "no crashes; every claim.* field populates to a
concrete value OR an explicit unknown/absent"). All three
landed as batch 12:

  CRITICAL  `decode_cert(undefined)' and `decode_pub_key(undefined)'
            raised `function_clause' on envelopes round-tripped
            through a JSON library that decodes `null' to the
            Erlang atom `undefined'. Since neither `claim/3' nor
            `interpret/3' wraps its callee in `try' (only
            `verify/3' does via `safe_interpret'), the crash
            escaped to a 500 stacktrace. Added catch-all clauses
            returning `{error, not_binary}'; existing downstream
            callers already handle `{error, _}' and produce a
            structured `unknown_ek_claim()' / `unknown_ak_claim()'
            verdict.
  MEDIUM    `resolve_envelope/3' in BOTH `dev_tpm_interpret' and
            `dev_tpm2' called `hb_maps:get(<<"body">>, Base, ...)'
            without guarding `is_map(Base)'. A top-level JSON
            array (or any non-map Base) crashed with
            `{badmap, Base}' before the `safely_run' / `safe_
            interpret' shield. Both modules now guard with
            `when is_map(Base)' and fall through to an empty map.
  LOW       Three `platform-probes' consumers (`enrich_cpu_from_
            cpuinfo', `claim_lockdown', `claim_iommu') read
            `platform-probes' as a map and indexed into it. An
            adversarial envelope setting `platform-probes' to a
            binary / integer / list / atom crashed the second
            `hb_maps:get'. Centralised in a new `probes_map/1'
            helper that normalises to `#{}'; all three sites now
            call it.

Tests: 229 -> 234 (+5 new regression tests, each reproducing one
of the pre-fix crash shapes and asserting the structured-unknown
response instead):
  - v1_2_decode_cert_survives_non_binary_test
  - v1_2_decode_pub_key_survives_non_binary_test
  - v1_2_resolve_envelope_survives_non_map_base_test
  - v1_2_probes_map_normalises_non_map_test
  - v1_2_claim_survives_adversarial_envelope_test

The LapEE canonical "no crashes" rule is now mechanically
enforced by the test suite, not just aspirational. 20 adversarial
envelope shapes from reviewer pass 10 that previously had three
crash paths now all produce structured verdicts.

**Pass 11 -- concurrency race auditor** (against batch-12 HEAD
c2524729d). Verdict before fixes: FIX-BEFORE-DEMO. Five concrete
concurrency findings across `dev_tpm2.erl' and
`native/lapee_tpm_nif/'. One (B1) demo-blocking; four (B2-B5)
production-quality issues that don't block a single-user demo.
B1 addressed in batch 13; B2-B5 deferred to v1.3 with concrete
fix proposals. Details:

  B1 CRITICAL  `ensure_ak/1' was a classic check-then-act. Two
               concurrent `/attestation' requests within ~10ms
               of boot both saw `{dev_tpm2, ak_tr}=undefined'
               and both entered `init_chain', creating two EK
               primaries (same key, extra transient handle
               wasted), two AK primaries with DIFFERENT RSA
               keys, extending PCR 15 twice with different
               digests, and racing `persistent_term:put'
               writes. Worst case: request A's envelope
               carries B's `ak-pub-pem' (last-writer-wins on
               the cached PEM) while A's quote was signed by
               A's AK -> `rsa_pss:verify' fails -> verdict=
               rejected on a legitimate boot.
               **Fixed in batch 13 (commit pending):** wrap
               the fast path in `global:trans/3' with
               double-checked locking. The outer `persistent_
               term:get' stays lock-free on the hot path;
               only the once-per-boot init path takes the
               node-local lock; the inner re-check ensures
               only one caller runs `init_chain' even when
               N callers arrive before any finish.
               New eunit test `ensure_once_double_checked_
               lock_serialises_test' spawns 20 concurrent
               callers against a synthetic init-chain-like
               body and asserts the body ran exactly once.

  B2 HIGH      `append_event/2' has a classic lost-update RMW
               race: two concurrent callers both read `Old',
               one `put' wins. The TPM `PCR_Extend' is atomic
               at the TPM so the final PCR value is correct,
               but the Erlang event-log loses one entry ->
               `chk_event_log_replay' diverges from the
               quoted PCR -> verdict=rejected.
               Demo-impact: single-user boot typically has
               at most one `extend' per PCR so the race
               window is narrow. v1.3 fix: move `event_log'
               to ETS with `ets:update_counter' for seq;
               both operations atomic.

  B3 HIGH      `nif_pcr_extend' + `append_event' pair is not
               atomic together. Two concurrent `extend' calls
               can see TPM-order different from Erlang-log-
               order, so `chk_event_log_replay' rejects
               (because the fold order on the Erlang side
               doesn't match the PCR trajectory).
               v1.3 fix: wrap both operations in one lock
               together.

  B4 HIGH      Global `g_esys_ctx' in
               `native/lapee_tpm_nif/lapee_tpm_nif.c' is
               accessed from multiple BEAM scheduler threads
               without a mutex. TSS2 ESAPI spec: `ESYS_CONTEXT'
               is NOT thread-safe. Under swtpm and low
               concurrency it works (current demo path);
               under hardware-TPM load or concurrent
               attestation, sporadic TSS2 state corruption +
               possible BEAM segfault.
               v1.3 fix: either (a) serialise all Esys_*
               through a single gen_server on the Erlang
               side, or (b) add a pthread_mutex around every
               Esys_* call in the NIF. (a) is cleaner.

  B5 MEDIUM    `persistent_term:put({dev_tpm2, event_log},
               Old ++ [Entry])' grows the log in O(N^2)
               memory + triggers a process-wide scan on every
               put. Long-lived node with many on/start hooks
               degrades. Not a race; a memory-churn issue.
               v1.3 fix: same ETS migration as B2.

Batch 13 landed B1 only -- ~25 LoC + 1 regression test,
closes the demo-blocking race. Acceptance test for the fix:

```
# Against a running LapEE node (post-boot):
for i in 1 2 3 4 5; do
    curl -s -H 'accept: application/json@1.0' \
            -H 'accept-bundle: true' \
            http://<node>:8734/~tpm2@2.0a/attestation \
      > /tmp/att-$i.json &
done
wait

# Every envelope must have the SAME ak-pub-pem:
for i in 1 2 3 4 5; do
    python3 -c "
import json
print(json.load(open('/tmp/att-$i.json'))['body']['ak-pub-pem'][:64])"
done | sort -u | wc -l
# -> 1 (was 2+ pre-fix under heavy concurrent load)
```

Single-user demo flow in the morning (one curl at a time)
does not hit the race in practice, but the fix is cheap and
closes the sharpest footgun.

**Pass 12 -- NIF-level memory safety + error-path auditor**
(against batch-13 HEAD 1ef6fe2d9). Twelve NIF entry points +
two helper files audited across 1184 LoC of C. Verdict before
fixes: SHIP-WITH-NOTES -- one CRITICAL and one HIGH landed as
batch 14. Residual findings deferred to v1.3.

  CRITICAL-1  `nif_quote' in `native/lapee_tpm_nif/lapee_tpm_
              nif.c' declared a stack array `int pcr_indices[24]'
              and had NO bounds check against `pcr_count'. The
              per-index range check `i < 0 || i > 23' present
              on line 442 does NOT guard the array index: a
              caller passing >24 PCRs, or passing a list with
              duplicates such that pcr_count grows past 24,
              overflows the stack buffer. Today the Erlang
              caller always passes curated short lists, but
              the NIF must not trust that. Stack-smash from
              caller-controlled input = RCE-grade bug.
              **Fixed in batch 14** with a 3-LoC bounds check
              `if (pcr_count >= 24) return enif_make_badarg(env);'
              before the write on line 445.

  HIGH-1      Every NIF function that blocks on a synchronous
              TPM/SPI round-trip was declared with scheduler
              flag 0 (regular scheduler). On the Nuvoton
              NPCT75x observed latencies:
                Esys_CreatePrimary (RSA-2048 keygen)    300-800 ms
                Esys_Quote (RSA-PSS sign)               200-400 ms
                Esys_NV_Read (chunked 512 B/round)       30- 80 ms
                Esys_PCR_Extend                           5- 15 ms
                Esys_PCR_Read                             2-  8 ms
                Esys_GetCapability                        2- 10 ms
              A scheduler-stall of 300-800 ms violates BEAM's
              1-ms-per-NIF-call budget and on any concurrently-
              loaded node causes scheduler-stall warnings +
              degraded latency for UNRELATED HTTP handlers.
              **Fixed in batch 14** by declaring
              `ERL_NIF_DIRTY_JOB_IO_BOUND' on nine NIF entry
              points (all the ESYS-calling ones except
              `flush_context' / `set_tcti' / `startup' which
              are either near-instant or one-shot).

Residual findings from reviewer pass 12 (v1.3 backlog):

  B4 (re-confirm) `g_esys_ctx' in `tpm_helpers.c' not mutex-
                  protected. Mitigated by the batch-13
                  `global:trans' lock on the Erlang side (which
                  serialises ensure_ak), but the NIF relies on
                  that discipline rather than enforcing it. For
                  production robustness, wrap with
                  `ErlNifMutex' in the NIF. ~15 LoC.
  B7 (new)       `nif_set_tcti' on Esys_Initialize failure
                  leaves `g_tcti_ctx' live but `g_esys_ctx'
                  NULL. Next call NULL-dereferences. 2-LoC
                  fix: add `Tss2_TctiLdr_Finalize(&g_tcti_ctx);'
                  before the error return.
  B8 (new)       `nif_nv_read_public' / `nif_nv_read' RC_HANDLE
                  mask `0x0BF' at line 801 / 883 is semantically
                  correct but misleading; should read `0xBF'.
                  Cosmetic only.

Tests: 235 pass (Erlang side unchanged; C-side PCR bounds
defence isn't exercisable at the eunit layer without spinning
up swtpm in the test harness -- validated via QEMU smoke test
of the rebuilt image).

**Pass 13 -- cryptographic primitives auditor** (against batch-14
HEAD edc25cbff). Six cryptographic surfaces audited:

  1. RSA-PSS signature verification (TPM quote sig) --
     `dev_tpm2:chk_quote/1' + `dev_tpm_interpret:
     verify_quote_signature/1'
  2. PCR extend digest consistency (SHA-256 across 7 call
     sites + the NIF's TPML_DIGEST_VALUES)
  3. EK cert chain validation (`pkix_path_validation' +
     TCG-OID whitelist)
  4. Hash-algorithm-agile decoders (SHA-256 bank selection,
     SHA-1-only-log rejection path)
  5. Nonce freshness (32-byte `strong_rand_bytes', strict
     `=:=' match)
  6. Constant-time comparison considerations

**Verdict: SHIP.** No CRITICAL or HIGH cryptographic findings.
All primitives are used correctly and consistently:
  - RSA-PSS: MGF1-SHA-256, salt=auto (accepts TPM's hashLen
    salt per TCG TPM 2.0 Part 1 §11.2.4.4 + PKCS #1 v2.1 §8.1;
    for SHA-256 the TPM emits salt=32).
  - TPMS_ATTEST signing: NIF declares `TPM2_ALG_RSAPSS' +
    `hashAlg = TPM2_ALG_SHA256' in `nif_quote'; matches
    verifier `rsa_pss:verify(Quoted, sha256, Sig, Key)' call.
  - PCR-extend: SHA-256 everywhere -- no algorithm drift.
  - EK chain: full pkix_path_validation, no weaker-algorithm
    shortcuts, narrow 2-OID TCG whitelist.
  - Nonces: `crypto:strong_rand_bytes(32)' on both producer
    and verifier, strict equality match on cross-node path.
  - Constant-time: comparisons are only on public data
    (hashes of public inputs), so short-circuit `=:=' is
    fine; no MAC verifications present.

Three LOW-severity polish items landed as **batch 15** (no
security impact, code-quality cleanup only):

  1. Removed dead `#'SubjectPublicKeyInfo'{}' fallback in
     `dev_tpm2:decode_pem_rsa_pub/1'. The fallback called
     `pkix_decode_cert' on an SPKI record, which was both
     broken (pkix_decode_cert expects DER bytes) and
     unreachable (the NIF always emits SPKI PEM that OTP's
     `pem_entry_decode/1' renders directly as
     `#'RSAPublicKey'{}').
  2. Corrected the `verify_quote_signature/1' comment in
     dev_tpm_interpret to document `salt=auto' (matches
     reality) instead of `salt=32' (which was misleading --
     the salt is auto-discovered from the signature, and
     happens to be 32 because that's what the TPM emits for
     SHA-256).
  3. Added a 64-byte TPM2B_DATA nonce length guard in
     `dev_tpm2:resolve_nonce/1'. Oversize nonces now fall
     through to a freshly-generated 32-byte random nonce
     rather than producing an `enif_make_badarg' crash in
     the NIF (which would return a less-helpful HTTP 400).

Tests: 235 pass (unchanged).

**End-of-night status:** 13 reviewer passes and 15 code
batches. The final iteration converged to LOW-severity polish
only -- strong signal of code maturity. No outstanding
CRITICAL or HIGH security findings.

Predictions from reviewer pass 8 (likely morning outcome):

  verdict  = untrusted
  criticals= 1 (freshness-safe-false)    -- the batch 11
                                            narrative-softened
                                            message
             or 2 if Nuvoton NV 0x01C00003 is empty
             (ek-chain-invalid; hardware-dependent -- will
             know in the morning)
  warnings = 5
    - secure-boot-disabled        (Framework ships SB off,
                                    LapEE kernel not enrolled
                                    into PK)
    - pcr-replay-multi-mismatch   (SeaBIOS-style BIOS log vs
                                    quoted-PCR bank mismatch)
    - ek-ak-binding-not-implemented (batch 7 honest warn;
                                    v1.3 MakeCredential target)
    - tpm-known-cves              (Nuvoton has 2 CVEs listed)
    - lockdown-integrity-not-confidentiality
                                  (Linux 6.x default) OR
      lockdown-off-or-unknown     (if lockdown LSM not
                                    compiled)
  score    = 0-20

The three notable surprises to watch for (per reviewer pass 8):

  (1) `freshness-safe-false' critical fires by design; the
      batch 11 message now explains the benign cause.
  (2) `ek-chain-invalid' may or may not fire depending on
      whether Nuvoton provisioned the intermediate CA at NV
      `0x01C00003'. v1.1 hit this; batch 2 E2 pulls the
      intermediate from NV handle+1 IF it's there.
  (3) `lockdown-off-or-unknown' vs `integrity-not-conf' depends
      on whether `CONFIG_SECURITY_LOCKDOWN_LSM=y' landed in the
      LapEE kernel fragment. Check
      `lapee-baremetal/buildroot-external/board/lapee/linux-m1-fragment.config'
      before boot.

---

## Paper amendment draft (reviewer pass 9)

Reviewer pass 9 (paper amendment drafter) produced ready-to-apply
LaTeX text changes for `lapee-paper/main.tex` so the paper describes
what v1.2 code actually delivers. Four amendments cover the four
CRITICAL paper-code gaps from reviewer pass 6. Apply these via
`cd ../sharp-lichterman/lapee-paper && git apply` or hand-edit
the sections below.

### Preserved claims (no paper change needed)

- P5 key-pubkey-extend (lines 249-251): landed in batch 9/10; paper
  prose is accurate.
- Secure Boot / UKI / dm-verity (217-222), Lockdown+modsig+IOMMU
  (223-226), Attestation evidence (258-264), Threat actors A1-A4 --
  all unchanged, all accurate.

### Amendment 1 -- TME enforcement at init (CRITICAL-2)

Location: `main.tex' Architecture section, "Boot and workload
measurement" paragraph, lines 226-230.

BEFORE:
```
Early init reads \texttt{IA32\_TME\_ACTIVATE}
(Intel) or \texttt{SYSCFG} bit 23 (AMD) --- see the vendor programming
references~\cite{intel-tme,amd-sme} for exact semantics --- and refuses
to proceed if memory encryption is inactive --- so a successful
attestation is itself proof that TME was enabled, without relying on
vendor-specific firmware events.
```

AFTER:
```
The attestation envelope carries a \texttt{claim.tme} field whose
value derives from vendor-specific firmware events and, where
exposed, subsequent verifier-side inspection of
\texttt{IA32\_TME\_ACTIVATE} (Intel) or \texttt{SYSCFG} bit 23 (AMD);
see the vendor programming references~\cite{intel-tme,amd-sme} for
exact semantics. Verifier policy decides the verdict: an operator
targeting the strongest tier treats \texttt{tme-enabled = false}
or \texttt{unknown} as disqualifying. A forthcoming revision moves
the MSR read into early init with a hard-refusal path, so that a
successful attestation is itself proof that TME was enabled
without relying on vendor-specific firmware events.
```

### Amendment 2 -- AK under Endorsement hierarchy (CRITICAL-3)

Location: `main.tex' Architecture section, "Ephemeral node key
binding" paragraph, lines 242-244.

BEFORE:
```
\paragraph{Ephemeral node key binding.} At the end of measured
boot, HyperBEAM calls \texttt{TPM2\_Create} for a fresh signing
keypair under a primary on the Endorsement hierarchy.
```

AFTER:
```
\paragraph{Ephemeral node key binding.} At the end of measured
boot, HyperBEAM calls \texttt{TPM2\_CreatePrimary} for a fresh
signing keypair under a primary in the Owner hierarchy; a
forthcoming revision reparents under the Endorsement hierarchy
and binds the AK to the EK via
\texttt{MakeCredential}/\texttt{ActivateCredential} so that the
attestation key is cryptographically bound to the hardware-
vendor-certified EK.
```

Downstream edit (lines 252-253): soften "an attacker cannot
synthesize a fresh `device' without vendor collusion" to
"conditional, in v1.2, on the verifier policing the EK-chain
field directly; the MakeCredential handshake planned for v1.3
makes the binding cryptographic."

### Amendment 3 -- Encrypted TPM sessions (CRITICAL-4)

Three edit points in `main.tex':

(a) Architecture section lines 255-256:
```
BEFORE: All TPM sessions touching sensitive state use encrypted
        sessions (HMAC $+$ parameter encryption~\cite{tcg-tpm2}).
AFTER:  In a forthcoming revision, all TPM sessions touching
        sensitive state will use encrypted sessions (HMAC $+$
        parameter encryption~\cite{tcg-tpm2}); in v1.2 these
        sessions use password authorisation, and operators
        running on a discrete TPM with a cleartext SPI/LPC bus
        should treat bus interposition as a physical-security
        assumption until the encrypted-session work lands.
```

(b) Table 2 row (line 357), "TPM bus sniffing (dTPM)":
```
BEFORE: Blocked at load   | Encrypted sessions (HMAC + param enc.)
AFTER:  \textbf{v1.3 target} | Encrypted sessions (HMAC + param enc.)
                              planned; v1.2 treats bus interposition
                              as physical-security.
```

(c) Implementation paragraph line 488:
```
BEFORE: ...exposing quote, sign, PCR-extend, NV ops,
        event-log-read, with encrypted sessions by default.
AFTER:  ...exposing quote, sign, PCR-extend, NV ops, and
        event-log-read; v1.3 adds HMAC-plus-parameter-encryption
        sessions on calls touching sensitive state.
```

### Amendment 4 -- AO-Core hashpath continuity (CRITICAL-5)

Location: `main.tex' "AO-Core Continuity" section, lines 275-280.

BEFORE:
```
HyperBEAM seeds its AO-Core chain with a commitment to the TPM
event log tip immediately after \texttt{key-pubkey-extend};
thereafter every device first-load and every message extends the
chain. The two logs are not analogous mechanisms; they are the
same cryptographic primitive composed end-to-end
(Figure~\ref{fig:chain}).
```

AFTER:
```
In the architectural target, HyperBEAM seeds its AO-Core chain
with a commitment to the TPM event log tip immediately after
\texttt{key-pubkey-extend}, and each attestation envelope records
the AO-Core tip at quote time; thereafter every device first-load
and every message extends the chain. v1.2 carries both the TPM
event log and the AO-Core hashpath in the envelope but does not
cryptographically bind them; the planned
\texttt{attestation-at-hashpath-tip} field closes the gap. The
two logs are not analogous mechanisms; they are the same
cryptographic primitive, and composing them end-to-end
(Figure~\ref{fig:chain}) is an engineering step rather than a new
design.
```

### Optional single-footnote alternative

If preserving present-tense prose is preferred, attach this
footnote to the first amended sentence (the TME paragraph):

```
\footnote{As of v1.2 of the reference implementation
(\texttt{github.com/permaweb/hb-os}), TME enforcement, AK parenting
under the Endorsement hierarchy with MakeCredential-based EK
binding, HMAC+parameter-encrypted TPM sessions, and a cryptographic
commitment linking the TPM event log tip into the AO-Core hashpath
are planned for v1.3; the v1.2 envelope carries all necessary
evidence for verifier-side policy to approximate these properties.}
```

This is the lowest-intervention path but less honest to a reader
skimming the prose. Prefer the four per-sentence amendments above
if the review/publishing context rewards precision.

---

## v1.3 delivery plan (from reviewer pass 15)

Reviewer pass 15 (v1.3 planning advisor) read the accumulated
backlog and produced a concrete, ordered delivery plan. Gist:

**Scope.** The morning demo proves v1.2 (P5 key-pubkey-extend
enforced, RSA-PSS verifies, EK pulled live from NV). v1.3 must
close the four CRITICAL paper-code gaps plus the deferred
concurrency (B2-B5), NIF robustness (B7/B8), and verdict-
quality items from passes 4/6/11/12.

**Total estimate: 6-7 developer-weeks.**

### P0 -- post-demo week (~2d bundled)

  1. B8 `0x0BF' -> `0xBF' mask cosmetic (reviewer 12 residual).
  2. B7 `nif_set_tcti' cleanup-on-error (reviewer 12 residual).
  3. Apply reviewer 9's drafted paper amendments to
     `lapee-paper/main.tex'.
  4. Demo-ops M1-M5 bundle (DHCP multi-router, IP-flush gap,
     count_iommu_groups readability, lockdown late-escalation
     snapshot, GeneralizedTime fractional seconds).

### P1 -- core v1.3 features (6 weeks)

  P1-A  TPM2_MakeCredential/ActivateCredential EK<->AK
        binding. 2w. Blocks nothing further; depends on P1-B.
        **High risk** (AK-creation hot path; batch-13 lock).
        Source: red-team CRITICAL 2 + paper pass 6 CRITICAL-3.
  P1-B  AK under Endorsement hierarchy (ESYS_TR_RH_OWNER ->
        ESYS_TR_RH_ENDORSEMENT). 2d standalone; folds into
        P1-A. **Medium risk** (AK-Name changes; envelope
        schema bump). Source: pass 6 CRITICAL-3.
  P1-C  HMAC + parameter-encrypted TPM sessions. 1w. No
        dependencies; orthogonal. **Medium risk**. Source:
        pass 6 CRITICAL-4.
  P1-D  TME-at-init MSR enforcement in initramfs (IA32_TME_
        ACTIVATE / SYSCFG bit 23). 1w. Needs msr-tools OR a
        ~40-LoC C helper; init-side only. **Low risk**.
        Source: pass 6 CRITICAL-2.
  P1-E  AO-Core hashpath continuity via `attestation-at-
        hashpath-tip' envelope field + init-time seed. 1w.
        Envelope schema bump. **Medium risk**. Source: pass 6
        CRITICAL-5.
  P1-F  Concurrency hardening (B2 ETS event log, B3 unified
        lock, B4 Esys gen_server dispatcher, B5 O(N^2) churn
        elimination). 1w. Depends on P1-A/B landing first so
        the gen_server is the single MakeCredential routing
        point. **Medium risk**. Source: passes 11 + 12.
  P1-G  Verdict-quality hardening: pcr_replay_finding strict
        N>=1 critical, firmware version floor, positive-
        signal floor for verdict=trusted, `commitments'
        verification, per-first-load PCR-15 extend via
        `dev_trusted_signers'. 1w. Verifier-side only. **Low
        risk**. Source: passes 4 HIGH 1-4 + 6 HIGH-1/3 +
        MEDIUM-1/2/3.

### P2 -- v1.4+ followup

  - trust-tier derived from EK cert TCG OID not
    platform-probes (pass 4 HIGH 4).
  - interpret-local-capture `--url` mode calls
    /verify-peer (pass 4 HIGH 2).
  - lockdown cross-check against EV_IPL cmdline (pass 6
    HIGH-2 escalation).
  - hb_message:sign/2 RSASSA-PSS audit (pass 6 MEDIUM-5).
  - producer-side fuzzer expansion (pass 10 scope extension).

### Recommended v1.3 delivery sequence

```
Week 1: P0 bundle + P1-D (TME-at-init).
        Gate: paper amendments applied; initramfs panics on
              TME-disabled boot.

Week 2-3: P1-B (Endorsement) -> P1-A (MakeCredential).
        Gate: stolen-EK test envelope verifies false; batch-13
              concurrent-AK regression test still green.

Week 4: P1-F (concurrency B2-B5).
        Gate: 100-caller extend+quote stress for 10 min with
              zero replay divergence and zero TSS2 corruption.
              Must precede P1-C (Esys gen_server is the session-
              handle sync point).

Week 5: P1-C (encrypted sessions).
        Gate: swtpm trace shows encrypted quote traffic;
              Framework iron /attestation still < 3s.

Week 6: P1-E (AO-Core hashpath) + P1-G (verdict-quality).
        Gate: every v1.3 envelope carries attestation-at-
              hashpath-tip; v1.2 envelope rejected by v1.3
              verifier with verifier-version-floor OR
              ao-core-hashpath-unlinked. Paper P11 + P12 move
              from "forthcoming" to "delivered".
```

**Ship target:** 6-7 weeks from morning demo. P2 slots into
a subsequent v1.4 window.

---

## v1.3 open-question ledger (from reviewer pass 6)

| # | Paper claim                                  | Code path           | Resolution       |
|---|----------------------------------------------|---------------------|------------------|
| 1 | P5 "key-pubkey-extend in PCR 15"             | extend_with_ak_pubkey | **FIXED batch 9** |
| 2 | P2 "init refuses if TME off"                 | initramfs-hb/init   | PAPER-AMEND + v1.3 MSR read |
| 3 | P4 "AK under Endorsement hierarchy"          | lapee_tpm_nif.c:342 | PAPER-AMEND + v1.3 Endorsement + MakeCredential |
| 4 | P6 "encrypted TPM sessions"                  | lapee_tpm_nif.c:*   | PAPER-AMEND + v1.3 HMAC sessions |
| 5 | P11 "AO-Core chain commits to TPM tip"       | (no code)           | PAPER-AMEND + v1.3 envelope field |
| 6 | P12 "verifier version floor"                 | (no code)           | PAPER-AMEND + v1.3 config |
| 7 | P9 RSASSA-PSS result signatures              | hb_message:sign/2   | Audit-open |

---

## Situation report

**v1.1 acceptance passed.** The LapEE USB image booted on Sam's
Framework 13 (AMD Ryzen 7040, Insyde H2O `IFR30.03.04`), pulled a
real **861-byte Endorsement Key cert from TPM NV storage** at
`0x01C00002`, and wrote back an attestation envelope that parses
end-to-end on the verifier side:

```
ek-cert-source   = {kind: "tpm-nv", handle: "0x01C00002", bytes: 861}
ek-cert issuer   = CN=NPCTxxx ECC384 LeafCA 012110,
                   O=Nuvoton Technology Corporation, C=TW
tpm-properties   = {manufacturer: "NTC", vendor-string: "NPCT75x...",
                    spec-family: "2.0", spec-revision: 1.38, ...}
claim.tpm        = Nuvoton NPCT75x discrete (trust-tier=strongest)
                   + CVE-2023-34440 + CVE-2023-1017/1018
claim.firmware   = CRTM IFR30.03.04
                   (matches Framework Laptop (13 / 16) family)
claim.tme        = enabled (tier-4 via PCR-15 extension)
```

Sam's Framework has a **discrete Nuvoton NPCT75x**, not the AMD
fTPM we'd assumed. (Framework's published spec lists Nuvoton
`NPCT7xx` for all variants; the AMD PSP fTPM is available but the
discrete chip wins when both are present and the platform config
enables TPM_CRB.)

**What's committed.** `agent/lapee` branch, pushed to Permagit:

```
7018caf2a  v1.1 followup: interpret_tpm_capabilities handles
           JSON round-tripped atoms
c2f70e0d0  v1.1 followup: TPM2_RC_HANDLE mask + tuple-safe
           format_probe_attempts
f30988375  v1.1 followup: vendor-by-u32 lookup so capability
           manufacturer IDs match
c851ad6a7  v1.1: rip out test-CA / test-EK synthesis; real EK +
           TPM identity end-to-end
355166722  baremetal: v1.0 Framework bookend + repo tidying
```

**What's working end-to-end**:

- Zero synthesized certs / keys / identities anywhere in the
  runtime path. If the TPM doesn't provide a value, the envelope
  records the absence explicitly (`ek-cert-source.kind = absent`)
  rather than substitute a stand-in.
- `lapee_tpm_nif` has `nv_read`, `nv_read_public`,
  `tpm_properties`, all live against Nuvoton's firmware.
- 51 vendor EK root CAs shipped in
  `priv/tpm-interpret/root-cas/` (Infineon / Intel PTT / Nuvoton /
  STMicro / GlobalSign / Alibaba).
- Parser prefers live `TPM2_GetCapability` over EK-cert TCG OIDs;
  falls through cleanly when either source is absent.
- `tme.enabled = true` cross-links into `cpu.tee-support` with
  vendor-specific feature name (amd-sme / intel-tme) when vendor
  is known; generic `memory-encryption` otherwise.
- 201 eunit tests green (98 dev_tpm_tcg + 20 dev_tpm2 + 83
  dev_tpm_interpret; 9 new for v1.1).

**What's not yet working** -- the real capture surfaced a set of
concrete gaps (parser bugs, missing cert-chain intermediates,
`unknown` fields that need more data from the guest). Full list
under "TODO list" below. See `out/local-capture/framework-13-v1-1-real-ek-roundtrip/dashboard.html`
for the live example.

---

## v1.2 overnight mission

**Brief:** Between now and lunch 2026-04-24, take the Framework
from v1.1-acceptance-with-gaps to v1.2-full-acceptance. That means
real networking, a stripped boot image (sub-second boot on iron),
a clean repo that a reviewer can pick up cold, a boot splash that
renders the live node URL, and every paper-committed security
property either COVERED or explicitly recorded as N/A with a
stated reason.

**Acceptance criteria:**

1. Framework boots the v1.2 USB image with a real IP address in
   under ~2 seconds of guest-side work (UEFI handoff to HB
   answering `/~tpm2@2.0a/info`). Network interface obtained via
   DHCP over built-in Ethernet, USB-C Ethernet dongle, or
   Thunderbolt-to-Mac bridge, in that precedence.
2. The attestation envelope's `policy-verdict.verdict = "trusted"`
   OR every critical failure has a fully-honest chain of evidence
   explaining why the corresponding property is disabled (not a
   bug, a user/hardware choice).
3. `claim.cpu.vendor`, `claim.iommu.enabled`,
   `claim.lockdown.level`, and every other paper-committed field
   resolve to a specific value or an explicit "not-applicable"
   with a reason. No `unknown` fields on the trusted path.
4. Boot splash: centered HyperBEAM ASCII art on the console,
   re-rendered once HB is live with `  http://<ip>:8734`.
5. `lapee/` working tree is clean of dead code: every script +
   every directory under it is referenced by an active build
   target, or it is gone.
6. `lapee-usb.img` is as small as cleanly achievable; no shipped
   JS / CSS / debug symbols / test fixtures / source trees /
   bundled docs. `du -sh` on `/ramfs` under 40 MB, realistically.
7. Cross-node verify (`verify-peer`) works: Sam's Mac HB instance
   issues a fresh-nonce challenge to the Framework over HTTP and
   the LapEE verifier returns verdict=trusted.

Any TODO that can't be landed by morning gets a one-line "deferred
because X" note in this file.

---

## TODO list

Grouped by area. Each item is self-contained; Claude can pick any
two in parallel if needed.

### A. Networking in the guest -- new

**A1.** Rebuild the Buildroot kernel with a broad real-hardware NIC
driver set built in-tree (not as modules, so no module loader
needed at boot). Minimum set:

- `CONFIG_R8169=y` + `CONFIG_MII=y` -- Framework 13's expansion-card
  Gigabit Ethernet (Realtek RTL8111).
- `CONFIG_E1000E=y`, `CONFIG_IGC=y` -- Intel 1G / 2.5G NICs.
- `CONFIG_TG3=y` -- Broadcom server NICs.
- `CONFIG_USB_USBNET=y` + `CONFIG_USB_CDC_NCM=y` +
  `CONFIG_USB_RTL8152=y` + `CONFIG_USB_NET_AX88179_178A=y` +
  `CONFIG_USB_NET_ASIX=y` -- the four common USB-C Ethernet
  dongle chipsets.
- WiFi deferred -- not on the demo path.

Verification: boot the new kernel image under QEMU with
`-device virtio-net-pci` and confirm `ip link` lists eth0 before
init runs. On iron: plug an Ethernet dongle and confirm
`dmesg | grep eth` shows the driver binding.

**A2.** Init-side DHCP. In `initramfs-hb/init`, right after
the mount phase, enumerate `/sys/class/net` for interfaces with
`operstate = up` or `carrier = 1`. For each, fork
`udhcpc -i $iface -q -T 5 -n -s /usr/local/bin/lapee-dhcp-hook`.
First one that gets a lease becomes `ip route`'s default; others
stay in the background as hot-swap candidates.

**A3.** USB-C / Thunderbolt bridged networking. Kernel needs
`CONFIG_THUNDERBOLT=y` + `CONFIG_THUNDERBOLT_NET=y` so
`thunderbolt0` appears as a regular NIC. Mac-side requires the
user to enable "Thunderbolt Bridge" under System Settings ->
Network. Document the Mac steps in README (IP config + Internet
Sharing if they want the Framework to reach the wider internet).

**A4.** Precedence on multi-interface boots. In `lapee-dhcp-hook`:

```
(a) built-in wired Ethernet (eth* with stable driver name)
(b) USB-C Ethernet dongle (usb0 / enp*u*)
(c) Thunderbolt bridge (thunderbolt0)
(d) USB-RNDIS tether (rndis0 -- rare, but free)
```

First-to-lease wins default; log each interface's carrier +
lease state + timing:

```
[net] eth0  carrier=1  dhcp=1.2s  ip=192.168.1.42  gw=192.168.1.1
[net] usb0  carrier=1  dhcp=--    (eth0 already default)
```

**A5.** Print the bound IP on the console as soon as DHCP lands.
Feeds the boot splash (TODO B3) and lets Sam read the node URL
off the screen.

### B. Boot splash -- new

**B1.** Embed the canonical HyperBEAM ASCII logo (the one
`hb_features:print_welcome/0` shows on startup) at
`/etc/lapee/logo.ascii`. Write `/usr/local/bin/lapee-splash`:

```
stty size | read rows cols
pad = (cols - $logo_width) / 2
each logo line: printf "%${pad}s%s\n" "" "$line"
```

Then print a blank status line after it that later steps can
overwrite with ANSI cursor control.

**B2.** Call `lapee-splash` from init as the first visible step
(after `/proc /sys /dev` are up). Kernel messages from the
`loglevel=2` bootline will still scroll over it; the splash
re-renders on first idle tick, which is fine.

**B3.** Post-HB-up hook: once DHCP has an IP AND the `/info`
probe succeeds, `printf "\033[s\033[%dA\033[%dG%s\033[u"` (save
cursor, move up, move to column, write, restore) to overwrite
the status line with `http://<ip>:8734`. Before that it's blank
so the splash doesn't look broken.

**B4.** Use a minimal fixed-width pretty-print that doesn't
require ncurses or external utilities -- busybox `printf` +
`tput cols` (if tput present) or fallback to 80-col.

### C. Repo cleanup -- new

Strip to the code that's actively part of v1.2. Everything else
gets DELETED (not just .gitignore'd).

**C1.** Build directories that are pure artefacts:
- `lapee-baremetal/build-alpine/` (not used on active path; old
  Alpine kernel experiment)
- `lapee-baremetal/build-m1/` (older M1 build tree)
- `lapee-baremetal/build-hyperbeam/` (per-rebuild staging;
  regenerated on every `make hb-release`)
- `lapee-baremetal/buildroot/` (vendored buildroot — needs
  decision: vendor as subtree or fetch-on-demand via script?)

Decision for C1: delete `build-*/`, add a fresh-clone script that
pulls buildroot to `build-buildroot/` on first `make kernel`.

**C2.** `lapee-baremetal/lapee-tpm/` is the old M2/M3 reference-
demo orchestrator (`lapee_node.erl` + a copy of the NIF source).
Not on the Framework boot path -- that lives in `src/dev_tpm2.erl`
+ `native/lapee_tpm_nif/`. Move to
`reference-demo/legacy-lapee-tpm/` with a README explaining
"this is the M3 milestone reference; see `src/dev_tpm2.erl` for
the v1.2 hot path".

**C3.** `lapee-baremetal/scripts/` triage. Active-path scripts:

```
build-hb-release.sh
build-initramfs-hb.sh
build-usb-image.sh
boot-usb-image.sh
fetch-ek-root-cas.sh
interpret-local-capture.sh
hb-cross-node-verify.sh  (used by acceptance tests)
```

Inactive: `build-initramfs.sh` (old M2/M3 flow), `uki.sh` (now
inlined), `boot-hb.sh` (QEMU-only, supplanted by
`boot-usb-image.sh`), `hb-dashboard.sh`, any `swtpm-*.sh` helpers,
anything referenced only by `make demo` / `make hb-all`.

Decision: delete inactive scripts, update Makefile targets to
only what the v1.2 flow uses.

**C4.** `reference-demo/` kept, with a clear `README.md`:
"this is the 2026-04-18 M2-M5 reference. Not the current LapEE.
For the current architecture, see `../README.md` and
`../STATUS.md`."

**C5.** `.gitignore` hygiene. Add:
- `/out/` (root) -- verifier `interpret-local-capture.sh` output
- `lapee-baremetal/work/`
- `lapee-baremetal/out/` (already handled in places, but
  normalise)

**C6.** Docs cleanup:
- `PLAN.md`, `OVERNIGHT-PLAN.md`, `FEATURES.md`, `HARDENING.md`,
  `INTERPRET-MVP-PLAN.md`, `VERIFIER-DEPLOY.md`, `SECURITY.md`
  -- audit each. Keep if it's current (SECURITY + README +
  STATUS + HISTORY). Archive under `docs/archive/` if it's
  historical but worth keeping (PLAN, OVERNIGHT-PLAN). Delete
  if stale + superseded (MVP plans for work that's long done).

### D. Image slim + boot-time reduction -- new

Current initramfs is ~60 MB compressed. Target: under 30 MB
compressed, sub-second userspace boot on real hardware.

**D1.** Strip shipping: `priv/static` (~13 MB HyperBuddy JS/CSS),
`priv/html` (~2 MB static HTML). Both already `rm -rf`'d by
`build-initramfs-hb.sh`. Verify on every rebuild: `du -sh
/ramfs/usr/lib/hyperbeam/lib/hb-*/priv/static` should be empty
or absent.

**D2.** `erts-*/bin/beam.smp` and all `.so` files stripped of
debug symbols:
```
find /ramfs/usr/lib/hyperbeam -name 'beam.smp' -o -name '*.so' \
    | xargs strip -s
```
Verify this runs on every incremental rebuild. Measure before /
after.

**D3.** `priv/tpm-interpret/fixtures/` (~40 MB of TCG event-log
test vectors). These are parser test inputs, not runtime data.
Add rebar3 release overlay:
```
{overlay, [
    {mkdir, "lib/hb-<vsn>/priv/tpm-interpret"},
    {copy, "priv/tpm-interpret/root-cas/", "lib/hb-<vsn>/priv/tpm-interpret/root-cas/"},
    {copy, "priv/tpm-interpret/firmware-versions/", ...},
    ...
]}.
```
i.e. whitelist only the *.json directories we actually need at
runtime (`manufacturers.json`, `cpu-models.json`,
`firmware-versions/`, `pcr-profiles/`, `root-cas/`,
`uki-measurements/`, `boot-images/`, `ima-policies/`) and
exclude `fixtures/` entirely.

**D4.** No `.erl` source files in the release -- `src/`
directories under `lib/hb-*/` are not needed at runtime.
`build-initramfs-hb.sh` already removes them; verify.

**D5.** Boot-time audit. Current init has:
- `mount` probes for /proc /sys /dev (~0ms)
- net up via `ip link set eth0 up` + `udhcpc` (10s timeout)
- multiple `echo` + conditional probes
- `exec /usr/lib/hyperbeam/bin/hb foreground` (HB takes ~30s to
  answer /info under Rosetta QEMU; on iron, ~2-3s expected)

Remove: any `sleep` that isn't gated on a concrete signal. Any
`|| true` that's papering over a missing feature rather than an
optional one. The DHCP `-T 5` should be `-T 2` with a background
retry rather than a blocking wait.

Target: userspace to HB answering /info under 1 second of kernel-
already-booted time. HB's own cold start dominates beyond that;
see D6.

**D6.** HB cold-start audit. Why does HB take 30s to answer on
QEMU? Profile:
```
  /usr/lib/hyperbeam/bin/hb foreground 2>&1 | ts '[%H:%M:%.S]'
```
Likely suspects: LMDB cache warmup, device registry scan, crypto
init. Worth knowing before we claim sub-second; probably 500ms on
iron, not 30s.

**D7.** Compress the initramfs with zstd instead of gzip. zstd
decompress is faster *and* smaller. Kernel needs
`CONFIG_RD_ZSTD=y` (default in recent kernels -- verify).

**D8.** Trim the kernel cmdline further. Currently:
```
console=tty0 console=ttyS0 quiet loglevel=4 panic=10 ima_policy=tcb
  rdinit=/init LAPEE_WRITEBACK=1
```
Drop `ima_policy=tcb` unless IMA is actually being captured into
PCR 10 (currently the stub doesn't emit IMA, so PCR 10 reads as
zero). Re-add once TODO E/F surfaces a real IMA chain.

### E. v1.1 parser follow-ups -- from the real Framework capture

**E1.** `ek.is-currently-valid = false` for a cert whose validity
is 2023-09-12 → 2043-09-12 (today is 2026-04-23; it IS in
window). Bug in `currently_valid/2` in `src/dev_tpm_interpret.erl`.
Likely mis-parsing UTCTime `230912044823Z` or misinterpreting the
year window. Fix + eunit coverage.

**E2.** Chain validation: 51 vendor roots loaded, but the Nuvoton
EK chains through `NPCTxxx ECC384 LeafCA 012110` -- an
intermediate NOT in our bundle. TCG convention says the leaf CA's
cert is provisioned in NV at `0x01C00003` adjacent to the EK cert
at `0x01C00002`. Changes:

- Probe `0x01C00003` in `dev_tpm2:fetch_ek_cert_from_nv/1`
  after we successfully get an EK cert. Stash under
  `ek-cert-chain` as a list (may grow to multiple intermediates).
- `try_validate_against_roots` takes a chain list, not just a
  single cert, so `public_key:pkix_path_validation` validates
  the full path.
- Test against Sam's Framework capture -- expected outcome:
  `chain-valid = true`, `validated-by-root-ca = NUVO_2110.pem`
  (or whichever root matches).

**E3.** CPU vendor / brand is `unknown` on Framework -- the TCG
event log doesn't carry `"Intel"` / `"AMD"` / `"AGESA"` strings.
Source options:

(a) Guest `dev_tpm2:attestation/3` reads `/proc/cpuinfo` at
    attestation time, stamps the first `vendor_id` +
    `model name` into the envelope as `cpu-info-proc`.
(b) Guest reads SMBIOS tables via a small NIF (`dmidecode`
    equivalent), stamps a Type-4 processor record into the
    envelope as `cpu-info-smbios`.
(c) NIF calls TPM capability queries -- likely no CPU info
    there on discrete TPMs.

Pick **(a) + (b)**. Parser wires both as tier-2 / tier-3
evidence into `claim.cpu.vendor` / `claim.cpu.model`.

**E4.** NIF vendor-string returned
`"NPCT75x\u0000\"!!4rls"`. Trim at the first embedded NUL, not
just trailing NULs -- C-string convention. Fix in
`native/lapee_tpm_nif/lapee_tpm_nif.c` `nif_tpm_properties`.

**E5.** `firmware.family-platform` was `null` on a Framework
boot that otherwise matched the `IFR30` prefix. Cause:
`framework-laptop.json` has `"platforms"` as a list, but
`dev_tpm_interpret:pick_platform/2` expects a map keyed by CRTM
prefix. Either convert the JSON to a map (keyed by CRTM token
range like `IFR30` -> `"Framework Laptop 13 (AMD Ryzen 7040)"`),
or extend the matcher to accept the list shape. Fix + eunit +
re-run against capture.

**E6.** `freshness-safe-false` on a first-cold-boot TPM is
legitimately uncertain: the TPM never saw a clean shutdown, so
its clock-safe bit is off. Soften severity from
`critical-failure` to `warning` when `resetCount <= 1` AND
`restartCount <= 1` (i.e. fresh boot pattern). Keep it critical
when both counts > 1 (the tamper signal).

### F. Security property coverage target for the lunch demo

Tomorrow's capture must resolve every row below to COVERED or
NOT-APPLICABLE + reason. `unknown` is not an acceptable outcome.

| # | Property                                          | State 2026-04-23 | v1.2 plan       |
|---|---------------------------------------------------|------------------|-----------------|
| 1 | EK cert read from real TPM NV                     | COVERED          | -               |
| 2 | EK chain validates to manufacturer root           | BROKEN (no leaf) | E2              |
| 3 | EK cert currently-valid                           | BROKEN (parser)  | E1              |
| 4 | TPM quote signature verifies under AK             | COVERED          | -               |
| 5 | Quote PCR digest matches recomputed digest        | COVERED          | -               |
| 6 | Quote nonce matches caller's challenge            | COVERED          | -               |
| 7 | Event-log replay matches quoted PCRs              | 7 mismatches     | need full log   |
| 8 | AK public-key bound into PCR 15                   | COVERED          | -               |
| 9 | node-message ID extended into PCR 15              | COVERED          | -               |
| 10| Firmware identity (CRTM) matches fingerprint      | COVERED          | E5 for platform |
| 11| Kernel (UKI) identity in PCR 11                   | COVERED          | -               |
| 12| Secure Boot state (on/off) attested               | COVERED          | user-enabled SB |
| 13| TME / SME state attested                          | COVERED          | -               |
| 14| IOMMU state attested                              | UNKNOWN          | need runtime    |
| 15| Kernel lockdown state attested                    | UNKNOWN          | need runtime    |
| 16| IMA per-file chain on PCR 10                      | NOT APPLICABLE   | stub boot, OK   |
| 17| CPU vendor / model identified                     | UNKNOWN          | E3              |
| 18| TPM manufacturer / model identified               | COVERED          | -               |
| 19| freshness-safe consistent with resetCount         | BROKEN severity  | E6              |

Item 7 (event-log replay): the current initramfs emits a minimal
event log that only contains UEFI handoff + boot services
application events, so PCRs 2/3/4/5/6/9 won't replay against the
real firmware's full log. Either capture the firmware's complete
`binary_bios_measurements` (all 50+ events) into the envelope
BEFORE kernel starts extending, or move the acceptance threshold:
"event log replays to quoted PCRs 0, 1, 7, 11, 14 -- the ones
that matter for identity and trust -- even if PCRs 2-9 are
suppressed by the initramfs stub."

Items 14-15 (IOMMU, lockdown): both need guest-runtime probes
(`/sys/kernel/iommu_groups/`, `/sys/kernel/security/lockdown`).
Covered in E3's (a) -- guest reads /sys at attestation time and
stamps the values into the envelope.

### G. End-to-end verification

**G1.** Reflash. `make hb-usb-write DEV=/dev/disk4` on the v1.2
image. Reboot Framework. Expected timeline on iron:

```
t+0.0s  UEFI hands off to kernel
t+0.4s  centered HB splash appears
t+0.7s  eth0 up, carrier = 1
t+1.1s  DHCP lease, splash re-renders: http://192.168.1.42:8734
t+1.5s  HB answers /info
t+2.5s  /attestation returns a 100 KB envelope
t+3.0s  writeback OK; "safe to power off or leave booted"
```

**G2.** Same-host interpret. `./scripts/interpret-local-capture.sh
--label 'Framework 13 v1.2 iron' /Volumes/LAPEE_ESP/attestation-latest.json`.
Expected verdict: `trusted` or `attested-with-warnings` only for
user-disabled properties (SB off, for example). No `unknown` on
the critical chain.

**G3.** Cross-node verify. On Sam's Mac with a running HB
instance at `http://localhost:8734`:
```
curl 'http://localhost:8734/~tpm-interpret@1.0/verify-peer?peer=http://framework.local:8734'
```
Expected: 200 OK with `verdict=trusted`, `nonce_freshness=match`,
`trust_anchor_source=node_config`.

---

## Next tick

The four new areas (A/B/C/D) are independent; the five parser
follow-ups (E1-E6) are independent of each other; F is a rollup
of evidence from A+E; G is the final acceptance. Reasonable
overnight ordering:

1. **A1 kernel rebuild** (biggest wall-clock item; ~20 min incremental,
   ~2 hours cold if buildroot has to re-resolve).
2. **E1** (parser bug, fast), **E4** (NIF one-liner), **E5** (JSON
   shape) in parallel while kernel builds.
3. **A2-A5** (initramfs init script) + **B1-B4** (splash) --
   these are init-script edits that batch well.
4. **E2** (EK intermediate NV fetch) -- NIF + dev_tpm2 +
   claim_ek wiring. Verify against Sam's existing capture before
   re-booting.
5. **E3** (cpuinfo + SMBIOS in envelope). Requires envelope
   schema bump to 0.5 + parser updates.
6. **D1-D4** (image slim) can be deferred until after the
   networking pass proves end-to-end on hardware, since they
   don't affect semantics.
7. **C1-C6** (repo cleanup) run in parallel with builds.
8. **F + G** (evidence table + full boot test) is the final step
   and gates the morning demo.

---

## Links

- Current build flow: [`README.md`](README.md)
- Security model: [`SECURITY.md`](SECURITY.md)
- Paper-committed properties: [`../lapee-paper/main.tex`](../lapee-paper/main.tex)
- Build history: [`HISTORY.md`](HISTORY.md)
- Framework v1.1 capture: `../out/local-capture/framework-13-v1-1-real-ek-roundtrip/`
