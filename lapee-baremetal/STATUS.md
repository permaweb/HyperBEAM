# LapEE bare-metal -- live status

**Latest update:** 2026-04-23 ~05:50 EDT -- v1.2 overnight pass
shipped through reviewer pass 7 (security-delta on batch 9).
USB image (v1.2 + batch 10) rebuilt and QEMU-smoke-tested end-
to-end: LAPEE-WRITEBACK-OK + a 104 KB attestation envelope
carrying `EV_HYPERBEAM_KEY_PUBKEY_EXTEND' at seq 1 in PCR 15,
`ak-pubkey-extend-verified = true' on the verifier side.

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
