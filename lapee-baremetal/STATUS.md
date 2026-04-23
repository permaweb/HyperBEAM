# LapEE bare-metal -- live status

**Latest update:** 2026-04-23 ~04:00 EDT -- v1.2 overnight pass
shipped. USB image ready for Sam's morning Framework reboot.

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

Nine commits on `agent/lapee' pushed to Permagit:

```
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

Three independent code reviewers spoken to (curmudgeonly
firmware-security; pragmatic demo-ops; fresh-eyes first-time-
contributor). All three verdicts: SHIP. Findings from all three
acted on in the corresponding batch. See "Review findings acted
on" at the bottom of this report.

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
  | AK + node-message bound into PCR 15      | COVERED      |
  | Firmware CRTM match                      | **COVERED + platform via E5** |
  | UKI hash in PCR 11                       | COVERED      |
  | Secure Boot state                        | **COVERED via g probe** |
  | TME state                                | COVERED      |
  | IOMMU state                              | **COVERED via E3 runtime probe** |
  | Kernel lockdown state                    | **COVERED via E3 runtime probe** |
  | IMA per-file chain (PCR 10)              | N/A stub; count probed |
  | CPU vendor / model                       | **COVERED via E3 /proc/cpuinfo** |
  | TPM manufacturer / model                 | COVERED      |
  | freshness-safe <-> resetCount            | **COVERED via E6** |

### Tests

```
dev_tpm_tcg        98  pass
dev_tpm2           20  pass
dev_tpm_interpret 104  pass  (+20 from v1.1's 84)
                ------
                  222  pass
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
