# `~tpm-interpret@1.0` — coverage audit

Honest answer to: *how much real-world hardware can this device usefully
interpret today, and what's missing?*

## Breadth — which vendors, which firmware

### TPM manufacturer IDs (`manufacturers.json`)

**State: comprehensive.** 30 TCG-registered vendor IDs, covering every
vendor whose TPM you're likely to encounter on consumer / enterprise /
server hardware. Each entry carries `{name, kind, notes}` with known
CVEs where relevant.

| tier | vendors |
|---|---|
| discrete TPM 2.0 | Infineon (SLB 9670/9672), STMicroelectronics (ST33), Nuvoton (NPCT7xx), Nationz, Atmel/Microchip, Broadcom (BCM0102), Flyslice |
| fTPM in CPU | AMD (PSP), Intel (PTT), Samsung, Qualcomm, Rockchip, SMSC, Huawei (HiSilicon), Fuzhou, Sinosun |
| server platform | HPE iLO, IBM, Cisco UCS, Lenovo ThinkSystem |
| virtual / software | swtpm (dev), vTPM under Azure / AWS Nitro / GCP Shielded VMs |

### TPM firmware/CRTM identifier strings (`firmware-versions/`)

**State: seed catalogue.** 5 shipped files covering the major OEM
and third-party UEFI stacks:

| file | matches | OEMs covered |
|---|---|---|
| `lenovo-thinkpad.json` | `N<model>HT<rev>` UTF-16LE | ThinkPad X/T/P-series (12 model prefixes enumerated) |
| `dell-latitude-xps.json` | `Dell Inc.`/`Dell Computer` ASCII | Latitude, XPS, Precision |
| `hp-elitebook.json` | `HP`/`HPQ` ASCII | EliteBook, ProBook, Z-workstation |
| `insyde-ami-common.json` | `INSYDE Corp.`, `American Megatrends`, `Phoenix`, `coreboot` | Framework, Acer, Asus, MSI, Gigabyte, Chromebooks, System76, Purism |
| `qemu-seabios.json` | `SeaBIOS`, `EDK II` | Dev-only; explicitly flagged `trust-tier: development-only` |

This is a **data-driven** layer — each additional entry is a JSON file
with no code changes. Coverage grows as operators onboard new
platforms. See `firmware-versions/README.md` for the schema.

### PCR profiles (`pcr-profiles/`)

**State: 1 populated (QEMU SeaBIOS), rest to be captured per platform.**

A PCR profile pins **expected PCR 0 + PCR 7 digests** for a known-
good measured boot. A verifier uses this to decide "this boot's
firmware + SecureBoot config matches profile X". The matching
happens in `dev_tpm_interpret:match_pcr_profile/2`.

Populating these requires **real measurements from real hardware** —
we haven't yet sampled that. Mechanism is in place; adding a profile
is a JSON file drop + release rebuild. Priority order for onboarding:

1. AMD Ryzen 7040 / 8040 fTPM (Framework 13 + Lenovo ThinkPad Z-series)
2. Intel Core 13th/14th gen PTT (ThinkPad X1 Carbon Gen 11/12, Dell XPS 13)
3. AMD EPYC fTPM (server / cloud)
4. Infineon discrete on ThinkPad X/T-series (enterprise)
5. Nuvoton on Framework / consumer boards

### Vendor root CAs (`root-cas/`)

**State: empty — deployer-provisioned.**

Each deployer supplies the vendor root CAs they trust. Files are dropped
in `priv/tpm-interpret/root-cas/*.pem` and loaded at node start. Known
sources:

| vendor | CA source | licensing |
|---|---|---|
| Infineon | https://pki.infineon.com/ | Free, CC-BY |
| STMicroelectronics | https://www.st.com/content/st_com/en/products/embedded-software/stm32-embedded-software/stm32cube-expansion-software-for-security/x-cube-sbsfu.html | Free |
| Nuvoton | https://www.nuvoton.com/security/tpm/ | Free registration |
| AMD fTPM | via AMD Platform Secure Boot docs (NDA for some generations) | Restricted |
| Intel PTT | Microsoft Windows update delivers; manual extract | Free |
| Lenovo | https://pki.lenovo.com/ | Free |
| Dell | Driver pack 7W5V2 | Free |

## Depth — how much of each attestation is turned into named fields

### Event-type decoders (`dev_tpm_tcg`)

**State: 16 of the ~30 TCG event types get structured decoding.** Every
event gets a minimal record (`pcr`, `seq`, `event-type`, `digests`,
`event-data`). Events with decoders also populate a `parsed` submap.

| event type code | mnemonic | decoder status |
|---|---|---|
| `0x01` | EV_POST_CODE | ASCII / bytes discriminator |
| `0x03` | EV_NO_ACTION | SpecID header parse (alg list, major/minor) |
| `0x04` | EV_SEPARATOR | `0xFFFFFFFF`/`0x00000000`/other → `separator-kind` |
| `0x05` | EV_ACTION | ASCII string parse |
| `0x06` | EV_EVENT_TAG | **NEW** GUID parse + tag categorisation |
| `0x08` | EV_S_CRTM_VERSION | UTF-16LE → UTF-8 (best-effort ASCII fallback) |
| `0x09` | EV_CPU_MICROCODE | Intel header layout (48B) |
| `0x0D` | EV_IPL | systemd-stub key=value (keys normalised to kebab) |
| `0x12` | EV_OMIT_BOOT_DEVICE_EVENTS | ASCII |
| `0x80000001` | EV_EFI_VARIABLE_DRIVER_CONFIG | UEFI_VARIABLE_DATA + SecureBoot/PK/KEK/db/dbx semantics |
| `0x80000002` | EV_EFI_VARIABLE_BOOT | **NEW** BootOrder (u16 list) + Boot#### (EFI_LOAD_OPTION) |
| `0x80000003` | EV_EFI_BOOT_SERVICES_APPLICATION | UEFI_IMAGE_LOAD_EVENT |
| `0x80000004` | EV_EFI_BOOT_SERVICES_DRIVER | UEFI_IMAGE_LOAD_EVENT |
| `0x80000005` | EV_EFI_RUNTIME_SERVICES_DRIVER | UEFI_IMAGE_LOAD_EVENT |
| `0x80000006` | EV_EFI_GPT_EVENT | **NEW** EFI_PARTITION_TABLE_HEADER (disk GUID + LBAs + partition count) |
| `0x80000007` | EV_EFI_ACTION | ASCII |
| `0x80000008` | EV_EFI_PLATFORM_FIRMWARE_BLOB | address + length (u64) |
| `0x8000000A` | EV_EFI_PLATFORM_FIRMWARE_BLOB2 | description + address + length |
| `0x8000000B` | EV_EFI_HANDOFF_TABLES2 | **NEW** table description (ACPI/SMBIOS) |
| `0x80000010` | EV_EFI_HCRTM_EVENT | `HCRTM` ASCII marker |
| `0x800000E0` | EV_EFI_VARIABLE_AUTHORITY | UEFI_VARIABLE_DATA |

**Gaps** (events with no structured decoder, fall through to
opaque `{}`):

| event type | status | reason |
|---|---|---|
| `0x07` EV_S_CRTM_CONTENTS | no decode | Format is vendor-specific; no canonical shape |
| `0x0A` EV_PLATFORM_CONFIG_FLAGS | no decode | Firmware-specific flag bits; varies per vendor |
| `0x0B` EV_TABLE_OF_DEVICES | no decode | Device-path array; need UEFI_DEVICE_PATH walker |
| `0x0C` EV_COMPACT_HASH | no decode | Rarely seen in modern logs |
| `0x0E` EV_IPL_PARTITION_DATA | no decode | GRUB-legacy, not seen on systemd-boot systems |
| `0x0F`-`0x11` EV_NONHOST_* | no decode | ME / SMM / PSP — firmware-proprietary |
| `0x80000009` EV_EFI_HANDOFF_TABLES (v1) | no decode | Deprecated in favour of v2 |
| `0x80000011`-`0x8000001F` EV_EFI_SPDM_* | no decode | Rare; SPDM-capable device firmware |

### AMD CPU microcode

Intel's EV_CPU_MICROCODE header layout is implemented (48-byte signed
header: header-version, update-revision, date, processor-signature,
checksum, loader-revision, processor-flags, data-size, total-size).
**AMD's microcode layout differs** — AMD uses a `patch_block_header`
format. Not yet implemented. On AMD Ryzen / EPYC hosts, PCR 1's
CPU microcode event currently surfaces as opaque bytes.

### UEFI EFI_SIGNATURE_LIST

Currently **counts** signatures per list type (PK/KEK/db/dbx) and
reports per-list `{type-guid, entry-count, entry-size}`. **Does not
decode individual X.509 certs** — that's a bigger lift (ASN.1 DER
parse + fingerprinting). Useful follow-up: for `db` and `dbx`, extract
issuer DN + SHA-256 fingerprint per cert so a policy engine can match
against a known-good authority list.

### IMA per-file events (PCR 10)

**State: not transported.** Only the final PCR 10 value is in the
envelope. The per-file IMA chain (which binary loaded which module,
in what order, against what policy) is collected by the kernel but
not yet read into the envelope. A future `~tpm@2.0a` envelope
version will include `ima-event-log` alongside the TCG firmware log.

## Per-PCR derived-field coverage

| PCR | role | derived-template fields | populated on SeaBIOS/QEMU? |
|---|---|---|---|
| 0 | firmware-srtm | crtm-version, hcrtm, post-codes, firmware-blobs, separator-seen, spec-id | partial (spec-id, separator) |
| 1 | platform-firmware-config | cpu-microcode, uefi-boot-order, separator-seen | separator only |
| 2 | option-rom-code | option-rom-scanned, separator-seen | both (via EV_ACTION) |
| 3 | option-rom-config | separator-seen | yes |
| 4 | boot-loader-code | boot-services-applications, boot-action-markers, separator-seen | separator + markers |
| 5 | boot-loader-config | gpt-partition-tables, separator-seen | separator only (no GPT on QEMU) |
| 7 | secure-boot-policy | secure-boot-enabled, pk/kek/db/dbx-entry-count, authorities, separator-seen | separator only (no EFI on QEMU SeaBIOS) |
| 8/9 | grub-legacy | grub-cmdline, grub-modules | no (systemd-boot path) |
| 10 | ima-runtime | ima-active, ima-event-count, ima-files-measured + note | partial — final digest only |
| 11 | uki-kernel-image | uki-measured, uki-image-hash, uki-kernel-version | no (no UKI on dev guest) |
| 12 | uki-kernel-cmdline | uki-cmdline, uki-initrd-hash | no |
| 13 | uki-system-extensions | uki-sysext-count | no |
| 14 | secure-boot-authority-mok | mok-entry-count | no |
| 15 | lapee-node-identity | lapee-node-identity-committed | **yes** — this is LapEE's trust anchor |

**The shape is always there** — every PCR carries its full derived-
fields template, so a consumer can rely on `pcrs/<N>/derived/<field>`
being a navigable path regardless of whether the current attestation
populates it. Missing evidence shows as the sentinel `"unknown"`
binary; empty collections show as `[]` / `0`.

## Bottom-line assessment

| axis | state | notable gaps |
|---|---|---|
| Vendor breadth (TPMs) | **good** — 30 vendors, tiered by kind | per-vendor root CAs are deployer-supplied |
| Firmware breadth (CRTM) | **seed** — 5 OEM+third-party families | per-platform PCR profiles |
| Event-type depth | **16/~30 decoded** — covers every type seen on a typical systemd-boot x86_64 system | AMD microcode, full device-path walker, IMA transport, full X.509 decode for SecureBoot lists |
| AO-Core navigability | **complete** — every derivable field is path-addressable; unknowns are honest sentinels | n/a — this is the primary guarantee |
| Trust-tier flagging | **in place** — `development-only` marker for QEMU/OVMF | |

Where the breadth goes next:

1. **Per-platform PCR profiles** — data-only additions as platforms
   are onboarded. Each profile is a ~30-line JSON file.
2. **Per-vendor root CA bundles** — operator-supplied on deploy.
3. **AMD CPU microcode decoder** + **full X.509 decode for SecureBoot
   db/dbx** — code additions, ~200 LoC each.
4. **IMA per-file event transport** — envelope schema bump in
   `dev_tpm2`.

Every one of these is a localised change, not a re-architecture.
