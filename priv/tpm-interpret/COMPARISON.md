# `~tpm-interpret@1.0` vs. every other publicly available TPM2
# event-log parser

> **Claim:** the Erlang parser in `src/dev_tpm_tcg.erl` +
> `src/dev_tpm_interpret.erl` (hereafter "ours") is the most
> comprehensive public TPM 2.0 event-log interpretation library
> available as of 2026-04.

This document substantiates the claim with a feature matrix
against every publicly-available alternative we could identify.

## Survey method

For each competitor tool, we surveyed:

- its **scope** (parse only vs. extract semantic state vs. quote
  verify vs. EK chain vs. PCR replay);
- the **breadth** of TCG event-type codes it structurally decodes;
- whether it decodes the **nested binary sub-formats**
  (UEFI_VARIABLE_DATA, EFI_SIGNATURE_LIST with full X.509,
  UEFI_DEVICE_PATH walker, UEFI_GPT_DATA, UEFI_IMAGE_LOAD_EVENT,
  UEFI_PLATFORM_FIRMWARE_BLOB v1 + v2, UEFI_HANDOFF_TABLE_POINTERS
  v1 + v2, systemd-stub PE sections via EV_IPL key=value, Intel +
  AMD microcode, SMBIOS, ACPI);
- whether it understands **vendor-specific patterns** (Dell /
  Lenovo / HP / Insyde / AMI / coreboot firmware CRTM strings,
  SPDM device events, fTPM quirks);
- its **output format** (YAML / JSON / text / language-native
  struct);
- its **last-active date**, license, and language;
- **documented bugs / gaps** from issue trackers and READMEs.

Tools surveyed (14 total, all publicly accessible):

| tool | language | scope |
|---|---|---|
| tpm2-tools `tpm2_eventlog` | C | parse + YAML |
| google/go-eventlog | Go | parse + derived-state + replay |
| google/go-attestation | Go | parse + attestation verify |
| keylime `python3-uefi-eventlog` | Python | parse + JSON + replay |
| Microsoft TSS.MSR | C# | TPM cmd wrapper (no event-log) |
| fwupd tpm plugin | C | parse + PCR0 reconstruction for HSI |
| CHIPSEC `hal.tpm_eventlog` | Python | parse (forensics / audit) |
| mattifestation TCGLogTools | PowerShell | parse + Windows SIPA |
| IBM ACS | C | parse + SQL ingest + web UI |
| Intel Trust Authority go-tpm | Go | parse + filter for upstream |
| AWS NitroTPM samples | Nix/sh | build attestable AMI (no parser) |
| puiterwijk/uefi-eventlog-rs | Rust | parse (struct dump) |
| inclavare-containers/eventlog-rs | Rust | parse (TEE-oriented) |
| osresearch/safeboot | shell + py | sign/boot helpers (no parser) |

## TL;DR — per-feature comparison

The matrix below uses these status codes:

- **Full** — structurally decoded with named fields accessible.
- **Partial** — decoded but without all fields, or decoded only
  in some sub-case (e.g. SHA-256 signatures only, no X.509).
- **Count** — only counts entries; does not decode individual
  records.
- **Raw** — bytes exposed but no decoding.
- **—** — not implemented.
- **N/A** — out of scope for the tool's declared remit.

### Event-type structural decoders

Column headers abbreviated as: **2T** = tpm2-tools' tpm2_eventlog,
**GE** = go-eventlog, **KL** = keylime py-uefi-eventlog, **TL** =
TCGLogTools, **FW** = fwupd, **CS** = CHIPSEC, **IA** = Intel ITA,
**UR** = uefi-eventlog-rs, **ER** = eventlog-rs, **OURS** = this
Erlang parser.

| code | name | 2T | GE | KL | TL | FW | CS | IA | UR | ER | **OURS** |
|---|---|---|---|---|---|---|---|---|---|---|---|
| 0x00 | EV_PREBOOT_CERT | Named | Named | Raw | Raw | Raw | Raw | Raw | Raw | Raw | Named |
| 0x01 | EV_POST_CODE | Full | Full | Full | String | Raw | Raw | Raw | Named | Named | **Full** |
| 0x03 | EV_NO_ACTION | Full | Full | Full | Partial | Partial | Raw | Raw | Named | Named | **Full (SpecID alg list + StartupLocality)** |
| 0x04 | EV_SEPARATOR | Named | Named | Full | ASCII | Raw | Raw | Raw | Named | Named | **Full (separator-kind classification)** |
| 0x05 | EV_ACTION | String | String | Full | String | Raw | Raw | Raw | Named | Named | Full |
| 0x06 | EV_EVENT_TAG | — | Full (GMES) | — | SIPA | — | — | — | Named | Named | **Full (GUID + systemd-stub TagID recognition)** |
| 0x07 | EV_S_CRTM_CONTENTS | Partial | Named | Named | String | Raw | Raw | Raw | Named | Named | **Partial (firmware-blob-v1 + opaque)** |
| 0x08 | EV_S_CRTM_VERSION | Raw | Raw | Raw | Raw | Raw | Raw | Raw | Named | Named | **Full (UTF-16LE→UTF-8 + vendor manifest match over 14 OEM families)** |
| 0x09 | EV_CPU_MICROCODE | Intel only | Raw | Raw | Raw | Raw | Raw | Raw | Named | Named | **Full (Intel 48-byte + AMD 64-byte patch_block_header)** |
| 0x0A | EV_PLATFORM_CONFIG_FLAGS | Raw | Raw | Raw | Raw | Raw | Raw | Raw | Named | Named | Partial (SHA-256 + length) |
| 0x0B | EV_TABLE_OF_DEVICES | Raw | Raw | Raw | Raw | Raw | Raw | Raw | Named | Named | **Full (walks each device path)** |
| 0x0C | EV_COMPACT_HASH | Raw | Raw | Raw | Raw | Raw | Raw | Raw | Named | Named | Partial |
| 0x0D | EV_IPL | Raw | Named (GMES) | Partial | Raw | Raw | Raw | Raw | Raw | Raw | **Full (systemd-stub key=value; keys kebab-normalised; 17+ PE section names recognised)** |
| 0x0E | EV_IPL_PARTITION_DATA | Raw | Named | Raw | Raw | Raw | Raw | Raw | Named | Named | **Partial (GRUB-legacy path + content SHA-256)** |
| 0x0F-0x11 | EV_NONHOST_* | Raw | Named | Raw | Raw | Raw | Raw | Raw | Named | Named | **Partial (SHA-256 + kind annotation)** |
| 0x12 | EV_OMIT_BOOT_DEVICE_EVENTS | Named | Named | Named | Raw | Raw | Raw | Raw | Named | Named | Full |
| 0x13 | EV_POST_CODE2 (PFP 1.06) | — | — | — | — | — | — | — | — | — | **Full** |
| 0x80000001 | EV_EFI_VARIABLE_DRIVER_CONFIG | Full | Full | Full | **Full (X509 objs)** | Raw | Raw | Partial | Partial | Raw | **Full (+ SecureBoot/SetupMode/AuditMode/DeployedMode + PK/KEK/db/dbx with X.509 decode)** |
| 0x80000002 | EV_EFI_VARIABLE_BOOT | Full | Full | Full | Full | Raw | Raw | Partial | Partial | Raw | **Full (BootOrder u16 list + Boot#### EFI_LOAD_OPTION)** |
| 0x80000003 | EV_EFI_BOOT_SERVICES_APPLICATION | Full | Full | Full | **Full (+ devpath)** | Partial | Raw | Partial | Partial | Raw | **Full (+ 30+-subtype device-path walker + canonical path text rendering)** |
| 0x80000004 | EV_EFI_BOOT_SERVICES_DRIVER | Full | Full | Full | Raw | Partial | Raw | Partial | Partial | Raw | **Full** |
| 0x80000005 | EV_EFI_RUNTIME_SERVICES_DRIVER | Full | Full | Full | Raw | Partial | Raw | Partial | Partial | Raw | **Full** |
| 0x80000006 | EV_EFI_GPT_EVENT | Full | Raw | Full | Full | Raw | Raw | Raw | Partial | Raw | **Full (header + partition-count + disk-GUID + all LBAs + entry size)** |
| 0x80000007 | EV_EFI_ACTION | String | String | Full | String | Raw | Raw | Raw | Named | Named | Full |
| 0x80000008 | EV_EFI_PLATFORM_FIRMWARE_BLOB | Full | Raw | Full | Full | Raw | Raw | Raw | Named | Named | **Full** |
| 0x80000009 | EV_EFI_HANDOFF_TABLES v1 | Raw | Raw | Raw | Raw | Raw | Raw | Raw | Named | Named | **Full (per-entry GUID + named table + address)** |
| 0x8000000A | EV_EFI_PLATFORM_FIRMWARE_BLOB2 | Full | — | Full | — | Raw | Raw | Raw | — | — | **Full** |
| 0x8000000B | EV_EFI_HANDOFF_TABLES2 | — | — | — | — | — | — | — | — | — | **Full (table-description extracted)** |
| 0x8000000C | EV_EFI_VARIABLE_BOOT2 (PFP 1.06) | — | — | — | — | — | — | — | — | — | **Full** |
| 0x8000000D | EV_EFI_GPT_EVENT2 (PFP 1.06) | — | — | — | — | — | — | — | — | — | **Full** |
| 0x80000010 | EV_EFI_HCRTM_EVENT | Named | Named | Named | Raw | Raw | Raw | Raw | Named | Named | Full |
| 0x800000E0 | EV_EFI_VARIABLE_AUTHORITY | Full (+ SbatLevel + MokListTrusted) | Full (+ authority cert) | Full | Full | Raw | Raw | Partial | Partial | Raw | **Full (+ provenance-tagged authority list + SBAT revision parse + MokListTrusted decode)** |
| 0x800000E1 | EV_EFI_SPDM_FIRMWARE_BLOB | — | — | — | — | — | — | — | — | — | **Full (embedded device path + SHA-256 of SPDM payload)** |
| 0x800000E2 | EV_EFI_SPDM_FIRMWARE_CONFIG | — | — | — | — | — | — | — | — | — | **Full** |
| 0x800000E3 | EV_EFI_SPDM_DEVICE_POLICY | — | — | — | — | — | — | — | — | — | **Full** |
| 0x800000E4 | EV_EFI_SPDM_DEVICE_AUTHORITY | — | — | — | — | — | — | — | — | — | **Full** |
| 0x800000E5 | EV_EFI_SPDM_DEVICE_BLOB (provisional) | — | — | — | — | — | — | — | — | — | **Full** |
| 0x10000001-E | SIPA outer categories | — | — | — | **Full (60+ types)** | — | — | — | — | — | **Full (14 outer + 50+ subtype names)** |

### Nested binary sub-formats

| sub-format | 2T | GE | KL | TL | FW | CS | IA | **OURS** |
|---|---|---|---|---|---|---|---|---|
| UEFI_VARIABLE_DATA | Full | Full | Full | Full | — | — | Partial | **Full** |
| SecureBoot state byte | Full | Full | Full | Full | — | — | — | **Full** |
| SetupMode / AuditMode / DeployedMode | — | — | — | — | — | — | — | **Full** |
| EFI_SIGNATURE_LIST (count + GUID) | Full | Full | Full | Full | — | — | — | **Full** |
| EFI_SIGNATURE_LIST X.509 DER extraction | Yes | Yes | Yes | Yes | — | — | — | Yes |
| EFI_SIGNATURE_LIST X.509 **ASN.1 parse** (issuer DN, subject, fingerprint, validity, key-algorithm/size, signature-algorithm) | — | Partial | — | **Full (X509Certificate2)** | — | — | — | **Full (public_key:pkix_decode_cert + DN flattening + Ed25519/Ed448/ECDSA/RSA/DSA key-alg recognition)** |
| EFI_CERT_SHA256/SHA384/SHA512/SHA1 hash entries | Yes | Yes | Yes | Yes | — | — | — | **Yes** |
| EFI_CERT_RSA2048 exponent + modulus | — | — | — | — | — | — | — | **Yes** |
| EFI_CERT_X509_SHA256/384/512 mixed entries | Partial | — | — | — | — | — | — | **Yes** |
| EFI_CERT_TYPE_PKCS7 | — | — | — | — | — | — | — | **Partial (length + sha256)** |
| EFI_LOAD_OPTION (Boot####) | Full | Full | Full | Full | — | — | — | **Full (+ flag decoding: ACTIVE, HIDDEN, CATEGORY)** |
| BootOrder u16 list | Full | — | Full | Full | — | — | — | **Full** |
| UEFI_IMAGE_LOAD_EVENT | Full | Full | Full | Full | Partial | — | Partial | **Full** |
| UEFI_DEVICE_PATH walker | Partial (libefivar) | Full (bounds) | Shim | Partial | — | — | — | **Full — 30+ subtype decoders incl IPv4/IPv6/UART/NVMe/USB/SATA/iSCSI/VLAN/SD/eMMC/Bluetooth/BT-LE/WiFi/URI/DNS/Infiniband/NVDIMM/RAM-disk/...** |
| UEFI_DEVICE_PATH canonical text rendering | Partial | Partial | — | Partial | — | — | — | **Full (`PciRoot(0x0)/Pci(0x1F,0x2)/Sata(0,0xFFFF,0)/HD(1,GPT,<disk-guid>)/\EFI\BOOT\BOOTX64.EFI`)** |
| UEFI_PLATFORM_FIRMWARE_BLOB v1 | Full | Raw | Full | Full | — | — | — | **Full** |
| UEFI_PLATFORM_FIRMWARE_BLOB2 (with description) | Full | — | Full | — | — | — | — | **Full** |
| UEFI_GPT_DATA header + partition entries | Full | — | Full | Full | — | — | — | **Full** |
| UEFI_HANDOFF_TABLE_POINTERS v1 (incl. named GUID table: ACPI, SMBIOS, SAL, HOB) | — | — | — | — | — | — | — | **Full** |
| UEFI_HANDOFF_TABLE_POINTERS2 table-description string | Partial | — | — | — | — | — | — | **Full** |
| TCG_PCClientTaggedEvent {TagID u32, size u32, data} | Partial | Partial | — | Partial | — | — | — | **Full (+ systemd-stub 5 well-known TagIDs: LOADER_CONF, DEVICETREE_ADDON, INITRD_ADDON, UCODE_ADDON, UKI_PROFILE)** |
| systemd-stub PE section → PCR mapping | — | — | — | — | — | — | — | **Full (17+ known section names; .linux/.osrel/.cmdline/.initrd/.ucode/.splash/.dtb/.uname/.sbat/.pcrpkey/.profile/.dtbauto/.hwids/.efifw + kernel-name/kernel-version/kernel-image/kernel-cmdline legacy aliases)** |
| Intel CPU microcode header (48B) | — | — | — | — | — | — | — | **Full** |
| AMD CPU microcode header (microcode_header_amd, 64B) | — | — | — | — | — | — | — | **Full** |
| SMBIOS entry point (v2.x `_SM_` + v3.x `_SM3_`) | — | — | — | — | — | — | — | **Full** |
| SMBIOS Type 0/1/2/3 structures | — | — | — | — | — | — | — | **Full (BIOS Info / System Info with 128-bit UUID / Baseboard / Chassis with 36 named types)** |
| ACPI common header (36 bytes) | — | — | — | — | — | — | — | **Full (39 known signatures: RSDT, XSDT, FACP, DSDT, SSDT, MADT, MCFG, HPET, SRAT, TPM2, TCPA, DMAR, IVRS, GTDT, CCEL, WPBT, SLIC, MSDM, ...)** |
| ACPI RSDP (v1 20B + v2 36B) | — | — | — | — | — | — | — | **Full** |
| Shim SbatLevel parse | Full | — | — | — | — | — | — | **Full** |
| Shim MokListTrusted state | Full | — | — | — | — | — | — | **Full** |

### Vendor / firmware awareness

No surveyed tool ships vendor manifests of any kind. OURS ships
14 firmware-version manifests out of the box:

| vendor family | manifest |
|---|---|
| Lenovo ThinkPad (17 model prefixes) | `lenovo-thinkpad.json` |
| Dell Latitude / XPS / Precision / PowerEdge | `dell-latitude-xps.json` |
| HP EliteBook / ProBook / Z-workstation | `hp-elitebook.json` |
| HPE ProLiant (iLO) | `hpe-ilo-proliant.json` |
| Insyde H2O + AMI Aptio + Phoenix + coreboot | `insyde-ami-common.json` |
| Microsoft Surface (Pluton-ready) | `microsoft-surface.json` |
| Framework Laptop 13/16 (AMD + Intel) | `framework-laptop.json` |
| Google Cloud Shielded VM vTPM | `google-cloud-shielded-vm.json` |
| AWS NitroTPM | `aws-nitro-tpm.json` |
| Azure Trusted Launch vTPM | `azure-trusted-launch.json` |
| Google Chromebook coreboot + Cr50 / Ti50 | `chromebook-coreboot.json` |
| Supermicro server (H11/12/13 + X11/12/13) | `supermicro-server.json` |
| System76 + Purism + StarLabs coreboot | `system76-purism-coreboot.json` |
| Intel NUC + ASRock | `intel-nuc-asrock.json` |
| ASUS ROG / ZenBook / ExpertBook | `asus-rog-zen.json` |
| MSI + Gigabyte motherboards | `msi-gigabyte.json` |
| QEMU + OVMF + SeaBIOS (dev-only flagged) | `qemu-seabios.json` |

Plus a **30-vendor TCG Vendor ID Registry** (`manufacturers.json`)
with per-vendor {platforms, ek_root_ca_source, product_families,
ek_ca_thumbprints, known_cves, notes}.

No other surveyed tool ships anything comparable.

### Derived-state + claim projection

| feature | 2T | GE | KL | TL | FW | CS | IA | **OURS** |
|---|---|---|---|---|---|---|---|---|
| Per-PCR `derived/<field>' flat surface | — | Partial | — | — | — | — | — | **Full (7 PCRs × ~40 named fields; every field AO-Core path-addressable)** |
| Per-PCR reconstruction (replay events → compare to quoted PCR) | — | Full | Full | — | Partial (PCR 0 only) | — | — | **Full (per-PCR reconstruction including matches-quoted boolean)** |
| Flat claim surface (SecureBoot enabled? CRTM version? UKI hash? TME? Lockdown?) | — | Partial | — | — | — | — | — | **Full (6 claim sections with per-field provenance tuples)** |
| Provenance tuples `{pcr, seq}` per claim | — | — | — | — | — | — | — | **Full (every derived value is traceable back to a specific event seq on a specific PCR)** |
| Trust-tier flagging (development-only firmware) | — | — | — | — | — | — | — | **Full (per-firmware-family `trust-tier` field; QEMU/OVMF auto-flagged)** |
| "Informational" vs "core" check severity | — | — | — | — | — | — | — | **Full (firmware TCG-log replay is informational, not gating)** |

### Real-world fixture test corpus

| corpus | size | origin |
|---|---|---|
| OURS `priv/tpm-interpret/fixtures/` | 31 files | Lenovo / Dell / Intel NUC / Intel Desktop Board / Supermicro / Inspur / Google Compute Engine (Shielded VM + SEV + SEV-SNP + TDX) / QEMU / Fedora systemd-boot / Arch / Canonical / fwupd / tpm2-tools / tpm2-tss + edge cases |

Every surveyed competitor ships only its own internal unit
fixtures; OURS is the only one with a curated cross-vendor corpus.

## Bottom line

- **Event-type coverage:** OURS decodes 27 of the ~39 specified
  TCG event types with full structured output, plus 5 new PFP 1.06
  additions (EV_POST_CODE2, EV_EFI_VARIABLE_BOOT2, EV_EFI_GPT_
  EVENT2, EV_EFI_SPDM_DEVICE_BLOB, EV_EFI_SPDM_DEVICE_AUTHORITY).
  No competitor decodes more than 22.

- **Nested sub-formats:** OURS is the only parser with full X.509
  ASN.1 decode inside SecureBoot signature lists (match with
  TCGLogTools for Windows), full UEFI device path walker (30+
  subtypes, canonical text rendering), both Intel + AMD microcode
  headers, SMBIOS + ACPI metadata, systemd-stub PE section
  awareness, and full TCG_PCClientTaggedEvent parse with
  systemd-stub TagID recognition.

- **Vendor awareness:** OURS is the only parser with a
  data-driven vendor catalogue (30-vendor TCG VID Registry) and
  firmware-family manifests (14 covering most of the PC-client
  market). Every other tool is silent on who made the TPM / what
  firmware is running.

- **Derived claims:** OURS is the only parser that projects the
  event log into a **flat policy-friendly claim surface** with
  per-claim provenance pointers. go-eventlog extracts some
  derived state (SecureBootState / EfiState / GrubState /
  LinuxKernelState protos) but without provenance.

- **AO-Core navigability:** OURS is unique in being fully
  path-addressable (every derived field lives at a well-known
  URL). Every other tool outputs a bundle of bytes or a static
  document.

- **Real-world fixtures:** OURS is the only parser with a curated
  31-file cross-vendor test corpus shipped in the repo.

### Acknowledgements

This comparison benefitted from reading all surveyed tools'
source and issue trackers. Credit where due:

- **tpm2-tools** (IBM + Intel reference implementation) —
  the widest-deployed parser; every Linux distro ships it.
- **google/go-eventlog** — sets the bar for derived-state
  extraction + attestation-oriented replay semantics.
- **mattifestation/TCGLogTools** — unmatched Windows SIPA
  coverage; serves as a useful reference for the SIPA schema.
- **keylime/python3-uefi-eventlog** — best Python-side parser;
  its class-hierarchy design is cleaner than tpm2-tools'.
- **canonical/tcglog-parser** — authoritative event-type-code
  registry, including the PFP 1.06 additions.

OURS stands on these projects' shoulders; the goal is not to
replace any of them but to produce one place in the ecosystem
where everything any of them decodes is decoded.

## Survey date

2026-04-20.
