# `~tpm-interpret@1.0` — authoritative coverage reference

Comprehensive catalogue of every TPM 2.0 event / variable / structure
this library decodes, with citations to the primary sources for each.
Maintained alongside the code; each entry names the module + function
that implements it.

> **Where this differs from COMPARISON.md:** COMPARISON.md ranks us
> against other parsers. COVERAGE.md is the self-referential catalogue
> of *what we do*, with citations, so an integrator knows exactly
> what they can expect from the library.

## Scope

We decode:

1. The TCG PC Client Platform Firmware Profile event log format,
   both the **TPM 1.2 legacy** layout (TCG_PCR_EVENT) and the
   modern **TPM 2.0 crypto-agile** layout (TCG_PCR_EVENT2 preceded
   by a SpecID NO_ACTION record).
2. Every published **TCG event-type code** (PFP 1.06 + EFI
   extension range 0x80000000-0x800000FF + Windows SIPA range
   0x10000000-0x10FFFFFF). Spec-inferred values fall through as
   "opaque but typed" records.
3. Every **nested binary sub-format** referenced inside an event's
   data field: UEFI_VARIABLE_DATA, EFI_SIGNATURE_LIST (with full
   ASN.1 X.509 parse per entry), UEFI_DEVICE_PATH (30+ sub-types),
   UEFI_LOAD_OPTION, UEFI_GPT_DATA header + partition entries,
   UEFI_IMAGE_LOAD_EVENT, UEFI_PLATFORM_FIRMWARE_BLOB v1 + v2,
   UEFI_HANDOFF_TABLE_POINTERS v1 + v2, TCG_PCClientTaggedEvent,
   Intel + AMD CPU microcode headers, SMBIOS v2.x + v3.x, ACPI
   header + RSDP v1 + v2.
4. **Vendor-specific patterns** via data files under
   `firmware-versions/` (14 OEM families) and `manufacturers.json`
   (30 TPM vendors, full TCG Vendor ID Registry v1.06).
5. **Derived-claim projection** — every decoded field folds into a
   flat policy-friendly `claim.*` surface with per-claim provenance
   tuples pointing back at the source event.

## Sources

Primary references, cited inline below:

- **[PFP]** TCG PC Client Platform Firmware Profile Specification,
  Level 00, Revisions 1.05 and 1.06.
- **[UEFI]** UEFI Specification 2.10 (chapters 7, 8, 10, 22, 28,
  32).
- **[ACPI]** ACPI Specification 6.5.
- **[SMBIOS]** DMTF DSP0134 SMBIOS 3.8.0.
- **[TCG-ALG]** TCG Algorithm Registry.
- **[TCG-OID]** TCG EK Credential Profile Specification v2.5 + TCG
  VID Registry v1.06.
- **[INTEL-SDM]** Intel 64 and IA-32 Architectures Software
  Developer's Manual, Volume 3A, Chapter 9.
- **[LINUX]** Linux kernel source, primarily
  `drivers/char/tpm/eventlog/`, `include/linux/tpm_eventlog.h`,
  `arch/x86/kernel/cpu/microcode/{intel.c,amd.c}`.
- **[SYSTEMD]** systemd source `src/boot/efi/stub.c` and
  `src/fundamental/tpm2-util.h`.

## 1. Event-type codes

Mapping: code → mnemonic → implementation clause → [spec] → test
fixture. Status legend: **✓** full structured decode; **~** partial
(opaque bytes + supplementary info); **—** not decoded.

### Core range (0x00-0x13)

| code | mnemonic | status | decoder | spec |
|---|---|---|---|---|
| 0x00 | EV_PREBOOT_CERT | ✓ named | `do_decode(_, _) -> #{}` | [PFP] §10.4.1 |
| 0x01 | EV_POST_CODE | ✓ | `decode_post_code` | [PFP] §10.4.1 |
| 0x03 | EV_NO_ACTION | ✓ | `decode_no_action` — parses SpecID alg list + StartupLocality | [PFP] §9.4.5 |
| 0x04 | EV_SEPARATOR | ✓ | `decode_separator` — firmware_error / normal / other classification | [PFP] §10.4.1 |
| 0x05 | EV_ACTION | ✓ | `decode_ascii_action` | [PFP] §10.4.1 |
| 0x06 | EV_EVENT_TAG | ✓ | `decode_event_tag` — TCG_PCClientTaggedEvent + 5 systemd-stub TagIDs | [PFP] §5.3.6 + `src/boot/measure.h` |
| 0x07 | EV_S_CRTM_CONTENTS | ~ | `decode_crtm_contents` — tries firmware-blob-v1, falls back to opaque | [PFP] §10.4.1 |
| 0x08 | EV_S_CRTM_VERSION | ✓ | `decode_crtm_version` — UTF-16LE → UTF-8 + firmware-family manifest match | [PFP] §10.4.1 |
| 0x09 | EV_CPU_MICROCODE | ✓ | `decode_cpu_microcode` — **Intel 48B header** [INTEL-SDM §9.11.1] **+ AMD 64B microcode_header_amd** [LINUX `arch/x86/kernel/cpu/microcode/amd.c`] |
| 0x0A | EV_PLATFORM_CONFIG_FLAGS | ~ | `decode_platform_config_flags` — length + SHA-256 | [PFP] §10.4.1 |
| 0x0B | EV_TABLE_OF_DEVICES | ✓ | `decode_table_of_devices` — array of UEFI_DEVICE_PATH, each walked | [PFP] §10.4.1 + [UEFI] §10 |
| 0x0C | EV_COMPACT_HASH | ~ | `decode_opaque_with_length` | [PFP] §10.4.1 |
| 0x0D | EV_IPL | ✓ | `decode_ev_ipl` — systemd-stub key=value + 17 known PE section names with PCR mapping | [PFP] §10.4.1 + [SYSTEMD] `stub.c` |
| 0x0E | EV_IPL_PARTITION_DATA | ~ | `decode_ipl_partition_data` — GRUB legacy path + content SHA-256 | [PFP] §10.4.1 |
| 0x0F | EV_NONHOST_CODE | ~ | `decode_nonhost` — SHA-256 + kind annotation | [PFP] §10.4.1 |
| 0x10 | EV_NONHOST_CONFIG | ~ | `decode_nonhost` | [PFP] §10.4.1 |
| 0x11 | EV_NONHOST_INFO | ~ | `decode_nonhost` | [PFP] §10.4.1 |
| 0x12 | EV_OMIT_BOOT_DEVICE_EVENTS | ✓ | `decode_ascii_action` | [PFP] §10.4.1 |
| 0x13 | EV_POST_CODE2 | ✓ | `decode_firmware_blob2` — same UEFI_PLATFORM_FIRMWARE_BLOB2 shape | [PFP 1.06] §10.4.2 |

### UEFI range (0x80000001+)

| code | mnemonic | status | decoder | spec |
|---|---|---|---|---|
| 0x80000001 | EV_EFI_VARIABLE_DRIVER_CONFIG | ✓ | `decode_uefi_variable` + SecureBoot/SetupMode/AuditMode/DeployedMode/PK/KEK/db/dbx semantics | [UEFI] §32.4 |
| 0x80000002 | EV_EFI_VARIABLE_BOOT | ✓ | `decode_uefi_variable_boot` — BootOrder u16 list + Boot#### EFI_LOAD_OPTION | [UEFI] §8.2 |
| 0x80000003 | EV_EFI_BOOT_SERVICES_APPLICATION | ✓ | `decode_uefi_image_load` + full `parse_device_path` walk | [PFP] §10.5.3 + [UEFI] §10 |
| 0x80000004 | EV_EFI_BOOT_SERVICES_DRIVER | ✓ | same | [PFP] §10.5.3 |
| 0x80000005 | EV_EFI_RUNTIME_SERVICES_DRIVER | ✓ | same | [PFP] §10.5.3 |
| 0x80000006 | EV_EFI_GPT_EVENT | ✓ | `decode_uefi_gpt` — 92-byte header + disk GUID + LBAs + partition-count | [UEFI] §5.3 |
| 0x80000007 | EV_EFI_ACTION | ✓ | `decode_ascii_action` | [PFP] §10.5.1 |
| 0x80000008 | EV_EFI_PLATFORM_FIRMWARE_BLOB | ✓ | `decode_firmware_blob` — address + length | [PFP] §10.5.7 |
| 0x80000009 | EV_EFI_HANDOFF_TABLES | ✓ | `decode_handoff_tables_v1` — per-entry {vendor-GUID, vendor-GUID-name, address}; 10 known GUIDs | [PFP] §10.5.9 |
| 0x8000000A | EV_EFI_PLATFORM_FIRMWARE_BLOB2 | ✓ | `decode_firmware_blob2` — description + address + length | [PFP 1.06] §10.5.8 |
| 0x8000000B | EV_EFI_HANDOFF_TABLES2 | ✓ | `decode_handoff_tables2` — description string | [PFP 1.06] §10.5.9 |
| 0x8000000C | EV_EFI_VARIABLE_BOOT2 | ✓ | `decode_uefi_variable_boot` (same layout, different digest) | [PFP 1.06] §10.5.5 |
| 0x8000000D | EV_EFI_GPT_EVENT2 | ✓ | `decode_uefi_gpt` (same layout, different digest) | [PFP 1.06] §10.5.3 |
| 0x80000010 | EV_EFI_HCRTM_EVENT | ✓ | `decode_ascii_action` — "HCRTM" marker | [PFP] §10.5.10 |
| 0x800000E0 | EV_EFI_VARIABLE_AUTHORITY | ✓ | `decode_uefi_variable` + MokListTrusted / SbatLevel / Shim* enrichments | [PFP] §10.5.4 |
| 0x800000E1 | EV_EFI_SPDM_FIRMWARE_BLOB | ✓ | `decode_spdm_event` — embedded EFI_DEVICE_PATH + SPDM payload length + SHA-256 | [PFP 1.06] §10.5.6 |
| 0x800000E2 | EV_EFI_SPDM_FIRMWARE_CONFIG | ✓ | same | [PFP 1.06] §10.5.6 |
| 0x800000E3 | EV_EFI_SPDM_DEVICE_POLICY | ✓ | same | [PFP 1.06] §10.5.6 |
| 0x800000E4 | EV_EFI_SPDM_DEVICE_AUTHORITY | ✓ | same | [PFP 1.06] §10.5.6 |
| 0x800000E5 | EV_EFI_SPDM_DEVICE_BLOB | ✓ | same | provisional (post-PFP-1.06 draft) |

### Windows SIPA range (0x10000000+)

| code range | category | status | decoder |
|---|---|---|---|
| 0x10000001 | SIPA_EVENTTYPE_TRUSTPOINT | ✓ | `decode_sipa_event` |
| 0x10000002 | SIPA_EVENTTYPE_ERROR | ✓ | |
| 0x10000003 | SIPA_EVENTTYPE_PREOSPARAMETER | ✓ | |
| 0x10000004 | SIPA_EVENTTYPE_OSPARAMETER | ✓ | |
| 0x10000005 | SIPA_EVENTTYPE_AUTHORITY | ✓ | |
| 0x10000006 | SIPA_EVENTTYPE_LOADEDMODULE | ✓ | |
| 0x10000007 | SIPA_EVENTTYPE_TRUSTBOUNDARY | ✓ | |
| 0x10000008 | SIPA_EVENTTYPE_ELAMAGGREGATION | ✓ | |
| 0x10000009 | SIPA_EVENTTYPE_LOADEDMODULEAGGREGATION | ✓ | |
| 0x1000000A | SIPA_EVENTTYPE_TRUSTPOINT_AGGREGATION | ✓ | |
| 0x1000000B-E | SIPA_EVENTTYPE_{ELAM_CERTIFICATE, VBS_MEASUREMENTS, KSR_SIGNATURE, KSR_AGGREGATION} | ✓ | |
| inner SIPA sub-event types (50+ names) | various | ✓ | see `sipa_subtype_name/1` |

## 2. Nested binary sub-formats

### UEFI_VARIABLE_DATA [UEFI §32.4]

`decode_uefi_variable/1` parses:
- Variable GUID (16 B, mixed-endian → canonical UUID string)
- UnicodeNameLength (u64 LE, count of UTF-16 code units)
- VariableDataLength (u64 LE)
- UnicodeName (UTF-16LE → UTF-8)
- VariableData

Per-variable semantic decoding in
`decode_uefi_variable_semantic/2`:

| variable | decoded fields |
|---|---|
| SecureBoot | secure-boot-enabled (bool) |
| SetupMode | setup-mode (bool) |
| AuditMode | audit-mode (bool) |
| DeployedMode | deployed-mode (bool) |
| PK / KEK / db / dbx / dbr / dbt | signature-list → summarise_signature_list |
| MokList / MokListX / MokListRT / MokListXRT / MokListTrusted / SbatLevelRT | signature-list |
| MokListTrusted (single-byte form) | moklist-trusted (bool) |
| SbatLevel | sbat-entries [{component, revision}], sbat-entry-count |
| Shim* (any shim-prefixed) | shim-variable-sha256, shim-variable-length |
| BootCurrent | boot-current (BootXXXX string) |
| BootNext | boot-next |
| Timeout | boot-menu-timeout-seconds |
| OsIndications | os-indications (u64) + os-indications-flags [BOOT_TO_FW_UI, TIMESTAMP_REVOCATION, FILE_CAPSULE_DELIVERY_SUPPORTED, FMP_CAPSULE_SUPPORTED, CAPSULE_RESULT_VAR_SUPPORTED, START_OS_RECOVERY, START_PLATFORM_RECOVERY, JSON_CONFIG_DATA_REFRESH] |
| OsIndicationsSupported | os-indications-supported + flags |

### EFI_SIGNATURE_LIST [UEFI §32.4.1]

`summarise_signature_list/1` walks multiple EFI_SIGNATURE_LISTs
and decodes per-entry based on SignatureType GUID:

| type GUID | name | decoder |
|---|---|---|
| a5c059a1-94e4-4aa7-87b5-ab155c2bf072 | EFI_CERT_X509_GUID | `decode_x509_cert/1` — **full ASN.1 decode**: x509-sha256-fingerprint, x509-serial, x509-issuer (DN flattened: `CN=..., O=..., C=...`), x509-subject, x509-not-before, x509-not-after, x509-public-key-alg (rsa/ecdsa/dsa/ed25519/ed448), x509-public-key-size-bits, x509-signature-alg |
| c1c41626-504c-4092-aca9-41f936934328 | EFI_CERT_SHA256_GUID | raw 32-byte digest → sha256 |
| 3bd2a492-96c0-4079-b420-fcf98ef103ed | EFI_CERT_SHA384_GUID | raw 48-byte digest → sha384 |
| 46dad11e-2b7a-4a3e-aaeb-f5fe0f0bc20e | EFI_CERT_SHA512_GUID | raw 64-byte digest → sha512 |
| 826ca512-cf10-4ac9-b187-be01496631bd | EFI_CERT_SHA1_GUID | raw 20-byte digest → sha1 |
| 3c5766e8-269c-4e34-aa14-ed776e85b3b6 | EFI_CERT_RSA2048_GUID | exponent (256B) + modulus (256B) + rsa-key-size-bits=2048 |
| e8665b96-b6bb-4bdf-ba9b-3a3bbecb6f99 | EFI_CERT_X509_SHA256_GUID | ToBeSigned length + hash-algorithm-guid + SHA-256 |
| a7717414-c616-4977-9420-844712a735bf | EFI_CERT_X509_SHA384_GUID | same shape |
| 64e0d72c-9e7a-4dc7-8ae5-a6c06c7b9fe0 | EFI_CERT_X509_SHA512_GUID | same shape |
| 4aafd29d-68df-49ee-8aa9-347d375665a7 | EFI_CERT_TYPE_PKCS7_GUID | pkcs7-data-length |
| other | unknown-cert-type | data-length + sha256 |

### UEFI_DEVICE_PATH [UEFI §10]

`parse_device_path/1` walks the linked list and decodes each node.
Returns {[StructuredNodes], CanonicalText} where CanonicalText is
the UEFI-spec form (`PciRoot(0x0)/Pci(0x1F,0x2)/Sata(0,0xFFFF,0)/HD(1,gpt,...)/\EFI\BOOT\BOOTX64.EFI`).

| type | subtype | name | fields |
|---|---|---|---|
| 0x01 | 0x01 | pci | function, device |
| 0x01 | 0x02 | pccard | function |
| 0x01 | 0x03 | memory-mapped | memory-type, start-address, end-address |
| 0x01 | 0x04 | hw-vendor | vendor-guid, data-length |
| 0x01 | 0x05 | controller | controller-number |
| 0x01 | 0x06 | bmc | interface-type, base-address |
| 0x02 | 0x01 | acpi | hid (+ 3-char EISA vendor decode), uid |
| 0x02 | 0x02 | acpi-expanded | hid, uid, cid + HID/UID/CID strings |
| 0x02 | 0x03 | acpi-adr | ADR u32 array |
| 0x03 | 0x01 | atapi | primary bool, slave bool, lun |
| 0x03 | 0x02 | scsi | target-id, lun |
| 0x03 | 0x03 | fibre-channel | wwn, lun |
| 0x03 | 0x04 | firewire-1394 | guid |
| 0x03 | 0x05 | usb | parent-port, interface |
| 0x03 | 0x06 | i2o | target-id |
| 0x03 | 0x09 | infiniband | resource-flags, port-gid, ioc-guid, target-port-id-guid, device-id |
| 0x03 | 0x0A | msg-vendor | vendor-guid, data-length |
| 0x03 | 0x0B | mac-addr | mac (canonical colon-separated), if-type |
| 0x03 | 0x0C | ipv4 | local-ip, remote-ip, ports, protocol, static bool, gateway-ip, subnet-mask |
| 0x03 | 0x0D | ipv6 | local-ip, remote-ip, ports, protocol, ip-addr-origin, prefix-length, gateway-ip |
| 0x03 | 0x0E | uart | baud-rate, data-bits, parity, stop-bits |
| 0x03 | 0x0F | usb-class | vendor-id, product-id, class/subclass/protocol |
| 0x03 | 0x10 | usb-wwid | interface-number, vendor-id, product-id, serial-number (UCS-2) |
| 0x03 | 0x11 | logical-unit | lun |
| 0x03 | 0x12 | sata | hba-port, pmp-port, lun |
| 0x03 | 0x13 | iscsi | protocol, login-options, lun, target-portal-group, target-name |
| 0x03 | 0x14 | vlan | vlan-id |
| 0x03 | 0x15 | fibre-channel-ex | wwn, lun |
| 0x03 | 0x16 | sas-ex | sas-address, lun, device-topology, relative-target-port |
| 0x03 | 0x17 | nvme-ns | namespace-id, ieee-eui-64 |
| 0x03 | 0x18 | uri | uri text |
| 0x03 | 0x19 | ufs | target-id, lun |
| 0x03 | 0x1A | sd | slot-number |
| 0x03 | 0x1B | bluetooth | bd-addr |
| 0x03 | 0x1C | wifi | ssid |
| 0x03 | 0x1D | emmc | slot-number |
| 0x03 | 0x1E | bluetooth-le | bd-addr, address-type |
| 0x03 | 0x1F | dns | is-ipv6 bool, dns-data-length |
| 0x03 | 0x20 | nvdimm-namespace | uuid |
| 0x03 | 0x21 | rest-service | service-type, access-mode, vendor-guid, data-length |
| 0x04 | 0x01 | hard-drive | partition-number, partition-start-lba, partition-size-lba, partition-signature (GPT disk GUID / MBR serial), partition-format (gpt/mbr/unknown), signature-type (none/mbr-serial/gpt-guid/unknown) |
| 0x04 | 0x02 | cdrom | boot-entry, partition-start-lba, partition-size-lba |
| 0x04 | 0x03 | media-vendor | vendor-guid, data-length |
| 0x04 | 0x04 | file-path | path (UCS-2 → UTF-8) |
| 0x04 | 0x05 | media-protocol | protocol-guid |
| 0x04 | 0x06 | piwg-fw-file | fv-file-name (GUID) |
| 0x04 | 0x07 | piwg-fw-volume | fv-name (GUID) |
| 0x04 | 0x08 | relative-offset-range | start-offset, end-offset |
| 0x04 | 0x09 | ram-disk | start-address, end-address, disk-type-guid, instance |
| 0x05 | 0x01 | bios-boot-spec | device-type, status-flag, description |
| 0x7F | 0x01 | end-instance | — |
| 0x7F | 0xFF | end-entire | — |

### Other structures

- **UEFI_LOAD_OPTION** (Boot####) [UEFI §3.1.3]:
  `parse_efi_load_option/1` returns load-option-attributes (u32),
  load-option-active + load-option-hidden (bool flags),
  load-option-description (UCS-2 → UTF-8),
  load-option-file-path-length.

- **UEFI_GPT_DATA** / EFI_PARTITION_TABLE_HEADER [UEFI §5.3]:
  `decode_uefi_gpt/1` returns disk-guid, my-lba, alternate-lba,
  first-usable-lba, last-usable-lba, partition-entry-lba,
  number-of-partition-entries, size-of-partition-entry,
  measured-partition-count.

- **UEFI_IMAGE_LOAD_EVENT** [PFP §10.5.3]: image-location-in-
  memory, image-length-in-memory, image-link-time-address,
  device-path-length, device-path (raw bytes),
  device-path-nodes (parsed), device-path-text (canonical).

- **UEFI_PLATFORM_FIRMWARE_BLOB** v1 + v2 [PFP §10.5.7]:
  blob-physical-address, blob-length, blob-description
  (v2 only).

- **UEFI_HANDOFF_TABLE_POINTERS v1 + v2** [PFP §10.5.9]:
  `decode_handoff_tables_v1/1` + `decode_handoff_tables2/1`
  return per-entry {vendor-guid, vendor-guid-name (10 known:
  ACPI 1.0 RSDP, ACPI 2.0 RSDP, SMBIOS 2.x, SMBIOS 3.x, SAL,
  MPS, HOB List, Memory Type Info, Debug Image Info, Memory
  Status Code Record), vendor-table-address}. v2 also returns
  table-description.

- **TCG_PCClientTaggedEvent** [PFP §5.3.6]:
  `decode_event_tag/1` returns tag-id (u32), tag-id-hex,
  tag-id-name (recognised: 5 systemd-stub sd-stub TagIDs for
  UKI measurements: LOADER_CONF / DEVICETREE_ADDON /
  INITRD_ADDON / UCODE_ADDON / UKI_PROFILE), tag-data-length,
  tag-description (for sd-stub, UTF-16LE → UTF-8).

- **Intel CPU microcode header** [INTEL-SDM Vol 3A §9.11.1]:
  `decode_microcode_intel/1` returns header-version,
  update-revision, date-bcd, date (canonical "YYYY-MM-DD"),
  processor-signature, cpu-family-model-stepping (decoded per
  Intel ExtFamily/ExtModel rules), checksum, loader-revision,
  processor-flags.

- **AMD CPU microcode header** (microcode_header_amd) [LINUX
  `arch/x86/kernel/cpu/microcode/amd.c`]:
  `decode_microcode_amd/1` returns data-code, date,
  patch-id, mc-patch-data-id, mc-patch-data-length,
  init-flag, mc-patch-data-checksum, nb-dev-id, sb-dev-id,
  processor-rev-id, processor-rev-id-hex, nb-rev-id, sb-rev-id,
  bios-api-rev.

- **SMBIOS entry point** [SMBIOS §5.1]:
  `parse_smbios/1` handles both v2.x `_SM_` (31B) and v3.x
  `_SM3_` (24B) anchors, returns version, entry-point-length,
  entry-point-revision, entry-point-checksum, table-length,
  table-address, number-of-structures.

- **SMBIOS structures** [SMBIOS §7]:
  `parse_smbios_structure/1` decodes common header +
  per-type fields for:
    Type 0 (BIOS Information)
    Type 1 (System Information) — UUID + manufacturer + product
    Type 2 (Baseboard)
    Type 3 (System Enclosure / Chassis) with 36 named types

- **ACPI common header** [ACPI §5.2.6]:
  `parse_acpi_table/1` returns signature, signature-name
  (39 recognised), length, revision, checksum, oem-id,
  oem-table-id, oem-revision, creator-id, creator-revision.

- **ACPI RSDP** [ACPI §5.2.5]:
  `parse_acpi_rsdp/1` handles v1 (20B) and v2 (36B). Returns
  signature, checksum, oem-id, revision, rsdt-address,
  (v2) length, xsdt-address, extended-checksum.

## 3. Per-PCR derived-field templates

Every PCR carries a `derived/` submessage containing named fields
extracted from the events extended into that PCR. Template fields
are always present (with `"unknown"` sentinel when evidence is
absent).

### PCR 0 — `firmware-srtm`

| field | type | sourced from |
|---|---|---|
| crtm-version | string | EV_S_CRTM_VERSION UTF-16LE decode |
| hcrtm | bool | EV_EFI_HCRTM_EVENT |
| post-codes | [string] | EV_POST_CODE |
| firmware-blobs | [{address, length, description}] | EV_EFI_PLATFORM_FIRMWARE_BLOB / 2 |
| separator-seen | bool | EV_SEPARATOR |
| separator-kind | string | EV_SEPARATOR data classification |
| spec-id | string | EV_NO_ACTION SpecID "Event03" |

### PCR 1 — `platform-firmware-config`

| field | type | sourced from |
|---|---|---|
| cpu-microcode | string | EV_CPU_MICROCODE (Intel + AMD) |
| cpu-vendor | string | same (intel / amd / unknown) |
| uefi-boot-order | [BootXXXX] | EV_EFI_VARIABLE_BOOT / _BOOT2 |
| boot-entries | [{name, description, active}] | EV_EFI_VARIABLE_BOOT |
| boot-current | string | BootCurrent variable |
| handoff-tables | [{vendor-guid, name, address}] | EV_EFI_HANDOFF_TABLES v1 |
| separator-seen | bool | EV_SEPARATOR |

### PCR 2 / PCR 3 — `option-rom-code` / `option-rom-config`

| field | type | sourced from |
|---|---|---|
| option-rom-scanned | bool | EV_ACTION "Start Option ROM Scan" |
| separator-seen | bool | EV_SEPARATOR |

### PCR 4 — `boot-loader-code`

| field | type | sourced from |
|---|---|---|
| boot-services-applications | [{image-location-in-memory, image-length-in-memory}] | EV_EFI_BOOT_SERVICES_APPLICATION |
| boot-action-markers | [string] | EV_ACTION on PCR 4 |
| separator-seen | bool | EV_SEPARATOR |

### PCR 5 — `boot-loader-config`

| field | type | sourced from |
|---|---|---|
| gpt-partition-tables | int | EV_EFI_GPT_EVENT |
| separator-seen | bool | EV_SEPARATOR |

### PCR 7 — `secure-boot-policy`

| field | type | sourced from |
|---|---|---|
| secure-boot-enabled | bool / unknown | SecureBoot UEFI variable |
| setup-mode / audit-mode / deployed-mode | bool / unknown | per-variable |
| pk-entry-count | int / unknown | PK signature list |
| pk-x509-fingerprints | [sha256-b64url] | per PK cert entry (ASN.1 decode) |
| kek-entry-count + kek-x509-fingerprints + kek-issuers | | KEK |
| db-entry-count + db-x509-fingerprints + db-issuers | | db |
| dbx-entry-count | | dbx |
| authorities | [string] | EV_EFI_VARIABLE_AUTHORITY variable names |
| moklist-trusted | bool / unknown | shim's MokListTrusted |
| sbat-self-revision | string / unknown | SBAT's first-line date-stamp |
| sbat-entry-count | int / unknown | SBAT entry count |
| separator-seen | bool | EV_SEPARATOR |

### PCR 8 / 9 — GRUB legacy

| field | type | sourced from |
|---|---|---|
| grub-cmdline | string | EV_IPL (GRUB legacy format) |
| grub-modules | [string] | EV_IPL |

### PCR 10 — `ima-runtime-measurements`

| field | type |
|---|---|
| ima-active | bool |
| ima-event-count | unknown (transport deferred) |
| ima-files-measured | unknown |
| note | documentation string |

### PCR 11 / 12 / 13 — UKI

| PCR | field | type | sourced from |
|---|---|---|---|
| 11 | uki-measured | bool | EV_IPL with kernel-image/kernel-name |
| 11 | uki-image-hash | hash | |
| 11 | uki-kernel-version | string | EV_IPL kernel-name value |
| 12 | uki-cmdline | string | EV_IPL kernel-cmdline |
| 12 | uki-initrd-hash | hash | |
| 13 | uki-sysext-count | int | |

### PCR 14 — `secure-boot-authority-mok`

| field | type |
|---|---|
| mok-entry-count | int / unknown |

### PCR 15 — `lapee-node-identity`

| field | type |
|---|---|
| lapee-node-identity-committed | bool (true when the enforced on.start hook ran) |

## 4. Data files

### `manufacturers.json`

30 TPM vendors from the TCG Vendor ID Registry v1.06. Each entry:
`{id, name, kind, platforms, product_families?,
ek_root_ca_source?, ek_subject_pattern?,
ek_ca_thumbprints?, known_cves?, notes}`. Kinds:

- `discrete` (13): Infineon, STMicro, Nuvoton, Nationz, Atmel/
  Microchip, Broadcom, Samsung, SMSC, Flyslice, Sinosun, NSING,
  Texas Instruments, Winbond, SecEdge.
- `fTPM-cpu` (7): AMD, Intel, Google (Cr50/Ti50 + GCE), HiSilicon,
  Qualcomm, Rockchip, Samsung.
- `server-platform` (5): Lenovo, HP Inc., IBM, Cisco, Solidigm.
- `virtual` (1): Microsoft.
- `other` (2): Ant Group, InteXX.

### `firmware-versions/` (17 manifests)

Each manifest: `{schema-version, name, description, match:
{crtm-version-prefix [+ regex]}, vendor, platforms,
tpm-chip-vendors?, secure-boot-default, ek-root-ca-source?,
trust-tier?, notes, source}`.

Shipped: lenovo-thinkpad (17 model prefixes tabulated),
dell-latitude-xps, hp-elitebook, hpe-ilo-proliant,
insyde-ami-common (Insyde/AMI/Phoenix/coreboot generic),
microsoft-surface, framework-laptop, google-cloud-shielded-vm,
aws-nitro-tpm, azure-trusted-launch, chromebook-coreboot,
supermicro-server, system76-purism-coreboot, intel-nuc-asrock,
asus-rog-zen, msi-gigabyte, qemu-seabios (dev-only flagged).

### `pcr-profiles/`

Per-platform expected PCR 0/7 digest catalogue. Ships 1 populated
(QEMU SeaBIOS). Populated per real-hardware capture — schema
ready, data gathered as deploys occur.

### `root-cas/`

Per-vendor EK root CA PEM files. Deployer-supplied; licensing
varies per vendor. Sources documented per vendor in
`manufacturers.json`.

### `uki-measurements/`

Per-UKI-image PE-section hash catalogue. Schema ready; data
gathered per deploy.

### `fixtures/`

31 real-world TCG event log vectors from 15 upstream repos,
covering Lenovo/Dell/Intel NUC/Supermicro/Inspur/GCE (Shielded
VM + SEV + SEV-SNP + TDX)/Intel TDX CCEL/QEMU/Fedora
systemd-boot/Arch/Canonical/fwupd/tpm2-tools + deliberate edge
cases (empty, bogus, truncated, option-rom, AWS EBS separator
quirk). Every fixture is parse-tested by the eunit harness in
`real_fixture_corpus_parses_without_crashes_test_/0`.

## 5. Known gaps (explicit, deliberate)

- **IMA per-file runtime event log (PCR 10)** — only the final PCR
  10 digest is in the attestation envelope. Per-file chain
  transport requires an envelope schema bump in `~tpm2@2.0a`.
- **Per-platform PCR 0/7 profiles** — `pcr-profiles/` has 1
  populated entry (QEMU). Additional real-hardware profiles
  require captures from the deployer's specific hardware mix.
- **Vendor EK root CAs** — `root-cas/` is empty; deployers supply
  the `.pem` files matching the TPMs they trust.

None of these gaps are code problems — each is either a data
problem (profiles, CAs) or a schema-bump problem (IMA transport).

## 6. Module map

- `src/dev_tpm_tcg.erl` — event-log parser + all decoders.
- `src/dev_tpm_interpret.erl` — interpret device: derived-field
  extraction, claim projection, /info/checks/events/claim/verify
  endpoints.
- `src/dev_tpm2.erl` — attester + verifier (quote + EK chain +
  PCR replay).
- `src/hb_db_tpm.erl` — static DB loader (manufacturers +
  firmware-versions + pcr-profiles + root-cas).
- `priv/tpm-interpret/` — data files.
- `priv/tpm-interpret/fixtures/` — real-world test vectors.

## 7. Test coverage

As of 2026-04: **102 eunit tests pass**:

    dev_tpm_tcg       71 tests
    dev_tpm_interpret 15 tests  (+ fixture validation via tcg)
    dev_tpm2          17 tests

The dev_tpm_tcg suite includes the real-fixture validation
harness — every new vector dropped under `priv/tpm-interpret/
fixtures/` is picked up automatically at the next `rebar3 eunit`
run.
