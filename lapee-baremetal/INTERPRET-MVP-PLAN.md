# MVP for `~tpm-interpret@1.0` — rich TCG event log parsing

**Goal (from paper §Architecture):** Every machine-identifying field
(CPU, TPM, TME/SME, Secure Boot, IOMMU, kernel, bootloader, firmware)
surfaces as a named, verified message key — not a raw PCR hash.
Green-zone-style predicates compose on top.

**Design corrections (this pass):**

- Everything is AO-Core native: parsed events are a map of
  messages keyed by sequence number, each with named keys
  (`pcr`, `event_type`, `digests`, `event_data`, ...).
  Individual events are path-addressable:
  `/~tpm-interpret@1.0/events/3/event_type`.
- Transport: raw binaries on the device side; client-side
  `accept-bundle: true` drives encoding.
- TME is proven by the kernel's own halt-at-init check (paper §Arch
  line 229), not by HB reading MSRs post-boot. Interpret device
  infers `claim.tme_enabled: true` from a match against a
  known-TME-checking UKI hash.

## Milestones (committed in order)

### M1 — envelope transport
- `dev_tpm2:attestation/3` reads `/sys/kernel/security/tpm0/binary_
  bios_measurements' (raw bytes) and adds `tcg_event_log' to the
  envelope.
- No explicit base64url on the binary; HB content-negotiation
  handles encoding.
- eunit fixture with a hand-built mini event log.

### M2 — parser (TCG_PCR_EVENT + TCG_PCR_EVENT2)
- New module `dev_tpm_tcg` (lives inside the hb app) — pure
  Erlang, no dependencies, parses both the legacy TCG_PCR_EVENT
  header and the crypto-agile TCG_PCR_EVENT2 records.
- Returns a map keyed by 1-based sequence number, each value an
  AO-Core message:

  ```
  #{<<"1">> => #{ <<"pcr">> => 0,
                  <<"event-type">> => <<"EV_NO_ACTION">>,
                  <<"event-type-code">> => 3,
                  <<"digests">> => #{ <<"sha1">> => <<…>>,
                                      <<"sha256">> => <<…>> },
                  <<"event-data">> => <<…>> },
    <<"2">> => …}
  ```

- Known event types (decoded → name) covered:
    TCG core: EV_PREBOOT_CERT, EV_POST_CODE, EV_NO_ACTION,
              EV_SEPARATOR, EV_ACTION, EV_EVENT_TAG,
              EV_S_CRTM_CONTENTS, EV_S_CRTM_VERSION,
              EV_CPU_MICROCODE, EV_PLATFORM_CONFIG_FLAGS,
              EV_TABLE_OF_DEVICES, EV_COMPACT_HASH,
              EV_IPL, EV_IPL_PARTITION_DATA, EV_NONHOST_*,
              EV_OMIT_BOOT_DEVICE_EVENTS
    UEFI:     EV_EFI_VARIABLE_DRIVER_CONFIG,
              EV_EFI_VARIABLE_BOOT,
              EV_EFI_BOOT_SERVICES_APPLICATION,
              EV_EFI_BOOT_SERVICES_DRIVER,
              EV_EFI_RUNTIME_SERVICES_DRIVER,
              EV_EFI_GPT_EVENT, EV_EFI_ACTION,
              EV_EFI_PLATFORM_FIRMWARE_BLOB(2),
              EV_EFI_HANDOFF_TABLES(2),
              EV_EFI_HCRTM_EVENT,
              EV_EFI_VARIABLE_AUTHORITY,
              EV_EFI_SPDM_* (FIRMWARE_BLOB/CONFIG/DEVICE_*)

### M3 — replay check in verify/3
- `chk_tcg_event_log_replay` — replay every parsed event into its
  declared PCR (starting from all-zeros), confirm each ≤14
  reconstructed PCR matches the quoted value.
- Becomes a 7th crypto check in the verifier battery.
- Zero-events in a PCR: not a hard reject unless that PCR's quoted
  value is non-zero.

### M4 — event-data decoders (batch 1, Secure Boot + firmware)
For each event type, add a decoder that enriches the event
message with a `parsed` sub-map holding structured fields:

- `EV_EFI_VARIABLE_DRIVER_CONFIG`:
  - `variable_guid`, `variable_name` (UTF-16 → UTF-8), `variable_data`
  - For the `SecureBoot` variable specifically:
    `parsed.secure_boot_enabled: bool` (from the single 0x01/0x00
    byte).
  - For `PK`/`KEK`/`db`/`dbx`: `parsed.signature_list: [<hash>…]`
    (SHA-256 of each contained cert; no full cert body).
- `EV_S_CRTM_VERSION`: `parsed.crtm_version: string` (UTF-16 →
  UTF-8 on best-effort).
- `EV_POST_CODE`: `parsed.post_code: string | bytes_b64url` (if
  printable → UTF-8 string).

### M5 — event-data decoders (batch 2, bootloader + UKI)

- `EV_EFI_BOOT_SERVICES_APPLICATION`:
  - parse `UEFI_IMAGE_LOAD_EVENT' → `parsed.image_length',
    `parsed.image_path' (if device path decodable)
- `EV_IPL' (systemd-stub style):
  - event data is typically `key=value' ASCII; split into
    `parsed.entries: #{<<"key">> => <<"value">>, …}'.
  - systemd-stub specifically: `kernel_cmdline`, `initrd`,
    `kernel_name`, `kernel_version`, etc.
- `EV_EFI_PLATFORM_FIRMWARE_BLOB(2)`:
  - `parsed.blob_physical_address', `parsed.blob_length'

### M6 — event-data decoders (batch 3, remainder)

- `EV_CPU_MICROCODE`: parse as signed update header (Intel/AMD
  layouts differ; try both).
- `EV_PLATFORM_CONFIG_FLAGS`: firmware-specific; surface raw +
  known-flag interpretation.
- `EV_TABLE_OF_DEVICES`: list of device paths.
- `EV_SEPARATOR`: just mark the boundary between pre-boot and
  runtime events (common "0xFF x4" marker).
- `EV_NO_ACTION`: contains the SpecID header for the event log
  itself; parse and expose `parsed.spec_id: #{major, minor, …}`.

### M7 — `claim.*` flat surface
- New top-level interpretation section: `claim` — flat
  policy-friendly fields with provenance pointers back to the
  source event(s) in `events`.

  ```
  claim.secure_boot.enabled          : bool
  claim.secure_boot.db_authorities   : [<hash>…]
  claim.firmware.crtm_version        : string | null
  claim.boot_loader.image_hash       : <binary>
  claim.kernel.uki_hash              : <binary>
  claim.kernel.cmdline               : string | null
  claim.kernel.iommu_strict          : bool | "unknown"
  claim.tme.enabled                  : bool | "unknown"
  claim.lockdown.level               : "confidentiality" | …
  <field>_provenance                 : [{pcr, event_seq}…]
  ```

- For each claim, if the derivation requires a known-good match
  that we don't yet have, value is `"unknown"`. Policy engines
  can distinguish `true` / `false` / `"unknown"` explicitly.

### M8 — corpus overhaul
- Delete the three `example-*.json` PCR profile stubs (they'd
  never match, just add noise).
- Keep the populated `qemu-seabios-tcg.json`.
- Add new DB files:
    `priv/tpm-interpret/event-types.json` —
        TCG event-type-code → {name, description}, built from
        the TCG PC Client spec. Covers the ~30 codes above.
    `priv/tpm-interpret/uki-measurements/` —
        per-known UKI hash → {kernel_version, initramfs_version,
                              cmdline_pattern, checks_tme}.
        Empty at MVP; ready for population.
    `priv/tpm-interpret/firmware-versions/` —
        per-CRTM-version-string → {vendor, model, version,
                                   cve_notes?}.
        Empty at MVP.

### M9 — new endpoint: `/~tpm-interpret@1.0/events`
- Returns the parsed event-log map. Individual events
  addressable via path traversal (HB does the subscripting):

  ```
  GET /~tpm-interpret@1.0/events                 → all events
  GET /~tpm-interpret@1.0/events/3               → event 3
  GET /~tpm-interpret@1.0/events/3/event_type    → its type string
  GET /~tpm-interpret@1.0/events/3/digests/sha256 → its SHA-256
  ```

- `info/3` documents this.

### M10 — tests + evidence
- eunit for every parser + every decoder (fixture-driven).
- Live test on the current QEMU guest (SeaBIOS emits a real event
  log even though it's minimal) — confirm parser works.
- Save fresh evidence.
- STATUS.md update.

## What's NOT in this pass
- Real-hardware UKI-measurement corpus (data problem).
- Vendor-specific post-code decoders for non-TCG extensions.
- IMA per-file event parsing (PCR 10 runtime — different log
  format; deferred, not blocking MVP).
- Kernel-side TME check implementation (lives in mkosi/Buildroot
  config, not in HB code).
