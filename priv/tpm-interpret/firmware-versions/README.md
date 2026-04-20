# Firmware-versions DB

This directory holds a per-vendor catalogue of **firmware CRTM (Core
Root of Trust for Measurement) identifier strings**. Each file is a
JSON document with a `match` predicate (prefix or regex) plus vendor/
platform metadata. Used by `dev_tpm_interpret:interpret_tpm_identity/2`
to resolve `pcrs/0/derived/crtm-version` → a human-readable
{vendor, model, version, trust-tier} record.

## File layout

```
lenovo-thinkpad.json          N-series ThinkPad CRTM identifiers
dell-latitude-xps.json        Dell `Dell Inc.' ASCII identifiers
hp-elitebook.json             HP EliteBook / ProBook `HPQ' identifiers
insyde-ami-common.json        InsydeH2O + AMI Aptio third-party UEFI
qemu-seabios.json             QEMU SeaBIOS + EDK II OVMF (dev only!)
```

## Schema (per file)

```json
{
  "schema-version": 1,
  "name": "<human-readable family name>",
  "description": "<what this family covers>",
  "match": {
    "crtm-version-prefix": ["<prefix>", ...],
    "crtm-version-regex": "<regex>"
  },
  "vendor": "<company>",
  "platforms": { "N1M": "ThinkPad X1 Carbon Gen 5", ... },
  "notes": "<CA provisioning, quirks, CVEs>"
}
```

Or, when multiple independent families share a file:

```json
{
  "schema-version": 1,
  "entries": [
    { "match": {...}, "vendor": "...", ... },
    { "match": {...}, "vendor": "...", ... }
  ]
}
```

## Adding a new entry

1. Observe the target platform's CRTM string:
   ```
   # On Linux with a hardware TPM:
   curl http://localhost:18734/~tpm2@2.0a/attestation \
       -H 'accept: application/json@1.0' \
       -H 'accept-bundle: true' \
     | jq -r '.body.interpretation.pcrs."0".derived."crtm-version"'
   ```
2. Identify a stable **prefix** or **regex** that uniquely matches
   this vendor's CRTM identifiers.
3. Add a JSON file in this directory. The interpret device picks it
   up on the next node start (rebuild the release) and surfaces
   matched results as `interpretation.firmware.match`.

## Trust tiers

Files may set `"trust-tier": "development-only"` to flag a firmware
family that a production verifier should REFUSE by default. See
`qemu-seabios.json` for the canonical example.
