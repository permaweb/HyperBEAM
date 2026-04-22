priv/tpm-interpret/boot-images/

Catalogue of known UEFI boot-image publishers. Each file is a JSON
message describing one image family (shim / grub / systemd-boot /
Windows Boot Manager / …). Entries match attestation boot-chain
rows either by exact SHA-256 image hash (most specific) or by
device-path suffix (robust across binary revisions of the same
product).

Schema (v1):

    {
      "schema-version": 1,
      "name":            <display name>,
      "publisher":       <vendor / distribution>,
      "product":         <product name>,
      "category":        "boot-manager" | "shim" | "bootloader" |
                         "uki" | "fallback" | "netboot",
      "match": {
        "image-hash-sha256": [<base64url-hash>, ...],  // exact
        "device-path-suffix": [<suffix-string>, ...]    // prefix
                                                        // of path-text
      },
      "recommended-min-version": <string>,     // optional
      "cve-status": "clear" | "has-known-cves" | "revoked",
      "cve-notes":  <string>,
      "signed-by": [<CA / signing key name>, ...],
      "notes":     <free-form policy notes>,
      "source":    <URL or attribution>
    }

The DB loader (`hb_db_tpm:load/1`) reads every `*.json` under this
directory at node start; `claim.boot-chain` cross-references each
row's image-hash + device-path-text against the catalogue and
attaches `publisher`, `product`, `category`, `cve-status` fields
when a match fires.

To add an entry: drop a new JSON file here and rebuild the release.
Hashes are easiest to obtain from `sha256sum` on the signed binary
or from upstream shim-review / security-advisory publications.
