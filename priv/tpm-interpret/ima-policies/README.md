priv/tpm-interpret/ima-policies/

Per-distribution / per-product IMA appraisal policies. Each file
describes the expected IMA measurements a correctly-booted machine
should produce, so a verifier can cross-reference the parsed IMA
log (`claim.ima`) against a known-good baseline and flag:

  * unexpected files (not in the policy)
  * expected files that never appeared in the IMA log
  * files that should be signed but aren't
  * hash-algorithm downgrades

Schema (v1):

    {
      "schema-version": 1,
      "name":            <display name>,
      "distribution":    <distro / product name>,
      "applies-to": {
        "kernel-name-prefix": [<string>, ...],
        "uki-profile-key":    [<filename-stem>, ...]
      },
      "minimum-hash-alg":  "sha1" | "sha256" | "sha384" | "sha512",
      "expected-files": [
        {
          "pathname":           <exact pathname>    OR
          "pathname-prefix":    <path prefix>       OR
          "pathname-suffix":    <path suffix>,
          "signature-required": true | false,
          "hash-alg":           "sha256",
          "category":           "system-binary" | "config" |
                                 "module" | "script" | "other",
          "notes":              <free-form>
        },
        ...
      ],
      "notes":  <free-form policy narrative>,
      "source": <URL or attribution>
    }

The DB loader (`hb_db_tpm:load/1`) reads every `*.json` under this
directory. `claim.ima-policy` picks the policy whose `applies-to`
most specifically matches the current envelope (kernel-name or
matched-UKI-profile), then classifies each parsed IMA entry:

    * "matched"    — pathname matches a policy-expected entry
    * "unexpected" — pathname does not match any policy entry
    * "signature-missing" — entry is matched but the policy
                             required a signature that wasn't
                             present
    * "hash-alg-downgrade" — entry uses a weaker hash than the
                             policy's minimum-hash-alg

Summary counts and a `violations' list are surfaced on the claim.

To add a policy: drop a new JSON file here and rebuild the release.
