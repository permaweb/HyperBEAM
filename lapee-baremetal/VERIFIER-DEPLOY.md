# Deploying a LapEE verifier

A LapEE verifier is a HyperBEAM node that **checks** other LapEE
nodes' attestations. It doesn't need a TPM — just the HB runtime,
a vendor trust anchor, and network reachability to the peers it
wants to verify.

This is *verifier-only*. For deploying a LapEE **attester** (a
node that produces attestations), see the top-level README +
`scripts/build-hb-release.sh` + `scripts/build-initramfs-hb.sh`
+ `scripts/boot-hb.sh`.

## Minimum viable verifier — one page

A LapEE verifier needs exactly this:

1. **HyperBEAM release with `~tpm-interpret@1.0`** (ships in the
   LapEE rebar3 profile — see `config/lapee-enforced.flat` and
   this repo's `hb-release` target).
2. **Erlang/OTP 27** with `public_key` + `crypto` apps (stdlib).
3. **A trust-anchor file** — the PEM of the TPM vendor root CA
   that signed the peers' EK certs. Path is `/etc/lapee/tpm-ca.crt`
   by default (override via `lapee_tpm_ca_cert` in HB config).
4. **Network access to the peers** it will verify.
5. **No TPM required**. `lapee_tpm_nif` gracefully degrades when
   `LAPEE_TPM_ALLOW_NO_NIF=1` is set — the module loads but TPM
   calls raise `nif_not_loaded` loudly if anyone tries to
   ATTEST on the verifier (which is never correct for a
   verifier-only box).

That's it. No libtss2, no swtpm, no `/dev/tpmrm0`, no root.

## Choosing a trust anchor

Depends on the peers you want to verify. The `manufacturers.json`
DB inside the release (`priv/tpm-interpret/manufacturers.json`)
lists the 27 TCG-assigned vendor IDs. In decreasing order of
practical relevance today:

| peers | required root CA | where to get it |
|---|---|---|
| **Infineon discrete TPM** (`49465800`) — Lenovo ThinkPad, Dell, many server boards | Infineon Optiga TPM EK root | Infineon's PKI site (published) |
| **AMD fTPM via PSP** (`414d4400`) — EPYC, Ryzen | AMD fTPM EK root | `https://ftpm.amd.com/pki/aia/*.crt` |
| **Intel PTT** (`494e5443`) — Core, Xeon | Intel PTT EK root | Intel's TPM PKI |
| **STMicro** (`53544d20`) — Dell XPS, Lenovo | ST33 EK root | ST's TPM PKI |
| **Nuvoton** (`4e544300`) — consumer laptops | Nuvoton TPM EK root | Nuvoton's TPM PKI |
| **QEMU swtpm** (test) | The per-boot CA captured by `scripts/boot-hb.sh` on the serial stream, at `out/test-tpm-ca.crt` | the attester node itself |

For a multi-vendor verifier, concatenate the PEMs into a single
bundle and point `lapee_tpm_ca_cert` at it. No code change.

## Three ways to supply the trust anchor

Priority order (first wins):

1. **Per-request, inline**: `?trusted-ca=<base64url PEM bytes>`
   on `/~tpm-interpret@1.0/verify-peer` or
   `/~tpm2@2.0a/attestation/verify~tpm-interpret@1.0`. Useful for
   ad-hoc verifications where the caller supplies their own
   anchor. Base64url avoids the URL-encoding ambiguity that
   bites raw PEM over HTTP GET.
2. **Per-request, raw PEM**: `?trusted-ca-pem=<raw PEM>`. Back-
   compat; only works cleanly over POST or when the caller
   carefully preserves the PEM's newlines + `+` characters.
3. **Node config**: `lapee_tpm_ca_cert: /path/to/ca.pem` in the
   HyperBEAM config. Default location `/etc/lapee/tpm-ca.crt`.

The response's `trust_anchor_source` field tells you which source
was actually used: `"request"`, `"node_config"`, or `"none"` (no
anchor available — chain check then fails with a clean "missing"
error).

## Running the verifier

Native (Linux, recommended):

```bash
# Point HB_CONFIG at a flat file setting your trust anchor
cat > /etc/lapee/verifier.flat <<EOF
lapee_tpm_ca_cert: /etc/lapee/tpm-ca.crt
EOF
export HB_CONFIG=/etc/lapee/verifier.flat
export LAPEE_TPM_ALLOW_NO_NIF=1   # no TPM on this box
/usr/lib/hyperbeam/bin/hb foreground
```

Native (macOS, for development — note: memory pressure can block
LMDB):

```bash
cd hyperbeam/
LAPEE_TPM_ALLOW_NO_NIF=1 HB_CONFIG=/tmp/verifier.flat \
    HB_PORT=18735 rebar3 shell
```

Docker (Linux amd64 isolation):

```bash
docker run -d --name verifier \
  -p 127.0.0.1:18735:8734 \
  -e LAPEE_TPM_ALLOW_NO_NIF=1 \
  -v /etc/lapee/verifier.flat:/cfg.flat:ro \
  -v /etc/lapee/tpm-ca.crt:/etc/lapee/tpm-ca.crt:ro \
  -e HB_CONFIG=/cfg.flat \
  lapee-hyperbeam-builder:latest \
  /opt/hb/bin/hb foreground
```

## Calling the verifier

Cheapest: is this peer reachable + LapEE-shaped?

```bash
curl -sS --get \
    --data-urlencode 'peer=http://peer.example:8734' \
    http://verifier:18735/~tpm-interpret@1.0/peer-status
```

Summary (no crypto, ~10× cheaper than full verify):

```bash
curl -sS --get \
    --data-urlencode 'peer=http://peer.example:8734' \
    http://verifier:18735/~tpm-interpret@1.0/peer-summary
```

Full trust decision (fresh nonce + 5 crypto checks + link-free
summary):

```bash
curl -sS --get \
    --data-urlencode 'peer=http://peer.example:8734' \
    http://verifier:18735/~tpm-interpret@1.0/verify-peer
```

Response fields (on success):

```jsonc
{
  "peer": "http://peer.example:8734",
  "verified": true,
  "verdict": "accepted",
  "checks": [ /* 5 entries, all ok:true */ ],
  "summary": {
    "envelope-version": "0.3",
    "tpm-manufacturer": "…",
    "ak-algorithm": "RSA",
    "ak-key-size-bits": 2048,
    "ak-public-key-b64url": "…",           // pin this to refuse AK swaps
    "quote-attest-type": "TPM_ST_ATTEST_QUOTE",
    "quote-clock-ms": 601558,
    "quote-reset-count": …,
    "secure-boot-measured": false,
    "wallet-address": "…",                 // the peer's wallet
    "node-message-id": "…",                // 43-char base64url
    "on-start-hook-device": "tpm2@2.0a",   // MUST be this — enforced hook
    "pcr15-event-count": 1
  },
  "trust-anchor-source": "node_config",
  "nonce-challenge": "…",                  // the verifier's 32-byte challenge
  "nonce-freshness": "verified"            // "verified" or "mismatch"
}
```

On failure, `verified: false` and `verdict: "rejected"`, with the
failing check(s) in `checks` carrying a specific `detail`.
Notable detail patterns:

- `chain invalid: EK's issuer DN matches the trusted CA's
  subject DN, but the signature does not verify …` — **stale
  trust anchor**. Refresh your `lapee_tpm_ca_cert` from the
  peer's current boot or vendor.
- `chain invalid: EK's issuer DN does not match any trusted CA's
  subject DN` — genuine mismatch. Peer is signed by a vendor
  you don't trust.
- `nonce_freshness: "mismatch"` — the peer didn't sign the
  verifier's specific challenge. Possible replay of a captured
  envelope, or the peer silently ignored `?nonce=` (older than
  v0.3).

## Self-description endpoints

Every LapEE verifier is self-describing. Clients should use this
rather than hard-coding behaviour:

```bash
curl http://verifier:18735/~tpm-interpret@1.0/info    # every handler's params + response
curl http://verifier:18735/~tpm-interpret@1.0/checks  # machine-readable 5-check spec
```

The `checks` response is particularly useful for building
dashboards / alerting rules — each entry has `{name, purpose,
failure_implies}` so you can render failure modes in plain
English without re-encoding verifier internals.

## Sanity-check the peer independently (without HB)

If the HB verifier's runtime is unavailable (LMDB issues,
misconfigured trust anchor, whatever), the Python reference
verifier is a fallback with no BEAM dependency:

```bash
scripts/hb-peer-probe.sh http://peer.example:8734
```

Uses `python3 + openssl + curl` only. Exits 0 iff all 5 crypto
checks pass against the peer's current envelope + the CA at
`out/test-tpm-ca.crt` (captured by `boot-hb.sh` when the peer
was last booted locally). Useful in CI and for proving "peer is
attesting correctly" when you suspect the verifier itself.

## What a verifier does NOT need to deploy

- A TPM, swtpm, or libtss2 runtime (verifier does no attesting)
- Root access (unless your config requires it)
- KVM / QEMU (the verifier runs in any Erlang 27 environment)
- Arweave connectivity (attestation verification is offline)
- A persistent disk (an ephemeral fs-store works; see STATUS.md
  re: HB-platform cache link-resolution nuance with fresh fs
  stores)
