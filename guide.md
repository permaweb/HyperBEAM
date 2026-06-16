# Odysee / LBRY → HyperBEAM verifiable-commitment layering

This guide explains the additive commitment layering landed in commit `de335a84`
(branch `feat/odysee-on-hb-extras`) and how to **use** and **run** it. Every claim
below was checked against the source:

- `src/preloaded/codec/dev_lbry_tee_tail.erl` — the `lbry-tee-tail@1.0` device and the layering model
- `src/preloaded/auth/dev_snp.erl` — the `snp@1.0` device (`verify/3`, `generate/3`)
- `src/core/lib/hb_snp_nif.erl` — the NIF wrapper (`check_snp_support/0`, graceful `init/0`)
- `src/core/resolver/hb_message.erl` — `commit/2,3` and `verify/1,2,3`
- `src/preloaded/message/dev_message.erl` — per-commitment `verify/3` dispatch
- `src/core/resolver/hb_opts.erl` — default `commitment-device` (`httpsig@1.0`)
- `native/dev_snp_nif/src/{helpers,verification}.rs` — AMD KDS fetch, on-disk cache, graceful errors

---

## 1. TL;DR — the layered model

Every MMR roll-forward / evidence message carries **additive** commitment layers:

| Layer | What it is | Device / mechanism | Trust required | Always present? |
|-------|-----------|--------------------|----------------|-----------------|
| **L0** | Trustless content proof — MMR membership / chunk / consistency against a pinned snapshot root | `lbry-header@1.0`, verified against node opt `lbry-header-root` | none — anyone recomputes it | yes (it is the payload) |
| **L1** | **Node-operator signature** over the root binding | `httpsig@1.0` / `rsa-pss-sha512` by the node's `priv_wallet` | trust the operator vouched | **yes — the always-on floor** |
| **L2** | SEV-SNP TEE attestation binding the same root | `snp@1.0` via `dev_lbry_tee_tail` (`tee-tail` / `mmr-genesis`) | trust AMD hardware + measurement | only when the host can attest |

**The one rule (verbatim from the code's own comment):** *"if snp, add tee+sig;
otherwise sig."*

- SNP-capable host → message ships **L0 + L1 + L2**.
- Non-SNP host → message ships **L0 + L1** — a complete, honest commitment set, no placeholder, no faked L2.

The switch is by **capability**, not a hand-set flag: `hb_snp_nif:check_snp_support/0`
decides. The **same node image** runs real on an SEV-SNP host and degrades cleanly
elsewhere with no code change. `snp-required` is a *verifier* policy that demands L2.

---

## 2. Signatures — the L1 node-operator signature (the primary layer)

**This is what integrators rely on first.** L1 is present on every served response
and on every layered evidence message, with or without SNP hardware.

### 2.1 What L1 is

L1 is the node operator's signature over the message. The default
`commitment-device` is `httpsig@1.0` (`hb_opts.erl:285`:
`<<"commitment-device">> => <<"httpsig@1.0">>`), which produces an
`rsa-pss-sha512` signature using the node's wallet (`priv_wallet`, passed in opts
as `priv-wallet`). This is the **same** signature HyperBEAM already attaches to its
HTTP responses — nothing LBRY-specific. It means: *the operator at this address
vouches that the bound content is what they served.*

### 2.2 Where L1 is produced

In `dev_lbry_tee_tail:layered_evidence/2` the node signs a small, commit-safe
**summary** that binds the snapshot root (`from-root`) and the rolled-forward root
(`to-root`):

```erlang
Summary = #{
    <<"device">>    => <<"lbry-header@1.0">>,
    <<"layer">>     => <<"lbry-mmr-rollforward">>,
    <<"from-root">> => to_hex(FromRoot),
    <<"to-root">>   => to_hex(ToRoot)
},
Signed = hb_message:commit(Summary, Opts).   %% L1: httpsig@1.0 / rsa-pss-sha512
```

`hb_message:commit/2` signs with the wallet in `Opts` under the default
`commitment-device`. To pin the device explicitly, `hb_message:commit/3` accepts a
codec name binary that becomes the `commitment-device`
(`hb_message.erl:482-483`):

```erlang
hb_message:commit(Msg, Opts, <<"httpsig@1.0">>).
```

The bulky trustless MMR proof (L0) is **not** re-signed under L1 — it is verified
separately in its raw form by the `lbry-header@1.0` codec against the pinned root.
The `to-root` binding ties L0, L1 and (when present) L2 to one root.

### 2.3 The `commitments` map

A committed message carries its signatures under a top-level `commitments` map:
`#{ CommitmentID => CommitmentMsg }`. Each commitment message names its
`commitment-device` (e.g. `httpsig@1.0` for L1, `snp@1.0` for L2). Verification
dispatches per commitment on that field.

### 2.4 How a verifier checks L1

`hb_message:verify/3` is the entry point. `verify(Msg, all, Opts)` normalizes to
`verify(Msg, <<"all">>, Opts)` (`hb_message.erl:547-548`) — verify all commitments.
It routes to `dev_message:verify/3`, which iterates the `commitments` map and ANDs
over them with `lists:all`, dispatching each commitment to the module named by its
`commitment-device` (`dev_message.erl:316-321, 331-340`). The result shape is
`{ok, Boolean}`.

So a message carrying both an `httpsig@1.0` (L1) and an `snp@1.0` (L2) commitment is
accepted only if **both** pass — the AND-composition is free.

`dev_lbry_tee_tail:verify_layered/2` uses exactly this:

```erlang
L1OK = hb_message:verify(Msg, #{ <<"commitment-ids">> => <<"all">> }, Opts) =:= true,
```

### 2.5 Concrete example (commit + verify, real API)

```erlang
%% --- L1 only (no SNP host): commit a root-binding summary and verify it ---
Wallet = ar_wallet:new(),
Opts   = #{ <<"priv-wallet">> => Wallet },

Summary = #{
    <<"device">>    => <<"lbry-header@1.0">>,
    <<"layer">>     => <<"lbry-mmr-rollforward">>,
    <<"from-root">> => FromRootHex,
    <<"to-root">>   => ToRootHex
},

Signed = hb_message:commit(Summary, Opts),            %% attaches httpsig@1.0 / rsa-pss-sha512
{ok, true} = hb_message:verify(Signed, all, Opts),    %% operator signature checks out

%% Recover who vouched:
Signers = hb_message:signers(Signed, Opts).           %% [<<"OperatorAddress">>]
```

This is the always-on floor. Any integrator can verify it with nothing but the
operator's public address — no hardware, no AMD network, no fixtures.

---

## 3. Mock mode — `snp-mode = mock`

Mock mode lets the L2 plumbing run with **no SEV-SNP hardware**. Set it via the
`snp-mode` opt (string `<<"mock">>`).

What it does, precisely:

- **Producer** (`generate_report/2`, `generate_backend/1`): reads the canned report
  `test/admissible-report.json`, primes the process-dictionary NIF mock
  (`mock_snp_nif_response` / `mock_snp_nif_enabled`, the same gate `dev_snp:generate/3`
  reads), and dispatches the real `snp@1.0` `generate`. It logs loudly:
  `"⚠ SNP report MOCKED - no SEV-SNP hardware; canned report (verification still real)"`.
- **Verifier** (`verify_attestation/2` with `verify_backend/1 => mock`): skips **only**
  the hardware/network crypto leaf (the AMD KDS signature check), and logs
  `"⚠ SNP NIF MOCKED - L2 attestation NOT cryptographically validated; CI/dev only"`.
- **Still real** even in mock mode: the L1 node signature, the MMR **linkage**
  recompute, and the **binding** check (attested `tee-to-root` must equal the
  committed `to-root`). The negatives (tampered tail, wrong `snp-trusted`,
  wrong genesis) all reject through real Erlang logic regardless of mode.

**When to use it:** offline development and CI, where there is no `/dev/sev-guest`
and no outbound network to AMD KDS.

**Never the production trust story.** Mock skips the attestation crypto leaf; it
proves the wiring and the verifier logic, not that a genuine enclave produced the
report.

---

## 4. SNP mode — `snp-mode = auto` (default) / `real`

This is real AMD SEV-SNP attestation (L2).

### 4.1 The capability switch (`snp-mode`)

The mode resolver lives in two small functions used by producer and verifier so the
two paths can never disagree:

- **`generate_backend/1`** (producer): `<<"real">>` → real; `<<"mock">>` → mock;
  otherwise (`<<"auto">>` / unset) probe `hb_snp_nif:check_snp_support/0` —
  `{ok, true}` → real, anything else → mock. A fresh hardware report needs real SNP
  support, so the producer probes.
- **`verify_backend/1`** (verifier): verification is hardware-free, so it defaults to
  the **real** `snp@1.0` device; only an explicit `<<"mock">>` engages the offline shim.

`attest_capable/1` decides whether L2 attaches: `mock`/`real` always attach; `auto`
attaches only if `generate_backend/1` resolves to `real` (i.e. the host actually has
SNP). **Auto on a non-SNP host yields the honest L0+L1 fallback — L2 is absent, not
faked.**

### 4.2 How L2 is verified

`verify_attestation/2` (real backend) delegates to the packaged device exactly as
`dev_green_zone` does:

```erlang
hb_ao:resolve({as, <<"snp@1.0">>, Evidence}, <<"verify">>, Opts)
```

This runs `dev_snp:verify/3`, the **6-check AND** pipeline
(`dev_snp.erl:75-96`), all of which must pass:

1. `verify_nonce` — report nonce binding
2. `verify_signature_and_address` — signer == the node's wallet address
3. `verify_debug_disabled` — SNP policy debug bit (bit 19) is off
4. `verify_trusted_software` — measurement is in the verifier's `snp-trusted` set
5. `verify_measurement` — measurement digest (NIF, pure byte-compare)
6. `verify_report_integrity` — AMD report ECDSA signature (NIF → AMD KDS)

Checks 1–4 are pure Erlang; 5–6 call the NIF. Note the identity link: `dev_snp`
signs its report with the node `priv_wallet` and requires that same address among
the signers — the **identical wallet** that produces the L1 `httpsig@1.0` signature.
So L1 and L2 are bound to one operator identity.

### 4.3 The two commitment classes (`lbry-tee-tail@1.0`)

- **`tee-tail`** — an attested live-tail consistency extension of the pinned
  snapshot root. `verify_tee_tail/2` holds iff:
  (1) **linkage** — the tail `delta-leaves` append to the pinned root (`lbry-header-root`)
  and re-bag to the committed `to-root` (`hb_lbry_mmr:verify_consistency/4`);
  (2) **attest** — the embedded `tee-evidence` is a valid `snp@1.0` attestation;
  (3) **binding** — attested `tee-to-root` == committed `to-root`.
- **`mmr-genesis`** (optional, off the serving path) — `verify_mmr_genesis/2`
  binds the network genesis hash and the snapshot root into one attestation, so a
  fresh verifier can accept the **provenance** of the 32-byte snapshot root from one
  attestation instead of re-validating the whole genesis→snapshot chain. A message
  without it still verifies against the pinned root through L0.

`dev_lbry_tee_tail:verify/3` dispatches on the commitment `type` (`<<"tee-tail">>`
or `<<"mmr-genesis">>`, request first then base), leaving the base `type` free for a
co-resident `lbry-header@1.0` commitment.

### 4.4 What a real demo host needs ("ready without mocks")

For the pushed image to run **fully real** with zero code change (all must be true):

- the built `dev_snp_nif` `.so` is present (release artifact)
- host is AMD SEV-SNP (Milan or newer); `check_snp_support/0` → `{ok, true}`
- outbound network to `https://kdsintf.amd.com` (VCEK + ARK/ASK cert chain)
- `test/OVMF-1.55.fd` on disk (the firmware fixture for `compute_launch_digest`)
- `snp-trusted` contains the host's real measurement (derived once via
  `compute_launch_digest` over its firmware/kernel/initrd/append, or pinned after
  first attested boot) plus the LBRY-codec component hashes
- `snp-mode = auto` (default)

If any are false, `auto` degrades to the labelled mock — **nothing in code changes**.

### 4.5 VCEK / cert on-disk cache and graceful degradation

The AMD ARK/ASK chain (per product) and the VCEK (per chip-id + reported TCB) are
immutable, and AMD rate-limits KDS, so `native/dev_snp_nif/src/helpers.rs` caches
them on disk under `$TMPDIR/hb-snp-cache`:

- `request_cert_chain/1` and `request_vcek/...` read the cache first, fetch on miss.
- Writes are **best-effort** and only cache a *parsable* artifact (a valid PEM chain
  of ≥ 2 certs, or a DER-decodable VCEK) — a rate-limit/error page is never stored.
- If the cache dir cannot be created, the fetch path is used directly.

Error handling is graceful, not panicking: `verify_signature` in `verification.rs`
returns an Erlang `{error, Reason}` tuple on JSON-parse failure rather than
unwinding. `hb_snp_nif:init/0` degrades when the `.so` is absent — it logs a warning
(`"dev_snp_nif NIF not loaded ...; SNP real path disabled, running L0+L1 +
capability-gated mock only"`) and still loads the module, so the node keeps serving
L0+L1. (Caveat for hardened deployments: the cert/VCEK fetch path can still fail
hard if KDS returns a garbled response; the device tests `try ... catch
error:nif_panicked` around the real-attestation path and **skip** rather than fail
when KDS is unavailable.)

---

## 5. How to run

### 5.1 Build (the Rust NIF)

`rebar.config` wires the `rebar3_rustler` plugin (`plugins` list) and a cargo build
hook for `native/dev_snp_nif`:

```erlang
{cargo_opts, [ {src_dir, "native/dev_snp_nif"}, {release, true} ]}.
{provider_hooks, [ {post, [ {compile, {cargo, build}}, {clean, {cargo, clean}} ]} ]}.
```

Per this repo's toolchain convention, build with `-fpermissive` for the C NIFs:

```bash
CFLAGS="-fpermissive" rebar3 compile
```

The cargo build needs the OpenSSL dev headers and network to crates.io plus the
`PeterFarber/sev` git fork. The `.so` lands at
`priv/crates/dev_snp_nif/dev_snp_nif` (loaded by the `?load_nif_from_crate` macro in
`src/core/include/cargo.hrl`). On a non-SNP box, `hb_snp_nif:check_snp_support/0`
then returns a clean `{ok, false}` — proof the NIF loaded and the capability probe
works.

> If the Rust toolchain/network is unavailable, the graceful `init/0` lets the same
> tree still run in mock mode — but the shipped artifact for the demo machine must
> include the built `.so`, or the real path can't engage.

### 5.2 Run the device tests (they double as runnable examples)

The eunit suites in `dev_lbry_tee_tail.erl` and `dev_snp.erl` are the executable
spec. These are PRELOADED devices: they are packaged and run under their
device names (e.g. `'lbry-tee-tail@1.0'`) by the Forge device-test harness, NOT
by plain `rebar3 eunit` (which reports `module dev_snp not found in project`).
Select one or more device roots with `-d/--devices`:

```bash
# Build a fresh preloaded-store and run the selected device suites:
CFLAGS="-fpermissive" rebar3 device test --devices dev_lbry_tee_tail
CFLAGS="-fpermissive" rebar3 device test --devices dev_snp,dev_lbry_tee_tail,dev_lbry_header,dev_lbry_transaction,dev_lbry_comment

# Omit --devices to run every device suite; add --with-core to also run the
# normal core `rebar3 eunit` modules in the same run:
CFLAGS="-fpermissive" rebar3 device test
CFLAGS="-fpermissive" rebar3 device test --with-core
```

The trustless MMR/commitment core libs (`hb_lbry_mmr`, `hb_lbry_commitment`,
`hb_lbry_attestation`) and the NIF wrapper (`hb_snp_nif`) ARE plain modules and
run under eunit directly:

```bash
CFLAGS="-fpermissive" rebar3 eunit --module=hb_snp_nif,hb_lbry_mmr,hb_lbry_commitment,hb_lbry_attestation
```

Port hygiene: the device-test / eunit harness boots a node on port 8734. If a
run is interrupted, free the port before the next run, or boots fail with
`eaddrinuse`:

```bash
until ! lsof -ti tcp:8734 >/dev/null 2>&1; do lsof -ti tcp:8734 | xargs -r kill -9; done
```

Tests worth knowing as examples:

- `layered_fallback_no_snp_test` — `auto` on this non-SNP host attaches L0+L1 only,
  verifies fully (no shim), and `snp-required` rejects the L1-only message.
- `layered_mock_positive_test` / `layered_snp_required_present_test` — mock attaches
  L2; L1 + L2 both verify; `snp-required` accepts.
- `tee_tail_mock_positive_test`, `tee_tail_tampered_tail_test`,
  `tee_tail_wrong_trusted_test`, `mmr_genesis_mock_positive_test` — positives and
  negatives through the real Erlang verifier.
- `layer_demo_test_` — **the self-describing demo**: it prints the attached layer set
  on this host vs a simulated SNP host, e.g.

  ```
  === Odysee MMR roll-forward: capability-switched layered commitment ===
   host hb_snp_nif:check_snp_support => {ok,false}
   auto (this host):     L0 trustless-MMR (verified separately) + L1 node-signature
   snp-host (simulated): L0 trustless-MMR (verified separately) + L1 node-signature + L2 TEE-attestation
   one verify_layered/2 accepts both; snp-required upgrades the floor.
  ```

  The single most important thing it shows: **the same `verify_layered/2` call passes
  on L0+L1 alone (laptop) and on L0+L1+L2 (SNP box)** — the fallback is not degraded
  plumbing, it is one fewer real layer.

### 5.3 Optional — HTTP node (shows the wire)

Start a node via `hb_http_server:start_node` and POST a committed message for verify.
**Port hygiene:** test/demo nodes bind **port 0** (an ephemeral port), not the
default `8734` — do not leave `8734` bound. Loop on an HTTP-200 readiness
precondition rather than sleeping. Pre-bake the committed message in the test process
and POST it, because the mock lives in the setter's process dictionary and is not
visible to the server process (this caveat disappears on a real SNP machine, where no
process-dict mock is in play).

---

## 6. Honest trust boundary (per layer)

| Layer | Proves | Does **not** prove |
|-------|--------|--------------------|
| **L0** (`lbry-header@1.0`) | The tail/chunk/membership is consistent with the pinned snapshot root `R_S`; fully recomputable by anyone. | That `R_S` itself is the canonical LBRY genesis→snapshot result (that is the optional `mmr-genesis` / L2 job) — the verifier *pins* `R_S`. |
| **L1** (`httpsig@1.0`) | The operator at a known address vouched for this root binding; tamper-evident; ties the message to one identity. | Anything about the hardware or that the verify code ran in an enclave. It is operator trust, not hardware trust. |
| **L2** (`snp@1.0`) | A genuine AMD SEV-SNP enclave, running a trusted measurement (in `snp-trusted`), with debug off, signed this exact root binding — and it is the same wallet as L1. | More than the measurement asserts. In **mock** mode it proves *nothing* cryptographic — only the wiring; mock is loudly labelled and CI/dev only. |

Rule of thumb for a verifier: **L0 is trustless**, **L1 is the always-present
operator floor**, **L2 is the upgrade** you can *demand* with `snp-required` when you
need hardware-rooted trust. The same producer image gives you L0+L1 everywhere and
adds L2 wherever the host can attest, with no code change.
