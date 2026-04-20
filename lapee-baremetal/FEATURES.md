# LapEE — feature coverage + hardware requirements

The commander's-intent question: *does a LapEE node have every
feature it needs to be practically useful? on what subset of
hardware?* This doc is the explicit answer, maintained alongside
the code so it can be re-audited each release.

Conventions below:

- **Shipped** = in the code + exercised by `make
  hb-final-acceptance`.
- **Partial** = enough to work in the common case but missing
  something for harder use.
- **Parked** = deliberate gap, documented in STATUS.md with
  rationale, not intended for this release.
- **Open** = unknown / undecided, needs thought.

## 1. Attestation loop (the core)

| feature | state | notes |
|---|---|---|
| Real Linux kernel boot | Shipped | Buildroot 2024.02 LTS, Linux 6.6.51, hardened (FORTIFY_SOURCE, HARDENED_USERCOPY, INIT_ON_ALLOC_FREE, SLAB_FREELIST_HARDENED, IO_STRICT_DEVMEM, MODULE_SIG_FORCE, SECURITY_DMESG_RESTRICT, IMA_APPRAISE, DEBUG_KERNEL=n). 12 MB uncompressed kernel + 62 MB gzipped initramfs = 74 MB boot image. |
| Real TPM 2.0 access | Shipped | `lapee_tpm_nif` → `libtss2-esys` (807 lines C). Supports `device:/dev/tpmrm0` and `swtpm:host=...` TCTI strings (`LAPEE_TPM_TCTI` env var). |
| HB release as PID 2 | Shipped | `init` (PID 1) → `exec /usr/lib/hyperbeam/bin/hb foreground`. |
| Enforced `on.start` hook | Shipped | `config/lapee-enforced.flat` — extends PCR 15 with `hb_message:id(node_message)` on every boot; layered last in `HB_CONFIG` so hostile user config cannot override. Acceptance test `user-hostile.flat` proves override is blocked. |
| `/attestation` endpoint | Shipped | `dev_tpm2:attestation/3` returns v0.3 envelope: EK cert PEM, AK pub PEM, TPM2_Quote (pcr_selection, nonce, quoted, signature, pcr_values), runtime_event_log, node_message, node_message_id, wallet_address, issued_at_unix. All binaries base64url. |
| Content negotiation | Shipped | `accept: application/json@1.0` + `accept-bundle: true` = canonical HB pattern. No `attestation-json` hack endpoint. |
| Anti-replay nonce (attester side) | Shipped | `Req/nonce` accepted (base64url); if absent, random 32 bytes. |
| Anti-replay nonce (verifier side) | Shipped | `verify-peer' generates a fresh random 32-byte challenge per call and passes it in the peer fetch. After receiving the envelope, verifies the envelope's quote nonce matches the challenge BEFORE any crypto. Mismatch → hard reject with `nonce_freshness: "mismatch"'. Match → `nonce_freshness: "verified"'. Protects against replay of a previously-valid envelope captured off the wire. |

## 2. Verification loop

| feature | state | notes |
|---|---|---|
| EK cert chain → vendor root | Shipped | `pkix_path_validation` with a tightened verify_fun (only `{extension, _}` events allowed; every `{bad_cert, _}` is a hard reject). Regression-tested against `{bad_cert, {invalid_signature / unknown_ca / selfsigned_peer / invalid_issuer / cert_expired}}`. |
| TPM2_Quote signature verify | Shipped | `rsa_pss:verify/4` (SHA-256, salt 32). Pure OTP — no NIF needed on the verifier. |
| TPMS_ATTEST parse | Shipped | Magic (0xFF "TCG"), type 0x8018 (TPM_ST_ATTEST_QUOTE), qualifiedSigner, extraData (== nonce), clockInfo, firmwareVersion, pcrSelection, pcrDigest. |
| pcrDigest matches reported PCRs | Shipped | `sha256(concat pcr_values in selection order)` compared byte-wise. |
| extraData == nonce | Shipped | Byte-wise comparison after base64url decode. |
| Event log replay of PCR 15 | Shipped | `foldl sha256(Acc \|\| digest)` from `<<0:256>>`, compared to quoted PCR 15. Tightened: empty events → hard reject (explicit) even though `chk_binding` also catches it. |
| PCR 15 event commits to node_message_id | Shipped | Decode id to raw bytes, decode each event's digest, compare. Tightened: id decoded byte-size must be exactly 32; empty/short ids are a hard reject. |
| node_message + id shape | Shipped | 43-char base64url id, decodes to 32 bytes, node_message is a map. |
| `verify-peer` (cross-node) | Shipped | Separate HB process fetches peer's attestation, verifies locally, returns link-free summary. Tested natively on macOS against QEMU-guest peer. |
| Inline trust anchor | Shipped | `trusted-ca` query param (base64url PEM bytes) — HB-wire-convention, no URL-encoding ambiguity. Back-compat `trusted-ca-pem` for raw PEM (documented unsafe over GET). Honoured by BOTH `verify-peer` AND the `.../verify~tpm-interpret@1.0` chain URL (asymmetry fixed in commit 45a605daf; earlier version silently dropped it on the chain path). |
| `trust_anchor_source` in response | Shipped | Every verify path returns `"request"` / `"node_config"` / `"none"` so callers can tell which anchor was used — no silent overrides. Present on `/verify`, `/verify-peer`, and the chain URL. |
| Targeted chain-failure diagnostic | Shipped | When EK's issuer DN matches CA's subject DN but signature doesn't verify, the error message calls out "same CN, different generation" (stale per-boot CA) vs a true rogue — so operators know whether to refresh or investigate. Live-proven against a real rogue CA with matching CN. |

## 3. Gaps in the verification story — known

| gap | state | rationale / follow-up |
|---|---|---|
| Vendor root CA bundle | Parked | We don't ship AMD / Intel / Infineon / STMicro / Nuvoton root CAs. Every deployer ships their own. `priv/tpm-interpret/root-cas/` is the intended location. |
| Revocation (CRL / OCSP) | Open | No revocation check today. A compromised EK cert would continue to be trusted until the deployer rotates their CA bundle. |
| Nonce freshness policy | Shipped (promoted from Partial) | `verify-peer' now enforces challenge-response freshness automatically: verifier generates a random 32-byte challenge, includes it in the peer fetch, and rejects with `nonce_freshness: mismatch` if the envelope's quote nonce doesn't match. See §2 "Anti-replay nonce (verifier side)". |
| Clock authority | Open | `issued_at_unix` is self-reported. No trusted-time binding. |
| IMA per-file event log | Parked | Only PCR 10 final value is in the envelope. Per-file IMA chain not yet transported. A future `~tpm2@2.0a` envelope version will include it. |
| UKI PCRs 11/12/13 | Parked | No UKI in the QEMU dev path. Works on real silicon with `systemd-stub`. |
| AK rotation | Open | Current AK is a persistent_term; no rotation policy. Acceptable for a LapEE boot lifetime; for long-lived deployments, need documented rotation. |
| Replay detection | Open | A verifier accepts any well-formed quote whose nonce matches its own challenge. If you don't challenge and rely on `issued_at_unix`, replay is possible. |

## 4. Introspection + availability

| feature | state | notes |
|---|---|---|
| `/~tpm-interpret@1.0/info` | Shipped | Documents every handler's params + response shape + wire format. Self-describing. |
| `/~tpm-interpret@1.0/checks` | Shipped | Machine-readable list of the 5 crypto checks with per-check `{name, purpose, failure_implies}`. |
| `/~tpm-interpret@1.0/interpret` | Shipped | 9-section rich interpretation of an envelope (no crypto). |
| `/~tpm-interpret@1.0/verify` | Shipped | Crypto verify + interpret in one call. |
| `/~tpm-interpret@1.0/verify-peer` | Shipped | Cross-node verify; fetches peer envelope and runs full 5-check battery locally. |
| `/~tpm-interpret@1.0/summary` | Shipped | Link-free summary of a supplied envelope. Cheap — no crypto. |
| `/~tpm-interpret@1.0/peer-summary?peer=…` | Shipped | Fetches peer's envelope and returns the summary — 10× cheaper than verify-peer. Dashboards / peer browsing. |
| `/~tpm-interpret@1.0/peer-status?peer=…` | Shipped | Cheapest probe: reachable + envelope_version + wallet + node_message_id. For liveness. |
| Self-description via `~meta@1.0/info` | Open | LapEE-specific fields (TPM manufacturer, hook device) are NOT surfaced through the standard `~meta@1.0/info` surface. A future `on.info` hook could expose a cheap snapshot. |

## 5. Hardware subset — what works, what doesn't

### In-scope (tested or shipped)

| hardware | notes |
|---|---|
| **Discrete TPM 2.0** (Infineon SLB 9670/9672, STMicro ST33, Nuvoton NPCT7xx) | The paper's intended case. High trust: TPM runs on a dedicated chip with its own RAM + hardware-isolated storage. EK cert chain to vendor root is meaningful. |
| **AMD fTPM via PSP** (EPYC, Ryzen) | ACPI device ID `MSFT0101`. TPM 2.0 inside the Platform Security Processor (AMD's on-die security co-processor). Manufacturer `"AMD\0"` / `41 4D 44 00` = key `414d4400` in `manufacturers.json`. Lower trust than discrete — PSP is a firmware TEE, not a separate chip — but still real hardware isolation. Deploys require AMD's fTPM EK root CA. |
| **Intel PTT / fTPM** (Core, Xeon) | Equivalent to AMD's fTPM, inside Intel ME (Management Engine). ACPI `MSFT0101`. Manufacturer `INTC` (`494e5443`). Same trust tier as AMD fTPM. |
| **QEMU + swtpm** (development only) | `trust_tier: development_only` in the QEMU profile (`priv/tpm-interpret/pcr-profiles/qemu-seabios-tcg.json`). **Profile now populated with real measured PCR 0 + 7 values** (from SeaBIOS rel-1.16.3) — a QEMU-based LapEE attestation will produce `boot.match.attributes.platform_vendor: "QEMU"` + `trust_tier: "development_only"`, a recognisable marker even if an operator forgot to restrict their trust anchors. A production deploy should either refuse development_only matches or require a test-only trust anchor. |

### Out-of-scope / won't work

| hardware | why |
|---|---|
| **TPM 1.2 chips** | Incompatible. LapEE is TPM 2.0 only. |
| **No TPM at all** | LapEE is undefined without a hardware (or hardware-adjacent) root of trust. |
| **Cloud VMs without TPM passthrough** | Can run swtpm but with no real root of trust. Same security posture as the QEMU dev setup — test / development only. |

### Deploy-specific prerequisites

For a LapEE node to be *useful* on specific silicon:

1. **UEFI Secure Boot on** (so PCR 7 is non-zero and meaningful). The Buildroot dev image doesn't enable SB; real deployments do.
2. **Measured boot via TCG event log** (so PCR 0-7 have values the firmware wrote). Real UEFI firmware does this; the QEMU SeaBIOS path produces a deterministic but non-SB event log.
3. **Linux kernel with TPM driver + IMA enabled** (our Buildroot kernel has both). On a Debian / Ubuntu kernel, `tpm_tis` / `tpm_crb` drivers are usually present; IMA needs `CONFIG_IMA=y` + boot-time `ima_policy=tcb`.
4. **`libtss2` dynamic libraries on the running host** (`libtss2-esys`, `libtss2-mu`, `libtss2-tctildr`, `libtss2-rc`). Our guest image ships these. On a native host deploy, `apt install libtss2-0` or equivalent.
5. **User in `tss` group** (to open `/dev/tpmrm0` without sudo). Sysadmin step.
6. **The right TPM vendor CA bundle on the *verifier* side**. An AMD fTPM box's EK chains to AMD's CA, not Infineon's. This must be provisioned out-of-band on each verifier.

## 6. 90%-coverage hardware enumeration

From my reading, the 90% of LapEE-viable hardware a verifier is
likely to encounter falls into five TPM manufacturers. For each we
need (a) a vendor root CA (so chain verification works), (b) one
reference PCR profile per distinct firmware version (so boot-chain
interpretation is meaningful).

| manufacturer | `id` in `manufacturers.json` | kind | common platforms | root CA provisioned? | seed profile? |
|---|---|---|---|---|---|
| Infineon | `49465800` | discrete | Lenovo ThinkPad (SLB 9670/9672), Dell Latitude, many server boards | deployer-supplied | example stub |
| AMD fTPM (PSP) | `414d4400` | fTPM_cpu | All modern AMD Ryzen + EPYC | deployer-supplied | none yet |
| Intel PTT | `494e5443` | fTPM_cpu | All modern Intel Core + Xeon | deployer-supplied | none yet |
| STMicroelectronics | `53544d20` | discrete | Dell XPS, some Lenovo, embedded | deployer-supplied | example stub |
| Nuvoton | `4e544300` | discrete | Various consumer laptops | deployer-supplied | none yet |

The remaining ~10% — Nationz, Sinosun, Rockchip, Broadcom, Atmel,
Samsung, Google — are already enumerated in `manufacturers.json`
but not profiled.

**Coverage work** (tracked, not blocking this release):

1. Ship a `priv/tpm-interpret/root-cas/vendor-bundle.pem` that
   deployers *opt in to* if they want out-of-the-box
   multi-vendor trust. Today, the bundle is empty and each
   deployer builds their own.
2. Gather PCR 0 / 7 reference values from one representative
   model per vendor. Mechanism is in place (profiles are JSON
   files dropped in `pcr-profiles/`); data is the bottleneck.

## 7. "Do you have enough information to be assured of each feature?"

**Yes, for the core loop**. Every box in §1 + §2 is exercised by
`make hb-final-acceptance` (6/6 PASS: release build,
initramfs assembly, 3-envelope positive battery, 7-flip tamper
battery with Erlang + Python verifiers, live interpret-demo,
cross-node verify-peer). Every one of the 5 crypto checks has a
dedicated regression test that asserts the failing envelope is
rejected at the correct check.

**Partial, for the operational lifecycle (§3)**. The gaps there are
known and documented, not silent. They don't block shipping this
release, but they do constrain the deployments it's suitable for:

- LapEE-as-shipped is suitable for **per-request trust**: a caller
  generates a fresh nonce, asks the peer to sign it with its AK,
  verifies the attestation, and then trusts the peer *for that
  request*. Challenge-response style. *Freshness is now enforced
  automatically by `verify-peer'* — the verifier won't accept an
  envelope that isn't signed over its own random challenge, so
  replay of captured envelopes is prevented by construction.
- LapEE is **not yet** suitable for **long-lived trust**: if a
  caller wants to trust a peer "for the next hour", there's no
  revocation (CRL / OCSP) and no clock authority (the envelope's
  `issued_at_unix` is self-reported). Operators can re-challenge
  periodically to approximate long-lived trust at the cost of
  per-interval round-trip.

**On hardware**: discrete TPMs and fTPMs (AMD, Intel) are the
target substrate. The code is hardware-agnostic at the NIF layer
(libtss2 abstracts the TCTI); the deploy-specific pieces are the
vendor CA bundle and the PCR-profile DB, both of which are *data*
problems, not code problems.

The one concrete "new work" item I'd flag for a hardware-native
test day is finishing the AMD-fTPM path end-to-end:

1. Install `swtpm` / `qemu-system-x86` + put `odysee` in the
   `tss` group on `neo.zephyrdev.xyz` (EPYC 9254 / AMD fTPM).
2. Obtain AMD's fTPM EK root CA from their PKI.
3. Run the attester as root (or via `tss` group membership) on
   the real `/dev/tpmrm0`.
4. Ask a separate verifier to pull + verify the attestation.
5. Confirm the real EK chains to AMD's root.

Until that round-trip is closed against real silicon, §5 in
SECURITY.md carries "discrete-vs-fTPM trust tier" as an explicit
caveat rather than a proven property.
