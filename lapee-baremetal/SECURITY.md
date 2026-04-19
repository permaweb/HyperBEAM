# LapEE `~tpm2@2.0a` — security analysis (and comparison to `~snp@1.0`)

Written 2026-04-19 as part of the `agent/lapee` PR sign-off. Applies
to the implementation at this commit; physical-silicon caveats are
called out where they appear.

> The LapEE paper's honest premise is "secure-*ish*" attestation for
> decentralised compute on commodity laptops. The point of this
> document is to be specific about what *ish* means, side-by-side with
> the strongest adversary model HyperBEAM currently attests under
> (AMD SEV-SNP via `~snp@1.0`). **Neither device gives you full
> remote-introspection resistance on its own.**

## 1. TL;DR

|    | `~snp@1.0` (AMD SEV-SNP) | `~tpm2@2.0a` (LapEE) |
|----|--------------------------|----------------------|
| Root of trust | AMD VCEK chain, rooted in AMD ARK/ASK | TPM vendor EK cert, rooted in Infineon/STM/Nuvoton/… |
| Runs on | EPYC 7xx3/7xx4 generation up, Intel-free | Any commodity board with a discrete/firmware TPM 2.0 |
| Primary adversary defended against | Malicious hypervisor / cloud operator | Remote network adversary + software-only local adversary |
| Memory confidentiality | Hardware-encrypted RAM (SME/SEV) | **None** — kernel runs in plaintext RAM |
| Runtime code attestation | Measured on launch; VMPLs for updates | Launch-time IMA + PCR 15 hash of node message; no running-memory hash |
| Replay protection | SNP report includes report-data nonce | TPM2_Quote includes extraData nonce |
| What's signed | Firmware + kernel + initrd + append = *launch measurement* | PCR state = SRTM (firmware → bootloader → kernel) ∥ IMA (files) ∥ LapEE PCR 15 (node-msg id) |
| Requires an on-prem/edge deployment to have | EPYC silicon + a cloud/host that passes SNP through | a laptop with a TPM 2.0 chip, which is ubiquitous |
| TCB below the attested code | AMD PSP firmware | TPM vendor firmware, platform UEFI firmware, bootloader, kernel |
| Confidentiality vs. physical attacker | RAM encrypted (but TCB-wide bus tamper still hard) | **None** — DMA / cold boot / SPI-bus sniff all apply |

LapEE's model is deliberately **weaker on the "protect against the
person holding the laptop" axis** and deliberately **stronger on the
"runs on anything" axis**. The paper's reasoning: if you target a
decentralised compute network where every participant could run an
attested node, SNP-level requirements would exclude most of the
fleet. TPM-based attestation proves enough to be useful — e.g. "this
node is running the code I compiled against this kernel with this
operator wallet" — without requiring specific silicon.

## 2. Threat model — `~tpm2@2.0a`

### In scope (what we defend against)

1. **Remote network attacker** who can only reach the HB HTTP port.
   They cannot rewrite the attestation envelope without breaking the
   TPM quote signature (`crypto:verify(rsa, sha256, ..., rsa_pss)`
   under the AK); they cannot replay a quote without hitting the
   nonce mismatch (verifier check 3); they cannot claim a different
   PCR set without breaking pcrDigest (check 4).
2. **Software-only local adversary with root after boot.** They can
   read/write memory, but by that time the PCR 15 value is already
   fixed and signed by a quote they cannot backdate. Any subsequent
   node-message mutation would produce an id that does not match the
   extended PCR.
3. **An operator who deliberately ships the wrong `node_message`** to
   a peer. The enforced config — the `on.start` hook — is layered
   LAST in `HB_CONFIG` (see `hb_opts:load/2` and the layering
   demonstration in `out/evidence/`). Even a malicious user config
   that says `on/start/device: noop@1.0` is overridden by the
   enforced config's `tpm2@2.0a/extend`, so the TPM still extends PCR
   15 with the real node-message id. The attestation encodes which
   node message was extended, so the peer can tell.
4. **Evil firmware replacing the kernel.** Defeated by Secure Boot +
   UKI (when deployed on physical hardware with the vendor UEFI CA
   trusted). The PCRs 0/1/7 from SRTM appear in the quote — a
   different kernel produces different measurements.
5. **Tampered envelope on the wire.** The peer-side `verify/3`
   rejects with a specific failing check (proven in
   `scripts/hb-tamper-test.sh`: seven targeted byte-flips, each
   rejected at the expected check).

### Out of scope (known weaknesses; documented, not defended)

1. **Physical attacker with tooling.** SPI/LPC-bus sniffing extracts
   the PCR extend operations in cleartext (the [BitLocker sniffing
   attack](https://pulsesecurity.co.nz/articles/TPM-sniffing) applies
   here). A determined local attacker can swap the SSD, reflash the
   UEFI, disable Secure Boot through the firmware UI, or detach the
   TPM chip altogether. **LapEE does not defend against this and
   never claimed to.** The paper calls it "secure-ish" for exactly
   this reason.
2. **Cold-boot / DMA memory attack.** The kernel runs in plaintext
   RAM. IOMMU + kernel lockdown + disabling FireWire/Thunderbolt
   helps, but cold-boot (literally freezing RAM and reading it
   elsewhere) is still practical on unsealed devices.
3. **Firmware TPM ("fTPM").** Some x86 platforms emulate TPM 2.0
   inside the CPU's management engine (Intel PTT, AMD fTPM). Those
   are weaker than a discrete TPM — compromising the CPU
   management engine compromises the TPM. **The EK-cert chain
   check verifies the TPM vendor** (`chk_ek_chain/2` in
   `dev_tpm2`), so a verifier can reject fTPMs by refusing AMD/Intel
   roots and trusting only discrete-TPM vendor roots.
4. **SRTM dishonesty window.** PCR 0 is extended by the *first*
   firmware stage (CRTM, typically UEFI ROM). If that ROM has been
   overwritten, it can lie about what follows. DRTM (Intel TXT, AMD
   SKINIT) mitigates this by restarting measurement from a known
   hardware event. LapEE today uses SRTM. A future
   `~tpm2-drtm@2.0a` could bring DRTM where hardware supports it.
5. **TCB below the attested code.** We attest software starting from
   what the kernel sees. The firmware, bootloader, initial RAM disk
   are *measured* (PCRs 0/1/4/7) but we don't audit those
   measurements against a known-good expected-values database today.
   That is exactly what `~tpm-interpret@1.0` (item 7 in the overnight
   plan) is about.
6. **Supply-chain trust in the TPM vendor.** If Infineon's private
   root key leaked, an attacker could mint valid EK certs for
   non-existent TPMs. Same class of trust we place in any commercial
   PKI; same CA-compromise mitigations apply.

## 3. Threat model — `~snp@1.0`

### In scope (stronger than TPM where it applies)

1. **Malicious hypervisor.** SEV-SNP's guiding design goal. The
   hypervisor can see ciphertext in guest RAM but cannot read or
   modify guest code at runtime without breaking the SNP report's
   launch measurement or the Reverse Map Table (RMP) integrity.
2. **Cloud operator.** Same as above: the cloud provider's staff,
   even with host-kernel access, cannot sniff guest memory.
3. **Some classes of DMA attack.** With SEV-SNP enabled, attacks
   that read device memory do not yield useful plaintext (because
   RAM is encrypted with a VM-specific key).

### Out of scope (SNP's known weaknesses)

1. **Microarchitectural side channels.** Spectre-family attacks,
   CacheWarp (CVE-2023-20592), and in general anything that reads
   timing/cache from a co-resident tenant still works. SEV-SNP does
   not isolate on this axis.
2. **Firmware/TCB version bugs.** CVE-2024-21944 (SEV-SNP firmware
   RCE), CVE-2024-56161 (SEV-ES/SNP boot corruption) both illustrate
   the attack surface of AMD's own SNP firmware. Attestation reports
   embed the TCB version; the verifier must refuse out-of-date TCBs.
   The paper and this repo acknowledge this is an ongoing arms race.
3. **Debug-mode VMs.** If the SNP report's `DEBUG` bit is set, the
   hypervisor CAN introspect the guest. The `dev_snp:verify/3`
   implementation here (see `src/dev_snp.erl`, line 19 / bit index
   `DEBUG_FLAG_BIT`) explicitly refuses reports with the debug flag
   on.
4. **Physical CPU attack.** Decapping, voltage glitching, and
   similar require lab equipment but are on the known attack
   surface for all such TEEs (SGX, TrustZone, SNP). Same order of
   magnitude as attacking a discrete TPM physically — i.e., not a
   defence against a nation-state adversary, definitely a defence
   against a software-only adversary.

## 4. What each device actually attests to

### `~snp@1.0` launch measurement

The SNP launch measurement is `H(firmware ∥ kernel ∥ initrd ∥ append)`
(see `COMMITTED_PARAMETERS` in `dev_snp.erl`). A verifier checks:
- the report is signed by a VCEK whose cert chains to the AMD
  root of trust;
- the launch measurement equals the hash of a *known* trusted
  configuration (verifier's `snp_trusted` list);
- the debug flag is off;
- the nonce in `report_data` matches a fresh challenge the verifier
  sent (so replay is impossible).

This tells you: "this specific VM image booted, inside a genuine AMD
SNP VM that is not in debug mode". It does NOT tell you what the
process inside the VM has been up to since boot.

### `~tpm2@2.0a` quote

A successful LapEE attestation carries the quote over PCRs 0, 1, 7,
10, 11, 14, 15 (configurable), where:
- `0, 1, 7` — SRTM: firmware, early boot policy, Secure Boot keys
- `10` — IMA runtime measurements (hash of every `execve`'d binary
  matching the `ima_policy=tcb` policy, cumulative)
- `15` — LapEE node identity (SHA-256 of `hb_message:id(NodeMsg,
  all, Opts)` at boot via the enforced `on.start` hook)

A verifier checks:
- EK cert chains to a trusted TPM vendor root;
- TPM2_Quote signature valid under the AK public key, AK loaded from
  the EK's endorsement hierarchy;
- the nonce in `extraData` matches a fresh challenge (replay
  protection);
- the quote's `pcrDigest` equals `SHA-256(pcr_values concatenated in
  selection order)` (so reported PCRs are the actual ones signed);
- replay the event log → recompute PCR 15 → matches the quoted
  value;
- that PCR 15 event's digest equals the envelope's `node_message_id`
  (so the attested PCR commits to the claimed node message).

This tells you: "as of quote-time, the TPM reports PCR 15 =
sha256(0 ∥ node_message_id), and PCRs 0/1/7 match what a known-good
kernel would produce, and PCR 10 has this IMA chain of file hashes".

Because the user config is merged into `node_message` before the
enforced hook fires, PCR 15 also commits to the operator's
configuration — a node claiming to be "provider X" under this
wallet cannot silently swap the wallet or the routes without the
PCR 15 value changing.

## 5. Where LapEE is stronger than SNP (for our model)

1. **Deployment reach.** Every modern laptop (post-2012-ish) has TPM
   2.0. SEV-SNP needs EPYC Rome/Milan/Genoa (3rd-gen+). For a
   compute network that wants "any operator with a laptop can join",
   only TPM clears that bar.
2. **Vendor diversity.** TPMs come from multiple vendors (Infineon,
   STM, Nuvoton, Nationz, a few others). Compromising one vendor's
   root breaks attestation for that vendor's chips but not the
   others. SNP is AMD-only; a compromise of AMD's VCEK issuing key
   ends the model.
3. **Operator controls physical security.** The attacker model for
   a laptop-under-your-desk is different from a VM-in-a-datacentre:
   the operator can lock the laptop in a safe, tamper-evident seals
   and all. In a cloud SNP deployment, *you* do not control the data
   centre's physical security; you trust the provider to.
4. **Public specs.** TPM 2.0 is a public TCG specification; SNP's
   firmware is proprietary.
5. **Orthogonality.** LapEE does not preclude SNP. A node can expose
   *both* `~tpm2@2.0a/attestation` and `~snp@1.0/generate`; a
   verifier can require both. Run LapEE on SNP silicon and you get
   SNP's memory encryption + TPM's orthogonal vendor root as
   defence-in-depth.

## 6. Where SNP is stronger

1. **Cloud-hosted compute** where the operator does not control the
   physical box. SNP defends against the hypervisor; TPM does not.
2. **Memory confidentiality.** SEV encrypts RAM. LapEE does not.
3. **Runtime introspection resistance.** SNP's RMP locks down who
   can touch guest pages at each access. TPM-only does not.
4. **Homogeneous TCB reasoning.** All SNP nodes have the same AMD
   TCB version set; you can reason about "all nodes at TCB >= N" as
   a single property. TPM + IMA deployments have heterogeneous TCBs
   unless pinned.

## 7. Composition: using both

The two are complementary, not competing:

```
+------------------------------+
|   HyperBEAM node on SNP VM   |
|                              |
|  [ kernel + initrd + HB ]    |  <-- SNP launch measurement
|       :                      |
|   vTPM / passthrough TPM     |  <-- PCRs 0,1,7,10 + PCR 15 extend
|                              |
+---------------------+--------+
                      |
                      v
        attested by  ~snp@1.0   + ~tpm2@2.0a
```

On SNP hosts that pass a vTPM into the guest (or where a
paravirtualised TPM is available), a node can expose both
endpoints; a verifier can cross-check. The two roots of trust are
INDEPENDENT: compromising AMD's VCEK doesn't give you Infineon's
EK key and vice versa. So composing both *multiplies* the
attacker's work to silently impersonate a legitimate node.

For LapEE-on-laptop (no SNP), you get exactly one root of trust:
the laptop's TPM vendor. That is enough for the paper's stated
"decentralised compute on untrusted commodity hardware" pitch; it
is not enough to replace SNP where a cloud hypervisor is the
adversary.

## 8. Current LapEE implementation — concrete weaknesses + mitigations

The bare `~tpm2@2.0a` implementation in this branch has a few
deliberate-for-now weaknesses. Listing them explicitly so they don't
accumulate silently.

| # | weakness | mitigation (shipped?) |
|---|---|---|
| 1 | EK cert chain roots at a per-boot *test* CA, not a TPM vendor root | when deploying on real silicon, point `lapee_tpm_ca_cert` at a bundle of real TPM vendor roots. The verifier is agnostic; only the trust anchor changes. |
| 2 | No challenge-response with the node's wallet (envelope carries `wallet_address` but doesn't prove live control) | easy addition: ask the node to sign the verifier's nonce with its wallet; the current envelope already carries `node_message_id`, so binding wallet to the attested node is straight-forward but not yet done |
| 3 | IMA currently only *measures*, does not *appraise* (so a tampered binary still runs, the PCRs just change) | kernel fragment now sets `IMA_APPRAISE=y`; a signed IMA policy + a trusted keyring is needed at deployment time — enabling that is a runtime step, not a rebuild |
| 4 | Secure Boot / UKI / dm-verity pipeline scripted but not exercised on the QEMU demo | pipeline lives in `scripts/uki.sh`, `scripts/secureboot-keys.sh`, `scripts/verity.sh`; physical-silicon demo is the right venue |
| 5 | Kernel debug flags set (`DEBUG_KERNEL=y`) in the Buildroot fragment used for the demo | fragment now says `# CONFIG_DEBUG_KERNEL is not set`; a Buildroot rebuild picks it up |
| 6 | `LOCK_DOWN_KERNEL_FORCE_CONFIDENTIALITY` not enabled (integrity-only lockdown) | deferred: confidentiality mode blocks perf/kprobes/tracefs, some of which HB's observer_cli uses |
| 7 | No TPM PCR policy-sealed secrets (e.g. sealing the operator wallet key to a specific PCR 15 value) | future `~tpm2-seal@2.0a` device; blocker to "restart unmodified or nothing unlocks" |
| 8 | No DRTM (SRTM only) | future `~tpm2-drtm@2.0a` on SKINIT/TXT-capable hardware |
| 9 | A single verifier's CA bundle is static — no revocation of compromised EK certs | the verifier has `trusted-ca-pem` as a Req field; rotating trust is a config change, but a proper revocation + OCSP/CRL story is work |
| 10 | Sub-resource measurements (e.g. Lua scripts the node loads at runtime) are not measured into PCR 15 | arguably belongs in a separate PCR (say PCR 14) via `dev_tpm2:extend' calls from `on.execute' hooks; future expansion |

## 9. What `~tpm-interpret@1.0` adds

The companion device (next overnight phase, item 7 of the user's
brief) interprets a verified attestation into named AO-Core fields a
verifier's policy can reason about: the TPM vendor (from the EK
cert), the firmware SRTM expected values for common board vendors,
the kernel/UKI identity (from PCR 4/11 / kernel appended hash), the
IMA chain, and so on. That moves the attestation from "a
cryptographic proof that SOMETHING happened" into "a structured
description of what you can assume about this peer" — which is what
the HB network needs to actually *make decisions* based on peer
attestations.

None of that changes the chain's guarantees; it just makes the
already-proven guarantees usable.
