# LapEE — core plan + acceptance criteria (self-reference)

Written at the start of the unattended-mode run. This file is how future-me
(later in the session, or after a restart) reminds present-me what "done"
looks like. Checked before committing anything as "complete."

## The thesis (paper §1, §4)

A real x86 guest boots a measured Linux kernel under QEMU with a real TPM
attached. Inside the guest, HyperBEAM loads as a proper HB application with
its standard wallet + node-message + hook machinery. On startup, a hook
calls the `~tpm@2.0a` device, which extends a TPM PCR with the hash of the
node message. Any consumer can request a signed attestation from the node;
the attestation's quote commits to that PCR, and therefore to the entire
node identity (wallet address, devices, config). A verifier can chain
through: signed AO-Core result → sender address → attestation's wallet
address → PCR 15 replay → TPM quote → EK cert chain → trusted TPM vendor.

## The three binding links the attestation must prove

1. **Hardware identity**. Signed by an EK whose cert chains to a trusted
   TPM vendor root (test CA for now, real vendor CA on physical deploy).
2. **Software identity (what's running)**. The TPM2_Quote commits to a
   PCR set including PCR 15, whose value = SHA-256 extension by the node
   message's `hb_message:id/3` output. Replay the event log ⇒ recompute
   PCR 15 ⇒ it matches the quote. Therefore this specific node message
   (containing the wallet identity, trusted_signers, device set, etc.)
   was the one running at attestation time.
3. **Result identity (this signed AO-Core result came from that node)**.
   The node's wallet signs AO-Core results. The wallet's address equals
   (or is derivable from) the wallet in the attested node message. Match.

## Acceptance criteria — unattended mode "done"

A single script `./scripts/boot-real.sh` on the user's laptop:

1. Builds (or reuses) the LapEE initramfs containing HyperBEAM `edge` with
   the `~tpm@2.0a` device preloaded.
2. Boots a real Linux kernel under QEMU with swtpm attached.
3. Inside the guest, HyperBEAM starts via its normal `hb_http_server`
   start flow — not a bespoke orchestrator. The `on.start` hook fires:
   `["~meta@1.0/info/extend~tpm@2.0a"]`, which resolves to a call to
   `dev_tpm2:extend/3` with the node message as subject. That extend
   call updates PCR 15 with `hb_message:id(NodeMsg, all, Opts)`.
4. A consumer (host-side script) sends an HTTP request to the running
   node for an attestation and receives a signed envelope.
5. The host-side verifier:
   - Replays the PCR 15 event chain, confirms it matches the quoted value
   - Recomputes `hb_message:id(NodeMsg, all, Opts)` from the envelope's
     embedded node message, confirms it matches the PCR 15 extension
   - Validates the TPM2_Quote signature under the AK public key
   - Validates the EK cert chain to the test TPM vendor root
   - Validates the node's wallet identity matches the attested wallet
     (by having the node sign a separately-provided challenge)
6. A negative test: modify the node message (e.g. flip a trusted_signers
   entry) → PCR 15 changes → verifier rejects.

## Assumptions baked in during unattended work

Written here so they're legible, not laundered:

- **Branch strategy.** The user's main work is on `feat/remote-device-load`,
  but asked me to base on `edge`. I'll work on a new branch `agent/lapee-dev-tpm2`
  off `edge`, in a fresh worktree at `.claude/worktrees/lapee-dev-tpm2/`.
  My current worktree (`sharp-lichterman`) keeps the paper + lapee-baremetal
  artifacts. I'll cross-reference between the two.
- **Device name.** `~tpm2@2.0a` — matching paper's `~lua@5.3a` naming
  convention. Specification = TPM 2.0; letter suffix = AO-Core device revision.
- **NIF keeps its current structure** in `lapee-baremetal/lapee-tpm/`. The
  HB device `src/dev_tpm2.erl` wraps it. No second NIF.
- **Cmdline merge strategy.** Following the user's suggested approach:
  enhance `hb_opts` (and/or `hb_http_server`) to accept a comma-separated
  `HB_CONFIG=a,b,c` and deep-merge with rightmost precedence. Init script
  writes the cmdline-sourced JSON to one file, the image ships another
  file with the enforced `on.start` hook, and the enforced one wins.
- **No base64 on the cmdline.** JSON is printable; just URL-ish-escape the
  cmdline (spaces → `_20_` or similar) or use a separate kernel param. I'll
  pick the simpler of the two when I get there.
- **Wallet address = node identity.** HB generates the wallet on first run
  at `priv_wallet_location` (ar_wallet format). The `on.start` extend is
  over the whole node message, so the wallet address is covered
  transitively via `hb_message:id(NodeMsg, all, Opts)`.
- **Test CA still stands in for TPM vendor CA.** Swapping to a real TPM's
  EK cert chain is a physical-hardware step; keep the test CA for this run.

## Rough milestone ordering (not a rigid schedule)

1. Write this file. (Done.)
2. Dispatch Buildroot sub-agent with a clear brief + acceptance test.
3. Create new worktree off `edge`; scaffold `src/dev_tpm2.erl` and NIF wiring.
4. Enhance `hb_opts` / `hb_http_server` for multi-file `HB_CONFIG`.
5. Write the enforced node-message JSON (the `on.start` hook).
6. Slim `/init` to: mount + cmdline parse + exec.
7. Rebuild initramfs with HyperBEAM (not lapee_node) as PID 2.
8. Boot, iterate until attestation envelope returned from a real HTTP request.
9. Host-side verifier update to match new envelope shape.
10. Negative test: flip a field, confirm rejection.
11. Integrate Buildroot sub-agent result when available.
12. Commit progressively; final commit summarizes + points at this plan.

## What "stop" looks like (from user's instruction)

Unattended mode persists until either:
- All acceptance criteria above are met, or
- The user explicitly interrupts, or
- A genuinely-blocking ambiguity arises where proceeding would materially
  commit to something I can't justify without input. In that case, STATUS
  gets a note and I continue on whatever parallel workstream IS unblocked.
