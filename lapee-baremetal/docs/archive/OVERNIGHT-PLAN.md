> **ARCHIVED.** This document describes a historical phase of
> LapEE and is preserved for continuity. It does NOT describe
> the current architecture -- see
> [`../../STATUS.md`](../../STATUS.md) and
> [`../../README.md`](../../README.md) for the v1.2
> Framework-boot path that is the current reference.

# Overnight pass -- TPM2 interpreter, world-class coverage

**Start:** 2026-04-20 ~18:00 EDT
**Brief:** Sam's directive -- produce *the* most comprehensive public
TPM2 event-log interpretation library. No time pressure, no scope
cuts, no early returns. Ship when further improvement is genuinely
diminishing.

**2026-04-22 -- v1.0 Framework bare-metal boot PASSED.** The LapEE
USB image booted successfully on Sam's Framework 13 (Insyde H2O
IFR30.03.04, AMD fTPM), produced a real signed quote, and parsed
end-to-end on the verifier. See
[STATUS.md -> v1.0 Framework bookend](STATUS.md#v10-framework-bare-metal-bookend-2026-04-22).
The parser-improvement loop described below is now the primary
workstream; each overnight iteration closes null fields observed
on real hardware.

**Acceptance**
1. Library exceeds every existing public TPM interpretation parser
   (tpm2-tools, go-eventlog, TSS.MSR, keylime, fwupd) by a
   significant margin. A `COMPARISON.md` table backs the claim.
2. On real TPM2 hardware: decode every field of every PCR and
   extension for ≥95% of devices. Verification: test vectors pulled
   from public sources (Option B).

## Phases (in flight / parallel)

### Phase 1 — reconnaissance (sub-agents, parallel)
- 1a. Public test vector hunt
- 1b. Competitor parser survey
- 1c. TCG spec exhaustive review
- 1d. UEFI spec review
- 1e. Vendor docs + EK cert patterns

### Phase 2 — decoders (Erlang, fixture-tested)
Every TCG event type (38 codes) + UEFI structures + vendor-specific
formats.

### Phase 3 — test vector corpus
Embed public samples under `priv/tpm-interpret/fixtures/`; run
parser against every one.

### Phase 4 — data expansion
Full TCG Vendor ID Registry; 15+ firmware-version families; UKI
measurement DB seeded.

### Phase 5 — competitor comparison
Line-by-line feature matrix.

### Phase 6 — documentation
`COVERAGE.md` rewritten as authoritative reference with citations.

### Phase 7 — final audit
Dashboard refresh, STATUS update, final commit.

## Progress log

**T+0** — setup, plan, kick off parallel sub-agents.
