# AO-Core Upgrade Plan

## Operating Rule

Reread these files before each implementation batch and before each commit:

- `decisions/ao-core-upgrade-north-star.md`
- `decisions/ao-core-upgrade-plan.md`
- `MEMORY.md`

If a change does not follow from those notes, stop and write a decision note
before implementing it.

## Branch Discipline

- Start a fresh branch from latest `hyperbeam-main/edge`.
- Treat current dirty detached worktree and prior feature branches only as
  reference material.
- Commit small green slices.
- Keep phase boundaries visible in commit history.
- Do not push or publish without explicit instruction.

## Phase 0: Baseline

- Record branch, base commit, and status in `STATUS.md`.
- Compile/test clean edge as far as practical before changes.
- Inspect current `hb_ao`, `hb_cache_control`, `hb_cache`, `hb_message`,
  `hb_singleton`, and relevant preloaded devices.
- Locate the smallest useful type/vary code from prior branches, but do not
  transplant broad cache rewrites or extension prototypes.

## Phase 1: Types And Varying

- Add a compact type extraction and varying module in the core area that fits
  current `edge` layout.
- Parse Dialyzer specs into schemas:
  - `_` means empty projection.
  - `any()` means opaque pass-through.
  - `#{ K := T }` means required key, loaded/materialized.
  - `#{ K => T }` means optional key, loaded/materialized if present.
  - `#{ _ => _ }` means carry unmatched keys forward lazily.
  - `#{ _ := _ }` means carry and force-load unmatched keys.
  - `#{ '...' => base | request }` marks overlay return intent.
- Resolve the actual device function before varying.
- Vary the base/request pair from that function's spec.
- Preserve direct `ID/key` reads where they are semantically direct
  `message@1.0` member reads.
- Use the varied pair for cache lookup, persistent grouping, validation,
  execution, hashpath generation, and cache write.

## Phase 1b: Overlay

- Cache the varied execution result, not the caller-specific final message.
- On cache hit or fresh compute, apply the result over the current caller's
  non-varied base when the return spec marks overlay intent.
- Prefer existing `set` semantics for phase 1.
- Avoid deep structural extension mechanics until message extension unless
  coherence forces it. If forced, isolate in labeled commits.

## Phase 1c: Signed Subsets

- Add `hb_message:with_only_signed/2`.
- The function walks `Msg`, then `Msg["..."]`, until it finds the first message
  with a commitment containing `signature`.
- It returns the committed subset of that signed message without verifying the
  signature.
- If no signed ancestor exists, it returns the original message.
- Update only call sites where the signed-subset semantics are necessary.
- Leave broader call-site migration to evidence, not enthusiasm.

## Phase 1d: Singleton Parser

- Adjust `hb_singleton:from/2` so path-derived request steps extend the inbound
  signed message instead of destructively rewriting it.
- Preserve the user's signed inbound message as an ancestor reachable through
  `...`.
- Keep the executable step list shape compatible with the resolver.

## Phase 1e: Mass Device Simplification

- Once precise specs and varying are real, simplify device internals that are
  only complex because they manually defended against lazy links or uncoerced
  input values.
- Treat correctly varied device inputs as normal Erlang maps at the function
  boundary.
- Prefer direct pattern matching and ordinary map access where the spec
  guarantees presence and type.
- Keep simplification radical in intent but disciplined in diff: remove obsolete
  ceremony, do not redesign unrelated device behavior.
- Use focused tests after each simplification batch, then the full paranoid
  suite before claiming completion.

## Phase 2: Message Extension

- Implement `...` as message inheritance in structured message semantics.
- Parent keys override inherited keys.
- Ordinary device inputs flatten through varying unless the function spec asks
  for `...`.
- Convert overlay from eager `set` to structural extension.
- Handle nested structural overlay carefully and atomically.

## Phase 3: Hashpaths

- Change hashpath syntax after message extension semantics are real.
- Represent execution and extension explicitly:
  - `ID1/ID2=ID3`
  - `ID1/ID2.ID3`
- Append the fully varied final result ID as the bookend element.
- Keep HTTP signatures tied to both the message response and the execution
  hashpath.

## Exclusions

- Do not revive `dev_process_cache`.
- Do not add broad `dev_green_zone` or `dev_snp` churn.
- Do not add `hb_device_archive` schema side indexes.
- Do not rename message members to edges.
- Do not carry forward large `hb_cache` rewrites unless a minimal failing test
  proves one exact edit is required.
- Do not add generic vary-on-everything specs to silence failures.

## Acceptance

- The overnight pass is not complete until this command passes:

  ```sh
  HB_PARANOID=cache_read,cache_write rebar3 eunit-all
  ```

- The passing state must preserve a clean, minimal patchset. If a fix increases
  scope, write a decision note and prefer the smaller reversible path.
