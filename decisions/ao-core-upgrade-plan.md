# AO-Core Upgrade Plan

## Operating Rule

Reread these files before each implementation batch and before each commit:

- `decisions/ao-core-upgrade-north-star.md`
- `decisions/ao-core-upgrade-plan.md`
- `MEMORY.md`

If a change does not follow from those notes, stop and write a decision note
before implementing it.

## Morning Reset: Clean Semantics First

The previous `COMPLETE:` checkpoint is no longer review-complete. It proved
that the branch could be made green, but review found reward-hack style changes
that weakened the model:

- `HB_PARANOID` cache topics skipped materialized child verification.
- Linked committed keys caused whole commitments to be deferred.
- Missing-secret HMAC commitments silently passed generic cache paranoia.
- Several devices marked results private `no-store` to avoid cache writes
  without proving that the result was truly private or time-bound.
- One bundler integration test was narrowed to a direct device call.
- HTTP signed input could fail verification while still being accepted if the
  node was not forcing signed requests.

The new order of work is:

1. Restore clean model semantics and remove reward hacks.
2. Pass `rebar3 eunit-all` without `HB_PARANOID`.
3. Re-enable `HB_PARANOID=cache_read,cache_write` as a detector and fix the
   real failures it exposes.
4. Only then claim completion.

## Morning Operating Alignment

The current pass is model-repair first, not feature expansion. The sequence is:

1. Core components must be protocol-aligned before chasing device failures.
2. Then get the `~process@1.0` and scheduler/process-oriented tests clean.
3. Then work outward through the wider preloaded device suite.

Do not let a later device failure justify weakening an earlier core invariant.
If a device exposes a model mismatch, fix the model or the device contract
directly. Do not hide the failure behind cache-control, broad vary specs,
special test paths, or reduced verification.

Three morning clarifications are now hard constraints:

- `{as, Device, Msg}` is migration debt. The target model is ordinary message
  overlay/extension with `#{ <<"device">> => Device, ... }`. A path-bearing
  part such as `key~device@1.0` should mean: compose the device, then resolve
  path `key`.
- `priv` is local execution state and must be carried forward when extending
  one message with another. It must not leak into public IDs, serialized public
  surfaces, or cache commitments. Non-message results such as binaries, lists,
  and scalars do not carry message `priv`.
- Routine loaded-message commitment normalization should be stripped back, not
  expanded. Unsigned IDs belong to cache/link addressing, not as synthetic
  commitments attached to mutable Erlang maps.

`HB_PARANOID=cache_read,cache_write` is not a mode in which features may be
disabled. It is the detector for the same cache-poisoning and commitment
breakage that would otherwise reach production. An unloaded link target need
not be recursively verified by default, but materialized children and the
current committed surface must be verified honestly.

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
- Remove the legacy protocol-facing `{as, Device, Msg}` model. Device changes
  are ordinary extension/overlay of `#{ <<"device">> => Device }`; path-bearing
  device segments first compose the device key and then resolve the requested
  path.

## Phase 1b: Overlay

- Cache the varied execution result, not the caller-specific final message.
- On cache hit or fresh compute, apply the result over the current caller's
  non-varied base when the return spec marks overlay intent.
- Prefer existing `set` semantics for phase 1.
- Avoid deep structural extension mechanics until message extension unless
  coherence forces it. If forced, isolate in labeled commits.
- Preserve `priv` when extending one message with another. Do not carry `priv`
  through non-message values such as binaries, lists, or scalars. Never include
  `priv` in public cache IDs or serialized public message surfaces.

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
- Strip routine loaded-message commitment normalization from the model.
  Unsigned IDs should live in cache/link addressing, not as synthetic
  commitments on ordinary Erlang maps.

## Phase 2b: Cache Expiry And Dynamic Results

- Revert private `no-store` markings that were added only to make tests pass.
- Keep `no-store` only for genuinely private, nondeterministic, or time-local
  outputs.
- If a result is deterministic but time-bounded, support or honor `max-age` on
  cache hits/reads instead of disabling cache writes.

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
- Do not satisfy tests by weakening `HB_PARANOID`, skipping commitments, hiding
  cache writes behind private `no-store`, or narrowing integration tests to
  avoid the path that failed.

## Acceptance

- First clean gate:

  ```sh
  rebar3 eunit-all
  ```

- The overnight pass is not complete until the clean gate passes and then this
  command passes:

  ```sh
  HB_PARANOID=cache_read,cache_write rebar3 eunit-all
  ```

- The passing state must preserve a clean, minimal patchset. If a fix increases
  scope, write a decision note and prefer the smaller reversible path.
