# Morning Model Reset

## Prompt As Understood

The branch reached a full paranoid green checkpoint, but review identified that
some of the green state came from reward-hack style changes. The goal is not to
preserve that checkpoint. The goal is a small, model-aligned branch that first
passes the ordinary suite cleanly, then passes paranoid cache verification
without weakening what paranoid mode is meant to detect.

## Issues

- `HB_PARANOID=cache_read,cache_write` must detect cache poisoning and broken
  commitments before they become production failures.
- The current branch skips too much in `hb_message:paranoid_verify/3`:
  materialized children, linked committed-key commitments, and missing-secret
  HMAC commitments.
- Several devices added private `no-store` while chasing paranoid failures.
  That is valid only for truly private, nondeterministic, or time-local data.
- HTTP signed input should be verified or rejected. Accepting failed signed
  input while merely not caching it is not the intended contract.
- GraphQL gateway reconstruction can fetch useful remote data without being a
  cryptographic verifier. If a reconstructed ANS-104 item fails
  `ar_bundles:verify_item/1`, return uncommitted fetched fields rather than a
  signed-looking message with broken or `trusted-keys` commitments.
- The old `{as, Device, Msg}` tuple is not aligned with message extension:
  device changes should be ordinary message composition.
- Loaded-message commitment normalization increases cache-poisoning risk by
  attaching unsigned ID facts to ordinary mutable Erlang maps.

## Decision

Reset the implementation target:

1. Remove reward hacks and restore model-aligned core behavior.
2. Pass `rebar3 eunit-all` without `HB_PARANOID`.
3. Re-enable `HB_PARANOID=cache_read,cache_write` and fix the actual failures.
4. Only claim completion when both gates are green.

Do not add private `no-store`, broaden specs, weaken tests, skip commitments,
or reduce integration coverage to get green.

## Execution Order

- Core first: `hb_message`, `hb_http`, extension/overlay, `{as}` cleanup, and
  commitment normalization boundaries.
- Then `~process@1.0` and scheduler process tests.
- Then wider devices.

## Non-Negotiables For This Pass

- Ordinary `rebar3 eunit-all` must pass first without known test-hacking
  residue.
- `HB_PARANOID=cache_read,cache_write rebar3 eunit-all` is the final detector,
  not a feature-reduced mode.
- `no-store` requires a real model reason: private, nondeterministic,
  time-local, or local node policy outside the cache key.
- Signed inbound HTTP messages verify or reject.
- Unloaded links may remain unloaded; materialized children and the current
  committed surface may not be skipped.
- Missing-secret HMAC verification in generic cache context must not silently
  succeed.
- The clean implementation should get simpler as varying becomes precise. If a
  device became more complex, inspect whether the resolver boundary is doing
  too little or the device spec is too broad.
- A local cache link to gateway-fetched data is an addressing fact, not an
  ANS-104 signature. Do not use `no-store` or paranoid bypasses to hide broken
  gateway commitments; strip the false commitment and keep the data model
  honest.
