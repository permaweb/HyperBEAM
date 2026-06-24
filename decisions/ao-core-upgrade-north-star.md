# AO-Core Upgrade North Star

## Scope

This note records the intended protocol shape for the clean AO-Core
types/varying pass and the follow-on message extension pass. It is the
reference for keeping implementation choices small, explicit, and aligned with
the model rather than the earlier prototype branches.

## Fundamentals

- Every AO-Core message has device semantics. If a message does not explicitly
  name a device, its device is `message@1.0`.
- Resolution is device application over messages:
  `AO(Base, Request) -> Result`.
- A request with a `path` invokes the selected device key.
- A request without a `path` is treated as extension/composition of the request
  message upon the base message.
- Results are messages or literals. There is no separate "result object" in the
  protocol model.
- Device casting is ordinary message composition. The legacy tuple form
  `{as, Device, Msg}` should be removed from the protocol-facing model and
  replaced by extending/overlaying `Msg` with `#{ <<"device">> => Device }`
  and any other local keys. A path-bearing device path segment such as
  `key~device@1.0` should mean: first compose the current message with the
  device key, then resolve the request path `key`.
- There should be no second "cast" or "as" execution universe. If a request
  without a path extends, changing the device is just one instance of extension.

## Message Extension

- `...` denotes inherited message content.
- Parent keys supersede inherited keys:
  - `{ a = 2, ... = { a = 1 } }/a => 2`
  - `{ b = 2, ... = { a = 1 } }/a => 1`
- Ordinary device calls should receive the flattened, varied view unless the
  function spec explicitly asks to see extension structure.
- `...+link` is a cache/TABM boundary concern, not structured-message semantics.
- Private state is carried forward when a message extends another message. The
  `priv` element belongs to local execution state, not the public message ID,
  but extension should not accidentally drop it. When the new base is not a
  message value, for example a list, binary, or scalar key result, there is no
  message `priv` to carry.
- `hb_message:with_only_signed/2` should supersede signed-subset call sites
  that currently use `with_only_committed/2`. It should walk the `...` chain
  from newest parent to oldest ancestor until it finds the first message with a
  commitment containing a signature, then return only that signed committed
  subset. It should be cheap and should not verify the signature itself.
- If no signed ancestor is found, `with_only_signed/2` should preserve the
  current message rather than fabricating a signed subset.
- `hb_singleton:from/2` should use message extension when it derives execution
  steps from a signed inbound message. The inbound signed message must remain
  recoverable while each path segment gets its own request shape.

## Types And Varying

- Before executing a path, AO-Core resolves the actual device function.
- That function's spec is protocol surface: it defines the consumed base and
  request shape.
- Varying loads and coerces only the keys declared by the spec.
- `_` means empty projection: vary on no user keys, aside from the implicit
  `device` key on base and `path` key on request.
- `any()` means opaque pass-through: leave the input unchanged, do not project
  it, and do not force-load it. This is an escape hatch, not the default shape.
- `#{ Key := _ }` requires `Key` and force-loads/materializes it.
- `#{ Key => _ }` accepts optional `Key` and force-loads/materializes it when
  present.
- `#{ A := _, B => _ }` varies only on required `A`, optional `B`, and the
  implicit device/path key.
- `#{ A := _, B => _, _ => _ }` carries all remaining visible keys forward
  lazily/accessibly, while only force-loading/materializing `A` and present `B`.
- `#{ A := _, B => _, _ := _ }` carries all remaining visible keys forward and
  force-loads/materializes them too.
- Return specs use explicit extension intent, not wildcard intent:
  `#{ '...' => base }` or `#{ '...' => request }`.
- A future type form `signed(Schema)` may vary an input down to the first
  signed subset found by `hb_message:with_only_signed/2`, then apply `Schema`.
  This is for scheduler, bundle upload, and similar flows that need to build on
  messages while preserving a signed core.
- Cache lookup, persistent grouping, validation, execution, hashpath generation,
  and cache write all operate on the varied base/request pair.
- Cheap direct `ID/key` reads remain valid when they are semantically direct
  `message@1.0` member reads.

## Overlay Results

- A device may return a complete message or an overlay/patch over the varied
  base.
- In the first pass, overlay means deeply applying returned keys over the varied
  base using the existing message set semantics.
- After message extension lands, overlay should become structural extension
  rather than eager expansion.
- Nested returned messages should extend the corresponding nested base message,
  not replace it wholesale. For example, returning:

  ```erlang
  #{ <<"a">> => #{ <<"b">> => 2 } }
  ```

  over:

  ```erlang
  #{ <<"a">> => #{ <<"a">> => 1 } }
  ```

  should represent the merged meaning:

  ```erlang
  #{
      <<"a">> => #{
          <<"b">> => 2,
          <<"...">> => #{ <<"a">> => 1 }
      },
      <<"...">> => #{ <<"a">> => #{ <<"a">> => 1 } }
  }
  ```

- If nested structural overlay is unavoidable before the morning review, isolate
  it in a clearly labeled commit before and after the work so it can be assessed
  atomically.

## Cache Semantics

- The cache records that `VariedBase` plus `VariedReq` begets the execution
  result returned by the device.
- If the result is an overlay, cache the overlay/result of the varied execution,
  not the final message after applying it to the caller's unvaried base.
- On both a cache hit and a fresh computation, apply the cached/computed result
  over the non-varied base for the current computation:

  ```text
  Base/Req -> VariedBase/VariedReq -> Result -> Base...Result
  ```

- This lets all inputs that vary to the same base/request share one execution
  while still producing caller-specific extended results.
- Unvaried and varied inputs must not be conflated.
- Loaded messages should not be routinely annotated with unsigned commitment
  IDs by `normalize_commitments/2`. Unsigned IDs and cache-addressing facts
  should be cache/link structure, not ordinary Erlang map state. This reduces
  cache-poisoning risk from normal map operations such as `maps:put/3` or
  `Msg#{ Key => Value }` against a message that merely carries an unsigned ID.
- `with_only_committed/2` remains a signed/committed-subset helper where truly
  needed, but normal cache operation should offload data and use links rather
  than deepening loaded messages with synthetic unsigned commitments.
- Cache expiration is the honest answer for time-bound data. Do not use private
  `no-store` merely to avoid cache/paranoid failures. If a result is reusable
  for a bounded interval, implement or honor `max-age` on cache reads rather
  than disabling storage. Use `no-store` only for genuinely private,
  non-deterministic, or time-local results.

## Hashpaths

- A hashpath is both a terse expression of a message and an atomically
  challengeable attestation to the results needed to produce it.
- Draft equivalences:
  - `ID1/ID2=ID3`: given `ID1`, applying `ID2` yields the same as extending
    `ID1` with `ID3`.
  - `ID1/ID2.ID3`: given `ID1`, applying `ID2` yields `ID3`.
- After extension-aware hashpaths, append the fully varied final result as the
  bookend element. Example:

  ```text
  GET /BaseID/ReqID/Req2

  Hashpath:
  VariedBaseID/VariedReq1=Res1/VariedReq2=Res2/UnsignedIDOfRes2OnRes1OnVariedBaseID
  ```

- The bookend should not be necessary for verification, but ties the HTTP
  response to the unsigned ID of the fully varied terminating message when the
  result itself lacks a direct commitment.

## HTTP Commitments

- The unsigned ID commits to the full known key/value set of the terminating
  message.
- The HTTP signature commits to that message response and the execution
  hashpath, binding "what is known" to "how it was reached."
- Inbound signed HTTP messages that fail verification should be rejected with a
  client error, not merely accepted without caching. If a message presents
  commitments as signed input, the node must either verify them or reject them.
- `store-all-signed` must not store unverified signed wire messages. This is a
  consequence of the previous point, not a separate cache-only policy.

## Paranoid Verification

- `HB_PARANOID=cache_read,cache_write` is a detector for production cache
  poisoning and commitment breakage. It must never be satisfied by disabling
  features, skipping commitments, weakening assertions, broadening specs, or
  marking otherwise-cacheable results as `no-store`.
- It is acceptable not to recursively verify unloaded links while checking the
  current message. A link is a normal cache resource boundary unless a bundle
  or explicit recursive-verification mode asks to verify the full subtree.
- It is not acceptable to skip verification of the current message's committed
  surface because one committed value is linked. Verify the exact committed
  link representation, load the value if the committed surface requires it, or
  fail loudly.
- Materialized nested messages that are present in the current message should be
  verified when paranoia is asked to recurse into present children. Only
  unloaded links are deferred to the later read that materializes them.
- Secret HMAC commitments must not silently pass in generic cache paranoia. If
  a secret is required for verification, the verifier should obtain it from the
  intended private/opts path or report that the commitment is unverifiable in
  that context. Silent success is a reward hack.

## Implementation Guardrails

- Start from latest `hyperbeam-main/edge` on a fresh branch.
- Keep phase 1 focused on types, varying, canonical cache/execution inputs, and
  basic overlay application.
- After precise varying is in place, simplify devices aggressively but only
  where their specs now guarantee normal loaded Erlang values. Correctly varied
  message inputs may be treated as ordinary Erlang maps at the device boundary;
  device internals should not keep defensive link-handling ceremony that the
  resolver now owns.
- Try to avoid implementing the full deep structural extension mechanics in
  phase 1. If they are unavoidable, isolate them in clearly labeled commits.
- Defer full message extension and extension-aware hashpath changes until the
  second layer unless they are required to make phase 1 coherent.
- Avoid carrying forward broad cache rewrites, result/message nomenclature
  drift, archive schema side indexes, generic "vary on everything" specs, and
  large unrelated device churn.
- Before claiming paranoid acceptance, the ordinary suite must first pass with
  clean semantics and no known reward hacks:

  ```sh
  rebar3 eunit-all
  ```

- Absolute success marker: every core and preloaded device test must pass with
  paranoid cache checking enabled:

  ```sh
  HB_PARANOID=cache_read,cache_write rebar3 eunit-all
  ```

- This must be achieved with the smallest clean patch that expresses the
  protocol. Passing tests by adding broad workarounds, relaxed assertions, or
  generic vary-on-everything specs is not success.
