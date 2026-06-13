# Agent Handoff: Vary Extension Progress

## Branch

Current branch:

```sh
codex/continue-vary-extension-77aebf44
```

This branch continues from commit `77aebf44a`, which is also
`origin/codex/vary-extension-progress-20260605`.

## Commander's Intent

Continue the message-extension commitment work without falling back to the old
`path`/singleton "do not sign this key" approach.

Hard constraints:

- Do not add cache-write stripping or paranoid verification bypasses.
- Do not reintroduce `priv/original-request`.
- Do not make scheduling wait for upload completion.
- Device functions should not receive message-extension/link internals. Vary
  should flatten extensions and load links before downstream device calls; links
  should not reach downstream devices as links. Any downstream need for the
  original request/signature must be explicit in the device/spec.
- Treat raw links exactly like message-extension scaffolding at device
  boundaries: they should be resolved during vary, not handled ad hoc by
  downstream devices.
- Validate meaningful fixes with strict cache-write verification:

```sh
HB_PARANOID=cache_write HB_STRIP_INVALID_CACHE_COMMITMENTS=false
```

- Final HyperBEAM verification is `rebar3 as genesis_wasm eunit-all`, not just
  `rebar3 as genesis_wasm eunit`. Focused `eunit` runs are useful diagnostics,
  but do not mark the work complete from them.

## Current Direction

The intended architecture is:

- Mutations that derive messages should use AO setter semantics.
- Rewritten values should produce messages with honest commitments before cache
  writes.
- Message extensions (`"..."` / `"...+link"`) should let derived messages
  preserve their derivation while committing to the extension edge, rather than
  treating common keys such as `path` as special unsigned cases.

The current code is partway through that direction. It is not a finished,
green state.

## Latest Checkpoint

Stopped on 2026-06-12 at the user's request with the working tree in a
checkpointable state.

Most recent focused run:

```sh
HB_PARANOID=cache_write HB_STRIP_INVALID_CACHE_COMMITMENTS=false \
  rebar3 as genesis_wasm device test --with-core --module hb_codec_test_vectors
```

Result: `Failed: 6. Skipped: 0. Passed: 1948.`

The prior `hb_message:do_normalize_commitments/3` `badmatch` crash for signed
nested bundle vectors is fixed. Generic JSON/HTTPSig bundle nested signed paths
now pass. Remaining focused failures are Arweave-style signed bundle vectors:

- Default opts: `ans104@1.0 (bundle): Signed nested complex signed message`
- Default opts: `ans104@1.0 (bundle): Signed with inner signed`
- Default opts: `tx@1.0 (bundle): Signed nested complex signed message`
- Default opts: `tx@1.0 (bundle): Signed with inner signed`
- Ed25519 opts: `ans104@1.0 (bundle): Signed with inner signed`
- Ed25519 opts: `tx@1.0 (bundle): Signed with inner signed`

Implementation detail added in `src/core/resolver/hb_message.erl`:

- List-shaped maps with no commitment material are treated as list containers
  during commitment normalization, avoiding normalization crashes on values such
  as `[1,2,3]`.
- Recursive child commitment normalization is conditional:
  plain child maps are still normalized when the parent has no commitment
  material, but plain child maps are preserved when the parent is already
  committed or extension-backed. Children with their own commitment material are
  still normalized.

An `eunit-all` run was started with the required strict command:

```sh
HB_PARANOID=cache_write HB_STRIP_INVALID_CACHE_COMMITMENTS=false \
  rebar3 as genesis_wasm eunit-all
```

It was intentionally interrupted to honor the user's five-minute stopping
request, so it is not a valid complete full-suite result. The partial log
showed the previously failing `hb_ao_test_vectors` singleton rewritten
path/query commitment vectors passing, then later recorded failures including:

- `hb_http` wasm state/request tests returning HTTP 500 HTML failures.
- `ans104@1.0: bundle_commitment_test`.
- `httpsig@1.0: validate_large_message_from_http_test`.
- `location@1.0: register_scheduler_test`.
- `p4@1.0: hyper_token_ledger`.
- Scheduler failures with `**exit:terminating` after the interruption signal;
  treat those as suspect until reproduced in a non-interrupted run.

## Recently Confirmed Passing

These were confirmed in strict paranoid cache-write mode during the latest pass:

```sh
HB_PARANOID=cache_write HB_STRIP_INVALID_CACHE_COMMITMENTS=false \
  rebar3 as genesis_wasm eunit -t hb_http:send_large_signed_request_test
```

Result: passed.

```sh
HB_PARANOID=cache_write HB_STRIP_INVALID_CACHE_COMMITMENTS=false \
  rebar3 as genesis_wasm eunit --module=hb_http
```

Result: all 14 tests passed.

```sh
HB_PARANOID=cache_write HB_STRIP_INVALID_CACHE_COMMITMENTS=false \
  rebar3 as genesis_wasm eunit --module=hb_client_remote
```

Result: all 2 tests passed.

```sh
HB_PARANOID=cache_write HB_STRIP_INVALID_CACHE_COMMITMENTS=false \
  rebar3 as genesis_wasm eunit --module=hb_cache
```

Result: all 39 tests passed.

`rebar3 as genesis_wasm compile` also passed before this handoff file was
created.

## Current Known Failure

The current focused failure is in Arweave-style nested signed bundle handling
for `ans104@1.0` and `tx@1.0`, not in the earlier singleton rewritten path/query
commitment vectors.

The next move should start around the ans104/tx bundle codecs and their
interaction with `hb_message` commitment normalization. The core invariant
remains: downstream devices should not receive message-extension scaffolding or
links. `vary` should flatten extensions and load links before device calls.
If a downstream device truly needs original request/signature material, that
must be represented explicitly in the spec rather than inferred ad hoc.

## Important Recent Changes

Key changes in the dirty state that should be understood before editing:

- `hb_link:normalize/3` now normalizes extension children before link/offload,
  which fixed stale nested HMAC failures in
  `hb_http:send_large_signed_request_test`.
- `hb_cache:read_resolved/3` and `hb_device:do_is_direct_key_access/3` now use
  AO-aware `hb_maps:find(<<"path">>, Req, Opts)` so request paths inherited via
  message extensions are visible to direct reads.
- `hb_ao:finalize_result/8` and extension varied messages currently use verify
  normalization in some extension cases.
- `hb_singleton:from/2` now anchors parsed singleton messages back through AO
  setters and normalizes commitments afterward.
- Existing report artifacts were intentionally left untracked:
  `BRANCH_CHANGE_REPORT.md`, `LIGHT_CHANGE_REPORT.md`,
  `PERFORMANCE_COMPARISON.md`, and the pre-existing `STATUS.md`.

## Suggested Verification Loop

After the focused `hb_message` fix:

```sh
HB_PARANOID=cache_write HB_STRIP_INVALID_CACHE_COMMITMENTS=false \
  rebar3 as genesis_wasm eunit --module=hb_ao_test_vectors
```

Then rerun the regressions that were most recently fixed:

```sh
HB_PARANOID=cache_write HB_STRIP_INVALID_CACHE_COMMITMENTS=false \
  rebar3 as genesis_wasm eunit --module=hb_http

HB_PARANOID=cache_write HB_STRIP_INVALID_CACHE_COMMITMENTS=false \
  rebar3 as genesis_wasm eunit --module=hb_cache

HB_PARANOID=cache_write HB_STRIP_INVALID_CACHE_COMMITMENTS=false \
  rebar3 as genesis_wasm eunit --module=hb_client_remote
```

Only then broaden to the full strict suite and ao-toolkit checks:

```sh
HB_PARANOID=cache_write HB_STRIP_INVALID_CACHE_COMMITMENTS=false \
  rebar3 as genesis_wasm eunit-all
```
