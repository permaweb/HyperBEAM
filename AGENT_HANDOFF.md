# Agent Handoff: Vary Extension Progress

## Branch

Current branch:

```sh
codex/vary-extension-progress-20260605
```

This branch was created from the dirty working state on `impr/vary-extension`
on 2026-06-05.

## Commander's Intent

Continue the message-extension commitment work without falling back to the old
`path`/singleton "do not sign this key" approach.

Hard constraints:

- Do not add cache-write stripping or paranoid verification bypasses.
- Do not reintroduce `priv/original-request`.
- Do not make scheduling wait for upload completion.
- Validate meaningful fixes with strict cache-write verification:

```sh
HB_PARANOID=cache_write HB_STRIP_INVALID_CACHE_COMMITMENTS=false
```

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

This currently fails:

```sh
HB_PARANOID=cache_write HB_STRIP_INVALID_CACHE_COMMITMENTS=false \
  rebar3 as genesis_wasm eunit --module=hb_ao_test_vectors
```

The failing cases are the singleton rewritten commitment vectors, repeated
across option suites:

- `singleton rewritten path commitments`
- `singleton rewritten query commitments`

The failure is currently internal, before the semantic assertions:

```erlang
**error:{badmatch,[...]}
```

at `hb_message:do_normalize_commitments/3`, where verify mode assumes:

```erlang
[NormID] = hb_maps:keys(NormCommitments, Opts)
```

That assumption is false after singleton anchoring because the message can carry
multiple unsigned commitments.

## Likely Next Move

Start in `src/core/resolver/hb_message.erl`, around
`do_normalize_commitments(Msg, Opts, verify)`.

The minimal conceptual fix is likely to make verify-mode normalization handle
multiple unsigned commitments deterministically instead of pattern matching on a
single normalized commitment ID. Be careful: this function is security-critical.
The right behavior should preserve valid unrelated commitments and replace only
the stale unsigned commitment view that no longer matches the normalized
message.

Do not simply revert singleton anchoring unless you first prove that it is not
needed for the extension-commitment design. Reverting it may make the immediate
vector failure disappear, but it likely moves away from the intended "commit the
extension edge" architecture.

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

Only then broaden to the full strict suite and ao-toolkit checks.
