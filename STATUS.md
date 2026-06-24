# AO-Core Vary Clean Status

## Baseline

- Worktree: `/Users/sam/.codex/worktrees/ao-core-vary-clean/hyperbeam`
- Branch: `feat/ao-core-vary-clean`
- Base: `hyperbeam-main/edge` at `6c9a0c97640e8991ea9c40210e49fe1aa06a4636`
- Mode: overnight unattended
- Acceptance: `HB_PARANOID=cache_read,cache_write rebar3 eunit-all`

## Log

- Created baseline notes and plan. No code changes yet.
- Added standalone `hb_types` draft for Dialyzer spec extraction and input
  varying. `rebar3 compile` passed.
- Fixed atom/key normalization in the type parser. `rebar3 eunit
  --module=hb_types` passed with 3 tests.
- Wired varying into `hb_ao` before non-direct cache lookup/execution. First
  `hb_ao_test_vectors` run exposed repeated abstract-code extraction timeouts.
- Added process-local type schema memoization, documented in
  `decisions/type-schema-memoization.md`. `rebar3 compile && rebar3 eunit
  --module=hb_ao_test_vectors` passed with 189 tests, including projection and
  cache-collapse coverage for varied inputs. `rebar3 eunit --module=hb_types`
  passed with 3 tests.
- Added `hb_message:with_only_signed/2`. Narrowed validation:
  `rebar3 device test --module hb_codec_test_vectors --test
  hb_codec_test_vectors:with_only_signed_walks_extension_test+with_only_signed_preserves_unsigned_test`
  passed with 2 tests.
- Added overlay cache coverage for varied executions. A focused failing vector
  showed map compute results were written by message ID but not linked at the
  execution hashpath; switched map result writes to existing
  `hb_cache:write_hashpath/2`. Validation:
  `rebar3 eunit --test hb_ao_test_vectors:vary_overlay_cache_applies_to_each_original_test`
  passed; `rebar3 eunit --module=hb_ao_test_vectors` passed with 191 tests;
  `rebar3 eunit --module=hb_types` passed with 3 tests; `git diff --check`
  passed. A parallel `hb_types`/AO-vector run hit the shared default HTTP port
  with `eaddrinuse`; the same `hb_types` command passed when rerun alone.
- Added signed singleton ancestry preservation. `hb_singleton:from/2` now
  strips direct commitments from path-derived child steps and attaches the
  original signed subset through `...`; `hb_message:with_only_signed/2` can
  recover the signed parent from a modified step. Validation:
  `rebar3 eunit --module=hb_singleton` passed with 37 tests;
  `rebar3 device test --module hb_codec_test_vectors --test
  hb_codec_test_vectors:with_only_signed_walks_extension_test+with_only_signed_preserves_unsigned_test`
  passed with 2 tests; `rebar3 eunit --module=hb_ao_test_vectors` passed with
  191 tests; `git diff --check` passed.
- Added the first precise preloaded device spec batch, targeting request/base
  keys actually consumed by process, scheduler, router/hooks, cookie, codec,
  delegated-compute, and arweave call boundaries. A process-device run exposed
  one real singleton-extension consequence: scheduled messages could now carry
  their signed process as a `...` ancestor while the scheduler/process identity
  path still used direct-only committed subsets. Migrated only those scheduling
  and process-ID call sites to `hb_message:with_only_signed/2`. Validation:
  `rebar3 compile` passed; `git diff --check` passed; `rebar3 eunit
  --module=hb_types` passed with 3 tests; `rebar3 eunit
  --module=hb_ao_test_vectors` passed with 191 tests; `rebar3 device test
  --module hb_process_test_vectors` passed with 17 tests; `rebar3 device test
  --module hb_codec_test_vectors --test
  hb_codec_test_vectors:with_only_signed_walks_extension_test+with_only_signed_preserves_unsigned_test`
  passed with 2 tests.
- Started the full paranoid suite. It exposed three concrete failure families:
  varying tried to inspect raw binary literals such as WASM module bodies;
  payment devices looked for direct request signers after singleton ancestry
  moved signatures into `...`; and cache-write paranoia verified linkified or
  HTTP-enveloped signed messages without first materializing their committed
  subset. Kept the fixes narrow: non-message base/request pairs use the
  existing cache path instead of type varying; payment admission/balance paths
  use `hb_message:with_only_signed/2`; paranoid verification decodes TABM link
  keys, loads values, recurses over uncommitted nested content, and verifies
  the materialized committed subset. Validation: `rebar3 compile` passed;
  `git diff --check` passed; `HB_PARANOID=cache_read,cache_write rebar3 eunit
  --module=hb_examples` passed with 6 tests; `HB_PARANOID=cache_read,cache_write
  rebar3 eunit --module=hb_ao_test_vectors` passed with 191 tests;
  `HB_PARANOID=cache_read,cache_write rebar3 device test --module
  hb_process_test_vectors` passed with 17 tests; `HB_PARANOID=cache_read,cache_write
  rebar3 device test --module dev_httpsig` passed with 5 tests. One attempted
  parallel focused run hit the shared default listener with `eaddrinuse`; it
  passed when rerun through the device-test wrapper alone.
- Tightened the first extension-aware read semantics. Added shared
  `hb_message:visible/2` and `hb_message:find_visible/3` child-wins helpers,
  used them for type projection and `message@1.0` member reads, and kept
  `message@1.0` tolerant of missing inherited parent links when only direct
  child keys are requested. Validation: `rebar3 compile` passed; `git diff
  --check` passed; `rebar3 eunit --module=hb_types` passed with 5 tests;
  `HB_PARANOID=cache_read,cache_write rebar3 device test --module dev_message`
  passed with 18 tests; `HB_PARANOID=cache_read,cache_write rebar3 device test
  --module dev_auth_hook` passed with 4 tests.
- Fixed three paranoid-cache failures without widening vary specs: Arweave
  uploads now strip back to the first signed target with
  `hb_message:with_only_signed/2`; manifest intermediate/fallback route maps
  use private `no-store`; paranoid verification checks committed subsets and
  already-present children without force-loading unrelated lazy links; ANS-104
  verification preserves the existing configured GraphQL-trust path when the
  request carries trust markers. Decisions recorded in
  `decisions/paranoid-lazy-extension-verification.md`,
  `decisions/manifest-private-no-store.md`, and
  `decisions/ans104-trusted-gql-paranoid.md`. Validation:
  `HB_PARANOID=cache_read,cache_write rebar3 device test --module dev_manifest`
  passed with 6 tests; `HB_PARANOID=cache_read,cache_write rebar3 device test
  --module dev_b32_name` passed with 9 tests;
  `HB_PARANOID=cache_read,cache_write rebar3 eunit --module=hb_store_gateway`
  passed with 10 tests; `HB_PARANOID=cache_read,cache_write rebar3 device test
  --module dev_arweave` passed with 39 tests;
  `HB_PARANOID=cache_read,cache_write rebar3 device test --module dev_ans104`
  passed with 24 tests; `HB_PARANOID=cache_read,cache_write rebar3 eunit
  --module=hb_ao_test_vectors` passed with 191 tests.
- Closed the next paranoid failure cluster with narrowly scoped semantics:
  wildcard carry in `hb_types` now preserves unmatched direct keys and the lazy
  `...` parent without force-enumerating inherited keys; relay now prefers the
  current explicit `relay-path` over an inherited target path; cache-writer,
  meta, and location authorization paths strip back to signed ancestors before
  inspecting signers; `cron@1.0` and mutable `meta@1.0/info` responses set
  private `no-store`; and the invalid bundler item test now exercises the
  device contract directly so cache-write paranoia is not asked to persist an
  intentionally invalid signed body. Decision recorded in
  `decisions/bundler-invalid-item-paranoid.md`. Validation:
  `rebar3 eunit --module=hb_types` passed with 5 tests;
  `HB_PARANOID=cache_read,cache_write rebar3 eunit --module=hb_examples`
  passed with 6 tests; `HB_PARANOID=cache_read,cache_write rebar3 device test
  --module dev_bundler` passed with 27 tests;
  `HB_PARANOID=cache_read,cache_write rebar3 device test --module dev_cache`
  passed with 2 tests; `HB_PARANOID=cache_read,cache_write rebar3 device test
  --module dev_cron` passed with 4 tests;
  `HB_PARANOID=cache_read,cache_write rebar3 device test --module
  dev_local_name` passed with 5 tests;
  `HB_PARANOID=cache_read,cache_write rebar3 device test --module dev_location`
  passed with 3 tests; `HB_PARANOID=cache_read,cache_write rebar3 device test
  --module dev_meta` passed with 11 tests; `rebar3 compile` passed; `git diff
  --check` passed.
