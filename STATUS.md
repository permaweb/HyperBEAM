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
