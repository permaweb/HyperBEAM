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
