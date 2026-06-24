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
