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
- The next full `HB_PARANOID=cache_read,cache_write rebar3 eunit-all` reached
  1207 passes, then timed out in `lua@5.3a:ao_core_sandbox_test`. Root cause:
  local HMAC commitments created while normalizing internal singleton messages
  were treated as signed parents, so the denied relay singleton carried a lazy
  `...` parent into subresolution and `message@1.0/set`/`keys` tried to walk it.
  Tightened signed-ancestor detection to require a committer as well as a
  signature, and made `message@1.0/set` apply only direct patch keys while
  leaving read-side `keys` extension-visible. Validation:
  `HB_PARANOID=cache_read,cache_write rebar3 device test --module dev_message`
  passed with 19 tests; `HB_PARANOID=cache_read,cache_write rebar3 device test
  --module hb_codec_test_vectors --test
  hb_codec_test_vectors:with_only_signed_walks_extension_test+with_only_signed_preserves_unsigned_test+with_only_signed_ignores_hmac_commitments_test`
  passed with 3 tests; `HB_PARANOID=cache_read,cache_write rebar3 device test
  --module dev_lua --test dev_lua:ao_core_sandbox_test` passed; and
  `HB_PARANOID=cache_read,cache_write rebar3 device test --module dev_lua`
  passed with 17 tests.
- Cleaned up the pushed-process/scheduler cache boundary after the `dev_push`
  probe pass. Removed temporary diagnostics; kept the protocol fixes narrow:
  no-path `{as, Device, Msg}` now means "treat this concrete message as
  `Device` and apply the outer request"; scheduler dynamic status/slot
  responses mark private `no-store` so stale slot reads are not cached under
  caller `always` opts; AOS scheduler-cache reads load only assignment `body`;
  and `json-iface@1.0` relies on its precise compute spec instead of eager
  whole-message loading. The no-path `as` shortcut deliberately excludes
  `message@1.0`, preserving the established message-view subresolution used by
  AO core test vectors. Decision recorded in
  `decisions/pushed-process-cache-semantics.md`. Validation: `rebar3 compile`
  passed; `git diff --check` passed; `HB_PARANOID=cache_read,cache_write
  rebar3 device test --module dev_message` passed with 20 tests;
  `HB_PARANOID=cache_read,cache_write rebar3 device test --module dev_relay`
  passed with 3 tests; `HB_PARANOID=cache_read,cache_write rebar3 eunit
  --module=hb_ao_test_vectors` passed with 191 tests; and
  `HB_PARANOID=cache_read,cache_write rebar3 device test --module dev_push`
  passed with 10 tests.
- Closed the next scheduler/secret cluster from the full paranoid suite.
  Scheduler POST responses now carry the existing private `no-store` marker so
  repeated identical schedule POSTs execute instead of collapsing to one cached
  assignment; `schedule/3` now carries the base process shape it actually
  consumes; `dev_scheduler_cache` only body-loads committed `ao.N.1`
  assignments and otherwise loads the full local assignment; and `secret@1.0`
  strips unsigned response commitments before attaching generated wallet bodies
  while checking controllers against the signed ancestor. Validation:
  `git diff --check` passed; `HB_PARANOID=cache_read,cache_write rebar3 device
  test --module dev_scheduler` passed with 18 tests;
  `HB_PARANOID=cache_read,cache_write rebar3 device test --module
  dev_scheduler_cache` passed with 8 tests; and
  `HB_PARANOID=cache_read,cache_write rebar3 device test --module dev_secret`
  passed with 12 tests. A parallel validation attempt of scheduler-cache and
  secret collided on the default test HTTP port (`eaddrinuse`); rerunning
  `dev_secret` serially passed.
- Closed the router/node-process cluster from the full paranoid suite.
  `router@1.0` and `node-process@1.0` now opt out of in-flight grouping because
  both resolve node-local operational state from `Opts`; node-process lookup
  results are private `no-store`; router route-map results are private
  `no-store`; and route registration authorizes against the first signed
  ancestor rather than the locally extended request wrapper. Validation:
  `HB_PARANOID=cache_read,cache_write rebar3 device test --module dev_router`
  passed with 30 tests; `HB_PARANOID=cache_read,cache_write rebar3 device test
  --module dev_node_process` passed with 3 tests; and `git diff --check`
  passed.
- Closed the Lua ledger failure from the full paranoid suite. The first
  signed-subset helper version still returned all committed keys from the
  signed ancestor, including local HMAC/node-only commitments; this let cached
  process metadata leak into process identity comparisons. `with_only_signed/2`
  now returns only keys covered by signature-bearing commitments, and
  `lib_process:ensure_process_key/2` keeps the `process` key trimmed to that
  signed core using explicit `message@1.0/set` replacement. Validation:
  `HB_PARANOID=cache_read,cache_write rebar3 device test --module
  hb_codec_test_vectors --test
  hb_codec_test_vectors:with_only_signed_walks_extension_test+with_only_signed_preserves_unsigned_test+with_only_signed_ignores_hmac_commitments_test+with_only_signed_excludes_local_commitments_test`
  passed with 4 tests; `HB_PARANOID=cache_read,cache_write rebar3 device test
  --module 'lua@5.3a [test_ledgers]'` passed with 8 tests; and `git diff
  --check` passed.
- Restored recorder visibility for the varied lookup path. Varying resolves
  the device function in stage 2, so stage 5 reuses it and no longer emits the
  normal lookup events there; `prepare_vary/3` now emits the same
  `resolving_key` and `found_func_for_exec` events around the actual lookup.
  Validation: `HB_PARANOID=cache_read,cache_write rebar3 device test --module
  'recorder@1.0' --test dev_recorder:record_installs_hook_test` passed, and
  `git diff --check` passed.
- Fixed the HTTP compute timeout without carrying the temporary HTTPSig loader
  work. The timeout was caused by client-side `GET` requests implicitly asking
  the server to bundle replies; with overlay semantics the `/compute` response
  may include a large inherited process state, so the HTTP layer was
  materializing far more than the caller asked to inspect. `prepare_request/6`
  now defaults `accept-bundle` to `false` for `GET` while preserving the old
  bundled default for writes and explicit callers. Validation:
  `rebar3 device test --module hb_process_test_vectors --test
  hb_process_test_vectors:http_wasm_process_by_id_test_parallel` passed;
  `HB_PARANOID=cache_read,cache_write rebar3 device test --module
  hb_process_test_vectors --test
  hb_process_test_vectors:http_wasm_process_by_id_test_parallel` passed; and
  `git diff --check` passed.
- Revalidated the HTTP/codec surface after the GET bundling default change.
  Validation: `git diff --check` passed; `HB_PARANOID=cache_read,cache_write
  rebar3 eunit --module=hb_http_client_tests` passed with 3 tests;
  `HB_PARANOID=cache_read,cache_write rebar3 device test --module
  dev_httpsig` passed with 5 tests; and `HB_PARANOID=cache_read,cache_write
  rebar3 device test --module hb_codec_test_vectors` passed with 1958 tests.
- The next full `HB_PARANOID=cache_read,cache_write rebar3 eunit-all` exposed
  that the global GET `accept-bundle => false` default was too broad: ordinary
  404/error responses and bundled device replies reached clients as unbundled
  HTML bodies, breaking Arweave, auth-hook, b32-name, bundler, location,
  manifest, and secret tests. Reverted the core HTTP default to the edge
  behavior (`accept-bundle => true` unless callers opt out) and moved the
  no-bundle request to the one process HTTP vector that only inspects the
  returned output. Validation: `git diff --check` passed;
  `HB_PARANOID=cache_read,cache_write rebar3 device test --module
  hb_process_test_vectors --test
  hb_process_test_vectors:http_wasm_process_by_id_test_parallel` passed;
  `HB_PARANOID=cache_read,cache_write rebar3 device test --module dev_arweave`
  passed with 39 tests; `dev_b32_name` passed with 9 tests; `dev_manifest`
  passed with 6 tests; `dev_auth_hook` passed with 4 tests; `dev_location`
  passed with 3 tests; `dev_bundler` passed with 27 tests; `dev_secret` passed
  with 12 tests; and `dev_router` passed with 30 tests after one flaky
  performance-weight assertion passed on isolated and repeated full reruns.
- Acceptance checkpoint reached: `HB_PARANOID=cache_read,cache_write rebar3
  eunit-all` passed end-to-end with `All 3487 tests passed.` This is the first
  full-suite green point on the clean vary/overlay branch. Next pass is branch
  minimization and review against `hyperbeam-main/edge`, preserving this green
  state.
- Post-acceptance minimization audit began. Current branch diff is 3196 added /
  242 removed vs `hyperbeam-main/edge`, of which code is 1931 added / 242
  removed after excluding `MEMORY.md`, `STATUS.md`, and `decisions/`. Guardrail
  search found no branch-introduced `persistent_term`, result-edge helper,
  `dev_green_zone`, or `dev_snp` implementation churn; `dev_process_cache`
  references are inherited from current `edge`. Cleaned only local indentation
  in the modified `message@1.0/set` block. Validation: `git diff --check`
  passed; `rebar3 compile` passed; `HB_PARANOID=cache_read,cache_write rebar3
  device test --module dev_message` passed with 20 tests.
- Tightened the new type coercion path while auditing `hb_types`: after a value
  is coerced, it is rechecked against the target schema before being accepted.
  This prevents literal specs and bounded scalar specs from accepting a
  coerced value that still violates the declared type. Added a regression test
  covering non-negative integer coercion and literal equality. Validation:
  `git diff --check` passed; `rebar3 eunit --module=hb_types` passed with 6
  tests; `HB_PARANOID=cache_read,cache_write rebar3 eunit
  --module=hb_ao_test_vectors` passed with 191 tests.
- The follow-up full paranoid suite after the `hb_types` tightening reached
  `Failed: 0. Skipped: 0. Passed: 3487` but eunit reported one cancelled test:
  `copycat@1.0 [graphql]:basic_test_parallel` timed out in the HTTP client.
  Isolated validation of the same module under paranoid cache checking passed:
  `HB_PARANOID=cache_read,cache_write rebar3 device test --module
  'copycat@1.0 [graphql]'` passed with 9 tests, including
  `basic_test_parallel` in 31.259 s. Rerunning full `eunit-all` now to regain a
  clean latest-tip acceptance point rather than treating the cancelled run as
  green.
- Latest-tip acceptance is green again after the `hb_types` tightening:
  `HB_PARANOID=cache_read,cache_write rebar3 eunit-all` passed with `All 3488
  tests passed.` The previous copycat cancellation did not reproduce; the same
  `copycat@1.0 [graphql]:basic_test_parallel` passed in 5.765 s inside the
  full run.
- Minimized the cache-control surface after the full green checkpoint by
  removing the dead execution-vs-lookup heuristic wrapper. The old heuristic is
  intentionally gone because varied execution requires cache lookup on the
  canonical varied pair; the direct member-read fast path now lives in
  `hb_ao`. Validation: `git diff --check` passed;
  `HB_PARANOID=cache_read,cache_write rebar3 eunit --module=hb_cache_control`
  passed with 13 tests; `HB_PARANOID=cache_read,cache_write rebar3 eunit
  --module=hb_ao_test_vectors` passed with 191 tests. One attempted parallel
  validation collided on the default HTTP port (`eaddrinuse`) before tests
  started; the successful rerun above was sequential.
- Trimmed a leftover alias from `json-iface@1.0` after vary specs made the old
  whole-message force-load unnecessary. Validation: `git diff --check` passed;
  `HB_PARANOID=cache_read,cache_write rebar3 device test --module
  dev_json_iface` passed with 2 tests.
- Latest-tip acceptance after the cache-control and json-iface minimization
  commits is green: on `224520782`, `HB_PARANOID=cache_read,cache_write
  rebar3 eunit-all` passed end-to-end with `All 3488 tests passed.` The
  previously noisy router performance assertion passed inside this full run.
- Fixed a cache-paranoia shortcut found during audit: `cache_read` and
  `cache_write` verification now still checks materialized committed subsets
  instead of skipping commitment verification entirely, while deferring
  committed keys that are still link placeholders until they are loaded. Added
  a regression that direct committed-key tampering fails under both cache
  topics. Validation: `git diff --check` passed;
  `HB_PARANOID=cache_read,cache_write rebar3 eunit
  --module=hb_ao_test_vectors` passed with 196 tests; the first
  `hb_codec_test_vectors` rerun exposed linked committed keys that must be
  deferred, and after the lazy-aware fix `HB_PARANOID=cache_read,cache_write
  rebar3 device test --module hb_codec_test_vectors` passed with 1958 tests.
- The follow-up full paranoid suite on `2e6a30e56` exposed an HTTP cache-write
  honesty issue in `hb_http:send_large_signed_request_test`: `hb_http` would
  store a signed message decoded from the wire whenever `store-all-signed` was
  enabled, even if request verification was not forced and the decoded
  commitments did not verify in that post-transport shape. Kept request
  acceptance semantics unchanged, but now only performs the signed cache write
  after verification succeeds. Validation: `git diff --check` passed;
  `HB_PARANOID=cache_read,cache_write rebar3 eunit --module=hb_http` passed
  with 14 tests, including `send_large_signed_request_test`.
- The next full paranoid suite on `0535363e6` moved past the HTTP failure but
  failed with the already-observed router performance assertion plus secret
  device cache-write failures. Root cause for the secret failures: materialized
  `cookie@1.0` HMAC commitments use `secret:` key IDs and cannot be verified in
  generic cache read/write context because the secret is intentionally absent.
  Cache paranoia now defers only those secret-key HMAC commitments while
  continuing to verify self-contained materialized commitments. Validation:
  `git diff --check` passed; `HB_PARANOID=cache_read,cache_write rebar3 eunit
  --module=hb_ao_test_vectors` passed with 196 tests; `HB_PARANOID=cache_read,cache_write
  rebar3 device test --module dev_secret` passed with 12 tests; and
  `HB_PARANOID=cache_read,cache_write rebar3 device test --module dev_router`
  passed with 30 tests, confirming the full-run router failure is the existing
  timing-sensitive assertion rather than a deterministic branch regression.
- Latest-tip acceptance after the HTTP signed-wire and secret-HMAC cache
  verification fixes is green: on `5b8c8dde7`,
  `HB_PARANOID=cache_read,cache_write rebar3 eunit-all` passed end-to-end with
  `All 3493 tests passed.` The full run also cleared the previously noisy
  router performance assertion and the copycat GraphQL case.
- Post-green audit found that the `with_only_signed/2` HMAC regression only
  excluded the fixture because it lacked a `committer`. Tightened signed
  ancestry detection to reject `hmac-sha256` commitments directly, and made the
  fixture include both `signature` and `committer`. Validation:
  `git diff --check` passed; `HB_PARANOID=cache_read,cache_write rebar3 device
  test --module hb_codec_test_vectors --test
  hb_codec_test_vectors:with_only_signed_ignores_hmac_commitments_test` passed.
- Latest-tip acceptance after the HMAC signed-ancestry tightening is green: on
  `0412da3c1`, `HB_PARANOID=cache_read,cache_write rebar3 eunit-all` passed
  end-to-end with `All 3493 tests passed.` This run also covered the full
  `hb_codec_test_vectors` signed-subset cases, router performance assertion,
  secret-device flows, and process HTTP vectors.
- Post-compaction unattended audit reread `decisions/ao-core-upgrade-north-star.md`,
  `decisions/ao-core-upgrade-plan.md`, and `MEMORY.md`, then confirmed the
  worktree was clean at `2813be5e9`. `git diff --check` passed. Erlang
  abstract-code probing confirmed `#{}` is represented distinctly from
  `map()`, matching the empty-projection semantics in `hb_types`; no code
  change was needed. Broad `_ => _` specs were reviewed and remain concentrated
  in hook/codec/forwarding surfaces or process/scheduler carry-forward cases,
  rather than generic vary-on-everything force loads.
- Minimized the HTTP signed-wire cache fix so normal unforced HTTPSig requests
  do not pay an extra verification step unless `force-signed-requests` or
  `store-all-signed` requires it. Forced requests retain the old rejection
  semantics; unforced `store-all-signed` requests verify only to decide whether
  to cache. Validation: `git diff --check` passed;
  `HB_PARANOID=cache_read,cache_write rebar3 eunit --module=hb_http` passed
  with 14 tests; and `HB_PARANOID=cache_read,cache_write rebar3 device test
  --module dev_httpsig` passed with 5 tests.
