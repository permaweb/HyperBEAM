# Pushed Process Cache Semantics

## Prompt As Understood

Keep the AO-Core vary/type branch minimal while making `HB_PARANOID=cache_read,cache_write`
work across core and preloaded device tests. Fix root causes rather than
masking scheduler/process failures with broad loading or cache bypasses.

## Issue

`push@1.0` exposed three related cache-boundary mistakes:

- `{as, Device, Msg}` with a concrete no-path message was treated as a
  self-subresolution of `Msg` before the outer request, even though the caller
  intended "treat this message as this device and apply the request."
- `scheduler@1.0/slot` returned public `cache-control: no-store`, but caller
  opts using `cache-control: always` override public message cache control, so
  dynamic slot reads could be cached stale.
- Pushed AOS assignments only need the body loaded at the scheduler boundary;
  force-loading whole assignments pulls unrelated lazy links and can break
  process-local WASM state handling.

## Options

1. Add special cases in `push@1.0`.
2. Disable result caching for the push tests or broaden vary specs.
3. Fix the boundary semantics where they belong.

## Decision

Use the boundary fixes:

- A no-path map in `{as, Device, Msg}` for non-`message@1.0` devices is a
  device view of `Msg`; set its device and continue with the outer request.
  Path-bearing `as` still subresolves. `{as, <<"message@1.0">>, Msg}` keeps
  the existing subresolution/load behavior so the message's own device can
  handle the following request.
- Dynamic scheduler status/slot responses keep their public `cache-control`
  field but also mark private `no-store`, which is the existing hard no-store
  signal honored even under operator `always` opts.
- Scheduler cache reads for `ao.N.1` assignments materialize only `body`;
  lookahead/local cache returns the assignment without whole-message loading.

This keeps the fix in core/device boundary semantics and avoids broad cache
rewrites, process-cache revival, or generic vary-on-everything specs.
