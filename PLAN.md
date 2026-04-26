**Updated Model**

Yes: `#{}` is “vary on nothing except implicit keys”, and `#{ _ => _ }` is “vary on everything, preserving unmatched keys unloaded/uncoerced.” That gives us the simple all/nothing vocabulary we need.

I also agree that `map()` should collapse to `_`: it means “no AO-specific shape constraint”, so the value is accepted as-is rather than treated as an empty closed message. AO message specs should use `#{...}` when they want projection semantics.

**Spec Syntax**

```erlang
-spec compute(
    #{ already_seen => [integer()] },
    #{ slot := integer() },
    _
) -> {ok, #{ results := _, _ => base }}.
```

Input rules:

- `_` or `map()` means unchanged/any.
- `#{}` means no explicit dependency.
- `#{ key := Type }` means required, load and coerce.
- `#{ key => Type }` means optional, load and coerce if present.
- `#{ _ => _ }` means preserve all unmatched keys as part of the varied message.
- Base implicitly includes `device => _`.
- Request implicitly includes `path := _`.
- For `add_key` handlers, AO-Core overrides request `path` with the resolved key before varying.

Return rules:

- `#{ _ => base }` means cache the raw result, then `set` it over the original base.
- `#{ _ => request }` means cache the raw result, then `set` it over the original request.
- The return spec takes precedence. We should skip runtime markers for the first implementation.

**Hashpath Rule**

Hashpath should cover exactly the values that influence the final returned result:

```erlang
none:
  HashBase = VariedBase
  HashReq  = VariedReq

_ => base:
  HashBase = OldBase
  HashReq  = VariedReq

_ => request:
  HashBase = VariedBase
  HashReq  = OldReq
```

So your correction is right: if the result extends the original request, the full original request participates in the final hashpath.

**AO-Core Flow**

1. Stage 1 normalizes `OldBase/OldReq`.
2. Resolve key/device/function once, before cache lookup.
3. Vary using the resolved function spec.
4. Always run `hb_message:normalize_commitments(..., fast)` on `VariedBase/VariedReq`.
5. Cache lookup uses `VariedBase/VariedReq`.
6. Persistent grouping uses `VariedBase/VariedReq`.
7. Execute on `VariedBase/VariedReq`.
8. Normalize raw result, set its generic hashpath from `VariedBase/VariedReq`, and cache only this generic result.
9. Notify persistent waiters with the generic result, not the caller-finalized result.
10. Each caller finalizes locally:
    - apply overlay with `hb_ao:set(OldBase | OldReq, Result, Opts#{ hashpath => ignore, ... })`
    - normalize commitments fast
    - set final hashpath using the rule above
11. Existing spawn-worker and stream continuation run with the finalized result.

The point behind step 9 is the cache-hit concern I was gesturing at earlier: if two callers share `VariedBase/VariedReq` but have different original bases/requests for overlay, the leader cannot broadcast its finalized overlay result. It must broadcast the generic cached result, and each waiter applies its own overlay/hashpath.

**Minimal Patch Strategy**

I agree on avoiding a context map. To keep the diff tight, I’d use the existing `Opts` temp-state pattern already used for `add_key`.

Minimal shape:

- Add temp keys to `?TEMP_OPTS`, for example `resolved_func` and `vary`.
- In stage 2, do function lookup plus vary, then continue with `VariedBase/VariedReq`.
- Stage 5 first checks `resolved_func`; if present, it skips lookup.
- Stage 9 remains the generic hashpath step for cacheability.
- Stage 10 still caches, but only the generic varied result.
- Stage 11 notifies waiters with the generic result, then calls one small finalization helper.
- Cache hits and persistent waits also call that same finalization helper before returning.

This keeps the change localized and avoids threading new arguments through every resolver stage.

**Trade-Offs**

Moving function lookup before cache means a missing/unloadable device can no longer be masked by an old exact cache hit. I think that is acceptable because variant caching cannot be sound without knowing the actual function.

The only new complexity I think is unavoidable is “generic result versus finalized result.” We can minimize it by naming exactly one helper for finalization and by keeping cache, persistent notify, and wait behavior visibly tied to the generic result.

**Tests I’d Add First**

- `#{}` varies to implicit-only.
- `map()` and `_` leave message unchanged.
- `#{ _ => _ }` preserves extras without force-loading them.
- Return `#{ _ => base }` caches generic result and overlays current base.
- Return `#{ _ => request }` hashes with full original request.
- Persistent waiters sharing a varied execution finalize against their own original inputs.
- `add_key` handler uses resolved key as request `path`.

If that matches your intent, I’m ready to start cutting the minimal patch.
