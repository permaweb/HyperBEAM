# `stack@1.0` — the device-composition (pipeline) device

- **Device name:** `stack@1.0`
- **Depends-on:** `message@1.0` (identity reads, `keys`, `set`, prefix bookkeeping), `multipass@1.0` (an example producer of the repass signal this device interprets). Both specs are provided to reimplementers.
- **Status:** Draft

## 1. Overview

`stack@1.0` is a **composition device**: a message bound to it carries a list of
*other* devices (the **device stack**) and, when a key is resolved on it, runs
that **same key** against each device in the stack. It has two modes. In **fold**
mode (the default) it threads a single accumulating message (the **state**)
through the devices in order — each device receives the previous device's output
as its base and returns the next state — and returns the final state to the
caller. In **map** mode it runs the key against every device **independently**,
each against the original base, and returns a message collecting each device's
result under that device's name.

Between devices, fold mode interprets a small **inter-device control protocol**
encoded in the *resolution outcome* of each device's key resolution (`ok`,
`skip`, the **repass signal**, `error`, and a non-message `ok`). This lets a
constituent device halt the rest of the stack, request a full re-run from the
first device (a **pass**), short-circuit with a raw value, or be transparently
skipped when it does not implement the key. The stack also maintains a small set
of per-execution bookkeeping keys (a pass counter and per-device input/output
prefixes) that constituent devices may read.

`stack@1.0` is the substrate on which layered behaviours are built (e.g. a
compute pipeline of [load → execute → meter] devices). `multipass@1.0` is a
canonical example of a device written specifically to drive this device's pass
mechanism.

## 2. Concepts & terminology

- **Device stack:** the ordered/named collection of devices this device runs,
  carried in the base message under the key `device-stack` (§3). Each entry's
  *value* is a device — a device identifier (`name@version`) or an inline device
  message — and each entry's *key* is the device's **name within the stack**.
- **State (fold):** the single accumulating message passed from device to device
  in fold mode. The caller's base message is the initial state; each device's
  `ok` output becomes the next device's base.
- **Pass:** one complete sweep of the stack from its first device. Tracked by the
  message key `pass` (a 1-based integer; the first sweep is pass `1`). A device
  may request another sweep via the **repass signal**.
- **Repass signal:** a distinguished resolution outcome — **separate from `ok`**
  — tagged `pass`, carrying a message payload. When a device emits it, the stack
  (fold mode only) increments `pass` and restarts from the first device. This is
  the outcome `multipass@1.0` emits. (Note: the *signal* is named `pass` and the
  *counter key* is also named `pass`; they are distinct. This spec writes
  "repass signal" for the outcome and "`pass` counter / `pass` key" for the
  counter.)
- **Skip signal:** a resolution outcome tagged `skip`, carrying a message
  payload. When a device emits it, the stack ends the current sweep early and
  returns the carried message (as a normal `ok`) to the caller.
- **The called key:** the key being resolved on the `stack@1.0` message (e.g.
  `compute`, `append`). The stack resolves *this same key name* on each
  constituent device. It is taken from the request path, exactly as for any
  device.
- **Per-device prefixes:** optional input/output key prefixes a constituent
  device may use to namespace the request fields it reads and the result fields
  it writes (§3, §5). Drawn from `input-prefixes` / `output-prefixes` maps keyed
  by stack-name.

The **internal representation** of the stack, of the state message, and of the
bookkeeping is **out of scope**; only the observable key/value contract and
resolution outcomes below are normative.

## 3. Device interface

- **Dispatch shape:** **default-handler.** The device does not enumerate a fixed
  set of data keys; a single catch-all handler answers **every** key by running
  it through the stack (fold or map), with the exceptions below.
- **Excluded keys (MUST delegate to `message@1.0`):** a catch-all (default-handler)
  device MUST exclude the **entire `message@1.0` reserved-key set** from the stack
  logic, NOT merely `set`/`keys`. The full set that MUST fall through to the
  identity/base-message device is at least: `keys`, `set`, `set-path`, `remove`,
  `device`, `id`, `commitments`, `committers`, `committed`, `commit`, `verify`,
  `path`. **This is load-bearing for termination, not just correctness:** because
  the catch-all answers "every other key" by re-running the stack, and the
  substrate itself resolves `device` (for dispatch) and `id`/commitment keys on
  the stack message during normal resolution, failing to exclude them makes a
  `device`/`id` resolution **re-enter the fold and recurse without bound**
  (observed: thousands of self-calls / a hang). An implementation that excludes
  only `set`/`keys` will infinite-loop. The two most-used delegations:
  - `keys` → the base message's public key listing (`message@1.0` `keys`).
  - `set` → the base message's deep-merge mutation (`message@1.0` `set`).
  This preserves the ability to bind the device onto a path, list its keys, and
  mutate the underlying message without the stack logic swallowing those
  operations. (The stack itself uses `set` internally to swap which device is
  active; that internal use is out of scope — see §11.)
- **The `transform` family (MUST):** a request whose first path segment is
  `transform` selects the **single-device transform** surface (§4 `transform`),
  used to run one named device from the stack in isolation:
  `…/transform/<stack-name>/<key>` resolves `<key>` against the base with *only*
  the device named `<stack-name>` active.
- **All other keys (MUST):** every other key is the **called key** and is run
  through the stack per the active mode (§4 default handler).
- **Optional exported-key restriction:** if the base message carries a
  `stack-keys` key (a list of key names), the device MUST restrict the set of
  keys it answers via its handler to exactly those names (keys outside the list
  are not answered by the stack and fall through as for any unexported key). When
  `stack-keys` is absent, the handler answers all (non-excluded) keys. This is an
  optional capability-narrowing control; an implementation that does not support
  it MUST still behave correctly for the (common) case where `stack-keys` is
  absent.

**Message shape (base):**

| Key | Type | Default | Meaning |
|---|---|---|---|
| `device-stack` | message (map of stack-name → device) | — (required) | the devices to run, keyed by name (§5) |
| `mode` | `Fold` \| `Map` | `Fold` | composition mode (overridable per request) |
| `pass` | integer ≥ 1 | `1` (set by the stack at fold start) | current pass counter (fold) |
| `input-prefixes` | map of stack-name → binary | absent → no per-device input prefix | per-device input key prefix (§5) |
| `output-prefixes` | map of stack-name → binary | absent → no per-device output prefix | per-device output key prefix (§5) |
| `input-prefix` | binary | `""` | the *current* device's input prefix (set by the stack while a device runs; readable by the device) |
| `output-prefix` | binary | `""` | the *current* device's output prefix (set by the stack while a device runs; readable by the device) |
| `stack-keys` | list of binary | absent | optional restriction of answerable keys (above) |

**Request shape:** the request carries the called key as its path. It MAY carry
`mode` (overrides the base `mode` for this resolution) and any request fields the
constituent devices consume. All keys are lowercase binary on the wire. `mode`
values are the exact binaries `Fold` and `Map` (capitalised first letter).

## 4. Resolved keys (normative)

### `keys` — list public keys (delegated)
- **Reads:** the base message.
- **Behaviour:** MUST return exactly what `message@1.0`'s `keys` returns for the
  base message (public, non-private keys, excluding `commitments`). The stack
  MUST NOT route `keys` through fold/map.
- **Returns:** `{ok, List}` as defined by `message@1.0`.
- **Side effects:** none.

### `set` — deep-merge mutation (delegated)
- **Reads:** the base message; the request as the set payload.
- **Behaviour:** MUST return exactly what `message@1.0`'s `set` returns for the
  base and that request.
- **Returns:** `{ok, NewMessage}` as defined by `message@1.0`.
- **Side effects:** none beyond those `message@1.0` `set` defines (none external).

### `transform/<stack-name>/<key>` — run one named device (the transform surface)
- **Reads:** `device-stack` from the base (read with identity/base-message
  semantics); the named sub-path segments `<stack-name>` and `<key>`.
- **Behaviour:** Resolving the first segment `transform` MUST yield a view of the
  base whose active device is replaced, on demand, by the device named
  `<stack-name>` taken from `device-stack`, such that resolving the **next**
  segment `<key>` runs `<key>` against the base with only that one device active.
  Concretely:
  1. Look up `<stack-name>` in `device-stack`. If it is absent, the **entire**
     `transform/<stack-name>/<key>` path MUST resolve to **`{ok, not_found}`** — a
     *successful* resolution to the `not_found` value (NOT `{error, not_found}`).
     Note the resolver walks one segment at a time, so it is NOT enough for the
     `transform` step alone to return the bare `not_found` atom — the trailing
     `<key>` would then resolve *against* that atom and yield `{error, not_found}`,
     failing this contract. Instead, the unknown-name transform MUST return a
     **view that absorbs every trailing key**: resolving ANY `<key>` against it
     yields `{ok, not_found}` (e.g. a small inline message whose device answers all
     keys with `{ok, not_found}`). So `<key>` never actually runs, and the whole
     path is a successful `{ok, not_found}`.
  2. If present, produce a copy of the base whose active device is the looked-up
     device, and which additionally records (for the device to read, and for
     later restoration): the active device's `device-key` = `<stack-name>`, the
     `input-prefix`/`output-prefix` for that device (from `input-prefixes` /
     `output-prefixes`, §5), and the displaced previous device/prefixes.
  3. Resolve `<key>` against that copy.
- **Returns:** the result of resolving `<key>` against the single-device view, or
  `{ok, not_found}` when `<stack-name>` is not in the stack.
- **Side effects:** none of its own (the resolved `<key>` may have side effects
  per its device).

### The default handler — run the called key through the stack
- **Reads:**
  - the **mode**: `mode` from the request if present, else `mode` from the base
    (read with identity/base-message semantics), else default `Fold`;
  - `device-stack` from the base;
  - in fold mode, the `pass` counter and per-device prefixes (below).
- **Behaviour:** Select the mode and run §4.A (fold) or §4.B (map). The *same*
  called key is resolved against each constituent device.
- **Returns:** fold → the final accumulated state (`{ok, State}`) or a
  short-circuit value; map → `{ok, ResultsMessage}`. See below.
- **Errors:** per the error strategy (§8) for device errors; otherwise the
  outcomes below.

#### 4.A Fold mode (default)
Fold runs the devices in **ascending numeric order** starting at `1` and
continuing `2, 3, …` until a device number is absent from `device-stack`
(end-of-stack). It threads a single state message.

Setup (once, before the first device):
1. Record the caller's current active device (to restore at the end).
2. Capture the caller's incoming `pass` value (default: *unset/absent*), to
   restore at the end.
3. Set the state's `pass` counter to `1`.

For the device at position `DevNum` (starting `DevNum = 1`):
1. **Activate** device `DevNum`: transform the state so that device's value
   (looked up as `device-stack/<DevNum>`) is the active device, recording
   `device-key`, the device's input/output prefixes, and the displaced previous
   device/prefixes (as in `transform`, §4 transform step 2).
   - If position `DevNum` is **absent** from `device-stack`, the stack is
     **complete**: stop and return the current state (see "Finalisation").
2. **Resolve the called key** against the activated state. Interpret the
   resolution outcome (the **control protocol**):

   | Outcome | Meaning | Stack action |
   |---|---|---|
   | `{ok, M}` where `M` is a **message (map)** | normal success | continue to `DevNum + 1` with `M` as the new state |
   | `{error, not_found}` | device does not implement the called key | **skip** this device; continue to `DevNum + 1` with the **current (pre-resolve activated) state** |
   | `{ok, V}` where `V` is **not a message** (a raw scalar/binary) | device produced a terminal raw value | **short-circuit**: return `{ok, V}` immediately (no finalisation restore; the raw value is returned as-is) |
   | `{skip, M}` (`M` a message) | the **skip signal** | end this sweep early: return `{ok, M}` (then finalise) |
   | `{pass, M}` (`M` a message) | the **repass signal** | increment the `pass` counter by 1 and **restart from `DevNum = 1`** with `M` as the state |
   | `{error, Info}` | device error | apply the **error strategy** (§8) |
   | any other shape | unexpected | apply the **error strategy** (§8) with reason `{unexpected_result, Outcome}` |

   Notes:
   - The two "not found"s are distinct. A **missing device position**
     (`device-stack/<DevNum>` absent) ends the stack. A device that **exists but
     returns `{error, not_found}`** for the key is skipped and the sweep
     continues with the next device. An implementation MUST treat the
     device-level `{error, not_found}` as *skip-and-continue*, never as a fatal
     error.
   - On skip-and-continue (`{error, not_found}`), the state carried forward is
     the state **as activated for this device** (i.e. including this device's
     activation bookkeeping), not the pre-activation state.
   - The repass increment is performed **by the stack**, not by the device that
     emitted the signal (the device returns the message unchanged; cf.
     `multipass@1.0`).

Finalisation (when the sweep ends via end-of-stack or the skip signal — **not**
for the non-message short-circuit):
- Restore the caller's original active device.
- Restore `input-prefix` / `output-prefix` to the displaced "previous" prefixes
  recorded at activation.
- Clear the per-execution activation bookkeeping (`device-key` and the
  previous-device marker) from the returned message.
- Restore the `pass` counter to the caller's captured incoming value (so a stack
  that the caller invoked without a `pass` returns without one; a re-invocation
  is idempotent in `pass`).
- Return `{ok, FinalState}`.

Re-invocation: invoking the same returned message again with the same key MUST
re-run the stack from scratch (the bookkeeping/`pass` are reset on entry and
cleared on exit), accumulating onto whatever the constituent devices accumulate.

#### 4.B Map mode
Map runs the called key against **each** device in the stack **independently**,
each against the **original** base (state is *not* threaded), and collects the
results.

1. Read `device-stack`. The set of devices to map over is **every entry** of
   `device-stack` **except** the AO-Core reserved keys (the protocol keys such as
   `device`, `path`, etc.; an implementation MUST exclude the same reserved-key
   set the substrate defines, so only genuine device entries are mapped).
2. For each remaining entry keyed `K`:
   - Activate device `K` (transform the base so device `device-stack/K` is
     active, as in `transform`).
   - Resolve the called key against that activated base.
   - If the outcome is `{ok, V}`, include `K => V` in the result.
   - For **any other** outcome (`{error, …}` including `not_found`, the skip
     signal, the repass signal, or an unexpected shape) **omit** `K` from the
     result entirely. Map mode does **not** apply the error strategy, does **not**
     short-circuit, and does **not** repass — non-`ok` outcomes are silently
     dropped.
3. **Returns:** `{ok, ResultsMessage}` where `ResultsMessage` maps each included
   device's stack-name `K` to that device's `ok` value. (The repass signal has
   no effect in map mode; `pass` is not maintained.)

## 5. Data formats & encodings

- **`device-stack` shape:** a message (map). Each **key** is a stack-name; each
  **value** is a device (a `name@version` binary, or an inline device message).
  For **fold** mode the stack-names MUST be the contiguous decimal strings
  `"1"`, `"2"`, …, `"N"`; fold visits them in ascending integer order and stops
  at the first missing integer. (A gap truncates the stack at the gap.) For
  **map** mode the stack-names MAY be arbitrary (the result keys mirror them);
  numeric names are the common case.
- **`mode`** values are the exact binaries `Fold` and `Map` (note the capital
  first letter). Any other value is unspecified; implementations SHOULD treat
  only these two and default to fold behaviour for the base-level default.
- **`pass`** is a 1-based integer; the stack sets it to `1` on fold entry and
  increments by 1 per repass.
- **Prefixes:** `input-prefixes` / `output-prefixes` are maps from stack-name to
  a binary prefix string. When activating device `K`, the stack sets the singular
  `input-prefix` / `output-prefix` to `input-prefixes/K` / `output-prefixes/K`
  respectively (absent → no prefix; the singular default is the empty binary
  `""`). A constituent device that wishes to namespace its I/O reads its inputs
  under `input-prefix` and writes its outputs under `output-prefix`; a device
  that ignores the prefixes operates on un-prefixed keys. The stack-level
  singular `input-prefix` / `output-prefix` on the base (if present) are
  preserved as the "previous" values and restored after the sweep. The map keys
  `input-prefixes`/`output-prefixes` MAY be keyed by integer or by decimal-string
  name equivalently (the activation looks up by the device's stack-name).
- The device emits **no** identifiers, commitments, or content-addressed values
  of its own; there is nothing to canonicalise and no base64url/hex concern at
  this layer. (IDs/commitments of the messages flowing through are governed by
  `message@1.0` and the constituent devices.)
- All key names are lowercase, hyphenated binary: `device-stack`, `mode`,
  `pass`, `input-prefix`, `output-prefix`, `input-prefixes`, `output-prefixes`,
  `stack-keys`, `device-key`.

## 6. Ordering, freshness & caching

- **Fold ordering is deterministic:** ascending integer order `1..N`,
  terminating at the first absent position. The state threaded is exactly the
  prior device's `ok` output (or the activated state on a skip-and-continue).
- **Map ordering** of *execution* is unspecified, but the result is a map keyed
  by stack-name, so order does not affect the observable result. Result map key
  ordering is unspecified; callers MUST NOT depend on it.
- **Pass loop termination** is **not** guaranteed by this device: the stack
  honours every repass signal (when multipass is permitted, §7) and will loop as
  long as some device keeps emitting it. Bounding the loop is the responsibility
  of the constituent devices (e.g. `multipass@1.0` stops emitting the signal once
  `pass` reaches its target). An implementation MUST NOT impose its own arbitrary
  pass cap.
- The device performs **no result caching of its own**; caching of a resolution
  that routes through the stack is governed by the surrounding substrate/node
  configuration.

## 7. Security & authority

- The stack performs no authorisation of its own and requires no commitment to
  invoke; authority is whatever the constituent devices enforce.
- **HashPath integrity:** the stack changes which device is active by *setting*
  the message's `device` key (a normal committed/inspectable mutation) rather
  than by opaque substitution, so the resolved message's HashPath remains
  correct and verifiable across the delegation to each constituent device. An
  implementation MUST perform device-switching through the standard message
  `set` path for this reason (the mechanism is internal; the **requirement** is
  that the through-stack resolution produces the same HashPath as the equivalent
  explicit sequence of single-device resolutions).
- **Multipass gate (failure-closed on looping):** the repass signal causes a
  re-run **only if** the node/run configuration permits multipass (an
  `allow-multipass` gate). Where multipass is **not** permitted, the stack MUST
  treat the repass signal as terminal (return the carried message as `{ok, …}`)
  rather than looping. This prevents a constituent device from forcing unbounded
  re-execution where the operator has disallowed it. (Where the gate is open, the
  bound is the constituent devices' behaviour, §6.)
- The excluded `keys`/`set` honour `message@1.0`'s private-key rules; the stack
  exposes no private keys itself.

## 8. Errors

Error handling in **fold** mode is governed by a node/run **error strategy**
option (`error-strategy`), with two values:

- **`throw` (default):** a device `{error, Info}` (or an unexpected outcome
  shape) MUST raise a fatal error that aborts the resolution, **via
  `erlang:error/1` (or `exit/1`)** — NOT a literal `erlang:throw/1`. (This matters
  observably: under a plain `catch`, an `error`/`exit` surfaces as `{'EXIT', _}`
  whereas a `throw` surfaces as the bare value; the strategy named "throw" must
  produce the `{'EXIT', _}` form.) The raised reason is
  **`{stack_call_failed, Base, Req, DevNum, Info}`** (the same 5-tuple as the stop
  strategy below; `Info` is the device's `Info`, equivalently
  `{unexpected_result, Outcome}` for a malformed outcome). The resolution MUST
  fail (not return a partial state).
- **`stop`:** a device `{error, Info}` MUST instead cause the stack to return,
  without raising, exactly
  **`{error, {stack_call_failed, Base, Req, DevNum, Info}}`** — a tagged 5-tuple
  inside `{error, _}` where `DevNum` is the failing device's **integer** position
  (1-based) and `Base`/`Req` are the state and request at that step. Resolution
  ends with that error value; subsequent devices are not run.

Other error-relevant outcomes (fold):

- `{error, not_found}` from a device is **not** an error — it is the
  skip-and-continue case (§4.A). The error strategy is **not** applied to it.
- The **skip signal** and **repass signal** are control outcomes, not errors.

In **map** mode there is **no** error strategy: any non-`ok` outcome (including
`{error, …}`) simply omits that device from the result (§4.B).

Hyphenated error atoms are used throughout (e.g. `not-found` semantics surfaced
by constituent devices; the stack's own `stop`-strategy error is a
`stack-call-failed`-style atom). The stack defines no error atoms beyond the
strategy-controlled propagation above; the only "errors" it *originates* are the
fatal raise (throw strategy) and the `{error, …}` return (stop strategy).

## 9. Composition

- **Building pipelines:** the canonical use is a fold stack of layered devices
  resolving the same key (e.g. `compute`) in sequence, each transforming the
  shared state. Because the called key is forwarded verbatim, a stack is
  *transparent* to callers: resolving `compute` on the stack behaves like
  resolving `compute` on a single device that happens to be the composition.
- **Repass-driven loops (`multipass@1.0`):** placing a device that emits the
  repass signal (e.g. `multipass@1.0` with `passes => N`) inside a fold stack
  causes the whole stack to run `N` sweeps. On each sweep the stack-maintained
  `pass` is visible to every device; the repass driver emits the signal while
  `pass < N`, the stack increments `pass` and restarts from device `1`, and the
  loop ends when the driver finally returns `ok` (at `pass == N`). The stack owns
  the increment and the restart; the driver owns the *decision*. This contract is
  the consumer half of the `multipass@1.0` spec.
- **Early exit:** a device that returns the **skip signal** ends the current
  sweep immediately, returning the carried state — useful for "this layer has
  produced the final answer; do not run later layers this pass".
- **Raw short-circuit:** a device that returns a **non-message `ok`** value makes
  the stack return that raw value directly (bypassing finalisation) — useful for
  a terminal scalar result (e.g. a serialised body) where no further state
  threading is wanted.
- **Per-key narrowing (`stack-keys`):** a stack MAY restrict which keys it
  answers via `stack-keys`, so the same composed message can expose only a chosen
  subset of the constituent devices' surface.
- **Single-device access (`transform`):** `…/transform/<name>/<key>` runs one
  named device from the stack in isolation, for inspection/testing or for calling
  a specific layer directly.
- **Prefix namespacing:** the per-device `input-prefix` / `output-prefix`
  mechanism lets several instances of the *same* constituent device coexist in a
  stack while reading/writing disjoint key namespaces (configured via
  `input-prefixes` / `output-prefixes`).
- **Path binding / mutation:** because `keys` and `set` are delegated to
  `message@1.0`, the stack message can be bound onto a path and have the
  underlying message listed/mutated without the stack logic interfering (the
  standard requirement for any default-handler device).

## 10. Conformance (normative checklist)

An implementation MUST exhibit all of the following, each checkable by resolving
keys against a constructed `stack@1.0` base message (with constituent devices
chosen to exercise the path):

1. **Fold default + order.** With `device-stack = #{ "1" => D1, "2" => D2 }` and
   no `mode`, resolving a key runs `D1` then `D2`, threading state: `D2` sees
   `D1`'s output as its base, and the caller receives the final state. (Verifiable
   with two accumulating devices that append distinct markers; the result shows
   D1's marker before D2's.)
2. **Mode precedence.** `mode` in the **request** overrides `mode` in the base;
   absent in both, the mode is `Fold`. `mode => Map` (request) forces map mode
   even if the base says `Fold`, and vice-versa.
3. **End-of-stack.** Fold stops at the first missing integer position: a stack
   with positions `1,2` runs exactly two devices; a gap (e.g. `1,3`) truncates at
   the gap (only `1` runs).
4. **Skip-and-continue on device `not_found`.** A device that returns
   `{error, not_found}` for the called key is skipped and the next device runs;
   the final state reflects every device that *did* implement the key, and a
   later device's other keys remain resolvable on the result. (No error is
   raised.)
5. **Skip signal.** A device returning the **skip signal** ends the sweep: the
   caller receives that device's carried message as `{ok, …}`, and **no** later
   device in the stack runs that sweep.
6. **Repass signal + `pass` counter.** A device returning the **repass signal**
   causes the stack to increment `pass` by 1 and re-run from device `1` with the
   carried message; the `pass` key is observable to devices (starts at `1`,
   increments each repass). A device that stops emitting the signal at a target
   pass ends the loop. Repass is **fold-only**.
7. **Multipass integration.** A fold stack containing `multipass@1.0` with
   `passes => N` runs exactly `N` sweeps; each constituent device executes `N`
   times (verifiable with an accumulating device whose output records one
   contribution per pass).
8. **Raw short-circuit.** A device returning `{ok, V}` with `V` **not** a message
   makes the stack return `{ok, V}` directly, with no further devices run and no
   finalisation rewrite of `V`.
9. **Map mode.** `mode => Map` runs the key against each device independently
   against the original base and returns a message keyed by stack-name: for
   `#{ "1" => D1, "2" => D2 }` the result has `1/<...>` from D1 and `2/<...>` from
   D2, each computed from the **same** input base (state not threaded). Non-`ok`
   device outcomes (including `not_found`) omit that device's entry; map mode
   never raises, skips-the-rest, or repasses.
10. **Error strategy.** Under the default (`throw`) strategy, a device
    `{error, Info}` (or an unexpected outcome) aborts the resolution fatally; under
    the `stop` strategy the same yields an `{error, …}` (`stack-call-failed`-style)
    return naming the failing device position, without raising. (`{error,
    not_found}` is exempt — it is skip-and-continue, item 4.)
11. **Delegated `keys`/`set`.** Resolving `keys` returns the `message@1.0` public
    key listing for the base (not a per-device fold); resolving `set` returns the
    `message@1.0` deep-merge of the request onto the base. Neither is routed
    through fold/map.
12. **Transform surface.** `…/transform/<name>/<key>` resolves `<key>` against the
    base with only the device named `<name>` active; an unknown `<name>` makes the
    transform of that name `not_found` (and `<key>` does not run).
13. **Prefix bookkeeping.** With `output-prefixes => #{ "1" => P1, "2" => P2 }`, a
    constituent device that writes under its `output-prefix` produces output under
    `P1` / `P2` respectively; with no prefixes configured the singular
    `input-prefix`/`output-prefix` default to the empty binary and the device's
    I/O is un-prefixed. After the sweep, the stack restores the caller's original
    active device and prefixes, and the returned message does not carry the
    per-execution activation bookkeeping (`device-key`, previous-device marker).
14. **`pass` restoration / re-invocation.** A caller that invokes the stack
    without a `pass` key receives a result without a `pass` key (the stack-internal
    counter is cleaned up); invoking the returned message again with the same key
    re-runs the stack from scratch and accumulates again (idempotent counters,
    accumulating data).
15. **HashPath transparency.** Resolving the key through the stack yields the same
    HashPath as the equivalent explicit sequence of single-device resolutions
    (device-switching is performed via message `set`, not opaque substitution).

## 11. Out of scope

- The **internal representation** of the stack, the state message, the
  bookkeeping keys (`device-key`, previous-device / previous-prefix markers), and
  links.
- The **exact reason structure** of the throw-strategy raise and the
  stop-strategy `{error, …}` return (only "must fail fatally" vs "must return an
  error naming the failing position" is normative).
- The mechanism by which device-switching is implemented (it MUST go through
  message `set` to preserve HashPath, but the specifics are internal).
- The semantics of the delegated `keys`/`set` beyond "identical to `message@1.0`"
  (see the `message@1.0` spec).
- The behaviour, side effects, and error atoms of the **constituent devices**;
  the stack only sequences them and interprets their control outcomes.
- The precise AO-Core reserved-key set excluded from map iteration (a substrate
  definition the implementation MUST reuse, not redefine).
- Result-caching/freshness of resolutions routed through the stack, and any
  `allow-multipass` / `error-strategy` configuration mechanics beyond their
  observable effect (node/substrate configuration concern).
- Performance and storage strategy.

## Open questions

These are ambiguities in the contract that a reimplementer may resolve either
way without (observably) diverging from the reference, or that the reference
itself leaves under-pinned:

1. **`mode` case-sensitivity and unknown values.** The reference matches the
   exact binaries `Fold` / `Map`. It is unspecified whether other casings
   (`fold`, `map`) or unknown values should be accepted, defaulted, or rejected.
   The reference effectively falls through to fold-vs-map on an exact match and
   does not define a third outcome; a reimplementer SHOULD accept exactly `Fold`
   and `Map` and treat the base-level default as fold.
2. **Source of the multipass gate and error strategy.** Whether
   `allow-multipass` and `error-strategy` are read from node options, from the
   message, or both, is left to the substrate (this spec pins only their
   observable effect: gate-off ⇒ repass is terminal; `throw` ⇒ fatal, `stop` ⇒
   `{error,…}` return). Default error strategy is `throw`. The default for the
   multipass gate is not pinned here; a conservative reimplementer treats the
   gate as a configuration input with the operator's chosen default.
3. **Map-mode reserved-key exclusion set.** Map iterates `device-stack` entries
   minus the AO-Core reserved/protocol keys. The exact membership of that
   reserved set is a substrate definition; this spec requires reusing it rather
   than enumerating it, so two implementations on the same substrate agree but
   the set is not reproduced here.
4. **Bookkeeping key names.** The per-execution activation bookkeeping (the
   "previous device" marker, the `device-key`) is internal; this spec pins only
   that it is *cleared from the returned message* and used to *restore* the prior
   device/prefixes. The exact key names are not normative and a reimplementer MAY
   choose its own (they are not part of the observable wire contract). The
   reference also exposes `device-key` to the active device during its run; a
   device MAY read it to learn its own stack-name, but relying on the precise key
   name is discouraged.
5. **`stack-keys` support.** Whether the optional `stack-keys` key-narrowing
   capability is implemented is left open; the spec requires correct behaviour
   when it is **absent** (the common case) and describes its effect when present.
6. **Raw short-circuit vs finalisation.** The reference returns a non-message
   `{ok, V}` value *without* running the device/prefix restoration. A reimplementer
   MUST preserve this asymmetry (raw values bypass finalisation); whether any
   later integration relies on the bookkeeping being absent from a raw return is
   not exercised by the reference.
