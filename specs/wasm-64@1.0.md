# `wasm-64@1.0` — 64-bit WebAssembly execution device

- **Device name:** `wasm-64@1.0`
- **Depends-on:** `message@1.0` (reserved-key surface, prefix resolution, `set`/`get` semantics). **Relates to:** `wasi@1.0` (an optional stdlib provider that plugs into the import bridge defined here), `process@1.0` (the usual host stack that drives this device). Only the `message@1.0` spec is required to reimplement; the others are informative.
- **Status:** Draft

## 1. Overview

`wasm-64@1.0` executes a WebAssembly (WASM) module against an AO-Core message.
It loads a WASM image, instantiates it, calls an exported function with
caller-supplied arguments, and records the function's return value in the
message. It targets the **WebAssembly Memory-64 (`memory64`) proposal**: the
module's linear memory and pointer values are 64-bit, so memory addresses
exchanged across the host boundary are 64-bit integers.

The device is an **execution device**: it is designed to be driven inside a
multi-pass process stack (e.g. `process@1.0`). The stack calls `init` once to
boot the instance, `compute` on each pass to run a function, `snapshot` to
capture a resumable memory image for checkpoint/replay, `normalize` to restore
an instance from such an image, and `terminate` to tear the instance down.

Crucially, the device does **not** define what WASM imports (host functions)
do. Every call the running module makes *out* to the host is reflected back
into AO-Core as a resolvable message, so host functions — including the
WASI standard library — are themselves ordinary AO-Core devices mounted under
a well-known path. `wasm-64@1.0` is the bridge; `wasi@1.0` (and others) are the
implementations.

## 2. Concepts & terminology

- **Image:** the WASM module to execute, as a binary (the bytes of a `.wasm`
  module). It may be supplied directly, by reference (a content identifier
  resolvable to a message whose `body` is the bytes), or wrapped in a message
  whose `body` is the bytes.
- **Instance:** a live, instantiated WASM execution context produced from an
  image. The instance is **node-local, mutable, non-serialisable runtime
  state**. It is held in the message's **private** section (see `message@1.0`
  §2: keys under `private`/`priv`) and is therefore never part of the message's
  committed content, never returned by `keys`/`get`, and never content-addressed.
  Its concrete form is out of scope; only that it exists, is reachable at a
  fixed private key, and can be serialised/deserialised is normative.
- **Memory:** the instance's linear memory. Its **snapshot** — an opaque binary
  capturing enough instance state to deterministically resume execution — is the
  device's observable representation of memory.
- **Input prefix:** the key path, relative to the base message, under which the
  device reads its *inputs* (notably `image`). Read from the base message key
  `input-prefix`; default the empty prefix (inputs read at the top level).
- **Output prefix:** the key path under which the device reads/writes its *own*
  state — the private instance handle and helpers, the public results, and the
  stdlib mount point. Read from the base message key `output-prefix`; default
  the empty prefix. Throughout this spec `<OUT>` denotes the output prefix and
  `<IN>` the input prefix. When a prefix is the empty binary, the leading
  `<prefix>/` collapses (e.g. `<OUT>/instance` with empty `<OUT>` is `instance`,
  `<OUT>/results/...` is `results/...`; an empty-prefix path never begins with a
  stray `/`).
- **Import / host function:** a function the WASM module imports and calls at
  runtime. Each WASM import is identified by a `(module, field)` pair and a type
  **signature**. When invoked, the host suspends the module, resolves the call
  through AO-Core, and resumes the module with the result.
- **Standard library (stdlib):** the conventional location, `<OUT>/stdlib/...`,
  under which host-function-providing devices are mounted so the import bridge
  can find them. `wasi@1.0` mounts itself here.
- **Result type / output:** the WASM runtime returns, for a completed call, a
  *type* tag and a list of return *values*. Both are recorded in the message.

## 3. Device interface

- **Dispatch shape:** **explicit-keys.** The device answers the named keys
  `init`, `compute`, `snapshot`, `normalize`, `terminate`, and `import`, each
  with the signature `(Base, Req, Opts) → {ok, Value} | {error, _}`. All other
  keys (including the `message@1.0` reserved keys `keys`, `set`, `set-path`,
  `remove`, `id`, `commit`, `verify`) fall through to `message@1.0`.
- **Internal-only helper (not a resolved key):** the device exposes a means for
  *other devices* to fetch the live instance handle out of a message's private
  section. This MUST NOT be reachable as an AO-Core resolved key (i.e. resolving
  `.../instance` against a `wasm-64@1.0` base MUST NOT return the handle). How
  the handle is exposed to sibling devices is out of scope; only its
  non-resolvability is normative. (This avoids leaking a live, node-local handle
  into resolution output and hashpaths.)
- **Message shape (base) the device operates on:**
  - `device` = `wasm-64@1.0` (selects this device).
  - `input-prefix` *(optional binary, default empty)* — see §2.
  - `output-prefix` *(optional binary, default empty)* — see §2.
  - `<IN>/image` *(optional)* — the image, by binary, by reference, or by
    wrapping message (see `init`). If absent, the top-level `body` is used.
  - `<IN>/mode` *(optional binary, default `WASM`)* — execution mode; see `init`.
  - `function` *(optional binary)* and `parameters` *(optional list)* — the
    default location of the call target if not supplied in the request (see
    `compute`).
  - `snapshot` *(optional message)* — a previously produced memory snapshot used
    to restore an instance without a fresh `init` (see `normalize`/`snapshot`).
  - `device-key` *(optional binary)* — when restoring from `snapshot`, an extra
    path segment under which the snapshot body is nested (see `normalize`).
- **Private state the device maintains** (under the output prefix, in the
  message's private section — never committed, never listed):
  - `<OUT>/instance` — the live instance handle.
  - `<OUT>/import-resolver` — the bridge invoked for each WASM import (see §4
    `import`). An implementation MUST install the device's default bridge here at
    `init`.
  - `<OUT>/read`, `<OUT>/write` — helpers to read/write the instance's memory.
    These are conveniences for sibling host-function devices; their presence is
    RECOMMENDED, their exact callable form is out of scope.

## 4. Resolved keys (normative)

### `init` — boot a WASM instance

- **Reads:**
  - `input-prefix`, `output-prefix` from `Base` (via `message@1.0` `get`;
    defaults empty).
  - `<IN>/image` from `Base`. Resolution rules, in order:
    1. **Reference (content identifier):** if the value is an **ID** — a base64url
       content-id per the platform's id predicate (43 chars; ~32/43 bytes) — **and**
       it resolves to a stored message, read that message and use its `body` binary
       as the image bytes. (An ID-shaped value that does not resolve to a stored
       message falls through to rule 3 — raw binary.)
    2. **Wrapping message:** if the value is a message (map), use its `body`
       binary as the image bytes.
    3. **Raw binary:** if the value is a binary, use it directly as the image
       bytes.
    4. **Absent:** if `<IN>/image` is not present, fall back to the base
       message's top-level `body` binary, if a binary.
  - `<IN>/mode` from `Base`: `WASM` (default, interpreter) or `AOT`
    (ahead-of-time). `AOT` MUST be honoured only when the node enables AOT (the
    exact node-option key is **node/platform configuration**, not pinned by this
    protocol); otherwise `AOT` MUST be downgraded to `WASM` (NOT an error). Mode
    comparison is exact-match on the binaries `WASM` / `AOT`.
- **Behaviour:**
  1. Obtain the image bytes per the rules above. If no image can be obtained
     (no `<IN>/image` and no usable top-level `body`), the device MUST fail with
     error `wasm-init-error` (see §8). The error message SHOULD identify the
     `<IN>/image` path that was searched.
  2. Instantiate the WASM module from the bytes in the chosen mode, targeting the
     `memory64` proposal (see §`6.4 64-bit specifics`).
  3. Store, in the **private** section of the returned message: the live instance
     handle at `<OUT>/instance`; the device's default import bridge at
     `<OUT>/import-resolver`; and the memory read/write helpers at `<OUT>/read`
     and `<OUT>/write`.
- **Returns:** `{ok, Base'}` where `Base'` is `Base` with the private state
  above installed. No public keys are added by `init`.
- **Side effects:** Creates a node-local, in-memory WASM execution context.
  Reading an image by reference reads from the content store. No commitments, no
  external network calls, no public-state mutation.

### `compute` — call an exported function

- **Reads:**
  - First runs `normalize(Base, Req, Opts)` (see below) to guarantee a live
    instance and to strip any literal `snapshot` key, yielding the normalised base
    `M`. The **execution state** is `M` with the **request's non-reserved public
    keys folded in** (dropping `path`/`device`/`priv`/`hashpath`). This fold is
    load-bearing: request-supplied mounts — notably a stdlib at
    `<OUT>/stdlib/<module>` — must be part of the state the import bridge resolves
    against (§4a/§4b), since `hb_ao:resolve(Base, Req, …)` does NOT merge `Req`
    into the base. Without it a request-supplied stdlib is invisible and every
    import hits the stub. All reads below are against `M` / the execution state.
  - `output-prefix` from `M`.
  - `pass` from `M` (an integer pass counter, optional). The device only executes
    on the **first pass**: `pass == 1` or `pass` absent. For any other pass value
    the device is a **no-op** (returns the message unchanged). This lets the same
    device sit in a multi-pass stack and run exactly once.
  - The **function name**, taken as the first present of, in order:
    `Req` `body/function`, then `Req` `function`, then `M` `function`.
  - The **parameters**, taken as the first present of, in order:
    `Req` `body/parameters`, then `Req` `parameters`, then `M` `parameters`.
    Absent parameters default to the empty list.
- **Behaviour:**
  1. If no function name is found anywhere, **skip execution** and return the
     normalised message unchanged (`{ok, M}`). This is NOT an error: a pass with
     no function simply performs no call (e.g. an init-only pass).
  2. Otherwise call the exported function named by the function name, with the
     parameter list, on the live instance (`<OUT>/instance`), supplying the
     import bridge from `<OUT>/import-resolver` so the module's host calls are
     routed (see `import`). Execution MAY mutate instance memory and MAY invoke
     host functions an arbitrary number of times before returning.
  3. The call yields a runtime **call status** (the success status — e.g. `ok`;
     **not** a per-value WASM type), a **list of return values**, and the
     (possibly host-call-mutated) message state. (The 64-bit runtime returns no
     per-value type tag — `results/type` carries the call status.)
  4. Set, on that resulting (**post-call**) message: `<OUT>/results/type` to the
     call status (the runtime success tag, `ok`), and `<OUT>/results/output` to the
     list of return values. (The `type` carrier follows the platform's value
     handling — the atom `ok` and its wire-form `<<"ok">>` are equivalent; it is
     not separately pinned.)
- **Returns:** `{ok, M'}` — the message after execution, carrying
  `<OUT>/results/type` and `<OUT>/results/output`. When execution was skipped
  (no function), `{ok, M}` with no results written.
- **Side effects:** Mutates the live instance's memory; may drive host-function
  resolutions (which may themselves have effects defined by the mounted stdlib
  devices). The hashpath MUST NOT be advanced by the internal normalisation step
  (see §6).

### `snapshot` — serialise memory for checkpoint/replay

- **Reads:** the live instance handle at `<OUT>/instance` of `Base`.
- **Behaviour:** Serialise the instance's resumable state (linear memory and
  whatever else is needed to deterministically resume) into a single opaque
  binary.
- **Returns:** `{ok, #{ <<"body">> => <Serialized> }}` — a message whose `body`
  key holds the snapshot binary. The snapshot binary is **opaque**; its byte
  layout is out of scope. Implementations of this device MUST be able to consume
  their own snapshots via `normalize`; cross-implementation snapshot
  interchange is NOT guaranteed.
- **Side effects:** None (read-only over the instance).
- **Note:** `snapshot` requires a live instance; if none is present, behaviour is
  that of attempting `<OUT>/instance` on a message lacking it (the caller is
  expected to have `init`/`normalize`d first). Callers that need a snapshot from
  a cold message restore via `normalize` first.

### `normalize` — ensure a live instance; strip the literal snapshot key

- **Reads:**
  - The current instance handle at `<OUT>/instance` of `Base`.
  - If absent: `device-key` from `Base` (optional), and the snapshot body at
    the path `snapshot` + (`device-key` if present) + `body` — i.e.
    `snapshot/body` normally, or `snapshot/<device-key>/body` when `device-key`
    is set. **The stored snapshot MUST be read through the `message@1.0` (raw,
    inert) view of the base — NEVER via this device's own `hb_ao:get`.** Because
    `snapshot` is ALSO a resolved key (the serialise op), a device-dispatched read
    of `snapshot[/…]` would INVOKE `snapshot` (serialising the — possibly absent —
    instance, crashing) instead of returning the stored bytes. The same caveat
    applies to any observer of §10.18 ("the key is stripped"): a device-dispatched
    `snapshot` read re-serialises a live instance every time, so the stripping is
    observable only through the raw `message@1.0` view.
- **Behaviour:**
  1. **If a live instance already exists** at `<OUT>/instance`: do nothing to the
     instance. (No deserialisation.)
  2. **If no live instance exists:** locate the snapshot body as above.
     - If no snapshot body is found, the device MUST fail with error
       `no-wasm-instance-or-snapshot` (see §8).
     - Otherwise: boot a fresh instance from the same image source by performing
       `init` (so `<OUT>/instance`, `<OUT>/import-resolver`, helpers are
       installed), then **deserialise** the snapshot body into that instance,
       restoring its memory to the snapshotted state.
  3. In all cases, **remove the literal `snapshot` key** from the returned
     message (set it to the unset sentinel — see `message@1.0` `set`).
- **Returns:** `{ok, M'}` — a message guaranteed to have a live instance in its
  private section and **no** `snapshot` key.
- **Side effects:** May create and populate a node-local instance; reads the
  image source (possibly from the content store) when restoring.
- **Rationale (informative):** Two messages must compute identically — one
  carrying a literal `snapshot` (cold) and one carrying a live private instance
  (warm). `normalize` collapses both to the warm form **without advancing the
  hashpath**, so the result is independent of which form was supplied.

### `terminate` — tear the instance down

- **Reads:** `output-prefix` and the instance handle at `<OUT>/instance` of
  `Base`.
- **Behaviour:** Stop the live instance and release its resources. Remove the
  private `<OUT>/instance` key (set it to the unset sentinel).
- **Returns:** `{ok, M'}` — the message with `<OUT>/instance` cleared from its
  private section.
- **Side effects:** Destroys the node-local instance.

### `import` — the host-function (import) bridge

This key is the contract by which a WASM module's calls *out* to imported
host functions are serviced. It has two layers: the **bridge** the device
installs at `init` (`<OUT>/import-resolver`), which the runtime invokes per
import call; and the **`import` resolved key**, which that bridge calls and
which performs the AO-Core dispatch into the mounted stdlib.

#### 4a. Bridge contract (what the runtime hands the bridge)

For each import call the running module makes, the runtime invokes the installed
bridge with a request describing the call. The request carries:
- the live instance handle;
- `module` — the WASM import module name (e.g. `wasi_snapshot_preview1`);
- `func` — the imported field name (the host-function name);
- `args` — the list of argument values passed by the module (integers; 64-bit
  where the signature so dictates);
- a type **signature** string for the imported function.

The bridge MUST:
1. Construct a request message with `path` = `import`, and keys `module`, `func`,
   `args`, and `func-sig` (the signature), with `module`/`func`/`func-sig` as
   binaries and `args` as the argument list.
2. Resolve that request against the **current execution state** — the message
   `compute` is running on (live instance under `<OUT>/instance` in its private
   section). **Stdlib mounts are read from THIS message — the compute-time base —
   not the init-time one.** So a driver supplies a stdlib by setting
   `<OUT>/stdlib/<module>` on the base it resolves `compute` against (that message
   becomes the runtime's initial state, against which the bridge resolves §4b);
   the mount need not — and does not — exist at `init` time.
3. From the resolution result, read back two keys: `state` (the new base message
   to continue execution with) and `results` (the list of values to hand back to
   the module as the import's return values).
4. Return the `results` list and the `state` to the runtime, which resumes the
   module. The returned `state` becomes the live message for the remainder of
   execution.

#### 4b. The `import` resolved key (dispatch into the stdlib)

- **Reads:** from `Req`: `module`, `func`, `args`, `func-sig`. From `Base`:
  `output-prefix`, and (on the stub path) any prior `undefined-calls` log.
- **Behaviour:**
  1. Compute the stdlib target path
     `<OUT>/stdlib/<module>/<func>` and the per-module state path
     `<OUT>/stdlib/<module>/state`.
  2. Place the **current base message** at the per-module state path
     (`<OUT>/stdlib/<module>/state` ← `Base`), so the stdlib handler can read the
     full execution state at its `state` key. The placement is a **public** key on
     the `<OUT>/stdlib/<module>` sub-message (so the resolver walks into it and the
     mounted device receives it) — set with `hashpath => ignore` so it does **not**
     advance the hashpath. Do **not** place it in the private section: a private
     placement is invisible to the mounted device.
  3. Resolve the request (now repointed to the stdlib target path, still carrying
     `module`/`func`/`args`/`func-sig`) against that augmented base.
  4. **On success** (`{ok, Res}`): return `{ok, Res}` unchanged. By the
     host-function calling convention (§4c), `Res` is expected to carry `state`
     and `results`.
  5. **On `not_found`** (no device is mounted for this `(module, func)`): invoke
     the **undefined-import stub** (§4d) and return its result.
- **Returns:** `{ok, Res}` where `Res` carries `state` (new execution message)
  and `results` (the return-value list), per §4c.
- **Side effects:** Whatever the mounted stdlib device does. The state placement
  itself is private/non-hashpath-advancing.

#### 4c. Host-function calling convention (what a mounted stdlib device MUST return)

A device mounted at `<OUT>/stdlib/<module>/<func>` and invoked by the `import`
key is a **host function**. It is resolved as a normal AO-Core key with the base
augmented as in §4b and the request carrying `args` and `func-sig`. It:
- **Reads** the live execution state from its `state` key (placed by §4b step 2),
  the call arguments from `args`, and the signature from `func-sig`.
- **MUST return** `{ok, #{ <<"state">> => NewState, <<"results">> => Values }}`,
  where `NewState` is the updated execution message (to continue with) and
  `Values` is the list of return values handed back to the module. A host
  function with no meaningful return SHOULD return `results` = `[0]`.
- (This is exactly the contract `wasi@1.0`'s host functions satisfy.)

#### 4d. Undefined-import stub (default for unmounted imports)

When no device is mounted for an import, the device MUST NOT crash the module.
Instead it MUST:
1. Append the import request (the `Req` describing the call) to a running log at
   the path `<OUT>/results/undefined-calls` **on the returned `state` message**
   (prepending the newest call; the value is a list of the import requests seen).
   The list MUST be created if absent. (Prefix **before** `results` — consistent
   with `compute`'s `<OUT>/results/...` outputs, §5/§9; the log lives on the
   `state` this stub returns.)
2. Return `{ok, #{ <<"state">> => <updated base with the log>, <<"results">> =>
   [0] }}` — i.e. hand the module back a single `0` value and continue.

This makes a module importing an unimplemented host function run to completion
(treating the call as a no-op returning `0`) while leaving an auditable record
of every unimplemented call in the public results.

## 5. Data formats & encodings

- **Keys** are binary, lowercase, hyphenated (`output-prefix`, `func-sig`,
  `undefined-calls`, `import-resolver`). IDs (image references) are base64url.
- **Result keys** written by `compute`:
  - `<OUT>/results/type` — the result-type tag returned by the call (an opaque
    tag from the runtime; reproduced verbatim).
  - `<OUT>/results/output` — the list of return values from the call.
- **Snapshot** is a message `#{ <<"body">> => <binary> }`; the body is an opaque,
  implementation-defined serialisation of instance state. When embedded in a
  base for restore, it lives under the `snapshot` key (optionally nested one
  level deeper under `device-key`).
- **Numbers across the host boundary:** WASM arguments/returns are conveyed as
  the runtime's numeric values. Under `memory64`, pointer/size arguments are
  64-bit integers; the device MUST faithfully convey 64-bit operand widths
  (i.e. it MUST NOT truncate addresses to 32 bits).
- **Private vs public:** the instance handle, import resolver, and read/write
  helpers live under the message's **private** section (see `message@1.0` §2) and
  are therefore excluded from `get`, `keys`, IDs, and commitments. The
  `results`, the `undefined-calls` log, and the `snapshot` message are **public**
  message content.

## 6. Ordering, freshness & caching

### 6.1 Determinism
Given (a) the same image bytes, (b) the same starting memory (fresh, or restored
from a given snapshot), (c) the same function name and parameters, and (d)
host-function devices that are themselves deterministic over the same inputs, a
`compute` MUST produce the same `results/output`, `results/type`, and resulting
memory snapshot. The device introduces no nondeterminism of its own: it reads no
wall-clock, no randomness, and no node-identity into the computation. (Any
nondeterminism a module exhibits via host calls is the responsibility of the
mounted stdlib device, e.g. a clock host function — see `wasi@1.0`.)

### 6.2 Snapshot equivalence
A message carrying a literal `snapshot` and a message carrying a live private
instance restored from that same snapshot MUST compute identically. `normalize`
guarantees this by collapsing both to the warm form.

### 6.3 Hashpath stability
Booting/restoring an instance and placing per-module state for host calls are
**private, non-content** operations and MUST NOT advance the message's hashpath —
they add **no extra step** (a cold compute's hashpath still ends in exactly
`/compute`, not `/normalize/compute`). Note: a *cold* arrival carries a **public**
`snapshot` key while a *warm* arrival carries the instance **privately**, so their
pre-compute public content — and thus their hashpath prefixes — necessarily
differ. Equality holds (i) on the **content id** once the snapshot has been
consumed by `normalize`, and (ii) between any two arrivals with identical public
content (identical inputs ⇒ identical hashpaths and results; the live instance
never leaks into the hashpath).

### 6.4 64-bit specifics
The device targets the WebAssembly **Memory-64 (`memory64`)** proposal:
- Linear memory is addressed with 64-bit indices; the module's memory may exceed
  the 32-bit (4 GiB) limit of baseline WASM.
- Host-boundary pointer and length operands are 64-bit integers; the device and
  its memory read/write helpers MUST operate on 64-bit addresses without
  truncation.
- The device name's `-64` denotes this width. (A 32-bit sibling, were one to
  exist, would differ only in operand width; this spec is normative for the
  64-bit variant.)

### 6.5 Caching
The device performs no result caching of its own; it operates purely on the
supplied message, request, and node-local instance. Whether a `compute` result
is cached is governed by the surrounding AO-Core resolution machinery, not by
this device.

## 7. Security & authority

- **Sandboxing.** The module executes in a WASM sandbox: it can affect the world
  only through (a) its own linear memory and (b) the host functions it imports.
  All imports are mediated by the `import` bridge, so a module can reach AO-Core
  state/effects **only** through devices explicitly mounted under
  `<OUT>/stdlib/...`. A module importing a function with no mounted device is
  contained by the undefined-import stub (returns `0`, logs the call) rather than
  gaining ambient authority.
- **Ahead-of-time mode is gated.** `AOT` mode MUST be ignored (downgraded to
  interpreted `WASM`) unless the node operator has explicitly enabled it via node
  configuration. A request alone MUST NOT be able to force AOT.
- **No implicit signing or external I/O.** The device makes no commitments and
  performs no network calls itself. Any external effect must come from a mounted
  stdlib device.
- **Private handle non-leakage.** The live instance handle MUST NOT be resolvable
  as a public key, MUST NOT appear in `keys`/`get`, and MUST NOT be committed or
  content-addressed (it is node-local and meaningless off-node).
- **Failure containment.** Missing image → `wasm-init-error`; missing instance
  and snapshot on `normalize` → `no-wasm-instance-or-snapshot`. Unimplemented
  imports fail **open-but-logged** (stub), by design, so partial stdlibs do not
  abort otherwise-valid computations.

## 8. Errors

The device's two error conditions surface as the platform atoms `wasm_init_error`
and `no_wasm_instance_or_snapshot` — **underscored** (the Erlang/thrown-atom
convention; hyphenation is the *wire/key* convention, not the atom). They may be
**thrown** (propagating as a raised `{wasm_init_error, <detail>}`-style term)
rather than returned as `{error, _}`. The **condition** (which error) is the
conformance observable; the exact carrier (thrown vs `{error, _}`, atom vs
binary, detail tuple vs bare) is not pinned.

- `wasm_init_error` — `init` (or the `init` performed inside `normalize`) could
  obtain no image: `<IN>/image` was absent (or not a usable reference/message/
  binary) **and** no usable top-level `body` binary was present. The detail MAY
  report the `<IN>/image` path searched.
- `no_wasm_instance_or_snapshot` — `normalize` found neither a live instance at
  `<OUT>/instance` nor a snapshot body at `snapshot[/<device-key>]/body`, so it
  could neither continue nor restore.

Note on `compute`: a **missing function name is not an error** — `compute`
returns the (normalised) message unchanged. Only the two conditions above are
device-level errors.

## 9. Composition

- **Process execution stack.** The canonical use is as an execution device
  inside a process stack (`process@1.0` driving a stack device). The stack calls
  `init` on the first pass and `compute` on subsequent passes; `compute`'s own
  pass-gating (only pass 1, or no pass, executes) makes it safe to invoke on
  every pass. `snapshot`/`normalize` let the stack checkpoint and resume the
  process's WASM memory across messages; `terminate` releases the instance.
- **Prefixing for stacks.** Inside a process stack a typical configuration sets
  `input-prefix` to the process namespace (so the image is read from the process
  definition, e.g. `process/image`) and `output-prefix` to this device's slot
  name (so its instance, results, and stdlib live under, e.g., `wasm/...`). Both
  prefixes default to empty for standalone use. Implementations MUST resolve both
  prefixes through `message@1.0` and MUST apply them consistently to every
  private key (`<OUT>/instance`, `<OUT>/import-resolver`), public result key
  (`<OUT>/results/...`), stdlib mount (`<OUT>/stdlib/...`), and the
  undefined-calls log path.
- **Stdlib devices (host functions).** A host-function library is composed in by
  mounting a device message at `<OUT>/stdlib/<module>` (so its functions answer
  at `<OUT>/stdlib/<module>/<func>`). `wasi@1.0` is mounted this way at
  `<OUT>/stdlib/wasi_snapshot_preview1` to provide the WASI-preview-1 system
  interface; any device satisfying the host-function calling convention (§4c) can
  be mounted under any module name a module imports. The set of mounted libraries
  defines the module's ambient capabilities.
- **Call-target sourcing.** A driver may pass the function/parameters per-call in
  the request (`function`/`parameters`, or nested under `body/`), or fix them on
  the base message; `compute` prefers the request over the base (§4 `compute`).

## 10. Conformance (normative checklist)

An implementation MUST exhibit all of the following observable behaviours:

1. **Explicit-keys dispatch.** `init`, `compute`, `snapshot`, `normalize`,
   `terminate`, and `import` resolve to this device's behaviour; the
   `message@1.0` reserved keys (`keys`, `set`, `set-path`, `remove`, `id`, …)
   fall through to `message@1.0`. Resolving `.../instance` against this device
   MUST NOT return the live handle.
2. **Prefix resolution + defaults.** `input-prefix` and `output-prefix` are read
   via `message@1.0` and default to the empty binary. With empty prefixes, the
   instance resolves at private `instance`, results at `results/...`, stdlib at
   `stdlib/...`. With `output-prefix = X`, the same move to `X/instance`,
   `X/results/...`, `X/stdlib/...`. (Both standalone-empty and prefixed
   configurations MUST work.)
3. **Image — direct binary.** `init` with `<IN>/image` a raw WASM binary
   instantiates that module and installs a live instance and an
   `import-resolver` in the private section.
4. **Image — by reference.** `init` with `<IN>/image` a content ID reads the
   referenced message and uses its `body` bytes as the image.
5. **Image — wrapping message.** `init` with `<IN>/image` a message uses that
   message's `body` bytes as the image.
6. **Image — top-level body fallback.** `init` with no `<IN>/image` but a binary
   top-level `body` uses `body` as the image.
7. **Missing image error.** `init` with no obtainable image fails with
   `wasm-init-error`.
8. **Mode default + AOT gating.** Absent/`WASM` mode runs interpreted. `AOT` runs
   ahead-of-time **only** when the node enables it; otherwise `AOT` is silently
   downgraded to `WASM` (not an error).
9. **Basic execution + result keys.** After `init`, a `compute` with a
   `function` and `parameters` that name an exported function calls it and writes
   `<OUT>/results/type` and `<OUT>/results/output`, where `output` is the list of
   the function's return values. (E.g. a factorial-style export called with `5`
   yields `output = [120]` in some numeric form.)
10. **Call-target precedence.** `compute` selects the function from request
    `body/function`, else request `function`, else base `function`; and the
    parameters from request `body/parameters`, else request `parameters`, else
    base `parameters`. Absent parameters default to `[]`.
11. **No-function no-op.** `compute` with no function found anywhere returns the
    (normalised) message unchanged and writes **no** results (not an error).
12. **Pass gating.** `compute` executes only when `pass` is `1` or absent; for
    any other `pass` value it returns the message unchanged.
13. **Import bridge → `import` key.** When the running module calls an imported
    host function, the installed bridge resolves an `import` request carrying
    `module`, `func`, `args`, and `func-sig`, and feeds the module the
    `results` list while continuing with the returned `state`.
14. **Stdlib dispatch.** `import` repoints the call to
    `<OUT>/stdlib/<module>/<func>`, places the current base at
    `<OUT>/stdlib/<module>/state`, and resolves it; a device mounted there
    (returning `{ok, #{state, results}}`) services the call and its `results`
    reach the module. (Demonstrable end-to-end: a module that imports a custom
    function returns a value computed by a mounted device.)
15. **Undefined-import stub.** An import with no mounted device does not abort the
    module: the call returns `[0]` to the module and the request is appended to
    the public list at `<OUT>/results/undefined-calls` on the returned state.
16. **Snapshot shape.** `snapshot` returns `{ok, #{ <<"body">> => Binary }}`
    carrying an opaque serialisation of instance state.
17. **Restore via snapshot (cold start).** A base with no live instance but a
    `snapshot` (a `{body := Binary}` message under `snapshot`, optionally nested
    under `device-key`) computes correctly: `normalize`/`compute` boot a fresh
    instance and deserialise the snapshot before executing. The result MUST equal
    that of the warm message the snapshot was taken from. (E.g. compute → take
    snapshot → re-attach snapshot to a fresh base → compute yields the same
    `results/output`.)
18. **`normalize` strips `snapshot`.** After `normalize` (and hence after
    `compute`), the returned message has **no** `snapshot` key.
19. **Missing instance + snapshot error.** `normalize` with neither a live
    instance nor a findable snapshot body fails with
    `no-wasm-instance-or-snapshot`.
20. **Hashpath stability.** Instance boot/restore and host-call state placement
    add **no** hashpath step (a cold compute still ends in `/compute`). Identical
    public inputs ⇒ identical hashpaths and results (the live instance never leaks
    into the hashpath); a cold (public-`snapshot`) and a warm (private-instance)
    arrival converge on the **same content id** after `normalize` — they do not
    share a pre-`normalize` hashpath, since their public content differs (§6.3).
21. **Determinism.** Identical image + identical starting memory + identical
    function/parameters (+ deterministic stdlib) ⇒ identical `output`, `type`,
    and resulting snapshot.
22. **64-bit operands.** Memory addresses and sizes crossing the host boundary
    are handled as 64-bit integers without truncation (`memory64`).
23. **`terminate`.** `terminate` stops the instance and clears the private
    `<OUT>/instance` key.
24. **Private non-leakage.** The instance handle, import resolver, and read/write
    helpers never appear in `keys`/`get` output, are never committed, and never
    affect content IDs.

## 11. Out of scope

- The **internal representation** of the live instance, the import resolver, and
  the read/write helpers (any node-local form is permitted).
- The **byte layout of the snapshot** binary and of the serialised instance
  state. Snapshots need only round-trip within one implementation; cross-
  implementation snapshot interchange is NOT specified.
- The **WASM runtime/engine** chosen, the interpreter-vs-JIT/AOT internals, and
  the concrete numeric representation the engine uses for operands (only the
  observable `output`/`type` values and 64-bit operand fidelity are constrained).
- The **semantics of any specific host function / stdlib device** (e.g. the WASI
  filesystem/clock behaviour) — see `wasi@1.0`. This spec fixes only the bridge
  and calling convention by which such devices are reached.
- The **result-type tag's** internal vocabulary beyond "reproduced verbatim from
  the runtime".
- **Performance**, memory limits beyond the `memory64` addressing model, and
  storage strategy.

## Open questions

1. **Mode key case.** The mode input is compared exact-match against the
   binaries `WASM`/`AOT` (upper-case), whereas most node-message keys are
   lowercase-hyphenated. An implementer might reasonably expect `wasm`/`aot`.
   This spec pins the observed behaviour (upper-case literals, absent ⇒ `WASM`);
   whether lowercase aliases should also be accepted is unspecified.
2. **`results/type` vocabulary.** The exact set of result-type tags the runtime
   can emit, and their wire encoding, is not enumerated here (reproduced verbatim
   from the engine). Two engines could emit different tag spellings for the same
   logical result type; this spec does not normalise them.
3. **Numeric encoding of `output` values.** Whether return values are surfaced as
   floats, integers, or a tagged numeric form is left to the runtime/codec; the
   conformance examples (`120`, `4`, …) are given value-wise, not
   representation-wise.
4. **`snapshot` on a cold message.** `snapshot` assumes a live instance. Whether
   calling `snapshot` directly on a base that has only a literal `snapshot` (no
   live instance) should first `normalize` is not specified by the observed
   behaviour; callers are expected to `normalize`/`compute` first.
5. **Image source for restore.** When `normalize` restores from a snapshot it
   re-runs `init`, which re-reads the image from the **same base** (`<IN>/image`
   or top-level `body`). A snapshot is therefore only restorable alongside its
   originating image reference; snapshots do not embed the image. This coupling
   is implied by the behaviour but not independently stated as a requirement.
6. **`func-sig` format.** The signature string's grammar (how WASM value types
   are spelled) is engine-defined and passed through opaquely to stdlib devices;
   it is not specified here. A stdlib device and the engine must agree on it
   out of band.
