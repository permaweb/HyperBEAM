# `lua@5.3a` — Lua 5.3 execution device

- **Device name:** `lua@5.3a`
- **Depends-on:** `message@1.0` (message model, reserved keys, commitments, `id`/`committers`, TABM conversion via `structured@1.0`). Relates to `process@1.0` (the orchestrator that drives this device as a process `execution-device`), `json@1.0` (the JSON codec a script may invoke through the host library), and `stack@1.0` (this device may be one member of an execution stack). All `Depends-on` specs are provided to reimplementers.
- **Status:** Draft

## 1. Overview

`lua@5.3a` is an **execution device**: it evaluates a **Lua 5.3** program against
an AO-Core message and maps the program's return values back into an AO-Core
message. A device message names one or more Lua source modules; resolving a key
on the message **calls the Lua function of that same name**, passing the message
(and the request) in as Lua values, and returns the function's results as the
resolved value. The device maintains a long-lived Lua interpreter state in the
message's private area so that successive calls observe mutations made by
previous calls (mutable globals, accumulated state), and it can **serialise and
restore** that state, which lets it serve as the `execution-device` of a
`process@1.0` process (deterministic replay from a snapshot).

The device also installs a small **host library** (the `ao` table) into every
Lua state, giving scripts the ability to perform AO-Core resolutions, read/set
message keys, and emit host events — bridged through the same value mapping. A
configurable **sandbox** can render dangerous standard-library functions inert,
and a **device sandbox** can restrict which AO-Core devices a script may reach.

This spec pins: the execution-device key surface; how source modules are located
and loaded; exactly which Lua function is called with which arguments; the
bidirectional value mapping (AO message ⇄ Lua value), including how a returned
Lua table becomes a result message; the host-library surface; sandboxing and
determinism; snapshot/restore; and the error outcomes. The interpreter's internal
representation is out of scope.

## 2. Concepts & terminology

- **Lua state:** the live interpreter (globals, loaded functions, mutable
  values) for one device message. It is held in the message's **private area**
  under the key `state` (so `private`-namespaced; never serialised by `id`/`keys`,
  never committed). Its internal representation is out of scope; only its
  observable effects (function results, mutation persistence, snapshot
  round-trip) are normative.
- **Module / source:** a unit of Lua source code loaded into the state. A device
  message supplies one or more modules (§3, §4.`init`). Each module is identified
  by a **module reference** — a name used as the Lua chunk name (for stack traces)
  — derived per §4.`init`.
- **Lua content type:** a value of `content-type` equal to **`application/lua`**
  or **`text/x-lua`**. A message bearing one of these (in `body`/`data`) is a
  literal Lua source module.
- **Resolved function:** for a resolved key `K`, the Lua global function whose
  name is `K` (unless overridden by a `function` field — §4.`compute`). Calling
  the device for `K` runs that function.
- **Result message:** the AO-Core message produced by mapping a Lua function's
  return value(s) back into the message world (§5.2). When the script returns a
  table, that table **is** the result message (after decoding).
- **Host library (the `ao` table):** a set of host-implemented functions injected
  into the Lua global table `ao` (§4.6). Scripts call e.g. `ao.resolve{…}` to
  reach back into AO-Core.
- **Sandbox:** a set of standard-library Lua paths (e.g. `os.execute`) replaced
  by inert values so the script cannot reach them (§7.1).
- **Device sandbox:** an allow-list of AO-Core **device names** that host-library
  resolutions are permitted to use (§7.2).
- **Ordered-list table:** the §5.3 convention by which a Lua sequence (table with
  contiguous integer keys `1..N`) maps to/from an AO-Core list, and an
  associative table maps to/from a message.

The device's internal data structures (how the Lua state is held, the serialised
byte layout of a snapshot) are **out of scope**; only the value mappings, the
resolved-key contracts, the host-library surface, and the sandbox/determinism
rules are normative.

## 3. Device interface

- **Dispatch shape:** **default-handler.** The device installs a catch-all
  handler that answers **arbitrary** keys by calling the Lua function of that
  name (§4.`compute`). It additionally answers the named keys `init`, `snapshot`,
  `normalize`, and `functions` with their specific behaviour (§4). The following
  keys are **excluded** from the catch-all and fall through to the base
  `message@1.0` device, so they MUST NOT be captured by this device:

  `id`, `commitments`, `committers`, `keys`, `path`, `set`, `remove`, `verify`,
  `encode`, `decode`

  plus **every key already present in the base message** (a key that exists as
  data on the message is returned as data by `message@1.0`, not interpreted as a
  Lua function call). Consequently, resolving a key reaches the Lua interpreter
  **only when that key is neither a reserved key above nor an existing field of
  the message**. (`encode`/`decode` are AO-Core codec verbs; they are excluded so a
  script's same-named function cannot shadow the codec surface, and they resolve
  via the base `message@1.0` device like the other reserved keys — they are **not**
  answered by a Lua-device key of their own.) Implementation note: the catch-all's
  *static* reserved set is the fixed list above; the **"every existing field"**
  part is per-message — the device computes it from the base message's current keys
  (its `info` callback receives the base). The reference relies on this excluded-key
  set alone; an implementation MAY additionally guard in-handler. ⚠ Known platform
  subtlety (found defect): the substrate's exclusion check normalises the *looked-up*
  key and the *excluded-key list* with different atom functions (existing-only vs
  new), so a data key whose atom is **not already interned** can slip past a
  key-set-only exclusion into the catch-all; common field names (already-interned
  atoms) are unaffected.

- **Base message shape.** A `lua@5.3a` message carries the source and options:

  | Key | Type | Required | Meaning |
  |---|---|---|---|
  | `module` | id \| message \| list \| name-map | one of `module` / inline body | The Lua source module(s) to load (§4.`init`). |
  | `content-type` + `body` | binary + binary | (alt. to `module`) | If `content-type` is a Lua content type, `body` is treated as an inline literal Lua module. |
  | `sandbox` | boolean \| list \| map | no (default `false`) | Standard-library sandbox spec (§7.1). |
  | `device-sandbox` | list of device names | no (default unrestricted) | AO-Core device allow-list for host resolutions (§7.2). |
  | `function` | binary | no | Overrides the function name to call (else the resolved key) (§4.`compute`). |
  | `parameters` | list | no | Overrides the Lua call arguments (§4.`compute`). |

  Either a `module` field or an inline Lua `body` (with a Lua `content-type`)
  MUST be present; otherwise `init`/`compute` error (§8).

- **Request message shape (compute path).** When the catch-all handler runs, the
  request message MAY carry, taking precedence over the base-message fields:
  - `function` or `body/function` — the Lua function name to call.
  - `parameters` or `body/parameters` — the Lua call arguments.

- **Roles.**
  - **Direct invocation / hook.** A caller resolves a key (e.g. `.../~lua@5.3a/hello`)
    and gets back the result of the Lua `hello` function. (E.g. as an HTTP
    request hook, where the request becomes the Lua arguments.)
  - **Process execution device.** A `process@1.0` process sets
    `execution-device => lua@5.3a`; the orchestrator then drives this device's
    `init`, `compute`, `snapshot`, and `normalize` keys to evolve and persist the
    process's Lua state across scheduled messages (§9).
  - **Stack member.** The device may be one element of a `stack@1.0` execution
    stack (e.g. behind `json-iface@1.0`), where each pass resolves `init`/`compute`.

## 4. Resolved keys (normative)

### `init` (Base, Req → result) — load modules, build the Lua state

- **Reads:** `module`, `content-type` + `body`, `sandbox`, `device-sandbox` from
  the base message; node options.
- **Behaviour (MUST):**
  1. **Idempotence.** If the base message's private area already holds a `state`,
     return the base message unchanged (the state is already initialised).
  2. **Locate modules.** Build an ordered list of source modules from the base
     message (§4.1). At least one module MUST be found, else error (§8,
     `no Lua modules found`).
  3. **Create state.** Create a fresh Lua 5.3 interpreter state.
  4. **Load each module** into the state, **in order**, evaluating the chunk so
     its top-level definitions (functions, globals) take effect. Each module is
     loaded under its **module reference** as the chunk name (used in stack
     traces, §8). A later module's globals override an earlier module's globals of
     the same name (standard Lua last-definition-wins).
  5. **Apply the sandbox** (§7.1) per the `sandbox` field.
  6. **Install the host library** (§4.6) into the global `ao` table, honouring
     `device-sandbox` (§7.2).
  7. Store the resulting state in the base message's private area under `state`.
- **Returns:** `{ok, BaseMessage'}` with the Lua state attached privately.
- **Side effects:** module sources referenced **by id** are read from the
  content-addressed store (and, if the store does not hold them, may be fetched
  from the network data layer). No commitments are created. The state lives only
  in the (private) message; nothing is written to the store by `init` itself.

#### 4.1 Module location (normative)

The source modules are assembled as follows. Let **B** be the base message.

1. **Inline body module.** If B's `content-type` is a Lua content type
   (`application/lua` or `text/x-lua`), B itself contributes one module whose
   source is B's `body` (the literal Lua text).
2. **`module` field.** Read B's `module`:
   - **absent**, and no inline body module → **error** (§8): no modules found.
   - **a binary id** → treat as a single-element list `[id]` and recurse.
   - **a message** →
     - if the message's own `content-type` is a Lua content type, it is a
       **literal module message** (its source is in `body`/`data`); wrap as a
       one-element list and recurse;
     - otherwise it is a **name-map** (a map whose *values* are modules); take its
       values, in the message's own key order, as the module list and recurse.
   - **a list** → the list of modules; concatenate **after** any inline body
     module and load each element (§4.2).
3. **Order.** The inline body module (if any) comes **first**, followed by the
   `module`-field modules in list order. Modules are loaded in this order (step
   4.4).

#### 4.2 Loading a single module element (normative)

Each element of the module list is resolved to `{reference, source-binary}`:

- **An id** (a 32-byte / base64url message identifier): read the message it
  names from the store.
  - If the stored item is a **binary**, that binary is the source; the
    **reference** is the id string.
  - If the stored item is a **message**, recurse on it as a module message (next
    bullet).
  - If **not found**, error (§8): module `<id>` not found (HTTP-style `404`).
- **A message** (a literal module message): the source is the value of the
  **first present** of its `body` then `data` field. If neither is present,
  error (§8): module not loadable. The **reference** is the message's `name`
  field if present, else the message's content id.
- The **reference** is used only as the chunk name in stack traces; it does not
  affect which functions become callable (those are the global names defined by
  the source).

### `compute` (the default handler) (Key, Base, Req → result) — call a Lua function

This is the catch-all handler; it fires for any key `K` that is not an excluded
reserved key and not an existing field of the base message (§3).

- **Reads:**
  - The base message's private `state` (initialised on demand: if absent, `init`
    is performed first, §4.`init`).
  - The **function name** (below) and **call arguments** (below).
  - `function` / `parameters` overrides from `Req` and `Base`.
- **Behaviour (MUST):**
  1. **Ensure initialised.** If the base message has no private `state`, run the
     `init` procedure (§4.`init`) to build it. (Resolution-load any commitments
     attached to the request first, so a signed request's full content is
     available to the script.)
  2. **Determine the function name.** Take the **first present** of, in order:
     - `Req`'s `body/function`,
     - `Req`'s `function`,
     - `Base`'s `function`,
     - **default:** the resolved key `K` itself.
  3. **Determine the call arguments.** Take the **first present** list of, in
     order:
     - `Req`'s `body/parameters`,
     - `Req`'s `parameters`,
     - `Base`'s `parameters`,
     - **default:** the 3-element list `[ Base-without-private, Req, {} ]` — i.e.
       the base message (with its private area removed), the request message, and
       an empty message — in that order.

     **Read these selection fields (`function`, `parameters`) as plain message
     fields** — direct field access on `Req`/`Base` — **never resolved *through*
     this device.** Resolving them via AO-Core against a base that carries
     `device => lua@5.3a` re-enters this same catch-all handler (it tries to call a
     Lua function named `function`/`parameters`) and does **not terminate**. The
     identical rule applies to **every** config field this device reads off its own
     base — `module`, `sandbox`, `device-sandbox`, and the snapshot location read
     by `normalize` (below): they are **data reads**, never device resolutions.
  4. **Fully resolve** the arguments (force any lazy links to concrete values).
  5. **Encode** the arguments to Lua values (§5.1) and **call** the named global
     Lua function with them as its positional arguments, against the current
     state.
  6. **Map the return** (§5.2 / §4.3) into the resolved value, attaching the
     **post-call** Lua state back into the result message's private area (under
     `state`), so the next resolution observes this call's mutations.
- **Returns:** the mapped result (§4.3) — `{Status, ResultMessage}` or
  `{Status, NonMessageValue}` on success, or an error (§8) on a Lua/Erlang fault.
- **Side effects:** whatever the script performs through the host library (§4.6) —
  AO-Core resolutions, host events — plus updating the private Lua state in the
  returned message. No commitments are created by the call itself.

#### 4.3 Mapping the Lua return to a result (normative)

A Lua function may return multiple values. The device interprets the return as
follows (let the decoded values be the Lua returns mapped per §5.2):

1. **One return value `R`.** Treated as **status `ok`** with result `R` — i.e.
   equivalent to a function that returned `("ok", R)`.
2. **Two (or more) return values `(S, R)`.** The **first** value `S` is the
   **status** and the **second** value `R` is the **result**. Any further return
   values are ignored.
3. **Status coercion.** The status `S` (a Lua string, e.g. `"ok"`, `"error"`) is
   coerced to the AO-Core outcome tag: the resolution returns `{<status-atom>, …}`
   where `<status-atom>` is `S` as an atom (so `"ok"` → an `ok` outcome,
   `"error"` → an `error` outcome). Only `ok` denotes success to the surrounding
   resolution machinery; `error` is the conventional handled-failure tag. The
   coercion is **existing-atom only** (`list_to_existing_atom` semantics): the atom
   for `S` MUST already exist in the running node — true for the standard tags
   (`ok`, `error`) and any status already in use. A status string whose atom does
   **not** already exist is a **fault**, surfacing as an error outcome (§8), *not*
   a success carrying a novel tag. Scripts SHOULD restrict their status to
   `ok`/`error`. A script signals failure to AO-Core by returning
   `("error", <value>)`.
4. **Result shaping.**
   - If the decoded result `R` is a **message** (an associative Lua table), the
     resolved value is that message, with the device's private area (carrying the
     updated `state`) merged in under `priv`. (So returning the passed-in base
     table, mutated, persists those mutations as the new message *and* the Lua
     state.)
   - If the decoded result `R` is a **non-message** (a scalar, or a sequence/list),
     the resolved value is `R` as-is (no private area is attached, since it is not
     a map).
5. The empty Lua table `{}` decodes to an **empty message** (`#{}`), not an empty
   list (§5.3).

### `snapshot` (Base, Req → result) — serialise the Lua state

- **Reads:** the base message's private `state`.
- **Behaviour (MUST):** Serialise the live Lua state to an opaque binary that can
  later be restored by `normalize` (§4.`normalize`) into an equivalent state. The
  serialisation MUST round-trip: a state serialised by `snapshot` and restored by
  `normalize` MUST produce identical subsequent computation results.
- **Returns:** `{ok, #{ "body" => <serialised-state-binary> }}` — a message whose
  `body` is the serialised state. If no `state` is present, **error** (§8):
  cannot snapshot, state not initialised.
- **Side effects:** none external (the serialised bytes are returned to the
  caller; persisting them is the caller's/process orchestrator's job).

### `normalize` (Base, Req → result) — restore the Lua state from a snapshot

- **Reads:** the base message's private `state` (if any); else a serialised
  snapshot located in the base message (below); optional `device-key` in the base
  message.
- **Behaviour (MUST):**
  1. If the base message **already** has a private `state`, return it unchanged
     (nothing to restore).
  2. Otherwise locate the serialised snapshot at the message path
     **`snapshot` / [ `<device-key>` ] / `body`** — i.e. `snapshot/body`, or
     `snapshot/<device-key>/body` when a `device-key` field is set on the base
     (so a stack can disambiguate multiple devices' snapshots). **Traverse this
     path as plain nested data fields on the base — NOT an AO-Core resolution:**
     resolving `snapshot` through the base would invoke this device's own
     `snapshot` key (§4.`snapshot`), which errors when there is no live state,
     instead of reading the stored bytes. If no such serialised state is found,
     **error/throw** (§8): no Lua state snapshot found.
  3. Deserialise it into a live Lua state (the inverse of `snapshot`) and store it
     in the base message's private area under `state`.
- **Returns:** `{ok, BaseMessage'}` with the restored state attached privately.
- **Side effects:** none external.

### `functions` (Base, Req → result) — list global functions

- **Reads:** the base message's private `state`.
- **Behaviour:** Return the list of names of every global Lua value that is a
  **function** in the current state (the names bound in the global environment to
  callables). If no `state` is present, **error**: not found.
- **Returns:** `{ok, [Name, …]}` — a list of function-name binaries. Order is
  unspecified.
- **Side effects:** none.

### Reserved keys (delegated)

`id`, `commitments`, `committers`, `keys`, `path`, `set`, `remove`, `verify`,
`encode`, `decode`, and **any existing base-message field** are **not** answered
by the Lua interpreter; they resolve via the base `message@1.0` device (or are
returned as data). See §3.

### 4.6 Host library — the `ao` table (normative)

`init` installs a host-implemented library into the Lua **global `ao` table**.
Any pre-existing `ao` table set by the loaded source is preserved and the host
functions are merged **over** it (host functions win on name collision). Each
host function follows the same value mapping as the device: its Lua arguments are
decoded to AO values (§5.1/§5.2 inverse), the host operation runs, and its results
are encoded back to Lua values (§5.1). Each function MAY return multiple Lua
values; the **first** is conventionally a status and the **second** the result,
matching the AO-Core `{status, result}` convention.

The library MUST expose **exactly** these functions on the `ao` table:

#### `ao.resolve(...)` — perform an AO-Core resolution

- **Forms (MUST accept all):**
  - `ao.resolve(singleton)` — one argument that is a **message/table**: parse it
    as a single AO-Core request *singleton* (a message that encodes a full
    request path, e.g. `{ path = "/hello", hello = "Hello, AO world!" }`) and
    resolve it.
  - `ao.resolve(base, path)` — a base message/table and a **string path**:
    resolve `path` (which may contain `/`-separated segments) against `base`.
  - `ao.resolve(list)` — a **sequence** of messages: resolve them as a chained
    multi-message request (each element applied in turn).
- **`as` shorthand.** Anywhere a message argument is expected, a 3-element
  sequence `{ "as", <device-name>, <message> }` denotes "treat `<message>` as
  bound to device `<device-name>`" for that resolution step.
- **Behaviour:** Run the resolution under the device's **device-sandbox**
  allow-list (§7.2). On success return two Lua values `(status, result)`, where
  `status` is the resolution status (e.g. `"ok"`) and `result` is the resolved
  value (encoded to Lua per §5.1). On an internal resolution fault, return
  `("error", <error>)` rather than raising.
- **Side effects:** whatever the resolved path performs.

#### `ao.get(key, base)` — read a single key

- **Args:** a `key` (string) and a `base` message/table. (`key` and `base` MAY
  each use the `{ "as", device, msg }` shorthand.)
- **Behaviour:** Resolve `key` against `base` (the single-key `get` of the
  message model), returning the value (or the not-found/default value) as one Lua
  value. No `{ok,_}` wrapper.
- **Returns:** the value, encoded to Lua (§5.1).

#### `ao.set(base, key, value)` / `ao.set(base, values)` — write keys

- **Args (two forms):**
  - `ao.set(base, key, value)` — set a single `key` to `value` on `base`.
  - `ao.set(base, values)` — deep-merge a `values` message/table onto `base`.
- **Behaviour:** Apply the message model's `set` (deep-merge semantics, including
  commitment-invalidation when a committed key changes — see `message@1.0` §4
  `set`) and return the resulting message as one Lua value.
- **Returns:** the new message, encoded to Lua (§5.1).

#### `ao.event(event)` / `ao.event(group, event)` — emit a host event

- **Args:**
  - `ao.event(event)` — emit `event` to the default/global event group.
  - `ao.event(group, event)` — emit `event` to the named `group`. If `event` is a
    Lua sequence (list), it is treated as a tuple of event terms.
- **Behaviour:** Signal an event into the host's internal event/telemetry system.
  This is a **diagnostic side channel only**: it MUST NOT alter the computation's
  result and MUST be safe (and effectively a no-op observationally) on a host that
  does not surface events.
- **Returns:** the single Lua value `"ok"`.

No other names are added to the `ao` table by the host. (A script is free to
define additional `ao.*` members itself; those are not part of this spec.)

## 5. Data formats & encodings (normative)

The mapping between AO-Core values and Lua values is **bidirectional and total**
over the supported types. "Encode" = AO value → Lua value (arguments into a call,
host-function results into Lua). "Decode" = Lua value → AO value (call return
values, host-function arguments).

### 5.1 Scalar type mapping

| AO-Core value | Lua value (encode) | Lua value | AO-Core value (decode) |
|---|---|---|---|
| binary (string) | string | string | binary |
| integer | number | number (integral) | integer |
| float | number | number (fractional) | float |
| boolean `true`/`false` | boolean `true`/`false` | boolean | boolean |
| atom (other than `true`/`false`) | string (the atom's name) | — | — |

Rules (MUST):

- **Atoms encode to strings.** An AO-Core atom that is **not** `true`/`false`
  encodes to a Lua **string** of the atom's textual name. (Booleans `true`/`false`
  remain Lua booleans.) Lua has no atom type; decode never produces an atom from a
  bare string (it produces a binary).
- **Numbers.** Integers and floats both map to Lua numbers; on decode, a Lua
  number is mapped back to an AO-Core integer or float per the value (integral vs
  fractional). Implementations MUST preserve integer values exactly within Lua's
  number range.
- **Strings/binaries** map 1:1.

### 5.2 Composite type mapping (tables ⇄ messages/lists)

- **Encode (AO → Lua):**
  - A **message** (associative map) → a Lua **table** with the same string keys,
    values encoded recursively. **Exception:** a message that is an *ordered list*
    (§5.3) is first converted to its list form and encoded as a sequence.
  - A **list** → a Lua **sequence** (table with integer keys `1..N`), values
    encoded recursively.
- **Decode (Lua → AO):**
  - The **empty** Lua table `{}` → the **empty message** `#{}` (NOT an empty
    list).
  - A Lua table whose keys are present as **`{key, value}` pairs** (an associative
    table) → a **message**: a map from each (string) key to the decoded value.
  - After building the map, if it is an **ordered list** (§5.3) it is converted to
    an AO-Core **list**; otherwise it stays a message.
  - Values are decoded recursively.

### 5.3 Ordered-list table convention (normative)

A message/table is an **ordered list** iff, after removing its private area, its
`commitments`, and its type-annotation control field (named **`ao-types`**), its
**only** keys are the contiguous integer keys `1, 2, …, N` (in their string form
`"1"`, `"2"`, …) for some `N ≥ 1`, with no gaps and no other keys. Key comparison
is by the normalised (string) key. The **empty (`N = 0`) table is deliberately
excluded** — it is a message, not a list (see the final bullet). A library
predicate that classifies the empty map as an ordered list disagrees with this
rule on exactly that case and MUST be special-cased.

- Such a message is equivalent to the AO-Core list `[v1, v2, …, vN]` (1-based,
  in numeric order) and encodes to a Lua **sequence**.
- A Lua sequence (contiguous `1..N` integer keys) decodes to an AO-Core list.
- A table with **any** non-integer key, or a gap in the integer keys, is a
  **message**, not a list.
- **The empty table is a message, not a list** (the `N = 0` table decodes to
  `#{}`; §5.2). This is the one asymmetry an implementer MUST get right: empty
  Lua tables become empty messages.

### 5.4 Result-message private area

When a Lua call returns a **message**, the device attaches its updated Lua state
into that message's **private area** under the key `state`. Because the private
area is `private`-namespaced, it is invisible to `id`/`keys`/commitments and is
never serialised onto the wire; it survives in-memory across resolutions and is
the carrier the process orchestrator snapshots via §4.`snapshot`.

### 5.5 Identifier & content encodings

- Module **ids** are AO-Core message identifiers — **base64url** (43 chars for
  32-byte values), never hex — and name a content-addressed message in the store.
- Lua **source** is UTF-8 text (the literal program). Its content type, when
  inline, MUST be `application/lua` or `text/x-lua`.
- The **serialised snapshot** (`snapshot/body`) is an opaque binary; its byte
  layout is out of scope (only its round-trip property is normative, §4.`snapshot`).
  The snapshot captures the live interpreter state **including the host `ao`
  closures bound at install time**, so it is **node/build-local**: round-trip is
  guaranteed within one node/build, but a snapshot is **not** guaranteed portable
  across nodes running different module versions. (Restore re-uses the serialised
  closures; an implementation MAY instead re-install the host library on restore.)

## 6. Ordering, freshness & caching

- **Determinism.** A Lua call is deterministic **given** (a) the loaded modules,
  (b) the prior Lua state, and (c) the encoded arguments — provided the script
  does not consult non-deterministic host facilities. The default sandbox is
  designed to remove the obvious sources of non-determinism and side effects
  (`os.execute`, `os.exit`, `os.getenv`, `os.tmpname`, file/`io` access,
  `package`/`require`/`load*`/`dofile`) — see §7.1. A script that performs
  `ao.resolve` of a non-deterministic path, or that reads a non-sandboxed
  non-deterministic function, is **not** deterministic; determinism for process
  replay relies on the script restricting itself to deterministic inputs.
- **Module load order is significant:** the inline body module loads first, then
  `module`-field modules in list order; last definition of a global name wins
  (§4.1, §4.`init`). Two implementations MUST load in the same order to converge.
- **State persistence.** Within a live message the Lua state accumulates across
  resolutions (mutations persist); §5.4. Across a snapshot/restore the state is
  reconstructed exactly (§4.`snapshot`/`normalize`), enabling deterministic
  replay.
- **Result caching/freshness** of resolutions routed through this device is
  governed by node/substrate configuration, not by this device. The device itself
  performs no result caching.

## 7. Security & authority

### 7.1 Standard-library sandbox

The `sandbox` field on the base message controls which Lua standard-library
facilities are reachable. The device replaces each sandboxed path with an inert
value (so calling/indexing it yields that value instead of the real function),
addressing each function as a **path** through the global table (e.g.
`os.execute` is addressed as the path `_G.os.execute`).

- `sandbox = false` (**default**): **no** sandboxing — all standard library
  available.
- `sandbox = true`: apply the **default sandbox set**, replacing each of the
  following with the inert string value `"sandboxed"`:
  - the whole `io` table, the whole `file` table, the whole `package` table;
  - `os.execute`, `os.exit`, `os.getenv`, `os.remove`, `os.rename`, `os.tmpname`;
  - `loadfile`, `require`, `dofile`, `load`, `loadstring`.
  (These cover file/OS/process access and arbitrary-code loading.)
- `sandbox = <list of paths>`: sandbox **each** listed path, each replaced with
  the inert string `"sandboxed"`.
- `sandbox = <map of path → value>`: sandbox each path in the map, replacing it
  with the **specified** value (so a caller may stub a function with a chosen
  return rather than the default `"sandboxed"`).

A sandboxed function does not raise on access; it yields the inert value, so a
script that *calls* it typically fails downstream (e.g. attempting to call the
string `"sandboxed"`), surfacing as a Lua error (§8).

### 7.2 Device sandbox (host-resolution allow-list)

The `device-sandbox` field restricts which AO-Core **devices** a host-library
resolution (`ao.resolve`/`ao.get`/`ao.set`) may use:

- **Absent (default):** unrestricted — host resolutions may use any device the
  node offers.
- **A list of device names:** the intended allow-list is **exactly those names
  plus the minimal AO-Core set** `[structured@1.0]` (the one device every
  resolution needs to decode messages). The effective allow-list is
  `device-sandbox ∪ {structured@1.0}`, de-duplicated. An implementation computes
  this set and threads it into the environment its host resolutions run under.

**Enforcement status (normative — read carefully).** In the **current platform the
deny path is NOT realized.** The computed allow-list is threaded into the host
resolver's `Opts` (in HyperBEAM under the key `admissible-devices`), but **no
resolver consults it** — there is no device-load hook that rejects an
out-of-allow-list device (`admissible-devices` is *set* by the host library and
*read by nothing*). Consequently a host resolution that *requires* a forbidden
device currently **succeeds**, and `device-sandbox` blocks nothing yet.
Conformance therefore requires only that an implementation **compute and thread**
the allow-list (forward-compatibility); the **deny behaviour is a latent feature,
not an observable today**, and MUST NOT be relied on as a security boundary. A
correct implementation does **not** attempt to enforce by scanning `ao.resolve`'s
arguments — that is not the intended mechanism and gives false assurance. (Closing
this gap between `dev_lua_lib`'s intent and the substrate needs a kernel
device-load allow-list hook; tracked as a found defect, not fixed here.)

### 7.3 Trust & commitments

- The device creates **no commitments** and re-signs nothing. A function that
  returns the (mutated) base message returns it **without** commitments unless the
  caller re-commits; mutating a committed key via the result follows the message
  model's commitment-invalidation rules (a changed committed value drops
  commitments — see `message@1.0`).
- The Lua state lives only in the **private** area and is never exposed via
  `id`/`keys`/`committers` nor committed.
- **Authority for a process** (which messages a process trusts, e.g. an
  `authority`/`from-process` check) is **script-level policy**, not enforced by
  this device. The device faithfully passes the request (including its
  commitments, loaded into the arguments) to the script, which MAY accept or
  reject it (e.g. returning an error / a "not trusted" result). This device does
  not itself gate on signer identity.
- **Failure-closed on faults.** A Lua runtime error or an Erlang/host fault during
  a call yields an **error outcome** (§8), never a silently-empty success.

## 8. Errors

| Condition | Outcome |
|---|---|
| `init`/`compute` finds **no** modules (no `module` field and no inline Lua body) | Error: a message stating no Lua modules were found when preparing the environment. (Hyphenated atom form of the cause: `no-lua-modules-found`.) |
| Module **id not found** in the store | Error message with `status => 404` and a body naming the missing module id. |
| Module **message not loadable** (no `body`/`data` source) | Error message with `status => 404` and a body explaining a module must carry a `body` of code; the offending module is echoed. |
| **Lua runtime error** while calling the function (a `lua_error`) | `{error, #{ status => 500, body => <the script's error value>, trace => <decoded stack trace> }}`. `body` is the object the script passed to `error(...)`, **unwrapped** from the engine's structured error: a Lua engine reports `error(X)` as a wrapper (e.g. `{error_call, [X]}`), so `body` is the decoded `X`, **not** the whole wrapper (decoding the wrapper as a value is wrong and may itself crash). For an engine-internal fault that carries no user value (e.g. an illegal-index error), `body` is a readable rendering of that error. The `trace` is a normalised list of frames, each a message with `function`, `parameters` (a numbered message of the frame's decoded arguments), and a `line` (`"<file>:<n>"` when available). |
| **Status string with no existing atom** returned by the script (§4.3 rule 3) | The status coercion (`list_to_existing_atom`) faults → an error outcome (a `status => 500` Erlang/host error, as the row below), **not** a success carrying the novel tag. |
| **Erlang/host error** while running Lua (a non-Lua exception) | `{error, #{ status => 500, body => "Erlang error while running Lua: <reason>", trace => <formatted-trace-binary> }}`. |
| `snapshot` with **no** initialised state | Error: cannot snapshot Lua state — state not initialised. |
| `normalize` with **no** state and **no** locatable snapshot | Error/throw: no Lua state snapshot found. |
| `functions` with **no** initialised state | Error: not found. |

Notes:

- A script that wishes to signal a *handled* failure returns `("error", <value>)`
  from its function; this is **not** a Lua runtime error — it produces an `error`
  **outcome** whose value is `<value>` (§4.3 status coercion), distinct from the
  `status => 500` Lua-runtime-error message above.
- The status atom of a successful return is whatever string the script returns as
  its first value (default `ok` for a single-value return). An unrecognised status
  string still becomes that atom; only `ok` denotes success to the surrounding
  resolution machinery.

## 9. Composition

- **As a process `execution-device`.** A `process@1.0` process whose
  `execution-device` is `lua@5.3a` is driven by the orchestrator through this
  device's four execution keys:
  - **`init`** once, to load the script(s) and build the initial Lua state.
  - **`compute`** per scheduled message, to advance the state. The orchestrator
    resolves `compute` with the scheduled message as the request; the script's
    function (default the key, but a process typically routes through a named
    entry such as via `json-iface@1.0`) reads the message and returns the new
    process state. By convention a process result places its outputs under a
    `results` sub-message (e.g. `results/output/body`), and outgoing messages /
    spawns are conveyed there; this device does not itself impose that shape — the
    **script** (and any cooperating stack device) defines it.
  - **`snapshot`** to serialise the Lua state for persistence at snapshot
    intervals, and **`normalize`** to restore it (deterministic replay from a
    stored snapshot). The serialised state lives at `snapshot/body` (or
    `snapshot/<device-key>/body` inside a stack).
  This is the mechanism by which a "pure Lua process" computes: each message
  mutates Lua globals (or the returned state map), `now` materialises the latest
  state, and the snapshot/restore pair lets a fresh node replay deterministically.
- **As a stack member.** When placed in a `stack@1.0` execution stack (e.g. behind
  `json-iface@1.0`, which serialises the AO message to the JSON shape a legacy
  handler expects and reads its JSON result back), this device supplies the actual
  Lua `handle`/`compute` evaluation. The stack disambiguates this device's
  snapshot via `device-key`.
- **Device switching via returned values.** Because a returned **message** is an
  ordinary AO-Core message, if the script returns a map carrying its own `device`
  field, resolving the *next* key on that result uses **that** device — the normal
  AO-Core multi-hop chaining. A script can therefore hand control to another
  device by shaping its return.
- **Host-library re-entry.** A script may call back into AO-Core via
  `ao.resolve`/`ao.get`/`ao.set` (subject to the device sandbox, §7.2), composing
  this device's computation with arbitrary other devices' behaviour mid-call.
- **Direct hooks.** As an HTTP request hook (bound via the node's `on`/`request`
  configuration), an inbound request resolves a Lua function (default `request`)
  whose returned `{status, message}` becomes the HTTP response — e.g. returning
  `("ok", { body = { { body = "i like turtles" } } })`.

## 10. Conformance (normative checklist)

An implementation MUST exhibit all of the following. Each item is checkable by
constructing a `lua@5.3a` message and resolving a key (or by code review of an
unreachable/offline path).

1. **Dispatch.** A key that is neither a reserved key (`id`, `commitments`,
   `committers`, `keys`, `path`, `set`, `remove`, `verify`, `encode`, `decode`)
   nor an existing base-message field is answered by **calling the Lua global
   function of that name**. A reserved key, or a key that already exists as data
   on the message, resolves via `message@1.0` (not the interpreter).
2. **Inline module.** A message with `content-type = application/lua` (or
   `text/x-lua`) and a `body` of Lua source loads that source; resolving a key
   runs the matching function. (E.g. a function returning the table
   `{ a = 1, b = 2, c = 3 }` makes `<key>/b` resolve to `2`.)
3. **Module by id.** A `module` field that is an id loads the source stored under
   that id; a list of ids/messages loads each, **in order**; a name-map loads its
   values. A missing id yields a `404`-style error. Two later modules' same-named
   globals resolve to the **last** loaded definition.
4. **Function/parameter selection.** The called function name is the first present
   of request `body/function`, request `function`, base `function`, else the
   resolved key. The arguments are the first present of request `body/parameters`,
   request `parameters`, base `parameters`, else `[ base-without-private, request,
   {} ]`.
5. **Single vs multi return.** A single Lua return value maps to status `ok` with
   that value as result; a two-value return maps the first to the **status** and
   the second to the **result**; `("error", V)` yields an `error` outcome carrying
   `V`.
6. **Returned table → result message.** When a function returns an associative
   table, the resolved value is the corresponding message and the device's updated
   Lua state is attached privately; reading the mutated keys back reflects the
   script's writes (e.g. a function that sets `base.hello = req.name or "world"`
   and returns `base` makes the result's `hello` equal the request `name`, or
   `"world"`).
7. **State persistence across calls.** Successive resolutions observe earlier
   calls' mutations, because the post-call Lua state is carried in the **result
   message's** private area. This is **map-result-bound:** the function must return
   a **message** (e.g. an `inc` that mutates a Lua global *and returns the base
   table* makes a chain report `1`, `2`, …). A function returning a bare **scalar**
   attaches no private area (§4.3 rule 4), so the next resolution rebuilds from the
   original base and does **not** see the increment.
8. **Value mapping — scalars.** binary⇄string, integer/float⇄number,
   boolean⇄boolean; an AO **atom** (not `true`/`false`) encodes to its **name as a
   Lua string**; a Lua number decodes to an integer or float per its value.
9. **Value mapping — composites.** An associative table decodes to a message; a
   sequence (`1..N`) decodes to a list; the **empty table `{}` decodes to an empty
   message `#{}`, not a list**; an AO ordered-list message encodes to a Lua
   sequence.
10. **Default sandbox.** With `sandbox = true`, calling a sandboxed function
    (e.g. `os.getenv`) does not return its real result; the attempt surfaces as an
    error outcome. With `sandbox = false` (default) the standard library is
    available. A `sandbox` **map** replaces each named path with the caller's
    specified value; a `sandbox` **list** replaces each with `"sandboxed"`.
11. **Host library present.** Every loaded state exposes `ao.resolve`, `ao.get`,
    `ao.set`, `ao.event` on the global `ao` table, merged over any `ao` table the
    script defined.
12. **`ao.resolve` forms.** A single message singleton (e.g.
    `{ path = "/hello", hello = "…" }`) resolves and returns `(status, result)`;
    a `(base, path)` form resolves the path against the base; an internal
    resolution fault returns `("error", …)` rather than raising.
13. **`ao.get` / `ao.set`.** `ao.get(key, base)` returns the single resolved
    value; `ao.set(base, key, value)` / `ao.set(base, values)` returns the merged
    message (with `message@1.0` `set` semantics).
14. **`ao.event`.** Returns `"ok"` and does not alter the computation result; safe
    on a host with no event surface.
15. **Device sandbox (latent — see §7.2).** With `device-sandbox = [<names>]` the
    implementation computes the allow-list `names ∪ {structured@1.0}` and threads it
    into the host resolver's environment. The **deny path is not realized in the
    current platform** (no resolver consults the allow-list), so a resolution
    needing an out-of-list device currently still **succeeds**; this item checks
    only that the allow-list is computed and threaded (forward-compatibility), not
    that out-of-list devices are blocked. With no `device-sandbox`, resolutions are
    unrestricted.
16. **Snapshot/restore round-trip.** `snapshot` returns `{ok, #{ body => <bin> }}`
    capturing the state; after a process replays from that snapshot via
    `normalize`, subsequent computation yields identical results (a restored
    counter continues from its snapshotted value). `snapshot` with no state errors;
    `normalize` with neither live state nor a locatable `snapshot/body` errors.
17. **`functions`.** Returns the list of global function names in the current
    state; errors (`not found`) when no state is initialised.
18. **Error shapes.** A Lua runtime error yields
    `{error, #{ status => 500, body => <error>, trace => <frames> }}` with a
    decoded stack trace; an Erlang/host fault yields a `status => 500` error whose
    `body` begins `"Erlang error while running Lua: "`.
19. **Idempotent init.** Re-running `init` on a message that already holds a
    private state is a no-op (the state is not rebuilt).
20. **No commitments minted.** Resolving any Lua key creates no commitments; a
    returned message is uncommitted unless re-committed by the caller, and the
    private Lua state never appears in `id`/`keys`/`committers`.

## 11. Out of scope

- The **internal representation** of the Lua interpreter state, the message, and
  links; the exact byte layout of a serialised snapshot (only its round-trip
  property is normative).
- The **Lua 5.3 language semantics** themselves (assume a conforming Lua 5.3
  interpreter); this spec fixes only the *host* surface (loading, the value
  mapping, the `ao` library, the sandbox).
- The **`structured@1.0`** TABM byte layout and the **`json@1.0`** codec format
  (a script may call the latter via the host library; assume conforming codecs).
- The cryptography of commitments and any specific commitment device (only the
  observable `message@1.0` `set`/commitment-invalidation and `id`/`committers`
  surfaces are referenced).
- The surrounding **`process@1.0`** orchestration (scheduling, slot assignment,
  the snapshot-interval policy, `now`/`compute` slot mechanics) and
  **`stack@1.0`**/**`multipass@1.0`** pass mechanics — this spec fixes only what
  this device must do when its `init`/`compute`/`snapshot`/`normalize` keys are
  driven.
- The **shape of a process's `results`** message (outbox/spawns/output): that is
  defined by the script and any cooperating stack device (e.g. `json-iface@1.0`),
  not mandated by this device.
- Result caching/freshness policy, performance, and storage strategy.

## Open questions

- **Status string → outcome mapping (RESOLVED — §4.3 rule 3).** A two-value return
  uses the first value as the outcome status atom (so `"ok"` succeeds, `"error"`
  fails). The coercion is **existing-atom only**: a status whose atom is not already
  present in the node **faults** (error outcome, §8) rather than producing a novel
  success tag. Only `ok` denotes success; `error` is the handled-failure
  convention. Scripts SHOULD restrict themselves to `ok`/`error` — other strings
  are not a portable success channel.
- **Multi-module name collisions.** When several modules define the same global
  function, last-loaded wins, and a name-map's module order is the map's own key
  order. The exact iteration order of a name-map's keys (and thus which definition
  wins) should be pinned if name-maps are expected to carry colliding definitions;
  callers SHOULD avoid relying on it.
- **Default-argument private stripping.** The default call arguments are
  `[ base-without-private, request, {} ]` — the base message with its private area
  removed (so the Lua state is not re-passed as data), the full request, and an
  empty message. The semantics of the third (empty) argument as a conventional
  "opts" slot are by convention only; confirm whether any standardised options are
  ever passed there.
- **Determinism of `ao.resolve` for process replay.** The device permits a script
  to perform arbitrary AO-Core resolutions mid-computation. For a process used in
  deterministic replay, such resolutions must themselves be deterministic (or
  snapshotted). The boundary — which host resolutions are safe under replay — is a
  property of the **script and the device sandbox**, not enforced here; a stricter
  default (e.g. forbidding side-effecting devices under a process) may be worth
  mandating.
- **`text/x-lua` vs `application/lua`.** Both are accepted as Lua content types
  with identical effect. If a canonical content type is preferred on the wire,
  pin one; otherwise both remain valid indefinitely.
