# `patch@1.0` — the message-reorganisation / state-patch device

- **Device name:** `patch@1.0`
- **Depends-on:** `message@1.0` (the `set`/`remove`/`get` resolution semantics every patch operation is expressed in terms of). Its spec is provided to reimplementers.
- **Status:** Draft

## 1. Overview

`patch@1.0` reorganises a message by **moving a value from one path inside it to
another path**. It runs in two modes:

- **`all`** — move the entire value found at a *source* path to a *destination*
  path, removing it from the source.
- **`patches`** — scan the submessages found at the source path and move *only*
  the ones that look like patch requests (those carrying `method: PATCH` or
  `device: patch@1.0`) onto the destination, leaving the non-matching
  submessages where they were.

The primary use is **applying the patch submessages that an execution emits**
(e.g. the `PATCH` messages in a computation's outbox) onto the live state
message, so that downstream resolution sees the patched state at a stable path.
To that end the device also implements the standard execution-device hooks
(`init`, `compute`, `normalize`, `snapshot`), letting it sit as a stage in an
execution-stack pipeline where its `compute` key performs the `patches`
operation automatically.

The device performs **no cryptography, no caching, and no network access**. Each
resolved key is a pure transformation: given a base message and a request
message, it returns a new message. The internal representation of messages is
out of scope (see §11); all behaviour below is defined over the logical
key/value content of messages and over path resolution as defined by the
AO-Core substrate and the `message@1.0` spec.

## 2. Concepts & terminology

- **Execution message (`Base`):** the message the device is resolving — the
  first operand of resolution. In pipeline use this is the evolving state.
- **Request message (`Req`):** the request for the current resolution step. It
  carries the `path` (the key being resolved) and MAY carry the patch
  parameters (`from`/`to`/`patch-from`/`patch-to`) and, under `patches`, the
  source submessages themselves.
- **Source path (`from`):** the path whose value is read and moved out.
- **Destination path (`to`):** the path the moved value is written under.
- **Path:** a `/`-separated sequence of binary key segments (e.g.
  `results/outbox`). A path is resolved against a message by walking each
  segment in turn, exactly as the substrate resolves a `/`-delimited path. The
  root path is `/` (the whole message). Empty/absent path segments are trimmed,
  so `/results/outbox`, `results/outbox`, and `results/outbox/` denote the same
  path.
- **Relative-prefix (`base:` / `req:`):** an optional prefix on the **first
  segment** of the *source* path selecting which message the source path is
  resolved against (§3, §4.5). Destination paths are always resolved against the
  same message the source was taken from.
- **Patch submessage:** a submessage that is recognised as a patch under
  `patches` mode because it carries `method` equal to `PATCH` **or** `device`
  equal to `patch@1.0` (§4.4).
- **Patch parameters:** the four request/base keys `from`, `to`, `patch-from`,
  `patch-to` that locate the source and destination (§3).

## 3. Device interface

- **Dispatch shape:** **explicit-keys.** The device answers a fixed set of named
  keys and nothing else: `all`, `patches`, plus the execution-device hooks
  `init`, `compute`, `normalize`, `snapshot`. It does **not** install a
  default/catch-all handler; any key it does not name falls through to the base
  `message@1.0` device (so `set`, `keys`, `id`, `commit`, etc. behave as for any
  message). An implementation MUST NOT capture arbitrary keys.

- **Patch-parameter resolution.** The source and destination paths are located
  from four candidate keys, searched in a fixed order. For a parameter `X` that
  is either `from` or `to`, the device reads the **first** of the following that
  resolves to a present value, and uses `/` (the root) if none is present:

  1. `patch-X` of the **request** message (`Req`).
  2. `patch-X` of the **execution** message (`Base`).
  3. `X` of the **request** message (`Req`).
  4. `X` of the **execution** message (`Base`).

  i.e. for `from` the order is `Req.patch-from`, `Base.patch-from`,
  `Req.from`, `Base.from`; for `to` it is `Req.patch-to`, `Base.patch-to`,
  `Req.to`, `Base.to`. The first present value wins; a key that is absent (or
  resolves to "not found") is skipped. If **all four** are absent the default is
  the literal path `/`. Each candidate value is read using ordinary key
  resolution (case-insensitive per `message@1.0`); the resolved value is treated
  as a path.

- **Message shape(s).** The device imposes no required keys on `Base`. The
  optional inputs it reads are:

  | Key | Read from | Type / encoding | Default | Meaning |
  |---|---|---|---|---|
  | `patch-from` | `Req`, then `Base` | binary path | — | source path (highest priority) |
  | `from` | `Req`, then `Base` | binary path | `/` | source path (fallback) |
  | `patch-to` | `Req`, then `Base` | binary path | — | destination path (highest priority) |
  | `to` | `Req`, then `Base` | binary path | `/` | destination path (fallback) |
  | `path` | `Req` | binary | — | the key being resolved (selects `all` vs `patches` vs a hook) |

  All keys are lowercase, hyphenated, binary on the wire. Path values are
  binaries such as `<<"/results/outbox">>` or, for a relative source,
  `<<"req:/results/outbox/1">>`. There are no numeric or atom value
  requirements. Unknown/extra keys on `Base` or `Req` are ignored and preserved
  in the output unless explicitly moved.

## 4. Resolved keys (normative)

The two operational keys, `all` and `patches`, share one algorithm
parameterised by mode. §4.1–§4.5 describe that algorithm; §4.6 covers the
hooks.

### `all` (Base, Req → message)
- **Reads:** the resolved `from`/`to` paths (§3) and the value at the source
  path.
- **Behaviour:** Run the move algorithm (§4.1) in **`all`** mode: move the
  *entire* value found at the source path to the destination path, then clear
  the source path.
- **Returns:** `{ok, NewMessage}` — the input message with the source value
  relocated to the destination and the source path emptied (§4.3). On a missing
  source path, error `not_found` (§4.2, §8).
- **Side effects:** none. No cache or store write, no commitment, no network.

### `patches` (Base, Req → message)
- **Reads:** the resolved `from`/`to` paths (§3) and the submessages at the
  source path.
- **Behaviour:** Run the move algorithm (§4.1) in **`patches`** mode: of the
  submessages directly under the source path, move only those recognised as
  patches (§4.4) onto the destination; leave the non-patch submessages in place
  at the source.
- **Returns:** `{ok, NewMessage}`. On a missing source path, error `not_found`.
- **Side effects:** none.

### 4.1 The move algorithm (shared)

Given a `Mode` (`all` or `patches`), `Base`, and `Req`:

1. **Resolve the source path.** Read the raw source value by the search order in
   §3 (`patch-from`/`from`), defaulting to `/`. Determine the **source message**
   and the **effective source path** by inspecting the relative-prefix on the
   first path segment (§4.5). Call these `FromMsg` and `SourcePath`. If
   resolving the path yields the empty path, treat `SourcePath` as `/`.

2. **Resolve the destination path.** Read the raw destination value by the
   search order in §3 (`patch-to`/`to`), defaulting to `/`. Call it `ToPath`.
   The destination is **always** resolved against `FromMsg` (the same message
   the source was read from) — there is no relative-prefix parsing for the
   destination.

3. **Read the source value.** Resolve `SourcePath` against `FromMsg`. If this
   does not resolve to a present value, **abort** the whole operation and return
   error `not_found` (§4.2). Otherwise let `Source` be the resolved value (for
   `patches`, `Source` MUST be a message whose entries are the candidate
   submessages).

4. **Partition the source** (depends on `Mode`):
   - **`all`:** the value to write is the entire `Source`; the new value for the
     source path is "unset" (the path is removed entirely, §4.3).
   - **`patches`:** iterate the **direct entries** of `Source` (each entry is a
     key → submessage). For each submessage decide whether it is a **patch**
     (§4.4):
     - If it **is** a patch: collect it (with `commitments` and `Tags`
       **stripped**, §4.4) into the set of patches to write. It is removed from
       the source.
     - If it is **not** a patch: keep it in the *new source* under its entry
       key. It stays where it was.
     The new value for the source path is the message of all non-patch entries
     (possibly empty). **The entry keys of the *patch* submessages are not
     preserved** — see step 6: the patches' *contents* are merged together, not
     re-keyed by their source-entry key.

5. **Clear / rewrite the source path** (§4.3): produce a message equal to
   `FromMsg` but with `SourcePath` replaced by the new source value computed in
   step 4 (`all`: removed; `patches`: the non-patch remainder).

6. **Build the to-write value** (`patches` only): **deep-merge the *contents* of
   all collected patches into a single message**, additionally stripping each
   patch's `method` key (so the relocated content no longer advertises itself as
   a `PATCH`). Concretely, fold the patches by deep-merging each (method-stripped)
   patch **body** onto an accumulator that starts empty, using `message@1.0`
   deep-merge semantics governed by the **accumulator** (a plain message) — **not**
   by each patch body's own top-level `device`. A patch body's own `device` (e.g.
   `patch@1.0`) is carried through as ordinary data and governs nothing in this
   fold; it survives only for the step-7 destination write. (A *nested*
   device-typed sub-value inside a body still merges through its own device,
   exactly as `message@1.0` deep-merge does for any nested typed value.) The
   source-entry keys (`1`, `2`, …) are **discarded** and only the patch bodies
   survive, with later patches deep-merging onto earlier ones where keys coincide
   (§6 notes the collision case is unspecified). The result is the to-write value:
   a single merged message, **not** a map keyed by source-entry key. In `all` mode
   the to-write value is the entire `Source` **unchanged** (its structure and keys
   are preserved verbatim — this is the key difference from `patches`).

7. **Apply to the destination.** Write the to-write value onto the message from
   step 5 at `ToPath`, using ordinary message-`set` semantics (§4.7). The result
   of this write is the operation's output. Because (in `patches` mode) the
   to-write value is the merged patch *bodies*, writing to `/` lands a patch's
   `prices` map at `/prices/...` (not `/<entry-key>/prices/...`). In `all` mode the
   to-write value is the source **value** verbatim, so *that value's own top-level
   keys* land directly under `ToPath`: moving a numbered-entries map
   `#{1=>…, 2=>…}` to `/state` gives `/state/1/…`, `/state/2/…`, whereas moving a
   plain content map `#{prices=>…}` to `/` gives `/prices/…`. (There is no
   "entry-key" notion in `all` — it writes whatever value sits at the source path.)

8. **Return** `{ok, Result}`.

### 4.2 Missing source ⇒ `not_found`

If step 3 cannot resolve the source path to a present value, the operation MUST
return the tagged error tuple **`{error, not_found}`** and MUST NOT write anything
to the destination. (It MUST NOT return the *bare* atom `not_found`: a device
handler returning a bare atom is wrapped by the resolver as `{ok, not_found}` — a
*success* — which would violate the failure-closed contract of §7. The error must
be the `{error, not_found}` tuple, exactly as the underlying `message@1.0`/source
resolution surfaces a missing key.) This is the only path-level error the device
raises. Because
the default source path is `/` (always present), a missing-source error only
arises when an explicit `from`/`patch-from` names a non-existent path.

### 4.3 Source clearing

After the source value is taken, the device rewrites the source path so callers
do not observe stale data there:

- **`all`:** the source path is **removed** — after the operation, resolving the
  source path MUST report "not found". (Observable as: the key no longer exists,
  e.g. `input/zones` resolves to not-found.) *Degenerate root case:* when the
  source path is the root `/` (e.g. the default `from` with no parameters), there
  is no key to remove — the whole message cannot resolve to not-found — so root
  clearing is a no-op and the destination write governs the result. `all` is
  meant for naming a sub-path; the root-source case is not a normal use.
- **`patches`:** the source path is **replaced** by a message containing exactly
  the non-patch submessages (those that were *not* moved). Moved patch entries
  MUST NOT remain at the source. If every entry was a patch, the source path
  holds an empty message.

**This rewrite is a REPLACE, not a deep-merge.** The destination write (§4.7)
uses message-`set`'s deep-merge, which *adds and overwrites but never deletes* —
so simply `set`-ting the non-patch remainder (or `unset`) over the old source
would **leave the moved patch entries behind** and fail to clear the source. An
implementation MUST first **remove** the old value at the source path (or
otherwise overwrite it wholesale) and then write the new source value, so that
the moved entries genuinely disappear. (The conformance check is observable:
after a `patches`/`all` move, the moved entry — e.g. `results/outbox/1` — MUST
resolve to not-found, not to its original submessage.)

The clearing MUST happen on `FromMsg`. When `FromMsg` is the execution message,
the cleared/rewritten state is what propagates downstream. When `FromMsg` is the
request message (a `req:` source), the rewrite is applied to the request copy
that the device threads into the destination write.

**Consequence (surprising — the operation's output is built from `FromMsg`):**
both the source rewrite (step 5) *and* the destination write (step 7) happen on
`FromMsg`, and that rewritten `FromMsg` is the value returned. So for an
unprefixed or `base:` source the result is derived from `Base` (the usual case);
but for a **`req:` source the result is derived from the *request* message**, and
`Base` is **discarded entirely** from the output. A caller expecting the patched
*base* state back from a `req:`-sourced move will be surprised — `req:` is for the
case where the request itself carries the data to relocate and the request-derived
message is the intended result (§9). This also means device-accumulation at a
constant destination (§10.14) only persists across calls when the source is
unprefixed/`base:` (so `FromMsg` = the evolving `Base`).

### 4.4 Patch recognition and stripping (`patches` mode)

A direct submessage `M` under the source is a **patch** iff **either**:
- `M.method` equals the binary `PATCH` (exact, case-sensitive — `PATCH`, not
  `patch` or `Patch`); **or**
- `M.device` equals the binary `patch@1.0` (exact).

For every recognised patch, before it is written to the destination the device
MUST remove the following keys from it:
- `commitments` — the relocated content is merged into another message and must
  not carry a signature that no longer covers it;
- `Tags` — the capitalised legacy tag list;
- `method` — so the moved submessage is no longer itself treated as a `PATCH`.

`commitments` and `Tags` are stripped in the partition step; `method` is
stripped in the normalisation step. Keys other than these three are carried
through unchanged. Recognition reads `method`/`device` with exact binary
equality; a submessage missing both keys, or with a different `method`/`device`
value, is **not** a patch and is left at the source.

Non-message entries under the source (if any) are treated as non-patches.

### 4.5 Relative-prefix parsing (source only)

The raw source path's **first segment** MAY carry a prefix of the form
`PREFIX:REST`, split on the first `:`:

- `base:` → the source is resolved against the **execution** message (`Base`);
  the effective first segment is `REST`.
- `req:` → the source is resolved against the **request** message (`Req`); the
  effective first segment is `REST`.
- any other first segment (no recognised prefix, including a bare path or a
  first segment containing no `:`) → the source is resolved against the
  **execution** message (`Base`) and the path is used **unchanged**.

So `req:/results/outbox/1` reads `/results/outbox/1` from `Req`;
`base:/state` reads `/state` from `Base`; `/results/outbox` (no prefix) reads
from `Base`. The prefix is recognised **only** on the first segment and **only**
for the source; the destination is never prefix-parsed and is always applied to
the message the source was taken from (`FromMsg`).

Path segmentation: the raw path is split on `/` with empty segments trimmed, so
a leading `/` is insignificant for segmentation. The prefix is then detected by
splitting the **first segment** on its first `:`. **Two encodings both occur and
both MUST work:**

- **Prefix immediately followed by `/`** (e.g. `req:/results/outbox/1`): the `/`
  ends the first token, so segmentation yields first segment `req:` (the
  `:`-terminated token, with an empty remainder after the `:`) plus `results`,
  `outbox`, `1`. Splitting `req:` on `:` gives prefix `req` and an **empty**
  remainder; the empty remainder is **dropped**, so the effective source is
  `results/outbox/1` resolved against `Req`.
- **Prefix joined directly to the rest** (e.g. `base:state`): the whole
  `base:state` is a single first segment; splitting on `:` gives prefix `base`
  and remainder `state`, so the effective source is `state` against `Base`.

Algorithm: split the first segment on its first `:`; if the token before the `:`
is exactly `base` or `req`, select that message and use the (possibly empty)
remainder as the new first segment — an **empty remainder is dropped** — leaving
the rest of the segments unchanged; otherwise no prefix applies and the path is
used verbatim against `Base`. After prefix handling, a segment list that
re-serialises to empty MUST be treated as `/`.

### 4.6 Execution-device hooks

To compose in an execution stack, the device implements four standard hooks.
Each takes `(Base, Req, Opts)`:

- **`init`** — returns `{ok, Base}` unchanged (no initialisation state).
- **`normalize`** — returns `{ok, Base}` unchanged.
- **`snapshot`** — returns `{ok, Base}` unchanged.
- **`compute`** — performs the **`patches`** operation: identical to resolving
  the `patches` key (§4.1, `patches` mode). Returns `{ok, NewMessage}` or
  error `not_found`.

`init`/`normalize`/`snapshot` MUST be pure identity passthroughs of the base
message and MUST NOT read or move any patch parameters. Only `compute`,
`patches`, and `all` perform a move.

### 4.7 Destination application semantics

The destination write (step 7) uses the ordinary message-`set` semantics
defined by `message@1.0` (deep-merge by default): the to-write value is merged
into the message at `ToPath`. Consequently:

- Writing onto `/` (the default destination) merges the moved keys into the
  **top level** of the message.
- Writing onto a sub-path (e.g. `/state`) deep-merges the moved value into the
  existing submessage there, recursing into nested message values; scalars at a
  key are replaced.
- If a moved submessage carries its own `device` whose `set` differs from the
  default deep-merge (a custom set handler), the destination application honours
  that device's `set` behaviour for that submessage — patching is expressed
  through resolution, so a device-specific `set` participates. (This is the
  mechanism by which, e.g., a `trie@1.0`-typed sub-state accumulates entries
  across successive patches rather than being wholesale replaced.)

The device itself adds no keys to the result other than those produced by the
move and the source rewrite. In particular it does not write a `device` key, a
commitment, or any bookkeeping key.

## 5. Data formats & encodings

- All keys are **lowercase, hyphenated, binary** on the wire, with the sole
  exception of the legacy **`Tags`** key (capitalised) which the device only
  ever *strips*, never produces.
- Path values are binaries, `/`-delimited, with empty segments trimmed; the root
  is `/`. A path that serialises to empty MUST be normalised to `/`.
- The relative-prefix tokens are the exact lowercase binaries `base` and `req`,
  matched before the first `:` of the source path's first segment.
- Patch-recognition compares `method` against the exact binary `PATCH` and
  `device` against the exact binary `patch@1.0`. These comparisons are
  **case-sensitive**.
- The device computes **no IDs and no commitments**; there is nothing
  content-addressed here. (Any IDs/commitments observed on inputs or outputs are
  produced by `message@1.0`/the commitment device, not by `patch@1.0` — except
  that `patches` mode *removes* `commitments` from moved patch submessages.)

## 6. Ordering, freshness & caching

- **Determinism.** Both modes are deterministic functions of `(Base, Req)`. The
  output depends only on the resolved paths and the source value.
- **Entry ordering (`patches`).** The partition iterates the source's entries;
  the moved patches are merged onto the destination. The result of merging the
  moved set onto the destination MUST be independent of entry iteration order
  for entries with **distinct** keys (a plain map merge). The device does not
  define a tie-break for two source entries that would write the **same**
  destination key after stripping; such collisions are not expected in normal
  use (outbox entries are distinct numbered keys) and the outcome is whatever
  the underlying `set` merge yields — implementations MUST NOT rely on a
  particular winner.
- **Freshness / caching.** The device performs no caching and reads no clock. It
  neither writes to nor reads from any cache or store. Result-caching of a
  resolution that invokes this device is governed entirely by the substrate's
  generic result-cache behaviour and node configuration, not by this device.
- **Mutability at constant path.** Because the device relocates state to a
  constant destination path (e.g. `/state`) that changes value as new patches
  are applied across successive computations, any caller that caches resolution
  results by path is responsible for freshness; the device does not and cannot
  opt its own results out of result caching.

## 7. Security & authority

- **No authority checks.** The device does not verify commitments, does not
  check committers, and does not require the request or any submessage to be
  signed. Any caller that can resolve the device can invoke a move. Authority,
  if required, MUST be enforced by the surrounding pipeline (e.g. an upstream
  device that only emits trusted patches), not by `patch@1.0`.
- **Commitment hygiene.** When `patches` mode relocates a patch submessage it
  **strips that submessage's `commitments`** (and `Tags`) before merging it
  elsewhere, so a signature is never carried onto content it no longer covers.
  The destination write itself uses message-`set` semantics, which independently
  drop commitments on a committed message whose committed keys change (per
  `message@1.0`); the device does not re-sign anything.
- **Failure mode.** The single error condition (missing source path) is
  **failure-closed for the write**: on `not_found` no destination write occurs
  and the operation returns the error rather than a partially-patched message.
- **No external effects.** No network, cache, or store access — the device
  cannot exfiltrate data or mutate shared state outside the message it returns.

## 8. Errors

- **`{error, not_found}`** — the resolved source path (after relative-prefix
  handling) does not resolve to a present value in the source message (§4.2).
  Returned as the **tagged tuple `{error, not_found}`** (NOT the bare atom, which
  the resolver would wrap as a spurious `{ok, not_found}` success); no destination
  write is performed. This is the only error the device originates.
- Errors raised by the underlying resolution/`set` of the destination write, or
  by resolving a malformed path, propagate unchanged from the substrate; the
  device adds no wrapping.

## 9. Composition

- **As a pipeline stage.** Because the device implements `init`/`compute`/
  `normalize`/`snapshot`, it can be a stage in an execution stack. Its `compute`
  runs `patches`, so placing `patch@1.0` after a computation stage automatically
  lifts that stage's emitted `PATCH` submessages (typically at
  `/results/outbox`) onto the live state. A common configuration sets
  `patch-from = /results/outbox` and `patch-to = /` (or `/state`), with the
  parameters carried on either the base (execution) message or the per-step
  request.
- **`all` vs `patches`.** Use `all` to move a whole subtree verbatim regardless
  of its contents; use `patches` (the `compute` default) to selectively lift
  only patch-shaped submessages and leave other outbox entries (e.g. `GET`
  results) untouched.
- **Relative sources.** `req:`-prefixed sources let a single base device act on
  patches supplied in the request (e.g. resolving a base state with a request
  that carries `results/outbox` and `patch-from = req:/results/outbox/1`),
  decoupling where the patch data lives from where the state lives.
- **Device-switching in moved values.** Because the destination write goes
  through resolution, a moved submessage's own `device` governs how it is
  merged (§4.7). This lets specialised state sub-trees (e.g. trie-typed
  balances) accumulate correctly under repeated patching.
- **Fall-through keys.** Being an explicit-keys device, `patch@1.0` inherits all
  reserved message operations (`set`, `keys`, `id`, `commit`, `verify`, …) from
  `message@1.0`; resolving any of those on a `patch@1.0` message behaves exactly
  as for any message.

## 10. Conformance (normative checklist)

An implementation MUST exhibit all of the following, each checkable by resolving
the device against constructed messages:

1. The device answers exactly the keys `all`, `patches`, `init`, `compute`,
   `normalize`, `snapshot`; every other key falls through to `message@1.0`
   (e.g. `set`/`keys`/`id` behave as for a plain message).
2. Patch-parameter search order is `Req.patch-X`, `Base.patch-X`, `Req.X`,
   `Base.X` (X ∈ {`from`,`to`}); the first present value wins; if none is
   present the path defaults to `/`.
3. `all` moves the entire value at the source path to the destination path and
   **removes** the source path: after the operation the source path resolves to
   not-found, and the moved value is readable under the destination path.
4. `patches`/`compute` move only submessages whose `method` equals `PATCH`
   **or** whose `device` equals `patch@1.0`; submessages with neither (e.g. a
   `GET` result) remain at the source and are NOT written to the destination.
5. Recognition of `method`/`device` is exact and case-sensitive: `PATCH`
   matches, `patch`/`Patch` do not; `patch@1.0` matches exactly.
6. Each moved patch submessage has its `commitments`, `Tags`, and `method` keys
   removed before being merged into the destination; its other keys are
   preserved. A signed patch submessage thus contributes its content without its
   signature.
7. A missing source path (an explicit `from`/`patch-from` naming a non-existent
   path) returns the tagged tuple `{error, not_found}` (not the bare atom — §4.2,
   §8) and performs no destination write.
8. The destination defaults to `/`: with no `to`/`patch-to`, moved keys merge
   into the top level of the message (e.g. a moved `prices` map becomes readable
   at `/prices/...`).
9. Writing to a sub-path destination (e.g. `/state`) **deep-merges** the moved
   value into the existing submessage there (existing sibling keys at that
   sub-path are preserved; nested maps merge recursively).
10. A source path bearing a `req:` prefix reads the source from the **request**
    message; a `base:` prefix (or no prefix) reads from the **execution**
    message. The prefix is honoured only on the source's first segment, never on
    the destination.
11. `init`, `normalize`, and `snapshot` return the base message unchanged
    (`{ok, Base}`); only `compute`/`patches`/`all` move data.
12. The device performs no cache write, no store write, no commitment creation,
    and no network access for any key.
13. Leading-slash/trailing-slash variants of a path are equivalent (segments are
    trimmed); a path that serialises to empty is treated as `/` (the whole
    message).
14. When a moved submessage carries its own `device`, the destination merge
    honours that device's `set` semantics rather than forcing a plain replace
    (observable: a custom-typed sub-state accumulates across successive
    `compute` calls rather than overwriting).

## 11. Out of scope

- The internal representation of messages, submessages, and paths.
- The exact wire/byte layout of any value (delegated to the substrate and to the
  `message@1.0`/structured encodings).
- The cryptographic behaviour of commitments (the device only *strips*
  `commitments`/`Tags` from moved patches; it neither creates nor verifies any).
- The substrate's generic result-cache and node configuration (which govern
  freshness of cached resolutions, not this device's logic).
- Performance, storage strategy, and concurrency.

## Open questions

- **Destination relative-prefix.** The destination path is documented as "always
  relative to the request", yet in the move algorithm the destination is applied
  to `FromMsg` (the message the source was read from), which is `Base` for an
  unprefixed or `base:` source and `Req` only for a `req:` source. The
  destination is **not** prefix-parsed. The spec above pins the observable
  behaviour (destination applies to `FromMsg`, no destination prefix parsing) as
  derived from the source; the prose-vs-behaviour mismatch in the device's own
  documentation is noted but does not change the contract.
- **Same-destination-key collisions (`patches`).** When two distinct source
  entries would, after stripping, target the **same** destination key, no
  deterministic tie-break is defined; the outcome is whatever the underlying
  merge yields. Normal outbox usage has distinct numbered keys, so this is not
  expected to arise, but it is left unspecified rather than invented here.
