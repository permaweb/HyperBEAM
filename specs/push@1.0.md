# `push@1.0` — recursive outbox message-passing driver

- **Device name:** `push@1.0`
- **Depends-on:** `message@1.0` (id / commit / verify / set), `process@1.0` (the `schedule`, `compute`, `slot` surface it drives), `scheduler@1.0` (the default scheduler device whose `schedule` assigns slots and whose `slot` numbering this device relies on). All three specs are provided to reimplementers.
- **Status:** Draft

## 1. Overview

`push@1.0` is the **message-passing engine of AO**. A process, when it computes
a slot, may emit messages into an **outbox** (each addressed to a `target`
process). `push@1.0` takes one such computed slot, reads its outbox, and — for
every outbox entry — **schedules** that message onto its target process (so the
target's next slot advances) and then **recursively computes and pushes** the
target's resulting slot. The recursion fans out across the whole graph of
processes reachable from the originating message until no further outbox
messages are produced, a depth bound is hit, or a target cannot be reached.

It is the bridge between three lower-level capabilities: `scheduler@1.0` decides
*what slot a message becomes* on a process; `process@1.0/compute` decides *what
that slot produces* (its outbox); and `push@1.0` closes the loop by feeding each
produced outbox message back into a target's schedule and driving its compute.
The device returns a **trace tree** mirroring the shape of the message graph it
walked, so a caller can observe (to a configurable depth) every message that was
delivered and where it led.

This device performs **real side effects**: it signs messages, writes them to
the message cache, schedules them onto processes (advancing scheduler state),
and — when a target lives on another node — issues outbound HTTP. It is not a
pure query.

## 2. Concepts & terminology

- **Slot:** an integer index of an assigned message on a process's schedule, as
  defined by `scheduler@1.0`. Computing a slot (via `process@1.0/compute`)
  produces that slot's result, including its outbox.
- **Outbox:** the set of messages a process emitted while computing one slot. It
  is exposed as a sub-map of the slot's **results** under the key `outbox`
  (§5.1). Each outbox entry is itself a message addressed to a `target`.
- **Outbox entry / outbox key:** one message in the outbox, identified by its
  **outbox key** — a 1-based positional index (`1`, `2`, …) recording the order
  in which the process emitted the messages. The key is a binary decimal string
  on the wire.
- **Target:** the process a given outbox message is to be delivered to,
  identified by the message's `target` field — a base64url, 43-character process
  id, **optionally** followed by a query string introduced by `?` or `&` (§5.3).
- **Hint:** the optional query-string portion of a `target` (everything after
  the first `?`/`&`). It MAY carry a `hint=<node>` locator naming a node that
  schedules the target. The bare process id (everything before the separator) is
  the **target id**.
- **Push (verb):** the act of taking one computed slot of a process, scheduling
  each of its outbox messages onto their targets, and recursively pushing each
  target's resulting slot.
- **Source process / source slot:** the process and slot whose outbox is being
  pushed at a given level of the recursion.
- **Trace / result tree:** the value `push` returns — a map mirroring the outbox
  it processed, each entry recording where that outbox message was delivered and
  (recursively) what *its* push produced (§5.5).
- **Provenance keys (`from-*`):** keys this device attaches to each scheduled
  message identifying the source it came from (§5.4).

The device defines no content-addressed artefacts of its own beyond reusing
`message@1.0` ids; all ids it emits in the trace are ordinary message ids.

## 3. Device interface

- **Dispatch shape:** **explicit-keys.** The device answers exactly one resolved
  key: **`push`**. It is not a default-handler device and imposes no `excludes`;
  message-manipulation keys (`set`, `keys`, …) resolve through the base
  `message@1.0` device as for any message.

- **Base message.** `push` operates on a **process** as its base. The base is
  treated as a `process@1.0` message (its `device` is interpreted as
  `process@1.0` for the schedule/compute/push delegations this device performs).
  The base MUST be a committed, verifiable process definition: deriving the
  source process id requires the process to **verify** and to carry **at least
  one signer** (§7). If the base instead carries the process under a nested
  `process` key, that nested value is the canonical process (§5.6).

- **Request message (`Req`).** The operation is selected by which of the
  following the request carries:

  | key | type | meaning |
  |---|---|---|
  | `slot` | integer (or decimal binary) | Push the **already-scheduled** message at this slot of the base process. Selects the *push existing slot* mode. |
  | `body` | message | Schedule this message (or process) onto the base **first**, then push the resulting slot. Selects the *schedule-then-push* mode. Used when `slot` is absent. |
  | `result-depth` | non-negative integer | How deep the **full** computed result is inlined into the trace. Default `1`. Decrements by 1 at each recursion level (§5.5, §6.2). |
  | `max-depth` | non-negative integer (or decimal binary) | Bounds the **recursive fan-out**. Absent ⇒ unbounded. `0` ⇒ schedule each target but do not drive its compute/push. `N>0` ⇒ recurse, inner push inheriting `N−1` (§4, §6.3). |
  | `async` | boolean (`true`/`false`) | When `true`, run the push detached and return immediately; otherwise run synchronously and return the trace. Default `false`. MAY also be read from the base process (§4.1). |

  Keys other than those the device consumes are **carried through** in two
  places: (a) when scheduling an outbox message, unrelated request keys do not
  ride along (the scheduled message is built from the outbox entry, §5.4); but
  (b) the post-compute hook that triggers a push (§9) forwards the originating
  request's payload keys (e.g. `result-depth`, `async`) into the push request.

## 4. Resolved keys (normative)

### `push`

Push a process's outbox (the recursive driver).

**Source authentication (failure-closed — §7).** A push proceeds only for an
authenticated source: deriving the base process's signed id REQUIRES it to
**verify** and carry **≥1 signer**, so an unverified or signerless base fails the
whole push (`process-not-verified` / `process-has-no-signers`) before any message
is scheduled. This holds in **both** request modes — it is a property of id
derivation, inherited by any push that delegates to the process device, not a
separate pre-dispatch gate.

The two request modes:

#### Mode A — push an existing slot (`slot` present)
- **Reads:** `slot` from `Req` (the source slot); `result-depth`, `max-depth`,
  `async` from `Req` (or base, for `async`); the base process.
- **Behaviour:** Run the **push loop** (§4.2) for the base process at `slot`.

#### Mode B — schedule then push (`slot` absent)
- **Reads:** `body` from `Req`; same control keys as Mode A.
- **Behaviour:**
  1. **Schedule the initial message.** POST the request to the base process's
     `schedule` (i.e. resolve `schedule` against the base with `method = POST`
     and the request's `body`). This assigns the message a slot and returns an
     **assignment**. (If scheduling redirects — status `307` — follow it to the
     scheduler named by `location`; if it returns a wrong-format error — status
     `422` — fail with that error. See §5.7.)
  2. **Branch on the scheduled object's type.** Read the assignment's `body`'s
     `type` (§5.8):
     - If the scheduled object is a **`Process`** (type `Process`): this was a
       process **initialisation**, not a message. Return the assignment as-is
       (`{ok, Assignment}`) and **do not** push. (A process has no outbox to
       drive on creation.)
     - Otherwise (a **message**): take the assignment's `slot` and run the push
       loop (§4.2) for the base process at that slot — i.e. continue exactly as
       Mode A.

In both modes the loop result is returned per §4.1 (sync vs async).

#### 4.1 Sync vs async dispatch
- Determine **async** as: the value of `async` taken from the **request** first,
  else from the **base process**, default `false`; coerced to boolean (the
  binary `true` or boolean `true` ⇒ async).
- If **async**: run the push loop in a **detached** activity and return
  immediately (the resolution does not wait for the loop; its return value is
  an opaque handle to the detached activity — its exact form is out of scope).
- If **sync** (default): run the push loop inline and return its `{ok, Trace}`
  (or `{error, …}`).

#### 4.2 The push loop (one level)

Given a source process `P` and source slot `S`, with inherited `result-depth`
`D` (default 1) and `max-depth` `M` (default unbounded):

1. **Identify the source.** Derive (§5.6): the source process **signed id** `ID`
   (over its signed commitments); the **uncommitted id** `UNCOMMITTED`; and the
   **base id** `BASE`. Deriving `ID` is itself the source-auth gate (§7) — it
   REQUIRES `P` to verify and carry ≥1 signer, so an unauthenticated source fails
   here, before any message is scheduled. These become provenance fields (§5.4).
   Also read the source process's `scheduler` and `authority` values for
   provenance. (The recursive step re-views each target as a `process@1.0`
   message — §4.5, §9 — so every downstream source is re-authenticated the same
   way when its id is derived.)

2. **Compute the slot's outbox.** Resolve `compute/results` on `P` (as a
   `process@1.0` message) for slot `S`, with hashpath updating disabled. This
   yields the slot's **results** message. Read its `outbox` (key `outbox`,
   default empty message). Computation MUST be resilient: if the compute step
   **raises**, the loop MUST NOT crash — it returns a structured error result
   for this slot (§8, `compute-failed`) carrying the process id, slot, and the
   failure reason, and the loop terminates at this level with that error.

3. **Empty outbox ⇒ leaf.** If the outbox is **empty** (an empty message, or a
   message carrying only a private section), the slot produced no further
   messages. Return the **leaf trace**: `{ok, R}` where `R` contains
   `slot = S`, `process = ID`, and — if `D > 0` — the full computed result
   merged in (§5.5). Recursion stops here.

4. **Non-empty outbox ⇒ fan out.** Normalise the outbox (lower-case its keys,
   strip any private section) and, for **each** outbox entry `K ⇒ Entry`
   (§4.3), in the outbox's key order:
   - Determine `Entry`'s `target` (§5.3). If the entry has **no** `target`, it
     contributes a **no-target** placeholder to the trace — a per-entry **404**
     placeholder (`status = 404`, `outbox-index`, `reason`; §8), NOT an empty
     `{}`. Nothing is scheduled and no top-level error is raised (the push as a
     whole still succeeds); the loop continues with the next entry.
   - **Maybe evaluate** the entry first (§4.4): if it carries a `resolve` key,
     resolve that path to obtain the message to schedule; otherwise the entry is
     the message to schedule unchanged. An evaluation failure is recorded as an
     error placeholder for that entry but does not abort the others.
   - **Load the target process.** Look up the target id in the message cache.
     - If **not found locally**, the entry contributes a `target-not-found`
       placeholder to the trace (§8) and the loop continues. (Remote delivery is
       still possible via routing in the recursion step for *already-scheduled*
       slots, but a target process that cannot be loaded at all here yields the
       placeholder.)
     - If **found**, **deliver and recurse** (§4.5) for this entry, producing
       its sub-trace.
   - The entry's sub-trace (or placeholder) is stored under key `K` in the
     result map (§5.5).

5. **Assemble.** Return `{ok, Trace}` where `Trace` is the per-entry result map
   from step 4, **merged with** `slot = S`, `process = ID`, and — if `D > 0` —
   the full computed result (§5.5).

#### 4.3 Reading the outbox
- The outbox is read as the `outbox` key of the slot's results message
  (default: empty).
- Its entries are a map keyed by **1-based positional indices** (`1`, `2`, …) —
  binary decimal strings — preserving emission order. An implementation MUST
  preserve each entry under its original outbox key in the trace (the trace's
  per-entry keys mirror the outbox keys).
- Before iterating, the outbox is normalised: keys lower-cased, any private
  section removed. Entries that are **not** messages with a `target` (e.g. a
  scalar, or a message lacking `target`) MUST be handled by the no-target /
  not-available placeholder path (§8) rather than scheduled.

#### 4.4 Maybe-evaluate an outbox entry (`resolve`)
- **Reads:** the entry; its optional `resolve` key.
- **Behaviour:** If the entry has **no** `resolve` key, the message to schedule
  is the entry unchanged.
  If it has `resolve = Path`:
  1. Build a request from the entry **without** its `target` key (so the target
     is not confused with functional fields of the evaluation), with `path` set
     to `Path`.
  2. Resolve it, forcing a message result.
  3. On success, take the evaluation's result message and **re-attach** the
     entry's original `target` to it; that becomes the message to schedule.
  4. On failure, the entry is recorded as an error placeholder (§8,
     `resolve-error`): a message with `resolve = error`, `status = 400`, the
     `outbox-index = K`, the failure `reason`, and the original entry as
     `source`. The other entries are unaffected.
- This lets a process emit an outbox entry that says "resolve this AO-Core path,
  then schedule the result on `target`", rather than scheduling a literal
  payload.

#### 4.5 Deliver one entry and recurse
Given the loaded target process `T`, the message-to-schedule `Msg` (target id
`TARGET`), and the inherited controls:

1. **Schedule onto the target.** Schedule `Msg` onto `T` (§5.4): augment it with
   provenance + protocol keys, apply the target's security policy (sign it,
   §7), verify it, write it to the message cache, then POST it to `T`'s
   `schedule`. This **assigns the message a slot** on the target (the target's
   slot count advances). Let `NEXTSLOT` be the assignment's slot. This step
   **always runs** for an entry with a target — even when recursion is later
   skipped — so the message is durably enqueued on the target (§6.3).
   - Scheduling failures (status not `200`, or an error) are recorded as an
     error placeholder for this entry (§8, `schedule-error`) and the loop
     continues with remaining entries.
   - A `307` redirect ⇒ the target is scheduled elsewhere: re-sign the
     normalised message and POST to the redirect `location` (§5.7). A `422`
     wrong-format ⇒ codec downgrade and retry (§5.9).

2. **Recurse (drive the target's slot).** Subject to the depth bound (§6.3):
   - If `max-depth = 0` for this level: **skip** the recursion. The entry's
     `resulted-in` is the binary marker `<<"skipped">>` (§5.5). The message is
     already enqueued on the target (step 1); the target's own later push
     (e.g. a cron tick or an explicit caller) will pick it up.
   - Otherwise: **push the target's `NEXTSLOT`** — resolve `push` on `T` (as a
     `process@1.0` message) with `slot = NEXTSLOT`, `result-depth = D−1`, and
     (if `max-depth` was a positive integer `N`) `max-depth = N−1`. This may run
     **locally** or be **routed to a remote node** (§5.10). The returned
     sub-trace becomes the entry's `resulted-in`.

3. **Entry trace.** On success the entry's trace is:
   `{ id = <signed id of the scheduled message>, target = TARGET,
      slot = NEXTSLOT, resulted-in = <sub-trace | "skipped"> }`.
   On a downstream failure it is an error placeholder
   `{ response = error, target = TARGET, reason = <error> }` (§8).

## 5. Data formats & encodings

### 5.1 Slot results and the outbox
- A slot's **results** are obtained by resolving `compute/results` on the
  process for that `slot`. The results message contains (among other keys
  defined by the execution device) an `outbox` map.
- The `outbox` is a map of **1-based decimal-string keys** (`1`, `2`, …) to
  **outbox messages**, ordered by emission. Each outbox message is an ordinary
  AO-Core message carrying at least a `target`; it MAY carry a `resolve` path
  (§4.4) and arbitrary application fields.
- An **empty** outbox is an empty message (or a message containing only a
  private section). Emptiness terminates the recursion at that node (§4.2).

### 5.2 Outbox key ↔ trace key
- The trace returned by `push` keys each entry under the **same** outbox key it
  came from (`1`, `2`, …). Callers index the trace by these positional keys
  (e.g. the first emitted message is at trace key `1`).

### 5.3 Target and hint parsing
- An outbox message's `target` is a binary. Split it on the **first** occurrence
  of `?` **or** `&`:
  - the **target id** is the portion **before** the separator (a base64url,
    43-char process id);
  - the **hint** is the portion **after** (a query string, possibly empty).
- If neither separator is present, the whole value is the target id and the hint
  is empty.
- The hint MAY carry `hint=<node-locator>` identifying a node that schedules the
  target (used by redirect/remote paths). The bare **target id** is what is
  looked up in the cache and used as the process address.

### 5.4 The scheduled message (augmentation + provenance)
Before scheduling an outbox message `Msg` onto target `T`, the device produces
the message actually enqueued by setting (without updating the hashpath) the
following keys, then stripping any existing commitments (so it can be re-signed
under the target's policy, §7):

- `target` — the target id (§5.3) (retained from the outbox entry).
- `data-protocol = ao`
- `variant = ao.N.1`
- `type = Message`
- `from-process` — the **source process signed id** `ID`.
- `from-uncommitted` — the source process **uncommitted id** `UNCOMMITTED`.
- `from-base` — the source process **base id** `BASE` (§5.6).
- `from-scheduler` — the source process's `scheduler` value.
- `from-authority` — the source process's `authority` value.

These `from-*` provenance keys let the recipient process establish *which*
process (and under which scheduler/authority) a message originated from — the
basis for trust decisions on the receiving side. (The recipient's own logic, not
this device, decides whether to act on the message.)

### 5.5 The trace (result tree)
`push` returns `{ok, Trace}`. `Trace` is a map combining:

- **Per outbox entry**, under that entry's outbox key `K`, one of:
  - a **delivered** sub-trace:
    `{ id, target, slot, resulted-in }` where `id` is the signed id of the
    message that was scheduled, `target` is the target id, `slot` is the slot
    the message was assigned on the target, and `resulted-in` is **either** the
    recursively-produced sub-trace of pushing that slot, **or** the binary
    `<<"skipped">>` when the depth bound stopped the recursion (§6.3);
  - a **placeholder** for a non-delivered entry (errors / no target), as
    specified in §8 (carrying `response`/`resolve` = `error`, a `status`, an
    `outbox-index` or `target`, and a `reason`).
- **`slot`** — the source slot `S` that was pushed at this level.
- **`process`** — the source process signed id `ID`.
- **The full computed result** of this level's slot, **inlined only when**
  `result-depth > 0` at this level. At `result-depth = 1` (the default for the
  top call) the **first** level inlines its full result but deeper levels
  (reached at `result-depth = 0`) inline only their tree (`slot`/`process` +
  children), not the full result — i.e. `result-depth` controls how many levels
  deep the *full result payload* is carried, while the *tree of children* is
  always present to the full traversal depth.

The trace's recursive shape therefore mirrors the message graph: each delivered
entry's `resulted-in` is itself a trace of the same shape for the target slot,
bottoming out at leaves whose outbox was empty (a node with just `slot`/`process`
and possibly its inlined result) or at `<<"skipped">>` markers / error
placeholders.

### 5.6 Source process ids
- **Signed id (`ID`):** the process's `message@1.0` id selecting its **signed**
  commitments. Deriving it REQUIRES the process to verify and to have ≥1 signer
  (§7); otherwise the push fails closed.
- **Uncommitted id (`UNCOMMITTED`):** the process's id computed with commitments
  excluded (the content id of the bare process).
- **Base id (`BASE`):** the **content id** of the canonical process with its
  scheduler/authority stripped. Compute it exactly so: take the canonical process
  (the value of the base's nested `process` key if present, else the base
  itself), **set both `authority` and `scheduler` to _unset_** (removing the two
  keys) **without advancing the hashpath**, then take that message's `id`
  selecting **`none`** committers — the **content** id, NOT the signed id. The
  base id is thus invariant under changes to a process's scheduler or authority
  configuration — it identifies the process's *logic*, independent of who
  schedules or authorises it. (Two common ways to get a *different*, wrong id:
  deleting the keys with a plain map-remove instead of a hashpath-neutral
  set-to-unset, or taking the id over `signed`/`all` committers instead of
  `none`.)

### 5.7 Initial scheduling and redirects (Mode B)
- Scheduling the initial `body` is a POST to the base's `schedule`. On result
  status:
  - `200` ⇒ proceed with the returned assignment.
  - `307` ⇒ the base is scheduled remotely; follow `location` to that scheduler
    and schedule there (§5.10 redirect handling), then proceed with the remote
    assignment.
  - `422` ⇒ wrong codec/format; the push fails with that `422` error (initial
    scheduling does **not** auto-downgrade — only result scheduling does, §5.9).
- The assignment's `slot` is the slot subsequently pushed (unless the scheduled
  object was a `Process`, §4 Mode B).

### 5.8 Type discrimination
- "Is this a process or a message?" is decided by reading the scheduled object's
  `type`. **When the object is a scheduler assignment** (the Mode B
  schedule-then-push case, §4 Mode B step 2), its *top-level* `type` is always
  the scheduler's wrapper constant `Assignment`, so the discriminating value
  lives at **`body/type`** — read it there, and **link-aware** (the assignment's
  `body` is returned as a lazy `{link, …}`, so a plain map lookup misses it). Do
  **NOT** try top-level `type` first: it would always match `Assignment` and
  *never* detect a `Process` (every Mode-B process initialisation would be pushed
  instead of returned). Only for a bare object that is *not* an assignment
  wrapper do you read its own top-level `type`. A value of `Process` ⇒ process
  (initialisation; no push). Any other value (typically `Message`) ⇒ message
  (push it). This same `type` read selects the redirect path shape for remote
  scheduling (a `Process` redirect targets the scheduler's `/schedule`; a
  `Message` redirect follows the returned path).

### 5.9 Codec downgrade on result scheduling
- When scheduling an outbox message's result onto a target, the message is first
  signed under a **codec** (default the node's configured scheduler commitment
  codec, typically `httpsig@1.0`).
- If the target scheduler rejects the format with status **`422`**:
  - if the codec was already `ans104@1.0`, the scheduling fails (`{error, …}`);
  - if the codec was `httpsig@1.0`, the device **downgrades** to `ans104@1.0`,
    re-signs, and retries the schedule once. This lets a message destined for a
    legacy (ANS-104) scheduler be re-encoded transparently.

### 5.10 Remote delivery
- **Result scheduling redirect (`307`).** If scheduling a result onto a target
  returns `307`, the device normalises the message, signs it, and POSTs it to
  the scheduler named by the redirect `location` (recursing on further `307`s).
- **Recursive push routing.** When driving a target's slot (§4.5 step 2), the
  device MAY route the `/push` to a **remote node**:
  - It asks the node's router to resolve a route for the path
    `/<target-id>/push&slot=<NEXTSLOT>`.
  - If **no route matches**, or the route resolves to **this same node**, the
    push runs **locally** (resolve `push` on the target, as in §4.5 step 2).
  - If the route resolves to **another node**, the device POSTs that
    `/<target-id>/push&slot=<NEXTSLOT>` path to that node and uses its response
    as the sub-trace.
  - Whether routing is attempted at all is a node option (default: attempt
    remote routing); a node MAY be configured to always push locally.
- Remote-push responses are HTTP and are spliced into the trace exactly as a
  local sub-trace would be.

### 5.11 Encodings
- All ids (process ids, message ids, target ids, committer addresses) are
  **base64url** (43 chars for 32-byte values), never hex.
- All keys are lowercase, hyphenated binaries on the wire. Provenance keys are
  exactly `from-process`, `from-uncommitted`, `from-base`, `from-scheduler`,
  `from-authority`. The skip marker is the literal binary `<<"skipped">>`.
- `slot`/`max-depth`/`result-depth` are integers; on the wire they MAY arrive as
  decimal binaries and MUST be parsed as base-10 integers.

## 6. Ordering, freshness & caching

### 6.1 Outbox traversal order
- Outbox entries are processed in **outbox key order** (the 1-based emission
  order). The trace preserves each entry under its original key, so the result is
  independent of map iteration order even though the per-entry work for distinct
  entries is independent. The recursion is **depth-first** per entry: an entry is
  fully scheduled-and-pushed (down to its depth bound) before its sub-trace is
  recorded; entries at one level are otherwise independent.

### 6.2 `result-depth` (inlined-result depth)
- `result-depth` defaults to `1`. At each recursion level the **full computed
  result** is inlined into the trace **iff** the current `result-depth > 0`, and
  the value passed to the next level is `result-depth − 1`. Thus with the default
  `1`, only the **top** level carries its full result payload; deeper levels
  carry only the structural tree (`slot`/`process`/children). `result-depth = 0`
  inlines no full result anywhere; larger values inline deeper. `result-depth`
  does **not** bound the traversal — the tree of children is always walked to the
  natural termination (empty outbox) or the `max-depth` bound.

### 6.3 `max-depth` (fan-out bound) and termination
- `max-depth` bounds how far the **recursive compute/push** fans out from a
  source slot's outbox. Parsing: a non-negative integer (verbatim or as a decimal
  binary) is taken as-is; **any** other value (absent, negative, non-numeric,
  boolean, empty) is treated as `undefined` ⇒ **unbounded**.
- Semantics:
  - **unbounded** (`undefined`): recurse until outboxes run dry.
  - **`0`**: for each outbox entry, still **schedule** the message on its target
    (the target's slot advances), but **skip** the recursive `/push`; the entry's
    `resulted-in` is `<<"skipped">>`. The target's compute is **not** invoked
    here.
  - **`N>0`**: recurse, with the inner `/push` inheriting `max-depth = N−1`. The
    traversal unwinds **at most `N` levels** deep before the `0` rule applies.
- **Termination.** A push terminates when, along every branch, one of: the
  outbox is empty (leaf), the `max-depth` bound reaches `0` (skip), the target
  cannot be loaded / has no target (placeholder), or a compute/schedule error
  occurs (error placeholder). Because each scheduled message advances a target to
  a **new** slot and the recursion only ever pushes *newly assigned* slots,
  forward progress is guaranteed; there is no cross-entry deduplication —
  termination relies on application logic eventually producing empty outboxes
  (or on `max-depth`) rather than on cycle detection (§6.4).

### 6.4 No deduplication / cycle detection
- The device performs **no** deduplication of outbox messages and **no** cycle
  detection across the recursion. A process that emits a message to itself (or a
  cycle of processes that reply to each other) will be pushed repeatedly, each
  emission producing a new slot, until the application stops emitting outbox
  messages or a `max-depth` bound halts the fan-out. Implementations MUST NOT
  silently coalesce or drop "duplicate" outbox messages; each is scheduled and
  (unless bounded) pushed. Bounding non-terminating message loops is the caller's
  responsibility (via `max-depth`, or via the application's own stop condition).

### 6.5 Freshness / caching
- The recursive `/push` and the result-scheduling resolutions run with
  cache-control set to **always** (results are cached), so repeated pushes of the
  same already-computed slot are served from cache rather than recomputed. The
  initial outbox **compute** runs with hashpath updating **disabled** (the push's
  traversal does not perturb the process's hashpath chain).
- Idempotence at the boundary: a freshly-computed slot may trigger **one** push
  (§9); recomputing the same slot is a cache hit that does **not** re-fire the
  push hook, so polling a process's state repeatedly does not multiply scheduled
  messages.

## 7. Security & authority

- **Source authentication (failure-closed).** To push a process's outbox, the
  device MUST derive the source process's signed id, which REQUIRES the process
  to **verify** and to carry at least one **signer**. A process that does not
  verify, or has no signers, MUST cause the push to fail (it cannot be the
  authenticated source of messages). This is failure-closed: an unauthenticated
  process cannot drive message passing.

- **Recipient security policy (signing the scheduled message).** Each message
  scheduled onto a target is signed according to the **target's** policy,
  resolved in this order:
  1. **`policy`** on the target process: if present and it resolves to a policy
     message carrying an **`accept-committers`** value (a committer address, or a
     parsed list of them), sign with **exactly** those committers.
  2. **`authority`** on the target process: if present (a single authority
     address or a parsed list), sign with every listed authority the node can
     **act as** — i.e. that it **holds a wallet for**. "Locally available
     identities" means the node's configured identity set, addressed by
     `message@1.0` address; a listed authority the node holds no wallet for
     contributes no committer.
  3. **Default:** sign with the node's default identity (its configured wallet).
- After signing, the message MUST **verify** before it is written to cache and
  scheduled; a message whose signing produced **no** committers (the policy /
  authority named only identities the node does not hold) MUST fail with a
  "no matching authority" error (§8, `no-matching-authority`) rather than be
  scheduled unsigned — **unless** the node is configured to permit unsigned
  pushes, in which case the default identity signs.
- *Platform note (not protocol):* the exact option keys — where `policy` is read
  from, the node's identity-set lookup, and the permit-unsigned-pushes switch —
  are node/platform configuration. The **normative, observable** contract is the
  3-step precedence above plus the failure-closed "no committers ⇒
  `no-matching-authority`" rule.
- The provenance keys (§5.4) are set **before** signing, so the signature covers
  the `from-*` source attestation; a recipient can therefore rely on the
  committed `from-process`/`from-base`/`from-authority` to decide trust.

- **Side-effecting authority.** A push causes the node to **schedule** messages
  onto processes (mutating scheduler state) and, for remote targets, to issue
  **outbound HTTP** to other nodes. Any caller able to reach a `push` path can
  thereby cause these effects (bounded by what the source process's outbox
  actually contains). The device itself imposes no caller authorisation beyond
  the source-process verification above; operators who must restrict who may
  trigger pushes MUST gate access upstream.

- **Trust is the recipient's call.** This device delivers messages with
  provenance; it does **not** decide whether a target should act on them. The
  target process's own execution logic (using the `from-*` keys and its
  authority configuration) determines acceptance. A message can be delivered
  (scheduled) yet ignored by the recipient's logic.

## 8. Errors

Two error surfaces: **top-level errors** that fail the whole push (returned as
`{error, …}`), and **per-entry placeholders** embedded in the trace (the push as
a whole still succeeds; individual entries record their failure). The table pins
each condition and the canonical hyphenated atom name; current payloads are
human-readable structured messages, so the **condition** is normative and the
exact payload text is not.

**Top-level (fail the push):**

| atom | condition |
|---|---|
| `process-not-verified` | the base process fails verification when deriving its id. |
| `process-has-no-signers` | the base process verifies but carries no signer. |
| `compute-failed` | computing the source slot's outbox **raised**; the result carries the process id, slot, and reason. (Returned as `{error, …}` for that level.) |
| `initial-schedule-wrong-format` | Mode B: scheduling the initial `body` returned status `422`. |

**Per-entry placeholders (embedded in the trace, push still succeeds):**

| atom | condition | placeholder shape (keys) |
|---|---|---|
| `no-target` | an outbox entry is a map but carries no `target` key (and none after maybe-evaluate). | `response = error`, `status = 404`, `outbox-index`, `reason` — recorded as a **404** placeholder, nothing scheduled. (NOT an empty `{}`.) |
| `target-not-found` | the target id is well-formed but not loadable from the cache. | `response = error`, `status = 404`, `target`, `reason`. |
| `target-not-available` | an outbox value is not a schedulable message **at all** (e.g. not a map). | `response = error`, `status = 404`, `outbox-index`, `reason`, `message`. |
| `resolve-error` | maybe-evaluate (`resolve`) of an entry failed. | `resolve = error`, `status = 400`, `target` (source process id), `outbox-index`, `reason`, `source`. |
| `schedule-error` | scheduling the result onto the target failed (status ≠ 200 / error, and any codec downgrade also failed). | `response = error`, `target`, `reason`. |
| `push-error` | the recursive downstream `/push` of a scheduled slot returned an error. | `response = error`, `target`, `reason`. |

Placeholders MUST NOT abort the traversal of sibling entries: a failure on one
outbox entry is recorded and the loop proceeds with the rest. Top-level errors
MUST short-circuit the current level (no partial trace is returned for a level
whose own compute failed).

## 9. Composition

- **Driven by `process@1.0` — re-view the base as a process before delegating.**
  `push` is reached as a process's `push` key: resolving `push` on a
  `process@1.0` message delegates to this device with the push device
  **temporarily bound** (per `process@1.0` §9 device-switching). So the base your
  `push` handler receives carries `device => push@1.0`, and the switch dropped its
  outer commitments (the real, committed process lives under the embedded
  `process` self-key). **Before** calling back into `process@1.0` for `schedule`,
  `compute/results`, or the recursive `push`, you MUST re-view the base **as a
  `process@1.0` message** — set `device => process@1.0` on it, carrying identity
  through the `process` self-key (per `process@1.0` §9). Delegating on the
  base *as received* (device `push@1.0`) re-enters **this** device and fails
  (`not_found` on `compute/results`). With the re-view, `push@1.0`,
  `scheduler@1.0`, and the execution device interlock to walk the message graph.

- **Post-compute push hook.** `process@1.0/compute` accepts a `push` request key:
  when a **fresh** slot is computed with a truthy `push`, the process fires an
  **async** `push@1.0/push` for that slot. `push = true` (or `<<"true">>`) ⇒
  unbounded; `push = N` (non-negative integer) ⇒ `max-depth = N`; anything else
  ⇒ no-op. The hook forwards the originating request's payload keys
  (e.g. `result-depth`, `async`) into the push. Because the hook fires **only**
  on a fresh compute (cache hits short-circuit before it), repeatedly polling a
  process (`/now`, `/compute`, a cron tick) triggers the push for each slot **at
  most once** per node lifetime — the push is naturally idempotent across polls.
  This is the cron-driven message-passing pattern: a timer advances a process's
  compute with `push` set, and the hook propagates the resulting outbox without a
  blocking caller.

- **Self-scheduling + push.** Combined with `cron@1.0` (or any periodic driver):
  a process whose compute path carries `push` will, each time its next slot is
  computed by the timer, push its outbox — letting a network of processes make
  progress with no external prodding.

- **`max-depth` as a back-pressure / fan-out control.** A caller (or the compute
  hook) can bound the synchronous fan-out with `max-depth`, scheduling the
  immediate hop(s) but deferring deeper propagation to each target's own push
  cycle. `max-depth = 0` is the "schedule only, let targets self-drive" mode.

## 10. Conformance (normative checklist)

An implementation MUST exhibit every behaviour below; each is observable via the
resolution/HTTP surface or the side effects on target processes' schedules.

1. `push` is the **only** resolved key; the device is explicit-key (no default
   handler, no `excludes`); `set`/`keys` on a push message reach `message@1.0`.
2. **Mode B, message body:** with no `slot` and a message `body`, the device
   schedules the body onto the base process (POST `schedule`), takes the assigned
   slot, and pushes it — driving its outbox.
3. **Mode B, process body:** with a `body` whose `type` is `Process`, the device
   schedules (initialises) it and returns the assignment **without** pushing
   (no outbox is driven on process creation).
4. **Mode A:** with a `slot`, the device pushes that already-scheduled slot of
   the base process directly.
5. **Outbox read & target:** the device reads the slot's outbox as the `outbox`
   key of `compute/results`, a 1-based positional map; each entry's destination
   is its `target`, with the bare process id parsed off any `?`/`&` hint.
6. **Per entry it schedules then recurses:** for each outbox entry with a
   loadable target, the device schedules the (augmented, signed) message onto the
   target — advancing the target's slot — and then pushes the target's resulting
   slot; the entry's trace records `id`, `target`, `slot`, and `resulted-in`.
7. **Empty outbox terminates** that branch: a slot with an empty outbox yields a
   leaf trace (`slot` + `process`, plus the inlined result iff `result-depth >
   0`) and no further scheduling.
8. **Trace shape & keys:** the returned trace is a map keyed by the outbox
   positional keys, each value a sub-trace `{id, target, slot, resulted-in}` (or
   an error/no-target placeholder), plus top-level `slot` and `process` (the
   source process **signed** id) at every level.
9. **Provenance:** every scheduled message carries `data-protocol = ao`,
   `variant = ao.N.1`, `type = Message`, and `from-process` / `from-uncommitted`
   / `from-base` / `from-scheduler` / `from-authority` derived from the source
   process; `from-base` excludes the process's `authority` and `scheduler` from
   the id; these keys are set before signing (covered by the signature).
10. **`max-depth = 0`:** each outbox message is still scheduled on its target
    (the target's current slot increases) but the recursive `/push` is skipped
    and the entry's `resulted-in` is the binary `<<"skipped">>`; the target's
    compute is not invoked by this push.
11. **`max-depth = N > 0`:** the device recurses, the inner push inheriting
    `N−1`, unwinding at most `N` levels before the `0`/skip rule applies; absent
    or unparseable `max-depth` ⇒ unbounded; negative/non-numeric/boolean/empty ⇒
    unbounded.
12. **`result-depth`:** defaults to `1`; the full computed result is inlined at a
    level iff `result-depth > 0` there, decrementing by 1 per level; it does not
    bound the traversal (children are always walked to termination).
13. **`async`:** with `async = true` (on request or base process) the call
    returns immediately and the push runs detached; default (`false`) runs
    synchronously and returns the trace.
14. **`resolve` entries:** an outbox entry carrying `resolve = Path` is resolved
    (with its `target` removed during evaluation, re-attached to the result)
    before scheduling; a failed evaluation produces a `resolve = error`
    placeholder (`status = 400`, `outbox-index`, `reason`, `source`) without
    aborting sibling entries.
15. **Recipient signing policy:** scheduled messages are signed per the target's
    `policy` (`accept-committers`) → `authority` → default-identity precedence;
    the signed message must verify before scheduling; signing that yields no
    committers errors (`no-matching-authority`) unless the node permits unsigned
    pushes.
16. **Codec downgrade:** a `422` from a target scheduler causes an `httpsig@1.0`
    message to be re-signed as `ans104@1.0` and retried once; an already-`ans104`
    `422` fails.
17. **Remote routing:** when driving a scheduled slot, if a route resolves the
    `/<target>/push&slot=<n>` path to another node the device POSTs to that node;
    if no route matches or the route is self, it pushes locally; a result-schedule
    `307` redirect re-signs and POSTs to the redirect location.
18. **Source authentication (failure-closed):** a base process that does not
    verify (`process-not-verified`) or has no signers (`process-has-no-signers`)
    fails the push.
19. **Compute resilience:** a compute that raises while reading the outbox does
    not crash the push; it returns a `compute-failed` error carrying the process
    id, slot, and reason.
20. **No dedup / cycle detection:** repeated/self-addressed outbox messages are
    each scheduled and (unless bounded by `max-depth`) pushed; the device adds no
    cycle detection — termination is via empty outboxes or `max-depth`.
21. **Compute hook idempotence:** `process@1.0/compute` with a truthy `push` on a
    **fresh** slot fires exactly one async push for that slot (`push = N` ⇒
    `max-depth = N`); recomputing the same slot is a cache hit that does not
    re-fire the push.

## 11. Out of scope

- The **internal representation** of messages, the outbox, the trace, the
  detached-async handle, and any worker/spawn mechanics.
- The behaviour of `scheduler@1.0` (slot assignment, ordering, assignment shape)
  and of `process@1.0`'s `compute`/`schedule` themselves — those are their own
  specs; this device only consumes their contracts (POST `schedule` returns an
  assignment with a `slot` and a `status`; `compute/results` yields a results
  message with an `outbox`).
- The behaviour of the **execution device** that *produces* the outbox (how a
  process decides what messages to emit and how they are encoded into the
  results `outbox`).
- The exact wire/codec details of `httpsig@1.0` vs `ans104@1.0` (see those
  specs); this device only chooses between them and triggers a downgrade on
  `422`.
- The **router**'s route-matching algorithm and node-discovery (`whois`-style)
  details; this device only consumes "resolve a route for this path → a node or
  no match".
- The exact human-readable **text** of error payloads and trace placeholder
  prose (only the conditions, the status codes, and the structural keys named in
  §8 are normative).
- **Performance**, concurrency limits, and back-pressure beyond the
  documented sync/async and `max-depth` controls.

## Open questions

1. **Error payload form.** Like sibling devices, failures are returned as
   structured human-readable messages (e.g. a `body`/`reason` map), not bare
   hyphenated atoms; §8 pins conditions and canonical atom names but marks the
   payloads non-normative. Should conformant implementations be required to emit
   the atoms, or may they keep the structured-message form?
2. **`target-not-found` vs remote routing asymmetry.** When an outbox entry's
   target process cannot be **loaded from the local cache**, the entry becomes a
   `target-not-found` placeholder and is **not** scheduled — yet for an
   *already-scheduled* slot the recursion will route a `/push` to a remote node.
   So a target that lives only on a remote node is reachable for the recursive
   push but not for the initial schedule of *this* hop. Is the intended contract
   that targets must be resolvable locally to be scheduled (relying on the cache
   being populated, e.g. via a gateway store), or should the initial result
   schedule also follow routing/hints to a remote scheduler? (The `307` redirect
   and `hint=` query suggest remote scheduling is partially supported but only
   via the scheduler's own redirect, not via pre-emptive routing here.)
3. **`result-depth` semantics for deep trees.** `result-depth` is documented as
   "how many levels inline the full result", decrementing per level, default `1`
   (top level only). It is decremented on the recursive `/push` request but the
   relationship between `result-depth` and the *structural* depth of the tree
   (which is unbounded) could be read two ways: does `result-depth` ever stop the
   traversal, or strictly only the inlining? This spec takes the latter
   (traversal bounded only by empty outboxes / `max-depth`); a future revision
   SHOULD confirm no implementation prunes children when `result-depth` hits 0.
4. **Provenance under codec downgrade / redirect.** On a `307` redirect (and in
   the `ans104` downgrade path) the message is **re-normalised** and re-signed;
   the spec should confirm that the `from-*` provenance survives those re-encodings
   intact (the redirect path uses a normalised message that re-derives `target`
   but may not re-attach every `from-*` key). If any provenance key can be lost on
   a redirect, that is a divergence reimplementers would need pinned.
5. **Async return value.** The synchronous path returns the trace; the async path
   returns an opaque handle to the spawned activity. The handle's form (and
   whether a caller can later join/observe the async push) is left out of scope —
   should the protocol define an observable handle, or is async strictly
   fire-and-forget?
