# `apply@1.0` — the resolution / indirection device

- **Device name:** `apply@1.0`
- **Depends-on:** `message@1.0` (the base device against which all of this device's reads and the excluded mutation/inspection keys resolve). The `message@1.0` spec is provided to reimplementers.
- **Status:** Draft

## 1. Overview

`apply@1.0` is an **indirection device**: it executes an AO-Core resolution
that is *named indirectly* by another message rather than written literally in
the path. It turns a key, or a `base`/`request` pair, held inside the bound
message (or the request) into an actual resolution and returns that
resolution's result.

It supports two modes. (1) **Eval mode:** the key the device is invoked with is
treated as the *name of a path* to look up and then resolve — optionally on top
of a `source` message drawn from elsewhere in the base. (2) **Pair mode:** a
`base` message and a `request` message are each named indirectly, fetched, and
then resolved against one another (`base` applied with `request`). Pair mode is
reached either by invoking the `pair` key explicitly, or automatically whenever
the request carries both a `base` and a `request` key.

The device is the mechanism behind patterns such as "store a path in a message
field and later execute whatever it points to", "swap which of two messages is
the subject and which is the applied request", and "resolve a path that is
selected at request time".

## 2. Concepts & terminology

- **Base / `Base`:** the message the device is bound to (the message whose
  `device` is `apply@1.0`, or the message reached at the current path). Reads
  marked "from `Base`" come from here.
- **Request / `Req`:** the per-step request message carrying the invoked key as
  its `path`, plus any request fields. Reads marked "from `Req`" come from here.
- **Invoked key:** the key name the device was asked to resolve (the path
  segment that selected this device's behaviour). In eval mode this key names a
  field that *contains* a path; in the default-handler pair case it becomes the
  `path` set on the prepared request.
- **Indirect reference:** a field whose **value is itself a path** to be looked
  up. `apply@1.0` reads such fields (`source`, `base`, `request`, and the
  invoked key) and then resolves the path their value denotes, rather than using
  the field value as final data.
- **Source-prefixed path:** a path value whose first segment carries a `base:`
  or `request:` (or `req:`) prefix selecting which message that path is read
  from. See §5.
- **Eval mode / Pair mode:** the two execution modes described in §1.

The **internal representation** of any of these messages (how the map is stored
in memory) is out of scope; the contract below is defined over logical
key/value content and resolution results only.

## 3. Device interface

- **Dispatch shape:** **default-handler.** The device answers an explicit
  `pair` key (see §4) and routes **every other key** through a single default
  handler. The default handler MUST be **excluded** from capturing the message
  manipulation/inspection keys, so that the following keys fall through to the
  base `message@1.0` device instead of being interpreted as indirect references:
  `keys`, `set`, `set-path`, `remove`. (`set` and `set-path` are the same
  reserved operation; hyphen and underscore are equivalent in key names.) Any
  key other than those four, and other than `pair`, is treated as an **invoked
  key** by the default handler.

- **Message shape(s):**
  - **Eval-mode base/request** — no required keys. Optional, read by name:
    - `source` *(optional)* — value is a **path** (possibly source-prefixed)
      naming the message to use as the resolution subject. If absent, the
      subject is the base message itself with its `device` key removed (see §4).
    - the **invoked key** *(as a field)* — a field of that name (looked up in
      `Req` first, else `Base`) whose value is the **path** set as the subject's
      `path` and resolved on the subject (§4). If **no such field exists**, the
      eval-execute step **errors** with `path-not-found` (``Path `<invoked-key>`
      to execute not found.``) — it is NOT returned as an unresolved subject. (An
      invoked key is always present when the device is invoked, so a missing
      invoked-key *field* is this error, never a silent pass-through.)
  - **Pair-mode base/request** — keys read by name:
    - `base` *(required for pair mode)* — value is a **path** naming the message
      to use as the resolution subject.
    - `request` *(required for pair mode)* — value is a **path** naming the
      message to apply as the request.
  - All field names are matched as lowercase binary keys. Reads use
    `message@1.0` semantics (case-insensitive key lookup; private keys are not
    visible). **These reads MUST treat the base/request as a `message@1.0`
    message, NOT route through the bound message's own device.** The base is bound
    to `apply@1.0`; reading any of its fields *through that device* would re-enter
    this handler and **fail to terminate**. Every `source`/`base`/`request`/
    invoked-key field read and every §5 path resolution is therefore performed
    against the message@1.0 identity of the base/request — this is the single most
    important implementation constraint, and it also delivers the case-
    insensitivity and private-key hiding above. Extra keys not named above are
    ignored by this device (but remain part of whichever message they belong to
    and are therefore visible to the downstream resolution).

## 4. Resolved keys (normative)

The device exposes exactly two resolution behaviours: the explicit `pair` key,
and the default handler (every non-excluded, non-`pair` key).

### `pair` — resolve a named base against a named request
- **Reads:**
  - `request` and `base` — each from `Req` first, then `Base` (per §5
    field-lookup order); each value is a **path**.
  - The messages those two paths resolve to (each read via §5 path resolution).
- **Behaviour:** MUST:
  1. Resolve the `request` field to a path `RequestPath`; if neither `Req` nor
     `Base` has a `request` field, fail with `path-not-found`.
  2. Resolve the `base` field to a path `BasePath`; if absent in both, fail with
     `path-not-found`.
  3. Resolve `RequestPath` to a message `RequestSource` (§5). On failure, fail
     with `source-not-found` (or `invalid-path` for a malformed path).
  4. Resolve `BasePath` to a message `BaseSource` (§5), with the same failure
     mapping.
  5. Resolve `BaseSource` **with** `RequestSource` as its request, and return
     that result.
- **Path injection (default-handler entry only):** When pair mode is entered
  via the default handler (request carries both `base` and `request`, see
  below), the prepared request MUST have its `path` set to the **invoked key**
  before the final resolution. When `pair` is invoked directly (the explicit
  key), no `path` is injected and `RequestSource` is used as-is. (Equivalently:
  a sentinel "undefined" path-to-set means "do not set a path".)
- **Returns:** `{ok, Result}` — whatever the inner `BaseSource`-with-request
  resolution returns — or an error message (§8).
- **Side effects:** none of its own; the inner resolution MAY have whatever side
  effects the resolved devices have.

### default handler — invoked key (eval mode and auto-pair)
- **Reads:** the invoked key name; `base`, `request`, `source` and the
  invoked-key field, per §5.
- **Behaviour:** MUST:
  1. Look up `base` and `request` **as raw, direct keys of `Req` only** — a plain
     map-key presence test on the request (NOT a `message@1.0` read, NOT the §5
     cross-message order): the trigger is case-sensitive and does not hide private
     keys. (This is *only* the trigger test; once pair mode is entered, the shared
     `pair` logic re-reads `base`/`request` via the §5 cross-message order, so a
     `base` present only on `Base` is still *used*.) If **both** `base` and
     `request` are direct keys of `Req`, enter **pair mode**: behave exactly as
     `pair` above, additionally setting the prepared request's `path` to the
     invoked key (path injection).
  2. Otherwise enter **eval mode**:
     a. **Determine the subject.** If a `source` field resolves to a path
        (§5 field-lookup: `Req` first, then `Base`), resolve that path to a
        message and use it as the subject. If there is no `source` field, use
        the base message **with its `device` key removed** as the subject.
        (Removing `device` is REQUIRED: leaving it as `apply@1.0` would cause
        the subsequent resolution to re-enter this device and recurse.)
     b. **Look up the path to execute.** Resolve the **invoked key** via §5
        **path resolution** (the "Path values and resolution" rules, NOT the bare
        "Field-lookup order"): a plain invoked key has no prefix, so it reads its
        field **value** request-first then base; a `base:`/`request:`/`req:`
        prefixed invoked key dispatches per §5. The resolved value **is**
        `ExecPath`. If the invoked-key field is **absent or cannot be resolved for
        any reason** (not-found, source-not-found, or invalid path), the
        eval-execute step fails uniformly with `path-not-found`,
        message body ``Path `<invoked-key>` to execute not found.`` — it does
        **NOT** return the subject unresolved. (There is no "no path to execute"
        success case via the default handler: an invoked key is always present, so
        a missing invoked-key field is this error, not a silent `{ok, Subject}`.)
     c. **Execute.** Set the **subject's** `path` key to `ExecPath` and resolve
        the resulting message as a plain `message@1.0` resolution **on the subject
        alone**. `ExecPath` is **not** re-resolved via §5 and the original request
        is **not** consulted for it (a no-prefix `ExecPath` reads from the subject,
        not request-first). Return that inner result; an inner error propagates
        unchanged (§8).
- **Returns:** `{ok, Result}` — the inner (subject-`ExecPath`) resolution result
  — or an error message (§8).
- **Side effects:** none of its own; inherited from the resolved devices.

> Worked examples (informative).
> - Base `{ device: apply@1.0, body: "/~meta@1.0/build/node", … }`, request
>   `{ path: "body" }`: eval mode, no `source`, subject = base without `device`,
>   invoked key `body` exists → its value `/~meta@1.0/build/node` is the path to
>   resolve → result is that resolution (`"HyperBEAM"`).
> - Base `{ device: apply@1.0, data-container: { relevant: "DATA" },
>   base: "data-container", … }`, request `{ data-path: "relevant",
>   request: "data-path", path: "pair" }`: explicit `pair` → `base` path
>   `data-container` and `request` path `data-path` are each resolved to
>   messages, then the container is resolved with `{ relevant ... }`'s target as
>   its request → `"DATA"`.
> - Path-string invocation
>   `/~meta@1.0/build/node~apply@1.0&node=TEST&base=request:&request=base:`:
>   binds `apply@1.0` over the `…/build` message with request fields
>   `node=TEST`, `base=request:`, `request=base:`; both `base` and `request` are
>   in the request → auto-pair with the two source-prefixed paths swapped, so the
>   `request:`-sourced message becomes the subject. Result `"TEST"`.

## 5. Data formats & encodings

### Field-lookup order (which message a field is read from)
Unless a step explicitly restricts to `Req` only (the auto-pair trigger in the
default handler reads `base`/`request` from `Req` only), a **field name** named
by this device (`source`, `base`, `request`, the invoked key) is looked up by
trying, **in order**, and taking the first that resolves:
1. the **request** message, read under `message@1.0` semantics;
2. the **base** message, read under `message@1.0` semantics.
If neither yields a value, the field is treated as **absent** (which, for
`source` and the invoked-key field, is not itself an error — it changes
behaviour as described in §4; for `base`/`request` in pair mode, absence is a
`path-not-found` error).

### Path values and resolution
A **path value** retrieved from a field is a path in ordinary AO-Core path form
(a `/`-separated binary, e.g. `"relevant"`, `"data-path"`,
`"/user-request/test-key"`, or `"/~meta@1.0/build/node"`). To **resolve** a path
value to a message/value:

1. **Split** the path into segments on `/`. The empty path and `"/"` denote the
   message itself (see source-only prefixes below).
2. **Inspect the first segment** for a source prefix by splitting it once on the
   first `:`:
   - First segment exactly `base:` (i.e. `base` then empty) → the value is the
     **whole base message** (no further resolution).
   - First segment exactly `request:` → the value is the **whole request
     message**.
   - First segment `base:<key>` → resolve the path `<key>` followed by the
     remaining segments **against the base message** (read as `message@1.0`).
   - First segment `request:<key>` **or** `req:<key>` → resolve that path
     **against the request message**.
   - First segment with **no** `:` → resolve the **entire** path by trying, in
     order, (i) against the **request** message, then (ii) against the **base**
     message; take the first that resolves.
3. The path is resolved using `message@1.0` read semantics over the chosen
   message(s). The result is the resolved value (which MAY itself be a nested
   message or a scalar).
4. If the chosen source(s) do not yield a value, the resolution fails with
   `source-not-found`. If the path cannot be parsed into segments at all
   (degenerate/non-path term), it fails with `invalid-path`.

Prefix matching is **exact** on the literal segment text `base`, `request`,
`req` followed by `:`; it is case-sensitive as written here and only the **first
segment** is inspected for a prefix. A path whose first segment merely *contains*
a colon but is not one of these tokens is treated by the colon-split: any
`X:<rest>` first segment that is not `base`/`request`/`req` is **not** a
recognised source prefix and the path is resolved by the no-prefix rule over the
literal first segment (request-then-base).

### Path normalisation in error/result text
For error messages, a path is normalised to its `/`-joined binary form, and an
empty/missing path is rendered as `"/"`. IDs, addresses, and any
content-addressed values that flow **through** this device are unchanged and
remain **base64url** (never hex) — this device performs no ID derivation of its
own.

## 6. Ordering, freshness & caching

- **Determinism / tie-breaks:** Field-lookup is **request-first, base-second**;
  no-prefix path resolution is likewise **request-first, base-second**. These
  orders are normative and MUST be observed so that a field present in both
  messages resolves to the request's value.
- This device performs **no caching or storage of its own.** It returns the
  inner resolution's result directly. Freshness and result-caching of the
  *inner* resolution are governed by that resolution's devices and by node
  configuration, not by `apply@1.0`.
- The device holds no mutable state; given the same base, request, and node
  options it produces the same indirection and therefore the same result the
  inner resolution would (subject to that inner resolution's own determinism).

## 7. Security & authority

- `apply@1.0` adds **no commitments and verifies none.** It neither signs nor
  checks signatures; it only redirects resolution. Any authority enforced is the
  authority of the **inner** devices it resolves into.
- The device reads only via `message@1.0` semantics, so **private keys are never
  exposed** by its field/path lookups (a private field cannot be used as a
  `source`/`base`/`request`/invoked-key reference, because `message@1.0` will
  not return it).
- A request MAY be committed (signed) by a caller; the signature covers the
  request's own fields (e.g. the indirect references and the invoked path). This
  device does not strip or alter those fields, so a downstream device that cares
  about the request's committers sees them intact. Conversely, because this
  device sets a `path` (in pair-via-default and in eval execute), a previously
  committed `path` on the prepared/subject message MAY be changed; per
  `message@1.0`, setting a committed key to a different value drops that
  message's commitments. Implementers MUST rely on `message@1.0`'s set semantics
  for this rather than inventing their own.
- **Failure-closed for references:** a named `base`/`request` (pair mode) or an
  invoked-key field that cannot be resolved is an **error**, not a silent empty
  result. Eval mode's **`source`** reference is the *only* deliberate exception:
  its **absence** selects an alternate, defined behaviour (subject = the base
  message with `device` removed) rather than erroring. The **invoked-key field is
  not** such an exception — when it is absent the eval-execute step errors
  (`path-not-found`, "to execute not found"), as in §4. (Note the asymmetry: a
  *missing* `source` is fine; a *present-but-unresolvable* `source` is a
  `source-not-found`/`invalid-path` error.)

## 8. Errors

All errors are returned as an error result whose **body is a human-readable
binary that distinguishes the condition** — the reference's carrier, and the one
this contract requires. The **condition** (one of the hyphenated atoms below) is
what is normative; the body's *exact wording* is informative (§11), but a body
from which the condition is recoverable (e.g. the distinct, condition-specific
strings below) MUST be produced — a bare condition atom with no body is **not**
conformant. An implementation MAY *additionally* surface the condition in a
dedicated field (e.g. `status`), but the distinguishing body is required.
Conformance is on *which condition is triggered*, observed via that body. The
hyphenated condition atoms:

- `path-not-found` — arises in two places:
  - a required indirect reference field was absent in **both** request and base
    (pair mode's `base` or `request`). Body: ``Path `<p>` to apply not found.``
  - the **invoked-key field could not be resolved** — it is absent (in both
    messages), or the invoked key is a source-prefixed reference that does not
    resolve — so `ExecPath` cannot be *obtained*. This **collapses every internal
    reason** (not-found, source-not-found, invalid path) into a single condition
    with the fixed body ``Path `<invoked-key>` to execute not found.``. **This is
    the failure to OBTAIN `ExecPath`, not the execution of an obtained one** — once
    `ExecPath` is obtained, resolving it on the subject (§4c) is an inner
    resolution whose error **propagates unchanged** (see the final paragraph), it
    is *not* re-collapsed to `path-not-found`.
  In both, `<p>` is the normalised path (`"/"` if empty).
- `source-not-found` — a path value (for `source`, `base`, or `request`
  references — i.e. **outside** the eval-execute step) was well-formed but the
  message/value it denotes could not be found in the selected source(s). Body:
  ``Source path `<p>` to apply not found.``
- `invalid-path` — a reference value (again, outside the eval-execute step)
  could not be parsed into a path at all. Body: ``Path `<p>` is invalid.``

The device MUST NOT mask an inner resolution's own error: if the inner
resolution (eval execute or pair apply) returns an error, that error propagates
unchanged.

## 9. Composition

- **Device switching in returned values.** Because the device returns the inner
  resolution's result verbatim, a returned value that itself carries a `device`
  key resolves its *next* path segment under that device. This is how
  `apply@1.0` participates in multi-hop paths.
- **Self-recursion avoidance.** In eval mode with no `source`, the subject is the
  base **with `device` removed**, so resolving a path on it does not re-enter
  `apply@1.0`. Implementers MUST preserve this removal; binding the subject's
  device back to `apply@1.0` (e.g. by merging the base's own keys over it) would
  loop. **Caveat — `device` removal is applied ONLY on the no-`source` branch.**
  A `source` that resolves to a message still bound to `apply@1.0` (most acutely a
  whole-base `source: base:`, which is the base verbatim, device intact) yields a
  subject whose execute step **re-enters `apply@1.0`** — a latent self-recursion.
  The whole-*request* `source: request:` is safe (the request carries no apply
  device). Avoid a whole-base `source` as an eval subject; this asymmetry is
  inherent to the device's `device`-strip being scoped to the no-`source` branch.
- **Fall-through of mutation/inspection keys.** Because `keys`, `set`,
  `set-path`, and `remove` are excluded from the default handler, an
  `apply@1.0`-bound message still supports being `set` on, listed, path-bound,
  and pruned via the base `message@1.0` device. A reimplementation MUST keep
  these four keys excluded, or those operations on an `apply@1.0` message break.
- **Singleton / request-merge invocation.** The device is commonly invoked from
  a path string of the form `…/<seg>~apply@1.0&k1=v1&k2=v2`, which binds
  `apply@1.0` over the message reached at `<seg>` and merges `k1=v1,k2=v2` as
  request fields (the AO-Core singleton encoding supplied by the platform). The
  `base`/`request`/`source` indirect references and source-prefixed path values
  are designed to be supplied this way (e.g. `base=request:&request=base:` to
  swap subject and request). This device does not define that encoding; it only
  consumes the resulting base + request.
- **HTTP exposure.** When such a path is requested over HTTP, the bound message
  may be signed; the device resolves it identically to a local resolution and
  returns the inner result as the HTTP response body.

## 10. Conformance (normative checklist)

An implementation MUST exhibit all of the following, each checkable by resolving
crafted messages (and by code review of the excluded-key set):

1. **Default routing.** Any invoked key other than `pair`, `keys`, `set`,
   `set-path`, `remove` is handled by the indirection logic; `keys`, `set`,
   `set-path`, `remove` fall through to `message@1.0` (e.g. `set` on an
   `apply@1.0` message mutates it rather than being interpreted as a reference).
2. **Eval, no source, invoked-key field present.** With base
   `{ device: apply@1.0, body: <path> }` and request `{ path: "body" }`, the
   device resolves `<path>` on the base (device removed) and returns that result
   (e.g. a base whose `body` is `/~meta@1.0/build/node` yields the node's build
   name).
3. **Eval execute sets `path`.** The value found at the invoked-key field is set
   as the subject's `path` and resolved **on the subject alone** (message@1.0);
   the result equals resolving the subject with that path — not the literal field
   value, and not a request-first re-resolution (a value present on both the
   subject and the request resolves to the subject's).
4. **Eval, no invoked-key field.** If the invoked key names no field in either
   message, the device returns the `path-not-found` error (body ``Path
   `<invoked-key>` to execute not found.``) — an error, **not** a silent
   `{ok, Subject}`.
5. **`source` redirection.** When a `source` field resolves to a path, the
   subject is the message that path denotes (not the base). A multi-segment
   execution path then resolves against that subject (e.g. invoked key
   `user-path` with value `/user-request/test-key` on a base containing
   `user-request: { test-key: "DATA" }` yields `"DATA"`).
6. **Auto-pair trigger.** If the **request** carries both `base` and `request`
   fields, the device enters pair mode and additionally sets the prepared
   request's `path` to the invoked key.
7. **Explicit `pair`.** Invoking `pair` resolves the `base`-named message with
   the `request`-named message and returns the result, without injecting a
   `path` (e.g. base path `data-container`, request path `data-path` over a base
   holding both yields the container's resolution against the data message).
8. **Source prefixes.** A path value beginning `base:` resolves against the base
   message and `request:`/`req:` against the request message; a bare `base:` or
   `request:` (empty key) denotes the whole base or whole request message
   respectively; swapping via `base=request:&request=base:` makes the
   request-sourced message the subject (e.g. resolving
   `/~meta@1.0/build/node~apply@1.0&node=TEST&base=request:&request=base:`
   yields `"TEST"`).
9. **Lookup order.** Field lookup and no-prefix path resolution are
   request-first then base-second; a name present in both resolves to the
   request's value.
10. **Reference errors.** A pair-mode invocation missing `base` or `request`
    gives `path-not-found`; an **un-obtainable invoked-key field** (eval mode —
    absent, or a source-prefixed invoked key that does not resolve) gives
    `path-not-found` for *any* internal reason (with the "to execute not found"
    body); an unresolvable **source/base/request** reference path gives
    `source-not-found`; an unparseable such reference gives `invalid-path`. Each
    is an error result, never a silent success. By contrast, once the invoked-key
    field is obtained, executing its `ExecPath` on the subject **propagates** the
    inner resolution's error unchanged (it is NOT re-collapsed to
    `path-not-found`) — see §4c and §8's final paragraph.
11. **No own commitments / caching.** The device produces no commitments, writes
    nothing to cache/store of its own, and returns the inner resolution's result
    (and any inner error) unchanged.
12. **No self-recursion.** Eval mode with no `source` strips `device` from the
    subject so the subsequent resolution does not re-enter `apply@1.0`.

## 11. Out of scope

- The internal representation of base/request/subject messages and of resolved
  values.
- The AO-Core singleton path encoding (`~dev@1.0&k=v` request merging) and HTTP
  request framing — supplied by the platform/`message@1.0` substrate.
- The cryptography, ID derivation, and caching of any device this device
  resolves into (governed by those devices and by node configuration).
- The exact wording of error-message bodies (only the triggering **condition**
  and the path-normalisation rule are normative; the example strings are
  informative).
- Performance, concurrency, and storage strategy.

## Open questions

- **`req:` vs `request:` field names.** The `req:` token is recognised as a
  *source prefix* on a path value (equivalent to `request:`). It is **not**
  recognised as an alternative *field name*: the device reads the field literally
  named `request` (and `base`). A path/field named only `req` (without colon) is
  treated as an ordinary, no-prefix path segment. This asymmetry is taken
  directly from the source; whether `req` should also be accepted as a field
  alias is undetermined from the source alone.
