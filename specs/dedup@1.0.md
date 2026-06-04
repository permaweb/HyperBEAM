# `dedup@1.0` — once-only message deduplication in an evaluation stream

- **Device name:** `dedup@1.0`
- **Depends-on:** `message@1.0` (ID derivation, `set`/`keys` delegation, the `as`/identity behaviour), `trie@1.0` (the seen-subjects set), `stack@1.0` (the `skip`/`pass` control-flow contract this device emits into). All three specs are provided to reimplementers.
- **Status:** Draft

## 1. Overview

`dedup@1.0` ensures that a given **subject** is acted upon only **once** within
an evaluation stream. When a key is resolved against a `dedup@1.0` message, the
device derives an identifier for the request's subject, consults a running set
of subjects it has already seen, and either lets the resolution proceed (subject
unseen — record it and continue) or signals the surrounding evaluation to
**skip** the remaining work (subject already seen).

It is most often placed as the first device of a `stack@1.0` execution stack
(e.g. the execution device of a `~process@1.0`), so that a message assigned more
than once is computed only once. It is, however, a general-purpose device and
may be used anywhere a key is resolved. The device runs its check only on the
**first pass** of a multi-pass stack evaluation, so it does not interfere with
devices (such as a multipass driver) that legitimately re-run a message for
additional passes.

## 2. Concepts & terminology

- **Subject:** the value whose identity is deduplicated. The subject is chosen by
  the `dedup-subject` configuration (§3). By default it is the request's `body`;
  it may be set to the literal string `request` to deduplicate the **entire
  request message**, or to any other key name to deduplicate the value found
  under that key.
- **Subject ID:** the content/commitment identifier of the subject, as computed
  by `message@1.0`'s `id` over the subject selecting its signed commitments where
  present and otherwise its content (a 43-character base64url string). Two
  subjects with identical committed/content form have identical Subject IDs; this
  is the equality test for "already seen".
- **Seen set:** the set of Subject IDs the device has already observed in this
  evaluation stream. It is carried on the base message under the `dedup` key as a
  `trie@1.0` message (a content-addressed set keyed by Subject ID). This spec
  treats it as an opaque set with membership-test and insertion operations
  delegated to `trie@1.0`; the trie's internal node layout is out of scope.
- **Pass:** the stack pass counter. In a `stack@1.0` evaluation the base message
  carries a `pass` integer (1 for the first pass, incremented when a device
  returns `pass`). Outside a stack there is no `pass` key and the device behaves
  as if on pass 1.
- **Skip:** the control signal `dedup@1.0` emits to its caller to mean "stop the
  rest of this pass". Under `stack@1.0` this halts the remaining devices for the
  current pass and yields the base message unchanged by those downstream devices
  (see §9 and the `stack@1.0` spec).

## 3. Device interface

- **Dispatch shape:** **default-handler.** Every key the device answers (other
  than the excluded keys below) is routed through one deduplication handler; the
  device does not enumerate a fixed key set. The handler's behaviour does **not**
  depend on *which* key was requested — the key name is read but only the request
  and base messages drive the decision (§4). This lets `dedup@1.0` mediate any
  key call (`compute`, `append`, an arbitrary path segment, …).
- **Excluded keys (delegated to `message@1.0`):** `keys`, `set`, `id`, `commit`.
  These MUST NOT be captured by the deduplication handler; they MUST resolve with
  the base `message@1.0` behaviour so that listing keys, mutating the message,
  taking its ID, and committing it continue to work while the device is bound to
  a path. (`set` in particular MUST fall through, because the device itself
  performs `set` operations on its base — see §4.) An implementation MUST exclude
  exactly these four reserved keys from its catch-all. This set is **deliberately
  narrower** than the general default-handler reserved set a `message@1.0`-backed
  device might delegate (e.g. `set-path`, `remove`): `dedup@1.0` is a thin
  mediator that intends *every* other resolved key to be subject to the
  deduplication check — only these four bypass it.
- **Message shape (`Base`):** a message whose device is `dedup@1.0`, optionally
  carrying:
  - `dedup-subject` (binary, optional): names the subject key, or the literal
    `request`. Default `body` (see §4). Read from `Base` first, then `Req`.
  - `dedup` (the seen set, optional): a `trie@1.0` message. Default: an empty
    `trie@1.0` message (`{ "device": "trie@1.0" }`).
  - `pass` (integer, optional): the stack pass counter. Default `1`.
- **Request shape (`Req`):** the request being resolved, carrying the resolution
  `path` and any request fields. Relevant optional keys:
  - `dedup-subject` (binary, optional): fallback source for the subject-key
    selection if absent on `Base`.
  - `slot` (optional): the value stored as the seen-set entry's payload when a new
    subject is recorded. Default `true` (see §4 and §5).
  - When `dedup-subject` selects `request`, the **entire** `Req` message is the
    subject. When it selects another key, that key is looked up on `Base` then
    `Req`.

All keys are **lowercase, hyphenated, binary on the wire**.

## 4. Resolved keys (normative)

### `keys`, `set`, `id`, `commit` — delegated
- **Behaviour:** Resolve with the `message@1.0` behaviour for the same key
  (§`message@1.0`). `dedup@1.0` adds nothing and MUST NOT deduplicate these.
- **Returns / errors / side effects:** exactly as `message@1.0`.

### default handler — any other key (the deduplication step)
- **Reads:**
  - the requested key name (read but does not alter the decision);
  - `dedup-subject` — the first value found across `Base` then `Req`, defaulting
    to `body` if present on neither;
  - the subject itself (§"Subject selection" below);
  - `pass` from `Base`, defaulting to `1`;
  - `dedup` (the seen set) from `Base`, defaulting to an empty `trie@1.0`;
  - `slot` from `Req`, defaulting to `true`.
- **These are reads of the base/request *as plain messages*, NOT resolutions
  routed back through `dedup@1.0`.** The device is a catch-all over its own base,
  and none of these config keys (`dedup-subject`, `pass`, `dedup`, the named
  subject key, `slot`) are reserved — so reading any of them by resolving the key
  *through* the bound `dedup@1.0` message would re-enter this same handler and
  **fail to terminate**. Read them from the message content directly: treat the
  base as its `message@1.0` identity (equivalently, read the raw map field). The
  `pass` comparison is an **integer** equality (`pass == 1`); the `stack@1.0`
  driver maintains `pass` as an integer counter and the device does not coerce
  other encodings.
- **Subject selection:**
  1. Resolve `dedup-subject` (default `body`). Comparison against the literal
     `request` is an **exact, case-sensitive** binary match.
  2. If `dedup-subject` equals `request`, the subject is the **entire `Req`
     message**.
  3. Otherwise the subject is the value found under the `dedup-subject` key,
     looked up on `Base` first and then `Req`. If the key is present on neither,
     the subject is **absent** (the sentinel `not_found`).
- **Behaviour (decision table):**
  - **Not the first pass** (`pass` ≠ 1): MUST take no deduplication action and
    return the base message unchanged → `{ok, Base}`. The seen set is **not**
    consulted or modified.
  - **First pass, subject absent** (`pass` = 1 and the selected subject is
    `not_found`): MUST take no deduplication action and return `{ok, Base}`. The
    seen set is not modified. (This is the case where `dedup-subject` names a key
    that is not present; deduplication is silently disabled for that request.)
  - **First pass, subject present** (`pass` = 1 and a subject was selected,
    including when the subject is the whole request): MUST compute the Subject ID
    (§5) and test membership in the seen set. Membership is a `trie@1.0` `get` of
    the Subject ID against the `dedup` set: a `not_found` result means **unseen**;
    any returned value means **seen** (the value itself is irrelevant — §5):
    - **Subject ID absent from the seen set (unseen):**
      1. MUST insert the Subject ID into the seen set, mapping it to the seen-set
         **payload** = `Req`'s `slot` value if present, else `true`. Insertion
         uses the `trie@1.0` `set` operation; the resulting trie is a new
         seen-set message.
      2. MUST write the updated seen set back onto the base message under the
         `dedup` key, using a **`set` with `set-mode = explicit`** (a shallow set
         of just the `dedup` key; other base keys are untouched, and no
         deep-merge of the trie occurs).
      3. MUST return the resulting base message → `{ok, Base'}` where `Base'` is
         `Base` with its `dedup` key replaced by the updated seen set. This lets
         the surrounding evaluation proceed (e.g. the next device in a stack).
    - **Subject ID present in the seen set (already seen):** MUST return the
      **skip** signal carrying the base message unchanged → `{skip, Base}`. No
      mutation of the base or the seen set occurs.
- **Returns:**
  - `{ok, Base}` — pass-through (not first pass, or subject absent), base
    returned unchanged.
  - `{ok, Base'}` — unseen subject recorded; base returned with an updated `dedup`
    seen set.
  - `{skip, Base}` — already-seen subject; base returned unchanged, with the
    skip signal.
  - **"Unchanged" means the device contributes nothing** — it adds no `dedup`
    key on the pass-through/skip paths and leaves the seen set and every base key
    as received. It does **not** assert byte/term identity of the resolved
    envelope: the resolution substrate may decorate the result with `priv`/
    `hashpath` bookkeeping (§11, out of scope). Conformance is observed on the
    control tag (`ok`/`skip`), the `dedup`-key state, and the base's content
    keys — never on whole-message equality.
- **Side effects:**
  - On the unseen-subject path, the updated seen set is produced via `trie@1.0`'s
    `set`, which (per the `trie@1.0` spec) commits and **writes the new trie
    message into the content-addressed store**. The base message's `dedup` key is
    then replaced with that trie. No commitment is added to the `dedup@1.0` base
    message itself by this device.
  - No other store writes, no network calls, no commitments produced by this
    device.
- **Errors:** This device defines no error atoms of its own (see §8). Errors
  surfaced by the delegated/sub-resolutions (`message@1.0` ID derivation,
  `trie@1.0` operations) propagate unchanged.

## 5. Data formats & encodings

- **Subject ID:** the `id` of the subject as defined by `message@1.0`, selecting
  the subject's signed commitments where present and otherwise its content. It is
  a **43-character base64url** string (never hex). For an uncommitted subject
  (including the common case `dedup-subject = request` with an unsigned request
  map) this is the content ID over the subject's normal form; for a signed
  subject it is the accumulation of its signed-commitment IDs. Equality of
  Subject IDs is the sole "already seen" criterion, so any two subjects that
  `message@1.0` assigns the same ID are treated as the same subject.
  - **Normative (this fall-back is load-bearing):** an implementation MUST derive
    the Subject ID with the `message@1.0` id that *selects signed commitments and
    falls back to the content id when the subject has none* (the `signed`
    selection — `message@1.0` recomputes the content id for a subject with no
    signed commitment). It MUST NOT use a derivation that **errors** on an
    uncommitted subject (dedup of unsigned subjects — the common case — would
    break), and MUST NOT use the plain content id unconditionally (a signed
    subject and an unsigned copy of the same content would then collide, when they
    are distinct subjects). The whole device hinges on this single derivation;
    do not skim past it.
- **Seen set:** a `trie@1.0` message stored under the base message's `dedup` key.
  Keys of the trie are Subject IDs (43-char base64url binaries). The value mapped
  to each key (the seen-set **payload**) is the `slot` value from the recording
  request, or the boolean-equivalent `true` when `slot` is absent. The payload is
  **never** read back as a decision input — only key presence matters — so its
  exact form is informational and an implementation MUST NOT base the
  seen/unseen decision on it.
- **`dedup-subject`:** a binary key name. The literal sentinel value `request`
  is matched **exactly and case-sensitively**; any other value is treated as a
  key name to look up. There is no normalisation of the sentinel (e.g. `Request`
  or `REQUEST` is **not** the sentinel and would be looked up as a key).
- **`set-mode`:** when the device rewrites its `dedup` key it MUST use the
  `explicit` set mode (shallow replace of the single key), not the default deep
  merge.

## 6. Ordering, freshness & caching

- The decision is **deterministic** given the base message (its `dedup` seen set
  and `pass`), the request, and the resolved `dedup-subject`/subject. There is no
  wall-clock, randomness, or external input.
- **Order sensitivity:** within a single evaluation stream the device is
  order-dependent in the obvious sense — the **first** occurrence of a subject is
  recorded and proceeds; **every subsequent** occurrence of the same Subject ID
  (within the same first-pass context, against a base whose `dedup` set already
  contains it) is skipped. Re-ordering which identical message arrives "first"
  does not change the outcome (all but one are skipped), but which concrete
  message object is the survivor is the first one resolved.
- The seen set is **threaded through the base message** (`dedup` key), not held
  in any device-global or cache-keyed state. A fresh base message with no `dedup`
  key (or a base whose `dedup` set does not contain the Subject ID) starts/treats
  the subject as unseen. The source notes the set is currently kept in memory and
  may later be persisted; this is an implementation concern and MUST NOT change
  the observable contract, which is: membership is whatever the base message's
  `dedup` trie contains at resolution time.
- This device performs no result caching of its own. (The `trie@1.0` `set` write
  is a content-addressed store write of the new seen-set message, not a
  resolution-result cache.)

## 7. Security & authority

- The device imposes **no authority or commitment requirements**: any caller may
  resolve any key through it. It neither verifies nor produces commitments on the
  base message.
- The deduplication identity is the subject's `message@1.0` ID. When the subject
  is signed, this binds deduplication to the signed content; when unsigned, to
  the content. The device trusts `message@1.0`'s ID derivation for this and adds
  no separate trust assumption.
- **Failure mode is fail-open for absent configuration:** if `dedup-subject`
  names a key that is missing (subject `not_found`), deduplication is silently
  disabled for that request (the message passes through). An implementation MUST
  NOT treat a missing subject as an error or as a duplicate.

## 8. Errors

- This device defines **no error atoms of its own.** It never returns
  `{error, _}` from its own logic; its outcomes are `{ok, _}` or `{skip, _}`.
- Errors arising in delegated work propagate unchanged to the caller:
  - `message@1.0` ID-derivation errors for the subject (see the `message@1.0`
    spec, e.g. `multiple-id-devices`).
  - `trie@1.0` operation errors (see the `trie@1.0` spec).
  - The delegated reserved keys (`keys`, `set`, `id`, `commit`) return exactly
    what `message@1.0` returns, including its error atoms (e.g. `not_found`).

## 9. Composition

- **As a stack device (primary use):** placed in a `stack@1.0` `device-stack`,
  typically first. Its return values map onto the `stack@1.0` control contract:
  - `{ok, Base'}` — the stack continues to the next device with the (possibly
    `dedup`-updated) base.
  - `{skip, Base}` — the stack **halts the remaining devices for the current
    pass** and yields `{ok, Base}` (the base message **unmodified by the
    downstream devices**). This is how a duplicate is prevented from being
    computed: the rest of the stack never runs for it.
  - The device never returns `pass`, so it does not itself drive multi-pass
    iteration; a separate device (e.g. a multipass driver) may, and `dedup@1.0`
    deliberately no-ops on passes other than the first so it does not block such
    iteration.
- **`dedup-subject` placement:** may be configured on the stack/base message
  (applies to every request) or supplied per-request; base takes precedence over
  request when both are present.
- **`dedup-subject = request`:** deduplicates whole requests — two requests that
  reduce to the same `message@1.0` ID are treated as duplicates regardless of
  which key is being resolved.
- **`dedup-subject = <some key>`:** deduplicates on the value under that key
  (looked up on base then request). Absence of the key disables deduplication for
  that request (pass-through), it does not skip.
- **Standalone use:** outside a stack, a `{skip, _}` return is simply the device's
  signal to its caller; callers that do not understand `skip` should treat it per
  their own resolution contract. The `{ok, _}` returns behave as ordinary
  resolutions.

## 10. Conformance (normative checklist)

An implementation MUST exhibit all of the following, each checkable by resolving
keys against a `dedup@1.0` message (optionally inside a `stack@1.0`) and
observing the returned control signal / message / store writes:

1. **Default dispatch.** Resolving any key other than `keys`/`set`/`id`/`commit`
   routes through the deduplication handler; the decision is independent of the
   key name (the same base/request yields the same decision for any such key).
2. **Reserved-key delegation.** `keys`, `set`, `id`, and `commit` resolve with the
   `message@1.0` behaviour and are never deduplicated. In particular, binding
   `dedup@1.0` onto a path and performing `set`/`keys` does not get swallowed by
   the handler.
3. **First-pass gating.** With `pass` = 1 (or no `pass` key), the device performs
   the dedup check. With `pass` ≠ 1, the device returns `{ok, Base}` unchanged and
   does not consult or modify the seen set.
4. **Subject default.** With no `dedup-subject`, the subject is the request's
   `body`. (E.g. two requests with the same `body` ID dedup against each other.)
5. **`request` subject.** With `dedup-subject = request` (exact, case-sensitive),
   the subject is the entire request; two requests with the same `message@1.0` ID
   are treated as the same subject. A value such as `Request`/`REQUEST` is NOT the
   sentinel and is instead looked up as a key.
6. **Named-key subject.** With `dedup-subject = <key>`, the subject is the value
   under `<key>` (looked up on `Base` then `Req`).
7. **Subject-key source precedence.** `dedup-subject` is taken from `Base` if
   present, otherwise from `Req`.
8. **Missing subject is pass-through.** With `pass` = 1 and the selected subject
   absent (the named key present on neither base nor request), the device returns
   `{ok, Base}` unchanged and records nothing — it MUST NOT skip and MUST NOT
   error.
9. **First occurrence proceeds and is recorded.** The first first-pass resolution
   of a present subject returns `{ok, Base'}` where `Base'` equals `Base` with its
   `dedup` key replaced by a seen set that now contains the subject's ID; the
   update uses an `explicit` (shallow) set of the `dedup` key. The new seen-set
   (`trie@1.0`) message is written to the content-addressed store.
10. **Repeat occurrence is skipped.** A subsequent first-pass resolution of a
    subject whose ID is already in the base's `dedup` seen set returns
    `{skip, Base}` with the base unchanged and no further store write.
11. **Subject identity is the `message@1.0` ID.** Two subjects that `message@1.0`
    assigns the same id are deduplicated together; two with different ids are not.
    IDs are 43-char base64url, never hex.
12. **Stack skip halts the pass.** Inside a `stack@1.0`, an already-seen subject's
    `{skip, _}` prevents the downstream devices from running for that request
    (their side effects do not occur), while an unseen subject lets them run
    exactly once. Demonstrable end-to-end: sending the same message twice through
    a stack of `dedup@1.0` + appending devices appends the downstream effect
    exactly once per distinct subject.
13. **Seen-set payload is irrelevant to the decision.** The value stored against a
    Subject ID (`slot` or `true`) does not affect whether a later identical
    subject is skipped; only key presence matters.
14. **No self-authored commitments / no network.** The device adds no commitment
    to its base message and makes no network call; its only side effect is the
    `trie@1.0` seen-set store write on the unseen path.

## 11. Out of scope

- The internal representation of messages and of the `dedup` seen set (the
  `trie@1.0` node/edge layout, link materialisation, where/whether the set is
  persisted across restarts). Only the observable membership semantics are
  normative.
- The cryptographic details of `message@1.0` ID derivation and of any commitment
  device (see `message@1.0`).
- The `stack@1.0` fold/pass mechanics beyond the `skip`/`ok`/`pass` contract this
  device emits into (see `stack@1.0`).
- The exact byte form / type of the seen-set payload value, and any future
  meaning ascribed to `slot`.
- Performance, memory footprint, and storage strategy of the seen set.

## Open questions

- **Subject ID `signed` vs content for an uncommitted subject** — resolved: now
  stated normatively in §5 ("Subject ID"). The device's dependency on
  `message@1.0` falling back to the content id (rather than erroring) for an
  uncommitted subject is load-bearing; §5 makes the required derivation explicit.
