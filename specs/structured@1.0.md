# `structured@1.0` — typed-message ⇄ TABM codec

- **Device name:** `structured@1.0`
- **Depends-on:** `httpsig@1.0` (its `commit`/`verify` keys delegate there). `message@1.0` for the surrounding message model.
- **Status:** Draft

## 1. Overview

`structured@1.0` is the **codec** that maps HyperBEAM's richly-typed message
representation to and from the **Type-Annotated Binary Message (TABM)** normal
form. TABM is the canonical, all-binary shape over which IDs and commitments are
computed; every other codec (HTTP-signature, ANS-104, flat, …) converts *through*
TABM. `structured@1.0` is the codec that knows how to (a) strip rich Erlang/AO
types down to binaries plus a type annotation (`from`, rich → TABM), and (b)
reconstitute them (`to`, TABM → rich).

The encoding is **content-addressed-critical**: two implementations that disagree
on a single output byte will compute different message IDs. This spec therefore
pins the wire format exactly.

## 2. Concepts & terminology

- **Rich message:** a message whose values may be binaries, **integers**,
  **floats**, **atoms** (interned symbolic constants), nested messages (maps), or
  **lists** of any of these.
- **TABM (Type-Annotated Binary Message):** a message whose values are **only**
  binaries or nested TABMs, plus a single reserved field **`ao-types`** that
  records the original rich type of any value that was not natively a binary.
  A key with no entry in `ao-types` is a plain binary.
- **Structured Fields (SF):** the value-encoding primitives of RFC 9651
  (HTTP Structured Fields): *bare items* (integers, tokens, strings, …),
  *lists*, and *dictionaries*. This device uses SF for value encodings and for
  the `ao-types` field itself. Where this spec says "SF integer / token / string
  / list / dictionary", it means the RFC 9651 serialisation of that construct.
- **Numbered message:** a message representing an ordered list, with keys `1`,
  `2`, `3`, … (1-based, in order) and a `.` ⇒ `list` marker in `ao-types`.

## 3. Device interface

- **Dispatch shape:** codec. The device answers the codec keys `from` (rich →
  TABM) and `to` (TABM → rich), plus helper keys `encode-types`,
  `decode-types`, and the commitment passthrough keys `commit` / `verify`.
- **`from` / `to`** each take the value to convert as the base and an optional
  request carrying `encode-types` and `bundle` (see §6).

## 4. Resolved keys (normative)

### `from` — rich → TABM
- **Reads:** the value; optional `encode-types` list in the request (default:
  all supported types — `integer`, `float`, `atom`, `list`); optional `bundle`
  flag (affects link handling, §6).
- **Behaviour:**
  - A **binary** value converts to itself.
  - A **map** converts field-by-field:
    1. Drop **private** keys and the regenerated keys `unsigned_id` and
       `content-digest`. Preserve a `commitments` field **verbatim** — it is
       never re-encoded by this codec.
    2. Process keys in **ascending sorted order** (byte-wise on the normalised,
       lower-cased key names) so the output is deterministic.
    3. Binary values pass through. Nested maps/lists recurse via `from`.
       Integer/float/atom/list values whose type is in `encode-types` are
       encoded to a binary (see §5) and the key→type pair is recorded; a value
       whose type is **not** in `encode-types` passes through in its rich form.
    4. If any types were recorded, emit an **`ao-types`** field: an SF
       **dictionary** whose members are `escaped-key` ⇒ SF-string(type-name)
       (a **double-quoted** string, e.g. `n="integer"`), ordered by sorted key.
       `escaped-key` is the key percent-encoded per §5.
  - A **list** converts by first turning it into a numbered message (`1`,`2`,…)
    and encoding that as a map; then, if `list` ∈ `encode-types` or the encoded
    map already carries an `ao-types`, the result is a map with `.` ⇒ `list`
    added to `ao-types`; otherwise the result is returned as a plain list.
- **Returns:** `{ok, TABM}`.

### `to` — TABM → rich
- **Reads:** the TABM; optional request (same shape).
- **Behaviour:** Parse `ao-types` to a map of key→type. For each field other than
  `ao-types`: a binary whose key has a recorded type is decoded per that type
  (§5); a nested map/list recurses via `to`; any already-rich value passes
  through. If `ao-types` contains `.` ⇒ `list`, the resulting numbered map is
  converted back to an ordered list.
- **Returns:** `{ok, RichMessage}`.

### `encode-types` / `decode-types`
- `encode-types` serialises a map of key→type into the `ao-types` SF dictionary
  string. `decode-types` parses an `ao-types` string back into a key→type map.
  Both operate on the request `body` (default: the base).

### `commit` / `verify`
- Delegate to **`message@1.0`** with `commitment-device` forced to `httpsig@1.0`,
  and return its result (see "Commit/verify (anti-recursion)" below and the
  `httpsig@1.0` spec for the leaf crypto). `message@1.0` does the commitment
  selection/merge; do NOT call `httpsig@1.0` directly (its `verify` needs the
  commitment fields merged in first).

## 5. Data formats & encodings (normative — byte-exact)

### Key escaping (for `ao-types` dictionary keys)
Percent-encoding: bytes `a`–`z`, `0`–`9`, and the literals `. - _ / ? &` are
emitted as-is; **every other byte** (including all uppercase letters) is emitted
as `%` followed by **two lowercase hex digits**. Decoding reverses this. (This
exists so keys are valid lowercase HTTP header names on the wire.)

### `ao-types` field
An SF **dictionary**: members are `escaped-key` ⇒ a member value that is an SF
**string** (a **double-quoted** string) naming the type — e.g. `n="integer"`,
`flag="atom"` — where the type name is `integer`, `float`, `atom`, `list`, or an
`empty-*` marker (§below). Members are ordered by sorted (unescaped) key and
serialised joined by `, ` (comma-space). A key absent from `ao-types` is a plain
binary. (Note: the type name is carried as a quoted SF *string*, NOT a bare SF
token — the quotes are part of the content-addressed bytes.)

### Per-type value encoding (`from`)
- **integer** → SF integer (decimal text). Type name `integer`.
- **float** → the platform default float-to-text form: a fixed-width ~20-
  significant-digit scientific notation (equivalent to Erlang's `float_to_binary/1`
  default). Examples (normative): `3.14` → `3.14000000000000012434e+00`, `2.0` →
  `2.00000000000000000000e+00`, `100.0` → `1.00000000000000000000e+02`, `0.1` →
  `1.00000000000000005551e-01`. Type name `float`. (This is reproducible and
  content-addressable; it is NOT the shortest-round-trip form.)
- **atom** → SF token of the atom's name (e.g. atom `ok` → token `ok`). Type name
  `atom`. On decode, the token is interned back to the atom; an atom name
  the decoder does not already know is an error.
- **list** → a list is **ALWAYS** encoded as a **numbered sub-message** (see
  "Lists as numbered messages" below), whether it is the whole operand OR the
  value of a map field. There is **NO** separate inline-list form emitted by
  `from`: a list-valued field `l => [E1,E2,…]` becomes a nested message
  `l => #{ "1" => from(E1), "2" => from(E2), …, "ao-types" => "...,.=\"list\"" }`,
  recursively, all the way down. Type name (on the *field carrying the numbered
  sub-message*, when that field itself is what `ao-types` annotates) is the
  numbered-message’s own `.`-marker, not a `key="list"` entry on the parent.
  (Note: a standalone SF-list form `(ao-type-<type>) <enc>` exists only as a
  **decode-robustness** path — `to`/`decode` MUST accept it — but `from` MUST NOT
  emit it. The reference never produces it.)
- **binary** values are never typed (no `ao-types` entry).

### Per-type value decoding (`to`)
Inverse of the above: integer via SF item parse; float via float text parse;
atom via SF token parse then intern; list via SF list parse, decoding any
`(ao-type-T) V` element recursively and reading bare elements as SF bare items.

### Empty / implicit values
A key may appear **only** in `ao-types` with a marker type `empty-binary` /
`empty-list` / `empty-message` and **no corresponding value field**. Such keys are
the message's **implicit keys**. Confirmed reference behaviour (validated by blind
reimplementation): on **decode**, an `empty-*`-typed key with no value field is
**dropped** — it does NOT appear in the decoded message (e.g. a TABM with
`x="empty-binary"` and no `x` field decodes to a message without `x`). On
**encode**, a plain empty binary value (`<<>>`) is **NOT** marked `empty-binary`;
it passes through as an ordinary (untyped) empty-binary value field. (The exact
conditions under which `from` emits an `empty-*` marker are not exercised by the
common path; see Open questions.)

### Lists as numbered messages
An ordered list is encoded as a message with 1-based numeric string keys
(`1`,`2`,…) in order, plus `.` ⇒ `list` in `ao-types`. Decoding reverses this,
restoring element order from the numeric keys.

**`.` marker ordering (normative, byte-load-bearing).** When the `.` ⇒ `list`
marker is present in `ao-types`, it MUST be the **first** member of the
serialised dictionary. The SF (RFC 9651) dictionary grammar does not accept `.`
as a key *start* in a non-leading position — a parser will reject
`a="integer", .="list"`. Because `.` (byte 0x2E) sorts before all digits and
letters, simply **including `.` in the same sorted-key ordering** as the other
members places it first automatically; an implementation MUST NOT append the
`.` marker after the already-built dictionary (that produces an `ao-types` string
that fails to re-parse). Sort the full member set (including `.`) together.

## 6. Ordering, freshness & caching

- **Determinism:** key processing and `ao-types` membership are both in sorted
  key order, so `from` is a pure deterministic function of its input (modulo the
  float caveat). This determinism is what makes TABM safe to hash.
- **Links / bundling:** when converting a map, link-valued fields are normalised;
  a request `bundle = true` keeps sub-messages inline (does not offload to
  links), `bundle = false`/absent may offload large sub-messages to links. This
  affects representation, not logical content.
- The codec performs no result caching of its own.

## 7. Security & authority

- The codec is unprivileged: it transforms representation only. It does not
  sign, verify, or grant authority — except that its `commit`/`verify` keys are
  thin pass-throughs to `httpsig@1.0`.
- `commitments` are copied verbatim and never altered by `from`/`to`, so a
  round-trip through the codec cannot silently change a message's signatures.

## 8. Errors

- `unexpected-type` — `to`/decode encountered an `ao-types` type token it does
  not recognise.
- atom-decode failure — decoding an `atom` whose name is unknown to the decoder.
  (Implementations MAY surface this as a generic decode error.)

## 9. Composition

- `structured@1.0` is the hub of the codec graph: every other codec defines its
  format relative to TABM, and conversions between any two formats route through
  TABM via this codec. `message@1.0`'s `id`/`commit` convert a message to TABM
  (through this codec) before hashing/signing.

## 10. Conformance (normative checklist)

1. A binary-only message round-trips unchanged through `from` then `to`.
2. `from` of a map with an integer/atom/list value produces a TABM whose value
   is the SF encoding of §5 and whose `ao-types` dictionary records the key with
   the correct type as a quoted SF string (`key="<type>"`); `to` reverses it
   exactly.
3. `ao-types` is an SF dictionary, ordered by sorted key, members joined by
   `, `, with keys percent-encoded per §5 (uppercase/non-`[a-z0-9.-_/?&]` bytes
   → `%xx` lowercase hex) and each value a double-quoted type name.
4. Integer encoding is SF-integer decimal text; atom encoding is an SF token of
   the atom name; float encoding is the platform default ~20-digit scientific form
   (§5). `from` NEVER emits the standalone `(ao-type-<type>)` SF-list form.
5. EVERY list — whether the whole operand or the value of a map field — encodes as
   a **numbered sub-message** (`1`,`2`,… keys) with `.` ⇒ `list` in `ao-types`,
   recursively; decoding restores the original ordered list. (`to` MUST also accept
   the standalone SF-list form on decode, but it is never produced by `from`.)
6. Private keys, `unsigned_id`, and `content-digest` are dropped by `from`; a
   `commitments` field is preserved byte-for-byte.
7. `from` is deterministic: the same rich message always yields byte-identical
   TABM (so identical message IDs), floats included (the float form is pinned, §5).
8. `commit`/`verify` behave exactly as `httpsig@1.0`'s `commit`/`verify`.

## 11. Out of scope

- The internal in-memory representation of rich messages, links, and TABMs.
- The full RFC 9651 grammar (assume a conforming SF implementation is available).
- The cryptography of `httpsig@1.0`.

## Open questions
- Exact set and spelling of `empty-*` markers (`empty-binary`, `empty-list`,
  `empty-message`) and the precise conditions under which `from` emits one (the
  common path never does; a plain `<<>>` passes through untyped).
- Keys whose bytes contain `/`, `?`, or `&` are NOT escaped by the `ao-types`
  key-escaping (§5) yet are invalid in the SF dictionary-key grammar, so such a
  key produces an unparseable `ao-types`. Edge case (such keys are unusual); a
  future revision should either escape them too or declare them unsupported.

## Commit/verify (anti-recursion)
`commit`/`verify` delegate to **`message@1.0`** with `commitment-device` forced to
`httpsig@1.0` — i.e. invoke `message@1.0`'s `commit`/`verify` (which performs
commitment **selection + field-merge + `type` defaulting**, then forwards the leaf
crypto to `httpsig@1.0`). Do NOT call `httpsig@1.0`'s `commit`/`verify` *directly*:
its `verify` needs each commitment's `type`/`signature`/`committed`/keyid merged in
first, so a direct call on a not-yet-merged base raises `badkey <<"type">>`; only
`message@1.0` does that selection. Do NOT re-resolve the `commit`/`verify` key on a
`structured@1.0`-deviced message either (that re-enters this codec and recurses
without bound) — delegate to `message@1.0` (≠ this codec, so no recursion) via the
`build-device` skill's "delegating to another device without recursion" primitive.
</content>
