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
- **`from` / `to`** each take the value to convert as the base; `from` reads an
  optional request carrying `encode-types` and `bundle` (see §6).

## 4. Resolved keys (normative)

### `from` — rich → TABM
- **Reads:** the value; optional `encode-types` list in the request (default:
  `integer`, `float`, `atom`, `list`; `binary` is typed only when listed);
  optional `bundle` flag (affects link handling, §6).
- **Behaviour:**
  - A **binary** value converts to itself.
  - A **map** converts field-by-field:
    1. Drop **private** keys and the regenerated key `unsigned_id`. Preserve a
       `commitments` field **verbatim** — it is never re-encoded by this codec.
    2. Process keys in **ascending sorted order** (byte-wise on the normalised
       key names; `from` keeps their case, and `to` lowercases every key but
       private keys, the keys in their values, and the IDs in `commitments`)
       so the output is deterministic.
    3. Binary values pass through, unless `binary` is in `encode-types` and the
       binary is not UTF-8 text. Nested maps/lists recurse via `from`.
       Integer/float/atom/binary values whose type is in `encode-types` are
       encoded to a binary (see §5) and the key→type pair is recorded; a value
       whose type is **not** in `encode-types` passes through in its rich form,
       except an integer whose magnitude is greater than the request's
       `max-integer`.
    4. If any types were recorded, emit an **`ao-types`** field: an SF
       **dictionary** with a member for each type, whose value is an inner list
       of the keys of that type as SF strings or byte sequences (e.g.
       `integer=("n")`), the types and the keys of each type ordered by their
       bytes (§5).
  - A **list** converts by first turning it into a numbered message (`1`,`2`,…)
    and encoding that as a map; then, if `list` ∈ `encode-types` or the encoded
    map already carries an `ao-types`, the result is a map with `.` ⇒ `list`
    added to `ao-types`; otherwise the result is returned as a plain list.
- **Returns:** `{ok, TABM}`.

### `to` — TABM → rich
- **Reads:** the TABM.
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
- `commit` delegates to **`message@1.0`** with `commitment-device` forced to
  `httpsig@1.0`, and returns its result (see "Commit/verify (anti-recursion)"
  below and the `httpsig@1.0` spec for the leaf crypto). `verify` calls
  `httpsig@1.0`'s `verify` with the base and the request, and returns its
  result.

## 5. Data formats & encodings (normative — byte-exact)

### Key escaping (for the keys in `ao-types`)
A key whose bytes are all `0x20`–`0x7e` is an SF **string**: its bytes are
emitted as-is, except `"` and `\`, which are emitted as `\"` and `\\`. A key
with any other byte is an SF **byte sequence**: its bytes in base64 (RFC 4648
§4, with `=` padding and the bits past the last byte zero) between colons, so
`café` is `:Y2Fmw6k=:`. Decoding reverses this, and refuses a key in any other
form: a byte sequence for a key that a string can contain, or one with a bit
set past the last byte. `to` lowercases the key.

### `ao-types` field
An SF **dictionary** with a member for each type: the type name ⇒ an inner list
of the keys of that type, each an SF **string** (a **double-quoted** string) or
an SF **byte sequence** (key escaping, above) — e.g.
`integer=("1" "n"), list=(".")` — where the type name is `atom`, `binary`,
`float`, `integer`, or `list`. Members are ordered by type name and the keys of
each member by their bytes; members are joined by `, ` (comma-space) and keys by
a space. A key absent from `ao-types` is a plain binary, which `to` decodes to a
link if it starts with `ao:`, except under `path`, `hashpath`, a private key or
in `commitments`. (Note: the quotes and colons around the keys are part of the
content-addressed bytes.)

### Per-type value encoding (`from`)
- **integer** → SF integer (decimal text). Type name `integer`.
- **float** → an SF Decimal of the float rounded to three places (to the even
  digit when halfway), with no trailing zero after the first fractional digit
  and at most 12 integer digits (a larger float gives the largest Decimal of its
  sign), then the parameter `binary64`: the float's 8 IEEE 754 bytes,
  big-endian, as base64url with no padding, in double quotes. Examples
  (normative): `0.0625` → `0.062;binary64="P7AAAAAAAAA"`, `0.1` →
  `0.1;binary64="P7mZmZmZmZo"`. Type name `float`. (This is reproducible and
  content-addressable.)
- **atom** → SF token of the atom's name (e.g. atom `ok` → token `ok`). Type name
  `atom`. On decode, the token is interned back to the atom; an atom name
  the decoder does not already know is an error.
- **list** → a list is **ALWAYS** encoded as a **numbered sub-message** (see
  "Lists as numbered messages" below), whether it is the whole operand OR the
  value of a map field. There is **NO** separate inline-list form emitted by
  `from`: a list-valued field `l => [E1,E2,…]` becomes a nested message
  `l => #{ "1" => from(E1), "2" => from(E2), …, "ao-types" => "…, list=(\".\")" }`,
  recursively, all the way down. Type name (on the *field carrying the numbered
  sub-message*, when that field itself is what `ao-types` annotates) is the
  numbered-message’s own `.`-marker, not a `list=("key")` entry on the parent.
  (Note: `to` refuses a standalone SF-list form `(ao-type-<type>) <enc>`, and
  `from` MUST NOT emit it.)
- **binary** → typed only when `binary` is in `encode-types` and the value is
  not UTF-8 text: base64url with no padding. Type name `binary`.

### Per-type value decoding (`to`)
Inverse of the above, refusing any other encoding (§8): integer from decimal
digits with no leading zero and `-` before any value but zero; float from its
`binary64` parameter, whose rounding must be the Decimal; atom via SF token
parse then intern; binary from base64url, refused if UTF-8 text; `list` only as
the type of the key `.`.

### Empty / implicit values
There are no `empty-*` marker types: an `ao-types` with an `empty-binary`,
`empty-list` or `empty-message` member does not parse, so `to` keeps it as a key
of the message and applies no types, and a type for a key with no value is
refused (§8). On **encode**, a plain empty binary value (`<<>>`) is **NOT**
typed; it passes through as an ordinary (untyped) empty-binary value field.

### Lists as numbered messages
An ordered list is encoded as a message with 1-based numeric string keys
(`1`,`2`,…) in order, plus `.` ⇒ `list` in `ao-types`. Decoding reverses this,
restoring element order from the numeric keys.

**`.` marker ordering (normative).** The `.` ⇒ `list` marker
is the member `list=(".")`, placed by its type name as every member is, so it is
the **last** member: `integer=("1" "2"), list=(".")`.

## 6. Ordering, freshness & caching

- **Determinism:** key processing is in sorted key order, and `ao-types` in
  sorted type and key order, so `from` is a deterministic function of its input.
  This determinism is what makes TABM safe to hash.
- **Links / bundling:** when converting a map, link-valued fields are normalised;
  a request `bundle = true` keeps sub-messages inline (does not offload to
  links), `bundle = false`/absent gives each sub-message (a map or a list) as
  its link, the value `ao:<ID>`, and writes it to the cache; the `linkify-mode`
  option `discard` gives the link without the write, and `false` keeps the
  sub-message inline. This affects representation, not logical content.
- The codec performs no result caching of its own.

## 7. Security & authority

- The codec is unprivileged: it transforms representation only. It does not
  sign, verify, or grant authority — except that its `commit`/`verify` keys are
  thin pass-throughs to `httpsig@1.0`.
- `from` copies `commitments` verbatim; `to` keeps their IDs and decodes each
  commitment without links, so a round-trip through the codec cannot silently
  change a message's signatures.

## 8. Errors

- `to` returns
  `{error, #{<<"status">> => 400, <<"body">> => <<"Invalid AO types.">>}}` for
  a typed value in any other encoding than §5's (an `atom` whose name is
  unknown to the decoder included), a type for a key with no binary value, or
  two keys of `ao-types` that lowercase to one key. `decode-types` returns the
  same for an `ao-types` in any other encoding than §5's.
- An `ao-types` type name that `to` does not recognise is no error: the
  `ao-types` does not parse, so `to` keeps it as a key of the message and
  applies no types.

## 9. Composition

- `structured@1.0` is the hub of the codec graph: every other codec defines its
  format relative to TABM, and conversions between any two formats route through
  TABM via this codec. `message@1.0`'s `id`/`commit` convert a message to TABM
  (through this codec) before hashing/signing.

## 10. Conformance (normative checklist)

1. A binary-only message with lowercase keys and no value that starts with
   `ao:` round-trips unchanged through `from` then `to`.
2. `from` of a map with an integer/atom value produces a TABM whose value
   is the SF encoding of §5 and whose `ao-types` dictionary records the key as a
   quoted SF string in the member of its type (`<type>=("key")`), or as an SF
   byte sequence for a key with a byte outside `0x20`–`0x7e`
   (`integer=(:Y2Fmw6k=:)` for `café`); `to` reverses it exactly.
3. `ao-types` is an SF dictionary with a member for each type, ordered by type
   name, members joined by `, `; each member's value is an inner list of the
   keys of that type as SF strings or byte sequences (§5), ordered by their
   bytes and joined by a space.
4. Integer encoding is SF-integer decimal text; atom encoding is an SF token of
   the atom name; float encoding is the SF Decimal of its rounding to three
   places with its `binary64` parameter (§5). `from` NEVER emits the standalone
   `(ao-type-<type>)` SF-list form.
5. EVERY list — whether the whole operand or the value of a map field — encodes as
   a **numbered sub-message** (`1`,`2`,… keys) with `.` ⇒ `list` in `ao-types`,
   recursively; decoding restores the original ordered list. (`to` refuses the
   standalone SF-list form, and `from` never produces it.)
6. Private keys and `unsigned_id` are dropped by `from`; a `commitments` field
   is preserved byte-for-byte.
7. `from` is deterministic: the same rich message always yields byte-identical
   TABM (so identical message IDs), floats included (the float form is pinned, §5).
8. `commit`/`verify` behave exactly as `httpsig@1.0`'s `commit`/`verify`.

## 11. Out of scope

- The internal in-memory representation of rich messages, links, and TABMs.
- The full RFC 9651 grammar (assume a conforming SF implementation is available).
- The cryptography of `httpsig@1.0`.

## Open questions
- None.

## Commit/verify (anti-recursion)
`commit` delegates to **`message@1.0`** with `commitment-device` forced to
`httpsig@1.0` — i.e. invokes `message@1.0`'s `commit` (which selects the keys to
commit and defaults `type`, then forwards the leaf crypto to `httpsig@1.0`).
`verify` calls `httpsig@1.0`'s `verify` *directly*, with the base and the
request: `message@1.0`'s `verify` calls this codec's `verify` for a commitment
whose `commitment-device` is `structured@1.0`, with the commitment as the
request, so calling `message@1.0`'s `verify` from here would call this codec
again, without end. Do NOT re-resolve the `commit`/`verify` key on a
`structured@1.0`-deviced message either (that re-enters this codec and recurses
without bound) — delegate to `message@1.0`'s `commit` or `httpsig@1.0`'s
`verify` (≠ this codec, so no recursion) via the `build-device` skill's
"delegating to another device without recursion" primitive.
</content>
