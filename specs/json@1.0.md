# `json@1.0` — JSON codec and HyperPATH-over-JSON device

- **Device name:** `json@1.0`
- **Depends-on:** `structured@1.0` (typed-message ⇄ TABM codec; this device round-trips through it), `message@1.0` (the surrounding message model and reserved-key surface), `httpsig@1.0` (its `commit`/`verify` keys delegate there). All three specs are provided to reimplementers.
- **Status:** Draft

## 1. Overview

`json@1.0` is the **JSON codec** for AO-Core messages: it serialises a message
to a JSON document and parses a JSON document back into a message, using
JSON-native types (objects, arrays, numbers, strings, booleans, `null`) wherever
JSON can represent the value directly. It is one of the interchangeable codecs in
the message-conversion graph (alongside `structured@1.0` and the HTTP-signature,
ANS-104, flat, … codecs); like all of them it defines its format relative to the
**Type-Annotated Binary Message (TABM)** normal form and converts *through* TABM.

Beyond the codec role, the device exposes a small set of **resolvable keys**
(`serialize`, `deserialize`) so a JSON document can be treated as a stateful
entity inside a HyperPATH — produced from any upstream message, or parsed from a
field of the current message — and then navigated or piped into further devices.

It is the canonical bridge between AO-Core's typed message model and the
ubiquitous JSON wire format used by external clients and HTTP APIs.

## 2. Concepts & terminology

- **Message:** a map of binary, lowercase, hyphenated keys to values (binaries,
  numbers, atoms, nested messages, or lists). Defined by the `message@1.0` spec.
- **TABM (Type-Annotated Binary Message):** the flat, all-binary normal form a
  message is reduced to before IDs/commitments are computed, in which every
  non-binary value is encoded to a binary and its original type recorded in a
  reserved `ao-types` field. Defined by the `structured@1.0` spec.
- **Structured (rich) message:** the fully-typed form in which integers, floats,
  atoms, and lists appear as their native types rather than as TABM binaries.
  Defined by the `structured@1.0` spec.
- **Atom:** an interned symbolic constant (e.g. `ok`, `true`) — a type AO-Core
  messages support but **JSON has no native representation for**. This device's
  type-handling rules (§5) centre on the asymmetry that JSON can carry floats,
  integers up to 9007199254740991 in magnitude, UTF-8 strings, booleans, arrays,
  objects, and `null` natively, but **not** atoms, other binaries or larger
  integers.
- **Codec keys vs resolvable keys:** the *codec keys* (`from`, `to`, and the
  commitment passthroughs) are invoked by the conversion subsystem when a caller
  converts a message to/from the `json@1.0` format; the *resolvable keys*
  (`serialize`, `deserialize`) are reached by ordinary HyperPATH resolution
  (`.../~json@1.0/serialize`). Both surfaces are normative.
- **Content type:** the MIME type this codec produces/consumes, `application/json`.

## 3. Device interface

- **Dispatch shape:** **explicit-keys codec + resolvable keys.** The device
  answers a fixed, named set of keys (below); it does **not** install a
  catch-all/default handler and does **not** capture arbitrary keys. Any key it
  does not name (including the message-manipulation and commitment-inspection
  reserved keys `keys`, `set`, `set-path`, `remove`, `id`, `commitments`,
  `committers`, `verify`) falls through to the base `message@1.0` device — except
  `commit`, `verify`, and `committed`, which this device overrides (§4).

- **Keys answered:**
  - Codec direction keys: **`to`** (message/TABM → JSON), **`from`** (JSON →
    message). These are the conversion-subsystem entry points.
  - Bundle hint for `structured@1.0`: **`to-hint`**.
  - Commitment surface: **`commit`**, **`verify`**, **`committed`**.
  - HyperPATH-over-JSON: **`serialize`**, **`deserialize`**.

- **Message shapes the device operates on:**
  - `to` accepts a **binary** (already-serialised content, or any opaque
    binary), a number, `true`, `false`, `null`, or a **message** (map).
  - `from` accepts either a **binary** (a JSON document) or a **message** (map,
    already decoded).
  - `serialize`/`deserialize` operate on the current resolution **base message**
    plus per-call request fields.

## 4. Resolved keys (normative)

### `to` — encode message → JSON
- **Reads:** the value to encode (`Base`); the request flag `bundle` (boolean,
  default `false`; `true` for a list), which the `bundle` of the message's
  `httpsig@1.0` commitment replaces.
- **Behaviour:**
  1. If the value is a **binary**, the device MUST JSON-encode that binary value
     directly and return the result. (A binary is treated as an opaque JSON
     value to be serialised, not as a pre-formed document to pass through.)
  2. If the value is a **message**, the device MUST:
     a. Strip the message's **private** keys (§2 of `message@1.0`) before
        encoding — private content MUST NOT appear in the JSON output.
     b. If `bundle` is `true`, fully **load any linked/lazy sub-values** so the
        emitted JSON is self-contained (no unresolved references). If `bundle`
        is `false` or absent, each nested message and list is given as its link,
        the string `ao:<ID>`, as `structured@1.0` §6 gives it.
     c. Produce a JSON-oriented intermediate in which **only `atom` values,
        binaries that are not UTF-8 text, and integers greater than
        9007199254740991 in magnitude are type-encoded** and **all other
        supported rich types remain JSON-native**: integers and floats become
        JSON numbers, strings/binaries become JSON strings, with `bundle = true`
        lists become JSON arrays (objects if an element is type-encoded) and
        nested messages JSON objects, and the type-encoded values are encoded
        per §5 (TABM-style typed string plus an `ao-types` entry).
        See §5 for the exact rule.
     d. JSON-encode that intermediate and return the document as a binary.
  3. The hashpath/commitment machinery MUST NOT be triggered as a side effect of
     encoding (encoding is a pure representation change).
- **Returns:** `{ok, JSONBinary}`.
- **Side effects:** none, except cache **reads** to materialise linked values,
  and without `bundle = true` the cache writes of `structured@1.0` §6.

### `from` — decode JSON → message
- **Reads:** the value to decode (`Base`); the request flag `bundle` (boolean,
  default `false`).
- **Behaviour:**
  1. If the value is already a **message** (map), the device MUST return it
     unchanged.
  2. If the value is a **binary**, the device MUST parse it as a JSON document.
     JSON objects become messages, arrays become lists, numbers become
     integers/floats, strings become binaries (links if they start with `ao:`),
     `true`/`false`/`null` are handled per §5.
  3. The device MUST **normalise** the parsed result so the output is a fully
     type-consistent message: any TABM type annotations carried *inside* the JSON
     (e.g. an `ao-types` field and its typed values, as emitted by `to` for
     atoms) are decoded to their rich types, and the message is re-reduced to
     TABM. Concretely, parsing then converting the result
     to rich (structured) form and back to TABM yields the normalised message;
     the visible effect is that atoms and any other embedded typed values are
     reconstituted rather than left as raw strings.
  4. The device MUST return the **TABM** form.
- **Returns:** `{ok, Message}`.
- **Errors:** a malformed JSON document is a decode failure (§8). An embedded
  `atom` whose name is unknown to the decoder raises
  `error({case_clause, {error, ErrorMap}})`, with `ErrorMap` the
  `structured@1.0` error (§8); an `ao-types` with a type name the decoder does
  not recognise is kept as a key, and no types are applied.
- **Side effects:** the cache writes of `structured@1.0` §6.

### `commit` — produce a commitment
- **Reads:** the target message (`Base`); the request `Req`.
- **Behaviour:** MUST delegate to **`message@1.0`** with `commitment-device`
  forced to `httpsig@1.0` — `message@1.0` reduces the (possibly rich) base to
  TABM, defaults `type`, and forwards the leaf crypto to `httpsig@1.0`, returning
  the committed message. Do NOT call `httpsig@1.0` directly on the base (it expects
  a TABM + `type`; a rich/HyperPATH base crashes). The JSON codec performs no
  cryptography of its own.
- **Returns:** `{ok, CommittedMessage}`.

### `verify` — check commitments
- **Reads:** the target message (`Base`); the request `Req`.
- **Behaviour:** MUST call `httpsig@1.0`'s `verify` with `Base` and `Req` and
  return its result. `message@1.0`'s `verify` calls it for a commitment whose
  `commitment-device` is `json@1.0`, with the commitment as `Req`.
- **Returns:** `{ok, Boolean}`.

### `committed` — list committed keys
- **Reads:** the target value (`Base`); the request `Req`.
- **Behaviour:**
  1. If `Base` is a **binary** (a JSON document), the device MUST first decode it
     via `from` (using `Req`), then proceed on the resulting message.
  2. The device MUST return the committed keys of the (decoded) message across
     **all** of its commitments, as the `message@1.0` committed-key surface
     defines.
- **Returns:** `{ok, [Key]}`.

### `serialize` — emit the base message as a JSON HTTP body
- **Reads:** the current base message (`Base`); the request `Req` (passed through
  to `to`, so `bundle` is honoured here as well).
- **Behaviour:** MUST serialise `Base` to JSON via the same rules as `to`, and
  return a small message carrying the result as an HTTP-shaped body.
- **Returns:** `{ok, M}` where `M` is a message with exactly:
  - `content-type` ⇒ `application/json`
  - `body` ⇒ the JSON document (binary), identical to what `to` would return for
    `Base` under the same request.
- **Side effects:** none beyond `to`'s.

### `deserialize` — parse a JSON field of the base message into a message
- **Reads:** the request field `target` (binary, **default `body`**); the value
  located at `target` within `Base`.
- **Behaviour:**
  1. Resolve the value at key/path `target` against `Base`.
  2. If no value is found at `target`, the device MUST return the error message
     in §8 (HTTP-shaped, status `404`).
  3. Otherwise the device MUST decode that value via `from` (using `Req`) and
     return the result.
- **Returns:** `{ok, Message}` on success, or the §8 error map on a missing
  target.
- **Side effects:** those of `from`.

## 5. Data formats & encodings (normative)

### Direction and the JSON⇄message mapping
The device's *target format* is JSON text; its *internal pivot* is TABM (every
codec converts through TABM). Implementations MUST realise the following
observable mapping. Internal representation of intermediate forms is out of scope
(§11) — only the JSON⇄message correspondence below is normative.

**Encoding (`to`/`serialize`), message → JSON, per value type:**

| Message value | JSON output |
|---|---|
| binary / string | JSON string; a binary that is not UTF-8 text is its base64url form, typed `binary` in `ao-types` |
| integer | JSON number (integer, no fraction/exponent); one greater than 9007199254740991 in magnitude is a JSON string of its decimal text, typed `integer` in `ao-types` |
| float | JSON number |
| nested message (map) | JSON object with `bundle = true`, else the string `ao:<ID>` (§4) |
| list | JSON array (elements encoded by the same rules) with `bundle = true`, else the string `ao:<ID>` (§4); with a type-encoded element, an object with the keys `1` to `N` and an `ao-types` |
| **atom** | JSON **string** carrying the TABM type-encoded form, **plus** an `ao-types` entry naming the key's type as `atom` (see below) |

Atoms, binaries that are not UTF-8 text and integers greater than
9007199254740991 in magnitude have **no** JSON-native form, so they alone are
type-annotated on the way out: the value is emitted in its `structured@1.0`
form and the enclosing object gains an `ao-types` field — an SF dictionary with
the key in the member of the value's type, e.g. `atom=("key")`. Every other
supported type is emitted natively and carries **no** `ao-types` entry. The
well-known atoms `true`/`false` are encoded this way as well (as typed `atom`
values), not as JSON booleans — see decoding note below.

**Decoding (`from`/`deserialize`), JSON → message, per JSON value:**

| JSON value | Message value |
|---|---|
| object | nested message; any `ao-types` field is consumed to retype its siblings |
| array | list |
| number without fraction or exponent | integer |
| number with a fraction or exponent | float |
| string | binary (unless its key is retyped by an `ao-types` sibling, e.g. an `atom`), or a link if it starts with `ao:` (`structured@1.0` §5) |
| `true` / `false` | boolean values, normalised to the `atom`s `true` / `false` |
| `null` | the `atom` `null` |

`null` and the JSON booleans therefore round-trip through the message model as
atoms (`null`, `true`, `false`). A field that was an `atom` on encode is restored
to that atom on decode via its `ao-types` annotation; a JSON string with no such
annotation decodes to a plain binary, or a link if it starts with `ao:`.

### `ao-types`, key escaping, and per-type encodings
The `ao-types` field, the escaping of its keys, the encodings of
`integer`/`atom`/`list`/`float`/`binary`, and the numbered-message
representation of lists are all defined **exactly** by the `structured@1.0` spec
and MUST match it byte-for-byte. This device adds **no** new type tokens or
escaping rules; it only chooses (in `to`) to leave floats, lists, UTF-8 text and
integers up to 9007199254740991 in magnitude in JSON-native form while
type-encoding atoms, other binaries and larger integers.

### JSON number, string, and document conventions
- Integers MUST be emitted without a decimal point or exponent; a JSON number
  with a fractional part or exponent decodes to a float.
- Strings are UTF-8 JSON strings; all standard JSON string escaping applies.
- The encoded document is returned as a single binary; no surrounding whitespace,
  framing, or content-length is added by the codec itself.
- IDs, addresses, and any other content-addressed material that appears as a
  value are carried as their ordinary base64url string forms (never hex) — this
  device does not re-encode them.

### Object key ordering
JSON object member order is **not** significant and MUST NOT be relied upon by
consumers. Implementations MAY emit members in any order (the reference pivots
through TABM, which processes keys in sorted order, but this ordering is not part
of the contract). Content-addressing is performed over TABM, not over the JSON
text, so JSON key order does not affect a message's ID.

## 6. Ordering, freshness & caching

- Encoding and decoding read the cache only to materialise links, and without
  `bundle = true` make the cache writes of `structured@1.0` §6; the device
  performs no result caching of its own.
- JSON is **not** a canonical, content-addressed wire form for this device:
  message IDs are computed over TABM (via `structured@1.0`), never over the JSON
  text. Two JSON documents that differ only in insignificant whitespace or member
  order denote the same message and MUST decode equal.
- `serialize`/`deserialize` are stateless transforms of the supplied base and
  request; they have no freshness semantics beyond those of whatever produced the
  base message in the HyperPATH.

## 7. Security & authority

- The codec is **unprivileged**: `to`/`from`/`serialize`/`deserialize` transform
  representation only and grant no authority.
- **Private keys MUST NOT be serialised.** `to` (and therefore `serialize`)
  strips the message's private section before encoding; private content never
  reaches the JSON output.
- `commit`/`verify` are thin pass-throughs to the `httpsig@1.0` commitment
  device, and `committed` to `message@1.0`; all signing/verification trust
  assumptions are those of `httpsig@1.0`. This device never invents or weakens
  a commitment.
- A round-trip `message → to → from` MUST NOT fabricate or strip commitments
  beyond the documented private-key removal; commitment integrity is governed by
  the codec/commitment rules of `structured@1.0` and `httpsig@1.0`.

## 8. Errors

- **JSON decode failure** — `from`/`deserialize` given a binary that is not a
  well-formed JSON document. Surfaced as a parse error (the device does not
  define a bespoke atom for this; it propagates the JSON parser's failure).
- **`deserialize` target missing** — when the value at `target` is absent in the
  base message, `deserialize` MUST return a **resolution error** carrying an
  HTTP-shaped map: the return value is **`{error, ErrorMap}`** (NOT `{ok, _}` and
  NOT a bare map — it is the error branch of the resolution, so a caller reading
  the result sees an error whose payload is `ErrorMap`). `ErrorMap` is:
  - `status` ⇒ `404` (an integer, not a binary)
  - `body` ⇒ the EXACT binary
    `<<"JSON payload not found in the base message.Searched for: ", Target/binary>>`
    — i.e. the literal prefix `JSON payload not found in the base message.Searched
    for: ` (NOTE: no space between `message.` and `Searched`) followed by the
    requested `target` value. (This exact byte sequence is part of the conformance
    contract.)
- **Embedded-type decode errors** — decoding JSON with an `atom` whose name is
  unknown to the decoder raises `error({case_clause, {error, ErrorMap}})`, with
  `ErrorMap` the `structured@1.0` error. An `ao-types` with an unrecognised type
  name is no error: it is kept as a key, and no types are applied.

All device-specific error atoms are hyphenated; this device introduces no new
atoms of its own beyond the HTTP-shaped `deserialize` error above and those
inherited from its dependencies.

## 9. Composition

- **As a codec:** any caller may request conversion of a message **to**
  `json@1.0` (yielding the JSON document) or **from** `json@1.0` (yielding a
  message); the conversion subsystem routes through this device's `to`/`from`.
  Passing `bundle = true` on a `to`/`serialize` conversion inlines linked
  sub-messages so the JSON is self-contained.
- **HyperPATH chaining (serialise):** `/<upstream>/~json@1.0/serialize` takes the
  message produced by `<upstream>` and returns its JSON representation, e.g.
  `/~meta@1.0/info/~json@1.0/serialize` serialises node info as JSON. The
  returned message carries `content-type: application/json` and the JSON in
  `body`, so it composes as an HTTP response body.
- **HyperPATH chaining (deserialise):** binding `~json@1.0` and resolving
  `deserialize` parses a JSON field (default `body`) of the current message into
  a message, which subsequent path segments can then navigate as ordinary
  message keys. Setting `target` selects a different source field.
- **Reserved keys fall through:** because the device declares no default handler,
  `keys`/`set`/`set-path`/`remove`/`id` and the other `message@1.0` reserved keys
  resolve against the base `message@1.0` device as normal; only `commit`,
  `verify`, and `committed` are overridden (`commit` and `verify` to use
  `httpsig@1.0`).

## 10. Conformance (normative checklist)

An implementation MUST exhibit every behaviour below.

1. **Content type.** The device has no `content-type` key: it falls through to
   `message@1.0`.
2. **Binary encode passthrough.** `to` of a binary value returns that value
   JSON-encoded (a JSON string), not the raw bytes.
3. **Object/array/number mapping.** `to` of a message emits a JSON object; with
   `bundle = true`, nested messages emit nested objects and lists JSON arrays
   (objects if an element is type-encoded); integers up to 9007199254740991 in
   magnitude emit JSON numbers without a fraction/exponent; floats emit JSON
   numbers; binaries emit JSON strings.
4. **Atom type-encoding.** `to` of a message containing an `atom` value emits
   that value as the `structured@1.0` typed form and adds the key to the `atom`
   member of `ao-types` (`atom=("key")`); floats, lists, and integers up to
   9007199254740991 in magnitude carry **no** `ao-types` entry. The `ao-types`
   field and key escaping match `structured@1.0` byte-for-byte.
5. **Private stripping.** `to`/`serialize` of a message with private keys
   produces JSON that contains none of those keys or their values.
6. **Bundling.** `to`/`serialize` with `bundle = true` produces JSON with all
   linked sub-values inlined (self-contained, no unresolved links); with
   `bundle` absent/false, linked sub-values need not be inlined.
7. **Map passthrough on decode.** `from` of a value that is already a message
   returns it unchanged.
8. **JSON decode mapping.** `from` of a JSON document yields a message in which
   objects→messages, arrays→lists, integer-valued numbers→integers,
   fractional/exponent numbers→floats, strings→binaries (links if they start
   with `ao:`), `true`/`false`→the atoms `true`/`false`, and `null`→the atom
   `null`.
9. **Atom round-trip.** A message whose value is an atom, encoded by `to` and
   decoded by `from`, yields the original atom (the `ao-types` annotation is
   honoured on decode); the booleans and `null` round-trip as the atoms
   `true`/`false`/`null`.
10. **TABM form.** `from` returns the TABM form.
11. **Commitment delegation.** `commit` and `verify` produce/verify commitments
    via `httpsig@1.0` (the commitment device is forced to `httpsig@1.0`), and
    their results equal `httpsig@1.0`'s for the same input.
12. **`committed` on a JSON string.** `committed` given a JSON document decodes
    it first (via `from`) and then returns the committed keys of the resulting
    message across all commitments.
13. **`serialize` shape.** `serialize` returns a message with exactly
    `content-type = application/json` and `body` equal to the JSON encoding of
    the base message under the same request.
14. **`deserialize` default target.** `deserialize` with no `target` reads the
    base message's `body` field and decodes it as JSON.
15. **`deserialize` custom target & miss.** `deserialize` with `target = K`
    reads field `K`; if `K` is absent, it returns an HTTP-shaped error with
    `status = 404` and a `body` naming the searched target.
16. **JSON insignificance.** Two JSON documents differing only in object
    member order or insignificant whitespace decode to equal messages; a
    message's ID does not depend on the JSON text (IDs are computed over TABM).
17. **No key capture.** Resolving `keys`/`set`/`set-path`/`remove`/`id` on a
    message bound to this device behaves as `message@1.0` (the device does not
    swallow them).

## 11. Out of scope

- The internal in-memory representation of messages, TABMs, and the structured
  intermediate forms this device pivots through.
- The full `ao-types` / Structured-Fields grammar, key-escaping algorithm, and
  per-type byte encodings — these are defined by `structured@1.0` and assumed
  here.
- The cryptography of commitments — defined by `httpsig@1.0`.
- The choice of underlying JSON parser/serialiser and any of its
  implementation-specific behaviours not pinned in §5 (e.g. JSON object member
  ordering, which is explicitly non-normative).
- Performance, streaming, and storage strategy.

## Open questions

- **Boolean/`null` asymmetry.** JSON `true`/`false`/`null` decode to the atoms
  `true`/`false`/`null`, and atoms re-encode (via `to`) as typed `ao-types`
  strings rather than JSON booleans/`null`. A value that entered as a JSON boolean
  therefore does **not** survive a `from`→`to` round-trip as a JSON boolean — it
  becomes a typed-`atom` string with an `ao-types` annotation. Whether
  `true`/`false`/`null` should instead round-trip back to native JSON literals is
  unresolved; this spec documents the observed atom-based behaviour.
- **`target` as a path.** `deserialize`'s `target` is resolved against the base
  message; whether it is restricted to a single key or may be a multi-segment
  sub-path is not pinned here. Treat it as "the value located at `target`",
  matching ordinary key/path resolution, and confirm multi-segment support
  during validation.
