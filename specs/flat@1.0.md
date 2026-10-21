# `flat@1.0` — flat path-delimited ⇄ TABM codec

- **Device name:** `flat@1.0`
- **Depends-on:** `structured@1.0` (the canonical TABM codec these values are drawn from / round-tripped through), `message@1.0` (the surrounding message model), `httpsig@1.0` (its `commit`/`verify` keys delegate there). All three specs are provided to reimplementers.
- **Status:** Draft

## 1. Overview

`flat@1.0` is a **codec** that maps a TABM (the all-binary message normal form;
see `structured@1.0`) to and from a **flat message**: a single-level map whose
keys are path-delimited binaries and whose values are binaries (or, recursively,
flat sub-maps). It exists to express a nested message as a set of
`a/b/c → value` entries — the shape used by path-addressed, header-style, and
line-oriented transports.

The codec is **not** itself a rich-type codec: it only moves the *structure*
(nesting) of a message between "nested maps" and "delimited keys". The values it
carries are already TABM binaries (or nested maps thereof); typing/encoding of
non-binary values is the job of `structured@1.0`, not this device. Because TABM
is the form over which message IDs are computed, the byte layout of keys and the
delimiter rules below are content-addressing-relevant and are pinned exactly.

## 2. Concepts & terminology

- **TABM (Type-Annotated Binary Message):** the normal form whose values are
  only binaries or nested TABMs (see `structured@1.0`). `flat@1.0` consumes and
  produces TABMs: `from` yields a (nested) TABM, `to` yields a flat TABM.
- **Flat message:** a map in which every key is a **path** — one or more
  **segments** joined by the path delimiter — and every value is a binary or a
  nested flat message. A flat message has no rich types of its own.
- **Segment:** one component of a path between delimiters. Segments are the keys
  of the corresponding nested message (lowercase, hyphenated binaries in the
  general message model).
- **Path delimiter:** the single byte `/` (U+002F SOLIDUS, `0x2F`). It separates
  segments in a flat key. There is **no** escape mechanism for a literal `/`
  inside a segment (see §5).
- **Numbered message:** a message representing an ordered list, with 1-based
  decimal-string keys `1`, `2`, `3`, … (see `structured@1.0`).

## 3. Device interface

- **Dispatch shape:** **codec.** The device answers the codec keys `from`
  (flat → TABM, i.e. *unflatten*) and `to` (TABM → flat, i.e. *flatten*), the
  commitment passthrough keys `commit` / `verify`, and `deserialize` (parse the
  text form, §5). It maintains no state and reads no node options that affect its
  output.
- **Resolved-key surface (normative).** The device's resolved keys are exactly
  `from`, `to`, `commit`, `verify`, `deserialize`. **`serialize` is a library
  helper, NOT a resolved device key** (a caller produces the text form via the
  helper, not via path resolution). An implementation MUST NOT expose
  `serialize` as a resolvable key, and MUST expose `deserialize` as one.
- **Codec entry signature.** The codec keys are invoked by the platform's
  conversion layer **directly** as `from(Base, Req, Opts)` / `to(Base, Req, Opts)`
  (the 3-argument key-function shape), not only via generic path resolution. Each
  returns `{ok, Converted}`.
- **`from` / `to`** each take the value to convert as the base (`Base`) and a
  request (`Req`); neither key reads any field from `Req` that changes the
  result (the request is forwarded unchanged through recursion only).
- **Operand types.** Both `from` and `to` accept a **binary** or a **map** as
  the operand; `to` additionally accepts a **list**. Any other operand type is
  outside the contract.

## 4. Resolved keys (normative)

### `from` — flat → TABM (unflatten)
- **Reads:** the operand (`Base`).
- **Behaviour:**
  - A **binary** operand MUST be returned **unchanged** (identity passthrough).
  - A **map** operand is converted by processing **each** `Key → Value` entry
    and **accumulating** the results into a single nested map:
    1. **Recurse into the value first:** apply `from` to `Value`. A binary value
       returns itself; a nested map value is unflattened recursively. (Values
       are thus themselves unflattened, so nesting expressed inside a value map's
       keys is also expanded.)
    2. **Split the key into path segments** (§5, "Key → segments"). For a
       well-formed key this yields an ordered list of one or more **non-empty**
       segments.
    3. **Insert** the (recursively-unflattened) value at that segment path,
       creating intermediate sub-maps as needed. Insertion is a **deep set**:
       - For a path of one segment `[K]`: if both the existing value at `K` and
         the new value are maps, the result is their **shallow merge** (new
         entries win on key collision); otherwise the new value **replaces** any
         existing value at `K`.
       - For a path `[K | Rest]` (length > 1): descend into the sub-map at `K`
         (an empty map if absent) and deep-set `Rest` within it, then store the
         updated sub-map at `K`.
    4. **Degenerate / empty keys are NOT a defined drop.** A key that reduces to
       zero segments has **no clean semantics** and is **ill-formed** (see §5 and
       §8): the empty binary `<<>>` and the empty list do not "vanish" — they
       produce an implementation-specific degenerate key — and a key consisting
       solely of delimiters (e.g. `<<"/">>`) has **unspecified** behaviour and an
       implementation MAY raise. Well-formed TABM never carries such a key (all
       TABM keys are non-empty lowercase binaries), so a conformant producer MUST
       NOT emit one; a consumer's behaviour on one is **out of scope** (§11).
  - An **empty map** operand returns an **empty map**.
- **Empty-list value note.** If any value in the operand map is the empty list
  `[]`, the implementation MAY emit a diagnostic but MUST NOT fail; processing
  continues and that entry is deep-set with an empty-list value. (Well-formed
  TABM inputs do not contain bare empty lists — `structured@1.0` represents the
  empty list as an `ao-types` marker — so this is an edge-case guard, not a
  supported encoding.)
- **Returns:** `{ok, TABM}` (a nested map), or the unchanged binary for a binary
  operand.
- **Side effects:** none.

### `to` — TABM → flat (flatten)
- **Reads:** the operand (`Base`).
- **Behaviour:**
  - A **binary** operand MUST be returned **unchanged** (identity passthrough).
  - A **list** operand MUST first be converted to a **numbered message** (keys
    `1`, `2`, …, one per element in order; see `structured@1.0`) and then
    flattened as a map.
  - A **map** operand is flattened by processing **each** `Key → Value` entry and
    merging the results into a single single-level map:
    1. **Recurse into the value first:** apply `to` to `Value`.
    2. **If the recursive result is a (sub-)map**, then for **every**
       `SubKey → SubValue` entry of that sub-map, emit an entry whose key is the
       **delimiter-join** of `[Key, SubKey]` (§5, "Segments → key") and whose
       value is `SubValue`. (This prefixes the parent segment onto each already-
       flattened child key, so arbitrarily deep nesting collapses one level per
       recursion.)
    3. **Otherwise** (the recursive result is a binary / scalar), emit a single
       entry whose key is the delimiter-join of the one-element list `[Key]`
       (i.e. the normalised `Key` itself) and whose value is that result.
  - An **empty map** operand returns an **empty map**.
- **Returns:** `{ok, FlatMap}` (a single-level map of path-keys → binary values),
  or the unchanged binary for a binary operand.
- **Side effects:** none.

### `commit` / `verify`
- Delegate to `httpsig@1.0`: force `commitment-device = httpsig@1.0` (set it into
  the request) and delegate to the base message device's commit/verify, returning
  its result unchanged. `commit` returns `{ok, CommittedMessage}`; `verify`
  returns `{ok, Boolean}` (the codec key MUST return the `{ok, _}`-wrapped result,
  not a bare message). The default committer selection follows `message@1.0`
  (`verify` defaults to **all** commitments). `commit` produces a commitment over
  the message; `verify` checks it. See the `httpsig@1.0` spec. The codec adds no
  commitment logic of
  its own.

## 5. Data formats & encodings (normative — byte-exact)

### Path delimiter
The delimiter between segments is the single byte `/` (`0x2F`). It is used both
to **split** a flat key into segments (`from`) and to **join** segments into a
flat key (`to`). No other byte is treated as a delimiter.

### Key → segments (used by `from`)
Given a flat key, produce its ordered segment list as follows. The key is one of:

- **A binary** `K`:
  - If `K` contains **no** `/` byte and is non-empty → segment list is the single
    segment `[K]` (the whole key, byte-for-byte, is one segment).
  - If `K` contains one or more `/` → split `K` on every `/` and **drop all empty
    pieces** (i.e. leading, trailing, and consecutive `//` produce **no** empty
    segments). The remaining non-empty pieces, in order, are the segments. (So
    `<<"a/b">>` → `[<<"a">>, <<"b">>]`; `<<"/a//b/">>` → `[<<"a">>, <<"b">>]`.)
  - **Degenerate binaries are ill-formed (§8):** the empty binary `<<>>` reduces
    to no segments and yields an implementation-specific degenerate key rather
    than a clean drop; the all-delimiter binary `<<"/">>` reduces to zero
    segments and an implementation MAY raise. A conformant producer MUST NOT emit
    either.
- **A list** of sub-keys: each element is itself converted to a segment list by
  these same rules and the results are **concatenated** in order (the list is
  flattened). Thus `[<<"y">>, <<"z">>]` → `[<<"y">>, <<"z">>]`, and a list
  element that itself contains `/` (e.g. `[<<"a/b">>, <<"c">>]`) expands to
  `[<<"a">>, <<"b">>, <<"c">>]`. The empty list reduces to no segments and is
  ill-formed (§8), as above.
- **An atom** → the single segment `[atom-name-as-binary]`.
- **An integer** → the single segment of its **decimal** text (e.g. `1` →
  `<<"1">>`).

A **string** (an Erlang list of character codepoints) is treated as a single
segment equal to that string's binary form — it is **not** split into characters.

### Segments → key (used by `to`)
Given an ordered list of segments, produce the flat key:

1. **Normalise** each segment to a binary: a binary stays as-is (byte-for-byte,
   **case preserved** — no lower-casing); an atom becomes its name; an integer
   becomes its decimal text; a nested non-string list is itself joined with `/`.
2. **Join** the normalised segments with the single byte `/`.
3. **Collapse delimiters:** the joined binary is then split on `/` with **all
   empty pieces dropped** and re-joined with a single `/`. Consequently the
   output key has **no** leading `/`, **no** trailing `/`, and **no** empty
   (`//`) segment, and any `/` already present inside a segment is folded into
   the delimiter structure (it does not survive as data — see "No segment
   escaping" below).

### No segment escaping (round-trip caveat)
There is **no** escape sequence for a literal `/` inside a segment. A segment
whose own bytes contain `/` is therefore **indistinguishable** from a segment
boundary: on `to` it widens the path, and on `from` it splits into multiple
segments. **Round-trip fidelity (`from(to(M)) == M`, `to(from(F)) == F`) holds
only for messages whose segment/key names contain no `/` byte.** Implementations
MUST NOT introduce escaping; the format is delimiter-collapsing by construction.

### Newline freedom
A flattened key MUST NOT contain a newline (`\n`, `0x0A`). This holds
automatically for any TABM whose keys are well-formed (lowercase hyphenated
binaries) and is what makes the line-oriented text form (below) unambiguous.

### Text serialisation form (informative; non-content-addressed)
A flat message has a canonical human-readable **text** rendering used for
display and line transports (it is **not** the content-addressed TABM form and
is not consumed by `from`/`to`):

- **Serialise:** flatten the message (`to`), then for each key in **ascending
  byte-wise sorted** key order emit the line
  `<key>` `: ` (colon-space) `<value>` followed by a single `\n`. Concatenate the
  lines. (Sorting makes the text deterministic; the flat map itself is unordered.)
- **Deserialise:** split the input on `\n`; for each line, split on the **first**
  `: ` (colon-space) into `key` and `value` (a line without a `: ` separator is
  ignored); collect into a flat map; then decode that flat map **through
  `structured@1.0`** (i.e. treat the result as a flat TABM and convert to the
  rich form). Because values are split on the *first* `: ` only, a value
  containing `: ` is preserved.

This text form is lossy for values that contain `\n` or that begin in a way that
collides with the `: ` split, and for keys/values requiring rich typing beyond
plain binaries; it is intended for diagnostics and simple transports, not for
content addressing.

## 6. Ordering, freshness & caching

- **Determinism of structure.** `from` and `to` are pure deterministic functions
  of their operand: the *set* of `(path, value)` pairs produced is fully
  determined by the input. The flat map and the nested map are both **unordered**
  containers, so neither `from` nor `to` depends on, or guarantees, any iteration
  order. (The **text** serialisation imposes sorted-key order to make the byte
  output deterministic; see §5.)
- **Key collisions (within one `from`).** If two distinct flat keys reduce to the
  same nested leaf path, the result at that leaf is the **deep-set merge** of
  their values (map+map → shallow merge; otherwise later-applied value replaces
  earlier). Because the operand is an unordered map, "later-applied" is **not**
  defined by the spec — therefore a flat message that contains colliding non-map
  leaves at the same path is **ill-formed** and its result is unspecified
  (implementations MUST NOT rely on a particular winner). Well-formed inputs have
  no such collisions.
- The codec performs **no** result caching of its own and reads no
  `cache-control` directives.

## 7. Security & authority

- The codec is **unprivileged**: it transforms representation (structure) only.
  It does not sign, verify, or grant authority — except that its `commit`/`verify`
  keys are thin pass-throughs to `httpsig@1.0`.
- `from`/`to` move structure but do not re-type or re-encode values, and they do
  not special-case a `commitments` field beyond treating it as an ordinary nested
  map; round-tripping structure through this codec does not by itself add or
  remove a signature. (Signing/verification is reached only via `commit`/`verify`,
  which defer to `httpsig@1.0`.)
- Failure mode: the codec does not fail-closed or fail-open on authority because
  it makes no authority decisions; malformed structure yields the unspecified
  results noted in §6, not a security decision.

## 8. Errors

`flat@1.0` defines **no** error atoms of its own for the `from`/`to` conversions:
every supported operand (binary, map, and — for `to` — list) yields `{ok, _}`.

- An operand of an **unsupported type** (neither binary, map, nor — for `to` —
  list) is outside the contract; behaviour is unspecified (an implementation will
  raise rather than return a defined error atom).
- A flat key that is a **non-string list containing a string-list element** and
  similar exotic key shapes are parsed by the segment rules of §5; they do not
  produce a defined error.
- A **degenerate key** that reduces to zero segments (an all-delimiter key such
  as `<<"/">>`, or an empty list) has **no defined error atom**: an implementation
  MAY raise. The empty binary `<<>>` likewise has no defined error; it yields an
  implementation-specific degenerate key. None of these arise from well-formed
  TABM, so they are out of scope (§11) rather than a contracted error.
- `commit`/`verify` surface whatever `httpsig@1.0` returns (including its error
  atoms); this codec adds none.

## 9. Composition

- `flat@1.0` is a structural codec layered **on top of** TABM: a typical pipeline
  is `rich-message → structured@1.0 (from) → TABM → flat@1.0 (to) → flat map`,
  and the reverse for ingestion. Conversions between the flat form and any other
  representation route **through** TABM, never directly.
- The text serialisation (§5) composes the flatten step with a sorted-line
  renderer; its inverse composes the line parser with `structured@1.0`'s `to`.
  An implementer building the text round-trip MUST place `structured@1.0` on the
  decode side (the parsed flat map is a TABM and must be de-typed to become rich).
- Because flattening collapses nesting into delimited keys, it is the natural
  adapter for path-addressed and header-style transports where keys are flat
  strings; un-flattening (`from`) reconstructs the nested message a path-based
  caller addressed piecewise.

## 10. Conformance (normative checklist)

An implementation MUST exhibit all of the following (each checkable by
constructing the input and comparing the converted output):

1. **Binary passthrough.** `from(B)` and `to(B)` return the binary `B` unchanged,
   for any binary `B`.
2. **Empty map.** `from(#{})` and `to(#{})` each return an empty map.
3. **Single segment.** `to(#{ <<"a">> => V })` (V a binary) yields
   `#{ <<"a">> => V }`; `from(#{ <<"a">> => V })` yields `#{ <<"a">> => V }`.
4. **One level of nesting.** `from(#{ <<"a/b">> => V })` yields
   `#{ <<"a">> => #{ <<"b">> => V } }`, and `to` of that nested map yields
   `#{ <<"a/b">> => V }`.
5. **Deep nesting.** `from(#{ <<"a/b/c/d">> => V })` yields the four-level nested
   map `#{a => #{b => #{c => #{d => V}}}}`; `to` reverses it to
   `#{ <<"a/b/c/d">> => V }`.
6. **Multiple paths share a prefix.** `from(#{ <<"x/y">> => <<"1">>,
   <<"x/z">> => <<"2">>, <<"a">> => <<"3">> })` yields
   `#{ <<"x">> => #{ <<"y">> => <<"1">>, <<"z">> => <<"2">> }, <<"a">> => <<"3">> }`;
   `to` reverses it.
7. **Delimiter is `/`; segments join with `/`.** Every flat key emitted by `to`
   uses the single byte `/` between segments and has no leading/trailing/double
   `/`.
8. **List-form keys flatten.** A nested map containing a key that is the list
   `[<<"y">>, <<"z">>]` flattens such that the resulting flat keys contain that
   path joined with `/` (the list segments are concatenated, not stringified).
9. **Lists become numbered messages.** `to` of a list operand produces the same
   flat map as `to` of the numbered message `#{ <<"1">> => E1, <<"2">> => E2, … }`
   built from the list in order.
10. **Recursion through values.** `from`/`to` recurse into nested-map values
    (not only top-level keys): nesting expressed in a value map is expanded /
    collapsed identically to top-level nesting.
11. **Delimiter collapsing.** A flat key with leading, trailing, or doubled
    delimiters (`<<"/a//b/">>`) on `from` parses to the same segments as
    `<<"a/b">>`; `to` never emits such forms.
12. **No segment escaping / round-trip caveat.** There is no escape for a literal
    `/` inside a segment; `from`/`to` round-trip exactly for messages whose
    segment names contain no `/`, and an implementation MUST NOT add escaping.
13. **No newline in keys.** No flat key produced by `to` contains `\n`.
14. **Text form ordering.** The text serialisation emits `key: value\n` lines in
    ascending byte-wise sorted key order; the text deserialiser splits each line
    on the first `: ` and decodes the resulting flat map through `structured@1.0`.
15. **`commit`/`verify`** behave exactly as `httpsig@1.0`'s `commit`/`verify`.

## 11. Out of scope

- The **internal in-memory representation** of flat and nested messages (maps,
  links, iteration order) — only the observable `(path, value)` content and the
  byte-exact key format are constrained.
- **Rich type encoding/decoding** of values: typing of integers, floats, atoms,
  and lists is `structured@1.0`'s responsibility. `flat@1.0` carries binaries and
  nested binaries only.
- The cryptography of `httpsig@1.0` (reached only via `commit`/`verify`).
- Behaviour on **unsupported operand types** and on **ill-formed inputs** —
  segment names containing `/`, colliding non-map leaves at one path, and
  **degenerate keys** that reduce to zero segments (the empty binary `<<>>`, the
  empty list, an all-delimiter key like `<<"/">>`) — explicitly unspecified
  (an implementation MAY raise).
- Performance, storage strategy, and any specific module structure.

## Open questions

- **Empty-list value handling.** A bare empty-list value reaching `from` is
  treated as an edge case: the reference path emits a diagnostic and then deep-
  sets it unchanged (it does not drop the entry or fail). Whether such a value
  should instead be dropped, or rejected, is unsettled — but well-formed TABM
  never carries a bare empty list (it is an `ao-types` marker in
  `structured@1.0`), so this should not arise in content-addressed use.
- **Collision determinism.** When two flat keys map to the same non-map leaf
  path, the surviving value is governed by unordered-map iteration and is left
  unspecified here. If a deterministic tie-break is ever required (e.g. for a
  transport that can legitimately present duplicate paths), it would need to be
  pinned (e.g. "lexicographically greatest key wins").
- **`/`-in-segment policy.** The format cannot represent a segment containing a
  literal `/`. If such keys must be carried losslessly through the flat form, an
  escaping scheme would have to be introduced; this spec deliberately forbids one
  to keep `to`/`from` delimiter-collapsing and stable.
