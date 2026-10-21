# `b32-name@1.0` — base32 self-certifying name resolver

- **Device name:** `b32-name@1.0`
- **Depends-on:** `name@1.0` (the resolver-dispatch device that invokes this device), `message@1.0` (base identity device for non-handled keys). Both specs are provided to reimplementers.
- **Status:** Draft

## 1. Overview

`b32-name@1.0` is a **name resolver**: a device that maps a textual *name* to a
content-addressed *identifier*. It exists to let a 32-byte AO-Core/Arweave ID be
written as a **base32** string short enough and DNS-safe enough to serve as a
hostname/subdomain label, and to recover the original ID from that string.

A name resolved by this device **self-certifies**: the name *is* the base32
encoding of the very ID it resolves to. No lookup table, registry, signature, or
external state is consulted — the mapping is a pure, reversible re-encoding
between two representations of the same 32 bytes. Anyone holding the name can
verify (by re-encoding the ID) that the ID is the unique pre-image of that name.

It is designed to be plugged into `name@1.0` as one of the node's ordered
resolvers (see §9). `name@1.0` (or any caller) asks this device to resolve a
single name, given as the **key** being read; this device answers with the
decoded ID, or reports not-found so the next resolver may try.

## 2. Concepts & terminology

- **Name (the key):** the textual label this device attempts to resolve. It is
  supplied as the **key** being resolved on the device — i.e. the path segment /
  key name, a binary — not as a field inside the request or base message. For
  example, resolving the key `4nuojs5tw6xtfjbq47dqk6ak7n6tqyr3uxgemkq5z5vmunhxphya`
  against this device asks it to resolve that name.
- **ID (the value):** a 32-byte native identifier, returned in its
  **human-readable** form: the **base64url**, unpadded, 43-character encoding of
  those 32 bytes (the standard AO-Core identifier form, never hex). This is the
  device's output value.
- **base32:** the encoding defined by RFC 4648 §6 (the standard base32 alphabet,
  see §5). This device uses base32 to render a 32-byte ID as DNS-label-safe text.
- **Self-certifying:** the property that the name carries, in itself, sufficient
  information to reconstruct and verify the ID. Because the name is a lossless
  re-encoding of the ID's bytes, `name → ID` and `ID → name` are exact inverses;
  there is no binding to verify against an authority.
- **Resolver:** the contract this device fulfils — given a key, return
  `{ok, Value}` if it can resolve the key, or a not-found result otherwise — as
  consumed by `name@1.0`. See the `name@1.0` spec for the dispatch semantics; the
  relevant subset is restated in §3 and §9.

## 3. Device interface

- **Dispatch shape:** **default-handler.** The device installs a single
  catch-all handler that receives the **key** being resolved and attempts to
  decode it. There is no fixed list of answerable keys: *any* key reaching the
  handler is interpreted as a candidate base32 name. The handler is the device's
  entire resolution surface.
- **Excluded keys:** the device excludes **exactly** the two message-manipulation
  keys `keys` and `set` from the default handler; they fall through to the base
  identity device (`message@1.0`). This prevents binding the device onto a path or
  mutating a message from being swallowed and misinterpreted as a name. The
  exclude set is **exactly** `[keys, set]` — do **not** add `set-path`/`remove`
  (the reference and the sibling `name@1.0` exclude only these two), and do **not**
  exclude ordinary data-like keys, because every other key is exactly what the
  handler is meant to receive.
- **Message shape:** the device reads **only the key**. It does not read the base
  message, the request message, or node options. The base and request messages
  MAY be anything (including empty); their contents do not affect the result.
- **Inputs that matter:** exactly one — the key being resolved. Everything else
  is ignored.

## 4. Resolved keys (normative)

### `<name>` (the default handler) — resolve a base32 name to its ID

- **Reads:** the **key** being resolved (a binary). Does **not** read the base
  message, the request message, or node options.
- **Behaviour:**
  1. **Length gate.** If the key is **not exactly 52 bytes** long, the device
     MUST NOT treat it as a name: it MUST return the not-found result (§8). No
     decoding is attempted. (A 32-byte ID encodes to exactly 52 base32 characters
     after padding is removed — see §5 — so any key of another length cannot be a
     base32-encoded 32-byte ID.)
  2. **Decode.** Otherwise, decode the 52-character key as base32 (alphabet and
     case rules per §5) into its raw bytes, then take the human-readable
     (base64url, 43-char) form of the resulting 32 bytes.
  3. **Failure is not-found.** If decoding fails for any reason — a character
     outside the base32 alphabet, or any other error in the decode/re-encode
     step — the device MUST return the not-found result (§8). It MUST NOT raise,
     and MUST NOT return a partial or malformed value.
  4. **Success.** On a successful decode, return the resulting ID as the value.
- **Returns:** `{ok, ID}` where `ID` is the 43-character base64url
  human-readable identifier, or the not-found result `{error, not_found}` (§8).
- **Side effects:** **none.** No cache read, no cache write, no store write, no
  commitment, no outbound network request, no node-options mutation. The device
  is a pure function of the key.

## 5. Data formats & encodings

### 5.1 base32 (input name)

- **Alphabet:** the standard RFC 4648 §6 base32 alphabet — the 32 symbols
  `ABCDEFGHIJKLMNOPQRSTUVWXYZ234567` mapping to values `0..31` in that order
  (`A`=0 … `Z`=25, `2`=26 … `7`=31). This is **not** the extended-hex
  (`base32hex`) alphabet.
- **Case (decode):** decoding is **case-insensitive.** Both uppercase `A–Z` and
  lowercase `a–z` decode to the same values; `2–7` are the digits. A name in any
  letter-case (or mixed case) that is otherwise a valid 52-character base32 string
  for a 32-byte value MUST decode to the same ID. (Hostname labels are commonly
  lower-cased in transit; the device tolerates either case on input.)
- **Padding:** the input name is **unpadded** — it carries no `=` characters and
  is exactly 52 characters. (Standard base32 of 32 bytes is 56 characters
  including four trailing `=` padding characters; this device's names are the
  56-character form with the padding removed, i.e. 52 significant characters. An
  implementation MUST accept the 52-character unpadded form. Whether it also
  accepts a 56-character padded form is unspecified, because the length gate in
  §4 rejects any key that is not exactly 52 bytes — so a padded name is treated as
  not-found regardless.)
- **Length:** exactly **52** characters. A 32-byte value is 256 bits; base32
  packs 5 bits per character, requiring ⌈256/5⌉ = 52 characters (the 52nd
  character carries the final bit with the low bits zero). The length gate is the
  device's fast rejection of anything that is not a 32-byte base32 name.

### 5.2 ID (output value)

- The output is the **human-readable** form of the decoded 32 bytes: **base64url
  (URL- and filename-safe alphabet, RFC 4648 §5), unpadded, 43 characters**.
  Never hex.
- The output is **lower/upper-faithful to base64url**, i.e. it is whatever
  base64url string represents those 32 bytes; it is not case-folded.

### 5.3 The self-certifying mapping (normative, reversible)

The name and the ID are two encodings of the **same 32 bytes**. The inverse
direction (ID → name), although this device does not expose it as a resolvable
key, defines the canonical name for an ID and MUST be the exact inverse of §4's
decode so that the round-trip is lossless:

1. Take the 32 raw bytes of the ID (decode the 43-char base64url to 32 bytes).
2. base32-encode those 32 bytes using the alphabet in §5.1, producing a
   56-character string whose last four characters are `=` padding.
3. **Lower-case** the result.
4. **Remove all `=` padding** characters.

The result is the 52-character lower-case name. Decoding that name with §4
recovers the identical 32 bytes and therefore the identical ID. Worked vector
(normative): the ID
`42jky7O3rzKkMOfHBXgK-304YjulzEYqHc9qyjT3efA`
maps to the name
`4nuojs5tw6xtfjbq47dqk6ak7n6tqyr3uxgemkq5z5vmunhxphya`
and the name decodes back to that ID. (The unstripped, upper-case base32 of the
same bytes is `4NUOJS5TW6XTFJBQ47DQK6AK7N6TQYR3UXGEMKQ5Z5VMUNHXPHYA====`.)

- There are **no IDs, commitments, or hashpaths produced** by this device. The
  only content-addressed artefact involved is the ID the name already encodes;
  the device neither signs nor re-derives it, it merely re-encodes bytes.

## 6. Ordering, freshness & caching

- The device is a **pure, deterministic function** of the key: the same name
  always yields the same ID (or the same not-found result), with no dependence on
  time, node state, prior calls, or call order.
- The device performs **no caching of its own**. Its output for a constant key is
  constant, so no freshness concern arises from this device. (Whether the
  *consumer* — e.g. `name@1.0` — then loads the message at the returned ID, and
  any caching of that load, is outside this device.)

## 7. Security & authority

- **No authority is consulted and none is required.** Resolution is unauthenticated
  and available to any caller; the base and request messages need not be
  committed or signed, and are not read at all.
- **Self-certification is the security model.** Because the name is exactly the
  base32 encoding of the ID's bytes, a returned ID is the unique pre-image of the
  name: a caller can verify the binding offline by re-encoding the ID (§5.3) and
  checking it equals the name. There is no spoofable indirection — the device
  cannot return an ID that is not the decoding of the supplied name without
  violating this spec.
- **Failure is closed and silent.** Any input that is not a well-formed
  52-character base32 name (wrong length, out-of-alphabet character, decode
  error) yields the not-found result, never an exception and never a guessed or
  defaulted ID. This lets the device sit in a resolver chain (§9) and cleanly
  decline names it does not recognise, so the next resolver may handle them.
- The device makes **no outbound requests** and mutates **no state**, so it
  introduces no network or persistence attack surface of its own.

## 8. Errors

- `not_found` — the device's sole failure result, returned (as `{error, not_found}`)
  whenever the key cannot be resolved to an ID, in exactly two cases:
  1. the key is not exactly 52 bytes long (length gate, §4 step 1); or
  2. the 52-byte key fails to decode as base32 to a 32-byte value (out-of-alphabet
     character or any other decode error, §4 step 3).
  These two cases are **indistinguishable** in the result; both produce the same
  `not_found`. The device defines no other error and never raises.
- There is **no** error for "decoded but to the wrong number of bytes" beyond the
  generic `not_found`: a 52-character base32 string always decodes to 32 bytes
  under this alphabet, so the only ways to fail after the length gate are an
  illegal character or a decode-layer error, both folded into `not_found`.
- Decoding is **lenient on the trailing bits**: a 52-character name carries 260
  bits but a 32-byte ID occupies only the high 256, so the final 4 bits are
  **ignored** on decode. A *non-canonical* name (whose trailing 4 bits are
  non-zero — e.g. the last character is `b` rather than `a`) is therefore **not**
  rejected; it decodes to the **same** ID as its canonical form. There is no
  "non-canonical encoding" failure case. (The canonical `ID → name` direction of
  §5.3 always zeroes those bits, so the round-trip stays exact; only hand-crafted
  off-canonical inputs are many-to-one.)

## 9. Composition

- **As a `name@1.0` resolver.** This device is intended to be listed among a
  node's ordered name resolvers (the `name-resolvers` node option), as a resolver
  entry naming this device — e.g. an entry `#{ "device" => "b32-name@1.0" }`.
  `name@1.0`, when asked to resolve a name (a key), tries each configured resolver
  in order and returns the value of the **first** that yields `{ok, _}`; a
  resolver that returns not-found (as this device does for non-base32 keys) is
  skipped and the next is tried. Because this device declines (not-found) anything
  that is not a 52-character base32 name, it composes safely with other resolvers
  (e.g. an ArNS/name-registry resolver) ordered before or after it: it claims
  *only* base32-shaped names and passes everything else through. The relative
  order matters only if another resolver could also match a 52-character base32
  string; otherwise placement is free.
- **Host/subdomain resolution.** The canonical use is resolving a request's host
  subdomain label to an ID: a request to host `<name>.<node-host>` has its leading
  label (`<name>`) resolved by `name@1.0` through this device to an ID, which is
  then prepended as the base of the execution path. This is how a 32-byte ID is
  reachable as a DNS subdomain. The host-parsing, subdomain-extraction, and
  path-prepending behaviour belongs to `name@1.0` (its request hook), not to this
  device; this device only performs the `name → ID` step. (See the `name@1.0`
  spec.)
- **Returned value is an ID, optionally loaded.** This device returns the ID
  itself. Whether the consumer then treats that ID as a pointer and **loads** the
  message it addresses (vs. using the ID verbatim) is the consumer's choice — for
  `name@1.0`, governed by its `load` request flag — and is outside this device's
  contract.
- **Base identity for other keys.** Because the device excludes `keys`/`set` (and
  installs no behaviour for the commitment surface `id`/`commit`/`verify`),
  binding `~b32-name@1.0` onto a message and resolving any of those keys resolves
  under `message@1.0` semantics for that message, not under this device.

## 10. Conformance (normative checklist)

An implementation MUST exhibit all of the following, each checkable by resolving
a key against the device (directly, or via `name@1.0` with this device as the
sole configured resolver):

1. The device is a **default-handler** device: resolving an arbitrary key (not in
   the excluded set) routes to the single name-decoding behaviour; there is no
   fixed allow-list of names.
2. Resolving a key that is **exactly 52 characters** and is a valid base32
   encoding (standard RFC 4648 alphabet `A–Z2–7`) of 32 bytes returns
   `{ok, ID}`, where `ID` is the **43-character base64url** human-readable form of
   those 32 bytes.
3. The specific vector holds:
   `4nuojs5tw6xtfjbq47dqk6ak7n6tqyr3uxgemkq5z5vmunhxphya` resolves to
   `42jky7O3rzKkMOfHBXgK-304YjulzEYqHc9qyjT3efA`.
4. Decoding is **case-insensitive**: the upper-case form
   `4NUOJS5TW6XTFJBQ47DQK6AK7N6TQYR3UXGEMKQ5Z5VMUNHXPHYA` (52 chars, no padding)
   resolves to the **same** ID as the lower-case form.
5. A key whose length is **anything other than 52 bytes** (e.g. a 43-char
   base64url ID, an empty key, a 51- or 53-char string, a padded 56-char base32
   string) returns the not-found result and triggers **no** decode attempt.
6. A 52-character key containing a character **outside** the base32 alphabet
   (e.g. `0`, `1`, `8`, `9`, or a non-alphanumeric) returns the not-found result
   and **never raises**.
7. The result on any failure is **`{error, not_found}`**; the device returns no
   other error atom and never throws.
8. The mapping is **lossless and reversible**: for every 32-byte ID, base32-encoding
   it (RFC 4648 alphabet), lower-casing, and stripping `=` padding yields a
   52-character name that this device decodes back to the identical ID
   (round-trip identity).
9. The device produces **no side effects**: resolving any key performs no cache
   write, store write, commitment, node-options mutation, or outbound network
   request, and reads neither the base message, the request message, nor node
   options.
10. As a resolver under `name@1.0`: a node configured with this device as a
    `name-resolvers` entry resolves a 52-char base32 name to its ID and, for a
    non-base32 name, declines (not-found) so resolution falls through to the next
    resolver (or to a 404 when no resolver matches and no path remains).

## 11. Out of scope

- The **internal representation** of messages, keys, and IDs; any specific data
  structure or module layout.
- The **inverse (ID → name) direction as a callable key.** §5.3 defines the
  canonical name for an ID (so the round-trip is pinned and verifiable), but this
  device exposes only the `name → ID` decode as a resolvable key. How a name is
  *minted* from an ID (e.g. for constructing a subdomain URL) is a separate
  concern.
- **Host parsing, subdomain extraction, and path prepending** — i.e. turning a
  request's `Host` header into a name and splicing the resolved ID into the
  execution path. That behaviour belongs to `name@1.0` (its request hook), not to
  this device; this device performs only the single `name → ID` step.
- **Resolver ordering policy** and the contents of the `name-resolvers` list
  beyond this device's own entry — which other resolvers a node configures, and
  in what order, is node configuration (constrained only by §9's note that this
  device claims solely 52-char base32 names).
- **Whether and how the returned ID is subsequently loaded** from the cache/store
  as a message (the consumer's `load` decision).
- **Result-cache configuration** of the hosting node (not needed here, since the
  device's output for a constant key is constant).
- Performance, concurrency, and the precise byte-level mechanics of the base32
  codec beyond the alphabet, case, padding, and length rules pinned in §5.

## Open questions

- **Padded / non-52-length valid base32.** The length gate accepts *only* exactly
  52 bytes, so a 56-character padded base32 name, or any base32 string of another
  length, is reported not-found even if it is a syntactically valid base32 of 32
  bytes. This is deliberate (52 is the canonical unpadded length for a 32-byte
  ID), but it means the device is stricter than "any base32 encoding of a 32-byte
  ID." Flagged in case a reimplementer expects padded input to be accepted — per
  this spec it MUST NOT be (it is not-found).
- **Non-32-byte IDs.** AO-Core also recognises 42-character identifiers (e.g.
  Ethereum-style addresses) and a 43-char base64url form. This device's length
  gate (52) and decode target only ever produce a 32-byte → 43-char ID; it does
  not resolve names for 42-byte/non-32-byte identifiers. Whether such IDs should
  ever be expressible as base32 names is left open; this device does not handle
  them.
- **Collision with other 52-char-name resolvers.** If a node configures another
  resolver that can also match a 52-character base32 string, the first-match
  ordering of `name@1.0` decides the winner. This spec does not constrain that
  ordering; it only guarantees this device matches exactly the set of 52-char
  decodable base32 names and declines everything else.
