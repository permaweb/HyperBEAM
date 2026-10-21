# `gzip@1.0` — gzip body compression codec

- **Device name:** `gzip@1.0`
- **Depends-on:** `none`
- **Status:** Draft

## 1. Overview

`gzip@1.0` is a **codec device** that compresses and decompresses the `body`
field of a message using the gzip format (RFC 1952, DEFLATE per RFC 1951). It is
a transport/representation transform, not a typed-message converter: it touches
exactly one key (`body`) and a marker key (`content-encoding`), leaving every
other key of the message untouched. `zip` shrinks a body and records that it did
so; `unzip` reverses that, restoring the original bytes and clearing the marker.

It is the codec used to put a message body on the wire in compressed form and to
recover it on the other side. Unlike `structured@1.0`, this device does **not**
produce a canonical, byte-stable normal form: the compressed output is not
guaranteed identical across implementations (see §5 and §6), so its only
cross-implementation guarantee is **round-trip recovery of the decompressed
body**, not byte-equality of the compressed body.

## 2. Concepts & terminology

- **Body:** the value of the message's `body` key. A binary (octet string) of
  arbitrary length, including empty. This is the only payload the device
  compresses or decompresses.
- **gzip member:** a self-delimited gzip stream as defined by RFC 1952 — a
  fixed 10-byte header (magic `0x1f 0x8b`, compression method `0x08` = DEFLATE,
  flags, 4-byte modification time, extra-flags, OS byte), the DEFLATE-compressed
  data, and an 8-byte trailer (CRC-32 then ISIZE, the uncompressed length mod
  2^32). The device's compressed bodies are single gzip members.
- **`content-encoding` marker:** a message key whose value (a binary) names the
  encoding currently applied to `body`. The value `gzip` means "the body is a
  gzip stream". This device sets it on `zip` and removes it on `unzip`. The key
  name and the token `gzip` are both lowercase binaries on the wire.
- **Round trip:** applying `zip` to a message and then `unzip` to the result
  (or vice-versa where applicable), and comparing the recovered `body` to the
  original.

The byte-level internal form of compressed data beyond what RFC 1952/1951
mandate is out of scope (see §11).

## 3. Device interface

- **Dispatch shape:** **explicit-keys.** The device answers exactly two keys —
  `zip` and `unzip` — by name. It has no default/catch-all handler: any other
  key resolved against a message bound to this device falls through to the base
  identity device (`message@1.0`) and is not intercepted here. Because the
  device exposes only these two operation keys (it does not bind itself onto the
  message as a persistent device), the message-manipulation keys (`keys`, `set`,
  `set-path`, `remove`, `id`, `commit`, `verify`, …) are unaffected by this
  device and behave per `message@1.0`.
- **Message shape (input to both keys):** an arbitrary message map. Relevant
  keys:
  - `body` — OPTIONAL, binary. The payload to (de)compress.
  - `content-encoding` — OPTIONAL, binary. The current encoding marker. Read by
    `unzip`; written/removed as described in §4.
  - All other keys — arbitrary; passed through verbatim by both operations.
- Both operations are invoked as a single resolution step that takes the message
  as the base and returns a new message; neither reads any field from the
  request message, and neither reads any node option that affects output (see
  §4). The operations are pure functions of the base message.

## 4. Resolved keys (normative)

### `zip` — compress the body in place

- **Reads:** `body` from `Base`. Reads **nothing** from the request or node
  options that affects the result.
- **Behaviour:**
  1. If `Base` has **no** `body` key, return error `no-body-to-zip` (§8). MUST
     NOT invent an empty body.
  2. If `Base` has a `body`, compress it into a single gzip member (RFC 1952)
     and produce a result message equal to `Base` with two changes:
     - `body` set to the gzip-compressed bytes.
     - `content-encoding` set to the binary `gzip` (added if absent, overwritten
       if present).
  3. All other keys of `Base` are preserved unchanged. The device MUST NOT add,
     remove, or alter any key other than `body` and `content-encoding`.
- **Returns:** `{ok, Message}` where `body` is the compressed bytes and
  `content-encoding` is `gzip`; or `{error, <<"no-body-to-zip">>}`.
- **Side effects:** none. No cache or store writes, no commitments, no external
  calls. (Compressing a `body` that is covered by an existing commitment will
  change its value; the consequences of that for the message's commitments are
  governed by `message@1.0`, not by this device, which does not itself touch the
  `commitments` key.)

### `unzip` — decompress the body in place

- **Reads:** `content-encoding` and `body` from `Base`. Reads **nothing** from
  the request or node options that affects the result.
- **Behaviour:**
  1. Determine the marker: read `content-encoding` from `Base`, **defaulting to
     the binary `gzip` when the key is absent**. (Consequence: a message with a
     `body` but no `content-encoding` is treated as gzip-encoded and WILL be
     decompressed.)
  2. **If the marker is not exactly the binary `gzip`:** return `Base`
     unchanged — no decompression, and the `content-encoding` key is left in
     place. This is the pass-through for bodies in other encodings.
  3. **If the marker is `gzip`:**
     - If `Base` has **no** `body` key, return `Base` unchanged (a no-op; the
       `content-encoding` key, if present, is **left in place**).
     - If `Base` has a `body`, decompress it as a gzip stream and produce a
       result message equal to `Base` with two changes:
       - `body` set to the decompressed bytes.
       - the `content-encoding` key **removed**.
  4. All other keys of `Base` are preserved unchanged.
- **Returns:** `{ok, Message}`. On the decompress path, `body` holds the
  decompressed bytes and `content-encoding` is absent. On either pass-through
  path, the original message is returned verbatim.
- **Decompression input handling:** decompression MUST accept any valid gzip
  member produced per §5. If the `body` is not a valid gzip stream, decompression
  fails (see §8, `decompress-failure`); the operation is **failure-closed** —
  it MUST NOT return the malformed body unchanged on the `gzip` path.
- **Side effects:** none (as for `zip`).

## 5. Data formats & encodings

- **Body on the wire is raw octets, not base64url.** The `body` value is a
  binary; the compressed body is the raw gzip member bytes (which begin with the
  magic bytes `0x1f 0x8b`). This device never base64-encodes or hex-encodes the
  body, and never wraps it in any envelope. (Identifiers elsewhere in AO-Core are
  base64url; this device produces none.)
- **Compression format:** gzip, RFC 1952, with the DEFLATE method (RFC 1951,
  header compression-method byte `0x08`). The output is a **single gzip member**.
- **Header fields an implementation SHOULD pin for determinism:**
  - **Modification time (MTIME, header bytes 4–7) MUST be zero.** The device
    MUST NOT embed a wall-clock timestamp; doing so would make the output
    nondeterministic and timestamp-leaking. (The reference implementation emits
    MTIME = 0.)
  - The magic/method bytes are fixed by the format (`0x1f 0x8b 0x08`).
- **Header fields that are NOT guaranteed cross-implementation:** the **OS
  byte** (header byte 9) and the exact **DEFLATE encoding** of the payload (which
  depends on the compressor's strategy and effort level). Two conformant
  implementations MAY therefore produce **different compressed bytes** for the
  same input body while both decompressing to the identical original. The
  reference implementation emits the OS byte its platform's zlib chooses and uses
  that zlib's default compression effort.
- **Decompression** follows RFC 1952: read the header, inflate the DEFLATE data,
  verify the trailer. A decompressor MUST handle the empty-input case (an empty
  body compresses to a valid ~20-byte gzip member that decompresses back to the
  empty binary). A decompressor that encounters multiple concatenated gzip
  members MAY decode and concatenate them (RFC 1952 multi-member streams);
  bodies produced by this device's `zip` are always single-member, so this only
  affects externally-produced inputs.
- **No canonicalisation guarantee.** Because compressed bytes are not pinned
  cross-implementation, the compressed `body` MUST NOT be relied upon as a
  content-addressed, byte-stable value. Only the **decompressed** body is a
  stable, well-defined value. Any content-addressing (IDs/commitments) over a
  compressed message therefore addresses the particular compressed bytes that
  one implementation produced, which another implementation need not reproduce.

## 6. Ordering, freshness & caching

- **Determinism within an implementation:** for a fixed compressor (same
  library and effort level), `zip` is a deterministic pure function of the input
  body — repeated calls on the same body yield identical bytes. The mandated
  zero MTIME is what removes the only otherwise-nondeterministic header field.
- **Determinism across implementations:** NOT guaranteed for the compressed
  bytes (see §5). The guaranteed invariant is the **round trip**: for any body
  `B`, `unzip(zip(M_with_body_B))` yields a message whose `body` equals `B`
  exactly and whose `content-encoding` is absent, regardless of which conformant
  compressor produced the intermediate. This holds for the empty body.
- **Idempotence / repetition:** `zip` is **not** idempotent — applying it twice
  double-compresses the body (the second `zip` compresses the first member's
  bytes) and leaves `content-encoding = gzip`; the body must then be `unzip`'d
  twice to recover the original. (Each `zip`/`unzip` is one layer.)
- The device performs **no result caching of its own** and reads no
  freshness/cache-control directives. Any caching of resolution results is the
  surrounding substrate's concern, not this device's.

## 7. Security & authority

- The device is **unprivileged**: it transforms the `body` representation only.
  It does not sign, verify, commit, or grant any authority, and it does not read
  or write the `commitments` key.
- Any caller may invoke `zip`/`unzip`; there is no committer or signature
  requirement.
- **Failure-closed on decode:** on the `gzip` path, a `body` that is not a valid
  gzip stream MUST cause `unzip` to fail rather than silently returning the
  malformed bytes (§8). `zip` is failure-closed on a missing body (it errors
  rather than fabricating one).
- The device imposes no size limit itself; a decompressed body can be much
  larger than its compressed form. Decompression-bomb mitigation (input/output
  size caps) is a node/substrate policy concern, out of scope for this device's
  contract.

## 8. Errors

- `no-body-to-zip` — returned by `zip` when the base message has no `body` key.
  (Reference implementation returns this as the result `{error, Reason}`; the
  Reason is a human-readable binary conveying "no `body` key to zip". Implementers
  MUST signal an error in this case; the exact Reason string is not part of the
  observable contract beyond that it indicates the missing-body condition — see
  Open questions.)
- `decompress-failure` — `unzip`, on the `gzip` path with a present but
  malformed/non-gzip `body`, fails to decompress. This surfaces as a resolution
  failure (the reference implementation raises rather than returning a tidy
  `{error, _}` tuple). Implementers MUST NOT swallow this and return the
  malformed body; an unrecoverable input MUST fail the operation.
- `unzip` has **no** missing-body error and **no** wrong-encoding error: a
  missing `body` (gzip path) and a non-`gzip` `content-encoding` are both
  defined **pass-throughs**, not errors (§4).

## 9. Composition

- **As a path-applied operation.** The operations are typically applied as a
  step in a resolution path against a stored message, e.g.
  `<id>/zip~gzip@1.0` to compress a cached message's body, and
  `<id>/unzip~gzip@1.0/body` to decompress and then read the recovered body.
  Each is a single transform step whose `{ok, Message}` result can be the base
  of the next step (read a key, write to cache, hand to another codec).
- **With other codecs.** Because `zip` operates on whatever is in `body`,
  pipelines place it **after** any codec that serialises a message into a `body`
  blob and **before** transport; `unzip` is applied **first** on receipt, before
  the body is handed to the decoder that interprets it. This device neither
  produces nor consumes TABM directly — it composes by transforming the `body`
  bytes that other codecs serialise to / deserialise from.
- **Marker handshake.** `zip` advertises the applied encoding by setting
  `content-encoding = gzip`; a generic `unzip` consumer keys off that marker
  (and the gzip default) to decide whether to decompress, then clears it so a
  downstream consumer sees a plain, unmarked body. A non-`gzip` marker is the
  contract for "leave this body for a different decoder".
- The device does **not** chain via a returned `device` key (it returns plain
  messages bound to no particular device), so there is no device-switching or
  self-recursion behaviour to manage.

## 10. Conformance (normative checklist)

An implementation MUST exhibit all of the following, each checkable by resolving
`zip`/`unzip` over constructed messages and inspecting the resulting `body`,
`content-encoding`, and other keys:

1. `zip` of a message with `body = B` returns a message whose `content-encoding`
   is the binary `gzip` and whose `body` is a single valid gzip member (begins
   with bytes `0x1f 0x8b 0x08`) that decompresses to exactly `B`.
2. `zip` preserves every other key of the input unchanged and adds/changes only
   `body` and `content-encoding`.
3. `zip` of a message with **no** `body` key signals an error
   (`no-body-to-zip` condition) and does not produce a body.
4. The gzip output embeds a **zero modification-time** field (no wall-clock
   timestamp), so repeated `zip` of the same body under the same implementation
   yields byte-identical output.
5. `unzip` of a message with `content-encoding = gzip` and a gzip `body`
   returns a message whose `body` is the decompressed bytes and from which the
   `content-encoding` key has been **removed**; all other keys are unchanged.
6. `unzip` of a message that has a `body` but **no** `content-encoding` key
   treats it as `gzip` (the default) and decompresses it as in (5).
7. `unzip` of a message whose `content-encoding` is any binary **other than**
   `gzip` returns the message **unchanged**, including leaving its
   `content-encoding` key in place and its `body` not decompressed.
8. `unzip` of a message with `content-encoding = gzip` (or absent) but **no**
   `body` key returns the message unchanged (a no-op; not an error), leaving any
   `content-encoding` key in place.
9. **Round trip:** for any body `B` (including the empty binary),
   `unzip(zip(M))` yields `M`'s body recovered exactly as `B` with
   `content-encoding` absent. This holds even if the compressed bytes were
   produced by a different conformant compressor.
10. `unzip` on the `gzip` path with a present but non-gzip / corrupt `body`
    **fails** (failure-closed); it MUST NOT return the malformed body as though
    decompressed.
11. Neither operation writes to any cache or store, makes any external call, or
    reads any field of the request message or node options that changes its
    output: each is a pure function of the base message.

## 11. Out of scope

- The internal in-memory representation of messages and bodies.
- The exact DEFLATE encoding of the payload and the gzip **OS** header byte —
  these MAY differ between conformant implementations (only round-trip recovery
  and the pinned header fields of §5 are constrained).
- The specific compression effort/level, beyond that it MUST be a valid gzip
  member and MUST decompress to the original. (The reference implementation uses
  its zlib's default level; this is not observable beyond output size.)
- The cryptography/identity of any commitment or ID device; how compressing a
  committed body interacts with `commitments` is governed by `message@1.0`.
- Node-level result caching, cache-control, and decompression-bomb size limits
  (substrate policy).
- Performance and storage strategy.

## Open questions

- **Error signalling shape is implementation-flavoured.** The reference
  implementation returns the missing-body case as `{error, <Reason-binary>}`
  with a prose Reason (`"No \`body' key to zip found in message."`), and lets the
  decompress failure **raise** (an exception) rather than return `{error, _}`.
  This spec pins the *conditions* and that each MUST fail, and names them
  `no-body-to-zip` / `decompress-failure` for reference, but the exact Reason
  string and whether a failure is a returned tuple vs a raised error are not
  nailed down by the reference. If a stable, machine-readable error atom is
  required across implementations, it should be specified (e.g. mandate
  `{error, <<"no-body-to-zip">>}` and a caught `{error, <<"decompress-failure">>}`).
- **Cross-implementation byte-stability of compressed bodies is intentionally
  unspecified.** If a use case needs content-addressed, reproducible compressed
  bodies (identical bytes across implementations), this device as specified does
  **not** provide it (the OS byte and DEFLATE output vary). That would require
  additionally pinning the OS byte (e.g. to `0xff` "unknown") and a single exact
  DEFLATE encoder — neither is currently mandated.
- **`unzip` asymmetry in clearing the marker.** On the no-body gzip path and on
  the non-`gzip` path, `unzip` leaves `content-encoding` in place; only the
  actual-decompress path removes it. This is the observed behaviour; whether the
  no-body gzip path *should* also clear the marker is a latent question (it
  currently does not).
