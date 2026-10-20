# `tx@1.0` — Arweave L1 transaction codec & commitment device

- **Device name:** `tx@1.0`
- **Depends-on:** `structured@1.0` (TABM ⇄ rich conversion), `message@1.0` (the surrounding message / commitment model). Both specs are provided to reimplementers.
- **Status:** Draft

## 1. Overview

`tx@1.0` is the codec that maps an AO-Core message to and from the external
**Arweave L1 transaction** format (the format 1 / format 2 transaction header
that Arweave nodes gossip and gateways serve, a.k.a. the `ar_tx` / `last_tx`
shape). It is simultaneously a **commitment device**: an Arweave transaction's
identity is content-addressed by hashing its signature preimage, so converting a
message *to* a transaction and back lets the device produce both **signed**
(RSA-PSS / ECDSA) and **unsigned** (content-hash) commitments that interoperate
byte-for-byte with the public Arweave standard.

The device sits beside `structured@1.0` in the codec graph: every conversion is
defined relative to **TABM** (the flat all-binary normal form; see the
`structured@1.0` spec). `tx@1.0` knows how to (a) decode a transaction into a
TABM message plus a commitment (`from`, transaction → TABM) and (b) re-encode a
TABM message into a transaction whose bytes, IDs and signature reproduce the
original (`to`, TABM → transaction). Its `commit` / `verify` keys drive the
Arweave signing and verification machinery over that mapping.

A second, closely related Arweave format — the **ANS-104 data item** (the
bundle/data-item shape) — is handled by a sibling device (`ans104@1.0`) and is
out of scope here, **except** that `tx@1.0` reuses ANS-104 nested-item encoding
for the data field of a *bundled* transaction (§5.7). This spec pins only the L1
transaction behaviour.

## 2. Concepts & terminology

- **L1 transaction (the wire object):** an Arweave transaction header with the
  field set in §5.1. It has two formats: **format 1** (legacy) and **format 2**
  (the current default). `tx@1.0` accepts and produces only formats `1` and `2`.
- **TABM:** the flat, all-binary message normal form over which IDs and
  commitments are computed. IDs and signatures are derived from the transaction
  reconstructed from a TABM, never from the rich message directly.
- **Field:** one of the fixed, named slots of a transaction header (owner,
  target, anchor, quantity, reward, data, data_size, data_root, format,
  signature, …). Fields are distinct from **tags**.
- **Tag:** one entry of the transaction's ordered, possibly-duplicated list of
  `{name, value}` binary pairs. Arweave preserves tag name case and order and
  permits duplicate names; AO-Core message keys are normalised (lower-cased,
  de-duplicated). The codec bridges the two (§5.4, §5.6).
- **Anchor (`last_tx`):** the transaction's anti-replay field. On the wire it is
  the JSON field `last_tx`; in the message and in this spec it is named
  **`anchor`**. It is empty, 32 bytes, or 48 bytes.
- **Signature preimage / signature data segment:** the exact byte structure that
  is hashed and signed to authenticate a transaction (§5.8). Getting one byte
  wrong yields a different ID and an invalid signature.
- **Commitment:** an entry in the message's `commitments` map attesting to a set
  of the message's keys (see `message@1.0`). `tx@1.0` produces commitments whose
  `commitment-device` is `tx@1.0`.
- **Bundle:** a transaction whose `data` field carries nested AO-Core messages
  encoded as an ANS-104 bundle, signalled by the `bundle-format` /
  `bundle-version` / `bundle-map` tags. Controlled by the `bundle` request flag.

## 3. Device interface

- **Dispatch shape:** **codec + commitment device.** The device answers the keys
  `from`, `to`, `commit`, `verify`. It is *not* a default-handler device and
  does not expose arbitrary message keys; message inspection/mutation
  (`keys`/`set`/…) is inherited from `message@1.0`.
- **`from` (Base = a transaction, Req, Opts):** decode a transaction (or a raw
  binary) to its TABM message form. Returns `{ok, TABM-or-binary}`.
- **`to` (Base = a TABM message / binary, Req, Opts):** encode a message to its
  transaction form. Returns `{ok, Transaction}`.
- **`commit` (Base = message, Req, Opts):** add a `tx@1.0` commitment (signed or
  unsigned) to the message. Returns `{ok, CommittedMessage}`.
- **`verify` (Base = message, Req, Opts):** verify a `tx@1.0` commitment on the
  message. Returns `{ok, Boolean}`.

The transaction representation itself (how the header is held in memory) is **out
of scope**; only the observable TABM ⇄ transaction mapping, the resulting bytes,
IDs, and commitments are normative.

### Request fields read by the device

- **`type`** (commit): `unsigned` | `signed` | `rsa-pss-sha256` |
  `unsigned-sha256` (§4.3). Aliases: `unsigned` ≡ `unsigned-sha256`; `signed` ≡
  `rsa-pss-sha256`.
- **`bundle`** (to/commit/verify): boolean (`true`/`false`, default `false`).
  When true the message's nested sub-messages are materialised into the
  transaction's `data` field as an ANS-104 bundle; when false `structured@1.0`
  replaces each with a link, which the transaction carries as a tag whose value
  is an `ao:` URI. The flag may be supplied on the matched commitment (`bundle`
  key) or on the request, in that precedence order; absent from both it is
  `false`.
- **`field-<name>`** (to, signed commit): for a message without a `tx@1.0`
  commitment, gives the base field `<name>` over the message's key `<name>`
  (§5.5).

## 4. Resolved keys (normative)

### 4.1 `from` — transaction → message (TABM)

- **Reads:** the transaction (Base); node options.
- **Behaviour:**
  1. A **binary** Base returns unchanged (`{ok, Binary}`) — a raw binary has no
     transaction structure.
  2. If the transaction's only tag is `{<<"ao-type">>, <<"binary">>}`, the
     transaction is the wrapper for a raw binary; return `{ok, Data}` where
     `Data` is the transaction's `data` field. (This is the inverse of the
     binary wrapping in `to`, §4.2.1.)
  3. Otherwise **validate** the transaction (§5.2); on failure throw the
     corresponding error atom (§8). Then normalise it (recompute IDs, and
     data_size from `data` if present; a data root for format 1, and for
     format 2 only without one; §5.3) and decode any bundled `data` into
     nested sub-messages.
  4. Compute the three component maps — **fields**, **tags**, **data** (§5.4) —
     then the **committed key set** (§5.6) and assemble the **base message** by
     selecting, for each committed key, its value with precedence **data > field
     > tag** (§5.5). A committed key absent from all three is an error
     (`{missing_key, Key}`).
  5. Attach **commitments** (§5.9): if the transaction is signed, a signed
     commitment keyed by its signed ID; else if its tags are not all-normal (case
     differs, duplicate names, a private name, a reserved field name appears as
     a tag, or a list bundle; §5.6), or the message has no `data` key (or an
     empty one) and a non-link `body` key, which `to` would take as its data
     (§5.7), an unsigned commitment keyed by its unsigned ID; else no
     commitment.
- **Returns:** `{ok, Message}` (a TABM map) or `{ok, Binary}`.
- **Side effects:** none beyond reading linked/cached values it is handed. (A
  bundled `data` field is decoded in place; no writes.)

### 4.2 `to` — message → transaction

- **Reads:** the message / binary (Base); `field-<name>` (Req); the message's
  `tx@1.0` commitment if present; node options.
- **Behaviour:**
  1. A **binary** Base is wrapped: produce a normalised format-2 transaction with
     a single tag `{<<"ao-type">>, <<"binary">>}`, `data = Binary`, and the rest
     defaulted. (Arweave cannot ID a bare binary, so this gives it a stable
     transaction identity. `from` strips the wrapper, step 4.1.2.)
  2. A Base that is already a transaction returns unchanged.
  3. A **map** Base is converted: locate the message's `tx@1.0` commitment
     (`commitment-device = tx@1.0`); more than one such commitment is an error
     (`{multiple_ans104_commitments_unsupported, TABM}`). The message is used
     as it is; `structured@1.0` loads it when `bundle` is true.
  4. Build the transaction from three sources, in this order:
     - **signature info** (§5.9.3): owner, signature, signature_type, and the
       original tag list, taken from the commitment if present, else defaults
       (unsigned, owner = all-zero, RSA type).
     - **fields**: reconstruct `format`, `target`, `anchor`, `quantity`,
       `reward`, `data_root`, and (if no inline `data`) `data_size` from the
       `field-`-prefixed commitment keys and/or the message keys; a message
       without a commitment takes the request's `field-<name>` keys over its
       own keys (§5.5, §5.7).
       The data root of a transaction with inline `data` comes from the
       commitment's `field-data-root` alone (§5.9.2).
     - **data**: the `data` field (§5.7).
     - **tags**: the ordered tag list (§5.6), including any preserved
       `original-tags`, bundle tags, and the `ao-data-key` tag if the data lives
       under a non-`data` key.
  5. Normalise the assembled transaction (§5.3) and re-validate it (§5.2); on
     failure throw (§8).
- **Returns:** `{ok, Transaction}`.
- **Side effects:** no writes.

### 4.3 `commit` — produce a `tx@1.0` commitment

- **Reads:** the message (Base); `type` and, for a signed type, `committer` and
  `field-<name>` (Req, §5.5): `message@1.0` gives the node's `priv-wallet` as
  `committer` when the request has none.
- **Behaviour:** by `type`:
  - **`unsigned` / `unsigned-sha256`:** remove any existing `commitments`, then
    round-trip the message through `to` then `from` (transaction encode →
    decode). This re-normalises the message and (re)computes the unsigned content
    ID. `commit` then **ALWAYS attaches an unsigned `tx@1.0` commitment** keyed by
    that unsigned ID — even when the tags are all-normal. (`from`'s rule that an
    unsigned commitment is attached *only* for non-all-normal tags or a `body`
    that `to` would take as the data, §4.1.5/§5.9.3, governs the **decode
    path** — where any other message needs no commitment
    to round-trip. An explicit `commit unsigned` request is different: it MUST
    yield a commitment, so if the round-trip's `from` produced none, attach the
    unsigned content commitment explicitly.) No wallet is used.
  - **`signed` / `rsa-pss-sha256`:** convert the message to a transaction via
    `to` (with private keys reset), **sign** it with the wallet (RSA-PSS over the
    §5.8 preimage; this sets `owner`, `signature`, `signature_type`, and the
    signed ID), then convert the signed transaction back via `from`. The result
    carries a signed `tx@1.0` commitment. A transaction with `data` is signed
    with the root that §5.3 gives a transaction without one, not with the root
    of a commitment that the message has: its data may have changed since.
- **Returns:** `{ok, CommittedMessage}`.
- **Side effects:** none (no cache writes); consumes the configured wallet.
- **Errors:** a signed commit without a `committer` in the request fails with
  `error:function_clause` in `lib_arweave_common:sign/4`.

### 4.4 `verify` — verify a `tx@1.0` commitment

- **Reads:** the message (Base) and the commitment (Req), which `message@1.0`
  selects by the `committers` / `ids` of its request.
- **Behaviour:** reset private keys of the message, which `message@1.0`
  restricts to the commitment and its committed keys, and convert it to a
  transaction via `to`. A signed commitment verifies when the transaction
  passes the Arweave transaction verification (§5.10), the commitment's ID is
  the signed ID and its `committer` is the URI of the owner's committer
  (§5.9.2); an unsigned one when its ID is the unsigned ID and it has no
  `committer`. Its `committed` keys must also be the keys that `from` gives
  the transaction and the keys of the message.
- **Returns:** `{ok, Boolean}` — `true` iff the commitment verifies as above.
  **Failure-closed:** any inconsistency (bad signature, ID ≠ hash of
  signature, data/size/root mismatch, unsupported field) yields `false`.
- **Side effects:** none.

## 5. Data formats & encodings (normative — byte-exact where content-addressed)

### 5.1 Transaction field set & message-key mapping

The transaction header carries these fields. The **message key** column is the
TABM key the field maps to (lower-case, binary). All ID-typed fields are
**base64url** on the wire (43 chars for 32-byte values), **never hex**. Integer
fields are decimal text.

| Field | Message key | Type / encoding | Default (omit when equal) |
|---|---|---|---|
| format | `format` | `1` or `2` (decimal text) | `2` |
| id (signed ID) | — (commitment ID) | 32 bytes → base64url | n/a (derived) |
| unsigned_id | — (commitment ID) | 32 bytes → base64url | n/a (derived) |
| anchor (`last_tx`) | `anchor` | 0, 32, or 48 bytes → base64url | `<<>>` (empty) |
| owner (public key) | — (`owner` in commitment) | bytes → base64url | all-zero (unsigned) |
| target | `target` | 0 or 32 bytes → base64url | `<<>>` (empty) |
| quantity | `quantity` | non-negative integer (decimal text) | `0` |
| data | `data` (or `ao-data-key`) | raw binary, or nested bundle | `<<>>` (empty) |
| data_size | `data-size` | non-negative integer (decimal text) | `0` |
| data_root | `data-root` (§5.4), `field-data-root` (§5.9.2) | 0 or 32 bytes → base64url | `<<>>` (empty) |
| signature | — (`signature` in commitment) | bytes → base64url | all-zero (unsigned) |
| reward | `reward` | non-negative integer (decimal text) | `0` |
| denomination | — | only `0` supported | `0` |
| signature_type | — (`type` in commitment) | see §5.9.1 | RSA `{rsa,65537}` |
| tags | message keys + `original-tags` | see §5.4, §5.6 | `[]` |

The set of **base fields** the codec promotes between the header and the
message body (and may carry in the commitment as `field-<name>`) is exactly:
`anchor`, `format`, `quantity`, `reward`, `target`, `data-root`, `data-size`.
(`owner`/`signature`/`id` live only in the commitment, never as body keys, and
so does the data root of a transaction with `data`.)

### 5.2 Transaction validity (enforced by `from` and `to`)

A transaction MUST satisfy all of the following or the conversion throws
`{invalid_field, Field, Value}`, `Field` as in parentheses below (§8). The
check exists so the rest of the codec can assume a well-formed header.

1. `format` ∈ {1, 2} — else `invalid_field` (format).
2. `id` and `unsigned_id` are each exactly 32 bytes — else `invalid_field`.
3. `anchor` size ∈ {0, 32, 48} — else `invalid_field` (anchor).
4. `owner` is a binary — else `invalid_field` (owner).
5. `target` size ∈ {0, 32} — else `invalid_field` (target).
6. `quantity`, `data_size`, `reward` are integers — else `invalid_field`.
7. `data_root` size ∈ {0, 32} — else `invalid_field` (data_root).
8. `signature` is a binary — else `invalid_field` (signature).
9. `denomination` = 0 (denomination changes are unsupported) — else
   `invalid_field` (denomination).
10. `signature_type` ∈ {RSA `{rsa,65537}`, ECDSA `{ecdsa,secp256k1}`} — else
    `invalid_field` (signature_type).
11. `tags` is a list of `{binary name, binary value}` pairs; each name ≤ 1024
    bytes, each value ≤ 3072 bytes — else `invalid_field` (tags / tag_name /
    tag_value). A non-tuple tag entry is `invalid_field` (tag).

A Base passed to `to` that is not a transaction, a binary or a map is
`{invalid_tx, Base}`.

### 5.3 Normalisation

Before IDs are taken, a transaction is normalised deterministically:

- If `data` is a non-empty binary: set `data_size = byte_size(data)`. A
  format-2 transaction that has a `data_root` keeps it: its signature covers
  that root, whatever the chunks of its data. Otherwise set `data_root` to the
  root of `data` in 256 KiB chunks: for format 1, fixed-size chunks with a
  smaller final chunk; for format 2, the size-balanced chunks of the
  arweave-js library, in which the last two chunks split the remainder when
  the final chunk would be under 32 KiB.
- If `data` is a map/list (a bundle), serialise it to its ANS-104 bundle bytes
  first, adding the `bundle-format`/`bundle-version`(/`bundle-map`) tags as
  needed, then normalise as a binary.
- Recompute **`unsigned_id`** = SHA-256 of the signature preimage computed with
  `owner` forced to the all-zero default (§5.8); if a non-default `signature` is
  present, recompute **`id`** = SHA-256 of the `signature` (§5.8). An unsigned
  transaction has `id` = all-zero.

### 5.4 Decoding components (`from`)

From a (normalised, validated) transaction the codec derives three maps:

- **Fields map:** for each base field whose value differs from its default
  (§5.1), an entry `key → encoded-value` (IDs base64url, integers decimal text,
  `format` encoded as `1` when format = 1, omitted when 2). `data-root` /
  `data-size` are included **only when `data` is empty** (a header that has a
  data root but no inline data). When `data` is present, `data_size` is derived
  from it and the data root is not a body key: only the commitment's
  `field-data-root` carries it (§5.9.2).
- **Tags map:** the transaction's tag list, **normalised**: each tag name is
  lower-cased; **duplicate** names are aggregated into a single key whose value
  is an RFC 9651 Structured-Fields **list** of the individual values, in
  original order (e.g. two `Test-Tag` values become `test-tag` → `"v1", "v2"`).
  An `ao-types` tag is kept as it is (§5.6).
- **Data map:** empty if `data` is empty. If `data` decodes to a map (a bundle),
  each child is decoded as an `ans104@1.0` item (`lib_arweave_common:from/3`)
  and keys normalised. Otherwise a single entry `DataKey → data`, where
  `DataKey` is the value of the `ao-data-key` tag if present, else `data`.

### 5.5 Base-message assembly & field/tag precedence (`from`)

The base message contains exactly the **committed keys** (§5.6). For each key the
value is chosen by precedence **data > fields > tags** (first hit wins); a key
found in none throws `{missing_key, Key}`.

This precedence is what lets a tag named like a base field (`anchor`, `quantity`,
…) but carrying a **non-conforming value** (not a valid ID / not an integer)
survive: such a value cannot become the typed header field, so it is preserved as
a tag and, because the field reconstruction (§5.7) rejects it, the message keeps
the tag value.

A field at its default value (§5.1) is not in the fields map, so a tag of the
same name gives the key's value. For a transaction with a `tx@1.0` commitment
(§5.9), a base field `<name>` and a tag of the same name give:

| Field | Tag of the same name | `field-<name>` in the commitment | The message's key `<name>` |
|---|---|---|---|
| at its default value | absent | absent | absent |
| at its default value | present | absent | the tag's value |
| not at its default value | absent | the field's value | the field's value |
| not at its default value | present | the field's value | the field's value |

A base field's name is reserved, so in both rows with a tag the commitment's
`original-tags` (§5.6) contains the tag, and `to` reproduces it. The same rows
apply to `ans104@1.0`, whose base fields are `target` and `anchor`.

A caller that wants the field reads the commitment's `field-<name>`, and takes
the default value when it is absent: the message's key can be the value of a
tag.

`commit` takes fields from its request. For a message without a `tx@1.0`
commitment, `to` takes the field `<name>` from a request key `field-<name>`
over the message's key `<name>`, and `commit` gives `to` its request; a key
whose field is then at its default value becomes a tag (§5.6). A `quantity: 1`
key committed with `field-quantity: 0` gives a transaction with quantity 0 and
the tag `quantity: 1`. `to` refuses a request `field-<name>` that is not at its
default value and differs from the message's key, or that the message does not
have (`{request_field_differs, Name}`).

Encoding a message without `data` (`to`): the encoder reconstructs the header's
`data_root`/`data_size` from the `field-data-root`/`field-data-size`
commitment keys (or message keys) and leaves `data` empty, producing a
value-transfer header that commits to a data root without inlining the bytes.

### 5.6 Tag handling & committed-key derivation

- **Committed keys (`from`)** = the de-duplicated union, in this order, of:
  1. **data keys** — the sorted keys of the data map;
  2. **tag keys** — every tag's normalised name, **excluding** the metadata tags
     `bundle-format`, `bundle-version`, `bundle-map`, `ao-data-key`, and `data`
     (a `data` *tag* is never promoted to a body key — it would collide with the
     message's actual data; it is preserved only inside `original-tags`), and
     any private name (`hb_private:is_private/1`);
  3. **field keys** — each base field present whose name also appears among the
     base-fields / tags / data (i.e. fields that have a corresponding value to
     commit).
- **Tag emission (`to`)** reverses this. The committed key order is taken from
  the commitment's `committed` list (or, with no commitment, the message's sorted
  keys minus `commitments`). For each committed key, emit a tag `{key, value}`
  from the message **EXCEPT**:
  - the **data key** (it becomes the transaction body, not a tag);
  - any **base field** key (§5.1) whose field is not at its default value: the
    field is reconstructed as a typed transaction **header field**, not a tag —
    emitting it *also* as a tag double-encodes it and corrupts the signature
    preimage / ID. A base field key whose field is at its default value — for
    example a value that is not a valid field value, or a request
    `field-<name>` at its default value (§5.5) — is emitted as a tag.
  - any value that is **not a top-level scalar** (a map / sub-message value, e.g.
    under `bundle=true`): only scalar binary values become tags; map values are
    bundle data, not tags (§5.7).
  Prepend bundle tags (from the commitment's `bundle-format`/`bundle-version`/
  `bundle-map`) and, when the data lives under a key other than `data`, an
  `{<<"ao-data-key">>, DataKey}` tag. A committed key with no value in the
  message is `{missing_committed_key, Key}`.
- **`original-tags`:** when the transaction's tags are **not all-normal** — any
  tag name is not already lower-cased or is private, or duplicates exist, or a
  tag name equals a reserved field name (`data`, `ao-data-key`, `ao-types` or a
  base field), or the data is a list bundle — the *exact*
  original tag list is preserved in the commitment as `original-tags`: a numbered
  message `{"1" → #{name, value}, "2" → …}` (1-based, in original order,
  preserving name case and duplicates). On `to`, while the message still has
  the commitment's field and tag values, `original-tags` is decoded back to the
  literal ordered tag list and used verbatim (it takes priority over
  re-deriving tags from body keys); a message whose values changed is given
  tags derived from its keys, which the commitment's signature does not verify.
  When the tags *are* all-normal, `original-tags` is omitted.
- **`ao-types` tags** are kept as they are on decode; `structured@1.0`
  lowercases the keys that the tag lists when it decodes the message.

### 5.7 Data field & bundling

- **`ao-data-key`:** if a tag `ao-data-key = K` is present, the transaction's
  data binary is exposed in the message under key `K` (and `to` re-adds the
  `ao-data-key` tag whenever the data key is not `data`). The encoder chooses the
  data key as: `data` if the message has a non-empty `data` key; else, if the
  message has a commitment of this codec, the value of the `ao-data-key` tag
  in the commitment's `original-tags`, or `data`; else `body` if the message
  has a `body` key (non-link); else `data`; a message's own `ao-data-key` is
  never the data key, and is carried in the data as a nested item.
- **`bundle = false`:** `structured@1.0` replaces each nested sub-message with a
  link, which the transaction carries as a tag whose value is an `ao:` URI. A
  key whose value or name exceeds the tag-size limits in §5.12 is still put
  into `data` as an ANS-104 bundle.
- **`bundle = true`:** the message's nested sub-messages (map-valued keys, and
  any key whose value or name exceeds the tag-size limits in §5.12) are
  materialised into `data` as an **ANS-104 bundle** (the sibling `ans104@1.0`
  format), with `bundle-format = binary`, `bundle-version = 2.0.0`, and
  `bundle-map` = the base64url ID of the bundle manifest. Each nested item is
  encoded recursively. The exact bundle byte layout is defined by the ANS-104
  format and is out of this spec's scope; what is normative here is that the
  triggering tags and `bundle-map` ID are produced and that the round-trip is
  lossless.

### 5.8 Signature preimage (signature data segment) — byte-exact

The preimage is hashed/signed and its SHA-256 is the transaction ID. It is
computed with the **Arweave deep-hash** function (§5.8.1) over an ordered list of
byte strings; the list depends on format and signature type. **All integers are
ASCII decimal; the format number is ASCII decimal; tags are
`[[name, value], …]`.** When `denomination > 0` (unsupported here, always 0) a
denomination element is prepended; with denomination 0 it is omitted.

- **Format 2, RSA** — list, in order:
  `[ format, owner, target, quantity, reward, anchor, tags, data_size, data_root ]`.
- **Format 2, ECDSA** — identical but **omitting `owner`** (the public key is
  recovered from the signature, not committed):
  `[ format, target, quantity, reward, anchor, tags, data_size, data_root ]`.
- **Format 1** (legacy): the v1 segment — with denomination 0, a flat
  concatenation `owner ‖ target ‖ data ‖ quantity ‖ reward ‖ anchor ‖
  tags-flattened`; with denomination > 0, a deep-hash list. (Format 1 is
  accepted but format 2 is the default; reimplementers MAY treat format-1
  signing as out of scope and only need to *decode* format-1 headers.)

The **unsigned ID** is SHA-256 of this segment computed with `owner` set to the
all-zero default. The **signed ID** is SHA-256 of the `signature` bytes
themselves (not of the preimage).

#### 5.8.1 Deep hash

`deep-hash(X)`:
- For a **binary** `B`: `SHA-384( SHA-384("blob" ‖ dec(byte_size B)) ‖ SHA-384(B) )`.
- For a **list** `L`: fold `Acc₀ = SHA-384("list" ‖ dec(length L))`, then for each
  element `E`: `Acc' = SHA-384( Acc ‖ deep-hash(E) )`; result is the final `Acc`.

(`dec(n)` is ASCII decimal of `n`; `‖` is concatenation. SHA-384 throughout.)

### 5.9 Commitment shape

A `tx@1.0` commitment is an entry in the message's `commitments` map, keyed by
the relevant ID (base64url, 43 chars): the **signed ID** for a signed commitment,
the **unsigned ID** for an unsigned one.

#### 5.9.1 Signature-type token

The commitment's `type` field encodes the signature type as a token:
- RSA `{rsa,65537}` → `rsa-pss-sha256`
- ECDSA `{ecdsa,secp256k1}` → `ecdsa-secp256k1-sha256`
- An **unsigned** commitment uses `type = unsigned-sha256`.

On decode, `unsigned-sha256` maps back to the RSA key type (an unsigned message
is treated as RSA-typed for preimage purposes).

#### 5.9.2 Signed commitment fields

- `commitment-device` = `tx@1.0`.
- `committer` = the URI that `hb_committer:uri/2` gives for the committer of
  the owner's key: `uri-did-ar-<L><B>` with type bits `000` for an RSA or
  ECDSA key.
- `committed` = the ordered committed-key list (§5.6).
- `signature` = the signature bytes, base64url.
- `owner` = base64url(owner public key).
- `type` = the §5.9.1 token.
- `bundle` = `true`/`false` (string) — whether the transaction's tags carry
  `bundle-format` (i.e. the data is a bundle) and no tag value is an `ao:` URI.
- `field-<name>` for each base field whose header value is not its default
  (§5.1), with or without a tag of the same name (§5.5), and `field-data-size`
  only without `data` — value encoded as in §5.1.
- `field-data-root` for every transaction with a non-empty `data_root`, with
  data or without: the signed root, base64url. For a transaction with `data`
  it is the only place the root is carried, and `to` gives the transaction
  this root as it is, without computing a root from the data.
- `original-tags` — present only when tags are not all-normal (§5.6).
- `bundle-format` / `bundle-version` / `bundle-map` — copied from the tags when
  present (so `to` can reproduce them).
- Any field whose value is the internal "unset" sentinel is omitted.

#### 5.9.3 Unsigned commitment fields

Same as signed, **minus** `committer`, `signature`, `owner` (there is no signer)
and `bundle-format` / `bundle-version` / `bundle-map`.
`type = unsigned-sha256`. Keyed by the unsigned ID. An unsigned commitment is
produced by `from` only when the tags are not all-normal, or the message has no
`data` key (or an empty one) and a non-link `body` key (otherwise the message
needs no commitment to round-trip).

#### 5.9.4 Reconstructing sig info (`to`)

From the matched commitment: `signature` = base64url-decode of `signature` (or
the all-zero default if absent); `owner` = base64url-decode of `owner` (or
all-zero default); `signature_type` = decode of `type`; tags = decode of
`original-tags` (or `[]`). The `field-` prefixed keys then reconstruct the
header fields (§5.7). The tags and fields come from the commitment only while
the message still has their values; for a message whose values changed, tags =
`[]` and its own keys give the fields (§5.6).

### 5.10 Verification (`verify` / round-trip)

A reconstructed transaction verifies iff **all** hold:
1. `format` ∈ {1, 2} and the `signature_type` is permitted for that format
   (format 1: RSA only; format 2: RSA or ECDSA).
2. `quantity ≥ 0`, and for format 2 `data_size ≥ 0`.
3. The owner address ≠ `target` (a transaction may not send to its own address).
4. `id` = SHA-256(`signature`) (the ID is the hash of the signature).
5. The signature validates against the owner public key over the §5.8 preimage.
6. For format 2: `(data_size == 0) == (data_root == <<>>)` (size and root agree
   on emptiness); and when `data` is inlined, `data_size == byte_size(data)` and
   `data_root` is the root of `data` in the size-balanced chunks of §5.3 or,
   checked second, in fixed-size 256 KiB chunks with a smaller final chunk,
   which an Arweave node uses for data posted in a transaction. The `data_root`
   checked is the signed root of `field-data-root`, so data changed after
   signing fails this rule.

### 5.11 base64url

Every ID, address, owner key, signature, anchor, target, and data_root is encoded
with **URL-safe base64 without padding** on the wire. Hex MUST NOT be used.
32-byte values encode to 43 characters.

### 5.12 Size limits

- A tag name MUST be ≤ 1024 bytes; a tag value ≤ 3072 bytes. A message key or
  value exceeding these is offloaded into the bundle `data`, not emitted as a
  tag.
- The number of emitted tags MUST be ≤ 128; exceeding it is
  `{too_many_keys, TABM}`.

## 6. Ordering, freshness & caching

- **Determinism:** all derivations are pure functions of the input transaction /
  message. The signature preimage fixes field order (§5.8); committed-key order
  is data-keys (sorted) then non-metadata tag-keys (in tag order) then field-keys
  (§5.6); tag emission follows `original-tags`, else the commitment's
  `committed` order, else the sorted keys of a message without a commitment
  (§5.6). Two conformant implementations MUST produce byte-identical
  transactions, IDs, and commitments for the same input.
- **Tag order & duplicates:** tags that are not all-normal keep their order,
  case and duplicates through `original-tags`; all-normal tags keep their order
  through the commitment's `committed` list. All-normal does not include order:
  an unsigned transaction with all-normal tags gets no commitment (unless
  §4.1.5 gives it one), and `to` gives its tags in sorted key order, so one
  whose tags are not sorted re-encodes with another unsigned ID.
- The device performs **no result caching** of its own; it transforms the
  supplied message/transaction. (`structured@1.0` reads linked values fully
  for a bundled conversion.)

## 7. Security & authority

- **Signing** requires a `committer` in the request (`message@1.0` gives the
  node's `priv-wallet`); a signed `commit` without one fails
  (`error:function_clause`), not a silent unsigned result.
- **Verification is failure-closed:** any reconstruction inconsistency (§5.10)
  yields `false`, never an exception-as-success. A signed commitment whose
  committed key values were altered will fail because the recomputed signature
  preimage — hence the ID and signature check — no longer matches.
- The committer is the URI of the **owner's** committer (§5.9.2), independent of
  the supplied ID; `verify` recomputes everything from the reconstructed
  transaction, so a forged ID or mismatched committer cannot pass.
- Private (`priv*`) keys are reset before encoding/verification and never enter
  the transaction or a commitment.
- Denomination changes and signature types other than RSA-PSS / ECDSA are
  rejected (`invalid_field`), failing closed against unsupported variants.

## 8. Errors

All error atoms use underscores.

- `{invalid_tx, Base}` — Base passed to `to` is neither a transaction, a
  binary, nor a map.
- `{invalid_field, Field, Value}` — a transaction field failed a §5.2 check.
  `Field` is one of: `format`, `id`, `unsigned_id`, `anchor`, `owner`,
  `target`, `quantity`, `data_size`, `data_root`, `signature`, `reward`,
  `denomination`, `signature_type`, `tags`, `tag`, `tag_name`, `tag_value`.
- `{missing_key, Key}` — `from` could not find a committed key in data,
  fields, or tags.
- `{missing_committed_key, Key}` — `to` could not find a committed key's
  value in the message while emitting tags.
- `{multiple_ans104_commitments_unsupported, TABM}` — the message carries
  more than one `tx@1.0` commitment, which `to` cannot disambiguate.
- `{too_many_keys, TABM}` — more than 128 tags would be emitted.
- `{request_field_differs, Name}` — a request `field-<name>` is not at its
  default value and differs from the message's key, or the message does not
  have the key (§5.5).
- `error:function_clause` in `lib_arweave_common:sign/4` — a signed `commit`
  request has no `committer`.
- `{invalid_signature_type, Type}` — the `type` of a commitment that `to`
  encodes is not a token of `ar_tx:deserialize_sig_type/1`.

## 9. Composition

- `tx@1.0` is reached through the standard message commitment surface: a message
  with `device` left default uses `message@1.0`, whose `commit`/`verify`/`id`
  delegate to the named commitment device — pass `commitment-device = tx@1.0` (or
  request the `tx@1.0` device for a codec conversion) to drive this device.
- Conversions go **through TABM**: `structured@1.0` rich⇄TABM on one side, this
  codec TABM⇄transaction on the other. To turn a rich message into an Arweave
  transaction, convert rich → TABM (`structured@1.0`) then TABM → transaction
  (`tx@1.0`), and the reverse to decode.
- A message decoded by `from` is an ordinary AO-Core message carrying a `tx@1.0`
  commitment; its `id`/`verify`/`committers` behave per `message@1.0` over that
  commitment.
- The binary-wrapper convention (§4.1.2 / §4.2.1) lets a raw binary be given a
  stable Arweave identity and recovered exactly, so binaries compose into bundles
  alongside structured items.

## 10. Conformance (normative checklist)

An implementation MUST exhibit all of the following, each checkable via the
public codec / commit / verify surface:

1. `from` of a transaction whose only tag is `{ao-type, binary}` returns the
   raw `data` binary; `to` of a binary returns a format-2 transaction with that
   single tag and `data` = the binary; the pair round-trips a binary exactly.
2. `from`/`to` reject a transaction failing any §5.2 check with `invalid_field`
   (correct field), and `to` rejects a non-transaction/non-binary/non-map Base
   with `invalid_tx`.
3. Base fields map to keys exactly per §5.1; IDs/addresses/keys/anchor/target/
   data_root are base64url (43 chars for 32 bytes), never hex; integer fields are
   decimal text; `format` is omitted when 2 and emitted as `1` when 1.
4. A field equal to its default (§5.1) is omitted from the decoded message; a
   `target`/`anchor`/`data_root` of empty, a `quantity`/`reward`/`data_size` of
   0, are not surfaced.
5. The unsigned ID equals SHA-256 of the §5.8 signature preimage with owner =
   all-zero; the signed ID equals SHA-256 of the signature bytes; both encode to
   43-char base64url. The deep-hash is exactly §5.8.1 (SHA-384, `blob`/`list`
   tags, decimal lengths).
6. The signature preimage element order is exactly §5.8 (format 2 RSA includes
   owner; format 2 ECDSA omits owner; integers and format are ASCII decimal; tags
   as `[[name,value],…]`).
7. Decoded tags are lower-cased; duplicate tag names aggregate into one key
   whose value is an SF list of the values in order; `data`, `ao-data-key`,
   the three `bundle-*` tags and private names are excluded from body keys.
8. Committed-key order is data-keys (sorted) ++ non-metadata tag-keys (tag order)
   ++ present field-keys, de-duplicated; `committed` in the commitment matches
   this.
9. When (and only when) the transaction's tags are not all-normal (case differs,
   duplicates, a private name, a reserved-field name as a tag, or a list
   bundle), `from` attaches a commitment carrying `original-tags` as a 1-based
   numbered `{name,value}` message in original order; `to` reproduces the exact
   original tag list from it.
10. Base-message value precedence is data > field > tag; a same-named tag whose
    value is non-conforming for the typed field is preserved (header field keeps
    the valid value, tag value survives via `original-tags`); a committed key
    missing from all three sources errors `{missing_key, Key}`.
11. A signed commitment has `commitment-device = tx@1.0`, `committer` = the
    URI of the owner's committer (§5.9.2), `signature` (base64url), `owner`
    (base64url), `type` per §5.9.1, `bundle`, the relevant
    `field-<name>` keys, and `original-tags` when applicable; an unsigned
    commitment omits `committer`/`signature`/`owner` and uses
    `type = unsigned-sha256`, keyed by the unsigned ID.
12. `commit type=signed` produces a message that `verify` accepts and whose
    reconstructed transaction satisfies §5.10; `commit type=unsigned` produces an
    unsigned commitment whose key is the unsigned ID and whose presence
    re-normalises the message.
13. `verify` returns `true` for a faithfully reconstructed signed transaction and
    `false` (never an exception) when any committed value, the signature, the ID,
    or the data/size/root consistency is broken; it recomputes identity rather
    than trusting supplied IDs.
14. `bundle=true` materialises nested sub-messages into `data` as an ANS-104
    bundle, adding `bundle-format=binary`, `bundle-version=2.0.0`, and
    `bundle-map=<base64url manifest id>`, and round-trips the nested structure
    losslessly; `bundle=false` gives each nested sub-message as a tag whose
    value is an `ao:` URI (a link).
15. A real signed Arweave transaction fetched from a gateway, decoded by `from`,
    re-encoded by `to`, reproduces a transaction with the identical signed ID and
    a valid signature (interoperability with the external Arweave standard).
16. Emitting more than 128 tags errors `{too_many_keys, TABM}`; tag name > 1024
    / value > 3072 bytes are offloaded to the bundle data, not emitted as tags.

## 11. Out of scope

- The in-memory representation of a transaction header and of messages/links.
- The **ANS-104 data-item** format and the exact bundle byte layout (the sibling
  `ans104@1.0` device), beyond the requirement that `tx@1.0`'s bundled `data`
  uses it and round-trips losslessly.
- The Merkle algorithm producing `data_root` from chunks is defined by the
  external Arweave standard; only the chunks that the codec computes a root
  over (§5.3, §5.10) are pinned here.
- The cryptographic primitives of RSA-PSS / ECDSA and SHA-256 / SHA-384
  themselves; the on-chain economic checks Arweave performs (fee sufficiency,
  overspend, last_tx validity) which `verify` here does **not** perform.
- Performance, storage strategy, and the gateway/HTTP fetch path used only by
  tests.

## Open questions

- **Format-1 signing.** Format-1 headers are accepted and decodable, and the v1
  preimage is specified (§5.8), but the reference codec's production path is
  format-2-centric. Whether a reimplementer must be able to *sign* (not just
  decode) format-1 transactions is unconfirmed; treat format-1 signing as
  optional pending validation.
- **ECDSA commit.** Decoding and verifying ECDSA (`ecdsa-secp256k1-sha256`)
  transactions is in scope and required (§5.9.1, §5.10), but the `commit` key as
  specified signs only RSA-PSS (`rsa-pss-sha256`); whether `commit` must also be
  able to *produce* ECDSA commitments is unconfirmed (the reference treats ECDSA
  signing tests as disabled).
- **`anchor` of 48 bytes.** Validity permits a 48-byte anchor (§5.2), and the
  base-field reconstruction gives the header any anchor that a `field-anchor`
  or `anchor` value decodes to (base64url).
- **Float / rich values in tags.** `ao-types` on a transaction's tags is kept
  as it is but the reference carries a disabled test indicating typed
  (non-binary) tag values are not fully supported; committed messages SHOULD
  avoid non-binary tag values.
