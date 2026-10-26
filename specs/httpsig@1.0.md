# `httpsig@1.0` — the default commitment, ID, and HTTP-message codec device

- **Device name:** `httpsig@1.0`
- **Depends-on:** `message@1.0` (the commitment data model — `id` / `commit` /
  `verify` / `committers` / `committed` / `commitments` — which delegates the
  cryptography here), `structured@1.0` (the TABM normal form, percent key-escaping,
  and Structured-Fields value encoding that the form signed over is built from).
  Both specs are provided to reimplementers.
- **Status:** Draft

## 1. Overview

`httpsig@1.0` is the **default commitment device** of AO-Core and the **default
ID device**: unless a message names another, every signature, every HMAC, and
every content identifier in the system is produced by this device. It is also a
**codec**: it maps a message's TABM form to and from an **HTTP message** (a set
of header fields plus an optional body) so that messages can travel natively over
HTTP/1.1, HTTP/2 and HTTP/3 and be signed in transit.

Its commitment format is **RFC 9421 (HTTP Message Signatures)**: a commitment is
expressed on the wire as a `signature` / `signature-input` Structured-Fields
dictionary pair, computed over a **signature base** assembled from the covered
message fields. Bodies are covered indirectly through an RFC 9530
`content-digest` header. Structured-Fields serialisation throughout is **RFC 9651
(Structured Field Values)**.

This device is the cryptographic root of trust for the whole protocol and is
**content-addressed-critical**: a single wrong byte in the signature base, the
`content-digest`, the keyid, or the multipart encoding yields a different ID and
a different (or invalid) signature. Every byte is therefore pinned below. Two
conformant implementations MUST produce **byte-identical** signature bases,
content digests, commitment maps, and IDs for the same input.

## 2. Concepts & terminology

- **Commitment.** An entry in a message's `commitments` map (see `message@1.0`),
  keyed by **commitment ID** and attesting to a subset of the message's keys.
  This device produces two kinds:
  - a **signed commitment** (a `rsa-pss-sha512` signature), and
  - an **HMAC commitment** (a `hmac-sha256` content commitment) — the device's
    *content/unsigned* commitment, used to content-address a message.
- **TABM (Type-Annotated Binary Message).** The flat, all-binary normal form (see
  `structured@1.0`). Every commitment and ID this device computes is computed over
  the TABM form of the message; the device never signs rich values directly.
- **HTTP message.** A map of binary, **lowercase** field names → binary field
  values, plus an optional `body`. This is the codec's output (`to`) and input
  (`from`). It is NOT itself a TABM: structured/large values may have been moved
  into a multipart body, and `signature` / `signature-input` / `content-digest`
  fields are present.
- **Signature base.** The exact byte string that is signed (for `rsa-pss-sha512`)
  or HMAC'd (for `hmac-sha256`), constructed per RFC 9421 §2.5 (§5.5 below).
- **Covered components / `committed` list.** The ordered list of field names the
  signature base covers. Two related forms exist and MUST be kept distinct:
  - the **encoded** (`httpsig@1.0`) form, used inside the signature base and on the
    wire in `signature-input` (e.g. `content-digest`, `@method`); and
  - the **decoded** (AO-Core) form, stored as the commitment's `committed` key
    (e.g. `body`, `method`).
  §5.4 pins the mapping between them.
- **keyid.** The URI of the committer that produced a commitment (§5.7): for a
  signed commitment, the URI that `hb_committer:uri/2` gives for the signing
  key; for an HMAC commitment, `hmac:constant:<value>` or `hmac:secret:<ID>`.
- **committer.** The keyid of a signed or `hmac:secret:` commitment (§5.7);
  absent from an `hmac:constant:` commitment.
- **Structured Fields (SF).** RFC 9651 *items*, *lists* and *dictionaries*. This
  spec writes `SF-string`, `SF-binary`, `SF-token`, `SF-integer`, `SF-list`,
  `SF-dictionary` for the RFC 9651 serialisations, pinned byte-exactly in §5.1.

## 3. Device interface

### 3.1 Dispatch shape

**Explicit-keys.** The device answers a fixed set of keys, each by its own
behaviour: the codec keys `to`, `to-hint`, `from` and `serialize`; the
commitment keys `commit` and `verify`; and `add-content-digest` and
`normalize-for-encoding`. It does **not** install a default handler and does
not capture arbitrary keys — message inspection/mutation (`keys`, `set`, …) and
the `id` / `committers` / `committed` surface remain with `message@1.0`, which
calls into this device's `commit` / `verify` for the cryptography.

This device is reached two ways:

1. As a **codec** (`to` / `from`), when a message is converted to/from
   `httpsig@1.0` form (e.g. for HTTP transport).
2. As a **commitment device** (`commit` / `verify`), invoked by `message@1.0`'s
   `id` / `commit` / `verify` when `commitment-device` (or the node default) is
   `httpsig@1.0`. `message@1.0` converts the target to TABM **before** calling
   here, and puts the `commitments` of the result on the original message.

### 3.2 Message shapes

- **`commit` request** carries:
  - `type` (REQUIRED): one of `signed`, `unsigned`, `rsa-pss-sha512`,
    `hmac-sha256` (§4.1). `signed` is an alias for `rsa-pss-sha512`; `unsigned`
    is an alias for `hmac-sha256`.
  - `committed` (OPTIONAL): an explicit list (or numbered message) of AO-Core key
    names to cover. Default: derived from the base (§5.3).
  - `bundle` (OPTIONAL, default `false`): if true, the commitment is computed over
    the **fully-inlined** form of the message (linked sub-messages resolved into
    the body) and the commitment carries a `bundle = "true"` parameter (§5.8).
  - `committer` (OPTIONAL): the committer that signs, as a committer message
    or its URI (§5.7); for an HMAC commitment, `hmac:constant:ao` if none. The
    secret of an `hmac:secret:` committer is `secret` in its `priv`. For a
    `type` other than `unsigned`, `message@1.0` gives the node's `priv-wallet`
    if the request has none.
- **`verify` request** is the commitment to check, with the `priv` of the
  `message@1.0` `verify` request: at minimum `type`, `signature`, `committed`,
  and the keyid material (`keyid`, and for an `hmac:secret:` keyid, `secret` in
  `priv`). The base is the TABM-form message.
- **`to` / `from`** take the message (TABM, for `to`) or HTTP message (for `from`)
  as the base; `to` honours an optional `index` flag (§4.4).

## 4. Resolved keys (normative)

### 4.1 `commit` — produce a commitment

- **Reads:** the TABM-form `Base`; `type`, `committed`, `bundle` and `committer`
  from `Req`.
- **Behaviour:**
  1. Normalise `type`: `signed` → `rsa-pss-sha512`; `unsigned` → `hmac-sha256`.
  2. For **`rsa-pss-sha512`**:
     a. The signing key is the request's `committer` (§3.2). If it has none, the
        request MUST fail: `throw({cannot_commit, {error, not_found}, Msg})`.
     b. Build the **unsigned commitment skeleton** (§5.2): `commitment-device`,
        `type = rsa-pss-sha512`, `keyid` and `committer` (both the URI of the
        request's committer, §5.7), `public-key` (the committer's `owner`, in
        base64url), and `committed` (§5.3). If the message carries a private
        `hashpath`, add it as `tag` (§5.8). If `bundle` is true, add
        `bundle = "true"`.
     c. Normalise for encoding (§5.4) to obtain the encoded message, the
        wire-form `committed` list, and the AO-Core-form `committed` list.
     d. Build the **signature base** (§5.5) and sign it with RSA-PSS / SHA-512
        (§5.6). The commitment ID is `base64url(sha256(signature))` (§5.6).
     e. Insert the commitment under that ID into `commitments`, with `signature`
        set to `base64url(signature)` and `committed` set to the **AO-Core-form**
        list.
  3. For **`hmac-sha256`**:
     a. The keyid is the URI of the request's `committer`, `hmac:constant:ao` if
        it gives none (§5.7); the `committer` is the keyid unless it is an
        `hmac:constant:` URI.
     b. Remove any existing HMAC commitment for the same keyid from the message.
     c. Build the unsigned commitment skeleton: `commitment-device`,
        `type = hmac-sha256`, `keyid`, `committed` (§5.3), `committer` (unless
        the keyid is `hmac:constant:`), and `bundle` as above.
     d. Normalise for encoding (§5.4); build the signature base (§5.5); compute
        `mac = base64url(HMAC-SHA256(key, signature-base))`.
     e. The commitment ID **is** `mac`. Insert the commitment under `mac`, with
        both `signature` **and** the map key set to `mac`, and `committed` set to
        the AO-Core-form list.
- **Returns:** `{ok, Message}` — the input message with the new commitment(s)
  added under `commitments`.
- **Side effects:** none beyond constructing the returned message. No cache or
  store writes.

### 4.2 `verify` — check one commitment

- **Reads:** the TABM-form `Base`; from `Req`: `type`, `signature`, `committed`,
  and keyid material; the commitment's other parameters.
- **Behaviour:**
  1. If a `committed` key is absent from the base, or the ID that §5.9 derives
     from `signature` is not a key of the base's `commitments`, return `false`.
     Normalise for encoding (§5.4) and rebuild the **signature base** (§5.5)
     from the request's `committed` list, exactly as `commit` would.
  2. Read the committer from the keyid with `hb_uri` (§5.7). A keyid that is not
     its committer's URI, or a `committer` other than the keyid (none for an
     `hmac:constant:` keyid), gives `false`.
  3. For **`rsa-pss-sha512`**: decode `signature` from base64url and return the
     boolean result of RSA-PSS / SHA-512 verification of that signature over the
     signature base, against the commitment's `public-key`, which must be a key
     of the committer's address, of at least 2048 bits.
  4. For **`hmac-sha256`**: recompute
     `base64url(HMAC-SHA256(key, signature-base))` and return whether it is
     **byte-equal** to the commitment's `signature` value.
  5. If keyid resolution fails, return `false` (an *unverifiable* commitment is
     not a *verified* one). A genuine processing failure surfaces as a failure
     result, not `true`.
- **Returns:** `{ok, Boolean}`.
  `message@1.0`'s `verify` requires **every** selected commitment to return
  `true`.
- **Side effects:** none.

### 4.3 `id` (delegated)

This device does not expose an `id` key directly; `message@1.0`'s `id` computes
the content ID by asking this device to `commit` with `type = unsigned` and
taking the resulting commitment's ID. Because the HMAC commitment ID **is** the
HMAC over the signature base (§4.1.3e), the content ID of a message is
`base64url(HMAC-SHA256(key, signature-base-over-the-TABM))` with the default key
(§5.7). §5.5/§5.6 pin this byte-exactly. A signed message's signed ID is the
accumulation of its signed-commitment IDs (per `message@1.0` §4.`id`); each such
ID is `base64url(sha256(signature))`.

### 4.4 `to` / `from` — the HTTP-message codec

#### `to` — TABM → HTTP message
- **Reads:** the TABM `Base`; optional `index` in `Req`.
- **Behaviour:** Encode the TABM as an HTTP message per §6, attaching the
  `commitments` as `signature` / `signature-input` fields (§5.9) and, if a body is
  present, a `content-digest` field (§5.4). With `bundle = true`,
  `hb_message:convert` has `structured@1.0` inline linked sub-messages in the
  TABM before `to`. With `index = true`, if the encoded message has no `body`
  and no `content-type`, the device resolves the message's `index` key and
  merges that result under the encoding (preferring the original message's keys
  on conflict); a binary or link base is returned unchanged.
- **Returns:** `{ok, HTTPMessage}`.

#### `from` — HTTP message → TABM
- **Reads:** the HTTP message `Base`.
- **Behaviour:** Decode per §6: parse the body (multipart or inline) back into
  fields, percent-decode field names, reconstruct `commitments` from
  the `signature` / `signature-input` fields (§5.9), and drop the wire-only fields
  (`signature`, `signature-input`, `content-digest`, a `multipart/*`
  `content-type`, and `ao-body-key`). A binary or link
  base is returned unchanged.
- **Returns:** `{ok, TABM}`.

The codec MUST round-trip: `from(to(M))` reproduces `M`'s TABM (including its
`commitments`) for any message `M`, other than what `to` does not send: in a
message with commitments, a key that no commitment covers and that would be
placed in the body (§6.1), and the unsigned commitments of its nested messages.

## 5. Data formats & encodings (normative — byte-exact)

### 5.1 Structured-Fields serialisation (RFC 9651)

All `signature`, `signature-input`, `content-digest`, and `@signature-params`
values are RFC 9651 Structured Fields. The serialisation MUST be exactly:

- **SF-integer:** decimal ASCII, no leading zeros, `-` sign if negative.
- **SF-string:** `"` + escaped + `"`, where escaping replaces `\` with `\\` and
  `"` with `\"` (no other escapes; the value is otherwise emitted verbatim).
- **SF-token:** the token bytes verbatim (no quoting).
- **SF-binary:** `:` + **standard RFC 4648 base64** (alphabet `A–Za–z0–9+/`,
  `=`-padded) of the raw bytes + `:`. **This is the one place base64 is NOT
  url-safe** — it is the RFC 9651 byte-sequence form. (All AO-Core IDs
  elsewhere remain base64url; a keyid is a URI, §5.7.)
- **SF parameters:** for each parameter in order, `;` + key + (`=` + bare-item)
  unless the value is boolean-true, in which case just `;` + key.
- **SF-list:** members joined by `, ` (comma + single space). An *inner list*
  member is `(` + items joined by single spaces + `)` followed by its parameters.
- **SF-dictionary:** members joined by `, ` (comma + single space). Each member
  is `key` (for a boolean-true value) or `key=` + item-or-inner-list. **Member
  order is the map's iteration order and is NOT defined by this spec** — see Open
  questions; for `signature`/`signature-input` the two dictionaries MUST use the
  **same** member order so names line up.

### 5.2 The commitment message (the entry inside `commitments`)

A commitment produced by this device is a map with these keys. All values other
than `committed`, `created` and `expires` are binaries.

| Key | Present when | Value |
|---|---|---|
| `commitment-device` | always | `httpsig@1.0`. |
| `type` | always | `rsa-pss-sha512` or `hmac-sha256`. |
| `committed` | always | the **AO-Core-form** covered-key list, as a **plain ordered Erlang/AO list of key-name binaries** (e.g. `[<<"basic">>, <<"num">>]`), in covered order (§5.4). Reading `committed` from a commitment in-memory yields this LIST — NOT a numbered message. (The numbered-message form is only how a *list* is serialised when the whole message is TABM-encoded to the wire; the commitment field itself is a list.) |
| `signature` | always | **signed:** `base64url(signature-bytes)`. **HMAC:** the 43-char base64url MAC (equal to the commitment ID). |
| `keyid` | always | §5.7. |
| `committer` | unless the keyid is `hmac:constant:` | the keyid (§5.7). |
| `public-key` | signed | the committer's public key, in base64url (§5.7). |
| `tag` | iff signing a message with a private `hashpath` | the hashpath value (§5.8). |
| `bundle` | iff `bundle` requested, or the message's `httpsig@1.0` commitment has it | `"true"` (§5.8). |
| `nonce`,`created`,`expires` | iff decoded from a `signature-input` with them | RFC 9421 signature parameters carried through (§5.8). |

The **map key** under which the commitment is stored in `commitments` is the
**commitment ID** (§5.6). `commitment-device`, `committer`, and `committed` are
NOT part of the signature parameters and are reconstructed from context on
decode (§5.9); the rest are carried in `signature-input` parameters.

### 5.3 Choosing the covered-key list (`committed`)

Given the base message and the `commit`/`verify` request, the covered keys are
chosen as follows (first matching case):

1. **Explicit.** If the request carries `committed`, use exactly those keys, in
   the caller's order. A request `committed = [c, a]` yields
   `committed = [c, a]`.
2. **Replicate existing.** Else, if the base already has commitments, use the
   union of keys those commitments cover (so a second signature "stacks" on the
   same fields and the message stays representable in one HTTP encoding).
3. **All content.** Else, cover **every** key of the TABM-form base **except**
   `commitments` and the private section.

In cases 2 and 3 the covered-key list follows the order of the encoded
message's keys (§5.4).

The chosen list is stored as a numbered message and is the input to §5.4.

### 5.4 Normalising covered keys for encoding (the wire ⇄ AO-Core mapping)

Before building the signature base, the covered-key list is mapped from AO-Core
names to **wire** (`httpsig@1.0`) names, and a parallel **AO-Core** list is
produced for the commitment's `committed` key. The mapping is deterministic:

1. Order the covered keys by their numbered-message order → `RawInputs`.
2. Every key of `RawInputs` must be a key of the message, as it is or
   percent-encoded, else `throw({committed_key_missing, {keys, Missing},
   {commitment, Commitment}, {msg, Msg}})` (`verify` returns `false`, §4.2).
   → `Inputs`.
3. Encode the message, which `message@1.0` restricts to `Inputs`, to an HTTP
   message via `to` (with the same `bundle` flag). Replace a binary `body` with
   a `content-digest` field (§below).
4. The **wire** covered list (`KeysForEncoding`) is the encoded message's field
   names, with:
   - the inlined body field name replaced by `body` (and `ao-body-key` recorded
     if the inline key was renamed), and
   - `body` replaced by `content-digest`.
   It follows the order of the `committed` keys (`content-digest` takes the
   place of the first body key, and a field that the list does not contain,
   such as `ao-types`, keeps its place), except for a `commit` request without
   `committed`, where it follows the order of the encoded message's keys.
5. The **AO-Core** covered list (the commitment's `committed`) is built from the
   wire list without its `ao-body-key` field (§6.1): `content-digest` is
   replaced by the covered keys carried in the body that the list does not
   contain, in ascending byte order; and each other name has any `@`-prefix
   stripped and is percent-decoded. If the wire list contains `ao-body-key`,
   `body` is replaced by its value. This is a **plain ordered list** of the
   **original AO-Core key names** (NOT a numbered message — the commitment's
   `committed` field is a list; §5.2). Crucially:
   - When a binary `body` (or a body-lifted field) was replaced by `content-digest`
     on the **wire**, the AO-Core `committed` list still names the **original body
     key(s)** (e.g. `body`, and any field whose value was lifted into the body) —
     it does NOT contain `content-digest`. `content-digest` is a *wire* concept
     only; covering it on the wire covers those body keys by reference.
   - A `multipart/*` `content-type` field, present only on the wire, is NOT in the
     AO-Core `committed` list.
   So: `committed` is the AO-Core key names of all covered keys, in the order of
   the wire list — default (case 3) = all content keys; explicit (case 1) = the
   requested keys in the caller's order; a body-bearing message's `committed`
   includes `body`.

**`content-digest` computation (RFC 9530).** When the (restricted) message has a
binary `body`, it is removed and replaced by:

```
content-digest = sha-256=:<base64-standard(SHA-256(body))>:
```

i.e. an SF-dictionary with a single member `sha-256` whose value is the SF-binary
(standard base64, §5.1) of the SHA-256 of the raw body bytes. This is the field
that appears in the signature base in place of the body; covering it covers the
body by reference.

### 5.5 The signature base (RFC 9421 §2.5)

The signature base is a single binary built from the wire covered list
(`KeysForEncoding`, §5.4) and the encoded message:

```
"<name-1>": <value-1>
"<name-2>": <value-2>
...
"<name-k>": <value-k>
"@signature-params": <params-line>
```

- One **component line** per covered field, in `KeysForEncoding` order, formatted
  as the component name (`@`-prefixed if derived, below) wrapped in double
  quotes, then `: ` (colon + single space), then the field's value **verbatim**
  as it appears in the encoded message. Lines are joined by a single `\n` (LF,
  0x0A — **not** CRLF). If a listed component is absent from the encoded
  message, signing MUST fail (`throw({missing_key_for_signature_component_line,
  Name, {message, Msg}, {commitment, Commitment}})`).
- Then a literal `\n`, then the fixed token `"@signature-params": ` (note the
  quotes, colon, single space), then the **params line**.

**Params line.** An SF-list with exactly one inner-list member:

```
(<covered-components>);<param>=<value>;...
```

- The inner list's items are the covered components, each an SF-string, in
  `KeysForEncoding` order, **after `add_derived_specifiers`**: any component name
  equal to one of the RFC 9421 derived components — `method`, `target-uri`,
  `authority`, `scheme`, `request-target`, `path`, `query` — is prefixed with
  `@` (e.g. `path` → `@path`). All other names are unchanged.
  (`status` is intentionally NOT in this set.)
- The inner list's **parameters**, emitted in **ascending byte-sorted parameter-
  name order**, are `alg` and every other key of the commitment except
  `committed`, `signature`, `type`, `commitment-device`, `committer` and its
  private keys (§5.8). `alg` is always present and its value is the
  commitment's `type` (`rsa-pss-sha512` / `hmac-sha256`); each integer value
  is an SF-integer and each other value an SF-string (§5.8). `signature` and
  `signature-input` themselves are never parameters.

This base string is what `commit` signs/HMACs and what `verify` recomputes. Two
implementations that order the covered list and the parameters identically (both
pinned above) produce identical bases.

### 5.6 Signing, HMAC, and IDs

- **`rsa-pss-sha512`:** sign the signature base with **RSA-PSS**, digest
  **SHA-512**, MGF1-SHA-512, public exponent 65537, salt length = digest length
  (the platform RSA-PSS default). The commitment's `signature` is
  `base64url(signature-bytes)`. The commitment **ID** is
  `base64url(SHA-256(signature-bytes))` (a 43-char string).
- **`hmac-sha256`:** compute `HMAC-SHA256(key, signature-base)`. The commitment's
  `signature` **and** its ID are both `base64url(mac)` (43 chars). Because the ID
  is derived from the content (via the base), the HMAC commitment is the
  message's **content commitment**, and its ID is the message's content ID.
- **base64url** here means RFC 4648 §5 url-safe base64 **without padding** (32
  bytes → 43 chars). Hex MUST NOT be used for any ID or signature value.

### 5.7 keyid and committer derivation

A keyid is the URI of the commitment's committer, which `hb_uri` reads to
verify the commitment (§4.2). Its form follows the commitment `type`:

- **Signed commitments** (`rsa-pss-sha512`): the keyid is the URI that
  `hb_committer:uri/2` gives for the signing key, `uri-did-ar-<L><B>` with type
  bits `000` for an RSA key, which holds the address
  `base64url(SHA-256(public-key))`. The raw public key (for RSA, the big-endian
  modulus bytes) is the `public-key` parameter, in base64url.
- **`hmac:constant:`** (default HMAC): the key for the HMAC is the text after
  `hmac:constant:` in the keyid. The default keyid when none is supplied is
  `hmac:constant:ao`, so the default content key is the byte string `ao`. This
  scheme yields **no committer** (content commitments have no signer).
- **`hmac:secret:`** (authenticated HMAC): the HMAC key is the SHA-256
  of `hmac:secret:` followed by a caller-supplied `secret`; the keyid is
  `hmac:secret:<ID>` where `ID = base64url(SHA-256(secret))`, as
  `lib_secret:to_committer/1` gives it, and that keyid is the
  commitment's `committer`.

A keyid that `hb_uri` cannot read, or that is not the URI of the committer it
reads, verifies as `false`. A `commit` whose committer cannot be read fails:
`throw({cannot_commit, Error, Msg})` for `rsa-pss-sha512`, and
`error({badmatch, Error})` for `hmac-sha256`. A `commit` of another `type`
fails with `error(function_clause)`.

The committer of an RSA, Ed25519 or ECDSA (secp256k1) key is the `did-ar@1.0`
label of `base64url(SHA-256(public-key))`, and that of an Ethereum or Solana key
is its `did:pkh:` URI (`hb_committer:uri/2`). RSA is the default and the only
type produced by the `signed` alias.

### 5.8 Carried RFC 9421 parameters

- **`tag`.** If a message being signed carries a private `hashpath`, the
  commitment records it as `tag` and it appears as a `;tag="<hashpath>"` signature
  parameter. (This binds a commitment to the computation that produced the
  message.)
- **`bundle`.** If `bundle` is requested, or the message's `httpsig@1.0`
  commitment has it, the commitment records `bundle = "true"` and it appears as
  `;bundle="true"`. The signature base is then built over the fully-inlined
  message (§4.4).
- **`created`, `expires`, `nonce`.** If a commitment has these, they are
  carried through verbatim as signature parameters (`created`/`expires` as
  SF-integers, `nonce` as SF-string) and reconstructed on decode. This device does
  not itself generate or enforce them.
- **Unknown extra parameters.** Any commitment key not in the reserved set
  (`alg`, `keyid`, `tag`, `created`, `expires`, `nonce`, `committed`, `signature`,
  `type`, `id`, `commitment-device`, `committer` of an `httpsig@1.0`
  commitment, and private keys) is serialised as an additional signature
  parameter: integer →
  SF-integer; atom → SF-string of its name; binary → SF-string with `%`, each
  byte outside printable ASCII and a leading `,` or `:` percent-encoded; list
  → SF-string of `,` followed by `:<element>` for each element; map →
  SF-string of `:` followed by its `<key>:<value>` pairs joined by `", "`. A
  key has `%`, `:`, `,` and each byte outside printable ASCII percent-encoded;
  an element or a value is its own encoding (for a binary, with those bytes
  percent-encoded), with those bytes percent-encoded again (`encode_param/1`
  in `dev_httpsig_siginfo`). This is how non-`httpsig` commitment devices
  (e.g. ANS-104 original tags) carry their extra fields through the
  `signature-input`.

### 5.9 `commitments` ⇄ `signature` / `signature-input` (codec)

When encoding (`to`), each commitment becomes one member of the `signature` and
`signature-input` SF-dictionaries:

- **Member name.** `comm-<n>`, where `<n>` is the lowercase base64url of
  `SHA-256(signature-bytes)` (so HMAC and signed members get distinct, stable
  names). The two dictionaries MUST use this same name for the same commitment.
- **`signature` member value.** the SF-binary (standard base64, §5.1) of the raw
  signature bytes (the base64url `signature` field decoded back to bytes).
- **`signature-input` member value.** an SF inner-list of the commitment's
  covered components (wire form, `@`-prefixed where derived, §5.5) with the
  signature parameters: `alg` (from `type`), `keyid`, `tag`, `created`, `expires`,
  `nonce`, any extra params (§5.8), and — **only when the decoder could not
  re-derive the map key from the signature** — an explicit `id` parameter equal to
  the commitment ID. For HMAC and RSA-PSS the ID **is** a function of the
  signature (`h(sig)` form), so `id` is omitted; content-addressed devices whose
  ID is not `h(sig)` (e.g. an IPFS CID) carry `id` explicitly. Parameters whose
  value is absent are omitted; `keyid` is absent on the wire iff the commitment
  has no keyid.
- **`alg` → device.** On decode, `alg` is mapped back to `commitment-device`+`type`:
  a bare token (e.g. `rsa-pss-sha512`) means `commitment-device = httpsig@1.0`,
  `type = <token>`; a device-specifier `name@ver` (optionally `name@ver/type`)
  means `commitment-device = name@ver` with the optional `type`.

When decoding (`from`), the inverse runs: parse both dictionaries, pair members by
name, rebuild each commitment's parameters, set `committed` from the inner-list
components mapped back to AO-Core form (§5.4), decode `keyid`/`signature`, derive
`committer` from the keyid, and key the commitment by its `id` parameter if
present else `base64url(sha256(signature-bytes))` (or the raw 32-byte sig if the
signature is exactly 32 bytes). The result is byte-stable: encode∘decode and
decode∘encode are identity on commitments.

## 6. The HTTP-message encoding (`to` / `from`)

A message's TABM is encoded into header fields plus an optional (possibly
multipart) body. This is the form actually transmitted and the form the body
`content-digest` is taken over.

### 6.1 Field placement

For each TABM key (excluding `commitments`, the private section, and, in a
message with commitments, a key that no commitment covers and that would be
placed in the body):

- A **binary** value of at most **4096 bytes** that does not start or end with a
  space or tab is emitted as a header field `name: value`, unless the name is
  `signature`, `signature-input`, `content-digest`, `ao-body-key`,
  `content-length`, `connection`, `keep-alive`, `proxy-connection`, `te`,
  `trailer`, `transfer-encoding` or `upgrade`, or the value is a `multipart/*` or
  `application/x.ao.*` `content-type`, or it is the `content-type` of a message
  that contains other messages, as values or as links: bundled, such a message
  is multipart, so its own `content-type` is in a body part in every form.
  Every name is **percent-encoded** on the wire: a byte other than `a-z`, `0-9`
  and the RFC 9110 `tchar` punctuation other than `%` is encoded as `%xx` in
  lowercase hex. `from` decodes it.
- The keys are placed in the byte order of their names, and each header field
  counts the bytes of its name and value, 32 more, and its name and 3 more for
  its entry in a `signature-input`. A value that the rule above emits as a
  header field is lifted into the body instead if its name is over 4096 bytes,
  or if its field would bring the count over **12,288 bytes**. In a message with
  commitments, a key that none of them covers is placed by the rule above and
  not counted.
- Any other **binary** value is lifted into the multipart body as its own part.
- A **map** (nested message) value is encoded as a part of the multipart body,
  recursively, preserving hierarchy. The fields of a part are keys of its
  message, `ao-body-key` included, and its body is its `body` key.
- The **inline body field** — `body` if set, else `data` — is
  placed as the multipart `inline` part. If the message has only this one body
  value and no other body parts, it is emitted as the bare HTTP `body` with no
  multipart wrapper. An `ao-body-key: data` field gives a `data` inline body
  field.

### 6.2 Multipart body

When more than one body part exists (or a single nested part), the body is
`multipart/form-data`:

- Each part is preceded by `--<boundary>` then CRLF, has at least a
  `content-disposition` header (`inline` for the body part, otherwise
  `form-data;name="<part-name>"`), then, if the part has a body, a blank line
  and the body's bytes. Header lines within a part use CRLF separators and `: `
  between name and value.
- The **boundary** is `base64url(SHA-256(<the encoded parts, with their headers,
  joined by CRLF>))` — deterministic and content-derived, so the encoding is
  reproducible without knowing the message ID in advance.
- The terminating delimiter is `CRLF--<boundary>--`.
- `content-type` is set to `multipart/form-data; boundary="<boundary>"`.
- A part whose sole key is `body` is named `<key>/body` to preserve the nesting
  level; a part name containing `/` denotes a nested (non-direct-child) path.

### 6.3 `content-digest` on the encoded message

After the body is determined, if the HTTP message has a body, empty or not, a
`content-digest` field is added per §5.4 (`sha-256=:…:`). This happens on **every**
`to` of a body-bearing message — independent of any commitment (it is NOT gated on
`commit`/signing): a plain `to` of `#{body => B}` MUST attach `content-digest`. This
ADD (alongside the verbatim body, §6.3) is distinct from the sig-base REPLACE (§5.4
swaps body→content-digest in the covered set). On `from`, the body is
parsed back into parts/fields and `content-digest` is dropped (the body itself is
authoritative); a `multipart/*` `content-type` is also dropped (it is regenerated
deterministically on the next encode and is therefore not part of the logical
message).

### 6.4 Round-trip guarantees

- `from(to(M))` yields `M`'s TABM, including reconstructed `commitments`, other
  than what `to` does not send (§4.4).
- The wire-only fields `signature`, `signature-input`, `content-digest`, a
  `multipart/*` `content-type`, and `ao-body-key` are NOT present in
  the decoded TABM.
- Header-name casing on the wire is **lowercase**; names are percent-encoded
  (§6.1); values are emitted with `\`, CR and LF escaped as `\\`, `\r` and `\n`
  (structured values were already encoded by `structured@1.0`).

## 7. Ordering, freshness & caching

- **Determinism.** Given the same TABM, the covered-key selection (§5.3), the
  wire/AO-Core mapping (§5.4), the signature base (§5.5), the content-digest, the
  multipart boundary, and the HMAC content ID are all deterministic functions of
  the input. Signed-commitment IDs depend on the signature, which for RSA-PSS is
  randomised (the salt) — i.e. signing the same message twice yields **different
  signed IDs** but the **same content (HMAC) ID** and the same signature base.
- **Component vs. parameter order.** Covered components appear in the signature
  base and the `signature-input` inner list in `KeysForEncoding` order (§5.4);
  signature parameters appear in ascending byte-sorted name order (§5.5). Both are
  fixed by this spec and MUST NOT vary between implementations.
- **No caching.** The device performs no result caching; it is a pure transform
  over its inputs.

## 8. Security & authority

- **Failure-closed verification.** A commitment verifies only if its signature
  base recomputes and the cryptographic check passes; unverifiable keyid material
  yields `false`, never `true`. `message@1.0`'s `verify` requires *all* selected
  commitments to pass.
- **Content vs. authorship.** The HMAC (content) commitment proves **integrity /
  identity** with a shared/constant key — it is NOT a claim of authorship and
  carries **no committer**. Only signed (`rsa-pss-sha512`) or secret-scheme
  commitments carry a `committer`. Consumers MUST NOT treat a constant-key HMAC
  commitment as a signature by a particular party.
- **Coverage is explicit.** A commitment attests only to the fields in its
  `committed` list. A verifier MUST recompute the base from exactly that list; a
  field outside the list is unattested even if present in the message. (Changing a
  committed field invalidates the commitment because the base — and thus the
  signature/HMAC — no longer matches; `message@1.0`'s `set` drops commitments when
  a committed field changes.)
- **Private wallet.** Signed commitments require a `committer` in the request or
  the node's `priv-wallet`; absence is a hard failure, not a silent unsigned
  result.
- **Secret keys never travel.** In the secret scheme the keyid carries only
  `base64url(SHA-256(secret))`, never the secret; verification re-derives the key
  from a separately-supplied secret.

## 9. Errors

- `throw({cannot_commit, {error, not_found}, Msg})` — `commit` of a
  signed/`rsa-pss-sha512` commitment with no `committer` in the request and no
  `priv-wallet` configured.
- `throw({missing_key_for_signature_component_line, Name, {message, Msg},
  {commitment, Commitment}})` — a covered component named in `committed` is
  absent from the encoded message when building the signature base.
- `throw({cannot_commit, Error, Msg})` — the committer of an `rsa-pss-sha512`
  `commit` cannot be read from its URI, or is not an RSA key.
- `error({badmatch, Error})` — the committer of an `hmac-sha256` `commit`
  cannot be read from its URI.
- `error(function_clause)` — `commit` with no `type`, or a `type` outside §4.1.
- `throw({error, no_content_disposition_in_multipart, Headers})` — `from` met a
  multipart part lacking a `content-disposition` header.
- A `verify` over malformed/unresolvable keyid material returns `false` (not an
  error atom); only genuine processing failures propagate as a failure result.

## 10. Conformance (normative checklist)

An implementation MUST exhibit all of the following, each observable through
`commit` / `verify` / `id` (via `message@1.0`) or the codec:

1. **Type aliasing.** `commit` with `type = signed` behaves identically to
   `type = rsa-pss-sha512`; `type = unsigned` identically to `type = hmac-sha256`.
2. **One commitment on sign.** A signed `commit` adds only a `rsa-pss-sha512`
   commitment to the message, no `hmac-sha256` content commitment.
3. **Commitment shape.** Every commitment carries `commitment-device =
   httpsig@1.0`, `type`, `committed` (list of AO-Core key names),
   `signature`; signed commitments also carry `keyid` and `committer`; a
   constant-key HMAC commitment carries **no** `committer`.
4. **Signed commitment ID** = `base64url(SHA-256(signature-bytes))` (43 chars);
   `signature` = `base64url(signature-bytes)`. **HMAC commitment ID** =
   `signature` = `base64url(HMAC-SHA256(key, signature-base))` (43 chars).
5. **Content ID stability.** The content (HMAC) ID of a message is a deterministic
   function of its TABM and the default key `ao`; re-encoding or
   re-signing the message does not change it. RSA-PSS signing the same message
   twice yields **different** signed IDs but the **same** content ID and the same
   signature base.
6. **Signature base format.** The base is the covered component lines
   (`"name": value`) joined by LF, then LF, then `"@signature-params": ` and the
   SF-list params line; component order is the encoded covered-key order;
   parameters are byte-sorted by name; `alg` is always present and equals `type`;
   derived components (`method`, `path`, … per §5.5) are `@`-prefixed in the
   field lines and in the `signature-input`/params.
7. **Body coverage via content-digest.** A binary body is covered as
   `content-digest = sha-256=:<standard-base64(SHA-256(body))>:`; the `committed`
   list stores `body` (AO-Core form) while the base/`signature-input` use
   `content-digest`.
8. **SF binary uses standard base64.** `content-digest` and the `signature`
   dictionary values use `:…:` standard (`+/`, `=`-padded) base64; all IDs,
   addresses, and the `signature` commitment field use **base64url**, and a
   `keyid` is a URI (§5.7).
   No value is hex.
9. **keyid/committer.** A signed commitment's keyid and committer are the URI of
   the signing key's committer (`uri-did-ar-<L><B>`, type bits `000`, for an RSA
   key); `hmac:constant:` keyids default to `hmac:constant:ao`, use the text
   after `hmac:constant:` as the HMAC key, and have no committer; `hmac:secret:`
   keyids are `hmac:secret:<base64url(SHA-256(secret))>`, use the SHA-256 of
   `hmac:secret:` followed by the secret as the HMAC key, and are the committer.
10. **Verification.** `verify` recomputes the signature base from the commitment's
    `committed` list and returns `true` iff the RSA-PSS check passes with a
    `public-key` of at least 2048 bits (signed) or the recomputed MAC
    byte-equals `signature` (HMAC); tampering with any covered field makes
    `verify` `false`; a commitment whose keyid material cannot be resolved
    verifies as `false`.
11. **Covered-key defaults.** With no `committed` request key: an uncommitted
    message covers all TABM keys except `commitments`/private, in the order of
    the encoded keys; a message with existing commitments covers the same keys
    those commitments cover. An explicit `committed` list is honoured verbatim
    and in order.
12. **`signature`/`signature-input` codec.** Commitments encode to `comm-<lower-
    base64url(sha256(sig))>` members of the `signature` and `signature-input`
    dictionaries (same member name in both); an `id` parameter is emitted **only**
    when the ID is not re-derivable from the signature; decode is the exact inverse
    and `committer` is re-derived from `keyid`.
13. **Codec round-trip.** `from(to(M))` reproduces `M`'s TABM and `commitments`,
    other than what `to` does not send (§4.4); the wire-only fields
    (`signature`, `signature-input`, `content-digest`, a `multipart/*`
    `content-type`, `ao-body-key`) are absent from the decoded TABM.
14. **Multipart determinism.** The multipart boundary is
    `base64url(SHA-256(parts joined by CRLF))`; the same message always yields the
    same boundary and the same encoded bytes.
15. **bundle / tag.** A `bundle`-requested commitment carries `bundle = "true"`,
    is computed over the inlined message, and emits `;bundle="true"`; a signed
    commitment of a message with a private `hashpath` emits `;tag="<hashpath>"`
    and records `tag`.

## 11. Out of scope

- The internal in-memory representation of messages, commitments, keys, and links.
- The byte layout of the TABM itself and of Structured-Fields parsing (see
  `structured@1.0` and assume a conforming RFC 9651 implementation).
- The wallet/keypair file format and key generation.
- Performance, storage strategy, and transport (HTTP/1.1 vs /2 vs /3) specifics
  beyond the field-name casing and percent-encoding pinned above.
- Commitment **selection** (which commitments `id`/`verify`/`committed` operate
  on) and the combined-ID accumulation — those belong to `message@1.0`.

## Open questions

- **SF-dictionary member order is unpinned.** `signature` and `signature-input`
  members are emitted in the underlying map's iteration order, and the
  `@signature-params` covered-component order is the covered-key list order while
  the *parameter* order is byte-sorted. For a single commitment over a fixed
  covered list this is fully determined, but the **relative order of multiple
  commitments** in the `signature`/`signature-input` dictionaries is not specified
  here. It does not affect any ID (each commitment's ID is independent and
  `message@1.0` accumulates them order-independently) or verification (members are
  paired by name), but two implementations MAY emit the dictionary members in
  different orders and thus produce different `signature` header **bytes** for a
  multi-commitment message. If byte-identical multi-commitment headers are
  required, the member order MUST be pinned (e.g. ascending by member name) — flag
  for validation.
- **RSA-PSS salt length / MGF.** The salt length is taken to be the digest length
  (SHA-512) with MGF1-SHA-512 per the platform default; this is not independently
  restated by RFC 9421 and SHOULD be confirmed against a cross-implementation test
  vector, since a mismatch makes signatures unverifiable across stacks.
- **`alg` token for non-default key types.** Only `rsa-pss-sha512` and
  `hmac-sha256` are emitted by this device's own `commit`. EdDSA/secp256k1 keys
  have address derivations defined here, but the exact `alg` token a signed EdDSA
  commitment would carry (and whether `verify` dispatches on it) is not exercised
  by the signed/unsigned aliases and should be pinned if EdDSA signing via this
  device is required.
