# `json-iface@1.0` — JSON bridge between AO messages and legacy compute units

- **Device name:** `json-iface@1.0`
- **Depends-on:** `message@1.0` (message model, commitments, `id`/`committers`, TABM conversion via `structured@1.0`), `json@1.0` (the JSON codec used to serialise/parse the structures). Relates to `process@1.0` (the orchestrator that places this device in a compute stack) and `multipass@1.0` / `stack@1.0` (the pass mechanism it keys off). All `Depends-on` specs are provided to reimplementers.
- **Status:** Draft

## 1. Overview

`json-iface@1.0` is a **translation device** that sits between AO-Core's native
message representation and the **JSON object shape that legacy AO compute units
(CUs) expect** — the "AOS2" / `ao` JSON structure. It performs two distinct,
separable jobs:

1. **Compute-stack bridge.** When placed inside a process's execution stack
   (next to a WASM device), it (a) on the **first pass** serialises the
   scheduled *Message* and the *Process* into JSON strings and stages them as the
   arguments for the legacy `handle` entrypoint, writing them into the execution
   environment; and (b) on the **second pass** reads the legacy handler's JSON
   result back out of the environment and converts it into an AO-Core *results*
   message (an `outbox` of outgoing messages, a `patches` list, and a `data`
   value).

2. **Standalone codec keys.** It exposes two directly-resolvable keys, `to` and
   `from`, that perform the same structure conversions without any WASM
   environment: `to` renders an AO message as the JSON "Message" structure, and
   `from` parses a legacy JSON result into a results message. These let other
   devices (e.g. a relay to an off-node legacy CU) reuse the exact mapping.

The device defines **two JSON schemas** and the mapping between them and AO
messages. This spec pins those schemas (field names, casing, types, defaults)
and the result-mapping rules exactly, because an independent legacy CU on the
other side of the bridge depends on byte-level agreement. The device's internal
representation, and the mechanics of the WASM device it cooperates with, are out
of scope.

## 2. Concepts & terminology

- **Message structure (JSON):** the JSON object describing a single AO message
  as a legacy CU expects to receive it. Its fields are **capitalised**
  (`Id`, `Owner`, `Tags`, …) — see §5.1. Produced by `to` and during first-pass
  staging.
- **Process structure (JSON):** the same per-message structure computed over the
  *process definition* message, wrapped under a top-level `Process` key. Staged
  alongside the message on the first pass.
- **Environment message (JSON):** the message structure with two extra fields
  added for the legacy handler — `Module` and `Block-Height` (§5.1). This is the
  object actually serialised as the message argument on the first pass.
- **Result structure (JSON):** the JSON object a legacy CU/WASM handler returns,
  describing the outcome of an evaluation: `Output`, `Messages`, `patches`,
  optionally `Error` (§5.2). Consumed by `from` and on the second pass.
- **Handler envelope (JSON):** when read out of a WASM environment, the result
  structure is wrapped as `{ "ok": <bool>, "response": <result-structure> }`
  (success) or `{ "ok": false, "error": <value> }` (failure) — see §4.2, §5.3.
- **Pass:** one full sweep of the surrounding execution stack, tracked by the
  message key `pass` (1-based). This device branches on `pass`: `1` = stage,
  `2` = collect. (See `multipass@1.0` for the pass mechanism.)
- **Output prefix:** an optional key `output-prefix` on the base message naming
  the sub-namespace under `results/` where the cooperating WASM device places
  its `type` and `output`, and under which the staged call arguments and read
  hooks live. Defaults to the **empty binary** (so paths are `results/type`,
  `results/output`). Read with case-insensitive message semantics.
- **results message:** the AO-Core message this device produces from a result
  structure: a map with keys `outbox`, `patches`, `data` (§4.2/§5.2). On the
  compute path it is written under the base message's `results` key.
- **Owner / signer address:** the 43-character base64url address of the
  message's primary committer (first signer). "base64url" is used everywhere on
  the wire — never hex.
- **Printable string:** a binary that is valid UTF-8 (decodes without error).
  Used to decide whether `Data` is emitted as a string or as JSON `null` (§5.1).

The device's internal data structures are **out of scope**; only the JSON
schemas, the resolved-key contracts, and the result-mapping rules are normative.

## 3. Device interface

- **Dispatch shape:** **explicit-keys.** The device answers exactly the keys
  `init`, `compute`, `to`, and `from` (§4). It does **not** install a
  default/catch-all handler: any other key falls through to the base
  message device (`message@1.0`) — including the reserved inspection/mutation
  keys (`keys`, `set`, `set-path`, `remove`, `id`, `commitments`, …), which are
  therefore handled by `message@1.0`. An implementation MUST NOT capture those
  keys.

- **Roles.** The device is used in two modes that share the same conversion core:
  - As a **member of an execution stack** (typically a `stack@1.0` between a
    WASI/WASM device and the multipass driver), where the surrounding process
    resolves `init` once and `compute` once per pass.
  - As a **directly-invoked codec**, where a caller resolves `to` or `from`
    against a message it supplies.

- **Base message shape (compute path).** When resolved as a stack member, the
  base message (`M1`) carries:

  | Key | Type | Required | Meaning |
  |---|---|---|---|
  | `pass` | integer | yes (for `compute`) | current stack pass; `1`=stage, `2`=collect |
  | `process` | message | yes | the process definition message |
  | `process/image` | binary (id) | yes | the WASM image/module id → JSON `Module` |
  | `output-prefix` | binary | no (default `<<>>`) | sub-namespace under `results/` (§2) |
  | `results/<prefix>/type` | binary | yes on pass 2 | `ok` or `error` (from the WASM device) |
  | `results/<prefix>/output` | binary | yes on pass 2 | the handler's raw JSON output |
  | a private staging area | — | optional | per-prefix `write`/`read` hooks the WASM device installs (§4.1) |

  The request message (`M2`) on the compute path carries:

  | Key | Type | Required | Meaning |
  |---|---|---|---|
  | `body` | message | yes on pass 1 | the scheduled AO message to evaluate |
  | `block-height` | integer | yes on pass 1 | block height → JSON `Block-Height` |

- **Request shape (codec keys).** `to` reads its subject from the request key
  `message` (defaulting to the base message). `from` reads its subject from the
  request key `json` (defaulting to the base message).

## 4. Resolved keys (normative)

### `init` (Base, Req → result)
- **Reads:** the base message only.
- **Behaviour:** Initialise the device for use in a stack. It MUST set the base
  message's `function` key to the binary `handle` (the legacy entrypoint name)
  and return the resulting message. No other change is made.
- **Returns:** `{ok, BaseMessage'}` where `BaseMessage'` has `function => handle`.
- **Side effects:** none external.

### `compute` (Base, Req → result)
- **Reads:** `pass` from the base message (case-insensitive); then, depending on
  pass, the inputs listed under each sub-behaviour below.
- **Behaviour:** Branch on the integer value of `pass`:
  - `pass == 1` → **stage** (§4.1).
  - `pass == 2` → **collect** (§4.2).
  - any other value → return the base message unchanged: `{ok, BaseMessage}`.
- **Returns / errors / side effects:** as defined by §4.1 / §4.2.

#### 4.1 `compute` on pass 1 — stage the call
- **Reads:**
  - `process` from the base message (resolved with hashpath ignored).
  - `body` from the request message (resolved with hashpath ignored) — the
    scheduled message to evaluate.
  - `process/image` from the base message — the WASM module id.
  - `block-height` from the request message.
  - `output-prefix` from the base message (default `<<>>`).
  - any per-prefix write hook the cooperating WASM device has staged in the base
    message's private area (§9). If absent, the staged arguments are the raw JSON
    strings; if present, each JSON string is handed to the hook and replaced by
    whatever pointer/handle the hook returns.
- **Behaviour (MUST):**
  1. **Denormalise** the scheduled message for legacy compatibility (§5.4): add
     an `id` field (its full message id), and — if it is signed — an `owner`
     field (the primary signer's human-readable address) and a `signature` field
     (the primary commitment's signature).
  2. Build the **message structure** JSON from the denormalised message (§5.1)
     and extend it with `Module => <process image>` and
     `Block-Height => <block-height>`. Serialise to a JSON string.
  3. Build the **process structure** JSON: `{ "Process": <message-structure(process)> }`.
     Serialise to a JSON string.
  4. **Stage** both strings into the execution environment as the legacy call
     arguments. The base message MUST be updated so that `function => handle`
     and `parameters => [ <message-arg>, <process-arg> ]` — in that order
     (message first, process second). When no write hook is present the
     arguments are the JSON strings themselves; when a write hook is present they
     are the hook's returned pointers (message pointer first, process pointer
     second).
- **Returns:** `{ok, BaseMessage'}` with `function`/`parameters` set as above.
- **Side effects:** whatever the write hook performs (e.g. writing the JSON into
  WASM linear memory). With no hook, no external side effect.

#### 4.2 `compute` on pass 2 — collect the result
- **Reads:**
  - `output-prefix` from the base message (default `<<>>`).
  - `results/<prefix>/type` from the base message — the WASM device's status
    (`ok` or `error`, compared case-insensitively).
  - `results/<prefix>/output` from the base message — the handler's raw JSON.
  - `process` from the base message — for outbox post-processing (§5.2).
  - any per-prefix read hook in the private area (§9): if present, the raw
    `output` is passed through it first to obtain the JSON text.
- **Behaviour (MUST):**
  1. If `type` normalises to `error` → return an **error result**: set the base
     message's `outbox` to the unset/undefined sentinel and `results` to
     `{ "body": "WASM execution error." }`, and return it as an error. (See §8.)
  2. If `type` normalises to `ok`:
     a. Read the output (through the read hook if present) to obtain the JSON
        text.
     b. Parse it. It MUST be the **handler envelope** `{ "ok": true,
        "response": <result-structure> }`. Convert `<result-structure>` to a
        results message (the `from`/result mapping of §5.2), then **post-process
        the outbox** (§5.2 step 5): add to **every** outbox message
        `from-process => <process id>` and `from-image => <process image>`.
     c. Set the base message's `results` key to the post-processed results
        message and return `{ok, BaseMessage'}`.
     d. **If parsing throws** (the output is not valid JSON / not the expected
        envelope) → return an error: set `results/outbox` to the unset sentinel
        and `results/body` to `"JSON error parsing result output."` on the base
        message, and return it as an error.
  3. A `type` that is neither `ok` nor `error` is unspecified by this device
     (the cooperating WASM device is expected to produce one of the two).
- **Returns:** `{ok, BaseMessage'}` on success (with `results` populated), or an
  error carrying the base message annotated as in steps 1/2d.
- **Side effects:** whatever the read hook performs. No commitments are created.

### `to` (Base, Req → result) — message → JSON Message structure
- **Reads:** the request key `message` (defaulting to the base message) as the
  subject; the subject's commitments/signers; node options.
- **Behaviour (MUST):** Produce the **message structure** JSON object (§5.1) for
  the subject message, with the `owner-as-address` rule in force (i.e. `Owner`
  is the signer's address; see §5.1). The result is the structure *object*
  (a message/map), **not** a serialised JSON string — serialisation, if needed,
  is the caller's concern (the `json@1.0` codec).
- **Returns:** `{ok, MessageStructure}`.
- **Side effects:** none.

### `from` (Base, Req → result) — JSON result → results message
- **Reads:** the request key `json` (defaulting to the base message) as the
  subject; node options.
- **Behaviour (MUST):** Convert a **result structure** into a results message
  (§5.2). The subject MAY be either a JSON **string** (which MUST first be
  parsed) or an already-parsed **object**. The conversion is the result mapping
  of §5.2; it does **not** apply the outbox post-processing of step 5 (that is
  applied only on the compute path, which has the process in hand).
- **Returns:** `{ok, ResultsMessage}` on a normal result; or, for the failure
  shapes, the error values of §8 (`{error, <Error>}` for an explicit
  `{ "ok": false, "error": … }`; an `invalid-json-message-input` style error for
  an unrecognised shape).
- **Side effects:** none.

## 5. Data formats & encodings (normative)

All keys quoted below are the **exact** JSON object member names, including
case. All identifiers are **base64url** (43 chars for 32-byte values), never hex.

### 5.1 Message structure (AO message → JSON)

For a subject message, emit a JSON object with **exactly** these members
(field name → value rule):

| JSON field | Type | Value |
|---|---|---|
| `Id` | string \| `""` | The message's **default** id — `hb_ao:get(<<"id">>, Msg)` (`hb_message:id/1`, no selector): the content (unsigned) id, **not** the signed id — human-readable (base64url). Empty string if absent. |
| `Anchor` | string | The message's `anchor` value (legacy "last_tx"); `""` if absent. |
| `Owner` | string \| `""` | base64url of the **primary signer's address** (native id of the first committer). `""` if the message is unsigned. |
| `From` | string | The message's `from-process` tag if present; otherwise **`Owner` itself** (the signer's human-readable address — NOT `base64url(Owner)`, which would double-encode). If the value is an id (binary of 32/42/43 bytes), normalised to human-readable id form (an address already is). |
| `Tags` | array | The message's tags as an ordered list (§5.5). |
| `Target` | string | The message's `target` value, human-readable id; `""` if absent. |
| `Data` | string \| `null` | The message's `data` value if it is a printable (valid-UTF-8) string; otherwise JSON `null`. `""`-equivalent empty data is emitted as `""` if printable. |
| `Signature` | string \| `""` | `""` if the message is unsigned (0-byte signature); the base64url of the signature if it is a 512-byte (RSA-2048) signature; otherwise the signature value as-is. |
| `PublicKey` | string \| `""` | The primary commitment's signing public key id, with any `scheme:` prefix removed; `""` if unsigned. |

Field-derivation rules (MUST):

- **Primary signer.** The "primary" committer is the first element of the
  message's signer (committer) list. If the message has no signers, `Owner`,
  `Signature`, and `PublicKey` are all `""`, and `From` defaults from the
  (empty) owner.
- **`Owner` encoding.** With the `owner-as-address` rule (used by both the
  compute staging path and the `to` key), `Owner` is the base64url of the
  signer's **native address** (the account address, 43-char base64url). The
  device MUST NOT place a full public key in `Owner` on these paths.
- **`Id` and `Target` and `From`-when-id** are passed through a *safe id*
  normalisation: an empty value stays `""`; otherwise it is rendered as a
  human-readable id (32-byte → base64url-encoded; an already-human id of
  42/43/44 chars passes through unchanged).
- **`From`.** Take the `from-process` field of the message if present; else use
  **`Owner` itself** (the signer's human-readable address) — NOT `base64url(Owner)`.
  `Owner` is already a base64url address, so re-encoding it would double-encode;
  `From` is exactly the address. (Then, if that value *looks like* an id — a binary
  of length 32/42/43 — normalise to human-readable id form; an address already is,
  so it passes through unchanged.) So a process that set `From-Process` to a process
  id yields that id; an ordinary user message yields the owner address verbatim.
- **`Data`.** The `data` value is emitted **verbatim** as a JSON string iff it is
  a printable UTF-8 binary; if it is not valid UTF-8 (binary blob) it is emitted
  as JSON `null`.
- **`Anchor`/`Target`/`Data`/`from-process`** are read with `message@1.0`
  semantics over the **commitments-stripped** message (the device removes
  `commitments` before reading these scalar fields), each defaulting to `""`
  (or `null`/owner as above) when absent.

The reading of `Anchor`/`Data`/`Target`/`From` MUST be performed against the
message **with its `commitments` removed**, so a commitment sub-map cannot shadow
these fields.

### 5.2 Result structure (JSON → results message)

Given a parsed **result structure** object, produce a results message with keys
`outbox`, `patches`, `data`. Mapping (MUST):

1. **Normalise the result** into `(Data, Messages, Patches)`:
   - If the object has an `Error` member → `Data = <Error value>`, `Messages = []`,
     `Patches = []`. (An explicit error short-circuits the rest.)
   - Otherwise:
     - `Output = <object's "Output" member, or {} if absent>`.
     - `Data = Output["data"]` if present, else the top-level `Data` member, else
       the empty binary `""`. (Note the **lowercase** `data` inside `Output` but
       the **capitalised** `Data` at top level.)
     - `Messages = <object's "Messages" member, or [] if absent>`.
     - `Patches = <object's "patches" member, or [] if absent>` (lowercase
       `patches`).
     - If reading these throws for any reason, fall back to
       `(Data="", Messages=[], Patches=[])`.
2. **`outbox`** is a map keyed by **1-based Erlang INTEGER position** — the keys
   are the integers `1`, `2`, … (NOT the binary strings `<<"1">>`/`<<"2">>` of the
   `structured@1.0` numbered-message convention; this is a plain integer-keyed map,
   `#{1 => Msg1, 2 => Msg2, …}`), in `Messages` input order — mapping to each
   outgoing message, each passed through **outbox-message preprocessing** (step 4).
   The list order of `Messages` is preserved by the ascending integer keys.
3. **`patches`** is the list of each patch passed through **patch
   normalisation** (step 4's tag-folding + unset-conversion, but **without** the
   key-dropping/normalisation that outbox messages get): for each patch, fold its
   `tags` into top-level key/value pairs (§5.5 inverse) and convert any
   `"__ao-unset__"` value to the unset sentinel (§5.6).
4. **Outbox-message preprocessing** (per outgoing message, MUST):
   a. Lower-case-normalise the message's keys; read its `tags` (a list of
      `{name,value}`; §5.5) into a flat key/value map.
   b. Remove the keys `from-process`, `from-image`, `anchor`, and `tags` from the
      message.
   c. Merge the folded tag key/values **over** the remaining (normalised) message
      keys (tags win on conflict).
   d. Convert any `"__ao-unset__"` value (recursively, into nested maps) to the
      unset sentinel (§5.6).
5. **Outbox post-processing (compute path only, MUST).** After step 2–4, for
   **every** message in `outbox`, set `from-process => <process id>` and
   `from-image => <process image>` (the process's id and image, read from the
   process definition message). **`<process id>` is the process's *default* id —
   the value of `hb_ao:get(<<"id">>, Process)` (equivalently `hb_message:id/1`
   with no selector): the content (unsigned) id. It is NOT the *signed* id
   (`hb_message:id(Process, signed, …)`) even when the process is committed, and
   it is NOT re-normalised through any id-coercion helper — stamp it verbatim.**
   The `from`/codec key does **not** perform this step (no process is available);
   the compute path does.
6. **`data`** is the `Data` from step 1.

The produced results message is therefore:
`{ "outbox": { "1": <msg1>, "2": <msg2>, … }, "patches": [ <patch1>, … ],
   "data": <Data> }`.

### 5.3 Handler envelope (WASM environment only)

On the compute collect path the raw `output` read from the environment MUST be
the JSON object:

- **Success:** `{ "ok": true, "response": <result-structure> }` — the device
  converts `<result-structure>` per §5.2.
- Any other parsed shape, or a parse failure, is a JSON/result error (§8). In
  particular `{ "ok": false, "error": <value> }` read directly by the `from`
  key maps to `{error, <value>}` (§8); on the compute path a non-`{ok:true}`
  envelope is treated as a parse error and yields the JSON-error result.

### 5.4 Denormalisation (compute staging only)

Before building the message structure on pass 1, the scheduled message is
**denormalised** (MUST):

- Add `id => <message id>` (the full message id; human-readable base64url).
- If the message has **no** signers, leave owner/signature untouched.
- If the message has signers, add:
  - `owner => <primary signer address, human-readable>` (base64url account
    address), and
  - `signature => <primary commitment's signature value, or "" if none>`.

(The `to` key does not denormalise; it derives `Owner`/`Signature` directly from
the commitments as in §5.1. Denormalisation exists so that an already-evaluated /
relayed message that carries `owner`/`signature` as plain fields surfaces them to
the legacy handler.)

### 5.5 Tags serialisation (`Tags` field)

A message's `Tags` array is produced as follows (MUST):

- **If the message carries an ANS-104 (`ans104@1.0`) commitment with an
  `original-tags` field** → emit those original tags, **in their original order**,
  as the ordered list of `{ "name": <Name>, "value": <Value> }` objects. (This
  preserves duplicate keys and exact original casing/order from a re-encoded
  ANS-104 item.)
- **Otherwise** (no original-tags) → emit one `{ "name": <Header-Case-Name>,
  "value": <Value> }` object **per remaining message field**, where:
  - the fields `id`, `anchor`, `owner`, `data`, `target`, `signature`, and
    `commitments` are **excluded** (they are represented by their own structure
    fields, not as tags);
  - each remaining key is rendered in **HTTP header case**: split the
    normalised (lower-cased) key on `-`, title-case each segment, and re-join
    with `-` (e.g. `block-height` → `Block-Height`, `from-process` →
    `From-Process`, `action` → `Action`);
  - values are emitted as-is (binary → JSON string).
  - The order of non-`original-tags` entries is **unspecified** (derived from
    map iteration); consumers MUST NOT depend on it. Only the `original-tags`
    path guarantees order.

The **inverse** (folding a JSON message's `Tags` back into a flat map, used in
§5.2 step 4a and for patches) is: lower-case the message's keys, take its `tags`
list, and produce a map from each tag's `name` (key) to its `value`. Tag names
are used as-is for the key (callers normalise as needed).

### 5.6 Unset sentinel

The binary string `"__ao-unset__"` appearing as a **value** anywhere in an
outbox message or patch (recursively through nested objects) MUST be converted to
the platform's **unset** sentinel — the value that, when later applied via a
`set`, **removes** that key. This bridges a source language's `nil`/key-deletion
semantics (which a legacy handler encodes as `"__ao-unset__"`) to AO-Core's
key-removal mechanism. Non-string and nested-map values are passed through
(maps recursed into).

### 5.7 Identifier & value encodings (summary)

- All ids (`Id`, `Target`, `From`-when-id, `owner`, process id, image id) are
  **base64url**, never hex. 32-byte native ids are base64url-encoded to 43-char
  form; already-human ids (42/43/44 chars) pass through.
- `Owner` is the **address**, not the public key, on the `to` and compute paths.
- `Signature` is base64url when it is a 512-byte RSA signature; `""` when absent;
  otherwise passed through unchanged (covers non-RSA committers).
- `Data` is a UTF-8 JSON string or JSON `null`.
- Integers (`Block-Height`) are emitted as JSON numbers.

## 6. Ordering, freshness & caching

- **Determinism.** Given the same subject message, `to` and the message-structure
  builder are deterministic **except** for the order of the `Tags` array on the
  non-`original-tags` path (map-iteration order). The `original-tags` path is
  fully ordered. `outbox` numeric keys preserve `Messages` input order.
- The device performs **no result caching of its own**; it is a pure transform
  over the supplied message/request (the compute path's only state is read/write
  through the cooperating WASM environment, which it does not own).
- Result-caching/freshness of resolutions routed through this device (and through
  the surrounding stack) is governed by node/substrate configuration, not by this
  device.

## 7. Security & authority

- The device performs **no authorisation checks** and requires no commitment to
  invoke any of its keys; it is a representation transform. It creates **no
  commitments** and removes none — it never re-signs a message.
- On the staging path it surfaces the scheduled message's **own** signer
  address, signature, and signing-key id into the JSON for the legacy handler;
  these are derived from the message's existing commitments and are not minted by
  this device.
- Private keys of the subject message are excluded from the JSON: the
  message-structure builder operates on a representation with the private area
  reset/removed and `commitments` stripped before scalar reads, so private fields
  do not leak into `Tags`/`Data`/etc.
- **Failure-closed on result errors.** A WASM `type == error`, an unparseable
  result, or an unrecognised result shape yields an **error** outcome (not a
  silently-empty success), so a failed evaluation cannot masquerade as an empty
  successful one.

## 8. Errors

| Condition | Outcome |
|---|---|
| `compute` pass 2, `results/<prefix>/type == error` | Error outcome; base message annotated with `outbox => <unset>` and `results => { "body": "WASM execution error." }`. |
| `compute` pass 2, output parses but is not the `{ ok:true, response:… }` envelope, or is not valid JSON | Error outcome; base message annotated with `results/outbox => <unset>` and `results/body => "JSON error parsing result output."`. |
| `from` / result mapping, subject is `{ "ok": false, "error": <E> }` | `{error, <E>}` — the explicit error value, passed through unchanged. |
| `from` / result mapping, subject is neither a normal result nor the explicit-error shape | The outcome is **`{error, ErrorMap}`** where `ErrorMap` carries the human-readable message under the **`error`** key (NOT `body`): `#{ <<"error">> => <<"Invalid JSON message input.">>, <<"received">> => <Term> }`. (Hyphenated atom form of the condition: `invalid-json-message-input`.) An "unrecognised shape" = a decoded subject that is **not** a map/object (e.g. a JSON array, number, or bare string), OR a map carrying none of `Output`/`Messages`/`patches`/`Error`/`Data`. |
| atom/type decode within a nested conversion | surfaced as the underlying codec's error (see `structured@1.0`). |

Notes:

- The error annotations on the compute path are **not** atoms but a base message
  decorated with the `results.body` text shown above; the *outcome* is an error
  (the stack treats it as failure).
- The device defines no other error atoms of its own.

## 9. Composition

- **In a process execution stack.** This device is designed to sit in a
  `stack@1.0` execution stack together with a WASM device (e.g. a `wasm-64@1.0`)
  and a multipass driver, under a `process@1.0` orchestrator. The canonical
  arrangement runs the stack over **two passes**:
  - **Pass 1:** this device stages the message+process JSON as the WASM call
    arguments (`function => handle`, `parameters => [msg, proc]`); the WASM
    device then executes `handle` and leaves its raw JSON output at
    `results/<output-prefix>/output` with a status at
    `results/<output-prefix>/type`.
  - **Pass 2:** this device reads that output and produces the `results` message
    (`outbox`/`patches`/`data`), post-processed with the process's
    `from-process`/`from-image`.
  The repass between passes is driven by `multipass@1.0`/`stack@1.0`, not by this
  device.
- **Cooperation via the private staging area.** The WASM device MAY install, in
  the base message's private area under the `output-prefix` namespace, a
  **write** hook (`<prefix>/write`) and a **read** hook (`<prefix>/read`). When
  present:
  - the write hook is called with each JSON string and returns a pointer/handle
    that is staged in `parameters` instead of the raw string (message pointer
    first, process pointer second);
  - the read hook is called with the raw `output` and returns the JSON text to
    parse.
  When absent, the JSON strings are staged directly and the raw `output` is
  parsed directly. This is how the JSON is moved into/out of WASM linear memory
  without this device knowing the WASM ABI.
- **`output-prefix` selection.** The base message's `output-prefix` selects the
  `results/<prefix>/…` sub-namespace and the `<prefix>/{write,read}` hook keys,
  so multiple compute layers can coexist. Default is the empty prefix
  (`results/type`, `results/output`).
- **Standalone codec reuse.** A device relaying to an off-node legacy CU resolves
  `to` to obtain the JSON Message structure for an AO message, and `from` to
  convert the CU's JSON result back into an AO results message — reusing the exact
  mapping without a WASM environment.
- **Outgoing message provenance.** Because pass-2 post-processing stamps every
  `outbox` message with `from-process`/`from-image`, downstream targets can tell
  the message was produced by a process (and which one). Spawns produced by a
  legacy handler are conveyed as ordinary entries of `Messages` (a spawn is a
  message whose tags mark it a process); this device does **not** read a separate
  `Spawns` or `Assignments` field (see §11 and Open questions).

## 10. Conformance (normative checklist)

An implementation MUST exhibit all of the following, each checkable by resolving
a key against a constructed message (or by parsing a known JSON input):

1. **Dispatch.** Only `init`, `compute`, `to`, `from` are answered by this
   device; `keys`/`set`/`set-path`/`remove`/`id`/`commitments` on a
   `json-iface@1.0` message produce exactly the `message@1.0` result (not
   captured by this device).
2. **`init`.** Resolving `init` returns the base message with `function` set to
   the binary `handle`.
3. **`compute` pass routing.** With `pass == 1` the device stages a call; with
   `pass == 2` it collects results; with any other `pass` value it returns the
   base message unchanged.
4. **Pass-1 staging.** After pass 1, the base message has `function => handle`
   and `parameters` is a 2-element list with the **message** argument first and
   the **process** argument second. With no write hook, those arguments are the
   JSON strings; the message JSON is the message structure (§5.1) extended with
   `Module` (= `process/image`) and `Block-Height` (= request `block-height`),
   and the process JSON is `{ "Process": <message-structure(process)> }`.
5. **Message-structure fields.** `to` (and the staging builder) emit a JSON
   object whose members are exactly `Id`, `Anchor`, `Owner`, `From`, `Tags`,
   `Target`, `Data`, `Signature`, `PublicKey` (plus `Module`/`Block-Height` only
   on the staged message), with the casing shown. No other top-level members are
   emitted.
6. **Owner/Signature/PublicKey for a signed message.** For a signed subject:
   `Owner` is the signer's 43-char base64url **address**; `Signature` is the
   base64url of the (512-byte RSA) signature; `PublicKey` is the signing key id
   with any `scheme:` prefix removed. For an unsigned subject all three are `""`.
7. **`Data` rule.** A printable-UTF-8 `data` is emitted as a JSON string;
   non-UTF-8 (binary) `data` is emitted as JSON `null`.
8. **`From` rule.** `From` is the message's `from-process` if present, else
   **`Owner` itself** (the owner address verbatim — NOT `base64url(Owner)`, which
   double-encodes); an id-shaped result is normalised to human-readable id form
   (the address already is).
9. **`Tags` rule.** With an ANS-104 `original-tags` present, `Tags` is the
   original ordered `{name,value}` list (order preserved). Otherwise `Tags` is
   one header-cased `{name,value}` per remaining field, **excluding** `id`,
   `anchor`, `owner`, `data`, `target`, `signature`, `commitments`; keys are
   rendered in `Header-Case` (e.g. `block-height` → `Block-Height`).
10. **Result mapping (`from`).** A result structure with `Output`/`Messages`/
    `patches` maps to `{ outbox, patches, data }`: `outbox` is keyed by 1-based
    integer position over `Messages` (input order preserved); `data` is
    `Output.data` (lowercase) if present else top-level `Data` (capitalised) else
    `""`; `patches` is the patch list with tags folded and unset converted.
11. **Outbox preprocessing.** Each outbox message has `from-process`,
    `from-image`, `anchor`, `tags` removed; its `tags` folded into top-level
    key/values (overriding remaining keys); `"__ao-unset__"` values converted to
    the unset sentinel.
12. **Outbox post-processing (compute path).** After a successful pass-2 collect,
    every `outbox` message carries `from-process => <process id>` (the process's
    *default*/content id, **not** the signed id — §5.2 step 5) and
    `from-image => <process image>`. The `from` codec key does **not** add these.
13. **Handler envelope.** On pass-2 collect with `type == ok`, the output is
    parsed as `{ ok:true, response:R }` and `R` is mapped per §5.2; a parse
    failure or non-`{ok:true}` envelope yields the JSON-error result
    (`results/body => "JSON error parsing result output."`), and `type == error`
    yields the WASM-execution-error result
    (`results => { body: "WASM execution error." }`).
14. **Explicit error result.** `from` of `{ "ok": false, "error": E }` returns
    `{error, E}`; `from` of an unrecognised shape returns an error carrying an
    `invalid-json-message-input`-style message and the received value.
15. **Unset conversion.** A `"__ao-unset__"` value (including nested) in a patch
    or outbox message becomes the unset sentinel; non-string values are
    untouched.
16. **Encodings.** Every id/address/signature emitted is base64url (never hex);
    `Module`/process/image ids are base64url; `Block-Height` is a JSON number.
17. **Commitments stripped for scalar reads.** `Anchor`/`Data`/`Target`/`From`
    are read from the subject with `commitments` removed, so a commitment sub-map
    cannot shadow them.

## 11. Out of scope

- The **internal representation** of messages, the results message, links, and
  the device's own state.
- The **WASM device's ABI** and the exact mechanics of the write/read hooks
  (how JSON is placed into/read from linear memory): this spec only fixes that
  the hooks, when present, replace raw strings with pointers (write) and raw
  output with text (read), and the staging order (message first, process second).
- The **`json@1.0` codec** byte format (assume a conforming JSON
  encoder/decoder is available) and the **`structured@1.0`** TABM byte layout.
- The cryptography of commitments / the `httpsig@1.0` and `ans104@1.0`
  commitment devices (only their observable `original-tags`/`signature`/
  `committer`/`keyid` surfaces are referenced).
- The surrounding **`process@1.0`** orchestration, scheduling, the `pass`
  increment mechanism (`stack@1.0`/`multipass@1.0`), and result caching/freshness
  policy.
- Performance and storage strategy.
- The legacy CU's own internal computation that produces the result structure.

## Open questions

- **`Spawns` and `Assignments` are not consumed.** A legacy CU result nominally
  may carry `Output`, `Messages`, `Spawns`, and `Assignments`. This device reads
  only `Output` (→ `data`), `Messages` (→ `outbox`), `patches` (→ `patches`), and
  `Error`. Spawns are expected to arrive folded into `Messages` (a spawn is a
  message marked as a process), and `Assignments` are not handled at all. A spec
  consumer relying on separate `Spawns`/`Assignments` arrays would diverge —
  confirm whether the legacy contract truly folds these into `Messages` or
  whether this device intentionally drops them.
- **Non-`original-tags` `Tags` order is unspecified.** When a message has no
  ANS-104 `original-tags`, the `Tags` array order is map-iteration order. If a
  legacy handler is order-sensitive on tags, this is a latent incompatibility;
  consider mandating a sorted order on that path.
- **`From` default uses base64url-of-owner-address.** The default `From` is the
  base64url encoding of the already-base64url owner address (i.e. an extra
  encoding hop), then id-normalised. Confirm the intended exact byte value of
  `From` for an ordinary signed user message (it should be the owner address) —
  the double-encode-then-normalise path is subtle and worth pinning against the
  legacy expectation.
- **`Signature` for non-512-byte committers.** A non-RSA (e.g. ECDSA/Ethereum)
  signature is passed through **as-is** rather than base64url-encoded. Confirm
  the legacy handler's expectation for such signatures, or mandate base64url for
  all signature byte-strings.
- **`type` values other than `ok`/`error`.** The collect path only defines `ok`
  and `error`; any other `type` value's behaviour is unspecified. Confirm the
  WASM device contract guarantees one of the two.
