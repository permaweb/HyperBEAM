# `delegated-compute@1.0` — delegate a process's compute step to a remote/legacy compute unit

- **Device name:** `delegated-compute@1.0`
- **Depends-on:** `message@1.0` (message model, `id`/`committers`, `set`, identity-key semantics), `process@1.0` (the orchestrator that installs this as a process's execution device and supplies the slot/assignment request), `relay@1.0` (the outbound HTTP mechanism that carries every remote call), `json-iface@1.0` (the AOS2/legacy JSON result mapping used to decode responses). All `Depends-on` specs are provided to reimplementers. **Relates to** `genesis-wasm@1.0` (a thin wrapper that delegates its execution-device keys to this device).
- **Status:** Draft

## 1. Overview

`delegated-compute@1.0` is an **execution device** for an AO process whose
compute step is performed **off-node** by a remote, "legacy"-style AO compute
unit (CU) rather than locally. It implements the process **execution-device
interface** (`init`, `compute`, `normalize`, `snapshot`) so that
`process@1.0` can install it as a process's `execution-device` and drive it once
per scheduled slot. On each compute, the device serialises the slot's assignment
(or a dry-run message) into the legacy CU's JSON shape, **relays** it over HTTP to
the configured CU endpoint, decodes the CU's JSON result back into an AO results
message via `json-iface@1.0`, and merges that result into the process state under
the `results` namespace.

It is the bridge that lets an existing legacy AO process definition execute on
HyperBEAM infrastructure: HyperBEAM schedules and orders the messages; the remote
CU runs the actual WASM/Lua handler and holds the process's internal state. The
device can also be used standalone to pull trusted results from a remote CU into
the local node.

The device performs **no local execution** of the process and keeps **no process
state of its own** — the remote CU is authoritative for state; HyperBEAM holds
only the per-slot result it returns and the snapshots the CU produces on request.

## 2. Concepts & terminology

- **Remote compute unit (CU):** the external HTTP service that actually evaluates
  the process. It exposes the legacy AO CU endpoints this device calls
  (`/result/<slot>`, `/dry-run`, `/state`, `/snapshot/<id>`). The CU holds the
  process's internal state across slots; HyperBEAM does not.
- **Relay:** the substrate's outbound HTTP mechanism (`relay@1.0`). This device
  never opens sockets itself; every remote call is expressed as a `relay@1.0`
  `call` whose destination is selected by the node's routing table (or an explicit
  peer) and whose method/path/body are set by this device. The set of endpoints a
  process's CU lives behind is therefore **node routing configuration**, not a
  field of this device — see §6, §7, and Open questions.
- **Assignment:** a scheduled message for a specific **slot** of the process,
  carried in the request as `type == "Assignment"`. Computing an assignment
  advances the process by one slot and the result is the canonical state at that
  slot.
- **Dry-run:** a speculative evaluation that does **not** advance the process —
  any request whose `type` is not `"Assignment"`. The result is computed against
  current state but not committed to a slot.
- **AOS2 / legacy JSON shape:** the JSON object format a legacy CU expects on the
  wire (the `ao`/AOS2 structure). Requests are encoded into it; responses are
  decoded out of it via `json-iface@1.0` (which pins the exact field schema).
- **Output prefix:** an optional binary key `output-prefix` on the base process
  message naming the sub-namespace under which this device writes the decoded
  result. Defaults to the **empty binary** (so the result lands at `results`,
  `results/raw`). See §4.`compute`.
- **Snapshot / checkpoint:** an opaque blob of the CU's internal state, produced
  by the CU on request (`snapshot`) and pushed back to the CU to restore state
  (`normalize` with a `Checkpoint` snapshot). HyperBEAM treats the blob as opaque.
- **Process id:** the 43-character base64url id of the process being computed,
  used to address the correct process on the remote CU.

## 3. Device interface

- **Dispatch shape:** **explicit-keys.** The device answers exactly the four
  execution-device keys `init`, `compute`, `normalize`, `snapshot` (§4). It
  installs **no** default/catch-all handler and exports no additional keys; any
  other key — including the reserved inspection/mutation keys
  (`keys`, `set`, `set-path`, `remove`) and the commitment keys
  (`id`, `commitments`, `committers`, `verify`) — is **not** captured by this
  device and resolves under the base identity device (`message@1.0`) for the
  message it is bound to. There is nothing to exclude (there is no default
  handler).

- **Role.** The device is installed as a process's **execution device** (the
  process message's `execution-device` is `delegated-compute@1.0`, or a wrapper
  such as `genesis-wasm@1.0` that forwards to it). `process@1.0` resolves
  `init` once, then `compute` once per scheduled slot, swapping this device in for
  the execution step and restoring the process device afterwards. `normalize`
  and `snapshot` are invoked by the orchestrator around state load/checkpoint.

- **Base message shape (`Base` / `M1`).** The base is the process state message.
  This device reads from it:

  | Key | Type | Required | Meaning |
  |---|---|---|---|
  | `process` | message | for compute/snapshot/normalize | the **signed** process definition; its id identifies the CU process (§5.1) |
  | `output-prefix` | binary | no (default `<<>>`) | sub-namespace under which the decoded result is written (read with `message@1.0` semantics) |
  | `snapshot` | message | only on `normalize` of a restored state | a CU snapshot to load back into the remote CU (§4.`normalize`) |

- **Request message shape (`Req` / `M2`) — `compute`.**

  | Key | Type | Required | Meaning |
  |---|---|---|---|
  | `type` | binary | no | `"Assignment"` ⇒ compute a slot; any other / absent value ⇒ dry-run |
  | `slot` | integer | yes when `type == "Assignment"` | the slot number being computed |
  | `process-id` | binary (base64url id) | only if the base has no derivable process id | fallback process id (§5.1) |
  | `commitments` | map | no | the request's **own** commitments (e.g. on a signed request). This device does **NOT** use it as a process-id selector — it derives the id under the default `signed` selector, passing an empty selector source (§5.1). |
  | (the assignment's own message fields) | — | — | the scheduled message itself, encoded into the request body |

## 4. Resolved keys (normative)

### `init` (Base, Req → result)
- **Reads:** the base message only.
- **Behaviour:** No-op initialisation. The device MUST return the base message
  unchanged. (The remote CU holds all state; there is nothing to initialise
  locally.)
- **Returns:** `{ok, Base}`.
- **Side effects:** none.

### `compute` (Base, Req → result)

Compute the result of one process step by relaying it to the remote CU and
merging the decoded result into the process state.

- **Reads:**
  - `output-prefix` from the base, read with `message@1.0` (identity-device)
    semantics; default **empty binary** `<<>>`.
  - the **process id** (§5.1), derived from the **base message itself** (its own
    committed id — preferred) or the request's `process-id` (the narrow fallback).
  - `type` from the request (default treated as "not an assignment" ⇒ dry-run).
  - on an assignment: `slot` from the request, and the full request message (the
    assignment) which is encoded into the request body (§5.2).
  - on a dry-run: the request message with its `commitments` removed, rendered to
    the legacy JSON Message shape via `json-iface@1.0`’s `to` mapping (§5.3).
- **Behaviour (MUST):**
  1. Determine the **process id** per §5.1. If it cannot be derived from the base
     and the request has no `process-id`, the operation fails (see §8).
  2. Branch on `type` (read as a plain request field and compared against the
     **exact binary** `<<"Assignment">>` — the comparison is **case-sensitive**, and
     the platform key-normaliser does **not** case-fold *values*, so a lowercased
     compare would silently misroute every assignment down the dry-run path):
     - **`type == "Assignment"`** ⇒ set the working slot to `slot` and perform an
       **assignment relay** (§5.2): POST the slot's assignment, in AOS2 JSON form,
       to the CU's `/result/<slot>?process-id=<process-id>` endpoint.
     - **otherwise** ⇒ set the working slot to the dry-run marker and perform a
       **dry-run relay** (§5.3): POST the JSON Message form of the (commitments-
       stripped) request to the CU's `/dry-run?process-id=<process-id>` endpoint.
  3. From the relay's response, extract the response **`body`** as the CU's raw
     JSON result string (§5.4). A relay error (transport, routing, or an
     `error`/`failure`-tagged result) MUST propagate as an error (§8) — the device
     MUST NOT fabricate a result.
  4. Decode the raw JSON result into an AO **results message** using
     `json-iface@1.0`’s `from` mapping (the AOS2 result → `{outbox, patches,
     data}` conversion). Also retain the **raw decoded JSON** value (the parsed
     JSON term, before the results mapping).
  5. **Merge** both into the process state by `set`ing, on the base message:
     - `<output-prefix>/results` ⇒ the decoded results message, and
     - `<output-prefix>/results/raw` ⇒ the raw decoded JSON term.

     The keys are formed by concatenating the prefix and the suffix
     (`<<prefix/binary, "/results">>`), then **resolved as AO-Core paths via `set`**:
     a non-empty prefix `P` yields `P/results` and `P/results/raw`, while the
     **default empty prefix yields a literal leading-slash `/results`** that the path
     layer normalises (trims) to `results` / `results/raw`. An implementation MUST
     go through the AO-Core path/`set` layer (a raw map insertion of the
     leading-slash key would not match a later `results` read), and MUST write
     `results` **before** `results/raw` so the deep-set does not clobber the parent.
  6. Return the updated base message.
- **Returns:** `{ok, Base'}` with the result merged under the output-prefix; or an
  error (§8) on a relay/decoder failure.
- **Side effects:** **one outbound HTTP request** to the remote CU (via the relay)
  — the device's defining side effect. No cache or store write of its own; the
  result is returned in the message for the orchestrator to persist.

Note on outbox provenance: this device's `compute` itself does **not** stamp
`from-process`/`from-image` onto outbox messages — that stamping is part of the
`json-iface@1.0` **compute** path, not its `from` codec path, which is what this
device uses. A wrapper such as `genesis-wasm@1.0` is responsible for any
subsequent outbox post-processing (e.g. applying `patch@1.0` over
`/results/outbox`). See §9.

### `normalize` (Base, Req → result)

Restore a previously-checkpointed CU state, then return the process message
without its transient `snapshot` key.

- **Reads:** the base's `snapshot` key (if any); within it, `type`, `data`, and
  the remaining snapshot header keys. **Read `snapshot` as plain data** (a raw map
  read / the inert `message@1.0` view), **NOT** via device resolution — `snapshot`
  is also one of *this* device's keys, so `hb_ao:get(<<"snapshot">>, Base)`
  re-dispatches into `snapshot`/3 and would relay to the CU instead of reading the
  stored field.
- **Behaviour:**
  1. If the base has **no** `snapshot` key ⇒ return `{ok, Base}` unchanged.
  2. If a `snapshot` is present, remove it from the message to form the
     normalised base.
  3. If the snapshot's `type` equals the **exact binary** `<<"Checkpoint">>` —
     a **case-sensitive** compare against the plain field value (the platform
     key-normaliser does **not** case-fold *values*, so a lowercased/normalised
     compare silently skips the load) ⇒
     **load the state into the remote CU** (§5.5): POST the snapshot's `data` as
     the body, and the snapshot's remaining keys (all keys except `data`) as
     headers, to the CU's `/state` endpoint. The load result is observed but the
     return value of `normalize` is the **normalised base** regardless.
  4. If `type` is anything other than `"Checkpoint"` ⇒ do **not** call the CU;
     just return the normalised base.
- **Returns:** the **normalised base message** (the base with `snapshot` removed).
  Per the interface convention, an implementation MAY return it bare (`Base'`) or
  wrapped (`{ok, Base'}`); the no-snapshot case returns `{ok, Base}`.
- **Side effects:** when a `Checkpoint` snapshot is present, **one outbound HTTP
  request** (`POST /state`) to the remote CU to restore its internal state.
  Otherwise none.

### `snapshot` (Base, Req → result)

Ask the remote CU to checkpoint the running computation and return the produced
checkpoint.

- **Reads:** the **process id** (§5.1) from the base.
- **Behaviour:**
  1. Derive the process id (§5.1).
  2. **Request a snapshot from the CU** (§5.6): relay a `POST` to the CU's
     `/snapshot/<process-id>` endpoint with content-type `application/json` and an
     empty JSON object body (`{}`).
  3. On a successful relay response `R` ⇒ return `{ok, R}` (the CU's checkpoint
     response message, verbatim).
  4. On a relay error `E` ⇒ return a **success-shaped** message reporting the
     absence of a checkpoint: `{ok, #{ "error" => "No checkpoint produced.",
     "error-details" => E }}`. (`snapshot` does **not** surface a relay failure as
     an `{error, _}` outcome; it reports it inside an `ok` message so a missing
     checkpoint does not abort the surrounding flow.)
- **Returns:** `{ok, CheckpointResponse}` on success, or
  `{ok, #{ "error" => "No checkpoint produced.", "error-details" => <E> }}` on a
  relay failure.
- **Side effects:** **one outbound HTTP request** (`POST /snapshot/<id>`) to the
  remote CU.

## 5. Data formats & encodings (normative)

All keys are binary, lowercase, hyphenated on the wire. All ids/addresses are
**base64url** (43 chars for 32-byte values), **never hex**.

### 5.1 Process-id derivation

The process id addresses the correct process on the remote CU and is derived as
follows (MUST):

1. Call the platform's **shared process-id derivation** on the base (the same one
   other process-execution devices use), with an **empty selector source** so the
   committer-selector defaults to `signed` (the id over the process's signed
   commitments). **Do not branch on the base's shape yourself** — the helper reads
   the base's **`process` self-key** if present (the normal case when driven by
   `process@1.0`), otherwise promotes the base itself to the process, then computes
   that process's committed id. **Do not pass the request as the selector source**:
   a signed request's own `commitments` is a map, not a selector, and passing it
   faults.
   - The base **MUST be verifiable as a process**: its commitments MUST verify and
     it MUST have **at least one signer**. A base that does not verify, or that has
     **no signers**, is a hard failure (§8) — the device MUST NOT relay an
     unverified/unsigned process to the CU. The shared derivation **raises/throws**
     in these cases; it does **not** return a sentinel.
2. **Fallback to the request's `process-id`** applies only when the shared
   derivation yields a genuine *no-process* sentinel (`not_found`) — i.e. there is
   no derivable process identity on the base at all. In practice the derivation
   returns an id or raises, so this is a narrow path: an implementation MUST gate on
   the actual `not_found` sentinel, **NOT** on the mere presence/absence of a
   `process` sub-key (gating on a sub-key wrongly skips derivation for a normal
   signed base, e.g. on `snapshot`, and never reaches the CU). If neither a derived
   id nor a request `process-id` is available, the operation fails (§8).

The derived id is used **human-readable** (base64url) in the relay paths and as
the `process-id` query parameter.

### 5.2 Assignment relay (compute on `type == "Assignment"`)

- **Endpoint:** `POST /result/<slot>?process-id=<process-id>`, where `<slot>` is
  the decimal slot number and `<process-id>` is the human-readable base64url
  process id.
- **Body:** the assignment, rendered into the **AOS2 assignments JSON** structure
  for this process — the legacy CU's "result" request shape. Concretely it is a
  JSON object carrying `page_info` (process id, paging flags, timestamp,
  block-height, block-hash) and an `edges` array whose single `node` is the
  assignment rendered in AOS2 form (one edge per assignment). The exact AOS2
  assignment node schema is the scheduler/format contract and is **out of scope**
  here (a reimplementer reuses the substrate's assignment-to-AOS2 rendering); what
  is normative for *this* device is the endpoint, method, the single-assignment
  `{ slot ⇒ assignment }` input, and that the body is JSON.
- **Content-type:** `application/json`.
- **Relay options:** the relay MUST be issued with **hashpath ignored** and
  **`cache-control: [no-store, no-cache]`** so the call is never served from or
  written to a result cache (the CU's state is authoritative and changes per
  slot).

### 5.3 Dry-run relay (compute on non-assignment `type`)

- **Endpoint:** `POST /dry-run?process-id=<process-id>`.
- **Body:** the request message with **only its `commitments` removed** (this
  device strips nothing else), rendered to the legacy **JSON Message structure** via
  `json-iface@1.0`’s `to` mapping, then serialised to a JSON string. Which request
  keys become Message fields vs. `Tags` — and how envelope keys such as
  `path`/`method`/`type` are treated — is `json-iface@1.0`’s `to` contract, not this
  device's (see `json-iface@1.0` §5.1 for the schema — `Id`, `Owner`, `Tags`,
  `Data`, … with the field casing pinned there).
- **Content-type:** `application/json`.
- **Relay options:** hashpath ignored; `cache-control: [no-store, no-cache]`.

### 5.4 Response extraction & result decoding

- **Extraction.** From the relay response:
  - A success response is the relay's `{ok, ResponseMessage}`; the CU's raw JSON
    result is the response message's **`body`** value (a JSON string). The
    remaining response metadata is not used.
  - A relay result tagged **`error`** or **`failure`** is extracted as
    `{error, <Reason>}` and propagated (§8).
- **Decoding.** The raw JSON result string is converted into the AO **results
  message** (`{outbox, patches, data}`) using `json-iface@1.0`’s **`from`**
  mapping (AOS2 result → results message; the device does not re-implement that
  schema). In parallel the raw JSON is parsed into its plain term and kept as the
  `results/raw` value.

### 5.5 Snapshot-load body (`normalize` of a `Checkpoint`)

- **Endpoint:** `POST /state`.
- **Body:** the snapshot's **`data`** value (the opaque checkpoint blob).
- **Headers:** the snapshot's remaining keys (every key **except** `data`),
  forwarded as request headers; the content-type defaults to `application/json`
  unless a `content-type` header is present among them.
- **Relay options:** hashpath ignored; `cache-control: [no-store, no-cache]`.
- The device treats the blob as **opaque**; its internal structure is the CU's
  concern.

### 5.6 Snapshot-request body (`snapshot`)

- **Endpoint:** `POST /snapshot/<process-id>`.
- **Body:** the literal JSON empty object `{}`.
- **Content-type:** `application/json`.
- **Relay options:** hashpath ignored; `cache-control: [no-store, no-cache]`.

### 5.7 Endpoint summary

| Key | Method | Path | Body |
|---|---|---|---|
| `compute` (assignment) | POST | `/result/<slot>?process-id=<id>` | AOS2 assignments JSON (§5.2) |
| `compute` (dry-run) | POST | `/dry-run?process-id=<id>` | JSON Message structure (§5.3) |
| `normalize` (Checkpoint) | POST | `/state` | snapshot `data` blob, snapshot headers (§5.5) |
| `snapshot` | POST | `/snapshot/<id>` | `{}` (§5.6) |

All four are dispatched through `relay@1.0`’s `call`; the **destination host** for
these paths is chosen by the node's routing table (or an explicit relay peer), not
by this device.

## 6. Ordering, freshness & caching

- **Not a pure function.** Every key except `init` performs a network request, so
  results depend on the remote CU's state at call time. Re-resolving re-issues the
  request.
- **Result-cache bypass.** Every relay this device issues sets
  `cache-control: [no-store, no-cache]` and ignores the hashpath, so a node's
  HTTP result cache MUST NOT serve or store these calls. A correct implementation
  MUST carry these directives on each relay (assignment, dry-run, state-load,
  snapshot) so stale slot results are never returned.
- **Determinism / ordering.** Slot ordering and the at-most-once advancement of
  the process are the responsibility of `process@1.0`/the scheduler, which calls
  `compute` once per slot in order. This device computes whatever slot the request
  names; it does not itself enforce monotonicity or deduplicate slots (a wrapper
  such as `genesis-wasm@1.0` applies dedup around it). The decoded result's
  `outbox` order follows `json-iface@1.0`’s mapping (1-based numeric keys
  preserving `Messages` order).
- **Authoritative state lives remotely.** Because the CU holds process state, two
  HyperBEAM nodes delegating the same process to the **same** CU observe a shared
  state; this device keeps no local copy beyond the per-slot result it returns.

## 7. Security & authority

- **Determinism / trust caveat (important).** The result of a delegated compute is
  **only as trustworthy as the remote CU and the channel to it.** HyperBEAM does
  not re-execute the step and cannot independently verify that the returned
  `results`/`outbox` are the correct output of the assignment — it accepts the
  CU's JSON at face value. Using this device therefore **moves the process's
  execution trust boundary off-node** to whoever operates the configured CU. This
  is acceptable when the CU is trusted (the "bring trusted results into the local
  node" use), but a reimplementer MUST NOT treat a delegated result as
  independently validated the way a locally-replayed `genesis-wasm`/WASM execution
  would be. Snapshots restored via `normalize` are similarly trusted blobs.
- **Process must be signed.** The process id is taken over the process's **signed**
  commitments (default selector `"signed"`), and the process definition MUST
  verify and MUST have ≥1 signer; an unverifiable or unsigned process is rejected
  before any relay (§5.1, §8). This binds each delegated computation to a
  cryptographically identified process.
- **No re-signing here.** This device does not commit (sign) anything itself. Any
  re-signing of the relayed request is governed by `relay@1.0`’s policy gate
  (`relay-allow-commit-request`) and is out of scope for this device.
- **Network capability.** Each non-`init` key causes the node to make an outbound
  HTTP request to the CU. Operators MUST scope the reachable CU endpoints via the
  routing table; exposing this device lets callers cause the node to relay to the
  configured CU.
- **Failure posture.** `compute` and `normalize` are **failure-propagating**
  (errors abort, no fabricated result — §8). `snapshot` is **failure-soft**: a
  relay failure is reported inside an `ok` message (`"No checkpoint produced."`)
  rather than as an error, so an absent checkpoint does not abort the caller.

## 8. Errors

| Condition | Outcome |
|---|---|
| `compute`/`snapshot` (NOT `normalize` — its `/state` load derives no id) : process definition present but **does not verify** | hard failure (raised/thrown). The shared derivation throws the **underscored** atom **`process_not_verified`** (often `{process_not_verified, <Process>}`), per the platform thrown-atom convention — *not* a hyphenated wire form. No relay is issued. |
| `compute`/`snapshot`: process definition verifies but has **no signers** | hard failure (raised/thrown), the **underscored** atom **`process_has_no_signers`**. No relay is issued. |
| `compute`: the derivation returns the `not_found` sentinel (no process identity on the base) **and** the request has no `process-id` | failure deriving the process id (a missing-key failure on `process-id`); no relay is issued. (Note §5.1: a normal signed base derives or throws — it does not reach this row.) |
| `compute`: the relay returns a transport/routing error, or a result whose status-tag is `error`/`failure` | `{error, <Reason>}` — the underlying relay/CU error is **propagated unchanged** (no result merged). The relay tags its result by HTTP status (via `relay@1.0`'s status→atom mapping: 2xx → `ok`/`created`, 4xx → `error`, ≥5xx → `failure`); this device treats `error`/`failure` as the propagated `{error, _}` and any 2xx tag as success. "Propagated unchanged" and "extracted as `{error, _}`" (§5.4) describe the **same** outcome — the relay's own error term is returned as-is, the device adds nothing. |
| `compute`: the CU's JSON result is **unparseable** | the error surfaced by `json-iface@1.0`’s `from` mapping (an `invalid-json-message-input`-style error) — propagated. **NOTE:** a CU body with an explicit `{ ok:false, error:E }` shape is **NOT** turned into an error by `from` — its generic object-mapping path yields a (possibly empty) results message that the device merges as a **success**. This device does not special-case `ok:false`; surfacing such a soft error is the caller's/wrapper's concern. |
| `snapshot`: the relay fails for any reason | **not** an error — returns `{ok, #{ "error" => "No checkpoint produced.", "error-details" => <E> }}`. |

The device defines no error atom of its own for the relay/decoder failures: it
propagates whatever `relay@1.0` or `json-iface@1.0` produced. The
`process-not-verified` / `process-has-no-signers` failures originate in the
process-id derivation shared with other process-execution devices.

## 9. Composition

- **As a process execution device.** Installed as a process's `execution-device`,
  this device is driven by `process@1.0`: `init` once, then `compute` once per
  scheduled slot (with `normalize`/`snapshot` around state load/checkpoint). The
  process orchestrator swaps this device in for the execution step (input prefix
  `process`, the output-prefix selecting the `results` sub-namespace) and restores
  the process device afterwards.
- **Under `genesis-wasm@1.0`.** `genesis-wasm@1.0` is a thin wrapper that forwards
  its own `compute`/`normalize`/`snapshot` to this device (`{as,
  "delegated-compute@1.0", …}`), adding around it: a CU-availability check
  (failing with a 500 "server not running" message if the CU is not up), `dedup@1.0`
  to suppress duplicate slot computations, and `patch@1.0` over `/results/outbox`
  to apply any state patches the result carries. A reimplementer of *this* device
  produces only the per-slot decoded `results`/`results/raw`; the dedup/patch/
  availability orchestration belongs to the wrapper, not here.
- **Standalone trusted-result import.** Resolved directly (not inside a process
  stack), `compute` against a trusted CU pulls that CU's result for a slot/dry-run
  into the local node — the "standalone, to bring trusted results into the local
  node" use named in the overview.
- **Result namespace.** The decoded result is always written under
  `<output-prefix>/results` (+ `/results/raw`), so a downstream device reads
  `results/outbox`, `results/data`, `results/patches` from the returned state
  exactly as it would for a locally-executed process — the delegation is
  transparent to consumers of the `results` namespace.

## 10. Conformance (normative checklist)

An implementation MUST exhibit all of the following:

1. **Dispatch.** The device answers exactly `init`, `compute`, `normalize`,
   `snapshot`; it installs no default handler, so `keys`/`set`/`set-path`/
   `remove`/`id`/`commitments` on a `delegated-compute@1.0` message resolve under
   `message@1.0` (not captured by this device).
2. **`init`.** Resolving `init` returns the base message unchanged and performs no
   network call.
3. **Process id.** The process id is the process's **committed** id under the
   **default `signed` selector** — derived via the shared helper with an *empty*
   selector source; the device does **not** use the request's `commitments` as a
   selector. A present process definition MUST verify and MUST have ≥1 signer, else
   the operation throws the underscored `process_not_verified` /
   `process_has_no_signers` and **no relay is issued**. The derivation never returns
   a no-process sentinel for a normal base, so the request-`process-id` fallback is a
   narrow/dead path; and only `compute` and `snapshot` derive the id — `normalize`
   (the id-less `/state` load) does not.
4. **Assignment compute.** With request `type == "Assignment"`, `compute` relays
   `POST /result/<slot>?process-id=<id>` with the assignment rendered as AOS2
   assignments JSON (content-type `application/json`).
5. **Dry-run compute.** With any other / absent `type`, `compute` relays
   `POST /dry-run?process-id=<id>` with the **commitments-stripped** request
   rendered to the `json-iface@1.0` JSON Message structure and JSON-serialised.
6. **Cache bypass.** Every relay issued by this device (assignment, dry-run,
   state-load, snapshot) carries `cache-control: [no-store, no-cache]` and ignores
   the hashpath.
7. **Result extraction.** The CU's raw JSON result is taken from the relay
   response's `body`; a relay result tagged `error`/`failure` is propagated as
   `{error, _}` and no result is merged.
8. **Result mapping.** The raw JSON result is converted to a results message via
   `json-iface@1.0`’s `from` mapping, and both the results message and the raw
   parsed JSON are merged into the base as `<output-prefix>/results` and
   `<output-prefix>/results/raw` (default prefix ⇒ `results`, `results/raw`).
9. **`normalize` no-snapshot.** With no `snapshot` key on the base, `normalize`
   returns the base unchanged and issues no relay.
10. **`normalize` Checkpoint.** With a `snapshot` whose `type == "Checkpoint"`,
    `normalize` relays `POST /state` with the snapshot's `data` as body and its
    remaining keys as headers, then returns the base **with `snapshot` removed**.
    A non-`Checkpoint` snapshot is removed but **not** loaded (no relay).
11. **`snapshot`.** `snapshot` relays `POST /snapshot/<id>` with body `{}`
    (content-type `application/json`); on success it returns the CU response
    verbatim as `{ok, R}`, and on **any** relay failure it returns
    `{ok, #{ "error" => "No checkpoint produced.", "error-details" => <E> }}`
    rather than an error.
12. **No local state / no fabrication.** The device keeps no process state of its
    own; on a `compute` relay/decoder failure it propagates the error and merges
    no result (it never invents a default result).
13. **Trust boundary.** The device performs no independent verification of the
    CU's returned result; a conforming implementation accepts the decoded
    `results` as-is (the determinism/trust caveat, §7).

## 11. Out of scope

- The **internal representation** of messages, the process state, results, and
  snapshots; the snapshot blob's structure (opaque).
- The **node routing table / route selection** that maps the CU endpoint paths to
  an actual host — which CU a process delegates to, route templates, peer
  selection, and transport mechanics (headers beyond those specified, timeouts,
  retries, TLS) are `relay@1.0` + router configuration, not this device.
- The **AOS2 assignment node schema** (the per-assignment JSON fields inside
  `edges[].node`) — the scheduler/format contract; only the endpoint, method, and
  that the body is JSON over the single `{slot ⇒ assignment}` input are pinned
  here.
- The exact **legacy JSON Message / result schemas** and the unset/tags/outbox
  mapping — delegated to `json-iface@1.0` (`to` for requests, `from` for
  responses).
- The **CU-availability check, dedup, and patch** orchestration applied around
  this device by `genesis-wasm@1.0` (that wrapper's behaviour, not this device's).
- Whether `relay@1.0` **re-signs** the outbound request (its `commit-request`
  policy gate) — out of scope for this device.
- Performance, concurrency, and storage strategy; the orchestrator's persistence
  of the returned `results`.

## Open questions

- **CU endpoint configuration is implicit in routing.** This device emits relative
  paths (`/result/...`, `/dry-run`, `/state`, `/snapshot/...`) and relies entirely
  on the node's routing table to resolve them to a concrete CU host. The spec
  cannot pin *how* an operator binds a given process to a given CU (no `cu-url` /
  `node` field is read by this device); that binding lives in router/node config.
  A reimplementer should match "emit these relative paths through the relay" and
  treat host selection as external — but confirm whether a deployment expects a
  per-process CU-URL field on the message that this device should read directly.
- **`snapshot`/`normalize` use direct vs. `target`-wrapped relays.** The relays
  issued by the four keys differ slightly in how the outbound HTTP fields are
  presented to `relay@1.0` (compute uses a `target => payload` indirection;
  `snapshot` uses `relay-method`/`relay-path` fields directly). Both reduce to
  "relay a POST to the given path with the given body and `application/json`
  content-type"; a reimplementer should reproduce the **observable** call
  (method, path, body, content-type, cache-control), not necessarily the exact
  relay-field plumbing, which is `relay@1.0`’s concern.
- **Dry-run does not advance state, but the device does not enforce slot
  monotonicity.** Whether a caller may interleave dry-runs and assignments, and
  any ordering guarantees between them, is determined by the orchestrator
  (`process@1.0`/scheduler/dedup), not this device. Confirm the orchestrator
  contract if reusing this device standalone.
- **`results/raw` consumers.** The device exposes both the mapped `results` and
  the unmapped `results/raw` (parsed JSON term). The intended consumers of
  `results/raw` (vs. the structured `results`) are not specified here; a
  reimplementer should preserve both but confirm whether downstream devices rely
  on `results/raw`.
