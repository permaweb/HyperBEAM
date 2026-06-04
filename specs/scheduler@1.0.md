# `scheduler@1.0` — the slot-assignment / ordering authority for AO processes

- **Device name:** `scheduler@1.0`
- **Depends-on:** `message@1.0` (commitment / ID / verification surface), `httpsig@1.0` (default commitment device for assignments), `structured@1.0` (TABM conversion that underpins IDs and the bundle wire form). Optionally interacts with `location@1.0` (scheduler-location discovery), `json-iface@1.0` and `ans104@1.0` (legacy interop). The first three specs are provided to reimplementers; the rest are referenced by `name@version` only.
- **Status:** Draft

## 1. Overview

`scheduler@1.0` is the **ordering authority** for an AO process. Messages
addressed to a process are not executed in the order a node happens to receive
them; instead the scheduler assigns each one a **slot** — a 0-based, gap-free,
monotonically increasing integer — and emits a signed **assignment** binding
that message to that slot, a wall-clock timestamp, an Arweave block reference,
and a **hash-chain** link to the previous assignment. The totally-ordered
sequence of assignments *is* the process's canonical input log; every node that
replays it computes the same process state. This device is the sole writer of
that log for the processes it hosts.

A process names its scheduler in its definition (the `scheduler` /
`scheduler-location` binding, §3). A node that holds the matching signing wallet
**is** that process's scheduler and assigns slots locally; a node that does not
**redirects** (or transparently proxies) to whichever node does. The device also
serves the log back out: a single slot's worth of state (`slot`), a forward
range of assignments (`schedule` GET), and — for the local `~process@1.0`
compute loop — the next unprocessed assignment (`next`).

This document specifies the **observable protocol contract**: the assignment
message shape, the slot/ordering guarantees, the read/write key behaviours, the
two serialization formats, and the discovery/authority model. Internal
representation, module layout, caching strategy, and concurrency mechanism are
out of scope (§11).

## 2. Concepts & terminology

- **Process / process ID:** the AO process being scheduled. Its ID is the
  base64url (43-char) committed ID of the process-definition message
  (`message@1.0` `id`). The process ID is the namespace under which all of its
  assignments live. A *process-definition message* is a committed message with
  `type: Process` carrying the scheduler binding (§3); it MUST verify and MUST
  have at least one signer, or it cannot be used to derive a process ID.
- **Assignment:** the signed message the scheduler emits for one slot. It binds
  the scheduled message (under `body`) to a slot number and the ordering metadata
  of §5. The assignment is **content-addressed and committed**: its ID and
  signature are computed exactly as for any `message@1.0` message.
- **Slot (a.k.a. nonce):** a non-negative integer identifying an assignment's
  position in a process's log. Slots are **0-based**, assigned in **strictly
  increasing** order with **no gaps**, one per assignment. "Slot" and "nonce" are
  used interchangeably; legacy formats spell it `nonce`, the current format
  spells it `slot`.
- **Hash-chain:** a per-process running hash that links each assignment to its
  predecessor, so the sequence cannot be reordered or have entries inserted
  without detection. Computed over slot positions as `base-hashpath` (§5, §6).
- **Schedule:** the ordered collection of all assignments for a process,
  slot `0..current`.
- **Current slot:** the highest slot the scheduler has assigned for a process, or
  `-1` if none has been assigned yet (an empty schedule). The *next* slot to be
  assigned is `current + 1`.
- **Scheduler-location binding:** the value of a process's `scheduler` (or
  `scheduler-location`) key. It names the address and/or URL of the scheduler
  responsible for the process (§3, §7).
- **Variant:** a tag distinguishing the **current** AO-Core scheduler protocol
  (`ao.N.1`) from the **legacy** net-SU protocol (`ao.TN.1`). The variant
  determines wire format, slot/nonce offset, and remote endpoint shape (§5).
- **Local vs remote scheduler:** a scheduler is *local* to a node iff the node
  holds a signing wallet whose address matches the process's scheduler-location
  binding. Otherwise it is *remote* and the device redirects or proxies.

## 3. Device interface

- **Dispatch shape:** **default-handler.** The device names a small set of
  specific keys (`status`, `next`, `schedule`, `slot`, `init`, `checkpoint`) and
  routes **every other key — and every bare invocation — to the `schedule`
  behaviour** (§4 `schedule`), which itself dispatches on the request `method`
  (POST → write, GET → read). The default handler is how a `POST` to the device
  with no explicit `path` reaches scheduling.
- **Excluded keys:** the device MUST NOT capture the message-manipulation /
  inspection keys; at minimum `set` and `keys` MUST fall through to
  `message@1.0`. (Per `message@1.0` §9, a default-handler device that swallows
  these breaks `set` / path-binding on the device.)
- **`init` / `checkpoint`:** these keys exist for the `~process@1.0` compute
  lifecycle. `init` has **no scheduler-specific behaviour** — it resolves through
  to `message@1.0` (an identity no-op returning the base). `checkpoint` returns
  the supplied state unchanged (`{ok, State}`); it is a hook for the compute
  framework and carries no scheduler semantics. An implementation MAY omit
  bespoke logic for both.

**Message shapes the device operates on:**

- **Process-definition message** (the `Base`, when scheduling for / reading a
  process the node hosts): a committed message with `type: Process` carrying a
  `scheduler` / `scheduler-location` binding. Required to derive the process ID
  and to decide locality.
- **Message-to-schedule** (the subject of a `POST`): an arbitrary committed
  message to be appended to a process's log, OR a process-definition message
  (which both registers the process and is itself scheduled at slot 0). It is
  located within the request per the **subject-selection** rules below.
- **Schedule-read request** (`GET`): carries optional `from`, `to`, `accept`,
  `target` / `process` fields (§4 `schedule`, `slot`).

### Process-ID selection (target resolution) — normative

`POST` and the read keys must agree on **which process** a request concerns. The
device resolves the process ID from the `(Base, Req, ToSched)` triple in this
precedence order (first match wins). `ToSched` is the message-to-schedule
(present only for `POST`); for reads, skip the `ToSched` steps.

1. If `ToSched` has `type: Process` → the process ID is `ToSched`'s committed ID.
2. Else if `ToSched` has a `target` key → that value (normalised to
   human-readable base64url).
3. Else if `Req` has a `target` key → that value (normalised).
4. Else if `Req` has `type: Process` → `Req`'s committed ID.
5. Else if `Base` has a `process` key → the ID of `Base`'s embedded process.
6. Else if `Base` has `type: Process` → `Base`'s committed ID.
7. Else → `Req`'s committed ID.

A process ID extracted from a `target`/path MAY carry a query-string **hint**
(`?hint=<url>`) and/or other query parameters; the bare 43-char base64url prefix
is the process ID, and the remainder is used only for discovery (§7). The
human-readable process ID (hint stripped) is what namespaces stored assignments.

### Subject-selection (which message gets scheduled) — normative

On `POST`, the message to schedule is located within `Req` in this order:

1. `Req`'s `subject` key equals `base` → schedule `Base`.
2. `Req`'s `subject` key equals `self` → schedule `Req` itself.
3. `Req`'s `subject` key has another value `K` → schedule the value of `Req`'s
   key `K`. If `Req` has no key `K`, fall back to `Req` itself.
4. No `subject` key → schedule `Req`'s `body` if present, else `Req` itself.

The selected message MUST be fully loadable; if any linked component cannot be
materialised the device MUST return `{status: 404, body: "Cannot fully load
message to schedule."}`.

## 4. Resolved keys (normative)

### `schedule` (the default handler) — append or read the log

- **Signature:** `(Base, Req, Opts) → {ok, Result} | {error, Map}`.
- **Reads:** `Req`'s `method` (default `GET`, case-insensitive). Dispatches:
  - `method = POST` → **append** (below).
  - `method = GET` (or anything non-POST) → **read** (below).

#### `POST` — append a message to the process's log

- **Reads:** the subject message (subject-selection, §3); the resolved process ID
  (target resolution, §3); the scheduler binding of the process; node options
  controlling verification and redirect-following.
- **Behaviour:**
  1. Locate and fully load the subject message; reduce it to **only its committed
     keys** (drop anything not covered by a commitment, retaining commitments).
     If committed components cannot be validated, error with `{status: 400, body:
     "Message invalid: Committed components cannot be validated.", reason: …}`.
  2. Resolve the process ID and **locate the scheduler** for it (§7 discovery):
     - **Local** (this node is the scheduler): proceed to (3).
     - **Remote**: if redirect-following is **enabled** (node option, default
       **on**), transparently proxy the append to the remote scheduler (§9) and
       return the resulting assignment. If **disabled**, return the redirect
       message itself: `{status: 307, location: <url>, variant: <variant>, body:
       "Redirecting to scheduler: <url>"}`.
     - If no scheduler binding can be found at all: error `"No scheduler
       information provided."`.
  3. **Re-verify** the committed subject message according to the node's
     assignment-verification policy (§7). Under the default policy a message with
     no signers, or one that fails `message@1.0` `verify`, is rejected with
     `{status: 400, body: "Message is not valid.", reason: "Given message is
     invalid."}`.
  4. **Assign** the next slot to the message: construct, commit, and persist the
     assignment of §5 (this is the only point at which `current` advances; see
     §6 for the serialization guarantee). If the subject is itself a
     process-definition (`type: Process`), the scheduler additionally persists
     the process definition and registers itself as that process's scheduler
     before assigning slot 0.
- **Returns:** `{ok, Assignment}` — the freshly committed assignment message
  (§5). On the non-following remote branch, `{ok, Redirect}` (the 307 map).
- **Side effects:** writes the assignment (and, for a new process, the process
  definition) into the node's store under the assignment keyspace (§5); links the
  per-slot path to the assignment ID; advances the hash-chain; and MAY upload the
  message and assignment to permanent storage (Arweave) depending on the node's
  scheduling mode (§6). None of the upload steps may change the returned result.

#### `GET` — read a range of the log

- **Reads:** the process ID (target resolution, §3); `from` and `to` from `Req`;
  `accept` from `Req` (default `application/http`).
- **Behaviour:**
  1. `from` defaults to `0`; a negative `from` is clamped to `0`. `to` defaults
     to *unbounded* (meaning "up to and including the current slot").
  2. If the process is **remote** and redirect-following is enabled, the device
     MAY fetch the missing range from the remote scheduler, merging any
     assignments it already holds locally with the remote response (§9); if
     disabled it returns the 307 redirect map.
  3. For a **local** process, gather assignments for slots `from..to` inclusive
     from the node's store. The range is **capped at 1000 assignments per
     response** (`MAX_ASSIGNMENT_QUERY_LEN`); if the requested range is larger,
     only the first 1000 (`from .. from+1000`) are returned and the response is
     marked as continuing (§5 bundle `continues`).
  4. Serialize the gathered assignments in the requested format (§5):
     `accept = application/aos-2` → the **legacy AOS2 JSON** form; anything else
     (including the default `application/http`) → the **HTTP-signed bundle** form.
- **Returns:** `{ok, Bundle}` (bundle form) or `{ok, #{content-type:
  application/json, body: <json>}}` (AOS2 form). The bundle's `assignments` map
  is keyed by slot number; absent slots in the requested range are simply not
  present (a sparse/short result is not an error).
- **Side effects:** none for a purely local read. A remote read MAY cache fetched
  assignments into the local store (best-effort; failure does not affect the
  result).

### `slot` — current-slot summary for a process

- **Signature:** `(Base, Req, Opts) → {ok, Map} | {error, Map}`.
- **Reads:** the process ID (target resolution, §3); the scheduler binding.
- **Behaviour:** Locate the scheduler. **Local**: read the current slot from the
  process's scheduler state and the current Arweave time reference, and return a
  summary. **Remote**: redirect (307) or, if following, query the remote
  scheduler's slot endpoint and normalise its answer.
- **Returns (local):** a map with **exactly** these keys:
  - `process` — the process ID (base64url).
  - `current` — the current (highest assigned) slot integer (`-1` if empty).
  - `timestamp` — current Arweave-time millisecond timestamp.
  - `block-height` — current Arweave block height (integer).
  - `block-hash` — current Arweave block hash (base64url / human-readable).
  - `addresses` — the list of scheduler signer addresses (base64url) that commit
    this process's assignments.
  - `cache-control` — the binary `no-store` (this result is volatile; §6).
- **Side effects:** none.

### `next` — yield the next unprocessed assignment (compute-loop key)

- **Signature:** `(Base, Req, Opts) → {ok, #{ body := Assignment, state :=
  NewBase }} | {error, Map}`.
- **Reads:** `Base`'s `at-slot` key (the last slot the caller has processed; an
  integer, `-1` before any slot is processed); any assignments already cached on
  `Base` from a prior `next` call.
- **Behaviour:** Determine the target slot `at-slot + 1`. Find that assignment —
  from assignments already attached to `Base`, else from the node's local store,
  else (for a process the node does not host) by reading a forward range from the
  scheduler via the `schedule` GET path. Return the **single** assignment whose
  slot is exactly `at-slot + 1`. This key exists for `~process@1.0`'s compute
  loop, which calls it repeatedly to walk the log in order.
- **Returns:** `{ok, #{ body => Assignment, state => NewBase }}` where `NewBase`
  is `Base` with the (possibly fetched) lookahead assignments attached for the
  next call. Errors:
  - the requested slot is not yet available → `{status: 404, body: "Requested
    slot not yet available in schedule."}`.
  - the located assignment's slot is unparseable → `{status: 500, body:
    "Unprocessable slot value received in assignment."}`.
  - the located assignment's slot ≠ `at-slot + 1` → `{status: 404, body:
    "Received assignment slot does not match expected slot.", unexpected-slot,
    expected-slot}`.
- **Side effects:** MAY populate the returned `state` with prefetched
  assignments; MAY cache fetched assignments locally. The returned assignment
  MUST have slot exactly `at-slot + 1` (gap-free ordering is enforced here, §6).

### `status` — node-wide scheduler status

- **Signature:** `(Base, Req, Opts) → {ok, Map}`.
- **Reads:** node options only (does not read `Base`/`Req` content).
- **Behaviour:** Report the scheduler's own identity and the processes it
  currently hosts.
- **Returns:** a map with:
  - `address` — the node's scheduler wallet address (base64url).
  - `processes` — a list of the process IDs (base64url) the node currently
    schedules locally. Order unspecified.
  - `cache-control` — the binary `no-store`.
- **Side effects:** none.

### `init` / `checkpoint`

- `init` → resolves to `message@1.0` (identity; returns `Base`). No
  scheduler-specific behaviour.
- `checkpoint` → returns the supplied state unchanged (`{ok, State}`).

## 5. Data formats & encodings

### 5.1 The assignment message (current variant `ao.N.1`) — normative

An assignment minted by a local scheduler is a committed message with the
following keys (binary, lowercase, hyphenated). All ID/address/hash values are
**base64url** ("human-readable"), never hex.

| Key | Type | Value |
|---|---|---|
| `type` | binary | constant `<<"Assignment">>` |
| `data-protocol` | binary | constant `<<"ao">>` |
| `variant` | binary | constant `<<"ao.N.1">>` |
| `process` | binary | the process ID (base64url, hint-stripped) |
| `slot` | integer | the assigned slot (`current + 1` at mint time); 0-based, gap-free, monotonic |
| `epoch` | binary | constant `<<"0">>` (epoch indicator; reserved for future segmentation) |
| `block-height` | integer | Arweave block height at mint time |
| `block-hash` | binary | Arweave block hash at mint time (base64url) |
| `block-timestamp` | integer | Arweave block timestamp at mint time |
| `timestamp` | integer | the scheduler's **local** wall-clock time in **milliseconds** at mint time (NOT Arweave time) |
| `base-hashpath` | binary | the hash-chain value linking this assignment to its predecessor (§5.3) |
| `path` | binary | the **scheduled (subject) message's OWN `path` key**, else the constant `<<"compute">>` if the subject carries none. (NOT the schedule-dispatch request path — a POST arrives at path `schedule`, but that is the device-invocation path, not the assignment's `path`. Read the subject's `path`, default `compute`.) |
| `body` | message | the **committed-only** scheduled message (its commitments retained, all uncommitted keys stripped) |
| `commitments` | map | added by signing — one commitment per scheduler wallet (§7) |

- The assignment is committed by the scheduler over its **committed (TABM) form**
  exactly as `message@1.0`/`structured@1.0` define; its ID is the
  `message@1.0` ID over that form. There is **no** bespoke assignment ID
  derivation.
- The assignment carries the scheduled message inline under `body`. The `body`
  itself retains only committed keys; a verifier checks the inner message's
  signature independently of the assignment's.
- Implementations MUST emit `slot` as a true integer (not a binary). `timestamp`,
  `block-height`, `block-timestamp` are integers.

### 5.2 The schedule bundle (`application/http`, the default GET form) — normative

A `GET /schedule` response in the default format is a single message ("bundle")
with these keys:

| Key | Type | Value |
|---|---|---|
| `type` | binary | constant `<<"schedule">>` |
| `process` | binary | the process ID (base64url) |
| `continues` | boolean | `true` iff the response was truncated (more slots exist beyond `to`); else `false` |
| `timestamp` | integer | current Arweave-time millisecond timestamp |
| `block-height` | integer | current Arweave block height |
| `block-hash` | binary | current Arweave block hash (base64url) |
| `assignments` | map | a map from **slot number** → the assignment message of §5.1 for that slot |

- `assignments` is keyed by slot, rendered as the slot's **decimal-string binary**
  (`<<"0">>`, `<<"1">>`, …) — **not** the bare integer. This is normative, not
  cosmetic: an integer-keyed map does **not** survive the `structured@1.0`
  TABM conversion the bundle undergoes (the codec cannot encode integer map keys,
  so the map collapses to empty), whereas decimal-string-binary keys round-trip
  intact. It is **not** an ordered list on the wire; ordering is recovered from the
  slot keys. The map contains only slots actually present in the store for the
  requested range; gaps below `current` are simply absent (the read does not
  fabricate them).
- When transported as an HTTP-signature bundle, the encoded message additionally
  acquires the structural keys an `httpsig@1.0` bundle carries (a hashpath entry
  and a `commitments` entry); these are an artefact of the transport encoding,
  not part of the logical schedule, and consumers count only the slot-keyed
  members as assignments.

### 5.3 Hash-chain (`base-hashpath`) — normative

Each assignment carries `base-hashpath`, a per-process running hash that orders
the log cryptographically:

- For the **first** assignment of a process (slot 0), the base value is the
  process ID itself (its human-readable form).
- For each subsequent slot, the base value is the **substrate hashpath** of (the
  previous assignment's `base-hashpath`, the previous assignment's committed ID).
  This is the AO-Core substrate's path-hashing function (`hb_path:hashpath`, the
  same operation the resolver uses to fold a path), with the process's configured
  algorithm (default **`sha-256-chain`**) — **NOT** a literal single hash
  `sha-256(a‖b)`. Its result is the substrate's **path-form** binary
  `<prev-base-hashpath>/<prev-id>` (a folded hashpath), not a bare 32-byte digest.
  So `base-hashpath(n) = hashpath(base-hashpath(n-1), id(assignment(n-1)))` where
  `hashpath` is the substrate fold; an implementer reading the shorthand
  `H(a, b)` as a single concatenated digest WILL produce a different value and
  fail the verifier.
- **The folded `id(assignment(n-1))` is the assignment's signed committed ID.**
  This id is **stable** — it is computed over the (single) signature the
  assignment carries, so a node minting it and a node reading it back from the
  log obtain the *same* id, **provided the stored assignment retains its signed
  commitment** (§5.5). The chain is therefore deterministic and re-derivable by
  any node reading the schedule (§6). The one way it goes wrong is a persistence
  path that **drops the signature** on read-back (the §5.5 link trap): then the
  read-back id silently becomes the assignment's *content* (HMAC) id instead of
  its signed id, and the re-derived chain no longer matches. So the rule is not
  "re-read to get a different id" — a correct implementation gets the *same*
  signed id back; it is "ensure the stored assignment keeps its signature so its
  committed id round-trips" (§5.5).
- Because each link folds in the previous assignment's ID, inserting, dropping,
  or reordering any assignment changes every subsequent `base-hashpath`,
  detectably breaking the chain. A verifier re-derives the chain from slot 0 and
  checks it against the assignments' `base-hashpath` values.

### 5.4 Legacy variant (`ao.TN.1`) and AOS2 JSON form — informative-normative

For interoperability with legacy net-SU schedulers, the device understands a
second variant, `ao.TN.1`, and a JSON serialization ("AOS2"). The device does
**not mint** `ao.TN.1` assignments; it only **reads, proxies, and normalises**
them. An implementation targeting only `ao.N.1` MAY omit legacy support, but if
it claims legacy interop it MUST observe:

- **Nonce vs slot offset:** the legacy scheduler reports the slots *after* a
  stated nonce. When fetching a range from a legacy scheduler with lower bound
  `from`, the request nonce sent is `from - 1`; the returned `nonce` field maps
  directly to the canonical `slot`.
- **AOS2 GET response (`accept = application/aos-2`):** a JSON object
  `{ page_info: { process, has_next_page, timestamp, block-height, block-hash },
  edges: [ { cursor, node: { message, assignment } }, … ] }`. `cursor` is the
  slot number. `message` and `assignment` are the JSON-encoded scheduled message
  and assignment (via the `json-iface@1.0` codec). On the wire the JSON body is
  returned under `body` with `content-type: application/json`.
- **AOS2 → canonical normalisation:** reading a legacy assignment maps `nonce` →
  `slot`, coerces `timestamp` / `epoch` / `slot` string fields to integers,
  defaults a missing `block-hash` to the base64url of 32 zero bytes, and attaches
  the inner message under `body`. **This normalisation is destructive to the
  assignment's verifiability** — a normalised legacy assignment can no longer be
  re-verified as originally signed; it is a compatibility view only.
- **Legacy POST:** a message scheduled to a legacy (`ao.TN.1`) process MUST be
  signed with an **ANS-104** commitment (`ans104@1.0`); the device serializes it
  as an ANS-104 data-item to the legacy scheduler. A message with no ANS-104
  signer is rejected with `{status: 422, body: "Process resides on legacy
  scheduler. Message must be signed with ANS-104."}`.

### 5.5 Assignment keyspace (store) — normative-observable

Assignments are stored under a **device-namespaced pseudo-path** rooted at the
literal prefix `~scheduler@1.0`:

```
~scheduler@1.0/assignments/<process-id>/<slot>  ->  <assignment message>
```

- `<process-id>` is the human-readable (base64url) process ID; `<slot>` is the
  slot rendered as its normalised key. The per-slot path resolves to the stored
  assignment by its committed ID; reading the path returns the assignment.
- **The stored, slot-indexed assignment MUST retain its signed commitment** — a
  read of the per-slot path MUST return the assignment with the scheduler's
  signature intact (so it re-verifies per §7 and its committed ID is the stable
  id the hash-chain folds, §5.3). CAUTION: a plain content-addressed *link* to a
  signed message can, on this substrate, resolve to the **content-only (HMAC)
  view** and **drop the signed (e.g. RSA-PSS) commitment** — yielding an
  assignment that no longer verifies and whose read-back id is the *content* id,
  silently breaking both §7 verification and the §5.3 chain. **The working
  pattern (pin this — every independent implementation that got it right used
  it):** `hb_cache:write` the signed assignment (making it readable by its signed
  committed id), store that committed **id as a raw value** at the slot path
  (`hb_store:write(#{<slot-path> => <committed-id>})`), and read the assignment
  back by resolving that id directly (`hb_cache:read(<committed-id>)`). Do **NOT**
  index the slot with a content-addressed *link* (`hb_cache:link`/`hb_store:link`)
  — **even a link to the *signed* id resolves to the content node and drops the
  signature**; this is the trap. The exact mechanism is out of scope, but
  "raw-id-index + read-by-id, never a link" is the only pattern observed to
  preserve the signature, and the signature-preserving outcome is **normative**.
- **`status`'s `processes` list is the set of process IDs that appear as
  children under `~scheduler@1.0/assignments`** — i.e. the processes for which the
  node holds at least one assignment. (Registration assigns slot 0, so a process
  this node schedules always has an assignments subtree and therefore appears.)
  Derive it by listing the `~scheduler@1.0/assignments` group, NOT from a separate
  registry keyspace.
- **The process *definition* (if persisted) MUST NOT pollute the per-process slot
  listing.** Listing `~scheduler@1.0/assignments/<process-id>` MUST yield exactly
  the assigned slot integers and nothing else. Therefore persist the definition at
  a **separate path NOT under the per-process assignments subtree** — e.g. a
  sibling keyspace such as `~scheduler@1.0/processes/<process-id>` — so that the
  raw listing of the assignments subtree is naturally clean. (Co-locating it under
  `.../assignments/<process-id>/` — even under a non-integer child key — is
  discouraged: it forces every reader to integer-filter the listing, and a reader
  that does not will see a phantom entry.) The exact location is out of scope, but
  the "slot listing = exactly the slots" invariant is normative.
- The **observable** requirements are: after slot `n` is assigned for process
  `P`, a read of `.../assignments/P/n` returns that assignment (signature intact);
  listing `.../assignments/P` enumerates exactly the assigned slot numbers (and
  nothing else); and the latest is the maximum such slot. The physical store tier
  is out of scope (§11).
- A node MAY direct assignment writes to a separate store tier from its main
  store (e.g. a volatile schedule store). Whether the schedule survives a restart
  is a deployment property, not a protocol guarantee.

## 6. Ordering, freshness & caching

- **Slot numbering.** Slots are integers starting at **0**, assigned in
  **strictly increasing** order, **one per assignment**, with **no gaps**. The
  "current slot" is the highest assigned (or `-1` for an empty schedule). The
  next assignment always takes `current + 1`.
- **Single-writer serialization.** For a given process on a given node, slot
  assignment MUST be **serialized**: concurrent append requests are ordered
  through a single point so that no two assignments ever receive the same slot
  and the `current` counter advances by exactly one per assignment. The mechanism
  (a per-process serialising server) is out of scope; the **guarantee** — no
  duplicate slots, no skipped slots, even under concurrent POSTs — is normative.
  (A node that loses confirmation of an assignment MUST NOT silently leave a gap;
  stale requests whose client has already timed out MAY be dropped without
  assigning a slot.)
- **Tie-break / arrival order.** There is **no** content-based tie-break: order
  is exactly the serialized arrival order at the scheduler. Two messages racing
  for the same process get adjacent slots in whatever order the single writer
  accepts them; the assignment's `timestamp` (scheduler-local milliseconds)
  records when each was accepted but does **not** redefine the order.
- **Hash-chain determinism.** Given the same sequence of assignment IDs, the
  `base-hashpath` chain is a deterministic function of the process ID and the
  per-assignment IDs (§5.3); any node can re-derive and check it.
- **Replay determinism.** The schedule (slots `0..current`) is the canonical,
  totally-ordered input log: any node replaying it through `~process@1.0` reaches
  the same state. This is the property the whole device exists to provide.
- **Freshness / cache-control.** `slot` and `status` are **volatile** and MUST be
  returned with `cache-control: no-store`. The `schedule` GET range up to a fixed
  `to` is effectively immutable (past assignments never change), but an
  unbounded/open-ended schedule grows at a constant path; a node serving the
  device over HTTP for the mutable views (`slot`, open `schedule`, `next`) MUST
  disable result caching for those paths (per the build-device skill:
  `force-message` + `no-store`/`no-cache`), or reads go stale.
- **Confirmation modes.** A node MAY confirm an append to its client at different
  points of the persist/upload pipeline (e.g. as soon as the slot is assigned, or
  only after local write, or only after upload to permanent storage). The choice
  affects latency and durability, not the assignment's content or slot; it is a
  node option and out of scope for the wire contract.
- **Read cap.** A single `schedule` GET returns at most **1000** assignments; a
  larger requested range is truncated and flagged via `continues = true`. Callers
  paginate by advancing `from`.

## 7. Security & authority

- **Assignment signing authority.** Assignments are committed by the
  **scheduler's wallet(s)** — the signing keys the node holds whose addresses
  match the process's scheduler-location binding. The set of wallets used is
  derived from the binding: for each scheduler address in the binding that the
  node can act as, its wallet signs the assignment. An assignment is therefore
  attributable to the scheduler that produced it; a verifier checks the
  assignment's commitment against the process's declared scheduler address.
- **Commitment device for assignments.** The selected commitment device is the
  process's `scheduler-commitment-spec` if set, else the **node's configured
  commitment device** — which under the default node configuration is
  **`httpsig@1.0`** (the broader AO-Core default; this is what a default node
  actually mints `ao.N.1` assignments with, and what the Depends-on line names).
  A bare `ans104@1.0` exists only as a deep code-level fallback when *no* node
  default is configured; on a default node it is NOT reached. **This choice
  changes the assignment's commitment bytes and therefore its committed ID** (and
  thus the hash-chain, §5.3) — implementers MUST pin the device their target
  network uses; for the documented default network it is `httpsig@1.0`. The device
  MUST sign each assignment with the selected device for each scheduler wallet.
- **Scheduler-location binding.** A process declares its scheduler via
  `scheduler` or `scheduler-location` (checked in that order) in its definition;
  a scheduled message MAY also carry a `scheduler-location`. The value is either
  a bare scheduler **address** (base64url), a comma-separated list of addresses,
  or a reference resolved through `location@1.0` to a URL. A `?hint=<url>` query
  parameter on the process ID / binding short-circuits discovery and names the
  scheduler URL directly (when hint-following is enabled, default on).
- **Where the binding is discovered (and the registered-process fallback).** The
  device looks for the scheduler-location binding, in order, on: (1) the scheduled
  message / `Req`, (2) the base process message (`Base`, incl. a process nested
  under `Base`'s `process` key), and (3) — **crucially for any append or read that
  targets an already-registered process by ID alone** — the **persisted process
  definition** recovered from the store (§4 POST step 4 persisted it at
  registration). Without this third source the Local arm of §4 is **unreachable**
  for a bare `POST target=<id>` / `GET <id>` after registration: such a request
  carries no binding of its own, so the device MUST reload the registered
  definition to recover it. Only if none of the three yields a binding does the
  request error `"No scheduler information provided."` (§8).
- **Locality decision (failure-closed on authority).** The node is the scheduler
  for a process **iff** the process has a discoverable scheduler binding (§3)
  **and** one of that binding's addresses equals an address the node can sign for
  (it holds that wallet). BOTH conjuncts are required. If so, it assigns locally;
  if not, it MUST NOT mint assignments — it redirects (307) to, or proxies to, the
  legitimate scheduler. Two failure-closed corollaries the node MUST observe even
  though it holds a signing wallet:
    - **Holding a wallet is not authority.** A node that holds a wallet is NOT
      thereby the scheduler for an arbitrary process. If the binding's addresses
      do not include the node's address (or the process names a *different*
      scheduler), the node MUST NOT self-assign — it redirects/proxies. Minting
      "because we happen to hold a key" is a conformance failure.
    - **No binding ⇒ not schedulable here.** A process with **no** discoverable
      scheduler binding cannot be scheduled by this node at all, wallet or not:
      it errors with `"No scheduler information provided."`. A node MUST NOT
      manufacture a binding for an unbound process by appointing itself.
  Equivalently: a wallet enables the node to *act as* an address the binding
  already names; it never *confers* that address. Absent a binding that names the
  node, the local arm is unreachable.
- **Append verification policy.** Before assigning a slot, the device re-checks
  the committed subject message under a node policy:
  - **default (`true`):** the message MUST have ≥ 1 signer **and** MUST pass
    `message@1.0` `verify`; otherwise it is rejected (`"Message is not valid."`).
  - **`accept_unsigned`:** the message is accepted if it passes `verify`
    (allowing unsigned/content-committed messages).
  - **`false`:** verification is skipped (the message is accepted as-is).
  Only **committed** keys of the subject ever enter the assignment's `body`;
  uncommitted keys are stripped before assignment, so a scheduler can never be
  induced to bind content the submitter did not commit to.
- **Remote proxy authority.** When proxying an append to a remote scheduler, the
  device forwards only the committed-only message; the remote scheduler is the
  authority that mints the slot. For a legacy (`ao.TN.1`) target the message MUST
  carry an ANS-104 commitment (§5.4).
- **Reads are unauthenticated.** `schedule` GET, `slot`, and `status` do not
  require a signed request.

## 8. Errors

The device returns errors as status-bearing maps (and, on a few legacy paths,
binary-body maps). The triggering conditions:

| Condition | Error |
|---|---|
| `POST`: subject message cannot be fully loaded | `{status: 404, body: "Cannot fully load message to schedule."}` |
| `POST`: committed components fail validation | `{status: 400, body: "Message invalid: Committed components cannot be validated.", reason}` |
| `POST`: subject fails the verification policy | `{status: 400, body: "Message is not valid.", reason: "Given message is invalid."}` |
| `POST`/read: no scheduler binding found for the process | `"No scheduler information provided."` |
| `POST` to a legacy process without an ANS-104 signature | `{status: 422, body: "Process resides on legacy scheduler. Message must be signed with ANS-104."}` |
| legacy POST: message cannot be encoded for the legacy scheduler | `{status: 422, body: "Incorrect encoding. Scheduler has variant: ao.TN.1", class, reason}` |
| remote (redirect-following disabled) | `{status: 307, location, variant, body: "Redirecting to scheduler: <url>"}` (returned as the result, not an error) |
| `next`: requested slot not yet available | `{status: 404, body: "Requested slot not yet available in schedule."}` |
| `next`: assignment slot unparseable | `{status: 500, body: "Unprocessable slot value received in assignment."}` |
| `next`: assignment slot ≠ expected (`at-slot + 1`) | `{status: 404, body: "Received assignment slot does not match expected slot.", unexpected-slot, expected-slot}` |
| target resolution: process message not retrievable | failure to resolve (e.g. `process_not_available` / `process_not_verified` raised by the process-ID derivation) |

> **Note on error style.** Like other process-layer devices (`location@1.0`),
> the scheduler's **observable** error contract is a `status` + `body` map (or a
> plain binary body for a few legacy-path messages), not the project's usual
> hyphenated error atoms. The status codes and body strings above are part of the
> contract and SHOULD match. Internal coercion / option handling MUST still use
> hyphenated atoms where it surfaces them, but the wire result shapes above are
> normative.

## 9. Composition

- **Driven by `~process@1.0`.** The process device delegates its own
  `schedule` / `slot` keys to its configured scheduler (defaulting to
  `scheduler@1.0`) and drives its compute loop by repeatedly resolving the
  scheduler's **`next`** key against the evolving process state (whose `at-slot`
  advances by one each step). `next` is the contract that makes ordered replay
  possible; `schedule`/`slot` are the public-facing append/inspect surface.
- **Fed by `~push@1.0`.** Messages typically reach a process's log via the push
  device performing a `POST /schedule`, or via a direct HTTP `POST` to
  `/<process-id>~process@1.0/schedule`.
- **Discovery via `location@1.0`.** When a scheduler binding is a reference (not a
  bare address or hinted URL), the device resolves it through `location@1.0` to a
  reachable URL before redirecting/proxying.
- **Serialization via `structured@1.0` / `httpsig@1.0`.** Assignment IDs,
  commitments, and the default HTTP bundle are produced through the standard TABM
  / HTTP-signature path; the schedule bundle is a `message@1.0` message and
  composes with any consumer that understands AO-Core messages.
- **Legacy bridge.** For `ao.TN.1` processes the device transcodes through
  `ans104@1.0` (POST) and `json-iface@1.0` (GET/normalise) so a current node can
  read from and write to legacy net-SU infrastructure transparently.
- **Default-handler hygiene.** Because the device is a default handler, it MUST
  let `set`/`keys` (and the other message-manipulation keys) fall through to
  `message@1.0`, exactly as any default-handler device must.

## 10. Conformance (normative checklist)

An implementation MUST exhibit all of the following:

1. **Dispatch.** Keys `status`, `next`, `schedule`, `slot` invoke their specific
   behaviours; `init` and `checkpoint` are no-ops (identity / pass-through state);
   every other key and every bare invocation routes to `schedule`, which
   dispatches on `method` (POST → append, GET/other → read). `set` and `keys` are
   NOT captured by the device.
2. **Process-ID resolution.** The process ID is selected from `(Base, Req,
   ToSched)` by the precedence of §3, normalised to human-readable base64url, and
   the same ID governs both append and read for one request.
3. **Subject selection.** On POST, the scheduled message is chosen by the
   `subject` rules of §3 (`base` / `self` / named key / `body` / `Req`), reduced
   to committed-only keys before assignment.
4. **Slot numbering.** The first assignment for a process is slot **0**; each
   subsequent assignment is exactly one greater; there are never gaps or
   duplicates, even under concurrent POSTs. `current` for an empty schedule is
   `-1`.
5. **Assignment shape.** A minted (`ao.N.1`) assignment carries exactly the keys
   of §5.1 with the stated constants (`type=Assignment`, `data-protocol=ao`,
   `variant=ao.N.1`, `epoch=0`), an integer `slot`, the Arweave block reference,
   a scheduler-local millisecond `timestamp`, the `base-hashpath` link, the
   request `path` (or `compute`), and the committed-only scheduled message under
   `body`.
6. **Assignment commitment.** Each assignment is committed by the scheduler's
   wallet(s) (the binding's address(es) the node holds) using the selected
   commitment device (process `scheduler-commitment-spec`, else node default,
   else `ans104@1.0`); the assignment's ID is the `message@1.0` ID over its TABM
   form (no bespoke derivation). A verifier can attribute the assignment to the
   process's declared scheduler.
7. **Hash-chain.** `base-hashpath` of slot 0 is the process ID; `base-hashpath`
   of slot `n>0` is the hashpath of `(base-hashpath(n-1), id(assignment(n-1)))`.
   Reordering/inserting/dropping any assignment breaks every subsequent link.
8. **`schedule` GET range.** `from` defaults to 0 (negative clamped to 0); `to`
   defaults to the current slot; the response includes assignments for the
   in-range slots keyed by slot number; a range wider than 1000 slots is
   truncated to 1000 with `continues = true`; otherwise `continues = false`.
9. **Bundle form.** The default (`application/http`) GET response is a message
   with `type=schedule`, `process`, `continues`, the current Arweave time
   reference, and an `assignments` map keyed by slot → assignment. The AOS2 form
   (`accept = application/aos-2`) is the JSON `{ page_info, edges:[{cursor,
   node:{message, assignment}}] }` shape with `cursor` = slot, returned as a JSON
   body with `content-type: application/json`.
10. **`slot` summary.** A local `slot` returns `process`, `current` (highest slot
    or `-1`), `timestamp`, `block-height`, `block-hash`, `addresses` (scheduler
    signer addresses), and `cache-control: no-store`.
11. **`next` ordering.** Given `Base` with `at-slot = k`, `next` returns the
    assignment whose slot is exactly `k+1`, or the specified 404/500 error if it
    is unavailable / mismatched; the returned slot MUST equal `k+1` (a mismatch is
    an error, never a silently-skipped slot).
12. **`status`.** Returns `address` (scheduler wallet address, base64url),
    `processes` (list of locally-hosted process IDs, base64url), and
    `cache-control: no-store`.
13. **Locality & redirect.** A node assigns locally **only when** the process has
    a discoverable scheduler binding **and** that binding names an address the node
    can sign for; merely holding *some* wallet is not authority to self-assign. A
    node that does not hold the binding's wallet returns a `307` redirect map
    (`location`, `variant`, body) when redirect-following is disabled, or
    transparently proxies the append/read to the remote scheduler when enabled. A
    process with no discoverable scheduler binding yields `"No scheduler
    information provided."` (it is not schedulable here, wallet or not).
14. **Append verification policy.** Under the default policy, a subject message
    with no signers or that fails `message@1.0` `verify` is rejected
    (`"Message is not valid."`); `accept_unsigned` accepts any `verify`-passing
    message; `false` skips verification. Only committed keys of the subject reach
    the assignment `body`.
15. **base64url everywhere.** Process IDs, scheduler addresses, block hashes, and
    assignment IDs are base64url on the wire and as store keys, never hex.
16. **Mutable-path freshness.** `slot`/`status` carry `no-store`; a node serving
    the volatile views over HTTP disables result caching so reads are not stale.
17. **(If legacy interop is claimed.)** A `ao.TN.1` process is read by mapping
    `nonce → slot` with the `from-1` offset, normalising AOS2 JSON to the
    canonical assignment shape (non-verifiable compatibility view), and a legacy
    POST requires an ANS-104 signature (else `422`).

## 11. Out of scope

- The internal representation of assignments, the schedule, the per-process
  serialising server, lookahead/prefetch workers, and the in-memory assignment
  cache.
- The specific store/cache backend and tiering (volatile vs. durable schedule
  store) used to satisfy the keyspace contract of §5.5; whether a schedule
  survives a node restart.
- The concurrency mechanism that enforces single-writer slot assignment (only its
  *guarantee* is normative).
- The exact confirmation point at which an append is acknowledged to the client
  (a node latency/durability option).
- The cryptographic mechanics of signing/verification and TABM byte layout
  (delegated to `message@1.0`, `httpsig@1.0`, `ans104@1.0`, `structured@1.0`).
- The Arweave-time source (block height/hash/timestamp) and any gateway/SU
  network protocol, retry, and selection policy. Network paths are specified
  behaviourally; they cannot be exercised offline.
- Performance, throughput, and storage strategy.

## Open questions

1. **`epoch` semantics.** Minted assignments hard-code `epoch: "0"`. The field is
   clearly intended for future log segmentation (epochs of assignments), but no
   current behaviour reads or advances it. An implementer should emit `"0"` and
   treat the field as reserved; whether non-zero epochs ever appear is unresolved.
2. **`timestamp` is scheduler-local, not Arweave.** The assignment's `timestamp`
   is the scheduler node's own wall-clock in milliseconds, distinct from
   `block-timestamp` (Arweave). Two schedulers' local clocks need not agree;
   `timestamp` is therefore **not** a cross-scheduler ordering key and must not be
   used as one. Confirm whether any consumer treats `timestamp` as authoritative
   time.
3. **Commitment device selection (see §7 for the normative rule).** Precedence:
   the process's per-process override key `scheduler-commitment-spec` → the node
   option (`commitment-device` / `scheduler-default-commitment-spec`) → a deep
   code-level `ans104@1.0` fallback reached **only** when no node default is
   configured. On the documented default `ao.N.1` network the node default is
   **`httpsig@1.0`**, so that is what a default node actually mints with; the bare
   `ans104@1.0` fallback is not reached on a configured node. This is no longer
   open for the default network (httpsig@1.0); the residual is only that the
   *option key names* are platform config, not protocol. The choice changes the
   assignment's commitment bytes and thus its ID, so an implementer MUST pin the
   device their target network expects.
4. **Hash-chain field naming.** Assignments expose the chain as `base-hashpath`;
   some legacy/aos2 paths and the cache's "latest" lookup also consult a
   `hash-chain` key as a fallback. Whether `hash-chain` is a distinct legacy field
   or an alias of `base-hashpath` is not fully pinned here; an `ao.N.1`-only
   implementation should standardise on `base-hashpath` and treat `hash-chain` as
   a legacy read-only alias.
5. **Bundle `assignments` key type — RESOLVED (§5.2).** The key MUST be the slot's
   decimal-string binary (`<<"0">>`), never the bare integer: integer map keys do
   not survive the bundle's `structured@1.0` TABM conversion (the map collapses to
   empty), while decimal-string-binary keys round-trip intact. Earlier drafts left
   this open ("either rendering"); it is not — only the decimal-string form works.
6. **Truncation `continues` vs. legacy `has_next_page`.** The bundle uses
   `continues` (boolean) and the AOS2 form uses `page_info.has_next_page`; both
   signal "more slots exist". An implementer bridging the two should map them
   directly.
