# `genesis-wasm@1.0` — legacy-net AO compute bridge (execution device)

- **Device name:** `genesis-wasm@1.0`
- **Depends-on:** `message@1.0` (message model, `id`/`committers`/`verify`, `set`/`get`/`remove`, commitment selection). Relates to `process@1.0` (the orchestrator that installs this device as a process's *execution device*), `delegated-compute@1.0` (the device this one drives to talk to the legacy CU over HTTP), `patch@1.0` (applies the computation's emitted patches onto state), `dedup@1.0` (once-only assignment semantics), and `json-iface@1.0` (the AOS2 JSON mapping the legacy CU speaks). The `message@1.0` spec is provided to reimplementers; the related specs are named for context.
- **Status:** Draft

## 1. Overview

`genesis-wasm@1.0` lets **existing "legacynet" (`ao.TN.1`) AO process
definitions** run unmodified under an AO-Core node. It is an **execution
device**: a process whose `execution-device` is `genesis-wasm@1.0` (or whose
`variant` is `ao.TN.1`, which a `process@1.0` orchestrator maps to this device
by default) has its per-slot computation handled here.

The device does **not** itself contain a WASM engine. Instead it **bridges to a
legacy AO "genesis" Compute Unit (CU)** — a separate local sidecar process
exposing an HTTP API and speaking the legacy AOS2 JSON protocol. On each compute
step the device (1) ensures the sidecar is live (starting it if necessary), (2)
asks a delegated-compute layer to POST the assignment to the CU and decode the
AOS2 result into an AO-Core *results* message, and (3) lifts the `PATCH`
submessages the computation emitted onto the live process state. It also
implements the remaining execution-device hooks (`init`, `normalize`,
`snapshot`) and a checkpoint **import** path that adopts a trusted legacy
on-chain checkpoint as local process state.

This spec pins the **execution-device interface**, the **legacy CU HTTP contract
and its endpoint configuration**, the **result→state mapping**, the **sidecar
lifecycle (ensure-started) and its configuration defaults**, the **import/trust
rules**, the **error atoms**, and the **side effects** (subprocess spawn, network
I/O). The internal representation of messages and the device's own bookkeeping
are out of scope (§11).

## 2. Concepts & terminology

- **Execution device.** A device a `process@1.0` orchestrator swaps into the
  process message in place of `process@1.0` to perform a phase of process logic
  (here: compute / init / normalize / snapshot). The orchestrator resolves the
  phase key (e.g. `compute`) against the process message **after** rebinding its
  `device` to the execution device; on return it restores the process device.
  This device is the default execution device for `ao.TN.1` processes.
- **Legacy CU (genesis Compute Unit).** A local sidecar HTTP server implementing
  the legacy AO compute-unit protocol (the AOS2 JSON request/response shape).
  It owns its own persistent process memory and checkpoints; the node treats it
  as an opaque trusted compute engine reachable at a configured local port.
- **AOS2 / legacy JSON.** The JSON object shape the legacy CU consumes and
  produces (the "Message"/"Process"/result structures). The exact field schema
  and the AO-message↔JSON mapping are defined by the `json-iface@1.0` spec; this
  device only references that mapping, it does not redefine it.
- **Assignment vs dryrun.** An **assignment** is a scheduled, ordered message
  with a concrete `slot`; computing it advances and persists process state. A
  **dryrun** is a speculative evaluation with no assignment that MUST NOT mutate
  persisted state (used for read-only queries against a process).
- **Outbox / results.** After a compute step, the decoded result is an AO-Core
  *results* message containing (among other keys) an `outbox` of outgoing
  messages and `data`. Submessages in the outbox marked as patches
  (`method: PATCH` or `device: patch@1.0`) describe mutations to apply to the
  live process state (see `patch@1.0`).
- **Checkpoint.** A signed legacy on-chain transaction (Arweave) tagged
  `Type: Checkpoint` for a given `Process`, carrying a process-memory snapshot
  and a `nonce` (the slot the snapshot is at). Importing one seeds local state.
- **Import authority.** A wallet address trusted to vouch for legacy checkpoints.
  Importing is **failure-closed** against this allow-list (§7).
- **Ensure-started.** The idempotent operation that guarantees the legacy CU
  sidecar is running before any CU-backed key proceeds (§4.6).

The device's internal data structures, the CU's internal computation, and the
AOS2 byte schema are **out of scope**; only the interface, the HTTP contract,
the result mapping, the lifecycle, and the trust rules are normative here.

## 3. Device interface

- **Dispatch shape:** **explicit-keys.** The device answers exactly the keys
  `init`, `compute`, `normalize`, `snapshot`, `import`, and `latest-checkpoint`
  (§4). It does **not** install a default/catch-all handler. Every other key —
  including the reserved inspection/mutation/commitment keys (`keys`, `set`,
  `set-path`, `remove`, `id`, `commitments`, `committers`, `committed`, `verify`,
  `commit`) — falls through to the base `message@1.0` device and behaves exactly
  as for any message. An implementation MUST NOT capture those keys.

- **Role.** The device is normally installed as a process's **execution device**
  and invoked by a `process@1.0` orchestrator, which resolves `init` once,
  `compute` once per slot, and `normalize`/`snapshot` as part of state
  save/restore. The orchestrator passes:
  - **`Base`** — the process **state** message (carries the process definition
    under `process`, and the evolving per-slot state including `at-slot`).
  - **`Req`** — the per-step request, carrying at least `path` (the phase key)
    and, for `compute`, the **assignment** fields below.
  - **`Opts`** — node options (carry the CU configuration of §6 and the import
    authorities of §7).

  `import`/`latest-checkpoint` MAY also be invoked directly (e.g.
  `<<"…~genesis-wasm@1.0/import=<id>&process-id=<id>">>`).

- **Compute request shape (`Req`).** On the `compute` path the request carries:

  | Key | Type | Required | Meaning |
  |---|---|---|---|
  | `type` | binary | for an assignment | `Assignment` selects assignment compute; any other value (or absent) is treated as a **dryrun** |
  | `slot` | integer | for an assignment | the slot being computed; identifies the assignment to the CU |
  | `process-id` | binary (43-char base64url id) | conditionally | the process id; used when it cannot be derived from `Base` (§5) |
  | `body` | message | for an assignment | the scheduled message being applied (its `data` is the evaluation input) |

  All keys are **lowercase, hyphenated, binary on the wire**; ids are
  **base64url**, never hex.

- **Import request shape (`Req`).** On the `import`/`latest-checkpoint` path:

  | Key | Type | Required | Meaning |
  |---|---|---|---|
  | `import` | binary (id) | no | a specific checkpoint id to adopt; if absent the device discovers the **latest** trusted checkpoint via the gateway (§4.5) |
  | `process-id` | binary (id) | no | the target process; if absent the process is taken to be `Base` itself |

## 4. Resolved keys (normative)

### `init` (Base, Req → result)
- **Reads:** the base message only.
- **Behaviour (MUST):** Identity. Perform no CU interaction, no network, and no
  state change. Return the base message unchanged.
- **Returns:** `{ok, Base}`.
- **Side effects:** none.

### `compute` (Base, Req → result)
The per-slot computation. It is the composition of an **ensure-started** guard, a
delegated CU call, and a **patch-apply** step, wrapped with **once-only**
deduplication.

- **Reads:** `type`, `slot`, `process-id`, `body` from `Req` (§3); the process
  definition and current `at-slot` from `Base`; the CU configuration and dedup
  seen-set from `Base`/`Opts`.
- **Behaviour (MUST):**
  1. **Ensure the legacy CU is started** (§4.6). If it cannot be made live, return
     `{error, #{ <<"status">> => 500, <<"message">> => Msg }}` (the binary key
     `message`, **not** `body`) and do **not** call the CU. For `compute`/`snapshot`
     `Msg` is the exact binary **`"HyperBEAM was not compiled with genesis-wasm@1.0
     on this node."`**; `normalize` instead uses **`"Genesis-wasm server not
     running."`** (the two paths return different sentinel strings — §8).
  2. **Deduplicate the assignment.** Determine whether this assignment's subject
     (the request `body`) has already been computed in this evaluation stream,
     using the `dedup@1.0` semantics (subject = `body`, first-pass only; see the
     `dedup@1.0` spec). The dedup check threads a seen-set through `Base`.
     - If the subject is **unseen** (or dedup is not applicable, e.g. a dryrun
       with no `body`): proceed to step 3 with the (dedup-updated) state.
     - If the subject is **already seen**: the assignment is a duplicate. The
       device MUST **not** recompute it; instead it re-runs the pipeline as a
       **no-op skip step** that advances the slot without changing process
       results (§4.4). The externally observable effect is that a thrice-assigned
       identical message computes its effect exactly once.
  3. **Delegate the compute to the legacy CU** by resolving the
     `delegated-compute@1.0` behaviour against the (dedup-updated) state with the
     same `Req`. That layer performs the legacy CU HTTP call (§5) and writes the
     decoded results message under the state's `results` key (and the raw decoded
     JSON under `results/raw`). On a CU/transport failure it returns
     `{error, _}`, which this device propagates unchanged.
  4. **Apply the emitted patches.** Resolve the `patch@1.0` behaviour against the
     result with `patch-from = /results/outbox`, lifting every outbox submessage
     that is a patch (`method: PATCH` or `device: patch@1.0`) onto the live state
     (default destination: the top level), per the `patch@1.0` spec. Non-patch
     outbox entries are left in place.
  5. Return the patched state message.
- **Returns:** `{ok, State'}` — the post-compute, post-patch process state
  (with `results` populated and patches applied), or `{error, Error}` if
  ensure-started failed (§4.6) or the delegated CU call failed (§5/§8).
- **Side effects:** may **spawn** the CU sidecar subprocess (first call only,
  §4.6); makes an outbound **HTTP** call to the local CU (§5); the dedup step may
  write a seen-set entry to the content-addressed store (per `dedup@1.0`).

#### 4.4 Duplicate-assignment (skip) handling
When the dedup step reports an already-seen subject, the device MUST re-enter the
compute pipeline against the **exit state** the dedup step returned, with a
rewritten request that:
- preserves the original `path` (default `compute`) and `slot` (default `-1` if
  absent);
- carries `skip = true`;
- records `original-assignment-id` = the signed id of the original request;
- carries a fresh, signed `body` whose only meaningful content is a current
  millisecond `timestamp` (so the no-op step has a distinct, advancing subject).

This produces an empty/no-op computation for the duplicate slot (the slot
advances; results are unchanged). The synthetic body's only field is the binary key
`timestamp` (a millisecond integer), and `original-assignment-id` is the
**`signed`**-selector id of the original request (`hb_message:id(Req, signed)`). If a
re-run *also* reports a skip whose request already carries `skip = true` (a
double-skip), the device MUST abort with a **bare `{error, <current-state>}`** — the
current state IS the error term, **not** a `{error, #{status, message}}` map (it
MUST NOT loop) — see §8.

This behaviour is required so that re-scheduling an identical message (a common
legacy-net occurrence) does not double-apply its effects, while still advancing
the slot cursor so subsequent distinct assignments compute at the correct slot.

### `normalize` (Base, Req → result)
Prepare a (possibly checkpoint-bearing) state for continued computation, ensuring
any embedded snapshot is loaded into the CU.

- **Reads:** the base message (notably a `snapshot` submessage if present); the
  CU configuration from `Opts`.
- **Behaviour (MUST):**
  1. **Ensure the legacy CU is started** (§4.6). If it cannot, return the
     failure-closed `status 500` error ("Genesis-wasm server not running.").
  2. Resolve the `delegated-compute@1.0` `normalize` behaviour against the base
     message with the same request. Per that device: if the base carries a
     `snapshot` whose `type` is `Checkpoint`, the snapshot is **loaded into the
     CU** (the snapshot `data` is POSTed to the CU's state endpoint, §5) and the
     `snapshot` key is stripped from the returned state; if there is no
     checkpoint snapshot the base is returned with any `snapshot` key removed.
- **Returns:** `{ok, State'}` — the normalized state with any embedded snapshot
  removed (and loaded into the CU when it was a checkpoint), or `{error, #{ status
  := 500, message := <binary> }}` if the CU is not running.
- **Side effects:** ensure-started (§4.6); may POST a snapshot body to the CU.

### `snapshot` (Base, Req → result)
Capture the CU's current process memory as a restorable snapshot.

- **Reads:** the base message (to derive the process id); the CU configuration.
- **Behaviour (MUST):**
  1. **Ensure the legacy CU is started** (§4.6). If it cannot, return the
     failure-closed `status 500` error using the **same `compute`/`snapshot`
     sentinel** `"HyperBEAM was not compiled with genesis-wasm@1.0 on this node."`
     (§4.compute step 1 — `snapshot` shares the compute delegate path; it does **not**
     use normalize's `"Genesis-wasm server not running."`).
  2. Resolve the `delegated-compute@1.0` `snapshot` behaviour, which asks the CU
     to produce a checkpoint for this process (§5, `POST /snapshot/<process-id>`)
     and returns the CU's response as the snapshot message. If the CU produces no
     checkpoint, that layer returns a message indicating "No checkpoint
     produced." rather than failing the whole resolution.
- **Returns:** `{ok, Snapshot}` — the CU's snapshot response (a message; its
  `data` is the process-memory blob), or `{error, _}` only if ensure-started
  failed.
- **Side effects:** ensure-started (§4.6); makes an outbound HTTP call to the CU.

### `import` (Base, Req → result)
Adopt a legacy on-chain checkpoint as local process state. Either a specific
checkpoint id is supplied, or the latest trusted one is discovered.

- **Reads:** `import` and `process-id` from `Req`; the import authorities from
  `Opts` (§7); the gateway/data layer (for discovery and checkpoint fetch).
- **Behaviour (MUST):**
  1. **Resolve the target process message.** If `Req` carries `process-id`, read
     that process message from the content-addressed store; else the target is
     `Base`.
  2. **Obtain the checkpoint message:**
     - If `Req` carries `import = <id>`, read that checkpoint message from the
       store. If it is not present, return `not_found` (§8).
     - Else, **discover** the latest trusted checkpoint for the process id via
       `latest-checkpoint` (§4.5). If discovery yields no checkpoint, return its
       error (`no-import-authorities` / `not_found`).
  3. **Validate and adopt** the checkpoint (§4.5 *Validation & adoption*).
- **Returns:** `{ok, State'}` — the target process message augmented with
  `at-slot = <checkpoint nonce>` and `snapshot = <checkpoint message>`; or one of
  the import errors of §8.
- **Side effects:** **writes** the imported state into the content-addressed
  store under the process's compute/latest/restore result paths (§4.5); reads the
  gateway/store. No CU subprocess is started by `import` itself.

### `latest-checkpoint` (Base, Req → result)
Discover the most recent **trusted** legacy checkpoint for a process id. As a
**resolved key** it is invoked `(Base, Req, Opts)` like any device key: the process
id is `Req`'s `process-id` if present, else derived from `Base` (its committed id).
`import` calls the same discovery **internally** with the process id it already
holds (an `(ProcID, Opts)` helper) — an earlier `(ProcID, Opts → result)` heading
named that internal helper, not the resolved-key arity (the two cannot both be the
public signature; the resolved key is `(Base, Req, Opts)`).

- **Reads:** the import authorities from `Opts` (§7); the gateway.
- **Behaviour (MUST):**
  1. If the configured import-authority list is **empty**, return
     `no-import-authorities` (§8). (Discovery is failure-closed: with no trusted
     signer, there is no "latest".)
  2. Otherwise query the Arweave gateway for the single most recent transaction
     (`first: 1`, height-descending) tagged `Type: Checkpoint` and
     `Process: <ProcID>`, restricted to **owners** ∈ the authority list, and
     convert the returned transaction node into a message. If the query errors,
     propagate the error; if there is no matching transaction, return
     `not_found`.
- **Returns:** `{ok, CheckpointMessage}` | `{error, no-import-authorities}` |
  `{error, not_found}` | `{error, <gateway error>}`.
- **Side effects:** a gateway (network) query; no store write, no CU start.

#### 4.5 Checkpoint validation, discovery & adoption (normative)

**Discovery (`latest-checkpoint`)** filters strictly by the trusted **owners**
list at the gateway, so an untrusted signer's checkpoint is never returned by
discovery in the first place.

**Validation & adoption (`import`).** Before adopting a checkpoint
the device MUST verify **all** of the following; any failure aborts with the
corresponding error (§8) and performs **no** store write:

1. **Valid target.** The target process message MUST be a legacy process —
   either its `variant` equals `ao.TN.1`, **or** its `execution-device` equals
   `genesis-wasm@1.0`. Otherwise → `invalid-import-target` (`status 400`).
2. **Trusted signer.** At least one committer (signer) of the checkpoint message
   MUST be a member of the configured import-authority list. Otherwise →
   `untrusted` (`status 400`). (For an explicit `import = <id>` this is the only
   place the allow-list is enforced, since the id bypasses owner-filtered
   discovery.)
3. **Verifiable.** The checkpoint message MUST `verify` (all commitments) under
   `message@1.0`. Otherwise → `unverified` (`status 400`).
4. **Process match.** The checkpoint's `process` field MUST equal the target
   process id. Otherwise → `process-mismatch` (`status 400`).

On success the device:
- reads the checkpoint's `nonce` as the slot (`at-slot`), coercing the binary to an
  integer (default if absent). **Read the checkpoint's fields (`nonce`, `process`,
  `variant`) via the inert `message@1.0` view** (`{as, message@1.0, Checkpoint}` / a
  raw map read), never by resolving through the checkpoint (which may carry its own
  `device`). ⚠ A checkpoint sourced from an Arweave gateway tx carries
  **capitalised** tag names (`Nonce`, `Process`); `message@1.0` does **not** case-fold
  stored keys, so read case-tolerantly.
- produces the adopted state = target process message + `at-slot = <slot>` +
  `snapshot = <checkpoint message>`;
- **writes** that state into the content-addressed store so subsequent resolution
  finds it. The process id keying the result paths is the **`all`-committed id**
  (`hb_message:id(Process, all)` — the full id, matching `process@1.0` §5, **not** the
  `signed` subset). The write goes through the **cache's high-level result-write**,
  which stores the state's **raw content-id value** (preserving its commitments) — it
  MUST NOT use a bare `hb_store:link` for the per-slot/restore indices, which would
  drop the commitments and change the id (the hazard `process@1.0` §5 warns about). It
  writes:
  - a **public** copy (the adopted state with the `snapshot` key removed) under
    the process's `compute@slot=<slot>` result path **and** its `latest` result
    path;
  - the **full** copy (with `snapshot`) under the process's `restore@slot=<slot>`
    and `restore` result paths;
  and returns the full adopted state.

(The split exists so that the heavyweight snapshot blob is reachable only via the
explicit restore paths, while ordinary `latest`/per-slot reads return the lean
public state.)

#### 4.6 Ensure-started (legacy CU lifecycle) — normative

Before any CU-backed key (`compute`, `normalize`, `snapshot`) talks to the CU it
MUST run **ensure-started**, which is **idempotent**:

1. **Liveness probe.** Consider the CU **already running** if **either** the
   CU's status endpoint responds (a `GET /status` to the configured local port
   returns success within a short timeout), **or** the node was compiled with the
   genesis-wasm feature **and** a process is registered under the device name
   `genesis-wasm@1.0`. If running → return success, start nothing. The liveness
   probe is **always attempted first** — it is the leading disjunct, evaluated
   regardless of feature-compile state. The compile gate (step 2) governs only
   whether a *missing* CU may be **started**, never whether the probe runs; an
   off-feature node still issues the `GET /status` probe (it just fails closed
   when the probe does not report a live server).
2. **Compile gate.** If the CU is not running, the device MAY only start it when
   the node is **compiled with genesis-wasm support**. On a node **not** compiled
   with the feature, the CU-backed keys MUST fail closed with `status 500` and a
   message stating HyperBEAM was not compiled with `genesis-wasm@1.0` (for
   `compute`/`snapshot`) or "Genesis-wasm server not running." (for `normalize`)
   — see §8. (Behaviourally: a non-feature node never bridges to a CU.)
3. **Start.** Otherwise, **spawn the sidecar** (a child OS process) and **register
   it** under the device name, then **block until the status endpoint reports
   live** (poll `GET /status` until success). The sidecar is launched via a
   small monitor wrapper that runs the Node server and terminates it when the
   parent node exits, with the process environment of §6.

The probe result MAY be cached per worker for the lifetime of a resolution so the
status endpoint is not hammered; a cached "live" short-circuits the probe.

- **Status-probe timeout.** The liveness `GET /status` uses a **short** timeout
  (on the order of 100 ms); a probe that does not answer in time is treated as
  "not running".
- **Boot wait.** After spawning, the device polls the status endpoint on a short
  interval (on the order of 2 s) until it reports live, then returns success.

## 5. Legacy CU HTTP contract (normative)

All CU interaction is performed by the `delegated-compute@1.0` layer this device
drives; it is pinned here because an independent legacy CU on the other side
depends on byte-level agreement. The base URL is the **local** CU:
`http://localhost:<genesis-wasm-port>` (default port **6363**, §6). Requests are
issued as a relayed HTTP call; the **request body is JSON** with
`Content-Type: application/json` and **caching disabled** (`no-store`,
`no-cache`) — CU calls MUST NOT be served from a result cache. Responses are
AOS2 JSON and are decoded via the `json-iface@1.0` `from` mapping.

| Purpose | Method & path | Request body | Response → mapping |
|---|---|---|---|
| **Liveness** | `GET /status` | — | any 2xx ⇒ "running"; error/timeout ⇒ "not running" (§4.6) |
| **Assignment compute** | `POST /result/<slot>?process-id=<process-id>` | the assignment rendered as an AOS2 message body (the scheduled message for `<slot>`) | the response `body` is the AOS2 result JSON; decoded into a results message and stored under `results` (and `results/raw`) on state |
| **Dryrun** | `POST /dry-run?process-id=<process-id>` | the request message (with `commitments` removed) rendered as the AOS2 "Message" JSON structure | as above; decoded into a results message but state is **not** persisted (read-only) |
| **Snapshot** | `POST /snapshot/<process-id>` | `{}` | the CU's checkpoint response (its `data` is the process-memory blob); becomes the `snapshot` message |
| **Load snapshot** | `POST /state` | the snapshot's `data` blob as the body, with the snapshot's other fields as headers | acknowledgement; used by `normalize` when adopting a checkpoint |

Contract requirements (MUST):

- **Slot in the path.** Assignment compute encodes the integer `slot` in the
  path (`/result/<slot>`); the `process-id` is a query parameter on both
  `/result` and `/dry-run`.
- **Process id.** `<process-id>` is the process's **signed** id (43-char
  base64url). It is derived from the process definition in `Base`; if it cannot
  be derived there, the request's `process-id` is used (§3). Deriving the id MUST
  reject an unverifiable or unsigned process definition (the process id is only
  defined for a verified, signed process) — see §7.
- **Assignment vs dryrun selection.** `type == Assignment` ⇒ the assignment
  compute endpoint with the concrete `slot`; **any other `type` (or none)** ⇒ the
  dryrun endpoint with no slot. A dryrun MUST NOT change persisted process state.
- **Result decoding.** The response body is treated as AOS2 JSON and converted to
  an AO-Core results message by the `json-iface@1.0` `from` mapping (`outbox`,
  `patches`, `data`, …). A transport/HTTP failure surfaces as `{error, _}` and
  fails the compute step (§8); the device does not silently treat a failed CU
  call as an empty success.

## 6. Sidecar configuration & process environment (normative)

The CU sidecar is a Node server started from a fixed server directory (resolved
relative to the node's working directory — a release-mode location, else a build
-tree location, with a working-directory fallback). It is launched through a
monitor wrapper that runs `npm run start` for that directory and is terminated
when the parent node process exits. The device passes the following **process
environment**, each value taken from a node option with the default shown:

| Env var | Source option (default) | Meaning |
|---|---|---|
| `UNIT_MODE` | fixed `hbu` | run the CU in HyperBEAM-unit mode |
| `HB_URL` | `http://localhost:<node-http-port>` | the node's own HTTP base URL (so the CU can call back) |
| `PORT` | `genesis-wasm-port` (default **`6363`**) | the CU's listen port (also the base for all CU calls of §5) |
| `DB_URL` | absolute path under `genesis-wasm-db-dir` (default **`cache-mainnet/genesis-wasm`**) `/genesis-wasm-db` | CU database location |
| `NODE_CONFIG_ENV` | fixed `production` | CU node config environment |
| `DEFAULT_LOG_LEVEL` | `genesis-wasm-log-level` (default **`debug`**) | CU log verbosity |
| `WALLET_FILE` | absolute path of the node's private key (`priv-key-location`) | the wallet the CU signs with |
| `DISABLE_PROCESS_FILE_CHECKPOINT_CREATION` | fixed `false` | enable file checkpoints |
| `PROCESS_MEMORY_FILE_CHECKPOINTS_DIR` | `genesis-wasm-checkpoints-dir` (default **`<db-dir>/checkpoints`**) | checkpoint output dir |
| `PROCESS_MEMORY_CACHE_MAX_SIZE` | `genesis-wasm-memory-cache-max-size` (default **`12_000_000_000`**) | CU in-memory process cache cap |
| `PROCESS_WASM_SUPPORTED_EXTENSIONS` | `genesis-wasm-supported-extensions` (default **`WeaveDrive`**) | enabled WASM extensions |
| `PROCESS_WASM_MEMORY_MAX_LIMIT` | `genesis-wasm-memory-max-limit` (default **`24_000_000_000`**) | per-process WASM memory cap |

The device also **ensures the DB and checkpoint directories exist** before
launch. The numeric env values are passed as the literal strings shown
(underscore digit-group separators are preserved verbatim, not parsed by the
device). The CU's own stdout/stderr are captured and logged line-by-line; this is
informational and not part of the contract.

Additional node options consulted. ⚠ These are **node options** (global-config
precedence): a resolution reached **through the node** (the request-handling layer
above a bare local resolve) reads these with `prefer => global`, so the node's
global config **wins over** any per-message `Opts` override — a caller cannot inject
a different authority list (or port) per request. (A direct in-process resolve does
not inject the flag, but a request that arrives via the node does.) Read them with
`hb_opts:get`, which applies that precedence.

| Option | Default | Used by |
|---|---|---|
| `genesis-wasm-import-authorities` | `[]` **in this spec** — ⚠ but a stock node's `hb_opts` default message **seeds a non-empty list** (one operator address), so on a default node import/discovery is **enabled**, not disabled, and the `no-import-authorities` branch is unreachable without an explicit empty override (which `prefer => global` may shadow). | import/discovery trust allow-list (§7) |
| `genesis_wasm_port` | `6363` | the CU `GET /status` liveness-probe port (§4.6) |

## 7. Security & authority

- **CU is trusted.** The legacy CU sidecar is a **trusted local compute engine**:
  the device sends it assignments and adopts its decoded results and its
  checkpoints as authoritative process state. There is no cryptographic
  verification of the CU's *computation* — trust is established by the CU running
  locally under the node operator's control (the device only starts a CU on a
  node compiled for it, signing with the node's own wallet). A node operator who
  enables genesis-wasm is trusting that sidecar.
- **Determinism caveat.** Results are only as deterministic as the legacy CU. The
  bridge itself injects two non-deterministic elements that implementers MUST be
  aware of: (a) the **dedup no-op step** stamps a wall-clock `timestamp` into the
  synthetic body it computes for a skipped duplicate (§4.4); and (b) the
  ensure-started lifecycle and status timeouts are wall-clock-sensitive. Neither
  changes the *committed result* of a normal assignment, but a reimplementer MUST
  NOT assume bit-for-bit reproducibility of a genesis-wasm process across
  different CU builds.
- **Import is failure-closed.** Adopting external state is gated on a configured
  **import-authority allow-list**: discovery filters by trusted owners, and
  explicit-id import still requires a trusted signer, a verifiable commitment, a
  matching process, and a legacy target (§4.5). An empty allow-list disables
  import entirely (`no-import-authorities`). A checkpoint that fails any check is
  rejected with no store write.
- **Process id requires a signed, verified process.** Deriving the process id
  (needed for every CU call) MUST reject an unsigned or unverifiable process
  definition (it throws rather than computing an id), so the bridge cannot be
  driven against an unauthenticated process.
- **Fall-through commitment surface.** Being an explicit-keys device, all
  commitment/verification operations on a genesis-wasm message are the
  `message@1.0` behaviours; this device mints no commitments of its own on the
  state it returns.

## 8. Errors

| Atom / shape | Condition |
|---|---|
| `{error, #{ status := 500, message := <"…compiled with genesis-wasm@1.0…"> }}` | `compute` (and the shared delegate path) on a node **not** compiled with the genesis-wasm feature and with no live CU (ensure-started returned not-running). |
| `{error, #{ status := 500, message := <"Genesis-wasm server not running."> }}` | `normalize` when ensure-started reports the CU is not running. |
| `invalid-import-target` (`status 400`) | `import`: the target process is neither `variant = ao.TN.1` nor `execution-device = genesis-wasm@1.0`. |
| `untrusted` (`status 400`) | `import`: no committer of the checkpoint is in the import-authority allow-list. |
| `unverified` (`status 400`) | `import`: the checkpoint message does not `verify`. |
| `process-mismatch` (`status 400`) | `import`: the checkpoint's `process` ≠ the target process id. |
| `not_found` | `import` with an explicit `import = <id>` that is absent from the store; or `latest-checkpoint` discovery finding no matching transaction. |
| `no-import-authorities` | `latest-checkpoint`/`import` discovery when the import-authority allow-list is empty. |
| double-skip error (carries current state) | `compute`: a duplicate re-run that *itself* reports a skip whose request already had `skip = true` (§4.4) — aborts rather than looping. |
| propagated `{error, _}` | any CU HTTP/transport failure (decoded by the delegated layer), or any error surfaced by `dedup@1.0`/`patch@1.0`/the gateway/`message@1.0`. Passed up unchanged. |

The `400`/`500` import/lifecycle errors are returned as **messages** carrying a
`status` and a `message`/`body` field (HTTP-shaped), not bare atoms; the
hyphenated atoms above name the internal failure causes that select those
messages. The single bare atom this device originates from `message@1.0` style
is `not_found`; `no-import-authorities` is a hyphenated cause atom.

## 9. Composition

- **As a process execution device (primary use).** A `process@1.0` orchestrator
  installs `genesis-wasm@1.0` as the `execution` slot (explicitly via
  `execution-device`, or by default for a `variant = ao.TN.1` process) and drives
  the standard hooks: `init` at process start, `compute` per slot (the device
  internally chains `dedup@1.0` → `delegated-compute@1.0` → `patch@1.0`),
  `snapshot` to capture restorable state at the orchestrator's snapshot cadence,
  and `normalize` when loading a snapshot back. The orchestrator caches each
  computed slot as an immutable result edge keyed by the process id and slot, so
  the same id serves as a stable handle to the growing interaction history.
- **Bridge stack.** `genesis-wasm@1.0` is the public face; the legacy-protocol
  detail lives in the devices it composes:
  - `delegated-compute@1.0` owns the **CU HTTP contract** (§5) and the
    AOS2 decode (via `json-iface@1.0`), writing results onto `results`.
  - `patch@1.0` lifts the computation's emitted `PATCH` outbox submessages onto
    the live state (`patch-from = /results/outbox`).
  - `dedup@1.0` provides once-only assignment semantics, threading a seen-set
    through the state.
- **Import vs compute lifecycle.** `import`/`latest-checkpoint` are **out-of-band
  state seeding** — they populate the process's cached state from a trusted legacy
  checkpoint **without** running a CU, so a freshly imported process can then be
  computed forward from the imported slot. Attempting to compute a slot **before**
  the imported slot is expected to fail (the snapshot defines the lower bound of
  available state).
- **Direct dryrun.** Resolving `compute` (via the execution face) with a request
  that carries **no** assignment (`type ≠ Assignment`, no `slot`) performs a CU
  **dryrun**: it returns a result (e.g. the outbox/data a handler would produce)
  **without** advancing or persisting state.

## 10. Conformance (normative checklist)

An implementation MUST exhibit all of the following. Items 1–6 and 9–14 are
checkable via public resolution / store reads (against a node compiled for
genesis-wasm with a live CU, or by code review of the unreachable-offline CU
paths); items 7–8 and the lifecycle items are checkable by code review of the
ensure-started / failure-closed paths.

1. **Dispatch.** The device answers exactly `init`, `compute`, `normalize`,
   `snapshot`, `import`, `latest-checkpoint`; every other key (`keys`, `set`,
   `set-path`, `remove`, `id`, `commitments`, `committers`, `committed`, `verify`,
   `commit`, and arbitrary data keys) resolves with the exact `message@1.0`
   behaviour and is **not** captured by this device.
2. **`init` is identity.** Resolving `init` returns the base message unchanged,
   with no CU interaction, network, or state change.
3. **Compute pipeline.** A successful assignment `compute` (a) ensures the CU is
   started, (b) calls the legacy CU for the slot, (c) decodes the AOS2 result
   onto `results`, and (d) lifts `/results/outbox` patches onto state — the
   returned state reflects both the CU result and the applied patches.
4. **Once-only assignment.** Scheduling and computing the **same** message
   multiple times applies its effect **exactly once**; the duplicate occurrences
   advance the slot via a no-op step (`skip = true`) without re-running the
   computation (demonstrable: a value mutated by a thrice-assigned identical
   handler ends at the single-application result).
5. **Double-skip safety.** A duplicate re-run whose request already carries
   `skip = true` MUST abort with an error rather than recurse — the device never
   loops on dedup skips.
6. **Dryrun is read-only.** A `compute` request with `type ≠ Assignment` (no
   `slot`) runs a CU dryrun and returns a result **without** changing the
   process's persisted state (a subsequent read of the same key shows the
   pre-dryrun value).
7. **Failure-closed off-feature.** On a node not compiled with genesis-wasm (and
   no live CU), `compute`/the shared delegate path returns `status 500` with a
   "not compiled with genesis-wasm@1.0" message, and `normalize` returns
   `status 500` "Genesis-wasm server not running." It never **starts** a CU and
   never performs a **delegated CU compute**. Note that ensure-started still
   attempts its liveness `GET /status` probe **first** (per §4.6 step 1 / item 8),
   so this path *does* touch the local status endpoint — which simply fails or
   times out when no server is listening — before failing closed. It is the CU
   bridge **beyond** the probe that is unreached, not the probe itself; do **not**
   skip the liveness probe on an off-feature node.
8. **Ensure-started idempotence & gating.** Ensure-started treats the CU as
   running if `GET /status` answers **or** (feature-compiled ∧ a process is
   registered under the device name); it starts the CU only when feature-compiled
   and not running, registers it, and blocks until `/status` reports live; a
   `/status` probe that does not answer within the short timeout counts as
   not-running.
9. **CU endpoints.** Assignment compute is `POST /result/<slot>?process-id=<id>`;
   dryrun is `POST /dry-run?process-id=<id>`; snapshot is
   `POST /snapshot/<id>`; snapshot-load is `POST /state`; liveness is
   `GET /status` — all against `http://localhost:<genesis-wasm-port>` (default
   6363), with `application/json` bodies and result caching disabled.
10. **Result is failure-propagating.** A CU transport/HTTP failure makes the
    compute step return `{error, _}` (never a silently-empty success).
11. **`normalize` snapshot load.** `normalize` of a state carrying a
    `Checkpoint`-typed `snapshot` loads that snapshot into the CU
    (`POST /state`) and returns the state with the `snapshot` key removed; a
    state with no checkpoint snapshot returns with `snapshot` removed and no CU
    load.
12. **`snapshot` capture.** `snapshot` returns the CU's checkpoint response (its
    `data` is the memory blob); if the CU produces none, the result indicates
    "No checkpoint produced." rather than failing the resolution.
13. **Import validation (failure-closed).** `import` adopts a checkpoint only if
    the target is a legacy process (`variant = ao.TN.1` **or**
    `execution-device = genesis-wasm@1.0`), a committer is in the
    import-authority allow-list, the checkpoint verifies, and its `process`
    matches the target id; each failure returns the corresponding error
    (`invalid-import-target` / `untrusted` / `unverified` / `process-mismatch`,
    `status 400`) and writes nothing.
14. **Import adoption & store writes.** On success `import` returns the target
    process + `at-slot = <checkpoint nonce>` + `snapshot = <checkpoint>`, and
    writes the **public** (snapshot-stripped) state under the process's
    `compute@slot` and `latest` result paths and the **full** state under its
    `restore@slot`/`restore` paths.
15. **Discovery trust.** `latest-checkpoint` returns `no-import-authorities` when
    the allow-list is empty; otherwise it returns only the single most-recent
    checkpoint owned by a trusted authority for the process (or `not_found`).
16. **Encodings.** Process ids, checkpoint ids, committer addresses are
    **base64url** (43-char), never hex; CU request bodies are JSON; the slot is a
    decimal integer in the assignment-compute path.

## 11. Out of scope

- The **internal representation** of process state, the results message, the
  dedup seen-set, links, and the device's own bookkeeping.
- The **AOS2 JSON schema** and the AO-message↔JSON mapping (defined by
  `json-iface@1.0`): this spec only fixes *which* CU endpoints carry *which*
  payloads, not the JSON field layout.
- The **legacy CU's internal computation**, its WASM engine, its persistence
  format, and its checkpoint byte format — the CU is an opaque trusted engine.
- The exact **process-id derivation** and **commitment/verification** mechanics
  (delegated to `message@1.0`).
- The `delegated-compute@1.0`, `patch@1.0`, and `dedup@1.0` device internals
  beyond the observable contracts referenced here.
- The **`process@1.0` orchestration** (scheduling, slot preparation, result-edge
  caching, snapshot cadence) and the node's result-cache/freshness policy.
- The CU sidecar's **build/packaging**, its `npm`/Node runtime, the monitor
  wrapper's shell mechanics, and log formatting.
- Performance, memory footprint, and storage strategy.

## Open questions

- **`compute` HTTP method labelling.** Source comments label the assignment
  compute path "GET method", but the actual CU call is a `POST` to
  `/result/<slot>`. This spec pins the observable behaviour (`POST`); the "GET"
  in the comment is treated as stale and non-normative. A reimplementer should
  confirm against a live legacy CU that `/result` is POST.
- **Dedup no-op timestamp & determinism.** The duplicate-skip step injects a
  wall-clock `timestamp` into a freshly-signed synthetic body (§4.4). This makes
  the *skip step's own subject* non-deterministic across runs. It does not change
  the surviving assignment's committed result, but confirm whether the synthetic
  body's content is ever observed downstream (it should only serve to advance the
  slot as a no-op).
- **`latest-checkpoint` as a public key vs internal helper.** Discovery is
  exposed as a resolvable key and also used internally by `import`. Its argument
  shape (a process id + options, rather than the usual `Base`/`Req`) is unusual
  for a resolved key; a reimplementer should confirm the exact public invocation
  form, or treat it as import-internal only.
- **Status-probe caching across a resolution.** The liveness probe result may be
  memoised per worker for the duration of a resolution. The exact lifetime of
  that memo (and whether a stale "live" could survive a CU crash within one
  resolution) is an implementation concern; the normative contract is only that
  `compute`/`normalize`/`snapshot` each see a live CU or fail closed.
- **Snapshot-load header/body split.** `normalize`'s snapshot load POSTs the
  snapshot `data` as the body and the snapshot's remaining fields as headers to
  `/state`. The exact header set the CU expects is defined by the snapshot
  message shape and the CU; confirm against a live CU which fields are required.
