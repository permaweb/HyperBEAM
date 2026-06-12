# `node-process@1.0` — node-local singleton processes

- **Device name:** `node-process@1.0`
- **Depends-on:** `message@1.0` (base identity device — the excluded keys fall
  through to it; commitment/signing/ID semantics used when a process is spawned),
  `process@1.0` (the device the spawned singleton runs; all compute/scheduling
  on a resolved process is delegated to it), `scheduler@1.0` (the
  default scheduling device of a spawned process; the device the spawn
  `schedule`/`POST` reaches to begin a process's slot sequence). All three specs
  are provided to reimplementers. The registry that names and persists the
  singletons is the node's name-resolution facility addressed as `local-name@1.0`
  (its `lookup`/`register` contract is referenced below; its spec MAY also be
  handed to reimplementers).
- **Status:** Draft

## 1. Overview

`node-process@1.0` implements the **singleton pattern for processes that a node
hosts and manages itself**. A *node process* is a single, well-known process —
identified by a short operator-chosen **name** (e.g. `ledger`, `router2`) rather
than by a content ID — that the node lazily brings into existence on first
access and thereafter reuses. The device's job is purely **resolution and
lifecycle**: given a name, return the one process registered under it, creating
and registering it on first use from a **process definition** the operator
placed in the node's configuration, and then **delegating** every further
operation (scheduling assignments, computing state, reading results) to the
`process@1.0` device that the returned process is bound to.

It sits between a caller and `process@1.0`: a caller never has to know, persist,
or transmit the process's ID — it addresses the process by its node-local name,
and this device maps that name to the live process, instantiating it once if
needed. The name→process binding is durable (it survives node restarts) because
the device stores it in the node's local name registry.

## 2. Concepts & terminology

- **Node process / singleton:** the unique process the node maintains under a
  given **name**. There is at most one process per name per node; repeated
  lookups of the same name return the *same* process (same ID, same content).
- **Name:** an operator-chosen binary key (lowercase, hyphenated on the wire —
  e.g. `ledger2`, `router2`) that identifies a singleton on this node. It is the
  path segment a caller uses to address the singleton (`.../<name>/...`) and the
  key under which the process is registered in the node's local name registry.
- **Process definition (base definition):** the unsigned message, supplied by the
  operator in node configuration, that describes the process to spawn for a name.
  It is a `process@1.0` process message: at minimum it carries
  `device = process@1.0` and the process's `execution-device` and
  `scheduler-device`, plus whatever the chosen execution device needs (e.g. a
  `module`). It is a *template*; the device augments it (§5.2) and signs it before
  the process exists. Out of scope: the meaning of any field consumed by
  `process@1.0` or the execution device — see those specs.
- **Definitions map (`node-processes`):** the node-configuration option, addressed
  as the binary key `node-processes`, mapping each singleton **name** to its
  **process definition**. A name with no entry here has no definition and cannot
  be spawned.
- **Local name registry:** the node's durable, operator-controlled name→value
  store, addressed as `local-name@1.0`. This device uses it to (a) `lookup` a name
  to find an already-spawned singleton's ID and (b) `register` a name→ID binding
  after spawning. The registry's persistence is what makes singletons survive
  reboots. Its `lookup`/`register` semantics are defined by that facility, not
  here.
- **Operator:** the node's controlling identity — the address of the node's
  configured signing key (the node wallet). The operator is the party whose
  configuration (`node-processes`) defines what may be spawned and whose key signs
  a spawned process; it is also added as a `scheduler`/`authority` of every
  spawned singleton (§5.2).
- **Lazy creation:** a singleton is **not** created when its definition is
  configured; it is created on the **first** lookup that does not find it already
  registered (and that is permitted to spawn — §4).

## 3. Device interface

### 3.1 Dispatch shape

**Default-handler.** The device implements **no** explicitly-named resolvable
data keys of its own. It installs a **default handler** that treats the resolved
key (the path segment) as a singleton **name** and returns the corresponding
process (§4, `lookup-by-name`). Resolving `~node-process@1.0/<name>` therefore
yields the process registered under `<name>` (spawning it first if required).

The default handler MUST be installed such that the keys **`set`** and **`keys`**
are **excluded** from it and fall through to the base identity device
(`message@1.0`). These are message-mutation/inspection keys; capturing them as
"names" would break `set`-ing on, or enumerating, a message that has this device
bound. The exclude set is **exactly** `set` and `keys` — matching the sibling
resolver devices `name@1.0` / `local-name@1.0`. Do **not** over-exclude: other
reserved keys (`id`/`commit`/`verify`/`commitments`, …) need no entry here; they
are handled by the normal resolution of the underlying `message@1.0` message.

Because the value the default handler returns is itself a `process@1.0` message
(it carries `device = process@1.0`), **any further path segment after the name is
resolved by `process@1.0`, not by this device** — this is the delegation
mechanism (§4, §9). The device's own contribution ends once it has produced the
named process.

### 3.2 Message shapes

All keys are lowercase, hyphenated, binary on the wire.

- The **base** message this device is bound to is not read for its content; it
  serves only to carry `device = node-process@1.0`. The operative input is the
  **name** (the path segment / requested key).
- The **request** message `Req` for a name lookup MAY carry:
  - `spawn` (boolean, optional, **default `true`**) — whether the device may
    **create** the singleton if it is not already registered. `spawn = false`
    makes the lookup non-creating: an absent singleton yields `not_found` instead
    of being spawned. Any other request keys are ignored by this device (they are
    not part of its contract; a path continuing past the name is consumed by
    `process@1.0`).

- **Node configuration** (read from node options, not from a message on the wire):
  - `node-processes` (map, optional, default empty) — name → process-definition
    map (§2). A name absent here cannot be spawned.
  - `node-process-spawn-codec` (binary, optional, **default `httpsig@1.0`**) — the
    commitment device used to **sign** a spawned process definition (§5.3). (Some
    nodes set this to `ans104@1.0`.)
  - the node's signing key (node wallet) — used to sign the spawned process and to
    derive the operator address added to the process's `scheduler`/`authority`
    and used as the registry `operator` (§5.2, §7).

## 4. Resolved keys (normative)

`Base` is the message bound to `node-process@1.0`; `Req` is the per-step request;
node options supply `node-processes`, `node-process-spawn-codec`, and the node
key.

### default handler — `lookup-by-name`

Signature: `(Name, Base, Req) → {ok, Process} | {error, Reason}` where `Name` is
the resolved key (path segment).

- **Reads:** the requested key `Name`; `spawn` from `Req` (default `true`); the
  local name registry (to find an existing singleton); on spawn, the
  `node-processes` definition for `Name`, the `node-process-spawn-codec`, and the
  node key. Does **not** read `Base` content.
- **Behaviour:**
  1. **Look up** the name in the node's local name registry (`local-name@1.0`
     `lookup` with `key = Name`). Addressed directly (not via `name@1.0`), the
     registry returns the **raw registered value** — the process **ID** string —
     not a loaded message (the `load`/dereference directive belongs to `name@1.0`
     and is inert at this layer). The result is either that process ID, or
     *not-found*.
  2. **If found:** **load** the process message for that ID from the node's content
     store (a content-addressed read by the ID) and return it.
     - **Returns:** `{ok, Process}` — the full `process@1.0` process message
       (carrying `device = process@1.0`), exactly the message that was committed
       and stored at spawn time. This is the singleton; it is the value subsequent
       path segments resolve against (via `process@1.0`).
  3. **If not found:** consult `spawn` in `Req`.
     - `spawn = false` → **Returns** `{error, not_found}`. Nothing is created.
     - `spawn = true` (the default) → **spawn-and-register** the singleton
       (`spawn-register`, below), returning its committed process message on
       success.
- **Side effects:** none on the *found* path beyond a possible registry-cache
  warm-up internal to `local-name@1.0`. On the *spawn* path, the side effects of
  `spawn-register` apply.

### `spawn-register` (the lazy-creation procedure)

Not a separately addressable key — it is the behaviour invoked by the default
handler when a permitted lookup misses. It MUST execute these steps, in order,
for the requested `Name`:

1. **Find the definition.** Look up `Name` in the node's `node-processes` option.
   - If `Name` has **no** definition there → **Return** `{error, not_found}`. The
     singleton cannot be created (there is nothing to create it from). No
     side effects.
2. **Augment** the base definition with the node's address (§5.2): ensure the
   node's operator address is present (last) in the definition's `scheduler` and
   `authority` lists. This yields the *effective* (still unsigned) process
   definition.
3. **Commit (sign)** the augmented definition with the node key, using the
   commitment device named by `node-process-spawn-codec` (default `httpsig@1.0`).
   The result is the **signed process message**; its committed ID is the
   singleton's **process ID** (§5.3).
4. **Initialise the process's sequence.** Resolve a `schedule` request **against
   the signed process as the base** — `#{ path = schedule, method = POST, body =`
   the signed process message itself `}`. Because the base carries
   `device = process@1.0`, this reaches its scheduling device (`scheduler@1.0` by
   default, per `scheduler-device`); the scheduler schedules it, producing the
   process's first **assignment** at **slot 0**. (Behaviourally: the process is now
   live and ordered on its scheduler. For this to land **locally** rather than
   redirect, the node must be a scheduler of the process — see §5.2.) This step
   **must succeed** (yield `{ok, Assignment}`) before step 5; a schedule failure
   propagates and the spawn does **not** proceed to registration.
   - **`type: Process` is not this device's concern.** Whether the definition
     carries `type: Process` is the operator's choice; this device neither injects
     nor requires it (§2's definition fields are the minimum, and `type` is not
     among them). The scheduler assigns **slot 0** regardless — a `type: Process`
     body *additionally* triggers process-registration, but the slot-0 → slot-1
     sequence holds either way.
5. **Register the name → ID binding** in the local name registry: `register` with
   `key = Name` and `value =` the signed process's **ID**. The registry's
   `register` is **operator-gated**, and the gate checks the **signers of the
   register request** against the node operator — so the register request MUST be
   **committed (signed) by the node key** (it does *not* read an `operator` field
   off the request body). The node key's address is, by configuration, the node
   operator. After this, `lookup` of `Name` finds the singleton (§7).
   - **Outcome:** a `register` return of `{ok, _}` is success (proceed to
     **Returns**). An `{error, Err}` return is a **registration failure** → return
     an error message with `status = 500`, `body = <<"Failed to register
     process.">>`, and a `details` field carrying `Err` (the underlying registry
     error).
- **Returns:** `{ok, SignedProcess}` — the committed process message — on success.
  Note this is the **signed** process message (the just-created singleton), which
  on a subsequent lookup is re-fetched from the store as in step 2 of
  `lookup-by-name`; the two are the same logical process (same ID, equal content
  once both are fully loaded and their commitments normalised).
- **Side effects (on success):** signing of a new process; a **scheduler
  assignment** creating the process's slot 0 (durable scheduler state); a content
  store write of the process and assignment; a **name registration** in the local
  name registry (durable, operator-gated) binding `Name` to the process ID. The
  device itself produces exactly one commitment — the signature over the process
  definition (the assignment and registration commitments belong to the
  scheduler/registry).

### Delegated keys (NOT implemented here)

This device does **not** implement `compute`, `now`, `schedule`, `slot`, `push`,
or any other process operation. Once the named process has been produced, those
are answered by **`process@1.0`** (and through it the process's
`scheduler-device`/`execution-device`). A path such as
`~node-process@1.0/<name>/now/results/output/body` is handled by this device only
up to `<name>` (yielding the process); `now/results/output/body` is resolved by
`process@1.0` against that process. Reimplementers MUST NOT re-implement those
semantics here; they MUST ensure the returned process value carries
`device = process@1.0` so the continuation dispatches there (§9).

## 5. Data formats & encodings

### 5.1 Names and option keys

- A **name** is a plain lowercase, hyphenated binary; it is the path segment and
  the registry key. (Case/`-`/`_` folding when used as a registry key follows the
  registry's normalisation — see `local-name@1.0`; this device passes the name
  through.)
- Node-option keys are the binaries `node-processes` and
  `node-process-spawn-codec`. (`-` and `_` are interchangeable in option-key
  lookup; an implementation MAY read them under either spelling, but the
  on-the-wire / node-message form is hyphenated.)

### 5.2 Augmenting the definition (operator address injection)

Before signing, the base definition is augmented so the **node itself** is a
scheduler and an authority of the singleton. Let `A` be the node operator address
— the human-readable (base64url) address of the node's signing key.

For each of the two lists `scheduler` and `authority` in the base definition
(each read as a list of base64url address strings; **absent ⇒ empty list**):

```
result_list = (base_list with every occurrence of A removed) ++ [A]
```

i.e. `A` MUST appear **exactly once and last**; any pre-existing entries (in their
original relative order) precede it, and a duplicate `A` already present is moved
to the end. The augmented definition is the base definition with `scheduler` and
`authority` **set** to these normalised lists (all other fields unchanged). Both
lists are normalised to lists of base64url address strings.

- Rationale (informative): the node must be a scheduler of its own singleton so it
  can drive the process's sequence (step 4), and an authority so its messages are
  accepted by the process. Critically, the node address in `scheduler` is the
  **locality binding**: the scheduler resolves locality from the `scheduler` list
  (then `scheduler-location`), so the node's presence there is what makes step 4
  schedule **locally** rather than 307-redirect. The "remove-then-append" rule
  gives idempotence and a deterministic last position regardless of the template.
- Observability (informative): this transform is applied to the definition
  **before** signing. The resulting list order is **not** reliably recoverable from
  the *committed + scheduled* singleton (commitment + scheduler normalisation may
  re-surface the base list), so "exactly once and last" is a construction rule, not
  a black-box postcondition. What is observable: with no base `scheduler`/
  `authority`, each list is `[A]`; and the node can schedule on, and is an authority
  of, its singleton (steps 4 / §9).

### 5.3 Signing and the process ID

- The augmented definition is committed (signed) with the node key under the
  commitment device `node-process-spawn-codec` (default `httpsig@1.0`). The
  **signed** message is the singleton.
- The singleton's **process ID** is the **committed/signed ID** of that message
  (the ID over its commitments), a 43-character base64url string. This exact ID
  — not the unsigned/content ID — is the `value` registered in the name registry
  and the ID by which the process is later loaded. (Reimplementers MUST use the
  signed ID for both registration and the post-registration store read; using the
  content ID would register/lookup the wrong identifier.)
- The signed process message is stored content-addressed (readable by its ID) as a
  consequence of scheduling/committing it; the registry binds the name to the ID
  as a pointer.
- **Terminology (informative):** the "signed ID" here is the ID over the message's
  commitments. In this substrate that is the **same** value the `process@1.0` spec
  calls the process ID `id(_, all)` (the process/scheduler identity layer keys on
  this committed ID); `signed` and `all` coincide for such a message. Register and
  load by it — not the unsigned/content ID.

### 5.4 Result shapes

- **Found / spawned process:** a `process@1.0` process message (a map carrying
  `device = process@1.0` and the augmented/committed definition fields). The
  device returns it as `{ok, Process}`.
- **Spawn initialisation** (step 4) yields an **assignment** message from the
  scheduler (its shape — e.g. a `slot` — is defined by `scheduler@1.0`); this
  device does not reshape or return it to the caller (the caller receives the
  process, not the assignment).
- **Errors** are either the bare atom `not_found` or a structured error message
  with an integer `status` and a `body` (§8).

## 6. Ordering, freshness & caching

- **Idempotent singleton.** For a fixed name and a fixed definition, the first
  permitted lookup creates the singleton and every later lookup returns the **same
  process** (same ID; equal content once both are fully loaded and their
  commitments normalised). Creation happens at most once; concurrent first-lookups
  MUST converge on a single registered process (the registry is the single source
  of truth for the name→ID binding). The internal mechanism for that convergence
  is out of scope.
- **Lazy, not eager.** A configured-but-never-accessed name has **no** process and
  no scheduler state; nothing is created until a permitted lookup occurs.
- **Determinism of the returned process.** The augmentation rule (§5.2) is
  deterministic, so the same base definition + same node key yield the same
  augmented definition; the signature (and thus the ID) is deterministic for a
  given codec and key over that definition.
- **Mutable at a constant path.** `~node-process@1.0/<name>` is **mutable at a
  constant path**: before first access it is `not_found` (or spawns); after, its
  *state-bearing* delegated reads (e.g. `<name>/now/...`) change as the process
  advances. A node that caches HTTP resolution results MUST be configured so these
  reads are not served stale from a result cache (a node-configuration concern,
  not device behaviour). The name→process binding itself is stable once
  registered.
- **Registry warm-up.** The first lookup MAY cause the registry to populate its
  in-node name index from durable storage; this is a `local-name@1.0` concern and
  does not change observable results.

## 7. Security & authority

- **Operator-defined surface.** What can be spawned is entirely the operator's
  `node-processes` configuration: a name with no definition there is unspawnable
  (`not_found`). The device adds no other source of definitions.
- **Operator-signed processes.** A spawned singleton is **signed by the node key**
  under the configured spawn codec. The operator's address is injected as a
  `scheduler` and `authority` of the process (§5.2). Thus the node vouches for, and
  is an authority over, every singleton it hosts.
- **Operator-gated registration.** The name→ID binding is written through the
  local name registry's **operator-gated** `register`. The gate (`meta@1.0`'s
  operator check) compares the **signers of the register request** against the node
  operator — it reads **no** `operator` field off the request body. So this device
  authorises the write by **committing (signing) the register request with the node
  key**; the node key's address is, by configuration, the node operator. If the
  node has **no** signing key the request is unsigned, and registration proceeds
  only insofar as the registry permits an unclaimed node (see `local-name@1.0`).
  Reimplementers MUST NOT bypass the registry's authority model.
- **Lookup is unauthenticated.** Resolving a name (and, by extension, spawning a
  configured singleton on first access) requires **no** caller commitment. The
  authority that matters is the **operator's** (whose configuration and key define
  and sign the singleton), not the caller's. `spawn = false` lets a caller probe
  for existence without triggering creation.
- **Failure-closed on missing definition.** With no definition for a name, the
  device creates nothing and returns `not_found`; it never fabricates a process
  from caller-supplied data.

## 8. Errors

- `not_found` (bare atom) — returned by the default handler / `lookup-by-name`
  when **either** the name is not registered **and** spawning is disabled
  (`spawn = false`), **or** spawning was attempted but the name has **no**
  definition in `node-processes`. In both cases nothing is created. (The error is
  the platform atom `not_found` — **underscored** — surfaced from the registry /
  cache `not_found`; hyphenation is the wire/binary-key convention, not the atom
  convention.)
- **Registration-failure error** — returned by `spawn-register` when the name
  registration fails after the process was spawned and initialised: a structured
  error message with `status = 500`, `body = <<"Failed to register process.">>`,
  and a `details` field carrying the underlying registry error. (The process may
  have been scheduled/created even though the binding was not recorded; the exact
  `500` status and the exact `body` binary are normative.)
- Errors arising inside the delegated steps — the scheduler `POST` (step 4), the
  signing (step 3), or the registry lookup/registration beyond the mapped 500 —
  propagate as encountered; this device does not re-wrap them except as specified
  above.

## 9. Composition

- **Name → process → delegation.** The device's place in a chain is: it consumes
  exactly one path segment (the name) and returns a `process@1.0` message; every
  segment after the name is dispatched to `process@1.0` because the returned value
  carries `device = process@1.0`. Canonical usages:
  - `GET /~node-process@1.0/<name>` → the singleton process message (spawning on
    first access).
  - `POST /~node-process@1.0/<name>/schedule` (with a committed `body`) → schedules
    a message onto the singleton (handled by the process's scheduler) — used to
    drive the process after creation.
  - `GET /~node-process@1.0/<name>/now/results/output/body` → the singleton's
    current computed result leaf (handled by `process@1.0`/its execution device).
- **Lazy bootstrap on first delegated call.** Because the name is resolved before
  the rest of the path, a *delegated* call (e.g. `.../<name>/schedule`) on a
  never-before-accessed name **also** triggers lazy creation: the device spawns
  and registers the singleton, then the continuation runs against it. The first
  scheduled user message therefore lands on a freshly-bootstrapped process whose
  slot sequence has already been initialised (step 4).
- **Excluded keys delegate to the base device.** `set` and `keys` (and the other
  reserved keys) on a `~node-process@1.0`-bound message reach `message@1.0`, not
  the name resolver, so binding the device onto a path and mutating/inspecting that
  message works.
- **Operator wiring.** Operators configure singletons declaratively under
  `node-processes`; nothing else needs to instantiate them. Patterns in the wild
  point routes/payment recipients at `~node-process@1.0/<name>` and rely on this
  device to materialise the process on demand.

## 10. Conformance (normative checklist)

An implementation MUST exhibit the following externally observable behaviours:

1. The device is a **default-handler** device with **no** named data keys of its
   own; resolving `~node-process@1.0/<name>` treats `<name>` as a singleton name
   and returns the process for it. It **excludes** `set` and `keys` (and does not
   capture the other reserved keys) so they fall through to the base identity
   device.
2. **Lookup of an existing singleton** returns the registered process: a
   `process@1.0` message (`{ok, #{ <<"device">> := <<"process@1.0">>, ... }}`),
   loaded by the **signed** process ID recorded in the local name registry.
3. **Lazy creation:** the first permitted lookup of a name that has a definition
   but no registration **spawns** the process and returns it; a configured name is
   **not** instantiated before such a lookup.
4. **Repeated lookups are idempotent:** two lookups of the same name return the
   **same** process — equal once both are fully loaded and their commitments are
   normalised (same process ID). The singleton is created at most once.
5. **`spawn = false`** makes a missing singleton return the bare atom `not_found`
   and create nothing; **`spawn = true` (the default)** spawns it.
6. **No definition ⇒ `not_found`:** a spawn attempt for a name absent from
   `node-processes` returns `not_found` and creates nothing.
7. **Augmentation:** the spawn injects the node operator address into the
   definition's `scheduler` and `authority` (remove-then-append-last, §5.2) before
   signing — making the node a scheduler (locality) and authority of its singleton.
   Observable postcondition: with **no** base `scheduler`/`authority`, each is `[A]`
   (the node address). (The list order for a *non-empty* base is a pre-commit
   construction rule, not reliably observable on the committed singleton — §5.2.)
8. **Signing & ID:** the spawned process is **signed by the node key** under the
   `node-process-spawn-codec` commitment device (**default `httpsig@1.0`**); the
   value registered for the name, and the ID used to later load the process, is the
   **committed/signed** ID (43-char base64url), not the content ID.
9. **Sequence initialisation:** spawning sends a `schedule` `POST` of the signed
   process to itself, producing the process's first assignment (slot 0) on its
   `scheduler-device` (default `scheduler@1.0`), so the process is live before it
   is returned.
10. **Operator-gated registration:** the name→ID binding is written via the local
    name registry's operator-gated `register`, authorised by **signing the register
    request with the node key** (the gate checks the request's signers, not an
    `operator` field); on registration failure the device returns a structured error
    with `status = 500` and `body = "Failed to register process."` (and a `details`
    field).
11. **Delegation:** the device does **not** answer `compute`/`now`/`schedule`/
    `slot`/etc.; a path continuing past the name (e.g. `<name>/now/results/output/body`)
    is resolved by `process@1.0` against the returned process, and a delegated call
    on a never-accessed name first lazily creates the singleton.
12. **Encodings & atoms:** names and the option keys `node-processes` /
    `node-process-spawn-codec` are lowercase hyphenated binaries; all IDs/addresses
    are base64url, never hex; the `not_found` error atom is **underscored** (the
    atom convention, not the hyphenated wire-key convention); the exact `500` status
    and `"Failed to register process."` body are used on registration failure.

## 11. Out of scope

- The internal representation of the process message, the definitions map, the
  name registry's index, and any links/pointers (only the observable
  name→process resolution, the augmentation rule, the signed-ID registration, and
  the delegation boundary are normative).
- **All `process@1.0` semantics** — scheduling, slot/assignment shapes, `compute`,
  `now`, `push`, execution-device behaviour, result message structure (e.g. what
  `now/results/output/body` evaluates to). This spec constrains only that the
  named process is produced and that those operations are delegated to
  `process@1.0`.
- The **`local-name@1.0`** registry internals — name normalisation, the
  operator/`is-operator` check, durable-store layout, in-node index warm-up, and
  the `load` dereferencing of a registered pointer. This spec constrains only that
  the binding is written via the registry's operator-gated `register` (as the
  operator) and read via its `lookup` (loading the pointer), and that the
  registered value is the signed process ID.
- The **`scheduler@1.0`** mechanics reached by the spawn `POST` (assignment
  production, ordering, slot numbering).
- The cryptographic details of the spawn commitment device (`httpsig@1.0` /
  `ans104@1.0`); only that the process is signed by the node key and identified by
  its signed ID.
- Concurrency/race resolution for simultaneous first-lookups (only the outcome —
  a single registered singleton — is required), persistence/storage strategy, and
  performance.
- The result-cache configuration a node must apply for the mutable-at-constant-path
  delegated reads (§6) — a node-configuration concern.

## Open questions

- **Process-ID stability across rebuilds.** The singleton's identity is the signed
  ID of the augmented definition. Because the signature is over an
  operator-augmented definition and signed with the node key under a configurable
  codec, the same `name` can map to a **different** ID on a node with a different
  key, a different spawn codec, or an edited definition. The binding persists (the
  registry keeps the first ID), but a reimplementer should note that "the singleton
  for `name`" is the *registered* one, not a deterministic function of the name
  alone — re-registration semantics (what happens if the registry is cleared and a
  changed definition is later spawned under the same name) are governed by the
  registry, not pinned here.
- **`spawn = false` vs. no-definition both surface `not_found`.** A non-creating
  lookup of an unregistered name and a spawn attempt for an undefined name return
  the **same** atom `not_found`; they are distinguished only by the `spawn` flag /
  configuration, not by the error value. Flagged as a shape an implementer must
  reproduce for parity.
- **Post-spawn registration failure leaves a live, unbound process.** If spawning
  and sequence-initialisation (steps 3–4) succeed but `register` (step 5) fails, a
  process has been created and scheduled but no name points at it; the caller gets
  the `500` error rather than the process. Whether/how such an orphan is retried or
  garbage-collected is not specified here.
- **Operator address source when the node is keyless.** The augmentation and the
  registry-`operator` both derive from the node's signing key. On a node with no
  key, §5.2 still appends an (effectively empty/placeholder) address and the
  registration omits the operator; the exact behaviour then reduces to the
  registry's unclaimed-node semantics. Reimplementers should defer to
  `local-name@1.0` for the keyless case rather than inventing a fallback here.
