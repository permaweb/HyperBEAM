# `multipass@1.0` — the repass driver device

- **Device name:** `multipass@1.0`
- **Depends-on:** `message@1.0` (delegated inspection/mutation and key reads), `stack@1.0` (the consumer that interprets the repass signal). Both specs are provided to reimplementers.
- **Status:** Draft

## 1. Overview

`multipass@1.0` is a tiny **control-flow** device whose only job is to ask the
surrounding execution to **run again from the start** until a target number of
passes has been reached. It carries no state of its own beyond the two counters
it reads from the message: a current pass number and a desired pass total. When
the current pass is below the total it emits a **repass signal**; otherwise it
returns the message unchanged.

It is meant to be placed inside a multi-device pipeline (a `stack@1.0`) whose
per-pass semantics require several full sweeps over the constituent devices
before the work is complete — for example, a compute stack where one device
produces inputs that a second device can only consume on a subsequent pass. The
device implements the smallest possible contract: *signal "go around again" or
"done"*. The mechanics of actually re-executing — incrementing the pass counter,
re-entering the first device, the gate that permits this — belong to the
consumer (`stack@1.0`), not to this device.

## 2. Concepts & terminology

- **Pass:** one complete execution sweep of the surrounding pipeline. The
  pipeline tracks which sweep it is on via the message key `pass` (a 1-based
  integer; the first sweep is pass `1`).
- **Pass total / target:** the number of sweeps desired, carried in the message
  key `passes` (an integer ≥ 1). The pipeline stops repassing once `pass`
  reaches `passes`.
- **Repass signal:** a distinguished resolution outcome, **separate from the
  ordinary `ok` outcome**, that tells the surrounding pipeline to increment
  `pass` and re-execute from its first device. In AO-Core resolution this is the
  `pass` outcome of a key resolution (i.e. a result tagged `pass` rather than
  `ok`/`error`); it carries a message payload exactly like an `ok` result does.
  This spec refers to it as the **repass signal** to distinguish it from the
  `pass` *key* (the counter). A device that never emits this signal can never
  cause a repass.
- **Counter source:** both counters are read as ordinary message keys using the
  identity/base-message reading semantics (the `message@1.0` `get` behaviour),
  not via this device's own dispatch. See §4.

The device's internal representation is **out of scope**; only the observable
key/value contract and resolution outcomes below are normative.

## 3. Device interface

- **Dispatch shape:** **default-handler.** The device does not enumerate a fixed
  set of keys. Instead a single catch-all handler answers **every** key, with two
  explicitly delegated exceptions (below). The handler's behaviour depends only
  on the key *name* relative to those exceptions — it does not branch on key
  contents.
- **Delegated keys (MUST):** the keys `keys` and `set` MUST NOT be answered by
  the repass logic. They MUST be delegated to the identity/base-message device
  (`message@1.0`) operating on the same base message:
  - `keys` → the base message's public key listing (the `message@1.0` `keys`
    behaviour).
  - `set` → the base message's deep-merge mutation (the `message@1.0` `set`
    behaviour), using the supplied request message as the set payload.
  Delegation means: produce exactly the result that `message@1.0` would produce
  for that key on that base (and request). This preserves the ability to bind the
  device onto a path, list its keys, and mutate the underlying message without the
  repass logic swallowing those operations.
- **All other keys (MUST):** every key other than `keys` and `set` — including
  the key the surrounding pipeline resolves during a compute sweep (e.g.
  `compute`, or any arbitrary key name) — is answered by the **repass logic** of
  §4. The handler does **not** read or use the resolved key's name beyond
  excluding the two delegated keys; the *same* repass decision is produced for
  any non-delegated key.
- **Message shape:** the device reads two optional keys from the base message:

  | Key | Type | Default | Meaning |
  |---|---|---|---|
  | `passes` | integer ≥ 1 | `1` | target number of sweeps |
  | `pass` | integer ≥ 1 | `1` | current sweep (1-based) |

  Both keys are lowercase binary on the wire. Neither is required; each defaults
  as above when absent. The device writes neither key and adds no keys of its
  own. Any other keys on the base message are ignored by the repass logic and
  passed through untouched in the returned message.

## 4. Resolved keys (normative)

### `keys` — list public keys (delegated)
- **Reads:** the base message.
- **Behaviour:** MUST return exactly what `message@1.0`'s `keys` returns for the
  base message (its public, non-private key listing, excluding `commitments`).
- **Returns:** `{ok, List}` as defined by `message@1.0`.
- **Side effects:** none.

### `set` — deep-merge mutation (delegated)
- **Reads:** the base message; the request message as the set payload.
- **Behaviour:** MUST return exactly what `message@1.0`'s `set` returns for the
  base message and that request (deep-merge by default, commitment-invalidation
  rules, private-key preservation — all as specified by `message@1.0`).
- **Returns:** `{ok, NewMessage}` as defined by `message@1.0`.
- **Side effects:** none beyond those `message@1.0` `set` defines (none external).

### The default handler — the repass decision (every other key)
- **Reads, from the base message only,** using the identity/base-message reading
  semantics (`message@1.0` `get`, i.e. case-insensitive field lookup, private
  keys excluded):
  - `passes` — the target; **default `1`** if absent.
  - `pass` — the current sweep; **default `1`** if absent.
  The request message and node options are **not** consulted for the decision
  (the request's `path`/key name only selects *this* handler vs. the two
  delegated keys; its contents do not affect the outcome).
- **Behaviour (MUST):** Compare the current pass to the target:
  - If `pass < passes` → emit the **repass signal** carrying the base message
    **unchanged**.
  - Otherwise (`pass >= passes`, including the equal case and the all-defaults
    case `1 >= 1`) → return the ordinary `ok` outcome carrying the base message
    **unchanged**.
  The comparison is a strict numeric `<`. The returned/forwarded message MUST be
  the base message with **no modification** — in particular the device MUST NOT
  itself increment `pass`, MUST NOT alter `passes`, and MUST NOT add or remove
  any key. (Incrementing `pass` between sweeps is the consumer's responsibility;
  see §9.)
- **Returns:**
  - Repass case: the **repass signal** `{pass, BaseMessage}` (the resolution
    outcome tagged `pass`, payload = the unchanged base message).
  - Terminal case: `{ok, BaseMessage}` (the unchanged base message).
- **Errors:** none. The handler does not raise; missing counters fall back to
  their defaults, so there is no missing-key error path. (See §8.)
- **Side effects:** none — no cache write, no store write, no commitment, no
  network. The device is pure with respect to the resolution: output is a total
  function of (`pass`, `passes`) on the base message.

## 5. Data formats & encodings

- `pass` and `passes` are integers. On the wire they are encoded as the platform
  encodes integer message values; the device treats them as integers for the
  numeric comparison. An implementation MUST obtain them through the standard
  base-message read path so that whatever scalar/integer normalisation the
  substrate applies to message values is honoured (the same path `message@1.0`
  `get` uses).
- Key names are lowercase binary: `pass`, `passes`, `keys`, `set`.
- The device emits **no** identifiers, commitments, or content-addressed values,
  so there is nothing to canonicalise and no base64url/hex concern.

## 6. Ordering, freshness & caching

- **Determinism:** the outcome is a pure function of the two counter values on
  the base message. Given the same `pass` and `passes`, the device always
  produces the same outcome (repass vs `ok`) and the same (unchanged) payload.
- The device performs **no caching of its own** and consults no clock, options,
  or external state; there are no ordering or tie-break choices to make beyond
  the single strict `<` comparison defined in §4.
- Result-caching of a resolution that *routes through* this device is governed by
  the surrounding substrate/node configuration, not by this device; the device
  neither sets nor inspects cache-control.

## 7. Security & authority

- The device performs no authorisation checks and requires no commitment: any
  caller may resolve any key. It reads only two public counters and returns the
  message unchanged, so it neither inspects nor exposes private keys itself
  (delegated `keys`/`set` honour `message@1.0`'s private-key rules).
- It produces no signatures or commitments and removes none (it never mutates the
  message), so it cannot invalidate an existing commitment via the repass path.
  Mutation only occurs through the delegated `set`, whose commitment semantics are
  exactly `message@1.0`'s.
- **Failure mode:** the repass logic is **failure-free / terminating-by-default**
  in the sense that a *missing* target (`passes` absent → `1`) yields the terminal
  `ok` outcome on the first pass; the default configuration does **not** loop. A
  repass is only ever requested when an explicit `passes > pass` is present. The
  device cannot by itself create an unbounded loop: it requests at most one
  additional sweep per resolution, and the bound is whatever finite `passes`
  value the message carries (the consumer increments `pass` toward it each sweep).

## 8. Errors

This device defines **no error atoms**. Specifically:

- A missing `passes` or `pass` is **not** an error — each defaults to `1`.
- A non-delegated key that is otherwise meaningless to the device is **not** an
  error — every such key produces the repass decision in §4.
- Delegated `keys`/`set` surface whatever errors `message@1.0` defines for those
  keys (e.g. `not_found` is not applicable to `keys`; `set` follows `message@1.0`).
  This device introduces none.

## 9. Composition

This device is designed to be a member of a `stack@1.0` device stack, and its
contract only has effect in that context:

- **Repass interpretation (consumer-side):** when any device in a `stack@1.0`
  stack returns the **repass signal** (`{pass, Message}`), the stack — if its
  multipass behaviour is enabled (the stack's `Allow-Multipass` gate) —
  **increments the `pass` key by one** and **re-executes the stack from its first
  device** with the returned message as the new base. This increment is performed
  by the stack, not by `multipass@1.0`. A stack that does not permit multipass
  treats the repass signal as terminal.
- **Driving the loop:** placing `multipass@1.0` in a stack (typically last) with
  a base message carrying `passes => N` causes the stack to run exactly `N`
  sweeps: on each sweep `multipass@1.0` sees the stack-maintained `pass`
  (starting at `1`, incremented each repass) and emits the repass signal while
  `pass < N`, then emits `ok` on the sweep where `pass == N`, ending the loop.
  Each constituent device therefore executes once per pass — `N` times in total.
- **Independence from deduplication:** if the stack also contains a deduplication
  device (one that suppresses re-execution of an already-seen identical message
  within a pass), `multipass@1.0`'s repass MUST still take effect across passes —
  i.e. a dedup device that fires within one pass does not prevent the next pass
  from re-running the upstream devices. (This is a property the consumer stack and
  dedup device guarantee; `multipass@1.0` contributes to it only by faithfully
  emitting the repass signal each pass until the target is met.)
- **Path binding / mutation:** because `keys` and `set` are delegated to
  `message@1.0`, the device can be bound onto a path and have the underlying
  message listed/mutated without the repass logic interfering — the standard
  requirement for any default-handler device.
- **Composability of the counters:** `pass`/`passes` are ordinary message keys.
  Other devices in the same stack MAY read `pass` to make their own per-pass
  decisions (e.g. "only act on the first pass", "stop after pass 3"); this device
  does not own those keys, it only reads them to decide whether to request another
  sweep.

## 10. Conformance (normative checklist)

An implementation MUST exhibit all of the following, each checkable by resolving
keys against a constructed base message (and, for items 1–2, comparing against a
`message@1.0` base):

1. Resolving `keys` on a `multipass@1.0` message returns the same public key
   listing that `message@1.0` returns for the same message (private keys and
   `commitments` excluded).
2. Resolving `set` on a `multipass@1.0` message, with a request payload, returns
   the same result `message@1.0`'s `set` would produce for that base and payload
   (deep-merge default, commitment-invalidation, private-key preservation).
3. Resolving **any** key other than `keys`/`set` (e.g. `compute`, or an arbitrary
   name) on a message with `passes => P`, `pass => C`:
   - returns the **repass signal** (`{pass, _}`) when `C < P`;
   - returns the ordinary `{ok, _}` outcome when `C >= P`.
   The chosen key name does not change the outcome (any non-delegated key yields
   the same decision for the same `C`/`P`).
4. The payload of the result (in both the repass and terminal cases) is the base
   message **unchanged**: `pass` and `passes` retain their input values and no key
   is added or removed by this device.
5. A message with **no** `passes` key resolves a compute key to `{ok, _}`
   (terminal) on pass `1` — i.e. the default target is `1` and a bare message does
   not request a repass.
6. A message with **no** `pass` key but `passes => P > 1` resolves a compute key
   to the repass signal — i.e. the default current pass is `1`, which is `< P`.
7. The comparison is strict `<`: with `pass => P` and `passes => P` (equal), the
   result is `{ok, _}` (terminal), not a repass.
8. The device performs no cache write, no store write, no commitment, and no
   network call for any resolution (verifiable by code review of an
   unreachable-offline path: the resolution result is a total function of `pass`
   and `passes` with no I/O).
9. In a `stack@1.0` stack with `passes => N`, the stack executes exactly `N`
   sweeps and each constituent device runs `N` times (verifiable by an
   accumulating device whose output records one contribution per pass): the
   device emits the repass signal on passes `1..N-1` and `{ok, _}` on pass `N`.
10. The device defines and returns no error atom of its own under any input
    (missing counters default; meaningless keys repass); only delegated
    `keys`/`set` may surface `message@1.0`'s errors.

## 11. Out of scope

- The **internal representation** of the device and of the message it operates on.
- The mechanism by which the surrounding pipeline acts on the repass signal —
  incrementing `pass`, re-entering the first device, the `Allow-Multipass` gate,
  starting-pass selection — all of which belong to `stack@1.0` (see its spec).
- The semantics of the delegated `keys`/`set` keys beyond "identical to
  `message@1.0`" (see the `message@1.0` spec for their full behaviour).
- The exact integer encoding of message values on the wire (substrate concern).
- Result-caching/freshness of resolutions routed through the device (node/substrate
  configuration concern).
- Performance and storage strategy.
