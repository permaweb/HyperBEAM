# `metering@1.0` — compute/resource metering and cost accounting

- **Device name:** `metering@1.0`
- **Depends-on:** `message@1.0` (base device for unhandled keys). Relates to `p4@1.0` (the payment/charging framework that drives this device as its pricing device). The `p4@1.0` spec is provided to reimplementers.
- **Status:** Draft

## 1. Overview

`metering@1.0` is a **dynamic pricing device** for the `p4@1.0` payment
framework. Over the lifetime of a single request/response, it accumulates the
**resource usage** charged to that request in a **process-local metering
session**, then converts the accumulated usage into a single integer **price**
denominated in the node's payment token, using operator-configured per-resource
**rates**. It plays the role of `p4@1.0`'s *pricing device*: `p4@1.0` calls its
`estimate` key at the start of a charged request to open the session, other
devices accumulate usage into the session as they do work, and `p4@1.0` calls
its `price` key at the end to close the session and obtain the amount to charge.

Usage is recorded per named **resource** (e.g. `arweave-bytes`,
`beam-reductions`). The device also automatically meters the BEAM-reduction work
performed between session open and close, so a node MAY price raw compute even
when no other device contributes a resource.

## 2. Concepts & terminology

- **Metering session:** an open accounting context bound to the *current
  resolution context* (the process/worker resolving the request). At most one
  session is open at a time in that context. A session holds a set of resource
  meters and the reduction count captured when it opened. Sessions do not nest:
  opening a session replaces any session already open in the context.

- **Resource:** a **normalized key** naming a kind of consumable work or output
  being metered — a binary, lowercase, hyphenated key (e.g. `arweave-bytes`,
  `beam-reductions`). A resource name is coerced to its binary form before use as
  a meter key (see §5); it is **not** case- or separator-folded, so distinct
  spellings are distinct meters and a caller MUST use one canonical spelling.

- **Amount:** a non-negative integer quantity of a resource consumed by one
  consume operation. Amounts for the same resource accumulate additively within a
  session.

- **Meter:** the running total amount accumulated for one resource within the
  current session.

- **Rate:** an integer number of **payment-token units per one unit of a
  resource**, configured by the operator (see §3, `metering-rates`). A resource
  with no configured rate has rate `0` and therefore contributes nothing to the
  price.

- **Price:** the final integer charge for the session: the sum, over every
  resource metered in the session, of `meter × rate`. The price is denominated in
  the same integer token units that `p4@1.0`'s ledger uses; this device assigns
  no other meaning to the unit.

- **`beam-reductions`:** the reserved resource name under which the device meters
  raw compute. The amount is the increase in the resolving context's
  Erlang/BEAM *reduction* counter (a monotonic count of executed work units)
  between session open and session close. This is an implementation-observable
  measure of compute; a reimplementation on another substrate MUST meter *its
  own* substrate's analogous monotonic work counter under this same key (see §6
  and §11).

## 3. Device interface

- **Dispatch shape:** **explicit-keys.** The device answers exactly two resolved
  keys: `estimate` and `price`. Any other key (including `consume` and any
  message-manipulation key such as `keys`/`set`/`set-path`/`remove`) MUST NOT be
  answered by this device and MUST fall through to the base `message@1.0` device.
  In particular, attempting to resolve a path ending in `consume` MUST NOT
  perform a consume operation; it resolves as an ordinary (absent) key and
  therefore yields an error / not-found via the base device (see §4, the consume
  operation, and §8).

- **The consume operation (non-resolved helper):** the device additionally
  exposes a **consume** operation that is **not** a resolved key and is **not**
  reachable through any AO-Core resolution path. It is invoked by *loading the
  metering device by name and calling its consume operation directly* — the
  mechanism the build-device skill describes for reaching a device's non-key
  exported helper. Other devices (e.g. an uploader/bundler device) call the
  metering device's consume operation to record the resources they consume. The
  operation and its contract are normative and specified in §4.

- **Active-session predicate (non-resolved helper):** the device also exposes a
  boolean predicate reporting whether the current context has an open metering
  session. Like consume, it is not a resolved key. Its existence is normative
  (§4) but callers need not consult it: consume is a no-op outside a session, so
  callers MAY call consume unconditionally.

- **Message shapes:**
  - `estimate` and `price` are invoked by `p4@1.0` with a request message whose
    contents this device **ignores** (it reads no fields from `Base` or `Req` for
    these keys); it reads only the node option `metering-rates` (for `price`).
  - The consume operation takes a **resource name** plus an **amount**, where the
    amount is supplied either as a bare integer or inside a request message under
    the key `amount` (integer; default `0` when absent). See §4.

## 4. Resolved keys (normative)

### `estimate`

- **Reads:** nothing from `Base` or `Req`. Captures the current resolving
  context's reduction counter.
- **Behaviour:** Open a fresh metering session in the current context. The new
  session MUST start with an **empty** set of resource meters and MUST record the
  context's current reduction count as the session's start point. If a session
  was already open in this context, it is **replaced** (its prior meters and
  start point are discarded). After this call, the active-session predicate MUST
  report active, and subsequent consume operations accumulate into this session.
- **Returns:** `{ok, 0}` — always the integer `0`. (`estimate` reserves the
  *up-front* estimated price; this device always estimates `0` and defers the
  whole charge to `price`.)
- **Side effects:** Mutates process/context-local metering state only. No cache,
  store, commitment, or network effects.

### `price`

- **Reads:** the node option `metering-rates` (default: empty map). Reads no
  fields from `Base` or `Req`. Captures the current reduction counter to finalise
  the `beam-reductions` meter.
- **Behaviour:** Close the current session and compute the final integer price.
  1. Finalise the `beam-reductions` meter: add to it the value
     `max(0, current_reductions − session_start_reductions)` — the reduction work
     performed since `estimate` (clamped at `0`; never negative). If no session is
     open, treat the session as having an empty meter set (price computes over no
     resources; see below).
  2. Compute `price = Σ over each resource R metered in the session of
     meter(R) × rate(R)`, where `rate(R)` is the integer value of
     `metering-rates[R]`, or `0` if `R` is absent from `metering-rates`. The rate
     value MUST be coerced to an integer. Summation starts from `0`.
  3. Close the session: after `price`, the context MUST have **no** open session
     (the active-session predicate reports inactive and the meters are
     discarded), so a subsequent `price` without an intervening `estimate`
     returns `0`.
- **Returns:** `{ok, Price}` where `Price` is a non-negative integer (it is a sum
  of non-negative `amount × rate` terms; rates are operator-configured and
  expected non-negative). No `{ok, ...}` wrapper of a non-integer.
- **Side effects:** Mutates process/context-local metering state only (clears the
  session). No cache, store, commitment, or network effects.

### The consume operation (non-resolved helper) — normative

- **Invocation:** Reached by loading the metering device by name and calling its
  consume operation directly as an **Erlang export with three arguments** —
  `consume(ResourceName, AmountOrRequestMessage, Opts)` — NOT by resolving a path.
  (`metering@1.0/consume` MUST NOT perform it — see §3 and §8.) This three-argument
  calling convention is the cross-device contract: platform resource consumers
  (e.g. the bundler, charging `arweave-bytes`) invoke it fire-and-forget on the
  configured pricing device, so the name, arity, and argument order are fixed.
- **Inputs:**
  - a **resource name** (any key; normalized per §5 before use as a meter key);
  - an **amount**, supplied either as
    - a request **message**, from which the integer under key `amount` is read
      (default `0` if `amount` is absent), or
    - a bare integer amount.
  - node options (used only for link-aware reads of the `amount` field).
- **Behaviour:**
  1. If **no** session is open in the current context, the operation is a
     **no-op** and MUST succeed (return the success sentinel `ok`). Callers
     therefore never need to check whether metering is active.
  2. If a session is open:
     a. Coerce the amount to an integer.
     b. If the amount is `< 0`, the operation MUST fail with an
        **invalid-amount** error carrying the offending amount (see §8). The
        session MUST be left unchanged.
     c. Otherwise add the amount to the meter for the normalized resource name:
        `meter(R) ← meter(R) + amount` (a previously unmetered resource starts
        from `0`). An amount of `0` is permitted and leaves the running total
        unchanged (but is still a successful, in-session operation). Return the
        success sentinel `ok`.
- **Returns:** `ok` on success (both the no-op and the in-session add); an
  invalid-amount error on a negative amount.
- **Side effects:** Mutates process/context-local metering state only. No cache,
  store, commitment, or network effects.

### Active-session predicate (non-resolved helper) — normative

- **Invocation:** Reached by loading the metering device by name; NOT a resolved
  key. This predicate is **introspection-only**: no other device in the current
  platform invokes it (consume's no-op-outside-a-session behaviour means callers
  never need to consult it), so — unlike `consume/3` — its exact Erlang name and
  arity are not a load-bearing cross-device contract. An implementation SHOULD
  still provide it for testing/debugging.
- **Behaviour:** Return `true` iff a metering session is currently open in the
  calling context (i.e. `estimate` has run and `price` has not yet closed it),
  else `false`.
- **Side effects:** None.

## 5. Data formats & encodings

- **Resource keys** are binary, lowercase, hyphenated keys (e.g. `arweave-bytes`,
  `beam-reductions`). Before a resource name is used as a meter key — in the
  consume operation — it is passed through the substrate's standard key
  normalization. That normalization coerces a **non-binary** key (an atom or
  integer) to its binary form, but passes a **binary** key through **unchanged**:
  it does **NOT** case-fold and does **NOT** separator-normalize. Consequently,
  binary resource names that differ only in case or separator (e.g.
  `arweave-bytes`, `Arweave-Bytes`, `arweave_bytes`) are **distinct** meters and
  do **not** accumulate together; a caller MUST use a single canonical spelling.
  The platform's own resource names are already canonical lower-hyphenated
  binaries. The reserved compute resource key is exactly `beam-reductions`.

- **Amounts, meters, rates, prices** are all **integers**. Amount and rate values
  read from messages/options MUST be coerced to integers via the standard
  integer-coercion path (the same coercion used elsewhere for `Opts`/request
  scalars); a binary like `<<"3">>` and the integer `3` are equivalent. There is
  no fractional unit.

- **`metering-rates`** is a node option (read from the run/node `Opts`): a map
  from resource key → integer rate (payment-token units per resource unit).
  Default when unset: the empty map (so every resource has rate `0`).

- This device performs no commitments, no IDs, and no content addressing; it has
  no wire format of its own beyond the integer values returned by `estimate` and
  `price`.

## 6. Ordering, freshness & caching

- **Determinism of `price` given the session:** for a *fixed* set of accumulated
  resource meters and a fixed `metering-rates`, `price` is a deterministic
  function — `Σ meter(R) × rate(R)` — independent of the order in which resources
  were consumed (addition is commutative/associative) and independent of meter
  iteration order.

- **`beam-reductions` is environment-dependent and NON-deterministic.** The
  reduction delta measures actual executed work in the resolving context and
  WILL vary between runs, implementations, and machines for the "same" logical
  request. Consequently a session that prices `beam-reductions` at a non-zero
  rate is **not** reproducible across nodes. A node that requires reproducible /
  agreed pricing across peers MUST set the `beam-reductions` rate to `0` (pricing
  only deterministic, explicitly-consumed resources such as `arweave-bytes`).
  This is the device's only source of non-determinism; all other resources are
  exactly the amounts callers consumed.

- **No result caching of its own:** the device caches nothing and stores nothing.
  Its outputs depend on mutable process-local session state plus a node option,
  not on content-addressed inputs, so its `estimate`/`price` results MUST NOT be
  served from an AO-Core result cache as if pure. (When driven by `p4@1.0` this is
  immaterial — `p4@1.0` invokes the keys directly within the request lifecycle.)

- **Session lifetime is one request:** a session is meaningful only between an
  `estimate` and the matching `price` in the same resolving context. The device
  does not persist sessions across requests, contexts, or restarts.

## 7. Security & authority

- **Operator-controlled pricing.** Rates come solely from the node option
  `metering-rates`, set by the operator. No request field can set or override a
  rate. A request cannot raise or lower its own price except by causing more or
  less resource consumption.

- **Fail-safe accumulation.** The consume operation is a no-op outside a session,
  so a device that meters resources cannot error merely because metering is
  disabled or not yet started. The only consume failure is a programming error
  (a negative amount), which is reported, not swallowed.

- **No trust surface of its own.** The device issues no commitments and verifies
  none; authority over whether a request is charged, and over the ledger, belongs
  to `p4@1.0` and its ledger device. This device only computes an amount.

- **Context isolation.** The session is local to the resolving context; one
  request's meters never leak into another's. (Opening a session replaces any
  stale session in the same context, so a leftover session from a prior, aborted
  request cannot corrupt a fresh `estimate`.)

## 8. Errors

- **invalid-amount** — the consume operation was called, within an active
  session, with a **negative** amount. The operation MUST **reject** it: it does
  **not** meter the amount and leaves the session unchanged, and it signals an
  invalid-amount condition carrying the offending amount value. The signalling
  **mechanism is implementation-defined and not load-bearing** — no caller observes
  it (consume is invoked fire-and-forget with non-negative amounts), so an
  implementation MAY raise an Erlang error/exception **or** return an `{error, _}`
  value (the reference raises). What is normative is only that the negative amount
  is rejected (not metered, session intact) rather than silently treated as a
  successful add. (Outside a session a negative amount is never inspected — the
  operation no-ops before coercion.)

- **(base-device) not-found / resolution error** — resolving any key other than
  `estimate` or `price` against this device (notably `consume`) does not reach a
  metering behaviour; it falls through to the base `message@1.0` device and
  yields whatever that device returns for an absent key (an error / `not_found`).
  An implementation MUST NOT expose `consume` as a resolvable key.

This device returns no other errors of its own. `estimate` and `price` always
succeed (returning `{ok, integer}`).

## 9. Composition

- **As a `p4@1.0` pricing device.** A processor is configured with
  `pricing-device => metering@1.0` (alongside `p4@1.0`'s ledger device). During
  the charged lifecycle, `p4@1.0` resolves `estimate` on this device at the start
  (opening the session) and `price` at the end (closing it and obtaining the
  integer charge), then debits that amount via its ledger device. `estimate`'s
  `0` return means no up-front hold is placed; the full charge is computed at
  `price`. End-to-end, a request that consumes `N` units of resource `R` (and
  whatever `beam-reductions` the work costs) is charged
  `Σ meter(R) × metering-rates[R]` against the requester's balance.

- **Resource producers call consume.** Any device that produces a chargeable
  resource (for example, a device that uploads `arweave-bytes`) records its usage
  by calling the metering device's consume operation with the resource name and
  amount, *unconditionally* — consume no-ops when no `p4@1.0`/metering session is
  active, so the producer needs no awareness of whether pricing is enabled. This
  is the integration seam between arbitrary work-performing devices and the
  pricing device.

- **Automatic compute metering.** Because `price` always folds in the
  `beam-reductions` delta, the device meters raw compute with no cooperation from
  any other device; pricing it is opt-in via the `beam-reductions` rate.

## 10. Conformance (normative checklist)

An implementation MUST exhibit all of the following:

1. The device answers exactly the resolved keys `estimate` and `price`. Resolving
   any other key — in particular `consume` (e.g. a request with `path = consume`,
   `resource`, `amount`) — does NOT perform a metering action and returns an
   error / not-found via the base device.
2. `estimate` returns `{ok, 0}` and opens a metering session: immediately after,
   the active-session predicate is `true`, the meter set is empty, and the
   session has captured the context's start reduction count.
3. A second `estimate` while a session is open replaces it: prior meters are
   discarded and the start reduction count is re-captured.
4. The consume operation is **not** reachable by AO-Core resolution; it is invoked
   only by loading the device by name and calling the operation directly.
5. Consume called with **no** active session is a no-op that succeeds (`ok`) and
   does not create a session (active-session predicate remains `false`).
6. Consume called with an active session adds the (integer-coerced) amount to the
   meter for the **normalized** resource key, accumulating additively across
   repeated calls; an amount supplied via a request message is read from the
   `amount` field (default `0` when absent); a bare-integer amount is accepted
   directly.
7. Consume with a **negative** amount, within an active session, fails with the
   **invalid-amount** error carrying the amount, and leaves the session unchanged.
8. `price` returns `{ok, P}` where `P = Σ over metered resources of
   meter × rate`, `rate` taken from the node option `metering-rates` (integer-
   coerced), defaulting to `0` for any resource absent from `metering-rates`;
   summation starts at `0`. With an empty rate map (or all rates `0`), `P = 0`.
   The result is order-independent of the consume sequence.
9. `price` folds in a `beam-reductions` meter equal to the prior `beam-reductions`
   total plus `max(0, current − start)` reductions; with a non-zero
   `beam-reductions` rate and a session in which real compute occurred between
   `estimate` and `price`, `P > 0` even if no other resource was consumed.
10. `price` closes the session: afterwards the active-session predicate is `false`
    and the meters are gone; a `price` with no open session returns `{ok, 0}`.
11. Neither `estimate`, `price`, nor consume performs any cache write, store
    write, commitment, or network call; their only effect is on context-local
    metering state.
12. No request field can set or override a rate; rates derive solely from the
    `metering-rates` node option.

## 11. Out of scope

- The internal representation of the session, meters, and start reductions — any
  **context-local** store that survives across the request's *independent*
  `estimate` / `consume` / `price` calls (e.g. the process dictionary, or ETS keyed
  by the worker) is permitted. (A value threaded through a message map is **not**
  viable: these helpers are independent entry points — the fire-and-forget
  `consume` in particular receives no shared map — so there is no map to ride on.)
- The exact mechanism by which the device is "loaded by name" so its consume /
  active-session helpers can be called — that is substrate machinery described by
  the build-device skill, not protocol behaviour.
- The substrate-specific definition of a "reduction." On a non-BEAM substrate the
  implementer MUST meter an analogous monotonic compute counter under
  `beam-reductions`; the precise counter is environment-defined. Its
  non-determinism is acknowledged (§6); pricing it for reproducible cross-node
  agreement is the operator's responsibility (set the rate to `0`).
- The behaviour of `p4@1.0` itself — when `estimate`/`price` are called, how the
  charge is applied to a ledger, balances, and the request/response hook wiring.
  See the `p4@1.0` spec.
- The token / unit semantics of the price beyond "a non-negative integer in the
  ledger's units."
- Performance and storage strategy.

## Open questions

- **Error-atom spelling.** The reference implementation raises its negative-amount
  error with a non-hyphenated atom (`invalid_meter_amount`) carrying the amount,
  via a raised error rather than an `{error, _}` return. This spec normalises the
  *condition* and names it **invalid-amount** to follow the hyphenated-error-atom
  convention; an implementer reproducing the reference byte-for-byte would use the
  underscored atom. Whether the canonical surface should be a hyphenated
  `{error, invalid-amount}` tuple or the raised underscored atom is unresolved —
  callers should treat "negative amount within a session" as the contract and not
  depend on the exact atom or raise/return form.
- **Rate sign.** Rates are assumed non-negative (the device does not validate
  them); a negative configured rate would yield a negative price contribution.
  The reference neither rejects nor clamps negative rates. Whether negative rates
  are legal is unspecified; operators SHOULD configure only non-negative rates.
- **Concurrent resolution within one context.** The session is bound to the
  resolving context; the spec assumes `estimate`/consume/`price` for one charged
  request all execute in that same single context. Behaviour if work for a metered
  request is split across multiple contexts (so consume runs in a different
  context than `estimate`) is undefined — those consume calls would see no session
  and no-op. `p4@1.0`'s lifecycle is assumed to keep them co-located.
