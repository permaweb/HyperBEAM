# Device: ~rac@1.0

`~rac@1.0` (Ratcheting Authenticated Channels) provides ordered, exactly-once,
replay-resistant messaging between AO processes.

## Overview

It is often necessary for an AO process to consume a stream of authenticated
messages — events, or state updates — from another process, applying each at
most once and in a well-defined order.

`~rac@1.0` addresses this by numbering messages rather than identifying them. A
sender assigns each message a monotonic *slot* on a named *channel*; a recipient
maintains a per-sender *ratchet* and admits a message only when its slot
satisfies the channel's ingest rule, rejecting replays and mis-ordered delivery.

## Core concept: channels, slots, and the ratchet

A **channel** is a named, unidirectional stream from a sending process to a
receiving process. Channels are independent and their slots begin at `0`.

- The **sender** records the highest slot it has emitted on each channel and
  stamps the next slot onto each outbound message.
- The **recipient** records the highest slot it has admitted from each sender on
  each channel — the **ratchet** — and advances it as messages are admitted.

A message carries its slot, its channel, and the *ingest rule* under which the
recipient may admit it.

## State

| Location | Meaning |
|---|---|
| `base/rac-outbound/<address>/<channel>` | Highest slot this process has sent to `<address>` on `<channel>`. Absent ⇒ `-1`; the next slot emitted is `0`. |
| `base/rac-inbound/<address>/<channel>` | Highest slot this process has admitted from `<address>` on `<channel>` (the ratchet). Absent ⇒ `-1`. |
| message key `rac-slot` | The channel slot this message occupies. |
| message key `rac-channel` | The channel name. Absent ⇒ `default`. |
| message key `rac-ratchet` | The ingest rule (see `compute`). Absent ⇒ `false`. |

`<address>` is the *counterparty's* process identifier: on the sending side, the
recipient's address; on the receiving side, the sender's address, taken from the
inbound message's `from-process` field (established by `~push@1.0`, and not
settable by the sending process's own logic).

`rac-outbound` and `rac-inbound` are the only state this device introduces.

## Keys

### `send`

**Action:** Emit `body` to `recipient` on a channel, stamping it with the
channel's next slot and appending it to the process outbox for delivery.

**Inputs (Request):**

- `recipient` — (required) address of the destination process.
- `body` — (required) the message to send.
- `channel` — (optional, default `default`) channel name.
- `ratchet` — (optional, default `false`) ingest rule to stamp on the message:
  `false`, `true`, or a non-negative integer (see `compute`).

**Behaviour:**

1. `Slot = base/rac-outbound/<recipient>/<channel>` + 1 (a missing counter is
   `-1`, so the first slot is `0`).
2. Stamp the outbound message with `rac-slot = Slot` and `target = recipient`,
   plus `rac-channel`/`rac-ratchet` when they differ from their defaults.
   Existing top-level RAC control keys, `target`, and `from-*` provenance keys
   in `body` are replaced or removed before stamping.
3. Append the stamped message to the outbox (`results/outbox`) at one past the
   highest canonical positive numeric key; sparse gaps and noncanonical,
   nonpositive, or non-numeric keys are not reused.
4. Set `base/rac-outbound/<recipient>/<channel> = Slot`.

**Response:** `{ok, <base>}` — the base with the outbox entry appended and the
outbound counter advanced.

Delivery, provenance (`from-process`), and any re-signing to the recipient's
policy are performed by `~push@1.0` when it drains the outbox; `~rac@1.0` only
stamps and enqueues.

### `compute`

**Action:** Admit or reject one inbound message according to its channel's
ingest rule, advancing the ratchet on admission.

`compute` is intended to run once per assigned message, ahead of application
logic. It is `~multipass@1.0`-aware, mirroring `~dedup@1.0`: it acts only on the
first pass and returns the base unchanged on later passes.

**Inputs:** the base is the process state; the inbound message is the request's
`body`, from which `compute` reads `rac-slot`, `rac-channel` (default
`default`), `rac-ratchet` (default `false`), and the sender (`from-process`).
Tagged RAC messages without process provenance are rejected.

**Behaviour:**

1. On any pass other than the first → `{ok, <base>}`.
2. If the inbound message has no `rac-slot` → `{ok, <base>}` (untagged traffic
   passes through unmodified).
3. Let `Ratchet = base/rac-inbound/<sender>/<channel>` (absent ⇒ `-1`) and
   `Slot = rac-slot`. Admit the message iff the ingest rule holds:

   | `rac-ratchet` | Admit when | Semantics |
   |---|---|---|
   | `false` (default) | `Slot == Ratchet + 1` | strict, in-order, exactly-once event ingest |
   | `true` | `Slot > Ratchet` | ratchet forward to any later slot (last-writer-wins sync) |
   | integer `N` | `Slot > Ratchet` and `Ratchet >= N` | ratchet forward, but only once the ratchet has reached `N` |

4. **Admit:** set `base/rac-inbound/<sender>/<channel> = Slot`, return
   `{ok, <base>}` (application logic proceeds). **Reject:** return
   `{skip, <base>}` — state is unchanged; in a stack, no subsequent device runs
   for this message.

*Example (integer rule).* With `rac-inbound/<sender>/default = 5`, a message
`rac-slot = 8, rac-ratchet = 6` is rejected (ratchet has not reached `6`); once
the ratchet reaches `6` the same message is admitted and jumps it to `8`. With
`rac-ratchet = true` it would be admitted immediately. In either case, once the
`rac-slot = 8` message is admitted the ratchet stands at `8`, so a later
`rac-slot = 7` is never admissible.

## Usage

`~rac@1.0` can be composed two ways:

- **In an execution stack.** Place `~rac@1.0` ahead of the application device
  (and ahead of `~dedup@1.0`, if also used) in a process's execution stack. A
  `{skip, ...}` from `compute` halts the stack for that message, so the
  application device never observes a rejected message.
- **Called directly.** A custom execution device resolves `compute` against
  `{as, ~rac@1.0, <base>}` before running its own logic, treating a `skip`
  result as "reject this message." A sender likewise resolves `send` against
  `{as, ~rac@1.0, <base>}` from within its own compute.

Both use-cases are expressed through `rac-ratchet`: `false` gives ordered,
exactly-once *event* ingest; `true` or an integer gives last-writer-wins *state
synchronization* that tolerates gaps.

## Properties

- **Replay resistance.** A message whose slot fails its ingest rule is rejected;
  application state advances at most once per admitted slot.
- **Ordering.** Under `false`, messages are admitted strictly in emission order,
  per channel.
- **Attribution.** The ratchet is keyed on the counterparty's process identity
  (`from-process`); its integrity is enforced by the `~security@1.0` device or
  another mechanism, not by `~rac@1.0` itself.
- **Determinism.** `send` and `compute` are pure functions of the base and the
  message; no wall-clock, randomness, or node configuration affects their
  output.

## Conformance

The following behaviours are required. The reference test suite exercises them
through `~lua@5.3a` scripts that resolve `send` and `compute` against
`{as, ~rac@1.0, <base>}` directly, and through a `~process@1.0` whose execution
stack folds `~rac@1.0` ahead of an application device.

1. **Stamp & advance.** Successive `send`s on a channel carry `rac-slot`
   `0, 1, 2, …`, and `rac-outbound/<recipient>/<channel>` tracks the last.
2. **Channel independence.** Sends on different `channel`s, or to different
   recipients, keep independent counters.
3. **Ordered ingest (`false`).** Slots `0, 1, 2` are admitted in order; the
   ratchet ends at `2`.
4. **Replay rejected.** Re-submitting an admitted slot — including a re-committed
   copy with a fresh message id — yields `skip`; state is unchanged.
5. **Gap rejected (`false`).** Slot `2` while the ratchet is at `0` yields
   `skip`; then `1` and `2` are admitted in order.
6. **Ratchet jump (`true`).** From a fresh channel, a `true` message with slot
   `5` is admitted (ratchet jumps to `5`); any later slot `≤ 5` is rejected.
7. **Conditional ratchet (integer).** A message `rac-slot = 8, rac-ratchet = 6`
   is rejected while the ratchet is below `6`, and admitted once it reaches `6`
   (jumping to `8`); a subsequent slot `7` is rejected.
8. **Multi-sender isolation.** Interleaved messages from two senders ratchet
   independently.
9. **First pass only.** With `~multipass@1.0`, an admitted message is applied
   once, not once per pass.
10. **Untagged pass-through.** A message with no `rac-slot` is admitted (`ok`)
    and unmodified.
