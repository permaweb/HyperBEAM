# HyperTokens: Networks of fungible, parallel ledgers.
## Status: Draft-1

This document describes the implementation details of the token ledger found in
`scripts/token.lua`. The script is built for operation with the HyperBEAM
`~lua@5.3a` and `~process@1.0` devices.

In addition to implementing the core AO token ledger API, `hyper-ledger.lua` also
implements a `sub-ledger` standard, which allows for the creation of networks
of ledgers that are may each hold fragments of the total supply of a given token.
Each ledger may execute in paralell fully asynchronously, while ownership in their
tokens can be viewed as fungible. The fungibility of tokens across these ledgers
is created as a result of their transitively enforced security properties -- each
ledger must be a precise copy of every other ledger in the network -- as well as
the transferrability of balances from one ledger to another. Ledgers that have
`register`ed with one another are able to transfer tokens directly. A multi-hop
routing option is also available for situations in which it may be desirable to
utilize pre-existing peer relationships instead.

This document provides a terse overview of the mechanics of this standard, and
the specifics of its implementation in `scripts/token.lua`.

## 1. Entities and State

### 1.1 Entities
- **User Account**: Identified by wallet address, may own tokens in `ledgers`.
- **Ledger Process**: An AO process implementing this token script.
- **Root Ledger**: The base token ledger process, from which supply scarcity is
  derived. Root ledgers are differentiated from `sub-ledger`s by the absence of
  a `token` field.
- **Sub-Ledger**: An independent ledger that that may own tokens in the root
  ledger, holding them on behalf of users and other ledgers in the network.
  All sub-ledgers must have a `token' field in their process definition, which
  contains the signed process ID of the root ledger.

### 1.2 State

Each ledger maintain the following fields:

- `balance`: A message containing a map of user addresses to token balances.
- `ledgers`: A message containing a map of peer-ledgers and the local ledger's
  balance in each.
- `token`: (Optional) The signed process ID of the root ledger.

Additionally, ledgers (root or sub-ledger) may maintain any other metadata fields
as needed in their process definition messages. Both metadata and necessary 
fields are available via the AO-Core HTTP API.

## 2. Message Paths and Actions

All instances of this script support calling the following functions, as either
the `path` or `action` of scheduled messages upon them:

- **Register**: Attempt to establish a trust relationship between ledger peers.
- **Register-Remote**: User-initiated request for the recipient ledger to
  register with a specific remote peer. After registration, the user may transfer
  tokens from their own account on the registrant ledger, to the remote ledger
  that they specified.
- **Transfer**: Move tokens between accounts (with optional routing).
- **Credit-Notice**: Notification sent by a ledger to a recipient of credit upon
  a successful transfer.
- **Debit-Notice**: A notification granted by a ledger to a sender of tokens,
  upon successful transfer.
- **Credit-Notice-Rejection**: A notice sent by a ledger to the sender of tokens,
  if the ledger is unable to accept the transfer (crediting its own account).
  This action occurs when the recipient ledger is unable to validate the sender
  as a valid peer, or when the recipient ledger is unable to validate the
  quantity of tokens being transferred. Upon receipt, a process will likely
  wish to reverse the transfer locally.
- **Route-Termination**: Notice, dispatched to a sender of tokens, if the
  ledger is unable to forward the transfer to the next hop in the stated route.
  Upon receipt, senders will typically wish to send a new transfer request to 
  the ledger with a different route to reach their recipient. If all other
  routes are exhausted, the sender may transfer the tokens to the root ledger.

## 3. Core Process Flows

### 3.1 Ledger Registration and Trust Negotiation.

1. Ledger A sends `Register` to Ledger B. The `push-device` that delivers this
   message to the recipient (for example, `push@1.0`) must add the
   `from-process-uncommitted` field to the message, containing the hash of the
   sending process.
2. Ledger B validates:
   - The admissibility of the assignment (its `scheduler` commitment),
   - Whether it trusts the computation commitments upon the message, and
   - Whether the message is from a process that is executing precisely the same
     code as the recipient, as signified by the `from-process-uncommitted` field.
3. Ledger B adds Ledger A to its list of trusted peers, if it is not already
   present, and sends a reciprocal `Register` message to the sending ledger.
4. Ledger A validates and adds Ledger B using the same mechanism, then adds it
   to its list of trusted peers.
5. **Result**: Bidirectional trust relationship, and the ability to transfer
   tokens directly between the two ledgers.

### 3.2 Direct Cross-Process Transfer

**Objective**: `Alice` wants to send tokens to `Bob`, who is on Ledger B. There
is an established peer relationship between Ledger A and Ledger B.

1. `Alice`, with tokens resident on Ledger A, initiates transfer to `Bob`, who 
   would like to receive tokens on Ledger B. Ledger B already has an established
   trust relationship and sufficient balance for `Alice` to send tokens to `Bob`.
2. Ledger A validates request, checks their balance for `Alice`, and debits the
   sender's account.
3. Ledger A sends a `Transfer` message to Ledger B.
4. Ledger B validates sender ledger, decrements sender's balance, and credits the
   recipient's balance.

### 3.3 Multi-Hop Transfer

**Objective**: `Alice` wants to send tokens to `Bob`, who is on Ledger N, but
there is no peer relationship between Ledger A and Ledger N.

1. `Alice` initiates a transfer on Ledger A, with a multi-hop route
   (`route=[L₁, L₂, ..., Lₙ]`) to reach `Bob` on Ledger N.
2. As with S`3.2`, Ledger A validates request, checks their balance for `Alice`,
   and debits the sender's account.
3. Each intermediate ledger in the route validates the balance of the sending 
   ledger, and debits their account. Each ledger also removes themselves from the
   list of hops remaining in the `route` parameter of the request, and forwards
   it onwards to the next hop.
4. Final ledger in the route validates the balance of the sending ledger, and
   credits the `Bob`'s account.

### 3.4 Transfer with Peer Registration

**Objective**: `Alice` wants to send tokens to `Bob`, who is on Ledger B. There
is no peer relationship between Ledger A and Ledger B, but `Alice` would rather
establish one than route the transfer through a potentially longer multi-hop
route.

1. `Alice` sends a `Register-Remote` message to Ledger B, with a `peer` parameter
   of Ledger A's signed process ID.
2. Ledger B validates the request, and adds Ledger A to its list of trusted peers.
3. `Alice` sends a `Transfer` message to Ledger B.
4. Ledger A and B validate the transfer request, as in `3.2`.

## 4. Intended Security Properties

1. **Code Integrity**: Each new peer relationship validates that each other 
   peer is executing precisely the same code as it is. Subsequently, the
   security properties of the original ledger are transitively applied to all
   new ledgers in the network.
2. **Conservation of Tokens**: As each peer may trust each other peer to monitor
   balances as they would, the network as a whole maintains the conservation
   of the total supply of tokens.
3. **Trustless Registration**: A subledger may register with any AO token process
   without that process needing to be aware of the subledger protocol, nor the
   security properties that it enforces. Instead, users that wish to participate
   in a subledger process network with security properties that they deem 
   acceptable may do so at-will. Processes that do choose to participate may
   maintain an index of known ledgers, allowing tokens within them to be shown
   as fungible with the root ledger's tokens in user interfaces, etc.

## 5. API Reference

### 5.1 External API Functions

#### `transfer(state, assignment)`

Transfers tokens from one account to another.

Parameters:

- `base`: The current state of the ledger process.
- `assignment`: An assignment of the transfer message from the process's
  `sheduler`.
- `assignment/body`: The transfer message.
- `assignment/body/from`: Source account (determined from signature or 
  `from-process`).
- `assignment/body/recipient`: The destination account.
- `assignment/body/quantity`: Amount to transfer (integer).
- `assignment/body/route`: (Optional) A list of ledger IDs that should be 
  traversed to reach the destination ledger.

Returns: The updated process ledger state, and:

- `result/status`: The status of the transfer.
- `result/outbox/1`: A message containing the `Action: Credit-Notice` field, sent
  to the recipient (either an end-user or another ledger).
- `result/outbox/2`: (Optional) A message containing the `Action: Debit-Notice`
  field, dispatched to the sender _if_ the transfer is not a multi-hop route.
  In the case of multi-hop routes, debit notices are not sent to any intermediate
  ledgers, but are sent to the initial sender upon completion of the final hop.

In event of error, the following messages are dispatched:

- `result/outbox/1`: (Optional) A message containing the
  `Action: Credit-Notice-Rejection` field, along with a `notice` field with the
  ID of the credit notice that was rejected.
- `result/outbox/1`: (Optional) A message containing `Action: Route-Termination`,
  sent to the initiator of an inter-ledger transfer, if the transfer is unable to
  reach the destination ledger. In this case, the sender will hold a balance on
  the intermediate ledger, and may attempt to route the transfer again with a
  different route.

#### `register-remote(base, assignment)`

Initiates a registration from the target ledger to the `peer` ledger.

Parameters:
- `base`: The current state of the ledger
- `assignment`: The message containing:
- `assignment/body/peer`: The signed process ID of the remote ledger to
  register with.

#### `register(base, assignment)`

Attempts to register the sending ledger with the target ledger.

Parameters:
- `base`: The current state of the ledger
- `assignment`: The message containing:
- `assignment/body/from`: The ledger requesting registration.
- `assignment/body/from-process-uncommitted`: A hash, added by the `push-device`,
  of the sending process.