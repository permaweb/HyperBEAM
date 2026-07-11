# AO-Core 1.0 Draft 4

AO-Core is a protocol for attestable computation over protocol-addressed
messages. It is not itself a virtual machine. It is a neutral meta-VM: a common
language for describing, transporting, attesting, challenging, and tracing the
computations performed by many VMs, called devices.

AO-Core offers three core properties:

1. **Atomic attestation**: any transition claim can be verified or challenged
   independently from unrelated computation history.
2. **Succinct portability**: a state can be moved as a hashpath plus the values
   needed by that hashpath, without enumerating every value that could later be
   resolved.
3. **Traceability**: every computation result can be traced through every state
   transformation to the origins of the values on which it depends.

Through these properties in a decentralized protocol, AO-Core intends to give
rise to a globally distributed supercomputer presenting a single system image
across a machine of arbitrary size.

## Values And Messages

A value is any protocol-addressable content.

A message is a value that contains keys and values. Values may identify other
values by protocol address; such links are literals from the point of view of the
core protocol.

All values can be named by ID. All messages can be represented by extension of
other messages through ancestry:

```text
set(Base, Patch) = { Patch..., ...: Base }
```

`...` is ancestry. `unset` masks an inherited key.

Extension does not mutate `Base`. It constructs a new value that shares
structure with `Base`. This is the basis of message deduplication: large states
can be represented as small patches over existing states.

## Resolution

The primitive relation is:

```text
Base / Request -> Result
```

For a request whose key is `K`, resolution walks the ancestry of `Base` from the
outermost layer inward.

At each layer:

1. Look locally for `K`.
2. If `K = Value` is found, return `Value`.
3. If `K = unset` is found, return `not_found`.
4. Otherwise, look locally for `device`.
5. If `device = Dev` is found, execute `Dev` against the original outermost
   `Base` and the original `Request`.
6. Otherwise, look locally for `...`.
7. If `... = Ancestor` is found, continue at `Ancestor`.
8. Otherwise, return `not_found`.

Pseudocode:

```text
resolve(Outer, Layer, Request):
  K = key(Request)

  case local(Layer, K) of
    Value -> return Value
    unset -> return not_found
    not_found -> continue
  end

  case local(Layer, "device") of
    Dev -> return execute(Dev, Outer, Request)
    not_found -> continue
  end

  case local(Layer, "...") of
    Ancestor -> return resolve(Outer, Ancestor, Request)
    not_found -> return not_found
  end
```

This local inspection is `message@1.0` behavior. It asks only about the current
layer's decoded members; it does not recursively resolve while inspecting that
layer.

If a device is inherited from an ancestor, it executes over the outermost state:

```text
{ x: 5, ...: { device: dev1, x: 1 } } / x-is-5
```

selects `dev1` from the ancestor, but `dev1` sees `x = 5`.

Layer order determines which value answers a key; it does not restrict what a
device sees. A local key answers before its layer's device, but a device that
answers always executes over the complete outermost state.

## Devices

A device defines how requests are computed for a state.

Examples of devices include message interpreters, process VMs, WASM VMs,
Lua VMs, codecs, stores, payment devices, and application-specific evaluators.
AO-Core does not privilege one compute model over another. It standardizes how
their transitions are named, witnessed, attested, transported, and traced.

If no device is found, the default device is `message@1.0`.

A device is itself a value, and may be named by link. When the device value is
a message, execution is itself resolution: the request is resolved against the
device message extended over the outermost state:

```text
execute(Dev, Outer, Request) = resolve(set(Outer, Dev), Request)
```

The device message's own keys answer first, its own `device` computes the
remainder, and the outermost state remains visible as ancestry. The recursion
terminates at a device the host implements directly; which devices those are is
an implementation fact.

Device loading, Erlang modules, function pointers, local caches, and runtime
workers are implementation facts. The protocol fact is the device value that
the computation commits to using.

## Vary

Before execution, a transition is varied:

```text
Base / Request / vary -> VariedBase + VariedRequest @ Depends
```

`VariedBase` and `VariedRequest` are ordinary messages containing exactly the
values required by the execution. They use canonical nested structure, not path
strings:

```text
VariedBase = {
  device: process@1.0,
  balance: {
    OUR_ADDRESS: 7,
    SENDER: 93
  }
}

VariedRequest = {
  path: transfer,
  from: SENDER,
  to: OUR_ADDRESS,
  quantity: 3
}
```

`Depends` records where each varied value originated. It has the same shape as
the varied messages, rooted under `base` and `request`. Each leaf is a hashpath
whose terminal value is the corresponding varied value:

```text
Depends = {
  base: {
    device: HP_for_process_device,
    balance: {
      OUR_ADDRESS: HP_for_prior_our_balance,
      SENDER: HP_for_prior_sender_balance
    }
  },
  request: {
    path: HP_for_request_path,
    from: HP_for_request_from,
    to: HP_for_request_to,
    quantity: HP_for_quantity
  }
}
```

The dependency leaf is a single value: the origin hashpath. The value itself
does not need to be duplicated inside `Depends`, because the hashpath binds the
origin to the value it yields.

If no exact vary specification is available, the conservative valid vary is
identity:

```text
VariedBase = Base
VariedRequest = Request
```

The core rule is:

```text
No hidden inputs.
```

Everything observed by execution must be present in `VariedBase` or
`VariedRequest`, and every varied value must have an origin in `Depends`.

## Shared Computation

Varying creates a reusable computation point.

Many concrete states may vary to the same pair:

```text
BaseA / Request -> VariedBase + VariedRequest
BaseB / Request -> VariedBase + VariedRequest
```

The execution:

```text
VariedBase / VariedRequest -> Patch
```

is shared by all sufficiently alike concrete states for that request. The final
states may still differ because the patch is equivalent to applying the original
transition to each original base:

```text
BaseA / Request == set(BaseA, Patch)
BaseB / Request == set(BaseB, Patch)
```

This is the default mode of AO-Core computation: do a computation once for the
material inputs that matter, then reuse it across every state/request pair that
varies to those inputs.

All AO-Core states and transition results are cacheable by address. Prior
computations from many different execution traces can therefore be reused
seamlessly. Because AO-Core is expressed naturally through HTTP semantics, the
same `Vary`-style caching and routing ideas that already power web
infrastructure can be applied to decentralized computation.

## Transition Equivalence

A transition asserts an equivalence between resolving a request and extending
the base with the patch produced by varied execution:

```text
Base / Request
  == set(Base, Patch)
```

where:

```text
Base / Request / vary -> VariedBase + VariedRequest @ Depends
VariedBase / VariedRequest -> Patch
```

Extension is just `set`. There is no special state update operation beyond
constructing a new message with ancestry.

## Hashpaths

A hashpath is a succinct executable claim and an addressable protocol value.
Like any other value, it may be encountered as a link and resolved through
AO-Core, or encountered as serialized content and decoded into its in-memory
form.

The full transition forms are:

```text
BaseID/ReqID>VariedBaseID+VariedReqID@DependsID=PatchID
BaseID/ReqID>VariedBaseID+VariedReqID@DependsID.ResultID
```

`=` means the execution produced a patch that extends the prior state:

```text
BaseID/ReqID>VariedBaseID+VariedReqID@DependsID=PatchID
```

is equivalent to:

```text
set(Base, Patch)
```

`.` means the execution produced a replacement value:

```text
BaseID/ReqID>VariedBaseID+VariedReqID@DependsID.ResultID
```

The accumulated state becomes `ResultID`, dropping the prior state's keys rather
than extending them.

A compact form may omit fields when they are derivable or supplied elsewhere,
but the full receipt must be recoverable for challenge and trace.

A hashpath is a sequence of transition claims. Later segments operate on the
result established by earlier segments.

Segments without explicit vary syntax are not special. They are compact
transition claims. For example:

```text
HP/*=FinalResultID
```

is simply a claim that resolving `*` at `HP` yields `FinalResultID`. HTTP
gateways commonly append such a segment so the response body is tied to the
specific keys and values returned to the client.

## Hashpath Loading And Portability

Hashpaths are not a storage system outside AO-Core. They are addressable values
whose loaded form reconstructs ordinary message semantics.

Reusable patches can be stored by their generic IDs, without caller-specific
ancestry. The hashpath records how that generic value is reached from a prior
state. For an extension segment:

```text
PriorHP/Req>VariedBase+VariedReq@Depends=Patch
```

the cacheable value may be the hashpath link itself:

```text
link:PriorHP/Req>VariedBase+VariedReq@Depends=Patch
```

loading the segment loads `Patch` and presents it as an extension whose `...`
is the transition context before the patch result:

```text
load(PriorHP/Req>VB+VR@Deps=Patch)
  -> { PatchKeys..., ...: PriorHP/Req>VB+VR@Deps }
```

The `...` value is itself a hashpath value. Loading it reconstructs the prior
state for inherited-key resolution and retains the request, varied witnesses,
and dependency message needed to challenge the transition. Implementations may
cache this decoded context in runtime metadata, but the protocol-visible
ancestry remains the hashpath value.

For replacement segments:

```text
PriorHP/Req>VB+VR@Deps.Result
```

loading the segment yields `Result`. The prior state is not inherited as active
message keys, but the hashpath still carries the transition context needed for
challenge and trace.

This gives portability as a single addressable value: posting a hashpath plus
the values needed to resolve the IDs it names gives another node enough data to
reconstruct the live state, challenge any segment, and continue computation.

## Challenge And Audit

A transition can be challenged locally, without verifying its entire dependency
tree. The usual practical operation is to pick one claim and verify only the
facts needed for that claim. A full provenance audit is the recursive version of
the same process.

To challenge a transition claim:

1. Verify that `BaseID` and `ReqID` identify the claimed values.
2. Verify the vary claim:

   ```text
   Base / Request / vary -> VariedBase + VariedRequest @ Depends
   ```

3. For every leaf in `VariedBase` and `VariedRequest`, follow the matching leaf
   in `Depends` and verify that it yields that value.
4. Verify execution:

   ```text
   VariedBase / VariedRequest -> Patch
   ```

5. Verify transition equivalence:

   ```text
   "=" means the accumulated state is set(Base, Patch)
   "." means the accumulated state is Result
   ```

Any one of these checks can be challenged independently. To audit the full
provenance tree, recursively challenge the dependency hashpaths named in
`Depends`.

## Traceability

To trace a value in a result:

1. Locate the transition that produced the state containing the value.
2. If the value was introduced by the patch, trace it to that transition's
   varied witness.
3. Follow every corresponding leaf in `Depends`.
4. If the value was inherited through `...`, continue tracing in the ancestor
   state.
5. Repeat recursively until reaching literals, signed inputs, codec inputs, or
   externally attested transition claims.

For example, a process state may claim:

```text
ProcessStateN.balance.OUR_ADDRESS = 10
```

The trace may show that this came from:

```text
ProcessStateN-1 / TransferRequest
  > VariedBase + VariedRequest @ Depends
  = ProcessStateN
```

with:

```text
VariedBase.balance.OUR_ADDRESS = 7
VariedBase.balance.SENDER = 93
VariedRequest.quantity = 3
```

`Depends` then points to the hashpaths that produced `7`, `93`, and `3`. The
quantity may trace to an inbound message from a swap process, whose sale-price
transition may itself be attested by another node.

The trace is not a narrative. It is a recursive chain of AO-Core claims.

## HTTP Expression

HTTP is an expression of AO-Core, not the foundation of AO-Core.

An HTTP request is decoded into an AO-Core request message. URL path segments,
query parameters, method, headers, and body are message members. Content
negotiation selects codecs for values and messages. The server resolves:

```text
Base / Request -> Result
```

and returns an HTTP response containing the encoded result plus enough hashpath
and commitment information for the receiver to verify, port, and continue the
computation.

Thus HTTP gives AO-Core a universal transport and user-facing syntax while the
protocol remains independent of HTTP itself.

HTTP computations should append a terminal materialization claim by default:

```text
HP/*=MaterializedID
```

This claim is not special in the hashpath calculus. It is the ordinary
materialization request for `*`, included so that the transport response is tied
to the concrete enumerated keys and values returned to the client.

The default HTTPSig response carries two independently useful commitments:

1. A commitment over the enumerated response keys and values, excluding the
   `...` hashpath field. This commitment is typically unsigned unless the
   response is already a pre-existing signed message. Its ID is
   `MaterializedID`.
2. A signature over just the excluded `...` field, whose value is the hashpath
   terminating in `=MaterializedID`.

The first commitment lets the returned value stand alone by ID. The second lets
the recipient port or challenge the computation through the hashpath. They can
be used together or independently.

## Summary

AO-Core turns computation into portable, challengeable, traceable message
transitions.

Devices provide the compute. Vary provides exact witnesses. Depends provides
origin trace. Hashpaths provide portable claims. Extension provides deduplicated
state. HTTP provides a practical expression layer.
