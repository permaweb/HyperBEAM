# AO 1.0: A universal protocol for computation over URIs.

AO is a language for describing a method of combining -- computing over -- URI-named
resourced, yielding further URI-addressable values. AO forms a computing environment,
but it is not itself a virtual machine. It is a neutral meta-VM: a common
language for describing, transporting, attesting, challenging, and tracing the
computations performed by many VMs, called devices.

AO offers three core properties:

1. **Atomic attestation**: any computation claim can be verified or challenged
   independently from all other history.
2. **Succinct portability**: any computation may be continued on any other 
   participating machine by invoking its accrued hashpath upon the recipient.
   Each element of the state of the computation may be moved eagerly or recalled
   from untrusted peers on-demand.
3. **Traceability**: every computation result can be traced through every state
   transformation to the origins of the values on which it depends.

Through these properties in a decentralized protocol, AO-Core intends to give
rise to a globally distributed supercomputer presenting a single system image
across a machine of arbitrary size.

## Resources, Collections, and Values.

A resource is any protocol-addressable content, referenced by URIs ([RFC 3986]).

As stated in the specification, URIs may come in hierarchical and non-hierarchical
forms. Their values are produced by resolvers that yield a representation of the
bytes that the named resource refers to. Resources may identify other values by
protocol address; creating a graph of connected computed elements.

AO runtime implementations use a system of caches that associated resources with
their associated values, then make these available for computations to access.
When a URI is not known in a node's existing stores, it may be requested for
resolution using the host environments native mechanisms, or through custom
providers built-in to the runtime.

Resources may share a common prefix, resembling a directory or other composite
structure via their path-parts. AO resolutions may both consume and produce such
collections of associated URIs by referring to their shared prefix in either
inputs or outputs to computations.

## Hashpaths and Execution

In addition to consuming URIs as inputs the AO protocol defines the `ao://`
URI-scheme, which allows its universal computation language to name state
transitions, attest to their dependencies, and their results. We call the
namespace of URIs under this scheme collectively `hashpaths`.

Hashpaths refer to a merklized log of computation contexts, each of which
contains at minimum a `Base` URI and a representation of the request.
The simplest form of a hashpath is resolving a `data:` `request` value against
a resource collection, which yields the standard resolution of the concatenated
values (`BaseURI/RequestBinary`).

If the request is instead of a composite form, we attempt to resolve `/path`
element upon the second ('request') URI for the  computation, and search the
primary ('base') with its value. In the event that the resource is found, its
value is returned. If the direct URI is not found, we look for a directly asserted
`BaseURI/device`, yielding a specification -- or a binary, whose value can be
resolved to a specification -- of a `device` (a virtual machine) to utilize for
the computation. If a device is not found in the `BaseURI` we recurse backwards
through prior computed results of the hashpath, repeating the process until
either a direct key or `device` is found, or the hashpath reaches a 'replacement'
element (implying that the computation's result does not 'extend' the elements
found in the hashpath prior to it, but instead nullifies each). Through this
recursive mechanism, new messages can be composed via the combination of prior
message IDs, and values in a present state may lazily include those from prior
states -- allowing their history (when each was set, what the inputs to that
computation were, etc) to be traced individually.

Each new Extension does not mutate the `Base`. Instead, it constructs a new value
that shares elements from the `Base`, overlayed with additional resolvable
resources and prefixed names. This grants the basis of message deduplication:
large states can be represented as small patches over existing states, maintaining
provenance.

Hashpaths represent a sequence of execution frames, each of which may contain a
number of elements:
1. A `Base` URI upon which the request is being made. Must always be present on
   the first context of the hashpath, but may be ommitted for the second
   and further elements, instead having their `Base` inferred from the rolling
   context of the hashpath itself.
2. A raw binary or `Request` URI, holding the metadata of the request upon the
   `Base`.
3. A `Result`, optionally, if computed, the result of the computation as a
   further URI.
4. `Varied-Base` and `Varied-Request` emelements, each containing URIs for
   collections of _only_ the necessary components that were utilized in the 
   state transition.
5. `Dependencies`, a collection hosting the origin hashpath of each utilized
   component from the `[Varied-][Base|Request]` elements. It may be omitted only
   when empty or fully derivable.

Claims are signatures over one or more complete hashpath assertions rather than
fields within their frames. A hashpath may convey any number of connected frames
of execution covered by a single claim. Hashpath may be extended by Merklizing
the ordered list of contexts, such that only a single root is provided as the
`Base` to the final frame. Alternatively, any number of `Base`/`Request` pairs may
be provided, with the `Base` of each frame being the ancestry (ordered prior
results) of the `Request` at that layer.

## Resolution Semantics

Concretely, the primitive AO relation is:

```text
BaseURI / RequestURI | Value -> ResultURI | Value
```

For a request whose `path` is `P`, resolution walks the ancestry of `Base` from
the outermost layer inward until either a direct result, a `device`, or a
non-extending hashpath context is found.

At each layer:

1. Look for `BaseURI/P` among the resources asserted directly at this layer.
   Return if found.
2. Look directly for `BaseURI/device` at this layer.
3. If `Device` is found, look up a runtime-compliant implementation and execute
   against the original outermost `Base` and the original `Request`.
4. Else, if a prior extending element of the hashpath context is found, recursively
   resolve at that element.

In the pseudocode below, `lookup` inspects direct assertions in the loaded layer.
It does not recursively perform AO execution.

```text
resolve(Outer, BaseURI, Request):
  P = path(Request)

  case lookup(BaseURI/P) of
    Value -> return Value
    unset -> return not_found
    not_found -> continue
  end

  case lookup(BaseURI/device) of
    Dev -> return execute(Dev, Outer, Request)
    not_found -> continue
  end

  case local(BaseURI/...) of
    Ancestor -> return resolve(Outer, Ancestor, Request)
    not_found -> return not_found
  end
```

If a device is inherited from an ancestor, it executes over the outermost state:

```text
{ x: 5, ...: { device: dev1, x: 1 } } / x-is-5
```

selects `dev1` from the ancestor, but `dev1` sees `x = 5`.

## Devices

A device defines how requests are computed for a state.

Examples of devices include message interpreters, process VMs, WASM VMs,
Lua VMs, codecs, stores, payment devices, and application-specific evaluators.
AO-Core does not privilege one compute model over another. It standardizes how
their transitions are named, witnessed, attested, transported, and traced.

`Device` references may be stated in three forms:

1. **Specification URIs**: fully-qualified URIs that yield the intended device's
   full specification when resolved.
2. **Binaries, resolved as Permaweb Names**: Binary literals that imply a 
   request to resolve the name against `ao://~name@1.0/[Name]` in the resolver.
3. **`ao://` Resource Prefixes**: Recursively resolved devices whos functionality
   is defined by extending the `Base` resource with its values and calling the
   resolution upon its new form.

The third form allows for recursive resolution of devices, in which a new device
is constructed using the same underlying virtual machine implementations inside 
the AO runtime, but with the values found in the extension resource taking 
precedence over the base resource.

## Vary

Device application has two phases. First, the selected device prepares an
executable context containing the local function to invoke, the minimized
`Varied-Base` and `Varied-Request`, their `Dependencies`, and whether the result
extends or replaces the `Base`. The protocol-visible portion of preparation is:

```text
Base / Request / vary -> VariedBase + VariedRequest @ Dependencies
```

`VariedBase` and `VariedRequest` are resource collections containing exactly the
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

`Dependencies` records where each varied value originated. It has the same shape
as the varied collections, rooted under `base` and `request`. Each leaf is a
hashpath whose terminal value is the corresponding varied value:

```text
Dependencies = {
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
does not need to be duplicated inside `Dependencies`, because the hashpath binds the
origin to the value it yields.

If no exact vary specification is available, the conservative valid vary is
identity:

```text
VariedBase = Base
VariedRequest = Request
```

Identity variation does not waive dependency recording.

The core rule is:

```text
No hidden inputs.
```

Everything observed by execution must be present in `VariedBase` or
`VariedRequest`, and every varied value must have an origin in `Dependencies`.

## Shared Computation

Varying creates a reusable computation point.

Many concrete bases may vary to the same pair:

```text
BaseA / Request / vary -> VariedBase + VariedRequest @ DependenciesA
BaseB / Request / vary -> VariedBase + VariedRequest @ DependenciesB
```

The execution:

```text
VariedBase / VariedRequest -> Patch
```

is shared by all sufficiently alike concrete bases for that request. The final
results may still differ because the patch is equivalent to applying the original
transition to each original base:

```text
BaseA / Request == set(BaseA, Patch)
BaseB / Request == set(BaseB, Patch)
```

This is the default mode of AO-Core computation: do a computation once for the
material inputs that matter, then reuse it across every base/request pair that
varies to those inputs.

Both the varied computation and complete transition results are cacheable by
address, allowing prior computations from different execution traces to be
reused.

## Transition Equivalence

A transition asserts an equivalence between resolving a request and extending
the base with the patch produced by varied execution:

```text
Base / Request
  == set(Base, Patch)
```

where:

```text
Base / Request / vary -> VariedBase + VariedRequest @ Dependencies
VariedBase / VariedRequest -> Patch
```

The device's preparation selects extension or replacement as the result mode.
AO-Core applies that mode: extension constructs a new layer whose ancestry is
the original `Base`, while replacement terminates active inheritance. Devices do
not implement ancestry traversal themselves.

## Hashpath Assertions And Claims

A hashpath without a terminal result is an address naming a computation. A
hashpath containing its result is an assertion. A signature over one or more
hashpaths is a claim. Like any other value, a hashpath may be encountered as a
link and resolved through AO-Core, or encountered as serialized content and
decoded into its in-memory form.

The full transition forms are:

```text
BaseID/ReqID>VariedBaseID+VariedReqID@DependenciesID=PatchID
BaseID/ReqID>VariedBaseID+VariedReqID@DependenciesID.ResultID
```

`=` means the execution produced a patch that extends the prior result:

```text
BaseID/ReqID>VariedBaseID+VariedReqID@DependenciesID=PatchID
```

is equivalent to:

```text
set(Base, Patch)
```

`.` means the execution produced a replacement value:

```text
BaseID/ReqID>VariedBaseID+VariedReqID@DependenciesID.ResultID
```

The accumulated result becomes `ResultID`, dropping the prior result's resources
rather than extending them.

A compact form may omit fields when they are derivable or supplied elsewhere,
but the full assertion must be recoverable for challenge and trace.

A hashpath is a sequence of transition assertions. Later segments operate on the
result established by earlier segments.

Segments without explicit vary syntax are not special. They are compact
transition assertions. For example:

```text
HP/*=FinalResultID
```

is simply an assertion that resolving `*` at `HP` yields `FinalResultID`. HTTP
gateways commonly append such a segment so the response body is tied to the
specific keys and values returned to the client.

## Hashpath Loading And Portability

Hashpaths are not a storage system outside AO-Core. They are addressable values
whose loaded form reconstructs resource-prefix and extension semantics.

Reusable patches can be stored by their generic IDs, without caller-specific
ancestry. The hashpath records how that generic value is reached from a prior
result. For an extension segment:

```text
PriorHP/Req>VariedBase+VariedReq@Dependencies=Patch
```

Stores may cache the asserted result at that hashpath. Loading the segment loads
`Patch` and presents it as an extension whose `...` is the previous accumulated
result:

```text
load(PriorHP/Req>VB+VR@Dependencies=Patch)
  -> { PatchKeys..., ...: PriorHP }
```

The `...` value is itself a hashpath. Loading it reconstructs the prior result
for inherited-key resolution. The complete result remains addressed by the full
hashpath, which retains the request, varied witnesses, and dependencies needed
to challenge the transition.

For replacement segments:

```text
PriorHP/Req>VB+VR@Dependencies.Result
```

loading the segment yields `Result`. The prior result is not inherited as active
keys, but the hashpath still carries the transition context needed for
challenge and trace.

This gives portability as a single addressable value: posting a hashpath plus
the values needed to resolve the IDs it names gives another node enough data to
reconstruct the result collection, challenge any segment, and continue computation.

## Challenge And Audit

A transition can be challenged locally, without verifying its entire dependency
tree. The usual practical operation is to pick one assertion and verify only the
values needed to reproduce it. A full provenance audit is the recursive version
of the same process.

To challenge a transition assertion:

1. Verify that `BaseID` and `ReqID` identify the asserted values.
2. Repeat the selected device's preparation phase and verify its asserted
   `VariedBase`, `VariedRequest`, `Dependencies`, and result mode:

   ```text
   Base / Request / vary -> VariedBase + VariedRequest @ Dependencies
   ```

3. For every leaf in `VariedBase` and `VariedRequest`, follow the matching leaf
   in `Dependencies` and verify that it yields that value.
4. Invoke the prepared function and verify its result. An existing result may
   substitute for execution only if it was previously verified under the
   verifier's trust policy:

   ```text
   VariedBase / VariedRequest -> Patch
   ```

5. Verify transition equivalence:

   ```text
   "=" means the accumulated result is set(Base, Patch)
   "." means the accumulated result is Result
   ```

Any one of these checks can be challenged independently. To audit the full
provenance tree, recursively challenge the dependency hashpaths named in
`Dependencies`.

## Traceability

To trace a value in a result:

1. Locate the transition that produced the result collection containing the
   value.
2. If the value was introduced by the patch, trace it to that transition's
   varied witness.
3. Follow every corresponding leaf in `Dependencies`.
4. If the value was inherited through `...`, continue tracing in the ancestor
   result.
5. Repeat recursively until reaching literals, signed inputs, codec inputs, or
   externally supplied claims.

For example, a process result may assert:

```text
ProcessStateN.balance.OUR_ADDRESS = 10
```

The trace may show that this came from:

```text
ProcessStateN-1 / TransferRequest
  > VariedBase + VariedRequest @ Dependencies
  = ProcessStateN
```

with:

```text
VariedBase.balance.OUR_ADDRESS = 7
VariedBase.balance.SENDER = 93
VariedRequest.quantity = 3
```

`Dependencies` then points to the hashpaths that produced `7`, `93`, and `3`. The
quantity may trace to an inbound message from a swap process, whose sale-price
transition may itself be attested by another node.

The trace is not a narrative. It is a recursive chain of AO-Core assertions.
Any signed assertion in the chain is a claim.

## HTTP Expression

HTTP is an expression of AO-Core, not the foundation of AO-Core.

An HTTP request is decoded into an AO-Core request resource. URL path segments,
query parameters, method, headers, and body are associated resources. Content
negotiation selects codecs for values and resource collections. The server
resolves:

```text
Base / Request -> Result
```

and returns an HTTP response containing the encoded result, its result assertion,
supporting values, and any signed claims needed for the receiver to verify, port,
and continue the computation.

Thus HTTP gives AO-Core a universal transport and user-facing syntax while the
protocol remains independent of HTTP itself.

HTTP computations should append a terminal materialization assertion by default:

```text
HP/*=MaterializedID
```

This assertion is not special in the hashpath calculus. It is the ordinary
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
