# AO 1.0: A universal protocol for computation over URIs.

AO is a language for describing a method of combining -- computing over -- URI-named
resources, yielding further URI-addressable values. AO forms a computing environment,
but it is not itself a virtual machine. It is a neutral meta-VM: a common
language for describing, transporting, attesting, challenging, and tracing the
computations performed by many VMs, called devices.

This document is intended to collate the protocol-visible AO-Core semantics:
resource identity, message resolution, device specification and implementation
discovery, commitment surfaces, hashpath assertions, witness packaging, and HTTP
expression. Runtime-local caches, scheduler choices, implementation languages,
and other execution optimizations are outside the protocol except where they
affect a value's public identity or verifiability.

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

## Reading This Draft

This document collates portable AO-Core semantics and the current HyperBEAM
source profile. Requirements are labeled by scope:

* **AO-Core requirement**: protocol-visible behavior that portable
  implementations must preserve.
* **HyperBEAM profile**: behavior of the current Erlang implementation or a
  source-backed implementation strategy that is not itself a portability rule.
* **Draft target**: intended protocol behavior whose exact source
  implementation, validation coverage, or compatibility boundary is still being
  completed.
* **Source gap**: a known mismatch between the portable rule stated here and
  the current source profile.
* **Implementation note**: local advice, optimization, or design rationale that
  may change without altering AO-Core semantics.
* **Source-green**: implemented in current source and covered by compiling,
  passing source tests or equivalent source-backed validation.

The terms `must`, `must not`, `should`, `should not`, and `may` mark normative
strength within the relevant scope label. Implementation notes are
non-normative. Until this draft is promoted to a formal standards-track
document, normative words should be read as AO-Core draft requirements rather
than as a completed external standards process.

Conformance is intentionally split across protocol surfaces. An implementation
may conform to one surface without claiming all of AO-Core:

* An **AO value implementation** preserves normalized message keys, private-key
  exclusion, value identity, and public value lookup.
* An **AO resolver implementation** evaluates `Base / Request` consistently,
  including `path`, `device`, `from`, message extension/replacement, and
  result/error normalization semantics.
* A **commitment and codec implementation** preserves declared commitment
  surfaces, ID generation, verification inputs, and the AO-visible public
  message surface across encodings.
* A **device-loading implementation** maps device names, IDs, and implementation
  descriptors to executable devices with explicit trust and compatibility rules.
* A **hashpath implementation** produces and verifies the hashpath profile it
  claims. The current source-green profile and the richer draft assertion profile
  are separate conformance targets.
* An **HTTP expression implementation** maps HTTP requests and responses to the
  equivalent AO messages without changing the AO-visible computation.

## Resources, Collections, and Values.

A resource is any protocol-addressable content, referenced by URIs ([RFC 3986]).

As stated in the specification, URIs may come in hierarchical and non-hierarchical
forms. Their values are produced by resolvers that yield a representation of the
bytes that the named resource refers to. Resources may identify other values by
protocol address; creating a graph of connected computed elements.

AO runtime implementations use a system of caches that associate resources with
their associated values, then make these available for computations to access.
When a URI is not known in a node's existing stores, it may be requested for
resolution using the host environment's native mechanisms, or through custom
providers built-in to the runtime.

Resources may share a common prefix, resembling a directory or other composite
structure via their path-parts. AO resolutions may both consume and produce such
collections of associated URIs by referring to their shared prefix in either
inputs or outputs to computations.

## Messages, Keys, and IDs

An AO value is a literal, a resource collection, or a link to another AO value.
A resource collection is the protocol form commonly called a message. Message
keys are normalized to binary names for protocol comparison. Runtime
implementations may expose native atoms, strings, or other local forms, but those
forms are converted to normalized keys before resolution, ID generation,
commitment selection, and hashpath construction.

Messages may be expressed through many wire formats. HyperBEAM documents and
implements at least richly typed AO structured messages, HTTP Signed Messages,
ANS-104 data items, Arweave transactions, and flat maps. The common normalized
interchange form is a Type Annotated Binary Message (TABM): a nested message
whose keys are binary names and whose literal values are encoded in binary or
typed binary form. Codecs convert external formats to TABM, then from TABM to
the target format. A codec transformation must preserve the AO-visible public
message surface, commitments, links, and IDs it claims to represent.

The AO-visible message surface is not identical to every field a codec may emit.
Codec-generated fields such as `ao-types`, `+link` variants, `content-digest`,
`ao-body-key`, `signature`, and `signature-input` are part of the codec or
commitment device surface when that device defines them. They must not silently
change application-visible keys, but they may be used to encode rich types,
linked children, body digests, or commitment records. A commitment record's
stored `committed` list names the device-encoded components that were actually
signed or otherwise proven. Public helper APIs may translate those components
back toward AO key names, but verification must reconstruct the exact
device-encoded surface.

The `structured@1.0` codec normalizes keys, filters private keys, filters
regenerated codec keys, recursively encodes nested messages, and records rich
literal types in `ao-types` when needed. The `commitments` key is preserved as
commitment metadata rather than treated as an ordinary application value during
structured encoding.

Converting through TABM excludes private keys from the public encoded surface.
Current HyperBEAM may restore the caller's existing `priv` map after a local
conversion so runtime state is not lost, but that restored private state is not
part of the encoded value, not part of a portable commitment, and not evidence
that the wire format carried private keys.

HTTP field names are case-insensitive, and the `message@1.0` identity device
performs ordinary binary key lookup case-insensitively. Portable messages should
not depend on two public keys that differ only by case unless the selected
device explicitly specifies how that collision is resolved.

Keys under the private namespace are local implementation state. A private key is
any normalized binary key whose name begins with the bytes `priv`; this includes
`priv`, `private`, `private.foo`, and `priv_foo`, as well as encoded or linked
spellings whose AO source key has that prefix. Private keys are not public
memberships: they are not returned by portable key enumeration, not resolved by
ordinary public lookup, not part of message equality, and not part of portable
identity. Two messages that differ only in private state are the same AO value,
and AO IDs must be invariant under private-only changes.

Implementations may use private keys to carry local execution state such as
loaded functions, cache hints, workers, or hashpath context. Portable encodings,
commitments, hashpath witnesses, continuation packages, and verification inputs
must be reconstructable without private state. If private-derived information
must become portable, the implementation must explicitly project it onto a
non-private public key, such as copying a private hashpath to public `hashpath`,
and commit that public key.

Current HyperBEAM source applies the `priv*` predicate for message lookup and key
filtering, but the HTTPSig commitment device does not yet enforce that predicate
on every key-selection path. Its default key set excludes exact `priv`, while
explicit `committed` lists and stacked committed-key lists are accepted as
supplied. That is a current source gap against the portable AO-Core rule.

If a message does not declare a `device`, its device is `message@1.0`. The
`message@1.0` device is the identity interpreter for ordinary resource
collections. For non-reserved keys it returns the direct public value at the key,
unless the value is `unset`, in which case the key is terminally masked at that
layer. If no direct value is present, `message@1.0` follows the `...` ancestry
and resolves the same request against the ancestor. A direct public key or
terminal mask in an outer layer shadows an inherited device or inherited key.

AO IDs are produced by resolving the `id` key of `message@1.0`. An ID is not a
hash of every byte in a runtime object; it is the output of an ID device over a
selected public surface. The default ID device is `httpsig@1.0`. A request may
select no committers, all committers, or a set of committers/commitment IDs:

```text
Message / { path: id, committers: none } -> unsigned ID
Message / { path: id, committers: all }  -> accumulated committed ID
```

Commitment selection for ID, verification, and committed-key queries uses the
following algorithm:

1. Normalize `committers` and `commitment-ids`. Each selector accepts `none`,
   `all`, a single normalized value, or a list of normalized values. If omitted,
   each selector is `none`.
2. `commitment-ids: none` selects no commitment IDs. `commitment-ids: all`
   selects every commitment ID in `Message.commitments`. Otherwise it selects
   exactly the named commitment IDs; a named commitment ID that is absent is an
   invalid request.
3. `committers: none` selects no commitment IDs. `committers: all` selects every
   commitment that has a `committer` field. Otherwise it selects every commitment
   whose `committer` equals one of the requested addresses; a requested committer
   with no matching commitment is an invalid request.
4. The selected commitment set is the set union of the IDs selected by both
   selectors. Duplicate IDs do not change the result.
5. If the selected set is empty, select existing unsigned commitments from the
   default commitment device: commitments whose `commitment-device` is
   `httpsig@1.0` and that do not have a `committer`. If none exist, generate a
   new unsigned ID commitment using the default ID device.

Current HyperBEAM source follows this selector shape, but some paths are more
permissive than the rule above. In particular, a missing named commitment ID can
be silently filtered in one ID path while later verification-style paths fail
when they fetch the missing ID. Portable AO-Core requests should treat missing
selected IDs as invalid. Current HyperBEAM also exposes these selector failures
mostly as thrown Erlang terms rather than as one normalized AO error shape; that
is a source-profile detail, not a portable error taxonomy.

If the selected commitment set is non-empty, every selected commitment must have
a `commitment-device`. If more than one distinct commitment device is present,
the ID request is ambiguous and invalid unless an explicitly selected
device-specific combination rule defines the combination. Implementations must
not accumulate commitment IDs across mixed commitment devices by default.

When selection falls back to a newly generated unsigned ID, the ID device creates
an unsigned commitment to the selected public surface and the unsigned commitment
ID is the message ID. When commitments are selected, the message ID is the
order-independent
accumulation of the selected commitment IDs. To accumulate, convert each selected
commitment ID to its native 32-byte value, start from 32 zero bytes interpreted
as an unsigned 256-bit integer, add each selected native ID modulo `2^256`, and
encode the resulting 32-byte value as the AO human ID. This allows new
commitments to be added without recalculating earlier commitment IDs, but it also
means the accumulated ID does not express an ordering among its component
commitments.

## Commitment Surfaces

A commitment is a public assertion about a selected surface of a message. It is
stored under the message's `commitments` key, keyed by the commitment ID. A
commitment record includes, at minimum, the `commitment-device`, the commitment
`type`, and the `committed` key list. Signed commitments also include a
`committer` and a device-specific proof such as a signature.

```text
Message.commitments = {
  CommitmentID: {
    commitment-device: httpsig@1.0,
    type: rsa-pss-sha512,
    committer: Address,
    committed: [Key1, Key2, ...],
    signature: Signature
  }
}
```

The commitment surface is the ordered set of values named by `committed`, encoded
by the commitment device. Portable AO commitment surfaces are public-only. A
commitment whose selected AO source surface contains a private key is invalid.
Commitment devices must reject explicit private committed keys, must reject
invalid stacked committed-key lists containing private keys, and must remove
private keys from default candidate sets before encoding. Verification must fail
for a commitment record that names a private key in its AO source surface,
regardless of whether the device-specific proof otherwise verifies.

The `commitments` key itself is not part of the signed value surface unless a
specific commitment device explicitly defines such a recursive commitment. A
portable response may still carry the selected public values together with their
commitment records so that another node can verify the surface.

The default `httpsig@1.0` commitment device uses HTTP Message Signatures
[[RFC9421]] as its commitment format. Its `type` controls the proof form:
`signed` resolves to `rsa-pss-sha512`, and `unsigned` resolves to a deterministic
`hmac-sha256` commitment used for unsigned IDs. A signed HTTPSig commitment ID is
`human_id(sha256(SignatureBytes))`. An HMAC/unsigned HTTPSig commitment ID is
`human_id(HMAC-SHA256(Key, SignatureBase))`; the HMAC bytes are also the
signature value for that commitment. The commitment record is stored under the
derived ID. When an HTTP signature-field representation transports a commitment
whose map key cannot be derived from its signature bytes, the `httpsig@1.0`
codec carries that commitment ID as an explicit `id` signature-input parameter
so decoding can preserve commitment selectors and map keys.

`httpsig@1.0` uses the following key-selection rule:

1. Normalize the requested or derived committed-key candidates to AO key names.
2. If the request supplies `committed`, commit exactly those keys only if every
   key is public; otherwise the request is invalid.
3. Else, if the message already has selected commitments with committed-key
   lists, reuse the keys common to those commitments so a new commitment can be
   stacked on the shared committed surface. If any reused key is private, the
   source commitment set is invalid for portable stacking.
4. Else, commit the encoded top-level public key set, excluding `commitments`
   and every key for which the private-key predicate is true. Current HyperBEAM
   source only excludes exact `priv` in this default path; that is a source gap
   against the portable rule above.

Encoding may transform the committed key list. For example, a binary `body` is
committed through `content-digest`, `ao-body-key` can recover which AO key
provided the body, linked values may appear through their `+link` encoded keys,
and HTTP derived component names may be percent-decoded after an optional `@`
specifier is removed. The stored `committed` list is therefore the commitment
device's encoded component list, not always the user's original key spelling.
Verification reconstructs the same encoded surface, invokes the commitment
device named by the commitment, and succeeds only if every selected commitment
verifies.

Commitment selection failure and proof failure are different outcomes. A missing
selected commitment ID, a requested committer with no matching commitment, a
commitment missing its `commitment-device`, a private key in the selected AO
source surface, or a missing encoded component needed for the signature base is
an invalid commitment request. A selected commitment whose device-specific proof
is present but does not verify is a verification result of `false`. Invalid
selection must not be treated as successful verification of an empty set.

Current HyperBEAM source is more permissive than that portable rule for some
legacy commitment records: `message@1.0` verification defaults a selected
commitment with no `commitment-device` to `httpsig@1.0`, and ID-device selection
ignores missing `commitment-device` fields before defaulting. That permissive
fallback is a current source gap; portable AO-Core commitments should name their
commitment device explicitly.

Commitment selection is part of many AO requests. `committers` selects
commitments by signer address, `commitment-ids` selects commitments by ID, and
`all` selects every commitment in that selector class. Callers may also
explicitly select `none`. `committers: all` and `commitment-ids: all` are
different selectors: the former selects commitments that have a signer address,
while the latter selects every commitment record. Unqualified selection is
device/API-specific: `message@1.0` selects the unsigned commitment from the
default commitment device when neither selector is provided, while higher-level
helpers may explicitly request all commitments.

The public helper operation `committed` returns the keys common to the selected
commitments after translating device-specific encoded keys back toward AO key
names where possible. `committers` returns signer addresses for selected signed
commitments. Unsigned commitments do not have a `committer`.

## message@1.0

`message@1.0` exposes the common protocol operations for ordinary resource
collections:

| Key | Meaning |
| --- | --- |
| `get` or any non-reserved key | Resolve a public key from the message or its ancestry. |
| `set` | Construct a new extension layer whose `...` points to the base. |
| `remove` (reserved/helper) | Mask one or more keys from the active layer; current HyperBEAM source exposes this through `hb_ao:remove/3` and `set(..., unset)`, not as an exported `dev_message` handler. |
| `keys` | Return public keys; deep key enumeration may include inherited public keys, but never private keys. |
| `id` | Return an ID for the selected commitment surface. |
| `commit` | Add a commitment using the requested commitment device. |
| `verify` | Verify selected commitments. |
| `commitments` | Store commitment records; not an ordinary application key. |
| `committed` | Return the public keys common to the selected commitments. |
| `committers` | Return committer addresses present in the selected commitments. |
| `vary` | Prepare varied witnesses for a computation; exact dependencies are present only when the selected profile produces them. |
| `schema` | Return schema information for device calls when available. |

The `path` key is request metadata: it selects the key to resolve and carries
remaining path segments during chained resolution. APIs may expose special
helpers for constructing or updating request paths, but those helpers are not
part of the portable message surface unless a device specification defines them.

`set(Base, Patch)` does not mutate `Base`. It constructs a new message layer
containing the patch keys and `...: Base`. A deep set recursively applies the set
operation to nested message-valued keys using those nested values' own devices.
Setting a key to `unset` masks that key at the new layer. Private keys from the
base may be preserved in the new local runtime layer, but this does not make them
portable public state. The public ancestry remains the unmodified base.

Removal is the deletion form of the same layering model. A removal produces a
new active layer that masks the named item or items by setting them to `unset`,
rather than rewriting the ancestor. Changing a public key that is covered by an
existing commitment makes that commitment irrelevant to the new public surface
unless the new layer still selects and verifies an equivalent committed value.

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

HyperBEAM currently has two source surfaces here: the rolling hashpath helper in
`hb_path`, and a richer draft assertion module in `hb_hashpath`.

### Current HyperBEAM Rolling Hashpath Profile

Current HyperBEAM source-backed hashpaths are rolling computation addresses. For
ordinary message execution, the initial hashpath of a base value is its AO ID; a
binary value uses a SHA-256-derived human ID. Applying a request derives the next
hashpath from the prior hashpath and the request ID using the base message's
selected `hashpath-alg`. If no algorithm is selected, the default is
`sha-256-chain`. A base may explicitly select another supported algorithm such as
`accumulate-256`, but that algorithm choice is part of the public semantics of
the resulting hashpath.

This profile supports address chaining and verification of a supplied
`[Base, Request, Result, ...]` history list. It does not by itself assert
`VariedBase`, `VariedRequest`, `Dependencies`, result mode, witness completeness,
loadability, or portable challengeability.

### Draft Rich Assertion Profile

The grammar, execution frames, loading rules, witness packages, and challenge
procedure below describe a draft AO-Core rich assertion profile. They are target
semantics, not current HyperBEAM source-backed behavior. The `hb_hashpath` module
in this branch shows source intent and is integrated into `hb_ao` stage 7, but
its parser, formatter, verifier, loader, resolver integration, and conformance
tests are not source-green. Implementations must not treat this rich profile as a
current HyperBEAM interoperability requirement until those pieces are
source-green.

Current source-green behavior for hashpath portability remains the rolling
`hb_path` profile. In the draft rich profile, `hb_hashpath` uses the textual
separators `/`, `>`, `+`, `@`, `=`, and `.` to carry request, Vary, Depends, and
result-mode components, but that module is a work-in-progress integration point.
Its `@` component is a pointer to an already-constructed `Dependencies` message;
the formatter does not discover or build Depends from execution by itself.

Rich hashpath loading follows the same active-layer model as message extension.
For each segment, resolution first checks the accumulated result of that segment.
If the requested value or selected `device` is not present there, an extension
segment may continue lookup through the prior accumulated result. A replacement
segment terminates that ancestry walk: it names a new accumulated result rather
than a patch over the earlier state.

This is not a third protocol operation called overlay. Extension constructs a
new layer whose `...` points to the prior result; replacement supplies the whole
new result. Through this mechanism, large states can be represented as small
patches over existing states while preserving provenance. In the rich profile,
Depends names the origin observations for the varied witnesses; the final
hashpath can point to that Depends message, but it does not discover those
origins after the fact.

Hashpaths represent a sequence of execution frames, each of which may contain a
number of elements:
1. A `Base` URI upon which the request is being made. Must always be present on
   the first context of the hashpath, but may be omitted for the second
   and further elements, instead having their `Base` inferred from the rolling
   context of the hashpath itself.
2. A raw binary or `Request` URI, holding the metadata of the request upon the
   `Base`.
3. A `Result`, optionally, if computed, the result of the computation as a
   further URI.
4. `Varied-Base` and `Varied-Request` elements, each naming the prepared witness
   collections for the transition. In an observed-exact AO-Core assertion these
   contain all public values observed by execution. In current HyperBEAM source,
   these witnesses are schema-declared projections and are not, by themselves,
   proof of observed-input exactness.
5. `Dependencies`, a collection naming the origin observations used to prepare
   those witnesses. In observed-exact AO-Core, this includes both positive value
   origins and negative observations such as absence, masking, defaulting, and
   failed device/key lookup. Current HyperBEAM hashpath formatting only emits a
   dependency component when a `dependencies` value already exists in the
   execution context; it does not construct that value.

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

### Base And Request Format

A single AO-Core resolution step is always evaluated as a `Base` and a
`Request`.

`Base` is the AO value being resolved. It is usually a message/resource
collection, or a URI/ID that resolves to one. If `Base` is a message and does not
declare a `device`, the active device is `message@1.0`. Private fields on the
base are local execution state; the protocol-visible base surface is the public
message, its links, its commitments, and its ancestry.

`Request` is an AO message describing the operation to perform against `Base`.
Its `path` key selects the requested key or device operation. Other public keys
in the request are arguments, selectors, or options for that operation, as
defined by the selected device. The `path` key is request metadata: it chooses
what to resolve, but it is not itself the target base key unless the selected
device explicitly resolves `path`.

The canonical one-step request form is:

```text
Request = {
  path: KeyOrOperation,
  ...additional public request fields...
}
```

For convenience, current HyperBEAM accepts several shorthands before constructing
the execution context:

1. A bare binary, atom, integer, or other path-like value used as a request is
   normalized to `{ path: NormalizedKey }`.
2. A `request-id` that is an AO ID is loaded as the request message. A
   `request-id` that is not an AO ID is treated as a literal path and normalized
   to `{ path: RequestID }`.
3. A request whose `path` has multiple parts is expanded into a sequence of
   one-step requests. Each generated step sets `path` to one part and preserves
   the original request as ancestry, so non-path fields remain available to the
   selected device.

Thus:

```text
Base / transfer
Base / { path: transfer, quantity: 3 }
Base / { path: [debit, credit], quantity: 3 }
```

are not three different resolution relations. They are shorthands or structured
forms for one or more ordinary `Base / Request` steps.

Current HyperBEAM also exposes helper APIs that are not separate AO-Core
relations. `resolve([M1, M2, ...])` repeatedly evaluates adjacent pairs, feeding
each result into the next request. `raw(Device, Key, Base, Request)` bypasses
normalization, cache, worker, and hashpath management for an internal device
call. `as(Device, Message)` constructs a local extension layer that forces the
active device. These helpers are useful implementation surfaces, but portable
claims are still judged as one or more ordinary `Base / Request` transitions.

For a request whose `path` is `P`, resolution walks the ancestry of `Base` from
the outermost layer inward until either a direct-accessible result, a `device`,
or a non-extending hashpath context is found.

At each layer:

1. Look for `BaseURI/P` among the resources asserted directly at this layer.
   Return if found and direct access is permitted for `P` under the active
   device profile.
2. Look directly for `BaseURI/device` at this layer.
3. If `Device` is found, look up a runtime-compliant implementation and apply it
   against the original outermost `Base` and the original `Request`.
4. Else, if a prior extending element of the hashpath context is found, recursively
   resolve at that element.

In the pseudocode below, `lookup` inspects direct assertions in the loaded layer.
It does not recursively perform AO execution. A `not_found` error is scoped to
the resource searched. Direct public key hits return before device execution
only when the active device profile classifies the key as direct-accessible. For
`message@1.0`, this means non-reserved public keys; reserved keys such as `set`,
`keys`, `id`, and `verify` dispatch through the message device even if a literal
field of the same name exists. `unset` is a terminal mask at the active layer
rather than a signal to inherit the ancestor's value. Private keys must not be
returned by this direct lookup. Current HyperBEAM source recognizes the binary
sentinel `<<"unset">>` in the stage-1 direct lookup path, while some helper paths
still construct atom `unset`; that mismatch is a source gap against a single
portable sentinel.

```text
resolve(Outer, BaseURI, Request):
  P = path(Request)

  case lookup(BaseURI/P) of
    {ok, Response} when direct_accessible(BaseURI, P) ->
      return {ok, Response}
    {ok, _ReservedOrDeviceHandledValue} -> continue
    {error, not_found} -> continue
  end

  case lookup(BaseURI/device) of
    {ok, Dev} -> return execute(Dev, Outer, Request)
    {error, not_found} -> continue
  end

  case local(BaseURI/...) of
    {ok, Ancestor} -> return resolve(Outer, Ancestor, Request)
    {error, not_found} -> return {error, not_found}
  end
```

If a device is inherited from an ancestor, it executes over the outermost state:

```text
{ x: 5, ...: { device: dev1, x: 1 } } / x-is-5
```

selects `dev1` from the ancestor, but `dev1` sees `x = 5`.

If the selected device is itself a message, current HyperBEAM treats device
execution as another resolution: the device message is extended over the
outermost base and the requested key is forced for that inner call. This is a
HyperBEAM profile for message-valued devices; a portable device specification
must still define the AO-visible behavior of the keys it resolves.

### HyperBEAM Resolver Profile

HyperBEAM's resolver implements the protocol relation above as a staged local
pipeline. The stage list is informative: a conforming AO-Core implementation
does not need the same internal phases, but it must expose the same successful
resolution, identity, and hashpath effects.

| Stage | Local operation | Protocol-visible effect |
| --- | --- | --- |
| 1 | Normalize context; perform device or direct key lookup. | Keys, path, request form, and selected device are put into AO form, or a direct key hit returns early. |
| 2 | Look up the device function. | The selected key handler is fixed for the varied/executed transition. |
| 3 | Vary `Base` and `Request`. | Current HyperBEAM prepares schema-declared `VariedBase`, `VariedRequest`, and result normalization. Exact AO-Core `Dependencies` are target protocol state and are not produced by this current stage. |
| 4 | Persistent resolver lookup. | An in-flight equivalent execution may be joined instead of recomputed. |
| 5 | Cache lookup. | A known result for the varied pair may be reused by address. |
| 6 | Execute the handler. | `VariedBase / VariedRequest -> RawResult` is computed when no reusable result exists. |
| 7 | Apply the selected normalizer and construct the hashpath result. | Extension/replacement semantics are applied. Draft rich hashpath result construction is an integration point, but rich cryptographic linkage is target semantics until the hashpath implementation is source-green. |
| 8 | Cache the fresh successful varied result. | Future requests may reuse the result. |
| 9 | Notify waiting callers. | Local concurrency bookkeeping. |
| 10 | Run the `step` hook. | Local observers may inspect or adapt the completed context; hooks are not hidden inputs unless their output changes the public result. |
| 11 | Fork a worker if requested. | Local execution strategy. |

Stages 4, 5, 8, 9, 10, and 11 are implementation mechanisms. They must not add
unrecorded public inputs to the transition. Stage 6 is the ordinary device
execution point after Vary has prepared the execution context; it is not a
separate protocol operation replacing Vary or Depends.

## Devices

A device defines how requests are computed for a state.

Examples of devices include message interpreters, process VMs, WASM VMs,
Lua VMs, codecs, payment devices, and application-specific evaluators.
AO-Core does not privilege one compute model over another. It standardizes how
their transitions are named, witnessed, attested, transported, and traced.

Every device specification defines a set of keys it can resolve and the public
semantics of those resolutions. All devices must provide a way to obtain an ID
for values they produce and a way to enumerate public keys when enumeration is
meaningful for the value. A concrete implementation may expose a direct handler,
per-key functions, a default handler, or a fallback to `message@1.0`; those are
implementation choices so long as the AO-visible key semantics match the device
specification.

Current HyperBEAM source accepts loaded message-valued devices, binary
references that resolve to device specification IDs or names, and internal
Erlang module references during bootstrap/loading. URI-shaped device references
are an AO-Core design surface, but they are not treated here as source-proven
HyperBEAM loader behavior.

## Device Specifications and Implementations

AO-Core distinguishes a device specification from a device implementation. The
specification is the protocol identity of the device: it defines the public
semantics that computation claims are judged against. An implementation is a
machine-local artifact that can execute those semantics on a particular runtime.
Different implementations are compatible only if they implement the same
specification and produce the same AO-visible results for the same varied inputs.

A device reference resolves to a specification ID. In current HyperBEAM source,
the reference may already be a specification ID or it may be a human-readable
name such as `message@1.0`. Name resolution is performed through `name@1.0`:
ordered resolver messages are asked for the name, and the first resolver that
yields a value wins. A resolver may return the value directly or return an
ID/link that is loaded according to the request.

```text
DeviceRef =
  SpecID
| Name@Version

Name@Version / name@1.0 -> SpecID
```

A device specification message has the following AO-Core target shape. Current
HyperBEAM loading primarily uses the committed `SpecID` and implementation
messages' `implements-device` reverse edge; it does not yet validate every field
below as a universal source-green requirement.

```text
DeviceSpecification = {
  data-protocol: ao,
  variant: ao.N.1,
  type: Device-Specification,
  name: Name@Version,
  content-type: SpecContentType,
  body: SpecificationBody
}
```

The committed ID of this message is the `SpecID`. Nodes may keep a preloaded
resolver message that maps each built-in `Name@Version` directly to its signed
`SpecID`. External name providers may publish equivalent resolver messages.

A device implementation message has the following minimum public shape:

```text
DeviceImplementation = {
  data-protocol: ao,
  variant: ao.N.1,
  implements-device: SpecID,
  content-type: ImplementationContentType,
  body: ImplementationPayload
}
```

The `implements-device` key is the reverse edge from an implementation to the
specification it claims to implement. The implementation ID is the committed ID
of the implementation message. A runtime may define additional public metadata
for its implementation format, such as module names, archive formats, runtime
versions, or architecture requirements. Those fields are part of that runtime's
device-loading profile, not the universal AO-Core device specification model.
Runtimes may add a `type` field for their own package profile; current
HyperBEAM BEAM implementation messages do not require one.

## Device Implementation Lookup

To execute a device reference, a node performs two lookups:

1. Resolve the device reference to a `SpecID`.
2. Find an admissible implementation whose public `implements-device` value is
   that `SpecID`.

The protocol-visible invariant is that claims about a device execution are
judged against the resolved device specification, not against an arbitrary local
module name. A node may choose any implementation that it trusts to implement
the resolved specification. If an implementation is presented as portable
evidence, its commitment must verify and its `implements-device` value must equal
the resolved `SpecID`.

### HyperBEAM Device Loading Profile

HyperBEAM's current Erlang runtime profile packages device implementations as
signed BEAM archive messages. That profile uses the following implementation
shape:

```text
HyperBEAMDeviceImplementation = {
  data-protocol: ao,
  variant: ao.N.1,
  content-type: application/beam-archive,
  archive-format: zip,
  implements-device: SpecID,
  module-name: RuntimeModuleName,
  requires-otp-release: OtpRelease,
  requires-system-architecture: SystemArchitecture?,
  body: ImplementationArchive
}
```

HyperBEAM implementation search order is local policy:

1. A reference that is already a loaded device message may be used directly.
2. A reference that is an ID is treated as a `SpecID`.
3. A non-ID reference is resolved through `name@1.0` with loading disabled, so
   the result is the `SpecID` rather than the specification body.
4. Operator-pinned implementation IDs may be trusted directly for a specific
   name or `SpecID`.
5. A preloaded store may contain signed device specifications, signed
   implementations, and a flat name-to-`SpecID` resolver message.
6. Local caches and remote stores may be searched for messages matching:

   ```text
   {
     data-protocol: ao,
     variant: ao.N.1,
     content-type: application/beam-archive,
     implements-device: SpecID
   }
   ```

7. Remote implementation lookup is attempted only when `trusted-device-signers`
   is configured as a non-empty list. A signer policy may name an address and
   may scope the signer to specific device names or specification IDs.
8. A low-trust implementation is loadable only if its commitment verifies, one
   of its committers is trusted for the requested device, its `implements-device`
   value equals the resolved `SpecID`, and each `requires-*` key is compatible
   with the local runtime.

Low-trust local-cache and remote implementation candidates use the same
verification gate. Remote gateway lookup is attempted only when
`trusted-device-signers` is explicitly configured as a non-empty list, but the
trusted-signer helper itself defaults to the node's own address when no signer
configuration is present. This is a HyperBEAM trust policy, not an AO-Core
requirement.

HyperBEAM generated `_hb_device_*` module names are derived from the unsigned
source-file message ID. That naming scheme prevents BEAM module collisions and
makes package identity reproducible inside the HyperBEAM runtime. AO-Core does
not require other runtimes to use BEAM, ZIP archives, or the same loading order.

## AO Process and Scheduler Profile

The process and scheduler devices are not additional AO-Core primitives. They
are standard device specifications built on messages, devices, paths, and
hashpaths. They are included here because the AO system relies on them as the
common profile for persistent shared computation.

A `process@1.0` message is a process definition. It usually does not execute the
application program directly. Instead, it names the devices responsible for the
process lifecycle:

| Process key | Role |
| --- | --- |
| `scheduler-device` | Determines the order of inbound assignments. |
| `execution-device` | Applies the next scheduled assignment to the current process state. |
| `push-device` | Injects new messages into the schedule. |
| `execution-stack` | Process-level ordered execution configuration when execution uses a stack-like execution device; the stack device itself reads `device-stack`. |

The process device is therefore an orchestrator. It routes `schedule`, `compute`,
`now`, `slot`, `snapshot`, and related keys to the configured scheduler,
execution, and push devices by changing the active device context while
preserving AO-Core path/hashpath semantics.

Current HyperBEAM process identity is the committed ID of the public `process`
message embedded in or reconstructed from the live state. Before deriving that
ID, the process helper verifies the process message's commitments and requires
at least one signer; an unverifiable process or a process with no signers is
invalid for the current profile. The caller's request may choose the commitment
selector used for the process ID through its `commitments` field, defaulting to
the signed process ID. Scheduler lookup, process cache keys, compute state, and
slot state are keyed from this process ID.

The scheduler profile gives a process a deterministic linear schedule. A slot is
a numbered position in that schedule. An assignment is the message occupying a
slot, together with the cryptographic material needed to verify that assignment
under the process's scheduling policy. The scheduler exposes keys such as:

| Scheduler key | Role |
| --- | --- |
| `schedule` (GET) | Return known assignments, often with cursor-like traversal. |
| `schedule` (POST) | Add a new assignment to the schedule or initialize scheduling for a new process. |
| `slot` | Query current or requested slot information. |
| `next` | Return the next assignment for process execution. |
| `status` | Return scheduler status information. |

`compute` resolves process state up to a requested slot or message. `now`
returns the latest computed process state, either from the current slot or from
cache depending on node options. Callers may then resolve `results` or any other
key on that returned state. Both operations are ordinary AO-Core computations:
cache hits and snapshots may avoid recomputation, but the returned value still
has AO identity and current rolling hashpath addressability. Rich local challenge
through full Vary/Depends/hashpath assertions is draft target behavior until the
rich profile is source-green.

Stack-style execution composes devices over the same base/request pair. In fold
mode, each device receives the state produced by the previous device. In map
mode, devices may execute separately and combine their outputs according to the
stack specification. Lua and WASM devices typically store live VM handles,
instance state, memory readers/writers, and sandbox state in `priv` fields.
Those handles are local execution aids; portable process state is the public
message surface, snapshots, commitments, and hashpaths that can be reloaded or
challenged by another node.

For current HyperBEAM, "challenged" here means through source-green commitment,
ID, process, and rolling-hashpath checks; full rich per-transition challenge is
the draft profile described below.

## Vary

Device application has two phases. First, the selected device prepares an
executable context containing the local function to invoke, the minimized
`Varied-Base` and `Varied-Request`, the result mode, and, for an observed-exact
claim, their `Dependencies`. The protocol-visible portion of exact preparation
is:

```text
Base / Request / vary -> VariedBase + VariedRequest @ Dependencies
```

Implementation status: current HyperBEAM `vary` is schema-declared preparation.
If no schema is available, `hb_types:vary/2` returns the original `Base` and
`Request` and records `normalizer: none`. If a schema is available, it projects
`Base` and `Request` through `apply_schema/3`, records the selected normalizer,
and, when rich hashpaths are enabled, produces schema-declared dependency
observations. It does not currently produce an observed-exact `Dependencies`
tree.

Depends is not an independent post-hoc overlay. It is the origin-observation
side of Vary: for every public value, absence, mask, default, or lookup failure
that an observed-exact witness depends on, Depends records where that
observation came from. A runtime may compute it by instrumentation, device
semantics, or another proof system, but a rich assertion cannot infer exact
Depends merely from the final result hashpath. The final hashpath can name the
Depends message if it exists; it cannot prove observed-input coverage unless the
Depends tree and supporting witness values are available.

`VariedBase` and `VariedRequest` are the selected device's execution witness:
the public base and request surface that the prepared function is executed
against. A Vary claim has a claim level.

Schema-declared variation means the witness is produced by applying the selected
device's declared schema to `Base` and `Request`. It is exact only with respect
to that declaration; it may include values not read by a particular execution,
and it does not by itself prove that the schema is the complete observed input
set.

Observed-exact variation means the witness is backed by device semantics,
instrumentation, or another proof sufficient to show that every public value,
absence, default, mask, or failed lookup observed by execution is represented in
the varied witness and dependency tree.

Witness collections use canonical nested structure, not path strings:

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

`Dependencies` records where each varied value or relevant negative observation
originated. It has the same shape as the positive varied collections, rooted
under `base` and `request`, with optional additional leaves for negative
observations that affected preparation or execution:

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

A dependency leaf is an AO message describing one observation:

```text
{ status: found,     origin: Hashpath }
{ status: found,     origin: Hashpath, observed: ObservedValue, value: VariedValue }
{ status: not_found, origin: Hashpath, path: Path }
{ status: unset,     origin: Hashpath, path: Path }
{ status: defaulted, origin: Hashpath, path: Path, default: DefaultID }
{ status: error,     origin: Hashpath, path: Path, error: ErrorValue }
```

A bare hashpath may be used only as shorthand for
`{ status: found, origin: Hashpath }`. For `found`, the origin hashpath's
terminal value is the varied value, so the value does not need to be duplicated
inside `Dependencies`. If preparation projects or coerces an observed value into
a different varied value, the `found` leaf MUST use the explicit
`observed`/`value` form: `observed` is the value resolved at `origin`, and
`value` is the value included in `VariedBase` or `VariedRequest`. A verifier
checks both that `origin` resolves to `observed` and that the varied witness
contains `value`. Negative leaves do not correspond to a varied value; they
record observations that affected preparation or execution, including absence,
masking by `unset`, default selection, or failure to find a direct key or device.

Dependency origins inherit the hashpath claim-strength model. An address-only
origin can name where a local or trusted runtime observed a value, but it is not
portable proof of that value. A dependency leaf used for portable challenge must
either be a full or uniquely expandable origin assertion, or be accompanied by
the committed value and witness material needed to verify the observation under
the receiver's trust policy.

Portable Depends uses the same nested message shape as the witness, not a flat
list of paths. The bare hashpath leaves in the `Dependencies` example above are
the shorthand form. If a lookup observes absence or masking, the corresponding
leaf must be the explicit observation message rather than a bare hashpath.

For an observed-exact Vary claim, every positive leaf in `VariedBase` and
`VariedRequest` must have a corresponding `found` dependency leaf. Additional
dependency leaves are valid only when they record observations that affected
preparation or execution. Negative leaves do not assert a value; they assert that
repeating the same lookup observes the same absence, mask, default, or failure.

If no observed-exact vary specification is available, the conservative valid
schema-declared vary is identity:

```text
VariedBase = Base
VariedRequest = Request
```

Identity variation does not waive dependency recording for an observed-exact
claim. For a schema-declared claim, identity variation means the declared witness
is the complete `Base` and `Request` surface, not that all observed origins are
known.

The core rule is:

```text
No hidden inputs.
```

A conforming transition must not depend on public inputs outside the asserted
witness for its claim level. For schema-declared variation, challengers verify
the selected schema and its application. For observed-exact variation,
challengers also verify the claimed observation coverage and dependency leaves.

## Shared Computation

The second phase of device application begins at the reusable computation point
created by varying.

Many concrete bases may vary to the same pair:

```text
BaseA / Request / vary -> VariedBase + VariedRequest @ DependenciesA
BaseB / Request / vary -> VariedBase + VariedRequest @ DependenciesB
```

The execution:

```text
VariedBase / VariedRequest -> VariedResult
```

names a reusable computation together with the selected device and result mode.
A runtime may satisfy that computation by joining an equivalent in-flight
execution, loading a previously verified result, executing the prepared function,
or using another local strategy.

Reuse is valid only when the candidate `VariedResult` is verified, or trusted
under the verifier's policy, as the result of the same varied computation and
result mode. Cache lookup and cache storage are implementation policy; they are
not required protocol steps and do not affect transition validity.

For extension, `VariedResult` is a patch. Final results may still differ because
the patch is applied to each original base:

```text
BaseA / Request == set(BaseA, Patch)
BaseB / Request == set(BaseB, Patch)
```

For replacement, `VariedResult` is the final result and is shared directly.

This is the default reuse property of AO-Core computation: the varied pair names
a reusable computation, and runtimes may reuse verified or trusted results for
base/request pairs that vary to those inputs and satisfy the same claim level.

Both the varied computation and complete transition results may be addressed and
stored by hashpath or ID. A cache is one way to make such results available, but
the protocol requirement is that reused results remain challengeable as the same
transition.

## Transition Equivalence

A transition asserts an equivalence between resolving a request and applying the
selected result mode to the varied result:

```text
Base / Request ==
  extension:   set(Base, VariedResult)
  replacement: VariedResult
```

where:

```text
Base / Request / vary -> VariedBase + VariedRequest @ Dependencies
VariedBase / VariedRequest -> VariedResult
```

AO-Core has two protocol result modes:

1. `extension`: `VariedResult` is a patch applied as
   `set(Base, VariedResult)`, constructing a new layer whose ancestry is the
   original `Base`. Hashpaths serialize this mode with `=`.
2. `replacement`: `VariedResult` is the accumulated result. Active inheritance
   from the prior `Base` terminates. Hashpaths serialize this mode with `.`.

Implementation normalizer names are not protocol vocabulary. In current
HyperBEAM source, `base` corresponds to protocol `extension`. Replacement-like,
omitted, or draft `replace` normalizers correspond to protocol `replacement`.
Names such as `request` and `none` are implementation or schema artifacts unless
a device specification explicitly defines protocol-visible semantics for them.
AO-Core applies the mode; devices do not implement ancestry traversal
themselves.

Current source has a naming mismatch here: `hb_types` may produce `normalizer:
none` when no schema is available, while `hb_hashpath:result_from_context/3`
treats unsupported normalizers as errors unless they are mapped to a protocol
mode. The portable rule is the two-mode `extension`/`replacement` relation
above; source normalizer atoms are local implementation details that must be
adapted before emitting a rich assertion.

## Hashpath Assertions And Claims

A hashpath without a terminal result is an address naming a computation. A
hashpath containing its result is an assertion. A signature over one or more
hashpaths is a claim. Like any other value, a hashpath may be encountered as a
link and resolved through AO-Core, or encountered as serialized content and
decoded into its in-memory form.

Hashpath values have claim-strength levels:

1. Address-only: names a computation path. It may be a current HyperBEAM rolling
   hashpath or a rich-profile path without terminal assertion material. It does
   not claim the result, dependencies, or challengeability.
2. Compact derivable assertion: includes a terminal result but omits fields only
   when the receiver can reconstruct one unique full assertion from context or a
   witness package. Missing non-derivable fields downgrade the value to a weak,
   non-challengeable assertion.
3. Full rich assertion: includes `BaseID`, `ReqID`, `VariedBaseID`,
   `VariedReqID`, `DependenciesID`, result mode, and `PatchID` or `ResultID`.
   This is the minimum level for local transition challenge under the rich
   profile.
4. Signed claim: a signature over one or more expanded assertions. The signature
   proves attribution and integrity of the assertion text; correctness still
   depends on successful challenge, prior verification, or the verifier's trust
   policy.

The full transition forms are:

```text
BaseID/ReqID>VariedBaseID+VariedReqID@DependenciesID=PatchID
BaseID/ReqID>VariedBaseID+VariedReqID@DependenciesID.ResultID
```

Each component in the full form is an AO ID or URI-safe reference to an AO value.
The separators `/`, `>`, `+`, `@`, `=`, and `.` are structural separators in the
hashpath grammar. If a component is serialized into a textual `ao://` URI, any
component data that would collide with those separators is encoded according to
the URI encoding rules of [[RFC3986]]. The expanded full assertion, not a
transport-specific pretty form, is the value used for challenge and claim
construction.

The `>` component names `VariedBaseID + VariedReqID`. The `@` component names
`DependenciesID`. `=` and `.` are not arbitrary punctuation: they select the
protocol result mode, with `=` for extension/patch application and `.` for
replacement/materialized result.

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
but the full assertion must be recoverable for challenge and trace. Fields are
derivable only when the receiver can reconstruct the exact same full assertion
from the surrounding hashpath, the request context, or the witness package. A
compact assertion that cannot be expanded to one unique full assertion is not
challengeable and must be treated as address-only or weak assertion material.

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

The loading rules in this section apply to full rich assertions or compact
assertions that are uniquely expandable to a full rich assertion. Current
HyperBEAM rolling hashpaths are computation addresses and do not, by themselves,
provide the loading guarantees described here.

Hashpaths are not a storage system outside AO-Core. They are addressable values
whose rich loaded form reconstructs resource-prefix and extension semantics.

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

This gives portability as a single addressable value for the rich profile:
posting a full or uniquely expandable hashpath plus the values needed to resolve
the IDs it names gives another node enough data to reconstruct the result
collection, challenge any full segment, and continue computation.

Because hashpaths and message IDs are immutable references, gateways may publish
shortcuts for previously verified computations. In HTTP this is naturally
expressed as a permanent redirect from a computation path to a cached hashpath or
materialized result. A redirect is only an optimization: it does not replace the
hashpath assertion or the receiver's ability to challenge it.

## Witness Packages

A portable computation package is an AO message that carries a hashpath and the
supporting values needed to resolve the IDs named by that hashpath. The package
format may be transport-specific, but its contents are protocol-visible. For each
full rich transition segment that the receiver is expected to load, challenge, or
continue from, the package must make the following values available directly or
through resolvable links:

1. `BaseID` or the prior hashpath result.
2. `ReqID`.
3. `VariedBaseID`.
4. `VariedReqID`.
5. `DependenciesID`.
6. `PatchID` or `ResultID`.
7. Any commitment records needed to verify those IDs under the receiver's trust
   policy.

The package does not need to eagerly include the full transitive provenance tree
for every dependency. Atomic challenge only needs the values for the challenged
transition. A full audit recursively asks for the dependency hashpaths named in
`Dependencies`.

Witness values may be deduplicated by ID. If two hashpath fields name the same
ID, one copy of the value is sufficient. Missing witness values make the package
partial: a receiver may fetch them from local stores, peers, or Arweave, but it
cannot claim that the package alone is complete until every named ID needed for
the requested operation resolves.

## Challenge And Audit

A full rich transition assertion can be challenged locally, without verifying
its entire dependency tree. The usual practical operation is to pick one full
assertion and verify only the values needed to reproduce it. A full provenance
audit is the recursive version of the same process. Address-only and
non-expandable compact hashpaths do not carry enough information for this
challenge procedure.

To challenge a transition assertion:

1. Verify that `BaseID` and `ReqID` identify the asserted values.
2. Repeat the selected device's preparation phase and verify its asserted
   `VariedBase`, `VariedRequest`, `Dependencies`, claim level, and result mode:

   ```text
   Base / Request / vary -> VariedBase + VariedRequest @ Dependencies
   ```

3. For every positive leaf in `VariedBase` and `VariedRequest`, follow the
   matching `found` dependency leaf and verify that it yields that value. For
   every negative dependency leaf, repeat the named observation and verify the
   asserted `not_found`, `unset`, `defaulted`, or `error` status.
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
3. Follow every corresponding `found` leaf in `Dependencies`; for negative
   dependency leaves, trace the observation that affected preparation or
   execution.
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

and returns an HTTP response containing the encoded result. Depending on the
selected response profile, the response may also carry a result assertion,
signed claims, or supporting values needed for the receiver to verify, port, and
continue the computation.
When present, supporting values are the HTTP expression of a witness package:
they should include, or link to, the values named by the returned hashpath that
are needed for the requested level of verification or continuation.

Thus HTTP gives AO-Core a universal transport and user-facing syntax while the
protocol remains independent of HTTP itself.

### Singleton Request Decoding

The HyperBEAM HTTP profile decodes an incoming HTTP request into a singleton
TABM message, then expands that singleton into an ordered list of AO-Core
messages to resolve.

| HTTP form | AO-Core meaning |
| --- | --- |
| `/Part1/Part2/...` | Each path segment becomes one request message in order. |
| `/ID/Part2/...` | If the first segment is an AO ID, it is the base value to load; later segments are requests. |
| No base ID | The request message is its own base, and the path is applied to that request. |
| `Part&Key=Value` | Add `Key: Value` to the request message for `Part`. |
| `Part&Key` | Add `Key: true` to the request message for `Part`. |
| `Part=Value` | Shorthand for `Part&Part=Value`. |
| `Part~Device` | Switch the device for that step; current HyperBEAM source expands this into a device-setting step plus the step message. |
| `Key+type=Value` | Decode `Value` using the named structured type before resolution. |
| `Key+resolve=(/A/B)` | Resolve the parenthesized subpath and use its result as `Key`. |
| `N.Key=Value` | Scope `Key` to the Nth path step rather than every step. |
| `(/A/B)` | Resolve `/A/B` independently and use its result as this path component. |

Query parameters and HTTP headers are equivalent AO message fields after codec
normalization. Unscoped fields are applied to every step. Scoped fields use
`N.Key`, where the first request step is `1` in the user-facing syntax. HTTP
method and body are also fields in the singleton message; codecs decide how a
body is represented as an AO value.

The current HyperBEAM singleton profile uses `+type` for typed fields. Older
HTTP API notes also mention `Key|Type`; implementations may support legacy
syntax, but portable examples in this specification use `+type`.

The draft rich hashpath grammar can represent terminal materialization as:

```text
HP/*=MaterializedID
```

This assertion is not special in the hashpath calculus. It is the ordinary
materialization request for `*`, tying a transport response to the concrete
enumerated keys and values returned to the client when the response profile
chooses to include it.

When HyperBEAM response signing is forced and the result carries a private
hashpath, the HTTPSig HTTP response profile first projects that private hashpath
onto the public `hashpath` field. It then adds two independently useful
commitment surfaces:

1. An unsigned HTTPSig commitment over the response's selected encoded surface.
2. A signed HTTPSig commitment over the public `hashpath` field, after the
   private hashpath is copied to that field.

The first commitment lets the returned value stand alone by ID. The second
attributes the public `hashpath` field. It does not, by itself, make the response
a complete rich challenge package; challengeability still depends on the returned
hashpath's claim strength and on the availability of the supporting values named
by that hashpath. The two commitments can be used together or independently.

## Extension Points

AO-Core is extensible through named devices, codecs, commitment devices, hashpath
algorithms, and transport profiles. An extension is portable only to the extent
that it defines its AO-visible behavior, canonical encoding, identity surface,
and trust requirements.

A device extension should publish a committed device specification and use
`implements-device` on concrete implementations that claim to implement it. A
codec extension must define how external bytes are converted to and from the
AO-visible TABM surface, including generated codec keys and commitment metadata.
A commitment-device extension must define commitment selection, canonical
signature or proof input, ID derivation, verification, and private-key rejection.
A hashpath-algorithm extension must define the native hash operation used for
rolling address construction. Rich hashpath result modes are not open-ended in
this draft: portable transition equivalence uses only `extension` and
`replacement`.

Until a formal registry exists, portable specifications should identify
extensions by committed specification ID, stable name, version, and, when useful,
URI. Runtime-local module names, cache keys, worker IDs, and operator-pinned
shortcuts are implementation details, not portable extension identifiers.

## Security And Privacy

Commitments prove only the committed public surface. They do not prove that a
device implementation is correct, that a result is valid, or that omitted fields
were irrelevant. Receivers must verify the IDs, commitment records, selected
device identity, varied witnesses, dependencies, and result mode required for the
claim strength they rely on. Reused cache entries and redirected HTTP responses
are safe only when they resolve to the same verified or trusted AO value and
transition claim.

Private state is local. Encodings, commitments, witness packages, and portable
hashpath claims must not depend on private keys unless the private-derived value
is explicitly projected onto a public key and then committed or witnessed like
ordinary public state. Projecting a private hashpath onto the public `hashpath`
field can make that address attributable, but it also reveals the computation
address and still does not create a complete challenge package.

Observed-exact Depends improves auditability, but it can reveal more than the
final result: exact dependency trees may expose which balances, device lookups,
absences, defaults, or failed paths influenced execution. Implementations should
choose claim levels deliberately, avoid recording secret material as public
dependencies, and treat negative observations as potentially sensitive metadata.

Device loading is a trust boundary. A node may execute local preloaded devices or
operator-pinned implementations by policy, but portable low-trust implementation
loading requires a committed implementation whose `implements-device` value
matches the selected specification and whose signer is trusted for that device.
Remote stores, caches, and gateways are delivery mechanisms; they must not be
treated as authority without ID, commitment, and policy checks.

HTTP signing profiles inherit the same limits. A signed HTTP request or response
authenticates the selected HTTP signature surface. It does not automatically sign
every AO-visible value unless the codec and commitment device define that surface
to include those values. Legacy HTTP syntaxes may be accepted for compatibility,
but portable examples should use the canonical forms in this specification.

## Summary

AO-Core turns computation into portable, challengeable, traceable message
transitions.

Devices provide the compute. Observed-exact Vary provides complete public
witnesses; current HyperBEAM Vary provides schema-declared witnesses. Depends
provides origin and negative-observation trace when an exact dependencies
message is present. Commitments provide public surfaces and IDs. Device
specifications provide protocol identity; implementations provide local
execution. Rolling hashpaths provide current HyperBEAM computation addresses;
rich hashpaths provide portable claims once their full assertion profile is
source-green. Extension provides deduplicated state. HTTP provides a practical
expression layer.
