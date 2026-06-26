# AO-Core 1.0 Minimal Semantics

AO-Core is a protocol for attestable computation over messages.

The only primitive relation is membership:

```text
Base / Request -> Result
```

Everything else is syntax, storage, caching, transport, or implementation.

## Required Properties

1. **All computations must be atomically attested**: Upon receipt of a succinct committed string, we must be able to identify a set of computation result claims, and then _individually_ verify if each is true, without dependence on executing any other computations in the results.

2. **All computations must be succinctly portable**: Any user wishing to move a computation from one machine to another should be able to take a hashpath and post a single message to another node, effectively duplicating the state and allowing for further computation elsewhere. By the nature of extension, complex collections of keys and values must be efficiently loadable from smaller, more common subsets.

3. **All computations must be fully traceable**: Given any result, the full set of computations that gave rise to each of its values must be enumeratable. Receipt of a single value should allow you to efficiently isolate every strand of other computations, no matter how distant or tangentially related, whose results were dependencies of the given value.

## Values

A value is a literal, message, or link.

A message is a device-interpreted value with public keys, optional commitments,
optional private runtime state, and optional ancestry.

`priv` is runtime-local. It is not part of IDs, public commitments, signatures,
or protocol equality.

A link names another value. Loading a link must preserve the same protocol
value.

## Extension

`...` is ancestry.

```text
{ a: 2, ...: { a: 1, b: 3 } }
```

has active `a = 2` and active `b = 3`.

Outer layers shadow inner layers. `unset` masks an inherited key.

`set(Base, Patch)` constructs extension:

```text
{ PatchKeys..., ...: Base }
```

It does not mutate `Base`.

Pathless composition is set:

```text
AO(Base, PatchWithoutPath) = set(Base, PatchWithoutPath)
```

A sequence is a fold:

```text
AO([M0, M1, M2]) = AO(AO(M0, M1), M2)
```

## Devices

A device defines memberships for request keys.

If no device applies, the default device is `message@1.0`.

Resolution through extension is layer-ordered. An outer layer interpreted by
`message@1.0` can return its own direct key before an inherited device is
reached. A layer that declares a device is interpreted by that device.

Implementation artifacts such as Erlang modules and function pointers are
node-local and must not be public protocol facts.

## `message@1.0`

`message@1.0` is the default message device.

Reserved keys include:

```text
get, set, remove, *, keys, id, commit, verify,
commitments, committed, committers, vary, schema
```

For non-reserved keys, `message@1.0` resolves:

```text
local public key -> value
local unset      -> not_found
otherwise        -> resolve through ...
```

`*` materializes active keys:

```text
M/* -> { ActiveKeys..., ...: M }
```

`remove(K)` is `set({ K: unset })`.

`id` commits to the selected public surface, never to `priv`.

## Vary

Execution is prepared before it runs.

Preparation is itself an AO-Core membership. For a transition:

```text
Base / Req > VarBase + VarReq = Res
```

the `>` clause means:

```text
Base / vary(Req) -> { base: VarBase, request: VarReq }
VarBase / VarReq -> Res
```

`vary` is device-owned. It determines the exact base and request witnesses used
for execution. Execution must observe only those witnesses.

A varied message may materialize keys, preserve links, or preserve extension,
but it must not hide dependencies. Any value it exposes must remain traceable
to the membership claim that produced it.

## Hashpaths

A hashpath is a succinct executable claim string.

```text
Base/Req>VarBase+VarReq=Patch
```

means the execution produced an extension patch. The accumulated state becomes:

```text
set(Base, Patch)
```

```text
Base/Req>VarBase+VarReq.Result
```

means the execution produced a full replacement. The accumulated state becomes:

```text
Result
```

```text
HP/*=ID
```

means materializing the active state at `HP` has unsigned ID `ID`.

A multi-step hashpath composes these transitions left to right.

Each transition must be independently verifiable from its witnesses. Verifying
step `N` must not require executing steps `0..N-1`; the prior state is
reconstructed from the hashpath and loaded witnesses.

## Storage And Portability

Reusable result patches should be stored without caller-specific ancestry.

Loading a hashpath reconstructs live extension semantics:

```text
load(HP/Req>VB+VR=Patch)
  -> { PatchKeys..., ...: load(HP), priv: { hashpath: HP/... } }
```

A portable computation package is one message containing a hashpath plus the
witness messages needed to resolve its IDs. Posting that package to another
node gives the node enough data to reconstruct the state and continue
computation.

## Commitments

A commitment attests public membership claims and public values.

Computed-result commitments attest the hashpath context.

Commitments never attest `priv`.

Signed ancestors remain valid under extension because extension does not mutate
them. A signed request used to construct a new extension layer does not
automatically sign that new layer.

## Core Rule

No hidden inputs.

Every value observed by execution must be present in `VarBase` or `VarReq`.

Every value in `VarBase`, `VarReq`, or `Result` must be traceable to ordinary
AO-Core memberships.
