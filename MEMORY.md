# AO-Core Upgrade Memory

Worktree: `/Users/sam/.codex/worktrees/ao-core-vary-clean/hyperbeam`
Branch: `feat/ao-core-vary-clean`

## Why This Exists

This file is the long-form memory for the clean AO-Core types, varying, overlay,
and message-extension work. It should be reread frequently during the overnight
pass, especially after context compaction, before each implementation batch, and
before every commit. The aim is not to preserve the prototype branches. The aim
is to preserve the protocol idea and express it in the smallest, clearest, most
HyperBEAM-native implementation that can pass the full test suite on top of
current `edge`.

The prior attempts became too large because they mixed several layers at once:
type extraction, varying, result cache rewriting, process cache replacement,
message extension, singleton parsing changes, signed-message preservation,
hashpath redesign, and broad device cleanup. Some of those are real protocol
changes, but they are not all one patch. The clean build must separate the core
engine discipline from later structural message extension. The first pass should
make the runtime understand exactly what a device consumes, canonicalize only
that part of the base and request, execute against that canonical pair, and
cache that execution so equivalent inputs share compute. Overlay semantics are
needed in the first pass because the output of a varied execution must still be
applied to the caller's concrete, non-varied message. Full structural extension
can come later.

The central idea is simple: device specs are not documentation decorations. They
are protocol surface. They tell AO-Core which parts of a message a device
function consumes, which parts must be loaded and coerced, and which parts can
remain lazy. Once that is true, device code becomes simpler. Devices should not
all defend against every value being a link, every field being an unexpected
binary, or every nested message needing manual loading. The resolver, not each
device, owns the device-boundary discipline.

## Morning Review Reset

After the first overnight pass reached a `COMPLETE:` commit with
`HB_PARANOID=cache_read,cache_write rebar3 eunit-all` green, review found that
the green result was not merge-clean. The branch had made real protocol
progress, but some changes satisfied paranoid mode by weakening it or routing
around it. This file must preserve that lesson after compaction.

The immediate objective is no longer "preserve the green paranoid checkpoint."
The objective is:

```text
rebar3 eunit-all passes with clean model-aligned code and no reward hacks.
Then HB_PARANOID=cache_read,cache_write rebar3 eunit-all becomes the detector
for remaining cache/commitment bugs.
```

Known suspect changes to unwind:

- `hb_message:paranoid_verify/3` skipped all materialized child verification
  for `cache_read` and `cache_write`.
- The same paranoid path deferred a whole commitment if any committed key was a
  link. It is normal not to recursively verify an unloaded link target, but it
  is not normal to skip verification of the current message's committed link
  surface.
- Missing-secret HMAC commitments silently passed generic cache paranoia. This
  must become either real verification using the intended secret source or an
  explicit unverifiable/failing state.
- Private `no-store` was added in multiple devices under paranoid pressure.
  Some outputs may truly be private, nondeterministic, or time-local, but
  `no-store` must not be used merely to avoid cache writes.
- `dev_bundler:invalid_item_test_parallel/0` was narrowed from an HTTP/server
  integration path plus direct device assertion to only a direct device call.
- `hb_http` accepted unforced signed inbound messages even when verification
  failed, merely avoiding the signed cache write. The intended contract is to
  reject invalid signed inbound messages with a client error.

The path from here is slow and model-first:

1. Restore core paranoia semantics and HTTP signed-input behavior.
2. Remove unjustified private `no-store` markings.
3. Restore reduced tests and let failures expose real model gaps.
4. Get the ordinary suite green.
5. Re-run paranoid cache mode and fix the exposed root causes.

## Current Morning Directive

The current unattended pass must move slowly through the system in this order:

1. Core model alignment: `hb_message`, `hb_ao`, `hb_http`, cache/control,
   commitment boundaries, extension/overlay, and singleton/signed ancestry.
2. `~process@1.0`, scheduler, and process-oriented vectors.
3. Wider preloaded devices.

This order matters. A wider device failure must not be solved by weakening a
core invariant. If the ordinary suite fails, first ask whether the failure
exposes an honest mismatch in the model or the device contract. If paranoid
mode fails, treat it as cache poisoning or commitment-honesty evidence until
proved otherwise.

The first gate is now:

```text
rebar3 eunit-all
```

Only after that gate is green with clean semantics should the second gate be
run:

```text
HB_PARANOID=cache_read,cache_write rebar3 eunit-all
```

Do not claim completion unless both pass at the latest tip. If an isolated
rerun proves a timing-sensitive assertion is noisy, record the evidence in
`STATUS.md`, but do not treat a failed full gate as green.

There are three fresh model clarifications:

- `{as, Device, Msg}` should disappear from the protocol-facing model. Casting
  is ordinary overlay/extension with `#{ <<"device">> => Device }` and any
  additional keys. For path syntax such as `key~device@1.0`, compose the device
  first, then resolve path `key`.
- `priv` must be carried forward when one message extends another message. It
  is local execution state, not public message content. It must never enter
  public IDs, signatures, cache commitments, or serialized public surfaces.
  Binaries, lists, and scalars are not messages and do not carry message
  `priv`.
- Routine normalization of commitments on loaded messages is suspect. Do not
  attach synthetic unsigned ID commitments to mutable Erlang maps as ordinary
  state. Prefer offloading to the cache and keeping loaded messages shallow.

`no-store` is not a reward for making cache paranoia angry. It is valid only
for genuinely private, nondeterministic, time-local, or node-local-policy
results that are not represented in the AO cache key. If a result is stable for
a bounded period, the right direction is cache expiry/max-age semantics, not
private no-store. Remove or narrow any no-store that exists only to avoid a
cache write.

Signed inbound HTTP messages have a verify-or-reject contract. If an inbound
wire message presents a signed commitment, the node must verify it or reject it
with a client error. Merely accepting it while skipping `store-all-signed` is
not enough.

Paranoid cache verification is allowed to leave unloaded link targets alone:
links are cache resource boundaries unless an explicit recursive/bundled
verification mode asks for the target. But materialized children and the
current message's committed surface must not be skipped. Secret-key HMAC
commitments must not silently pass generic cache verification without the
secret.

## AO-Core As A Device Calculus

Every AO-Core message has device semantics. A message may explicitly name its
device with a `device` key. If it does not, the device is `message@1.0`. This
means device semantics are never optional. A message is not merely a map with a
few special fields; it is a thing whose keys are interpreted by a device. The
default device gives ordinary message-member behavior, but it is still a
device.

Resolution is the repeated application of requests to bases:

```text
AO(Base, Request) -> Result
```

When the request has a `path`, that path selects a key on the base's device. The
resolver locates the actual Erlang function that implements that key, builds the
arguments, calls the function, and then handles subresolution, hashpath update,
result caching, worker spawning, and recursive path execution. When a request
does not have a `path`, it is not an invocation of an empty key. It is
composition: the request message extends the base message.

Results are messages or literals. A result is not a separate class from a
message, and this matters. A result can become the next base in a sequence. A
cached computation output is a message in the same model as other messages. The
language "result edge" was misleading in earlier work because it suggested a
separate result/member category. In AO-Core, messages beget messages.

Hashpaths are the cryptographic trace of this calculus. They are not just cache
keys. They are terse expressions of how a message was reached, and they must be
challengeable. Existing hashpaths will need to evolve after message extension
lands, but phase 1 should avoid redesigning them except where varying forces the
canonical inputs to be used.

The old tuple cast syntax `{as, Device, Msg}` should be removed from the
protocol-facing model. It is an out-of-band second form of message composition.
The clean model is that changing a message's device is just extension/overlay:

```erlang
Msg#{ <<"device">> => Device }
```

or, once structural extension is fully in place:

```erlang
#{ <<"device">> => Device, <<"...">> => Msg }
```

Path syntax such as `key~device@1.0` should be interpreted in the same spirit:
compose the base with `device = device@1.0`, then resolve request path `key`.
There should not be a special `{as, ...}` execution universe with bespoke
loading, subresolution, or cache semantics. Existing code still contains many
`{as, <<"message@1.0">>, Msg}` uses; they are migration targets, not precedent.

## Message Extension Semantics

The message extension key is `...`. It means the current message inherits keys
from another message. Parent keys win over inherited keys. These examples are
canonical:

```text
GET { a = 2, ... = { a = 1 } }/a => 2
GET { b = 2, ... = { a = 1 } }/a => 1
```

This means extension is not a shallow merge that destroys history. It is a
message-level inheritance chain. The parent message can override inherited keys,
but the ancestor remains available as the extension. This matters for signed
messages. A user may send a signed message; the node may need to add path
segments, scheduling metadata, or local execution parameters. Those additions
must not obliterate the user's signed core. The new message can extend the
signed message, and later code can strip back to the signed subset when needed.

`...+link` is not structured-message semantics. It may appear at the TABM or
cache boundary where linkified data is represented, but `structured@1.0`
semantics should speak in terms of `...`. The core model is extension through
`...`; representation-specific link tags should not leak into device logic.

Private state has a special local rule. When one message extends another
message, the `priv` element should be brought forward into the new local base.
This keeps runtime state such as hashpath, local cache hints, wallet state, and
device-private execution state available as execution composes messages. That
does not make `priv` public: `hb_private:reset/1`, cache writes, codecs, and
public ID generation must continue to exclude it. If the current result is a
binary, list, or scalar rather than a message, there is no message `priv` to
carry.

Ordinary devices should not need to reason about extension chains unless they
ask for them. The type/vary boundary should flatten inherited keys into the
concrete view requested by the device spec. If a function spec explicitly
mentions `...`, that is a signal that the function wants extension structure.
Otherwise it receives the resolved values it declared.

Nested extension matters, but it is expensive to get wrong and should not be
dragged into phase 1 unless unavoidable. The intended later structural overlay
is deep. Returning:

```erlang
#{ <<"a">> => #{ <<"b">> => 2 } }
```

over:

```erlang
#{ <<"a">> => #{ <<"a">> => 1 } }
```

should represent:

```erlang
#{
    <<"a">> => #{
        <<"b">> => 2,
        <<"...">> => #{ <<"a">> => 1 }
    },
    <<"...">> => #{ <<"a">> => #{ <<"a">> => 1 } }
}
```

If this deep structural behavior must be faced before the morning review, it
must be isolated in clearly labeled commits so it can be reviewed atomically.
The preferred phase-1 approach is to use existing `set` semantics for overlay,
then convert that to structural extension after the extension machinery is
complete.

## Types And Varying

The resolver must vary at the device boundary. The flow is:

```text
Base/Req -> resolve actual device function -> VariedBase/VariedReq -> execute
```

The actual function is important. A device key may be implemented by an exported
function, a handler, a default handler, or a default device. The spec must be
looked up for the function that will actually be called, not merely for the
path name on the apparent device. If a default handler takes the key as its
first argument, the type extractor must account for that `AddKey` argument and
interpret base and request at the correct positions.

The type syntax is intentionally pressure-forming. It should encourage device
authors to be precise, because precision increases cache deduplication and
allows the resolver to load only what the function will consume.

`_` means empty projection. For a base argument, this means vary on no user keys
aside from the implicit `device` key. For a request argument, it means vary on
no user keys aside from the implicit `path` key. `_` should not mean opaque
pass-through. Treating `_` as empty projection is important because it rewards
devices that consume no message keys with maximal cache collapse.

`any()` means opaque pass-through. The input is left unchanged. It is not
projected, not force-loaded, and not collapsed. This is an escape hatch for
functions that truly accept arbitrary input and do not want AO-Core to inspect
or normalize the shape.

`#{ Key := Type }` means the key is required. The resolver must find it through
message semantics, load/materialize it if needed, and coerce it according to
`Type`. If the key is absent, the function's input contract is not satisfied.

`#{ Key => Type }` means the key is optional. If present, it is found through
message semantics, loaded/materialized, and coerced. If absent, it is omitted
from the varied message.

`#{ A := _, B => _ }` varies only on required `A`, optional `B`, and the
implicit device or path key. Other user keys do not participate in the varied
execution input. This is the common precise shape.

`#{ A := _, B => _, _ => _ }` carries all remaining visible keys forward but
does not force-load the unmatched keys. This is needed for devices like process
compute. A process device may know it needs `process-id`, `at-slot`, and some
device-selection keys loaded, but it may also need to pass the rest of the
state forward so downstream devices can resolve keys like `stack-prefix` later.
The unmatched keys remain accessible/lazy.

`#{ A := _, B => _, _ := _ }` carries all remaining visible keys forward and
force-loads/materializes them. This is the explicit "load all keys" form. It
should be rare and intentional.

Return specs use explicit extension intent:

```erlang
#{ '...' => base }
#{ '...' => request }
```

This is not a wildcard. `_ => base` would confuse key wildcard semantics with
overlay semantics. `...` says that the returned message should be interpreted
as an overlay over the chosen input side.

A future type form may be useful:

```erlang
signed(Schema)
```

That should first strip the input to the first signed subset available through
`hb_message:with_only_signed/2`, then apply `Schema`. This is not necessary for
the first implementation unless a concrete call site proves it is needed, but
it is the right direction for scheduler, bundle upload, and similar flows that
build on a message while preserving a signed core.

## Cache And Overlay

The cache semantics must be exact:

```text
Base/Req
  -> VariedBase/VariedReq
  -> ExecResult cached at VariedBase/VariedReq
  -> overlay ExecResult onto this caller's non-varied Base or Req
```

The cache stores the result of the varied execution. It does not store the
caller-specific final message after applying the overlay to the original base.
This distinction is what makes compute deduplication work. Many different
caller inputs can vary to the same `VariedBase/VariedReq`. They should share
one execution. After a cache hit or fresh compute, the shared result is applied
to the current caller's non-varied base to produce that caller's concrete final
message.

This is the reason some old-base/new-base plumbing is necessary after all. The
resolver needs to remember the non-varied input for overlay application while
also using the varied pair for cache lookup, persistent grouping, execution,
hashpath generation, and result cache write. The old pair should not sprawl
through the code as a second universe. It exists for the narrow purpose of
final overlay onto the caller's original message. Everything that represents
the execution itself should use the varied pair.

Cache lookup should preserve the existing direct-member optimization where it
is semantically valid. If the base is an ID and the request is a direct
`message@1.0` key read, HyperBEAM can read `/ID/key` without loading and varying
the whole base. That is a member read, not a computation whose input contract
must be projected. But if the key is device-computed, the engine must resolve
the function, vary the inputs, and then use the varied pair for cache.

Avoid broad `hb_cache` rewrites. Current `edge` already has cache lookup and
store APIs parameterized by base and request. The right first move is to feed
those APIs the canonical varied pair. Only add cache code if a failing test
shows an exact missing operation. Do not reintroduce "result edge" terminology.
Do not build side indexes to make a prototype easier.

Routine commitment normalization should be removed from loaded-message flow.
`hb_message:normalize_commitments/2` and similar patterns that annotate normal
Erlang maps with unsigned IDs make cache poisoning more likely: after a message
is annotated, ordinary map updates can mutate public keys while stale
commitment/ID metadata remains attached. The better model is that unsigned IDs
belong to cache addressing and link structure. Loaded messages should stay as
plain messages plus real commitments. Offload deep data to the cache and keep
links shallow rather than deepening loaded messages with synthetic unsigned
commitments.

## Signed Subsets

`with_only_committed/2` filters a message down to keys covered by commitments,
but in the extension paradigm the operational need is sharper: find the signed
ancestor. A node may receive a signed inbound message, then build new messages
on top of it with local additions. Schedulers, bundle uploaders, relays, and
other devices need to strip back to the signed subset if it exists, without
losing the ability to build extended messages during normal operation.

The new helper should be:

```erlang
hb_message:with_only_signed(Msg, Opts)
```

It should inspect `Msg`. If `Msg` has a commitment containing a `signature`, it
should return only the committed subset of `Msg`, using the same cheap,
non-verifying spirit as `with_only_committed/2`. If `Msg` has no signed
commitment, it should inspect `Msg["..."]`. It should continue popping `...`
until it finds the first signed ancestor. If no signed ancestor exists, it
should return the original message unchanged.

This function should not verify the signature. Verification is expensive and
belongs at call sites that need authenticity. The helper's job is to recover
the signed subset shape. It is a structural filter, not an authentication
decision.

It should ignore unsigned commitments for the purpose of deciding that it found
a signed ancestor. A message with only unsigned ID commitments is not the signed
subset being requested. It may still be useful elsewhere, but
`with_only_signed/2` is about finding the first commitment with a signature
inside.

Do not migrate every `with_only_committed/2` call blindly. Some call sites may
want committed keys regardless of signature. Others need signed-subset
semantics. Use evidence from scheduling, bundling, location records, and process
flows to decide. Broad mechanical replacement is exactly the kind of churn this
branch must avoid.

HMAC commitments are not signed ancestry for `with_only_signed/2`. Local HMACs,
including secret-key HMACs, may protect a local channel or cache relationship,
but they should not cause a message to be treated as the user's signed subset.

## Singleton Parsing

`hb_singleton:from/2` turns an inbound TABM/HTTP singleton into an ordered list
of AO-Core messages. In the old model, it can effectively rewrite path parts
and scoped query/header values into per-step request messages. In the extension
model, signed inbound messages from users should be preserved as ancestors.

The desired shape is that path-derived request steps extend the inbound signed
message, or the relevant request ancestor, instead of destructively rewriting
it. The parser should still produce an executable list compatible with
`resolve_many`, but each path segment's request should be a message that can
reach the original signed input via `...` when that matters.

This is important because signing and path execution pull in opposite
directions. HTTP request parsing naturally wants to modify the request for each
segment. Signature preservation wants the user's signed message to remain
available unchanged. Extension gives both: the executable request can add or
override `path`, while `...` points back to the signed original. Then
`with_only_signed/2` can recover the signed core for scheduling, uploads, or
other operations.

This change should be kept surgical. Do not redesign singleton syntax. Do not
invent a second parser. Add extension where it preserves signed inbound
messages and keeps the existing execution list shape coherent.

## Hashpaths And HTTP Commitments

A hashpath is both a terse expression of a message and an atomically
challengeable attestation to every result necessary to produce it. After
message extension, hashpaths should be able to express both execution and
extension.

Draft equivalences:

```text
ID1/ID2=ID3
```

Given `ID1`, applying `ID2` yields the same message as extending `ID1` with
`ID3`.

```text
ID1/ID2.ID3
```

Given `ID1`, applying `ID2` yields `ID3`.

The hashpath should also end with the fully varied result ID as a bookend:

```text
GET /BaseID/ReqID/Req2

Hashpath:
VariedBaseID/VariedReq1=Res1/VariedReq2=Res2/UnsignedIDOfRes2OnRes1OnVariedBaseID
```

The bookend should not be required for verification, but it ties the HTTP
response to the unsigned ID of the fully varied terminating message when the
result itself does not have a direct commitment.

HTTP responses should commit to two things: the full set of keys and values at
the terminating message known by the node, and the hashpath of the execution.
The unsigned ID commits to the known message content. The HTTP signature binds
the returned representation to the execution trace. This gives the client both
"what message did you return?" and "how do you claim it was reached?"

Do not force this hashpath redesign into phase 1 unless it becomes necessary
for correctness. Phase 1 can use existing hashpath mechanics over the varied
pair. Extension-aware hashpaths are the later layer.

Inbound signed HTTP messages have a simple acceptance contract: if a request
presents commitments as signed input, the node verifies them or rejects the
request with a client error. It is not enough to accept the message but decline
to store it. `store-all-signed` is only allowed after verification succeeds.

## Device Specs And Device Cleanup

The device spec corpus is valuable. The earlier branches contained many
detailed specs; the clean branch should recover their intent while pruning
their excess. Specs should be as specific as possible. Do not use
`#{ _ => _ }` because a test failed and a broad pass-through was convenient.
Every broad wildcard should have a reason rooted in the device's actual
behavior.

Device cleanup is allowed only where the new boundary contract makes it
obviously simpler. If a function spec now guarantees that a value is loaded and
coerced, the device can pattern match on that value. If a device no longer has
to handle linkified forms for declared inputs, remove that local ceremony. But
do not refactor neighboring code for taste. Do not change AO process behavior
without direct need. Do not revive removed modules.

Good candidate specs are exported device key functions, handlers, and default
handlers. Internal helpers do not need protocol specs unless they already have
ordinary type specs for clarity. The resolver only needs the call boundary.

Be especially careful with process devices. A process state must load and vary
keys like `process-id`, `at-slot`, and device selection keys, while allowing
downstream execution devices to access keys that the process device itself does
not know about. This is exactly why `#{ Known := _, _ => _ }` exists: carry the
rest forward lazily without force-loading everything.

There is also a positive cleanup opportunity here, and it is important. Once
the resolver varies inputs correctly, many devices should become radically
simpler. A correctly varied message at the device boundary is essentially an
ordinary Erlang map containing the keys the function asked for. The device does
not need to behave as if every declared value may still be a link. It does not
need to manually load every field it declared. It does not need elaborate
fallback plumbing for the shape that the spec now guarantees. The resolver owns
that boundary.

This means device internals should start looking more like normal Erlang:
function heads can pattern match on required keys, direct map access is fine
where the spec has guaranteed a present key, and link-handling ceremony should
be removed when it is only compensating for the old unvaried world. This is not
a license for broad refactoring. It is a license to delete obsolete defensive
mechanics after the spec makes them false. Radical simplification is valuable
when it reduces the surface area that each device author must reason about.
Keep the diff disciplined: simplify the functions whose specs now justify it,
run focused tests, and do not change unrelated behavior.

## Archive Loading And Type Extraction

Do not add an archive schema side index as a first move. Prior work used
`persistent_term` around device archive object code and schema lookup. That was
too heavy and too easy to justify as a workaround. If normal loaded modules
provide abstract code, extract specs from them. If generated archive-loaded
modules do not expose abstract code through normal means, let varying degrade
to no-op for that device unless a concrete test proves that archive device
varying is required in phase 1.

If archive type extraction becomes necessary, define a small loader contract.
For example, retaining object code for modules loaded from an archive may be
reasonable if it is clearly part of the device loading contract and not a
side-index kludge. But do not design that in advance. First build the clean
path for local/preloaded modules and let evidence drive the archive support.

Likewise, do not use store-backed type extraction caches unless needed.
Correctness does not require caching extracted schemas. Performance may later
justify a small memoization keyed by module and BEAM identity, but it should be
simple, local, and documented. The default implementation should privilege
clarity.

## Implementation Shape

The clean resolver edit should be small. Existing `hb_ao` stage flow already
normalizes, checks cache, validates, groups persistent executions, looks up the
device, executes, handles hooks/subresolution, updates hashpath, stores cache,
notifies waiters, and maybe spawns workers. The insertion is not a rewrite of
that machine. It is a reordering around function lookup and cache.

The resolver needs to normalize base/request as today. It should preserve the
cheap direct lookup for proven direct member reads. For computed paths it should
load enough of the base to resolve the device function, then call the type/vary
module for that function. The varied pair becomes the execution pair. Cache
lookup should happen on that varied pair. Persistent grouping should use that
varied pair so equivalent executions collapse. The device call should receive
the varied pair so device code can rely on its specs. Hashpath and cache write
should describe the varied execution. Overlay should then apply the shared
execution result to the caller's original base where the return spec says so.

`hb_ao:raw/*` must stay raw. It bypasses normalization, cache, hashpath,
workers, varying, and overlay machinery. Callers use raw when they explicitly
want direct function application.

Avoid naming drift. Do not call message members "edges" in new code. If an old
helper or branch used "edge" to mean a result/member relationship, do not carry
that forward. HyperBEAM normally speaks in messages, keys, links, devices,
hashpaths, commitments, and paths.

## Validation And Commit Discipline

The branch must be reviewed by its shape as much as by its tests. Small,
coherent commits matter. A good commit might be "add AO type schema extraction",
"vary resolver inputs before cache/execution", "add signed-subset helper", or
"spec process compute inputs precisely." A bad commit mixes singleton parsing,
cache rewrites, message extension, and forty device edits.

Run progressively stronger validation:

```text
rebar3 compile
rebar3 eunit --module=<focused module>
rebar3 eunit
rebar3 eunit-all
```

Use focused tests while developing, but do not claim completion without the full
suite requested by Sam. Do not relax assertions. Do not replace real data with
mocks to make failures disappear. If a test fails because the new semantics
expose a real mismatch, fix the mismatch or write a decision note if the fix
requires protocol judgment.

The commander's intent for the overnight run is stricter than ordinary green
tests. The final state must pass:

```text
HB_PARANOID=cache_read,cache_write rebar3 eunit-all
```

That means both the core suite and the preloaded device suite must pass while
cache reads and writes are under paranoid checking. This is the absolute marker
of success. A branch that only passes non-paranoid eunit is not complete. A
branch that passes because assertions were relaxed, specs were broadened to
`#{ _ => _ }` without real cause, or cache correctness checks were routed around
is not complete. The clean branch should be small, comprehensible, and correct
under paranoid cache validation.

Keep `STATUS.md` updated during unattended work. Chat is for blocking questions
only. Decisions that would normally require discussion but are reversible should
go in `decisions/<name>.md` with options, reasoning, and the selected path.

Before that final paranoid gate, the ordinary suite must pass with clean
semantics and no known reward hacks:

```text
rebar3 eunit-all
```

Only then should paranoid mode be reintroduced. `HB_PARANOID` exists to expose
bugs before production, especially cache poisoning from devices mutating signed
public keys. It must not be made green by skipping commitments, disabling
features, reducing integration coverage, marking cacheable results private
`no-store`, or broadly varying/loading everything.

It is normal not to recursively verify unloaded links. Links are cache resource
boundaries. It is not normal to skip verification of the current message's
committed surface merely because one committed key is represented as a link.
Verify the link representation, load if the committed surface requires it, or
fail.

## Things To Avoid

Do not build from the dirty detached prototype worktree. Start from
`hyperbeam-main/edge` on a fresh branch. Use previous branches as references,
not ancestry.

Do not revive `dev_process_cache`. It is gone from `edge`.

Do not add broad `dev_green_zone` or `dev_snp` changes.

Do not add large RFC-style device spec docs as part of the engine patch.

Do not add rate-limit hacks or paper over failures with HTTP option changes
unless a specific test proves that exact operational fix is the root cause.

Do not use `persistent_term` as a schema side index in `hb_device_archive`
without a written decision and evidence that no smaller loader contract works.

Do not make `hb_cache` the center of the patch. Feed it the right varied inputs
first.

Do not conflate `any()` with `_`. `_` is the empty projection shorthand.
`any()` is opaque pass-through.

Do not conflate wildcard `_` with return overlay. Return overlay is explicit
`...`.

Do not implement full deep structural extension in phase 1 if existing `set`
semantics can carry the first version. If deep structural extension is
unavoidable, isolate it.

Do not claim tests pass without running them. Do not claim a change is present
without checking the diff.
