# `name@1.0` — name resolution via configured resolvers

- **Device name:** `name@1.0`
- **Depends-on:** `message@1.0` (base identity device for excluded/unhandled keys), and — as *resolver types*, not hard build dependencies — `local-name@1.0` and `b32-name@1.0` (the two stock map-shaped resolver devices). A deployment MAY configure any resolver device in their place. The `message@1.0` spec is provided to reimplementers; the resolver specs are provided for context but a conformant `name@1.0` does not embed their behaviour.
- **Status:** Draft

## 1. Overview

`name@1.0` maps a **name** (an arbitrary, human- or machine-meaningful binary
key) to a **value** (a message, an identifier, or a link to a stored message),
by consulting an ordered list of configured **resolvers**. A resolver is itself
addressable AO-Core logic that, given a name, either yields a value or declines.
The device tries each resolver **in configured order** and returns the value of
the **first** resolver that succeeds; if none succeeds the lookup is
**not-found**.

The device has two facets:

1. **A resolved-key facet** (the primary contract): reading any key `X` off a
   message bound to `name@1.0` means "resolve the name `X`". By default the
   resolved value is then **loaded** (dereferenced) if it is an identifier or a
   link; this can be disabled per request.
2. **A request-hook facet**: an `on/request` hook that maps the **subdomain** of
   an incoming request's `Host` header to a name, resolves it, and **prepends**
   the resolved message onto the request's execution path — so that
   `https://<name>.<node-host>/<rest>` is served as `<resolved-message>/<rest>`.

It exists to give a node a configurable, layered namespace: local operator-set
names, on-chain naming systems (e.g. ArNS), base32 transaction-ID subdomains,
and JSON name snapshots can all be stacked behind one uniform lookup surface.

## 2. Concepts & terminology

- **Name:** a binary key being resolved. For the resolved-key facet it is the
  key requested on the bound message (e.g. `GET /~name@1.0/hello` resolves the
  name `hello`). For the hook facet it is the **subdomain** extracted from the
  request `Host` header. Names are used verbatim — the device does **not**
  lower-case, hyphen-fold, or otherwise canonicalise the name before handing it
  to a resolver (case rules below).
- **Resolver:** one element of the configured resolver list. Two shapes are
  defined (§3, §4): a **path-prefix resolver** (a binary) and a **resolver
  message** (a map). Each is asked for a single name and returns `{ok, Value}`
  to claim it or anything else (including an error or a raised exception) to
  decline.
- **Resolved value:** whatever a resolver returns under `{ok, _}`. It MAY be:
  a fully-formed message (map); an **identifier** (a content/commitment ID); a
  **link** (a lazy reference to a stored message); or any other binary/scalar.
- **Identifier:** a binary that is a native-or-encoded AO-Core ID — concretely,
  a binary whose byte length is 32, 42, or 43 (a raw 32-byte ID, or its
  43-char / legacy-42-char base64url text form). Identifiers, when **loaded**,
  are read from the node's content-addressed store.
- **Link:** a lazy, not-yet-materialised reference to a stored message. When
  **loaded**, a link is forced to its target message. (The internal shape of a
  link is out of scope; behaviourally it is a value the resolver layer hands
  back that, when loaded, yields the referenced message.)
- **Loading (dereferencing):** turning a resolved identifier/link into the
  message it points at, by reading from the node's message store. Controlled by
  the request key `load` (§4).
- **`name-resolvers` (node option):** the ordered list of resolvers, addressed
  on the node options as the binary, lowercase, hyphenated key `name-resolvers`.
  (An implementation whose option layer canonicalises case/underscore MUST
  resolve this to the same option; the underlying key name is `name-resolvers`.)
  Default when unset: the **empty list** `[]` (no resolvers → every lookup is
  not-found).
- **`node-host` / `host` (node options):** the node's own externally-reachable
  host, used by the hook facet to strip the node's domain off an incoming `Host`
  header and isolate the subdomain. `node-host` is consulted first; `host` is
  the fallback; if neither is set the hook uses a no-host fallback parser
  (§4.`request`).

## 3. Device interface

- **Dispatch shape:** **default-handler.** Every key not explicitly excluded is
  handled by the default resolver behaviour (§4.`<name>`); i.e. the device
  answers an **arbitrary** key by treating that key as a name to resolve.
- **Excluded keys:** the device MUST NOT capture the keys `keys` and `set`.
  These fall through to the base identity device (`message@1.0`) so that the
  bound message can still be inspected (`keys`) and mutated (`set` / its
  path-setting variant) — operations needed to manipulate the base message
  itself rather than to resolve a name. No other keys are excluded; in
  particular `id`, `commit`, `verify`, `path`, etc. are **not** in this device's
  exclude list (they are not part of its contract and an implementation MAY rely
  on the substrate's normal handling of them, but the only keys this device
  guarantees to forward are `keys` and `set`).
- **Resolved-key message shape (Base):** the device ignores the content of the
  base message for resolution purposes — the name comes from the requested key,
  not from base data. (The base message is still the message the device is bound
  to; binding is what routes a key to this device.)
- **Resolved-key request shape (Req):** one optional key:
  - `load` — boolean (or a binary coercible to a boolean, e.g. `true`/`false`).
    **Default `true`.** When `true`, a resolved identifier/link is loaded to its
    target message before being returned; when `false`, the resolved value is
    returned **as-is** (the raw identifier/link/message the resolver produced).
- **Resolver-list shape (`name-resolvers` option):** an ordered list. Each
  element is one of:
  - **Path-prefix resolver — a binary** `P`. Resolving name `K` against it means
    resolving the AO-Core path `P/K` (the name is appended as a single path
    segment after `P`, separated by `/`). `P` MAY itself be a multi-segment /
    device-bearing path (e.g. `<id>~json@1.0/deserialize&target=data`, or
    `<some-prefix>`). The element's success/decline is exactly the
    success/decline of resolving `P/K`.
  - **Resolver message — a map** `M`. Resolving name `K` against it means
    resolving key `K` **on the message `M`** (i.e. asking `M` — with whatever
    `device` it carries — to answer the key `K`). Typical `M` is a device
    binding such as `#{ <<"device">> => <<"b32-name@1.0">> }` or
    `#{ <<"device">> => <<"local-name@1.0">> }` or
    `#{ <<"device">> => <<"arweave@2.9">> }`, but `M` MAY be any message whose
    device answers the name as a key (e.g. a literal in-line map
    `#{ <<"hello">> => <<"world">> }` answers the name `hello` with `world`).
- **An empty or unset resolver list** disables resolution: every lookup is
  not-found, with no error.

## 4. Resolved keys (normative)

### `<name>` (the default handler) — resolve a name

- **Reads:** the requested key (the **name**); the node option `name-resolvers`
  (the ordered resolver list, default `[]`); the request key `load` (default
  `true`); and, on load, the node's content-addressed message store.
- **Behaviour:**
  1. Read the ordered resolver list from `name-resolvers` (default `[]`).
  2. **First-match scan.** Walk the list in order. For each resolver, attempt to
     resolve the name against it (per the per-shape rules in §3):
     - a **binary** resolver `P` → resolve path `P/<name>`;
     - a **map** resolver `M` → resolve key `<name>` on `M`.
     The attempt **succeeds** iff it yields `{ok, Value}`. Any other outcome —
     an error result, a not-found, or a **raised exception/crash** during the
     attempt — is treated as a **decline**: the device MUST catch it and move on
     to the next resolver. The device MUST NOT let a single resolver's failure
     abort the scan.
  3. On the **first** success, take that resolver's `Value` and stop scanning
     (later resolvers are not consulted — first-match wins).
  4. If **no** resolver succeeds (including the empty-list case), the result is
     **`not_found`** (see §8). The device MUST NOT fabricate a value or fall
     through to any other device for the name.
  5. **Loading.** On a success, read `load` from the request (default `true`,
     coercing a binary `true`/`false`):
     - `load = false` → return `{ok, Value}` **verbatim** (the raw resolver
       output: identifier, link, or message — unmodified, unloaded).
     - `load = true` (default) → **dereference**:
       - if `Value` is an **identifier** (a 32/42/43-byte binary, §2), read the
         message it identifies from the store and **return that read's result
         directly** — the **stored message** on a hit; and on a miss the read's
         own outcome (`not_found` / `{error, _}`) is **propagated**, NOT swallowed
         into a fallback that returns the raw ID (see §8);
       - else if `Value` is a **link**, force the link and return its target
         message;
       - else (a map or any other value) return `{ok, Value}` unchanged.
- **Returns:** `{ok, Resolved}` — the resolver's value, loaded per `load`; or
  the atom `not_found` when no resolver matches. The shape of `Resolved` is
  whatever the matching resolver produced (after optional loading): commonly a
  message, but MAY be a bare identifier/binary when `load = false`.
- **Side effects:** **none of its own** — no cache write, no store write, no
  commitment, no outbound request originate **in this device**. (A path-prefix
  or map resolver, or the load step, MAY itself read from the store or the
  network; those effects belong to the resolver / store layer, not to
  `name@1.0`. The load step is a **read**, never a write.)

### `request` — `on/request` host-to-path hook

This key implements an `on/request`-compatible hook. It is invoked by the node's
request pipeline (when `name@1.0` is installed under `on/request`) with a hook
request that carries the incoming `request` (including its `host`) and the
current execution `body` (the list of messages to be resolved). It rewrites the
`body` so a name-bearing subdomain is served as the resolved message.

- **Reads:** from the **hook request message** (the *request* argument the hook
  is invoked with — NOT the base/bound message): the nested `request` map and,
  inside it, the `host` key (the incoming `Host` header, a binary); and the
  top-level `body` (the current ordered list of request messages, possibly
  empty). From the node options: `node-host` (preferred) else `host` (fallback)
  else neither — used to identify the node's own domain. Plus everything the
  `<name>` resolution reads (the resolver list, the store).
- **Behaviour:**
  1. **Extract the name from the host.** Compute the **subdomain** of `host`
     relative to the node's own host:
     - Let `NodeHost` be `node-host` if set, else `host` (the node option), else
       the no-host fallback.
     - If `NodeHost` is known: parse the host portion out of `NodeHost` (it MAY
       be a full URL such as `http://localhost`; take its host component) and
       split `host` on the literal `.<NodeHost-host>` suffix. If `host` is
       exactly `<the node host>` (no subdomain) → **skip** (no name). Otherwise
       the **subdomain** (everything before that suffix) is the name. If `host`
       does not end in the node host suffix, fall back to the no-host rule
       below.
     - **No-host fallback** (no node host configured, or parse failed, or the
       request host does not contain the node host): split `host` on `.`. If
       there is only a single label (no `.`) → **skip**. If the host is an **IP
       address** (parses as a literal IPv4/IPv6 address) → **skip** (an IP has
       no meaningful subdomain). Otherwise the **first label** (before the first
       `.`) is the name.
  2. **Skip path.** If step 1 yields a *skip* (no subdomain / bare host / IP
     literal), the hook returns the **unmodified** hook request `{ok, HookReq}`
     — name resolution does not apply; the request proceeds normally.
  3. **Resolve the name.** Resolve the extracted name exactly as the `<name>`
     handler does (same resolver list, same first-match, **with loading at its
     default**, i.e. the resolved message is loaded). Call the result
     `ResolvedMsg`.
  4. **Not-found path.** If resolution is not-found (or otherwise fails to
     produce a message):
     - If the current `body` is **empty** (a request for the root path `/`
       under that subdomain) → return an **error** with HTTP status **404** and
       body `Not Found` (see §8). A bare subdomain that names nothing is a hard
       404.
     - If the current `body` is **non-empty** → return the **unmodified** hook
       request `{ok, HookReq}` (the subdomain did not resolve, but there is a
       real path to serve, so defer to normal handling rather than 404).
  5. **Prepend the resolved message.** On a successful resolution, prepend
     `ResolvedMsg` as the **base** of the execution `body`, with this
     **de-duplication** rule (so the named base is not executed twice when the
     path already names it):
     - If `body` is empty → new body is `[ResolvedMsg]`.
     - Else let `OldBase` be the head of `body` and `Rest` the tail. Compare the
       **identity** of `OldBase` and `ResolvedMsg` (their IDs — see "permissive
       identity" in §5):
       - **Same identity** and `OldBase` is a map or list → keep `body`
         unchanged (the resolved base is already present in loaded form; do not
         duplicate it).
       - **Same identity** and `OldBase` is not a map/list (e.g. it is the bare
         ID) → replace the head with the loaded `ResolvedMsg`:
         `[ResolvedMsg | Rest]` (prefer the loaded form over the bare ID).
       - **Different identity:** if `OldBase` is a map carrying its own `path`
         key, keep it and prepend: `[ResolvedMsg, OldBase | Rest]` (both are
         executed, resolved subdomain first, then the path-bearing old base).
         Otherwise drop `OldBase` and use `[ResolvedMsg | Rest]`.
  6. Return `{ok, #{ <<"body">> => NewBody }}` — i.e. a hook result whose `body`
     is the rewritten execution list. The request pipeline then executes that
     body.
- **Returns:**
  - `{ok, HookReq}` (unmodified) on a **skip** (no subdomain) or on a
    **non-root not-found**;
  - `{ok, #{ <<"body">> => NewBody }}` on a **successful** name resolution
    (path rewritten);
  - `{error, #{ <<"status">> => 404, <<"body">> => <<"Not Found">> }}` on a
    **root-path not-found** (subdomain present, names nothing, empty body).
- **Side effects:** the resolution + load it performs are **reads** only; the
  hook itself writes nothing and emits no commitment. Its observable effect is
  the rewritten `body` it returns (the pipeline, not this device, executes it).

## 5. Data formats & encodings

- **Names** are binaries, used **verbatim** by the resolved-key facet: the
  device does not lower-case or fold them before handing them to a resolver.
  (Whether a *given resolver* canonicalises the name is that resolver's
  concern — e.g. a local-name resolver MAY normalise keys internally. `name@1.0`
  imposes no normalisation.) The hook facet's name is the host subdomain,
  likewise passed verbatim to resolution.
- **Identifiers** are AO-Core IDs. For the load decision, the trigger is a
  binary of byte length **32** (raw), **43** (base64url text), or **42** (legacy
  base64url text). IDs on the wire are **base64url**, never hex. A 32-byte raw
  binary is also treated as an identifier (native form).
- **Links** are lazy references; their concrete representation is out of scope.
  The only behaviour pinned here: a resolved link, when `load = true`, is forced
  to its target message; when `load = false`, it is returned unforced.
- **Path-prefix resolver join:** the path resolved for name `K` against binary
  prefix `P` is the byte concatenation `P <> "/" <> K` — a single `/`
  separator, `K` appended as one trailing segment. `P` is **not** otherwise
  transformed.
- **Permissive identity (hook de-duplication):** to compare an "old base" in the
  request body with the freshly resolved message, the hook reduces each to an
  ID as follows: an **identifier** binary → itself; a **link** → the ID it
  references; a wrapped *resolution request* form (a base+device pairing) →
  the identity of its inner message; a **map** message → its **signed** ID. Two
  bases are "the same" iff these IDs are equal. (This lets the hook recognise
  that `<id>.<host>/` and `<id>.<host>/<same-id>/...` name the same base and
  avoid executing it twice.)
- The hook's HTTP error is the structured pair `status => 404` (integer) and
  `body => <<"Not Found">>` (binary). The hook's success result is a message
  with a single `body` key holding the rewritten execution list.

## 6. Ordering, freshness & caching

- **Resolver order is significant and is the configured list order.** The first
  resolver (in list position 0) is tried first; resolution is **first-match**:
  the earliest resolver that returns `{ok, _}` wins and no later resolver is
  consulted. Reordering the `name-resolvers` list can therefore change which
  value a name resolves to. Implementations MUST preserve configured order.
- **No internal result caching.** The device caches nothing of its own; each
  lookup re-reads `name-resolvers` and re-runs the scan. A name's value is only
  as stable as its resolvers (e.g. a local-name resolver or an on-chain resolver
  MAY change what a name maps to over time).
- **Mutability at a constant path.** Because a name's mapping can change (a
  resolver's backing data changes) while the request path `/~name@1.0/<name>`
  stays constant, a node that caches HTTP resolution results MUST be configured
  so these reads are not served stale (result caching disabled / forced-fresh
  at the node layer for the name paths). This is node configuration, not device
  behaviour.
- **Determinism.** Given a fixed resolver list and fixed resolver backing data,
  resolution is deterministic: same name in → same first-match value out.

## 7. Security & authority

- **Resolution requires no signature.** Neither the resolved-key facet nor the
  hook facet requires the request or base message to be committed/signed; any
  caller may resolve any name. (Whether a *resolver* imposes its own authority —
  e.g. a local-name **registration** that only the operator may perform — is
  that resolver's contract, not `name@1.0`'s. `name@1.0` only ever **reads**
  names.)
- **The resolver list is operator-controlled trust.** A name resolves to
  whatever the configured resolvers say; an operator configuring
  `name-resolvers` is asserting trust in those resolvers (and their backing
  data / networks) to map names truthfully. The device performs no validation of
  resolver output beyond the load step.
- **Failure is closed for the root subdomain.** The hook returns a hard **404**
  when a subdomain is present but names nothing and there is no further path —
  it does not fabricate, default, or fall through to an arbitrary base for an
  unresolved named host. Conversely, when a real path is present it **fails
  open** to normal handling (the request is served by the path, not 404'd on the
  name miss).
- **Resolver isolation.** A crashing or erroring resolver MUST NOT abort the
  scan or leak its failure to the caller; it is silently treated as a decline.
  This prevents one misbehaving resolver from breaking the namespace.
- **Excluded keys preserve base-message integrity.** Because `keys` and `set`
  are forwarded to the base device, binding `~name@1.0` onto a message does not
  prevent that message from being inspected or mutated through its normal
  identity-device surface.

## 8. Errors

- **`not_found`** — the resolved-key facet's result when **no** resolver in
  `name-resolvers` claims the name (including the empty/unset-list case). The
  handler **returns the bare atom `not_found`** (not a structured map, not
  `{error, _}`); through `hb_ao:resolve` this is observed inside the standard
  success envelope as **`{ok, not_found}`** (the pipeline wraps a bare device
  return in `{ok, _}`) — it is never `{error, not_found}`. A resolver declining,
  erroring, or crashing contributes to this outcome but is **not** itself
  surfaced.
- **HTTP 404 (`status => 404`, `body => <<"Not Found">>`)** — the hook facet's
  result when a host **subdomain is present**, names nothing (resolution
  not-found), **and** the execution body is empty (root path). This is a
  structured error map, returned as `{error, #{...}}`.
- The hook's **skip** and **non-root not-found** outcomes are **not** errors:
  they return `{ok, <unmodified hook request>}`. A skip carries an internal
  human-readable reason describing why no subdomain was found; that reason is
  **not** part of the externally observable contract (the observable effect is
  simply that the request is unmodified).
- The device defines **no other** error atoms of its own. Any error that arises
  *inside* a resolver or the load step is either swallowed (during the
  first-match scan, becoming a decline) or — for the load step on an already
  chosen value — propagated as that read's result.

## 9. Composition

- **As a path device.** Binding `~name@1.0` onto a path makes that segment a
  name lookup: `GET /~name@1.0/<name>` resolves and (by default) loads the
  named message; `GET /~name@1.0/<name>&load=false` returns the raw mapping.
  Because resolution returns the **target message** (loaded), subsequent path
  segments resolve against that message under *its own* device — e.g.
  `/~name@1.0/<name>/<key>` resolves `<key>` on the named message. This is how a
  name fronts an arbitrary message graph.
- **As a host hook.** Installed under `on/request`, `name@1.0` turns
  `https://<name>.<node-host>/<rest>` into "resolve `<name>`, prepend it as the
  base, then serve `<rest>`". It is commonly stacked **before** other request
  hooks (e.g. a manifest device) so the named base is established first and the
  remaining path is interpreted by the downstream hook/device. The hook chains
  by **rewriting the body** it returns; later hooks see the rewritten list.
- **Resolver stacking.** The power of the device is the **ordered stack** of
  resolvers: e.g. operator local names first, then an on-chain naming device,
  then base32-ID subdomains, then a JSON snapshot — each a single
  `name-resolvers` element. New resolver types compose simply by being added to
  the list; each is a path-prefix binary or a device-bearing message, and each
  is consulted in turn until one claims the name.
- **Excludes & the base device.** `keys` and `set` deliberately fall through to
  `message@1.0`, so a `~name@1.0`-bound message remains inspectable/mutable;
  default-handler hygiene (excluding the manipulation keys) is what keeps a
  named binding from swallowing those operations.

## 10. Conformance (normative checklist)

1. The device is a **default-handler** device: it answers an arbitrary key by
   treating it as a name to resolve, and it **excludes** exactly `keys` and
   `set` (those forward to the base identity device).
2. With `name-resolvers` unset or `[]`, resolving any name yields the bare atom
   `not_found`, with no error and no side effect.
3. Resolution scans `name-resolvers` **in order** and returns the value of the
   **first** resolver that yields `{ok, _}`; a later resolver that would also
   match is **not** consulted once an earlier one matches (first-match wins,
   order significant).
4. A resolver that declines — returns a non-`{ok,_}` result, not-found, **or
   raises/crashes** — is skipped and the scan continues; a single resolver's
   failure never aborts the scan or surfaces to the caller.
5. A **binary** resolver `P` resolves name `K` by resolving the path
   `P <> "/" <> K`; a **map** resolver `M` resolves name `K` by resolving key
   `K` on `M` (honouring `M`'s own `device`). An in-line map
   `#{ <<"hello">> => <<"world">> }` resolves the name `hello` to `world`.
6. With request `load = false`, the resolver's value is returned **verbatim**
   (e.g. a bare ID stays a bare ID); the default (`load` absent) and
   `load = true` behave identically.
7. With `load = true` (the default), a resolved **identifier** (32/42/43-byte
   binary) is **read from the store** and the **stored message** is returned;
   a resolved **link** is forced to its target; any other value (a map) is
   returned unchanged.
8. `load` accepts a binary `true`/`false` as well as the boolean, coerced
   consistently; an absent `load` is treated as `true`.
9. The device performs **no writes** (no cache write, no commitment) and emits
   no outbound request of its own; the load step is a read only.
10. The `request` hook extracts the **subdomain** of the request `Host`
    relative to the node's `node-host` (then `host`, then a no-host fallback);
    a host with **no subdomain**, a host **equal to** the node host, or an
    **IP-literal** host yields a **skip**, and the hook returns the request
    **unmodified**.
11. On a skip, the hook returns `{ok, <unmodified hook request>}`; it never
    rewrites the body for a skipped request.
12. When a subdomain **is** present and resolves, the hook returns
    `{ok, #{ <<"body">> => NewBody }}` where the resolved message is **prepended
    as the base** of the execution body, applying the de-duplication rule (no
    double-execution when the path already names the same base; a path-bearing
    differing old base is retained behind the resolved base).
13. When a subdomain is present but resolution is **not-found** and the
    execution body is **empty** (root path), the hook returns
    `{error, #{ <<"status">> => 404, <<"body">> => <<"Not Found">> }}`.
14. When a subdomain is present but resolution is **not-found** and the
    execution body is **non-empty**, the hook returns the request **unmodified**
    (`{ok, <hook request>}`), deferring to normal path handling rather than
    404ing.
15. End-to-end (resolved-key): with `name-resolvers` set to a resolver that maps
    `K → <id-of-a-stored-message>`, `GET /~name@1.0/K` returns the stored
    message (loaded), and `GET /~name@1.0/K/<leaf>` returns `<leaf>` resolved on
    that message; with `load=false` the same `GET /~name@1.0/K` returns the bare
    ID.
16. End-to-end (hook): with the hook installed and a resolver mapping a
    subdomain `S` to a stored message, an HTTP request with
    `Host: S.<node-host>` and path `/<leaf>` is served as that message's
    `<leaf>`; an HTTP request to `Host: <bare-or-IP>/` (no subdomain) is served
    by normal handling, and `Host: <unknown-subdomain>.<node-host>` with path
    `/` returns HTTP 404.

## 11. Out of scope

- The **internal representation** of messages, identifiers, and links.
- The **behaviour of any specific resolver** — the path-prefix target's device
  semantics, and the map resolvers' devices (`local-name@1.0`, `b32-name@1.0`,
  on-chain naming devices, JSON-snapshot deserialisation). This spec constrains
  only how `name@1.0` *selects, orders, invokes, and loads* a resolver's output,
  not what any resolver computes. (Resolver specs are supplied separately for
  context.)
- The mechanics of the `on/request` hook **invocation** — how/when the request
  pipeline calls the hook, how the hook request (`request`, `host`, `body`) is
  assembled, and how the rewritten `body` is subsequently executed. This spec
  constrains only the hook's input→output rewrite contract.
- The precise parsing of arbitrary `Host` header forms beyond the rules in
  §4.`request` (e.g. exotic URL schemes in `node-host`, ports embedded in the
  host); only the subdomain-extraction outcomes specified (subdomain / equal /
  IP-literal / no-suffix) are pinned.
- **Result-cache configuration** of the hosting node (see §6) and any
  content-negotiation/codec applied to the returned value by the HTTP layer.
- Performance, concurrency, and the storage strategy of the underlying store.

## Open questions

- **Excludes are minimal (`keys`, `set` only).** Unlike the guidance that
  default-handler devices exclude the full manipulation set
  (`keys`/`set`/`set-path`/`remove`), this device excludes only `keys` and
  `set`. So a name equal to `set-path`, `remove`, `id`, `commit`, or `verify`
  would be captured by the resolver default rather than forwarded to the base
  device — i.e. those names are not directly resolvable as base-message
  operations through a `~name@1.0` binding. Flagged in case the intended exclude
  set is broader; the spec pins the observed behaviour (only `keys`/`set`
  forwarded).
- **Permissive identity uses the *signed* ID of a map base.** The hook's
  de-duplication compares a map old-base by its **signed** identity. For an
  **uncommitted** map base this identity is computed over the message as-is; the
  spec does not constrain what that yields for an unsigned message versus the
  identifier the subdomain resolved to, so the "same base" determination for an
  uncommitted inline base is only as well-defined as that identity. Flagged as a
  corner of the de-dup rule a reimplementer should mirror exactly (compare IDs;
  map → signed ID) rather than guess.
- **`load` coercion of unexpected values.** `load` is coerced to a boolean; the
  spec pins `true`/`false` (boolean or binary) and treats absent as `true`, but
  does not enumerate behaviour for other binaries (e.g. `1`/`0`). A reimplementer
  should route `load` through the same boolean coercion the rest of the system
  uses rather than inventing a parser.
