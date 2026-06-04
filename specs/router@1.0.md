# `router@1.0` — outbound request routing by rule matching

- **Device name:** `router@1.0`
- **Depends-on:** `message@1.0` (base device for reads and the
  mutation/inspection keys), `relay@1.0` and `apply@1.0` (the
  `preprocess` key emits a two-stage pipeline bound to these devices). The
  `message@1.0` spec is provided to reimplementers; `relay@1.0`/`apply@1.0`
  are referenced only by name in the emitted pipeline.
- **Status:** Draft

## 1. Overview

`router@1.0` decides **where an outbound request should go**. A node holds a
**routing table** — a precedence-ordered collection of **routes**, each a
**template** (the matcher) plus one or more **upstream nodes** (the
destinations). Given a request message, the device finds the first route whose
template matches, then, if that route names multiple upstreams, applies a
**load-distribution strategy** to choose a subset. The result is either a
concrete destination URI, a single upstream node descriptor, or the matched
route carrying its chosen list of nodes.

The device also answers questions *about* the table (`routes`, `match`),
mutates the table under operator authority (`routes` via POST, `register`), and
provides a request-hook entry point (`preprocess`) that rewrites a locally
received request into a relayed call to the matched upstream.

It sits below any device that needs to make an HTTP request to another node: a
relay, a gateway proxy, a chunk fetcher. Those callers ask `router@1.0` for a
route and then perform the transport themselves.

## 2. Concepts & terminology

- **Routing table / routes:** an ordered collection of **route** messages. May
  be supplied as an Erlang/AO-Core **list**, or as a **numbered map** (keys
  `1`, `2`, … contiguous from 1) which is treated as a list in that numeric
  order. Lower index = higher precedence (checked first).
- **Route:** a message describing one routing rule. Recognised keys:
  - `template` — the matcher (see **Template**). Absent template means "match
    everything" (an empty-map template matches every request).
  - `node` — a single destination, either a binary URI or a **node descriptor**
    map (see §4 `route`).
  - `nodes` — a list (or numbered map) of node descriptors, used when a route
    fans out to a cluster.
  - `strategy` — the load-distribution strategy name (see **Strategy**).
  - `choose` — how many nodes to select from `nodes` (default `1`).
  - `priority` — an integer used only to order the table when a route is added
    via POST (lower sorts earlier).
  - Any further keys (e.g. `parallel`, `responses`, `stop-after`,
    `admissible-status`) are **opaque pass-through** parameters: the device does
    not interpret them, but they MUST survive into the returned route so a
    transport layer can read them.
- **Template:** the per-route matcher. Two forms:
  - **Binary (path regex):** matched against the request's **target path**.
  - **Map (structural):** an optional path regex (carried inside the template
    under its own `path` / `route-path` key) AND a set of key/value constraints
    that the request must satisfy as a subset.
- **Node descriptor:** a map describing one upstream. Recognised keys: `prefix`,
  `suffix`, `match`+`with`, `uri`, `opts` (path-transform inputs, §4 `route`);
  and, depending on strategy, `weight`, `center`, `wallet`, `salt`, `min`,
  `max`.
- **Target path:** the path a request is matched/routed by. It is the request's
  `route-path` if present, else its `path`. (`route-path` is the
  externally-facing spelling; `path` is the internal spelling. `route-path`
  always wins when both are present.)
- **Strategy:** the algorithm selecting a subset of a route's `nodes`. One of
  `All`, `Random`, `By-Base`, `By-Weight`, `Nearest`, `Nearest-Integer`,
  `Range`, or any of these prefixed `Shuffled-` (see §4 `route` and §6).
- **Operator / route owner:** an address authorised to mutate the routing table
  (see §7).
- **Route provider:** an optional indirection that *computes* the routing table
  dynamically (by resolving a configured path/message) instead of reading a
  static list (see §6).

The **internal representation** of any of these is out of scope; the contract
is defined over logical key/value content and resolution results.

## 3. Device interface

- **Dispatch shape:** **explicit-keys.** The device answers exactly the keys
  `info`, `routes`, `route`, `match`, `register`, `preprocess` (and the bound
  base device, `message@1.0`, supplies `keys`/`set`/`id`/… for the message it is
  attached to). It is NOT a default-handler device: a key it does not name is
  not interpreted by the router.
- **Two call conventions for `route`:** `route` is defined as a device key
  `(Base, Req, Opts)` that **ignores `Base`** (routing is driven entirely by the
  node's table and the request), and MAY also be invoked as a standalone
  `(Req, Opts)` function (equivalent to `Base = undefined`). An implementation
  MUST make routing independent of `Base`.
- **Request shape consumed by routing:** the request message SHOULD carry a
  `path` (internal) or `route-path` (external) giving the target path; it MAY
  carry additional keys that a map template can constrain (`method`, custom
  keys) and that selection strategies can read (`route-by`). Missing target path
  is permitted — see §4/§8.

## 4. Resolved keys (normative)

### `info`
- **Reads:** nothing (static).
- **Behaviour:** Return a human-facing description of the device and its API
  (a `description`, a `version` of `1.0`, and an `api` map enumerating the keys
  above). The exact prose is non-normative; only that the key returns
  `{ok, Map}`.
- **Returns:** `{ok, Map}`.
- **Side effects:** none.

### `routes`
Read or mutate the routing table.
- **Reads:** the node's routing table (from node options, possibly via the route
  provider, §6); on mutation, the request `Req` (its `method`, its commitments,
  and the route fields to add). The node's `operator` / route-owner list and any
  registrar configuration (from node options).
- **Behaviour:**
  - If `method` (read from `Req`, default `GET`) is **not** `POST`: return the
    current routing table.
  - If `method` is `POST`: **add a route**. Two sub-modes:
    - **No registrar configured:** authorise then insert. The request MUST be
      signed by an authorised operator/route-owner (§7). If authorised, insert
      the request message (its route fields) into the table and **re-sort the
      whole table ascending by each route's `priority`** (a route with no
      `priority` sorts as if its key were absent — see §6 ordering), then
      install the new table as the node's routes. Return
      `{ok, <<"Route added.">>}`. If not authorised, return error
      `not_authorized`.
    - **Registrar configured:** do not insert locally; instead forward the
      registration to the configured registrar resolution (an indirection named
      in node options, optionally with a path override). On success return
      `{ok, <<"Route added.">>}`; on failure propagate the underlying error.
- **Returns:** `{ok, Table}` (GET) | `{ok, <<"Route added.">>}` (authorised
  POST) | `{error, not_authorized}` | `{error, Reason}` (registrar failure).
- **Side effects:** authorised POST **persists** a new routing table into the
  node's options (table mutation). The registrar sub-mode performs whatever
  resolution the registrar denotes (may include outbound calls).

### `route`
Find the destination for a request. **Ignores `Base`.**
- **Reads:** the node's routing table; from `Req` the target path
  (`route-path` else `path`), any keys a map template constrains, and the
  strategy inputs (`route-by`, and `path` for hash-based strategies).
- **Behaviour:**
  1. **Explicit URL short-circuit:** if the target path begins `http://` or
     `https://`, return that URL directly as the destination, bypassing the
     table. (When returned as a map it is `node => <URL>` with
     `reference => <<"explicit">>`.)
  2. Otherwise find the **first matching route** (§ template matching) in
     table order. If none matches, return error `no_matches`.
  3. From the matched route `R`:
     - If `R` has a binary `node`, return that binary URI: `{ok, Node}`.
     - If `R` has a map `node` (a node descriptor), **apply the path transform**
       (below) and return **exactly** `{ok, #{ <<"opts">> => <opts, default-typed>,
       <<"uri">> => <transformed path> }}` — **only these two keys**; the
       descriptor's other rule keys (`prefix`/`suffix`/`match`/`with`) are consumed
       by the transform and are NOT carried into this single-node result.
     - Else if `R` has `nodes`: **apply the path transform to every node** — each
       node **RETAINS all of its original descriptor keys** (`prefix`, `suffix`,
       `center`, `weight`, `wallet`, `salt`, `min`, `max`, … — the strategies read
       these *after* the transform) and **additionally gains** a resolved `uri`
       and a default-typed `opts`. **This is UNLIKE the single map-`node` case
       above** (which is reduced to exactly `#{opts,uri}`): the `nodes` entries are
       NOT reduced — they keep every key they had **plus** `uri`+`opts` (i.e. each
       transformed node = `OriginalNode` merged with `#{opts,uri}`). Producing a
       route whose `nodes` each carry a resolved `uri`. Then resolve the
       **strategy** (`strategy` key, default `All`, normalised per §6):
       - `All`: return `{ok, R'}` — the full matched route with every node's
         `uri` populated (caller hits all of them).
       - Any other strategy: compute `choose` = `min(N, length(nodes))` where
         `N` is the route's `choose` (default `1`, non-positive/garbage → `0`),
         select that many nodes by the strategy (§6), and return:
         - `[]` selected → error `no_matches`;
         - exactly one map node → `{ok, Node}` (that node descriptor, with its
           `uri`);
         - exactly one binary node → `{ok, NodeURI}`;
         - more than one → `{ok, R'#{ <<"nodes">> => Selected }}`.
     - Else (no `node`, no `nodes`): error `no_matches`.
- **Path transform (per node descriptor):** produce that node's request `uri`
  from the target path and the descriptor's rule keys, evaluated in this
  priority:
  1. `uri` present → use it verbatim.
  2. `prefix` present → `uri = prefix ++ target-path`.
  3. `suffix` present → `uri = target-path ++ suffix`.
  4. `match` + `with` present → `uri = ` target-path with **every** match of
     regex `match` replaced by `with` (global replace).
  The descriptor's `opts` sub-map is carried through onto the result under `opts`,
  **coerced to the node's default option types via
  `hb_opts:mimic_default_types(opts, existing, Opts)`** (each `opts` value whose
  key names a known node option is converted to that option's default
  representation — e.g. a binary `<<"httpc">>` for an option whose default is the
  atom `httpc` becomes that atom), and is removed from the map the transform
  reads. A descriptor with no `opts` yields `opts => #{}` (default-typed). If the
  descriptor supplies none of the above and no `uri`, the per-node transform has
  no defined output and that node is left unchanged (it keeps whatever keys it
  had).
- **Returns:** `{ok, Binary}` (a concrete URL) | `{ok, NodeDescriptor}` |
  `{ok, Route}` (route carrying `nodes`) | `{error, no_matches}`.
- **Side effects:** none on the table. MAY read the route provider (§6), which
  can perform resolution.

### `match`
Return the **first matching route** message itself (not a destination).
- **Reads:** the routing table to match against is taken from `Base` (its
  `routes` key, read as a `message@1.0` value), **not** from node options; the
  target path and constraint keys from `Req`. **Read it as inert data via
  `hb_ao:get(<<"routes">>, {as, <<"message@1.0">>, Base}, [], Opts)` — NOT a bare
  `hb_ao:get(<<"routes">>, Base, …)`, which would re-enter THIS device's own
  `routes` key (returning the node-options table, not `Base`'s field). This is
  the default-handler/explicit-key self-recursion trap.**
- **Behaviour:** Compute the target path of `Req` (`route-path` else `path`).
  Find the first route in `Base`'s `routes` whose template matches (§ template
  matching, including the explicit `http(s)://` short-circuit which yields the
  synthetic `#{ node => <URL>, reference => <<"explicit">> }`). Return the
  matched route augmented with a `reference` key naming its position
  (`routes/<index>`).
- **Returns:** `{ok, Route}` | `{error, no_matching_route}`.
- **Side effects:** none.

### `register`
Tell **this** node to register one or more of its **offered routes** with a
remote router peer. Idempotent (safe to call once; repeated calls re-post).
- **Reads:** the node's configured **offered** route(s) (from router options; a
  single map or a list of maps); from `Req` an optional `as` selector naming an
  identity to sign as.
- **Behaviour:** For each offered route, read its `registration-peer` (the
  remote router's location) and POST a signed registration message to that
  peer's `routes` endpoint. The registration body names the offered route and an
  action of `register`; it is committed/signed (as the `as` identity if given,
  else the node's default identity) so the peer can authenticate and
  charge/verify.
- **Returns:** `{ok, <<"Routes registered.">>}`.
- **Side effects:** outbound signed POST(s) to remote router peer(s).

### `preprocess`
A **request hook**: rewrite a request the node just received into a pipeline
that relays it to the matched upstream, or run it locally if no route matches.
- **Reads:** from `Req` the wrapped inbound `request` (and the original `body`);
  the node's routing table; from `Base` an optional `commit-request` flag
  (default `false`); from node options a `router-preprocess-default` of `local`
  (default) or `error`.
- **Behaviour:** Attempt to route the wrapped `request` (as in `route`).
  - **No route matches:** fall back per `router-preprocess-default`:
    - `local` → return `{ok, #{ <<"body">> => <inbound body> }}` (the request
      proceeds locally, unmodified).
    - `error` → return `{ok, #{ <<"body">> => [ #{ <<"status">> => 404,
      <<"message">> => <<"No matching template found in the given routes.">> }
      ] }}`.
  - **A route matches (resolving to a concrete upstream `Node`):** emit a
    **two-stage pipeline** as a 2-element `body` list that proxies the user's
    request to the upstream via the relay device. The exact stage maps (keys
    normative):
    1. **Stage 1** binds the relay: `#{ <<"device">> => <<"relay@1.0">>,
       <<"relay-device">> => <<"apply@1.0">>, <<"method">> => <<"POST">>,
       <<"peer">> => <upstream Node> }`, **plus** `<<"commit-request">> => true`
       **iff** the base's `commit-request` was true (omit the key entirely when
       false).
    2. **Stage 2** invokes `call`: `#{ <<"path">> => <<"call">>,
       <<"target">> => <<"proxy-message">>, <<"proxy-message">> => ProxyMsg }`
       where `ProxyMsg = #{ <<"device">> => <<"apply@1.0">>,
       <<"path">> => <<"user-path">>, <<"source">> => <<"user-message">>,
       <<"user-path">> => <the user's path>,
       <<"user-message">> => <the user request (committed per below)> }`.
    The user's inbound request is **committed first if it is unsigned** (an
    unsigned `httpsig@1.0` commitment is added — `commitment-device =>
    httpsig@1.0`, `type => unsigned`) so headers the relaying node adds are not
    folded into the user's request; an already-signed request (non-empty signers)
    is left as-is. That (possibly-committed) request is the `user-message`. The
    user's `path` MUST be a non-empty binary, else **raise** `invalid_user_path`.
- **Returns:** `{ok, #{ <<"body">> => … }}` (a single-message local body, an
  error-status body, or a two-element relay pipeline). On a bad user path,
  raises `invalid_user_path`.
- **Side effects:** none directly; the emitted pipeline, when later resolved by
  the caller, performs the relayed outbound request.

## 5. Data formats & encodings

- **Keys** are binary, lowercase, hyphenated on the wire (`route-path`,
  `commit-request`, `registration-peer`, …).
- **URIs / node addresses** are binaries. Wallet addresses used by `Nearest`
  are base64url (43-char) human IDs (never hex).
- **Templates:**
  - A **binary template** is a regular expression matched against the target
    path. Matching is via substring regex search (`re:run` semantics), **not**
    anchored, UNLESS the regex begins with `^` (in which case it anchors at the
    start). Before matching, both the path and a non-`^` regex are normalised to
    have a single leading `/`. Thus `/.*/schedule` matches `/abc/schedule` and
    `/a/b/c/schedule`; `^/arweave` matches only paths beginning `/arweave`; a
    bare substring like `worker` matches any path containing `worker`.
  - A **map template** matches iff: (a) if the template itself carries a path
    regex (under its own `path` or `route-path`), that regex matches the target
    path; AND (b) every **other** key/value pair in the template is present with
    an equal value in the request (structural subset match). A template of `{}`
    (or a route with no `template`) matches every request.
  - The special template binary `*` is a regex that matches any non-empty path
    (used as a catch-all in examples).
- **Strategy names** are compared case-insensitively with `-` and `_` folded
  (see §6). The canonical forms are `All`, `Random`, `By-Base`, `By-Weight`,
  `Nearest`, `Nearest-Integer`, `Range`, and `Shuffled-<Base>`.
- **Hash-to-integer:** strategies that map a value to a 256-bit integer use:
  a native/human 32-byte ID decoded big-endian; a 32-byte binary decoded
  big-endian; any other binary first SHA-256'd then decoded; an integer used
  as-is. `route-by` is interpreted as an integer when it parses as a decimal
  integer, otherwise hashed.
- **`reference`** on a matched route is a path string: `routes/<index>` for a
  table hit, or `explicit` for the `http(s)://` short-circuit. It is added by
  the device, not supplied by the user.

## 6. Ordering, freshness & caching

- **Route table ordering = precedence.** Routes are evaluated **in order**;
  the **first** matching route wins. A list is evaluated in list order; a
  numbered map is evaluated in ascending numeric-key order (`1`, `2`, …).
- **Priority on insert.** When a route is added via POST (no-registrar mode),
  the whole table is re-sorted **ascending by `priority`** (stable for equal
  priorities, lower value earlier). `priority` affects ONLY this insert-time
  sort; at match time, precedence is purely positional. An implementation
  SHOULD compute the sort without recomputing message hashpaths (a performance
  concern, not observable).
- **Route provider (dynamic table).** The table source is selected by the node
  option **`router-opts`** (a map): if `router-opts` carries a **`provider`** key
  (an indirection — a message or path to resolve, or a **list** of them to
  resolve in sequence), the table is obtained by resolving that provider and
  reading the resulting `routes` (or the result itself if it is already a route
  list). **Resolve the provider AS-IS** — a non-list provider with
  `hb_ao:resolve(Provider, Opts)` (the 2-arg form: the provider message/path
  carries its **own** `path`, e.g. `/key/routes`, which drives the navigation —
  do **NOT** inject a `path` such as `routes`); a **list** provider with
  `hb_ao:resolve_many(List, Opts)` (resolved in sequence, the final result
  supplying the table). The resolved result is then the route **list** directly,
  or a message from which the `routes` key is read. **Otherwise** (no provider)
  the table is the static top-level **`routes`** node option
  (default `[]`). So the read order is: `router-opts.provider` (dynamic) → else
  top-level `routes` (static). A provider that fails MUST surface as an error
  (`routes_provider_failed`). Provider results are treated as fully-loaded route
  data. The provider mechanism is how prices/weights/membership can change at
  runtime.
- **Strategy normalisation.** The `strategy` value is lower-cased, `-`/`_`
  folded, and mapped to a canonical name; a `shuffled-`/`shuffled_` prefix is
  stripped, the remainder normalised, and the result re-prefixed `Shuffled-`.
  **Any unrecognised strategy normalises to `All`.**
- **Selection determinism (per strategy):**
  - `All` — return every node (order preserved). Deterministic.
  - `Random` — choose `N` nodes uniformly at random without replacement.
    **Non-deterministic.**
  - `By-Weight` — choose `N` nodes by weighted-random without replacement, each
    node's weight read from its `weight` key. **Non-deterministic**, but biased
    by weight (a node with far higher weight is chosen far more often).
  - `By-Base` — derive an integer from the request's hash key
    (`path`, else `route-by`, else the supplied hashpath) and pick the node at
    `index = (hashInt mod count) + 1`, repeating on the remaining nodes for `N`.
    **Deterministic**: identical hash + node set ⇒ identical selection.
  - `Nearest-Integer` — read `route-by` (or derive an integer from `path`),
    score each node by the **circular distance** (§ field distance) between that
    integer and the node's `center`, and return the `N` lowest-distance nodes
    (ascending). A node lacking `center` is scored at `2^256` (effectively last).
    **Deterministic.**
  - `Nearest` — for each node, compute `SHA-256(hashpath ++ ":" ++ wallet
    [++ ":" ++ salt])`, score by circular distance to the (normalised, 32-byte)
    hashpath, return the `N` lowest-distance nodes. A node lacking a binary
    `wallet` causes the strategy to **fail** (`wallet_not_found` /
    `invalid_wallet`). **Deterministic** for a fixed node set + hashpath.
  - `Range` — keep only nodes whose `[min, max]` window contains `route-by`
    (`min` default −∞, `max` default +∞; bounds inclusive), then take the first
    `N` of the survivors in their original order. **Deterministic.**
  - `Shuffled-<Base>` — run `<Base>` **with the same `choose` `N`** (so it yields
    the base strategy's `N` selected members), then apply `Random` selection over
    **that `N`-member list** — which simply re-orders those `N`. Equivalently:
    `Random(N, <Base>(N, Nodes))`. So the returned **set** is exactly the base
    strategy's `N`-member selection (NOT a random `N` drawn from all nodes); only
    the **order** is randomised. **Non-deterministic** (re-orders the base result).
- **Field distance (circular, 256-bit):** for integers `A`, `B`,
  `distance = min(|A−B|, 2^256 − |A−B|)`. This treats the 256-bit space as a
  ring so the two nearest directions are both considered.
- **Lowest-distance tie-break:** when selecting by distance, a candidate
  replaces the current best only if its distance is **strictly less**; an
  `infinity`/sentinel distance is treated as immediately selectable when no
  finite best has been seen. Equal finite distances keep the earlier candidate.
- **Caching / freshness.** The device performs no result caching of its own.
  Because routing can be **mutable at a constant path** (the `routes` POST
  changes the table, and a provider changes it at runtime), a node that caches
  HTTP resolutions MUST be configured so that `route`/`routes` reads are not
  served stale (result caching disabled for these paths). This is node
  configuration, not device behaviour.

## 7. Security & authority

- **Adding a route (no registrar)** is **operator-gated**: the POST request MUST
  carry a commitment from an address in the node's authorised set — the node
  `operator`, plus any configured additional route-owners. The check is: at
  least one authorised address appears among the request's committers/signers.
  If not, the device returns `not_authorized` and the table is unchanged
  (**failure-closed**).
- **Registrar mode** delegates the authority decision to the configured
  registrar resolution; the local node does not itself gate in that mode.
- **`register`** signs its outbound registration with the node's identity (or
  the `as` identity), so the remote peer can authenticate the offer.
- **`preprocess`** signs the user's request with an **unsigned** commitment only
  when it was previously unsigned — purely to fence off relay-added headers; it
  MUST NOT add a *signed* commitment on the user's behalf. An already-signed
  user request is forwarded untouched.
- **Reading** the table (`routes` GET, `route`, `match`) requires no authority.

## 8. Errors

- `no_matches` — `route` found no matching route (and no explicit-URL
  short-circuit), or a strategy selected zero nodes, or a matched route had
  neither `node` nor `nodes`.
- `no_matching_route` — `match` found no matching route.
- `not_authorized` — a `routes` POST (no-registrar mode) was not signed by an
  authorised operator/route-owner.
- `invalid_user_path` — `preprocess` matched a route but the user's request had
  an empty or non-binary `path`.
- `invalid_replace_args` — a node's `match`/`with` path transform could not
  produce a replacement.
- `wallet_not_found` / `invalid_wallet` — the `Nearest` strategy encountered a
  node without a usable binary `wallet`.
- `routes_provider_failed` — the configured route provider could not be
  resolved.
- Registrar-mode POST failures propagate the registrar's own `{error, Reason}`
  unchanged.

## 9. Composition

- **As a routing oracle for transports.** A relay/proxy/fetch layer resolves
  `route` on this device, then performs the transport: a binary result is hit
  directly; a single node descriptor is hit at its `uri` with its `opts`; a
  route carrying `nodes` is fanned out (the caller reads pass-through keys such
  as `parallel`, `responses`, `stop-after`, `admissible-status` to govern the
  fan-out). The router itself performs no HTTP.
- **As a request hook.** A node configures `preprocess` as its inbound
  `request` hook. On a match it returns a pipeline that **switches devices**:
  stage 1 carries `device => relay@1.0`, so the relay device takes over the
  outbound proxying; stage 2 invokes `apply@1.0` indirection to run the user's
  path. This is the supported way to make a node transparently forward matched
  traffic to upstreams while charging/verifying via the relay.
- **As a price/match source for other devices.** Because `match` returns the
  matched route message (with its arbitrary keys), other devices (e.g. a
  pricing device) can resolve `match` against a `routes`-bearing base to read a
  per-route attribute (such as a `price`) for an incoming path.
- **Dynamic tables.** Pairing the `provider` (read side) with a `registrar`/
  `offered`/`register` (write side) lets an external process (e.g. a scheduled
  computation) own and continuously adjust the routing table; the device reads
  whatever that process currently publishes.

## 10. Conformance (normative checklist)

An implementation MUST exhibit all of the following, observable via resolve /
HTTP:

1. `routes` with method `GET` (or absent method) returns the current routing
   table; the table preserves insertion/config order for matching.
2. A target path beginning `http://` or `https://` is returned by `route`
   **verbatim** as the destination, without consulting the table.
3. `route` selects the **first** route (in table/numbered-map order) whose
   template matches; later matching routes are ignored.
4. A **binary** template matches the target path by unanchored regex, except a
   `^`-prefixed regex anchors at the start; `/.*/schedule` matches both
   `/abc/schedule` and `/a/b/c/schedule`, and does not match `/a/b/c/other`.
5. A **map** template matches iff its embedded path regex (if any) matches AND
   every other template key equals the corresponding request key; a route with
   no/empty template matches every request (usable as a fallback).
6. `route-path` overrides `path` as the target path wherever both are present.
7. A matched route with a binary `node` returns that binary URI; with a map
   `node` returns `{ok, #{ uri, opts }}` after applying the path transform.
8. Path transform precedence is `uri` > `prefix` > `suffix` > `match`+`with`;
   `prefix` prepends, `suffix` appends, `match`+`with` does a **global** regex
   replace on the path; the node's `opts` is carried onto the result.
9. With multiple `nodes` and strategy `All` (the default), every node is
   returned with its `uri` populated.
10. `choose` caps the number selected to `min(choose, #nodes)`; non-positive or
    non-integer `choose` yields zero selected (→ `no_matches`); default `choose`
    is `1`.
11. `By-Base` is **deterministic**: the same target-path/hash and node set
    always select the same node(s).
12. `Nearest-Integer` returns the `choose` nodes whose `center` values are
    closest (by 256-bit **circular** distance) to `route-by`; a node without
    `center` is ordered last.
13. `Range` returns nodes whose inclusive `[min, max]` window contains
    `route-by` (open-ended when a bound is absent), capped to `choose`.
14. `Random` / `By-Weight` distribute across nodes non-deterministically;
    `By-Weight` biases selection by each node's `weight`; selection is **without
    replacement** (no node appears twice in one result).
15. `Shuffled-<Strategy>` produces the base strategy's members in a randomised
    order.
16. Strategy names are case-insensitive with `-`/`_` folding; an unrecognised
    strategy behaves as `All`.
17. Selecting exactly one node returns that node (map or binary) unwrapped;
    selecting more than one returns the route with its `nodes` narrowed to the
    selection; selecting none returns `no_matches`.
18. Pass-through route keys (`parallel`, `responses`, `stop-after`,
    `admissible-status`, etc.) survive into the returned route unchanged.
19. `match` returns the matched route message itself, with a `reference` of
    `routes/<index>` (or `explicit` for an `http(s)://` short-circuit); no match
    returns `no_matching_route`. `match` reads its routes from `Base`.
20. A `routes` POST in no-registrar mode adds the route **iff** the request is
    signed by an authorised operator/route-owner (returns `Route added.`),
    otherwise returns `not_authorized` and leaves the table unchanged; after a
    successful add, the table is re-sorted ascending by `priority`.
21. With a registrar configured, a `routes` POST is forwarded to the registrar
    resolution rather than inserted locally; success returns `Route added.`,
    failure propagates the registrar error.
22. `register` reads the node's offered route(s) and POSTs a **signed**
    registration to each route's `registration-peer`, returning
    `Routes registered.`.
23. `preprocess` with no matching route returns `{ok, #{ body => <inbound
    body> }}` when `router-preprocess-default` is `local`, or a body of a single
    `status: 404` message when it is `error`.
24. `preprocess` with a matching route returns a two-element `body` list: stage 1
    binds `relay@1.0` (`relay-device => apply@1.0`, `method => POST`, `peer =>
    <upstream>`, and `commit-request => true` iff the base requested it); stage 2
    is `#{ path => call, target => proxy-message, proxy-message => #{ device =>
    apply@1.0, path => user-path, source => user-message, user-path => <path>,
    user-message => <user request> } }` (§4 `preprocess`). An unsigned user
    request is committed (unsigned httpsig) before relay and becomes the
    `user-message`, a signed one is left untouched; an empty user path **raises**
    `invalid_user_path`.
25. If a `provider` is configured, the table is computed by resolving it rather
    than read statically; a provider failure surfaces as an error.

## 11. Out of scope

- The internal representation of routes, node descriptors, and the table.
- The transport layer itself (how a chosen URI/route is actually fetched, the
  meaning of `parallel`/`responses`/`stop-after`/`admissible-status`, retry and
  wave scheduling) — this device only *selects*; it does not perform HTTP.
- The behaviour of `relay@1.0` and `apply@1.0` beyond the shape of the pipeline
  `preprocess` emits.
- The cryptographic details of commitment/verification (see `message@1.0` and
  the commitment device).
- The exact RNG used by `Random`/`By-Weight`/`Shuffled-*` (only the
  distribution properties are normative).
- The wire/storage format of the node-options that hold the table, provider,
  registrar, offered routes, operator, and route-owners (these are node
  configuration; only their observable effect on the keys above is specified).
- Performance, the insert-time hashpath-avoidance optimisation, and any
  human-readable text in the `info` response.
