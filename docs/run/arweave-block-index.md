# Deploying the Arweave block index

Deploy the complete `feat/copycat-block-index` stack through `54273a215`.
It includes the compact block index (`6edf9fae2`), reuse of containing headers
within GraphQL pages (`1c4cc587f`), and unfiltered base-layer TX pagination.
Rebuild the release's preloaded device store so `arweave@2.9`, `copycat@1.0`
and `query@1.0` are updated together, then restart using the retained stores.

## Configuration

Merge these settings into the serving node's existing `config.json`. Use the
**existing block-store path** if headers have already been backfilled; the path
below is an example, not a migration destination.

```json
{
  "arweave-block-store": {
    "store-module": "hb_store_lmdb",
    "ao-types": "store-module=\"atom\"",
    "name": "/var/lib/hyperbeam/arweave-blocks"
  },
  "arweave-index-blocks": true,
  "query-arweave-remote-block-ranges": true
}
```

- `arweave-block-store` holds both cached headers and the compact index. Copycat
  and GraphQL must use the same configured store. Each serving node needs access
  to populated block storage. With no explicit block store, the node's `store`
  is used instead.
- `arweave-index-blocks=true` retains headers fetched on demand. `mode=blocks`
  stores headers regardless of this setting.
- `query-arweave-remote-block-ranges=true` permits GraphQL to fetch missing
  headers during backfill. It is already the default. With it disabled,
  unfiltered queries need a compact indexed tip and all headers they traverse.
- Existing `match-index`, `arweave-index-store`, `pending-store` and published
  ArLMDB definitions need no changes. There is no match-index rebuild or ArLMDB
  re-upload for this feature.

`include-block-index` is a **Copycat request argument**, defaulting to false.
Adding it at the top level of `config.json` does not enable it. Add
`&include-block-index=true` to the configured request used by each confirmed
ingestion/backfill job. Keep the existing upstream Arweave routing; the compact
import needs the native `/block_index/LOW/HIGH` endpoint.

Roll out in this order: deploy the code and retain the existing block store,
complete the compact import below, then enable `include-block-index=true` on
recurring ingestion. With that argument, a missing compact entry makes a block
incomplete for Copycat even if its TXs were already indexed. Bootstrapping first
avoids reprocessing historical TXs just to populate compact entries.

## Match index read batches

With the store batching patch, `match@1.0` requests `limit=batch`. Each store
chooses its batch policy. Deploy the core changes with rebuilt `match@1.0`
and `query@1.0` devices. On the existing LMDB message inside `match-index`,
start with `"list-batch-size": 32`; retain its name and other settings:

```json
{
  "store-module": "hb_store_lmdb",
  "ao-types": "store-module=\"atom\"",
  "name": "/var/lib/hyperbeam/match-index",
  "list-batch-size": 32,
  "from-list": "~match@1.0/entries"
}
```

The default is 256, with a minimum of 2 for inclusive cursor progress.
Volatile and filesystem stores honor the same setting and bounded default.
`batch-size` controls write flushing, independently. Move any node-level
`match-batch-size` tuning to this store setting. ArLMDB retains its natural
pages. No reindex is needed.

`from-list` normalizes a returned batch in one AO-Core call. Add the native
`~match@1.0/entries` path to each native `match-index` read definition, including
the pending index view. Leave the message `store` and `pending-store` writer
definitions as they are. On the published index with 39/40/49-bit fields, add:

```json
{
  "from-list": "~match@1.0/members&key-hash-size=39&value-hash-size=40&offset-size=49&commitment-device=ans104@1.0"
}
```

Retain its existing `from-key`, `to-key`, root, prefix and other settings.
`from-list` replaces per-child normalization only for listing. Deploy updated
`hb_store` and rebuilt `match@1.0` before enabling it. Custom encodings need
their own equivalent batch decoder; omitting `from-list` keeps per-key decoding.

## Generate the starting index

Run these requests against the node's local administrative listener, avoiding
public gateway timeouts. Substitute its actual port:

```sh
HB_NODE='http://127.0.0.1:8002'
```

First import the compact index from the current tip through genesis:

```sh
curl --fail-with-body -sS \
  "$HB_NODE/~copycat@1.0/arweave&mode=block-index&from=tip&to=0&reindex=false"
```

This imports the native block hash, end weave size and transaction root,
inferring each height from the native range order. Requests are automatically
split into ranges of at most 10,000 entries. It downloads **no block headers,
TX headers, payloads or pending messages**. Existing cached headers are reused
by GraphQL once the corresponding index entries exist.

The explicit `to=0` matters: it visits the complete range and fills holes.
`reindex=false` skips existing compact entries, so the same bounded request
can resume an interrupted import. Without `to`, Copycat stops at the first
indexed height, which is suitable for catching up but not filling old gaps.
The direct request returns the lower bound, `0`, on successful completion.

## Populate the headers

The compact index finds the right block; GraphQL still needs its header for
TX IDs, height and timestamp. Continue the existing header backfill in the
same store. To fill every remaining header from the tip down, use:

```sh
curl --fail-with-body -sS \
  "$HB_NODE/~copycat@1.0/arweave&mode=blocks&from=tip&to=0&reindex=false&include-proofs=false&include-block-index=true"
```

`mode=blocks` only processes block headers. It does not enumerate/download TXs
or bundled data, or run pending ingestion. `include-proofs=false` excludes
`poa`/`poa2` from cached headers. Cached headers satisfying the request are
skipped; they do not need to be downloaded again. If a header exists but its
compact entry does not, the entry can be generated from that cached header.

For long backfills, use the existing background-job mechanism. The equivalent
one-shot cron launch is:

```sh
curl --fail-with-body -sS \
  "$HB_NODE/~cron@1.0/once?cron-path=/~copycat@1.0/arweave&mode=blocks&from=tip&to=0&reindex=false&include-proofs=false&include-block-index=true"
```

The returned body identifies the task. This confirms launch, not completion.
`/~cron@1.0/report=TASK_ID` reports whether it is active;
`/~cron@1.0/stop=TASK_ID` stops that task. An inactive report does not distinguish
success from an error; inspect the node's task logs and stored entries. The
same cron form with `mode=block-index` can run the compact import in the
background. Cron workers do not survive a node restart; recreate them through
the deployment's existing job setup. Avoid duplicating an active backfill.

## Keep it current

Add `&include-block-index=true` to the existing recurring confirmed Copycat
request, preserving its other arguments. For example:

```text
/~copycat@1.0/arweave&mode=full&include-block-index=true&include-proofs=false
```

This records compact entries from the headers already read during ingestion.
It also works with `blocks`, `shallow`, `deep` and `list` modes. A pending-only
request processes no confirmed blocks and cannot advance the compact tip.

If confirmed ingestion is managed separately, this compact-only catch-up
request advances from the current tip to the first indexed height:

```text
/~copycat@1.0/arweave&mode=block-index&from=tip
```

Unfiltered GraphQL uses the highest **indexed** height whenever compact entries
exist, so a stale compact tip means stale latest-TX results. If the index is
empty, remote-enabled queries fall back to network status. Ordinary GraphQL
header reads do not advance the compact index.

For a deliberate refresh of existing compact entries, supply both numeric
bounds and `reindex=true`. For example:

```text
/~copycat@1.0/arweave&mode=block-index&from=2001426&to=2001425&reindex=true
```

This refreshes the canonical rows from the upstream index. With `to` omitted,
even `reindex=true` stops at the first indexed height.

## Inspect and query

These bounded reads inspect the compact index without enumerating all headers:

```sh
curl --fail-with-body -sS -H 'accept: application/json' \
  "$HB_NODE/~arweave@2.9/blocks&direction=desc&limit=1"
curl --fail-with-body -sS -H 'accept: application/json' \
  "$HB_NODE/~arweave@2.9/blocks&height=0"
```

The first returns the indexed tip; the second returns the genesis entry after
a full import. They check the endpoints, not completeness between them. A
height lookup is exact: a missing height returns an empty list.

```sh
curl --fail-with-body -sS -H 'content-type: application/json' \
  --data '{"query":"{transactions(first:10,sort:HEIGHT_DESC){pageInfo{hasNextPage}edges{cursor node{id block{height timestamp}}}}}"}' \
  "$HB_NODE/~query@1.0/graphql"
```

Add `block:{min:2001425,max:2001426}` for an inclusive height range. Either
bound can be omitted. Use `HEIGHT_ASC` for ascending order, and pass the last
returned cursor unchanged as `after` with the same filters and ordering.

These unfiltered/block-only queries return **confirmed base-layer TXs**;
bundled items and pending messages are outside this enumeration. Within each
height, IDs sort lexically. Selecting only `edges { cursor }` avoids TX reads;
selected nodes use the configured stores and fetch missing TX headers by ID.
`count` remains capped at `query-arweave-max-index-count` (default 1000).
Queries with tags, owners, recipients or explicit IDs retain their existing
matching paths.
