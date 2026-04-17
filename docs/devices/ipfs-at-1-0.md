# Device: ~ipfs@1.0

## Overview

The `~ipfs@1.0` device is an **optional, user-loadable** commitment device that lets a HyperBEAM node address messages by their [IPFS CIDv1](https://github.com/multiformats/cid). It computes a CID over a message's `body` and attaches it as an [unsigned commitment](../resources/unsigned-commitments.md) — a cryptographic commitment that has no `committer`, only a content-addressed identity.

The elegance comes from HyperBEAM's existing machinery, not from any new plumbing: `hb_cache:write/2` already links every commitment ID to the uncommitted root ID of the message it belongs to. Once an `~ipfs@1.0` commitment is attached, `hb_cache:read(CID, Opts)` finds the message — so a standard `GET /<CID>` request resolves without any routing, path, or kernel change.

This device covers the outer edges of the IPFS / IPLD spec intentionally: `sha2-256` multihashes, base32-lowercase multibase, and the `raw` (multicodec `0x55`) and `dag-cbor` (multicodec `0x71`) codecs. See the phase-2 notes below for what's coming next, and the **Non-goals** section for what this device will never do.

`~ipfs@1.0` is **not** in `preloaded_devices` by default. A node operator opts in; see **Enabling** below.

## When to use it

- Serving content to IPFS clients (`GET /<bafk…>` returns the `body` bytes that hash to the CID).
- Exchanging content-addressed payloads with other IPFS-aware peers.
- Acting as a caching mirror of public IPFS data via the companion `hb_store_ipfs_gateway` store backend.

If your content only needs to be addressed within HyperBEAM's own ID space, use [`~httpsig@1.0`](httpsig-at-1-0.md) or [`~ans104@1.0`](ans104-at-1-0.md) instead — they give you signed commitments with a committer.

## Enabling

Two ways, pick whichever fits your deployment:

### In node config

```erlang
{preloaded_devices, DefaultDevices ++ [
    #{<<"name">> => <<"ipfs@1.0">>, <<"module">> => dev_codec_ipfs}
]}.
```

### Per-message, for ad-hoc use

```erlang
Msg = #{ <<"body">> => <<"hello world">> },
Committed =
    hb_message:commit(
        Msg,
        Opts,
        #{ <<"commitment-device">> => <<"ipfs@1.0">>,
           <<"type">>              => <<"unsigned">> }
    ).
```

For external CID reads, append `hb_store_ipfs_gateway` after your local stores in the node `store` chain:

```erlang
{store, [
    #{ <<"store-module">> => hb_store_lmdb, <<"name">> => <<"main">> },
    #{ <<"store-module">> => hb_store_ipfs_gateway,
       <<"gateways">>     => [<<"https://w3s.link">>, <<"https://ipfs.io">>] }
]}.
```

The gateway store hashes every fetched body against the requested CID before handing it up the chain. A lying gateway is treated as `not_found` and the next one is tried.

## Core operations

### `commit` — attach a CID

Compute a CIDv1 over `Msg`'s `body` and add it as an unsigned commitment. The commitment map is keyed by the CID string:

```erlang
#{
    <<"commitments">> => #{
        <<"bafkreifzjut3te2nhyekklss27nh3k72ysco7y32koao5eei66wof36n5e">> => #{
            <<"commitment-device">> => <<"ipfs@1.0">>,
            <<"type">>              => <<"unsigned">>,
            <<"codec">>             => <<"raw">>,
            <<"hash-alg">>          => <<"sha2-256">>,
            <<"committed">>         => [<<"body">>]
        }
    }
}
```

**Supported `Req` fields**

| Field | Default | Values |
| --- | --- | --- |
| `type` | (none — required) | `unsigned`, `unsigned-sha256` |
| `codec` | `raw` | `raw` (0x55), `dag-cbor` (0x71) |
| `hash-alg` | `sha2-256` | `sha2-256` |

Anything else — `signed`, a wallet, a second hash function — returns `{error, {unsupported_type, _}}` or `{error, {unsupported_codec, _}}`. IPFS does not have signed CIDs.

### `verify` — check a CID

Recompute the CID from `body` with the commitment's declared codec + hash-alg, then confirm it is a key in the message's `commitments` map. Tampering with the body produces a different CID, which is not present — verification returns `{ok, false}`. Called implicitly by `hb_message:verify/2,3`.

### `committed` — list covered keys

`dev_message:committed/3` reads the commitment's own `committed` list. For `~ipfs@1.0` that list is always `[<<"body">>]`.

### `content_type` — MIME

`application/vnd.ipld.raw` for `codec = raw`, `application/vnd.ipld.dag-cbor` for `codec = dag-cbor`. Falls back to `application/vnd.ipld.raw` when unspecified.

## End-to-end example

```erlang
%% 1. Stamp a blob with its CID.
Msg       = #{ <<"body">> => <<"hello world">> },
Committed = hb_message:commit(Msg, Opts,
                #{ <<"commitment-device">> => <<"ipfs@1.0">>,
                   <<"type">> => <<"unsigned">> }),
%% 2. Write it. Cache auto-links the CID to the uncommitted ID.
{ok, _UncommittedID} = hb_cache:write(Committed, Opts),
%% 3. Read by CID. Works because of the link established in step 2 —
%%    nothing special, no new path, no routing change.
{ok, Recovered} =
    hb_cache:read(
        <<"bafkreifzjut3te2nhyekklss27nh3k72ysco7y32koao5eei66wof36n5e">>,
        Opts
    ),
<<"hello world">> =
    hb_cache:ensure_loaded(maps:get(<<"body">>, Recovered), Opts).
```

## What's next (phase 2)

A pure-Erlang dag-cbor encoder/decoder (`dev_codec_ipfs_cbor`) and proper `to/3` / `from/3` routed through [`~structured@1.0`](../resources/source-code/dev_codec_structured.md), so that a HyperBEAM message with native types and links round-trips bit-for-bit against the IPLD codec-fixtures. Phase 1 treats the `dag-cbor` codec as an opaque blob for hashing only; phase 2 makes it a full peer of [`~json@1.0`](json-at-1-0.md).

## Non-goals

- CIDv0 (legacy base58 dag-pb CIDs, `Qm…`).
- `dag-pb`, UnixFS, file chunking.
- Hash algorithms other than `sha2-256`.
- Multibases other than base32-lower (decode accepts `B`/`f` defensively).
- IPNS, bitswap, pubsub, libp2p.
- IPLD Schemas, Selectors, or path resolution into sub-blocks.

## Related source

- [`dev_codec_ipfs.erl`](../resources/source-code/dev_codec_ipfs.md) — device entry points.
- [`dev_codec_ipfs_cid.erl`](../resources/source-code/dev_codec_ipfs_cid.md) — varint, multihash, multibase, CIDv1.
- [`hb_store_ipfs_gateway.erl`](../resources/source-code/hb_store_ipfs_gateway.md) — read-only gateway store.
- [`dev_codec_ipfs_test.erl`](../resources/source-code/dev_codec_ipfs_test.md) — integration tests including the cache-linkage proof.
