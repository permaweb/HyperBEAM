# Device: ~ipfs@1.0

## Overview

The `~ipfs@1.0` device is an **optional, user-loadable** commitment device that lets a HyperBEAM node address messages by their [IPFS CIDv1](https://github.com/multiformats/cid). It computes a CID over a message's `body` and attaches it as an [unsigned commitment](../resources/unsigned-commitments.md) — a cryptographic commitment that has no `committer`, only a content-addressed identity.

The elegance comes from HyperBEAM's existing machinery, not from any new plumbing: `hb_cache:write/2` already links every commitment ID to the uncommitted root ID of the message it belongs to. Once an `~ipfs@1.0` commitment is attached, `hb_cache:read(CID, Opts)` finds the message — so content-addressed retrieval works via the standard `~lookup@1.0` device with no routing, path, or kernel changes.

The commitment is expressed as an HTTPSig HMAC-shaped item on the wire (`alg="ipfs@1.0/unsigned"`, `keyid="constant:ipfs"`, `signature` = base64url of the sha2-256 digest). This is **IPFS over HTTP Message Signatures**: a remote node decoding the response round-trips the commitment back to `commitment-device: ipfs@1.0` form without bespoke wire support.

This device covers the outer edges of the IPFS / IPLD spec intentionally: `sha2-256` multihashes, base32-lowercase multibase, and the `raw` (multicodec `0x55`) and `dag-cbor` (multicodec `0x71`) codecs.

`~ipfs@1.0` is **not** in `preloaded_devices` by default. A node operator opts in; see **Enabling** below.

## When to use it

- Serving content to IPFS clients via `GET /~lookup@1.0/read&target=<CID>`.
- Preloading a list of CIDs into a HyperBEAM node's cache by looping HEAD/GET lookups (the response's write-through pins locally).
- Pulling IPFS content into the Arweave / AO ecosystem: fetch a CID, apply an ANS-104 signed commitment using the node's wallet, POST it to a bundler.
- Acting as a verifying caching mirror of public IPFS data via the companion `hb_store_ipfs_gateway` store backend.

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
            <<"type">>              => <<"sha2-256-raw">>,
            <<"committed">>         => [<<"body">>],
            <<"signature">>         => <<"uU0nuZNNPgilLlLX2n2r-sSE7-N6U4DukIj3rOLvzek">>,
            <<"keyid">>             => <<"constant:ipfs">>
        }
    }
}
```

The commitment's `type` names the CID's construction — the multihash function and the CID's multicodec in one string, matching IPFS tooling. `sha2-256-raw` produces `bafk…` CIDs; `sha2-256-dag-cbor` produces `bafy…` CIDs. The `type` flows onto the wire as the `alg` parameter of the signature-input line (`alg="ipfs@1.0/sha2-256-raw"`), exactly the same way `dev_codec_ans104` surfaces `unsigned-sha256` and `dev_codec_httpsig` surfaces `hmac-sha256`.

The `signature` field holds the raw sha2-256 digest of the body (base64url), and the `keyid` is the universal constant `constant:ipfs`. Structurally this is an HTTPSig HMAC item — anyone can reverify without a secret — which lets the commitment ride over HTTP Message Signatures without any custom metadata parameters.

**Supported `Req` fields**

| Field | Default | Values |
| --- | --- | --- |
| `type` | `unsigned` | `unsigned` (caller convenience) or a native type directly |
| `hash-alg` | `sha2-256-raw` | `sha2-256-raw`, `sha2-256-dag-cbor` — only meaningful when `type=unsigned` |

`type=unsigned` is the caller-facing knob; it is translated into the native type named by `hash-alg` (or the default `sha2-256-raw`). Callers who already know the native type (`sha2-256-raw` / `sha2-256-dag-cbor`) may pass it directly. `signed` and other non-unsigned types delegate to `~httpsig@1.0` — the codec composes like `dev_codec_flat` / `dev_codec_json` for those paths. Unknown types return `{error, {unsupported_type, _}}`. Messages can carry both an IPFS commitment and an ANS-104 / HTTPSig signed commitment simultaneously.

### `verify` — check a CID

Recompute the CID from `body` under the commitment's declared `type`, then confirm it is a key in the message's `commitments` map. Tampering with the body produces a different CID, which is not present — verification returns `{ok, false}`. Called implicitly by `hb_message:verify/2,3`.

### `committed` — list covered keys

`dev_message:committed/3` reads the commitment's own `committed` list. For `~ipfs@1.0` that list is always `[<<"body">>]`.

### `content_type` — MIME

`application/vnd.ipld.raw` for `type = sha2-256-raw`, `application/vnd.ipld.dag-cbor` for `type = sha2-256-dag-cbor`. Falls back to `application/vnd.ipld.raw` when unspecified.

### `to` / `from` — dag-cbor serialization

`~ipfs@1.0` is a full codec in the `hb_message:convert/3,4` pipeline:

```erlang
%% Encode a message as dag-cbor bytes:
CborBytes = hb_message:convert(Msg, <<"ipfs@1.0">>, Opts).

%% Decode dag-cbor bytes back into a HyperBEAM message:
Msg = hb_message:convert(CborBytes, <<"structured@1.0">>, <<"ipfs@1.0">>, Opts).
```

The pipeline is `TABM <-> ~structured@1.0 (native types) <-> IPLD intermediate <-> dag-cbor bytes`. Encoding is deterministic per [the dag-cbor spec](https://ipld.io/specs/codecs/dag-cbor/spec/): shortest-form integers, canonical length-first map ordering, 64-bit floats only, definite-length containers. Non-canonical inputs on the decode side are rejected with a specific reason:

| Decode rejection | Reason atom |
| --- | --- |
| Indefinite-length item | `indefinite_length_forbidden` |
| Half / single float | `half_float_forbidden`, `single_float_forbidden` |
| NaN / Infinity | `nan_or_infinity_forbidden` |
| Non-UTF-8 text string | `invalid_utf8` |
| Non-string map key | `non_string_map_key` |
| Out-of-order or duplicate map keys | `non_canonical_map_order` |
| Unsupported tag | `{unsupported_tag, N}` |
| Non-canonical integer encoding | `non_canonical_integer` |

The `priv` sub-map is stripped before encoding. Commitments pass through the codec boundary unchanged, matching every other HyperBEAM codec (json, flat, ans104). Atoms outside `{null, true, false}` cannot be represented in IPLD and are rejected with `{error, {dag_cbor_encode, {unsupported_atom, _}}}`.

### Composing `commit` with `to`

The natural end-to-end pipeline for "publish a HyperBEAM message over IPFS" is:

```erlang
Bytes = hb_message:convert(Msg, <<"ipfs@1.0">>, Opts),
Carrier = #{ <<"body">> => Bytes },
Committed = hb_message:commit(Carrier, Opts,
                 #{ <<"commitment-device">> => <<"ipfs@1.0">>,
                    <<"type">>              => <<"unsigned">>,
                    <<"hash-alg">>          => <<"sha2-256-dag-cbor">> }),
{ok, _} = hb_cache:write(Committed, Opts).
```

The CID produced by `commit` over the dag-cbor bytes matches exactly what `ipfs dag put --store-codec dag-cbor` would produce on the same logical message. `hb_cache:read(CID, Opts)` then returns the committed message from the local cache; if the CID is not local, the optional `hb_store_ipfs_gateway` backend fetches it from a configured HTTP gateway and verifies the bytes against the CID before admitting them.

## HTTP recipes

With a node configured as above, a user drives the three production flows entirely through standard AO-Core paths — no kernel edits, no custom route handlers.

### 1. Serve a CID

```bash
curl 'http://localhost:8734/~lookup@1.0/read&target=bafkreifzjut3te2nhyekklss27nh3k72ysco7y32koao5eei66wof36n5e'
# => hello world
```

The node resolves the CID through its store chain. On first hit, the `hb_store_ipfs_gateway` backend fetches from a configured HTTP gateway, verifies `sha256(body)` matches the CID's digest, wraps the body in a message with an `~ipfs@1.0` commitment, and writes it through the cache. On subsequent hits, the body is served from the local store.

### 2. Preload (en-masse pin)

Loop over your CIDs:

```bash
for CID in bafkreif… bafkreig… bafyreib… ; do
  curl -sI "http://localhost:8734/~lookup@1.0/read&target=$CID" > /dev/null
done
```

Each successful lookup pins the CID to the local store via the HTTP request-response write-through path.

### 3. Push IPFS content to Arweave

Chain a server-side ANS-104 commit onto the lookup. The node's `priv_wallet` does the signing:

```bash
curl 'http://localhost:8734/~lookup@1.0/read&target=<CID>/commit&type=signed&commitment-device=ans104@1.0'
```

The response carries the IPFS body plus an `ans104@1.0/rsa-pss-sha256` signed commitment in the `signature-input` header. That's a bundler-ready message: follow up with a `POST` to `/~arweave@2.9/tx` (or `/~bundler@1.0/tx`) with that signed message as the body, and the node will push it to Arweave, provided its wallet is topped up and `bundler_ans104` is configured.

## Programmatic end-to-end example (Erlang)

```erlang
%% 1. Stamp a blob with its CID.
Msg       = #{ <<"body">> => <<"hello world">> },
Committed = hb_message:commit(Msg, Opts,
                #{ <<"commitment-device">> => <<"ipfs@1.0">>,
                   <<"type">> => <<"unsigned">> }),
%% 2. Write it. Cache auto-links the CID to the uncommitted ID.
{ok, _UncommittedID} = hb_cache:write(Committed, Opts),
%% 3. Read by CID. Works because of the link established in step 2.
{ok, Recovered} =
    hb_cache:read(
        <<"bafkreifzjut3te2nhyekklss27nh3k72ysco7y32koao5eei66wof36n5e">>,
        Opts
    ),
<<"hello world">> =
    hb_cache:ensure_loaded(maps:get(<<"body">>, Recovered), Opts).
```

## What's next

A link-aware mapping through `hb_link`, so that IPLD CID links (dag-cbor tag 42) integrate with HyperBEAM's lazy-loaded link primitive and nested messages can be addressed as first-class IPLD sub-blocks. For now, CID links decode to plain CID strings and arbitrary Erlang atoms throw on encode.

## Non-goals

- CIDv0 (legacy base58 dag-pb CIDs, `Qm…`).
- `dag-pb`, UnixFS, file chunking.
- `dag-json` (trivial to add on top of the existing encoder; out of scope for v1).
- Hash algorithms other than `sha2-256`.
- Multibases other than base32-lower on encode (decode accepts `B`/`f` defensively).
- Bytes / text distinction from `structured@1.0`: both flatten to plain binaries.
- IPLD-native links: tag-42 decodes to a plain CID string; it does not wire into `hb_link` or `hb_cache` lazy resolution.
- IPNS, bitswap, pubsub, libp2p.
- IPLD Schemas, Selectors, or path resolution into sub-blocks.

## Related source

- [`dev_codec_ipfs.erl`](../resources/source-code/dev_codec_ipfs.md) — device entry points (`commit`, `verify`, `to`, `from`, `content_type`, `info`).
- [`dev_codec_ipfs_cid.erl`](../resources/source-code/dev_codec_ipfs_cid.md) — varint, multihash, multibase, CIDv1.
- [`dev_codec_ipfs_cbor.erl`](../resources/source-code/dev_codec_ipfs_cbor.md) — deterministic dag-cbor encoder/decoder.
- [`hb_store_ipfs_gateway.erl`](../resources/source-code/hb_store_ipfs_gateway.md) — read-only gateway store.
- [`dev_codec_ipfs_test.erl`](../resources/source-code/dev_codec_ipfs_test.md) — integration tests including the cache-linkage proof and the full `to`/`from` roundtrip.
