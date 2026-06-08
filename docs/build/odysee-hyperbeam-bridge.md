# Odysee HyperBEAM bridge

The first useful bridge is read-only: expose existing Odysee/LBRY data as
AO-Core messages through HyperBEAM, while still using the current Odysee
serving path for live content policy. The bridge should make legacy content
inspectable and playable without bulk-migrating the catalog or replacing the
product stack.

This page is both the implementation plan and the current bridge contract.
Route names are intentionally left open for the first prototype, but the
implementation must provide two surfaces:

* an AO-Core message view for metadata, claims, descriptors, comments, and
  verification state;
* a playback surface that can feed the existing Odysee browser player while
  deeper descriptor/blob verification is added.

## Constraints

The bridge starts from current Odysee services, then adds lower-level proof
paths only when they are needed.

* Start with reads only.
* Use existing front-door services before direct database or bucket access.
* Preserve raw legacy bytes beside decoded fields.
* Preserve legacy IDs, hashes, signatures, signing inputs, and source
  encodings.
* Treat source-format devices as compatibility layers, not as a replacement
  Odysee data model.
* Do not flatten claims into ANS-104 tags. Claims can exceed practical tag
  sizes; keep source-format bodies and TABM/structured AO messages.
* Keep reflector/blob-serving policy controls in front of live serving until a
  signed decentralized policy format exists.

## HyperBEAM fit

HyperBEAM resolves AO-Core messages by finding the base message's
`<<"device">>` key, loading that device, and resolving the requested path key
through the device's exported functions. If no device is set, the default is
`message@1.0`.

For this bridge, that means each Odysee source format should be modeled as a
normal HyperBEAM device:

* source modules use the `dev_<name>.erl` namespace;
* public keys are exported functions, usually
  `key(Base, Req, Opts) -> {ok, Result} | {error, Reason}`;
* `info/1` should restrict the public API with `#{ exports => [...] }`;
* runtime devices must be packaged through the Forge and loaded as generated
  `_hb_device_*` BEAM archive modules;
* decoded data should round-trip through TABM and `structured@1.0`;
* outbound reads should use existing HyperBEAM HTTP paths, such as `hb_http`
  and the patterns already used by `relay@1.0`.

The bridge should be written as application-layer devices. It should not change
the AO-Core resolver, device loader, or message semantics.

## Odysee integration boundaries

Use these services as the first integration points.

| Data | First adapter | Later adapter | Notes |
| --- | --- | --- | --- |
| Claims and channels | `POST https://api.na-backend.odysee.com/api/v1/proxy` | chainquery, direct hub, or raw lbrycrd proof path | Use SDK proxy methods such as `resolve`, `claim_search`, `get`, `transaction_show`, `status`, and `version`. |
| Stream descriptors | reflector/blobcache `GET /blob?hash=<sd_hash>` | player path or direct object-store lookup | Descriptor bytes are proof inputs, so prefer blobcache/reflector before player output. |
| Blobs | reflector/blobcache `GET /blob?hash=<blob_hash>` | direct Wasabi/S3/MinIO/Globalstake lookup | Keep current reflector policy behavior in the live read path. |
| Comments | `POST https://comments.odysee.com/api/v2` | read-only MySQL replica | Use `comment.List`, `comment.ByID`, and commentron verification helpers first. |
| Blocklists | existing Odysee serving path | signed policy device | Do not bypass current policy for live serving in v1. |

## Bridge device set

The first implementation should be a small codec set. Each device preserves the
source format and exposes decoded fields as AO-Core messages.

| Device | Purpose | First milestone |
| --- | --- | --- |
| `~lbry-stream-descriptor@1.0` | Parse descriptor JSON, verify encrypted blobs, decrypt, and reconstruct media bytes. | Implemented proof device. |
| `~lbry-claim@1.0` | Preserve raw SDK proxy resolve JSON, claim ID, name, value, canonical URL, and current resolved state. | Implemented playback-stage adapter. |
| `~lbry-stream@1.0` | Represent stream/content claims, stream metadata, `source.sd_hash`, and player-compatible playback URLs. | Implemented playback-stage adapter. |
| `~lbry-channel@1.0` | Represent channel identity, public key, signature context, and AO-Core committer mapping. | Implemented read-only identity adapter. |
| `~odysee-comment@1.0` | Represent commentron comments, signature payloads, moderation metadata, and verification inputs. | Implemented read-only Commentron adapter; signature verification awaits signed vectors. |

## Current playback slice

The current minimum end-to-end target is one Odysee frontend video resolving to
a HyperBEAM-derived playback contract. The default path still returns the
existing Odysee player/CDN URL, while byte mode returns a HyperBEAM media URL so
the browser can request media ranges from `~lbry-stream@1.0/media`. The media
endpoint prefers descriptor/blob reads when descriptor or blob settings are
supplied. If no descriptor/blob settings are present, it falls back to a capped
player-media proxy so the browser still talks to HyperBEAM for playable ranges
while the lower-level blob path is made reliable.

Implemented devices:

| Device | Key | Behavior |
| --- | --- | --- |
| `~lbry-stream-descriptor@1.0` | `media` | Fetches or decodes a stream descriptor, verifies and decrypts only the blobs needed for the requested plaintext byte range, and returns browser-compatible `HEAD`/`Range` responses. |
| `~lbry-claim@1.0` | `resolve` | Accepts an Odysee URL, LBRY URI, claim fixture, or SDK proxy JSON result; calls the SDK proxy when needed; returns a normalized claim message while preserving raw JSON in `body`. |
| `~lbry-stream@1.0` | `stream` / `from-claim` | Derives stream metadata from the claim, including `media-type`, `sd-hash`, source fields, dimensions, duration, thumbnail, and generated player/download URLs. |
| `~lbry-stream@1.0` | `playback` | Returns a JSON body with Odysee-compatible `streaming_url`/`download_url`, or a `307` redirect when `redirect=true` or `format=redirect`. With `mode=bytes`, `mode=media`, `mode=hyperbeam`, or `bytes=true`, the returned URL points to the local `media` endpoint. |
| `~lbry-stream@1.0` | `media` | Resolves the claim, serves `HEAD` metadata, and serves capped `Range` responses. Descriptor/blob settings route through the descriptor device; otherwise the device proxies bounded ranges from the current player media URL. |
| `~lbry-channel@1.0` | `channel` / `from-claim` | Normalizes direct channel claims, claim-device messages, or a stream claim's `signing_channel`; preserves public key fields and source claim context for later verification. |
| `~odysee-comment@1.0` | `list` / `by-id` / `normalize` | Normalizes `comment.List` and `comment.ByID` responses from supplied fixtures or the Commentron API; preserves comment signatures, signing timestamps, signed message hints, author channel IDs, parent IDs, and moderation metadata. |

For
`https://odysee.com/@veritasium:f/why-is-it-so-easy-to-disrupt-gps:3`, the
claim resolve result contains `video/mp4` source media, `sd_hash` prefix
`6ee8f7`, and claim ID `346c1fed0fbc2f0b3ecc8bf3915aa8aaa029c169`. The stream
device therefore generates:

```text
https://player.odycdn.com/api/v3/streams/free/why-is-it-so-easy-to-disrupt-gps/346c1fed0fbc2f0b3ecc8bf3915aa8aaa029c169/6ee8f7.mp4
```

That is the default playback contract for the first playable video. The
byte-mode JSON contract for the same stream is:

```text
http://127.0.0.1:8734/~lbry-stream@1.0/playback?mode=bytes&media-base-url=http%3A%2F%2F127.0.0.1%3A8734&url=lbry%3A%2F%2F%40veritasium%23f%2Fwhy-is-it-so-easy-to-disrupt-gps%233
```

That returns a JSON body whose `streaming_url` is:

```text
http://127.0.0.1:8734/~lbry-stream@1.0/media?claim-name=why-is-it-so-easy-to-disrupt-gps&claim-id=346c1fed0fbc2f0b3ecc8bf3915aa8aaa029c169
```

For clients that want an HTTP redirect instead of JSON, add `redirect=true` or
`format=redirect`.

The media endpoint returns `accept-ranges: bytes` on `HEAD`. On `GET`, explicit
`Range: bytes=start-end` requests return `206` and `content-range`. Open-ended
requests such as `bytes=0-` are capped to `range-chunk-size` bytes, defaulting
to 1 MiB, so a browser can continue fetching incrementally. The descriptor path
only allows no-range `GET` for small media, defaulting to 8 MiB or below, unless
`allow-full=true` is supplied. The player-proxy fallback converts no-range
requests into capped range requests. Playback and media endpoints return
permissive CORS headers for `GET`, `HEAD`, and `OPTIONS`, and expose range
headers needed by browser media elements.

The Odysee frontend integration is opt-in through `HYPERBEAM_PLAYBACK_URL`.
When set to a HyperBEAM playback endpoint such as
`http://127.0.0.1:8734/~lbry-stream@1.0/playback`, the browser file-info fetch
asks HyperBEAM for JSON and stores the returned `streaming_url` as
`fileInfo.streaming_url`. The existing video viewer already consumes
`fileInfo.streaming_url`, so normal player rendering works without changing the
player component. Server-side stream routes use the same playback endpoint for
metadata/route-generated stream URLs. If the environment variable is unset, the
HyperBEAM request fails, or the content requires an access key, the frontend
keeps the existing Odysee SDK playback path.

Byte mode can carry descriptor/blob runtime settings in the playback URL. The
stream device preserves those settings when building the `media` URL, so a
frontend value can point at public reflectors, a local mirror, or tuned cache
behavior without modifying the player:

| Setting | Meaning |
| --- | --- |
| `blob-base-url` / `reflector-url` | One reflector/blobcache base URL. The descriptor device requests `/blob?hash=<hash>`. |
| `blob-base-urls` / `reflector-urls` | Comma-separated or list form of multiple reflector/blobcache bases. Default order is `blobcache-eu.odycdn.com`, `blobcache-us.odycdn.com`, then `blobcache.lbry.com`. |
| `blob-url-template` / `blob-url-templates` | Explicit blob URL template containing `{hash}` for nonstandard mirrors. |
| `blob-dir` / `blob-dirs` / `blob-directory` | Local directory of encrypted blob files named by their SHA-384 hash. Useful for deterministic tests and private mirrors. |
| `cache-blobs` / `blob-cache` | Enable or disable encrypted blob cache reads/writes. Defaults to enabled. |
| `plain-cache-blobs` | Enable or disable decrypted plaintext blob cache reads/writes. Defaults to enabled. |
| `blob-connect-timeout`, `blob-recv-timeout`, `blob-checkout-timeout` | Per-request reflector HTTP timeout overrides. |

To test the full HyperBEAM byte path from the frontend, set:

```text
HYPERBEAM_PLAYBACK_URL=http://127.0.0.1:8734/~lbry-stream@1.0/playback?mode=bytes
```

For deterministic local byte-path testing, populate a directory with descriptor
and encrypted blob files named by their hash, then include that directory in the
frontend endpoint:

```text
HYPERBEAM_PLAYBACK_URL=http://127.0.0.1:8734/~lbry-stream@1.0/playback?mode=bytes&blob-dir=/absolute/path/to/lbry-blobs
```

The current implementation is validated against fixture descriptors, supplied
blob maps, local blob directories, encrypted blob cache reuse, plaintext blob
cache reuse, CORS preflight, and browser range forms. Live byte-serving still
depends on the configured reflector/blobcache being reachable from the running
node. In this environment, resolving the Veritasium claim succeeds, but
descriptor/blob fetches from the public blobcache timed out; that is an
integration/network blocker, not a descriptor parsing or range serving failure.
The PR therefore keeps the player-proxy fallback enabled by default for the
minimum frontend playback path. Set `player-proxy=false` when a deployment
should fail instead of using the current player media URL as the upstream.

## Current channel/comment slice

The channel and comment devices are read-only adapters. They are meant to make
the data inspectable as AO-Core messages before any moderation or write path is
added.

`~lbry-channel@1.0` accepts:

* a direct channel claim;
* a normalized `~lbry-claim@1.0` message containing a channel claim;
* a stream claim with `signing_channel`.

It returns channel ID/name, canonical URL, title, description, thumbnail,
public key, public key ID, tags, and the containing source claim when the
channel came from a signed stream.

`~odysee-comment@1.0` accepts supplied `comment.List` / `comment.ByID` JSON, or
fetches from `POST https://comments.odysee.com/api/v2?m=<method>`. It exposes
normalized comments with comment ID, claim ID, parent ID, channel ID/name/URL,
comment text, timestamps, signature, `signing_ts`, pin/reply/support fields,
and moderation fields. When a comment row has a signature and comment text, the
device records `signed-field=comment` and `signed-message=<comment text>`, but
marks verification as `not-verified` until real signed vectors are selected and
validated against the channel public key.

The Odysee frontend routes these read-only calls through HyperBEAM when
`HYPERBEAM_BASE_URL` is set, for example:

```text
HYPERBEAM_BASE_URL=http://127.0.0.1:8734
```

On the Veritasium video page, that produces:

```text
POST http://127.0.0.1:8734/~lbry-channel@1.0/channel
POST http://127.0.0.1:8734/~odysee-comment@1.0/list
```

If HyperBEAM is unset, unavailable, or returns a non-OK response, the frontend
falls back to the existing Odysee channel/comment data already present in the
claim or Commentron API response.

## Milestone 1: stream descriptor

Build `~lbry-stream-descriptor@1.0` first. It gives the bridge an objective
proof loop: descriptor bytes lead to encrypted blobs, encrypted blobs verify by
hash, decrypted blobs reassemble to media bytes, and the reconstructed file can
be compared with the current player path.

### Inputs

The device should support two input forms:

* raw descriptor JSON bytes in the message body;
* an `sd_hash` plus a configured reflector/blobcache base URL.

The raw JSON bytes must be retained in the returned message even when the
descriptor is also decoded.

### Public keys

Use this minimum public API:

| Key | Behavior |
| --- | --- |
| `decode` | Decode descriptor JSON into a structured AO-Core message while preserving the original bytes. |
| `fetch` | Fetch descriptor bytes from `sd_hash` through the configured blob front door, then decode. |
| `verify` | Validate descriptor shape and verify encrypted blob hashes for any supplied or fetched blobs. |
| `reconstruct` | Verify, decrypt, and concatenate blobs into original media bytes. |
| `media` | Serve the reconstructed media through `HEAD` and byte-range `GET` responses without decrypting unnecessary trailing blobs. |

These keys are device keys, not final public HTTP route names. The final route
shape remains open.

### Descriptor fields

The decoded descriptor message must preserve:

* `stream_type`;
* `stream_name`;
* `suggested_file_name`;
* `key`;
* `stream_hash`;
* `sd_hash`;
* ordered blob entries with `blob_num`, encrypted length, IV, and SHA-384 blob
  hash;
* the final zero-length terminator entry.

The device should reject descriptors with missing required fields, duplicate
blob numbers, out-of-order non-terminator blobs, bad hex fields, invalid IV
lengths, or a missing terminator.

### Blob verification and reconstruction

The first proof path is:

1. Fetch the descriptor blob by `sd_hash`.
2. Decode and validate descriptor structure.
3. Fetch each non-terminator encrypted blob by its descriptor hash.
4. Verify `crypto:hash(sha384, EncryptedBlob)` matches the descriptor hash.
5. Decrypt each encrypted blob with AES-128-CBC using the descriptor `key` and
   the blob IV.
6. Remove PKCS7 padding from each decrypted blob.
7. Concatenate the decrypted blobs in `blob_num` order.
8. Return both the AO-Core descriptor/reconstruction message and the playable
   media bytes through separate bridge surfaces.

Do not normalize away the encrypted bytes, hash inputs, IVs, or stream key.
Those are compatibility and verification inputs.

### Acceptance

Milestone 1 is complete when a small corpus of real Odysee streams can be
resolved to descriptors, reconstructed, and compared with the current player
path.

Required samples:

* normal channel-signed video;
* anonymous stream;
* old descriptor ordering or old stream metadata shape;
* large multi-blob video.

Required tests:

* descriptor JSON decodes while preserving the original bytes;
* descriptor message round-trips through TABM and `structured@1.0`;
* descriptor hash and every encrypted blob hash match source bytes;
* malformed descriptors fail with explicit errors;
* reconstructed media bytes match the current player path for the sample set.

## Milestone 2: claims and streams

After descriptor reconstruction works, add claim resolution around the current
Odysee SDK proxy.

`~lbry-claim@1.0` should preserve:

* `claim_id`, name, transaction/outpoint context, and block height when
  available;
* raw claim value bytes;
* raw script shape for `OP_CLAIM_NAME` and `OP_UPDATE_CLAIM`;
* detected value encoding, including historical JSON and protobuf layouts;
* protobuf `Claim` shared metadata and typed `oneof` value;
* signature placement and signing metadata;
* current resolved state from trusted Odysee or hub reads.

`~lbry-stream@1.0` should derive from the claim envelope and preserve:

* title, description, media type, tags, author, license, license URL, and
  release time;
* `Stream.source` fields, including source hash, source name, size, media type,
  URL when present, and `sd_hash`;
* channel signature relationship or anonymous stream state.

The proof-oriented end-to-end path is:

```text
Odysee URL or claim ID
-> SDK proxy resolve
-> lbry claim message
-> lbry stream message
-> source.sd_hash
-> stream descriptor message
-> verified reconstructed bytes
```

## Milestone 3: channels and comments

Add `~lbry-channel@1.0` before comment verification. It should preserve channel
claim ID, channel public key, protobuf metadata, `@`-prefixed name form,
canonical Odysee suffix form, and legacy `SECP256k1` signature context. The
read-only identity adapter is implemented.

Then add `~odysee-comment@1.0` from commentron API rows. It should preserve
comment ID, parent ID, claim ID, channel ID, body, timestamps, signature,
signing timestamp, and moderation fields. Policy and moderation state should be
metadata, not part of the signed comment body. The read-only Commentron adapter
is implemented.

Comment verification should use the channel public key from the LBRY channel
claim and real comment rows as test vectors. This remains pending.

## Open decisions

These are intentionally not locked for the first doc:

* exact public route names for AO message views and playable byte streams;
* first signed policy artifact format;
* when product events or a dedicated `hb.api.odysee.com` coordinator enter the
  bridge;
* when raw lbrycrd/ClaimTrie proofs replace trusted Odysee/hub reads.

## Development order

1. Select 10-20 real sample IDs covering the required stream, claim, and comment
   cases.
2. Implement and test `~lbry-stream-descriptor@1.0`. Done for the local proof
   device and unit fixtures.
3. Implement SDK proxy claim resolution and the `~lbry-claim@1.0` envelope.
   Done for the playback-stage adapter.
4. Add `~lbry-stream@1.0` derived from the claim envelope. Done for the
   playback-stage adapter.
5. Add `~lbry-channel@1.0` identity mapping. Done for the read-only adapter.
6. Add `~odysee-comment@1.0` and comment signature tests. Done for read-only
   normalization; signed-vector verification remains.
7. Decide public route names only after the descriptor proof path works.
