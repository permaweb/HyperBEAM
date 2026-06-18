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
* Keep reflector/blob-serving policy controls in front of live serving until
  the signed policy artifact format has a production trust and distribution
  model.

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

The bridge should be written as application-layer devices and stores. It should
not change the AO-Core resolver, device loader, or message semantics. The only
core-level hook used by the first milestone is adding LBRY-shaped lengths to
the shared ID recognizer so store-native `GET /<id>` routing can see
descriptor/blob hashes, txids, and outpoints.

## Odysee integration boundaries

Use these services as the first integration points.

| Data | First adapter | Later adapter | Notes |
| --- | --- | --- | --- |
| Claims and channels | `POST https://api.na-backend.odysee.com/api/v1/proxy` | chainquery, direct hub, or raw lbrycrd proof path | Use SDK proxy methods such as `resolve`, `claim_search`, `get`, `transaction_show`, `status`, and `version`. |
| Stream descriptors | reflector/blobcache `GET /blob?hash=<sd_hash>` | player path or direct object-store lookup | Descriptor bytes are proof inputs, so prefer blobcache/reflector before player output. |
| Blobs | reflector/blobcache `GET /blob?hash=<blob_hash>` | direct Wasabi/S3/MinIO/Globalstake lookup | Keep current reflector policy behavior in the live read path. |
| Comments and comment reactions | `POST https://comments.odysee.com/api/v2` | read-only MySQL replica | Use `comment.List`, `comment.ByID`, `reaction.List`, and commentron verification helpers first. |
| File stats | `POST https://api.odysee.com/file/view_count` | read-only engagement replica | Use public view count reads; keep view recording direct. |
| File reactions | `POST https://api.odysee.com/reaction/list` | read-only engagement replica | Use unauthenticated `reaction/list` for public fire/slime counts; keep signed `reaction/react` direct. |
| Subscription counts | `POST https://api.odysee.com/subscription/sub_count` | read-only engagement replica | Use public follower count reads; keep signed follow/unfollow notifications direct. |
| Blocklists | existing Odysee serving path | `~odysee-policy@1.0` signed policy device | Do not bypass current policy for live serving in v1; the bridge can now evaluate signed policy artifacts before playback/media responses. |

## Bridge device set

The first implementation should be a small codec set. Each device preserves the
source format and exposes decoded fields as AO-Core messages.

| Device | Purpose | First milestone |
| --- | --- | --- |
| `~odysee@1.0` | Facade and operator/debug surface for normalized Odysee views. It can preserve public Odysee API surfaces, but it is not the canonical native LBRY source-commitment layer. | Implemented composition/source-getter endpoints; native LBRY commitments are carried by the `~lbry-*` codec family. |
| `~odysee-stream-descriptor@1.0` | Parse descriptor JSON, verify encrypted blobs, decrypt, and reconstruct media bytes. | Implemented proof device. |
| `~lbry-blob@1.0` | Commit encrypted LBRY blob bytes to their native SHA-384 blob ID. | Implemented source-native blob commitment device. |
| `~lbry-stream-descriptor@1.0` | Commit raw LBRY stream descriptor JSON to its `sd_hash`, stream hash, and blob store paths while preserving descriptor decode/media compatibility. | Implemented source-native descriptor commitment device. |
| `~lbry-transaction@1.0` | Commit raw LBRY transaction bytes to their display-order txid. | Implemented source-native transaction commitment device. |
| `~lbry-claim@1.0` | Commit verified raw LBRY transaction-output claim objects by immutable txid and output index. | Implemented source-native generic claim codec. |
| `~lbry-channel@1.0` | Commit verified channel claim objects, including public-key material used by downstream signature checks. | Implemented source-native channel codec. |
| `~lbry-stream@1.0` | Commit verified stream claim objects, including signed source descriptor references. | Implemented source-native stream codec. |
| `~lbry-channel-attestation@1.0` | Commit the channel-signed stream attestation over claim ID, signing channel, and `sd_hash`. | Implemented source-native stream/channel attestation codec. |
| `~odysee-claim@1.0` | Preserve raw SDK proxy resolve/search JSON, claim ID, name, value, canonical URL, and current resolved state. | Implemented playback/discovery adapter. |
| `~odysee-claim-proof@1.0` | Verify raw LBRY transaction output evidence for a claim by parsing transaction bytes, claim script prefixes, output index, and claim ID derivation. | Implemented transaction-output proof device. |
| `~odysee-stream@1.0` | Represent stream/content claims, stream metadata, `source.sd_hash`, and player-compatible playback URLs. | Implemented playback-stage adapter. |
| `~odysee-channel@1.0` | Represent channel identity, public key, signature context, and AO-Core committer mapping. | Implemented read-only identity adapter. |
| `~odysee-comment@1.0` | Represent commentron comments, signature payloads, moderation metadata, and verification inputs. | Implemented read-only Commentron adapter with secp256k1 signature verification. |
| `~odysee-reaction@1.0` | Represent Commentron reaction summaries for comments. | Implemented read-only reaction adapter. |
| `~odysee-file@1.0` | Represent Odysee internal API stats for stream/file claims. | Implemented read-only view count adapter. |
| `~odysee-file-reaction@1.0` | Represent Odysee internal API reaction summaries for stream/file claims. | Implemented unauthenticated read-only reaction adapter. |
| `~odysee-subscription@1.0` | Represent Odysee internal API follower counts for channel claims. | Implemented read-only subscription count adapter. |
| `~odysee-policy@1.0` | Represent signed content-policy artifacts for public Odysee/LBRY IDs and optional country constraints. | Implemented signed artifact verifier/evaluator; `~odysee-stream@1.0` can opt into policy enforcement before playback/media responses. |

`~lbry-claim-output@1.0` remains as the Odysee-side proof adapter around
`transaction_show` evidence. The canonical source-native claim-family codecs
are `~lbry-claim@1.0`, `~lbry-stream@1.0`, `~lbry-channel@1.0`, and
`~lbry-channel-attestation@1.0`; they verify raw LBRY transaction evidence by
immutable outpoint. Current-claim lookup by claim ID remains a locator concern
until full ClaimTrie inclusion proof lands.

## Store / codec split

The next bridge milestone separates sourcing from computation:

* `hb_store_odysee` is a read-only public Odysee source store. It supports stable
  paths such as `odysee/claim/<percent-encoded-url>`,
  `odysee/claim-id/<claim-id>`, `odysee/stream/<percent-encoded-url>`,
  `odysee/stream-id/<claim-id>`, `odysee/channel/<channel-claim-id>`,
  `odysee/channel-id/<channel-claim-id>`, `odysee/claim-proof/<txid>/<nout>`,
  `odysee/transaction/<txid>`, `odysee/descriptor/<sd-hash>`,
  `odysee/descriptor-id/<sd-hash>`, `odysee/stream-descriptor/<sd-hash>`,
  `odysee/comment/<comment-id>`, `odysee/comment-id/<comment-id>`,
  `odysee/blob/<blob-sha384>`, and `odysee/blob-id/<blob-sha384>`. It can sit
  below a local cache or behind `hb_store_remote_node`.
* `hb_store_lbry_stream_descriptor`, `hb_store_lbry_blob`,
  `hb_store_lbry_claim_output`, and `hb_store_lbry_transaction` expose native
  LBRY key forms for the source objects that can be verified locally today:
  bare descriptor/blob SHA-384 hashes, `lbry/descriptor/<sd_hash>`,
  `lbry/stream-descriptor/<sd_hash>`, bare txids, and immutable
  `txid:nout` outpoints for the claim family. Because descriptors and blobs
  are both 96-hex SHA-384 values, deployments that want direct `GET /<id>`
  descriptor reads should place `hb_store_lbry_stream_descriptor` before
  `hb_store_lbry_blob`; non-descriptor bytes fall through to the blob store.
  Claim/channel/stream output shape is selected by the
  `hb_store_lbry_claim_output` `kind` option.
* Store reads return normalized messages with source commitments: `~odysee@1.0`
  only for Odysee resolver/comment/product API surfaces,
  `~lbry-stream-descriptor@1.0`
  for stream descriptors, `~lbry-blob@1.0` for encrypted blobs,
  `~lbry-transaction@1.0` for raw transaction bytes, and the
  `~lbry-claim@1.0` / `~lbry-stream@1.0` / `~lbry-channel@1.0` /
  `~lbry-channel-attestation@1.0` family for raw transaction-output proof
  objects. A downstream node can select those commitment IDs from
  `commitments` and call `hb_message:verify/3` on them instead of trusting a
  proxy response. This matters when remote transport also adds ordinary HTTP
  signatures to the same message.
* LBRY-native commitments carry `signature`, `native-id`, and `native-id-type`
  fields. The signature is the source object's native identifier bytes, so the
  commitment ID is derived from the blob hash, descriptor `sd_hash`, txid, or
  outpoint rather than from a local transport signature.
* The current claim and channel commitments bind the raw Odysee resolver/search
  body plus stable public claim fields. The LBRY stream descriptor commitment
  binds the raw descriptor body, recomputes the descriptor `sd_hash`, and
  verifies it through `~odysee-stream-descriptor@1.0`. Blob commitments bind the
  encrypted blob bytes, blob SHA-384 hash, and RFC 9530 `content-digest`.
  Comment commitments bind the normalized Commentron row and verify the comment
  signature when the channel public key is present in the committed message.
* `~odysee-claim-proof@1.0` verifies raw transaction-output proof objects from
  `transaction_show` evidence: txid, output index, LBRY claim script prefix,
  claim name, and claim ID derivation. The LBRY claim-family codecs bind those
  verified source objects as native LBRY commitments. Full block/ClaimTrie
  inclusion proof is still the next proof tier.
* `~odysee@1.0/source` is an operator/debug source getter. It accepts public
  source IDs without auth tokens: blob SHA-384, descriptor `sd_hash` with
  `kind=descriptor`, transaction txid, claim-output outpoint (`txid:nout`), and
  explicit Odysee surface store paths. Bare 40-hex claim IDs are deliberately
  not treated as native store IDs: claim ID to transaction/outpoint resolution
  remains an index layer until a ClaimTrie/currentness proof exists. The source
  getter maps supported IDs onto store paths and returns the committed store
  object through `~cache@1.0/read`.
* HTTPSig response encoding now keeps unsafe binary fields out of HTTP headers.
  Binary values containing control bytes, plus large scalar values, are encoded
  in the message body instead. This is required for raw LBRY transaction/blob
  source surfaces to move through HyperBEAM without leaking or corrupting bytes
  in headers.
* `hb_store_remote_node` supports opt-in `verify-remote-read=true`. When enabled,
  it infers or accepts the expected commitment device for the requested key,
  verifies those commitment IDs with `hb_message:verify/3`, requires the
  commitment `native-id` to match the requested blob hash, descriptor hash,
  txid, or outpoint, and only then writes the remote value into the local cache.
  The `verify-remote-devices` option narrows ambiguous keys, such as
  descriptor/blob SHA-384 values or claim-family outpoints, to the expected
  source device. Verified reads can also be cached under their source
  commitment IDs.
* Normalized claim, stream, descriptor, channel, comment, blob, and claim-proof
  messages expose stable `*-store-path` fields. Stream messages point to their
  claim, descriptor, channel, and raw transaction-output proof paths when the
  source claim includes those IDs. Descriptor messages expose
  `descriptor-store-path` plus numbered `blob-store-paths`, and descriptor blob
  verification can read encrypted blobs through the configured HyperBEAM store
  stack before falling back to local/LBRY/blobcache sources.

## Current playback slice

The current minimum end-to-end target is one Odysee frontend video resolving to
a HyperBEAM-derived playback contract. The default path still returns the
existing Odysee player/CDN URL, while byte mode returns a HyperBEAM media URL so
the browser can request media ranges from `~odysee-stream@1.0/media`. The media
endpoint prefers descriptor/blob reads when descriptor or blob settings are
supplied. If no descriptor/blob settings are present, it falls back to a capped
player-media proxy so the browser still talks to HyperBEAM for playable ranges
while the lower-level blob path is made reliable.

Implemented devices:

| Device | Key | Behavior |
| --- | --- | --- |
| `~odysee@1.0` | `source` / `commit` / `verify` / `to-hint` | Provides facade/source-getter behavior and preserves Odysee API surface commitments where useful. Native LBRY source evidence is represented by the `~lbry-*` commitment devices, not by the facade. |
| `~odysee-stream-descriptor@1.0` | `media` | Fetches or decodes a stream descriptor, verifies and decrypts only the blobs needed for the requested plaintext byte range, and returns browser-compatible `HEAD`/`Range` responses. |
| `~odysee-claim@1.0` | `resolve` | Accepts an Odysee URL, LBRY URI, claim fixture, or SDK proxy JSON result; calls the SDK proxy when needed; returns a normalized claim message while preserving raw JSON in `body`. |
| `~odysee-claim@1.0` | `search` | Accepts `claim_search` params, supplied SDK search JSON, or a supplied result; calls the SDK proxy when needed; preserves the exact SDK result while exposing `items`, normalized `claims`, and `claim-ids`. |
| `~odysee-claim@1.0` | `transaction` | Calls or normalizes SDK proxy `transaction_show` JSON and exposes raw transaction hex plus parsed SDK transaction fields for proof devices. |
| `~odysee-claim-proof@1.0` | `decode` / `verify` | Parses raw LBRY transaction hex, verifies txid, output index, claim/update/support script shape, claim name, and claim ID derivation. |
| `~lbry-claim@1.0` / `~lbry-stream@1.0` / `~lbry-channel@1.0` / `~lbry-channel-attestation@1.0` | `commit` / `verify` / `to-hint` | Commit verified raw transaction-output proof surfaces using native LBRY claim, stream, channel, and attestation boundaries. |
| `~odysee-stream@1.0` | `stream` / `from-claim` | Derives stream metadata from the claim, including `media-type`, `sd-hash`, source fields, dimensions, duration, thumbnail, and generated player/download URLs. |
| `~odysee-stream@1.0` | `playback` | Returns a JSON body with Odysee-compatible `streaming_url`/`download_url`, or a `307` redirect when `redirect=true` or `format=redirect`. With `mode=bytes`, `mode=media`, `mode=hyperbeam`, or `bytes=true`, the returned URL points to the local `media` endpoint. |
| `~odysee-stream@1.0` | `media` | Resolves the claim, serves `HEAD` metadata, and serves capped `Range` responses. Descriptor/blob settings route through the descriptor device; otherwise the device proxies bounded ranges from the current player media URL. |
| `~odysee-stream@1.0` | `verified-stream` | Resolves the claim, checks the resolved signing-channel relationship, validates the descriptor hash against the claim `sd_hash`, and returns an explicit JSON attestation with `valid`, `signature-valid`, `channel-hash-valid`, `descriptor-valid`, `signed-sd-hash`, and nested evidence. The current stream-signature field is SDK/resolve-attested; raw transaction-output proof is represented separately by `~odysee-claim-proof@1.0`. |
| `~odysee-channel@1.0` | `channel` / `from-claim` | Normalizes direct channel claims, claim-device messages, or a stream claim's `signing_channel`; preserves public key fields and source claim context for later verification. |
| `~odysee-comment@1.0` | `list` / `by-id` / `normalize` / `verify-signature` / `verify-claim-signature` | Normalizes `comment.List` and `comment.ByID` responses from supplied fixtures or the Commentron API; preserves comment signatures, signing timestamps, signed message hints, author channel IDs, parent IDs, and moderation metadata; verifies Commentron-compatible signatures when a channel public key is supplied or resolvable. |
| `~odysee-reaction@1.0` | `list` / `normalize` | Normalizes `reaction.List` responses from supplied fixtures or the Commentron API; preserves exact `my_reactions` and `others_reactions` maps and exposes the involved comment IDs. |
| `~odysee-file@1.0` | `view-count` / `normalize` | Normalizes internal API `/file/view_count` responses from supplied fixtures or `POST https://api.odysee.com/file/view_count`; preserves exact ordered count arrays and exposes a `by-claim-id` map. |
| `~odysee-file-reaction@1.0` | `list` / `normalize` | Normalizes internal API `/reaction/list` responses from supplied fixtures or `POST https://api.odysee.com/reaction/list`; preserves exact `my_reactions` and `others_reactions` maps and exposes the involved claim IDs. |
| `~odysee-subscription@1.0` | `sub-count` / `normalize` | Normalizes internal API `/subscription/sub_count` responses from supplied fixtures or `POST https://api.odysee.com/subscription/sub_count`; preserves exact ordered count arrays and exposes a `by-claim-id` map. |

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
http://127.0.0.1:8734/~odysee-stream@1.0/playback?mode=bytes&media-base-url=http%3A%2F%2F127.0.0.1%3A8734&url=lbry%3A%2F%2F%40veritasium%23f%2Fwhy-is-it-so-easy-to-disrupt-gps%233
```

That returns a JSON body whose `streaming_url` is:

```text
http://127.0.0.1:8734/~odysee-stream@1.0/media?claim-name=why-is-it-so-easy-to-disrupt-gps&claim-id=346c1fed0fbc2f0b3ecc8bf3915aa8aaa029c169
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
`http://127.0.0.1:8734/~odysee-stream@1.0/playback`, the browser file-info fetch
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
| `lbrynet-api-url` / `lbrynet-api-urls` | Optional local LBRY SDK daemon JSON-RPC endpoint. When configured and a blob is missing from `blob-dir`, the descriptor device calls `blob_get`, then reads and verifies the fetched blob from `blob-dir`. |
| `lbrynet-stream-url` / `lbrynet-stream-base-url` | Optional local LBRY SDK media-server URL used to warm missing stream blobs into `blob-dir` before descriptor decryption. Use `lbrynet-stream-base-url=http://127.0.0.1:5280/stream` for the default SDK media server. |
| `cache-blobs` / `blob-cache` | Enable or disable encrypted blob cache reads/writes. Defaults to enabled. |
| `plain-cache-blobs` | Enable or disable decrypted plaintext blob cache reads/writes. Defaults to enabled. |
| `use-store-blobs` | Enable or disable descriptor blob reads through the configured HyperBEAM store stack. Defaults to enabled when a store is configured. |
| `blob-connect-timeout`, `blob-recv-timeout`, `blob-checkout-timeout` | Per-request reflector HTTP timeout overrides. |
| `lbrynet-timeout`, `lbrynet-connect-timeout`, `lbrynet-recv-timeout`, `lbrynet-checkout-timeout` | Per-request local daemon timeout overrides. `lbrynet-timeout` is passed to `blob_get` in seconds. |
| `player-proxy=false` | Disable fallback to the current Odysee player media URL. Descriptor/blob serving failures surface as media errors. |
| `mode=blob` / `mode=blob-native` / `blob-native=true` | Request strict descriptor/blob-native media serving. The generated media URL carries `blob-native=true` and does not fall back to the player proxy. |

To test the full HyperBEAM byte path from the frontend, set:

```text
HYPERBEAM_PLAYBACK_URL=http://127.0.0.1:8734/~odysee-stream@1.0/playback?mode=bytes
```

To test strict blob-native playback from the frontend, set:

```text
HYPERBEAM_PLAYBACK_URL=http://127.0.0.1:8734/~odysee-stream@1.0/playback?mode=blob
```

For deterministic local byte-path testing, populate a directory with descriptor
and encrypted blob files named by their hash, then include that directory in the
frontend endpoint:

```text
HYPERBEAM_PLAYBACK_URL=http://127.0.0.1:8734/~odysee-stream@1.0/playback?mode=blob&blob-dir=/absolute/path/to/lbry-blobs
```

For live local SDK-backed blob-native testing, run a LBRY SDK daemon and point
HyperBEAM at its blob directory and JSON-RPC API:

```text
HYPERBEAM_PLAYBACK_URL=http://127.0.0.1:8734/~odysee-stream@1.0/playback?mode=blob&blob-dir=/Users/<user>/Library/Application%20Support/LBRY/blobfiles&lbrynet-api-url=http://127.0.0.1:5279&lbrynet-stream-base-url=http://127.0.0.1:5280/stream&lbrynet-timeout=120
```

The current implementation is validated against fixture descriptors, supplied
blob maps, local blob directories, encrypted blob cache reuse, plaintext blob
cache reuse, store-backed encrypted blob reads, local daemon `blob_get` fetches,
SDK media-server blob warmups, CORS preflight, and browser range forms. Live
byte-serving still depends on the
configured reflector/blobcache or local daemon being reachable from the running
node. In this environment, resolving the Veritasium claim succeeds, but
descriptor/blob fetches from the public blobcache timed out. The PR therefore
keeps the player-proxy fallback enabled by default for the minimum frontend
playback path. Set
`player-proxy=false`, `blob-native=true`, or `mode=blob` when a deployment
should fail instead of using the current player media URL as the upstream.

## Current discovery slice

`~odysee-claim@1.0/search` wraps the SDK proxy `claim_search` method. It is a
read-only adapter: request fields become SDK search params, while control fields
such as `body`, `result`, `proxy-url`, and device routing fields are stripped
before proxying. The response preserves the exact SDK `result` for frontend
compatibility and also exposes AO-friendly `items`, normalized `claims`, and
`claim-ids`.

The Odysee frontend can route unauthenticated `claim_search` calls through this
device. Search calls that require wallet/user context, such as
`include_purchase_receipt` or `include_is_my_output`, intentionally remain on
the existing SDK/proxy path until authenticated HyperBEAM forwarding is added.

## Current channel/comment/reaction slice

The channel, comment, and reaction devices are read-only adapters. They are
meant to make the data inspectable as AO-Core messages before any moderation or
write path is added.

`~odysee-channel@1.0` accepts:

* a direct channel claim;
* a normalized `~odysee-claim@1.0` message containing a channel claim;
* a stream claim with `signing_channel`.

It returns channel ID/name, canonical URL, title, description, thumbnail,
public key, public key ID, tags, and the containing source claim when the
channel came from a signed stream. It also exposes `claim-store-path`,
`channel-store-path`, and `claim-proof-store-path` when the source claim has
transaction evidence.

`~odysee-comment@1.0` accepts supplied `comment.List` / `comment.ByID` JSON, or
fetches from `POST https://comments.odysee.com/api/v2?m=<method>`. It exposes
normalized comments with comment ID, claim ID, parent ID, channel ID/name/URL,
comment text, timestamps, signature, `signing_ts`, pin/reply/support fields,
moderation fields, and store paths for the comment, claim, and author channel.
When a comment row has a signature and comment text, the device records
`signed-field=comment` and `signed-message=<comment text>`. If the row or
request includes a channel public key, the device verifies the
Commentron digest `sha256(signing_ts || reverse_hex(channel_id) || data)` with
the compact secp256k1 ECDSA signature and marks the comment `valid` or
`invalid`. If the public key is not available, the comment stays
`not-verified` while still preserving all verification inputs. The explicit
`verify-signature` and `verify-claim-signature` routes can also resolve a
channel key from `channel-url`, `channel-name`, and `channel-id`.

`~odysee-reaction@1.0` accepts supplied `reaction.List` JSON, or fetches from
`POST https://comments.odysee.com/api/v2?m=reaction.List`. It preserves
`my_reactions` and `others_reactions` exactly for frontend compatibility and
adds `comment-ids` for AO-side indexing/debugging.

`~odysee-file@1.0` accepts supplied internal API `/file/view_count` JSON, or
fetches from `POST https://api.odysee.com/file/view_count`. It preserves the
ordered count array expected by the frontend and adds `by-claim-id` for AO-side
inspection. View recording (`file/view`) remains direct until the product event
contract is defined.

`~odysee-file-reaction@1.0` accepts supplied internal API `/reaction/list` JSON,
or fetches from `POST https://api.odysee.com/reaction/list`. It preserves
`my_reactions` and `others_reactions` exactly for frontend compatibility and
adds `claim-ids` for AO-side indexing/debugging. The frontend only uses this
HyperBEAM path when there is no auth token; authenticated reads and signed
`reaction/react` mutations remain direct until an authenticated HyperBEAM
forwarding contract exists.

`~odysee-subscription@1.0` accepts supplied internal API
`/subscription/sub_count` JSON, or fetches from
`POST https://api.odysee.com/subscription/sub_count`. It preserves the ordered
count array expected by the frontend and adds `by-claim-id` for AO-side
inspection. Follow/unfollow writes (`subscription/new` and
`subscription/delete`) remain direct.

The Odysee frontend routes these read-only calls through HyperBEAM when
`HYPERBEAM_BASE_URL` is set, for example:

```text
HYPERBEAM_BASE_URL=http://127.0.0.1:8734
```

On the Veritasium video page, that produces:

```text
POST http://127.0.0.1:8734/~odysee-channel@1.0/channel
POST http://127.0.0.1:8734/~odysee-comment@1.0/list
```

If HyperBEAM is unset, unavailable, or returns a non-OK response, the frontend
falls back to the existing Odysee channel/comment data already present in the
claim or Commentron API response.

## Milestone 1: stream descriptor

Build `~odysee-stream-descriptor@1.0` first. It gives the bridge an objective
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

`~odysee-claim@1.0` should preserve:

* `claim_id`, name, transaction/outpoint context, and block height when
  available;
* raw claim value bytes;
* raw script shape for `OP_CLAIM_NAME` and `OP_UPDATE_CLAIM`;
* detected value encoding, including historical JSON and protobuf layouts;
* protobuf `Claim` shared metadata and typed `oneof` value;
* signature placement and signing metadata;
* current resolved state from trusted Odysee or hub reads.

`~odysee-stream@1.0` should derive from the claim envelope and preserve:

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

Add `~odysee-channel@1.0` before comment verification. It should preserve channel
claim ID, channel public key, protobuf metadata, `@`-prefixed name form,
canonical Odysee suffix form, and legacy `SECP256k1` signature context. The
read-only identity adapter is implemented.

Then add `~odysee-comment@1.0` from commentron API rows. It should preserve
comment ID, parent ID, claim ID, channel ID, body, timestamps, signature,
signing timestamp, and moderation fields. Policy and moderation state should be
metadata, not part of the signed comment body. The read-only Commentron adapter
is implemented.

Comment verification uses the channel public key from the LBRY channel claim
or request payload and real Commentron digest rules as test vectors.

`~odysee-reaction@1.0` should preserve read-only `reaction.List` summaries for
comments. The read-only adapter is implemented; `reaction.React` remains a
direct signed user mutation.

`~odysee-file@1.0` should preserve read-only `/file/view_count` summaries for
stream/file claims. The read-only adapter is implemented; `file/view` remains a
direct product event.

`~odysee-file-reaction@1.0` should preserve read-only `/reaction/list`
summaries for stream/file claims. The unauthenticated read-only adapter is
implemented; authenticated reads and `reaction/react` remain direct.

`~odysee-subscription@1.0` should preserve read-only `/subscription/sub_count`
summaries for channel claims. The read-only adapter is implemented; follow and
unfollow notification writes remain direct.

## Open decisions

These are intentionally not locked for the first doc:

* exact public route names for AO message views and playable byte streams;
* production distribution and trust model for signed policy artifacts;
* when product events or a dedicated `hb.api.odysee.com` coordinator enter the
  bridge;
* when raw lbrycrd/ClaimTrie proofs replace trusted Odysee/hub reads.

## Development order

1. Select 10-20 real sample IDs covering the required stream, claim, and comment
   cases.
2. Implement and test `~odysee-stream-descriptor@1.0`. Done for the local proof
   device and unit fixtures.
3. Implement SDK proxy claim resolution and the `~odysee-claim@1.0` envelope.
   Done for the playback/discovery adapter.
4. Add `~odysee-stream@1.0` derived from the claim envelope. Done for the
   playback-stage adapter.
5. Add `~odysee-channel@1.0` identity mapping. Done for the read-only adapter.
6. Add `~odysee-comment@1.0` and comment signature tests. Done for read-only
   normalization and signed-vector verification.
7. Add `~odysee-reaction@1.0` for read-only comment reaction summaries. Done.
8. Add `~odysee-file@1.0` for read-only view counts. Done.
9. Add `~odysee-file-reaction@1.0` for unauthenticated video/file reaction
   summaries. Done.
10. Add `~odysee-subscription@1.0` for read-only follower counts. Done.
11. Add `~odysee@1.0` source commitments and `hb_store_odysee` read-store
    plumbing so public Odysee objects can be sourced by stores and verified via
    `hb_message:verify/3`. Claim URL, claim ID, stream URL, stream claim ID,
    channel ID aliases, descriptor hash aliases, comment ID aliases, encrypted
    blob hash aliases, raw transaction txid paths, native source getter paths,
    and raw transaction-output claim proof paths are implemented. Bare native
    `GET /<id>` reads are implemented for descriptor/blob SHA-384 values,
    transaction txids, and `txid:nout` outpoints; claim ID discovery remains an
    index/resolution concern.
12. Use two HyperBEAM nodes to prove the remote-store path: node A misses
    locally, asks node B, verifies the returned source commitment, then
    caches/serves the verified message. `hb_store_remote_node` now supports
    opt-in `verify-remote-read=true` and source commitment ID cache aliases.
13. Add `~odysee-policy@1.0` signed policy artifacts and optional
    `~odysee-stream@1.0` playback/media enforcement. Done for signed deny/allow
    evaluation over public claim/channel/blob/descriptor/tx/outpoint IDs and
    optional country constraints.

## June review alignment

The June 12 and June 17 reviews narrowed the immediate milestone to
store-native, source-committed reads for known LBRY/Odysee IDs. The current
implementation lines up with that milestone as follows:

| Review requirement | Current status |
| --- | --- |
| Source data must return as HyperBEAM messages with a `commitments` map, not proof-like JSON fields. | Implemented for Odysee surfaces and native LBRY blob, descriptor, transaction, claim, channel, stream, and attestation objects. |
| Commitment metadata must identify source-format devices. | Implemented through `~lbry-blob@1.0`, `~lbry-stream-descriptor@1.0`, `~lbry-transaction@1.0`, `~lbry-claim@1.0`, `~lbry-channel@1.0`, `~lbry-stream@1.0`, and `~lbry-channel-attestation@1.0`. |
| HTTP wire output should expose source-format commitment metadata through normal `Signature-Input`, not only JSON body fields. | Implemented for store-native and source-helper reads; tests assert direct `GET /<txid>`, `GET /<blob-sha384>`, and `GET /<descriptor-sd-hash>` responses include `signature-input` with source-format `alg` and `native-id` parameters. |
| Normal store reads should work as `GET /<id>` instead of only through an Odysee facade route. | Implemented for descriptor/blob SHA-384 IDs, transaction txids, and `txid:nout` outpoints. The Odysee `source` route remains only as an operator/debug adapter for explicit public source paths. |
| Node A must verify Node B's returned source commitments before caching. | Implemented with `hb_store_remote_node` `verify-remote-read=true`; the two-node demo starts Node A as the verifying cache and Node B as the source store. |
| Blob byte integrity should lean on standard `Content-Digest` for SHA-384 over encrypted blob bytes. | Implemented in native blob messages and committed under `~lbry-blob@1.0`; invalid digest fields fail blob verification. On the HTTP wire, the generic HTTPSig codec may also emit its normal body `content-digest`, while the LBRY source identity remains the `lbry-blob@1.0/sha-384` commitment metadata. |
| Claim/name/latest resolution should stay separate from the source-object proof path. | Preserved. Bare claim IDs remain index/resolution inputs, not native store IDs. Known txid/output objects are the verifiable source boundary. |
| Signed geoblock/DMCA policy artifacts should be represented as HyperBEAM messages instead of ad hoc product flags. | Implemented `~odysee-policy@1.0` signed artifacts. The device verifies the policy message commitment, matches only public identifiers and country codes, and can return `451` before playback/media fetches. |

The following are deliberately not claimed as complete by this milestone:

* full LBRY block inclusion, SPV, or ClaimTrie currentness proof;
* proof of completeness for channel-to-stream listings or search/index results;
* production trust, route, and operator-distribution model for signed policy
  artifacts;
* production deployment automation and router/WeaveSpace worker operations;
* deterministic, TEE-backed, or decentralized transcoding verification;
* browser-side replay of ancestry/currentness proofs.
