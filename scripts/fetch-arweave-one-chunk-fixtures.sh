#!/usr/bin/env bash
# Fetch a mainnet one-chunk block pair for the ~arweave-block@2.9 test suite.
# Read-only GETs against public peers; nothing is written to the network.
#
# Usage: scripts/fetch-arweave-one-chunk-fixtures.sh [child-height] [fixture-dir] [shape]
#
# `shape' is `one-chunk' (the default) or `any'. The one-chunk assertion exists
# so a refetch cannot silently produce a pair that no longer exercises the
# single-proof paths. `any' is for pairs fetched to exercise something else --
# the reset-crossing pair that pins the VDF chain check, for instance, which is
# necessarily a two-chunk block.
#
# A block whose solution came out of a single recall chunk declares
# `recall_byte2: null' and carries no second proof, so it is the only shape
# that exercises the one-proof paths through validation. They are a minority of
# blocks -- one in sixty around the height below -- so the height is picked by
# scanning rather than computed, and is recorded here rather than rediscovered.
#
# The pair has to be recent. Peers serve `/block_time_history' only while the
# block is in their cache, ~50 heights behind the tip, and the parent's history
# is what carries validation through to the proof of access. Re-running this at
# a later date will 404 on that endpoint for the default height; the fixtures
# under `test/fixtures/arweave' are frozen precisely so it never has to be run
# again. A refetch means scanning for a fresh one-chunk block near the tip and
# passing its height as the first argument.
set -euo pipefail

CHILD="${1:-1975090}"
OUT="${2:-test/fixtures/arweave}"
SHAPE="${3:-one-chunk}"
PARENT=$(( CHILD - 1 ))

PEERS=(tip-1.arweave.xyz tip-2.arweave.xyz tip-3.arweave.xyz tip-4.arweave.xyz)
PORT=1984

MANIFEST="$OUT/MANIFEST.json"
INDEX_MANIFEST="$OUT/MANIFEST-block-index.json"

# Every height read during the recall byte search is kept, so a rerun of the
# search does not refetch the levels it shares with the previous one.
CACHE=$(mktemp -d)
trap 'rm -rf "$CACHE"' EXIT

# Rotate peers across requests rather than leaning on the first one.
NEXT_PEER=0

# Fetch a URL from whichever peer answers first, starting from the next in the
# rotation.
fetch() {
    local path="$1" dest="$2" i peer
    for i in $(seq 0 $(( ${#PEERS[@]} - 1 ))); do
        peer="${PEERS[$(( (NEXT_PEER + i) % ${#PEERS[@]} ))]}"
        if curl -sf -m 30 "http://${peer}:${PORT}${path}" -o "$dest"; then
            [ -s "$dest" ] && {
                NEXT_PEER=$(( (NEXT_PEER + i + 1) % ${#PEERS[@]} ))
                return 0
            }
        fi
    done
    echo "FAILED: $path" >&2
    return 1
}

fetch_block() {
    local height="$1"
    if [ -s "$OUT/block-${height}.json" ] && [ -s "$OUT/block-${height}.bin" ]; then
        echo "  ${height}: cached"
        return 0
    fi
    fetch "/block/height/${height}"  "$OUT/block-${height}.json" || return 1
    fetch "/block2/height/${height}" "$OUT/block-${height}.bin"  || return 1
    printf "  %s: json=%s bin=%s\n" \
        "$height" \
        "$(wc -c < "$OUT/block-${height}.json" | tr -d ' ')" \
        "$(wc -c < "$OUT/block-${height}.bin"  | tr -d ' ')"
}

# Fetch every transaction body referenced by a block. Block responses carry TX
# ids only; the bodies are needed for verify_weave_size / verify_tx_root /
# verify_block_txs.
fetch_block_txs() {
    local height="$1"
    local txdir="$OUT/txs-${height}"
    mkdir -p "$txdir"
    local n=0
    while read -r txid; do
        [ -n "$txid" ] || continue
        [ -s "$txdir/${txid}.json" ] && { n=$((n+1)); continue; }
        fetch "/tx/${txid}" "$txdir/${txid}.json" && n=$((n+1)) || true
    done < <(python3 -c "
import json,sys
print('\n'.join(json.load(open('$OUT/block-${height}.json'))['txs']))
")
    echo "  ${height}: ${n} txs"
}

# A field of a frozen block, read out of its JSON form.
field() {
    python3 - "$OUT/block-${1}.json" "$2" <<'PY'
import json, sys
print(json.load(open(sys.argv[1]))[sys.argv[2]])
PY
}

# Fetch the parent's block-time history. The endpoint is keyed by independent
# hash; the fixture is named by height, as the block fixtures are.
fetch_block_time_history() {
    local height="$1"
    local dest="$OUT/block-time-history-${height}.bin"
    if [ -s "$dest" ]; then
        echo "  ${height}: cached"
        return 0
    fi
    fetch "/block_time_history/$(field "$height" indep_hash)" "$dest" || return 1
    printf "  %s: %s bytes\n" "$height" "$(wc -c < "$dest" | tr -d ' ')"
}

# The path of the one-entry wire binary for a height, fetching it if the search
# has not read that height already.
entry() {
    local height="$1"
    local path="$CACHE/entry-${height}.bin"
    if [ ! -s "$path" ]; then
        fetch "/block_index2/${height}/${height}" "$path" >&2
        # Keep the request rate polite: the searches are latency bound anyway.
        sleep 0.2
    fi
    printf '%s' "$path"
}

# The weave size an entry records, read out of its variable-width wire form.
weave_size() {
    python3 - "$(entry "$1")" <<'PY'
import sys
data = open(sys.argv[1], "rb").read()
size = int.from_bytes(data[48:50], "big")
print(int.from_bytes(data[50:50 + size], "big"))
PY
}

# The lowest height whose weave size exceeds `offset': the block that wrote the
# byte at that offset. Weave sizes are non-decreasing in height, so this is a
# binary search. `hi' is known to satisfy the predicate.
covering() {
    local offset="$1" lo=0 hi="$2" mid size
    while [ "$lo" -lt "$hi" ]; do
        mid=$(( lo + ((hi - lo) / 2) ))
        # Read the probe into a variable rather than testing it inline: a
        # failed fetch inside `[ ]' would read as a false predicate and walk
        # the search to the wrong height instead of stopping.
        size=$(weave_size "$mid")
        if [ "$size" -gt "$offset" ]; then
            hi="$mid"
        else
            lo=$(( mid + 1 ))
        fi
    done
    printf '%s' "$lo"
}

# The recall bytes a block declares, one per line. A one-chunk block declares
# only `recall_byte', which is what makes its index slice two entries rather
# than four.
recall_bytes() {
    python3 - "$OUT/block-${1}.json" <<'PY'
import json, sys
block = json.load(open(sys.argv[1]))
for key in ("recall_byte", "recall_byte2"):
    if block.get(key) is not None:
        print(int(block[key]))
PY
}

mkdir -p "$OUT"

echo "fetching the pair {${PARENT}, ${CHILD}} into ${OUT}"
fetch_block "$PARENT"
fetch_block "$CHILD"

# The pair is only a pair if the peers agree it is one, so check the link
# before anything downstream is fetched against it.
python3 - "$OUT/block-${PARENT}.json" "$OUT/block-${CHILD}.json" "$SHAPE" <<'PY'
import json, sys
parent, child = (json.load(open(path)) for path in sys.argv[1:3])
shape = sys.argv[3]
assert child["previous_block"] == parent["indep_hash"], "pair is not linked"
assert child["height"] == parent["height"] + 1, "pair is not consecutive"
if shape == "one-chunk":
    assert child.get("recall_byte2") is None, "child is not a one-chunk block"
    assert child.get("chunk2_hash") is None, "child carries a second chunk hash"
    print("  linked, and the child is one-chunk")
else:
    print(f"  linked; child shape not constrained ({shape})")
PY

echo "fetching transaction bodies"
fetch_block_txs "$CHILD"

echo "fetching the parent's block-time history"
fetch_block_time_history "$PARENT"

# `check_poa' maps the child's recall byte onto the block that wrote it, so the
# test needs the block index entries covering that offset and nothing else: the
# block whose weave size first exceeds it, and its immediate predecessor. Both
# are required -- `bounds/3' reads a block's start offset off the preceding
# entry, so an index that begins at the covering block reports a start of zero.
#
# The entries go out in the `/block_index2' wire form, oldest first, which is
# the form `from-binary/3' ingests.
SERVES="$CACHE/serves.txt"
WRITTEN="$CACHE/written.txt"
: > "$SERVES"
: > "$WRITTEN"

if [ -s "$OUT/block-index-${CHILD}.bin" ]; then
    echo "block index: cached"
else
    echo "resolving the recall byte to its block index entries"
    heights=()
    while IFS= read -r offset; do
        [ -n "$offset" ] || continue
        height=$(covering "$offset" "$CHILD")
        echo "  ${CHILD}: ${offset} -> height ${height}"
        printf '%s %s %s\n' "$CHILD" "$height" "$offset" >> "$SERVES"
        heights+=( $(( height - 1 )) "$height" )
    done < <(recall_bytes "$CHILD")

    deduped=$(printf '%s\n' "${heights[@]}" | sort -n | uniq)
    : > "$OUT/block-index-${CHILD}.bin"
    while IFS= read -r height; do
        [ -n "$height" ] || continue
        cat "$(entry "$height")" >> "$OUT/block-index-${CHILD}.bin"
        printf '%s %s\n' "$CHILD" "$height" >> "$WRITTEN"
    done <<< "$deduped"
    printf "  %s: %s entries, %s bytes\n" \
        "$CHILD" \
        "$(printf '%s\n' "$deduped" | wc -l | tr -d ' ')" \
        "$(wc -c < "$OUT/block-index-${CHILD}.bin" | tr -d ' ')"
fi

# Record what the index fixture holds, in the order the entries appear in it,
# so an assertion can name a height without reparsing the binary. The manifest
# is shared with `scripts/fetch-arweave-block-index-fixtures.sh', so this adds
# the child's entry rather than rewriting the file.
python3 - "$OUT" "$INDEX_MANIFEST" "$WRITTEN" "$SERVES" "$CHILD" <<'PY'
import base64, json, os, sys

out, manifest, written, serves, child = sys.argv[1:6]
child = int(child)


def b64url(value):
    return base64.urlsafe_b64encode(value).decode().rstrip("=")


def decode(data):
    entries, offset = [], 0
    while offset < len(data):
        indep_hash = data[offset:offset + 48]
        offset += 48
        width = int.from_bytes(data[offset:offset + 2], "big")
        offset += 2
        weave_size = int.from_bytes(data[offset:offset + width], "big")
        offset += width
        width = data[offset]
        offset += 1
        tx_root = data[offset:offset + width]
        offset += width
        entries.append((b64url(indep_hash), weave_size, b64url(tx_root)))
    return entries


document = {
    "peers": "tip-1..4.arweave.xyz:1984",
    "wire_order": "oldest first, as GET /block_index2/<from>/<to> serves it",
    "blocks": {},
}
if os.path.exists(manifest):
    document = json.load(open(manifest))

heights = [int(line.split()[1]) for line in open(written)]
if not heights:
    # The binary was already frozen, so the manifest entry it belongs with is
    # frozen too.
    print("manifest: unchanged")
    raise SystemExit

covering = {}
for line in open(serves):
    _, height, offset = line.split()
    covering.setdefault(int(height), []).append(int(offset))

path = os.path.join(out, "block-index-%d.bin" % child)
entries = decode(open(path, "rb").read())
assert len(entries) == len(heights), path

document["blocks"][str(child)] = [
    {
        "height": height,
        "indep_hash": indep_hash,
        "weave_size": weave_size,
        "tx_root": tx_root,
        "serves_recall_bytes": covering.get(height, []),
    }
    for height, (indep_hash, weave_size, tx_root) in zip(heights, entries)
]

json.dump(document, open(manifest, "w"), indent=2, sort_keys=True)
print("manifest: %d fixtures" % len(document["blocks"]))
PY

# Record the pair's provenance alongside the blocks already frozen. The
# manifest is curated, so only the missing heights are filled in.
python3 - "$MANIFEST" "$OUT" "$PARENT" "$CHILD" <<'PY'
import json, os, sys

manifest, out = sys.argv[1:3]
heights = [int(height) for height in sys.argv[3:5]]

document = {
    "fetched": None,
    "source": "Arweave mainnet via tip-1..4.arweave.xyz:1984 (read-only GET)",
    "blocks": {},
}
if os.path.exists(manifest):
    document = json.load(open(manifest))

blocks = document["blocks"]
added = 0
for height in heights:
    if str(height) in blocks:
        continue
    block = json.load(open(os.path.join(out, "block-%d.json" % height)))
    blocks[str(height)] = {
        "indep_hash": block["indep_hash"],
        "previous_block": block["previous_block"],
        "has_parent_fixture":
            os.path.exists(os.path.join(out, "block-%d.json" % (height - 1))),
        "packing_difficulty": block.get("packing_difficulty"),
        "replica_format": block.get("replica_format"),
        "two_chunk": block.get("recall_byte2") is not None,
        "txs": len(block.get("txs", [])),
        "double_signing_proof": bool(block.get("double_signing_proof")),
        "vdf_difficulty": block["nonce_limiter_info"]["vdf_difficulty"],
        "global_step_number":
            block["nonce_limiter_info"]["global_step_number"],
        # Upstream's JSON encoder emits the record's `steps' list under the key
        # `checkpoints' for backwards compatibility; there is no JSON `steps'
        # key. `last_step_checkpoints' is the separate 25 intra-step values.
        "steps": len(block["nonce_limiter_info"].get("checkpoints", []) or []),
        "last_step_checkpoints":
            len(block["nonce_limiter_info"].get("last_step_checkpoints", [])
                or []),
        "timestamp": block["timestamp"],
    }
    added += 1

json.dump(document, open(manifest, "w"), indent=2, sort_keys=True)
print("manifest: %d blocks, %d added" % (len(blocks), added))
PY
