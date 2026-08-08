#!/usr/bin/env bash
# Fetch the minimal Arweave block-index entries the ~arweave-block@2.9 proof of
# access tests need. Read-only GETs against public peers; nothing is written to
# the network.
#
# Usage: scripts/fetch-arweave-block-index-fixtures.sh [fixture-dir]
#
# `check_poa' maps a block's recall byte onto the block that wrote it, so a
# test needs the block index entries covering that offset and nothing else. For
# every recall byte a child block declares this fetches two entries: the block
# whose weave size first exceeds the offset, and its immediate predecessor.
# Both are required -- `bounds/3' reads the block's start offset off the
# preceding entry, so an index that begins at the covering block reports a
# start of zero.
#
# The entries are written in the `/block_index2' wire form, oldest first, which
# is the form `from-binary/3' ingests.
set -euo pipefail

OUT="${1:-test/fixtures/arweave}"
PEERS=(tip-1.arweave.xyz tip-2.arweave.xyz tip-3.arweave.xyz tip-4.arweave.xyz)
PORT=1984

# The blocks whose proofs of access the test suite validates. Each is the child
# of a pair the block tests already carry, and declares one or two recall bytes.
CHILDREN=(1975040 1974871 1974860)

MANIFEST="$OUT/MANIFEST-block-index.json"

# Every height read during a search is kept, so the searches for the five
# offsets share their upper levels rather than refetching them.
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

# The recall bytes a block declares, one per line.
recall_bytes() {
    python3 - "$OUT/block-${1}.json" <<'PY'
import json, sys
block = json.load(open(sys.argv[1]))
for key in ("recall_byte", "recall_byte2"):
    if block.get(key) is not None:
        print(int(block[key]))
PY
}

cached=1
[ -s "$MANIFEST" ] || cached=0
for child in "${CHILDREN[@]}"; do
    [ -s "$OUT/block-index-${child}.bin" ] || cached=0
done
if [ "$cached" = 1 ]; then
    echo "block index fixtures: cached"
    exit 0
fi

mkdir -p "$OUT"

# `covering/2' resolves a height per recall byte; the manifest records which
# byte each covering entry serves, which is what the assertions key off.
SERVES="$CACHE/serves.txt"
: > "$SERVES"

for child in "${CHILDREN[@]}"; do
    heights=()
    while IFS= read -r offset; do
        [ -n "$offset" ] || continue
        height=$(covering "$offset" "$child")
        echo "  ${child}: ${offset} -> height ${height}"
        printf '%s %s %s\n' "$child" "$height" "$offset" >> "$SERVES"
        heights+=( $(( height - 1 )) "$height" )
    done < <(recall_bytes "$child")

    # Sort and deduplicate: the entries go out in weave order, and two recall
    # bytes falling in the same or adjacent blocks share entries.
    deduped=$(printf '%s\n' "${heights[@]}" | sort -n | uniq)
    : > "$OUT/block-index-${child}.bin"
    while IFS= read -r height; do
        [ -n "$height" ] || continue
        cat "$(entry "$height")" >> "$OUT/block-index-${child}.bin"
        printf '%s %s\n' "$child" "$height" >> "$CACHE/written.txt"
    done <<< "$deduped"
    printf "  %s: %s entries, %s bytes\n" \
        "$child" \
        "$(printf '%s\n' "$deduped" | wc -l | tr -d ' ')" \
        "$(wc -c < "$OUT/block-index-${child}.bin" | tr -d ' ')"
done

# Record what each fixture holds, in the order the entries appear in it, so an
# assertion can name a height without reparsing the binary.
python3 - "$OUT" "$MANIFEST" "$CACHE/written.txt" "$SERVES" <<'PY'
import base64, json, os, sys

out, manifest, written, serves = sys.argv[1:5]


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


order = {}
for line in open(written):
    child, height = line.split()
    order.setdefault(int(child), []).append(int(height))

covering = {}
for line in open(serves):
    child, height, offset = line.split()
    covering.setdefault((int(child), int(height)), []).append(int(offset))

blocks = {}
for child, heights in sorted(order.items()):
    path = os.path.join(out, "block-index-%d.bin" % child)
    entries = decode(open(path, "rb").read())
    assert len(entries) == len(heights), path
    blocks[child] = [
        {
            "height": height,
            "indep_hash": indep_hash,
            "weave_size": weave_size,
            "tx_root": tx_root,
            "serves_recall_bytes": covering.get((child, height), []),
        }
        for height, (indep_hash, weave_size, tx_root) in zip(heights, entries)
    ]

json.dump(
    {
        "peers": "tip-1..4.arweave.xyz:1984",
        "wire_order": "oldest first, as GET /block_index2/<from>/<to> serves it",
        "blocks": blocks,
    },
    open(manifest, "w"),
    indent=2,
    sort_keys=True,
)
print("manifest: %d fixtures" % len(blocks))
PY
