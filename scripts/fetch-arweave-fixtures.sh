#!/usr/bin/env bash
# Fetch real mainnet Arweave block fixtures for the ~arweave-block@2.9 test
# suite. Read-only GETs against public peers; nothing is written to the network.
#
# Usage: scripts/fetch-arweave-fixtures.sh [output-dir]
#
# The default is the directory the suite actually reads
# (`dev_arweave_block.erl' lists `test/fixtures/arweave' at fixture load), so a
# fetch with no argument lands where the tests look.
#
# Heights are chosen to cover the awkward consensus boundaries:
#   - retarget          height rem 10  == 0
#   - price adjustment  height rem 50  == 0
#   - VDF difficulty    height rem 720 == 0
#   - plain runs        for apply/3 chains
set -euo pipefail

OUT="${1:-test/fixtures/arweave}"
PEERS=(tip-1.arweave.xyz tip-2.arweave.xyz tip-3.arweave.xyz tip-4.arweave.xyz)
PORT=1984

mkdir -p "$OUT"

# Fetch a URL from whichever peer answers first.
fetch() {
    local path="$1" dest="$2"
    for peer in "${PEERS[@]}"; do
        if curl -sf -m 30 "http://${peer}:${PORT}${path}" -o "$dest"; then
            [ -s "$dest" ] && return 0
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

TIP=$(curl -sf -m 15 "http://${PEERS[0]}:${PORT}/info" | python3 -c 'import json,sys;print(json.load(sys.stdin)["height"])')
echo "network tip: ${TIP}"

# Stay well below the tip so the range is settled and will not reorg under us.
BASE=$(( TIP - 200 ))

RETARGET=$(( (BASE / 10) * 10 ))
VDF=$(( (BASE / 720) * 720 ))

# Each target needs its predecessor too: apply/3 validates a transition.
HEIGHTS=()
for h in "$RETARGET" "$VDF"; do
    HEIGHTS+=( $((h-1)) "$h" )
done
# A plain consecutive run for chained apply/3 tests. `%.0f' because macOS `seq'
# renders large integers in scientific notation by default.
for h in $(seq -f '%.0f' $((BASE+10)) $((BASE+20))); do
    HEIGHTS+=( "$h" )
done

# Deduplicate, preserving order. `readarray' is bash 4+; macOS ships 3.2.
DEDUPED=$(printf '%s\n' "${HEIGHTS[@]}" | awk '!seen[$0]++')
HEIGHTS=()
while IFS= read -r h; do
    [ -n "$h" ] && HEIGHTS+=( "$h" )
done <<< "$DEDUPED"

echo "fetching ${#HEIGHTS[@]} blocks into ${OUT}"
for h in "${HEIGHTS[@]}"; do
    fetch_block "$h" || echo "  ${h}: SKIPPED"
done

echo "fetching transaction bodies"
for h in "${HEIGHTS[@]}"; do
    [ -s "$OUT/block-${h}.json" ] && fetch_block_txs "$h"
done

# Record provenance so a fixture can always be traced back to the network.
python3 - "$OUT" "$TIP" <<'PY'
import glob, json, os, sys
out, tip = sys.argv[1], int(sys.argv[2])
blocks = {}
for path in sorted(glob.glob(os.path.join(out, "block-*.json"))):
    b = json.load(open(path))
    blocks[b["height"]] = {
        "indep_hash": b["indep_hash"],
        "previous_block": b["previous_block"],
        "packing_difficulty": b.get("packing_difficulty"),
        "replica_format": b.get("replica_format"),
        "recall_byte2": b.get("recall_byte2"),
        "two_chunk": b.get("recall_byte2") is not None,
        "txs": len(b.get("txs", [])),
        "double_signing_proof": bool(b.get("double_signing_proof")),
        "vdf_difficulty": b["nonce_limiter_info"]["vdf_difficulty"],
        "global_step_number": b["nonce_limiter_info"]["global_step_number"],
        # Upstream's JSON encoder emits the record's `steps' list under the key
        # `checkpoints' for backwards compatibility; there is no JSON `steps'
        # key. `last_step_checkpoints' is the separate 25 intra-step values.
        "steps": len(b["nonce_limiter_info"].get("checkpoints", []) or []),
        "last_step_checkpoints":
            len(b["nonce_limiter_info"].get("last_step_checkpoints", []) or []),
        "timestamp": b["timestamp"],
    }
json.dump(
    {"network_tip_at_fetch": tip, "peers": "tip-1..4.arweave.xyz:1984", "blocks": blocks},
    open(os.path.join(out, "MANIFEST.json"), "w"),
    indent=2, sort_keys=True,
)
print(f"manifest: {len(blocks)} blocks")
PY

du -sh "$OUT"
