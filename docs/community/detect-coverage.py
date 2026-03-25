#!/usr/bin/env python3
"""Detect which block height ranges an Arweave node has chunk data for.

Uses the /data_sync_record endpoint to get byte offset ranges, then binary
searches for the block heights at each boundary using weave_size from block
headers.

Usage:
    python3 detect-coverage.py [NODE_URL]

NODE_URL defaults to http://localhost:1984
"""

import json
import sys
import urllib.request

DEFAULT_NODE = "http://localhost:1984"

def fetch_json(url, headers=None, timeout=30):
    req = urllib.request.Request(url)
    if headers:
        for k, v in headers.items():
            req.add_header(k, v)
    with urllib.request.urlopen(req, timeout=timeout) as resp:
        return json.load(resp)

_weave_cache = {}

def get_weave_size(node, height):
    if height in _weave_cache:
        return _weave_cache[height]
    try:
        data = fetch_json(f"{node}/block/height/{height}")
    except urllib.error.HTTPError:
        return None
    ws = int(data["weave_size"])
    _weave_cache[height] = ws
    return ws

def find_height_for_offset(node, target, lo, hi):
    """Binary search for the block height where weave_size crosses target.
    A 404 means the node doesn't have that block — treat as below target.
    """
    while lo < hi:
        mid = (lo + hi) // 2
        ws = get_weave_size(node, mid)
        if ws is None or ws < target:
            lo = mid + 1
        else:
            hi = mid
    return lo

def merge_offset_ranges(sync_record, gap_bytes=3_600_000_000_000):
    """Parse and merge byte offset ranges from the sync record.
    gap_bytes controls how close ranges must be to merge (default ~1 partition).
    """
    raw = []
    for entry in sync_record:
        for end_s, start_s in entry.items():
            raw.append((int(start_s), int(end_s)))
    raw.sort()
    if not raw:
        return []
    merged = [raw[0]]
    for lo, hi in raw[1:]:
        prev_lo, prev_hi = merged[-1]
        if lo <= prev_hi + gap_bytes:
            merged[-1] = (prev_lo, max(prev_hi, hi))
        else:
            merged.append((lo, hi))
    return merged

def main():
    node = sys.argv[1] if len(sys.argv) > 1 else DEFAULT_NODE

    print(f"Node: {node}")

    info = fetch_json(f"{node}/info")
    max_height = info["height"]
    print(f"Chain height: {max_height:,}")

    print("Fetching data sync record...")
    sync = fetch_json(
        f"{node}/data_sync_record",
        headers={"Content-Type": "application/json"}
    )
    print(f"Raw chunk ranges: {len(sync)}")

    total_bytes = sum(int(e) - int(s) for r in sync for e, s in r.items())
    print(f"Total chunk data: {total_bytes / 1e12:.2f} TB")

    print("Merging offset ranges...")
    merged_offsets = merge_offset_ranges(sync)
    print(f"Merged to {len(merged_offsets)} offset ranges")

    searches = len(merged_offsets) * 2
    steps = searches * 21
    print(f"Binary searching height boundaries ({searches} searches, ~{steps} requests)...")
    height_ranges = []
    for i, (off_lo, off_hi) in enumerate(merged_offsets):
        h_lo = find_height_for_offset(node, off_lo, 0, max_height)
        h_hi = find_height_for_offset(node, off_hi, h_lo, max_height)
        height_ranges.append((h_lo, h_hi))
        sys.stdout.write(f"\r  {i + 1}/{len(merged_offsets)}")
        sys.stdout.flush()
    print()

    height_ranges.sort()
    merged_heights = [height_ranges[0]]
    for lo, hi in height_ranges[1:]:
        prev_lo, prev_hi = merged_heights[-1]
        if lo <= prev_hi + 100:
            merged_heights[-1] = (prev_lo, max(prev_hi, hi))
        else:
            merged_heights.append((lo, hi))

    print(f"\nChunk data coverage ({len(merged_heights)} ranges):")
    print("-" * 50)
    for lo, hi in merged_heights:
        print(f"  {lo:>10,} - {hi:>10,}  ({hi - lo:>10,} blocks)")

    total_blocks = sum(hi - lo for lo, hi in merged_heights)
    pct = total_blocks / max_height * 100
    print("-" * 50)
    print(f"Total: ~{total_blocks:,} blocks ({pct:.1f}% of {max_height:,})")

    print(f"\nJSON:")
    json_out = {
        "node": node,
        "chain_height": max_height,
        "total_chunk_data_bytes": total_bytes,
        "ranges": [{"from": lo, "to": hi} for lo, hi in merged_heights]
    }
    print(json.dumps(json_out, indent=2))

if __name__ == "__main__":
    main()
