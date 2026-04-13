#!/usr/bin/env python3
"""
Generate the hostname-safe Arweave offset vocabulary from a saner system
dictionary.

The resulting word list is:
- lowercase ASCII only
- one word per line
- sorted and deduplicated
- gzipped for inclusion in HyperBEAM's `priv` directory

Usage:
    ./scripts/gen-arweave-offset-words.py
    ./scripts/gen-arweave-offset-words.py /path/to/output.txt.gz
"""

from __future__ import annotations

import gzip
import re
import sys
from pathlib import Path

DEFAULT_OUTPUT = Path("resources/arweave-offset-words.txt.gz")
SOURCE_PATHS = (
    Path("/usr/share/dict/web2"),
    Path("/usr/share/dict/words"),
)
WORD_RE = re.compile(r"^[a-z]+$")


def source_path() -> Path:
    for path in SOURCE_PATHS:
        if path.exists():
            return path
    raise SystemExit(
        "could not find a system dictionary at "
        + ", ".join(str(path) for path in SOURCE_PATHS)
    )


def main() -> int:
    output_path = Path(sys.argv[1]) if len(sys.argv) > 1 else DEFAULT_OUTPUT
    output_path.parent.mkdir(parents=True, exist_ok=True)
    words: set[str] = set()
    source = source_path()
    line_count = 0

    with source.open("rt", encoding="utf-8", errors="ignore") as handle:
        for raw_line in handle:
            line_count += 1
            word = raw_line.strip().lower()
            if WORD_RE.fullmatch(word):
                words.add(word)

    encoded = ("\n".join(sorted(words)) + "\n").encode()
    with gzip.open(output_path, "wb", compresslevel=9) as handle:
        handle.write(encoded)

    print(
        f"wrote {len(words)} words from {line_count} records in {source} "
        f"to {output_path}",
        file=sys.stderr,
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
