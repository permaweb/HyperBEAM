#!/usr/bin/env bash
# build-hb-release.sh — produce `_build/lapee/rel/hb' for the dev_tpm2
# LapEE image. Runs inside the `lapee-hyperbeam-builder' Docker image
# (Ubuntu 24.04 amd64 under Rosetta on Apple Silicon) with a bind-
# mount into `build-hyperbeam/src-edge/'.
#
# Expected wall-clock on M-series Macs: 12-20 min first build, 2-5 min
# incremental (only Erlang changes rebuild; the Rust NIF chain and
# wamr stay cached in `_build/lapee/lib/').
#
# Output: build-hyperbeam/src-edge/_build/lapee/rel/hb/bin/hb (+rel)
#
# Preconditions:
#   - `docker images lapee-hyperbeam-builder:latest' exists
#     (run `make builders' if not)
#   - `build-hyperbeam/src-edge/' holds a HyperBEAM source checkout
#     with the dev_tpm2 module + NIF sources
set -euo pipefail
cd "$(dirname "$0")/.."
LAPEE=$(pwd)

SRC="$LAPEE/build-hyperbeam/src-edge"
HB_ROOT="$LAPEE/.."

# Always re-seed src-edge from the parent HB repo so edits to src/,
# native/, etc. on the lapee branch propagate without a manual delete.
# `_build/' is preserved across seedings (cheap incremental rebar3).
if [[ -f "$HB_ROOT/rebar.config" && -d "$HB_ROOT/src" ]]; then
    echo "=== seeding $SRC from $HB_ROOT (sync source) ==="
    # `native/lib/secp256k1/' is a submodule. In-tree Makefile runs
    # `git submodule update' inside the builder container, which
    # can't reach the host's worktree .git/ — initialise the
    # submodule on the host once so the builder sees a populated
    # tree.
    if [[ ! -f "$HB_ROOT/native/lib/secp256k1/CMakeLists.txt" ]]; then
        echo "=== initialising secp256k1 submodule on host ==="
        (cd "$HB_ROOT" && git submodule update --init \
            native/lib/secp256k1 >/dev/null 2>&1) || true
    fi
    mkdir -p "$SRC"
    rsync -a \
        --exclude='_build/' --exclude='.git/' \
        --exclude='priv/' --exclude='logs/' --exclude='metrics/' \
        --exclude='rebar3.crashdump' \
        --exclude='native/lib/secp256k1/build/' \
        --exclude='*.o' --exclude='*.so' --exclude='*.dylib' \
        --exclude='*.cargo/' \
        --exclude='lapee-baremetal/build-hyperbeam/' \
        --exclude='lapee-baremetal/work/' \
        --exclude='lapee-baremetal/out/' \
        "$HB_ROOT/" "$SRC/"
    # Also scrub any platform-specific build detritus that may have
    # been rsynced on earlier passes. If the host previously ran
    # `rebar3 compile' natively (e.g. to build a macOS-native
    # verifier HB), native/*/ will contain Mach-O .o files that the
    # Linux container's linker can't grok.
    find "$SRC/native" \( -name '*.o' -o -name '*.so' -o -name '*.dylib' \) \
        -delete 2>/dev/null || true
    find "$SRC/native/lib/secp256k1/build" -type f -delete 2>/dev/null || true
elif [[ ! -f "$SRC/rebar.config" ]]; then
    echo "missing HyperBEAM source at $SRC, and parent ($HB_ROOT) does"   >&2
    echo "not look like a HyperBEAM checkout either. Either populate"    >&2
    echo "$SRC manually or run this script from inside a HyperBEAM repo" >&2
    echo "where lapee-baremetal/ is a subdirectory."                     >&2
    exit 1
fi

# Kill any dangling build container.
docker rm -f lapee-hb-edge-build 2>/dev/null || true

# First-run cleanup of the release output directory so relx doesn't
# complain about the existing release. `_build/lapee/lib' is kept for
# incremental rebuilds.
rm -rf "$SRC/_build/lapee/rel"

# relx copies `config.flat' into the release when it exists; when it
# doesn't, it prints a warning and returns a nonzero exit. The file
# isn't used by the LapEE guest flow (HB_CONFIG is set in init-hb),
# so we drop an empty placeholder to keep relx quiet.
touch "$SRC/config.flat"

docker run --platform=linux/amd64 --rm --name lapee-hb-edge-build \
    -v "$SRC":/src \
    -w /src \
    lapee-hyperbeam-builder:latest \
    bash -c '
        set -e
        rebar3 as lapee release
    '

echo ""
ls -lh "$SRC/_build/lapee/rel/hb/bin/hb"
echo ""
echo "HB release ready for: ./scripts/build-initramfs-hb.sh"
