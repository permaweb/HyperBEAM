#!/usr/bin/env bash
# overlay-hyperbeam.sh — graft an already-built HyperBEAM release into the
# Buildroot initramfs as a post-build step.
#
# Why post-build instead of Buildroot package: HyperBEAM has Rust NIFs
# (rustler, quicer, rocksdb) that are painful to cross-compile via
# Buildroot's package framework. We build HB separately in the
# `lapee-hyperbeam-builder:latest` Ubuntu-amd64 container (that path is
# already working in scripts/build-initramfs.sh) and graft the _build/prod
# release + dynamic libs it depends on into the Buildroot initramfs tree.
#
# Input:
#   build-alpine/vmlinuz-lapee              (Buildroot kernel, unused here)
#   work/initramfs-lapee.cpio.gz            (Buildroot initramfs — our base)
#   build-hyperbeam/src/_build/prod/rel/hb  (HyperBEAM release)
#
# Output:
#   work/initramfs-lapee-hb.cpio.gz         (initramfs with HB overlay)
#
# After this, the lapee-init PID 1 will find /usr/bin/hyperbeam on boot
# and exec it instead of falling back to /bin/sh.

set -euo pipefail
cd "$(dirname "$0")/.."

BASE=work/initramfs-lapee.cpio.gz
HB_REL=build-hyperbeam/src/_build/prod/rel/hb
OUT=work/initramfs-lapee-hb.cpio.gz

[[ -f "$BASE" ]] || { echo "missing $BASE (run: make buildroot)"; exit 1; }
[[ -d "$HB_REL" ]] || { echo "missing $HB_REL (HyperBEAM release)"; exit 1; }

WORK=$(mktemp -d)
trap "rm -rf $WORK" EXIT

echo "=== Unpacking $BASE into $WORK ==="
mkdir -p "$WORK/root"
(cd "$WORK/root" && gzip -dc "$OLDPWD/$BASE" | cpio -idm --quiet)

echo "=== Copying HyperBEAM release into /usr/lib/hyperbeam ==="
mkdir -p "$WORK/root/usr/lib/hyperbeam"
cp -a "$HB_REL/." "$WORK/root/usr/lib/hyperbeam/"

echo "=== Installing /usr/bin/hyperbeam wrapper ==="
cat > "$WORK/root/usr/bin/hyperbeam" <<'EOF'
#!/bin/sh
# LapEE hyperbeam wrapper: exec the OTP release in foreground mode.
exec /usr/lib/hyperbeam/bin/hb foreground "$@"
EOF
chmod +x "$WORK/root/usr/bin/hyperbeam"

# Collect shared libs HB needs from the hyperbeam builder container.
# The Buildroot rootfs uses musl; HB was built against glibc. We need to
# either rebuild HB against musl or ship glibc + all HB's deps. For M1
# demonstration, the simpler path is to ship glibc into /lib64/ and use
# the exact library set from the HB builder.
#
# This helper is kept simple: it relies on build-initramfs.sh's already-
# computed library layout and just copies work/initramfs-mini tree's libs
# if present.
if [[ -f work/initramfs-mini.cpio.gz ]]; then
    echo "=== Copying shared libs from work/initramfs-mini.cpio.gz ==="
    MINI="$WORK/mini"
    mkdir -p "$MINI"
    (cd "$MINI" && gzip -dc "$OLDPWD/work/initramfs-mini.cpio.gz" | cpio -idm --quiet \
        './usr/local/lib/erlang/*' \
        './usr/local/bin/erl' \
        './lib/x86_64-linux-gnu/*' \
        './lib64/*' \
        2>/dev/null || true)
    cp -a "$MINI"/lib/x86_64-linux-gnu/. "$WORK/root/lib/x86_64-linux-gnu/" 2>/dev/null || \
        mkdir -p "$WORK/root/lib/x86_64-linux-gnu" && cp -a "$MINI"/lib/x86_64-linux-gnu/. "$WORK/root/lib/x86_64-linux-gnu/" 2>/dev/null || true
    [[ -d "$MINI/lib64" ]] && cp -a "$MINI"/lib64/. "$WORK/root/lib64/" 2>/dev/null || true
    [[ -d "$MINI/usr/local/lib/erlang" ]] && mkdir -p "$WORK/root/usr/local/lib" && cp -a "$MINI"/usr/local/lib/erlang "$WORK/root/usr/local/lib/" || true
    [[ -f "$MINI/usr/local/bin/erl" ]] && mkdir -p "$WORK/root/usr/local/bin" && cp "$MINI"/usr/local/bin/erl "$WORK/root/usr/local/bin/" || true
fi

echo "=== Re-packing cpio ==="
(cd "$WORK/root" && find . | cpio -o -H newc --quiet | gzip -1 > "$OLDPWD/$OUT")
ls -lh "$OUT"

echo "=== Overlay complete ==="
echo "Boot with:"
echo "  INITRD=work/initramfs-lapee-hb.cpio.gz ./scripts/boot-buildroot.sh"
