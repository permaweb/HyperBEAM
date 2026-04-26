#!/usr/bin/env bash
# collect-buildroot-artefacts.sh — copy Buildroot outputs from the lapee-build-m1
# Docker volume into the worktree's build-kernel/ and work/ directories.
set -euo pipefail
cd "$(dirname "$0")/.."

VOLUME=lapee-build-m1
IMAGE=lapee-builder:latest

# Sanity-check that outputs actually exist in the volume.
docker run --rm --platform=linux/amd64 -v $VOLUME:/build $IMAGE \
    bash -c "ls /build/out/images/" || {
    echo "[FAIL] /build/out/images/ empty — build didn't complete?"
    exit 1
}

mkdir -p build-kernel work

# Copy kernel.
docker run --rm --platform=linux/amd64 -v $VOLUME:/build -v "$PWD/build-kernel:/host-out" $IMAGE \
    bash -c "test -f /build/out/images/bzImage && cp /build/out/images/bzImage /host-out/vmlinuz-lapee && \
             ls -lh /host-out/vmlinuz-lapee"

# Copy initramfs.
docker run --rm --platform=linux/amd64 -v $VOLUME:/build -v "$PWD/work:/host-work" $IMAGE \
    bash -c "if [ -f /build/out/images/rootfs.cpio.gz ]; then \
               cp /build/out/images/rootfs.cpio.gz /host-work/initramfs-lapee.cpio.gz; \
             elif [ -f /build/out/images/rootfs.cpio ]; then \
               gzip -c /build/out/images/rootfs.cpio > /host-work/initramfs-lapee.cpio.gz; \
             else \
               echo 'no rootfs.cpio found'; exit 1; \
             fi; \
             ls -lh /host-work/initramfs-lapee.cpio.gz"

# Also copy the kernel .config as evidence of the hardened symbols.
docker run --rm --platform=linux/amd64 -v $VOLUME:/build -v "$PWD/work:/host-work" $IMAGE \
    bash -c "cp /build/out/build/linux-*/.config /host-work/linux.config.built 2>/dev/null || \
             echo 'no kernel .config found (linux pkg not built yet)'"

echo
echo "=== Artefacts collected ==="
ls -lh build-kernel/vmlinuz-lapee work/initramfs-lapee.cpio.gz 2>&1 || true
[[ -f work/linux.config.built ]] && echo "kernel .config: work/linux.config.built"
