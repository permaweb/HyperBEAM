#!/usr/bin/env bash
# build-buildroot.sh — drive the Buildroot build of LapEE's kernel + initramfs
# inside the lapee-builder:latest Ubuntu-amd64 container.
#
# First-build on Rosetta-emulated amd64 takes ~30-60 min. We use Bootlin's
# pre-built x86_64 musl toolchain so the host GCC doesn't have to be built
# from source.
#
# Artefacts produced:
#   build-kernel/vmlinuz-lapee
#   work/initramfs-lapee.cpio.gz
set -euo pipefail
cd "$(dirname "$0")/.."

LAPEE_ROOT="$(pwd)"
VOLUME=lapee-build-m1
IMAGE=lapee-builder:latest
DEFCONFIG=${DEFCONFIG:-lapee_m1_defconfig}

# Ensure the docker volume + buildroot tree exist inside it.
docker volume inspect $VOLUME >/dev/null 2>&1 || docker volume create $VOLUME

# Sync the external tree into the volume (always — it's tiny).
docker run --rm --platform=linux/amd64 \
  -v $VOLUME:/build \
  -v "$LAPEE_ROOT/buildroot-external":/src-external:ro \
  $IMAGE bash -c "rm -rf /build/buildroot-external && cp -r /src-external /build/buildroot-external"

# If the buildroot source tree isn't in the volume yet, copy it in.
if ! docker run --rm --platform=linux/amd64 -v $VOLUME:/build $IMAGE \
        bash -c "test -f /build/buildroot/Makefile"; then
    echo "=== Copying buildroot/ into volume (one-time) ==="
    docker run --rm --platform=linux/amd64 \
      -v $VOLUME:/build \
      -v "$LAPEE_ROOT/buildroot":/src-buildroot:ro \
      $IMAGE bash -c "cp -r /src-buildroot /build/buildroot"
fi

# Re-generate defconfig if /build/out doesn't exist or defconfig changed.
if ! docker run --rm --platform=linux/amd64 -v $VOLUME:/build $IMAGE \
        bash -c "test -f /build/out/.config"; then
    echo "=== Generating $DEFCONFIG ==="
    docker run --rm --platform=linux/amd64 -v $VOLUME:/build $IMAGE \
      bash -c "mkdir -p /build/out && cd /build/buildroot && \
               make O=/build/out BR2_EXTERNAL=/build/buildroot-external $DEFCONFIG"
fi

# Run the build. Remove any previous detached container.
docker rm -f lapee-br-build 2>/dev/null || true
echo "=== Starting Buildroot build in detached container 'lapee-br-build' ==="
docker run -d --name lapee-br-build --platform=linux/amd64 \
  -v $VOLUME:/build \
  $IMAGE bash -c "cd /build/out && date && make -j4 2>&1 | tee /build/out/build.log; echo BUILDROOT-EXIT=\$?"

echo "Build started. Tail logs with:"
echo "  docker logs -f lapee-br-build"
echo
echo "When build finishes, run:"
echo "  ./scripts/collect-buildroot-artefacts.sh"
