#!/usr/bin/env bash
# verity.sh — seal a rootfs image with dm-verity and emit the root hash.
#
# Input:  a squashfs or ext4 rootfs image
# Output: a .verity hash tree file and the hex root hash on stdout
#
# The root hash is what goes into the UKI cmdline, so that the running
# kernel can validate every block of the rootfs at read time.

set -euo pipefail

LAPEE_ROOT="${LAPEE_ROOT:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"
source "${LAPEE_ROOT}/scripts/tools.sh"

ROOTFS="${1:?rootfs image required}"
VERITY_OUT="${2:-${ROOTFS}.verity}"

ROOTFS_BASENAME=$(basename "${ROOTFS}")
VERITY_BASENAME=$(basename "${VERITY_OUT}")

# Copy rootfs into /work for container access.
cp "${ROOTFS}" "${LAPEE_ROOT}/work/${ROOTFS_BASENAME}"

HASH=$(lapee_tool bash -c "
    veritysetup format '/work/${ROOTFS_BASENAME}' '/work/${VERITY_BASENAME}' 2>&1 \
      | awk '/Root hash:/ {print \$3}'
")

cp "${LAPEE_ROOT}/work/${VERITY_BASENAME}" "${VERITY_OUT}"
echo "${HASH}"
