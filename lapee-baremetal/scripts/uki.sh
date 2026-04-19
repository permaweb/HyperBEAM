#!/usr/bin/env bash
# uki.sh — build and sign a Unified Kernel Image for LapEE.
#
# Produces a single signed PE binary containing:
#   - Linux kernel (bzImage)
#   - initramfs (cpio.xz, embedding lapee-init)
#   - cmdline (includes dm-verity root hash and IOMMU args)
#   - os-release stub
#
# Uses systemd-stub (from Buildroot host-systemd) as the EFI PE glue
# and sbsign with operator-enrolled db key for signing.
#
# Runs Linux-only tools via the lapee-tools container.
#
# Invocation (from Makefile):
#   ./scripts/uki.sh \
#       --kernel out/bzImage \
#       --initrd out/initrd.img \
#       --cmdline "root=/dev/mapper/verity-root ..." \
#       --verity-root-hash <hex> \
#       --db-key out/keys/db.key \
#       --db-cert out/keys/db.crt \
#       --output out/lapee-uki.efi
#
# Once the Buildroot build is complete, this is the last step before
# the UKI is bootable under QEMU+OVMF.

set -euo pipefail

LAPEE_ROOT="${LAPEE_ROOT:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"
source "${LAPEE_ROOT}/scripts/tools.sh"

KERNEL=""
INITRD=""
CMDLINE=""
VERITY_ROOT=""
DB_KEY=""
DB_CERT=""
OUTPUT=""

while [[ $# -gt 0 ]]; do
    case $1 in
        --kernel)           KERNEL="$2"; shift 2 ;;
        --initrd)           INITRD="$2"; shift 2 ;;
        --cmdline)          CMDLINE="$2"; shift 2 ;;
        --verity-root-hash) VERITY_ROOT="$2"; shift 2 ;;
        --db-key)           DB_KEY="$2"; shift 2 ;;
        --db-cert)          DB_CERT="$2"; shift 2 ;;
        --output)           OUTPUT="$2"; shift 2 ;;
        *) echo "unknown: $1" >&2; exit 1 ;;
    esac
done

: "${KERNEL:?--kernel required}"
: "${INITRD:?--initrd required}"
: "${CMDLINE:?--cmdline required}"
: "${OUTPUT:?--output required}"

WORK="${LAPEE_ROOT}/work/uki-build"
mkdir -p "${WORK}"

# Inject verity root hash into cmdline if provided.
if [[ -n "${VERITY_ROOT}" ]]; then
    CMDLINE="${CMDLINE} roothash=${VERITY_ROOT}"
fi

echo "${CMDLINE}" > "${WORK}/cmdline.txt"
cat > "${WORK}/os-release" <<EOF
NAME="LapEE"
ID=lapee
VERSION_ID="${LAPEE_VERSION:-dev}"
PRETTY_NAME="LapEE (${LAPEE_VERSION:-dev})"
EOF

# Use systemd-ukify if available (cleaner), else fall back to manual sd-stub
# objcopy dance. Both are in the tools container.
lapee_tool bash -c "
    set -e
    if command -v ukify >/dev/null 2>&1; then
        ukify build \\
            --linux='${KERNEL}' \\
            --initrd='${INITRD}' \\
            --cmdline='$(cat "${WORK}/cmdline.txt")' \\
            --os-release='@${WORK}/os-release' \\
            --output='/work/uki-build/uki.unsigned.efi'
    else
        # Manual: use systemd-stub with objcopy sections.
        STUB=\$(find / -name 'linuxx64.efi.stub' -print -quit 2>/dev/null)
        : \${STUB:?systemd-stub not found}
        objcopy \\
            --add-section .osrel='${WORK}/os-release' --change-section-vma .osrel=0x20000 \\
            --add-section .cmdline='${WORK}/cmdline.txt' --change-section-vma .cmdline=0x30000 \\
            --add-section .linux='${KERNEL}' --change-section-vma .linux=0x2000000 \\
            --add-section .initrd='${INITRD}' --change-section-vma .initrd=0x3000000 \\
            \"\${STUB}\" '/work/uki-build/uki.unsigned.efi'
    fi
"

# Sign the UKI with the operator's db key.
if [[ -n "${DB_KEY}" && -n "${DB_CERT}" ]]; then
    lapee_tool sbsign --key "/work/uki-build/$(basename "${DB_KEY}")" \
                      --cert "/work/uki-build/$(basename "${DB_CERT}")" \
                      --output "/work/uki-build/uki.signed.efi" \
                      "/work/uki-build/uki.unsigned.efi"
    cp "${WORK}/uki.signed.efi" "${OUTPUT}"
    echo "signed UKI: ${OUTPUT}"
else
    echo "WARNING: no signing key supplied, producing unsigned UKI (will not boot under Secure Boot)"
    cp "${WORK}/uki.unsigned.efi" "${OUTPUT}"
fi
