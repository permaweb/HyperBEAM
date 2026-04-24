#!/usr/bin/env bash
# build-usb-image.sh — assemble a UEFI-bootable LapEE USB image.
#
# Takes three inputs (kernel + initramfs + cmdline) and produces
# a GPT-partitioned disk image with a single EFI System Partition
# containing a single Unified Kernel Image at the UEFI fallback
# boot path (\EFI\Boot\BootX64.efi). UEFI firmware executes that
# path automatically when no NVRAM BootOrder entry is configured
# — so the image is fully portable between machines without
# touching Framework NVRAM.
#
# The UKI is assembled in-container by running `systemd-ukify` over
# the supplied kernel + initramfs + cmdline, stamped with os-release
# metadata, linked against systemd-stub (the systemd-boot-efi shim).
#
# For signed-UKI workflows see `scripts/sb-setup.sh': it signs the
# UKI produced by this script with the operator's db.key and
# re-invokes this script with `--uki <signed>' to wrap the signed
# PE into a new USB image.
#
#   Inputs  : --kernel PATH --initramfs PATH --cmdline TEXT
#             [--size MIB]     image size in MiB (default 1024)
#             [--uki PATH]     skip the inline ukify build and use
#                              a pre-built UKI (e.g. the signed
#                              one produced by sb-setup.sh)
#   Outputs : --image PATH     write an .img file you can dd
#             OR
#             --device PATH    write directly to a raw block dev
#                              (macOS: /dev/rdiskN; Linux:
#                               /dev/sdX). Prompts before writing.
#
# On macOS the script wraps all Linux tooling through the
# lapee-tools Docker image. On Linux it runs natively when the
# required binaries (parted, mkfs.vfat, mcopy, systemd-ukify or
# objcopy) are already installed, else falls back to the same
# container.
#
# Example:
#
#   ./scripts/build-usb-image.sh \
#     --kernel build-alpine/vmlinuz-lapee \
#     --initramfs work/initramfs-hb.cpio.gz \
#     --cmdline "console=tty0 console=ttyS0 ima_policy=tcb panic=10" \
#     --image work/lapee-usb.img
#
#   # then to write to a USB stick on macOS:
#   diskutil list                                 # find /dev/diskN
#   diskutil unmountDisk /dev/diskN
#   sudo dd if=work/lapee-usb.img of=/dev/rdiskN bs=4m status=progress
#
# Or directly:
#
#   ./scripts/build-usb-image.sh \
#     --kernel ... --initramfs ... --cmdline ... \
#     --device /dev/disk4

set -euo pipefail

LAPEE_ROOT="${LAPEE_ROOT:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"
LAPEE_IMAGE="${LAPEE_IMAGE:-lapee-tools:latest}"
WORK="${LAPEE_ROOT}/work"

KERNEL=""
INITRAMFS=""
CMDLINE=""
PREBUILT_UKI=""
OUT_IMAGE=""
OUT_DEVICE=""
SIZE_MIB=1024

die() { echo "error: $*" >&2; exit 1; }

usage() {
    sed -n '/^# /,/^$/p' "$0" | sed 's/^# \{0,1\}//'
    exit 1
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --kernel)    KERNEL="$2"; shift 2 ;;
        --initramfs) INITRAMFS="$2"; shift 2 ;;
        --cmdline)   CMDLINE="$2"; shift 2 ;;
        --uki)       PREBUILT_UKI="$2"; shift 2 ;;
        --size)      SIZE_MIB="$2"; shift 2 ;;
        --image)     OUT_IMAGE="$2"; shift 2 ;;
        --device)    OUT_DEVICE="$2"; shift 2 ;;
        -h|--help)   usage ;;
        *) die "unknown argument: $1 (use --help)" ;;
    esac
done

# ---- input validation ------------------------------------------

if [[ -z "$OUT_IMAGE" && -z "$OUT_DEVICE" ]]; then
    die "one of --image or --device is required"
fi
if [[ -n "$OUT_IMAGE" && -n "$OUT_DEVICE" ]]; then
    die "--image and --device are mutually exclusive"
fi

if [[ -z "$PREBUILT_UKI" ]]; then
    [[ -n "$KERNEL"    ]] || die "--kernel required (or supply --uki)"
    [[ -n "$INITRAMFS" ]] || die "--initramfs required (or supply --uki)"
    [[ -n "$CMDLINE"   ]] || die "--cmdline required (or supply --uki)"
    [[ -f "$KERNEL"    ]] || die "kernel not found: $KERNEL"
    [[ -f "$INITRAMFS" ]] || die "initramfs not found: $INITRAMFS"
else
    [[ -f "$PREBUILT_UKI" ]] || die "UKI not found: $PREBUILT_UKI"
fi

mkdir -p "$WORK"
BUILD_DIR="$WORK/usb-build"
rm -rf "$BUILD_DIR"
mkdir -p "$BUILD_DIR"

# ---- step 1: build or stage the UKI ---------------------------

if [[ -n "$PREBUILT_UKI" ]]; then
    cp "$PREBUILT_UKI" "$BUILD_DIR/lapee.efi"
    echo ">> using pre-built UKI: $PREBUILT_UKI"
else
    echo ">> building UKI from kernel + initramfs"
    # Stage inputs under $BUILD_DIR (== $LAPEE_ROOT/work/usb-build).
    # The tools container mounts $LAPEE_ROOT/work as /work, so
    # the same directory is visible at /work/usb-build inside.
    # All paths passed to ukify below are container-absolute to
    # avoid host/container path confusion.
    cp "$KERNEL"    "$BUILD_DIR/kernel"
    cp "$INITRAMFS" "$BUILD_DIR/initramfs.cpio.gz"
    cat > "$BUILD_DIR/os-release" <<EOF
NAME="LapEE"
ID=lapee
VERSION_ID="${LAPEE_VERSION:-dev}"
PRETTY_NAME="LapEE (${LAPEE_VERSION:-dev})"
EOF
    echo "$CMDLINE" > "$BUILD_DIR/cmdline.txt"

    docker run --rm --platform=linux/amd64 \
        -v "${LAPEE_ROOT}/work":/work \
        -w /work/usb-build \
        "$LAPEE_IMAGE" \
        bash -euo pipefail -c "
            if command -v ukify >/dev/null 2>&1; then
                ukify build \\
                    --linux=/work/usb-build/kernel \\
                    --initrd=/work/usb-build/initramfs.cpio.gz \\
                    --cmdline=\"\$(cat /work/usb-build/cmdline.txt)\" \\
                    --os-release=@/work/usb-build/os-release \\
                    --output=/work/usb-build/lapee.efi
            else
                # Manual fallback via systemd-stub + objcopy.
                STUB=\$(find /usr/lib /lib -name 'linuxx64.efi.stub' \\
                        -print -quit 2>/dev/null)
                : \${STUB:?systemd-stub not found}
                objcopy \\
                    --add-section .osrel=/work/usb-build/os-release \\
                    --change-section-vma .osrel=0x20000 \\
                    --add-section .cmdline=/work/usb-build/cmdline.txt \\
                    --change-section-vma .cmdline=0x30000 \\
                    --add-section .linux=/work/usb-build/kernel \\
                    --change-section-vma .linux=0x2000000 \\
                    --add-section .initrd=/work/usb-build/initramfs.cpio.gz \\
                    --change-section-vma .initrd=0x3000000 \\
                    \"\${STUB}\" /work/usb-build/lapee.efi
            fi
        "
fi

UKI_SIZE=$(stat -f %z "$BUILD_DIR/lapee.efi" 2>/dev/null \
           || stat -c %s "$BUILD_DIR/lapee.efi")
echo ">> UKI size: $UKI_SIZE bytes"

ESP_MIN=$(( (UKI_SIZE / (1024 * 1024)) + 32 ))
if (( SIZE_MIB < ESP_MIN )); then
    die "--size $SIZE_MIB MiB too small (UKI needs at least $ESP_MIN MiB plus GPT overhead)"
fi

# ---- step 1b: stage SB enrolment .auth files if present -------
# When `sb-setup.sh enrol' has produced PK/KEK/db .auth files
# under secureboot/enrol/, drop them into the ESP root so the
# Framework BIOS can enrol from the same stick that carries the
# signed UKI. One-stick flow: plug in, F2, Setup Mode, enrol
# db.auth / KEK.auth / PK.auth, save+exit, boot.

SB_ENROL_DIR="${LAPEE_ROOT}/secureboot/enrol"
STAGED_AUTH=""
if [[ -d "$SB_ENROL_DIR" ]]; then
    for f in PK.auth KEK.auth db.auth; do
        if [[ -f "$SB_ENROL_DIR/$f" ]]; then
            cp "$SB_ENROL_DIR/$f" "$BUILD_DIR/$f"
            STAGED_AUTH="${STAGED_AUTH}${STAGED_AUTH:+ }$f"
        fi
    done
fi
if [[ -n "$STAGED_AUTH" ]]; then
    echo ">> staging SB enrolment bundle: $STAGED_AUTH"
fi

# Stage host-side wifi.conf if present. Lands at /EFI/boot/
# wifi.conf in the ESP. Credentials are NOT measured (the ESP
# partition is not part of the UKI). Parser in the init script
# enforces size bounds, charset, and NUL-freeness.
if [[ -f "${LAPEE_ROOT}/wifi.conf" ]]; then
    cp "${LAPEE_ROOT}/wifi.conf" "$BUILD_DIR/wifi.conf"
    echo ">> staging wifi.conf ($(wc -c <"${LAPEE_ROOT}/wifi.conf" | tr -d ' ') bytes)"
fi

# ---- step 2: build the disk image inside the tools container --

IMG_IN_WORK="usb-build/disk.img"

docker run --rm --platform=linux/amd64 \
    -v "${LAPEE_ROOT}/work":/work \
    -w /work \
    "$LAPEE_IMAGE" \
    bash -euo pipefail -c "
        # Blank raw image.
        truncate -s ${SIZE_MIB}M /work/${IMG_IN_WORK}

        # GPT with one EFI System Partition spanning the volume.
        parted --script /work/${IMG_IN_WORK} \\
            mklabel gpt \\
            mkpart ESP fat32 1MiB 100% \\
            set 1 esp on

        # Compute where the ESP starts. parted prints a machine-
        # readable layout with 'unit s print' — we grab partition
        # 1's start LBA (512-byte sectors).
        START_LBA=\$(parted --script --machine /work/${IMG_IN_WORK} \\
            unit s print | awk -F: '/^1:/ {gsub(\"s\",\"\",\$2); print \$2}')
        SECTORS=\$(parted --script --machine /work/${IMG_IN_WORK} \\
            unit s print | awk -F: '/^1:/ {gsub(\"s\",\"\",\$4); print \$4}')
        echo \">> ESP starts at sector \$START_LBA, spans \$SECTORS sectors\"

        # Carve the ESP out to a separate file so mkfs.vfat /
        # mtools can work on it directly (no loopback = no
        # privileged mount inside the container).
        dd if=/work/${IMG_IN_WORK} of=/work/usb-build/esp.img \\
            bs=512 skip=\$START_LBA count=\$SECTORS \\
            status=none conv=sparse

        mkfs.vfat -F 32 -n LAPEE_ESP /work/usb-build/esp.img \\
            >/dev/null

        # Populate the ESP via mtools (no mount needed).
        mmd -i /work/usb-build/esp.img ::/EFI
        mmd -i /work/usb-build/esp.img ::/EFI/Boot
        mcopy -i /work/usb-build/esp.img \\
            /work/usb-build/lapee.efi ::/EFI/Boot/BootX64.efi
        # Also drop a friendly marker so \`ls\`-ing the stick on
        # a host shows what it is.
        echo 'LapEE UEFI-bootable USB. UKI at /EFI/Boot/BootX64.efi.' \\
            > /work/usb-build/README.TXT
        mcopy -i /work/usb-build/esp.img \\
            /work/usb-build/README.TXT ::/README.TXT

        # SB enrolment .auth files at ESP root (if staged by the
        # host wrapper above). Framework BIOS Setup-Mode enrolment
        # browses this partition; the operator picks each file in
        # order db.auth -> KEK.auth -> PK.auth.
        for _a in PK.auth KEK.auth db.auth; do
            if [[ -f /work/usb-build/\$_a ]]; then
                mcopy -i /work/usb-build/esp.img \\
                    /work/usb-build/\$_a ::/\$_a
            fi
        done

        # wifi.conf at /EFI/boot/wifi.conf if staged. The signed
        # UKI cmdline carries lapee.wifi=enabled -- the capability
        # flag -- but the credentials themselves are unmeasured.
        if [[ -f /work/usb-build/wifi.conf ]]; then
            mcopy -i /work/usb-build/esp.img \\
                /work/usb-build/wifi.conf ::/EFI/boot/wifi.conf
        fi

        # Seal the ESP back into the disk image.
        dd if=/work/usb-build/esp.img of=/work/${IMG_IN_WORK} \\
            bs=512 seek=\$START_LBA count=\$SECTORS \\
            conv=notrunc,sparse status=none

        echo '>> verifying partition layout:'
        parted --script /work/${IMG_IN_WORK} unit MiB print

        ls -lh /work/${IMG_IN_WORK}
    "

FINAL_IMG="${LAPEE_ROOT}/work/${IMG_IN_WORK}"
if [[ ! -f "$FINAL_IMG" ]]; then
    die "image build failed (no $FINAL_IMG)"
fi

# ---- step 3: move to --image or write to --device --------------

if [[ -n "$OUT_IMAGE" ]]; then
    mkdir -p "$(dirname "$OUT_IMAGE")"
    mv "$FINAL_IMG" "$OUT_IMAGE"
    IMG_BYTES=$(stat -f %z "$OUT_IMAGE" 2>/dev/null \
                || stat -c %s "$OUT_IMAGE")
    echo ""
    echo "=========================================================="
    echo ">> USB image ready: $OUT_IMAGE ($IMG_BYTES bytes)"
    echo "=========================================================="
    echo "To write to a USB stick on macOS:"
    echo "  diskutil list                       # find /dev/diskN"
    echo "  diskutil unmountDisk /dev/diskN"
    echo "  sudo dd if=$OUT_IMAGE of=/dev/rdiskN bs=4m status=progress"
    echo "  diskutil eject /dev/diskN"
    echo ""
    echo "On Linux:"
    echo "  sudo dd if=$OUT_IMAGE of=/dev/sdX bs=4M status=progress conv=fsync"
    echo ""
fi

if [[ -n "$OUT_DEVICE" ]]; then
    # Safety: device must exist, be a block device, and be
    # explicitly confirmed before any writes.
    [[ -e "$OUT_DEVICE" ]] || die "device not found: $OUT_DEVICE"
    if [[ "$(uname -s)" == "Darwin" ]]; then
        [[ "$OUT_DEVICE" =~ ^/dev/(r?disk[0-9]+)$ ]] \
            || die "macOS device must be /dev/diskN or /dev/rdiskN"
        DISKID="${BASH_REMATCH[1]#r}"
        # Prefer the raw device on macOS — 5-10x faster.
        RAW="/dev/r${DISKID}"
        echo ">> target : $OUT_DEVICE → will write through $RAW"
        echo ">> $(diskutil info "/dev/$DISKID" \
                   | grep -E '(Device.*(Identifier|Node)|Media Name|Disk Size)' \
                   | sed 's/^/     /')"
        echo ""
        read -r -p "Unmount and write image to $RAW? [type YES] " CONFIRM
        [[ "$CONFIRM" == "YES" ]] || die "aborted"
        diskutil unmountDisk "/dev/$DISKID"
        sudo dd if="$FINAL_IMG" of="$RAW" bs=4m
        diskutil eject "/dev/$DISKID"
    else
        [[ -b "$OUT_DEVICE" ]] || die "not a block device: $OUT_DEVICE"
        echo ">> target : $OUT_DEVICE"
        echo ">> $(lsblk -o NAME,SIZE,MODEL "$OUT_DEVICE" 2>/dev/null \
                   | sed 's/^/     /')"
        read -r -p "Write image to $OUT_DEVICE? [type YES] " CONFIRM
        [[ "$CONFIRM" == "YES" ]] || die "aborted"
        sudo dd if="$FINAL_IMG" of="$OUT_DEVICE" bs=4M status=progress conv=fsync
        sync
    fi
    mv "$FINAL_IMG" "${LAPEE_ROOT}/work/lapee-usb-last.img"
    echo ""
    echo "=========================================================="
    echo ">> $OUT_DEVICE ready. Image saved at work/lapee-usb-last.img."
    echo "=========================================================="
fi
