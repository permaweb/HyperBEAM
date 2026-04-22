#!/usr/bin/env bash
# boot-usb-image.sh — boot the LapEE USB image under QEMU+OVMF+
# swtpm. This is the same artefact we'd write to a USB stick for
# Framework native boot; booting it in QEMU gives us high-
# confidence validation that the image is correct (UEFI +
# FAT32 ESP + \EFI\Boot\BootX64.efi + UKI + kernel + init +
# writeback logic) before we hand it to hardware.
#
# The image is copied to a scratch path first so the run-time
# writeback does not mutate the original. When the guest's init
# emits LAPEE-WRITEBACK-OK we consider the boot successful and
# copy the written attestation JSON out of the scratch image.
#
# Usage:
#   ./scripts/boot-usb-image.sh
#   ./scripts/boot-usb-image.sh --img work/lapee-usb.img
#   ./scripts/boot-usb-image.sh --timeout 600   (seconds)

set -euo pipefail
cd "$(dirname "$0")/.."

IMG=${IMG:-work/lapee-usb.img}
TIMEOUT=${TIMEOUT:-420}
LOGFILE=${LOGFILE:-/tmp/lapee-usb-qemu.log}

while (($# > 0)); do
    case "$1" in
        --img)     IMG=$2; shift 2;;
        --timeout) TIMEOUT=$2; shift 2;;
        --log)     LOGFILE=$2; shift 2;;
        *) echo "unknown arg: $1" >&2; exit 2;;
    esac
done

[[ -f "$IMG" ]] || { echo "no $IMG (run: make hb-usb-image)" >&2; exit 1; }

OVMF_CODE=/opt/homebrew/opt/qemu/share/qemu/edk2-x86_64-code.fd
OVMF_VARS_TEMPLATE=/opt/homebrew/opt/qemu/share/qemu/edk2-i386-vars.fd
for f in "$OVMF_CODE" "$OVMF_VARS_TEMPLATE"; do
    [[ -f "$f" ]] || { echo "missing OVMF firmware: $f" >&2; exit 1; }
done

# Scratch copies so we don't mutate the source image or NVRAM.
mkdir -p work/qemu-usb
SCRATCH_IMG=work/qemu-usb/scratch.img
SCRATCH_VARS=work/qemu-usb/vars.fd
cp "$IMG" "$SCRATCH_IMG"
cp "$OVMF_VARS_TEMPLATE" "$SCRATCH_VARS"

# Fresh swtpm.
if [[ -f work/tpm-qemu/swtpm.pid ]]; then
    kill "$(cat work/tpm-qemu/swtpm.pid)" 2>/dev/null || true
fi
rm -rf work/tpm-qemu && mkdir -p work/tpm-qemu
swtpm socket --tpm2 --tpmstate dir=work/tpm-qemu \
    --ctrl type=unixio,path="$(pwd)/work/tpm-qemu/swtpm-sock" \
    --flags not-need-init,startup-clear \
    --log "file=work/tpm-qemu/swtpm.log,level=5" \
    --daemon --pid "file=work/tpm-qemu/swtpm.pid"
sleep 1

echo "=== booting $SCRATCH_IMG under QEMU+OVMF+swtpm ==="
echo "    log: $LOGFILE  (timeout: ${TIMEOUT}s)"

# QEMU invocation. The image boots via \EFI\Boot\BootX64.efi so
# no -kernel / -initrd is needed — UEFI finds and executes the
# UKI itself.
qemu-system-x86_64 \
    -machine q35,accel=tcg \
    -cpu qemu64,+rdtscp,+ssse3,+sse4.1,+sse4.2,+avx \
    -m 2048 -smp 4 -nographic \
    -drive "if=pflash,format=raw,readonly=on,file=${OVMF_CODE}" \
    -drive "if=pflash,format=raw,file=${SCRATCH_VARS}" \
    -drive "file=${SCRATCH_IMG},format=raw,if=virtio" \
    -chardev "socket,id=chrtpm,path=$(pwd)/work/tpm-qemu/swtpm-sock" \
    -tpmdev emulator,id=tpm0,chardev=chrtpm \
    -device tpm-tis,tpmdev=tpm0 \
    -netdev "user,id=net0,hostfwd=tcp:127.0.0.1:18734-:8734" \
    -device virtio-net-pci,netdev=net0 \
    > "$LOGFILE" 2>&1 &
QEMUPID=$!
trap 'kill $QEMUPID 2>/dev/null || true; kill $(cat work/tpm-qemu/swtpm.pid 2>/dev/null) 2>/dev/null || true' EXIT

# Poll for the writeback success marker (or QEMU exit).
deadline=$((SECONDS + TIMEOUT))
while (( SECONDS < deadline )); do
    if grep -q "LAPEE-WRITEBACK-OK" "$LOGFILE" 2>/dev/null; then
        echo ">> LAPEE-WRITEBACK-OK detected in serial log"
        break
    fi
    if ! kill -0 $QEMUPID 2>/dev/null; then
        echo "!! qemu exited before writeback completed" >&2
        tail -60 "$LOGFILE"
        exit 1
    fi
    sleep 2
done

if ! grep -q "LAPEE-WRITEBACK-OK" "$LOGFILE" 2>/dev/null; then
    echo "!! timeout waiting for LAPEE-WRITEBACK-OK" >&2
    echo "!! last 80 lines of serial log:" >&2
    tail -80 "$LOGFILE" >&2
    exit 1
fi

# Give HB a couple more seconds to settle, then terminate QEMU
# and extract the writeback artefacts from the scratch image.
sleep 2
kill $QEMUPID 2>/dev/null || true
wait $QEMUPID 2>/dev/null || true
kill "$(cat work/tpm-qemu/swtpm.pid 2>/dev/null)" 2>/dev/null || true

# Pull the ESP contents back out (without needing to mount on
# macOS host). mtools inside the tools container knows FAT32.
echo "=== extracting writeback artefacts ==="
mkdir -p out/qemu-usb-test
docker run --rm --platform=linux/amd64 \
    -v "$(pwd)":/w \
    lapee-tools:latest \
    bash -euo pipefail -c '
        # Extract ESP partition bytes from the disk image.
        START=$(parted --script --machine /w/work/qemu-usb/scratch.img \
            unit s print | awk -F: "/^1:/ {gsub(\"s\",\"\",\$2); print \$2}")
        SECTORS=$(parted --script --machine /w/work/qemu-usb/scratch.img \
            unit s print | awk -F: "/^1:/ {gsub(\"s\",\"\",\$4); print \$4}")
        dd if=/w/work/qemu-usb/scratch.img \
           of=/w/work/qemu-usb/esp.img \
           bs=512 skip=$START count=$SECTORS status=none
        # List everything on the ESP so we can see writeback artefacts.
        echo ">> ESP contents after boot:"
        mdir -i /w/work/qemu-usb/esp.img ::
        # Copy writeback payload out.
        mcopy -i /w/work/qemu-usb/esp.img \
            ::/attestation-latest.json \
            /w/out/qemu-usb-test/ 2>/dev/null || \
            echo "   (attestation-latest.json not present)"
        mcopy -i /w/work/qemu-usb/esp.img \
            ::/tpm-ca.crt \
            /w/out/qemu-usb-test/ 2>/dev/null || true
        mcopy -i /w/work/qemu-usb/esp.img \
            ::/README-VALIDATOR.txt \
            /w/out/qemu-usb-test/ 2>/dev/null || true
    '

echo ""
echo "=== QEMU boot test PASSED ==="
ls -lh out/qemu-usb-test/
echo ""
echo "Interpret on verifier side:"
echo "  ./scripts/interpret-local-capture.sh \\"
echo "      --label 'QEMU USB image self-test' \\"
echo "      out/qemu-usb-test/attestation-latest.json"
