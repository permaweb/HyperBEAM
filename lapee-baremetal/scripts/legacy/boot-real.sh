#!/usr/bin/env bash
# boot-real.sh — boot a REAL Linux guest (Debian kernel) under QEMU with
# swtpm attached, run HyperBEAM's attestation orchestrator inside the
# guest, capture the resulting signed attestation from the serial log.
#
# Prereqs: docker, swtpm, qemu-system-x86_64, base64, and:
#   - build-alpine/vmlinuz-debian          Debian linux-image-amd64 (TPM+IMA built-in)
#   - work/initramfs-mini.cpio.gz          our minimal rootfs with BEAM + NIF
#   - work/tpm-qemu/swtpm-sock             unix-socket swtpm for QEMU's emulator TPM
set -euo pipefail
cd "$(dirname "$0")/.."

# Kernel + initramfs are overridable; defaults are the Debian kernel + our
# HB-in-initramfs build that has been working end-to-end for this project.
#
# To use the Buildroot-built kernel + initramfs instead:
#   KERNEL=build-alpine/vmlinuz-lapee INITRD=work/initramfs-lapee-hb.cpio.gz \
#       ./scripts/boot-real.sh
#
# (See BUILDROOT-RESULT.md for how vmlinuz-lapee is produced and
# scripts/overlay-hyperbeam.sh for how the HB overlay is grafted onto the
# Buildroot initramfs.)
KERNEL=${KERNEL:-build-alpine/vmlinuz-debian}
INITRD=${INITRD:-work/initramfs-mini.cpio.gz}

if [[ ! -f "$KERNEL" ]]; then
    echo "Missing kernel $KERNEL."
    echo "  For Debian-kernel flow: make fetch-debian-kernel"
    echo "  For Buildroot-kernel flow: make buildroot && scripts/collect-buildroot-artefacts.sh"
    exit 1
fi
if [[ ! -f "$INITRD" ]]; then
    echo "Missing initramfs $INITRD."
    echo "  For HB-in-initramfs: make initramfs"
    echo "  For Buildroot+HB overlay: scripts/overlay-hyperbeam.sh"
    exit 1
fi

# Fresh swtpm in unix-socket-ctrl mode (QEMU's emulator TPM backend needs
# CMD_SET_DATAFD via unix socket).
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

# Boot under TCG emulation (Apple Silicon + Rosetta; KVM unavailable).
LOGFILE=${LOGFILE:-/tmp/lapee-guest.log}
echo "=== booting LapEE guest (log: $LOGFILE) ==="
qemu-system-x86_64 \
    -machine q35 -m 2048 -smp 4 -nographic \
    -kernel "$KERNEL" -initrd "$INITRD" \
    -append "console=ttyS0 panic=10 ima_policy=tcb rdinit=/init" \
    -chardev "socket,id=chrtpm,path=$(pwd)/work/tpm-qemu/swtpm-sock" \
    -tpmdev emulator,id=tpm0,chardev=chrtpm \
    -device tpm-tis,tpmdev=tpm0 \
    -netdev user,id=net0 -device virtio-net-pci,netdev=net0 \
    > "$LOGFILE" 2>&1 &
QEMUPID=$!
echo "qemu pid $QEMUPID; waiting for attestation (up to 10 minutes)..."

# Wait for guest to emit attestation or die
for i in $(seq 1 120); do
    if grep -q "LAPEE-ATTESTATION-END" "$LOGFILE" 2>/dev/null; then
        echo "attestation emitted"; break
    fi
    if ! kill -0 $QEMUPID 2>/dev/null; then
        echo "qemu exited before emitting attestation"
        tail -20 "$LOGFILE"
        exit 1
    fi
    sleep 5
done

# Guest should poweroff shortly after emit; force-kill just in case.
wait $QEMUPID 2>/dev/null || true
kill $QEMUPID 2>/dev/null || true

# Extract from serial log
mkdir -p out
sed -n '/^---LAPEE-ATTESTATION-BEGIN---/,/^---LAPEE-ATTESTATION-END---/p' "$LOGFILE" \
    | sed '1d;$d' | base64 -D > out/attestation.json
sed -n '/^---LAPEE-CA-BEGIN---/,/^---LAPEE-CA-END---/p' "$LOGFILE" \
    | sed '1d;$d' > out/test-tpm-ca.crt
ADDR=$(grep -oE 'LAPEE-NODE-ADDRESS: [0-9a-f]+' "$LOGFILE" | head -1 | awk '{print $2}')
echo "$ADDR" > out/node-address.txt

echo ""
echo "=== attestation extracted ==="
ls -lh out/attestation.json out/test-tpm-ca.crt out/node-address.txt
echo "node address: $ADDR"
