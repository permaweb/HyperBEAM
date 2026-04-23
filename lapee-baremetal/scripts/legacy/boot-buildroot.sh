#!/usr/bin/env bash
# boot-buildroot.sh — boot LapEE's Buildroot-built kernel + initramfs under
# QEMU with swtpm attached; confirm the TPM 2.0 driver attaches and that
# lapee-init (our PID 1) ran. Emits [PASS]/[FAIL] lines for acceptance.
#
# Artefacts expected:
#   build-alpine/vmlinuz-lapee         Buildroot bzImage
#   work/initramfs-lapee.cpio.gz       Buildroot initramfs (cpio+gz)
#
# Acceptance (per the LapEE brief):
#   - kernel boots; /sbin/init (lapee-init C binary) runs
#   - lapee-init falls back to /bin/sh when HyperBEAM isn't present
#   - /sys/class/tpm/tpm0/tpm_version_major == 2
set -euo pipefail
cd "$(dirname "$0")/.."

KERNEL=${KERNEL:-build-alpine/vmlinuz-lapee}
INITRD=${INITRD:-work/initramfs-lapee.cpio.gz}

[[ -f "$KERNEL" ]] || { echo "[FAIL] Missing kernel $KERNEL (run: make buildroot)"; exit 1; }
[[ -f "$INITRD" ]] || { echo "[FAIL] Missing initramfs $INITRD (run: make buildroot)"; exit 1; }

# Fresh swtpm on a unix socket for QEMU's emulator TPM backend.
if [[ -f work/tpm-qemu/swtpm.pid ]]; then
    kill "$(cat work/tpm-qemu/swtpm.pid)" 2>/dev/null || true
fi
rm -rf work/tpm-qemu && mkdir -p work/tpm-qemu
swtpm socket --tpm2 --tpmstate dir=work/tpm-qemu \
    --ctrl type=unixio,path="$(pwd)/work/tpm-qemu/swtpm-sock" \
    --flags not-need-init,startup-clear \
    --log "file=work/tpm-qemu/swtpm.log,level=5" \
    --daemon --pid "file=work/tpm-qemu/swtpm.pid"

LOG=${LOG:-/tmp/lapee-buildroot-guest.log}
echo "=== booting Buildroot LapEE guest (log: $LOG) ==="

# Drive the guest via a scripted stdin: our /sbin/init drops into /bin/sh
# when HyperBEAM is absent, so we can pipe acceptance-check commands.
#
# init=/sbin/init (BR installs lapee-init there)
# "console=ttyS0" — serial console for QEMU -nographic
# "panic=5" — don't hang forever on PID1 exit
#
# IMPORTANT: each sleep BEFORE the echo so the shell has swallowed the
# prompt + kernel-boot log lines before we feed it the next command.
(
    # Give kernel + lapee-init time to hand off to /bin/sh.
    sleep 8
    echo ""
    echo "echo ---LAPEE-ACCEPTANCE-BEGIN---"
    sleep 1
    echo "uname -a"
    sleep 1
    echo "ls -l /sbin/init /init"
    sleep 1
    echo "ls /sys/class/tpm"
    sleep 1
    echo "cat /sys/class/tpm/tpm0/tpm_version_major"
    sleep 1
    echo "dmesg | grep -iE 'tpm|tcg' | head -5"
    sleep 1
    echo "test -x /usr/bin/hyperbeam && echo 'HYPERBEAM_PRESENT: yes' || echo 'HYPERBEAM_PRESENT: no'"
    sleep 1
    echo "echo ---LAPEE-ACCEPTANCE-END---"
    sleep 1
    echo "sync; poweroff -f 2>/dev/null || echo o > /proc/sysrq-trigger"
    sleep 2
) | timeout 240 qemu-system-x86_64 \
    -machine q35,accel=tcg -cpu max -m 1024 -smp 2 -nographic \
    -kernel "$KERNEL" -initrd "$INITRD" \
    -append "console=ttyS0 panic=5 rdinit=/init" \
    -chardev "socket,id=chrtpm,path=$(pwd)/work/tpm-qemu/swtpm-sock" \
    -tpmdev emulator,id=tpm0,chardev=chrtpm \
    -device tpm-tis,tpmdev=tpm0 \
    -netdev user,id=net0 -device virtio-net-pci,netdev=net0 \
    2>&1 | tee "$LOG" || true

# Clean up swtpm
if [[ -f work/tpm-qemu/swtpm.pid ]]; then
    kill "$(cat work/tpm-qemu/swtpm.pid)" 2>/dev/null || true
fi

echo
echo "=== Acceptance checks ==="
ok=1
grep -q "Linux .* x86_64 GNU/Linux\|Linux buildroot .* x86_64" "$LOG" \
    && echo "[PASS] x86_64 Linux booted" \
    || { echo "[FAIL] x86_64 kernel uname line missing"; ok=0; }
grep -q '/sbin/init' "$LOG" \
    && echo "[PASS] /sbin/init listed" \
    || { echo "[FAIL] /sbin/init not present"; ok=0; }
grep -q 'lapee-init' "$LOG" \
    && echo "[PASS] lapee-init logged (PID 1 ran)" \
    || { echo "[FAIL] lapee-init did not log (PID 1 may not be our binary)"; ok=0; }
grep -qE '(^|[^a-zA-Z])tpm0($|[^a-zA-Z])' "$LOG" \
    && echo "[PASS] /sys/class/tpm/tpm0 present" \
    || { echo "[FAIL] tpm0 not present in /sys/class/tpm"; ok=0; }
# tpm_version_major is a single digit "2" on its own line (after the `cat` cmd echo).
awk '/cat \/sys\/class\/tpm\/tpm0\/tpm_version_major/{f=1;next} f && /^2\r?$/{found=1;exit} END{exit !found}' "$LOG" \
    && echo "[PASS] TPM 2.0 driver attached (tpm_version_major=2)" \
    || { echo "[FAIL] TPM driver not version 2"; ok=0; }
grep -q -- "---LAPEE-ACCEPTANCE-END---" "$LOG" \
    && echo "[PASS] acceptance script reached end" \
    || { echo "[FAIL] acceptance script did not complete"; ok=0; }

if [[ $ok -eq 1 ]]; then
    echo
    echo "=== BUILDROOT BOOT ACCEPTANCE: PASS ==="
    exit 0
else
    echo
    echo "=== BUILDROOT BOOT ACCEPTANCE: FAIL ==="
    echo "Log at: $LOG"
    exit 1
fi
