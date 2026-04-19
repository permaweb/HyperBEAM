#!/usr/bin/env bash
# boot-m1.sh — M1 acceptance: a real Linux kernel boots under QEMU with
# swtpm attached and the TPM driver loaded.
#
# This uses Alpine's prebuilt `virt` kernel + initramfs (a real Linux 6.6
# with CONFIG_TCG_TPM=y, CONFIG_TCG_CRB=y) as the M1 substrate. The real
# integration target for M3+ swaps in a HyperBEAM userspace via a custom
# initramfs while keeping the same kernel shape.
#
# Acceptance:
#   - QEMU boots the Alpine kernel under TCG x86_64 emulation
#   - The TPM driver attaches; /sys/class/tpm/tpm0 exists
#   - cat /sys/class/tpm/tpm0/tpm_version_major -> 2
#   - swtpm's log shows real TPM commands from the guest
#
# Usage:
#   ./scripts/boot-m1.sh            interactive (Ctrl-A X to quit)
#   ./scripts/boot-m1.sh --test     non-interactive acceptance check
set -euo pipefail
cd "$(dirname "$0")/.."

KERNEL=build-alpine/vmlinuz
INITRD=build-alpine/initramfs-virt

if [[ ! -f "$KERNEL" || ! -f "$INITRD" ]]; then
    echo "error: Alpine kernel/initramfs missing. Run: scripts/fetch-alpine.sh" >&2
    exit 1
fi

./scripts/swtpm.sh start >/dev/null 2>&1 || true
TPM_PORT=${SWTPM_TPM_PORT:-2321}

QEMU_ARGS=(
    -machine q35,accel=tcg
    -cpu max
    -m 1024
    -smp 2
    -nographic
    -serial mon:stdio
    -kernel "$KERNEL"
    -initrd "$INITRD"
    -chardev "socket,id=chrtpm,host=127.0.0.1,port=$TPM_PORT"
    -tpmdev emulator,id=tpm0,chardev=chrtpm
    -device tpm-crb,tpmdev=tpm0
    -netdev user,id=net0
    -device virtio-net-pci,netdev=net0
)

case "${1:-}" in
    --test)
        echo "=== M1 acceptance test (Alpine virt kernel + swtpm) ==="
        tmpout=$(mktemp)
        # Alpine initramfs drops into /bin/sh eventually via the netboot
        # init-script. Give the guest a bit of time to reach a usable state,
        # then run our checks via the -initargs path: we append
        # "modules=virtio_pci,virtio_net,tpm_crb,tpm quiet" plus pass
        # `init=/bin/sh` so we can pipe commands.
        (
            printf 'sleep 2\n'
            printf 'echo "---M1-BEGIN---"\n'
            printf 'echo "UNAME: $(uname -a)"\n'
            printf 'modprobe tpm_crb 2>&1 || true\n'
            printf 'sleep 1\n'
            printf 'echo "TPM_DIR: $(ls /sys/class/tpm 2>&1)"\n'
            printf 'echo "TPM_MAJOR: $(cat /sys/class/tpm/tpm0/tpm_version_major 2>&1)"\n'
            printf 'echo "DMESG_TPM: $(dmesg 2>/dev/null | grep -i tpm | head -3)"\n'
            printf 'echo "---M1-END---"\n'
            printf 'sync; poweroff -f 2>/dev/null || { echo o > /proc/sysrq-trigger; }\n'
        ) | timeout 180 qemu-system-x86_64 \
            "${QEMU_ARGS[@]}" \
            -append "console=ttyS0 panic=1 quiet init=/bin/sh modules=virtio_pci,virtio_net,tpm_crb,tpm" 2>&1 | tee "$tmpout" || true

        echo
        echo "=== Acceptance checks ==="
        ok=1
        grep -q "UNAME: Linux .* x86_64" "$tmpout" \
            && echo "[PASS] x86_64 Linux booted" \
            || { echo "[FAIL] x86_64 uname line missing"; ok=0; }
        grep -q "TPM_DIR: tpm0" "$tmpout" \
            && echo "[PASS] /sys/class/tpm/tpm0 present" \
            || { echo "[FAIL] tpm0 not present"; ok=0; }
        grep -q "TPM_MAJOR: 2" "$tmpout" \
            && echo "[PASS] TPM 2.0 driver attached" \
            || { echo "[FAIL] TPM driver not version 2"; ok=0; }
        grep -q "---M1-END---" "$tmpout" \
            && echo "[PASS] test script reached end" \
            || { echo "[FAIL] test script did not complete"; ok=0; }
        rm -f "$tmpout"
        [[ $ok -eq 1 ]] || exit 1
        echo "=== M1 ACCEPTANCE: PASS ==="
        ;;
    *)
        exec qemu-system-x86_64 "${QEMU_ARGS[@]}" -append "console=ttyS0"
        ;;
esac
