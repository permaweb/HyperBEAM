#!/usr/bin/env bash
# boot-hb.sh — boot the LapEE HyperBEAM guest under QEMU + swtpm and
# drive the end-to-end attestation chain that this project is here to
# prove:
#
#   kernel (debian/buildroot)
#       -> /init  (initramfs-hb/init)
#       -> exec hb foreground
#          -> on.start hook fires: `~tpm2@2.0a/extend' with the running
#             node message as `subject'.
#          -> PCR 15 is extended with `hb_message:id(NodeMsg, all, Opts)'
#             via `hb_util:native_id/1'.
#       -> HB HTTP server comes up on :8734 (HB default).
#       -> Host hits `/~tpm2@2.0a/attestation' and gets an envelope
#          that chains EK cert -> AK -> quote -> PCR 15 -> event log
#          -> node_message -> wallet.
#
# Usage:
#   ./scripts/boot-hb.sh                 accept-test, non-interactive
#   ./scripts/boot-hb.sh --user CFG      inject a user config (flat path)
#   ./scripts/boot-hb.sh --shell         drop into guest shell instead
#
# Prereqs:
#   - build-alpine/vmlinuz-debian             Debian linux-image-amd64
#   - work/initramfs-hb.cpio.gz               our HB-bearing initramfs
#   - swtpm, qemu-system-x86_64, openssl, curl
set -euo pipefail
cd "$(dirname "$0")/.."

# Default to the Buildroot-built LapEE kernel (virtio_net + tpm_tis +
# IMA all =y in-tree), falling back to the Debian `linux-image-amd64'
# if the Buildroot artefact hasn't been produced yet. The Debian kernel
# ships virtio_net as a module that our thin initramfs can't load,
# so HB's HTTP port is unreachable through slirp there — use the
# Buildroot kernel whenever it exists.
KERNEL=${KERNEL:-build-alpine/vmlinuz-lapee}
if [[ ! -f "$KERNEL" ]]; then
    KERNEL=build-alpine/vmlinuz-debian
fi
INITRD=${INITRD:-work/initramfs-hb.cpio.gz}
HOST_HTTP_PORT=${HOST_HTTP_PORT:-18734}   # host -> guest :8734
LOGFILE=${LOGFILE:-/tmp/lapee-hb-guest.log}
USER_CFG=""
MODE=attest
USER_B64=""

KEEP_ALIVE=0
while (( $# > 0 )); do
    case "$1" in
        --user)   USER_CFG=$2; shift 2;;
        --user-b64) USER_B64=$2; shift 2;;
        --shell)  MODE=shell; shift;;
        --diag)   MODE=diag; shift;;
        --log)    LOGFILE=$2; shift 2;;
        --port)   HOST_HTTP_PORT=$2; shift 2;;
        --keep-alive) KEEP_ALIVE=1; shift;;
        *)        echo "unknown arg: $1"; exit 2;;
    esac
done

if [[ ! -f "$KERNEL" ]]; then
    echo "missing $KERNEL — run: make fetch-debian-kernel" >&2
    exit 1
fi
if [[ ! -f "$INITRD" ]]; then
    echo "missing $INITRD — run: scripts/build-initramfs-hb.sh" >&2
    exit 1
fi

# Fresh swtpm for each run.
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

# Compose kernel cmdline. LAPEE_HB_VERBOSE=1 (opt-in) turns on HB
# startup tracing in init-hb. It is noisy — only enable for debugging.
CMDLINE="console=ttyS0 panic=10 ima_policy=tcb rdinit=/init"
if [[ "${LAPEE_HB_VERBOSE:-}" == "1" ]]; then
    CMDLINE="$CMDLINE LAPEE_HB_VERBOSE=1"
fi
if [[ "$MODE" == "diag" ]]; then
    # In diag mode the guest runs a netcat listener on 8734 before HB
    # starts, so we can separate "slirp hostfwd works" from "HB handles
    # requests". init-hb drops to /bin/sh after the diag window.
    CMDLINE="$CMDLINE LAPEE_HB_DIAG=1"
fi
if [[ -n "$USER_B64" ]]; then
    CMDLINE="$CMDLINE lapee.user_b64=${USER_B64}"
elif [[ -n "$USER_CFG" ]]; then
    CMDLINE="$CMDLINE lapee.user_b64=$(base64 -i "$USER_CFG" | tr -d '\n')"
fi

# QEMU args. The guest's init generates a test CA inside the initramfs
# and emits it on serial between `---LAPEE-CA-BEGIN/END---' markers so
# we can retrieve it here.
QEMU_ARGS=(
    -machine q35,accel=tcg
    # qemu64 is the baseline x86_64 CPU — Rosetta has been seen
    # cascading into odd BEAM behaviour under `-cpu max' (advertised
    # features Rosetta does not actually support), so we pick the
    # minimum viable CPU.
    -cpu qemu64,+rdtscp,+ssse3,+sse4.1,+sse4.2,+avx
    -m 2048 -smp 4 -nographic
    -kernel "$KERNEL" -initrd "$INITRD"
    -append "$CMDLINE"
    -chardev "socket,id=chrtpm,path=$(pwd)/work/tpm-qemu/swtpm-sock"
    -tpmdev emulator,id=tpm0,chardev=chrtpm
    -device tpm-tis,tpmdev=tpm0
    -netdev "user,id=net0,hostfwd=tcp:127.0.0.1:${HOST_HTTP_PORT}-:8734"
    -device virtio-net-pci,netdev=net0
)

if [[ "$MODE" == "shell" ]]; then
    exec qemu-system-x86_64 -serial mon:stdio "${QEMU_ARGS[@]}"
fi

if [[ "$MODE" == "diag" ]]; then
    echo "=== DIAG boot: guest runs nc -l 8734 for 20s before HB starts ==="
    qemu-system-x86_64 "${QEMU_ARGS[@]}" > "$LOGFILE" 2>&1 &
    QEMUPID=$!
    trap 'kill $QEMUPID 2>/dev/null || true; kill $(cat work/tpm-qemu/swtpm.pid 2>/dev/null) 2>/dev/null || true' EXIT

    # Wait for the guest to reach `LAPEE-DIAG-BEGIN' on the serial log,
    # then fire a probe request at the hostfwd. Slirp should forward it
    # to the guest's nc listener, whose stdout appears on the serial
    # console prefixed with `[diag-recv]'.
    for i in $(seq 1 180); do
        if grep -q 'LAPEE-DIAG-BEGIN' "$LOGFILE" 2>/dev/null; then
            echo "=== guest ready: firing probe curl http://127.0.0.1:${HOST_HTTP_PORT}/diag-probe ==="
            curl -sv -m 5 -o /dev/null \
                "http://127.0.0.1:${HOST_HTTP_PORT}/diag-probe?from=host" \
                2>&1 | sed 's/^/[host-curl] /' | head -20 || true
            break
        fi
        if ! kill -0 $QEMUPID 2>/dev/null; then
            echo "!! qemu exited before DIAG window"
            tail -60 "$LOGFILE"
            exit 1
        fi
        sleep 2
    done

    # Wait for the diag window to close (LAPEE-DIAG-END on serial), then
    # dump the interesting parts of the log.
    for i in $(seq 1 60); do
        if grep -q 'LAPEE-DIAG-END' "$LOGFILE" 2>/dev/null; then break; fi
        sleep 1
    done
    echo ""
    echo "=== diag log excerpt ==="
    sed -n '/LAPEE-DIAG-BEGIN/,/LAPEE-DIAG-END/p' "$LOGFILE" | tail -40
    echo ""
    # Leave qemu running so the user (or a follow-up script) can poke
    # the shell interactively if needed; trap kills it on exit.
    kill $QEMUPID 2>/dev/null || true
    wait $QEMUPID 2>/dev/null || true
    exit 0
fi

echo "=== booting LapEE HB guest (log: $LOGFILE, http: :$HOST_HTTP_PORT) ==="
qemu-system-x86_64 "${QEMU_ARGS[@]}" > "$LOGFILE" 2>&1 &
QEMUPID=$!
trap 'kill $QEMUPID 2>/dev/null || true; kill $(cat work/tpm-qemu/swtpm.pid 2>/dev/null) 2>/dev/null || true' EXIT

# Wait for HB HTTP to come up (or qemu to die). The first responses
# from cowboy under Rosetta emulation can take several seconds; use a
# generous per-request timeout and require two consecutive 200s before
# we declare "ready" so we don't rush into the attestation fetch while
# HB is still finishing its startup pipeline.
echo "qemu pid $QEMUPID; waiting for HB HTTP on :$HOST_HTTP_PORT (up to 15 min)..."
READY=0
STREAK=0
for i in $(seq 1 180); do
    if ! kill -0 $QEMUPID 2>/dev/null; then
        echo "!! qemu exited before HB came up"
        tail -60 "$LOGFILE"
        exit 1
    fi
    CODE=$(curl -fsS -m 8 -o /dev/null -w '%{http_code}' \
        "http://127.0.0.1:${HOST_HTTP_PORT}/~meta@1.0/info" 2>/dev/null \
        || echo "TO")
    if [[ "$CODE" == "200" ]]; then
        STREAK=$((STREAK + 1))
        if [[ $STREAK -ge 2 ]]; then
            READY=1
            echo "HB HTTP responding (streak=$STREAK) after ~${i}*5s"
            break
        fi
    else
        STREAK=0
    fi
    sleep 5
done
if [[ $READY -ne 1 ]]; then
    echo "!! HB HTTP never came up"
    tail -80 "$LOGFILE"
    exit 1
fi

# Fetch attestation envelope via the standard AO-Core content
# negotiation: `accept: application/json@1.0' picks the JSON codec,
# `accept-bundle: true' asks HB to resolve the response's inner body
# inline instead of leaving it as a `body+link' cache reference.
# First TPM2_CreatePrimary + the AK policy+create chain on an
# emulated TPM under Rosetta typically takes 30-60s, so the timeout
# is generous.
mkdir -p out
ENV=out/attestation.json
echo "=== GET /~tpm2@2.0a/attestation ==="
curl -fsS -m 180 \
    -H 'accept: application/json@1.0' \
    -H 'accept-bundle: true' \
    "http://127.0.0.1:${HOST_HTTP_PORT}/~tpm2@2.0a/attestation" \
    -o "$ENV"
ls -lh "$ENV"

# Extract the CA the guest emitted on serial between LAPEE-CA markers.
# The verifier uses this to check the EK cert chain.
sed -n '/^---LAPEE-CA-BEGIN---/,/^---LAPEE-CA-END---/p' "$LOGFILE" \
    | sed '1d;$d' > out/test-tpm-ca.crt
if [[ ! -s out/test-tpm-ca.crt ]]; then
    echo "!! failed to extract CA from serial log" >&2
fi

if [[ $KEEP_ALIVE -eq 1 ]]; then
    echo ""
    echo "=== keeping guest alive (--keep-alive) ==="
    echo "qemu pid = $QEMUPID; swtpm pid file = work/tpm-qemu/swtpm.pid"
    echo "HB HTTP on 127.0.0.1:${HOST_HTTP_PORT}"
    echo "kill with: kill $QEMUPID; kill \$(cat work/tpm-qemu/swtpm.pid)"
    # Disable the EXIT trap so a plain `exit' here doesn't kill qemu.
    trap - EXIT
    exit 0
fi

# Graceful shutdown via HB HTTP if possible, else SIGTERM qemu.
curl -fsS -m 5 "http://127.0.0.1:${HOST_HTTP_PORT}/~meta@1.0/stop" -o /dev/null 2>/dev/null || true
sleep 2
kill $QEMUPID 2>/dev/null || true
wait $QEMUPID 2>/dev/null || true

echo ""
echo "=== captured ==="
ls -lh out/attestation.json out/test-tpm-ca.crt
echo ""
echo "next: python3 reference-demo/verifier/verifier_hb.py \\"
echo "         out/attestation.json out/test-tpm-ca.crt"
