#!/usr/bin/env bash
# swtpm.sh — manage the software TPM 2.0 instance for LapEE development.
#
# Starts swtpm in socket/TCP mode so the Docker tools container and QEMU
# guest can both reach it. State lives in ./work/tpm-state/.
#
# Commands:
#   start   — initialize state if needed and start swtpm as a background process
#   stop    — kill the running swtpm
#   status  — check whether swtpm is running
#   reset   — wipe TPM state (destroys keys, resets PCRs)
#   chardev — path to the QEMU chardev socket for -chardev socket,id=chrtpm,...

set -euo pipefail

LAPEE_ROOT="${LAPEE_ROOT:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"
STATE_DIR="${LAPEE_ROOT}/work/tpm-state"
PID_FILE="${STATE_DIR}/swtpm.pid"
CTRL_PORT="${SWTPM_CTRL_PORT:-2322}"
TPM_PORT="${SWTPM_TPM_PORT:-2321}"
LOG_FILE="${STATE_DIR}/swtpm.log"

mkdir -p "${STATE_DIR}"

cmd="${1:-status}"

case "${cmd}" in
    start)
        if [[ -f "${PID_FILE}" ]] && kill -0 "$(cat "${PID_FILE}")" 2>/dev/null; then
            echo "swtpm already running (pid $(cat "${PID_FILE}"))"
            exit 0
        fi
        # Initial setup if state dir is empty.
        if [[ ! -f "${STATE_DIR}/tpm2-00.permall" ]]; then
            swtpm_setup --tpm2 --tpmstate "${STATE_DIR}" --create-ek-cert \
                --create-platform-cert --allow-signing --not-overwrite \
                --display >"${STATE_DIR}/setup.log" 2>&1 || {
                echo "swtpm_setup failed; see ${STATE_DIR}/setup.log"
                exit 1
            }
        fi
        # Start in TCP mode so tools and QEMU can talk to it.
        swtpm socket \
            --tpm2 \
            --tpmstate dir="${STATE_DIR}" \
            --server type=tcp,port="${TPM_PORT}" \
            --ctrl type=tcp,port="${CTRL_PORT}" \
            --flags not-need-init,startup-clear \
            --log "file=${LOG_FILE},level=5" \
            --daemon \
            --pid "file=${PID_FILE}"
        sleep 0.3
        if kill -0 "$(cat "${PID_FILE}")" 2>/dev/null; then
            echo "swtpm started (pid $(cat "${PID_FILE}"), TPM tcp/${TPM_PORT}, ctrl tcp/${CTRL_PORT})"
        else
            echo "swtpm failed to start; see ${LOG_FILE}"
            exit 1
        fi
        ;;
    stop)
        if [[ -f "${PID_FILE}" ]]; then
            pid="$(cat "${PID_FILE}")"
            if kill -0 "${pid}" 2>/dev/null; then
                kill "${pid}"
                sleep 0.2
                echo "swtpm stopped (was pid ${pid})"
            fi
            rm -f "${PID_FILE}"
        else
            echo "swtpm not running"
        fi
        ;;
    status)
        if [[ -f "${PID_FILE}" ]] && kill -0 "$(cat "${PID_FILE}")" 2>/dev/null; then
            echo "running (pid $(cat "${PID_FILE}"))"
        else
            echo "stopped"
        fi
        ;;
    reset)
        "$0" stop
        rm -rf "${STATE_DIR}"
        mkdir -p "${STATE_DIR}"
        echo "swtpm state wiped"
        ;;
    chardev)
        echo "socket,id=chrtpm,host=127.0.0.1,port=${TPM_PORT}"
        ;;
    *)
        echo "usage: $0 {start|stop|status|reset|chardev}" >&2
        exit 1
        ;;
esac
