#!/usr/bin/env bash
# Keep a HyperBEAM node in sync with live Arweave mainnet, validating every
# block, and record what happened.
#
#   scripts/arweave-live-sync.sh [duration-seconds] [log-path]
#
# Defaults to 3900 seconds (65 minutes) so that a full hour of steady-state
# sync is recorded even after bootstrap has eaten into the start.
#
# Notes on why this is shaped the way it is:
#   - `rebar3 shell --eval' silently drops multi-line scripts in a non-TTY, so
#     the eval below is a single line and all real logic lives in the module.
#   - stdin is fed from a `tail -f' on /dev/null so the shell does not exit at
#     EOF; the module halts itself when its deadline passes.
#   - The harness writes JSON lines to the log AND to stdout, so the run can be
#     followed live and audited afterwards.
set -euo pipefail

cd "$(dirname "$0")/.."

DURATION="${1:-3900}"
LOG="${2:-evidence/live-sync.log}"
PEERS="${ARWEAVE_PEERS:-http://tip-1.arweave.xyz:1984,http://tip-2.arweave.xyz:1984,http://tip-3.arweave.xyz:1984,http://tip-4.arweave.xyz:1984}"

mkdir -p "$(dirname "$LOG")"

echo "duration : ${DURATION}s"
echo "log      : ${LOG}"
echo "peers    : ${PEERS}"

# Refuse to write over evidence that is already committed. The harness's default
# output path used to be the same path the acceptance evidence lived on, so a
# later run silently overwrote the thing it was meant to corroborate.
if git ls-files --error-unmatch "${LOG}" >/dev/null 2>&1; then
    echo "refusing to overwrite ${LOG}: it is already tracked by git." >&2
    echo "pass a different log path, or archive the committed one first." >&2
    exit 1
fi
echo

# Record the network's view at the moment we start, so the log can be checked
# against something independent of the node under test.
echo "network at start:"
curl -sf -m 15 "http://tip-1.arweave.xyz:1984/info" | tee "${LOG}.network-start.json"
echo

ARGS="#{<<\"duration\">> => <<\"${DURATION}\">>, <<\"log\">> => <<\"${LOG}\">>, <<\"peers\">> => <<\"${PEERS}\">>}"

# `rebar3 shell' exits the moment stdin closes, so it is fed from a `tail -f'.
# That tail must not outlive it: as a plain pipeline member it keeps the whole
# pipeline -- and therefore this script -- alive after the beam has died, so a
# node that crashes at boot leaves a process that looks healthy and never
# returns. It is backgrounded through a FIFO and killed on exit instead, and
# the beam's own status is what this script reports.
FIFO="$(mktemp -u)"
mkfifo "${FIFO}"
tail -f /dev/null > "${FIFO}" &
TAIL_PID=$!
trap 'kill "${TAIL_PID}" 2>/dev/null; rm -f "${FIFO}"' EXIT

set +e
rebar3 shell \
    --eval "hb_arweave_live_sync:run(${ARGS}), halt(0)." \
    < "${FIFO}" 2>&1 | tee -a "${LOG}.console"
RC=${PIPESTATUS[0]}
set -e

echo
echo "beam exited rc=${RC}"
echo "network at end:"
curl -sf -m 15 "http://tip-1.arweave.xyz:1984/info" | tee "${LOG}.network-end.json"

exit "${RC}"
