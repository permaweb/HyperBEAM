#!/usr/bin/env bash
# tools.sh — wrap Linux-only CLIs inside the lapee-tools Docker image.
#
# Usage:
#   source scripts/tools.sh
#   lapee_tool tpm2_quote -Q -c ak.ctx -l sha256:15 -q $nonce -m quote.msg -s quote.sig -f plain
#   lapee_tool sbsign --key db.key --cert db.crt --output uki.efi uki.raw
#
# The container mounts ./work as /work and ./out as /out so files written inside
# land in our filesystem. TPM calls go via TCP to the host swtpm on :2321/:2322.

LAPEE_ROOT="${LAPEE_ROOT:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"
LAPEE_IMAGE="${LAPEE_IMAGE:-lapee-tools:latest}"

lapee_tool() {
    docker run --rm \
        -v "${LAPEE_ROOT}/work":/work \
        -v "${LAPEE_ROOT}/out":/out \
        -v "${LAPEE_ROOT}/scripts":/scripts:ro \
        -e TPM2TOOLS_TCTI="swtpm:host=host.docker.internal,port=2321" \
        --network=host \
        -w /work \
        "${LAPEE_IMAGE}" "$@"
}

# Interactive shell inside the tools image for debugging.
lapee_shell() {
    docker run --rm -it \
        -v "${LAPEE_ROOT}/work":/work \
        -v "${LAPEE_ROOT}/out":/out \
        -v "${LAPEE_ROOT}/scripts":/scripts:ro \
        -e TPM2TOOLS_TCTI="swtpm:host=host.docker.internal,port=2321" \
        --network=host \
        -w /work \
        "${LAPEE_IMAGE}" /bin/bash
}

export -f lapee_tool lapee_shell
