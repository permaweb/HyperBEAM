#!/usr/bin/env bash
# hb-peer-probe.sh — is the peer currently attesting correctly?
#
# Independent of the HB verifier (which needs a full BEAM + LMDB). Uses
# the Python reference verifier directly against a live peer.
#
# Value: this script ALWAYS works as long as `python3 + openssl` are
# available, so when the HB-side verifier has runtime issues (macOS
# memory pressure, LMDB env_open fails, etc.) the operator still has
# a way to answer "is the peer healthy?".
#
# Usage:
#     ./scripts/hb-peer-probe.sh [PEER_URL]
#
# PEER_URL defaults to http://127.0.0.1:18734 (the boot-hb.sh hostfwd).
#
# Exit 0 iff the peer is reachable AND its current attestation verifies
# cleanly against a just-captured CA.
set -euo pipefail
cd "$(dirname "$0")/.."

PEER=${1:-http://127.0.0.1:18734}
ENV=/tmp/hb-peer-probe-env.json
CA=/tmp/hb-peer-probe-ca.crt

echo "=== probing peer: $PEER ==="

# 1. Fetch the live attestation envelope (accept-bundle so there's no
#    body+link we'd need another node's cache to resolve).
HTTP=$(curl -fsS -m 180 \
    -H 'accept: application/json@1.0' \
    -H 'accept-bundle: true' \
    "$PEER/~tpm2@2.0a/attestation" \
    -o "$ENV" \
    -w '%{http_code}' || echo FAIL)
if [[ "$HTTP" != "200" ]]; then
    echo "FAIL: peer /attestation returned HTTP=$HTTP"
    exit 1
fi
SIZE=$(wc -c <"$ENV" | tr -d ' ')
echo "  attestation fetched: $SIZE bytes"

# 2. The CA we should verify against is whichever this peer's EK cert
#    chains to. In the dev setup, boot-hb.sh captures it to
#    `out/test-tpm-ca.crt' immediately after the peer's first
#    /attestation succeeds. If the peer is a local boot-hb.sh guest,
#    that file is authoritative.
if [[ -s out/test-tpm-ca.crt ]]; then
    cp out/test-tpm-ca.crt "$CA"
    CA_SOURCE="out/test-tpm-ca.crt (local boot-hb.sh capture)"
else
    echo "FAIL: no CA available in out/test-tpm-ca.crt — this script "\
         "is only useful against a peer that our own boot-hb.sh booted."
    exit 1
fi
echo "  trust anchor: $CA_SOURCE"

# 3. Verify with the Python reference verifier (openssl under the hood).
echo ""
echo "=== Python reference verifier ==="
if python3 reference-demo/verifier/verifier_hb.py "$ENV" "$CA"; then
    echo ""
    echo "=== PEER-PROBE: PASS ==="
    echo "peer $PEER is attesting correctly against the currently-"
    echo "captured CA. Cross-node verify-peer via HB should also"
    echo "succeed once the verifier's runtime environment is available."
    exit 0
else
    echo ""
    echo "=== PEER-PROBE: FAIL ==="
    echo "peer $PEER responded, but its current attestation does"
    echo "NOT verify against $CA_SOURCE. Either the peer re-booted"
    echo "after the CA was captured (refresh with ./scripts/boot-hb.sh)"
    echo "or there's a genuine attestation problem."
    exit 1
fi
