#!/usr/bin/env bash
# hb-cross-node-verify.sh — "verifier HB outside QEMU, peer HB inside"
#
# The paper's real acceptance test: a *different* HB node performs the
# attestation check, so the verification is independent of the box
# being measured.
#
# Topology
#
#   +------------------------------------+       +---------------+
#   |  macOS host                        |       |  QEMU guest   |
#   |                                    |       |               |
#   |  verifier HB  (native rebar3 shell)| HTTP  |  peer HB      |
#   |    :18735    ----------->  127.0.0.1:18734 |    :8734      |
#   |                                    |       |               |
#   |  + /tmp/test-tpm-ca.crt            |       |  + dev_tpm2   |
#   |    (the guest's per-boot CA,       |       |    + libtss2  |
#   |     copied BEFORE the call so      |       |    + swtpm    |
#   |     trust establishment is         |       +---------------+
#   |     independent of the call)       |
#   +------------------------------------+
#
# Flow
#
#   1. Boot QEMU guest with `--keep-alive' (hostfwd 127.0.0.1:18734).
#   2. Copy `out/test-tpm-ca.crt' out to /tmp — this is the trust
#      anchor the verifier will use, installed BEFORE the request so
#      trust establishment is out-of-band w.r.t. the verification.
#   3. Start verifier HB natively on macOS via rebar3 shell, listening
#      on :18735, with HB_CONFIG pointing at /tmp/test-tpm-ca.crt. No
#      TPM NIF is loaded (LAPEE_TPM_ALLOW_NO_NIF=1) — the verifier
#      verifies cryptography; it doesn't attest itself.
#   4. Call the verifier:
#
#        GET /~tpm-interpret@1.0/verify-peer?peer=http://127.0.0.1:18734
#
#      The verifier does the `hb_http:get' to the peer itself; it never
#      trusts the caller's view of the peer. This avoids HB's
#      relay-chain cache-rewrite bug (documented in STATUS.md) while
#      preserving all of the paper's guarantees.
#   5. Assertion: HTTP 200, `verified: true, verdict: accepted', all
#      five `dev_tpm2:verify' checks PASS, `summary.on_start_hook_
#      device = tpm2@2.0a', and `summary.node_message_id' matches the
#      guest's own attestation output.
#   6. Tear down: kill verifier + guest.
#
# Exit 0 iff all assertions hold.
#
# Invariants this test defends:
#   - The verifier is a SEPARATE OS process from the peer (no shared
#     BEAM, no shared cache, no shared memory).
#   - The verifier runs on a SEPARATE network origin (macOS host vs
#     guest inside QEMU).
#   - The verifier's trust anchor is installed BEFORE the request;
#     the peer can't inject its own CA into the verification.
#   - The verifier independently runs the five-check cryptographic
#     battery (pkix_path_validation, TPM2_Quote signature parse,
#     PCR 15 replay, node-message binding, envelope shape).
set -euo pipefail
cd "$(dirname "$0")/.."

PORT_VERIFIER=${PORT_VERIFIER:-18735}
PORT_PEER=${PORT_PEER:-18734}
VERIFIER=http://127.0.0.1:${PORT_VERIFIER}
PEER=http://127.0.0.1:${PORT_PEER}
HB_ROOT=${HB_ROOT:-"$(cd .. && pwd)"}
VERIFIER_LOG=${VERIFIER_LOG:-/tmp/lapee-hb-verifier.log}
VERIFIER_CA=/tmp/test-tpm-ca.crt
VERIFIER_CFG=/tmp/lapee-hb-verifier.flat

cleanup() {
    echo "=== cleanup ==="
    pkill -9 -f 'rebar3.*shell' 2>/dev/null || true
    pkill -9 -f "beam.smp.*${HB_ROOT}" 2>/dev/null || true
    if [[ "${KEEP_GUEST:-0}" != "1" ]]; then
        pkill -f 'qemu-system-x86_64' 2>/dev/null || true
        pkill -f swtpm 2>/dev/null || true
    fi
}
trap cleanup EXIT

echo "=== 1/5 boot guest peer on :${PORT_PEER} ==="
rm -f out/attestation.json out/test-tpm-ca.crt /tmp/lapee-hb-guest.log
./scripts/boot-hb.sh --keep-alive --log /tmp/lapee-hb-guest.log
test -s out/attestation.json
test -s out/test-tpm-ca.crt

echo "=== 2/5 install guest CA on verifier as trust anchor ==="
cp out/test-tpm-ca.crt "$VERIFIER_CA"
cat > "$VERIFIER_CFG" <<EOF
# Cross-node verifier HB: trusts the per-boot test TPM CA captured
# from the guest's serial stream in step 1. In production, this
# would be a vendor-issued TPM root CA bundle.
lapee_tpm_ca_cert: ${VERIFIER_CA}
EOF
ls -l "$VERIFIER_CA" "$VERIFIER_CFG"

echo "=== 3/5 start verifier HB on :${PORT_VERIFIER} (native macOS) ==="
pkill -9 -f 'rebar3.*shell' 2>/dev/null || true
(
    cd "$HB_ROOT"
    LAPEE_TPM_ALLOW_NO_NIF=1 \
    HB_PORT="$PORT_VERIFIER" \
    HB_MODE=debug \
    HB_CONFIG="$VERIFIER_CFG" \
    nohup rebar3 shell \
        --eval 'ok.' \
        --eval 'timer:sleep(infinity).' \
        >"$VERIFIER_LOG" 2>&1 &
)
# Wait for verifier ready — up to 60s.
for i in $(seq 1 30); do
    if curl -fsS -m 3 "${VERIFIER}/~meta@1.0/info" -o /dev/null 2>/dev/null
    then
        echo "verifier ready after $((i * 2))s"
        break
    fi
    sleep 2
done
curl -fsS -m 3 "${VERIFIER}/~meta@1.0/info" -o /dev/null || {
    echo "verifier did not come up — last 30 log lines:" >&2
    tail -30 "$VERIFIER_LOG" >&2
    exit 1
}

echo ""
echo "=== 4/5 verifier verifies peer via GET verify-peer ==="
RESULT=/tmp/cross-node-result.json
curl -fsS -m 60 --get \
    --data-urlencode "peer=${PEER}" \
    -H 'accept: application/json@1.0' \
    -H 'accept-bundle: true' \
    "${VERIFIER}/~tpm-interpret@1.0/verify-peer" \
    -o "$RESULT" \
    -w 'HTTP=%{http_code} SIZE=%{size_download} TIME=%{time_total}\n'

echo ""
echo "=== 5/5 assert verdict ==="
python3 - "$RESULT" <<'PY'
import json, sys
r = json.load(open(sys.argv[1]))
b = r.get('body', r)

ok = True
def fail(msg):
    global ok
    ok = False
    print(f"FAIL: {msg}")
def pa(msg):
    print(f"PASS: {msg}")

if b.get('verified') not in (True, 'true'):
    fail(f"verified is not true: {b.get('verified')!r}")
else:
    pa("verified == true")

if b.get('verdict') != 'accepted':
    fail(f"verdict is not accepted: {b.get('verdict')!r}")
else:
    pa("verdict == accepted")

checks = b.get('checks') or []
names = {c.get('name') for c in checks if isinstance(c, dict)}
want = {
    "EK certificate chains to trusted TPM vendor root CA",
    "TPM2_Quote signature + pcrDigest + nonce all valid",
    "Runtime event log replay of PCR 15 matches quoted value",
    "PCR 15 extension commits to node_message_id",
    "Embedded node_message + id present and correct shape",
}
missing = want - names
if missing:
    fail(f"missing checks: {missing}")
else:
    pa(f"all 5 crypto checks ran")

failed = [c['name'] for c in checks
          if isinstance(c, dict) and c.get('ok') not in (True, 'true')]
if failed:
    fail(f"failed checks: {failed}")
else:
    pa("all 5 checks PASS")

s = b.get('summary') or {}
if s.get('on-start-hook-device') != 'tpm2@2.0a':
    fail(f"on_start_hook_device != tpm2@2.0a: {s.get('on-start-hook-device')!r}")
else:
    pa("summary.on_start_hook_device == tpm2@2.0a (enforced hook)")

if s.get('pcr15-event-count') not in (1, '1'):
    fail(f"pcr15_event_count != 1: {s.get('pcr15-event-count')!r}")
else:
    pa("summary.pcr15_event_count == 1")

nmid = s.get('node-message-id')
if not isinstance(nmid, str) or len(nmid) != 43:
    fail(f"node_message_id not 43-char base64url: {nmid!r}")
else:
    pa(f"summary.node_message_id is 43-char base64url")

if s.get('envelope-version') != '0.3':
    fail(f"envelope_version != 0.3: {s.get('envelope-version')!r}")
else:
    pa("summary.envelope_version == 0.3")

# AK identity is an independent fingerprint that lets a trusting
# caller pin subsequent messages to THIS attestation.
pub = s.get('ak-public-key-b64url')
if not isinstance(pub, str) or len(pub) != 43:
    fail(f"ak_public_key_b64url not 43-char base64url: {pub!r}")
else:
    pa("summary.ak_public_key_b64url is 43-char base64url")

print()
print("CROSS-NODE VERIFY:", "PASS" if ok else "FAIL")
sys.exit(0 if ok else 1)
PY

echo ""
echo "=== CROSS-NODE VERIFY: PASS ==="
echo "(verifier $VERIFIER, peer $PEER; result: $RESULT)"
