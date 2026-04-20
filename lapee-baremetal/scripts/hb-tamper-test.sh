#!/usr/bin/env bash
# hb-tamper-test.sh — completeness test for the verifier.
#
# Starts with a valid envelope and produces seven targeted
# byte-flipped variants, one per verifier check. Runs the Python
# verifier on each; non-zero exit is required. Exit 0 iff all seven
# are correctly rejected.
#
# Uses the v0.3 AO-Core schema (base64url binaries, unwrapped body).
set -euo pipefail
cd "$(dirname "$0")/.."

BASE=${1:-out/evidence/att-baseline.json}
CA=${2:-out/evidence/ca-baseline.crt}
VERIFIER=reference-demo/verifier/verifier_hb.py
TAMPER_DIR=out/acceptance/tamper
rm -rf "$TAMPER_DIR" && mkdir -p "$TAMPER_DIR"

if [[ ! -f "$BASE" || ! -f "$CA" ]]; then
    echo "need $BASE and $CA (run: make hb-acceptance first)" >&2
    exit 1
fi

echo "=== sanity: baseline envelope should ACCEPT ==="
python3 "$VERIFIER" "$BASE" "$CA" >/dev/null
echo "baseline accepted ✓"

run_variant() {
    local name=$1
    local script=$2
    local out="$TAMPER_DIR/${name}.json"

    echo ""
    echo "============================================================"
    echo "=== tamper: $name"
    echo "============================================================"
    python3 - "$BASE" "$out" <<PY
import json, sys, base64
base_path, out_path = sys.argv[1], sys.argv[2]
raw = json.load(open(base_path))
# Envelope lives under body in the HB response wrapper; accept
# both shapes.
env = raw if "lapee-attestation-version" in raw else raw.get("body", raw)

def b64url_decode(s):
    pad = "=" * (-len(s) % 4)
    return base64.urlsafe_b64decode(s + pad)

def b64url_encode(b):
    return base64.urlsafe_b64encode(b).rstrip(b"=").decode()

def flip_first_byte_b64url(s):
    raw = bytearray(b64url_decode(s))
    raw[0] ^= 0xFF
    return b64url_encode(bytes(raw))

$script

# Re-wrap if original was wrapped.
if raw is not env:
    raw["body"] = env
    json.dump(raw, open(out_path, "w"), indent=2)
else:
    json.dump(env, open(out_path, "w"), indent=2)
print(f"wrote {out_path}")
PY

    # Run verifier; we EXPECT failure.
    if python3 "$VERIFIER" "$out" "$CA" >/tmp/tamper-output.txt 2>&1; then
        echo "FAIL: verifier ACCEPTED tampered envelope '$name'"
        tail -20 /tmp/tamper-output.txt
        return 1
    fi
    FAILED=$(grep '^\[FAIL\]' /tmp/tamper-output.txt | head -1)
    if [[ -n "$FAILED" ]]; then
        echo "PASS: verifier rejected — $FAILED"
    else
        VERDICT=$(grep 'VERDICT:' /tmp/tamper-output.txt | head -1)
        echo "PASS: verifier rejected — $VERDICT"
    fi
    return 0
}

ALL_OK=1

run_variant flip-signature '
env["tpm-quote"]["signature"] = flip_first_byte_b64url(env["tpm-quote"]["signature"])
' || ALL_OK=0

run_variant flip-quoted '
env["tpm-quote"]["quoted"] = flip_first_byte_b64url(env["tpm-quote"]["quoted"])
' || ALL_OK=0

run_variant flip-pcr15-reported '
env["tpm-quote"]["pcr-values"]["15"] = flip_first_byte_b64url(env["tpm-quote"]["pcr-values"]["15"])
' || ALL_OK=0

run_variant swap-nonce '
env["tpm-quote"]["nonce"] = b64url_encode(b"\xde\xad\xbe\xef" * 8)
' || ALL_OK=0

run_variant flip-event-digest '
for e in env["runtime-event-log"]:
    if int(e["pcr"]) == 15:
        e["digest"] = flip_first_byte_b64url(e["digest"])
        break
' || ALL_OK=0

run_variant flip-node-id '
env["node-message-id"] = flip_first_byte_b64url(env["node-message-id"])
' || ALL_OK=0

# Rogue EK cert: throwaway self-signed, not chained to our trust anchor.
openssl req -x509 -newkey rsa:2048 -nodes -days 30 \
    -subj "/CN=not-a-real-lapee-ca" \
    -keyout /tmp/tamper-rogue.key \
    -out /tmp/tamper-rogue.crt 2>/dev/null

run_variant swap-ek-cert '
with open("/tmp/tamper-rogue.crt") as f:
    env["ek-cert-pem"] = f.read()
' || ALL_OK=0

echo ""
echo "============================================================"
if [[ $ALL_OK -eq 1 ]]; then
    echo "=== TAMPER-TEST VERDICT: PASS"
    echo "all seven variants were correctly REJECTED by the verifier"
    exit 0
else
    echo "=== TAMPER-TEST VERDICT: FAIL"
    echo "one or more tampered variants sneaked past the verifier"
    exit 1
fi
