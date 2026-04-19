#!/usr/bin/env bash
# hb-tamper-test.sh — negative completeness check for the verifier.
# Starts with a valid envelope (out/evidence/att-baseline.json) and
# produces ~N tampered variants, confirming that the verifier
# REJECTS each one. This is what gives us confidence the verifier is
# actually checking the chain, rather than just rubber-stamping a
# well-formed JSON blob.
#
# Variants:
#   1. Flip one byte in the TPM2_Quote signature           → quote sig fails
#   2. Flip one byte in the `quoted' blob                  → quote sig fails
#   3. Flip the reported PCR 15 value                      → pcrDigest mismatch
#   4. Change `nonce_hex' to a different nonce             → extraData mismatch
#   5. Change the single event-log entry's digest_sha256   → PCR 15 replay fail
#   6. Change `node_message_id_hex'                        → binding check fails
#   7. Replace EK cert PEM with a self-signed cert for the same key
#                                                          → CA chain fails
#
# Each variant is written under out/acceptance/tamper/*.json; the
# verifier is invoked on each; a non-zero exit from verifier is
# expected and counted as PASS for this script.
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

# First confirm the BASE passes.
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
import json, sys, base64, os
base_path, out_path = sys.argv[1], sys.argv[2]
env = json.load(open(base_path))

def flip_first_byte_of_b64(s):
    raw = bytearray(base64.b64decode(s))
    raw[0] ^= 0xFF
    return base64.b64encode(bytes(raw)).decode()

def flip_first_hex_byte(h):
    b = bytearray.fromhex(h)
    b[0] ^= 0xFF
    return b.hex()

$script

json.dump(env, open(out_path, "w"), indent=2)
print(f"wrote {out_path}")
PY

    # Run verifier — we EXPECT failure. Capture exit code.
    if python3 "$VERIFIER" "$out" "$CA" >/tmp/tamper-output.txt 2>&1; then
        echo "FAIL: verifier ACCEPTED tampered envelope '$name'"
        tail -20 /tmp/tamper-output.txt
        return 1
    fi
    # Verifier exited non-zero — expected. Show the failing check line.
    FAILED=$(grep '^\[FAIL\]' /tmp/tamper-output.txt | head -1)
    if [[ -n "$FAILED" ]]; then
        echo "PASS: verifier rejected — $FAILED"
    else
        # No [FAIL] line; show the verdict.
        VERDICT=$(grep 'VERDICT:' /tmp/tamper-output.txt | head -1)
        echo "PASS: verifier rejected — $VERDICT"
    fi
    return 0
}

ALL_OK=1

run_variant flip-signature '
env["tpm_quote"]["signature_b64"] = flip_first_byte_of_b64(env["tpm_quote"]["signature_b64"])
' || ALL_OK=0

run_variant flip-quoted '
env["tpm_quote"]["quoted_b64"] = flip_first_byte_of_b64(env["tpm_quote"]["quoted_b64"])
' || ALL_OK=0

run_variant flip-pcr15-reported '
env["tpm_quote"]["pcr_values"]["15"] = flip_first_hex_byte(env["tpm_quote"]["pcr_values"]["15"])
' || ALL_OK=0

run_variant swap-nonce '
env["tpm_quote"]["nonce_hex"] = "deadbeef" * 4
' || ALL_OK=0

run_variant flip-event-digest '
for e in env["runtime_event_log"]:
    if int(e["pcr"]) == 15:
        e["digest_sha256"] = flip_first_hex_byte(e["digest_sha256"])
        break
' || ALL_OK=0

run_variant flip-node-id '
env["node_message_id_hex"] = flip_first_hex_byte(env["node_message_id_hex"])
' || ALL_OK=0

# EK cert swap: use a throwaway self-signed cert (not chained to our CA).
python3 - <<'PY'
import subprocess, sys
subprocess.check_call([
    "openssl","req","-x509","-newkey","rsa:2048","-nodes","-days","30",
    "-subj","/CN=not-a-real-lapee-ca",
    "-keyout","/tmp/tamper-rogue.key",
    "-out","/tmp/tamper-rogue.crt",
], stderr=subprocess.DEVNULL)
PY

run_variant swap-ek-cert '
with open("/tmp/tamper-rogue.crt") as f:
    env["ek_cert_pem"] = f.read()
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
