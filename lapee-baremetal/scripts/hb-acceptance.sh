#!/usr/bin/env bash
# hb-acceptance.sh — the three-envelope acceptance battery for the
# dev_tpm2 LapEE guest. Boots three times:
#
#   1. baseline         (no user config)
#   2. benign user      (out/evidence/user-diff.flat: custom labels)
#   3. hostile user     (out/evidence/user-hostile.flat: tries to
#                        disable the on.start hook)
#
# Each boot:
#   - captures the attestation envelope via
#     GET /~tpm2@2.0a/attestation-json
#   - extracts the per-boot test CA from serial
#   - runs reference-demo/verifier/verifier_hb.py
#
# The script then prints a summary showing:
#   - each run's node_message_id (must differ between runs if
#     the user config was picked up correctly)
#   - the hostile run's on.start.device (must be `tpm2@2.0a', not
#     the `noop@1.0' the user supplied)
#
# Exit 0 iff all three envelopes verify AND the enforced config
# wins in the hostile case.
set -euo pipefail
cd "$(dirname "$0")/.."

EVIDENCE=out/evidence
VERIFIER=reference-demo/verifier/verifier_hb.py
RUNS_DIR=out/acceptance
rm -rf "$RUNS_DIR" && mkdir -p "$RUNS_DIR"

run_once() {
    local name=$1
    local user_cfg=${2:-}

    echo ""
    echo "============================================================"
    echo "=== RUN: $name"
    [[ -n "$user_cfg" ]] && echo "=== user config: $user_cfg"
    echo "============================================================"

    rm -f out/attestation.json out/test-tpm-ca.crt /tmp/lapee-hb-guest.log
    if [[ -n "$user_cfg" ]]; then
        ./scripts/boot-hb.sh --user "$user_cfg" \
            --log "/tmp/lapee-hb-guest-${name}.log"
    else
        ./scripts/boot-hb.sh --log "/tmp/lapee-hb-guest-${name}.log"
    fi

    cp out/attestation.json    "$RUNS_DIR/${name}-attestation.json"
    cp out/test-tpm-ca.crt     "$RUNS_DIR/${name}-ca.crt"

    echo ""
    echo "--- $name: verifier output ---"
    python3 "$VERIFIER" \
        "$RUNS_DIR/${name}-attestation.json" \
        "$RUNS_DIR/${name}-ca.crt"
}

run_once baseline
run_once user-diff      "$EVIDENCE/user-diff.flat"
run_once user-hostile   "$EVIDENCE/user-hostile.flat"

echo ""
echo "============================================================"
echo "=== ACCEPTANCE SUMMARY"
echo "============================================================"
python3 - <<PY
import json, sys

runs = ["baseline", "user-diff", "user-hostile"]
ids = {}
hook_devices = {}
for r in runs:
    raw = json.load(open(f"out/acceptance/{r}-attestation.json"))
    # HB wraps the device response in {status, body, commitments};
    # the envelope lives under body. Accept either shape.
    env = raw if "lapee_attestation_version" in raw else raw.get("body", raw)
    ids[r] = env["node_message_id"]
    hook_devices[r] = (
        env.get("node_message", {}).get("on", {}).get("start", {}).get("device")
    )

ok = True

print("node_message_id per run:")
for r in runs:
    print(f"  {r:14}: {ids[r]}")

# Every pair of runs must yield a different id.
pairs_match = []
for i, a in enumerate(runs):
    for b in runs[i+1:]:
        same = ids[a] == ids[b]
        pairs_match.append((a, b, same))
        if same:
            ok = False
print()
print("All ids differ between runs:")
for a, b, same in pairs_match:
    mark = "FAIL (equal!)" if same else "PASS"
    print(f"  {a} vs {b}: {mark}")

# Hostile run must still have on.start.device == tpm2@2.0a.
print()
print("on.start.device per run:")
for r in runs:
    print(f"  {r:14}: {hook_devices[r]}")
if hook_devices["user-hostile"] != "tpm2@2.0a":
    ok = False
    print("FAIL: hostile user config overrode enforced on.start.device")
else:
    print("PASS: hostile user config was overridden by enforced config")

print()
print("ACCEPTANCE VERDICT:", "PASS" if ok else "FAIL")
sys.exit(0 if ok else 1)
PY
