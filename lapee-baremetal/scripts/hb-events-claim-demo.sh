#!/usr/bin/env bash
# hb-events-claim-demo.sh — exercise the /events and /claim handlers
# of ~tpm-interpret@1.0 against a running LapEE peer, using the same
# paper-style chain URL pattern as hb-interpret-demo.sh.
#
# /events  — rich per-record TCG event log interpretation
#            (decoded per event type with base64url-encoded binaries
#             so the wire is JSON-safe).
# /claim   — flat, policy-friendly projection of the events list
#            with concrete booleans/strings per measurable property
#            plus per-field provenance (which PCR + which event seq).
#
# Output: /tmp/hb-events-result.json, /tmp/hb-claim-result.json.
# Prints headline per-section summaries so the demo is human-readable.
set -euo pipefail
cd "$(dirname "$0")/.."

PORT=${PORT:-18734}
PEER=${PEER:-"http://127.0.0.1:${PORT}"}

if ! curl -fsS -m 5 "$PEER/~meta@1.0/info" -o /dev/null; then
    echo "HB is not reachable at $PEER; boot the guest first:" >&2
    echo "  ./scripts/boot-hb.sh --keep-alive" >&2
    exit 1
fi

# Refresh the four `format~hyperbuddy@1.0' AO-Core message trees so
# the dashboard can embed them as the "full picture" views. Silent
# failures are fine — the dashboard degrades gracefully if any of
# the four aren't available.
mkdir -p out/evidence
for pair in \
    'attestation:/~tpm2@2.0a/attestation/format~hyperbuddy@1.0&truncate-keys=1000' \
    'interpret:/~tpm2@2.0a/attestation/interpret~tpm-interpret@1.0/format~hyperbuddy@1.0&truncate-keys=1000' \
    'events:/~tpm2@2.0a/attestation/events~tpm-interpret@1.0/format~hyperbuddy@1.0&truncate-keys=1000' \
    'claim:/~tpm2@2.0a/attestation/claim~tpm-interpret@1.0/format~hyperbuddy@1.0&truncate-keys=1000' \
; do
    label=${pair%%:*}
    path=${pair#*:}
    curl -fsS -m 60 "${PEER}${path}" \
        -o "out/evidence/hyperbuddy-${label}.txt" 2>/dev/null || true
done

echo "============================================================"
echo "=== GET ${PEER}/~tpm2@2.0a/attestation/events~tpm-interpret@1.0"
echo "============================================================"
curl -fsS -m 180 \
    -H 'accept: application/json@1.0' \
    -H 'accept-bundle: true' \
    "$PEER/~tpm2@2.0a/attestation/events~tpm-interpret@1.0" \
    -o /tmp/hb-events-result.json \
    -w 'HTTP=%{http_code} SIZE=%{size_download} TIME=%{time_total}\n'

python3 <<'PY'
import json
r = json.load(open('/tmp/hb-events-result.json'))
b = r.get('body', r)
# HB may wrap the handler output in a {status,body} envelope AND
# inject HB-internal keys ("commitments", ...) at every level.
# Keep only numeric event keys.
events = b.get('body', b) if isinstance(b, dict) else b
event_keys = sorted([k for k in events.keys() if k.isdigit()],
                    key=lambda k: int(k))
print()
print(f"--- events: {len(event_keys)} record(s) ---")
for key in event_keys:
    e = events[key]
    et = e.get('event_type', '?')
    pcr = e.get('pcr', '?')
    seq = e.get('seq', '?')
    have_parsed = 'parsed' in e
    have_semantic = have_parsed and 'semantic' in (e.get('parsed') or {})
    tag = 'parsed+semantic' if have_semantic else ('parsed' if have_parsed else '-')
    print(f"  seq={seq:<3}  pcr={pcr:<3}  {et:<35} ({tag})")
PY

echo ""
echo "============================================================"
echo "=== GET ${PEER}/~tpm2@2.0a/attestation/claim~tpm-interpret@1.0"
echo "============================================================"
curl -fsS -m 180 \
    -H 'accept: application/json@1.0' \
    -H 'accept-bundle: true' \
    "$PEER/~tpm2@2.0a/attestation/claim~tpm-interpret@1.0" \
    -o /tmp/hb-claim-result.json \
    -w 'HTTP=%{http_code} SIZE=%{size_download} TIME=%{time_total}\n'

python3 <<'PY'
import json
r = json.load(open('/tmp/hb-claim-result.json'))
b = r.get('body', r)
claim = b.get('body', b) if isinstance(b, dict) else b
print()
print("--- claim (flat policy surface) ---")

def fmt_value(v):
    if v in (None, 'null'):
        return '-'
    if isinstance(v, (dict, list)):
        return json.dumps(v)[:60]
    return str(v)

def show_section(label):
    sec = claim.get(label) or {}
    if not isinstance(sec, dict):
        return
    print(f"  [{label}]")
    # HB injects "commitments" (signature metadata) at every nested
    # level; filter it from the human-readable view.
    for k in sorted(sec.keys()):
        if k.endswith('_provenance') or k == 'commitments':
            continue
        v = sec[k]
        prov = sec.get(f"{k}_provenance")
        prov_tag = (
            f"  (prov: {len(prov)} src)"
            if isinstance(prov, list) and prov else ""
        )
        print(f"    {k:<28} {fmt_value(v):<32}{prov_tag}")

for s in ('secure_boot','firmware','boot_loader','kernel','tme','lockdown'):
    show_section(s)
PY
