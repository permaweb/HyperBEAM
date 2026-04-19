#!/usr/bin/env bash
# hb-interpret-demo.sh — end-to-end demo of `~tpm-interpret@1.0'.
#
# Assumes the LapEE guest is running (either via `make hb-boot
# --keep-alive' or because you just finished `make hb-all').
#
# Flow:
#   1. POST the baseline attestation envelope (from out/evidence/) to
#      /~tpm-interpret@1.0/verify on the same node.
#   2. Print the returned interpretation — verdict, checks, and the
#      rich per-section fields (tpm / ak / quote / pcrs / boot /
#      kernel / ima / node).
#   3. Same for a tampered envelope, showing the interpret pathway
#      ALSO fires on rejected input (so a caller sees both the
#      rejection reason and the structured description).
#
# Uses the same content-negotiation as the rest of the flow:
#   accept: application/json@1.0 + accept-bundle: true.
set -euo pipefail
cd "$(dirname "$0")/.."

PORT=${PORT:-18734}
PEER=${PEER:-"http://127.0.0.1:${PORT}"}
ENV=out/evidence/att-baseline.json

if [[ ! -f "$ENV" ]]; then
    echo "missing $ENV — run: make hb-boot (once) to populate" >&2
    echo "                     or drop an envelope there manually" >&2
    exit 1
fi

# Sanity: is HB actually up?
if ! curl -fsS -m 5 "$PEER/~meta@1.0/info" -o /dev/null; then
    echo "HB is not reachable at $PEER; boot the guest first:" >&2
    echo "  ./scripts/boot-hb.sh --keep-alive" >&2
    exit 1
fi

echo "============================================================"
echo "=== verify + interpret (baseline)"
echo "============================================================"
curl -fsS -m 180 \
    -H 'accept: application/json@1.0' \
    -H 'accept-bundle: true' \
    -H 'content-type: application/json' \
    --data-binary @"$ENV" \
    "$PEER/~tpm-interpret@1.0/verify" \
  | python3 -c "
import json, sys
r = json.load(sys.stdin)
b = r.get('body', r)
print('verified:', b.get('verified'), '| verdict:', b.get('verdict'))
checks = b.get('checks') or []
if isinstance(checks, list):
    for c in checks:
        if isinstance(c, dict):
            mark = 'OK ' if c.get('ok') else 'XX '
            name = (c.get('name') or '')[:55]
            det = (str(c.get('detail')) or '')[:60]
            print(f'  {mark}| {name:<55} -> {det}')

interp = b.get('interpretation') or {}
print()
print('--- TPM identity ---')
for k in ('manufacturer_id','manufacturer_name','manufacturer_kind',
         'model','firmware_version','spec_family','spec_level',
         'spec_revision','ek_cert_issuer'):
    v = interp.get('tpm', {}).get(k)
    if v is not None:
        print(f'  {k}: {v}')
print()
print('--- AK ---')
for k in ('algorithm','key_size_bits','public_exponent','pub_der_sha256_b64url'):
    v = interp.get('ak', {}).get(k)
    if v is not None:
        print(f'  {k}: {v}')
print()
print('--- Quote metadata ---')
for k in ('magic_ok','attest_type','clock_ms','reset_count','restart_count','safe'):
    v = interp.get('quote', {}).get(k)
    if v is not None:
        print(f'  {k}: {v}')
print()
print('--- PCR roles ---')
for key, e in sorted((interp.get('pcrs') or {}).items(), key=lambda kv: int(kv[0]) if kv[0].isdigit() else 99):
    role = e.get('role')
    z = 'zero' if e.get('is_zero') else 'set'
    h = (e.get('hex') or '')[:16]
    print(f'  PCR {key:>2} ({z}): {role:<35} {h}...')
print()
print('--- Boot chain ---')
for k in ('secure_boot_measured','match'):
    v = interp.get('boot', {}).get(k)
    if v is not None:
        print(f'  {k}: {v if not isinstance(v, dict) else v.get(\"name\",\"(matched)\")}')
print()
print('--- Kernel ---')
for k in ('uki_measured','uki_image_hex'):
    v = interp.get('kernel', {}).get(k)
    if v is not None:
        print(f'  {k}: {v}')
print()
print('--- IMA ---')
for k in ('active','note'):
    v = interp.get('ima', {}).get(k)
    if v is not None:
        print(f'  {k}: {v}')
print()
print('--- Node identity ---')
for k in ('wallet_address','node_message_id','node_message_key_count',
         'on_start_hook_device','pcr15_event_count'):
    v = interp.get('node', {}).get(k)
    if v is not None:
        print(f'  {k}: {v}')
"

echo ""
echo "============================================================"
echo "=== verify + interpret (TAMPERED — signature byte flipped)"
echo "============================================================"
python3 -c "
import json, base64
r = json.load(open('out/evidence/att-baseline.json'))
env = r.get('body', r) if 'body' in r else r
sig = env['tpm_quote'].get('signature') or env['tpm_quote'].get('signature_b64')
pad = '=' * (-len(sig) % 4)
try:
    raw = bytearray(base64.urlsafe_b64decode(sig + pad))
except Exception:
    raw = bytearray(base64.b64decode(sig + pad))
raw[0] ^= 0xFF
encoded = base64.urlsafe_b64encode(bytes(raw)).rstrip(b'=').decode()
if 'signature' in env['tpm_quote']:
    env['tpm_quote']['signature'] = encoded
else:
    env['tpm_quote']['signature_b64'] = encoded
json.dump(env, open('/tmp/hb-interpret-tampered.json','w'))
"

curl -fsS -m 180 \
    -H 'accept: application/json@1.0' \
    -H 'accept-bundle: true' \
    -H 'content-type: application/json' \
    --data-binary @/tmp/hb-interpret-tampered.json \
    "$PEER/~tpm-interpret@1.0/verify" \
  | python3 -c "
import json, sys
r = json.load(sys.stdin)
b = r.get('body', r)
print('verified:', b.get('verified'), '| verdict:', b.get('verdict'))
for c in (b.get('checks') or []):
    if isinstance(c, dict):
        mark = 'OK ' if c.get('ok') else 'XX '
        name = (c.get('name') or '')[:55]
        det = (str(c.get('detail')) or '')[:60]
        print(f'  {mark}| {name:<55} -> {det}')
# Interpretation was still generated despite rejection.
i = b.get('interpretation') or {}
print()
print('interpretation still produced for rejected envelope:',
      list(i.keys())[:6])
print('  tpm.manufacturer_name:', i.get('tpm', {}).get('manufacturer_name'))
"
