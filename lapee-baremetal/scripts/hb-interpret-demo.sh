#!/usr/bin/env bash
# hb-interpret-demo.sh — end-to-end demo of `~tpm-interpret@1.0'.
#
# Uses the chain URL that matches the user's documented paper-ready
# pattern:
#
#     ~relay@1.0/call&relay-path="http://PEER/~tpm2@2.0a/attestation"
#         /verify~tpm-interpret@1.0
#
# Here we call the LOCAL HB as both the peer and the verifier (so
# the demo doesn't need a relay route configured). The semantics are
# identical: `attestation' is resolved first, its result is piped
# into `verify' with device ~tpm-interpret@1.0.
#
# Prints the returned `verified' / `verdict' / `checks', then the
# rich per-section interpretation (tpm / ak / quote / pcrs / boot /
# kernel / ima / node).
set -euo pipefail
cd "$(dirname "$0")/.."

PORT=${PORT:-18734}
PEER=${PEER:-"http://127.0.0.1:${PORT}"}

# Sanity: is HB actually up?
if ! curl -fsS -m 5 "$PEER/~meta@1.0/info" -o /dev/null; then
    echo "HB is not reachable at $PEER; boot the guest first:" >&2
    echo "  ./scripts/boot-hb.sh --keep-alive" >&2
    exit 1
fi

echo "============================================================"
echo "=== GET ${PEER}/~tpm2@2.0a/attestation/verify~tpm-interpret@1.0"
echo "============================================================"
curl -fsS -m 180 \
    -H 'accept: application/json@1.0' \
    -H 'accept-bundle: true' \
    "$PEER/~tpm2@2.0a/attestation/verify~tpm-interpret@1.0" \
    -o /tmp/hb-interpret-result.json \
    -w 'HTTP=%{http_code} SIZE=%{size_download} TIME=%{time_total}\n'

echo ""
python3 <<'PY'
import json
r = json.load(open('/tmp/hb-interpret-result.json'))
b = r.get('body', r)

print('verified:', b.get('verified'), '| verdict:', b.get('verdict'))
print()
print('--- verifier checks ---')
for c in (b.get('checks') or []):
    if isinstance(c, dict):
        mark = 'OK ' if (c.get('ok') in (True, 'true')) else 'XX '
        name = (c.get('name') or '')[:55]
        det = (str(c.get('detail')) or '')[:60]
        print(f'  {mark}| {name:<55} -> {det}')

interp = b.get('interpretation') or {}

def say(section, key):
    v = (interp.get(section) or {}).get(key)
    if v not in (None, 'null'):
        if isinstance(v, (dict, list)):
            v = json.dumps(v)[:100]
        print(f'  {key:<30} {v}')

print()
print('--- envelope ---')
for k in ('version','issued_at_unix','wallet_address','node_message_id'):
    say('envelope', k)

print()
print('--- TPM identity ---')
for k in ('manufacturer_id','manufacturer_name','manufacturer_kind',
         'model','firmware_version','spec_family','spec_level',
         'spec_revision','ek_cert_issuer','ek_cert_serial'):
    say('tpm', k)

print()
print('--- AK ---')
for k in ('algorithm','key_size_bits','public_exponent',
         'pub_der_sha256_b64url'):
    say('ak', k)

print()
print('--- Quote metadata ---')
for k in ('magic_ok','attest_type','clock_ms','reset_count',
         'restart_count','safe','nonce_b64url'):
    say('quote', k)

print()
print('--- PCR roles ---')
pcrs = interp.get('pcrs') or {}
for key, e in sorted([(k, v) for k, v in pcrs.items()
                      if isinstance(v, dict) and k.isdigit()],
                     key=lambda kv: int(kv[0])):
    role = e.get('role', '')
    z = 'zero' if e.get('is_zero') in (True, 'true') else 'set '
    h = (e.get('hex') or '')[:16]
    print(f'  PCR {key:>2} ({z}): {role:<35} {h}...')

print()
print('--- Boot chain ---')
for k in ('secure_boot_measured','secure_boot_policy_hex',
         'firmware_srtm_hex','match'):
    say('boot', k)

print()
print('--- Kernel ---')
for k in ('uki_measured','uki_image_hex','boot_loader_hex'):
    say('kernel', k)

print()
print('--- IMA ---')
for k in ('active','pcr10_hex','note'):
    say('ima', k)

print()
print('--- Node identity ---')
for k in ('wallet_address','node_message_id','node_message_key_count',
         'on_start_hook_device','pcr15_event_count',
         'pcr15_event_types'):
    say('node', k)
PY
