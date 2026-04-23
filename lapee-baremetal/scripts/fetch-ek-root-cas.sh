#!/usr/bin/env bash
# fetch-ek-root-cas.sh -- populate priv/tpm-interpret/root-cas/ with
# public TPM vendor EK-cert root + intermediate CAs.
#
# These roots are required for legitimate end-to-end EK chain
# validation: without them, claim_ek.chain-validation reports
# `no-roots-loaded' and a real attestation chain has nothing to
# anchor against.
#
# Primary source: the keylime project's TPM cert store
# (github.com/keylime/keylime/tpm_cert_store). Keylime is the
# upstream reference for TPM-based remote attestation in Linux
# distros, is BSD-licensed, and maintains a curated set of
# Infineon / Intel / Nuvoton / STMicro / GlobalSign / Alibaba /
# Nationz roots and intermediates. Using it as the trust anchor
# keeps us aligned with the broader Linux ecosystem rather than
# shipping a boutique bundle.
#
# AMD fTPM is intentionally not in keylime's bundle. AMD's fTPM
# EK cert chain is per-chip and fetched out-of-band from the PSP
# -- there is no single stable root to preload. When we encounter
# an AMD fTPM EK cert at runtime, ek-cert-source.kind reports
# `tpm-nv' with the handle and the verifier records chain-valid =
# false + reason = "no matching AMD fTPM root loaded", which is
# the honest answer.
#
# Usage:
#   ./scripts/fetch-ek-root-cas.sh            # refresh all
#   ./scripts/fetch-ek-root-cas.sh --check    # verify existing files
#   ./scripts/fetch-ek-root-cas.sh --add URL  # add a new root URL

set -euo pipefail

cd "$(dirname "$0")/../.."
DEST="priv/tpm-interpret/root-cas"
mkdir -p "$DEST"

MODE="${1:-fetch}"

KEYLIME_API="https://api.github.com/repos/keylime/keylime/contents/tpm_cert_store"

if [ "$MODE" = "--check" ]; then
    echo "=== current root-cas/ contents ==="
    n=0
    for f in "$DEST"/*.pem; do
        [ -f "$f" ] || continue
        subj=$(openssl x509 -in "$f" -noout -subject 2>/dev/null \
                | sed 's/^subject= *//' || echo "(unparseable)")
        notafter=$(openssl x509 -in "$f" -noout -enddate 2>/dev/null \
                | sed 's/^notAfter= *//')
        printf "  %-40s  %s\n                                          (expires %s)\n" \
            "$(basename "$f")" "$subj" "$notafter"
        n=$((n+1))
    done
    echo ""
    echo "$n root/intermediate certs loaded."
    exit 0
fi

if [ "$MODE" = "--add" ]; then
    URL="${2:?}"
    NAME="${3:-$(basename "$URL")}"
    NAME="${NAME%.crt}.pem"
    NAME="${NAME%.cer}.pem"
    out="$DEST/$NAME"
    tmp="$out.tmp.$$"
    echo "=> fetching $URL"
    curl -fsSL --connect-timeout 10 --max-time 60 "$URL" -o "$tmp"
    if head -1 "$tmp" | grep -q "BEGIN CERTIFICATE"; then
        mv "$tmp" "$out"
    else
        openssl x509 -inform DER -in "$tmp" -out "$out"
        rm -f "$tmp"
    fi
    openssl x509 -in "$out" -noout -subject
    exit 0
fi

# -- bulk fetch from keylime -------------------------------------------

echo "=> enumerating keylime tpm_cert_store ..."
LIST=$(curl -fsSL --connect-timeout 15 --max-time 60 "$KEYLIME_API" \
    | python3 -c "
import json,sys
for f in json.load(sys.stdin):
    n = f.get('name','')
    if n.lower().endswith(('.pem','.crt','.cer')):
        print(n + '|' + f['download_url'])
")
echo "$LIST" | wc -l | awk '{printf "   %s certs in upstream store\n", $1}'

fail=0
got=0
for spec in $LIST; do
    IFS='|' read -r name url <<< "$spec"
    out="$DEST/$name"
    tmp="$out.tmp.$$"
    if curl -fsSL --connect-timeout 10 --max-time 60 "$url" -o "$tmp" \
            2>/dev/null; then
        if head -1 "$tmp" | grep -q "BEGIN CERTIFICATE"; then
            mv "$tmp" "$out"
            got=$((got+1))
        elif openssl x509 -inform DER -in "$tmp" -out "$out" 2>/dev/null; then
            rm -f "$tmp"
            got=$((got+1))
        else
            rm -f "$tmp"
            echo "   [warn] $name: not PEM or DER" >&2
            fail=1
        fi
    else
        rm -f "$tmp"
        echo "   [warn] $name: fetch failed" >&2
        fail=1
    fi
done

# Keep the explicit Infineon Optiga fetches from the original script
# -- these are the CA030 roots used for current-production SLB96xx
# TPMs, which are easier to audit individually than via keylime's
# numerically-named IFX_RSA_034.pem etc.
EXTRA=(
  "infineon-optiga-rsa-ca030.pem|https://pki.infineon.com/OptigaRsaMfrCA030/OptigaRsaMfrCA030.crt"
  "infineon-optiga-ecc-ca030.pem|https://pki.infineon.com/OptigaEccMfrCA030/OptigaEccMfrCA030.crt"
)
for spec in "${EXTRA[@]}"; do
    IFS='|' read -r name url <<< "$spec"
    out="$DEST/$name"
    tmp="$out.tmp.$$"
    if curl -fsSL --connect-timeout 10 --max-time 60 "$url" -o "$tmp" \
            2>/dev/null; then
        if head -1 "$tmp" | grep -q "BEGIN CERTIFICATE"; then
            mv "$tmp" "$out"
        elif openssl x509 -inform DER -in "$tmp" -out "$out" 2>/dev/null; then
            rm -f "$tmp"
        fi
        got=$((got+1))
    else
        rm -f "$tmp"
    fi
done

echo ""
echo "=== summary ==="
printf "  %s root/intermediate certs loaded in %s\n" "$got" "$DEST"
[ "$fail" = 0 ] || echo "  (some fetches failed; see warnings above)"
exit $fail
