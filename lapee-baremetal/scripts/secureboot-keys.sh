#!/usr/bin/env bash
# secureboot-keys.sh — generate operator-enrolled Secure Boot trust anchors.
#
# Produces PK, KEK, db (and derived EFI signature lists) for the LapEE
# operator. These keys replace the factory-default Microsoft-rooted anchors;
# after enrollment only UKIs signed by db will boot.
#
# Outputs under out/keys/:
#   PK.key / PK.crt / PK.esl / PK.auth
#   KEK.key / KEK.crt / KEK.esl / KEK.auth
#   db.key / db.crt / db.esl / db.auth
#   (.auth files are signed EFI variable update payloads)

set -euo pipefail

LAPEE_ROOT="${LAPEE_ROOT:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"
source "${LAPEE_ROOT}/scripts/tools.sh"

KEYS_DIR="${LAPEE_ROOT}/out/keys"
mkdir -p "${KEYS_DIR}"

GUID="${SECUREBOOT_GUID:-$(uuidgen 2>/dev/null || python3 -c 'import uuid; print(uuid.uuid4())')}"
echo "${GUID}" > "${KEYS_DIR}/GUID.txt"

for name in PK KEK db; do
    if [[ ! -f "${KEYS_DIR}/${name}.crt" ]]; then
        echo "generating ${name}..."
        lapee_tool bash -c "
            cd /out/keys
            openssl req -newkey rsa:2048 -nodes -keyout '${name}.key' \
                -new -x509 -sha256 -days 3650 \
                -subj '/CN=LapEE ${name}/' -out '${name}.crt'
            cert-to-efi-sig-list -g '${GUID}' '${name}.crt' '${name}.esl'
        "
    fi
done

# Sign the variable update payloads.
lapee_tool bash -c "
    cd /out/keys
    # PK: self-signed (signs itself).
    sign-efi-sig-list -g '${GUID}' -k PK.key -c PK.crt PK PK.esl PK.auth
    # KEK: signed by PK.
    sign-efi-sig-list -g '${GUID}' -k PK.key -c PK.crt KEK KEK.esl KEK.auth
    # db: signed by KEK.
    sign-efi-sig-list -g '${GUID}' -k KEK.key -c KEK.crt db db.esl db.auth
"

echo "Secure Boot keys generated in ${KEYS_DIR}/"
ls -la "${KEYS_DIR}"
